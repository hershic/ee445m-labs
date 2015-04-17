/* -*- mode: c; c-basic-offset: 4; -*- */
/* Standard Libs */
#include <stdint.h>
#include <stdbool.h>

/* TI Includes */
#include "inc/hw_ints.h"
#include "inc/hw_memmap.h"
#include "inc/hw_nvic.h"
#include "inc/hw_types.h"

/* Driverlib Includes */
#include "driverlib/debug.h"
#include "driverlib/fpu.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/rom.h"

#include "libut/utlist.h"
#include "libhw/hardware.h"

#include "libbuffer/buffer.h"
/* Supported devices */
#include "driverlib/uart.h"
#include "driverlib/timer.h"

#include "libuart/uart.h"
#include "libtimer/timer.h"
#include "libbutton/button.h"

#include "libos/thread_structures.h"
#include "libos/os.h"

/* Each driver is statically allocated */
static hw_driver HW_UART_DRIVER;
static hw_driver HW_TIMER_DRIVER;
static hw_driver HW_BUTTON_DRIVER;

static uint8_t UART0_RX_BUFFER[BUFFER_MAX_LENGTH];
static uint8_t UART0_RX_BUFFER_SIZE = 0;

static uint8_t UART0_TX_BUFFER[BUFFER_MAX_LENGTH];
static uint8_t UART0_TX_BUFFER_SIZE = 0;

uint32_t ADC0_SEQ0_SAMPLES[4];
uint32_t ADC0_SEQ1_SAMPLES[4];
uint32_t ADC0_SEQ2_SAMPLES[4];
uint32_t ADC0_SEQ3_SAMPLES[4];

semaphore_t HW_ADC_SEQ2_SEM;
semaphore_t HW_BUTTON_RAW_SEM;
semaphore_t sem_button_debounce;

uint32_t jitter_begin;
uint32_t jitter_end;

void hw_init_daemon() {

    os_add_thread(hw_daemon);
}

/* To satisfy our need for speed, we must avoid the branches and
* memory ready necessary for lazy initialization; that is to say the
* \hw_driver_init also executing \hw_channel_init or vice versa. */
void hw_driver_init(HW_TYPE type, hw_metadata metadata) {

    switch(type) {
    case HW_UART:
        SysCtlPeripheralEnable(SYSCTL_PERIPH_UART0 +
                               (metadata.uart.channel - UART0_BASE) / 0x1000);
        /* todo: parametrize - are they all on A? */
        SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOA);
        break;

    case HW_TIMER:
        SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER0 +
                               (metadata.timer.base - TIMER0_BASE) / 0x1000);
        break;

    case HW_BUTTON:
        /* TODO: parametrize to allow other buttons to be driven */
        SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);
        /* #need4speed: Buttons on the board are only used for input,
         * we know how they're going to be used. Break the libhw
         * convention and initialize these buttons so this code never
         * has to be run again */
        button_init(metadata);
        break;

    default: postpone_death();
    }
}

/* Initialize internal data structures and call driver functions to
 * initialize hardware relating to the specified channel in
 * \metadata. */
void hw_channel_init(HW_TYPE type, hw_metadata metadata) {

    hw_iterator i;
    hw_channel* channel = _hw_get_channel(type, metadata);
    channel->full_slots = NULL;
    for(i=0; i<HW_DRIVER_MAX_SUBSCRIPTIONS; ++i) {
        CDL_PREPEND(channel->free_slots, &channel->isr_subscriptions[i]);
    }

    switch(type) {
    case HW_UART:
        uart_init(metadata);
        break;

    case HW_TIMER:
        timer_add_interrupt(metadata);
        break;

    case HW_BUTTON:
        /* TODO: parametrize */
        button_set_interrupt(metadata);
        break;

    default: postpone_death();
    }
}

void _hw_subscribe(HW_TYPE     type,
                   hw_metadata metadata,
                   void (*isr)(notification note),
                   bool        single_shot) {

    hw_iterator i;
    hw_channel* channel = _hw_get_channel(type, metadata);
    _isr_subscription* new_subscrip = channel->free_slots;

    CDL_DELETE(channel->free_slots, new_subscrip);
    CDL_PREPEND(channel->full_slots, new_subscrip);

    new_subscrip->single_shot_subscription = single_shot;
    new_subscrip->slot = isr;
}

void hw_unsubscribe(HW_TYPE type,
                    hw_metadata metadata,
                    void (*isr)(notification note)) {

    hw_channel* channel = _hw_get_channel(type, metadata);
    _isr_subscription* remove = channel->full_slots;

    while(remove->slot != isr) {++remove;}
    CDL_DELETE(channel->full_slots, remove);
    CDL_PREPEND(channel->free_slots, remove);
}

void hw_notify(HW_TYPE type, hw_metadata metadata, notification note) {

    hw_iterator i=0;
    hw_channel* channel = _hw_get_channel(type, metadata);
    _isr_subscription* subscrip = channel->full_slots;
    _isr_subscription* new_subscrip = NULL;

    /* TODO: make this a doubly linked list, not circular */
    /* consider similar changes to os lib also */
    while(subscrip && subscrip != new_subscrip) {
        subscrip->slot(note);
        new_subscrip = subscrip->next;
        if (subscrip->single_shot_subscription) {
            /* Cease fire! cease fire! */
            subscrip->single_shot_subscription = false;
            CDL_DELETE(channel->full_slots, subscrip);
            CDL_PREPEND(channel->free_slots, subscrip);
        }
        subscrip = new_subscrip;
    }
}

/* todo: programatically generate this function too */
void hw_daemon(void) {
    while (1) {
        /* mind ordering of sem_checks */
        /* fixme: one thread for each hardware device, so individual
         * priorities can be assigned to the threads (which will
         * handle simultaneous interrupts according to priority
         * scheduler's interpretation of priority) */
        sem_guard(HW_SEM_UART0) {
            sem_take(HW_SEM_UART0);
            /* todo: schedule */
            uart_metadata_init(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
            hw_notify_uart(uart_metadata);
        }
        os_surrender_context();
    }
}

void hw_notify_uart(hw_metadata uart_metadata) {

    notification note;
    /* get-it-working: assume the dev knows which buffer to use */
    uint8_t* buffer = UART0_RX_BUFFER;
    while(!buffer_empty(UART0_RX_BUFFER)) {
        note._char = buffer_last(UART0_RX_BUFFER);
        buffer_dec(UART0_RX_BUFFER);
        hw_notify(HW_UART, uart_metadata, note);
    }
}

/* immaculate hashing function, much fast */
inline
hw_channel* _hw_get_channel(HW_TYPE type, hw_metadata metadata) {

    memory_address_t idx;
    switch(type){
        /* optimize: divide should be a shift */
    case HW_UART:   idx = (metadata.uart.channel - UART0_BASE)     / 0x1000; break;
    case HW_TIMER:  idx = (metadata.timer.base   - TIMER0_BASE)    / 0x1000; break;
    case HW_BUTTON: idx = (metadata.button.base  - GPIO_PORTE_BASE)/ 0x1000; break;
        /* Note: libhw won't allow for the use of ports higher than
         * GPIO_PORTE without modification of the above indexing
         * algorithm */
    default: postpone_death();
    }
    return &(hw_driver_singleton(type)->channels[idx]);
}

inline
hw_driver* hw_driver_singleton(HW_TYPE type) {

    switch(type) {
    case HW_UART:   return &HW_UART_DRIVER;
    case HW_TIMER:  return &HW_TIMER_DRIVER;
    case HW_BUTTON: return &HW_BUTTON_DRIVER;
    default:        postpone_death();
    }
    return NULL;
}

/*----------------------------------------------------------------------------*
 *                        Interrupt Service Routines                          *
 *----------------------------------------------------------------------------*/

/*! GPIO PortF (includes on-board buttons) isr responsible for
 *  notifying all subscriptions with information describing the
 *  interrupt. */
void GPIOPortF_Handler(void) {

    GPIOIntClear(GPIO_PORTF_BASE, BUTTONS_BOTH);

    /* This is the only line needed for test-lab4 */
    /* ++HW_BUTTON_RAW_SEM; */

    /* this is for test-ping */
    sem_signal(sem_button_debounce);
}


/* These ISRs were generated programatically -- see
 * /bin/lisp/rtos-interrupt-generator.el */

/*! UART0 isr responsible for notifying all subscriptions with information
 * describing the interrupt.
 */
void UART0_Handler(void) {

    bool post;
    uint8_t recv;
    /* Get and clear the current interrupt sources */
    uint32_t interrupts = UARTIntStatus(UART0_BASE, true);
    UARTIntClear(UART0_BASE, interrupts);

    /* Are we being interrupted because the TX FIFO has space available? */
    /* if(interrupts & UART_INT_TX) { */
        /* Move as many bytes as we can into the transmit FIFO */
        /* uart_prime_transmit(UART0_BASE); */
    /* } */

    /* Are we being interrupted due to a received character? */
    if(interrupts & (UART_INT_RX | UART_INT_RT)) {
        /* Get all available chars from the UART */
        while(UARTCharsAvail(UART0_BASE)) {
            recv = (unsigned char) (UARTCharGetNonBlocking(UART0_BASE) & 0xFF);

            /* optional: check for '@echo_off */

            /* Handle backspace by erasing the last character in the
             * buffer */
            switch(recv) {
            case 127:
            case '\b':
                /* If there are any chars to delete, delete the last text */
                if(!buffer_empty(UART0_RX_BUFFER)) {
                    /* Erase previous characters on the user's terminal */
                    UARTCharPut(UART0_BASE, '\b');
                    UARTCharPut(UART0_BASE, ' ');
                    UARTCharPut(UART0_BASE, '\b');
                    /* Decrement the number of chars in the buffer */
                    buffer_dec(UART0_RX_BUFFER);
                    /* Skip ahead to next buffered char */
                    continue;
                }
                /* if it is empty, somebody is watching so pass along
                 * the backspace */
                break;

            case '\r':
            case '\n':
                if(recv == '\r') {
                    UART_LAST_WAS_CR = true;
                }
                else if (UART_LAST_WAS_CR) {
                    UART_LAST_WAS_CR = false;
                    /* Don't react twice to a single newline */
                    continue;
                }
            case 0x1b:
                /* Regardless of the newline received, our convention
                 * is to mark end-of-lines in a buffer with the CR
                 * character. */
                recv = '\r';

                /* Echo the received character to the newline */
                UARTCharPut(UART0_BASE, '\n');
                break;

            default: break;
            }

            post = !buffer_full(UART0_RX_BUFFER);
            /* If there is room in the RX FIFO, store the char there,
             * else dump it. optional: a circular buffer might keep
             * more up-to-date data, considering this is a RTOS */
            /* this could be cleaned up with error-catching in the buffer library */
            if(post) {
                buffer_add(UART0_RX_BUFFER, recv);
                sem_post(HW_SEM_UART0);
            }
        }
    }
}


/*! TIMER0A isr responsible for notifying all subscriptions with
 * information describing the interrupt.
 *
 * This isr was generated
 * automatically by bin/lisp/rtos-interrupt-generator.el
 */
void TIMER0A_Handler(void) {

  TimerIntClear(TIMER0_BASE, TIMER_TIMA_TIMEOUT);
  notification_init(int, 1);
  timer_metadata_init(TIMER0_BASE, NULL, NULL, NULL);
  hw_notify(HW_TIMER, timer_metadata, note);
}

void ADC0Seq0_Handler(void) {

    ADCIntClear(ADC0_BASE, 0);
    ADCSequenceDataGet(ADC0_BASE, 3, ADC0_SEQ3_SAMPLES);
}

void ADC0Seq1_Handler(void) {

    ADCIntClear(ADC0_BASE, 1);
    ADCSequenceDataGet(ADC0_BASE, 3, ADC0_SEQ3_SAMPLES);
}

void ADC0Seq3_Handler(void) {

    ADCIntClear(ADC0_BASE, 3);
    ADCSequenceDataGet(ADC0_BASE, 3, ADC0_SEQ3_SAMPLES);
    /* GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_1, GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_1) ^ GPIO_PIN_1); */
}
