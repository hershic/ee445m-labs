/* -*- mode: c; c-basic-offset: 4; -*- */
/* Standard Libs */
#include <stdint.h>
#include <stdbool.h>

/* TI Includes */
#include "inc/hw_ints.h"
#include "inc/hw_memmap.h"

/* Driverlib Includes */
#include "driverlib/debug.h"
#include "driverlib/fpu.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/timer.h"
#include "driverlib/rom.h"

#include "libut/utlist.h"
#include "libhw/hardware.h"

/* Supported devices */
/* #include "libuart/uart.h" */
#include "libtimer/timer.h"
#include "libbutton/button.h"

/* Each driver is statically allocated */
hw_driver HW_UART_DRIVER;
hw_driver HW_TIMER_DRIVER;
hw_driver HW_BUTTON_DRIVER;

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

void hw_daemon(void) {
    while (1) {
        sem_check(uart_binary_semaphore) {
            GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2,
                         GPIO_PIN_1 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_1));
            /* TODO: Do something interesting */
        }
    }
}

/* immaculate hashing function, much fast */
inline
hw_channel* _hw_get_channel(HW_TYPE type, hw_metadata metadata) {

    memory_address_t idx;
    switch(type){
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

    notification_init(int, GPIOPinRead(GPIO_PORTF_BASE, BUTTONS_BOTH));
    button_metadata_init(GPIO_PORTF_BASE, BUTTONS_BOTH, NULL);
    hw_notify(HW_BUTTON, button_metadata, note);
}


/* These ISRs were generated programatically -- see
 * /bin/lisp/rtos-interrupt-generator.el */

/*! UART0 isr responsible for notifying all subscriptions with information
 * describing the interrupt.
 *
 * This isr was generated
 * automatically by bin/lisp/rtos-interrupt-generator.el
 */
void UART0_Handler(void) {
    sem_post(uart_binary_semaphore);
}

/*! UART1 isr responsible for notifying all subscriptions with information
 * describing the interrupt.
 *
 * This isr was generated
 * automatically by bin/lisp/rtos-interrupt-generator.el
 */
void UART1_Handler(void) {

  unsigned short i;
  notification note;
  hw_metadata metadata;

  /* TODO: determine which bit to clear:  */
  unsigned long look_at_me = UARTIntStatus();
  /* UARTIntClear(UART1_BASE, ); */

  metadata.uart.channel = UART1_BASE;

  while(UARTCharsAvail(UART1_BASE)) {

    /* Notify every subscribed task of each incoming character
     * (but schedule them for later so we can return from this ISR
     * asap). */
    note._char = uart_get_char();

    /* TODO: schedule this thread instead of running it immediately */
    hw_notify(HW_UART, metadata, note);
  }
}

/*! UART2 isr responsible for notifying all subscriptions with information
 * describing the interrupt.
 *
 * This isr was generated
 * automatically by bin/lisp/rtos-interrupt-generator.el
 */
void UART2_Handler(void) {

  unsigned short i;
  notification note;
  hw_metadata metadata;

  /* TODO: determine which bit to clear:  */
  unsigned long look_at_me = UARTIntStatus();
  /* UARTIntClear(UART2_BASE, ); */

  metadata.uart.channel = UART2_BASE;

  while(UARTCharsAvail(UART2_BASE)) {

    /* Notify every subscribed task of each incoming character
     * (but schedule them for later so we can return from this ISR
     * asap). */
    note._char = uart_get_char();

    /* TODO: schedule this thread instead of running it immediately */
    hw_notify(HW_UART, metadata, note);
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

/*! TIMER1A isr responsible for notifying all subscriptions with
 * information describing the interrupt.
 *
 * This isr was generated
 * automatically by bin/lisp/rtos-interrupt-generator.el
 */
void TIMER1A_Handler(void) {

  TimerIntClear(TIMER1_BASE, TIMER_TIMA_TIMEOUT);
  notification_init(int, 1);
  timer_metadata_init(TIMER1_BASE, NULL, NULL, NULL);
  hw_notify(HW_TIMER, timer_metadata, note);
}

/*! TIMER2A isr responsible for notifying all subscriptions with
 * information describing the interrupt.
 *
 * This isr was generated
 * automatically by bin/lisp/rtos-interrupt-generator.el
 */
void TIMER2A_Handler(void) {

  TimerIntClear(TIMER2_BASE, TIMER_TIMA_TIMEOUT);
  notification_init(int, 1);
  timer_metadata_init(TIMER2_BASE, NULL, NULL, NULL);
  hw_notify(HW_TIMER, timer_metadata, note);
}
