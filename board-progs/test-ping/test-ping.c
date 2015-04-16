/* Created by Hershal Bhave and Eric Crosson on 2015-01-25 */
/* Revision History: Look in Git FGT */

/* Standard Libs */
#include <stdint.h>
#include <stdbool.h>

/* TI Includes */
#include "inc/hw_ints.h"
#include "inc/hw_memmap.h"

/* Driverlib Includes */
#include "driverlib/debug.h"
#include "driverlib/adc.h"
#include "driverlib/gpio.h"
#include "driverlib/timer.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/can.h"

#include "libos/os.h"
#include "libhw/hardware.h"
#include "libstd/nexus.h"
#include "libuart/uart.h"

#include "libtimer/timer.h"
#include "libbutton/button.h"
#include "libos/thread_structures.h"

#define led_toggle(port, pin)                                   \
    GPIOPinWrite(port, pin, pin ^ GPIOPinRead(port, pin))

/*! Ping))) Control */
semaphore_t sem_ping;
semaphore_t sem_ping_do_avg;
uint32_t ping_idx = 0;
#define ping_samples_to_avg 5
bool ping_sample_ready = false;
uint32_t ping_avg;
uint32_t ping_time[ping_samples_to_avg];

uint32_t button_left_pressed;
uint32_t button_right_pressed;

uint32_t button_debounced_mailbox;
uint32_t button_debounced_wtf;

uint32_t timer_overflow;

extern semaphore_t sem_button_debounce;

typedef enum ping_status {
    ping_not_active,
    ping_signal,
    ping_response,
} ping_status_t;

ping_status_t ping_status;

/*! Sample the Ping))) Sensor */
int sample(void) {

    uint32_t counter;
    ping_status = ping_not_active;

    while(true) {
        sem_guard(sem_ping) {
            sem_take(sem_ping);
            IntMasterDisable();

            /* TODO: is this a 5V tolerant pin? */
            /* Set Ping))) SIG to output */
            GPIOIntDisable(GPIO_PORTB_BASE, GPIO_INT_PIN_0);
            GPIOPinTypeGPIOOutput(GPIO_PORTB_BASE, GPIO_PIN_0);

            /* Set SIG high for 5usec */
            GPIOPinWrite(GPIO_PORTB_BASE, GPIO_PIN_0, 1);
            /* Delay1us(5); */
            while(counter < 4){counter++;}
            counter = 0;

            GPIOPinWrite(GPIO_PORTB_BASE, GPIO_PIN_0, 0);

            /* Set Ping))) SIG to input */
            GPIOPinTypeGPIOInput(GPIO_PORTB_BASE, GPIO_PIN_0);
            GPIOIntTypeSet(GPIO_PORTB_BASE, GPIO_PIN_0, GPIO_BOTH_EDGES);
            GPIOIntEnable(GPIO_PORTB_BASE, GPIO_PIN_0);
            IntEnable(INT_GPIOB_TM4C123);
            IntEnable(INT_GPIOB);

            IntMasterEnable();
        }
        os_surrender_context();
    }
}

inline
int schedule_sample() {
    sem_signal(sem_ping);
}

void TIMER1A_Handler() {
    TimerIntClear(TIMER1_BASE, TIMER_TIMA_TIMEOUT);
    uint32_t test = TimerValueGet(TIMER1_BASE, TIMER_A);
    ++timer_overflow;
}

void button_debounce_end(notification button_notification) {

    button_debounced_mailbox = GPIOPinRead(GPIO_PORTF_BASE, BUTTONS_BOTH);
    schedule_sample();
}

/* what the btn handler calls */
void button_debounce_start(notification button_notification) {

    button_debounced_wtf = GPIOPinRead(GPIO_PORTF_BASE, BUTTONS_BOTH);
    timer_metadata_init(TIMER0_BASE, 10 Hz, INT_TIMER0A, TIMER_CFG_ONE_SHOT);
    hw_channel_init(HW_TIMER, timer_metadata);
    hw_subscribe_single_shot(HW_TIMER, timer_metadata, button_debounce_end);
}

/* Record how long the Ping))) took to respond */
int GPIOPortB_Handler() {

    GPIOIntClear(GPIO_PORTB_BASE, GPIO_PIN_0);

    ++ping_status;
    timer_overflow = 0;

    if (ping_status == ping_signal) {
        /* begin timer init */
        timer_metadata_init(TIMER1_BASE, 0, INT_TIMER1A, TIMER_CFG_PERIODIC_UP);
        timer_metadata.timer.subtimer = TIMER_A;
        hw_driver_init(HW_TIMER, timer_metadata);
        timer_add_interrupt(timer_metadata);
        TimerLoadSet(TIMER1_BASE, TIMER_A, 0x0fffffe);
        /* ping_time[ping_idx++] = TimerValueGet(TIMER1_BASE, TIMER_A); */
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_3, GPIO_PIN_3);
        /* end timer init */
    } else if (ping_status == ping_response) {
        ping_time[ping_idx++] = TimerValueGet(TIMER1_BASE, TIMER_A);
        ping_status = ping_not_active;
        TimerDisable(TIMER1_BASE, TIMER_A);
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_3, 0);
    }

    sem_post(sem_ping_do_avg);
}

/* Exists to decouple work form the GPIOPortB_Handler ISR */
void ping_average_samples() {

    while(1) {
        sem_guard(sem_ping_do_avg) {
            sem_take(sem_ping_do_avg);

            /* Each sample of the Ping))) triggers \ping_samples_to_avg
             * samples and averages the results */
            if (ping_idx == ping_samples_to_avg) {
                uint32_t sample_sum;
                for(ping_idx = 0; ping_idx <= ping_samples_to_avg; ++ping_idx) {
                    /* TODO: guarantee no overflow */
                    sample_sum += ping_time[ping_idx];
                }
                ping_idx = 0;
                ping_avg = sample_sum/ping_samples_to_avg;
                uart_send_string("averaged sample))) ");
                uart_send_udec(ping_avg);
                ping_sample_ready = true;
            } else {
                uart_send_udec(ping_time[ping_idx]);
                schedule_sample();
            }
        }
        os_surrender_context();
    }
}

void button_debounce_daemon() {

    int32_t button_raw_data = 0xff;

    while (1) {
        sem_guard(sem_button_debounce) {
            sem_take(sem_button_debounce);
            button_raw_data = GPIOPinRead(GPIO_PORTF_BASE, BUTTONS_BOTH);

            if (~button_raw_data & BUTTON_LEFT) {
                led_toggle(GPIO_PORTF_BASE, GPIO_PIN_2);
            }
            if (~button_raw_data & BUTTON_RIGHT) {
                led_toggle(GPIO_PORTF_BASE, GPIO_PIN_2);
            }
            schedule_sample();
        }
        os_surrender_context();
    }
}

int main(void) {

    hw_metadata metadata;

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    /* Enable processor interrupts */
    IntMasterDisable();

    /* begin initialize ping))) */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOB);
    sem_init(sem_ping);
    sem_init(sem_ping_do_avg);
    /* end initialize ping))) */

    /* begin hearts init */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_3);
    /* end hearts init */

    /* button init */
    button_debounced_mailbox = 0xff;
    sem_init(sem_button_debounce);
    button_anon_init(GPIO_PORTF_BASE, BUTTONS_BOTH, GPIO_BOTH_EDGES);
    /* end button init */

    /* begin uart init */
    uart_anon_init(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
    /* end uart init */

    /* begin timer init */
    timer_anon_init(TIMER0_BASE, 10 Hz, INT_TIMER0A, TIMER_CFG_ONE_SHOT);
    /* end timer init */

    /* begin shell init */
    system_init();
    system_register_command((const char*) "s", schedule_sample);
    shell_spawn();
    /* end shell init */

    /* begin os init */
    os_threading_init();
    sched(hw_daemon);
    sched(button_debounce_daemon);
    sched(sample);
    sched(ping_average_samples);
    os_launch();
    /* end os init */

    /* main never terminates */
    while (1);
}
