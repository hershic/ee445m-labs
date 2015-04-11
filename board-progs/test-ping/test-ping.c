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

#define led_toggle(port, pin) \
    GPIOPinWrite(port, pin, pin ^ GPIOPinRead(port, pin))

/*! Ping))) Control */
semaphore_t sem_ping;
uint32_t ping_time = 0xDEADBEEF;

uint32_t button_left_pressed;
uint32_t button_right_pressed;

uint32_t button_debounced_mailbox;
uint32_t button_debounced_wtf;

extern semaphore_t sem_button_debounce;


/*! Sample the Ping))) Sensor */
int sample(void) {

    uint32_t counter;

    while(true) {
        sem_guard(sem_ping) {
            sem_take(sem_ping);

            /* Set Ping))) SIG to output */
            GPIOIntDisable(GPIO_PORTB_BASE, GPIO_INT_PIN_0);
            GPIOPinTypeGPIOOutput(GPIO_PORTB_BASE, GPIO_PIN_0);

            /* Set SIG high for 5usec */
            GPIOPinWrite(GPIO_PORTB_BASE, GPIO_PIN_0, 1);
            /* Delay1us(5); */
            while(counter < 4){
                counter++;
            }
            counter = 0;

            GPIOPinWrite(GPIO_PORTB_BASE, GPIO_PIN_0, 0);

            /* Set Ping))) SIG to input */
            GPIOIntEnable(GPIO_PORTB_BASE, GPIO_INT_PIN_0);
            IntEnable(INT_GPIOB);

            /* Reconfigure PB0 as edge-triggered input */
            GPIOPinTypeGPIOInput(GPIO_PORTB_BASE, GPIO_PIN_0);
            /* begin timer init */
            timer_metadata_init(TIMER1_BASE, 0x0fffffff, INT_TIMER1A, TIMER_CFG_ONE_SHOT_UP);
            timer_metadata.timer.subtimer = TIMER_A | TIMER_B;
            hw_driver_init(HW_TIMER, timer_metadata);
            timer_add_interrupt(timer_metadata);
            /* end timer init */
        }
        os_surrender_context();
    }
}

void TIMER1A_Handler() {
  TimerIntClear(TIMER1_BASE, TIMER_TIMA_TIMEOUT);
  uint32_t test = TimerValueGet(TIMER0_BASE, TIMER_A);
}

void button_debounce_end(notification button_notification) {

    button_debounced_mailbox = GPIOPinRead(GPIO_PORTF_BASE, BUTTONS_BOTH);
    sem_post(sem_ping);
}

/* what the btn handler calls */
void button_debounce_start(notification button_notification) {

    button_debounced_wtf = GPIOPinRead(GPIO_PORTF_BASE, BUTTONS_BOTH);
    timer_metadata_init(TIMER0_BASE, 10 Hz, INT_TIMER0A, TIMER_CFG_ONE_SHOT);
    hw_channel_init(HW_TIMER, timer_metadata);
    hw_subscribe_single_shot(HW_TIMER, timer_metadata,
                             button_debounce_end);
}

/* Record how long the Ping))) took to respond */
int GPIOPortB_Handler() {

    GPIOIntClear(GPIO_PORTB_BASE, GPIO_PIN_0);

    ping_time = TimerValueGet(TIMER0_BASE, TIMER_A);
    /* TimerDisable(TIMER0_BASE, TIMER_A); */
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

int schedule_sample() {
    sem_signal(sem_ping);
}

int main(void) {

    hw_metadata metadata;

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    /* Enable processor interrupts */
    IntMasterDisable();

    /* Enable the Ping))) peripheral */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOB);

    /* Initialize Ping))) semaphore */
    sem_init(sem_ping);

    /* begin timer init */
    timer_metadata_init(TIMER0_BASE, 10 Hz, INT_TIMER0A, TIMER_CFG_ONE_SHOT);
    hw_driver_init(HW_TIMER, timer_metadata);
    /* end timer init */

    /* hearts init -- all are outputs */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_1);
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_2);
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_3);
    /* end hearts init */

    /* button init */
    button_debounced_mailbox = 0xff;
    sem_init(sem_button_debounce);

    button_metadata_init_(portf, GPIO_PORTF_BASE, BUTTONS_BOTH, GPIO_BOTH_EDGES);

    hw_init(HW_BUTTON, portf);
    /* end button init */

    os_threading_init();
    schedule(hw_daemon, 100 Hz, DL_SOFT);
    schedule(button_debounce_daemon, 100 Hz, DL_SOFT);
    schedule(sample, 100 Hz, DL_SOFT);

    system_init();
    system_register_command((const char*) "s", schedule_sample);

    uart_metadata_init(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
    hw_init(HW_UART, uart_metadata);

    /* Initialize the shell and the system it interacts with */
    shell_spawn();

    IntMasterEnable();
    os_launch();

    /* main never terminates */
    while (1);
}
