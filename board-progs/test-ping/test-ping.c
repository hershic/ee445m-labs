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

/*! Ping))) Control */
semaphore_t sem_ping;
uint32_t ping_time = 0xDEADBEEF;

uint32_t button_left_pressed;
uint32_t button_right_pressed;

uint32_t button_debounced_mailbox;
uint32_t button_debounced_wtf;

semaphore_t button_debounced_new_data;

// Subroutine to wait 1 usec
// Inputs: n Number of usecs to wait
// Outputs: None
// Notes: This assumes 80 MHz system clock.
void Delay1us(uint32_t n){
  uint32_t volatile time;
  while(n){
    time = 72724*2/91000;  // 1usec, tuned at 80 MHz
    while(time){
      time--;
    }
    n--;
  }
}

/*! Sample the Ping))) Sensor */
int sample(void) {

    while(true) {
        sem_guard(sem_ping) {
            /* Set Ping))) SIG to output */
            GPIOPinTypeGPIOOutput(GPIO_PORTB_BASE, GPIO_PIN_0);
            /* Set SIG high for 5usec */
            GPIOPinWrite(GPIO_PORTB_BASE, GPIO_PIN_0, 1);
            Delay1us(5);
            GPIOPinWrite(GPIO_PORTB_BASE, GPIO_PIN_0, 0);
            /* begin timer init */
            timer_metadata_init(TIMER0_BASE, 2 Hz, INT_TIMER0A, TIMER_CFG_ONE_SHOT_UP);
            hw_driver_init(HW_TIMER, timer_metadata);
            hw_channel_init(HW_TIMER, timer_metadata);
            /* end timer init */

            /* Reconfigure PB0 as edge-triggered input */
            GPIOPinTypeGPIOInput(GPIO_PORTB_BASE, GPIO_PIN_0);
            button_metadata_init(GPIO_PORTF_BASE, BUTTONS_BOTH, GPIO_BOTH_EDGES);
            hw_init(HW_BUTTON, button_metadata);
        }
        os_surrender_context();
    }
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


void postpone_suicide() {

    while (1) {
    }
}


/* Better than Default_Handler */
int TIMER0_Handler() {

}

/* Record how long the Ping))) took torespond */
int GPIOPortB_Handler() {

    ping_time = TimerValueGet(TIMER0_BASE, TIMER_A);
    TimerDisable(TIMER0_BASE, 0xFFFFFFFF);
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

    /* button init */
    button_left_pressed = 0;
    button_right_pressed = 0;
    button_debounced_mailbox = 0xff;
    sem_init(button_debounced_new_data);

    button_metadata_init(GPIO_PORTF_BASE, BUTTONS_BOTH, GPIO_BOTH_EDGES);

    hw_init(HW_BUTTON, button_metadata);
    hw_subscribe(HW_BUTTON, button_metadata, button_debounce_start);
    /* end button init */

    os_threading_init();
    schedule(postpone_suicide, 100 Hz, DL_SOFT);

    IntMasterEnable();
    os_launch();

    /* main never terminates */
    while (1);
}
