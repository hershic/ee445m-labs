/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson on 2015-01-24 */
/* Revision History: Look in Git FGT */

/* Standard Libs */
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/* TI Includes */
#include "inc/hw_ints.h"
#include "inc/hw_gpio.h"
#include "inc/hw_memmap.h"

/* Driverlib Includes */
#include "driverlib/debug.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/systick.h"
#include "driverlib/timer.h"

#include "libos/os.h"
#include "libbutton/button.h"
#include "libtimer/timer.h"
#include "libhw/hardware.h"
#include "libhw/semaphore.h"

static uint32_t button_left_pressed;
static uint32_t button_right_pressed;

static uint32_t button_debounced_mailbox;

static sem_t button_debounced_sem;

void button_debounce_end(notification button_notification) {
    /* What was that technique we learned in 460n to avoid a bunch of
     branches? cond && action*/

    button_debounced_mailbox = *(button_notification.raw_data);
    sem_post(&button_debounced_sem);
}

/* what the btn handler calls */
void button_debounce_start(notification button_notification) {
    timer_metadata_init(TIMER0_BASE, 10 Hz, INT_TIMER0A, TIMER_CFG_ONE_SHOT);
    hw_init(HW_TIMER, timer_metadata);
    hw_subscribe_single_shot(HW_TIMER, timer_metadata,
                             button_debounce_end, &binary_notification);
}

 /* THREAD CLIENT CODE */
void postpone_suicide() {
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_3);
    sem_t* button_debounced_new_data = &button_debounced_sem;
    uint32_t* button_debounced_data = &button_debounced_mailbox;

    while (1) {

        /* blocks */
        sem_wait(&button_debounced_sem);
        /* if (*button_debounced_data & BUTTONS_BOTH) { */
            GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_1, GPIO_PIN_1 ^
                         GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_1));
        /* } */


    }
}

int main() {

    button_left_pressed = 0;
    button_right_pressed = 0;

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    IntMasterDisable();

    /* button init */
    button_metadata_init(GPIO_PORTF_BASE, BUTTONS_BOTH, GPIO_RISING_EDGE);
    hw_init(HW_BUTTON, button_metadata);
    hw_subscribe(HW_BUTTON, button_metadata, button_debounce_start, &gpio_portf_mailbox);
    /* hw_subscribe(HW_BUTTON, button_metadata, update_pid); */

    IntMasterEnable();

    postpone_suicide();
}
