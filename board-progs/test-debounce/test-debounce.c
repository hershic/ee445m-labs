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

static uint32_t button_left_pressed;
static uint32_t button_right_pressed;

void update_pid(notification button_bitmask) {
    /* What was that technique we learned in 460n to avoid a bunch of
     branches? cond && action*/
    if (button_bitmask._int & BUTTON_LEFT) {
        button_left_pressed++;
    }
    if (button_bitmask._int & BUTTON_RIGHT) {
        button_right_pressed++;
    }
}

/* what the btn handler calls */
void button_debounce_start() {
    timer_metadata_init(TIMER0_BASE, 1 Hz, INT_TIMER0A, TIMER_CFG_ONE_SHOT);
    hw_init(HW_TIMER, timer_metadata);
    hw_subscribe(HW_TIMER, timer_metadata, update_pid);
}

void postpone_suicide() {
    while (1) {}
}

int main() {

    button_left_pressed = 0;
    button_right_pressed = 0;

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    IntMasterDisable();

    /* button init */
    button_metadata_init(GPIO_PORTF_BASE, BUTTONS_BOTH, GPIO_BOTH_EDGES);
    hw_init(HW_BUTTON, button_metadata);
    hw_subscribe(HW_BUTTON, button_metadata, button_debounce_start);
    /* hw_subscribe(HW_BUTTON, button_metadata, update_pid); */

    IntMasterEnable();

    postpone_suicide();
}
