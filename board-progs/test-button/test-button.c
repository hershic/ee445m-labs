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
#include "driverlib/rom.h"

#include "libos/os.h"
#include "libbutton/button.h"
#include "libhw/hardware.h"

uint32_t button_left_pressed = 0;
uint32_t button_right_pressed = 0;

void update_pid(int32_t button_bitmask) {
    if (button_bitmask & BUTTON_LEFT) {
        button_left_pressed++;
    }
    if (button_bitmask & BUTTON_RIGHT) {
        button_right_pressed++;
    }
}

int main() {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    IntMasterDisable();

    hw_driver_init(HW_BUTTON);

    hw_metadata button_metadata;
    button_metadata.button.base = GPIO_PORTF_BASE;
    button_metadata.button.pin = BUTTONS_ALL;
    button_metadata.button.int_type = GPIO_BOTH_EDGES;

    hw_channel_init(HW_BUTTON, GPIO_PORTF_BASE, button_metadata);
    hw_connect(HW_BUTTON, GPIO_PORTF_BASE, update_pid);

    IntMasterEnable();

    postpone_death();
}
