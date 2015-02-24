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
#include "inc/hw_memmap.h"

/* Driverlib Includes */
#include "driverlib/debug.h"
#include "driverlib/fpu.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/rom.h"

#include "libtimer/timer.h"
#include "libheart/heartbeat.h"
#include "libhw/hardware.h"

#include <sys/stat.h>

/*! \brief Blink the onboard LED three times to show activity */
void blink_onboard_led(notification note) {
    heart_beat();
    heart_toggle();
}

/*! Accept input on UART 0, and parrot input back out to UART 0.
 * \return Exit status
 */
int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    /* Enable processor interrupts. */
    IntMasterDisable();

    heart_init();

    timer_metadata_init(TIMER0_BASE, 1 Hz, INT_TIMER0A, TIMER_CFG_PERIODIC);
    hw_init(HW_TIMER, timer_metadata);
    hw_subscribe(HW_TIMER, timer_metadata, blink_onboard_led);

    IntMasterEnable();

    postpone_death();
}
