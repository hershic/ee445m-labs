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
void blink_onboard_led(void) {
    heart_beat();
    heart_toggle();
}

/*! Accept input on UART 0, and parrot input back out to UART 0.
 * \return Exit status
 */
int main(void) {

    FPUEnable();
    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    /* Enable the GPIO port that is used for the on-board LED. */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);

    /* Enable the GPIO pins for the LED (PF2). */
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_2);

    /* Enable processor interrupts. */
    IntMasterEnable();

    hw_driver_init(HW_TIMER);
    hw_metadata timer_metadata;
    timer_metadata.timer.TIMER_FREQUENCY = 1 Hz;
    hw_channel_init(HW_TIMER, TIMER0_BASE, timer_metadata);

    /* Postpone death */
    while (1) {
    }
}
