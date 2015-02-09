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
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/rom.h"

#include "libtimer/timer.h"

#include <sys/stat.h>

/*! \brief Blink the onboard LED three times */
void blink_onboard_led(void) {
    /* Toggle the LED three times to show activity. */
    ROM_GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, ROM_GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2) ^ GPIO_PIN_2);
    ROM_GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, ROM_GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2) ^ GPIO_PIN_2);
    ROM_GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, ROM_GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2) ^ GPIO_PIN_2);
}

/*! Accept input on UART 0, and parrot input back out to UART 0.
 * \return Exit status
 */
int main(void) {

    ROM_SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                       SYSCTL_XTAL_16MHZ);

    /* Enable the GPIO port that is used for the on-board LED. */
    ROM_SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);

    /* Enable the GPIO pins for the LED (PF2). */
    ROM_GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_2);

    /* Enable the peripherals used by this example. */
    ROM_SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER2);

    /* Enable processor interrupts. */
    ROM_IntMasterEnable();

    timer_add_periodic_thread(blink_onboard_led, 1, 1, TIMER0);

    /* Postpone death */
    while (1) {
    }
}
