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

#include "libdisplay/ST7735.h"

#include <sys/stat.h>

/*! Accept input on UART 0, and parrot input back out to UART 0.
 * \return Exit status
 */
int main(void) {

    int j;
    FPUEnable();
    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    /* Enable the GPIO port that is used for the on-board LED. */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);

    /* Enable the timer for display driver timings */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER0);

    /* Enable the GPIO pins for the LED (PF2). */
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_2);

    /* Enable processor interrupts. */
    IntMasterEnable();

    ST7735_InitR(INITR_REDTAB);

    ST7735_OutString("test on screen 0\n");
    ST7735_OutString("test on screen 0\n");

    ST7735_DrawFastHLine(0, 70, 128, ST7735_YELLOW);

    ST7735_OutString2("test on screen 1\n");
    ST7735_OutString2("test on screen 1\n");

    /* Postpone death */
    while (1) {}
}
