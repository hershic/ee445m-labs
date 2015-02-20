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
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/systick.h"
#include "driverlib/rom.h"

#include "libos/os.h"
#include "libio/button.h"

uint32_t count = 0;

void GPIO_PortF_Handler(void) {

    GPIOIntClear(GPIO_PORTF_BASE, GPIO_PIN_4);
    count++;
}

int main() {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    IntMasterDisable();

    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);
    GPIOPinTypeGPIOInput(GPIO_PORTF_BASE, GPIO_PIN_4);
    GPIODirModeSet(GPIO_PORTF_BASE, GPIO_PIN_4, GPIO_DIR_MODE_IN);
    GPIOIntTypeSet(GPIO_PORTF_BASE, GPIO_PIN_4, GPIO_FALLING_EDGE);
    GPIOIntEnable(GPIO_PORTF_BASE, GPIO_PIN_4);

    /* Load and enable the systick timer */
    SysTickPeriodSet(SysCtlClockGet() / 10);
    SysTickEnable();
    SysTickIntEnable();

    /* PONDER: why do interrupts fire without this? */
    IntMasterEnable();
    postpone_death();
}
