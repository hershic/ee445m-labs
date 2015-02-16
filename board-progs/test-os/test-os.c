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

#include <sys/stat.h>

void turn_on_led() {
    while (1) {
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, GPIO_PIN_2);
    }
}

void turn_off_led() {
    while (1) {
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, 0);
    }
}

/* VALVANO CRAP */
uint32_t Count1, Count2, Count3;
void Thread1b(void){
    Count1 = 0;
    for(;;){
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_4, GPIO_PIN_4 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_4));
        Count1++;
    }
}
void Thread2b(void){
    Count2 = 0;
    for(;;){
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_3, GPIO_PIN_3 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_3));
        Count2++;
    }
}
void Thread3b(void){
    Count3 = 0;
    for(;;){
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, GPIO_PIN_2 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2));
        Count3++;
    }
}
/* END VALVANO CRAP */

int main() {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    /* Enable the GPIO port that is used for the on-board LED. */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);

    /* Enable the GPIO pins for the LED (PF2). */
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_2);
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_3);
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_4);

    IntMasterDisable();

    os_threading_init();
    /* os_add_thread(turn_on_led); */
    /* os_add_thread(turn_off_led); */
    os_add_thread(Thread1b);
    os_add_thread(Thread2b);
    os_add_thread(Thread3b);

    /* Load and enable the systick timer */
    SysTickPeriodSet(SysCtlClockGet() / 10);
    SysTickEnable();
    SysTickIntEnable();

    os_launch();

    /* And we're done; this should never execute */
    while (1) {}
}
