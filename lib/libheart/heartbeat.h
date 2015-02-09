/* -*- mode: c; c-basic-offset: 4; -*- */
/* TODO: doxygenize */
#ifndef __HEARTBEAT__
#define __HEARTBEAT__

#include "driverlib/rom.h"

inline
void heart_init() {

    /* Enable the GPIO port that is used for the on-board LED. */
    ROM_SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);

    /* Enable the GPIO pins for the LED (PF2). */
    ROM_GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_2);
}

inline
void heart_off() {

    ROM_GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, 0);
}

inline
void heart_on() {

    ROM_GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, GPIO_PIN_2);
}

inline
void heart_toggle() {

    ROM_GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2,
                     ROM_GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2) ^ GPIO_PIN_2);
}

inline
void heart_beat() {

    heart_toggle();
    heart_toggle();
}

#endif
