/* -*- mode: c; c-basic-offset: 4; -*- */
/* TODO: doxygenize */
#ifndef __HEARTBEAT__
#define __HEARTBEAT__

inline
void heart_init() {

    /* Enable the GPIO port that is used for the on-board LED. */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);

    /* Enable the GPIO pins for the LED (PF2). */
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_2);
}

inline
void heart_off() {

    GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, 0);
}

inline
void heart_on() {

    GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, 1);
}

inline
void heart_toggle() {

    GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2,
		 GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2) ^ GPIO_PIN_2);
}

inline
void heart_beat() {

    heart_toggle();
    heart_toggle();
}

#endif
