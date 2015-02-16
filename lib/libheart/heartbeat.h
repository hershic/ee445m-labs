/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __HEARTBEAT__
#define __HEARTBEAT__

/*!
 *  \brief Initialize the heart for visible transformation
 *  \returns void
 *  \ingroup Heart
 */
inline
void heart_init() {

    /* Enable the GPIO port that is used for the on-board LED. */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);

    /* Enable the GPIO pins for the LED (PF2). */
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_2);
}

/*!
 *  \brief Turn the heart off
 *  \returns void
 *  \ingroup Heart
 */
inline
void heart_off() {

    GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, 0);
}

/*!
 *  \brief Turn the heart on
 *  \returns void
 *  \ingroup Heart
 */
inline
void heart_on() {

    GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, 1);
}

/*!
 *  \brief Toggle the heart once
 *  \details end_state = ~starting_state
 *  \returns void
 *  \ingroup Heart
 */
inline
void heart_toggle() {

    GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2,
		 GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2) ^ GPIO_PIN_2);
}

/*!
 *  \brief Toggle the heart twice
 *  \details end_state = starting_state
 *  \returns void
 *  \ingroup Heart
 */
inline
void heart_beat() {

    /* This is unrolled -- ensure it matches \heart_toggle() */
    GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2,
		 GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2) ^ GPIO_PIN_2);
    GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2,
		 GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2) ^ GPIO_PIN_2);
}

/* TOOD: use bind (or equivalent function) to wrap a fn pointer with
 * beat(), ptr(), toggle() and return prt()'s ret value */

#endif
