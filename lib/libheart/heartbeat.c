/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Eric Crosson 2015-02-10 */

#include "heartbeat.h"

inline
void heart_init() {

    /* Enable the GPIO port that is used for the on-board LED. */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);

    /* Enable the GPIO pins for the LED (PF2). */
    GPIOPinTypeGPIOOutput(THORACIC_CAVITY, HEART_MUSCLE);
}

inline
int32_t heart_status() {GPIOPinRead(THORACIC_CAVITY, HEART_MUSCLE);}

inline
void heart_off() {GPIOPinWrite(THORACIC_CAVITY, HEART_MUSCLE, 0);}

inline
void heart_on() {GPIOPinWrite(THORACIC_CAVITY, HEART_MUSCLE, 1);}

inline
void heart_toggle() {

    GPIOPinWrite(THORACIC_CAVITY, HEART_MUSCLE, heart_status() ^ HEART_MUSCLE);
}

inline
void heart_beat() {

    GPIOPinWrite(THORACIC_CAVITY, HEART_MUSCLE, heart_status() ^ HEART_MUSCLE);
    GPIOPinWrite(THORACIC_CAVITY, HEART_MUSCLE, heart_status() ^ HEART_MUSCLE);
}

/*--------------------------------------------------------------*
 * Begin non-modal API -- implicit subject is \HEART_MUSCLE.    *
 *--------------------------------------------------------------*/

inline
void heart_init_(memory_address_t base, memory_address_t pin) {

    /* Enable the GPIO port that is used for \HEART_ANCILLARY_MUSCLE. */
    switch(base) {
    case GPIO_PORTA_BASE: SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOA); break;
    case GPIO_PORTB_BASE: SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOB); break;
    case GPIO_PORTC_BASE: SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOC); break;
    case GPIO_PORTD_BASE: SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOD); break;
    case GPIO_PORTE_BASE: SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOE); break;
    case GPIO_PORTF_BASE: SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF); break;
    case GPIO_PORTG_BASE: SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOG); break;
    case GPIO_PORTH_BASE: SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOH); break;
    case GPIO_PORTJ_BASE: SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOJ); break;
    }

    /* Enable the GPIO pins for \HEART_ANCILLARY_MUSCLE. */
    GPIOPinTypeGPIOOutput(base, pin);

    /* Save metadata allowing for modal use of this library */
    HEART_MODAL_METADATA[os_running_thread_id()].base = base;
    HEART_MODAL_METADATA[os_running_thread_id()].pin = pin;
}

inline
int32_t heart_status_modal(muscle_t* ancillary_muscle) {

    GPIOPinRead(ancillary_muscle->base, ancillary_muscle->pin);
}

inline
int32_t heart_status_() {

    heart_status_modal(&HEART_MODAL_METADATA[os_running_thread_id()]);
}

inline
void heart_off_modal(muscle_t* ancillary_muscle) {

    GPIOPinWrite(ancillary_muscle->base, ancillary_muscle->pin, 0);
}

inline
void heart_off_() {

    heart_off_modal(&HEART_MODAL_METADATA[os_running_thread_id()]);
}

inline
void heart_on_modal(muscle_t* ancillary_muscle) {

    GPIOPinWrite(ancillary_muscle->base, ancillary_muscle->pin, 1);
}

inline
void heart_on_() {

    heart_on_modal(&HEART_MODAL_METADATA[os_running_thread_id()]);
}

inline
void heart_toggle_modal(muscle_t* ancillary_muscle) {

    GPIOPinWrite(ancillary_muscle->base, ancillary_muscle->pin,
		 heart_status_(ancillary_muscle->base,
			       ancillary_muscle->pin) ^ ancillary_muscle->pin);
}

inline
void heart_toggle_() {

    heart_toggle_modal(&HEART_MODAL_METADATA[os_running_thread_id()]);
}

inline
void heart_beat_modal(muscle_t* ancillary_muscle) {

    GPIOPinWrite(ancillary_muscle->base, ancillary_muscle->pin,
		 heart_status_(ancillary_muscle->base,
			       ancillary_muscle->pin) ^ ancillary_muscle->pin);
    GPIOPinWrite(ancillary_muscle->base, ancillary_muscle->pin,
		 heart_status_(ancillary_muscle->base,
			       ancillary_muscle->pin) ^ ancillary_muscle->pin);
}

inline
void heart_beat_() {

    heart_beat_modal(&HEART_MODAL_METADATA[os_running_thread_id()]);
}

/*--------------------------------------------------------------*
 * End GPIO API                                                 *
 *--------------------------------------------------------------*/

inline
muscle_t* heart_hew_muscle_(muscle_t*        muscle,
			    memory_address_t base,
			    memory_address_t pin) {

    /* Initialize the muscle_t data structure */
    muscle->base = base;
    muscle->pin  = pin;
    /* Ensure this peripheral is initialized properly */
    heart_init_(base, pin);
    /* A convenience for the client developer */
    return muscle;
}
