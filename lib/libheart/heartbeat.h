/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Eric Crosson 2015-02-10
 * Note that this function looks gnarly with the #defines so the
 * client code doesn't. */
#ifndef __HEARTBEAT__
#define __HEARTBEAT__

/* Standard Libs */
#include <stdint.h>
#include <stdbool.h>

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
#include "driverlib/timer.h"
#include "driverlib/rom.h"

/*! The on-board LED colloquially referred to as the 'heart.' Does
 *  every computer have one? */
#define HEART_RED GPIO_PIN_1
#define HEART_BLUE GPIO_PIN_2
#define HEART_GREEN GPIO_PIN_3

/*! The GPIO base where the \HEART_BLUE may be found. Otherwise
 *  knows as a subdivision of the ventral body cavity which houses the
 *  heart. */
#define THORACIC_CAVITY GPIO_PORTF_BASE

/*--------------------------------------------------------------*
 * Begin non-modal API -- implicit subject is \HEART_BLUE.    *
 *--------------------------------------------------------------*/

/*!
 *  \brief Initialize \HEART_BLUE for visible transformation.
 *  \returns void
 *  \ingroup Heart
 */
inline
void heart_init() {

#if !(defined(PROFILING_DISABLE))
    /* Enable the GPIO port that is used for the on-board LED. */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);

    /* Enable the GPIO pins for the LED (PF2). */
    GPIOPinTypeGPIOOutput(THORACIC_CAVITY, HEART_BLUE);
#endif
}

/*!
 * \brief Return the status of \HEART_BLUE.
 * \returns The current status of \HEART_BLUE.
 * \ingroup Heart
 */
inline
int32_t heart_status() {

#if !(defined(PROFILING_DISABLE))
    GPIOPinRead(THORACIC_CAVITY, HEART_BLUE);
#endif
}

/*!
 *  \brief Turn \HEART_BLUE off.
 *  \returns void
 *  \ingroup Heart
 */
inline
void heart_off() {

#if !(defined(PROFILING_DISABLE))
    GPIOPinWrite(THORACIC_CAVITY, HEART_BLUE, 0);
#endif
}

/*!
 *  \brief Turn \HEART_BLUE on.
 *  \returns void
 *  \ingroup Heart
 */
inline
void heart_on() {

#if !(defined(PROFILING_DISABLE))
    GPIOPinWrite(THORACIC_CAVITY, HEART_BLUE, 1);
#endif
}

/*!
 *  \brief Toggle \HEART_BLUE once.
 *  \details end_state = ~starting_state
 *  \returns void
 *  \ingroup Heart
 */
inline
void heart_toggle() {

#if !(defined(PROFILING_DISABLE))
    GPIOPinWrite(THORACIC_CAVITY, HEART_BLUE, heart_status() ^ HEART_BLUE);
#endif
}

/*!
 *  \brief Toggle \HEART_BLUE twice.
 *  \details end_state = starting_state
 *  \returns void
 *  \ingroup Heart
 */
inline
void heart_beat() {

#if !(defined(PROFILING_DISABLE))
    GPIOPinWrite(THORACIC_CAVITY, HEART_BLUE, heart_status() ^ HEART_BLUE);
    GPIOPinWrite(THORACIC_CAVITY, HEART_BLUE, heart_status() ^ HEART_BLUE);
#endif
}

/*! Surround the execution of a code block with two hooks.
 * - pre_hook: heart_beat();
 * - post_hook: heart_toggle();
 */
#define heart_wrap(x) \
    heart_beat();     \
    x                 \
    heart_toggle();   \

/*--------------------------------------------------------------*
 * Begin GPIO API -- concentrate on a muscle and pump it baby   *
 *   One day, this section desires to be fully modal            *
 *--------------------------------------------------------------*/

/* TODO: determine how to document the HEARTBEAT_MODAL switch in
 * Doxygen. */
#ifdef HEARTBEAT_MODAL

#include "../libos/os.h"

/*! Surround the execution of a code block with two hooks.
 * - pre_hook: heart_beat_();
 * - post_hook: heart_toggle_();
 */
#define heart_wrap_(x) \
    heart_beat_();     \
    x                  \
    heart_toggle_();   \

/*! \brief An alias for \heart_init_. I like the imagery. */
#define heart_pump_(a, b) heart_init_(a, b)

/*! A pointer to a memory location on the ARM Cortex M4. */
typedef uint32_t memory_address_t;

/*! Contains information to identify a GPIO pin. This includes the
 *  port base and the specific pin. */
typedef struct muscle_t {
    memory_address_t base;
    memory_address_t pin;
} muscle_t;

static muscle_t HEART_MODAL_METADATA[SCHEDULER_MAX_THREADS];

/*!
 *  \brief Initialize \HEART_ANCILLARY_MUSCLE for visible
 *  transformation and set the modal state. That is to say, all modal
 *  function calls will act on \gpio_base and \gpio_pin until another
 *  mode is intialized.
 *  \param The muscle to use as an ancillary heart.
 *  \returns void
 *  \ingroup Heart
 */
inline
void heart_init_(memory_address_t base, memory_address_t pin) {

#if !(defined(PROFILING_DISABLE))
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
#endif
}

/*! TODO; doxygenize */
inline
int32_t heart_status_modal(muscle_t* ancillary_muscle) {

#if !(defined(PROFILING_DISABLE))
    GPIOPinRead(ancillary_muscle->base, ancillary_muscle->pin);
#endif
}

/*!
 * \brief Return the status of \HEART_ANCILLARY_MUSCLE.
 *  \param The ancillary muscle to act upon.
 * \returns The current status of \HEART_ANCILLARY_MUSCLE.
 * \ingroup Heart
 */
inline
int32_t heart_status_() {

#if !(defined(PROFILING_DISABLE))
    heart_status_modal(&HEART_MODAL_METADATA[os_running_thread_id()]);
#endif
}

/*! TODO; doxygenize */
inline
void heart_off_modal(muscle_t* ancillary_muscle) {

#if !(defined(PROFILING_DISABLE))
    GPIOPinWrite(ancillary_muscle->base, ancillary_muscle->pin, 0);
#endif
}

/*!
 *  \brief Turn \ancillary_muscle off.
 *  \param The ancillary muscle to act upon.
 *  \returns void
 *  \ingroup Heart
 */
inline
void heart_off_() {

#if !(defined(PROFILING_DISABLE))
    heart_off_modal(&HEART_MODAL_METADATA[os_running_thread_id()]);
#endif
}

/*! TODO; doxygenize */
inline
void heart_on_modal(muscle_t* ancillary_muscle) {

#if !(defined(PROFILING_DISABLE))
    GPIOPinWrite(ancillary_muscle->base, ancillary_muscle->pin, 1);
#endif
}

/*!
 *  \brief Turn \ancillary_muscle on.
 *  \param The ancillary muscle to act upon.
 *  \returns void
 *  \ingroup Heart
 */
inline
void heart_on_() {

#if !(defined(PROFILING_DISABLE))
    heart_on_modal(&HEART_MODAL_METADATA[os_running_thread_id()]);
#endif
}

/*! TODO; doxygenize */
inline
void heart_toggle_modal(muscle_t* ancillary_muscle) {

#if !(defined(PROFILING_DISABLE))
    GPIOPinWrite(ancillary_muscle->base, ancillary_muscle->pin,
                 heart_status_(ancillary_muscle->base,
                               ancillary_muscle->pin) ^ ancillary_muscle->pin);
#endif
}

/*!
 *  \brief Toggle \ancillary_muscle once.
 *  \details end_state = ~starting_state
 *  \param The ancillary muscle to act upon.
 *  \returns void
 *  \ingroup Heart
 */
inline
void heart_toggle_() {

#if !(defined(PROFILING_DISABLE))
    heart_toggle_modal(&HEART_MODAL_METADATA[os_running_thread_id()]);
#endif
}

/*! TODO; doxygenize */
inline
void heart_beat_modal(muscle_t* ancillary_muscle) {

#if !(defined(PROFILING_DISABLE))
    GPIOPinWrite(ancillary_muscle->base, ancillary_muscle->pin,
                 heart_status_(ancillary_muscle->base,
                               ancillary_muscle->pin) ^ ancillary_muscle->pin);
    GPIOPinWrite(ancillary_muscle->base, ancillary_muscle->pin,
                 heart_status_(ancillary_muscle->base,
                               ancillary_muscle->pin) ^ ancillary_muscle->pin);
#endif
}

/*!
 *  \brief Toggle \ancillary_muscle twice.
 *  \details end_state = starting_state
 *  \param The ancillary muscle to act upon.
 *  \returns void
 *  \ingroup Heart
 */
inline
void heart_beat_() {

#if !(defined(PROFILING_DISABLE))
    heart_beat_modal(&HEART_MODAL_METADATA[os_running_thread_id()]);
#endif
}

/*--------------------------------------------------------------*
 * End GPIO API                                                 *
 *--------------------------------------------------------------*/

/*!
 * \brief Allocate and initialize a muscle_t for control via libheart.
 * \details This is a convenience macro for developers. This wraps
 * \hew_heart_muscle and converts what was a three-line process to
 * allocate and initialize a muscle_t into a single call.
 * \param muscle_t to allocate and initialize
 * \param GPIO port base of the pin to target
 * \param GPIO pin on \base to control
 * \returns Initialized muscle_t from \heart_hew_muscle_
 * \note This macro creates a new muscle_t on the current function's
 * stack. Thus, according to C standards of yore this line belongs at
 * the top of your function with the rest of the memory-allocation
 * calls, before the execution code of the function.
 */
#define heart_hew_muscle(a, b, c) \
    muscle_t a;                   \
    heart_hew_muscle_(&a, b, c)

/*!
 * \brief Initialize \muscle according to \base and \pin, and
 * initialize the relevant GPIO peripherals on the Cortex M4.
 * \param Name of the muscle_t structure to allocate and initialize
 * \param GPIO port base of the pin to target
 * \param GPIO pin on \base to control
 * \returns Initialized muscle_t
 */
inline
muscle_t* heart_hew_muscle_(muscle_t*        muscle,
                            memory_address_t base,
                            memory_address_t pin) {

#if !(defined(PROFILING_DISABLE))
    /* Initialize the muscle_t data structure */
    muscle->base = base;
    muscle->pin  = pin;
    /* Ensure this peripheral is initialized properly */
    heart_init_(base, pin);
    /* A convenience for the client developer */
    return muscle;
#endif
}
#endif /* HEARTBEAT_MODAL */

#endif
