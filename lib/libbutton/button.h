/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __BUTTON__
#define __BUTTON__

#include "libstd/nexus.h"
#include "libhw/hardware.h"

/*! Create a \hw_metadata struct named \_name. */
#define button_metadata_init_(_name, _base, _pin, _interrupt)	\
    hw_metadata _name;						\
    _name.button = (hw_button_metadata) {			\
	.base     = (memory_address_t) _base,			\
	.pin      = (memory_address_t) _pin,			\
	.interrupt = (uint32_t) _interrupt			\
    }


/*! Create a hardware_metadata struct named `button_metadata' */
#define button_metadata_init(_base, _pin, _interrupt) \
    button_metadata_init_(button_metadata, _base, _pin, _interrupt)

#define BUTTON_LEFT          GPIO_PIN_4
#define BUTTON_RIGHT         GPIO_PIN_0
#define BUTTONS_BOTH         (BUTTON_LEFT | BUTTON_RIGHT)

/*! Initialize buttons for use as input.
 * \note Currently only supports on-board peripherals.
 */
void button_init(hw_metadata);

/*! Subscribe to a hardware interrupt.
 * \param Hardware target to subscribe to and isr to invoke upon
 * interrupt
 */
void button_set_interrupt(hw_metadata);

#endif  /* __BUTTON__ */
