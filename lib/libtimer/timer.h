/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __TIMER__
#define __TIMER__

#include <stdint.h>
#include <stdbool.h>

#include "libhw/hardware.h"

#define Hz

/*! Create a \hw_metadata struct named \_name. */
#define timer_metadata_init_(_name, _base, _frequency, _interrupt, _periodic) \
    hw_metadata _name;							\
    _name.timer = (hw_timer_metadata) {					\
        .base      = (memory_address_t) _base,				\
        .frequency = (uint32_t)  _frequency,				\
        .interrupt = (uint32_t)  _interrupt,				\
        .periodic  = (uint32_t)  _periodic				\
    }

/*! Create a \hw_metadata struct named `timer_metadata'. */
#define timer_metadata_init(_base, _frequency, _interrupt, _periodic) \
    timer_metadata_init_(timer_metadata, _base, _frequency, _interrupt, _periodic)

/*! Schedule a task (function) to be invoked periodically.
 * \param Metadata regarding the timer peripheral and isr to use.
 */
void timer_add_interrupt(hw_metadata);

#endif  /* __TIMER__ */
