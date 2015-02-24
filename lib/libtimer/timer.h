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
    _name.timer.base = (memory_address_t) _base;			\
    _name.timer.frequency = (uint32_t)  _frequency;			\
    _name.timer.interrupt = (uint32_t)  _interrupt;			\
    _name.timer.periodic = (uint32_t)  _periodic

/*! Create a \hw_metadata struct named `timer_metadata'. */
#define timer_metadata_init(_base, _frequency, _interrupt, _periodic) \
    timer_metadata_init_(timer_metadata, _base, _frequency, _interrupt, _periodic)

/*!
 * \brief Schedule a task (function) to be invoked periodically.
 * \param Metadata regarding the timer peripheral and isr to use.
 */
void timer_add_interrupt(hw_metadata);

#endif  /* __TIMER__ */
