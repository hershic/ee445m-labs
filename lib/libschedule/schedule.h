/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-03-07 */
/* Revision history: Look in Git FGT */
#ifndef __SCHEDULE__
#define __SCHEDULE__

#include "libnotify/notify.h"

/*! \addtogroup
 * @{
 */

/* fixme: remove and do elsewhere */
#define SCHEDULE_PRIORITY
#define SCHEDULER_DEFAULT_MAX_THREADS    16

#ifdef SCHEDULE_PRIORITY
/* We are using the priority scheduler */

/* Recognized priority schedulers */
#include "priority_schedule_structures.h"
#include "edf.h"

#include "libhw/hardware.h"
#include "libut/uthash.h"

#if !(defined(SCHEDULER_MAX_THREADS))
#define SCHEDULER_MAX_THREADS   SCHEDULER_DEFAULT_MAX_THREADS
#endif

/*! Initialize all deep datastructures used by libschedule. */
void schedule_init();

void schedule(task_t, frequency_t, DEADLINE_TYPE);

/*! Schedule a pseudo-isr to be executed when a hardware event
 *  described by HW_TYPE and hw_metadata occurs. */
void schedule_aperiodic(pisr_t, HW_TYPE, hw_metadata, microseconds_t, DEADLINE_TYPE);

#else
/* We are using the non-priority scheduler */

#endif

#endif	/* __SCHEDULE__ */

/* End Doxygen group
 * @}
 */
