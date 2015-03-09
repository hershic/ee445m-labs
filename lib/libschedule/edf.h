/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Eric Crosson 2015-03-07 */
/* Revision history: Look in Git FGT */
#ifndef __EDF__
#define __EDF__

/*! \addtogroup
 * @{
 */

#include "libos/os.h"
#include "libos/thread_structures.h"
#include "priority_schedule_structures.h"

/* todo: this is a duplicate, remove it and fix */
#define SCHEDULER_MAX_THREADS    16

/*! Statically allocated array of periodic tasks arranged by
 *  increasing time-to-deadline. */
static sched_task EDF[SCHEDULER_MAX_THREADS];

/*! Linked list of tasks (with different periods) ready to be run. */
static sched_task *EDF_QUEUE = NULL;

/*! Initialize the Earliest Deadline First task queue */
void edf_init();

/*! Add by insertion sort the specified task into the EDF queue */
void edf_insert(sched_task*);

tcb_t* edf_pop();

#endif

/* End Doxygen group
 * @}
 */
