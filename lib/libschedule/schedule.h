/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-03-07 */
/* Revision history: Look in Git FGT */
#ifndef __SCHEDULE__
#define __SCHEDULE__

#include "libnotify/notify.h"

/*! \addtogroup
 * @{
 */

#define SCHEDULE_PRIORITY
#define SCHEDULER_DEFAULT_MAX_THREADS    16

#if defined(SCHEDULE_PRIORITY)
/* We are using the priority scheduler */

#include "libhw/hardware.h"
#include "libut/uthash.h"

#if !(defined(SCHEDULER_MAX_THREADS))
#define SCHEDULER_MAX_THREADS   SCHEDULER_DEFAULT_MAX_THREADS
#endif

typedef void (*task_t)();	/* a task capable of being run */
typedef void (*isr_t)();	/* isr capable of hw_notifying */
typedef void (*pisr_t)(notification note);	/* a task capable of being run */
typedef int32_t frequency_t;
typedef int32_t microseconds_t; 	/* smallest divisible task time */

typedef enum {
    DL_HARD,
    DL_SOFT,
} DEADLINE_TYPE;

typedef struct sched_task {
    task_t task;
    DEADLINE_TYPE seriousness;

    /* For use by utlist library */
    struct sched_task *next;
    struct sched_task *prev;
} sched_task;

typedef struct {
    frequency_t deadline;   	/* Key */
    sched_task* queue;

    /* ALlow this structure to be hashable */
    UT_hash_handle hh;
    /* Allow this structure to be linked-list-able */
    struct sched_task_pool* next;
    struct sched_task_pool* prev;
} sched_task_pool;

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
