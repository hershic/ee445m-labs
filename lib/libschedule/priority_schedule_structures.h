/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Eric Crosson 2015-03-07 */
/* Revision history: Look in Git FGT */
#ifndef __PRIORITY_SCHEDULE_STRUCTURES__
#define __PRIORITY_SCHEDULE_STRUCTURES__

#include "libut/uthash.h"

/*! \addtogroup
 * @{
 */

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

typedef struct sched_task_pool {
    frequency_t deadline;   	/* Key */
    sched_task* queue;

    /* ALlow this structure to be hashable */
    /* UT_hash_handle hh; */
    /* Allow this structure to be linked-list-able */
    struct sched_task_pool *next, *prev;
} sched_task_pool;


#endif

/* End Doxygen group
 * @}
 */
