/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-03-08 */
/* Revision history: Look in Git FGT */
#ifndef __thread_structrues__
#define __thread_structrues__

#include "libstd/defines.h"

/*! \addtogroup
   * @{
    */

/*! OS thread priority levels; 0 is highest. */
typedef uint8_t priority_t;

typedef int32_t deadline_t;
typedef int32_t microseconds_t;         /* smallest divisible task time */

typedef enum {
    DL_HARD,
    DL_SOFT,
} DEADLINE_TYPE;


/*! Thread Control Block definition */
typedef struct tcb_t {

    /*! pointer to stack (valid for threads not running */
    int32_t *sp;

    /*! linked-list pointer to next tcb */
    struct tcb_t *next;
    /*! linked-list pointer to prev tcb */
    struct tcb_t *prev;

    /*! Unique numeric identifier for the tcb. */
    immutable int32_t id;

    priority_t priority;

    /*! The function used as this thread's entry point. This is
     *  recorded for developer convenience, i.e. the developer may get
     *  a handle to a tcb from his task pointer. */
    task_t entry_point;

    /* Semaphore value of this thread.
     * sem == NULL :: unblocked
     * sem contains a pointer's address :: blocked
     */
    semaphore_t* sem;

    /*! sleep timer of the thread */
    /* int32_t sleep_timer;

    /*! priority of the thread */
    /* int8_t priority; */
} tcb_t;

typedef struct sched_task {
    task_t task;
    tcb_t* tcb;
    DEADLINE_TYPE seriousness;
    deadline_t absolute_deadline;

    void* pool;

    /* For use by utlist library */
    struct sched_task *next;
    struct sched_task *prev;
    struct sched_task *pri_next;
    struct sched_task *pri_prev;
    int id;
} sched_task;

typedef struct sched_task_pool {
    frequency_t deadline;       /* Key */
    sched_task *queue;

    /* allow this structure to be hashable */
    /* UT_hash_handle hh; */
    /* Allow this structure to be linked-list-able */
    struct sched_task_pool *next, *prev;
    int id;
} sched_task_pool;

#endif

/* End Doxygen group
    * @}
     */
