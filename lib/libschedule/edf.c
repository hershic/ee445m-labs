/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-03-07 */
/* Revision history: Look in Git FGT */
#include "libos/os.h"
#include "schedule.h"
#include "libut/utlist.h"

/* Create the EDF queue for the first time */
/*! pre ensure at least one task has been scheduled prior to invoking
 *  this function */
void edf_init() {

    sched_task_pool *start = SCHEDULER_QUEUES;
    sched_task_pool *next = start->next;

    /* avoid the NULL pool to allow optimized form of `edf_insert' */
    LL_EDF_APPEND(EDF_QUEUE, start->queue);

    /* Create the rest of the EDF */
    while(next != start) {
        edf_insert(next->queue);
        next = next->next;
    }
}

/*! pre ensure that that least one task has been added to \EDF_QUEUE
 *  prior to invoking this function */
void edf_insert(sched_task* task) {

    sched_task *elt = EDF_QUEUE;
    while(elt && task->absolute_deadline > elt->absolute_deadline) {
        elt = elt->pri_next;
    }
    /* inject task at point -- if elt is null, this is the new end*/
    if (elt) {
        LL_EDF_PREPEND(EDF_QUEUE, elt);
    } else {
        /* TODO: this incurs the O(n) again, optimize */
        LL_EDF_APPEND(EDF_QUEUE, task);
    }
}

tcb_t* edf_pop() {

    sched_task *elt = EDF_QUEUE;
    LL_EDF_DELETE(EDF_QUEUE, elt);

    /* todo: after use, put the command back in the appropriate pool */
    return elt->tcb;
}

/* todo: rewrite both these functions in the appropriate style of uthash */
