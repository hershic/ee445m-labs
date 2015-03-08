/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-03-07 */
/* Revision history: Look in Git FGT */
#include "edf.h"
#include "schedule.h"

/* Create the EDF queue for the first time */
/*! pre ensure at least one task has been scheduled prior to invoking
 *  this function */
void edf_init() {

    sched_task_pool *start = SCHEDULER_QUEUES;
    sched_task_pool *next = start->next;

    /* avoid the NULL pool to allow optimized form of `edf_insert' */
    LL_APPEND(EDF_QUEUE, SCHEDULER_QUEUES);

    /* Create the rest of the EDF */
    while(next != start) {
	edf_insert(next->queue);
	next = next->next;
    }
}

void edf_insert(sched_task* task) {

    sched_task *elt = EDF_QUEUE;
    while(elt && task->absolute_deadline > elt->absolute_deadline) {
	elt = elt->pri_next;
    }
    /* inject task at point -- if elt is null, this is the new end*/
    if (elt) {
	LL_PREPEND(EDF_QUEUE, elt, task);
    } else {
	/* TODO: this incurs the O(n) again, optimize */
	LL_APPEND(EDF_QUEUE, task);
    }
}

tcb_t* edf_pop() {


}

/* todo: rewrite both these functions in the appropriate style of uthash */
