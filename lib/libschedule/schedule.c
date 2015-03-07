/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Eric Crosson 2015-03-07 */
/* Revision history: Look in Git FGT */
#include "schedule.h"

#include "libstd/nexus.h"
#include "libut/utlist.h"

#if defined(SCHEDULE_PRIORITY)
/* We are using the priority scheduler */

/*! Statically allocated multiple queues of tasks */
static sched_task_pool SCHEDULER_TASK_QUEUES[SCHEDULER_MAX_THREADS];

/*! Doubly linked list of unused task queues */
static sched_task_pool* SCHEDULER_UNUSED_QUEUES = NULL;

/*! UTHash of live task queues */
/* for now it's a utlist though, don't get confused */
static sched_task_pool* SCHEDULER_QUEUES = NULL;

/*! Statically allocated task metadata structures for the scheduler to
 *  manage */
static sched_task SCHEDULER_TASKS[SCHEDULER_MAX_THREADS][SCHEDULER_MAX_THREADS];

/*! Doubly linked list of unused tasks */
static sched_task* SCHEDULER_UNUSED_TASKS = NULL;


/* private functions */
sched_task_pool* schedule_hash_find_int(sched_task_pool* queues, frequency_t target_frequency);
void schedule_hash_add_int(sched_task_pool* queues, sched_task_pool* add);

void schedule(task_t task, frequency_t frequency, DEADLINE_TYPE seriousness) {

    sched_task *ready_task = NULL;
    sched_task_pool *ready_queue = NULL;

    /* Grab a new task from the unused task pile */
    ready_task = SCHEDULER_UNUSED_TASKS;
    DL_DELETE(SCHEDULER_UNUSED_TASKS, ready_task);

    /* Set new task's metadata */
    ready_task->task = task;
    ready_task->seriousness = seriousness;

    /* Test the pool of ready queues for a queue of tasks with this
     * frequency */

    /* todo: uthash configurable without malloc */
    ready_queue = schedule_hash_find_int(SCHEDULER_QUEUES, frequency);
    /* HASH_FIND_INT(SCHEDULER_QUEUES, &frequency, ready_queue); */

    /* No similar tasks exist yet -- create the pool */
    if (!ready_queue) {
	/* Grab a new queue, remove it from the unused pile,
	 * initialize it and associate it with this requency of
	 * task */
	ready_queue = SCHEDULER_UNUSED_QUEUES;
	DL_DELETE(SCHEDULER_UNUSED_QUEUES, ready_queue);

	ready_queue->deadline = frequency;
	schedule_hash_add_int(SCHEDULER_QUEUES, ready_queue);
	/* HASH_ADD_INT(SCHEDULER_QUEUES, deadline, ready_queue); */
    }

    /* Add task to ready queue */
    DL_APPEND(ready_queue->queue, ready_task);
}

void schedule_aperiodic(pisr_t pisr,
			HW_TYPE hw_type,
			hw_metadata metadata,
			microseconds_t allowed_run_time,
			DEADLINE_TYPE seriousness) {

    /* todo: utilize \allowed_run_time, \seriousness */
    _hw_subscribe(hw_type, metadata, pisr, true);
}

void schedule_init() {

    int32_t i, j;

    /* Add all tasks to the unused pile */
    for(i=0; i<SCHEDULER_MAX_THREADS; ++i) {
	for(j=0; j<SCHEDULER_MAX_THREADS; ++j) {
	    DL_APPEND(SCHEDULER_UNUSED_TASKS, &SCHEDULER_TASKS[i][j]);
	}
    }
    /* Add all task queues to the unused pile */
    for(i=0; i<SCHEDULER_MAX_THREADS; ++i) {
	DL_APPEND(SCHEDULER_UNUSED_QUEUES, &SCHEDULER_TASK_QUEUES[i]);
    }
}

sched_task_pool* schedule_hash_find_int(sched_task_pool* queues, frequency_t target_frequency) {

    int32_t i = 0;
    sched_task_pool* start = queues;
    sched_task_pool* inspect = queues;
    while(i == 0 || inspect != start) {
	if(inspect->deadline == target_frequency) {
	    return inspect;
	}
    }
}

void schedule_hash_add_int(sched_task_pool* queues, sched_task_pool* add) {

    DL_APPEND(queues, add);
}

#else
/* We are using the non-priority scheduler */

#endif
