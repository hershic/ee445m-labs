/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Eric Crosson 2015-03-07 */
/* Revision history: Look in Git FGT */
#include "schedule.h"

#include "libstd/nexus.h"

#if defined(SCHEDULE_PRIORITY)
/* We are using the priority scheduler */

sched_ready SCHEDULER_READY[SCHEDULER_MAX_THREADS];

void schedule(task_t task, frequency_t frequency, DEADLINE_TYPE seriousness) {

    sched_task ready;
    sched_ready* ready_queue;
    ready.task = task;
    ready.seriousness = seriousness;
    HASH_FIND_INT(SCHEDULER_READY, &frequency, ready_queue);
    /* TODO: do we need to initialize the queue? FIND should fail it not */
    DL_PREPEND(ready_queue->queue, ready);
}

void schedule_aperiodic(pisr_t pisr,
			HW_TYPE hw_type,
			hw_metadata metadata,
			microseconds_t allowed_run_time,
			DEADLINE_TYPE seriousness) {

    /* todo: use \allowed_run_time, \seriousness */
    _hw_subscribe(hw_type, metadata, pisr, true);
}

/* TODO: initialize all deep datastructures used by libschedule */
void schedule_init() {

}


#else
/* We are using the non-priority scheduler */

#endif
