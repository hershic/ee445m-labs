/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Eric Crosson 2015-03-07 */
/* Revision history: Look in Git FGT */
#include "schedule.h"

#include "libstd/nexus.h"
#include "libut/utlist.h"
#include "libos/os.h"

void schedule(task_t task, frequency_t frequency, DEADLINE_TYPE seriousness) {

    sched_task *ready_task = NULL;
    sched_task_pool *ready_queue = NULL;

    /* Grab a new task from the unused task pile */
    ready_task = SCHEDULER_UNUSED_TASKS;
    CDL_DELETE(SCHEDULER_UNUSED_TASKS, ready_task);

    /* Set new task's metadata */
    ready_task->task = task;
    ready_task->seriousness = seriousness;

    if (frequency > MAX_SYSTICKS_PER_HZ) {
        postpone_death();
    }
    ready_task->absolute_deadline = frequency * SYSTICKS_PER_HZ;

    ready_task->tcb = os_add_thread(task);

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
        CDL_DELETE(SCHEDULER_UNUSED_QUEUES, ready_queue);

        ready_queue->deadline = frequency;
        CDL_PREPEND(SCHEDULER_QUEUES, ready_queue);
        /* HASH_ADD_INT(SCHEDULER_QUEUES, deadline, ready_queue); */
    }

    /* Add task to ready queue */
    CDL_APPEND(ready_queue->queue, ready_task);
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

    int32_t i;
    for(i=0; i<SCHEDULER_MAX_THREADS; ++i) {
        /* Add all tasks to the unused pile */
        SCHEDULER_TASKS[i].id = i;
        DL_PREPEND(SCHEDULER_UNUSED_TASKS, &SCHEDULER_TASKS[i]);
        /* Add all task queues to the unused pile */
        SCHEDULER_TASK_QUEUES[i].id = i;
        DL_PREPEND(SCHEDULER_UNUSED_QUEUES, &SCHEDULER_TASK_QUEUES[i]);
    }
}

sched_task_pool* schedule_hash_find_int(sched_task_pool* queues, frequency_t target_frequency) {

    sched_task_pool* start = queues;
    sched_task_pool* inspect = queues;

    if (!inspect) { return NULL; }

    do {
        if(inspect->deadline == target_frequency) {
            return inspect;
        }
        inspect = inspect->next;
    } while (inspect != start);
}

/* Create the EDF queue for the first time */
/*! \pre ensure at least one task has been scheduled prior to invoking
 *  this function */
void edf_init() {

    sched_task_pool *start = SCHEDULER_QUEUES;
    sched_task_pool *next = start->next;

    /* avoid the NULL EDF_QUEUE to allow optimized form of `edf_insert' */
    LL_EDF_APPEND(EDF_QUEUE, start->queue);

    /* Create the rest of the EDF */
    while(next && next != start) {
        edf_insert(next->queue);
        next = next->next;
    }
}

/*! \pre ensure that that least one task has been added to \EDF_QUEUE
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

    volatile sched_task *executing = EDF_QUEUE;
    volatile sched_task_pool *pool = SCHEDULER_QUEUES;

    /* LL_EDF_DELETE(EDF_QUEUE, elt); */
    /* TODO: CHECKME: should we use pri_next or pri_prev?? */
    EDF_QUEUE = EDF_QUEUE->pri_next;

    /* KLUDGE: fix this for production */
    while (pool->queue != executing) {
        pool = pool->next;
    }
    /* will drop out with pool->queue = executing */

    executing->absolute_deadline = pool->deadline * SYSTICKS_PER_HZ;

    /* do the recycling, change the pool's head */
    /* TODO: CHECKME: should we use next or prev?? */
    pool->queue = executing->next;
    edf_insert(pool->queue);

    /* TODO: after use, put the command back in the appropriate pool */
    return executing->tcb;
}

sched_task* edf_get_edf_queue() {
    return EDF_QUEUE;
}
