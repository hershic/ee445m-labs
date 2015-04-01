/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave 2015-02-08 */
/* Revision History: Look in Git FGT */

#include "os.h"

#include "inc/hw_nvic.h"
#include "inc/hw_types.h"

#include "libstd/nexus.h"
#include "libut/utlist.h"
#include "libsystick/systick.h"
#include "libschedule/schedule.h"

/*! A block of memory for each thread's local stack. */
static int32_t OS_PROGRAM_STACKS[SCHEDULER_MAX_THREADS][OS_STACK_SIZE];
volatile sched_task *executing;
volatile sched_task_pool *pool;

volatile uint32_t clock = 0;

/*! Statically allocated array of periodic tasks arranged by
 *  increasing time-to-deadline. */
volatile sched_task EDF[SCHEDULER_MAX_THREADS];

/*! Linked list of tasks (with different periods) ready to be run. */
volatile sched_task* EDF_QUEUE = NULL;

bool OS_FIRST_RUN = true;

bool OS_THREADING_INITIALIZED;
uint8_t OS_NUM_THREADS;

void os_threading_init() {

    uint32_t i;

    /* This check exists so libraries may call os_threading_init
     * without breaking everything. We dont' want to contribute to the
     * black magic that is the specific initialization of TI
     * libraries, do we? */
    if (OS_THREADING_INITIALIZED) {
        return;
    }

    os_running_threads = NULL;

    /* Initialize thread metadata */
    for (i=0; i<SCHEDULER_MAX_THREADS; ++i) {
        CDL_APPEND(os_dead_threads, &OS_THREADS[i]);
        OS_THREADS[i].sp = OS_PROGRAM_STACKS[i];
        OS_THREADS[i].id = i;
        OS_THREADS[i].entry_point = NULL;
        /* OS_THREADS[i].status = THREAD_DEAD; */
        /* OS_THREADS[i].sleep_timer = 0; */
    }

    /* Initialize thread pool metadata */
    for (i=0; i<OS_NUM_POOLS; ++i) {
        OS_THREAD_POOL[i] = (tcb_t*) NULL;
    }

    OS_THREADING_INITIALIZED = true;

    schedule_init();
}

int8_t get_os_num_threads() {
    return OS_NUM_THREADS;
}

tcb_t* os_add_thread(task_t task) {

    int32_t status;
    tcb_t* thread_to_add;

    /* 1. Disable interrupts and save the priority mask */
    atomic_start();

    /* 2. Pop the task from the dead_thread pile */
    thread_to_add = os_dead_threads;
    CDL_DELETE(os_dead_threads, thread_to_add);

    /* 3. Add the thread to the appropriate priority pool. */
    /* CDL_APPEND(os_running_threads, thread_to_add); */
    /* CDL_APPEND(OS_THREAD_POOL[priority], thread_to_add); */

    /* 4. Set the initial stack contents for the new thread. */
    _os_reset_thread_stack(thread_to_add, task);

    /* 5. Set metadata for this thread's TCB. */

    /* TODO: Remove this from semaphore. The scheduler manages
     * this. */
    /* thread_to_add->status = THREAD_RUNNING; */
    thread_to_add->entry_point = task;
    /* thread_to_add->priority = priority; */
    /* thread_to_add->sleep_timer = 0; */

    atomic_end();

    /* 5. Return. */
    return thread_to_add;
}

tcb_t* os_remove_thread(task_t task) {

    int32_t status;
    tcb_t* thread_to_remove;

    /* 1. Disable interrupts and save the priority mask */
    /* TODO: clean this up, make uniform */
    status = StartCritical();

    /* 2. Pop the task from the running_thread pile and add it to the
     * list of dead threads. */
    thread_to_remove = os_tcb_of(task);
    CDL_DELETE(OS_THREAD_POOL[thread_to_remove->priority], thread_to_remove);
    /* Append means low tcb_t memory reusability. Prepend means high */
    CDL_APPEND(os_dead_threads, thread_to_remove);

    /* OPTIONAL TODO: do the check for overwritten stacks as long as
     * we kill every thread in our testing we'll get valuable yet
     * unobtrusive debugging data. also consider a check on the
     * immutable fields, using the offset from the array base as
     * verification. */

    /* 3. Reset metadata for this thread's TCB. */
    /* thread_to_remove->status = THREAD_DEAD; */
    thread_to_remove->entry_point = NULL;
    thread_to_remove->next = NULL;
    thread_to_remove->prev = NULL;

    /* 4. Return. */
    /* TODO: clean this up, make uniform */
    --OS_NUM_THREADS;

    EndCritical(status);
    return thread_to_remove;
}

int32_t os_running_thread_id() {

    return os_running_threads->id;
}

tcb_t* os_tcb_of(const task_t task) {

    int32_t i;
    for(i=0; i<SCHEDULER_MAX_THREADS; ++i) {
        if (task == OS_THREADS[i].entry_point) {
            return &OS_THREADS[i];
        }
    }
#ifndef DANGER_ZONE
    postpone_death();
#endif
    return NULL;
}

/*! \note This enables interrupts */
void os_launch() {

    edf_init();
    _os_choose_next_thread();
    os_running_threads = OS_NEXT_THREAD;

    /* acquire the pointer to the stack pointer here */
    asm volatile("LDR     R0, =OS_NEXT_THREAD");

    /* acquire the current value of the stack pointer */
    asm volatile("LDR R0, [R0]");
    asm volatile("LDR R0, [R0]");

    /* set the process stack value */
    asm volatile("MSR MSP, R0");

    /* change EXC_RETURN for return on MSP */
    /* asm volatile("ORR LR, LR, #4"); */

    /* change the active stack to use msp */
    /* asm volatile("MRS R0, CONTROL"); */
    /* asm volatile("ORR R0, R0, #2"); */
    /* asm volatile("MSR CONTROL, R0"); */

    asm volatile("POP     {R4-R11}");

    asm volatile("POP     {LR}");
    asm volatile("POP     {R0-R3}");

    asm volatile("pop {r12, lr}");
    asm volatile("CPSIE  I");
    asm volatile("pop {pc}");

    asm volatile ("BX LR");
}

always inline
void _os_reset_thread_stack(tcb_t* tcb, task_t task) {

    hwcontext_t* hwcontext = (hwcontext_t*)
        (((uint32_t)OS_PROGRAM_STACKS[tcb->id]) +
         sizeof(uint32_t)*OS_STACK_SIZE - sizeof(hwcontext_t));

    swcontext_t* swcontext = (swcontext_t*)
        (((uint32_t)hwcontext) - sizeof(swcontext_t));

    hwcontext->pc = (uint32_t)(task);
    hwcontext->psr = 0x01000000;

    swcontext->lr = 0xfffffff9;

    #ifdef OS_REGISTER_DEBUGGING_ENABLED
    hwcontext->r0 = 0x00000000;
    hwcontext->r1 = 0x01010101;
    hwcontext->r2 = 0x02020202;
    hwcontext->r3 = 0x03030303;

    swcontext->r4 = 0x04040404;
    swcontext->r5 = 0x05050505;
    swcontext->r6 = 0x06060606;
    swcontext->r7 = 0x07070707;
    swcontext->r8 = 0x08080808;
    swcontext->r9 = 0x09090909;
    swcontext->r10 = 0x10101010;
    swcontext->r11 = 0x11111111;
    #endif  /* OS_REGISTER_DEBUGGING_ENABLED */

    tcb->sp = (uint32_t*)(((uint32_t)hwcontext) - sizeof(swcontext_t));
    asm volatile ("PUSH {R9, R10, R11, R12}");
    asm volatile ( "mrs     r12, msp" );
    asm volatile ( "mrs     r11, msp" );
    asm volatile ( "mrs     r10, control" );

    asm volatile ("POP {R9, R10, R11, R12}");
}

always inline
void scheduler_reschedule(void) {
    executing = EDF_QUEUE;
    pool = SCHEDULER_QUEUES;

    /* find the queue that the executing task is a member of */
    /* TODO: Should we link from the sched_task to the
       sched_task_pool? That will avoid this loop. */
    while (pool->queue != executing) {
        pool = pool->next;
    }

    /* update  */
    if (EDF_QUEUE) {
        EDF_QUEUE = EDF_QUEUE->pri_next;
    }

    if (OS_FIRST_RUN) {
        OS_FIRST_RUN = false;
        SysTickEnable();
        SysTickIntEnable();
    } else {
        clock += pool->deadline;
    }

    SysTickDisable();
    /* SysTickPeriodSet(SysCtlClockGet() / (executing->absolute_deadline - clock)); */
    SysTickPeriodSet(SYSCTLCLOCK / (pool->deadline));
    HWREG(NVIC_ST_CURRENT) = 0;
    executing->absolute_deadline = pool->deadline + clock;
    SysTickEnable();

    /* do the recycling, change the pool's head */
    pool->queue = executing->next;
    edf_insert(pool->queue);

    OS_NEXT_THREAD = executing->tcb;

    /* Queue the PendSV_Handler after this ISR returns */
    IntPendSet(FAULT_PENDSV);
}


/*! \warning Ensure you have something to run before enabling
 *  SysTick */
/* this is the canonical code for edf scheduling */
void SysTick_Handler() {

    asm volatile("CPSID  I");

    scheduler_reschedule();

    asm volatile("CPSIE  I");
}

void PendSV_Handler() {

    asm volatile("CPSID  I");

    /* -------------------------------------------------- */
    /* phase 1: store context                             */
    /* -------------------------------------------------- */

    /* load the msp of thread A into r12 */
    asm volatile("mrs     r12, msp" );

    /* save thread A's registers into the msp */
    asm volatile("stmdb   r12!, {r4 - r11, lr}");

    /* -------------------------------------------------- */
    /* phase 2: os_running_threads manipulation    */
    /* -------------------------------------------------- */

    /* set the profiling data structures for the current thread */
    /* should this be "enabled" or "disabled"? */
    #ifdef OS_TIME_PROFILING_ENABLED
    if (os_running_threads->time_started >= 0) {
        os_running_threads->time_running_last =
            HWREG(NVIC_ST_CURRENT) - os_running_threads->time_started;

        /* If we haven't reached the max samples yet, increment the
           number of samples taken */
        if (os_running_threads->time_running_samples_taken < OS_TIME_MAX_SAMPLES) {
            ++(os_running_threads->time_running_samples_taken);
        }

        HWREG(NVIC_ST_CURRENT) = 0;
        /* _os_reset_thread_stack(os_running_threads, os_running_threads->entry_point); */

        /* take another sample */
        os_running_threads->time_running_avg = os_running_threads->time_running_samples_taken > 1 ?
            /* if true */
            (os_running_threads->time_running_last +
             os_running_threads->time_running_samples_taken * os_running_threads->time_running_avg) /
            (os_running_threads->time_running_samples_taken+1) :
            /* else */
            os_running_threads->time_running_last;

    }
    #endif /* OS_TIME_PROFILING */

    /* load the value of os_running_threads */
    asm volatile("LDR     R2, =os_running_threads");

    /* r3 = *os_running_threads, of thread A */
    asm volatile("LDR     R3, [R2]");

    /* load the value of os_running_threads->next into r1 */
    asm volatile("LDR     R1, =OS_NEXT_THREAD");
    asm volatile("LDR     R1, [R1]");

    /* os_running_threads = OS_NEXT_THREAD */
    asm volatile("STR     R1, [R2]");

    /* -------------------------------------------------- */
    /* phase 3: load context                              */
    /* -------------------------------------------------- */

    /* store the msp from thread A */
    asm volatile("str     r12, [r3, #0]");

    /* load thread B's msp */
    asm volatile("ldr     r12, [r1]");

    /* set the profiling data structures for the next thread */
    #ifdef OS_TIME_PROFILING_ENABLED
        os_running_threads->time_started = HWREG(NVIC_ST_CURRENT);
    #endif  /* OS_TIME_PROFILING_ENABLED */

    /* load thread B's context */
    asm volatile("ldmia   r12!, {r4 - r11, lr}");

    /* put thread B's msp into the arch msp register */
    asm volatile("msr     msp, r12");

    /* reenable interrupts */
    asm volatile("CPSIE   I");

    asm volatile ("bx lr");
}

/*! Put the invoking thread to sleep and let another thread take
 *  over. This s another way to say "set the interrupt bit of the
 *  \PendSV_Handler". */
void os_suspend() {

    scheduler_reschedule();
}


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
    ready_task->absolute_deadline = frequency + clock;

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
        if (!SCHEDULER_QUEUES) {
            SCHEDULER_QUEUES = ready_queue;
            SCHEDULER_QUEUES->next = SCHEDULER_QUEUES;
            SCHEDULER_QUEUES->prev = SCHEDULER_QUEUES;
        } else {
            CDL_PREPEND(SCHEDULER_QUEUES, ready_queue);
        }
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
        DL_PREPEND(SCHEDULER_UNUSED_TASKS, &SCHEDULER_TASKS[i]);
        /* Add all task queues to the unused pile */
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
    return NULL;
}

/* Create the EDF queue for the first time */
/*! \pre ensure at least one task has been scheduled prior to invoking
 *  this function */
void edf_init() {

    sched_task_pool *start = SCHEDULER_QUEUES;
    sched_task_pool *next = start->next;

    /* avoid the NULL EDF_QUEUE to allow optimized form of `edf_insert' */
    DL_EDF_INSERT(EDF_QUEUE, start->queue);

    /* Create the rest of the EDF */
    while(next && next != start) {
        /* DL_EDF_INSERT(EDF_QUEUE, next->queue); */
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
        if (elt == EDF_QUEUE) {
            DL_EDF_PREPEND(elt, task);
            EDF_QUEUE = elt;
        } else {
            DL_EDF_PREPEND(elt, task);
        }
    } else {
        /* TODO: this incurs the O(n) again, optimize */
        DL_EDF_INSERT(EDF_QUEUE, task);
    }
}

tcb_t* edf_pop() {

    volatile sched_task *executing = EDF_QUEUE;
    volatile sched_task_pool *pool = SCHEDULER_QUEUES;

    /* DL_EDF_DELETE(EDF_QUEUE, elt); */
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

/* TODO: implement the edf queue of queues */
/*! \pre disable interrupts before we get here */
void _os_choose_next_thread() {

    scheduler_reschedule();

}
