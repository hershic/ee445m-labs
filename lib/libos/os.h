/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave 2015-02-08 */
/* Revision History: Look in Git FGT */

#ifndef __OS__
#define __OS__

#include <stdint.h>
#include <stdbool.h>

#include "inc/hw_ints.h"

#include "libstd/nexus.h"
#include "semaphore.h"

/*! \addtogroup OS
 * @{
 */

/* TODO: determine the best ways to document this and choose a default
 * scheduler -- something like http://bit.ly/1zQSuqw */

/* Possible values are:
 * SCHEDULER_ROUND_ROBIN
 * SCHEDULER_MULTILEVEL_QUEUE
 * SCHEDULER_MULTILEVEL_FEEDBACK_QUEUE
 */

#if !(defined SCHEDULER_ROUND_ROBIN && \
      defined SCHEDULER_MULTILEVEL_QUEUE && \
      defined SCHEDULER_MULTILEVEL_FEEDBACK_QUEUE)
#define SCHEDULER_MULTILEVEL_QUEUE
#endif

/*! Maximum number of concurrent threads */
#define OS_MAX_THREADS  4

/*! Maximum number of 32-bit values allowed in each thread's stack */
#define OS_STACK_SIZE   100

/*! Static number of thread pools of distinct priority in use */
#define OS_NUM_POOLS    2

/*! Highest priority thread pool alias */
#define OS_SYSTEM_POOL      0
/*! Highest priority thread pool alias */
#define OS_REAL_TIME_POOL   0
/*! Second highest priority thread pool alias */
#define OS_INTERACTIVE_POOL  1

/*! An alias to \os_add_thread. Feels like home sweet home. */
#define os_spawn_thread(_thread, _priority)     \
    os_add_thread(_thread, priority)

/*! An alias to \os_remove_thread. Feels like home sweet home.*/
#define os_kill_thread(_thread)                 \
    os_remove_thread(_thread)

typedef enum {
    /* thread is not running and will not run */
    THREAD_DEAD,

    /* thread is waiting and will run when the semaphore blocking it
     * is released */
    THREAD_SLEEPING,

    /* thread is running */
    THREAD_RUNNING,

    /* thread is not running but will run eventually */
    THREAD_ACTIVE
} tstate_t;

/*! Iterator capable of representing all pools. */
typedef uint8_t pool_t;

/*! OS thread priority levels; 0 is highest. */
typedef uint8_t priority_t;

/*! Type declaration of a task. \warning This is not a thread. We need
 *  real threads. */
typedef void (*task_t)();

/*! Thread Control Block definition */
typedef struct tcb {

    /*! pointer to stack (valid for threads not running */
    int32_t *sp;

    /*! linked-list pointer to next tcb */
    struct tcb *next;
    /*! linked-list pointer to prev tcb */
    struct tcb *prev;

    /*! Unique numeric identifier for the tcb. */
    immutable int32_t id;

    priority_t priority;

    /*! The function used as this thread's entry point. This is
     *  recorded for developer convenience, i.e. the developer may get
     *  a handle to a tcb from his task pointer. */
    task_t entry_point;

    /*! State of this thread */
    tstate_t status;

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

/*! A circular doubly linked list of currently running threads.
 * \note The head of this list is 'os_current_running_thread'
 */
static tcb_t* os_running_threads = NULL;

/*! A circular doubly linked list of currently dead threads. */
static tcb_t* os_dead_threads = NULL;

/*! The contents of the stack immediately after a function call. */
typedef struct hwcontext {
    uint32_t r0;
    uint32_t r1;
    uint32_t r2;
    uint32_t r3;
    uint32_t r12;
    uint32_t lr;
    uint32_t pc;
    uint32_t psr;
} hwcontext_t;

/*! The contents of the stack during a context switch. */
typedef struct swcontext {
    uint32_t r4;
    uint32_t r5;
    uint32_t r6;
    uint32_t r7;
    uint32_t r8;
    uint32_t r9;
    uint32_t r10;
    uint32_t r11;
    uint32_t lr;
} swcontext_t;

/*! Communication mailbox from the SysTick_Handler to the
 *  PendSV_Handler; will contain the tcb_t* of the next thread to
 *  execute come PendSV_Handler execution time. */
static tcb_t* OS_NEXT_THREAD;

/*! An array of statically allocated threads. */
static tcb_t OS_THREADS[OS_MAX_THREADS];

/*! An array of thread pools to place OS_THREADS into. */
static tcb_t* OS_THREAD_POOL[OS_NUM_POOLS];

/*! Initialize the threading engine, setting all threads to dead. This
 *  initializes the dead thread circle appropriately and sets the
 *  running thread circle to null.
 */
void os_threading_init();

/*! Resets the thread stack for a given tcb to run a given task.
 *  \param thread the thread whose stack is to be reset
 *  \param the task for which the thread should run
 */
void _os_reset_thread_stack(tcb_t* tcb, task_t task);

/*! Adds a new thread with the specified task.
 *  \returns the TCB of the newly added thread, null if the addition
 *  was not possible for some reason.
 */
tcb_t* os_add_thread(task_t, priority_t priority);

/*! Remove a thread from the queue the schedule will choose from.
 * \param The task to kill. This should be the task that was used to start the thread to remove via \os_add_thread.
 * \returns The newly released tcb
 */
tcb_t* os_remove_thread(task_t);

/*! Return the tcb used to control the specified task_t.
 * \param The task to lookup the tcb of
 * \returns The tcb of the specified task_t
 */
tcb_t* os_tcb_of(task_t);

/*! Returns the next dead thread in the dead thread circle.
 *  If the length of the dead thread circle is greater than 1, then
 *  the current dead thread is removed and the circle is relinked with
 *  the next dead thread. If the circle contains only one dead thread,
 *  then that thread is removed and the current dead thread is set to
 *  null. If there is no thread in the dead thread circle, then it
 *  retuns null.
 *  \returns The first thread removed from the dead thread circle, or
 *  null if the dead thread circle is empty.
 */
tcb_t* _os_next_dead_thread();

/* TODO: doxygenize */
int32_t os_running_thread_id();

/*! Launches the operating system.
 *  \note This function drastically changes the execution paradigm of
 *  the machine; do not expect to call it and be able to return to
 *  the previous context.
 */
void os_launch();

/*! A convenience alias to \os_suspend to circumnavigate the naming
 *  conventions chosen by the couse Administrators. */
#define os_surrender_context()                  \
    os_suspend()

/*! Put the invoking thread to sleep and let another thread take
 *  over. This s another way to say "set the interrupt bit of the
 *  \PendSV_Handler". */
always static inline os_suspend() {

    IntPendSet(FAULT_PENDSV);
    /* TODO: penalize long threads, reward quick threads */
}

/*! Choose the next thread in the specified pool to execute based on a
 *  round robin scheme. */
always static inline
tcb_t* _os_scheduler_round_robin(pool_t pool, tcb_t* first_live_thread) {

    while(os_running_threads && first_live_thread != os_running_threads) {
        first_live_thread = first_live_thread->next;
    }
    return first_live_thread->next;
}

/*! Returns the tcb of the first running thread in the specified pool,
 *  or NULL if the specified pool consists of only sleeping
 *  (deadlocked) threads */
always static inline
tcb_t* _os_pool_waiting(pool_t pool) {

    /* optimize: this will be called unbelievably often */
    tcb_t* tcb = OS_THREAD_POOL[pool];
    if (!tcb) {return NULL;}
    do {
        if (THREAD_RUNNING == tcb->status) {
            return tcb;
        }
        tcb = tcb->next;
    } while (tcb != OS_THREAD_POOL[pool]);
    return NULL;
}

/* TODO: what happens when we run out of threads to use? aka, when
 * they are all blocked. I bet it's time again to whip out the idle
 * thread. */
static inline
void _os_choose_next_thread() {
    pool_t pool = 0;
    tcb_t* next_thread = _os_pool_waiting(pool);

    while(!next_thread) {
        next_thread = _os_pool_waiting(++pool);
    }
    /* TODO: use the Multilevel Feedback-Queue Scheduling here */
    /* For now, all threads are of the same priority so simply
     * implement a round robin scheme. */
    next_thread = _os_scheduler_round_robin(pool, next_thread);

    OS_NEXT_THREAD = next_thread;
}


#endif

/*! End doxygen group
 * @}
 */
