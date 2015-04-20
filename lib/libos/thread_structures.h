/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-03-08 */
/* Revision history: Look in Git FGT */
#ifndef __thread_structrues__
#define __thread_structrues__

/* #include "libstd/defines.h" */
#include <stdint.h>

/*! \addtogroup
   * @{
    */

typedef void (*task_t)();

typedef void (*isr_t)();        /* isr capable of hw_notifying */
typedef uint32_t frequency_t;

/*! Count of SysTick ticks (each of which is equivalent to a bus
 *  cycle). Note that the SysTick can only count down from a 24-bit
 *  register. */
typedef uint32_t tick_t;

/*! The default initialization value of a semaphore_t. */
#define SEMAPHORE_DEFAULT_VALUE                 0

/*! Typedef of a semaphore. */
typedef int8_t semaphore_t;

/*! Spinlock until \blocker is non-zero. */
#define spinlock_until(blocker)                 \
    while (!(*blocker))

/*! Initialize \sem to \initial_value. */
#define sem_init_(sem, initial_value)           \
    semaphore_t sem;                            \
    sem = initial_value

/*! Guard the following code block to evaluate only in the semaphore
 *  may be obtained. */
#define sem_guard(sem)                          \
    if(!semaphore_blocked(sem))

/*! \sem_wait without the br, to be used when you know you can take
 *  the semaphore. */
#define sem_take(sem)                           \
    atomic (                                    \
        --sem;                                  \
       /* TODO: wake thread */                  \
    )

/* TODO: define */
#define sem_check(sem)

/*! Initialize a semaphore_t to \SEMAPHORE_DEFAULT_VALUE. */
#define sem_init(sem)                           \
    sem_init_(sem, SEMAPHORE_DEFAULT_VALUE)

/*! A convenience alias to \sem_post to allow comparison of source
 *  code to course documents. */
#define sem_signal(sem)                         \
    sem_post(sem)

/*! Signal \sem and update all threads watching \sem. If the highest
 *  priority thread watching \sem is released by this post, and it is
 *  of a higher priority than the currently executing thread, then
 *  suspend the current thread. The thread-context switcher will then
 *  choose the higher-priority thread to execute. */
#define sem_post(sem)                           \
    atomic (                                    \
        ++sem;                                  \
       /* TODO: wake thread */                  \
    )

/*! Stall execution of current thread until \sem is nonblocking. If
 *  \sem becomes blocking during this invocation, call
 *  \os_surrender_context for the current executing thread. */
#define sem_wait(sem)                           \
    while (semaphore_blocked(sem)) {            \
        os_surrender_context();                 \
    }                                           \
    atomic (                                    \
        --sem;                                  \
    )

/*! Conditional evaluating to true when a semaphore is blocked by
 *  virtue of being non-NULL. */
#define semaphore_blocked(sem)                  \
    (sem <= 0)

/*! Conditional evaluating to true when a thread's tcb_t is blocked by
 * a semaphore. */
#define thread_blocked(tcb_t)                   \
    (tcb_t->sem != NULL)


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
    int32_t id;

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
    tick_t absolute_deadline;

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
