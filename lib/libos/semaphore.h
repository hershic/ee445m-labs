/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-02-28 */
/* Revision history: Look in Git FGT */
#ifndef __SEMAPHORE__
#define __SEMAPHORE__

/*! \addtogroup OS
 * @{
 */

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
    (sem < 0)

/*! Conditional evaluating to true when a thread's tcb_t is blocked by
 * a semaphore. */
#define thread_blocked(tcb_t)                   \
    (tcb_t->sem != NULL)

#endif

/* End Doxygen group
 * @}
 */
