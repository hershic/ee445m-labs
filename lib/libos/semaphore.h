/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-02-28 */
/* Revision history: Look in Git FGT */
#ifndef __semaphore__
#define __semaphore__

/*! \addtogroup OS
 * @{
 */

typedef bool sem_t;

#define spinlock_until(blocker)                 \
    while (!(*blocker))

#define sem_init(sem, initial_value)            \
    sem_t sem;                                  \
    sem = initial_value

#define sem_check(sem)                          \
    if (sem)

#define sem_post(sem)                           \
    sem = 1

/* TODO: convert this to make the current thread sleep instead of
   unnecessarily consuming resources. OS thread sleeping must be
   completed before this can happen, however. */
#define sem_wait(sem)                    \
    while (!sem);                       \
    sem = 0

#endif

/* End Doxygen group
 * @}
 */
