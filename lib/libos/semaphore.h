/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave 2015-02-17 */
/* Revision History: Look in Git FGT */

#ifndef __SEMAPHORE__
#define __SEMAPHORE__

#include <stdbool.h>
#include <stdint.h>

/*! Implements a simple spinlock-based locking mechanism.
 *  \param Block until this value is nonzero.
 */
void spinlock_until(int32_t*);

/*! Semaphore typedef */
typedef int32_t sem_t;

/*! Initializes a blocking semaphore
 *  \param The semaphore to initialize.
 *  \param The maximum number of concurrent consumers of
 *  the resource guarded by the semaphore.
 */
void sem_init(sem_t*, int32_t);

/*! Post (release) a semaphore lock. This increments the value of the
 *  semaphore so that additional resources may be consumed.
 *  \param The semaphore to post (release).
 */
void sem_post(sem_t*);

/*! Wait on (block) a semaphore. This waits until the value of the
 *  semaphore is larger than zero to unblock the current thread. The
 *  value of the semaphore is then decremented to consume the resource.
 *  \param The semaphore to wait on (block).
 */
void sem_wait(sem_t*);

#endif  /* __SEMAPHORE__ */
