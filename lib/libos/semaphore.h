/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave 2015-02-17 */
/* Revision History: Look in Git FGT */

#include <stdbool.h>
#include <stdint.h>

/*! Implements a simple spinlock-based locking mechanism. 
 *  \param blocker the spinlock will wait until this value is set to
 *  nonzero.
 */
void spinlock_until(int32_t* blocker);

/*! Semaphore typedef */
typedef int32_t sem_t;

/*! Initializes a blocking semaphore
 *  \param sem the semaphore to initialize.
 *  \param initial_value the maximum number of concurrent consumers of
 *  the resource guarded by the semaphore.
 */
void sem_init(sem_t* sem, int32_t initial_value);

/*! Post (release) a semaphore lock. This increments the value of the
 *  semaphore so that additional resources may be consumed.
 *  \param sem the semaphore to post (release).
 */
void sem_post(sem_t* sem);

/*! Wait on (block) a semaphore. This waits until the value of the
 *  semaphore is larger than zero to * unblock the current thread. The
 *  value of the semaphore is then decremented to consume the resource.
 *  \param sem the semaphore to wait on (block).
 */
void sem_wait(sem_t* sem);
