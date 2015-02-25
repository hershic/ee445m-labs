#include "semaphore.h"

void spinlock_until(int32_t* blocker) {

    while (!(*blocker));
}

void sem_init(sem_t* sem, bool initial_value) {
    *sem = initial_value;
}

void sem_post(sem_t* sem) {
    *sem = 1;
}

/* TODO: convert this to make the current thread sleep instead of
   unnecessarily consuming resources. OS thread sleeping must be
   completed before this can happen, however. */
void sem_wait(sem_t* sem) {
    /* spinlock */
    while (!*sem);
    *sem = 0;
}
