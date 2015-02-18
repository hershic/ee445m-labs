#include "semaphore.h"

void spinlock_until(int32_t* blocker) {

    while (!(*blocker));
}

void sem_init(sem_t* sem, int32_t initial_value) {
    *sem = initial_value;
}

void sem_post(int32_t* sem) {
    ++*sem;
}

/* TODO: convert this to make the current thread sleep instead of
   unnecessarily consuming resources. OS thread sleeping must be
   completed before this can happen, however. */
void sem_wait(int32_t* sem) {
    /* spinlock */
    while (*sem <=0);
    --*sem;
}
