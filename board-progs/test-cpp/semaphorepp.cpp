#include "semaphorepp.hpp"

#include "libos/os.h"

semaphore::semaphore() {

    reset();
}

void semaphore::reset() {

    int32_t status = StartCritical();
    value = 0;
    EndCritical(status);
}

void semaphore::wait() {

    while(blocked()) {
        os_surrender_context();
    }
    int32_t status = StartCritical();
    --value;
    EndCritical(status);
}

void semaphore::post() {

    int32_t status = StartCritical();
    value++;
    EndCritical(status);
}

bool semaphore::guard() {

    return value > 0;
}

bool semaphore::blocked() {

    return value <= 0;
}

/*! \warning: you had better be guaranteed permission to take this */
void semaphore::take() {

    int32_t status = StartCritical();
    value--;
    EndCritical(status);
}
