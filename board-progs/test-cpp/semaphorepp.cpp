#include "semaphorepp.hpp"

#include "libos/os.h"

semaphore::semaphore() {

    reset();
}

semaphore::semaphore(int16_t initial_value) {

    int32_t status = StartCritical();
    value = initial_value;
    EndCritical(status);
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
    if(value <=0) {
        /* if you ever get caught here this means you need to remove
         * the critical section that exists between the method calls
         * to semaphore::guard and semaphore:;take */
        while(1) {}
    }
    value--;
    EndCritical(status);
}
