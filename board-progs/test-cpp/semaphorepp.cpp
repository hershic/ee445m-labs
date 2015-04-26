#include "semaphorepp.hpp"

#include "libos/os.h"

#include "criticalpp.hpp"

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

/*! \note after checking all guard's, ensure you call
 *  \semaphore::done_with_guards() to re-enable interrupts */
bool semaphore::guard() {

    if (!critical::primask_saved()) {
        critical::save_primask();
    }
    return value > 0;
}

void semaphore::done_with_guards() {

    critical::restore_primask(true);
}

bool semaphore::blocked() {

    return value <= 0;
}

/*! \warning: you had better be guaranteed permission to take this */
void semaphore::take() {

    if(value <=0) {
        /* if you ever get caught here this means you need to remove
         * the critical section that exists between the method calls
         * to semaphore::guard and semaphore:;take */
        while(1) {}
    }
    value--;
    if (critical::primask_saved()) {
        critical::restore_primask();
    }
}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
