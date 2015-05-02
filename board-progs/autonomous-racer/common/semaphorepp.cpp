#include "semaphorepp.hpp"

#include "libos/os.h"

#include "criticalpp.hpp"

semaphore::semaphore() {

    init();
    reset();
}

semaphore::semaphore(int16_t initial_value) {

    int32_t status = StartCritical();
    value = initial_value;
    EndCritical(status);
    init();
}

void semaphore::init() {

#if TEST_SEMAPHORE == 1
    total_posts = 0;
#endif
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
    ++value;
#if TEST_SEMAPHORE == 1
    ++total_posts;
#endif
    EndCritical(status);
}

/*! \warning this decrements the semaphore if the return value is
 *  true. */
bool semaphore::guard() {

    uint32_t status = StartCritical();
    bool ret = value > 0;
    if (ret) {
        --value;
    }
    EndCritical(status);
    return ret;
}

bool semaphore::blocked() {

    return value <= 0;
}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
