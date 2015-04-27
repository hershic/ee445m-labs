/* -*- mode: c++; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-03-08 */
/* Revision history: Look in Git FGT */
#ifndef __semaphorepp__
#define __semaphorepp__

#include <stdint.h>

#include "criticalpp.hpp"

/*! \addtogroup semaphore
 * @{
 */

class semaphore : public critical {
private:
    int16_t value;
public:
    semaphore();
    semaphore(int16_t initial_value);

    /*! Increment the semaphore. */
    void post(void);

    /*! Wait on a semaphore. */
    void wait(void);

    /*! Take the semaphore */
    void take(void);

    /*! \Wait without the waiting -- take if ready, leave if not. */
    bool guard(void);

    /*! True if the semaphore is blocked. */
    bool blocked(void);

    /*! Reset the semaphore. */
    void reset(void);
};

#endif

/* End Doxygen group
    * @}
     */

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
