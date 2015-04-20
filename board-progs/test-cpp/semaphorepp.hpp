/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-03-08 */
/* Revision history: Look in Git FGT */
#ifndef __semaphorepp__
#define __semaphorepp__

/*! \addtogroup semaphore
 * @{
 */

class semaphore {
private:
    int16_t value;
public:
    semaphore();

    /*! Increment the semaphore. */
    void post(void);

    /*! Wait on a semaphore. */
    void wait(void);

    /*! Take the semaphore */
    void take(void);

    /*! True if the semaphore is ready to take. */
    bool guard(void);

    /*! True if the semaphore is blocked. */
    bool blocked(void);

    /*! Reset the semaphore. */
    void reset(void);
}

#endif

/* End Doxygen group
    * @}
     */
