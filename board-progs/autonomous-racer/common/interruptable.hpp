/* -*- mode: c++; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-03-18 */
/* Revision history: Look in Git FGT */
#ifndef __interruptable__
#define __interruptable__

/*! \addtogroup Interruptable
 * @{
 */

typedef uint32_t port_t;
typedef uint32_t pin_t;

class interruptable {
public:
    interruptable() {}
    virtual void start() = 0;
    virtual void stop() = 0;
    virtual uint32_t ack() = 0;
};
#endif

/* End Doxygen group
 * @}
 */
