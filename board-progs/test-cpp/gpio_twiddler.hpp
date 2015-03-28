/* -*- mode: c++; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-03-18 */
/* Revision history: Look in Git FGT */
#ifndef __gpio_twiddler__
#define __gpio_twiddler__

/*! \addtogroup
 * @{
 */

typedef uint32_t port_t;
typedef uint32_t pin_t;

class gpio_twiddler {
public:
    gpio_twiddler() {}
    virtual void set_base(port_t) = 0;
    virtual void turn_on(pin_t) = 0;
    virtual void turn_off(pin_t) = 0;
    virtual void toggle(pin_t) = 0;
};
#endif

/* End Doxygen group
 * @}
 */

