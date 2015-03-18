/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-03-15 */
/* Revision history: Look in Git FGT */

#ifndef __blinker__
#define __blinker__

/*! \addtogroup
   * @{
    */

#include <stdint.h>

#include "gpio_twiddler.hpp"

#include "driverlib/gpio.h"

const pin_t PIN_RED   = GPIO_PIN_1;
const pin_t PIN_GREEN = GPIO_PIN_3;
const pin_t PIN_BLUE  = GPIO_PIN_2;

class blinker : public gpio_twiddler {
public:
    blinker();
    virtual void set_base(port_t port);
    virtual void turn_on(pin_t pin);
    virtual void turn_off(pin_t pin);
    virtual void toggle(pin_t pin);
private:
    uint32_t port_base;
};

#endif

/* End Doxygen group
    * @}
     */
