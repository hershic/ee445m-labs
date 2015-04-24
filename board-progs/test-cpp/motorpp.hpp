/* -*- mode: c++; c-basic-offset: 4; -*- */
#ifndef __motorpp__
#define __motorpp__

#include <stdint.h>

#include "blinker.hpp"
#include "direction.hpp"
#include "criticalpp.hpp"

/*! \addtogroup Motor
 * @{
 */

typedef uint32_t memory_address_t;
#define DEFAULT_PWM_PERIOD 0xFFFF

class motor : public critical {
private:
    blinker ctrl;
    blinker pwm;
    memory_address_t ctrl_pin;
    memory_address_t pwm_base;
    memory_address_t pwm_pin;
    uint16_t pwm_max_period;
    bool motor_installed_backwards;

    uint32_t pwm_period;
    uint32_t duty_period;
    Direction direction;

    /* Matt's functions -- untouched for sanity */
    /*! Initializes pins PE0-3 for output */
    void motor_init(uint16_t period, uint16_t duty);

public:
    void pwm0a_init(uint16_t period, uint16_t duty);

    motor();
    motor(memory_address_t ctrl_base, memory_address_t ctrl_pin,
          memory_address_t pwm_base, memory_address_t pwm_pin,
          bool logical_reverse);

    /*! Cut all power to the motor. */
    void stop(void);

    /*! Enable power to the motor. */
    void start(void);

    /*! Set the motor duty cycle and direction. */
    void set(uint32_t pwm_period, uint32_t duty_period);
};

#endif

/*! End doxygen group
 * @}
 */
