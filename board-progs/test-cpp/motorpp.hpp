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

typedef uint16_t percent_t;
typedef uint32_t memory_address_t;
#define DEFAULT_PWM_PERIOD 0xFFFF

class motor : public critical {
private:
    blinker ctrl;
    blinker pwm;
    memory_address_t ctrl_pin;
    memory_address_t pwm_base;
    memory_address_t pwm_gen;
    memory_address_t pwm_out;
    uint16_t pwm_max_period;
    bool motor_installed_backwards;

    uint32_t pwm_period;
    uint32_t duty_period;
    Direction direction;

    /* Matt's functions -- untouched for sanity */
    /*! Initializes pins PE0-3 for output */
    void motor_init();

public:
    void pwm_init();

    motor();
    motor(memory_address_t ctrl_base, memory_address_t ctrl_pin,
          memory_address_t pwm_base, memory_address_t pwm_gen,
          memory_address_t pwm_out, bool logical_reverse);

    /*! Cut all power to the motor. */
    void stop(void);

    /*! Enable power to the motor. */
    void start(void);

    /*! Set the motor duty cycle and direction. */
    void set(percent_t percent_full_speed);

    /*! Set motor speed and direction. */
    void set(percent_t percent_full_speed, Direction dir);

    /*! Reverse motor direction. */
    void reverse(void);
};

#endif

/*! End doxygen group
 * @}
 */
