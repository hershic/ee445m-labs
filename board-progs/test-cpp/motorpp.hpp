/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __motorpp__
#define __motorpp__

#include <stdint.h>

/*! \addtogroup Motor
 * @{
 */

enum Direction { FORWARD, BACKWARD, };

class motor {
private:
    uint32_t pwm_period;
    uint32_t duty_period;
    Direction direction;

    /* Matt's functions -- untouched for sanity */
    /*! Initializes pins PE0-3 for output */
    void motor_init(void);

    void pwm0a_init(uint16_t period, uint16_t duty);

public:
    motor();
    motor(uint32_t pwm_period, uint32_t duty_period,
          Direction direction);

    /*! Cut all power to the motor. */
    void stop(void);

    /*! Set the motor duty cycle and direction. */
    void set(uint32_t pwm_period, uint32_t duty_period,
             Direction direction);
};

#endif

/*! End doxygen group
 * @}
 */
