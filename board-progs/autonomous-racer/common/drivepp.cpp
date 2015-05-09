/* -*- mode: c++; c-basic-offset: 4; -*- */
#include "drivepp.hpp"
#include "math.hpp"
#include "ir.hpp"
#include "motorpp.hpp"

#include "driverlib/gpio.h"
#include "inc/hw_memmap.h"

drive::drive() {}

drive::drive(motor* left, motor* right, percent_t speed, Direction direction) {

    this->left = left;
    this->right = right;
    set(speed, direction);
    integral_oblique_error = 0;
}

void drive::stop() {

    left->stop();
    right->stop();
}

void drive::start() {

    left->start();
    right->start();
}

void drive::forward(percent_t speed) {

    left->set(speed, FORWARD);
    right->set(speed, FORWARD);
}

void drive::backward(percent_t speed) {

    left->set(speed, BACKWARD);
    right->set(speed, BACKWARD);
}

void drive::set(percent_t speed, Direction dir) {

    left->set(speed, dir);
    right->set(speed, dir);
}

void drive::steer(uint32_t left_sens, uint32_t left_front_sens,
                  uint32_t right_sens, uint32_t right_front_sens,
                  uint32_t front_sens) {

    /* todo: feed the lf/f, rf/r data here for porportional control of
     * the motors. the side with the larger coefficient slows more */

    Direction dir = FORWARD;

    int32_t left_speed = left->pwm_max_period*55/100;
    int32_t right_speed = right->pwm_max_period*55/100;

    int32_t oblique_error = ((int32_t)left_front_sens - (int32_t)right_front_sens);
    integral_oblique_error = clamp(integral_oblique_error + oblique_error, -motor::pwm_max_period/4, motor::pwm_max_period/4);

    int32_t side_error = ((int32_t)left_sens - (int32_t)right_sens);
    integral_side_error = clamp(integral_side_error + side_error, -motor::pwm_max_period/8, motor::pwm_max_period/8);

    int32_t should_use_side_sensors = front_sens < 200;
    should_use_side_sensors = 0;

    int32_t should_slow_down = front_sens < 200;

    int32_t delta = oblique_error*kp_oblique_num/kp_oblique_denom;

    if (should_slow_down > 0) { delta /= 2; }

    left_speed +=  delta;
    right_speed -= delta;

    left->set(left_speed, dir);
    right->set(right_speed, dir);

    ++last_counter;
}

void drive::reset_history() {

    integral_oblique_error = 0;
    integral_side_error = 0;
}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
