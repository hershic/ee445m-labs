/* -*- mode: c++; c-basic-offset: 4; -*- */
#include "drivepp.hpp"
#include "math.hpp"

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
                  uint32_t back_sens) {

    /* todo: feed the lf/f, rf/r data here for porportional control of
     * the motors. the side with the larger coefficient slows more */

    Direction dir = FORWARD;

    /* int32_t left_speed = left->get(); */
    /* int32_t right_speed = right->get(); */

    int32_t left_speed = left->pwm_max_period/2;
    int32_t right_speed = right->pwm_max_period/2;

    int32_t oblique_error = ((int32_t)left_front_sens - (int32_t)right_front_sens);
    integral_oblique_error += oblique_error;

    int32_t side_error = ((int32_t)left_sens - (int32_t)right_sens);
    integral_side_error += side_error;

    int32_t should_use_side_sensors = (left_front_sens < use_side_sensor_threshold) ||
        (right_front_sens < use_side_sensor_threshold);

    left_speed += oblique_error*kp_oblique_num/kp_oblique_denom +
        (should_use_side_sensors * side_error * kp_side_num/kp_side_denom);
    right_speed -= oblique_error*kp_oblique_num/kp_oblique_denom +
        (should_use_side_sensors * side_error * kp_side_num/kp_side_denom);

    /* left_speed += oblique_error*kp_num/kp_denom + integral_oblique_error*ki_num/ki_denom; */
    /* right_speed -= oblique_error*kp_num/kp_denom + integral_oblique_error*ki_num/ki_denom; */

    left->set(left_speed, dir);
    right->set(right_speed, dir);
}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
