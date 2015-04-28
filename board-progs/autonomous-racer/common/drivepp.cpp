/* -*- mode: c++; c-basic-offset: 4; -*- */
#include "drivepp.hpp"

/* map, map, where art thou map */

drive::drive() {}

drive::drive(motor* left, motor* right, percent_t speed, Direction direction) {

    this->left = left;
    this->right = right;
    set(speed, direction);
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

int32_t clamp(int32_t value, int32_t min, int32_t max) {
    if (value > max) {
        return max;
    } else if (value < min) {
        return min;
    } else {
        return value;
    }
}

void drive::steer(uint32_t left_sens, uint32_t left_front_sens,
                  uint32_t right_sens, uint32_t right_front_sens,
                  uint32_t back_sens) {

    /* todo: feed the lf/f, rf/r data here for porportional control of
     * the motors. the side with the larger coefficient slows more */

    percent_t race_speed = 30;
    Direction dir = FORWARD;

    uint32_t left_speed = race_speed;
    uint32_t right_speed = race_speed;

    /* percent_t left_speed = delta(left_sens, left_front_sens); */
    /* percent_t right_speed = delta(right_sens, right_front_sens); */

    /* todo: utilize \back_sens */

    /* optional: path-centering algorithm */

    /* left->set(race_speed*left_speed/100, dir); */
    /* right->set(race_speed*right_speed/100, dir); */

    if (left_sens < 1500) {
        left_speed += 2;
        right_speed -= 2;
    } else if (left_sens > 1600) {
        left_speed -= 2;
        right_speed += 2;
    }

    if (left_front_sens < 750) {
        left_speed += (750-left_front_sens)/30;
        right_speed -= (750-left_front_sens)/30;
    } else if (left_front_sens > 950) {
        left_speed -= clamp(left_front_sens, 950, 1500)/68;
        right_speed += clamp(left_front_sens, 950, 1500)/68;
    }

    if (left_front_sens < 450) {
        left_speed = race_speed;
        right_speed = race_speed/2;
    }

    left->set(left_speed);
    right->set(right_speed);

}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
