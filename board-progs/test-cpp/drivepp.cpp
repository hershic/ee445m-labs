/* -*- mode: c++; c-basic-offset: 4; -*- */
#include "drivepp.hpp"

#include "libos/os.h"

/* map, map, where art thou map */

drive::drive() {}

drive::drive(motor* left, motor* right) {

    this->left = left;
    this->right = right;
}

void drive::stop() {

    left->stop();
    right->stop();
}

void drive::forward(percent speed) {

    left->set(speed);
    right->set(speed);
}

void drive::backward(percent speed) {

    left->reverse();
    right->reverse();
}

void drive::steer(uint32_t lfol, uint32_t rfor) {

    /* todo: feed the lf/f, rf/r data here for porportional control of
     * the motors */
}
