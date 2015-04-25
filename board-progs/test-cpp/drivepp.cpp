/* -*- mode: c++; c-basic-offset: 4; -*- */
#include "drivepp.hpp"

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

void drive::steer(uint32_t lfol, uint32_t rfor) {

    /* todo: feed the lf/f, rf/r data here for porportional control of
     * the motors. the side with the larger coefficient slows more */
}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
