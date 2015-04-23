/* -*- mode: c++; c-basic-offset: 4; -*- */
#include "drivepp.hpp"

#include "libos/os.h"

drive::drive() {

}

drive::drive(motor* left, motor* right) {

    this->left = left;
    this->right = right;
}

void drive::stop() {

}

void drive::forward(percent speed) {

    // todo: correct arguments for this fn
    left->set(10000, speed*100, FORWARD);
    // todo: one motor needs to be reversed
    right->set(10000, speed*100, FORWARD);
}

// todo: some variation of forward
void drive::backward(percent speed) {

}

void drive::turn(Direction dir, percent speed) {

}
