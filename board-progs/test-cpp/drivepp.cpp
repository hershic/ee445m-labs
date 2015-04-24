/* -*- mode: c++; c-basic-offset: 4; -*- */
#include "drivepp.hpp"

#include "libos/os.h"

drive::drive() {}

drive::drive(motor* left, motor* right) {

    this->left = left;
    this->right = right;
}

void drive::stop() {

}

void drive::forward(percent speed) {

}

void drive::backward(percent speed) {

}

void drive::turn(Direction dir, percent speed) {


}
