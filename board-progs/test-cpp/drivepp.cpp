#include "drivepp.hpp"

#include "libos/os.h"

drive::drive() {

}

drive::drive(motor left, motor right, distance wheel_circumference) {

    this->left = left;
    this->right = right;
    this->wheel_circum = wheel_circumference;
}

void drive::stop() {

}

void drive::forward(percent speed) {

}

void drive::forward(percent speed, distance distance) {

}

void drive::backward(percent speed) {

}

void drive::backward(percent speed, distance distance) {

}

void drive::turn(Direction dir, angle ang) {

}
