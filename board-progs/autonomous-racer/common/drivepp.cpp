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

void drive::steer(uint32_t left_sens, uint32_t left_front_sens,
                  uint32_t right_sens, uint32_t right_front_sens,
                  uint32_t back_sens) {

    /* todo: feed the lf/f, rf/r data here for porportional control of
     * the motors. the side with the larger coefficient slows more */

    percent_t race_speed = 50;
    Direction dir = FORWARD;

    uint32_t left_speed = race_speed;
    uint32_t right_speed = race_speed;

    /* int32_t side_error = ((int32_t)left_sens - (int32_t)right_sens)*race_speed/30; */
    /* int32_t oblique_error = ((int32_t)left_front_sens - (int32_t)right_front_sens)*race_speed/30; */
    /* int32_t side_error_dir = (side_error < 0)*2-1; */
    /* int32_t oblique_error_dir = (oblique_error < 0)*2-1; */

    /* if (oblique_error < -100 || oblique_error > 100) { */
    /*     if ((oblique_error > oblique_error_dir*2000) || (oblique_error < oblique_error_dir*2000)) { */
    /*         left_speed -= (oblique_error)/race_speed/4; */
    /*         right_speed += (oblique_error)/race_speed/5; */
    /*     } else if ((oblique_error > oblique_error_dir*1500) || (oblique_error < oblique_error_dir*1500)) { */
    /*         left_speed -= (oblique_error)/race_speed/4; */
    /*         right_speed += (oblique_error)/race_speed/4; */
    /*     } else if ((oblique_error > oblique_error_dir*1000) || (oblique_error < oblique_error_dir*1000)) { */
    /*         left_speed -= (oblique_error)/race_speed/4; */
    /*         right_speed += (oblique_error)/race_speed/4; */
    /*     } else if ((oblique_error > oblique_error_dir*500) || (oblique_error < oblique_error_dir*500)) { */
    /*         left_speed -= (oblique_error)/race_speed/4; */
    /*         right_speed += (oblique_error)/race_speed/4; */
    /*     } */
    /* } */

    left_speed = 50;
    right_speed = 50;

    left->set(left_speed, dir);
    right->set(right_speed, dir);

}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
