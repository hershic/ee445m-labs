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


    int32_t side_error = (int32_t)left_sens - (int32_t)right_sens;
    int32_t oblique_error = (int32_t)left_front_sens - (int32_t)right_front_sens;
    int32_t side_error_dir = (side_error < 0)*2-1;
    int32_t oblique_error_dir = (oblique_error < 0)*2-1;

    /* if (side_error < -100) { */
    /*     /\* go left *\/ */

    /*     if (side_error > side_error_dir*2000) { */
    /*         left_speed -= side_error_dir*2; */
    /*         right_speed += side_error_dir*2; */
    /*     } else if (side_error > side_error_dir*1500) { */
    /*         left_speed -= side_error_dir*3; */
    /*         right_speed += side_error_dir*3; */
    /*     } else if (side_error > side_error_dir*1000) { */
    /*         left_speed -= side_error_dir*4; */
    /*         right_speed += side_error_dir*4; */
    /*     } else if (side_error > side_error_dir*100) { */
    /*         left_speed -= side_error_dir*5; */
    /*         right_speed += side_error_dir*5; */
    /*     } */

    /*     /\* left_speed += 2; *\/ */
    /*     /\* right_speed -= 2; *\/ */
    /*     /\* left_speed += (-side_error)/race_speed/10; *\/ */
    /*     /\* right_speed -= (-side_error)/race_speed/10; *\/ */
    /* } else if (side_error > 100) { */
    /*     /\* go right *\/ */

    /*     if (side_error > 2000) { */
    /*         left_speed -= 2; */
    /*         right_speed += 2; */
    /*     } else if (side_error > 1500) { */
    /*         left_speed -= 3; */
    /*         right_speed += 3; */
    /*     } else if (side_error > 1000) { */
    /*         left_speed -= 4; */
    /*         right_speed += 4; */
    /*     } else if (side_error > 100) { */
    /*         left_speed -= 5; */
    /*         right_speed += 5; */
    /*     } */

    /*     /\* left_speed -= 2; *\/ */
    /*     /\* right_speed += 2; *\/ */
    /*     /\* left_speed -= (side_error)/race_speed/10; *\/ */
    /*     /\* right_speed += (side_error)/race_speed/10; *\/ */
    /* } */

    if (oblique_error < -100 || oblique_error > 100) {
        if ((oblique_error > oblique_error_dir*2000) || (oblique_error < oblique_error_dir*2000)) {
            left_speed -= (oblique_error)/race_speed/2;
            right_speed += (oblique_error)/race_speed/2;
        } else if ((oblique_error > oblique_error_dir*1500) || (oblique_error < oblique_error_dir*1500)) {
            left_speed -= (oblique_error)/race_speed/3;
            right_speed += (oblique_error)/race_speed/3;
        } else if ((oblique_error > oblique_error_dir*1000) || (oblique_error < oblique_error_dir*1000)) {
            left_speed -= (oblique_error)/race_speed/4;
            right_speed += (oblique_error)/race_speed/4;
        } else if ((oblique_error > oblique_error_dir*100) || (oblique_error < oblique_error_dir*100)) {
            left_speed -= (oblique_error)/race_speed/5;
            right_speed += (oblique_error)/race_speed/5;
        }
    }

    /* almost evade */
    /* if (left_sens > 3200 || left_front_sens > 3200) { */
    /*     left_speed -= (oblique_error)/race_speed/6; */
    /*     right_speed += (oblique_error)/race_speed/6; */
    /* } */

    /* if (right_sens > 3200 | right_front_sens > 3200) { */
    /*     left_speed -= (oblique_error)/race_speed/6; */
    /*     right_speed += (oblique_error)/race_speed/6; */
    /* } */

    /* evade! */
    /* if (left_sens > 3350 || left_front_sens > 3350) { */
    /*     left_speed -= (oblique_error)/race_speed/3; */
    /*     right_speed += (oblique_error)/race_speed/3; */
    /* } */

    /* if (right_sens > 3350 | right_front_sens > 3350) { */
    /*     left_speed -= (oblique_error)/race_speed/3; */
    /*     right_speed += (oblique_error)/race_speed/3; */
    /* } */

    left->set(left_speed);
    right->set(right_speed);

}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
