/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __drivepp__
#define __drivepp__

#include <stdint.h>

#include "motorpp.hpp"
#include "direction.hpp"

/*! \addtogroup Drive
 * @{
 */

typedef uint8_t percent;        /* 1 - 100 */
typedef uint32_t distance;      /* cm */
typedef uint16_t angle;         /* 1 - 360 */

class drive {
private:
    motor left;
    motor right;
    distance wheel_circum;

public:
    drive();
    drive(motor left, motor right, distance wheel_circumference);

    /*! Move forward at some percent of full-speed. */
    void forward(percent speed);

    /*! Move forward a set distance at some percent of full-speed. */
    void forward(percent speed, distance distance);

    /*! Move backward at some percent of full-speed. */
    void backward(percent speed);

    /*! Move backward a set distance at some percent of full-speed. */
    void backward(percent speed, distance distance);

    /*! Turn ang degrees in direction dir. */
    void turn(Direction dir, angle ang);

    /*! Stop all motors */
    void stop(void);
};

#endif

/*! End doxygen group
 * @}
 */
