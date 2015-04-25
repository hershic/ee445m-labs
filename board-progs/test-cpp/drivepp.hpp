/* -*- mode: c++; c-basic-offset: 4; -*- */
#ifndef __drivepp__
#define __drivepp__

#include <stdint.h>

#include "motorpp.hpp"
#include "direction.hpp"

/*! \addtogroup Drive
 * @{
 */

typedef uint32_t distance;      /* cm */
typedef uint16_t angle;         /* 0 - 360 */

class drive {
private:
    motor* left;
    motor* right;

public:
    drive();
    drive(motor* left, motor* right);

    /*! Move forward at some percent_t of full-speed. */
    void forward(percent_t speed);

    /*! Move backward at some percent_t of full-speed. */
    void backward(percent_t speed);

    /*! Set motor vectors */
    void set(percent_t speed, Direction dir);

    /*! Stop all motors */
    void stop(void);

    /*! Re-enable all previoulsy stopped motors */
    void start(void);

    /*! Feed the autonomous driver. */
    void steer(uint32_t lfol, uint32_t rfor);
};

#endif

/*! End doxygen group
 * @}
 */

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
