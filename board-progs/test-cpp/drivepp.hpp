/* -*- mode: c++; c-basic-offset: 4; -*- */
#ifndef __drivepp__
#define __drivepp__

#include <stdint.h>

#include "motorpp.hpp"
#include "direction.hpp"

/* As much precision as we can get with uint32_t's. With these two
 * 31-bit numbers, the difference between sqrt(2) and SQRT_2 is
 * -3.0950797e-12.  */
#define SQRT_2 (1855077841/1311738121)

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
    drive(motor* left, motor* right, percent_t speed = 0,
          Direction direction = FORWARD);

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

    /*! Feed the autonomous driver sensor inputs. */
    void steer(uint32_t left_sens, uint32_t left_front_sens,
               uint32_t right_sens, uint32_t right_front_sens);
};

#endif

/*! End doxygen group
 * @}
 */

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
