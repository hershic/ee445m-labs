/* -*- mode: c++; c-basic-offset: 4; -*- */
#ifndef __drivepp__
#define __drivepp__

#include <stdint.h>

#include "motorpp.hpp"
#include "direction.hpp"

#define steps_to_wait 200

/*! \addtogroup Drive
 * @{
 */

typedef uint32_t distance;      /* cm */
typedef uint16_t angle;         /* 0 - 360 */

class drive {
private:
    motor* left;
    motor* right;
    int32_t integral_oblique_error;
    int32_t integral_side_error;

    static const int32_t kp_oblique_num = 10;
    static const int32_t kp_oblique_denom = 1;

    static const int32_t kp_side_num = 5;
    static const int32_t kp_side_denom = 2;

    static const int32_t ki_oblique_num = 1;
    static const int32_t ki_oblique_denom = 10000;

    static const int32_t ki_side_num = 1;
    static const int32_t ki_side_denom = 2000;

    int32_t last_counter;

public:
    drive();
    drive(motor* left, motor* right, percent_t speed = 0,
          Direction direction = FORWARD);

    /*! Move forward at some percent of full-speed. */
    void forward(percent_t speed);

    /*! Move backward at some percent of full-speed. */
    void backward(percent_t speed);

    /*! Set motor vectors */
    void set(percent_t speed, Direction dir);

    /*! Stop all motors */
    void stop(void);

    /*! Re-enable all previoulsy stopped motors */
    void start(void);

    /*! Feed the autonomous driver sensor inputs. */
    void steer(uint32_t left_sens, uint32_t left_front_sens,
               uint32_t right_sens, uint32_t right_front_sens,
               uint32_t front_sens);

    void reset_history();
};

#endif

/*! End doxygen group
 * @}
 */

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
