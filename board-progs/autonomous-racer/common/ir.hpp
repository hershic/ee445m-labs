/* -*- mode: c++; c-basic-offset: 4; */
#ifndef __irpp__
#define __irpp__

#include "circularbuffer.hpp"
#include "semaphorepp.hpp"
#include "adcpp.hpp"

#include <stdbool.h>
#include <stdint.h>

#define IR_BUFFER_LENGTH 32
#define IR_BUFFER_TYPE   int16_t

class ir {

    /*
      IR Sensor Calibration:

      1 / (d + k) = a * ADC + b

      d - distance in millimeters.
      k - corrective constant (fund using tial-and-error method)
      ADC - digitalized value of voltage.
      a - linear member (value is determined by the trend line equation)
      b - free member(value is determined by the trend line equation)

      d = (1 / (a * ADC + B)) - k
      d = [(1 / a) / (ADC + B / a)] - k // circumvent floating point values

      Example: YELLOW
      a = 0.000003392083496396130
      b = 0.000157499374810688000
      k = 7

      (1/a) = 294804.06395138431054320313024055
      (b/a) = 46.431455763993112340595936913376

      d = (294804 / (ADC + 46)) - 7    millimeters
      calibrate(294804, 46, 7);
    */

    typedef struct calibration_data {
        int32_t a;
        int32_t b;
        int32_t k;
    } calibration_data;

private:
    calibration_data calibration;
    circularbuffer<IR_BUFFER_TYPE, IR_BUFFER_LENGTH> buf;
    uint8_t adc_sequence_step;
    adc* assoc_adc;

    int32_t cached_average;

    static const int32_t max_distance = 600;
    static const int32_t min_distance = 0;

public:
    ir();
    /* the default calibration is ideal as per the GP2Y0A21YK spec sheet*/
    ir(uint8_t ir_adc_sequence_step, adc* ir_assoc_adc,
       int32_t ir_a=240333, int32_t ir_b=-314, int32_t ir_k=0);
    void calibrate(int32_t a, int32_t b, int32_t k);
    void sample(void);
    int32_t average();
    int32_t distance();
};

#endif  /* __IR__ */

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
