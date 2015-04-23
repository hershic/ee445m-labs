#ifndef __irpp__
#define __irpp__

/* #include "circularbuffer.hpp" */
#include "circularbuffer.hpp"
#include "semaphorepp.hpp"
#include "adcpp.hpp"

#include <stdbool.h>
#include <stdint.h>

#define IR_BUFFER_LENGTH 64
#define IR_BUFFER_TYPE   int16_t

class ir {
private:

public:
    ir();
    ir(semaphore* ir_sem, uint8_t ir_adc_sequence_step, adc* ir_assoc_adc);
    void sample();

    circularbuffer<IR_BUFFER_TYPE, IR_BUFFER_LENGTH> buf;
    semaphore* sem;
    uint8_t adc_sequence_step;
    adc* assoc_adc;
};

#endif  /* __IR__ */
