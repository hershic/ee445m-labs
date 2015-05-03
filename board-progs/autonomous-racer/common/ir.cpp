#include "ir.hpp"

ir::ir() {}

ir::ir(uint8_t ir_adc_sequence_step, adc* ir_assoc_adc,
       int32_t ir_a, int32_t ir_b, int32_t ir_k) {

    buf = circularbuffer<IR_BUFFER_TYPE, IR_BUFFER_LENGTH>();
    adc_sequence_step = ir_adc_sequence_step;
    assoc_adc = ir_assoc_adc;
    calibrate(ir_a, ir_b, ir_k);
}

void ir::calibrate(int32_t a, int32_t b, int32_t k) {

    calibration.a = a;
    calibration.b = b;
    calibration.k = k;
}

void ir::sample() {
    buf.add(assoc_adc->get_sample(adc_sequence_step));
    /* sem += adc_producer_index / (signal_length-1); */
}

int32_t ir::average() {

    uint32_t i, value;
    for (i=0; i<buf.len; ++i) {
        value += buf.buf[i];
    }
    value /= buf.len;
    return (IR_BUFFER_TYPE)value;
}

/*! Converts the values of the IR distance sensor to centimeters */
/*! \returns 0 if the conversion did not succeed, data otherwise */
int32_t ir::distance() {

    return (int32_t)(calibration.a / (average() + calibration.b) - calibration.k);
}
