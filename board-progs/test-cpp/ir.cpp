#include "ir.hpp"

ir::ir() {}

ir::ir(uint8_t ir_adc_sequence_step, adc* ir_assoc_adc) {

    buf = circularbuffer<IR_BUFFER_TYPE, IR_BUFFER_LENGTH>();
    adc_sequence_step = ir_adc_sequence_step;
    assoc_adc = ir_assoc_adc;
}

void ir::sample() {
    buf.add(assoc_adc->get_sample(adc_sequence_step));
    /* sem += adc_producer_index / (signal_length-1); */
}
