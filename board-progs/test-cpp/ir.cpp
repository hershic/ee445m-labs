#include "ir.hpp"

ir::ir() {}

ir::ir(semaphore* ir_sem, uint8_t ir_adc_sequence_step,
       adc* ir_assoc_adc) {

    buf = circularbuffer<IR_BUFFER_TYPE, IR_BUFFER_LENGTH>();
    sem = ir_sem;
    adc_sequence_step = ir_adc_sequence_step;
    assoc_adc = ir_assoc_adc;
}

void ir::sample() {
    buf.buf[buf.pos] = assoc_adc->get_sample(adc_sequence_step);
    /* sem += adc_producer_index / (signal_length-1); */
}
