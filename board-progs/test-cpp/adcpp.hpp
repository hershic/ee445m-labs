#ifndef __adcpp__
#define __adcpp__

#include "semaphorepp.hpp"

#include <stdbool.h>
#include <stdint.h>

typedef uint32_t memory_address_t;

class adc {
private:
    static const uint8_t default_priority = 0;
    static const uint8_t max_num_sequencer_steps = 8;

public:
    memory_address_t base;
    uint8_t sequencer;
    uint8_t channel_count;
    uint32_t configuration;
    uint8_t trigger_source;
    uint32_t producer_index;
    uint32_t sequencer_data[max_num_sequencer_steps];

    /*! Initialize adc. */
    adc();
    adc(memory_address_t adc_base, uint8_t adc_trigger_source, uint8_t adc_sequencer);
    void configure_sequence(uint32_t sequencer_configuration);
    void configure_timer_interrupt(uint32_t timer_base, uint32_t timer_subtimer);

    void start();
    void stop();
    void sample();
    void ack();
};

#endif  /* __ADC__ */
