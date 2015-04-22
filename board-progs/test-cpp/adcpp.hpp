#ifndef __adcpp__
#define __adcpp__

#include "semaphorepp.hpp"

#include <stdbool.h>
#include <stdint.h>

typedef uint32_t memory_address_t;

class adc {
private:

    static const uint8_t default_priority = 0;
    static const uint16_t signal_length = 128;

    memory_address_t base;
    uint8_t sequencer;
    uint8_t channel;
    uint32_t configuration;
    uint8_t trigger_source;

    uint32_t producer_index;
public:
    /*! Initialize adc. */
    adc();
    adc(memory_address_t adc_base);
    void adc_configure(uint8_t adc_sequencer, uint8_t adc_channel,
                       uint32_t adc_configuration, uint8_t adc_trigger_source);

    void start();
    void stop();
    void sample();

    semaphore sem;
    uint32_t data[signal_length];
};

#endif  /* __ADC__ */
