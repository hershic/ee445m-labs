/* -*- mode: c++; c-basic-offset: 4; */
#ifndef __adcpp__
#define __adcpp__

#include "semaphorepp.hpp"
#include "interruptable.hpp"

#include <stdbool.h>
#include <stdint.h>

#define TIMER_KNOWN
#ifdef TIMER_KNOWN
#include "timerpp.hpp"
#endif

typedef uint32_t memory_address_t;

class adc : public interruptable {
private:
    static const uint8_t default_priority = 0;
    static const uint8_t max_num_sequencer_steps = 8;

public:
    memory_address_t base;
    uint8_t sequencer;
    uint8_t channel_count;
    uint32_t configuration;
    uint8_t trigger_source;
    uint32_t sequencer_data[max_num_sequencer_steps];

    /*! Initialize adc. */
    adc();
    adc(memory_address_t adc_base, uint8_t adc_trigger_source, uint8_t adc_sequencer);
    void configure_sequence(uint32_t sequencer_configuration);
    void configure_timer_interrupt(uint32_t timer_base, uint32_t timer_subtimer);
#ifdef TIMER_KNOWN
    void configure_timer_interrupt(timer* t);
#endif

    virtual void start(void);
    virtual void stop(void);
    virtual uint32_t ack(void);
    void sample(void);
    uint32_t get_sample(uint8_t i);
};

#endif  /* __ADC__ */

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
