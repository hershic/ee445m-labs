#include "libtimer/timer.h"

#include <stdint.h>

void adc_init(void);
uint32_t decode_adc_channel(uint32_t channel, uint32_t* decoded_channel);
uint32_t adc_collect(uint32_t channel, uint32_t frequency,
                     unsigned long buffer[], uint32_t numsamples,
                     utimer_t timer_peripheral);
void do_adc_func();
uint32_t adc_open(uint32_t channel);
