#ifndef __ADC__
#define __ADC__

#include <stdbool.h>
#include <stdint.h>
#include "libhw/hardware.h"

/* TODO: Create macros for this */

/* TODO: fix documentation */
/*! Initialize the ADC device on the board
 *  \return void
 */
void adc_init(hw_metadata metadata);

void adc_channel_init(hw_metadata metadata);
void adc_interrupt_init(hw_metadata metadata);

#endif  /* __ADC__ */
