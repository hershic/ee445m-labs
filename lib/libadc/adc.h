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

/*! Encode the ADC channel bit mask from the integer ADC channel number
 *  \param channel the integer channel to encode into the bit mask
 *  \param encoded_channel the encoded channel's bit mask
 *  \returns whether the decoding was successful or not
 */
bool encode_adc_channel(uint32_t channel, uint32_t* encoded_channel);

/*! Collect ADC data into the specified buffer at a specified interval
 *  \param channel the channel from which to sample
 *  \param frequency the frequency in Hz of the sampling process
 *  \param buffer a pointer to the buffer to populate
 *  \param timer_peripheral the timer to use for this process
 */
uint32_t adc_collect(uint32_t channel, uint32_t frequency,
                     unsigned long buffer[], uint32_t timer_peripheral);

/*! The function the periodic timer calls to actually populate the ADC
 *  data. It operates on the array given in the adc_collect method
 *  \returns void
 */
uint32_t do_adc_func();

/*! KLUDGE: Opens the specified channel on ADC0
 *  \param channel the channel on ADC0 to open
 *  \returns void
 */
uint32_t adc_open(uint32_t channel);

#endif  /* __ADC__ */
