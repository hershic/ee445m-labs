/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Eric Crosson 2015-03-22 */
/* Revision history: Look in Git FGT */

#ifndef __adc_sim__
#define __adc_sim__

/*! \addtogroup Adc_Sim Static methods for simulation of adc data
 * @{
 */

#include <stdint.h>

class adc_sim {
public:
    /*! Initialize the adc simulator */
    adc_sim();
    /*! Sample the simulated adc for the next value of the sine
     *  wave */
    uint32_t adc_get();
private:
    /*! Current index in the sine wave */
    uint32_t idx;
    /*! Length of the sine wave model */
    const static uint32_t sine_length = 1024;
    const static uint32_t sine[sine_length];
};

#endif

/* End Doxygen group
 * @}
 */
