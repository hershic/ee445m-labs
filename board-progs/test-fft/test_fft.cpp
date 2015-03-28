/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Eric Crosson 2015-03-22 */
/* Revision history: Look in Git FGT */

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

#include "inc/hw_memmap.h"

#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"

#include "arm_math.h"

#include "adc_sim.hpp"

const uint32_t test_len_samples = 1024;
const uint32_t fft_len = 512;
uint32_t idx = 0;

/*! Location of FFT output data */
static q31_t input[test_len_samples];
static q31_t output[test_len_samples/2];

adc_sim sim;

/* Grab one more sample from the adc */
void TIMER0A_Handler(void) {

    input[idx++] = sim.adc_get();
}

int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN | SYSCTL_XTAL_16MHZ);
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_3);

    /* =================================== get data =================================== */
    /* timer init for periodic sampling of simulated adc */
    /* timer_metadata_init(TIMER0_BASE, 100 Hz, INT_TIMER0A, TIMER_CFG_PERIODIC); */
    /* hw_driver_init(HW_TIMER, timer_metadata); */
    /* end timer init */

    /* begin timer substitution code -- this runs in place of timer
     * init above, not with it */
    adc_sim adc_simulator;
    sim = adc_simulator;
    for(uint32_t i=0; i<test_len_samples; ++i) {
        TIMER0A_Handler();
    }
    /* end timer substitution code */
    /* =================================== end data =================================== */

    /* fft code influenced by http://bit.ly/1GZaHXo */
    uint32_t ifftFlag = 0;
    uint32_t doBitReverse = 0;  /* not sure what this does */

    arm_status status = ARM_MATH_SUCCESS;
    arm_cfft_radix4_instance_q31 S;

    /* Initialize the CFFT/CIFFT module */
    status = arm_cfft_radix4_init_q31(&S, fft_len, ifftFlag, doBitReverse);

    /* Process the data through the CFFT/CIFFT modulke */
    arm_cfft_radix4_q31(&S, input);

    /* Process the data through the Complex Magnitude Model for
     * calculating the magnitude at each bin */
    arm_cmplx_mag_q31(input, output, fft_len);

    /* Loop here to signal a test PASS. Looping in the postpone_death
     * indicates test FAILure. */
    if (status != ARM_MATH_SUCCESS) {
        while(1);
    }

    /* main method does not return */
    while(1);
}
