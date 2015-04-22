#include "adcpp.hpp"

#include "inc/tm4c123gh6pm.h"

#include "driverlib/adc.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/sysctl.h"

#include "inc/hw_memmap.h"

adc::adc() {}

adc::adc(memory_address_t adc_base) {

    base = adc_base;
    /* todo: determine where the other ADC ports are located */
    switch(adc_base) {
    case ADC0_BASE:
        SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOE);
        SysCtlGPIOAHBEnable(SYSCTL_PERIPH_GPIOE);
        GPIOPinTypeADC(GPIO_PORTE_AHB_BASE,
                       GPIO_PIN_3 | GPIO_PIN_2 | GPIO_PIN_1);
        SysCtlPeripheralEnable(SYSCTL_PERIPH_ADC0);
        SysCtlPeripheralReset(SYSCTL_PERIPH_ADC0);
        break;
    default:
        while(1) {}
    }
    /* we don't have to set the reference for the internal ADC, but we
       might if we have an external one. */
    ADCReferenceSet(base, ADC_REF_INT);
}

void adc::adc_configure(uint8_t adc_sequencer, uint8_t adc_channel,
    uint32_t adc_configuration, uint8_t adc_trigger_source) {

    sequencer = adc_sequencer;
    channel = adc_channel;
    configuration = adc_configuration;
    trigger_source = adc_trigger_source;

    /* Disable the sample sequencer so we can configure it */
    ADCSequenceDisable(base, sequencer);

    /* From TI: Configure step 0 on sequence 3.  Sample channel 0
       (ADC_CTL_CH0) in single-ended mode (default) and configure the
       interrupt flag (ADC_CTL_IE) to be set when the sample is done.
       Tell the ADC logic that this is the last conversion on sequence
       3 (ADC_CTL_END).  Sequence 3 has only one programmable step.
       Sequence 1 and 2 have 4 steps, and sequence 0 has 8
       programmable steps.  Since we are only doing a single
       conversion using sequence 3 we will only configure step 0.  For
       more information on the ADC sequences and steps, reference the
       datasheet. */
    ADCSequenceConfigure(base, sequencer, trigger_source, default_priority);
    ADCSequenceStepConfigure(base, sequencer, channel, configuration);
    IntEnable(INT_ADC0SS0 + sequencer);
}

void adc::start() {

    ADCIntEnable(base, sequencer);
    ADCSequenceEnable(base, sequencer);
}

void adc::stop() {

    ADCIntDisable(base, sequencer);
    ADCSequenceDisable(base, sequencer);
}

inline void increment_ptr(uint32_t* ptr, uint32_t wrap_len) {
    *ptr = (*ptr + 1) % wrap_len;
}

void adc::sample() {

    ADCSequenceDataGet(base, sequencer, &data[producer_index]);
    /* sem += producer_index / (signal_length-1); */
    increment_ptr(&producer_index, signal_length);
}
