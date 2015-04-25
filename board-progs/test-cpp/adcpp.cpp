#include "adcpp.hpp"

#include "inc/tm4c123gh6pm.h"

#include "driverlib/adc.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/sysctl.h"
#include "driverlib/timer.h"

#include "inc/hw_memmap.h"

adc::adc() {}

adc::adc(memory_address_t adc_base, uint8_t adc_trigger_source,
         uint8_t adc_sequencer) {

    base = adc_base;
    trigger_source = adc_trigger_source;
    sequencer = adc_sequencer;

    /* TODO: determine where the other ADC ports are located */
    switch(adc_base) {
    case ADC0_BASE:
        SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOE);
        SysCtlGPIOAHBEnable(SYSCTL_PERIPH_GPIOE);
        GPIOPinTypeADC(GPIO_PORTE_AHB_BASE,
                       GPIO_PIN_3 | GPIO_PIN_2 | GPIO_PIN_1 | GPIO_PIN_0);
        SysCtlPeripheralEnable(SYSCTL_PERIPH_ADC0);
        SysCtlPeripheralReset(SYSCTL_PERIPH_ADC0);
        break;
    default:
        while(1) {}
    }
    /* we don't have to set the reference for the internal ADC, but we
       might if we have an external one. */
    ADCReferenceSet(base, ADC_REF_INT);

    ADCSequenceConfigure(base, sequencer, trigger_source, default_priority);

    channel_count = 0;
}

void adc::configure_timer_interrupt(timer* t, bool adc_start) {

    TimerControlTrigger(t->base, t->subtimer, true);
    if(adc_start) { start(); }
}

void adc::configure_timer_interrupt(uint32_t timer_base, uint32_t timer_subtimer) {

    TimerControlTrigger(timer_base, timer_subtimer, true);
}

/*! Configure an adc channel */
void adc::configure_sequence(uint32_t sequencer_configuration) {

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
    ADCSequenceStepConfigure(base, sequencer, channel_count,
                             sequencer_configuration);
    ++channel_count;
}

void adc::start() {

    IntEnable(INT_ADC0SS0 + sequencer);
    ADCIntEnable(base, sequencer);
    ADCSequenceEnable(base, sequencer);
}

void adc::stop() {

    IntDisable(INT_ADC0SS0 + sequencer);
    ADCIntDisable(base, sequencer);
    ADCSequenceDisable(base, sequencer);
}

void adc::sample() {

    ADCSequenceDataGet(base, sequencer, sequencer_data);
}

uint32_t adc::get_sample(uint8_t i) {
    return sequencer_data[i];
}

uint32_t adc::ack() {

    ADCIntClear(base, sequencer);
    return 0xDEADBEEF;
}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
