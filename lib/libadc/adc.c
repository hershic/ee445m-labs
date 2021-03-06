/* -*- mode: c; c-basic-offset: 4; -*- */
#include "adc.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include "driverlib/pin_map.h"
#include "driverlib/gpio.h"
#include "driverlib/sysctl.h"
#include "driverlib/adc.h"
#include "driverlib/timer.h"

#include "libhw/hardware.h"

/* TODO: Need to be able to init the adc for any combination of ports and pins */
void adc_init(hw_metadata metadata) {

    /* TODO: determine where the other ADC ports are located */
    switch(metadata.adc.base) {
    case ADC0_BASE:
        SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOE);
        SysCtlGPIOAHBEnable(SYSCTL_PERIPH_GPIOE);
        GPIOPinTypeADC(GPIO_PORTE_AHB_BASE,
                       GPIO_PIN_3 | GPIO_PIN_2 | GPIO_PIN_1);
        SysCtlPeripheralEnable(SYSCTL_PERIPH_ADC0);
        SysCtlPeripheralReset(SYSCTL_PERIPH_ADC0);
        break;
    default:
        postpone_death();
    }
    /* we don't have to set the reference for the internal ADC, but we
       might if we have an external one. */
    ADCReferenceSet(metadata.adc.base, ADC_REF_INT);
}

void adc_channel_init(hw_metadata metadata) {

    /* disable the sample sequencer so we can configure it */
    ADCSequenceDisable(metadata.adc.base, metadata.adc.sample_sequence);

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
    /* TODO: PARAMETRIZE THE OPTIONS */
    ADCSequenceConfigure(metadata.adc.base, metadata.adc.sample_sequence,
                         metadata.adc.trigger_source, 0);

    ADCSequenceStepConfigure(metadata.adc.base,
                             metadata.adc.sample_sequence,
                             metadata.adc.channel,
                             metadata.adc.channel_configuration);

    ADCIntEnable(metadata.adc.base, metadata.adc.sample_sequence);

    /* TODO: Parametrize the sequence number */
    ADCSequenceEnable(metadata.adc.base, metadata.adc.sample_sequence);
    /* ADCProcessorTrigger(metadata.adc.base, metadata.adc.sample_sequence); */
}

void adc_interrupt_init(hw_metadata metadata) {

    if (metadata.adc.trigger_source == ADC_TRIGGER_TIMER) {
        hw_init(HW_TIMER, (hw_metadata)(metadata.adc.trigger_metadata.timer));
        TimerControlTrigger(metadata.adc.trigger_metadata.timer.base,
                            TIMER_A, true);
        hw_channel_init(HW_TIMER, (hw_metadata)(metadata.adc.trigger_metadata.timer));

    }

    /* Clear the interrupt status flag.  This is done to make sure the
       interrupt flag is cleared before we sample. */
    ADCIntClear(metadata.adc.base, metadata.adc.sample_sequence);
    IntEnable(INT_ADC0SS0 + metadata.adc.sample_sequence);
    /* ADCProcessorTrigger(metadata.adc.base, metadata.adc.sample_sequence); */
}
