/* -*- mode: c; c-basic-offset: 4; -*- */
#include "adc.h"

#include "libhw/hardware.h"
#include "libnotify/notify.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include "driverlib/pin_map.h"
#include "driverlib/gpio.h"
#include "driverlib/sysctl.h"
#include "driverlib/adc.h"
#include "driverlib/timer.h"

#define MAX_NUM_SAMPLES 10
#define NUM_ADC_CHANNELS 11

/* global buffers */
uint32_t* adc_sample_buffer;

/* TODO: Need to be able to init the adc for any combination of ports
   and pins */
void adc_init(hw_metadata metadata) {
    uint8_t i;

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
    /* ADCReferenceSet(metadata.adc.base, ADC_REF_INT); */
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
    ADCIntClear(metadata.adc.base, 3);
    IntEnable(INT_ADC0SS3);
    /* ADCProcessorTrigger(metadata.adc.base, metadata.adc.sample_sequence); */
}

uint32_t adc_open(uint32_t channel) {
    uint32_t encoded_channel;
    if (encode_adc_channel(channel, &encoded_channel)) {
        ADCSequenceConfigure(ADC0_BASE, 3, ADC_TRIGGER_PROCESSOR, 0);
        ADCSequenceStepConfigure(ADC0_BASE, 3, 0,
                                 encoded_channel | ADC_CTL_IE | ADC_CTL_END);
        ADCSequenceEnable(ADC0_BASE, 3);
        ADCIntClear(ADC0_BASE, 3);
        return 1;
    }
    return 0;
}

bool encode_adc_channel(uint32_t channel, uint32_t* encoded_channel) {
    switch (channel) {
    case 0: *encoded_channel = ADC_CTL_CH0; break;
    case 1: *encoded_channel = ADC_CTL_CH1; break;
    case 2: *encoded_channel = ADC_CTL_CH2; break;
    case 3: *encoded_channel = ADC_CTL_CH3; break;
    case 4: *encoded_channel = ADC_CTL_CH4; break;
    case 5: *encoded_channel = ADC_CTL_CH5; break;
    case 6: *encoded_channel = ADC_CTL_CH6; break;
    case 7: *encoded_channel = ADC_CTL_CH7; break;
    case 8: *encoded_channel = ADC_CTL_CH8; break;
    case 9: *encoded_channel = ADC_CTL_CH9; break;
    case 10: *encoded_channel = ADC_CTL_CH10; break;
    case 11: *encoded_channel = ADC_CTL_CH11; break;
    default: while (1) {}
    }
    return true;
}

uint32_t adc_collect(uint32_t channel, uint32_t frequency,
                     uint32_t buffer[], uint32_t timer_peripheral) {
    uint32_t encoded_channel;
    if (encode_adc_channel(channel, &encoded_channel)) {

        adc_sample_buffer = buffer;

        /* low priority */
        /* timer_add_periodic_thread(do_adc_func, frequency, timer_peripheral); */
        while (!do_adc_func()) {}
        return 1;
    }
    return 0;
}

/* TODO: Need to be able to populate many subscribers' data into their respective arrays */
uint32_t do_adc_func() {

    ADCProcessorTrigger(ADC0_BASE, 0);
    uint32_t status = ADCIntStatus(ADC0_BASE, 0, false);
    if (status != 0) {

        /* Clear the ADC interrupt. */
        ADCIntClear(ADC0_BASE, 0);

        /* Read the data and trigger a new sample request. */
        /* ADCSequenceDataGet(ADC0_BASE, 0, &adc_channel_buffer[0]); */
        ADCSequenceDataGet (ADC0_BASE, 0, adc_sample_buffer);
        ADCProcessorTrigger(ADC0_BASE, 0);

        /* TODO: Update our report of the data somehow (whatever
           means we define are necessary). For now the data
           resides in adc_data_buffer ready for copying and
           interpretation. */
    }
    return status;
}

