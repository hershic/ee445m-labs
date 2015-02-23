#include "adc.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include "driverlib/pin_map.h"
#include "driverlib/gpio.h"
#include "driverlib/sysctl.h"
#include "driverlib/adc.h"

#define MAX_NUM_SAMPLES 10
#define NUM_ADC_CHANNELS 11

/* global buffers */
uint32_t adc_sample_buffer[NUM_ADC_CHANNELS];
uint32_t adc_max_set_samples_per_channel[NUM_ADC_CHANNELS];
uint32_t adc_current_samples_per_channel[NUM_ADC_CHANNELS];
uint8_t adc_active_scoreboard[NUM_ADC_CHANNELS];

/* TODO: Need to be able to init the adc for any combination of ports and pins */
void adc_init(void) {
    /* int32_t ui32Chan; */
    uint8_t i;

    /* Enable the GPIOs and the ADC used by this example. */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOE);
    SysCtlGPIOAHBEnable(SYSCTL_PERIPH_GPIOE);

    SysCtlPeripheralEnable(SYSCTL_PERIPH_ADC0);
    SysCtlPeripheralReset(SYSCTL_PERIPH_ADC0);

    /* Select the external reference for greatest accuracy. */
    ADCReferenceSet(ADC0_BASE, ADC_REF_EXT_3V);

    /* Configure the pins which are used as analog inputs. */
    GPIOPinTypeADC(GPIO_PORTE_AHB_BASE, GPIO_PIN_3 | GPIO_PIN_2 | GPIO_PIN_1);

    /* Configure the sequencer for 3 steps. */
    for(i = 0; i < 2; ++i) {
        /* Configure the sequence step */
        ADCSequenceStepConfigure(ADC0_BASE, 0, i, i);
    }

    ADCSequenceStepConfigure(ADC0_BASE, 0, 2, ADC_CTL_CH2 | ADC_CTL_IE |
                             ADC_CTL_END);
    /* Enable the sequence but do not start it yet. */
    ADCSequenceEnable(ADC0_BASE, 0);

    for (i=0; i<NUM_ADC_CHANNELS; ++i) {
        adc_active_scoreboard[i] = false;
    }
}

uint32_t adc_open(uint32_t channel) {
    uint32_t encoded_channel;
    if (encode_adc_channel(channel, &encoded_channel)) {
        ADCSequenceConfigure(ADC0_BASE, 3, ADC_TRIGGER_PROCESSOR, 0);
        ADCSequenceStepConfigure(ADC0_BASE, 3, 0, encoded_channel | ADC_CTL_IE | ADC_CTL_END);
        ADCSequenceEnable(ADC0_BASE, 3);
        ADCIntClear(ADC0_BASE, 3);
        return 1;
    }
    return 0;
}

bool encode_adc_channel(uint32_t channel, uint32_t* encoded_channel) {
    switch (channel) {
    case 0:
        *encoded_channel = ADC_CTL_CH0;
        break;
    case 1:
        *encoded_channel = ADC_CTL_CH1;
        break;
    case 2:
        *encoded_channel = ADC_CTL_CH2;
        break;
    case 3:
        *encoded_channel = ADC_CTL_CH3;
        break;
    case 4:
        *encoded_channel = ADC_CTL_CH4;
        break;
    case 5:
        *encoded_channel = ADC_CTL_CH5;
        break;
    case 6:
        *encoded_channel = ADC_CTL_CH6;
        break;
    case 7:
        *encoded_channel = ADC_CTL_CH7;
        break;
    case 8:
        *encoded_channel = ADC_CTL_CH8;
        break;
    case 9:
        *encoded_channel = ADC_CTL_CH9;
        break;
    case 10:
        *encoded_channel = ADC_CTL_CH10;
        break;
    case 11:
        *encoded_channel = ADC_CTL_CH11;
        break;
    default:
        return false;
    }
    return true;
}

uint32_t adc_collect(uint32_t channel, uint32_t frequency,
                     unsigned long buffer[], uint32_t timer_peripheral) {
    uint32_t encoded_channel;
    if (encode_adc_channel(channel, &encoded_channel)) {

        adc_sample_buffer[channel] = (int32_t)buffer;

        /* low priority */
        /* timer_add_periodic_thread(do_adc_func, frequency, timer_peripheral); */
        return 1;
    }
    return 0;
}

/* TODO: Need to be able to populate many subscribers' data into their respective arrays */
void do_adc_func() {

    uint32_t status = ADCIntStatus(ADC0_BASE, 0, false);
    if (status != 0) {

        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2) ^ GPIO_PIN_2);
        /* Clear the ADC interrupt. */
        ADCIntClear(ADC0_BASE, 0);

        /* Read the data and trigger a new sample request. */
        /* ADCSequenceDataGet(ADC0_BASE, 0, &adc_channel_buffer[0]); */
        ADCSequenceDataGet(ADC0_BASE, 0, &adc_sample_buffer[0]);
        ADCProcessorTrigger(ADC0_BASE, 0);

        /* TODO: Update our report of the data somehow (whatever
           means we define are necessary). For now the data
           resides in adc_data_buffer ready for copying and
           interpretation. */
    }
}

void ADC0Seq0_Handler(void) {

    /* Clear the ADC interrupt. */
    ADCIntClear(ADC0_BASE, 0);

    /* Read the data and trigger a new sample request. */
    /* first is channel, second is beginning of buffer length */
    ADCSequenceDataGet(ADC0_BASE, 0, (int32_t*)(adc_sample_buffer[0]));
    /* ADCProcessorTrigger(ADC0_BASE, 0); */

    GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2) ^ GPIO_PIN_2);

    /* TODO: Update our report of the data somehow (whatever
       means we define are necessary). For now the data
       resides in adc_data_buffer ready for copying and
       interpretation. */
}
