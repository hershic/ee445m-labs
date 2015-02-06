#include "adc.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include <stdbool.h>
#include <stdlib.h>

#include "driverlib/pin_map.h"
#include "driverlib/gpio.h"
#include "driverlib/sysctl.h"
#include "driverlib/adc.h"

#include "utils/ustdlib.h"

/* Initialize the ADC inputs used by the game pad device. This example
   uses the ADC pins on Port E pins 1, 2, and 3(AIN0-2). */
void adc_init(void) {
    int32_t ui32Chan;

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
    for(ui32Chan = 0; ui32Chan < 2; ui32Chan++) {
        /* Configure the sequence step */
        ADCSequenceStepConfigure(ADC0_BASE, 0, ui32Chan, ui32Chan);
    }

    ADCSequenceStepConfigure(ADC0_BASE, 0, 2, ADC_CTL_CH2 | ADC_CTL_IE |
                             ADC_CTL_END);
    /* Enable the sequence but do not start it yet. */
    ADCSequenceEnable(ADC0_BASE, 0);
}
