#include "adcpp.hpp"

#include "driverlib/adc.h"
#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"

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
