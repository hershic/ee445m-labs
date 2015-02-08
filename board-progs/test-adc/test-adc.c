/* Created by Hershal Bhave and Eric Crosson on 2015-01-25 */
/* Revision History: Look in Git FGT */

/* Standard Libs */
#include <stdint.h>
#include <stdbool.h>

/* TI Includes */
#include "inc/hw_ints.h"
#include "inc/hw_memmap.h"

/* Driverlib Includes */
#include "driverlib/debug.h"
#include "driverlib/fpu.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/rom.h"

#include "libadc/adc.h"

#define ADC_DATA_BUFFER_LEN 3
static uint32_t adc_data_buffer[ADC_DATA_BUFFER_LEN];

int main(void) {

    FPUEnable();
    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    /* Enable the GPIO port that is used for the on-board LED. */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);

    /* Enable the GPIO pins for the LED (PF2). */
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_2);

    /* Enable the peripherals used by this example. */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_UART0);
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOA);

    SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER2);

    /* Enable processor interrupts. */
    IntMasterEnable();

    /* Activate the ADC on PE1, 2, and 3 (AIN0-2). */
    adc_init();

    /* Trigger an initial ADC sequence. As far as I know this is
       required for proper init. */
    ADCProcessorTrigger(ADC0_BASE, 0);

    adc_open(0);
    adc_collect(0, 10, &adc_data_buffer[0], 10, TIMER0);

    /* Do nothing */
    /* while (1) { */
    /*  */
    /*     if(ADCIntStatus(ADC0_BASE, 0, false) != 0) { */
    /*         /\* Clear the ADC interrupt. *\/ */
    /*         ADCIntClear(ADC0_BASE, 0); */
    /*  */
    /*         /\* Read the data and trigger a new sample request. *\/ */
    /*         ADCSequenceDataGet(ADC0_BASE, 0, &adc_data_buffer[0]); */
    /*         ADCProcessorTrigger(ADC0_BASE, 0); */
    /*  */
    /*         /\* TODO: Update our report of the data somehow (whatever */
    /*            means we define are necessary). For now the data */
    /*            resides in adc_data_buffer ready for copying and */
    /*            interpretation. *\/ */
    /*     } */
    /* } */
    while (1) {}

}
 
