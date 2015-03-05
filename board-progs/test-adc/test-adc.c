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
#include "driverlib/adc.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/rom.h"

#include "libadc/adc.h"
#include "libstd/nexus.h"
#include "libheart/heartbeat.h"

#define ADC_DATA_BUFFER_LEN 3
static uint32_t adc_data_buffer[ADC_DATA_BUFFER_LEN];

int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    heart_init();

    /* Enable TIMER2 (we are using) */
    /* why is this here? */
    /* SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER); */

    /* Enable processor interrupts. */

    /* Activate the ADC on PE1, 2, and 3 (AIN0-2). */
    hw_metadata metadata;
    metadata.adc.base = ADC0_BASE;
    metadata.adc.trigger_source = ADC_TRIGGER_TIMER;
    metadata.adc.sample_sequence = 3;
    metadata.adc.channel = 0;
    metadata.adc.channel_configuration =
        ADC_CTL_CH0 | ADC_CTL_IE | ADC_CTL_END;
    metadata.adc.trigger_metadata.timer.base = TIMER1_BASE;
    metadata.adc.trigger_metadata.timer.frequency = 2000 Hz;
    metadata.adc.trigger_metadata.timer.interrupt = INT_TIMER1A;
    metadata.adc.trigger_metadata.timer.periodic = TIMER_CFG_PERIODIC;

    adc_init(metadata);
    adc_channel_init(metadata);
    adc_interrupt_init(metadata);

    /* adc_open(0); */
    IntMasterEnable();

    /* Trigger an initial ADC sequence. As far as I know this is
       required for proper init. */
    while (1) { }

}
