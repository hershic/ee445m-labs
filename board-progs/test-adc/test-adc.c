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

#define HEARTBEAT_MODAL

#include "libheart/heartbeat.h"

extern semaphore_t HW_ADC_SEQ2_SEM;
extern uint32_t ADC0_SEQ2_SAMPLES[4];
uint32_t *adc_data_buffer;
uint32_t red_work = 0;
uint32_t blue_work = 0;
uint32_t green_work = 0;

void led_blink_red() {
    while (1) {
        ++red_work;
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_1,
                     GPIO_PIN_1 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_1));
        os_surrender_context();
    }
}

void led_blink_green() {
    while (1) {
        sem_guard(HW_ADC_SEQ2_SEM) {
            sem_take(HW_ADC_SEQ2_SEM);
            ++green_work;
            GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_3,
                         GPIO_PIN_3 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_3));
            os_surrender_context();
        }
    }
}

void led_blink_blue() {
    while (1) {
        ++blue_work;
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2,
                     GPIO_PIN_2 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2));
        os_surrender_context();
    }
}

int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    /* Enable processor interrupts. */
    IntMasterDisable();

    heart_init();
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_1);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_2);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_3);

    /* Activate the ADC on PE1, 2, and 3 (AIN0-2). */
    /* start adc init */
    hw_metadata metadata;
    metadata.adc.base = ADC0_BASE;
    metadata.adc.trigger_source = ADC_TRIGGER_TIMER;
    metadata.adc.sample_sequence = 2;
    metadata.adc.channel = 0;
    metadata.adc.channel_configuration =
        ADC_CTL_CH0 | ADC_CTL_IE | ADC_CTL_END;
    metadata.adc.trigger_metadata.timer.base = TIMER1_BASE;
    metadata.adc.trigger_metadata.timer.frequency = 2 Hz;
    metadata.adc.trigger_metadata.timer.interrupt = INT_TIMER1A;
    metadata.adc.trigger_metadata.timer.periodic = TIMER_CFG_PERIODIC;

    adc_init(metadata);
    adc_channel_init(metadata);
    adc_interrupt_init(metadata);

    adc_data_buffer = ADC0_SEQ2_SAMPLES;
    /* end adc init */

    os_threading_init();
    schedule(led_blink_red, 100 Hz, DL_SOFT);
    schedule(led_blink_blue, 100 Hz, DL_SOFT);
    schedule(led_blink_green, 100 Hz, DL_SOFT);

    IntMasterEnable();
    os_launch();

    while (1) {

    }
}
