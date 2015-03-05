/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson on 2015-01-24 */
/* Revision History: Look in Git FGT */

/* Standard Libs */
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/* TI Includes */
#include "inc/hw_ints.h"
#include "inc/hw_gpio.h"
#include "inc/hw_memmap.h"

/* Driverlib Includes */
#include "driverlib/debug.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/systick.h"
#include "driverlib/timer.h"

#define HEARTBEAT_MODAL

#include "libos/os.h"
#include "libos/jitter.h"
#include "libheart/heartbeat.h"
#include "libbutton/button.h"
#include "libtimer/timer.h"
#include "libhw/hardware.h"
#include "libos/semaphore.h"
#include "libdisplay/ST7735.h"

volatile uint32_t button_left_pressed;
volatile uint32_t button_right_pressed;

volatile uint32_t button_debounced_mailbox;
volatile uint32_t button_debounced_wtf;

volatile semaphore_t button_debounced_new_data;

volatile uint32_t* adc_sample_buffer;


void button_debounce_end(notification button_notification) {

    button_debounced_mailbox = GPIOPinRead(GPIO_PORTF_BASE, BUTTONS_BOTH);
    sem_post(button_debounced_new_data);
}

/* what the btn handler calls */
void button_debounce_start(notification button_notification) {

    button_debounced_wtf = GPIOPinRead(GPIO_PORTF_BASE, BUTTONS_BOTH);
    timer_metadata_init(TIMER0_BASE, 10 Hz, INT_TIMER0A, TIMER_CFG_ONE_SHOT);
    hw_init(HW_TIMER, timer_metadata);
    hw_subscribe_single_shot(HW_TIMER, timer_metadata,
                             button_debounce_end);
}

void adc_trigger_sample(notification timer_notification) {
    ADCSequenceDataGet(ADC0_BASE, 0, (int32_t*)(adc_sample_buffer[0]));
    ADCProcessorTrigger(ADC0_BASE, 0);
    ST7735_DrawCharS(60,0,(uint8_t) 'T', ST7735_YELLOW,ST7735_BLACK, 1);

}

void ADC0Seq0_Handler(void) {

    /* Clear the ADC interrupt. */
    ADCIntClear(ADC0_BASE, 0);

    /* Read the data and trigger a new sample request. */
    /* first is channel, second is beginning of buffer length */
    ADCSequenceDataGet(ADC0_BASE, 0, (int32_t*)(adc_sample_buffer[0]));
    /* ADCProcessorTrigger(ADC0_BASE, 0); */

    ST7735_DrawCharS(50,0,(uint8_t) 'H', ST7735_YELLOW,ST7735_BLACK, 1);

    /* TODO: Update our report of the data somehow (whatever
       means we define are necessary). For now the data
       resides in adc_data_buffer ready for copying and
       interpretation. */
}

void postpone_suicide() {

    while (1) {
        sem_wait(button_debounced_new_data);
        ST7735_DrawCharS(30,0,(uint8_t) (get_os_num_threads() + 0x30), ST7735_YELLOW,ST7735_BLACK, 1);

        if (~button_debounced_mailbox & BUTTON_LEFT) {
            GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, GPIO_PIN_2 ^
                         GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2));
            ST7735_DrawCharS(10,0,'1',ST7735_YELLOW,ST7735_BLACK, 1);
            ++button_left_pressed;
        } else { ST7735_DrawCharS(10,0,(uint8_t)'0',ST7735_YELLOW,ST7735_BLACK, 1); }

        if (~button_debounced_mailbox & BUTTON_RIGHT) {
            GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_3, GPIO_PIN_3 ^
                         GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_3));
            ST7735_DrawCharS(20,0,'1',ST7735_YELLOW,ST7735_BLACK, 1);
            ++button_right_pressed;
        } else { ST7735_DrawCharS(20,0,'0',ST7735_YELLOW,ST7735_BLACK, 1); }
    }
}

int main() {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    IntMasterDisable();

    /* button init */
    button_left_pressed = 0;
    button_right_pressed = 0;
    button_debounced_mailbox = 0;
    sem_init(button_debounced_new_data);

    button_metadata_init(GPIO_PORTF_BASE, BUTTONS_BOTH, GPIO_BOTH_EDGES);

    hw_init(HW_BUTTON, button_metadata);
    hw_subscribe(HW_BUTTON, button_metadata, button_debounce_start);

    pidwork_init();

    os_threading_init();
    os_add_thread(postpone_suicide);
    os_add_thread(pidwork_record);
    os_add_thread(pidwork_increment);

    heart_init();
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_1);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_2);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_3);

    ST7735_InitR(INITR_REDTAB);

    timer_metadata_init(TIMER0_BASE, 10 Hz, INT_TIMER1A, TIMER_CFG_PERIODIC);
    hw_init(HW_TIMER, timer_metadata);
    hw_subscribe_single_shot(HW_TIMER, timer_metadata,
                             adc_trigger_sample);

    /* Load and enable the systick timer */
    SysTickPeriodSet(SysCtlClockGet() / 1000);
    SysTickEnable();
    SysTickIntEnable();

    IntMasterEnable();

    os_launch();

    while (1) {}
}
