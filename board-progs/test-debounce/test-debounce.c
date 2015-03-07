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
#include "driverlib/adc.h"

#define HEARTBEAT_MODAL

#include "libos/os.h"
#include "libos/jitter.h"
#include "libheart/heartbeat.h"
#include "libbutton/button.h"
#include "libtimer/timer.h"
#include "libhw/hardware.h"
#include "libos/semaphore.h"
#include "libdisplay/ST7735.h"
#include "libadc/adc.h"
#include "libuart/uart.h"
#include "libshell/shell.h"

volatile uint32_t button_left_pressed;
volatile uint32_t button_right_pressed;

volatile uint32_t button_debounced_mailbox;
volatile uint32_t button_debounced_wtf;

volatile uint32_t demo_adc_data[4];

volatile semaphore_t button_debounced_new_data;

int doctor() {

    /* uart_set_active_channel(UART0_BASE); */
    uart_send_string("Well what did you expect would happen? You're dreaming!\n");
    return EXIT_SUCCESS;
}

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

void display_adc_data_for_checkout(void) {
    uint8_t string_buf[5];
    uint8_t tmp;
    uint16_t i, j, k, l;
    uint32_t num;

    for (i=0; i<4; ++i) {
        num = ADC0_SEQ2_SAMPLES[i];
        for (j=0; j<4 && num > 0; ++j) {
            string_buf[4-j] = num % 10 + 0x30;
            num /= 10;
        }

        string_buf[j] = 0;

        for (k=0, l=4; k<l; ++k, --l) {
            ST7735_DrawCharS(40,l,(uint8_t) (string_buf[k] + 0x30), ST7735_YELLOW,ST7735_BLACK, 1);
            /* tmp = string_buf[k]; */
            /* string_buf[k] = string_buf[l]; */
            /* string_buf[l] = tmp; */
        }

        ST7735_DrawString(2, 2+i, string_buf, ST7735_YELLOW);
    }
}

void postpone_suicide() {

    ADC0_SEQ2_SAMPLES = demo_adc_data;

    while (1) {
        sem_wait(button_debounced_new_data);
        ST7735_DrawCharS(30,0,(uint8_t) (get_os_num_threads() + 0x30), ST7735_YELLOW,ST7735_BLACK, 1);

        if (~button_debounced_mailbox & BUTTON_LEFT) {
            GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, GPIO_PIN_2 ^
                         GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2));
            ST7735_DrawCharS(10,0,'1',ST7735_YELLOW,ST7735_BLACK, 1);
            display_adc_data_for_checkout();
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
    /* end button init */

    /* begin main os init */
    pidwork_init();

    os_threading_init();
    os_add_thread(postpone_suicide);
    os_add_thread(pidwork_record);
    os_add_thread(pidwork_increment);
    /* end main os init */

    /* begin shell init */
    hw_init_daemon();

    system_init();
    system_register_command((const char*) "doctor", doctor);

    /* Initialize hardware devices */
    uart_metadata_init(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
    hw_init(HW_UART, uart_metadata);

    /* Initialize the shell and the system it interacts with */
    shell_spawn();
    /* end shell init */

    /* begin heartbeat init */
    heart_init();
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_1);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_2);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_3);
    /* end heartbeat init */

    ST7735_InitR(INITR_REDTAB);

    /* Load and enable the systick timer */
    SysTickPeriodSet(SysCtlClockGet() / 1000);
    SysTickEnable();
    SysTickIntEnable();
    /* end */

    /* start adc init */
    hw_metadata metadata;
    metadata.adc.base = ADC0_BASE;
    metadata.adc.trigger_source = ADC_TRIGGER_TIMER;
    metadata.adc.sample_sequence = 2;
    metadata.adc.channel = 0;
    metadata.adc.channel_configuration = ADC_CTL_CH0;
    metadata.adc.trigger_metadata.timer.base = TIMER1_BASE;
    metadata.adc.trigger_metadata.timer.frequency = 2 Hz;
    metadata.adc.trigger_metadata.timer.interrupt = INT_TIMER1A;
    metadata.adc.trigger_metadata.timer.periodic = TIMER_CFG_PERIODIC;

    adc_init(metadata);
    adc_channel_init(metadata);

    metadata.adc.channel = 1;
    metadata.adc.channel_configuration = ADC_CTL_CH1;
    adc_channel_init(metadata);

    metadata.adc.channel = 2;
    metadata.adc.channel_configuration = ADC_CTL_CH2 | ADC_CTL_IE | ADC_CTL_END;
    adc_channel_init(metadata);

    adc_interrupt_init(metadata);
    /* end adc init */

    IntMasterEnable();

    /* hit the green button */
    os_launch();

    while (1) {}
}
