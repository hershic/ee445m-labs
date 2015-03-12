/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Eric Crosson on 2015-02-20 */
/* Revision History: Look in Git FGT */

/* Standard Libs */
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/* TI Includes */
#include "inc/hw_ints.h"
#include "inc/hw_memmap.h"

/* Driverlib Includes */
#include "driverlib/debug.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/systick.h"
#include "driverlib/adc.h"

#define HEARTBEAT_MODAL
#define SCHEDLUE_PRIORITY
#define SCHEDULER_MAX_THREADS 16

#include "libbutton/button.h"
#include "libtimer/timer.h"
#include "libschedule/schedule.h"
#include "libos/os.h"
#include "libheart/heartbeat.h"
#include "libadc/adc.h"
#include "libuart/uart.h"
#include "libdisplay/ST7735.h"

#define HEART_RED GPIO_PIN_1
#define HEART_BLUE GPIO_PIN_2
#define HEART_GREEN GPIO_PIN_3

volatile uint32_t button_left_pressed;
volatile uint32_t button_right_pressed;

volatile uint32_t button_debounced_mailbox;
volatile uint32_t button_debounced_wtf;

volatile semaphore_t button_debounced_new_data;

extern semaphore_t HW_ADC_SEQ2_SEM;
extern uint32_t ADC0_SEQ2_SAMPLES[4];

volatile uint32_t red_work = 0;
volatile uint32_t green_work = 0;
volatile uint32_t blue_work = 0;
volatile uint32_t red_single_work = 0;
volatile uint32_t green_single_work = 0;
volatile uint32_t blue_single_work = 0;

void button_debounce_end(notification button_notification) {

    button_debounced_mailbox = GPIOPinRead(GPIO_PORTF_BASE, BUTTONS_BOTH);
    sem_post(button_debounced_new_data);
}

/* what the btn handler calls */
void button_debounce_start(notification button_notification) {

    button_debounced_wtf = GPIOPinRead(GPIO_PORTF_BASE, BUTTONS_BOTH);
    timer_metadata_init(TIMER0_BASE, 10 Hz, INT_TIMER0A, TIMER_CFG_ONE_SHOT);
    hw_channel_init(HW_TIMER, timer_metadata);
    hw_subscribe_single_shot(HW_TIMER, timer_metadata,
                             button_debounce_end);
}

void postpone_suicide() {

    while (1) {
        sem_guard(button_debounced_new_data) {
            sem_take(button_debounced_new_data);
            if (~button_debounced_mailbox & BUTTON_LEFT) {
                led_single_blink_green();
                ++button_left_pressed;
            }
            if (~button_debounced_mailbox & BUTTON_RIGHT) {
                led_single_blink_green();
                ++button_right_pressed;
            }
        }
        os_surrender_context();
    }
}

void led_single_blink_red() {
    ++red_single_work;
    GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_1,
                 GPIO_PIN_1 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_1));
}

void led_single_blink_green() {
    ++green_single_work;
    GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_3,
                 GPIO_PIN_3 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_3));
}

void led_single_blink_blue() {
    ++blue_single_work;
    GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2,
                 GPIO_PIN_2 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2));
}


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
        ++green_work;
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_3,
                     GPIO_PIN_3 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_3));
        os_surrender_context();
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

int doctor() {

    /* uart_set_active_channel(UART0_BASE); */
    UARTCharPut(UART0_BASE, 'd');
    /* uart_send_string("Well what did you expect would happen? You're dreaming!\n"); */
    return EXIT_SUCCESS;
}

void display_adc_data_for_checkout() {

    while (1) {
        sem_guard(HW_ADC_SEQ2_SEM) {
            sem_take(HW_ADC_SEQ2_SEM);
            uint8_t string_buf[5];
            uint8_t tmp;
            uint16_t i, j, k, l;
            uint32_t num;

            for (i=0; i<4; ++i) {
                num = ADC0_SEQ2_SAMPLES[i];
                for (j=0; j<4 && num > 0; ++j) {
                    string_buf[3-j] = (num % 10) + 0x30;
                    num /= 10;
                }

                string_buf[j] = 0;

                ST7735_DrawString(2, 2+i, string_buf, ST7735_YELLOW);
            }
        }
        os_surrender_context();
    }
}

void main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    IntMasterDisable();

    heart_init();
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_1);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_2);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_3);

    /* begin display init */
    Output_On();
    /* end display init */

    /* begin timer init */
    timer_metadata_init(TIMER0_BASE, 10 Hz, INT_TIMER0A, TIMER_CFG_ONE_SHOT);
    hw_driver_init(HW_TIMER, timer_metadata);
    /* end timer init */

    /* button init */
    button_metadata_init(GPIO_PORTF_BASE, BUTTONS_BOTH, GPIO_BOTH_EDGES);

    hw_init(HW_BUTTON, button_metadata);
    hw_subscribe(HW_BUTTON, button_metadata, button_debounce_end);

    pidwork_init();

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
    /* adc_data_buffer = ADC0_SEQ2_SAMPLES; */
    /* end adc init */

    /* begin shell init */
    system_init();
    system_register_command((const char*) "doctor", doctor);
    uart_metadata_init(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
    hw_init(HW_UART, uart_metadata);

    /* Initialize the shell and the system it interacts with */
    shell_spawn();
    /* end shell init */

    os_threading_init(1000 Hz);
    schedule(led_blink_red, 100 Hz, DL_SOFT);
    schedule(led_blink_blue, 100 Hz, DL_SOFT);
    schedule(hw_daemon, 100 Hz, DL_SOFT);
    schedule(postpone_suicide, 100 Hz, DL_SOFT);
    schedule(display_adc_data_for_checkout, 100 Hz, DL_SOFT);
    /* next test: different frequencies,pools */

    IntMasterEnable();
    os_launch();

    postpone_death();
}
