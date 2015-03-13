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

#define HEARTBEAT_MODAL
#define SCHEDLUE_PRIORITY
#define SCHEDULER_MAX_THREADS 16

#include "libbutton/button.h"
#include "libtimer/timer.h"
#include "libschedule/schedule.h"
#include "libos/os.h"
#include "libheart/heartbeat.h"

#define HEART_RED GPIO_PIN_1
#define HEART_BLUE GPIO_PIN_2
#define HEART_GREEN GPIO_PIN_3

volatile uint32_t button_left_pressed;
volatile uint32_t button_right_pressed;

volatile uint32_t button_debounced_mailbox;
volatile uint32_t button_debounced_wtf;

volatile semaphore_t button_debounced_new_data;

volatile uint32_t red_work = 0;
volatile uint32_t blue_work = 0;

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
                GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, GPIO_PIN_2 ^
                             GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2));
                ++button_left_pressed;
            }
            if (~button_debounced_mailbox & BUTTON_RIGHT) {
                GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_3, GPIO_PIN_3 ^
                             GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_3));
                ++button_right_pressed;
            }
        }
        /* os_surrender_context(); */
    }
}

void led_blink_red() {
    while (1) {
        ++red_work;
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_1,
                     GPIO_PIN_1 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_1));
        /* os_surrender_context(); */
    }
}

void led_blink_blue() {
    while (1) {
        ++blue_work;
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2,
                     GPIO_PIN_2 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2));
        /* os_surrender_context(); */
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

    /* begin timer init */
    timer_metadata_init(TIMER0_BASE, 10 Hz, INT_TIMER0A, TIMER_CFG_ONE_SHOT);
    hw_driver_init(HW_TIMER, timer_metadata);
    /* end timer init */

    /* button init */
    button_metadata_init(GPIO_PORTF_BASE, BUTTONS_BOTH, GPIO_BOTH_EDGES);

    hw_init(HW_BUTTON, button_metadata);
    hw_subscribe(HW_BUTTON, button_metadata, button_debounce_end);

    pidwork_init();

    os_threading_init();
    schedule(led_blink_red, 10 Hz, DL_SOFT);
    /* schedule(led_blink_green, 10 Hz, DL_SOFT); */
    schedule(led_blink_blue, 100 Hz, DL_SOFT);
    schedule(postpone_suicide, 100 Hz, DL_SOFT);
    /* next test: different frequencies,pools */

    IntMasterEnable();
    os_launch();

    postpone_death();
}
