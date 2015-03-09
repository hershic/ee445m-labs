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
#include "driverlib/rom.h"

#define HEARTBEAT_MODAL
#define SCHEDLUE_PRIORITY
#define SCHEDULER_MAX_THREADS 16

#include "libschedule/schedule.h"
#include "libos/os.h"
#include "libheart/heartbeat.h"
#include "libtimer/timer.h"

#define HEART_RED GPIO_PIN_1
#define HEART_BLUE GPIO_PIN_2
#define HEART_GREEN GPIO_PIN_3

volatile uint8_t start_on_this;

void led_blink_red() {
    while (1) {
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_1,
                     GPIO_PIN_1 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_1));
        os_surrender_context();
    }
}

void led_blink_blue() {
    while (1) {
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2,
                     GPIO_PIN_2 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2));
        os_surrender_context();
    }
}

void start_threading(notification note) {
    os_threading_init(10 Hz);
    schedule(led_blink_blue, 1 Hz, DL_SOFT);
    schedule(led_blink_red, 1 Hz, DL_SOFT);
    /* next test: different frequencies,pools */
    start_on_this = 1;
}

void main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    IntMasterDisable();

    heart_init();
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_1);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_2);

    os_threading_init(10 Hz);
    schedule(led_blink_blue, 1 Hz, DL_SOFT);
    schedule(led_blink_red, 1 Hz, DL_SOFT);

    /* timer_metadata_init(TIMER0_BASE, 10 Hz, INT_TIMER0A, TIMER_CFG_ONE_SHOT); */
    /* hw_init(HW_TIMER, timer_metadata); */
    /* hw_subscribe_single_shot(HW_TIMER, timer_metadata, start_threading); */
    IntMasterEnable();

    /* while (! start_on_this) { } */
    os_launch();

    postpone_death();
}
