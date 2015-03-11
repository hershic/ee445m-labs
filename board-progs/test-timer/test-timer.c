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
#include "inc/hw_memmap.h"

/* Driverlib Includes */
#include "driverlib/debug.h"
#include "driverlib/gpio.h"
#include "driverlib/timer.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"

#include "libtimer/timer.h"
#include "libos/semaphore.h"

#include <sys/stat.h>

#define HEARTBEAT_MODAL
#include "libheart/heartbeat.h"

volatile uint32_t sem;
uint32_t interrupt_counter;
uint32_t wait_counter;

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
    /* while (1) { */
        ++green_work;
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_3,
                     GPIO_PIN_3 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_3));
        /* os_surrender_context(); */
    /* } */
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

    /* begin timer init */
    /* timer_metadata_init(TIMER0_BASE, 10 Hz, INT_TIMER0A, TIMER_CFG_ONE_SHOT); */
    timer_metadata_init(TIMER0_BASE, 2 Hz, INT_TIMER0A, TIMER_CFG_PERIODIC);
    hw_driver_init(HW_TIMER, timer_metadata);
    hw_channel_init(HW_TIMER, timer_metadata);
    hw_subscribe(HW_TIMER, timer_metadata, led_blink_green);
    /* end timer init */

    os_threading_init(1000 Hz);
    schedule(led_blink_red, 100 Hz, DL_SOFT);
    schedule(led_blink_blue, 100 Hz, DL_SOFT);
    /* schedule(postpone_suicide, 100 Hz, DL_SOFT); */

    IntMasterEnable();
    os_launch();

    while (1) { }
    /* postpone_death(); */
}
