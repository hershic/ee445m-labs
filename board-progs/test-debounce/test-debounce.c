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
#include "libheart/heartbeat.h"
#include "libbutton/button.h"
#include "libtimer/timer.h"
#include "libhw/hardware.h"
#include "libos/semaphore.h"

volatile uint32_t button_left_pressed;
volatile uint32_t button_right_pressed;

volatile uint32_t button_debounced_mailbox;
volatile uint32_t button_debounced_wtf;

volatile sem_t button_debounced_new_data;

volatile uint32_t pidwork;
volatile uint32_t lowest_pidwork;
volatile uint32_t highest_pidwork;

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

void Thread1(void){

    while(1) {
        asm volatile("CPSID  I");
        ++pidwork;
        asm volatile("CPSIE  I");
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_1, GPIO_PIN_1);
    }
}

/*! A thread that continuously toggles GPIO pin 2 on GPIO_PORT_F. */
void Thread2(void){

    while(1) {
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_1, 0);
        if (pidwork != 0) {
            if (highest_pidwork < pidwork) {
                highest_pidwork = pidwork;
            }
            if (lowest_pidwork > pidwork) {
                lowest_pidwork = pidwork;
            }
        }
        pidwork = 0;
    }
}

void postpone_suicide() {

    while (1) {
        sem_wait(button_debounced_new_data);
        if (~button_debounced_mailbox & BUTTON_LEFT) {
            GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, GPIO_PIN_2 ^
                         GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2));
        }
        if (~button_debounced_mailbox & BUTTON_RIGHT) {
            GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_3, GPIO_PIN_3 ^
                         GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_3));
        }
    }
}

int main() {

    button_left_pressed = 0;
    button_right_pressed = 0;

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    IntMasterDisable();

    /* button init */
    button_metadata_init(GPIO_PORTF_BASE, BUTTONS_BOTH, GPIO_BOTH_EDGES);

    hw_init(HW_BUTTON, button_metadata);
    hw_subscribe(HW_BUTTON, button_metadata, button_debounce_start);

    pidwork = 0;
    highest_pidwork = 0;
    lowest_pidwork = (uint32_t)(-1);

    os_threading_init();
    os_add_thread(postpone_suicide);
    os_add_thread(Thread1);
    os_add_thread(Thread2);

    heart_init();
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_1);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_2);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_3);

    /* Load and enable the systick timer */
    SysTickPeriodSet(SysCtlClockGet() / 1000);
    SysTickEnable();
    SysTickIntEnable();

    IntMasterEnable();

    os_launch();

    while (1) {}
}
