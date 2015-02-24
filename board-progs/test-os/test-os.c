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
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/systick.h"

#define HEARTBEAT_MODAL

#include "libos/os.h"
#include "libheart/heartbeat.h"
#include "libbutton/button.h"

uint32_t CountPF1 = 0; // number of times thread1 has looped
uint32_t CountPF2 = 0; // number of times thread2 has looped
uint32_t CountPF3 = 0; // number of times thread3 has looped

uint32_t which_led = 0;

muscle_t pf1;
muscle_t pf2;
muscle_t pf3;

int32_t atom;

/*! A thread that continuously toggles GPIO pin 1 on GPIO_PORT_F. */
void Thread1(void){
    heart_toggle_modal(&pf1);
    ++CountPF1;

    os_remove_thread_and_switch(Thread1);
}

/*! A thread that continuously toggles GPIO pin 2 on GPIO_PORT_F. */
void Thread2(void){
    heart_toggle_modal(&pf2);
    ++CountPF2;

    os_remove_thread_and_switch(Thread2);
}

/*! A thread that continuously toggles GPIO pin 3 on GPIO_PORT_F. */
void Thread3(void){
    heart_toggle_modal(&pf3);
    ++CountPF3;

    os_remove_thread_and_switch(Thread3);
}

void flash_some_led(notification button_bitmask) {
    void* task;

    switch(which_led) {
    case 0:
        task = Thread1;
        break;
    case 1:
        task = Thread2;
        break;
    case 2:
        task = Thread3;
        break;
    default:
        task = Thread1;
        break;
    }

    which_led = which_led+1 % 3;
    atom = StartCritical();
    os_add_thread(task);
    EndCritical(atom);
}

void postpone_death_() {
    while(1) {
    }
}

int main() {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    IntMasterDisable();

    /* Load and enable the systick timer */
    SysTickPeriodSet(SysCtlClockGet() / 10);
    SysTickEnable();
    SysTickIntEnable();

    button_metadata_init(GPIO_PORTF_BASE, BUTTONS_BOTH, GPIO_BOTH_EDGES);
    hw_init(HW_BUTTON, button_metadata);
    hw_subscribe(HW_BUTTON, button_metadata, flash_some_led);

    heart_hew_muscle_(&pf1, GPIO_PORTF_BASE, GPIO_PIN_1);
    heart_hew_muscle_(&pf2, GPIO_PORTF_BASE, GPIO_PIN_2);
    heart_hew_muscle_(&pf3, GPIO_PORTF_BASE, GPIO_PIN_3);

    os_threading_init();

    atom = StartCritical();
    os_add_thread(postpone_death_);
    EndCritical(atom);

    /* os_trap_ */
    IntMasterEnable();
    os_launch();

    /* PONDER: why do interrupts fire without this? */
    postpone_death();
}

