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
#include "driverlib/rom.h"

#define HEARTBEAT_MODAL

#include "libos/os.h"
#include "libbutton/button.h"
#include "libheart/heartbeat.h"

#include <sys/stat.h>

uint32_t CountPF1 = 0; // number of times thread1 has looped
uint32_t CountPF2 = 0; // number of times thread2 has looped
uint32_t CountPF3 = 0; // number of times thread3 has looped

/*! A thread that continuously toggles GPIO pin 1 on GPIO_PORT_F. */
void Thread1(){
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_1);
    heart_toggle_();
    ++CountPF1;
    os_remove_thread(Thread1);
    while (1) {}
}

/*! A thread that continuously toggles GPIO pin 2 on GPIO_PORT_F. */
void postpone_suicide(void){
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_2);
    while (1) {
        heart_toggle_();
        ++CountPF2;
    }
}

int main() {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    IntMasterDisable();

    os_threading_init();
    /* os_add_thread(Thread1); */
    /* os_add_thread(Thread2); */
    /* os_add_thread(Thread3); */

    os_add_thread(postpone_suicide);

    button_metadata_init(GPIO_PORTF_BASE, BUTTONS_BOTH, GPIO_BOTH_EDGES);
    hw_init(HW_BUTTON, button_metadata);
    hw_subscribe(HW_BUTTON, button_metadata, Thread1);

    /* Load and enable the systick timer */
    SysTickPeriodSet(SysCtlClockGet() / 10);
    SysTickEnable();
    SysTickIntEnable();

    /* os_trap_ */
    os_launch();

    /* PONDER: why do interrupts fire without this? */
    IntMasterEnable();
    postpone_death();
}
