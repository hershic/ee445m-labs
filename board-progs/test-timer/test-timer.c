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
#include "driverlib/fpu.h"
#include "driverlib/timer.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"

#include "libtimer/timer.h"
#include "libos/semaphore.h"

#include <sys/stat.h>

#define HEARTBEAT_MODAL
#include "libheart/heartbeat.h"

sem_t sem;
uint32_t interrupt_counter;
uint32_t wait_counter;

muscle_t red_muscle;
muscle_t blue_muscle;

void TIMER0A_Handler(void) {

    /* IntMasterDisable(); */
    TimerIntClear(TIMER0_BASE, TIMER_TIMA_TIMEOUT);
    heart_toggle_modal(&red_muscle);
    ++interrupt_counter;
    sem_post(&sem);
    /* IntMasterEnable(); */
}

/*! Accept input on UART 0, and parrot input back out to UART 0.
 * \return Exit status
 */
int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    /* Enable processor interrupts. */
    IntMasterDisable();

    interrupt_counter = 0;
    wait_counter = 0;

    heart_init();

    sem_init(&sem, 0);

    timer_metadata_init(TIMER0_BASE, 2 Hz, INT_TIMER0A, TIMER_CFG_PERIODIC);

    SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER0);
    TimerConfigure(timer_metadata.timer.base, timer_metadata.timer.periodic);
    TimerLoadSet(timer_metadata.timer.base, TIMER_A, SysCtlClockGet() / timer_metadata.timer.frequency);
    TimerIntEnable(timer_metadata.timer.base, TIMER_TIMA_TIMEOUT);
    IntEnable(timer_metadata.timer.interrupt);
    TimerEnable(timer_metadata.timer.base, TIMER_A);

    IntMasterEnable();

    heart_hew_muscle(red_muscle, GPIO_PORTF_BASE, HEART_RED);
    heart_hew_muscle(blue_muscle, GPIO_PORTF_BASE, HEART_BLUE);

    while (1) {
        while(!sem) {
            heart_beat();
            heart_toggle_modal(&blue_muscle);
        }
        ++wait_counter;
    }
    /* postpone_death(); */
}
