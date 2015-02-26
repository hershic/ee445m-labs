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

muscle_t red_muscle;
muscle_t blue_muscle;

void (*timer_task)(void);   // user function

void timer_set_task(void (*task)(void)) {
    timer_task = task;
}

void timer_release() {
    sem = 0xdeadfeed;
    /* sem_post(&sem); */
}

void TIMER0A_Handler(void) {

    TimerIntClear(TIMER0_BASE, TIMER_TIMA_TIMEOUT);
    (*timer_task)();

    /* doesn't work */
    /* heart_toggle_modal(&red_muscle); */

    GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_1,
                 GPIO_PIN_1 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_1));

    ++interrupt_counter;
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

    /* sem_init(&sem, 0); */
    sem = false;

    timer_set_task(&timer_release);

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
    heart_init_(red_muscle.base, red_muscle.pin);
    heart_init_(blue_muscle.base, blue_muscle.pin);

    while (1) {
        while (!sem) {}
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2,
                     GPIO_PIN_2 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2));

        /* heart_toggle_modal(&blue_muscle); */
        ++wait_counter;
        sem = 0;
    }
    /* postpone_death(); */
}
