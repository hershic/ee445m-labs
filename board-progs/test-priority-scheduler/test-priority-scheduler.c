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
#include "libos/os.h"
#include "libheart/heartbeat.h"
#include "libos/semaphore.h"
#include "libtimer/timer.h"
#include "libsystick/systick.h"

uint32_t PIDWork = 0;
semaphore_t semaphore;
uint32_t interrupt_counter;

void timer_task() {
    TimerIntClear(TIMER0_BASE, TIMER_TIMA_TIMEOUT);
    sem_post(semaphore);
}

void Task1() {

    /* Consider: if I can trigger pendsv from an atomic block, will
     * the atomic block be restarted when the context resumes? */
    while(1) {
	/* defun: sem_wait */
	--semaphore;
	while (semaphore_blocked(semaphore)) {
	    /* defun: os_surrender_context */
	    /* TODO: examine in the context of this not switching to
	     * Task2 but instead running the loop endlessly */
	    IntPendSet(FAULT_PENDSV);
	    /* end_defun */
	}
	/* end_defun */
	heart_toggle();
    }
}

void Task2() {
    while(1) {
	++PIDWork;
    }
}

/* Why does this not trigger more than once? */
void TIMER0A_Handler(void) {

    TimerIntClear(TIMER0_BASE, TIMER_TIMA_TIMEOUT);

    /* the task */
    sem_signal(semaphore);
    /* we should see Task1 blink the heartbeat */

    ++interrupt_counter;
}

void main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
		   SYSCTL_XTAL_16MHZ);

    IntMasterDisable();

    heart_init();
    sem_init(semaphore);

    timer_metadata_init(TIMER0_BASE, 1 Hz, INT_TIMER0A, TIMER_CFG_PERIODIC);
    SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER0);
    TimerConfigure(timer_metadata.timer.base, timer_metadata.timer.periodic);
    TimerLoadSet(timer_metadata.timer.base, TIMER_A, SysCtlClockGet() / timer_metadata.timer.frequency);
    TimerIntEnable(timer_metadata.timer.base, TIMER_TIMA_TIMEOUT);
    IntEnable(timer_metadata.timer.interrupt);
    TimerEnable(timer_metadata.timer.base, TIMER_A);

    systick_init(10 Hz);

    os_threading_init();
    os_add_thread(Task1);
    os_add_thread(Task2);

    IntMasterEnable();
    os_launch();

    postpone_death();
}
