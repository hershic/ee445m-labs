/* -*- mode: c; c-basic-offset: 4; -*- */

#include "timer.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

#include "driverlib/pin_map.h"
#include "driverlib/timer.h"
#include "driverlib/sysctl.h"

#include "driverlib/rom.h"

int timer_add_periodic_thread(void(*task)(void),
			      unsigned long frequency,
			      unsigned long priority,
			      utimer_t timer_peripheral) {

    unsigned long timer_base, timer_periph, timer_int;

    switch (timer_peripheral) {
    case TIMER0:
        timer_base = TIMER0_BASE;
        timer_periph = SYSCTL_PERIPH_TIMER0;
        timer_int = INT_TIMER0A;
        _task0 = task;
        break;
    case TIMER1:
        timer_base = TIMER1_BASE;
        timer_periph = SYSCTL_PERIPH_TIMER1;
        timer_int = INT_TIMER1A;
        _task1 = task;
        break;
    case TIMER2:
        timer_base = TIMER2_BASE;
        timer_periph = SYSCTL_PERIPH_TIMER2;
        timer_int = INT_TIMER2A;
        _task2 = task;
        break;
    default:
        /* you broke the world */
        return 1;
    }

    ROM_SysCtlPeripheralEnable(timer_periph);
    ROM_TimerConfigure(timer_base, TIMER_CFG_PERIODIC);
    ROM_TimerLoadSet(timer_base, TIMER_A, ROM_SysCtlClockGet() / frequency);

    ROM_TimerIntEnable(timer_base, TIMER_TIMA_TIMEOUT);
    ROM_IntEnable(timer_int);
    ROM_TimerEnable(timer_base, TIMER_A);

    /* Success */
    return 0;
}

void Timer0A_Handler(void) {
    ROM_TimerIntClear(TIMER0_BASE, TIMER_TIMA_TIMEOUT);
    _task0();
}

void Timer1A_Handler(void) {
    ROM_TimerIntClear(TIMER1_BASE, TIMER_TIMA_TIMEOUT);
    _task1();
}

void Timer2A_Handler(void) {
    ROM_TimerIntClear(TIMER2_BASE, TIMER_TIMA_TIMEOUT);
    _task2();
}


