/* -*- mode: c; c-basic-offset: 4; -*- */

#include "timer.h"
#include "libhw/hardware.h"
#include "libnotify/notify.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include <stdlib.h>
#include <stdint.h>

#include "driverlib/pin_map.h"
#include "driverlib/timer.h"
#include "driverlib/sysctl.h"

bool timer_add_periodic_interrupt(uint32_t frequency,
                                  uint32_t timer_peripheral) {

    uint32_t timer_base, timer_periph, timer_int;

    /* can give it a raw timer address (TIMERx_BASE) or an int with
       the timer number (0, 1, 2) for convenience */
    switch (timer_peripheral) {
    case TIMER0_BASE:
        timer_base = TIMER0_BASE;
        timer_periph = SYSCTL_PERIPH_TIMER0;
        timer_int = INT_TIMER0A;
        break;
    case TIMER1_BASE:
        timer_base = TIMER1_BASE;
        timer_periph = SYSCTL_PERIPH_TIMER1;
        timer_int = INT_TIMER1A;
        break;
    case TIMER2_BASE:
        timer_base = TIMER2_BASE;
        timer_periph = SYSCTL_PERIPH_TIMER2;
        timer_int = INT_TIMER2A;
        break;
    default:
        /* you broke the world */
        return false;
    }

    SysCtlPeripheralEnable(timer_periph);
    TimerConfigure(timer_base, TIMER_CFG_PERIODIC);
    TimerLoadSet(timer_base, TIMER_A, SysCtlClockGet() / frequency);

    TimerIntEnable(timer_base, TIMER_TIMA_TIMEOUT);

    /* TODO: Fill in priority */
    IntEnable(timer_int, 0);
    TimerEnable(timer_base, TIMER_A);

    /* Success */
    return true;
}

bool timer_remove_periodic_thread(const void* task,
                              uint32_t timer_peripheral) {
    hw_disconnect(HW_TIMER, timer_peripheral, task);
}

