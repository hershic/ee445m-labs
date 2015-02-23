/* -*- mode: c; c-basic-offset: 4; -*- */

#include "timer.h"
#include "libhw/hardware.h"
#include "libnotify/notify.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include <stdlib.h>

#include "driverlib/pin_map.h"
#include "driverlib/timer.h"
#include "driverlib/sysctl.h"

bool timer_add_periodic_interrupt(hw_metadata metadata) {

    uint32_t timer_base, timer_periph, timer_int;
    uint32_t meta_frequency, meta_peripheral;

    meta_frequency = metadata.timer.frequency;
    meta_peripheral = metadata.timer.base;

    /* can give it a raw timer address (TIMERx_BASE) or an int with
       the timer number (0, 1, 2) for convenience */
    switch (meta_peripheral) {
    case 0:
    case TIMER0_BASE:
        timer_base = TIMER0_BASE;
        timer_periph = SYSCTL_PERIPH_TIMER0;
        timer_int = INT_TIMER0A;
        break;
    case 1:
    case TIMER1_BASE:
        timer_base = TIMER1_BASE;
        timer_periph = SYSCTL_PERIPH_TIMER1;
        timer_int = INT_TIMER1A;
        break;
    case 2:
    case TIMER2_BASE:
        timer_base = TIMER2_BASE;
        timer_periph = SYSCTL_PERIPH_TIMER2;
        timer_int = INT_TIMER2A;
        break;
    default:
        /* you broke the world */
	postpone_death();	/* for debugging only */
        return false;
    }

    SysCtlPeripheralEnable(timer_periph);
    TimerConfigure(timer_base, TIMER_CFG_PERIODIC);
    TimerLoadSet(timer_base, TIMER_A, SysCtlClockGet() / meta_frequency);

    TimerIntEnable(timer_base, TIMER_TIMA_TIMEOUT);

    /* TODO: Fill in priority */
    IntEnable(timer_int, 0);
    TimerEnable(timer_base, TIMER_A);

    /* Success */
    return true;
}
