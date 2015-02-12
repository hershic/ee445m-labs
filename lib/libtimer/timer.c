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

bool timer_add_periodic_thread(const void* task,
                              uint32_t frequency,
                              uint32_t priority,
                              utimer_t timer_peripheral) {

    unsigned long timer_base, timer_periph, timer_int;

    switch (timer_peripheral) {
    case TIMER0:
        timer_base = TIMER0_BASE;
        timer_periph = SYSCTL_PERIPH_TIMER0;
        timer_int = INT_TIMER0A;
        break;
    case TIMER1:
        timer_base = TIMER1_BASE;
        timer_periph = SYSCTL_PERIPH_TIMER1;
        timer_int = INT_TIMER1A;
        break;
    case TIMER2:
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
    IntEnable(timer_int, priority);
    TimerEnable(timer_base, TIMER_A);

    hw_connect(HW_TIMER, (uint32_t)(timer_peripheral), task);

    /* Success */
    return true;
}

bool timer_remove_periodic_thread(const void* task,
                              utimer_t timer_peripheral) {
    hw_disconnect(HW_TIMER, (uint32_t)timer_peripheral, task);
}

void Timer0A_Handler(void) {
    TimerIntClear(TIMER0_BASE, TIMER_TIMA_TIMEOUT);
    hw_notification notification;
    notification._int = 1;
    hw_notify(HW_TIMER, 0, notification);
}

void Timer1A_Handler(void) {
    TimerIntClear(TIMER1_BASE, TIMER_TIMA_TIMEOUT);
    hw_notification notification;
    notification._int = 1;
   hw_notify(HW_TIMER, 1, notification);
}

void Timer2A_Handler(void) {
    TimerIntClear(TIMER2_BASE, TIMER_TIMA_TIMEOUT);
    hw_notification notification;
    notification._int = 1;
    hw_notify(HW_TIMER, 2, notification);
}
