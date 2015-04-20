#include "timerpp.hpp"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"
#include "driverlib/interrupt.h"

timer::timer() {}

timer::timer(timer_t timer_id, subtimer_t timer_subtimer,
             uint32_t timer_cfg_bitset, reload_t timer_load_val) {

    id = timer_id;
    base = TIMER0_BASE + 0x1000 * timer_id;
    subtimer = timer_subtimer;
    reload_value = timer_load_val;
    cfg_bitset = timer_cfg_bitset;

    SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER0 + id);
    TimerConfigure(base, cfg_bitset);

    switch(subtimer) {
    case TIMER_A:
        IntEnable(INT_TIMER0A + id*2);
        break;
    case TIMER_B:
        IntEnable(INT_TIMER0A + id*2 + 1);
        break;
    case TIMER_BOTH:
        IntEnable(INT_TIMER0A + id*2);
        break;
    default:
        while (1) {}
    }

    reload();
}

void timer::reload() {

    TimerLoadSet(base, subtimer, reload_value);
}

void timer::start() {

    TimerIntEnable(base, TIMER_TIMA_TIMEOUT);
    TimerEnable(base, subtimer);
}

void timer::stop() {

    TimerIntDisable(base, TIMER_TIMA_TIMEOUT);
    TimerDisable(base, subtimer);
}

void timer::ack() {

    switch(subtimer) {
    case TIMER_A:
        TimerIntClear(base, INT_TIMER0A + id*2);
        break;
    case TIMER_B:
        TimerIntClear(base, INT_TIMER0A + id*2 + 1);
        break;
    case TIMER_BOTH:
        TimerIntClear(base, INT_TIMER0A + id*2);
        break;
    default:
        while (1) {}
    }
}
