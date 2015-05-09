/* -*- mode: c++; c-basic-offset: 4; */
/* Created by Hershal Bhave and Eric Crosson on <2015-03-15 Sun> */
/* Revision History: Look in Git FGT */
#include "timerpp.hpp"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"
#include "driverlib/interrupt.h"

timer::timer() {}

timer::timer(utimer_t timer_id, subtimer_t timer_subtimer,
             uint32_t timer_configuration, reload_t timer_load_val,
             uint32_t timer_interrupt, bool timer_start) {

    id = timer_id;
    base = TIMER0_BASE + 0x1000 * timer_id;
    subtimer = timer_subtimer;
    reload_value = timer_load_val;
    configuration = timer_configuration;
    interrupt = timer_interrupt;

    SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER0 + id);
    TimerConfigure(base, configuration);

    switch(subtimer) {
    case TIMER_A:
    case TIMER_BOTH:
        IntEnable(INT_TIMER0A + id*2);
        break;
    case TIMER_B:
        IntEnable(INT_TIMER0A + id*2 + 1);
        break;
    default:
        while (1) {}
    }

    reload();
    if (timer_start) { start(); }
}

void timer::reload() {

    load(reload_value);
}

void timer::load(uint32_t load_value) {

    switch(subtimer) {
    case TIMER_A:
    case TIMER_BOTH:
        TimerLoadSet(base, TIMER_A, load_value);
        break;
    case TIMER_B:
        TimerLoadSet(base, TIMER_B, load_value);
        break;
    default:
        while (1) {}
    }
}

void timer::start() {

    TimerIntEnable(base, interrupt);
    TimerEnable(base, subtimer);
}

void timer::stop() {

    TimerIntDisable(base, interrupt);
    TimerDisable(base, subtimer);
}

uint32_t timer::ack() {

    switch(subtimer) {
    case TIMER_A:
    case TIMER_BOTH:
        TimerIntClear(base, INT_TIMER0A + id*2);
        break;
    case TIMER_B:
        TimerIntClear(base, INT_TIMER0A + id*2 + 1);
        break;
    default:
        while (1) {}
    }
    return 0xDEADBEEF;
}

uint32_t timer::get() {

    switch(subtimer) {
    case TIMER_A:
    case TIMER_BOTH:
        TimerValueGet(base, TIMER_A);
        break;
    case TIMER_B:
        TimerValueGet(base, TIMER_B);
        break;
    default:
        while (1) {}
    }
}

const subtimer_t timer::get_subtimer() {
    return subtimer;
}

const uint32_t timer::get_base() {
    return base;
}


/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
