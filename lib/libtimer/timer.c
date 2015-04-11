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

#define TIMER_DEFAULT_PRIORITY 0

void timer_add_interrupt(hw_metadata metadata) {

    TimerConfigure(metadata.timer.base, metadata.timer.periodic);
    TimerLoadSet(metadata.timer.base, metadata.timer.subtimer, SysCtlClockGet() / metadata.timer.frequency);
    TimerIntEnable(metadata.timer.base, TIMER_TIMA_TIMEOUT);
    IntEnable(metadata.timer.interrupt, TIMER_DEFAULT_PRIORITY);
    TimerEnable(metadata.timer.base, metadata.timer.subtimer);
}
