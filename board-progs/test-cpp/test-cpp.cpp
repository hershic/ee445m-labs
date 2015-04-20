/* -*- mode: c++; c-basic-offset: 4; */
/* Created by Hershal Bhave on <2015-03-15 Sun> */
/* Revision History: Look in Git FGT */

#include "blinker.hpp"
#include "timerpp.hpp"

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

#include "inc/hw_memmap.h"

#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"

blinker blink;
timer timer0a;

int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN | SYSCTL_XTAL_16MHZ);
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_3);

    blink = blinker(GPIO_PORTF_BASE);
    timer0a = timer(0, TIMER_A, TIMER_CFG_PERIODIC, SysCtlClockGet() / 2);
    timer0a.start();

    while (1) {
        blink.toggle(PIN_BLUE);
    }
}

extern "C" void Timer0A_Handler() {
    TimerIntClear(TIMER0_BASE, TIMER_TIMA_TIMEOUT);
    GPIOPinWrite(GPIO_PORTF_BASE, PIN_GREEN, PIN_GREEN ^ GPIOPinRead(GPIO_PORTF_BASE, PIN_GREEN));
    blink.toggle(PIN_RED);
}

extern "C" void __cxa_pure_virtual() { while (1); }
