/* -*- mode: c++; c-basic-offset: 4; */
/* Created by Hershal Bhave on <2015-03-15 Sun> */
/* Revision History: Look in Git FGT */

#include "blinker.hpp"
#include "timerpp.hpp"
#include "uartpp.hpp"

#include "libos/os.h"
#include "libschedule/schedule.h"

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#include "inc/hw_memmap.h"

#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"

blinker blink;
timer timer0a;
uart uart0;

void thread_0() {

    while (1) {
        blink.toggle(PIN_BLUE);
        os_surrender_context();
    }
}

void thread_1() {

    while (1) {
        blink.toggle(PIN_GREEN);
        os_surrender_context();
    }
}


int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN | SYSCTL_XTAL_16MHZ);
    IntMasterDisable();

    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_3);
    blink = blinker(GPIO_PORTF_BASE);

    timer0a = timer(0, TIMER_A, TIMER_CFG_PERIODIC, SysCtlClockGet() / 2, TIMER_TIMA_TIMEOUT);
    timer0a.start();

    uart0 = uart(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
    uart0.printf("bite my shiny metal ass %d times\n\r", 1);

    /* begin os init */
    os_threading_init();
    schedule(thread_1, 200);
    schedule(thread_0, 50);
    os_launch();
    /* end os init */

    /* main never terminates */
    while (1);
}

extern "C" void Timer0A_Handler() {
    timer0a.ack();
    blink.toggle(PIN_RED);
}

extern "C" void __cxa_pure_virtual() { while (1); }
