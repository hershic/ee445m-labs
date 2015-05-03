/* -*- mode: c++; c-basic-offset: 4; */
/* Created by Hershal Bhave and Eric Crosson on <2015-03-15 Sun> */
/* Revision History: Look in Git FGT */

#include "../common/adcpp.hpp"
#include "../common/blinker.hpp"
#include "../common/uartpp.hpp"
#include "../common/shellpp.hpp"
#include "../common/semaphorepp.hpp"
#include "../common/motorpp.hpp"
#include "../common/drivepp.hpp"
#include "../common/canpp.hpp"
#include "../common/ctlsysctl.hpp"
#include "../common/switchpp.hpp"
#include "../common/pingpp.hpp"

#include "libos/os.h"
#include "libschedule/schedule.h"

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#include "inc/hw_memmap.h"

#include "driverlib/adc.h"
#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/uart.h"

blinker blink;
uart uart0;
ping ping0;
semaphore* ping0_sem;

#define UART0_RX_BUFFER_SIZE 8
static semaphore UART0_RX_SEM;
static buffer<char, UART0_RX_BUFFER_SIZE> UART0_RX_BUFFER;

void ping_handler() {

    ping0.start();

    while(1){
        if (ping0_sem->guard()) {
            ping0.sample();
            uart0.printf("ping: %u\n", ping0.average());
        }
        os_surrender_context();
    }
}

int main(void) {

    ctlsys::set_clock();
    IntMasterDisable();

    UART0_RX_BUFFER = buffer<char, UART0_RX_BUFFER_SIZE>(&UART0_RX_SEM);
    uart0 = uart(UART0_BASE, INT_UART0);

    blink = blinker(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_3);
    ping0 = ping(GPIO_PORTD_BASE, GPIO_PIN_3, 1, TIMER_A);
    ping0_sem = ping0.get_sem();

    os_threading_init();
    schedule(ping_handler, 200);
    os_launch();
}

extern "C" void Timer1A_Handler() {
    ping0.handle_timer();
}

extern "C" void GPIOPortD_Handler() {
    ping0.handle_gpio();
}

extern "C" void __cxa_pure_virtual() { while (1) {} }
