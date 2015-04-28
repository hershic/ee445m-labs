/* -*- mode: c++; c-basic-offset: 4; */
/* Created by Hershal Bhave and Eric Crosson on <2015-03-15 Sun> */
/* Revision History: Look in Git FGT */

#include "blinker.hpp"
#include "uartpp.hpp"
#include "shellpp.hpp"
#include "semaphorepp.hpp"
#include "ctlsysctl.hpp"
#include "switchpp.hpp"

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
#include "driverlib/pwm.h"

#define thread(x)                               \
    do {                                        \
        x;                                      \
        os_surrender_context();                 \
    } while(true)

blinker blink;
uart uart0;
shell shell0;
lswitch switch0;

#define UART0_RX_BUFFER_SIZE 8
static semaphore UART0_RX_SEM;
static buffer<char, UART0_RX_BUFFER_SIZE> UART0_RX_BUFFER;

uint32_t blink_count_red = 0;
uint32_t blink_count_green = 0;
uint32_t blink_count_blue = 0;

semaphore sem_switch;
semaphore sem_blink_red;
semaphore sem_blink_blue;
semaphore sem_blink_green;

void thread_blink_red() {

    thread (
        /* if (sem_blink_red.guard()) { */
        blink.toggle(PIN_RED);
        ++blink_count_red;
        /* } */
        );
}

void thread_blink_blue() {

    thread (
        if (sem_blink_blue.guard()) {
            blink.toggle(PIN_BLUE);
            ++blink_count_blue;
        }
        );
}

void thread_blink_green() {

    thread (
        /* if (sem_blink_green.guard()) { */
        blink.toggle(PIN_GREEN);
        ++blink_count_green;
        /* } */
        );
}

void thread_uart_update() {

    thread (
        uart0.atomic_printf("%x %x %x\n\r", blink_count_red,
                            blink_count_blue, blink_count_green);
        );
}

extern "C" void __cxa_pure_virtual() { while (1) {} }

extern "C" void UART0_Handler(void) {

    if(!(uart0.ack() & (UART_INT_RX | UART_INT_RT))) {
        return;
    }

    while(UARTCharsAvail(UART0_BASE)) {
        char recv = uart0.get_char();

        /* Regardless of newline received, our convention is to
         * mark end-of-lines in a buffer with the CR character. */
        switch(recv) {
        case '\n':
            if (uart::LAST_WAS_CR) {
                uart::LAST_WAS_CR = false;
                continue;
            }
            break;
        case '\r':
            uart::LAST_WAS_CR = true;
            break;
        case 0x1b:
            recv = '\r';
            break;
        default: break;
        }
        UART0_RX_BUFFER.notify((const int8_t) recv);
        blink.blink(PIN_RED);
    }
}

extern "C" void GPIOPortB_Handler(void) {

    switch0.ack();
}

void lswitch_handler() {

    while(1) {
        if(sem_switch.guard()) {

            uint32_t sample = switch0.sample();
            uint32_t sample_bro = 5;
        }
        os_surrender_context();
    }
}

void shell_handler() {

    while(1) {
        if(UART0_RX_SEM.guard()) {

            bool ok;
            char recv = UART0_RX_BUFFER.get(ok);

            if(ok) { shell0.accept(recv); }
        }
        os_surrender_context();
    }
}

int main(void) {

    ctlsys::set_clock();
    IntMasterDisable();

    blink = blinker(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_3);

    sem_blink_red = semaphore();
    sem_blink_green = semaphore();
    sem_blink_blue = semaphore();

    UART0_RX_BUFFER = buffer<char, UART0_RX_BUFFER_SIZE>(&UART0_RX_SEM);

    uart0 = uart(UART0_BASE, INT_UART0);
    shell0 = shell(&uart0);

    switch0 = lswitch(GPIO_PORTB_BASE, GPIO_PIN_0, &sem_switch, GPIO_BOTH_EDGES, true);

    os_threading_init();
    schedule(thread_blink_red, 200);
    schedule(thread_blink_blue, 200);
    schedule(thread_blink_green, 200);
    schedule(shell_handler, 200);
    schedule(lswitch_handler, 200);
    /* schedule(thread_uart_update, 200); */
    os_launch();
}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
