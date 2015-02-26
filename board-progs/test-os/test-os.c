/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson on 2015-01-24 */
/* Revision History: Look in Git FGT */

/* Standard Libs */
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/* TI Includes */
#include "inc/hw_ints.h"
#include "inc/hw_memmap.h"

/* Driverlib Includes */
#include "driverlib/debug.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/systick.h"
#include "driverlib/uart.h"

#define HEARTBEAT_MODAL

#include "libos/os.h"
#include "libheart/heartbeat.h"

#include <sys/stat.h>

#define UART_FIFO_SIZE 128

volatile uint32_t pidwork = 0; // number of times thread1 has looped
volatile uint32_t highest_pidwork = 0;

volatile uint32_t uart_producer_idx;
volatile uint32_t uart_consumer_idx;
volatile char* uart_fifo;
volatile uint32_t uart_dropped_chars;

/*! A thread that continuously toggles GPIO pin 1 on GPIO_PORT_F. */
void Thread1(void){
    while(1) {
        ++pidwork;
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2,
                     GPIO_PIN_1 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_1));
    }
}

/*! A thread that continuously toggles GPIO pin 2 on GPIO_PORT_F. */
void Thread2(void){
    while(1) {
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2,
                     GPIO_PIN_2 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2));
        if (highest_pidwork < pidwork) {
            highest_pidwork = pidwork;
        }
        pidwork = 0;
    }
}

void uart_consumer(void) {

    while (1) {
        while (((uart_consumer_idx+1) % UART_FIFO_SIZE) != uart_producer_idx) {

            uart_consumer_idx = (uart_consumer_idx+1) % UART_FIFO_SIZE;
            GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_3,
                         GPIO_PIN_3 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_3));
            UARTCharPut(UART0_BASE, uart_fifo[uart_consumer_idx]);
        }
    }
}

void init_uart(void) {
}

/*! A thread that continuously toggles GPIO pin 3 on GPIO_PORT_F. */
int main() {

    char uart_fifo_raw[UART_FIFO_SIZE];

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    IntMasterDisable();

    SysCtlPeripheralEnable(SYSCTL_PERIPH_UART0);
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOA);
    GPIOPinConfigure(GPIO_PA0_U0RX);
    GPIOPinConfigure(GPIO_PA1_U0TX);
    GPIOPinTypeUART(GPIO_PORTA_BASE, GPIO_PIN_0 | GPIO_PIN_1);
    /* This is the HF aculprit Thursday February 26, 2015 */
    UARTConfigSetExpClk(UART0_BASE, SysCtlClockGet(),
                        115200, (UART_CONFIG_WLEN_8 | UART_CONFIG_STOP_ONE |
                                 UART_CONFIG_PAR_NONE));

    /* Enable the UART interrupt. */
    IntEnable(INT_UART0);
    UARTIntEnable(UART0_BASE, UART_INT_RX | UART_INT_RT);

    uart_fifo = uart_fifo_raw;
    uart_producer_idx = 0;
    uart_consumer_idx = UART_FIFO_SIZE - 1;

    os_threading_init();
    os_add_thread(Thread1);
    os_add_thread(Thread2);
    os_add_thread(uart_consumer);

    /* Load and enable the systick timer */
    SysTickPeriodSet(SysCtlClockGet() / 1000);
    SysTickEnable();
    SysTickIntEnable();

    heart_init();
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_1);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_2);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_3);

    /* os_trap_ */
    os_launch();

    /* PONDER: why do interrupts fire without this? */
    IntMasterEnable();
    postpone_death();
}

void UART0_Handler(void) {

    unsigned long look_at_me = UARTIntStatus(UART0_BASE, false);
    UARTIntClear(UART0_BASE, look_at_me);

    while(UARTCharsAvail(UART0_BASE)) {

        uart_fifo[uart_producer_idx] = UARTCharGet(UART0_BASE);
        uart_producer_idx = (uart_producer_idx+1) % UART_FIFO_SIZE;
        if (uart_producer_idx == uart_consumer_idx) {
            ++uart_dropped_chars;
        }

    }
}
