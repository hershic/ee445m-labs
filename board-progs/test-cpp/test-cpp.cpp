/* -*- mode: c++; c-basic-offset: 4; */
/* Created by Hershal Bhave on <2015-03-15 Sun> */
/* Revision History: Look in Git FGT */

#include "adcpp.hpp"
#include "blinker.hpp"
#include "timerpp.hpp"
#include "uartpp.hpp"
#include "shellpp.hpp"
#include "semaphorepp.hpp"
#include "motorpp.hpp"
#include "ir.hpp"

#include "libio/kbd.h"
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

/* I avoid using a buffer object in the UART*_Handler so that the data
 * structures are instantiated correctly without client code. */
#include "pseudo-buffer.h"

blinker blink;
timer timer0a;
uart uart0;
shell shell0;
adc adc0;
motor motor0;

ir ir0;
ir ir1;
ir ir2;
ir ir3;

static semaphore UART0_RX_SEM;

static uint8_t UART0_RX_BUFFER[BUFFER_MAX_LENGTH];
static uint8_t UART0_RX_BUFFER_SIZE = 0;

uint32_t blink_count_green = 0;
uint32_t blink_count_blue = 0;

void thread_0() {

    while (1) {
        blink.toggle(PIN_BLUE);
        ++blink_count_blue;
        os_surrender_context();
    }
}

void thread_1() {

    while (1) {
        blink.toggle(PIN_GREEN);
        ++blink_count_green;
        os_surrender_context();
    }
}

void thread_uart_update() {

    while(1) {
        int32_t status = StartCritical();
        uart0.printf("%d\n\r", blink_count_blue);
        EndCritical(status);
        os_surrender_context();
    }
}

void shell_handler() {

    while(1) {
        if(UART0_RX_SEM.guard()) {
            UART0_RX_SEM.take();

            char recv = UART0_RX_BUFFER[buffer_len(UART0_RX_BUFFER)-1];
            buffer_dec(UART0_RX_BUFFER);

            switch(recv) {
            case SC_CR:
                shell0.execute_command();
            break;

            case 127:
            case SC_BACKSPACE:
                shell0.backspace();
                break;

            default:
                shell0.type(recv);
                break;
            }
        }
        os_surrender_context();
    }
}

extern "C" void Timer0A_Handler() {
    timer0a.ack();
}

extern "C" void UART0_Handler(void) {

    uint8_t recv;

    /* Get and clear the current interrupt sources */
    uint32_t interrupts = UARTIntStatus(UART0_BASE, true);
    UARTIntClear(UART0_BASE, interrupts);

    /* Are we being interrupted due to a received character? */
    if(interrupts & (UART_INT_RX | UART_INT_RT)) {
        /* Get all available chars from the UART */
        while(UARTCharsAvail(UART0_BASE)) {
            recv = (unsigned char) (UARTCharGetNonBlocking(UART0_BASE) & 0xFF);

            /* Handle backspace by erasing the last character in the
             * buffer */
            switch(recv) {
            case 127:
            case '\b':
                /* If there are any chars to delete, delete the last text */
                if(!buffer_empty(UART0_RX_BUFFER)) {
                    /* Erase previous characters on the user's terminal */
                    uart0.printf("\b \b");
                    /* Decrement the number of chars in the buffer */
                    buffer_dec(UART0_RX_BUFFER);
                    /* Skip ahead to next buffered char */
                    continue;
                }
                /* if it is empty, somebody is watching so pass along
                 * the backspace */
                break;

            case '\r':
            case '\n':
                if(recv == '\r') {
                    UART_LAST_WAS_CR = true;
                }
                else if (UART_LAST_WAS_CR) {
                    UART_LAST_WAS_CR = false;
                    /* Don't react twice to a single newline */
                    continue;
                }
            case 0x1b:
                /* Regardless of the newline received, our convention
                 * is to mark end-of-lines in a buffer with the CR
                 * character. */
                recv = '\r';
                break;

            default: break;
            }

            /* If there is room in the RX FIFO, store the char there,
             * else dump it. optional: a circular buffer might keep
             * more up-to-date data, considering this is a RTOS */
            /* this could be cleaned up with error-catching in the buffer library */
            if(!buffer_full(UART0_RX_BUFFER)) {
                buffer_add(UART0_RX_BUFFER, recv);
                UART0_RX_SEM.post();
            }
        }
    }
}

extern "C" void ADC0Seq0_Handler(void) {

    adc0.ack();
    adc0.sample();

    ir0.sample();
    ir1.sample();
    ir2.sample();
    ir3.sample();

    blink.toggle(PIN_RED);
}

extern "C" void __cxa_pure_virtual() { while (1) {} }

int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);
    IntMasterDisable();

    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_3);
    blink = blinker(GPIO_PORTF_BASE);

    timer0a = timer(0, TIMER_A, TIMER_CFG_PERIODIC, SysCtlClockGet() / 2,
                    TIMER_TIMA_TIMEOUT);
    timer0a.start();

    UART0_RX_SEM = semaphore();
    uart0 = uart(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
    shell0 = shell(uart0);

    adc0 = adc(ADC0_BASE, ADC_TRIGGER_TIMER, 0);
    /* PE3 */
    adc0.configure_sequence(ADC_CTL_CH0);
    /* PE2 */
    adc0.configure_sequence(ADC_CTL_CH1);
    /* PE1 */
    adc0.configure_sequence(ADC_CTL_CH2);
    /* PE0 */
    adc0.configure_sequence(ADC_CTL_CH3 | ADC_CTL_IE | ADC_CTL_END);

    adc0.configure_timer_interrupt(timer0a.base, timer0a.subtimer);
    adc0.start();

    ir0 = ir(0, &adc0);
    ir1 = ir(1, &adc0);
    ir2 = ir(2, &adc0);
    ir3 = ir(3, &adc0);

    /*******************************************************/
    /* !!!! NOTICE !!!!                                    */
    /* Right now, the motor uses the same pins as the ADC. */
    /* TODO: This needs to be fixed                        */
    /*******************************************************/
    /* motor0 = motor(10000, 9999, FORWARD); */

    /* begin os init */
    os_threading_init();
    /* schedule(thread_1, 200); */
    schedule(thread_0, 200);
    schedule(shell_handler, 200);
    /* schedule(thread_uart_update, 1000000); */
    os_launch();
    /* end os init */

    /* main never terminates */
    while (1);
}
