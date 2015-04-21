/* -*- mode: c++; c-basic-offset: 4; */
/* Created by Hershal Bhave on <2015-03-15 Sun> */
/* Revision History: Look in Git FGT */

#include "blinker.hpp"
#include "timerpp.hpp"
#include "uartpp.hpp"
#include "shellpp.hpp"

#include "libos/os.h"
#include "libschedule/schedule.h"

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#include "inc/hw_memmap.h"

#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/uart.h"

blinker blink;
timer timer0a;
uart uart0;
shell shell0;

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
        uart0.printf("%d\n\r", blink_count_green);
        os_surrender_context();
        EndCritical(status);
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
    shell0 = shell(uart0);

    /* begin os init */
    os_threading_init();
    schedule(thread_1, 200);
    schedule(thread_0, 200);
    /* schedule(thread_uart_update, 1000000); */
    os_launch();
    /* end os init */

    /* main never terminates */
    while (1);
}

extern "C" void Timer0A_Handler() {
    timer0a.ack();
    blink.toggle(PIN_RED);
}

/* I avoid using a buffer object here so that the data structures are
 * instantiated correctly without client code. */
#define BUFFER_MAX_LENGTH 16

static uint8_t UART0_RX_BUFFER[BUFFER_MAX_LENGTH];
static uint8_t UART0_RX_BUFFER_SIZE = 0;

static uint8_t UART0_TX_BUFFER[BUFFER_MAX_LENGTH];
static uint8_t UART0_TX_BUFFER_SIZE = 0;

/*! True if a buffer is empty */
#define buffer_empty(buf)			\
    (buf##_SIZE == 0)

/*! Decrement the number of characters in buffer by the standard
 *  amount. */
#define buffer_dec(buf)				\
    buffer_dec_(buf, 1)

/*! Decrement the number of characters in buffer by amount.
 * \note This macro needs to be called on its own line.
 * \bug No warning on underflow failure
 * \bug Non-atomic
 */
#define buffer_dec_(buf, amount)				\
    do {							\
        if(buffer_len(buf) - amount >= 0) {			\
	    buffer_len(buf) = buffer_len(buf) - amount;		\
        }							\
    } while (false)

/*! Evaluate to the length of a buffer. */
#define buffer_len(buf)				\
    buf##_SIZE

/*! True if a buffer is full */
#define buffer_full(buf)			\
    (buf##_SIZE == BUFFER_MAX_LENGTH-1)

/*! True if a buffer is empty */
#define buffer_empty(buf)			\
    (buf##_SIZE == 0)

#define buffer_clear(buf)			\
    buffer_len(buf) = 0

/*! Initialize buffer and its metadata */
#define buffer_init(buf)			\
    buffer_len(buf) = 0;

/*! Add to buffer a single element.
 * \bug No warning on overflow failure
 */
#define buffer_add(buf, elt)				\
    do {						\
        if(buffer_len(buf) + 1 <= BUFFER_MAX_LENGTH) {  \
	    buf[buffer_len(buf)] = elt;			\
	    ++buffer_len(buf);                          \
        }                                               \
    } while (false)

extern "C" void UART0_Handler(void) {

    bool post;
    uint8_t recv;
    /* Get and clear the current interrupt sources */
    uint32_t interrupts = UARTIntStatus(UART0_BASE, true);
    UARTIntClear(UART0_BASE, interrupts);

    /* Are we being interrupted because the TX FIFO has space available? */
    /* if(interrupts & UART_INT_TX) { */
        /* Move as many bytes as we can into the transmit FIFO */
        /* uart_prime_transmit(UART0_BASE); */
    /* } */

    /* Are we being interrupted due to a received character? */
    if(interrupts & (UART_INT_RX | UART_INT_RT)) {
        /* Get all available chars from the UART */
        while(UARTCharsAvail(UART0_BASE)) {
            recv = (unsigned char) (UARTCharGetNonBlocking(UART0_BASE) & 0xFF);

            /* optional: check for '@echo_off */

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

                /* Echo the received character to the newline */
                UARTCharPut(UART0_BASE, '\n');
                break;

            default: break;
            }

            post = !buffer_full(UART0_RX_BUFFER);
            /* If there is room in the RX FIFO, store the char there,
             * else dump it. optional: a circular buffer might keep
             * more up-to-date data, considering this is a RTOS */
            /* this could be cleaned up with error-catching in the buffer library */
            if(post) {
                buffer_add(UART0_RX_BUFFER, recv);
                sem_post(HW_SEM_UART0);
            }
        }
    }
}

extern "C" void __cxa_pure_virtual() { while (1); }
