/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson on 2015-01-24 */
/* Revision History: Look in Git FGT */

/* Standard Libs */
#include <stdint.h>
#include <stdbool.h>

/* TI Includes */
#include "inc/hw_ints.h"
#include "inc/hw_memmap.h"

/* Driverlib Includes */
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"

#include "libuart/uart.h"
#include "libheart/heartbeat.h"
#include "libhw/hardware.h"

/*! Read the next character from the UART and write it back to the UART.
 *  \return void
 */
void uart_handler(notification note) {

    char recv = note._char;

    heart_wrap (
	uart_send_char(recv);
    );
}

/*! Accept input on UART 0, and parrot input back out to UART 0.
 * \return Exit status
 */
int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    IntMasterDisable();

    heart_init();

    uart_metadata_init(UART_DEFAULT_BAUD_RATE, UART0_BASE);
    hw_init(HW_UART, uart_metadata);
    hw_subscribe(HW_UART, uart_metadata, uart_handler);

    IntMasterEnable();

    /* Prompt for text to be entered. */
    uart_send_string("Enter text:");

    postpone_death();
}
