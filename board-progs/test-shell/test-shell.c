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
#include "driverlib/fpu.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/rom.h"

#include "libtimer/timer.h"
#include "libshell/shell.h"
#include "libnotify/notify.h"
#include "libhw/hardware.h"
#include "libuart/uart.h"

#include <sys/stat.h>

void UART0_Handler(void) {

    unsigned short i;

    while(UARTCharsAvail(UART0_BASE)) {
	/* Notify every subscribed task of each incoming character
	 * (but schedule them for later so we can return from this ISR
	 * asap). */

	hw_notification notification;
	notification._char = uart_get_char();

	/* TODO: schedule this thread instead of running it immediately */
	hw_notify(HW_UART, UART0_BASE, notification, NOTIFY_CHAR);
	uart_send_char(notification._char);
    }
}

int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    /* Enable the GPIO port that is used for the on-board LED. */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);

    /* Enable the peripherals used by this example. */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER2);

    hw_driver_init(HW_UART);
    shell_spawn();

    /* Enable processor interrupts. */
    IntMasterEnable();


    /* Postpone death */
    while(1) {};
}
