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

#include "libtimer/timer.h"
#include "libshell/shell.h"
#include "libnotify/notify.h"
#include "libhw/hardware.h"
#include "libuart/uart.h"
#include "libheart/heartbeat.h"

#include "driverlib/rom.h"

#include <sys/stat.h>

void UART0_Handler(void) {

    unsigned short i;

    while(ROM_UARTCharsAvail(UART0_BASE)) {

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

int doctor() {

    uart_send_string("\nWell what did you expect would happen? You're dreaming!\n");
    return EXIT_SUCCESS;
}

int main(void) {

    ROM_SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    /* Enable the GPIO port that is used for the on-board LED. */
    heart_init();

    /* Enable the peripherals used by this example. */
    ROM_SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER2);

    hw_driver_init(HW_UART);
    uart_init();                /* defaults to UART0_BASE (thanks hw_driver) */
    /* TODO: remove */
    uart_send_string("Driver initialized!\n");

    shell_spawn();
    shell_register_command((const char*) "doctor", doctor);

    /* Enable processor interrupts. */
    ROM_IntMasterEnable();

    /* Postpone death */
    while(1) {};
}
