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
#include "libheart/heartbeat.h"
#include "libstd/nexus.h"

#include <sys/stat.h>


int doctor() {

    uart_send_string("\nWell what did you expect would happen? You're dreaming!\n");
    return EXIT_SUCCESS;
}

int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
		   SYSCTL_XTAL_16MHZ);

    /* Enable the GPIO port that is used for the on-board LED. */
    heart_init();

    /* Enable the peripherals used by this example. */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_TIMER2);

    hw_driver_init(HW_UART);
    hw_channel_init(HW_UART, UART0_BASE);
    uart_init();		/* defaults to UART0_BASE (thanks hw_driver) */

    shell_spawn();
    shell_register_command((const char*) "doctor", doctor);

    /* Enable processor interrupts. */
    IntMasterEnable();

    postpone_death();
}
