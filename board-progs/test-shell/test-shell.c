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

#include "libshell/shell.h"
#include "libnotify/notify.h"
#include "libhw/hardware.h"
#include "libuart/uart.h"
#include "libheart/heartbeat.h"
#include "libstd/nexus.h"
#include "libos/system.h"
#include "libos/os.h"

#include <sys/stat.h>

int doctor() {

    /* uart_set_active_channel(UART0_BASE); */
    uart_send_string("Well what did you expect would happen? You're dreaming!\n");
    return EXIT_SUCCESS;
}

int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    IntMasterDisable();

    heart_init();
    os_threading_init(100 Hz);
    hw_init_daemon();

    system_init();
    system_register_command((const char*) "doctor", doctor);

    /* Initialize hardware devices */
    uart_metadata_init(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
    hw_init(HW_UART, uart_metadata);

    /* Initialize the shell and the system it interacts with */
    shell_spawn();

    /* Enable processor interrupts. */
    IntMasterEnable();

    os_launch();

    /* TODO: set self priority to lowest or kill thyself */
    postpone_death();
}
