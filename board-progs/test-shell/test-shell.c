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
#include "libstd/nexus.h"
#include "libos/system.h"
#include "libos/os.h"

#include <sys/stat.h>

#define HEARTBEAT_MODAL
#include "libheart/heartbeat.h"

uint32_t red_work = 0;
uint32_t blue_work = 0;
uint32_t green_work = 0;

void led_blink_red() {
    while (1) {
        ++red_work;
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_1,
                     GPIO_PIN_1 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_1));
        os_surrender_context();
    }
}

void led_blink_green() {
    /* while (1) { */
        ++green_work;
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_3,
                     GPIO_PIN_3 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_3));
        /* os_surrender_context(); */
    /* } */
}

void led_blink_blue() {
    while (1) {
        ++blue_work;
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2,
                     GPIO_PIN_2 ^ GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2));
        os_surrender_context();
    }
}

int doctor() {

    /* uart_set_active_channel(UART0_BASE); */
    UARTCharPut(UART0_BASE, 'd');
    /* uart_send_string("Well what did you expect would happen? You're dreaming!\n"); */
    return EXIT_SUCCESS;
}

int witch() {

    /* uart_set_active_channel(UART0_BASE); */
    uart_send_string("no help here\n");
    return EXIT_SUCCESS;
}

int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    IntMasterDisable();

    heart_init();
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_1);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_2);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_3);

    os_threading_init();
    schedule(hw_daemon, 100 Hz, DL_SOFT);
    schedule(led_blink_red, 100 Hz, DL_SOFT);
    schedule(led_blink_blue, 100 Hz, DL_SOFT);

    system_init();
    system_register_command((const char*) "doctor", doctor);
    system_register_command((const char*) "witch", witch);

    /* Initialize hardware devices */
    uart_metadata_init(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
    hw_init(HW_UART, uart_metadata);

    /* Initialize the shell and the system it interacts with */
    shell_spawn();

    IntMasterEnable();
    os_launch();
    postpone_death();
}
