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
#include "libhw/hardware.h"

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

/*! Read the next character from the UART and write it back to the UART.
 *  \return void
 */
void uart_handler(notification note) {

    char recv = note._char;

    uart_send_char(recv);
    led_blink_green();
}

/*! Accept input on UART 0, and parrot input back out to UART 0.
 * \return Exit status
 */
int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    IntMasterDisable();

    heart_init();
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_1);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_2);
    heart_init_(GPIO_PORTF_BASE, GPIO_PIN_3);

    uart_metadata_init(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
    hw_init_and_subscribe(HW_UART, uart_metadata, uart_handler);

    os_threading_init();
    schedule(led_blink_red, 100 Hz, DL_SOFT);
    schedule(led_blink_blue, 100 Hz, DL_SOFT);
    schedule(hw_daemon, 100 Hz, DL_SOFT);

    /* Prompt for text to be entered. */
    uart_send_string("Enter text:");

    IntMasterEnable();
    os_launch();
    postpone_death();
}
