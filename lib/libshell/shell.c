/* -*- mode: c; c-basic-offset: 4; -*- */
#include "shell.h"

#include "libhw/hardware.h"
#include "libstd/nexus.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

#include "driverlib/pin_map.h"
#include "driverlib/timer.h"
#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"

char shell_buffer[SHELL_BUFFER_LENGTH];
unsigned short shell_buffer_position;

void shell_spawn() {

    SysCtlPeripheralEnable(SYSCTL_PERIPH_UART0);

    hw_connect(HW_UART, UART0_BASE, shell_uart0_handler);

    /* clear current contents of shell_buffer */
    memset(shell_buffer, 0, sizeof(shell_buffer));
    shell_buffer_position = 0;
}

char* shell_represent() {

    return shell_buffer;
}

void shell_kill() {

    hw_disconnect(HW_UART, UART0_BASE, shell_uart0_handler);
}

void shell_uart0_handler(char recv) {

    if (SHELL_BUFFER_LENGTH > shell_buffer_position) {
	shell_buffer[shell_buffer_position++] = recv;
    }
}
