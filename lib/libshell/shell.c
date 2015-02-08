/* -*- mode: c; c-basic-offset: 4; -*- */
#include "shell.h"

#include "libhw/libhw.h"
#include "libstd/nexus.h"
#include "libshell/shell.h"

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
    /* device class, device specifics, task's interrupt handler */
    hw_connect(HW_UART, UART0_BASE, shell_uart0_handler);
    /* clear current contents of shell_buffer */
    memset(shell_buffer, 0, sizeof(shell_buffer));
    shell_buffer_position = 0;

    /* check: if this breaks, try the below line (indicating
     * shell_buffer is on the heap) */
    /* memset(shell_buffer, 0, SHELL_BUFFER_LENGTH*sizeof(*shell_buffer)); */
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
