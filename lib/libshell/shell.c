/* -*- mode: c; c-basic-offset: 4; -*- */
#include "shell.h"

#include "libhw/hardware.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include <stdint.h>
#include <stdbool.h>

#include "driverlib/pin_map.h"
#include "driverlib/timer.h"
#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"

static unsigned short SHELL_BUFFER_POSITION;
static char SHELL_BUFFER[SHELL_BUFFER_LENGTH];

static char* SHELL_PS1[SHELL_MAX_PS1_LENGTH];
static char* SHELL_DEFAULT_PS1 = "> ";


void shell_spawn() {

    SysCtlPeripheralEnable(SYSCTL_PERIPH_UART0);
    hw_connect(HW_UART, UART0_BASE, shell_uart0_handler);
    shell_clear_shell_buffer();
    shell_set_ps1(SHELL_DEFAULT_PS1);
    shell_print_ps1();
}

char* shell_represent() {

    return SHELL_BUFFER;
}

void shell_kill() {

    hw_disconnect(HW_UART, UART0_BASE, shell_uart0_handler);
}

void shell_uart0_handler(char recv) {

    switch(recv) {
    case SC_CR:
        {   /* TODO: schedule? */
	    shell_execute_command();
	    shell_clear_shell_buffer();
        }
	/* TODO: represent better, with shell_represent */
	uart_send_char('\n');
	shell_print_ps1();
	break;
    case SC_BACKSPACE:
	SHELL_BUFFER[SHELL_BUFFER_POSITION--] = (char) 0;
	/* TODO: represent at all, with shell_represent */
	break;
    default:
	if (SHELL_BUFFER_LENGTH > SHELL_BUFFER_POSITION) {
	    SHELL_BUFFER[SHELL_BUFFER_POSITION++] = recv;
	}
	break;
    }
    /* Echo char to terminal for user */
    uart_send_char(recv);
}

void shell_set_ps1(char* new_ps1) {

    memcpy(SHELL_PS1, new_ps1, ustrlen(new_ps1));
    /* TODO: ensure this copies a null terminator */
}

void shell_clear_shell_buffer() {

    memset(SHELL_BUFFER, 0, sizeof(SHELL_BUFFER));
    SHELL_BUFFER_POSITION = 0;
}

void shell_print_ps1() {

    uart_send_string(SHELL_PS1);
}

/* TODO: implement */
exit_status_t shell_execute_command() {

    const char** arguments = NULL;
    return system_exec(SHELL_BUFFER, arguments);
}
