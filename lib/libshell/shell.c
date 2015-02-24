/* -*- mode: c; c-basic-offset: 4; -*- */
#include "shell.h"

#include "libuart/uart.h"
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
static char* SHELL_DEFAULT_PS1 = "\n> ";

void shell_spawn() {

    /* TODO: have this handled by hw_connect (look at the scoreboard,
     * see what HAS been used (don't need to initialize), what IS
     * being used (blocked)) */
    /* SysCtlPeripheralEnable(SYSCTL_PERIPH_UART0); */
    uart_metadata_init(UART_DEFAULT_BAUD_RATE, UART0_BASE);
    hw_subscribe(HW_UART, uart_metadata, shell_uart0_handler);
    shell_set_ps1(SHELL_DEFAULT_PS1);
    shell_clear_shell_buffer();
    shell_print_ps1();
}

char* shell_represent() {

    /* TODO: improve */
    return SHELL_BUFFER;
}

/* TODO: abstract boilerplate, re: metadata */
void shell_kill() {

    uart_metadata_init(UART_DEFAULT_BAUD_RATE, UART0_BASE);
    hw_unsubscribe(HW_UART, uart_metadata, shell_uart0_handler);
}

void shell_uart0_handler(notification note) {

    char recv = note._char;
    switch(recv) {
    case SC_CR:
        {   /* TODO: schedule */
	    shell_execute_command();
	    shell_clear_shell_buffer();
	    uart_send_string("\r\n");
	    /* TODO: why doesn't PS1 print twice? */
	    shell_print_ps1();
        }
	break;

    case SC_BACKSPACE:
	SHELL_BUFFER[SHELL_BUFFER_POSITION--] = (char) 0;
	/* TODO: represent at all, with shell_represent */
	break;

    default:
	if (SHELL_BUFFER_LENGTH > SHELL_BUFFER_POSITION) {
	    SHELL_BUFFER[SHELL_BUFFER_POSITION++] = recv;
	    /* Echo char to terminal for user */
	    uart_send_char(recv);
	}
	break;
    }
}

void shell_set_ps1(char* new_ps1) {

    /* TODO: ensure this copies a null terminator */
    memcpy(SHELL_PS1, new_ps1, ustrlen(new_ps1));
}

void shell_clear_shell_buffer() {

    memset(SHELL_BUFFER, 0, sizeof(SHELL_BUFFER));
    SHELL_BUFFER_POSITION = 0;
}

void shell_print_ps1() {

    uart_send_string((const char*) SHELL_PS1);
}

exit_status_t shell_execute_command() {

    const char** arguments = NULL;
    return system_exec(SHELL_BUFFER, arguments);
}
