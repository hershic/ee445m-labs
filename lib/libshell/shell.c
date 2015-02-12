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
static shell_command SHELL_COMMANDS[SHELL_MAX_COMMANDS];

void shell_spawn() {

    SysCtlPeripheralEnable(SYSCTL_PERIPH_UART0);
    hw_connect(HW_UART, UART0_BASE, shell_uart0_handler);
    shell_clear_shell_buffer();
    shell_print_prompt();
}

char* shell_represent() {

    return SHELL_BUFFER;
}

void shell_kill() {

    hw_disconnect(HW_UART, UART0_BASE, shell_uart0_handler);
}

void shell_uart0_handler(char recv) {

    /* TODO: handle enter */
    /* TODO: handle deregestering commands */
    /* TODO: test both re/dereg commands */

    switch(recv) {
    case '\r':
	shell_execute_command();
	uart_send_char('\n');
	shell_clear_shell_buffer();
	shell_print_prompt();
	break;
    case 8:
	/* TODO: backspace */
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

void shell_clear_shell_buffer() {

    memset(SHELL_BUFFER, 0, sizeof(SHELL_BUFFER));
    SHELL_BUFFER_POSITION = 0;
}

/* TODO: globalize prompt */
/* TODO: make sure shell is explicit about its uart channel */
void shell_print_prompt() {

    uart_send_string("> ");
}

/* returns success (could be out of room) */
bool shell_register_command(const char* command_name, int(*command)()) {

    shell_iterator i = 0;
    while(i<SHELL_MAX_COMMANDS && SHELL_COMMANDS[i].valid) {++i;}
    if(SHELL_COMMANDS[i].valid) {
    	/* There are no empty slots for a new shell command */
    	return false;
    }
    SHELL_COMMANDS[i].valid = true;
    memcpy(SHELL_COMMANDS[i].name, command_name, SHELL_MAX_COMMAND_NAME_LENGTH);
    SHELL_COMMANDS[i].command = command;
    return true;
}

bool shell_deregister_command(const char* command_name) {

    shell_iterator i=0;
    while(i<SHELL_MAX_COMMANDS &&
    	  0 != strcmp(SHELL_COMMANDS[i].name, command_name)) {
    	++i;
    }
    SHELL_COMMANDS[i].valid = false;
    return true;
}

/* TODO: decouple from SHELL_BUFFER: aka allow any command to
 * be executed from a char* */
/* TODO: allow for arguments */
bool shell_execute_command() {

    shell_iterator i=0;
    while(i<SHELL_MAX_COMMANDS) {
    	if (SHELL_COMMANDS[i].valid &&
    	    0 == strcmp(SHELL_COMMANDS[i].name, SHELL_BUFFER)) {
    	    SHELL_COMMANDS[i].command(/*arguments*/);
    	    return true;
    	}
    }
    return false;
}
