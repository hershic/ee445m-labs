/* -*- mode: c; c-basic-offset: 4; -*- */
#include "shell.h"

#include "libhw/hardware.h"
#include "libstd/nexus.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include <stdint.h>
#include <stdbool.h>

#include "driverlib/pin_map.h"
#include "driverlib/timer.h"
#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"

#include "driverlib/rom.h"

unsigned short SHELL_BUFFER_POSITION;
char SHELL_BUFFER[SHELL_BUFFER_LENGTH];
shell_command SHELL_COMMANDS[SHELL_MAX_COMMANDS];

void shell_spawn() {

    ROM_SysCtlPeripheralEnable(SYSCTL_PERIPH_UART0);

    hw_connect(HW_UART, UART0_BASE, shell_uart0_handler);

    /* clear current contents of SHELL_BUFFER */
    memset(SHELL_BUFFER, 0, sizeof(SHELL_BUFFER));
    SHELL_BUFFER_POSITION = 0;
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
    case 13:			/* TODO: enter */
	shell_execute_command();
	break;
    case 8: 			/* TODO: backspace */
    default:
	if (SHELL_BUFFER_LENGTH > SHELL_BUFFER_POSITION) {
	    SHELL_BUFFER[SHELL_BUFFER_POSITION++] = recv;
	}
    }
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
    strcmp(SHELL_COMMANDS[i].name, command_name);
    /* while(i<SHELL_MAX_COMMANDS && */
    /* 	  0 != strcmp(SHELL_COMMANDS[i].name, command_name)) { */
    /* 	++i; */
    /* } */
    SHELL_COMMANDS[i].valid = false;
    return true;
}

/* OPTIONAL TODO: decouple from SHELL_BUFFER */
/* TODO: allow for arguments */
bool shell_execute_command() {

    /* shell_iterator i=0; */
    /* while(i<SHELL_MAX_COMMANDS) { */
    /* 	if (SHELL_COMMANDS[i].valid && */
    /* 	    0 == strcmp(SHELL_COMMANDS[i].name, SHELL_BUFFER)) { */
    /* 	    SHELL_COMMANDS[i].command(/\*arguments*\/); */
    /* 	    return true; */
    /* 	} */
    /* } */
    return false;
}
