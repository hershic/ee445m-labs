/* -*- mode: c; c-basic-offset: 4; -*- */
#include "shell.h"

#include "libuart/uart.h"
#include "libhw/hardware.h"
#include "libos/thread_structures.h"
#include "libos/os.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include <stdint.h>
#include <stdbool.h>

#include "driverlib/pin_map.h"
#include "driverlib/timer.h"
#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"

static uint16_t SHELL_BUFFER_POSITION;
/* Wondering why there's a one here? Where's waldo? */
static char SHELL_BUFFER[SHELL_BUFFER_LENGTH+1];

static char* SHELL_PS1[SHELL_MAX_PS1_LENGTH];
static char* SHELL_DEFAULT_PS1 = "\r\n> ";

void shell_spawn() {

    /* TODO: have this handled by hw_connect (look at the scoreboard,
     * see what HAS been used (don't need to initialize), what IS
     * being used (blocked)) */
    uart_metadata_init(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
    hw_subscribe(HW_UART, uart_metadata, shell_uart_handler);
    shell_set_ps1(SHELL_DEFAULT_PS1);
    shell_clear_shell_buffer();
    shell_print_ps1();
}
char* shell_represent() {

    return SHELL_BUFFER;
}

/* TODO: abstract boilerplate, re: metadata */
void shell_kill() {

    uart_metadata_init(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
    hw_unsubscribe(HW_UART, uart_metadata, shell_uart_handler);
}

/* TODO: re-doxygenize */
void shell_uart_handler(notification note) {

    char recv = note._char;
    int exit_code;

    switch(recv) {
    case SC_CR:
    {   /* TODO: schedule */
        uart_send_string("\r");

        exit_code = shell_execute_command();
        if(exit_code != 0) {
            uart_send_udec(exit_code);
        }
        shell_clear_shell_buffer();
        uart_send_string("\r\n");
        /* fixme: why doesn't PS1 print twice? */
        shell_print_ps1();
    }
    break;

    case 127:
    case SC_BACKSPACE:
        SHELL_BUFFER[SHELL_BUFFER_POSITION--] = (char) 0;
        uart_send_string("\b \b");
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

    umemcpy(SHELL_PS1, new_ps1, ustrlen(new_ps1));
}

void shell_clear_shell_buffer() {

    umemset(SHELL_BUFFER, 0, sizeof(SHELL_BUFFER));
    SHELL_BUFFER_POSITION = 0;
}

void shell_print_ps1() {

    uart_send_string((const char*) SHELL_PS1);
}

exit_status_t shell_execute_command() {

    /* Null terminate to separate the cmd from the args */
    uint8_t idx = 0;
    while(idx < SHELL_BUFFER_POSITION && SHELL_BUFFER[idx] != ' ') {
	++idx;
    }
    SHELL_BUFFER[idx] = 0;
    /* Waldo says this line requires the extra char to be a 0 */
    return system_exec(SHELL_BUFFER, &SHELL_BUFFER[idx+1]);
}
