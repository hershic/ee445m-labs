/* -*- mode: c; c-basic-offset: 4; -*- */
#include "shellpp.hpp"

#include "libstd/nexus.h"
#include "libio/kbd.h"
#include "libuart/uart.h"
#include "libhw/hardware.h"
#include "libos/thread_structures.h"
#include "libos/os.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include <stdint.h>
#include <stdbool.h>

static unsigned short SHELL_BUFFER_POSITION;
/* Wondering why there's a one here? Where's waldo? */
static char SHELL_BUFFER[SHELL_BUFFER_LENGTH+1];

static char* SHELL_PS1[SHELL_MAX_PS1_LENGTH];
static char* SHELL_DEFAULT_PS1 = "> ";

shell::shell() {}

shell::shell(uint32_t uart_base, int32_t uart_int, char* ps1) {

    this->base = uart_base;
    this->_int = uart_int;
    uart_metadata_init(UART_DEFAULT_BAUD_RATE, this->base, this->_int);
    hw_subscribe(HW_UART, uart_metadata, this->shell_uart_handler);
    set_ps1(ps1);
    clear_cmd_buffer();
    shell_print_ps1();
}

char* shell_represent() {

    return SHELL_BUFFER;
}

/* TODO: abstract boilerplate, re: metadata */
shell::~shell() {

    uart_metadata_init(UART_DEFAULT_BAUD_RATE, this->base, this->_int);
    hw_unsubscribe(HW_UART, uart_metadata, this->shell_uart_handler);
}

void shell::shell_uart_handler(notification note) {

    char recv = note._char;
    int exit_code;

    switch(recv) {
    case SC_CR:
        uart_send_string("\r");

        exit_code = this->execute_cmd();
        if(exit_code != 0) {
            uart_send_udec(exit_code);
        }
        this->clear_cmd_buffer();
        uart_send_newline();
        this->print_ps1();
        break;

    case 127:
    case SC_BACKSPACE:
        this->cmd_buf_del();
        break;

    default:
        if (SHELL_BUFFER_LENGTH > this->buffer_position) {
            this->cmd_buf_add(recv);
            /* Echo char to terminal for user */
            uart_send_char(recv);
        }
        break;
    }
}

void shell::cmd_buf_del() {

    this->cmd[this->buffer_position--] = (char) 0;
    uart_send_backspace();
}

void shell::cmd_buf_add(char c) {

    this->cmd[this->buffer_position++] = c;
}

void shell::set_ps1(char* new_ps1) {

    umemcpy(ps1, new_ps1, ustrlen(new_ps1));
}

void shell::clear_cmd_buffer(void) {

    umemset(this->cmd, 0, sizeof(this->cmd));
    this->buffer_position = 0;
}

void shell::print_ps1(void) {

    uart_send_newline();
    uart_send_string((const char*) this->ps1);
}

exit_status_t shell::execute_cmd() {

    /* Null terminate to separate the cmd from the args */
    uint8_t idx = 0;
    while(idx < this->buffer_position && this->cmd[idx] != ' ') {
	++idx;
    }
    cmd[idx] = 0;
    /* Waldo says this line requires the extra char to be a 0 */
    return system_exec(cmd, &cmd[idx+1]);
}
