/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __shellpp__
#define __shellpp__

#include <stdbool.h>

#include "uartpp.hpp"

#include "libos/system.h"

/*! \addtogroup Shell
 * @{
 */

/*! PS1 maximum length */
#define SHELL_MAX_PS1_LENGTH 4

/*! Maximum length of shell input per command */
#define SHELL_BUFFER_LENGTH 64

#define UART_VERBOSE true

class shell {
private:
    uart uart0;
    uint8_t pos;
    char buf[SHELL_BUFFER_LENGTH];
    char ps1[SHELL_MAX_PS1_LENGTH];
public:
    shell();
    shell(uart u);

    /*! Clear the shell buffer. */
    void clear_buffer();

    /*! Set the PS1. */
    void set_ps1(char* new_ps1);

    /*! Print the PS1. */
    void print_ps1();

    /*! Execute the shell command described in \buf */
    exit_status_t execute_command();

    /*! Shell's pseudo isr -- feed it chars to advance the shell's
     *  state. */
    void shell_uart_handler(const char);
};

#endif

/*! End doxygen group
 * @}
 */
