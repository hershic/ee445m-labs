/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __shellpp__
#define __shellpp__

#include <stdbool.h>

#include "uartpp.hpp"

/*! \addtogroup Shell
 * @{
 */

#define SHELL_COMMANDS 2

#define SYSTEM_MAX_NAME_LENGTH 10
#define SYSTEM_MAX_COMMANDS    5

/*! PS1 maximum length */
#define SHELL_MAX_PS1_LENGTH 4

/*! Maximum length of shell input per command */
#define SHELL_BUFFER_LENGTH 64

#define UART_VERBOSE true

typedef uint8_t exit_status_t;
typedef exit_status_t (*sys_cmd)(const char*);

class shell {
private:
    uart uart0;
    uint8_t pos;
    char buf[SHELL_BUFFER_LENGTH];
    char ps1[SHELL_MAX_PS1_LENGTH];

    /*! Execute a system command. */
    int32_t ustrncmp(const char*, const char*, uint32_t);
    void* memset(void* b, int c, int len);

    /*! Return the length of a null-terminated string. */
    uint32_t strlen(const char*);

    static exit_status_t doctor(const char* args);
    static exit_status_t witch(const char* args);

    static char system_command_names[SHELL_COMMANDS][SYSTEM_MAX_NAME_LENGTH];
    static sys_cmd system_command_funcs[SHELL_COMMANDS];

    static void ustrcpy(char* dest, const char* source);

public:
    static char str_doc[10];
    static char str_witch[10];

    shell();
    shell(uart u);

    /*! Clear the shell buffer. */
    void clear_buffer();

    /*! Set the PS1. */
    void set_ps1(char* new_ps1);

    /*! Print the PS1. */
    void print_ps1();

    /*! Execute this command. */
    exit_status_t execute_command(char* cmd_and_args);
};

#endif

/*! End doxygen group
 * @}
 */
