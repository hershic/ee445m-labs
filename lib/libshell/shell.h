/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __SHELL__
#define __SHELL__

#include <stdbool.h>

#include "libstd/nexus.h"
#include "libos/system.h"
#include "libio/kbd.h"

/* TODO: doxygenize */

#define SHELL_BUFFER_LENGTH 64

/* PS1 properties */
#define SHELL_MAX_PS1_LENGTH 4

typedef uint8_t shell_iterator;

/* Obey the `thread` interface */
void shell_spawn();
char* shell_represent();
void shell_kill();

void shell_uart0_handler(char);

void shell_clear_shell_buffer();
void shell_print_ps1();
void shell_set_ps1();

exit_status_t shell_execute_command();

#endif
