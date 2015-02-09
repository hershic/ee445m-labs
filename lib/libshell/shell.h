/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __SHELL__
#define __SHELL__

#include <stdbool.h>

#include "libstd/nexus.h"

/* TODO: doxygenize */

typedef unsigned char shell_iterator;
#define SHELL_BUFFER_LENGTH 64
#define SHELL_MAX_COMMAND_NAME_LENGTH 16
#define SHELL_MAX_COMMANDS            16

typedef struct shell_command {

    bool valid;
    char name[SHELL_MAX_COMMAND_NAME_LENGTH];
    int(*command)();
} shell_command;

/* Obey the `thread` interface */
void shell_spawn();
char* shell_represent();
void shell_kill();

void shell_uart0_handler(char);

void shell_clear_shell_buffer();

bool shell_register_command(const char*, int(*)());
bool shell_deregister_command(const char*);

bool shell_execute_command();

#endif
