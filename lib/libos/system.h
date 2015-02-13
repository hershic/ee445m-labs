/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __SYSTEM__
#define __SYSTEM__

#include <stdbool.h>
#include "libstd/nexus.h"

/* TODO: doxygenize */

#define SYSTEM_MAX_COMMAND_NAME_LENGTH 16
#define SYSTEM_MAX_COMMANDS            16

typedef uint8_t  system_iterator;
typedef uint32_t exit_status_t;

typedef struct system_command {

    bool valid;
    char name[SYSTEM_MAX_COMMAND_NAME_LENGTH];
    int(*command)();
} system_command;

bool system_register_command(const char*, int(*)());
bool system_deregister_command(const char*);

/* Conventionally named */
exit_status_t system_exec(const char*, const char**);

#endif
