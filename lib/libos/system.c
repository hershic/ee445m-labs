/* -*- mode: c; c-basic-offset: 4; -*- */
#include "system.h"

#include <stdbool.h>
#include <stdlib.h>

#include "libut/utlist.h"
#include "libstd/nexus.h"

/*! libsystem debugging switch */
#define SYSTEM_DEBUG false

/** Statically allocated space for all system commands to reside. */
static system_command SYSTEM_COMMANDS[SYSTEM_MAX_COMMANDS];

/** Circular doubly-linked list containing all registered commands. */
system_command* registered_commands = NULL;

/** Circular doubly-linked list containing all unregistered commands. */
system_command* unregistered_commands = NULL;

/*! Invalid command for graceful exit on command-not-found */
system_command SYSTEM_INVALID_COMMAND;

void system_init() {

    system_iterator i;
    /* Invalidate the invalid command stub */
    SYSTEM_INVALID_COMMAND.valid = false;
    /* All commands begin in the unregistered state */
    for(i=0; i<SYSTEM_MAX_COMMANDS; ++i) {
        CDL_PREPEND(unregistered_commands, &SYSTEM_COMMANDS[i]);
    }
}

/* returns success (failure could be out of room) */
bool system_register_command(const char* command_name, int(*command)()) {

    /* Grab the first free command, move it from the unregisted to the
     * registered pile, and populate it with this function's
     * arguments */
    system_command* sys_command = unregistered_commands;
    CDL_DELETE(unregistered_commands, sys_command);
    CDL_PREPEND(registered_commands, sys_command);

    sys_command->valid = true;
    umemset(sys_command->name, 0, SYSTEM_MAX_COMMANDS);
    ustrcpy(sys_command->name, command_name);
    sys_command->command = command;
    return true;
}

bool system_deregister_command(const char* command_name) {

    /* Grab the structure of the command to deregister and invaldiate
     * the metadata. Move it from the registered to the unregistered
     * pile. */
    system_command* command = _system_command_from_name(command_name);
    command->valid = false;
    CDL_DELETE(registered_commands, command);
    CDL_PREPEND(unregistered_commands, command);
    return true;
}

/* OPTIMIZE: inline */
system_command* _system_command_from_name(const char* command_name) {

    system_iterator i=0;
    system_command *ret;
    while(i<SYSTEM_MAX_COMMANDS &&
          0 != ustrcmp(SYSTEM_COMMANDS[i].name, command_name)) {
        ++i;
    }

    /* Graceful exit on invalid command entry */
    if (i >= SYSTEM_MAX_COMMANDS) {
	ret = &SYSTEM_INVALID_COMMAND;
    } else {
	ret = &SYSTEM_COMMANDS[i];
    }
    return ret;
}

/* TODO: allow for argument passing */
exit_status_t system_exec(const char* command, const char** arguments) {

    system_command* sys_command = _system_command_from_name(command);
#ifdef SYSTEM_DEBUG
    uart_send_string("System executed is: ");
    uart_send_string(command);
    uart_send_string("\n");
#endif
    if (sys_command->valid) {
        return sys_command->command(/*arguments*/);
    } else {
        /* TODO: determine what to do here */
        /* postpone_death();       /\* plan a *\/ */
        return EXIT_FAILURE;    /* plan b */
    }
}
