/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Eric Crosson 2015-02-10 */
#ifndef __SYSTEM__
#define __SYSTEM__

#include <stdbool.h>
#include "libstd/nexus.h"

/*! \addtogroup System
 * @{
 */

/*! Maximum length of a callable command. */
#define SYSTEM_MAX_COMMAND_NAME_LENGTH 16

/*! Maximum number of commands supported by the system. */
#define SYSTEM_MAX_COMMANDS            16

/*! Iterator optimized for size that is guaranteed to be able to
 * iterate over \SYSTEM_COMMANDS. */
typedef uint8_t  system_iterator;

/*! Type definition of an exit code. */
typedef uint8_t exit_status_t;

/*! Struct containing properties of a system command. */
struct system_command;
typedef struct system_command {

    /* TODO: document members of the struct */
    bool valid;
    char name[SYSTEM_MAX_COMMAND_NAME_LENGTH];
    int(*command)();

    struct system_command* next;
    struct system_command* prev;
} system_command;

/*! Initialize internal data structures.
 * \note Ensure this method is called before any commands are registered. */
void system_init();

/*! Register a command for later execution.
 * \returns True if command was successfully registered.
 */
bool system_register_command(const char*, int(*)());

/*! Remove a command from the index of executable system commands.
 * \returns True if command was successfully deregistered.
 */
bool system_deregister_command(const char*);

/*! Execute a system command.
 * \returns The exit code of the executed command.
 * \bug Currently argument passing is not supported.
 */
exit_status_t system_exec(const char*, const char**);

/*! For internal use only. This function returns a pointer to the
 * \system_command registered to the command name \command_name. */
system_command* _system_command_from_name(const char* command_name);

#endif

/*! End doxygen group
 * @}
 */
