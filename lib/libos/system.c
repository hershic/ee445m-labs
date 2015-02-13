#include "system.h"
#include "libstd/nexus.h"

#include <stdbool.h>

static system_command SYSTEM_COMMANDS[SYSTEM_MAX_COMMANDS];

/* returns success (could be out of room) */
bool system_register_command(const char* command_name, int(*command)()) {

    system_iterator i = 0;
    while(i<SYSTEM_MAX_COMMANDS && SYSTEM_COMMANDS[i].valid) {++i;}
    if(SYSTEM_COMMANDS[i].valid) {
    	/* There are no empty slots for a new shell command */
    	return false;
    }
    SYSTEM_COMMANDS[i].valid = true;
    memcpy(SYSTEM_COMMANDS[i].name, command_name, SYSTEM_MAX_COMMAND_NAME_LENGTH);
    SYSTEM_COMMANDS[i].command = command;
    return true;
}

bool system_deregister_command(const char* command_name) {

    system_iterator i=0;
    while(i<SYSTEM_MAX_COMMANDS &&
    	  0 != strcmp(SYSTEM_COMMANDS[i].name, command_name)) {
    	++i;
    }
    SYSTEM_COMMANDS[i].valid = false;
    return true;
}

exit_status_t system_exec(char* command, char** arguments) {


}
