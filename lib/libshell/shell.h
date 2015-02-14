/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __SHELL__
#define __SHELL__

#include <stdbool.h>

#include "libstd/nexus.h"
#include "libos/system.h"
#include "libio/kbd.h"

/* PS1 properties */
#define SHELL_MAX_PS1_LENGTH 4

/** Maximum length of shell input per command */
#define SHELL_BUFFER_LENGTH 64

/********************** Obey the `thread` interface **********************/
/** Spawn a shell. Enable necessary peripherals, initialize internal
 * data structures.
 * \bug No scoreboard prevents multiple shells from spawning - better
 * trust the init'ing dev
 */
void shell_spawn();

/** Display the current state of the shell process. The behavior of
 * representing a non-spawned shell is undetermined.
 * \bug This is not the sole method of displaying this process - it
 * should be.
 * \bug The behavior of representing a non-spawned shell is
 * undetermined.
 */
char* shell_represent();

/** Kill the current shell process. This involves disconnecting from
 * the hardware driver but not clearing internal data structures. The
 * behavior of killing a non-spawned shell is undetermined.
 * \bug The behavior of killing a non-spawned shell is undetermined.
 */
void shell_kill();
/*************************************************************************/

/** Pseudo-ISR used to handle incoming chars from a user over UART.
 * \bug Ambiguity on which uart channel is used for terminal communication.
 */
void shell_uart0_handler(char);

/** Clear \SHELL_BUFFER and reset \SHELL_BUFFER_POSITION */
void shell_clear_shell_buffer();

/** Print the PS1.
 * \bug Ambiguity on which uart channel is used for terminal communication.
 * \note This just displays to stdout, not a uart channel, which can
 * be picked up by a display entity driver or whatever they're going
 * to be called.
 */
void shell_print_ps1();

/** Update the PS1 char* in \SHELL_PS1 */
void shell_set_ps1();

/** Execute the command contained in \SHELL_BUFFER.
 * \bug This function does not allow the passing of arguments
 */
exit_status_t shell_execute_command();

#endif
