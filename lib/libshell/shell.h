/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __SHELL__
#define __SHELL__

#define SHELL_BUFFER_LENGTH 64

/* TODO: document */

/* Obey the `thread` interface */
void shell_spawn();
char* shell_represent();
void shell_kill();

void shell_uart0_handler(char);

#endif
