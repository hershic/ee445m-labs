/* -*- mode: c; c-basic-offset: 4; -*- */

/* TODO: document */

/* Obey the `thread` interface */
void shell_spawn();
char* shell_represent();
void shell_kill();

void shell_uart0_handler(char);

#define SHELL_BUFFER_LENGTH 64
