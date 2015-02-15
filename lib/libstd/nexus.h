/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __NEXUS__
#define __NEXUS__

#include "defines.h"
#include <stdint.h>

/** A macro to make it clear what we're doing with this while loop. */
inline static
void postpone_death() {

    while(1) {};
}

/** A duplicate of the c standard memset function. */
void* memset(void*, int, int);
/** A duplicate of the c standard memcpy function. */
void* memcpy(void*, const void*, long);
/** A duplicate of the c standard strcmp function. */
int strcmp(const char*, const char*);
/** A duplicate of the c standard strncmp function. */
int ustrncmp(const char*, const char*, uint32_t);
/** A duplicate of the c standard strcpy function.  */
void ustrcpy(char*, const char*);

#endif
