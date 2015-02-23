/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __NEXUS__
#define __NEXUS__

#include "defines.h"
#include <stdint.h>

/*! A keyword to signify that a value should never be reassigned. */
#define immutable

/*! Wrap a block of code and ensure it is executed without
 * interruption.
 * \warning Assumes that an int32t named atom exists in local
 * scope. */
#define atomic(x) {				\
    atom = StartCritical();			\
    x						\
    EndCritical(atom);				\
}

/*! A pointer to a memory location on the ARM Cortex M4. */
typedef int32_t memory_address_t;

/*! A macro to make it clear what we're doing with this while
 *  loop. Note that this macro accepts a body; that is to say you can
 *  pass a code block as an argument that will be executed
 *  infinitely. */
#define postpone_death(x) { \
    while(1) {              \
        x                   \
    }                       \
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

/*! Begin a non-interruptable critical section which preserves the
    priority mask
    \returns the current priority mask
*/
int32_t StartCritical();

/*! End a non-interruptable critical section which restores the
    priority mask
    \param primask the priority mask to restore
*/
void EndCritical(int32_t primask);

#endif
