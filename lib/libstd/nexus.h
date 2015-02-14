/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __NEXUS__
#define __NEXUS__

#include "defines.h"
#include <stdint.h>

#define immutable

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
int strcmp(const char* s1, const char* s2);
/** A duplicate of the c standard ustrncmp function. */
int ustrncmp(const char *s1, const char *s2, uint32_t n);

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
