/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __NEXUS__
#define __NEXUS__

#include "defines.h"
#include <stdint.h>

/* TODO: doxytize */

inline static
void postpone_death() {

    while(1) {};
}

void* memset(void*, int, int);
void *memcpy(void*, const void*, long);
int strcmp(const char* s1, const char* s2);
int ustrncmp(const char *s1, const char *s2, uint32_t n);

#endif
