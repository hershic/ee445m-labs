/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __NEXUS__
#define __NEXUS__

#include <stdint.h>

/*! Convert an intever to a string.
 *  \brief Convert an integer to a string.
 *  \param i Int to convert into a string
 *  \param buffer Buffer for string contents
 *  \param length Length of allocated buffer
 *  \returns char* Buffer containing i represented as a string
 *  \ingroup Framebuffer
 */
/* char* itoa(int, char*, unsigned char); */
void* memset(void*, int, int);
void *memcpy(void*, const void*, long);
int strcmp(const char* s1, const char* s2);
int ustrncmp(const char *s1, const char *s2, uint32_t n);

#endif
