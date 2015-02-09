/* -*- mode: c; c-basic-offset: 4; -*- */
#include "nexus.h"

/* char* itoa(int i, char* buffer, uchar length) { */

/*     if (snprintf(buffer, length, "%02d", i) == -1) { */
/* 	return ""; /\* base case, or error *\/ */
/*     } */
/*     return buffer; */
/* } */

void* memset(void* b, int c, int len) {

    int i;
    unsigned char *p = b;
    i = 0;
    while(len > 0) {
	*p = c;
	++p;
	--len;
    }
    return b;
}

void *memcpy(void *str1, const void *str2, long n) {

}
