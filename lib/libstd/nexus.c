/* -*- mode: c; c-basic-offset: 4; -*- */
#include "nexus.h"

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

/* TODO: sanitize long to a type of specific length */
/* TODO: use a duff device (speed is the name of the game, remember?) */
void *memcpy(void *str1, const void *str2, long n) {

    long i = 0;
    uint8_t *dest8 = (uint8_t*)str1;
    uint8_t *source8 = (uint8_t*)str2;
    for (i=0; i<n; ++i) {
        dest8[i] = source8[i];
    }
}

int strcmp(const char* s1, const char* s2) {

    return(ustrncmp(s1, s2, (uint32_t)-1));
}

int ustrncmp(const char *s1, const char *s2, uint32_t n) {

    /* Loop while there are more characters. */
    while(n) {
        /* If we reached a NULL in both strings, they must be equal so
	 * we end the comparison and return 0 */
        if(!*s1 && !*s2) {
            return(0);
        }

        /* Compare the two characters and, if different, return the
	 * relevant return code. */
        if(*s2 < *s1) {
            return(1);
        }
        if(*s1 < *s2) {
            return(-1);
        }

        /* Move on to the next character. */
        s1++;
        s2++;
        n--;
    }
    /* If we fall out, the strings must be equal for at least the
     * first n characters so return 0 to indicate this. */
    return 0;
}

int32_t StartCritical() {
    asm("MRS    R0, PRIMASK  ;// save old status\n"
        "CPSID  I            ;// mask all (except faults)\n");
}

void EndCritical(int32_t primask) {
    asm("MSR    PRIMASK, R0\n");
}
