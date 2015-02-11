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

int strcmp(const char* s1, const char* s2) {
    return(ustrncmp(s1, s2, (uint32_t)-1));
}

int ustrncmp(const char *s1, const char *s2, uint32_t n)
{
    //
    // Loop while there are more characters.
    //
    while(n)
    {
        //
        // If we reached a NULL in both strings, they must be equal so we end
        // the comparison and return 0
        //
        if(!*s1 && !*s2)
        {
            return(0);
        }

        //
        // Compare the two characters and, if different, return the relevant
        // return code.
        //
        if(*s2 < *s1)
        {
            return(1);
        }
        if(*s1 < *s2)
        {
            return(-1);
        }

        //
        // Move on to the next character.
        //
        s1++;
        s2++;
        n--;
    }

    //
    // If we fall out, the strings must be equal for at least the first n
    // characters so return 0 to indicate this.
    //
    return(0);
}
