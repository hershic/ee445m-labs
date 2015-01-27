#ifndef _STRING_CASE_
#define _STRING_CASE_

/* TODO: Doxygenize */

#define streql(n, m) (0 == strcmp(n, m))

#define kString "String"

typedef struct {

  char* string;
  char* (*function)();
} StringCase;

char* StringCase_chars_chars(StringCase[], char*);

#endif
