#include "StringCase.h"

/* TODO: templatize or macro-ize */

#define NDEBUG

#ifndef NDEBUG
#include <stdio.h>
#endif

#ifndef NULL
#define NULL 0
#endif

char* StringCase_chars_chars(StringCase cases[],
			     char* match_string) {

  StringCase* pcase;
  for(pcase = cases;
      pcase != NULL && \
	pcase != cases + sizeof(cases) / sizeof(cases[0]) + 1;
      ++pcase) {
    if (streql(pcase->string, match_string)) {
#ifndef NDEBUG
      printf("Found a match!: %s\n", pcase->string);
#endif
      return (*pcase->function)();
    }
  }
#ifndef NDEBUG
  printf("%s: no matches found\n", __FUNCTION__);
#endif
}
