/* This file is not intended to be flashed on the board */

#include "timekit.h"
#include <stdio.h>
#include <string.h>

int main(void) {

  clocktime t;
  t.hours = 0;
  t.minutes = 0;
  t.seconds = 0;
  t.ms = 0;
  t.mode = TK_PRINT_MODE_HOURS_MINUTES_SECONDS_MS;

  clocktime t2;

  printf("Testing valid increment test cases\n");
  for (int i=0; i<=24*2; i++) {
    t2 = t;
    TKIncrementHours(&t, i);
    printf("%s + %2i hours = %s\n", TKToString(&t2), i, TKToString(&t));
    if(TKValidateTime(&t) != TK_TIME_VALID) {
      printf("Error in valid hours increment\n");
    }
  }
  
  for (int i=0; i<=60*2; i++) {
    t2 = t;
    TKIncrementMinutes(&t, i);
    printf("%s + %2i minutes = %s\n", TKToString(&t2), i, TKToString(&t));
    if(TKValidateTime(&t) != TK_TIME_VALID) {
      printf("Error in valid minutes increment\n");
    }
  }
  
  for (int i=0; i<=60*2; i++) {
    t2 = t;
    TKIncrementSeconds(&t, i);
    printf("%s + %2i seconds = %s\n", TKToString(&t2), i, TKToString(&t));
    if(TKValidateTime(&t) != TK_TIME_VALID) {
      printf("Error in valid seconds increment\n");
    }
  }
  
  for (int i=0; i<=100*2; i++) {
    t2 = t;
    TKIncrementMS(&t, i);
    printf("%s + %2i ms = %s\n", TKToString(&t2), i, TKToString(&t));
    if(TKValidateTime(&t) != TK_TIME_VALID) {
      printf("Error in valid ms increment\n");
    }
  }

  printf("Testing valid decrement test cases\n");
  for (int i=0; i<=24*2; i++) {
    t2 = t;
    TKIncrementHours(&t, -i);
    printf("%s - %2i hours = %s\n", TKToString(&t2), i, TKToString(&t));
    if(TKValidateTime(&t) != TK_TIME_VALID) {
      printf("Error in valid hours decrement\n");
    }
  }

  for (int i=0; i<=60*2; i++) {
    t2 = t;
    TKIncrementMinutes(&t, -i);
    printf("%s - %2i minutes = %s\n", TKToString(&t2), i, TKToString(&t));
    if(TKValidateTime(&t) != TK_TIME_VALID) {
      printf("Error in valid minutes decrement\n");
    }
  }

  for (int i=0; i<=60*2; i++) {
    t2 = t;
    TKIncrementSeconds(&t, -i);
    printf("%s - %2i seconds = %s\n", TKToString(&t2), i, TKToString(&t));
    if(TKValidateTime(&t) != TK_TIME_VALID) {
      printf("Error in valid seconds decrement\n");
    }
  }

  for (int i=0; i<=100*2; i++) {
    t2 = t;
    TKIncrementMS(&t, -i);
    printf("%s - %2i ms = %s\n", TKToString(&t2), i, TKToString(&t));
    if(TKValidateTime(&t) != TK_TIME_VALID) {
      printf("Error in valid ms decrement\n");
    }
  }

}
