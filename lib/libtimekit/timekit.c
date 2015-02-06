/* timekit.c
 * Hershal Bhave and Eric Crosson
 * 2014-02-04
 * TimeKit framework for time representation
 * Lab 3
 * Last Revision: LOOK IN GIT FGT
 * LM3S1968- pinless
 */

#include <stdlib.h>
#include <stdio.h>
#include "timekit.h"

clocktime* TKCreateTimeHandle(char mode) {
  clocktime* t = (clocktime*)malloc(sizeof(clocktime));
  t->hours = 0;
  t->minutes = 0;
  t->seconds = 0;
  t->ms = 0;
  t->mode = mode;
  return t;
}

void TKDestroy(clocktime* tm) {
  free(tm);
}

// Return the human-readable string of tm's internal time value.
// Input:  Pointer to time struct
// Output: Pointer to start of human-readable string
char* TKToString(clocktime* tm) {

  switch(tm->mode) {
  case TK_PRINT_MODE_HOURS_MINUTES:
    sprintf(tm->printBuffer, "%0.2d:%0.2d", tm->hours, tm->minutes);
    break;
  case TK_PRINT_MODE_HOURS_MINUTES_SECONDS:
    sprintf(tm->printBuffer, "%0.2d:%0.2d:%0.2d",
	    tm->hours, tm->minutes, tm->seconds);
    break;
  case TK_PRINT_MODE_HOURS_MINUTES_SECONDS_MS:
    sprintf(tm->printBuffer, "%0.2d:%0.2d:%0.2d:%0.3d",
	    tm->hours, tm->minutes, tm->seconds, tm->ms);
    break;
  default:
    printf("ERROR OCCURRED IN toString!!");
    break;
  }
  return tm->printBuffer;
}

// Increment value of tm's hours
// Input:  tm to increment, hours to increment by
// Output: void
void TKIncrementHours(clocktime* tm, short hours) {

  short tmpHours = tm->hours + hours;

	while ((1U * tmpHours) >= TK_NUM_HOURS) {
		tmpHours %= TK_NUM_HOURS;
	}
 
  tm->hours = tmpHours;
}

// Increment the value of tm's minutes
// Input:  tm to increment, minutes to increment by
// Output: void
void TKIncrementMinutes(clocktime* tm, short minutes) {
  short tmpMinutes = tm->minutes + minutes;

  if(minutes > 0) {
    while (tmpMinutes >= TK_NUM_MINUTES) {
      tmpMinutes -= TK_NUM_MINUTES;
      TKIncrementHours(tm, 1);
    }
  } else {
    while (tmpMinutes < 0) {
      tmpMinutes += TK_NUM_MINUTES;
      TKIncrementHours(tm, -1);
    }
  }
  tm->minutes = tmpMinutes;
}

// Increment the value of tm's seconds
// Input:  tm to increment, seconds to increment by
// Output: void
void TKIncrementSeconds(clocktime* tm, short seconds) {
  short tmpSeconds = tm->seconds + seconds;

  if(seconds > 0) {
  while (tmpSeconds >= TK_NUM_SECONDS) {
    tmpSeconds -= TK_NUM_SECONDS;
    TKIncrementMinutes(tm, 1);
  }
  } else {
    while (tmpSeconds < 0) {
      tmpSeconds += TK_NUM_SECONDS;
      TKIncrementMinutes(tm, -1);
  }
}
  tm->seconds = tmpSeconds;
}

// Increment the value of tm's miliseconds
// Input:  tm to increment, ms to increment by
// Output: void
void TKIncrementMS(clocktime* tm, short ms) {
  short tmpMs = tm->ms + ms;

  if(ms > 0) {
    while (tmpMs >= TK_NUM_MS) {
      tmpMs -= TK_NUM_MS;
      TKIncrementSeconds(tm, 1);
    }
  } else {
    while (tmpMs < 0) {
      tmpMs += TK_NUM_MS;
      TKIncrementSeconds(tm, -1);
    }
  }

  tm->ms = tmpMs;
}

// Verify that the values inside of alleged time object tm are values
// possible to see on a clock.
// Input:  time object tm
// Output: either TK_TIME_VALID or not
short TKValidateTime(clocktime* tm) {
  if(tm->hours > TK_NUM_HOURS) {
    return TK_TIME_INVALID_HOURS;
  } else if(tm->minutes > TK_NUM_MINUTES) {
    return TK_TIME_INVALID_MINUTES;
  } else if((tm->mode > 0) && (tm->seconds > TK_NUM_SECONDS)) {
    return TK_TIME_INVALID_SECONDS;
  } else if((tm->mode > 1) && (tm->ms > TK_NUM_MS)) {
    return TK_TIME_INVALID_MS;
  }
  return TK_TIME_VALID;
}

// Reset a time object back to zero.
// Input:  time object tm
// Output: void
void TKCleanupTime(clocktime* tm) {
  short error = TKValidateTime(tm);

  switch(error) {
  case TK_TIME_VALID:
      break;
  case TK_TIME_INVALID_HOURS:
    tm->hours = 0;
    break;
  case TK_TIME_INVALID_MINUTES:
    tm->minutes = 0;
    break;
  case TK_TIME_INVALID_SECONDS:
    tm->seconds = 0;
    break;
  case  TK_TIME_INVALID_MS:
    tm->ms = 0;
    break;
  default:
    break;
  }
}

clocktime* TKDuplicateTimeHandle(clocktime* th) {

  clocktime* handle = (clocktime*)malloc(sizeof(clocktime));

  handle->hours = th->hours;
  handle->minutes = th->minutes;
  handle->seconds = th->seconds;
  handle->ms = th->ms;
  handle->mode = th->mode;

  /* Don't copy the printbuffer! */

  return handle;
}
