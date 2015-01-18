#ifndef __TIMEKIT__
#define __TIMEKIT__

/* timekit.h
 * Hershal Bhave and Eric Crosson
 * 2013-02-04
 * TimeKit framework for time representation
 * Lab 3
 * Last Revision: LOOK IN GIT FGT
 * LM3S1968- pinless
 */

#define TK_MAX_PRINTBUFFER_LEN 20

#define TK_PRINT_MODE_HOURS_MINUTES			0
#define TK_PRINT_MODE_HOURS_MINUTES_SECONDS		1
#define TK_PRINT_MODE_HOURS_MINUTES_SECONDS_MS	2

#define TK_TIME_VALID		0
#define TK_TIME_INVALID_HOURS	1
#define TK_TIME_INVALID_MINUTES 2
#define TK_TIME_INVALID_SECONDS 3
#define TK_TIME_INVALID_MS	4

#define TK_NUM_HOURS	24
#define TK_NUM_MINUTES	60
#define TK_NUM_SECONDS	60
#define TK_NUM_MS	1000

typedef struct clocktime {
  short hours;
  short minutes;
  short seconds;
  short ms;
  char mode;
  char printBuffer[TK_MAX_PRINTBUFFER_LEN];
} clocktime;

clocktime* TKCreateTimeHandle(char mode);
clocktime* TKDuplicateTimeHandle(clocktime* th);
void TKDestroy(clocktime* tm);
char* TKToString(clocktime* tm);
void TKIncrementHours(clocktime* tm, short hours);
void TKIncrementMinutes(clocktime* tm, short minutes);
void TKIncrementSeconds(clocktime* tm, short seconds);
void TKIncrementMS(clocktime* tm, short ms);
short TKValidateTime(clocktime* tm);
void TKCleanupTime(clocktime* tm);

#endif /* __TIMEKIT__ */
