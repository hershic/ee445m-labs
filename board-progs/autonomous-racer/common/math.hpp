/* -*- mode: c++; c-basic-offset: 4; -*- */
#ifndef __math__
#define __math__

#include <stdint.h>

/*! \addtogroup math
 * @{
 */

int32_t clamp(int32_t value, int32_t min, int32_t max);
int32_t floor(int32_t value, int32_t scaling);
int32_t ceil(int32_t value, int32_t scaling);
int32_t max(int32_t v1, int32_t v2);
int32_t abs(int32_t val);

#endif  /* __math__ */

/*! End doxygen group
 * @}
 */
