/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-03-15 */
/* Revision history: Look in Git FGT */

#ifndef __complex__
#define __complex__

/*! \addtogroup Complex Static methods for complex analysis
 * @{
 */

#include <stdint.h>

class complex {
public:
    /*! Compute \x convolved with \h and store the result in \y, all
     *  arrays being of size \length. */
    static void convolve(uint32_t *x, uint32_t *h, uint32_t *y, uint32_t length);
};

#endif

/* End Doxygen group
 * @}
 */
