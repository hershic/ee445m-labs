/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-02-28 */
/* Revision history: Look in Git FGT */
#ifndef __jitter__
#define __jitter__

#include <stdint.h>
#include <stdbool.h>

/*! \addtogroup OS
   * @{
''    */

#define PIDWORK_BUFFER_SIZE 32

static volatile uint32_t PIDWORK;
static volatile uint32_t HIGHEST_PIDWORK;
static volatile uint32_t LOWEST_PIDWORK;
static volatile uint32_t PIDWORK_IDX;
static volatile uint32_t PIDWORK_BUFFER[PIDWORK_BUFFER_SIZE];

void pidwork_increment();
void pidwork_record();

#endif  /* __jitter__ */

/* End Doxygen group
    * @}
     */
