/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Eric Crosson 2015-03-07 */
/* Revision history: Look in Git FGT */
#ifndef __EDF__
#define __EDF__

/*! \addtogroup
 * @{
 */

#include "priority_schedule_structures.h"

/*! Initialize the Earliest Deadline First task queue */
void EDF_init();

/*! Add by insertion sort the specified task into the EDF queue */
void EDF_insert();

#endif

/* End Doxygen group
 * @}
 */
