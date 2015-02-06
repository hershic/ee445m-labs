/* -*- mode: c; c-basic-offset: 4; -*- */
#include "../libtimer/timer.h"
/* Make this doxygen output match libtimer's */

/*!
 * \brief Schedule a task (function) to be invoked periodically.
 * \param task A pointer to the function to execute every \period milliseconds
 * \param period Number of milliseconds to wait before invoking \task repeatedly
 * \param priority The value to be specified in the NVIC for this thread (task).
 * \returns TODO
 * \note Note that this function name was chosen by the class
 * administrators. This name has been the source of some confusion;
 * namely the fact that the function name refers to a thread and the
 * function argument refers to a task. This function actually takes a
 * task (a function pointer, not a fully-fledged thread) and invokes
 * said function on the designated schedule. No true threads exist in
 * this context.
 */
inline
int OS_AddPeriodicThread(void(*task)(void),
			 unsigned long period,
			 unsigned long priority) {

  timer_add_periodic_thread(task, period, priority);
}

/*!
 * \brief Clear the current 32-bit global timer counter.
 * \returns void
 */
inline
void OS_ClearPeriodicTime(void) {

  /* TODO: implement */
}

/*!
 * \brief Return the current 32-bit global timer counter.
 * \returns unsigned long The current 32-bit global counter.
 * \note The units of this system time are the period of interrupt
 * passed in by the user when initializing with OS_AddPeriodicThread.
 */
inline
unsigned long OS_ReadPeriodicTime(void) {

  /* TODO: implement */
}
