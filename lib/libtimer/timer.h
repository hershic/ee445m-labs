/* -*- mode: c; c-basic-offset: 4; -*- */
/*!
 * \brief Schedule a task (function) to be invoked periodically.
 * \param task A pointer to the function to execute every \period milliseconds
 * \param period_in_ms Number of milliseconds to wait before invoking
 * \task repeatedly
 * \param priority The value to be specified in the NVIC for this thread (task)
 * \param timer_peripheral The timer device (peripheral) to use for scheduling
 * \returns TODO
 * \note Note that this function name was chosen by the class
 * administrators. This name has been the source of some confusion;
 * namely the fact that the function name refers to a thread and the
 * function argument refers to a task. This function actually takes a
 * task (a function pointer, not a fully-fledged thread) and invokes
 * said function on the designated schedule. No true threads exist in
 * this context.
 */

/* TODO: allow subscribers */

typedef enum {TIMER0, TIMER1, TIMER2} utimer_t;

int timer_add_periodic_thread(void(*task)(void),
			      unsigned long frequency,
			      unsigned long priority,
			      utimer_t timer_peripheral);
static void (*_task0)(void);
static void (*_task1)(void);
static void (*_task2)(void);
