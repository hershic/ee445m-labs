/* -*- mode: c; c-basic-offset: 4; -*- */

#include "libtimer/timer.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"
#include "inc/hw_types.h"

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

#include "driverlib/pin_map.h"
#include "driverlib/gpio.h"
#include "driverlib/uart.h"

#include "utils/ustdlib.h"

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
#define NUMTHREADS  3        /* maximum number of threads */
#define STACKSIZE   100      /* number of 32-bit words in stack */

typedef enum {
    /* thread is not running and will not run */
    THREAD_DEAD,

    /* thread is waiting and will run when the timer runs out */
    THREAD_SLEEPING,

    /* thread is running */
    THREAD_RUNNING,

    /* thread is not running but will run eventually */
    THREAD_ACTIVE
} tstate_t;

typedef struct thread {
    /*! pointer to stack (valid for threads not running */
    int32_t *sp;

    /*! numerical identifier for the thread */
    /* DO NOT MODIFY THIS VARIABLE */
    int32_t thread_id;

    /*! linked-list pointer to next thread */
    struct thread *next_thread;

    /*! state of this thread */
    tstate_t status;

    /* int32_t sleep_timer; */
    /* int8_t priority; */
} thread_t;

/* This data structure was stolen from Valvano website
   http://users.ece.utexas.edu/~valvano/arm/os.c */
/*! \brief  */
typedef enum {
    R_THUMB = STACKSIZE-1,
    R_PC = STACKSIZE-2,
    R_R0 = STACKSIZE-8,
    R_R1 = STACKSIZE-7,
    R_R2 = STACKSIZE-6,
    R_R3 = STACKSIZE-5,
    R_R4 = STACKSIZE-16,
    R_R5 = STACKSIZE-15,
    R_R6 = STACKSIZE-14,
    R_R7 = STACKSIZE-13,
    R_R8 = STACKSIZE-12,
    R_R9 = STACKSIZE-11,
    R_R10 = STACKSIZE-10,
    R_R11 = STACKSIZE-9,
    R_R12 = STACKSIZE-4,
    R_R14 = STACKSIZE-3
} stack_reg;

/*! \brief resets the stack for a particular thread */
/*! \param thread the thread whose stack is to be reset */
void os_reset_thread_stack(thread_t* thread);

thread_t* os_add_thread(void(*task)(void));
thread_t* os_first_dead_thread();
thread_t* os_next_active_thread(thread_t* from_thread);

/*! \brief sets the pc for a thread to a specified function */
/*! \param thread the thread whose pc is to be set */
/*! \param task the function to set the thread's pc to */
void os_set_thread_pc(thread_t* thread, void(*task)(void));

/*! \brief a do-nothing idle thread */
void idle();
