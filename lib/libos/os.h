/* -*- mode: c; c-basic-offset: 4; -*- */

#include "libstd/nexus.h"
#include "inc/hw_ints.h"

#include <stdint.h>
#include <stdbool.h>

/* TODO: Make this doxygen output match libtimer's */

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
#define OS_MAX_THREADS  4        /* maximum number of threads */
#define OS_STACK_SIZE   100      /* number of 32-bit words in stack */

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

typedef void (*task_t)();

/*! \brief Thread Control Block */
typedef struct tcb {
    /*! pointer to stack (valid for threads not running */
    int32_t *sp;

    /*! linked-list pointer to next tcb */
    struct tcb *next;

    /*! linked-list pointer to prev tcb */
    struct tcb *prev;

    /*! numerical identifier for the tcb */
    /*! THIS PROPERY IS IMMUTABLE */
    immutable int32_t id;

    /*! state of this thread */
    tstate_t status;

    /*! sleep timer of the thread */
    int32_t sleep_timer;

    /*! priority of the thread */
    /* int8_t priority; */
} tcb_t;

/*! The contents of the stack immediately after a function call. */
typedef struct hwcontext {
    uint32_t r0;
    uint32_t r1;
    uint32_t r2;
    uint32_t r3;
    uint32_t r12;
    uint32_t lr;
    uint32_t pc;
    uint32_t psr;
} hwcontext_t;

/*! The contents of the stack during a context switch. */
typedef struct swcontext {
    uint32_t r4;
    uint32_t r5;
    uint32_t r6;
    uint32_t r7;
    uint32_t r8;
    uint32_t r9;
    uint32_t r10;
    uint32_t r11;
    uint32_t lr;
} swcontext_t;

/*! Resets the thread stack for a given tcb to run a given task
 *  \param thread the thread whose stack is to be reset
 *  \param the task for which the thread should run
 */
void os_reset_thread_stack(tcb_t* tcb, task_t task);

/*! Adds a new thread with the specified task.
 *  \returns the TCB of the newly added thread, null if the addition
 *  was not possible for some reason.
 */
tcb_t* os_add_thread(void(*task)(void));

/*! Returns the next dead thread in the dead thread circle.
 *  If the length of the dead thread circle is greater than 1, then
 *  the current dead thread is removed and the circle is relinked with
 *  the next dead thread. If the circle contains only one dead thread,
 *  then that thread is removed and the current dead thread is set to
 *  null. If there is no thread in the dead thread circle, then it
 *  retuns null.
 *  \returns The first thread removed from the dead thread circle, or
 *  null if the dead thread circle is empty.
 */
tcb_t* os_next_dead_thread();

/*! Initialize the threading engine, setting all threads to dead. This
 *  initializes the dead thread circle appropriately and sets the
 *  running thread circle to null.
 */
void os_threading_init();

void os_launch();


/*! \brief a do-nothing idle thread */
void idle();
