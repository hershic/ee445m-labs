/* -*- mode: c; c-basic-offset: 4; -*- */

#include <stdint.h>
#include <stdbool.h>
#include "libstd/nexus.h"

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
#define OS_MAX_THREADS  3        /* maximum number of threads */
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
    int32_t id;

    /*! state of this thread */
    tstate_t status;

    /*! sleep timer of the thread */
    int32_t sleep_timer;

    /*! priority of the thread */
    int8_t priority;
} tcb_t;

/*! \brief resets the stack for a particular thread */
/*! \param thread the thread whose stack is to be reset */
void os_reset_thread_stack(tcb_t* thread);

tcb_t* os_add_thread(void(*task)(void));


/*! \brief a do-nothing idle thread */
void idle();
