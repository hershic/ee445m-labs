/* -*- mode: c; c-basic-offset: 4; -*- */
#include "os.h"

/*! An array of statically allocated threads. */
tcb_t OS_THREADS[OS_MAX_THREADS];

/*! A doubly linked list of currently running threads. */
tcb_t* os_current_running_thread = NULL;

/*! A doubly linked list of currently dead threads. */
tcb_t* os_current_dead_thread = NULL;

tcb_t* os_add_thread(task_t thread) {

    int32_t status;

    status = StartCritical();
    /* 1. Set up the initial stack contents */
    
}

/*! Sets the data structures for the operating system launch */
void os_launch(task_t thread) {

    if (!os_current_running_thread) {
        os_add_thread(thread);
    }
}
