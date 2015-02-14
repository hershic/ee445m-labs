/* -*- mode: c; c-basic-offset: 4; -*- */
#include "os.h"

/*! An array of statically allocated threads. */
tcb_t OS_THREADS[OS_MAX_THREADS];

/*! A block of memory for each thread's local stack. */
int32_t OS_PROGRAM_STACKS[OS_MAX_THREADS][OS_STACK_SIZE];

/*! A doubly linked list of currently running threads. */
tcb_t* os_current_running_thread = NULL;

/*! A doubly linked list of currently dead threads. */
tcb_t* os_current_dead_thread = NULL;

void os_threading_init() {

    uint32_t i;
    os_current_dead_thread = &OS_THREADS[0];

    for (i=0; i<OS_MAX_THREADS; ++i) {
        OS_THREADS[i].sp = OS_PROGRAM_STACKS[i];
        OS_THREADS[i].next = &OS_THREADS[i+1];
        OS_THREADS[i].prev = &OS_THREADS[i-1];
        OS_THREADS[i].id = i;
        OS_THREADS[i].status = THREAD_DEAD;
        OS_THREADS[i].sleep_timer = 0;
    }

    OS_THREADS[0].prev = &OS_THREADS[OS_MAX_THREADS-1];
    OS_THREADS[OS_MAX_THREADS-1].next = &OS_THREADS[0];

    os_current_running_thread = NULL;
}

tcb_t* os_add_thread(task_t task) {

    int32_t status;
    tcb_t* thread_to_add;

    /* 1. Disable interrupts and save the priority mask */
    status = StartCritical();

    /* 2. Add the task to the linked list of running threads. */
    /* 2a. If there is no more room for running threads, then return
       immediately. */
    if (!(thread_to_add = os_next_dead_thread())) {
        EndCritical(status);
        return NULL;
    }
    /* 2b. If no thread is running, then create the initial running
       thread. */
    if (!os_current_running_thread) {
        os_current_running_thread = thread_to_add;
        os_current_running_thread->next = os_current_running_thread;
        os_current_running_thread->prev = os_current_running_thread;
    }
    /* 2c. If we don't have any problems with either thread circles,
       then remove a thread from the dead thread pool and add it to
       the running thread pool, relinking the linked list
       appropriately. This thread will run next. */
    else {
        os_current_running_thread->next->prev = thread_to_add;
        thread_to_add->next = os_current_running_thread->next;
        os_current_running_thread->next = thread_to_add;
        thread_to_add->prev = os_current_running_thread;
    }

    /* 3. Set the initial stack contents for the new thread. */
    os_reset_thread_stack(thread_to_add, task);

    /* 4. Set other metadata for this thread's TCB. */
    thread_to_add->status = THREAD_RUNNING;
    thread_to_add->sleep_timer = 0;

    /* 5. Return. */
    EndCritical(status);
}

tcb_t* os_next_dead_thread() {

    tcb_t* return_tcb = os_current_dead_thread;

    if (!return_tcb) { return return_tcb; }

    if ((os_current_dead_thread->next == os_current_dead_thread) &&
        (os_current_dead_thread->prev == os_current_dead_thread)) {
        return_tcb = os_current_dead_thread;
        os_current_dead_thread = NULL;
        return return_tcb;
    }

    os_current_dead_thread->next->prev = os_current_dead_thread->prev;
    os_current_dead_thread->prev->next = os_current_dead_thread->next;
    os_current_dead_thread = os_current_dead_thread->next;
    return return_tcb;
}

/*! Sets the data structures for the operating system launch */
void os_launch(task_t thread) {

    if (!os_current_running_thread) {
        os_add_thread(thread);
    }
}

void os_reset_thread_stack(tcb_t* tcb, task_t task) {
    /* TODO: set the stack vars here */
}
