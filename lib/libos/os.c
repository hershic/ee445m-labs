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
void os_launch() {

    /* acquire the pointer to the stack pointer here */
    asm volatile("LDR     R0, =os_current_running_thread");

    /* acquire the current value of the stack pointer */
    asm volatile("LDR R0, [R0]");
    asm volatile("LDR R0, [R0]");

    /* set the process stack value */
    asm volatile("MSR PSP, R0");

    /* change EXC_RETURN for return on PSP */
    asm volatile("ORR LR, LR, #4");

    /* change the active stack to use psp */
    asm volatile("MRS R0, CONTROL");
    asm volatile("ORR R0, R0, #2");
    asm volatile("MSR CONTROL, R0");

    asm volatile("POP     {R4-R11}");

    /* asm volatile("POP     {R12}"); */
    asm volatile("POP     {LR}");
    asm volatile("POP     {R0-R3}");

    asm volatile("pop {r12, lr}");
    asm volatile("pop {pc}");

    /* return from handler */
    /* asm volatile("POP {R0, R1, R2, R3, R12, LR, PC, PSR} "); */

    /* asm volatile("BX LR"); */
}

void os_reset_thread_stack(tcb_t* tcb, task_t task) {

    hwcontext_t* hwcontext = (hwcontext_t*)
        (((uint32_t)OS_PROGRAM_STACKS[tcb->id]) +
         sizeof(uint32_t)*OS_STACK_SIZE - sizeof(hwcontext_t));

    swcontext_t* swcontext = (swcontext_t*)
        (((uint32_t)hwcontext) - sizeof(swcontext_t));

    hwcontext->pc = (uint32_t)(task);
    hwcontext->psr = 0x010000000;

    swcontext->lr = 0xfffffffd;

    hwcontext->r0 = 0x00000000;
    hwcontext->r1 = 0x01010101;
    hwcontext->r2 = 0x02020202;
    hwcontext->r3 = 0x03030303;

    swcontext->r4 = 0x04040404;
    swcontext->r5 = 0x05050505;
    swcontext->r6 = 0x06060606;
    swcontext->r7 = 0x07070707;
    swcontext->r8 = 0x08080808;
    swcontext->r9 = 0x09090909;
    swcontext->r10 = 0x10101010;
    swcontext->r11 = 0x11111111;

    tcb->sp = (uint32_t*)(((uint32_t)hwcontext) - sizeof(swcontext_t));
    asm volatile ("PUSH {R9, R10, R11, R12}");
    asm volatile ( "mrs     r12, psp" );
    asm volatile ( "mrs     r11, msp" );
    asm volatile ( "mrs     r10, control" );

    asm volatile ("POP {R9, R10, R11, R12}");
}

/* NOTE: Make sure you have something to run before letting the
   SysTick run! */
void SysTick_Handler() {

    /* try 1 */
    /* uint32_t status = StartCritical(); */
    asm volatile("CPSID   I");
    asm volatile("PUSH    {R4-R11}"); /* r0-11 now saved in 'old' stack */
    asm volatile("LDR     R0, =os_current_running_thread"); /* this is a tcb */
    /* asm volatile("LDR     R1, [R0]"); */
    /* asm volatile("STR     SP, [R1]"); */
    /* asm volatile("LDR     R1, [R1,#4]"); */
    /* asm volatile("STR     R1, [R0]"); */
    /* asm volatile("LDR     SP, [R1]"); */
    /* asm volatile("POP     {R4-R11}"); */
    /* asm volatile("CPSIE   I"); */
    /* asm volatile("BX      LR"); */


    /* try 2 */
    /* something wrong with psp/msp stuff */
    /* asm volatile ( "mrs     r12, msp" ); */
    /*  */
    /* asm volatile("push    {r4 - r11}"); */
    /* asm volatile("ldmia   r12!, {r4 - r11, lr}"); */
    /* asm volatile("mrs     r12, psp"); */
    /* asm volatile("stmdb   r12!, {r4 - r11, lr}"); */
    /* asm volatile("pop     {r4 - r11}"); */
    /*  */
    /* asm volatile("LDR     R0, =os_current_running_thread"); */
    /* asm volatile("LDR     R0, [R0]"); */
    /* asm volatile("LDR     R1, [R0,#4]"); */
    /* asm volatile("STR     R1, [R0]"); */
    /*  */
    /* asm volatile("str     r12, [r1, #0]"); */
    /* asm volatile("ldr     r12, [r0, #0]"); */
    /* asm volatile("msr     msp, r11"); */
    /* asm volatile("ldmia   r12!, {r4 - r11, lr}"); */
    /* asm volatile("msr     psp, r12"); */
    /* asm volatile("bx      lr"); */

    /* /\* r2 = os_current_running_thread *\/ */
    /* asm volatile("LDR R2, [R0]");  */
    /*  */
    /* /\* r1 = os_current_running_thread->next *\/ */
    /* asm volatile("LDR R1, [R2, #4]");  */
    /*  */
    /* /\* os_current_running_thread = r1 *\/ */
    /* asm volatile("STR R1, [R0]"); */
    /*  */
    /* /\* load the pointers to the stack pointers and change the current */
    /*    running thread to the next thread *\/ */
    /* asm volatile("LDR R0, [R2]"); */
    /* /\* asm volatile("LDR R1, [R1]"); *\/ */
    /*  */
    /* /\* update the stack pointers *\/ */
    /* asm volatile("STR SP, [R0]"); */
    /* asm volatile("LDR SP, [R1]"); */
    /*  */
    /* /\* finally, pop the remaining registers *\/ */
    /* asm volatile("POP {R4-R11}"); */

    /* asm volatile("POP {r0, r1, r2, r3, r12, lr, pc, psr} "); */

    /* try 3 */
    asm volatile ("PUSH {R11, R12}");
    asm volatile ("mrs   r12, psp");
    asm volatile ("mrs   r11, msp");
    asm volatile ("POP  {R11, R12}");
    asm volatile("CPSIE I"); 	/* re-enable interrupts */

}
