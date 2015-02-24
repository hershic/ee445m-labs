/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave 2015-02-08 */
/* Revision History: Look in Git FGT */

#include "os.h"
#include "libstd/nexus.h"
#include "libut/utlist.h"

/*! An array of statically allocated threads. */
static tcb_t OS_THREADS[OS_MAX_THREADS];

/*! A block of memory for each thread's local stack. */
static int32_t OS_PROGRAM_STACKS[OS_MAX_THREADS][OS_STACK_SIZE];

void os_threading_init() {

    uint32_t i;
    os_running_threads = NULL;

    for (i=0; i<OS_MAX_THREADS; ++i) {
        CDL_PREPEND(os_dead_threads, &OS_THREADS[i]);
        OS_THREADS[i].sp = OS_PROGRAM_STACKS[i];
        OS_THREADS[i].id = i;
        OS_THREADS[i].entry_point = NULL;
        OS_THREADS[i].status = THREAD_DEAD;
        OS_THREADS[i].sleep_timer = 0;
    }
}

tcb_t* os_add_thread(task_t task) {

    int32_t atom;
    tcb_t* thread_to_add;

    /* 1. Disable interrupts and save the priority mask */
    /* atomic ( */
        /* 2. Pop the task from the dead_thread pile and add it to the
         * list of running threads. */
        thread_to_add = os_dead_threads;
        CDL_DELETE(os_dead_threads, thread_to_add);
        CDL_PREPEND(os_running_threads, thread_to_add);

        /* 3. Set the initial stack contents for the new thread. */
        os_reset_thread_stack(thread_to_add, task);

        /* 4. Set metadata for this thread's TCB. */
        thread_to_add->status = THREAD_RUNNING;
        thread_to_add->sleep_timer = 0;
        thread_to_add->entry_point = task;
    /* ) */
    /* 5. Return. */
    return thread_to_add;
}

tcb_t* os_remove_thread(task_t task) {

    int32_t status ;
    tcb_t* thread_to_remove;

    /* 1. Disable interrupts and save the priority mask */
    status = StartCritical();

    /* 2. Pop the task from the running_thread pile and add it to the
     * list of dead threads. */
    thread_to_remove = os_tcb_of(task);
    CDL_DELETE(os_running_threads, thread_to_remove);
    /* Means high tcb_t memory reusability */
    CDL_PREPEND(os_dead_threads, thread_to_remove);

    /* OPTIONAL TODO: do the check for overwritten stacks as long as
     * we kill every thread in our testing we'll get valuable yet
     * unobtrusive debugging data. also consider a check on the
     * immutable fields, using the offset from the array base as
     * verification. */

    /* 3. Reset metadata for this thread's TCB. */
    thread_to_remove->status = THREAD_DEAD;
    thread_to_remove->entry_point = NULL;

    /* 4. Return. */
    EndCritical(status);
    return thread_to_remove;
}

void os_remove_thread_and_switch(task_t task) {
    if (os_remove_thread(task)) {
        IntPendSet(FAULT_PENDSV);
    }
}

int32_t os_running_thread_id() {

    return os_running_threads->id;
}

tcb_t* os_tcb_of(const task_t task) {

    int32_t i;
    for(i=0; i<OS_MAX_THREADS; ++i) {
        if (task == OS_THREADS[i].entry_point) {
            return &OS_THREADS[i];
        }
    }
    /* TODO: create a compile-time flag to catch all scary
     * situations, or let them go. #DANGER_ZONE */
    postpone_death();
    return NULL;
}

void os_launch() {

    /* acquire the pointer to the stack pointer here */
    asm volatile("LDR     R0, =os_running_threads");

    /* acquire the current value of the stack pointer */
    asm volatile("LDR R0, [R0]");
    asm volatile("LDR R0, [R0]");

    /* set the process stack value */
    asm volatile("MSR PSP, R0");

    /* change EXC_RETURN for return on PSP */
    /* asm volatile("ORR LR, LR, #4"); */

    /* change the active stack to use psp */
    asm volatile("MRS R0, CONTROL");
    asm volatile("ORR R0, R0, #2");
    asm volatile("MSR CONTROL, R0");

    asm volatile("POP     {R4-R11}");

    asm volatile("POP     {LR}");
    asm volatile("POP     {R0-R3}");

    asm volatile("pop {r12, lr}");
    asm volatile("pop {pc}");

    asm volatile ("BX LR");

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
    hwcontext->psr = 0x01000000;

    swcontext->lr = 0xfffffffd;

    /* hwcontext->r0 = 0x00000000; */
    /* hwcontext->r1 = 0x01010101; */
    /* hwcontext->r2 = 0x02020202; */
    /* hwcontext->r3 = 0x03030303; */

    /* swcontext->r4 = 0x04040404; */
    /* swcontext->r5 = 0x05050505; */
    /* swcontext->r6 = 0x06060606; */
    /* swcontext->r7 = 0x07070707; */
    /* swcontext->r8 = 0x08080808; */
    /* swcontext->r9 = 0x09090909; */
    /* swcontext->r10 = 0x10101010; */
    /* swcontext->r11 = 0x11111111; */

    tcb->sp = (uint32_t*)(((uint32_t)hwcontext) - sizeof(swcontext_t));
    asm volatile ("PUSH {R9, R10, R11, R12}");
    asm volatile ( "mrs     r12, psp" );
    asm volatile ( "mrs     r11, msp" );
    asm volatile ( "mrs     r10, control" );

    asm volatile ("POP {R9, R10, R11, R12}");
}

/*! \warning Ensure you have something to run before enabling
 *  SysTick */
void SysTick_Handler() {

    IntPendSet(FAULT_PENDSV);
    /* todo: allow for swappable schedulers.
     * esc's plan:
     * here call a method, something like os_reschedule_tasks

     * - build a new list with CDL_PREPEND, prepending tasks in order
         of lowest priority to highest. this will reassign all *next,
         *prev pointers and when we do the unmodified PendSV_Handler
         it'll grab not the round-robin *next ptr but the
         just-calculated next thread to grant foreground.

     * notes: still seems a little sub-optimal to me so we'll put a
     * pin in it, let our subconsciousness do the designing, continue
     * looking for examples and confer in person later.
     */

    return;
}

void PendSV_Handler() {

    asm volatile("CPSID  I            ;// mask all (except faults)\n");

    /* -------------------------------------------------- */
    /* phase 1: store context                             */
    /* -------------------------------------------------- */

    /* load the psp of thread A into r12 */
    asm volatile("mrs     r12, psp" );

    /* save thread A's registers into the psp */
    asm volatile("stmdb   r12!, {r4 - r11, lr}");

    /* -------------------------------------------------- */
    /* phase 2: os_running_threads manipulation    */
    /* -------------------------------------------------- */

    /* load the value of os_running_threads */
    asm volatile("LDR     R2, =os_running_threads");

    /* r3 = *os_running_threads, of thread A */
    asm volatile("LDR     R3, [R2]");

    /* load the value of os_running_threads->next into r1 */
    asm volatile("LDR     R1, [R3,#4]");

    /* os_running_threads = os_running_threads->next */
    asm volatile("STR     R1, [R2]");

    /* -------------------------------------------------- */
    /* phase 3: load context                              */
    /* -------------------------------------------------- */

    /* store the psp from thread A */
    asm volatile("str     r12, [r3, #0]");

    /* load thread B's psp */
    asm volatile("ldr     r12, [r1]");

    /* load thread B's context */
    asm volatile("ldmia   r12!, {r4 - r11, lr}");

    /* put thread B's psp into the arch psp register */
    asm volatile("msr     psp, r12");

    /* reenable interrupts */
    asm volatile("CPSIE   I");

    /* This never would have worked- can't change CONTROL in this
     * context */
    /* asm volatile("MRS R0, CONTROL"); */
    /* asm volatile("ORR R0, R0, #3"); */
    /* asm volatile("MSR CONTROL, R0"); */

    asm volatile ("bx lr");
}

/* returns the os_dead_threads */
tcb_t* os_suspend() {

    return NULL;
}
