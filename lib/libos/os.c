/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave 2015-02-08 */
/* Revision History: Look in Git FGT */

#include "os.h"

#include "inc/hw_nvic.h"
#include "inc/hw_types.h"

#include "libstd/nexus.h"
#include "libut/utlist.h"
#include "libsystick/systick.h"
#include "libschedule/schedule.h"

/*! A block of memory for each thread's local stack. */
static int32_t OS_PROGRAM_STACKS[SCHEDULER_MAX_THREADS][OS_STACK_SIZE];

void os_threading_init() {

    uint32_t i;
    os_running_threads = NULL;

    /* Initialize thread metadata */
    for (i=0; i<SCHEDULER_MAX_THREADS; ++i) {
        CDL_APPEND(os_dead_threads, &OS_THREADS[i]);
        OS_THREADS[i].sp = OS_PROGRAM_STACKS[i];
        OS_THREADS[i].id = i;
        OS_THREADS[i].entry_point = NULL;
        /* OS_THREADS[i].status = THREAD_DEAD; */
        /* OS_THREADS[i].sleep_timer = 0; */
    }

    /* Initialize thread pool metadata */
    for (i=0; i<OS_NUM_POOLS; ++i) {
        OS_THREAD_POOL[i] = (tcb_t*) NULL;
    }

    schedule_init();
}

tcb_t* os_add_thread(task_t task) {

    tcb_t* thread_to_add;

    /* 1. Disable interrupts and save the priority mask */
    atomic_start();

    /* 2. Pop the task from the dead_thread pile */
    thread_to_add = os_dead_threads;
    CDL_DELETE(os_dead_threads, thread_to_add);

    /* 3. Add the thread to the appropriate priority pool. */
    /* CDL_APPEND(os_running_threads, thread_to_add); */
    /* CDL_APPEND(OS_THREAD_POOL[priority], thread_to_add); */

    /* 4. Set the initial stack contents for the new thread. */
    _os_reset_thread_stack(thread_to_add, task);

    /* 5. Set metadata for this thread's TCB. */

    /* TODO: Remove this from semaphore. The scheduler manages
     * this. */
    /* thread_to_add->status = THREAD_RUNNING; */
    thread_to_add->entry_point = task;
    /* thread_to_add->priority = priority; */
    /* thread_to_add->sleep_timer = 0; */

    atomic_end();

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
    CDL_DELETE(OS_THREAD_POOL[thread_to_remove->priority], thread_to_remove);
    /* Append means low tcb_t memory reusability. Prepend means high */
    CDL_APPEND(os_dead_threads, thread_to_remove);

    /* OPTIONAL TODO: do the check for overwritten stacks as long as
     * we kill every thread in our testing we'll get valuable yet
     * unobtrusive debugging data. also consider a check on the
     * immutable fields, using the offset from the array base as
     * verification. */

    /* 3. Reset metadata for this thread's TCB. */
    /* thread_to_remove->status = THREAD_DEAD; */
    thread_to_remove->entry_point = NULL;
    thread_to_remove->next = NULL;
    thread_to_remove->prev = NULL;

    /* 4. Return. */
    EndCritical(status);
    return thread_to_remove;
}

int32_t os_running_thread_id() {

    return os_running_threads->id;
}

tcb_t* os_tcb_of(const task_t task) {

    int32_t i;
    for(i=0; i<SCHEDULER_MAX_THREADS; ++i) {
        if (task == OS_THREADS[i].entry_point) {
            return &OS_THREADS[i];
        }
    }
#ifndef DANGER_ZONE
    postpone_death();
#endif
    return NULL;
}

void os_launch() {

    edf_init();
    _os_choose_next_thread();
    os_running_threads = OS_NEXT_THREAD;

    /* acquire the pointer to the stack pointer here */
    asm volatile("LDR     R0, =OS_NEXT_THREAD");

    /* acquire the current value of the stack pointer */
    asm volatile("LDR R0, [R0]");
    asm volatile("LDR R0, [R0]");

    /* set the process stack value */
    asm volatile("MSR MSP, R0");

    /* change EXC_RETURN for return on MSP */
    /* asm volatile("ORR LR, LR, #4"); */

    /* change the active stack to use msp */
    /* asm volatile("MRS R0, CONTROL"); */
    /* asm volatile("ORR R0, R0, #2"); */
    /* asm volatile("MSR CONTROL, R0"); */

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

always inline
void _os_reset_thread_stack(tcb_t* tcb, task_t task) {

    hwcontext_t* hwcontext = (hwcontext_t*)
        (((uint32_t)OS_PROGRAM_STACKS[tcb->id]) +
         sizeof(uint32_t)*OS_STACK_SIZE - sizeof(hwcontext_t));

    swcontext_t* swcontext = (swcontext_t*)
        (((uint32_t)hwcontext) - sizeof(swcontext_t));

    hwcontext->pc = (uint32_t)(task);
    hwcontext->psr = 0x01000000;

    swcontext->lr = 0xfffffff9;

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
    asm volatile ( "mrs     r12, msp" );
    asm volatile ( "mrs     r11, msp" );
    asm volatile ( "mrs     r10, control" );

    asm volatile ("POP {R9, R10, R11, R12}");
}

/*! \warning Ensure you have something to run before enabling
 *  SysTick */
void SysTick_Handler() {

    /* Queue the PendSV_Handler after this ISR returns */
    IntPendSet(FAULT_PENDSV);
}

void PendSV_Handler() {

    asm volatile("CPSID  I            ;// mask all (except faults)\n");

    /* -------------------------------------------------- */
    /* phase 1: store context                             */
    /* -------------------------------------------------- */

    /* load the msp of thread A into r12 */
    asm volatile("mrs     r12, msp" );

    /* save thread A's registers into the msp */
    asm volatile("stmdb   r12!, {r4 - r11, lr}");

    /* -------------------------------------------------- */
    /* phase 2: os_running_threads manipulation    */
    /* -------------------------------------------------- */

    _os_choose_next_thread();
    HWREG(NVIC_ST_CURRENT) = 0;
    _os_reset_thread_stack(os_running_threads, os_running_threads->entry_point);

    /* load the value of os_running_threads */
    asm volatile("LDR     R2, =os_running_threads");

    /* r3 = *os_running_threads, of thread A */
    asm volatile("LDR     R3, [R2]");

    /* load the value of os_running_threads->next into r1 */
    asm volatile("LDR     R1, =OS_NEXT_THREAD");
    asm volatile("LDR     R1, [R1]");

    /* os_running_threads = OS_NEXT_THREAD */
    asm volatile("STR     R1, [R2]");

    /* -------------------------------------------------- */
    /* phase 3: load context                              */
    /* -------------------------------------------------- */

    /* store the msp from thread A */
    asm volatile("str     r12, [r3, #0]");

    /* load thread B's msp */
    asm volatile("ldr     r12, [r1]");

    /* load thread B's context */
    asm volatile("ldmia   r12!, {r4 - r11, lr}");

    /* put thread B's msp into the arch msp register */
    asm volatile("msr     msp, r12");

    /* reenable interrupts */
    asm volatile("CPSIE   I");

    asm volatile ("bx lr");
}

/*! Put the invoking thread to sleep and let another thread take
 *  over. This s another way to say "set the interrupt bit of the
 *  \PendSV_Handler". */
void os_suspend() {

    IntPendSet(FAULT_PENDSV);
    /* TODO: penalize long threads, reward quick threads */
    while (1) {}
}

/* TODO: implement the edf queue of queues */
/*! \pre disable interrupts before we get here */
void _os_choose_next_thread() {
    uint8_t pool = 0;

    /* from the old priority scheduler init */
    /* tcb_t* next_thread = _os_pool_waiting(pool); */

    sched_task* next_task = edf_get_edf_queue();
    HWREG(NVIC_ST_RELOAD) = next_task->absolute_deadline - 1;

    tcb_t* next_tcb = edf_pop();
    OS_NEXT_THREAD = next_tcb;


}
