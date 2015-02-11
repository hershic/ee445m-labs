/* -*- mode: c; c-basic-offset: 4; -*- */
#include "os.h"

#include <stdint.h>

static int32_t PROGRAM_STACKS[NUMTHREADS][STACKSIZE];
static thread_t THREAD_LIST[NUMTHREADS];
static thread_t* RUNNING_THREAD;

bool my_idle_has_run = false;
bool my_pendsv_has_run = false;
bool locked_idle_has_run = false;

/* This data structure was stolen from Valvano website
   http://users.ece.utexas.edu/~valvano/arm/os.c */
void os_reset_thread_stack(thread_t* thread) {
    uint32_t th_id = thread->thread_id;
    THREAD_LIST[th_id].sp = &PROGRAM_STACKS[th_id][STACKSIZE-16];
    PROGRAM_STACKS[th_id][R_R0]  = 0xDEAD0000;
    PROGRAM_STACKS[th_id][R_R1]  = 0xDEAD0001;
    PROGRAM_STACKS[th_id][R_R2]  = 0xDEAD0002;
    PROGRAM_STACKS[th_id][R_R3]  = 0xDEAD0003;
    PROGRAM_STACKS[th_id][R_R4]  = 0xDEAD0004;
    PROGRAM_STACKS[th_id][R_R5]  = 0xDEAD0005;
    PROGRAM_STACKS[th_id][R_R6]  = 0xDEAD0006;
    PROGRAM_STACKS[th_id][R_R7]  = 0xDEAD0007;
    PROGRAM_STACKS[th_id][R_R8]  = 0xDEAD0008;
    PROGRAM_STACKS[th_id][R_R9]  = 0xDEAD0009;
    PROGRAM_STACKS[th_id][R_R10] = 0xDEAD0010;
    PROGRAM_STACKS[th_id][R_R11] = 0xDEAD0011;
    PROGRAM_STACKS[th_id][R_R12] = 0xDEAD0012;
    /* PROGRAM_STACKS[th_id][R_R13] = 0xDEAD0013; */
    PROGRAM_STACKS[th_id][R_R14] = 0xDEAD0014;
    PROGRAM_STACKS[th_id][R_THUMB] = 0x01000000;
}

void os_threading_init() {
    int i;

    for (i=0; i<NUMTHREADS; ++i) {
        THREAD_LIST[i].status = THREAD_DEAD;
        THREAD_LIST[i].thread_id = i;

        /* yes, I do a double-indirection here, but we're only doing
           it once and the code is much cleaner this way. */
        os_reset_thread_stack(&THREAD_LIST[i]);

        /* THREAD_LIST[i].next_thread = &THREAD_LIST[i+1]; */
    }

    /* THREAD_LIST[NUMTHREADS-1].next_thread = &THREAD_LIST[0]; */

    RUNNING_THREAD = os_add_thread(idle);
}

/* This never returns, watch out! */
void os_begin() {
    asm("LDR     R0, =RUNNING_THREAD  /* currently running thread*/  \n\t");
    asm("LDR     R2, [R0]             /* R2 = value of RunPt*/  \n\t");
    asm("LDR     SP, [R2]             /* new thread SP; SP = RunPt->stackPointer;*/  \n\t");
    asm("POP     {R4-R11}             /* restore regs r4-11*/  \n\t");
    asm("POP     {R0-R3}              /* restore regs r0-3*/  \n\t");
    asm("POP     {R12} \n\t");
    asm("POP     {LR}                 /* discard LR from initial stack*/  \n\t");
    asm("POP     {LR}                 /* start location*/  \n\t");
    asm("POP     {R1}                 /* discard PSR*/  \n\t");
    asm("CPSIE   I                    /* Enable interrupts at processor level*/  \n\t");
    asm("BX      LR                   /* start first thread*/  \n\t");
}

thread_t* os_add_thread(void(*task)(void)) {
    thread_t* thread;
    if (thread = os_first_dead_thread()) {
        os_reset_thread_stack(thread);
        os_set_thread_pc(thread, task);
        thread->status = THREAD_ACTIVE;

        return thread;
    }
    return 0;
}

thread_t* os_first_dead_thread() {
    int i;
    for (i=0; i<NUMTHREADS; ++i) {
        if (THREAD_LIST[i].status == THREAD_DEAD) {
            return &THREAD_LIST[i];
        }
    }
    return 0;
}

thread_t* os_next_active_thread(thread_t* from_thread) {
    uint32_t from_thread_id = from_thread->thread_id;
    uint32_t iter_id = (from_thread_id+1 % NUMTHREADS);

    /* This implements the round-robin algorithm */
    while (iter_id != from_thread_id) {
        iter_id = ((iter_id+1) % NUMTHREADS);
        if (THREAD_LIST[iter_id].status == THREAD_ACTIVE) {
            return &THREAD_LIST[iter_id];
        }
    }
    /* Fall-through case. This means that no other thread can
       currently be run, so run that thread again. */
    return from_thread;
}

void os_set_thread_pc(thread_t* thread, void(*task)(void)) {
    PROGRAM_STACKS[thread->thread_id][R_PC] = (int32_t)(task);
}

void os_sleep_thread(thread_t* thread_to_sleep) {
    /* TODO: Fill in */
}

void os_sleep_thread_util(thread_t* thread_to_sleep, bool until_true) {
    /* TODO: Fill in */
}

/* Do-nothing thread */
void idle() {
    while (1) {
        my_idle_has_run = true;
    }
}

void idle_until(bool* until_this_is_true) {
    while (*until_this_is_true) {
        locked_idle_has_run = true;
    }
}

void PendSV_Handler() {
    RUNNING_THREAD = os_next_active_thread(RUNNING_THREAD);
    my_pendsv_has_run = true;
}

void SysTick_Handler() {

    asm("CPSID   I                       /* 2) Prevent interrupt during switch*/ \n\t");
    asm("PUSH    {R4-R11}                /* 3) Save remaining regs r4-11*/ \n\t");

    /* Hopefully for not too long */
    /* IntMasterDisable(); */

    /* GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2) ^ GPIO_PIN_2); */

    asm("LDR     R0, =RUNNING_THREAD     /* 4) R0=pointer to RunPt, old thread*/ \n\t");
    asm("LDR     R1, [R0]                /*    R1 = RunPt*/ \n\t");
    asm("STR     SP, [R1]                /* 5) Save SP into TCB*/ \n\t");

    asm("PUSH {R0-R3, LR}");
    PendSV_Handler();
    asm("POP {R0-R3, LR}");

    asm("LDR     R1, =RUNNING_THREAD        /* 6) R1 = RunPt->next*/ \n\t");
    asm("LDR     R1, [R1]                /*    R1 = RunPt*/ \n\t");
    /* asm("STR     R1, [R0]                /\*    RunPt = R1*\/ \n\t"); */
    asm("LDR     SP, [R1]                /* 7) new thread SP; SP = RunPt->sp;*/ \n\t");
    asm("POP     {R4-R11}                /* 8) restore regs r4-11*/ \n\t");
    asm("CPSIE   I                       /* 9) tasks run with interrupts enabled*/ \n\t");
    asm("BX      LR                      /* 10) restore R0-R3,R12,LR,PC,PSR*/ \n\t");

    /* See, that wasn't for too long! */

}
