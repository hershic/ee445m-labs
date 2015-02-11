/* -*- mode: c; c-basic-offset: 4; -*- */
#include "os.h"

#include <stdint.h>

static int32_t program_stacks[NUMTHREADS][STACKSIZE];
static thread_t thread_list[NUMTHREADS];
static thread_t* running_thread;

bool my_idle_has_run = false;

/* This data structure was stolen from Valvano website
   http://users.ece.utexas.edu/~valvano/arm/os.c */
void os_reset_thread_stack(thread_t* thread) {
    uint32_t th_id = thread->thread_id;
    thread_list[th_id].sp = &program_stacks[th_id][STACKSIZE-16];
    program_stacks[th_id][R_R0]  = 0xDEAD0000;
    program_stacks[th_id][R_R1]  = 0xDEAD0001;
    program_stacks[th_id][R_R2]  = 0xDEAD0002;
    program_stacks[th_id][R_R3]  = 0xDEAD0003;
    program_stacks[th_id][R_R4]  = 0xDEAD0004;
    program_stacks[th_id][R_R5]  = 0xDEAD0005;
    program_stacks[th_id][R_R6]  = 0xDEAD0006;
    program_stacks[th_id][R_R7]  = 0xDEAD0007;
    program_stacks[th_id][R_R8]  = 0xDEAD0008;
    program_stacks[th_id][R_R9]  = 0xDEAD0009;
    program_stacks[th_id][R_R10] = 0xDEAD0010;
    program_stacks[th_id][R_R11] = 0xDEAD0011;
    program_stacks[th_id][R_R12] = 0xDEAD0012;
    /* program_stacks[th_id][R_R13] = 0xDEAD0013; */
    program_stacks[th_id][R_R14] = 0xDEAD0014;
    program_stacks[th_id][R_THUMB] = 0x01000000;
}

void os_threading_init() {
    int i;

    for (i=0; i<NUMTHREADS; ++i) {
        thread_list[i].status = THREAD_DEAD;
        thread_list[i].thread_id = i;

        /* yes, I do a double-indirection here, but we're only doing
           it once and the code is much cleaner this way. */
        os_reset_thread_stack(&thread_list[i]);

        thread_list[i].next_thread = &thread_list[i+1];
    }

    thread_list[NUMTHREADS-1].next_thread = &thread_list[0];

    running_thread = os_add_thread(idle);
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
        if (thread_list[i].status == THREAD_DEAD) {
            return &thread_list[i];
        }
    }
    return 0;
}

thread_t* os_next_active_thread(thread_t* from_thread) {
    thread_t* th;
    for (th=from_thread->next_thread; th != from_thread; th = th->next_thread) {
        if (th->status == THREAD_ACTIVE) {
            return th;
        }
    }
    return 0;
}

void os_set_thread_pc(thread_t* thread, void(*task)(void)) {
    program_stacks[thread->thread_id][R_PC] = (int32_t)(task);
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
        lock_idle_has_run = true;
    }
}

void SysTick_Handler() {

    thread_t* next_thread = running_thread;

    /* Look until we can find a new thread to run */
    /* do { */
    /*     next_thread = (thread_t*)(next_thread->next_thread); */
    /* } while (next_thread->status != THREAD_ACTIVE); */
    /*  */
    /* running_thread->next_thread = next_thread; */

    /* GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, GPIOPinRead(GPIO_PORTF_BASE, GPIO_PIN_2) ^ GPIO_PIN_2); */

    asm("CPSID   I                       /* 2) Prevent interrupt during switch*/ \n\t");
    asm("PUSH    {R4-R11}                /* 3) Save remaining regs r4-11*/ \n\t");
    asm("LDR     R0, =running_thread     /* 4) R0=pointer to RunPt, old thread*/ \n\t");
    asm("LDR     R1, [R0]                /*    R1 = RunPt*/ \n\t");
    asm("STR     SP, [R1]                /* 5) Save SP into TCB*/ \n\t");
    asm("LDR     R1, [R1,#4]             /* 6) R1 = RunPt->next*/ \n\t");
    asm("STR     R1, [R0]                /*    RunPt = R1*/ \n\t");
    asm("LDR     SP, [R1]                /* 7) new thread SP; SP = RunPt->sp;*/ \n\t");
    asm("POP     {R4-R11}                /* 8) restore regs r4-11*/ \n\t");
    asm("CPSIE   I                       /* 9) tasks run with interrupts enabled*/ \n\t");
    asm("BX      LR                      /* 10) restore R0-R3,R12,LR,PC,PSR*/ \n\t");


}

