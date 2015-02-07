/* -*- mode: c; c-basic-offset: 4; -*- */
#include "os.h"

#include <stdint.h>

static int32_t program_stacks[NUMTHREADS][STACKSIZE];
static tcb_t tcb_list[NUMTHREADS];
static tcb_t running_thread;

bool my_systick_has_run = false;

/* This data structure was stolen from Valvano website
   http://users.ece.utexas.edu/~valvano/arm/os.c */
void os_reset_thread_stack(uint32_t thread) {
    tcb_list[thread].sp = &program_stacks[thread][STACKSIZE-16];
    program_stacks[thread][R_R0]  = 0xDEAD0000;
    program_stacks[thread][R_R1]  = 0xDEAD0001;
    program_stacks[thread][R_R2]  = 0xDEAD0002;
    program_stacks[thread][R_R3]  = 0xDEAD0003;
    program_stacks[thread][R_R4]  = 0xDEAD0004;
    program_stacks[thread][R_R5]  = 0xDEAD0005;
    program_stacks[thread][R_R6]  = 0xDEAD0006;
    program_stacks[thread][R_R7]  = 0xDEAD0007;
    program_stacks[thread][R_R8]  = 0xDEAD0008;
    program_stacks[thread][R_R9]  = 0xDEAD0009;
    program_stacks[thread][R_R10] = 0xDEAD0010;
    program_stacks[thread][R_R11] = 0xDEAD0011;
    program_stacks[thread][R_R12] = 0xDEAD0012;
    /* program_stacks[thread][R_R13] = 0xDEAD0013; */
    program_stacks[thread][R_R14] = 0xDEAD0014;
}

void os_threading_init() {
    int i;

    for (i=0; i<NUMTHREADS; ++i) {
        tcb_list[i].status = THREAD_DEAD;
        os_reset_thread_stack(i);
    }

    os_add_thread(idle_thread);
    tcb_list[i].status = THREAD_ACTIVE;
}

int os_add_thread(void(*task)(void)) {
    int thread_id;
    if (os_next_dead_thread(&thread_id)) {
        os_reset_thread_stack(thread_id);
        os_set_thread_pc(thread_id, task);
        tcb_list[thread_id].status = THREAD_ACTIVE;
        return 1;
    }
    return 0;
}

bool os_next_dead_thread(int* thread_id) {
    int i;
    for (i=0; i<NUMTHREADS; ++i) {
        if (tcb_list[i].status == THREAD_DEAD) {
            *thread_id = i;
            return true;
        }
    }
    return false;
}

void os_set_thread_pc(int thread_id, void(*task)(void)) {
    program_stacks[thread_id][R_PC] = (int32_t)(task);
}

/* Do-nothing thread */
void idle_thread() {
    while (1) {}
}

void SysTick_Handler() {
    GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, GPIO_PIN_2);
    my_systick_has_run = true;
}
