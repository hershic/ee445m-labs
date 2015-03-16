/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-02-28 */
/* Revision history: Look in Git FGT */
#include "jitter.h"

void pidwork_init() {

    PIDWORK = 0;
    HIGHEST_PIDWORK = 0;
    LOWEST_PIDWORK = (uint32_t)(-1);
    PIDWORK_IDX = 0;

}

void pidwork_increment() {

    while(1) {
        asm volatile("CPSID  I");
        ++PIDWORK;
        asm volatile("CPSIE  I");
    }
}

/*! A thread that continuously toggles GPIO pin 2 on GPIO_PORT_F. */
void pidwork_record() {

    while(1) {
        if (PIDWORK != 0) {
            if (HIGHEST_PIDWORK < PIDWORK) {
                HIGHEST_PIDWORK = PIDWORK;
            }
            if (LOWEST_PIDWORK > PIDWORK) {
                LOWEST_PIDWORK = PIDWORK;
            }
            PIDWORK_BUFFER[PIDWORK_IDX] = PIDWORK;
            PIDWORK_IDX = (PIDWORK_IDX + 1) & PIDWORK_BUFFER_SIZE;
        }
        PIDWORK = 0;
    }
}
