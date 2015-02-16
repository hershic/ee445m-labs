/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson on 2015-01-24 */
/* Revision History: Look in Git FGT */

/* Standard Libs */
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/* TI Includes */
#include "inc/hw_ints.h"
#include "inc/hw_memmap.h"

/* Driverlib Includes */
#include "driverlib/debug.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/systick.h"
#include "driverlib/rom.h"

#include "libos/os.h"
#include "libheart/heartbeat.h"

#include <sys/stat.h>

unsigned long CountPF2Toggle; // number of times thread1 loops
unsigned long CountPF3Toggle; // number of times thread2 loops
unsigned long CountPF4Toggle; // number of times thread3 loops

muscle_t muscle_pf2;
muscle_t muscle_pf3;
muscle_t muscle_pf4;

void Thread1(void){
    CountPF2Toggle = 0;
    while(1){
	heart_toggle_(&muscle_pf2);
        CountPF2Toggle++;
    }
}

void Thread2(void){
    CountPF3Toggle = 0;
    while(1){
	heart_toggle_(&muscle_pf3);
        CountPF3Toggle++;
    }
}

void Thread3(void){
    CountPF4Toggle = 0;
    while(1){
	heart_toggle_(&muscle_pf4);
        CountPF4Toggle++;
    }
}

int main() {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    /* Enable the GPIO port that is used for the on-board LED. */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);

    /* Enable the GPIO pins for the LED (PF2). */
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_2|GPIO_PIN_3|GPIO_PIN_4);

    /* Initialize GPIO */
    muscle_pf2.base = GPIO_PORTF_BASE;
    muscle_pf3.base = GPIO_PORTF_BASE;
    muscle_pf4.base = GPIO_PORTF_BASE;
    muscle_pf2.pin = GPIO_PIN_2;
    muscle_pf3.pin = GPIO_PIN_3;
    muscle_pf4.pin = GPIO_PIN_4;

    heart_init_(&muscle_pf2);
    heart_init_(&muscle_pf3);
    heart_init_(&muscle_pf4);
    /* End GPIO Init */

    IntMasterDisable();

    os_threading_init();
    os_add_thread(Thread1);
    os_add_thread(Thread2);
    os_add_thread(Thread3);

    /* Load and enable the systick timer */
    SysTickPeriodSet(SysCtlClockGet() / 10);
    SysTickEnable();
    SysTickIntEnable();

    os_launch();
    /* PONDER: why do interrupts fire without this? */
    IntMasterEnable();

    /* And we're done; this should never execute */
    postpone_death();
}
