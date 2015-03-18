/* -*- mode: c; c-basic-offset: 4; */
/* Created by Hershal Bhave on <2015-03-15 Sun> */
/* Revision History: Look in Git FGT */

#include "blinker.hpp"

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

#include "inc/hw_memmap.h"

#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"

int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN | SYSCTL_XTAL_16MHZ);
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOF);
    GPIOPinTypeGPIOOutput(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_3);

    blinker blink;
    blink.set_base(GPIO_PORTF_BASE);

    while (1) {
        blink.toggle(RED);
    }
}

/* extern "C" void* malloc(size_t) { */
/*     return (void*) 0; */
/* } */

/* extern "C" void free(void*) { } */

/* void* operator new(size_t size) throw() { return malloc(size); } */
/* void  operator delete(void* p) { free(p); } */
/* extern "C" int __aeabi_atexit(void* object, void (*detructor)(void*), */
/*                               void *dso_handle) { return 0; } */
