/* -*- mode: c++; c-basic-offset: 4; */
/* Created by Hershal Bhave and Eric Crosson on <2015-03-15 Sun> */
/* Revision History: Look in Git FGT */
#include "switchpp.hpp"
#include "ctlsysctl.hpp"

#define NULL 0x00

lswitch::lswitch() {

    this->sem = NULL;
}

lswitch::lswitch(memory_address_t lswitch_base, memory_address_t lswitch_pin,
                 semaphore *sem, uint32_t interrupt_mask, bool start) {

    base = lswitch_base;
    pin = lswitch_pin;
    ctlsys::enable_periph(base);
    GPIOPinTypeGPIOInput(base, pin);
    GPIOIntTypeSet(base, pin, interrupt_mask);

    this->sem = sem;
    *(this->sem) = semaphore();

    if (start) {
        this->start();
    }
}

void lswitch::start() {

    GPIOIntEnable(base, pin);
}

void lswitch::stop() {

    GPIOIntDisable(base, pin);
}

uint32_t lswitch::ack() {

    GPIOIntClear(base, pin);
    if(NULL != sem) {
        sem->post();
    }
}

uint32_t lswitch::sample() {

    return GPIOPinRead(base, pin);
}


/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
