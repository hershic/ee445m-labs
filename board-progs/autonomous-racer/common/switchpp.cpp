/* -*- mode: c++; c-basic-offset: 4; */
/* Created by Hershal Bhave and Eric Crosson on <2015-03-15 Sun> */
/* Revision History: Look in Git FGT */

#include "switchpp.hpp"
#include "ctlsysctl.hpp"

#include "driverlib/interrupt.h"

#include "inc/hw_types.h"
#include "inc/hw_gpio.h"

#define NULL 0x00

lswitch::lswitch() {

    this->sem = NULL;
}

lswitch::lswitch(memory_address_t lswitch_base, memory_address_t lswitch_pin,
                 semaphore *sem, timer* tim, uint32_t switch_interrupt,
                 uint32_t interrupt_mask, bool start) {

    base = lswitch_base;
    pin = lswitch_pin;

    ctlsys::enable_periph(base);
    GPIOPinTypeGPIOInput(base, pin);
    GPIODirModeSet(base, pin, GPIO_DIR_MODE_IN);

    if ((base == GPIO_PORTF_BASE) && (pin & GPIO_PIN_0)) {
        HWREG(base + GPIO_O_LOCK) = GPIO_LOCK_KEY;
        HWREG(base + GPIO_O_CR) = 0x01;
        HWREG(base + GPIO_O_LOCK) = 0;

        GPIOPadConfigSet(base, pin, GPIO_STRENGTH_2MA, GPIO_PIN_TYPE_STD_WPU);
    }

    this->tim = tim;
    /* TODO: parametrize first two parameters, this decision of hw choice belongs in main */
    *(this->tim) = timer(1, TIMER_A, TIMER_CFG_ONE_SHOT, SysCtlClockGet() / 10,
                         TIMER_TIMA_TIMEOUT);

    GPIOIntTypeSet(base, pin, switch_interrupt);
    IntEnable(interrupt_mask);

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

/*! Call this  in the isr of the switch  */
void lswitch::debounce() {

    // sched TIMER
    ack();
    stop();
    tim->start();
}

/* todo: remove need for external (client) isr's by changing the isr
 * at runtime in tis lib */

/*! Call this in the isr of the switch's timer  */
uint32_t lswitch::end_debounce() {

    tim->ack();
    if(NULL != sem) {
        sem->post();
    }
    start();
    return sample();
}

uint32_t lswitch::ack() {

    GPIOIntClear(base, pin);
}

uint32_t lswitch::sample() {

    return GPIOPinRead(base, pin);
}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
