#include "pingpp.hpp"

#include "delay.hpp"
#include "ctlsysctl.hpp"

#include "driverlib/interrupt.c"

#define TEST_PING 1

ping::ping() {}

ping::ping(memory_address_t port_base, memory_address_t port_pin, semaphore* sem,
    timer_t timer_id, subtimer_t timer_subtimer) {

    status = PING_INACTIVE;

    base = port_base;
    pin = port_pin;
    sig = blinker(base);

    this->sem = sem;
    *(this->sem) = semaphore();

    tim = timer(timer_id, timer_subtimer, TIMER_CFG_PERIODIC_UP, 0x0fffffe,
        ctlsys::timer_timeout_from_subtimer(timer_subtimer));

    ctlsys::enable_periph(base);
}

/*! \warning this device should not be sampled more than once every
 *  200 micro-seconds */
void ping::sample() {

    uint32_t status = StartCritical();

    /* Disable interrupts in SIG */
    ctlsys::gpio_int_disable(base, pin);
    IntDisable(INT_GPIOB_TM4C123);
    IntDisable(INT_GPIOB);      /* todo: parametrize */

    /* Set Ping))) SIG to output */
    GPIOPinTypeGPIOOutput(base, pin);
    sig.turn_on(pin);
    /* Set SIG high for 5usec */
    delay::count(4);
    sig.turn_off(pin);

    /* Set Ping))) SIG to input */
    GPIOPinTypeGPIOInput(base, pin);
    GPIOIntTypeSet(base, pin, GPIO_BOTH_EDGES);
    delay::count(200);

    /* Enable interupts on SIG */
    ctlsys::gpio_int_enable(base, pin, true);
    IntEnable(INT_GPIOB_TM4C123);
    IntEnable(INT_GPIOB);

    EndCritical(status);
}
/* resume: finish hooking this above function up, the receiving ISR,
 * internal data structures, etc */

/*! \note this acknowledges the interrupt */
uint32_t ping::notify() {

    switch(status) {
    case PING_INACTIVE: start(); break;
    case PING_SENT: stop(); break;
    case PING_RESPONSE:
        #if TEST_PING == 1
        while(1) {}
        #else
        /* do nothing for now */
        #endif
        break;
    default: while(1) {}
    }
    return ack();
}

/* resume:: implement these virtual functions */
void ping::start() {
#if TEST_PING == 1
    if (status != PING_INACTIVE) {
        while(1) {}
    }
#endif
    status = PING_SENT;
    tim.reload();
    tim.start();
}

/* TODO: watch overflow or make time 32 bit by default */

void ping::stop() {
#if TEST_PING == 1
    if (status != PING_SENT) {
        while(1) {}
    }
#endif
    status = PING_RESPONSE;
    /* TODO: read timer contents and stop timer, populate buffer  */
    sem->post();
    status = PING_INACTIVE;
}

uint32_t ping::ack() {

    return GPIOIntClear(base, pin);
}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
