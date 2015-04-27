#include "pingpp.hpp"

#include "ctlsysctl.hpp"

#include "driverlib/interrupt.c"

ping::ping() {}

ping::ping(memory_address_t port_base, memory_address_t port_pin, semaphore* sem) {

    base = port_base;
    pin = port_pin;
    this->sem = sem;
    *(this->sem) = semaphore();

    ctlsys::enable_periph(base);
}

#define counter_delay(time, counter)                    \
    while(counter < time){counter++;}

/*! \warning this device should not be sampled more than once every
 *  200 micro-seconds */
void ping::sample() {

    uint32_t status = StartCritical();
    uint32_t counter = 0;

    /* Set Ping))) SIG to output */
    ctlsys::gpio_int_disable(base, pin);
    IntDisable(INT_GPIOB_TM4C123);
    IntDisable(INT_GPIOB);      /* todo: parametrize */
    GPIOPinTypeGPIOOutput(base, pin);

    /* Set SIG high for 5usec */
    GPIOPinWrite(base, pin, 1);
    /* Delay1us(5); */
    counter_delay(4, counter);

    GPIOPinWrite(base, pin, 0);

    /* Set Ping))) SIG to input */
    GPIOPinTypeGPIOInput(base, pin);
    GPIOIntTypeSet(base, pin, GPIO_BOTH_EDGES);

    counter_delay(200, counter);

    GPIOIntClear(base, pin);
    ctlsys::gpio_int_enable(base, pin);
    GPIOIntEnable(base, pin);
    IntEnable(INT_GPIOB_TM4C123);
    IntEnable(INT_GPIOB);

    EndCritical(status);
}
/* resume: finish hooking this above function up, the receiving ISR,
 * internal data structures, etc */

/* todo: implement */
void ping::start() {


}

/* resume:: implement these virtual functions */

/* todo: implement */
void ping::stop() {


}

/* todo: implement */
uint32_t ping::ack() {


}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
