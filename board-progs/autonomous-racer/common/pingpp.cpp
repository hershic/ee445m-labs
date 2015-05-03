/* -*- mode: c++; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-04 */
/* Revision history: Look in Git FGT */
#include "pingpp.hpp"

#include "delay.hpp"
#include "ctlsysctl.hpp"

#include "driverlib/interrupt.c"

ping::ping() {}

ping::ping(memory_address_t ping_base, memory_address_t ping_pin,
           timer_t ping_timer_id, subtimer_t ping_timer_subtimer) {

    status = ping_not_active;

    base = ping_base;
    pin = ping_pin;
    sig = blinker(base);

    sem = semaphore();
    buf = circularbuffer<PING_BUFFER_TYPE, PING_BUFFER_LENGTH>();

    switch(ping_timer_subtimer) {
    case TIMER_A:
    case TIMER_BOTH:
        timer_interrupt = TIMER_TIMA_TIMEOUT;
        break;
    case TIMER_B:
        timer_interrupt = TIMER_TIMB_TIMEOUT;
        break;
    default:
        while (1) {}
    }

    tim = timer(ping_timer_id, ping_timer_subtimer, TIMER_CFG_ONE_SHOT_UP, 0x0fffffe,
                timer_interrupt);

    ctlsys::enable_periph(base);
}

/*! \warning this device should not be sampled more than once every
 *  200 micro-seconds */
void ping::sample() {

    if (status == ping_not_active) {
        uint32_t intstatus = StartCritical();

        /* Disable interrupts in SIG */
        ctlsys::gpio_int_disable(base, pin);
        IntDisable(ctlsys::periph_to_int(base));

        /* Set Ping))) SIG to output */
        GPIOPinTypeGPIOOutput(base, pin);
        sig.turn_on(pin);
        /* Set SIG high for 5usec */
        delay::count(1);
        sig.turn_off(pin);

        /* Set Ping))) SIG to input */
        GPIOPinTypeGPIOInput(base, pin);
        GPIOIntTypeSet(base, pin, GPIO_BOTH_EDGES);
        delay::count(200);

        /* Enable interupts on SIG */
        ctlsys::gpio_int_enable(base, pin, true);
        IntEnable(ctlsys::periph_to_int(base));

        EndCritical(intstatus);
    }
}

void ping::start() {

    /* start the chain */
    sem.post();
}

void ping::stop() {

    sem.reset();
}

uint32_t ping::handle_timer() {

    tim.ack();

    if (status == ping_not_active) {
        /* TODO: remove when done debugging */
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_1, GPIO_PIN_1);
    } else if (status == ping_signal) {
        /* TODO: remove when done debugging */
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, GPIO_PIN_2);
    } else if (status == ping_response) {
        /* TODO: remove when done debugging */
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_3, GPIO_PIN_3);
    } else if (status == ping_sample_delay) {
        status = ping_not_active;
    }

    /* either a timeout happened or the sensor delay period has been
       achieved */
    sem.post();
}

uint32_t ping::handle_gpio() {

    GPIOIntClear(base, pin);

    status = ping_status_t(int(status) + 1);

    if (status == ping_signal) {
        tim.reload();
        timer_signal_value = tim.get();
        tim.start();
    } else if (status == ping_response) {
        timer_response_value = tim.get();
        buf.add(timer_response_value - timer_signal_value);
        status = ping_sample_delay;
        tim.load(SysCtlClockGet()/25);
        tim.start();
    } else {
        /* TODO: do something here? */
    }

    return 0xDEADBEEF;
}

semaphore* ping::get_sem() {

    return &sem;
}

int32_t ping::average() {

    int32_t i, value;
    value = 0;
    for (i=0; i<buf.len; ++i) {
        value += buf.buf[i];
    }
    value /= buf.len;
    return value;
}

int32_t ping::distance() {

    /* TODO: calibrate */
    return 0;
}


/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
