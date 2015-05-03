/* -*- mode: c++; c-basic-offset: 4; */
/* Created by Eric Crosson on 2015-04-26 */
#ifndef __pingpp__
#define __pingpp__

#include "circularbuffer.hpp"
#include "interruptable.hpp"
#include "semaphorepp.hpp"
#include "criticalpp.hpp"
#include "blinker.hpp"
#include "timerpp.hpp"

#include <stdbool.h>
#include <stdint.h>

#define PING_BUFFER_LENGTH 32
#define PING_BUFFER_TYPE   int32_t

typedef uint32_t memory_address_t;

typedef enum ping_status {
    ping_not_active=0,
    ping_signal=1,
    ping_response=2,
    ping_sample_delay=3
} ping_status_t;

class ping : public critical {
private:
    semaphore sem;
    blinker sig;
    ping_status_t status;
    timer tim;
    uint32_t timer_interrupt;
    memory_address_t base;
    memory_address_t pin;

    uint32_t timer_signal_value;
    uint32_t timer_response_value;

public:
    ping();
    ping(memory_address_t port_base, memory_address_t port_pin,
         timer_t timer_id, subtimer_t timer_subtimer);
    void sample(void);

    /*! Start the timer monitoring sig. */
    virtual void start(void);

    /*! Stop the timer monitoring sig. */
    virtual void stop(void);

    /*! Acknowledge isr for sig. */
    uint32_t handle_gpio(void);
    uint32_t handle_timer(void);

    /*! The ping sensor's semaphore */
    semaphore* get_sem(void);

    /*! Alert object that the isr watching both edges of sig has been triggered. */
    uint32_t notify(void);

    int32_t average(void);
    int32_t distance(void);

    circularbuffer<PING_BUFFER_TYPE, PING_BUFFER_LENGTH> buf;
};

#endif  /* __PING__ */

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
