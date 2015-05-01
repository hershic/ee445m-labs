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
#define PING_BUFFER_TYPE   int16_t

typedef uint32_t memory_address_t;

#define PING_INACTIVE 0x01
#define PING_SENT     0x02
#define PING_RESPONSE 0x04

class ping : public critical, public interruptable {
private:
    semaphore *sem;
    blinker sig;
    uint16_t status;
    timer tim;
public:
    memory_address_t base;
    memory_address_t pin;

    ping();
    ping(memory_address_t port_base, memory_address_t port_pin, semaphore* sem,
         timer_t timer_id, subtimer_t timer_subtimer);
    void sample(void);

    virtual void start(void);
    virtual void stop(void);
    virtual uint32_t ack(void);

    circularbuffer<PING_BUFFER_TYPE, PING_BUFFER_LENGTH> buf;
};

#endif  /* __PING__ */

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
