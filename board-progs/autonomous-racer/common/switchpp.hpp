/* -*- mode: c++; c-basic-offset: 4; */
#ifndef __lswitchpp__
#define __lswitchpp__

#include <stdbool.h>
#include <stdint.h>

#include "semaphorepp.hpp"
#include "timerpp.hpp"
#include "interruptable.hpp"

#include "driverlib/gpio.h"

typedef uint32_t memory_address_t;

class lswitch : public interruptable {
private:
    memory_address_t base;
    memory_address_t pin;
    semaphore *sem;
    timer* tim;
public:
    /*! Initialize lswitch. */
    lswitch();
    lswitch(memory_address_t lswitch_base, memory_address_t lswitch_pin,
            semaphore *sem, timer_t timer_id, subtimer_t timer_subtimer,
            uint32_t switch_interrupt, uint32_t interrupt_mask = GPIO_BOTH_EDGES,
            bool start = false);

    virtual void start(void);
    virtual void stop(void);
    virtual uint32_t ack(void);

    void debounce(void);
    uint32_t end_debounce(void);

    /*! Return current value of the limit lswitch */
    uint32_t sample(void);
};

#endif  /* __LSWITCHPP__ */

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
