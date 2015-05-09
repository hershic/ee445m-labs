/* -*- mode: c++; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-04-19 */
/* Revision history: Look in Git FGT */
#ifndef __timerpp__
#define __timerpp__

#include <stdint.h>
#include <stdbool.h>

#include "interruptable.hpp"

#include "driverlib/pin_map.h"
#include "driverlib/timer.h"
#include "driverlib/sysctl.h"

/*! \addtogroup
   * @{
    */

typedef uint8_t utimer_t;
typedef uint32_t subtimer_t;
typedef uint32_t reload_t;

const uint32_t TIMER_DEFAULT_PRIORITY = 0;

class timer : public interruptable {
private:
    /*! Defined between 0 to 4, where 0 indicates TIMER0 */
    utimer_t id;

    /*! The base address of the specified timer */
    uint32_t base;

    /*! TIMER_A, TIMER_B, or TIMER_BOTH */
    subtimer_t subtimer;

    /*! The reload value for the timer */
    reload_t reload_value;

    /*! The configuration bitset for the timer */
    /*! \note Reference timer.h for possible options (e.g. TIMER_CFG_PERIODIC) */
    uint32_t configuration;

    /*! The timer interrupt to be acknowdeged, enabled, disabled */
    /*! \note Reference timer.h for possible options (e.g. TIMER_TIMA_TIMEOUT) */
    uint32_t interrupt;

public:

    timer();
    timer(utimer_t timer_id, subtimer_t timer_subtimer,
          uint32_t timer_configuration, reload_t timer_load_val,
          uint32_t timer_interrupt, bool timer_start = false);

    /*! Start a timer. */
    virtual void start();

    /*! Stop a timer. */
    virtual void stop();

    /*! Acknowledge an interrupt. Clears the interrupt bits for this timer. */
    virtual uint32_t ack();

    virtual uint32_t get();

    const subtimer_t get_subtimer();

    const uint32_t get_base();

    /*! Reload the timer with the initial reload value */
    void reload(void);
    void load(uint32_t load_value);
};

#endif

/* End Doxygen group
 * @}
 */

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
