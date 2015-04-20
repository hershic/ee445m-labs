/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __hardwarepp__
#define __hardwarepp__

#include <map>
#include <list>
#include <stdbool.h>

#include "libstd/nexus.h"
#include "libnotify/notify.h"
#include "libos/thread_structures.h"

#include "bufferpp.hpp"

using std::map;
using std::list;

/*! \addtogroup hardware
 * @{
 */

typedef void (*pseudo_isr)(notification note);

static volatile semaphore_t HW_SEM_UART0;
static volatile buffer BUFFER_UART0_RX;

enum HW_TYPE { UART, TIMER, };

class hardware {
private:
    static bool hardware_uninitialized = true;
    static hardware single;
    std::map<int, std::map<int, list<pseudo_isr> > > notifies;

    /*! Return the hw_type that \interrupt belongs to. */
    static HW_TYPE interrupt_type(memory_address_t interrupt);

public:
    hardware();
    static hardware singleton(void);

    void daemon(void);

    /*! Initialize a uart channel. */
    void init_uart(memory_address_t base, memory_address_t interrupt);

    /*! Notify a pseudo-isr of an interrupt. */
    void notify(memory_address_t base, memory_address_t interrupt, notification note);

    /*! Subscribe to a channel. */
    void subscribe(memory_address_t interrupt, void (*isr)(notification note));

    /*! Unsubscribe from a channel. */
    void unsubscribe(memory_address_t interrupt, void (*isr)(notification note));
};

#endif

/*! End doxygen group
 * @}
 */
