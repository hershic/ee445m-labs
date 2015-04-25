/* -*- mode: c++; c-basic-offset: 4; -*- */
#ifndef __canpp__
#define __canpp__

/*! \addtogroup CAN
 * @{
 */

#include <stdint.h>
#include <stdbool.h>

#include "interruptable.hpp"

#include "driverlib/can.h"

typedef uint32_t memory_address_t;

class can : public interruptable {
private:
    memory_address_t base;
    uint32_t interrupt;
    bool sender;

    tCANMsgObject sCANMessage;

    uint32_t errors_tx;
    uint32_t errors_rx;
    uint32_t messages_sent;
    uint32_t messages_received;

    /*! Configure CAN for 1 Mbit operation -- http://bit.ly/1CW7hUO */
    void set_timing(void);

    /*! Shared init between constructors. */
    void init(void);

    /*! Add a message to the global count. */
    uint32_t count_message(void);

public:
    can();
    /*! Initialize a can sender. */
    can(memory_address_t can_base, uint32_t can_interrupt, bool can_sender);

    /*! Start CAN transmissions. */
    virtual void start(void);

    /*! Stop CAN transmissions. */
    virtual void stop(void);

    /*! Acknowledge CAN interrupt */
    virtual uint32_t ack(void);

    void pack(uint8_t* dest, uint32_t data, uint8_t offset = 0);

    /*! Transmit a message via CAN. */
    void transmit(uint8_t* data, uint32_t length, uint32_t id);

    /*! Set data mailbox for received can message. */
    void get(uint8_t *data);
};

#endif

/*! End doxygen group
 * @}
 */

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
