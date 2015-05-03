/* -*- mode: c++; c-basic-offset: 4; -*- */
#ifndef __canpp__
#define __canpp__

/*! \addtogroup CAN
 * @{
 */

#define DEFAULT_CAN_MSG_ID 1

#include <stdint.h>
#include <stdbool.h>

#include "interruptable.hpp"
#include "semaphorepp.hpp"

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
    /*! Initialize a can node. */
    can(memory_address_t can_base, uint32_t can_interrupt,
        bool can_sender, uint32_t msg_length);

    /*! Start CAN transmissions. */
    virtual void start(void);

    /*! Stop CAN transmissions. */
    virtual void stop(void);

    /*! Acknowledge CAN interrupt */
    virtual uint32_t ack(void);

    void pack(uint8_t* dest, uint32_t data, uint8_t offset = 0);

    /*! Transmit a message via CAN. */
    void transmit(uint8_t* data, uint32_t length, uint32_t id = DEFAULT_CAN_MSG_ID);

    /*! Set mailbox for received can message data. */
    void get(uint8_t *data);

    /*! Register a tx error. */
    void error_tx(void);

    semaphore recv_sem;
};

#endif

/*! End doxygen group
 * @}
 */

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
