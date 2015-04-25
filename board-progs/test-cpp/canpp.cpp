#include "canpp.hpp"

#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"
#include "driverlib/pin_map.h"
#include "driverlib/interrupt.h"

#include "inc/hw_memmap.h"
#include "inc/hw_can.h"

#include <stdint.h>

#include "ctlsysctl.hpp"

#define CAN_MSG_OBJ 1

void can::init() {

    errors_rx = 0;
    errors_tx = 0;
    messages_sent = 0;
    messages_received = 0;

    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOB);
    ctlsys::enable_periph(base);
    GPIOPinConfigure(GPIO_PB4_CAN0RX);
    GPIOPinConfigure(GPIO_PB5_CAN0TX);
    GPIOPinTypeCAN(GPIO_PORTB_BASE, GPIO_PIN_4 | GPIO_PIN_5);

    CANInit(base);

    CANBitRateSet(base, SysCtlClockGet(), 500000);
    IntEnable(interrupt);
    start();
}

can::can() {}

can::can(memory_address_t can_base, uint32_t can_interrupt,
         bool can_sender, uint32_t msg_length) {

    sender = can_sender;
    base = can_base;
    interrupt = can_interrupt;

    init();

    switch(can_sender) {
    case true:
        sCANMessage.ui32MsgID = 1;
        sCANMessage.ui32MsgIDMask = 0;
        sCANMessage.ui32Flags = MSG_OBJ_TX_INT_ENABLE;
        sCANMessage.ui32MsgLen = msg_length;
        break;
    case false:
        // Initialize a message object to be used for receiving CAN messages with
        // any CAN ID.  In order to receive any CAN ID, the ID and mask must both
        // be set to 0, and the ID filter enabled.
        sCANMessage.ui32MsgID = 0;
        sCANMessage.ui32MsgIDMask = 0;
        sCANMessage.ui32Flags = MSG_OBJ_RX_INT_ENABLE | MSG_OBJ_USE_ID_FILTER;
        sCANMessage.ui32MsgLen = msg_length;
        // Load the message object into the CAN peripheral.  Once
        // loaded the CAN will receive any message on the bus, and an
        // interrupt will occur.  Use message object 1 for receiving
        // messages (this is distinct from the CAN ID).
        CANMessageSet(base, CAN_MSG_OBJ, &sCANMessage, MSG_OBJ_TYPE_RX);
        break;
    default: while(1) {}
    }
}

void can::set_timing() {

    tCANBitClkParms psClkParms;
    psClkParms.ui32SyncPropPhase1Seg = 5; /* from 2 to 16   */
    psClkParms.ui32Phase2Seg = 2;         /* from 1 to 8    */
    psClkParms.ui32QuantumPrescaler = 1;  /* from 1 to 1023 */
    psClkParms.ui32SJW = 2;               /* from 1 to 4    */
    CANBitTimingSet(base, &psClkParms);
}

void can::start() {

    CANIntEnable(base, CAN_INT_MASTER | CAN_INT_ERROR | CAN_INT_STATUS);
    CANEnable(base);
}

void can::stop() {

    CANIntDisable(base, CAN_INT_MASTER | CAN_INT_ERROR | CAN_INT_STATUS);
    CANDisable(base);
}

void can::error_tx() {

    ++errors_tx;
}

void can::get(uint8_t *data) {

    sCANMessage.pui8MsgData = data;
    CANMessageGet(base, 1, &sCANMessage, 0);
    if(sCANMessage.ui32Flags & MSG_OBJ_DATA_LOST) {
        ++errors_rx;
    }
}

void can::pack(uint8_t* dest, uint32_t data, uint8_t offset) {

    uint8_t i;
    for(i=0; i<sizeof(data); ++i) {
        /* todo: perfect the endian-swap */
        *(dest+offset) = (uint8_t) (data && (0xFF << (i*4)));
    }
}

/*! note this function will cause the message to be transmtted
 *  immediately. */
void can::transmit(uint8_t* data, uint32_t length, uint32_t id) {

    sCANMessage.ui32MsgID = id;
    sCANMessage.ui32MsgIDMask = 0;
    sCANMessage.ui32Flags = MSG_OBJ_TX_INT_ENABLE;
    sCANMessage.ui32MsgLen = length;
    sCANMessage.pui8MsgData = data;

    CANMessageSet(base, CAN_MSG_OBJ, &sCANMessage, MSG_OBJ_TYPE_TX);

    /* todo: resend if errors occured */
}

uint32_t can::count_message() {

    uint32_t ret;
    switch(sender) {
    case true:  ret = ++messages_sent; break;
    case false: ret = ++messages_received; break;
    default: while(1) {}
    }
    return ret;
}

uint32_t can::ack() {

    /* Read the CAN interrupt status to find the cause of the interrupt */
    uint32_t ui32Status = CANIntStatus(base, CAN_INT_STS_CAUSE);
    CANIntClear(base, ui32Status);
    count_message();
    return ui32Status;
}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
