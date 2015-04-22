#include "canpp.hpp"

#include "driverlib/can.h"
#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"
#include "driverlib/pin_map.h"
#include "driverlib/interrupt.h"

#include "inc/hw_memmap.h"

can::can() {}

can::can(memory_address_t can_base, uint32_t can_interrupt,
         bool can_sender) {

    base = can_base;
    interrupt = can_interrupt;
    CANInit(base);

    set_timing();
    enable();

    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOB);
    GPIOPinConfigure(GPIO_PB4_CAN0RX);
    GPIOPinConfigure(GPIO_PB5_CAN0TX);
    GPIOPinTypeCAN(GPIO_PORTB_BASE, GPIO_PIN_4 | GPIO_PIN_5);
    SysCtlPeripheralEnable(SYSCTL_PERIPH_CAN0);
    CANInit(base);
    CANBitRateSet(base, SysCtlClockGet(), 500000);
    CANIntEnable(base, CAN_INT_MASTER | CAN_INT_ERROR | CAN_INT_STATUS);
    IntEnable(interrupt);
    CANEnable(base);
}

void can::set_timing() {

    tCANBitClkParms psClkParms;
    psClkParms.ui32SyncPropPhase1Seg = 5; /* from 2 to 16   */
    psClkParms.ui32Phase2Seg = 2;         /* from 1 to 8    */
    psClkParms.ui32QuantumPrescaler = 1;  /* from 1 to 1023 */
    psClkParms.ui32SJW = 2;               /* from 1 to 4    */
    CANBitTimingSet(base, &psClkParms);
}

void can::enable() {

    CANEnable(base);
    CANIntEnable(base, CAN_INT_MASTER | CAN_INT_ERROR | CAN_INT_STATUS);
}

void can::disable() {

    CANDisable(base);
    CANIntDisable(base, CAN_INT_MASTER | CAN_INT_ERROR | CAN_INT_STATUS);
}

void can::transmit(uint8_t* data, uint32_t length, uint32_t id) {

    tCANMsgObject sCANMessage;
    sCANMessage.ui32MsgID = id;
    sCANMessage.ui32MsgIDMask = 0;
    sCANMessage.ui32Flags = MSG_OBJ_TX_INT_ENABLE;
    sCANMessage.ui32MsgLen = length;
    /* todo: endian-swap */
    sCANMessage.pui8MsgData = data;

    // Send the CAN message using object number 1 (not the same thing
    // as CAN ID). This function will cause the message to be
    // transmitted right away.
    CANMessageSet(base, 1, &sCANMessage, MSG_OBJ_TYPE_TX);
}
