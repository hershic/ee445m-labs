#include "canpp.hpp"

#include "driverlib/can.h"
#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"
#include "driverlib/pin_map.h"
#include "driverlib/interrupt.h"

#include "inc/hw_memmap.h"
#include "inc/hw_can.h"

bool can::error_flag;

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

    // Check the error flag to see if errors occurred
    if(error_flag) {
        ++errors_tx;
        /* UARTprintf(" error - cable connected?\n"); */
    }
}

extern "C" void CAN0_Handler(void) {
    uint32_t ui32Status;

    // Read the CAN interrupt status to find the cause of the interrupt
    ui32Status = CANIntStatus(CAN0_BASE, CAN_INT_STS_CAUSE);

    // If the cause is a controller status interrupt, then get the status
    if(ui32Status == CAN_INT_INTID_STATUS)
    {
        // Read the controller status.  This will return a field of status
        // error bits that can indicate various errors.  Error processing
        // is not done in this example for simplicity.  Refer to the
        // API documentation for details about the error status bits.
        // The act of reading this status will clear the interrupt.  If the
        // CAN peripheral is not connected to a CAN bus with other CAN devices
        // present, then errors will occur and will be indicated in the
        // controller status.
        ui32Status = CANStatusGet(CAN0_BASE, CAN_STS_CONTROL);

        // Set a flag to indicate some errors may have occurred.
        can::error_flag = 1;
    }

    // Check if the cause is message object 1, which what we are using for
    // sending messages.
    else if(ui32Status == 1)
    {
        // Getting to this point means that the TX interrupt occurred on
        // message object 1, and the message TX is complete.  Clear the
        // message object interrupt.
        CANIntClear(CAN0_BASE, 1);

        // Increment a counter to keep track of how many messages have been
        // sent.  In a real application this could be used to set flags to
        // indicate when a message is sent.
        /* g_ui32MsgCount++; */

        // Since the message was sent, clear any error flags.
        can::error_flag = 0;
    }
}
