/* Created by Hershal Bhave and Eric Crosson on 2015-01-25 */
/* Revision History: Look in Git FGT */

/* Standard Libs */
#include <stdint.h>
#include <stdbool.h>

/* TI Includes */
#include "inc/hw_ints.h"
#include "inc/hw_memmap.h"

/* Driverlib Includes */
#include "driverlib/debug.h"
#include "driverlib/adc.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/can.h"

#include "libstd/nexus.h"
#include "libuart/uart.h"

int main(void) {

    hw_metadata metadata;

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    /* Enable processor interrupts */
    IntMasterDisable();

    SysCtlPeripheralEnable(SYSCTL_PERIPH_CAN0);

    /**********************/
    /* CAN Initialization */
    /**********************/
    /* ctags _CANBaseValid(uint32_t ui32Base) for more info */
    uint32_t can_base = CAN0_BASE;
    CANInit(can_base);

    /* configure the controller for 1 Mbit operation -- http://bit.ly/1CW7hUO */
    tCANBitClkParms psClkParms;
    psClkParms.ui32SyncPropPhase1Seg = 5; /* from 2 to 16   */
    psClkParms.ui32Phase2Seg = 2;         /* from 1 to 8    */
    psClkParms.ui32QuantumPrescaler = 1;  /* from 1 to 1023 */
    psClkParms.ui32SJW = 2;               /* from 1 to 4    */
    CANBitTimingSet(can_base, &psClkParms);

    /* After CANInit() and CANBitTimingSet() we may CANEnable() */
    CANEnable(can_base);    /* can CANDisable(); which does not re-init */

    /* Begin CAN transmission init */
    uint32_t ui32Base;
    uint32_t ui32ObjID = 0;
    tCANMsgObject *psMsgObject;
    tMsgObjType eMsgType;
    uint32_t num_data_frame_bytes = 8;
    uint8_t data_frame[] = {'T','h','i','s',' ','i','s',0};

#define CAN_SEND 0
#define CAN_RECV 1
#define CAN_ROLE CAN_RECV

    if (CAN_ROLE == CAN_SEND) {
        /* To send a data frame or remote frame (in response to a remote */
        /* request), take the following steps: */

        /* 1. Set eMsgType to MSG_OBJ_TYPE_TX. */
        /* 2. Set psMsgObject->ui32MsgID to the message ID. */
        /* 3. Set psMsgObject->ui32Flags. Make sure to set */
        /*    MSG_OBJ_TX_INT_ENABLE to allow an interrupt to be generated */
        /*    when the message is sent. */
        /* 4. Set psMsgObject->ui32MsgLen to the number of bytes in the data */
        /*    frame. */
        /* 5. Set psMsgObject->pui8MsgData to point to an array containing */
        /*    the bytes to send in the message. */
        /* 6. Call this function with ui32ObjID set to one of the 32 object buffers. */

        /* 1. */ eMsgType = MSG_OBJ_TYPE_TX;
        /* 2. */ psMsgObject->ui32MsgID = 0; /* initial message id */
        /* 3. */ psMsgObject->ui32Flags = MSG_OBJ_TX_INT_ENABLE; /* generate interrupt when message is sent */
        /* 4. */ psMsgObject->ui32MsgLen = num_data_frame_bytes;
        /* 5. */ psMsgObject->pui8MsgData = data_frame;

    } else if (CAN_ROLE == CAN_RECV) {

        /* Example: To receive a specific data frame, take the following steps: */

        /* 1. Set eMsgObjType to MSG_OBJ_TYPE_RX. */
        /* 2. Set psMsgObject->ui32MsgID to the full message ID, or a partial */
        /*    mask to use partial ID matching. */
        /* 3. Set psMsgObject->ui32MsgIDMask bits that are used for masking */
        /*    during comparison. */
        /* 4. Set psMsgObject->ui32Flags as follows: */
        /*    - Set MSG_OBJ_RX_INT_ENABLE flag to be interrupted when the data */
        /*      frame is received. */
        /*    - Set MSG_OBJ_USE_ID_FILTER flag to enable identifier-based */
        /*      filtering. */
        /* 5. Set psMsgObject->ui32MsgLen to the number of bytes in the */
        /*    expected data frame. */
        /* 6. The buffer pointed to by psMsgObject->pui8MsgData is not used */
        /*    by this call as no data is present at the time of the call. */
        /* 7. Call this function with ui32ObjID set to one of the 32 object */
        /*    buffers.  If you specify a message object buffer that already */
        /*    contains a message definition, it is overwrit- ten. */

        /* 1. */ eMsgType = MSG_OBJ_TYPE_RX;
        /* 2. */ psMsgObject->ui32MsgID = 0; /* initial message id */
        /* 3. */ psMsgObject->ui32MsgIDMask = 0xFFFFFFFF;
        /* 4. */ psMsgObject->ui32Flags = MSG_OBJ_RX_INT_ENABLE;
        /* 5. */ psMsgObject->ui32MsgLen = num_data_frame_bytes;
        /* 6. */
    }
    /* Fin. */ CANMessageSet(ui32Base, ui32ObjID, psMsgObject, eMsgType);
    /**************************/
    /* End CAN Initialization */
    /**************************/

    /* main never terminates */
    while (1);
}
