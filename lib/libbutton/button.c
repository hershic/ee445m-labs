/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave 2015-02-22 */
/* Revision History: Look in Git FGT */

#include "button.h"

#include "libhw/hardware.h"
#include "libnotify/notify.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include <stdlib.h>

#include "driverlib/pin_map.h"
#include "driverlib/gpio.h"
#include "driverlib/sysctl.h"


/* TODO: Currently, only left works (PF4). From Valvano: */
/* NOTE: The NMI (non-maskable interrupt) is on PF0.  That means that
   the Alternate Function Select, Pull-Up Resistor, Pull-Down
   Resistor, and Digital Enable are all locked for PF0 until a value
   of 0x4C4F434B is written to the Port F GPIO Lock Register.  After
   Port F is unlocked, bit 0 of the Port F GPIO Commit Register must
   be set to allow access to PF0's control registers.  On the LM4F120,
   the other bits of the Port F GPIO Commit Register are hard-wired to
   1, meaning that the rest of Port F can always be freely
   re-configured at any time.  Requiring this procedure makes it
   unlikely to accidentally re-configure the JTAG pins as GPIO, which
   can lock the debugger out of the processor and make it permanently
   unable to be debugged or re-programmed. */
/* [[http://users.ece.utexas.edu/~valvano/arm/InputOutput_4C123.zip]] */

void button_init(hw_metadata metadata) {

    GPIODirModeSet(GPIO_PORTF_BASE, metadata.button.pin, GPIO_DIR_MODE_IN);
    GPIOPadConfigSet(GPIO_PORTF_BASE, metadata.button.pin,
		     GPIO_STRENGTH_2MA, GPIO_PIN_TYPE_STD_WPU);
    GPIOIntEnable(GPIO_PORTF_BASE, metadata.button.pin);
}

void button_set_interrupt(hw_metadata metadata) {

    if (metadata.button.interrupt) {
        GPIOIntTypeSet(metadata.button.base,
		       metadata.button.pin,
		       metadata.button.interrupt);
        IntEnable(INT_GPIOF_TM4C123);
    } else {
        IntDisable(INT_GPIOF_TM4C123);
    }
}
