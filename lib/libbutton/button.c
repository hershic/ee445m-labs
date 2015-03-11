/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave 2015-02-22 */
/* Revision History: Look in Git FGT */

#include "button.h"

#include "libos/thread_structures.h"
#include "libhw/hardware.h"
#include "libnotify/notify.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"
#include "inc/hw_types.h"
#include "inc/hw_gpio.h"

#include <stdlib.h>

#include "driverlib/pin_map.h"
#include "driverlib/gpio.h"
#include "driverlib/sysctl.h"

void button_init(hw_metadata metadata) {

    GPIODirModeSet(GPIO_PORTF_BASE, metadata.button.pin, GPIO_DIR_MODE_IN);

    HWREG(GPIO_PORTF_BASE + GPIO_O_LOCK) = GPIO_LOCK_KEY;
    HWREG(GPIO_PORTF_BASE + GPIO_O_CR) = 0x01;
    HWREG(GPIO_PORTF_BASE + GPIO_O_LOCK) = 0;

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
