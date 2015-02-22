/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Eric Crosson on 2015-02-20 */
/* Revision History: Look in Git FGT */

/* Standard Libs */
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/* TI Includes */
#include "inc/hw_ints.h"
#include "inc/hw_memmap.h"

/* Driverlib Includes */
#include "driverlib/debug.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/systick.h"
#include "driverlib/rom.h"

#define HEARTBEAT_MODAL
#include "libos/os.h"
#include "libheart/heartbeat.h"

void main(void) {

  SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
		 SYSCTL_XTAL_16MHZ);

  IntMasterDisable();

  os_threading_init();

  /* Load and enable the systick timer */
  SysTickPeriodSet(SysCtlClockGet() / 10);
  SysTickEnable();
  SysTickIntEnable();

  /* os_trap_ */
  os_launch();

  /* PONDER: why do interrupts fire without this? */
  IntMasterEnable();

  int32_t sem;
  sem_init(&sem, 1);
  while(1) {
    /* sem_wait */
  }
}
