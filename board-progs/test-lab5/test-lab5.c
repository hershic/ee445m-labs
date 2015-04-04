/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave 2015-04-03 */
/* Revision history: Look in Git FGT */

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
#include "driverlib/fpu.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/pin_map.h"
#include "driverlib/sysctl.h"
#include "driverlib/rom.h"

#include "libshell/shell.h"
#include "libnotify/notify.h"
#include "libhw/hardware.h"
#include "libuart/uart.h"
#include "libstd/nexus.h"
#include "libos/system.h"
#include "libos/os.h"

#include "libfatfs/diskio.h"
#include "libfatfs/ff.h"
#include "libdisplay/stdio_hershic.h"
#include "libdisplay/ST7735.h"

/* #include "inc/tm4c123gh6pm.h" */

#include "driverlib/sysctl.h"

int doctor() {

     UARTCharPut(UART0_BASE, 'd');
    /* uart_send_string("Well what did you expect would happen? You're dreaming!\n"); */
    return EXIT_SUCCESS;
}

int witch(char* args) {

     uart_send_string("no help here\r\n");
     uart_send_string(args);
     uart_send_string("\r\n");
     return EXIT_SUCCESS;
}

int main(void){
    UINT successfulreads, successfulwrites;
    uint8_t c, x, y;
    /* PLL_Init();    // 80 MHz */

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    ST7735_InitR(INITR_REDTAB);
    ST7735_FillScreen(0);                 // set screen to black

    os_threading_init();
    schedule(hw_daemon, 100 Hz, DL_SOFT);
    system_init();
    system_register_command((const char*) "doctor", doctor);
    system_register_command((const char*) "witch", witch);

    /* Initialize hardware devices */
    uart_metadata_init(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
    hw_init(HW_UART, uart_metadata);

    /* Initialize the shell and the system it interacts with */
    shell_spawn();

    IntMasterEnable();
    os_launch();

    /* main function never returns */
    postpone_death();

}
