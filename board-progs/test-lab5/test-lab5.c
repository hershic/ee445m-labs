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

#include "driverlib/sysctl.h"

static FATFS g_sFatFs;

#define GP_BUFFER_LEN 512
unsigned char buffer[GP_BUFFER_LEN];

int mount(char* args) {

    FRESULT MountFresult = f_mount(&g_sFatFs, "", 0);
    if(MountFresult != FR_OK) {
        uart_send_string("f_mount_error\r\n");
    }
    return (int32_t)MountFresult;
}

int umount(char* args) {

    FRESULT MountFresult = f_mount(0, "", 0);
    if(MountFresult != FR_OK){
        uart_send_string("f_umount_error\r\n");
    }
    return (int32_t)MountFresult;
}

int cat(char* args) {

    UINT successfulreads;
    uint8_t c;
    FRESULT Fresult;
    FIL Handle;

    Fresult = f_open(&Handle, args, FA_READ);
    if(Fresult == FR_OK){
        // get a character in 'c' and the number of successful reads in 'successfulreads'
        Fresult = f_read(&Handle, &c, 1, &successfulreads);
        while((Fresult == FR_OK) && (successfulreads == 1)) {
            uart_send_char(c);
            /* get the next character in 'c' */
            Fresult = f_read(&Handle, &c, 1, &successfulreads);
        }
        // close the file
        Fresult = f_close(&Handle);
    }
}

int main(void){
    UINT successfulreads, successfulwrites;

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    ST7735_InitR(INITR_REDTAB);
    ST7735_FillScreen(0);                 // set screen to black

    os_threading_init();
    schedule(hw_daemon, 100 Hz, DL_SOFT);
    system_init();
    system_register_command((const char*) "mount", mount);
    system_register_command((const char*) "umount", umount);
    system_register_command((const char*) "cat", cat);

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
