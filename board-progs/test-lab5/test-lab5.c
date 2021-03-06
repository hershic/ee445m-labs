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
static FIL filehandle;
static DIR dirhandle;

extern FIL logfilehandle;
extern bool logging_ready;

#define GP_BUFFER_LEN 512
unsigned char buffer[GP_BUFFER_LEN];

#define logfilename "log"

inline void open_log() {

    if (logging_ready == 0) {
        f_open(&logfilehandle, logfilename, FA_OPEN_ALWAYS | FA_WRITE);
        f_lseek(&logfilehandle, logfilehandle.fsize);
        logging_ready = 1;
    }
}

inline void close_log() {

    if (logging_ready == 1) {
        f_close(&logfilehandle);
        logging_ready = 0;
    }
}

/* Mount the sdcard. */
int mount(char* args) {

    FRESULT MountFresult = f_mount(&g_sFatFs, "", 0);
    if(MountFresult != FR_OK) {
        uart_send_string("f_mount_error\r\n");
    }
    open_log();
    return (int32_t)MountFresult;
}

/* Unmount the sdcard. */
int umount(char* args) {

    close_log();
    FRESULT MountFresult = f_mount(0, "", 0);
    if(MountFresult != FR_OK){
        uart_send_string("f_umount_error\r\n");
    }
    return (int32_t)MountFresult;
}

/* View a file. */
int cat(char* args) {

    UINT successfulreads;
    uint8_t c;
    FRESULT Fresult;

    Fresult = f_open(&filehandle, args, FA_READ);
    if(Fresult == FR_OK){
        // get a character in 'c' and the number of successful reads in 'successfulreads'
        Fresult = f_read(&filehandle, &c, 1, &successfulreads);
        while((Fresult == FR_OK) && (successfulreads == 1)) {
            uart_send_char(c);
            /* get the next character in 'c' */
            Fresult = f_read(&filehandle, &c, 1, &successfulreads);
        }
        // close the file
        Fresult = f_close(&filehandle);
    }
    return (int32_t)Fresult;
}

/* List the structure of a directory. */
int ls(char* args) {

    int32_t i;
    FRESULT result;
    FILINFO fileinfo;

    result = f_opendir(&dirhandle, args);
    if (result == FR_OK) {

        while (1) {
            result = f_readdir(&dirhandle, &fileinfo);
            if (result != FR_OK || fileinfo.fname[0] == 0) {
                break;
            }

            if (fileinfo.fattrib & AM_DIR) {
                uart_send_string(fileinfo.fname);
                uart_send_string("/\r\n");
            } else {
                uart_send_string(fileinfo.fname);
                uart_send_string("\r\n");
            }
        }
        result = f_closedir(&dirhandle);
    }
    return result;
}

/* Create a dir. */
int mkdir(char* args) {

    FRESULT MkdirFresult = f_mkdir(args);
    if(MkdirFresult != FR_OK){
        uart_send_string("f_mkdir_error\r\n");
    }
    return (int32_t)MkdirFresult;
}

/* Remove a file/empty dir. */
int rm(char* args) {

    FRESULT result;
    if (ustrcmp(args, logfilename) == 0) {
        close_log();
    }

    result = f_unlink(args);

    return (uint32_t)result;
}

/* Write a string to a file. The file is created or overwritten if one
   ">" is given. The file is created or appended to if ">>" is
   given. Takes in a ">"/">>"-terminated string to echo to the given
   file. */
int echo(char* args) {

    char* file = args;
    int8_t append;
    UINT bytes_written;
    uint32_t bytes_to_write;
    FRESULT result;

    while (file[0] != '>') {
        if (file[0] == 0) {
            /* command failed */
            return 1;
        }
        ++file;
    }

    file[-1] = file[-1] == ' ' ? 0 : file[-1];
    file[0] = 0;
    append = (file[1] == '>');
    file += append + 1 + (file[append + 1] == ' ');

    bytes_to_write = ustrlen(args);

    if (append) {
        result = f_open(&filehandle, file, FA_OPEN_ALWAYS | FA_WRITE);
    } else {
        result = f_open(&filehandle, file, FA_CREATE_ALWAYS | FA_WRITE);
    }

    /* check if the open was successful before continuing */
    if (result == FR_OK) {
        if (append) {
            f_lseek(&filehandle, filehandle.fsize);
            f_write(&filehandle, "\r\n", 2, &bytes_written);
        }
        result = f_write(&filehandle, args, bytes_to_write, &bytes_written);
        if (result == FR_OK) {
            result = f_close(&filehandle);
        }
    }
    return (uint32_t) result;
}

/* Change directory. */
int cd(char* args) {

    FRESULT result = f_chdir(args);
    return (uint32_t)result;
}

/* Create a file if it doesn't already exist. */
int touch(char* args) {

    FRESULT Fresult;

    Fresult = f_open(&filehandle, args, FA_CREATE_NEW);
    if(Fresult == FR_OK) {
        Fresult |= f_close(&filehandle);
    }
    return (uint32_t)Fresult;
}

/* Format the sdcard. */
int mkfs(char* args) {

    FRESULT result = f_mkfs("", 0, 0);
    return (uint32_t)result;
}

/* Print working directory. */
int pwd(char* args) {

    FRESULT result = f_getcwd(buffer, GP_BUFFER_LEN);
    if (result == FR_OK) {
        uart_send_string(buffer);
    }
    return (uint32_t)result;
}

int main(void){

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);

    ST7735_InitR(INITR_REDTAB);
    ST7735_FillScreen(0);

    os_threading_init();
    schedule(hw_daemon, 100 Hz, DL_SOFT);
    system_init();
    system_register_command((const char*) "mount", mount);
    system_register_command((const char*) "umount", umount);
    system_register_command((const char*) "cat", cat);
    system_register_command((const char*) "pwd", pwd);
    system_register_command((const char*) "ls", ls);
    system_register_command((const char*) "mkdir", mkdir);
    system_register_command((const char*) "rm", rm);
    system_register_command((const char*) "echo", echo);
    system_register_command((const char*) "touch", touch);
    system_register_command((const char*) "mkfs", mkfs);
    system_register_command((const char*) "cd", cd);

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
