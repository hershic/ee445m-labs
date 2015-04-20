/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __shellpp__
#define __shellpp__

#include <stdbool.h>

#include "libos/system.h"
#include "libnotify/notify.h"

#include "driverlib/pin_map.h"
#include "driverlib/timer.h"
#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"

/*! \addtogroup Shell
 * @{
 */

/*! PS1 maximum length */
#define SHELL_MAX_PS1_LENGTH 4

/** Maximum length of shell input per command */
#define SHELL_BUFFER_LENGTH 64

class shell {
private:
    char ps1[SHELL_MAX_PS1_LENGTH];
    char cmd[SHELL_BUFFER_LENGTH];
    uint32_t buffer_position;
    uint32_t base;
    int32_t _int;

    inline void cmd_buf_add(char c);
    inline void cmd_buf_del();

public:
    shell();
    ~shell();
    shell(uint32_t uart_base, int32_t uart_int, char* ps1);

    void set_ps1(char* new_ps1);
    void print_ps1(void);
    void shell_print_ps1(void);
    void clear_cmd_buffer(void);

    void shell_uart_handler(notification note);
    exit_status_t execute_cmd(void);
};

#endif

/*! End doxygen group
 * @}
 */
