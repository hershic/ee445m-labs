/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __uartpp__
#define __uartpp__

#include <stdint.h>
#include <stdbool.h>

#include "libstd/nexus.h"

/*! \addtogroup UART
 * @{
 */

/*! Flag for proper handling of newlines input from terminal. */
static bool UART_LAST_WAS_CR;

/*! Default uart baud rate in today's modern world. */
#define UART_DEFAULT_BAUD_RATE 115200

/*! Default max length string that uart may return */
#define UART_DEFAULT_MAX_GET_STRING_LENGTH 64

class uart {
private:
    uint32_t baud_rate;
    memory_address_t channel;
    memory_address_t interrupt;

public:
    char buffer[UART_DEFAULT_MAX_GET_STRING_LENGTH];

    uart();
    uart(uint32_t uart_baud_rate, memory_address_t uart_channel,
         memory_address_t uart_interrupt);

    /*! Enable the uart. */
    void enable(void);

    /*! Disable the uart. */
    void disable(void);

    /*! Acknowledge interrupt. */
    void ack(void);

    /*! Send a char. */
    void send_char(const char);

    /*! Send a string. */
    void send_string(const char*);

    /*! Send a newline and carriage return; */
    void send_newline(void);

    /*! Receive a char. */
    char get_char(void);

    /*! Receive a string of LENGTH characters. */
    char* get_string(const uint32_t length);
}

#endif

/*! End doxygen group
 * @}
 */
