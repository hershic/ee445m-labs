/* -*- mode: c++; c-basic-offset: 4; -*- */
#ifndef __uartpp__
#define __uartpp__

#include <stdarg.h>
#include <stdint.h>
#include <stdbool.h>

#include "interruptable.hpp"

/*! \addtogroup UART
 * @{
 */

/*! Default uart baud rate in today's modern world. */
#define UART_DEFAULT_BAUD_RATE 115200

/*! Default max length string that uart may return */
const uint32_t UART_DEFAULT_MAX_GET_STRING_LENGTH = 32;

/*! A reference to a memory location on the ARM Cortex M4. */
typedef uint32_t memory_address_t;

/*! A representation of a periodic frequency. */
typedef uint32_t frequency_t;

class uart : public interruptable {
private:
    uint32_t baud_rate;
    memory_address_t channel;
    memory_address_t interrupt;

    void vprintf(const char *pcString, va_list vaArgP);

public:
     /*! Flag for proper handling of newlines input from terminal. */
    static bool LAST_WAS_CR;

    char buffer[UART_DEFAULT_MAX_GET_STRING_LENGTH];

    uart();
    uart(uint32_t uart_baud_rate, memory_address_t uart_channel,
         memory_address_t uart_interrupt);

    /*! Enable the uart. */
    virtual void start(void);

    /*! Disable the uart. */
    virtual void stop(void);

    /*! Acknowledge interrupt. */
    virtual uint32_t ack(void);

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

    /*! Printf over uart. */
    void printf(const char *pcString, ...);

    /*! Printf atomically over uart. */
    void atomic_printf(const char *pcString, ...);
};

#endif

/*! End doxygen group
 * @}
 */
