/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __UART__
#define __UART__

#include <stdbool.h>

#define UART_UNUSED            -1
#define UART_DEFAULT_BAUD_RATE 115200

/*! Create a hardware_metadata struct named _name */
#define uart_metadata_init_(_name, _baud_rate, _channel) \
    hw_metadata _name;					 \
    _name.uart = (hw_uart_metadata) {			 \
        .baud_rate = (uint32_t) _baud_rate,		 \
        .channel = (memory_address_t) _channel		 \
    }

/*! Create a hardware_metadata struct named `uart_metadata' */
#define uart_metadata_init(_baud_rate, _channel)		\
    uart_metadata_init_(uart_metadata, _baud_rate, _channel)

/*! Modally set the active uart channel.
 *  \return void
 */
void uart_set_active_channel(const long channel);

/*! Modally clear the active uart channel.
 *  \return void
 */
void uart_clear_active_channel();

/*! Test whether or not an active uart channel exists.
 *  \return boolean indicating uart active channel is modally set.
 */
bool uart_has_active_channel();

/*! Initialize the active uart channel.
 *  \return void
 */
void uart_init();

/*! Initialize the specified uart channel.
 *  \param channel The uart channel to initialize.
 *  \return void
 */
void uart_init_(const long channel);

/*! Send a char array over the active uart channel.
 *  \param text The null-terminated array of chars to send via uart.
 *  \return void
 */
void uart_send_char(const char text);

/*! Send a char array over the specified uart channel.
 *  \param channel The uart channel to send \text over.
 *  \param text The null-terminated array of chars to send via uart.
 *  \return void
 */
void uart_send_char_(const long channel, const char text);

/*! Send a char array over the active uart channel.
 *  \param text The null-terminated array of chars to send via uart.
 *  \return void
 */
void uart_send_string(const char* text);

/*! Send a char array over the specified uart channel.
 *  \param channel The uart channel to send \text over.
 *  \param text The null-terminated array of chars to send via uart.
 *  \return void
 */
void uart_send_string_(const long channel, const char* text);

/*! Read a char from the active uart channel.
 *  \brief Read a char from the active uart channel.
 *  \details Read a char from the active uart channel.
 *  \returns char read from uart
 */
char uart_get_char();

/*! Read a char from the specified uart channel.
 *  \brief Read a char from the specified uart channel.
 *  \details Read a char from the specified uart channel.
 *  \param channel The uart channel to read.
 *  \returns char read from uart
 */
char uart_get_char_(const long channel);

/*! Read a char array from the active uart channel.
 *  \brief Read a char array from the active uart channel.
 *  \details Read a char array from the active uart channel.
 *  \param string_length The maximum number of characters to read.
 *  \returns char array read from uart
 */
char* uart_get_string(const long string_length);

/*! Read a char array from the specified uart channel.
 *  \brief Read a char array from the active uart channel.
 *  \details Read a char array from the active uart channel.
 *  \param channel The uart channel to read.
 *  \param string_length The maximum number of characters to read.
 *  \returns char array read from uart
 */
char* uart_get_string_(const long channel, const long string_length);

#endif
