#include "uart.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include <stdint.h>
#include <stdbool.h>

#include "driverlib/pin_map.h"
#include "driverlib/gpio.h"
#include "driverlib/uart.h"

#include "utils/ustdlib.h"

/*!
 *  \brief     UART standard library
 *  \details   UART initialization, transmission, reception.
 *  \author    Hershal Bhave
 *  \author    Eric Crosson
 *  \version   0.1
 *  \date      2015
 *  \pre       None
 *  \bug       No scoreboard to detect UART collisions yet
 *  \warning   Do not bother a UART who is busy slaving for another user.
 *  \copyright GNU Public License.
 */

#define UART_UNUSED -1

/*
 * \var long uart_active_channel
 * \brief Allows for modal interaction with uart channels.
 */
long uart_active_channel = UART_UNUSED;

/*! Modally set the active uart channel.
 *  \return void
 */
void uart_set_active_channel(const long channel) {

  uart_active_channel = channel;
}

/*! Modally clear the active uart channel.
 *  \return void
 */
void uart_clear_active_channel() {

  uart_active_channel = UART_UNUSED;
}

/*! Test whether or not an active uart channel exists.
 *  \return boolean indicating uart active channel is modally set.
 */
bool uart_has_active_channel() {

  return uart_active_channel == UART_UNUSED;
}

/*! Initialize the active uart channel.
 *  \return void
 */
void uart_init() {

  uart_init_(uart_active_channel);
}

/* TODO: update uart0_base with channel */
/*! Initialize the specified uart channel.
 *  \param channel The uart channel to initialize.
 *  \return void
 */
void uart_init_(const long channel) {

  GPIOPinConfigure(GPIO_PA0_U0RX);
  GPIOPinConfigure(GPIO_PA1_U0TX);
  GPIOPinTypeUART(GPIO_PORTA_BASE, GPIO_PIN_0 | GPIO_PIN_1);

  UARTConfigSetExpClk(UART0_BASE, SysCtlClockGet(), 115200,
		      (UART_CONFIG_WLEN_8 | UART_CONFIG_STOP_ONE |
		       UART_CONFIG_PAR_NONE));

  /* Enable the UART interrupt. */
  IntEnable(INT_UART0);
  UARTIntEnable(UART0_BASE, UART_INT_RX | UART_INT_RT);
}

/*! Send a char array over the active uart channel.
 *  \param text The null-terminated array of chars to send via uart.
 *  \return void
 */
void uart_send(const char* text) {

  uart_send_(uart_active_channel, text);
}

/*! Send a char array over the specified uart channel.
 *  \param channel The uart channel to send \text over.
 *  \param text The null-terminated array of chars to send via uart.
 *  \return void
 */
void uart_send_(const long channel, const char* text) {

  uint32_t cnt = ustrlen(text);
  char* ptr = (char*)text;

  while(cnt--) {
    UARTCharPutNonBlocking(channel, *(ptr++));
  }
}

/*! Read a char array from the active uart channel.
 *  \brief Read a char array from the active uart channel.
 *  \details Read a char array from the active uart channel.
 *  \param string_length The maximum number of characters to read.
 */
char* uart_get_string(const long string_length) {

  return uart_get_string_(uart_active_channel, string_length);
}

/* TODO: replace UART0_BASE with channel */
/*! Read a char array from the specified uart channel.
 *  \brief Read a char array from the active uart channel.
 *  \details Read a char array from the active uart channel.
 *  \param channel The uart channel to read.
 *  \param string_length The maximum number of characters to read.
 */
char* uart_get_string_(const long channel,
		       const long string_length) {

  char* read;
  uint32_t ui32Status;

  long remaining_chars = string_length;

  /* \read is a null-terminated string */
  char* read = calloc(string_length, sizeof(char));
  read[string_length] = 0;

  /* Get the interrrupt status. */
  ui32Status = UARTIntStatus(UART0_BASE, true);

  /* Clear the asserted interrupts. */
  UARTIntClear(UART0_BASE, ui32Status);

  while(UARTCharsAvail(UART0_BASE) && remaining_chars > 0) {
    read[remaining_chars - string_length] = uart_get_char_(channel);
    remaining_chars--;
  }

  return read;
}

/*! Read a char from the active uart channel.
 *  \brief Read a char from the active uart channel.
 *  \details Read a char from the active uart channel.
 */
char uart_get_char() {

  return uart_get_char_(uart_active_channel);
}

/* TODO: update uart0_base with channel */
/*! Read a char from the specified uart channel.
 *  \brief Read a char from the specified uart channel.
 *  \details Read a char from the specified uart channel.
 *  \param channel The uart channel to read.
 */
char uart_get_char_(const long channel) {

  return UARTCharGetNonBlocking(UART0_BASE);
}
