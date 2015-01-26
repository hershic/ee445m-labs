#include "uart.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

#include "driverlib/pin_map.h"
#include "driverlib/gpio.h"
#include "driverlib/uart.h"

#include "utils/ustdlib.h"

/*
 * \var long uart_active_channel
 * \brief Allows for modal interaction with uart channels.
 */
long uart_active_channel = UART_UNUSED;

void uart_set_active_channel(const long channel) {

  uart_active_channel = channel;
}

void uart_clear_active_channel() {

  uart_active_channel = UART_UNUSED;
}

bool uart_has_active_channel() {

  return uart_active_channel == UART_UNUSED;
}

void uart_init() {

  uart_init_(uart_active_channel);
}

/* TODO: update uart0_base with channel */
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

void uart_send(const char* text) {

  uart_send_(uart_active_channel, text);
}

void uart_send_(const long channel, const char* text) {

  uint32_t cnt = ustrlen(text);
  char* ptr = (char*)text;

  while(cnt--) {
    UARTCharPutNonBlocking(channel, *(ptr++));
  }
}

char uart_get_char() {

  return uart_get_char_(uart_active_channel);
}

/* TODO: update uart0_base with channel */
char uart_get_char_(const long channel) {

  return UARTCharGetNonBlocking(UART0_BASE);
}

char* uart_get_string(const long string_length) {

  return uart_get_string_(uart_active_channel, string_length);
}

/* TODO: replace UART0_BASE with channel */
char* uart_get_string_(const long channel,
		       const long string_length) {

  char* read;
  uint32_t ui32Status;

  long remaining_chars = string_length;

  /* \buffer is a null-terminated string */
  char* buffer = mem_malloc(string_length*sizeof(char));
  buffer[string_length] = 0;

  /* Get the interrrupt status. */
  ui32Status = UARTIntStatus(UART0_BASE, true);

  /* Clear the asserted interrupts. */
  UARTIntClear(UART0_BASE, ui32Status);

  while(UARTCharsAvail(UART0_BASE) && remaining_chars > 0) {
    buffer[remaining_chars - string_length] = uart_get_char_(channel);
    remaining_chars--;
  }

  return buffer;
}
