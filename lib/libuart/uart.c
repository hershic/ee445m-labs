/* -*- mode: c; c-basic-offset: 4; -*- */
#include <stdint.h>

#include "uart.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include "driverlib/pin_map.h"
#include "driverlib/gpio.h"
#include "driverlib/uart.h"

#include "libstd/nexus.h"
#include "libnotify/notify.h"

/*! \addtogroup UART
 * @{
 */

/*! \UART_BUFFER max length */
#define UART_BUFFER_LEN               128

/*! Buffer for reading and returning chars from hardware uart fifo. */
static char UART_BUFFER[UART_BUFFER_LEN];

/*! Allows for modal interaction with uart channels. */
hw_metadata uart_active_metadata;

/*! \warning If you set the active uart channel yourself, ensure
 *  \metadata is already initialized or you're gonna have a bad
 *  time. */
inline
void uart_set_active_metadata(hw_metadata metadata) {

    uart_active_metadata = metadata;
}

/* Initializing has the side effect of setting the active channel. */
void uart_init(hw_metadata metadata) {

    uart_set_active_metadata(metadata);
    GPIOPinConfigure(GPIO_PA0_U0RX);
    GPIOPinConfigure(GPIO_PA1_U0TX);
    /* todo: parametrize */
    GPIOPinTypeUART(GPIO_PORTA_BASE, GPIO_PIN_0 | GPIO_PIN_1);

    /* This is the HF aculprit Thursday February 26, 2015 */
    UARTConfigSetExpClk(metadata.uart.channel, SysCtlClockGet(),
                        metadata.uart.baud_rate,
                        (UART_CONFIG_WLEN_8 | UART_CONFIG_STOP_ONE |
                         UART_CONFIG_PAR_NONE));

    /* Enable the UART interrupt. */
    IntEnable(metadata.uart.interrupt);
    UARTIntEnable(metadata.uart.channel, UART_INT_RX | UART_INT_RT);
}

inline
void uart_send_char(const char text) {

    uart_send_char_(uart_active_metadata, text);
}

inline
void uart_send_char_(hw_metadata metadata, const char text) {

    while(!UARTSpaceAvail(metadata.uart.channel)) {}
    UARTCharPut(metadata.uart.channel, text);
}

inline
void uart_send_string(const char* text) {

    uart_send_string_(uart_active_metadata, text);
}

inline
void uart_send_newline(void) {

    uart_send_string_(uart_active_metadata, "\r\n");
}

void uart_send_string_(hw_metadata metadata, const char* text) {

    uint32_t cnt = ustrlen(text);
    char* ptr = (char*)text;

    while(cnt--) {
        while(!UARTSpaceAvail(metadata.uart.channel)) {}
        UARTCharPut(metadata.uart.channel, *(ptr++));
    }
}

inline
void uart_send_udec(uint32_t n) {

    uart_send_udec_(uart_active_metadata, n);
}

void uart_send_udec_(hw_metadata metadata, uint32_t n) {

  if(n >= 10){
      uart_send_udec_(metadata, n/10);
  }
  uart_send_char_(metadata, (n % 10) + '0');
}

inline
char uart_get_char() {

    return uart_get_char_(uart_active_metadata);
}

char uart_get_char_(hw_metadata metadata) {

    uint32_t ui32Status;
    /* Get the interrrupt status. */
    ui32Status = UARTIntStatus(metadata.uart.channel, true);

    /* Clear the asserted interrupts. */
    UARTIntClear(metadata.uart.channel, ui32Status);

    char ret = UARTCharGetNonBlocking(metadata.uart.channel);
    return ret;
}

inline
char* uart_get_string(const long string_length) {

    return uart_get_string_(uart_active_metadata, string_length);
}

char* uart_get_string_(hw_metadata metadata,
                       const long string_length) {

    uint32_t ui32Status;
    long remaining_chars = string_length;

    /* \UART_BUFFER is a null-terminated string */
    UART_BUFFER[string_length] = 0;

    /* Get the interrrupt status. */
    ui32Status = UARTIntStatus(metadata.uart.channel, true);

    /* Clear the asserted interrupts. */
    UARTIntClear(metadata.uart.channel, ui32Status);

    while(UARTCharsAvail(metadata.uart.channel) && remaining_chars > 0) {
        UART_BUFFER[remaining_chars - string_length] =
            UARTCharGet(metadata.uart.channel);
        remaining_chars--;
    }
    return UART_BUFFER;
}

/*! End doxygen group
 * @}
 */
