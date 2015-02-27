/* -*- mode: c; c-basic-offset: 4; -*- */
#include "uart.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include <stdint.h>

#include "driverlib/pin_map.h"
#include "driverlib/gpio.h"
#include "driverlib/uart.h"

#include "libstd/nexus.h"
#include "libnotify/notify.h"

#define NDEBUG

#ifndef NDEBUG
#include <stdio.h>
#endif

static char UART_BUFFER[128];

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

/* Initializing has the side effect of setting the active channel. */
void uart_init(hw_metadata metadata) {

    uart_set_active_channel(metadata.uart.channel);
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
#ifndef NDEBUG
    printf("%s channel %d initialized\n", __FUNCTION__, channel);
#endif
}

void uart_send_char(const char text) {

    uart_send_char_(uart_active_channel, text);
}

void uart_send_char_(const long channel, const char text) {

    UARTCharPut(UART0_BASE, text);
}

void uart_send_string(const char* text) {

    uart_send_string_(uart_active_channel, text);
}

void uart_send_string_(const long channel, const char* text) {

    uint32_t cnt = ustrlen(text);
    char* ptr = (char*)text;

    while(cnt--) {
        UARTCharPut(channel, *(ptr++));
    }
}

char uart_get_char() {

    return uart_get_char_(uart_active_channel);
}

/* TODO: update uart0_base with channel */
char uart_get_char_(const long channel) {

    uint32_t ui32Status;

    /* Get the interrrupt status. */
    ui32Status = UARTIntStatus(UART0_BASE, true);

    /* Clear the asserted interrupts. */
    UARTIntClear(UART0_BASE, ui32Status);

    char ret = UARTCharGetNonBlocking(UART0_BASE);

#ifndef NDEBUG
    printf("%s got char: %c", ret);
#endif

    return ret;
}

char* uart_get_string(const long string_length) {

    return uart_get_string_(uart_active_channel, string_length);
}

/* TODO: replace uart0_base with channel */
char* uart_get_string_(const long channel,
                       const long string_length) {

    /* char* buffer; */
    uint32_t ui32Status;

    long remaining_chars = string_length;

    /* \buffer is a null-terminated string */
    /* buffer = malloc(string_length*sizeof(char) + 1); */
    UART_BUFFER[string_length] = 0;

    /* Get the interrrupt status. */
    ui32Status = UARTIntStatus(UART0_BASE, true);

    /* Clear the asserted interrupts. */
    UARTIntClear(UART0_BASE, ui32Status);

    while(UARTCharsAvail(UART0_BASE) && remaining_chars > 0) {
        UART_BUFFER[remaining_chars - string_length] = UARTCharGetNonBlocking(channel);
        remaining_chars--;
    }

#ifndef NDEBUG
    printf("%s Got string: %s\n", __FUNCTION__, UART_BUFFER);
#endif
    return UART_BUFFER;
}
