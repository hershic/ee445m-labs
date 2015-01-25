#include "uart.h"

#include "inc/hw_memmap.h"
#include "inc/hw_ints.h"

#include <stdint.h>
#include <stdbool.h>

#include "driverlib/pin_map.h"
#include "driverlib/gpio.h"
#include "driverlib/uart.h"

#include "utils/ustdlib.h"

/* TODO: DOXYGENIZE */
/* TODO: should be extended to allow init of all available UARTs */
void uart_init() {

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

/* Text is null-terminated */
void uart_send(const char* text) {

    uint32_t cnt = ustrlen(text);
    char* ptr = (char*)text;

    while(cnt--) {
        UARTCharPutNonBlocking(UART0_BASE, *(ptr++));
    }
}

void uart_recv() {
    uint32_t ui32Status;

    /* Get the interrrupt status. */
    ui32Status = UARTIntStatus(UART0_BASE, true);

    /* Clear the asserted interrupts. */
    UARTIntClear(UART0_BASE, ui32Status);

    /* Loop while there are characters in the receive FIFO. */
    while(UARTCharsAvail(UART0_BASE)) {
        /* Read the next character from the UART and write it back to the UART. */
        UARTCharPutNonBlocking(UART0_BASE, UARTCharGetNonBlocking(UART0_BASE));

        /* WARNING: This clears the UART0 FIFO. Do not use in
           combination with the above line. */
        /* UARTCharGetNonBlocking(UART0_BASE); */

        /* Blink the LED to show a character transfer is occuring. */
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, GPIO_PIN_2);

        /* Delay for 1 millisecond.  Each SysCtlDelay is about 3 clocks. */
        SysCtlDelay(SysCtlClockGet() / (1000 * 3));

        /* Turn off the LED */
        GPIOPinWrite(GPIO_PORTF_BASE, GPIO_PIN_2, 0);
    }
}
