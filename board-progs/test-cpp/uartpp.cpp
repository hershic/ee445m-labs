#include "uartpp.hpp"

#include "inc/hw_memmap.h"
#include "driverlib/pin_map.h"
#include "driverlib/interrupt.h"
#include "driverlib/sysctl.h"
#include "driverlib/rom_map.h"

#include "driverlib/gpio.h"
#include "driverlib/uart.h"

uint32_t ustrlen(const char* s) {

    uint32_t len = 0;
    while(s[len]) { ++len; }
    return len;
}

uart::uart() {}

/* \warning currently only allows uart from GPIO_PORTA on the TM4C123GXL */
uart::uart(uint32_t uart_baud_rate, memory_address_t uart_channel,
           memory_address_t uart_interrupt) {

    baud_rate = uart_baud_rate;
    channel = uart_channel;
    interrupt = uart_interrupt;

    SysCtlPeripheralEnable(SYSCTL_PERIPH_UART0 +
                           (uart_channel - UART0_BASE) / 0x1000);

    /* todo: parametrize */
    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOA);
    GPIOPinConfigure(GPIO_PA0_U0RX);
    GPIOPinConfigure(GPIO_PA1_U0TX);
    GPIOPinTypeUART(GPIO_PORTA_BASE, GPIO_PIN_0 | GPIO_PIN_1);

    UARTConfigSetExpClk(channel, SysCtlClockGet(), baud_rate,
                        (UART_CONFIG_WLEN_8 | UART_CONFIG_STOP_ONE |
                         UART_CONFIG_PAR_NONE));

    enable();
}

void uart::send_string(const char* str) {

    uint32_t len = ustrlen(str);
    char* ptr = (char*)str;

    while(len--) {
        while(!UARTSpaceAvail(channel)) {}
        send_char(*(ptr++));
    }
}

static uint32_t g_ui32Base = UART0_BASE;
static const char * const g_pcHex = "0123456789abcdef";

int
UARTwrite(memory_address_t channel, const char *pcBuf, uint32_t ui32Len)
{
#ifdef UART_BUFFERED
    unsigned int uIdx;

    // Send the characters
    for(uIdx = 0; uIdx < ui32Len; uIdx++) {
        // If the character to the UART is \n, then add a \r before it so that
        // \n is translated to \n\r in the output.
        if(pcBuf[uIdx] == '\n') {
            if(!TX_BUFFER_FULL) {
                g_pcUARTTxBuffer[g_ui32UARTTxWriteIndex] = '\r';
                ADVANCE_TX_BUFFER_INDEX(g_ui32UARTTxWriteIndex);
            } else {
                // Buffer is full - discard remaining characters and return.
                break;
            }
        }

        // Send the character to the UART output.
        if(!TX_BUFFER_FULL) {
            g_pcUARTTxBuffer[g_ui32UARTTxWriteIndex] = pcBuf[uIdx];
            ADVANCE_TX_BUFFER_INDEX(g_ui32UARTTxWriteIndex);
        } else {
            // Buffer is full - discard remaining characters and return.
            break;
        }
    }

    // If we have anything in the buffer, make sure that the UART is set
    // up to transmit it.
    if(!TX_BUFFER_EMPTY) {
        UARTPrimeTransmit(g_ui32Base);
        UARTIntEnable(channel, UART_INT_TX);
    }

    // Return the number of characters written.
    return(uIdx);
#else
    unsigned int uIdx;

    // Send the characters
    for(uIdx = 0; uIdx < ui32Len; uIdx++) {
        // If the character to the UART is \n, then add a \r before it so that
        // \n is translated to \n\r in the output.
        if(pcBuf[uIdx] == '\n') {
            UARTCharPut(channel, '\r');
        }

        // Send the character to the UART output.
        UARTCharPut(channel, pcBuf[uIdx]);
    }

    // Return the number of characters written.
    return(uIdx);
#endif
}

void uart::vprintf(const char *pcString, va_list vaArgP) {
    uint32_t ui32Idx, ui32Value, ui32Pos, ui32Count, ui32Base, ui32Neg;
    char *pcStr, pcBuf[16], cFill;

    // Loop while there are more characters in the string.
    while(*pcString)
    {
        // Find the first non-% character, or the end of the string.
        for(ui32Idx = 0;
            (pcString[ui32Idx] != '%') && (pcString[ui32Idx] != '\0');
            ui32Idx++) {}

        // Write this portion of the string.
        UARTwrite(channel, pcString, ui32Idx);

        // Skip the portion of the string that was written.
        pcString += ui32Idx;

        // See if the next character is a %.
        if(*pcString == '%') {

            // Skip the %.
            pcString++;

            // Set the digit count to zero, and the fill character to space
            // (in other words, to the defaults).
            ui32Count = 0;
            cFill = ' ';

            // It may be necessary to get back here to process more characters.
            // Goto's aren't pretty, but effective.  I feel extremely dirty for
            // using not one but two of the beasts.
again:

            // Determine how to handle the next character.
            switch(*pcString++) {

                // Handle the digit characters.
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    // If this is a zero, and it is the first digit, then the
                    // fill character is a zero instead of a space.
                    if((pcString[-1] == '0') && (ui32Count == 0)) {
                        cFill = '0';
                    }

                    // Update the digit count.
                    ui32Count *= 10;
                    ui32Count += pcString[-1] - '0';

                    // Get the next character.
                    goto again;

                // Handle the %c command.
                case 'c':
                    // Get the value from the varargs.
                    ui32Value = va_arg(vaArgP, uint32_t);

                    // Print out the character.
                    UARTwrite(channel, (char *)&ui32Value, 1);

                    // This command has been handled.
                    break;

                // Handle the %d and %i commands.
                case 'd':
                case 'i':
                    // Get the value from the varargs.
                    ui32Value = va_arg(vaArgP, uint32_t);

                    // Reset the buffer position.
                    ui32Pos = 0;

                    // If the value is negative, make it positive and indicate
                    // that a minus sign is needed.
                    if((int32_t)ui32Value < 0) {

                        // Make the value positive.
                        ui32Value = -(int32_t)ui32Value;
                        // Indicate that the value is negative.
                        ui32Neg = 1;
                    } else {
                        // Indicate that the value is positive so that a minus
                        // sign isn't inserted.
                        ui32Neg = 0;
                    }

                    // Set the base to 10.
                    ui32Base = 10;

                    // Convert the value to ASCII.
                    goto convert;

                // Handle the %s command.
                case 's':
                    // Get the string pointer from the varargs.
                    pcStr = va_arg(vaArgP, char *);

                    // Determine the length of the string.
                    for(ui32Idx = 0; pcStr[ui32Idx] != '\0'; ui32Idx++) {}

                    // Write the string.
                    UARTwrite(channel, pcStr, ui32Idx);

                    // Write any required padding spaces
                    if(ui32Count > ui32Idx) {
                        ui32Count -= ui32Idx;
                        while(ui32Count--) {
                            UARTwrite(channel, " ", 1);
                        }
                    }
                    break;

                // Handle the %u command.
                case 'u':
                    // Get the value from the varargs.
                    ui32Value = va_arg(vaArgP, uint32_t);

                    // Reset the buffer position.
                    ui32Pos = 0;

                    // Set the base to 10.
                    ui32Base = 10;

                    // Indicate that the value is positive so that a minus sign
                    // isn't inserted.
                    ui32Neg = 0;

                    // Convert the value to ASCII.
                    goto convert;

                // Handle the %x and %X commands.  Note that they are treated
                // identically; in other words, %X will use lower case letters
                // for a-f instead of the upper case letters it should use.  We
                // also alias %p to %x.
                case 'x':
                case 'X':
                case 'p':
                    // Get the value from the varargs.
                    ui32Value = va_arg(vaArgP, uint32_t);

                    // Reset the buffer position.
                    ui32Pos = 0;

                    // Set the base to 16.
                    ui32Base = 16;

                    // Indicate that the value is positive so that a minus sign
                    // isn't inserted.
                    ui32Neg = 0;

                    // Determine the number of digits in the string version of
                    // the value.
convert:
                    for(ui32Idx = 1;
                        (((ui32Idx * ui32Base) <= ui32Value) &&
                         (((ui32Idx * ui32Base) / ui32Base) == ui32Idx));
                        ui32Idx *= ui32Base, ui32Count--) {}

                    // If the value is negative, reduce the count of padding
                    // characters needed.
                    if(ui32Neg) {
                        ui32Count--;
                    }

                    // If the value is negative and the value is padded with
                    // zeros, then place the minus sign before the padding.
                    if(ui32Neg && (cFill == '0')) {
                        // Place the minus sign in the output buffer.
                        pcBuf[ui32Pos++] = '-';

                        // The minus sign has been placed, so turn off the
                        // negative flag.
                        ui32Neg = 0;
                    }

                    // Provide additional padding at the beginning of the
                    // string conversion if needed.
                    if((ui32Count > 1) && (ui32Count < 16)) {
                        for(ui32Count--; ui32Count; ui32Count--) {
                            pcBuf[ui32Pos++] = cFill;
                        }
                    }

                    // If the value is negative, then place the minus sign
                    // before the number.
                    if(ui32Neg) {
                        // Place the minus sign in the output buffer.
                        pcBuf[ui32Pos++] = '-';
                    }

                    // Convert the value into a string.
                    for(; ui32Idx; ui32Idx /= ui32Base) {
                        pcBuf[ui32Pos++] =
                            g_pcHex[(ui32Value / ui32Idx) % ui32Base];
                    }

                    // Write the string.
                    UARTwrite(channel, pcBuf, ui32Pos);

                    break;

                // Handle the %% command.
                case '%':
                    // Simply write a single %.
                    UARTwrite(channel, pcString - 1, 1);
                    break;

                // Handle all other commands.
                default:
                     // Indicate an error.
                    UARTwrite(channel, "ERROR", 5);
                     break;
            }
        }
    }
}

inline
int32_t StartCritical() {
    asm("MRS    R0, PRIMASK  ;// save old status\n");
    asm("CPSID  I            ;// mask all (except faults)\n");
}

/*! End a critical section by restoring a previously saved PRIMASK.
 * \param PRIMASK to restore
 */
inline
void EndCritical(int32_t primask) {
    /* asm("MSR    PRIMASK, R0\n"); */

    /*! bug: this line should be removed in favor of the above to
     *  avoid blindly enable interrupts, but instead enabling
     *  interrupts only if they were previously enabled before the
     *  last \StartCritical function call. */
    asm("CPSIE I");
}

void uart::atomic_printf(const char *pcString, ...) {

    va_list vaArgP;
    va_start(vaArgP, pcString);

    uint32_t ui32Status = StartCritical();
    vprintf(pcString, vaArgP);
    EndCritical(ui32Status);

    va_end(vaArgP);
}

void uart::printf(const char *pcString, ...) {

    va_list vaArgP;
    va_start(vaArgP, pcString);

    vprintf(pcString, vaArgP);

    va_end(vaArgP);
}


void uart::send_char(const char ch) {

    UARTCharPut(channel, ch);
}

void uart::send_newline(void) {

    send_string("\r\n");
}

char uart::get_char(void) {

    return UARTCharGet(channel) & 0xFF;
}

uint32_t uart::ack(void) {

    uint32_t ui32Status;
    ui32Status = UARTIntStatus(channel, true);
    UARTIntClear(channel, ui32Status);
    return ui32Status;
}

char* uart::get_string(const uint32_t length) {

    uint32_t remaining_chars = (uint32_t) length;

    ack();

    while(UARTCharsAvail(channel) && (remaining_chars > 0)) {
        buffer[remaining_chars-- - length] = get_char();
    }
    buffer[length] = 0;

    return buffer;
}

void uart::enable(void) {

    IntEnable(interrupt);
    UARTIntEnable(channel, UART_INT_RX | UART_INT_RT);
}

void uart::disable(void) {

    IntDisable(interrupt);
    UARTIntDisable(channel, UART_INT_RX | UART_INT_RT);
}
