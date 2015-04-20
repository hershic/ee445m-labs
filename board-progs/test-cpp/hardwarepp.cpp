#include "hardwarepp.hpp"

#include "inc/hw_ints.h"
#include "inc/hw_memmap.h"

#include "libnotify/notify.h"

#include "uartpp.hpp"
#include "driverlib/uart.h"

extern bool UART_LAST_WAS_CR;

/* Client code :*/
/* hardware hw = hardware(); */
/* hw.init_uart(UART_BASE0, INT_UART0); */
/* hw.subscribe(UART0_Handler, pseudo_handler); */

hardware::hardware() {

    BUFFER_UART0_RX = buffer();
    /* list<pseudo_isr> uart_list; */
    /* todo: init uart_list */
    map<int, list<pseudo_isr> > uart_map;
    notifies[UART] = uart_map;
    /* todo: init timer */
}

/* Ensure \hardware_uninitialized = true when you want a constructor call */
hardware hardware::singleton(void) {

    if (hardware_uninitialized) {
        single = hardware();
        hardware_uninitialized = false;
    }
    return single;
}

void hardware::daemon(void) {

    notification note;

    while(1) {
        sem_guard(HW_SEM_UART0) {
            sem_take(HW_SEM_UART0);
            note._char = BUFFER_UART0_RX.get();
            notify(UART0_BASE, INT_UART0, note);
        }
    }
}

void hardware::notify(memory_address_t base, memory_address_t interrupt,
                      notification note) {


}

HW_TYPE hardware::interrupt_type(memory_address_t interrupt) {

    switch(interrupt) {
    case INT_UART0:
    case INT_UART1:
    case INT_UART2:
    case INT_UART3:
    case INT_UART4:
    case INT_UART5:
    case INT_UART6:
    case INT_UART7:
        return UART;
    case INT_TIMER0A:
    case INT_TIMER0B:
    case INT_TIMER1A:
    case INT_TIMER1B:
    case INT_TIMER2A:
    case INT_TIMER2B:
    case INT_TIMER3A:
    case INT_TIMER3B:
    case INT_TIMER4A:
    case INT_TIMER4B:
    case INT_TIMER5A:
    case INT_TIMER5B:
        return TIMER;
    default:
        while(1) {}
    }
}

void hardware::subscribe(memory_address_t interrupt, void (*isr)(notification note)) {

    HW_TYPE int_type = interrupt_type(interrupt);
    map<int, list<pseudo_isr> > pisr = notifies[int_type];

}

void UART0_Handler(void) {

    bool post;
    uint8_t recv;
    /* Get and clear the current interrupt sources */
    uint32_t interrupts = uart::ack(UART0_BASE);

    /* Are we being interrupted due to a received character? */
    if(interrupts & (UART_INT_RX | UART_INT_RT)) {
        /* Get all available chars from the UART */
        while(UARTCharsAvail(UART0_BASE)) {
            recv = (unsigned char) (UARTCharGetNonBlocking(UART0_BASE) & 0xFF);

            /* optional: check for '@echo_off */

            /* Handle backspace by erasing the last character in the
             * buffer */
            switch(recv) {
            case 127:
            case '\b':
                /* If there are any chars to delete, delete the last text */
                if(!buffer_empty(UART0_RX_BUFFER)) {
                    /* Erase previous characters on the user's terminal */
                    UARTCharPut(UART0_BASE, '\b');
                    UARTCharPut(UART0_BASE, ' ');
                    UARTCharPut(UART0_BASE, '\b');
                    /* Decrement the number of chars in the buffer */
                    UART0_RX_BUFFER.get();
                    /* Skip ahead to next buffered char */
                    continue;
                }
                /* if it is empty, somebody is watching so pass along
                 * the backspace */
                break;

            case '\r':
            case '\n':
                if(recv == '\r') {
                    UART_LAST_WAS_CR = true;
                }
                else if (UART_LAST_WAS_CR) {
                    UART_LAST_WAS_CR = false;
                    /* Don't react twice to a single newline */
                    continue;
                }
            case 0x1b:
                /* Regardless of the newline received, our convention
                 * is to mark end-of-lines in a buffer with the CR
                 * character. */
                recv = '\r';

                /* Echo the received character to the newline */
                UARTCharPut(UART0_BASE, '\n');
                break;

            default: break;
            }

            post = !UART0_RX_BUFFER.full();
            /* If there is room in the RX FIFO, store the char there,
             * else dump it. optional: a circular buffer might keep
             * more up-to-date data, considering this is a RTOS */
            /* this could be cleaned up with error-catching in the buffer library */
            if(post) {
                UART0_RX_BUFFER.add(recv);
                sem_post(hardware::HW_SEM_UART0);
            }
        }
    }
}
