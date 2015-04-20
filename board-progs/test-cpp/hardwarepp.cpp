#include "hardwarepp.hpp"

#include "inc/hw_ints.h"
#include "inc/hw_memmap.h"

#include "libnotify/notify.h"

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
