#include "hardwarepp.hpp"

#include "inc/hw_ints.h"
#include "inc/hw_memmap.h"

#include "libnotify/notify.h"

extern buffer BUFFER_UART0_RX;

/* Client code :*/
/* hardware hw = hardware(); */
/* hw.init_uart(UART_BASE0, INT_UART0); */
/* hw.subscribe(UART0_Handler, pseudo_handler); */

hardware::hardware() {

    BUFFER_UART0_RX = buffer();
    std::map<int, void (*isr)(notification note)> uart_map;
    notifies[UART] = uart_map;
}

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
    default:
        while(1) {}
    }
}

void hardware::subscribe(memory_address_t interrupt, void (*isr)(notification note)) {

    HW_TYPE int_type = interrupt_type(interrupt);
    map<int, list<pseudo_isr> > pisr = notifies[int_type];

}
