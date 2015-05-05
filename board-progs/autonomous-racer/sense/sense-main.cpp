/* -*- mode: c++; c-basic-offset: 4; */
/* Created by Hershal Bhave and Eric Crosson on <2015-03-15 Sun> */
/* Revision History: Look in Git FGT */

#include "adcpp.hpp"
#include "blinker.hpp"
#include "timerpp.hpp"
#include "uartpp.hpp"
#include "shellpp.hpp"
#include "semaphorepp.hpp"
#include "motorpp.hpp"
#include "ir.hpp"
#include "drivepp.hpp"
#include "canpp.hpp"
#include "ctlsysctl.hpp"
#include "pingpp.hpp"

#include "libos/os.h"
#include "libschedule/schedule.h"

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#include "inc/hw_memmap.h"

#include "driverlib/adc.h"
#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"
#include "driverlib/interrupt.h"
#include "driverlib/uart.h"
#include "driverlib/pwm.h"

#define thread(x)                   \
    do {                            \
        x;                          \
        os_surrender_context();     \
    } while(true)

blinker blink;
timer timer0a;
uart uart0;
shell shell0;
adc adc0;
ir ir0, ir1, ir2, ir3;
ping ping0;
semaphore* ping0_sem;

#define can_data_length 5*2
const uint32_t can_msg_id = 1;
const bool can_sender = true;
can can0;
uint8_t can_data[can_data_length];

#define UART0_RX_BUFFER_SIZE 8
static semaphore UART0_RX_SEM;
static buffer<char, UART0_RX_BUFFER_SIZE> UART0_RX_BUFFER;

uint32_t blink_count_red = 0;
uint32_t blink_count_green = 0;
uint32_t blink_count_blue = 0;

void thread_blink_red() {

    thread (
            blink.toggle(PIN_RED);
            ++blink_count_red;
        );
}

void thread_blink_blue() {

    thread (
            blink.toggle(PIN_BLUE);
            ++blink_count_blue;
        );
}

void thread_blink_green() {

    thread (
            blink.toggle(PIN_GREEN);
            ++blink_count_green;
        );
}

void thread_uart_update() {

    thread (
            uart0.atomic_printf("%x %x %x\n\r", blink_count_red,
                                blink_count_blue, blink_count_green);
        );
}

extern "C" void __cxa_pure_virtual() { while (1) {} }

extern "C" void Timer0A_Handler() {
    timer0a.ack();
}

extern "C" void Timer1A_Handler() {
    ping0.handle_timer();
}

extern "C" void GPIOPortD_Handler() {
    ping0.handle_gpio();
}

extern "C" void UART0_Handler(void) {

    if(!(uart0.ack() & (UART_INT_RX | UART_INT_RT))) {
        return;
    }

    while(UARTCharsAvail(UART0_BASE)) {
        char recv = uart0.get_char();

        /* Regardless of newline received, our convention is to
         * mark end-of-lines in a buffer with the CR character. */
        switch(recv) {
        case '\n':
            if (uart::LAST_WAS_CR) {
                uart::LAST_WAS_CR = false;
                continue;
            }
            break;
        case '\r':
            uart::LAST_WAS_CR = true;
            break;
        case 0x1b:
            recv = '\r';
            break;
        default: break;
        }
        UART0_RX_BUFFER.notify((const int8_t) recv);
        blink.blink(PIN_RED);
    }
}

extern "C" void ADC0Seq0_Handler(void) {

    adc0.ack();
    adc0.sample();

    ir0.sample();
    ir1.sample();
    ir2.sample();
    ir3.sample();
}

extern "C" void CAN0_Handler(void) {

    uint32_t message_id = can0.ack();

    switch(message_id) {
    case 1:
        can0.recv_sem.post();
        break;
    default:
        can0.error_tx();
        break;
    }
}

void ping_handler() {

    ping0.start();

    while(1){
        if (ping0_sem->guard()) {
            ping0.sample();
            /* uart0.printf("ping: %u\n", ping0.average()); */
        }
        os_surrender_context();
    }
}

void can_handler(void) {

    while(1) {
        if(can0.recv_sem.guard()) {

            can0.get(can_data);
            uart0.printf("Received CAN data: %0X %0X %0X %0X %0X %0X %0X %0X ",
                         can_data[0], can_data[1], can_data[3], can_data[4],
                         can_data[4], can_data[5], can_data[6], can_data[7]);
        }
        os_surrender_context();
    }
}

void can_transmitter(void) {

    uint16_t sens_ir_left;
    uint16_t sens_ir_left_front;
    uint16_t sens_ir_right;
    uint16_t sens_ir_right_front;
    uint16_t sens_ping_front;

    uint8_t* ir_left_ptr;
    uint8_t* ir_left_front_ptr;
    uint8_t* ir_right_ptr;
    uint8_t* ir_right_front_ptr;
    uint8_t* ping_front_ptr;

    ir_left_ptr = (uint8_t*)(&sens_ir_left);
    ir_left_front_ptr = (uint8_t*)(&sens_ir_left_front);
    ir_right_ptr = (uint8_t*)(&sens_ir_right);
    ir_right_front_ptr = (uint8_t*)(&sens_ir_right_front);
    ping_front_ptr = (uint8_t*)(&sens_ping_front);

    while(1) {

        sens_ir_left = ir0.distance(); /* PE3 */
        sens_ir_left_front = ir1.distance(); /* PE2 */
        sens_ir_right = ir2.distance();      /* PE1 */
        sens_ir_right_front = ir3.distance(); /* PE0 */
        sens_ping_front = ping0.distance();

        can_data[0] = ir_left_ptr[0];
        can_data[1] = ir_left_ptr[1];

        can_data[2] = ir_left_front_ptr[0];
        can_data[3] = ir_left_front_ptr[1];

        can_data[4] = ir_right_ptr[0];
        can_data[5] = ir_right_ptr[1];

        can_data[6] = ir_right_front_ptr[0];
        can_data[7] = ir_right_front_ptr[1];

        can_data[8] = ping_front_ptr[0];
        can_data[9] = ping_front_ptr[1];

        can0.transmit(can_data, can_data_length);

        uart0.printf("l: %u lf: %u r: %u rf: %u pf: %u\r\n",
                            sens_ir_left, sens_ir_left_front,
                            sens_ir_right, sens_ir_right_front,
                            sens_ping_front);

        blink.toggle(PIN_BLUE);

        os_surrender_context();
    }
}

void shell_handler() {

    while(1) {
        if(UART0_RX_SEM.guard()) {

            bool ok;
            char recv = UART0_RX_BUFFER.get(ok);

            if(ok) { shell0.accept(recv); }
        }
        os_surrender_context();
    }
}

void can_prepare_dummy_data(void) {

    for(uint8_t i=0; i<can_data_length; ++i) {
        can_data[i] = i;
    }
}

int main(void) {

    ctlsys::set_clock();
    IntMasterDisable();

    blink = blinker(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_3);

    timer0a = timer(0, TIMER_A, TIMER_CFG_PERIODIC, SysCtlClockGet() / 1000,
                    TIMER_TIMA_TIMEOUT, true);

    ping0 = ping(GPIO_PORTD_BASE, GPIO_PIN_3, 1, TIMER_A);
    ping0_sem = ping0.get_sem();

    adc0 = adc(ADC0_BASE, ADC_TRIGGER_TIMER, 0);
    adc0.configure_sequence(ADC_CTL_CH0); /* PE3 */
    adc0.configure_sequence(ADC_CTL_CH1); /* PE2 */
    adc0.configure_sequence(ADC_CTL_CH2); /* PE1 */
    adc0.configure_sequence(ADC_CTL_CH3 | ADC_CTL_IE | ADC_CTL_END); /* PE0 */

    adc0.configure_timer_interrupt(&timer0a);
    adc0.start();

    ir0 = ir(1, &adc0, 307042, -200, 14); /* PE3 */
    ir1 = ir(0, &adc0, 267697, -276, 10); /* PE2 */
    ir2 = ir(3, &adc0, 249113, -408, 0);  /* PE1 */
    ir3 = ir(2, &adc0, 302703, -198, 11); /* PE0 */

    UART0_RX_BUFFER = buffer<char, UART0_RX_BUFFER_SIZE>(&UART0_RX_SEM);

    uart0 = uart(UART0_BASE, INT_UART0);

    can0 = can(CAN0_BASE, INT_CAN0, can_sender, can_data_length);
    if(can_sender) {
        can_prepare_dummy_data();
    }

    shell0 = shell(&uart0);

    os_threading_init();
    /* schedule(thread_blink_red, 200); */
    /* schedule(thread_blink_blue, 200); */
    /* schedule(thread_blink_green, 200); */
    schedule(ping_handler, 200);
    schedule(shell_handler, 200);
    if(can_sender) {
        schedule(can_transmitter, 200);
    } else {
        schedule(can_handler, 200);
    }
    os_launch();
}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
