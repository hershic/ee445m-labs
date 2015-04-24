/* -*- mode: c++; c-basic-offset: 4; */
/* Created by Hershal Bhave on <2015-03-15 Sun> */
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

/* I avoid using a buffer object in the UART*_Handler so that the data
 * structures are instantiated correctly without client code. */
#include "pseudo-buffer.h"

blinker blink;
timer timer0a;
uart uart0;
shell shell0;
adc adc0;
motor motor0;
motor motor1;
drive drive0;

can can0;
const bool can_sender = true;
semaphore can_recv_sem;
#define can_data_length 8
#define can_msg_id 1
uint8_t can_data[can_data_length];

semaphore motor_start;
semaphore motor_stop;

ir ir0;
ir ir1;
ir ir2;
ir ir3;

#define thread(x)                               \
    while(1) {                                  \
        x                                       \
            os_surrender_context();             \
    }

static semaphore UART0_RX_SEM;

#define UART0_RX_BUFFER_SIZE 8

static buffer<char, UART0_RX_BUFFER_SIZE> UART0_RX_BUFFER;

uint32_t blink_count_green = 0;
uint32_t blink_count_blue = 0;

void thread_0() {

    thread (
            blink.toggle(PIN_BLUE);
            ++blink_count_blue;
            )
        }

void thread_1() {

    thread (
            blink.toggle(PIN_GREEN);
            ++blink_count_green;
            )
        }

void thread_uart_update() {

    thread (
            uart0.atomic_printf("%d\n\r", blink_count_blue);
            )
        }

void shell_handler() {

    while(1) {
        if(UART0_RX_SEM.guard()) {
            UART0_RX_SEM.take();

            bool ok;
            char recv = UART0_RX_BUFFER.get(ok);

            if(ok) { shell0.accept(recv); }
        }
        os_surrender_context();
    }
}

extern "C" void Timer0A_Handler() {
    timer0a.ack();
}

extern "C" void UART0_Handler(void) {

    char recv;
    uint32_t interrupts = uart0.ack();

    if(interrupts & (UART_INT_RX | UART_INT_RT)) {
        /* Get all available chars from the UART */
        while(UARTCharsAvail(UART0_BASE)) {
            recv = uart0.get_char();

            /* Regardless of newline received, our convention is to
             * mark end-of-lines in a buffer with the CR character. */
            switch(recv) {
            case '\n':
                if (UART_LAST_WAS_CR) {
                    UART_LAST_WAS_CR = false;
                    continue;
                }
                break;
            case '\r':
                UART_LAST_WAS_CR = true;
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
}

extern "C" void ADC0Seq0_Handler(void) {

    adc0.ack();
    adc0.sample();

    ir0.sample();
    ir1.sample();
    ir2.sample();
    ir3.sample();

    blink.toggle(PIN_RED);
}

extern "C" void CAN0_Handler(void) {

    // Read the CAN interrupt status to find the cause of the interrupt
    uint32_t message_id = can0.ack();

    switch(message_id) {
    case 1:
        // Since the message was sent, clear any error flags.
        can_recv_sem.post();
        break;
    default:
        /* will this ever hang? put your money in now */
        while(1) {}
    }
}

void can_handler(void) {

    while(1) {
        if(can_recv_sem.guard()) {
            can_recv_sem.take();

            can0.mailbox(can_data);
            uart0.atomic_printf("Received CAN data\n\r");
        }
        os_surrender_context();
    }
}

void can_transmitter(void) {

    while(1) {
        can0.transmit(can_data, can_data_length, can_msg_id);
        os_surrender_context();
    }
}

extern "C" void __cxa_pure_virtual() { while (1) {} }

void motor_control(void) {
    while(1) {

        if(motor_start.guard()) {
            motor_start.take();
            /* todo: fill me in ^^ */
        }
        if(motor_stop.guard()) {
            motor_stop.take();
            /* todo: fill me in ^^ */
        }
        os_surrender_context();
    }
}

int main(void) {

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);
    IntMasterDisable();

    blink = blinker(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_3);

    timer0a = timer(0, TIMER_A, TIMER_CFG_PERIODIC, SysCtlClockGet() / 2,
                    TIMER_TIMA_TIMEOUT);
    /* timer0a.start(); */

    motor0 = motor();
    motor1 = motor();
    /*******************************************************/
    /* !!!! NOTICE !!!!                                    */
    /* Right now, the motor uses the same pins as the ADC. */
    /* TODO: This needs to be fixed                        */
    /*******************************************************/
    /* motor0 = motor(10000, 9999, FORWARD); */
    drive0 = drive(&motor0, &motor1);

    UART0_RX_SEM = semaphore();
    UART0_RX_BUFFER = buffer<char, UART0_RX_BUFFER_SIZE>(&UART0_RX_SEM);

    motor_start = semaphore();
    motor_stop = semaphore();

    uart0 = uart(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
    uart0.printf("\n\rWelcome to RRTOS v0\n\r");
    shell0 = shell(&uart0, &motor_start, &motor_stop);

    adc0 = adc(ADC0_BASE, ADC_TRIGGER_TIMER, 0);
    adc0.configure_sequence(ADC_CTL_CH0); /* PE3 */
    adc0.configure_sequence(ADC_CTL_CH1); /* PE2 */
    adc0.configure_sequence(ADC_CTL_CH2); /* PE1 */
    adc0.configure_sequence(ADC_CTL_CH3 | ADC_CTL_IE | ADC_CTL_END); /* PE0 */

    adc0.configure_timer_interrupt(timer0a.base, timer0a.subtimer);
    adc0.start();

    ir0 = ir(0, &adc0);
    ir1 = ir(1, &adc0);
    ir2 = ir(2, &adc0);
    ir3 = ir(3, &adc0);

    can_recv_sem = semaphore();
    can0 = can(CAN0_BASE, INT_CAN0, can_sender);
    if(can_sender) {
        uint8_t i;
        for(i=0; i<can_data_length; ++i) {
            can_data[i] = i;
        }
    }

    os_threading_init();
    /* schedule(motor_control, 200); */
    schedule(thread_1, 200);
    schedule(thread_0, 200);
    schedule(shell_handler, 200);
    if(can_sender) {
        schedule(can_transmitter, 200);
    } else {
        schedule(can_handler, 200);
    }
    /* schedule(thread_uart_update, 1000000); */
    os_launch();
}
