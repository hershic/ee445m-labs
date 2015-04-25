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

semaphore motor_start, motor_stop;
motor motor0, motor1;
drive drive0;

#define can_data_length 8
const uint32_t can_msg_id = 1;
const bool can_sender = false;
can can0;
semaphore can_recv_sem;
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

    blink.toggle(PIN_RED);
}

extern "C" void CAN0_Handler(void) {

    uint32_t message_id = can0.ack();

    switch(message_id) {
    case 1:
        can_recv_sem.post();
        break;
    default:
        can0.error_tx();
        break;
    }
}

void can_handler(void) {

    while(1) {
        if(can_recv_sem.guard()) {
            can_recv_sem.take();

            can0.get(can_data);
            uart0.atomic_printf("Received CAN data: %0X %0X %0X %0X %0X %0X %0X %0X ",
                                can_data[0], can_data[1], can_data[3], can_data[4],
                                can_data[4], can_data[5], can_data[6], can_data[7]);
        }
        os_surrender_context();
    }
}

void can_transmitter(void) {

    while(1) {
        can0.transmit(can_data, can_data_length, can_msg_id);
        ++can_data[0];
        os_surrender_context();
    }
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

void motor_control(void) {

    while(1) {
        if(motor_start.guard()) {
            motor_start.take();
            drive0.start();
        }
        if(motor_stop.guard()) {
            motor_stop.take();
            drive0.stop();
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

    SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                   SYSCTL_XTAL_16MHZ);
    IntMasterDisable();

    blink = blinker(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_3);

    timer0a = timer(0, TIMER_A, TIMER_CFG_PERIODIC, SysCtlClockGet() / 2,
                    TIMER_TIMA_TIMEOUT);
    /* timer0a.start(); */

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

    UART0_RX_SEM = semaphore();
    UART0_RX_BUFFER = buffer<char, UART0_RX_BUFFER_SIZE>(&UART0_RX_SEM);

    uart0 = uart(UART_DEFAULT_BAUD_RATE, UART0_BASE, INT_UART0);
    uart0.printf("\n\rWelcome to RRTOS v0\n\r");
    shell0 = shell(&uart0, &motor_start, &motor_stop);

    can_recv_sem = semaphore();
    can0 = can(CAN0_BASE, INT_CAN0, can_sender, can_data_length);
    if(can_sender) { can_prepare_dummy_data(); }

    motor_start = semaphore();
    motor_stop = semaphore();
    motor0 = motor(GPIO_PORTA_BASE, GPIO_PIN_6, PWM0_BASE, PWM_GEN_0, PWM_OUT_0);
    motor1 = motor(GPIO_PORTA_BASE, GPIO_PIN_7, PWM0_BASE, PWM_GEN_0, PWM_OUT_1);
    motor1.set_motor_installed_backwards(true);
    drive0 = drive(&motor0, &motor1);
    /* drive0.forward(20); */

    os_threading_init();
    /* schedule(motor_control, 200); */
    schedule(thread_blink_red, 200);
    schedule(thread_blink_blue, 200);
    schedule(thread_blink_green, 200);
    schedule(shell_handler, 200);
    if(can_sender) {
        schedule(can_transmitter, 200);
    } else {
        schedule(can_handler, 200);
    }
    /* schedule(thread_uart_update, 1000000); */
    os_launch();
}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
