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
bool ping_sent = false;

uint32_t sens_ir_left;
uint32_t sens_ir_left_front;
uint32_t sens_ir_right;
uint32_t sens_ir_right_front;
uint32_t sens_ping_back;

uint8_t* ir_left_ptr;
uint8_t* ir_left_front_ptr;
uint8_t* ir_right_ptr;
uint8_t* ir_right_front_ptr;
uint8_t* ping_back_ptr;

semaphore motor_start, motor_stop;
motor motor0, motor1;
drive drive0;

#define can_data_length 5*2
const uint32_t can_msg_id = 1;
const bool can_sender = true;
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

/* Record how long the Ping))) took to respond */
extern "C"
int GPIOPortB_Handler() {

    GPIOIntClear(ping0.base, ping0.pin);

    switch(ping_sent) {
    case false:
        ping0.start();
        break;
    case true:
        ping0.stop();
        break;
    }
    ping_sent = !ping_sent;
}

void can_handler(void) {

    while(1) {
        if(can_recv_sem.guard()) {

            can0.get(can_data);
            uart0.atomic_printf("Received CAN data: %0X %0X %0X %0X %0X %0X %0X %0X ",
                                can_data[0], can_data[1], can_data[3], can_data[4],
                                can_data[4], can_data[5], can_data[6], can_data[7]);
        }
        os_surrender_context();
    }
}

void can_transmitter(void) {

    ir_left_ptr = (uint8_t*)(&sens_ir_left);
    ir_left_front_ptr = (uint8_t*)(&sens_ir_left_front);
    ir_right_ptr = (uint8_t*)(&sens_ir_right);
    ir_right_front_ptr = (uint8_t*)(&sens_ir_right_front);

    while(1) {

        sens_ir_left = ir0.buf.peek();
        sens_ir_left_front = ir1.buf.peek();
        sens_ir_right = ir2.buf.peek();
        sens_ir_right_front = ir3.buf.peek();

        can_data[0] = ir_left_ptr[0];
        can_data[1] = ir_left_ptr[1];

        can_data[2] = ir_left_front_ptr[0];
        can_data[3] = ir_left_front_ptr[1];

        can_data[4] = ir_right_ptr[0];
        can_data[5] = ir_right_ptr[1];

        can_data[6] = ir_right_front_ptr[0];
        can_data[7] = ir_right_front_ptr[1];

        /* TODO: Populate */
        can_data[8] = 0;              /* sens_ping_back  */
        can_data[9] = 0;              /* sens_ping_back  */

        can0.transmit(can_data, can_data_length);
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

void motor_control(void) {

    while(1) {
        if(motor_start.guard()) {
            drive0.start();
        }
        if(motor_stop.guard()) {
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

void driver(void) {

    while(1) {
        /* todo: populate these uint32_t's with sensor data */

        drive0.steer(sens_ir_left, sens_ir_left_front,
                     sens_ir_right, sens_ir_right_front, sens_ping_back);
        os_surrender_context();
    }
}

int main(void) {

    ctlsys::set_clock();
    IntMasterDisable();

    blink = blinker(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_3);

    timer0a = timer(0, TIMER_A, TIMER_CFG_PERIODIC, SysCtlClockGet() / 10,
                    TIMER_TIMA_TIMEOUT, true);

    /* resume: create timer for ping sensor */
    /* timer1a = timer(1, TIMER_A, TIMER_CFG_ONE_SHOT, S); */

    adc0 = adc(ADC0_BASE, ADC_TRIGGER_TIMER, 0);
    adc0.configure_sequence(ADC_CTL_CH0); /* PE3 */
    adc0.configure_sequence(ADC_CTL_CH1); /* PE2 */
    adc0.configure_sequence(ADC_CTL_CH2); /* PE1 */
    adc0.configure_sequence(ADC_CTL_CH3 | ADC_CTL_IE | ADC_CTL_END); /* PE0 */

    adc0.configure_timer_interrupt(&timer0a);
    adc0.start();
    ir0 = ir(0, &adc0);
    ir1 = ir(1, &adc0);
    ir2 = ir(2, &adc0);
    ir3 = ir(3, &adc0);

    UART0_RX_BUFFER = buffer<char, UART0_RX_BUFFER_SIZE>(&UART0_RX_SEM);

    uart0 = uart(UART0_BASE, INT_UART0);

    can0 = can(CAN0_BASE, INT_CAN0, can_sender, can_data_length);
    if(can_sender) {
        can_prepare_dummy_data();
    } else {
        can_recv_sem = semaphore();
    }

    motor_start = semaphore();
    motor_stop = semaphore();
    shell0 = shell(&uart0, &motor_start, &motor_stop);
    motor0 = motor(GPIO_PORTA_BASE, GPIO_PIN_6, PWM0_BASE, PWM_GEN_0, PWM_OUT_0);
    motor1 = motor(GPIO_PORTA_BASE, GPIO_PIN_7, PWM0_BASE, PWM_GEN_0, PWM_OUT_1, true);
    drive0 = drive(&motor0, &motor1, 50);

    os_threading_init();
    schedule(motor_control, 200);
    /* schedule(thread_blink_red, 200); */
    schedule(thread_blink_blue, 200);
    schedule(thread_blink_green, 200);
    schedule(shell_handler, 200);
    /* schedule(driver, 200); */
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
