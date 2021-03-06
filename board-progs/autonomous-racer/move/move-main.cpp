/* -*- mode: c++; c-basic-offset: 4; */
/* Created by Hershal Bhave and Eric Crosson on <2015-03-15 Sun> */
/* Revision History: Look in Git FGT */

#include "adcpp.hpp"
#include "blinker.hpp"
#include "math.hpp"
#include "uartpp.hpp"
#include "shellpp.hpp"
#include "semaphorepp.hpp"
#include "motorpp.hpp"
#include "drivepp.hpp"
#include "canpp.hpp"
#include "ctlsysctl.hpp"
#include "switchpp.hpp"

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

#define BUTTON_LEFT          GPIO_PIN_4
#define BUTTON_RIGHT         GPIO_PIN_0
#define BUTTONS_BOTH         (BUTTON_LEFT | BUTTON_RIGHT)

#define thread(x)                   \
    do {                            \
        x;                          \
        os_surrender_context();     \
    } while(true)

blinker blink;
uart uart0;
shell shell0;

uint16_t sens_ir_left;
uint16_t sens_ir_left_front;
uint16_t sens_ir_right;
uint16_t sens_ir_right_front;
uint16_t sens_ping_front;

uint16_t duty_cycle;

lswitch switch0;
semaphore sem_switch;
timer countdown_timer;

semaphore motor_start, motor_stop;
motor motor0, motor1;
drive drive0;

#define can_data_length 10
const uint32_t can_msg_id = 1;
const bool can_sender = false;
can can0;
uint8_t can_data[can_data_length];

#define UART0_RX_BUFFER_SIZE 8
static semaphore UART0_RX_SEM;
static buffer<char, UART0_RX_BUFFER_SIZE> UART0_RX_BUFFER;

uint32_t blink_count_red = 0;
uint32_t blink_count_green = 0;
uint32_t blink_count_blue = 0;

semaphore sem_blink_red;
semaphore sem_blink_blue;
semaphore sem_blink_green;

#define BLINK_RED_WAIT_FOR_SEM 1
#define BLINK_BLUE_WAIT_FOR_SEM 1
#define BLINK_GREEN_WAIT_FOR_SEM 1

void thread_blink_red() {

    while(1) {
#if BLINK_RED_WAIT_FOR_SEM == 1
        if (sem_blink_red.guard()) {
#endif
            blink.toggle(PIN_RED);
            ++blink_count_red;
#if BLINK_RED_WAIT_FOR_SEM == 1
        }
#endif
        os_surrender_context();
    }
}

void thread_blink_blue() {

    while(1) {
#if BLINK_BLUE_WAIT_FOR_SEM == 1
        if (sem_blink_blue.guard()) {
#endif
            blink.toggle(PIN_BLUE);
            ++blink_count_blue;
#if BLINK_BLUE_WAIT_FOR_SEM == 1
        }
#endif
        os_surrender_context();
    }
}

void thread_blink_green() {

    while(1) {
#if BLINK_GREEN_WAIT_FOR_SEM == 1
        if (sem_blink_green.guard()) {
#endif
            blink.toggle(PIN_GREEN);
            ++blink_count_green;
#if BLINK_GREEN_WAIT_FOR_SEM == 1
        }
#endif
        os_surrender_context();
    }
}

void thread_uart_update() {

    thread (
            uart0.atomic_printf("%x %x %x\n\r", blink_count_red,
                                blink_count_blue, blink_count_green);
        );
}

extern "C" void __cxa_pure_virtual() { while (1) {} }

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

extern "C" void GPIOPortF_Handler() {

    switch0.ack();
    switch0.debounce();
}

void switch_responder() {

    const uint32_t counter_max = SysCtlClockGet()/100;
    uint32_t pins, counter;
    percent_t left_speed, right_speed;

    while(1) {
        if(sem_switch.guard() && switch0.debounced_data) {
            counter = 0;

            if(switch0.debounced_data == BUTTON_LEFT) {
                blink.toggle(PIN_RED);
                duty_cycle = clamp(duty_cycle-1, 0, 200);
                motor0.set(duty_cycle*motor::pwm_max_period/200);
            } else if(switch0.debounced_data == BUTTON_RIGHT) {
                blink.toggle(PIN_BLUE);
                duty_cycle = clamp(duty_cycle+1, 0, 200);
                motor0.set(duty_cycle*motor::pwm_max_period/200);
            }
            uart0.atomic_printf("%i\n", duty_cycle/2);

            /* while (++counter < counter_max) { } */
        }
        os_surrender_context();
    }
}

void can_handler(void) {

    uint16_t can_msg_id;

    while(1) {
        if(can0.recv_sem.guard()) {

            can0.get(can_data);
            blink.toggle(PIN_BLUE);

            can_msg_id = (can_data[1] << 8) | (can_data[0]);

            if (can_msg_id == 0) {
                sens_ir_left = (can_data[3] << 8) | (can_data[2]);
                sens_ir_left_front = (can_data[5] << 8) | (can_data[4]);
                sens_ir_right = (can_data[7] << 8) | (can_data[4]);
            } else {
                sens_ir_right_front = (can_data[3] << 8) | (can_data[2]);
                sens_ping_front = (can_data[5] << 8) | (can_data[4]);
            }

            /* uart0.atomic_printf("                                                \r"); */
            /* uart0.atomic_printf("l: %u lf: %u r: %u rf: %u pf: %u\r", */
            /*                     sens_ir_left, sens_ir_left_front, */
            /*                     sens_ir_right, sens_ir_right_front, */
            /*                     sens_ping_front); */

            /* uart0.atomic_printf("Received CAN data: %0X %0X %0X %0X %0X %0X %0X %0X \n", */
            /*                     can_data[0], can_data[1], can_data[2], can_data[3], */
            /*                     can_data[4], can_data[5], can_data[6], can_data[7]); */
        }
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

/*! Allows motor control from shell */
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

extern "C" void Timer1A_Handler() {

    switch0.end_debounce();
}

extern "C" void Timer0A_Handler() {

    countdown_timer.ack();
    drive0.stop();
}

void driver(void) {

    while(1) {
        drive0.steer(sens_ir_left, sens_ir_left_front,
                     sens_ir_right, sens_ir_right_front, sens_ping_front);
        os_surrender_context();
    }
}

int main(void) {

    ctlsys::set_clock();
    IntMasterDisable();

    blink = blinker(GPIO_PORTF_BASE, GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_3);

    sem_blink_red = semaphore();
    sem_blink_green = semaphore();
    sem_blink_blue = semaphore();

    UART0_RX_BUFFER = buffer<char, UART0_RX_BUFFER_SIZE>(&UART0_RX_SEM);
    uart0 = uart(UART0_BASE, INT_UART0);

    can0 = can(CAN0_BASE, INT_CAN0, can_sender, can_data_length);

    duty_cycle = 15;

    motor_start = semaphore();
    motor_stop = semaphore();
    shell0 = shell(&uart0, &motor_start, &motor_stop);
    motor0 = motor(GPIO_PORTA_BASE, GPIO_PIN_6, PWM0_BASE, PWM_GEN_0, PWM_OUT_0);
    motor1 = motor(GPIO_PORTA_BASE, GPIO_PIN_7, PWM0_BASE, PWM_GEN_0, PWM_OUT_1);
    /* drive0 = drive(&motor0, &motor1, 20); */

    motor0.set(duty_cycle*motor::pwm_max_period/200);
    motor0.start();

    countdown_timer = timer(0, TIMER_BOTH, TIMER_CFG_ONE_SHOT, SysCtlClockGet()*180,
                            TIMER_TIMA_TIMEOUT, true);
    switch0 = lswitch(GPIO_PORTF_BASE, BUTTONS_BOTH,
                      &sem_switch, 1, TIMER_A, GPIO_BOTH_EDGES,
                      INT_GPIOF_TM4C123, true);

    os_threading_init();
    schedule(motor_control, 200);
    schedule(thread_blink_red, 200);
    schedule(thread_blink_blue, 200);
    schedule(thread_blink_green, 200);
    schedule(shell_handler, 200);
    /* schedule(thread_uart_update, 200); */
    schedule(switch_responder, 200);
    schedule(driver, 200);
    schedule(can_handler, 200);
    os_launch();
}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
