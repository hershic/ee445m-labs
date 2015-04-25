/* -*- mode: c++; c-basic-offset: 4; -*- */
#include "motorpp.hpp"

#include "inc/tm4c123gh6pm.h"
#include "inc/hw_pwm.h"

#include "driverlib/sysctl.h"
#include "driverlib/pin_map.h"
#include "driverlib/pwm.h"

#include "ctlsysctl.hpp"

motor::motor() {}

motor::motor(memory_address_t ctrl_base, memory_address_t ctrl_pin,
             memory_address_t pwm_base, memory_address_t pwm_gen,
             memory_address_t pwm_out, bool logical_reverse) {

    this->ctrl = blinker(ctrl_base, ctrl_pin);
    this->ctrl_base = ctrl_base;
    this->ctrl_pin = ctrl_pin;
    this->pwm_base = pwm_base;
    this->pwm_gen = pwm_gen;
    this->pwm_out = pwm_out;
    this->motor_installed_backwards = logical_reverse;

    ctlsys::enable_periph(ctrl_base);
    ctlsys::enable_periph(pwm_base);

    this->pwm_max_period = DEFAULT_PWM_PERIOD;
    motor_init();
}

Direction motor::adjusted_direction() {

    return motor_installed_backwards ? direction : nav::opposite(direction);
}

void motor::stop() {

    PWMGenDisable(pwm_base, pwm_gen);
}

void motor::set(percent_t speed, Direction dir) {

    this->direction = dir;
    switch(adjusted_direction()) {
    case FORWARD: ctrl.turn_off(ctrl_pin); break;
    case BACKWARD: ctrl.turn_on(ctrl_pin); break;
    default: while(1) {}
    }
    set(speed);
}

void motor::set(percent_t speed) {

    if(speed > 100) { speed = 100; }

    current_speed = speed;

    uint16_t adjusted_duty;
    switch(adjusted_direction()) {
    case FORWARD:
        adjusted_duty = speed * pwm_max_period / 100;
        break;
    case BACKWARD:
         adjusted_duty = pwm_max_period - speed * pwm_max_period / 100;
         break;
    default: while(1) {}
    }

    PWMPulseWidthSet(pwm_base, pwm_out, speed*pwm_max_period/100);
}

void motor::motor_init() {

    direction = motor_installed_backwards ? BACKWARD : FORWARD;

    pwm_init();
}

void motor::start() {

    PWMGenEnable(pwm_base, pwm_gen);
}

void motor::pwm_init() {

    uint32_t status = StartCritical();
    /* nucleo: abstract */
    ctlsys::enable_periph(SYSCTL_PERIPH_GPIOB);
    ctlsys::enable_periph(ctrl_base);

    SysCtlPWMClockSet(SYSCTL_PWMDIV_64);
    PWMGenConfigure(pwm_base, pwm_gen, PWM_GEN_MODE_DOWN | PWM_GEN_MODE_NO_SYNC);
    PWMGenPeriodSet(pwm_base, pwm_gen, DEFAULT_PWM_PERIOD);
    PWMGenEnable(pwm_base, pwm_gen);

    /* this isn't used, is it? */
    /* switch(pwm_out) { */
    /* case PWM_OUT_0: */
    /*     GPIOPinConfigure(GPIO_PB6_M0PWM0); */
    /*     GPIOPinTypePWM(GPIO_PORTB_BASE, GPIO_PIN_6); */
    /*     break; */
    /* case PWM_OUT_1: */
    /*     GPIOPinConfigure(GPIO_PB7_M0PWM1); */
    /*     GPIOPinTypePWM(GPIO_PORTB_BASE, GPIO_PIN_7); */
    /*     break; */
    /* default: */
    /*     while(1) {} */
    /* } */

    /* Enable the outputs. */
    PWMOutputState(pwm_base, (PWM_OUT_0_BIT | PWM_OUT_1_BIT), true);

    set(50, FORWARD);
    start();
    EndCritical(status);
}

/*! note maintains current speed */
void motor::reverse() {

    set(current_speed, nav::opposite(direction));
}

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
