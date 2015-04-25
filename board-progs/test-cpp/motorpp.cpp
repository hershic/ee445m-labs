#include "motorpp.hpp"

#include "inc/tm4c123gh6pm.h"
#include "inc/hw_pwm.h"

#include "driverlib/sysctl.h"
#include "driverlib/pin_map.h"
#include "driverlib/pwm.h"

#include "ctlsysctl.hpp"

motor::motor() {

    while(1) {}
    /* this is not a safe constructor to call, severe breakages will
     * shortly follow */
}

motor::motor(memory_address_t ctrl_base, memory_address_t ctrl_pin,
             memory_address_t pwm_base, memory_address_t pwm_gen,
             memory_address_t pwm_out, bool logical_reverse) {

    this->ctrl = blinker(ctrl_base, ctrl_pin);
    this->ctrl_pin = ctrl_pin;
    this->pwm_base = pwm_base;
    this->pwm_gen = pwm_gen;
    this->pwm_out = pwm_out;
    this->motor_installed_backwards = logical_reverse;

    pwm_hw = (pwm_pin = GPIO_PIN_6) ? PWM0_BASE : PWM1_BASE;

    ctlsys::enable_periph(ctrl_base);
    ctlsys::enable_periph(pwm_base);

    this->pwm_max_period = DEFAULT_PWM_PERIOD;
    motor_init();
}

void motor::stop() {

    PWMGenDisable(pwm_hw, PWM_GEN_0);
}

void motor::set(percent_t percent_full_speed) {

    uint16_t adjusted_duty;
    Direction adjusted_direction = motor_installed_backwards ?
        direction : nav::opposite(direction);

    switch(adjusted_direction) {
    case FORWARD:
        ctrl.turn_off(ctrl_pin);
        adjusted_duty = pwm_max_period/(100*percent_full_speed);
        break;
    case BACKWARD:
         ctrl.turn_on(ctrl_pin);
         adjusted_duty = pwm_max_period - pwm_max_period/(100*percent_full_speed);
         break;
    default: break;
    }

    switch(ctrl_pin) {
    case GPIO_PIN_6:
        PWM0_0_LOAD_R = pwm_max_period - 1;
        PWM0_0_CMPA_R = adjusted_duty - 1;
        break;
    case GPIO_PIN_7:
        PWM1_0_LOAD_R = pwm_max_period - 1;
        PWM1_0_CMPA_R = adjusted_duty - 1;
        break;
    default: while (1) {}
    }

}

void motor::motor_init() {

    direction = motor_installed_backwards ? BACKWARD : FORWARD;

    pwm_init();
}

void motor::start() {

    PWMGenEnable(pwm_hw, PWM_GEN_0);
}

void motor::pwm_init() {

    uint32_t status = StartCritical();
    ctlsys::enable_periph(pwm_base);
    ctlsys::enable_periph(GPIO_PORTB_BASE);

    SysCtlPWMClockSet(SYSCTL_PWMDIV_64);
    PWMGenConfigure(pwm_base, pwm_gen, PWM_GEN_MODE_DOWN | PWM_GEN_MODE_NO_SYNC);
    PWMGenPeriodSet(pwm_base, pwm_gen, DEFAULT_PWM_PERIOD);
    PWMPulseWidthSet(pwm_base, pwm_out, 75*DEFAULT_PWM_PERIOD/100);
    PWMGenEnable(pwm_base, pwm_gen);

    switch(pwm_out) {
    case PWM_OUT_0:
        GPIOPinConfigure(GPIO_PB6_M0PWM0);
        GPIOPinTypePWM(GPIO_PORTB_BASE, GPIO_PIN_6);
        break;
    case PWM_OUT_1:
        GPIOPinConfigure(GPIO_PB7_M0PWM1);
        GPIOPinTypePWM(GPIO_PORTB_BASE, GPIO_PIN_7);
        break;
    default:
        while(1) {}
    }

    /* Enable the outputs. */
    PWMOutputState(pwm_base, (PWM_OUT_0_BIT | PWM_OUT_1_BIT), true);

    /* set(0); */
    /* start(); */
    /* PWM0_ENABLE_R |= 0x00000001;          // enable PB6/M0PWM0 */
    EndCritical(status);
}

void motor::reverse() {

    direction = nav::opposite(direction);
}
