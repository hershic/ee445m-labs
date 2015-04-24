#include "motorpp.hpp"

#include "inc/tm4c123gh6pm.h"

#include "driverlib/sysctl.h"
#include "driverlib/pin_map.h"

#include "ctlsysctl.hpp"

motor::motor() {

    while(1) {}
    /* this is not a safe constructor to call, severe breakages will
     * shortly follow */
}

motor::motor(memory_address_t ctrl_base, memory_address_t ctrl_pin,
             memory_address_t pwm_base, memory_address_t pwm_pin,
             bool logical_reverse) {

    this->ctrl = blinker(ctrl_base, ctrl_pin);
    this->ctrl_pin = ctrl_pin;
    this->pwm_base = pwm_base;
    this->pwm_pin = pwm_pin;
    this->motor_installed_backwards = logical_reverse;

    ctlsys::enable_periph(ctrl_base);
    ctlsys::enable_periph(pwm_base);

    this->pwm_max_period = DEFAULT_PWM_PERIOD;
    motor_init(DEFAULT_PWM_PERIOD, 0);
}

void motor::stop() {

}

void motor::set(uint32_t pwm_period, uint32_t duty_period) {

    uint16_t adjusted_pwm;
    uint16_t adjusted_duty;
    Direction adjusted_direction = motor_installed_backwards ?
        direction : nav::opposite(direction);

    switch(adjusted_direction) {
    case FORWARD:
        ctrl.turn_off(ctrl_pin);
        adjusted_pwm = pwm_period;
        adjusted_duty = duty_period;
        break;
    case BACKWARD:
        ctrl.turn_on(ctrl_pin);
        adjusted_pwm = pwm_max_period - pwm_period;
        adjusted_duty = pwm_max_period - duty_period;
        break;
    default: break;
    }

    PWM0_0_LOAD_R = pwm_period - 1;
    PWM0_0_CMPA_R = duty_period - 1;
}

void motor::motor_init(uint16_t period, uint16_t duty) {

    GPIO_PORTE_DIR_R |= 0x01;    // make PE3-0 output heartbeats
    GPIO_PORTE_AFSEL_R &= ~0x01;   // disable alt funct on PE3-0
    GPIO_PORTE_DEN_R |= 0x01;     // enable digital I/O on PE3-0
    GPIO_PORTE_PCTL_R = ~0x0000FFFF;
    GPIO_PORTE_AMSEL_R &= ~0x01;      // disable analog functionality on PF

    direction = motor_installed_backwards ? BACKWARD : FORWARD;

    pwm0a_init(10000, 2);
}

void motor::start() {

    PWM0_0_CTL_R |= 1;
}

void motor::pwm0a_init(uint16_t period, uint16_t duty) {

    uint32_t status = StartCritical();
    ctlsys::enable_periph(pwm_base);
    uint32_t pwm_conf;
    switch(pwm_pin) {
    case GPIO_PIN_6: pwm_conf = GPIO_PB6_M0PWM0;
    case GPIO_PIN_7: pwm_conf = GPIO_PB7_M0PWM1;
        /* todo: verify the above GPIO_PBx_MOPWMx's are what we want */
    default: while(1) {}
        /* Don't attempt to initialize something other than these
         * recognized values */
    }
     GPIOPinConfigure(pwm_conf);
    GPIOPinTypePWM(pwm_base, pwm_pin);
    GPIO_PORTB_AMSEL_R &= ~0x40;          // disable analog functionality on PB6
    GPIO_PORTB_DEN_R |= 0x40;             // enable digital I/O on PB6
    SYSCTL_RCC_R = 0x00100000 |           // 3) use PWM divider
        (SYSCTL_RCC_R & (~0x000E0000));   //    configure for /2 divider
    PWM0_0_CTL_R = 0;                     // 4) re-loading down-counting mode
    PWM0_0_GENA_R = 0xC8;                 // low on LOAD, high on CMPA down
    // PB6 goes low on LOAD
    // PB6 goes high on CMPA down
    set(period, duty);
    start();
    PWM0_ENABLE_R |= 0x00000001;          // enable PB6/M0PWM0
    EndCritical(status);
}
