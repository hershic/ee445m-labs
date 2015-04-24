#include "motorpp.hpp"

#include "inc/tm4c123gh6pm.h"

#include "driverlib/sysctl.h"

motor::motor() {

    motor_init();
}

motor::motor(uint32_t pwm_period, uint32_t duty_period,
             Direction direction) {

    this->pwm_period = pwm_period;
    this->duty_period = duty_period;
    this->direction = direction;

    motor_init();
}

void motor::stop() {

    /* todo: keeping direction unchanged might result in better
     * performance */
    set(10000, 0, FORWARD);
}

void motor::set(uint32_t pwm_period, uint32_t duty_period,
                Direction direction) {

    PWM0_0_LOAD_R = pwm_period - 1;
    PWM0_0_CMPA_R = duty_period - 1;
    /* TODO: implement direction */
}

void motor::motor_init() {

    SysCtlPeripheralEnable(SYSCTL_PERIPH_GPIOE);
    GPIO_PORTE_DIR_R |= 0x01;    // make PE3-0 output heartbeats
    GPIO_PORTE_AFSEL_R &= ~0x01;   // disable alt funct on PE3-0
    GPIO_PORTE_DEN_R |= 0x01;     // enable digital I/O on PE3-0
    GPIO_PORTE_PCTL_R = ~0x0000FFFF;
    GPIO_PORTE_AMSEL_R &= ~0x01;      // disable analog functionality on PF

    pwm0a_init(pwm_period, duty_period);
}

void motor::pwm0a_init(uint16_t period, uint16_t duty) {

    uint32_t status = StartCritical();
    SYSCTL_RCGCPWM_R |= 0x01;             // 1) activate PWM0
    SYSCTL_RCGCGPIO_R |= 0x02;            // 2) activate port B
    while((SYSCTL_PRGPIO_R&0x02) == 0){};
    GPIO_PORTB_AFSEL_R |= 0x40;           // enable alt funct on PB6
    GPIO_PORTB_PCTL_R &= ~0x0F000000;     // configure PB6 as PWM0
    GPIO_PORTB_PCTL_R |= 0x04000000;
    GPIO_PORTB_AMSEL_R &= ~0x40;          // disable analog functionality on PB6
    GPIO_PORTB_DEN_R |= 0x40;             // enable digital I/O on PB6
    SYSCTL_RCC_R = 0x00100000 |           // 3) use PWM divider
        (SYSCTL_RCC_R & (~0x000E0000));   //    configure for /2 divider
    PWM0_0_CTL_R = 0;                     // 4) re-loading down-counting mode
    PWM0_0_GENA_R = 0xC8;                 // low on LOAD, high on CMPA down
    // PB6 goes low on LOAD
    // PB6 goes high on CMPA down
    PWM0_0_LOAD_R = period - 1;           // 5) cycles needed to count down to 0
    PWM0_0_CMPA_R = duty - 1;             // 6) count value when output rises
    PWM0_0_CTL_R |= 0x00000001;           // 7) start PWM0
    PWM0_ENABLE_R |= 0x00000001;          // enable PB6/M0PWM0
    EndCritical(status);
}
