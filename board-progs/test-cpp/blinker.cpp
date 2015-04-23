/* -*- mode: c++; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave on <2015-03-15 Sun> */
/* Revision History: Look in git FGT */

#include "blinker.hpp"

#include "inc/hw_memmap.h"

#include "driverlib/sysctl.h"

blinker::blinker() { }

blinker::blinker(port_t port_base) {
    set_base(port_base);
}

blinker::blinker(port_t port_base, port_t output_pins) {
    set_base(port_base);

    port_t periph_base = 0xDEADBEEF;
    switch(port_base) {
    case GPIO_PORTA_BASE: periph_base = SYSCTL_PERIPH_GPIOA; break;
    case GPIO_PORTB_BASE: periph_base = SYSCTL_PERIPH_GPIOB; break;
    case GPIO_PORTC_BASE: periph_base = SYSCTL_PERIPH_GPIOC; break;
    case GPIO_PORTD_BASE: periph_base = SYSCTL_PERIPH_GPIOD; break;
    case GPIO_PORTE_BASE: periph_base = SYSCTL_PERIPH_GPIOE; break;
    case GPIO_PORTF_BASE: periph_base = SYSCTL_PERIPH_GPIOF; break;
    case GPIO_PORTG_BASE: periph_base = SYSCTL_PERIPH_GPIOG; break;
    case GPIO_PORTH_BASE: periph_base = SYSCTL_PERIPH_GPIOH; break;
    case GPIO_PORTJ_BASE: periph_base = SYSCTL_PERIPH_GPIOJ; break;
    }
    SysCtlPeripheralEnable(periph_base);

    GPIOPinTypeGPIOOutput(port_base, output_pins);
}

void blinker::set_base(port_t port_base) {
    this->port_base = port_base;
}

void blinker::turn_on(pin_t pin) {
    GPIOPinWrite(port_base, pin, pin);
}

void blinker::turn_off(pin_t pin) {
    GPIOPinWrite(port_base, pin, 0);
}

void blinker::toggle(pin_t pin) {
    GPIOPinWrite(port_base, pin, pin ^ GPIOPinRead(port_base, pin));
}

void blinker::blink(pin_t pin) {
    GPIOPinWrite(port_base, pin, pin ^ GPIOPinRead(port_base, pin));
    GPIOPinWrite(port_base, pin, pin ^ GPIOPinRead(port_base, pin));
}
