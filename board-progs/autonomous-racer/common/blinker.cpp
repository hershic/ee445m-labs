/* -*- mode: c++; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave on <2015-03-15 Sun> */
/* Revision History: Look in git FGT */

#include "blinker.hpp"
#include "ctlsysctl.hpp"

blinker::blinker() { }

blinker::blinker(port_t port_base) {
    set_base(port_base);
}

blinker::blinker(port_t port_base, port_t output_pins) {

    set_base(port_base);

    ctlsys::enable_periph(port_base);
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
