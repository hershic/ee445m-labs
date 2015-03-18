/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave on <2015-03-15 Sun> */
/* Revision History: Look in git FGT */

#include "blinker.hpp"

#include "driverlib/gpio.h"

blinker::blinker() { }

uint32_t blinker::get_led_pin_from_color(color_t color) {
    switch(color) {
    case RED:   return GPIO_PIN_1;
    case GREEN: return GPIO_PIN_3;
    case BLUE:  return GPIO_PIN_2;
    default:    return 0;
    }
}

void blinker::set_base(port_t port_base) {
    this->port_base = port_base;
}

void blinker::turn_on(color_t color) {
    uint32_t pin = get_led_pin_from_color(color);
    GPIOPinWrite(port_base, pin, pin);
}

void blinker::turn_off(color_t color) {
    uint32_t pin = get_led_pin_from_color(color);
    GPIOPinWrite(port_base, pin, 0);
}

void blinker::toggle(color_t color) {
    uint32_t pin = get_led_pin_from_color(color);
    GPIOPinWrite(port_base, pin, pin ^ GPIOPinRead(port_base, pin));
}
