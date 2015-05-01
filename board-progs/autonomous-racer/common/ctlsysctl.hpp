/* -*- mode: c++; c-basic-offset: 4; */
/* Created by Hershal Bhave and Eric Crosson on <2015-03-15 Sun> */
/* Revision History: Look in Git FGT */
#ifndef __ctlsysctl__
#define __ctlsysctl__

/*! \addtogroup CppTest Test of cpp functionality
 * @{
 */

#include "inc/hw_memmap.h"

#include "driverlib/sysctl.h"
#include "driverlib/gpio.h"

#include "driverlib/timer.h"

#include "criticalpp.hpp"

class ctlsys : public critical {
public:
    /*! Enable peripherals based on a GPIO_PORTx_BASE. */
    static void enable_periph(uint32_t sys_periph) {
        uint32_t periph_base = 0xDEADBEEF;
        switch(sys_periph) {
        case GPIO_PORTA_BASE: periph_base = SYSCTL_PERIPH_GPIOA; break;
        case GPIO_PORTB_BASE: periph_base = SYSCTL_PERIPH_GPIOB; break;
        case GPIO_PORTC_BASE: periph_base = SYSCTL_PERIPH_GPIOC; break;
        case GPIO_PORTD_BASE: periph_base = SYSCTL_PERIPH_GPIOD; break;
        case GPIO_PORTE_BASE: periph_base = SYSCTL_PERIPH_GPIOE; break;
        case GPIO_PORTF_BASE: periph_base = SYSCTL_PERIPH_GPIOF; break;
        case GPIO_PORTG_BASE: periph_base = SYSCTL_PERIPH_GPIOG; break;
        case GPIO_PORTH_BASE: periph_base = SYSCTL_PERIPH_GPIOH; break;
        case GPIO_PORTJ_BASE: periph_base = SYSCTL_PERIPH_GPIOJ; break;
        case PWM0_BASE:       periph_base = SYSCTL_PERIPH_PWM0; break;
        case PWM1_BASE:       periph_base = SYSCTL_PERIPH_PWM1; break;
        case CAN0_BASE:       periph_base = SYSCTL_PERIPH_CAN0; break;
        case CAN1_BASE:       periph_base = SYSCTL_PERIPH_CAN1; break;
        default: while(1) {}
        }
        uint32_t ui32Status = static_StartCritical();
        SysCtlPeripheralEnable(periph_base);
        static_EndCritical(ui32Status);
    }

    static uint32_t timer_timeout_from_subtimer(uint32_t subtimer) {

        uint32_t timeout =  0x0;
        switch(subtimer) {
        case TIMER_BOTH:
        case TIMER_A: timeout = TIMER_TIMA_TIMEOUT; break;
        case TIMER_B: timeout = TIMER_TIMB_TIMEOUT; break;
        default: while(1) {}
        }
        return timeout;
    }

    static uint32_t gpio_pin_to_int(uint32_t pin) {
        switch(pin) {
        case GPIO_PIN_0: return GPIO_INT_PIN_0;
        case GPIO_PIN_1: return GPIO_INT_PIN_1;
        case GPIO_PIN_2: return GPIO_INT_PIN_2;
        case GPIO_PIN_3: return GPIO_INT_PIN_3;
        case GPIO_PIN_4: return GPIO_INT_PIN_4;
        case GPIO_PIN_5: return GPIO_INT_PIN_5;
        case GPIO_PIN_6: return GPIO_INT_PIN_6;
        case GPIO_PIN_7: return GPIO_INT_PIN_7;
        default: while(1) {}
        }
    }

    static void gpio_int_clear(uint32_t base, uint32_t pin) {
        uint32_t interrupt = gpio_pin_to_int(pin);
        GPIOIntClear(base, interrupt);
    }

    static void gpio_int_disable(uint32_t base, uint32_t pin) {

        uint32_t interrupt = gpio_pin_to_int(pin);
        GPIOIntDisable(base, interrupt);
    }

    static void gpio_int_enable(uint32_t base, uint32_t pin, bool clear_int = false) {

        uint32_t interrupt = gpio_pin_to_int(pin);
        if (clear_int) {
            gpio_int_clear(base, interrupt);
        }
        GPIOIntEnable(base, interrupt);
    }

    /*! Set the clock as used in our labs. */
    static void set_clock(void) {
        SysCtlClockSet(SYSCTL_SYSDIV_1 | SYSCTL_USE_OSC | SYSCTL_OSC_MAIN |
                       SYSCTL_XTAL_16MHZ);
    }
};

#endif

/* End Doxygen group
 * @}
 */

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
