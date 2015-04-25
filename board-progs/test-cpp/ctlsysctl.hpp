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
