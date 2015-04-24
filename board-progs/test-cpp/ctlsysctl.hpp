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
        }
        uint32_t ui32Status = static_StartCritical();
        SysCtlPeripheralEnable(periph_base);
        static_EndCritical(ui32Status);
    }
};

#endif

/* End Doxygen group
 * @}
 */
