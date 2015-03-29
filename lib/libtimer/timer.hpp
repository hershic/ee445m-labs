#ifndef __timer__
#define __timer__

#define TIMER_DEFAULT_PERIODICITY TIMER_CFG_PERIODIC

#include <stdint.h>

class TimerController {
public:
    TimerController(uint32_t base,
		    uint32_t frequency,
		    uint32_t interrupt,
		    uint32_t periodic = TIMER_DEFAULT_PERIODICITY) :
	m_base(base) ,
	m_frequency(frequency) ,
	m_interrupt(interrupt) ,
	m_periodic(periodic) { }
private:
    uint32_t m_base;
    uint32_t m_frequency;
    uint32_t m_interrupt;
    uint32_t m_periodic; /* TIMER_CFG_PERIODIC or TIMER_CFG_ONE_SHOT */
}

#endif
