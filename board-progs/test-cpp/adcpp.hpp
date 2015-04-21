#ifndef __adcpp__
#define __adcpp__

#include <stdbool.h>
#include <stdint.h>

typedef uint32_t memory_address_t;

class adc {
private:
    memory_address_t base;

public:
    /*! Initialize adc. */
    adc();
    adc(memory_address_t adc_base);
};

#endif  /* __ADC__ */
