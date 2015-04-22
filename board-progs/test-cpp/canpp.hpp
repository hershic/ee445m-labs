#ifndef __canpp__
#define __canpp__

/*! \addtogroup
 * @{
 */

#include <stdint.h>
#include <stdbool.h>

typedef uint32_t memory_address_t;

class can {
private:
    memory_address_t base;
    uint32_t interrupt;
    bool sender;
    uint32_t errors_tx;

    /*! Configure CAN for 1 Mbit operation -- http://bit.ly/1CW7hUO */
    void set_timing(void);
public:
    static bool error_flag;

    can();
    can(memory_address_t can_base, uint32_t can_interrupt, bool can_sender);

    /*! Enable CAN transmissions. */
    void enable(void);

    /*! Disable CAN transmissions. */
    void disable(void);

    /*! todo */
    void transmit(uint8_t* data, uint32_t length, uint32_t id);
};

/*! End doxygen group
 * @}
 */


#endif
