/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Hershal Bhave and Eric Crosson 2015-03-15 */
/* Revision history: Look in Git FGT */

#ifndef __blinker__
#define __blinker__

/*! \addtogroup
   * @{
    */

#include <stdint.h>

typedef uint32_t port_t;
typedef enum color_t {RED, GREEN, BLUE} color_t;

class blinker {
public:
    blinker();
    void set_base(port_t port);
    void turn_on(color_t color);
    void turn_off(color_t color);
    void toggle(color_t color);
private:
    uint32_t get_led_pin_from_color(color_t color);
    uint32_t port_base;
};

#endif

/* End Doxygen group
    * @}
     */
