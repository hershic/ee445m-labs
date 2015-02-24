#ifndef __BUTTON__
#define __BUTTON__

#include "libstd/nexus.h"
#include "libhw/hardware.h"

/*! Create a \hw_metadata struct named \_name. */
#define button_metadata_init_(_name, _base, _pin, _interrupt) \
  hw_metadata _name;                                          \
  _name.button.base = _base;                                  \
  _name.button.pin = _pin;                                    \
  _name.button.int_type = _interrupt

/*! Create a hardware_metadata struct named `button_metadata' */
#define button_metadata_init(_base, _pin, _interrupt) \
  hw_metadata button_metadata;                        \
  button_metadata.button.base = _base;                \
  button_metadata.button.pin = _pin;                  \
  button_metadata.button.int_type = _interrupt

#define BUTTON_LEFT          GPIO_PIN_4
#define BUTTON_RIGHT         GPIO_PIN_0
#define BUTTONS_BOTH         (BUTTON_LEFT | BUTTON_RIGHT)

void button_set_interrupt(hw_metadata, memory_address_t);

#endif  /* __BUTTON__ */
