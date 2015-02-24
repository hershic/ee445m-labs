#ifndef __BUTTON__
#define __BUTTON__

#include "libstd/nexus.h"
#include "libhw/hardware.h"

/*! Create a \hw_metadata struct named \_name. */
#define button_metadata_init_(_name, _base, _pin, _interrupt) \
  hw_metadata _name;                                          \
  _name.button.base = (memory_address_t) _base;		      \
  _name.button.pin = (memory_address_t) _pin;		      \
  _name.button.int_type = (uint32_t) _interrupt

/*! Create a hardware_metadata struct named `button_metadata' */
#define button_metadata_init(_base, _pin, _interrupt)	  \
  hw_metadata button_metadata;				  \
  button_metadata.button.base = (memory_address_t) _base; \
  button_metadata.button.pin = (memory_address_t) _pin;	  \
  button_metadata.button.int_type = (uint32_t) _interrupt

#define BUTTON_LEFT          GPIO_PIN_4
#define BUTTON_RIGHT         GPIO_PIN_0
#define BUTTONS_BOTH         (BUTTON_LEFT | BUTTON_RIGHT)

/*! TODO: doxygenize */
void button_init(hw_metadata);

/*! TODO: doxygenize */
void button_set_interrupt(hw_metadata, memory_address_t);

#endif  /* __BUTTON__ */
