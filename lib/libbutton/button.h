#ifndef __BUTTON__
#define __BUTTON__

/* TODO: Make sure other drivers conform to this behavior */
#define button_metadata_init(_name, _base, _pin, _int_type) \
    hw_metadata _name;                                      \
    _name.button.base = _base;                              \
    _name.button.pin = _pin;                                \
    _name.button.int_type = _int_type;                      \

#define BUTTON_LEFT             GPIO_PIN_4
#define BUTTON_RIGHT            GPIO_PIN_0
#define BUTTONS_ALL             (BUTTON_LEFT | BUTTON_RIGHT)

void buttons_init();

#endif  /* __BUTTON__ */
