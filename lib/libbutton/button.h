#ifndef __BUTTON__
#define __BUTTON__

#define BUTTON_LEFT             GPIO_PIN_4
#define BUTTON_RIGHT            GPIO_PIN_0
#define BUTTONS_ALL             (BUTTON_LEFT | BUTTON_RIGHT)

void buttons_init();

#endif  /* __BUTTON__ */
