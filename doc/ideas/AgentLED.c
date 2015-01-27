#include "AgentLED.h"

/* TODO: use a macro to create the _as_string part of the function
 * name from a defined base ('led_identify') */
StringCase led_string_cases[] = {
  {kString, led_identify_as_string},
};

char* led_identify_as_string() {

  return "> I am an LED! This is my string";
}

char* led_identify(char* identification_format) {

  return StringCase_chars_chars(led_string_cases, identification_format);
}

bool led_make_observable() {

}

bool deliver_process_envelope(ProcessEnvelope* envelope) {

}
