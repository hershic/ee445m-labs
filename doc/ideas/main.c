#include "ObservationAgent.h"

#include <stdlib.h>

#ifndef NDEBUG
#include <stdio.h>
#endif

#include "StringCase.h"

char* led_identify_as_string() {

  return "> I am an LED! This is my string\n";
}

StringCase led_string_cases[] = {
  {kString, led_identify_as_string},
};

char* led_identify(char* identification_format) {

  return StringCase_chars_chars(led_string_cases, identification_format);
}

bool led_make_observable() {

}

int main(void) {

  Node* root = calloc(1, sizeof(Node));
  Node* next = calloc(1, sizeof(Node));
  root->process = 1;
  root->next = next;
  next->process = 2;
  next->next = 0;

  /* Create the observation agent for our (imaginary) LED */
  char description[] = "Test agent";
  ObservationAgent* led = new( ObservationAgent(description) );
  led->identify = led_identify;
  led->make_observable = led_make_observable;

#ifndef NDEBUG
  printf("I am going to ask the led ObservationAgent to identify itself:\n%s\n",
	 led->identify(kString));
  printf("Ok, I am pretty sure this is an LED I can work with.\n");
  printf("Let's try to pass it a message\n");
#endif
  /* TODO: simplify this code with a lib */
  ProcessEnvelope envelope = calloc(1, sizeof(ProcessEnvelope));

  envelope->process = monitor1;
  led->deliver_process_envelope(envelope);

  /* TODO: display contents of process_envelope */
  return 0;
}
