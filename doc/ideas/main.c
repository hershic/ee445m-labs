#include "ObservationAgent.h"
#include "AgentLED.h"

#include "Process.h"

#include <stdlib.h>

#ifndef NDEBUG
#include <stdio.h>
#endif

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
  ProcessEnvelope* envelope = new( ProcessEnvelope() );

  led->deliver_process_envelope(envelope);

  /* TODO: display contents of process_envelope */
  return 0;
}
