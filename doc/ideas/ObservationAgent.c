#include "ObservationAgent.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* TODO: Doxygenize */

ObservationAgent* new_ObservationAgent(char* description) {

  ObservationAgent* agent = calloc(1, sizeof(ObservationAgent));
  memcpy(description, agent->description, OA_MAX_LEN_DESCRIPTION);
  agent->description[OA_MAX_LEN_DESCRIPTION] = 0;
  return agent;
}

/* TODO: implement */
bool is_ObservationAgent() {

}

char* led_identify() {

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
}
