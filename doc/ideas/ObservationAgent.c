#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "ObservationAgent.h"

ObservationAgent* new_ObservationAgent(char* description) {

  ObservationAgent* agent = calloc(1, sizeof(ObservationAgent));
  memcpy(description, agent->description, OA_MAX_LEN_DESCRIPTION);
  agent->description[OA_MAX_LEN_DESCRIPTION] = 0;
  return agent;
}

/* TODO: implement */
bool is_ObservationAgent() {

}

int main(void) {

  Node* root = calloc(1, sizeof(Node));
  Node* next = calloc(1, sizeof(Node));
  root->next = next;
  root->process = 1;
  next->next = 0;
  next->process = 2;


  char description[] = "Test agent";
  ObservationAgent* led = new(ObservationAgent(description));
}
