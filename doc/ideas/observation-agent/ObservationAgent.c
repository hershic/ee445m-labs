#include "ObservationAgent.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ObservationAgent* new_ObservationAgent(char* description) {

  ObservationAgent* agent = calloc(1, sizeof(ObservationAgent));
  memcpy(description, agent->description, OA_MAX_LEN_DESCRIPTION);
  agent->description[OA_MAX_LEN_DESCRIPTION] = 0;
  return agent;
}

/* TODO: implement */
bool is_ObservationAgent() {

}
