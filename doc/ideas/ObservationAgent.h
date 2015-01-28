#ifndef _OBSERVATION_AGENT_
#define _OBSERVATION_AGENT_

#include <stdbool.h>

#include "globals.h"
#include "Agent.h"
#include "Process.h"

/* TODO: Doxygenize */

/* 15 chars for description ++ '\0' */
#define OA_MAX_LEN_DESCRIPTION 16

typedef struct {

  /* Member variables */
  char description[OA_MAX_LEN_DESCRIPTION];

  /* Member functions */
  AgentID (*identify)();
  bool (*deliver_process_envelope)(ProcessEnvelope*);
  bool (*make_observable)(char*);
} ObservationAgent;

/* Agent transformation functions */
ObservationAgent* new_ObservationAgent(char*);
bool is_ObservationAgent();

/* Agent metadata functions */


#endif
