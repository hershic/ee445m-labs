#ifndef _OBSERVATION_AGENT_
#define _OBSERVATION_AGENT_

#include <stdbool.h>

#include "StringCase.h"

/* TODO: Doxygenize */

/* 15 chars for description ++ '\0' */
#define OA_MAX_LEN_DESCRIPTION 16

#define new(a) new_##a

typedef long pid;

typedef struct Node {

  pid process;
  struct Node* next;
} Node;

typedef struct {

  Node* process;
} ProcessEnvelope;

typedef struct {

  /* Member variables */
  char description[OA_MAX_LEN_DESCRIPTION];

  /* Member functions */
  char* (*identify)();
  bool (*deliver_process_envelope)(ProcessEnvelope*);
  bool (*make_observable)(char*);
} ObservationAgent;

ObservationAgent* new_ObservationAgent(char*);
bool is_ObservationAgent();

#endif
