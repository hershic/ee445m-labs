#include <stdbool.h>

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

  /* long size; */
  Node* process;
} ProcessEnvelope;

typedef struct {

  /* Member variables */
  char description[OA_MAX_LEN_DESCRIPTION];

  /* Member functions */
  char* (*identify)();
  bool (*make_observable)();
} ObservationAgent;

ObservationAgent* new_ObservationAgent(char* description);
bool is_ObservationAgent();
