#ifndef _PROCESS_
#define _PROCESS_

/* TODO: Doxygenize */

typedef long pid;

typedef struct Node {

  pid process;
  struct Node* next;
} Node;

typedef struct {

  Node* process;
} ProcessEnvelope;

ProcessEnvelope* new_ProcessEnvelope();

#endif
