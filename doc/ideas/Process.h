#ifndef _PROCESS_
#define _PROCESS_

/* TODO: Doxygenize */

typedef long pid;

typedef struct ProcessNode {

  pid process;
  struct ProcessNode* next;
} ProcessNode;

typedef struct {

  ProcessNode* process;
} ProcessEnvelope;

ProcessEnvelope* new_ProcessEnvelope();

#endif
