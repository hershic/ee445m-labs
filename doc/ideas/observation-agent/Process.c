#include "Process.h"

#include <stdlib.h>

ProcessEnvelope* new_ProcessEnvelope() {

  ProcessEnvelope* envelope = calloc(1, sizeof(ProcessEnvelope));
  return envelope;
}
