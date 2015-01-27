#ifndef _AgentLED_
#define _AgentLED_

#include <stdbool.h>

#include "Agent.h"
#include "Process.h"

/* Must include the 3 functions that define an ObservationAgent */
AgentID led_identify();
bool led_make_observable();
bool deliver_process_envelope(ProcessEnvelope* envelope);

/* Ways this Agent knows to identify itself */
char* led_identify_as_string();

#endif
