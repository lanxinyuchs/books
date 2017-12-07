#ifndef ComMwSpiComtELog_1_h
#define ComMwSpiComtELog_1_h

#include <MafMwSpiLog_1.h>
#include <MafMwSpiTrace_1.h>

typedef struct comte_log comte_log_t;
typedef struct comte_trace comte_trace_t;

#include "ComMwSpiComtEComponent_1.h"

struct comte_log {
	MafMwSpiLog_1T base;
};

struct comte_trace {
	MafMwSpiTrace_1T base;
};


MafReturnT comte_log_create(comte_mw_component_t* component);
MafReturnT comte_log_destroy(comte_mw_component_t* component);

#endif
