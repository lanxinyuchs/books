#ifndef ComMwSpiComtEAvailabilityController_1_h
#define ComMwSpiComtEAvailabilityController_1_h

#include <MafMwSpiAvailabilityController_1.h>

typedef struct comte_ac comte_ac_t;

#include "ComMwSpiComtEComponent_1.h"

struct comte_ac {
	MafMwSpiAvailabilityController_1T base;
};

MafReturnT comte_availability_controller_create(comte_mw_component_t* component);
MafReturnT comte_availability_controller_destroy(comte_mw_component_t* component);

MafReturnT comte_setHaMode(MafMwSpiHaModeT haMode, MafMwSpiHaReasonT haReason);
MafReturnT comte_healthCheck();
MafReturnT comte_prepareTermination();
MafReturnT comte_terminateProcess();

#endif
