#ifndef ComOamSpiComtEPm_1_h
#define ComOamSpiComtEPm_1_h

#include <ComOamSpiPm_2.h>

#define CAT_HELPER(a, b) a ## b
#define CAT(a, b) CAT_HELPER(a, b)

typedef struct comte_pm comte_pm_t;

#include "ComOamSpiComtEComponent_1.h"

struct comte_pm {
	ComOamSpiPm_2T base;
};


MafReturnT comte_pm_create(comte_oam_component_t* component);
MafReturnT comte_pm_destroy(comte_oam_component_t* component);


#endif

