#ifndef ComMwSpiComtEAccessManagement_1_h
#define ComMwSpiComtEAccessManagement_1_h

#include <MafMwSpiAccessManagement_1.h>

typedef struct comte_am comte_am_t;

#include "ComMwSpiComtEComponent_1.h"

struct comte_am {
	MafMwSpiAccessManagement_1T base;
};

MafReturnT comte_access_management_create(comte_mw_component_t* component);
MafReturnT comte_access_management_destroy(comte_mw_component_t* component);

#endif
