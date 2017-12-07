#ifndef ComOamSpiComtEManagedObject_1_h
#define ComOamSpiComtEManagedObject_1_h

#include <MafOamSpiManagedObject_3_1.h>

typedef struct comte_mo comte_mo_t;

#include "ComOamSpiComtEComponent_1.h"

struct comte_mo {
    MafOamSpiManagedObject_3T base;
    MafOamSpiManagedObject_3_1T base_ext;
};

MafReturnT comte_managed_object_create(comte_oam_component_t* component);
MafReturnT comte_managed_object_destroy(comte_oam_component_t* component);

#endif
