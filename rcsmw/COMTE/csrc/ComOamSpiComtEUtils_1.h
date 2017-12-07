#ifndef ComOamSpiComtEUtils_1_h
#define ComOamSpiComtEUtils_1_h

#include <stdlib.h>
#include <MafOamSpiManagedObject_3.h>
#include <MafMgmtSpiCommon.h>

// AVC Things
MafMoAttributeValueContainer_3T* create_avc(MafOamSpiMoAttributeType_3T type, uint16_t nrOfValues);
MafMoNamedAttributeValueContainer_3T* create_navc(uint8_t* attr_name,
                                                  uint32_t attr_name_length,
                                                  uint8_t type,
                                                  uint16_t nrOfValues);
void destroy_avc(MafMoAttributeValueContainer_3T* container);
void destroy_avc_opt(MafMoAttributeValueContainer_3T* container, bool freeContainer);
void destroy_navc(MafMoNamedAttributeValueContainer_3T* namedContainer);

#endif
