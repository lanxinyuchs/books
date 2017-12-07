#ifndef GLMS_PERSISTENT_STORAGE_H
#define GLMS_PERSISTENT_STORAGE_H

#include "stdint.h"

/* This is a list of clientId's that is used to request different tables */
#define GLMS_NO_OF_PARAMETER_TABLES       (11)
#define GLMS_TABLE_RID_LM                 (0)
#define GLMS_TABLE_RID_KEYFILE            (1)
#define GLMS_TABLE_RID_FEATURESTATE       (2)
#define GLMS_TABLE_RID_FEATUREKEY         (3)
#define GLMS_TABLE_RID_EU                 (4)
#define GLMS_TABLE_RID_IU                 (5)
#define GLMS_TABLE_RID_PU                 (6)
#define GLMS_TABLE_RID_AM                 (7)
#define GLMS_TABLE_RID_CAPACITYSTATE      (8)
#define GLMS_TABLE_RID_CAPACITYKEY        (9)
#define GLMS_TABLE_RID_LICENSESUPPORT     (10)

#define GLMS_SP  (0)
#define GLMS_PP  (1)

void             pp_requestIndexList(uint32_t requestId);
void             pp_set(uint32_t requestId, uint32_t index, const char *value);
void             pp_get(uint32_t requestId);
void             pp_deleteIndex(uint32_t requestId, uint32_t index);

void             sp_requestIndexList(uint32_t requestId);
void             sp_set(uint32_t requestId, uint32_t index, const char *value);
void             sp_get(uint32_t requestId);
void             sp_deleteIndex(uint32_t requestId, uint32_t index);

int32_t          getParameterTableFromSig(uint32_t sigNo);

/* rid map */
int32_t          ridMap_parseStoredParameters(union itc_msg *sig, uint32_t rid);

#endif /* GLMS_PERSISTENT_STORAGE_H */
