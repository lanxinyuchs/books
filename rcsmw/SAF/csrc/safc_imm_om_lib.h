/*
 * %EricssonCopyright%
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012. All Rights Reserved.
 *
 * The program may be used and/or copied only with the written permission from
 * Ericsson AB, or in accordance with the terms and conditions stipulated in
 * the agreement/contract under which the program has been supplied.
 *
 * %CopyrightEnd%
 *
 * ----------------------------------------------------------------------
 */
#ifndef _SAFC_IMM_OM_LIB_H
#define _SAFC_IMM_OM_LIB_H

#include "safc_imm_lib.h"

/*
 * Client structures
 */
typedef struct imm_om_client {
   int hasCallbacks;
   union {
      SaImmCallbacksT callbacks;
      SaImmCallbacksT_o2 callbacksA2b;
   } cb;
   int isImmA2b;
   int sockfd;
   int cb_sockfd;
   long long srv_handle;
   pthread_mutex_t mutex;
   safc_array allocated_memory;
} SafcImmOmClientT;

typedef struct imm_admin_owner_client {
   SaImmHandleT immHandle;
   SaUint32T adminOwnerId;
   SaImmAdminOwnerNameT adminOwnerName;
   SaBoolT releaseOnFinalize;
   long long srv_handle;
} SafcImmAdminOwnerClientT;

typedef struct {
   SaImmHandleT immHandle;
   long long srv_handle;
   /* Maybe a mutex is needeed */
   int objectsOrAttributes;
   SaImmSearchObjectsT_s2 **objects;
   SaImmAttrValuesT_2 **attributes;
} SafcImmSearchClientT;

typedef struct {
   SaImmHandleT immHandle;
   long long srv_handle;
   /* Maybe a mutex is needeed */
   SaImmAttrValuesT_2 **attributes;
} SafcImmAccessorClientT;

typedef struct {
   SafcImmOmClientT *immClient;
} SafcImmAdminOwnerHandleT;

typedef struct {
   SaImmHandleT immHandle;
   SaImmAdminOwnerHandleT aoHandle;
   SaImmCcbFlagsT ccbFlags;
   SaUint32T erlCcbHandle;
   /* Maybe a mutex is needeed */
   SaImmAttrValuesT_2 **attributes;
   SaStringT *errorStrings;
} SafcImmCcbClientT;

typedef enum _SafcImmOmMemoryTypeT {
   class_definition_attrs = 1,
   admin_op_params = 2
} SafcImmOmMemoryTypeT;

typedef struct {
   SafcImmOmMemoryTypeT type;
   void* ptr;
} SafcImmOmMemoryStructT;

#endif
