#ifndef GLMS_LICENSE_SUPPORT_DATA_H
#define GLMS_LICENSE_SUPPORT_DATA_H

#include "stdint.h"
#include "glmsadpi/glmsDataTypes.h"

typedef struct
{
   char         areaId[GLMS_AREA_ID_LEN];  /* Software Parameter */
   uint32_t     seqNr;  /* Persistent Parameter */

   /* Internal Data */
   GlmsBool     sendMoUpdateInd;
   GlmsBool     pendingUpdates[2];
   uint32_t     *tableIndex[2];
   uint32_t     indexesInTable[2];
   int32_t      readStatus[2];
   char         *alkf;
} LicenseSupportData;

struct AreaLicenseKey_s
{
   char            keyId[GLMS_KEY_ID_LEN];
   time_t32        validFrom;
   time_t32        expiration;
   GlmsLicenseType licenseType;
   /*GlmsAreaDataCapacityKey *capacityData;*/
};
typedef struct AreaLicenseKey_s AreaLicenseKey;

/*
 * License Support Data function prototypes
 *
 */
void        licenseSupportDataInit();
void        licenseSupportDataClear();
const char* ls_getAreaId();
void        ls_setAreaId(const char *newAreaId);
void        ls_setAlkf(const char *alkf);

void        ls_storeParameters();
void        ls_fetchParameters();
int32_t     ls_parseStoredParameters(union itc_msg *sig);
void        ls_sendMoUpdateLicenseSupportInd();
GlmsInstallAreaLicenseKeyResult ls_installAreaLicenseKeys(const char *alkf,
                                                          GlmsBool createStateMOs);
void        ls_reinstallAreaLicenseKeys();

#endif /* GLMS_LICENSE_SUPPORT_DATA_H */
