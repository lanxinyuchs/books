#ifndef GLMS_KEYFILE_DATA_H
#define GLMS_KEY_FILE_DATA_H

#include "stdint.h"
#include "glmsadpi/glmsDataTypes.h"
#include "kfParser.h"
#include "timetFix.h"

#define KEYFILE_PARSE_OK "keyfile_parse_ok"

typedef struct
{
   GlmsAsyncAction reportProgress;
   uint32_t        sequenceNumber;
   time_t32        installationTime;
   GlmsBool        locatable;
   char            productType[GLMS_PRODUCT_TYPE_LEN];

   /* Internal Data */
   int32_t         readStatus[2]; /* read status of stored parameters at startup */
   GlmsBool        pendingUpdates;
   GlmsBool        installActionOngoing;
   GlmsBool        keyFileFaultAlarmActive;
   uint32_t        kffAlarmEventId;
   GlmsBool        sendMoUpdateInd;
   GlmsBool        pendingInstallations;
   char *          installedLicenses;
   KeyFileInstallData *kfInstallData;
} KeyFileData;



/*
 * Key File data function prototypes.
 *
 */
void              keyFileDataInit();
char *            keyFile_getInstalledLicenses();
void              keyFile_freeInstalledLicenses();
uint32_t          keyFile_getSequenceNumber();
void              keyFile_setSequenceNumber(uint32_t newSequenceNumber);
time_t32          keyFile_getInstallationTime();
void              keyFile_setInstallationTime(time_t32 newKfInstallationTime);
GlmsBool          keyFile_getLocatable();
void              keyFile_setLocatable(GlmsBool kfRead);
char*             keyFile_getProductType();
void              keyFile_setProductType(char *newProductType);
GlmsBool          keyFile_hasKfBeenRead();
GlmsBool          keyFile_isRestricted(char *feature);
uint32_t          keyFile_getKffAlarmEventId();

GlmsAsyncAction  *keyFile_getPointerToReportProgress();
void              keyFile_installActionStart();
void              keyFile_installActionLkfDownloaded();
void              keyFile_installActionLkfInstalled();
void              keyFile_installActionFailed(char *resultInfo);
GlmsBool          keyFile_isInstallActionRunning();
uint32_t          keyFile_getInstallActionId();
void              keyFile_fillReportProgressStruct(GlmsAsyncAction *reportProgress);
char *            keyFile_readAndValidateKeyFile(const char *kfLocation);
void              keyFile_activateKeyFile(GlmsBool install);
void              keyFile_activateKeyFileFaultAlarm();
void              keyFile_ceaseKeyFileFaultAlarm();
GlmsAlarmState    keyFile_getKeyFileFaultAlarmState();
void              keyFile_storeParameters();
void              keyFile_fetchParameters();
int32_t           keyFile_parseStoredParameters(union itc_msg *sig);
void              keyFile_sendMoUpdateKeyfileInd();

/* Get internal data for dumping */
int32_t           keyFile_getStartupReadStatus(uint32_t elem);
GlmsBool          keyFile_getPendingUpdates();
GlmsBool          keyFile_getInstallActionOngoing();
GlmsBool          keyFile_getKfHasBeenReadByGlms();
GlmsBool          keyFile_getSendMoUpdateInd();


#endif /* GLMS_KEY_FILE_DATA_H */
