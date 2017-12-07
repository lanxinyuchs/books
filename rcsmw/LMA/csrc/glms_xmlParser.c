/**
 *   Copyright (C) 2013 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2013-06-29 Ramakrushna Mishra
 *   Change  : First version.
 *
 * ========================================================================
 */

/******************************************************************************
 * INCLUDE FILES
 *****************************************************************************/

#include <itc.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <stdarg.h>

#include "com_ericsson_glms.h"
#include "kfParser.h"
#include "osa_xmlParser.h"
#include "ctype.h"



/*******************************************************************************
 * TYPES
 ******************************************************************************/
typedef struct XmlParser
{
   OSA_XML_PARSER(osaXmlParser);
   KeyFileInstallData *keyFileInstallData_p;
} XmlParser;

typedef enum
{
   FEATUREKEY,
   CAPACITYKEY,
   EMERGENCYRESETKEY
} Key;

/******************************************************************************
 * FUNCTIONS
 *****************************************************************************/
static GlmsBool
isComplementaryDataPresent(const char *buf_p);

static GlmsBool
hwacString(const char *buf_p, char *hwac);


/******************************************************************************
 *
 * Name  : createFormatStr
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
static char *
createFormatStr(char *fmt_p, ...)
{
#define MAX_FORMAT_LEN (128)
   static char format[MAX_FORMAT_LEN];
   va_list arg;

   va_start(arg, fmt_p);

   if(vsnprintf(format, MAX_FORMAT_LEN, fmt_p, arg) >= MAX_FORMAT_LEN)
   {
      format[0] = '\0';
   }

   va_end(arg);
   return(format);
}


/******************************************************************************
 *
 * Name  : bodyFormatVersion
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
static GlmsBool
bodyFormatVersion(const void *xmlParser_p)
{
   char str[] = "<body formatVersion=\"%%%d[^\"]\" signatureType=\"%%%s\">";
   KeyFileInstallData *keyFileInstallData_p;

   keyFileInstallData_p =
      ((XmlParser *)xmlParser_p)->keyFileInstallData_p;

   if (sscanf(osaXmlParserTag(xmlParser_p),
              createFormatStr(str, GLMS_FORMAT_VERSION_LEN - 1, "u"),
              keyFileInstallData_p->formatVersion,
              &keyFileInstallData_p->signatureType) == 2)
   {
      return(GLMS_TRUE);
   }

   return(GLMS_FALSE);
}

/******************************************************************************
 *
 * Name  : sequenceNumber
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
static GlmsBool
sequenceNumber(const void *xmlParser_p)
{
   KeyFileInstallData *keyFileInstallData_p;
   keyFileInstallData_p =
      ((XmlParser *)xmlParser_p)->keyFileInstallData_p;

   if (sscanf(osaXmlParserTag(xmlParser_p),
              "<sequenceNumber>%u</sequenceNumber>",
              &keyFileInstallData_p->sequenceNumber) == 1)
   {
      return(GLMS_TRUE);
   }

   return(GLMS_FALSE);
}

/******************************************************************************
 *
 * Name  : swlt
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
static GlmsBool
swlt(const void *xmlParser_p)
{
   char *start_p;
   char  str[] = "productType=\"%%%d[^\"]\" swltId=\"%%%d[^\"]\"";

   KeyFileInstallData *keyFileInstallData_p;
   keyFileInstallData_p =
      ((XmlParser *)xmlParser_p)->keyFileInstallData_p;

   start_p = strstr(osaXmlParserTag(xmlParser_p), "productType=");

   if (start_p != NULL)
   {
      if (sscanf(start_p,
                 createFormatStr(str, GLMS_PRODUCT_TYPE_LEN - 1, GLMS_SWLT_LEN - 1),
                 keyFileInstallData_p->productType,
                 keyFileInstallData_p->swlt) == 2)
      {
         return (GLMS_TRUE);
      }
   }

   return GLMS_FALSE;
}

/******************************************************************************
 *
 * Name  : fingerPrint
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
static GlmsBool
fingerPrint(const void *xmlParser_p)
{
   char str[] = "<fingerprint method=\"%%%s\" print=\"%%%d[^\"^>]\">";

   KeyFileInstallData *keyFileInstallData_p;
   keyFileInstallData_p =
      ((XmlParser *)xmlParser_p)->keyFileInstallData_p;

   if (sscanf(osaXmlParserTag(xmlParser_p),
              createFormatStr(str, "u", GLMS_FINGERPRINT_LEN - 1),
              &keyFileInstallData_p->fingerprintMethod,
              keyFileInstallData_p->fingerprint) == 2)
   {
      return (GLMS_TRUE);
   }

   return (GLMS_FALSE);
}


/******************************************************************************
 *
 * Name  : featureKeyId
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
static GlmsBool
featureKeyId(const char *buf_p,char * keyId  )
{
   char *keyId_p= strstr(buf_p, "<featureKey id=");

   if (keyId_p != NULL)
   {
      if (sscanf(keyId_p,
                 createFormatStr("<featureKey id=\"%%%d[^\"^>]\">",
                                 GLMS_KEY_ID_LEN - 1),
                 keyId) == 1)
      {
         formatKeyId(keyId, GLMS_KEY_ID_LEN);
         return (GLMS_TRUE);
      }
   }

   return (GLMS_FALSE);
}
/******************************************************************************
 *
 * Name  : capacityKeyId
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
static GlmsBool
capacityKeyId(const char *buf_p, char * keyId)
{
   char *keyId_p = strstr(buf_p, "<capacityKey id=");

   if (keyId_p != NULL)
   {
      if (sscanf(keyId_p,
                 createFormatStr("<capacityKey id=\"%%%d[^\"^>]\">",
                                 GLMS_KEY_ID_LEN - 1),
                keyId ) == 1)
      {
         formatKeyId(keyId,GLMS_KEY_ID_LEN );
         return(GLMS_TRUE);
      }
   }

   return(GLMS_FALSE);
}

/******************************************************************************
 *
 * Name  : formatKeyId
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/

void
formatKeyId (char *keyId_p, uint32_t len)
{
   char *read_p = keyId_p;
   char *write_p = keyId_p;
   uint32_t i = 0;

   while ((*read_p != '\0') && (i++ < len))
   {
       if (isspace ((int32_t) (*read_p)) == 0)
       {
	   /* Convert to upper case */
	   if (islower ((int32_t) (*read_p)))
	   {
	       *write_p = (*read_p - 'a') + 'A';
	   }
	   else
	   {
	       *write_p = *read_p;
	   }

	   write_p++;
       }

       read_p++;
   }

   *write_p = '\0';
}

/******************************************************************************
 *
 * Name  : startDate
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
static GlmsBool
startDate(const char *buf_p, time_t32 * startDate )
{
   char *start_p = strstr(buf_p, "<start>");
   struct timespec rawtime;
   struct tm *time_tm;
   int max_int = 0xffff;


   clock_gettime(CLOCK_REALTIME, &rawtime);
   time_tm = gmtime(&rawtime.tv_sec);

   time_tm->tm_sec    = 0;
   time_tm->tm_min    = 0;
   time_tm->tm_hour   = 0;
   time_tm->tm_mday   = max_int;
   time_tm->tm_mon    = max_int;
   time_tm->tm_year   = max_int;

   if (start_p != NULL)
   {
      if (sscanf(start_p, "<start>%4d-%2d-%2d</start>",
                 &time_tm->tm_year,
                 &time_tm->tm_mon,
                 &time_tm->tm_mday) == 3)
      {
         time_tm->tm_mon    =((time_tm->tm_mon)-1) ;
         time_tm->tm_year   = ((time_tm->tm_year)-1900);

         *startDate = mktime(time_tm);

	 return(GLMS_TRUE);
      }
   }

   return(GLMS_FALSE);
}

/******************************************************************************
 *
 * Name  : stopDate
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
static GlmsBool
stopDate(const char *buf_p,time_t32 * stopDate,Key keyType )
{
   char *stop_p = strstr(buf_p, "<stop>");
   struct timespec rawtime;
   struct tm *time_tm;

   clock_gettime(CLOCK_REALTIME, &rawtime);
   time_tm = gmtime(&rawtime.tv_sec);

   time_tm->tm_sec    = 0;
   time_tm->tm_min    = 0;
   time_tm->tm_hour   = 0;
   time_tm->tm_mday   = 31;
   time_tm->tm_mon    = 11;
   time_tm->tm_year   = 8099;


   if (stop_p != NULL)
   {
      if (sscanf(stop_p, "<stop>%4d-%2d-%2d</stop>",
                 &time_tm->tm_year,
                 &time_tm->tm_mon,
                 &time_tm->tm_mday) == 3)
      {
           time_tm->tm_mon    =((time_tm->tm_mon)-1) ;
           time_tm->tm_year   = ((time_tm->tm_year)-1900);
           *stopDate= mktime(time_tm);
	   return(GLMS_TRUE);
      }
   }
   /* Stop tag is mandatory for an emergency reset key */
   else if (keyType != EMERGENCYRESETKEY )
   {
      if (((strstr(buf_p, "<noStop>") != NULL) &&
           (strstr(buf_p, "</noStop>") != NULL)) ||
           (strstr(buf_p, "<noStop/>") != NULL))
      {
         *stopDate = GLMS_NO_STOP_DATE;
         return(GLMS_TRUE);
      }
   }

   return(GLMS_FALSE);
}
/******************************************************************************
 *
 * Name  : capacity
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
static GlmsBool
capacity(const char *buf_p, GlmsCapacityValue  *capacity)
{
   char *capacity_p = strstr (buf_p, "<capacity>");

   if (capacity_p != NULL)
   {
      if (sscanf(capacity_p,
                 "<capacity>%d</capacity>",
                  &capacity->value) == 1)
      {
         capacity->noLimit = GLMS_FALSE;
         return (GLMS_TRUE);
      }
   }
   else if (((strstr (buf_p, "<noCapacityLimit>") != NULL) &&
            (strstr (buf_p, "</noCapacityLimit>") != NULL)) ||
	    (strstr (buf_p, "<noCapacityLimit/>") != NULL))
   {
      capacity->value = 0;
      capacity->noLimit = GLMS_TRUE;
      return (GLMS_TRUE);
   }
   else if (((strstr (buf_p, "<notContractuallyLimited>") != NULL) &&
            (strstr (buf_p, "</notContractuallyLimited>") != NULL)) ||
	    (strstr (buf_p, "<notContractuallyLimited/>") != NULL))
   {
      capacity->value = -1;
      capacity->noLimit = GLMS_TRUE;
      return (GLMS_TRUE);
   }

   return(GLMS_FALSE);
}

/******************************************************************************
 *
 * Name  : hardLimit
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
static GlmsBool
hardLimit(const char *buf_p, GlmsCapacityValue * hardLimit)
{
   char *hardLimit_p = strstr (buf_p, "<hardLimit>");

   if (hardLimit_p != NULL)
   {
      if (sscanf(hardLimit_p,
                 "<hardLimit>%d</hardLimit>",
                 &hardLimit->value) == 1)
      {
         hardLimit->noLimit = GLMS_FALSE;
        return(GLMS_TRUE);
      }
   }
   else if (((strstr (buf_p, "<noHardLimit>") != NULL) &&
            (strstr (buf_p, "</noHardLimit>") != NULL)) ||
	    (strstr (buf_p, "<noHardLimit/>") != NULL))
   {

      hardLimit->value = 0;
      hardLimit->noLimit = GLMS_TRUE;
      return(GLMS_TRUE);
   }

   return(GLMS_FALSE);
}

static GlmsBool
isComplementaryDataPresent(const char *buf_p)
{

  if (strstr(buf_p, "<ComplementaryData>"))
  {
    return GLMS_TRUE;
  }

  return GLMS_FALSE;
}

/******************************************************************************
 *
 * Name  : hwacString
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
static GlmsBool
hwacString(const char *buf_p, char *hwac)
{
   char *start_p = strstr(buf_p, "<HWAC>");
   char *stop_p = strstr(buf_p, "</HWAC>");
   size_t len;

   if (start_p && stop_p && stop_p > start_p)
   {
     len = stop_p - (start_p + strlen("<HWAC>"));
     if(len >= MAX_SIZE_OF_LCCI_LICENSE_INFO_S_HWAC_STRING)
        len = MAX_SIZE_OF_LCCI_LICENSE_INFO_S_HWAC_STRING - 1;
     strncpy(hwac, start_p + strlen("<HWAC>"), len);
     hwac[len] = '\0';

     return GLMS_TRUE;
   }

   return(GLMS_FALSE);
}

/******************************************************************************
 *
 * Name  : keyFeature
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
static GlmsBool
keyFeature(const void *xmlParser_p)
{
   Key keyType = FEATUREKEY;
   GlmsBool parsed= GLMS_FALSE;
   char     currKeyId[GLMS_KEY_ID_LEN];
   time_t32   currStartDate;
   time_t32   currStopDate;
   FeatureKeyInstallData *currFeatureKey,*newFeatureKey,*lastFeatureKey;
   FeatureKeyInstanceInstallData *currFeatureKeyInstance,*newFeatureKeyInstance;

   KeyFileInstallData *keyFileInstallData_p;
   keyFileInstallData_p =
      ((XmlParser *)xmlParser_p)->keyFileInstallData_p;

   if(featureKeyId(osaXmlParserTag(xmlParser_p), currKeyId) &&
      startDate(osaXmlParserTag(xmlParser_p), &currStartDate) &&
      stopDate(osaXmlParserTag(xmlParser_p), &currStopDate, keyType))
   {
      parsed =GLMS_TRUE;
   }

   if(parsed)
   {
      /* Find Feature Key */
      lastFeatureKey = NULL;
      for(currFeatureKey = keyFileInstallData_p->featureKeyList;
          currFeatureKey != NULL;
          currFeatureKey = currFeatureKey->nextFeatureKey)
      {
         if(strcmp(currKeyId, currFeatureKey->keyId) == 0)
         {
            break;
         }

         if(currFeatureKey->nextFeatureKey == NULL)
         {
            lastFeatureKey = currFeatureKey;
         }
      }

      /* Feature Key doesn't exist so create it */
      if(currFeatureKey == NULL)
      {
         newFeatureKey = malloc(sizeof( struct FeatureKeyInstallData_s));
         strcpy(newFeatureKey->keyId ,currKeyId );
         newFeatureKey->instanceList = NULL;
         newFeatureKey->nextFeatureKey = NULL;

         if(lastFeatureKey != NULL)
         {
            lastFeatureKey->nextFeatureKey = newFeatureKey;
         }
         else
         {
            keyFileInstallData_p->featureKeyList = newFeatureKey;
         }

         currFeatureKey = newFeatureKey;
      }

      /* Create Key Instance */
      newFeatureKeyInstance  = malloc(sizeof(struct FeatureKeyInstanceInstallData_s));
      newFeatureKeyInstance->startDate = currStartDate;
      newFeatureKeyInstance->stopDate  = currStopDate;
      newFeatureKeyInstance->nextInstance = NULL;

      /* Find last Capacity Key instance */
      if(currFeatureKey->instanceList == NULL)
      {
         currFeatureKey->instanceList = newFeatureKeyInstance;
      }
      else
      {
         for(currFeatureKeyInstance = currFeatureKey->instanceList;
             currFeatureKeyInstance->nextInstance != NULL;
             currFeatureKeyInstance = currFeatureKeyInstance->nextInstance);

         currFeatureKeyInstance->nextInstance = newFeatureKeyInstance;
      }

      return(GLMS_TRUE);
   }

   return (GLMS_FALSE);
 }
 /******************************************************************************
 *
 * Name  : keyCapacity
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
static GlmsBool
keyCapacity(const void *xmlParser_p)
{
   Key keyType = CAPACITYKEY;
   GlmsBool parsed = GLMS_FALSE;
   char     currKeyId[GLMS_KEY_ID_LEN];
   time_t32   currStartDate;
   time_t32   currStopDate;
   GlmsCapacityValue  currCapacity;
   GlmsCapacityValue  currHardLimit;
   char currHwacString[MAX_SIZE_OF_LCCI_LICENSE_INFO_S_HWAC_STRING] = "";
   CapacityKeyInstallData *currCapacityKey,*newCapacityKey,*lastCapacityKey;
   CapacityKeyInstanceInstallData *currCapacityKeyInstance,*newCapacityKeyInstance;

   KeyFileInstallData *keyFileInstallData_p;
   keyFileInstallData_p =
      ((XmlParser *)xmlParser_p)->keyFileInstallData_p;

   if(capacityKeyId(osaXmlParserTag(xmlParser_p),currKeyId))
   {
     if(startDate(osaXmlParserTag(xmlParser_p), &currStartDate ))
     {
       if(stopDate(osaXmlParserTag(xmlParser_p), &currStopDate ,keyType ))
       {
         if(capacity(osaXmlParserTag(xmlParser_p), &currCapacity ))
	 {
           if(hardLimit(osaXmlParserTag(xmlParser_p), &currHardLimit ))
	   {
             // here for the new PV2 version HWAC string is optional to parse
             // within "<ComplementaryData><HWAC>...</HWAC></ComplementaryData>" tags
             if(isComplementaryDataPresent(osaXmlParserTag(xmlParser_p)))
             {
               if(hwacString(osaXmlParserTag(xmlParser_p), currHwacString))
               {
                 parsed = GLMS_TRUE;
               }
             }
             else
             {
               parsed = GLMS_TRUE;
             }
	   }
	 }
       }
     }
   }

   if(parsed)
   {
     /* Find Capacity Key */
     lastCapacityKey = NULL;
     for(currCapacityKey = keyFileInstallData_p->capacityKeyList;
         currCapacityKey != NULL;
         currCapacityKey = currCapacityKey->nextCapacityKey)
     {
        if(strcmp(currKeyId, currCapacityKey->keyId) == 0)
        {
           break;
        }

        if(currCapacityKey->nextCapacityKey == NULL)
        {
           lastCapacityKey = currCapacityKey;
        }
     }

     /* Capacity Key doesn't exist so create it */
     if(currCapacityKey == NULL)
     {
        newCapacityKey = malloc(sizeof( struct CapacityKeyInstallData_s));
        strcpy(newCapacityKey->keyId, currKeyId);
        newCapacityKey->instanceList = NULL;
        newCapacityKey->nextCapacityKey = NULL;
        
        if(lastCapacityKey != NULL)
        {
           lastCapacityKey->nextCapacityKey = newCapacityKey;
        }
        else
        {
           keyFileInstallData_p->capacityKeyList = newCapacityKey;
        }
        
        currCapacityKey = newCapacityKey;
     }

     /* Create Key Instance */
     newCapacityKeyInstance = malloc(sizeof(struct CapacityKeyInstanceInstallData_s));
     newCapacityKeyInstance->startDate = currStartDate;
     newCapacityKeyInstance->stopDate  = currStopDate;
     newCapacityKeyInstance->capacity  = currCapacity;
     newCapacityKeyInstance->hardLimit = currHardLimit;
     strcpy(newCapacityKeyInstance->hwacString, currHwacString);
     if(newCapacityKeyInstance->capacity.value == -1)
     {
        newCapacityKeyInstance->notContractuallyLimited = 1;
     }
     else
     {
        newCapacityKeyInstance->notContractuallyLimited = 0;
     }
     newCapacityKeyInstance->nextInstance = NULL;
     
     /* Find last Capacity Key instance */
     if(currCapacityKey->instanceList == NULL)
     {
        currCapacityKey->instanceList = newCapacityKeyInstance;
     }
     else
     {
        for(currCapacityKeyInstance = currCapacityKey->instanceList;
            currCapacityKeyInstance->nextInstance != NULL;
            currCapacityKeyInstance = currCapacityKeyInstance->nextInstance);
        currCapacityKeyInstance->nextInstance = newCapacityKeyInstance;
     }
     return(GLMS_TRUE);
     
   }

   return(GLMS_FALSE);
 }

/******************************************************************************
 *
 * Name  : keyEmergencyReset
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
static GlmsBool
keyEmergencyReset(const void *xmlParser_p)
{
   time_t32 currStartDate;
   time_t32 currStopDate;
   KeyFileInstallData *keyFileInstallData_p;
   keyFileInstallData_p =
      ((XmlParser *)xmlParser_p)->keyFileInstallData_p;
   Key keyType = EMERGENCYRESETKEY;

   if (startDate(osaXmlParserTag(xmlParser_p),&currStartDate ))
   {
      if (stopDate(osaXmlParserTag(xmlParser_p),&currStopDate,keyType))
      {
          keyFileInstallData_p->emergencyKey.resetKeyAvailable = GLMS_TRUE;
	  keyFileInstallData_p->emergencyKey.startDate = currStartDate;
	  keyFileInstallData_p->emergencyKey.stopDate  = currStopDate;
          return(GLMS_TRUE);
      }
   }

   return(GLMS_FALSE);
}


/******************************************************************************
 *
 * Name  : parseLicenseKeyFile
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
ParseResult
parseKeyFile(const char *keyFileLocation,
             KeyFileInstallData **keyFileInstallData)
{
   ParseResult          result;
   KeyFileInstallData   *kfData;
   XmlParser            xmlParser;
   struct timespec      currentTime;

   static const Tag tagTable[] = {
      {"<body formatVersion=", NULL, bodyFormatVersion},
      {"<sequenceNumber>", "</sequenceNumber>", sequenceNumber},
      {"<SWLT", NULL, swlt},
      {"<fingerprint method=", NULL, fingerPrint},
      {"<featureKey id=", "</featureKey>", keyFeature},
      {"<capacityKey id=", "</capacityKey>", keyCapacity},
      {"<emergencyResetKey>", "</emergencyResetKey>", keyEmergencyReset}
   };


   kfData = (KeyFileInstallData *) malloc(sizeof(KeyFileInstallData));
   *keyFileInstallData = kfData;

   kfData->emergencyKey.resetKeyAvailable = GLMS_FALSE;
   kfData->emergencyKey.startDate = 0;
   kfData->emergencyKey.stopDate = 0;
   kfData->capacityKeyList = NULL;
   kfData->featureKeyList = NULL;
   kfData->swlt[0] = '\0';
   kfData->productType[0] = '\0';
   kfData->formatVersion[0] = '\0';
   kfData->fingerprint[0] = '\0';
   kfData->generated = 0;
   kfData->fingerprintMethod= 0;
   kfData->sequenceNumber= 0;
   kfData->signatureType= 0;

   xmlParser.keyFileInstallData_p = kfData;

   if (osaXmlParserInit(&xmlParser,
                        tagTable,
                        sizeof(tagTable),
                        4096) == OSA_XML_SUCCESS)
   {
      if (osaXmlFileParser(keyFileLocation, &xmlParser) == OSA_XML_SUCCESS)
      {
         clock_gettime(CLOCK_REALTIME, &currentTime);
         kfData->generated = currentTime.tv_sec;
      }
   }

   switch(osaXmlGetResult(&xmlParser))
   {
      case OSA_XML_SUCCESS:
         result = LKF_SUCCESS;
	 break;
      case OSA_XML_USER_ERROR:
         result = LKF_ERROR;
         break;
      case OSA_XML_FS_ERROR:
         result = FS_ERROR;
         break;
      case OSA_XML_MEM_ERROR:
         result = MEM_ERROR;
         break;
      case OSA_XML_ERROR:
         result = PROG_ERROR;
         break;
      default:
         result = LKF_ERROR;
         break;
   }

   if(result != LKF_SUCCESS)
   {
      tracepoint(com_ericsson_glms, info_trace_w_str_arg,
                 "parseKeyFile",
                 osaXmlGetErrorStr(&xmlParser));
   }

   return(result);
}
/******************************************************************************
 *
 * Name  : cleanUpFeatureKey
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
void
cleanUpFeatureKey(KeyFileInstallData *keyFileInstallData_p)
{
   FeatureKeyInstallData *currFeatureKey, *deleteFeatureKey;
   FeatureKeyInstanceInstallData *currFeatureKeyInstance,*deleteFeatureKeyInstance;

   for(currFeatureKey = keyFileInstallData_p->featureKeyList;
       currFeatureKey != NULL;
       /* currFeatureKey stepped inside body */)
   {
      for(currFeatureKeyInstance = currFeatureKey ->instanceList;
          currFeatureKeyInstance != NULL;
          /* currFeatureKeyInstance stepped inside body */ )
      {
         deleteFeatureKeyInstance = currFeatureKeyInstance;
         currFeatureKeyInstance = currFeatureKeyInstance->nextInstance;
         free(deleteFeatureKeyInstance);
      }

      deleteFeatureKey = currFeatureKey;
      currFeatureKey = currFeatureKey->nextFeatureKey;
      free(deleteFeatureKey);
   }
}
/******************************************************************************
 *
 * Name  : cleanUpCapacityKey
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
void
cleanUpCapacityKey(KeyFileInstallData *keyFileInstallData_p)
{
   CapacityKeyInstallData *currCapacityKey, *deleteCapacityKey;
   CapacityKeyInstanceInstallData *currCapacityKeyInstance,*deleteCapacityKeyInstance;

   for(currCapacityKey = keyFileInstallData_p->capacityKeyList;
       currCapacityKey != NULL;
       /* currCapacityKey is stepped inside body */ )
   {
      for(currCapacityKeyInstance = currCapacityKey ->instanceList;
          currCapacityKeyInstance != NULL;
          /* currCapacityKeyInstance is stepped inside body */ )
      {
         deleteCapacityKeyInstance = currCapacityKeyInstance;
         currCapacityKeyInstance = currCapacityKeyInstance->nextInstance;
         free(deleteCapacityKeyInstance);
      }

      deleteCapacityKey = currCapacityKey;
      currCapacityKey = currCapacityKey->nextCapacityKey;
      free(deleteCapacityKey);
   }
}

/******************************************************************************
 *
 * Name  : cleanUpLicenseKeyFile
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 *****************************************************************************/
void
freeKeyFileInstallData(KeyFileInstallData **keyFileInstallData_p)
{
   if(*keyFileInstallData_p != NULL)
   {
      cleanUpFeatureKey(*keyFileInstallData_p);
      cleanUpCapacityKey(*keyFileInstallData_p);
      free(*keyFileInstallData_p);
      *keyFileInstallData_p = NULL;
   }
}
