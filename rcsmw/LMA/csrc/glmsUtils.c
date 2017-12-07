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
 *   INCLUDE FILES
 * ========================================================================
 */

#include <itc.h>

#ifndef GLMS_DISABLE_FORCED_BIG_ENDIAN
#include <endian.h>
#endif

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <limits.h>
#include "glms_main.h"
#include "persistentStorage.h"
#include "com_ericsson_glms.h"
#include "glmsadpi/glms_adpi.sig"

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */


/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

union itc_msg
{
   uint32_t  sigNo;

   GlmsAdpiLogInd              glmsLogInd;
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

/* ========================================================================
 *   FUNCTION
 * ========================================================================
 */

 GlmsBool
compareStrings(const char *first, const char *second, int length)
{
   return (strncmp(first, second, length) == 0) ? GLMS_TRUE : GLMS_FALSE;
}

GlmsBool
compareMoKeys(const char *first, const char *second)
{
   return compareStrings(first, second, GLMS_MO_KEY_LEN);
}

GlmsBool
compareKeyIds(const char *first, const char *second)
{
   return compareStrings(first, second, GLMS_KEY_ID_LEN);
}


/* verifyMoStringValidity verifies that subscribe data is of
   the correct format. If the data is empty string or has an
   ascii value of less than 32 or greater than 126 it
   is considered faulty data.
*/
GlmsBool
verifyMoStringValidity(const char *moId)
{
   const char *c = moId;

   if(*c == '\0')
      return GLMS_FALSE;

   while(*c != '\0')
   {
      if(*c < 32 || *c > 126)
         return GLMS_FALSE;

      c++;
   }

   return GLMS_TRUE;
}

GlmsBool
isSignalResultOk(GlmsResult result)
{
   return (result == GLMS_OK);
}

int32_t
strInStr(const char *needle, const char *haystack)
{
   int i = 0;

   while((haystack = strstr(haystack, needle)) != NULL)
   {
      i++;

      /* Move haystack pointer so that we don't find the same needle again */
      haystack++;
   }

   return i;
}

/* getNextParameter will take a string in strIn argument and look for the first
   occurence of the needle string. If the needle is found then that character in
   the string is set to NULL and the start of the string is returned.

   Consecutive calls to getNextParameter can be called with strIn set to NULL.
   In that case the parsing will continue from the end of the last returned
   string.
*/
char *
getNextParameter(const char *needle, char *strIn)
{
   static char  *src = NULL;
   char *p, *strStart;

   if(strIn != NULL)
   {
      src = strIn;
   }

   if(src == NULL)
   {
      return NULL;
   }

   strStart = src;

   /* The token is found */
   if((p = strstr(src, needle)) != NULL)
   {
      *p  = '\0';
      src = p + strlen(needle);
   }

   return strStart;
}


/* fillParameter takes an input string and turns any occurence of
   ';' in that string into ';+' so that the ; will not interfere with
   the ';:' that is used to delimit parameters that are stored as persistent
   and software parameters. 
   The resulting string is then concatenated to the input buffer along with
   the delimiter parameter ";:".*/
void
fillParameter(char *buf, const char *str, GlmsBool last)
{
   static uint32_t posInBuf = 0;
   uint32_t i;
   for (i = 0; i < strlen(str); i++, posInBuf++)
   {
      buf[posInBuf] = str[i];
      if (buf[posInBuf] == ';')
      {
         ++posInBuf;
         buf[posInBuf] = '+';
      }
   }

   if (last)
   {
      buf[posInBuf] = '\0';
      posInBuf = 0;
   }
   else
   {
      buf[posInBuf++] = ';';
      buf[posInBuf++] = ':';
   }
}

/* cleanupEscapeCharacters takes an input string and turns any occurence
   of ";+" into just a ";". The input string will be modified. */
void
cleanupEscapeCharacters(char *str)
{
   size_t i, j;

   if(!strstr(str, ";+"))
   {
      /* String does not contain any escape characters */
      return;
   }

   for(i = 0, j = 0; i < strlen(str); i++, j++)
   {
      str[j] = str[i];
      
      if(str[i] == ';' && str[i+1] == '+')
      {
         i++;
      }
   }

   str[j] = '\0';
}


GlmsBool
validLkDates(time_t32 startTime, time_t32 stopTime)
{
   struct timespec currentTime;
   
   clock_gettime(CLOCK_REALTIME, &currentTime);

   return ((currentTime.tv_sec >= startTime) &&
           (currentTime.tv_sec <= stopTime || stopTime == GLMS_NO_STOP_DATE));
}


void
glms_logEvent(GlmsLogLevel logLevel, const char *fmt, ...)
{
   union itc_msg *sig;
   va_list args;
   uint32_t logLength;
   char logEntry[1024];

   /* send logs if GLMS is activated or is initiating (in other words,
      it is not deactivated) */
   if(adapter_getMid() != 0 &&
      adapter_getLogLevel() >= logLevel &&
      adapter_getActivationState() != ADPI_DEACTIVATED)
   {
      va_start(args, fmt);
      logLength = vsnprintf(logEntry, sizeof(logEntry), fmt, args);
      va_end(args);

      sig = itc_alloc(sizeof(GlmsAdpiLogInd) + logLength,
                  GLMS_ADPI_LOG_IND);
      strcpy(sig->glmsLogInd.log, logEntry);
      sendMsgToAdapter(sig);
   }
}

uint32_t
glms_increment_uint32(uint32_t u32_variable)
{
  if( u32_variable ==  UINT_MAX)
      u32_variable = 0;
  
   return (++u32_variable);
}


GlmsBool
noStopDate(time_t32 time)
{
   struct tm *time_tm;
   time_t timeTmp = (time_t) time;

   time_tm = gmtime(&timeTmp);

   if(time_tm->tm_year == 8099)
   {
      return GLMS_TRUE;
   }
   
   return GLMS_FALSE;
}


/*
 * The below functions shall be used when interacting over the 
 * LIHI interfaces (LFCI and LCCI). For reading incomming signals
 * use the rd32, rd16 functions. For setting outgoing variables
 * use the wr32, wr16 functions.
 *
 **/

uint32_t
rd32(uint32_t in)
{
#ifdef GLMS_DISABLE_FORCED_BIG_ENDIAN
   return in;
#else
   return be32toh(in);
#endif
}


uint16_t
rd16(uint16_t in)
{
#ifdef GLMS_DISABLE_FORCED_BIG_ENDIAN
   return in;
#else
   return be16toh(in);
#endif
}


uint32_t
wr32(uint32_t in)
{
#ifdef GLMS_DISABLE_FORCED_BIG_ENDIAN
   return in;
#else
   return htobe32(in);
#endif
}


uint16_t
wr16(uint16_t in)
{
#ifdef GLMS_DISABLE_FORCED_BIG_ENDIAN
   return in;
#else
   return htobe16(in);
#endif
}
