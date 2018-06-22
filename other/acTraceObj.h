/*
 ******************************************************************************
 ************************* Copyright ERICSSON AB 2016 *************************
 ******************************************************************************
 * The Copyright to the computer programs herein is the property of ERICSSON AB
 *
 * The programs may be used and/or copied only with the written permission from
 * ERICSSON AB or in accordance with the terms and conditions stipulated in
 * the agreement/contract under which the programs have been supplied.
 ******************************************************************************
 */

/* >  1  DESCRIPTION
 ******************************************************************************
 *
 * <text...>
 *
 * @(#) ClearCase ID: ac.h /main/..  25 Jul 2016 EYEZHEN
 *
 * Revision history
 * 2016-7-25           EDINGZH           First Revision
 *
 ******************************************************************************
 */

#ifndef __AC_TRACE_OBJ_H__
#define __AC_TRACE_OBJ_H__

/*******   XPP header  *******/
//#include "xpai_xte_if_trace.h"
//#include "xpai_xte_if_ose.h"
#include <itc.h>
#include <string.h>
#include "acSignal.h"

#define MAX_ANTENNA_NUM 128   //can be replaced by the global Macro
#define MAX_FREQUENCE_NUM 3   //can be replaced by the global Macro
#define MAX_ACTRACE_COLS 10
#define MAX_ACTRACE_DATA_LEN 495
#define ACTRACE_UP     0
#define ACTRACE_DOWN   1

//AC Trace Type
typedef enum
{
   ACTRACE_DATA_DISABLE,
   ACTRACE_DATA_UP_ENABLE,
   ACTRACE_DATA_DOWN_ENABLE,
   ACTRACE_DATA_BOTH_ENABLE
}ACTRACE_TYPE;

//AC Trace Test Cmd Type
typedef enum
{
   ENABLE_INFO,
   DISABLE_INFO,
   ENABLE_DATA_PER_STREAM,
   DISABLE_DATA_PER_STREAM,
   DISABLE_DATA_PER_ANNT,
   DISABLE_DATA_ALL,
   DISPLAY_TRACE_CFG,
   MAX_ACTRACE_CMD
}ACTRACE_CMD;

typedef struct {
   U16 paraNum;
   U16 stringNum;
   const char* cmdString[5];
} AcTestCmd;


class AcTraceObj
{
public:
   AcTraceObj(void);
   ~AcTraceObj(void);

   bool isAcTraceInfoEnable() { return isInfoEnable;}
   bool isAcTraceDataEnable(U16 antId, U16 freq, U16 receiveTransmit); 
   void convertTraceToAscii(S16 *adrs, U32 arraySize, char *traceMsg);
   inline void enableAcTraceInfo() { isInfoEnable=true; }
   inline void disableAcTraceInfo() { isInfoEnable=false; }
   void enableAcTraceData(U16 antId, U16 freq, ACTRACE_TYPE acTraceType);
   void disableAcTraceData(U16 antId, U16 freq, ACTRACE_TYPE acTraceType);
   void disableAcTraceData(U16 antId);
   inline void disableAcTraceData(void) { memset(acTraceConfig, 0, MAX_ANTENNA_NUM*MAX_FREQUENCE_NUM*sizeof(char)); }
   void displayAcTraceConfig(void);

   //for test cmd
   int testCmdHandler(int argc, char **argv);
   void defaultHelp();

private:   
   char acTraceConfig[MAX_ANTENNA_NUM][MAX_FREQUENCE_NUM];
   bool isInfoEnable;

   //for test cmd
   AcTestCmd acTraceCmd[MAX_ACTRACE_CMD];
   bool matchCmd(int argc, char **argv, U16* cmdNum);
};

static AcTraceObj AcTraceObjInst;

#define TRACE_AC_INFO(MSG)\
{\
  if (AcTraceObjInst.isAcTraceInfoEnable())\
    TRACE(1, MSG);\
}\


#define TRACE_AC_DATA(ANTID, FREQ, RXORTX, DATAARRAY, ARRAYSIZE)\
do \
{\
   if (AcTraceObjInst.isAcTraceDataEnable(ANTID, FREQ, RXORTX))\
   {\
      int size = 0;\
      int copySize = 0;\
      int index = 0;\
      int bufSize = 256;\
      char msgBuf[256];\
      char* newMsg=(char*)alloc(12*ARRAYSIZE, 0);\
      size = sprintf(newMsg,"AC_TRACE_DATA: antenna=%d, frequence=%d, receiveTransmit=%d:\n",\
           ANTID, FREQ, RXORTX);\
      AcTraceObjInst.convertTraceToAscii(DATAARRAY, ARRAYSIZE, &newMsg[size]);\
      do\
      {\
         copySize = snprintf((char*)msgBuf, (bufSize-1),"%s", (char*)&newMsg[index]);\
         TRACE(1, msgBuf); \
         index += (copySize > (bufSize - 1))? (bufSize-1): copySize;\
      }while(copySize > (bufSize-1));\
      FREE_BUF((union itc_msg **)( (void *)&newMsg ) );  \
   }\
} while(0)

#endif /*AC_TRACE_OBJ_H*/



