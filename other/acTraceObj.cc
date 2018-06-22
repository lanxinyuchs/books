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

/*******C libary header*******/
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>


/*******   OS header   *******/
#include <shell.h>

#include "acTraceObj.h"




AcTraceObj::AcTraceObj()
{
   memset(acTraceConfig, 0, MAX_ANTENNA_NUM*MAX_FREQUENCE_NUM*sizeof(char));
   isInfoEnable = false;

   //test cmd init
   acTraceCmd[ENABLE_INFO].paraNum = 3;
   acTraceCmd[ENABLE_INFO].stringNum = 3;
   acTraceCmd[ENABLE_INFO].cmdString[0] = "trace";
   acTraceCmd[ENABLE_INFO].cmdString[1] = "en";
   acTraceCmd[ENABLE_INFO].cmdString[2] = "info";

   acTraceCmd[DISABLE_INFO].paraNum = 3;
   acTraceCmd[DISABLE_INFO].stringNum = 3;
   acTraceCmd[DISABLE_INFO].cmdString[0] = "trace";
   acTraceCmd[DISABLE_INFO].cmdString[1] = "dis";
   acTraceCmd[DISABLE_INFO].cmdString[2] = "info";

   acTraceCmd[ENABLE_DATA_PER_STREAM].paraNum = 6;
   acTraceCmd[ENABLE_DATA_PER_STREAM].stringNum = 3;
   acTraceCmd[ENABLE_DATA_PER_STREAM].cmdString[0] = "trace";
   acTraceCmd[ENABLE_DATA_PER_STREAM].cmdString[1] = "en";
   acTraceCmd[ENABLE_DATA_PER_STREAM].cmdString[2] = "data";

   acTraceCmd[DISABLE_DATA_PER_STREAM].paraNum = 6;
   acTraceCmd[DISABLE_DATA_PER_STREAM].stringNum = 3;
   acTraceCmd[DISABLE_DATA_PER_STREAM].cmdString[0] = "trace";
   acTraceCmd[DISABLE_DATA_PER_STREAM].cmdString[1] = "dis";
   acTraceCmd[DISABLE_DATA_PER_STREAM].cmdString[2] = "data";

   acTraceCmd[DISABLE_DATA_PER_ANNT].paraNum = 4;
   acTraceCmd[DISABLE_DATA_PER_ANNT].stringNum = 3;
   acTraceCmd[DISABLE_DATA_PER_ANNT].cmdString[0] = "trace";
   acTraceCmd[DISABLE_DATA_PER_ANNT].cmdString[1] = "dis";
   acTraceCmd[DISABLE_DATA_PER_ANNT].cmdString[2] = "data";

   acTraceCmd[DISABLE_DATA_ALL].paraNum = 3;
   acTraceCmd[DISABLE_DATA_ALL].stringNum = 3;
   acTraceCmd[DISABLE_DATA_ALL].cmdString[0] = "trace";
   acTraceCmd[DISABLE_DATA_ALL].cmdString[1] = "dis";
   acTraceCmd[DISABLE_DATA_ALL].cmdString[2] = "data";

   acTraceCmd[DISPLAY_TRACE_CFG].paraNum = 2;
   acTraceCmd[DISPLAY_TRACE_CFG].stringNum = 2;
   acTraceCmd[DISPLAY_TRACE_CFG].cmdString[0] = "trace";
   acTraceCmd[DISPLAY_TRACE_CFG].cmdString[1] = "disp";
}

AcTraceObj::~AcTraceObj()
{
   return;
}


bool AcTraceObj::isAcTraceDataEnable(U16 antId, U16 freq, U16 receiveTransmit)
{
   if ((antId>=MAX_ANTENNA_NUM) || (freq>=MAX_FREQUENCE_NUM))
      return false;
     
   if ((acTraceConfig[antId][freq] & (1<<receiveTransmit)))
   {
      return true;
   }
   
   return false;
}


void AcTraceObj::enableAcTraceData(U16 antId, U16 freq, ACTRACE_TYPE acTraceType)
{
   if ((antId>=MAX_ANTENNA_NUM) || (freq>=MAX_FREQUENCE_NUM) || (acTraceType>ACTRACE_DATA_BOTH_ENABLE))
   {
      return;
   }

   acTraceConfig[antId][freq] = acTraceType;
   return;
}

void AcTraceObj::disableAcTraceData(U16 antId, U16 freq, ACTRACE_TYPE acTraceType)
{
   if ((antId>=MAX_ANTENNA_NUM) || (freq>=MAX_FREQUENCE_NUM) || (acTraceType>ACTRACE_DATA_BOTH_ENABLE))
   {
      return;
   }

   if (acTraceConfig[antId][freq] == ACTRACE_DATA_BOTH_ENABLE)
   {
      acTraceConfig[antId][freq] = ACTRACE_DATA_BOTH_ENABLE - acTraceType;
   }
   else
   {
      acTraceConfig[antId][freq] = ACTRACE_DATA_DISABLE;
   }

   return;
}

void AcTraceObj::disableAcTraceData(U16 antId)
{
   if ( antId>=MAX_ANTENNA_NUM )
   {
      return;
   }

   for (int i=0; i < MAX_FREQUENCE_NUM; i++)
   {
      acTraceConfig[antId][i] = ACTRACE_DATA_DISABLE;
   }

   return;
}

void AcTraceObj::displayAcTraceConfig(void)
{
         printf("===========AC Trace Config==========");
         printf("AC Trace Info Config: %d\n", isAcTraceInfoEnable());
         printf("AC Trace Data Config:\n");
         for (int i=0; i <MAX_ANTENNA_NUM; i++)
         {
                  printf("AntennaId(%d):", i);
                  for (int j=0; j<MAX_FREQUENCE_NUM; j++)
                  {
         printf("Freq(%d):%d", j, acTraceConfig[i][j]);
      }
      printf(";\n");
   }
   printf("===========AC Trace Config==========");
}

void AcTraceObj::convertTraceToAscii(S16 *adrs, U32 arraySize, char *traceMsg)
{
   U32 size;
   U32 index;
   char *msgIndex = NULL;

   msgIndex = traceMsg;

   for (index = 0; index < arraySize; index++)
   {
     if (index % MAX_ACTRACE_COLS == 0)
     {
       /* First on new line */
       size = snprintf(msgIndex, MAX_ACTRACE_DATA_LEN, "%04d ", index);
       msgIndex += size;
     }

     size = snprintf(msgIndex, MAX_ACTRACE_DATA_LEN, " %-8d ", adrs[index]);
     msgIndex += size;

     if ((index + 1) % MAX_ACTRACE_COLS == 0 || index == arraySize - 1)
     {
       size = snprintf(msgIndex, MAX_ACTRACE_DATA_LEN, "\n");
       msgIndex += size;
     }
   }

   *msgIndex = '\0';

   return;
}

bool AcTraceObj::matchCmd(int argc, char **argv, U16* cmdNum)
{
         U16 matchedStrNum;

   for (U16 i=0;i <MAX_ACTRACE_CMD ; i++)
   {
          matchedStrNum = 0;

      if (argc == acTraceCmd[i].paraNum+1)
      {
         for (U16 j=0;j<acTraceCmd[i].stringNum; j++)
         {
            if(strcmp(acTraceCmd[i].cmdString[j], argv[i+1]) ==0)
            {
                 matchedStrNum++;
            }
         }

         if (matchedStrNum == acTraceCmd[i].stringNum)
         {
            *cmdNum = i;
            return true;
         }
      }
   }

   return false;
}

void AcTraceObj::defaultHelp()
{
   printf("AC Trace command include:\n");
   printf("trace <enable|disable> info\n");
   printf("trace <enable|disable> data <antennaId> <frequence> <0|1|2>     0:up, 1:down, 2:both\n");
   printf("trace disable data <antennaId>\n");
   printf("trace disable data\n");
}

int AcTraceObj::testCmdHandler(int argc, char **argv)
{
   U16 cmdNum =0;
   U16 antId = 0;
   U16 freq =0;
   U16 receiveTransmit = 0;

   if (!matchCmd(argc, argv, &cmdNum))
   {
      return RET_SUCCESS;
   }

   switch(cmdNum)
   {
      case ENABLE_INFO:
         enableAcTraceInfo();
         break;

      case DISABLE_INFO:
         disableAcTraceInfo();
         break;

      case ENABLE_DATA_PER_STREAM:
         antId = atoi(argv[4]);
         freq = atoi(argv[5]);
         receiveTransmit = atoi(argv[6]);
         enableAcTraceData(antId, freq, (ACTRACE_TYPE)(receiveTransmit+1));  //ACTRACE_TYPE = receiveTransmit+1
         break;

      case DISABLE_DATA_PER_STREAM:
         antId = atoi(argv[4]);
         freq = atoi(argv[5]);
         receiveTransmit = atoi(argv[6]);
         disableAcTraceData(antId, freq, (ACTRACE_TYPE)(receiveTransmit+1)); //ACTRACE_TYPE = receiveTransmit+1
         break;

      case DISABLE_DATA_PER_ANNT:
         antId = atoi(argv[4]);
         disableAcTraceData(antId);
         break;

      case DISABLE_DATA_ALL:
         disableAcTraceData();
         break;

      case DISPLAY_TRACE_CFG:
         displayAcTraceConfig();
         break;

      default:
         defaultHelp();
         break;
   }

   return RET_SUCCESS;
}

