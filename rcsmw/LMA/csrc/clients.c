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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#include "com_ericsson_glms.h"

#include "glmsadpi/glmsDataTypes.h"
#include "glmsadpi/glms_adpi.sig"
#include "lihi/licenseFeatureControlI.sig"
#include "lihi/licenseCapacityControlI.sig"
#include "glms_main.h"
#include "clients.h"
#include "data_featureState.h"
#include "data_featureKey.h"
#include "data_capacityState.h"
#include "data_capacityKey.h"


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

   GlmsAdpiDumpLfciClientDataReq   glmsDumpLfciClientDataReq;
   GlmsAdpiDumpLfciClientDataRsp   glmsDumpLfciClientDataRsp;
   GlmsAdpiDumpLcciClientDataReq   glmsDumpLcciClientDataReq;
   GlmsAdpiDumpLcciClientDataRsp   glmsDumpLcciClientDataRsp;
    
   #include "lihi/licenseFeatureControlIUnionContent.h"
   #include "lihi/licenseCapacityControlIUnionContent.h"
};

/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

LihiClientRoot clientRoot;
uint32_t       lfciServerRefPool;
uint32_t       lcciServerRefPool;


/* ========================================================================
 *   FUNCTION PROTOTYPES
 * ========================================================================
 */

void client_deleteAndUnmonitorAllClients();

/* ========================================================================
 *   FUNCTIONS
 * ========================================================================
 */
void
lihiClientInit()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "lihiClientInit");

   clientRoot.firstLihiClient  = NULL;
   clientRoot.lastLihiClient   = NULL;
   lfciServerRefPool = 1;
   lcciServerRefPool = 1;
}


/* lihiClientClear will delete all lfci and lcci clients
   and send disconnect indications to each client */
void
lihiClientClear()
{
   tracepoint(com_ericsson_glms, call_to_function,
              "lihiClientClear");

   client_deleteAndUnmonitorAllClients();
   lihiClientInit(); /* Reset all data */
}


GlmsBool
client_isLastClient(LihiClient *lihiClient)
{
   if(lihiClient == NULL)
   {
      return GLMS_TRUE;
   }
   else if(lihiClient == clientRoot.lastLihiClient)
   {
      return GLMS_TRUE;
   }
   else
   {
      return GLMS_FALSE;
   }
}


LihiClient
*client_getFirst()
{
   return clientRoot.firstLihiClient;
}


LihiClient
*client_getLast()
{
   return clientRoot.lastLihiClient;
}


LihiClient
*client_getNext(LihiClient *lihiClient)
{
   if(lihiClient == NULL)
   {
      return NULL;
   }
   else
   {
      return lihiClient->nextLihiClient;
   }
}


LihiClient
*client_getPrevious(LihiClient *lihiClient)
{
   if(lihiClient == NULL)
   {
      return NULL;
   }
   else
   {
      return lihiClient->prevLihiClient;
   }
}


LihiClient
*client_findByMid(itc_mbox_id_t clientMid)
{
   LihiClient *client = client_getFirst();

   while(client != NULL)
   {
      if(client_getMid(client) == clientMid)
      {
         return client;
      }
      else
      {
         client = client_getNext(client);
      }
   }

   return client;
}


void
client_deleteAndUnmonitorAllClients()
{
   LihiClient *client, *delClient;

   tracepoint(com_ericsson_glms, call_to_function,
              "client_deleteAndUnmonitorAllClients");

   for(client = client_getFirst(); client != NULL; )
   {
      delClient = client;
      client = client_getNext(client);
      client_delete(delClient, GLMS_TRUE);
   }
}


void
client_delete(LihiClient *lihiClient, GlmsBool unmonitorClient)
{
   tracepoint(com_ericsson_glms, call_to_function_w_int_arg,
              "client_delete", unmonitorClient);

   if(lihiClient == NULL)
   {
      return;
   }

   if(unmonitorClient)
   {
      itc_unmonitor(lihiClient->monitorId);
      lihiClient->monitorId = 0;
   }

   if(lihiClient == clientRoot.firstLihiClient &&
      lihiClient == clientRoot.lastLihiClient)
   {
      clientRoot.firstLihiClient = NULL;
      clientRoot.lastLihiClient  = NULL;
   }
   else if(lihiClient == clientRoot.firstLihiClient)
   {
      clientRoot.firstLihiClient = lihiClient->nextLihiClient;
   }
   else if(lihiClient == clientRoot.lastLihiClient)
   {
      clientRoot.lastLihiClient = lihiClient->prevLihiClient;
   }

   if(lihiClient->nextLihiClient != NULL)
   {
      (lihiClient->nextLihiClient)->prevLihiClient = lihiClient->prevLihiClient;
   }
   if(lihiClient->prevLihiClient != NULL)
   {
      (lihiClient->prevLihiClient)->nextLihiClient = lihiClient->nextLihiClient;
   }

   free(lihiClient);
}


LihiClient
*client_create(itc_mbox_id_t clientMid)
{
   LihiClient *lihiClient;

   tracepoint(com_ericsson_glms, call_to_function_w_hex_arg,
              "client_create", clientMid);

   lihiClient = (LihiClient *) malloc(sizeof(LihiClient));

   lihiClient->clientMid      = clientMid;
   lihiClient->monitorId      = itc_monitor(clientMid, NULL);
   lihiClient->lfciClientList = NULL;
   lihiClient->lcciClientList = NULL;
   lihiClient->nextLihiClient = NULL;

   if(clientRoot.lastLihiClient == NULL)
   {
      clientRoot.lastLihiClient   = lihiClient;
      clientRoot.firstLihiClient  = lihiClient;
      lihiClient->prevLihiClient  = NULL;
   }
   else
   {
      lihiClient->prevLihiClient = clientRoot.lastLihiClient;
      clientRoot.lastLihiClient->nextLihiClient = lihiClient;
      clientRoot.lastLihiClient = lihiClient;
   }

   tracepoint(com_ericsson_glms, lihi_client_created, lihiClient->clientMid);
   return lihiClient;
}


itc_mbox_id_t
client_getMid(LihiClient *lihiClient)
{
   if(lihiClient == NULL)
   {
      return 0;
   }
   else
   {
      return lihiClient->clientMid;
   }
}


LfciClient
*lfci_create(itc_mbox_id_t clientMid, uint32_t lfciPv, int32_t clientServerRef)
{
   LfciClient *lfciClient, *lfciC_i;
   LihiClient *lihiClient;

   tracepoint(com_ericsson_glms, call_to_function_w_hex_arg,
              "lfci_create", clientMid);

   lihiClient = client_findByMid(clientMid);
   if(lihiClient == NULL)
   {
      lihiClient = client_create(clientMid);
   }


   lfciClient = (LfciClient *) malloc(sizeof(LfciClient));

   lfciClient->lfciPv          = lfciPv;
   lfciClient->serverRef       = lfciServerRefPool++;
   lfciClient->clientServerRef = clientServerRef;
   lfciClient->nextLfciClient  = NULL;


   if(lihiClient->lfciClientList == NULL)
   {
      lihiClient->lfciClientList = lfciClient;
   }
   else
   {
      lfciC_i = lihiClient->lfciClientList;
      while(lfciC_i->nextLfciClient != NULL)
      {
         lfciC_i = lfciC_i->nextLfciClient;
      }
      lfciC_i->nextLfciClient = lfciClient;
   }

   tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
              "lfci client created with PV", lfciPv);

   return lfciClient;
}


LfciClient
*lfci_getFirst(LihiClient *lihiClient)
{
   if(lihiClient != NULL)
   {
      return lihiClient->lfciClientList;
   }

   return NULL;
}


LfciClient
*lfci_getNext(LfciClient *lfciClient)
{
   if(lfciClient != NULL)
   {
      return lfciClient->nextLfciClient;
   }

   return NULL;
}

/* lfci_deleteByMidAndServerRef will delete the data structures connected with
   the lfci client with given mid and serverRef.

   Arguments:
   mid            - MBOXID of the client that shall be removed.
   serverRef      - The serverRef of the client that shall be removed.
                    Note that a serverRef = 0 will remove all clients
                    with the given MBOXID.
   unmonitorMid   - If true then unmonitor the client process if no remaining clients
                    exist from the MBOXID.
*/
void
lfci_deleteByMidAndServerRef(itc_mbox_id_t mid,
                             int32_t serverRef,
                             GlmsBool unmonitorMid)
{
   LfciClient *lfciClient, *prevLfciC, *nextLfciC;
   LihiClient *lihiClient;

   tracepoint(com_ericsson_glms, call_to_function_w_hex_arg,
              "lfci_deleteByMidAndServerRef", mid);
   tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
              "deleting client with serverRef", serverRef);


   featureState_clearSubscriptionsByMidAndServerRef(mid, serverRef);
   lihiClient = client_getFirst();

   while(lihiClient != NULL)
   {
      if(client_getMid(lihiClient) != mid)
      {
         lihiClient = client_getNext(lihiClient);
         continue;
      }

      break;
   }

   if(lihiClient == NULL)
   {
      return;
   }

   prevLfciC  = NULL;
   lfciClient = lihiClient->lfciClientList;

   while(lfciClient != NULL)
   {
      if(serverRef == 0 ||
         lfci_getServerRef(lfciClient) == serverRef)
      {
         if(prevLfciC != NULL)
         {
            prevLfciC->nextLfciClient = lfciClient->nextLfciClient;
         }
         else if(prevLfciC == NULL)
         {
            lihiClient->lfciClientList = lfciClient->nextLfciClient;
         }

         nextLfciC = lfciClient->nextLfciClient;
         free(lfciClient);
         lfciClient = nextLfciC;
      }
      else
      {
         prevLfciC  = lfciClient;
         lfciClient = lfciClient->nextLfciClient;
      }
   }

   /* If there are no more lfci or lcci clients from the
      process then delete the lihi client structure and
      unmonitor the process. */
   if(lihiClient->lfciClientList == NULL && lihiClient->lcciClientList == NULL)
   {
      tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
                 "detatching from client", unmonitorMid);
      client_delete(lihiClient, unmonitorMid);
   }

   return;
}


LfciClient
*lfci_findByMidAndServerRef(itc_mbox_id_t mid, int32_t serverRef)
{
   LfciClient *lfciClient = NULL;
   LihiClient *lihiClient = NULL;

   lihiClient = client_getFirst();

   while(lihiClient != NULL)
   {
      if(client_getMid(lihiClient) != mid)
      {
         lihiClient = client_getNext(lihiClient);
         continue;
      }


      lfciClient = lihiClient->lfciClientList;
      while(lfciClient != NULL)
      {
         if(lfci_getServerRef(lfciClient) == serverRef)
         {
            return lfciClient;
         }
         else
         {
            lfciClient = lfciClient->nextLfciClient;
         }
      }
      break;
   }

   return NULL;
}


uint32_t
lfci_getPv(LfciClient *lfciClient)
{
   if(lfciClient != NULL)
   {
      return lfciClient->lfciPv;
   }
   else
   {
      return 0;
   }
}


int32_t
lfci_getServerRef(LfciClient *lfciClient)
{
   if(lfciClient != NULL)
   {
      return lfciClient->serverRef;
   }
   else
   {
      return 0;
   }
}

int32_t
lfci_getClientServerRef(LfciClient *lfciClient)
{
   if(lfciClient != NULL)
   {
      return lfciClient->clientServerRef;
   }
   else
   {
      return 0;
   }
}

void
handleGlmsAdpiDumpLfciClientDataReq(union itc_msg *sigRec)
{
   union itc_msg *sigSend;
   uint32_t dataBufferSizeIter = 0;
   uint32_t dataBufferSize     = 0;
   uint32_t currentBufferUsed  = 0;
   char *buf;
   GlmsBool dumpDone = GLMS_FALSE;
   char dumpStr[GLMS_MAX_DUMPSTR_LEN];
   uint32_t dumpStrLen;

   LihiClient *lihiClient = client_getFirst();
   LfciClient *lfciClient = NULL;

   dataBufferSize = GlmsDumpSize[dataBufferSizeIter];
   buf = malloc(dataBufferSize);
   buf[0] = '\0';

   while(dumpDone == GLMS_FALSE)
   {
      /* Get dumpStr */
      if(lihiClient == NULL)
      {
         /* All LFCI data have been written to 'buf'. */
         break;
      }
      else if(lfciClient == NULL)
      {
         dumpStrLen = snprintf(dumpStr, GLMS_MAX_DUMPSTR_LEN,
                               "LIHI client with MailboxId 0x%08x:\n",
                               client_getMid(lihiClient));

         /* Get the first LFCI client of the LIHI client. */
         lfciClient = lfci_getFirst(lihiClient);
      }
      else
      {
         /* Print the LFCI client data to dumpStr */
         dumpStrLen = snprintf(dumpStr, GLMS_MAX_DUMPSTR_LEN,
                               " - LFCI: PV %d, ServerRef %d, ClientServerRef %d\n",
                               lfci_getPv(lfciClient),
                               lfci_getServerRef(lfciClient),
			       lfci_getClientServerRef(lfciClient));

         /* Get the next LFCI client of the LIHI client. */
         lfciClient = lfci_getNext(lfciClient);
      }

      if(lfciClient == NULL)
      {
         /* If no more LFCI clients remain on this LIHI client then
            get the next LIHI client */
         lihiClient = client_getNext(lihiClient);
      }

      if(dumpStrLen >= GLMS_MAX_DUMPSTR_LEN)
      {
         tracepoint(com_ericsson_glms, error_trace,
                    "Dump string was truncated in call"
                    " to handleGlmsAdpiDumpLfciClientDataReq");
         dumpStrLen = GLMS_MAX_DUMPSTR_LEN-1;
      }


      /* If the total dump size will be greater than the maximum allowed
         dump size, then send the already existing dump text and start
         a new dump string. */
      if((currentBufferUsed + dumpStrLen) >
         GlmsDumpSize[GLMS_LAST_DUMP_SIZE_ELEMENT])
      {
         sigSend = itc_alloc(sizeof(GlmsAdpiDumpLfciClientDataRsp) +
                         GlmsDumpSize[GLMS_LAST_DUMP_SIZE_ELEMENT],
                         GLMS_ADPI_DUMP_LFCI_CLIENT_DATA_RSP);
         sigSend->glmsDumpLfciClientDataRsp.result       = GLMS_OK;
         strncpy(sigSend->glmsDumpLfciClientDataRsp.resultInfo,
                 "",
                 GLMS_RESULT_INFO_LEN);
         sigSend->glmsDumpLfciClientDataRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
         sigSend->glmsDumpLfciClientDataRsp.lastResponse = 0;
         sigSend->glmsDumpLfciClientDataRsp.sizeOfDump = currentBufferUsed + 1;
         // codechecker_suppress [unknown] Size argument is greater than the length of the destination buffer
         // ('dump' attribute has size 1 in signal definition)
         strncpy(sigSend->glmsDumpLfciClientDataRsp.dump,
                 buf,
                 GlmsDumpSize[GLMS_LAST_DUMP_SIZE_ELEMENT]);
         sigSend->glmsDumpLfciClientDataRsp.
            dump[GlmsDumpSize[GLMS_LAST_DUMP_SIZE_ELEMENT]] = '\0';
         itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);

         /* Reset the buffer to write a new string. We keep the prviously
            malloced buffer to store the new dump string. */
         buf[0] = '\0';
         currentBufferUsed = 0;
      }
      /* If the total dump size will be greater than the currently allowed
         dump size, then reallocate the dump memory to allow it to be larger. */
      else if((currentBufferUsed + dumpStrLen) >
              GlmsDumpSize[dataBufferSizeIter])
      {
         dataBufferSizeIter++;
         buf = realloc(buf, GlmsDumpSize[dataBufferSizeIter]);
      }


      /* We now have enough space in 'buf' to add the new text from 'dumpStr' */
      strcat(buf, dumpStr);
      currentBufferUsed += dumpStrLen;
   }

   /* 'buf' now contains the last part of the dump. Send it to the adapter
      and free the allocated memory. */
   sigSend = itc_alloc(sizeof(GlmsAdpiDumpLfciClientDataRsp) + currentBufferUsed,
                   GLMS_ADPI_DUMP_LFCI_CLIENT_DATA_RSP);
   sigSend->glmsDumpLfciClientDataRsp.result       = GLMS_OK;
   strncpy(sigSend->glmsDumpLfciClientDataRsp.resultInfo,
           "",
           GLMS_RESULT_INFO_LEN);
   sigSend->glmsDumpLfciClientDataRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   sigSend->glmsDumpLfciClientDataRsp.lastResponse = 1;
   sigSend->glmsDumpLfciClientDataRsp.sizeOfDump = currentBufferUsed + 1;
   // codechecker_suppress [unknown] Size argument is greater than the length of the destination buffer
   // ('dump' attribute has size 1 in signal definition)
   strncpy(sigSend->glmsDumpLfciClientDataRsp.dump,
           buf,
           currentBufferUsed);
   sigSend->glmsDumpLfciClientDataRsp.dump[currentBufferUsed] = '\0';
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);


   free(buf);
}

/* LCCI Implementation */
LcciClient
*lcci_create(itc_mbox_id_t clientMid, uint32_t lcciPv, int32_t clientServerRef)
{
   LcciClient *lcciClient, *lcciC_i;
   LihiClient *lihiClient;

   tracepoint(com_ericsson_glms, call_to_function_w_hex_arg,
              "lcci_create", clientMid);

   lihiClient = client_findByMid(clientMid);
   if(lihiClient == NULL)
   {
      lihiClient = client_create(clientMid);
   }


   lcciClient = (LcciClient *) malloc(sizeof(LcciClient));

   lcciClient->lcciPv          = lcciPv;
   lcciClient->serverRef       = lcciServerRefPool++;
   lcciClient->clientServerRef = clientServerRef;
   lcciClient->isGpControlled  = 0;
   lcciClient->nextLcciClient  = NULL;


   if(lihiClient->lcciClientList == NULL)
   {
      lihiClient->lcciClientList = lcciClient;
   }
   else
   {
      lcciC_i = lihiClient->lcciClientList;
      while(lcciC_i->nextLcciClient != NULL)
      {
         lcciC_i = lcciC_i->nextLcciClient;
      }
      lcciC_i->nextLcciClient = lcciClient;
   }

   tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
              "lcci client created with PV", lcciPv);

   return lcciClient;
}

LcciClient
*lcci_getFirst(LihiClient *lihiClient)
{
   if(lihiClient != NULL)
   {
      return lihiClient->lcciClientList;
   }

   return NULL;
}


LcciClient
*lcci_getNext(LcciClient *lcciClient)
{
   if(lcciClient != NULL)
   {
      return lcciClient->nextLcciClient;
   }

   return NULL;
}

/* lcci_deleteByMidAndServerRef will delete the data structures connected with
   the lcci client with given mid and serverRef.

   Arguments:
   mid            - MBOXID of the client that shall be removed.
   serverRef      - The serverRef of the client that shall be removed.
                    Note that a serverRef = 0 will remove all clients
                    with the given MBOXID.
   unmonitorMid   - If true then unmonitor client process if no remaining clients
                    exist from the MBOXID.
*/
void
lcci_deleteByMidAndServerRef(itc_mbox_id_t mid,
                             int32_t serverRef,
                             GlmsBool unmonitorMid)
{
   LcciClient *lcciClient, *prevLcciC, *nextLcciC;
   LihiClient *lihiClient;

   tracepoint(com_ericsson_glms, call_to_function_w_hex_arg,
              "lcci_deleteByMidAndServerRef", mid);
   tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
              "deleting client with serverRef", serverRef);

   capacityState_clearSubscriptionsByMidAndServerRef(mid, serverRef);
   lihiClient = client_getFirst();

   while(lihiClient != NULL)
   {
      if(client_getMid(lihiClient) != mid)
      {
         lihiClient = client_getNext(lihiClient);
         continue;
      }

      break;
   }

   if(lihiClient == NULL)
   {
      return;
   }

   prevLcciC  = NULL;
   lcciClient = lihiClient->lcciClientList;

   while(lcciClient != NULL)
   {
      if(serverRef == 0 ||
         lcci_getServerRef(lcciClient) == serverRef)
      {
         if(prevLcciC != NULL)
         {
            prevLcciC->nextLcciClient = lcciClient->nextLcciClient;
         }
         else if(prevLcciC == NULL)
         {
            lihiClient->lcciClientList = lcciClient->nextLcciClient;
         }

         nextLcciC = lcciClient->nextLcciClient;
         free(lcciClient);
         lcciClient = nextLcciC;
      }
      else
      {
         prevLcciC  = lcciClient;
         lcciClient = lcciClient->nextLcciClient;
      }
   }

   /* If there are no more lfci or lcci clients from the
      process then delete the lihi client structure and
      unmonitor the process. */
   if(lihiClient->lfciClientList == NULL && lihiClient->lcciClientList == NULL)
   {
      tracepoint(com_ericsson_glms, debug_trace_w_int_arg,
                 "detatching from client", unmonitorMid);
      client_delete(lihiClient, unmonitorMid);
   }
}


LcciClient
*lcci_findByMidAndServerRef(itc_mbox_id_t mid, int32_t serverRef)
{
   LcciClient *lcciClient = NULL;
   LihiClient *lihiClient = NULL;

   lihiClient = client_getFirst();

   while(lihiClient != NULL)
   {
      if(client_getMid(lihiClient) != mid)
      {
         lihiClient = client_getNext(lihiClient);
         continue;
      }


      lcciClient = lihiClient->lcciClientList;
      while(lcciClient != NULL)
      {
         if(lcci_getServerRef(lcciClient) == serverRef)
         {
            return lcciClient;
         }
         else
         {
            lcciClient = lcciClient->nextLcciClient;
         }
      }
      break;
   }

   return NULL;
}


uint32_t
lcci_getPv(LcciClient *lcciClient)
{
   if(lcciClient != NULL)
   {
      return lcciClient->lcciPv;
   }
   else
   {
      return 0;
   }
}


int32_t
lcci_getServerRef(LcciClient *lcciClient)
{
   if(lcciClient != NULL)
   {
      return lcciClient->serverRef;
   }
   else
   {
      return 0;
   }
}

int32_t
lcci_getClientServerRef(LcciClient *lcciClient)
{
   if(lcciClient != NULL)
   {
      return lcciClient->clientServerRef;
   }
   else
   {
      return 0;
   }
}


void
handleGlmsAdpiDumpLcciClientDataReq(union itc_msg *sigRec)
{
   union itc_msg *sigSend;
   uint32_t dataBufferSizeIter = 0;
   uint32_t dataBufferSize     = 0;
   uint32_t currentBufferUsed  = 0;
   char *buf;
   GlmsBool dumpDone = GLMS_FALSE;
   char dumpStr[GLMS_MAX_DUMPSTR_LEN];
   uint32_t dumpStrLen;

   LihiClient *lihiClient = client_getFirst();
   LcciClient *lcciClient = NULL;

   dataBufferSize = GlmsDumpSize[dataBufferSizeIter];
   buf = malloc(dataBufferSize);
   buf[0] = '\0';


   while(dumpDone == GLMS_FALSE)
   {
      /* Get dumpStr */
      if(lihiClient == NULL)
      {
         /* All LCCI data have been written to 'buf'. */
         break;
      }
      else if(lcciClient == NULL)
      {
         dumpStrLen = snprintf(dumpStr, GLMS_MAX_DUMPSTR_LEN,
                               "LIHI client with MailboxId 0x%08x:\n",
                               client_getMid(lihiClient));

         /* Get the first LCCI client of the LIHI client. */
         lcciClient = lcci_getFirst(lihiClient);
      }
      else
      {
         /* Print the LCCI client data to dumpStr */
         dumpStrLen = snprintf(dumpStr, GLMS_MAX_DUMPSTR_LEN,
                               " - LCCI: PV %d, ServerRef %d, ClientServerRef %d\n",
                               lcci_getPv(lcciClient),
                               lcci_getServerRef(lcciClient),
			       lcci_getClientServerRef(lcciClient));

         /* Get the next LCCI client of the LIHI client. */
         lcciClient = lcci_getNext(lcciClient);
      }

      if(lcciClient == NULL)
      {
         /* If no more LCCI clients remain on this LIHI client then
            get the next LIHI client */
         lihiClient = client_getNext(lihiClient);
      }

      if(dumpStrLen >= GLMS_MAX_DUMPSTR_LEN)
      {
         tracepoint(com_ericsson_glms, error_trace,
                    "Dump string was truncated in call"
                    " to handleGlmsAdpiDumpLcciClientDataReq");
         dumpStrLen = GLMS_MAX_DUMPSTR_LEN-1;
      }


      /* If the total dump size will be greater than the maximum allowed
         dump size, then send the already existing dump text and start
         a new dump string. */
      if((currentBufferUsed + dumpStrLen) >
         GlmsDumpSize[GLMS_LAST_DUMP_SIZE_ELEMENT])
      {
         sigSend = itc_alloc(sizeof(GlmsAdpiDumpLcciClientDataRsp) +
                         GlmsDumpSize[GLMS_LAST_DUMP_SIZE_ELEMENT],
                         GLMS_ADPI_DUMP_LCCI_CLIENT_DATA_RSP);
         sigSend->glmsDumpLcciClientDataRsp.result       = GLMS_OK;
         strncpy(sigSend->glmsDumpLcciClientDataRsp.resultInfo,
                 "",
                 GLMS_RESULT_INFO_LEN);
         sigSend->glmsDumpLcciClientDataRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
         sigSend->glmsDumpLcciClientDataRsp.lastResponse = 0;
         sigSend->glmsDumpLcciClientDataRsp.sizeOfDump = currentBufferUsed + 1;
         // codechecker_suppress [unknown] Size argument is greater than the length of the destination buffer
         // ('dump' attribute has size 1 in signal definition)
         strncpy(sigSend->glmsDumpLcciClientDataRsp.dump,
                 buf,
                 GlmsDumpSize[GLMS_LAST_DUMP_SIZE_ELEMENT]);
         sigSend->glmsDumpLcciClientDataRsp.
            dump[GlmsDumpSize[GLMS_LAST_DUMP_SIZE_ELEMENT]] = '\0';
         itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);

         /* Reset the buffer to write a new string. We keep the prviously
            malloced buffer to store the new dump string. */
         buf[0] = '\0';
         currentBufferUsed = 0;
      }
      /* If the total dump size will be greater than the currently allowed
         dump size, then reallocate the dump memory to allow it to be larger. */
      else if((currentBufferUsed + dumpStrLen) >
              GlmsDumpSize[dataBufferSizeIter])
      {
         dataBufferSizeIter++;
         buf = realloc(buf, GlmsDumpSize[dataBufferSizeIter]);
      }


      /* We now have enough space in 'buf' to add the new text from 'dumpStr' */
      strcat(buf, dumpStr);
      currentBufferUsed += dumpStrLen;
   }

   /* 'buf' now contains the last part of the dump. Send it to the adapter
      and free the allocated memory. */
   sigSend = itc_alloc(sizeof(GlmsAdpiDumpLcciClientDataRsp) + currentBufferUsed,
                   GLMS_ADPI_DUMP_LCCI_CLIENT_DATA_RSP);
   sigSend->glmsDumpLcciClientDataRsp.result       = GLMS_OK;
   strncpy(sigSend->glmsDumpLcciClientDataRsp.resultInfo,
           "",
           GLMS_RESULT_INFO_LEN);
   sigSend->glmsDumpLcciClientDataRsp.resultInfo[GLMS_RESULT_INFO_LEN - 1] = '\0';
   sigSend->glmsDumpLcciClientDataRsp.lastResponse = 1;
   sigSend->glmsDumpLcciClientDataRsp.sizeOfDump = currentBufferUsed + 1;
   // codechecker_suppress [unknown] Size argument is greater than the length of the destination buffer
   // ('dump' attribute has size 1 in signal definition)
   strncpy(sigSend->glmsDumpLcciClientDataRsp.dump,
           buf,
           currentBufferUsed);
   sigSend->glmsDumpLcciClientDataRsp.dump[currentBufferUsed] = '\0';
   itc_send(&sigSend, itc_sender(sigRec), ITC_MY_MBOX);


   free(buf);
}
