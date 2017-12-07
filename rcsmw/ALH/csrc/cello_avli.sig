/*
 ****************************************************************************
 *
 * 
 * Copyright (c) Ericsson AB  2014-2016 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 *
 ****************************************************************************/


#ifndef CELLO_AVLI_SIG
#define CELLO_AVLI_SIG

#ifndef NO_LITS
#include "ose.h"
#include "osetypes.h"
#endif

#include "cello_avli.h"
#ifdef __cplusplus
extern "C" {
#endif

/* EXPORT */

/*
********* SIGNALS DEFINED IN PROTOCOL VERSION 1 **********
*/

/*******************************************************************************
 *
 * Signal Name: CELLO_AVLI_WRITE_CFM
 *
 * Descr      : This signal should received in the clients receive loop and be 
 *              forwarded to the proxy as parameter in a Cello_Avli_internal(sig_p)
 *              call.
 *

 ******************************************************************************/

#define CELLO_AVLI_WRITE_CFM (0x1055C) /* !-SIGNO(CelloAvliWriteCfm)-! */

typedef struct
{
   U32                sigNo;
   CelloAvliCmd       cmd;
   CelloAvliClientRef clientRef;
   Cello_AvliResult   result;
} CelloAvliWriteCfm;





/*******************************************************************************
 *
 * Signal Name: CELLO_AVLI_HUNT_NS_IND
 *
 * Descr      : This signal should received in the clients receive loop and be 
 *              forwarded to the proxy as parameter in a 
 *              Cello_Avli_internal(sig_p) call.
 *
 *
 ******************************************************************************/

#define CELLO_AVLI_HUNT_NS_IND (0x1055D) /* !-SIGNO(CelloAvliHuntNsInd)-! */

typedef struct
{
   U32 sigNo;
} CelloAvliHuntNsInd;


/*******************************************************************************
 *
 * Signal Name: CELLO_AVLI_SERVER_UP_IND
 *
 * Descr      : This signal should received in the clients receive loop and be 
 *              forwarded to the proxy as parameter in a 
 *              Cello_Avli_internal(sig_p) call.
 *
 *
 ******************************************************************************/

#define CELLO_AVLI_SERVER_UP_IND (0x1055E) /* !-SIGNO(CelloAvliServerUpInd)-! */

typedef struct
{
   U32 sigNo;
} CelloAvliServerUpInd;



/*******************************************************************************
 *
 * Signal Name: CELLO_AVLI_SERVER_DOWN_IND
 *
 * Descr      : This signal should received in the clients receive loop and be 
 *              forwarded to the proxy as parameter in a Cello_Avli_internal
 *              (sig_p) call.
 *
 *
 ******************************************************************************/

#define CELLO_AVLI_SERVER_DOWN_IND (0x1055F) /* !-SIGNO(CelloAvliServerDownInd)-! */

typedef struct
{
   U32 sigNo;
} CelloAvliServerDownInd;



/*******************************************************************************
 *
 * Signal Name: CELLO_AVLI_ATTACH_NS_IND
 *
 * Descr      : This signal should received in the clients receive loop and be 
 *              forwarded to the proxy as parameter in a 
 *              Cello_Avli_internal(sig_p) call.
 *
 *
 ******************************************************************************/

#define CELLO_AVLI_ATTACH_NS_IND (0x10560) /* !-SIGNO(CelloAvliAttachNsInd)-! */


typedef struct
{
  U32 sigNo;
} CelloAvliAttachNsInd;



/*******************************************************************************
 *
 * Signal Name: CELLO_AVLI_UNPUBLISH_IND
 *
 * Descr      : This signal should received in the clients receive loop and be 
 *              forwarded to the proxy as parameter in a 
 *              Cello_Avli_internal(sig_p) call.
 *
 *
 ******************************************************************************/

#define CELLO_AVLI_UNPUBLISH_IND (0x10561) /* !-SIGNO(CelloAvliUnpublishInd)-! */

typedef struct
{
   U32 sigNo;
} CelloAvliUnpublishInd;

/*
********* SIGNALS DEFINED IN PROTOCOL VERSION 2 **********
*/

/******************************************************************************
 *
 * Signal : CelloAvli2InitiateServiceCfm
 *
 * Descr  : Signal to client to confirm the initiation of the service.
 *          This signal should also be forwarded to the proxy as 
 *              parameter in a Cello_Avli_internal(sig_p) call.
 *
 * Data   : 
 *
 *****************************************************************************/
#define CELLO_AVLI2_INITIATE_SERVICE_CFM (0x10826) /*!- SIGNO(CelloAvli2InitiateServiceCfm) -!*/

typedef struct
{
   U32 sigNo;
   U32 signalRevision;
   U32 selectedPV;
} CelloAvli2InitiateServiceCfm;

/******************************************************************************
 *
 * Signal : CelloAvli2InitiateServiceRej
 *
 * Descr  : Signal to client indicating total reject of the service.
 *          This signal should also be forwarded to the proxy as 
 *              parameter in a Cello_Avli_internal(sig_p) call.
 *          
 * Data   : 
 *
 *****************************************************************************/
#define CELLO_AVLI2_INITIATE_SERVICE_REJ (0x10827) /*!- SIGNO(CelloAvli2InitiateServiceRej) -!*/

typedef struct
{
   U32 sigNo;
   U32 signalRevision;
   U32 highestPV; 
} CelloAvli2InitiateServiceRej;

/******************************************************************************
 *
 * Signal : CelloAvli2InitiateServiceSus
 *
 * Descr  : Signal to client to suspend the usage of the service.
 *          This signal should also be forwarded to the proxy as 
 *              parameter in a Cello_Avli_internal(sig_p) call.
 *          
 * Data   : 
 *
 *****************************************************************************/
#define CELLO_AVLI2_INITIATE_SERVICE_SUS (0x10828) /*!- SIGNO(CelloAvli2InitiateServiceSus) -!*/

typedef struct
{
   U32 sigNo;
   U32 signalRevision;
   U32 highestPV; 
} CelloAvli2InitiateServiceSus;


#ifdef __cplusplus
}

#endif

#endif

