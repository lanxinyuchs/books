/*****************************************************************************
 *
 *
 * Copyright (c) Ericsson AB  2015-2016 All rights reserved.
 *
 * The information in this document is the property of Ericsson. Except
 * as specifically authorized in writing by Ericsson,the receiver of this
 * document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties. Disclosure and disseminations to the
 * receiver's employees shall only be made on a strict need to know basis.
 *
 ****************************************************************************/
#ifndef ECOLI_COLI_SIG
#define ECOLI_COLI_SIG

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define COLI_SERVER_MBOX    "ecoli_coli"

typedef uint32_t ColiResult;
#define COLI_RESULT_OK              0
#define COLI_RESULT_NO_FRU_EXISTS   1
#define COLI_RESULT_OTHER_ERROR    99
#define COLI_RESULT_OK_CONTINUE   100

#define COLI_MAX_FRU_LENGTH       32
#define COLI_MAX_LDN_LENGTH      512
#define COLI_MAX_COMMAND_LENGTH  256
#define COLI_MAX_REPLY_LENGTH    65355



/******************************************************************************
 *
 * Message Name: COLI_INTERFACE_REGISTRATION
 *
 * Descr      : Message to server specifying the callback mailbox
 *
 * Data       : fruTypes  Specifies the FRU Types that are handled by this interface.
 *                        A NULL terminated string.
 *                        Each fruType is separated by a comma (,).
 *
 *****************************************************************************/
#define COLI_INTERFACE_REGISTRATION (0x0190001)
typedef struct
{
   uint32_t  msg_no;
   char      fruTypes[COLI_MAX_FRU_LENGTH];
} ColiInterfaceRegistration;


/******************************************************************************
 *
 * Message Name: COLI_ADD_FRU
 *
 * Descr      : Message to add new FRU
 *
 * Data       : fruType      Specifies the FRU Type. A null terminated string.
 *              ldn          Specifies the full LDN, including the FRU id,
 *                           for the added FRU. A null terminated string.
 *
 *****************************************************************************/
#define COLI_ADD_FRU (0x0190002)
typedef struct
{
   uint32_t  msg_no;
   char      fruType[COLI_MAX_FRU_LENGTH];
   char      ldn[COLI_MAX_LDN_LENGTH];
} ColiAddFru;


/******************************************************************************
 *
 * Message Name: COLI_DELETE_FRU
 *
 * Descr      : Message to delete a FRU
 *
 * Data       : fruType      Specifies the FRU Type. A null terminated string.
 *              ldn          Specifies the full LDN for the deleted FRU.
 *                           A null terminated string.    
 *****************************************************************************/
#define COLI_DELETE_FRU (0x0190003)
typedef struct
{
   uint32_t  msg_no;
   char      fruType[COLI_MAX_FRU_LENGTH];
   char      ldn[COLI_MAX_LDN_LENGTH];
} ColiDeleteFru;


/******************************************************************************
 *
 * Message Name: COLI_COMMAND_REQUEST
 *
 * Descr      : Request to execute a coli command
 *
 * Data       : commandId    An identity to be returned in the command reply
 *              command      The COLI command to be executed. 
 *                           A null terminated string.
 *              ldn          Specifies the full LDN for the FRUyes. 
 *                           A null terminated string.
 *
 *****************************************************************************/
#define COLI_COMMAND_REQUEST (0x0190004)
typedef struct
{
   uint32_t  msg_no;
   uint32_t  commandId;
   char      command[COLI_MAX_COMMAND_LENGTH];
   char      ldn[COLI_MAX_LDN_LENGTH];
} ColiCommandRequest;



/******************************************************************************
 *
 * Message Name: COLI_COMMAND_REPLY
 *
 * Descr      : Request to execute a coli command
 *
 * Data       : result       The result of the COLI command
 *              commandId    An identity received in the COLI command request
 *              reply        The reply of the command. A null terminated string.
 *                           The reply must not be longer than COLI_MAX_REPLY_LENGTH
 *
 *****************************************************************************/
#define COLI_COMMAND_REPLY (0x0190005)
typedef struct
{
   uint32_t         msg_no;
   ColiResult       result;
   uint32_t         commandId;
   char             reply[1];
} ColiCommandReply;


/******************************************************************************
 *
 * Message Name: COLI_HUNT
 *
 * Descr      : Reply on RCS mailbox hunt
 *
 * Data       : This is a reserved signal number for applications,
 *              not used by RCS.		
 *
 *****************************************************************************/
#define COLI_HUNT (0x0190006)


/******************************************************************************
 *
 * Message Name: COLI_ATTACH
 *
 * Descr      : Informs that the RCS mailbox has terminated
 *
 * Data       : This is a reserved signal number for applications,
 *              not used by RCS.		
 *
 *****************************************************************************/
#define COLI_ATTACH (0x0190007)




#ifdef __cplusplus
}
#endif

#endif /* ECOLI_COLI_SIG */
