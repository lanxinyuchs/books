/**
 *   cello_cdefs.h header file
 * 
 *   @file
 *   @version @(#) ClearCase ID: 
 *
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
 *   Revised : 2013-05-14 Anette Schött
 *   Change  : Updated according to template and added ifdef for Linux.
 * ========================================================================
 */

/* ===================================================================== */
#ifndef LNX    /* OSE implementation */
/* ===================================================================== */

#ifndef CELLO_CDEFS_H
#define CELLO_CDEFS_H

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */ 

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */

/** 
 * Enum: OM_yesOrNoE
 * Description:
 *   Standard Yes or No type.
 *
 */
typedef enum OM_yesOrNoE
{
  OM_NO,
  OM_YES
} OM_yesOrNoE;


/**
 * Enum: OM_resultE
 * Description:
 *   Standard result type.
 *
 */
typedef enum OM_resultE
{
   OM_OK     = 0,
   OM_NOT_OK = 1
} OM_resultE;


/**
 * Enum: OM_stateE
 * Description:
 *   Standard state type.
 *
 */
typedef enum OM_stateE 
{
  OM_SETUP         = 1,
  OM_CONFIGURED    = 2,
  OM_STARTED       = 3,
  OM_CONNECTED     = 4,
  OM_DISCONNECTED  = 5,
  OM_RELEASED      = 6
} OM_stateE;


/**  
 * Enum: OM_subrackTypeE
 * Description:
 *   The type of subrack according to the configuration file.
 *   This enum is defined in the configuration file and used
 *   for the leaf BS.HW.SUBRACK(*).type.
 *
 */
typedef enum OM_subrackTypeE
{
  OM_SUBRACK_TX,
  OM_SUBRACK_RX,
  OM_SUBRACK_NO_TYPE
} OM_subrackTypeE;


/**
 * Enum: OM_piuTypeE
 * Description:
 *   The type of PIU according to the configuration file.
 *   This enum is defined in the configuration file and
 *   used for the leaf BS.HW.SUBRACK(*).PIU(*).type.
 *
 */
typedef enum OM_piuTypeE
{
  OM_PIU_ENC,
  OM_PIU_BBTX,
  OM_PIU_TRXDIG,
  OM_PIU_DEC,
  OM_PIU_BBRX,
  OM_PIU_BBRA,
  OM_PIU_BBIF,
  OM_PIU_NO_TYPE
} OM_piuTypeE;


/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

#ifdef __cplusplus
}
#endif

#endif /* CELLO_CDEFS_H */

/* ===================================================================== */
#endif   /* OSE implementation */
/* ===================================================================== */
