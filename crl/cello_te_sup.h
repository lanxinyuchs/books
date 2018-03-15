/**
 *   The contents of this file declares the Trace & Error Handler
 *   supervisions interface. The interface gives the opportunity
 *   to subscribe, and unsubscribe, to notifications whenever
 *   an error event has been logged in the Trace & Error Log.
 *   This interface can be used to implement software error
 *   alarms as well as improved tests of fault cases during
*   black box tests.
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
 *   Revised : 2010-09-23 Anette Schött
 *   Change  : Updated according to template and added ifdef for Linux.
 * ========================================================================
 */

/* ===================================================================== */
#ifndef LNX    /* OSE implementation */
/* ===================================================================== */

#ifndef CELLO_TE_SUP_H
#define CELLO_TE_SUP_H
 
/* ========================================================================
 *   INCLUDE FILES
 * ========================================================================
 */

#include <osetypes.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================
 *   DEFINITIONS
 * ========================================================================
 */

/* ========================================================================
 *   TYPE DEFINITIONS
 * ========================================================================
 */ 

/**
 * The supervision indication signal structure sent back
 * with the signal number specified by the subscription
 * function OMCSF_teSupervisionSubscribe.
 */
struct OMCSF_teSupervisionIndS
{
  SIGSELECT sigNo;
};


/* ========================================================================
 *   DATA DECLARATIONS
 * ========================================================================
 */

/**
 * Public function: OMCSF_teSupervisionSubscribe()
 * Description:
 *   Used for subscribing for supervision indications whenever
 *   an error event has been logged in the Trace & Error Log.
 *   The structure of the indication signal that is sent back
 *   with the specified signal number is defined by the struct
 *   OMCSF_teSupervisionIndS.
 *
 * Pre-conditions:
 *   None.
 *
 * Post-conditions:
 *   A supervision indication with the specified signal number
 *   will be sent to the caller of this function whenever an
 *   error event gets logged to the Trace & Error Log.
 *
 * Returns:
 *   Returns True if the Trace & Error Log process was found
 *   and if it was possible to subscribe to supervision indications.
 *   Otherwise False is returned.
 *
 */
extern Boolean
OMCSF_teSupervisionSubscribe(
  SIGSELECT supervisionSigNo);  /* I: Signal no for supervision indication */



/**
 * Public function: OMCSF_teSupervisionUnsubscribe()
 * Description:
 *   Used for unsubscribing to supervision indications.
 *
 * Pre-conditions:
 *   None.
 *
 * Post-conditions:
 *   No more supervision indications will be sent to the
 *   caller of this function whenever an error event gets
 *   logged to the Trace & Error Log.
 *
 * Returns:
 *   Returns True if the Trace & Error Log process was found
 *   and if it was possible to unsubscribe to supervision
 *   indications. Otherwise False is returned.
 *
 */
extern Boolean
OMCSF_teSupervisionUnsubscribe(
  void);


#ifdef __cplusplus
}
#endif

#endif /* CELLO_TE_SUP_H */

/* ===================================================================== */
#endif    /* OSE implementation */
/* ===================================================================== */
