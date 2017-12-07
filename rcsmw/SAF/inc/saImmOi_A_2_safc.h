/*
 * %EricssonCopyright%
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2015-2015. All Rights Reserved.
 *
 * The program may be used and/or copied only with the written permission from
 * Ericsson AB, or in accordance with the terms and conditions stipulated in
 * the agreement/contract under which the program has been supplied.
 *
 * %CopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *  Purpose : SAF IMM OI SAFC additions
 * ----------------------------------------------------------------------
 *
 */

#ifndef _SA_IMM_OI_A_2_SAFC_H
#define _SA_IMM_OI_A_2_SAFC_H

#ifdef  __cplusplus
extern "C" {
#endif

   /*
    *************************
    *                       *
    *   Oi Function Calls   *
    *                       *
    *************************
    */

   /* saImmOiRtObjectUpdateDelayedNcValues_s2() */
SaAisErrorT saImmOiRtObjectUpdateDelayedNcValues_s2(SaImmOiHandleT immOiHandle,
						    const SaNameT *objectName,
						    const SaImmAttrNameT *attributeNames);

#ifdef  __cplusplus
}
#endif

#endif   /* _SA_IMM_OI_A_2_SAFC_H */
