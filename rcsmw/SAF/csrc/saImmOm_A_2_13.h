/*
 * Author(s): Ericsson AB
 */

/*
 * DESCRIPTION:
 *   This file provides the suggested additions to the C language binding for 
 *   the Service Availability(TM) Forum Information Model Management Service (IMM).
 *   It contains only the prototypes and type definitions that are part of this
 *   proposed addition. 
 *   These additions are currently NON STANDARD. But the intention is to get these
 *   additions approved formally by SAF in the future.
 *
 */


#ifndef _SA_IMM_OM_A_2_13_H
#define _SA_IMM_OM_A_2_13_H

#ifdef  __cplusplus
extern "C" {
#endif

   /*
    *************************
    *                       *
    *   Om Function Calls   *
    *                       *
    *************************
    */


   /* saImmOmCcbObjectRead() */
   extern SaAisErrorT
   saImmOmCcbObjectRead(SaImmCcbHandleT ccbHandle,
			const SaNameT *objectName,
			const SaImmAttrNameT *attributeNames, 
			SaImmAttrValuesT_2 ***attributes);
   
#ifdef  __cplusplus
}
#endif

#include <saImmOm_A_2_14.h>

#endif   /* _SA_IMM_OM_A_2_13_H */
