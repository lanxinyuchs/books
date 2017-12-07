/*
 * %EricssonCopyright%
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2015-2016. All Rights Reserved.
 *
 * The program may be used and/or copied only with the written permission from
 * Ericsson AB, or in accordance with the terms and conditions stipulated in
 * the agreement/contract under which the program has been supplied.
 *
 * %CopyrightEnd%
 *
 * ----------------------------------------------------------------------
 *  Purpose : SAF IMM OM SAFC additions
 * ----------------------------------------------------------------------
 *
 */

#ifndef _SA_IMM_OM_A_2_SAFC_H
#define _SA_IMM_OM_A_2_SAFC_H

#ifdef  __cplusplus
extern "C" {
#endif

        /* 4.2.12 SaImmSearchOptionsT */
	/*
#define SA_IMM_SEARCH_ONE_ATTR       0x0001
#define SA_IMM_SEARCH_GET_ALL_ATTR   0x0100
#define SA_IMM_SEARCH_GET_NO_ATTR    0x0200
#define SA_IMM_SEARCH_GET_SOME_ATTR  0x0400
#define SA_IMM_SEARCH_GET_CONFIG_ATTR 0x0000000000010000   
	*/
#define SA_IMM_SEARCH_GET_PERSISTENT_ATTR 0x0100000000000000   

   /* SaImmSearchObjectsT_s2 */
   typedef struct {
      SaNameT objectName;
      SaImmAttrValuesT_2 **attributes;
   } SaImmSearchObjectsT_s2;

   /* SaImmCsStructT */
   typedef struct {
      SaStringT structName; /* Name of the corresponding struct class definition */
      SaImmAttrValuesT_2 **structMembers;
   } SaImmCsStructT;

   /*
    *************************
    *                       *
    *   Om Function Calls   *
    *                       *
    *************************
    */

   /* saImmOmSearchClassInitialize_s2() */
   extern SaAisErrorT 
   saImmOmSearchClassInitialize_s2(SaImmHandleT immHandle,
				   const SaNameT *rootName,
				   SaImmScopeT scope,
				   SaImmSearchOptionsT searchOptions,
				   const SaStringT *classNames,
				   const SaImmAttrNameT *attributeNames,
				   SaImmSearchHandleT *searchHandle);

   /* saImmOmSearchNextN_s2() */
   extern SaAisErrorT
   saImmOmSearchNextN_s2(SaImmSearchHandleT searchHandle,
			 const SaUint32T requestedNumberOfObjects,
			 SaImmSearchObjectsT_s2 ***objects);

#ifdef  __cplusplus
}
#endif

#endif   /* _SA_IMM_OM_A_2_SAFC_H */
