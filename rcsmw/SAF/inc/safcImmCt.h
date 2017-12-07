/*
 * %EricssonCopyright%
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012. All Rights Reserved.
 *
 * The program may be used and/or copied only with the written permission from
 * Ericsson AB, or in accordance with the terms and conditions stipulated in
 * the agreement/contract under which the program has been supplied.
 *
 * %CopyrightEnd%
 *
 * ----------------------------------------------------------------------
 */
#ifndef _SAFC_IMM_CT_H
#define _SAFC_IMM_CT_H

#include <saAis.h>
#include <saImm.h>
#include <saImmOi.h>

#ifdef  __cplusplus
extern "C" {
#endif

   typedef SaUint64T SafcImmCtHandleT;

   typedef struct {
      SaUint32T version; 
      SaUint32T release; 
   } SafcImmCtSchemaVersionT;

   typedef struct {
      SaUint32T version; 
      SaUint32T release; 
      SaUint32T correction; 
   } SafcImmCtSchemaVersionT_2;
      
   typedef struct {
      SaNameT *parentName;
      SaImmAttrValuesT_2 **attrValues;
   } SafcImmCtInstanceT; 
   
   typedef struct {
      SaImmClassNameT className;
      SafcImmCtInstanceT **instances;
   } SafcImmCtInstanceGroupT;
   
   
   extern SaAisErrorT safcImmCtInitialize(SafcImmCtHandleT *handle);

   extern SaAisErrorT safcImmCtInitialize_2(SafcImmCtHandleT *handle, SaBoolT delayedStart);
   
   extern SaAisErrorT safcImmCtFinalize(SafcImmCtHandleT handle);
      
   extern SaAisErrorT safcImmCtReadSchemaVersion(SafcImmCtHandleT handle, 
						 SaStringT schemaName,
						 SafcImmCtSchemaVersionT *oldSchemaVersion,
						 SafcImmCtSchemaVersionT *newSchemaVersion);

   extern SaAisErrorT safcImmCtReadSchemaVersion_2(SafcImmCtHandleT handle, 
						   SaStringT schemaName,
						   SafcImmCtSchemaVersionT_2 *oldSchemaVersion,
						   SafcImmCtSchemaVersionT_2 *newSchemaVersion);
      
   extern SaAisErrorT safcImmCtFailUpgrade(SafcImmCtHandleT handle, SaStringT message);
      
   extern SaAisErrorT safcImmCtReadInstances(SafcImmCtHandleT handle, 
					     SaImmClassNameT *classNames,
					     SafcImmCtInstanceGroupT ***instanceGroups);

     extern SaAisErrorT safcImmCtReadInstances_2(SafcImmCtHandleT handle, 
						 SaImmClassNameT *classNames,
						 SafcImmCtInstanceGroupT ***instanceGroups); 
   
   extern SaAisErrorT safcImmCtWriteInstances(SafcImmCtHandleT handle, 
					      SafcImmCtInstanceGroupT **instanceGroups);
      
   extern SaAisErrorT safcImmCtCopyInstances(SafcImmCtHandleT handle, 
					     SaImmClassNameT *classNames);
   
   extern SaAisErrorT safcImmCtWaitForClasses(SafcImmCtHandleT handle, 
					      SaImmClassNameT *classNames);
   
   extern SaAisErrorT safcImmCtWriteRtInstances(SafcImmCtHandleT handle, 
					       SaImmOiImplementerNameT implementerName,
					       SafcImmCtInstanceGroupT **instanceGroups);
      
   extern SaAisErrorT safcImmCtCopyRtInstances(SafcImmCtHandleT handle, 
					       SaImmOiImplementerNameT implementerName,
					       SaImmClassNameT *classNames);
   
   extern SaAisErrorT safcImmCtInstanceGroupsMemoryFree(SafcImmCtHandleT handle, 
							SafcImmCtInstanceGroupT **instanceGroups);
   
#ifdef  __cplusplus
}
#endif

#endif   /* _SAFC_IMM_CT_H */
