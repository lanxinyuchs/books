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

#ifndef _SAFC_IMM_LIB_H
#define _SAFC_IMM_LIB_H

#include <pthread.h>
#include <saAis.h>
#include <saImm.h>
#include <saImmOm.h>
#include <saImmOm_A_2_safc.h>
#include "sa_ais.pb-c.h"
#include "om.pb-c.h"

#define IMM_RELEASE_CODE 'A'
#define IMM_MAJOR_VERSION 0x02
#define IMM_MINOR_VERSION 0x0d

/*
 * Macros for lock handling.
 */
#define ZERO(expr) assert(0 == (expr))
#define LOCK_INIT(lock) ZERO(pthread_rwlock_init(lock, NULL))
#define LOCK_RDLOCK(lock) ZERO(pthread_rwlock_rdlock(lock))
#define LOCK_WRLOCK(lock) ZERO(pthread_rwlock_wrlock(lock))
#define LOCK_UNLOCK(lock) ZERO(pthread_rwlock_unlock(lock))
#define LOCK_DESTROY(lock) ZERO(pthread_rwlock_destroy(lock))

#define MUTEX_INIT(mutex) ZERO(pthread_mutex_init(mutex, NULL))
#define MUTEX_LOCK(mutex) ZERO(pthread_mutex_lock(mutex))
#define MUTEX_UNLOCK(mutex) ZERO(pthread_mutex_unlock(mutex))
#define MUTEX_DESTROY(mutex) ZERO(pthread_mutex_destroy(mutex))

extern SaAisErrorT safc_imm_version_validate(SaVersionT *version);
extern void safc_free_admop_params(int no_params, SafsImmAdminOperationParams2 **params);
extern void safc_copy_from_imm_attributes(const SaImmAttrValuesT_2* from_attr, SafsImmAttrValues2 **to_attr);
extern void safc_copy_to_imm_attributes(const SafsImmAttrValues2 *from_attr, SaImmAttrValuesT_2* to_attr);
extern void safc_copy_from_imm_attr_value(SafsImmAttrValue *aval, const SaImmValueTypeT attrValueType,
					  const SaImmAttrValueT attrValue);
extern SaImmAttrValueT safc_copy_to_imm_attr_value(const SaImmValueTypeT attrValueType,
						   SafsImmAttrValue *aval);
extern void safc_free_imm_attributes(SaImmAttrValuesT_2** attributes);
extern void safc_free_imm_objects(SaImmSearchObjectsT_s2** objects);
extern void safc_free_imm_attr_value(const SaImmValueTypeT attrValueType, SaImmAttrValueT attrValue);
extern void safc_free_safs_attr_values_2(SafsImmAttrValues2 *attr);

#endif
