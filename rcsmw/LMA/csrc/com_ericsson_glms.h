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

#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_glms

#if !defined(_TRACEPOINT_COM_ERICSSON_GLMS_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define _TRACEPOINT_COM_ERICSSON_GLMS_H

#ifdef __cplusplus
extern "C" {
#endif


#include <stdint.h>
#include <lttng/tracepoint.h>
#include "glmsadpi/glmsDataTypes.h"


TRACEPOINT_EVENT(com_ericsson_glms, glms_startup,
        TP_ARGS(char *, startup),
	TP_FIELDS(ctf_string(startup, startup))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, glms_startup, TRACE_INFO)


TRACEPOINT_EVENT(com_ericsson_glms, call_to_function,
        TP_ARGS(char *, functionName),
        TP_FIELDS(ctf_string(functionName, functionName))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, call_to_function, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, call_to_function_w_int_arg,
        TP_ARGS(char *, functionName, int, arg1),
        TP_FIELDS(ctf_string(functionName, functionName)
                  ctf_integer(int, arg1, arg1))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, call_to_function_w_int_arg, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, call_to_function_w_str_arg,
        TP_ARGS(char *, functionName, char *, arg1),
        TP_FIELDS(ctf_string(functionName, functionName)
                  ctf_string(arg1, arg1))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, call_to_function_w_str_arg, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, call_to_function_w_hex_arg,
        TP_ARGS(char *, functionName, int, arg1),
        TP_FIELDS(ctf_string(functionName, functionName)
                  ctf_integer_hex(int, arg1, arg1))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, call_to_function_w_hex_arg, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, debug_trace,
                 TP_ARGS(char *, trace),
                 TP_FIELDS(ctf_string(trace, trace))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, debug_trace, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, debug_trace_w_str_arg,
                 TP_ARGS(char *, trace, char *, arg1),
                 TP_FIELDS(ctf_string(trace, trace)
                           ctf_string(arg1, arg1))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, debug_trace_w_str_arg, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, debug_trace_w_int_arg,
                 TP_ARGS(char *, trace, int, arg1),
                 TP_FIELDS(ctf_string(trace, trace)
                           ctf_integer(int, arg1, arg1))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, debug_trace_w_int_arg, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, debug_trace_w_hex_arg,
                 TP_ARGS(char *, trace, int, arg1),
                 TP_FIELDS(ctf_string(trace, trace)
                           ctf_integer_hex(int, arg1, arg1))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, debug_trace_w_hex_arg, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, info_trace,
                 TP_ARGS(char *, trace),
                 TP_FIELDS(ctf_string(trace, trace))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, info_trace, TRACE_INFO)


TRACEPOINT_EVENT(com_ericsson_glms, info_trace_w_int_arg,
                 TP_ARGS(char *, trace, int, arg1),
                 TP_FIELDS(ctf_string(trace, trace)
                           ctf_integer(int, arg1, arg1))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, info_trace_w_int_arg, TRACE_INFO)


TRACEPOINT_EVENT(com_ericsson_glms, info_trace_w_str_arg,
                 TP_ARGS(char *, trace, char *, str),
                 TP_FIELDS(ctf_string(trace, trace)
                           ctf_string(str, str))
                 )
TRACEPOINT_LOGLEVEL(com_ericsson_glms, info_trace_w_str_arg, TRACE_INFO)


TRACEPOINT_EVENT(com_ericsson_glms, error_trace,
                 TP_ARGS(char *, trace),
                 TP_FIELDS(ctf_string(trace, trace))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, error_trace, TRACE_ERR)


TRACEPOINT_EVENT(com_ericsson_glms, error_trace_w_int_arg,
                 TP_ARGS(char *, trace, int, arg1),
                 TP_FIELDS(ctf_string(trace, trace)
                           ctf_integer(int, arg1, arg1))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, error_trace_w_int_arg, TRACE_ERR)


TRACEPOINT_EVENT(com_ericsson_glms, error_trace_w_str_arg,
                 TP_ARGS(char *, trace, char *, arg1),
                 TP_FIELDS(ctf_string(trace, trace)
                           ctf_string(arg1, arg1))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, error_trace_w_string_arg, TRACE_ERR)


TRACEPOINT_EVENT(com_ericsson_glms, error_trace_w_hex_arg,
                 TP_ARGS(char *, trace, uint32_t, arg1),
                 TP_FIELDS(ctf_string(trace, trace)
                           ctf_integer_hex(uint32_t, arg1, arg1))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, error_trace_w_hex_arg, TRACE_ERR)


TRACEPOINT_EVENT(com_ericsson_glms, parameter_set_rsp,
        TP_ARGS(char *, table, int, result, int, requestId),
        TP_FIELDS(ctf_string(table, table)
                  ctf_integer(int, result, result)
                  ctf_integer(int, requestId, requestId))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, parameter_set_rsp, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, parameter_delete_index_rsp,
        TP_ARGS(char *, parameter, int, result, int, requestId),
        TP_FIELDS(ctf_string(parameter, parameter)
                  ctf_integer(int, result, result)
                  ctf_integer(int, requestId, requestId))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, parameter_delete_index_rsp, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, lihi_client_created,
	TP_ARGS(uint32_t, mailboxId),
        TP_FIELDS(ctf_integer_hex(uint32_t, mailboxId, mailboxId))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, glms_client_created, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, pp_set,
        TP_ARGS(uint32_t, requestId, char *, table, uint32_t, index, char *, value),
        TP_FIELDS(ctf_integer(uint32_t, requestId, requestId)
                  ctf_string(table, table)
                  ctf_integer(uint32_t, index, index)
                  ctf_string(value, value))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, pp_set, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, sp_set,
        TP_ARGS(uint32_t, requestId, char *, table, uint32_t, index, char *, value),
        TP_FIELDS(ctf_integer(uint32_t, requestId, requestId)
                  ctf_string(table, table)
                  ctf_integer(uint32_t, index, index)
                  ctf_string(value, value))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, sp_set, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, pp_deleteIndex,
        TP_ARGS(uint32_t, requestId, char *, table, uint32_t, index),
        TP_FIELDS(ctf_integer(uint32_t, requestId, requestId)
                  ctf_string(table, table)
                  ctf_integer(uint32_t, index, index))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, pp_deleteIndex, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, sp_deleteIndex,
        TP_ARGS(uint32_t, requestId, char *, table, uint32_t, index),
        TP_FIELDS(ctf_integer(uint32_t, requestId, requestId)
                  ctf_string(table, table)
                  ctf_integer(uint32_t, index, index))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, sp_deleteIndex, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, disconnect_client,
        TP_ARGS(char *, trace, 
                uint32_t, mailboxId, 
                int32_t, clientRef, 
                int32_t, serverRef, 
                char *, errorInfo),
        TP_FIELDS(ctf_string(trace, trace)
                  ctf_integer(uint32_t, mailboxId, mailboxId)
                  ctf_integer(int32_t, clientRef, clientRef)
                  ctf_integer(int32_t, serverRef, serverRef)
                  ctf_string(errorInfo, errorInfo))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, disconnect_client, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, handleLfciFeatureLicenseUnsubscribeReq,
        TP_ARGS(uint32_t, mailboxId, 
                int32_t, clientRef, 
                int32_t, serverRef),
        TP_FIELDS(ctf_integer(uint32_t, mailboxId, mailboxId)
                  ctf_integer(int32_t, clientRef, clientRef)
                  ctf_integer(int32_t, serverRef, serverRef))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, handleLfciFeatureLicenseUnsubscribeReq, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, handleLfciFeatureLicenseSubscribeReq,
        TP_ARGS(uint32_t, mailboxId, 
                int32_t, clientRef, 
                int32_t, serverRef),
        TP_FIELDS(ctf_integer(uint32_t, mailboxId, mailboxId)
                  ctf_integer(int32_t, clientRef, clientRef)
                  ctf_integer(int32_t, serverRef, serverRef))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, handleLfciFeatureLicenseSubscribeReq, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_glms, handleLcciCapacityLicenseGpActivated,
        TP_ARGS(uint32_t, mailboxId, 
                int32_t, clientRef, 
                int32_t, serverRef),
        TP_FIELDS(ctf_integer(uint32_t, mailboxId, mailboxId)
                  ctf_integer(int32_t, clientRef, clientRef)
                  ctf_integer(int32_t, serverRef, serverRef))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, handleLcciCapacityLicenseGpActivated, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_glms, handleLcciCapacityLicenseUnsubscribeReq,
        TP_ARGS(uint32_t, mailboxId, 
                int32_t, clientRef, 
                int32_t, serverRef),
        TP_FIELDS(ctf_integer(uint32_t, mailboxId, mailboxId)
                  ctf_integer(int32_t, clientRef, clientRef)
                  ctf_integer(int32_t, serverRef, serverRef))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, handleLcciCapacityLicenseUnsubscribeReq, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, handleLcciCapacityLicenseSubscribeReq,
        TP_ARGS(uint32_t, mailboxId, 
                int32_t, clientRef, 
                int32_t, serverRef),
        TP_FIELDS(ctf_integer(uint32_t, mailboxId, mailboxId)
                  ctf_integer(int32_t, clientRef, clientRef)
                  ctf_integer(int32_t, serverRef, serverRef))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, handleLcciCapacityLicenseSubscribeReq, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_glms, requestTimer,
        TP_ARGS(uint32_t, timerLength, 
                int32_t,  perodic, 
                uint32_t, clientId,
                uint32_t, timerId),
        TP_FIELDS(ctf_integer(uint32_t, timerLength, timerLength)
                  ctf_integer(int32_t,  perodic, perodic)
                  ctf_integer(uint32_t, clientId, clientId)
                  ctf_integer(uint32_t, timerId, timerId))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, requestTimer, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, featureKey_createKeyWithoutMoIndication,
        TP_ARGS(char *, keyId,
                char *, featureKeyId,
                uint32_t, validFrom,
                uint32_t, expiration,
                uint32_t, tableIndex),
        TP_FIELDS(ctf_string(keyId, keyId)
                  ctf_string(featureKeyId, featureKeyId)
                  ctf_integer(uint32_t, validFrom, validFrom)
                  ctf_integer(uint32_t, expiration, expiration)
                  ctf_integer(uint32_t, tableIndex, tableIndex))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, featureKey_createKeyWithoutMoIndication, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, featureState_createFeatureStateWithoutMoIndication,
        TP_ARGS(char *, featureStateId,
                char *, description,
                char *, keyId,
                char *, name,
                char *, productType,
                uint32_t, featureState,
                uint32_t, licenseState,
                uint32_t, markedForDeletion,
                uint32_t, tableIndex,
                GlmsBool, performLicenseValidation),
        TP_FIELDS(ctf_string(featureStateId, featureStateId)
                  ctf_string(description, description)
                  ctf_string(keyId, keyId)
                  ctf_string(name, name)
                  ctf_string(productType, productType)
                  ctf_integer(uint32_t, featureState, featureState)
                  ctf_integer(uint32_t, licenseState, licenseState)
                  ctf_integer(uint32_t, markedForDeletion, markedForDeletion)
                  ctf_integer(uint32_t, tableIndex, tableIndex)
                  ctf_integer(GlmsBool, performLicenseValidation, performLicenseValidation))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, featureState_createFeatureStateWithoutMoIndication, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, featureState_addSubscriber,
        TP_ARGS(char *, keyId,
                char *, name,
                char *, description,
                uint32_t, mailboxId,
                int32_t,  serverRef,
                uint32_t, clientRef),
        TP_FIELDS(ctf_string(keyId, keyId)
                  ctf_string(name, name)
                  ctf_string(description, description)
                  ctf_integer(uint32_t, mailboxId, mailboxId)
                  ctf_integer(int32_t,  serverRef, serverRef)
                  ctf_integer(uint32_t, clientRef, clientRef))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, featureState_addSubscriber, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_glms, capacityKey_createKeyWithoutMoIndication,
        TP_ARGS(char *, keyId,
                char *, capacityKeyId,
                uint32_t, validFrom,
                uint32_t, expiration,
                uint32_t, tableIndex,
                uint32_t, noLimit,
                int32_t, value,
                uint32_t, notContractuallyLimited),
        TP_FIELDS(ctf_string(keyId, keyId)
                  ctf_string(capacityKeyId, capacityKeyId)
                  ctf_integer(uint32_t, validFrom, validFrom)
                  ctf_integer(uint32_t, expiration, expiration)
                  ctf_integer(uint32_t, tableIndex, tableIndex)
                  ctf_integer(uint32_t, noLimit, noLimit)
                  ctf_integer(int32_t, value, value)
                  ctf_integer(uint32_t, notContractuallyLimited, notContractuallyLimited))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, capacityKey_createKeyWithoutMoIndication, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_glms, capacityState_createCapacityStateWithoutMoIndication,
        TP_ARGS(char *, capacityStateId,
                char *, description,
                char *, keyId,
                char *, name,
		char *, capacityUnit,
                char *, productType,
                uint32_t, licenseState,
                uint32_t, markedForDeletion,
                uint32_t, tableIndex,
                GlmsBool, performLicenseValidation),
        TP_FIELDS(ctf_string(capacityStateId, capacityStateId)
                  ctf_string(description, description)
                  ctf_string(keyId, keyId)
                  ctf_string(name, name)
		  ctf_string(capacityUnit, capacityUnit)
                  ctf_string(productType, productType)
                  ctf_integer(uint32_t, licenseState, licenseState)
                  ctf_integer(uint32_t, markedForDeletion, markedForDeletion)
                  ctf_integer(uint32_t, tableIndex, tableIndex)
                  ctf_integer(GlmsBool, performLicenseValidation, performLicenseValidation))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, capacityState_createCapacityStateWithoutMoIndication, TRACE_DEBUG)

TRACEPOINT_EVENT(com_ericsson_glms, capacityState_createCapacityStateWithoutMoIndication_part2,
        TP_ARGS(GlmsBool, currentCapacityValue_noLimit,
                int32_t,  currentCapacityValue_value,
                uint16_t, isGracePeriodControlled,
                uint32_t, gracePeriodActivationValue,
                uint32_t, gracePeriod_gracePeriodExpiration,
                int32_t,  gracePeriod_gpConfiguredLength,
                int32_t,  gracePeriod_gpConfiguredResetThreshold,
                int32_t,  gracePeriod_gpConfiguredActivationThreshold,
                uint32_t, gracePeriod_gracePeriodState,
                char *,   gracePeriod_gracePeriodId),
        TP_FIELDS(ctf_integer(GlmsBool, currentCapacityValue_noLimit, currentCapacityValue_noLimit)
                  ctf_integer(int32_t,  currentCapacityValue_value, currentCapacityValue_value)
                  ctf_integer(uint16_t, isGracePeriodControlled, isGracePeriodControlled)
                  ctf_integer(uint32_t, gracePeriodActivationValue, gracePeriodActivationValue)
                  ctf_integer(uint32_t, gracePeriod_gracePeriodExpiration, gracePeriod_gracePeriodExpiration)
                  ctf_integer(int32_t,  gracePeriod_gpConfiguredLength, gracePeriod_gpConfiguredLength)
                  ctf_integer(int32_t,  gracePeriod_gpConfiguredResetThreshold, gracePeriod_gpConfiguredResetThreshold)
                  ctf_integer(int32_t,  gracePeriod_gpConfiguredActivationThreshold, gracePeriod_gpConfiguredActivationThreshold)
                  ctf_integer(uint32_t, gracePeriod_gracePeriodState, gracePeriod_gracePeriodState)
                  ctf_string(gracePeriod_gracePeriodId, gracePeriod_gracePeriodId))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, capacityState_createCapacityStateWithoutMoIndication_part2, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, capacityState_addSubscriber,
        TP_ARGS(char *, keyId,
                char *, name,
		char *, capacityUnit,
                char *, description,
                uint32_t, mailboxId,
                int32_t,  serverRef,
                uint32_t, clientRef),
        TP_FIELDS(ctf_string(keyId, keyId)
                  ctf_string(name, name)
		  ctf_string(capacityUnit, capacityUnit)
                  ctf_string(description, description)
                  ctf_integer(uint32_t, mailboxId, mailboxId)
                  ctf_integer(int32_t,  serverRef, serverRef)
                  ctf_integer(uint32_t, clientRef, clientRef))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, capacityState_addSubscriber, TRACE_DEBUG)


TRACEPOINT_EVENT(com_ericsson_glms, capacity_value_updated,
        TP_ARGS(char *, keyId,
                uint32_t, noLimit,
                int32_t,  value),
        TP_FIELDS(ctf_string(keyId, keyId)
                  ctf_integer(uint32_t, noLimit, noLimit)
                  ctf_integer(int32_t,  value, value))
)
TRACEPOINT_LOGLEVEL(com_ericsson_glms, capacity_value_updated, TRACE_DEBUG)


#endif /* _TRACEPOINT_COM_ERICSSON_GLMS_H */

#undef TRACEPOINT_INCLUDE_FILE
#define TRACEPOINT_INCLUDE_FILE ./com_ericsson_glms.h

/* This part must be outside ifdef protection */
#include <lttng/tracepoint-event.h>

#ifdef __cplusplus 
}
#endif
