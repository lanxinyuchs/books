%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
%%
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%%
%% %CopyrightEnd%
%%
%%----------------------------------------------------------------------
%% File: safs_com.hrl
%%
%% Description:
%%
%%
%%----------------------------------------------------------------------
-ifndef(safs_ntf_hrl).
-define(safs_ntf_hrl, true).

-include("ntf.hrl").
-include("safs_ais.hrl").

%%----------------------------------------------------------------------
%% 3.12.1.1        SaNtfHandleT
%%typedef SaUint64T SaNtfHandleT;
%% @type sa_ntf_handle() = sa_uint64().
%%   An opaque handle to the notification service.
-type sa_ntf_handle() :: sa_uint64().

%%----------------------------------------------------------------------
%% 3.12.1.4        SaNtfReadHandleT
%%typedef SaUint64T SaNtfReadHandleT;
-type sa_ntf_read_handle() :: sa_uint64().

%%----------------------------------------------------------------------
%% 3.12.1.2        SaNtfNotificationHandleT
%%typedef SaUint64T SaNtfNotificationHandleT;
-type sa_ntf_notification_handle() :: term().

%%----------------------------------------------------------------------
%% 3.12.1.3        SaNtfNotificationFilterHandleT
%%typedef SaUint64T SaNtfNotificationFilterHandleT;
-type sa_ntf_notification_filter_handle() :: term().

%%----------------------------------------------------------------------
%% 3.12.2.1        SaNtfCallbacksT
-type sa_ntf_callbacks() :: #safsNtfCallbacks{}.

%%----------------------------------------------------------------------
%% 3.12.4 SaNtfEventTypeT
-define(SA_NTF_NOTIFICATIONS_TYPE_MASK, 16#F000).

%%----------------------------------------------------------------------
%% Event types enum, these are only generic
%% types as defined by the X.73x standards

-define(SA_NTF_OBJECT_NOTIFICATIONS_START, sa_ntf_object_notifications_start).
-define(SA_NTF_OBJECT_CREATION,            sa_ntf_object_creation).
-define(SA_NTF_OBJECT_DELETION,            sa_ntf_object_deletion).

-define(SA_NTF_ATTRIBUTE_NOTIFICATIONS_START, sa_ntf_attribute_notifications_start).
-define(SA_NTF_ATTRIBUTE_ADDED,               sa_ntf_attribute_added).
-define(SA_NTF_ATTRIBUTE_REMOVED,             sa_ntf_attribute_removed).
-define(SA_NTF_ATTRIBUTE_CHANGED,             sa_ntf_attribute_changed).
-define(SA_NTF_ATTRIBUTE_RESET,               sa_ntf_attribute_reset).

-define(SA_NTF_STATE_CHANGE_NOTIFICATIONS_START, sa_ntf_state_change_notifications_start).
-define(SA_NTF_OBJECT_STATE_CHANGE,              sa_ntf_object_state_change).

-define(SA_NTF_ALARM_NOTIFICATIONS_START, sa_ntf_alarm_notifications_start).
-define(SA_NTF_ALARM_COMMUNICATION,       sa_ntf_alarm_communication).
-define(SA_NTF_ALARM_QOS,                 sa_ntf_alarm_qos).
-define(SA_NTF_ALARM_PROCESSING,          sa_ntf_alarm_processing).
-define(SA_NTF_ALARM_EQUIPMENT,           sa_ntf_alarm_equipment).
-define(SA_NTF_ALARM_ENVIRONMENT,         sa_ntf_alarm_environment).

-define(SA_NTF_SECURITY_ALARM_NOTIFICATIONS_START, sa_ntf_security_alarm_notifications_start).
-define(SA_NTF_INTEGRITY_VIOLATION,                sa_ntf_integrity_violation).
-define(SA_NTF_OPERATION_VIOLATION,                sa_ntf_operation_violation).
-define(SA_NTF_PHYSICAL_VIOLATION,                 sa_ntf_physical_violation).
-define(SA_NTF_SECURITY_SERVICE_VIOLATION,         sa_ntf_security_service_violation).
-define(SA_NTF_TIME_VIOLATION,                     sa_ntf_time_violation).

-type sa_ntf_event_type() :: ?SA_NTF_OBJECT_NOTIFICATIONS_START
			   | ?SA_NTF_OBJECT_CREATION
			   | ?SA_NTF_OBJECT_DELETION
			   | ?SA_NTF_ATTRIBUTE_NOTIFICATIONS_START
			   | ?SA_NTF_ATTRIBUTE_ADDED
			   | ?SA_NTF_ATTRIBUTE_REMOVED
			   | ?SA_NTF_ATTRIBUTE_CHANGED
			   | ?SA_NTF_ATTRIBUTE_RESET
			   | ?SA_NTF_STATE_CHANGE_NOTIFICATIONS_START
			   | ?SA_NTF_OBJECT_STATE_CHANGE
			   | ?SA_NTF_ALARM_NOTIFICATIONS_START
			   | ?SA_NTF_ALARM_COMMUNICATION
			   | ?SA_NTF_ALARM_PROCESSING
			   | ?SA_NTF_ALARM_EQUIPMENT
			   | ?SA_NTF_ALARM_ENVIRONMENT
			   | ?SA_NTF_SECURITY_ALARM_NOTIFICATIONS_START
			   | ?SA_NTF_INTEGRITY_VIOLATION
			   | ?SA_NTF_OPERATION_VIOLATION
			   | ?SA_NTF_PHYSICAL_VIOLATION
			   | ?SA_NTF_SECURITY_SERVICE_VIOLATION
			   | ?SA_NTF_TIME_VIOLATION.

%%----------------------------------------------------------------------
%% 3.12.5  Notification Object
%% Use SaNameT.

%%----------------------------------------------------------------------
%% 3.12.6  Notifying Object
%% Use SaNameT.

%%----------------------------------------------------------------------
%% 3.12.7 SaNtfClassIdT
%%
-type sa_ntf_class_id() :: #safsNtfClassId{}.

-define(SA_NTF_VENDOR_ID_SAF, 18568).

%%----------------------------------------------------------------------
%% 3.12.8 SaServicesT

%%----------------------------------------------------------------------
%% 3.12.9  SaNtfElementIdT
%%typedef SaUint16T SaNtfElementIdT;
-type sa_ntf_element_id() :: sa_uint16().

%%----------------------------------------------------------------------
%% 3.12.10  SaNtfIdentifierT
%%typedef SaUint64T SaNtfIdentifierT;
%% @type sa_ntf_identifier() = integer().
%%   An identifier
-type sa_ntf_identifier() :: sa_uint64().

-define(SA_NTF_IDENTIFIER_UNUSED, 0).

%%----------------------------------------------------------------------
%% 3.12.11 Event Time
%% Use SaTimeT.

%%----------------------------------------------------------------------
%% 3.12.12 SaNtfValueTypeT
-define(SA_NTF_VALUE_UINT8,     sa_ntf_value_uint8).
-define(SA_NTF_VALUE_INT8,      sa_ntf_value_int8).
-define(SA_NTF_VALUE_UINT16,    sa_ntf_value_uint16).
-define(SA_NTF_VALUE_INT16,     sa_ntf_value_int16).
-define(SA_NTF_VALUE_UINT32,    sa_ntf_value_uint32).
-define(SA_NTF_VALUE_INT32,     sa_ntf_value_int32).
-define(SA_NTF_VALUE_FLOAT,     sa_ntf_value_float).
-define(SA_NTF_VALUE_UINT64,    sa_ntf_value_uint64).
-define(SA_NTF_VALUE_INT64,     sa_ntf_value_int64).
-define(SA_NTF_VALUE_DOUBLE,    sa_ntf_value_double).
-define(SA_NTF_VALUE_LDAP_NAME, sa_ntf_value_ldap_name).
-define(SA_NTF_VALUE_STRING,    sa_ntf_value_string).
-define(SA_NTF_VALUE_IPADDRESS, sa_ntf_value_ipaddress).
-define(SA_NTF_VALUE_BINARY,    sa_ntf_value_binary).
-define(SA_NTF_VALUE_ARRAY,     sa_ntf_value_array).

-type sa_ntf_value_type() :: ?SA_NTF_VALUE_INT8
			   | ?SA_NTF_VALUE_UINT8
			   | ?SA_NTF_VALUE_INT16
			   | ?SA_NTF_VALUE_UINT16
			   | ?SA_NTF_VALUE_INT32
			   | ?SA_NTF_VALUE_UINT32
			   | ?SA_NTF_VALUE_INT64
			   | ?SA_NTF_VALUE_UINT64
			   | ?SA_NTF_VALUE_FLOAT
			   | ?SA_NTF_VALUE_DOUBLE
			   | ?SA_NTF_VALUE_STRING
			   | ?SA_NTF_VALUE_LDAP_NAME
			   | ?SA_NTF_VALUE_IPADDRESS
			   | ?SA_NTF_VALUE_BINARY
			   | ?SA_NTF_VALUE_ARRAY.

%%----------------------------------------------------------------------
%% 3.12.13 SaNtfValueT
%% (Union between a number of integers, floats and the following records)
%% The first few are fixed size data types


-type sa_ntf_ldap_name()    :: sa_string().
-type sa_ntf_ipaddress()    :: sa_string().
-type sa_ntf_binary()       :: binary().
-type sa_ntf_array()        :: [term()].


-type sa_ntf_value() ::    sa_int8()
			 | sa_uint8()
			 | sa_int16()
			 | sa_uint16()
			 | sa_int32()
			 | sa_uint32()
			 | sa_int64()
			 | sa_uint64()
			 | sa_float()
			 | sa_double()
			 | sa_ntf_ldap_name()
			 | sa_string()
			 | sa_ntf_ipaddress()
			 | sa_ntf_binary()
			 | sa_ntf_array().

%% This struct can represent variable length fields like
%% LDAP names, strings, IP addresses and binary data.
%% It may only be used in conjunction with the data type values
%% SA_NTF_VALUE_LDAP_NAME, SA_NTF_VALUE_STRING,
%% SA_NTF_VALUE_IPADDRESS and SA_NTF_VALUE_BINARY.
%% This field shall not be directly accessed.
%% To initialise this structure and to set a pointer to the real data
%% use saNtfPtrValAllocate(). The function saNtfPtrValGet() shall be used
%% for retrieval of the real data.
%% -record(ptr_val, {
%%        data_offset,
%%        data_size
%%       }).
%% This struct represents sets of data of identical type
%% like notification identifiers, attributes  etc.
%% It may only be used in conjunction with the data type value
%% SA_NTF_VALUE_ARRAY. Functions
%% SaNtfArrayValAllocate() or SaNtfArrayValGet() shall be used to
%% get a pointer for accessing the real data. Direct access is not allowed.
%% -record(array_val, {
%%        array_offset,
%%        num_elements,
%%        element_size
%%       }).

%%----------------------------------------------------------------------
%% 3.12.14 Additional Text  */
%% Use SaStringT. A string consists of UTF-8 encoded characters and is
%% terminated by the '\0' character.

%%----------------------------------------------------------------------
%% 3.12.15 SaNtfAdditionalInfoT

%%----------------------------------------------------------------------
%% 3.12.16 SaNtfProbableCauseT
%% @type sa_ntf_probable_cause() = atom().
%%   A description of the probable fault in the system .......
-define(SA_NTF_ADAPTER_ERROR,                                    sa_ntf_adapter_error).
-define(SA_NTF_APPLICATION_SUBSYSTEM_FAILURE,                    sa_ntf_application_subsystem_failure).
-define(SA_NTF_BANDWIDTH_REDUCED,                                sa_ntf_bandwidth_reduced).
-define(SA_NTF_CALL_ESTABLISHMENT_ERROR,                         sa_ntf_call_establishment_error).
-define(SA_NTF_COMMUNICATIONS_PROTOCOL_ERROR,                    sa_ntf_communications_protocol_error).
-define(SA_NTF_COMMUNICATIONS_SUBSYSTEM_FAILURE,                 sa_ntf_communications_subsystem_failure).
-define(SA_NTF_CONFIGURATION_OR_CUSTOMIZATION_ERROR,             sa_ntf_configuration_or_customization_error).
-define(SA_NTF_CONGESTION,                                       sa_ntf_congestion).
-define(SA_NTF_CORRUPT_DATA,                                     sa_ntf_corrupt_data).
-define(SA_NTF_CPU_CYCLES_LIMIT_EXCEEDED,                        sa_ntf_cpu_cycles_limit_exceeded).
-define(SA_NTF_DATASET_OR_MODEM_ERROR,                           sa_ntf_dataset_or_modem_error).
-define(SA_NTF_DEGRADED_SIGNAL,                                  sa_ntf_degraded_signal).
-define(SA_NTF_D_T_E,                                            sa_ntf_d_t_e).
-define(SA_NTF_ENCLOSURE_DOOR_OPEN,                              sa_ntf_enclosure_door_open).
-define(SA_NTF_EQUIPMENT_MALFUNCTION,                            sa_ntf_equipment_malfunction).
-define(SA_NTF_EXCESSIVE_VIBRATION,                              sa_ntf_excessive_vibration).
-define(SA_NTF_FILE_ERROR,                                       sa_ntf_file_error).
-define(SA_NTF_FIRE_DETECTED,                                    sa_ntf_fire_detected).
-define(SA_NTF_FLOOD_DETECTED,                                   sa_ntf_flood_detected).
-define(SA_NTF_FRAMING_ERROR,                                    sa_ntf_framing_error).
-define(SA_NTF_HEATING_OR_VENTILATION_OR_COOLING_SYSTEM_PROBLEM, sa_ntf_heating_or_ventilation_or_cooling_system_problem).
-define(SA_NTF_HUMIDITY_UNACCEPTABLE,                            sa_ntf_humidity_unacceptable).
-define(SA_NTF_INPUT_OUTPUT_DEVICE_ERROR,                        sa_ntf_input_output_device_error).
-define(SA_NTF_INPUT_DEVICE_ERROR,                               sa_ntf_input_device_error).
-define(SA_NTF_L_A_N_ERROR,                                      sa_ntf_l_a_n_error).
-define(SA_NTF_LEAK_DETECTED,                                    sa_ntf_leak_detected).
-define(SA_NTF_LOCAL_NODE_TRANSMISSION_ERROR,                    sa_ntf_local_node_transmission_error).
-define(SA_NTF_LOSS_OF_FRAME,                                    sa_ntf_loss_of_frame).
-define(SA_NTF_LOSS_OF_SIGNAL,                                   sa_ntf_loss_of_signal).
-define(SA_NTF_MATERIAL_SUPPLY_EXHAUSTED,                        sa_ntf_material_supply_exhausted).
-define(SA_NTF_MULTIPLEXER_PROBLEM,                              sa_ntf_multiplexer_problem).
-define(SA_NTF_OUT_OF_MEMORY,                                    sa_ntf_out_of_memory).
-define(SA_NTF_OUTPUT_DEVICE_ERROR,                              sa_ntf_output_device_error).
-define(SA_NTF_PERFORMANCE_DEGRADED,                             sa_ntf_performance_degraded).
-define(SA_NTF_POWER_PROBLEM,                                    sa_ntf_power_problem).
-define(SA_NTF_PRESSURE_UNACCEPTABLE,                            sa_ntf_pressure_unacceptable).
-define(SA_NTF_PROCESSOR_PROBLEM,                                sa_ntf_processor_problem).
-define(SA_NTF_PUMP_FAILURE,                                     sa_ntf_pump_failure).
-define(SA_NTF_QUEUE_SIZE_EXCEEDED,                              sa_ntf_queue_size_exceeded).
-define(SA_NTF_RECEIVE_FAILURE,                                  sa_ntf_receive_failure).
-define(SA_NTF_RECEIVER_FAILURE,                                 sa_ntf_receiver_failure).
-define(SA_NTF_REMOTE_NODE_TRANSMISSION_ERROR,                   sa_ntf_remote_node_transmission_error).
-define(SA_NTF_RESOURCE_AT_OR_NEARING_CAPACITY,                  sa_ntf_resource_at_or_nearing_capacity).
-define(SA_NTF_RESPONSE_TIME_EXCESSIVE,                          sa_ntf_response_time_excessive).
-define(SA_NTF_RETRANSMISSION_RATE_EXCESSIVE,                    sa_ntf_retransmission_rate_excessive).
-define(SA_NTF_SOFTWARE_ERROR,                                   sa_ntf_software_error).
-define(SA_NTF_SOFTWARE_PROGRAM_ABNORMALLY_TERMINATED,           sa_ntf_software_program_abnormally_terminated).
-define(SA_NTF_SOFTWARE_PROGRAM_ERROR,                           sa_ntf_software_program_error).
-define(SA_NTF_STORAGE_CAPACITY_PROBLEM,                         sa_ntf_storage_capacity_problem).
-define(SA_NTF_TEMPERATURE_UNACCEPTABLE,                         sa_ntf_temperature_unacceptable).
-define(SA_NTF_THRESHOLD_CROSSED,                                sa_ntf_threshold_crossed).
-define(SA_NTF_TIMING_PROBLEM,                                   sa_ntf_timing_problem).
-define(SA_NTF_TOXIC_LEAK_DETECTED,                              sa_ntf_toxic_leak_detected).
-define(SA_NTF_TRANSMIT_FAILURE,                                 sa_ntf_transmit_failure).
-define(SA_NTF_TRANSMITTER_FAILURE,                              sa_ntf_transmitter_failure).
-define(SA_NTF_UNDERLYING_RESOURCE_UNAVAILABLE,                  sa_ntf_underlying_resource_unavailable).
-define(SA_NTF_VERSION_MISMATCH,                                 sa_ntf_version_mismatch).
-define(SA_NTF_AUTHENTICATION_FAILURE,                           sa_ntf_authentication_failure).
-define(SA_NTF_BREACH_OF_CONFIDENTIALITY,                        sa_ntf_breach_of_confidentiality).
-define(SA_NTF_CABLE_TAMPER,                                     sa_ntf_cable_tamper).
-define(SA_NTF_DELAYED_INFORMATION,                              sa_ntf_delayed_information).
-define(SA_NTF_DENIAL_OF_SERVICE,                                sa_ntf_denial_of_service).
-define(SA_NTF_DUPLICATE_INFORMATION,                            sa_ntf_duplicate_information).
-define(SA_NTF_INFORMATION_MISSING,                              sa_ntf_information_missing).
-define(SA_NTF_INFORMATION_MODIFICATION_DETECTED,                sa_ntf_information_modification_detected).
-define(SA_NTF_INFORMATION_OUT_OF_SEQUENCE,                      sa_ntf_information_out_of_sequence).
-define(SA_NTF_INTRUSION_DETECTION,                              sa_ntf_intrusion_detection).
-define(SA_NTF_KEY_EXPIRED,                                      sa_ntf_key_expired).
-define(SA_NTF_NON_REPUDIATION_FAILURE,                          sa_ntf_non_repudiation_failure).
-define(SA_NTF_OUT_OF_HOURS_ACTIVITY,                            sa_ntf_out_of_hours_activity).
-define(SA_NTF_OUT_OF_SERVICE,                                   sa_ntf_out_of_service).
-define(SA_NTF_PROCEDURAL_ERROR,                                 sa_ntf_procedural_error).
-define(SA_NTF_UNAUTHORIZED_ACCESS_ATTEMPT,                      sa_ntf_unauthorized_access_attempt).
-define(SA_NTF_UNEXPECTED_INFORMATION,                           sa_ntf_unexpected_information).
-define(SA_NTF_UNSPECIFIED_REASON,                               sa_ntf_unspecified_reason).

-type sa_ntf_probable_cause() :: ?SA_NTF_ADAPTER_ERROR
			       | ?SA_NTF_APPLICATION_SUBSYSTEM_FAILURE
			       | ?SA_NTF_BANDWIDTH_REDUCED
			       | ?SA_NTF_CALL_ESTABLISHMENT_ERROR
			       | ?SA_NTF_COMMUNICATIONS_PROTOCOL_ERROR
			       | ?SA_NTF_COMMUNICATIONS_SUBSYSTEM_FAILURE
			       | ?SA_NTF_CONFIGURATION_OR_CUSTOMIZATION_ERROR
			       | ?SA_NTF_CONGESTION
			       | ?SA_NTF_CORRUPT_DATA
			       | ?SA_NTF_CPU_CYCLES_LIMIT_EXCEEDED
			       | ?SA_NTF_DATASET_OR_MODEM_ERROR
			       | ?SA_NTF_DEGRADED_SIGNAL
			       | ?SA_NTF_D_T_E
			       | ?SA_NTF_ENCLOSURE_DOOR_OPEN
			       | ?SA_NTF_EQUIPMENT_MALFUNCTION
			       | ?SA_NTF_EXCESSIVE_VIBRATION
			       | ?SA_NTF_FILE_ERROR
			       | ?SA_NTF_FIRE_DETECTED
			       | ?SA_NTF_FLOOD_DETECTED
			       | ?SA_NTF_FRAMING_ERROR
			       | ?SA_NTF_HEATING_OR_VENTILATION_OR_COOLING_SYSTEM_PROBLEM
			       | ?SA_NTF_HUMIDITY_UNACCEPTABLE
			       | ?SA_NTF_INPUT_OUTPUT_DEVICE_ERROR
			       | ?SA_NTF_INPUT_DEVICE_ERROR
			       | ?SA_NTF_L_A_N_ERROR
			       | ?SA_NTF_LEAK_DETECTED
			       | ?SA_NTF_LOCAL_NODE_TRANSMISSION_ERROR
			       | ?SA_NTF_LOSS_OF_FRAME
			       | ?SA_NTF_LOSS_OF_SIGNAL
			       | ?SA_NTF_MATERIAL_SUPPLY_EXHAUSTED
			       | ?SA_NTF_MULTIPLEXER_PROBLEM
			       | ?SA_NTF_OUT_OF_MEMORY
			       | ?SA_NTF_OUTPUT_DEVICE_ERROR
			       | ?SA_NTF_PERFORMANCE_DEGRADED
			       | ?SA_NTF_POWER_PROBLEM
			       | ?SA_NTF_PRESSURE_UNACCEPTABLE
			       | ?SA_NTF_PROCESSOR_PROBLEM
			       | ?SA_NTF_PUMP_FAILURE
			       | ?SA_NTF_QUEUE_SIZE_EXCEEDED
			       | ?SA_NTF_RECEIVE_FAILURE
			       | ?SA_NTF_RECEIVER_FAILURE
			       | ?SA_NTF_REMOTE_NODE_TRANSMISSION_ERROR
			       | ?SA_NTF_RESOURCE_AT_OR_NEARING_CAPACITY
			       | ?SA_NTF_RESPONSE_TIME_EXCESSIVE
			       | ?SA_NTF_RETRANSMISSION_RATE_EXCESSIVE
			       | ?SA_NTF_SOFTWARE_ERROR
			       | ?SA_NTF_SOFTWARE_PROGRAM_ABNORMALLY_TERMINATED
			       | ?SA_NTF_SOFTWARE_PROGRAM_ERROR
			       | ?SA_NTF_STORAGE_CAPACITY_PROBLEM
			       | ?SA_NTF_TEMPERATURE_UNACCEPTABLE
			       | ?SA_NTF_THRESHOLD_CROSSED
			       | ?SA_NTF_TIMING_PROBLEM
			       | ?SA_NTF_TOXIC_LEAK_DETECTED
			       | ?SA_NTF_TRANSMIT_FAILURE
			       | ?SA_NTF_TRANSMITTER_FAILURE
			       | ?SA_NTF_UNDERLYING_RESOURCE_UNAVAILABLE
			       | ?SA_NTF_VERSION_MISMATCH
			       | ?SA_NTF_AUTHENTICATION_FAILURE
			       | ?SA_NTF_BREACH_OF_CONFIDENTIALITY
			       | ?SA_NTF_CABLE_TAMPER
			       | ?SA_NTF_DELAYED_INFORMATION
			       | ?SA_NTF_DENIAL_OF_SERVICE
			       | ?SA_NTF_DUPLICATE_INFORMATION
			       | ?SA_NTF_INFORMATION_MISSING
			       | ?SA_NTF_INFORMATION_MODIFICATION_DETECTED
			       | ?SA_NTF_INFORMATION_OUT_OF_SEQUENCE
			       | ?SA_NTF_INTRUSION_DETECTION
			       | ?SA_NTF_KEY_EXPIRED
			       | ?SA_NTF_NON_REPUDIATION_FAILURE
			       | ?SA_NTF_OUT_OF_HOURS_ACTIVITY
			       | ?SA_NTF_OUT_OF_SERVICE
			       | ?SA_NTF_PROCEDURAL_ERROR
			       | ?SA_NTF_UNAUTHORIZED_ACCESS_ATTEMPT
			       | ?SA_NTF_UNEXPECTED_INFORMATION
			       | ?SA_NTF_UNSPECIFIED_REASON.

%%----------------------------------------------------------------------
%% 3.12.17 SaNtfSpecificProblemT

-type sa_ntf_specific_problem() :: #safsNtfSpecificProblem{}.

%%----------------------------------------------------------------------
%% 3.12.18 SaNtfSeverityT
%% @type sa_ntf_severity() = atom().
%%  The severity of the notification.
-define(SA_NTF_SEVERITY_CLEARED,       sa_ntf_severity_cleared). %% alarm notification, only
-define(SA_NTF_SEVERITY_INDETERMINATE, sa_ntf_severity_indeterminate).
-define(SA_NTF_SEVERITY_WARNING,       sa_ntf_severity_warning).
-define(SA_NTF_SEVERITY_MINOR,         sa_ntf_severity_minor).
-define(SA_NTF_SEVERITY_MAJOR,         sa_ntf_severity_major).
-define(SA_NTF_SEVERITY_CRITICAL,      sa_ntf_severity_critical).

-type sa_ntf_severity() :: ?SA_NTF_SEVERITY_CLEARED
			 | ?SA_NTF_SEVERITY_INDETERMINATE
			 | ?SA_NTF_SEVERITY_WARNING
			 | ?SA_NTF_SEVERITY_MINOR
			 | ?SA_NTF_SEVERITY_MAJOR
			 | ?SA_NTF_SEVERITY_CRITICAL.

%%----------------------------------------------------------------------
%% 3.12.19 SaNtfSeverityTrendT
%% @type sa_ntf_severity_trend() = atom().
%%  The trend of the severity .
-define(SA_NTF_TREND_MORE_SEVERE, sa_ntf_trend_more_severe).
-define(SA_NTF_TREND_NO_CHANGE,   sa_ntf_trend_no_change).
-define(SA_NTF_TREND_LESS_SEVERE, sa_ntf_trend_less_severe).

-type sa_ntf_severity_trend() :: ?SA_NTF_TREND_MORE_SEVERE
			       | ?SA_NTF_TREND_NO_CHANGE
			       | ?SA_NTF_TREND_LESS_SEVERE.

%%----------------------------------------------------------------------
%% 3.12.20 SaNtfThresholdInformationT
-type sa_ntf_threshold_information() :: #safsNtfThresholdInformation{}.

%%----------------------------------------------------------------------
%% 3.12.21 SaNtfProposedRepairActionT
-type sa_ntf_proposed_repair_action() :: #safsNtfProposedRepairAction{}.

%%----------------------------------------------------------------------
%% 3.12.22 SaNtfSourceIndicatorT
-define(SA_NTF_OBJECT_OPERATION,     sa_ntf_object_operation).
-define(SA_NTF_MANAGEMENT_OPERATION, sa_ntf_management_operation).
-define(SA_NTF_UNKNOWN_OPERATION,    sa_ntf_unknown_operation).

-type sa_ntf_source_indicator() :: ?SA_NTF_OBJECT_OPERATION
				 | ?SA_NTF_MANAGEMENT_OPERATION
				 | ?SA_NTF_UNKNOWN_OPERATION.

%%----------------------------------------------------------------------
%% 3.12.23 SaNtfStateChangeT
-type sa_ntf_state_change() :: #safsNtfStateChange{}.

%%----------------------------------------------------------------------
%% 3.12.24 SaNtfAttributeT
-type sa_ntf_attribute() :: #safsNtfAttribute{}.

%%----------------------------------------------------------------------
%% 3.12.25 SaNtfAttributeChangeT
-type sa_ntf_attribute_change() :: #safsNtfAttributeChange{}.

%%----------------------------------------------------------------------
%% 3.12.26 SaNtfServiceUserT
-type sa_ntf_service_user() :: #safsNtfServiceUser{}.

%%----------------------------------------------------------------------
%% 3.12.27 SaNtfSecurityAlarmDetectorT
-type sa_ntf_security_alarm_detector() :: #safsNtfSecurityAlarmDetector{}.

%%----------------------------------------------------------------------
%% 3.12.28 SaNtfNotificationHeaderT
-type sa_ntf_notification_header() :: #safsNtfNotificationHeader{}.

%%----------------------------------------------------------------------
%% 3.12.29 SaNtfObjectCreateDeleteNotificationT
-type sa_ntf_object_create_delete_notification() :: #safsNtfObjectCreateDeleteNotification{}.

%%----------------------------------------------------------------------
%% 3.12.30 SaNtfAttributeChangeNotificationT
-type sa_ntf_attribute_change_notification() :: #safsNtfAttributeChangeNotification{}.

%%----------------------------------------------------------------------
%% 3.12.31 SaNtfStateChangeNotificationT
-type sa_ntf_state_change_notification() :: #safsNtfStateChangeNotification{}.

%%----------------------------------------------------------------------
%% 3.12.32 SaNtfAlarmNotificationT
-type sa_ntf_alarm_notification() :: #safsNtfAlarmNotification{}.

%%----------------------------------------------------------------------
%% 3.12.33 SaNtfSecurityAlarmNotificationT
-type sa_ntf_security_alarm_notification() :: #safsNtfSecurityAlarmNotification{}.

%%----------------------------------------------------------------------
%% 3.12.34

%%----------------------------------------------------------------------
%% 3.12.35 SaNtfSubscriptionIdT
-type sa_ntf_subscription_id() :: sa_uint32().

%%----------------------------------------------------------------------
%% 3.12.36 SaNtfNotificationFilterHeaderT
-type sa_ntf_notification_filter_header() :: #safsNtfNotificationFilterHeader{}.

%%----------------------------------------------------------------------
%% 3.12.37 SaNtfObjectCreateDeleteNotificationFilterT
-type sa_ntf_object_create_delete_notification_filter() :: #safsNtfObjectCreateDeleteNotificationFilter{}.

%%----------------------------------------------------------------------
%% 3.12.38 SaNtfAttributeChangeNotificationFilterT
-type sa_ntf_attribute_change_notification_filter() :: #safsNtfAttributeChangeNotificationFilter{}.

%%----------------------------------------------------------------------
%% 3.12.39 SaNtfStateChangeNotificationFilterT
-type sa_ntf_state_change_notification_filter() :: #safsNtfStateChangeNotificationFilter{}.

%%----------------------------------------------------------------------
%% 3.12.40 SaNtfAlarmNotificationFilterT
-type sa_ntf_alarm_notification_filter() :: #safsNtfAlarmNotificationFilter{}.

%%----------------------------------------------------------------------
%% 3.12.41 SaNtfSecurityAlarmNotificationFilterT
-type sa_ntf_security_alarm_notification_filter() :: #safsNtfSecurityAlarmNotificationFilter{}.

%%----------------------------------------------------------------------
%% 3.12.42 SaNtfSearchModeT
-define(SA_NTF_SEARCH_BEFORE_OR_AT_TIME, sa_ntf_search_before_or_at_time).
-define(SA_NTF_SEARCH_AT_TIME,           sa_ntf_search_at_time).
-define(SA_NTF_SEARCH_AT_OR_AFTER_TIME,  sa_ntf_search_at_or_after_time).
-define(SA_NTF_SEARCH_BEFORE_TIME,       sa_ntf_search_before_time).
-define(SA_NTF_SEARCH_AFTER_TIME,        sa_ntf_search_after_time).
-define(SA_NTF_SEARCH_NOTIFICATION_ID,   sa_ntf_search_notification_id).
-define(SA_NTF_SEARCH_ONLY_FILTER,       sa_ntf_search_only_filter).

-type sa_ntf_search_mode() :: ?SA_NTF_SEARCH_BEFORE_OR_AT_TIME
			    | ?SA_NTF_SEARCH_AT_TIME
			    | ?SA_NTF_SEARCH_AT_OR_AFTER_TIME
			    | ?SA_NTF_SEARCH_BEFORE_TIME
			    | ?SA_NTF_SEARCH_AFTER_TIME
			    | ?SA_NTF_SEARCH_NOTIFICATION_ID
			    | ?SA_NTF_SEARCH_ONLY_FILTER.

%%----------------------------------------------------------------------
%% 3.12.43 SaNtfSearchCriteriaT
-type sa_ntf_search_criteria() :: #safsNtfSearchCriteria{}.

%%----------------------------------------------------------------------
%% 3.12.44 SaNtfSearchDirectionT
-define(SA_NTF_SEARCH_OLDER,   sa_ntf_search_older).
-define(SA_NTF_SEARCH_YOUNGER, sa_ntf_search_younger).

-type sa_ntf_search_direction() :: ?SA_NTF_SEARCH_OLDER
				 | ?SA_NTF_SEARCH_YOUNGER.

%%----------------------------------------------------------------------
%% 3.12.45 SaNtfNotificationTypeFilterHandlesT
%% Unused handles in SaNtfNotificationTypeFilterHandlesT should be set to SA_NTF_FILTER_HANDLE_NULL
-define(SA_NTF_FILTER_HANDLE_NULL, undefined).

-define(SA_NTF_TYPE_FILTER_HANDLE_NULL, undefined). %% ???

-type sa_ntf_notification_type_filters() :: #safsNtfNotificationTypeFilters{}.

%%----------------------------------------------------------------------
%% 3.12.46 SaNtfNotificationsT
-type sa_ntf_notification() :: #safsNtfNotification{}.


-endif.
