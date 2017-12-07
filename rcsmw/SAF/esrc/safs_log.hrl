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
%% File: safs_log.hrl
%% 
%% Description:
%%    
%%
%%----------------------------------------------------------------------
-ifndef(safs_log_hrl).
-define(safs_log_hrl, true).

-include("log.hrl").

-define(SERVER, safs_log_srv).

-define(MAX_NUM_APP_LOG_STREAMS, 1000).
-define(NUM_PREDEF_STREAMS,      3).

-define(LIMIT_MAX_APP_STREAMS, sa_log_max_num_cluster_app_log_streams_id).

-define(APP_LOGS_DIR,        "app_logs").
-define(SYS_LOGS_DIR,        "saf_logs").
-define(SYS_LOGS_FILE_SIZE,  1000).
-define(SYS_LOGS_REC_SIZE,   150).
-define(SYS_LOGS_NOOF_FILES, 4).


%% The supported versions. 
%% The values denotes {releaseCode, majorVersion, minorVersion}
%% NOTE: The versions must be defined from the highest to the lowest.
-define(LOG_RELEASE_CODE, $A).
-define(LOG_MAJOR_VERSION, 2).
-define(LOG_MINOR_VERSION, 1).
-define(SUPPORTED_LOG_VERSIONS, [#safsVersion{releaseCode=?LOG_RELEASE_CODE,
					      majorVersion=?LOG_MAJOR_VERSION,
					      minorVersion=?LOG_MINOR_VERSION}]).

%% {enum,safsLogFileFullAction}
-define(LOG_WRAP,   sa_log_file_full_action_wrap).
-define(LOG_HALT,   sa_log_file_full_action_halt).
-define(LOG_ROTATE, sa_log_file_full_action_rotate).

%% {enum,safsLogStreamOpenFlags}
-define(LOG_OPEN_FLAGS, sa_log_stream_create).

%% {enum,safsLogAckFlags}
-define(LOG_ACK_FLAGS, sa_log_record_write_ack).

%% {enum,safsLogHeaderType}
-define(LOG_HEADER_NTF,     sa_log_ntf_header).
-define(LOG_HEADER_GENERIC, sa_log_generic_header).

%% {enum,safsLogSeverity}
-define(LOG_EMERGENCY, sa_log_sev_emergency).
-define(LOG_ALERT,     sa_log_sev_alert).
-define(LOG_CRITICAL,  sa_log_sev_critical).
-define(LOG_ERROR,     sa_log_sev_error).
-define(LOG_WARNING,   sa_log_sev_warning).
-define(LOG_NOTICE,    sa_log_sev_notice).
-define(LOG_INFO,      sa_log_sev_info).

%% {enum,safsNtfEventType}
%% Event types enum, these are only generic
%% types as defined by the X.73x standards
%% Also defined in safs_ntf.hrl with longer macros.
-define(NTF_OBJ_NOTIFS_START, sa_ntf_object_notifications_start).
-define(NTF_OBJ_CREATION,     sa_ntf_object_creation).
-define(NTF_OBJ_DELETION,     sa_ntf_object_deletion).

-define(NTF_ATTR_NOTIFS_START, sa_ntf_attribute_notifications_start).
-define(NTF_ATTR_ADDED,        sa_ntf_attribute_added).
-define(NTF_ATTR_REMOVED,      sa_ntf_attribute_removed).
-define(NTF_ATTR_CHANGED,      sa_ntf_attribute_changed).
-define(NTF_ATTR_RESET,        sa_ntf_attribute_reset).

-define(NTF_STATE_CHANGE_NOTIFS_START, sa_ntf_state_change_notifications_start).
-define(NTF_OBJ_STATE_CHANGE,          sa_ntf_object_state_change).

-define(NTF_ALARM_NOTIFS_START,  sa_ntf_alarm_notifications_start).
-define(NTF_ALARM_COMMUNICATION, sa_ntf_alarm_communication).
-define(NTF_ALARM_QOS,           sa_ntf_alarm_qos).
-define(NTF_ALARM_PROCESSING,    sa_ntf_alarm_processing).
-define(NTF_ALARM_EQUIPMENT,     sa_ntf_alarm_equipment).
-define(NTF_ALARM_ENVIRONMENT,   sa_ntf_alarm_environment).

-define(NTF_SEC_ALARM_NOTIFS_START, sa_ntf_security_alarm_notifications_start).
-define(NTF_INTEGRITY_VIOLATION,    sa_ntf_integrity_violation).
-define(NTF_OPERATION_VIOLATION,    sa_ntf_operation_violation).
-define(NTF_PHYSICAL_VIOLATION,     sa_ntf_physical_violation).
-define(NTF_SEC_SERVICE_VIOLATION,  sa_ntf_security_service_violation).
-define(NTF_TIME_VIOLATION,         sa_ntf_time_violation).


%% Default stream names
-define(LOG_SYSTEM, "safLgStrCfg=saLogSystem,safApp=safLogService").
-define(LOG_NOTIFY, "safLgStrCfg=saLogNotification,safApp=safLogService").
-define(LOG_ALARM,  "safLgStrCfg=saLogAlarm,safApp=safLogService").

%% Default log record format expressions
-define(LOG_APP_FORMAT,    "@Cr @Ch:@Cn:@Cs @Cm/@Cd/@CY @Sv @Sl \"@Cb\"").
-define(LOG_SYSTEM_FORMAT, "@Cr @Ch:@Cn:@Cs @Cm/@Cd/@CY @Sv @Sl \"@Cb\"").
%-define(LOG_NOTIFY_FORMAT, "@Cr @Ct @Nt @Ne5 @No30 @Ng30 \"@Cb\"").
%-define(LOG_ALARM_FORMAT,  "@Cr @Ct @Nt @Ne5 @No30 @Ng30 \"@Cb\"").
-define(LOG_NOTIFY_FORMAT, "@Cr @Ct @Nt @Ne6 @No30 @Ng30 \"@Cb\"").
-define(LOG_ALARM_FORMAT,  "@Cr @Ct @Nt @Ne6 @No30 @Ng30 \"@Cb\"").


-define(SA_OK,                   sa_ais_ok).
-define(SA_ERR_LIBRARY,          sa_ais_err_library).
-define(SA_ERR_TIMEOUT,          sa_ais_err_timeout).
-define(SA_ERR_TRY_AGAIN,        sa_ais_err_try_again).
-define(SA_ERR_BAD_HANDLE,       sa_ais_err_bad_handle).
-define(SA_ERR_BAD_FLAG,         sa_ais_err_bad_flag).
-define(SA_ERR_BAD_OPERATION,    sa_ais_err_bad_operation).
-define(SA_ERR_FAILED_OPERATION, sa_ais_err_failed_operation).
-define(SA_ERR_INIT,             sa_ais_err_init).
-define(SA_ERR_INVALID_PARAM,    sa_ais_err_invalid_param).
-define(SA_ERR_NO_RESOURCES,     sa_ais_err_no_resources).
-define(SA_ERR_NO_MEMORY,        sa_ais_err_no_memory).
-define(SA_ERR_NOT_EXIST,        sa_ais_err_not_exist).
-define(SA_ERR_EXIST,            sa_ais_err_exist).
-define(SA_ERR_VERSION,          sa_ais_err_version).
-define(SA_ERR_NO_OP,            sa_ais_err_no_op).
-define(SA_ERR_NOT_SUPPORTED,    sa_ais_err_not_supported).
-define(SA_ERR_BUSY,             sa_ais_err_busy).

-type sa_log_callbacks() :: #safsLogCallbacks{}.

-type sa_log_handle() :: term().
-type sa_log_stream_handle() :: term().
-type sa_log_stream_name() :: term().
-type sa_log_create_attributes() :: term().
-type sa_log_identifier() :: term().
-type sa_log_timeout() :: term().
-type sa_log_invocation() :: term().
-type sa_log_open_flags() :: term().
-type sa_log_ack_flags() :: term().
-type sa_log_log_record() :: term().
-type sa_log_severity_flags_record() :: term().
-type sa_log_limit_id() :: term().
-type sa_log_limit_value() :: integer().



%%----------------------------------------------------------------------
%% STOLEN FROM safs_ntf.hrl until the hrl files are fixed
%%----------------------------------------------------------------------
%% 3.12.7 SaNtfClassIdT
%%
-type sa_ntf_class_id() :: #safsNtfClassId{}.

-define(SA_NTF_VENDOR_ID_SAF, 18568).


-endif.
