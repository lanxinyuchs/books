
-define(TESTNODE, testnode).

-define(MICRO_SECS(Now),
	begin
	    {MeS, S, MiS} = Now,
	    Mult = 1000000,
	    Mult*(Mult*MeS + S) + MiS
	end).


-define(TIME_STAMP, ?MICRO_SECS(os:timestamp()) * 1000).


-define(CURRENT_VSN,     current_vsn).
-define(RC_PLUS_VSN,     rc_plus_vsn).
-define(MAJOR_PLUS_VSN,  major_plus_vsn).
-define(MAJOR_MINUS_VSN, major_minus_vsn).
-define(MINOR_PLUS_VSN,  minor_plus_vsn).



-define(SEVERITY_EMERGENCY, severity_emergency).
-define(SEVERITY_ALERT,     severity_alert).
-define(SEVERITY_CRITICAL,  severity_critical).
-define(SEVERITY_ERROR,     severity_error).
-define(SEVERITY_WARNING,   severity_warning).
-define(SEVERITY_NOTICE,    severity_notice).
-define(SEVERITY_INFO,      severity_info).



%%=========================================================================
%% safs definitions, copied from safs_log.hrl and log.hrl
%%=========================================================================

%% {enum,safsLogStreamOpenFlags}
-define(LOG_OPEN_FLAGS, sa_log_stream_create).

%% {enum,safsLogAckFlags}
-define(LOG_ACK_FLAGS, sa_log_record_write_ack).

%% {enum,safsLogFileFullAction}
-define(LOG_WRAP,   sa_log_file_full_action_wrap).
-define(LOG_HALT,   sa_log_file_full_action_halt).
-define(LOG_ROTATE, sa_log_file_full_action_rotate).

%% Default log record format expressions
-define(LOG_APP_FORMAT,    "@Cr @Ch:@Cn:@Cs @Cm/@Cd/@CY @Sv @Sl \"@Cb\"").
-define(LOG_SYSTEM_FORMAT, "@Cr @Ch:@Cn:@Cs @Cm/@Cd/@CY @Sv @Sl \"@Cb\"").
-define(LOG_NOTIFY_FORMAT, "@Cr @Ct @Nt @Ne6 @No30 @Ng30 \"@Cb\"").
-define(LOG_ALARM_FORMAT,  "@Cr @Ct @Nt @Ne6 @No30 @Ng30 \"@Cb\"").

-define(SA_OK,                sa_ais_ok).
-define(SA_ERR_LIBRARY,       sa_ais_err_library).
-define(SA_ERR_TIMEOUT,       sa_ais_err_try_timeout).
-define(SA_ERR_TRY_AGAIN,     sa_ais_err_try_again).
-define(SA_ERR_BAD_HANDLE,    sa_ais_err_bad_handle).
-define(SA_ERR_BAD_FLAG,      sa_ais_err_bad_flag).
-define(SA_ERR_INIT,          sa_ais_err_init).
-define(SA_ERR_INVALID_PARAM, sa_ais_err_invalid_param).
-define(SA_ERR_NO_RESOURCES,  sa_ais_err_no_resources).
-define(SA_ERR_NO_MEMORY,     sa_ais_err_no_memory).
-define(SA_ERR_NOT_EXIST,     sa_ais_err_not_exist).
-define(SA_ERR_EXIST,         sa_ais_err_exist).
-define(SA_ERR_VERSION,       sa_ais_err_version).
-define(SA_ERR_NO_OP,         sa_ais_err_no_op).
-define(SA_ERR_NOT_SUPPORTED, sa_ais_err_not_supported).

%% {enum,safsLogSeverity}
-define(LOG_EMERGENCY, sa_log_sev_emergency).
-define(LOG_ALERT,     sa_log_sev_alert).
-define(LOG_CRITICAL,  sa_log_sev_critical).
-define(LOG_ERROR,     sa_log_sev_error).
-define(LOG_WARNING,   sa_log_sev_warning).
-define(LOG_NOTICE,    sa_log_sev_notice).
-define(LOG_INFO,      sa_log_sev_info).

%% Default stream names
-define(LOG_SYSTEM, "safLgStrCfg=saLogSystem,safApp=safLogService").
-define(LOG_NOTIFY, "safLgStrCfg=saLogNotification,safApp=safLogService").
-define(LOG_ALARM,  "safLgStrCfg=saLogAlarm,safApp=safLogService").

-define(APP_LOG_DIR, "app_logs").
-define(SAF_LOG_DIR, "saf_log").

%% {enum,safsNtfEventType}
%% Event types enum, these are only generic
%% types as defined by the X.73x standards
%% Also defined in safs_ntf.hrl with longer macros.
-define(NTF_OBJ_NOTIFS_START, sa_ntf_object_notifications_start).
-define(NTF_OBJ_CREATION,     sa_ntf_object_creation).
-define(NTF_OBJ_DELETION,     sa_ntf_object_deletion).

%% {enum,safsLogHeaderType}
-define(LOG_HEADER_NTF,     sa_log_ntf_header).
-define(LOG_HEADER_GENERIC, sa_log_generic_header).


-record(safsLogFileCreateAttributes_2,
        {logFileName,                   % = 1, string
         logFilePathName,               % = 2, string (optional)
         maxLogFileSize,                % = 3, uint64
         maxLogRecordSize,              % = 4, uint32
         haProperty,                    % = 5, bool
         logFileFullAction,             % = 6, {enum,safsLogFileFullAction}
         maxFilesRotated,               % = 7, uint32 (optional)
         logFileFmt                     % = 8, string (optional)
        }).

-record(safsLogSeverityFlags,
        {saLogSevFlagEmergency,         % = 1, bool
         saLogSevFlagAlert,             % = 2, bool
         saLogSevFlagCritical,          % = 3, bool
         saLogSevFlagError,             % = 4, bool
         saLogSevFlagWarning,           % = 5, bool
         saLogSevFlagNotice,            % = 6, bool
         saLogSevFlagInfo               % = 7, bool
        }).

-record(safsLogCallbacks,
        {saLogFilterSetCallback,        % = 1, bool
         saLogStreamOpenCallback,       % = 2, bool
         saLogWriteLogCallback          % = 3, bool
        }).

-record(safsVersion,
        {releaseCode,                   % = 1, uint32
         majorVersion,                  % = 2, uint32
         minorVersion                   % = 3, uint32
        }).

-record(safsLogHeader,
        {ntfHdr,                        % = 1, {msg,safsLogNtfLogHeader} (optional)
         genericHdr                     % = 2, {msg,safsLogGenericLogHeader} (optional)
        }).

-record(safsLogRecord,
        {logTimeStamp,                  % = 1, uint64
         logHdrType,                    % = 2, {enum,safsLogHeaderType}
         logHeader,                     % = 3, {msg,safsLogHeader}
         logBuffer                      % = 4, string (optional)
        }).

