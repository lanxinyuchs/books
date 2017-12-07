%%% #---------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	log.hrl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_vsn('/main/R2A/R3A/R4A/R5A/R8A/R9A/R10A/R11A/6').
-hrl_date('2017-09-11').
-hrl_author('uabesvi').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: 54/002 01-LXA 119 334 Ux, Rev: /main/6 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
%%% 
%%% The information in this document is the property of Ericsson.
%%% 
%%% Except as specifically authorized in writing by Ericsson, the 
%%% receiver of this document shall keep the information contained 
%%% herein confidential and shall protect the same in whole or in 
%%% part from disclosure and dissemination to third parties.
%%% 
%%% Disclosure and disseminations to the receivers employees shall 
%%% only be made on a strict need to know basis.
%%% %CCaseCopyrightEnd%
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R2A/1      2013-10-01 uabesvi     Created
%%% R4A/1      2015-05-20 etxpejn     Removed SystemLog from PREDEF_INT_LOGS
%%%                                   Created RCS_LOGS
%%% R9A/2    2017-03-27 uabesvi  Added macros for compress
%%% R10A/2   2017-03-27 uabesvi  Added macros for logWeb 
%%% R11A/1-3 2017-08-30 uabesvi  Added macros for RU ESI
%%% ----------------------------------------------------------
%%% 
%%% #---------------------------------------------------------
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description: 
%%% ----------------------------------------------------------


-define(WEB_VERSION_1, v1).

-define(UPGR_LOGS,        ["SwmLog", "SwmInternal"]).
-define(UPGR_DIR,         sysEnv:rcs_dir() ++ "/upgrade_logs").
-define(UPGR_DIRS_MAX,    3).
-define(UPGR_FILE_PREFIX, "copied_logs").

-define(ITC_LOG_ESI, "log_esi").

-define(LOG_ESI_EXT_CONN_ESTABLISH_UREQ,        16#0191001).
-define(LOG_ESI_EXT_GET_BOARDS_RREQ,            16#0191002).
-define(LOG_ESI_EXT_GET_BOARDS_RCFM,            16#0191003).
-define(LOG_ESI_EXT_GET_BOARD_INFO_RREQ,        16#0191004).
-define(LOG_ESI_EXT_GET_BOARD_INFO_RCFM,        16#0191005).
-define(LOG_ESI_EXT_GET_BOARD_INFO_RREJ,        16#0191006).
-define(LOG_ESI_EXT_GET_BOARD_INFO_READY_RUREQ, 16#0191007).

-define(LOG_ESI_EXT_MAX_PROD_NUMBER_LENGTH,   400).
-define(LOG_ESI_EXT_MAX_IP_ADD_LENGTH,         32).
-define(LOG_ESI_EXT_ERROR_STRING_MAX_LENGTH,  150).
-define(LOG_ESI_EXT_DIR_PATH_MAX_LENGTH,      400).
-define(LOG_ESI_EXT_NAMESPACE_MAX_LENGTH,     256).


-define(ESI_DIR,      sysEnv:rcs_dir() ++ "/imported_esi").
-define(ESI_DIRS_MAX, 1).
-define(ESI_FILE_PREFIX, "esi").

-define(ESI_VSN_FULL,    full).
-define(ESI_VSN_LIMITED, limited).
-define(ESI_VSN_CHUNK,   chunk).

-define(AVLI_DIR, "alh").
-define(AVLI_LOG, "AvliLog").

-define(VAR_DIR, "/var/log").
-define(VAR_LOG, "var.tar.gz").

-define(WEB_ERROR_VERSION, <<"not supported version">>).
-define(WEB_ERROR_SAVE,    <<"could not store received bonary">>).

-define(HTTP_INTERNAL_SERVER_ERROR, 500).
-define(HTTP_NOT_IMPLEMENTED,       501).

-define(GET_LOG_SIZE_BYTES(__Size), __Size * 1024).

-define(RCS_LOGS, ["SecurityLog",
		   "AuditTrailLog",
		   "NotificationLog"]).

-define(PREDEF_MO_LOGS, ["AlarmLog",
			 "SecurityLog",
			 "AuditTrailLog"
			]).


-define(LOG_REC_SIZE_DEF, 200).

-define(PREDEF_INT_LOGS, ["NotificationLog"]).

-define(PREDEF_LOGS, ?PREDEF_INT_LOGS ++ ?PREDEF_MO_LOGS).

-define(PREDEF_ENCRYPTED_LOGS, ["SecurityLog", "AuditTrailLog"]).

-define(UPGRADE_LOGS, ["SwmLog", "SwmInternal"]).
-define(NOOF_SAVED_UPGRADE_LOGS, 3).




%%===================================================================
%% logData
%% 
%% logId             - Name of the LOG in format {ME, SF, LOGM, Name}
%% maxSize           - deprecated, use maxSizeKb
%%                     Size of the log files in mega bytes
%% maxSizeKb         - Size of the log files in kilo bytes
%% rotatingSegments  - Number of files
%% recordSize        - Size of each log entry
%%                     Only valid for SAF logs. Used instead of 
%%                     the specified default value in SAF standard
%% appLog            - Specifies if the LOG is defined 
%%                     in an appdata file
%% internal          - Specifies if an MO is associate with the log
%% milliSec          - Specifies if milli seconds are included 
%%                     in the timestamp
%% 
%%===================================================================
-record(logData, {logId,
		  maxSize,
		  maxSizeKb,
		  rotatingSegments,
		  recordSize,
		  appLog   = false,
		  internal = false,
		  milliSec = false
		 }).

%%===================================================================
%% logStorageMode
%% 
%% logId      - Name of the LOG
%% local      - Specifies if the LOG is local for the VM or common for the node 
%% encrypted  - Specifies if the LOG is to be encrypted
%% compressed - Specifies if the LOG files are to be compressed
%% 
%%===================================================================
-record(logStorageMode, {logId              :: string(),
			 local      = false :: boolean(),
			 encrypted  = false :: boolean(),
			 compressed = false :: boolean()
			}).

-define(LOG_LDN, [<<"ManagedElement=1">>,
		  <<"SystemFunctions=1">>,
		  <<"LogM=1">>]).

%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------

-record(logAlarm, {sa_name,         %% name as defined in safs
		   stream_handle,   %% safs stream handle
		   log_name,        %% name of the log as in the MO 
		   ldn,             %% ldn to the LOG MO
		   alarm = false}). %% is alarm sent


-record(logRamLog, {name,           %% name of the RAM log
		    severity = 1}). %% current severity level [0..9]


%%% ----------------------------------------------------------
%%% #           record_name
%%% Description: Record for internal logs (not visible in the LogM model)
%%% ----------------------------------------------------------


