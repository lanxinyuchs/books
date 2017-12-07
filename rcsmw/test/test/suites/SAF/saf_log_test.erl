%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	saf_log_test.erl %
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R4A/R5A/2

%%% @doc 
%%% == Library for basic test of CS SAF LOG ==
%%%  
%%% @end
%%% ----------------------------------------------------------
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
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2013-07-25 uabesvi     created
%%% R4A/1      2015-09-30 etxmlar     now() depricated in OTP 18 changed to os:timestamp() 
%%% ----------------------------------------------------------
%%% 
-module(saf_log_test).


%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([err_bad_flag/0]).
-export([err_exist/0]).
-export([err_invalid_handle/0]).
-export([err_invalid_param/0]).
-export([err_no_resources/0]).
-export([err_version/0]).
-export([get_log_rec_gen/1]).
-export([get_log_rec_gen/2]).
-export([get_log_rec_ntf/1]).
-export([host/0]).
-export([module/0]).
-export([noof_app_streams/1]).
-export([ok/0]).
-export([all_severities/0]).
-export([severity/1]).
-export([version/1]).


-include_lib("common_test/include/ct.hrl").
-include_lib("safe/include/safe.hrl").
-include_lib("safe/include/safe_log.hrl").
-include_lib("saf_log_test.hrl").





%%=======================================================================
%% External Interface Functions
%%=======================================================================


%%========================================================================
%% ok and error results
%%========================================================================
ok() -> ok(host()).
ok(?TESTNODE) -> ok;
ok(_)         -> ?SA_OK.

err_bad_flag() -> err_bad_flag(host()).
%% safe does not check the flags properly. Should be ?SAFE_AIS_ERR_BAD_FLAGS;
err_bad_flag(?TESTNODE) -> {error, badarg};   
err_bad_flag(_)         -> ?SA_ERR_BAD_FLAG.

err_exist() -> err_exist(host()).
err_exist(?TESTNODE) -> ?SAFE_AIS_ERR_EXIST;
err_exist(_)         -> ?SA_ERR_EXIST.

err_invalid_handle() -> err_invalid_handle(host()).
%% should be ?SAFE_AIS_ERR_BAD_HANDLE;
err_invalid_handle(?TESTNODE) -> {error, function_clause}; 
err_invalid_handle(_)         -> ?SA_ERR_BAD_HANDLE.

err_invalid_param() -> err_invalid_param(host()).
err_invalid_param(?TESTNODE) -> ?SAFE_AIS_ERR_INVALID_PARAM;
err_invalid_param(_)         -> ?SA_ERR_INVALID_PARAM.

err_no_resources() -> err_no_resources(host()).
err_no_resources(?TESTNODE) -> ?SAFE_AIS_ERR_NO_RESOURCES;
err_no_resources(_)         -> ?SA_ERR_NO_RESOURCES.

err_version() -> err_version(host()).
err_version(?TESTNODE) -> ?SAFE_AIS_ERR_VERSION;
err_version(_)         -> ?SA_ERR_VERSION.










%%=======================================================================
%% host() -> Host
%% 
%% get host name
%%=======================================================================
host() -> 
    ?TESTNODE.


%%=======================================================================
%% module() -> Module
%% 
%% get module to be used for connecting to the log impl
%% In cc environment rct_safe_log_rpc and in git safs_log
%%=======================================================================
module() -> 
    rct_safe_log_rpc.



%%========================================================================
%% noof_app_streams(Expected) -> true | {error, Reason}
%% check number of streams, 
%% both that there are expected number of processes and
%% that the streams are configured in the db (#safs_log_stream_config{})
%%========================================================================
noof_app_streams(Expected) ->
    noof_app_streams(host(), Expected).

noof_app_streams(?TESTNODE, _Expected) ->
    %% not yet implemented
    true;
noof_app_streams(_, Expected) ->
    %% Decrment with the 3 well known logs
    ok = nas_pids(Expected),
    ok = nas_db(Expected),
    true.
    

nas_pids(Expected) ->
    Pids = safs_log_srv:stream_pids(),
    case length(Pids) - 3 of
	Expected ->
	    ok;
	_Actual ->
	    {error, {noof_pids, {Expected, Pids}}}
    end.

nas_db(Expected) ->
    case safs_log_db:stream_config_noof() - 3 of
	Expected ->
	    ok;
	_Actual ->
	    {error, {noof_db, {Expected, 
			       ets:tab2list(safs_log_stream_config)}}}    
    end.

   
%%=======================================================================
%% all_severities() -> Severity
%%
%% get severity
%%=======================================================================
all_severities() ->
    all_severities(host()).

all_severities(?TESTNODE) ->
    [?SAFE_LOG_SEV_FLAG_EMERGENCY,
     ?SAFE_LOG_SEV_FLAG_ALERT,     
     ?SAFE_LOG_SEV_FLAG_CRITICAL,  
     ?SAFE_LOG_SEV_FLAG_ERROR,     
     ?SAFE_LOG_SEV_FLAG_WARNING,   
     ?SAFE_LOG_SEV_FLAG_NOTICE,    
     ?SAFE_LOG_SEV_FLAG_INFO];
all_severities(_) ->
    [?LOG_EMERGENCY,
     ?LOG_ALERT,   
     ?LOG_CRITICAL,
     ?LOG_ERROR,   
     ?LOG_WARNING, 
     ?LOG_NOTICE,  
     ?LOG_INFO].


severity(Sev) ->
    severity(host(), Sev).

severity(?TESTNODE, ?SEVERITY_EMERGENCY) -> ?SAFE_LOG_SEV_EMERGENCY;
severity(?TESTNODE, ?SEVERITY_ALERT)     -> ?SAFE_LOG_SEV_ALERT;
severity(?TESTNODE, ?SEVERITY_CRITICAL)  -> ?SAFE_LOG_SEV_CRITICAL;
severity(?TESTNODE, ?SEVERITY_ERROR)     -> ?SAFE_LOG_SEV_ERROR;
severity(?TESTNODE, ?SEVERITY_WARNING)   -> ?SAFE_LOG_SEV_WARNING;
severity(?TESTNODE, ?SEVERITY_NOTICE)    -> ?SAFE_LOG_SEV_NOTICE; 
severity(?TESTNODE, ?SEVERITY_INFO)      -> ?SAFE_LOG_SEV_INFO;
severity(_,         ?SEVERITY_EMERGENCY) -> ?LOG_EMERGENCY;
severity(_,         ?SEVERITY_ALERT)     -> ?LOG_ALERT;
severity(_,         ?SEVERITY_CRITICAL)  -> ?LOG_CRITICAL;
severity(_,         ?SEVERITY_ERROR)     -> ?LOG_ERROR;
severity(_,         ?SEVERITY_WARNING)   -> ?LOG_WARNING;
severity(_,         ?SEVERITY_NOTICE)    -> ?LOG_NOTICE; 
severity(_,         ?SEVERITY_INFO)      -> ?LOG_INFO.
    
    


%%=======================================================================
%% version(Vsn) -> VersionRec
%%
%% get version record
%%=======================================================================
version(Vsn) ->
    version(host(), Vsn).

version(?TESTNODE, ?CURRENT_VSN) ->
    v_safe_vsn($A, 2, 1);
version(?TESTNODE, ?RC_PLUS_VSN) ->
    v_safe_vsn($D, 2, 1);
version(?TESTNODE, ?MAJOR_PLUS_VSN) ->
    v_safe_vsn($A, 3, 1);
version(?TESTNODE, ?MAJOR_MINUS_VSN) ->
    v_safe_vsn($A, 1, 1);
version(?TESTNODE, ?MINOR_PLUS_VSN) ->
    v_safe_vsn($A, 2, 7);
version(_, ?CURRENT_VSN) ->
    v_safs_vsn($A, 2, 1);
version(_, ?RC_PLUS_VSN) ->
    v_safs_vsn($D, 2, 1);
version(_, ?MAJOR_PLUS_VSN) ->
    v_safs_vsn($A, 3, 1);
version(_, ?MAJOR_MINUS_VSN) ->
    v_safs_vsn($A, 1, 1);
version(_, ?MINOR_PLUS_VSN) ->
    v_safs_vsn($A, 2, 7).


v_safe_vsn(RC, Maj, Min) ->
    #safe_version{release_code  = RC,
		  major_version = Maj,
		  minor_version = Min}.

v_safs_vsn(RC, Maj, Min) ->
    #safsVersion{releaseCode  = RC,
		 majorVersion = Maj,
		 minorVersion = Min}.


%%========================================================================
%% 
%% 
%% 
%% 
%%========================================================================
get_log_rec_gen(Msg) ->
    get_log_rec_gen(Msg, severity(?SEVERITY_INFO)).

get_log_rec_gen(Msg, Severity) ->
    get_log_rec_gen(host(), Msg, Severity).


get_log_rec_gen(?TESTNODE, Msg, Severity) ->
    GenHdr = #safe_log_generic_log_header{notification_class_id = undefined,
					  log_svc_usr_name      = "userName",
					  log_severity          = Severity},
    %% LogHeader = #safe_log_header{generic_hdr = GenHdr},
    #safe_log_record{time_stamp = ?TIME_STAMP,
		     hdr_type   = ?SAFE_LOG_GENERIC_HEADER,
		     header     = GenHdr,
		     buffer     = list_to_binary(Msg)
		    };
get_log_rec_gen(_, Msg, Severity) ->
    GenHdr = {safsLogGenericLogHeader,
	      undefined,
	      <<"userName">>,
	      Severity},
    LogHeader = #safsLogHeader{genericHdr = GenHdr},
    #safsLogRecord{logTimeStamp = calendar:time_to_seconds(os:timestamp()) * 1000,
		   logHdrType   = ?LOG_HEADER_GENERIC,
		   logHeader    = LogHeader,
		   logBuffer    = list_to_binary(Msg)
		  }.


get_log_rec_ntf(Msg) ->
    get_log_rec_ntf(host(), Msg).


get_log_rec_ntf(?TESTNODE, Msg) ->
%%     NtfHdr = {safsLogNtfLogHeader,
%% 	      123,
%% 	      ?NTF_OBJ_NOTIFS_START,
%% 	      "notificationObject",
%% 	      "notifyingObject",
%% 	      undefined,
%% 	      1234567
%% 	     },

    NotId     = ?SAFE_NTF_IDENTIFIER_UNUSED,
    EventType = ?SAFE_NTF_OBJECT_NOTIFICATIONS_START,
    TimeStamp = ?TIME_STAMP,
    
    Hdr = #safe_log_ntf_log_header{notification_id       = NotId,
				   event_type            = EventType,
				   notification_object   = "notificationObject",
				   notifying_object      = "notifyingObject",
				   notification_class_id = undefined,
				   event_time            = TimeStamp
				  },

%%     GenHdr = #safe_log_generic_log_header{notification_class_id = undefined,
%% 					  log_svc_usr_name      = "userName",
%% 					  log_severity  = ?SAFE_LOG_SEV_INFO},
    #safe_log_record{time_stamp = TimeStamp,
		     hdr_type   = ?SAFE_LOG_NTF_HEADER,
		     header     = Hdr,
		     buffer     = list_to_binary(Msg)
		    };
%%     LogHeader = #safsLogHeader{ntfHdr = Hdr},
%%     #safsLogRecord{logTimeStamp = ?TIME_STAMP,
%% 		   logHdrType   = ?LOG_HEADER_NTF,
%% 		   logHeader    = LogHeader,
%% 		   logBuffer    = list_to_binary(Msg)
%% 		  }.
get_log_rec_ntf(_, Msg) ->
    GenHdr = {safsLogNtfLogHeader,
	      123,
	      ?NTF_OBJ_NOTIFS_START,
	      "notificationObject",
	      "notifyingObject",
	      undefined,
	      1234567
	     },
    LogHeader = #safsLogHeader{ntfHdr = GenHdr},
    #safsLogRecord{logTimeStamp = calendar:time_to_seconds(os:timestamp()) * 1000,
		   logHdrType   = ?LOG_HEADER_NTF,
		   logHeader    = LogHeader,
		   logBuffer    = list_to_binary(Msg)
		  }.

