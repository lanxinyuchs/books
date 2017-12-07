%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	alh_service.erl %
%%% @hidden
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R10A/R11A/R12A/3
%%%
%%% @doc == ALH Service ==
%%% This module implements the RBS CS Availabilty Log Service.
%%%
%%% ----------------------------------------------------------
-module(alh_service).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R10A/R11A/R12A/3').
-date('2017-11-08').
-author('etxberb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R2A/1      2012-10-30 eolaand     Created
%%% R2A/14     2013-10-02 etxlg       Added coli function
%%% R2A/15     2013-11-06 eolaand     Added export log function for LogM
%%% R2A/17     2014-01-24 etxberb     Introduced AVLI version 4.
%%% R2A/18     2014-01-28 etxberb     Added AdditionalInfo & AvailabilityInfo in
%%%                                   the Erlang (alhI) Interface.
%%% R2A/23     2014-02-18 etxberb     Added * delete_oldest_objs/0.
%%%                                   * swmI:write_upgrWindow_table in
%%%                                   store_logrecord/3.
%%% R2A/24     2014-03-03 etxberb     Removed ets table for Log Record Number
%%%                                   and added mnesia:lock in store_logrecord/3
%%%                                   for safe cluster handling.
%%% R2A/25     2014-03-10 etxberb     Handling of strings for free text:
%%%                                   * Indentation at line feed & carriage ret.
%%%                                   * Padding
%%%                                   * XML comment
%%%                                   * handling of faulty XML strings (e.g.
%%%                                     incomplete comment, etc.)
%%% R2A/26     2014-03-12 etxberb     Added validate_dtd/1.
%%% R2A/29     2014-03-17 etxberb     Added logInfoRecord/1 & logInfoRecord/2.
%%% R2A/30     2014-03-19 etxberb     Added call to comsaI:subscribe_timestep.
%%% R2A/31     2014-03-20 etxberb     Removed EventId from log entry.
%%% R2A/35     2014-03-25 etxberb     Added DTD validation in write_event.
%%% R2A/38     2014-03-27 etxberb     Added ClientPid in tcp messages.
%%% R2A/39     2014-03-31 etxberb     Increased call timer for write_event to
%%%                                   10 seconds.
%%% R2A/40     2014-04-01 etxberb     * Changed upgrWindow to copy whole table.
%%%                                   * Added write_event_async/2.
%%%                                   * Added init_continue.
%%%                                   * Changed xmerl_to_simple for handling of
%%%                                     text between xml tags.
%%% R2A/43     2014-04-15 etxberb     Allow write_event/2 to execute regardless
%%%                                   of alh_service process is alive or not.
%%% R2A/44     2014-04-30 etxberb     Added print_xxx functions as COLI commands
%%% R2A/45     2014-08-05 etxberb     Added trace_info for
%%%                                   'CelloAvli2_initiateService'.
%%% -----      -------    --------    ------------------------
%%% R3A/1      2015-01-15 etxberb     Increased timer for cec_setup from default
%%%                                   5 sec to 10 sec.
%%% R3A/2      2015-02-23 etxberb     Added clhI:role() in init/1.
%%% R3A/3      2015-03-18 etxberb     TR HT50597: Added nodeDown_ts_update.
%%% R3A/4      2015-05-22 etxberb     TR HT76921: Removed gen_server:call for
%%%                                   cec_setup.
%%% -----      -------    --------    ------------------------
%%% R4A/1      2015-06-22 etxberb     Increased CALL_TIMEOUT from 10 to 180 sec.
%%% R4A/2      2015-06-23 etxberb     Replaced deprecated call to clhI.
%%% R4A/3      2015-08-20 etxberb     Added warm_restart/1.
%%% R4A/4      2015-09-24 etxberb     Moved nodeDown timestamp from APPM and
%%%                                   mnesia to ALH and file under /rcs/alh.
%%% R4A/5      2015-09-25 etxberb     Added info_report at RestartCompleted.
%%% R4A/6      2015-09-29 etxberb     Correction of nodeDown_ts_get for an
%%%                                   upgrade-cancel-upgrade scenario.
%%% R4A/7      2015-09-30 etxberb     Minor correction of nodeDown_ts.
%%% R4A/8      2015-10-06 etxberb     Added cec_takeover/1.
%%% R4A/9      2015-10-19 etxberb     OTP-18: Replaced erlang:now/0.
%%% R4A/10     2015-12-09 etxberb     Added NodeDown timestamp file on ram
%%%                                   storage, written every 1 second, and
%%%                                   changed interval for disk write to 10 s.
%%% -----      -------    --------    ------------------------
%%% R5A/1      2015-12-11 ekurnik     Moved cec interface to alh_cec_service.
%%%                                   Implemented common interface for cec and 
%%%                                   itc service.
%%% R5A/2      2015-12-29 etomist     Removed alh_cec_service
%%% R5A/3      2016-04-11 etxberb     Changed 'zip:zip' to 'zlib:gzip'.
%%% -----      -------    --------    ------------------------
%%% R6A/1      2016-08-23 etxarnu     MR2230: Added handling of Avli5_WriteHw
%%% R6A/2      2016-09-05 etxarnu     Added missing 'Cello_Avli5_writeHwEvent'
%%% -----      -------    --------    ------------------------
%%% R8A/2      2017-01-03 uabesvi     Moved LOG_DIR and RAM_DIR to point to /tmp
%%%                                   - used when exporting availability log
%%%                                   - the log file should not be written on disk (cloud)
%%% R8A/3      2017-01-19 etxberb     Moved FILE_nodeDownTs_disk back to disk
%%%                                   storage.
%%% -----      -------    --------    ------------------------
%%% R10A/1     2017-05-18 etxjotj     Restart PM events for vrcs
%%% -----      -------    --------    ------------------------
%%% R11A/1     2017-10-01 etxberb     SP531: Add start_traffic/1,stop_traffic/3,
%%%                                   upgrade_activated/0, upgrade_confirmed/0,
%%%                                   dump_to_file/1, push_from_file/1,
%%%                                   to_datetime/1, vLog_dir/0, get_ts/1.
%%% R11A/2     2017-10-16 etxberb     * 'replace_nodeDown' done.
%%%                                   * Adaptions to OTP20.
%%% R11A/3     2017-10-18 etxberb     Added handling of NodeDown timestamp for
%%%                                   rollback.
%% ----    ---------- -------  -------------------------------------------------
%% R12A/1  2017-10-26 etxberb  SP531: Bugfix in replace_nodeDown.
%% R12A/2  2017-10-27 etxberb  Additions for "SP277: backup/restore of vSD/vPP"
%% R12A/3  2017-11-08 etxberb  SP531: Bugfix in consume_eventQ/2.
%%% ----------------------------------------------------------

%%%===================================================================
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([write_event/2,
	 write_event_async/2,
	 default_log_file/0,
	 default_log_dir/0,
	 get_log_created/0,
	 reset_log/0,
	 get_log/1,
	 get_ts/1,
	 export_log/3,
	 to_datetime/1,
	 to_time/1,
	 vLog_dir/0]).

%% From SWM REST interface
-export([start_traffic/1,
	 stop_traffic/3,
	 upgrade_activated/0,
	 upgrade_confirmed/0]).

%% From LOG REST interface
-export([dump_to_file/1]).
-export([push_from_file/1]).

-export([warm_restart/1]).

-export([subscribe/0,
	 subscribe/1,
	 unsubscribe/0,
	 unsubscribe/1]).


-export([start/0,
	 start/1,
	 start_link/0,
	 start_link/1,
	 stop/0,
	 stop/1]).

-export([activate/0]).
-export([delete_oldest_objs/0]).

-export([cec_setup/1,
	 cec_takeover/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([print_from/2,
	 print_clientInfo_from/2,
	 print_latest/1,
	 print_oldest/1]).

%%% Exported for COLI
-export([coli_read_records/1]).

%% Test
-export([get_UgInfo/0,
	 get_UgInfo/1]).

%-compile(export_all).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export(['CelloAvli2_writePiuEvent'/2,
	 'Cello_Avli2_writeHwEvent'/2,
	 'Cello_Avli2_writeServiceEvent'/2,
	 'Cello_Avli2_writeOtherEvent'/2,
	 'CelloAvli3_writePgmEvent'/2,
	 'CelloAvli4_writeNodeEvent'/2,
	 'Cello_Avli5_writeHwEvent'/2,
	 logInfoRecord/2]).

-export([logInfoRecord/1]).

-export([alh_dir/0,
	 alh_home_dir/0]).

%% Used only in test purposes
-export([get_state/0,
	 info/0,
	 info_all/0]).

%%%===================================================================
%%% Include files
%%%===================================================================
-include("alh_service.hrl").
-include("alhI.hrl").
-include_lib("kernel/include/file.hrl").

%%%===================================================================
%%% Macros
%%%===================================================================
-define(SERVER, ?MODULE).
-define(AVAILABILITY_LOG, "RBS_CS_AVAILABILITY_LOG").
-define(AVAILABILITY_LOG_XML, ?AVAILABILITY_LOG ++ ".xml").
-define(AVAILABILITY_LOG_DTD, ?AVAILABILITY_LOG ++ ".dtd").
-define(LOG_DIR, filename:join([sysEnv:rcs_root(), "tmp", "log", "alh"])).
-define(RAM_DIR, filename:join([sysEnv:rcs_root(), "tmp", "log", "alh_ram"])).
-define(VLOG_DIR, filename:join([sysEnv:vnf_dir(), "log", "AlhLog"])).
-define(ALH_DIR, alh_dir()).
-define(DISK_DIR, ?ALH_DIR).
-define(FILE_nodeDownTs_disk, filename:join(?DISK_DIR, "nodeDown_ts")).
-define(FILE_nodeDownTs_ram, filename:join(?RAM_DIR, "nodeDown_ts")).
-define(FILE_nodeDownTs_rollback, filename:join(?VLOG_DIR, "nodeDown_ts")).
-define(MAX_LOG_REC_NO, 5000).
-define(CALL_TIMEOUT, 180000).
-define(Interval_ts_ram, 999).
-define(Interval_ts_disk, 9999).
-define(Interval_ts_ram_msg, interval_ts_ram_msg).
-define(Interval_ts_disk_msg, interval_ts_disk_msg).

-define(LF, #xmlText{value = "\n"}).
-define(SP, #xmlText{value = " "}).

-define(TRUE, 1).
-define(FALSE, 0).

%% Versions
-define(AVLI_SIGNAL_REVISION, 0).
-define(CELLO_AVLI_NO_PV, 0).
-define(CELLO_AVLI_PV2,   2).
-define(CELLO_AVLI_PV3,   3).
-define(CELLO_AVLI_PV4,   4).
-define(CELLO_AVLI_LATEST_PV, ?CELLO_AVLI_PV4).
-define(AVLI_SUPPORTED_PVs, [?CELLO_AVLI_PV4]).

%% Timestamp
-define(CELLO_AVLI_TIME_BY_AVLI, 16#FFFFFFFF).

%% Signals
-define(CELLO_AVLI2_INITIATE_SERVICE_CFM, 16#10826).
-define(CELLO_AVLI2_INITIATE_SERVICE_REJ, 16#10827).
-define(CELLO_AVLI_SERVER_UP_IND,         16#1055E).
-define(CELLO_AVLI_SERVER_DOWN_IND,       16#1055F).
-define(CELLO_AVLI_WRITE_CFM,             16#1055C).

%% C API
-define(AVLI2_INIT_SERVICE,               7).
-define(AVLI2_WRITE_PIU_EVENT,            9).
-define(AVLI2_WRITE_HW_EVENT,            10).
-define(AVLI2_WRITE_SERVICE_EVENT,       11).
-define(AVLI2_WRITE_OTHER_EVENT,         12).
-define(AVLI3_WRITE_PGM_EVENT,           13).
-define(AVLI4_WRITE_NODE_EVENT,          14).
-define(AVLI5_WRITE_HW_EVENT,            15).

%% Commands
-define(CELLO_AVLI_WRITE_NODE_EVENT,     0).
-define(CELLO_AVLI_WRITE_PIU_EVENT,      1).
-define(CELLO_AVLI_WRITE_HW_EVENT,       2).
-define(CELLO_AVLI_WRITE_SERVICE_EVENT,  3).
-define(CELLO_AVLI_WRITE_OTHER_EVENT,    4).
-define(CELLO_AVLI_WRITE_PGM_EVENT,      5).
-define(CELLO_AVLI_WRITE_HW5_EVENT,      6).

%% Service Status
-define(CELLO_AVLI_EVENT_NOT_USED,           0).
-define(CELLO_AVLI_IN_SERVICE,               1).
-define(CELLO_AVLI_OUT_OF_SERVICE,           2).
-define(CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE, 3).

%% Reason
-define(CELLO_AVLI_REASON_NOT_USED,  0).
-define(CELLO_AVLI_SHUTDOWN_COMMAND, 1).
-define(CELLO_AVLI_UNOPERATIONAL,    2).
-define(CELLO_AVLI_STARTING,         3).
-define(CELLO_AVLI_OPERATIONAL,      4).

%% Piu Type
-define(CELLO_AVLI_NONE, 0).
-define(CELLO_AVLI_MP,   1).
-define(CELLO_AVLI_BP,   2).

%% Result
-define(CELLO_AVLI_SUCCESS,                      0).
-define(CELLO_AVLI_SERVICE_UNAVAIL,              1).
-define(CELLO_AVLI_TOO_LONG_STRING,              2).
-define(CELLO_AVLI_OUT_OF_MEMORY,                3).
-define(CELLO_AVLI_ILLEGAL_SIGNAL,               4).
-define(CELLO_AVLI_ILLEGAL_EVENT,                5).
-define(CELLO_AVLI_LOG_NOT_CREATED,              6).
-define(CELLO_AVLI_ILLEGAL_PARAM,                7).
-define(CELLO_AVLI_MEMORY_NOT_INITIATED,         8).
-define(CELLO_AVLI_NOT_SUPPORTED_BY_SELECTED_PV, 9).

%%% ----------------------------------------------------------
%%% Type specs
%%% ----------------------------------------------------------
-type now_ts() :: {MegSec::integer(), Sec::integer(), MicSec::integer()} | 
		  integer() .

%%%===================================================================
%%% Records
%%%===================================================================
-record(state, {data,
		nodeDownTs,
		nodeDownTs_warm_restart,
		nodeDownTs_IntervalRef,
		clientPidInfo = [],
		subscribers = []}).

-record(tRefs, {ram,
		disk}).

-record(alh_eventQ, {key,
		     obj}).

-record(alh_eventQ_rollback, {key,
			      obj}).

%%%===================================================================
%%% Include files
%%%===================================================================
-include_lib("xmerl/include/xmerl.hrl").

%%%===================================================================
%%% Misc
%%%===================================================================
alh_dir() ->
    filename:join(sysEnv:rcs_dir(), "alh").

alh_home_dir() ->
    filename:join(sysEnv:home_dir(), "alh").

%%%===================================================================
%%% API
%%%===================================================================
write_event(Event, Params) ->
    case whereis(?SERVER) of
	Pid when is_pid(Pid) ->
	    TimeBefore = ?MonoTime,
	    Res = gen_server:call(?SERVER,
				  {write_event, Event, Params},
				  ?CALL_TIMEOUT),
	    write_event_validate_callTime(?MonoTime - TimeBefore),
	    Res;
	undefined ->
	    %% This may happen during startup of the system when cold reboot is
	    %% ordered. The comsaNtpServer is doing this when the clock was
	    %% stepped too much.
	    write_event(Event,
			Params,
			self(),
			#state{})   % Pretend being server process!
    end.

write_event_validate_callTime(Diff) ->
    TimeWithinLimit = is_time_within_limit(Diff, 5000, milli_seconds),
    if
	TimeWithinLimit ->
	    ok;
	?ELSE ->
	    Reason =
		['Check value of (validate_dtd) in a previous INFO REPORT',
		 'If several seconds - there might be a disk reading problem!'],
	    WarnInfo =
		[?AVLI_HEADING,
		 'Synchronous call took more than 10 secs',
		 {'Elapsed time (write_event)', ?time2string(Diff)} |
		 Reason],
	    sysInitI:warning_report(WarnInfo)
    end.

%%%===================================================================
%%% Messages from VNFM (LCM).
%%%===================================================================
%%%===================================================================
%%% Attributes from stop_traffic via LCM/VNFM.
%%%===================================================================
start_traffic(StopTime) ->
    ?LOG_INFO([{stopTime, StopTime}]),
    ST =
	try
	    list_to_integer(StopTime)
	catch
	    ExcClass : ExcReason ->
		?LOG_WARN(["Invalid StopTime",
			   {stopTime, StopTime},
			   {ExcClass, ExcReason}]),
		undefined
	end,
    case file:read_file_info(?FilePushedFromOldNode) of
	{ok, _} ->
	    file:delete(?FilePushedFromOldNode);
	{error, enoent} ->
	    ?LOG_ERR(["Log not copied from old node"]),
	    MyAvlEvent =
		evtNode_ShutdownCommand_create(StopTime,
					       ?ALH_TAG_UpgradeNormal),
	    ?LOG_WARN(["--- Logging with default values ---" | MyAvlEvent]),
	    evtNode_ShutdownCommand_write(MyAvlEvent, async)
    end,
    gen_server:cast(?SERVER, {start_traffic, ST, self()}).

%%%===================================================================
stop_traffic(TimeOfAction, AvliCause, StopTime) ->
    AvlEvent = evtNode_ShutdownCommand_create(TimeOfAction, AvliCause),
    ok = evtNode_ShutdownCommand_write(AvlEvent, sync),
    mnesia_sync(),
    [{stop_time, ensureBin(to_secs(StopTime))}].

%%%===================================================================
upgrade_activated() ->
    UgInfo = #{state      => ?UgState_activated,
	       last_event => mnesia:dirty_last(?MNESIA_TAB)},
    file:write_file(?FILE_UpgradeInfo, term_to_binary(UgInfo)),
    ?LOG_INFO([{ugInfo, get_UgInfo()},
	       ?FILE_UpgradeInfo,
	       file:read_file_info(?FILE_UpgradeInfo)]),
    ok.

%%%===================================================================
upgrade_confirmed() ->
    file:delete(?FILE_UpgradeInfo),
    delete_Rollback(),
    ok.

%%%===================================================================
mnesia_sync() ->
    gen_server:call(?SERVER, mnesia_sync, infinity).

%%%===================================================================
evtNode_ShutdownCommand_create(TimeOfAction, Cause) ->
    AvliCause =
	case Cause of
	    "" ->
		?LOG_WARN([{"No cause value. Assuming",
			    ?ALH_TAG_UpgradeNormal}]),
		?ALH_TAG_UpgradeNormal;
	    _ ->
		Cause
	end,
    EvtId = 0,
    [{evt_type, ensureStr(?NODE_EVENT)},
     {time_of_action, ensureStr(to_secs(TimeOfAction))},
     {service_status, ensureStr(?ALH_TAG_OutOfService)},
     {reason, ensureStr(?ALH_TAG_ShutdownCommand)},
     {event_id, ensureStr(EvtId)},
     {rank, ensureStr(?ALH_TAG_RankCold)},
     {cause, ensureStr(AvliCause)}].

%%%===================================================================
to_secs(TimeStr) when is_list(TimeStr) ->
    Time =
	try
	    list_to_integer(TimeStr)
	catch
	    _ : _ ->
		?LOG_WARN(["No time value. Assuming current local time."]),
		calendar:local_time()
	end,
    to_secs(Time);
to_secs(Time) ->
    calendar:datetime_to_gregorian_seconds(to_datetime(Time)).

%%%===================================================================
evtNode_ShutdownCommand_write(AvlEvent, ExecType) ->
    try
	{_, TimeOfAction} = lists:keyfind(time_of_action, 1, AvlEvent),
	{_, ServiceStatus} = lists:keyfind(service_status, 1, AvlEvent),
	{_, Reason} = lists:keyfind(reason, 1, AvlEvent),
	{_, EvtId} = lists:keyfind(event_id, 1, AvlEvent),
	{_, Rank} = lists:keyfind(rank, 1, AvlEvent),
	{_, AvliCause} = lists:keyfind(cause, 1, AvlEvent),
	AddInfo = lists:flatten([list_to_atom(Rank),
				 ?ALH_TAG_Cause(AvliCause)]),
	alhI:write_node_event(list_to_integer(TimeOfAction),
			      list_to_atom(ServiceStatus),
			      list_to_atom(Reason),
			      list_to_integer(EvtId),
			      ?ALH_TAG_Rcs(AddInfo),
			      ExecType)
    catch
	ExcClass : ExcReason ->
	    Stacktrace = ?STACKTRACE_E,
	    Exception = {ExcClass, ExcReason},
	    ?LOG_WARN(["Missing or incorrect data in avl_event",
		       {avlEvent, AvlEvent},
		       Exception,
		       {stacktrace, Stacktrace}]),
	    Exception
    end.

%%%===================================================================
is_time_within_limit(NativeTime, TimeLimit, TimeUnit) ->
    Time = erlang:convert_time_unit(NativeTime, native, TimeUnit),
    is_time_within_limit(Time, TimeLimit).

is_time_within_limit(Time, TimeLimit) when Time =< TimeLimit ->
    true;
is_time_within_limit(_, _) ->
    false.

write_event_async(Event, Params) ->
    gen_server:cast(?SERVER, {write_event_async, Event, Params, self()}).

logInfoRecord(Params) ->
    gen_server:cast(?SERVER, {logInfoRecord, Params, self()}).


get_log_created() ->
    TimeStamp = calendar:local_time(),
    {'LogCreated', [{'LogName', [" " ++ ?AVAILABILITY_LOG ++ " "]},
		    timestamp_xml(TimeStamp),
		    {'AdditionalLogInfo', [[]]}]}.


default_log_file() ->
    ?AVAILABILITY_LOG_XML.


default_log_dir() ->
    ?LOG_DIR.

vLog_dir() ->
    ?VLOG_DIR.


reset_log() ->
    gen_server:call(?SERVER, reset_log).


get_log(Opts) ->
    Reset = proplists:get_value(reset_log, Opts, false),
    Self = self(),
    case whereis(?SERVER) of
	Self ->
	    {ok, to_xml_log(handle_get_log(Reset))};
	_ ->
	    {ok, DbLog} = gen_server:call(?SERVER, {get_log, Reset}),
	    {ok, to_xml_log(DbLog)}
    end.


export_log(LogDir, File, Opts) ->
    put(trace_info, []),
    create_log_dir(LogDir),
    FileName = filename:join(LogDir, File),
    Time1 = ?MonoTime,
    {ok, Log} = get_log(Opts),
    Time2 = ?MonoTime,
    put(trace_info, [{'Elapsed time (get_log)', ?time2string(Time2 - Time1)} |
		     get(trace_info)]),
    case validate_dtd(Log) of
	ok ->
	    ok;
	{error, ErrInfo} ->
	    ?LOG_ERR(ErrInfo ++ [{file, FileName}])
    end,
    Time3 = ?MonoTime,
    put(trace_info, [{'Elapsed time (validate_dtd)', ?time2string(Time3 -
								  Time2)} |
		     get(trace_info)]),
    case proplists:get_value(zip, Opts, false) of
	false ->
	    ok = file:write_file(FileName, Log),
	    Time4 = ?MonoTime,
	    put(trace_info, [{'Elapsed time (file:write_file)',
			      ?time2string(Time4 - Time3)} |
			     get(trace_info)]);
	_True ->
	    Ext = proplists:get_value(zipfile_ext, Opts, ".gz"),
	    ZipFile = FileName ++ Ext,
	    Data = iolist_to_binary([Log]),
	    Time4 = ?MonoTime,
	    put(trace_info, [{'Elapsed time (iolist_to_binary)',
			      ?time2string(Time4 - Time3)} |
			     get(trace_info)]),
	    ZipData = zlib:gzip(Data),
	    Time5 = ?MonoTime,
	    put(trace_info, [{'Elapsed time (zip:zip)', ?time2string(Time5 -
								     Time4)} |
			     get(trace_info)]),
	    ok = file:write_file(ZipFile, ZipData),
	    Time6 = ?MonoTime,
	    put(trace_info, [{'Elapsed time (file:write_file)',
			      ?time2string(Time6 - Time5)} |
			     get(trace_info)])
    end,
    ?LOG_INFO([{exported, File} | lists:reverse(get(trace_info))]),
    ok.

validate_dtd(Log) ->
    ScanOpts = [{fetch_path, [sysEnv:dev_patches_dir(),
			      code:priv_dir(alh) ++ "/dtd"]},
		{validation, dtd}],
    try xmerl_scan:string(Log, ScanOpts) of
	{Doc, _Rest} when is_record(Doc, xmlElement) orelse
			  is_record(Doc, xmlDocument) ->
	    ok;
	Error ->
	    ErrInfo =
		[?AVLI_HEADING,
		 {'Validation of XML', 'Format not matching DTD.'},
		 Error],
	    {error, ErrInfo}
    catch
	error : {_, {error, enoent}} ->
	    ?LOG_WARN([?AVLI_HEADING,
		       {'No validation of the XML file performed',
			'DTD file not found.'}]),
	    ok;
	ErrClass : ErrReason ->
	    ErrInfo =
		[?AVLI_HEADING,
		 {'Validation of XML', 'Format not matching DTD.'},
		 {ErrClass, ErrReason}],
	    {error, ErrInfo}
    end.



subscribe() ->
    subscribe(self()).

subscribe(Pid) ->
    gen_server:call(?SERVER, {subscribe, Pid}).


unsubscribe() ->
    unsubscribe(self()).

unsubscribe(Pid) ->
    gen_server:call(?SERVER, {unsubscribe, Pid}).


%called from ecoli-shell
coli_read_records(["-x"]) ->
    alhI:export_log(),
    io:format(?LOG_DIR ++ "/" ++ ?AVAILABILITY_LOG_XML ++ "~n");
coli_read_records(["-sl"]) ->
    print_latest(?DEFAULT_MAX_PRINTOUTS);
coli_read_records(["-sl", Arg]) ->
    try list_to_integer(Arg) of
	Max ->
	    print_latest(Max)
    catch
	_ : _ ->
	    io:format("argument error~n"),
	    exit(error)
    end;
coli_read_records(["-so"]) ->
    print_oldest(?DEFAULT_MAX_PRINTOUTS);
coli_read_records(["-so", Arg]) ->
    try list_to_integer(Arg) of
	Max ->
	    print_oldest(Max)
    catch
	_ : _ ->
	    io:format("argument error~n"),
	    exit(error)
    end;
coli_read_records(["-sf", Arg]) ->
    try list_to_integer(Arg) of
	LogNo ->
	    print_from(LogNo, ?DEFAULT_MAX_PRINTOUTS)
    catch
	_ : _ ->
	    io:format("argument error~n"),
	    exit(error)
    end;
coli_read_records(["-sf", Arg1, Arg2]) ->
    try {list_to_integer(Arg1), list_to_integer(Arg2)} of
	{LogNo, Max} ->
	    print_from(LogNo, Max)
    catch
	_ : _ ->
	    io:format("argument error~n"),
	    exit(error)
    end;
coli_read_records(["-sc", Arg]) ->
    try list_to_integer(Arg) of
	LogNo ->
	    print_clientInfo_from(LogNo, ?DEFAULT_MAX_PRINTOUTS)
    catch
	_ : _ ->
	    io:format("argument error~n"),
	    exit(error)
    end;
coli_read_records(["-sc", Arg1, Arg2]) ->
    try {list_to_integer(Arg1), list_to_integer(Arg2)} of
	{LogNo, Max} ->
	    print_clientInfo_from(LogNo, Max)
    catch
	_ : _ ->
	    io:format("argument error~n"),
	    exit(error)
    end;
coli_read_records(["-f" | Args]) ->
    coli_read_records(Args);
coli_read_records(Args) ->
    {ok, DbLog} = gen_server:call(?SERVER, {get_log, false}),
    filter_and_format(DbLog, Args).

%%%===================================================================
%%% Start and Stop
%%%===================================================================
start() ->
    start([]).


start(Opts) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Opts, []).


start_link() ->
    start_link([]).


start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).


stop() ->
    stop(normal).

stop(Reason) ->
    gen_server:cast(?SERVER, {stop, Reason}).


activate() ->
    gen_server:cast(?SERVER, activate).

cec_setup(Socket) ->
    %% Used for every event request. No need to store Socket in the server
    %% process since it is never used. Only Sockets received in
    %% 'CelloAvli2_initiateService' are used for sending messages back.
    %% (Should be possible to send messages without setting up a Socket each
    %% time, but for the moment - let it be like this!)
    case whereis(?MODULE) of
	Pid when is_pid(Pid) ->
	    Pid;
	_ ->
	    timer:sleep(250),
	    cec_setup(Socket)
    end.

cec_takeover(Socket) ->
    gen_server:cast(?SERVER, {cec_takeover, Socket}).

%%% ###########################################################################
%%% @doc Get the server process state.
%%%
%%% @end
%%% ###=====================================================================###
get_state() ->
    gen_server:call(?SERVER, {?MODULE, get_state}).

%%% ###########################################################################
%%% @doc Print the server process state and information.
%%%
%%% @end
%%% ###=====================================================================###
info() ->
    Msg = {?MODULE, info},
    gen_server:cast(?SERVER, Msg).

%%% ###=====================================================================###
info_all() ->
    [rpc:cast(Node, ?MODULE, info, []) || Node <- clhI:erlang_nodes(all)].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_Opts) ->
    erlang:process_flag(trap_exit, true),
    put(trace_info, []),
    case clhI:core_state() of
	active ->
	    create_log_dir(?LOG_DIR),
	    create_alh_dir(),
	    create_ram_dir(),
	    swmI:copy_upgrWindow_table(?MNESIA_TAB),% SWM keeps track of upgrade
						% window or not. I need to copy
						% what's in there no matter
						% which state we're in now.
	    delete_oldest_objs(),
	    init_eventQ(),
	    self() ! init_continue;
	_ ->
	    ok
    end,
    {ok, #state{data = gb_trees:empty()}}.

%%%===================================================================
init_eventQ() ->
    init_eventQ(sysEnv:rcs_mode_2(),
		swmI:is_upgrade_ongoing() orelse swmI:is_swRestore_ongoing()),
    ?LOG_INFO([{?FILE_UpgradeInfo, file:read_file_info(?FILE_UpgradeInfo)},
	       {ugInfo, get_UgInfo()},
	       {alh_eventQ, ets:info(alh_eventQ, size)},
	       {alh_eventQ_rollback, ets:info(alh_eventQ_rollback, size)}]).

init_eventQ(vrcs, true) ->
    create_rollback_dir(),
    case get_UgInfo(state) of
	?UgState_rollbackStb_traffic ->
	    populateTbl_eventQ_rollback();
	_ ->
	    UgInfo = #{state => ?UgState_rollbackStb_started},
	    file:write_file(?FILE_UpgradeInfo, term_to_binary(UgInfo)),
	    ets:new(alh_eventQ,
		    [named_table, {keypos, 2}, ordered_set, public]),
	    case populateTbl_eventQ_rollback() of
		undefined ->
		    ets:new(alh_eventQ_rollback,
			    [named_table, {keypos, 2}, ordered_set, public]);
		_ ->
		    Evts = ets:tab2list(alh_eventQ_rollback),
		    [ets:insert(alh_eventQ, #alh_eventQ{key = Key,
							obj = Obj})
		     || #alh_eventQ_rollback{key = Key,
					     obj = Obj} <- Evts]
	    end
    end;
init_eventQ(vrcs, false) ->
    create_rollback_dir(),
    case get_UgInfo(state) of
	?UgState_activated ->
	    First = mnesia:dirty_last(?MNESIA_TAB) + 1,
	    consume_eventQ_rollback(),
	    Last = mnesia:dirty_last(?MNESIA_TAB),
	    alhI:print_from(First, Last - First + 1),
	    file:delete(?FILE_UpgradeInfo);
	undefined ->
	    ?LOG_INFO([{ugState, undefined}]),
	    ok;
	UgState ->
	    ?LOG_ERR(["Unexpected state:",
		      {ugState, UgState},
		      {?FILE_EventQRollback,
		       file:read_file_info(?FILE_EventQRollback)},
		      {?FILE_EventQRollback_backup,
		       file:read_file_info(?FILE_EventQRollback_backup)}]),
	    file:delete(?FILE_UpgradeInfo)
    end;
init_eventQ(_, _) ->
    ok.

%%%===================================================================
populateTbl_eventQ_rollback() ->
    case ets:file2tab(?FILE_EventQRollback) of
	{error, _} ->
	    case ets:file2tab(?FILE_EventQRollback_backup) of
		{error, cannot_create_table} ->
		    Size = ets:info(alh_eventQ_rollback, size),
		    ?LOG_WARN([{"Table for EventQRollback already created",
				Size}]),
		    Size;
		{error, _} ->
		    ?LOG_INFO(["No EventQRollback found"]),
		    undefined;
		{ok, _} ->
		    Size = ets:info(alh_eventQ_rollback, size),
		    ?LOG_WARN([{"EventQRollback_backup found", Size}]),
		    Size
	    end;
	{ok, _} ->
	    Size = ets:info(alh_eventQ_rollback, size),
	    ?LOG_INFO([{"EventQRollback found", Size}]),
	    Size
    end.

%%%===================================================================
consume_eventQ_rollback() ->
    populateTbl_eventQ_rollback(),
    case ets:info(alh_eventQ_rollback, size) of
	undefined ->
	    ?LOG_INFO([{alh_eventQ_rollback, undefined},
		       {?MNESIA_TAB, ets:info(?MNESIA_TAB, size)}]);
	Size ->
	    ?LOG_INFO([{alh_eventQ_rollback, Size},
		       {?MNESIA_TAB, ets:info(?MNESIA_TAB, size)}]),
	    F = fun() ->
			consume_eventQ_rollback(ets:first(alh_eventQ_rollback))
		end,
	    mnesia:transaction(F)
    end.

consume_eventQ_rollback(Key) when Key /= '$end_of_table' ->
    [#alh_eventQ_rollback{obj = #?MNESIA_TAB{logRec = LogRec} = Obj}] =
	ets:lookup(alh_eventQ_rollback, Key),
    LogNo = mnesia:last(?MNESIA_TAB) + 1,
    Attr = [{number, itl(LogNo)}],
    mnesia:write(Obj#?MNESIA_TAB{logNo = LogNo,
				 logRec = LogRec#'LogRecord'{attr = Attr}}),
    consume_eventQ_rollback(ets:next(alh_eventQ_rollback, Key));
consume_eventQ_rollback('$end_of_table') ->
    ok.

%%%===================================================================
delete_Rollback() ->
    catch ets:delete(alh_eventQ_rollback),
    file:delete(?FILE_nodeDownTs_rollback),
    file:delete(?FILE_EventQRollback),
    file:delete(?FILE_EventQRollback_backup).

%%%===================================================================
get_UgInfo() ->
    case file:read_file(?FILE_UpgradeInfo) of
	{ok, Binary} ->
	    binary_to_term(Binary);
	_ ->
	    undefined
    end.

%%%===================================================================
get_UgInfo(Key) ->
    case get_UgInfo() of
	undefined ->
	    undefined;
	UgInfo ->
	    maps:get(Key, UgInfo, undefined)
    end.

%%%===================================================================
dump_to_file(File) ->
    dump_to_file(get_UgInfo(), File).

dump_to_file(UgInfo, File) when is_map(UgInfo) ->
    ?LOG_INFO([{ugInfo, UgInfo},
	       ?FILE_UpgradeInfo,
	       file:read_file_info(?FILE_UpgradeInfo)]),
    case maps:get(last_event, UgInfo, undefined) of
	undefined ->
	    ok;
	Key ->
	    NextKey = mnesia:dirty_next(?MNESIA_TAB, Key),
	    List = dump_to_file_read(mnesia:dirty_read(?MNESIA_TAB, NextKey)),
	    ok = file:write_file(File, term_to_binary(List)),
	    SyncRes = os:cmd("sync"),
	    ?LOG_INFO([{keyFirst, NextKey},
		       {keyLast, mnesia:dirty_last(?MNESIA_TAB)},
		       {number_of_objs_dumped, length(List)},
		       {syncRes, SyncRes}]),
	    timer:sleep(1000)
    end;
dump_to_file(_, _) ->
    ok.

dump_to_file_read([#?MNESIA_TAB{logNo = Key} = Obj]) ->
    NextKey = mnesia:dirty_next(?MNESIA_TAB, Key),
    [Obj | dump_to_file_read(mnesia:dirty_read(?MNESIA_TAB, NextKey))];
dump_to_file_read([]) ->
    [].

%%%===================================================================
push_from_file(File) ->
    [NewLast] = mnesia:dirty_read(?MNESIA_TAB, mnesia:dirty_last(?MNESIA_TAB)),
    [NewFirst] =
	mnesia:dirty_read(?MNESIA_TAB, mnesia:dirty_first(?MNESIA_TAB)),
    ?LOG_INFO([{file, File},
	       {newFirst, NewFirst},
	       {newLast, NewLast}]),
    case file:read_file(File) of
	{ok, Old} ->
	    FromOld = binary_to_term(Old),
	    [begin
		 LogNo = mnesia:dirty_last(?MNESIA_TAB) + 1,
		 mnesia:dirty_write(Obj#?MNESIA_TAB{logNo = LogNo})
	     end
	     || Obj <- FromOld],
	    ok = file:write_file(?FilePushedFromOldNode, <<>>);
	FileErr ->
	    ?LOG_ERR([FileErr,
		      {file, File},
		      file:read_file_info(File)])
    end.

%%%===================================================================
handle_call({write_event, Event, Parameters}, From, State) ->
    Res = write_event(Event, Parameters, From, State),
    {reply, Res, State};

handle_call({subscribe, Pid}, _From, State) ->
    erlang:monitor(process, Pid),
    Subscribers = lists:delete(Pid, State#state.subscribers),
    {reply, ok, State#state{subscribers = [Pid | Subscribers]}};

handle_call({unsubscribe, Pid}, _From, State) ->
    Subscribers = lists:delete(Pid, State#state.subscribers),
    {reply, ok, State#state{subscribers = Subscribers}};

handle_call({get_log, Reset}, _From, State) ->
    {reply, {ok, handle_get_log(Reset)}, State};

handle_call(reset_log, _From, State) ->
    reset_db_log(),
    {reply, ok, State};

handle_call(mnesia_sync, _From, State) ->
    T0 = ?MonoTime,
    MSL_Res = mnesia:sync_log(),
    T1 = ?MonoTime,
    ?LOG_INFO([{"mnesia:sync_log() ->", MSL_Res},
	       sysUtil:time_to_string(T1 - T0)]),
    SFAB_Res = swmDbMonitor:force_auto_backup(),
    T2 = ?MonoTime,
    ?LOG_INFO([{"swmDbMonitor:force_auto_backup() ->", SFAB_Res},
	       sysUtil:time_to_string(T2 - T1)]),
    {reply, ok, State};

handle_call({?MODULE, get_state}, _From, State) ->
    {reply, State, State};
handle_call(Command, _From, State) ->
    {reply, {error, {invalid_call, Command}}, State}.

%%%===================================================================
handle_cast({start_traffic, StopTime, From}, State) ->
    ?LOG_INFO([start_traffic, {stopTime, StopTime} | ?PROC_INFO(From)]),
    do_start_traffic(StopTime, State),
    {noreply, State};
handle_cast({write_event_async, Event, Parameters, From}, State) ->
    write_event(Event, Parameters, From, State),
    {noreply, State};

handle_cast({logInfoRecord, Parameters, From}, State) ->
    write_event(logInfoRecord, Parameters, From, State),
    {noreply, State};

handle_cast({warm_restart, {nodeDown, TS}, _From}, State) ->
    NewState = nodeDownTsSet_stopInterval(State),
    {noreply, NewState#state{nodeDownTs_warm_restart = to_datetime(TS)}};

handle_cast({warm_restart, {starting, TS}, From},
	    #state{nodeDownTs_warm_restart = _TsNd} = State) ->
    write_event(logInfoRecord,
		{TS, ?ALH_TAG_RcsNodeRestart},
		From,
		State),
    write_event(?NODE_EVENT,
		{TS,
		 ?ALH_TAG_OutOfService,
		 ?ALH_TAG_Unoperational,
		 0,   % EventId
		 ?ALH_TAG_Rcs([?ALH_TAG_RankWarm,
			       ?ALH_TAG_RcsNodeDown(nodeDownTs_get(State))])},
		From,
		State),
    write_event(?NODE_EVENT,
		{TS,
		 ?ALH_TAG_InService,
		 ?ALH_TAG_Starting,
		 0,   % EventId
		 ?ALH_TAG_Rcs([])},
		From,
		State),
    {noreply, State};

handle_cast({warm_restart, {operational, TS}, From}, State) ->
    write_event(?NODE_EVENT,
		{TS,
		 ?ALH_TAG_InService,
		 ?ALH_TAG_Operational,
		 0,   % EventId
		 ?ALH_TAG_Rcs([])},
		From,
		State),
    comsaServer:avli_other_event(?ALH_TAG_NodeRestarted),
    {noreply, State#state{nodeDownTs_warm_restart = undefined}};

handle_cast(nodeDownTsSet_startInterval, State) ->
    NewState = nodeDownTsSet_startInterval(State),
    {noreply, NewState};

handle_cast({cec_takeover, Socket}, S) ->
    inet:setopts(Socket, [{active, once}]),
    {noreply, S};

handle_cast({stop, Reason}, S) ->
    {stop, Reason, S};

handle_cast(activate, State) ->
    {noreply, State};

handle_cast({?MODULE, info}, State) ->
    error_logger:info_report(?PROC_INFO(self()) ++ ?STATE_INFO(State)),
    {noreply, State};
handle_cast(_Msg, S) ->
    {noreply, S}.

%%%===================================================================
handle_info(?Interval_ts_ram_msg, State) ->
    nodeDownTsSet(?FILE_nodeDownTs_ram),
    case is_RollbackStb_ongoing() of
	false ->
	    ok;
	true ->
	    nodeDownTsSet(?FILE_nodeDownTs_rollback)
    end,
    {noreply, State};

handle_info(?Interval_ts_disk_msg, State) ->
    nodeDownTsSet(?FILE_nodeDownTs_disk),
    {noreply, State};

handle_info({tcp, Socket,
	     <<ClientPid:4/native-unsigned-integer-unit:8,
	       ?AVLI2_INIT_SERVICE:4/native-unsigned-integer-unit:8,
	       Spid:4/native-unsigned-integer-unit:8,
	       PvFirstWanted:4/native-unsigned-integer-unit:8,
	       PvSecondWanted:4/native-unsigned-integer-unit:8,
	       PvThirdWanted:4/native-unsigned-integer-unit:8>>}, State) ->
    put(trace_info, []),
    PidInfo = sysUtil:pid_name(ClientPid),
    NewState = 'CelloAvli2_initiateService'({Socket,
					     Spid,
					     PvFirstWanted,
					     PvSecondWanted,
					     PvThirdWanted}, State),
    Inet_setopts = inet:setopts(Socket, [{active, once}]),
    ?LOG_INFO([{received, 'CelloAvli2_initiateService'} | PidInfo] ++
	      get(trace_info) ++
	      [{inet_setopts, Inet_setopts}]),
    {noreply, NewState};

handle_info({tcp, Socket,
	     <<ClientPid:4/native-unsigned-integer-unit:8,
	       ?AVLI2_WRITE_PIU_EVENT:4/native-unsigned-integer-unit:8,
	       TimeStampB:4/native-unsigned-integer-unit:8,
	       ServiceStatusB:4/native-unsigned-integer-unit:8,
	       ReasonB:4/native-unsigned-integer-unit:8,
	       PiuTypeB:4/native-unsigned-integer-unit:8,
	       PiuHwAddrDefined:2/native-unsigned-integer-unit:8,
	       Smn:4/native-unsigned-integer-unit:8,
	       Apn:4/native-unsigned-integer-unit:8,
	       Ern:4/native-unsigned-integer-unit:8,
	       HwPidDefined:2/native-unsigned-integer-unit:8,
	       ProdNumB:?MAX_PROD_NUM_LEN/binary,
	       ProdRevB:?MAX_PROD_REV_LEN/binary,
	       ProdNameB:?MAX_PROD_NAME_LEN/binary,
	       ProdDateB:?MAX_PROD_DATE_LEN/binary,
	       SerialNumB:?MAX_SERIAL_NUM_LEN/binary,
	       AddInfoLen:2/native-unsigned-integer-unit:8,
	       AddInfoB:AddInfoLen/binary,
	       ClientRef:4/native-unsigned-integer-unit:8>>}, State) ->
    TimeStamp = to_datetime(TimeStampB),
    ServiceStatus = to_status(ServiceStatusB),
    Reason = to_reason(ReasonB),
    PiuType = to_piu_type(PiuTypeB),
    PiuHwAddr = to_piu_hw_addr(PiuHwAddrDefined, {Smn, Apn, Ern}),
    ProdNum = to_string(ProdNumB),
    ProdRev = to_string(ProdRevB),
    ProdName = to_string(ProdNameB),
    ProdDate = to_string(ProdDateB),
    SerialNum = to_string(SerialNumB),
    HwPid = to_hw_pid(HwPidDefined,
		      {ProdNum, ProdRev, ProdName, ProdDate, SerialNum}),
    AddInfo = to_string(AddInfoB),
    Res = write_event(?PIU_EVENT,
		      {TimeStamp, ServiceStatus, Reason, PiuType, PiuHwAddr,
		       HwPid, AddInfo},
		      ClientPid,
		      State),
    Cmd = ?CELLO_AVLI_WRITE_PIU_EVENT,
    send_write_confirm(Cmd, ClientRef, Res, Socket, State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp, Socket,
	     <<ClientPid:4/native-unsigned-integer-unit:8,
	       ?AVLI2_WRITE_HW_EVENT:4/native-unsigned-integer-unit:8,
	       TimeStampB:4/native-unsigned-integer-unit:8,
	       ServiceStatusB:4/native-unsigned-integer-unit:8,
	       ReasonB:4/native-unsigned-integer-unit:8,
	       HwTypeB:?MAX_HW_TYPE_LEN/binary,
	       HwAddrB:?MAX_HW_ADDR_LEN/binary,
	       HwPidDefined:2/native-unsigned-integer-unit:8,
	       ProdNumB:?MAX_PROD_NUM_LEN/binary,
	       ProdRevB:?MAX_PROD_REV_LEN/binary,
	       ProdNameB:?MAX_PROD_NAME_LEN/binary,
	       ProdDateB:?MAX_PROD_DATE_LEN/binary,
	       SerialNumB:?MAX_SERIAL_NUM_LEN/binary,
	       AddInfoLen:2/native-unsigned-integer-unit:8,
	       AddInfoB:AddInfoLen/binary,
	       ClientRef:4/native-unsigned-integer-unit:8>>}, State) ->
    TimeStamp = to_datetime(TimeStampB),
    ServiceStatus = to_status(ServiceStatusB),
    Reason = to_reason(ReasonB),
    HwType = to_string(HwTypeB),
    HwAddr = to_string(HwAddrB),
    ProdNum = to_string(ProdNumB),
    ProdRev = to_string(ProdRevB),
    ProdName = to_string(ProdNameB),
    ProdDate = to_string(ProdDateB),
    SerialNum = to_string(SerialNumB),
    HwPid = to_hw_pid(HwPidDefined,
		      {ProdNum, ProdRev, ProdName, ProdDate, SerialNum}),
    AddInfo = to_string(AddInfoB),
    Res = write_event(?HW_EVENT,
		      {TimeStamp, ServiceStatus, Reason, HwType,
		       HwAddr, HwPid, AddInfo},
		      ClientPid,
		      State),
    Cmd = ?CELLO_AVLI_WRITE_HW_EVENT,
    send_write_confirm(Cmd, ClientRef, Res, Socket, State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp, Socket,
	     <<ClientPid:4/native-unsigned-integer-unit:8,
	       ?AVLI5_WRITE_HW_EVENT:4/native-unsigned-integer-unit:8,
	       TimeStampB:4/native-unsigned-integer-unit:8,
	       ServiceStatusB:4/native-unsigned-integer-unit:8,
	       ReasonB:4/native-unsigned-integer-unit:8,
	       HwTypeB:?MAX_HW_TYPE_LEN/binary,
	       HwAddrB:?MAX_HW_ADDR_LEN/binary,
	       HwPidDefined:2/native-unsigned-integer-unit:8,
	       ProdNumB:?MAX_PROD_NUM_LEN/binary,
	       ProdRevB:?MAX_PROD_REV_LEN/binary,
	       ProdNameB:?MAX_AVLI_PROD_NAME_LEN/binary,
	       ProdDateB:?MAX_PROD_DATE_LEN/binary,
	       SerialNumB:?MAX_SERIAL_NUM_LEN/binary,
	       AddInfoLen:2/native-unsigned-integer-unit:8,
	       AddInfoB:AddInfoLen/binary,
	       ClientRef:4/native-unsigned-integer-unit:8>>}, State) ->
    TimeStamp = to_datetime(TimeStampB),
    ServiceStatus = to_status(ServiceStatusB),
    Reason = to_reason(ReasonB),
    HwType = to_string(HwTypeB),
    HwAddr = to_string(HwAddrB),
    ProdNum = to_string(ProdNumB),
    ProdRev = to_string(ProdRevB),
    ProdName = to_string(ProdNameB),
    ProdDate = to_string(ProdDateB),
    SerialNum = to_string(SerialNumB),
    HwPid = to_hw_pid(HwPidDefined,
		      {ProdNum, ProdRev, ProdName, ProdDate, SerialNum}),
    AddInfo = to_string(AddInfoB),
    Res = write_event(?HW5_EVENT,
		      {TimeStamp, ServiceStatus, Reason, HwType,
		       HwAddr, HwPid, AddInfo},
		      ClientPid,
		      State),
    Cmd = ?CELLO_AVLI_WRITE_HW5_EVENT,
    send_write_confirm(Cmd, ClientRef, Res, Socket, State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp, Socket,
	     <<ClientPid:4/native-unsigned-integer-unit:8,
	       ?AVLI2_WRITE_SERVICE_EVENT:4/native-unsigned-integer-unit:8,
	       TimeStampB:4/native-unsigned-integer-unit:8,
	       ServiceStatusB:4/native-unsigned-integer-unit:8,
	       ReasonB:4/native-unsigned-integer-unit:8,
	       ServiceTypeB:?MAX_SERV_TYPE_LEN/binary,
	       ServiceInstB:?MAX_SERV_INST_LEN/binary,
	       AddInfoLen:2/native-unsigned-integer-unit:8,
	       AddInfoB:AddInfoLen/binary,
	       ClientRef:4/native-unsigned-integer-unit:8>>}, State) ->
    TimeStamp = to_datetime(TimeStampB),
    ServiceStatus = to_status(ServiceStatusB),
    Reason = to_reason(ReasonB),
    ServiceType = to_string(ServiceTypeB),
    ServiceInst = to_string(ServiceInstB),
    AddInfo = to_string(AddInfoB),
    Res = write_event(?SERVICE_EVENT,
		      {TimeStamp, ServiceStatus, Reason, ServiceType,
		       ServiceInst, AddInfo},
		      ClientPid,
		      State),
    Cmd = ?CELLO_AVLI_WRITE_SERVICE_EVENT,
    send_write_confirm(Cmd, ClientRef, Res, Socket, State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp, Socket,
	     <<ClientPid:4/native-unsigned-integer-unit:8,
	       ?AVLI2_WRITE_OTHER_EVENT:4/native-unsigned-integer-unit:8,
	       TimeStampB:4/native-unsigned-integer-unit:8,
	       ServiceStatusB:4/native-unsigned-integer-unit:8,
	       ReasonB:4/native-unsigned-integer-unit:8,
	       AvailInfoLen:2/native-unsigned-integer-unit:8,
	       AvailInfoB:AvailInfoLen/binary,
	       ClientRef:4/native-unsigned-integer-unit:8>>}, State) ->
    TimeStamp = to_datetime(TimeStampB),
    ServiceStatus = to_status(ServiceStatusB),
    Reason = to_reason(ReasonB),
    AvailInfo = to_string(AvailInfoB),
    Res = write_event(?OTHER_EVENT,
		      {TimeStamp, ServiceStatus, Reason, AvailInfo},
		      ClientPid,
		      State),
    Cmd = ?CELLO_AVLI_WRITE_OTHER_EVENT,
    send_write_confirm(Cmd, ClientRef, Res, Socket, State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp, Socket,
	     <<ClientPid:4/native-unsigned-integer-unit:8,
	       ?AVLI3_WRITE_PGM_EVENT:4/native-unsigned-integer-unit:8,
	       TimeStampB:4/native-unsigned-integer-unit:8,
	       ServiceStatusB:4/native-unsigned-integer-unit:8,
	       ReasonB:4/native-unsigned-integer-unit:8,
	       PiuTypeB:4/native-unsigned-integer-unit:8,
	       PiuHwAddrDefined:2/native-unsigned-integer-unit:8,
	       Smn:4/native-unsigned-integer-unit:8,
	       Apn:4/native-unsigned-integer-unit:8,
	       Ern:4/native-unsigned-integer-unit:8,
	       SwPidDefined:2/native-unsigned-integer-unit:8,
	       ProdNumB:?MAX_PROD_NUM_LEN/binary,
	       ProdRevB:?MAX_PROD_REV_LEN/binary,
	       ProdNameB:?MAX_PROD_NAME_LEN/binary,
	       ProdDateB:?MAX_PROD_DATE_LEN/binary,
	       AddInfoLen:2/native-unsigned-integer-unit:8,
	       AddInfoB:AddInfoLen/binary,
	       ClientRef:4/native-unsigned-integer-unit:8>>}, State) ->
    TimeStamp = to_datetime(TimeStampB),
    ServiceStatus = to_status(ServiceStatusB),
    Reason = to_reason(ReasonB),
    PiuType = to_piu_type(PiuTypeB),
    PiuHwAddr = to_piu_hw_addr(PiuHwAddrDefined, {Smn, Apn, Ern}),
    ProdNum = to_string(ProdNumB),
    ProdRev = to_string(ProdRevB),
    ProdName = to_string(ProdNameB),
    ProdDate = to_string(ProdDateB),
    SwPid = to_sw_pid(SwPidDefined,
		      {ProdNum, ProdRev, ProdName, ProdDate}),
    AddInfo = to_string(AddInfoB),
    Res = write_event(?PGM_EVENT,
		      {TimeStamp, ServiceStatus, Reason,
		       PiuType, PiuHwAddr, SwPid, AddInfo},
		      ClientPid,
		      State),
    Cmd = ?CELLO_AVLI_WRITE_PGM_EVENT,
    send_write_confirm(Cmd, ClientRef, Res, Socket, State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp, Socket,
	     <<ClientPid:4/native-unsigned-integer-unit:8,
	       ?AVLI4_WRITE_NODE_EVENT:4/native-unsigned-integer-unit:8,
	       TimeStampB:4/native-unsigned-integer-unit:8,
	       ServiceStatusB:4/native-unsigned-integer-unit:8,
	       ReasonB:4/native-unsigned-integer-unit:8,
	       EventIdB:4/native-unsigned-integer-unit:8,
	       AddInfoLen:2/native-unsigned-integer-unit:8,
	       AddInfoB:AddInfoLen/binary,
	       ClientRef:4/native-unsigned-integer-unit:8>>}, State) ->
    TimeStamp = to_datetime(TimeStampB),
    ServiceStatus = to_status(ServiceStatusB),
    Reason = to_reason(ReasonB),
    EvtId = EventIdB,
    AddInfo = to_string(AddInfoB),
    Res = write_event(?NODE_EVENT,
		      {TimeStamp, ServiceStatus, Reason, EvtId, AddInfo},
		      ClientPid,
		      State),
    Cmd = ?CELLO_AVLI_WRITE_NODE_EVENT,
    send_write_confirm(Cmd, ClientRef, Res, Socket, State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp_closed, Socket}, State) ->
    ItcPort =
	try
	    {_Spid, ItcPort_Value} = gb_trees:get(Socket, State#state.data),
	    ItcPort_Value
	catch
	    _ : _ ->
		undefined
	end,
    NewData = gb_trees:delete(Socket, State#state.data),
    itc_close(ItcPort),
    NewState = State#state{data = NewData},
    {noreply, NewState};

handle_info({tcp, Socket, _Data}, State) ->
    ?ALH_TRACE("Unexpected tcp Data: ~p~n", [_Data]),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info(init_continue, State) ->
    TS = os:timestamp(),
    State1 = nodeDownTs_update(State),
    write_event(logInfoRecord,
		{TS, ?ALH_TAG_RcsNodeRestart},
		self(),
		State1),
    try
	ok = comsaI:subscribe_timestep(makeFun_timeChange())
    catch
	ErrClass : ErrReason ->
	    Stacktrace = ?STACKTRACE_E,
	    ?LOG_ERR([{'[AVLI] timestep subscription failed',
		       'Logging of RcsTimeChange disabled'},
		      {comsaI, subscribe_timestep},
		      {ErrClass, [ErrReason | Stacktrace]}])
    end,
    AddInfo_NodeDown =
	lists:flatten([appmServer:avli_get_rank(),
		       ?ALH_TAG_RcsNodeDown(nodeDownTs_get(State1))]),
    write_event(?NODE_EVENT,
		{TS,
		 ?ALH_TAG_OutOfService,
		 ?ALH_TAG_Unoperational,
		 0,   % EventId
		 ?ALH_TAG_Rcs(AddInfo_NodeDown)},
		self(),
		State1),
    AddInfo_Starting =
	case swmI:is_upgrade_ongoing() of
	    true ->
		?ALH_TAG_Cause(?ALH_TAG_ExtUpgradeRequest);
	    false ->
		[]
	end,
    write_event(?NODE_EVENT,
		{TS,
		 ?ALH_TAG_InService,
		 ?ALH_TAG_Starting,
		 0,   % EventId
		 ?ALH_TAG_Rcs(AddInfo_Starting)},
		self(),
		State1),
    case get_UgInfo(state) of
	undefined ->
	    delete_Rollback();
	_ ->
	    ok
    end,
    {noreply, State1};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};

handle_info({'DOWN', _Ref, _Type, Pid, _Info}, State) ->
    Subscribers = lists:delete(Pid, State#state.subscribers),
    {noreply, State#state{subscribers = Subscribers}};

handle_info(#'LogRecord'{} = LogRec, State) ->
    ?ALH_TRACE("~p~n~n", [LogRec]),
    XML = lists:flatten(xmerl:export_simple_content([LogRec], xmerl_xml)),
    ?ALH_TRACE("~s~n~n", [XML]),
    {noreply, State};

handle_info(_Info, State) ->
    ?ALH_TRACE("Unexpected Info: ~p~n", [_Info]),
    {noreply, State}.

%%%===================================================================
nodeDownTsSet(FileName) ->
    case
	file:write_file_info(FileName,
			     #file_info{mtime =
					now_to_seconds(os:timestamp()) + 1},
			     [{time, posix}])
	of
	ok ->
	    ok;
	{error, enoent} ->
	    file:write_file(FileName, <<>>),
	    ok;
	Error ->
	    Stacktrace = ?STACKTRACE_C,
	    ?LOG_ERR([{'file:write_file_info', Error} | Stacktrace]),
	    Error
    end.

%%%===================================================================
nodeDownTsSet_startInterval(#state{nodeDownTs_IntervalRef = undefined}
			      = State) ->
    {ok, TRefRam} =
	timer:send_interval(?Interval_ts_ram, ?Interval_ts_ram_msg),
    {ok, TRefDisk} =
	timer:send_interval(?Interval_ts_disk, ?Interval_ts_disk_msg),
    State#state{nodeDownTs_IntervalRef = #tRefs{ram = TRefRam,
						disk = TRefDisk}};
nodeDownTsSet_startInterval(State) ->
    State.

%%%===================================================================
nodeDownTsSet_stopInterval(#state{nodeDownTs_IntervalRef
				  = #tRefs{ram = RamRef, disk = DiskRef}}
			 = State) ->
    timer:cancel(RamRef),
    timer:cancel(DiskRef),
    State#state{nodeDownTs_IntervalRef = undefined};
nodeDownTsSet_stopInterval(State) ->
    State.

%%%===================================================================
nodeDownTsSet_startInterval(#state{nodeDownTs_IntervalRef = InRef}, _)
  when InRef /= undefined ->
    ok;
nodeDownTsSet_startInterval(_, Obj) ->
    case is_RestartCompleted(Obj) of
	false ->
	    ok;
	true ->
	    nodeDownTsSet(?FILE_nodeDownTs_ram),
	    nodeDownTsSet(?FILE_nodeDownTs_disk),
	    sysInitI:info_report([{?MODULE, ?FUNCTION},
				  {avli, ?ALH_TAG_RestartCompleted},
				  "The node is now available for traffic."]),
	    gen_server:cast(?SERVER, nodeDownTsSet_startInterval),
        case sysEnv:rcs_mode_2() of
            simulated -> ok;
            _ -> pesServer:restartEventJobs() %%HV23638
        end
    end.

%%%===================================================================
now_to_seconds({MegSec, Sec, MicrSec}) ->
    if
	MicrSec >= 500000 ->
	    (MegSec * 1000000) + Sec + 1;
	?ELSE ->
	    (MegSec * 1000000) + Sec
    end.

%%%===================================================================
terminate(_Reason, State) ->
    ok = data_foreach(
	   fun({Socket, {Spid, ItcPort}}) ->
		   send_avli_server_down_ind({ItcPort, Spid}),
		   itc_close(ItcPort),
		   gen_tcp:close(Socket)
	   end, State#state.data),
    ok.

%%%===================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
do_start_traffic(StopTime, State) ->
    First = mnesia:dirty_last(?MNESIA_TAB) + 1,
    R1 = replace_nodeDown(StopTime),
    R2 = consume_eventQ(State),
    R3 = file:write_file(?FILE_UpgradeInfo,
			 term_to_binary(#{state =>
					  ?UgState_rollbackStb_traffic})),
    Last = mnesia:dirty_last(?MNESIA_TAB),
    ?LOG_INFO([{replace_nodeDown, R1},
	       {consume_eventQ, R2},
	       {'file:write_file', R3},
	       {first, First},
	       {last, Last}]),
    alhI:print_from(First, Last - First + 1),
    State.

%%%===================================================================
consume_eventQ(State) ->
    F = fun() ->
		consume_eventQ(ets:lookup(alh_eventQ, ets:first(alh_eventQ)),
			       State)
	end,
    mnesia:transaction(F).

consume_eventQ([#alh_eventQ{key = Key,
			    obj = #?MNESIA_TAB{logRec = LogRec} = Obj}],
	       State) ->
    LogNo = mnesia:last(?MNESIA_TAB) + 1,
    Attr = [{number, itl(LogNo)}],
    mnesia:write(Obj#?MNESIA_TAB{logNo = LogNo,
				 logRec = LogRec#'LogRecord'{attr = Attr}}),
    consume_eventQ(ets:lookup(alh_eventQ, ets:next(alh_eventQ, Key)), State);
consume_eventQ(_, _) ->
    catch ets:delete(alh_eventQ).

%%%===================================================================
replace_nodeDown(undefined) ->
    ok;
replace_nodeDown(StopTime) ->
    replace_nodeDown(ets:first(alh_eventQ), StopTime).

replace_nodeDown(Key, StopTime) when Key /= '$end_of_table' ->
    case ets:lookup(alh_eventQ, Key) of
	[#alh_eventQ{obj =
		     #?MNESIA_TAB{logRec = #'LogRecord'{val = Evt}} = Obj}] ->
	    case keyfind_recursive('RcsNodeDown', Evt) of
		false ->
		    replace_nodeDown(ets:next(alh_eventQ, Key), StopTime);
		?ALH_TAG_RcsNodeDown([{'DownTime', ['Unknown']}]) ->
		    ?LOG_INFO(["No previous up time. Nothing replaced."]),
		    ok;
		?ALH_TAG_RcsNodeDown([{'DownTime', _}]) ->
		    EvtTS = timestamp_xml(os:timestamp()),
		    TS = time_xml(StopTime),
		    NewNodeDown =
			?ALH_TAG_RcsNodeDown([{'DownTime', TS}]),
		    Evt1 =
			keyreplace_recursive('TimeStamp', Evt, EvtTS),
		    Evt2 =
			keyreplace_recursive('RcsNodeDown', Evt1, NewNodeDown),
		    NewObj =
			Obj#?MNESIA_TAB{logRec = #'LogRecord'{val = Evt2}},
		    NewKey = {get_ts_gs(NewObj), os:system_time()},
		    ets:delete(alh_eventQ, Key),
		    ets:delete(alh_eventQ_rollback, Key),
		    write_logrecord_q(NewKey, NewObj),
		    write_logrecord_rollback(NewKey, NewObj)
	    end;
	_ ->
	    replace_nodeDown(ets:next(alh_eventQ, Key), StopTime)
    end;
replace_nodeDown('$end_of_table', _) ->
    ok.

%%% ###=====================================================================###
keyfind_recursive(Key, [{Key, _} = Prop | _]) ->
    Prop;
keyfind_recursive(Key, [{_, Value} | Tail]) ->
    case keyfind_recursive(Key, Value) of
	false ->
	    keyfind_recursive(Key, Tail);
	Prop ->
	    Prop
    end;
keyfind_recursive(Key, [_ | Tail]) ->
    keyfind_recursive(Key, Tail);
keyfind_recursive(_, []) ->
    false.

%%% ###=====================================================================###
keyreplace_recursive(Key, [{Key, _} = OldTuple | Tail], NewTuple) ->
    ?LOG_INFO([{replaced, OldTuple},
	       {with, NewTuple}]),
    [NewTuple | keyreplace_recursive(Key, Tail, NewTuple)];
keyreplace_recursive(Key, [{OtherKey, Value} | Tail], NewTuple) ->
    [{OtherKey, keyreplace_recursive(Key, Value, NewTuple)}
     | keyreplace_recursive(Key, Tail, NewTuple)];
keyreplace_recursive(Key, [Other | Tail], NewTuple) ->
    [Other | keyreplace_recursive(Key, Tail, NewTuple)];
keyreplace_recursive(_, [], _) ->
    [].

%%%===================================================================
nodeDownTs_get(#state{nodeDownTs = LogTS}) ->
    FileTS = nodeDownTs_get_file(),
    AppmTS =
	try appmServer:legacy_nodeDown_ts_erase() of
	    undefined ->
		undefined;
	    AppmTS_Tmp ->
		AppmTS_DateTime = to_datetime(AppmTS_Tmp),
		case AppmTS_DateTime >= LogTS of
		    true ->
			AppmTS_DateTime;
		    false ->
			LogTS
		end
	catch
	    ErrClass : ErrReason ->
		Stacktrace = ?STACKTRACE_E,
		?LOG_ERR([{ErrClass, ErrReason} | Stacktrace]),
		undefined
	end,
    nodeDownTs_get(FileTS, AppmTS).

%%%===================================================================
nodeDownTs_get_file() ->
    RollbackTS =
	case file:read_file_info(?FILE_nodeDownTs_rollback) of
	    {ok, #file_info{mtime = MTimeRollback}} ->
		MTimeRollback;
	    {error, enoent} ->
		undefined;
	    ErrorRollback ->
		?LOG_ERR([?FILE_nodeDownTs_rollback,
			  ErrorRollback
			  | ?STACKTRACE_C]),
		undefined
	end,
    RamTS =
	case file:read_file_info(?FILE_nodeDownTs_ram) of
	    {ok, #file_info{mtime = MTimeRam}} ->
		MTimeRam;
	    {error, enoent} ->
		undefined;
	    ErrorRam ->
		?LOG_ERR([?FILE_nodeDownTs_ram, ErrorRam | ?STACKTRACE_C]),
		undefined
	end,
    DiskTS =
	case file:read_file_info(?FILE_nodeDownTs_disk) of
	    {ok, #file_info{mtime = MTimeDisk}} ->
		MTimeDisk;
	    {error, enoent} ->
		undefined;
	    ErrorDisk ->
		?LOG_ERR([?FILE_nodeDownTs_disk, ErrorDisk | ?STACKTRACE_C]),
		undefined
	end,
    lists:last(lists:sort([RollbackTS, RamTS, DiskTS])).

%%%===================================================================
nodeDownTs_get(FileTS, AppmTS) when FileTS > AppmTS ->
    ?ALH_TAG_DownTime(FileTS);
nodeDownTs_get(undefined, undefined) ->
    %% Initial start.
    ?ALH_TAG_DownTime_Unknown;
nodeDownTs_get(FileTS, AppmTS) when FileTS < AppmTS ->
    %% Upgrade from UP with old mnesia solution in APPM.
    file:write_file(?FILE_nodeDownTs_disk, <<>>),
    file:write_file_info(?FILE_nodeDownTs_disk, #file_info{mtime = AppmTS}),
    ?ALH_TAG_DownTime(AppmTS);
nodeDownTs_get(FileTS, _) ->
    ?ALH_TAG_DownTime(FileTS).

%%%===================================================================
%% TR HT50597:
%% Due to a fault observed intermittently during software upgrade, we need to
%% find the best possible timestamp for 'downTime'. The timestamp received here
%% from APPM is too old when this fault has occurred. The fault is, in short,
%% that ets:tab2file in SWM fails during node takedown. When the node comes up
%% again, the file (that contains the APPM timestamp for 'downTime') is corrupt.
%% This is a workaround for the actual fault (OTP disk writing problem).
%% << 2015-09-23: Moved nodeDown timestamp from mnesia to file under /rcs/alh.
%% This makes the above problem obsolete, but mechanism is needed anyway for
%% backwards and forward moves between UP:s - upgrade & restore.>>
nodeDownTs_update(State) ->
    case ets:lookup(?MNESIA_TAB, ets:last(?MNESIA_TAB)) of
	[#?MNESIA_TAB{logRec = #'LogRecord'{val = Values}}] ->
	    case proplists:get_value('TimeStamp', Values) of
		TSList when is_list(TSList) ->
		    State#state{nodeDownTs = to_datetime(TSList)};
		undefined ->
		    State
	    end;
	_ ->
	    State
    end.

%%%===================================================================
is_RestartCompleted(#?MNESIA_TAB{logRec = #'LogRecord'{val = Values}}) ->
    try lists:keyfind('RecordContent', 1, Values) of
	{_, Content} ->
	    is_RestartCompleted(Content);
	_ ->
	    false
    catch
	_ : _ ->
	    false
    end;
is_RestartCompleted([{_, Content} | Tail]) ->
    case is_RestartCompleted(Content) of
	true ->
	    true;
	false ->
	    is_RestartCompleted(Tail)
    end;
is_RestartCompleted([?ALH_TAG_RestartCompleted | _]) ->
    true;
is_RestartCompleted([Tag | Tail]) when is_atom(Tag) ->
    is_RestartCompleted(Tail);
is_RestartCompleted(_) ->
    false.

%%%===================================================================
is_RollbackStb_ongoing() ->
    case ets:info(alh_eventQ_rollback) of
	undefined ->
	    false;
	_ ->
	    true
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%===================================================================
%%% General server functions
%%%===================================================================
handle_get_log(Reset) ->
    Log = ets:tab2list(?MNESIA_TAB),
    Reset == false orelse reset_db_log(),
    Log.

%%%===================================================================
print_from(LogNo, Max) ->
    [print_row(mnesia:dirty_read(?MNESIA_TAB, Key)) ||
	Key <- collect_from(LogNo, Max)].

print_clientInfo_from(LogNo, Max) ->
    [print_row_client(mnesia:dirty_read(?MNESIA_TAB, Key)) ||
	Key <- collect_from(LogNo, Max)].

%%%===================================================================
print_latest(Max) ->
    [print_row(mnesia:dirty_read(?MNESIA_TAB, Key)) ||
	Key <- collect_latest(Max)].

%%%===================================================================
print_oldest(Max) ->
    [print_row(mnesia:dirty_read(?MNESIA_TAB, Key)) ||
	Key <- collect_oldest(Max)].

%%%===================================================================
collect_from(Key, Max) when Key /= 0 ->
    [Key | collect_next(Max - 1, Key)];
collect_from(0, Max) ->
    collect_next(Max - 1, 0).

collect_latest(Max) ->
    lists:reverse(case mnesia:dirty_last(?MNESIA_TAB) of
		      Key when Key /= 0 ->
			  [Key | collect_prev(Max - 1, Key)];
		      _ ->
			  []
		  end).

collect_oldest(Max) ->
    case mnesia:dirty_next(?MNESIA_TAB,
			   mnesia:dirty_first(?MNESIA_TAB)) of
	Key when is_integer(Key) ->
	    [Key | collect_next(Max - 1, Key)];
	'$end_of_table' ->
	    []
    end.

collect_prev(Cnt, PrevKey) when Cnt > 0 ->
    case mnesia:dirty_prev(?MNESIA_TAB, PrevKey) of
	Key when Key /= 0 ->
	    [Key | collect_prev(Cnt - 1, Key)];
	_ ->
	    []
    end;
collect_prev(_, _) ->
    [].

collect_next(Cnt, PrevKey) when Cnt > 0 ->
    case mnesia:dirty_next(?MNESIA_TAB, PrevKey) of
	Key when is_integer(Key) ->
	    [Key | collect_next(Cnt - 1, Key)];
	'$end_of_table' ->
	    []
    end;
collect_next(_, _) ->
    [].

print_row([#?MNESIA_TAB{logNo = LogNo, logRec = LogRec}]) ->
    Row =
	integer_to_list(LogNo) ++ "  " ++
	format_LogRecord(LogRec) ++ " ",
    io:format("~s~n", [Row]),
    LogNo;
print_row([]) ->
    undefined.

print_row_client([#?MNESIA_TAB{logNo = LogNo, clientInfo = ClientInfo}]) ->
    Row =
	integer_to_list(LogNo) ++ "  " ++
	io_lib:format("~w", [ClientInfo]),
    io:format("~s~n", [Row]),
    LogNo;
print_row_client([]) ->
    undefined.

format_LogRecord([{'TimeStamp', Val} | Tail]) ->
    format_time(Val) ++ " " ++ format_LogRecord(Tail);
format_LogRecord([{'RecordContent', Val} | Tail]) ->
    format_content(Val) ++ " " ++ format_LogRecord(Tail);
format_LogRecord([_ | Tail]) ->
    format_LogRecord(Tail);
format_LogRecord(#'LogRecord'{val = Content}) ->
    format_LogRecord(Content);
format_LogRecord([]) ->
    " ".

format_time({Attr, TimeAttrs}) when is_atom(Attr) ->
    format_time(TimeAttrs);
format_time(TimeAttrs) ->
    format_year(TimeAttrs) ++ "-" ++
	format_month(TimeAttrs) ++ "-" ++
	format_day(TimeAttrs) ++ " " ++
	format_hour(TimeAttrs) ++ ":" ++
	format_minute(TimeAttrs) ++ ":" ++
	format_second(TimeAttrs) ++ " ".

format_content([{'LogInfoRecord', [Val]} | Tail]) ->
    [format_LogInfoRecord(Val) | format_content(Tail)];
format_content([{'RcsNodeDown' = Tag, [Time]} | Tail]) ->
    [format_tag(Tag) ++ " " ++ format_time(Time) ++ " " |
     format_content(Tail)];
format_content([{'RcsNodeIdentityInfo' = Tag, Val} | Tail]) ->
    [format_tag(Tag) ++ format_RcsNodeIdentityInfo(Val) |
     format_content(Tail)];
format_content([{Tag, Val} | Tail]) when is_atom(Tag) ->  
    [format_tag(Tag) ++ format_content(Val) | format_content(Tail)];
format_content([Tag | Tail]) when is_atom(Tag) ->
    [format_tag(Tag) | format_content(Tail)];
format_content([Str | Tail]) when is_list(Str) ->  
    [unpad(Str) ++ " " | format_content(Tail)];
format_content([_ | Tail]) ->
    format_content(Tail);
format_content([]) ->
    "".

format_LogInfoRecord(Val) when is_atom(Val) ->
    atom_to_list(Val);
format_LogInfoRecord({'RcsTimeChange' = Attr, [OldTime, NewTime]}) ->
    atom_to_list(Attr) ++ "  " ++
	format_time(OldTime) ++ " " ++
	format_time(NewTime);
format_LogInfoRecord(_) ->
    "".

format_RcsNodeIdentityInfo([{'NodeIdReason', [Str]} | Tail]) ->
    [unpad(Str) ++ " " | format_RcsNodeIdentityInfo(Tail)];
format_RcsNodeIdentityInfo([{'UpgradePackage', [Str]} | Tail]) ->
    [unpad(Str) ++ " " | format_RcsNodeIdentityInfo(Tail)];
format_RcsNodeIdentityInfo([_ | Tail]) ->
    format_RcsNodeIdentityInfo(Tail);
format_RcsNodeIdentityInfo([]) ->
    "".

format_tag(Tag)
  when Tag == 'AdditionalInfo' orelse
       Tag == 'AvailabilityInfo' orelse
       Tag == 'Cause' orelse
       Tag == 'EventReason' orelse
       Tag == 'HwPid' orelse
       Tag == 'SwPid' orelse
       Tag == 'ProdNo' orelse
       Tag == 'ProdRev' orelse
       Tag == 'ProdName' orelse
       Tag == 'PiuAddress' orelse
       Tag == 'SwitchModuleNumber' orelse
       Tag == 'SwitchPortNumber' orelse
       Tag == 'PiuInfo' orelse
       Tag == 'NodeInfo' ->
    "";
format_tag(Tag) ->
    atom_to_list(Tag) ++ " ".

format_year([{year, [Str]} | _]) ->
    unpad(Str);
format_year([_ | Tail]) ->
    format_year(Tail);
format_year([]) ->
    "0000".

format_month([{month, [Str]} | _]) ->
    format_length(2, unpad(Str));
format_month([_ | Tail]) ->
    format_month(Tail);
format_month([]) ->
    "00".

format_day([{day, [Str]} | _]) ->
    format_length(2, unpad(Str));
format_day([_ | Tail]) ->
    format_day(Tail);
format_day([]) ->
    "00".

format_hour([{hour, [Str]} | _]) ->
    format_length(2, unpad(Str));
format_hour([_ | Tail]) ->
    format_hour(Tail);
format_hour([]) ->
    "00".

format_minute([{minute, [Str]} | _]) ->
    format_length(2, unpad(Str));
format_minute([_ | Tail]) ->
    format_minute(Tail);
format_minute([]) ->
    "00".

format_second([{second, [Str]} | _]) ->
    format_length(2, unpad(Str));
format_second([_ | Tail]) ->
    format_second(Tail);
format_second([]) ->
    "00".

unpad([$  | Tail]) ->
    unpad(Tail);
unpad("") ->
    "";
unpad(Str) ->
    case lists:last(Str) of
	$  ->
	    unpad(lists:sublist(Str, length(Str) - 1));
	_ ->
	    Str
    end.

format_length(Len, Str) ->
    lists:nthtail(length(Str), lists:duplicate(Len, $0) ++ Str).

select_pv([WantedPV | Tail], SupportedPVs) ->
    case lists:member(WantedPV, SupportedPVs) of
	true ->
	    WantedPV;
	false ->
	    select_pv(Tail, SupportedPVs)
    end;
select_pv([], _) ->
    undefined.

%%%===================================================================
%%% Erlang impl of C API functions
%%%===================================================================
'CelloAvli2_initiateService'({Socket, Spid, PvFirstWanted,
			      PvSecondWanted, PvThirdWanted}, State) ->
    ItcPort = itc_open(Spid),
    ItcSend_Result1 = send_avli_server_up_ind({ItcPort, Spid}),
    ItcSend_Result2 =
	case
	    select_pv([PvFirstWanted, PvSecondWanted, PvThirdWanted],
		      ?AVLI_SUPPORTED_PVs)
	    of
	    SelectedPV when is_integer(SelectedPV) ->
		send_avli2_initiate_service_cfm({ItcPort, Spid},
						{?AVLI_SIGNAL_REVISION,
						 SelectedPV});
	    undefined ->
		send_avli2_initiate_service_rej({ItcPort, Spid},
						{?AVLI_SIGNAL_REVISION,
						 ?CELLO_AVLI_LATEST_PV})
	end,
    put(trace_info, [{itc_open, ItcPort},
		     {itc_send1, ItcSend_Result1},
		     {itc_send2, ItcSend_Result2}]),
    NewData =
	try
	    gb_trees:insert(Socket, {Spid, ItcPort}, State#state.data)
	catch
	    _ : _ ->
		gb_trees:update(Socket, {Spid, ItcPort}, State#state.data)
	end,
    State#state{data = NewData}.


'CelloAvli2_writePiuEvent'({TimeStamp, ServiceStatus, Reason, PiuType,
			    PiuHwAddr, HwPid, AddInfo}, State) ->
    Content = ['PlugInUnitEvent',
	       status_xml(ServiceStatus),
	       reason_xml(Reason),
	       piu_type_xml(PiuType),
	       piu_address_xml(PiuHwAddr),
	       hw_pid_xml(HwPid),
	       add_info_xml(AddInfo)],
    store_logrecord(TimeStamp, Content, State).


'Cello_Avli5_writeHwEvent'({TimeStamp, ServiceStatus, Reason, HwType,
			    HwAddress, HwPid, AddInfo}, State) ->
    Content = ['HardWareEvent',
	       status_xml(ServiceStatus),
	       reason_xml(Reason),
	       hw_type_xml(HwType),
	       hw_address_xml(HwAddress),
	       hw_pid_xml(HwPid),
	       add_info_xml(AddInfo)],
    io:format("#### Cello_Avli5_writeHwEvent - Content = ~p~n",[Content]),
    store_logrecord(TimeStamp, Content, State).

'Cello_Avli2_writeHwEvent'({TimeStamp, ServiceStatus, Reason, HwType,
			    HwAddress, HwPid, AddInfo}, State) ->
    Content = ['HardWareEvent',
	       status_xml(ServiceStatus),
	       reason_xml(Reason),
	       hw_type_xml(HwType),
	       hw_address_xml(HwAddress),
	       hw_pid_xml(HwPid),
	       add_info_xml(AddInfo)],
    store_logrecord(TimeStamp, Content, State).

'Cello_Avli2_writeServiceEvent'({TimeStamp, ServiceStatus, Reason, ServiceType,
				 ServiceInstance, AddInfo}, State) ->
    Content = ['ServiceEvent',
	       status_xml(ServiceStatus),
	       reason_xml(Reason),
	       service_type_xml(ServiceType),
	       service_inst_xml(ServiceInstance),
	       add_info_xml(AddInfo)],
    store_logrecord(TimeStamp, Content, State).


'Cello_Avli2_writeOtherEvent'({TimeStamp, ServiceStatus, Reason,
			       AvailabilityInfo}, State) ->
    Content = ['OtherEvent',
	       status_xml(ServiceStatus),
	       reason_xml(Reason),
	       avail_info_xml(AvailabilityInfo)],
    store_logrecord(TimeStamp, Content, State).


'CelloAvli3_writePgmEvent'({TimeStamp, ServiceStatus, Reason, PiuType,
			    PiuHwAddr, SwPid, AddInfo}, State) ->
    Content = ['ProgramEvent',
	       status_xml(ServiceStatus),
	       reason_xml(Reason),
	       piu_type_xml(PiuType),
	       piu_address_xml(PiuHwAddr),
	       sw_pid_xml(SwPid),
	       add_info_xml(AddInfo)],
    store_logrecord(TimeStamp, Content, State).


'CelloAvli4_writeNodeEvent'({TimeStamp, ServiceStatus, Reason, _EvtId, AddInfo},
			    State) ->
    Content = ['NodeEvent',
	       status_xml(ServiceStatus),
	       reason_xml(Reason),
	       add_info_xml(AddInfo)],
    store_logrecord(TimeStamp, Content, State).


logInfoRecord({TimeStamp, LogInfo}, State) ->
    Content = [{'LogInfoRecord', [logInfo_xml(LogInfo)]}],
    store_logrecord(TimeStamp, Content, State).


write_event(Event, Parameters, From, State) ->
    PidInfo = sysUtil:pid_name(From),
    put(trace_info, []),
    try ?MODULE:Event(Parameters, State#state{clientPidInfo = PidInfo}) of
	{ok, #?MNESIA_TAB{logNo = LogNo}} ->
	    Info =
		[{'Wrote AVLI LogRecord number', LogNo} | PidInfo] ++
		lists:reverse(get(trace_info)),
	    ?LOG_INFO(Info),
	    alhI:print_from(LogNo, 1),
	    ok;
	_ ->
	    ok
    catch
	throw : {validate_dtd_failed, ErrInfo} ->
	    MyErrInfo =
		[{event, Event} | list_params(Parameters) ++ PidInfo],
	    ?LOG_ERR(ErrInfo ++ MyErrInfo),
	    {error, illegal_param};
	ErrClass : ErrReason ->
	    Stacktrace = ?STACKTRACE_E,
	    ?LOG_ERR(["[AVLI] Illegal Parameter",
		      {ErrClass, [ErrReason | Stacktrace]},
		      {event, Event},
		      {parameters, Parameters} |
		      ?STATE_INFO(State)]),
	    {error, illegal_param}
    end.

%%%===================================================================
list_params(Params) when is_tuple(Params) ->
    [{param, Param} || Param <- tuple_to_list(Params)];
list_params(Params) ->
    [{params, Params}].

%%%===================================================================
%%% Send result signals to C
%%%===================================================================
send_avli2_initiate_service_cfm({ItcPort, Spid},
				{SignalRevision, SelectedPV}) ->
    SigNo = ?CELLO_AVLI2_INITIATE_SERVICE_CFM,
    Data = <<SignalRevision:4/native-unsigned-integer-unit:8,
             SelectedPV:4/native-unsigned-integer-unit:8>>,
    itc_send(ItcPort, Spid, SigNo, Data).


send_avli2_initiate_service_rej({ItcPort, Spid},
				{SignalRevision, SupportedPV}) ->
    SigNo = ?CELLO_AVLI2_INITIATE_SERVICE_REJ,
    Data = <<SignalRevision:4/native-unsigned-integer-unit:8,
             SupportedPV:4/native-unsigned-integer-unit:8>>,
    itc_send(ItcPort, Spid, SigNo, Data).


send_write_confirm(Cmd, ClientRef, Res, Socket, State) ->
    {Spid, ItcPort} = gb_trees:get(Socket, State#state.data),
    SigNo = ?CELLO_AVLI_WRITE_CFM,
    CARes = to_c_res(Res),
    Data = <<Cmd:4/native-unsigned-integer-unit:8,
	     ClientRef:4/native-unsigned-integer-unit:8,
	     CARes:2/native-unsigned-integer-unit:8>>,
    itc_send(ItcPort, Spid, SigNo, Data).


send_avli_server_up_ind({ItcPort, Spid}) ->
    SigNo = ?CELLO_AVLI_SERVER_UP_IND,
    Data = <<>>,
    itc_send(ItcPort, Spid, SigNo, Data).


send_avli_server_down_ind({ItcPort, Spid}) ->
    SigNo = ?CELLO_AVLI_SERVER_DOWN_IND,
    Data = <<>>,
    itc_send(ItcPort, Spid, SigNo, Data).

%%%===================================================================
%%% Miscellaneous
%%%===================================================================
makeFun_timeChange() ->
    fun() ->
	    NewSystemTime = os:system_time(),
	    OldSystemTime = NewSystemTime - comsaNtpServer:timestep(latest),
	    NewTimestamp = sysUtil:time_to_timestamp(NewSystemTime),
	    OldTimestamp = sysUtil:time_to_timestamp(OldSystemTime),
	    ?MODULE:logInfoRecord({NewTimestamp,
				   ?ALH_TAG_RcsTimeChange(OldTimestamp,
							  NewTimestamp)})
    end.

%%%===================================================================
%%% Simple XML functions
%%%===================================================================
create_logrecord(LogNo, TimeStamp, Content) ->
    Record = record_content_xml(Content),
    logrecord_xml(LogNo, [timestamp_xml(TimeStamp), Record]).


logrecord_xml(Number, Content) ->
    #'LogRecord'{attr = [{number, itl(Number)}], val = Content}.


record_content_xml(Content) ->
    {'RecordContent', Content}.


timestamp_xml(Time) ->
    {'TimeStamp', time_xml(Time)}.


status_xml(ServiceStatus) when ServiceStatus =:= undefined;
			       ServiceStatus =:= [] ->
    [];

status_xml(ServiceStatus) when is_list(ServiceStatus);
			       is_atom(ServiceStatus) ->
    lta(ServiceStatus).


reason_xml(Reason) when Reason =:= undefined;
			Reason =:= [] ->
    [];

reason_xml(Reason) when is_list(Reason); is_atom(Reason) ->
    {'EventReason', [pad(atl(Reason))]}.


add_info_xml(AddInfo) when is_tuple(AddInfo) ->
    {'AdditionalInfo', info_xml_tags(AddInfo)};
add_info_xml([]) ->
    [];
add_info_xml(AddInfo) when is_list(AddInfo) ->
    {'AdditionalInfo', try_to_simple(AddInfo)}.

logInfo_xml(Val) when is_atom(Val) ->
    Val;
logInfo_xml(?ALH_TAG_RcsTimeChange(OldTime, NewTime)) ->
    ?ALH_TAG_RcsTimeChange(time_xml(OldTime), time_xml(NewTime)).

time_xml(Timestamp) ->
    {{Y, Mo, D}, {H, Mi, S}} = to_datetime(Timestamp),
    [{year, [pad(itl(Y))]},
     {month, [pad(itl(Mo))]},
     {day, [pad(itl(D))]},
     {hour, [pad(itl(H))]},
     {minute, [pad(itl(Mi))]},
     {second, [pad(itl(S))]}].

info_xml_tags(?ALH_TAG_DownTime(Timestamp)) ->
    [{'DownTime', time_xml(Timestamp)}];
info_xml_tags({Tag, Value}) when is_tuple(Value) ->
    [{Tag, info_xml_tags(Value)}];
info_xml_tags({Tag, [Value1 | _] = Values}) when is_tuple(Value1);
						 is_atom(Value1);
						 is_list(Value1) ->
    [{Tag, lists:flatten([info_xml_tags(Value) || Value <- Values])}];
info_xml_tags({Tag, Value}) when is_list(Value) ->
    [{Tag, [pad(Value)]}];
info_xml_tags({Tag, Value}) when is_integer(Value) ->
    [{Tag, [pad(itl(Value))]}];
info_xml_tags({Tag, Value}) when is_atom(Value) ->
    [{Tag, [Value]}];
info_xml_tags(Value) when is_atom(Value) ->
    [[Value]].


piu_type_xml(PiuType) when PiuType =:= undefined;
			   PiuType =:= [] ->
    [];

piu_type_xml(PiuType) when is_list(PiuType); is_atom(PiuType) ->
    {'PiuType', [pad(atl(PiuType))]}.


piu_address_xml(undefined) ->
    [];

piu_address_xml({Smn, Apn, _Ern}) ->
    piu_address_xml({Smn, Apn});

piu_address_xml({Smn, Apn}) ->
    {'PiuAddress', [{'SwitchModuleNumber',[pad(itl(Smn))]},
		    {'SwitchPortNumber', [pad(itl(Apn))]}]}.


hw_pid_xml(undefined) ->
    [];

hw_pid_xml({ProdNo, ProdRev, ProdName, _ProdDate, _SerialNum}) ->
    hw_pid_xml({ProdNo, ProdRev, ProdName});

hw_pid_xml({ProdNo, ProdRev, ProdName}) ->
    {'HwPid', [{'ProdNo', [pad(ProdNo)]},
	       {'ProdRev', [pad(ProdRev)]},
	       {'ProdName', [pad(ProdName)]}]}.


hw_type_xml(HwType) ->
    {'HwType', [pad(HwType)]}.


hw_address_xml(HwAddress) ->
    {'HwAddress', [pad(HwAddress)]}.


sw_pid_xml(undefined) ->
    [];

sw_pid_xml({ProdNo, ProdRev, _ProdName, _ProdDate}) ->
    sw_pid_xml({ProdNo, ProdRev});

sw_pid_xml({ProdNo, ProdRev}) ->
    {'SwPid', [{'ProdNo', [pad(ProdNo)]},
	       {'ProdRev', [pad(ProdRev)]}]}.


service_type_xml([]) ->
    [];

service_type_xml(ServiceType) ->
    {'ServiceType', [pad(ServiceType)]}.


service_inst_xml([]) ->
    [];

service_inst_xml(ServiceInst) ->
    {'ServiceInstance', [pad(ServiceInst)]}.


avail_info_xml(AvailInfo) when is_tuple(AvailInfo) ->
    {'AvailabilityInfo', info_xml_tags(AvailInfo)};
avail_info_xml([]) ->
    [];
avail_info_xml(AvailInfo) when is_list(AvailInfo) ->
    {'AvailabilityInfo', try_to_simple(AvailInfo)}.


pad([]) ->
    " ";
pad(" ") ->
    " ";
pad([First | Tail]) ->
    case First of
	%% 32 == " "
	32 ->
	    [First | pad_last(Tail)];
	_ ->
	    " " ++ [First | pad_last(Tail)]
    end.

pad_last("") ->
    " ";
pad_last(String) ->
    case lists:last(String) of
	%% 32 == " "
	32 ->
	    String;
	_ ->
	    String ++ " "
    end.


try_to_simple(Data) ->
    try
	true = lists:member($<, Data),
	Elems = to_xmerl(Data),
	xmerl_to_simple(Elems)
    catch
	_:_ ->
	    [Data]
    end.


to_xmerl([]) ->
    [];

to_xmerl(String) ->
    try xmerl_scan:string(String) of
	{#xmlElement{} = El, Rest} ->
	    [El | to_xmerl(Rest)];
	Error ->
	    Stacktrace = ?STACKTRACE_C,
	    ErrHeading =
		"RCS: Incorrect AVLI input, unrecognized decoding result",
	    LogFile = ?LOG_DIR ++ "/" ++ ?AVAILABILITY_LOG_XML,
	    ?LOG_ERR([ErrHeading,
		      {xmerl_scan, string, String},
		      {return_value, Error},
		      {check_file, LogFile},
		      {stacktrace, Stacktrace}]),
	    catch alhI:export_log(),
	    [#xmlElement{name = list_to_atom(ErrHeading),
			 content = [?LF, #xmlText{value = String}, ?LF]}]

    catch
	ErrClass : ErrReason ->
	    Stacktrace = ?STACKTRACE_E,
	    ErrHeading =
		"RCS: Incorrect AVLI input, decoding crash",
	    LogFile = ?LOG_DIR ++ "/" ++ ?AVAILABILITY_LOG_XML,
	    ?LOG_ERR([ErrHeading,
		      {input, String},
		      {check_file, LogFile},
		      {ErrClass, [ErrReason | Stacktrace]}]),
	    catch alhI:export_log(),
	    [#xmlElement{name = list_to_atom(ErrHeading),
			 content = [?LF, #xmlText{value = String}, ?LF]}]
    end.

%%% ------------------------------------------------------------------
xmerl_to_simple([#xmlElement{name = Name, content = []} | Tail]) ->
    [case is_empty_tag(Name) of
 	 true ->
 	     Name;
 	 false ->
 	     {Name, []}
     end
     | xmerl_to_simple(Tail)];
xmerl_to_simple([#xmlElement{name = Name, content = Content} | Tail]) ->
    [{Name, xmerl_to_simple(Content)} | xmerl_to_simple(Tail)];
xmerl_to_simple([#xmlText{value = Val} | Tail]) ->
    case Tail of
 	[#xmlComment{} | _] ->
 	    [lists:flatten([Val | xmerl_to_simple(Tail)])];
 	_ ->
 	    [Val | xmerl_to_simple(Tail)]
    end;
xmerl_to_simple([#xmlComment{value = Val} | Tail]) ->
    case Tail of
 	[#xmlText{} | _] ->
 	    [lists:flatten(["<!--" ++ Val ++ "-->" | xmerl_to_simple(Tail)])];
 	_ ->
 	    ["<!--" ++ Val ++ "-->" | xmerl_to_simple(Tail)]
    end;
xmerl_to_simple([]) ->
    [].

%%% ------------------------------------------------------------------
is_empty_tag(?ALH_TAG_RankCold) ->
    true;
is_empty_tag(?ALH_TAG_RankWarm) ->
    true;
is_empty_tag(?ALH_TAG_RankColdWTest) ->
    true;
is_empty_tag(?ALH_TAG_RestartCompleted) ->
    true;
is_empty_tag(_) ->
    false.

%%%===================================================================
%%% Export XML file functions
%%%===================================================================
to_xml_log(DbLog) ->
    Indent = #xmlText{value = "  "},
    Log = [[?LF, format_log_rec(LogRec, Indent), ?LF] ||
	      #?MNESIA_TAB{logRec = LogRec} <- DbLog],
    Simple = {'Log', lists:flatten([?LF, Log, ?LF])},
    Prolog = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
	++ "<!DOCTYPE Log SYSTEM \"" ++ ?AVAILABILITY_LOG_DTD ++ "\">\n\n",
    Xml = xmerl:export_simple([Simple],
			      xmerl_xml,
			      [#xmlAttribute{name=prolog,
					     value=Prolog}]),
    lists:flatten([Xml,"\n"]).


format_log_rec({LC, [Name, TS, AddInfo]}, Indent1) ->
    Indent2 = add_indent(Indent1),
    [Indent1, {LC, [?LF, Indent2, Name | format_timestamp(TS, Indent2)] ++
		   format_add_info(AddInfo, Indent2) ++ [?LF, Indent1]}];

format_log_rec({LR, No, [TS, Content]}, Indent1) ->
    Indent2 = add_indent(Indent1),
    [Indent1, {LR, No, format_timestamp(TS, Indent2) ++
			format_content([Content], Indent2) ++ [?LF, Indent1]}].


format_timestamp({TS, [Y, Mo, D, H, Mi, S]}, Indent1) ->
    Indent2 = add_indent(Indent1),
    [?LF, Indent1, {TS, [?LF, Indent2, Y, ?SP, Mo, ?SP, D, ?LF, Indent2, H,
			 ?SP, Mi, ?SP, S, ?LF, Indent1]}].


format_add_info({AI, [[H |_]] = Val}, Indent) when is_integer(H) ->
    [?LF, Indent, {AI, [?LF, add_indent(Indent) | Val] ++ [?LF, Indent]}];

format_add_info({AI, Val}, Indent1) ->
    Indent2 = add_indent(Indent1),
    [?LF, Indent1, {AI, format_content(Val, Indent2) ++ [?LF, Indent1]}].


format_content(Content, Indent) ->
    F = fun({Tag, _Val} = AddInfo) when Tag =:= 'AdditionalInfo';
					Tag =:= 'AvailabilityInfo' ->
		format_add_info(AddInfo, Indent);
	   ({Tag, [String]}) when is_list(String) ->
		[?LF, Indent, {Tag, split_on_lf_and_comment(String, Indent)}];
	   ({Tag, Val}) ->
		[?LF, Indent,
		 {Tag, format_content(Val, add_indent(Indent)) ++
		      [?LF, Indent]}];
	   (Tag) when is_atom(Tag) ->
		[?LF, Indent, Tag];
	   (Val) ->
		[Val]
	end,
    lists:flatmap(F, Content).


add_indent(#xmlText{value = Val}) ->
    #xmlText{value = Val ++ "  "}.

split_on_lf_and_comment(String, Indent) ->
    Is_LineFeed_or_CarriageReturn =
	lists:member($\n, String) or lists:member($\r, String),
    case Is_LineFeed_or_CarriageReturn of
	true ->
	    Indent2 = add_indent(Indent),
	    case String of
		[Lf] when Lf == $\n orelse Lf == $\r ->
		    make_comment_and_lf(String, Indent);
		[Lf | _] when Lf == $\n orelse Lf == $\r ->
		    make_comment_and_lf(String, Indent2) ++ [?LF, Indent];
		_ ->
		    [?LF, Indent2 | make_comment_and_lf(String, Indent2)] ++
			[?LF, Indent]
	    end;
	false ->
	    make_comment(pad(String))
    end.

make_lf_text({String1, []}, _) ->
    [#xmlText{value = String1}];
make_lf_text({String1, [_ | [_ | _] = String2]}, Indent) ->
    [#xmlText{value = String1}, ?LF, Indent |
     make_lf_text(splitwith_lf(String2), Indent)];
make_lf_text({String1, [_ | []]}, Indent) ->
    [#xmlText{value = String1}, ?LF, Indent].


make_lf_comment({String1, []}, _) ->
    [#xmlComment{value = String1}];
make_lf_comment({String1, [_ | [_ | _] = String2]}, Indent) ->
    [#xmlComment{value = String1}, ?LF, Indent |
     make_lf_comment(splitwith_lf(String2), Indent)];
make_lf_comment({String1, [_ | []]}, Indent) ->
    [#xmlComment{value = String1}, ?LF, Indent].


splitwith_lf(String) ->
    lists:splitwith(fun($\n) ->
			    false;
		       ($\r) ->
			    false;
		       (_) ->
			    true
		    end,
		    String).

make_comment(String) ->
    make_comment_and_lf(String, undefined).

make_comment_and_lf(String, Indent) ->
    make_comment_and_lf(String, Indent, "", []).

make_comment_and_lf([$<,$!,$-,$- | Tail], Indent, AccHead, AccRes) ->
    %% 60,33,45,45 == "<!--"
    case extract_comment(Tail) of
	{complete, Comment, TailAfterComment} ->
	    Res =
		make_lf_text(splitwith_lf(AccHead), Indent) ++
		make_lf_comment(splitwith_lf(Comment), Indent),
	    make_comment_and_lf(TailAfterComment, Indent, "", AccRes ++ Res);
	incomplete ->
	    IncompleteComment = [$<,$!,$-,$- | Tail],
	    ErrorText = "ERROR: Incomplete comment detected.",
	    LogFile = ?LOG_DIR ++ "/" ++ ?AVAILABILITY_LOG_XML,
	    ?LOG_ERR(["Incorrect AVLI input.",
		      ErrorText,
		      {missing_end, IncompleteComment},
		      {check_file, LogFile}]),
	    AccRes ++
		make_lf_text(splitwith_lf(AccHead ++ IncompleteComment),
			     Indent) ++
		[{'RCS note', [?LF, #xmlText{value = ErrorText}, ?LF, Indent]}]
    end;
make_comment_and_lf([Char | Tail], Indent, AccHead, AccRes) ->
    make_comment_and_lf(Tail, Indent, AccHead ++ [Char], AccRes);
make_comment_and_lf([], Indent, AccHead, AccRes) ->
    AccRes ++ make_lf_text(splitwith_lf(AccHead), Indent).

extract_comment(String) ->
    extract_comment(String, "").

extract_comment([$-,$-,$> | Tail], AccComment) ->
    %% 45,45,62 == "-->"
    {complete, AccComment, Tail};
extract_comment([Char | Tail], AccComment) ->
    extract_comment(Tail, AccComment ++ [Char]);
extract_comment([], _) ->
    incomplete.

%%%===================================================================
%%% ITC functions
%%%===================================================================
itc_open(Spid) ->
    Name = "AVLI" ++ itl(Spid),
    itc:open(Name).


itc_send(ItcPort, Spid, SigNo, Data) ->
    itc:send(ItcPort, Spid, SigNo, iolist_to_binary(Data)).


itc_close(undefined) ->
    ok;

itc_close(ItcPort) ->
    itc:close(ItcPort).


%%%===================================================================
%%% DB functions
%%%===================================================================
store_logrecord(TimeStamp, Content, State) ->
    Time1 = ?MonoTime,
    LogRec_ForValidation = create_logrecord(1,
					    TimeStamp,
					    lists:flatten(Content)),
    XmlLog = to_xml_log(mnesia:dirty_read(?MNESIA_TAB, 0) ++
			[#?MNESIA_TAB{logNo = 1,
				      logRec = LogRec_ForValidation}]),
    case validate_dtd(XmlLog) of
	ok ->
	    ok;
	{error, ErrInfo_Validate} ->
	    throw({validate_dtd_failed, ErrInfo_Validate})
    end,
    put(trace_info, [{'Elapsed time (validate_dtd)',
		      ?time2string(?MonoTime - Time1)} |
		     get(trace_info)]),
    Store_logrecord =
	fun() ->
		mnesia:write_lock_table(?MNESIA_TAB),
		LogNo = mnesia:last(?MNESIA_TAB) + 1,
		LogRec = create_logrecord(LogNo,
					  TimeStamp,
					  lists:flatten(Content)),
		Obj = #?MNESIA_TAB{logNo = LogNo,
				   clientInfo = State#state.clientPidInfo,
				   logRec = LogRec},
		write_logrecord(Obj),
		lists:foreach(fun(Pid) ->
				      Pid ! LogRec
			      end,
			      State#state.subscribers),
		Obj
	end,
    Time2 = ?MonoTime,
    try mnesia:transaction(Store_logrecord) of
	{atomic, Obj} ->
	    nodeDownTsSet_startInterval(State, Obj),
	    put(trace_info, [{'Elapsed time (mnesia:transaction)',
			      ?time2string(?MonoTime - Time2)} |
			     get(trace_info)]),
	    delete_oldest_objs(),
	    Time3 = ?MonoTime,
	    swmI:write_upgrWindow_table(?MNESIA_TAB),
	    put(trace_info, [{'Elapsed time (swmI:write_upgrWindow_table)',
			      ?time2string(?MonoTime - Time3)} |
			     get(trace_info)]),
	    {ok, Obj};
	Aborted ->
	    Stacktrace = ?STACKTRACE_C,
	    ?LOG_ERR([{timeStamp, TimeStamp},
		      {content, Content},
		      {state, State},
		      {transaction, Aborted},
		      {stacktrace, Stacktrace}]),
	    aborted
    catch
	ErrClass : ErrReason ->
	    Stacktrace = ?STACKTRACE_E,
	    ?LOG_ERR([{timeStamp, TimeStamp},
		      {content, Content},
		      {state, State},
		      {ErrClass, [ErrReason | Stacktrace]}]),
	    error
    end.

%%%===================================================================
write_logrecord(Obj) ->
    case get_UgInfo(state) of
	?UgState_rollbackStb_started ->
	    Key = {get_ts_gs(Obj), os:system_time()},
	    write_logrecord_q(Key, Obj),
	    write_logrecord_rollback(Key, Obj);
	?UgState_rollbackStb_traffic ->
	    Key = {get_ts_gs(Obj), os:system_time()},
	    write_logrecord_rollback(Key, Obj),
	    mnesia:write(Obj);
	_ ->
	    mnesia:write(Obj)
    end.

%%%===================================================================
write_logrecord_q(Key, Obj) ->
    ets:insert(alh_eventQ, #alh_eventQ{key = Key, obj = Obj}).

%%%===================================================================
write_logrecord_rollback(Key, Obj) ->
    ets:insert(alh_eventQ_rollback, #alh_eventQ_rollback{key = Key, obj = Obj}),
    RB = ets:tab2file(alh_eventQ_rollback, ?FILE_EventQRollback),
    RBB = ets:tab2file(alh_eventQ_rollback, ?FILE_EventQRollback_backup),
    ?LOG_INFO([{?FILE_EventQRollback, RB}, {?FILE_EventQRollback_backup, RBB}]).

%%%===================================================================
get_ts_gs(Obj) ->
    calendar:datetime_to_gregorian_seconds(get_ts(Obj)).

%%%===================================================================
get_ts(#?MNESIA_TAB{logRec = #'LogRecord'{val = Val}}) ->
    ts2datetime(lists:keyfind('TimeStamp', 1, Val));
get_ts(#?MNESIA_TAB{logRec = {'LogCreated', [_, Ts | _]}}) ->
    ts2datetime(Ts).

%%% #---------------------------------------------------------
reset_db_log() ->
    {atomic, ok} = mnesia:clear_table(?MNESIA_TAB),
    Created = get_log_created(),
    {atomic, ok} =
    	mnesia:transaction(fun() ->
				   mnesia:write(#?MNESIA_TAB{logNo = 0,
							     logRec = Created})
    			   end).

%%% #---------------------------------------------------------
%%% #---------------------------------------------------------
delete_oldest_objs() ->
    MaxSize = ?MAX_LOG_REC_NO + 1,   % +1 because the first object in the table
						% is untouchable and doesn't
						% count as a normal log event.
    delete_oldest_objs(?MNESIA_TAB, MaxSize).

%%% #---------------------------------------------------------
delete_oldest_objs(Tab, MaxSize) ->
    delete_oldest_obj(mnesia:table_info(Tab, size) > MaxSize,
		      Tab,
		      MaxSize).

%%% #---------------------------------------------------------
delete_oldest_obj(false, _, _) ->
    ok;
delete_oldest_obj(true, Tab, MaxSize) ->
    %% First object must never be deleted, so get the 2nd key!
    OldestKey = mnesia:dirty_next(Tab, mnesia:dirty_first(Tab)),
    mnesia:dirty_delete({Tab, OldestKey}),
    delete_oldest_objs(Tab, MaxSize).

%%%===================================================================
%%% COLI functions
%%%===================================================================
filter_and_format(DbLog, []) ->
    filter_and_format(DbLog, 0, ?MAX_LOG_REC_NO);
filter_and_format(DbLog, [First]) ->
    filter_and_format(DbLog, try_lti(First), ?MAX_LOG_REC_NO);
filter_and_format(DbLog, [First, Last]) ->
    filter_and_format(DbLog, try_lti(First), try_lti(Last));
filter_and_format(_, _) ->
    io:format("argument error~n"),
    exit(error).

filter_and_format(_, First, Last) when First < 0;
				       Last > ?MAX_LOG_REC_NO;
				       First > Last ->
    io:format("argument error~n"),
    exit(error);
filter_and_format(DbLog, First, Last) ->
    f_and_f(DbLog, First, Last).

f_and_f([], _, _) ->
    exit(ok);
f_and_f([#?MNESIA_TAB{logNo = LogNo} | T], First, Last)
  when LogNo < First ->
    f_and_f(T, First, Last);
f_and_f([#?MNESIA_TAB{logNo = LogNo, logRec = LogRec} | T], First, Last)
  when LogNo =< Last ->
    coli_format_print(LogNo, LogRec),
    f_and_f(T, First, Last);
f_and_f(_, _, _) ->
    exit(ok).

try_lti(L) ->
    try
	list_to_integer(string:strip(L, both))
    catch
	_:_ ->
	    io:format("argument error~n"),
	    exit(error)
    end.

coli_format_print(LogNo, {'LogCreated', [_, Ts | _]}) ->
    DateTime = decode_ts(Ts),
    io:format("~-4b: ~s LogCreated~n", [LogNo, DateTime]);
coli_format_print(LogNo, #'LogRecord'{val = [Ts | Rest]}) ->
    DateTime = decode_ts(Ts),
    io:format("~-4b: ~s LogRecord: ~p~n", [LogNo, DateTime, Rest]).

%just assume the timestamp components are in a fixed order
decode_ts({'TimeStamp', Ts}) ->
    Tc = [ try_lti(T) || {_ , [T]} <- Ts],
    %YYYYMMDD HH:MM:SS
    io_lib:format("~4.10.0b~2.10.0b~2.10.0b ~2.10.0b:~2.10.0b:~2.10.0b", Tc).

ts2datetime({'TimeStamp', Ts}) ->
    {{try_lti(lists:flatten(proplists:get_value(year, Ts))),
      try_lti(lists:flatten(proplists:get_value(month, Ts))),
      try_lti(lists:flatten(proplists:get_value(day, Ts)))},
     {try_lti(lists:flatten(proplists:get_value(hour, Ts))),
      try_lti(lists:flatten(proplists:get_value(minute, Ts))),
      try_lti(lists:flatten(proplists:get_value(second, Ts)))}}.

%%%===================================================================
%%% Commonly used functions
%%%===================================================================
create_alh_dir() ->
    file:make_dir(alh_dir()),
    file:make_dir(alh_home_dir()).

create_ram_dir() ->
    case file:read_link(filename:join(sysEnv:rcs_dir(), "erlang")) of
	{ok, Link_tmp} ->
	    %% Target Node.
	    TmpDir = filename:dirname(Link_tmp),
	    RamDir_Target = filename:join(TmpDir, "alh_ram"),
	    file:make_dir(RamDir_Target),
	    file:make_symlink(RamDir_Target, ?RAM_DIR);
	{error, einval} ->
	    %% Simulator.
	    file:make_dir(?RAM_DIR);
	Error ->
	    Stacktrace = ?STACKTRACE_C,
	    ?LOG_ERR([Error, {stacktrace, Stacktrace}]),
	    Error
    end.

create_rollback_dir() ->
    try
	ok = filelib:ensure_dir(?FILE_nodeDownTs_rollback),
	ok = filelib:ensure_dir(?FILE_EventQRollback),
	ok = filelib:ensure_dir(?FILE_EventQRollback_backup)
    catch
	EC : ER ->
	    Stacktrace = ?STACKTRACE_E,
	    ?LOG_ERR([{EC, ER}, "--- Stacktrace ---" | Stacktrace])
    end.

create_log_dir(LogDir) ->
    case create_dir(LogDir) of
	ok ->
	    ok;
	_Error ->
	    Comp = filename:split(LogDir),
	    create_log_dir(Comp, [])
    end.


create_log_dir([DirName | T], Path) ->
    Dir = filename:join(Path, DirName),
    ok = create_dir(Dir),
    create_log_dir(T, Dir);

create_log_dir([], _) ->
    ok.


create_dir(Dir) ->
    case file:make_dir(Dir) of
	Res when Res =:= {error, eexist};
		 Res =:= ok ->
	    ok;
	Error ->
	    Error
    end.


to_time(T) ->
    to_datetime(T).

to_datetime(?CELLO_AVLI_TIME_BY_AVLI) ->
    calendar:local_time();
to_datetime(T) when is_integer(T) ->
    SecsStr = integer_to_list(T),
    {LeftmostDigitStr, _} = lists:split(1, SecsStr),
    case list_to_integer(LeftmostDigitStr) of
	LeftmostDigit when LeftmostDigit >= 6 ->   % Gregorian
	    calendar:gregorian_seconds_to_datetime(T);
	LeftmostDigit when LeftmostDigit >= 1 ->   % POSIX
	    {SecondsStr, _} = lists:split(10, SecsStr),
	    Seconds = list_to_integer(SecondsStr),
	    calendar:now_to_local_time({Seconds div 1000000,
					Seconds rem 1000000,
					0})
    end;
to_datetime({_MeS, _S, _MiS} = Now) ->
    calendar:now_to_local_time(Now);
to_datetime({{Y, Mo, D}, {H, Mi, S}} = Time)
  when is_integer(Y) andalso
       is_integer(Mo) andalso
       is_integer(D) andalso
       is_integer(H) andalso
       is_integer(Mi) andalso
       is_integer(S) ->
    Time;
to_datetime(LogRecT) when is_list(LogRecT) ->
    {{lr_ts(year, LogRecT), lr_ts(month, LogRecT), lr_ts(day, LogRecT)},
     {lr_ts(hour, LogRecT), lr_ts(minute, LogRecT), lr_ts(second, LogRecT)}}.

%%% ----------------------------------------------------------
%%% @doc Information from APPM about the WarmRestart procedure.
%%% @end
%%% ----------------------------------------------------------
-spec warm_restart(RestartInfo :: {RestartPhase :: (nodeDown |
						    starting |
						    operational),
				   TimeStamp :: now_ts()}) ->
    ok.
%%% ----------------------------------------------------------
warm_restart(RestartInfo) ->
    gen_server:cast(?SERVER, {warm_restart, RestartInfo, self()}),
    ok.

lr_ts(Tag, TimeList) ->
    Value = lists:flatten(proplists:get_value(Tag, TimeList)),
    list_to_integer(string:strip(Value)).


to_status(?CELLO_AVLI_IN_SERVICE) ->
    ?ALH_TAG_InService;
to_status(?CELLO_AVLI_OUT_OF_SERVICE) ->
    ?ALH_TAG_OutOfService;
to_status(?CELLO_AVLI_PARTIALLY_OUT_OF_SERVICE) ->
    ?ALH_TAG_PartiallyOutOfService;
to_status(?CELLO_AVLI_EVENT_NOT_USED) ->
    undefined;
to_status(Unknown) ->
    {illegal_param, Unknown}.


to_reason(?CELLO_AVLI_SHUTDOWN_COMMAND) ->
    ?ALH_TAG_ShutdownCommand;
to_reason(?CELLO_AVLI_UNOPERATIONAL) ->
    ?ALH_TAG_Unoperational;
to_reason(?CELLO_AVLI_STARTING) ->
    ?ALH_TAG_Starting;
to_reason(?CELLO_AVLI_OPERATIONAL) ->
    ?ALH_TAG_Operational;
to_reason(?CELLO_AVLI_REASON_NOT_USED) ->
    undefined;
to_reason(Unknown) ->
    integer_to_list(Unknown).


to_piu_type(?CELLO_AVLI_MP) ->
    "Mp";
to_piu_type(?CELLO_AVLI_BP) ->
    "Bp";
to_piu_type(?CELLO_AVLI_NONE) ->
    undefined;
to_piu_type(Unknown) ->
    integer_to_list(Unknown).


to_piu_hw_addr(?TRUE, HwAddr) ->
    HwAddr;
to_piu_hw_addr(?FALSE, _) ->
    undefined.


to_hw_pid(?TRUE, HwPid) ->
    HwPid;
to_hw_pid(?FALSE, _) ->
    undefined.


to_sw_pid(?TRUE, SwPid) ->
    SwPid;
to_sw_pid(?FALSE, _) ->
    undefined.


to_string(Text) when is_binary(Text) ->
    to_string(binary_to_list(Text));

to_string(Text) when is_list(Text) ->
    lists:takewhile(fun(B) -> B =/= 0 end, Text).


to_c_res(ok) ->
    ?CELLO_AVLI_SUCCESS;
%% to_c_res({error, service_unavailable}) ->
%%     ?CELLO_AVLI_SERVICE_UNAVAIL;
%% to_c_res({error, too_long_string}) ->
%%     ?CELLO_AVLI_TOO_LONG_STRING;
%% to_c_res({error, out_of_memory}) ->
%%     ?CELLO_AVLI_OUT_OF_MEMORY;
%% to_c_res({error, illegal_signal}) ->
%%     ?CELLO_AVLI_ILLEGAL_SIGNAL;
%% to_c_res({error, illegal_event}) ->
%%     ?CELLO_AVLI_ILLEGAL_EVENT;
%% to_c_res({error, log_not_created}) ->
%%     ?CELLO_AVLI_LOG_NOT_CREATED;
to_c_res({error, illegal_param}) ->
    ?CELLO_AVLI_ILLEGAL_PARAM.
%% to_c_res({error, memory_not_initiated}) ->
%%     ?CELLO_AVLI_MEMORY_NOT_INITIATED;
%% to_c_res({error, not_supported_by_selected_pv}) ->
%%     ?CELLO_AVLI_NOT_SUPPORTED_BY_SELECTED_PV.


%% to_c_string(Text, Length) when is_list(Text) ->
%%     to_c_string(list_to_binary(Text), Length);
%% to_c_string(Text, Length) ->
%%     case byte_size(Text) of
%%         TextLength when TextLength >= Length ->
%%             [binary:part(Text, 0, Length-1), 0];
%%         TextLength ->
%%             [Text, binary:copy(<<0>>, Length-TextLength)]
%%     end.
data_foreach(Fun, Data) ->
    Iter = gb_trees:iterator(Data),
    data_foreach1(Fun, Iter).

data_foreach1(Fun, Iter) ->
    case gb_trees:next(Iter) of
	none ->
	    ok;
	{Key, Val, Iter2} ->
	    Fun({Key, Val}),
	    data_foreach1(Fun, Iter2)
    end.


itl(I) when is_integer(I) ->
    integer_to_list(I);
itl(L) when is_list(L) ->
    L.


lta(L) when is_list(L) ->
    list_to_atom(L);
lta(A) when is_atom(A) ->
    A.


atl(A) when is_atom(A) ->
    atom_to_list(A);
atl(L) when is_list(L) ->
    L.

%% #############################################################################
%% ensureBin
%%
%% ###=======================================================================###
ensureBin(Term) when not is_binary(Term) ->
    list_to_binary(ensureStr(Term)).
    
%% #############################################################################
%% ensureStr
%%
%% ###=======================================================================###
ensureStr(Term) ->
    catch sysUtil:term_to_string(Term).
