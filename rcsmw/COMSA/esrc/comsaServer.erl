%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaServer.erl %
%%% @author eivomat
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/4
%%%
%%% @doc == COMSA server ==
%%% This module sets up the COM models and acts as agent support
%%% for certain managed elements
%%%
-module(comsaServer).
-behaviour(gen_server).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/4').
-author(etxjotj).
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% R1A/12     20120618   etxlg       Ntp actions moved to sysEEinterface
%%% R2A/4      20121116   etxkols     Changed R to r in register_transaction_serveR
%%% R2A/5      20121123   etxjotj     Added update schema again
%%% R2A/18     20130226   etxarnu     Delay update schema 10 sec at start
%%% R2A/23     2013-04-04 erarafo     Compiler warnings cleared
%%% R2A/26     2013-05-02 etxjotj     Fixed timezone setting for linux shell
%%%                                   Export ntp conf for upgrade
%%% R2A/30     2013-05-24 etxderb     Adapted for new interface in
%%%                                   sysEEinterface:configure_ntp
%%% R2A/31     2013-05-27 etxlg       Limit sudo use
%%% R2A/32     2013-06-03 etxderb     Corr of format for ntp_conf comments
%%% R2A/33     2013-06-05 etxarnu     Removed com health check
%%% R2A/36     2013-09-24 erarafo     Fixed unused argument warning
%%% R2A/37     2013-11-21 etxpejn     Added support for alarm buffering
%%% R2A/39     2013-12-20 etxjotj     Spelling corrections
%%% R2A/41     2014-01-08 etxpeno     Empty alarm_list after
%%%                                   sending buffered alarms
%%% R2A/42     2014-01-08 etxpeno     Add wrapper healthCheck()
%%% R2A/44     2014-01-23 erarafo     #state.com_timer value format fixed
%%% R2A/48     2014-02-03 etxberb     Added actvate/0 and AVLI logging.
%%% R2A/52     2014-02-17 etxlg       Ntp table subscriptions removed
%%% 					no more setting of timezone
%%% R2A/53     2014-02-17 etxlg       Removed som unused functions
%%% R2A/54     2014-02-18 erarafo     Compiler warning quick fix
%%% R2A/55     2014-02-19 etxjotj     Added Selected model options
%%% R2A/56     2014-03-18 erarafo     Added event for restart log
%%% R2A/57     2014-03-19 erarafo     Guard against multiple log entries
%%% R2A/58     2014-03-19 erarafo     Comments and cleanup
%%% R2A/59     2014-04-14 etxberb     Changed calls to alhI to async.
%%% R2A/60     2014-04-30 etxpejn     Added clear_alarm
%%% R2A/61     2014-05-23 erarafo     Dialyzer fault fixed
%%% R2A/62     2014-09-11 etxlg       kick the firewall
%%% R2A/64     2014-09-11 etxlg       subscribe and kick also for ldap
%%% R2A/65     2014-09-19 etxlg       subscribe snmp, agent and targets
%%% R2A/66     2014-09-19 etxlg       firewall completely esorcised
%%% R3A/1      2014-09-30 erarafo     Support for Additional Info
%%% R3A/6      2015-01-14 etxpeno     Support for regular role
%%% R3A/7      2015-01-30 etxtory     Use sysEnv interface for www
%%% R3A/8      2015-02-02 etxarnu     Added print_com_version/0 and call
%%%                                   it at activate
%%% R3A/9      2015-02-03 etxarnu     improved print_com_version
%%% R3A/10     2015-02-03 etxlg       subscribe snmp -> oot
%%% R3A/11     2015-02-04 etxarnu     moved print_com_version to com_started
%%% R3A/12     2015-02-23 etxberb     Added role check in activate/0.
%%% R4A/1      2015-05-12 erarafo     Using interface functions in comsaEvent
%%% R4A/2      2015-05-28 etxtory     Merge from R3A/13
%%% R4A/3      2015-06-02 etxpeno     remove calls to deprecated functions
%%% R4A/4      2015-09-04 uabesvi     HT83145
%%% R4A/8      2015-09-09 etxtory     Softlinks for .xml in document_root
%%% R4A/9      2015-09-10 etxtory     Fixed above
%%% R4A/10     2015-09-15 erarafo     Support for warm restart
%%% R4A/11     2015-10-14 etxpeno     Using the time functionality in OTP 18
%%% R4A/12     2015-12-04 etxjotj     HU42180 Improved index file
%%% R6A/1      2016-05-04 uabhgma     ComSecM -> RcsSecM
%%% R7A/1      2016-09-08 etxpeno     Dialyzer fixes
%%% R8A/1      2017-01-16 etxarnu     Added rmeSds and rmeExeR callback reg.
%%% R8A/2      2017-01-19 etxpeno     add is_com_started/0
%%% R9A/1      2017-02-13 etomist     HV59470
%%% R9A/2      2017-02-19 etxarnu     Call comsaServDiscServer:activate
%%% R9A/3      2017-03-03 etomist     HV69350
%%% R9A/4      2017-03-08 etomist     Revert HV59470, HV69350
%%% R9A/5      2017-03-10 etomist     Return HV59470, HV69350
%%% R9A/6      2017-03-14 etxpeno     Add info_msg/2 and error_msg/2
%%% R10A/1     2017-04-28 ekrebak     Add info_msg printout for HV81969
%%% R10A/2     2017-05-03 ekrebak     Fix for HV65435 and Revert HV59470, HV69350
%%% R10A/3     2017-05-19 etxpeno     change com_stopped/0 to be synchronously
%%% R10A/4     2017-06-22 etxpeno     Support for dev_patches when validating XML
%%% R10A/5     2017-06-26 ecotjos     HV92106
%%% R11A/1     2017-09-06 etxpeno     increase timeout in com_stopped
%%% R11A/2     2017-09-13 etxpeno     increase timeout in com_stopped
%%% R11A/3     2017-10-03 etxpejn     Call SWM when networkManagedElementId has changed
%%% R11A/6     2017-10-19 etxpeno     (MR36328) handle program groups
%%% R11A/7     2017-10-19 eolaand     HW35849, act as object implementer for 
%%%                                   NodeSupport
%%% R12A/2     2017-11-21 qostjoa     Added rmeSdsServer callback reg.
%%% R12A/3     2017-11-27 qostjoa     Back out rmeSdsServer
%%% R12A/4     2017-12-04 eivomat     HW48860
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0]).
-export([activate/0]).

-export([com_started/0]).
-export([com_stopped/0]).
-export([is_com_started/0]).
-export([send_alarm/7]).
-export([send_alarm/8]).
-export([clear_alarm/5]).
-export([clear_alarm_by_instance/3]).
-export([get_alarm_correlation_info/1]).

-export([avli_other_event/1]).
-export([print_com_version/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-compile(nowarn_unused_vars).

-include("ComTop.hrl").
-include("RcsSysM.hrl").
-include("comte_ac.hrl").
-include("alhI.hrl").
-include("ComsaTypes.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/file.hrl").

-type milli_seconds() :: integer().

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Called by comsaSuper to start the comsaServer process
%%% @end
%%% ----------------------------------------------------------
start() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

%%% ----------------------------------------------------------
%%% @doc Called by sysApp. A proprietary StartPhase mechanism.
%%% @end
%%% ----------------------------------------------------------
activate() ->
    comsaServDiscServer:activate(),
    activate(clhI:core_state()).

activate(active) ->

    comsaNtpServer:start_ntp(),

    %%% This AVLI logging can not be done from the init start phase because:
    %%% * Should be done after APPM's initial AVLI logging, and APPM is started
    %%%   after COMSA.
    %%% * COMSA is started before ALH - AVLI logging can not be called before
    %%%   ALH is started.
    avli_other_event(?ALH_TAG_NodeRestarted),

    %% Notify server that we are in activate phase
    gen_server:cast(?MODULE, activate),
    ok;
activate(_) ->
    ok.

%%% ----------------------------------------------------------
%%% @doc Send alarms through the server to check that COM is up.
%%% @end
%%% ----------------------------------------------------------

-type dn_type()          :: [binary()].
-type alarm_id_type()    :: {VendorId::integer(), MajorId::integer(), MinorId::integer()}|atom().
-type severity_type()    :: indeterminate | warning | minor | major | critical.
-type uuid_type()        :: binary().
-type correlation_info_type() :: {} | {primary, CorrelationUUID::uuid_type()}.

-spec send_alarm(AlarmName::alarm_id_type(),
		 Severity::severity_type(),
		 Dn::dn_type(),
		 AdditionalText::string(),
		 AdditionalInfo::list(),
		 AppAlarm::boolean(),
		 PrgGrp::any()) -> ok.

send_alarm(AlarmName, Severity, Dn, AdditionalText, AdditionalInfo, AppAlarm,
	   PrgGrp) ->
    gen_server:cast(?MODULE, {send_alarm, AlarmName, Severity, Dn,
			      AdditionalText, AdditionalInfo, AppAlarm,
			      PrgGrp}).

-spec send_alarm(AlarmName::alarm_id_type(),
		 Severity::severity_type(),
		 Dn::dn_type(),
		 AdditionalText::string(),
		 AdditionaIUUIDInfo::list(),
		 AppAlarm::boolean(),
		 PrgGrp::any(),
		 CorrelationInfo::correlation_info_type()) -> ok.

send_alarm(AlarmName, Severity, Dn, AdditionalText, AdditionalInfo, AppAlarm,
	   PrgGrp, CorrelationInfo) ->
    NewAdditionalInfo =
	[get_alarm_correlation_info(CorrelationInfo) | AdditionalInfo],
    gen_server:cast(?MODULE, {send_alarm, AlarmName, Severity, Dn,
			      AdditionalText, NewAdditionalInfo, AppAlarm,
			      PrgGrp}).

%%% ----------------------------------------------------------
%%% @doc Clear alarms through the server to check that COM is up.
%%% @end
%%% ----------------------------------------------------------
clear_alarm(AlarmName, Dn, AdditionalText, AdditionalInfo, AppAlarm) ->
    gen_server:cast(?MODULE, {clear_alarm, AlarmName, Dn, AdditionalText,
			      AdditionalInfo, AppAlarm}).

%%% ----------------------------------------------------------
%%% @doc Clear alarm specified as alarm instance.
%%% @end
%%% ----------------------------------------------------------
clear_alarm_by_instance(MajorType, MinorType, Dn) ->
    gen_server:cast(?MODULE,
		    {clear_alarm_by_instance, MajorType, MinorType, Dn}).

%%% ----------------------------------------------------------
%%% @doc Alerts the server that COM has started
%%% @end
%%% ----------------------------------------------------------
com_started() ->
    catch log_com_version(),
    gen_server:cast(?MODULE, com_started),
    comsaEvent:com_started().

%%% ----------------------------------------------------------
%%% @doc Alerts the server that COM has stopped.
%%% @end
%%% ----------------------------------------------------------
com_stopped() ->
	%% info_msg("Number of messages in comsaServer:~p~n",
	%%			[erlang:process_info(whereis(?MODULE), message_queue_len)]), traces when comsaServer is overloaded
    gen_server:call(?MODULE, com_stopped, 40000),
    comsaEvent:com_stopped().

%%% ----------------------------------------------------------
%%% @doc Checks if COM is started
%%% @end
%%% ----------------------------------------------------------
is_com_started() ->
    gen_server:call(?MODULE, is_com_started).

%%% ----------------------------------------------------------
%%% @doc Prints the COM and MAF version
%%% @end
%%% ----------------------------------------------------------
print_com_version() ->
    case catch begin
		   Cmd = filename:join([code:priv_dir(comte),"bin","com_version"]),
		   Str = os:cmd(Cmd),
		   [Maf, Com|_] = string:tokens(Str,"\n"),
		   {Maf, Com}
	       end of
	{'EXIT', Reason} ->
	    info_msg("~nCould not read MAF version~nReason~p~n", [Reason]);
	{M, C} ->
	    info_msg("~n~p~n~p~n", [M, C])
    end.

log_com_version() ->
    case catch begin
           Cmd = filename:join([code:priv_dir(comte),"bin","com_version"]),
           Str = os:cmd(Cmd),
           [Maf, Com|_] = string:tokens(Str,"\n"),
           {Maf, Com}
           end of
    {'EXIT', Reason} ->
        info_msg("~nCould not read MAF version~nReason~p~n", [Reason]),
        comsaLib:write_com_version(
          "~nCould not read MAF version~nReason~p~n", [Reason]);
    {M, C} ->
        info_msg("~n~p~n~p~n", [M, C]),
        comsaLib:write_com_version("~n~p~n~p~n", [M, C])
    end.

%%% ----------------------------------------------------------
%%% @doc Writes node information in the AVLI log.
%%% @end
%%% ----------------------------------------------------------
avli_other_event(NodeIdReason) ->
    Self = self(),
    case whereis(?MODULE) of
	Self ->
	    handle_avli_other_event(NodeIdReason);
	_ ->
	    gen_server:cast(?MODULE, {avli_other_event, NodeIdReason})
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

-record(state, {alarm_list = []           :: [tuple()],     % tuple arity is 5, 4 or 3
		com_started = false       :: boolean(),
		com_timer                 :: timer:tref() | 'undefined',
		latest_restart_log_entry  :: milli_seconds() | 'undefined',
		imm_slave                 :: pid() | 'undefined',
		oi_handle                 :: integer() | 'undefined'
	       }).

init(Args) ->
    %% Allow for speedier return at startup
    gen_server:cast(self(), {initialize, Args}),
    {ok, initializing}.

initialize(_Args) ->
    %% comsaServer only owns the tablec
    ets:new(comsaTransactionMapping, [ordered_set,
				      public,
				      named_table]),
    case comsaLib:get_variable(first_run) of
	true -> ok;
	undefined ->
	    enable_com(),
	    comsaLib:set_variable(first_run, true)
    end,

%% Timezone should never be changed, it should always remain UTC/lg
%%    MeFun = fun() -> mnesia:read({managedElement, {"1"}}) end,
%%    case mnesia:transaction(MeFun) of
%%	{atomic, [ManagedElement]} ->
%%	    TimeZone = ManagedElement#managedElement.timeZone,
%%	    set_timezone(TimeZone);
%%	_ ->
%%	    ok
 %%   end,

    %% Set up mnesia subscriptions
    mnesia:subscribe({table, managedElement, detailed}),
    %% {ComStatus, TimerRef, Latest} =
    %% 	case healthCheck() of
    %% 	    ?ComOk ->
    %% 		Now = restart_logger_trace(?LINE, {0, 0, 0}),
    %% 		{true, undefined, Now};
    %% 	    _ ->
    %% 		{ok, TRef} = timer:apply_interval(1000, gen_server, cast,
    %% 						  [?MODULE, check_com_status]),
    %% 		{false, TRef, {0, 0, 0}}
    %% 	end,
    %% To warning about avoid missing model_file_list.cfg at initial start
    timer:apply_after(10000, gen_server, cast, [comsaServer, update_schema]),
    start_periodicLogging(),

    mnesia:subscribe({table, snmp, simple}), %this stuff is sent on to OOT

    {noreply, #state{}}.
    %% {noreply, #state{com_started = ComStatus,
    %% 		com_timer = TimerRef,
    %% 		latest_restart_log_entry = Latest}}.

start_periodicLogging() ->
    {ok, _TRef} = timer:send_interval(86400000, periodicLogging_24h).

handle_call(is_com_started, _From, #state{com_started = ComStarted} = State) ->
    {reply, ComStarted, State};

handle_call(com_stopped, _From, State) ->
    %% {ok, TRef} = timer:apply_interval(1000, gen_server, cast,
    %% 				      [?MODULE, check_com_status]),
    %% {noreply, State#state{com_started = false, com_timer=TRef}};
    info_msg("COM is stopped~n", []),
    {reply, ok, State#state{com_started = false}};

handle_call(dump, _From, State) ->
    {reply, {ok, State}, State};

handle_call(_Request, _From, State) ->
    sysInitI:warning_report([{?MODULE, handle_call},
				 {from, _From},
				 {unrecognized_msg, _Request}]),
    {reply, ok, State}.

handle_cast({initialize, Args}, initializing) ->
    initialize(Args);

handle_cast(activate, State) ->
    case swmI:is_upgrade_ongoing() of
        true ->
            Pid = spawn_link(fun wait_for_imm/0),
            {noreply, State#state{imm_slave = Pid}};
	_False ->
	    gen_server:cast(?MODULE, imm_ready),
            {noreply, State}
    end;

handle_cast(imm_ready, State) ->
    OiHandle = comsaImm:initialize_ns_oi(),
    MeData = swmI:get_current_up_metadata(),
    Vsn = proplists:get_value(productRevision, MeData),
    ok = comsaImm:set_node_support_release(OiHandle, Vsn),
    {noreply, State#state{oi_handle = OiHandle, imm_slave = undefined}};

handle_cast(update_schema, State) ->
    Info = comte:info(),
    {value, {environment, Env}} = lists:keysearch(environment, 1, Info),
    {value, {start_com_prog, StartComFile}} =
    	lists:keysearch(start_com_prog, 1, Env),
    {value, {com_top, ComTop}} =
    	lists:keysearch(com_top, 1, Env),

    ModelFilePath = filename:join(filename:dirname(StartComFile),
    				  "model_file_list.cfg"),
    case file:read_file(ModelFilePath) of
	{ok, Bin}  ->

	    Files = string:tokens(binary_to_list(Bin), "\r\n"),
	    IncDir = filename:join([ComTop, "opt", "com", "etc", "model"]),
	    handle_update_schema(Files, IncDir);
	{error, _Reason} ->
	    warning_msg("Cannot update schema. ~p missing.~n",
			[ModelFilePath]),
	    timer:apply_after(10000, gen_server, cast, [comsaServer, update_schema])
    end,

    {noreply, State};

%% handle_cast(check_com_status, State) ->
%%     case healthCheck() of
%% 	?ComOk ->
%% 	    NewTimestamp =
%% 		restart_logger_trace(
%% 		  ?LINE,
%% 		  State#state.latest_restart_log_entry),
%% 	    case State#state.com_timer of
%% 		undefined -> ok;
%% 		TimerRef -> timer:cancel(TimerRef)
%% 	    end,
%% 	    {noreply, State#state{com_started=true,
%% 				  com_timer=undefined,
%% 				  latest_restart_log_entry = NewTimestamp}};
%% 	_ ->
%% 	    {noreply, State}
%%     end;

handle_cast(com_started, #state{alarm_list = AlarmList} = State) ->
    %% HV81969 debug printout.
    info_msg("COM started, sending buffered alarms:~n~p~n~n", [AlarmList]),
    send_buffered_alarms(lists:reverse(AlarmList)),
    case State#state.com_timer of
	undefined -> ok;
	TimerRef -> timer:cancel(TimerRef)
    end,
    NewTimestamp =
	restart_logger_trace(
	  ?LINE,
	  State#state.latest_restart_log_entry),
    {noreply, State#state{alarm_list = [],
			  com_started = true,
			  com_timer = undefined,
			  latest_restart_log_entry = NewTimestamp}};

handle_cast({send_alarm, AlarmName, Severity, Dn, AdditionalText,
	     AdditionalInfo, AppAlarm, PrgGrp},
	    #state{alarm_list = AlarmList,
		   com_started = false} = State) ->
    %% Buffer the alarm and send when COM is up
    AlarmReq = {send_alarm, AlarmName, Severity, Dn, AdditionalText,
		AdditionalInfo, AppAlarm, PrgGrp},
    NewAlarmList = [AlarmReq|AlarmList],
    {noreply, State#state{alarm_list = NewAlarmList}};

handle_cast({send_alarm, AlarmName, Severity, Dn, AdditionalText,
	     AdditionalInfo, AppAlarm, PrgGrp},
	    #state{com_started = true} = State) ->
    comFm:send_alarm(AlarmName, Severity, Dn, AdditionalText, AdditionalInfo,
		     AppAlarm, PrgGrp),
    {noreply, State};

handle_cast({clear_alarm, AlarmName, Dn, AdditionalText, AdditionalInfo,
	     AppAlarm},
	    #state{alarm_list = AlarmList,
		   com_started = false} = State) ->
    %% Buffer the clear request and handle when COM is up
    AlarmReq = {clear_alarm, AlarmName, Dn, AdditionalText, AdditionalInfo,
		AppAlarm},
    NewAlarmList = [AlarmReq|AlarmList],
    {noreply, State#state{alarm_list = NewAlarmList}};
handle_cast({clear_alarm, AlarmName, Dn, AdditionalText, AdditionalInfo,
	     AppAlarm},
	    #state{com_started = true} = State) ->
    comFm:clear_alarm(AlarmName, Dn, AdditionalText, AdditionalInfo, AppAlarm),
    {noreply, State};

handle_cast({clear_alarm_by_instance, MajorType, MinorType, Dn},
	    #state{alarm_list = AlarmList,
		   com_started = false} = State) ->
    %% Buffer the clear request and handle when COM is up
    AlarmReq = {clear_alarm_by_instance, MajorType, MinorType, Dn},
    NewAlarmList = [AlarmReq|AlarmList],
    {noreply, State#state{alarm_list = NewAlarmList}};
handle_cast({clear_alarm_by_instance, MajorType, MinorType, Dn},
	    #state{com_started = true} = State) ->
    comFm:clear_alarm_by_instance(MajorType, MinorType, Dn),
    {noreply, State};

handle_cast({avli_other_event, NodeIdReason}, State) ->
    handle_avli_other_event(NodeIdReason),
    {noreply, State};

handle_cast(_Request, State) ->
    sysInitI:warning_report([{?MODULE, handle_cast},
				 {unrecognized_msg, _Request}]),
    {noreply, State}.

handle_info({mnesia_table_event, Event}, State) ->
    handle_mnesia_table_event(Event),
    %% case catch handle_mnesia_table_event(Event) of
    %%     {'EXIT', Reason} ->
    %%         sysInitI:error_report(
    %%           [{mfa, {?MODULE, handle_mnesia_table_event, [Event]}},
    %%            {'EXIT', Reason}]);
    %%     _ ->
    %%         ok
    %% end,
    {noreply, State};

handle_info(periodicLogging_24h, State) ->
    handle_avli_other_event(?ALH_TAG_PeriodicLogging),
    {noreply, State};

handle_info(_Msg, State) ->
    sysInitI:warning_report([{?MODULE, handle_info},
			     {unrecognized_msg, _Msg}]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{imm_slave = Pid, oi_handle = OiH}) ->
    %% This is serious overkill but anyway..
    case is_pid(Pid) of
	true ->
	    exit(Pid, kill);
	_False ->
	    comsaImm:finalize_ns_oi(OiH)
    end,
    ok.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           enable_com()
%%% Input:-
%%% Output:
%%% Exceptions:
%%% Description: Make all registrations towards com
%%% ----------------------------------------------------------
enable_com() ->
    System = ["ManagedElement", "SystemFunctions"],
    %% Support for COM models
    comsaLib:register_callback(["ManagedElement"], comTop),
    comsaLib:register_callback(System++["Fm"], comFm),
    comsaLib:register_callback(System++["SysM"], comSysM),
    comsaLib:register_callback(System++["SysM","Snmp"], comSnmp),
    %% All COM security related models are implemented in this module
    comsaLib:register_callback(System++["SecM"], rcsSecM),
    %% Support for the RcsUser model
    comsaLib:register_callback(
      System++["SecM", "UserManagement", "UserIdentity"], comsaUser),

%    comsaLib:register_callback(["ManagedElement", "NodeSupport"], comsaNodeSupport),
    comsaLib:register_callback(
      ["ManagedElement", "NodeSupport", "ExecutionResource"], rmeExeR),
    comsaLib:register_callback(
      ["ManagedElement", "NodeSupport", "ServiceDiscovery"], rmeSds),
    comsaLib:register_role_callback(comsaRoles),
    comsaLib:register_transaction_server(comsaTransactionServer).


%%% ----------------------------------------------------------
%%% #           wait_for_imm()
%%% Input:-
%%% Output:
%%% Exceptions:
%%% Description: Make all registrations towards com
%%% ----------------------------------------------------------
%% run by fun() in separately spawned process
wait_for_imm() ->
    sysInitI:info_msg("~p: Upgrade ongoing, begin waiting for IMM objects.~n", 
		      [?MODULE]),
    comsaImm:wait_for_upgrade_transform(),
    sysInitI:info_msg( "~p: IMM class NodeSupport available.~n", [?MODULE]),
    gen_server:cast(?MODULE, imm_ready).

%%% ----------------------------------------------------------
%%% #           send_buffered_alarms(AlarmParamList)
%%% Input: AlarmParamList - a list with tuples that holds alarm parameters that
%%%                         should be sent to COM
%%% Output:
%%% Exceptions:
%%% Description: COM is up and there are buffered alarms in the server to send.
%%% ----------------------------------------------------------
send_buffered_alarms(AlarmParamList) ->
    lists:foreach(fun send_buffered_alarm/1, AlarmParamList).

send_buffered_alarm({send_alarm, AlarmName, Severity, DN, Text, Info, AppAlarm,
		     PrgGrp}) ->
    comFm:send_alarm(AlarmName, Severity, DN, Text, Info, AppAlarm, PrgGrp);
send_buffered_alarm({clear_alarm, AlarmName, DN, Text, Info, AppAlarm}) ->
    comFm:clear_alarm(AlarmName, DN, Text, Info, AppAlarm);
send_buffered_alarm({clear_alarm_by_instance, MajorType, MinorType, DN}) ->
    comFm:clear_alarm_by_instance(MajorType, MinorType, DN).

%%% ----------------------------------------------------------
%%% #           handle_mnesia_table_event(MnesiaEvent)
%%% Input: MnesiaEvent - according to mnesia doc
%%% Output:
%%% Exceptions:
%%% Description: Handle any updates to the models owned by COMSA, which have
%%%              side effects
%%% ----------------------------------------------------------

handle_mnesia_table_event({write, managedElement, New, [Old], _}) ->
   case {New#managedElement.networkManagedElementId,
	 Old#managedElement.networkManagedElementId} of
       {Same, Same} ->
	   ok;
       {NewNetworkManagedElementId, _} ->
	   %% The networkManagedElementId has changed call SWM to see if it should be
	   %% written in the HWLOG or not
	   swmI:hw_log(NewNetworkManagedElementId)
   end;

handle_mnesia_table_event({write, Record, _})
                when element(1, Record) =:= snmp ->
    catch ootI:snmp_agent_update(comSnmp:snmp_agent_to_prop_list(Record)),
    ok;
handle_mnesia_table_event(_) ->
    ok.

%%% ----------------------------------------------------------
%%% #           set_timezone(TimeZone)
%%% Input: TimeZone:string()
%%% Output:
%%% Exceptions:
%%% Description: Sets the given string as time zone string. The string
%%%              must be recognized as a time zone string by the os.
%%% ----------------------------------------------------------

%%set_timezone(TimeZone) ->
%%    os:putenv("TZ", TimeZone),
%%    %% Cause erlang to evaluate the timezone environment variable
%%    erlang:localtime_to_universaltime(calendar:local_time(), true),
%%    case sysEnv:rcs_mode() of
%%	target ->
%%	    cmd(["cd /tmp; sudo set_timezone.sh -t ", TimeZone]);
%%	simulated ->
%%	    %% Cannot set linux system timezone
%%	    ok
%%    end.

%%% ----------------------------------------------------------
%%% #           handle_update_schema(Paths, IncDir)
%%% Input: Paths:[string()]
%%%        IncDir::string()
%%% Output:
%%% Exceptions:
%%% Description: Parses all model files for metadata
%%% ----------------------------------------------------------
handle_update_schema(Paths,IncDir) ->
    WwwPath = filename:join([sysEnv:www_doc_root(), "models"]),
    TmpPath = WwwPath++".tmp",
    Fun = fun() ->
		  Keys = mnesia:all_keys(sysMSchema),
		  [mnesia:delete({sysMSchema, Key})||Key<-Keys],
		  [add_schema_info(Path, IncDir, TmpPath)||
		      Path<-Paths,
		      filename:basename(Path) /= "ECIM_CommonLibrary.xml"
		  ],
		  ok
	  end,
    case mnesia:transaction(Fun) of
	{atomic, ok} ->
	    %% Mo shell support
	    generate_index("models", TmpPath),
	    os:cmd(["rm -rf ",WwwPath," ; mv ",TmpPath," ",WwwPath]),

	    ok;
	Nok->
	    sysInitI:error_report(
	      [{?MODULE, handle_update_schema, [Paths, IncDir]},
	       Nok])
    end.

add_schema_info(Path, IncDir, TmpPath) ->
    PatchesDir = sysEnv:dev_patches_dir(),
    IncDirs = [PatchesDir, IncDir],
    Options = [{validation, dtd}, {fetch_path, IncDirs}],
    try xmerl_scan:file(Path, Options) of
	Result ->
	    try do_add_schema_info(Path, Result, TmpPath)
	    catch _Type:{missing_attribute, Attr, Base} ->
		    sysInitI:warning_report(
		      [{?MODULE, add_schema_info},
		       {missing_attribute, Attr},
		       {mim, Base}]);
		  Type:Error ->
		    sysInitI:error_report(
		      [{Type, Error},
		       erlang:get_stacktrace()])
	    end
    catch  Type:Error ->
	    sysInitI:error_report(
	      [{Type, Error},
	       erlang:get_stacktrace()])
    end.

do_add_schema_info(Path, {TopE, _}, TmpPath) ->
    %% Mo support support: start
    %% Index is generated after all do_add_schema_info is run
    Basename =
	re:replace(filename:basename(Path), "_mp", "",[{return, list}]),
    WwwPath = filename:join(TmpPath, Basename),
    filelib:ensure_dir(WwwPath),
    try file:make_symlink(Path, WwwPath) of
	ok -> ok;
	{error, eexist} -> ok
    catch T:E ->
	    sysInitI:error_report(
	      [{mfa, {file, make_symlink, [Path, WwwPath]}},
	       {T, E},
	       erlang:get_stacktrace()])
    end,
    %% Mo shell support: end

    MimE = find_element(mim, TopE),

    Base = filename:basename(Path),

    Name = find_attribute(name, Base, MimE),
    MimVersion = find_attribute(version, Base, MimE),
    MimRelease = find_attribute(release, Base, MimE),
    MimCorrection = try find_attribute(correction, Base, MimE) of
			Corr -> Corr
		    catch _:_ ->
			    undefined
		    end,
    %% _Namespace = find_attribute(namespace, Base, MimE),

    Version = MimVersion++"."++MimRelease++case MimCorrection of
					       undefined -> "";
					       _ -> "."++MimCorrection
					   end,
    {BaseModelIdentifier, BaseModelVersion}  =
	try find_element(domainExtension, MimE) of
	    DomainExtensionE ->
		case find_attribute(domain, DomainExtensionE) of
		    "ECIM" ->
			Data = extract_extension_data(
				 DomainExtensionE#xmlElement.content),
			case find_ecim_data(ecimMomName, Path, Data) of
			    {missing_key, _} ->
				{"", ""};
			    {ok, BMid} ->
				{ok, BVersion} =
				    find_ecim_data(ecimMomVersion, Path, Data),
				{ok, BRelease} =
				    find_ecim_data(ecimMomRelease, Path, Data),
				{BMid, BVersion++"."++BRelease}
			end;
		    _ -> % We only support ECIM so far...
			{"", ""}
		end
	catch _:_ ->
		{"", ""}
	end,

    %% Location = filename:basename(Path),
    mnesia:write(#comsaSchemaLocations{model=Name, path=Path, file=Basename}),

    SelectedModelOptions =
	try find_element(modelFeatures, MimE) of
	    ModelFeaturesE ->
		case find_attribute(modelType, ModelFeaturesE) of
		    "Derived" ->
			[find_attribute(name, ModelFeatureE)||
			    ModelFeatureE<-ModelFeaturesE#xmlElement.content,
			    ModelFeatureE#xmlElement.name == modelFeature];
		    "Template" ->
			error_msg("~p is a template model~n", [Path]),
			erlang:error({template_model, Path})
		end
	catch _:_ ->
		[]
	end,

    Obj = #sysMSchema{schemaId = {"1","1","1",Name},
		      identifier = Name,
		      baseModelIdentifier = BaseModelIdentifier,
		      version = Version,
		      baseModelVersion = BaseModelVersion,
		      selectedModelOptions = SelectedModelOptions},
    ok = mnesia:write(Obj).

find_ecim_data(Key, _Path, Data) ->
    case lists:keysearch(Key, 1, Data) of
	{value, {Key, Value}} ->
	    {ok, Value};
	false ->
	    {missing_key, Key}
    end.

find_attribute(AttributeName, Base, Element)
  when is_record(Element, xmlElement) ->
    find_attribute(AttributeName, Base,Element#xmlElement.attributes);
find_attribute(AttributeName, Base, AttributeList) ->
    case lists:keysearch(AttributeName, #xmlAttribute.name, AttributeList) of
	{value, Attribute} ->
	    Attribute#xmlAttribute.value;
	false ->
	    throw({missing_attribute, AttributeName, Base})
    end.

extract_extension_data([ExtensionE|Content])
  when is_record(ExtensionE, xmlElement) ->
    Name = find_attribute(name, ExtensionE),
    Value = find_attribute(value, ExtensionE),
    [{list_to_atom(Name), Value}|extract_extension_data(Content)];
extract_extension_data([_|Content]) ->
    extract_extension_data(Content);
extract_extension_data([]) -> [].

%%% ----------------------------------------------------------
%%% #           find_element(ElementName, Element)
%%% #           find_element(ElementName, Content)
%%% Input: ElementName:atom()
%%%        Element:#xmlElement{} or
%%%        Content.[#xmlElement{}] a list of elements
%%% Output: #xmlElement{}
%%% Exceptions:
%%% Description: Finds a sub element to an xml element, or in a list
%%%              of element contents. Assumes there is only one element
%%%              with the same name
%%% ----------------------------------------------------------

find_element(ElementName, Element) when is_record(Element, xmlElement) ->
    find_element(ElementName, Element#xmlElement.content);
find_element(ElementName, ContentList) ->
    {value, Element} =
	lists:keysearch(ElementName, #xmlElement.name, ContentList),
    Element.

%%% ----------------------------------------------------------
%%% #           find_attribute(AttributeName, Element)
%%% #           find_attribute(AttributeName, AttributeList)
%%% Input: AttributeName:atom()
%%%        Element:#xmlElement{} or
%%%        AttributeList:[#xmlattribute{}] a list of xml attributes
%%% Output: Value:string()
%%% Exceptions:
%%% Description: Finds an attribute to an xml element, or in a list of
%%%              attributes and returns the value of the attribute
%%% ----------------------------------------------------------

find_attribute(AttributeName, Element) when is_record(Element, xmlElement) ->
    find_attribute(AttributeName, Element#xmlElement.attributes);
find_attribute(AttributeName, AttributeList) ->
    {value, Attribute} =
	lists:keysearch(AttributeName, #xmlAttribute.name, AttributeList),
    Attribute#xmlAttribute.value.

%%%-----------------------------------------------------------------------------
handle_avli_other_event(NodeIdReason) ->
    NodeIdInfo =
	swmI:get_current_up_metadata() ++ comsaI:get_managed_element_data(),
    AvailInfo =
	?ALH_TAG_RcsNodeIdentityInfo(NodeIdReason,
				     avli_get_NodeIdParam(NodeIdInfo,
							  siteLocation),
				     avli_get_NodeIdParam(NodeIdInfo,
							  networkManagedElementId),
				     avli_get_NodeIdParam(NodeIdInfo,
							  productNumber),
				     avli_get_NodeIdParam(NodeIdInfo,
							  productRevision),
				     avli_get_NodeIdParam(NodeIdInfo,
							  productName),
				     avli_get_upgradePackage(NodeIdInfo)),
    alhI:write_other_event(os:timestamp(),
			   undefined,
			   undefined,
			   AvailInfo,
			   async).

%%%-----------------------------------------------------------------------------
avli_get_NodeIdParam([{Tag, Value} | _], Tag) ->
    case Value of
	undefined ->
	    [];
	_ ->
	    Value
    end;
avli_get_NodeIdParam([_ | Tail], Tag) ->
    avli_get_NodeIdParam(Tail, Tag);
avli_get_NodeIdParam([], _) ->
    not_found.

avli_get_upgradePackage(NodeIdInfo) ->
    avli_get_NodeIdParam(NodeIdInfo, productNumber) ++
	"-" ++
	avli_get_NodeIdParam(NodeIdInfo, productRevision).


%%% ----------------------------------------------------------
%%% @doc Constructs a CorrelationInfo (CI) name-value tuple for
%%% the AditionalInfo list that is used in a send_alarm.
%%%
%%% This method was added as part of sp539, "Alarm Correlation",
%%% where we add CorrelationInfo like ComputeName and SystemUUID,
%%% to AdditionalInfo.
%%% @end
%%% ----------------------------------------------------------
-spec get_alarm_correlation_info(correlation_info_type())
    -> {Name::binary(), ValueJsonEncoded::binary()}. % The Name is alwayes <<"CI">>

get_alarm_correlation_info({}) ->
    DefCorrInfo = get_default_correlation_info(),
    {<<"CI">>, jsone:encode(DefCorrInfo)};

get_alarm_correlation_info({primary, CorrelationUUID}) ->
    DefCorrInfo = get_default_correlation_info(),
    CorrInfo = DefCorrInfo#{'P' => list_to_binary(CorrelationUUID)},
    {<<"CI">>, jsone:encode(CorrInfo)}.

get_default_correlation_info() ->
    C = #{'I' => list_to_binary(sysEnv:get_systemUUID()),
          'n' => list_to_binary(sysEnv:get_computeName())
        },
    CA = [C],
    #{'C' => CA}.


%%% ----------------------------------------------------------
%%% @doc Write a restart log entry, unless one was written
%%% just recently. The given monotonic time tells when the previous
%%% entry was written. The returned value is the same monotonic time
%%% if nothing was written, or the current monotonic time otherwise.
%%%
%%% "Recently" is since 2.5 s or less. This is well above the
%%% period of the timer that polls COM health when not yet started.
%%% @end
%%% ----------------------------------------------------------

-spec restart_logger_trace(non_neg_integer(), milli_seconds()|undefined) ->
	  milli_seconds().
restart_logger_trace(Line, Latest) ->
    Now = erlang:monotonic_time(milli_seconds),

    if
	%% Either first try or more than 2500 milliseconds
	Latest == undefined;
	Now-Latest >= 2500 ->
	    sysInitI:restart_logger_trace(?MODULE, Line, "COM is running"),
	    Now;
       true ->
	    Latest
    end.

%%% ----------------------------------------------------------
%%% Generates index.html for a directory.
%%% ----------------------------------------------------------
generate_index(Dir, Path) ->
    WebPath = filename:join(["/", Dir]),
    Extra =
	case Dir of
	    "models" -> models;
	    _ -> undefined
	end,
    {ok, Str} = do_generate_index(Path, WebPath, Extra),
    Index = filename:join([Path, "index.html"]),
    case file:open(Index, [write]) of
	{ok, Fd} ->
	    file:write(Fd, Str),
	    file:close(Fd);
	{error, Reason} ->
	    warning_msg("Generating index.html for ~p failed: ~p~n",
			[Dir, file:format_error(Reason)])
    end.

do_generate_index(Path, WebPath, Extra) ->
    case file:list_dir(Path) of
	{ok, FileList} ->
	    NewFileList = lists:delete("index.html", FileList),
	    SortedFileList = lists:sort(NewFileList),
	    {ok,[header(WebPath),
		 body(Path, WebPath, Extra, SortedFileList)]};
	{error, Reason} ->
	    {error, Reason}
    end.

header(WebPath) ->
    "<html>\n" ++
	"<head>\n" ++
	"<title>Index of " ++ WebPath ++ "</title>\n" ++
	"</head>\n" ++
	"<body>\n" ++
	"<h1>Index of " ++ WebPath ++ "</h1>\n" ++
	"<pre><img src=\"" ++
	"\" alt="     "> Name                            Last modified          Size <hr>\n".

body(Path, WebPath, Extra, SortedFileList) ->
    [format_body(Path, WebPath, Extra, File)||File<-SortedFileList].


format_body(Path, WebPath, Extra, File) ->
    Fn = filename:join([Path, File]),
    case file:read_file_info(Fn) of
	{ok, FileInfo} when FileInfo#file_info.type == directory ->
	    {{Year, Month, Day}, {Hour, Minute, _Second}} =
		FileInfo#file_info.mtime,
	    FileLength = length(File),
	    if
		FileLength > 31 ->
		    io_lib:format("<a href=\"~s\">~-21.s..</A>"
				  "~2.2.0w-~s-~w ~2.2.0w:~2.2.0w"
				  "        -\n",
				  [WebPath ++ "/" ++ File ++"/",
				   File,
				   Day, httpd_util:month(Month),
				   Year, Hour, Minute]);
		true ->
		    io_lib:format("<a href=\"~s\">~s</A>~*.*c~2.2.0"
				  "w-~s-~w ~2.2.0w:~2.2.0w        -\n",
				  [WebPath ++ "/" ++ File ++ "/",
				   File,
				   33-FileLength, 33-FileLength, $ , Day,
				   httpd_util:month(Month), Year, Hour, Minute])
	    end;
	{ok, FileInfo} ->
	    {{Year, Month, Day},{Hour, Minute,_Second}} =
		FileInfo#file_info.mtime,
	    FileLength = length(File),
	    ExtraInfo =
		case Extra of
		    models ->
			WP = mnesia:table_info(comsaSchemaLocations,
					       wild_pattern),
			Pattern = WP#comsaSchemaLocations{file= File},
			Schema =
			    case mnesia:dirty_match_object(Pattern) of
				[{_, S, _,_}] -> S;
				[] -> erlang:error({not_found, File},
						   [Path, WebPath, Extra, File])
			    end,
			Key = {"1","1","1",Schema},
			[SchemaMO] = mnesia:dirty_read({sysMSchema,Key}),
			BaseModelIdentifier =
			    SchemaMO#sysMSchema.baseModelIdentifier,
			BaseModelVersion =
			    SchemaMO#sysMSchema.baseModelVersion,
			Version = SchemaMO#sysMSchema.version,

			io_lib:format("Schema=~s "
				      "baseModelIdentifier=~p,"
				      "baseModelVersion=~p,"
				      "identifier=~p,"
				      "version=~p",
				      [Schema, BaseModelIdentifier,
				       BaseModelVersion,
				       Schema, Version]);
		    _ ->
		    	[]
		end,
	    if
		FileLength > 31 ->
		    io_lib:format("<a href=\"~s\">~-21.s..</A>~2.2.0"
				  "w-~s-~w ~2.2.0w:~2.2.0w~8wk  "++
				      ExtraInfo++"~n",
				  [WebPath ++ "/" ++ File,
				   File, Day,
				   httpd_util:month(Month), Year, Hour, Minute,
				   (FileInfo#file_info.size div 1024)+1]);
		true ->
		    io_lib:format("<a href=\"~s\">~s</A>~*.*c~2.2.0w-~s-~w"
				  " ~2.2.0w:~2.2.0w~8wk  "++
				      ExtraInfo++"~n",
				  [WebPath ++ "/" ++ File,
				   File, 33-FileLength,
				   33-FileLength, $ , Day,
				   httpd_util:month(Month), Year, Hour, Minute,
				   (FileInfo#file_info.size div 1024)+1])
	    end;
	{error, _Reason} ->
	    ""
    end.

%%% ----------------------------------------------------------
%%% info_msg
%%% ----------------------------------------------------------
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%%% ----------------------------------------------------------
%%% warning_msg
%%% ----------------------------------------------------------
warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%%% ----------------------------------------------------------
%%% error_msg
%%% ----------------------------------------------------------
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
