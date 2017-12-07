%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 1.    BASIC INFORMATION
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 1.1   MODULE INFORMATION
%% ###-----------------------------------------------------------------------###
%% %CCaseFile:	swmFtp.erl %
%% @author etxberb
%% @copyright Ericsson AB 2017
%% @version /main/R9A/R10A/R11A/4

%% @doc == Handling of file transfer channels ==
%%
%% @end

%% ###=======================================================================###
%% # 1.2   MODULE DEFINITION
%% ###-----------------------------------------------------------------------###
-module(swmFtp).
-vsn('/main/R9A/R10A/R11A/4').
-date('2017-10-04').
-author(etxberb).

%% ###=======================================================================###
%% # 1.3   LEGAL RIGHTS
%% ###-----------------------------------------------------------------------###
%% %CCaseTemplateFile:	module.erl %
%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%
%% %CCaseCopyrightBegin%
%% Copyright (c) Ericsson AB 2017 All rights reserved.
%% 
%% The information in this document is the property of Ericsson.
%% 
%% Except as specifically authorized in writing by Ericsson, the 
%% receiver of this document shall keep the information contained 
%% herein confidential and shall protect the same in whole or in 
%% part from disclosure and dissemination to third parties.
%% 
%% Disclosure and disseminations to the receivers employees shall 
%% only be made on a strict need to know basis.
%% %CCaseCopyrightEnd%

%% ###=======================================================================###
%% # 1.4   REVISION LOG
%% ###-----------------------------------------------------------------------###
%% Rev     Date       Name     What
%% ----    ---------- -------  -------------------------------------------------
%% R9A/1   2017-02-23 etxberb  Created.
%% R9A/2   2017-02-24 etxberb  Minor changes after code review.
%% R9A/5   2017-03-03 etxberb  Redesign - healthCheck_URIs introduced.
%% R9A/6   2017-03-07 etxberb  WARNING when alarm_not_registered.
%% R9A/7   2017-03-13 etxberb  Added defaultUri & defaultPassword.
%% R9A/8   2017-03-16 etxberb  HV71684: Added timeout in start_watchdog/1.
%% R9A/9   2017-03-17 etxberb  HV71684: Added error msg in start_watchdog/1.
%% R9A/10  2017-04-20 etxberb  Changed alarm name.
%% R9A/12  2017-05-10 etxberb  Added update_dets/2.
%% R9A/13  2017-05-16 etxberb  HV83608: WatchdogMode always healthCheck_URIs
%% R9A/14  2017-05-27 etxberb  HV90763: Correction of HV83608 fix.
%% R9A/15  2017-05-30 etxpejn  HV91631: Keep original Additional Text to alarm
%% R9A/16  2017-06-01 etxberb  HV92126: Added verified_URIs/0 & /1.
%% R9A/17  2017-06-08 etxpejn  HV91631: Addtional fix
%% R9A/18  2017-06-20 etxberb  HV91631 & HV96636: Added sysUtil:reason_phrase.
%% ----    ---------- -------  -------------------------------------------------
%% R10A/1  2017-06-28 etxjotj  Removed printout for mnesia_table_event
%% ----    ---------- -------  -------------------------------------------------
%% R11A/1  2017-08-29 etxberb  HW23830: Added set_default_attrs/0.
%% R11A/2  2017-08-30 etxberb  HW23830: Added mnesia:subscribe for 'swM'.
%% R11A/3  2017-09-01 etxberb  HW24159: Added validate_server_files/3.
%% R11A/4  2017-10-04 etxberb  HW33586: Added more readable reasons in
%%                             validate_channel/5.
%% ------  ---------- -------  ---END-OF-LOG------------------------------------

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 2.    INTERNAL DEFINITIONS
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 2.1.1 Interface functions
%% ###-----------------------------------------------------------------------###
%% Basic
-export([start_watchdog/1]).
-export([verified_URIs/0,
	 verified_URIs/1,
	 verify_URIs/0,
	 verify_URIs/1]).

%% sys interface
-export([system_code_change/4, 
	 system_continue/3,
	 system_get_state/1,
	 system_replace_state/2,
	 system_terminate/4]).

%% Test
-export([info/0,
	 info/1]).
-export([restart_watchdog/1]).
-export([stop_watchdog/1]).

%% ###-----------------------------------------------------------------------###
%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###
-export([init_watchdog/2]).

%% ###=======================================================================###
%% # 2.3   IMPORT OF DEFINITIONS
%% ###-----------------------------------------------------------------------###
-include("RcsSwM.hrl").
-include_lib("kernel/include/file.hrl").

%% ###=======================================================================###
%% # 2.4   LOCAL DEFINITION OF MACROS
%% ###-----------------------------------------------------------------------###
-define(AlarmName_SMLCFS,      'SoftwareManagementLostConnectiontoFileServer').
-define(AlarmsFalse,           alarms_false).
-define(AlarmsFalse_Interval,  timer:hms(24, 0, 0)).
-define(AlarmsTrue,            alarms_true).
-define(AlarmsTrue_Interval,   timer:hms(0, 30, 0)).
-define(TIME_mnesiaTransDelay, timer:hms(0, 0, 10)).
-define(TIME_VerifyNewObj,     ?AlarmsTrue_Interval).
-define(UpId(__UpKey), {"1", "1", "1", __UpKey}).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Usage of error_logger:XXX_report
-define(LOG_ERR(__ReportInfo),
	try
	    sysInitI:error_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:error_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_INFO(__ReportInfo),
	try
	    sysInitI:info_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:info_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_WARN(__ReportInfo),
	try
	    sysInitI:warning_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:warning_report(?RepInfo(__ReportInfo))
	end).
-define(RepInfo(__RepInfo),
	[{?MODULE, ?FUNCTION} | __RepInfo]).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).
-define(MODULE_STR, atom_to_list(?MODULE)).
-define(MonoTime, erlang:monotonic_time()).
-define(PROC_INFO(__Pid), sysUtil:pid_info(__Pid, {all, [error_handler]})).
-define(STATE_INFO(__Record),
	sysUtil:record_format(record_info(fields, state), __Record)).
-define(time2string(NativeTime),
	sysUtil:time_to_string(NativeTime, nano_seconds)).

%% ###=======================================================================###
%% # 2.5   LOCAL DEFINITION OF RECORDS
%% ###-----------------------------------------------------------------------###
-record(state,
	{name,
	 scheme,
	 intervalTimer,
	 log = true,
	 parent,
	 debug = []}).

%% ###=======================================================================###
%% # 2.6   LOCAL DEFINITION OF TYPES
%% ###-----------------------------------------------------------------------###
-type alrm_res() :: {alarm, Reason :: string()}.
-type err_res()  :: {error, Reason :: string()}.

-type result_verify_URIs() :: list({Watchdog  :: atom(),
				    WD_Result :: (list({UpId :: up_id(),
							ok | alrm_res()}) |
						  err_res())}).

-type scheme() :: sftp.

-type spec_verified_URIs() :: #{up_ids  => all | list(up_id()),
				schemes => all | list(scheme())}.

-type spec_verify_URIs() :: #{up_ids  => all | list(up_id()),
			      schemes => all | list(scheme()),
			      reply   => (false |
					  {true, Timeout :: integer()})}.
						% Timeout :: milliseconds

-type up_id()  :: {string(), string(), string(), string()}.

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 3.    CODE
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 3.1.1 Basic
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% @doc Start a watchdog process.
%%
%% @end
%% ###=======================================================================###
-spec start_watchdog(Scheme :: scheme()) ->
    {ok, pid()} | err_res().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start_watchdog(Scheme) ->
    Name = make_watchdog_name(Scheme),
    Parent = self(),
    Pid = spawn(?MODULE, init_watchdog, [#state{name = Name,
						scheme = Scheme,
						parent = Parent},
					 self()]),
    MonitorRef = erlang:monitor(process, Pid),
    T1 = ?MonoTime,
    receive
	{Pid, started} ->
	    ?LOG_INFO([{"Started", ?time2string(?MonoTime - T1)}]),
	    erlang:demonitor(MonitorRef),
	    {ok, Pid};
	{Pid, {error, _} = Error} ->
	    ?LOG_WARN([{"Dormant", ?time2string(?MonoTime - T1)}]),
	    erlang:demonitor(MonitorRef),
	    Error;
	{'DOWN', MonitorRef, process, Pid, Info} ->
	    case whereis(Name) of
		undefined ->
		    {error, Info};
		AlreadyStartedPid ->
		    ?LOG_INFO(["Already started"
			       | ?PROC_INFO(AlreadyStartedPid)]),
		    {ok, AlreadyStartedPid}
	    end
    after
	30000 ->
	    erlang:demonitor(MonitorRef),
	    Reason = "Timeout",
	    ?LOG_WARN([Reason | ?PROC_INFO(Pid)]),
	    {error, Reason}
    end.

%% #############################################################################
%% @doc URIs are verified by actual usage of the connection to the server.
%%
%% @end
%% ###=======================================================================###
-spec verified_URIs() ->
    ok |
	{error, Reason :: string()}.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
verified_URIs() ->
    verified_URIs(#{}).

%% ###=======================================================================###
-spec verified_URIs(Specification :: spec_verified_URIs()) ->
    ok |
	{error, Reason :: string()}.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
verified_URIs(Arg) when is_map(Arg) ->
    Default = #{up_ids  => all,
		schemes => all},
    Spec = maps:merge(Default, Arg),
    ?LOG_INFO([Spec]),
    Msg = {verified_URIs, Spec},
    try
	[send(make_watchdog_name(ST), Msg) || ST <- get_schemes(Spec)],
	ok
    catch
	throw : {uc_error, Reason} ->
	    {error, Reason}
    end.

%% #############################################################################
%% @doc Verify URIs.
%%
%% @end
%% ###=======================================================================###
-spec verify_URIs() ->
    Result :: result_verify_URIs() |
	{error, Reason :: string()}.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
verify_URIs() ->
    verify_URIs(#{}).

%% ###=======================================================================###
-spec verify_URIs(Specification :: spec_verify_URIs()) ->
    Result :: result_verify_URIs() |
	{error, Reason :: string()}.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
verify_URIs(Arg) when is_map(Arg) ->
    Default = #{up_ids  => all,
		schemes => all,
		reply   => {true, 30000}},
    Spec = maps:merge(Default, Arg),
    ?LOG_INFO([Spec]),
    try
	verify_URIs([make_watchdog_name(ST) || ST <- get_schemes(Spec)], Spec)
    catch
	throw : {uc_error, Reason} ->
	    {error, Reason}
    end.

%% ###-----------------------------------------------------------------------###
%% # 3.1.2 Test
%% ###-----------------------------------------------------------------------###
%%% ###########################################################################
%%% @doc Print the server process state and information.
%%%
%%% @end
%%% ###=====================================================================###
info() ->
    info(get_schemes(#{schemes => all})).

%%% ###=====================================================================###
info(Schemes) when is_list(Schemes) ->
    [info(Scheme) || Scheme <- Schemes];
info(Scheme) ->
    ProcName = make_watchdog_name(Scheme),
    try
	ProcName ! {?MODULE, info}
    catch
	_ : _ ->
	    ?LOG_INFO(?PROC_INFO(whereis(ProcName)))
    end.

%% #############################################################################
%% @doc Restart a watchdog process.
%%
%% @end
%% ###=======================================================================###
-spec restart_watchdog(Scheme :: scheme()) ->
    {ok, pid()} | err_res().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
restart_watchdog(Scheme) ->
    stop_watchdog(Scheme),
    start_watchdog(Scheme).

%% #############################################################################
%% @doc Stop a watchdog process.
%%
%% @end
%% ###=======================================================================###
-spec stop_watchdog(Scheme :: scheme()) ->
    ok.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stop_watchdog(Scheme) ->
    Pid = whereis(make_watchdog_name(Scheme)),
    MonitorRef = erlang:monitor(process, Pid),
    catch Pid ! stop,
    receive
	{'DOWN', MonitorRef, process, _, _} ->
	    ok
    after
	300000 ->
	    ?LOG_ERR(["Watchdog not responding. Force kill will be performed"
		      | ?PROC_INFO(Pid)]),
	    erlang:demonitor(MonitorRef),
	    exit(Pid, kill),
	    ok
    end.

%% ###=======================================================================###
%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% init_watchdog
%%
%% ###=======================================================================###
init_watchdog(State1, Starter) ->
    try
	erlang:register(State1#state.name, self())
    catch
	_ : _ ->
	    exit("Already started")
    end,
    try
	mnesia:subscribe({table, upgradePackage, detailed}),
	mnesia:subscribe({table, swM, detailed}),
	ok = set_default_attrs(),
	State2 = alarms_init(State1),
	InitState = intervalTimer_update(State2),
	?LOG_INFO(?STATE_INFO(InitState)),
	Starter ! {self(), started},
	stopped = main_watchdog(InitState)
    catch
	throw : alarm_not_registered ->
	    ?LOG_WARN([{alarm_not_registered, ?AlarmName_SMLCFS}]),
	    Starter ! {self(), {error, "Alarm not registered"}},
	    main_void();
	ErrClass : ErrReason ->
	    Stacktrace = erlang:get_stacktrace(),
	    ?LOG_ERR([{ErrClass, ErrReason},
		      "--- Stacktrace ---"
		      | Stacktrace] ++
		     ?STATE_INFO(State1) ++
		     ?PROC_INFO(self())),
	    erlang:unregister(State1#state.name),
	    {ErrClass, ErrReason}
    end.

%% ###=======================================================================###
%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 3.3.1 State machine
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% main_watchdog
%%
%% ###=======================================================================###
main_watchdog(#state{name = MyName, scheme = Scheme} = State) ->
    receive
	{verified_URIs, Spec} ->
	    UpIds = get_upIds(Spec),
	    State2 = alarms_update([{UpId, ok} || UpId <- UpIds], State),
	    main_watchdog(State2);
	{verify_URIs, Spec, ClientPid} ->
	    T1 = ?MonoTime,
	    {State1, Result} = verify_URIs_loop(Spec, State),
	    State2 = alarms_update(Result, State1),
	    send(ClientPid, {MyName, Result}),
	    ?LOG_INFO([{scheme, Scheme},
		       {spec, Spec},
		       {"URI verification", ?time2string(?MonoTime - T1)}
		       | Result]),
	    timer:send_after(?TIME_mnesiaTransDelay, intervalTimer_update),
	    main_watchdog(State2);
	{healthCheck_URIs, Spec, ClientPid} ->
	    T1 = ?MonoTime,
	    {State1, Result} = verify_URIs_loop(Spec, State),
	    State2 = health_update(Result, State1),
	    send(ClientPid, {MyName, Result}),
	    ?LOG_INFO([{scheme, Scheme},
		       {spec, Spec},
		       {"URI health check", ?time2string(?MonoTime - T1)}
		       | Result]),
	    timer:send_after(?TIME_mnesiaTransDelay, intervalTimer_update),
	    main_watchdog(State2);
	intervalTimer_update ->
	    ?LOG_INFO([intervalTimer_update]),
	     main_watchdog(intervalTimer_update(State));
	{mnesia_table_event, {write, upgradePackage, Obj, [OldObj], _}} ->
	    %% ======= Update of existing object =======
	    if State#state.log ->
		    %% This confirms that the subscription is active
		    %% No need to write every time
		    ?LOG_INFO([{mnesia_table_event, write},
			       "Write of upgradePackage object"]);
	       true ->
		    ok
	    end,
		    
	    update_dets(upgradePackage, Obj),
	    case is_uri_changed(Obj, OldObj) of
		false ->
		    main_watchdog(State#state{log=false});
		true ->
		    ?LOG_INFO([{mnesia_table_event, write},
			       "Uri / password changed",
			       {obj, Obj},
			       {oldObj, OldObj}]),
		    case erase(default_settings) of
			undefined ->
			    UpId = get_upIds(Obj),
			    Spec = #{up_ids  => [UpId],
				     schemes => [Scheme]},
			    ClientPid = undefined,
			    timer:send_after(0, {verify_URIs, Spec, ClientPid});
			ongoing ->
			    ok
		    end,
		    main_watchdog(State#state{log=false})
	    end;
	{mnesia_table_event, {write, upgradePackage, Obj, [] = _Old, _}} ->
	    %% ======= New object written =======
	    UpId = get_upIds(Obj),
	    ?LOG_INFO([{scheme, Scheme},
		       {"New UP, no checks needed", UpId}]),
	    main_watchdog(State);
	{mnesia_table_event, {delete, upgradePackage, _, Objects, _}} ->
	    %% ======= Object(s) deleted =======
	    Result = [{UpId, ok} || UpId <- get_upIds(Objects)],
	    ?LOG_INFO([{mnesia_table_event, delete}
		       | Result]),
	    NewState = alarms_update(Result, State),
	    main_watchdog(NewState);
	{mnesia_table_event, {write, swM, Obj, [_OldObj], _}} ->
	    F = fun() ->
			set_default_attrs(Obj)
		end,
	    transaction(F),
	    main_watchdog(State);
	stop ->
	    ?LOG_INFO([{self(), stopped} | ?STATE_INFO(State)]),
	    stopped;
	{?MODULE, info} ->
	    ?LOG_INFO(?STATE_INFO(State) ++ ?PROC_INFO(self())),
	    main_watchdog(State);
	{system, From, Msg} ->
	    Parent = State#state.parent,
	    Debug = State#state.debug,
	    sys:handle_system_msg(Msg, From, Parent, ?MODULE, Debug, State);
	UnrecMsg ->
	    ?LOG_WARN(["Unrecognized message", {msg, UnrecMsg}]),
	    main_watchdog(State)
    end.

%% #############################################################################
%% main_void
%%
%% ###=======================================================================###
main_void() ->
    receive
	stop ->
	    ?LOG_INFO([{self(), stopped}]),
	    stopped;
	UnrecMsg ->
	    ?LOG_WARN([{msg, UnrecMsg}]),
	    main_void()
    end.

%% ###-----------------------------------------------------------------------###
%% # 3.3.2 Help Functions
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% alarms2UpIds
%%
%% ###=======================================================================###
alarms2UpIds([Alarm | Tail]) ->
    [makeUpId(proplists:get_value(source, Alarm)) | alarms2UpIds(Tail)];
alarms2UpIds([]) ->
    [].

%% #############################################################################
%% alarms_init
%%
%% ###=======================================================================###
alarms_init(State) ->
    Alarms =
	try
	    comsaI:get_alarms(?AlarmName_SMLCFS)
	catch
	    ErrClass : ErrReason ->
		Stacktrace = erlang:get_stacktrace(),
		?LOG_INFO([{ErrClass, ErrReason} | Stacktrace]),
		throw(alarm_not_registered)
	end,
    Alarm_UpIds = alarms2UpIds(Alarms),
    Alarm_RemovedUpIds = Alarm_UpIds -- get_upIds_all(),
    [alarm_cease(Alarm_UpId, State) || Alarm_UpId <- Alarm_RemovedUpIds],
    State.

%% #############################################################################
%% alarms_update
%%
%% ###=======================================================================###
alarms_update([{UpId, Result} | Tail], State) ->
    case Result of
	ok ->
	    alarms_update(Tail, alarm_cease(UpId, State));
	{alarm, Reason}  ->
	    alarms_update(Tail, alarm_raise(UpId, Reason, State))
    end;
alarms_update([], State) ->
    State.

%% #############################################################################
%% alarm_cease
%%
%% ###=======================================================================###
alarm_cease(UpId, State) ->
    Dn = swmLib:make_dn(UpId),
    DnString = swmLib:make_dn_string(Dn),
    case is_alarm(?AlarmName_SMLCFS, DnString) of
	true ->
	    comsaI:clear_alarm(?AlarmName_SMLCFS, Dn);
	false ->
	    ok
    end,
    State.

%% #############################################################################
%% alarm_raise
%%
%% ###=======================================================================###
alarm_raise(UpId, Reason, State) ->
    comsaI:send_alarm(?AlarmName_SMLCFS,
		      warning,
		      swmLib:make_dn(UpId),
		      sysUtil:reason_phrase(Reason)),
    State.

%% #############################################################################
%% get_schemes
%%
%% ###=======================================================================###
get_schemes(Spec) ->
    case maps:get(schemes, Spec) of
	all ->
	    [sftp];
	Schemes ->
	    Schemes
    end.

%% #############################################################################
%% get_upIds
%%
%% ###=======================================================================###
get_upIds(Objects) when is_list(Objects) ->
    [get_upIds(Obj) || Obj <- Objects];
get_upIds(#upgradePackage{upgradePackageId = UpId}) ->
    UpId;
get_upIds(Spec) when is_map(Spec) ->
    case maps:get(up_ids, Spec) of
	all ->
	    get_upIds_all();
	UpIds ->
	    UpIds
    end.

%% #############################################################################
%% get_upIds_all
%%
%% ###=======================================================================###
get_upIds_all() ->
    mnesia:dirty_all_keys(upgradePackage).

%% #############################################################################
%% health_update
%%
%% ###=======================================================================###
health_update(_Result, State) ->
    ?LOG_INFO(["Not implemented yet"]),
    State.

%% #############################################################################
%% intervalTimer_send
%%
%% ###=======================================================================###
intervalTimer_send(Interval, #state{scheme = Scheme}, WatchdogMode, UpIds) ->
    Spec = #{up_ids  => UpIds,
	     schemes => [Scheme]},
    ClientPid = undefined,
    Msg = {WatchdogMode, Spec, ClientPid},
    {ok, TRef} = timer:send_interval(Interval, self(), Msg),
    TRef.

%% #############################################################################
%% intervalTimer_update
%%
%% ###=======================================================================###
intervalTimer_update(State) ->
    intervalTimer_update(State,
			 comsaI:get_alarms(?AlarmName_SMLCFS)).

%% ###=======================================================================###
intervalTimer_update(#state{intervalTimer = undefined} = S,
		     []) ->  % Alarms
    WatchdogMode = healthCheck_URIs,
    S#state{intervalTimer =
	    {?AlarmsFalse, intervalTimer_send(?AlarmsFalse_Interval,
					      S,
					      WatchdogMode,
					      all)}};
intervalTimer_update(#state{intervalTimer = undefined} = S,
		     [_ | _] = Alarms) ->
    WatchdogMode = verify_URIs,
    S#state{intervalTimer =
	    {?AlarmsTrue, intervalTimer_send(?AlarmsTrue_Interval,
					     S,
					     WatchdogMode,
					     alarms2UpIds(Alarms))}};
intervalTimer_update(#state{intervalTimer = {?AlarmsTrue, TRef}} = S,
		     []) ->  % Alarms
    WatchdogMode = healthCheck_URIs,
    timer:cancel(TRef),
    S#state{intervalTimer =
	    {?AlarmsFalse, intervalTimer_send(?AlarmsFalse_Interval,
					      S,
					      WatchdogMode,
					      all)}};
intervalTimer_update(#state{intervalTimer = {?AlarmsFalse, TRef}} = S,
		     [_ | _] = Alarms) ->
    WatchdogMode = verify_URIs,
    timer:cancel(TRef),
    S#state{intervalTimer =
	    {?AlarmsTrue, intervalTimer_send(?AlarmsTrue_Interval,
					     S,
					     WatchdogMode,
					     alarms2UpIds(Alarms))}};
intervalTimer_update(State, _) ->
    State.

%% #############################################################################
%% is_alarm
%%
%% ###=======================================================================###
is_alarm(AlarmType, DnString) ->
    Alarms = comsaI:get_alarms(AlarmType),
    lists:any(fun(Alarm) ->
		      case proplists:get_value(source, Alarm) of
			  DnString ->
			      true;
			  _ ->
			      false
		      end
	      end,
	      Alarms).

%% #############################################################################
%% is_uri_changed
%%
%% ###=======================================================================###
is_uri_changed(#upgradePackage{uri = Uri, password = Pwd},
	       #upgradePackage{uri = Uri, password = Pwd}) ->
    false;
is_uri_changed(_, _) ->
    true.

%% #############################################################################
%% makeUpId
%%
%% ###=======================================================================###
makeUpId(String) when is_list(String) ->
    list_to_tuple(makeUpId_values(string:tokens(String, "=,"))).

%% ###=======================================================================###
makeUpId_values([_, Value | Tail]) ->
    [Value | makeUpId_values(Tail)];
makeUpId_values([]) ->
    [].

%% #############################################################################
%% make_watchdog_name
%%
%% ###=======================================================================###
make_watchdog_name(Scheme) ->
    list_to_atom(?MODULE_STR ++ "_" ++ sysUtil:term_to_string((Scheme))).

%% #############################################################################
%% send
%%
%% ###=======================================================================###
send(undefined, _) ->
    ok;
send(Pid, Msg) when is_pid(Pid) ->
    Pid ! Msg;
send(RegName, Msg) when is_atom(RegName) ->
    try
	whereis(RegName) ! Msg
    catch
	_ : _ ->
	    ?LOG_ERR([{"Process not registered", RegName}])
    end.

%% #############################################################################
%% set_default_attrs
%%
%% ###=======================================================================###
set_default_attrs() ->
    UpId = swmInventory:make_currentUP_key(),
    F = fun() ->
		set_default_attrs(UpId)
	end,
    transaction(F).

%% ###=======================================================================###
set_default_attrs(#swM{} = SwM) ->
    UpId = swmInventory:make_currentUP_key(),
    set_default_attrs(SwM, UpId);
set_default_attrs(UpId) ->
    [SwM] = mnesia:read(swM, {"1","1","1"}),
    set_default_attrs(SwM, UpId).

%% ###=======================================================================###
set_default_attrs(#swM{defaultUri = DefaultUri,
		       defaultPassword = DefaultPwd},
		  UpId)
  when DefaultUri /= undefined ->
    case mnesia:read(upgradePackage, UpId) of
	[#upgradePackage{uri = undefined,
			 password = undefined} = Obj] ->
	    ?LOG_INFO([{upId, UpId},
		       {defaultUri, DefaultUri},
		       {defaultPassword, DefaultPwd}]),
	    put(default_settings, ongoing),
	    mnesia:write(Obj#upgradePackage{uri = DefaultUri,
					    password = DefaultPwd});
	_ ->
	    ok
    end;
set_default_attrs(_, _) ->
    ok.

%% #############################################################################
%% transaction
%%
%% ###=======================================================================###
transaction(F) ->
    case mnesia:is_transaction() of
	false ->
	    case mnesia:transaction(F) of
		{atomic, Result} ->
		    Result;
		Aborted ->
		    Aborted
	    end;
	true ->
	    F()
    end.

%% #############################################################################
%% update_dets
%%
%% ###=======================================================================###
update_dets(TblName, Obj) ->
    try
	ok = dets:insert(TblName, Obj)
    catch
	ExcClass : ExcReason ->
	    Stacktrace = erlang:get_stacktrace(),
	    ?LOG_ERR([{tblName, TblName},
		      {obj, Obj},
		      {ExcClass, ExcReason},
		      {stacktrace, Stacktrace}])
    end.

%% #############################################################################
%% validate_channel
%%
%% ###=======================================================================###
validate_channel(Scheme, Host, Port, User, Pwd) ->
    case ftpI:start_channel(Scheme, Host, Port, User, Pwd) of
	{ok, _, _} = Channel ->
	    Channel;
	{error, etimedout} ->
	    throw({uc_error, "Connection to remote server timed out"});
	{error, closed} ->
	    throw({uc_error, "The remote server closed the connection"});
	{error, Reason} when is_integer(Reason) orelse is_atom(Reason) ->
	    throw({uc_error, "Remote server, "++sysUtil:reason_phrase(Reason)});
	{error, {options, {socket_options, _Opts}}} = Reason ->   % HW33586
	    ?LOG_WARN([{error, Reason}]),
	    throw({uc_error, "Wrong IP version to remote server"});
	{error, Reason} ->
	    ?LOG_WARN([{error, Reason}]),
	    throw({uc_error, "Cannot establish a connection to remote server"})
    end.

%% #############################################################################
%% validate_filepath
%%
%% ###=======================================================================###
validate_filepath(Path) ->
    case file:read_file_info(Path) of
	{ok, #file_info{type = Type}} when Type /= directory ->
	    throw({uc_error, "URI file path: not a directory"});
	{ok, #file_info{access = Access}} when Access /= read_write andalso
					       Access /= write ->
	    throw({uc_error, "URI file path: no write access"});
	{ok, #file_info{}} ->
	    ok;
	{error, Reason} ->
	    {uc_error, "URI file path: " ++ sysUtil:reason_phrase(Reason)}
    end.

%% #############################################################################
%% validate_pwd
%%
%% ###=======================================================================###
validate_pwd(Pwd) ->
    try comsaI:decrypt_password(Pwd) of
	String when is_list(String) ->
	    {ok, String};
	{error, Reason} ->
	    throw({uc_error, "Password: " ++ sysUtil:reason_phrase(Reason)})
    catch
	_ : _ ->
	    throw({uc_error, "Password: Unknown encryption format"})
    end.

%% #############################################################################
%% validate_server
%%
%% ###=======================================================================###
validate_server(Scheme, ChannelPid, Dir, ArchiveDir) ->
    FileInfo =
	case ssh_sftp:read_file_info(ChannelPid, Dir, 10000) of
	    {ok, FI} ->
		FI;
	    {error, Reason} ->
		UcReason =
		    "On file server, " ++ sysUtil:reason_phrase(Reason, Dir),
		throw({uc_error, UcReason})
	end,
    case FileInfo#file_info.type of
	directory ->
	    RemoteDir = Dir,
	    BaseNames = validate_server_dir(Scheme, ChannelPid, Dir, 10000),
	    ok = validate_server_files(ArchiveDir, RemoteDir, BaseNames);
	regular ->
	    throw({uc_error, "On file server, URI path is not a directory"})
    end.

%% ###=======================================================================###
validate_server_dir(Scheme, ChannelPid, Path, Timeout) ->
    Mod =
	case Scheme of
	    sftp ->
		ftpI;
	    ftpes ->
		ftpesI
	end,
    case Mod:list_dir(Scheme, ChannelPid, Path, Timeout) of
	{ok, Listing} ->
	    Listing -- [".", ".."];
	{error, Reason} ->
	    throw({uc_error,
		   "File transfer error: " ++ sysUtil:reason_phrase(Reason,
								    Path)})
    end.

%% ###=======================================================================###
validate_server_files(ArchiveDir, RemoteDir, RemoteBaseNames) ->
    {ok, [{products, ProductsOTH}]} =
	swmBoardList:swp([products],
			 [{boardTypesOTH, all},
			  {options, [{global, ArchiveDir}]}]),
    UpOthFileNames = [FileName || {_, {global, FileName}} <- ProductsOTH],
    case UpOthFileNames -- RemoteBaseNames of
	[] ->
	    ok;
	MissingFiles ->
	    ?LOG_WARN([{archiveDir, ArchiveDir},
		       {remoteDir, RemoteDir},
		       "------- Files missing on the server -------"
		       | [{lmc, MF} || MF <- MissingFiles]] ++
		      ["------- Existing server files -------"
		       | RemoteBaseNames]),
	    UcReason =
		"On file server, number of LMC files not found: " ++
		integer_to_list(length(MissingFiles)),
	    throw({uc_error, UcReason})
    end.

%% #############################################################################
%% validate_uri
%%
%% ###=======================================================================###
validate_uri(Uri) ->
    validate_uri(Uri, []).

validate_uri(Uri, ParseOpts) ->
    case http_uri:parse(Uri, ParseOpts) of
	{ok, {file = _Scheme, [], [], _Port, _Path, _Query} = Parsed} ->
	    {ok, Parsed};
	{ok, {file = _Scheme, _UserInfo, _Host, _Port, Path, _Query}} ->
	    throw("Malformed uri: " ++ Path);
	{ok, Parsed} ->
	    {ok, Parsed};
	{error, {no_default_port, file, _Uri}} ->
	    %% Reported to support@erlang.ericsson.se: [seq13257].
	    %% TODO: Remove this case clause when OTP fixed!
	    validate_uri(Uri, [{scheme_defaults, [{file, 9999}]}]);
	{error, {malformed_url, _Scheme, AbsURI}} ->
	    throw({uc_error, "Malformed uri: " ++ AbsURI});
	{error, {no_default_port, Scheme, _AbsURI}} ->
	    S = sysUtil:term_to_string(Scheme),
	    throw({uc_error, "The system has no default port for " ++ S});
	{error, no_scheme} ->
	    throw({uc_error, "The uri has no scheme or it is unknown"});
	{error, Reason} ->
	    throw({uc_error, sysUtil:reason_phrase(Reason, Uri)})
    end.

%% #############################################################################
%% verify_URI
%%
%% ###=======================================================================###
verify_URI(Uri, Pwd, ArchiveDir, #state{scheme = Scheme} = State) ->
    try
	{ok, DecryptPwd} = validate_pwd(Pwd),
	case validate_uri(http_uri:decode(Uri)) of
	    {ok, {Scheme, User, Host, Port, RemoteDir, _}}
	    when Scheme == sftp orelse
		 Scheme == ftpes ->
		{ok, ChPid, ChRef} =
		    validate_channel(Scheme, Host, Port, User, DecryptPwd),
		try
		    ok = validate_server(Scheme, ChPid, RemoteDir, ArchiveDir)
		after
		    ftpI:stop_channel(Scheme, ChPid),
		    ssh:close(ChRef)
		end;
	    {ok, {Scheme, _, _, _, Path, _}} when Scheme == file ->
		ok = validate_filepath(Path);
	    {ok, _} ->
		ok
	end,
	{ok, State}
    catch
	throw : {uc_error, Reason} ->
	    {{alarm, sysUtil:reason_phrase(Reason)}, State};
	  ErrClass : ErrReason ->
	    Stacktrace = erlang:get_stacktrace(),
	    Result = {alarm, "Software error"},
	    ?LOG_ERR([{ErrClass, ErrReason} | Stacktrace]),
	    {Result, State}
    end.

%% #############################################################################
%% verify_URIs
%%
%% ###=======================================================================###
verify_URIs(Watchdogs, Spec) ->
    verify_URIs(Watchdogs, Spec, [], []).

%% ###=======================================================================###
verify_URIs([Watchdog | Tail], Spec, WDs, WD_Results) ->
    {NewWDs, NewWD_Results} =
	try
	    whereis(Watchdog) ! {verify_URIs, Spec, self()},
	    {[Watchdog | WDs], WD_Results}
	catch
	    _ : _ ->
		{WDs, [{Watchdog, {error, "Watchdog not found"}} | WD_Results]}
	end,
    verify_URIs(Tail, Spec, NewWDs, NewWD_Results);
verify_URIs([], Spec, WDs, WD_Results) ->
    verify_URIs_wait(WDs, Spec, WD_Results).

%% ###=======================================================================###
verify_URIs_loop(Spec, State) ->
    F = fun() ->
		verify_URIs_loop(get_upIds(Spec), State, [])
	end,
    case mnesia:transaction(F) of
	{atomic, Result} ->
	    Result;
	Aborted ->
	    Aborted
    end.

%% ###=======================================================================###
verify_URIs_loop([UpId | Tail], State, AccRes) ->
    case mnesia:read(upgradePackage, UpId) of
	[#upgradePackage{uri = Uri,
			 password = Pwd} = Obj] when Uri /= undefined ->
	    ArchiveDir = swmServer:get_up_archive_dir(Obj),
	    {Result, NewState} = verify_URI(Uri, Pwd, ArchiveDir, State),
	    verify_URIs_loop(Tail, NewState, [{UpId, Result} | AccRes]);
	[Obj] ->
	    case mnesia:read(swM, {"1","1","1"}) of
		[#swM{defaultUri = DefaultUri, defaultPassword = DefaultPwd}]
		when DefaultUri /= undefined ->
		    mnesia:write(Obj#upgradePackage{uri = DefaultUri,
						    password = DefaultPwd}),
		    verify_URIs_loop([UpId | Tail], State, AccRes);
		_ ->
		    Result = {alarm, "Uri missing"},
		    verify_URIs_loop(Tail, State, [{UpId, Result} | AccRes])
	    end
    end;
verify_URIs_loop([], State, AccRes) ->
    {State, AccRes}.

%% ###=======================================================================###
verify_URIs_wait([_ | _] = RemainingWDs,
		 #{reply := {true, Timeout}} = Spec,
		 WD_results) ->
    receive
	{Watchdog, Result} ->
	    verify_URIs_wait(RemainingWDs -- [Watchdog],
			     Spec,
			     [{Watchdog, Result} | WD_results])
    after
	Timeout ->
	    [{WD, {error, "Timeout"}} || WD <- RemainingWDs] ++ WD_results
    end;
verify_URIs_wait(_, _, WD_results) ->
    WD_results.

%% ###=======================================================================###
system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.

system_continue(_Parent, _Debug, State) ->    
    ?MODULE:main_watchdog(State).

system_get_state(State) ->
    {ok, State}.

system_replace_state(StateFun, State) ->
    {ok, StateFun(State), StateFun(State)}.

system_terminate(_Reason, _Parent, _Debug, _State) ->
    ok.
    


%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 4     CODE FOR TEMPORARY CORRECTIONS
%% ###-----------------------------------------------------------------------###
