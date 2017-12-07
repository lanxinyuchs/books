%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmDbMonitor.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R4A/R5A/11

%%% @doc ==Failsafe configuration support==
%%% @end

-module(swmDbMonitor).
-behaviour(gen_server).
-vsn('/main/R4A/R5A/11').
-date('2016-02-29').
-author('etxjotj').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
%%% -----      ---------  --------    ------------------------
%%% R4A/1      2015-08-17 etxjotj     Created
%%% R4A/2      2015-09-24 etxjotj     Exemption handling
%%% R4A/8      2015-10-06 etxjotj     Removed autobackups for now
%%% R4A/9      2015-10-08 etxjotj     Flush events
%%% R4A/10     2015-10-09 etxarnu     Bug fix in force_auto_backup
%%% R4A/11     2015-10-12 etxjotj     Another bugfix in force auto backup
%%% R4A/14     2015-10-22 etxjotj     Make auto backup if there is none
%%% R4A/15     2015-10-23 etxjtoj     Backup for cluster
%%% R4A/16     2015-10-26 etxarnu     Reverted Backup for cluster
%%% R4A/17     2015-10-26 etxarnu     Backup for cluster again
%%% R5A/4      2015-11-11 etxjotj     Merge from R4A/18
%%% R5A/5      2016-01-11 etxjotj     Quarantine mechanism
%%% R5A/6      2016-01-11 etxjotj     Bugfix in quarantine timeout timer
%%% R5A/7      2016-01-14 etxjotj     Quarantine limit set to 0 (temporarily)
%%% R5A/8      2016-01-14 etxjotj     Quarantine reset API
%%%                                   Reverted quarantine timeout
%%% R5A/9      2016-01-26 etxjotj     Removed quarantine timeout again
%%% R5A/10     2016-02-05 etxjotj     Added quarantine again
%%%                                   Added 5 mins waiting period after q-reset
%%% R5A/11     2016-02-29 etxjotj     Bugfix
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0, activate/0]).
-export([register_exemption/2, force_auto_backup/0]).
-export([quarantine_reset/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-export([test_loop/1, test_loop_cancel/0, test_func/0]).

%-include("RcsBrM.hrl").
-include("SwmInternal.hrl").

-compile([nowarn_unused_vars, nowarn_unused_function]).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Start the swmDbMonitor server process
%%% @end
%%% ----------------------------------------------------------
start() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

activate() ->   
    ok.

register_exemption(Table, Field) ->
    case mnesia:transaction(
	   fun() -> 
		   Obj = #swmDbMonitorExemptions{table=Table, field=Field},
		   mnesia:write(Obj)
	   end) of
	{atomic, ok} -> 
	    ok;
	{aborted, Reason} ->
	    error_msg("Register exemption failed: ~p~n",[{aborted, Reason}]),
	    erlang:error({aborted, Reason}, [Table, Field])
    end.

force_auto_backup() ->
    gen_server:call(?MODULE, force, infinity),
    swmLib:sync().

quarantine_reset() ->
    gen_server:cast(?MODULE, quarantine_reset).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
-define(Q_PERIOD_DEFAULT, 30). 

-record(state, {timerRef,     %% The current db timer or 'undefined'
		qPeriod = ?Q_PERIOD_DEFAULT,  %% The next period length
		qEndTime=0,   %% The next quarantine period end time
		              %% as measured in monotonoic time
		qReset=true,  %% Flag indicating if qPeriod should be reset
		qTimer,       %% Quaratine timer or 'undefined'
		qWait=0       %% Raise quarantine only if past this time
	       }).

init(Args) ->
    ok = gen_server:cast(self(), {initialize, Args}),
    {ok, initializing}.

initialize(Args) ->
    case sysInitI:ets_new(swmDbMonitor, [set, public, named_table]) of
	{ok, _} ->
	    ets:insert(swmDbMonitor, {changes, 0}),
	    ets:insert(swmDbMonitor, {latestBackup, {{1970,1,1},{0,0,0}}}),
	    ets:insert(swmDbMonitor, {latestChange, {{1970,1,1},{0,0,0}}});
	{error, badarg} ->
	    %% Table already present, this is a local restart
	    ok
    end,
    Tables = mnesia:system_info(tables),
    Permanent = 
	[Table||Table<-Tables, 
		case {mnesia:table_info(Table, disc_only_copies), 
		      mnesia:table_info(Table, disc_copies)} of 
		    {[], []} -> false; 
		    _-> true 
		end],
    %% info_msg("Starting subscriptions on:~n~p~n",[lists:sort(Permanent)]),
    [mnesia:subscribe({table, Table, detailed})||Table<-Permanent],
    Path = filename:join(sysEnv:home_dir(), "autobackup.gz"),
    case filelib:is_file(Path) of
	true ->
	    ok;
	false ->
	    ok = make_auto_backup()
    end,
    {noreply, #state{}}.

handle_call(activate, _, State) ->
    {reply, ok, State};

handle_call(force, _, State) ->
    {Reply, NewState} = handle_force_backup(State),
    {reply, Reply, NewState}.
                      
handle_cast({initialize, Args}, initializing) ->
    initialize(Args);
handle_cast(quarantine_reset, State) ->
    NewQTimer = 
	case State#state.qTimer of
	    undefined -> undefined;
	    QTimer ->
		timer:cancel(QTimer),
		{ok, Tref} = timer:send_after(30000, quarantine_timeout),
		Tref
	end,
    logI:write_log("SwmInternal", "swmDbMonitor", info, "Quarantine reset"),
    {noreply, State#state{qPeriod = ?Q_PERIOD_DEFAULT,
			  qTimer = NewQTimer,
			  qReset = true,
			  qEndTime = 0,
			  qWait = erlang:monotonic_time()+300*1000000000
			 }}.


handle_info(write_timeout, State) ->
    case is_backup_allowed() of
	true ->
	    handle_write_timeout(State);
	false ->
	    {ok, Tref} = timer:send_after(30000, write_timeout),
	    {noreply, State#state{timerRef=Tref}}
    end;

handle_info(quarantine_timeout, State) ->
    Msg = format_msg("Quarantine expired",[]),
    logI:write_log("SwmInternal", "swmDbMonitor", info, Msg),
    case handle_force_backup(State) of
	{ok, NewState} ->
	    {noreply, NewState#state{qTimer = undefined}};
	{{error,busy}, NewState} ->
	    {ok, Tref} = timer:send_after(30000, quarantine_timeout),
	    {noreply, NewState#state{qTimer = Tref}}
    end;

%% Permanent exceptions
handle_info({mnesia_table_event, {write, swmVariables, 
				  {swmVariables, bu_restore, _},
				  _, _}}, State) ->
    {noreply, State};
handle_info({mnesia_table_event, {delete, swmVariables, 
				  {swmVariables, bu_restore}, _, _}}, State) ->
    {noreply, State};
handle_info({mnesia_table_event, {write, _, Same, [Same], _}}, State) ->
    {noreply, State};
handle_info({mnesia_table_event, {write, Table, New, [Old], _}=E}, State) ->
    case mnesia:dirty_read(swmDbMonitorExemptions, Table) of
	[] ->
	    debug(E),
	    ChangedTable = 
		case Table of
		    imm_objects ->
			extract_imm_table(E);
		    _ -> Table
		end,
	    NewState = set_burst_timer(State, ChangedTable),
	    {noreply, NewState};
	[Obj] ->
	    Field = Obj#swmDbMonitorExemptions.field,
	    case {setelement(Field, New, '$swmDbMonitor'),
		  setelement(Field, Old, '$swmDbMonitor')} of
		{Same, Same} -> %% This change is exempted
		    {noreply, State};
		_ ->
		    debug(E),
		    NewState = set_burst_timer(State, Table),
		    {noreply, NewState}
	    end
    end;
		    
handle_info({mnesia_table_event, E}, State) ->
    debug(E),
    Table = case E of
		{write, T, _, _, _} ->
		    T ;
		{delete, T, _, _, _} ->
		    T
	    end,
    ChangedTable = 
	case Table of
	    imm_objects ->
		extract_imm_table(E);
	    _ -> Table
	end,
    NewState = set_burst_timer(State, ChangedTable),
    {noreply, NewState};

handle_info(_Request, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           handle_write_timeout(State)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 30 seconds has passed since db update. Make an autobackup
%%%              Maybe setup quarantine
%%% ----------------------------------------------------------

handle_write_timeout(State) ->
    [{changes, Changes}] = ets:lookup(swmDbMonitor, changes),
    Msg = format_msg("Auto backup due to ~w changes",[Changes]),
    logI:write_log("SwmInternal", "swmDbMonitor", info, Msg),
    case make_auto_backup() of
	ok -> 
	    ets:insert(swmDbMonitor, {changes, 0}),
	    ets:insert(swmDbMonitor, {latestBackup, 
				      calendar:universal_time()}),
	    setup_quarantine(erlang:monotonic_time(), State);
	error ->
	    {ok, Tref} = timer:send_after(30000, write_timeout),
	    {noreply, State#state{timerRef=Tref}}
    end.

%%% ----------------------------------------------------------
%%% #           setup_quarantine(MT, State)
%%% Input: MT:integer() - monotonic time in nanoseconds
%%% Output: 
%%% Exceptions: 
%%% Description: Setup quarantine unless we're in the 5 min wait period after
%%%              a forced quarantine reset
%%% ----------------------------------------------------------


setup_quarantine(MT, State) when MT>State#state.qWait->
    
    %% Set up quarantine period
    NewQEndTime = MT+State#state.qPeriod*1000000000, 
    %% The next quarantine should be twice as long but no longer than
    %% a full day
    NewQPeriod = case State#state.qPeriod*2 of
		     NQP when NQP > 86400 ->
			 86400;
		     NQP -> 
			 NQP
		 end,
    
    QMsg = format_msg("Quarantine set for ~w seconds",[State#state.qPeriod]),
    logI:write_log("SwmInternal", "swmDbMonitor", info, QMsg),
    {noreply, State#state{timerRef=undefined,
			  qEndTime = NewQEndTime,
			  qPeriod = NewQPeriod,
			  qReset = true
			 }};
setup_quarantine(MT, State) ->
    %% In wait period after quarantine reset
    %% Don't set quarantine
    QMsg = "No quarantine in wait period",
    logI:write_log("SwmInternal", "swmDbMonitor", info, QMsg),
    {noreply, State#state{timerRef = undefined,
			  qEndTime = MT,
			  qReset = true}}.


%%% ----------------------------------------------------------
%%% #           extract_imm_table(E)
%%% Input: E - mnesia table event content
%%% Output: 
%%% Exceptions: 
%%% Description: When the table imm_objects is updated extract the info about
%%%              which IMM object that has changed
%%% ----------------------------------------------------------


extract_imm_table(E) ->
    try do_extract_imm_table(E) of
	ImmTab -> ImmTab
    catch _:_ ->
	    {imm_objects, unknown}
    end.

do_extract_imm_table({write, _, Ldap, _, _}) ->
    {imm_objects, element(5, Ldap)};
do_extract_imm_table({delete, _, What, _, _}) ->
    {imm_objects, delete}.

%%% ----------------------------------------------------------
%%% #           set_burst_timer(State, Table)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Wait 30 seconds to allow collection of subsequent mnesia
%%%              transactions
%%%              If db update occurs while in quarantine, the burst timer
%%%              is not activated, but a quarantine timer
%%% ----------------------------------------------------------


set_burst_timer(State, Table) ->
    ets:update_counter(swmDbMonitor, changes, 1),
    ets:insert(swmDbMonitor, {latestChange, calendar:universal_time()}),
    case State#state.timerRef of
	undefined -> 
	    case erlang:monotonic_time() of
		MonoCurrent when MonoCurrent > State#state.qEndTime ->
		    %% Not in quarantine 
		    %% Set a burst timer
		    do_set_burst_timer(State, Table);
		_ -> 
		    %% In quarantine Since events have been received
		    %% in quarantine, no reset of quarantine period
		    %% will occur next time
		    set_quarantine_timer(State, Table)
	    end;
	_ -> %% Timer is already ongoing
	    State
    end.
	    

do_set_burst_timer(State, Table) ->
    %% Not in quarantine Set a timer
    TimerRef = 
	case State#state.timerRef of
	    undefined -> 
		{ok, Tref} = timer:send_after(30000, write_timeout),
		Tref;
	    _ -> 
		undefined
	end,
    %% Log 
    case ets:lookup(swmDbMonitor, changes) of
	[{changes, Changes}] when Changes =< 5 ->
	    Msg = format_msg("~p changed",[Table]),
	    logI:write_log("SwmInternal", "swmDbMonitor", info, Msg);
	_ ->
	    ok
    end,

    %% Don't do this for other events in queue
    flush_events(),

    %% If no events have been received while in quarantine, the 
    %% quarantine period is reset to 10 sec
    NewQPeriod = case State#state.qReset of
		     true -> ?Q_PERIOD_DEFAULT;
		     false -> State#state.qPeriod
		 end,
    State#state{timerRef=TimerRef,
		qPeriod = NewQPeriod}.

flush_events() ->
    receive 
	{mnesia_table_event, _} ->
	    ets:update_counter(swmDbMonitor, changes, 1),
	    ets:insert(swmDbMonitor, {latestChange, calendar:universal_time()}),
	    flush_events()
    after 0 ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% #           set_quarantine_timer(State, Table)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Fire a timer when the quarantine period expires to cause
%%%              an autobackup at the end of the quarantine period
%%% ----------------------------------------------------------

set_quarantine_timer(State, Table) ->
    TimerRef = 
	case State#state.qTimer of
	    undefined ->
		Timeout = (State#state.qEndTime - erlang:monotonic_time()) div
		    1000000,
		Msg = format_msg("Change in ~w, while in quarantine which "
				 "expires in ~w seconds",[Table, Timeout/1000]),
		logI:write_log("SwmInternal", "swmDbMonitor", warning, Msg),
		{ok, Tref} = timer:send_after(Timeout, quarantine_timeout),
		Tref;
	    _ ->
		State#state.qTimer			      
	end,
    State#state{qReset = false,
		qTimer = TimerRef}.

%%% ----------------------------------------------------------
%%% #           handle_force_backup(State)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: DO a forced backup
%%% ----------------------------------------------------------

handle_force_backup(State) ->
    case is_backup_allowed() of
	true ->
	    [{changes, Changes}] = ets:lookup(swmDbMonitor, changes),
	    case Changes of 
		0 -> {ok, State};
		_ -> 
		    Msg = "Forced auto backup. "++
			integer_to_list(Changes)++" changes",
		    logI:write_log(
		      "SwmInternal", "swmDbMonitor", info, Msg),
		    timer:cancel(State#state.timerRef),
		    case make_auto_backup() of
			ok -> 
			    ets:insert(swmDbMonitor, {changes, 0}),
			    ets:insert(
			      swmDbMonitor, 
			      {latestBackup, calendar:universal_time()}),
			    {ok, State#state{timerRef=undefined}};
			error ->
			    {ok, Tref} = timer:send_after(30000, write_timeout),
			    {ok, State#state{timerRef=Tref}}
		    end
	    end;
	false ->
	    {{error, busy}, State}
    end.

%%% ----------------------------------------------------------
%%% #           is_backup_allowed()
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: This funciton intends to check for occations where backup
%%%              making is inappropriate, for example during manual or 
%%%              scheduled backups or upgrade
%%% ----------------------------------------------------------

is_backup_allowed() ->
    case os:timestamp() of
	{0,0,0} -> false;
	_ -> true
    end.

%%% ----------------------------------------------------------
%%% #           make_auto_backup()
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Standard backup procedure
%%% ----------------------------------------------------------



make_auto_backup() ->
    flush_events(),
    TmpPath = filename:join(TmpDir = sysEnv:tmp_dir(), "autobackup"),
    Path = filename:join(Dir = sysEnv:home_dir(), "autobackup.gz"),
    case swmLib:mnesia_backup(TmpPath) of
	ok ->
	    try move_file(TmpDir, TmpPath, Path)
	    catch Type:Error ->
		    sysInitI:error_report(
		      [{?MODULE, make_auto_backup},
		       {mfa, {?MODULE, move_file, [TmpDir, TmpPath, Path]}},
		       {Type, Error}]),
		    logI:write_log("SwmInternal", "swmDbMonitor", critical,
				   "Autobackup failed due to a disk problem"),
		    error
	    end;
	{error, Reason} ->
	    sysInitI:error_report([{?MODULE, make_auto_backup},
				   {mfa, {mnesia, backup, [Path]}},
				   {error, Reason}]),
	    logI:write_log("SwmInternal", "swmDbMonitor", critical,
			   "Autobackup failed due to a DBMS problem"),
	    error
    end.

move_file(TmpDir, TmpPath, Path) ->
    {0,_} = swmOs:cmdres(["cd ", TmpDir, " ; gzip ", TmpPath]),
    {0,_} = swmOs:cmdres(["mv -f ", TmpPath, ".gz ", Path, ".new"]),
    {0,_} = swmOs:cmdres(["mv -f ", Path, ".new ", Path, " ; sync"]),
    ok.

%%% Make a flat string out of an io call
    
format_msg(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).


%%% This function is to make efficient tracing possible
debug(_) ->
    %% _ = 1+1,
    ok.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.
%% info_msg(Format) ->
%%     info_msg(Format, []).
%% info_msg(Format, Args) ->
%%     sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%error_msg(Format) ->
%    error_msg(Format, []).
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     error_logger:warning_msg("~w: "++Format, [?MODULE|Args]).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


test_loop(Time) ->
    {ok, T} = timer:apply_interval(Time, ?MODULE, test_func, []),
    put(timer, T).

test_loop_cancel() ->
    T = get(timer),
    case T of 
	undefined ->
	    ok;
	_ ->
	    timer:cancel(T)
    end.


test_func() ->
    swmLib:set_variable(apa, erlang:monotonic_time()).
