%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmBackupScheduler.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R7A/R10A/R11A/4

%%% @doc ==Scheduled backup managmement==

%%% This module contains the main scheduler implementation of the
%%% scheduled backup function. It relies on individual scheduler
%%% processes for the individual instances of the different scheduler
%%% MOCs.
%%% 
%%% Main tasks 
%%% 1. Perform a schedulerd backup at given time
%%% 2. Subscribe to and possible route mnesia table envets for the MO
%%%    classes concerned
%%% 3. Start individual schedulers for every event MOC in the common 
%%%    library

-module(swmBackupScheduler).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R7A/R10A/R11A/4').
-date('2017-10-16').
-author('etxjotj').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2013-08-29 etxjotj     Created
%%% R2A/15     2014-02-28 erarafo     HS36541
%%% R2A/17     2014-03-03 etxjotj     HS37092 
%%% R2A/20     2014-03-07 etxjotj     HS37489, HS38353
%%% R2A/22     2014-03-09 etxjotj     Added a lot of logging
%%% R2A/23     2014-03-14 etxjotj     Added a note about months
%%% R2A/24     2014-03-17 etxjotj     Wait a second before rescheduling
%%%                                   to avoid the same time being used again
%%% R2A/25     2014-03-24 etxjotj     Connection to rollback list
%%% R2A/26     2014-04-12 etxjotj     Aligned with BrM 3.1 UCD on autoexport
%%% R2A/27     2014-04-13 etxjotj     Fixed alarm sending, HS42748
%%% R2A/28     2014-04-13 etxjotj     Added comment
%%% R2A/29     2014-04-14 etxjotj     Fix of HS42636: cancel old timer on update
%%% R2A/31     2014-06-12 etxjotj     Updated additional text after CPI review
%%% R2A/32     2014-07-25 etxjotj     Scheduling error in calendar based
%%% R2A/35     2014-08-20 etxjotj     HS88578: Handle clock misalignment
%%% R3A/1      2015-04-16 etxjotj     HS65980: Handle make backup failures
%%% R4A/1      2015-08-24 etxpejn     Changed logI:write_log to 
%%%                                   swmLib:write_swm_log
%%% R4A/3      2015-09-21 etxjotj     Handle action id
%%% R5A/1      2015-10-06 etxjotj     Removed clock_test
%%% R4A/4      2015-10-08 etxjotj     Removed clock test
%%% R5A/4      2016-03-21 ekurnik     WP5369 added actionCapable locking of
%%%                                   create_backup and export actions
%%% R7A/1      2016-10-07 etxjotj     HV30763 Reset scheduler progress
%% ----    ---------- -------  -------------------------------------------------
%% R10A/2  2017-05-04 etxberb  Added swmBackup:export_backup_sched/4.
%% ----    ---------- -------  -------------------------------------------------
%% R11A/2  2017-09-21 etxjojt  
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%% Called by supervisor
-export([start/1]).

%%% Called by swmBackup
-export([default_progress_report/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-export([schedule_single/1, schedule_periodic/1, schedule_calendar/1]).

-include("ComTop.hrl").
-include("RcsBrM.hrl").
%-include("ECIM_CommonLibrary.hrl").
-include("SwmInternal.hrl").
%-compile(export_all).
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

start(Args) ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).


%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% some_method(Parameter)->
%%    nn.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

-record(state, {scheduler, %% Id of the brmBackupSchedule MO
		timer,     %% Reference to the timer
		next       %% Current expiry time of the timer
		}).
init([Scheduler]) ->
    process_flag(trap_exit, true),
    set_oper_state(Scheduler, enabled),
    maybe_reset_progress(Scheduler), % HV30673
    gen_server:cast(self(), calculate_next),
    mnesia:subscribe({table, managedElement, detailed}),
    mnesia:subscribe({table, brmBackupScheduler, detailed}),
    mnesia:subscribe({table, brmSingleEvent, detailed}),
    mnesia:subscribe({table, brmPeriodicEvent, detailed}),
    mnesia:subscribe({table, brmCalendarBasedPeriodicEvent, detailed}), %HS37489
    {ok, #state{scheduler=Scheduler}}.

%% HV30673
maybe_reset_progress(Scheduler) ->
    case swmLib:get_variable(bu_restore) of
	undefined -> ok;
	_ ->
	    mnesia:transaction(fun() -> reset_progress(Scheduler) end)
    end.  

reset_progress(Scheduler) ->
    [Obj] = mnesia:read({brmBackupScheduler, Scheduler}),
    mnesia:write(Obj#brmBackupScheduler{progressReport = undefined}).
%% HV30673 fix ends here  


handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(calculate_next, State) ->
    scheduler_debug("Calculate next received~n"),
    Next = calculate_next(State#state.scheduler), 

    case Next of
	Next when State#state.next == Next ->
	    {noreply, State};
	undefined  ->
	    Msg = format_msg("No further backups are scheduled at this time"),
	    log(Msg),
	    timer:cancel(State#state.timer),
	    set_next(State#state.scheduler, undefined),
	    {noreply, State#state{timer=undefined, next=undefined}};
	Next ->
	    Msg = format_msg("Next scheduled backup is ~sZ",
			     [format_date(Next)]),
	    log(Msg),
	    set_next(State#state.scheduler, Next),
	    NextTimestamp = datetime_to_now(Next),
	    Wait = (now_to_integer(NextTimestamp)-
			now_to_integer(os:timestamp())) div 1000,
	    %% HS42636 fix
	    case State#state.timer of
		undefined -> ok;
		OldTimerRef -> timer:cancel(OldTimerRef)
	    end,
	    %% HS42636 fix ends here
	    {ok, TimerRef} = timer:send_after(Wait, backup_scheduler_timeout),
	    {noreply, State#state{timer=TimerRef, next=Next}}
    end;


handle_cast(_, State) ->
    {noreply, State}.

handle_info({mnesia_table_event, Event}, State) ->
    scheduler_debug("Table event: ~w ~n",[element(2, Event)]),
    handle_mnesia_table_event(Event, State),
    {noreply, State};

handle_info(backup_scheduler_timeout, State) ->
    scheduler_debug("Received backup_scheduler_timeout~n"),
    %% HS88578 Sanity check for early timeouts
    case calendar:universal_time() of
	UT when UT >= State#state.next ->
	    make_backup(State#state.scheduler),
	    timer:sleep(1000),
	    scheduler_debug("Reschedule after timeout~n"),
	    handle_cast(calculate_next, State#state{timer=undefined});
	UT -> % The timer fired to early (clock misalignment)
	    warning_msg("Clock misalignment.~n"
			"Timer fired: ~p~n"
			"Expected:    ~p~n",
			[UT, State#state.next]),
	    TimeRemaining = 
		calendar:datetime_to_gregorian_seconds(State#state.next) - 
		calendar:datetime_to_gregorian_seconds(UT),
	    Wait = TimeRemaining*1000,
	    {ok, TimerRef} = timer:send_after(Wait, backup_scheduler_timeout),
	    {noreply, State#state{timer=TimerRef}}
    end;
    
handle_info(restart, State) ->
    {stop, restart, State};
handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, State) ->
    set_oper_state(State#state.scheduler, disabled),
    ok.

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% internal_function1(One, Two)->
%%    nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


create_backup_common(Name, BrmBackupManagerKey, Type, Progress) ->
    case sysEnv:rcs_mode_2() of
	target ->
	    swmBackup:create_backup_common(
	      Name, BrmBackupManagerKey, Type, Progress);
	simulated ->
	    swmBackup:create_backup_common(
	      Name, BrmBackupManagerKey, Type, Progress);
	vrcs ->
	    swmvBackup:create_backup_common(
	      Name, BrmBackupManagerKey, Type, Progress)
    end.

%%% ----------------------------------------------------------
%%% SCHEDULING
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           calculate_next()
%%% Input: -
%%% Output: A date time tuple in universal time
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

calculate_next(Scheduler) ->
    scheduler_debug("Calculating next date~n"),
    {atomic, Next} = 
	mnesia:transaction(fun() -> do_calculate_next(Scheduler) end),
    scheduler_debug("Calculating next date ~p UT~n",[Next]),
    Next.


do_calculate_next(Scheduler) ->
    [Obj] = mnesia:read({brmBackupScheduler, Scheduler}),
    case Obj#brmBackupScheduler.adminState of
	?BasicAdmState_LOCKED ->
	    scheduler_debug("Scheduler state is LOCKED~n"),
	    undefined;
	?BasicAdmState_UNLOCKED ->
	    do_calculate_next(undefined,
			      [brmSingleEvent, 
			       brmPeriodicEvent, 
			       brmCalendarBasedPeriodicEvent])
    end.


do_calculate_next(Current, [Table|Tables]) ->
    scheduler_debug("Calculating next date for ~p~n",[Table]),
    Next = do_calculate_next(Current, Table, mnesia:first(Table)),
    do_calculate_next(Next, Tables);
do_calculate_next(Next, []) ->
    Next.

do_calculate_next(Current, Table, '$end_of_table') ->
    scheduler_debug("End of ~p~n",[Table]),
    Current;
do_calculate_next(Current, Table, Key) ->
    [Obj] = mnesia:read({Table, Key}),
    scheduler_debug("Calculating next date for ~p~n",[Obj]),
    Func = case Table of
	       brmSingleEvent -> schedule_single;
	       brmPeriodicEvent -> schedule_periodic;
	       brmCalendarBasedPeriodicEvent -> schedule_calendar
	   end,

    NowUT = calendar:universal_time(), % HS38353
    scheduler_debug("Time is now ~p UT~n",[NowUT]),
    NextCurrent = 
	case apply(?MODULE, Func, [Obj]) of
	    undefined ->
		%% This schedule is not a contributor
		Current;
	    Next when Current == undefined, Next > NowUT ->
		scheduler_debug("~p UT replacing undefined~n",[Next]),
		Next;
	    Next when Next < Current, Next > NowUT -> 
		scheduler_debug("~p UT replacing ~p~n",[Next, Current]),
		Next;
	    _ ->
		Current
	end,
    do_calculate_next(NextCurrent, Table, mnesia:next(Table, Key)).

%%% ----------------------------------------------------------
%%% #           set_next(Scheduler, Next)
%%% Input: Scheduler::tuple() - Key to the scheduler object
%%%        Next - A datetime tuple in universal time or 'undefined'
%%% Output: 
%%% Exceptions: 
%%% Description: Update the next schedule
%%% ----------------------------------------------------------

set_next(Scheduler, Next) ->
    Fun = fun() ->
		  [Obj] = mnesia:wread({brmBackupScheduler, Scheduler}),
		  mnesia:write(Obj#brmBackupScheduler{nextScheduledTime=Next})
	  end,
    {atomic, ok} = mnesia:transaction(Fun),
    ok.

%%% ----------------------------------------------------------
%%% #           schedule_single(Obj)
%%% Input: Obj:#brmBackupSingle{}
%%% Output: Next:datetime()|undefined - Next time in universal time
%%% Exceptions: 
%%% Description: Calculate the next time according to the schedule
%%% ----------------------------------------------------------

schedule_single(Obj) ->
    try do_schedule_single(Obj) of
	N -> 
	    clear_alarm(Obj),
	    N
    catch throw:undefined ->
	    clear_alarm(Obj),
	    undefined;
	  Type:Error ->
	    %% HS36541
	    send_alarm(Obj, "Failed to schedule event"),
	    sysInitI:error_report(
	      [{Type, Error}, 
	       {obj, Obj}, 
	       erlang:get_stacktrace()]),
	    undefined
    end.

do_schedule_single(Obj) ->
    DateString = Obj#brmSingleEvent.scheduledTime,
    NowCurrent = os:timestamp(),
    Current = calendar:now_to_universal_time(NowCurrent),
    scheduler_debug("Single event ~p~n",[DateString]),
    case swmLib:parse_date(DateString) of
	{local, LT} -> 
	    scheduler_debug("Translated to ~p~n",[LT]),
	    case calendar:local_time_to_universal_time_dst(LT) of
		[UT] when UT > Current -> 
		    scheduler_debug("Scheduling ~p UT~n",[UT]),
		    UT;
		[UT] ->
		    scheduler_debug("Already past ~p UT. Ignoring...~n",[UT]),
		    undefined;
		[UT1, _] when UT1 > Current -> 
		    scheduler_debug("DST first hour. Scheduling ~p UT~n",[UT1]),
		    UT1;
		[_, UT2] when UT2 > Current -> 
		    scheduler_debug("DST last hour. Scheduling ~p UT~n",[UT2]),
		    UT2;
		[] ->
		    scheduler_debug("DST forward shift. No applicable time~n"), 
		    undefined
	    end;
	{absolute, UT} -> 
	    scheduler_debug("Scheduling ~p UT~n",[UT]),
	    UT
    end.

%%% ----------------------------------------------------------
%%% #           schedule_periodic(Obj)
%%% Input: Obj:#brmPeriodicEvent{}
%%% Output: Next:datetime()|undefined - Next time in universal time
%%% Exceptions: 
%%% Description: Calculate the next time according to the schedule
%%% ----------------------------------------------------------

schedule_periodic(Obj) ->
    try do_schedule_periodic(Obj) of
	N -> 
	    clear_alarm(Obj),
	    N
    catch throw:undefined ->
	    clear_alarm(Obj),
	    undefined;
	  Type:Error ->
	    %% HS36541
	    send_alarm(Obj, "Failed to schedule event"),
	    sysInitI:error_report(
	      [{Type, Error}, 
	       {obj, Obj}, 
	       erlang:get_stacktrace()]),
	    undefined
    end.


do_schedule_periodic(Obj) ->
    Current = calendar:universal_time(),

    do_schedule_periodic(Obj, Current).

do_schedule_periodic(Obj, Current) ->
    %% Test if we're past the stop date
    case Obj#brmPeriodicEvent.stopTime of
	undefined -> 
	    scheduler_debug("stopTime is not set~n"),
	    ok;
	StopTime ->
	    case swmLib:parse_date(StopTime) of
		{local, LT} -> 
		    scheduler_debug("stop time is ~p LT~n",[LT]),
		    case calendar:local_time_to_universal_time_dst(LT) of
			[UT] when Current =< UT -> 
			    scheduler_debug("stopTime is not passed~n"),
			    ok;
			[_, UT2] when Current =< UT2 -> 
			    scheduler_debug("DST stopTime is not passed~n"),
			    ok;
			_ -> 
			    scheduler_debug("stopTime is not passed~n"),
			    throw(undefined)
		    end;
		{absolute, UT} when Current<UT -> 
		    scheduler_debug("stopTime is ~p UT continuing~n",[UT]),
		    ok;
		{absolute, UT} ->
		    scheduler_debug("stopTime is ~p UT discontinuing~n",[UT]),
		    throw(undefined)		    
	    end
    end,

    %% Test if we're past the start date

    %% HS36541
    StartTime =
	case Obj#brmPeriodicEvent.startTime of
	    undefined ->
		scheduler_debug("start time is not given, using current~n"),
		{local, calendar:local_time()};
	    _ ->
		swmLib:parse_date(Obj#brmPeriodicEvent.startTime)
	end,
    scheduler_debug("start time is ~p ~n ",[StartTime]),
    %% end HS36541
    case StartTime of
	{local, LTx} -> 
	    case calendar:local_time_to_universal_time_dst(LTx) of
		[UTx] when UTx =< Current ->
		    scheduler_debug("startTime passed~n"),
		    do_schedule_periodic_start(LTx, Obj, Current);
		[UTx] -> 
		    scheduler_debug("startTime is first time~n"),
		    UTx;
		[UT1x, UT2x] when UT1x =< Current, UT2x =< Current -> 
		    scheduler_debug("startTime passed~n"),
		    do_schedule_periodic_start(LTx, Obj, Current);
		[UT1x, UT2x] when UT2x =< Current -> 
		    scheduler_debug("DST startTime not passed first hour~n"),
		    UT1x;
		[_, UT2x] -> 
		    scheduler_debug("DST startTime not passed second hour~n"),
		    UT2x;
		_ -> 
		    scheduler_debug("DST invalid startTime~n"),
		    throw(undefined)
	    end;
	{absolute, UTx} when UTx > Current-> 
	    scheduler_debug("startTime is first time~n"),
	    UTx;
	{absolute, UTx} -> 
	    scheduler_debug("startTime is passed~n"),
	    LTx = calendar:universal_time_to_local_time(UTx),
	    do_schedule_periodic_start(LTx, Obj, Current)
    end.


do_schedule_periodic_start(StartTime, Obj, Current) ->
    scheduler_debug("Base time is ~p UT~n",[Current]),

    Period = 
	Obj#brmPeriodicEvent.days*86400+
	Obj#brmPeriodicEvent.hours*3600+
	Obj#brmPeriodicEvent.minutes*60+
	Obj#brmPeriodicEvent.weeks*604800, % months are intentionally ignored
    scheduler_debug("Period is ~p seconds~n",[Period]),

    StartSec = calendar:datetime_to_gregorian_seconds(StartTime),
    Offset = StartSec rem Period,
    scheduler_debug("Offset is ~p seconds~n",[Offset]),

    LocalTime = calendar:universal_time_to_local_time(Current),
    LocalSecs = calendar:datetime_to_gregorian_seconds(LocalTime),
    TimeSecs = 
	case LocalSecs rem Period of
	    LocalOffset when LocalOffset < Offset ->
		LocalSecs+Offset-LocalOffset;
	    LocalOffset ->
		LocalSecs-LocalOffset+Period+Offset
	end,
    Time = calendar:gregorian_seconds_to_datetime(TimeSecs),
    scheduler_debug("Propsed time is ~p seconds~n",[Time]),

    
    case calendar:local_time_to_universal_time_dst(Time) of
	[] when Period < 7200 -> 
	    %% This time doesn't exist. The making of a backup is omitted,
	    %% because in less than two hours we're going to make one anyway
	    scheduler_debug("Invalid time move to next period",[]),

	    UTSecs = calendar:datetime_to_gregorian_seconds(Current),
	    NewCurrent = calendar:gregorian_seconds_to_datetime(UTSecs+Period),
	    do_schedule_periodic_start(StartTime, Obj, NewCurrent);
	[] -> 
	    %% This time doesn't exists, but the period is two hours or 
	    %% longer. So we fill in a gap thus:
	    scheduler_debug("Invalid time, recalculating~n",[]),
	    [UStartTime] = calendar:local_time_to_universal_time_dst(StartTime),
	    UStartSec = calendar:datetime_to_gregorian_seconds(UStartTime),
	    UOffset = UStartSec rem Period,
	    UTSecs = calendar:datetime_to_gregorian_seconds(Current),
	    AltTimeSecs = 
		case UTSecs rem Period of
		    UTOffset when UTOffset < UOffset ->
			UTSecs+UOffset-UTOffset;
		    UTOffset ->
			UTSecs-UTOffset+Period+UOffset
		end,
	    calendar:gregorian_seconds_to_datetime(AltTimeSecs);
	[UT]  ->
	    %% Standard case
	    UT;
	[UT1, _] when UT1 > Current ->
	    %% DST backwards shift. This time occurs twice.
	    %% If the first time has not passed, use it as next time
	    scheduler_debug("DST first hour ~p UT~n",[UT1]),
	    UT1;
	[_, UT2] when UT2 > Current, Period < 7200 ->
	    %% DST backwards shift. This time occurs twice.
	    %% The first time has passed. If the second time has not passed
	    %% use it if the Period is less than two hours
	    scheduler_debug("DST second hour long period ~p UT~n",[UT2]),
	    UT2;
	_ ->
	    %% DST backwards shift. This time occurs twice and the period
	    %% is longer than two hours. Wait until next period.
	    scheduler_debug("DST second hour wait for next period~n",[]),
	    UTSecs = calendar:datetime_to_gregorian_seconds(Current),
	    NewCurrent = calendar:gregorian_seconds_to_datetime(UTSecs+Period),
	    do_schedule_periodic_start(StartTime, Obj, NewCurrent)
    end.
    
%%% ----------------------------------------------------------
%%% #           schedule_calendar(Obj)
%%% Input: Obj:#brmCalendarBasedPeriodicEvent{}
%%% Output: Next:datetime()|undefined - Next time in universal time
%%% Exceptions: 
%%% Description: Calculate the next time according to the schedule
%%% ----------------------------------------------------------

schedule_calendar(Obj) ->
    try do_schedule_calendar(Obj) of
	N ->
	    clear_alarm(Obj),
	    N
    catch throw:undefined ->
	    clear_alarm(Obj),
	    undefined;
	  throw:inconsistent_configuration ->
	    send_alarm(Obj, "No valid date found");
	  Type:Error ->
	    send_alarm(Obj, "Failed to schedule event"),
	    sysInitI:error_report(
	      [{Type, Error}, 
	       {obj, Obj}, 
	       erlang:get_stacktrace()]),
	    undefined
    end.


do_schedule_calendar(Obj) ->
    Current = calendar:universal_time(),
    do_schedule_calendar(Obj, Current).

do_schedule_calendar(Obj, Current) ->

    %% Test if we're past the stop date
    case Obj#brmCalendarBasedPeriodicEvent.stopTime of
	undefined -> 
	    scheduler_debug("stopTime is not set~n"),
	    ok;
	StopTime ->
	    case swmLib:parse_date(StopTime) of
		{local, LT} -> 
		    scheduler_debug("stop time is ~p LT~n",[LT]),
		    case calendar:local_time_to_universal_time_dst(LT) of
			[UT] when Current =< UT -> 
			    scheduler_debug("stopTime is not passed~n"),
			    ok;
			[_, UT2] when Current =< UT2 -> 
			    scheduler_debug("DST stopTime is not passed~n"),
			    ok;
			_ -> 
			    scheduler_debug("stopTime is passed~n"),
			    throw(undefined)
		    end;
		{absolute, UT} when Current<UT -> 
		    scheduler_debug("stopTime is ~p UT continuing~n",[UT]),
		    ok;
		{absolute, UT} ->
		    scheduler_debug("stopTime is ~p UT discontinuing~n",[UT]),
		    throw(undefined)		    
	    end
    end,

    %% Find out where to start iteration
    case Obj#brmCalendarBasedPeriodicEvent.startTime of
	undefined ->
	    First = calendar:local_time(),
	    scheduler_debug(
	      "startTime is not given, using current as first ~p LT~n",[First]),
	    do_schedule_calendar_start({local, First}, Obj, Current);
%%% Alternative implementation
	StartDate ->
	    scheduler_debug("startTime is ~s~n",[StartDate]),
	    do_schedule_calendar_start(swmLib:parse_date(StartDate),
				       Obj, Current)
    end.
%%% Original implementationation    
    %% 	StartDate ->
    %% 	    case swmLib:parse_date(StartDate) of
    %% 		{local, LTx} ->
    %% 		    First = 
    %% 		    	case calendar:local_time_to_universal_time_dst(LTx) of
    %% 		    	    [UTx] when UTx > Current -> 
    %% 		    		%% We've not passed the start date
    %% 		    		scheduler_debug("startTime passed~n"),
    %% 		    		LTx;
    %% 		    	    [UT1x, _] when UT1x > Current -> 
    %% 		    		%% We've not passed the start date
    %% 		    		scheduler_debug("startTime is not passed~n"),
    %% 		    		LTx;
    %% 		    	    [] ->
    %% 		    		%% Well what do you know
    %% 		    		case first_legal_date(LTx) of
    %% 		    		    LegalUT when LegalUT > Current ->
    %% 		    			%% We've not passed the start date
    %% 		    			scheduler_debug(
    %% 		    			  "DST startTime not passed~n"),
    %% 		    			LTx;
    %% 		    		    LegalUT ->
    %% 		    			%% We've passed the start date
    %% 		    			 scheduler_debug(
    %% 		    			   "DST startTime passed~n"),
    %% 		    			calendar:universal_time_to_local_time(
    %% 		    			  LegalUT)
    %% 		    		end;
    %% 		    	    _ ->
    %% 		    		%% We've passed the start date, use
    %% 		    		%% current time
    %% 		    		calendar:universal_time_to_local_time(Current)
    %% 		    	end,
    %% 		    do_schedule_calendar_start(First, Obj, Current);
    %% 		{absolute, UTx} when UTx > Current-> 
    %% 		    %% We've not passed the start date, use start date
    %% 		    scheduler_debug("startTime is not passed~n"),
    %% 		    First = calendar:universal_time_to_local_time(UTx),
    %% 		    do_schedule_calendar_start(First, Obj, Current);
    %% 		_ ->
    %% 		    scheduler_debug("startTime is passed~n"),
    %% 		    First = calendar:universal_time_to_local_time(Current),
    %% 		    do_schedule_calendar_start(First, Obj, Current)
    %% 	    end
    %% end.

%% first_legal_date(LT) ->
%%     scheduler_debug("DST invalid time ~p, finding next valid time~n"),
%%     GS = calendar:datetime_to_gregorian_seconds(LT),
%%     first_legal_date(GS, 0).

%% first_legal_date(_, N) when N >= 86401 ->
%%     %% No valid time for a whole day? Give up!
%%     scheduler_debug("DST invalid no replacement time for a day~n"),
%%     throw(inconsistent_configuration);
%% first_legal_date(GS, N)->
%%     NextS = GS+N,
%%     NextDT = calendar:gregorian_seconds_to_datetime(NextS),

%%     case calendar:local_time_to_universal_time_dst(NextDT) of
%% 	[UT] ->
%% 	    UT;
%% 	[UT1, _] ->
%% 	    UT1;
%% 	[] ->
%% 	    first_legal_date(GS, N+1)
%%     end.

%% do_schedule_calendar_start(StartTime, Obj, Current) ->
%%     {ok, [H,M,S], _} = io_lib:fread("~d:~d:~d",
%% 				    Obj#brmCalendarBasedPeriodicEvent.time),
%%     Time = {H,M,S},
%%     do_schedule_calendar_start(StartTime, Time, Obj, Current, 367).

%% do_schedule_calendar_start(_, _, _, _, 0) ->
%%     %% No match for more than a year -> give up
%%     throw(inconsistent_configuration);
%% do_schedule_calendar_start(StartTime, Time, Obj, Current, N) ->
%%     CurrentL = calendar:universal_time_to_local_time(Current),
%%     #brmCalendarBasedPeriodicEvent{
%%        dayOfMonth = DayOfMonth,
%%        dayOfWeek = DayOfWeek,
%%        dayOfWeekOccurrence = DayOfWeekOccurrence,
%%        month = Month} = Obj,
%%     NextCandidateL = find_next_candidate(StartTime, Time, CurrentL),
%%     scheduler_debug("Testing candidate ~p LT~n",[NextCandidateL]),
%%     try begin
%% 	    test_dst(NextCandidateL),
%% 	    test_month(NextCandidateL, Month),
%% 	    test_day_of_month(NextCandidateL, DayOfMonth),
%% 	    test_day_of_week(NextCandidateL, DayOfWeek),
%% 	    test_day_of_week_occurrence(NextCandidateL, DayOfWeekOccurrence)
%% 	end
%%     of
%% 	ok ->
%% 	    case calendar:local_time_to_universal_time_dst(NextCandidateL) of
%% 		[UT] -> UT;
%% 		[UT1, _] when UT1>Current -> UT1;
%% 		[_, _] -> 
%% 		    {DstNextDate, DstNextTime} = NextCandidateL,
%% 		    DstNext = {next_day(DstNextDate), DstNextTime},
%% 		    do_schedule_calendar_start(DstNext, Time, Obj, Current, N)
%% 	    end
%%     catch throw:undefined ->
%% 	    do_schedule_calendar_start(NextCandidateL, Time, Obj, Current, N-1);
%% 	  throw:dst ->
%% 	    {DstNextDate, DstNextTime} = NextCandidateL,
%% 	    DstNext = {next_day(DstNextDate), DstNextTime},
%% 	    do_schedule_calendar_start(DstNext, Time, Obj, Current, N)
%%     end.

%% find_next_candidate(StartTime, Time, CurrentL) ->
%%     case StartTime of
%% 	{StartDate, _} when StartTime > CurrentL -> 
%%  	    scheduler_debug("startTime ~p LT has not passed~n",[StartTime]),
%% 	    %% We have't passed StartTime yet
%% 	    case {StartDate, Time} of
%% 		Candidate when Candidate > CurrentL ->
%% 		    scheduler_debug("candidate ~p LT has not passed~n",
%% 				    [Candidate]),
%% 		    Candidate;
%% 		Candidate ->
%% 		    %% Next time occurs next day
%% 		    scheduler_debug("Candidate ~p LT has passed. "
%% 				    "Trying next day~n",[Candidate]),
%% 		    NextDate = next_day(StartDate),
%% 		    {NextDate, Time}
%% 	    end;
%% 	_ ->
%% 	    %% We have passed StartTime
%%  	    scheduler_debug("startTime ~p LT has not passed~n",[StartTime]),
%% 	    case CurrentL of
%% 		{CurrentDate, CurrentTime} when CurrentTime > Time ->
%% 		    %% Next time occurs next day
%% 		    scheduler_debug("Candidate ~p LT has passed. "
%% 				    "Trying next day~n",[{CurrentDate, CurrentTime}]),
%% 		    NextDate = next_day(CurrentDate),
%% 		    {NextDate, Time};
%% 		{CurrentDate, _} ->
%% 		    scheduler_debug("Candidate ~p LT has not passed.~n",
%% 				    [CurrentDate]),
%% 		    {CurrentDate, Time}
%% 	    end
%%     end.

do_schedule_calendar_start(StartLT, Obj, CurrentUT) ->
    CurrentLT = calendar:universal_time_to_local_time(CurrentUT),
    {ok, [H,M,S], _} = io_lib:fread("~d:~d:~d",
				    Obj#brmCalendarBasedPeriodicEvent.time),
    Time = {H,M,S},
    FirstCandidate = 
	case StartLT of
	    {local, LTx} ->
		calculate_first_candidate(LTx, Time, CurrentLT);
	    {absolute, UTx} -> 
		LTx = calendar:universal_time_to_local_time(UTx),
		calculate_first_candidate(LTx, Time, CurrentLT)		
	end,
    do_schedule_calendar_start_loop(FirstCandidate, Obj, 366*7*4).

calculate_first_candidate(LTx, Time, CurrentLT) ->
    CurrentDate = element(1, CurrentLT),
    StartDate = element(1, LTx),

    RealStartTime = 
	if {StartDate, Time} < LTx -> 
		scheduler_debug(
		  "FC: Real time ~p is earlier than start time~n",
		  [{StartDate,Time}, LTx]),
		{next_day(StartDate), Time};
	   true ->
		scheduler_debug("FC: Real start time is ~p~n",
				[{StartDate,Time}]),
		{StartDate, Time}
	end,
    
    if RealStartTime > CurrentLT ->
	    scheduler_debug("FC: ~p has not passed~n",[RealStartTime]),
	    RealStartTime;
       true ->
	    if {CurrentDate, Time} < CurrentLT ->
		    scheduler_debug(
		      "FC: Today's desired time has passed, using next day~n"),
		    {next_day(CurrentDate), Time};
	       true ->
		    scheduler_debug(
		      "FC: Todays desired time will be used for first test~n"),
		    {CurrentDate, Time}
	    end
    end.
    
			

next_day(Date) ->
    Secs = calendar:datetime_to_gregorian_seconds({Date, {0,0,0}}),
    {NextDate, _} = calendar:gregorian_seconds_to_datetime(Secs+86400),
    NextDate.

do_schedule_calendar_start_loop(_, _, 0) ->
    throw(inconsistent_configuration);

do_schedule_calendar_start_loop(CandidateL, Obj, N) ->
    scheduler_debug("Testing candidate ~p LT~n",[CandidateL]),
    #brmCalendarBasedPeriodicEvent{
       dayOfMonth = DayOfMonth,
       dayOfWeek = DayOfWeek,
       dayOfWeekOccurrence = DayOfWeekOccurrence,
       month = Month} = Obj,
    try begin
	    test_month(CandidateL, Month),
	    test_day_of_month(CandidateL, DayOfMonth),
	    test_day_of_week(CandidateL, DayOfWeek),
	    test_day_of_week_occurrence(CandidateL, DayOfWeekOccurrence),
	    test_dst(CandidateL)
	end
    catch throw:undefined ->
	    {NextDate, NextTime} = CandidateL,
	    Next = {next_day(NextDate), NextTime},
	    do_schedule_calendar_start_loop(Next, Obj,  N-1);
	  throw:dst ->
	    {DstNextDate, DstNextTime} = CandidateL,
	    DstNext = {next_day(DstNextDate), DstNextTime},
	    do_schedule_calendar_start_loop(DstNext,  Obj, N)
    end.


test_dst(CandidateL) ->
    Current = calendar:universal_time(),
    case calendar:local_time_to_universal_time_dst(CandidateL) of
	[UT] ->
	    UT;
	[UT1, _] when UT1>Current -> ok;
	[_, _] -> % Not using second hour
	    throw(dst);
	[] -> % This time doesn't exist
	    throw(dst)
    end.

test_month(_, 0) ->
    ok;
test_month({{_, Month, _},_}, Month) ->
    ok;
test_month(_, _) ->
    throw(undefined).

test_day_of_month(_, 0) ->
    ok;
test_day_of_month({{_, _, Day}, _}, Day) ->
    ok;
test_day_of_month(_, _) ->
    throw(undefined).

test_day_of_week(_, ?DayOfWeek_ALL) ->
    ok;
test_day_of_week({Date, _}, DayOfWeek) ->
    %% It so happens that the ECIM Common library ENUM DayOfWeek
    %% uses the same numerical values for the days as the calendar
    %% module, but I choose to regard that as a coincidence, hence
    %% this little case clause instead of a simple match 
    case calendar:day_of_the_week(Date) of
	1 when DayOfWeek == ?DayOfWeek_MONDAY -> ok;
	2 when DayOfWeek == ?DayOfWeek_TUESDAY -> ok;
	3 when DayOfWeek == ?DayOfWeek_WEDNESDAY -> ok;
	4 when DayOfWeek == ?DayOfWeek_THURSDAY -> ok;
	5 when DayOfWeek == ?DayOfWeek_FRIDAY -> ok;
	6 when DayOfWeek == ?DayOfWeek_SATURDAY -> ok;
	7 when DayOfWeek == ?DayOfWeek_SUNDAY -> ok;
	_ -> throw(undefined)
    end.

test_day_of_week_occurrence(_, ?DayOfWeekOccurrence_ALL) ->
    ok;
test_day_of_week_occurrence({{_, _, D},_}, Occurrence) 
  when Occurrence == ?DayOfWeekOccurrence_FIRST,
       D =< 7 ->
    ok;
test_day_of_week_occurrence({{_, _, D},_}, Occurrence) 
  when Occurrence == ?DayOfWeekOccurrence_SECOND,
       D =< 14,
       D > 7->
    ok;
test_day_of_week_occurrence({{_, _, D},_}, Occurrence) 
  when Occurrence == ?DayOfWeekOccurrence_THIRD,
       D =< 21,
       D > 14 ->
    ok;
test_day_of_week_occurrence({{_, _, D},_}, Occurrence) 
  when Occurrence == ?DayOfWeekOccurrence_FOURTH,
       D =< 28,
       D > 21 ->
    ok;
test_day_of_week_occurrence({{Y,M,D},_}, Occurrence) 
  when Occurrence == ?DayOfWeekOccurrence_LAST ->
    LastDay = calendar:last_day_of_the_month(Y,M),
    case LastDay-D of
	Diff when Diff < 7 ->
	    ok;
	_ ->
	    throw(undefined)
    end;
test_day_of_week_occurrence(_, _) ->
    throw(undefined).

%%% ----------------------------------------------------------
%%% MAKING A SCHEDULED BACKUP
%%% ----------------------------------------------------------

make_backup(Scheduler) ->
    ActionId = 
	case swmLib:get_variable(sched_bu_id) of
	    undefined ->
		swmLib:set_variable(sched_bu_id, 1),
		1;
	    Id ->
		NextId = (Id+1) rem 65536,
		swmLib:set_variable(sched_bu_id, NextId),
		NextId
	end,
    set_default_progress_report(Scheduler, ActionId),
    ScheduledBackupFailedFun = 
        fun(Msg, CurrentActionInfo) ->
            send_alarm(Scheduler, Msg),
            CompleteTime = comsaI:iso_time(os:timestamp(), extended),
            swmBackupModel:update_progress(
              schedule, [{additionalInfo, CurrentActionInfo},
                 {result, ?ActionResultType_NOT_AVAILABLE},
                 {resultInfo, 
                  "The making of a scheduled backup was omitted"},
                 {progressPercentage, 0},
                 {state, ?ActionStateType_FINISHED},
                 {timeActionCompleted, CompleteTime}])
        end,
    case {swmFailsafe:get_usage_state(), 
          swmLib:lock_action_capable(?BACKUP_SCHEDULER_ACTION_CAPABLE_ID, 
                                    ?MAKE_SCHED_BACKUP_ACTION_CAPABLE_INFO)} of
    {busy, LockActionCapableResult} ->
        Msg = "Backup creation suppressed while the failsafe backup is activated",
        ScheduledBackupFailedFun(Msg, Msg),
        case LockActionCapableResult of
        ok ->
            ok = swmLib:unlock_action_capable(?BACKUP_SCHEDULER_ACTION_CAPABLE_ID);
        _ ->
            ok
        end;
	{idle, {nok, _}} ->
        CurrentActionInfo = swmLib:get_action_capable_info(),
	    Msg = CurrentActionInfo ++ " Backup creation suppressed.",
	    ScheduledBackupFailedFun(Msg, CurrentActionInfo);
	{idle, ok} ->
	    %% HT65980
	    %% Send alarm on failures
	    case 
		swmBackupModel:action_handler(
		  fun() -> do_make_backup(Scheduler) end, schedule) of
		ok -> 
		    clear_alarm(Scheduler);
		{failed, Reason} ->
		    send_alarm(Scheduler, Reason)
	    end,
        ok = swmLib:unlock_action_capable(?BACKUP_SCHEDULER_ACTION_CAPABLE_ID)
    end.

set_default_progress_report(Scheduler, Id) ->
    mnesia:transaction(
      fun() ->
              [Obj] = mnesia:wread({brmBackupScheduler, Scheduler}),
              Progress = default_progress_report(Id),
              mnesia:write(Obj#brmBackupScheduler{progressReport=Progress})
      end).

default_progress_report(Id) ->
    #'AsyncActionProgress'
	{actionName="SCHEDULED_BACKUP",
	 additionalInfo=["Scheduled backup started"],
	 progressInfo = "Scheduled backup started",
	 progressPercentage=0,
	 result=?ActionResultType_NOT_AVAILABLE,
	 resultInfo="",
	 state=?ActionStateType_RUNNING,
	 actionId=Id,
	 timeActionStarted = 
	     comsaI:iso_time(os:timestamp(), extended),
	 timeActionCompleted= "",
	 timeOfLastStatusUpdate= 
	     comsaI:iso_time(os:timestamp(), extended)}.

do_make_backup(Scheduler) ->
    {atomic, [Obj]} = mnesia:transaction(
			fun() -> mnesia:read({brmBackupScheduler,Scheduler}) end
			),
    Name = Obj#brmBackupScheduler.scheduledBackupName++"-"++
	comsaI:iso_time(os:timestamp(), extended),
    log(e, "Making backup " ++Name),
    case swmBackupModel:validate_name(list_to_binary(Name)) of
	ok ->
	    ok;
	duplicate_name ->
	    Dmsg = "Duplicate name: "++Name,
	    log(e, Dmsg),
	    swmBackupModel:update_progress(
	      schedule, [{resultInfo, Dmsg}])
    end,

    housekeeping(Obj#brmBackupScheduler.maxStoredScheduledBackups),

    {ME, SF, BrM, BMgr, _} = Scheduler,
    MgrKey = {ME, SF, BrM, BMgr},
    DN = create_backup_common(
	   list_to_binary(Name), MgrKey, scheduled, schedule),
    swmFallbackList:add_backup(latest, Name),
    Fun = 
	fun() ->
		[O2] = mnesia:wread({brmBackupScheduler,Scheduler}),
		New = O2#brmBackupScheduler{mostRecentlyCreatedAutoBackup=Name},
		ok = mnesia:write(New),
		#brmBackupScheduler{autoExport=AeState, 
				    autoExportPassword=Password,
				    autoExportUri=ExportUri} = O2,
		{AeState, Password, ExportUri}
	end,
    {atomic, {AutoExport, Passwd, Uri}} = mnesia:transaction(Fun),
    clear_alarm(Scheduler),
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    swmBackupModel:update_progress(
      schedule, [{additionalInfo, "Scheduled backup complete"},
		 {result, ?ActionResultType_SUCCESS},
		 {resultInfo, DN},
		 {progressPercentage, 100},
		 {state, ?ActionStateType_FINISHED},
		 {timeActionCompleted, CompleteTime}]),
    %% This is for testing purposes only
    %% Wait before resetting progress report to allow the test server
    %% to read the progress report before it is resetted
    case swmLib:get_variable(backup_test_sched_bu_wait) of
	true ->
	    receive
		go_ahead ->
		    ok
	    after 60000 ->
		    who_cares
	    end;
	_ ->
	    ok
    end,

    case AutoExport of
	?BrmAutoExport_ENABLED ->
        %% Changing actionCapableInfo from create to export
        ok = swmLib:update_action_capable_info(
               ?BACKUP_SCHEDULER_ACTION_CAPABLE_ID, 
               ?EXPORT_SCHED_BACKUP_ACTION_CAPABLE_INFO),
        
	    swmBackupModel:update_progress(
	      schedule, [{actionName, "SCHEDULED_EXPORT"},
			 {additionalInfoClear, "Export started"},
			 {progressPercentage, 0},
			 {result, ?ActionResultType_NOT_AVAILABLE},
			 {state, ?ActionStateType_RUNNING},
			 {timeActionStarted, CompleteTime},
			 {timeActionCompleted, ""}]),
	    Index = lists:last(string:tokens(DN,"=,")),
	    BuKey = {ME, SF, BrM, BMgr, Index},
	    Cleartext = comsaI:decrypt_password(Passwd),
	    %% One hour export time is likely never to occur, but better 
	    %% safe than sorry. 
	    ActionId = swmLib:get_new_action_id(BuKey),
	    export_backup_sched(BuKey, Uri, Cleartext, ActionId),
	    CompleteTime2 = comsaI:iso_time(os:timestamp(), extended),
	    RFun = fun() -> mnesia:read({brmBackup, BuKey}) end,
	    case mnesia:transaction(RFun) of
		{atomic, [BrmBackup]} -> 
		    Progress = BrmBackup#brmBackup.progressReport,
		    case Progress#'AsyncActionProgress'.result of
			?ActionResultType_FAILURE ->
			    Msg1 = Progress#'AsyncActionProgress'.resultInfo,
			    Msg2 = " For more info, check the progress report "
				"of BrmBackup="++Index,
			    Msg3 = " actionId="++integer_to_list(ActionId),
			    send_file_alarm(Scheduler, Msg1++Msg2++Msg3);
			?ActionResultType_SUCCESS ->
			    clear_file_alarm(Scheduler);
			?ActionResultType_NOT_AVAILABLE ->
			    ok
		    end,
		    swmBackupModel:update_progress(
		      schedule, 
		      [{additionalInfo, "Auto export complete"},
		       {result, Progress#'AsyncActionProgress'.result},
		       {resultInfo, Progress#'AsyncActionProgress'.resultInfo},
		       {progressPercentage, 100},
		       {state, Progress#'AsyncActionProgress'.state},
		       {timeActionCompleted, CompleteTime2}]);		    
		Other ->
		    swmBackupModel:update_progress(
		      schedule, 
		      [{additionalInfo, "A database error occured"},
		       {result, ?ActionResultType_NOT_AVAILABLE},
		       {progressPercentage, 0},
		       {state, ?ActionStateType_FINISHED},
		       {timeActionCompleted, CompleteTime2}]),		   
		    sysInitI:error_report(
		      [{mfa, {mnesia, read, [{brmBackup, BuKey}]}},
		       Other]),
		    send_file_alarm(Scheduler, "Database error")
	    end;
	AutoExport when AutoExport==?BrmAutoExport_DISABLED;
			AutoExport==undefined ->
	    clear_file_alarm(Scheduler)
    end,
    ok.

export_backup_sched(BuKey, Uri, Passwd, _ActionId) ->
    %% swmBackupModel:set_default_progress_report(
    %%   BuKey, "SCHEDULED_AUTOEXPORT", ActionId),
    swmBackupModel:action_handler(
      fun() ->
	      swmBackupFile:handle_export_backup(BuKey, BuKey, Passwd, Uri)
      end,
      BuKey),
    ok = swmLib:unlock_action_capable(?EXPORT_BACKUP_ACTION_CAPABLE_ID).


housekeeping(MaxNo) ->
    {atomic, Result} = mnesia:transaction(fun() -> do_housekeeping(MaxNo) end),
    case Result of
	{cleaned, Indices} ->
	    [begin 
		 BuDir = swmLib:backup_dir(Index),
		 cmd("rm -rf "++BuDir),
		 Msg = "Backup "++BackupName++" removed automatically "
		     "due to housekeeping.",
		 swmBackupModel:update_progress(schedule, [{additionalInfo, Msg}])
		     
	     end ||{Index, BackupName}<-Indices],
	    {cleaned, Indices};
	Result ->
	    Result
    end.

do_housekeeping(MaxNo) ->
    Type = ?BrmBackupCreationType_SCHEDULED,
    WP = mnesia:table_info(brmBackup, wild_pattern),
    Pattern = WP#brmBackup{creationType = Type},
    Backups = mnesia:match_object(Pattern),
    info_msg("Now holding ~w scheduled backups~n",[length(Backups)]),
    case length(Backups) of
	N when N >= MaxNo ->
	    Remove = N-MaxNo+1, % HS42748
	    info_msg("Removing ~w backups~n",[Remove]),
	    Sorted = lists:keysort(#brmBackup.creationTime, Backups),
	    Indices = housekeeping_autodelete(Remove, Sorted),
	    {cleaned, Indices};
	_ ->
	    ok
    end.

housekeeping_autodelete(0, _) ->
    [];
housekeeping_autodelete(_, []) ->
    [];
housekeeping_autodelete(N, [Backup|Sorted]) ->
    Key = Backup#brmBackup.brmBackupId,
    BackupName = Backup#brmBackup.backupName,
    ok = mnesia:delete({brmBackup, Key}),
    [{lists:last(tuple_to_list(Key)), BackupName}|
     housekeeping_autodelete(N-1, Sorted)].

%%% ----------------------------------------------------------
%%% HANDLING OF MODEL EVENTS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           handle_mnesia_table_event(Event, State)
%%% Input: Event:tuple() - A detailed mnesia table event
%%%        State:#state{} - The state record
%%% Output: 
%%% Exceptions: 
%%% Description: Handle events or route them to the appropriate scheduler
%%% ----------------------------------------------------------

handle_mnesia_table_event({write, managedElement, New, [Old], _}, _) ->
    case {New#managedElement.timeZone, Old#managedElement.timeZone} of
        {Same, Same} ->
            ok;
	_ ->
	    %% Timezone has changed, reevaluate all schedules
	    log("Recalculating schedule due to time zone change"),
	    gen_server:cast(self(), calculate_next)
    end;
handle_mnesia_table_event({write, brmBackupScheduler, New, [Old], _}, State) ->
    case New#brmBackupScheduler.brmBackupSchedulerId of
	Id when State#state.scheduler == Id ->
	    case {New#brmBackupScheduler.adminState,
		  Old#brmBackupScheduler.adminState} of
		{Same, Same} ->
		    ok;
		{?BasicAdmState_LOCKED, _} ->
		    log(e, "Scheduler State is LOCKED"),
		    clear_alarm(Id), % HT65980
		    clear_file_alarm(Id),
		    gen_server:cast(self(), calculate_next);
		{?BasicAdmState_UNLOCKED, _} ->
		    log(e, "Scheduler State is UNLOCKED"),
		    gen_server:cast(self(), calculate_next)
	    end,
	    case {New#brmBackupScheduler.autoExport,
		  Old#brmBackupScheduler.autoExport} of
		{Same2,Same2} ->
		    ok;
		{?BrmAutoExport_DISABLED, _} ->
		    log(e, "Scheduled backup autoexport DISABLED"),
		    clear_file_alarm(Id);
		{?BrmAutoExport_ENABLED, _} ->
		    log(e, "Scheduled backup autoexport ENABLED"),
		    ok
	    end;
	_ ->
	    ok
    end;

handle_mnesia_table_event({delete, brmSingleEvent, _, [Obj], _}, _) ->
    log(e, format_msg("Deleted ~s", [id(Obj)])),
    clear_alarm(Obj),
    gen_server:cast(self(), calculate_next);
handle_mnesia_table_event({delete, brmPeriodicEvent, _, [Obj], _}, _) ->
    log(e, format_msg("Deleted ~s", [id(Obj)])),
    clear_alarm(Obj),
    gen_server:cast(self(), calculate_next);
handle_mnesia_table_event(
  {delete, brmCalendarBasedPeriodicEvent, _, [Obj], _}, _) ->
    log(e, format_msg("Deleted ~s", [id(Obj)])),
    clear_alarm(Obj),
    gen_server:cast(self(), calculate_next);

handle_mnesia_table_event({_, brmSingleEvent, Obj, _, _}, _) ->
    log(e, format_msg("Modified ~s", [id(Obj)])),
    gen_server:cast(self(), calculate_next);
handle_mnesia_table_event({_, brmPeriodicEvent, Obj, _, _}, _) ->
    log(e, format_msg("Modified ~s", [id(Obj)])),
    case Obj#brmPeriodicEvent.startTime of
	undefined ->
	    StartTime = comsaI:iso_time(os:timestamp(), extended),
	    mnesia:dirty_write(Obj#brmPeriodicEvent{startTime=StartTime});
	_ ->
	    ok
    end,
    gen_server:cast(self(), calculate_next);
handle_mnesia_table_event({_, brmCalendarBasedPeriodicEvent, Obj, _, _}, _) ->
    log(e, format_msg("Modified ~s", [id(Obj)])),
    case Obj#brmCalendarBasedPeriodicEvent.startTime of
	undefined ->
	    StartTime = comsaI:iso_time(os:timestamp(), extended),
	    mnesia:dirty_write(Obj#brmCalendarBasedPeriodicEvent{startTime=StartTime});
	_ ->
	    ok
    end,
    gen_server:cast(self(), calculate_next);
    
handle_mnesia_table_event(Event, _) ->
    info_msg("~p~n",[Event]),
    ok.
    

id(Obj) ->
    Key = element(2, Obj),
    Index = element(6, Key),
    Class = element(1, Obj),
    atom_to_list(Class)++" "++Index.

%%% ----------------------------------------------------------
%%% ALARM HANDLING
%%% ----------------------------------------------------------

send_alarm(Scheduler, Msg) ->
    Dn = make_dn(Scheduler),
    DnString = make_dn_string(Dn),
    case is_alarm('ScheduledBackupFailed', DnString) of
	true ->
	    ok;
	false ->
	    comsaI:send_alarm('ScheduledBackupFailed', warning, Dn, Msg)
    end.

clear_alarm(Scheduler) ->
    Dn = make_dn(Scheduler),
    DnString = make_dn_string(Dn),
    case is_alarm('ScheduledBackupFailed', DnString) of
	true ->
	    comsaI:clear_alarm('ScheduledBackupFailed', Dn);
	false ->
	    ok
    end.

send_file_alarm(Scheduler, Msg) ->
    Dn = make_dn(Scheduler),
    DnString = make_dn_string(Dn),
    case is_alarm('AutoExportBackupFailed', DnString) of
	true ->
	    ok;
	false ->
	    comsaI:send_alarm('AutoExportBackupFailed', warning, Dn, Msg)
    end.

clear_file_alarm(Scheduler) ->
    Dn = make_dn(Scheduler),
    DnString = make_dn_string(Dn),
    case is_alarm('AutoExportBackupFailed', DnString) of
	true -> 
	    comsaI:clear_alarm('AutoExportBackupFailed', Dn);
	false ->
	    ok
    end.

is_alarm(AlarmType, DnString) ->
    Alarms = comsaI:get_alarms(AlarmType),
    do_is_alarm(DnString, Alarms).

do_is_alarm(DnString, [Alarm|Alarms]) ->
    case proplists:get_value(source, Alarm) of
	DnString ->
	    true;
	_ ->
	    do_is_alarm(DnString, Alarms)
    end;
do_is_alarm(_, []) ->
    false.

make_dn_string(Dn) ->
    binary_to_list(list_to_binary([hd(Dn)|[[<<",">>,X]||X<-tl(Dn)]])).

make_dn(Obj) when is_record(Obj, brmSingleEvent) ->
    {ME, SF, BrM, BMgr, BSch, N} = Obj#brmSingleEvent.brmSingleEventId,
    [list_to_binary("ManagedElement="++ME),
     list_to_binary("SystemFunctions="++SF),
     list_to_binary("BrM="++BrM), 
     list_to_binary("BrmBackupManager="++BMgr),
     list_to_binary("BrmBackupScheduler="++BSch),
     list_to_binary("BrmSingleEvent="++N)];
make_dn(Obj) when is_record(Obj, brmPeriodicEvent) ->
    {ME, SF, BrM, BMgr, BSch, N} = Obj#brmPeriodicEvent.brmPeriodicEventId,
    [list_to_binary("ManagedElement="++ME),
     list_to_binary("SystemFunctions="++SF),
     list_to_binary("BrM="++BrM), 
     list_to_binary("BrmBackupManager="++BMgr),
     list_to_binary("BrmBackupScheduler="++BSch),
     list_to_binary("BrmPeriodicEvent="++N)];
make_dn(Obj) when is_record(Obj, brmCalendarBasedPeriodicEvent) ->
    {ME, SF, BrM, BMgr, BSch, N} = 
	Obj#brmCalendarBasedPeriodicEvent.brmCalendarBasedPeriodicEventId,
    [list_to_binary("ManagedElement="++ME),
     list_to_binary("SystemFunctions="++SF),
     list_to_binary("BrM="++BrM), 
     list_to_binary("BrmBackupManager="++BMgr),
     list_to_binary("BrmBackupScheduler="++BSch),
     list_to_binary("BrmCalendarBasedEvent="++N)];
    
make_dn(Scheduler) ->
    {ME, SF, BrM, BMgr, BSch} = Scheduler,
    [list_to_binary("ManagedElement="++ME),
     list_to_binary("SystemFunctions="++SF),
     list_to_binary("BrM="++BrM), 
     list_to_binary("BrmBackupManager="++BMgr),
     list_to_binary("BrmBackupScheduler="++BSch)].


%%% ----------------------------------------------------------
%%% LIBRARY
%%% ----------------------------------------------------------

%% admin_state(Scheduler) ->
%%     [Obj] = mnesia:dirty_read({brmBackupScheduler, Scheduler}),
%%     case Obj#brmBackupScheduler.adminState of
%% 	?BasicAdmState_LOCKED ->
%% 	    locked;
%% 	?BasicAdmState_UNLOCKED ->
%% 	    unlocked
%%     end.

%% Prepare to deprecate this attribute

%% set_oper_state(Scheduler, State) ->
set_oper_state(_, _) ->
    %% S = case State of
    %% 	    enabled -> ?OperState_ENABLED;
    %% 	    disabled -> ?OperState_DISABLED
    %% 	end,
    %% Fun = fun() ->
    %% 		  [Obj] = mnesia:wread({brmBackupScheduler, Scheduler}),
    %% 		  mnesia:write(Obj#brmBackupScheduler{schedulerState=S})
    %% 	  end,
    %% {atomic, ok} = mnesia:transaction(Fun),
    ok.

datetime_to_now(DateTime) when is_tuple(DateTime) ->
    T = (calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200),
    {T div 1000000, T rem 1000000, 0}.

now_to_integer({A,B,C}) ->
    (A*1000000+B)*1000000+C.

log(Msg) ->
    logI:write_log("SwmInternal", "BrmBackupScheduler", info, Msg).

log(e, Msg) ->
    logI:write_log("SwmInternal", "BrmBackupScheduler", info, Msg),
    swmLib:write_swm_log("BrmBackupScheduler", info, Msg).


format_msg(Format) ->		      
    format_msg(Format, []).
%    re:replace(lists:flatten(io_lib:format("~p",[Term])), "\n", " ").
format_msg(Format, Args) ->		      
    re:replace(lists:flatten(io_lib:format(Format,Args)), "\n", " ").

format_date({{Y,M,D},{H,Mi,S}}) ->
    lists:append([integer_to_list(Y), "-", padzero(M), "-", padzero(D),"T",
		  padzero(H), ":", padzero(Mi), ":", padzero(S)]).

padzero(N) ->
    if N<10 -> [$0, N+$0];
       true -> integer_to_list(N)
    end.

scheduler_debug(Fmt) ->
    scheduler_debug(Fmt, []).

scheduler_debug(Fmt, Args) ->
    case swmLib:get_variable(scheduler_debug) of
	true ->
	    info_msg(Fmt, Args);
	_ ->
	    ok
    end.

%%% ----------------------------------------------------------
%%% #           cmd(Cmd)
%%% Input: Cmd:string()
%%% Output: 
%%% Exceptions: 
%%% Description: Execute a shell command and print the result in the erlan
%%% ----------------------------------------------------------
cmd(Cmd) ->
    info_msg("~s~n~s~n",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
    R.
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
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%error_msg(Format) ->
%    error_msg(Format, []).
%% error_msg(Format, Args) ->
%%     sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
