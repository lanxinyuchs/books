%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_2038_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R6A/2
%%%
%%% @doc == Test Suite for testing the 2038 problem in scheduled backup==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end

-module(swm_2038_SUITE).
-vsn('/main/R2A/R3A/R6A/2').
-author('etxkols').

%%% Except as specifically authorized in writing by Ericsson, the
%%% receiver of this document shall keep the information contained
%%% herein confidential and shall protect the same in whole or in
%%% part from disclosure and dissemination to third parties.
%%%
%%% Disclosure and disseminations to the receivers employees shall
%%% only be made on a strict need to know basis.
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2014-04-11 etxjotj     Created
%%% R3A/1      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R6A/2      2016-08-18 etxkols     Git migration requires that CC paths is not used 
%%% ----------------------------------------------------------


-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([single_scheduled_time/1,
 	 start_time_periodic/1,
	 stop_time_periodic/1,
	 next_job_periodic/1,
	 start_time_calendar/1,
	 stop_time_calendar/1]).

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [single_scheduled_time, 
     start_time_periodic, stop_time_periodic,
     next_job_periodic,
     start_time_calendar, stop_time_calendar].

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_netconf,nc1},
                 {cth_conn_log,[]},
                 {rct_core,[]},
                 {rct_logging,
		  {upgrade , [{erlang,{["ERROR REPORT","CRASH REPORT"],
				  ["localtime_to_universaltime"]}}]}}
    ]}].

%% @hidden
init_per_suite(Config) ->

    {Wallclock,Now}=rct_rpc:call(rpc_1,swmBackupScheduler,clock_test,[],10000),
    Diff = (element(1, Wallclock)-element(1, Now))*1000000+
	(element(2, Wallclock)-element(2, Now)),
    ct:pal("Wallclock: "++format_date(Wallclock)++"~n"++
	       "Now time:  "++format_date(Now)++"~n"++
	       "Diff: ~w seconds~n",[Diff]),
    case Diff of
	Diff when Diff > 300 ->
	    ct:fail(large_time_difference);
	_ ->
	    ok
    end,

    rct_rpc:call(rpc_1, swmLib, set_variable, [scheduler_debug, true], 10000),
    RootDir = proplists:get_value(priv_dir, Config),
    os:cmd(["chmod a+rwx ", RootDir]),

    Name = "swm_backup_SUITE",
    MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
    MeId =
	case proplists:get_value(networkManagedElementId, MeData) of
	    undefined ->
		"1";
	    NMEI -> NMEI
	end,
    [{meId, MeId},
     {backupName, Name},
     {sftp_host, "10.68.200.11"},
     {sftp_user, "mauve"},
     {sftp_pass, "dilbert"},
     {sftp_root, RootDir} | Config].
%% @hidden
end_per_suite(_Config) ->
    rct_rpc:call(rpc_1, swmLib, erase_variable, [scheduler_debug], 10000),
    ok.
%% @hidden
init_per_testcase(TestCase, Config) ->
    ct:print("Running ~w~n",[TestCase]),
    set_admin_state(Config, "LOCKED"),
    clear_schedule(Config),
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.

format_date(Now) ->
    {{Y,Mo,D},{H,Mi,S}} = calendar:now_to_datetime(Now),
    Frac = element(3, Now),
    io_lib:format("~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~6..0w UTC",
		  [Y,Mo,D,H,Mi,S,Frac]).

%%--------------------------------------------------------------------
%% @doc Prevent 2038 dates in Single Event
%% @end

single_scheduled_time(Config) ->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config),
    ok= swmSchedBuLib:set_scheduled_backup_name(MeId, Name),
    Attributes = [{brmSingleEventId, "swm_2038_SUITE"},
		  {scheduledTime, "2038-01-19T03:14:08"}],
    case swmSchedBuLib:create_single_event(MeId, Attributes) of
	ok ->
	    ct:fail("BrmSingleEvent accepted a 2038 date");
	{error, Reason} ->
	    Msg = get_error_message(Reason),
	    ct:pal("~s~n",[Msg]),
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc Prevent 2038 dates in periodic event start time
%% @end


start_time_periodic(Config) ->
     MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config),
    ok = swmSchedBuLib:set_scheduled_backup_name(MeId, Name),

    Attributes = [{brmPeriodicEventId, "swm_scheduled_backup_SUITE"},
		  {startTime, "2038-01-19T03:14:08"},
		  {stopTime, "2038-02-19T03:14:08"},
		  {hours, "0"},
		  {minutes, "5"}],
    case swmSchedBuLib:create_periodic_event(MeId, Attributes) of
	ok ->
	    ct:fail("BrmPeriodicEvent accepted a 2038 date");
	{error, Reason} ->
	    Msg = get_error_message(Reason),
	    ct:pal("~s~n",[Msg]),
	    ok
    end.

stop_time_periodic(Config) ->
     MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config),
    ok = swmSchedBuLib:set_scheduled_backup_name(MeId, Name),

    TS = timestamp(),
    StartTime = iso_time(add_time(1000, TS), extended),
    Attributes = [{brmPeriodicEventId, "swm_scheduled_backup_SUITE"},
		  {startTime, StartTime},
		  {stopTime, "2038-01-19T03:14:08"},
		  {hours, "0"},
		  {minutes, "5"}],
    case swmSchedBuLib:create_periodic_event(MeId, Attributes) of
	ok ->
	    ct:fail("BrmPeriodicEvent accepted a 2038 date");
	{error, Reason} ->
	    Msg = get_error_message(Reason),
	    ct:pal("~s~n",[Msg]),
	    ok
    end.

next_job_periodic(Config) ->
    MeId = proplists:get_value(meId, Config),
    Name = proplists:get_value(backupName, Config),
    ok = swmSchedBuLib:set_scheduled_backup_name(MeId, Name),

    set_admin_state(Config, "UNLOCKED"),

    ProgressFilter = {'ManagedElement',
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],[MeId]},
		       {'SystemFunctions',
			[{systemFunctionsId,[],["1"]},
			 {'BrM',
			  [{brMId,[],["1"]},
			   {'BrmBackupManager',
			    [{brmBackupManagerId,[],["1"]},
			     {'BrmBackupScheduler',
			      [{brmBackupSchedulerId, [], ["1"]},
			       {progressReport, []}]}]}]}]}]},
    ActionId = get_action_id(ProgressFilter),
    ct:pal("Previous ActionId is ~p~n",[ActionId]),

    TS = timestamp(),

    StartTime = iso_time(add_time(add_time(), TS), extended),
    Attributes = [{brmPeriodicEventId, "swm_scheduled_backup_SUITE"},
		  {startTime, StartTime},
		  {hours, "0"},
		  {weeks, "2345"}],
    ok = swmSchedBuLib:create_periodic_event(MeId, Attributes),
    case wait_for_progress(progressReport, ActionId, ProgressFilter) of
	["SUCCESS"] ->
	    Res = get_result_info(ProgressFilter),
	    ct:pal("resultInfo: ~p~n",[Res]),
	    timer:sleep(5000),
	    case is_alarm(MeId, "9175044") of
		true ->
		    ok;
		false ->
		    ct:fail("No 2038 alarm was sent")
	    end;
	Result ->
	    ct:pal("createBackup: ~s~n",[Result]),
	    ct:fail(Result)
    end.




start_time_calendar(Config) ->
    MeId = proplists:get_value(meId, Config),
    Time = "13:34:00",
    Attributes = 
	[{brmCalendarBasedPeriodicEventId, "swm_scheduled_backup_SUITE"},
	 {startTime, "2038-01-19T03:14:08"},
	 {stopTime, "2038-02-19T03:14:08"},
	 {time, Time}],
    case swmSchedBuLib:create_calendar_event(MeId, Attributes) of
	ok ->
	    ct:fail("BrmCalendarBasedPeriodicEvent accepted a 2038 date");
	{error, Reason} ->
	    Msg = get_error_message(Reason),
	    ct:pal("~s~n",[Msg]),
	    ok
    end.

stop_time_calendar(Config) ->
    MeId = proplists:get_value(meId, Config),
    TS = timestamp(),
    StartTime = iso_time(add_time(1000, TS), extended),
    Time = "13:34:00",
    Attributes = 
	[{brmCalendarBasedPeriodicEventId, "swm_scheduled_backup_SUITE"},
	 {startTime, StartTime},
	 {stopTime, "2038-02-19T03:14:08"},
	 {time, Time}],
    case swmSchedBuLib:create_calendar_event(MeId, Attributes) of
	ok ->
	    ct:fail("BrmCalendarBasedPeriodicEvent accepted a 2038 date");
	{error, Reason} ->
	    Msg = get_error_message(Reason),
	    ct:pal("~s~n",[Msg]),
	    ok
    end.




clear_schedule(Config) ->
    MeId = proplists:get_value(meId, Config),
    Get = {'ManagedElement',
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],[MeId]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'BrM',
	       [{brMId,[],["1"]},
		{'BrmBackupManager',
		 [{brmBackupManagerId,[],["1"]},
		  {'BrmBackupScheduler',
		   [{brmBackupSchedulerId,[], ["1"]}]}]}]}]}]},
    {ok, Res} = netconf(get_config, [nc1, running, Get]),
    {ok, {'BrmBackupScheduler', Attrs, Content}} =
	extract_element('BrmBackupScheduler', Res),
    Delete = {'ManagedElement',
	      [{xmlns,"urn:com:ericsson:ecim:ComTop"},
	       {'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
	      [{managedElementId,[],[MeId]},
	       {'SystemFunctions',
		[{systemFunctionsId,[],["1"]},
		 {'BrM',
		  [{brMId,[],["1"]},
		   {'BrmBackupManager',
		    [{brmBackupManagerId,[],["1"]},
		     {'BrmBackupScheduler', Attrs,
		      [{brmBackupSchedulerId,[], ["1"]}|
		       [case element(1, Item) of
			    'BrmSingleEvent' = Element ->
				{ok, Id} = extract_element(
					     brmSingleEventId, [Item]),
				{Element, [{'xc:operation', "delete"}], [Id]};
			    'BrmPeriodicEvent' = Element->
				{ok, Id} = extract_element(
					     brmPeriodicEventId, [Item]),
				{Element, [{'xc:operation', "delete"}], [Id]};
			    'BrmCalendarBasedPeriodicEvent' = Element ->
				{ok, Id} = extract_element(
					     brmCalendarBasedPeriodicEventId, [Item]),
				{Element, [{'xc:operation', "delete"}], [Id]}
			end||Item<-Content,
			     (element(1,Item) == 'BrmSingleEvent') or
				 (element(1, Item) == 'BrmPeriodicEvent') or
				 (element(1, Item) == 'BrmCalendarBasedPeriodicEvent')]
		       ]}]}]}]}]},

    ok = netconf(edit_config, [nc1, running, Delete]).




%%%--------------------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%%--------------------------------------------------------------------

is_alarm(MeId, MinorType) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    Res = swm_test_lib:is_alarm(nc1, MeId, MinorType),
    ct_netconfc:close(nc1),
    Res.

get_error_message(Reason) ->
    {ok, {_, _, [Msg]}} = extract_element('error-message', Reason),
    Msg.

add_time() ->
    case rct_rpc:call(rpc_1, sysEnv, rcs_mode, [], 10000) of
	simulated ->
	    15;
	target ->
	    120
    end.

%%%--------------------------------------------------------------------
%%% Description: Set the BrmBackupScheduler.adminState attribute
%%%--------------------------------------------------------------------

set_admin_state(Config, Value) ->
    MeId = proplists:get_value(meId, Config),
    Set =
    	{'ManagedElement',
    	 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    	 [{managedElementId,[],[MeId]},
    	  {'SystemFunctions',
    	   [{systemFunctionsId,[],["1"]},
    	    {'BrM',
    	     [{brMId,[],["1"]},
    	      {'BrmBackupManager',
    	       [{brmBackupManagerId,[],["1"]},
    		{'BrmBackupScheduler',
    		 [{brmBackupSchedulerId,[], ["1"]},
		  {adminState, [Value]}]}]}]}]}]},

    ok = netconf(edit_config, [nc1, running, Set]).

%%%--------------------------------------------------------------------
%%% Description: Read the action id of the last progress report to prevent
%%%              reading old reports
%%%--------------------------------------------------------------------

get_action_id(ProgressFilter) ->
    get_progress_report_member(actionId, ProgressFilter).

%%%--------------------------------------------------------------------
%%% Description: Read the resultinfo info of the last progress report
%%%--------------------------------------------------------------------

get_result_info(ProgressFilter) ->
    get_progress_report_member(resultInfo, ProgressFilter).

%%%--------------------------------------------------------------------
%%% Description: Read a member of the progress report struct
%%%--------------------------------------------------------------------

get_progress_report_member(Member, ProgressFilter) ->
    {ok, A} = netconf(get, [nc1, ProgressFilter]),
    case extract_element(progressReport, A) of
	{ok, {progressReport, [{unset, "true"}], []}}  ->
	    undefined;
	_ ->
	    {ok, {Member, [], [Value]}} =
		extract_element(Member, A),
	    Value
    end.

%%%--------------------------------------------------------------------
%%% Description: Read the value of BrmBackupScheduler.nextScheduledTime
%%%--------------------------------------------------------------------

%% get_next_scheduled_time(MeId) ->
%%     Get = {'ManagedElement',
%% 	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
%% 	   [{managedElementId,[],[MeId]},
%% 	    {'SystemFunctions',
%% 	     [{systemFunctionsId,[],["1"]},
%% 	      {'BrM',
%% 	       [{brMId,[],["1"]},
%% 		{'BrmBackupManager',
%% 		 [{brmBackupManagerId,[],["1"]},
%% 		  {'BrmBackupScheduler',
%% 		   [{brmBackupSchedulerId, [], ["1"]},
%% 		    {nextScheduledTime, []}]}]}]}]}]},

%%     {ok, A} = netconf(get, [nc1, Get]),
%%     case extract_element(nextScheduledTime, A) of
%% 	{ok, {nextScheduledTime, [{unset, "true"}], []}} ->
%% 	    undefined;
%% 	{ok, {nextScheduledTime, _, [Time]}} ->
%% 	    Time
%%     end.



%%% ----------------------------------------------------------
%%% @doc Convert an os:timestamp() tuple to an ISO 8601 string
%%%
%%% Input: Now  - An os:timestamp() tuple
%%%        Type - basic|extended|extended_zonefree|extended_z
%%% Output: string()
%%% @end
%%% ----------------------------------------------------------

iso_time(Now, Type) ->
    fn_date(Now, Type)++"T"++fn_time(Now, Type).

%% time_offset(Now) ->
%%     DT = calendar:now_to_local_time(Now),
%%     UTC = calendar:now_to_universal_time(Now),
%%     DiffSecs = calendar:datetime_to_gregorian_seconds(DT) -
%%         calendar:datetime_to_gregorian_seconds(UTC),
%%     [Sign, DH, DM] = diff(DiffSecs),
%%     lists:append([[Sign], padzero(DH), ":", padzero(DM)]).


fn_date(Now, Type) ->
    {{Y,M,D}, _} = calendar:now_to_local_time(Now),
    case Type of
        basic ->
            lists:append([integer_to_list(Y),
                          padzero(M),
                          padzero(D)]);
	extended_z ->
	    {{YU,MU,DU}, _} = calendar:now_to_universal_time(Now),

            lists:append([integer_to_list(YU), "-",
                          padzero(MU), "-", padzero(DU)]);

        Extended when Extended==extended; Extended==extended_zonefree ->
            lists:append([integer_to_list(Y), "-",
                          padzero(M), "-", padzero(D)])
    end.

fn_time(Now, Type) ->
    DT={_, {H, M, S}} = calendar:now_to_local_time(Now),
    UTC = calendar:now_to_universal_time(Now),
    DiffSecs = calendar:datetime_to_gregorian_seconds(DT) -
        calendar:datetime_to_gregorian_seconds(UTC),
    [Sign, DH, DM] = diff(DiffSecs),
    case Type of
        basic ->
            lists:append([padzero(H),
                          padzero(M),
                          padzero(S),
                          [Sign],
                          padzero(DH),
                          padzero(DM)]);
	extended_z ->
	    {_, {HU, MU, SU}} = UTC,
            lists:append([padzero(HU), ":",padzero(MU), ":",padzero(SU),"Z"]);
        extended ->
            lists:append([padzero(H), ":", padzero(M), ":", padzero(S),
                          [Sign], padzero(DH), ":", padzero(DM)]);
        extended_zonefree ->
            lists:append([padzero(H), ":", padzero(M), ":", padzero(S)])
    end.

%% format_date({{Y,M,D},{H,Mi,S}}) ->
%%     lists:append([integer_to_list(Y), "-", padzero(M), "-", padzero(D),"T",
%% 		  padzero(H), ":", padzero(Mi), ":", padzero(S)]).


padzero(N) ->
    if N<10 -> [$0, N+$0];
       true -> integer_to_list(N)
    end.


diff(Secs) ->
    case calendar:seconds_to_daystime(Secs) of
        {0, {H, M,_}} ->
                [$+, H, M];
        {-1, _} ->
                {0, {H, M, _}} = calendar:seconds_to_daystime(-Secs),
                [$-, H, M]
    end.

%%%--------------------------------------------------------------------
%%% Description: Adds seconds to a now-tuple
%%%--------------------------------------------------------------------

add_time(N, {N1,N2,N3}) ->
    case N2+N of
	F2 when F2 >= 1000000 ->
	    {N1+F2 div 1000000, F2 rem 1000000, N3};
	F2 ->
	    {N1, F2, N3}
    end.

%%%--------------------------------------------------------------------
%%% Description: Get node time
%%%--------------------------------------------------------------------

timestamp() ->
    rct_rpc:call(rpc_1, os, timestamp, [], 10000).


%%%--------------------------------------------------------------------
%%% Description: Evaluate an os cmd and print both the command and result
%%%--------------------------------------------------------------------

%% cmd(Cmd) ->
%%     ct:pal("~s~n~s~n",[lists:flatten(Cmd), R = os:cmd(Cmd)]),
%%     R.

%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------

netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc1, []),
    case apply(ct_netconfc, F, A) of
	ok ->
	    ok = ct_netconfc:close_session(nc1),
	    ok;
	{ok, Res} ->
	    ok = ct_netconfc:close_session(nc1),
	    {ok, Res};
	Other ->
	    Other
    end.

%%%--------------------------------------------------------------------
%%% Description: This function and the associated check_progress_elements
%%%              loops over a netconf get for the progress information until
%%%              the state is FINISHED, otherwise it prints the status and
%%%              progress percentage
%%%--------------------------------------------------------------------


wait_for_progress(Attribute, OldActionId, ProgressFilter) ->
    timer:sleep(5000),
    case ct_netconfc:open(nc1, []) of
	{ok, _} ->
	    Get = ct_netconfc:get(nc1, ProgressFilter),
	    ct_netconfc:close_session(nc1),
	    case Get of
		{ok, Report} ->
		    case extract_element(actionId, Report) of
			{ok, {actionId, _, [OldActionId]}} ->
			    %% ct:pal("Waiting for updated progress ~n~p~n",[Report]),
			    wait_for_progress(Attribute, OldActionId,
					      ProgressFilter);
			_ ->
			    case check_progress_elements(Attribute, Report) of
				loop ->
				    wait_for_progress(Attribute, OldActionId,
						      ProgressFilter);
				{ok, Result} ->
				    Result
			    end
		    end;
		{error, Error} ->
		    ct:pal("Netconf get error:~n~p",[Error]),
		    wait_for_progress(Attribute, OldActionId, ProgressFilter)
	    end;
	{error, Reason} ->
	    ct:pal("Netconf open error:~n~p",[Reason]),
	    wait_for_progress(Attribute, OldActionId, ProgressFilter)
    end.


check_progress_elements(Attribute, ProgressReport) ->
    {ok, Report} = extract_element(Attribute, ProgressReport),
    {ok, State} = extract_element(state, [Report]),
    case State of
	{state, _, ["FINISHED"]} ->
	    format_progress_report(Report),
	    ct:pal("~s",[format_progress_report(Report)]),
	    {ok, {result, _, Result}} = extract_element(result, [Report]),
	    {ok, Result};
	{state, _, [Current]} ->
	    {ok, {progressPercentage, _, [Percent]}} =
		extract_element(progressPercentage, [Report]),
	    {ok, {progressInfo, _, [Info]}} =
		extract_element(progressInfo, [Report]),
	    ct:pal("State: ~s ~s % ready~n~s",[Current, Percent, Info]),
	    loop
    end.


%%%--------------------------------------------------------------------
%%% Description: Format a AsyncActionStruct for printing
%%%--------------------------------------------------------------------


format_progress_report({progressReport, _, Members}) ->
    [io_lib:format("progressReport:~n",[])|format_progress_report(Members)];
format_progress_report([{Key, _, [Value]}|Members]) ->
    [io_lib:format("~w: ~s ~n",[Key, Value])|
     format_progress_report(Members)];
format_progress_report([{Key, _, []}|Members]) ->
    [io_lib:format("~w: ~s ~n",[Key, ""])|
     format_progress_report(Members)];
format_progress_report([]) -> [];
format_progress_report(X) ->
    ct:pal("Unknown format: ~p~n",[X]),
    ct:fail(unknown_progress_report_format).


%%%--------------------------------------------------------------------
%%% Description: From a general short xml syntax, extract a certain xml
%%%              element
%%%--------------------------------------------------------------------

extract_element(Element, [{Element, Attribute, Content}|_]) ->
    {ok, {Element, Attribute, Content}};
extract_element(Element, [{Element, Content}|_]) ->
    {ok, {Element, Content}};
extract_element(Element, [{_, _, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [{_, Content}|Elements]) ->
    case extract_element(Element, Content) of
	{ok, Value} ->
	    {ok, Value};
	not_found ->
	    extract_element(Element, Elements)
    end;
extract_element(Element, [_|Elements]) ->
    extract_element(Element, Elements);
extract_element(_, []) ->
    not_found.

