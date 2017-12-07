%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	measure_restart_ug_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R5A/2
%%%
%%% @doc ==Measure times for differents restarts causes.==
%%% This Test Suite can be used on target enviroment.
%%% It will also measure differents startup times and som cases measure used memory size,
%%% after differents types of restarts.
%%% Used sw version and measured data is written to a file with same name as TC.
%%% Path: /proj/rcs/measurements/
%%%
%%% Note! Measure time when CS starts will sometimes fail due to sometimes the printouts on the consol
%%% will consist of of other text than expect.
%%% So until a better way to measure when CS start can be done, then measure when login prompt comes.
%%% <br/><br/>
%%%
%%% @end


-module(measure_restart_ug_SUITE).

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2014-02-20 etxivri     Created, use this after upgrade.
%%%                                   Same as measure_restart_SUITE.erl,
%%%                                   print measured data to another file.
%%% R2A/3      2014-03-07 etxivri     Update to add Branch file name when
%%%                                   other than Branch R2A is used.
%%%                                   And changed RESULTDIR path.
%%% R2A/4      2014-06-03 etxivri     Changed check for "du1 login" prompt to "login:"
%%% R3A/1      2014-12-1 etxivri      read SwVersion instead of SwVersionMain.
%%% R5A/1      2016-01-21 erarafo     Fixed deprecation warnings and copyright
%%% R5A/2      2016-02-17 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%%
%%% This SUITE is now obsolete. Tests exist in measure_restart_SUITE, ug group.
%%% Changed due to skip to maintain two SUITEs.
%%%
%%% ----------------------------------------------------------
%%%

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 %% wait_for_mw_start/1,
	 wait_for_login_prompt/1,
	 wait_for_com_started/1,
	 wait_for_netconf_started/1,
	 wait_for_appl_started/1,
	 get_used_memsize/0,
	 get_sw_version/0,
	 groups/0,
	 all/0,
	 measure_reboot_mw_login_com_appl_netconf_time_memsize/1,
	 measure_power_mw_login_com_appl_netconf_time_memsize/1,
	 measure_initial_restart_com_appl_netconf_time_memsize/1
	]).

-define(RESULTDIR, "/proj/rcs/measurements/ug/").
%% -define(RESULTDIR, "/home/etxivri/tmp/").

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 600}}, % 10 hours
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {rct_power,node},
		 {rct_netconf, nc1},
		 {cth_conn_log, []},
		 {rct_consserv,cs1},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
                 {rct_rs232,console},
		 {rct_cli, {cli, [manual_connect]}},
		 {rct_tlib,{load,[]}},
                 {rct_core,[]}
		]}].


%% @hidden
init_per_suite(Config) ->
    %% Not working yet.
    %% TestNodes = ct:get_config(test_nodes),
    %% Node = proplists:get_value(1, TestNodes),
    %% ct:require(console, {Node, rs232}),
    Config.
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     measure_reboot_mw_login_com_appl_netconf_time_memsize,
     measure_power_mw_login_com_appl_netconf_time_memsize,
     measure_initial_restart_com_appl_netconf_time_memsize
    ].

%%--------------------------------------------------------------------
%% @doc
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{group_measure_restart_ug_1,[],[measure_reboot_mw_login_com_appl_netconf_time_memsize,
				     measure_power_mw_login_com_appl_netconf_time_memsize,
				     measure_initial_restart_com_appl_netconf_time_memsize
				    ]}].


%%--------------------------------------------------------------------
%% @doc
%% Measure start MW, login prompt, COM, Application and Netconf time and memory size, after reboot. <br/>
%% If failing to match the search string to measure MW, for a numbers of retires,<br/>
%% then measure from login.<br/>
%% Measured times and memory size is logged in a file.<br/>
%% /proj/rcs/measurements/measure_reboot_mw_login_com_appl_netconf_time_memsize.txt <br/>
%% SW_version;MW;Login;COM;Application;Netconf;Diff;Com_rss;Beam_rss;Tot_bc <br/>
%% @spec measure_reboot_mw_login_com_appl_netconf_time_memsize(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
measure_reboot_mw_login_com_appl_netconf_time_memsize(_Config) ->
    ct:pal("Measure start MW, login prompt, Application, COM and Netconf time and memsize after reboot."),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),

    %% %%%%
    %% %% Measure time for starting MW
    %% %% If failing to match the search string for a numbers of retires,
    %% %% then measure from login.
    %% %%%%
    %% case wait_for_mw_start(reboot) of
    %% 	{Start1, End1} ->
    %% 	    {Start1, End1};
    %% 	error_to_may_retries ->
    %% 	    %% Skip measure time for start MW
    %% 	    ok = ct:pal("### reboot!",[]),
    %% 	    ok = rct_rs232:login(console),
    %% 	    Start1 = erlang:timestamp(),
    %% 	    ok = ct_telnet:send(console, "reboot"),
    %% 	    {Start1, End1 = dummy}
    %% end,

    %%%%
    %% Measure time for starting
    %% Time for starting MW is not measured, soo it will be hardcoded.
    %%%%
    %% Skip measure time for start MW
    ok = ct:pal("### reboot!",[]),
    ok = rct_rs232:login(console),
    Start1 = erlang:timestamp(),
    ok = ct_telnet:send(console, "reboot"),
    End1 = dummy,
    StartMwTime = '-',

    %% case End1 of
    %% 	dummy ->
    %% 	    StartMwTime = '-';
    %% 	_ ->
    %% 	    StartMwTime = trunc(timer:now_diff(End1, Start1) / 1000 / 1000)
    %% end,

    %%%%
    %% Measure time for login prompt.
    %%%%
    End2 = wait_for_login_prompt(),
    StartLogInTime = trunc(timer:now_diff(End2, Start1) / 1000 / 1000),

    %%%%
    %% Measure time for COM to start
    %%%%
    End3 = wait_for_com_started(),
    StartComTime = trunc(timer:now_diff(End3, Start1) / 1000 / 1000),

    %%%%
    %% Measure startup time for Applications to be started
    %%%%
    End4 = wait_for_appl_started(),
    StartApplTime = trunc(timer:now_diff(End4, Start1) / 1000 / 1000),

    %%%%
    %% Measure startup time for Netconf
    %%%%
    End5 = wait_for_netconf_started(),
    ok = ct_netconfc:close_session(nc1),
    StartNetConfTime = trunc(timer:now_diff(End5, Start1) / 1000 / 1000),

    case End1 of
    	dummy -> %% If no time for starting MW then Diff uses from StartLogInTime.
    	    DiffStarts = StartNetConfTime-StartLogInTime;
    	_ ->
    	    DiffStarts = StartNetConfTime-StartMwTime
    end,

    ct:pal("StartUp times: MW: ~w,  LogIn: ~w,  COM: ~w,  Appl: ~w,  Netconf: ~w,  Diff: ~w~n",
    	   [StartMwTime,
    	    StartLogInTime,
    	    StartComTime,
    	    StartApplTime,
    	    StartNetConfTime,
    	    DiffStarts]),

    %%%%
    %% Get sw version, using cli
    %%%%
    CXS_label = get_sw_version(),

    %%%%
    %% Get used memory size
    %%%%
    [_Tot, Tot_bc, Beam_rss, Com_rss] = get_used_memsize(),

    %%%%
    %% Get disc usage
    %%%%
    [SW_DiscUsage, RCS_DiscUsage , COM_DiscUsage] = get_disc_usage(),

    %%%%
    %% Update log measurement file
    %%%%
    FileName = get_filename("ug_measure_reboot_time_memsize.txt"),
    updateMeasResFile(FileName, "~p;~w;~w;~w;~w;~w;~w;~w;~s;~s;~s;~s;~s;~s~n",
		      [httpd_util:rfc1123_date(),
		       CXS_label,
		       StartMwTime,
		       StartLogInTime,
		       StartComTime,
		       StartApplTime,
		       StartNetConfTime,
		       DiffStarts,
		       Com_rss,
		       Beam_rss,
		       Tot_bc,
		       SW_DiscUsage,
		       RCS_DiscUsage ,
		       COM_DiscUsage]),

    ct:pal("results file: ~p ,\n path: ~p \n",[FileName, ?RESULTDIR]),

    %%%%
    %% Evaluate times
    %%%%
    case End1 of
    	dummy -> %% If no time for starting MW
	    ok = evaluate_times_after_reboot(StartLogInTime, DiffStarts);
    	_ ->
	    ok = evaluate_times_after_reboot(StartMwTime, DiffStarts)
    end,

    ct:pal("Memsize: Com_rss: ~s,  Beam_rss: ~s,  Tot_bc: ~s ~n",
	   [Com_rss,
	    Beam_rss,
	    Tot_bc]),

    ct:pal("Disc Usage: SW: ~s,  RCS: ~s,  Com: ~s ~n",
    	   [SW_DiscUsage,
	    RCS_DiscUsage ,
	    COM_DiscUsage]),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Measure start MW, Login, COM, Application and Netconf time and memory size after power on.<br/>
%% If failing to match the search string to measure MW, for a numbers of retires,<br/>
%% then measure from login.<br/>
%% Measured times is logged in a file.<br/>
%% /proj/rcs/measurements/measure_power_mw_login_com_appl_netconf_time_memsize.txt <br/>
%% SW_version;MW;Login;COM;Application;Netconf;Diff;Com_rss;Beam_rss;Tot_bc <br/>
%% @spec measure_power_mw_login_com_appl_netconf_time_memsize(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
measure_power_mw_login_com_appl_netconf_time_memsize(_Config) ->
    ct:pal("Measure start MW, login prompt, COM, Application and Netconf time and memsize after power on."),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),

    %% %%%%
    %% %% Measure time for starting MW.
    %% %% If failing to match the search string for a numbers of retires,
    %% %% then measure from login.
    %% %%%%
    %% case wait_for_mw_start(power) of
    %% 	{Start1, End1} ->
    %% 	    {Start1, End1};
    %% 	error_to_may_retries -> %% Skip measure time for MW"
    %% 	    ok = ct:pal("### power off/on!",[]),
    %% 	    ok = rct_power:off(node),
    %% 	    Start1 = wait_for_power_on(),
    %% 	    {Start1, End1 = dummy}
    %% end,

    %%%%
    %% Measure time for starting.
    %% Time for starting MW is not measured, soo it will be hardcoded.
    %%%%
    ok = ct:pal("### power off/on!",[]),
    ok = rct_power:off(node),
    Start1 = wait_for_power_on(),
    End1 = dummy,
    StartMwTime = '-',

    %% case End1 of
    %% 	dummy ->
    %% 	    StartMwTime = '-';
    %% 	_ ->
    %% 	    StartMwTime = trunc(timer:now_diff(End1, Start1) / 1000 / 1000)
    %% end,

    %%%%
    %% Measure time for login prompt.
    %%%%
    End2 = wait_for_login_prompt(),
    StartLogInTime = trunc(timer:now_diff(End2, Start1) / 1000 / 1000),

    %%%%
    %% Measure time for COM to start
    %%%%
    End3 = wait_for_com_started(),
    StartComTime = trunc(timer:now_diff(End3, Start1) / 1000 / 1000),

    %%%%
    %% Measure startup time for Applications to be started
    %%%%
    End4 = wait_for_appl_started(),
    StartApplTime = trunc(timer:now_diff(End4, Start1) / 1000 / 1000),

    %%%%
    %% Measure startup time for Netconf
    %%%%
    End5 = wait_for_netconf_started(),
    ok = ct_netconfc:close_session(nc1),
    StartNetConfTime = trunc(timer:now_diff(End5, Start1) / 1000 / 1000),

    case End1 of
	dummy -> %% If no time for starting MW then Diff uses from StartLogInTime.
	    DiffStarts = StartNetConfTime-StartLogInTime;
	_ ->
	    DiffStarts = StartNetConfTime-StartMwTime
    end,

    ct:pal("StartUp times: MW: ~w,  LogIn: ~w,  COM: ~w,  Appl: ~w,  Netconf: ~w,  Diff: ~w~n",
	   [StartMwTime,
	    StartLogInTime,
	    StartComTime,
	    StartApplTime,
	    StartNetConfTime,
	    DiffStarts]),

    %%%%
    %% Get sw version, using cli
    %%%%
    CXS_label = get_sw_version(),

    %%%%
    %% Get used memory size
    %%%%
    [_Tot, Tot_bc, Beam_rss, Com_rss] = get_used_memsize(),

    %%%%
    %% Get disc usage
    %%%%
    [SW_DiscUsage, RCS_DiscUsage , COM_DiscUsage] = get_disc_usage(),

    %%%%
    %% Update log measurement file
    %%%%
    FileName = get_filename("ug_measure_power_time_memsize.txt"),
    updateMeasResFile(FileName, "~p;~w;~w;~w;~w;~w;~w;~w;~s;~s;~s;~s;~s;~s~n",
		      [httpd_util:rfc1123_date(),
		       CXS_label,
		       StartMwTime,
		       StartLogInTime,
		       StartComTime,
		       StartApplTime,
		       StartNetConfTime,
		       DiffStarts,
		       Com_rss,
		       Beam_rss,
		       Tot_bc,
		       SW_DiscUsage,
		       RCS_DiscUsage ,
		       COM_DiscUsage
		      ]),

    ct:pal("results file: ~p ,\n path: ~p \n",[FileName, ?RESULTDIR]),


    %%%%
    %% Evaluate times
    %%%%
    case End1 of
    	dummy -> %% If no time for starting MW
	    ok = evaluate_times_after_power(StartLogInTime, DiffStarts);
    	_ ->
	    ok = evaluate_times_after_power(StartMwTime, DiffStarts)
    end,

    ct:pal("Memsize: Com_rss: ~s,  Beam_rss: ~s,  Tot_bc: ~s ~n",
	   [Com_rss,
	    Beam_rss,
	    Tot_bc]),

    ct:pal("Disc Usage: SW: ~s,  RCS: ~s,  Com: ~s ~n",
    	   [SW_DiscUsage,
	    RCS_DiscUsage ,
	    COM_DiscUsage]),

    ok.


%% ===========================================================================
%% @doc
%% Measure restart COM, Application and Netconf time and memory size after initial restart.<br/>
%% Measured times is logged in a file.<br/>
%% /proj/rcs/measurements/measure_initial_restart_time_memsize.txt <br/>
%% SW_version;COM;Application;Netconf;Diff;Com_rss;Beam_rss;Tot_bc <br/>
%% @spec measure_initial_restart_com_appl_netconf_time_memsize(_Config) -> ok
%% @end
%% ===========================================================================
measure_initial_restart_com_appl_netconf_time_memsize(_Config) ->
    ct:pal("Measure start COM, Application and Netconf time and memsize after initial restart."),

    %%%%
    %% Get COM pid.
    %%%%
    Com = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 20000),
    ct:pal("COM: ~p", [Com]),
    ComPid = case re:run(Com, "^[0-9]+$", [{capture,[1],list}]) of
    		 {match, _Result} ->
    		     Com;
    		 _ ->
    		     ct:fail("No com pid found")
    end,

    %% Data = rct_rpc:call(rpc, os, cmd, ["pgrep com"], 10000),
    %% ComPid = check_for_invalid_data(Data),

    %%%%
    %% Perform a initial restart.
    %%%%
    Start1 = erlang:timestamp(),
    rct_rpc:call(rpc, init, restart, [], 10000),

    %%%%
    %% Check that com is restarted with a new pid.
    %%%%
    End3 = wait_for_com_restarted(ComPid),
    StartComTime = trunc(timer:now_diff(End3, Start1) / 1000 / 1000),

    %%%%
    %% Measure startup time for Applications to be started
    %%%%
    End4 = wait_for_appl_started(),
    StartApplTime = trunc(timer:now_diff(End4, Start1) / 1000 / 1000),

    %%%%
    %% Measure startup time for Netconf
    %%%%
    End5 = wait_for_netconf_started(),
    ok = ct_netconfc:close_session(nc1),
    StartNetConfTime = trunc(timer:now_diff(End5, Start1) / 1000 / 1000),

    DiffStarts = StartNetConfTime - StartComTime,

    ct:pal("StartUp times: Com: ~w, Appl: ~w,  Netconf: ~w,  Diff: ~w~n",
    	   [StartComTime,
    	    StartApplTime,
    	    StartNetConfTime,
    	    DiffStarts]),

    %%%%
    %% Get sw version, using cli
    %%%%
    CXS_label = get_sw_version(),

    %%%%
    %% Get used memory size
    %%%%
    [_Tot, Tot_bc, Beam_rss, Com_rss] = get_used_memsize(),

    %%%%
    %% Get disc usage
    %%%%
    [SW_DiscUsage, RCS_DiscUsage , COM_DiscUsage] = get_disc_usage(),

    %%%%
    %% Update log measurement file
    %%%%
    FileName = get_filename("ug_measure_initial_restart_time_memsize.txt"),
    updateMeasResFile(FileName, "~p;~w;~w;~w;~w;~w;~w;~w;~s;~s;~s;~s;~s;~s~n",
    		      [httpd_util:rfc1123_date(),
    		       CXS_label,
		       '-', %StartMwTime not meassured.
		       '-', %StartLogInTime not meassured.
    		       StartComTime,
    		       StartApplTime,
    		       StartNetConfTime,
    		       DiffStarts,
    		       Com_rss,
    		       Beam_rss,
    		       Tot_bc,
		       SW_DiscUsage,
		       RCS_DiscUsage ,
		       COM_DiscUsage]),

    ct:pal("results file: ~p ,\n path: ~p \n",[FileName, ?RESULTDIR]),

    %%%%
    %% Evaluate times
    %%%%
    evaluate_times_after_init_restart(StartComTime, DiffStarts),

    ct:pal("Memsize: Com_rss: ~s,  Beam_rss: ~s,  Tot_bc: ~s ~n",
    	   [Com_rss,
    	    Beam_rss,
    	    Tot_bc]),

    ct:pal("Disc Usage: SW: ~s,  RCS: ~s,  Com: ~s ~n",
    	   [SW_DiscUsage,
	    RCS_DiscUsage ,
	    COM_DiscUsage]),

    ok.


%% ===========================================================================
%% @hidden
%% ===========================================================================
%% evaluate_times(RestartTime, _TimeDiff) when  RestartTime < 5 ->
%%     ct:fail("node has not been restarted",[]);
%% evaluate_times(RestartTime, _TimeDiff) when RestartTime > 50 ->
%%     ct:fail("node has not started up within expected time");
%% evaluate_times(_RestartTime, TimeDiff) when TimeDiff > 25 ->
%%     ct:fail("From start to end takes more time than expected!");
%% evaluate_times(_RestartTime, _TimeDiff) ->
%%     ok.

evaluate_times_after_power(RestartTime, _TimeDiff) when  RestartTime < 5 ->
    ct:fail("node has not been restarted",[]);
evaluate_times_after_power(RestartTime, _TimeDiff) when RestartTime > 90 ->
    ct:pal("TC fail after power on, To high restart Time: ~p sec > limit: 90 sec \n",[RestartTime]),
    ct:fail("node has not started up within expected time");
evaluate_times_after_power(_RestartTime, TimeDiff) when TimeDiff > 120 ->
    ct:pal("TC fail after power on. TimeDiff from MW to Netconf is to high. TimeDiff: ~p sec > limit: 120 sec \n",[TimeDiff]),
    ct:fail("From start to end takes more time than expected!");
evaluate_times_after_power(_RestartTime, _TimeDiff) ->
    ok.

evaluate_times_after_reboot(RestartTime, _TimeDiff) when  RestartTime < 5 ->
    ct:fail("node has not been restarted",[]);
evaluate_times_after_reboot(RestartTime, _TimeDiff) when RestartTime > 90 ->
    ct:pal("TC fail after reboot,  Restart Time: ~p sec > limit: 90 sec \n",[RestartTime]),
    ct:fail("node has not started up within expected time");
evaluate_times_after_reboot(_RestartTime, TimeDiff) when TimeDiff > 120 ->
    ct:pal("TC fail after reboot. TimeDiff from MW to Netconf is to high. TimeDiff: ~p sec > limit: 120 sec \n",[TimeDiff]),
    ct:fail("From start to end takes more time than expected!");
evaluate_times_after_reboot(_RestartTime, _TimeDiff) ->
    ok.

evaluate_times_after_init_restart(RestartTime, _TimeDiff) when  RestartTime < 5 ->
    ct:fail("init restart has dot been performed!",[]);
evaluate_times_after_init_restart(RestartTime, _TimeDiff) when RestartTime > 90 ->
    ct:pal("TC fail after init restart, Com restart Time: ~p sec > limit: 90 sec \n",[RestartTime]),
    ct:fail("Com has not restarted within expected time");
evaluate_times_after_init_restart(_RestartTime, TimeDiff) when TimeDiff > 120 ->
    ct:pal("TC fail after init restart. \nTime from Com started to reply on netconf message: ~p sec > limit: 120 sec \n",[TimeDiff]),
    ct:fail("From Com started to reply on netconf message takes more time than expected!");
evaluate_times_after_init_restart(_RestartTime, _TimeDiff) ->
    ok.

%% ===========================================================================
%% @doc
%% Wait for power on. <br/>
%% Start timestamp when a power on. <br/>
%% Sometimes power on does not take affect due to consolserver. <br/>
%% Then a new power off/on will be done. <br/>
%% @spec wait_for_power_on() -> timestamp
%% @end
%% ===========================================================================
wait_for_power_on() ->
     ct:pal("### Start Power On.", []),
     wait_for_power_on(60000).

wait_for_power_on(Timeout) when Timeout < 0 ->
    ct:fail("Power ON failed within expected time.");
wait_for_power_on(Timeout) ->
    case wait_for_power_on_2() of
	{ok, SecondStageUboot} ->
	    Start = erlang:timestamp(),
	    case check_node_is_starting(SecondStageUboot) of
		ok ->
		    Start;
		{_Error, Time} ->
		    wait_for_power_on(Timeout - Time)
	    end;
	{_Error, Time} ->
	    wait_for_power_on(Timeout - Time)
    end.

wait_for_power_on_2() ->
     case  rct_power:on(node, no_retries) of
	ok ->
	    %% Check that node has restarted. If no try to restart again
	    BoardType =
		 proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))),
	     SecondStageUboot = case BoardType of
				    BoardType when BoardType == "dus4101";
						   BoardType == "duw4101" ->
					"2:nd Stage Boot Loader";
				    _ ->
					"Ericsson Version: 2/CXC1736593/" % tcu03, dus5201, dus3201?
				end,
	     ct:log("SecondStageUboot: ~p", [SecondStageUboot]),
	     {ok, SecondStageUboot};
	 {error,econnrefused} = Error ->
	    timer:sleep(250), % If occupied then wait and try again.
	     {Error, 250};
	 Return ->
	     ct:pal("### Power ON failed, due to: ~w", [Return]),
	     ct:fail("Power ON failed.")
     end.

check_node_is_starting(SecondStageUboot) ->
    case ct_telnet:expect(console,
			  SecondStageUboot,
			  [{timeout, 20000}, no_prompt_check]) of
	{ok, _} ->
	    ok;
	{error, timeout} = Error ->
	    ct:pal("Power on failed within expected time, do power off/on again.",[]),
	    ok = rct_power:off(node),
	    {Error, 20000};
	Res ->
	    ct:pal("### Res: ~w", [Res]),
	    ct:fail("Power ON failed, du to unexpected return value from power on.")
     end.


%% %% ===========================================================================
%% %% @doc
%% %% Check for MW to be started. <br/>
%% %% Wait for a specific text string "Starting RCS" in the consol printouts. <br/>
%% %% There will be N:ths retries to get the search string.<br/>
%% %% Cmd = reboot | power
%% %% @spec wait_for_mw_start(Cmd) -> {Start, End} | ct_fail
%% %% @end
%% %% ===========================================================================
%% wait_for_mw_start(Cmd) ->
%%     wait_for_mw_start(Cmd, 1).

%% %% wait_for_mw_start( _Cmd, N) when N > 2 -> %% N:th tries
%% wait_for_mw_start( _Cmd, N) when N >= 1 -> %% Starting RCS does not printout in console any longer !
%%     error_to_may_retries;

%% %% This is not used due to, Starting RCS does not printout in console any longer !
%% wait_for_mw_start(Cmd, N) ->
%%     case Cmd of
%%     	reboot ->
%% 	    ok = rct_rs232:login(console),
%%     	    ct:pal("### reboot!",[]),
%%     	    Start = erlang:timestamp(),
%%     	    ok = ct_telnet:send(console, "reboot"),
%%     	    Start;
%%     	power ->
%%     	    ct:pal("### power off/on!",[]),
%%     	    ok = rct_power:off(node),
%%     	    Start = wait_for_power_on(),
%%     	    Start
%%     end,

%%     %% Sometimes consolprintouts with other info in "Starting RCS".
%%     %% case ct_telnet:expect(console, " RCS", [{timeout,50000}, no_prompt_check]) of
%%     case ct_telnet:expect(console, "Starting RCS", [{timeout,50000}, no_prompt_check]) of
%% 	{ok, _} ->
%% 	    End = erlang:timestamp(),
%% 	    {Start,End};
%% 	_  ->
%% 	    ct:pal("Could not match \"Starting RCS\", Do the test from the begining: Nr: ~p. ", [N+1]),
%% 	    wait_for_mw_start(Cmd, N+1)
%%     end.



%% ===========================================================================
%% @doc
%% Check for login prompt. <br/>
%% Wait for login prompt arrives in the consol printouts. <br/>
%% @spec wait_for_login_prompt() -> timestamp
%% @end
%% ===========================================================================
wait_for_login_prompt() ->
    wait_for_login_prompt(60000).

wait_for_login_prompt(Timeout) when Timeout < 500 ->
    ct:fail("No login prompt within max timeout after restart.");

wait_for_login_prompt(Timeout) ->
    case ct_telnet:expect(console, "login:", [{timeout,250}, no_prompt_check]) of
	{ok, _} ->
	    End = erlang:timestamp(),
	    End;
	_  ->
	    wait_for_login_prompt(Timeout - 250)
    end.


%% ===========================================================================
%% @doc
%% Check for COM started. <br/>
%% Wait for COM process to be started. <br/>
%% @spec wait_for_com_started() -> timestamp
%% @end
%% ===========================================================================
wait_for_com_started() ->
    wait_for_com_started(90000).

wait_for_com_started(Timeout) when Timeout < 500 ->
    ct:fail("COM not started within max timeout.");

wait_for_com_started(Timeout) ->
    %% case rct_rpc:call(rpc, os, cmd, ["pgrep com"], 10000)  of
    case rct_rpc:call(rpc, os, cmd, ["pgrep -f -u $USER com/bin/com"], 10000)  of
    	{badrpc,_} -> %{badrpc, timeout | nodedown}
	    timer:sleep(1000),
	    wait_for_com_started(Timeout - 1000);
	[] ->
	    timer:sleep(1000),
	    wait_for_com_started(Timeout - 1000);
	Data ->
	    case re:run(Data, "^[0-9]+$", [{capture,[1],list}]) of
    		{match, _Result} ->
		    End = erlang:timestamp(),
		    %% ct:pal("Data: ~p",[Data]),
		    End;
    		_ ->
		    timer:sleep(1000),
		    wait_for_com_started(Timeout - 1000)
    	    end
    end.

%% wait_for_com_started(Timeout) ->
%%     case rct_rpc:call(rpc, os, cmd, ["pgrep -f -u $USER com/bin/com"], 10000)  of
%%     	{badrpc,_} -> %{badrpc, timeout | nodedown}
%% 	    timer:sleep(1000),
%% 	    wait_for_com_started(Timeout - 1000);
%% 	[] ->
%% 	    timer:sleep(1000),
%% 	    wait_for_com_started(Timeout - 1000);
%% 	Data ->
%% 	    %% ct:pal("Data: ~p",[Data]),
%% 	    NewData = check_for_invalid_data(Data),
%% 	    %% ct:pal("NewData: ~p",[NewData]),
%% 	    case re:run(NewData, "^[0-9]+$", [{capture,[1],list}]) of
%%     		{match, _Result} ->
%% 		    End = erlang:timestamp(),
%% 		    ct:log("NewComPid: ~p",[NewData]),
%% 		    End;
%%     		_ ->
%% 		    timer:sleep(1000),
%% 		    wait_for_com_started(Timeout - 1000)
%%     	    end
%%     end.

%% check_for_invalid_data(Data)->
%%     case re:run(Data, "HZ") of
%% 	{match, _} ->
%% 	    [_H|T] = string:tokens(Data,"\n"),
%% 	    T;
%% 	nomatch ->
%% 	    Data
%%     end.

%% ===========================================================================
%% @doc
%% Check for COM restarted. <br/>
%% Wait for COM process to be started again with a new pid. <br/>
%% @spec wait_for_com_restarted(ComPid) -> timestamp
%% @end
%% ===========================================================================
wait_for_com_restarted(ComPid) ->
    wait_for_com_restarted(ComPid, 90000).

wait_for_com_restarted(_ComPid, Timeout) when Timeout < 500 ->
    ct:fail("COM not restarted within max timeout after restart.");

wait_for_com_restarted(ComPid, Timeout) ->
    case rct_rpc:call(rpc, os, cmd, ["pgrep -f -u $USER com/bin/com"], 10000)  of
    	{badrpc, _} -> %{badrpc, timeout | nodedown}
	    timer:sleep(1000),
	    wait_for_com_restarted(ComPid, Timeout - 1000);
	[] ->
	    timer:sleep(1000),
	    wait_for_com_restarted(ComPid, Timeout - 1000);
	Data ->
    	    case re:run(Data, "^[0-9]+$", [{capture,[1],list}]) of
    		{match, _} ->
		    End = erlang:timestamp(),
		    %% ct:pal("ComPid: ~p",[ComPid]),
		    %% ct:pal("NewComPid: ~p",[Data]),
		    %% ct:pal("End: ~p",[End]),
		    case ComPid == Data of
			true ->
			    %% ct:pal("com has not restarted yet!", []),
			    timer:sleep(1000),
			    wait_for_com_restarted(ComPid, Timeout - 1000);
			false -> %% Com is restarted with new Pid.
			    End
		    end;
		_Rec ->
		    timer:sleep(1000),
		    wait_for_com_restarted(ComPid, Timeout - 1000)
    	    end
    end.

%% wait_for_com_restarted(ComPid, Timeout) ->
%%     case rct_rpc:call(rpc, os, cmd, ["pgrep -f -u $USER com/bin/com"], 10000)  of
%%     	{badrpc, _} -> %{badrpc, timeout | nodedown}
%% 	    timer:sleep(1000),
%% 	    wait_for_com_restarted(ComPid, Timeout - 1000);
%% 	[] ->
%% 	    timer:sleep(1000),
%% 	    wait_for_com_restarted(ComPid, Timeout - 1000);
%% 	Data ->
%% 	    %% ct:pal("Data: ~p",[Data]),
%% 	    NewData = check_for_invalid_data(Data),
%% 	    %% ct:pal("NewData: ~p",[NewData]),
%%     	    case re:run(NewData, "^[0-9]+$", [{capture,[1],list}]) of
%%     		{match, _} ->
%% 		    End = erlang:timestamp(),
%% 		    %% ct:pal("ComPid: ~p",[ComPid]),
%% 		    %% ct:pal("NewComPid: ~p",[NewData]),
%% 		    %% ct:pal("End: ~p",[End]),
%% 		    case ComPid == NewData of
%% 			true ->
%% 			    ct:log("com has not restarted yet, OrgPid: ~p , NewPid: ~p!", [ComPid, NewData]),
%% 			    timer:sleep(1000),
%% 			    wait_for_com_restarted(ComPid, Timeout - 1000);
%% 			false -> %% Com is restarted with new Pid.
%% 			    ct:log("NewComPid: ~p",[NewData]),
%% 			    End
%% 		    end;
%% 		nomatch ->
%% 		    %% ct:log("ComPid: ~p",[ComPid]),
%% 		    %% ct:log("NewComPid: ~p",[NewData]),
%% 		    timer:sleep(1000),
%% 		    wait_for_com_restarted(ComPid, Timeout - 1000)
%%     	    end
%%     end.

%% ===========================================================================
%% @doc
%% Check for Netconf to be started. <br/>
%% Wait for ct_netconfc:get_config... returns  ok. <br/>
%% @spec wait_for_netconf_started() -> timestamp
%% @end
%% ===========================================================================
wait_for_netconf_started() ->
    wait_for_netconf_started(60000).

wait_for_netconf_started(Timeout) when Timeout < 500 ->
    ct:fail("Netconf not started within max timeout after restart.");

wait_for_netconf_started(Timeout) ->
    case ct_netconfc:open(nc1,[]) of
	{ok,_} ->
	    {ok,_} = ct_netconfc:get_config(nc1,running,{'ManagedElement',
							 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
							 [{managedElementId,[],["1"]}]}),
	    End = erlang:timestamp(),
	    End;
	_  ->
	    timer:sleep(250),
	    wait_for_netconf_started(Timeout - 250)
    end.


%% ===========================================================================
%% @doc
%% Check for Application to be started. <br/>
%% Wait for test_app is started. <br/>
%% @spec wait_for_appl_started() -> timestamp
%% @end
%% ===========================================================================
wait_for_appl_started() ->
    wait_for_appl_started(60000).

wait_for_appl_started(Timeout) when Timeout < 500 ->
    ct:fail("No Appl started within max timeout after restart.");

wait_for_appl_started(Timeout) ->
    case rct_rpc:call(rpc, appmServer, get_apps, [], 10000) of
    	{badrpc, _} ->
	    timer:sleep(1000),
	    wait_for_appl_started(Timeout - 1000);
	AppProplist ->
    	    case proplists:lookup("test_app", AppProplist) of
    		{"test_app", _} ->
		    End = erlang:timestamp(),
		    End;
    		_ ->
		    timer:sleep(1000),
		    wait_for_appl_started(Timeout - 1000)
    	    end
    end.


%% ===========================================================================
%% @doc
%% Check used RAM memory size. <br/>
%% Get memory size from, os:cmd "free" and "ps -eo fname,vsz,rss | egrep '(beam)|(com)'". <br/>
%% @spec get_used_memsize() -> int
%% @end
%% ===========================================================================
get_used_memsize() ->
    ct:pal("### Get Used MemSize",[]),
    Free = rct_rpc:call(rpc, os, cmd, ["free"], 10000, noprint),
    Free1 = string:tokens(Free,"\n"),
    ct:log("Free: ~p. ",[Free1]),
    [MemUsed] = [A||A<-Free1, string:str(A,"Mem:") == 1],
    ct:log("MemUsed: ~p. ",[MemUsed]),
    [BuffersCache] = [S||S<-Free1, string:str(S,"-/+ buffers/cache:") == 1],
    ct:log("BuffersCache: ~p. ",[BuffersCache]),

    %%%%
    %% Total used RAM size
    %%%%
    Tot = list_to_integer(lists:nth(3,string:tokens(MemUsed," "))),
    Tot_bc = list_to_integer(lists:nth(3,string:tokens(BuffersCache," "))),

    PS = rct_rpc:call(rpc, os, cmd, ["ps -eo fname,vsz,rss | egrep '(beam)|(com)'"], 10000, noprint),
    %%%%
    %% Get used RAM size beam uses.
    %%%%
    %% BeamPid = get_valid_beam_pid(),
    BeamPid = rct_rpc:call(rpc, os, getpid, [], 10000, noprint),
    ct:log("BeamPid: ~p. ",[BeamPid]),
    Beam_Rss_Data = rct_rpc:call(rpc, os, cmd, ["ps -o fname,rss -p "++BeamPid], 10000, noprint),
    ct:log("Beam_Rss_Data: ~p. ",[Beam_Rss_Data]),
    BeamRssData = string:tokens(Beam_Rss_Data," \n"),
    ct:log("BeamRssData: ~p. ",[BeamRssData]),
    Beam_rss = lists:last(BeamRssData),
    ct:log("Beam_rss: ~p. ",[Beam_rss]),

    %%%%
    %% Get used RAM size com uses.
    %%%%
    {match,[_Com_vsz,Com_rss]} = re:run(PS,"com *([0-9]+) *([0-9]+).*",[{capture,[1,2],list}]),
    ct:log("_Com_vsz: ~p. Com_rss: ~p",[_Com_vsz,Com_rss]),
    %% [list_to_integer(Tot), list_to_integer(Tot_bc), list_to_integer(Beam_rss), list_to_integer(Com_rss)].
    [integer_to_list(Tot), integer_to_list(Tot_bc), Beam_rss, Com_rss].


%% get_valid_beam_pid() ->
%%     get_valid_beam_pid(30000).
%% get_valid_beam_pid(Timeout) when Timeout < 500 ->
%%     ct:fail("No correct beam found within expected time");
%% get_valid_beam_pid(Timeout) ->
%%     BeamPid = rct_rpc:call(rpc, os, getpid, [], 10000, noprint),
%%     case re:run(BeamPid, "HZ") of
%% 	{match, _} ->
%% 	    timer:sleep(1000),
%% 	    get_valid_beam_pid(Timeout-1000);
%% 	nomatch ->
%% 	    BeamPid
%%     end.

%% ===========================================================================
%% @doc
%% Get disc usage of directories. <br/>
%% SW: /software/ <br/>
%% RCS: /software/RCS_CXP9021221*
%% COM: /software/RCS_CXP9021221*/COM_CXC1733991*/
%% @spec get_disc_usage() -> string
%% @end
%% ===========================================================================
get_disc_usage() ->
    %% SW_du = rct_rpc:call(rpc, os, cmd, ["du -sh /software/"], 10000, noprint),
    %% %% get disc usage size.
    %% [SW_DISC_USAGE|_] = string:tokens(SW_du, "M\t"),
    SW_DISC_USAGE = "-",
    BoardType = proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))),
    ct:log("BoardType: ~p", [BoardType]),
    RCS_CXP = get_rcs_cxp(BoardType),
    ct:log("RCS_CXP: ~p", [RCS_CXP]),
    COM_CXC = get_rcs_cxc(BoardType),
    ct:log("COM_CXC: ~p", [COM_CXC]),
    RCS_du = rct_rpc:call(rpc, os, cmd, ["du -sh /software/"++RCS_CXP], 10000, noprint),
    [RCS_DISC_USAGE|_] = string:tokens(RCS_du, "M\t"),
    COM_du = rct_rpc:call(rpc, os, cmd, ["du -sh /software/"++RCS_CXP++"/"++COM_CXC++"/"], 10000, noprint),
    [COM_DISC_USAGE|_] = string:tokens(COM_du, "M\t "),
    [SW_DISC_USAGE, RCS_DISC_USAGE , COM_DISC_USAGE].

get_rcs_cxp(BoardType) ->
    RCS_CXP = case BoardType of
		  BoardType when BoardType == "dus4101";
				 BoardType == "duw4101" ->
		      "RCS_CXP9021221*";
		  _ ->
		      "RCS-ARM_CXP9021221*" % tcu03, dus5201, dus3201?
	      end,
    RCS_CXP.

get_rcs_cxc(BoardType) ->
    COM_CXC = case BoardType of
		  BoardType when BoardType == "dus4101";
				 BoardType == "duw4101" ->
		      "COM_CXC1733991*";
		  _ ->
		      "COM3_CXC1733991*" % tcu03, dus5201, dus3201?
	      end,
    COM_CXC.

%% ===========================================================================
%% @doc
%% Get CXS label. <br/>
%% Get SW version using COM cli interface. <br/>
%% @spec get_sw_version() -> 'CXS_LABEL'
%% @end
%% ===========================================================================
get_sw_version() ->
    ct:pal("### Get SW version",[]),
    ok = rct_cli:connect(cli),
    {ok ,RecievedData} = rct_cli:send(cli,"show ManagedElement=1,SystemFunctions=1,SwInventory=1"),
    %% ct:pal("RecievedData: ~p", [RecievedData]),

    %% Clean upp RecievedData string to a list of strings.
    Var = string:tokens(RecievedData, "=\r\n "),
    %% drop data until you reach "SwVersion", the wanted CXS label is the second element.
    [_, CXS | _ ] = lists:dropwhile(fun(X) ->
					    X =/= "SwVersion"
				    end, Var),
    ct:pal("CXS: ~p", [CXS]),
    ok = rct_cli:disconnect(cli),

    list_to_atom(CXS).


%% ===========================================================================
%% @hidden
%% ===========================================================================
get_filename(ReastartCause_info) ->
    RecievedData = rct_rpc:call(rpc, os, cmd, ["cat /sys/rbs-fn/rbs-sys/board_type"], 10000, noprint),
    %% Clean upp HW_type string to a list of HW_type.
    [HW_type] = string:tokens(RecievedData, "\n "),
    ct:pal("### HW type: ~p",[HW_type]),
    FileName = HW_type ++"_"++ ReastartCause_info,
    FileName.

%% @hidden
%% ===========================================================================
%% @doc
%% For each testcase to use for writing testcase results and/or store measurement results <br/>
%% into a common result file. <br/>
%% @spec updateMeasResFile(FileName, PrintOpts, MeasData) -> ok
%% @end
%% ===========================================================================
updateMeasResFile(FileName, PrintOpts, MeasData) ->
    [{_, NodeName}] = ct:get_config(test_nodes),
    MeasInfo = MeasData++[NodeName],
    ct:pal("MeasInfo: ~p",[MeasInfo]),
    %% insert a ;~w before ~n due to the field NodeName.
    CompletePrintOpts = re:replace(PrintOpts, "~n", ";~w~n", [{return, list}]),
    rct_tlib:
	writeDataToFile(?RESULTDIR, FileName, CompletePrintOpts, MeasInfo),
    ok.
