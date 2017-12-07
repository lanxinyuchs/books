%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rbs_meas_restart_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/R4A/3
%%% 
%%% @doc ==Measure times for differents restarts causes, on UP build by node_ci.==
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


-module(rbs_meas_restart_SUITE).
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
%%% R2A/2      2013-05-30 etxivri     Created
%%% R2A/3      2013-08-13 etxkols     Changed raw to no_prompt_check in ct_telnet:expect
%%% R2A/4      2014-03-07 etxivri     Update to add Branch file name when 
%%%                                   other than Branch R2A is used.
%%% R3A/1      2014-12-01 etxivri     Use SwInventory when get sw version.
%%% R4A/1      2015-09-30 etxmlar     now() depricated in OTP 18 changed to os:timestamp() 
%%% R4A/2      2015-10-23 etxivri     removed use of init restart.
%%% R4A/3      2016-02-19 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
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
	 wait_for_mw_start/1,
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

-define(RESULTDIR, "/proj/rcs/measurements/rbs").
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
     measure_power_mw_login_com_appl_netconf_time_memsize
     %% measure_initial_restart_com_appl_netconf_time_memsize
    ].

%%--------------------------------------------------------------------
%% @doc
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{group_measure_restart_1,[],[measure_reboot_mw_login_com_appl_netconf_time_memsize,
				  measure_power_mw_login_com_appl_netconf_time_memsize
				  %% measure_initial_restart_com_appl_netconf_time_memsize
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

    NrOfImmObj = length(rct_rpc:call(rpc, ets, tab2list, [imm_objects], 10000)),
    ct:pal("Nr of IMM Objects: ~p",[NrOfImmObj]),

    Applications = rct_rpc:call(rpc, appmServer, get_apps, [], 10000),
    ct:pal("Applications: ~p",[Applications]),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),

    %%%%
    %% Measure time for starting MW
    %% If failing to match the search string for a numbers of retires,
    %% then measure from login.
    %%%%
    case wait_for_mw_start(reboot) of
	{Start1, End1} -> 
	    {Start1, End1};
	error_to_may_retries ->
	    %% Skip measure time for start MW
	    ok = ct:pal("### reboot!",[]),
	    ok = rct_rs232:login(console),
	    Start1 = os:timestamp(),
	    ok = ct_telnet:send(console, "reboot"),
	    {Start1, End1 = dummy}
    end,

    case End1 of
	dummy -> 
	    StartMwTime = '-';
	_ ->
	    StartMwTime = trunc(timer:now_diff(End1, Start1) / 1000 / 1000)
    end,

    %%%%
    %% Measure time for login prompt.
    %%%%
    End2 = wait_for_login_prompt(),
    StartLogInTime = trunc(timer:now_diff(End2, Start1) / 1000 / 1000),

    %%%%
    %% Measure time for COM to start
    %%%%
    {End3, ComPid} = wait_for_com_started(),
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
    FileName = get_filename("rbs_measure_reboot_time_memsize.txt"),
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

    %%%%
    %% Check com has not reatarted
    %%%%
    ok = check_that_com_has_not_restarted(ComPid),
    %%%%
    %% Check that IMM objects does not increase.
    %%%%
    ok = check_nr_of_imm_obj_not_increased(NrOfImmObj),

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

    NrOfImmObj = length(rct_rpc:call(rpc, ets, tab2list, [imm_objects], 10000)),
    ct:pal("Nr of IMM Objects: ~p",[NrOfImmObj]),

    Applications = rct_rpc:call(rpc, appmServer, get_apps, [], 10000),
    ct:pal("Applications: ~p",[Applications]),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),

    %%%%
    %% Measure time for starting MW.
    %% If failing to match the search string for a numbers of retires,
    %% then measure from login.
    %%%%
    case wait_for_mw_start(power) of
	{Start1, End1} -> 
	    {Start1, End1};
	error_to_may_retries -> %% Skip measure time for MW"
	    ok = ct:pal("### power off/on!",[]),
	    ok = rct_power:off(node),
	    Start1 = wait_for_power_on(),
	    {Start1, End1 = dummy}
    end,

    case End1 of
	dummy -> 
	    StartMwTime = '-';
	_ ->
	    StartMwTime = trunc(timer:now_diff(End1, Start1) / 1000 / 1000)
    end,

    %%%%
    %% Measure time for login prompt.
    %%%%
    End2 = wait_for_login_prompt(),
    StartLogInTime = trunc(timer:now_diff(End2, Start1) / 1000 / 1000),

    %%%%
    %% Measure time for COM to start
    %%%%
    {End3, ComPid} = wait_for_com_started(),
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
    FileName = get_filename("rbs_measure_power_time_memsize.txt"),
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

    %%%%
    %% Check com has not reatarted
    %%%%
    ok = check_that_com_has_not_restarted(ComPid),
    %%%%
    %% Check that IMM objects does not increase.
    %%%%
    ok = check_nr_of_imm_obj_not_increased(NrOfImmObj),

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
    Com = rct_rpc:call(rpc, os, cmd, ["pgrep com -u $USER"], 10000),
    ComPid = case re:run(Com, "^[0-9]+$", [{capture,[1],list}]) of
		 {match, _Result} ->
		     Com;
		 _ ->
		     ct:fail("No com pid found")
    end,

    NrOfImmObj = length(rct_rpc:call(rpc, ets, tab2list, [imm_objects], 10000)),
    ct:pal("Nr of IMM Objects: ~p",[NrOfImmObj]),

    Applications = rct_rpc:call(rpc, appmServer, get_apps, [], 10000),
    ct:pal("Applications: ~p",[Applications]),

    %%%%
    %% Perform a initial restart.
    %%%%
    Start1 = os:timestamp(),
    rct_rpc:call(rpc, init, restart, [], 10000),

    %%%%
    %% Check that com is restarted with a new pid.
    %%%%
    {End3, NewComPid} = wait_for_com_restarted(ComPid),  
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
    FileName = get_filename("rbs_measure_initial_restart_time_memsize.txt"),
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

    ct:pal("Memsize: Com_rss: ~s,  Beam_rss: ~s,  Tot_bc: ~s ~n", 
    	   [Com_rss,
    	    Beam_rss,
    	    Tot_bc]),

    ct:pal("Disc Usage: SW: ~s,  RCS: ~s,  Com: ~s ~n", 
    	   [SW_DiscUsage, 
	    RCS_DiscUsage , 
	    COM_DiscUsage]),

    %%%%
    %% Evaluate times
    %%%%
    evaluate_times_after_init_restart(StartComTime, DiffStarts),

    %%%%
    %% Check com has not reatarted
    %%%%
    ok = check_that_com_has_not_restarted(NewComPid),
    %%%%
    %% Check that IMM objects does not increase.
    %%%%
    ok = check_nr_of_imm_obj_not_increased(NrOfImmObj),

    ok.
   

%% ===========================================================================
%% @hidden
%% ===========================================================================
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
evaluate_times_after_init_restart(RestartTime, _TimeDiff) when RestartTime > 60 ->
    ct:pal("TC fail after init restart, Com restart Time: ~p sec > limit: 60 sec \n",[RestartTime]),
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

wait_for_power_on(Timeout) when Timeout < 500 ->
    ct:fail("Power ON failed within expected time.");

wait_for_power_on(Timeout) ->
    case  rct_power:on(node, no_retries) of
    	ok -> 
    	    Start = os:timestamp(),
	    %% Check that node has restarted. If no try to restart again
	    case ct_telnet:expect(console, "2:nd Stage Boot Loader", [{timeout,20000}, no_prompt_check]) of
	       	{ok, _} ->
		    Start;
		{error, timeout} ->
		    ct:pal("Power on failed within expected time, do power off/on again.",[]),
		    ok = rct_power:off(node),
		    wait_for_power_on(Timeout - 10000);		
		Res ->
		    ct:pal("### Res: ~w", [Res]),
		    ct:fail("Power ON failed, du to unexpected return value from power on.")
	    end;
	{error,econnrefused} ->
	    timer:sleep(250), % If occupied then wait and try again.
	    wait_for_power_on(Timeout - 250);
	 Return ->
	    ct:pal("### Power ON failed, due to: ~w", [Return]),
	    ct:fail("Power ON failed.")
    end.


%% ===========================================================================
%% @doc
%% Check for MW to be started. <br/>
%% Wait for a specific text string "Starting RCS" in the consol printouts. <br/>
%% There will be N:ths retries to get the search string.<br/>
%% Cmd = reboot | power
%% @spec wait_for_mw_start(Cmd) -> {Start, End} | ct_fail
%% @end
%% ===========================================================================
wait_for_mw_start(Cmd) ->
    wait_for_mw_start(Cmd, 1).

%% wait_for_mw_start( _Cmd, N) when N > 2 -> %% N:th tries
wait_for_mw_start( _Cmd, N) when N >= 1 -> %% Starting RCS does not printout in console any longer !
    error_to_may_retries;

wait_for_mw_start(Cmd, N) ->
    case Cmd of
    	reboot ->
	    ok = rct_rs232:login(console),
    	    ct:pal("### reboot!",[]),
    	    Start = os:timestamp(),
    	    ok = ct_telnet:send(console, "reboot"),
    	    Start;
    	power ->
    	    ct:pal("### power off/on!",[]),
    	    ok = rct_power:off(node),
    	    Start = wait_for_power_on(),
    	    Start
    end,

    %% Sometimes consolprintouts with other info in "Starting RCS".
    %% case ct_telnet:expect(console, " RCS", [{timeout,50000}, no_prompt_check]) of
    case ct_telnet:expect(console, "Starting RCS", [{timeout,50000}, no_prompt_check]) of
	{ok, _} -> 
	    End = os:timestamp(),
	    {Start,End};
	_  ->
	    ct:pal("Could not match \"Starting RCS\", Do the test from the begining: Nr: ~p. ", [N+1]),
	    wait_for_mw_start(Cmd, N+1)
    end.



%% ===========================================================================
%% @doc
%% Check for login prompt. <br/>
%% Wait for du1 login prompt arrives in the consol printouts. <br/>
%% @spec wait_for_login_prompt() -> timestamp
%% @end
%% ===========================================================================
wait_for_login_prompt() ->
    wait_for_login_prompt(120000).

wait_for_login_prompt(Timeout) when Timeout < 500 ->
    ct:fail("No login prompt within max timeout after restart.");

wait_for_login_prompt(Timeout) ->
    case ct_telnet:expect(console, "du1 login", [{timeout,250}, no_prompt_check]) of
	{ok, _} -> 
	    End = os:timestamp(),
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
    ct:fail("COM not started within max timeout after restart.");

wait_for_com_started(Timeout) ->
    case rct_rpc:call(rpc, os, cmd, ["pgrep com -u $USER"], 10000)  of
    	{badrpc,_} -> %{badrpc, timeout | nodedown}
	    timer:sleep(200),
	    wait_for_com_started(Timeout - 200);
	[] ->	    
	    timer:sleep(200),
	    wait_for_com_started(Timeout - 250);
	Data ->
	    ct:pal("### ComPid: ~p",[Data]),
    	    case re:run(Data, "^[0-9]+$", [{capture,[1],list}]) of
    		{match, _Result} ->
		    End = os:timestamp(),
		    %% ct:pal("Data: ~p",[Data]),
		    {End, Data};
    		_ ->
		    timer:sleep(200),
		    wait_for_com_started(Timeout - 250)
    	    end
    end.

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
    case rct_rpc:call(rpc, os, cmd, ["pgrep com -u $USER"], 10000)  of
    	{badrpc, _} -> %{badrpc, timeout | nodedown}
	    timer:sleep(200),
	    wait_for_com_restarted(ComPid, Timeout - 200);
	[] ->	
	    timer:sleep(200),
	    wait_for_com_restarted(ComPid, Timeout - 250);
	Data ->
	    %% ct:pal("### ComPid: ~p, org ComPid: ~p ",[Data,ComPid]),
    	    case re:run(Data, "^[0-9]+$", [{capture,[1],list}]) of
    		{match, _} ->
		    End = os:timestamp(),
		    %% ct:pal("ComPid: ~p",[ComPid]),
		    %% ct:pal("NewComPid: ~p",[Data]),
		    %% ct:pal("End: ~p",[End]),
		    case ComPid == Data of
			true ->
			    %% ct:pal("com has not restarted yet!", []),
			    timer:sleep(200),
			    wait_for_com_restarted(ComPid, Timeout - 250);
			false -> %% Com is restarted with new Pid.
			    {End, Data}
		    end;
		_Rec ->
		    timer:sleep(200),
		    wait_for_com_restarted(ComPid, Timeout - 250)
    	    end
    end.

%% ===========================================================================
%% @doc
%% @spec check_that_com_has_not_restarted(ComPid) -> timestamp
%% @end
%% ===========================================================================
check_that_com_has_not_restarted(ComPid) ->
    ComPid1 = rct_rpc:call(rpc, os, cmd, ["pgrep com -u $USER"], 10000),
    case ComPid == ComPid1 of
    	true ->
	    ct:pal("Com has not restarted unexspected:\n Start COM: ~p, End COM: ~p",[ComPid, ComPid1]),
    	    ok;
    	false -> %% Com is restarted with new Pid.
    	    ct:pal("Start COM: ~p, End COM: ~p",[ComPid, ComPid1]),
    	    ct:fail("COM has been restarterted unexpected!")
    end,
    ok.

%% ===========================================================================
%% @doc
%% @spec check_nr_of_imm_obj_not_increased(NrOfImmObj) -> timestamp
%% @end
%% ===========================================================================
check_nr_of_imm_obj_not_increased(NrOfImmObj) ->
    End_NrOfImmObj = length(rct_rpc:call(rpc, ets, tab2list, [imm_objects], 10000)),
    case NrOfImmObj of
    	End_NrOfImmObj ->
	    %% ct:pal("Nr of IMM objects is expected:\n Start : ~p, End : ~p",[NrOfImmObj, End_NrOfImmObj]),
    	    ok;
    	_ ->
	    ct:pal("Nr of IMM objects is NOT expected:\n Start : ~p, End : ~p",[NrOfImmObj, End_NrOfImmObj]),
    	    ct:fail(" Nr of IMM objects is NOT expected!")
    end,
    ok.

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
	    						 [{managedElementId,[],["1"]}
							 ]}),

	    End = os:timestamp(),
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
    wait_for_appl_started(90000).

wait_for_appl_started(Timeout) when Timeout < 500 ->
    ct:fail("No Appl started within max timeout after restart.");

wait_for_appl_started(Timeout) ->
    case rct_rpc:call(rpc, appmServer, get_apps, [], 10000) of
    	{badrpc, _} ->
	    timer:sleep(250),
	    wait_for_appl_started(Timeout - 250);
	[] ->
	    timer:sleep(250),
	    wait_for_appl_started(Timeout - 250);
	_AppProplist ->
	    End = os:timestamp(),
	    End
	%% case proplists:lookup("eqmh", AppProplist) of
    	    %% 	{"eqmh", _} ->
	    %% 	    End = os:timestamp(),
	    %% 	    End;
    	    %% 	_ ->
	    %% 	    timer:sleep(250),
	    %% 	    wait_for_appl_started(Timeout - 250)
    	    %% end
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
    {match,[_,Tot]} = re:run(Free,"Mem: *([0-9]+) *([0-9]+).*",[{capture,[1,2],list}]),

    %%%%
    %% Total used RAM size
    %%%%
    {match,[Tot_bc]} = re:run(Free,"buffers/cache: *([0-9]+).*",[{capture,[1],list}]),

    PS = rct_rpc:call(rpc, os, cmd, ["ps -eo fname,vsz,rss | egrep '(beam)|(com)'"], 10000, noprint),

    %%%%
    %% Get used RAM size beam uses. 
    %%%%
    %% {match,[_Beam_vsz,Beam_rss]} = re:run(PS,"beam.smp *([0-9]+) *([0-9]+).*",[{capture,[1,2],list}]),
    %% CorrectBeam = rct_rpc:call(rpc, os, cmd, ["pgrep -fl beam | grep -v testnode "], 10000, noprint),
    CorrectBeam = rct_rpc:call(rpc, os, getpid, [], 10000),
    [BeamPid | _T] = string:tokens(CorrectBeam," \n"),
    Beam_Rss_Data = rct_rpc:call(rpc, os, cmd, ["ps -o fname,rss -p "++BeamPid], 10000, noprint),
    ["COMMAND","RSS","beam.smp", Beam_rss] = string:tokens(Beam_Rss_Data," \n"),

    %%%%
    %% Get used RAM size com uses. 
    %%%%
    {match,[_Com_vsz,Com_rss]} = re:run(PS,"com *([0-9]+) *([0-9]+).*",[{capture,[1,2],list}]),

    %% [list_to_integer(Tot), list_to_integer(Tot_bc), list_to_integer(Beam_rss), list_to_integer(Com_rss)].
    [Tot, Tot_bc, Beam_rss, Com_rss].


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
    SW_du = rct_rpc:call(rpc, os, cmd, ["du -sh /software/"], 10000, noprint), 
    %% get disc usage size.
    [SW_DISC_USAGE|_] = string:tokens(SW_du, "M\t"),

    RCS_du = rct_rpc:call(rpc, os, cmd, ["du -sh /software/RCS_CXP9021221*"], 10000, noprint), 
    %% get disc usage size.
    [RCS_DISC_USAGE|_] = string:tokens(RCS_du, "M\t"),

    COM_du = rct_rpc:call(rpc, os, cmd, ["du -sh /software/RCS_CXP9021221*/COM_CXC1733991*/"], 10000, noprint), 
    %% get disc usage size.
    [COM_DISC_USAGE|_] = string:tokens(COM_du, "M\t "),

    [SW_DISC_USAGE, RCS_DISC_USAGE , COM_DISC_USAGE].


%% ===========================================================================
%% @doc
%% Get CXS label. <br/>
%% Get SW version using COM cli interface. <br/>
%% @spec get_sw_version() -> 'CXS_LABEL'
%% @end
%% ===========================================================================
get_sw_version() ->
    CXS_label = rct_tlib:get_sw_version(cli),
    CXS_label.

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
