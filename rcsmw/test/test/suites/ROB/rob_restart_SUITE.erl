%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rob_restart_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/R4A/2
%%% 
%%% @doc ==Check that repeat of differnt types of restarts does not affect startup.==
%%% This Test Suite can be used on target enviroment.
%%%
%%% <br/><br/>
%%% 
%%% @end

-module(rob_restart_SUITE).
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
%%% R2A/2      2013-06-03 etxivri     Created
%%% R2A/3      2013-08-13 etxkols     Changed raw to no_prompt_check in ct_telnet:expect
%%% R2A/4      2013-10-09 etxivri     Updates to make it more predictable, due to sometimes
%%%                                   some imm_objects is missing. 
%%% R2A/5      2013-10-18 etxivri     Updates for TCU.
%%% R2A/6      2014-01-16 etxkols     Support for dus5201. 
%%% R2A/7      2014-06-04 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:"
%%% R3A/1      2014-12-01 etxivri     Use SwInventory when get sw version.
%%% R4A/1      2015-09-30 etxmlar     now() depricated in OTP 18 changed to os:timestamp() 
%%% R4A/2      2016-02-19 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
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
	 wait_for_login_prompt/1,
	 %% wait_for_com_started/1,
	 wait_for_netconf_started/1,
	 wait_for_appl_started/1,
	 get_sw_version/0,
	 groups/0,
	 all/0,
	 init_restart_10_times/1,
	 init_restart_100_times/1,
	 reboot_10_times/1,
	 reboot_100_times/1,
	 power_off_on_10_times/1,
	 power_off_on_100_times/1
	]).

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
		 %% {cth_conn_log, []},
		 {rct_consserv,cs1},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
                 {rct_rs232,console},
		 {rct_cli, {cli, [manual_connect]}},
                 {rct_core,[]}
		]}].


%% @hidden
init_per_suite(Config) ->
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
end_per_testcase(_TestCase, Config) ->
    case proplists:get_value(tc_status, Config) of
   	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p.  \nClean up added rbs units", [Reason]),
	    
	    Top = rct_rpc:call(rpc, os, cmd, ["top -b -n 1"], 10000, noprint),	    
	    TOP = string:tokens(Top, "\n"),
	    ct:pal("~p",[TOP]),
	    
	    BeamPid = rct_rpc:call(rpc, os, getpid, [], 10000),
	    ct:pal("Beam: ~p",[BeamPid]),
	    
	    ComPid = rct_rpc:call(rpc, os, cmd, ["pgrep com -u $USER"], 10000),
	    ct:pal("Com: ~p",[ComPid]),

	    NrOfImmObj = length(rct_rpc:call(rpc, ets, tab2list, [imm_objects], 10000)),
	    ct:pal("Nr of IMM Objects: ~p",[NrOfImmObj])

    end,
    
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
    ].

%%--------------------------------------------------------------------
%% @doc
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @doc
%% @spec init_restart_10_times(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
init_restart_10_times(_Config) ->
    restart(init_restart, 10).
%%--------------------------------------------------------------------
%% @doc
%% @spec init_restart_100_times(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
init_restart_100_times(_Config) ->
    restart(init_restart, 100).


%%--------------------------------------------------------------------
%% @doc
%% @spec reboot_10_times(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
reboot_10_times(_Config) ->
    restart(reboot, 10).
%%--------------------------------------------------------------------
%% @doc
%% @spec reboot_100_times(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
reboot_100_times(_Config) ->
    restart(reboot, 100).


%%--------------------------------------------------------------------
%% @doc
%% @spec power_off_on_10_times(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
power_off_on_10_times(_Config) ->
    restart(power, 10).
%%--------------------------------------------------------------------
%% @doc
%% @spec power_off_on_100_times(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
power_off_on_100_times(_Config) ->
    restart(power, 100).


%%--------------------------------------------------------------------
%% @doc 
%% @spec restart(RestartCause, Nr) -> ok
%% @end
%%--------------------------------------------------------------------
restart(RestartCause, Nr) ->
    Top = rct_rpc:call(rpc, os, cmd, ["top -b -n 1"], 10000, noprint),	    
    TOP = string:tokens(Top, "\n"),
    ct:pal("~p",[TOP]),
    do_restart_loop(RestartCause, Nr, 1).

%%--------------------------------------------------------------------
%% @doc 
%% @spec do_restart_loop(RestartCause, Nr, Counter) -> ok
%% @end
%%--------------------------------------------------------------------
do_restart_loop(RestartCause, 0, Counter)->
    ct:pal("### TC is done, RestartCause: ~p, Nr of times: ~p.",[RestartCause, Counter-1]),
    ok;
do_restart_loop(RestartCause, Nr, Counter)->
    ct:pal("### Start loop Nr: ~p",[Counter]),
    ok = do_restart(RestartCause),
    ct:pal("### End loop Nr: ~p",[Counter]),
    do_restart_loop(RestartCause, Nr-1, Counter+1).


%%--------------------------------------------------------------------
%% @doc 
%% This will do the restart depending on which RestartCause is used. <br/>
%% @spec do_restart(RestartCause) -> ok
%% @end
%%--------------------------------------------------------------------
do_restart(RestartCause) ->
    %%%%
    %% Get Beam pid.
    %%%%
    BeamPid = rct_rpc:call(rpc, os, getpid, [], 10000, noprint),
    ct:pal("Beam: ~p",[BeamPid]),
    %%%%
    %% Get COM pid.
    %%%%
    ComPid = rct_rpc:call(rpc, os, cmd, ["pgrep com -u $USER"], 10000, noprint),
    ct:pal("Com: ~p",[ComPid]),

    wait_for_appl_started(),
    wait_for_netconf_started(),
    %%%%
    %% Nr of IMM objects.
    %%%%
    ImmObj = rct_rpc:call(rpc, ets, tab2list, [imm_objects], 10000, noprint),
    %% NrOfImmObj = length(rct_rpc:call(rpc, ets, tab2list, [imm_objects], 10000)),
    NrOfImmObj = length(ImmObj),
    ct:pal("Nr of IMM Objects: ~p",[NrOfImmObj]),

    Start1 = case RestartCause of
		 init_restart ->
		     ct:pal("### init_restart!",[]),
		     Start = os:timestamp(),
		     rct_rpc:call(rpc, init, restart, [], 10000),
		     Start;
		 reboot  ->
		     {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
		     net_kernel:disconnect(ErlNode),
		     ct:pal("### reboot!",[]),
		     ok = rct_rs232:login(console),
		     Start = os:timestamp(),
		     ok = ct_telnet:send(console, "reboot"),
		     wait_for_login_prompt(),
		     Start;
		 power ->
		     {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
		     net_kernel:disconnect(ErlNode),
		     ct:pal("### power off / on!",[]),
		     ok = rct_power:off(node),
		     Start = wait_for_power_on(),
		     wait_for_login_prompt(),
		     Start
	     end,

    %%%%
    %% Check that com is restarted with a new pid.
    %%%%
    {End3, NewComPid} = case RestartCause of
			    init_restart ->
				wait_for_com_restarted(ComPid);
			    reboot  ->
				wait_for_com_started();
			    power ->
				wait_for_com_started()
			end,
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
    StartNetConfTime = trunc(timer:now_diff(End5, Start1) / 1000 / 1000),

    %%%%
    %% Restart Time
    %%%%
    %% RestartTime = trunc(timer:now_diff(End5, Start1) / 1000 / 1000),

    ct:pal("StartUp times: Com: ~p, Appl: ~p,  Netconf: ~p,~n", 
    	   [StartComTime,
    	    StartApplTime, 
    	    StartNetConfTime]),

    %%%%
    %% Get sw version, using cli
    %%%%
    CXS_label = get_sw_version(),
    ct:pal("CXS_label: ~p", [CXS_label]), 

    %%%%
    %% Check com has not reatarted
    %%%%
    ok = check_that_com_has_not_restarted(NewComPid),
    %%%%
    %% Check that IMM objects does not increase.
    %%%%
    ok = check_nr_of_imm_obj_not_increased(NrOfImmObj),

    ok.


%% Internal
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
	    BoardType = proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))),
	    RestartStr = case BoardType of
			     BoardType when BoardType == "dus4101";
					    BoardType == "duw4101" ->
				 "2:nd Stage Boot Loader";
			     _ ->
				 "Ericsson Version: 2/CXC1736593/" % tcu03, dus5201, dus3201?
			 end,
	    ct:log("RestartStr: ~p", [RestartStr]),
	    ok = check_node_is_starting(RestartStr, Timeout),
	    Start;
	{error,econnrefused} ->
	    timer:sleep(250), % If occupied then wait and try again.
	    wait_for_power_on(Timeout - 250);
	Return ->
	    ct:pal("### Power ON failed, due to: ~w", [Return]),
	    ct:fail("Power ON failed.")
    end.

check_node_is_starting(RestartStr, Timeout) ->
    case ct_telnet:expect(console, RestartStr, [{timeout,20000}, no_prompt_check]) of
	{ok, _} ->
	    ok;
	{error, timeout} ->
	    ct:pal("Power on failed within expected time, do power off/on again.",[]),
	    ok = rct_power:off(node),
	    wait_for_power_on(Timeout - 10000);		
	Res ->
	    ct:pal("### Res: ~w", [Res]),
	    ct:fail("Power ON failed, du to unexpected return value from power on.")
    end.

%% wait_for_power_on(Timeout) ->
%%     case  rct_power:on(node, no_retries) of
%%     	ok -> 
%%     	    Start = os:timestamp(),
%% 	    %% Check that node has restarted. If no try to restart again
%% 	    case ct_telnet:expect(console, "2:nd Stage Boot Loader", [{timeout,20000}, no_prompt_check]) of
%% 	       	{ok, _} ->
%% 		    Start;
%% 		{error, timeout} ->
%% 		    ct:pal("Power on failed within expected time, do power off/on again.",[]),
%% 		    ok = rct_power:off(node),
%% 		    wait_for_power_on(Timeout - 10000);		
%% 		Res ->
%% 		    ct:pal("### Res: ~w", [Res]),
%% 		    ct:fail("Power ON failed, du to unexpected return value from power on.")
%% 	    end;
%% 	{error,econnrefused} ->
%% 	    timer:sleep(250), % If occupied then wait and try again.
%% 	    wait_for_power_on(Timeout - 250);
%% 	 Return ->
%% 	    ct:pal("### Power ON failed, due to: ~w", [Return]),
%% 	    ct:fail("Power ON failed.")
%%     end.


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

%% wait_for_mw_start(Cmd, N) ->
%%     case Cmd of
%%     	reboot ->
%% 	    ok = rct_rs232:login(console),
%%     	    ct:pal("### reboot!",[]),
%%     	    Start = os:timestamp(),
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
%% 	    End = os:timestamp(),
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
	    %% End = os:timestamp(),
	    %% End;
	    ok;
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
    case rct_rpc:call(rpc, os, cmd, ["pgrep com -u $USER"], 500, noprint)  of
    	{badrpc,_} -> %{badrpc, timeout | nodedown}
	    timer:sleep(500),
	    wait_for_com_started(Timeout - 500);
	[] ->	    
	    timer:sleep(500),
	    wait_for_com_started(Timeout - 500);
	Data ->
    	    case re:run(Data, "^[0-9]+$", [{capture,[1],list}]) of
    		{match, _Result} ->
		    End = os:timestamp(),
		    %% ct:pal("Data: ~p",[Data]),
		    {End, Data};
    		_ ->
		    timer:sleep(500),
		    wait_for_com_started(Timeout - 500)
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
    case rct_rpc:call(rpc, os, cmd, ["pgrep com -u $USER"], 500, noprint)  of
    	{badrpc, _} -> %{badrpc, timeout | nodedown}
	    timer:sleep(500),
	    wait_for_com_restarted(ComPid, Timeout - 500);
	[] ->	
	    timer:sleep(500),
	    wait_for_com_restarted(ComPid, Timeout - 500);
	Data ->
    	    case re:run(Data, "^[0-9]+$", [{capture,[1],list}]) of
    		{match, _} ->
		    End = os:timestamp(),
		    %% ct:pal("ComPid: ~p",[ComPid]),
		    %% ct:pal("NewComPid: ~p",[Data]),
		    %% ct:pal("End: ~p",[End]),
		    case ComPid == Data of
			true ->
			    %% ct:pal("com has not restarted yet!", []),
			    timer:sleep(500),
			    wait_for_com_restarted(ComPid, Timeout - 500);
			false -> %% Com is restarted with new Pid.
			    {End, Data}
		    end;
		_Rec ->
		    timer:sleep(500),
		    wait_for_com_restarted(ComPid, Timeout - 500)
    	    end
    end.

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
    ct_netconfc:close_session(nc1),
    ct:fail("Netconf not started within max timeout after restart.");

wait_for_netconf_started(Timeout) ->
    case ct_netconfc:open(nc1,[]) of
	{ok,_} ->
	    {ok,_} = ct_netconfc:get_config(nc1,running,{'ManagedElement',
							 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
							 [{managedElementId,[],["1"]}]}),
	    End = os:timestamp(),
	    ok = ct_netconfc:close_session(nc1),
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
    case rct_rpc:call(rpc, appmServer, get_apps, [], 500, noprint) of
    	{badrpc, _} ->
	    timer:sleep(500),
	    wait_for_appl_started(Timeout - 500);
	[] -> 
	    timer:sleep(500),
	    wait_for_appl_started(Timeout - 500);
	AppProplist ->
	    ct:pal("Apps: ~p ",[AppProplist]),
	    End = os:timestamp(),
	    End
	end.


%% ===========================================================================
%% @doc
%% Get CXS label. <br/>
%% Get SW version using COM cli interface. <br/>
%% @spec get_sw_version() -> 'CXS_LABEL'
%% @end
%% ===========================================================================
get_sw_version() ->
    ct:pal("### Get SW version",[]),
    ok = rct_cli:connect(cli, noprint),
    {ok ,RecievedData} = rct_cli:send(cli,"show ManagedElement=1,SystemFunctions=1,SwInventory=1", noprint),
    %% ct:pal("RecievedData: ~p", [RecievedData]),

    %% Clean upp RecievedData string to a list of strings.
    Var = string:tokens(RecievedData, "=\r\n "),
    %% drop data until you reach "SwVersion", the wanted CXS label is the second element.
    [_, CXS | _ ] = lists:dropwhile(fun(X) ->
					    X =/= "SwVersion" 
				    end, Var),
    ct:pal("CXS: ~p", [CXS]),
    ok = rct_cli:disconnect(cli, noprint),

    list_to_atom(CXS).


%% ===========================================================================
%% @doc
%% @spec check_that_com_has_not_restarted(ComPid) -> ok
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
%% @spec check_nr_of_imm_obj_not_increased(NrOfImmObj) -> ok
%% @end
%% ===========================================================================
check_nr_of_imm_obj_not_increased(NrOfImmObj) ->
    End_NrOfImmObj = length(rct_rpc:call(rpc, ets, tab2list, [imm_objects], 10000, noprint)),
    case NrOfImmObj of
    	End_NrOfImmObj ->
	    %% ct:pal("Nr of IMM objects is expected:\n Start : ~p, End : ~p",[NrOfImmObj, End_NrOfImmObj]),
    	    ok;
    	_ ->
	    ct:pal("Nr of IMM objects is NOT expected:\n Start : ~p, End : ~p",[NrOfImmObj, End_NrOfImmObj]),
    	    ct:fail(" Nr of IMM objects is NOT expected!")
    end,
    ok.
