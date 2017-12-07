%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	xl_cold_5g_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2016
%%% @version /main/R6A/4
%%% 
%%% @doc == Restart Cold. ==
%%% <br/><br/>
%%% 
%%% @end


-module(xl_cold_5g_SUITE).
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
%%% R10A/1      2017-07-04 etxivri     Created
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
	 %% wait_for_login_prompt/1,
	 wait_for_netconf_started/1,
	 groups/0,
	 all/0,
	 coli_restart_cold/1,
	 coli_cold_restart/1
	]).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{timetrap, {minutes, 600}}, % 10 hours
     {ct_hooks, [
		 {rct_logging, [{1, log1, all, 
		 		 [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], 
		 		 []} ]},
		 {rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {rct_netconf, nc1},
		 %% {cth_conn_log, []},
		 {rct_cli, {cli, [manual_connect]}},
		 {rct_coli, {coli, [manual_connect]}},
                 {rct_core,[[]]}
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
	    ct:pal("Testcase failed due to: ~p. Export logs. \n", [Reason])
    end.


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
    [    
     ].


coli_restart_cold(Config) ->
    Type = ct:get_config({jenkins_config, installed_type}),
    ct:log("Type: ~p", [Type]),
    case Type of
	 Type when Type == vRC;
		   Type == vPP ->
	    coli_cold_restart(Config);
    	_Undef -> %% 
	    coli_cold_restart(Config)
    end.


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
coli_cold_restart(_Config) ->
    %% check node is up and running.
    wait_for_netconf_started(),
    ct_netconfc:close_session(nc1),
    ct:pal("Node is up."),
    clear_llog(),
    
    ok = rct_coli:connect(coli),
    timer:sleep(1000),
    ct:pal("Restart Node cold via coli."),
    {ok,_} = rct_coli:send(coli,"/board/restart -c"),
    rct_coli:disconnect(coli),
    
    wait_for_netconf_stoped(),
    timer:sleep(10000),
    
    wait_for_netconf_started(),
    ct_netconfc:close_session(nc1),

    ct:pal("### check llog.",[]),
    Llog = rct_rpc:call(rpc, os, cmd, ["llog"], 10000, print),
    ct:pal("### llog after cold restart : ~n~p",[fix_str(Llog)]),

    ok = wait_for_str_exist_in_llog("Cold "),

    coli_llog(),

    ct:pal("Start sleep 2 min to make sure everything is is OK after node UP."),
    ct:sleep({minutes,2}),
    ct:pal("End sleep 2 min."),
    ok.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
clear_llog() -> 
    ct:pal("### clear llog.",[]),
    rct_rpc:call(rpc, os, cmd, ["llog -c"], 10000, print),
    timer:sleep(2000),
    rct_rpc:call(rpc, os, cmd, ["llog -c"], 10000, print),

    %% Llog = rct_rpc:call(rpc, os, cmd, ["llog"], 10000, print),
    %% ct:pal("~p ", [fix_str(Llog)]),
    Llog = coli_llog(),
    case re:run(Llog, "Cold") of
	{match, _} ->
	    ct:fail("llog is not cleared!");
	nomatch ->
	    ok
    end.


wait_for_str_exist_in_llog(Str) ->
    wait_for_str_exist_in_llog(Str, 120000).
wait_for_str_exist_in_llog(_Str, Timeout) when Timeout < 0 ->
    ct:fail("Warm not exist in llog within expected timeout");
wait_for_str_exist_in_llog(Str, Timeout) ->
    %% Check_llog = rct_rpc:call(rpc, os, cmd, ["llog"], 10000, print),
    Check_llog = coli_llog(),
    case re:run(Check_llog, Str) of
	{match, _} -> 
	    ct:pal("Now : ~p, exist in llog.", [fix_str(Str)]),
	    ok;
	_Res -> 
	    ct:log(" Str not exist in llog, sleep and check again. ~n~p",
		   [_Res]),
	    timer:sleep(20000), 
	    wait_for_str_exist_in_llog(Timeout-20000)
    end.

%% ===========================================================================
%% @doc
%% Check for Netconf to be started. <br/>
%% Wait for ct_netconfc:get_config... returns  ok. <br/>
%% @spec wait_for_netconf_started() -> timestamp
%% @end
%% ===========================================================================
wait_for_netconf_started() ->
    wait_for_netconf_started(300000).
wait_for_netconf_started(Timeout) when Timeout < 500 ->
    ct:fail("Netconf not started within max timeout after restart.");
wait_for_netconf_started(Timeout) ->
    case ct_netconfc:open(nc1,[]) of
	{ok,_} ->
	    ct:pal("netconf open - ok.",[]);
	{error,{ssh,could_not_connect_to_server,econnrefused}} ->
	    timer:sleep(250),
	    wait_for_netconf_started(Timeout - 250);
	_Other ->
	    timer:sleep(10000),
	    wait_for_netconf_started(Timeout - 10000)
    end.


wait_for_netconf_stoped() ->
    wait_for_netconf_stoped(120000).
wait_for_netconf_stoped(Timeout) when Timeout < 500 ->
    ct:fail("Netconf not started within max timeout after restart.");
wait_for_netconf_stoped(Timeout) ->
    case ct_netconfc:open(nc1,[]) of
	{ok,_} ->
	    ct:pal("netconf still open - nok.",[]),
	    ct_netconfc:close_session(nc1),
	    timer:sleep(5000),
	    wait_for_netconf_stoped(Timeout - 5000);
	{error,{ssh,could_not_connect_to_server,econnrefused}} ->
	    ct:pal("netconf not open - ok.",[]),
	    ct_netconfc:close_session(nc1)
	%% _Other ->
	%%     timer:sleep(10000),
	%%     wait_for_netconf_started(Timeout - 10000)
    end.



coli_llog() ->
    ok = rct_coli:connect(coli),
    timer:sleep(1000),
    ct:pal("Check llog via coli."),
    {ok,Llog} = rct_coli:send(coli,"diagm/llog"),
    rct_coli:disconnect(coli),
    LLog = string:tokens(Llog, "\n\r"),
    ct:pal("coli llog: ~n~p", [LLog]),
    LLog.


fix_str(Str) ->
    string:tokens(Str,"\n\r").
