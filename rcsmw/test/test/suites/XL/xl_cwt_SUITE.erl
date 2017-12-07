%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	xl_cwt_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2016
%%% @version /main/R6A/4
%%% 
%%% @doc == Restart node with Cold With Test. ==
%%% Cold with test will now perform an power on after cwt.
%%% <br/><br/>
%%% 
%%% @end


-module(xl_cwt_SUITE).
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
%%% R6A/1      2016-05-15 etxivri     Created
%%% R6A/3      2016-05-16 etxivri     Add print llog using coli on sec board.
%%% R6A/4      2016-05-17 etxivri     change sheck of exp str in console log.
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
	 %% wait_for_login_prompt/1,
	 wait_for_netconf_started/1,
	 groups/0,
	 all/0,
	 %% create_fru/1,
	 restart_cwt/1,
	 coli_cwt_restart/1,
	 fru_restart_cwt/1,
	 sec_fru_restart_cwt/1
	]).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    case check_if_vc_board()  of
	"yes" -> [{timetrap, {minutes, 600}}, % 10 hours
		  {ct_hooks, [{rct_htmllink,[]},
			      {rct_consserv,cs1},
			      {rct_rs232,console},
			      {cth_conn_log,[]},
			      {rct_ssh,{ssh,[manual_connect]}},
			      {rct_coli, {coli, [manual_connect]}},
			      {rct_netconf, {nc1,man_auth}},
			      {rct_cli, {cli, [{user, "SysAdminTest"}, 
					       {password, "SysAdminTest"},
					       manual_connect]}}
			     ]}];
	_-> 
    [{timetrap, {minutes, 600}}, % 10 hours
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {rct_power,node},
		 {rct_netconf, nc1},
		 {rct_rs232,console},
		 {cth_conn_log, []},
		 {rct_consserv,cs1},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT",
						"CRASH REPORT"],
					       [
					       ]}}]}},
		 {rct_cli, {cli, [manual_connect]}},
		 {rct_coli, {coli, [manual_connect]}},
		 %% {rct_tlib,{load,[]}},
                 {rct_core,[]}
		]}]
    end.

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
	    ct:pal("Testcase failed due to: ~p. Export logs. \n", [Reason]),
	    aic_httpc:export_ai_log(Config),
	    aic_httpc:export_esi(Config)
    end.

netconf_open(Session, InPar)->
    Param = [{timeout,10000}|InPar],
    case check_if_vc_board()  of
	"yes" ->  ct_netconfc:open(Session, [{user, "SysAdminTest"}, {password, "SysAdminTest"}|Param]);
	_ -> ct_netconfc:open(Session,Param)
    end.

check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.


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


restart_cwt(Config) ->
    RatType = ct:get_config({jenkins_config, installed_type}),
    ct:log("RatType: ~p", [RatType]),
    case RatType of
	RatType when RatType == wrat;
		     RatType == lrat;
		     RatType == tcu ->
	    case check_if_vc_board() of
		"yes" ->
		    sec_fru_restart_cwt(Config);
		_Other ->
		    fru_restart_cwt(Config)
	    end;
	no -> %% No full UP.
	    coli_cwt_restart(Config);
    	_Undef -> %% 
	    coli_cwt_restart(Config)
    end.


%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
sec_fru_restart_cwt(_Config) ->
    rct_logging:get_all(log1),
    ct:pal("RBS.  trig restart cold with test. "),
    %% {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    %% net_kernel:disconnect(ErlNode),

    sec_wait_for_netconf_started(),
    ct_netconfc:close_session(nc1),
    ct:pal("Node is up."),
    ct:pal("Start sleep 2 min to make sure everything is written to disc."),
    ct:sleep({minutes,2}),
    ct:pal("End sleep 2 min. Now test can start."),

    coli_llog(),

    rct_cli:connect(cli),
    A = rct_cli:send(cli,"show ManagedElement=1,Equipment=1"),
    ct:pal("A: ~p", [A]),
    B = rct_cli:send(cli,"ManagedElement=1,Equipment=1,FieldReplaceableUnit=1,restartUnit RESTART_COLDWTEST PLANNED_RECONFIGURATION 0"),
    rct_cli:disconnect(cli),
    ct:pal("B: ~p", [B]),

    wait_for("Ericsson Version: "),
    %% %% wait_for("Starting RCS", 600000),
    wait_for("Cold with test boot", 600000),
    wait_for("RBS boot configuration", 600000),

    sec_wait_for_netconf_started(),
    ct_netconfc:close_session(nc1),
    
    %% Print llog
    coli_llog(),
    ok.

%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
fru_restart_cwt(_Config) ->
    rct_logging:get_all(log1),
    ct:pal("RBS.  trig restart cold with test. "),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),

    wait_for_netconf_started(),
    ct_netconfc:close_session(nc1),
    ct:pal("Node is up."),
    ct:pal("Start sleep 2 min to make sure everything is written to disc."),
    ct:sleep({minutes,2}),
    ct:pal("End sleep 2 min. Now test can start."),
    clear_llog(),

    rct_cli:connect(cli),
    rct_cli:send(cli,"ManagedElement=1,Equipment=1,FieldReplaceableUnit=1,restartUnit RESTART_COLDWTEST PLANNED_RECONFIGURATION 0"),
    rct_cli:disconnect(cli),
    %% timer:sleep(1000), %% take some sec before node restart.

    wait_for("Ericsson Version: "),
    %% wait_for("Starting RCS", 600000),
    wait_for("Cold with test boot", 600000),
    wait_for("RBS boot configuration", 600000),
    wait_for_login(),

    wait_for_netconf_started(),
    ct_netconfc:close_session(nc1),
    
    ct:pal("### check llog.",[]),
    Llog = rct_rpc:call(rpc, os, cmd, ["llog"], 10000),
    rct_rs232:login(console),
    ct_telnet:cmd(console, "llog"), %% Just for printouts of llog.

    ok = wait_for_str_exist_in_llog("Cold With Test", Llog),
    ok = wait_for_str_exist_in_llog("Power on", Llog),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
coli_cwt_restart(_Config) ->
    rct_logging:get_all(log1),
    %% check node is up and running.
    wait_for_netconf_started(),
    ct_netconfc:close_session(nc1),
    ct:pal("Node is up."),
    ct:pal("Start sleep 2 min to make sure everything is written to disc."),
    ct:sleep({minutes,2}),
    ct:pal("End sleep 2 min. Now test can start."),
    clear_llog(),

    ok = rct_coli:connect(coli),
    timer:sleep(1000),
    ct:pal("Restart Node with cwt."),
    {ok,_} = rct_coli:send(coli,"/board/restart -t"),
    rct_coli:disconnect(coli),
    %% timer:sleep(1000), %% take some sec before node restart.

    wait_for("Ericsson Version: "),
    %% wait_for("Starting RCS", 600000),
    wait_for("Cold with test boot", 600000),
    wait_for("RBS boot configuration", 600000),
    wait_for_login(),

    wait_for_netconf_started(),
    ct_netconfc:close_session(nc1),

    ct:pal("### check llog.",[]),
    Llog = rct_rpc:call(rpc, os, cmd, ["llog"], 10000),
    rct_rs232:login(console),
    ct_telnet:cmd(console, "llog"), %% Just for printouts of llog.

    ok = wait_for_str_exist_in_llog("Cold With Test", Llog),
    ok = wait_for_str_exist_in_llog("Power on", Llog),

    ok.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
wait_for(Str) ->
    wait_for(Str, 600000).
 wait_for(Str, Timeout) when Timeout < 0 ->
    ct:pal("Tc will fail due to str : ~p, not rcvd within expected time",[Str]),
    ct:fail("No str within expected time");
wait_for(Str, Timeout) ->
    case ct_telnet:expect(console, Str, 
			  [{timeout,20000}, no_prompt_check]) of
	{ok,_} -> 
	    ok;
	_Other ->
	    timer:sleep(20000),
	    wait_for(Str, Timeout-20000)
    end.


wait_for_login() ->
    wait_for_login(600000).
 wait_for_login(Timeout) when Timeout < 0 ->
    ct:fail("No login prompt within expected time");
wait_for_login(Timeout) ->
    case ct_telnet:expect(console, "login:", 
			  [{timeout,10000}, no_prompt_check]) of
	{ok,_} -> 
	    ok;
	_Other ->
	    timer:sleep(10000),
	    ct_telnet:send(console, ""),
	    wait_for_login(Timeout-10000)
    end.

clear_llog() -> 
    ct:pal("### clear llog.",[]),
    rct_rs232:login(console),
    ct_telnet:cmd(console, "llog -c"),
    timer:sleep(5000),
    ct_telnet:cmd(console, "llog"),
    ok.

wait_for_str_exist_in_llog(Str) ->
    wait_for_str_exist_in_llog(Str, 120000).
wait_for_str_exist_in_llog(_Str, Timeout) when Timeout < 0 ->
    ct:fail("Warm not exist in llog within expected timeout");
wait_for_str_exist_in_llog(Str, Timeout) ->
    Check_llog = rct_rpc:call(rpc, os, cmd, ["llog"], 10000, print),
    case re:run(Check_llog, Str) of
	{match, _} -> 
	    ct:pal("Now : ~p, exist in llog.", [Str]),
	    ok;
	_Res -> 
	    ct:log(" Str exist in llog, sleep and check again. ~n~p",
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

sec_wait_for_netconf_started() ->
    sec_wait_for_netconf_started(300000).
sec_wait_for_netconf_started(Timeout) when Timeout < 500 ->
    ct:fail("Netconf not started within max timeout after restart.");
sec_wait_for_netconf_started(Timeout) ->
    case netconf_open(nc1,[]) of
	{ok,_} ->
	    ct:pal("netconf open - ok.",[]);
	{error,{ssh,could_not_connect_to_server,econnrefused}} ->
	    timer:sleep(250),
	    sec_wait_for_netconf_started(Timeout - 250);
	_Other ->
	    timer:sleep(10000),
	    sec_wait_for_netconf_started(Timeout - 10000)
    end.

coli_llog() ->
    ok = rct_coli:connect(coli),
    timer:sleep(1000),
    ct:pal("Restart Node with cwt."),
    {ok,Llog} = rct_coli:send(coli,"diagm/llog"),
    rct_coli:disconnect(coli),
    LLog = string:tokens(Llog, "\n\r"),
    ct:log("llog: ~p", [LLog]),
    timer:sleep(1000).

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% @spec create_fru(_Config) -> ok
%% %% @end
%% %%--------------------------------------------------------------------
%% create_fru(_Config) ->
%%     ct:pal("Config:~n~p~n",[_Config]),
%%     NodeName = get_node_name(),
%%     ct:pal("Node:~n~p~n",[NodeName]),
%%     timer:sleep(90000), %% To ensure node is up.

%%     ct:pal("Conf: LRAT_basic_fru_netconf_create.xml. ~n"
%% 	   "- FieldReplaceableUnit=1 is created in Equipment=1",[]),
%%     os:cmd("rcs_exec -m netconf -f /home/etxivri/Kista/rcs_ithub/egenskaper/boan/7_nov/LRAT_basic_fru_netconf_create.xml " ++ NodeName),

%%     timer:sleep(10000),

%%     ct:pal("Conf: RBSNC_2_basic_fru_netconf_create_common.xml ~n"
%% 	   "- FieldReplaceableUnit=2 is created in Equipment=1 ~n"
%% 	   "- RiLink=1 is created in Equipment=1 ~n"
%% 	   "- SectorEquipmentFunction=1 is created in ManagedElement=1",[]),
%%     os:cmd("rcs_exec -m netconf -f /home/etxivri/Kista/rcs_ithub/egenskaper/boan/7_nov/RBSNC_2_basic_fru_netconf_create_common.xml "++ NodeName),

%%     timer:sleep(10000),
%%     ok.

%% %% ===========================================================================
%% %% @doc
%% %% @end
%% %% ===========================================================================
%% get_node_name() ->
%%     [{_, NodeName}] = ct:get_config(test_nodes),
%%     Node = atom_to_list(NodeName),
%%     Node.

