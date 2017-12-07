%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rob_cluster_restart_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R4A/R5A/R11A/5
-module(rob_cluster_restart_SUITE).
-include_lib("common_test/include/ct.hrl").
%%% ----------------------------------------------------------
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
%%% R4A/2      2012-10-26 etxivri     Created
%%% R4A/5      2012-11-02 etxivri     Update wait for nodes comes up. 
%%%                                   And some cleanup.
%%% R5A/2      2012-11-12 etxivri     Update to be runed in our CI.
%%% R5A/3      2012-11-16 etxivri     Update syntax in groups so it works in 
%%%                                   jovp script.
%%% R5A/3      2012-12-08 etxivri     Update due to changed behaviour.
%%%                                   Update warm restart check on core.
%%% R11A/2     2017-09-12 etxivri     Update due to now dual and trippel is hot again.
%%% R11A/2     2017-09-22 etxivri     Update to install dual. Special procedure is needed at the moment.
%%% R11A/3     2017-09-27 etxivri     Add path to rcs_exec for git env.
%%% R11A/5     2017-10-06 etxivri     changed a pal to log
%%%                                   
%%% ----------------------------------------------------------
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 groups/0,
         core_reboot/1,
	 regular_reboot/1,
	 core_mo_warm/1,
	 regular_mo_warm/1,
	 core_mo_cold/1,
	 regular_mo_cold/1,
	 core_power/1,
	 regular_power/1,
	 core_mo_cwt/1,
	 regular_mo_cwt/1,

	 export_ai_log/1,
	 export_esi/1,

	 %% Install
	 reboot_core_after_install/1,
	 config_core_with_mo_regular_1/1,
	 set_mpid_3_on_regular_1/1,
	 coli_reinstall_regular_1/1,
	 wait_for_regular_1_enabled/1
	]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
suite() -> 
    case check_if_vc_board() of
	"yes"-> 
	    [{ct_hooks, [{rct_htmllink,[]},
			 {rct_rs232,console},
			 {rct_power,node},                                           
			 {cth_conn_log,[]},
			 {rct_consserv,cs1},
			 {rct_cli, {cli, [{user, "SysAdminTest"}, 
					  {password, "SysAdminTest"},
					  manual_connect]}},
			 {rct_netconf, {nc1, man_auth}}
			]}];
	_->
	    %% test_server:break("A"),
	    N = length(ct:get_config(test_nodes)),
	    Hooks = 
		[{rct_consserv,
		  [list_to_atom(
		     "cs"++integer_to_list(X))||X<-lists:seq(1,N)]},
		 {rct_rs232,   
		  [list_to_atom(
		     "console"++integer_to_list(X))||X<-lists:seq(1,N)]},
		 {rct_logging, 
		  [{1, log1, all, 
		    [{erlang,{["ERROR REPORT","CRASH REPORT"],
			      [
			      "not responding"
			      %% "Program ID [0-9]+ has crashed"
			      ]}}],[]},
		   {2, log2, all, 
		    [{erlang,{["ERROR REPORT","CRASH REPORT"],
			      [
			       "not responding",
			       "COI not started, cannot clear application alarms"
			      ]}}],[]}
		  ]},


		 %% {rct_core, 
		 %%  [[]||_X<-lists:seq(1,N)]},
		 {rct_rpc,[list_to_atom(
			     "rpc"++integer_to_list(X))||X<-lists:seq(1,N)]},
		 {rct_power,[list_to_atom(
			       "power"++integer_to_list(X))||X<-lists:seq(1,N)]}
		],
	    [{ct_hooks, [{rct_htmllink,[]}] ++
		  Hooks ++
		  [{rct_netconf, {nc1, pretty}},
		   {rct_cli, {cli1, [manual_connect]}},
		   %% {rct_coli, {coli1, [manual_connect]}},
		   {rct_coli,[{1, coli1, ssh_lmt_ipv4, [manual_connect]},
			      {2, coli2, ssh_lmt_ipv4, [manual_connect]}
			     ]},
		   %% {rct_coli, {coli2, [manual_connect]}}, %% need for coli reinstall
		   {cth_conn_log,[]}]}]
    end.

%%% ----------------------------------------------------------
%%% Definitions
%%% ----------------------------------------------------------
-define(NC, nc1).
-define(CLI, cli1).

-define(N, length(ct:get_config(test_nodes)) ).
-define(NODE_LIST, lists:seq(1, ?N) ).

-define(CONSOLE_1, console1). %% Core
-define(CONSOLE_2, console2).

-define(CONSOLE_LIST, [list_to_atom(
			 "console"++integer_to_list(X))||X<-lists:seq(1, ?N)] ).

-define(RPC_1, rpc1). %% Core
-define(RPC_2, rpc2).
-define(COLI, coli1 ). %% Core
-define(COLI2, coli2 ). %% regular 1, need for coli reinstall

-define(RPC_LIST, [list_to_atom(
		     "rpc"++integer_to_list(X))||X<-lists:seq(1, ?N)] ).

-define(POWER_LIST, [list_to_atom(
		     "power"++integer_to_list(X))||X<-lists:seq(1, ?N)] ).

-define(ERL_NODE_LIST, lists:map(fun(Rpc) ->
					 {ok, ErlNode} = 
					     rct_rpc:get_erlnode(Rpc),
					 ErlNode
				 end, ?RPC_LIST) ).

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
    %% %% lists:foreach(fun(Rpc) ->
    %% %% 			  rct_rpc:call(Rpc, appmServer, 
    %% %% 				       reset_restart_list, [], 10000)
    %% %% 			      end, ?RPC_LIST),
    %% clear_llog_all_nodes(),
    Config.

%% @hidden
end_per_testcase(_TestCase, Config) ->
    %% clear_llog_all_nodes(),
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p", [Reason]),
	    %% ct:log("Try to export esi"),
	    %% export_esi(Config),
	    %% export_ai_log(Config),
	    %% ct:sleep({minuites, 1})  %% To get more in console log if neede.
	    ok
    end,
    ok.

%% export_esi(Config) ->
%%     ct:log("Try to export esi"),
%%     aic_httpc:export_ai_log(Config),
%%     aic_httpc:export_esi(Config).

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [].

groups() ->
    [
     {sbc__pre_dc_cluster_lrat__dual_dus__1__group, [], [{group, group_1}]},
     {group_install, [], [
			  config_core_with_mo_regular_1,
			  reboot_core_after_install,
			  set_mpid_3_on_regular_1,
			  coli_reinstall_regular_1,
			  wait_for_regular_1_enabled
			  ]},
     {group_1, [], [
		    core_reboot,
		    regular_reboot,
		    core_mo_warm,
		    regular_mo_warm,
		    core_mo_cold,
		    regular_mo_cold,
		    core_power,
		    regular_power,
		    core_mo_cwt
		    %% regular_mo_cwt %% Not working!
		    %% export_esi
		   ]}
    ].


%% Install dual
%% config_core_with_regular_and_reboot(Config) ->
%%     config_core_with_mo_regular_1(_Config),
%%     reboot_core_after_install(Config),
%%     ok.


reboot_core_after_install(_Config) ->
    ok = ct:pal("### reboot core after install !",[]),
    [Core | _RegList] = ?CONSOLE_LIST,
    [ErlNodeCore | _T]= ?ERL_NODE_LIST,
    ok = rct_rs232:login(Core),
    ok = ct_telnet:send(Core, "cup --reboot"),
    net_kernel:disconnect(ErlNodeCore),
    {ok,_} = ct_telnet:expect(Core, "Ericsson Version:",
    			      [{total_timeout,90000}, 
    			       {idle_timeout,90000}, no_prompt_check]), 
    {ok,_} = ct_telnet:expect(Core, "login:",
    			      [{total_timeout,90000}, 
    			       {idle_timeout,90000}, no_prompt_check]), 
    lists:foreach(fun(Rpc) ->
    			  wait_node_is_working(node, Rpc)
    		  end, ?RPC_LIST),
    timer:sleep(60000),
    ok.

config_core_with_mo_regular_1(_Config) ->
    CoreNodeName = atom_to_list(ct:get_config({test_nodes,1})),
    ct:log("CoreNodeName:~n~p~n",[CoreNodeName]),
    ConfRegular1Xml = "/proj/rcs-tmp/etxarnu/multiDU/RBSNC_2_dual_du_netconf_create2.xml",
    Ls1 = os:cmd("ls "++ConfRegular1Xml),
    ct:log("Ls1 : ~p", [Ls1]),
    Ls2 = os:cmd("ls $RCT_TOP/test/bin/rcs_exec"),
    ct:log("Ls2 : ~p", [Ls2]),
    Answ = os:cmd("$RCT_TOP/test/bin/rcs_exec -m netconf -f "++ ConfRegular1Xml ++" "++ CoreNodeName),
    ct:log("Answ:~n~p~n",[string:tokens(Answ, "\n\r")]),
    timer:sleep(300000),
    ok.

set_mpid_3_on_regular_1(_Config)->
    [_Core | RegList] = ?CONSOLE_LIST,
    [Regular_1| _T] = RegList,
    %% ok = rct_rs232:login(),
    ok = ct_telnet:send(Regular_1, "echo \"3\" > /rcs/networkloader/mpid"),
    ok = ct_telnet:send(Regular_1, "cat /rcs/networkloader/mpid"),
    {ok,_} = ct_telnet:expect(Regular_1, "3",
    			      [{total_timeout,10000}, 
    			       {idle_timeout,10000}, no_prompt_check]), 
    %% net_kernel:disconnect(ErlNodeCore),
    ok.

coli_reinstall_regular_1(_Config) ->
    %% ct:pal("Coli: ~p, ",[?COLI2]),
    %% [_Core| Reglist] = ?COLI2,
    ColiRgular_1 = ?COLI2,
    rct_coli:connect(ColiRgular_1),
    {ok, _A} = rct_coli:send(ColiRgular_1, "/misc/authlevel disabled"),
    timer:sleep(2000),
    {ok, _B} = rct_coli:send(ColiRgular_1, "/sysm/sw -r"),

    [_Core | RegList] = ?CONSOLE_LIST,
    [ConsRegular_1 | _T1] =RegList,

    [_ErlNodeCore | ErlNodeRegList]= ?ERL_NODE_LIST,
    [ErlNodeRegular_1 | _T2] = ErlNodeRegList,

    %% ok = rct_rs232:login(Core),
    net_kernel:disconnect(ErlNodeRegular_1),
    {ok,_} = ct_telnet:expect(ConsRegular_1, "Ericsson Version:",
    			      [{total_timeout,90000}, 
    			       {idle_timeout,90000}, no_prompt_check]), 
    {ok,_} = ct_telnet:expect(ConsRegular_1, "login:",
    			      [{total_timeout,90000}, 
    			       {idle_timeout,90000}, no_prompt_check]), 

    lists:foreach(fun(Rpc) ->
    			  wait_node_is_working(node, Rpc)
    		  end, ?RPC_LIST),

    %% timer:sleep(10000),
    wait_for_netconf_started(),
    wait_for_site_config_complete(),
    ok.


wait_for_regular_1_enabled(_Config) ->
    wait_for_cli_start_on_regular(?CLI, "2", 300000).
wait_for_cli_start_on_regular(_CliRegular, _FRU, Timeout) when Timeout < 0 ->
    ct:fail("Cli not started within max timeout after restart.");
wait_for_cli_start_on_regular(CliRegular, FRU, Timeout) ->
    rct_cli:connect(CliRegular),
    case rct_cli:send(CliRegular,"show ManagedElement=1,Equipment=1,FieldReplaceableUnit="++FRU++",operationalState") of
	{ok, A} ->
	    ct:pal("# A: ~p", [A]),
	    rct_cli:disconnect(CliRegular),
	    case re:run(A, "ENABLED") of
		{match, _} ->
		    ct:pal("Regular is up and running");
		 nomatch ->
		    timer:sleep(10000),
		    wait_for_cli_start_on_regular(CliRegular, FRU, Timeout-10000)
	    end;
	Other ->
	    ct:pal("# Other: ~p", [Other]),
	    rct_cli:disconnect(CliRegular),
	    timer:sleep(10000),
	    wait_for_cli_start_on_regular(CliRegular, FRU, Timeout-10000)
    end.


%%--------------------------------------------------------------------
%% @doc
%% @spec core_reboot(Config) -> ok
%% @end
%%--------------------------------------------------------------------
core_reboot(Config) ->
    restart(Config, reboot, core),
    ok = check_expected_restart_in_llog("Cold", core),
    ok = check_expected_restart_in_llog("Cold", regular1).


%%--------------------------------------------------------------------
%% @doc
%% @spec regular_reboot(Config) -> ok
%% @end
%%--------------------------------------------------------------------
regular_reboot(Config) ->
    restart(Config, reboot, regular1),
    check_for_unexpected_restarts_on_core().

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
core_mo_warm(Config) ->
    restart(Config, mo_warm, core),
    %% [RpcCore| _Rest] = ?RPC_LIST,
    %% Llog = rct_rpc:call(RpcCore, os, cmd, ["llog"], 10000),
    %% ok = check_restart_rank("Warm", Llog).
    ok = check_expected_restart_in_llog("Warm", core),
    nok = check_expected_restart_in_llog("Warm", regular1),
    ok = check_expected_restart_in_llog("Cold", regular1).

regular_mo_warm(Config) ->
    restart(Config, mo_warm, regular1),
    ok = check_expected_restart_in_llog("Warm", regular1),
    check_for_unexpected_restarts_on_core().


check_expected_restart_in_llog(Restart, Node) ->
    [Core| RegularList] = ?CONSOLE_LIST,
    ct:log("Core : ~p", [Core]),
    ct:log("RegularList : ~p", [RegularList]),
    Console = case Node of
	      core -> Core;
	      regular1 ->lists:nth(1, RegularList)
	  end,
    rct_rs232:login(Console),
    {ok, Llog} = ct_telnet:cmd(Console, "llog"),
    ct:log("llog: ~p", [Llog]),
    case re:run(Llog, Restart) of
	{match, _} -> 
	    ct:log("### restart : ~p exist on conole :~p", [Restart, Console]),
	    ok;
	nomatch ->
	    ct:log("### restart : ~p NOT exist on conole :~p", [Restart, Console]),
	    nok
    end.
    
%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
core_mo_cold(Config) ->
    restart(Config, mo_cold, core),
    ok = check_expected_restart_in_llog("Cold", core),
    ok = check_expected_restart_in_llog("Cold", regular1).

regular_mo_cold(Config) ->
    restart(Config, mo_cold, regular1),
    ok = check_expected_restart_in_llog("Cold", regular1),
    check_for_unexpected_restarts_on_core().

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
core_mo_cwt(Config) ->
    restart(Config, mo_cwt, core),
    ok = check_expected_restart_in_llog("Cold With Test", core),
    ok = check_expected_restart_in_llog("Power", core),
    ok = check_expected_restart_in_llog("Cold", regular1).
    %% exist = check_if_str_exist_in_llog("Cold With Test", core),
    %% exist = check_if_str_exist_in_llog("Power On", core).

regular_mo_cwt(Config) ->
    restart(Config, mo_cwt, regular1),
    check_for_unexpected_restarts_on_core().

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------	
core_power(Config) ->
    restart(Config, power, core),
    ok = check_expected_restart_in_llog("Power", core),
    ok = check_expected_restart_in_llog("Cold", regular1).

regular_power(Config) ->
    restart(Config, power, regular1),
    ok = check_expected_restart_in_llog("Power", regular1),
    check_for_unexpected_restarts_on_core().


check_for_unexpected_restarts_on_core() ->
    [RpcCore| _Rest] = ?RPC_LIST,
    Llog = rct_rpc:call(RpcCore, os, cmd, ["llog"], 10000),
    nok = check_restart_rank("Warm", Llog),
    nok = check_restart_rank("Cold", Llog).



%%--------------------------------------------------------------------
%% @doc
%% - Behaviour.
%% - Warm on core -> Cold on regular.
%% - Warm on regular -> Cold on regular, Warm is disabled on regular.
%% - restart on regular -> Warm or Cold on core !!.
%% @end
%%--------------------------------------------------------------------
restart(_Config, Restart, NodeRole) ->
    ct:pal("Restart : ~p, NodeRole : ~p ", [Restart, NodeRole]),
    ct:pal("Nr of Nodes : ~p", [?N]),
    ct:pal("Node List : ~p", [?NODE_LIST]),
    ct:pal("CONSOLE_LIST : ~p", [?CONSOLE_LIST]),
    ct:pal("RPC_LIST : ~p", [?RPC_LIST]),
    ct:pal("POWER_LIST : ~p", [?POWER_LIST]),
    ct:pal("ERL_NODE_LIST : ~p", [?ERL_NODE_LIST]),

    %% test_server:break("B"),
    %% precondition all nodes is working.
    lists:foreach(fun(Rpc) ->
    			  wait_node_is_working(node, Rpc)
    		  end, ?RPC_LIST),

    lists:foreach(fun(Cons) ->
    			  ok = rct_rs232:login(Cons)
    		  end, ?CONSOLE_LIST),

    clear_llog_all_nodes(),
    timer:sleep(5000),
    check_llog_is_cleared(),
    %% AllApps = rct_rpc:call(Rpc, appmServer, get_apps, [], 1000),

    case Restart of
	reboot ->
	    reboot(NodeRole, ?CONSOLE_LIST);
	mo_warm ->
	    mo_restart(NodeRole, "WARM");
	mo_cold ->
	    mo_restart(NodeRole, "COLD");
	mo_cwt ->
	    mo_restart(NodeRole, "COLDWTEST");
	power ->	    
	    power(NodeRole, ?POWER_LIST)
    end,

    %% Check node restarts
    [Core | RegList] = ?CONSOLE_LIST,
    case Restart of
    	mo_warm -> %% will alway trig cold on regular
	    %% [_Core | RegList] = ?CONSOLE_LIST,
    	    check_nodes_restarts(RegList);
    	    %% wait_apps_restart(Rpc, AllApps);
	mo_cwt ->
	    Console = case NodeRole of 
			  core ->
			      Core;
			  regular1 ->
			      lists:nth(1, RegList)
		      end,
	    wait_for("Cold with test boot", Console),
	    wait_for("RBS boot configuration", Console);
    	_Other ->
    	    %% %% check_node_restarts(?CONSOLE_LIST)
    	    check_nodes_restarts(NodeRole)
    end,
    %% check_nodes_restarts(NodeRole),
    %% wait_for_all_node_login(?CONSOLE_LIST),
    
    lists:foreach(fun(RPC) ->
    			  wait_node_is_working(node, RPC)
    		  end, ?RPC_LIST),

    wait_for_netconf_started(),
    wait_for_site_config_complete(),
    %% timer:sleep(90000),
    %% nc_get_from_all_frus(),
    %% check_coli_to_all_frus(),

    lists:foreach(fun(Fru) ->
    			  wait_nc_get_from_fru(Fru)
    		  end, ?NODE_LIST),

    lists:foreach(fun(Fru) ->
    			  wait_coli_to_fru(Fru)
    		  end, ?NODE_LIST),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
check_nodes_restarts(NodeRole) ->
    case NodeRole of
    	core ->
    	    check_node_restart(?CONSOLE_LIST),
    	    wait_for_node_login(?CONSOLE_LIST);
    	_ -> %% regular.
    	     %% Use this when a restart on regular not trig restart on core.
    	    [_Core | RegConsolList] = ?CONSOLE_LIST,
    	    ct:pal("RegConsolList: ~p. ", [RegConsolList]),
    	    check_node_restart(RegConsolList),
    	    wait_for_node_login(RegConsolList)
    end.


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
check_node_restart(ConsoleList) ->
    Self = self(),    
    PidList = 
    	lists:map(
    	  fun(X) ->
    		  spawn(fun() -> 
    				ct:pal("### check node restart: ~p", 
    				       [X]),
    				case nodes_restart(X) of
    				    error ->
    					Self ! error;
    				    ConsName ->
					ct:pal("rcvd rply from : ~p", 
    					       [ConsName]),
    					Self ! {done, ConsName}
    				end
			end)
    	  end,
    	  ConsoleList),

    %%%%
    %% Wait on answer from check node restarts request.
    %%%%
    ct:pal("### Wait for answer that node har start to restart!", []), 
    Res = lists:map(fun(_Pid) ->
			    receive
				error -> 			      
				    error;
				{done, Console} ->
				    ct:pal("Node restarts OK: ~p", 
					   [Console]),
				    ok
			    after 120000 -> 
				    ct:fail("Error no answer within time!",
					    [])
			    end
		    end, PidList),
    
    lists:foreach(fun(PidA) ->
			  exit(PidA, kill)
		  end, PidList),
    
    %% ct:pal("Result: ~p", [Res]),
    %% check all expected ok is rcvd.
    ct:pal("# check all expected ok is rcvd from Result list: ~p", [Res]),
    ResOk = lists:filter(fun(Y) -> 
				 case Y of 
				     ok ->
					 true;
				     error -> 
					 false; 
				     _ -> 
					 false
				 end 
			 end, Res),
    ct:pal("#ResOk List from node restart: ~p", [ResOk]),
    A = length(ResOk),
    B = length(ConsoleList),
    A = B.


nodes_restart(Console) ->
    case ct_telnet:expect(Console,"Ericsson Version: ",
			  [{timeout, 300000},no_prompt_check]) of
	{ok,_} ->
	    Console;
	_ ->
	    error
    end.


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
wait_for_node_login(ConsoleList) ->
    Self = self(),    
    PidList = 
    	lists:map(
    	  fun(X) ->
    		  spawn(fun() -> 
    				ct:pal("### check login: ~p", 
    				       [X]),
    				case wait_for_login(X) of
    				    error ->
    					Self ! error;
    				    ConsName ->
					ct:pal("rcvd login from : ~p", 
    					       [ConsName]),
    					Self ! {done, ConsName}
    				end
			end)
    	  end,
    	  ConsoleList),

    %%%%
    %% Wait on answer from wait for login request.
    %%%%
    ct:pal("### Wait for node login !", []), 
    Res = lists:map(fun(_Pid) ->
			    receive
				error -> 			      
				    error;
				{done, Console} ->
				    ct:pal("Node login OK: ~p", 
					   [Console]),
				    ok
			    after 300000 -> 
				    ct:fail("Error no answer within time!",
					    [])
			    end
		    end, PidList),
    
    lists:foreach(fun(PidA) ->
			  exit(PidA, kill)
		  end, PidList),
    
    %% ct:pal("Result: ~p", [Res]),
    %% check all expected ok is rcvd.
    ct:pal("## check all expected ok is rcvd from Result list: ~p", [Res]),
    ResOk = lists:filter(fun(Y) -> 
				 case Y of 
				     ok ->
					 true;
				     error -> 
					 false; 
				     _ -> 
					 false
				 end 
			 end, Res),
    ct:pal("## ResOk List from node login: ~p", [ResOk]),
    A = length(ResOk),
    B = length(ConsoleList),
    A = B.

wait_for_login(Console) ->
    case ct_telnet:expect(Console,"login:",
			  [{timeout, 90000},no_prompt_check]) of
	{ok,_} ->
	    Console;
	_ ->
	    error
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
reboot(NodeRole, ConsoleList) ->
    Console = case NodeRole of
		  core ->
		      lists:nth(1, ConsoleList);
		  regular1 ->
		      lists:nth(2, ConsoleList)
	      end,
    
    case check_if_vc_board() of
	"yes"-> 
	    ok;
	_ ->
	    ct:pal("### trig reboot. pgh_restart_board, Node: ~p, console: ~p", 
		   [NodeRole, Console]),
	    ok = ct_telnet:send(Console, "/home/sirpa/bin/pgh_restart_board")
    end.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% @end
%% %%--------------------------------------------------------------------
%% mo_warm(NodeRole) ->
%%     ct:pal("### trig mo restart warm via cli."),
%%     FruId = case NodeRole of
%% 		core ->
%% 		    "1";
%% 		regular ->
%% 		    "2"
%% 	    end,

%%     case check_if_vc_board() of
%% 	"yes"-> 
%% 	    ok;
%% 	_ ->
%% 	    rct_cli:connect(?CLI),
%% 	    rct_cli:send(?CLI,"ManagedElement=1,Equipment=1,"
%% 			 "FieldReplaceableUnit="++FruId++
%% 			     ",restartUnit "
%% 			 "RESTART_WARM PLANNED_RECONFIGURATION 0"), 
%% 	    rct_cli:disconnect(?CLI)
%%     end.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% @end
%% %%--------------------------------------------------------------------
%% mo_cold(NodeRole) ->
%%     ct:pal("### trig mo restart cold via cli."),
%%     FruId = case NodeRole of
%% 		core ->
%% 		    "1";
%% 		regular ->
%% 		    "2"
%% 	    end,

%%     case check_if_vc_board() of
%% 	"yes"-> 
%% 	    ok;
%% 	_ ->
%% 	    rct_cli:connect(?CLI),
%% 	    rct_cli:send(?CLI,"ManagedElement=1,Equipment=1,"
%% 			 "FieldReplaceableUnit="++FruId++
%% 			     ",restartUnit "
%% 			 "RESTART_COLD PLANNED_RECONFIGURATION 0"), 
%% 	    rct_cli:disconnect(?CLI)
%%     end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
mo_restart(NodeRole, RestartType) ->
    ct:pal("### trig mo restart via cli. Restart Type: ~p ", [RestartType]),
    FruId = case NodeRole of
		core ->
		    "1";
		regular1 ->
		    "2"
	    end,

    case check_if_vc_board() of
	"yes"-> 
	    ok;
	_ ->
	    rct_cli:connect(?CLI),
	    rct_cli:send(?CLI,"ManagedElement=1,Equipment=1,"
			 "FieldReplaceableUnit="++FruId++
			     ",restartUnit "
			 "RESTART_"++RestartType++" PLANNED_RECONFIGURATION 0"), 
	    rct_cli:disconnect(?CLI)
    end.


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
power(NodeRole, PowerList) ->
    Power = case NodeRole of
		  core ->
		      lists:nth(1, PowerList);
		  regular1 ->
		      lists:nth(2, PowerList)
	      end,

    lists:foreach(fun(Nr) ->
    			  Log = "log"++integer_to_list(Nr),
    			  rct_logging:get_all(list_to_atom(Log))
    		  end, ?NODE_LIST),

    %% rct_logging:get_all(log1), %% To get all erl log after power
    %% rct_logging:get_all(log2), %% To get all erl log after power

    net_kernel_disconnect(?ERL_NODE_LIST),
    timer:sleep(2000),
    ct:pal("### power off/on! : ~p",[Power]),
    ok = rct_power:cycle(Power).

net_kernel_disconnect(ErlNodeList) ->
    lists:foreach(fun(ErlNode) ->
			  ct:pal("disconnetc ErlNode: ~p ", [ErlNode]),
			  net_kernel:disconnect(ErlNode)
		  end, ErlNodeList).

					   


%%========================================================================
%% wait_node_is_working
%%========================================================================
wait_node_is_working(NodeRole, Rpc) ->
    ct:pal("Poll start for Node: ~p to be working, using rpc: ~p", 
	   [NodeRole, Rpc]), 
    wait_node_is_working(NodeRole, Rpc, 300000).
wait_node_is_working(NodeRole, Rpc, Timeout) when Timeout < 0 ->
    ct:pal("Tc will fail. Node: ~p , not working. rct_rpc: ~p", 
	   [NodeRole, Rpc]), 
    ct:fail("Node not working within max timeout after restart.");
wait_node_is_working(NodeRole, Rpc, Timeout) ->
    %% Answ = rct_rpc:call(rpc1, clhI, get_node_op_state, [], 10000),
    %% ct:pal("A: ~p", [Answ]),
    case rct_rpc:call(Rpc, clhI, get_node_op_state, [], 10000) of
	{ok,"Node is working"} ->
	    ct:pal("Node is working: ~p , rct_rpc: ~p", [NodeRole, Rpc]);
	Other -> 
	    ct:log("Answ not expected: ~p. sleep and try again", [Other]),
	    timer:sleep(10000),
	    wait_node_is_working(NodeRole, Rpc, Timeout-10000)
	    
    end.


%%========================================================================
%% wait_for_netconf_started
%%========================================================================
wait_for_netconf_started() ->
    ct:pal("Poll start nc"), 
    wait_for_netconf_started(300000).
wait_for_netconf_started(Timeout) when Timeout < 0 ->
    ct:fail("Netconf not started within max timeout after restart.");
wait_for_netconf_started(Timeout) ->
    case ct_netconfc:open(?NC, []) of
	{ok,_} ->
	    ct:pal("netconf open - ok.",[]),
	    ct_netconfc:close_session(?NC);
	_Other ->
	    ct:log("nc session not opened: ~p, sleep and try again.", [_Other]),
	    timer:sleep(10000),
	    ct_netconfc:close_session(?NC),
	    wait_for_netconf_started(Timeout - 10000)
    end.

%%========================================================================
%% wait_for_site_config_complete
%%========================================================================
wait_for_site_config_complete() ->
    ct:pal("Poll for SITE_CONFIG_COMPLETE"),
    wait_for_site_config_complete(600000).
wait_for_site_config_complete(Timeout)  when Timeout < 0 ->
    ct:fail(" SITE_CONFIG_COMPLETE not rcvd within max timeout after restart.");
wait_for_site_config_complete(Timeout) ->
    case ct_netconfc:open(?NC,[{timeout, 5000}]) of
	{ok,_} ->
	    {ok,[{'ManagedElement',
		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		  [{managedElementId,[],["1"]},
		   {'NodeSupport',
		    [{xmlns,"urn:com:ericsson:ecim:RmeSupport"}],
		    [{nodeSupportId,[],["1"]},
		     {'AutoProvisioning',
		      [{xmlns,"urn:com:ericsson:ecim:RmeAI"}],
		      [{autoProvisioningId,[],["1"]},
		       {rbsConfigLevel,[],["SITE_CONFIG_COMPLETE"]}]}]}]}]} = 
		ct_netconfc:get(?NC,{'ManagedElement',
				     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				     [{managedElementId,[],["1"]},
				      {'NodeSupport',[],
				       [{nodeSupportId,[],["1"]},
					{'AutoProvisioning',[],
					 [{autoProvisioningId,[],["1"]}]}]}]}),
	    ct_netconfc:close_session(?NC),
	    ct:pal("SITE_CONFIG_COMPLETE rcvd - ok.",[]);
	_Other ->
	    ct:log("SITE_CONFIG_COMPLETE not rcvd: ~p, ~n"
		   "sleep and try again.", [_Other]),
	    timer:sleep(10000),
	    ct_netconfc:close_session(?NC),
	    wait_for_site_config_complete(Timeout - 10000)
    end.

%%========================================================================
%% nc_get_from_all_frus
%%========================================================================
%% nc_get_from_all_frus() ->
%%     {ok, _} = ct_netconfc:open(?NC, [{timeout, 5000}]),
%%     %% nc_get_all_frus(),
%%     lists:foreach(
%%       fun(Node) ->
%% 	      {ok, A} =
%% 		  ct_netconfc:get(?NC,{'ManagedElement',
%% 				       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
%% 				       [{managedElementId,[],["1"]},
%% 					{'Equipment',[],
%% 					 [{equipmentId,[],["1"]},
%% 					  {'FieldReplaceableUnit',
%% 					   [{xmlns,
%% 					     "urn:com:ericsson:ecim:ReqFieldReplaceableUnit"}],
%% 					   [{fieldReplaceableUnitId,[],[integer_to_list(Node)]}]}
%% 					 ]}]}),
%% 	      ct:pal("rcvd : ~p",[A])
%%       end, ?NODE_LIST),
%%     ct_netconfc:close_session(?NC).


wait_nc_get_from_fru(Fru) ->
    wait_nc_get_from_fru(Fru, 90000).
wait_nc_get_from_fru(Fru, Timeout) when Timeout < 0 ->
    ct:pal("No reply on nc get from FRU: ~p", [Fru]),
    ct:fail("TC will fail. no reply on nc get.");
wait_nc_get_from_fru(Fru, Timeout) ->
    ct_netconfc:open(?NC, [{timeout, 5000}]),
	case ct_netconfc:get(?NC,{'ManagedElement',
				  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				  [{managedElementId,[],["1"]},
				   {'Equipment',[],
				    [{equipmentId,[],["1"]},
				     {'FieldReplaceableUnit',
					   [{xmlns,
					     "urn:com:ericsson:ecim:ReqFieldReplaceableUnit"}],
				      [{fieldReplaceableUnitId,[],[integer_to_list(Fru)]}]}
				    ]}]}) of
	    {ok, A} ->
		ct_netconfc:close_session(?NC),
		ct:log("OK from FRU: ~p, rcvd : ~p",[Fru, A]);
	    _Other ->
		ct_netconfc:close_session(?NC),
		ct:log("## NOK from FRU: ~p, rcvd : ~p. sleep",[Fru, _Other]),
		timer:sleep(10000),
		wait_nc_get_from_fru(Fru, Timeout-10000)
	end.

%% %%========================================================================
%% %% wait_apps_restart(Rpc, AllApps),
%% %%========================================================================
%% wait_apps_restart(Rpc, AllApps) ->
%%     NrOfApps = length(AllApps),
%%     ct:pal("wait for nr of apps: ~p is restarted" , [NrOfApps]),
%%     ct:pal("# Wait for applications goes down." , []),
%%     wait_apps_goes_down(Rpc, NrOfApps, 60000),
%%     ct:pal("# Wait for all applications is up." , []),
%%     wait_all_apps_up(Rpc, NrOfApps, 90000).

%% %%========================================================================
%% %% wait_apps_goes_down(_Rpc, _NrOfApps, Timeout)
%% %%========================================================================
%% wait_apps_goes_down(_Rpc, _NrOfApps, Timeout) when Timeout < 0 ->
%%     ct:fail("Appl not going down within max timeout.");
%% wait_apps_goes_down(Rpc, NrOfApps, Timeout) ->
%%     case rct_rpc:call(Rpc, appmServer, get_apps, [], 5000) of
%% 	{badrpc,timeout} ->
%% 	    ct:log("{badrpc,timeout}, sleep and try again", []),
%% 	    wait_apps_goes_down(Rpc, NrOfApps, Timeout-5000);
%% 	Apps ->
%% 	    NewNrOfApps = length(Apps),
%% 	    ct:pal("# Nr of existing Apps: ~p." , [NewNrOfApps]),
%% 	    case NewNrOfApps of 
%% 		NewNrOfApps when NewNrOfApps < NrOfApps ->
%% 		    ct:pal("Applications goes down", []);
%% 		_Other ->
%% 		    ct:log("Applications NOT going down. "
%% 			   "Sleep and check again.", []),
%% 		    timer:sleep(2000),
%% 		    wait_apps_goes_down(Rpc, NrOfApps, Timeout-2000)
%% 	    end
%%     end.

%% %%========================================================================
%% %% wait_apps_is_up
%% %%========================================================================
%% wait_all_apps_up(_Rpc, _NrOfApps, Timeout) when Timeout < 0 ->
%%     ct:fail("All Appl not within max timeout.");
%% wait_all_apps_up(Rpc, NrOfApps, Timeout) ->
%%    case rct_rpc:call(Rpc, appmServer, get_apps, [], 5000) of
%% 	{badrpc,timeout} ->
%% 	    ct:pal("{badrpc,timeout}, sleep and try again", []),
%% 	    wait_all_apps_up(Rpc, NrOfApps, Timeout-5000);
%% 	Apps ->
%% 	    NewNrOfApps = length(Apps),
%% 	   ct:log("# Nr of existing Apps: ~p." , [NrOfApps]),
%% 	   case NewNrOfApps of 
%% 	       NewNrOfApps when NewNrOfApps == NrOfApps ->
%% 		   ct:pal("Applications goes down", []);
%% 	       _Other ->
%% 		   ct:log("Applications NOT going down. "
%% 			  "Sleep and check again.", []),
%% 		   timer:sleep(2000),
%% 		   wait_all_apps_up(Rpc, NrOfApps, Timeout-2000)
%% 	   end
%%     end.


%%========================================================================
%% check_coli_to_all_frus
%%========================================================================
%% check_coli_to_all_frus() ->
%%     ok = rct_coli:connect(?COLI),
%%     lists:foreach(
%%       fun(FruNr) ->
%% 	      ct:pal("Check coli on FRU: ~p", [FruNr]),
%% 	      %% rct_coli:send(?COLI,"ManagedElement=1,Equipment=1,FieldReplaceableUnit="++integer_to_list(FruNr)++" help coli", "cmd shell usage")
%% 	      ColiCmd = "ManagedElement=1,Equipment=1,FieldReplaceableUnit="++
%% 		  integer_to_list(FruNr)++" help coli",
%% 	      case rct_coli:send(?COLI, ColiCmd, "cmd shell usage") of
%% 	      	  {ok,_} -> 
%% 	      	      ok;
%% 	      	  Error -> 
%% 	      	      ct:fail("~p",[Error])
%% 	      end
%%       end, ?NODE_LIST),
%%     ok = rct_coli:disconnect(?COLI).



wait_coli_to_fru(Fru) ->
    wait_coli_to_fru(Fru, 90000).
wait_coli_to_fru(Fru, Timeout) when Timeout < 0 ->
    ct:pal("No reply using coli on FRU: ~p", [Fru]),
    ct:fail("TC will fail. no reply using coli.");
wait_coli_to_fru(Fru, Timeout) ->
    rct_coli:connect(?COLI),
    ColiCmd = "ManagedElement=1,Equipment=1,FieldReplaceableUnit="++
	integer_to_list(Fru)++" help coli",
	case rct_coli:send(?COLI, ColiCmd, "cmd shell usage") of
	    {ok, A} ->
		rct_coli:disconnect(?COLI),
		ct:log("OK coli on FRU: ~p, rcvd : ~p",[Fru, A]);
	    _Other ->
		rct_coli:disconnect(?COLI),
		ct:log("## NOK coli on FRU: ~p, rcvd : ~p. sleep",
		       [Fru, _Other]),
		timer:sleep(10000),
		wait_coli_to_fru(Fru, Timeout-10000)
	end.


check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.

%% netconf_open(Session, Param)->
%%     case check_if_vc_board() of
%% 	"yes"->  ct_netconfc:open
%% 		   (Session, [{user, "SysAdminTest"},
%% 			      {password, "SysAdminTest"}|Param]);
%% 	_-> ct_netconfc:open(Session,Param)
%%     end.



export_ai_log(Config) ->
    lists:foreach(fun(NodeNr) ->
			  IP = get_node_ip(NodeNr),
			  FileName = "ai_log_node_" ++ integer_to_list(NodeNr),
			  ct:pal("# Export AI log, ~n "
				 "FileName: ~p, ~n "
				 "NodeNr: ~p, ~n "
				 "IP: ~p", [FileName, NodeNr,IP]),
			  aic_httpc:export_ai_log(Config, FileName, IP)
		  end, ?NODE_LIST).

export_esi(Config) ->
    lists:foreach(fun(NodeNr) ->
			  IP = get_node_ip(NodeNr),
			  ct:pal("# Export Esi from : ~p , IP: ~p",
				 [NodeNr,IP]),
			  aic_httpc:export_esi(Config, IP)
		  end, ?NODE_LIST).

get_node_ip(NodeNr) ->
    HW = atom_to_list(ct:get_config({test_nodes,NodeNr})),
    Hw = list_to_atom(HW),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    ct:pal("Node IP : ~p", [IP]),
    IP.


clear_llog_all_nodes() -> 
    lists:foreach(fun(Console) ->
			  ct:pal("### clear llog. ~p",[Console]),
			  rct_rs232:login(Console),
			  ct_telnet:cmd(Console, "llog -c"),
			  timer:sleep(5000),
			  ct_telnet:cmd(Console, "llog")
		  end, ?CONSOLE_LIST).


check_llog_is_cleared() ->
    check_if_str_exist_in_llog("Warm"),
    check_if_str_exist_in_llog("Cold"),
    ok.

check_if_str_exist_in_llog(Restart) ->
    lists:foreach(fun(Console) ->
			  ct:pal("### Check llog is cleared. ~p",[Console]),
			  rct_rs232:login(Console),
			  {ok, Llog} = ct_telnet:cmd(Console, "llog"),
			  ct:log("llog: ~p", [Llog]),
			  case Llog of
			      [] -> ok;
			      _Oter ->
				  case re:run(Llog, Restart) of
				      {match, _} -> 
					  ct:fail("### exist => unexpected, fail the Test");
				      nomatch ->
					  ct:pal("### not_exist => ok")
				  end
			  end
		  end, ?CONSOLE_LIST).


check_restart_rank(SearchStr, Log) ->
    ct:log("### SearchStr: ~p", [SearchStr]),
    LogA = string:tokens(Log , " \n-"),
    ct:log("### Log: ~p", [LogA]),

    SearchList = lists:dropwhile(fun(X) ->
					 X=/=SearchStr
				 end, LogA),
    ct:log("SearchList log: ~p", [SearchList]),
    case re:run(SearchList, "Warm") of
	{match, _} ->
	    ok;
	nomatch ->
	    nok
    end.



wait_for(Str, Console) ->
    wait_for(Str, Console, 600000).
wait_for(Str, _Console, Timeout) when Timeout < 0 ->
    ct:pal("Tc will fail due to str : ~p, not rcvd within expected time",[Str]),
    ct:fail("No str within expected time");
wait_for(Str, Console, Timeout) ->
    case ct_telnet:expect(Console, Str, 
			  [{timeout,20000}, no_prompt_check]) of
	{ok,_} -> 
	    ok;
	_Other ->
	    timer:sleep(20000),
	    wait_for(Str, Console, Timeout-20000)
    end.
