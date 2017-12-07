%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	restart_vnf_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/R10A/R12A/4
-module(restart_vnf_SUITE).
-id('Updated by CCase').
-vsn('/main/R9A/R10A/R12A/4').
-date('2017-11-15'). 
-author('etxivri').
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
%%% R9A/1      2017-03-09 etxivri     create
%%% R9A/2      2017-03-10 etxivri     changed ct:pal to ct:log
%%% R10A/1     2017-07-04 etxivri     Update to llog
%%% R12A/1     2017-10-24 etxivri     Split it into sveral tests.
%%% R12A/2     2017-10-26 etxivri     Add timing measurement for heal.
%%% R12A/3     2017-11-07 etxivri     Update due to new beahviour
%%% R12A/4     2017-11-15 etxivri     -Set vnfm to down in hook due to soon rpc will not work 
%%%                                    on vnfm and the logging will fail.
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
	 
	 manual_restart_vnf_heal/1,
	 wait_for_cli_and_nc_is_up/1,
	 wait_for_cold_exist_in_llog/1,
	 sleep_2_min/1,

	 break/1
	]).

-define(RESULTDIR, "/proj/rcs/measurements/heal/master").
%% -define(RESULTDIR, "/proj/rcs/measurements/heal/tmp/").

%% @hidden
suite() -> 
    [{ct_hooks,[{rct_node_state, [{1,vnfm,down},{2,vnf,up} ]},
		{rct_logging, [{1, log1, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], 
				["comsaNtpServer: NTP: initial supervision timer expired"]},
			       {2, log2, all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}], []}]},
		%% {rct_cli, [{cli1, [manual_connect]},{cli2, [manual_connect]}]},
		{rct_cli, [{1, cli1, ssh_lmt_ipv4, [manual_connect]},
			   {2, cli2ipv4, ssh_lmt_ipv4, [manual_connect]},
			   {2, cli2ipv6, ssh_lmt_ipv6, [manual_connect]}
			  ]},
	       
		{rct_coli,[{coli1,[manual_connect]},{coli2,[manual_connect]}]},
		{rct_rpc,[rpc1,rpc2]},
		%% {rct_netconf,[nc1,nc2]},
		{rct_netconf,[{1, nc1, oam_auto},
			      {2, nc2ipv4, ssh_lmt_ipv4},
			      {2, nc2ipv6, ssh_lmt_ipv6}
			     ]},

		{rct_http,[http1,http2]},
		{rct_vnfm,[vnfm1]},
		{cth_conn_log, []},
		{rct_core,[[],[]]}
		]}].

%% @hidden
init_per_suite(Config) -> Config.
%% @hidden
end_per_suite(_Config) -> ok.
%% @hidden
init_per_testcase(_TestCase, Config) -> Config.
%% @hidden
end_per_testcase(_TestCase, _Config) -> ok.
%% @hidden

all() -> [manual_restart_vnf_heal,
	  wait_for_cli_and_nc_is_up,
	  wait_for_cold_exist_in_llog,
	  wait_for_cli_and_nc_is_up,
	  sleep_2_min
	 ].


break(_Config) ->
    test_server:break("AA").


%%%--------------------------------------------------------------------
%%% @doc
%%%   manual_restart_vnf_heal , this will trig an manual restart on VNF. <br/>
%%% @end
%%%--------------------------------------------------------------------
manual_restart_vnf_heal(_Config) ->    
    %% Check cli and nc before resstart.
    ct:pal("## Check Cli and Netconf before restart ##"),
    ct:pal("## Check cli using ipv4 ##"),
    check_vnf_cli(cli2ipv4),
    ct:pal("## Check Netconf using ipv4 ##"),
    check_vnf_nc(nc2ipv4),

    ct:pal("## Check cli using ipv6 ##"),
    check_vnf_cli(cli2ipv6),
    ct:pal("## Check Netconf using ipv6 ##"),
    check_vnf_nc(nc2ipv6),
    
    clear_llog(),

    ct:pal("## Get VNF instance"),
    {ok, #{<<"vnfs">> := [VnfMap]}} = rct_vnfm:get_vnfs(vnfm1),
    ct:pal("VnfMap: ~p",[VnfMap]),
    VnfInstanceId = maps:get(<<"instanceId">>, VnfMap),
    ct:pal("vnfInstanceId ~p",[VnfInstanceId]),

    Start1 = os:timestamp(),
    ct:pal("VNF HEAL"),
    {ok, Header, _Body} = rct_vnfm:vnf_heal(vnfm1, VnfInstanceId),
    ct:log("HeaderPropList:~p", [Header]),
    BinaryVnfLcOpId = rct_vnfm:get_vnf_lcm_op_occs_binary(Header),

    %% {ok, #{<<"vnfLcOpId">> := VnfLcOpId}} = 
    %% 	rct_vnfm:vnf_heal(vnfm1, VnfInstanceId),
    %% ct:pal("VnfLcOpId: ~p",[VnfLcOpId]),
    timer:sleep(10000),
    ok = rct_vnfm:wait_for_lcop_success(vnfm1, BinaryVnfLcOpId, 300000),

    End1 = os:timestamp(),
    HealTime = trunc(timer:now_diff(End1, Start1) /1000/1000),
    ct:pal("HealTime: ~p sec",[HealTime]),

    ct:pal("## Wait for Netconf using ipv6 is up after restart ##"),
    wait_for_nc_is_up(nc2ipv6, 300000),
    End2 = os:timestamp(),
    NcIPv6Time = trunc(timer:now_diff(End2, Start1) /1000/1000),
    ct:pal("Time from ordered heal to netconf ipv6 on lmt is up: ~p sec",[NcIPv6Time]),

    write_times(HealTime, NcIPv6Time).


%%%--------------------------------------------------------------------
%%% @doc
%%%   write_times
%%% @end
%%%--------------------------------------------------------------------
write_times(HealTime, NcIPv6Time) ->
    %% test_server:break("AA"),
    
    %% Branch = ct:get_config({jenkins_config, branch}),
    Branch = get_from_jenkins_config(branch),
    ct:log("Branch: ~p", [Branch]),

    USER = os:getenv("USER"),
    ct:log("USER: ~p", [USER]),

    %% TestType = ct:get_config({jenkins_config, test_type}),
    TestType = get_from_jenkins_config(test_type),
    ct:log("TestType: ~p", [TestType]),

    %% BoardType = proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))),
    %% ct:log("BoardType: ~p", [BoardType]),

    FileName = Branch++"_"++USER++"_"++TestType++"_"++"restart_vnf_suite.txt",
    ct:log("FileName: ~p~nExist under: ~p ", [FileName, ?RESULTDIR]),

    %% Container = ct:get_config({jenkins_config, container}),
    Container = get_from_jenkins_config(container),
    ct:log("Container: ~p", [Container]),

    write_to_file(FileName, 
    		  "~p;~p;~p;~p~n", 
    		  [httpd_util:rfc1123_date(),
    		   HealTime,
    		   NcIPv6Time,
		   Container
    		   ],
    		  ?RESULTDIR),
    ok.


%%%--------------------------------------------------------------------
%%% @doc
%%%   get_from_jenkins_config
%%% @end
%%%--------------------------------------------------------------------
get_from_jenkins_config(Atom) ->
    Variable = case ct:get_config({jenkins_config, Atom}) of
		   undefined ->
		       "undefined";
		   Value ->
		       Value
	       end,
    ct:log("Jenkins variable: ~p~nReturns: ~p", [Atom, Variable]),
    Variable.


%%%--------------------------------------------------------------------
%%% @doc
%%%   write_to_file
%%% @end
%%%--------------------------------------------------------------------
write_to_file(FileName, PrintOpts, MeasData, ResultDir) ->
    rct_tlib:
    	writeDataToFile(ResultDir, FileName, PrintOpts, MeasData),
    ct:pal("LogFile exist in : ~p , ~nfilename: ~p", [ResultDir, FileName]),
    ok.


%%%--------------------------------------------------------------------
%%% @doc
%%%   wait_for_cold_exist_in_llog
%%% @end
%%%--------------------------------------------------------------------
wait_for_cold_exist_in_llog(_Config) ->
    wait_for_str_exist_in_llog("Cold ").

%%%--------------------------------------------------------------------
%%% @doc
%%%   wait_for_cli_and_nc_is_up
%%% @end
%%%--------------------------------------------------------------------
wait_for_cli_and_nc_is_up(_Config) ->
    ct:pal("## Check cli using ipv4 ##"),
    wait_for_cli_is_up(cli2ipv4, 300000),
    ct:pal("## Check Netconf using ipv4 ##"),
    wait_for_nc_is_up(nc2ipv4, 300000),

    ct:pal("## Check cli using ipv6 ##"),
    wait_for_cli_is_up(cli2ipv6, 300000),
    ct:pal("## Check Netconf using ipv6 ##"),
    wait_for_nc_is_up(nc2ipv6, 300000).

%%%--------------------------------------------------------------------
%%% @doc
%%%   sleep_2_min
%%% @end
%%%--------------------------------------------------------------------
sleep_2_min(_Config) ->
    %%  to ensure no unexpected ERROR exist in erl log.
    ct:pal("Start sleep 2 min"),
    ct:sleep({minutes,2}),
    ct:pal("End sleep 2 min.").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Internal functions %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_vnf_cli(Cli) ->
    ok = rct_cli:connect(Cli),
    {ok,Res} = rct_cli:send(Cli,"show ManagedElement=1,SystemFunctions=1,SwInventory=1"),
    ct:log("## Cli get SwInventory: ~n~p", [Res]),
    ok = rct_cli:disconnect(Cli).


check_vnf_nc(Nc) ->
    {ok,_} = ct_netconfc:open(Nc, []),
    {ok, Res} = ct_netconfc:get(Nc,
				{'ManagedElement',
				 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				 [{managedElementId,[],["1"]}]}),
    ct:log("## Nc get config Res: ~n~p", [Res]),
    ok = ct_netconfc:close_session(Nc).


%%--------------------------------------------------------------------
%% @doc
%% wait_for_cli_is_up
%% @end
%%--------------------------------------------------------------------
wait_for_cli_is_up(_Cli, Timeout) when Timeout < 0 ->
    ct:pal("# Cli is not up within expected time #"),
    nok;
wait_for_cli_is_up(Cli, Timeout) ->
    case rct_cli:connect(Cli) of
	ok ->
	    ct:pal("# Cli is up an ready to be used #"),
	    ok = rct_cli:disconnect(Cli),
	    ok;
	_Other->
	    ct:log("Cli is NOT ready to be used : ~p. ~nSleep and try again.", 
		   [_Other]),
	    timer:sleep(5000),
	    wait_for_cli_is_up(Cli, Timeout-5000)
    end.

%%--------------------------------------------------------------------
%% @doc
%% wait_for_nc_is_up
%% @end
%%--------------------------------------------------------------------
wait_for_nc_is_up(_Nc, Timeout) when Timeout < 0 ->
    ct:pal("# Netconf is not up within expected time #"),
    nok;
wait_for_nc_is_up(Nc, Timeout) ->
    case ct_netconfc:open(Nc, []) of
	{ok,_} ->
	    ct:pal("# Netconf is up an ready to be used #"),
	    ok = ct_netconfc:close_session(Nc),
	    ok;
	_Other->
	    ct:log("Netconf is NOT ready to be used : ~p. ~nSleep and try again.", [_Other]),
	    timer:sleep(5000),
	    wait_for_nc_is_up(Nc, Timeout-5000)
    end.

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% wait_for_lcop_success
%% %% @end
%% %%--------------------------------------------------------------------
%% wait_for_lcop_success(_Vnfm, _VnfLcOpId, Timeout) when Timeout < 0 ->
%%     ct:pal("## lcop not successful within expected time."),
%%     nok;
%% wait_for_lcop_success(Vnfm, VnfLcOpId, Timeout) ->
%%     {ok, #{<<"status">> := LcopRes}} = 
%% 	rct_vnfm:vnf_show_lcop(Vnfm, VnfLcOpId),
%%     ct:log("LcopRes ~p",[LcopRes]),
%%     case binary_to_list(LcopRes) of
%% 	"success" ->
%% 	    ct:pal("## lcop is success."),
%% 	    ok;
%% 	 Other ->
%% 	    ct:log("## lcop is : ~p , not expected. sleep and try againg.", [Other]),
%% 	    timer:sleep(5000),
%% 	    wait_for_lcop_success(Vnfm, VnfLcOpId, Timeout-5000)
%%     end.


clear_llog() -> 
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc2),
    ct:pal("### clear llog.",[]),
    rct_rpc:xcall(rpc2, os, cmd, ["llog -c"], 10000, print),
    timer:sleep(2000),
    rct_rpc:xcall(rpc2, os, cmd, ["llog -c"], 10000, print),

    Llog = coli_llog(),
    case re:run(Llog, "Cold") of
	{match, _} ->
	    ct:fail("llog is not cleared!");
	nomatch ->
	    net_kernel:disconnect(ErlNode),
	    ok
    end.


wait_for_str_exist_in_llog(Str) ->
    wait_for_str_exist_in_llog(Str, 120000).
wait_for_str_exist_in_llog(_Str, Timeout) when Timeout < 0 ->
    ct:fail("Warm not exist in llog within expected timeout");
wait_for_str_exist_in_llog(Str, Timeout) ->
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

coli_llog() ->
    ok = rct_coli:connect(coli2),
    timer:sleep(1000),
    ct:pal("Check llog via coli."),
    {ok,Llog} = rct_coli:send(coli2,"diagm/llog"),
    rct_coli:disconnect(coli2),
    LLog = string:tokens(Llog, "\n\r"),
    ct:pal("coli llog: ~n~p", [LLog]),
    LLog.


fix_str(Str) ->
    string:tokens(Str,"\n\r").
