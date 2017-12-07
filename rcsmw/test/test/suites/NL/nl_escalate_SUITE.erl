%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nl_escalate_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R7A/R8A/R9A/1
%%%
%%% @doc == Escalate to NL testcases. TC shall be runed on unsec board with RCS cxs.==
%%%
%%%
%%% @end

-module(nl_escalate_SUITE).
-author('etxmlar').
-vsn('/main/R3A/R4A/R7A/R8A/R9A/1').
-date('2017-04-21').

%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% R3A/1      2015-02-24 etxivri     Created
%%% R3A/2      2015-03-03 etxivri     Update for https
%%% R3A/5      2015-03-05 etxivri     Add request_download_pogress
%%% R3A/6      2015-05-06 etxivri     Minor updates.
%%% R3A/7      2015-05-07 etxivri     Minor edoc fix.
%%% R3A/8      2015-06-03 etxivri     Add new TC. And update to use lib
%%% R3A/9      2015-06-09 etxivri     Update search string when boot from sda1.
%%% R4A/1      2015-07-02 etxivri     Update due to new behaviour. 
%%%                                   Some tests on TCU04 is skiped due to fail.
%%% R4A/2      2015-07-02 etxivri     Removed skip test if tcu04 is used.
%%% R4A/4      2015-10-29 etxmlar     Update to handle warm restart for TCU.
%%% R4A/5      2015-11-19 etxmlar     Update to handle warm restart for DUS.
%%% R4A/6      2015-12-09 etxmlar     Moved timer to make it more robust
%%% R4A/7      2016-02-19 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%% R7A/1      2016-10-24 etxmlar     Update if nl on dus/tcu is to old 
%%% R7A/2      2016-11-07 etxmlar     Update to be able to run with MSRBS UP
%%% R7A/3      2017-01-11 etxmlar     Update the check, if nl is to old  
%%% R7A/4      2017-01-18 etxmlar     Added rpc call because changes in SWM/Backups.
%%%                                   rct_rpc:call(rpc, appmServer, set_revert_state, 
%%%                                   [before_ug], 10000)
%%% R8A/1      2017-01-18 etxmlar     Added R7A/4 to 17B
%%% R9A/1      2017-04-21 etxmlar     R7 NL can install X3 UPs from R9A
%%%                                   as soon as boot files are signed so now
%%%                                   inhibit hardfactory reset for this case
%%% ----------------------------------------------------------
%%%

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 groups/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 wait_for_appl_started/2,
	 wait_for_appl_started/3,
	 %% wait_for_com_started/0,
	 wait_for_netconf_started/0,
	 all/0,
	 board_restore_with_faulty_sda2/1,
	 kill_appl_escalate_to_revert/1,
	 export_ai_log/1,
	 download_files/1,
	 download_files_after_faulty_sda2/1,
	 integrate/1
	]).

-define(TEST_APP, "test_app").
-define(TEST_OI, "test_oi").
-define(IFT_APP, "ift_app").
-define(TEST_APPS,[?TEST_APP,?TEST_OI,?IFT_APP]).


-define(APP4, "restart"). %% RCS CXS APP
-define(APP5, "frumLm").  %% MSRBS APP
-define(NC, nc1).
%% -define(Protocol, "http").
%% -define(Port, "8080").
-define(Protocol, "https").
%% -define(Port, "443"). %% Not needed
%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    case nl_lib:check_if_vc_board() of
	"yes" -> 
	    [{timetrap,{minutes,60}},
	     {ct_hooks, [{rct_htmllink,[]},
			 {rct_power,power},
			 {rct_consserv,cs1},
			 {rct_netconf, {nc1, man_auth}},
			 {rct_ssh,{ssh,[manual_connect]}},
			 {rct_rs232,console},
			 {cth_conn_log,[]}]}];
	_Other ->
	    [{timetrap, {minutes, 600}}, % 10 hours
	     {ct_hooks, [{rct_rpc, rpc},
			 {rct_htmllink,[]},
			 {rct_consserv,cs1},
			 {rct_rs232,console},
			 {rct_power,node},
			 {rct_netconf, nc1},
			 {cth_conn_log, []},
			 %% {rct_logging, {all,
			 %% 		[{erlang,
			 %% 		  {["ERROR REPORT","CRASH REPORT"],
			 %% 		   ["Program ID [0-9]+ has crashed",
			 %% 		    "Program ID [0-9]+ has terminated",
			 %% 		    "has failed to start after 3 attempts within 300 seconds"]}
			 %% 		 }]}},
			 %% {rct_core,[]}
			 {rct_cli, {cli, [manual_connect]}}
			]}]
    end.


%% @hidden
init_per_suite(Config) ->

    HW = atom_to_list(ct:get_config({test_nodes,1})),
    TftpBootDir = "/proj/rcs-tmp/tftpboot/"++HW++"/",

    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:log("ls : ~p ", [Ls]),

    [{tftpboot_dir, TftpBootDir},
     {hw, HW}| Config].

%% @hidden
end_per_suite(_Config) ->
    %% Set rollback  to default 3600 sec
    rct_rpc:call(rpc, appmServer, set_rollback_time, [3600], 10000),
    ok.
%% @hidden
init_per_group(GroupName, Config) ->
    case GroupName of
	GroupName when GroupName == d_1_1;
		       GroupName == d_2_1 ->
	    case aic_httpc:check_if_vc_board() of
		"yes" -> ct:comment("Not valid for secure board"),
			 ct:fail("TC shall not be runed on secured board.");
		_Other ->ok
	    end;
	_Other -> ok
    end,
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

%% @hidden
init_per_testcase(kill_appl_escalate_to_revert = TC, Config) ->
    %% Set rollback time to 60 sec, default is 3600 sec
    ct:pal("TC: ~p",[TC]),
    case aic_httpc:check_if_vc_board() of
	"yes" ->
	    ct:comment("Not valid for secure board"),
	    ct:fail("TC shall not be runed on secured board.");
	_Other ->
	    ok = rct_rpc:call(rpc, appmServer, set_rollback_time, [60], 10000),
	    ok = rct_rpc:call(rpc, appmServer, set_revert_state, [before_ug], 10000)
    end,
    Config;
init_per_testcase(board_restore_with_faulty_sda2 = TC, Config) ->
    ct:pal("TC: ~p",[TC]),
    case aic_httpc:check_if_vc_board() of
	"yes" ->
	    ct:comment("Not valid for secure board"),
	    ct:fail("TC shall not be runed on secured board.");
	_Other ->
	    ok
    end,
    Config;

init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, Config) ->
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p. Export logs. \n", [Reason]),
	    aic_httpc:export_ai_log(Config),
	    aic_httpc:export_esi(Config)
    end,
    ok.

string_token(Str) ->
    ListOfStr = string:tokens(Str, "\n"),
    ListOfStr.
%%--------------------------------------------------------------------
%% @doc
%% groups.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [
     {d_1_1, [], [board_restore_with_faulty_sda2,
		  export_ai_log,
		  download_files_after_faulty_sda2,
		  integrate
		 ]},
     {d_2_1, [], [kill_appl_escalate_to_revert,
		  export_ai_log,
		  download_files,
		  integrate
		 ]}
    ].

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
%% @spec board_restore_with_faulty_sda2(Config) -> ok
%% @end
%%--------------------------------------------------------------------
board_restore_with_faulty_sda2(Config) ->
    ct:pal("D_1_1: Board restore with fault on sda2. "
	   "Then sda1 NL -> download -> integrate."),


   case is_hard_factory_reset_ok() of
        false ->
            N = length(ct:get_config(test_nodes)),
            Hwa = ct:get_config({test_nodes,N}),
            BoardType = ct:get_config({Hwa,board_type}),
            Prodno = ct:get_config({Hwa, product_no}),
            case is_r_state(Prodno) of
                false ->
                    ct:pal("Notice, this board seems to be a P revision (~p) X3 board (~p) ~n"
                           " => this board most likely dont support to install R9A bootfiles (signed)~n"
                           " => so performing BoardRestore instead of hardFactoryReset", [Prodno, BoardType]),
                    aic_httpc:board_restore(Config, console),
                    ok;
                true ->
		    make_sda2_faulty(console),
		    aic_httpc:board_restore(Config, console),
		    check_type2_booted_from_sda1(console),
                    ok
            end;
       true ->
	   make_sda2_faulty(console),
	   aic_httpc:board_restore(Config, console),
	   check_type2_booted_from_sda1(console),
	   ok
   end.

is_hard_factory_reset_ok() ->
    N = length(ct:get_config(test_nodes)),
    Hwa = ct:get_config({test_nodes,N}),
    BoardType = ct:get_config({Hwa,board_type}),
    Prodno = ct:get_config({Hwa, product_no}),
    is_hard_factory_reset_ok(BoardType, Prodno).
    
is_hard_factory_reset_ok(BoardType, Prodno) ->
    case {is_x3_board(BoardType), is_r_state(Prodno)} of
        {true, false} ->
            false;
        _ ->
            true
    end.

is_r_state(Prodno) ->
    case string:tokens(Prodno, "R") of
        [Prodno]  -> false;
        [_, _] -> true
    end.

-define(X3_BOARDS, ["dus5301","dus3301", "dus6303", "dus6502"]).
is_x3_board(BoardType) ->
    lists:member(BoardType, ?X3_BOARDS).


make_sda2_faulty(Console) ->
    ok = rct_rs232:login(console),
    A = ct_telnet:send(Console, "dd if=/dev/zero of=/dev/sda2 count=10 bs=1K"),
    ct:pal("A: ~p",[A]),
    %% {ok,_} = ct_telnet:expect(Console, "Failed to mount ext2 filesystem", 
    %% 			 [{timeout,60000}, no_prompt_check]),

    timer:sleep(10000),
    ok.

check_type2_booted_from_sda1(Console) ->
    ct_telnet:send(Console, "cat /nl/log/nl_log.1"),
    {ok, _} = ct_telnet:expect(Console,
			       "booted from partition /dev/sda1", 
			       [{timeout,300000},no_prompt_check]),

    ok.



%%--------------------------------------------------------------------
%% @doc
%% @spec kill_appl_escalate_to_revert(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
kill_appl_escalate_to_revert(Config) ->
    ct:pal("D_2_1: Kill application several times will result in "
	   "escalation to revert back to NL."),
    rct_rpc:call(rpc, sysInitApp, reset_warm_cnt, [], 10000),

    %% BoardType = proplists:get_value(
    %% 		  board_type,ct:get_config(
    %% 			       ct:get_config({test_nodes,1}))),

    %%Apps = get_apps(),
    Apps = 
	case is_rcs_up(Config) of
	    true ->
		ct:log("RCS UP."),
		get_apps();
	    false ->
		ct:log("MSRBS UP."),
		get_apps_msrbs()
	end,

    FoundApps = [A || {A,_} <-Apps],

    kill_and_wait(Config, FoundApps,"first"),

    kill_and_wait(Config, FoundApps,"second"),

    kill_and_wait(Config, FoundApps,"third_cwt"),

    kill_and_wait(Config, FoundApps,"fourth"),


    BoardType = proplists:get_value(
		  board_type,ct:get_config(
			       ct:get_config({test_nodes,1}))),
    ct:pal("BoardType: ~p", [BoardType]),
    
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ct:pal("Wait for networkloader prompt."),
    net_kernel:disconnect(ErlNode),

    ct_telnet:expect(console, "boot_count = 7, load network loader", 
		     [{timeout,300000}, no_prompt_check]),

    case ct_telnet:expect(console, "\\[networkloader\\]", 
			  [{timeout,120000}, no_prompt_check]) of
	{ok,_} ->
	    ok;
    	_Other ->
	    ok = ct_telnet:send(console, ""),
	    {ok, _} = 
		ct_telnet:expect(console, 
				 "\\[networkloader\\]", 
				 [{timeout,120000},no_prompt_check])
    end,

    timer:sleep(10000),
    ct:pal("Rcvd networkloader prompt."),
    timer:sleep(10000),

    %% test_server:break("A"),
    ok.

export_ai_log(Config) ->
    aic_httpc:export_ai_log(Config),
    ok.

download_files_after_faulty_sda2(Config)->
    download_after_hfr(Config).


download_files(Config) ->
    aic_httpc:download_files(Config, console),
    ok.

integrate(Config) ->
    aic_httpc:integrate(Config, console, ?NC),
    ok.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
kill_and_wait(Config, FoundApps,T) ->

    BoardType = proplists:get_value(
		  board_type,ct:get_config(
			       ct:get_config({test_nodes,1}))),
    ct:pal("BoardType: ~p", [BoardType]),

    ct:pal("Sleep a while before killing app ",[]),
    timer:sleep(60000),

    %% Apps = get_apps(),
    %% {?APP4,UnixPid} =  lists:keyfind(?APP4, 1, Apps),
    %%ct:pal("Sleep a while before killing app ",[]),
    %%timer:sleep(60000),

    case is_rcs_up(Config) of
	true ->
	    ct:log("RCS UP."),
	    Apps = get_apps(),
	    {?APP4,UnixPid} =  lists:keyfind(?APP4, 1, Apps);
	false ->
	    ct:log("MSRBS UP."),
	    Apps = get_apps_msrbs(),
	    {?APP5,UnixPid} =  lists:keyfind(?APP5, 1, Apps)
    end,

    ct:pal("Killing ~p ~p time",[UnixPid,T]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    rct_rpc:call(rpc, os,cmd,["kill  " ++ UnixPid],10000),
    net_kernel:disconnect(ErlNode),
    case T of
	%% "first" -> %only warm restart
	%%     wait_for_appl_started(FoundApps); % wait for all apps to start
	%% %% This results in node restart. warm restart not suported in R3.
	"first" -> %only warm restart
	    %% case BoardType of
	    %% 	BoardType when BoardType == "tcu03";
	    %% 		       BoardType == "tcu0401" ->
	    %% 	    ok;
	    %% 	_Other ->
	    %% 	    node_restarts()
	    %% end,
	    wait_for_appl_started(FoundApps, ErlNode, 300000);

	"third_cwt" -> 
	    check_cwt_results_in_two_restarts(),
	    net_kernel:disconnect(ErlNode),
	    wait_for_appl_started(FoundApps, ErlNode, 600000);
	"fourth" -> %revert started
	    timer:sleep(5000);
	_ -> 
	    node_restarts(),
	    wait_for_appl_started(FoundApps, ErlNode, 300000)
    end.


%% --------------------------
%% Internal functions
%% --------------------------
get_apps() ->
    get_apps(10).
get_apps(Cnt) ->
    Apps = rct_rpc:call(rpc, appmServer, get_apps, [], 10000),
    case {Cnt, catch lists:keyfind(undefined,2,Apps)} of
	{_,false} when is_list(Apps) andalso length(Apps) > 0 ->
	    Apps;
	{0,UndefApps} ->
	    ct:fail("Apps still undefined: ~p, giving up",[UndefApps]),
	    Apps;
	{Cnt,UndefApps} ->
	    ct:pal("Apps still undefined: ~p, wait a while",[UndefApps]),
	    timer:sleep(1000),
	    get_apps(Cnt-1)
    end.


node_restarts() ->
    {ok,_} =  ct_telnet:expect(console, "Ericsson Version: ",
			      [{timeout,60000}, no_prompt_check]), 
    case  ct_telnet:expect(console, "login",
			   [{timeout,40000}, no_prompt_check]) of
	{ok,_} ->
	    ok;
	_Other ->
	    ct_telnet:send(console, ""),
	    {ok,_} = ct_telnet:expect(console, "login",
				      [{timeout,30000}, no_prompt_check])
    end.

%% ===========================================================================
%% @doc
%% Check for Application to be started. <br/>
%% @spec wait_for_appl_started(Apps, ErlNode) -> {ok,Pids} | {error,Reason}
%% @end
%% ===========================================================================
%% wait_for_appl_started(Apps) ->
%%     wait_for_appl_started(Apps, dummy).
wait_for_appl_started(Apps, ErlNode) ->
    wait_for_appl_started(Apps, ErlNode, 120000).

wait_for_appl_started(_, _ErlNode,Timeout) when Timeout < 500 ->
    ct:fail("No Appl started within max timeout.");

wait_for_appl_started(Apps, ErlNode,Timeout) ->
    case rct_rpc:call(rpc, appmServer, get_apps, [], 10000) of
	[] ->
	    net_kernel:disconnect(ErlNode),
	    timer:sleep(5000),
	    wait_for_appl_started(Apps, ErlNode, Timeout - 5000);
    	{badrpc, _} ->
	    net_kernel:disconnect(ErlNode),
	    timer:sleep(5000),
	    wait_for_appl_started(Apps, ErlNode, Timeout - 5000);
	AppProplist ->
	    net_kernel:disconnect(ErlNode),
	    ct:log("AppProplist: ~p",[AppProplist]),
	    FoundApps = [A || {A,_} <-AppProplist],
	    ct:log("FoundApps: ~p",[FoundApps]),
	    ct:log("Apps: ~p",[Apps]),
	    case Apps -- FoundApps of
		[] ->
		    Pids = [P || {N1,P} <-AppProplist,
				 N2 <- Apps,
				 N1 == N2],
		    ct:log("WaitPids: ~p",[Apps]),
		    {ok,Pids};
		_Res ->
		    %% {error,not_all_found}
		    ct:pal("Nol all expecting apps exist, wait and get apps again.",[]),
		    timer:sleep(5000),
		    wait_for_appl_started(Apps, ErlNode,Timeout - 5000)
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
    ct:pal("### Check Netconf",[]),
    wait_for_netconf_started(180000).

wait_for_netconf_started(Timeout) when Timeout < 500 ->
    ct:fail("Netconf not started within max timeout.");

wait_for_netconf_started(Timeout) ->
    case ct_netconfc:open(nc1,[]) of
	{ok,_} ->
	    {ok,_} = 
		ct_netconfc:get_config(nc1,running,
				       {'ManagedElement',
					[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
					[{managedElementId,[],["1"]}]}),
	    ok = ct_netconfc:close_session(nc1),
	    ok;
	Res  ->
	    ct:log("Res: ~p", [Res]),
	    timer:sleep(5000),
	    wait_for_netconf_started(Timeout - 5000)
    end.


%% %% ===========================================================================
%% %% @doc
%% %% Wait for new beam pid. <br/>
%% %% @end
%% %% ===========================================================================
%% wait_for_new_beam_pid(OldBeamPid) ->
%%     ct:pal("Wait for new beam pid, OldPid=~p", [OldBeamPid]),
%%     wait_for_new_beam_pid(OldBeamPid, dummy).
%% wait_for_new_beam_pid(OldBeamPid, ErlNode) ->
%%     ct:pal("Wait for new beam pid, OldPid=~p", [OldBeamPid]),
%%     wait_for_new_beam_pid(OldBeamPid, ErlNode, 600000).

%% wait_for_new_beam_pid(_OldBeamPid, _ErlNode, Timeout) when Timeout < 500 ->
%%     ct:fail("New Beam Pid not recived within expected time!");

%% wait_for_new_beam_pid(OldBeamPid, ErlNode, Timeout) ->
%%     case rct_rpc:call(rpc, os, getpid, [], 10000) of
%% 	{badrpc,_Cause} ->
%% 	    net_kernel:disconnect(ErlNode),
%% 	    ct:log("{badrpc,~p}", [_Cause]),
%% 	    timer:sleep(10000),
%% 	    wait_for_new_beam_pid(OldBeamPid, ErlNode, Timeout - 10000);
%% 	Res  ->
%% 	    ct:pal("#### New Beam ~p", [Res]),
%% 	    Res
%%     end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %% Check two restarts. This check is made robust.
check_cwt_results_in_two_restarts() ->
    ct_telnet:expect(console, "Ericsson",
		     [{timeout,30000}, no_prompt_check]), 
    case  ct_telnet:expect(console, "login:",
			   [{timeout,30000}, no_prompt_check]) of
	{ok,_} -> 
	    ct:pal("# First restart done. Wait for another restart."),
	    ok;
	_ ->
	    ct_telnet:send(console, ""),
	    ct_telnet:expect(console, "login:",
			     [{timeout,30000}, no_prompt_check]) 
    end,
    
    ct_telnet:expect(console, "Ericsson",
		     [{timeout,120000}, no_prompt_check]), 
    case  ct_telnet:expect(console, "login:",
			   [{timeout,120000}, no_prompt_check]) of
	{ok,_} -> 
	    ct:log("## Second and last restart done as expected."),
	    ok;
	_ ->
	    ct_telnet:send(console, ""),
	    ct_telnet:expect(console, "login:",
			     [{timeout,60000}, no_prompt_check]) 
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
download_after_hfr(Config) ->
    Console = console,
    N = length(ct:get_config(test_nodes)),
    Hwa = ct:get_config({test_nodes,N}),
    ct:log("# Hwa: ~p", [Hwa]),
    BoardType = ct:get_config({Hwa,board_type}),
    ct:pal("# BoardType: ~p", [BoardType]),
    case {aic_httpc:check_if_vc_board(), 
	  is_hard_factory_reset_ok(), 
	  is_x3_board(BoardType)} of
	{"yes", _, _} -> 
	    download_files(Config);
	{_, false, _} ->
	    download_files(Config);
	{_, _, true} ->
	    %%No need to check NL version on dusX3 boards
	    download_files(Config);
	_Other ->
	    %% Could only be runed on unsec.
	    ct:pal("check rev on nl type2"),
	    NlStatus = get_rev_on_nl_type2(Config, Console),
	    case NlStatus of
		nl_is_ok ->
		    download_files(Config);
		{to_old_nl, NLUpgradeFile} ->
		    ct:pal("Do NL upgrade before install."),
		    aic_httpc:nl_upgrade(Config, Console, NLUpgradeFile),
		    ct:pal("Install SW."),
		    download_files(Config)
	    end
    end,
    ok.


get_rev_on_nl_type2(Config, Console) -> 
    ct:pal("cat nl_log"),
    %% ok = ct_telnet:send(console, "cat /rcs/networkloader/nl_log.1"),
    ok = ct_telnet:send(console, "cat /nl/log/nl_log.1 "),	    
    {ok, _} = ct_telnet:expect(console,
			       "Running version:", 
			       [{timeout,30000},no_prompt_check]),

    %% Get NL version on node
    NodeNLVersion = get_nl_version_node(Config),
    ct:log("Network Loader Version on Node: ~p", [NodeNLVersion]),


    NlStatus =
	%% first check for Multi NL support
	case support_multi_nl() of
	    true ->
		ct:pal("grep for Networkloader type2 in nl_log"),
		ok = ct_telnet:send(console, "grep 'Networkloader type2' /nl/log/nl_log.1"),
		{ok, _} = ct_telnet:expect(Console,
					   "Networkloader type2 booted from partition /dev/sda1", 
					   [{timeout,5000},no_prompt_check]),

		NLVerNotOk = "CNX9012629-R4F10", %%TR HU14731

		case do_nl_upgrade(NodeNLVersion, NLVerNotOk) of
		    false ->
			ct:pal("The NL on sda1 is to old."
			       " NL can not handle large lkf/config files."),
			%% After NL version CNX9012629-R4B18 nl upgrade is done 
			%% with UP not with the CXP
			NLUpgradeFile = 
			    get_what_nl_upgrade_file_to_use(NodeNLVersion, "CNX9012629-R4B18"),
			{to_old_nl, NLUpgradeFile};
		    true ->
			ct:pal("The NL on sda1 is OK. NL ug supported."),
			nl_is_ok
		end;	 

	    false ->
		ct:pal("The NL on sda1 is to old."
		       " NL Upgrade is not supported."),
		%% After NL version CNX9012629-R4B18 nl upgrade is done with UP not with the CXP
		NLUpgradeFile = get_what_nl_upgrade_file_to_use(NodeNLVersion, "CNX9012629-R4B18"),
		{to_old_nl, NLUpgradeFile}
	end,

    ct:pal("NlStatus : ~p", [NlStatus]),
    NlStatus.

%% Run on MSRBS UP
get_apps_msrbs()->
    get_apps_msrbs(10).

get_apps_msrbs(Cnt) ->
    Apps = rct_rpc:call(rpc, appmServer, get_apps, [], 10000),
    case {Cnt, catch lists:keyfind(undefined,2,Apps)} of
	{_,false} when is_list(Apps) andalso length(Apps) > 0 ->
	    Apps;
	{0,UndefApps} ->
	    ct:pal("OK: Apps still undefined for MSRBS UP: ~p, giving up",[UndefApps]),
	    Apps;
	{Cnt,UndefApps} ->
	    ct:pal("Apps still undefined: ~p, wait a while",[UndefApps]),
	    timer:sleep(1000),
	    get_apps_msrbs(Cnt-1)
    end.

is_rcs_up(Config)->

    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    FileList = filelib:wildcard(TftpBootDir++"*_CXS101549_*"),

    case  FileList of 
	[] -> 
	    false; 
	_Else -> 
	    true
    end.


%%--------------------------------------------------------------------
support_multi_nl() ->

    Cmd = "ls /proc/device-tree/rcs_nodes/",
    ok = ct_telnet:send(console, Cmd),

    %% Ans = 
    %% 	ct_telnet:expect(console, "boot_nl3_partition",[{timeout, 30000}, no_prompt_check]),

    {ok, DataList} = ct_telnet:get_data(console),

    Ans = 
	case re:run(DataList, "boot_nl3_partition") of
	    {match, Value} ->
		ct:log("Value: ~p ~n",[Value]),
		ok;
	    nomatch ->
		nok
	end,

    Is_Multi_nl =
	case Ans of
	    ok -> 
		true;
	    nok -> 
		false
	end,

    ct:log("Is_Multi_nl: ~p", [Is_Multi_nl]),
    Is_Multi_nl.
%%--------------------------------------------------------------------
get_nl_version_node(_Config) ->
    HW = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    _BOARDTYPE = ct:get_config({Hwa,board_type}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),

    start_needed_applications(?Protocol),

    HttpsUrl= "https://"++IP++"/help.html",
    HttpUrl= "http://"++IP++":"++?Protocol++"/help.html",

    UrlRes =  
	case httpc:request(HttpsUrl) of
	    {ok,{{_,200,"OK"}, _ReplyHttpsA, ReplyHttpsB}} ->
		%%ct:log("ReplyHttpsB: ~p ~n",[ReplyHttpsB]),
		ReplyHttpsB;
	    {error, Reason} ->
		case httpc:request(HttpUrl) of
		    {ok,{{_,200,"OK"}, _ReplyHttpA, ReplyHttpB}} ->
			%%ct:log("ReplyHttpB: ~p ~n",[ReplyHttpB]),
			ReplyHttpB; 
		    {error, Reason} ->
			ct:fail("TC will fail due to unknown protocol.")
		end
	end,

    stop_needed_applications(?Protocol),

    UrlListRes = string:tokens(UrlRes, " \n\t"),  
    NLNodeVerStr = 
	lists:flatten([VersionStr||VersionStr<-UrlListRes, lists:prefix("CNX", VersionStr)]),

    %% NLNodeVerStr = "CNX9012629-R4AC07<p>"
    NLNodeVersionStr = lists:flatten(string:tokens(NLNodeVerStr, "<p>")),
    NLNodeVersionStr.
%%--------------------------------------------------------------------
do_nl_upgrade(UpNLVer, NodeNLVer)->

    %%NL Version in UP: "CNX9012629-R4AA02"
    %%NL on Node: "CNX9012629-R4U02

    [CNX, UpNLLabel] = string:tokens(UpNLVer, "/-"),         
    [CNX, NodeNLLabel] = string:tokens(NodeNLVer, "/-"),

    gt(UpNLLabel, NodeNLLabel).

%%--------------------------

gt(A,B) ->
    {N1,L1,M1} = split_version(string:to_lower(A)),
    {N2,L2,M2} = split_version(string:to_lower(B)),

    (gt_(N1, N2)) orelse
    (eq_(N1, N2) andalso gt_(L1, L2)) orelse
    (eq_(N1, N2) andalso eq_(L1, L2) andalso gt_(M1, M2)).


gt_(A,B) when length(A) == length(B) -> A > B;
gt_(A,B) when length(A) > length(B) -> true;
gt_(_A,_B) -> false.

eq_(A,B) -> A == B.

%% split_version("r1234abcde5678")->{1234, "abcde", 5678}
split_version([$r | NLM]) -> 
    {N, LM} = consume(NLM, integers),
    {L, M} = consume(LM, lc_letters),
    case consume(M, integers) of
	{M, _} ->
	    %% Normal
	    ok;
	{_Real_m, Extra} ->
	    ct:log("split_version contains extra info ~p - ignoring~n",[Extra]),
	    ok
    end,
    {N, L, M}.

%% --------------------------------------------------------------------------
%% consume(S, Type) ->
%% 			{Hit, Res} | {error, version_parse_error}
%% 			 
%%	where
%%		S = string()
%%		Type = Must be defined in regexp(Type) -> regexp()
%%		Hit = Consecutive pattern found in S matching Type
%%		Rest = The rest of the string so that Hit ++ Rest == S.
%%      
%% --------------------------------------------------------------------------
consume(S, Type) ->
    case re:run(S, regexp(Type)) of
	{match, [{0, No_hits_in_a_row}]} ->
	    Hit = string:substr(S, 1, No_hits_in_a_row),
	    Rest = string:substr(S, No_hits_in_a_row+1),
	    {Hit, Rest};
	No_correct_match ->
	    ct:log("Incorrect int match ~p in version string ~p to pattern ~p~n", 
		   [No_correct_match, S, regexp(Type)]),
	    {error, version_parse_error}
    end.

regexp(integers) -> "[0-9]+";
regexp(lc_letters) -> "[a-z]+".


%%--------------------------------------------------------------------
get_what_nl_upgrade_file_to_use(NodeNLVersion, NLVerNotOk)->

    %%NL Version in UP: "CNX9012629-R4AA02"
    %%NL on Node: "CNX9012629-R4U02

    [CNX, UpNLLabel] = string:tokens(NodeNLVersion, "/-"),         
    [CNX, NodeNLLabel] = string:tokens(NLVerNotOk, "/-"),

    case gt(UpNLLabel, NodeNLLabel) of
	false ->
	    ct:pal("The NL on sda1 supports NL upgrade with cxp. Use RbsSummaryFile.xml.nl15B"),
	    "RbsSummaryFile.xml.nl15B";
	true ->
	    ct:pal("The NL on sda1 supports NL upgrade with UP. Use RbsSummaryFile.xml.nl"),
	    "RbsSummaryFile.xml.nl"
    end.	 

%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start_needed_applications(Protocol) ->	
    inets:start(), %% För httpc
    case Protocol of
	"https" ->
	    crypto:start(),
	    ssl:start();
	_Other ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
stop_needed_applications(Protocol) ->	
    inets:stop(), %% För httpc
    case Protocol of
	"https" ->
	    crypto:stop(),
	    ssl:stop();
	_Other ->
	    ok
    end.
