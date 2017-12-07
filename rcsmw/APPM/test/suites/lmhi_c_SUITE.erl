%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmhi_c_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R8A/R12A/1
%%%
%%% @doc ==Basic Test Suite for the LMHI interface on SUT.==
%%% This Test Suite can be used on both target and simulated enviroment.
%%% <br/><br/>
%%% rct_proxy is used in ct_hooks:
%%% see [https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/edoc/rct_proxy.html]
%%% <br/>
%%%
%%%
%%% @end

-module(lmhi_c_SUITE).
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% ----------------------------------------------------------
%%% R2A/1      2013-04-19 etxpeno     Created
%%% R2A/2      2013-11-23 erarafo     Interference w other dyn pgms resolved
%%% R2A/3      2013-11-29 etxarnu     Added lmhi_start_stop_many_pgms/1
%%% R3A/2      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R4A/1      2015-10-08 etxpejn     Cluster adaptions
%%% R4A/2      2016-02-12 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%% R5A/1      2016-03-06 etxarnu     Handle more rt_app files
%%% R5A/2      2016-03-06 etxarnu     Handle more rt_app files
%%% R6A/1      2016-08-12 etxberb     Added tests in lmhi_get_lms/1 for the new
%%%                                   API function Lmhi_get_lms_3.
%%% R12A/1     2017-10-23 etxkols     Adapt to cloud
%%% ----------------------------------------------------------

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         init_per_group/2,
         end_per_group/2,
         groups/0,
	 all/0
        ]).

-export([
	 lmhi_get_lms/1,
	 lmhi_start_stop_pgm_on_own_du/1,
	 lmhi_start_stop_pgm_on_other_du/1,
	 lmhi_start_crash_stop_pgm/1,
	 lmhi_start_stop_many_pgms_on_own_du/1,
	 lmhi_start_stop_many_pgms_on_other_du/1
	]).

-include_lib("common_test/include/ct.hrl").
-include("test_lmhi.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Returns list of tuples to set default properties
%%  for the suite.
%%
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%
%% @spec suite() -> Info
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 300}},
     {ct_hooks, [{rct_rpc, rpc},
                 {rct_htmllink,[]},
                 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
					       ["Program ID [0-9]+ has crashed"]
                                              }}]}},
                 {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]},
		 {cth_conn_log, []},
                 {rct_core,[]}
                ]}].

%% @hidden
init_per_suite(Config) ->
    {ok, client_started} = rct_proxy:start_proxy(node1, lmhi1, ?LMHI),
    Config.

%% @hidden
end_per_suite(_Config) ->
    {ok, client_stopped} = rct_proxy:stop_proxy(node1, lmhi1),
    ok = rct_proxy:exit_master(node1).

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

%% @hidden
groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []},
     %% This suite can be run on both MPs within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]},
     {sbc__cluster__dual_sim_3__1__group, [], [{group, default__group}]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     lmhi_get_lms,
     lmhi_start_stop_pgm_on_own_du,
     lmhi_start_stop_pgm_on_other_du,
     lmhi_start_stop_many_pgms_on_own_du,
     lmhi_start_stop_many_pgms_on_other_du
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%
%% @lmhi_get_lms(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
lmhi_get_lms(_Config) ->
    %% See $RCT_TOP/FAKE/FAKE_CNX9012695/FAKE_CAX1033161/appdata/lmlist_sim.xml
    %% for configured LM Lists.
    IsSsit = rct_rpc:call(rpc, sysEnv, ssit, [], 10000),
    Args_ExpResults = lmhi_get_lms_TestCases(IsSsit),
    Cnt_Args_ExpResults = add_cnt(Args_ExpResults),
    [begin
	 Result = rct_proxy:send_proxy(node1, lmhi1, ?Lmhi_get_lms, Arg),
	 ct:log("Test " ++ Cnt ++ ": ~p" ++ " ->" ++ "~n~p", [Arg, Result]),
	 ok = validate_get_lms(ExpRes, Result)
     end
     || {Cnt, Arg, ExpRes} <- Cnt_Args_ExpResults],

    ct:pal("~p different combinations passed.", [length(Cnt_Args_ExpResults)]),

    rct_proxy:send_proxy(node1, lmhi1, ?Lmhi_free_buffers, {}),
    ok.

%%--------------------------------------------------------------------
lmhi_get_lms_TestCases(_IsSsit = true) ->
    [
%%% Phase /= NORMAL:
%%%	 {{"", "", "dynamic", 1},   % TODO: Should give error (wrong phase)
%%%	  {error, zero_length}},
%%%	 {{"sim", "", "dynamic", 1},   % TODO: Should give error (wrong phase)
%%%	  {error, zero_length}},
%%%	 {{"", "", "dynamic", 7},   % TODO: Should give error (wrong phase)
%%%	  {error, zero_length}},
%%% --- 1 ---
	 {{""},
	  {error, {badarg, arity}}},
	 {{"", "undefined tag"},
	  {error, zero_length}},
	 {{"", "dynamic"},
	  {ok, "rt_app"}},
	 {{"", "", "dynamic", 0},
	  {ok, "rt_app"}},
%%% --- 5 ---
	 {{"sim", "", "dynamic", 0},
	  {ok, "rt_app"}},
	 {{"", "", "", 0},
	  {error, zero_length}},
	 {{"", "", "", 1},
	  {error, zero_length}},
	 {{"", "", "", "", "", 1},   % Not wild card on Tag. Tag is unknown
	  {error, zero_length}},
	 {{"", "", "", "", "central", 0},   % "sim" is default boardType!
	  {ok, "test_app"}},
%%% --- 10 ---
	 {{"", "", "sim", "", "central", 0},
	  {ok, "test_app"}},
	 {{"", "", "sim", "", "local", 0},
	  {ok, "ift_app"}},
	 {{"", "", "sim", "", "dynamic", 0},
	  {ok, "rt_app"}},
	 {{"BASEBALL", "BB1234", "", "", "dynamic", 0},
	  {ok, "rt_app"}},
	 {{"BASEBALL", "", "", "", "dynamic", 0},
	  {ok, "rt_app"}},
%%% --- 15 ---
	 {{"BASEBALL", "", "sim", "", "dynamic", 0},
	  {ok, "rt_app"}},
	 {{"BASEBALL-T", "", "", "", "central", 0},
	  {ok, "test_app"}},
	 {{"BASEBALL-T", "", "", "", "local", 0},
	  {ok, "ift_app"}},
	 {{"BASEBALL-T", "T500", "", "", "central", 0},
	  {ok, "test_app"}},
	 {{"BASEBALL-T", "T500", "", "", "local", 0},
	  {error, zero_length}},
%%% --- 20 ---
	 {{"BASEBALL-T", "T555", "", "", "local", 0},
	  {ok, "ift_app"}},
	 {{"BASEBALL-T", "T555", "", "", "dynamic", 0},
	  {ok, "rt_app"}}
	];
lmhi_get_lms_TestCases(_IsSsit = false) ->
    [
     {{"", "undefined tag"},
      {error, zero_length}},
     {{"boardtype", "undefined tag"},
      {error, zero_length}},
     {{"", "dynamic"},
      {ok, "rt_app"}}
    ].

%%--------------------------------------------------------------------
validate_get_lms({error, _} = Match, Match) ->
    ok;
validate_get_lms({ok, FirstElement} = ExpRes, {ok, LMs}) ->
    case lists:keyfind(FirstElement, 1, LMs) of
	false ->
	    {nok, {"Expected result:", ExpRes}};
	_ ->
	    ok
    end;
validate_get_lms(ExpRes, _) ->
    {nok, {"Expected result:", ExpRes}}.

%%--------------------------------------------------------------------
add_cnt(List) ->
    add_cnt(List, 1).

add_cnt([{E1, E2} | Tail], Cnt) ->
    [{integer_to_list(Cnt), E1, E2} | add_cnt(Tail, Cnt + 1)];
add_cnt([], _) ->
    [].

%%--------------------------------------------------------------------
%% @doc
%%
%% @lmhi_start_stop_pgm(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
lmhi_start_stop_pgm_on_own_du(_Config) ->
    OwnDuId = rct_rpc:call(rpc, clhI, own_mp_id, [], 10000),
    CpuSet = get_cpuset(),
    Args = [],

    {ok, DynPgms} =
	rct_proxy:send_proxy(node1, lmhi1, ?Lmhi_get_lms, {"", "dynamic"}),
    
    {PgmName, LmId, _, _} = lists:keyfind("rt_app", 1, DynPgms),
    
    rct_proxy:send_proxy(node1, lmhi1, ?Lmhi_free_buffers, {}),
    
    AppmId = start_dynamic(OwnDuId, CpuSet, LmId, PgmName, Args),

    stop_dynamic(OwnDuId,AppmId),

    ok.

lmhi_start_stop_pgm_on_other_du(_Config) ->
    OwnDuId = rct_rpc:call(rpc, clhI, own_mp_id, [], 10000),
    case rct_rpc:call(rpc, clhI, mp_id, [all], 10000) of
	[OwnDuId] ->
	    %% Signle DU, nothing to test
	    ct:pal("No cluster configuration on this node, skip this tc"),
	    ok;
	AllMpIdList ->
	    [OtherMpId] = AllMpIdList -- [OwnDuId],
	    CpuSet = get_cpuset(),
	    
	    Args = [],

	    {ok, DynPgms} =
		rct_proxy:send_proxy(node1, lmhi1, ?Lmhi_get_lms, {"", "dynamic"}),
	    
	    {PgmName, LmId, _, _} = lists:keyfind("rt_app", 1, DynPgms),
    
	    rct_proxy:send_proxy(node1, lmhi1, ?Lmhi_free_buffers, {}),
    
	    AppmId = start_dynamic(OtherMpId, CpuSet, LmId, PgmName, Args),

	    stop_dynamic(OtherMpId, AppmId),
	    
	    ok
    end.

lmhi_start_crash_stop_pgm(_Config) ->
    DuId = 0,
    CpuSet = get_cpuset(),
    Args = [],
    APP="rt_app",

    {ok, DynPgms} =
	rct_proxy:send_proxy(node1, lmhi1, ?Lmhi_get_lms, {"", "dynamic"}),
    
    {PgmName, LmId, _, _} = lists:keyfind(APP, 1, DynPgms),
    
    rct_proxy:send_proxy(node1, lmhi1, ?Lmhi_free_buffers, {}),
    
    AppmId = start_dynamic(DuId, CpuSet, LmId, PgmName, Args),
    [UnixPid] = get_dynamic_pid(),
    rct_rpc:call(rpc, os,cmd,["kill -ABRT " ++ UnixPid],10000),
    timer:sleep(5000),
    wait_for_app_restarted(APP,UnixPid),
    stop_dynamic(DuId,AppmId),

    ok.

lmhi_start_stop_many_pgms_on_own_du(_Config) ->
    OwnDuId = rct_rpc:call(rpc, clhI, own_mp_id, [], 10000),
    CpuSet = get_cpuset(),
    Instances = lists:seq(1,4),
    {ok, DynPgms} =
	rct_proxy:send_proxy(node1, lmhi1, ?Lmhi_get_lms, {"", "dynamic"}),
    
    {PgmName, LmId, _, _} = lists:keyfind("rt_app", 1, DynPgms),
    
    rct_proxy:send_proxy(node1, lmhi1, ?Lmhi_free_buffers, {}),
    AppmIds = [start_dynamic(OwnDuId, CpuSet, LmId, PgmName, [Inst]) || Inst <- Instances],
    StartedAppmIds = get_dynamic(),
    case AppmIds -- StartedAppmIds of
	[] ->
	    ct:pal("Dynamic programs have been started correctly: ~p~n",[AppmIds]);
	_ ->
	    ct:fail("Dynamic programs have NOT been started correctly: ~p~n",[StartedAppmIds])
    end,
    
    [stop_dynamic(OwnDuId, AppmId)  || AppmId <- AppmIds],
    ok.

lmhi_start_stop_many_pgms_on_other_du(_Config) ->
    OwnDuId = rct_rpc:call(rpc, clhI, own_mp_id, [], 10000),
    case rct_rpc:call(rpc, clhI, mp_id, [all], 10000) of
	[OwnDuId] ->
	    %% Signle DU, nothing to test
	    ct:pal("No cluster configuration on this node, skip this tc"),
	    ok;
	AllMpIdList ->
	    [OtherMpId] = AllMpIdList -- [OwnDuId],
	    OtherErlangNode = rct_rpc:call(rpc, clhI, erlang_node, [OtherMpId], 10000),
	    CpuSet = get_cpuset(),
	    Instances = lists:seq(1,4),
	    {ok, DynPgms} =
		rct_proxy:send_proxy(node1, lmhi1, ?Lmhi_get_lms, {"", "dynamic"}),
    
	    {PgmName, LmId, _, _} = lists:keyfind("rt_app", 1, DynPgms),
    
	    rct_proxy:send_proxy(node1, lmhi1, ?Lmhi_free_buffers, {}),
	    AppmIds = [start_dynamic(OtherMpId, CpuSet, LmId, PgmName, [Inst]) 
		       || Inst <- Instances],
	    StartedAppmIds = get_dynamic(OtherErlangNode),
	    case AppmIds -- StartedAppmIds of
		[] ->
		    ct:pal("Dynamic programs have been started correctly: ~p~n",[AppmIds]);
		_ ->
		    ct:fail("Dynamic programs have NOT been started correctly: ~p~n",
			    [StartedAppmIds])
	    end,
    
	    [stop_dynamic(OtherMpId, AppmId)  || AppmId <- AppmIds],
	    ok
    end.


start_dynamic(DuId, CpuSet, LmId, PgmName, Args) ->
    {ok, AppmId} = rct_proxy:send_proxy(node1, lmhi1, ?Lmhi_start_pgm,
				       {DuId, CpuSet, LmId, PgmName, Args}),
    AppmId.

stop_dynamic(DuId,AppmId) ->
    {ok} = rct_proxy:send_proxy(node1, lmhi1, ?Lmhi_stop_pgm, {DuId, AppmId}).
    

get_dynamic() ->
    Pgms = rct_rpc:call(rpc, appmServer, get_info, [pgms], 10000),
    [proplists:get_value(appmId,Pgm) || {{"rt_app",_},Pgm} <- Pgms].

get_dynamic(ErlangNode) ->
    Pgms = rct_rpc:call(rpc, rpc, call, [ErlangNode, appmServer, get_info, [pgms]], 10000),
    ct:pal("Pgms: ~p", [Pgms]),
    [proplists:get_value(appmId,Pgm) || {{"rt_app",_},Pgm} <- Pgms].

get_dynamic_pid() ->
    Pgms = rct_rpc:call(rpc, appmServer, get_info, [pgms], 10000),
    [proplists:get_value(pid,Pgm) || {{"rt_app",_},Pgm} <- Pgms].


%%--------------------------------------------------------------------
%% @doc
%% Wait for a specific apllication to be restarted. <br/>
%% @spec wait_for_app_restarted(AppName, PidList) -> ok
%% @end
%%--------------------------------------------------------------------
wait_for_app_restarted(AppName, AppPid) ->
    wait_for_app_restarted(AppName, AppPid, 60000).
wait_for_app_restarted(_AppName, _AppPid, Timeout) when Timeout < 500 ->
    ct:fail("App has not restarted within expected time!!");
wait_for_app_restarted(AppName, AppPid, Timeout) -> 
    case rct_rpc:call(rpc, appmServer, get_apps, [], 250) of
	{badrpc,timeout} ->
	    ct:pal("{badrpc,timeout}, wait and try again", []),
	    timer:sleep(5000),
	    wait_for_app_restarted(AppName, AppPid, Timeout-5000);
	NewApplPidPropList ->
	    ct:log("NewApplPidPropList: ~p",[NewApplPidPropList]),
	    ct:log("AppName: ~p",[AppName]),
	    case proplists:get_value(AppName, NewApplPidPropList) of
		undefined ->
		    ct:pal("~p has not started, wait and try again", [AppName]),
		    timer:sleep(5000),
		    wait_for_app_restarted(AppName, AppPid, Timeout-5000);
		{badrpc,timeout} ->
		    ct:pal("{badrpc,timeout}, wait and try again", [AppName]),
		    timer:sleep(5000),
		    wait_for_app_restarted(AppName, AppPid, Timeout-5000);
		NewAppPid ->
		    case NewAppPid of
			AppPid ->
			    ct:pal("ift_app has not restarted, wait and try again", []),
			    timer:sleep(5000),
			    wait_for_app_restarted(AppName, AppPid, Timeout-5000);
			_Other ->
			    NewAppPid
		    end
	    end
    end.

get_cpuset() ->
    case os:getenv("SIM_OR_TARGET") of
	"cloudish" -> 
	    ct:log("Cloud detected, setting CpuSet = 0"),
	    0;
	_ -> 
	    ct:log("Target or old simulator detected, setting CpuSet = 3"),
	    3
    end.
