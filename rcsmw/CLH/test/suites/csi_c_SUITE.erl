%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	csi_c_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/R5A/1
%%%
%%% @doc ==Basic Test Suite for the CSI interface on SUT.==
%%% This Test Suite can be used on both target and simulated enviroment.
%%% <br/><br/>
%%% rct_proxy is used in ct_hooks:
%%% see [https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/edoc/rct_proxy.html]
%%% <br/>
%%%
%%%
%%% @end

-module(csi_c_SUITE).
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% R2A/3      2013-02-29 etxkols     Added rct_core hook
%%% R2A/4      2013-04-17 etxjovp     change timetrap to 30
%%% R4A/2      2015-05-19 etxberb     Added test cases for new functions.
%%%                                   Removed obsolete.
%%% R4A/3      2015-05-19 etxberb     Empty all/0. Delivery coordination tmp fix
%%% R4A/4      2015-05-19 etxberb     Tmp coordination fix restored.
%%% R4A/5      2015-05-21 etxberb     Changed getHuntPathPrefix_other.
%%% R4A/6      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R4A/8      2015-09-08 uabesvi     cluster restart
%%% R4A/10     2015-09-10 etxberb     edoc corrections.
%%% R4A/11     2015-09-16 etxpejn     Adaption to be able to run on both DUs.
%%% R5A/1      2016-02-12 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%% ----------------------------------------------------------

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 groups/0
	]).

-export([all/0,
	 subscribeCoreState_normal_unsubscribe/1,
	 subscribeCoreState_negative/1,
	 subscribeCoreState_terminate/1,
	 getOwnMpid/1,
	 getHuntPathPrefix_own/1,
	 getHuntPathPrefix_other/1,
	 subscribeClusterRestart_normal_unsubscribe/1,
	 subscribeClusterRestart_restart_node/1
	]).

-include_lib("common_test/include/ct.hrl").
-include("test_csi.hrl").
-include("test_chi.hrl").

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
%% @spec suite() -> list()
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 30}},
     {ct_hooks, [{rct_rpc, rpc},
                 {rct_htmllink,[]},
		 {rct_logging, {all, [{erlang, {["ERROR REPORT",
						 "CRASH REPORT"],[]
					       }}]}},
		 {rct_proxy,[{1, node1, ssh_lmt_ipv4, du1, username}]},
		 {rct_coli, {coli, [manual_connect]}},
                 {rct_core,[]}
		]}].

%% @hidden
init_per_suite(Config) ->
    {ok, client_started} = rct_proxy:start_proxy(node1, csi1, ?CSI),
    {ok, client_started} = rct_proxy:start_proxy(node1, chi1, ?CHI),
    Config.

%% @hidden
end_per_suite(_Config) ->
    timer:sleep(2000),          %% TODO: delay, why?
    %%    {ok, memory_freed} =
    %%        rct_proxy:send_proxy(node1, csi1, ?CelloPri_freeMemory),

    {ok, _} = rct_proxy:stop_proxy(node1, csi1),
    {ok, _} = rct_proxy:stop_proxy(node1, chi1),
    ok.
%%%    ok = rct_proxy:exit_master(node1).

%% @hidden
init_per_group(GroupName, Config) ->
    ct:pal("##################~n### Test group ###  ~p~n##################",
	   [GroupName]),
    Config.

%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.

%% @hidden
init_per_testcase(TestCase, Config) ->
    ct:pal("#################~n### Test case ###  ~p~n#################",
	   [TestCase]),
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
     subscribeCoreState_normal_unsubscribe,
     subscribeCoreState_negative,
     subscribeCoreState_terminate,
     getOwnMpid,
     getHuntPathPrefix_own,
     getHuntPathPrefix_other
    %% ,
    %%  subscribeClusterRestart_normal_unsubscribe,
    %%  subscribeClusterRestart_restart_node
    ].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Csi_subscribeCoreState function of the CSI interface.<br/>
%% @spec subscribeCoreState_normal_unsubscribe(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
subscribeCoreState_normal_unsubscribe(_Config) ->
    {ok, ?CSI_OK} =
        rct_proxy:send_proxy(node1,
			     csi1,
			     ?Csi_subscribeCoreState,
			     {?PIU_INSTANCE_ID_OWN}),
    {ok, {?CSI_CORE_STATE_CHANGE_IND,
 	  ?PIU_INSTANCE_ID_OWN,
 	  ?CSI_CORE_STATE_ACTIVE}} = rct_proxy:receive_proxy(),
    rct_rpc:call(rpc,
		 clh_csi_service,
		 trigger_CsiCoreStateChangeInd,
		 [?PIU_INSTANCE_ID_OWN],
		 10000),
    {ok, {?CSI_CORE_STATE_CHANGE_IND,
 	  ?PIU_INSTANCE_ID_OWN,
 	  ?CSI_CORE_STATE_ACTIVE}} = rct_proxy:receive_proxy(),
    {ok, ?CSI_OK} =
        rct_proxy:send_proxy(node1,
			     csi1,
			     ?Csi_unsubscribeCoreState,
			     {?PIU_INSTANCE_ID_OWN}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Csi_subscribeCoreState function of the CSI interface.<br/>
%% @spec subscribeCoreState_negative(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
subscribeCoreState_negative(_Config) ->
    {ok, ?CSI_NO_EXIST} =
        rct_proxy:send_proxy(node1,
			     csi1,
			     ?Csi_subscribeCoreState,
			     {?PIU_INSTANCE_ID_NONEXISTENT}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Csi_subscribeCoreState function of the CSI interface.<br/>
%% @spec subscribeCoreState_terminate(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
subscribeCoreState_terminate(_Config) ->
    {ok, ?CSI_OK} =
        rct_proxy:send_proxy(node1,
			     csi1,
			     ?Csi_subscribeCoreState,
			     {?PIU_INSTANCE_ID_OWN}),
    {ok, {?CSI_CORE_STATE_CHANGE_IND,
 	  ?PIU_INSTANCE_ID_OWN,
 	  ?CSI_CORE_STATE_ACTIVE}} = rct_proxy:receive_proxy(),
    {ok, client_stopped} = rct_proxy:stop_proxy(node1, csi1),
    {ok, client_started} = rct_proxy:start_proxy(node1, csi1, ?CSI),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Csi_getOwnMpid function of the CSI interface.<br/>
%% @spec getOwnMpid(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
getOwnMpid(_Config) ->
    OwnMpId = rct_rpc:call(rpc, clhI, own_mp_id, [], 10000),
    {ok, ?CSI_OK, OwnMpId} =
        rct_proxy:send_proxy(node1,
			     csi1,
			     ?Csi_getOwnMpid,
			     {?PIU_INSTANCE_ID_NONEXISTENT}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Csi_getHuntPathPrefix function of the CSI interface.<br/>
%% @spec getHuntPathPrefix_own(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
getHuntPathPrefix_own(_Config) ->
    OwnMpId = rct_rpc:call(rpc, clhI, own_mp_id, [], 10000),
    {ok, ?CSI_OK, ""} =
        rct_proxy:send_proxy(node1,
			     csi1,
			     ?Csi_getHuntPathPrefix,
			     {OwnMpId, "obsolete string"}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Csi_getHuntPathPrefix function of the CSI interface.<br/>
%% @spec getHuntPathPrefix_other(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
getHuntPathPrefix_other(_Config) ->
    MpIdList = rct_rpc:call(rpc, clhI, mp_id, [all], 10000),
    OwnMpId = rct_rpc:call(rpc, clhI, own_mp_id, [], 10000),
    case (MpIdList -- [OwnMpId]) of
	[] ->
	    %% Singel DU
	    PathPrefix =
		rct_rpc:call(rpc,
			     clhI,
			     hunt_path_prefix,
			     [?PIU_INSTANCE_ID_OTHER],
			     10000),
	    ct:pal("PathPrefix = ~p", [PathPrefix]),
	    {ok, ?CSI_OK, PathPrefix} =
		rct_proxy:send_proxy(node1,
				     csi1,
				     ?Csi_getHuntPathPrefix,
				     {?PIU_INSTANCE_ID_OTHER, "obsolete string"});
	[OtherMpId] ->
	    PathPrefix =
		rct_rpc:call(rpc,
			     clhI,
			     hunt_path_prefix,
			     [OtherMpId],
			     10000),
	    ct:pal("PathPrefix = ~p", [PathPrefix]),
	    {ok, ?CSI_OK, PathPrefix} =
		rct_proxy:send_proxy(node1,
				     csi1,
				     ?Csi_getHuntPathPrefix,
				     {OtherMpId, "obsolete string"})
    end,
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Exercises the Csi_subscribeCoreState function of the CSI interface.<br/>
%% @spec subscribeClusterRestart_normal_unsubscribe(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
subscribeClusterRestart_normal_unsubscribe(_Config) ->
    {ok, ?CSI_OK} =
        rct_proxy:send_proxy(node1,
			     csi1,
			     ?Csi_subscribeClusterRestart),

    {ok, ?CSI_OK} =
        rct_proxy:send_proxy(node1,
			     csi1,
			     ?Csi_unsubscribeClusterRestart),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Csi_subscribeCoreState function of the CSI interface.<br/>
%% @spec subscribeClusterRestart_restart_node(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
subscribeClusterRestart_restart_node(_Config) ->
    {ok, ?CSI_OK} =
        rct_proxy:send_proxy(node1,
			     csi1,
			     ?Csi_subscribeClusterRestart),

    AppsBeforeRestart = restart_cluster(),

    {ok, {?CSI_CLUSTER_RESTART_IND,
	  ?CSI_RESTART_TYPE_SOFT}} = rct_proxy:receive_proxy(),

    {ok, ?CSI_OK} =
        rct_proxy:send_proxy(node1,
			     csi1,
			     ?Csi_clusterRestartReply),

    wait_cluster_up(AppsBeforeRestart),

    ok.


%%% ###########################################################################
%%% restart_cluster
%%%
%%% ###=====================================================================###
restart_cluster() ->
    [CoreNode] = rct_rpc:call(rpc, clhI, erlang_node, [active], 10000),
    %% AppsBeforeRestart = rct_rpc:call(rpc, appmServer, get_apps, [], 10000),
    CoreAppsBeforeRestart =
	rct_rpc:call(rpc, rpc, call, [CoreNode, appmServer, get_apps, []], 10000),

    {ok, ?CHI_RESULT_OK} =
        rct_proxy:send_proxy(node1,
			     chi1,
			     ?Chi_restartCluster,
			     {?CHI_RESTART_TYPE_SOFT,
			      ?CHI_RESTART_RANK_COLD,
			      atom_to_list(?MODULE)}),
    CoreAppsBeforeRestart.


wait_cluster_up(AppsBeforeRestart) ->
    TimeBefore = erlang:monotonic_time(),
    ct:pal("Check that the COLI connection goes down..."),
    poll_connection(coli, 1000, {error, econnrefused}, 40),
    ct:pal("...COLI down!~nWait for the COLI connection to come up again..."),
    poll_connection(coli, 3000, ok, 60),
    ct:pal("...COLI up!~nCheck that APPM started all applications..."),
    ok = check_all_apps_started(AppsBeforeRestart),
    TimeAfter = erlang:monotonic_time(),
    RestartTime = erlang:convert_time_unit(TimeAfter-TimeBefore, native,
					   seconds),
    ct:log("Restart time: ~p seconds", [RestartTime]),
    {ok, client_started} = rct_proxy:start_proxy(node1, csi1, ?CHI),
    ct:pal("...APPM done. Cluster restarted."),
    ok.


%%% ###########################################################################
%%% poll_connection
%%%
%%% ###=====================================================================###
poll_connection(HookName, _Timer, _Result, 0)->
    ct:fail("Tried too many times to connect to rct_~p", [HookName]) ;
poll_connection(HookName, Timer, Result, NumTry)->
    timer:sleep(Timer),
    case HookName of
	coli ->
	    case rct_coli:connect(coli) of
		Result ->
		    ct:log("OK: ~p",[Result]),
		    rct_coli:disconnect(HookName),
		    ok;
		_ ->
		    ok = rct_coli:disconnect(HookName),
		    poll_connection(HookName, Timer, Result, NumTry - 1)
	    end;
	_ ->
	    ct:fail("HookName ~p not suported",[HookName])
    end.

%%% ###########################################################################
%%% check_all_apps_started
%%%
%%% ###=====================================================================###
check_all_apps_started(AppsBeforeRestart)->
    check_all_apps_started(AppsBeforeRestart, [], 30).

check_all_apps_started(AppsBeforeRestart, _, NumTry) when NumTry > 0 ->
    AppsAfterRestart = rct_rpc:call(rpc, appmServer, get_apps, [], 10000),
    timer:sleep(1000),
    case lists:foreach(fun(App) ->
			       lists:keymember(App, 1, AppsBeforeRestart)
		       end,
		       AppsAfterRestart)
	of
	ok ->
	    ok;
	_ ->
	    check_all_apps_started(AppsBeforeRestart,
				   AppsAfterRestart,
				   NumTry - 1)
    end;
check_all_apps_started(AppsBeforeRestart, AppsAfterRestart, _)->
    ct:fail("Not all apps started~nApps before restart:~n~p"
	    "~nApps after restart:~n~p",
	    [AppsBeforeRestart, AppsAfterRestart]).
