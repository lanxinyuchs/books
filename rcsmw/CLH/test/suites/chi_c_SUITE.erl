%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	chi_c_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R3A/R4A/R5A/2
%%%
%%% @doc ==Basic Test Suite for the CHI interface on SUT.==
%%% This Test Suite can be used on both target and simulated enviroment.
%%% <br/><br/>
%%% rct_proxy is used in ct_hooks:
%%% see [https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/edoc/rct_proxy.html]
%%% <br/>
%%%
%%%
%%% @end

%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% ----------------------------------------------------------Add
%%% R3A/1      2014-12-17 etxpeno     Created
%%% R4A/1      2015-04-17 etxberb     Prepared according to changed interfaces.
%%% R4A/2      2015-04-24 etxberb     Added chi_xxx functions.
%%% R4A/3      2015-05-19 etxberb     More checks in test cases.
%%% R4A/4      2015-05-19 etxberb     Empty all/0. Delivery coordination tmp fix
%%% R4A/5      2015-05-19 etxberb     Tmp coordination fix restored.
%%% R4A/6      2015-05-20 etxberb     Added more associateMp cases.
%%% R4A/7      2015-06-05 etxberb     Changed FruId_undefined to FruId_default.
%%% R4A/8      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R4A/9      2015-07-15 etxjovp     modify group definitions used by CS CI
%%% R4A/10     2015-09-16 etxpejn     Core rank fix in associateMp_negative_FruId_MpId_mismatch
%%% R5A/1      2015-12-18 etxpejn     Added TC associateMp_negative_coreMpId_wrongCoreRank
%%% R5A/2      2016-02-12 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%% R5A/3      2016-10-17 egjknpa     add FAKE_VNFM for vrcs 
%%% ----------------------------------------------------------
-module(chi_c_SUITE).

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
	 associateMp_normal_disassociateMp/1,
	 associateMp_negative_CoreRank/1,
	 associateMp_negative_FruId_MpId_mismatch/1,
	 associateMp_negative_existMpId_nonexistFruId/1,
	 associateMp_negative_nonexistMpId_existFruId/1,
	 associateMp_negative_coreMpId_wrongCoreRank/1,
	 disassociateMp_negative_nonexistMpId/1,
	 subscribeOpState_normal_unsubscribe/1,
	 subscribeOpState_negative/1,
	 subscribeOpState_terminate/1,
	 restartCluster/1
	]).

-include_lib("common_test/include/ct.hrl").
-include("test_chi.hrl").

-define(RE_COLD,".*\\s+Cold.*").
-define(COMMAND_COLD, "grep \"Cold\" /var/log/syslog").

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
    [{timetrap, {minutes, 30}},
     {ct_hooks, [{rct_rpc, rpc},
                 {rct_htmllink, []},
		 {rct_logging, {all, [{erlang, {["ERROR REPORT",
						 "CRASH REPORT"],[]}}]}},
		 {rct_proxy, [{1, node1, ssh_lmt_ipv4, du1, username}]},
		 {rct_coli, {coli, [manual_connect]}},
		 {rct_core, []}
		]}].

%% @hidden
init_per_suite(Config) ->
    {ok, client_started} = rct_proxy:start_proxy(node1, chi1, ?CHI),
    %% fake VNFM for vrcs
    case rct_rpc:call(rpc, sysEnv, rcs_mode_2, [], 10000) of
        vrcs ->
            rct_rpc:call(rpc, os, putenv, ["FAKE_VNFM", ""], 10000);
        _ ->
            rct_rpc:call(rpc, os, unsetenv, ["FAKE_VNFM"], 10000)
    end,
    Config.

%% @hidden
end_per_suite(_Config) ->
    timer:sleep(2000),          %% TODO: delay, why?

    rct_proxy:stop_proxy(node1, chi1),
    %% fake VNFM for vrcs
    case rct_rpc:call(rpc, sysEnv, rcs_mode_2, [], 10000) of
        vrcs ->
            rct_rpc:call(rpc, os, unsetenv, ["FAKE_VNFM"], 10000);
        _ ->
            ok
    end,
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
     {cover__group, [], AllGroup -- [restartCluster]},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},
     {sdc__cover__sim__1__group, [], [{group, cover__group}]},
     {sdc__nocover__sim__1__group, [], [restartCluster]},
     {sdc__def__all__1__group, [], [{group, default__group}]},
     {sdc__qual__all__1__group, [], []},
     %% This suite should only work on core MP within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     associateMp_normal_disassociateMp,
     associateMp_negative_CoreRank,
     associateMp_negative_FruId_MpId_mismatch,
     associateMp_negative_existMpId_nonexistFruId,
     associateMp_negative_nonexistMpId_existFruId,
     associateMp_negative_coreMpId_wrongCoreRank,
     disassociateMp_negative_nonexistMpId,
     subscribeOpState_normal_unsubscribe,
     subscribeOpState_negative,
     subscribeOpState_terminate,
     restartCluster
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% Exercises the Chi_associateMp function of the CHI interface.<br/>
%% @spec associateMp_normal_disassociateMp(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
associateMp_normal_disassociateMp(_Config) ->
    {ok, Erl_CoreRank} =
	rct_rpc:call(rpc, clh_csi_service, get_coreRank, [?MP_ID_OWN], 10000),
    CoreRank = erl2c_CoreRank(Erl_CoreRank),
    DefaultFruId =
	rct_rpc:call(rpc, clhI, make_default_fru_id, [?MP_ID_OWN], 10000),
    case rct_rpc:call(rpc, clh_csi_service, get_fruId, [?MP_ID_OWN], 10000) of
	{ok, DefaultFruId} ->
	    FruId = "FruId " ++ atom_to_list(?MODULE),
	    {ok, ?CHI_RESULT_OK} =
		rct_proxy:send_proxy(node1,
				     chi1,
				     ?Chi_associateMp,
				     {?MP_ID_OWN, FruId, CoreRank}),
	    {ok, ?CHI_RESULT_OK} =
		rct_proxy:send_proxy(node1,
				     chi1,
				     ?Chi_disassociateMp,
				     {?MP_ID_OWN});
	{ok, Bin_FruId} ->
	    FruId = binary_to_list(Bin_FruId)
    end,
    {ok, ?CHI_RESULT_OK} =
        rct_proxy:send_proxy(node1,
			     chi1,
			     ?Chi_associateMp,
			     {?MP_ID_OWN, FruId, CoreRank}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Chi_associateMp function of the CHI interface.<br/>
%% @spec associateMp_negative_CoreRank(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
associateMp_negative_CoreRank(_Config) ->
    {ok, Bin_FruId} =
	rct_rpc:call(rpc, clh_csi_service, get_fruId, [?MP_ID_OWN], 10000),
    FruId = binary_to_list(Bin_FruId),
    case rct_rpc:call(rpc, clh_csi_service, get_coreRank, [?MP_ID_OWN], 10000) of
	{ok, primary} ->
	    CoreRank = ?CHI_CORE_RANK_SECONDARY;
	{ok, _} ->
	    CoreRank = ?CHI_CORE_RANK_PRIMARY
    end,
    {ok, ?CHI_RESULT_BAD_PARM} =
        rct_proxy:send_proxy(node1,
			     chi1,
			     ?Chi_associateMp,
			     {?MP_ID_OWN, FruId, CoreRank}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Chi_associateMp function of the CHI interface.<br/>
%% @spec associateMp_negative_FruId_MpId_mismatch(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
associateMp_negative_FruId_MpId_mismatch(_Config) ->
    {ok, Bin_FruId} =
	rct_rpc:call(rpc, clh_csi_service, get_fruId, [?MP_ID_OWN], 10000),
    FruId = binary_to_list(Bin_FruId),
    case
	rct_rpc:call(rpc, clh_csi_service, get_coreRank, [?MP_ID_OTHER], 10000)
    of
	{ok, Erl_CoreRank} ->
	    CoreRank = erl2c_CoreRank(Erl_CoreRank),
	    {ok, ?CHI_RESULT_BAD_PARM} =
		rct_proxy:send_proxy(node1,
				     chi1,
				     ?Chi_associateMp,
				     {?MP_ID_OTHER, FruId, CoreRank});
	{error, mp_undefined} ->
	    associateMp_negative_nonexistMpId_existFruId
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Chi_associateMp function of the CHI interface.<br/>
%% @spec associateMp_negative_existMpId_nonexistFruId(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
associateMp_negative_existMpId_nonexistFruId(_Config) ->
    {ok, Erl_CoreRank} =
	rct_rpc:call(rpc, clh_csi_service, get_coreRank, [?MP_ID_OWN], 10000),
    CoreRank = erl2c_CoreRank(Erl_CoreRank),
    DefaultFruId =
	rct_rpc:call(rpc, clhI, make_default_fru_id, [?MP_ID_OWN], 10000),
    case rct_rpc:call(rpc, clh_csi_service, get_fruId, [?MP_ID_OWN], 10000) of
	{ok, DefaultFruId} ->
	    ok;
	{ok, _} ->
	    FruId =
		"nonexistFruId " ++ atom_to_list(?MODULE),
	    {ok, ?CHI_RESULT_BAD_PARM} =
		rct_proxy:send_proxy(node1,
				     chi1,
				     ?Chi_associateMp,
				     {?MP_ID_OWN, FruId, CoreRank})
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Chi_associateMp function of the CHI interface.<br/>
%% @spec associateMp_negative_nonexistMpId_existFruId(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
associateMp_negative_nonexistMpId_existFruId(_Config) ->
    {ok, Bin_FruId} =
	rct_rpc:call(rpc, clh_csi_service, get_fruId, [?MP_ID_OWN], 10000),
    FruId = binary_to_list(Bin_FruId),
    {ok, ?CHI_RESULT_BAD_PARM} =
        rct_proxy:send_proxy(node1,
			     chi1,
			     ?Chi_associateMp,
			     {?MP_ID_NONEXISTENT,
			      FruId,
			      ?CHI_CORE_RANK_PRIMARY}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Chi_associateMp function of the CHI interface.<br/>
%% @spec associateMp_negative_coreMpId_wrongCoreRank(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
associateMp_negative_coreMpId_wrongCoreRank(_Config) ->
    {ok, ?CHI_RESULT_BAD_PARM} =
        rct_proxy:send_proxy(node1,
			     chi1,
			     ?Chi_associateMp,
			     {?MP_ID_STANDBY,
			      "CoreMpIdWithCoreRankUndefined",
			      ?CHI_CORE_RANK_UNDEFINED}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Chi_disassociateMp function of the CHI interface.<br/>
%% @spec disassociateMp_negative_nonexistMpId(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
disassociateMp_negative_nonexistMpId(_Config) ->
    {ok, ?CHI_RESULT_OK} =
        rct_proxy:send_proxy(node1,
			     chi1,
			     ?Chi_disassociateMp,
			     {?MP_ID_NONEXISTENT}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Chi_subscribeOpState function of the CHI interface.<br/>
%% @spec subscribeOpState_normal_unsubscribe(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
subscribeOpState_normal_unsubscribe(_Config) ->
    {ok, ?CHI_RESULT_OK} =
        rct_proxy:send_proxy(node1,
			     chi1,
			     ?Chi_subscribeOpState,
			     {?MP_ID_OWN}),
    {ok, {?CHI_OP_STATE_CHANGE_IND,
 	  ?MP_ID_OWN,
 	  ?CHI_OPSTATE_ENABLED}} = rct_proxy:receive_proxy(),
    rct_rpc:call(rpc,
		 clh_csi_service,
		 trigger_ChiOpStateChangeInd,
		 [?MP_ID_OWN],
		 10000),
    {ok, {?CHI_OP_STATE_CHANGE_IND,
 	  ?MP_ID_OWN,
 	  ?CHI_OPSTATE_ENABLED}} = rct_proxy:receive_proxy(),
    {ok, ?CHI_RESULT_OK} =
        rct_proxy:send_proxy(node1,
			     chi1,
			     ?Chi_unsubscribeOpState,
			     {?MP_ID_OWN}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Chi_subscribeOpState function of the CHI interface.<br/>
%% @spec subscribeOpState_negative(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
subscribeOpState_negative(_Config) ->
    {ok, ?CHI_RESULT_NO_EXIST} =
        rct_proxy:send_proxy(node1,
			     chi1,
			     ?Chi_subscribeOpState,
			     {?MP_ID_NONEXISTENT}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Chi_subscribeOpState function of the CHI interface.<br/>
%% @spec subscribeOpState_terminate(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
subscribeOpState_terminate(_Config) ->
    {ok, ?CHI_RESULT_OK} =
        rct_proxy:send_proxy(node1,
			     chi1,
			     ?Chi_subscribeOpState,
			     {?MP_ID_OWN}),
    {ok, {?CHI_OP_STATE_CHANGE_IND,
 	  ?MP_ID_OWN,
 	  ?CHI_OPSTATE_ENABLED}} = rct_proxy:receive_proxy(),
    {ok, client_stopped} = rct_proxy:stop_proxy(node1, chi1),
    {ok, client_started} = rct_proxy:start_proxy(node1, chi1, ?CHI),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Exercises the Chi_restartCluster function of the CHI interface.<br/>
%% @spec restartCluster(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
restartCluster(_Config) ->
    AppsBeforeRestart = rct_rpc:call(rpc, appmServer, get_apps, [], 10000),
    {ok, ?CHI_RESULT_OK} =
        rct_proxy:send_proxy(node1,
			     chi1,
			     ?Chi_restartCluster,
			     {?CHI_RESTART_TYPE_HARD,
			      ?CHI_RESTART_RANK_COLD,
			      atom_to_list(?MODULE)}),
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
    {ok, client_started} = rct_proxy:start_proxy(node1, chi1, ?CHI),
    ct:pal("...APPM done. Cluster restarted."),
    ok.

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
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

%%% ###########################################################################
%%% erl2c_CoreRank
%%%
%%% ###=====================================================================###
erl2c_CoreRank(primary) ->
    ?CHI_CORE_RANK_PRIMARY;
erl2c_CoreRank(secondary) ->
    ?CHI_CORE_RANK_SECONDARY;
erl2c_CoreRank(undefined) ->
    ?CHI_CORE_RANK_UNDEFINED.

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
