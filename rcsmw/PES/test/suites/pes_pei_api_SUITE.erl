%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pes_pei_api_SUITE.erl %
%%% @copyright Ericsson AB 2015
%%% @version /main/R3A/R4A/1

%%% @doc 
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pmssim.
%%% @end

-module(pes_pei_api_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("pes_test.hrl").

%%% ----------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015 All rights reserved.
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
%%% R3A/1      2014-11-10 uabesvi     Created
%%% R3A/2      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R4A/1      2016-10-17 egjknpa     add FAKE_VNFM for vrcs 
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 all/0,
	 groups/0]).


-export([one_job_grp/1]).


-define(ERL_APP, pes_erl_app).
-define(LIB,     pes_test_lib).

-define(L2B(__L), list_to_binary(__L)).


-define(TESTNODE, testnode).

-define(TS_TO, 2500).
-define(SLEEP,  500).

-define(SUNE,   sune).
-define(STINA,  stina).
-define(BERIT,  berit).

-define(TEST_APPS,  [?SUNE]).
-define(TEST_APPS2, [?SUNE, ?STINA, ?BERIT]).


-define(COMTOP,   "urn:com:ericsson:ecim:ComTop").
-define(ECIM_PM,  "urn:com:ericsson:ecim:ECIM_PM").
-define(SYS_FNCS, "urn:com:ericsson:ecim:SYS_FNCS").
-define(PM,       "urn:com:ericsson:ecim:ECIM_PM").
-define(PM_JOB,   "urn:com:ericsson:ecim:PM_JOB").
-define(PM_MR,    "urn:com:ericsson:ecim:PM_MR").

-define(NC_NAMESPACE, "urn:ietf:params:xml:ns:netconf:base:1.0").

-define(DELETE,  [{'xmlns:nc',     ?NC_NAMESPACE},
		  {'nc:operation', "delete"}]).



%%=========================================================
%% cli macros
%%=========================================================
-define(CLI_USER, cli_user).

-define(CONFIGURE, "configure").
-define(COMMIT,    "commit").
-define(TOP,       "top").

-define(PRINT_OPT, print).


%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() ->
    DefHooks = hooks(),
    AddProxyCfg = get_ct_config(pes_pei_proxy, [{erl_api, true}]),
    ProxyCfg = proplists:get_value(pes_pei_proxy, DefHooks, AddProxyCfg), 
    NewProxyCfg = {pes_pei_proxy, lists:ukeysort(1, AddProxyCfg ++ ProxyCfg)},
    CliHook = [{rct_cli, {?CLI_USER, [manual_connect]}}],
    Hooks = lists:keystore(pes_pei_proxy, 1, DefHooks, NewProxyCfg),
    [{ct_hooks, Hooks ++ CliHook}].


%% @hidden
init_per_suite(Config) ->
    %% fake VNFM for vrcs
    case rpc(sysEnv, rcs_mode_2, []) of
        vrcs ->
            rpc(os, putenv, ["FAKE_VNFM", ""]);
        _ ->
            rpc(os, unsetenv, ["FAKE_VNFM"])
    end,
    Config.

%% @hidden
end_per_suite(_Config) ->
    %% fake VNFM for vrcs
    case rpc(sysEnv, rcs_mode_2, []) of
        vrcs ->
            rpc(os, unsetenv, ["FAKE_VNFM"]);
        _ ->
            ok
    end,
    ok.

%% @hidden
init_per_group(_Group, Config) ->
    Config.

%% @hidden
end_per_group(_Group, _Config) ->
    ok.

%% @hidden
init_per_testcase(TestCase, Config) ->
    ct:pal("~n ########## TC:~p ##########~n~n", [TestCase]),
    %% rpc(os, cmd, ["tex log clear"]),
%%     cleanup(),
    Config.

%% @hidden
end_per_testcase(session_kill_app_proc, _Config) ->
%%    cleanup(),
    ok;
end_per_testcase(_TestCase, _Config) ->
    %% X = rpc(os, cmd, ["tex log read"]),
    %% ct:pal("TEX LOG ~n~p~n", [io:format(X)]),
    cleanup(),
%%     close_trans(),
%%     [catch ?PES_ERL_APP:stop(App) || App <- ?TEST_APPS],
    ok.



%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
     one_job_grp
    ].


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
     %% This suite should only be run on core MP within a sim cluster
     {sbc__cluster__dual_sim_1__1__group, [], [{group, default__group}]}
   ].

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR TESTCASES
%%% #---------------------------------------------------------


%%========================================================================
%% one_job_grp(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
one_job_grp(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    
    App = ?SUNE,
    Job = "one_job_grp",

    {ok, Handle} = create_app(App),
    
    GrpRef = lists:append([?EVENT_GRP_1, ?EVENT_GRP_2]),
    
    Data = get_job_rec(Job, [{eventGroupRef, GrpRef}]), 
    
    Types   = [1, 2, 3],
    ExpData = #peiEventJob{jobId = Job, 
			   types = Types},
    
    {ok, RefJob} = expected(App, Handle, [ExpData]),
    ok = create_job(Data),
    wait_expected_result(RefJob, App),
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?FIN_DATA(Job, ExpData)),
    ok = delete_jobs(Data),
    wait_expected_result(RefFinal, App),
    
    ok = delete_app(App, Handle),
    ok.



%%========================================================================
%% Misc help functions
%%========================================================================


get_job_rec(Job, Opts) ->
    gjr(Opts, ?DEF_JOB(Job)).

gjr([], Rec) -> 
    Rec;
gjr([{eventFilter, V} | T], Rec) -> 
    gjr(T, Rec#eventJob{eventFilter = V});
gjr([{requestedJobState, V} | T], Rec) -> 
    gjr(T, Rec#eventJob{requestedJobState = V});
gjr([{jobControl, V} | T], Rec) -> gjr(T, Rec#eventJob{jobControl = V});
gjr([{eventGroupRef, V} | T], Rec) -> 
    gjr(T, Rec#eventJob{eventGroupRef = V});
gjr([{eventTypeRef, V} | T], Rec) -> 
    gjr(T, Rec#eventJob{eventTypeRef = V});
gjr([{fileOutputEnabled, V} | T], Rec) -> 
    gjr(T, Rec#eventJob{fileOutputEnabled = V});
gjr([{reportingPeriod, V} | T], Rec) -> 
    gjr(T, Rec#eventJob{reportingPeriod = V});
gjr([{fileCompressionType, V} | T], Rec) -> 
    gjr(T, Rec#eventJob{fileCompressionType = V});
gjr([{streamOutputEnabled, V} | T], Rec) -> 
    gjr(T, Rec#eventJob{streamOutputEnabled = V});
gjr([{streamDestinationIpAddress, V} | T], Rec) -> 
    gjr(T, Rec#eventJob{streamDestinationIpAddress = V});
gjr([{streamDestinationPort, V} | T], Rec) -> 
    gjr(T, Rec#eventJob{streamDestinationPort = V});
gjr([{streamCompressionType, V} | T], Rec) -> 
    gjr(T, Rec#eventJob{streamCompressionType = V}).



get_ct_config(TC, DEF) -> ?LIB:get_ct_config(TC, DEF).

cleanup()   -> ?LIB:cleanup().



create_app(A)    -> ?LIB:create_app(A).
delete_app(A, B) -> ?LIB:delete_app(A, B).


create_job(B)       -> ?LIB:create_job(?PRODUCER_DEF, B).
delete_jobs(B)      -> ?LIB:delete_jobs(?PRODUCER_DEF, B).

expected(A, B, C)         -> ?LIB:expected(A, B, C).
wait_expected_result(A,B) -> ?LIB:wait_expected_result(A, B).

hooks() -> ?LIB:hooks().

rpc(A, B, C) -> ?LIB:rpc(A, B, C).
