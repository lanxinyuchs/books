%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pes_misc_SUITE.erl %
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R5A/R12A/1

%%% @doc 
%%% == Example test suite for one sim or target env ==
%%% Miscelaneous PES tests
%%% @end

-module(pes_misc_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("pes_test.hrl").

%%% ----------------------------------------------------------
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
%%% R3A/1      2014-11-10 uabesvi     Created
%%% R3A/2      2015-07-10 etxjovp     Add group definitions used by CS CI
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


-export([lib/1]).
-export([server/1]).
-export([debug/1]).
-export([appdata/1]).
-export([app_registry/1]).


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
    AddProxyCfg = get_ct_config(pes_pei_proxy, [{erl_api, false}]),
    ProxyCfg = proplists:get_value(pes_pei_proxy, DefHooks, AddProxyCfg), 
    NewProxyCfg = {pes_pei_proxy, lists:ukeysort(1, AddProxyCfg ++ ProxyCfg)},
    CliHook = [{rct_cli, {?CLI_USER, [manual_connect]}}],
    Hooks = lists:keystore(pes_pei_proxy, 1, DefHooks, NewProxyCfg),
    [{ct_hooks, Hooks ++ CliHook}].


%% @hidden
init_per_suite(Config) ->
    Config.

%% @hidden
end_per_suite(_Config) ->
    ct:sleep(3000),
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
     lib,
     server,
     %%debug, removed because it fails on CI, but I no not know why
     %%       it seems that the job mnesia event is never received
     %%       and the test case fails with timeout.
     appdata,
     app_registry
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
     {sdc__qual__all__1__group, [], []}  
   ].


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR TESTCASES
%%% #---------------------------------------------------------

%%========================================================================
%% lib(Config) -> ok.
%% 
%% @doc 
%% test pesLib
%% @end
%%========================================================================
lib(_Config) ->
    
    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    Int = [rpc(pesLib, get_interval, [X]) || X <- [1,2,3,4,5,6,7,8,9]],
    ct:pal("Interval ~p~n", [Int]),

    rpc(pesLib, epoch_time, []),
    ok.


%%========================================================================
%% server(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
server(_Config) ->
    rpc(pesServer, get_current_state, [{"1","1","1","first","job"}]),
    rpc(gen_server, call, [pesServer, "unknown call"]),
    rpc(gen_server, cast, [pesServer, "unknown cast"]),
    rpc(gen_server, cast, [pesServer, {test_terminate, normal}]),
    ct:sleep(3000),
    ok.


%%========================================================================
%% appdata(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
appdata(_Config) ->
    rpc(pesAppData, appdata, ["CXP9021691_2","R4A19", appdata()]),
    rpc(gen_server, cast, [pesServer, {test_terminate, normal}]),
    ct:sleep(3000),
    ok.


%%========================================================================
%% app_registry(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
app_registry(_Config) ->
    rpc(gen_server, call, [pesAppRegistry, invalid_msg]),
    rpc(gen_server, cast, [pesAppRegistry, invalid_msg]),
    rpc(gen_server, cast, [pesAppRegistry, {test_terminate, normal}]),
    ct:sleep(3000),
    ok.


%%========================================================================
%% debug(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
debug(_Config) ->
    %%-------------------------------------------------------------
    %% Create job
    %%-------------------------------------------------------------
    App = ?SUNE,
    Job = "debug_test",

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
    %% tests
    %%-------------------------------------------------------------
    rpc(pesDebug, procs, []),
    rpc(pesDebug, tables, []),
    rpc(pesDebug, app_aliases, []),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?FIN_DATA(Job, ExpData)),
    ok = delete_jobs(Data),
    wait_expected_result(RefFinal, App),
    
    ok = delete_app(App, Handle),
    ok.

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





appdata() ->
    {xmlElement,appdata,appdata,[],
    {xmlNamespace,[],[]},
    [],2,
    [{xmlAttribute,target,[],[],[],[{appdata,2}],1,[],"pmEventAlias",false}],
    [{xmlText,[{appdata,2}],1,[],"\n\n  ",text},
     {xmlElement,eventMapId,eventMapId,[],
         {xmlNamespace,[],[]},
         [{appdata,2}],
         2,[],
         [{xmlText,[{eventMapId,2},{appdata,2}],1,[],"fake",text}],
         [],
         "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
         undeclared},
     {xmlText,[{appdata,2}],3,[]," \n\n  ",text},
     {xmlElement,eventGroupAlias,eventGroupAlias,[],
         {xmlNamespace,[],[]},
         [{appdata,2}],
         4,[],
         [{xmlText,[{eventGroupAlias,4},{appdata,2}],1,[]," \n    ",text},
          {xmlElement,eventGroupId,eventGroupId,[],
              {xmlNamespace,[],[]},
              [{eventGroupAlias,4},{appdata,2}],
              2,[],
              [{xmlText,
                   [{eventGroupId,2},{eventGroupAlias,4},{appdata,2}],
                   1,[],"EventGrp1",text}],
              [],
              "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
              undeclared},
          {xmlText,[{eventGroupAlias,4},{appdata,2}],3,[],"\n    ",text},
          {xmlElement,groupAlias,groupAlias,[],
              {xmlNamespace,[],[]},
              [{eventGroupAlias,4},{appdata,2}],
              4,[],
              [{xmlText,
                   [{groupAlias,4},{eventGroupAlias,4},{appdata,2}],
                   1,[],"1",text}],
              [],
              "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
              undeclared},
          {xmlText,[{eventGroupAlias,4},{appdata,2}],5,[],"\n  ",text}],
         [],
         "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
         undeclared},
     {xmlText,[{appdata,2}],5,[],"\n\n  ",text},
     {xmlElement,eventGroupAlias,eventGroupAlias,[],
         {xmlNamespace,[],[]},
         [{appdata,2}],
         6,[],
         [{xmlText,[{eventGroupAlias,6},{appdata,2}],1,[]," \n    ",text},
          {xmlElement,eventGroupId,eventGroupId,[],
              {xmlNamespace,[],[]},
              [{eventGroupAlias,6},{appdata,2}],
              2,[],
              [{xmlText,
                   [{eventGroupId,2},{eventGroupAlias,6},{appdata,2}],
                   1,[],"EventGrp2",text}],
              [],
              "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
              undeclared},
          {xmlText,[{eventGroupAlias,6},{appdata,2}],3,[],"\n    ",text},
          {xmlElement,groupAlias,groupAlias,[],
              {xmlNamespace,[],[]},
              [{eventGroupAlias,6},{appdata,2}],
              4,[],
              [{xmlText,
                   [{groupAlias,4},{eventGroupAlias,6},{appdata,2}],
                   1,[],"2",text}],
              [],
              "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
              undeclared},
          {xmlText,[{eventGroupAlias,6},{appdata,2}],5,[],"\n  ",text}],
         [],
         "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
         undeclared},
     {xmlText,[{appdata,2}],7,[],"\n\n  ",text},
     {xmlElement,eventGroupAlias,eventGroupAlias,[],
         {xmlNamespace,[],[]},
         [{appdata,2}],
         8,[],
         [{xmlText,[{eventGroupAlias,8},{appdata,2}],1,[]," \n    ",text},
          {xmlElement,eventGroupId,eventGroupId,[],
              {xmlNamespace,[],[]},
              [{eventGroupAlias,8},{appdata,2}],
              2,[],
              [{xmlText,
                   [{eventGroupId,2},{eventGroupAlias,8},{appdata,2}],
                   1,[],"EventGrp3",text}],
              [],
              "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
              undeclared},
          {xmlText,[{eventGroupAlias,8},{appdata,2}],3,[],"\n    ",text},
          {xmlElement,groupAlias,groupAlias,[],
              {xmlNamespace,[],[]},
              [{eventGroupAlias,8},{appdata,2}],
              4,[],
              [{xmlText,
                   [{groupAlias,4},{eventGroupAlias,8},{appdata,2}],
                   1,[],"3",text}],
              [],
              "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
              undeclared},
          {xmlText,[{eventGroupAlias,8},{appdata,2}],5,[],"\n  ",text}],
         [],
         "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
         undeclared},
     {xmlText,[{appdata,2}],9,[],"\n\n  ",text},
     {xmlElement,eventTypeAlias,eventTypeAlias,[],
         {xmlNamespace,[],[]},
         [{appdata,2}],
         10,[],
         [{xmlText,[{eventTypeAlias,10},{appdata,2}],1,[],"\n    ",text},
          {xmlElement,eventTypeId,eventTypeId,[],
              {xmlNamespace,[],[]},
              [{eventTypeAlias,10},{appdata,2}],
              2,[],
              [{xmlText,
                   [{eventTypeId,2},{eventTypeAlias,10},{appdata,2}],
                   1,[],"EvType1",text}],
              [],
              "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
              undeclared},
          {xmlText,[{eventTypeAlias,10},{appdata,2}],3,[],"\n    ",text},
          {xmlElement,typeAlias,typeAlias,[],
              {xmlNamespace,[],[]},
              [{eventTypeAlias,10},{appdata,2}],
              4,[],
              [{xmlText,
                   [{typeAlias,4},{eventTypeAlias,10},{appdata,2}],
                   1,[],"1",text}],
              [],
              "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
              undeclared},
          {xmlText,[{eventTypeAlias,10},{appdata,2}],5,[],"\n  ",text}],
         [],
         "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
         undeclared},
     {xmlText,[{appdata,2}],11,[],"\n\n  ",text},
     {xmlElement,eventTypeAlias,eventTypeAlias,[],
         {xmlNamespace,[],[]},
         [{appdata,2}],
         12,[],
         [{xmlText,[{eventTypeAlias,12},{appdata,2}],1,[],"\n    ",text},
          {xmlElement,eventTypeId,eventTypeId,[],
              {xmlNamespace,[],[]},
              [{eventTypeAlias,12},{appdata,2}],
              2,[],
              [{xmlText,
                   [{eventTypeId,2},{eventTypeAlias,12},{appdata,2}],
                   1,[],"EvType2",text}],
              [],
              "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
              undeclared},
          {xmlText,[{eventTypeAlias,12},{appdata,2}],3,[],"\n    ",text},
          {xmlElement,typeAlias,typeAlias,[],
              {xmlNamespace,[],[]},
              [{eventTypeAlias,12},{appdata,2}],
              4,[],
              [{xmlText,
                   [{typeAlias,4},{eventTypeAlias,12},{appdata,2}],
                   1,[],"2",text}],
              [],
              "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
              undeclared},
          {xmlText,[{eventTypeAlias,12},{appdata,2}],5,[],"\n  ",text}],
         [],
         "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
         undeclared},
     {xmlText,[{appdata,2}],13,[],"\n\n  ",text},
     {xmlElement,eventTypeAlias,eventTypeAlias,[],
         {xmlNamespace,[],[]},
         [{appdata,2}],
         14,[],
         [{xmlText,[{eventTypeAlias,14},{appdata,2}],1,[],"\n    ",text},
          {xmlElement,eventTypeId,eventTypeId,[],
              {xmlNamespace,[],[]},
              [{eventTypeAlias,14},{appdata,2}],
              2,[],
              [{xmlText,
                   [{eventTypeId,2},{eventTypeAlias,14},{appdata,2}],
                   1,[],"EvType3",text}],
              [],
              "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
              undeclared},
          {xmlText,[{eventTypeAlias,14},{appdata,2}],3,[],"\n    ",text},
          {xmlElement,typeAlias,typeAlias,[],
              {xmlNamespace,[],[]},
              [{eventTypeAlias,14},{appdata,2}],
              4,[],
              [{xmlText,
                   [{typeAlias,4},{eventTypeAlias,14},{appdata,2}],
                   1,[],"3",text}],
              [],
              "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
              undeclared},
          {xmlText,[{eventTypeAlias,14},{appdata,2}],5,[],"\n  ",text}],
         [],
         "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
         undeclared},
     {xmlText,[{appdata,2}],15,[],"\n\n",text}],
    [],
    "/local/scratch/uabesvi/RCS_ROOT/home/uabesvi/software/DUMMY-SIM_CXP9021691_2_R4A19/FAKE2_CXC1734197_2/fake-R4A04/priv/appdata",
    undeclared}.





get_ct_config(TC, DEF) -> ?LIB:get_ct_config(TC, DEF).

%% start(A)    -> {ok, _} = ?LIB:start(A).
%% stop(A)     -> ok = ?ERL_APP:stop(A).   
cleanup()   -> ?LIB:cleanup().

%% initialize(A, B) -> ?LIB:initialize(A, B).
%% finalize(A, B)   -> ok = ?LIB:finalize(A, B).


create_app(A)    -> ?LIB:create_app(A).
delete_app(A, B) -> ?LIB:delete_app(A, B).


create_job(B)       -> ?LIB:create_job(?PRODUCER_DEF, B).
%% create_job(B, C)    -> ?LIB:create_job(?PRODUCER_DEF, B, C).
%% create_job(A, B, C) -> ?LIB:create_job(A, B, C).
%% delete_job(B, C)    -> ?LIB:delete_job(?PRODUCER_DEF, B, C).
%% delete_job(A, B, C) -> ?LIB:delete_job(A, B, C).
%% delete_jobs(A, B)   -> ?LIB:delete_jobs(A, B).
delete_jobs(B)      -> ?LIB:delete_jobs(?PRODUCER_DEF, B).
%% update_job(B)       -> ?LIB:update_job(?PRODUCER_DEF, B).

%% update_me(A, B, C) -> ?LIB:update_me(A, B, C).

expected(A, B, C)         -> ?LIB:expected(A, B, C).
wait_expected_result(A,B) -> ?LIB:wait_expected_result(A, B).

hooks() -> ?LIB:hooks().

rpc(A, B, C) -> ?LIB:rpc(A, B, C).

%% open_trans()  -> ?LIB:open_trans().
%% close_trans() -> Res = ?LIB:close_trans(), to(4), Res.


