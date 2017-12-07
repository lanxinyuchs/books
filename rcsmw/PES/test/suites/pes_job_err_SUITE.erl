%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pes_job_err_SUITE.erl %
%%% @version /main/R3A/5

%%% @doc 
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pmssim.
%%% @end

-module(pes_job_err_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("pes_test.hrl").

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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


-export([unknown_appdata/1]).
-export([non_legacy_rp/1]).


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



%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() ->
    DefHooks = proplists:delete(rct_logging, hooks()),
    AddProxyCfg = get_ct_config(pes_pei_proxy, [{erl_api, false}]),
    ProxyCfg = proplists:get_value(pes_pei_proxy, DefHooks, AddProxyCfg), 
    NewProxyCfg = {pes_pei_proxy, lists:ukeysort(1, AddProxyCfg ++ ProxyCfg)},
    Hooks = lists:keystore(pes_pei_proxy, 1, DefHooks, NewProxyCfg),
    [{ct_hooks, Hooks}].


%% @hidden
init_per_suite(Config) ->
    Config.

%% @hidden
end_per_suite(_Config) ->
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
    [unknown_appdata].



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
%% unknown_appdata(Config) -> ok.
%% 
%% @doc 
%% initialize and finalize an application
%% @end
%%========================================================================
unknown_appdata(_Config) ->
    
    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE),
    {error, Reason} = initialize(?SUNE, ?CB_DEF, "unknown_file"),    
    
    ct:pal(" $$$$$ Reason ~p~n", [Reason]),
    stop(?SUNE),
    ok.


%%========================================================================
%% non_legacy_rp(Config) -> ok.
%% 
%% @doc 
%% change RP mode to legacy and try to start a job with non legacy RP
%% @end
%%========================================================================
non_legacy_rp(Config) ->

    RP = proplists:get_value(reporting_period, Config),

    rpc(pmsDb, pms_env_set, [reporting_period, legacy]),
    ct:pal("Reporting period mode = ~p~n", [legacy]),
    rpc(pmsI, rp_legacy, [[]]),


    App = ?SUNE,
    Job = "non_legacy_rp",

    {ok, Handle} = create_app(App),
    
    %%-------------------------------------------------------------
    %% Non legacy RP
    %%-------------------------------------------------------------
    GrpRef = lists:append([?EVENT_GRP_1, ?EVENT_GRP_2]),    
    Data = get_job_rec(Job, [{reportingPeriod, ?TEN_SECONDS},
			     {eventGroupRef,   GrpRef}]), 

    ct:pal("####### Data ~p~n", [Data]),
    
    {error, _} = create_job(Data),


    ok = delete_app(App, Handle),

    %%-------------------------------------------------------------
    %% reset to the old mode
    %%-------------------------------------------------------------
    rpc(pmsDb, pms_env_set, [reporting_period, RP]),
    case RP of
	legacy ->
	    ok;
	_ ->
	    rpc(pmsI, rp_ecim, [[]])
    end.



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


create_app(A)    -> ?LIB:create_app(A).
delete_app(A, B) -> ?LIB:delete_app(A, B).

create_job(B)    -> ?LIB:create_job(?PRODUCER_DEF, B).
%%create_job(B, C) -> ?LIB:create_job(?PRODUCER_DEF, B, C).


get_ct_config(TC, DEF) -> ?LIB:get_ct_config(TC, DEF).

start(A)    -> {ok, _} = ?LIB:start(A).
stop(A)     -> ok = ?ERL_APP:stop(A).   
cleanup()   -> ?LIB:cleanup().

initialize(A, B, C) -> ?LIB:initialize(A, B, C).

hooks() -> ?LIB:hooks().

rpc(A, B, C) -> ?LIB:rpc(A, B, C).
