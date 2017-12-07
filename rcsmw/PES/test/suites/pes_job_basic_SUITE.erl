%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pes_job_basic_SUITE.erl %
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R3A/30

%%% @doc 
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pmssim.
%%% @end

-module(pes_job_basic_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("pes_test.hrl").

%%% ----------------------------------------------------------
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


-export([session_init_finalize/1]).
-export([session_init_finalize_no_cb/1]).
-export([one_job_grp/1]).
-export([one_job_type/1]).
-export([one_job_grp_type/1]).
-export([two_jobs/1]).
-export([two_jobs_start_stop/1]).
-export([file/1]).
-export([file_fail/1]).
-export([stream/1]).
-export([stream_fail/1]).
-export([filter/1]).
-export([mod_ref/1]).
-export([mod_stream/1]).
-export([mod_filter/1]).
-export([rm_predef/1]).
-export([max_jobs/1]).
-export([two_producers/1]).
-export([me_attrs/1]).
-export([predef_job/1]).
-export([job_first/1]).
-export([show_jobs/1]).
-export([show_groups/1]).
-export([show_types/1]).


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
     session_init_finalize,
     session_init_finalize_no_cb,
     one_job_grp,
     one_job_type,
     one_job_grp_type,
     two_jobs,
     two_jobs_start_stop,
     file,
     stream,
     filter,
     mod_ref,
     mod_stream,
     mod_filter,
     max_jobs,
     two_producers,
     me_attrs
     %%  rm_predef     predefined jobs are removed
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
%% session_init_finalize(Config) -> ok.
%% 
%% @doc 
%% initialize and finalize an application
%% @end
%%========================================================================
session_init_finalize(_Config) ->
    
    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE),
    {ok, Sune} = initialize(?SUNE, ?CB_DEF),    
    
    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE, Sune),
    stop(?SUNE),
    ok.


%%========================================================================
%% session_init_finalize_no_cb(Config) -> ok.
%% 
%% @doc 
%% initialize and finalize an application
%% @end
%%========================================================================
session_init_finalize_no_cb(_Config) ->
    %%-------------------------------------------------------------
    %% initialize
    %%-------------------------------------------------------------
    start(?SUNE),
    {ok, Sune} = initialize(?SUNE, []),    
    
    %%-------------------------------------------------------------
    %% cleanup and check all is ok
    %%-------------------------------------------------------------
    finalize(?SUNE, Sune),
    stop(?SUNE),
    ok.


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
%% one_job_type(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
one_job_type(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------

    App = ?SUNE,
    Job = "one_job_type",

    {ok, Handle} = create_app(App),

    TypeRef = lists:append([?EVENT_TYPE_1, ?EVENT_TYPE_2, ?EVENT_TYPE_3]),

    Data = get_job_rec(Job, [{eventTypeRef, TypeRef}]), 

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
%% one_job_grp_type(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
one_job_grp_type(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------

    App = ?SUNE,
    Job = "one_job_grp_type",

    {ok, Handle} = create_app(App),

    GrpRef  = ?EVENT_GRP_1,
    TypeRef = lists:append([?EVENT_TYPE_2, ?EVENT_TYPE_3]),

    Data = get_job_rec(Job, [{eventGroupRef, GrpRef},
			     {eventTypeRef,  TypeRef}]), 
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
%% two_jobs(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
two_jobs(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------

    App  = ?SUNE,
    JobOne = "two_job_grp_type_1",
    JobTwo = "two_job_grp_type_2",

    {ok, Handle} = create_app(App),

    %%-------------------------------------------------------------
    %% create job 1
    %%-------------------------------------------------------------

    GrpRef  = ?EVENT_GRP_1,

    Data1    = get_job_rec(JobOne, [{eventGroupRef, GrpRef}]), 
    Types1   = [1],
    ExpData1 = #peiEventJob{jobId = JobOne, 
			    types = Types1},

    {ok, RefJobOne} = expected(App, Handle, [ExpData1]),
    ok = create_job(Data1),
    wait_expected_result(RefJobOne, App),


    %%-------------------------------------------------------------
    %% create job 2
    %%-------------------------------------------------------------

    TypeRef = lists:append([?EVENT_TYPE_2, ?EVENT_TYPE_3]),

    Data2 = get_job_rec(JobTwo, [{eventTypeRef, TypeRef}]), 
    Types2   = [2, 3],
    ExpData2 = #peiEventJob{jobId = JobTwo, 
			    types = Types2},

    {ok, RefJobTwo} = expected(App, Handle, [ExpData2]),
    ok = create_job(Data2),
    wait_expected_result(RefJobTwo, App),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------

    {ok, RefFinal2} = expected(App, Handle, ?FIN_DATA(JobTwo, ExpData2)),
    ok = delete_jobs(Data2),
    wait_expected_result(RefFinal2, App),

    {ok, RefFinal1} = expected(App, Handle, ?FIN_DATA(JobOne, ExpData1)),
    ok = delete_jobs(Data1),
    wait_expected_result(RefFinal1, App),



    ok = delete_app(App, Handle),
    ok.


%%========================================================================
%% two_jobs_start_stop(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
two_jobs_start_stop(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------

    App  = ?SUNE,
    JobOne = "two_jobs_start_stop_1",
    JobTwo = "two_jobs_start_stop_2",

    {ok, Handle} = create_app(App),

    %%-------------------------------------------------------------
    %% create job 1 stopped
    %%-------------------------------------------------------------

    GrpRef = ?EVENT_GRP_1,

    Data1Stop = get_job_rec(JobOne, [{eventGroupRef,     GrpRef},
				     {requestedJobState, ?STOPPED}]), 
    %% Because job is stopped no reply is received
    ok = create_job(Data1Stop),

    %%-------------------------------------------------------------
    %% create job 2 active
    %%-------------------------------------------------------------

    TypeRef = lists:append([?EVENT_TYPE_2, ?EVENT_TYPE_3]),

    Data2Act = get_job_rec(JobTwo, [{eventTypeRef, TypeRef}]), 
    ExpData2Act = #peiEventJob{jobId = JobTwo, 
			       types = [2, 3]},

    {ok, RefJobTwoAct} = expected(App, Handle, [ExpData2Act]),
    ok = create_job(Data2Act),
    wait_expected_result(RefJobTwoAct, App),

    %%-------------------------------------------------------------
    %% activate job 1
    %%-------------------------------------------------------------
    Data1Act = Data1Stop#eventJob{requestedJobState = ?ACTIVE}, 
    ExpData1Act = #peiEventJob{jobId = JobOne,
			       types = [1]},

    {ok, RefJobOneAct} = expected(App, Handle, [ExpData1Act]),
    ok = create_job(Data1Act),
    wait_expected_result(RefJobOneAct, App),


    %%-------------------------------------------------------------
    %% stop job 2
    %%-------------------------------------------------------------

    Data2Stop = Data2Act#eventJob{requestedJobState = ?STOPPED}, 
    ExpData2Stop = #peiEventJob{jobId       = JobTwo,
				reqJobState = ?STOPPED,
				types       = [2, 3]},

    {ok, RefJobTwoStop} = expected(App, Handle, [ExpData2Stop]),
    ok = create_job(Data2Stop),
    wait_expected_result(RefJobTwoStop, App),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------

    %% job is already stopped -> no message is received
    {ok, RefFinal2} = expected(App, Handle, ?FIN_DATA(JobTwo)),
    ok = delete_jobs(Data2Stop),
    wait_expected_result(RefFinal2, App),



    ExpData1F = [#peiEventJob{jobId       = JobOne,
			      reqJobState = ?STOPPED,
			      types       = [1]},
		 #peiEventJob{jobId       = JobOne,
			      reqJobState = ?STOPPED,
			      types       = []}],
    {ok, RefFinal1} = expected(App, Handle, ExpData1F),
    ok = delete_jobs(Data1Stop),
    wait_expected_result(RefFinal1, App),



    ok = delete_app(App, Handle),
    ok.


%%========================================================================
%% file(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
file(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------

    App = ?SUNE,
    Job = "file",

    {ok, Handle} = create_app(App),

    GrpRef = ?EVENT_GRP_1,

    Data = get_job_rec(Job, [{eventGroupRef,       GrpRef},
			     {fileOutputEnabled,   true},
			     {reportingPeriod,     ?FIFTEEN_MIN},
			     {fileCompressionType, ?GZIP}
			    ]), 

    Types   = [1],
    ExpData = #peiEventJob{jobId    = Job,
			   fileCtrl = fileCtrl(Data),
			   types    = Types},

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
%% file_fail(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
file_fail(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------

    App = ?SUNE,
    Job = "file_fail",

    {ok, Handle} = create_app(App),

    GrpRef = ?EVENT_GRP_1,

    %% No reporting period
    Data = get_job_rec(Job, [{eventGroupRef,       GrpRef},
			     {fileOutputEnabled,   true},
			     {fileCompressionType, ?GZIP}
			    ]), 

    ok = create_job(Data, error),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    ok = delete_app(App, Handle),
    ok.


%%========================================================================
%% stream(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
stream(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------

    App = ?SUNE,
    Job = "stream",

    {ok, Handle} = create_app(App),

    GrpRef = ?EVENT_GRP_1,

    Data = get_job_rec(Job, [{eventGroupRef,              GrpRef},
			     {streamOutputEnabled,        true},
			     {streamDestinationIpAddress, "101.101.20.34"},
			     {streamDestinationPort,      2121},
			     {streamCompressionType,      ?GZIP}
			    ]), 

    Types   = [1],
    ExpData = #peiEventJob{jobId      = Job,
			   streamCtrl = streamCtrl(Data),
			   types      = Types},

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
%% stream_fail(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
stream_fail(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------

    App = ?SUNE,
    Job = "stream_fail",

    {ok, Handle} = create_app(App),

    GrpRef = ?EVENT_GRP_1,

    %% No IpAddress
    Data = get_job_rec(Job, [{eventGroupRef,              GrpRef},
			     {streamOutputEnabled,        true},
			     {streamDestinationPort,      2121},
			     {streamCompressionType,      ?GZIP}
			    ]), 

    ok = create_job(Data, error),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------

    ok = delete_app(App, Handle),
    ok.


%%========================================================================
%% filter(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
filter(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------

    App = ?SUNE,
    Job = "filter",

    {ok, Handle} = create_app(App),

    GrpRef  = ?EVENT_GRP_1,
    Filters = [{"name", "value"},
	       {"sune", "berit"}],

    Data = get_job_rec(Job, [{eventGroupRef, GrpRef},
			     {eventFilter,   Filters}
			    ]), 

    Types   = [1],
    ExpData = #peiEventJob{jobId     = Job,
			   filterIds = Filters,
			   types     = Types},
	       
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
%% mod_ref(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
mod_ref(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------

    App = ?SUNE,
    Job = "mod_ref",

    {ok, Handle} = create_app(App),

    GrpRef = ?EVENT_GRP_1,
    
    Data = get_job_rec(Job, [{eventGroupRef, GrpRef}]), 
    
    Types   = [1],
    ExpData = #peiEventJob{jobId = Job,
			   types = Types},
    
    {ok, RefJob} = expected(App, Handle, [ExpData]),
    ok = create_job(Data),
    wait_expected_result(RefJob, App),
    
    %%-------------------------------------------------------------
    %% stop job
    %%-------------------------------------------------------------
    DataS = Data#eventJob{requestedJobState = ?STOPPED}, 
    ExpDataS = #peiEventJob{jobId       = Job,
			    reqJobState = ?STOPPED,
			    types       = [1]},
    
    {ok, RefJobS} = expected(App, Handle, [ExpDataS]),
    ok = create_job(DataS),
    wait_expected_result(RefJobS, App),

    %%-------------------------------------------------------------
    %% change from group ref to type ref
    %%-------------------------------------------------------------
    TypeRef = lists:append([?EVENT_TYPE_2, ?EVENT_TYPE_3]),

    DataUp = get_job_rec(Job, [{eventGroupRef, undefined},
			       {eventTypeRef,  TypeRef}]), 
    TypesUp = [2, 3],
    
    ExpDataUp = ExpData#peiEventJob{types = TypesUp},
    
    {ok, RefJobUp} = expected(App, Handle, [ExpDataUp]),
    ok = update_job(DataUp),
    wait_expected_result(RefJobUp, App),
    
    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?FIN_DATA(Job, ExpDataUp)),
    ok = delete_jobs(DataUp),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    ok.


%%========================================================================
%% mod_stream(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
mod_stream(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------

    App = ?SUNE,
    Job = "mod_stream",

    {ok, Handle} = create_app(App),

    GrpRef = ?EVENT_GRP_1,
    
    DataInit = get_job_rec(Job, [{eventGroupRef,              GrpRef},
				 {streamOutputEnabled,        true},
				 {streamDestinationIpAddress, "101.101.20.34"},
				 {streamDestinationPort,      2121},
				 {streamCompressionType,      ?GZIP}
				]), 
    ct:pal("+++ Create job  ~n~p~n", [DataInit]),
    
    Types   = [1],
    ExpDataInit = #peiEventJob{jobId      = Job,
			       streamCtrl = streamCtrl(DataInit),
			       types      = Types},
    
    {ok, RefJobInit} = expected(App, Handle, [ExpDataInit]),
    ok = create_job(DataInit),
    wait_expected_result(RefJobInit, App),
    
    %%-------------------------------------------------------------
    %% stop job
    %%-------------------------------------------------------------
    DataStop = DataInit#eventJob{requestedJobState = ?STOPPED}, 
    ct:pal("+++ Stop job ~n~p~n", [DataStop]),
    ExpDataStop = #peiEventJob{jobId       = Job,
			       streamCtrl  = streamCtrl(DataInit),
			       reqJobState = ?STOPPED,
			       types       = [1]},
    
    {ok, RefJobStop} = expected(App, Handle, [ExpDataStop]),
    ok = update_job(DataStop),
    wait_expected_result(RefJobStop, App),

    %%-------------------------------------------------------------
    %% remove stream
    %%-------------------------------------------------------------
    DataRm = get_job_rec(Job, [{eventGroupRef,              GrpRef},
			       {streamOutputEnabled,        false},
			       {streamDestinationIpAddress, undefined},
			       {streamDestinationPort,      undefined},
			       {streamCompressionType,      undefined},
			       {requestedJobState,          ?STOPPED}
			      ]), 
    
    ct:pal("+++ Remove stream  ~n~p~n", [DataRm]),
    ExpDataRm = ExpDataStop#peiEventJob{streamCtrl = streamCtrl(DataRm)},
    
    {ok, RefJobRm} = expected(App, Handle, [ExpDataRm]),
    ok = update_job(DataRm),
    wait_expected_result(RefJobRm, App),
    
    %%-------------------------------------------------------------
    %% add stream
    %%-------------------------------------------------------------
    DataAdd = get_job_rec(Job, [{eventGroupRef,              GrpRef},
				{streamOutputEnabled,        true},
				{streamDestinationIpAddress, "101.1.1.123"},
				{streamDestinationPort,      10048},
				{streamCompressionType,      undefined},
				{requestedJobState,          ?STOPPED}
			    ]), 
    ct:pal("+++ Add stream~n~p~n", [DataAdd]),

    ExpDataAdd = ExpDataRm#peiEventJob{streamCtrl = streamCtrl(DataAdd)},
    
    {ok, RefJobAdd} = expected(App, Handle, [ExpDataAdd]),
    ok = update_job(DataAdd),
    wait_expected_result(RefJobAdd, App),


    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    ct:pal("+++ Delete job~n"),
    {ok, RefFinal} = expected(App, Handle, ?FIN_DATA(Job)),
    ok = delete_jobs(DataAdd),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    ok.



%%========================================================================
%% mod_filter(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
mod_filter(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------

    App = ?SUNE,
    Job = "mod_filter",

    {ok, Handle} = create_app(App),

    GrpRef  = ?EVENT_GRP_1,
    Filters = [{"name", "value"},
	       {"sune", "berit"}],

    DataInit = get_job_rec(Job, [{eventGroupRef, GrpRef},
				 {eventFilter,   Filters}
				]), 
    
    Types   = [1],
    ExpData = #peiEventJob{jobId     = Job,
			   filterIds = Filters,
			   types     = Types},
	       
    {ok, RefJob} = expected(App, Handle, [ExpData]),
    ok = create_job(DataInit),
    wait_expected_result(RefJob, App),

    %%-------------------------------------------------------------
    %% stop job
    %%-------------------------------------------------------------
    DataStop = DataInit#eventJob{requestedJobState = ?STOPPED}, 
    ExpDataStop = ExpData#peiEventJob{jobId       = Job,
				      reqJobState = ?STOPPED,
				      types       = [1]},
    
    {ok, RefJobStop} = expected(App, Handle, [ExpDataStop]),
    ok = create_job(DataStop),
    wait_expected_result(RefJobStop, App),
    

    %%-------------------------------------------------------------
    %% modify filter
    %%-------------------------------------------------------------
    FiltersMod = [{"sune", "berit"}],

    DataMod = get_job_rec(Job, [{eventGroupRef, GrpRef},
				{eventFilter,   FiltersMod}
			       ]), 
    
    ExpDataMod = ExpData#peiEventJob{filterIds = FiltersMod},
	       
    {ok, RefJobMod} = expected(App, Handle, [ExpDataMod]),
    ok = update_job(DataMod),
    wait_expected_result(RefJobMod, App),


    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?FIN_DATA(Job, ExpDataMod)),
    ok = delete_jobs(DataMod),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    ok.



%%========================================================================
%% rm_predef(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
rm_predef(_Config) ->
    Job = "PredefEventJob",
    Data = get_job_rec(Job, []), 
    ok = delete_job(Data, error),
    ok.


%%========================================================================
%% max_jobs(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
max_jobs(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------

    App  = ?SUNE,
    J1 = "max_jobs_1",
    J2 = "max_jobs_2",
    J3 = "max_jobs_3",
    J4 = "max_jobs_4",
    J5 = "max_jobs_5",
    J6 = "max_jobs_6",
    Jmax = "max_jobs_max",

    {ok, Handle} = create_app(App),

    %%-------------------------------------------------------------
    %% create OK jobs
    %%-------------------------------------------------------------
    JobData = [create_one_job(App, Handle, Job) 
	       || Job <- [J1, J2, J3, J4, J5, J6]],
    
						       
    %%-------------------------------------------------------------
    %% create Max job
    %%-------------------------------------------------------------
    GrpRef = ?EVENT_GRP_1,
    Data   = get_job_rec(Jmax, [{eventGroupRef, GrpRef}]), 
    ok     = create_job(?PRODUCER_2ND, Data, error),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
	       
    ok = delete_all_jobs(JobData, App, Handle),
    ok = delete_app(App, Handle).


%%========================================================================
%% two_producers(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
two_producers(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------

    App  = ?SUNE,
    JobOne = "two_producers_1",
    JobTwo = "two_producers_2",

    {ok, Handle} = create_app(App),

    %%-------------------------------------------------------------
    %% create job 1
    %%-------------------------------------------------------------
	       
    GrpRef  = ?EVENT_GRP_1,

    Data1    = get_job_rec(JobOne, [{eventGroupRef, GrpRef}]), 
    Types1   = [1],
    ExpData1 = #peiEventJob{jobId = JobOne, 
			     types = Types1},
    
    {ok, RefJobOne} = expected(App, Handle, [ExpData1]),
    ok = create_job(Data1),
    wait_expected_result(RefJobOne, App),


    %%-------------------------------------------------------------
    %% create job 2
    %%-------------------------------------------------------------
	       
    TypeRef = lists:append([?EVENT_TYPE_2, ?EVENT_TYPE_3]),

    Data2 = get_job_rec(JobTwo, [{eventTypeRef, TypeRef}]), 
    Types2   = [2, 3],
    ExpData2 = #peiEventJob{producerId = ?PRODUCER_2ND,
			    jobId      = JobTwo, 
			    types      = Types2},
    
    {ok, RefJobTwo} = expected(App, Handle, [ExpData2]),
    ok = create_job(?PRODUCER_2ND, Data2, ok),
    wait_expected_result(RefJobTwo, App),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
	       
    {ok, RefFinal2} = expected(App, Handle, ?FIN_DATA(JobTwo, ExpData2)),
    ok = delete_job(?PRODUCER_2ND, Data2, ok),
    wait_expected_result(RefFinal2, App),

    {ok, RefFinal1} = expected(App, Handle, ?FIN_DATA(JobOne, ExpData1)),
    ok = delete_jobs(Data1),
    wait_expected_result(RefFinal1, App),



    ok = delete_app(App, Handle),
    ok.



%%========================================================================
%% me_attrs(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
me_attrs(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    App = ?SUNE,
    Job = "me_attrs",

    {ok, Handle} = create_app(App),

    GrpRef = lists:append([?EVENT_GRP_1, ?EVENT_GRP_2]),

    Data = get_job_rec(Job, [{eventGroupRef, GrpRef}]), 

    Types   = [1, 2, 3],
    ExpData = #peiEventJob{jobId = Job, 
			   types = Types},
	       
    {ok, RefJob} = expected(App, Handle, [ExpData]),
    ok = create_job(Data),
    wait_expected_result(RefJob, App),

    X1 = rpc(ets,tab2list, [managedElement]),
    ct:pal("## X1 ~p~n", [X1]),

    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    {ok, RefJobMe1} = expected(App, Handle, [?ME_ATTR("ul", "dn")]),
    update_me("1", "ul", "dn"),
    wait_expected_result(RefJobMe1, App),

    X2 = rpc(ets,tab2list, [managedElement]),
    ct:pal("## X2 ~p~n", [X2]),

    {ok, RefJobMe2} = expected(App, Handle, [?ME_ATTR(undefined, "1")]),
    update_me("dn", undefined, "1"),
    wait_expected_result(RefJobMe2, App),

    X3 = rpc(ets,tab2list, [managedElement]),
    ct:pal("## X3 ~p~n", [X3]),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?FIN_DATA(Job, ExpData)),
    ok = delete_jobs(Data),
    wait_expected_result(RefFinal, App),

    ok = delete_app(App, Handle),
    ok.



%%========================================================================
%% job_first(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
job_first(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------


%%     GrpRef = lists:append([?EVENT_GRP_1, ?EVENT_GRP_2]),

%%     Data = get_job_rec(Job, [{eventGroupRef, GrpRef}]), 

%%     Types   = [1, 2, 3],
%%     ExpData = #peiEventJob{jobId = Job, 
%% 			   types = Types},
	       
%%     ok = create_job(Data),

%%     App = ?SUNE,
%%     Job = "job_first",

%%     {ok, RefJob} = expected(App, Handle, [ExpData]),
%%     {ok, Handle} = create_app(App),
%%     wait_expected_result(RefJob, App),


%%     %%-------------------------------------------------------------
%%     %% delete apps and jobs
%%     %%-------------------------------------------------------------
%%     {ok, RefFinal} = expected(App, Handle, ?FIN_DATA(Job, ExpData)),
%%     ok = delete_jobs(Data),
%%     wait_expected_result(RefFinal, App),

%%     ok = delete_app(App, Handle),
    ok.

%%========================================================================
%% predef_job(Config) -> ok.
%% 
%% @doc 
%% create and delete an event job 
%% @end
%%========================================================================
predef_job(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------

%%     App = ?SUNE,
%%     Job = "PredefEventJob",

%%     {ok, Handle} = create_app(App),

%%     GrpRef = lists:append([?EVENT_GRP_1, ?EVENT_GRP_2]),

%%     Data = get_job_rec(Job, [{eventGroupRef, GrpRef}]), 

%%     Types   = [1, 2, 3],
%%     ExpData = #peiEventJob{jobId = Job, 
%% 			   types = Types},
	       
%%     {ok, RefJob} = expected(App, Handle, [ExpData]),
%%     ok = create_job(Data),
%%     wait_expected_result(RefJob, App),

%%     %%-------------------------------------------------------------
%%     %% delete apps and jobs
%%     %%-------------------------------------------------------------
%%     {ok, RefFinal} = expected(App, Handle, ?FIN_DATA(Job, ExpData)),
%%     ok = delete_jobs(Data),
%%     wait_expected_result(RefFinal, App),

%%     ok = delete_app(App, Handle),
    ok.

%%========================================================================
%% show_jobs(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
show_jobs(_Config) ->
    %%-------------------------------------------------------------
    %% create apps and jobs
    %%-------------------------------------------------------------
    
    App = ?SUNE,
    Job = "show_jobs",

    {ok, Handle} = create_app(App),
    
    GrpRef = lists:append([?EVENT_GRP_1, ?EVENT_GRP_2]),
    
    Data = get_job_rec(Job, [{eventGroupRef, GrpRef}]), 
    
    Types   = [1, 2, 3],
    ExpData = #peiEventJob{jobId = Job, 
			   types = Types},
    
    {ok, RefJob} = expected(App, Handle, [ExpData]),
    ok = create_job(Data),
    wait_expected_result(RefJob, App),
    


    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, 
		 "ManagedElement=1,SystemFunctions=1,"
		 "PmEventM=1,EventProducer=first",
		 ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show", ?PRINT_OPT),
    ok  = check_result(["show_jobs"],
		       Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),

    %%-------------------------------------------------------------
    %% delete apps and jobs
    %%-------------------------------------------------------------
    {ok, RefFinal} = expected(App, Handle, ?FIN_DATA(Job, ExpData)),
    ok = delete_jobs(Data),
    wait_expected_result(RefFinal, App),
    
    ok = delete_app(App, Handle),


    ok.

%%========================================================================
%% show_groups(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
show_groups(_Config) ->
    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, 
		 "ManagedElement=1,SystemFunctions=1,"
		 "PmEventM=1,EventProducer=first",
		 ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show", ?PRINT_OPT),
    ok  = check_result(["EventGrp1"],
		       Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),
    ok.

%%========================================================================
%% show_types(Config) -> ok.
%% 
%% @doc 
%% 
%% @end
%%========================================================================
show_types(_Config) ->
    ok  = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, 
		 "ManagedElement=1,SystemFunctions=1,"
		 "PmEventM=1,EventProducer=first,EventGroup=EventGrp1",
		 ?PRINT_OPT),
    Rcv = rct_cli:send(?CLI_USER, "show", ?PRINT_OPT),
    ok  = check_result(["EvType1"],
		       Rcv),
    ok  = rct_cli:disconnect(?CLI_USER),
    ok.


%%========================================================================
%% Misc help functions
%%========================================================================

create_one_job(App, Handle, Job) ->
    
    GrpRef  = ?EVENT_GRP_1,

    Data    = get_job_rec(Job, [{eventGroupRef, GrpRef}]), 
    Types   = [1],
    ExpData = #peiEventJob{producerId = ?PRODUCER_2ND,
			   jobId      = Job, 
			   types      = Types},
    
    {ok, RefJob} = expected(App, Handle, [ExpData]),
    ok = create_job(?PRODUCER_2ND, Data, ok),
    wait_expected_result(RefJob, App),
    {Job, Data, ExpData}.


delete_all_jobs([], _App, _Handle) ->
    ok;
delete_all_jobs([{_Job, Data, ExpData} | T], App, Handle) ->
    {ok, RefFinal} = expected(App, Handle, ?FIN_DATA(Job, ExpData)),
    ok = delete_jobs(?PRODUCER_2ND, Data),
    wait_expected_result(RefFinal, App),
    delete_all_jobs(T, App, Handle).
    




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


%%========================================================================
%% fileCtrl(Job) -> {RP, Compression}
%% 
%% 
%% 
%% 
%%========================================================================
fileCtrl(#eventJob{fileOutputEnabled   = true,
		   reportingPeriod     = RP,
		   fileCompressionType = CT
		  }) ->
    {get_rp(RP), get_ct(CT)};
fileCtrl(_) ->
    undefined.
    

get_rp(?FIFTEEN_MIN) -> 900;
get_rp(_)            -> undefined.


get_ct("GZIP") -> 0;
get_ct(_)      -> undefined.

%%========================================================================
%% streamCtrl(Job) -> {RP, Compression}
%% 
%% 
%% 
%% 
%%========================================================================
streamCtrl(#eventJob{streamOutputEnabled        = true,
		     streamDestinationIpAddress = IpAddr,
		     streamDestinationPort      = Port,
		     streamCompressionType      = CT
		    }) ->
    {get_ct(CT), IpAddr, Port};
streamCtrl(_) ->
    undefined.


%%=====================================================================
%% wait until the expected string is received from CLI
%%=====================================================================
check_result([H | _] = Expected, Received) when is_list(H) ->
    cr(Expected, Received);
check_result(Expected, Received) ->
    check_result([Expected], Received).


cr([], {ok, _})->
    ok;
cr([], Error)->
    Error;
cr([Expected | T], Received)->
    Cont = cr_loop(Expected, Received, 10),
    cr(T, Cont).



cr_loop(Expected, {ok, Received}, N) when N > 0 ->
    Rcvd = re:replace(Received, "[<>]", "", [{return, list},global]),
    ct:pal("### Expected cr loop  ~p~n"
	   "### Received  ~p~n", [Expected, Rcvd]),
    case string:str(Rcvd, Expected) of
	0 ->
	    ct:pal("#### 00000~n"),
	    timer:sleep(1000),
	    Rec = rct_cli:send(?CLI_USER, ?TOP, ?PRINT_OPT),
	    cr_loop(Expected, Rec, N - 1);
	X ->
	    ct:pal("#### ~p~n ", [X]),
	    {ok, string:substr(Received, X)}
    end;
cr_loop(_, {ok, _}, _) ->
    {error, not_received};
cr_loop(_, Error, _) ->
    {error, Error}.




get_ct_config(TC, DEF) -> ?LIB:get_ct_config(TC, DEF).

start(A)    -> {ok, _} = ?LIB:start(A).
stop(A)     -> ok = ?ERL_APP:stop(A).   
cleanup()   -> ?LIB:cleanup().

initialize(A, B) -> ?LIB:initialize(A, B).
finalize(A, B)   -> ok = ?LIB:finalize(A, B).


create_app(A)    -> ?LIB:create_app(A).
delete_app(A, B) -> ?LIB:delete_app(A, B).


create_job(B)       -> ?LIB:create_job(?PRODUCER_DEF, B).
create_job(B, C)    -> ?LIB:create_job(?PRODUCER_DEF, B, C).
create_job(A, B, C) -> ?LIB:create_job(A, B, C).
delete_job(B, C)    -> ?LIB:delete_job(?PRODUCER_DEF, B, C).
delete_job(A, B, C) -> ?LIB:delete_job(A, B, C).
delete_jobs(A, B)   -> ?LIB:delete_jobs(A, B).
delete_jobs(B)      -> ?LIB:delete_jobs(?PRODUCER_DEF, B).
update_job(B)       -> ?LIB:update_job(?PRODUCER_DEF, B).

update_me(A, B, C) -> ?LIB:update_me(A, B, C).

expected(A, B, C)         -> ?LIB:expected(A, B, C).
wait_expected_result(A,B) -> ?LIB:wait_expected_result(A, B).

hooks() -> ?LIB:hooks().

%% open_trans()  -> ?LIB:open_trans().
%% close_trans() -> Res = ?LIB:close_trans(), to(4), Res.

rpc(A, B, C) -> ?LIB:rpc(A, B, C).
