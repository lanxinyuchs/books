%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms2_test_lib.erl %
%%% @version /main/R3A/R4A/R5A/R6A/7

%%% @doc
%%% == Example test suite for one sim or target env ==
%%% This Test Suite can be used only on pmssim.
%%% @end

-module(pms2_test_lib).
-include_lib("common_test/include/ct.hrl").
-include("pms2_test.hrl").

%%% ----------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% Rev        Date        Name        What
%%% -----      ----------  --------    ------------------------
%%% R3A/1      2014-09-04  uabesvi     Copied from pms_test_lib
%%% R4A/4      2015-12-03  erarafo     Support for fetching ift_app.log
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------


-export([start/1]).
-export([stop/1]).

-export([initialize/1]).
-export([initialize/2]).
-export([initialize/3]).

-export([rop_data/6]).
-export([rop_data_pmi/5,
         rop_data_pmi/6]).
-export([sc_data/6]).

-export([finalize/2]).

-export([ldn_aliases/1,
         ldn_aliases/2]).

-export([counter_map/3]).

-export([create_app/1]).
-export([create_app/2]).
-export([create_app/3]).
-export([delete_app/2]).

-export([create_app_pmi/1]).
-export([create_app_pmi/2]).
-export([create_app_pmi/3]).
-export([delete_app_pmi/2]).

-export([create_flex_app/1]).
-export([create_flex_app/2]).

-export([create_job/2]).
-export([create_job/3]).
-export([create_jobs/1]).
-export([delete_job/1]).
-export([delete_job/2]).
-export([delete_jobs/1]).
-export([update_job/2]).
-export([create_mr/4]).
-export([delete_mr/2]).

-export([create_job_mr/2]).
-export([create_job_mr/3]).

-export([create_grp_mt/3]).

-export([expected/3]).
-export([wait_expected_result/1]).
-export([wait_expected_result/2]).
-export([wait_expected_result/3]).
-export([wait_expected_error_result/2]).

-export([cleanup/0]).

-export([open_trans/0]).
-export([close_trans/0]).

-export([get_table/1]).
-export([tables/0]).
-export([check_tables/1]).
-export([wait_table_size/2]).
-export([wait_table_size/3]).

-export([rop_file_handling/1]).

-export([get_processes/0]).
-export([get_processes/1]).
-export([check_processes/1]).
-export([check_processes/2]).

-export([get_aliases/0]).

-export([check_rop_file_name/2]).
-export([check_rop_file_name/3]).
-export([check_rop_file/2]).
-export([check_rop_file/3]).
-export([wait_one_gp/1]).
-export([get_noof_rop_files/0]).
-export([wait_subscribe/1]).
-export([wait_until_subscribe/1]).
-export([wait_report_rop/2]).
-export([wait_report_rop/3]).
-export([get_subscribe_data/2]).
-export([wait_pmi_subscribe/1]).
-export([wait_until_pmi_subscribe/1]).
-export([wait_pmi_report_rop/2]).
-export([get_mrs/1, get_mrs/3]).
-export([aliasify_values/1]).
-export([aliasify_flex_mts/1]).

-export([subscribe_testnode/0]).
-export([unsubscribe_testnode/0]).

-export([rpc/3]).
-export([host/0]).
%%-export([module/0]).
-export([hooks/0]).
-export([get_ct_config/1,
         get_ct_config/2]).

-export([log_te_log/3,
         relative_file_path/1]).

-export([cancel_timer/1]).


-define(L2B(__L), list_to_binary(__L)).

-define(TESTNODE, testnode).
-define(ERL_API, true).

-define(PMI2_ERL_APP, pms2_erl_app).
-define(PMI_ERL_APP,  pmi_erl_app).
-define(PROXY,   pms_pmi_proxy).
-define(ROP_HOOK, pms_rop_hook).

-define(LDN_TN,     ["ManagedElement=1,Transport=1"]).

-define(PM_GRP_DEF, ["Group1", "Group2"]).


-define(SLEEP,  500).


-define(MR_SPEC(__Grp),
        [{<<"groupRef">>,
          {11, ?L2B("ManagedElement=1,"
                    "SystemFunctions=1,"
                    "Pm=1,"
                    "PmGroup=" ++ __Grp)}},
         {<<"measurementTypeRef">>,
          {11, ?L2B("")}}]).

-define(MR_SPEC(__Grp, __MT),
        [{<<"groupRef">>,
          {11, ?L2B("")}},
         {<<"measurementTypeRef">>,
          {11, ?L2B("ManagedElement=1,"
                    "SystemFunctions=1,"
                    "Pm=1,"
                    "PmGroup=" ++ __Grp ++ "," ++
                    "MeasurementType=" ++ __MT)}}]).


-define(Aggregation_SUM, 2).

-record(pmGroup, {pmGroupId,
                  category,
                  consistentData,
                  generation,
                  switchingTechnology,
                  validity,
                  moClass,
                  description,
                  pmGroupVersion}).

-record(measurementType, {measurementTypeId,
                          measurementName,
                          size,
                          collectionMethod,
                          description,
                          condition,
                          aggregation = ?Aggregation_SUM,
                          measurementStatus,
                          measurementResult,
                          multiplicity = 1,
                          initialValue,
                          resetAtGranPeriod,
                          derSampleRate,
                          fmAlarmType,
                          thresholdDirection,
                          isCompressed}).

%%% #---------------------------------------------------------
%%% #3.2   EXPORTED FUNCTIONS
%%% #---------------------------------------------------------




%%========================================================================
%% initialize(App) -> ok | {error, Reason}
%% initialize(App, CbFlags) -> ok | {error, Reason}
%% initialize(App, CbFlags, CounterMap) -> ok | {error, Reason}
%%========================================================================
%% Use default groups
initialize(App) ->
    initialize(App, ?CB_DEF).

initialize(App, CbFlags) ->
    ?PMI2_ERL_APP:pmi2Initialize(App, CbFlags, ?COUNTER_MAP_DEF).

initialize(App, CbFlags, CounterMap) ->
    ?PMI2_ERL_APP:pmi2Initialize(App, CbFlags, CounterMap).

initialize_pmi(App, Groups) ->
    ?PMI_ERL_APP:pmiInitialize(App, Groups).

initialize_2_pmi(App, Groups, TopLdn) ->
    ?PMI_ERL_APP:pmiInitialize_2(App, Groups, TopLdn).

%%========================================================================
%% finalize(App) -> ok | {error, Reason}
%%========================================================================
finalize(App, Handle) ->
    ct:pal("$$$$ FINALIZE ~n"),
    ?PMI2_ERL_APP:pmi2Finalize(App, Handle).


finalize_pmi(App, Handle) ->
    ct:pal("$$$$ FINALIZE ~n"),
    ?PMI_ERL_APP:pmiFinalize(App, Handle).

%%========================================================================
%% rop_data(App, Handle, GP, ReportId, ValueBundle, FF) ->
%%    ok | {error, Reason}
%%
%% If GP == gp use the same GP as set in erl app, otherwise use the
%% specified value in the GP attribute.
%%========================================================================
rop_data(App, Handle, GP, ReportId, ValueBundle, FF) ->
    ct:pal("#### pms2_test_lib  rop_data ~n"),
    ?PMI2_ERL_APP:pmi2DataRop(App, Handle, GP, ReportId, ValueBundle, FF).


rop_data_pmi(App, Handle, GP, ReportId, Values) ->
    ct:pal("#### pms2_test_lib  rop_data_pmi ~n"),
    ?PMI_ERL_APP:pmiDataRop(App, Handle, GP, ReportId, Values).

rop_data_pmi(App, Handle, GP, ReportId, MOLDN, Values) ->
    ct:pal("#### pms2_test_lib  rop_data_pmi ~n"),
    ?PMI_ERL_APP:pmiDataRop(App, Handle, GP, ReportId, MOLDN, Values).

%%========================================================================
%% sc_data(App, Handle, GP, ReportId, ValueBundle, FF) ->
%%    ok | {error, Reason}
%%
%% If GP == gp use the same GP as set in erl app, otherwise use the
%% specified value in the GP attribute.
%%========================================================================
sc_data(App, Handle, GP, ReportId, ValueBundle, FF) ->
    ?PMI2_ERL_APP:pmi2DataSc(App, Handle, GP, ReportId, ValueBundle, FF).


%%========================================================================
%% counter_map(App, Handle, Maps) -> ok | {error, Reason}
%%========================================================================
counter_map(App, Handle, Maps) ->
    ?PMI2_ERL_APP:pmi2CounterMap(App, Handle, Maps).



%%========================================================================
%% ldn_aliases(App, Ldns) -> {ok, [{Ldn, Alias}]} | {error, Reason}
%%========================================================================
ldn_aliases(App) ->
    ldn_aliases(App, ?LDN_DEF(App)).

ldn_aliases(App, Ldns) ->
    Aliases = [la(rpc(gmfI, get_integer, [Ldn]), Ldn) || Ldn <- Ldns],
    ?PMI2_ERL_APP:aliases(App, Aliases),
    {ok, Aliases}.

la({ok, Alias}, Ldn) ->
    {Ldn, Alias}.



%%========================================================================
%% expected(App, Handle, Expected) -> Ref | {error, Reason}
%%
%% App           - string()   The application name
%% Handle        - term()     Handle received from initialize
%% Expected      - [Expect]   List of expected callbacks
%% Expect        - Action | {Action, [Options]} | {Action, Times, [Options]}
%% Action        - pmi2SubscribeRop | pmi2ReportRop | pmi2ReportShowCounters |
%%                 no_subscribe_request | no_rop_report_request | send_copy |
%%                 relay
%% Times         - {repeat, N} | {ignore, N}
%% N             - integer() | wait_until_subscribe | wait_until_show_counters
%% Options       - {gp, GP} | {spec, [Spec]} | {values, [Values]} |
%%                 {reportId, ReportId} | {ff, FinalFragment} |
%%                 {delay, DelayTime}
%%                 'gp' 'spec' are only valid for pmi2SubscribeRop
%%                 'values' 'reportId' 'ff' are only valid for pmi2ReportRop
%%                 'delay' is valid for all messages
%% GP            - integer() Granularity period in seconds,
%% Spec          - {GroupAlias, [MtAlias]}
%% GroupAlias    - integer()
%% MtAlias       - integer()
%% Values        - {GroupAlias, [{LdnAlias, [{MtAlias, [Value]}]}]} |
%%                 {OverrideFF, {GroupAlias, [{LdnAlias, [{MtAlias, [Value]}]}]}
%% LdnAlias      - integer()
%% Value         - integer()
%% OverrideFF    - bool()
%%                 If defined, this attribute overrides 'ff' in Options
%% DelayTime     - integer()
%%                 Time in milliseconds the reply will be delayed with
%% ReportId      - integer()   Replace the actual reportId with this value
%% FinalFragment - boolean()
%% Ref           - Reference to be used in wait_expected_results
%%
%% Expect: Action is equal to {Action, {repeat, 1}, []}
%%         {Action, [Options]} is equal to {Action, {repeat, 1}, [Options]}
%%
%% Action: Normally wait for subscribe or report rop requests.
%%         - no_subscribe_request: there should not be any subscribe request.
%%           Used when no application has announced the groups.
%%         - no_rop_report_request there should not be any report rop
%%           request for the next GP.
%%           no_rop_report_request is useful when checking that no report
%%           rop request is received when there are no measurements
%%           ongoing
%%         - send_copy, the erl app sends a copy of all received messages
%%           to the test suite
%%         - relay, relay all messages to the test case
%% Times:  If the same message is to received several times use
%%         {repeat, N} where N indicates the number of times the message
%%         is to be received.
%%         wait_until_subscribe and wait_until_show_counters can only be
%%         used together with pmi2ReportRop.
%%         These are used when there is at least one active job
%%         and waiting for a new subsrcribe request or for a show counters
%%         request, respectively.
%%         {ignore, N} means that the next N messages of type Action shall
%%         be ignored and no reply message is to be replied to PMS.
%% Values: List of measurement values.
%%         If Values is a list of Values each Value will be sent in a
%%         separate pmi2RopData message
%%
%%========================================================================
expected(App, Handle, Expected) ->
    ?PMI2_ERL_APP:expected(App, Handle, [cb_format(Cb) || Cb <- Expected]).

cb_format({_, _, _} = Cb) ->
    Cb;
cb_format({Fnc, Opt}) ->
    {Fnc, {repeat, 1}, Opt};
cb_format(Fnc) when is_atom(Fnc) ->
    {Fnc, {repeat, 1}, []}.







%%========================================================================
%% wait_expected_result([{Ref, App}]) -> ok | ct:fail()
%% wait_expected_result(Ref, App) -> ok | ct:fail()
%%
%% wait that the erl app replies with the result of the expected call
%%========================================================================
wait_expected_result(Apps) when is_list(Apps) ->
    wait_expected_result(Apps, []).


wait_expected_result(Ref, App) when is_atom(App) ->
    wait_expected_result([{Ref, App}]);
wait_expected_result(Apps, Time) when is_list(Apps), is_integer(Time) ->
    wer(Apps, [], Time);
wait_expected_result(Apps, Rops) when is_list(Apps), is_list(Rops) ->
    wer(Apps, Rops, 120000).


wait_expected_result(Ref, App, Time) when is_atom(App), is_integer(Time) ->
    wait_expected_result([{Ref, App}], Time);
wait_expected_result(Apps, Rops, Time)
  when is_list(Apps), is_list(Rops), is_integer(Time) ->
    wer(Apps, Rops, Time).


wer([], [], _Timeout) ->
    ct:pal("%%##### wait_results OKOKOK ~p ~n", [ok]),
    ok;
wer(Apps, Rops, Timeout) ->
    ct:pal("%%##### wait_results waiting ~p ~n", [{Apps, Rops}]),
    receive
        {expected_result, Ref, App, ok} = Msg ->
            case {lists:member({Ref, App}, Apps),
                  lists:delete({Ref, App}, Apps)} of
                {true, RemApps} ->
                    ct:pal("%%##### wait_results 1 true ~p ~n", [RemApps]),
                    wer(RemApps, Rops, Timeout);
                {false, _} ->
                    ct:pal("%%##### wait_results 1 ~p ~n", [cb_false]),
                    ct:fail({error, {not_found, Msg, Apps, Rops}})
            end;
        {rop_result, Ref, Rop, ok} = Msg ->
            case {lists:member({Ref, Rop}, Rops),
                  lists:delete({Ref, Rop}, Rops)} of
                {true, RemRops} ->
                    wer(Apps, RemRops, Timeout);
                {false, _} ->
                    ct:pal("%%##### wait_results 2 ~p ~n", [rop_false]),
                    ct:fail({error, {not_found, Msg, Apps, Rops}})
            end;
        {Reply, _UnknownRef, AppOrRop, Error} = Msg ->
            ct:pal("%%##### wait_results 3 Msg ~p  Apps ~p ~n", [Msg, Apps]),
            ct:fail({error, {AppOrRop, Reply, Error}})
    after Timeout ->
            ct:pal("%%##### wait_results 4 ~p ~n", [timeout]),
            ct:fail({error, {timeout, Apps, Rops}})
    end.







%%========================================================================
%% wait_expected_error_result(App, Expected) -> ok | ct:fail()
%%
%%
%%
%%========================================================================
wait_expected_error_result(App, Expected) ->
    weer(App, [cb], Expected, 20000).


weer(App, Replies, Expected, Timeout) ->
    receive
        {expected_result, _, App, ok} = Reply ->
            case lists:member(cb, Replies) of
                true ->
                    ct:fail({error, {App, Reply, Expected}});
                false ->
                    weer(App,
                         lists:delete(cb, Replies),
                         Expected,
                         Timeout)
            end;
        {rop_result, _, App, ok} = Reply ->
            case lists:member(rop, Replies) of
                true ->
                    ct:fail({error, {App, Reply, Expected}});
                false ->
                    weer(App,
                         lists:delete(cb, Replies),
                         Expected,
                         Timeout)
            end;
        {expected_result, _, App, {error, {Expected, _}}} ->
            ok;
        {expected_result, _, App, Error} ->
            ct:fail({error, {App, expected_result, Error}});
        {rop_result, _, App, {error, {Expected, _}}} ->
            ok;
        {rop_result, _, App, Error} ->
            ct:fail({error, {App, rop_result, Error}});
        X ->
            ct:pal("$$$$$$  ~p~n", [X])

    after Timeout ->
            ct:fail({error, {App, Replies, timeout}})
    end.




%%========================================================================
%% cleanup() -> ok
%%
%% delete any hanging pmsJob and pmsJobGroup processes and
%% cleanup pmJob and meaurementReader tables
%%========================================================================
cleanup() ->
    delete_old_job_tables(),
    kill_old_job_procs(),
    ok.


%%========================================================================
%% get_aliases() ->
%%========================================================================
get_aliases() ->
    timer:sleep(2000),
    rpc(pmsDebug, get_aliases, []).


subscribe_testnode() ->
    ?PROXY:subscribe_testnode().

unsubscribe_testnode() ->
    ?PROXY:unsubscribe_testnode().




%%========================================================================
%% create_app(App) -> ok.
%% create_app(App, Groups) -> ok.
%% create_app(App, Groups, LDNs) -> ok.
%%
%% App       = AppName | {AppName, [Opt]}
%% AppName   = atom()
%% Opt       = {Action, [ActionOpt]}
%% Action    = atom()
%% ActionOpt = term()
%% Groups    = Group | [Group]
%% Group     = atom()
%% LDNs      = [LdnAlias]
%% LdnAlias  = integer()
%%
%% For Opt:
%%   Action =  expected - see expected/3 Expected attribute
%%          /= expected - see pms_erl_app:init/3 code for valid actions
%%
%%   Groups =  undefined - the group aliases are specified in
%%                         pmi2Initialize counterMapId
%%                         (no pmi2CounterMap messages will be sent)
%%          /= undefined - group aliases are specified by sending
%%                         pmi2CounterMap messages
%%
%% @doc
%% start an app
%% @end
%%========================================================================
create_app(App) ->
    %% create_app(App, ?CM_GRP_DEF).
    create_app(App, undefined).

create_app({App, _} = AppData, Groups) ->
    create_app(AppData, Groups, ?LDN_DEF(App));
create_app(App, Groups) ->
    create_app(App, Groups, ?LDN_DEF(App)).

create_app(AppData, undefined, LDNs) ->
    %%-------------------------------------------------------------
    %% initialize with CounterMapId
    %%-------------------------------------------------------------
    {App, Opts} = case AppData of
                      {A, Opt} -> {A, Opt};
                      A        -> {A, []}
                  end,
    start({App, Opts}),
    Res = {ok, _Handle} = initialize(App, ?CB_DEF),
    ldn_aliases(App, LDNs),
    %% {ok, Handle}.
    Res;

create_app(AppData, Groups, LDNs) ->
    %%-------------------------------------------------------------
    %% initialize with CounterMap function.
    %%-------------------------------------------------------------
    {App, Opts} = case AppData of
                      {A, Opt} -> {A, Opt};
                      A        -> {A, []}
                  end,
    ct:pal("#### create_app  ~p~n", [{App, Opts}]),
    start({App, Opts}),
    Res = {ok, Handle} = initialize(App, ?CB_DEF, undefined),
    counter_map(App, Handle, Groups),
    ldn_aliases(App, LDNs),
    Res.



%%========================================================================
%% create_flex_app(App) -> ok.
%% create_flex_app(App, CounterMapId) -> ok.
%%
%% App       = AppName | {AppName, [Opt]}
%% AppName   = atom()
%%
%% @doc
%% start a flex app
%% @end
%%========================================================================
create_flex_app(App) ->
    create_flex_app(App, undefined).

create_flex_app(AppData, CounterMapId) ->
    ct:pal("###### create_flex_app  ~p   ~p~n", [AppData, CounterMapId]),
    %%-------------------------------------------------------------
    %% initialize with CounterMapId
    %%-------------------------------------------------------------
    {App, Opts} = case AppData of
                      {A, Opt} -> {A, Opt};
                      A        -> {A, [{expected, [relay]}]}
                  end,
    start({App, Opts}),
    Res = {ok, _Handle} = initialize(App, ?CB_DEF, CounterMapId),
    ldn_aliases(App, ?LDN_DEF(App)),
    Res.

%%========================================================================
%% create_app_pmi(App) -> ok.
%%
%% @doc
%% start an app (pmi) and create Job
%% @end
%%========================================================================
create_app_pmi(App) ->
    create_app_pmi(App, [?Group1]).

create_app_pmi({Name, _Opts} = App, Groups) ->
    start_pmi(App),
    {ok, _Handle} = initialize_pmi(Name, Groups);
create_app_pmi(App, Groups) ->
    start_pmi(App),
    {ok, _Handle} = initialize_pmi(App, Groups).

create_app_pmi({Name, _Opts} = App, Groups, TopLdn) ->
    start_pmi(App),
    {ok, _Handle} = initialize_2_pmi(Name, Groups, TopLdn);
create_app_pmi(App, Groups, TopLdn) ->
    start_pmi(App),
    {ok, _Handle} = initialize_2_pmi(App, Groups, TopLdn).


%%========================================================================
%% delete_app(App, Handle) -> ok.
%%
%% @doc
%% delete an app and check all is ok
%% @end
%%========================================================================
delete_app(App, Handle) ->
    finalize(App, Handle),
    ok.

%%========================================================================
%% delete_app_pmi(App, Handle) -> ok.
%%
%% @doc
%% delete an app (pmi) and check all is ok
%% @end
%%========================================================================
delete_app_pmi(App, Handle) ->
    finalize_pmi(App, Handle),
    ok.

%%========================================================================
%% create_job(JobName, [MR])         -> ok | {error, Reason}
%% create_job(JobName, [MR], [Attr]) -> ok | {error, Reason}
%%
%%  JobName = string()
%%  MR      = {MrName, PmGrp, MeasType}
%%  Attr    = {AttrName, AttrVal}
%%
%% @doc
%% create job
%% @end
%%========================================================================
create_job(JobName, MR) ->
    create_jobs([{JobName, MR}]).

create_job(JobName, MR, Attrs) ->
    create_jobs([{JobName, MR, Attrs}]).

%%========================================================================
%% create_jobs([{JobName, [MR]} | {JobName, [MR], [Attr]}]) -> ok | {error, Reason}
%%
%%  JobName = string()
%%  MR      = {MrName, PmGrp, MeasType}
%%  Attr    = {AttrName, AttrVal}
%%
%% @doc
%% create jobs
%% @end
%%========================================================================
create_jobs(Jobs) ->
    {ok, _} = open_trans(),
    lists:foreach(fun cj_attrs/1, Jobs),
    close_trans().


cj_attrs({Job, JobGrp}) ->
    cj(Job, cj_get_job_defs(Job, JobGrp), ?DEF_JOB_ATTRS);
cj_attrs({Job, JobGrp, Attrs}) ->
    cj(Job, cj_get_job_defs(Job, JobGrp), Attrs).


cj_get_job_defs(_Job, ?JOB_NO_MR) ->
    [];
cj_get_job_defs(Job, ?JOB_NO_SR) ->
    [{Job ++ "_mr1", []}];
cj_get_job_defs(Job, ?JOB_GROUP_1) ->
    [{Job ++ "_mr1", "Group1", "Type1"}];
cj_get_job_defs(Job, ?JOB_GROUP_2) ->
    [{Job ++ "_mr1", "Group2", "Type2"},
     {Job ++ "_mr2", "Group2", "Type3"}];
cj_get_job_defs(_Job, JobGrps) ->
    JobGrps.

cj(Job, JobDefinition, Attrs) ->
    %%-------------------------------------------------------------
    %% create a PM Job and a MR
    %%-------------------------------------------------------------
    create_job_mr(Job, JobDefinition, Attrs).

    %%-------------------------------------------------------------
    %% check the tables
    %%-------------------------------------------------------------
%%     ok = wait_table_size(pmJob, 1),
%%     ok = wait_table_size(measurementReader, length(JobDefinition)),

%%     ok.


%%========================================================================
%% delete_jobs(App, Job, Handle) -> ok.
%%
%% @doc
%% delete an app and the created Job
%% @end
%%========================================================================
delete_jobs([Job | _] = Jobs) when is_list(Job) ->
    dj(Jobs);
delete_jobs(Job) ->
    dj([Job]).

dj(Jobs) ->
    LJ  = length(get_table(pmJob)),
    LMR = length(get_table(measurementReader)),

    MRTab  = get_table(measurementReader),
    JobIds = [element(4, element(2, MR)) || MR <- MRTab],
    MRs    = [JobId || JobId <- JobIds, lists:member(JobId, Jobs)],

    %%-------------------------------------------------------------
    %% delete the pm jobs
    %%-------------------------------------------------------------
    {ok, _} = open_trans(),
    DelJobs = [delete_job(Job) || Job <- Jobs],
    ok = close_trans(),

    ok = wait_table_size(pmJob, LJ - length(Jobs)),
    ok = wait_table_size(measurementReader, LMR - length(MRs)),

    case lists:usort(DelJobs) of
        [ok] -> ok;
        Sort -> {error, Sort}
    end.


%%========================================================================
%% delete_job
%%
%%
%%========================================================================
delete_job(Job) ->
    delete_job(Job, [{"requestedJobState", ?STOPPED}]).

delete_job(Job, Opts) ->
    update_job_trans(Job, Opts),
    ct:pal("*** PMS <--- SUITE *** delete Job: ~p~n", [Job]),
    Res = pms_netconf:delete_job(Job),
    to(),
    Res.

%%========================================================================
%% update_job
%%
%%
%%========================================================================
update_job_trans(App, Attrs) ->
    Res = pms_netconf:create_job(App, Attrs),
    to(),
    Res.

%%========================================================================
%% update_job
%%
%%
%%========================================================================
update_job(App, Attrs) ->
    {ok, _} = open_trans(),
    Res = pms_netconf:create_job(App, Attrs),
    ok = close_trans(),
    to(),
    Res.


%%========================================================================
%% create_job_mr(Job, MR, Attrs) -> ok | {error, Reason}
%%
%% Job   = string()  Name of the job
%% MR    = [{MrId, Group, Type}]
%% Attrs = [{Key, Value}]
%%
%% MrId  = string()
%% Group = string()
%% Type  = string()
%% Key   = string()
%% Value = integer()
%%
%%========================================================================
create_job_mr(Job, MR) ->
    create_job_mr(Job, MR,  ?DEF_JOB_ATTRS).

create_job_mr(Job, MR, Attrs) ->
    ct:pal("*** PMS <--- SUITE *** create job~n Job: ~p~n MR:  ~p~n"
           " Attrs: ~p~n", [Job, MR, Attrs]),
    Res = pms_netconf:create_job_mr(Job, Attrs, MR),
    to(),
    Res.


%%========================================================================
%% create_mr
%%
%%
%%========================================================================
%% create_mr(Job, Mr, Grp) ->
%%     create_mr_g(host(), Job, Mr, Grp).

%% create_mr_g(?TESTNODE, Job, Mr, Grp) ->
%%     Res = pms_netconf:create_mr(Job, Mr, Grp),
%%     to(),
%%     Res;
%% create_mr_g(_, Job, Mr, Grp) ->
%%     Attrs = [{"measurementSpecification", ?MR_SPEC(Grp)}],
%%     Res = rpc(comte_stub, mr, [Job, Mr, Attrs]),
%%     to(),
%%     Res.


create_mr(Job, Mr, Grp, Type) ->
    ct:pal("$$$$ create_mr~n  ~p ~p ~p ~p ~n", [Job, Mr, Grp, Type]),
    Res = pms_netconf:create_mr(Job, Mr, Grp, Type),
    to(),
    Res.


%%========================================================================
%% delete_mr
%%
%%
%%========================================================================
delete_mr(Job, MR) ->
    ct:pal("*** PMS <--- SUITE *** delete MR~n Job: ~p~n MR:  ~p~n", [Job, MR]),
    {ok, _} = open_trans(),
    Res = pms_netconf:delete_mr(Job, MR),
    close_trans(),
    to(),
    Res.

%%========================================================================
%% create_job
%%
%%
%%========================================================================
%% create_job(Name) ->
%%     ct:pal("*** PMS <--- SUITE *** create Job: ~p~n", [Name]),
%%     Attrs = [{"granularityPeriod", ?TEN_SECONDS},
%%           {"reportingPeriod",   ?TEN_SECONDS},
%%           {"currentJobState",   ?ACTIVE}],
%%     create_job2(Name, Attrs).

%% create_job2(Name, Attrs) ->
%%     ct:pal("*** PMS <--- SUITE *** create Job: ~p~n", [Name]),
%%     ct:pal("CREATE JOB host ~p~n", [host()]),
%%     create_job2(host(), Name, Attrs).

%% create_job2(?TESTNODE, Name, Attrs) ->
%%     Res = pms_netconf:create_job(Name, Attrs),
%%     to(),
%%     Res;
%% create_job2(_, Name, Attrs) ->
%%     Res = rpc(comte_stub, job, [Name, Attrs]),
%%     to(),
%%     Res.


%%========================================================================
%% create_grp_mt
%%
%%
%%========================================================================
create_grp_mt(Name, NGroups, NCounts) ->
    CM = [init_group_counter_map(group_name(Name, GN), GN, NCounts) || 
	     GN <- lists:seq(1, NGroups)],
    create_mts(CM),
    get_mrs(Name, NGroups, NCounts).

init_group_counter_map(GName, GN, NCounts) ->
    {GName, GN, [{mt_name(GName, MTN), MTN} || MTN <- lists:seq(1, NCounts)]}.


group_name(Prefix, N) ->
    atl(Prefix) ++ "PmGroup" ++ itl(N).

mt_name(Prefix, N) ->
    itl(N) ++ atl(Prefix) ++ "MeasType".


create_mts(CM) ->
    Recs = lists:flatmap(fun({Group, _, MTs}) ->
                 [group_rec(Group) | 
                  [mt_rec(Group, MT) || {MT, _} <- MTs]]
			 end, CM),
    write_db(Recs).


    

group_rec(Name) ->
    #pmGroup{pmGroupId = group_key(Name)}.


group_key(Name) ->
    {"1", "1", "1", Name}.


mt_rec(Group, Name) ->
    #measurementType{measurementTypeId = mt_key(Group, Name),
             measurementName = Name}.


mt_key(Group, Name) ->
    {"1", "1", "1", Group, Name}.


write_db(Recs) ->
    FragmentedRecs = fragment(Recs, 1000),
    WriteDb = fun(Records) -> 
		      timer:sleep(10),
      rpc(?MODULE, rpc_write_db, [Records])
    end,

    lists:foreach(WriteDb, FragmentedRecs).


get_mrs(Grp, NoGrps, NoMrs) ->
    get_mrs(Grp,  NoGrps, NoMrs, 1, []).


get_mrs(_Grp,  NoGrps, _NoMrs, GrpNo, Acc) when GrpNo > NoGrps ->
    lists:append(Acc);
get_mrs(Grp,  NoGrps, NoMrs, GrpNo, Acc) ->
    AddN = NoMrs*(GrpNo - 1),
    Mr = [{Grp ++ "_mr_" ++ itl(N + AddN), 
	   atl(Grp) ++ "PmGroup" ++ itl(GrpNo), 
	   itl(N + AddN) ++ atl(Grp) ++ "MeasType"} || 
	     N <- lists:seq(1, NoMrs)],
	  
    get_mrs(Grp,  NoGrps, NoMrs, GrpNo + 1, [lists:reverse(Mr) | Acc]).
				     

%%    create_job_mr(JobId, Attrs, [{Mr, PmGrp, Mt}]);


%%--------------------------------------------------------------------
%% @doc
%% Splits a given list into lists of specified maximum length.
%% For example, if given a list of 25 elements, and a length of 10,
%% it will produce 3 lists: two lists with 10 elements each (20 in
%% total), and a third list with remaining 5 elements.
%% @end
%%--------------------------------------------------------------------
-spec fragment(InList::[term()], FragmentLength::integer()) -> OutList::[[term()]].

fragment(List, FragmentLength) ->
    ReversedFragments = fragment(List, FragmentLength, []),
    lists:reverse(ReversedFragments).


fragment(List, FragmentLength, Acc) when length(List) >= FragmentLength ->
    {Fragment, Rest} = lists:split(FragmentLength, List),
    fragment(Rest, FragmentLength, [Fragment | Acc]);

fragment([], _FragmentLength, Acc) ->
    Acc;

fragment(Remainder, _FragmentLength, Acc) ->
    [Remainder | Acc].


atl(A) when is_atom(A) ->
    atom_to_list(A);

atl(L) when is_list(L) ->
    L.

itl(I) ->
    integer_to_list(I).





%%========================================================================
%% rop_file_handling
%%
%%
%%========================================================================
rop_file_handling(State)
  when State == ?SINGLE_ROP orelse
       State == ?MULTI_ROP ->
    {ok, _} = open_trans(),
    Res = pms_netconf:rop_file_handling(State),
    close_trans(),
    to(),
    Res;
rop_file_handling(State) ->
    {error, {illegal_state, State}}.

%%========================================================================
%% Misc functions
%%
%%
%%========================================================================


%%========================================================================
%%
%%========================================================================
get_table(Tab) ->
    T = rpc(ets, tab2list, [Tab]),
    ct:pal("*** get_table ~p~n~p~n", [Tab, T]),
    T.

%%========================================================================
%% tables
%%========================================================================
tables() ->
    [rpc(ets, tab2list, [pmsAppRegistry]),
     rpc(ets, tab2list, [pmJob]),
     rpc(ets, tab2list, [pmGroup]),
     rpc(ets, tab2list, [measurementType]),
     rpc(ets, tab2list, [measurementReader])
    ].


%% tables(Tabs) ->
%%     [rpc(ets, tab2list, [Tab]) || Tab <- Tabs].


%%========================================================================
%%
%%========================================================================
check_tables(Before) ->
    check_tables(Before, 100).

check_tables(Before, N) when N < 0 ->
    Before = tables();
check_tables(Before, N) ->
    to(),
    After = tables(),
    case ct(Before, After) of
        ok -> ok;
        _  -> check_tables(Before, N - 1)
    end.


ct([], []) ->
    ok;
ct([H|TB], [H|TA]) ->
    ct(TB, TA);
ct([HB|_], [HA|_]) ->
    ct:pal("*** Table diff:~nBefore: ~p~nAfter:  ~p~n", [HB, HA]),
    error.


%%========================================================================
%%
%%========================================================================
wait_table_size(Tab, Size) ->
    wait_table_size(Tab, Size, 50).


wait_table_size(Tab, Size, N) when N > 0 ->
    T = rpc(ets, tab2list, [Tab]),
    Length = length(T),
    if
        Length =:= Size ->
            ct:pal("*** get_table ~p~n~p~n", [Tab, T]),
            ok;
        true ->
            timer:sleep(100),
            wait_table_size(Tab, Size, N - 1)
    end;

wait_table_size(_Tab, _Size, _N) ->
    {error, timeout}.


%%========================================================================
%%
%%========================================================================
get_processes() ->
    get_processes([pmsJob, pmsJobGroup, pmsAppJob]).

get_processes(Mod) when is_atom(Mod) ->
    get_processes([Mod]);
get_processes(Mods) ->
    Procs = rpc(erlang, processes, []),
    IC = [{P, gp(P)} || P <- Procs],
    [{P, C} || {P, {_, {M, _, _} = C}} <- IC, lists:member(M, Mods)].

gp(P) ->
    rpc(erlang, process_info, [P, current_function]).

%%========================================================================
%%
%%========================================================================
check_processes(Number) ->
    check_processes(Number, 20).


check_processes(Number, N) when is_integer(N), N > 0 ->
    case get_processes() of
        Procs when length(Procs) == Number ->
            ok;
        _Procs ->
            timer:sleep(1000),
            check_processes(Number, N - 1)
    end;

check_processes(Number, _N) ->
    case get_processes() of
        Procs when length(Procs) == Number ->
            ok;
        Procs ->
            ct:pal("ERROR unexpected number of processes: ~p (~p)~n"
                   "      these are running: ~n~p~n",
                   [length(Procs), Number, Procs]),
            Number = length(Procs) %% make it crash
    end.

%%========================================================================
%%
%%========================================================================
kill_old_job_procs() ->
    ct:pal("KILLING JOB PROCESSES ~p~n", [get_processes()]),
    kojp(get_processes()).

kojp([]) ->
    ok;
kojp([{Pid, {pmsJobGroup, send, _}} | T]) ->
    timer:sleep(6000), %% TO specified in pmsJobGroup when sending a message
    %% rpc(pmsJobGroup, delete, [Pid]),
    rpc(pmsJobGroup, terminate, [Pid, []]),
    kojp(T);
kojp([{Pid, {pmsJobGroup, _, _}} | T]) ->
    %% rpc(pmsJobGroup, delete, [Pid]),
    rpc(pmsJobGroup, terminate, [Pid, []]),
    kojp(T);
kojp([{Pid, {pmsJob, send, _}} | T]) ->
    timer:sleep(6000), %% TO specified in pmsJob when sending a message
    %% rpc(pmsJob, delete, [Pid, killed_from_init_per_test_case]),
    rpc(pmsJob, terminate, [Pid, killed_from_init_per_test_case]),
    kojp(T);
kojp([{Pid, {pmsJob, _, _}} | T]) ->
    %% rpc(pmsJob, delete, [Pid, killed_from_init_per_test_case]),
    rpc(pmsJob, terminate, [Pid, killed_from_init_per_test_case]),
    kojp(T);
kojp([{Pid, {pmsAppJob, _, _}} | T]) ->
    rpc(pmsAppJob, stop, [Pid]),
    kojp(T).

%%========================================================================
%% delete_old_job_tables
%%========================================================================
delete_old_job_tables() ->
    Tabs = tables(),
    %%ct:pal("DELETING JOB TABLES ~p~n", [Tabs]),
    dojt(Tabs).

dojt([]) ->
    ok;
dojt([H | T]) ->
    dojt_tab(H),
    dojt(T).

dojt_tab([]) ->
    ok;
dojt_tab([H | T]) when element(1, H) == pmJob;
                       element(1, H) == measurementReader ->
    rpc(mnesia, dirty_delete, [element(1, H), element(2, H)]),
    dojt_tab(T);
dojt_tab([_ | T]) ->
    dojt_tab(T).




%%========================================================================
%% trans functions
%%========================================================================
open_trans() ->
    open_trans(host()).

open_trans(?TESTNODE) ->
    ct_netconfc:open(nc1, []);
open_trans(_) ->
    {ok, ok}.


close_trans() ->
    close_trans(host()).

close_trans(?TESTNODE) ->
    ct_netconfc:close_session(nc1);
close_trans(_) ->
    ok.




%%========================================================================
%% rpc(M, F, A) -> Result
%%
%%
%%========================================================================
rpc(M, F, A) ->
    ?PROXY:cs_node_call(M, F, A).

host() -> ?TESTNODE.




hooks() -> hooks(host()).

hooks(?TESTNODE) ->
    [{cth_conn_log,[]},
     {rct_htmllink,[]},
     %% {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
     {rct_logging, {rabbe_logs, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
     {rct_core, []},
     {rct_netconf, nc1},
     {pms_pmi_proxy, []},
     {pms_rop_hook, []},
     {pms_log_hook, []},
     {pms2_erl_app, []}
    ];

hooks(_) ->
    [].




to() ->
    to(1).

to(N) when N > 0 ->
    timer:sleep(?SLEEP),
    to(N-1);
to(_) ->
    ok.


%% get_ct_config(TC) ->
%%     case ct:get_config(TC) of
%%      undefined ->
%%          [];
%%      Config ->
%%          Config
%%     end.

get_ct_config(Key) ->
    get_ct_config(Key, []).


get_ct_config(Key, Default) ->
    case ct:get_config(Key) of
        undefined ->
            Default;
        Val ->
            Val
    end.



%% a2l(A) ->
%%     atom_to_list(A).


start({A, B}) -> {ok, _} = ?PMI2_ERL_APP:start(A, B);
start(A)      -> {ok, _} = ?PMI2_ERL_APP:start(A, []).
stop(A)       -> ok      = ?PMI2_ERL_APP:stop(A).

start_pmi({A, B}) -> {ok, _} = ?PMI_ERL_APP:start(A, B);
start_pmi(A)      -> {ok, _} = ?PMI_ERL_APP:start(A, []).
%%stop_pmi(A)       -> ok      = ?PMI_ERL_APP:stop(A).

%%========================================================================
%% log_te_log() -> any().
%%
%%
%%========================================================================
log_te_log(TELog, Prefix, PrivDir) when is_atom(Prefix) ->
    log_te_log(TELog, atom_to_list(Prefix), PrivDir);

log_te_log(TELog, Prefix, PrivDir) ->
    {ok, RelPath} =  relative_file_path(PrivDir),
    Name = "trace_and_error_log",
    FileName = Prefix ++ "_" ++ Name ++ ".txt",
    File = filename:join(PrivDir, FileName),
    ok = file:write_file(File, TELog),
    ct:log("TE Log: <a href=\"~s\">~s</a>",
           [filename:join(RelPath, FileName), Name]).


%%========================================================================
%% relative_file_path() -> any().
%%
%%========================================================================
relative_file_path(FileDir) ->
    {ok, CWD} = file:get_cwd(),
    SplitPath = filename:split(FileDir),
    SplitCWD = filename:split(CWD),
    relative_file_path(SplitCWD, SplitPath).


relative_file_path([H | T1], [H | T2]) ->
    relative_file_path(T1, T2);

relative_file_path([], [_TC, _Run, RelFilePath]) ->
    {ok, RelFilePath};

relative_file_path(_, _) ->
    {error, abspath}.



%%========================================================================
%% check_rop_file(RopNumber, Expected, NotExpected) -> ok.
%%
%% Check ROP file
%%========================================================================
check_rop_file(No, Expected) ->
    check_rop_file(No, Expected, ["suspect"]).

check_rop_file(No, Expected, NotExpected) ->
    {ok, Ref} = pms_rop_hook:expected([{No, Expected, NotExpected}]),
    wait_expected_result([], [{Ref, ?ROP_HOOK}]).

%%========================================================================
%% check_rop_file_name(RopNumber, Expected, NotExpected) -> ok.
%%
%% Check ROP file name
%%========================================================================
check_rop_file_name(No, Expected) ->
    check_rop_file_name(No, Expected, ["suspect"]).

check_rop_file_name(No, Expected, NotExpected) ->
    {ok, AllFiles} = ?ROP_HOOK:list_rop_files(),
    crfn(length(AllFiles) >= No, AllFiles, No, Expected, NotExpected).

crfn(true, AllFiles, No, Expected, NotExpected) ->
    crfn_check(lists:nth(No, AllFiles), Expected, NotExpected);
crfn(false, AllFiles, No, _, _) ->
    {error, {file_no_not_found, {No, length(AllFiles)}}}.

crfn_check(Name, Expected, NotExpected) ->
    Exp   = [E || E <- Expected,    string:str(Name, E) == 0],
    UnExp = [E || E <- NotExpected, string:str(Name, E) > 0],
    case {Exp, UnExp} of
        {[], []} ->
            ok;
        {Error, []} ->
            {error, {missing_expected, Error}};
        {_, Error}->
            {error, {found_not_expected, Error}}
    end.



%%     {ok, Ref} = pms_rop_hook:check_file_name([{No, Expected, NotExpected}]),
%%     wait_expected_result([], [{Ref, ?ROP_HOOK}]).

%%========================================================================
%% wait_one_gp(GP) -> ok.
%%
%% Wait one GP
%%========================================================================
wait_one_gp(GP) ->
    timer:sleep((GP + 2)*1000).

%%========================================================================
%% get_noof_rop_files() -> integer()
%%
%% Get number of ROP files
%%========================================================================
get_noof_rop_files() ->
    {ok, Rops} = pms_rop_hook:list_rop_files(),
    length(Rops).

%%========================================================================
%% wait_subscribe(ExpectedData) -> ok | {error, Reason}
%%
%% Next CB must be a subscribe message
%%========================================================================
wait_subscribe(ExpectedData) when is_tuple(ExpectedData) ->
    wait_subscribe([ExpectedData]);
wait_subscribe([]) ->
    ok;
wait_subscribe(ExpectedData) ->
    receive
        {copy, _, {pmi2SubscribeRop, Data}} ->
            wait_subscribe(ExpectedData -- [Data]);
        {copy, _, Unexpected} ->
            ct:pal("wait subscribe: Unexpected ~p~n", [Unexpected]),
            {error, Unexpected}
    after 10000 ->
            {error, no_subscribe_received}
    end.

%%========================================================================
%% wait_pmi_subscribe(ExpectedData) -> ok | {error, Reason}
%%
%% Next CB must be a PMI subscribe message
%%========================================================================
wait_pmi_subscribe(ExpectedData) when is_tuple(ExpectedData) ->
    wait_pmi_subscribe([ExpectedData]);
wait_pmi_subscribe([]) ->
    ok;
wait_pmi_subscribe(ExpectedData) ->
    receive
        {copy, _, {pmiSubscribe, Data}} ->
            wait_pmi_subscribe(ExpectedData -- [Data]);
        {copy, _, Unexpected} ->
            ct:pal("wait PMI subscribe: Unexpected ~p~n", [Unexpected]),
            {error, Unexpected}
    after 10000 ->
            {error, no_pmi_subscribe_received}
    end.

%%========================================================================
%% wait_until_subscribe(ExpectedData) -> ok | {error, Reason}
%%
%% Wait until subscribe message is received
%%========================================================================
wait_until_subscribe(ExpectedData) when is_tuple(ExpectedData) ->
    wait_until_subscribe([ExpectedData]);
wait_until_subscribe([]) ->
    ok;
wait_until_subscribe(ExpectedData) ->
    receive
        {copy, _, {pmi2SubscribeRop, Data}} ->
            wait_until_subscribe(ExpectedData -- [Data]);
        {copy, _, {pmi2ReportRop, _}} ->
            wait_until_subscribe(ExpectedData);
        {copy, _, Unexpected} ->
            ct:pal("wait subscribe: Unexpected ~p~n", [Unexpected]),
            {error, Unexpected}
    after 10000 ->
            {error, no_subscribe_received}
    end.

%%========================================================================
%% wait_until_pmi_subscribe(ExpectedData) -> ok | {error, Reason}
%%
%% Wait until PMI subscribe message is received
%%========================================================================
wait_until_pmi_subscribe(ExpectedData) when is_tuple(ExpectedData) ->
    wait_until_pmi_subscribe([ExpectedData]);
wait_until_pmi_subscribe([]) ->
    ok;
wait_until_pmi_subscribe(ExpectedData) ->
    receive
        {copy, _, {pmiSubscribe, Data}} ->
            wait_until_pmi_subscribe(ExpectedData -- [Data]);
        {copy, _, {pmiReport, _}} ->
            wait_until_pmi_subscribe(ExpectedData);
        {copy, _, Unexpected} ->
            ct:pal("wait PMI subscribe: Unexpected ~p~n", [Unexpected]),
            {error, Unexpected}
    after 10000 ->
            {error, no_pmi_subscribe_received}
    end.

%%========================================================================
%% wait_report_rop(ReplyData, FinalFlag, AppData) -> ok | {error, Reason}
%%
%% Next CB must be a rop data request
%%========================================================================
wait_report_rop(Values, FF, {App, Handle, GP}) ->
    wait_report_rop([{Values, FF}], {App, Handle, GP}).

wait_report_rop(VFFs, {App, Handle, GP}) ->
    receive
        {copy, _, {pmi2ReportRop, {_, Id, _}}} ->
            [rop_data(App, Handle, GP, Id, Values, FF) ||
                {Values, FF} <- VFFs],
            ok;
        {copy, _, Unexpected} ->
            ct:pal("wait report rop: Unexpected ~p~n", [Unexpected]),
            {error, Unexpected}
    after (GP + 5)*1000 ->
            {error, no_report_rop_received}
    end.

%%========================================================================
%% wait_pmi_report_rop(ReplyData, FinalFlag, AppData) -> ok | {error, Reason}
%%
%% Next CB must be a pmi rop data request
%%========================================================================
wait_pmi_report_rop(LDNValues, {App, Handle, GP}) ->
    receive
        {copy, _, {pmiReport, {_, Id, _}}} ->
            [rop_data_pmi(App, Handle, GP, Id, LDN, Values) ||
                {LDN, Values} <- LDNValues],
            ok;
        {copy, _, Unexpected} ->
            ct:pal("wait pmi report rop: Unexpected ~p~n", [Unexpected]),
            {error, Unexpected}
    after (GP + 5)*1000 ->
            {error, no_pmi_report_received}
    end.

%%========================================================================
%% get_subscribe_data(GP, MeasReaders) -> {GP, AliasMeasReaders}
%%
%% Aliasify the MeasReaders
%%========================================================================
get_subscribe_data(GP, MRs) ->
    {GP, gsd(MRs, [])}.

gsd([], Acc) ->
    ct:pal("#### ~p get_subscribe_data  ~p~n",
           [?MODULE, lists:usort(lists:reverse(Acc))]),
    lists:usort(lists:reverse(Acc));
gsd([{Grp, MRs} | T], Acc) ->
    gsd(T, [{?GRP_ALIAS(Grp), [?MT_ALIAS(MR) || MR <- MRs]} | Acc]);
gsd([Grp | T], Acc) ->
    gsd(T, [{?GRP_ALIAS(Grp), [?MT_ALIAS(MR) || MR <- ?ALL_MR(Grp)]} | Acc]).




%%========================================================================
%% get_mrs(MeasTypes) -> {MeasReaderId, PmGroup, MeasurementType}
%%
%% Get measurement readers as defined for the pmJob
%%========================================================================
get_mrs(MTs) ->
    get_mrs(MTs, []).

get_mrs([], Acc) ->
    lists:flatten(lists:reverse(Acc));
get_mrs([{Grp, MTs} | T], Acc) ->
    get_mrs(T, [[{MT, Grp, MT} || MT <- MTs] | Acc]);
get_mrs([Grp | T], Acc) ->
    get_mrs(T, [[{MT, Grp, MT} || MT <- ?ALL_MR(Grp)] | Acc]).



%%========================================================================
%% aliasify_values(Values) -> AliasValues
%%
%% Change all Grp and MR identities to aliases
%%========================================================================
aliasify_values(Vals) ->
    alfy(Vals, []).

alfy([], Acc) ->
    lists:reverse(Acc);
alfy([{Grp, LDNs} | T], Acc) ->
    Res = {?GRP_ALIAS(Grp), alfy_ldn(LDNs, [])},
    alfy(T, [Res | Acc]).

alfy_ldn([], Acc) ->
    lists:reverse(Acc);
alfy_ldn([{LDN, Vals} | T], Acc) ->
    Res = {LDN, [{?MT_ALIAS(MR), Vs} || {MR, Vs} <- Vals]},
    alfy_ldn(T, [Res | Acc]).

%%========================================================================
%% aliasify_fles_mts(Values) -> AliasValues
%%
%% Change all Grp and MR identities to aliases
%%========================================================================
aliasify_flex_mts(Vals) ->
    afmt(Vals, []).

afmt([], Acc) ->
    lists:reverse(Acc);
afmt([{Grp, MTs} | T], Acc) ->
    Res = {Grp, ?GRP_ALIAS(Grp), afmt_mt(MTs, [])},
    afmt(T, [Res | Acc]).

afmt_mt([], Acc) ->
    lists:reverse(Acc);
afmt_mt([MT | T], Acc) ->
    afmt_mt(T, [{MT, ?MT_ALIAS(MT)} | Acc]).


%%========================================================================
%% cancel_timer(Ref) -> ok
%%
%% Cancel timer and flush buffer
%%========================================================================
cancel_timer(undefined) ->
    ok;

cancel_timer(Ref) ->
    erlang:cancel_timer(Ref),
    receive
        {timeout, Ref, _} ->
            ok
    after 0 ->
            ok
    end.
