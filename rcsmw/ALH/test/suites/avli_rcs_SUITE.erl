%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	avli_rcs_SUITE.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R5A/R7A/R10A/1
%%%
%%% @doc ==Test Suite for RCS logged events through the AVLI interface on SUT.==
%%% This Test Suite can be used on target enviroment.
%%% Before running the suite the node should be clean, i.e. just installed.
%%% Also, after the test, the node is in network loader, so it must be 
%%% reinstalled.
%%% <br/><br/>
%%% rct_proxy is used in ct_hooks:
%%% see [https://rbs-rde.rnd.ki.sw.ericsson.se/vobs/rcs/dev/RCP_CSX10179/RCT_CRX901275/edoc/rct_proxy.html]
%%% <br/>
%%%
%%%
%%% @end

-module(avli_rcs_SUITE).

%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% R5A/1      2016-02-18 etomist     Created
%%% R5A/2      2016-03-01 etomist     Test suite description updated
%%% R5A/3      2016-05-24 etxpejn     Prolonged timer for cold with test
%%% R7A/1      2016-09-12 etxpejn     OTP19 - changed more to cat
%%% R7A/2      2016-10-13 etxpejn     Removed check for ProdName since this
%%%                                   has changed for new CXPs  
%%% R10A/1     2017-06-22 etxpejn     Pro-longed timer for restart
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

-export([check_rcs_avli_log/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-spec suite() -> [tuple()].

suite() ->
   [{timetrap, {minutes, 30}},
    {ct_hooks, [{rct_rpc, rpc},
                {rct_consserv,cs1},
                {rct_rs232,console},
                {rct_power,node},    
                {rct_htmllink,[]},
                {rct_logging, {all, [{erlang, {["ERROR REPORT","CRASH REPORT"],
                                               ["\"test_app\" died with Exit code 137",
                                                "\"test_oi\" died with Exit code 137",
                                                "\"ift_app\" died with Exit code 137",
                                                "Program ID [0-9]+ has crashed",
                                                "Program ID [0-9]+ has terminated",
                                                "has failed to start after 3 attempts within 300 seconds"]}
                                    }]}
                },               
                {rct_netconf, nc1},
                {rct_cli, {cli, [manual_connect]}},
                {rct_coli, {coli, [manual_connect]}},
                {rct_core,[]}
           ]}].

%% @hidden
init_per_suite(Config) ->
    Config.

%% @hidden
end_per_suite(_Config) ->
    ok.

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
     {default__group, [], AllGroup}
    ].

all() ->
    [
        check_rcs_avli_log
    ].

check_rcs_avli_log(_Config) ->
    Apps = get_apps(),
    FoundApps = [A || {A,_} <-Apps],

    rct_rpc:call(rpc, alhI, reset_log, [], 10000),
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    RestartReason = "rcs_avli_test",
    rct_rpc:call(rpc, appmI, restart_node, [cold, RestartReason], 10000),
    timer:sleep(10000),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    wait_for_appl_started(FoundApps, ErlNode, 180000),
    {ok, Log} = rct_rpc:call(rpc, alhI, get_log, [], 10000),
    DevPatchesDir = rct_rpc:call(rpc, sysEnv, dev_patches_dir, [], 10000),
    AlhPrivDir = rct_rpc:call(rpc, code, priv_dir, [alh], 10000) ++ "/dtd",
    ScanOpts = [{fetch_path, [DevPatchesDir, AlhPrivDir]},
                {validation, dtd}], 
    {XmlTree, _} = rct_rpc:call(rpc, xmerl_scan, string, [Log, ScanOpts], 10000),

    %% Check OutOfService - ShutdownCommand NodeEvent; cold restart, with cause rcs_avli_test
    ct:pal("Checking NodeEvent - OutOfService, ShutdownCommand, cold restart, cause rcs_avli_test~n"),
    Shutdown_NodeEvent = construct_node_event(4),
    Shutdown_OutOfService = construct_out_of_service(4),
    ShutdownCommand = "ShutdownCommand",
    Shutdown_RankCold = construct_rank_cold(4),
    [Shutdown_NodeEvent] = xmerl_xpath:string("//LogRecord[@number='1']//RecordContent/NodeEvent", 
					      XmlTree),
    [Shutdown_OutOfService] = xmerl_xpath:string("//LogRecord[@number='1']//RecordContent/OutOfService", 
						 XmlTree),
    [L1 | _] = xmerl_xpath:string("//LogRecord[@number='1']//RecordContent/EventReason/text()", 
				  XmlTree),
    ShutdownCommand = string:strip(L1#xmlText.value, both),
    [Shutdown_RankCold] = xmerl_xpath:string("//LogRecord[@number='1']//RecordContent/AdditionalInfo/Rcs/RankCold", XmlTree),
    [L2 | _] = xmerl_xpath:string("//LogRecord[@number='1']//RecordContent/AdditionalInfo/Rcs/Cause/text()", XmlTree),
    RestartReason = string:strip(L2#xmlText.value, both),

    %% Check OutOfService - UnOperational Node Event; downtime is not unknown
    ct:pal("Checking NodeEvent - OutOfService, UnOperational~n"),
    UnOperational_NodeEvent = construct_node_event(8),
    UnOperational_OutOfService = construct_out_of_service(8),
    UnOperational = "UnOperational",
    UnOperational_RankCold = construct_rank_cold(8),
    [UnOperational_NodeEvent] = xmerl_xpath:string("//LogRecord[@number='3']//RecordContent/NodeEvent", XmlTree),
    [UnOperational_OutOfService] = xmerl_xpath:string("//LogRecord[@number='3']//RecordContent/OutOfService", XmlTree),
    [L3 | _] = xmerl_xpath:string("//LogRecord[@number='3']//RecordContent/EventReason/text()", 
				  XmlTree),
    UnOperational = string:strip(L3#xmlText.value, both),
    [UnOperational_RankCold] = xmerl_xpath:string("//LogRecord[@number='3']//RecordContent/AdditionalInfo/Rcs/RankCold", XmlTree),

    %% Check InService - Starting Node Event
    ct:pal("Checking NodeEvent - InService, Starting~n"),
    Starting_NodeEvent = construct_node_event(10),
    Starting_InService = construct_in_service(10),
    Starting = "Starting",
    [Starting_NodeEvent] = xmerl_xpath:string("//LogRecord[@number='4']//RecordContent/NodeEvent", XmlTree),
    [Starting_InService] = xmerl_xpath:string("//LogRecord[@number='4']//RecordContent/InService", XmlTree),
    [L5 | _] = xmerl_xpath:string("//LogRecord[@number='4']//RecordContent/EventReason/text()", XmlTree),
    Starting = string:strip(L5#xmlText.value, both),
    [L6 | _] = xmerl_xpath:string("//LogRecord[@number='4']//RecordContent/AdditionalInfo/Rcs", XmlTree),
    'Rcs' = L6#xmlElement.name,

    %% Check NodeRestarted - Other Event
    ct:pal("Checking OtherEvent - NodeRestarted~n"),
    NodeRestarted_OtherEvent = construct_other_event(12),
    NodeRestarted = "NodeRestarted",
    NodeRestarted_ProdNoStart = "CXS101549",
    %% NodeRestarted_ProdNameStart = "RCP", %% Could be changed to RCS
    [NodeRestarted_OtherEvent] = xmerl_xpath:string("//LogRecord[@number='5']//OtherEvent", XmlTree),
    [L7 | _] = xmerl_xpath:string("//LogRecord[@number='5']//AvailabilityInfo/RcsNodeIdentityInfo/NodeIdReason/text()", XmlTree),
    NodeRestarted = string:strip(L7#xmlText.value, both),
    [L8 | _] = xmerl_xpath:string("//LogRecord[@number='5']//AvailabilityInfo/RcsNodeIdentityInfo/NodeIdentity/ProdNo/text()", XmlTree),
    NodeRestarted_ProdNoStart = string:substr(string:strip(L8#xmlText.value, both), 1, 9),
    %% [L9 | _] = xmerl_xpath:string("//LogRecord[@number='5']//AvailabilityInfo/RcsNodeIdentityInfo/NodeIdentity/ProdName/text()", XmlTree),
    %% NodeRestarted_ProdNameStart = string:substr(string:strip(L9#xmlText.value, both), 1, 3),

    %% Check InService - Operational Node Event
    ct:pal("Checking NodeEvent - InService, Operational~n"),
    Operational_NodeEvent = construct_node_event(14),
    Operational_InService = construct_in_service(14),
    Operational = "Operational",
    [Operational_NodeEvent] = xmerl_xpath:string("//LogRecord[@number='6']//RecordContent/NodeEvent", XmlTree),
    [Operational_InService] = xmerl_xpath:string("//LogRecord[@number='6']//RecordContent/InService", XmlTree),
    [L10 | _] = xmerl_xpath:string("//LogRecord[@number='6']//RecordContent/EventReason/text()", XmlTree),
    Operational = string:strip(L10#xmlText.value, both),
    [L11 | _] = xmerl_xpath:string("//LogRecord[@number='6']//RecordContent/AdditionalInfo/Rcs", XmlTree),
    'Rcs' = L11#xmlElement.name,

    %% Prepare for first batch of program related AVLI entries
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),
    rct_rpc:call(rpc, alhI, reset_log, [], 10000),
    Pid = kill_app("ift_app"),
    timer:sleep(5000),
    wait_for_app_restarted("ift_app", Pid),
    {ok, Log2} = rct_rpc:call(rpc, alhI, get_log, [], 10000),
    {XmlTree2, _} = rct_rpc:call(rpc, xmerl_scan, string, [Log2, ScanOpts], 30000),

    %% Check OutOfService - UnOperational Program Event, kill ift_app
    ct:pal("Checking ProgramEvent - OutOfService, UnOperational~n"),
    UnOperational_ProgramEvent = construct_program_event(4),
    UnOperational_ProgramOutOfService = construct_out_of_service(4),
    UnOperational_ProdNoStart = "CXC1735023",
    Unoperational_Cause = "RestartRequest",
    [UnOperational_ProgramEvent] = xmerl_xpath:string("//LogRecord[@number='1']//RecordContent/ProgramEvent", XmlTree2),
    [UnOperational_ProgramOutOfService] = xmerl_xpath:string("//LogRecord[@number='1']//RecordContent/OutOfService", XmlTree2),
    [L12 | _] = xmerl_xpath:string("//LogRecord[@number='1']//RecordContent/EventReason/text()", XmlTree2),
    UnOperational = string:strip(L12#xmlText.value, both),
    [L13 | _] = xmerl_xpath:string("//LogRecord[@number='1']//SwPid/ProdNo/text()", XmlTree2),
    UnOperational_ProdNoStart = string:substr(string:strip(L13#xmlText.value, both), 1, 10),
    [L14 | _] = xmerl_xpath:string("//LogRecord[@number='1']//AdditionalInfo/Rcs/Cause/text()", XmlTree2),
    Unoperational_Cause = string:strip(L14#xmlText.value, both),

    %% Check InService - Starting Program Event
    ct:pal("Checking ProgramEvent - InService, Starting~n"),
    Starting_ProgramEvent = construct_program_event(6),
    InService_ProgramEvent = construct_in_service(6),
    Starting_ProdNoStart = "CXC1735023",
    [Starting_ProgramEvent] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/ProgramEvent", XmlTree2),
    [InService_ProgramEvent] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/InService", XmlTree2),
    [L15 | _] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/EventReason/text()", XmlTree2),
    Starting = string:strip(L15#xmlText.value, both),
    [L16 | _] = xmerl_xpath:string("//LogRecord[@number='2']//SwPid/ProdNo/text()", XmlTree2),
    Starting_ProdNoStart = string:substr(string:strip(L16#xmlText.value, both), 1, 10),
    [L17 | _] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/AdditionalInfo/Rcs", XmlTree2),
    'Rcs' = L17#xmlElement.name,

    %% Clear escalation list
    rct_rpc:call(rpc, appmServer, reset_restart_list, [], 10000),

    %% Check OutOfService - Node Event, warm restart, ProgramErrEscalated cause
    ct:pal("Checking NodeEvent - OutOfService, warm restart, ProgramErrEscalated cause~n"),
    XmlTree3 = prepare_escalation_test(FoundApps, ScanOpts, warm, 10000),
    UnOperational_Escalated_NodeEvent = construct_node_event(6),
    UnOperational_Escalated_OutOfService = construct_out_of_service(6),
    UnOperational_Escalated_RankWarm = construct_rank_warm(6),
    EscalatedRestartReason = "ProgramErrEscalated",
    [UnOperational_Escalated_NodeEvent] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/NodeEvent", XmlTree3),
    [UnOperational_Escalated_OutOfService] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/OutOfService", XmlTree3),
    [L18 | _] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/EventReason/text()", XmlTree3),
    UnOperational = string:strip(L18#xmlText.value, both),
    [UnOperational_Escalated_RankWarm] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/AdditionalInfo/Rcs/RankWarm", XmlTree3),
    [L19 | _] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/AdditionalInfo/Rcs/Cause/text()", XmlTree3),
    EscalatedRestartReason = string:strip(L19#xmlText.value, both),

    %% Check OutOfService - Node Event, cold restart, ProgramErrEscalated cause
    ct:pal("Checking NodeEvent - OutOfService, cold restart, ProgramErrEscalated cause~n"),
    XmlTree4 = prepare_escalation_test(FoundApps, ScanOpts, cold, 10000),
    UnOperational_Escalated_RankCold = construct_rank_cold(6),
    [UnOperational_Escalated_NodeEvent] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/NodeEvent", XmlTree4),
    [UnOperational_Escalated_OutOfService] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/OutOfService", XmlTree4),
    [L20 | _] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/EventReason/text()", XmlTree4),
    UnOperational = string:strip(L20#xmlText.value, both),
    [UnOperational_Escalated_RankCold] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/AdditionalInfo/Rcs/RankCold", XmlTree4),
    [L21 | _] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/AdditionalInfo/Rcs/Cause/text()", XmlTree4),
    EscalatedRestartReason = string:strip(L21#xmlText.value, both),

    %% Check OutOfService - Node Event, cold with test restart, ProgramErrEscalated cause
    ct:pal("Checking NodeEvent - OutOfService, cold with test restart, ProgramErrEscalated cause~n"),
    XmlTree5 = prepare_escalation_test(FoundApps, ScanOpts, cold_w_test, 10000),
    UnOperational_Escalated_RankColdWTest = construct_rank_cold_w_test(6),
    [UnOperational_Escalated_NodeEvent] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/NodeEvent", XmlTree5),
    [UnOperational_Escalated_OutOfService] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/OutOfService", XmlTree5),
    [L22 | _] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/EventReason/text()", XmlTree5),
    UnOperational = string:strip(L22#xmlText.value, both),
    [UnOperational_Escalated_RankColdWTest] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/AdditionalInfo/Rcs/RankColdWTest", XmlTree5),
    [L23 | _] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/AdditionalInfo/Rcs/Cause/text()", XmlTree5),
    EscalatedRestartReason = string:strip(L23#xmlText.value, both),

    %% Check OutOfService - Node Event, cold with test restart, ProgramErrEscalatedRestore1 cause
    ct:pal("Checking NodeEvent - OutOfService, ProgramErrEscalatedRestore1 cause~n"),
    ok = rct_rpc:call(rpc, appmServer, set_rollback_time, [180], 10000),
    XmlTree6 = prepare_escalation_test(lists:delete("restart", FoundApps), ScanOpts, revert, 10000),
    ProgramErrEscalatedRestore1 = "ProgramErrEscalatedRestore1",
    [UnOperational_Escalated_NodeEvent] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/NodeEvent", XmlTree6),
    [UnOperational_Escalated_OutOfService] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/OutOfService", XmlTree6),
    [L24 | _] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/EventReason/text()", XmlTree6),
    UnOperational = string:strip(L24#xmlText.value, both),
    [L25 | _] = xmerl_xpath:string("//LogRecord[@number='2']//RecordContent/AdditionalInfo/Rcs/Cause/text()", XmlTree6),
    ProgramErrEscalatedRestore1 = string:strip(L25#xmlText.value, both),

    ok.


construct_node_event(LogRecordNr) ->
    #xmlElement{name = 'NodeEvent',expanded_name = 'NodeEvent',
                nsinfo = [],
                namespace = #xmlNamespace{default = [],nodes = []},
                parents = [{'RecordContent',4},{'LogRecord', LogRecordNr},{'Log',1}],
                pos = 2,attributes = [],content = [],language = [],
                xmlbase = undefined,elementdef = undeclared}.

construct_other_event(LogRecordNr) ->
    #xmlElement{name = 'OtherEvent',expanded_name = 'OtherEvent',
                nsinfo = [],
                namespace = #xmlNamespace{default = [],nodes = []},
                parents = [{'RecordContent',4},{'LogRecord', LogRecordNr},{'Log',1}],
                pos = 2,attributes = [],content = [],language = [],
                xmlbase = undefined,elementdef = undeclared}.

construct_program_event(LogRecordNr) ->
    #xmlElement{name = 'ProgramEvent',expanded_name = 'ProgramEvent',
                nsinfo = [],
                namespace = #xmlNamespace{default = [],nodes = []},
                parents = [{'RecordContent',4},{'LogRecord', LogRecordNr},{'Log',1}],
                pos = 2,attributes = [],content = [],language = [],
                xmlbase = undefined,elementdef = undeclared}.

construct_out_of_service(LogRecordNr) ->
    #xmlElement{name = 'OutOfService',expanded_name = 'OutOfService',
                nsinfo = [],
                namespace = #xmlNamespace{default = [],nodes = []},
                parents = [{'RecordContent',4},{'LogRecord', LogRecordNr},{'Log',1}],
                pos = 4,attributes = [],content = [],language = [],
                xmlbase = undefined,elementdef = undeclared}.

construct_in_service(LogRecordNr) ->
    #xmlElement{name = 'InService',expanded_name = 'InService',
                nsinfo = [],
                namespace = #xmlNamespace{default = [],nodes = []},
                parents = [{'RecordContent',4},{'LogRecord', LogRecordNr},{'Log',1}],
                pos = 4,attributes = [],content = [],language = [],
                xmlbase = undefined,elementdef = undeclared}.

construct_rank_cold(LogRecordNr) ->
    #xmlElement{name = 'RankCold',expanded_name = 'RankCold',
                nsinfo = [],
                namespace = #xmlNamespace{default = [],nodes = []},
                parents = [{'Rcs', 2}, {'AdditionalInfo', 8}, {'RecordContent',4},{'LogRecord', LogRecordNr},{'Log',1}],
                pos = 2,attributes = [],content = [],language = [],
                xmlbase = undefined,elementdef = undeclared}.

construct_rank_warm(LogRecordNr) ->
    #xmlElement{name = 'RankWarm',expanded_name = 'RankWarm',
                nsinfo = [],
                namespace = #xmlNamespace{default = [],nodes = []},
                parents = [{'Rcs', 2}, {'AdditionalInfo', 8}, {'RecordContent',4},{'LogRecord', LogRecordNr},{'Log',1}],
                pos = 2,attributes = [],content = [],language = [],
                xmlbase = undefined,elementdef = undeclared}.

construct_rank_cold_w_test(LogRecordNr) ->
    #xmlElement{name = 'RankColdWTest',expanded_name = 'RankColdWTest',
                nsinfo = [],
                namespace = #xmlNamespace{default = [],nodes = []},
                parents = [{'Rcs', 2}, {'AdditionalInfo', 8}, {'RecordContent',4},{'LogRecord', LogRecordNr},{'Log',1}],
                pos = 2,attributes = [],content = [],language = [],
                xmlbase = undefined,elementdef = undeclared}.

prepare_escalation_test(FoundApps, ScanOpts, RestartRank, SleepTimer) ->
    rct_rpc:call(rpc, alhI, reset_log, [], 10000),
    kill_app("restart"),
    timer:sleep(SleepTimer),
    case RestartRank of
        revert ->
            wait_for_cold_with_test_restart(360000);
        cold_w_test ->
            wait_for_cold_with_test_restart(360000);
        cold ->
            ok;
        warm ->
            ok
    end,
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    wait_for_appl_started(FoundApps, ErlNode, 360000),
    {ok, Log} = rct_rpc:call(rpc, alhI, get_log, [], 10000),
    {XmlTree, _} = rct_rpc:call(rpc, xmerl_scan, string, [Log, ScanOpts], 30000),
    XmlTree.


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

wait_for_appl_started(_, _ErlNode,Timeout) when Timeout < 500 ->
    ct:fail("No Appl started within max timeout.");

wait_for_appl_started(Apps, ErlNode,Timeout) ->
    case rct_rpc:call(rpc, appmServer, get_apps, [], 1000) of
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
            ct:pal("Not all expecting apps exist, wait and get apps again.",[]),
            timer:sleep(5000),
            wait_for_appl_started(Apps, ErlNode,Timeout - 5000)
        end

    end.

kill_app(AppName) ->
    Apps = rct_rpc:call(rpc, appmServer, get_apps, [], 1000),
    {AppName, Pid} = lists:keyfind(AppName, 1, Apps),
    rct_rpc:call(rpc, os, cmd, ["kill -9 " ++ Pid], 10000),
    Pid.

wait_for_app_restarted(AppName, AppPid) ->
    wait_for_app_restarted(AppName, AppPid, 90000).
wait_for_app_restarted(_AppName, _AppPid, Timeout) when Timeout < 500 ->
    ct:fail("App has not restarted within expected time!!");
wait_for_app_restarted(AppName, AppPid, Timeout) -> 
    case rct_rpc:call(rpc, appmServer, get_apps, [], 1000) of
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

wait_for_cold_with_test_restart(Timeout) when Timeout < 0 ->
    ct:fail("No cold with test restart within max timeout.");

wait_for_cold_with_test_restart(Timeout) ->
    case rct_rpc:call(rpc, os, cmd, ["cat /var/log/syslog | grep hwtest:1 | wc -l"], 10000) of
    {badrpc,_} -> %{badrpc, timeout | nodedown}
        timer:sleep(1000),
        wait_for_cold_with_test_restart(Timeout - 10000);
    NewRes ->
        {NewNr, _} = string:to_integer(NewRes),
        NewNr
    end.
