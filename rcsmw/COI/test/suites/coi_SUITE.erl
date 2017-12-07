%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	coi_SUITE.erl %
%%% @author eralils
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R5A/R6A/R8A/R11A/2

%%% @doc ==Tests of COI functionality==
%%%
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-module(coi_SUITE).
-id('Updated by CCase').
-vsn('/main/R3A/R4A/R5A/R6A/R8A/R11A/2').
-date('2017-10-17').
-author('eralils').

%%% ###=====================================================================###
%%% # 0.3   LEGAL RIGHTS
%%% ###---------------------------------------------------------------------###
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

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 1.    REVISION LOG
%%% ###---------------------------------------------------------------------###
%% Rev     Date       Name     What
%% ------- ---------- -------  ------------------------------------------------
%% R3A/1   2015-02-04 etxberb  Created.
%% R3A/2   2015-02-10 etxberb  Changed from warning to notice in SecurityLog.
%% R3A/3   2015-02-13 etxberb  Adjustments to changed alarm functionality.
%% R3A/4   2015-02-18 etxberb  Changed log_check to check in all possible files.
%% R3A/5   2015-04-01 etxberb  Added mim_sqr.
%% R3A/6   2015-07-10 etxjovp  Add group definitions used by CS CI
%% R3A/7   2015-07-15 etxjovp  modify group definitions used by CS CI
%% R4A/1   2015-09-03 etxberb  Changed post_init to post_init_from_gmf.
%% R4A/2   2015-10-22 etxberb  Added mib_mimVal2mibStr/1 & mib_sqr/1.
%% R5A/1   2016-01-08 etxberb  Changed post_init_from_gmf to post_init.
%% R5A/2   2016-01-08 etxpeno  Changed log_security/1
%% R6A/1   2016-04-21 erarafo  Extended tests of coi:getMoAttributes/3.
%% R8A/1   2016-12-06 uabesvi  New all/0 for cloud 
%% R11A/1  2017-10-03 eralils  Added get_https_ports, lookup_maintenance_user 
%%                             and lookup_pw_user.
%% R11A/2  2017-10-16 eralils  New group for target, ci_test_target.
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 2.    MODULE BORDER LINE AND INTERNAL DEFINITIONS
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 2.1.1 Common Test
%%% ###---------------------------------------------------------------------###
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 groups/0,
	 all/0]).

%%% ###---------------------------------------------------------------------###
%%% # 2.1.2 Test Cases
%%% ###---------------------------------------------------------------------###
-export([alarm_raise_and_cease/1,
	 log_auditTrail/1,
	 log_availability/1,
	 log_security/1,
	 mib_mimVal2mibStr/1,
	 mib_sqr/1,
	 mim_class/1,
	 mim_classPath/1,
	 mim_files/1,
	 mim_sqr/1,
	 mim_tree/1,
	 misc_keyfind/1,
	 misc_keyfind_all/1,
	 misc_pairElems/1,
	 mo_req_action/1,
	 mo_req_countMoChildren/1,
	 mo_req_createMo/1,
	 mo_req_deleteMo/1,
	 mo_req_existsMo/1,
	 mo_req_getMoAttributes/1,
	 mo_req_getMoAttributes_extended/1,
	 mo_req_getMoIterator/1,
	 mo_req_setMoAttributes/1,
	 mo_req_bundle_getMoTree/1,
	 mo_req_AllTogether/1,
	 notif_subscribe/1,
	 trans_seq_commit/1,
	 trans_seq_abort/1,
     get_https_ports/1,
     lookup_maintenance_user/1,
     lookup_pw_user/1]).

%%% ###---------------------------------------------------------------------###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###
-include_lib("common_test/include/ct.hrl").

%%% ###=====================================================================###
%%% # 2.4   LOCAL DEFINITION OF MACROS
%%% ###---------------------------------------------------------------------###
%% identifiers used by RCT hooks
-define(RCS, rct_rcsTestServer:ct_hook(rct_rpc)).

%% General
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

-define(MODULE_STR, atom_to_list(?MODULE)).

%% Miscellaneous
-define(Alrm_Name, 'LogHasReachedFullCapacity').
-define(Alrm_Dn, [<<"ManagedElement=1">>,
		  <<"SystemFunctions=1">>,
		  <<"LogM=1">>]).
-define(Alrm_AdditionalText, ?MODULE_STR).
-define(Log_Msg(Text), ?MODULE_STR ++ ", " ++ Text).
-define(StrVal(Str), list_to_binary(Str)).
-define(RPC_read_file(File),
	rct_rpc:call(?RCS, file, read_file, [File], 10000)).
-define(RPC_term_to_string(Term),
	rct_rpc:call(?RCS, sysUtil, term_to_string, [Term], 10000)).

%% MO:s
-define(ACTION_exportAVL, <<"exportAvailabilityLog">>).
-define(ATTR_int32, <<"int32">>).
-define(ATTRval_int32(Val), {3, Val}).
-define(ATTR_struct1, <<"struct1">>).
-define(ATTRval_struct1(Mem1, Mem2), {14, [{<<"struct1mem1">>, {3, Mem1}},
					   {<<"struct1mem2">>, {9, Mem2}}]}).
-define(ATTR_additionalText, <<"additionalText">>).
-define(ATTR_pwd, <<"password">>).
-define(ATTRval_pwd, {9, <<"pwd">>}).
-define(ATTR_uri, <<"uri">>).
-define(ATTRval_uri, {9, <<"http://www.testweb.se">>}).
-define(DN_Fm, <<"ManagedElement=1,SystemFunctions=1,Fm=1">>).
-define(DN_LogM, <<"ManagedElement=1,SystemFunctions=1,LogM=1">>).
-define(DN_TestClass1(Val),
	list_to_binary("ManagedElement=1,TestRoot=1,TestClass1=" ++ Val)).
-define(DN_TestRoot, <<"ManagedElement=1,TestRoot=1">>).
-define(CLASS_FmAlarm, <<"FmAlarm">>).
-define(CLASS_TestClass1, <<"TestClass1">>).
-define(CLASSid_TestClass1, <<"testClass1Id">>).
-define(CLASSval_TestClass1, <<"77">>).

-define(CLI, cli1).

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 3.1.1 Common Test Callback Functions
%%% ###---------------------------------------------------------------------###
%%% For descriptions, see the Common Test manual.
%%% ###---------------------------------------------------------------------###

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @hidden
%%% ----------------------------------------------------------
%%% ###=====================================================================###
suite() ->
    [{timetrap, {minutes, 30}},
     {ct_hooks, [{rct_htmllink, []},
		 {cth_conn_log, []},
		 {rct_logging, {all,
				[{erlang,
				  {["ERROR REPORT", "CRASH REPORT"], []}}]}},
		 {rct_rpc, ?RCS},
		 {rct_core, []},
		 {rct_cli, {?CLI, [manual_connect]}}
		]}
    ].

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Per-suite initialization.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
init_per_suite(Config) ->
    NewConfig = rct_rcsTestServer:start(Config),
    ct:pal("### init_per_suite, Config: ~n~p", [NewConfig]),
    NewConfig.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Per-suite clean up.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
end_per_suite(Config) ->
    ct:pal("### end_per_suite, Config: ~n~p", [Config]),
    TId = trans_join_new(Config),
    MFA_deleteMo =
	{coi, deleteMo, [TId,
			 ?DN_TestClass1("77")]},
    rct_rcsTestServer:run(MFA_deleteMo, Config),
    MFA_prepare =
	{coi, prepare, [TId]},
    case rct_rcsTestServer:run(MFA_prepare, Config) of
	ok ->
	    trans_commit(TId, Config);
	_ ->
	    ok
    end,
    trans_finish(TId, Config),
    rct_rcsTestServer:stop(Config),
    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @hidden
%%% ----------------------------------------------------------
%%% ###=====================================================================###
init_per_group(GroupName, Config) ->
    ct:pal("##################~n### Test group ###  ~p~n##################",
	   [GroupName]),
    Config.

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @hidden
%%% ----------------------------------------------------------
%%% ###=====================================================================###
end_per_group(_GroupName, _Config) ->
    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @hidden
%%% ----------------------------------------------------------
%%% ###=====================================================================###
init_per_testcase(TestCase, Config) ->
    ct:pal("#################~n### Test case ###  ~p~n#################",
	   [TestCase]),
    Config.

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @hidden
%%% ----------------------------------------------------------
%%% ###=====================================================================###
end_per_testcase(TestCase, _Config) ->
    if
	TestCase == alarm_raise_and_cease ->
	    alarm_rpc(cease);
	TestCase == notif_subscribe ->
	    alarm_rpc(cease);
	TestCase == mo_req_getMoAttributes_extended ->
	    cliExec("top", []),
	    cliExec("ManagedElement=1,TestRoot=1", []),
	    cliExec("configure", []),
	    cliExec("no TestClassE=1", []), 
	    cliExec("commit", []),
	    cliExec("top", []),
	    cliExec("ManagedElement=1,TestRoot=1", []),
	    cliExec("configure", []),
	    cliExec("no TestClassA=991", []), 
	    cliExec("commit", []),
	    rct_cli:disconnect(?CLI, print);
	TestCase == lookup_maintenance_user ->
        rct_rpc:call(?RCS, coi, deleteFakeMaintenanceUser, [], 10000);
	TestCase == lookup_pw_user ->
        rct_rpc:call(?RCS, coi, deleteFakeUser, [], 10000);
	true ->
	    ok
    end,
    T = 10000,
    rct_rpc:call(?RCS, sysTestServer, erase_callback_results, [], T),
    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @hidden
%%% ----------------------------------------------------------
%%% ###=====================================================================###
groups() ->
     %%AllGroup = all(),
    [
%%%  START Group definitions used by CS CI
     {default__group, [], [{group, ci_test_sim_1}]},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},
     {sdc__cover__sim__1__group, [], [{group, all}]},
     {sdc__def__all__1__group, [], [{group, all}]},
     {sdc__qual__all__1__group, [], []},
%%%  END Group definitions used by CS CI
     %% Automation:
     {delivery_test_sim, [], [{group, all}]},
%%%     {delivery_test_tgt, [], [{group, delivery_test_sim}]},
     {ci_test_sim, [], [{group, ci_test_sim_1}]},
     {ci_test_sim_1,       [], [mo_req_action,
			       mo_req_AllTogether,
			      {group, mib},
			      {group, mim},
			      {group, alarm},
			      {group, notif},
			      {group, sec},
			      {group, log}]},
     %% All:
     {all,               [], [{group, trans_seq},
			      {group, mo_req},
			      {group, mo_req_bundle},
			      {group, mib},
			      {group, mim},
			      {group, alarm},
			      {group, notif},
			      {group, log},
			      {group, sec},
			      {group, misc}]},
     %% Target:
     {ci_test_target,       [], [mo_req_action,
			       mo_req_AllTogether,
			      {group, mib},
			      {group, mim},
			      {group, alarm},
			      {group, notif},
			      {group, log}]},
     %% Cloud:
     {cloud,             [], [{group, trans_seq},
			      {group, mo_req},
			      {group, mo_req_bundle},
			      {group, mib},
			      {group, mim},
			      {group, alarm},
			      {group, notif},
			      %%{group, log},
			      {group, misc}]},
     %% Sub-suites:
     {trans_seq,         [], [trans_seq_commit,
			      trans_seq_abort]},
     {mo_req,            [], [mo_req_action,
			      mo_req_countMoChildren,
			      mo_req_createMo,
			      mo_req_deleteMo,
			      mo_req_existsMo,
			      mo_req_getMoAttributes,
			      mo_req_getMoAttributes_extended,
			      mo_req_getMoIterator,
			      mo_req_setMoAttributes,
			      mo_req_AllTogether]},
     {mo_req_bundle,     [], [mo_req_bundle_getMoTree]},
     {mib,               [], [mib_mimVal2mibStr,
			      mib_sqr]},
     {mim,               [], [mim_files,
			      mim_class,
			      mim_classPath,
			      mim_sqr,
			      mim_tree]},
     {alarm,             [], [alarm_raise_and_cease]},
     {notif,             [], [notif_subscribe]},
     {log,               [], [log_auditTrail,
			      log_availability,
			      log_security]},
     {sec,               [], [get_https_ports,
                  lookup_maintenance_user,
                  lookup_pw_user]},
     {misc,              [], [misc_keyfind,
			      misc_keyfind_all,
			      misc_pairElems]}].

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @hidden
%%% ----------------------------------------------------------
%%% ###=====================================================================###
all() ->

    case os:getenv("SIM_OR_TARGET") of
	"cloudish" ->
	    [{group, cloud}];
	"target" ->
	    [{group, ci_test_target}];
	_Other ->
	    [{group, all}]
    end.


%%% ###---------------------------------------------------------------------###
%%% # 3.1.2 Test Cases
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Trigger an alarm and check the FmAlarm table.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
alarm_raise_and_cease(Config) ->
    TId = trans_join_new(Config),
    alarm_rpc(raise),
    {ok, Instance} = alarm_check_raised(TId, Config),
    alarm_rpc(cease),
    alarm_check_ceased(TId, Instance, Config),
    ok = trans_finish(TId, Config).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Logging in the Audit Trail Log.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
log_auditTrail(Config) ->
    %% ------- Preparations:
    T = 10000,
    TId1 = trans_join_new(Config),
    TransNo1 = rct_rpc:call(?RCS, coi, get_transaction_number, [TId1], T),
    TransNoStr1 = "Transaction " ++ integer_to_list(TransNo1),
    TId2 = trans_join_new(Config),
    TransNo2 = rct_rpc:call(?RCS, coi, get_transaction_number, [TId2], T),
    TransNoStr2 = "Transaction " ++ integer_to_list(TransNo2),
    LogFiles = get_logfiles("AuditTrailLog"),
    Expected =
	[TransNoStr1 ++ " Invoke action",
	 TransNoStr1 ++ " Commit",
	 TransNoStr2 ++ " Invoke createMo",
	 TransNoStr2 ++ " Invoke setMo",
	 TransNoStr2 ++ " Invoke deleteMo",
	 TransNoStr2 ++ " Abort"],
    %% ------- Log actions:
    run_mfa({coi, action, [TId1,
			   ?DN_LogM,
			   ?ACTION_exportAVL,
			   [{?ATTR_uri, ?ATTRval_uri},
			    {?ATTR_pwd, ?ATTRval_pwd}]]},
	    Config),
    ok = trans_prepare(TId1, Config),   % Logs as Commit.
    run_mfa({coi, createMo, [TId2,
			     ?DN_TestRoot,
			     ?CLASS_TestClass1,
			     ?CLASSid_TestClass1,
			     ?CLASSval_TestClass1,
			     [{?ATTR_int32, ?ATTRval_int32(88)},
			      {?ATTR_struct1,
			       ?ATTRval_struct1(33, ?StrVal(?MODULE_STR))}]]},
	    Config),
    run_mfa({coi, setMoAttributes,[TId2,
				   ?DN_TestClass1("1"),
				   [{?ATTR_int32, ?ATTRval_int32(88)},
				    {?ATTR_struct1,
				     ?ATTRval_struct1(33,
						      ?StrVal(?MODULE_STR))}]]},
	    Config),
    run_mfa({coi, deleteMo, [TId2,
			     ?DN_TestClass1("1")]},
	    Config),
    trans_abort(TId2, Config),
    %% ------- Finish and check result:
    ok = trans_finish(TId1, Config),
    ok = trans_finish(TId2, Config),
    ct:pal("Expected in the Log:~n~p", [Expected]),
    ok = log_check(Expected, LogFiles).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Logging in the Availability Log.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
log_availability(Config) ->
    %% ------- Preparations:
    T = 10000,
    TS = os:timestamp(),
    TSstr = integer_to_list(calendar:time_to_seconds(TS)),
    HwInfo = ?MODULE_STR ++ " HwInfo " ++ TSstr,
    NodeInfo = ?MODULE_STR ++ " NodeInfo " ++ TSstr,
    OtherInfo = ?MODULE_STR ++ " OtherInfo " ++ TSstr,
    PgmInfo = ?MODULE_STR ++ " PgmInfo " ++ TSstr,
    PiuInfo = ?MODULE_STR ++ " PiuInfo " ++ TSstr,
    ServiceInfo = ?MODULE_STR ++ " ServiceInfo " ++ TSstr,
    Expected =
	[HwInfo, NodeInfo, OtherInfo, PgmInfo, PiuInfo, ServiceInfo],
    %% ------- Log actions:
    ok =
	run_mfa({coi, log_availability_hw, [TS,
					    'InService',
					    'Starting',
					    "HwType",
					    "HwAddress",
					    {"ProdNo", "ProdRev", "ProdName"},
					    {'AppLog', {'InfoText', HwInfo}},
					    sync]},
		Config),
    ok =
	run_mfa({coi, log_availability_node, [TS,
					      'OutOfService',
					      'ShutdownCommand',
					      0,
					      {'AppLog', {'InfoText',
							  NodeInfo}},
					      sync]},
		Config),
    ok =
	run_mfa({coi, log_availability_other, [TS,
					       'PartiallyOutOfService',
					       'UnOperational',
					       {'AppLog', {'InfoText',
							   OtherInfo}},
					       sync]},
		Config),
    ok =
	run_mfa({coi, log_availability_pgm, [TS,
					     undefined,
					     undefined,
					     'Mp',
					     {1, 2},
					     {"ProdNo", "ProdRev"},
					     {'AppLog', {'InfoText', PgmInfo}},
					     sync]},
		Config),
    ok =
	run_mfa({coi, log_availability_piu, [TS,
					     'InService',
					     'Starting',
					     undefined,
					     undefined,
					     {"ProdNo", "ProdRev", "ProdName"},
					     {'AppLog', {'InfoText', PiuInfo}},
					     sync]},
		Config),
    ok =
	run_mfa({coi, log_availability_service, [TS,
						 'InService',
						 'Operational',
						 "ServiceType",
						 "ServiceInstance",
						 {'AppLog', {'InfoText',
							     ServiceInfo}},
						 sync]},
		Config),
    ok =
	run_mfa({alhI, export_log, []},   % For validation against DTD.
		Config),
    %% ------- Check result:
    ct:pal("Expected in the Log:~n~p", [Expected]),
    {ok, LogString} = rct_rpc:call(?RCS, alhI, get_log, [], T),
    ok = result_checks(Expected, LogString).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Logging in the Security Log.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
log_security(Config) ->
    LogMsg = ?Log_Msg(?RPC_term_to_string(os:timestamp())),
    ok =
	run_mfa({coi, log_security, [3,
				     notice,
				     LogMsg]},
		Config),
    LogFiles = get_logfiles("SecurityLog"),
    ok = log_check([LogMsg], LogFiles),

    LogMsg2 = ?Log_Msg(?RPC_term_to_string(os:timestamp())),
    SrcIp = string:join([integer_to_list(rand:uniform(256)-1)|| _<-[1,2,3,4]],
			"."),
    ok =
	run_mfa({coi, log_security, [3,
				     notice,
				     LogMsg2,
				     SrcIp]},
		Config),
    ok = log_check([LogMsg2, SrcIp], LogFiles).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Translate integer value in MIM to enum string in MIB.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mib_mimVal2mibStr(Config) ->
    "diskFailure" =
	run_mfa({coi, mimVal_to_mibStr, ["ProbableCause", 73]},
		Config),
    "physicalViolation" =
	run_mfa({coi, mimVal_to_mibStr, ["EventType", 9]},
		Config),
    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc For SQR statistics. Functionality that is executed during system
%%%   restart does not show up in the SQR statistics. Needs to be run explicitly
%%%   by a testcase in order to become part of the coverage statistics.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mib_sqr(Config) ->
    Pid = run_mfa({erlang, spawn, [coiMib, post_init, []]}, Config),
    MaxTime = 60000,   % 60 seconds
    ok = wait_process_died(Pid, MaxTime, Config).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Get MIM class.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mim_class(Config) ->
    [{class, Attrs}] =
	run_mfa({coi, getMimClass, ["TestRoot"]},
		Config),
    true = is_list(Attrs),
    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Get MIM class path.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mim_classPath(Config) ->
    ["/ManagedElement/TestRoot"] =
	run_mfa({coi, getMimClassPath, ["TestRoot"]},
		Config),
    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Get MIM files.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mim_files(Config) ->
    MimFiles =
	run_mfa({coi, getMimFiles, []},
		Config),
    ct:pal("Files: ~p", [[lists:last(string:tokens(FileName, "/")) ||
			     FileName <- MimFiles]]),
    ok = mim_files_check(MimFiles).


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc For SQR statistics. Functionality that is executed during system
%%%   restart does not show up in the SQR statistics. Needs to be run explicitly
%%%   by a testcase in order to become part of the coverage statistics.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mim_sqr(Config) ->
    Pid = run_mfa({erlang, spawn, [coiMim, post_init, []]}, Config),
    MaxTime = 60000,   % 60 seconds
    ok = wait_process_died(Pid, MaxTime, Config).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Get MIM tree.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mim_tree(Config) ->
    [{"TestRoot", [{_, Level2} | _]}] =
	run_mfa({coi, getMimTree, [?DN_TestRoot,
				   2]},
		Config),
    true = is_list(Level2),
    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Help function keyfind.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
misc_keyfind(Config) ->
    MimClass =
	run_mfa({coi, getMimClass, ["TestRoot"]},
		Config),
    [{name, "TestRoot"}] =
	run_mfa({coi, keyfind, [MimClass,
				name]},
		Config),
    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Help function keyfind_all.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
misc_keyfind_all(Config) ->
    MimClass =
	run_mfa({coi, getMimClass, ["TestRoot"]},
		Config),
    Names =
	run_mfa({coi, keyfind_all, [MimClass,
				    name]},
		Config),
    Expected =
	[{name, "TestRoot"},
	 {name, "TestClass1"},   % There are more "TestClassX", but no need to
						% check them.
	 {name, "ManagedElement"},
	 {name, "testRootId"}],
    ok = member_check(Expected, Names).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Help function pairElems.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
misc_pairElems(Config) ->
    [{1, a}, {2, b}, {3, undefined}] =
	run_mfa({coi, pairElems, [[1, 2, 3],
				  [a, b]]},
		Config),
    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc MO action.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mo_req_action(Config) ->
    TId = trans_join_new(Config),
    run_mfa({coi, action, [TId,
			   ?DN_LogM,
			   ?ACTION_exportAVL,
			   [{?ATTR_uri, ?ATTRval_uri},
			    {?ATTR_pwd, ?ATTRval_pwd}]]},
	    Config),
    ok = trans_finish(TId, Config).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc MO countMoChildren.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mo_req_countMoChildren(Config) ->
    TId = trans_join_new(Config),
    Cnt =
	run_mfa({coi, countMoChildren, [TId,
					?DN_TestRoot,
					?CLASS_TestClass1]},
		Config),
    true = is_integer(Cnt),
    ok = trans_finish(TId, Config).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc MO createMo.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mo_req_createMo(Config) ->
    TId = trans_join_new(Config),
    ok =
	run_mfa({coi, createMo, [TId,
				 ?DN_TestRoot,
				 ?CLASS_TestClass1,
				 ?CLASSid_TestClass1,
				 ?CLASSval_TestClass1,
				 [{?ATTR_int32, ?ATTRval_int32(88)},
				  {?ATTR_struct1,
				   ?ATTRval_struct1(33,
						    ?StrVal(?MODULE_STR))}]]},
		Config),
    ok = trans_finish(TId, Config).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc MO deleteMo.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mo_req_deleteMo(Config) ->
    TId = trans_join_new(Config),
    ok =
	run_mfa({coi, deleteMo, [TId,
				 ?DN_TestClass1("1")]},
		Config),
    ok = trans_finish(TId, Config).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc MO existsMo.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mo_req_existsMo(Config) ->
    TId = trans_join_new(Config),
    false =
	run_mfa({coi, existsMo, [TId,
				 ?DN_TestClass1("77")]},
		Config),
    ok = trans_finish(TId, Config).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc MO getMoAttributes.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mo_req_getMoAttributes(Config) ->
    TId = trans_join_new(Config),
    Result =
	run_mfa({coi, getMoAttributes, [TId,
					?DN_TestClass1("1"),
					[?ATTR_int32, ?ATTR_struct1]]},
		Config),
    true = is_list(Result),
    ok = trans_finish(TId, Config).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc MO getMoAttributes, extended tests.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mo_req_getMoAttributes_extended(Config) ->
    % NOTE, this TC name is referred to in end_per_testcase

    rct_cli:connect(?CLI, print),

    ["scriptmode on",">"] = cliExec("scriptmode on", []),
    
    % instance with enum-valued attributes, no values yet
    cliExec("top", []),
    cliExec("ManagedElement=1,TestRoot=1", []),
    cliExec("configure", []),
    cliExec("TestClassA=991", []),
    cliExec("commit", []),
    [[],[]] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassA=991">>, [<<"enum">>, <<"menum">>]),
    
    % add a single enum value
    cliExec("configure", []),
    cliExec("enum=V117", []),
    cliExec("commit", []),
    [{12, 117}] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassA=991">>, [<<"enum">>]),
    
    % add a sequence-of-enum value
    cliExec("configure", []),
    cliExec("menum=[V117,V101]", []),
    cliExec("commit", []),
    [[{12,117},{12,101}],{12,117}] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassA=991">>, [<<"menum">>, <<"enum">>]),
    
    % modify the sequence-of-enum value to have just one element
    cliExec("configure", []),
    cliExec("menum=[V118]", []),
    cliExec("commit", []),
    [[{12,118}]] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassA=991">>, [<<"menum">>]),
    
    % create a TestClassE instance
    cliExec("top", []),
    cliExec("ManagedElement=1,TestRoot=1", []),
    cliExec("configure", []),
    cliExec("TestClassE=1", []),
    cliExec("intB=66", []),
    cliExec("intC=67", []),
    cliExec("commit", []),
    
    % unknown class

    % TODO, this case causes a crash; see Erlang log
    % {error, <<"Internal error">>} = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassZZZ=888">>, [<<"WXYZ">>]),

    % instance does not exist
    [undefined] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=888">>, [<<"intA">>]),
    
    % instance does not exist, two attributes asked for
    [undefined,undefined] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=888">>, [<<"intA">>, <<"intB">>]),
    
    % attribute does not exist
    % causes an ERROR in Erlang log, so don't do this
    % [undefined] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"WXYZ">>]),
    
    % attribute has no value
    [[]] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"intA">>]),
    
    % nillable attribute has value
    [{3,66}] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"intB">>]),
    
    % non-nillable attribute has value
    [{3,67}] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"intC">>]),
    
    % no attribute asked for (instance exists, or does not exist, or class unknown)
    [] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, []),
    [] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=888">>, []),
    [] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassZZZ=888">>, []),
    
    % three attributes asked for
    [{3,67}, {3,66}, []] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"intC">>, <<"intB">>, <<"intA">>]),
    
    % int sequence, empty
    [[]] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"intD">>]),
    
    % int sequence, one element
    cliExec("top", []),
    cliExec("ManagedElement=1,TestRoot=1,TestClassE=1", []),
    cliExec("configure", []),
    cliExec("intD=[68]", []), 
    cliExec("commit", []),
    [[{3,68}]] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"intD">>]),
    
    % int sequence, many elements
    cliExec("top", []),
    cliExec("ManagedElement=1,TestRoot=1,TestClassE=1", []),
    cliExec("configure", []),
    cliExec("intD=[68,69,70]", []), 
    cliExec("commit", []),
    [[{3,68},{3,69},{3,70}]] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"intD">>]),

    % string, no value
    [[]] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"strE">>]),

    % string, with value
    cliExec("top", []),
    cliExec("ManagedElement=1,TestRoot=1,TestClassE=1", []),
    cliExec("configure", []),
    cliExec("strE=\"sudavik\"", []), 
    cliExec("commit", []),
    [{9,<<"sudavik">>}] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"strE">>]),
    
    % struct attribute, no value
    [[]] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"structF">>]),
    
    % struct attribute, value is a struct with no values for any member
    cliExec("top", []),
    cliExec("ManagedElement=1,TestRoot=1,TestClassE=1", []),
    cliExec("configure", []),
    cliExec("structF", []), 
    cliExec("commit", []),    
    [{14,
      [{<<"enumK">>,[]},
       {<<"enumSeqL">>,[]},
       {<<"intA">>,[]},
       {<<"intSeqB">>,[]},
       {<<"strSeqC">>,[]}]}] = 
	get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"structF">>]),

    % update a primitive member of the struct instance
    cliExec("top", []),
    cliExec("ManagedElement=1,TestRoot=1,TestClassE=1", []),
    cliExec("configure", []),
    cliExec("structF,intA=965", []), 
    cliExec("commit", []), 
    [{14,
      [{<<"enumK">>,[]},
       {<<"enumSeqL">>,[]},
       {<<"intA">>,{3,965}},
       {<<"intSeqB">>,[]},
       {<<"strSeqC">>,[]}]}] = 
	get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"structF">>]),

    % update sequence members of the struct instance; one element in sequence
    cliExec("configure", []),
    cliExec("structF,intSeqB=[901]", []),
    cliExec("structF,strSeqC=[\"c\"]", []),
    cliExec("commit", []), 
    [{14,
      [{<<"enumK">>,[]},
       {<<"enumSeqL">>,[]},
       {<<"intA">>,{3,965}},
       {<<"intSeqB">>,[{3,901}]},
       {<<"strSeqC">>,[{9,<<"c">>}]}]}] = 
	get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"structF">>]),
    
    % update sequence members of the struct instance; several elements in sequence
    cliExec("configure", []),
    cliExec("structF,intSeqB=[901,-902,-903]", []),
    cliExec("structF,strSeqC=[\"xx\", \"yy\"]", []),
    cliExec("commit", []), 
    [{14,
      [{<<"enumK">>,[]},
       {<<"enumSeqL">>,[]},
       {<<"intA">>,{3,965}},
       {<<"intSeqB">>,[{3,901},{3,-902},{3,-903}]},
       {<<"strSeqC">>,[{9,<<"xx">>},{9,<<"yy">>}]}]}] = 
	get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"structF">>]),
    
    % update a simple enum member of the struct instance
    cliExec("configure", []),
    cliExec("structF,enumK=V118", []),
    cliExec("commit", []), 
    [{14,
      [{<<"enumK">>,{12,118}},
       {<<"enumSeqL">>,[]},
       {<<"intA">>,{3,965}},
       {<<"intSeqB">>,[{3,901},{3,-902},{3,-903}]},
       {<<"strSeqC">>,[{9,<<"xx">>},{9,<<"yy">>}]}]}] = 
	get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"structF">>]),
    
    % update a sequence-of-enum member of the struct instance, sequence length 1
    cliExec("configure", []),
    cliExec("structF,enumSeqL=[V117]", []),
    cliExec("commit", []), 
    [{14,
      [{<<"enumK">>,{12,118}},
       {<<"enumSeqL">>,[{12,117}]},
       {<<"intA">>,{3,965}},
       {<<"intSeqB">>,[{3,901},{3,-902},{3,-903}]},
       {<<"strSeqC">>,[{9,<<"xx">>},{9,<<"yy">>}]}]}] = 
	get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"structF">>]),
    
    % update a sequence-of-enum member of the struct instance, sequence length 3
    cliExec("configure", []),
    cliExec("structF,enumSeqL=[V117,V118,V101]", []),
    cliExec("commit", []), 
    [{14,
      [{<<"enumK">>,{12,118}},
       {<<"enumSeqL">>,[{12,117},{12,118},{12,101}]},
       {<<"intA">>,{3,965}},
       {<<"intSeqB">>,[{3,901},{3,-902},{3,-903}]},
       {<<"strSeqC">>,[{9,<<"xx">>},{9,<<"yy">>}]}]}] = 
	get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"structF">>]),
    
    % sequence of struct; sequence is empty
    [[]] = get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"structG">>]),
    
    % sequence of struct; sequence has one element
    cliExec("configure", []),
    cliExec("structG[@1],intC=765", []),
    cliExec("structG[@1],strF=\"zxc\"", []),
    cliExec("commit", []),    
    [[{14,
       [{<<"enumK">>,[]},
	{<<"intA">>, []},
	{<<"intB">>, []},
	{<<"intC">>, {3,765}},
	{<<"strD">>, []},
	{<<"strE">>, []},
	{<<"strF">>, {9,<<"zxc">>}}]}]] = 
	get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"structG">>]),
    
    % sequence of struct; sequence has one element, enum member is set too
    cliExec("configure", []),
    cliExec("no structG", []),
    cliExec("commit", []),
    cliExec("configure", []),
    cliExec("structG[@1],enumK=V101", []),
    cliExec("structG[@1],intC=765", []),
    cliExec("structG[@1],strF=\"zxc\"", []),
    cliExec("commit", []),    
    [[{14,
       [{<<"enumK">>, {12,101}},
	{<<"intA">>, []},
	{<<"intB">>, []},
	{<<"intC">>, {3,765}},
	{<<"strD">>, []},
	{<<"strE">>, []},
	{<<"strF">>, {9,<<"zxc">>}}]}]] = 
	get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"structG">>]),
    
    
    % sequence of struct; sequence has several elements
    cliExec("configure", []),
    cliExec("no structG", []),
    cliExec("commit", []), 
    cliExec("configure", []),
    cliExec("structG[@1],intC=333", []),
    cliExec("structG[@1],strF=\"aaa\"", []),
    cliExec("structG[@2],intA=111", []),
    cliExec("structG[@2],intC=444", []),
    cliExec("structG[@2],strF=\"bbb\"", []),
    cliExec("structG[@2],enumK=V117", []),
    cliExec("commit", []), 
    
    [[{14,[{<<"enumK">>,[]},
	   {<<"intA">>, []},
	   {<<"intB">>, []},
	   {<<"intC">>, {3,333}},
	   {<<"strD">>, []},
	   {<<"strE">>, []},
	   {<<"strF">>, {9,<<"aaa">>}}]},
      {14,[{<<"enumK">>,{12,117}},
	   {<<"intA">>, {3,111}},
	   {<<"intB">>, []},
	   {<<"intC">>, {3,444}},
	   {<<"strD">>, []},
	   {<<"strE">>, []},
	   {<<"strF">>, {9,<<"bbb">>}}]}]] = 
	get_mo_attributes(Config, <<"ManagedElement=1,TestRoot=1,TestClassE=1">>, [<<"structG">>]),

    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc MO getMoIterator.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mo_req_getMoIterator(Config) ->
    TId = trans_join_new(Config),
    Result =
	run_mfa({coi, getMoIterator, [TId,
				      ?DN_TestRoot,
				      ?CLASS_TestClass1]},
		Config),
    true = is_list(Result),
    ok = trans_finish(TId, Config).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc MO setMoAttributes.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mo_req_setMoAttributes(Config) ->
    TId = trans_join_new(Config),
    ok =
	run_mfa({coi,
		 setMoAttributes,
		 [TId,
		  ?DN_TestClass1("1"),
		  [{?ATTR_int32, ?ATTRval_int32(88)},
		   {?ATTR_struct1,
		    ?ATTRval_struct1(33,
				     ?StrVal(?MODULE_STR))}]]},
		Config),
    ok = trans_finish(TId, Config).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc MO bundle getMoTree.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mo_req_bundle_getMoTree(Config) ->
    [{{"TestRoot", {9, <<"1">>}}, Tree}] =
	run_mfa({coi, getMoTree, [?DN_TestRoot,
				  2]},
		Config),
    true = is_list(Tree),
    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc MO, complete test of all MO requests and check that they are committed.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
mo_req_AllTogether(Config) ->
    ct:pal("Transaction 1 ------- createMo:"),
    TId1 = trans_join_new(Config),
    MoChildren1 =
	run_mfa({coi, countMoChildren, [TId1,
					?DN_TestRoot,
					?CLASS_TestClass1]},
		Config),
    ok =
	run_mfa({coi, createMo, [TId1,
				 ?DN_TestRoot,
				 ?CLASS_TestClass1,
				 ?CLASSid_TestClass1,
				 ?CLASSval_TestClass1,
				 [{?ATTR_int32, ?ATTRval_int32(88)},
				  {?ATTR_struct1,
				   ?ATTRval_struct1(33,
						    ?StrVal(?MODULE_STR))}]]},
		Config),
    {ok, true} = trans_validate(TId1, Config),
    ok = trans_prepare(TId1, Config),
    ok = trans_commit(TId1, Config),
    ok = trans_finish(TId1, Config),
    ct:pal("Transaction 2 ------- Check createMo result:"),
    TId2 = trans_join_new(Config),
    true =
	run_mfa({coi, existsMo, [TId2,
				 ?DN_TestClass1("77")]},
		Config),
    MoChildren2 =
	run_mfa({coi, countMoChildren, [TId2,
					?DN_TestRoot,
					?CLASS_TestClass1]},
		Config),
    true = MoChildren2 == (MoChildren1 + 1),
    MoIterator1 =
	run_mfa({coi, getMoIterator, [TId2,
				      ?DN_TestRoot,
				      ?CLASS_TestClass1]},
		Config),
    ok = member_check([{9, <<"77">>}], MoIterator1),
    GetMoAttributes1 =
	run_mfa({coi, getMoAttributes, [TId2,
					?DN_TestClass1("77"),
					[?ATTR_int32, ?ATTR_struct1]]},
		Config),
    ok = member_check([?ATTRval_int32(88),
		       ?ATTRval_struct1(33, ?StrVal(?MODULE_STR))],
		      GetMoAttributes1),
    ok = trans_finish(TId2, Config),
    ct:pal("Transaction 3 ------- setMoAttributes:"),
    TId3 = trans_join_new(Config),
    ok =
	run_mfa({coi, setMoAttributes,[TId3,
				       ?DN_TestClass1("77"),
				       [{?ATTR_int32, ?ATTRval_int32(99)},
					{?ATTR_struct1,
					 ?ATTRval_struct1(33,
							  ?StrVal("iTest"))}]]},
		Config),
    {ok, true} = trans_validate(TId3, Config),
    ok = trans_prepare(TId3, Config),
    ok = trans_commit(TId3, Config),
    ok = trans_finish(TId3, Config),
    ct:pal("Transaction 4 ------- Check MoAttributes & deleteMo:"),
    TId4 = trans_join_new(Config),
    GetMoAttributes2 =
	run_mfa({coi, getMoAttributes, [TId4,
					?DN_TestClass1("77"),
					[?ATTR_int32, ?ATTR_struct1]]},
		Config),
    ok = member_check([?ATTRval_int32(99),
		       ?ATTRval_struct1(33, ?StrVal("iTest"))],
		      GetMoAttributes2),
    ok =
	run_mfa({coi, deleteMo, [TId4,
				 ?DN_TestClass1("77")]},
		Config),
    ok = trans_prepare(TId4, Config),
    ok = trans_commit(TId4, Config),
    ok = trans_finish(TId4, Config),
    ct:pal("Transaction 5 ------- Check deleteMo:"),
    TId5 = trans_join_new(Config),
    false =
	run_mfa({coi, existsMo, [TId5,
				 ?DN_TestClass1("77")]},
		Config),
    ok = trans_finish(TId5, Config).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Subscription of COI notifications.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
notif_subscribe(Config) ->
    T = 10000,
    {ok, STS_CbNotif_Tag} =   % Monitor callback results.
	rct_rpc:call(?RCS, sysTestServer, subscr_callback_result, [self()], T),
    ok =
	run_mfa({coi, subscribe, [coiServer]},
		Config),
    alarm_rpc(raise),
    receive
	{STS_CbNotif_Tag, {coi_notification_0, Events} = STS_CbNotif} ->
	    ct:pal("Callback result:~n~p", [STS_CbNotif]),
	    notif_subscribe_check(Events, Config)
    after
	T ->
	    ct:fail("~nNo notification received.~nTimeout: ~p ms", [T])
    end,
    alarm_rpc(cease),
    ok =
	run_mfa({coiEvent, unsubscribe, [coiServer]},
		Config).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Normal transaction sequence with commit.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
trans_seq_commit(Config) ->
    TId = trans_join_new(Config),
    {ok, true} = trans_validate(TId, Config),
    ok = trans_prepare(TId, Config),
    ok = trans_commit(TId, Config),
    ok = trans_finish(TId, Config).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Normal transaction sequence with abort.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
trans_seq_abort(Config) ->
    TId = trans_join_new(Config),
    ok = trans_abort(TId, Config),
    ok = trans_finish(TId, Config).


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Get HTTPS ports
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
get_https_ports(Config) ->
    [{https, X}, {https_login, Y}] =
	run_mfa({coi, getHttpsPorts, []},
		Config),
    true = is_integer(X),
    true = is_integer(Y),
    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc User lookup of maintenance user (authentication and fetch roles)
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
lookup_maintenance_user(Config) ->
    %% Create a maintenance user first
    ok = run_mfa({coi, createFakeMaintenanceUser, []}, Config),
    {true, MaintUserRoles} = 
    run_mfa({coi, lookup, ["faketestmaintuser", "Letmein01"]}, Config),
    true = is_list(MaintUserRoles),

    ok.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc User lookup in "LDAP" of normal user with password (authentication and fetch roles)
%%%
%%% @end
%%% ----------------------------------------------------------
%%% ###=====================================================================###
lookup_pw_user(Config) ->
    %% Create a faked user first
    {ok, _} = run_mfa({coi, createFakeUser, []}, Config),
    {true, NormalUserRoles} = 
    run_mfa({coi, lookup, ["faketestuser", "pw"]}, Config),
    true = is_list(NormalUserRoles),

    ok.

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% alarm_rpc
%%%
%%% ###=====================================================================###
alarm_rpc(raise) ->
    T = 10000,
    SendAlrm_Args =
	[?Alrm_Name,
	 warning,
	 ?Alrm_Dn,
	 ?Alrm_AdditionalText,
	 []],
    ok = rct_rpc:call(?RCS, comsaI, send_alarm, SendAlrm_Args, T);
alarm_rpc(cease) ->
    T = 10000,
    ClearAlrm_Args = [?Alrm_Name, ?Alrm_Dn],
    ok = rct_rpc:call(?RCS, comsaI, clear_alarm, ClearAlrm_Args, T).

%%% ###########################################################################
%%% alarm_check_ceased
%%%
%%% ###=====================================================================###
alarm_check_ceased(TId, Instance, Config) ->
    alarm_check_ceased_loop(40,TId, Instance, Config).
						% 250 ms * 40 = 10 seconds.

alarm_check_ceased_loop(Cnt, TId, Instance, Config) when Cnt >= 0 ->
    MFA = {coi, getMoIterator, [TId, ?DN_Fm, ?CLASS_FmAlarm]},
    Instances = rct_rcsTestServer:run(MFA, Config),
    case lists:member(Instance, Instances) of
	false ->
	    {_, InstanceValue} = Instance,
	    ct:pal("FmAlarm=~s deleted from the MO table after ~p ms",
		   [binary_to_list(InstanceValue), (40 - Cnt) * 250]),
	    ok;
	true ->
	    timer:sleep(250),   % 250 ms * 40 = 10 seconds.
	    alarm_check_ceased_loop(Cnt - 1, TId, Instance, Config)
    end;
alarm_check_ceased_loop(_, _, _, _) ->
    ct:fail("~nAlarm not deleted from FmAlarm table.").

%%% ###########################################################################
%%% alarm_check_raised
%%%
%%% ###=====================================================================###
alarm_check_raised(TId, Config) ->
    alarm_check_raised_loop(40, TId, Config).
						% 250 ms * 40 = 10 seconds.

alarm_check_raised_loop(Cnt, TId, Config) when Cnt >= 0 ->
    MFA = {coi, getMoIterator, [TId, ?DN_Fm, ?CLASS_FmAlarm]},
    Instances = rct_rcsTestServer:run(MFA, Config),
    case alarm_check_raised_isInTable(Instances, TId, Config) of
	{true, {_, InstanceValue} = Instance} ->
	    ct:pal("FmAlarm=~s found in the MO table after ~p ms",
		   [binary_to_list(InstanceValue), (40 - Cnt) * 250]),
	    {ok, Instance};
	false ->
	    timer:sleep(250),   % 250 ms * 40 = 10 seconds.
	    alarm_check_raised_loop(Cnt - 1, TId, Config)
    end;
alarm_check_raised_loop(_, _, _) ->
    ct:fail("~nAlarm not stored in FmAlarm table.").

alarm_check_raised_isInTable([{_, InstanceValue} = Instance| Tail],
			     TId,
			     Config) ->
    DN =
	list_to_binary(
	  binary_to_list(?DN_Fm) ++
	  "," ++
	  binary_to_list(?CLASS_FmAlarm) ++
	  "=" ++
	  binary_to_list(InstanceValue)),
    My_Alrm_AdditionalText = list_to_binary(?Alrm_AdditionalText),
    MFA = {coi, getMoAttributes, [TId, DN, [?ATTR_additionalText]]},
    case rct_rcsTestServer:run(MFA, Config) of
	[{_, My_Alrm_AdditionalText}] ->
	    {true, Instance};
	_ ->
	    alarm_check_raised_isInTable(Tail, TId, Config)
    end;
alarm_check_raised_isInTable([], _, _) ->
    false.

%%% ###########################################################################
%%% get_logfiles
%%%
%%% ###=====================================================================###
get_logfiles(LogName) ->
    case rct_rpc:call(?RCS, disk_log, info, [LogName], 10000) of
	LogInfo when is_list(LogInfo) ->
	    File = proplists:get_value(file, LogInfo),
	    {_, MaxNoFiles} = proplists:get_value(size, LogInfo),
	    Suffixes = get_logfiles_suffixes(MaxNoFiles),
	    [File ++ "." ++ Suffix || Suffix <- Suffixes];
	Error ->
	    ct:fail("~p; ~p", [LogName, Error])
    end.

%%% ###=====================================================================###
get_logfiles_suffixes(MaxNoFiles) when is_integer(MaxNoFiles) ->
    get_logfiles_suffixes(lists:seq(1, MaxNoFiles));
get_logfiles_suffixes([Suffix | Tail]) ->
    [integer_to_list(Suffix) | get_logfiles_suffixes(Tail)];
get_logfiles_suffixes([]) ->
    [].

%%% ###########################################################################
%%% log_check
%%%
%%% ###=====================================================================###
log_check(Expected, LogFiles) ->
    log_check(Expected, LogFiles, 40).

%%% ###=====================================================================###
log_check([LogMsg | Tail], LogFiles, Cnt) ->
    case log_check_loop(Cnt, LogFiles, LogMsg) of
	{ok, LoopedCnt} ->
	    log_check(Tail, LogFiles, LoopedCnt);
	fail ->
	    ct:fail("~nLog file not updated.~nChecked 40 times.")
    end;
log_check([], _, Cnt) ->
    ct:pal("Log file(s) updated after ~p ms", [(40 - Cnt) * 250]),
    ok.

%%% ###=====================================================================###
log_check_loop(Cnt, LogFiles, LogMsg) when Cnt > 0 ->
    case log_check_files(LogFiles, LogMsg) of
	not_found ->
	    timer:sleep(250),
	    log_check_loop(Cnt - 1, LogFiles, LogMsg);
	found ->
	    {ok, Cnt}
    end;
log_check_loop(_, _, _) ->
    fail.

%%% ###=====================================================================###
log_check_files([LogFile | Tail], LogMsg) ->
    case ?RPC_read_file(LogFile) of
	{ok, LogFileContent} ->
	    LogString = binary_to_list(LogFileContent),
	    case string:str(LogString, LogMsg) of
		0 ->
		    log_check_files(Tail, LogMsg);
		_ ->
		    found
	    end;
	_ ->
	    log_check_files(Tail, LogMsg)
    end;
log_check_files([], _) ->
    not_found.

%%% ###########################################################################
%%% member_check
%%%
%%% ###=====================================================================###
member_check([Elem | Tail], List) ->
    case lists:member(Elem, List) of
	true ->
	    member_check(Tail, List);
	false ->
	    ct:pal("Result: ~p", [List]),
	    {error, {missing_in_result, Elem}}
    end;
member_check([], _) ->
    ok.

%%% ###########################################################################
%%% mim_files_check
%%%
%%% ###=====================================================================###
mim_files_check([FileName | Tail]) ->
    T = 10000,
    case rct_rpc:call(?RCS, file, read_file_info, [FileName], T) of
	{ok, _} ->
	    mim_files_check(Tail);
	Error ->
	    ct:fail("~n~p~nFile: ~p", [Error, FileName])
    end;
mim_files_check([]) ->
    ok.

%%% ###########################################################################
%%% notif_subscribe_check
%%%
%%% ###=====================================================================###
notif_subscribe_check(Events, Config) ->
    EventProps = proplists:get_value(coi_event_0, Events),
    Attrs = proplists:get_value(attributes, EventProps),
    true = lists:member(?ATTR_additionalText, Attrs),
    [Dn] = proplists:get_value(dn, EventProps),
    TId = trans_join_new(Config),
    [{_, EventAdditionalText}] =
	run_mfa({coi, getMoAttributes, [TId,
					Dn,
					[?ATTR_additionalText]]},
		Config),
    trans_finish(TId, Config),
    AdditionalText = ?Alrm_AdditionalText,
    case ?RPC_term_to_string(EventAdditionalText) of
	AdditionalText ->
	    ok;
	_ ->
	    ct:fail("~nNo match:~nExpected:~p~nGot:~p",
		    [AdditionalText, EventAdditionalText])
    end.

%%% ###########################################################################
%%% result_checks
%%%
%%% ###=====================================================================###
result_checks([ExpectedSubstring | Tail], ResultString) ->
    case string:str(ResultString, ExpectedSubstring) of
	0 ->
	    {error, {missing_in_result, ExpectedSubstring}};
	_ ->
	    result_checks(Tail, ResultString)
    end;
result_checks([], _) ->
    ok.

%%% ###########################################################################
%%% run_mfa
%%%
%%% ###=====================================================================###
run_mfa(MFA, Config) ->
    Result = rct_rcsTestServer:run(MFA, Config),
    ct:pal("MFA: ~p~nResult: ~p", [MFA, Result]),
    Result.

%%% ###########################################################################
%%% trans_abort
%%%
%%% ###=====================================================================###
trans_abort(TId, Config) ->
    run_mfa({coi, abort_transaction, [TId]}, Config).

%%% ###########################################################################
%%% trans_commit
%%%
%%% ###=====================================================================###
trans_commit(TId, Config) ->
    run_mfa({coi, commit, [TId]}, Config).

%%% ###########################################################################
%%% trans_finish
%%%
%%% ###=====================================================================###
trans_finish(TId, Config) ->
    run_mfa({coi, finish, [TId]}, Config).

%%% ###########################################################################
%%% trans_join_new
%%%
%%% ###=====================================================================###
trans_join_new(Config) ->
    MFA =
	{coi, join_new, [?MODULE, ?FUNCTION]},
    {ok, TId} = rct_rcsTestServer:run(MFA, Config),
    ct:pal("MFA: ~p~nTransactionId: ~p", [MFA, TId]),
    TId.

%%% ###########################################################################
%%% trans_prepare
%%%
%%% ###=====================================================================###
trans_prepare(TId, Config) ->
    run_mfa({coi, prepare, [TId]}, Config).

%%% ###########################################################################
%%% trans_validate
%%%
%%% ###=====================================================================###
trans_validate(TId, Config) ->
    run_mfa({coi, validate, [TId]}, Config).

%%% ###########################################################################
%%% wait_process_died
%%%
%%% ###=====================================================================###
wait_process_died(Pid, MaxTime, Config) ->
    wait_process_died(Pid, MaxTime, 0, Config).

wait_process_died(Pid, MaxTime, PrevWaitTime, Config) when MaxTime > 0 ->
    T = 250,
    timer:sleep(T),
    WaitTime = PrevWaitTime + T,
    case run_mfa({erlang, is_process_alive, [Pid]}, Config) of
	true ->
	    wait_process_died(Pid, MaxTime - T, WaitTime, Config);
	false ->
	    ct:pal("Process ~p gone after ~p ms", [Pid, WaitTime]),
	    ok
    end;
wait_process_died(Pid, _, WaitTime, Config) ->
    ProcInfo = run_mfa({erlang, process_info, [Pid]}, Config),
    ct:fail("~nTimeout after ~p ms, process ~p still alive.~nProcess info:~n~p",
	    [WaitTime, Pid, ProcInfo]).

%%% ###########################################################################
%%% get_mo_attributes
%%%
%%% ###=====================================================================###
-spec get_mo_attributes(any(), binary(), [binary()]) -> [any()].

get_mo_attributes(Config, Dn, AttrNames) ->
    Tid = trans_join_new(Config),
    MFA = {coi, getMoAttributes, [Tid, Dn, AttrNames]},
    Result = run_mfa(MFA, Config),
    trans_finish(Tid, Config),
    Result.

%%% ###########################################################################
%%% cliExec
%%%
%%% ###=====================================================================###
cliExec(Format, Data) ->
    {ok, Response} =
	rct_cli:send(?CLI, lists:flatten(io_lib:format(Format, Data))),
    cliResponseSplit(Response).

%%% ###########################################################################
%%% cliResponseSplit
%%%
%%% ###=====================================================================###
cliResponseSplit(Response) ->
    string:tokens(
      lists:filter(
	fun(C) -> C =/= $\r end, 
	lists:flatten(Response)), "\n").

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 4     CODE FOR TEMPORARY CORRECTIONS
%%% ###---------------------------------------------------------------------###
