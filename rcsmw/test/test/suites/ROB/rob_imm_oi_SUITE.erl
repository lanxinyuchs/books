%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rob_imm_oi_SUITE.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R8A/1

%%% @doc == ROB IMM OI test suite ==
%%% @end

-module(rob_imm_oi_SUITE).
-include_lib("common_test/include/ct.hrl").

%%% ----------------------------------------------------------
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
%%% R1A/1      2013-07-02 etxivri     Moved TC from /CM/cm_safc_basic_SUITE
%%% R2A/3      2013-09-10 etxivri     Updates due to renamed saf-rpc hooks.
%%% R2A/4      2013-09-23 etxivri     Updates due usage of lib-netconf.
%%%                                   Added more TCs.
%%% R2A/5      2013-09-23 etxivri     Added more TCs.
%%% R2A/6      2013-09-26 etxivri     Added more TCs.
%%% R2A/7      2013-09-26 etxivri     Changed ct:pal to ct:log in get_pids().
%%% R2A/8      2013-09-30 etxivri     Added more TCs
%%% R2A/9      2013-10-09 etxivri     Update to make init:reboot more robust.
%%% R2A/10     2013-11-19 etxivri     Update in check that implementer exist.
%%% R2A/11     2013-12-11 etxivri     Update to use add one attribute results in
%%%                                   expect one modify callback.
%%% R2A/12     2014-01-10 etxivri     Added printout to get a non rpeteteive problem.
%%% R2A/13     2014-02-07 etxivri     Added a check that testnode is alive,
%%%                                   before wait for testnode up.
%%%                                   To make it more robust.
%%% R2A/14     2014-06-04 etxivri     Changed check for "du1 login" prompt to
%%%                                   "login:"
%%% R2A/15     2014-10-07 etxivri     A try make it mor robust.
%%% R3A/1      2015-03-17 etxivri     Update logging
%%% R4A/1      2015-09-10 etxivri     Update due to changed behaviour.
%%% R4A/2      2015-09-18 etxivri     Again, Update due to changed behaviour.
%%% R4A/3      2015-10-20 etxivri     Add sleep 35 sec in restart tc due to new
%%%                                   behaviour when write conf to disc.
%%% R4A/4      2015-10-23 etxivri     Remove init_restart tests to be runed.
%%% R4A/5      2016-02-19 etxkols     Changed rct_rpc:call timeouts from 1000 to 10000 millisecs
%%% R5A/1      2016-05-31 etxivri     Update ERROR filter for 16B, due to
%%%                                   sometimes cec error occours. That is a
%%%                                   timining issue that is fixed in R17.
%%% R56/1      2016-06-01 etxivri     Update ERROR filter for R17.
%%% R8A/1      2016-12-20 etxpeno     Change attribute in nc_add_one_attr/3
%%%                                   due to no support of "struct as attribute"
%%%                                   in SAFE
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).

-export([testclass1_create_mod_delete_delay_on_complete/1,
	 ci_kill_appl_set_impl_again/1,
	 ci_init_restart_set_impl_again/1,
	 ci_init_reboot_set_impl_again/1,
	 oi_kill_appl_set_impl_again/1,
	 oi_init_restart_set_impl_again/1,
	 oi_init_reboot_set_impl_again/1,
	 several_oi_kill_appl_set_impl_again/1,
	 several_oi_init_restart_set_impl_again/1,
	 several_oi_init_reboot_set_impl_again/1
	]).

%-compile(export_all).
-include_lib("safe/include/safe.hrl").
-include_lib("safe/include/safe_imm.hrl").

-define(TESTMOM_NAME,'TESTMOMTestClass1').
-define(IMPL_NAME_1, "ImplementerOne").
-define(IMM_TEST_CLASS1, "TESTMOMTestClass1").
-define(NC_SESSION_1, nc1).
-define(INST_NAME_1, "nc_1").
-define(ATTR_NAME_1, "12345").
-define(OBJ_IMPL_NAME_1, "nisse").
-define(CONF_OBJ_1, "testClass1Id=nc_1,TESTMOMtestRootId=1").
-define(NR_OF_OI, 50).

-define(SAFE_CCB_CB_CREATE, safe_imm_oi_ccb_object_create_callback_2).
-define(SAFE_CCB_CB_MOD, safe_imm_oi_ccb_object_modify_callback_2).
-define(SAFE_CCB_CB_DEL, safe_imm_oi_ccb_object_delete_callback).
-define(SAFE_CCB_CB_COMP, safe_imm_oi_ccb_completed_callback).
-define(SAFE_CCB_CB_APP, safe_imm_oi_ccb_apply_callback).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log, rct_safe_imm_oi_rpc
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks, [%%{rct_rpc, rpc},
		 %%{rct_htmllink,[]},
		 %% {rct_safe_imm_oi_rpc,[{finalize_on_fail,true}]},
		 {rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {rct_netconf,nc1},
		 {rct_rs232,console},
		 %% {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
		 %% 			       ["Program ID [0-9]+ has terminated"]}}]}},
		 %% {cth_conn_log,[]},
		 %% %% {rct_safe_imm_oi_rpc,[{safe_debug_level, 2},{finalize_on_fail,true}]},
		 {rct_logging, {oi_testapp,
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"],
					  ["Program ID [0-9]+ has terminated"]}}]}},
		 {rct_safe_imm_oi_rpc,[{finalize_on_fail,true}]},
		 {rct_core,[]}
		]}].

%% @hidden
init_per_suite(Config) ->
    Config.

%% @hidden
end_per_suite(_Config) ->
    ok.

%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.

%% @hidden
end_per_testcase(_TestCase, Config) ->
    print_pids(),
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p.  \nClean up instances", [Reason]),

	    CI_1 = rct_rpc:call(rpc, safs_imm_db, ci_get, ['TESTMOMTestClass1'], 10000, noprint),
	    ct:pal("OI_1: ~p.", [CI_1]),

	    NrList = lists:seq(1,?NR_OF_OI),
	    lists:foreach(fun(Nr) ->
				  ct_netconfc:open(?NC_SESSION_1, [{timeout,30000}]),
				  NR = integer_to_list(Nr),
				  InstName = "nc_"++NR,
				  rct_nc_testclass1_lib:nc_delete_mo_instance_no_check(?NC_SESSION_1,
											    InstName),
				  ct_netconfc:close_session(?NC_SESSION_1, 30000)
			  end, NrList)
    end,

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     testclass1_create_mod_delete_delay_on_complete,
     ci_kill_appl_set_impl_again,
     %% ci_init_restart_set_impl_again,
     ci_init_reboot_set_impl_again,
     oi_kill_appl_set_impl_again,
     %% oi_init_restart_set_impl_again,
     oi_init_reboot_set_impl_again,
     several_oi_kill_appl_set_impl_again,
     %% several_oi_init_restart_set_impl_again,
     several_oi_init_reboot_set_impl_again
    ].

%%--------------------------------------------------------------------
%% @doc
%% Test create, modify and delete of an object of TestClass1 with ok result.
%% A 10 sec delay is added when complete call i received.
%% Verify that the correct OI callbacks are executed.
%% <br/><br/>
%% @spec testclass1_create_mod_delete_delay_on_complete(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
testclass1_create_mod_delete_delay_on_complete(_Config) ->
    print_pids(),
    CbFun = get_cb_fun_delay_on_complete(), %% add a sleep of 10 sec when rec complete.
    Handle = create_class_impl(CbFun, ?IMPL_NAME_1, ?IMM_TEST_CLASS1),
    try
	ok = nc_open_session([?NC_SESSION_1]),
	{error, _} = rct_nc_testclass1_lib:nc_get_mo_instance_no_check(?NC_SESSION_1,
								       ?INST_NAME_1),
	ok = rct_nc_testclass1_lib:nc_add_mo_instance(?NC_SESSION_1,
						      ?INST_NAME_1),
	ok = rct_nc_testclass1_lib:nc_get_mo_instance_check(?NC_SESSION_1,
							    ?INST_NAME_1),
	StartA = os:timestamp(),
	ok = nc_close_session([?NC_SESSION_1]),

	ok = expect_callback(?SAFE_CCB_CB_CREATE),
	%% ok = expect_callback(?SAFE_CCB_CB_MOD),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	EndA = os:timestamp(),
	ok = expect_callback(?SAFE_CCB_CB_APP),
	TimeDiffA = trunc(timer:now_diff(EndA, StartA) / 1000 / 1000),
	ct:pal("TimeDiffA: ~p", [TimeDiffA]),
	case TimeDiffA > 9 of
	    true ->
		ok;
	    false ->
		ct:fail("callback was sent before expected time")
	end,
	timer:sleep(100),
	ok = nc_open_session([?NC_SESSION_1]),
	ok = rct_nc_testclass1_lib:nc_get_mo_instance_check(?NC_SESSION_1,
							    ?INST_NAME_1),
	ok = nc_add_one_attr(?NC_SESSION_1, ?INST_NAME_1, ?ATTR_NAME_1),

	StartB = os:timestamp(),
	ok = nc_close_session([?NC_SESSION_1]),

	ok = expect_callback(?SAFE_CCB_CB_MOD),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	EndB = os:timestamp(),
	ok = expect_callback(?SAFE_CCB_CB_APP),
	TimeDiffB = trunc(timer:now_diff(EndB, StartB) / 1000 / 1000),
	ct:pal("TimeDiffA: ~p", [TimeDiffB]),
	case TimeDiffB > 9 of
	    true ->
		ok;
	    false ->
    	    ct:fail("callback was sent before expected time")
	end,

	timer:sleep(100),
	ok = nc_open_session([?NC_SESSION_1]),
	ok = rct_nc_testclass1_lib:nc_delete_mo_instance(?NC_SESSION_1,
							 ?INST_NAME_1),
	{error, _} = rct_nc_testclass1_lib:nc_get_mo_instance_no_check(?NC_SESSION_1,
								       ?INST_NAME_1),
	StartC = os:timestamp(),
	ok = nc_close_session([?NC_SESSION_1]),

	ok = expect_callback(?SAFE_CCB_CB_DEL),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	EndC = os:timestamp(),
	ok = expect_callback(?SAFE_CCB_CB_APP),

	TimeDiffC = trunc(timer:now_diff(EndC, StartC) / 1000 / 1000),
	ct:pal("TimeDiffA: ~p", [TimeDiffC]),
	case TimeDiffC > 9 of
	    true ->
		ok;
	    false ->
		ct:fail("callback was sent before expected time")
	end,

	timer:sleep(100),
	ok = nc_open_session([?NC_SESSION_1]),
	{error, _} = rct_nc_testclass1_lib:nc_get_mo_instance_no_check(?NC_SESSION_1, ?INST_NAME_1),
	ok = nc_close_session([?NC_SESSION_1]),

	delete_class_impl(?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle)
    catch
	_:Reason ->
	    ct:pal("TC fail, Reason: ~p", [Reason]),
	    cleanup_class_impl(?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle),
	    ct:fail(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Class Implementer. After Class impolementer is set and application restart,
%% then it is enough to set implementer again to be able to receive callbacks.
%% Verify that the correct OI callbacks are executed.
%% <br/><br/>
%% @spec ci_kill_appl_set_impl_again(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
ci_kill_appl_set_impl_again(_Config ) ->
    {_ComPid, _BeamPid, _TestNodeBeamPid, TestOiPid} = print_pids(),
    %%%%
    %% Create Class Implementer.
    %%%%
    CbFun = get_cb_fun(),
    ct:pal("### CbFun: ~p",[CbFun]),
    Handle = create_class_impl(CbFun, ?IMPL_NAME_1, ?IMM_TEST_CLASS1),
    %%%%
    %% Check CI exist.
    %%%%
    ok = check_ci_impl_name_1_exist(),
    try
        %%%%
	%% Add MO inst
        %%%%
	ok = add_testclass1_mo_inst(),
	ok = get_testclass1_mo_inst(),
	ok = expect_callback(?SAFE_CCB_CB_CREATE),
	%% ok = expect_callback(?SAFE_CCB_CB_MOD),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	ok = expect_callback(?SAFE_CCB_CB_APP),
        %%%%
	%% kill testnode
        %%%%
	ok = kill_test_oi(TestOiPid)
    catch
	_:Reason ->
	    ct:pal("TC fail, Reason: ~p", [Reason]),
	    cleanup_class_impl(?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle),
	    ct:fail(Reason)
    end,

    ci_set_impl_again_and_configure(CbFun) ,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Class Implementer. After Class impolementer is set and init:restart,
%% then it is enough to set implementer again to be able to receive callbacks.
%% Verify that the correct OI callbacks are executed.
%% <br/><br/>
%% @spec ci_init_restart_set_impl_again(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
ci_init_restart_set_impl_again(_Config ) ->
    print_pids(),
    %%%%
    %% Create Class Implementer.
    %%%%
    CbFun = get_cb_fun(),
    ct:pal("### CbFun: ~p",[CbFun]),
    Handle = create_class_impl(CbFun, ?IMPL_NAME_1, ?IMM_TEST_CLASS1),
    %%%%
    %% Check CI exist.
    %%%%
    ok = check_ci_impl_name_1_exist(),
    try
        %%%%
	%% Add MO inst
        %%%%
	ok = add_testclass1_mo_inst(),
	ok = get_testclass1_mo_inst(),
	ok = expect_callback(?SAFE_CCB_CB_CREATE),
	%% ok = expect_callback(?SAFE_CCB_CB_MOD),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	ok = expect_callback(?SAFE_CCB_CB_APP),

	ct:pal("sleep 35 sec"),
	ct:sleep({seconds, 35}),

        %%%%
	%% init:restart
        %%%%
	ok = init_restart()
    catch
	_:Reason ->
	    ct:pal("TC fail, Reason: ~p", [Reason]),
	    cleanup_class_impl(?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle),
	    ct:fail(Reason)
    end,

    ci_set_impl_again_and_configure(CbFun),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Class Implementer. After Class impolementer is set and init:reboot,
%% then it is enough to set implementer again to be able to receive callbacks.
%% Verify that the correct OI callbacks are executed.
%% <br/><br/>
%% @spec ci_init_reboot_set_impl_again(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
ci_init_reboot_set_impl_again(_Config ) ->
    print_pids(),
    %%%%
    %% Create Class Implementer.
    %%%%
    CbFun = get_cb_fun(),
    ct:pal("### CbFun: ~p",[CbFun]),
    Handle = create_class_impl(CbFun, ?IMPL_NAME_1, ?IMM_TEST_CLASS1),
    %%%%
    %% Check CI exist.
    %%%%
    ok = check_ci_impl_name_1_exist(),
    try
        %%%%
	%% Add MO inst
        %%%%
	ok = add_testclass1_mo_inst(),
	ok = get_testclass1_mo_inst(),
	ok = expect_callback(?SAFE_CCB_CB_CREATE),
	%% ok = expect_callback(?SAFE_CCB_CB_MOD),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	ok = expect_callback(?SAFE_CCB_CB_APP),

	ct:pal("sleep 35 sec"),
	ct:sleep({seconds, 35}),

        %%%%
	%% init:reboot node
        %%%%
	ok = init_reboot()
    catch
	_:Reason ->
	    ct:pal("TC fail, Reason: ~p", [Reason]),
	    cleanup_class_impl(?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle),
	    ct:fail(Reason)
    end,

    ci_set_impl_again_and_configure(CbFun),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Class Implementer, Set implementer again. Do not set class implementer again.
%% - Delete existing MO inst.
%% - Add MO inst again.
%% - Add attributes.
%% - Delete MO instans.
%% Verify that the correct OI callbacks are executed.
%% <br/><br/>
%% @spec ci_set_impl_again_and_configure(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
ci_set_impl_again_and_configure(CbFun) ->
    print_pids(),
    %%%%
    %% implementer set again. NOTE! Not set class implementer.
    %%%%
    ct:pal("Initialize OI Handle", []),
    {ok, Handle, Vsn} = rct_safe_imm_oi_rpc:initialize_2(CbFun),
    ct:pal("Again Initialize ok: ~p ~p",[Handle, Vsn]),
    ok = rct_safe_imm_oi_rpc:implementer_set(Handle, ?IMPL_NAME_1),
    ct:pal("Again Set Implementer name: ~p", [?IMPL_NAME_1]),

    try
   	%%%%
	%% Delete MO instance.
	%%%%
	timer:sleep(1000),
	ok = del_testclass1_mo_inst(),
	{error, _} = get_testclass1_mo_inst_no_check(),

	ok = expect_callback(?SAFE_CCB_CB_DEL),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	ok = expect_callback(?SAFE_CCB_CB_APP),

	%%%%
	%% Add MO instance.
	%%%%
	ok = add_testclass1_mo_inst(),

	ok = expect_callback(?SAFE_CCB_CB_CREATE),
	%% ok = expect_callback(?SAFE_CCB_CB_MOD),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	ok = expect_callback(?SAFE_CCB_CB_APP),

	ok = get_testclass1_mo_inst(),
	%%%%
	%% Modify MO instance, add atrributes.
	%%%%
	timer:sleep(1000),
	ok = add_one_inst_attr(),

	ok = expect_callback(?SAFE_CCB_CB_MOD),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	ok = expect_callback(?SAFE_CCB_CB_APP),
    	%%%%
	%% Delete MO instance.
	%%%%
	timer:sleep(1000),
	ok = del_testclass1_mo_inst(),
	{error, _} = get_testclass1_mo_inst_no_check(),

	ok = expect_callback(?SAFE_CCB_CB_DEL),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	ok = expect_callback(?SAFE_CCB_CB_APP),

	delete_class_impl(?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle)

    catch
	_:Reason ->
	    ct:pal("TC fail, Reason: ~p", [Reason]),
	    cleanup_class_impl(?IMM_TEST_CLASS1, ?IMPL_NAME_1, Handle),
	    ct:fail(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Object Implementer. After object implementer is set and application restart,
%% then it is enough to set implementer again to be able to receive callbacks.
%% Verify that the correct OI callbacks are executed.
%% <br/><br/>
%% @spec oi_kill_appl_set_impl_again(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
oi_kill_appl_set_impl_again(_Config ) ->
    {_ComPid, _BeamPid, _TestNodeBeamPid, TestOiPid} = print_pids(),
    %%%%
    %% Add MO inst
    %%%%
    ok = add_testclass1_mo_inst(),
    ok = get_testclass1_mo_inst(),
    %%%%
    %% Create Object Implementer.
    %%%%
    CbFun = get_cb_fun(),
    ct:pal("### CbFun: ~p",[CbFun]),
    create_obj_impl(CbFun, ?OBJ_IMPL_NAME_1, ?CONF_OBJ_1, ?SAFE_IMM_ONE),
    %%%%
    %% Check OI exist.
    %%%%
    ok = check_oi_impl_name_1_exist(),
    %%%%
    %% kill testnode
    %%%%
    ok = kill_test_oi(TestOiPid),

    oi_set_impl_again_and_configure(CbFun) ,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Object Implementer. After Object impolementer is set and init:restart,
%% then it is enough to set implementer again to be able to receive callbacks.
%% Verify that the correct OI callbacks are executed.
%% <br/><br/>
%% @spec oi_init_restart_set_impl_again(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
oi_init_restart_set_impl_again(_Config ) ->
    print_pids(),
    %%%%
    %% Add MO inst
    %%%%
    ok = add_testclass1_mo_inst(),
    ok = get_testclass1_mo_inst(),
    %%%%
    %% Create Object Implementer.
    %%%%
    CbFun = get_cb_fun(),
    ct:pal("### CbFun: ~p",[CbFun]),
    create_obj_impl(CbFun, ?OBJ_IMPL_NAME_1, ?CONF_OBJ_1, ?SAFE_IMM_ONE),
    %%%%
    %% Check OI exist.
    %%%%
    ok = check_oi_impl_name_1_exist(),
    %%%%
    %% init:restart
    %%%%

    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    ok = init_restart(),

    oi_set_impl_again_and_configure(CbFun),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Object Implementer. After Object impolementer is set and init:reboot,
%% then it is enough to set implementer again to be able to receive callbacks.
%% Verify that the correct OI callbacks are executed.
%% <br/><br/>
%% @spec oi_init_reboot_set_impl_again(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
oi_init_reboot_set_impl_again(_Config ) ->
    print_pids(),
    %%%%
    %% Add MO inst
    %%%%
    ok = add_testclass1_mo_inst(),
    ok = get_testclass1_mo_inst(),
    %%%%
    %% Create Object Implementer.
    %%%%
    CbFun = get_cb_fun(),
    ct:pal("### CbFun: ~p",[CbFun]),
    create_obj_impl(CbFun, ?OBJ_IMPL_NAME_1, ?CONF_OBJ_1, ?SAFE_IMM_ONE),
    %%%%
    %% Check OI exist.
    %%%%
    ok = check_oi_impl_name_1_exist(),

    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),

    %%%%
    %% init:reboot node
    %%%%
    ok = init_reboot(),

    oi_set_impl_again_and_configure(CbFun),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Object Implementer, Set implementer again. Do not set Object implementer again.
%% - Add attributes.
%% - Delete MO instans.
%% Verify that the correct OI callbacks are executed.
%% <br/><br/>
%% @spec oi_set_impl_again_and_configure(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
oi_set_impl_again_and_configure(CbFun)->
    print_pids(),

    ct:pal("Initialize OI Handle", []),
    {ok, Handle, Vsn} = rct_safe_imm_oi_rpc:initialize_2(CbFun),
    ct:pal("safe Handle = ~p, Vsn = ~p", [Handle, Vsn]),
    try
        %%%%
	%% Set implementer again. Do not set object implementer.
	%%%%
	ok = rct_safe_imm_oi_rpc:implementer_set(Handle,
						 ?OBJ_IMPL_NAME_1),
	ct:log("SAFE_IMM_ONE: ~p",[?SAFE_IMM_ONE]),
        %%%%
	%% Modify MO instance, add atrributes.
	%%%%
	ok = add_one_inst_attr(),

	ok = expect_callback(?SAFE_CCB_CB_MOD),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	ok = expect_callback(?SAFE_CCB_CB_APP),

    	%%%%
	%% Delete MO instance.
	%%%%
	ok = del_testclass1_mo_inst(),
	{error, _} = get_testclass1_mo_inst_no_check(),
	ok = expect_callback(?SAFE_CCB_CB_DEL),
	ok = expect_callback(?SAFE_CCB_CB_COMP),
	ok = expect_callback(?SAFE_CCB_CB_APP),

	%%%% object implmenter is released when MO inst is deleted.
	%% rct_safe_imm_oi_rpc:object_implementer_release(Handle,
	%% 					       CONF_OBJ_1,
	%% 					       SAFE_IMM),
	ok = rct_safe_imm_oi_rpc:implementer_clear(Handle),
	ok = rct_safe_imm_oi_rpc:finalize(Handle)
    catch
	_:Reason ->
	    ct:pal("TC fail, Reason: ~p", [Reason]),
	    cleanup_obj_impl(?CONF_OBJ_1, ?SAFE_IMM_ONE, Handle),
	    ct:fail(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Several Object Implementer. After Object implementer is set and application restart,
%% then it is enough to set implementer again to be able to receive callbacks.
%% Verify that the correct OI callbacks are executed.
%% <br/><br/>
%% @spec several_oi_kill_appl_set_impl_again(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
several_oi_kill_appl_set_impl_again(_Config ) ->
    {_ComPid, _BeamPid, _TestNodeBeamPid, TestOiPid} = print_pids(),
    %%%%
    %% Add MO inst
    %%%%
    NrList = lists:seq(1,?NR_OF_OI),
    ok = nc_open_session([?NC_SESSION_1]),
    lists:foreach(fun(Nr) ->
			  NR = integer_to_list(Nr),
			  InstName = "nc_"++NR,
			  ok = rct_nc_testclass1_lib:nc_add_mo_instance(?NC_SESSION_1,
									InstName)
		  end, NrList),
    ok = nc_close_session([?NC_SESSION_1]),
    %%%%
    %% Create Object Implementer.
    %%%%
    CbFun = get_cb_fun(),
    ct:pal("### CbFun: ~p",[CbFun]),
    HandleList =
	lists:map(fun(Nr1) ->
			      NR1 = integer_to_list(Nr1),
			      create_obj_impl(CbFun,
					      ?OBJ_IMPL_NAME_1++"_"++NR1,
					      "testClass1Id=nc_"++NR1++",TESTMOMtestRootId=1",
					      ?SAFE_IMM_ONE)
		      end, NrList),
    ZipList = lists:zip(HandleList, NrList),
    %%%%
    %% Check OI exist.
    %%%%
    ok = check_several_oi_impl_name_1_exist(NrList),
    try
        %%%%
	%% kill testnode
        %%%%
	ok = kill_test_oi(TestOiPid)
    catch
	_:Reason ->
	    ct:pal("TC fail, Reason: ~p", [Reason]),
	    lists:foreach(fun(Handle3, Nr3) ->
				  NR3 = integer_to_list(Nr3),
				  CONF_OBJ = "testClass1Id=nc_"++NR3++",TESTMOMtestRootId=1",
				  cleanup_obj_impl(CONF_OBJ, ?SAFE_IMM_ONE, Handle3)
			  end, ZipList),
	    ct:fail(Reason)
    end,

    several_oi_set_impl_again_and_configure(CbFun, NrList),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Several Object Implementer. After Object implementer is set and init restart,
%% then it is enough to set implementer again to be able to receive callbacks.
%% Verify that the correct OI callbacks are executed.
%% <br/><br/>
%% @spec several_oi_init_restart_set_impl_again(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
several_oi_init_restart_set_impl_again(_Config ) ->
    print_pids(),
    %%%%
    %% Add MO inst
    %%%%
    NrList = lists:seq(1,?NR_OF_OI),
    ok = nc_open_session([?NC_SESSION_1]),
    lists:foreach(fun(Nr) ->
			  NR = integer_to_list(Nr),
			  InstName = "nc_"++NR,
			  ok = rct_nc_testclass1_lib:nc_add_mo_instance(?NC_SESSION_1,
									InstName)
		  end, NrList),
    ok = nc_close_session([?NC_SESSION_1]),
    %%%%
    %% Create Object Implementer.
    %%%%
    CbFun = get_cb_fun(),
    ct:pal("### CbFun: ~p",[CbFun]),
    HandleList =
	lists:map(fun(Nr1) ->
			      NR1 = integer_to_list(Nr1),
			      create_obj_impl(CbFun,
					      ?OBJ_IMPL_NAME_1++"_"++NR1,
					      "testClass1Id=nc_"++NR1++",TESTMOMtestRootId=1",
					      ?SAFE_IMM_ONE)
		      end, NrList),
    ZipList = lists:zip(HandleList, NrList),
    %%%%
    %% Check OI exist.
    %%%%
    ok = check_several_oi_impl_name_1_exist(NrList),
    try
	ct:pal("sleep 35 sec"),
	ct:sleep({seconds, 35}),

        %%%%
	%% init:restart
        %%%%
	ok = init_restart()
    catch
	_:Reason ->
	    ct:pal("TC fail, Reason: ~p", [Reason]),
	    lists:foreach(fun(Handle3, Nr3) ->
				  NR3 = integer_to_list(Nr3),
				  CONF_OBJ = "testClass1Id=nc_"++NR3++",TESTMOMtestRootId=1",
				  cleanup_obj_impl(CONF_OBJ, ?SAFE_IMM_ONE, Handle3)
			  end, ZipList),
	    ct:fail(Reason)
    end,

    several_oi_set_impl_again_and_configure(CbFun, NrList),
    ok.
%%--------------------------------------------------------------------
%% @doc
%% Several Object Implementer. After Object implementer is set and init reboot,
%% then it is enough to set implementer again to be able to receive callbacks.
%% Verify that the correct OI callbacks are executed.
%% <br/><br/>
%% @spec several_oi_init_reboot_set_impl_again(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
several_oi_init_reboot_set_impl_again(_Config ) ->
    print_pids(),
    %%%%
    %% Add MO inst
    %%%%
    NrList = lists:seq(1,?NR_OF_OI),
    ok = nc_open_session([?NC_SESSION_1]),
    lists:foreach(fun(Nr) ->
			  NR = integer_to_list(Nr),
			  InstName = "nc_"++NR,
			  ok = rct_nc_testclass1_lib:nc_add_mo_instance(?NC_SESSION_1,
									InstName)
		  end, NrList),
    ok = nc_close_session([?NC_SESSION_1]),
    %%%%
    %% Create Object Implementer.
    %%%%
    CbFun = get_cb_fun(),
    ct:pal("### CbFun: ~p",[CbFun]),
    HandleList =
	lists:map(fun(Nr1) ->
			      NR1 = integer_to_list(Nr1),
			      create_obj_impl(CbFun,
					      ?OBJ_IMPL_NAME_1++"_"++NR1,
					      "testClass1Id=nc_"++NR1++",TESTMOMtestRootId=1",
					      ?SAFE_IMM_ONE)
		      end, NrList),
    ZipList = lists:zip(HandleList, NrList),
    %%%%
    %% Check OI exist.
    %%%%
    ok = check_several_oi_impl_name_1_exist(NrList),
    try
	ct:pal("sleep 35 sec"),
	ct:sleep({seconds, 35}),

        %%%%
	%% init:reboot
        %%%%
	ok = init_reboot()
    catch
	_:Reason ->
	    ct:pal("TC fail, Reason: ~p", [Reason]),
	    lists:foreach(fun(Handle3, Nr3) ->
				  NR3 = integer_to_list(Nr3),
				  CONF_OBJ = "testClass1Id=nc_"++NR3++",TESTMOMtestRootId=1",
				  cleanup_obj_impl(CONF_OBJ, ?SAFE_IMM_ONE, Handle3)
			  end, ZipList),
	    ct:fail(Reason)
    end,

    several_oi_set_impl_again_and_configure(CbFun, NrList),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Several Object Implementer, Set implementer again. Do not set Object implementer again.
%% - Delete existing MO inst.
%% - Add attributes.
%% - Delete MO instans.
%% Verify that the correct OI callbacks are executed.
%% <br/><br/>
%% @spec several_oi_set_impl_again_and_configure(CbFun, NrList) -> ok
%% @end
%%--------------------------------------------------------------------
several_oi_set_impl_again_and_configure(CbFun, NrList)->
    print_pids(),
    %%%%
    %% Initialize.
    %%%%
    HandleList =
	lists:map(fun(_Nr1) ->
			  {ok, Handle, Vsn} = rct_safe_imm_oi_rpc:initialize_2(CbFun),
			  ct:log("safe Handle = ~p, Vsn = ~p", [Handle, Vsn]),
			  Handle
		  end, NrList),

    ZipList = lists:zip(HandleList, NrList),
    ct:log("ZipList: ~p",[ZipList]),
    try
        %%%%
	%% Set implementer again. Do not set object implementer.
	%%%%
	lists:foreach(fun({Handle, Nr}) ->
			      NR = integer_to_list(Nr),
			      ok = rct_safe_imm_oi_rpc:implementer_set(Handle,
								       ?OBJ_IMPL_NAME_1++"_"++NR)
		      end, ZipList),
	ct:log("SAFE_IMM_ONE: ~p",[?SAFE_IMM_ONE]),
        %%%%
	%% Modify MO instance, add atrributes.
	%%%%
	ok = nc_open_session([?NC_SESSION_1]),
	lists:foreach(fun(Nr1) ->
			      NR1 = integer_to_list(Nr1),
			      InstName = "nc_"++NR1,
			      ok = nc_add_one_attr(?NC_SESSION_1,
						   InstName,
						   ?ATTR_NAME_1)
			      end, NrList),
	ok = nc_close_session([?NC_SESSION_1]),

	lists:foreach(fun(_A) ->
			      ok = expect_callback(?SAFE_CCB_CB_MOD)
		      end, NrList),
	lists:foreach(fun(_AAA) ->
			      ok = expect_callback(?SAFE_CCB_CB_COMP)
		      end, NrList),
	lists:foreach(fun(_AAA) ->
			      ok = expect_callback(?SAFE_CCB_CB_APP)
		      end, NrList),

    	%%%%
	%% Delete MO instance.
	%%%%
	ok = nc_open_session([?NC_SESSION_1]),
	lists:foreach(fun(Nr2) ->
			      NR2 = integer_to_list(Nr2),
			      InstName1 = "nc_"++NR2,
			      ok = rct_nc_testclass1_lib:nc_delete_mo_instance(?NC_SESSION_1,
									       InstName1)
		      end, NrList),
	ok = nc_close_session([?NC_SESSION_1]),

	lists:foreach(fun(_Nr3) ->
			      ok = expect_callback(?SAFE_CCB_CB_DEL)
		      end, NrList),
	lists:foreach(fun(_Nr4) ->
			      ok = expect_callback(?SAFE_CCB_CB_COMP)
		      end, NrList),
	lists:foreach(fun(_Nr5) ->
		      ok = expect_callback(?SAFE_CCB_CB_APP)
	      end, NrList),

	lists:foreach(fun(Handle2) ->
			      ok = rct_safe_imm_oi_rpc:implementer_clear(Handle2),
			      ok = rct_safe_imm_oi_rpc:finalize(Handle2)
		      end, HandleList)
    catch
	_:Reason ->
	    ct:pal("TC fail, Reason: ~p", [Reason]),
	    lists:foreach(fun(Handle3, Nr3) ->
				  NR3 = integer_to_list(Nr3),
				  CONF_OBJ = "testClass1Id=nc_"++NR3++",TESTMOMtestRootId=1",
				  cleanup_obj_impl(CONF_OBJ, ?SAFE_IMM_ONE, Handle3)
			  end, ZipList),
	    ct:fail(Reason)
    end.



%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
get_cb_fun() ->
    cb_fun(ok).

%% get_cb_fun(SafeErr) ->
%%     cb_fun({error, SafeErr}).

cb_fun(Res) ->
    Self = self(),
    fun(Arg) ->
	    ct:pal("Executing ~p.", [element(1,Arg)]),
	    Self ! {oi_ccb_callback, Arg},
	    ct:pal("Return result: ~p.", [Res]),
	    Res
    end.

get_cb_fun_delay_on_complete() ->
    cb_fun_delay_on_complete(ok).

cb_fun_delay_on_complete(Res) ->
    Self = self(),
    fun(Arg) ->
	    case element(1,Arg) of
		safe_imm_oi_ccb_completed_callback ->
		    ct:pal("Executing ~p.", [element(1,Arg)]),
		    timer:sleep(10000),
		    %% timer:sleep(20000),
		    Self ! {oi_ccb_callback, Arg},
		    ct:pal("Return result: ~p.", [Res]),
		    Res;
		_  ->
		    ct:pal("Executing ~p.", [element(1,Arg)]),
		    Self ! {oi_ccb_callback, Arg},
		    ct:pal("Return result: ~p.", [Res]),
		    Res
	    end
    end.


expect_callback(ExpectedCb) ->
    case receive_callback_arg() of
	{ok, Arg} ->
	    expect_callback(ExpectedCb, element(1, Arg));
	Error ->
	    Error
    end.


expect_callback(ExpectedCb,  ExpectedCb) ->
    ok;

expect_callback(ExpectedCb, OtherCb) ->
    Reason = lists:flatten(io_lib:format("Expected Callback ~p, Got ~p",
					 [ExpectedCb, OtherCb])),
    ct:fail(Reason).


receive_callback_arg() ->
    receive
	{oi_ccb_callback, Arg} ->
	    ct:pal("Received ~p Args:~n~p", [element(1,Arg),Arg]),
	    {ok, Arg}
    after 5000 ->
	    ct:pal("Failed to receive CB Args"),
	    {error, "Failed to receive CB Args"}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_class_impl(CbFun, IMPL_NAME, IMM_TEST_CLASS) ->
    ct:pal("Initialize OI Handle", []),
    {ok, Handle, Vsn} = rct_safe_imm_oi_rpc:initialize_2(CbFun),
    ct:pal("Initialize ok: ~p ~p",[Handle, Vsn]),
    ct:pal("Set Implementer name: ~p", [IMPL_NAME]),
    ok = rct_safe_imm_oi_rpc:implementer_set(Handle, IMPL_NAME),
    ct:pal("Set Class Implementer for: ~p", [IMM_TEST_CLASS]),
    ok = rct_safe_imm_oi_rpc:class_implementer_set(Handle, IMM_TEST_CLASS),
    Handle.

delete_class_impl(IMM_TEST_CLASS, IMPL_NAME, Handle) ->
			  ct:pal("Release Class Implementer for: ~p", [IMM_TEST_CLASS]),
			  ok = rct_safe_imm_oi_rpc:class_implementer_release(Handle, IMM_TEST_CLASS),
			  ct:pal("Clear Implementer ~p", [IMPL_NAME]),
			  ok = rct_safe_imm_oi_rpc:implementer_clear(Handle),
			  ct:pal("Finalize OI Handle ~p", [Handle]),
			  ok = rct_safe_imm_oi_rpc:finalize(Handle).

cleanup_class_impl(IMM_TEST_CLASS, IMPL_NAME, Handle) ->
			  ct:pal("Release Class Implementer for: ~p", [IMM_TEST_CLASS]),
			  rct_safe_imm_oi_rpc:class_implementer_release(Handle, IMM_TEST_CLASS),
			  ct:pal("Clear Implementer ~p", [IMPL_NAME]),
			  rct_safe_imm_oi_rpc:implementer_clear(Handle),
			  ct:pal("Finalize OI Handle ~p", [Handle]),
			  rct_safe_imm_oi_rpc:finalize(Handle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_obj_impl(CbFun, OBJ_IMPL_NAME_1, CONF_OBJ_1, SAFE_IMM) ->
    CbFun = get_cb_fun(),
    ct:pal("Initialize OI Handle", []),
    {ok, Handle, Vsn} = rct_safe_imm_oi_rpc:initialize_2(CbFun),
    ct:pal("safe Handle = ~p, Vsn = ~p", [Handle, Vsn]),
    ok = rct_safe_imm_oi_rpc:implementer_set(Handle,
					     OBJ_IMPL_NAME_1),
    ok = rct_safe_imm_oi_rpc:object_implementer_set(Handle,
						    CONF_OBJ_1,
						    SAFE_IMM),
    Handle.

cleanup_obj_impl(CONF_OBJ_1, SAFE_IMM, Handle) ->
    rct_safe_imm_oi_rpc:object_implementer_release(Handle,
						   CONF_OBJ_1,
						   SAFE_IMM),
    rct_safe_imm_oi_rpc:implementer_clear(Handle),
    rct_safe_imm_oi_rpc:finalize(Handle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_ci_impl_name_1_exist() ->
    case rct_rpc:call(rpc, safs_imm_db, ci_get, [?TESTMOM_NAME], 10000,
		      noprint) of
    	{_,<<?IMPL_NAME_1>>} ->
    	    ok;
    	{_,<<?IMPL_NAME_1>>, []} ->
    	    ok;
    	Ans ->
    	    ct:pal("~p", [Ans]),
    	    ct:fail(" Expecting OI does not exist! ")
    end,
    ok.

check_oi_impl_name_1_exist() ->
    case rct_rpc:call(rpc, safs_imm_db, oi_get, [?CONF_OBJ_1], 10000,
		      noprint) of
	{_,<<?OBJ_IMPL_NAME_1>>} ->
	    ok;
	{_,<<?OBJ_IMPL_NAME_1>>, []} ->
	    ok;
	Ans ->
	    ct:pal("~p", [Ans]),
	    ct:fail(" Expecting OI does not exist! ")
    end,
    ok.

check_several_oi_impl_name_1_exist(NrList) ->
    lists:foreach(
      fun(Nr2) ->
	      NR2 = integer_to_list(Nr2),
	      ExpObjName = "nisse_"++NR2,
	      case rct_rpc:call(rpc, safs_imm_db, oi_get,
				["testClass1Id=nc_"++NR2++","
				 "TESTMOMtestRootId=1"],
				10000, noprint) of
		  {_,ResExpObjName} ->
		      check_oi(ResExpObjName, ExpObjName);
		  {_,ResExpObjName, []} ->
		      check_oi(ResExpObjName, ExpObjName);
		  Ans ->
		      ct:pal("~p", [Ans]),
		      ct:fail(" Expecting OI does not exist! ")
	      end
      end,NrList),
    ok.

check_oi(ResExpObjName, ExpObjName)->
    case re:run(ResExpObjName, ExpObjName) of
	{match, _} ->
	    ct:log("received ExpObjName: ~p", [ExpObjName]),
	    ok;
	nomatch ->
	    ct:fail(" EEEExpecting OI does not exist! ")
    end.

%%--------------------------------------------------------------------
%% @doc
%% Print some useful process pids. <br/>
%% @end
%%--------------------------------------------------------------------
print_pids()->
    ComPid= rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 10000, noprint),
    BeamPid = rct_rpc:call(rpc, os, getpid, [], 10000),
    {ok, TestNode} = rct_safe_imm_oi_rpc:get_testnode(rct_safe_imm_oi_rpc),
    TestNodeBeamPid = rpc:call(TestNode, os, getpid, [], 10000),
    Test_OI_Pid = rct_rpc:call(rpc, os, cmd, ["pgrep test_oi -u $USER"], 10000, noprint),
    ct:log("### Com: ~p",[ComPid]),
    ct:log("### BeamPid: ~p .",[BeamPid]),
    ct:log("### TestNodeBeamPid: ~p .",[TestNodeBeamPid]),
    ct:log("### Test_OI_Pid: ~p .",[Test_OI_Pid]),
    {ComPid, BeamPid,TestNodeBeamPid, Test_OI_Pid}.

%%--------------------------------------------------------------------
%% @doc
%% Open netconf sessions. <br/>
%% @end
%%--------------------------------------------------------------------
%% nc_open_session(NC_SessionNameList) ->
%%     lists:foreach(fun(Name) ->
%% 			  ct:pal("### Open Name: ~p", [Name]),
%%     			  {ok,_} = ct_netconfc:open(Name, [{timeout,30000}])
%%     		  end,
%% 		  NC_SessionNameList),
%%     ok.

nc_open_session(NC_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  ct:log("### Open Name: ~p", [Name]),
			  ok = wait_for_netconf_started(Name)
    		  end,
		  NC_SessionNameList),
    ok.

wait_for_netconf_started(Name) ->
    wait_for_netconf_started(Name, 60000).

wait_for_netconf_started(_Name, Timeout) when Timeout < 500 ->
    ct:fail("Netconf session not open within 60sec!!.");

wait_for_netconf_started(Name, Timeout) ->
    case ct_netconfc:open(Name, [{timeout,2000}]) of
	{ok,_} ->
	    ct:log("### NC session is opened: ~p", [Name]),
	    ok;
	_  ->
	    timer:sleep(2000),
	    wait_for_netconf_started(Name, Timeout - 2000)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Close netconf sessions. <br/>
%% @end
%%--------------------------------------------------------------------
nc_close_session(NC_SessionNameList) ->
    lists:foreach(fun(Name) ->
			  ct:log("### Close Name: ~p", [Name]),
			  ok = ct_netconfc:close_session(Name, 30000)
    		  end,
    		  NC_SessionNameList),
    ok.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
wait_for_testnode(NodeState)->
    wait_for_testnode(NodeState, rct_safe_imm_oi_rpc).
wait_for_testnode(NodeState, InstanceName) ->
    receive
	{InstanceName, {NodeName, NodeState}} ->
	    ct:pal("NodeName: ~p , NodeState: ~p .",[NodeName, NodeState]),
	    ok
    after 180000 ->
	    ct:fail({"Expected NodeState not rceived",NodeState})
    end.

%%%%%%%%%%
%% Netconf
%%%%%%%%%%
add_testclass1_mo_inst() ->
    ok = nc_open_session([?NC_SESSION_1]),
    ok = rct_nc_testclass1_lib:nc_add_mo_instance(?NC_SESSION_1,
						  ?INST_NAME_1),
    ok = nc_close_session([?NC_SESSION_1]),
    ok.

%% add_testclass1_mo_inst_attr() ->
%%     ok = nc_open_session([?NC_SESSION_1]),
%%     ok = rct_nc_testclass1_lib:nc_add_mo_instance_and_attr(?NC_SESSION_1,
%% 							   ?INST_NAME_1,
%% 							   ?ATTR_NAME_1),
%%     ok = nc_close_session([?NC_SESSION_1]),
%%     ok.

add_one_inst_attr() ->
    ok = nc_open_session([?NC_SESSION_1]),
    ok = nc_add_one_attr(?NC_SESSION_1, ?INST_NAME_1, ?ATTR_NAME_1),
    ok = nc_close_session([?NC_SESSION_1]),
    ok.

del_testclass1_mo_inst() ->
    ok = nc_open_session([?NC_SESSION_1]),
    ok = rct_nc_testclass1_lib:nc_delete_mo_instance(?NC_SESSION_1,
						     ?INST_NAME_1),
    ok = nc_close_session([?NC_SESSION_1]),
    ok.

get_testclass1_mo_inst() ->
    ok = nc_open_session([?NC_SESSION_1]),
    ok = rct_nc_testclass1_lib:nc_get_mo_instance_check(?NC_SESSION_1,
							?INST_NAME_1),
    ok = nc_close_session([?NC_SESSION_1]),
    ok.

get_testclass1_mo_inst_no_check() ->
    ok = nc_open_session([?NC_SESSION_1]),
    RES = rct_nc_testclass1_lib:nc_get_mo_instance_no_check(?NC_SESSION_1,
						      ?INST_NAME_1),
    ok = nc_close_session([?NC_SESSION_1]),
    RES.

%% get_testclass1_mo_inst_attr() ->
%%     ok = nc_open_session([?NC_SESSION_1]),
%%     ok = rct_nc_testclass1_lib:nc_get_attribute_check(?NC_SESSION_1,
%% 						      ?INST_NAME_1,
%% 						      ?ATTR_NAME_1),
%%     ok = nc_close_session([?NC_SESSION_1]),
%%     ok.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
kill_test_oi(TestOiPid)->
    ct:pal("### Kill test_oi : ~p !",[TestOiPid]),
    rct_safe_imm_oi_rpc:subscribe_testnode(rct_safe_imm_oi_rpc),
    rct_rpc:call(rpc, os, cmd, ["pkill test_oi"], 500),
    ok = wait_for_testnode(node_down),
    ok = wait_for_testnode(node_up),
    ok.

init_restart() ->
    ct:pal("### init:restart!",[]),
    {ok, TestNode} = rct_safe_imm_oi_rpc:get_testnode(rct_safe_imm_oi_rpc),
    rct_safe_imm_oi_rpc:subscribe_testnode(rct_safe_imm_oi_rpc),
    net_kernel:disconnect(TestNode),
    %% rct_safe_imm_oi_rpc:subscribe_testnode(rct_safe_imm_oi_rpc),
    rct_rpc:call(rpc, init, restart, [], 500),
    ok = wait_for_testnode(node_down),
    ok = wait_for_testnode(node_up),
    ok.

init_reboot() ->
    ct:pal("### init:reboot!",[]),
    {ok, TestNode} = rct_safe_imm_oi_rpc:get_testnode(rct_safe_imm_oi_rpc),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    rct_safe_imm_oi_rpc:subscribe_testnode(rct_safe_imm_oi_rpc),
    net_kernel:disconnect(TestNode),
    net_kernel:disconnect(ErlNode),
    do_init_reboot(),
    {ok,_} = ct_telnet:expect(console, "Restarting system",
			      [{timeout,60000}, no_prompt_check]),
    {ok,_} = ct_telnet:expect(console, "login:", [{timeout,90000},
						  no_prompt_check]),
    net_kernel:connect(ErlNode),
    check_testnode_alive(TestNode), %% Make it more robust!
    %% ct:sleep({seconds, 20}),
    ok = wait_for_testnode(node_up),
    net_kernel:connect(TestNode),
    ok.

%% ===========================================================================
%% @doc
%% wait_for_appl_started. <br/>
%% @end
%% ===========================================================================
check_testnode_alive(TestNode) ->
    check_testnode_alive(TestNode, 180000).

check_testnode_alive(TestNode, Timeout) when Timeout < 500 ->
    ct:pal("Testnode is not alive within max timeout: ~p ",[TestNode]),
    ct:fail("Testnode not started within max timeout after restart.");

check_testnode_alive(TestNode, Timeout) ->
    case net_adm:ping(TestNode) of
    	pang ->
	    ct:pal("Testnode is not alive: ~p, wait and try agin. ",[TestNode]),
	    timer:sleep(5000),
	    check_testnode_alive(TestNode, Timeout - 5000);
	pong ->
	    ct:pal("Testnode is alive: ~p ",[TestNode])
    end.

%%--------------------------------------------------------------------
%% @doc
%% do_init_reboot. <br/>
%% @end
%%--------------------------------------------------------------------
do_init_reboot() ->
    do_init_reboot(3).
do_init_reboot(0) ->
    ct:fail("Node has not rebooted, tried 3 times!!");
do_init_reboot(Nr) ->
    case rct_rpc:call(rpc, init, reboot, [], 2000) of
	ok ->
	    ok;
	{badrpc,nodedown} ->
	    ct:pal("init:reboot() did not take effect! try again",[]),
	    timer:sleep(2000),
	    do_init_reboot(Nr-1)
    end.

%%--------------------------------------------------------------------
%% @doc
%% nc_add_one_attr. <br/>
%% @end
%%--------------------------------------------------------------------
nc_add_one_attr(NC_session, InstName, AttrName) ->
    ct:log("### Add one attr",[]),
    ct_netconfc:edit_config(NC_session,
			    running,{'ManagedElement',
				     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				     [{managedElementId,[],["1"]},
				      {'TestRoot',
				       [{xmlns,
					 "urn:com:ericsson:ecim:TESTMOM"}],
				       [{testRootId,[],["1"]},
					{'TestClass1',
					 [],
					 [{'testClass1Id',[],[InstName]},
					  {int32,[],[AttrName]}
					 ]}]}]}, 30000),
    ok.
