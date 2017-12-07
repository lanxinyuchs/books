%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	log_alarm_SUITE.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R10A/R11A/1
%%% 
%%% @doc == Test Suite for testing application logs.==
%%% This Test Suite can be used on both target and simulated enviroment
%%% and both single and multi nodes.
%%% <br/><br/>
%%% @end


-module(log_alarm_SUITE).

%%% ----------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R2A/1      2013-10-07 uabesvi     Created
%%% R3A/5      2015-02-28 etxkols     Preparation for cluster
%%% R3A/6      2015-02-28 etxkols     Preparation for 2 labs
%%% R3A/7      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R4A/1      2016-10-17 egjknpa     add FAKE_VNFM for vrcs 
%%% ----------------------------------------------------------
%%% 

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 groups/0]).

%%-export([internal_log_full/1]).
-export([external_log_full/1]).

-include_lib("safe/include/safe.hrl").
-include_lib("safe/include/safe_log.hrl").

-define(LEN, 100).

%% -define(SFTP_HOST, "10.68.200.11").
%% -define(USER, "mauve").
%% -define(PSWD, "dilbert").
%% -define(SFTP_URL, "sftp://"++?USER++"@"++?SFTP_HOST).

-define(SNMP_MGR_NAME, snmp1).
-define(NETCONF_SESSION, nc1).


-define(WAIT_TRAP_OPTS, [{allowed_traps, 
			  [[{type, eriAlarmMinor}],
			   [{type, eriAlarmCritical}],
			   [{type, eriAlarmHeartBeatNotif}],
			   [{type, eriAlarmAlarmListRebuilt}]]}]).
-define(TRAP_TIMEOUT, 200).

-define(LDN_FAKE,      "ManagedElement=1,SystemFunctions=1,LogM=1,Log=FakeLog").
-define(LDN_FAKE_HALT, "ManagedElement=1,SystemFunctions=1,LogM=1,Log=FakeHaltLog").
-define(ALARM_SP, "Log Has Reached Full Capacity").
%%-define(ALARM_SP, "safLgStr=FakeHaltLog,safApp=safLogService has reached full capatcity").

-define(EXPECT_FAKE_RAISE, 
	[[{type, eriAlarmWarning},
	  {eriAlarmActiveSpecificProblem, ?ALARM_SP},
	  {eriAlarmActiveManagedObject, ?LDN_FAKE}]]).

-define(EXPECT_FAKE_CLEAR, 
	[[{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, ?ALARM_SP},
	  {eriAlarmActiveManagedObject, ?LDN_FAKE}]]).

-define(EXPECT_FAKE_HALT_RAISE, 
	[[{type, eriAlarmWarning},
	  {eriAlarmActiveSpecificProblem, ?ALARM_SP},
	  {eriAlarmActiveManagedObject, ?LDN_FAKE_HALT}]]).

-define(EXPECT_FAKE_HALT_CLEAR, 
	[[{type, eriAlarmCleared},
	  {eriAlarmActiveSpecificProblem, ?ALARM_SP},
	  {eriAlarmActiveManagedObject, ?LDN_FAKE_HALT}]]).

-define(CLI_USER, cli_user).
-define(PRINT_OPT, print).

-define(INV1, 123).
-define(INV2, 456).
-define(INV3, 789).
-define(INV4, 100).

-define(TESTNODE, testnode).
-define(APP_LOGS_DIR, "saf_log").
-define(SA_TIME_UNKNOWN, 16#8000000000000000).
-define(VERSION, #safe_version{release_code  = $A,
			       major_version = 2,
			       minor_version = 1}).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks,
      [
       {rct_cli, {?CLI_USER, [manual_connect]}},
       {rct_snmpmgr, ?SNMP_MGR_NAME},
       {rct_safe_log_rpc, [{safe_debug_level, 2}]},
       %%{rct_safe_rpc, [{safe_services, [{ntf, 2}, {imm, 2}, {log, 2}]}]},
       {rct_htmllink,[]},
       {rct_rpc, rpc_1},
       {rct_netconf,{nc1, html}},
       {cth_conn_log,[]},
       {rct_logging, {oi_testapp, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}}
%%       {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}}
      ]}].


%% @hidden
init_per_suite(Config) ->
    crypto:start(),
    ssh:start(),
    MeData = rct_rpc:call(rpc_1, comsaI, get_managed_element_data, [], 10000),
    MeId = 
	case proplists:get_value(networkManagedElementId, MeData) of
	    undefined ->
		"1";
	    NMEI -> NMEI
	end,
    
    %% reset severity filters
    ok = set_severity_filter("FakeLog", ["EMERGENCY", 
					 "ALERT",
					 "CRITICAL",
					 "ERROR",
					 "WARNING",
					 "NOTICE",
					 "INFO"
					]),
    %%sys:trace(rct_safe_log_rpc, true),
    ok = trap_receiver_create(?SNMP_MGR_NAME),
    %% fake VNFM for vrcs
    case rct_rpc:call(rpc_1, sysEnv, rcs_mode_2, [], 10000) of
        vrcs ->
            rct_rpc:call(rpc_1, os, putenv, ["FAKE_VNFM", ""], 10000);
        _ ->
            rct_rpc:call(rpc_1, os, unsetenv, ["FAKE_VNFM"], 10000)
    end,
    [{meId, MeId}|Config].
%% @hidden
end_per_suite(_Config) ->
    %% fake VNFM for vrcs
    case rct_rpc:call(rpc_1, sysEnv, rcs_mode_2, [], 10000) of
        vrcs ->
            rct_rpc:call(rpc_1, os, unsetenv, ["FAKE_VNFM"], 10000);
        _ ->
            ok
    end,
    ok = trap_receiver_delete().
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
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []} 
    ].

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
     external_log_full
    ].





%%--------------------------------------------------------------------
%% 
%% write to fake log until it is full
%% 
%%--------------------------------------------------------------------
%% internal_log_full(_) ->
%%     SN = "safLgStr=internal_log",
%%     {ok, H, _} = initialize(get_cb_fun(), ?VERSION),

%%     {ok, SH} = stream_open_2(H, SN, ca(SN)),

%%     w(5, SH),
%%     ok = stream_close(SH),
%%     ok = finalize(H).


%% w(A, _) when A < 0 ->
%%     ct:pal("##### stop ~p ~n",[A]),
%%     ok;
%% w(A, SH) ->
%%     ct:pal("##### ~p ~n",[A]),
%%     L = integer_to_list(A) ++ " abcdefghijk",
%%     ok = write_log_async(SH, L, error, ?INV1),
%% %%    ok = expected_cb({write_log, {?INV1, ok}, undefined}),
%%     w(A - 1, SH).
    
    
%%--------------------------------------------------------------------
%% @doc
%% write to external halt fake log until it is full
%% @end
%%--------------------------------------------------------------------
external_log_full(Config) ->

    %%-------------------------------------------------------------
    %% export file to clean up the file
    %%-------------------------------------------------------------
    ok = rct_cli:connect(?CLI_USER),
    rct_cli:send(?CLI_USER, ?LDN_FAKE_HALT, ?PRINT_OPT),

    PrivDir = ?config(priv_dir, Config),
    os:cmd("chmod 777 " ++ PrivDir), % else permission.

    [{host, SftpHost},{username, Username},{password, PSWD}] = ct:get_config(sftp_server),
    SFTP_URL = "sftp://"++Username++"@"++SftpHost,
    Export = "export " ++ SFTP_URL ++ PrivDir ++ " " ++ PSWD,
    %% Export = "export " ++ ?SFTP_URL ++ PrivDir ++ " " ++ ?PSWD,
    rct_cli:send(?CLI_USER, Export, ?PRINT_OPT),

    ct:pal("cli send  Export = ~p~n", [Export]), 
    ct:pal("waiting for alarm trap~n"), 

    %%-------------------------------------------------------------
    %% Prepare tap receiver for alarm
    %%-------------------------------------------------------------
    HaltRaise = wait_for_traps(?EXPECT_FAKE_HALT_RAISE),
    ct:pal("wait_for_traps res = ~p~n", [HaltRaise]), 
    

    timer:sleep(5000),

    ct:pal("slept ~n"), 

    SN = "safLgStr=FakeHaltLog",
    {ok, H, _} = initialize(get_cb_fun(), ?VERSION),

    {ok, SH} = stream_open_2(H, SN, undefined, undefined),

    write(5250, SH),

    ok = stream_close(SH),
    ok = finalize(H),
    
    %%-------------------------------------------------------------
    %% Check that alarm is raised
    %%-------------------------------------------------------------
    ok = rct_snmpmgr:check_traps(),
    ct:pal("alarm trap OK. Clear the alarm by exporting the file.~n"), 

    %%-------------------------------------------------------------
    %% export file and check that alarm has been cleared
    %%-------------------------------------------------------------
    rct_cli:send(?CLI_USER, ?LDN_FAKE_HALT, ?PRINT_OPT),

    PrivDir = ?config(priv_dir, Config),
    os:cmd("chmod 777 " ++ PrivDir), % else permission.

    [{host, SftpHost},{username, Username},{password, PSWD}] = ct:get_config(sftp_server),
    SFTP_URL = "sftp://"++Username++"@"++SftpHost,
    Export = "export " ++ SFTP_URL ++ PrivDir ++ " " ++ PSWD,
    %% Export = "export " ++ ?SFTP_URL ++ PrivDir ++ " " ++ ?PSWD,
    rct_cli:send(?CLI_USER, Export, ?PRINT_OPT),

    %%-------------------------------------------------------------
    %% Prepare tap receiver for clear alarm
    %%-------------------------------------------------------------
    wait_for_traps(?EXPECT_FAKE_HALT_CLEAR).



write(A, _) when A < 0 ->
    ct:pal("##### stop ~p ~n",[A]),
    ok;
write(A, SH) ->
    %%ct:pal("##### ~p ~n",[A]),
    L = integer_to_list(A) ++ 
	" abcdefghijk afdasf  asf asdasf  fdasfdas sdaf asfd sffdsa"
	" abcdefghijk afdasf  asf asdasf  fdasfdas sdaf asfd sffdsa"
	" abcdefghijk afdasf  asf asdasf  fdasfdas sdaf asfd sffdsa"
	" abcdefghijk afdasf  asf asdasf  fdasfdas sdaf asfd sffdsa",
    ok = write_log_async(SH, L, error, ?INV1),
%%    ok = expected_cb({write_log, {?INV1, ok}, undefined}),
    write(A - 1, SH).
    

initialize(CB, Vsn) ->
    rct_safe_log_rpc:initialize(CB, Vsn).
    

%% stream_open_2(Handle) ->
%%     StreamName = "safLgStr=FakeLog,safApp=safLogService",    
%%     rct_safe_log_rpc:stream_open_2(Handle, StreamName, 0, 5000, undefined).

%% stream_open_2(Handle, StreamName, CA) ->
%%     ct:pal("##### stream open CA  ~p ~n",[CA]),
%% %%    StreamName = "safLgStr=FakeLog,safApp=safLogService",    
%%     rct_safe_log_rpc:stream_open_2(Handle, 
%% 				   StreamName, 
%% 				   ?SAFE_LOG_STREAM_CREATE, 
%% 				   5000, 
%% 				   CA).

stream_open_2(Handle, StreamName, CA, _OpenFlag) ->
    ct:pal("##### stream open CA  ~p ~n",[CA]),
%%    StreamName = "safLgStr=FakeLog,safApp=safLogService",    
    rct_safe_log_rpc:stream_open_2(Handle, 
				   StreamName, 
				   0, 
				   5000, 
				   CA).

write_log_async(SH, Data, Level, Inv) ->
	    GL = {safe_log_generic_log_header, undefined, "userName", l(Level)},
	    LR = {safe_log_record, 
		  ?SA_TIME_UNKNOWN, 
		  2, 
		  GL,
		  list_to_binary(Data)},
	    rct_safe_log_rpc:write_log_async(SH, Inv, 0, LR).


l(emergency) -> 0;
l(alert)     -> 1;
l(citical)   -> 2;
l(error)     -> 3;
l(warning)   -> 4;
l(notice)    -> 5;
l(info)      -> 6.

stream_close(SH) ->
    rct_safe_log_rpc:stream_close(SH).

finalize(LH) ->
    rct_safe_log_rpc:finalize(LH).


%%========================================================================
%% set_severity_filter(LogName)
%% 
%% 
%%========================================================================
set_severity_filter(LogName, NewFilter) ->
    Filter = [{severityFilter, [], [F]} || F <- NewFilter],
    Cmd = {'ManagedElement',          
	   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	   [{managedElementId,[],["1"]},
	    {'SystemFunctions',
	     [{systemFunctionsId,[],["1"]},
	      {'LogM',
	       [{xmlns,"urn:com:ericsson:ecim:LogM"}],
	       [{logMId,[],["1"]},
		{'Log', [{logId,[],[LogName]} | Filter]
		}]}
	     ]}]},
    
    {ok, _}   = ct_netconfc:open(nc1, []),
    ok = ct_netconfc:edit_config(nc1, running, Cmd),
    ct_netconfc:close_session(nc1),
    ok.



%%========================================================================
%% get_cb_fun() -> function()
%% 
%% Get a function used as callback fun in initialize/2
%%========================================================================
get_cb_fun() ->
    Self = self(),
    fun(Data) -> 
	    Self ! {log_cb, Data}, 
	    ok
    end.


%% ca(SN) ->
%%     #safe_log_file_create_attributes_2
%%       {log_file_name        = SN,
%%        max_log_file_size    = 200,
%%        max_log_record_size  = 100,
%%        ha_property          = ?SAFE_FALSE,
%%        max_files_rotated    = 0,
%%        log_file_full_action = ?SAFE_LOG_FILE_FULL_ACTION_HALT
%%       }.



%%-------------------------------------------------------------
%% Trap receiver create
%%-------------------------------------------------------------

trap_receiver_create(MgrName) ->
    Attrs = rct_snmpmgr:get_attrs_trap_receiver_create(MgrName),
    netconf_edit(Attrs).

%%-------------------------------------------------------------
%% Trap receiver delete
%%-------------------------------------------------------------
trap_receiver_delete() ->
    Attrs = rct_snmpmgr:get_attrs_trap_receiver_delete(),
    netconf_edit(Attrs).

%%-------------------------------------------------------------
%% Prepare the trap receiver for receiving expeted trap.
%%-------------------------------------------------------------
wait_for_traps(TrapsExpected) ->
    rct_snmpmgr:wait_for_traps(TrapsExpected, ?WAIT_TRAP_OPTS, ?TRAP_TIMEOUT).
    

%%-------------------------------------------------------------
%% NetConf edit trap receiver
%%-------------------------------------------------------------
netconf_edit(Attrs) ->
    try
	{ok,_} = ct_netconfc:open(?NETCONF_SESSION,[]),
	ct_netconfc:edit_config(?NETCONF_SESSION, running, Attrs),
	ok = ct_netconfc:close_session(?NETCONF_SESSION),
	timer:sleep(5000),
	ok
    catch
	_:Reason ->
	    ct_netconfc:close_session(?NETCONF_SESSION),
	    ct:fail(Reason)
    end.



%% expected_cb({Msg, timeout, Pid}) ->
%%     expected_cb({Msg, {0, timeout}, Pid});
%% expected_cb({Msg, {Inv, Res}, _Pid} = Exp) ->
%%     receive 
%% 	{log_cb, 
%% 	 {safe_log_write_log_callback, Inv, Res}} when Msg == write_log ->
%% 	    ok;
%% 	_Other ->
%% 	    ct:pal("Got unexpected msg: ~p", [_Other]),
%% 	    expected_cb(Exp)
%%     after 3000 ->
%% 	    case Res of
%% 		timeout ->
%% 		    ok;
%% 		_ ->
%% 		    ct:fail("### CB cc Timeout ~p~n", [Exp])
%% 	    end
%%     end.

