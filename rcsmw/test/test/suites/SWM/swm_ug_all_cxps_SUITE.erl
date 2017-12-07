%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_ug_all_cxps_SUITE.erl %
%%% @author eransbn
%%% @copyright Ericsson AB 2014-2016
%%% @version /main/R2A/R3A/R4A/6
%%%
%%% @doc == Test Suite for Upgrade Mechanism, create, prepare, verify, activate and confirm. To UP is build from pre dc and all cxps is changed. To make dure disc space is enough. ==
%%% <br/><br/>
%%% @end

-module(swm_ug_all_cxps_SUITE).
-vsn('/main/R2A/R3A/R4A/6').

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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/2      2014-03-10 etxivri     Created
%%% R2A/3      2014-10-13 etxivri     Update path so tc can be runed by jenkins.
%%% R2A/4      2014-10-13 etxivri     Added more debug printouts.
%%% R2A/5      2014-10-14 etxivri     Changed path to get DC CXP
%%% R2A/6      2014-12-03 eransbn     Updated for VC card
%%% R3A/1      2014-12-03 etxivri     Tmp update to perform only one UG.
%%%                                   Two UGs will results in failure,
%%%                                   due to second rebuild of EE cxp is not
%%%                                   possible by Jockes script.
%%% R3A/2      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/3      2015-05-29 etxkols     Changed rct_netconf hook format 
%%% R4A/1      2015-07-16 etxivri     Cleanup log in mot_to_up.
%%% R4A/2      2015-10-06 etxivri     Remove files after mod_up due to not 
%%%                                   fill disc on /proj/rcs-tmp/
%%% R4A/3      2015-11-03 etxivri     Update tc to fail if mod up fails.
%%% R4A/4      2016-02-04 eransbn     Change working dir to have enough disk space (mod up) .
%%% ----------------------------------------------------------

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0]).
-export([create/1,
	 prepare/1,
	 verify/1,
	 activate/1,
	 confirm/1,
	 remove/1,
	 mod_to_up/1]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).
-define(NC_Session, nc1).
-define(CLI_Session, cli1).

-define(MOD_TO_UP, "$RDE_TOP/tools/jenkins/rcs_build_valid_to_up.sh -S ").
%% -define(TO_UP, "/tmp/etxivri/DC-CXP*.tgz ").
-define(TO_UP, "/proj/rcs-tmp/tftpboot/").
-define(DC_UP_NAME, "/DC-*CXP*.tgz").
-define(RCS_MOD_PATH, "rcs_mod/").
%% -define(RCS_MOD_PATH, "/tmp/etxivri/rcs_mod/").
%%Change working dir to have enough disk space
-define(WORKSPACE, ct:get_config({jenkins_config, workspace})).
%%-define(WORKSPACE, "/tmp/eransbn").
-define(WORKING_DIR, ?WORKSPACE ++ "/ct_tmp/").


%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf.
%% @end
%%--------------------------------------------------------------------
suite() ->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    case Secure_board  of
	"yes" ->
	    [{timetrap, {hours, 2}},
	     {ct_hooks, [{rct_htmllink,[]},
			 {rct_upgrade,ug1},
			 {rct_rs232,console},
			 {cth_conn_log,[]},
			 {rct_cli, {cli, [{user, "SysAdminTest"}, {password, "SysAdminTest"},manual_connect]}},
			 {rct_netconf, {nc1, man_auth}}]}];
	_ ->
	    [{timetrap, {hours, 2}},
	     {ct_hooks, [{rct_htmllink,[]},
			 {rct_rpc, rpc_1},
			 {rct_upgrade,ug1},
			 {rct_rs232,console},
			 {cth_conn_log,[]},
			 {rct_core,[]},

			 {rct_logging, {upgrade,
					[{erlang,{["ERROR REPORT",
						   "CRASH REPORT"],
						  []
						 }}]}},
			 {rct_cli, {cli1, [manual_connect]}},
			 {rct_netconf,nc1}]}]


    end.

%% @hidden
init_per_suite(Config) ->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    [{sec_board, Secure_board}|Config].

%% @hidden
end_per_suite(_Config) ->
     ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    %%%% get permission denied
    %% LVS = rct_rpc:call(rpc_1, os, cmd, ["vgs"], 10000, print),
    %% VGS = rct_rpc:call(rpc_1, os, cmd, ["vgs"], 10000, print),
    %% ct:log("lvs: ~n~p~n", [LVS]),
    %% ct:log("vgs: ~n~p~n", [VGS]),
    Config.
%% @hidden
end_per_testcase(mod_to_up, _Config)  ->
    ct:pal("Remove existing files in working dir: ~p ",[?WORKING_DIR]),
    C_M_D_2 = "chmod 777 "++ ?WORKING_DIR  ++"*",
    os:cmd(C_M_D_2),
    CMD2 = "rm -rf " ++ ?WORKING_DIR ++ "*",
    ct:log("CMD2:~n~p~n",[CMD2]),
    os:cmd(CMD2),
    ok;
end_per_testcase(_TestCase, Config) ->
    %% LVS = rct_rpc:call(rpc_1, os, cmd, ["vgs"], 10000, print),
    %% VGS = rct_rpc:call(rpc_1, os, cmd, ["vgs"], 10000, print),
    %% ct:log("lvs: ~n~p~n", [LVS]),
    %% ct:log("vgs: ~n~p~n", [VGS]),

    %% Check if testcase failed and clean up
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p.  \n", [Reason])
    end,

    ok.

%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() ->
    %% [test_all].
    [mod_to_up, create, prepare, verify, activate, confirm
     %% mod_to_up, create, prepare, verify, activate, confirm
    ].

%%%--------------------------------------------------------------------
%%% @doc
%%% Create. <br/>
%%% @spec mod_to_up(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
mod_to_up(Config) ->
    ct:pal("Config:~n~p~n",[Config]),

    case ?config(saved_config, Config) of
	{_Saver, ConfigList} ->
	    {nr, Nr} = lists:keyfind(nr, 1, ConfigList),
	    Nr;
	undefined -> %% First time it will be undefined
	    ct:log("No saved nr exist in Config. probably first one!"),
	    Nr = 1
    end,

    ct:pal("Start Nr: ~p",[Nr]),

    %% Get STP name
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:log("UGPath:~n~p~n",[UGPath]),
    StpName = lists:last(string:tokens(UGPath, "/")),
    ct:log("StpName: ~p", [StpName]),

    ct:pal("Remove existing files in upgrade dir",[]),
    C_M_D = "chmod 777 "++ UGPath ++"/*",
    os:cmd(C_M_D),
    CMD = "rm -rf " ++ UGPath ++ "/*",
    ct:log("CMD:~n~p~n",[CMD]),
    os:cmd(CMD),

    timer:sleep(5000),

    os_cmd(os:cmd("mkdir -p "++ ?WORKING_DIR)),
    ct:log("WORKING_DIR ~p",[?WORKING_DIR]),
    %%Clean working dir
    ct:pal("Remove existing files in working dir: ~p ",[?WORKING_DIR]),
    C_M_D_2 = "chmod 777 "++ ?WORKING_DIR  ++"*",
    os:cmd(C_M_D_2),
    CMD2 = "rm -rf " ++ ?WORKING_DIR ++ "*",
    ct:log("CMD2:~n~p~n",[CMD2]),
    os:cmd(CMD2),

    A = os:cmd("ls " ++?TO_UP++StpName),
    ct:log("A: ls tftboot dir: ~p", [fix_str(A)]), 

    AA = os:cmd("cd " ++ ?WORKING_DIR ++";ls "),
    ct:log("AA: ls in current dir ~p", [fix_str(AA)]),
   
    AAA = os:cmd("cd " ++ ?WORKING_DIR ++";pwd "),
    ct:log("AAA: pwd: ~p", [AAA]),
  
    B = create_new_up_loop(StpName, Nr, 3, 1),

    BB = os:cmd("ls "++ ?WORKING_DIR ++ ?RCS_MOD_PATH),
    ct:log("BB: ~p", [fix_str(BB)]),

    E = os:cmd("mv "++ ?WORKING_DIR ++ ?RCS_MOD_PATH ++"*.cxp "++ UGPath),
    D = os:cmd("cp "++?WORKING_DIR++ ?RCS_MOD_PATH ++"*.xml "++ UGPath),
    os:cmd("rm -rf "++?WORKING_DIR++ ?RCS_MOD_PATH++"*.tgz"),
    XX = os:cmd("ls "++?WORKING_DIR ++ ?RCS_MOD_PATH ),
    ct:log("ls under: ~p ~n~p", [?WORKING_DIR ++?RCS_MOD_PATH , fix_str(XX)]),

    DE = os:cmd("ls "++ UGPath),

    ct:log("D: ~p", [D]),
    ct:log("E: ~p", [E]),
    ct:log("DE: ~p", [fix_str(DE)]),

    %%% due to disc problem in hub, mod all ups could fail due to out of space.
    %% case re:run(fix_str(B), 
    %% 		"FATAL ERROR:Probably out of space on output filesystem") of
    case re:run(fix_str(B), 
		"FATAL ERROR:") of
	{match, _} -> 
	    ct:fail(" Failed in mod up.");
	_Else ->
	    ok
    end,

    NewNr = Nr+1, %% Will be used in next mod_to_up
    NewConfig = [{nr, NewNr}],
    ct:pal("Add NewConfig: ~p",[NewConfig]),
    {save_config, NewConfig}.

fix_str(Str) ->
    string:tokens(Str, "\n").
os_cmd(Cmd)->
    case Cmd of
	[] -> ok;
	Reply  -> ct:fail("Cmd fail: ~p",[Reply])
    end.

create_new_up_loop(StpName, Nr, TimerSleepM, LoopCount)->
    B_CMD = ?MOD_TO_UP++?TO_UP++StpName++?DC_UP_NAME++" -N "++integer_to_list(Nr),
    ct:pal("B_CMD: ~p", [B_CMD]),
    B = os:cmd("cd " ++  ?WORKING_DIR ++"; "++B_CMD),
    ct:log("B: ~p", [fix_str(B)]),

    %% due to disc problem in hub, mod all ups could fail due to out of space.
    case re:run(fix_str(B), 
		"FATAL ERROR:") of
	{match, _} -> case LoopCount of
			  -1 -> B;
			  _Else -> 
			      ct:log("Sleep ~p minutes to see if that can solve the disc space problem when creating new up",
				     [TimerSleepM]),
			      timer:sleep(TimerSleepM*60*1000),
			      create_new_up_loop(StpName, Nr, TimerSleepM, LoopCount -1)
		      end;
	_Else ->
	    B
    end.

%%%--------------------------------------------------------------------
%%% @doc
%%% Create. <br/>
%%% @spec create(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
create(Config) ->
    ct:pal("Config:~n~p~n",[Config]),
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Create UG
    %%%%
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),

    ok = swm_test_lib:up_create_generic(?NC_Session,
					?SftpHost,
					?SftpUser,
					?SftpPassword,
					UGPath,
					MeId),

    case swm_test_lib:
	wait_for_swm_progress_result(?NC_Session, MeId, no_check) of
	[{"SUCCESS" = Result,
	  "createUpgradePackage" = ActionName,
	  ProgressInfo,
	  ResultInfo,
	  _State,
	  _ProgReport}] ->
	    ct:log("result:~p~n"
		   "actionName:~p~n"
		   "progressInfo:~p~n"
		   "resultInfo:~p~n",[Result,
				      ActionName,
				      ProgressInfo,
				      ResultInfo]),
	    ok;
	Result ->
	    ResultInfo =dummy,
	    ct:pal("createUpgradePackage: ~p",[Result]),
	    ct:fail(Result)
    end,

    Label = get_up_package(ResultInfo),
    ct:pal("Label: ~p", [Label]),

    Nr = get_nr_from_saved_config(Config),
    ct:pal("Nr: ~p",[Nr]),
    NewConfig = [{nr, Nr}, {uplabel, Label}],
    ct:pal("Add NewConfig: ~p",[NewConfig]),
    {save_config, NewConfig}.


%%%--------------------------------------------------------------------
%%% @doc
%%% Prepare. <br/>
%%% @spec prepare(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
prepare(Config) ->
    ct:pal("Config:~n~p~n",[Config]),

    %%%%
    %% Prepares an upgrade package,
    %% which means downloading a complete UP
    %%%%
    Label = perform_ug_action(Config, prepare),

    %% Add Nr and Label to Config.
    Nr = get_nr_from_saved_config(Config),
    ct:pal("Nr: ~p",[Nr]),
    NewConfig = [{nr, Nr}, {uplabel, Label}],
    ct:pal("Add NewConfig: ~p",[NewConfig]),
    {save_config, NewConfig}.

%%%--------------------------------------------------------------------
%%% @doc
%%% verify. <br/>
%%% @spec verify(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
verify(Config) ->
    ct:pal("Config:~n~p~n",[Config]),

    %%%%
    %% Verifies an upgrade package.
    %%%%
    Label = perform_ug_action(Config, verify),

    %% Add Nr and Label to Config.
    Nr = get_nr_from_saved_config(Config),
    ct:pal("Nr: ~p",[Nr]),
    NewConfig = [{nr, Nr}, {uplabel, Label}],
    ct:pal("Add NewConfig: ~p",[NewConfig]),
    {save_config, NewConfig}.

%%%--------------------------------------------------------------------
%%% @doc
%%% activate. <br/>
%%% @spec activate(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
activate(Config) ->
    ct:pal("Config:~n~p~n",[Config]),

    %%%%
    %% Activates an upgrade package.
    %% During activate the node will reboot.
    %%%%
    Label = perform_ug_action(Config, activate),

    %% Add Nr and Label to Config.
    Nr = get_nr_from_saved_config(Config),
    ct:pal("Nr: ~p",[Nr]),
    NewConfig = [{nr, Nr}, {uplabel, Label}],
    ct:pal("Add NewConfig: ~p",[NewConfig]),
    {save_config, NewConfig}.

%%%--------------------------------------------------------------------
%%% @doc
%%% confirm. <br/>
%%% @spec confirm(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
confirm(Config) ->
    ct:pal("Config:~n~p~n",[Config]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    Label = get_latest_up(Config),

    ct:pal("Perform ug confirm~n",[]),
    swm_test_lib:ug_confirm(?NC_Session,
			    Label,
			    MeId),

    %% Add Nr and Label to Config.
    Nr = get_nr_from_saved_config(Config),
    ct:pal("Nr: ~p",[Nr]),
    NewConfig = [{nr, Nr}, {uplabel, Label}],
    ct:pal("Add NewConfig: ~p",[NewConfig]),
    {save_config, NewConfig}.

%%%--------------------------------------------------------------------
%%% @doc
%%% remove. <br/>
%%% @spec remove(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
remove(Config) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    Label = get_latest_up(Config),

    %%%%
    %% Remove the created up.
    %%%%
    ct:pal("Remove upgrade package: : ~p",[Label]),
    swm_test_lib:remove_upgrade_package(?NC_Session,
					"SUCCESS",
					MeId,
					Label),

    ok.

%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------
get_latest_up(Config) ->
    {_Saver, ConfigList} = ?config(saved_config, Config),
    {uplabel, Label} = lists:keyfind(uplabel, 1, ConfigList),
    Label.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
perform_ug_action(Config, Action) ->
    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    %%%%
    %% Get Latest UP
    %%%%
    Label = get_latest_up(Config),

    %%%%
    %% Perform ug action
    %%%%
    ct:pal("Perform ug action:~n~p~n",[Action]),
    swm_test_lib:ug_action_match_result(?NC_Session,
    					"SUCCESS",
    					Label,
    					MeId,
    					Action),
    Label.

%%%--------------------------------------------------------------------
%%% @doc
%%% @end
%%%--------------------------------------------------------------------
get_nr_from_saved_config(Config) ->
    {_Saver, ConfigList} = ?config(saved_config, Config),
    {nr, Nr} = lists:keyfind(nr, 1, ConfigList),
    Nr.

%%%--------------------------------------------------------------------
%%% Description: Get latest UP
%%%--------------------------------------------------------------------
get_up_package(ResultInfo) ->
    UPLabel = lists:last(string:tokens(ResultInfo,"=")),
    ct:pal("UPLabel: ~p",[UPLabel]),
    UPLabel.
