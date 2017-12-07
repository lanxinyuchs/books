%%coding: latin-1
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_test_lib.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R9A/R10A/R11A/8
%%%
%%% @doc == support lib when testing upgrade mechanism. ==
%%% <br/>
%%%
%%%
%%% @end

-module(swm_test_lib).
%%% Except as specifically authorized in writing by Ericsson, the
%%% receiver of this document shall keep the information contained
%%% herein confidential and shall protect the same in whole or in
%%% part from disclosure and dissemination to third parties.
%%%
%%% Disclosure and disseminations to the receivers employees shall
%%% only be made on a strict need to know basis.
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/2      2013-12-13 etxivri     Created
%%% R2A/3      2014-01-07 etxivri     Update SwM ecim model name from RcsSwM.
%%% R2A/4      2014-01-14 etxivri     Fix printout of the procentage status.
%%% R2A/5      2014-01-15 etxivri     Changed a ct:pal to ct:log
%%% R2A/6      2014-01-15 etxivri     Updates due to be used from other SUITEs
%%% R2A/7      2014-01-23 etxivri     Export check_nc_session.
%%%                                   Added wait_for_node_state.
%%%                                   Added get_from_up.
%%% R2A/7      2014-01-27 etxivri     Added build_valid_ug_package
%%% R2A/7      2014-01-29 etxivri     Removed a check when open nc session.
%%% R2A/11     2014-02-05 etxivri     Added build_faulty_ug_packakage.
%%% R2A/13     2014-03-03 etxivri     Added wait_for_expecting_state.
%%% R2A/14     2014-03-19 etxivri     Update to get SwVersion.
%%%                                   Added check of alarms.
%%% R2A/14     2014-03-19 etxivri     Minor update when open nc session.
%%% R2A/15     2014-03-20 etxivri     Cleanup in upgrade dir, to make sure
%%%                                   Nothing old is used.
%%% R2A/16     2014-03-21 etxivri     Option no check on actionId to get result.
%%% R2A/17     2014-03-25 etxivri     Export get_report_progress
%%% R2A/18     2014-03-28 etxivri     Update get progress report.
%%% R2A/19     2014-03-28 etxivri     Changed ct:pal to ct:log
%%% R2A/19     2014-04-04 etxivri     Added to get just the report.
%%% R2A/22     2014-04-08 erarafo     Deprecation warning
%%% R2A/23     2014-04-09 etxivri     Cleanup in upgrade path befor build to up %%% R2A/24     2014-04-15 etxivri     Update to handle if several swVersions exist.
%%% R2A/25     2014-04-16 etxarnu     Moved SwVersion to end in get_sw_version
%%% R2A/25     2014-04-16 etxivri     Add pkill to trig fallback since reboot
%%%                                   not suported yet.
%%% R2A/27     2014-04-16 etxivri     Use of confirm instead dor commit that is
%%%                                   depricated.
%%% R2A/28     2014-05-07 etxivri     Added get_node_restart_str.
%%% R2A/29     2014-05-13 etxivri     Added use increase of DUMMY CXP rev when
%%%                                   build_valid_ug_packakage.
%%% R2A/30     2014-05-22 erarafo     Adapted for HS62272
%%% R2A/31     2014-05-22 etxivri     Added functio no check actionId when
%%%                                   remove up.
%%% R2A/31     2014-05-22 etxivri     Update to handle confirm resul when match
%%%                                   result on confirm/commit.
%%% R2A/33     2014-06-10 etxivri     Added new functions.
%%% R2A/34     2014-06-11 etxivri     Update to check report progress in UP MO
%%%                                   for prepare,verify,activate, confirm.
%%% R2A/35     2014-06-11 etxivri     Changed a ct::pal to ct:log.
%%% R2A/36     2014-06-26 etxivri     Added new functions. and coding: latin-1
%%% R2A/37     2014-08-04 etxivri     Update due to confirm will not update
%%%                                   progressReport
%%% R2A/39     2014-08-11 etxivri     Added wait_for_exp_up_state.
%%% R2A/40     2014-08-14 etxivri     Added new function.
%%% R2A/41     2014-08-15 etxivri     Add new functions.
%%% R2A/42     2014-08-15 etxivri     Update to handle that reportProgress could
%%%                                   take a while until it is presented
%%%                                   on dus41
%%% R2A/43     2014-08-29 etxivri     Update to handle if revnr start with zero.
%%% R2A/44     2014-09-02 etxivri     Update modify_cxs to change only CXS101549
%%%                                   ,otherwise other revision could be changed
%%% R2A/45     2014-09-02 etxivri     More update in modify_cxs.
%%% R2A/46     2014-09-08 etxivri     Add fuctions to get highest and lowest UP.
%%% R2A/47     2014-09-12 etxivri     Update get_me_id to minimize netconf
%%%                                   printouts in tc html log.
%%% R2A/48     2014-09-17 etxivri     update check when remove up.
%%% R2A/49     2014-09-25 etxivri     Better way to get highest label
%%% R2A/50     2014-09-26 etxivri     More updtae when get highest label.
%%% R2A/51     2014-09-30 etxivri     More updatee when get highest label.
%%% R2A/52     2014-10-15 eransbn     Updated for vc card
%%% R3A/1      2014-10-17 etxivri     set and erase housekeeping_delay_variable.
%%% R3A/2      2015-01-13 etxivri     Update to avoit ERROR in ct shell 
%%%                                   when node is down.
%%% R3A/3      2015-01-28 etxivri     decreased some sleeps due to it is used 
%%%                                   when measures of times.
%%%                                   Also changed ct:pal to ct:log to avoid
%%%                                   Error in ct shell.
%%% R3A/4      2015-01-29 etxivri     Update a activate progress check.
%%% R3A/5      2015-01-29 etxivri     Update a sleep.
%%% R3A/6      2015-02-10 etxivri     Wait for login prompt.
%%% R3A/7      2015-02-26 etxivri     Update check_kdu to handle not only tcu.
%%% R3A/8      2015-04-30 etxivri     update get_node_restart_str.
%%% R3A/9      2015-05-28 etxivri     Add fuction to get sftp server data.
%%% R4A/1      2015-06-09 erarafo     version_to_integer() mapping extended.
%%% R4A/2      2015-06-29 etxmlar     Changed modify_cxs
%%% R4A/3      2015-07-10 etxivri     Update to handle empty actionname after
%%%                                   unexpected restart.
%%% R4A/4      2015-09-02 etxivri     Add info about build to UP in html.log
%%% R4A/5      2015-09-02 etxivri     increased timeout when wait for netconf.
%%% R4A/5      2015-11-10 etxivri     Update get_me_id due to changed behaviour
%%%                                   in com in R5.
%%% R4A/7      2015-12-01 etxivri     Add disable/enable max allowed UPs.
%%% R5A/1      2016-01-13 etxivri     Add new functions used for ug loops.
%%% R5A/2      2016-03-29 etxivri     Update wait_for_expecting_state to use label.
%%% R5A/3      2016-04-11 etxivri     Bugfix in wait_for_expecting_state
%%% R6A/1      2016-04-25 etxjotj     Handle reportProgress undefined
%%% R6A/2      2016-06-07 etxivri     Increased timeout wait for node restart.
%%%                                   And some cleanup.
%%% R6A/3      2016-06-09 erarafo     Support for creating large MO trees.
%%% R6A/4      2016-06-10 erarafo     Support for creating large MO trees; performance
%%% R6A/5      2016-06-10 erarafo     Corrected createInstances/3
%%% R6A/6      2016-06-10 erarafo     Reverted to R6A/2
%%% R6A/7      2016-08-18 etxkols     Git migration requires that CC paths is not used 
%%% R7A/1      2016-10-03 etxivri     Add get_active_sw_version. 
%%% R7A/1      2016-11-15 etxivri     Add get_action_capable
%%% R9A/1      2017-02-28 eivmiha     Added support for FTPES
%%% R10A/1     2017-04-18 etxivri     Update for git env.
%%% R10A/2     2017-05-29 eivmiha     Added build_new_ug_package and step_down_version.
%%% R10A/4     2017-06-02 etxivri     Update for new git env to use another
%%%                                   way to step the rev in to_up.
%%% R11A/1     2017-08-29 etxivri     More update to be able to run in git env.
%%% R11A/2     2017-08-29 etxivri     More update to be able to run in git env.
%%% R11A/2     2017-09-01 etxivri     More update to be able to run in git env.
%%% R11A/4     2017-09-05 etxivri     More update to be able to run in git env.
%%% R11A/7     2017-09-14 etxivri     More update to be able to run in git env.
%%% R11A/8     2017-10-06 etxivri     Add usage of /env/RCSDE/bin/upgradeprep.sh
%%%                                   that shall work for both CC and GIT.
%%% ----------------------------------------------------------

% -compile([export_all]).

-export([
     build_valid_ug_packakage/1,
     build_valid_ug_packakage/2,
     build_faulty_ug_packakage/3,
     build_faulty_ug_packakage/4,
     up_create_generic/7,
     up_create_generic/6,
     up_action_generic/4,
     up_action_generic_no_check/4,
     remove_upgrade_package/2,
     remove_upgrade_package/4,
     remove_upgrade_package/5,
     remove_up_check_allowed_result/2,
     get_ups/1,
     get_sw_version/1,
     get_sw_version/2,
     get_active_sw_version/2,
     get_me_id/1,
     get_action_id/2,
     get_swm_action_id/2,
     get_up_action_id/3,
     wait_for_progress_result/3,
     wait_for_progress_result/4,
     wait_for_progress_result/5,
     wait_for_progress_result/6,
     wait_for_progress_result/7,
     wait_for_swm_progress_result/3,
     wait_for_swm_progress_result/4,
     check_nc_session/1,
     check_nc_session/2,
     wait_for_node_state/2,
     wait_for_node_state/3,
     %% get_from_up/1,
     get_specific_reportprogress_info/2,
     ug_create_match_result/8,
     ug_create_match_result/7,
     ug_action_match_result/5,
     ug_action_match_result/6,
     ug_action_match_result/7,
     ug_confirm/3,
     wait_for_swm_progress_done/5,
     wait_for_swm_progress_done/6,
     wait_for_progress_done/5,
     wait_for_progress_done/6,
     wait_for_progress_done/7,
     wait_for_expecting_state/4,
     wait_for_expecting_state/5,
     get_all_alarms/2,
     is_alarm/3,
     get_report_progress/2,
     get_report_progress/3,
     get_up_reportprogress/3,
     get_up_report_progress/2,
     get_up_report_progress/3,
     get_report/2,
     check_node_state/1,
     check_node_state/2,
     get_swm_report/2,
     pkill_to_trig_fallback/1, %% Need rct_rpc hook i tc.
     get_node_restart_str/0,
     get_up_state/3, %% Need rct_netconf hook i tc.
     wait_for_exp_up_state/4, %% Need rct_netconf hook i tc.
     modify_cxs/2,
     get_info_to_build_up/2,
     wait_for_ntp_synch/1, %% Need rct_rpc hook i tc.
     wait_for_ntp_synch/2,
     check_exp_reply_err_from_nc_action/2,
     get_highest_label/1,
     get_lowest_label/1,
     netconf_open/2,
     check_kdu/0,
     set_housekeeping_delay_variable/1,
     erase_housekeeping_delay_variable/1,
     wait_for_login/1,
     wait_for_login/2,
     get_sftp_host/0,
     get_sftp_user/0,
     get_sftp_password/0,
     get_sftp_data/0,
     disable_max_up_check/1,
     enable_max_up_check/1,
     build_same_ug_package/2,
     set_start_ug_rev_in_ug_top_xml/2,
     get_action_capable/2,
     wait_for_action_capable/3,
     extract_element/2,
     make_key/1,
     check_progress_elements/2,
     build_new_ug_package/2,
     step_down_version/1,
     mod_ug_package_1/2,  %% need for ug packages in new git env
     get_created_up/2,
     construct_to_ver/1,
     modify_existing_cxs_up_xml/1,
     check_env/0,
     get_cxs_up_xml/1
    ]).

-define(Sleep, 1000).

%%% ===========================================================================
%%% @doc
%%% @spec get_sftp_host() -> ok
%%% @end
%%% ===========================================================================
get_sftp_host() ->
    ct:log("### get SftpHost.", []),
    {SftpHost, _, _} = get_sftp_data(),
    ct:log("### SftpHost:~p ", [SftpHost]),
    SftpHost.

%%% ===========================================================================
%%% @doc
%%% @spec get_sftp_user() -> ok
%%% @end
%%% ===========================================================================
get_sftp_user() ->
    ct:log("### get SftpUser.", []),
    {_, SftpUser, _} = get_sftp_data(),
    ct:log("### SftpUser:~p ", [SftpUser]),
    SftpUser.

%%% ===========================================================================
%%% @doc
%%% @spec get_sftp_password() -> ok
%%% @end
%%% ===========================================================================
get_sftp_password() ->
    ct:log("### get SftpPassword.", []),
    {_, _, SftpPassword} = get_sftp_data(),
    ct:log("### SftpPassword:~p ", [SftpPassword]),
    SftpPassword.

%%% ===========================================================================
%%% @doc
%%% @spec get_sftp_data() -> ok
%%% @end
%%% ===========================================================================
get_sftp_data() ->
    ct:log("### get sftp data. SftpHost, Username, Password", []),
    [{host, SftpHost},{username, SftpUser},{password, SftpPassword}] = 
    ct:get_config(sftp_server),
    ct:log("### sftp data:\n SftpHost:~p ,\n "
       "Username:~p ,\n Password:~p ", [SftpHost, SftpUser, SftpPassword]),
    {SftpHost, SftpUser, SftpPassword }.

%%% ===========================================================================
%%% @doc
%%% Use this to set housekeeping_delay_variable. <br/>
%%% This will set the time for backups to exist before they can be removed.<br/>
%%% rct_rpc hook need to be used from TC. <br/>
%%% RpcHook = atom() <br/>
%%% @spec set_housekeeping_delay_variable(RpcHook) -> ok
%%% @end
%%% ===========================================================================
set_housekeeping_delay_variable(RpcHook) ->
    ct:log("### set variable: swm_test_housekeeping_delay to 10 ms!", []),
    ok = rct_rpc:call(RpcHook, swmLib, set_variable, 
              [swm_test_housekeeping_delay, 10], 10000, print),
    A = rct_rpc:call(RpcHook, swmLib, get_variable, 
             [swm_test_housekeeping_delay], 10000, print),
    ct:log("### swm_test_housekeeping_delay: ~p",[A]).

%%% ===========================================================================
%%% @doc
%%% Use to erase housekeeping_delay_variable.. <br/>
%%% Then the default value 3600 seconds will be used on backups.<br/>
%%% rct_rpc hook need to be used from TC. <br/>
%%% RpcHook = atom() <br/>
%%% @spec erase_housekeeping_delay_variable(RpcHook) -> ok
%%% @end
%%% ===========================================================================
erase_housekeeping_delay_variable(RpcHook) ->
    ct:log("### erase variable: swm_test_housekeeping_delay !", []),
    ok = rct_rpc:call(RpcHook, swmLib, erase_variable, 
              [swm_test_housekeeping_delay], 10000, print),
    undefined = rct_rpc:call(RpcHook, swmLib, get_variable, 
              [swm_test_housekeeping_delay], 10000, print).
    %% ct:log("### swm_test_housekeeping_delay: ~p",[A]).

%%% ===========================================================================
%%% @doc
%%% Use this to trig a fallback after activate succes. <br/>
%%% Pkill heart and beam at same time.<br/>
%%% rct_rpc hook need to be used from TC. <br/>
%%% RpcHook = atom() <br/>
%%% @spec pkill_to_trig_fallback(RpcHook) -> ok
%%% @end
%%% ===========================================================================
pkill_to_trig_fallback(RpcHook) ->
    ct:log("### trig fallback!",[]),
    rct_rpc:call(RpcHook, os, cmd, ["pkill -9 heart ; pkill -9 beam"],
         10000, print).

%%% ===========================================================================
%%% @doc
%%% Build upgrade packages using same deleivery that is installed on Node.
%%% CXS rev is steped. Using  rcs_upmod_and_upgradeprep script. <br/>
%%% NC_Session = atom() <br/>
%%% @spec build_valid_ug_packakage(NC_Session) -> ok
%%% @end
%%% ===========================================================================
build_valid_ug_packakage(NC_Session) ->
    Env = check_env(),
    case Env of
    	git ->
    	    ct:pal("New git env is used."),
	    CXS = ct:get_config({jenkins_config, cxp}),
	    %% CXS = "/proj/rcs/DCI/SBC/cache/20170901185144_3a9b6f1f6b98a91401ccbb013ecca6b534068269/RCS-BB_CXS2010013_2.cxs",
	    mod_ug_package_1(CXS, ug1);
    	_Other ->
    	    ct:pal("CC env is used."),
	    build_valid_ug_packakage_cc(NC_Session, ug1)
    end.

check_env() ->
    LsRCTpath = os:cmd("ls /vobs/rcs/test/RCT*/test/"),
    ct:log("LsRCTpath : ~p", [LsRCTpath]), 
    Env = case re:run(LsRCTpath,"No such file or directory") of
	      {match, _} ->
		  git;
	      nomatch ->
		  cc
	  end,
    ct:log("Env : ~p", [Env]),
    Env.

build_valid_ug_packakage(NC_Session, UgHook) ->
    Env = check_env(),
    case Env of
    	git ->
    	    ct:pal("New git env is used."),
	    CXS = ct:get_config({jenkins_config, cxp}),
	    %% CXS = "/proj/rcs/DCI/SBC/cache/20170912204503_f0444100262191466d8e6f905f647bd986f37c32/RCS-BB_CXS2010013_2.cxs",
	    mod_ug_package_1(CXS, UgHook);
	_Other ->
	    ct:pal("CC env is used."),
	    build_valid_ug_packakage_cc(NC_Session, UgHook)
    end.


build_valid_ug_packakage_cc(NC_Session, UgHook) ->
	    {StpName, CXS_LABEL} = get_info_to_build_up(NC_Session, UgHook),
	    
	    ct:pal("Remove existing files in upgrade dir",[]),
	    %% CMD = "\rm -rf /proj/rcs-tmp/upgrade/" ++ StpName ++"/*",
	    C_M_D = "chmod 777 /proj/rcs-tmp/upgrade/" ++ StpName ++"/*",
	    ct:pal("CMD:~n~p~n",[C_M_D]),
	    os:cmd(C_M_D),
	    CMD = "rm -rf /proj/rcs-tmp/upgrade/" ++ StpName ++"/*",
	    ct:pal("CMD:~n~p~n",[CMD]),
	    os:cmd(CMD),
	    
	    timer:sleep(5000),
	    
	    ct:log("# Run script that step unpack CXS and step REV nr with 1, ~n"
		   "Also update DUMMY CXP REV with 1. ~n"
		   "then build CXS again. ~n"
		   "After that it will be moved to UGPath unpacked.",[]),
	    %% CMD1 = "rcs_upmod_and_upgradeprep.sh -n " ++ StpName ++ " -r " ++ CXS_LABEL,
	    ModScriptPath = "$RCT_TOP/test/suites/SWM/bin/",
	    ModScriptName = "swm_mod_valid_fake.sh",
	    %% CMD1 = "rcs_upmod_and_upgradeprep.sh"
	    CMD1 = "$RCT_TOP/test/bin/rcs_upmod_and_upgradeprep.sh"
		" -n " ++ StpName ++
		" -m " ++ ModScriptPath ++ ModScriptName ++
		" -r " ++ CXS_LABEL,
	    ct:pal("CMD1:~n~p~n",[CMD1]),
	    
	    A = os:cmd(CMD1),
	    ct:log("~s", [A]).


%%% ===========================================================================
%%% @doc
%%% Build faulty upgrade packages using same deleivery that is installed <br/>
%%% on Node.
%%% CXS rev is steped. Using  rcs_upmod_and_upgradeprep script. <br/>
%%% NC_Session = atom() <br/>
%%% ModScriptPath = string() , path to mod script <br/>
%%% ModScriptName = string() , mod script name <br/>
%%% @spec build_faulty_ug_packakage(NC_Session,ModScriptPath,ModScriptName)->ok
%%% @end
%%% ===========================================================================
build_faulty_ug_packakage(NC_Session, ModScriptPath, ModScriptName) ->
    build_faulty_ug_packakage(NC_Session, ModScriptPath, ModScriptName, ug1).
build_faulty_ug_packakage(NC_Session, ModScriptPath, ModScriptName, UgHook) ->
    {StpName, CXS_LABEL} = get_info_to_build_up(NC_Session, UgHook),

    ct:pal("Remove existing files in upgrade dir",[]),
    %% CMD = "\rm -rf /proj/rcs-tmp/upgrade/" ++ StpName ++"/*",
    C_M_D = "chmod 777 /proj/rcs-tmp/upgrade/" ++ StpName ++"/*",
    ct:pal("CMD:~n~p~n",[C_M_D]),
    os:cmd(C_M_D),
    CMD = "rm -rf /proj/rcs-tmp/upgrade/" ++ StpName ++"/*",
    ct:pal("CMD:~n~p~n",[CMD]),
    os:cmd(CMD),

    timer:sleep(5000),

    ct:log("# Run script that step unpack CXS and step REV nr with 1, ~n"
       "also run mod script to build in a fault in UP.~n"
           "then build CXS again. ~n"
           "After that it will be moved to UGPath unpacked.",[]),

    %% CMD1 = "rcs_upmod_and_upgradeprep.sh"
    CMD1 = "$RCT_TOP/test/bin/rcs_upmod_and_upgradeprep.sh"
        " -n " ++ StpName ++
        " -m " ++ ModScriptPath ++ ModScriptName ++
        " -r " ++ CXS_LABEL,
    ct:log("CMD:~n~p~n",[CMD1]),
    A = os:cmd(CMD1),
    ct:log("~s", [A]).


%%% ===========================================================================
%%% @doc
%%% Preforms one of the UP actions - create. <br/>
%%% NC_Session = atom() <br/>
%%% Host = string(), IpAdress to sftp host <br/>
%%% Usr = string(), Sftp useer <br/>
%%% Password = string(), Sftp password <br/>
%%% UGPath = string(), Path to upgrade sw.
%%% MeId = string(), managedElementId default "1"
%%% @spec up_create_generic(Session, Host, User, Password, UGPath, MeId) -> ok
%%% @end
%%% ===========================================================================
up_create_generic(Session, Host, User, Password, UGPath, MeId) ->
    up_create_generic(Session, Host, User, Password, UGPath, MeId, sftp).

up_create_generic(Session, Host, User, Password, UGPath, MeId, Protocol) ->
    SwVerion = get_sw_version(Session, MeId),
    ct:pal("Active SwVersion : ~p , before create up.",[SwVerion]),

    ct:pal("Execution create upgrade package.",[]),

    Uri = case Protocol of 
              sftp -> "sftp://";
              ftpes -> "ftpes://"
          end ++User++"@"++Host++UGPath,

    Action = {'ManagedElement',
          [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
          [{managedElementId, [], [MeId]},
           {'SystemFunctions',
        [{systemFunctionsId,[],["1"]},
         {'SwM',
          [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
          [{swMId,[],["1"]},
           {createUpgradePackage, [],
            [{uri, [Uri]},
             {password, [Password]}]
           }]}]}]},
    {ok, _} = netconf(Session, action, [Action]),

    ok.

%%% ===========================================================================
%%% @doc
%%% Description: Performs one of the UP actions prepare, verify, activate, confirm  <br/>
%%% Session = atom() <br/>
%%% Host = string(), IpAdress to sftp host <br/>
%%% Usr = string(), Sftp useer <br/>
%%% Password = string(), Sftp password <br/>
%%% UGPath = string(), Path to upgrade sw.
%%% MeId = string(), managedElementId default "1"
%%% @spec up_action_generic(Session, UgAction, Label, MeId) -> ok
%%% @end
%%% ===========================================================================
up_action_generic(Session, UgAction, Label, MeId) ->
    ct:pal("Execution action ~w",[UgAction]),

    Key = make_key(Label),

    Action =  {'ManagedElement',
               [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
               [{managedElementId, [], [MeId]},
            {'SystemFunctions',
             [{systemFunctionsId,[],["1"]},
              {'SwM',
               [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
               [{swMId,[],["1"]},
            {'UpgradePackage', [],
             [{upgradePackageId, [Key]},
              {UgAction, [], []}]}]}]}]},

    ct:log("Calling action"),

    case netconf(Session, action, [Action]) of
        {error, Error} ->
            ct:pal("ActionError = ~p~n",[Error]),
            ct:fail("Action could not be started");
        {ok, A} ->
        %% ct:pal("# ~p",[A]),
            {ok, {returnValue, _, [ActionResult]}} =
                extract_element(returnValue, A),
            ct:pal("Action result: ~s",[ActionResult]),
            case ActionResult of
            "true" -> ok;
            "false" ->
                ct:fail("Action could not start")
            end
    end,

    ok.

%%% ===========================================================================
%%% @doc
%%% Description: Performs one of the UP actions prepare, verify, activate, confirm, returns the netconf reply from the action.  <br/>
%%% Session = atom() <br/>
%%% Host = string(), IpAdress to sftp host <br/>
%%% Usr = string(), Sftp useer <br/>
%%% Password = string(), Sftp password <br/>
%%% UGPath = string(), Path to upgrade sw.
%%% MeId = string(), managedElementId default "1"
%%% @spec up_action_generic_no_check(Session, UgAction, Label, MeId) -> Res
%%% @end
%%% ===========================================================================
up_action_generic_no_check(Session, UgAction, Label, MeId) ->
    ct:pal("Execution action ~w",[UgAction]),

    Key = make_key(Label),

    Action =  {'ManagedElement',
               [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
               [{managedElementId, [], [MeId]},
            {'SystemFunctions',
             [{systemFunctionsId,[],["1"]},
              {'SwM',
               [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
               [{swMId,[],["1"]},
            {'UpgradePackage', [],
             [{upgradePackageId, [Key]},
              {UgAction, [], []}]}]}]}]},

    {_, Res} = netconf(Session, action, [Action]),
    ct:log("Reply from netconf acttion: ~p. ~n ~p", [Action, Res]),
    Res.

%%% ===========================================================================
%%% @doc
%%% Description: Remove a upgrade package and match expected result. <br/>
%%% Session = atom() , Netcon session<br/>
%%% Label = string(), UP label ex "CXS101549/1-R2A3331" <br/>
%%% Result = String(), Expected result when action is finished, <br/>
%%%          ex. "SUCCESS" | "FAILURE"
%%% @spec remove_upgrade_package(Session, Result, MeId, Label) -> ok
%%% @end
%%% ===========================================================================
remove_upgrade_package(Session, Result, MeId, Label) ->
    ActionId = get_action_id(Session, MeId),
    remove_upgrade_package(Session, Result, MeId, Label, ActionId).

remove_upgrade_package(Session, Result, MeId, Label, ActionId) ->
    ok = remove_upgrade_package(Session, Label),
    timer:sleep(20000),
    ct:pal("Wait for progress after removeUpgradePackage"),
    wait_for_swm_progress_done(Session, Result, "removeUpgradePackage",
                   MeId, ActionId),
    ok.

%%% ===========================================================================
%%% @doc
%%% Description: Remove a upgrade package. <br/>
%%% Session = atom() <br/>
%%% Label = string(), UP label <br/>
%%% @spec remove_upgrade_package(Session, Label) -> ok
%%% @end
%%% ===========================================================================
remove_upgrade_package(Session, Label) ->
    ct:pal("removeUpgradePackage,  ~p",[Label]),
    MeId = get_me_id(Session),
    netconf_open(Session, []),
    {ok, Res} =
    ct_netconfc:action(Session,
               {'ManagedElement',
                [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
                [{managedElementId, [], [MeId]},
                 {'SystemFunctions',
                  [{systemFunctionsId,[],["1"]},
                   {'SwM',
                [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
                [{swMId,[],["1"]},
                 {removeUpgradePackage, [],
                  [{upgradePackage,[],
                    ["ManagedElement=1,SystemFunctions=1,"
                     "SwM=1,UpgradePackage="++Label]} ]}
                ]}]}]}),

    ok = ct_netconfc:close_session(Session),
    ct:pal("Remove upgrade package:~p, ~p",[Res, Label]),

    ok.

%%% ===========================================================================
%%% @doc
%%% Description: Remove a upgrade package. <br/>
%%% Check if remove fail if it is accepted or not. <br/>
%%% Session = atom() <br/>
%%% MeId = string()
%%% @spec remove_up_check_allowed_result(Session, Label) -> ok
%%% @end
%%% ===========================================================================
remove_up_check_allowed_result(Session, MeId) ->
    remove_up_check_allowed_result(Session, MeId, 60000).
remove_up_check_allowed_result(_Session, _MeId, Timeout) when Timeout < 0 ->
    ct:fail("Remove upgrade package fail due to no expected state rcvd");
remove_up_check_allowed_result(Session, MeId, Timeout) ->
    %%%%
    %% Check swm report
    %%%%
    Report = get_swm_report(Session, MeId),
    ct:log("reportProgress after remove up: ~p", [Report]),

    case [A || {state, _ , _} = A <- Report] of
    [ {state,[], ["FINISHED"] }] ->
        case [B || {result, _ , _} = B <- Report] of
        [ {result,[], ["SUCCESS"] }] ->
            ok;
        [ {result,[], ["FAILURE"] }] ->
            check_remove(Report)
        end;
    [ {state,[], [_Other] }] ->
        ct:log("state not expected: ~p , sleep and try again", [_Other]),
        timer:sleep(5000),
        remove_up_check_allowed_result(Session, MeId, Timeout-5000)
    end.


    %% %%%
    %% %% Check result.
    %% %% If fail then check if resultInfo is accpeted.
    %% %%%
    %% [ {state,[], ["FINISHED"] }] =
    %%  [A || {state, _ , _} = A <- Report],

    %% case [B || {result, _ , _} = B <- Report] of
    %%  [ {result,[], ["SUCCESS"] }] ->
    %%      ok;
    %%  _FailRes ->
    %%      case check_remove(Report) of
    %%      ok ->
    %%          ok;
    %%      Other ->
    %%          ct:pal("FailRes: ~p", [Other]),
    %%          ct:fail("Remove fail")
    %%      end
    %% end,

    %% ok.

check_remove(Report) ->
    ResultInfo_1 = "This upgrade package contains active software. "
    "It cannot be removed at this time.",
    ResultInfo_2 = "This upgrade package is referred by a backup. "
    "It cannot be removed at this time.",
    [ {resultInfo,[], [ExpStr] }] = [B || {resultInfo, _ , _} = B <- Report],
    case ExpStr of
    ResultInfo_1 ->
        ok;  %% don't care.
    ResultInfo_2 ->
        ok;  %% don't care.
    _Other ->
        %% _Other
        ct:pal("FailRes: ~p", [_Other]),
        ct:fail("Remove fail")
    end.

%%% ===========================================================================
%%% @doc
%%% Description: Get Managed Element Id. Print out SwM.<br/>
%%% Session = atom() <br/>
%%% MeId = string()
%%% @spec get_me_id(Session) -> MeId
%%% @end
%%% ===========================================================================
get_me_id(Session) ->
    netconf_open(Session, []),

    %% {ok,
    %%  [{'ManagedElement',
    %%    [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    %%    [{managedElementId,[],[MeId]}]}]} =
    %%  ct_netconfc:get(Session,
    %%          {'ManagedElement',
    %%           [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    %%           [{managedElementId,[],[]}]
    %%          }),

    {ok,
     [{'ManagedElement',
       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
       [{managedElementId,[],[MeId]} | _]}]} =
        ct_netconfc:get(Session,
                {'ManagedElement',
                 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
                 [{managedElementId,[],[]}]
                }),
    
    ct_netconfc:close_session(Session),
    ct:pal("MeId:~n~p~n",[MeId]),

    MeId.


%%% ===========================================================================
%%% @doc
%%% @end
%%% ===========================================================================
get_action_capable(Session, MeId) ->
    netconf_open(Session, []),

    {ok, CabableRes
    
    }   =
        ct_netconfc:get(Session,
                {'ManagedElement',
             [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
             [{managedElementId,[],[MeId]},
              {'SystemFunctions',
               [{systemFunctionsId,[],["1"]},
                {'SwM',
                 [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
                 [{swMId,[],["1"]},
                  {'actionCapable', [],[]}
                   ]}]}]}),
    
    ct_netconfc:close_session(Session),
    ct:log("QQQQ CabableRes:~n~p~n", [CabableRes]),

     [{'ManagedElement',
     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
     [{managedElementId,[],["1"]},
      {'SystemFunctions',[],
          [{systemFunctionsId,[],["1"]},
           {'SwM',
               [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
               [{swMId,[],["1"]},{actionCapable,[],[CapableValue]}]}]}]}] =
    CabableRes,
    ct:log("ZZZZZ CapableValue:~n~p~n", [CapableValue]),

    CapableValue.


wait_for_action_capable(Session, MeId, Value) ->
    wait_for_action_capable(Session, MeId, Value, 600000).
wait_for_action_capable(_Session, _MeId, _Value, Timeout) when Timeout < 0 ->
    ct:fail("TC will fail due to actionCapable has not correct value within exp time");
wait_for_action_capable(Session, MeId, Value, Timeout) ->
    ct:pal("check for action_capable value : ~p", [Value]),
    NowValue = get_action_capable(Session, MeId),
    case NowValue of
    Value ->
        ct:log("Rcvd exp action_capable value : ~p", [NowValue]);
    _Else ->
        ct:log("No exp action_capable value rcvd sleep and try again"),
        timer:sleep(5000),
        wait_for_action_capable(Session, MeId, Value, Timeout)
    end.



%%% ===========================================================================
%%% @doc
%%% Description: Read the action id of the last progress report to prevent
%%%              reading old reports. <br/>
%%%              Note! This will get up action Id. <br/>
%%% Session = atom() <br/>
%%% ActionId = string() or atom() ubdefined
%%% @spec get_action_id(Session, MeId) -> ActionId
%%% @end
%%% ===========================================================================
get_action_id(Session, MeId) ->
    [UP_Label | _] = lists:reverse(get_ups(Session)),
    ct:pal("Label:~n~p~n",[UP_Label]),
    get_up_action_id(Session, MeId, UP_Label).

    %% %% {ok,_} = ct_netconfc:open(Session, [{timeout, 30000}]),
    %% ct_netconfc:open(Session, [{timeout, 30000}]),
    %% case get_report_progress(Session, MeId) of
    %%  {reportProgress, [{unset, "true"}], []} ->
    %%      ActionId = undefined;
    %%  {reportProgress,
    %%   [{struct,"AsyncActionProgress"}],
    %%   ProgressReport} ->
    %%      {actionId,_,[ActionId]} =
    %%      lists:keyfind(actionId, 1, ProgressReport),
    %%      ActionId
    %% end,

    %% ok = ct_netconfc:close_session(Session, 30000),
    %% ct:pal("### ActionId: ~p", [ActionId]),

    %% ActionId.

%%% ===========================================================================
%%% @doc
%%% Description: Read the action id of the last progress report to prevent
%%%              reading old reports. <br/>
%%%              Note! This shall be used for create and remove. <br/>
%%% Session = atom() <br/>
%%% ActionId = string() or atom() ubdefined
%%% @spec get_swm_action_id(Session, MeId) -> ActionId
%%% @end
%%% ===========================================================================
get_swm_action_id(Session, MeId) ->
    %% {ok,_} = ct_netconfc:open(Session, [{timeout, 30000}]),
    netconf_open(Session, [{timeout, 30000}]),
    {reportProgress, Attributes, Report} = get_report_progress(Session, MeId),
    case lists:keyfind(unset, 1, Attributes) of
    {unset, "true"} ->
        ActionId = undefined;
    false ->
        {actionId,_,[ActionId]} =
        lists:keyfind(actionId, 1, Report),
        ActionId
    end,

    ok = ct_netconfc:close_session(Session, 30000),
    ct:pal("### ActionId: ~p", [ActionId]),

    ActionId.

%%% ===========================================================================
%%% @doc
%%% Description: Read the action id of the last progress report to prevent
%%%              reading old reports. <br/>
%%%      Note! This shall be used for prepare, verify, activate, confirm. <br/>
%%% Session = atom() <br/>
%%% ActionId = string() or atom() ubdefined
%%% @spec get_up_action_id(Session, MeId, Label) -> ActionId
%%% @end
%%% ===========================================================================
get_up_action_id(Session, MeId, UP_Label) ->
    %% {ok,_} = ct_netconfc:open(Session, [{timeout, 30000}]),
    netconf_open(Session, [{timeout, 30000}]),

    %% ProgressReport = get_up_report_progress(Session, MeId, UP_Label),
    ProgressReport = get_up_reportprogress(Session, MeId, UP_Label),
    ct:log("ProgressReport: ~p",[ProgressReport]),

    case ProgressReport of
    undefined ->
        ActionId = no_check;
    _Other ->
        {actionId,_,[ActionId]} =
        lists:keyfind(actionId, 1, ProgressReport),
        ActionId
    end,
    ok = ct_netconfc:close_session(Session, 30000),
    ct:pal("### UP ActionId: ~p", [ActionId]),

    ActionId.

%%% ===========================================================================
%%% @doc
%%% Description: Returns a list of all UP labels.<br/>
%%% Example: ["CXS101549/1-R2A1460","CXS101549/1-R2A1461","CXS101549/1-R2A1462"]
%%% Session = atom() <br/>
%%% @spec get_ups(Session) -> ok
%%% @end
%%% ===========================================================================
get_ups(Session) ->
    MeId = get_me_id(Session),
    Get_config = {'ManagedElement',
          [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
          [{managedElementId,[],[MeId]},
           {'SystemFunctions',
            [{systemFunctionsId,[],["1"]},
             {'SwM',
              [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
              [{swMId,[],["1"]}]}]}]},
    %% {ok,_} = ct_netconfc:open(nc1,[]),
    %% case ct_netconfc:get_config(nc1,running,Get_config) of
    case netconf(Session, get_config, [running, Get_config]) of
    {ok, [{'ManagedElement',
           [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
           [{managedElementId,[],[MeId]},
        {'SystemFunctions',[],
         [{systemFunctionsId,[],["1"]},
          {'SwM',
           [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
           Packages}]}]}]} ->
        [Label ||{'UpgradePackage',[],[{upgradePackageId,[],[Label]}|_]}
             <- Packages];
        %% ct_netconfc:close_session(nc1),
        %% Labels;
    {ok, Reply} ->
        %% ct_netconfc:close_session(nc1),
        ct:pal("Reply = ~p~n",[Reply]),
        ct:fail("Unexpected netconf Reply");
        {error, Error} ->
        %% ct_netconfc:close_session(nc1),
        ct:pal("get_config Error = ~p~n",[Error]),
        ct:fail("get_config Error")
    end.

%%% ===========================================================================
%%% @doc
%%% Description: SwVersion string.<br/>
%%% Example: "CXS101549/3-R2A2425"
%%% Session = atom() <br/>
%%% @spec get_sw_version(Session) -> SwVersion
%%% @end
%%% ===========================================================================
get_sw_version(Session) ->
    MeId = get_me_id(Session),
    get_sw_version(Session, MeId).

get_sw_version(Session, MeId) ->
    Get_SwInventory = {'ManagedElement',
                   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
                   [{managedElementId,[],[MeId]},
                {'SystemFunctions',
                 [{systemFunctionsId,[],["1"]},
                  {'SwInventory',[],
                   [{swInventoryId,[],["1"]},
                    {'SwVersion',[],[]}
                   ]}]}]},

    %% ct:pal("Get_SwInventory = ~p~n",[Get_SwInventory]),

    %% case  netconf(Session, get_config, [running, Get_SwInventory]) of
    %%  {ok,[{'ManagedElement',
    %%        [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    %%        [{managedElementId,[],["1"]},
    %%         {'SystemFunctions',[],
    %%      [{systemFunctionsId,[],["1"]},
    %%       {'SwInventory',
    %%        [{xmlns,"urn:com:ericsson:ecim:RcsSwIM"}],
    %%        [{swInventoryId,[],["1"]},
    %%         {'SwVersion',[],
    %%          [{swVersionId,[],
    %%            [SwVersion]}]}]}]}]}]} ->
    %%      ct:pal("SwVersion = ~p~n",[SwVersion]),
    %%      SwVersion;
    %%  {error, Error} ->
    %%      ct:pal("get_sw_version Error = ~p~n",[Error]),
    %%      ct:fail("get_sw_version Error")
    %% end.

    case  netconf(Session, get_config, [running, Get_SwInventory]) of
    {ok, [{'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],["1"]},
          {'SystemFunctions',[],
              [{systemFunctionsId,[],["1"]},
               {'SwInventory',
                   [{xmlns,"urn:com:ericsson:ecim:RcsSwIM"}],
        SwInventory
           }]}]}]} ->
        ct:pal("### ~p", [SwInventory]),

        SwVersions = lists:keydelete(swInventoryId, 1 , SwInventory),
        ct:pal("SwVersions = ~p~n",[SwVersions]),
        Sw_Versions =
        [X || {'SwVersion',[],[{swVersionId,[],[X]}]} <- SwVersions],
        [SwVersion|_T] = Sw_Versions,
        ct:pal("### SwVersion ~p", [SwVersion]),
        SwVersion;
    {error, Error} ->
        ct:pal("get_sw_version Error = ~p~n",[Error]),
        ct:fail("get_sw_version Error"),
        _SwVersion = dummy
    end.

%%% ===========================================================================
%%% @doc
%%% Description: SwVersion string.<br/>
%%% Example: "CXS101549/3-R2A2425"
%%% Session = atom() <br/>
%%% @spec get_active_sw_version(Session, MeId) -> SwVersion
%%% @end
%%% ===========================================================================
get_active_sw_version(Session, MeId) ->
    Get_SwInventory = {'ManagedElement',
                   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
                   [{managedElementId,[],[MeId]},
                {'SystemFunctions',
                 [{systemFunctionsId,[],["1"]},
                  {'SwInventory',[],
               [{swInventoryId,[],["1"]},
                {active,[],[]}
                    %% {'SwVersion',[],[]}
                   ]}]}]},

    case  netconf(Session, get, [Get_SwInventory]) of
    {ok, [{'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],["1"]},
          {'SystemFunctions',[],
              [{systemFunctionsId,[],["1"]},
               {'SwInventory',
                   [{xmlns,"urn:com:ericsson:ecim:RcsSwIM"}],
        SwInventory
           }]}]}]} ->
        ct:log("### ~p", [SwInventory]),
        {active,[],[SwVerMO]} = lists:keyfind(active,1, SwInventory),
        ct:log("### SwVerMO: ~p", [SwVerMO]),
        Token = string:tokens(SwVerMO,"="),
            ct:log("### Token SwVerMO: ~p", [Token]),
        SwVersion = lists:last(Token),
        ct:pal("### Active SwVersion: ~p", [SwVersion]),
        SwVersion;
    {error, Error} ->
        ct:pal("get_active_sw_version Error = ~p~n",[Error]),
        ct:fail("get_active_sw_version Error"),
        _SwVersion = dummy
    end.

%%% ===========================================================================
%%% @doc
%%% Description: wait for swm progress results. use for create and remove. <br/>
%%% Session = atom() <br/>
%%% MeId = string() <br/>
%%% OldActionId = string() <br/>
%%% @spec wait_for_swm_progress_result(Session, MeId, OldActionId) -> ok
%%% @end
%%% ===========================================================================
wait_for_swm_progress_result(Session, MeId, OldActionId) ->
    wait_for_progress_result(Session, MeId, OldActionId, dummy,
                 "swm_progess_report", dummy_label, 3600000).
wait_for_swm_progress_result(Session, MeId, OldActionId, Node) ->
    wait_for_progress_result(Session, MeId, OldActionId, Node,
                 "swm_progess_report", dummy_label, 3600000).
%%% ===========================================================================
%%% @doc
%%% Description: wait for progress results.<br/>
%%% Session = atom() <br/>
%%% MeId = string() <br/>
%%% OldActionId = string() <br/>
%%% @spec wait_for_progress_result(Session, MeId, OldActionId) -> ok
%%% @end
%%% ===========================================================================
wait_for_progress_result(Session, MeId, Old_ActionId) ->
    [UP_Label | _] = lists:reverse(swm_test_lib:get_ups(Session)),
    %% {ok,_} = ct_netconfc:open(Session, [{timeout, 30000}]),
    netconf_open(Session, [{timeout, 30000}]),
    case Old_ActionId of
    no_check ->
        OldActionId = no_check;
    _ ->
        OldActionId = Old_ActionId
    end,
    wait_for_progress_result(Session, MeId, OldActionId, dummy, dummy_action,
                UP_Label, 3600000).

wait_for_progress_result(Session, MeId, OldActionId, Node) ->
    %% [UP_Label | _] = lists:reverse(swm_test_lib:get_ups(Session)),
    UPs = swm_test_lib:get_ups(Session),
    UP_Label = get_highest_label(UPs),
    %% {ok,_} = ct_netconfc:open(Session, [{timeout, 30000}]),
    netconf_open(Session, [{timeout, 30000}]),
    wait_for_progress_result(Session, MeId, OldActionId, Node, dummy_action,
                 UP_Label, 3600000).

%%% Action = string() to match the action from progress.
wait_for_progress_result(Session, MeId, OldActionId, Node, Action) ->
    [UP_Label | _] = lists:reverse(swm_test_lib:get_ups(Session)),
        %% {ok,_} = ct_netconfc:open(Session, [{timeout, 30000}]),
    netconf_open(Session, [{timeout, 30000}]),
    wait_for_progress_result(Session, MeId, OldActionId, Node, Action,
                 UP_Label, 3600000).

wait_for_progress_result(Session, MeId, OldActionId, Node, Action, UP_Label) ->
    %% {ok,_} = ct_netconfc:open(Session, [{timeout, 30000}]),
    netconf_open(Session, [{timeout, 30000}]),
    wait_for_progress_result(Session, MeId, OldActionId, Node, Action,
                 UP_Label, 3600000).

wait_for_progress_result(Session, _MeId, _OldActionId, _Node, _Action,
             _UP_Label, Timeout)
  when Timeout < 0 ->
    ct_netconfc:close_session(Session, 30000),
    ct:fail("No Expected progress results within max timeout.");

wait_for_progress_result(Session, MeId, OldActionId, Node, Action,
             UP_Label, Timeout) ->
    ct:pal("¤¤ UP_Label : ~p", [UP_Label]),
    ct:pal("¤¤ Action : ~p", [Action]),

    case Node of
    dummy ->
        ok;
    _ ->
        ok = check_nc_session(Session, Node)
    end,

    case Action of
    Action when Action == "createUpgradePackage";
            Action == "removeUpgradePackage";
            Action == "swm_progess_report"->
        ct:pal("¤¤¤¤¤¤¤¤ check swm prog report ¤¤¤¤¤", []),
        {reportProgress,
         [{struct,"AsyncActionProgress"}],
         ProgressReport} = get_report_progress(Session, MeId),
        ProgressReport;

    _Action ->
        ProgressReport =  get_up_report_progress(Session, MeId, UP_Label),

        ok

    %% _Action -> %% prepare, verify, activate and confirm
    %%     %% Get UP progress report
    %%     {reportProgress,
    %%      [{struct,"AsyncActionProgress"}],
    %%      ProgressReport} = get_report_progress(Session, MeId),
    %%     ProgressReport
    end,

    {actionId,_,[ActionId]} = lists:keyfind(actionId, 1, ProgressReport),
    case ActionId of
    OldActionId ->
        ct:pal("Waiting for updated progress~n: old actionId : ~p ",
           [OldActionId]),
        timer:sleep(1000),
        wait_for_progress_result(Session, MeId, OldActionId, Node,
                     Action, UP_Label, Timeout-1000);
    _ ->
        case check_progress_elements(ProgressReport) of
        loop ->
            timer:sleep(1000),
            wait_for_progress_result(Session, MeId, OldActionId, Node,
                         Action, UP_Label, Timeout-1000);
        {ok, Result} ->
            ct:log("Result: ~p", [Result]),
            ct_netconfc:close_session(Session, 30000),
            ct:pal("New ActionId: ~p, OldActionId: ~p", [ActionId,
                                 OldActionId]),
            Result
        end
    end.

%%% ===========================================================================
%%% @doc
%%% Description: wait for an certain expected state in progress report.<br/>
%%% Session = atom() <br/>
%%% MeId = string() <br/>
%%% OldActionId = string() <br/>
%%% ExpState = string(),  "FINISHED" | "CANCELLED"<br/>
%%% @spec wait_for_expecting_state(Session, MeId, OldActionId, ExpState) -> ok
%%% @end
%%% ===========================================================================
wait_for_expecting_state(Session, MeId, Old_ActionId, ExpState) ->
    case Old_ActionId of
    no_check ->
        OldActionId = no_check;
    _ ->
        OldActionId = Old_ActionId
    end,
    netconf_open(Session, [{timeout, 30000}]),
    UPs = get_ups(Session),
    ct:log(" #UPs:~n~p~n",[UPs]),
    Label = get_highest_label(UPs),
    ct:log("# Label:~n~p~n",[Label]),
    wait_for_expecting_state(Session, MeId, OldActionId, ExpState, Label,60000).

wait_for_expecting_state(Session, MeId, Old_ActionId, ExpState, Label) ->
        case Old_ActionId of
    no_check ->
        OldActionId = no_check;
    _ ->
        OldActionId = Old_ActionId
    end,
    netconf_open(Session, [{timeout, 30000}]),
    wait_for_expecting_state(Session, MeId, OldActionId, ExpState, Label,60000).

wait_for_expecting_state(Session, _MeId, _OldActionId, _ExpState,_Label,Timeout)
  when Timeout < 0 ->
    ct_netconfc:close_session(Session, 30000),
    ct:fail("Not received Expected state within max timeout.");

wait_for_expecting_state(Session, MeId, OldActionId, ExpState, Label,Timeout) ->
    %% ProgressReport = get_up_report_progress(Session, MeId),
    ProgressReport = get_up_report_progress(Session, MeId, Label),

    {actionId,_,[ActionId]} = lists:keyfind(actionId, 1, ProgressReport),
    %ct:pal("###¤¤¤¤ ProgressReport: ~p",[ProgressReport]),
    %ct:pal("###: ~p, ~p",[ActionId, OldActionId]),
    case ActionId of
    OldActionId ->
        ct:pal("Waiting for updated progress~n"),
        timer:sleep(1000),
        wait_for_expecting_state(Session, MeId, OldActionId, ExpState,Label,
                     Timeout-1000);
    _ ->
        case check_progress_elements(ProgressReport, ExpState) of
        loop ->
            timer:sleep(1000),
            wait_for_expecting_state(Session,MeId, OldActionId,ExpState,
                         Label, Timeout-1000);
        {ok, Result} ->
            ct:log("Result: ~p", [Result]),
            ct_netconfc:close_session(Session, 30000),
            ct:pal("New ActionId: ~p, OldActionId: ~p", [ActionId,
                                 OldActionId]),
            Result
        end
    end.

%%% ===========================================================================
%%% @doc
%%% Description: check nc session exist, if not then check node is up.<br/>
%%%              If not then wait for node and nc sessions is up.<br/>
%%% Session = atom() <br/>
%%% Node = ErlNode, dummy if Node is not used. <br/>
%%% @spec check_nc_session(Session) -> ok
%%% @end
%%% ===========================================================================
check_nc_session(Session) ->
    check_nc_session(Session, dummy).
check_nc_session(Session, Node) ->
    check_nc_session(Session, Node, 900000). %% 15 min

check_nc_session(_Session, _Node, Timeout) when Timeout < 0 ->
    ct:fail("NC session not up within max timeout.");

check_nc_session(Session, Node, Timeout) ->
    case netconf_open(Session, [{timeout, 30000}]) of
    {ok,_} ->
        ok;
    {error,{connection_exists, _}} ->
        ok;
    _Err ->
        %% case Node of
        %%  dummy ->
        %%      ok;
        %%  _ ->
        %%      check_node_state(Node)
        %% end,
        ct:log("¤¤¤ nc_session not open: ~p. Sleep and try again",[_Err]),
        timer:sleep(?Sleep),
        check_nc_session(Session, Node, Timeout-?Sleep)
    end.

%%% ===========================================================================
%%% @doc
%%% Description: Wait for expected node state.<br/>
%%% Node = ErlNode <br/>
%%% State = atom(), down | up
%%% @spec wait_for_node_state(Node, ExpState) -> ok
%%% @end
%%% ===========================================================================
wait_for_node_state(Node, ExpState) ->
    wait_for_node_state(Node, ExpState, 600000). %% 10 min

wait_for_node_state(Node, ExpState, Timeout) when Timeout < 0 ->
    ct:pal("ErlNode state not expected : ~p, ~p", [Node, ExpState]),
    ct:fail("Node state not expected within max timeout.");

wait_for_node_state(Node, ExpState, Timeout) ->
    case ExpState of
    down ->
        case net_adm:ping(Node) of
        pang ->
            net_kernel:disconnect(Node),
            ct:pal("Nodedown."),
            timer:sleep(5000);
        _ ->
            net_kernel:disconnect(Node),
            timer:sleep(5000),
            wait_for_node_state (Node, ExpState, Timeout-5000)
        end;
    up ->
        case net_adm:ping(Node) of
        pong ->
            net_kernel:disconnect(Node),
            ct:pal("Nodedup."),
            timer:sleep(5000);
        _ ->
            net_kernel:disconnect(Node),
            timer:sleep(5000),
            wait_for_node_state (Node, ExpState, Timeout-5000)
        end
    end.

%% %%%--------------------------------------------------------------------
%% %%% Description: get_from_up
%% %%%--------------------------------------------------------------------
%% get_from_up(CLI_Session)->
%%     ok = rct_cli:connect(CLI_Session),
%%     {ok ,RecievedData} =
%%  rct_cli:send(CLI_Session,
%%           "show ManagedElement=1,SystemFunctions=1,SwM=1"),
%%     ok = rct_cli:disconnect(CLI_Session),
%%     Data = string:tokens(RecievedData, "=\r\n "),
%%     ct:log("show : ~p", [Data]),
%%     [_SwVersionMain, CXSLabel| _ ] = lists:dropwhile(fun(X) ->
%%                      X =/= "SwVersionMain"
%%                  end, Data),
%%     ct:pal("SwVersionMain : ~p", [CXSLabel]),
%%     CXSLabel.



%%% ===========================================================================
%%% @doc
%%% Description: Get data from specific report progress frpm SwM.<br/>
%%% CLI_Session = CLI session <br/>
%%% ReportProg = string(), ex "SwVersionMain" <br/>
%%% @spec  get_specific_reportprogress_info(CLI_Session, ReportProg) -> ok
%%% @end
%%% ===========================================================================
get_specific_reportprogress_info(CLI_Session, ReportProg) ->
    cli_connect(CLI_Session),
    {ok ,RecievedData} =
    rct_cli:send(CLI_Session,
             "show ManagedElement=1,SystemFunctions=1,SwM=1"),
    cli_disconnect(CLI_Session),

    Data = string:tokens(RecievedData, "=\r\n \""),
    ct:log("show : ~p", [Data]),
    [_Report, InfoStr| _ ] = lists:dropwhile(fun(X) ->
                        X =/=  ReportProg
                    end, Data),
    ct:log(" ~p: ~p", [ReportProg, InfoStr]),
    InfoStr.

%%% ===========================================================================
%%% @doc
%%% Description: Creste action will be done.<br/>
%%% Match the expected result is the same as recieved.
%%% NC_Session = Netconf session <br/>
%%% Result = string(), Expected result,  "SUCCESS" | FAILURE <br/>
%%% SftpHost <br/>
%%% SftpUser <br/>
%%% SftpPassword <br/>
%%% UGPath <br/>
%%% MeId <br/>
%%% @spec ug_create_match_result(NC_Session, Result, SftpHost, SftpUser, SftpPassword, UGPath, MeId)  -> ok
%%% @end
%%% ===========================================================================
ug_create_match_result(NC_Session, Result, SftpHost, SftpUser,
               SftpPassword, UGPath, MeId) ->
    ug_create_match_result(NC_Session, Result, SftpHost, SftpUser,
               SftpPassword, UGPath, MeId, sftp).

ug_create_match_result(NC_Session, Result, SftpHost, SftpUser,
               SftpPassword, UGPath, MeId, Protocol) ->
    %% ct:pal("#### A", []),
    ActionId = get_swm_action_id(NC_Session, MeId),
    %% ct:pal("#### B", []),
    ok = up_create_generic(NC_Session,
               SftpHost,
               SftpUser,
               SftpPassword,
               UGPath,
               MeId,
               Protocol),

    ct:pal("Wait for progress after create UP"),
    wait_for_swm_progress_done(NC_Session, Result,
                  "createUpgradePackage",
                  MeId,
                  ActionId).

%%% ===========================================================================
%%% @doc
%%% Description: Specific upgrade action will be done. <br/>
%%% Match the expected result is the same as recieved. <br/>
%%% confirm will not result in reportProgress updates. <br/>
%%% Just check return value when confirm is used. <br/>
%%% NC_Session = Netconf session <br/>
%%% Result = string(), Expected result,  "SUCCESS" | FAILURE <br/>
%%% Label <br/>
%%% MeId <br/>
%%% Action <br/>
%%% @spec ug_action_match_result(NC_Session, Result, Label, MeId, Action) -> ok
%%% @end
%%% ===========================================================================
ug_action_match_result(NC_Session, Result, Label, MeId, Action) ->
    ug_action_match_result(NC_Session,Result,Label,MeId,Action,dummy,dummy).

ug_action_match_result(NC_Session, Result, Label, MeId, Action, ErlNode) ->
    ug_action_match_result(NC_Session,Result,Label,MeId,Action,ErlNode,dummy).

ug_action_match_result(NC_Session,_Result,Label,MeId,confirm,_ErlNode,_Console)->
    ug_confirm(NC_Session, Label, MeId),
    "COMMIT_COMPLETED" = get_up_state(NC_Session, MeId, Label);

%% ug_action_match_result(NC_Session,Result,Label,MeId,Action,ErlNode) ->
ug_action_match_result(NC_Session,Result,Label,MeId,Action,ErlNode,Console) ->
    ActionId = get_action_id(NC_Session, MeId),
    %% ActionId = get_up_action_id(NC_Session, MeId, Label),
    ok = up_action_generic(NC_Session,
               Action,
               Label,
               MeId),

    %% %% Needed to prevent error in ct shell when node restarts and nc-sess is opened.
    ct:pal("Wait for progress after action: ~p.", [Action]),
    case Action of
    activate ->
        case Console of 
        dummy ->
            ct:pal("Activate, Sleep 120sec before check progress. ~n"
               "Do not use this when measure times.", []),
            timer:sleep(120000); %% Shall not be used when meas times.
        _Else ->
            ct:pal("Activate, Wait for node restarts.", []),
            %% wait_for_node_state(ErlNode, down),
            {ok, _} = ct_telnet:expect(Console, 
                           "Ericsson", 
                           [{idle_timeout,300000}, 
                        no_prompt_check]),
            timer:sleep(20000) %% Before start wait for progress.
        end;
    _Other ->
        ok
    end,

    ct:pal("Start check progress.", []),
    wait_for_progress_done(NC_Session, Result, atom_to_list(Action),
               MeId, ActionId, ErlNode, Label).


ug_confirm(NC_Session, Label, MeId) ->
    ct:pal("Execute action confirm~n",[]),
    Key = make_key(Label),
    Action =  {'ManagedElement',
               [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
               [{managedElementId, [], [MeId]},
            {'SystemFunctions',
             [{systemFunctionsId,[],["1"]},
              {'SwM',
               [],
               [{swMId,[],["1"]},
            {'UpgradePackage', [],
             [{upgradePackageId, [Key]},
              {confirm, [], []}]}]}]}]},
    ct:pal("Calling action"),
    case netconf(NC_Session, action, [Action]) of
        {error, Error} ->
        ct:pal("ActionError = ~p~n",[Error]),
        ct:fail("Action confirm failed");
    {ok, A} ->
        {ok, {returnValue, _, [ActionResult]}} =
        extract_element(returnValue, A),
        ct:pal("Action result: ~s",[ActionResult]),
        case ActionResult of
        "true" -> ok;
        "false" ->
            ct:fail("Action confirm could not start")
        end
    end.


%%% ===========================================================================
%%% @doc
%%% Description: Specific upgrade action will be done. <br/>
%%%              Match the expected result is the same as recieved.<br/>
%%% NC_Session = Netconf session <br/>
%%% ExpResult = string(), Expected result,  "SUCCESS" | FAILURE <br/>
%%% ActionName = string(), "create" | "remove" <br/>
%%% MeId = string() <br/>
%%% ActionId = string() , previous action id. <br/>
%%% @spec  wait_for_swm_progress_done(NC_Session, ExpResult, ActionName, MeId, ActionId) -> ok
%%% @end
%%% ===========================================================================
%%%%
%% Used when create and remove is used.
%%%%
wait_for_swm_progress_done(NC_Session, ExpResult, ActionName,
               MeId, ActionId) ->
    wait_for_swm_progress_done(NC_Session, ExpResult, ActionName,
                  MeId, ActionId, dummy).

wait_for_swm_progress_done(NC_Session, ExpResult, ActionName, MeId,
               ActionId, ErlNode) ->
    case wait_for_progress_result(NC_Session, MeId, ActionId, ErlNode,
                 ActionName) of
        [{ExpResult, ActionName, ProgressInfo, ResultInfo,
      State, _ProgReport}] ->
            ct:log("result:~p~n"
               "actionName:~p~n"
               "progressInfo:~p~n"
               "resultInfo:~p~n"
           "state:~p~n",[ExpResult,
                 ActionName,
                 ProgressInfo,
                 ResultInfo,
                 State]),
            ok;
        Result ->
            ct:pal("Progress report not expected: ~p",[Result]),
            ct:fail(Result)
    end.

%% ===========================================================================
%%% @doc
%%% Description: Specific upgrade action will be done. <br/>
%%%              Match the expected result is the same as recieved.<br/>
%%% NC_Session = Netconf session <br/>
%%% ExpResult = string(), Expected result,  "SUCCESS" | FAILURE <br/>
%%% ActionName = string(), "prepare" | "verify" | "activate" | "commit" <br/>
%%% MeId = string() <br/>
%%% ActionId = string() , previous action id. <br/>
%%% @spec  wait_for_progress_done(NC_Session, ExpResult, ActionName, MeId, ActionId) -> ok
%%% @end
%%% ===========================================================================
%%%%
%% Used when prepare, verify, activate, confirm is used.
%%%%
wait_for_progress_done(NC_Session, ExpResult, ActionName, MeId, ActionId) ->
    wait_for_progress_done(NC_Session, ExpResult, ActionName, MeId,
               ActionId, dummy).

wait_for_progress_done(NC_Session, ExpResult, ActionName, MeId,
               ActionId, ErlNode) ->
    wait_for_progress_done(NC_Session, ExpResult, ActionName, MeId,
               ActionId, ErlNode, dummy_up_label).

wait_for_progress_done(NC_Session, ExpResult, ActionName, MeId,
               ActionId, ErlNode, UP_Label) ->

    case UP_Label of
        dummy_up_label -> %% Take latest created UP.
            [Label | _] = lists:reverse(get_ups(NC_Session)),
            ct:pal("Label:~n~p~n",[Label]),
        Label;
    _UP_Label ->
        Label = UP_Label,
        ct:pal("Label:~n~p~n",[Label]),
        Label
    end,


    case wait_for_progress_result(NC_Session, MeId, ActionId, ErlNode,
                  ActionName, UP_Label) of
        [{ExpResult, ActionName, ProgressInfo, ResultInfo,
      State, _ProgReport}] ->
            ct:log("result:~p~n"
               "actionName:~p~n"
               "progressInfo:~p~n"
               "resultInfo:~p~n"
           "state:~p~n",[ExpResult,
                 ActionName,
                 ProgressInfo,
                 ResultInfo,
                 State]),
            ok;
        Result ->
            ct:pal("Progress report not expected: ~p",[Result]),
            ct:fail(Result)
    end.

%%% ===========================================================================
%%% @doc
%%% Description: This function returns 'true' if an active alarm with the <br/>
%%%              given minorType (as a string) is present <br/>
%%% Nc = Netconf session <br/>
%%% ExpResult = string(), Expected result,  "SUCCESS" | FAILURE <br/>
%%% ActionNmae = atom(), prepare | verify | activate | commit <br/>
%%% MeId = string() <br/>
%%% ExpMinorType = string() eaxmpel "9175043"
%%% @spec is_alarm(Nc, MeId, MinorType) -> true
%%% @end
%%% ===========================================================================
is_alarm(Nc, MeId, MinorType) ->
    FmAlarms = get_all_alarms(Nc, MeId),
    lists:foldr(
      fun(_FmAlarm, true) ->
          true;
     (FmAlarm, false) ->
          case extract_element(minorType, [FmAlarm]) of
          {ok, {_, _, [MinorType]}} ->
              ct:pal("FmAlarm MinorType : ~p, Exist.", [MinorType]),
              true;
          _ ->
              ct:pal("FmAlarm MinorType : ~p, Not Exist.", [MinorType]),
              false
          end
      end, false, FmAlarms).

%%% ===========================================================================
%%% @doc
%%% Description: Returns a list of all FmAlarm objects
%%% Nc = Netconf session <br/>
%%% MeId = string() <br/>
%%% @spec get_all_alarms(Nc, MeId) -> FmAlarm
%%% @end
%%% ===========================================================================
get_all_alarms(Nc, MeId) ->
    Get = {'ManagedElement',
       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
       [{managedElementId,[],[MeId]},
        {'SystemFunctions',
         [{systemFunctionsId,[],["1"]},
          {'Fm',
           [{fmId,[],["1"]}]}]}]},
    {ok, Result} = netconf(Nc, get, [Get]),
    {ok, {_, _, Contents}} = extract_element('Fm', Result),
    [FmAlarmE||FmAlarmE<-Contents,
           element(1, FmAlarmE) == 'FmAlarm'].

%%% ===========================================================================
%%% @doc
%%% Description: Get a string to match that node start to restart. <br/>
%%% @spec get_node_restart_str() -> SecondStageUboot
%%% @end
%%% ===========================================================================
get_node_restart_str() ->
    BoardType =
    proplists:get_value(board_type,
                ct:get_config(ct:get_config({test_nodes,1}))),
    SecondStageUboot = case BoardType of
               BoardType when BoardType == "dus4101";
                      BoardType == "duw4101" ->
                    "2:nd Stage Boot Loader";
               _ ->
                   "Ericsson Version: " % tcu03, dus5201, dus3201?
               end,
    ct:log("SecondStageUboot: ~p", [SecondStageUboot]),
    SecondStageUboot.


%%% ===========================================================================
%%% @doc
%%% Description: Get state from specific UpgradePackage. <br/>
%%% Nc = Netconf session <br/>
%%% MeId = string() <br/>
%%% Label = string() , ex "CXS101549/3-R2A2956" <br/>
%%% @spec get_up_state(NC_Session, MeId, Label) -> UP_state
%%% @end
%%% ===========================================================================
get_up_state(NC_Session, MeId, Label) ->
    netconf_open(NC_Session, []),

    UPInfoList = get_up_info_list(NC_Session, MeId, Label),

    ct_netconfc:close_session(NC_Session),

    {state,[],[UP_state]} = lists:keyfind(state, 1, UPInfoList),
    ct:pal("### UP_state: ~p", [UP_state]),

    UP_state.

%%% ===========================================================================
%%% @doc
%%% Description: Wait for specific state from specific UpgradePackage. <br/>
%%% Nc = Netconf session <br/>
%%% MeId = string() <br/>
%%% Label = string() , ex "CXS101549/3-R2A2956" <br/>
%%% ExpState = string() , ex "INITIALIZED" <br/>
%%% @spec wait_for_exp_up_state(NC_Session, MeId, Label, ExpState) ->ok
%%% @end
%%% ===========================================================================
wait_for_exp_up_state(NC_Session, MeId, Label, ExpState) ->
    wait_for_exp_up_state(NC_Session, MeId, Label, ExpState, 60000).
wait_for_exp_up_state(_NC_Session, _MeId, _Label, _ExpState, Timeout) when Timeout < 0 ->
    ct:fail("Exp up state not rcvd within 60 sec.!");
wait_for_exp_up_state(NC_Session, MeId, Label, ExpState, Timeout) ->
    ct:pal("wait for exp up state: ~p , Label: ~p", [ExpState, Label]),
    case get_up_state(NC_Session, MeId, Label) of
    ExpState ->
        ct:pal("Exp up state: ~p rcvd.", [ExpState]),
        ok;
    _Other ->
        ct:log("NOK, Exp up state: ~p not rcvd, try again.", [_Other]),
        timer:sleep(5000),
        wait_for_exp_up_state(NC_Session, MeId, Label, ExpState, Timeout-5000)
    end.

%%% ===========================================================================
%%% @doc
%%% Description: Get report progress specific UpgradePackage. <br/>
%%% Nc = Netconf session <br/>
%%% MeId = string() <br/>
%%% Label = string() , ex "CXS101549/3-R2A2956" <br/>
%%% @spec get_up_reportprogress(NC_Session, MeId, Label) -> ReportProgress
%%% @end
%%% ===========================================================================
get_up_reportprogress(NC_Session, MeId, UP_Label) ->
    UPInfoList = get_up_info_list(NC_Session, MeId, UP_Label),

    case lists:keyfind(reportProgress, 1, UPInfoList) of
    {reportProgress, [_], Report_Progress} ->
        ReportProgress = Report_Progress;
    false ->
        ReportProgress = undefined;
    _Err ->
        ct:pal("_Err: ~p",[_Err]),
        ReportProgress = dummy,
        ct:fail("Unexxpected result from get_up_info_list.")
    end,
    ct:log("### UP_ReportProgress: ~p", [ReportProgress]),
    ReportProgress.

%%% ===========================================================================
%%% @doc
%%% Description: Get report progress from specific UpgradePackage. <br/>
%%% Nc = Netconf session <br/>
%%% MeId = string() <br/>
%%% @spec get_up_report_progress(NC_Session, MeId) -> ReportProgress
%%% @end
%%% ===========================================================================
get_up_report_progress(NC_Session, MeId) ->
    %% Get latest created UP label
    [UP_Label | _] = lists:reverse(swm_test_lib:get_ups(NC_Session)),
    get_up_report_progress(NC_Session, MeId, UP_Label, 60000).
%%% ===========================================================================
%%% @doc
%%% Description: Get report progress from specific UpgradePackage. <br/>
%%% Nc = Netconf session <br/>
%%% MeId = string() <br/>
%%% Label = string() , ex "CXS101549/3-R2A2956" <br/>
%%% @spec get_up_report_progress(NC_Session, MeId, Label) -> ReportProgress
%%% @end
%%% ===========================================================================
get_up_report_progress(NC_Session, MeId, UP_Label) ->
    get_up_report_progress(NC_Session, MeId, UP_Label, 60000).

get_up_report_progress(_NC_Session, _MeId, _UP_Label, Timeout)
  when Timeout < 0 ->
    ct:fail("No up progress report rceived within max timeout.");

get_up_report_progress(NC_Session, MeId, UP_Label, Timeout) ->
    ct:pal("### UP_Label: ~p", [UP_Label]),
    case ct_netconfc:get(NC_Session,
             {'ManagedElement',
           [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
           [{managedElementId, [], [MeId]},
            {'SystemFunctions',
             [{systemFunctionsId,[],["1"]},
              {'SwM',
               [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
               [{swMId,[],["1"]},
            {'UpgradePackage', [],
             [{upgradePackageId,[],[UP_Label] }] }
               ]}]}]}, 30000) of

    {ok, [{'ManagedElement',
           [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
           [{managedElementId,[],["1"]},
        {'SystemFunctions',[],
         [{systemFunctionsId,[],["1"]},
          {'SwM',
           [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
           [{swMId,[],["1"]},
            {'UpgradePackage',[], UPInfoList}
           ]}]}]}] } ->

        %% ct:pal("UPInfoList:  ~p ",[UPInfoList]),
        %% Filter out reportProgress

        ct:log("UPInfoList:~p",[UPInfoList]),
        %% {reportProgress,[_], ReportProgress } =
        %%  lists:keyfind(reportProgress, 1, UPInfoList),
        %% ct:pal("### UP_ReportProgress: ~p", [ReportProgress]),
        %% ReportProgress;
        case lists:keyfind(reportProgress, 1, UPInfoList) of
        {reportProgress,[_], ReportProgress } ->
            ct:log("### UP_ReportProgress: ~p", [ReportProgress]),
            ReportProgress;
        _Other ->
            ct:log("Other:~p",[_Other]),
            timer:sleep(2000),
            get_up_report_progress(NC_Session,
                       MeId,
                       UP_Label,
                       Timeout-2000)
        end;
    _Err ->
        ct:log("No progress report recived: ~p ~n"
           "Check nc session. ~n",[_Err]),
        timer:sleep(1000),
        check_nc_session(NC_Session),
        get_up_report_progress(NC_Session, MeId, UP_Label, Timeout-1000)
    end.


%%%%
%%%%
get_up_info_list(NC_Session, MeId, Label) ->

    {ok, Res} = ct_netconfc:get(NC_Session,
          {'ManagedElement',
           [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
           [{managedElementId, [], [MeId]},
            {'SystemFunctions',
             [{systemFunctionsId,[],["1"]},
              {'SwM',
               [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
               [{swMId,[],["1"]},
            {'UpgradePackage', [],
             [{upgradePackageId,[],[Label] }] }
               ]}]}]}, 30000),

    ct:log("# # # Res: ~p", [Res]),

    [{'ManagedElement',
      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
      [{managedElementId,[],["1"]},
       {'SystemFunctions',[],
    [{systemFunctionsId,[],["1"]},
     {'SwM',
      [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
      [{swMId,[],["1"]},
       {'UpgradePackage',[], UPInfoList}
      ]}]}]}] = Res,


    ct:log("UPInfoList: ~p", [UPInfoList]),

    UPInfoList.


%%% ===========================================================================
%%% @doc
%%% Description: Increase version in cxs xml on the previous label. <br/>
%%%              This is used by create. ex dir /proj/rcs-tmp/upgrade/tcu021/
%%%              Increase the PrevLabel with IncreseNr.
%%% PrevLabel = string() , ex "CXS101549/3-R2A2956" <br/>
%%% IncreseNr = integer() <br/>
%%% @spec modify_cxs(PrevLabel, IncreseNr) -> ok
%%% @end
%%% ===========================================================================
modify_cxs(PrevLabel, IncreseNr) ->
    ct:log("# modify_cxs for next upgrade.~n"
           "# PrevLabel: ~p , IncreseNr: ~p", [PrevLabel, IncreseNr]),

    %% SW_Vers = swm_test_lib:get_sw_version(NC_Session, MeId),
    SW_Vers = PrevLabel,
    ct:pal("A_CXS:~n~p~n",[SW_Vers]),

    %% Get STP name
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),
    
    %% Modify cxs.xml. Construct a CXS label.
    [CXS, Index, Label] = string:tokens(SW_Vers, "/-"),
    ct:pal("CXS:~p, Index: ~p, Label: ~p ~n",[CXS, Index, Label]),

    %% Increase label version with 1.
    ToVersionNr = integer_to_list(IncreseNr),

    LabelLenght = length(Label),
    LastElementNr = LabelLenght - 1,
    {Pre, LastElement} =  lists:split(LastElementNr,Label),

    ToVer =   
    case string:to_integer(LastElement) of
        {error,no_integer} ->
        Label++ToVersionNr;
        {Integer, []} ->
        Pre++integer_to_list(Integer + IncreseNr)
    end,
    ct:pal("ToVer: ~p~n", [ToVer]),

    %% CxsUp = "cxs101549_"++Index++"-up.xml",
    XmlFilename = get_cxs_up_xml(UGPath),
    ct:pal("XmlFilename: ~p~n", [XmlFilename]),

    %%%% This sed cmd change version on line that match CXS.
    %%%% Changes is done directly in cxs-xml when using -i flag in sed cmd.
    CMD1 = "sed -i -e '/"++CXS++"/s/"++Label++"/"++ToVer++"/' "++UGPath++"/"++XmlFilename,
    ct:pal("CMD1:~n~p~n",[CMD1]),
    GG = os:cmd(CMD1),
    ct:pal("GG:~n~p~n",[GG]),
    
    CMD2 = os:cmd("cat "++UGPath++"/"++XmlFilename),
    ct:log("cat cxs:~n~p~n",[string:tokens(CMD2, "\n\" ")]).


%%%%%
%% Only for dummy UP, both CC and git.
%%%%%
modify_existing_cxs_up_xml(UgHook) ->
    {ok, UGPath} = rct_upgrade:get_up_dir(UgHook),
    ct:log("UGPath:~n~p~n",[UGPath]),

    XmlFilename = get_cxs_up_xml(UGPath),
    SearchStr = get_cxs_nr(XmlFilename),
    GrepRes = grep_product_info_from_xml(SearchStr, UGPath, XmlFilename),
    FromVer = get_from_ver(GrepRes),
    ToVer = construct_to_ver(FromVer),
    ct:pal("ToVer: ~p",[ToVer]),
    timer:sleep(2000),
    CXS = get_cxs(GrepRes),
    increase_version_in_up_xml(CXS, FromVer, ToVer, UGPath, XmlFilename).


get_cxs_up_xml(UGPath) ->
    {ok, Filenames} = file:list_dir(UGPath),
    ct:pal("Filenames: ~p",[Filenames]),
    XmlFilename = case get_xml_filename(Filenames) of
		      no_xml -> ct:fail("No xml file in folder");
		      Filename -> Filename
		  end,
    ct:pal("XmlFilename: ~p",[XmlFilename]),
    XmlFilename.

get_cxs_nr(XmlFilename) ->
    [SearchStr | _] = string:tokens(XmlFilename,"-up"),
    ct:pal("SearchStr: ~p",[SearchStr]),
    SearchStr.

grep_product_info_from_xml(SearchStr, UGPath, XmlFilename) ->
    GrepRes = os:cmd("grep -i -m 1 "++SearchStr++" "++UGPath++"/"++XmlFilename),
    ct:pal("GrepRes: ~p",[GrepRes]),
    ModGrepRes = string:tokens(GrepRes,"<>\" \n"),
    ct:pal("ModGrepRes: ~p",[ModGrepRes]),
    ModGrepRes.

get_from_ver(GrepRes) ->
    ["version=", FromVer | _] = lists:dropwhile(fun(X) ->
				      X =/= "version="
			   end, GrepRes),
    ct:pal("FromVer: ~p",[FromVer]),
    FromVer.

get_cxs(GrepRes) ->
    ["id=", CXS | _] = lists:dropwhile(fun(X) ->
					       X =/= "id="
				       end, GrepRes),
    ct:pal("CXS: ~p",[CXS]),
    CXS.

increase_version_in_up_xml(CXS, FromVer, ToVer, UGPath, XmlFilename) ->
    CMD1 = "sed -i -e '/"++CXS++"/s/"++FromVer++"/"++ToVer++"/' "++UGPath++"/"++XmlFilename,
    ct:pal("CMD1:~n~p~n",[CMD1]),
    GG = os:cmd(CMD1),
    ct:log("GG:~n~p~n",[GG]),
    timer:sleep(2000),

    CMD2 = os:cmd("cat "++UGPath++"/"++XmlFilename),
    A= string:tokens(CMD2, "\n\"< >"),
    %% ct:log("CMD2:~n~p~n",[CMD2]),
    ct:log("cat cxs:~n~p~n",[A]),
    ok.

%%% ===========================================================================
%%% @doc
%%% Description: Make sure that NTP is synced.
%%%              This shall be used when test restarts nodes in sequence,
%%%              to make sure node not restarts due to timediff more than 5 min.
%%% Rpc = atom() . name from rct_rpc hook.<br/>
%%% @spec wait_for_ntp_synch(Rpc) -> ok
%%% @end
%%% ===========================================================================
wait_for_ntp_synch(Rpc) ->
    wait_for_ntp_synch(Rpc, 60).

wait_for_ntp_synch(_Rpc, Timeout) when Timeout < 0 ->
    ct:fail("NTP synch not OK within expected time.");

wait_for_ntp_synch(Rpc, Timeout) ->
    NtpState = rct_rpc:call(Rpc, comsaNtpServer,
                is_synced_dirty, [], 10000, noprint),
    case NtpState of
    true ->
        ct:pal("## NtpState : ~p", [NtpState]),
        ok;
     _ ->
        ct:pal("## ## NtpState : ~p", [NtpState]),
        timer:sleep(5000),
        wait_for_ntp_synch(Rpc, Timeout - 5)
    end.

%%% ===========================================================================
%%% @doc
%%% Chek that expected error from an netconf action is recvd  <br/>
%%% Res = xml error reply from netconf action. <br/>
%%% ExpErrInfoStr = string(), <br/>
%%% @spec check_exp_reply_err_from_nc_action(Res, ExpErrInfoStr) -> ok
%%% @end
%%% ===========================================================================
check_exp_reply_err_from_nc_action(Res, ExpErrInfoStr) ->
    {'error-message', _, ErrMess} = lists:keyfind('error-message', 1, Res),
    ct:log("ErrMess:~n~p~n",[ErrMess]),
    %%%%
    %% Check err info
    %%%%
    Flatten = lists:flatten(ErrMess),
    ErrStr = string:tokens(Flatten,"[]"),
    ErrInfoStr = lists:last(ErrStr),
    ct:log("ErrInfoStr:~n~p~n",[ErrInfoStr]),
    ErrInfoStr = ExpErrInfoStr,
    ok.

%%% ===========================================================================
%%% @doc
%%% Get highest cxs label  <br/>
%%% UPs = list of CXS strings, from get_ups(?NC_Session),
%%% HighestUP = string , ex "CXS101549/4-R2C98". <br/>
%%% @spec get_highest_label(UPs) -> HighestUP
%%% @end
%%% ===========================================================================
get_highest_label(UPs) ->
    StartUP = lists:nth(1, UPs),
    get_highest_label(UPs, 0, StartUP).
get_highest_label([], _HighestNr, HighestUP) ->
    ct:log("### The Highest UP Label : ~p", [HighestUP]),
    HighestUP;
get_highest_label([UP | T], HighestNr, HighestUP) ->
    [CXS, Rev] = string:tokens(UP,"-"),
    ct:log("### CXS: ~p, Rev: ~p", [CXS, Rev]),

    %% This construck a integer from the version.
    %% Check what integer is the highest to get latest created up label.
    {ok, Number} = version_to_integer(Rev),

    case Number > HighestNr of
        true ->
        NewHighestNr = Number,
        ct:log("### Number: ~p ,  "
           "NewHighestNr: ~p ,  "
           "NewHighestUP: ~p", [Number, NewHighestNr, UP]),
        get_highest_label(T, NewHighestNr, UP);
        false ->
            get_highest_label(T, HighestNr, HighestUP)
    end.


%%% ===========================================================================
%%% @doc
%%% Get Lowest cxs label  <br/>
%%% UPs = list of CXS strings, from get_ups(?NC_Session),
%%% HighestUP = string , ex "CXS101549/4-R2C98". <br/>
%%% @spec get_lowest_label(UPs) -> HighestUP
%%% @end
%%% ===========================================================================
get_lowest_label(UPs) ->
    StartUP = lists:nth(1, UPs),
    get_lowest_label(UPs, "1000000", StartUP). %% Use a very high value to start
get_lowest_label([], _OldestRevNr, OldestUP) ->
    OldestUP;
get_lowest_label([UP | T], OldestRevNr, OldestUP) ->
    [CXS, Rev] = string:tokens(UP,"-"),
    ct:log("### CXS: ~p, Rev: ~p", [CXS, Rev]),
    {_R, RevNr} = lists:split(3, Rev),
    ct:log("### RevNr: ~p ,  "
       "OldestRevNr: ~p, "
       "OldestUP: ~p", [RevNr,
                OldestRevNr,
                OldestUP]),

    case list_to_integer(RevNr) < list_to_integer(OldestRevNr) of
        true ->
        NewOldestRevNr = RevNr,
        ct:log("### RevNr: ~p ,  "
           "NewOldestRevNr: ~p, "
           "NewOldestUP: ~p", [RevNr,
                       NewOldestRevNr,
                       UP]),
        get_lowest_label(T, NewOldestRevNr, UP);
        false ->
            get_lowest_label(T, OldestRevNr, OldestUP)
    end.

%%% ===========================================================================
%%% @doc
%%% @spec wait_for_login(Consol) -> ok
%%% @end
%%% ===========================================================================
wait_for_login(Consol) ->
    wait_for_login(Consol, 90000).
wait_for_login(_Consol, Timeout) when Timeout < 0 ->
    ct:fail("No login prompt within expected time");
wait_for_login(Consol, Timeout) ->
    case ct_telnet:expect(Consol, "login:", 
              [{timeout,30000}, no_prompt_check]) of
    {ok,_} -> 
        ok;
    _Other ->
        ct_telnet:send(Consol, ""),
        timer:sleep(5000),
        wait_for_login(Consol, Timeout-5000)
    end.


%%% ===========================================================================
%%% @doc
%%% @spec disable_max_up_check(Coli) -> ok
%%% @end
%%% ===========================================================================
disable_max_up_check(Coli) ->
    ct:pal("### disable_max_up_check, to allowe create more UPs !",[]),
    ok = rct_coli:connect(Coli),
    timer:sleep(1000),
    {ok,_} = rct_coli:send(Coli,"/misc/authlevel BasebandSupportExpert"), 
    timer:sleep(1000),
    {ok,_} = rct_coli:send(Coli,"/labonly/rcs/disable-max-up-check"),
    timer:sleep(1000),
    rct_coli:disconnect(Coli),
    timer:sleep(1000).

%%% ===========================================================================
%%% @doc
%%% @spec enable_max_up_check(Coli) -> ok
%%% @end
%%% ===========================================================================
enable_max_up_check(Coli) ->
    ct:pal("### enable_max_up_check !",[]),
    ok = rct_coli:connect(Coli),
    timer:sleep(1000),
    {ok,_} = rct_coli:send(Coli,"/misc/authlevel BasebandSupportExpert"), 
    timer:sleep(1000),
    {ok,_} = rct_coli:send(Coli,"/labonly/rcs/enable-max-up-check"),
    timer:sleep(1000),
    rct_coli:disconnect(Coli),
    timer:sleep(1000).

%%% ===========================================================================
%%% @doc
%%% @spec build_same_ug_package(NC_Session, UgHook) -> ok
%%% @end
%%% ===========================================================================
build_same_ug_package(NC_Session, UgHook) ->
    Env = check_env(),
    CXSLABEL = case Env of
		   git ->
		       ct:pal("# New git env is used. #"),
		       CXS_LABEL = ct:get_config({jenkins_config, cxp}),
		       %% CXS_LABEL = "/proj/rcs/DCI/SBC/cache/20170912204503_f0444100262191466d8e6f905f647bd986f37c32/RCS-BB_CXS2010013_2.cxs",
		       CXS_LABEL;
		   _Other ->
		       ct:pal("# CC env is used. #"),
		       CxsLabel = swm_test_lib:get_sw_version(NC_Session),
		       ct:pal("Active version:~n~p~n",[CxsLabel]),
		       %% replace / with _.
		       [CXS, REV] = string:tokens(CxsLabel, "/"),
		       CXS_LABEL = CXS ++ "_" ++ REV,
		       ct:pal("FixLabel:~n~p~n",[CXS_LABEL]),
		       CXS_LABEL
	       end,

    ct:pal("## Active version:~n~p~n",[CXSLABEL]),

    %% Get STP name
    {ok, UGPath} = rct_upgrade:get_up_dir(UgHook),
    ct:log("UGPath:~n~p~n",[UGPath]),
    StpName = lists:last(string:tokens(UGPath, "/")),
    ct:log("StpName: ~p", [StpName]),

    timer:sleep(5000),

    CMD1 = "upgradeprep.sh"
        " -stp " ++ StpName ++
        " " ++ CXSLABEL,
    ct:pal("CMD1:~n~p~n",[CMD1]),
    A = os:cmd(CMD1),
    ct:log("~s", [A]),
    
    CMD2 = "ls /proj/rcs-tmp/upgrade/" ++ StpName ++"/*",
    ct:pal("CMD2:~n~p~n",[CMD2]),
    B = os:cmd(CMD2),
    ct:log("~s", [B]).

%%% ===========================================================================
%%% @doc
%%% @spec build_new_ug_package(NC_Session, UgHook) -> ok
%%% @end
%%% ===========================================================================
build_new_ug_package(NC_Session, UgHook) ->
    %% Get STP name
    {ok, UGPath} = rct_upgrade:get_up_dir(UgHook),
    ct:log("UGPath:~n~p~n",[UGPath]),
    StpName = lists:last(string:tokens(UGPath, "/")),
    ct:log("StpName: ~p", [StpName]),

    CxsLabel = swm_test_lib:get_sw_version(NC_Session),
    ct:pal("Active version:~n~p~n",[CxsLabel]),
    %% replace / with _.
    [CXS, REV] = string:tokens(CxsLabel, "/"),
    CXS_LABEL = CXS ++ "_" ++ REV,
    ct:pal("FixLabel:~n~p~n",[CXS_LABEL]),

    ct:pal("Remove existing files in upgrade dir",[]),
    C_M_D = "chmod 777 /proj/rcs-tmp/upgrade/" ++ StpName ++"/*",
    ct:pal("CMD:~n~p~n",[C_M_D]),
    os:cmd(C_M_D),
    CMD = "rm -rf /proj/rcs-tmp/upgrade/" ++ StpName ++"/*",
    ct:pal("CMD:~n~p~n",[CMD]),
    os:cmd(CMD),

    timer:sleep(5000),

    %% CMD1 = "$RDE_TOP/bin/upgradeprep.sh"
    CMD1 = "/env/RCSDE/bin/upgradeprep.sh"
        " -stp " ++ StpName ++
        " " ++ CXS_LABEL,
    ct:pal("CMD1:~n~p~n",[CMD1]),
    A = os:cmd(CMD1),
    ct:log("~s", [A]),
    
    {ok, Filenames} = file:list_dir(UGPath),
    
    case get_xml_filename(Filenames) of
        no_xml -> ct:fail("No xml file in folder");
        XmlFilename -> {ok, String} = file:read_file(UGPath ++ "/" ++ XmlFilename),
                        {ok, RE} = re:compile("<product.*?version=\".*?(\\d*)\".*?>"),
                        {match,[{_FirstIndex, _FirstOffset},{SecondIndex,SecondOffset}]} = re:run(String, RE),
                        Version = binary:part(String, SecondIndex, SecondOffset),
        
                        NewVersion = integer_to_binary(binary_to_integer(Version) + 1),
                        
                        NewBinaryXml = [binary:part(String, 0, SecondIndex), NewVersion, 
                                        binary:part(String, SecondIndex+SecondOffset, byte_size(String)-SecondIndex-SecondOffset)],
                        ok = file:write_file(UGPath ++ "/" ++ XmlFilename, NewBinaryXml)
    end.

%%% ===========================================================================
%%% @doc
%%% @spec step_down_version(UgHook) -> ok
%%% @end
%%% ===========================================================================
step_down_version(UgHook) ->
    {ok, UGPath} = rct_upgrade:get_up_dir(UgHook),
    {ok, Filenames} = file:list_dir(UGPath),
    
    case get_xml_filename(Filenames) of
    no_xml -> ct:fail("No xml file in folder");
    XmlFilename -> {ok, String} = file:read_file(UGPath ++ "/" ++ XmlFilename),
                    {ok, RE} = re:compile("<product.*?version=\".*?(\\d*)\".*?>"),
                    {match,[{_FirstIndex, _FirstOffset},{SecondIndex,SecondOffset}]} = re:run(String, RE),
                    Version = binary:part(String, SecondIndex, SecondOffset),
    
                    NewVersion = integer_to_binary(binary_to_integer(Version) - 1),
                    
                    NewBinaryXml = [binary:part(String, 0, SecondIndex), NewVersion, 
                                    binary:part(String, SecondIndex+SecondOffset, byte_size(String)-SecondIndex-SecondOffset)],
                    ok = file:write_file(UGPath ++ "/" ++ XmlFilename, NewBinaryXml)
    end.


get_xml_filename([]) ->
    no_xml;
get_xml_filename([Filename|Filenames]) ->
    case filename:extension(Filename) of
        ".xml" -> Filename;
        _ -> get_xml_filename(Filenames)
    end.

%%% ===========================================================================
%%% @doc
%%% @spec set_start_ug_rev_in_ug_top_xml(NC_Session, MeId) -> ok
%%% @end
%%% ===========================================================================
set_start_ug_rev_in_ug_top_xml(NC_Session, MeId) ->
    SW_Vers = swm_test_lib:get_sw_version(NC_Session, MeId),
    ct:pal("SW_Vers:~n~p~n",[SW_Vers]),

    %% Get STP name
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),
    
    %% Modify cxs.xml. Construct a CXS label.
    [CXS, Index, Label] = string:tokens(SW_Vers, "/-"),
    ct:pal("CXS:~p, Index: ~p, Label: ~p ~n",[CXS, Index, Label]),

    %% Set a fake Rev that will not exeed max nr of char in rev. 
    ToVer = "R99A1", %% To match algorthm when get highest rev.
    ct:pal("Start ToVer: ~p~n", [ToVer]),

    Env = check_env(),
    CxsUp = case Env of
		git ->
		    ct:pal("# git #"),
		    XmlFilename = get_cxs_up_xml(UGPath),
		    XmlFilename;
		_Other -> 
		    ct:pal("# cc #"),
		    XmlFilename = "cxs101549_"++Index++"-up.xml",
		    XmlFilename
	    end,
    ct:pal("CxsUp: ~p~n", [CxsUp]),

    %%%% This sed cmd change version on line that match CXS.
    %%%% Changes is done directly in cxs-xml when using -i flag in sed cmd.
    CMD1 = "sed -i -e '/"++CXS++"/s/"++Label++"/"++ToVer++"/' "++UGPath++"/"++
        CxsUp,
    ct:pal("CMD1:~n~p~n",[CMD1]),

    GG = os:cmd(CMD1),
    ct:log("GG:~n~p~n",[GG]).


%%%--------------------------------------------------------------------
%%% Internal
%%%--------------------------------------------------------------------

%%%--------------------------------------------------------------------
%%% Description: wait forFINISHED in  progress results and return results.
%%%--------------------------------------------------------------------
check_progress_elements(ProgressReport) ->
    check_progress_elements(ProgressReport, "FINISHED").

check_progress_elements(ProgressReport, ExpState) ->
    {state,[],[State]} =
    lists:keyfind(state, 1, ProgressReport),
    %% ct:pal("### ~p",[State]),
    ct:pal("# ~p, ~p",[State, ExpState]),
    case State of
     ExpState->
        ct:log("# ~p~n",[ProgressReport]),
        {result,[],[Result]} =
        lists:keyfind(result, 1, ProgressReport),
        %% {actionName,[],[ActionName]} =
        %%  lists:keyfind(actionName, 1, ProgressReport),
        case lists:keyfind(actionName, 1, ProgressReport) of
        {actionName,[],[ActionName]} ->
            ActionName;
        {actionName,[],[]} ->
            ActionName = []
        end,
        {progressInfo,[],[ProgressInfo]} =
        lists:keyfind(progressInfo, 1, ProgressReport),
        case lists:keyfind(resultInfo, 1, ProgressReport) of
        {resultInfo,[],[ResultInfo]} ->
            ResultInfo;
        {resultInfo,[],[]} ->
            ResultInfo = []
        end,
        {state,[],[State]} =
            lists:keyfind(state, 1, ProgressReport),
        {ok, [{Result, ActionName, ProgressInfo, ResultInfo, State,
          ProgressReport}]};
    CurrentState -> %% Ej klar
        {progressPercentage,[],[Percent]} =
        lists:keyfind(progressPercentage, 1, ProgressReport),
        {progressInfo,[],[Info]} =
        lists:keyfind(progressInfo, 1, ProgressReport),
        ct:log("# State: ~s ~p % ready~n~s",[CurrentState,
                             list_to_integer(Percent),
                             Info]),
        loop
    end.

%%%--------------------------------------------------------------------
%%% Description: From a general short xml syntax, extract a certain xml
%%%              element
%%%--------------------------------------------------------------------
extract_element(Element, [{Element, Attribute, Content}|_]) ->
    {ok, {Element, Attribute, Content}};
extract_element(Element, [{Element, Content}|_]) ->
    {ok, {Element, Content}};
extract_element(Element, [{_, _, Content}|Elements]) ->
    case extract_element(Element, Content) of
    {ok, Value} ->
        {ok, Value};
    not_found ->
        extract_element(Element, Elements)
    end;
extract_element(Element, [{_, Content}|Elements]) ->
    case extract_element(Element, Content) of
    {ok, Value} ->
        {ok, Value};
    not_found ->
        extract_element(Element, Elements)
    end;
extract_element(Element, [_|Elements]) ->
    extract_element(Element, Elements);
extract_element(_, []) ->
    not_found.

%%%--------------------------------------------------------------------
%%% Description: Generate a mo key from a label
%%%--------------------------------------------------------------------
make_key([$_|T]) ->
    [$/|make_key(T)];
make_key([X|T]) ->
    [X|make_key(T)];
make_key([]) -> [].

%%%--------------------------------------------------------------------
%%% Description: get_report_progress
%%%--------------------------------------------------------------------
get_report_progress(Session, MeId) ->
    get_report_progress(Session, MeId, 60000).
get_report_progress(_Session, _MeId, Timeout) when Timeout < 0 ->
    ct:fail("No progress report rceived within max timeout.");
get_report_progress(Session, MeId, Timeout) ->
    case ct_netconfc:get(Session,
             {'ManagedElement',
              [{xmlns, "urn:com:ericsson:ecim:ComTop"}],
              [{managedElementId, [], [MeId]},
               {'SystemFunctions',
                [{systemFunctionsId,[],["1"]},
                 {'SwM',
                  [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
                  [{swMId,[],["1"]},
                   {reportProgress, []}
                  ]}]}]}, 30000) of
    {ok,
     [{'ManagedElement',
       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
       [{managedElementId,[],["1"]},
        {'SystemFunctions',[],
         [{systemFunctionsId,[],["1"]},
          {'SwM',
           [{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
           AllAttr} ]}]}]} ->
        %% Filter out reportProgress
        [ReportProgress] = [X || {reportProgress, _ , _} = X <- AllAttr],
        %% ct:pal("ReportProgr: ~p ",[ReportProgress]),
        ReportProgress;
    _Err ->
        ct:log("No progress report recived: ~p ~n"
           "Check nc session. ~n",[_Err]),
        timer:sleep(1000),
        check_nc_session(Session),
        get_report_progress(Session, MeId, Timeout-1000)
    end.

%%%--------------------------------------------------------------------
%%% Description: get_progress_report
%%%--------------------------------------------------------------------
%%%%
%% Shall be used for prepare, verify, activate, confirm.
%%%%
get_report(Session, MeId) ->
    %% {reportProgress,
    %%  [{struct,"AsyncActionProgress"}],
    %%  Report} =
    %%  get_report_progress(Session, MeId),

    Report =
        get_up_report_progress(Session, MeId),

    Report.

get_swm_report(Session, MeId) ->
    {reportProgress,
     [{struct,"AsyncActionProgress"}],
     Report} =
    get_report_progress(Session, MeId),
    Report.


%%%--------------------------------------------------------------------
%%% Description: Netconf
%%%--------------------------------------------------------------------
netconf(Session, F, A) ->
    %% {ok, _} = ct_netconfc:open(Session, []),
    netconf_open(Session, []),
    Res = apply(ct_netconfc, F, [Session | A]),
    %% ok = ct_netconfc:close_session(Session),
    ct_netconfc:close_session(Session),
    Res.


%%%--------------------------------------------------------------------
%%% Description: check_node_state
%%% Node = ErlNode
%%%--------------------------------------------------------------------
check_node_state(Node) ->
    check_node_state(Node, 600000). %% 10 min

check_node_state(Node, Timeout) when Timeout < 0 ->
    ct:pal("ErlNode not up : ~p", [Node]),
    ct:fail("Node not up within max timeout.");

check_node_state(Node, Timeout) ->
    case net_adm:ping(Node) of
    pang ->
        net_kernel:disconnect(Node),
        ct:pal("Nodedown. ~n"
               "sleep and try again."),
            timer:sleep(10000),
            check_node_state(Node, Timeout-10000);
    _Res ->
        net_kernel:disconnect(Node),
        ct:pal("Ping res: ~p", [_Res]),
        ok
    end.

%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
cli_connect(CLI_Session)->
    ok = rct_cli:connect(CLI_Session).
cli_disconnect(CLI_Session)->
    ok = rct_cli:disconnect(CLI_Session).

%%%--------------------------------------------------------------------
%%% Description:
%%%--------------------------------------------------------------------
get_info_to_build_up(NC_Session, UgHook) ->
    %% [SW_Vers | _] = get_ups(NC_Session),
    SW_Vers = get_sw_version(NC_Session),
    ct:log("SW_Vers:~n~p~n",[SW_Vers]),

    %% Construct a CXS label.
    [CXS, Index, Label] = string:tokens(SW_Vers, "/-"),
    ct:log("CXS:~p, Index: ~p, Label: ~p ~n",[CXS, Index, Label]),
    CXS_LABEL = CXS++"_"++Index++"-"++Label,

    %% Get STP name
    {ok, UGPath} = rct_upgrade:get_up_dir(UgHook),
    ct:log("UGPath:~n~p~n",[UGPath]),
    StpName = lists:last(string:tokens(UGPath, "/")),
    ct:log("StpName: ~p", [StpName]),

    B = os:cmd("ls -l "++UGPath),
    C = string:tokens(B,"\n"),
    ct:log("ls -l ~p:~n~p~n",[UGPath, C]),

    {StpName, CXS_LABEL}.


%%%--------------------------------------------------------------------
%%% Description: Cosntruct a integer from the version str.
%%% The integer will increase when newer version is used.
%%% Then this is used to get the latest UP.
%%% In a version string such as R15XYZ23 the "XYZ" part may
%%% consist of 1, 2 or 3 uppercase letters.
%%%--------------------------------------------------------------------
version_to_integer(R) ->
    ct:log("R: ~p", [R]),
    %% RE = "(R[1-9][0-9]*)([A-Z]+)([0-9]*)",
    RE = "([A-Z][0-9][0-9]*)([A-Z]+)([0-9]*)", % In our new expr git env rev could be P0A01.
    
    case re:run(R, RE, [anchored]) of
    nomatch ->
      {nomatch, unspecified};
    {match, [{_, Length}, {Rpos, Rlen}, {Apos, Alen}, {Npos, Nlen}]} ->
      if
        Length < length(R) ->
          {nomatch, garbage_at_end};
        Alen > 3 ->
          {nomatch, too_many_letters};
        true ->
      % Alen is in the range 1..3
          Rpart = string:substr(R, Rpos+2, Rlen-1),
          Apart = string:substr(R, Apos+1, Alen),
          NpartValue =
            if Nlen =:= 0 ->
              0;
            true ->
              1 + list_to_integer(string:substr(R, Npos+1, Nlen))
            end,
          if
            NpartValue > 9999 ->
              {error, out_of_range};
            true ->
          RpartValue = list_to_integer(Rpart)*100*100*100*10000,
          ApartValue =
        case Apart of
          [Z] ->
            (Z-64)*10000;
          [Y, Z] ->
            ((Y-64)*100 + (Z-64))*10000;
          [X, Y, Z] ->
            (((X-64)*100 + (Y-64))*100 + (Z-64))*10000
        end,
          {ok, RpartValue + ApartValue + NpartValue}
          end
      end
  end.


%% %% Return sim,not_sec_card or sec_card
check_kdu()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    case ct:get_config({list_to_atom(Hw),secure_board}) of
    "yes" ->
        sec_card;
    _Other ->
        not_sec_card
    end.

netconf_open(Session, Param)->
    case check_kdu()  of
    TARGET when TARGET == sim;
            TARGET == not_sec_card -> ct_netconfc:open(Session,Param);
    sec_card ->  ct_netconfc:open(Session, [{user, "SysAdminTest"}, {password, "SysAdminTest"}|Param])
    end.



mod_ug_package_1(CXS_LABEL, UgHook) ->
    {ok, UGPath} = rct_upgrade:get_up_dir(UgHook),
    ct:log("UGPath:~n~p~n",[UGPath]),
    StpName = lists:last(string:tokens(UGPath, "/")),
    ct:log("StpName: ~p", [StpName]),
    ct:log("CXS_LABEL: ~p", [CXS_LABEL]),

    %% CMD = "$RDE_TOP/bin/upgradeprep.sh"
    CMD = "upgradeprep.sh"
        " -stp " ++ StpName ++
        " " ++ CXS_LABEL,
    ct:pal("CMD:~n~p~n",[CMD]),
    A = os:cmd(CMD),
    ct:log("~s", [A]),

    {ok, Filenames} = file:list_dir(UGPath),
    ct:pal("Filenames: ~p",[Filenames]),
    XmlFilename = case get_xml_filename(Filenames) of
		      no_xml -> ct:fail("No xml file in folder");
		      Filename -> Filename
		  end,
    ct:pal("XmlFilename: ~p",[XmlFilename]),
    [SearchStr | _] = string:tokens(XmlFilename,"-up"),
    ct:pal("SearchStr: ~p",[SearchStr]),

    GrepRes = os:cmd("grep -i -m 1 "++SearchStr++" "++UGPath++"/"++XmlFilename),
    ct:pal("GrepRes: ~p",[GrepRes]),
    ModGrepRes = string:tokens(GrepRes,"<>\" \n"),
    ct:pal("ModGrepRes: ~p",[ModGrepRes]),

    ["version=", FromVer | _] = lists:dropwhile(fun(X) ->
				      X =/= "version="
			   end, ModGrepRes),
    ct:pal("FromVer: ~p",[FromVer]),

    ToVer = construct_to_ver(FromVer),
    ct:pal("ToVer: ~p",[ToVer]),
    timer:sleep(2000),

    ["id=", CXS | _] = lists:dropwhile(fun(X) ->
				      X =/= "id="
			   end, ModGrepRes),
    
    %%%% This sed cmd change version on line that match CXS.
    %%%% Changes is done directly in cxs-xml when using -i flag in sed cmd.
    CMD1 = "sed -i -e '/"++CXS++"/s/"++FromVer++"/"++ToVer++"/' "++UGPath++"/"++XmlFilename,
    ct:pal("CMD1:~n~p~n",[CMD1]),
    GG = os:cmd(CMD1),
    ct:pal("GG:~n~p~n",[GG]),
    timer:sleep(2000),

    CMD2 = os:cmd("cat "++UGPath++"/"++XmlFilename),
    ct:log("cat cxs:~n~p~n",[string:tokens(CMD2, "\n\" ")]),

    %% test_server:break("E"),
    ok.

get_created_up(Session, MeId) ->
    check_nc_session(Session),
    ReportProgress = get_swm_report(Session, MeId),
    ct_netconfc:close_session(Session),
    ct:pal("AAA ReportProgress:~n~p~n",[ReportProgress]),
    ResultInfo = [X || {resultInfo, _ , _} = X <- ReportProgress],
    ct:pal("BBB ResultInfo:~n~p~n",[ResultInfo]),
    [{resultInfo,[],[UpgradePackage]}] = ResultInfo,
    ["UpgradePackage", CreatedUp] = string:tokens(UpgradePackage, "="),
    ct:pal("CCC CreatedUp:~n~p~n",[CreatedUp]),
    CreatedUp.


construct_to_ver(FromVer) ->
    ct:pal("# FromVer: ~p",[FromVer]),
    ReverseFromVer = lists:reverse(FromVer),
    ct:pal("# ReverseFromVer: ~p",[ReverseFromVer]),


    {Reversed_EndVer, Reversed_StartVer} = lists:splitwith(fun(X) ->
				      ct:pal("X: ~p", [[X]]),
    				  case re:run([X], "^[0-9]") of
    				      {match, _} ->
    				  	  true;
    				      nomatch ->
    				  	  false
    				  end
    			  end, ReverseFromVer),
    ct:pal("# Reversed_EndVerList: ~p",[Reversed_EndVer]),
    ct:pal("# Reversed_StartVerList: ~p",[Reversed_StartVer]),
    
    OrgStartVer = lists:reverse(Reversed_StartVer),
    ct:pal("# OrgStartVer: ~p",[OrgStartVer]),

    OrgEndVer = lists:reverse(Reversed_EndVer),
    ct:pal("# OrgEndVerList: ~p",[OrgEndVer]),
    
    NewEndVer = integer_to_list(list_to_integer(OrgEndVer) + 1),
    ct:pal("# NewEndVer: ~p",[NewEndVer]),
    
    %% Check if 0x combination is used,  hen keep the 0.
    New_EndVer = case list_to_integer(NewEndVer) of
			     W when W < 10 ->
				 "0"++NewEndVer;
			     _Else ->
				 NewEndVer
			 end,
    ct:pal("# New_EndVer: ~p",[New_EndVer]),

    New_Version = OrgStartVer++New_EndVer,
    ct:pal("# New_Version: ~p",[New_Version]),
    New_Version.
    		  
