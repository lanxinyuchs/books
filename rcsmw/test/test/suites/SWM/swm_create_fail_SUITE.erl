%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_create_fail_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R11A/1
%%% 
%%% @doc == Test Suite for Upgrade Mechanism when create fails. ==
%%% <br/><br/>
%%% @end

-module(swm_create_fail_SUITE).
-vsn('/main/R2A/R3A/R4A/R5A/R11A/1').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% R2A/2      2013-12-13 etxivri     Created
%%% R2A/3      2014-01-10 etxivri     Added create create of cxps used for UG.
%%%                                   This is runed in init_per_suite.
%%% R2A/4      2014-01-16 etxivri     Update due to updates in swm_test_lib
%%% R2A/5      2014-01-27 etxivri     Use lib function to build UP.
%%% R2A/5      2014-02-06 etxivri     Add rct_upgrade hook name when create up.
%%% R2A/7      2014-02-24 etxivri     Add new TC u1. renamed other TCs, 
%%%                                   Update of edoc. 
%%% R2A/8      2014-03-21 etxivri     Due to TR HS43611 and HS43738, this tc has
%%%                                   been updated. Note fix this when TRs is 
%%%                                   corrected.
%%% R2A/9      2014-04-11 erarafo     Adjusted for adjustment in swmServer
%%% R2A/10     2014-04-14 erarafo     Update due to TR HS43611 and HS43738 
%%%                                   is solved
%%% R2A/11     2014-06-12 etxivri     Update due to swm changes of reportprogr.
%%% R2A/12     2014-07-09 etxivri     Update expected reultinfo in u1 tc
%%% R2A/13     2014-07-11 etxivri     Update expected reultinfo in u1 tc
%%% R2A/14     2014-09-09 etxivri     Added permission denied on a file tc.
%%% R3A/1      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/2      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R4A/1      2015-07-01 etxivri     Updete due to new behaviour.
%%% R5A/1      2016-01-26 etxivri     Update due to new behaviour.
%%% R5A/2      2016-07-12 etxivri     Update due to new behaviour.
%%% R11A/2     2017-09-05 etxivri     Update for git
%%% ----------------------------------------------------------

%-compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 %% upgr_create_generic/2, %% Usae as lib
	 all/0]).
-export([u1_fault_no_metadatfile_exist/1,
	 u2_fault_sftp_pswd/1,
	 u2_fault_sftp_user/1,
	 u7_permission_denied/1
	]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).

-define(FaultPassword, "diilbert").
-define(FaultUser, "mauvee").
-define(ActionName, "createUpgradePackage").
-define(ProgressInfo, "The action could not be completed").
-define(ResultInfo_U1, "Could not find an upgrade package").
%% -define(ResultInfo_U1, "UP metadata file not found").
%% -define(ResultInfo_U2, "Unable to connect using the available authentication methods").
%% -define(ResultInfo_U2, "Too many authentication failures for dustest").
%% -define(ResultInfo_U3, "Too many authentication failures for mauvee").
-define(ResultInfo_U2, "Unable to connect using the available authentication methods").
-define(ResultInfo_U3, "Unable to connect using the available authentication methods").

-define(NC_Session, nc1).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_htmllink,[]},
		 {rct_rpc, rpc_1},
		 {rct_upgrade,ug1},
		 {cth_conn_log, []},
		 {rct_core,[]},
                 {rct_logging, {upgrade, 
				[{erlang,{["ERROR REPORT",
					   "CRASH REPORT"
					  ],
					  [
					   "swmServer: throw \"UP metadata file not found\"",
					   "Unable to connect using the available authentication methods"
					  ]
					 }}]}},
		 {rct_netconf,nc1}]}].

%% @hidden
init_per_suite(Config) ->
    ct:log("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 
    swm_test_lib:build_valid_ug_packakage(?NC_Session, ug1),
    Config.
%% @hidden
end_per_suite(_Config) ->
    ok.

%% @hidden
init_per_testcase(u7_permission_denied, Config) ->
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:log("UGPath:~n~p~n",[UGPath]),
    %% set Only the owner has read and write permissions.
    os:cmd("chmod 600 " ++ UGPath++"/*-up.xml"),
    Ls = os:cmd("ls -l " ++ UGPath++"/*-up.xml"),
    ct:log("ls:~p~n",[Ls]),
    Config;
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(u7_permission_denied, _Config) ->
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:log("UGPath:~n~p~n",[UGPath]),
    %% set that owner and groupl has read and write permissions.
    os:cmd("chmod 664 " ++ UGPath++"/*-up.xml"),
    Ls = os:cmd("ls -l " ++ UGPath++"/*-up.xml"),
    ct:log("ls:~p~n",[Ls]),
    ok;
end_per_testcase(_TestCase, _Config) ->
    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% Run u1 after U2 due to removal of cxs*.xml.
%%% @end
%%%--------------------------------------------------------------------
all() -> 
    [u2_fault_sftp_pswd,
     u2_fault_sftp_user,
     u7_permission_denied,
     u1_fault_no_metadatfile_exist
    ].


%%%--------------------------------------------------------------------
%%% @doc Create an upgrade package when no UP metadatafile (CXS.xml)  <br/>
%%% exist in given path. <br/>
%%% The result shall be FAILURE. <br/>
%%% Note ! Run u1 after U2 due to removal of cxs*.xml <br/>
%%% @spec u1_fault_no_metadatfile_exist(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
u1_fault_no_metadatfile_exist(_Config) ->
    ct:pal("UG create fails due to no metadata file exist",[]),

    Up = rct_rpc:call(rpc_1, swmI, get_current_up_metadata, [], 10000, noprint),
    ct:pal("Current UP:~n~p~n",[Up]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:pal("UGPath:~n~p~n",[UGPath]),
    A=os:cmd("rm "++UGPath++"/cxs*.xml"),
    ct:pal("A:~n~p~n",[A]),

    OldActionId = swm_test_lib:get_swm_action_id(?NC_Session, MeId),
    ok = swm_test_lib:up_create_generic(?NC_Session, 
					?SftpHost, 
					?SftpUser, 
					?SftpPassword, 
					UGPath, 
					MeId),

    case swm_test_lib:
    	wait_for_swm_progress_result(?NC_Session, MeId, OldActionId) of
    	[{"FAILURE" = Result, 	  
    	  ?ActionName = ActionName, 
    	  ?ProgressInfo = ProgressInfo, 
    	  ?ResultInfo_U1 = ResultInfo, 
    	  _State, _ProgReport}] ->
    	    ct:pal("result:~p~n"
    		   "actionName:~p~n"
    		   "progressInfo:~p~n"
    		   "resultInfo:~p~n",[Result, 
    				      ActionName, 
    				      ProgressInfo, 
    				      ResultInfo]),
    	    ok;
    	Result ->
    	    ct:pal("createUpgradePackage: ~p",[Result]),
    	    ct:fail(Result)
    end.

%%%--------------------------------------------------------------------
%%% @doc Create an upgrade package when sftp password is wrong. <br/>
%%% The result shall be FAILURE.<br/>
%%% @spec u2_fault_sftp_pswd(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
u2_fault_sftp_pswd(_Config) ->
    ct:pal("UG create fails due to wrong password to sftp is used",[]),

    Up = rct_rpc:call(rpc_1, swmI, get_current_up_metadata, [], 10000, noprint),
    ct:pal("Current UP:~n~p~n",[Up]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:log("UGPath:~n~p~n",[UGPath]),

    OldActionId = swm_test_lib:get_swm_action_id(?NC_Session, MeId),
    ok = swm_test_lib:up_create_generic(?NC_Session, 
					?SftpHost, 
					?SftpUser, 
					?FaultPassword, 
					UGPath, 
					MeId),

    case swm_test_lib:
	wait_for_swm_progress_result(?NC_Session, MeId, OldActionId) of
	[{"FAILURE" = Result,
	  ?ActionName = ActionName, 
	  ?ProgressInfo = ProgressInfo, 
	  ?ResultInfo_U2 = ResultInfo, 
	  _State, _ProgReport}] ->
	    ct:log("result:~p~n"
		   "actionName:~p~n"
		   "progressInfo:~p~n"
		   "resultInfo:~p~n",[Result,
				      ActionName, 
				      ProgressInfo, 
				      ResultInfo]),
	    ok;
	Result ->
	    ct:pal("createUpgradePackage: ~p",[Result]),
	    ct:fail(Result)
    end.

%%%--------------------------------------------------------------------
%%% @doc Create an upgrade package when sftp user is wrong. <br/>
%%% The result shall be FAILURE.<br/>
%%% @spec u2_fault_sftp_user(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
u2_fault_sftp_user(_Config) ->
    ct:pal("UG create fails due to wrong user to sftp is used",[]),

    Up = rct_rpc:call(rpc_1, swmI, get_current_up_metadata, [], 10000, noprint),
    ct:pal("Current UP:~n~p~n",[Up]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:log("UGPath:~n~p~n",[UGPath]),

    OldActionId = swm_test_lib:get_swm_action_id(?NC_Session, MeId),
    ok = swm_test_lib:up_create_generic(?NC_Session, 
					?SftpHost, 
					?FaultUser, 
					?SftpPassword, 
					UGPath, 
					MeId),

    case swm_test_lib:
	wait_for_swm_progress_result(?NC_Session, MeId, OldActionId) of
	[{"FAILURE" = Result,
 	  ?ActionName = ActionName, 
	  ?ProgressInfo = ProgressInfo, 
	  ?ResultInfo_U3 = ResultInfo, 
	  _State, _ProgReport}] ->
	    ct:log("result:~p~n"
		   "actionName:~p~n"
		   "progressInfo:~p~n"
		   "resultInfo:~p~n",[Result,
				      ActionName, 
				      ProgressInfo, 
				      ResultInfo]),
	    ok;
	Result ->
	    ct:pal("createUpgradePackage: ~s",[Result]),
	    ct:fail(Result)
    end.

%%%--------------------------------------------------------------------
%%% @doc Create an upgrade package when permission denied on a file. <br/>
%%% The result shall be FAILURE.<br/>
%%% @spec u7_permission_denied(_Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
u7_permission_denied(_Config) ->
    ct:pal("UG create fails due to permission denied on *up-.xml",[]),

    %%%%
    %% Get managedElementId
    %%%%
    MeId = swm_test_lib:get_me_id(?NC_Session),

    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    ct:log("UGPath:~n~p~n",[UGPath]),

    OldActionId = swm_test_lib:get_swm_action_id(?NC_Session, MeId),
    ok = swm_test_lib:up_create_generic(?NC_Session, 
					?SftpHost, 
					?SftpUser, 
					?SftpPassword, 
					UGPath, 
					MeId),

    case swm_test_lib:
	wait_for_swm_progress_result(?NC_Session, MeId, OldActionId) of
	[{"FAILURE" = Result,
 	  ?ActionName = ActionName, 
	  ?ProgressInfo = ProgressInfo, 
	  %% "Permission denied:*" = ResultInfo, 
	  ResultInfo,
	  _State, _ProgReport}] ->
	    ct:log("result:~p~n"
		   "actionName:~p~n"
		   "progressInfo:~p~n"
		   "resultInfo:~p~n",[Result,
				      ActionName, 
				      ProgressInfo, 
				      ResultInfo]),
	    ok;
	Result ->
	    ResultInfo = dummy,
	    ct:pal("createUpgradePackage: ~p",[Result]),
	    ct:fail(Result)
    end,
    
    %% Check exp str exist in ResultInfo
    ExpStr = "Permission denied",
    case re:run(ResultInfo, ExpStr) of
	{match, _} ->
	    ok;
        _ ->
	    ct:fail("TC will fail due to exp string not found in Resultinfo")
    end.
	

%%%--------------------------------------------------------------------
%%% Internal Functions
%%%--------------------------------------------------------------------
