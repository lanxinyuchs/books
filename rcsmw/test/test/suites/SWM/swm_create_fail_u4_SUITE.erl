%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swm_create_fail_u4_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R11A/1
%%% 
%%% @doc == Test Suite for Upgrade Mechanism when create fails. Create fails due to metadatafile is not correct syntax. ==
%%% <br/><br/>
%%% @end

-module(swm_create_fail_u4_SUITE).
-vsn('/main/R2A/R3A/R4A/R11A/1').

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
%%% R2A/2      2014-02-05 etxivri     Created
%%% R2A/3      2014-06-12 etxivri     Update due to swm changes of reportprogr.
%%% R2A/4      2014-07-09 etxivri     Update exp result info and progress info.
%%% R3A/1      2015-05-28 etxivri     Remove hard coded sftp server data.
%%% R3A/2      2015-05-28 etxkols     Changed rct_netconf hook format 
%%% R4A/1      2015-10-20 etxivri     Update error filter.     
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
-export([create_fail_due_to_fault_metadata/1
	]).

-define(SftpHost, swm_test_lib:get_sftp_host() ).
-define(SftpUser , swm_test_lib:get_sftp_user() ).
-define(SftpPassword , swm_test_lib:get_sftp_password() ).

-define(ActionName, "createUpgradePackage").
-define(ProgressInfo, "The action could not be completed").
-define(ResultInfo, "A software related error occured").

-define(NC_Session, nc1).

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
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
					   "CRASH REPORT"],
					  [%% "exit"
					   "unexpected_end_of_STag"
					  ]
					 }}]}},
		 {rct_netconf,nc1}]}].

%% @hidden
init_per_suite(Config) ->
    ct:log("# init per suite. ~n"
    	   "Create cxps that shall be used for UG."), 
    swm_test_lib:build_valid_ug_packakage(?NC_Session, ug1),

    %%%%
    %% Create a faulty metadata file. 
    %% Remove last character in created cxs101549 xml.
    %%%%
    %% 1. Remove last character from cxs and copy to tmp file.
    {ok, UGPath} = rct_upgrade:get_up_dir(ug1),
    XmlFilename = swm_test_lib:get_cxs_up_xml(UGPath),

    CMD1 = "sed '$s/.$//' " ++ 
    	UGPath ++ 
    	"/"++XmlFilename++" > " ++ 
    	UGPath ++ 
    	"/tmp_cxs.xml",
    ct:log("CMD:~n~p~n",[CMD1]),
    os:cmd(CMD1),

    %% 2. cp tmp file to cxs file.
    CMD2 = "cp " ++ 
    	UGPath ++ 
    	"/tmp_cxs.xml " ++ 
    	UGPath ++ "/"++XmlFilename,
    ct:log("CMD:~n~p~n",[CMD2]),
    os:cmd(CMD2),

    %% 3. rm tmp file.
    CMD3 = "rm " ++ UGPath ++ "/tmp_cxs.xml",
    os:cmd(CMD3),

    Config.
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(_TestCase, _Config) ->
    ok.


%%%--------------------------------------------------------------------
%%% @doc Runs all testcases in SUITE.
%%% @end
%%%--------------------------------------------------------------------
all() -> 
    [create_fail_due_to_fault_metadata
    ].

%%%--------------------------------------------------------------------
%%% @doc 
%%% @end
%%%--------------------------------------------------------------------
create_fail_due_to_fault_metadata(_Config) ->
    ct:pal("UG create fails due to faulty metadata file is used",[]),

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
					?SftpPassword, 
					UGPath, 
					MeId),

    case swm_test_lib:
    	wait_for_swm_progress_result(?NC_Session, MeId, OldActionId) of
    	[{"FAILURE", ?ActionName, ?ProgressInfo, ?ResultInfo, 
    	  _State, _ProgReport}] ->
    	    ct:log("result:~p~n"
    		   "actionName:~p~n"
    		   "progressInfo:~p~n"
    		   "resultInfo:~p~n",["FAILURE", 
    				      ?ActionName, 
    				      ?ProgressInfo, 
    				      ?ResultInfo]),
    	    ok;
    	Result ->
    	    ct:pal("createUpgradePackage: ~p",[Result]),
    	    ct:fail(Result)
    end.
