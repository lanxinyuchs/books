%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nl_upgrade_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R6A/R7A/R9A/1
%%%
%%% @doc == AI NL upgrade. ==
%%%
%%%
%%% @end

-module(nl_upgrade_SUITE).
-author('etxmlar').
-vsn('/main/R6A/R7A/R9A/1').
-date('2017-01-26').

%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% Rev       Date        Name        What
%%% -----     ---------   --------    ------------------------
%%% R6A/1     2016-09-19  etxmlar     Created
%%% R6A/2     2016-09-20  etxmlar     Updated and checked in
%%% R7A/1     2016-10-06  etxmlar     Updated for TCU
%%% R7A/2     2016-10-28  etxmlar     Remove TC from all()
%%% R7A/3     2016-10-31  etxmlar     Added TC nl_upgade_003
%%% R9A/1     2017-01-26  etxmlar     Added support for new boardtypes
%%% ----------------------------------------------------------

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 groups/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 nl_upgade_001/1,
	 nl_upgade_002/1,
	 nl_upgade_003/1
	]).

-define(NC, nc1).
-define(Protocol, "https").
-define(DefaultPort, "8080").
-define(DUS_UP_WITH_OLD_NL, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_5/R2DLD/CXP9024418_5-R2DLD.zip"). %%NL version CNX9012629-R6S04
-define(TCU_UP_WITH_OLD_NL, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_5/R2GSX/CXP9024419_5-R2GSX.zip"). %%NL version CNX9012629-R6S04
-define(C608_OLD_UP_NL_VERSION_R8N04, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_6/R5A278/CXP9024419_6-R5A278.zip").  %%NL version CNX9012629-R8N04

-define(DUS3_OLD_UP_NL_VERSION_R8N04, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_6/R4A241/CXP9024418_6-R4A241.zip").  %%NL version CNX9012629-R8N04

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() ->
    case nl_lib:check_if_vc_board() of
	"yes" -> 
	    [{timetrap,{minutes,60}},
	     {ct_hooks, [{rct_htmllink,[]},
			 {rct_power,power},
			 {rct_consserv,cs1},
			 {rct_netconf, {nc1, man_auth}},
			 {rct_ssh,{ssh,[manual_connect]}},
			 {rct_power,node},
			 {rct_rs232,console},
			 {cth_conn_log,[]}]}];
	_Other ->
	    [{timetrap, {minutes, 600}}, % 10 hours
	     {ct_hooks, [{rct_rpc, rpc},
			 {rct_htmllink,[]},
			 {rct_consserv,cs1},
			 {rct_rs232,console},
			 {rct_power,node},
			 {rct_netconf, nc1},
			 {cth_conn_log, []},
			 {rct_logging, {all,
			 		[{erlang,
			 		  {["ERROR REPORT","CRASH REPORT"],
			 		   []}
			 		 }]}},
			 {rct_core,[]},
			 {rct_cli, {cli, [manual_connect]}}
			]}]
    end.


%% @hidden
init_per_suite(Config) ->

    HW = atom_to_list(ct:get_config({test_nodes,1})),
    TftpBootDir = "/proj/rcs-tmp/tftpboot/"++HW++"/",
    ct:pal("TftpBootDir: ~p ", [TftpBootDir]),

    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:pal("ls : ~p ", [Ls]),

    TftpUP = get_node_up(TftpBootDir),
    ct:log("Tftp UP : ~p ", [TftpUP]),

    [{tftpboot_dir, TftpBootDir},
     {tftp_up, TftpUP},
     {hw, HW}| Config].

   

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
init_per_testcase(TestCase, Config)-> 

    ct:log("TestCase: ~p",[TestCase]),
    NL_Upgrade_File = "RbsSummaryFile.xml.nl",
    HW = proplists:get_value(hw, Config),
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    Org_UP_File = proplists:get_value(tftp_up, Config),

    %% create tmp dir in priv_dir
    TmpPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++TmpPath),
    os:cmd("mkdir "++TmpPath++"tmp"),
    TmpDir =TmpPath++"tmp/",
    ct:pal("TmpDir: ~p", [TmpDir]),

    case TestCase of
	nl_upgade_001 ->

	    %% cp original UP to priv_dir to use in end_per_testcase
	    copy(Org_UP_File, TftpBootDir, TmpDir),

	    %% run rcstprep.sh for old UP
	    BoardType = get_board_type(),
	    case BoardType of
		BoardType when BoardType == "tcu04";				
			       BoardType == "tcu0401" ->

		    RcstPrep_CMD = "rcstprep.sh "++HW++" "++?TCU_UP_WITH_OLD_NL,
		    ct:log("RcstPrep_CMD: ~p", [RcstPrep_CMD]),
		    os:cmd(RcstPrep_CMD),

		    %% cp old UP to priv_dir to use in unpacking the UP to find NL version
		    Old_UP_File = filename:basename(?TCU_UP_WITH_OLD_NL),
		    copy(Old_UP_File, TftpBootDir, TmpDir);

		BoardType when BoardType == "dus5201";				
			       BoardType == "dus3201" ->

		    RcstPrep_CMD = "rcstprep.sh "++HW++" "++?DUS_UP_WITH_OLD_NL,
		    ct:log("RcstPrep_CMD: ~p", [RcstPrep_CMD]),
		    os:cmd(RcstPrep_CMD),

		    %% cp old UP to priv_dir to use in unpacking the UP to find NL version
		    Old_UP_File = filename:basename(?DUS_UP_WITH_OLD_NL),
		    copy(Old_UP_File, TftpBootDir, TmpDir);

		BoardType when BoardType == "dus5301";				
			       BoardType == "dus3301";
			       BoardType == "dus6303";
			       BoardType == "dus6502" ->

		    RcstPrep_CMD = "rcstprep.sh "++HW++" "++?DUS3_OLD_UP_NL_VERSION_R8N04,
		    ct:log("RcstPrep_CMD: ~p", [RcstPrep_CMD]),
		    os:cmd(RcstPrep_CMD),

		    %% cp old UP to priv_dir to use in unpacking the UP to find NL version
		    Old_UP_File = filename:basename(?DUS3_OLD_UP_NL_VERSION_R8N04),
		    copy(Old_UP_File, TftpBootDir, TmpDir);

		BoardType when BoardType == "c608" -> 
		    RcstPrep_CMD = "rcstprep.sh "++HW++" "++?C608_OLD_UP_NL_VERSION_R8N04,
		    ct:log("RcstPrep_CMD: ~p", [RcstPrep_CMD]),
		    os:cmd(RcstPrep_CMD),

		    %% cp old UP to priv_dir to use in unpacking the UP to find NL version
		    Old_UP_File = filename:basename(?C608_OLD_UP_NL_VERSION_R8N04),
		    copy(Old_UP_File, TftpBootDir, TmpDir)
	    end;

	nl_upgade_002 ->
	    %% cp original UP to priv_dir to use in unpacking the UP to find NL version
	    copy(Org_UP_File, TftpBootDir, TmpDir);
	nl_upgade_003 ->
	    %% cp original UP to priv_dir to use in unpacking the UP to find NL version
	    copy(Org_UP_File, TftpBootDir, TmpDir)
    end,

    LS_TmpDir = string_token(os:cmd("ls "++TmpDir)),
    ct:log("Ls TmpDir: ~p", [LS_TmpDir]),

    [{nlupgradefile, NL_Upgrade_File},
     {tmp_dir, TmpDir}
     | Config].



%% @hidden
end_per_testcase(TestCase, Config)->

    ct:log("TestCase: ~p",[TestCase]),

    case TestCase of
	nl_upgade_001 ->

	    %% run rcstprep.sh on orginal UP
	    HW = proplists:get_value(hw, Config),
	    TmpDir = proplists:get_value(tmp_dir, Config),
	    Filename = proplists:get_value(tftp_up, Config),  
	    Orignal_UP = filename:join(TmpDir, Filename),

	    %% run rcstprep.sh on orginal UP in priv_dir
	    RcstPrep_CMD = "rcstprep.sh "++HW++" "++Orignal_UP,
	    ct:log("RcstPrep_CMD: ~p", [RcstPrep_CMD]),
	    os:cmd(RcstPrep_CMD);

	nl_upgade_002 ->
	    continue;
	nl_upgade_003 ->
	    continue
    end,    

    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p. ~n export ai log. \n",[Reason]),
    	    nl_lib:export_ai_log(Config, ?Protocol)
    end,
    ok.


string_token(Str) ->
    ListOfStr = string:tokens(Str, "\n"),
    ListOfStr.

copy(Filename, Fromdir, Todir) ->
    file:copy(filename:join(Fromdir, Filename), 
	      filename:join(Todir, Filename)).

%%--------------------------------------------------------------------
%% @doc
%% groups.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [
     {upgrade_nl,[],[nl_upgade_001,
     		     nl_upgade_002
     		    ]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [nl_upgade_003
    ].

%%%--------------------------------------------------------------------
%%% @doc Download and Integrate no check
%%% @spec nl_upgade_001(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nl_upgade_001(Config) ->
    ct:log("nl_upgade_001:~n"
	   "Run on both Sec/UnSec board. Using nl upgrade file with tag initialSwFilePath ~n"
	   "Download -> Reboot on new NL"),

    %% Check NL version in UP before TC starts. To verify against
    BoardType = get_board_type(),

    UpNLVerStr = 
	case BoardType of
	    "tcu04" ->

		UP = filename:basename(?TCU_UP_WITH_OLD_NL),
		TcuUpNLVerStr = get_nl_version_up(Config, UP),
		ct:log("Network Loader Version in UP: ~p",[TcuUpNLVerStr]),
		TcuUpNLVerStr;

	    "tcu0401" ->

		UP = filename:basename(?TCU_UP_WITH_OLD_NL),
		TcuUpNLVerStr = get_nl_version_up(Config, UP),
		ct:log("Network Loader Version in UP: ~p",[TcuUpNLVerStr]),
		TcuUpNLVerStr;

	    _Other ->		  
		UP = filename:basename(?DUS_UP_WITH_OLD_NL),
		DusUpNLVerStr = get_nl_version_up(Config, UP),
		ct:log("Network Loader Version in UP: ~p",[DusUpNLVerStr]),
		DusUpNLVerStr
	end,

   
    %%%%
    %% Precondition: Board restore.
    %%%%
    ct:log("Precondition: perform board restore."),
    board_restore(Config),

    %%%%
    %% Upgrade NL
    %%%%
    
    %% NL_Upgrade_File = "RbsSummaryFile.xml.nl",
    NL_Upgrade_File=  proplists:get_value(nlupgradefile, Config),
    nl_upgrade(Config, UpNLVerStr, NL_Upgrade_File),
    
    %% reinstall node
    reinstall_node(Config),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Download and Integrate no check
%%% @spec nl_upgade_002(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nl_upgade_002(Config) ->
    ct:log("nl_upgade_002:~n"
	   "Run on both Sec/UnSec board. Using nl upgrade file with tag initialSwFilePath ~n"
	   "Download -> Reboot on new NL"),

    %% Check NL version in UP before TC starts. To verify against
    UP = proplists:get_value(tftp_up, Config), 
    UpNLVerStr = get_nl_version_up(Config, UP),
    ct:log("Network Loader Version in UP: ~p",[UpNLVerStr]),

    
    %%%%
    %% Precondition: Board restore.
    %%%%
    ct:log("Precondition: perform board restore."),
    board_restore(Config),

    %%%%
    %% Upgrade NL
    %%%%
    
    %% NL_Upgrade_File = "RbsSummaryFile.xml.nl",
    NL_Upgrade_File=  proplists:get_value(nlupgradefile, Config),
    nl_upgrade(Config, UpNLVerStr, NL_Upgrade_File),
    
    %% reinstall node
    reinstall_node(Config),

    ok.

%%%--------------------------------------------------------------------
%%% @doc Download and Integrate no check
%%% @spec nl_upgade_003(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nl_upgade_003(Config) ->
    ct:log("nl_upgade_003:~n"
	   "Run on both Sec/UnSec board. Using nl upgrade file with tag initialSwFilePath ~n"
	   "Download -> Reboot on new NL"),

    %% Check NL version in UP before TC starts. To verify against
    UP = proplists:get_value(tftp_up, Config), 
    UpNLVerStr = get_nl_version_up(Config, UP),
    ct:log("Network Loader Version in UP: ~p",[UpNLVerStr]),

    %%%%
    %% Precondition: Board restore.
    %%%%
    ct:log("Precondition: perform board restore."),
    board_restore(Config),

    %%%%
    %% Upgrade NL
    %%%%
    
    %% NL_Upgrade_File = "RbsSummaryFile.xml.nl",
    NL_Upgrade_File=  proplists:get_value(nlupgradefile, Config),
    nl_upgrade(Config, UpNLVerStr, NL_Upgrade_File),
    
    %% reinstall node
    reinstall_node(Config),

    ok.


%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
%% hard_factory_reset(Config) ->
%%     aic_httpc:hard_factory_reset(Config, console),
%%     ct:log("Board is hard factory reset. Now test can start."),
%%     ok.

board_restore(Config) ->
    aic_httpc:board_restore(Config, console),
    ct:log("Board is restored. Now test can start."),
    timer:sleep(5000).

nl_upgrade(Config, UpNLVerStr, NL_Upgrade_File)->
    ct:pal("UpNLVerStr: ~p", [UpNLVerStr]),
    ok = nl_lib:download_file_nl_upgrade(Config, console, NL_Upgrade_File, no_local_file, UpNLVerStr).

get_node_up(TftpBootDir)->

    UpfileList = filelib:wildcard(TftpBootDir++"*.{tgz,cxs,zip}"),
    UpfileString = lists:flatten(UpfileList),
    Up = filename:basename(UpfileString),
    Up. 

get_nl_version_up(Config, TftpUP) ->
    
    TmpDir = proplists:get_value(tmp_dir, Config), 

    %% cd tempdir to find NL version in UP (nl_product_version.txt)
    ok = file:set_cwd(TmpDir),
    FileExt = filename:extension(TftpUP),
    
    CMD_prefix = 
	case FileExt of
	    ".cxs" ->
		%% "RCP*cxs "; 
		"tar -Ozxf "++TftpUP++" --wildcards RCS-*.cxp";
	    ".tgz" ->
		%%"DC*tgz "; %% 
		"tar -Ozxf "++TftpUP++" --wildcards RCS-*.cxp";
	    ".zip" -> 
		%%"CXP*zip "  %% 
		"unzip -p "++TftpUP++" RCS-*.cxp"
	end,
    
    %% Unpack UP to get file nl_product_version.txt from UP in tmpDir
    NL_CMD = CMD_prefix++" | tar -Ozxf - --wildcards RCSEE*cxp | tar -zxf - --wildcards NL3*/nl*/*version* --strip=3",
    ct:log("NL_CMD: ~p", [NL_CMD]),
    os:cmd(NL_CMD),
 
    NL_up_version = os:cmd("egrep CNX* nl_product_version.txt"), 
    NLUpVersionStr= lists:flatten(string:tokens(NL_up_version, "\n")),
    NLUpVersionStr.


reinstall_node(Config) ->
    aic_httpc:download_files(Config, console),
    aic_httpc:integrate(Config, console, ?NC),
    ct:log("sleep 60sec to make sure node is up and ready for next tests!"),
    timer:sleep(60000),
    ok.

   
get_board_type()->
    BoardType = proplists:get_value(
		  board_type,ct:get_config(
			       ct:get_config({test_nodes,1}))),

    ct:log("BoardType: ~p", [BoardType]),
    BoardType.
