%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nl_no_downgrade_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R4A/R5A/R6A/R7A/R9A/5
%%%
%%% @doc == NL / AI Error testcases. Faulty RCS UP, TCU on DUS and vise versa ==
%%%
%%%
%%% @end

-module(nl_no_downgrade_SUITE).
-author('etxmlar').
-vsn('/main/R4A/R5A/R6A/R7A/R9A/5').
-date('2017-04-20').

%%% 
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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R4A/1      2015-10-12 etxmlar     Created. 
%%% R4A/2      2015-10-12 etxmlar     Updated 
%%% R4A/3      2015-10-20 etxmlar     Corrected CXS UP check  
%%% R4A/4      2015-10-23 etxmlar     Corrected ExpString 
%%% R4A/5      2015-10-29 etxmlar     Corrected for TCU    
%%% R4A/6      2015-10-30 etxmlar     Bug fix      
%%% R4A/7      2015-12-09 etxmlar     OLD_R3_TCU_UP_URL define
%%%                                   changed to UP in vobs   
%%% R4A/8      2015-12-14 etxmlar     Corrected if single NL
%%% R5A/1      2016-05-27 etxmlar     Changed check after download
%%% R5A/2      2016-05-31 etxmlar     Changed check after download again,
%%%                                   missed dus.
%%% R6A/1      2016-08-19 etxkols     Git migration requires that CC paths is not used 
%%% R7A/1      2016-10-25 etxmlar     Updated TC
%%% R7A/2      2016-12-06 etxmlar     Updated after NL correction 
%%% R7A/3      2017-01-10 etxmlar     Updated TC after NL code update  
%%% R9A/1      2017-01-26 etxmlar     Added support for new boardtypes
%%% R9A/2      2017-02-03 etxmlar     Added tests for R5, R7, R8
%%% R9A/3      2017-02-22 etxmlar     Changed skip function
%%% R9A/4      2017-03-15 etxmlar     Try to make test cases more robust
%%% R9A/5      2017-04-20 etxmlar     R7 NL can install X3 UPs from R9A
%%%                                   as soon as boot files are signed so now
%%%                                   inhibit hardfactory reset for this case
%%% ----------------------------------------------------------
%%%

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
	 board_restore/1,
	 old_R3_rcs_up/1,
	 old_R4_rcs_up/1,
	 old_R5_rcs_up/1,
	 old_R7_rcs_up/1,
	 old_R8_rcs_up/1
	 %%install_node/1
	]).


-define(NC, nc1).
-define(Protocol, "https").


-define(OLD_R3_TCU_UP_URL, "https://rbs-rde-dev.rnd.ki.sw.ericsson.se/vobs/rcs/test/RCT_CRX901275/test/suites/NL/CXP9024419_2-R8HHB.zip").
%%-define(OLD_R3_DUS_UP_URL, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/g2ci/CXP9024418_1/R3HJ/CXP9024418_1-R3HJ.zip"). %% NL CNX9012629-R3A63
-define(OLD_R3_DUS_UP_URL, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_1/R14TD/CXP9024418_1-R14TD.zip"). %% NL CNX9012629-R3T01

%% Full TCU UP NL vesion CNX9012629-R4G01
-define(OLD_R4_TCU_UP_URL, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_3/R7DK/CXP9024419_3-R7DK.zip").

%% Full DUS UP NL vesion R4U06
-define(OLD_R4_DUS_UP_URL, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_2/R11CB/CXP9024418_2-R11CB.zip").


%% Full TCU UP NL vesion 
-define(OLD_R5_TCU_UP_URL,"https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_4/R17F/CXP9024419_4-R17F.zip"). %% NL 
%% Full DUS UP NL vesion 
-define(OLD_R5_DUS_UP_URL, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_4/R19DU/CXP9024418_4-R19DU.zip").  %% NL


%% Full TCU UP NL vesion 
-define(OLD_R7_TCU_UP_URL, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_5/R11J/CXP9024419_5-R11J.zip").
%% Full DUS UP NL vesion 
-define(OLD_R7_DUS_UP_URL, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_5/R19AA/CXP9024418_5-R19AA.zip").


%% Full TCU UP NL vesion
-define(OLD_R8_TCU_UP_URL, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_6/R5A278/CXP9024419_6-R5A278.zip").
%% Full DUS UP NL vesion 
-define(OLD_R8_DUS_UP_URL, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_6/R4A241/CXP9024418_6-R4A241.zip"). %%NL version CNX9012629-R8N04

%% Full DUS UP NL vesion R9
-define(DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_DETECTING_AXM5600_BOOTSIG, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_6/R11A122/CXP9024418_6-R11A122.zip"). %% NL R9U04

-define(C608_OLD_UP_NL_VERSION_R8N04, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_6/R5A278/CXP9024419_6-R5A278.zip").  %%NL version CNX9012629-R8N04

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
			 {rct_rs232,console},
			 {cth_conn_log,[]}]}];
	_Other ->
	    [{timetrap, {minutes, 600}}, % 10 hours
	     {ct_hooks, [{rct_rpc, rpc},
			 {rct_htmllink,[]},
			 {rct_consserv,cs1},
			 {rct_rs232,console},
			 {rct_power,power},
			 {rct_netconf, nc1},
			 {cth_conn_log, []},
			 {rct_cli, {cli, [manual_connect]}}
			]}]
    end.


%% @hidden
init_per_suite(Config) ->
    HW = atom_to_list(ct:get_config({test_nodes,1})),
    TftpBootDir = "/proj/rcs-tmp/tftpboot/"++HW++"/",

    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:log("ls : ~p ", [Ls]),

    NodeUP = get_node_up(TftpBootDir),
    ct:log("Node UP : ~p ", [NodeUP]),

    %% create tmp dir in priv_dir
    TmpPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++TmpPath),
    os:cmd("mkdir "++TmpPath++"tmp"),
    TmpDir =TmpPath++"tmp/",
    ct:pal("TmpDir: ~p", [TmpDir]),

    %% CxsUP = check_if_cxs_up(NodeUP, TftpBootDir),
    %% ct:log("CXS UP : ~p ", [CxsUP]),

    [{tftpboot_dir, TftpBootDir},
     {node_up, NodeUP},
     {tmp_dir, TmpDir},
     %%{cxs_up, CxsUP},
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
init_per_testcase(TestCase, Config) 
  when TestCase == old_R3_rcs_up;
       TestCase == old_R4_rcs_up;
       TestCase == old_R5_rcs_up;
       TestCase == old_R7_rcs_up ->
    ct:log("Init TestCase: ~p",[TestCase]),

    BoardType = check_board_type(),
    case BoardType of
	BoardType when BoardType == "tcu03";
		       BoardType == "dus5301";				
		       BoardType == "dus3301";
		       BoardType == "dus6303";
		       BoardType == "dus6502";
		       BoardType == "c608" ->

	    Skip = "TC not valid for "++BoardType,
	    [{skip, Skip}| Config];
	    %%{skip, "TC not valid for "++BoardType}; %% Same as failed in Jenkins

	BoardType ->

	    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
	    Org_UP = proplists:get_value(node_up, Config),
	    TmpDir = proplists:get_value(tmp_dir, Config), 


	    %% cp original UP to priv_dir to use in testcase
	    copy(Org_UP, TftpBootDir, TmpDir),
	    LS_TmpDir = string_token(os:cmd("ls "++TmpDir)),
	    ct:log("Ls TmpDir: ~p", [LS_TmpDir]),
	    Config
    end;


init_per_testcase(TestCase, Config) ->
    ct:log("Init TestCase: ~p",[TestCase]),


    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    Org_UP = proplists:get_value(node_up, Config),
    TmpDir = proplists:get_value(tmp_dir, Config), 


    %% cp original UP to priv_dir to use in testcase
    copy(Org_UP, TftpBootDir, TmpDir),
    LS_TmpDir = string_token(os:cmd("ls "++TmpDir)),
    ct:log("Ls TmpDir: ~p", [LS_TmpDir]),

    Config.


end_per_testcase(TestCase, Config)
  when TestCase == old_R3_rcs_up;
       TestCase == old_R4_rcs_up;
       TestCase == old_R5_rcs_up;
       TestCase == old_R7_rcs_up ->

    ct:log("End TestCase: ~p",[TestCase]),

    BoardType = check_board_type(),
    case BoardType of
	BoardType when BoardType == "tcu03";
		       BoardType == "dus5301";				
		       BoardType == "dus3301";
		       BoardType == "dus6303";
		       BoardType == "dus6502";
		       BoardType == "c608" ->
	    ct:log("Reinstall Original UP not needed. TC not valid for: ~p",[BoardType]),
	    continue;
	BoardType ->
	    case proplists:get_value(tc_status, Config) of
		ok ->
		    HW =  proplists:get_value(hw, Config), 
		    TmpDir = proplists:get_value(tmp_dir, Config), 
		    Org_UP =  proplists:get_value(node_up, Config), 
		    Orignal_UP = filename:join(TmpDir, Org_UP),

		    download_config_in_tftpboot_dir(Config, HW, Orignal_UP),

		    ct:log("## Make sure node is up after tests.  Download and integrate."),
		    nl_lib:download_files(Config, console, ?Protocol),
		    nl_lib:integrate(Config, console, ?NC, ?Protocol),
		    ok;
		{failed, Reason}  ->
		    ct:log("Testcase failed due to: ~p. ~n export ai log. \n",[Reason]),
		    nl_lib:export_ai_log(Config, ?Protocol)
	    end
    end,
    ok;

end_per_testcase(TestCase, Config)
  when TestCase == old_R8_rcs_up ->

    ct:log("End TestCase: ~p",[TestCase]),

    case proplists:get_value(tc_status, Config) of
	ok ->
	    HW =  proplists:get_value(hw, Config), 
	    TmpDir = proplists:get_value(tmp_dir, Config), 
	    Org_UP =  proplists:get_value(node_up, Config), 
	    Orignal_UP = filename:join(TmpDir, Org_UP),

	    download_config_in_tftpboot_dir(Config, HW, Orignal_UP),

	    ct:log("## Make sure node is up after tests.  Download and integrate."),
	    nl_lib:download_files(Config, console, ?Protocol),
	    nl_lib:integrate(Config, console, ?NC, ?Protocol),
	    ok;
	{failed, Reason}  ->
	    ct:log("Testcase failed due to: ~p. ~n export ai log. \n",[Reason]),
	    nl_lib:export_ai_log(Config, ?Protocol)
    end,
    ok;

end_per_testcase(TestCase, Config)  ->
    ct:log("End TestCase: ~p",[TestCase]),

    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:log("Testcase failed due to: ~p. ~n export ai log. \n",[Reason]),
    	    nl_lib:export_ai_log(Config, ?Protocol)
    end,
    ok.


string_token(Str) ->
    ListOfStr = string:tokens(Str, "\n"),
    ListOfStr.

get_node_up(TftpBootDir)->

    UpfileList = filelib:wildcard(TftpBootDir++"*.{tgz,cxs,zip}"),
    UpfileString = lists:flatten(UpfileList),
    Up = filename:basename(UpfileString),
    Up. 


copy(Filename, Fromdir, Todir) ->
    file:copy(filename:join(Fromdir, Filename), 
	      filename:join(Todir, Filename)).

check_board_type()->
    BoardType = proplists:get_value(
		  board_type,ct:get_config(
			       ct:get_config({test_nodes,1}))),

    ct:log("BoardType: ~p", [BoardType]),
    BoardType.

%%--------------------------------------------------------------------
%% @doc
%% groups.
%% @end
%%--------------------------------------------------------------------
groups() ->
    [
    ].

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [old_R3_rcs_up,
     old_R4_rcs_up,
     old_R5_rcs_up,
     old_R7_rcs_up,
     old_R8_rcs_up
    ].

%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec old_R3_rcs_up(Config) -> ok
%% @end
%%--------------------------------------------------------------------
old_R3_rcs_up(Config) ->
    ct:pal("F_1_18: Old R3 RCS UP with no filename in .xml file (tcu04). ~n"
	   "F_1_18: Test to check that a new NL can download an old R3 UP (dus32 and dus52)."),

   
    case proplists:is_defined(skip, Config) of 
	true->    
	    CommentStr = proplists:get_value(skip, Config),
	    ok = ct:comment(CommentStr);
	false ->

            %%%%
	    %% Board restore
            %%%%   
	    board_restore(Config),

	    %% Check NL version on node
	    NodeNLVerBefore = get_nl_version_node(Config),
	    ct:log("Network Loader Version on Node: ~p", [NodeNLVerBefore]),


	    HW =  proplists:get_value(hw, Config),  
	    BoardType = proplists:get_value(
			  board_type,ct:get_config(
				       ct:get_config({test_nodes,1}))),

	    ct:log("BoardType: ~p", [BoardType]),

	    {UP, ExpStr, ExpStr1} =
		case BoardType of
		    BoardType when BoardType == "tcu04";				
				   BoardType == "tcu0401" ->
			ct:pal(BoardType++", download old R3 TCU UP (tcu03 UP)", []), 
			{?OLD_R3_TCU_UP_URL,
			 ["Failure: could not find product RCS-T.*cxp in inventory file", 
			  "Failure: could not find product KATLA.*CXP102185 in inventory file"],
			 "Network Loader Ready"};

		    BoardType when BoardType == "dus5201";				
				   BoardType == "dus3201" ->
			ct:pal(BoardType++", download old R3 DUS UP", []), 
			{?OLD_R3_DUS_UP_URL, 
			 "Network Loader up to date", 
			 "Download Completed"}
		end,

	    UP_URL = lists:flatten(string:tokens(UP,"\n ")),

	    ct:log("UP URL: ~p", [UP_URL]),
	    ct:log("HW: ~p", [HW]),


	    download_config_in_tftpboot_dir(Config, HW, UP_URL),

	    %%Check if Multi NL
	    {ExpectStr, ExpectStr1} =
		case support_multi_nl() of
		    true ->
			{ExpStr, ExpStr1};	
		    false ->
			{"upgrade not supported for single NL partitions", "Download Completed"}
		end,

	    %%Just in case for ct_telnet
	    timer:sleep(15000),

	    start_download(Config, "RbsSummaryFile.xml"),
	    check_after_download(Config, ExpectStr, ExpectStr1)
    end,
    
    ok.

%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec old_R4_rcs_up(Config) -> ok
%% @end
%%--------------------------------------------------------------------
old_R4_rcs_up(Config) ->
    ct:pal("F_1_19: Old R4 RCS UP. Test to check that a new NL can install an old R4 UP"),


    case proplists:is_defined(skip, Config) of 
	true->    
	    CommentStr = proplists:get_value(skip, Config),
	    ok = ct:comment(CommentStr);
	false ->

	    %% Board restore
	    board_restore(Config),

	    %% Check NL version on node
	    NodeNLVerBefore = get_nl_version_node(Config),
	    ct:log("Network Loader Version on Node: ~p", [NodeNLVerBefore]),


	    HW =  proplists:get_value(hw, Config),  
	    BoardType = proplists:get_value(
			  board_type,ct:get_config(
				       ct:get_config({test_nodes,1}))),

	    ct:log("BoardType: ~p", [BoardType]),

	    UP_URL =
		case BoardType of
		    BoardType when BoardType == "tcu04";				
				   BoardType == "tcu0401" ->
			ct:pal(BoardType++", download old R4 TCU UP", []), 
			?OLD_R4_TCU_UP_URL;
		    BoardType when BoardType == "dus5201";				
				   BoardType == "dus3201" ->
			ct:pal(BoardType++", download old R4 DUS UP", []), 
			?OLD_R4_DUS_UP_URL
		end,

	    %%UP_URL = lists:flatten(string:tokens(UP,"\n ")),


	    ct:log("UP URL: ~p", [UP_URL]),
	    ct:log("HW: ~p", [HW]),


	    download_config_in_tftpboot_dir(Config, HW, UP_URL),

	    %%Check if Multi NL
	    ExpectStr =
		case support_multi_nl() of
		    true ->
			"Network Loader up to date";
		    false ->
			"upgrade not supported for single NL partitions"
		end,

	    %%Just in case for ct_telnet
	    timer:sleep(15000),

	    start_download(Config, "RbsSummaryFile.xml"),
	    check_after_download(Config, ExpectStr, "Download Completed"),

	    %% Integrate
	    generic_integrate(Config),

	    %% Board restore + check nl version on node 
	    board_restore(Config),

	    %% Check NL version on node
	    NodeNLVerAfter = get_nl_version_node(Config),
	    ct:log("Network Loader Version on Node: ~p", [NodeNLVerAfter]),

	    %% Check correct NL version on node + booted from sda2
	    check_nl_version(NodeNLVerBefore, NodeNLVerAfter)
    end,

    ok.

%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec old_R5_rcs_up(Config) -> ok
%% @end
%%--------------------------------------------------------------------
old_R5_rcs_up(Config) ->
    ct:pal("Old R5 RCS UP. Test to check that a new NL can install an old R5 UP"),


    case proplists:is_defined(skip, Config) of 
	true->    
	    CommentStr = proplists:get_value(skip, Config),
	    ok = ct:comment(CommentStr);
	false ->

	    %% Board restore 
	    board_restore(Config),

	    %% Check NL version on node
	    NodeNLVerBefore = get_nl_version_node(Config),
	    ct:log("Network Loader Version on Node: ~p", [NodeNLVerBefore]),


	    HW =  proplists:get_value(hw, Config),  
	    BoardType = proplists:get_value(
			  board_type,ct:get_config(
				       ct:get_config({test_nodes,1}))),

	    ct:log("BoardType: ~p", [BoardType]),

	    UP_URL =
		case BoardType of
		    BoardType when BoardType == "tcu04";				
				   BoardType == "tcu0401" ->
			ct:pal(BoardType++", download old R4 TCU UP", []), 
			?OLD_R5_TCU_UP_URL;
		    BoardType when BoardType == "dus5201";				
				   BoardType == "dus3201" ->
			ct:pal(BoardType++", download old R4 DUS UP", []), 
			?OLD_R5_DUS_UP_URL
		end,

	    %%UP_URL = lists:flatten(string:tokens(UP,"\n ")),


	    ct:log("UP URL: ~p", [UP_URL]),
	    ct:log("HW: ~p", [HW]),


	    download_config_in_tftpboot_dir(Config, HW, UP_URL),

	    %%Check if Multi NL
	    ExpectStr =
		case support_multi_nl() of
		    true ->
			"Network Loader up to date";
		    false ->
			"upgrade not supported for single NL partitions"
		end,

	    %%Just in case for ct_telnet
	    timer:sleep(15000),

	    start_download(Config, "RbsSummaryFile.xml"),
	    check_after_download(Config, ExpectStr, "Download Completed"),

	    %% Integrate  
	    generic_integrate(Config),

	    %% Board restore + check nl version on node   
	    board_restore(Config),

	    %% Check NL version on node%
	    NodeNLVerAfter = get_nl_version_node(Config),
	    ct:log("Network Loader Version on Node: ~p", [NodeNLVerAfter]),

	    %% Check correct NL version on node + booted from sda2
	    check_nl_version(NodeNLVerBefore, NodeNLVerAfter)
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec old_R7_rcs_up(Config) -> ok
%% @end
%%--------------------------------------------------------------------
old_R7_rcs_up(Config) ->
    ct:pal("Old R7 RCS UP. Test to check that a new NL can install an old R7 UP"),

    case proplists:is_defined(skip, Config) of 
	true->    
	    CommentStr = proplists:get_value(skip, Config),
	    ok = ct:comment(CommentStr);
	false ->

	    %% Board restore
	    board_restore(Config),

	    %% Check NL version on node
	    NodeNLVerBefore = get_nl_version_node(Config),
	    ct:log("Network Loader Version on Node: ~p", [NodeNLVerBefore]),


	    HW =  proplists:get_value(hw, Config),  
	    BoardType = proplists:get_value(
			  board_type,ct:get_config(
				       ct:get_config({test_nodes,1}))),

	    ct:log("BoardType: ~p", [BoardType]),

	    UP_URL =
		case BoardType of
		    BoardType when BoardType == "tcu04";				
				   BoardType == "tcu0401" ->
			ct:pal(BoardType++", download old R4 TCU UP", []), 
			?OLD_R7_TCU_UP_URL;
		    BoardType when BoardType == "dus5201";				
				   BoardType == "dus3201" ->
			ct:pal(BoardType++", download old R4 DUS UP", []), 
			?OLD_R7_DUS_UP_URL
		end,

	    %%UP_URL = lists:flatten(string:tokens(UP,"\n ")),


	    ct:log("UP URL: ~p", [UP_URL]),
	    ct:log("HW: ~p", [HW]),


	    download_config_in_tftpboot_dir(Config, HW, UP_URL),

	    %%Check if Multi NL
	    ExpectStr =
		case support_multi_nl() of
		    true ->
			"Network Loader up to date";
		    false ->
			"upgrade not supported for single NL partitions"
		end,

	    %%Just in case for ct_telnet
	    timer:sleep(15000),

	    start_download(Config, "RbsSummaryFile.xml"),
	    check_after_download(Config, ExpectStr, "Download Completed"),

	    %% Integrate  
	    generic_integrate(Config),

	    %% Board restore + check nl version on node 
	    board_restore(Config),

	    %% Check NL version on node
	    NodeNLVerAfter = get_nl_version_node(Config),
	    ct:log("Network Loader Version on Node: ~p", [NodeNLVerAfter]),

	    %% Check correct NL version on node + booted from sda2
	    check_nl_version(NodeNLVerBefore, NodeNLVerAfter)
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec old_R8_rcs_up(Config) -> ok
%% @end
%%--------------------------------------------------------------------
old_R8_rcs_up(Config) ->
    ct:pal("Old R8 RCS UP. Test to check that a new NL can install an old R8 UP"),

    %%%%
    %% Board restore
    %%%%   
    board_restore(Config),

    %%%
    %% Check NL version on node
    %%%%
    NodeNLVerBefore = get_nl_version_node(Config),
    ct:log("Network Loader Version on Node: ~p", [NodeNLVerBefore]),


    HW =  proplists:get_value(hw, Config),  
    BoardType = proplists:get_value(
		  board_type,ct:get_config(
			       ct:get_config({test_nodes,1}))),

    ct:log("BoardType: ~p", [BoardType]),
    
    UP_URL =
	case BoardType of
	    BoardType when BoardType == "tcu03" ->
		{skip, "TC not valid for tcu03"};
	    BoardType when BoardType == "tcu04";				
			   BoardType == "tcu0401" ->
		ct:pal(BoardType++", download old R4 TCU UP", []), 
		?OLD_R8_TCU_UP_URL;
	    BoardType when BoardType == "dus5201";				
			   BoardType == "dus3201" ->
		ct:pal(BoardType++", download old R4 DUS UP", []), 
		?OLD_R8_DUS_UP_URL;
	    BoardType when BoardType == "dus5301";				
			   BoardType == "dus3301";
			   BoardType == "dus6303";
			   BoardType == "dus6502" ->
		ct:pal(BoardType++", download old R9 DUS UP", []), 
		%%?DUS53_DUS33_DUS6303_DUS6502_OLD_UP_NL_VERSION_R8N04;
		?DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_DETECTING_AXM5600_BOOTSIG;
	    BoardType when BoardType == "c608" ->
		ct:pal(BoardType++", download old R8 TCU UP", []), 
		?C608_OLD_UP_NL_VERSION_R8N04
	end,

    %%UP_URL = lists:flatten(string:tokens(UP,"\n ")),
    

    ct:log("UP URL: ~p", [UP_URL]),
    ct:log("HW: ~p", [HW]),
    
               
    download_config_in_tftpboot_dir(Config, HW, UP_URL),

    %%Check if Multi NL
    ExpectStr =
	case support_multi_nl() of
	    true ->
		"Network Loader up to date";
	    false ->
		"upgrade not supported for single NL partitions"
	end,

    %%Just in case for ct_telnet
    timer:sleep(15000),
    
    start_download(Config, "RbsSummaryFile.xml"),
    check_after_download(Config, ExpectStr, "Download Completed"),

    %%%%
    %% Integrate
    %%%%   
    generic_integrate(Config),

    %%%%
    %% Board restore + check nl version on node
    %%%%   
    board_restore(Config),
   
    %%%%
    %% Check NL version on node
    %%%%
    NodeNLVerAfter = get_nl_version_node(Config),
    ct:log("Network Loader Version on Node: ~p", [NodeNLVerAfter]),

    %%%%
    %% Check correct NL version on node + booted from sda2
    %%%%
    check_nl_version(NodeNLVerBefore, NodeNLVerAfter),

    ok.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
board_restore(Config) ->
    ct:pal("Perform board restore."),
    nl_lib:board_restore(Config, console, ?NC, ?Protocol),
    ct:pal("Board is restored. Now test can start"),
    timer:sleep(5000).

generic_integrate(Config) ->
    ct:pal("### Start Integrate."),
    nl_lib:generic_integrate(Config, console, ?NC),
    ct:pal("### Done Integrate."),
    timer:sleep(60000).

start_download(Config, XmlFile) ->
    ct:pal("## Start Download"),
    nl_lib:httpc_request(Config, post, "Download", ?Protocol, dummy,  XmlFile),
    ct:pal("## Download started").

%% generic_download(Config) ->
%%     ct:pal("# Start Download."),
%%     nl_lib:generic_download_files(Config, console),
%%     ct:pal("# Done download."),
%%     timer:sleep(5000).

%% install_node(Config) ->
%%     ct:log("## Make sure node is up after tests.  Download and integrate."),
%%     nl_lib:download_files(Config, console, ?Protocol),
%%     nl_lib:integrate(Config, console, ?NC, ?Protocol).

%%--------------------------------------------------------------------
download_config_in_tftpboot_dir(Config, HW, UP)->
    ct:pal("Insert UP in tftpboot dir: ~p ",  [UP]),

    %%%%
    %% Insert faulty/old UP tftpboot dir
    %%%%
   
    %% CMD = "rcstprep.sh "++HW++" "++UP,
    %% ct:log("CMD:~n~p~n",[CMD]),
    %% os:cmd(CMD),
    %% timer:sleep(10000). % Just in case!


    %%HW = proplists:get_value(hw, Config),
    CMD = "rcstprep.sh "++HW++" "++UP,
    ct:log("CMD:~n~p~n",[CMD]),

    ExpectStr = "Downloading of UP and preparations for installation OK",
    RcstprepStr = os:cmd(CMD),

    Options = [global,{capture, all, binary}],

    case re:run(RcstprepStr, ExpectStr, Options) of
	{match, [[Value]]} ->
	    ct:log("Value: ~p~n", [Value]),
	    ok;
	_Other ->
	    ct:fail("TC will fail due to NOT expected answere from rcstprep.sh")
    end,

    timer:sleep(10000), % Just in case!

    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:pal("ls : ~p ", [Ls]).
 
%%--------------------------------------------------------------------
check_after_download(_Config, ExpectStr, ExpectStr1) ->
    case nl_lib:check_if_vc_board() of
	"yes" -> 
	    ok;
	_ ->
	    {ok,_} = ct_telnet:expect(console, 
				      ExpectStr,
				      [{timeout,60000}, no_prompt_check]),

	    case ExpectStr1 of
		"Download Completed" ->
		    {ok,_} =  ct_telnet:expect(console, 
					       ExpectStr1,
					       [{timeout, 60000}, no_prompt_check]);
		_Else ->
		    {ok,_} =  ct_telnet:expect(console, 
					       ExpectStr1,
					       [{timeout, 60000}, no_prompt_check]),

		    {ok,_} = ct_telnet:expect(console,
					      "Build date:", 
					      [{timeout,30000},no_prompt_check])
	    end

    end.

%%--------------------------------------------------------------------
support_multi_nl() ->

    Cmd = "ls /proc/device-tree/rcs_nodes/",
    ok = ct_telnet:send(console, Cmd),

    Ans = 
	ct_telnet:expect(console, "boot_nl3_partition",[no_prompt_check]),

    Is_Multi_nl =
	case Ans of
	    {ok, _Data} -> 
		true;
	    _ -> 
		false
	end,

    ct:log("Is_Multi_nl: ~p", [Is_Multi_nl]),
    Is_Multi_nl.

%%--------------------------------------------------------------------
get_nl_version_node(_Config) ->
    HW = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    _BOARDTYPE = ct:get_config({Hwa,board_type}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),

    start_needed_applications(?Protocol),

    HttpsUrl= "https://"++IP++"/help.html",
    HttpUrl= "http://"++IP++":"++?Protocol++"/help.html",

    UrlRes =  
	case httpc:request(HttpsUrl) of
	    {ok,{{_,200,"OK"}, _ReplyHttpsA, ReplyHttpsB}} ->
		%%ct:log("ReplyHttpsB: ~p ~n",[ReplyHttpsB]),
		ReplyHttpsB;
	    {error, Reason} ->
		case httpc:request(HttpUrl) of
		    {ok,{{_,200,"OK"}, _ReplyHttpA, ReplyHttpB}} ->
			%%ct:log("ReplyHttpB: ~p ~n",[ReplyHttpB]),
			ReplyHttpB; 
		    {error, Reason} ->
			ct:fail("TC will fail due to unknown protocol.")
		end
	end,

    stop_needed_applications(?Protocol),

    UrlListRes = string:tokens(UrlRes, " \n\t"),  
    NLNodeVerStr = 
	lists:flatten([VersionStr||VersionStr<-UrlListRes, lists:prefix("CNX", VersionStr)]),

    %% NLNodeVerStr = "CNX9012629-R4AC07<p>"
    NLNodeVersionStr = lists:flatten(string:tokens(NLNodeVerStr, "<p>")),
    NLNodeVersionStr.


%%--------------------------------------------------------------------
check_nl_version(NodeNLVerBefore, NodeNLVerAfter) ->
    case NodeNLVerBefore == NodeNLVerAfter of
	true ->
	    check_type3_booted_from_sda2(console);
	false ->
	    ct:pal("Testcase failed due to NL version missmatch:\n~p == ~p.", [NodeNLVerBefore, NodeNLVerAfter]),
	    ct:fail("Wrong Network Loader version on node: ~p",[NodeNLVerAfter])

    end.


check_type3_booted_from_sda2(Console) ->
    ct_telnet:send(Console, "cat /nl/log/nl_log.1"),
    {ok, _} = ct_telnet:expect(Console,
			       "booted from partition /dev/sda2", 
			       [{timeout,300000},no_prompt_check]),

    ok.

%%--------------------------------------------------------------------
start_needed_applications(Protocol) ->	
    inets:start(), %% För httpc
    case Protocol of
	"https" ->
	    crypto:start(),
	    ssl:start();
	_Other ->
	    ok
    end.

stop_needed_applications(Protocol) ->	
    inets:stop(), %% För httpc
    case Protocol of
	"https" ->
	    crypto:stop(),
	    ssl:stop();
	_Other ->
	    ok
    end.

