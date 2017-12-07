%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nl_err_6_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R4A/R5A/R7A/R9A/3
%%%
%%% @doc == NL / AI Error testcases. Faulty RCS UP, TCU on DUS and vise versa ==
%%%
%%%
%%% @end

-module(nl_err_6_SUITE).
-author('etxmlar').
-vsn('/main/R4A/R5A/R7A/R9A/3').
-date('2017-02-22').

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
%%% R4A/1      2015-09-21 etxmlar     Created.
%%% R4A/2      2015-09-22 etxmlar     Upgrade. 
%%% R4A/3      2015-09-25 etxmlar     Added check for Multi NL
%%% R4A/4      2015-10-06 etxmlar     Fixed a printout 
%%% R4A/4      2015-10-12 etxmlar     Removed TC old_rcs_up
%%% R4A/5      2015-10-20 etxmlar     Corrected CXS UP check   
%%% R4A/6      2015-10-20 etxmlar     Corrected end_per_testcase 
%%% R4A/7      2015-10-23 etxmlar     Corrected ExpString  
%%% R5A/1      2016-02-08 etxmlar     Added check for rcstprep.sh 
%%% R7A/1      2016-10-18 etxmlar     Updated to install correct UP  
%%% R7A/2      2016-11-30 etxmlar     Updated to get UP from MIA   
%%% R7A/3      2016-12-06 etxmlar     Updated after NL correction 
%%% R7A/4      2016-12-06 etxmlar     Removed a printout 
%%% R7A/5      2017-01-10 etxmlar     Updated TC after NL code update 
%%% R9A/1      2017-01-26 etxmlar    Added support for new boardtypes 
%%% R9A/2      2017-02-06 etxmlar    Make test more robust   
%%% R9A/3      2017-02-06 etxmlar    Corrected for dus6303 and dus6502
%%% -----------------------------------------------------------------
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
	 install_node/1,
	 faulty_rcs_up/1
	]).


-define(NC, nc1).
-define(Protocol, "https").

-define(TCU_UP_URL, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_5/R2MJH/CXP9024419_5-R2MJH.zip").
-define(DUS_UP_URL, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_5/R2GKF/CXP9024418_5-R2GKF.zip").

-define(C608_UP_WITH_NL_VERSION_R8N04, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_6/R5A278/CXP9024419_6-R5A278.zip").  %%NL version CNX9012629-R8N04

-define(DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_VERSION_R8N04, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_6/R4A241/CXP9024418_6-R4A241.zip").  %%NL version CNX9012629-R8N04

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
    
    [{tftpboot_dir, TftpBootDir},
     {tftp_up, NodeUP},
     {tmp_dir, TmpDir},
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
init_per_testcase(TestCase, Config) when TestCase == faulty_rcs_up ->

    ct:log("Init TestCase: ~p",[TestCase]),
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    Org_UP = proplists:get_value(tftp_up, Config),
    TmpDir = proplists:get_value(tmp_dir, Config), 
    

    %% cp original UP to priv_dir to use in testcase
    copy(Org_UP, TftpBootDir, TmpDir),
    LS_TmpDir = string_token(os:cmd("ls "++TmpDir)),
    ct:log("Ls TmpDir: ~p", [LS_TmpDir]),

    Config;

init_per_testcase(TestCase, Config) ->
    ct:log("Init TestCase: ~p",[TestCase]),
    Config.


end_per_testcase(TestCase, Config) when TestCase == faulty_rcs_up ->

    ct:log("End TestCase: ~p",[TestCase]),

    HW =  proplists:get_value(hw, Config), 
    TmpDir = proplists:get_value(tmp_dir, Config), 
    Org_UP =  proplists:get_value(tftp_up, Config), 
    Orignal_UP = filename:join(TmpDir, Org_UP),

    download_config_in_tftpboot_dir(HW, Orignal_UP),

    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:log("Testcase failed due to: ~p. ~n export ai log. \n",[Reason]),
    	    nl_lib:export_ai_log(Config, ?Protocol)
    end,
    ok;

end_per_testcase(TestCase, _Config) ->
    ct:log("End TestCase: ~p",[TestCase]),
    ok.


string_token(Str) ->
    ListOfStr = string:tokens(Str, "\n"),
    ListOfStr.

get_node_up(TftpBootDir)->

    UpfileList = filelib:wildcard(TftpBootDir++"*.{tgz,cxs,zip}"),
    UpfileString = lists:flatten(UpfileList),
    Up = filename:basename(UpfileString),
    Up. 

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
    [board_restore,
     faulty_rcs_up,
     install_node
    ].

board_restore(Config) ->
    case nl_lib:open_netconf(?NC) of
	{ok, _} ->
	    ct:log("Precondition: perform board restore."),
	    nl_lib:board_restore(Config, console, ?NC, ?Protocol),
	    ct:log("Board is restored. Now test can start."),
	    timer:sleep(5000);
	_Other ->
	    ct:log("Board Restored not needed !. ~p", [_Other])
    end.

install_node(Config) ->
    ct:log("## Make sure node is up after tests.  Download and integrate."),
    nl_lib:download_files(Config, console, ?Protocol),
    nl_lib:integrate(Config, console, ?NC, ?Protocol).

%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec faulty_rcs_up(Config) -> ok
%% @end
%%--------------------------------------------------------------------
faulty_rcs_up(Config) ->
    ct:pal("F_1_17: Faulty RCS UP, TCU on DUS and vise versa"),

    HW =  proplists:get_value(hw, Config),  
    BoardType = proplists:get_value(
		  board_type,ct:get_config(
			       ct:get_config({test_nodes,1}))),

    ct:log("BoardType: ~p", [BoardType]),
    
    {UP_URL, ExpStr1, ExpStr2} =
	case BoardType of
	    BoardType when BoardType == "tcu03" ->
		{skip, "TC not valid for tcu03"};

	    BoardType when BoardType == "tcu04";				
			   BoardType == "tcu0401" ->
		ct:pal(BoardType++", download DUS UP", []), 
		{?DUS_UP_URL, "RCS-T.*cxp", "KATLA.*CXP102185"}; 

	    BoardType when BoardType == "dus5201";				
			   BoardType == "dus3201" ->
		ct:pal(BoardType++", download TCU UP", []), 
		{?TCU_UP_URL, "RCS-DUS2.*cxp", "COBRA.*CXP102171"};

	    BoardType when BoardType == "dus3301";
			   BoardType == "dus5301" ->
		ct:pal(BoardType++", download TCU UP", []), 
		change_to_tcu_config_in_tftpboot_dir(Config, ?C608_UP_WITH_NL_VERSION_R8N04),
		{no_up, "RCS.*cxp", "TIGER.*CXP102194"};

	    BoardType when BoardType == "dus6303";
			   BoardType == "dus6502" ->
		ct:pal(BoardType++", download TCU UP", []),
		change_to_tcu_config_in_tftpboot_dir(Config, ?C608_UP_WITH_NL_VERSION_R8N04),
		{no_up, "RCS.*cxp", "MTIGER.*CXP102201"};

	    BoardType when BoardType == "c608"->
		ct:pal(BoardType++", download DUS UP", []), 
		{?DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_VERSION_R8N04, "RCS-T.*cxp", "KATLA.*CXP102185"}
	end,

    ct:log("UP URL: ~p", [UP_URL]),
    ct:log("HW: ~p", [HW]),

    download_config_in_tftpboot_dir(HW, UP_URL),

    %%Check if Multi NL
    {ExpectStr1, ExpectStr2}  =
	case support_multi_nl() of
	    true ->
		{"Failure: could not find product "++ExpStr1++" in inventory file",
		 "Failure: could not find product "++ExpStr2++" in inventory file"};
	    false ->
		{"Failure: FPGA software not found", no}
	end,
    %%Just in case for ct_telnet
    timer:sleep(15000),

    start_download(Config, "RbsSummaryFile.xml"),

    check_after_download_fail(Config, console, ExpectStr1, ExpectStr2,"Network Loader Ready"),

    ok.

change_to_tcu_config_in_tftpboot_dir(Config, C608_UP)->

    TftpBootDir = proplists:get_value(tftpboot_dir, Config),

    Node = list_to_atom(proplists:get_value(hw, Config)),
    ct:pal("Node: ~p",[Node]),  
    AiInstallDir = ct:get_config({Node, sftp_ai_install_dir}),
    ct:pal("AiInstallDir: ~p",[AiInstallDir]),

    %%copy C608_UP to tftpboot dir
    ok = file:set_cwd(TftpBootDir),
    Wget = os:cmd("wget "++C608_UP),
    ct:log("wget : ~p ", [Wget]),
    UPname = filename:basename(C608_UP),
    %%UpFilePath= TftpBootDir++ UPname,

    UpFilePath= AiInstallDir++"/"++UPname,
    ct:pal("UpFilePath: ~p",[UpFilePath]),

    %% Modify RbsSummaryFile.xml to use  TCU UP
    %% Add \ before " and \ in cmd to make it an valid erlan str.
    Sed_CMD = "sed -e \"s|.*upgradePackageFilePath=.*|upgradePackageFilePath='"++UpFilePath++"'|\" "
	++TftpBootDir++"RbsSummaryFile.xml > "++TftpBootDir++"mod_RbsSummaryFile.xml",


    %% %%copy C608_UP to tftpboot dir
    %% ok = file:set_cwd(TftpBootDir),
    %% Wget = os:cmd("wget "++C608_UP),
    %% ct:log("wget : ~p ", [Wget]),
    %% UPname = filename:basename(C608_UP),
    %% UpFilePath= TftpBootDir++ UPname,
    %% %% Modify RbsSummaryFile.xml to use  TCU UP
    %% %% Add \ before " and \ in cmd to make it an valid erlan str.
    %% Sed_CMD = "sed -e \"s|.*upgradePackageFilePath=.*|upgradePackageFilePath='"++UpFilePath++"'|\" "
    %% 	++TftpBootDir++"RbsSummaryFile.xml > "++TftpBootDir++"mod_RbsSummaryFile.xml",


    %% Sed_CMD = "sed -e \"s|.*siteBasicFilePath=.*|backupFilePath='"++BackupFilePath++"'|\" "
    %% 	"-e '/siteEquipmentFilePath=/d' "
    %% 	"-e '/licensingKeyFilePath=/d' "
    %% 	"-e '/labConfigFilePath=/d' "
    %% 	"-e '/initialSecurityConfigurationFilePath=/d' "
    %% 	"-e 's|upgradePackageFilePath=\\(.*\\)|upgradePackageFilePath=\\1/>|' "
    %% 	++TftpBootDir++"RbsSummaryFile.xml > "++TftpBootDir++"mod_RbsSummaryFile.xml",

    ct:log("Sed_CMD: ~s",[Sed_CMD]),
    os:cmd(Sed_CMD),

    os:cmd("cp "++TftpBootDir++"mod_RbsSummaryFile.xml "++TftpBootDir++"RbsSummaryFile.xml"),

    A = string:tokens(os:cmd("cat "++TftpBootDir++"RbsSummaryFile.xml"), " \r[\"]"),
    ct:pal("Cat: ~s",[A]),
    ok.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
start_download(Config, XmlFile) ->
    ct:pal("## Start Download"),
    nl_lib:httpc_request(Config, post, "Download", ?Protocol, dummy,  XmlFile),
    ct:pal("## Download started").

%%--------------------------------------------------------------------
download_config_in_tftpboot_dir(_HW, no_up)->
    continue;

download_config_in_tftpboot_dir(HW, UP)->
    ct:log("Insert UP in tftpboot dir: ~p ",  [UP]),

%%%%
    %% Insert faulty/old UP tftpboot dir
%%%%

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

    timer:sleep(10000). % Just in case!
 
%%--------------------------------------------------------------------
check_after_download_fail(_Config, Console, ExpectStr1, ExpectStr2, ExpectStr3) ->

    case nl_lib:check_if_vc_board() of
	"yes" -> 
	    ok;
	_ ->
	    case ExpectStr2 of
		no ->	    
		    {ok,_} = ct_telnet:expect(console, 
					      ExpectStr1,
					      [{timeout,60000}, no_prompt_check]);
		_Str ->
		    {ok,_} = ct_telnet:expect(console,
					      [ExpectStr1,ExpectStr2],
					      [{timeout,60000}, no_prompt_check])
	    end
    end,
    
    {ok,_} = 
	ct_telnet:expect(console, 
			 ExpectStr3,
			 [{timeout, 60000}, no_prompt_check]),


    ct_telnet:expect(Console,
		     "Build date:", 
		     [{timeout,30000},no_prompt_check]).

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

copy(Filename, Fromdir, Todir) ->
    file:copy(filename:join(Fromdir, Filename), 
	      filename:join(Todir, Filename)).
