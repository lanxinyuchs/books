%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nl_err_5_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R4A/R5A/R6A/R7A/R9A/1
%%%
%%% @doc == NL / AI Error testcases. Faulty content in upgradePackageFilePath ==
%%%
%%%
%%% @end

-module(nl_err_5_SUITE).
-author('etxmlar').
-vsn('/main/R4A/R5A/R6A/R7A/R9A/1').
-date('2017-01-26').

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
%%% R4A/1      2015-09-01 etxmlar     Created.
%%% R4A/2      2015-09-21 etxmlar     Updated.    
%%% R4A/3      2015-09-22 etxmlar     Updated fake UP path.  
%%% R4A/4      2015-10-29 etxmlar     Temporary removed 
%%%                                   faulty_up_file_1_in_RbsSummaryFile
%%%                                   Needs correction in NL. 
%%% R5A/1      2016-04-14 etxmlar     Updated a string corresponding to 
%%%                                   more AI cases in 16B release
%%% R6A/1      2016-08-19 etxkols     Git migration requires that CC paths is not used 
%%% R7A/1      2016-10-18 etxmlar     Added support for .zip UP 
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
	 install_node/1,
	 faulty_up_file_format_in_RbsSummaryFile/1,
	 faulty_up_file_in_RbsSummaryFile/1,
	 faulty_up_file_1_in_RbsSummaryFile/1
	]).


-define(NC, nc1).
-define(Protocol, "https").
-define(CC_PRX, "https://rbs-rde-dev.rnd.ki.sw.ericsson.se").
-define(Fake_tcu_UP, "/vobs/rcs/test/RCT_CRX901275/test/suites/NL/nl_SUITE_data/boards_tcu.tgz").
-define(Fake_dus_UP, "/vobs/rcs/test/RCT_CRX901275/test/suites/NL/nl_SUITE_data/boards_dus.tgz").
%%-define(Fake_tcu_cs_UP, "/vobs/rcs/test/RCT_CRX901275/test/suites/NL/").
%%-define(Fake_dus_cs_UP, "/vobs/rcs/test/RCT_CRX901275/test/suites/NL/").
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
    ct:pal("ls : ~p ", [Ls]),

    CurrentUp = get_current_up(TftpBootDir),
    ct:pal("Current UP : ~p ", [CurrentUp]),

    [{tftpboot_dir, TftpBootDir},
     {current_up, CurrentUp},
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
init_per_testcase(TestCase = faulty_up_file_format_in_RbsSummaryFile, Config) ->
    ct:pal("TestCase: ~p",[TestCase]),
    cp_RbsSummaryFile_to_org(Config),
    Config;

init_per_testcase(TestCase = faulty_up_file_in_RbsSummaryFile, Config) ->
    ct:pal("TestCase: ~p",[TestCase]),
    cp_RbsSummaryFile_to_org(Config),
    Config;

init_per_testcase(TestCase = faulty_up_file_1_in_RbsSummaryFile, Config) ->
    ct:pal("TestCase: ~p",[TestCase]),
    cp_RbsSummaryFile_to_org(Config),

    %%Cp x.tgz file in tftpboot_dir
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    CpCmdTcu = "cd "++TftpBootDir++" && wget "++?CC_PRX++?Fake_tcu_UP,
    CpCmdDus = "cd "++TftpBootDir++" && wget "++?CC_PRX++?Fake_dus_UP,
    
    ct:log("CpCmdTcu: ~p ", [CpCmdTcu]),
    ct:log("CpCmdDus: ~p ", [CpCmdDus]),

    CpTcu = os:cmd(CpCmdTcu),
    CpDus = os:cmd(CpCmdDus),

    ct:log("CpTcu : ~p ", [CpTcu]),
    ct:log("CpDus : ~p ", [CpDus]),

    Config;

init_per_testcase(TestCase, Config) ->
    ct:pal("Init TestCase: ~p",[TestCase]),
    Config.

end_per_testcase(TestCase, Config) 
  when TestCase == faulty_up_file_format_in_RbsSummaryFile;
       TestCase == faulty_up_file_in_RbsSummaryFile;
       TestCase == faulty_up_file_1_in_RbsSummaryFile ->
    
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    %% Move org_RbsSummaryFile to RbsSummaryFile.
    MvCmd = "mv "++TftpBootDir++"org_RbsSummaryFile.xml "++TftpBootDir++"RbsSummaryFile.xml",
    Mv = os:cmd(MvCmd),
    ct:log("Mv : ~p ", [Mv]),
 
    %%CurrentUP =  proplists:get_value(current_up, Config),   
    %%File = filename:rootname(CurrentUP),
    
    RmCmdTcu = "rm "++TftpBootDir++"boards_tcu.tgz",
    RmTcu = os:cmd(RmCmdTcu),
    ct:log("RmTcu : ~p ", [RmTcu]),

    RmCmdDus = "rm "++TftpBootDir++"boards_dus.tgz",
    RmDus = os:cmd(RmCmdDus),
    ct:log("RmDus : ~p ", [RmDus]),

    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:log("ls : ~p ", [Ls]),
    
    false = lists:member("org_RbsSummaryFile.xml", Ls),
    true = lists:member("RbsSummaryFile.xml", Ls),

    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p. ~n export ai log. \n",[Reason]),
    	    nl_lib:export_ai_log(Config, ?Protocol)
    end,
    ok;

end_per_testcase(TestCase, _Config) ->
    ct:pal("End TestCase: ~p",[TestCase]),
    ok.

string_token(Str) ->
    ListOfStr = string:tokens(Str, "\n"),
    ListOfStr.


cp_RbsSummaryFile_to_org(Config) ->
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    %% copy RbsSummaryFile to org_RbsSummaryFile.
    CpCmd = "cp "++TftpBootDir++"RbsSummaryFile.xml "++TftpBootDir++"org_RbsSummaryFile.xml",
    Cp = os:cmd(CpCmd),
    ct:log("Cp : ~p ", [Cp]),
    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:log("ls : ~p ", [Ls]),
    true = lists:member("org_RbsSummaryFile.xml" ,Ls),
    true = lists:member("RbsSummaryFile.xml", Ls).

get_current_up(TftpBootDir)->
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
     faulty_up_file_format_in_RbsSummaryFile,
     faulty_up_file_in_RbsSummaryFile,
     %%faulty_up_file_1_in_RbsSummaryFile,
     install_node
    ].

board_restore(Config) ->
    case nl_lib:open_netconf(?NC) of
	{ok, _} ->
	    ct:pal("Precondition: perform board restore."),
	    nl_lib:board_restore(Config, console, ?NC, ?Protocol),
	    ct:pal("Board i restored. Now test can start."),
	    timer:sleep(5000);
	_Other ->
	    ct:log("Board Restored not needed !. ~p", [_Other])
    end.

install_node(Config) ->
    ct:pal("## Make sure node is up after tests.  Download and integrate."),
    nl_lib:download_files(Config, console, ?Protocol),
    nl_lib:integrate(Config, console, ?NC, ?Protocol).

%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec faulty_up_file_format_in_RbsSummaryFile(Config) -> ok
%% @end
%%--------------------------------------------------------------------
faulty_up_file_format_in_RbsSummaryFile(Config) ->
    ct:pal("F_1_14: Faulty UP file format in RbsSummaryFile.xml"),

    %% Add faulty UP file format in RbsSummaryFile.xml
    %% 1. Change UP in xml to LKF.xml.

    UpFilePath= "upgradePackageFilePath",
    CurrentUP =  proplists:get_value(current_up, Config),
    FaultyUPfile = "LKF.xml",
    
    SedCmd = "sed -i -e '/" ++UpFilePath++"/s/"++CurrentUP++"/"++FaultyUPfile++"/' ",   
               
    create_fault_in_xml(Config, "/RbsSummaryFile.xml", SedCmd),

    start_download(Config, "RbsSummaryFile.xml"),

    check_after_download_fail(Config, "Unknown format in software package"),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec faulty_up_file_in_RbsSummaryFile(Config) -> ok
%% @end
%%--------------------------------------------------------------------
faulty_up_file_in_RbsSummaryFile(Config) ->
    ct:pal("F_1_15: Faulty UP file in RbsSummaryFile.xml"),

   
    %% Add faulty UP file name.tgz in RbsSummaryFile.xml
    %% 1. Change UP in xml to .tgz 

    UpFilePath= "upgradePackageFilePath",
    CurrentUP =  proplists:get_value(current_up, Config),
    FaultyUPfile = "LKF.tgz",
   
    SedCmd = "sed -i -e '/" ++UpFilePath++"/s/"++CurrentUP++"/"++FaultyUPfile++"/' ",   
               
    create_fault_in_xml(Config, "/RbsSummaryFile.xml", SedCmd),
    
    start_download(Config, "RbsSummaryFile.xml"),
    
    check_after_download_fail(Config, "file doesn't exist on specified path"),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec faulty_up_file_1_in_RbsSummaryFile(Config) -> ok
%% @end
%%--------------------------------------------------------------------
faulty_up_file_1_in_RbsSummaryFile(Config) ->
    ct:pal("F_1_16: Faulty UP file in RbsSummaryFile.xml"),

   
    %% Add faulty UP file fake.tgz in RbsSummaryFile.xml
    %% 1. Change UP in xml to fake.tgz to 

    UpFilePath= "upgradePackageFilePath",
    CurrentUP =  proplists:get_value(current_up, Config),  
    
    BoardType = proplists:get_value(
		  board_type,ct:get_config(
			       ct:get_config({test_nodes,1}))),

    ct:pal("BoardType: ~p", [BoardType]),

    FaultyUPfile =
	case BoardType of
	    BoardType when BoardType == "tcu03" ->
		{skip, "TC not valid for tcu03"};
	    BoardType when BoardType == "tcu04";				
			   BoardType == "tcu0401" ->
		ct:pal(BoardType++", download fake tcu UP", []), 
		filename:basename(?Fake_tcu_UP); 
	    BoardType when BoardType == "dus5201";				
			   BoardType == "dus3201" ->
		ct:pal(BoardType++", download fake dus UP", []), 
		filename:basename(?Fake_dus_UP);
	    BoardType when BoardType == "dus5301";				
			   BoardType == "dus3301";
			   BoardType == "dus6303";
			   BoardType == "dus6502" ->
	    	ct:pal(BoardType++", download fake dus UP", []), 
		filename:basename(?Fake_dus_UP);
	    BoardType when BoardType == "c608" ->
		ct:pal(BoardType++", download fake tcu UP", []), 
		filename:basename(?Fake_tcu_UP)
	end,
  
    SedCmd = "sed -i -e '/" ++UpFilePath++"/s/"++CurrentUP++"/"++FaultyUPfile++"/' ",   
               
    create_fault_in_xml(Config, "/RbsSummaryFile.xml", SedCmd),
    
    start_download(Config, "RbsSummaryFile.xml"),
    
    check_after_download_fail(Config, "no such file"),
    
    ok.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
start_download(Config, XmlFile) ->
    ct:pal("## Start Download"),
    nl_lib:httpc_request(Config, post, "Download", ?Protocol, dummy,  XmlFile),
    ct:pal("## Download started").

%%--------------------------------------------------------------------
create_fault_in_xml(Config, XmlFile, SedCmd) ->
    ct:pal("Insert fault in file: ~p ",  [XmlFile]),
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),

    %%%%
    %% Insert faulty UP in RbsSummaryFile.xml
    %%%%
   
    CMD1 = SedCmd ++ TftpBootDir ++ XmlFile,
    ct:log("CMD:~n~p~n",[CMD1]),
    os:cmd(CMD1).

%%--------------------------------------------------------------------
check_after_download_fail(Config, ExpectStr) ->
    case nl_lib:check_if_vc_board() of
	"yes" -> 
	    ok;
	_ ->
	    {ok,_} = ct_telnet:expect(console, 
				      ExpectStr,
				      [{timeout,60000}, no_prompt_check])
    end,
    
    {ok,_} = 
	ct_telnet:expect(console, 
			 "Network Loader Ready",
			 [{timeout, 60000}, no_prompt_check]),

    ct_telnet:expect(console, 
		     "Build date:",
		     [{timeout,60000}, no_prompt_check]),
    
    Res = nl_lib:request_download_pogress(Config, ?Protocol),
    ct:log("Res: ~p", [Res]).
