%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nl_err_2_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R3A/R4A/R5A/1
%%%
%%% @doc == NL / AI Error testcases. syntax and wrong content fault ==
%%%
%%%
%%% @end

-module(nl_err_2_SUITE).
-author('etxmlar').
-vsn('/main/R3A/R4A/R5A/1').
-date('2016-04-11').

%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
%%% R3A/1      2015-04-13 etxivri     Created.
%%% R3A/2      2015-05-08 etxivri     Added new TCs.
%%% R4A/1      2015-10-09 etxmlar     Corrected an expected string  
%%% R5A/1      2015-10-09 etxmlar     Updated a string corresponding to 
%%%                                   more AI cases in 16B release                           
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
	 syntax_fault_in_RbsSummaryFile/1,
	 syntax_fault_in_SiteInstallationFile/1,
	 wrong_content_in_RbsSummaryFile/1,
	 wrong_content_in_SiteInstallationFile/1
	]).


-define(NC, nc1).
-define(Protocol, "https").
%% -define(Port, "8080").
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
    [{tftpboot_dir, TftpBootDir},
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
init_per_testcase(TestCase = syntax_fault_in_RbsSummaryFile, Config) ->
    ct:pal("TestCase: ~p",[TestCase]),
    cp_RbsSummaryFile_to_org(Config),
    Config;
init_per_testcase(TestCase = wrong_content_in_RbsSummaryFile, Config) ->
    ct:pal("TestCase: ~p",[TestCase]),
    cp_RbsSummaryFile_to_org(Config),
    Config;
init_per_testcase(TestCase, Config) ->
    ct:pal("TestCase: ~p",[TestCase]),
    Config.

end_per_testcase(_TestCase, Config) ->
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    %% Move org_RbsSummaryFile to RbsSummaryFile.
    MvCmd = "mv "++TftpBootDir++"org_RbsSummaryFile.xml "++TftpBootDir++"RbsSummaryFile.xml",
    Mv = os:cmd(MvCmd),
    ct:log("Mv : ~p ", [Mv]),
 
    RmCmd = "rm "++TftpBootDir++"SiteInstallationFile.xml ",
    Rm = os:cmd(RmCmd),
    ct:log("Mv : ~p ", [Rm]),

    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:log("ls : ~p ", [Ls]),

    false = lists:member("org_RbsSummaryFile.xml", Ls),
    true = lists:member("RbsSummaryFile.xml", Ls),
    false = lists:member("SiteInstallationFile.xml", Ls),

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
     syntax_fault_in_RbsSummaryFile,
     syntax_fault_in_SiteInstallationFile,
     wrong_content_in_RbsSummaryFile,
     wrong_content_in_SiteInstallationFile,
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
%% @spec syntax_fault_in_RbsSummaryFile(Config) -> ok
%% @end
%%--------------------------------------------------------------------
syntax_fault_in_RbsSummaryFile(Config) ->
    ct:pal("F_1_7 : Syntax fault, RbsSummaryFile.xml"),

    %% Create a syntax fault in RbsSummaryFile.xml
    %% 1. Remove last character from xml and copy to tmp file.
    SedCmd = "sed '$s/.$//' ",
    create_fault_in_xml(Config, "RbsSummaryFile.xml", SedCmd),

    start_download(Config, "RbsSummaryFile.xml"),

    check_after_download_fail(Config, "Xml parsing error at line"),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec syntax_fault_in_SiteInstallationFile(Config) -> ok
%% @end
%%--------------------------------------------------------------------
syntax_fault_in_SiteInstallationFile(Config) ->
    ct:pal("F_1_8 : Syntax fault, SiteInstallationFile.xml"),

    %% Generate a SiteInstallationFile.xml
    generate_sif(Config),

    %% 1. Remove last character from xml and copy to tmp file.
    SedCmd = "sed '$s/.$//' ",
    create_fault_in_xml(Config, "SiteInstallationFile.xml", SedCmd),
    
    start_download(Config, "SiteInstallationFile.xml"),

    check_after_download_fail(Config, "Xml parsing error at line"),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec wrong_content_in_RbsSummaryFile(Config) -> ok
%% @end
%%--------------------------------------------------------------------
wrong_content_in_RbsSummaryFile(Config) ->
    ct:pal("F_1_9 : Wrong Content, RbsSummaryFile.xml"),

    %% 1. Remove a content from xml
    Content1 = "siteBasicFilePath",
    SedCmd = "sed -e  '/"++Content1++"=/d' ",
    create_fault_in_xml(Config, "RbsSummaryFile.xml", SedCmd),

    start_download(Config, "RbsSummaryFile.xml"),

    check_after_download_fail(
      Config, "Missing required file path: siteBasicFilePath"),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% - 
%% @spec wrong_content_in_SiteInstallationFile(Config) -> ok
%% @end
%%--------------------------------------------------------------------
wrong_content_in_SiteInstallationFile(Config) ->
    ct:pal("F_1_10 : Wrong content, SiteInstallationFile.xml"),

    %% Generate a SiteInstallationFile.xml
    generate_sif(Config),

    %% 1. Remove a content from xml
    Content1 = "summaryFilePath",
    SedCmd = "sed -e  '/"++Content1++"=/d' ",
    create_fault_in_xml(Config, "SiteInstallationFile.xml", SedCmd),

    start_download(Config, "SiteInstallationFile.xml"),

    check_after_download_fail(
      Config, "'SmrsData' element is missing mandatory attribute 'summaryFilePath' in Site Installation File"),

    ok.


%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
start_download(Config, XmlFile) ->
    ct:pal("## Start Download"),
    nl_lib:httpc_request(Config, post, "Download", ?Protocol, dummy,  XmlFile),
    ct:pal("## Download started").

%%--------------------------------------------------------------------
generate_sif(Config) ->
    %% Generate a SiteInstallationFile.xml
    HwId = list_to_atom(proplists:get_value(hw, Config)),
    ct:pal("Hwid: ~p",[HwId]),
    ok = nl_lib:gen_and_write_sif("SiteInstallationFile.xml", 
				  "RbsSummaryFile.xml", 
				  HwId),

    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:log("ls : ~p ", [Ls]),
    true = lists:member("SiteInstallationFile.xml", Ls).

%%--------------------------------------------------------------------
create_fault_in_xml(Config, XmlFile, SedCmd) ->
    ct:pal("Insert syntax fault in file: ~p ",  [XmlFile]),
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),

    %%%%
    %% Create a syntax fault in .xml 
    %%%%
    %% 1. Remove last character from xml and copy to tmp file.
    CMD1 = SedCmd ++ 
    	TftpBootDir ++ 
    	XmlFile ++ " > " ++ 
    	TftpBootDir ++ 
    	"/tmp_file.xml",
    ct:log("CMD:~n~p~n",[CMD1]),
    os:cmd(CMD1),

    %% 2. mv tmp file to RbsSummaryFile.
    CMD2 = "mv " ++ 
    	TftpBootDir ++ 
    	"/tmp_file.xml " ++ 
    	TftpBootDir ++ XmlFile,
    ct:log("CMD:~n~p~n",[CMD2]),
    os:cmd(CMD2).

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

    Res = nl_lib:request_download_pogress(Config, ?Protocol),
    ct:log("Res: ~p", [Res]).
