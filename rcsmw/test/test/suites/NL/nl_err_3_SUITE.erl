%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	nl_err_3_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R5A/R6A/R7A/R9A/1
%%%
%%% @doc == NL / AI Error testcases. Corrupt UP ==
%%%
%%%
%%% @end

-module(nl_err_3_SUITE).
-author('etxmlar').
-vsn('/main/R3A/R4A/R5A/R6A/R7A/R9A/1').
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
%%% R3A/1      2015-05-12 etxivri     Created.
%%% R3A/3      2015-05-20 etxivri     Update to remove TAIPAN/KATLA on tcu.
%%% R4A/1      2015-07-01 etxivri     Update for tcu04
%%% R4A/2      2015-07-02 etxivri     Add check that FPGA is missing.
%%% R5A/1      2016-06-02 etxmlar     Added for tcu03 
%%%                                   (because of only one NL partition):
%%%	                              Do NL upgrade to run on correct track
%%% R6A/1      2016-07-07 etxmlar     Updated to handle .zip UPs  
%%% R7A/1      2016-10-18 etxmlar     Updated NL upgrade for tcu03
%%% R7A/2      2016-11-04 etxmlar     Updated to support "Full UP"
%%% R9A/1      2017-01-26 etxmlar     Added support for new boardtypes
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
	 corrupt_up/1
	]).


-define(NC, nc1).
-define(Protocol, "https").
-define(NL_UPGRADE_FILE, "RbsSummaryFile.xml.nl").
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

    SecType = case nl_lib:check_if_vc_board() of
		    "yes" -> 
			sec;
		    _Other1 ->
			unsec
		end,
    ct:pal("### SecType : ~p",[SecType]),

    SearchPackage = case SecType of
			sec -> 
			    "DC-*.tgz";
			unsec ->
			    get_node_up(TftpBootDir)
			    %%"RCP-*.cxs"
		    end,

    ct:pal("###  SearchPackage: ~p",[SearchPackage]),
    
    LS = os:cmd("ls "++TftpBootDir++SearchPackage),
    ct:pal("### LS : ~p",[LS]),
    PackageName = lists:last(string:tokens(LS," /\n")),
    ct:pal("### PackageName: ~p",[PackageName]),

    [{tftpboot_dir, TftpBootDir},
     {hw, HW},
     {sec_type, SecType},
     {package_name, PackageName} | Config].

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
init_per_testcase(TestCase = corrupt_up, Config) ->
    ct:pal("TestCase: ~p",[TestCase]),
    %% SecType = proplists:get_value(sec_type, Config),
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    PackageName = proplists:get_value(package_name, Config),

    LogPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++LogPath),
    os:cmd("mkdir "++LogPath++"tmp"),
    TmpDir =LogPath++"tmp/",
    ct:pal("TmpDir: ~p", [TmpDir]),
    os:cmd("cp "++TftpBootDir++PackageName++" "++LogPath),
    LS_TmpDir = string_tokens(os:cmd("ls "++LogPath)),
    ct:log("Ls TmpDir: ~p", [LS_TmpDir]),

    %% cp package name to an orginal
    os:cmd("cp "++TftpBootDir++PackageName++" "++
    	       TftpBootDir++"org_"++PackageName),
    LS_TftpBootDir = string_tokens(os:cmd("ls "++TftpBootDir)),
    ct:log("Ls TftpBootDir: ~p", [LS_TftpBootDir]),
    [{log_path, LogPath},
     {tmp_dir, TmpDir} | Config];
	 

init_per_testcase(TestCase, Config) ->
    ct:pal("TestCase: ~p",[TestCase]),
    Config.

end_per_testcase(corrupt_up, Config) ->
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    PackageName = proplists:get_value(package_name, Config),
    %% mv orinal package name to the right one.
    os:cmd("mv "++TftpBootDir++"org_"++PackageName++" "++
	       TftpBootDir++PackageName),
    LS = string_tokens(os:cmd("ls "++TftpBootDir)),
    ct:log("Ls TftpBootDir: ~p", [LS]),

    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p. ~n export ai log. \n",[Reason]),
    	    nl_lib:export_ai_log(Config, ?Protocol)
    end,
    ok;

end_per_testcase(_TestCase, _Config) ->
    ok.


string_token(Str) ->
    ListOfStr = string:tokens(Str, "\n"),
    ListOfStr.


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
    [
     board_restore,
     corrupt_up,
     install_node
    ].

board_restore(Config) ->
    case nl_lib:open_netconf(?NC) of
	{ok, _} ->
	    ct:pal("Precondition: perform board restore."),
	    nl_lib:board_restore(Config, console, ?NC, ?Protocol),
	    ct:pal("Board is restored. Now test can start."),
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
%% @spec corrupt_up(Config) -> ok
%% @end
%%--------------------------------------------------------------------
corrupt_up(Config) ->
    ct:pal("F_1_6 : Corrupt UP"),
    ct:pal("Config: ~p", [Config]),

    %% Remove COBRA on dus or TAIPAN or KATLA on tcu CXP to Create a corrupt UP.
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    PackageName = proplists:get_value(package_name, Config),
    LogPath = proplists:get_value(log_path, Config),
    TmpDir = proplists:get_value(tmp_dir, Config),

    unpack_remove_pack(Config, LogPath, TmpDir, PackageName),

    os:cmd("cp "++TmpDir++PackageName++" "++TftpBootDir),

    ct:pal("## Start Download"),
    nl_lib:httpc_request(Config, post, "Download", ?Protocol),
    ct:pal("## Download started"),
    
    %%check_after_download_fail(Config, "Failure: FPGA software not found"), %%16A
    check_after_download_fail(Config, "Mismatch between UP / HAL metadata files and SW packages"),
    ok.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
unpack_remove_pack(Config, LogPath, TmpDir, PackageName) ->
    ct:pal("pwd: ~p", [string_tokens(os:cmd("pwd"))]),

    %% unpack
    FileExt = filename:extension(PackageName),
    ct:log("FileExt: ~p",[FileExt]),

    case FileExt of
	".cxs" ->
	    os:cmd("tar -xzf "++LogPath++PackageName);
	".tgz" ->
	    os:cmd("tar -xzf "++LogPath++PackageName);
	".zip" ->  
	     os:cmd("unzip "++LogPath++PackageName)
	  
    end,

    ct:pal("LS 1: ~p", [string_tokens(os:cmd("ls"))]),

    timer:sleep(5000),
    os:cmd("rm "++PackageName),

    BoardType = proplists:get_value(
		  board_type,ct:get_config(
			       ct:get_config({test_nodes,1}))),
    ct:pal("BoardType: ~p", [BoardType]),
    case BoardType of
	BoardType when BoardType == "tcu03" ->
	    %% Special for tcu03 (because of only one NL partition)
	    %% Do NL upgrade to run on correct track
	    do_nl_upgrade(Config, ?NL_UPGRADE_FILE),
	    ct:pal(BoardType++", remove TAIPAN cxp.", []), 
	    os:cmd("rm TAIPAN*.cxp");
	BoardType when BoardType == "tcu04";				
		       BoardType == "tcu0401" ->
	    ct:pal(BoardType++", remove KATLA cxp.", []), 
	    os:cmd("rm KATLA*.cxp");
	BoardType when BoardType == "dus5201";				
		       BoardType == "dus3201" ->
	    ct:pal(BoardType++", remove COBRA cxp.", []),
	    os:cmd("rm COBRA*.cxp");
	BoardType when BoardType == "dus5301";				
		       BoardType == "dus3301"->
	    ct:pal(BoardType++", remove TIGER cxp.", []),
	    os:cmd("rm TIGER*.cxp");
	BoardType when BoardType == "dus6303";
		       BoardType == "dus6502" ->  
	    ct:pal(BoardType++", remove MTIGER cxp.", []),
	    os:cmd("rm MTIGER*.cxp");
	BoardType when BoardType == "c608" ->
	    ct:pal(BoardType++", remove KATLA cxp.", []), 
	    os:cmd("rm KATLA*.cxp")
    end,
    ct:pal("LS 2: ~p", [string_tokens(os:cmd("ls"))]),

    %% pack
    FileExt = filename:extension(PackageName),
    ct:log("FileExt: ~p",[FileExt]),

    case FileExt of
	".cxs" ->
	    os:cmd("tar -czf "++PackageName++" "++"*.cxp"++" "++"*.xml");
	".tgz" ->
	    os:cmd("tar -czf "++PackageName++" "++"*.cxp"++" "++"*.xml");
	".zip" ->
	    os:cmd("zip "++PackageName++" "++"*.cxp"++" "++"*.xml")	  
    end,

    ct:pal("LS 4: ~p", [string_tokens(os:cmd("ls"))]),

    %% cp package to tmpdir
    os:cmd("cp "++PackageName++" "++TmpDir),
    ct:pal("LS 5: ~p", [string_tokens(os:cmd("ls "++TmpDir))]),

    ok.



string_tokens(Str) ->
    string:tokens(Str, " \n").


%%--------------------------------------------------------------------
check_after_download_fail(_Config, ExpectStr) ->
    case nl_lib:check_if_vc_board() of
	"yes" -> 
	    ok;
	_ ->
	    %% ct:pal("ToDo, use this check when it is implemented by Frodis. ~p",
	    %% 	   [ExpectStr])

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
		     [{total_timeout, 60000},no_prompt_check]).

    %% Res = nl_lib:request_download_pogress(Config, ?Protocol),
    %% ct:log("Res: ~p", [Res]).

%%--------------------------------------------------------------------
do_nl_upgrade(Config, NL_Upgrade_File)->

    ct:log("# Start download for NL upgrade."),
    aic_httpc:nl_upgrade(Config, console, NL_Upgrade_File),
    ct:log("# Done download for NL upgrade."),
    timer:sleep(5000),
    ok.

%%--------------------------------------------------------------------
get_node_up(TftpBootDir)->

    UpfileList = filelib:wildcard(TftpBootDir++"*.{cxs,zip,tgz}"),
    UpfileString = lists:flatten(UpfileList),
    Up = filename:basename(UpfileString),
    Up. 
