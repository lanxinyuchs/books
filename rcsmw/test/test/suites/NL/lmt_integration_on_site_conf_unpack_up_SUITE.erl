%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmt_integration_on_site_conf_unpack_up_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2017
%%% @version /main/R7A/2
%%%
%%% @doc == AI lmt integration on-site conf test with SIF and an anpacked UP.==
%%%
%%%
%%% @end

-module(lmt_integration_on_site_conf_unpack_up_SUITE).
-author('etxmlar').
-vsn('/main/R7A/2').
-date('2017-01-09').

%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% Rev      Date        Name        What
%%% -----    ---------   --------    ------------------------
%%% R7A/1    2016-12-22  etxmlar     Created
%%% R7A/2    2016-12-22  etxmlar     Updated and checked in
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
	 nodeUC640_N_010/1
	]).


-define(NC, nc1).
-define(Protocol, "https").
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

    NodeUP = get_node_up(TftpBootDir),
    ct:log("Node UP : ~p ", [NodeUP]),

    %% create tmp dir in priv_dir
    TmpPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++TmpPath),
    os:cmd("mkdir "++TmpPath++"tmp"),
    TmpDir =TmpPath++"tmp/",
    ct:pal("TmpDir: ~p", [TmpDir]),

    SiteInstallationFile = "SiteInstallationFile.xml",

    [{tftpboot_dir, TftpBootDir},
     {hw, HW},
     {tftp_up, NodeUP},
     {tmp_dir, TmpDir},
     {sif, SiteInstallationFile} | Config].

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
init_per_testcase(TestCase, Config) ->

    ct:pal("init_per_testcase Testcase: ~p.\n", [TestCase]),
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    Org_UP = proplists:get_value(tftp_up, Config),
    ct:pal("TftpBootDir: ~p.\n", [TftpBootDir]),

    %%Unpack UP Before download
    Dir_Name = filename:rootname(Org_UP),
    ct:log("Dir_Name: ~p ", [Dir_Name]),

    ok = file:make_dir(TftpBootDir ++ Dir_Name),

    UP_Dir = TftpBootDir++Dir_Name++"/",
    ct:log("UP_Dir: ~p ", [UP_Dir]),

    CpCmd = "cp "++TftpBootDir++Org_UP++" "++UP_Dir,
    send_os_cmd(CpCmd),

    LS_UP_Dir = string_tokens(os:cmd("ls "++UP_Dir)),
    ct:log("Ls UP_Dir: ~p", [LS_UP_Dir]),
    UP_to_unpack = lists:flatten(LS_UP_Dir),
    ct:log("UP_to_unpack: ~p", [UP_to_unpack]),

    FileExt = filename:extension(UP_to_unpack),

    case FileExt of
	".cxs" ->
	    UnzipCmd = "cd "++UP_Dir++" ; tar -xzf "++UP_to_unpack,
	    send_os_cmd(UnzipCmd);
	".tgz" ->
	    UnzipCmd = "cd "++UP_Dir++" ; tar -xzf "++UP_to_unpack,
	    send_os_cmd(UnzipCmd);
	".zip" -> 
	    UnzipCmd = "cd "++UP_Dir++" ; unzip "++UP_to_unpack,
	    send_os_cmd(UnzipCmd)
    end,
    
    
    %% Modify RbsSummaryfile so upgradePackageFilePath=Only path to unpacked UP
    os:cmd("cp "++TftpBootDir++"RbsSummaryFile.xml " ++ TftpBootDir++"org_RbsSummaryFile.xml"),
    Sed_CMD = "sed -e \"s|.*upgradePackageFilePath=.*|upgradePackageFilePath='"++TftpBootDir++Dir_Name++"'|\" "
	++TftpBootDir++"RbsSummaryFile.xml > "++TftpBootDir++"mod_RbsSummaryFile.xml",

    os:cmd(Sed_CMD),

    os:cmd("cp "++TftpBootDir++"mod_RbsSummaryFile.xml "++TftpBootDir++"RbsSummaryFile.xml"),

    A = string:tokens(os:cmd("cat "++TftpBootDir++"RbsSummaryFile.xml"), " \r[\"]"),
    ct:pal("Cat: ~n ~s",[A]),

    [{unpack_up_dir, UP_Dir}| Config].

%% @hidden
end_per_testcase(TestCase, Config) ->

    ct:pal("end_per_testcase Testcase: ~p.\n", [TestCase]),
    ct:pal("cleanup_in_tftpboot_dir"),

    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    UP_Dir = proplists:get_value(unpack_up_dir, Config),
    ct:pal("TftpBootDir: ~p.\n", [TftpBootDir]),
    ct:pal("UP_Dir: ~p.\n", [UP_Dir]),

    RmUPDirCmd = "rm -rf "++UP_Dir,
    send_os_cmd(RmUPDirCmd),

    os:cmd("cp "++TftpBootDir++"org_RbsSummaryFile.xml " ++ 
	       TftpBootDir++"RbsSummaryFile.xml"),

    os:cmd("rm "++TftpBootDir++"org_RbsSummaryFile.xml "),
    os:cmd("rm "++TftpBootDir++"mod_RbsSummaryFile.xml "),

    Ls = send_os_cmd("ls "++TftpBootDir),
    ct:pal("ls ~p : ~p", [TftpBootDir, Ls]),


    %%remove SIF in priv dir
    SiteInstallationFile = proplists:get_value(sif, Config),
    remove_sif_in_tftp_dir(Config, SiteInstallationFile),


    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
	    ct:pal("Testcase failed due to: ~p. Export logs. \n", [Reason]),
	    nl_lib:export_ai_log(Config, ?Protocol),
	    nl_lib:export_esi(Config, ?Protocol)
    end,
    ok.

string_token(Str) ->
    ListOfStr = string:tokens(Str, "\n"),
    ListOfStr.

string_tokens(Str) ->
    string:tokens(Str, " \n").

get_node_up(TftpBootDir)->

    UpfileList = filelib:wildcard(TftpBootDir++"*.{tgz,cxs,zip}"),
    UpfileString = lists:flatten(UpfileList),
    Up = filename:basename(UpfileString),
    Up. 

send_os_cmd(Cmd) ->
    ct:log("# os cmd #: ~p", [Cmd]),
    Answ = os:cmd(Cmd),
    ct:log("# Answ : ~p", [Answ]),
    Answ.
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
    [nodeUC640_N_010
    ].


%%%--------------------------------------------------------------------
%%% @doc Download -> Integrate.
%%% @spec nodeUC640_N_010(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nodeUC640_N_010(Config) ->

    ct:pal("nodeUC640_N_010:~n" 
	   "Run on both Sec/UnSec board.~n"
	   "Board restore -> Download with an unpacked UP -> Integrate.", []),

    %%%
    %% Precondition. Board restore.
    %%%
    board_restore(Config),

    %%%%
    %% Download.
    %%%%

    %% SIF file = "SiteInstallationFile.xml",
    SiteInstallationFile =  proplists:get_value(sif, Config),
    generate_sif(Config, SiteInstallationFile),

    
    download(Config, SiteInstallationFile, no_local_file),
    timer:sleep(10000),    

    %%%%
    %% Integrate
    %%%%  
    ct:log("##: Integrate"),
    integrate(Config),

    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
board_restore(Config) ->
    ct:log("Precondition: perform board restore."),
    nl_lib:board_restore(Config, console, ?NC, ?Protocol),
    ct:log("Board i restored. Now test can start."),
    timer:sleep(5000).

download(Config, SiteInstallationFile, Localfile) ->
  
    ct:log("# Start Download: LMT Integration on-stie configuration."),
    aic_httpc:download_files_lmt_on_site_conf_or_zt_off_site_pre_conf(Config, 
								      console, 
								      SiteInstallationFile, 
								      Localfile),

    ct:log("# Done download: LMT Integration on-stie configuration."),
   
    timer:sleep(5000).


integrate(Config) ->

    ct:log("# Start: LMT Integration on-stie configuration."),
    aic_httpc:integrate_lmt_int_on_site_conf(Config, console, nc1),
    ct:log("# Done: LMT Integration on-stie configuration."),
    ct:log("sleep 60sec to make sure node is up and ready for next tests!"),
    timer:sleep(60000).


generate_sif(Config, SiteInstallationFile)->

    ct:log("Generate SIF and write to tftpboot dir"),  
    HwId = list_to_atom(proplists:get_value(hw, Config)),
    ct:pal("Hwid: ~p",[HwId]),

    ok = nl_lib:gen_and_write_sif_lmt_on_site_or_zt_off_site(SiteInstallationFile, 
							"RbsSummaryFile.xml", 
							HwId),

    %% Check SIF exist in tftpboot dir
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:log("ls : ~p ", [Ls]),
    true = lists:member(SiteInstallationFile, Ls).


remove_sif_in_tftp_dir(Config, SiteInstallationFile)->
    
    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    RmCmd = "rm "++TftpBootDir++SiteInstallationFile,
    Rm = os:cmd(RmCmd),
    ct:log("Mv : ~p ", [Rm]),

    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:log("ls : ~p ", [Ls]),
    false = lists:member("SiteInstallationFile.xml", Ls).




    
