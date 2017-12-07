%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	zt_integration_off_site_pre_conf_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R5A/R6A/R9A/1
%%%
%%% @doc == AI zero touch integration off-site pre-conf test with SIF. ==
%%%
%%%
%%% @end

-module(zt_integration_off_site_pre_conf_SUITE).
-author('etxmlar').
-vsn('/main/R5A/R6A/R9A/1').
-date('2017-03-07').

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
%%% Rev      Date        Name        What
%%% -----    ---------   --------    ------------------------
%%% R5A/1    2016-05-25  etxmlar     Created
%%% R5A/2    2016-05-25  etxmlar     Updated and checked in
%%% R5A/3    2016-06-07  etxmlar     Updated with new TC name to 
%%%                                  correspond with testspecification.
%%% R6A/1    2016-08-26  etxmlar     Deselect Browse button TC 
%%%                                  nodeUC640_N_006 (using local_file)
%%% R9A/1    2017-03-07  etxmlar     Changed timeout to total_timeout
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
	 nodeUC640_N_002/1,
	 nodeUC640_N_006/1
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

    SiteInstallationFile = "SiteInstallationFile.xml",

    [{tftpboot_dir, TftpBootDir},
     {hw, HW},
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
init_per_testcase(_TestCase, Config) ->
    Config.
%% @hidden
end_per_testcase(TestCase, Config) 
  when TestCase == nodeUC640_N_002;
       TestCase == nodeUC640_N_006 ->

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
    [nodeUC640_N_002
     %%nodeUC640_N_006
    ].


%%%--------------------------------------------------------------------
%%% @doc Download -> power off/on -> integrate.
%%% @spec nodeUC640_N_002(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nodeUC640_N_002(Config) ->

    ct:pal("nodeUC640_N_002:~n" 
	   "Run on both Sec/UnSec board, Automatic perform integrate after power on.~n"
	   "Board restore -> Download -> power off/on -> integrate.", []),

    %%%
    %% Precondition. Board restore.
    %%%%
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
    %% Power off
    %%%%
    ok = ct:pal("### power off!",[]),
    ok = rct_power:off(node),
    timer:sleep(10000),    

    %%%%
    %% Power on
    %%%%    
    ct:pal("### power on!",[]),
    ok = rct_power:on(node),

    {ok, _} = ct_telnet:expect(console,
    			       "Ericsson Version:", 
    			       [{total_timeout,60000},no_prompt_check]),
    ct_telnet:expect(console,
    		     "Mounting /sys",
    		     [{total_timeout,300000},no_prompt_check]),

    timer:sleep(30000),
    nl_lib:wait_for_netconf_started(?NC),
    timer:sleep(30000),
    nl_lib:site_config_complete(?NC, 60000),

    ok.

%--------------------------------------------------------------------
%%% @doc Download -> power off/on -> integrate.
%%% @spec nodeUC640_N_006(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nodeUC640_N_006(Config) ->

    ct:pal("nodeUC640_N_006: ~n" 
	   "Run on both Sec/UnSec board, Automatic perform integrate after power on. ~n"
	   "Board restore -> Download -> power off/on -> integrate.", []),

    %%%
    %% Precondition. Board restore.
    %%%%
    board_restore(Config),

    %%%%
    %% Download.
    %%%%

    %% SIF file = "SiteInstallationFile.xml",
    SiteInstallationFile =  proplists:get_value(sif, Config),
    generate_sif(Config, SiteInstallationFile),

    
    download(Config, SiteInstallationFile, local_file),
    timer:sleep(10000),    

    %%%%
    %% Power off
    %%%%
    ok = ct:pal("### power off!",[]),
    ok = rct_power:off(node),
    timer:sleep(10000),    

    %%%%
    %% Power on
    %%%%    
    ct:pal("### power on!",[]),
    ok = rct_power:on(node),

    {ok, _} = ct_telnet:expect(console,
    			       "Ericsson Version:", 
    			       [{total_timeout,60000},no_prompt_check]),
    ct_telnet:expect(console,
    		     "Mounting /sys",
    		     [{total_timeout,300000},no_prompt_check]),

    timer:sleep(30000),
    nl_lib:wait_for_netconf_started(?NC),
    timer:sleep(30000),
    nl_lib:site_config_complete(?NC, 60000),

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
  
    ct:log("# Start Download: Zero Touch Integration off-stie pre-configuration."),
    aic_httpc:download_files_lmt_on_site_conf_or_zt_off_site_pre_conf(Config, console, SiteInstallationFile, Localfile),
    ct:log("# Done download: Zero Touch Integration off-stie pre-configuration."),
    timer:sleep(5000).

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




    
