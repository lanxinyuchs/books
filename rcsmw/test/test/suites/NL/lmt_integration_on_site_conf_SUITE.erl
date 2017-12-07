%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmt_integration_on_site_conf_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2016
%%% @version /main/R5A/R6A/1
%%%
%%% @doc == AI lmt integration on-site conf test with SIF.==
%%%
%%%
%%% @end

-module(lmt_integration_on_site_conf_SUITE).
-author('etxmlar').
-vsn('/main/R5A/R6A/1').
-date('2016-08-26').

%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%%                                  nodeUC640_N_005 (using local_file)
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
	 nodeUC640_N_001/1,
	 nodeUC640_N_005/1
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
  when TestCase == nodeUC640_N_001;
       TestCase == nodeUC640_N_005 ->

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
    [nodeUC640_N_001
     %%nodeUC640_N_005
    ].


%%%--------------------------------------------------------------------
%%% @doc Download -> Integrate.
%%% @spec nodeUC640_N_001(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nodeUC640_N_001(Config) ->

    ct:pal("nodeUC640_N_001:~n" 
	   "Run on both Sec/UnSec board.~n"
	   "Board restore -> Download -> Integrate.", []),

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

%%%--------------------------------------------------------------------
%%% @doc Download -> Integrate.
%%% @spec nodeUC640_N_005(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nodeUC640_N_005(Config) ->

    ct:pal("nodeUC640_N_005: ~n" 
	   "Run on both Sec/UnSec board. ~n"
	   "Board restore -> Download -> Integrate.", []),

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




    
