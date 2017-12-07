%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmt_integration_on_site_conf_self_upgrade_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2017
%%% @version /main/R7A/R8A/R9A/4
%%%
%%% @doc == AI user interface tests.==
%%%
%%%
%%% @end

-module(lmt_integration_on_site_conf_self_upgrade_SUITE).
-author('etxmlar').
-vsn('/main/R7A/R8A/R9A/4').
-date('2017-06-21').

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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R7A/1      2017-01-13 etxmlar     Created 
%%% R7A/2      2017-01-13 etxmlar     Updated 
%%% R7A/3-4    2017-01-17 etxmlar     Updated 
%%% R8A/1      2017-01-19 etxmlar     Updated 
%%% R8A/2      2017-01-23 etxmlar     Updated for DUS33/DUS53/c608
%%% R9A/1      2017-01-24 etxmlar     Updated for c608
%%% R9A/2      2017-01-25 etxmlar     Updated for dus6303 and dus6502
%%% R9A/3      2017-04-02 etxderb     X3 UP: Downgrade to NL supp bootsig AXM5600
%%% R9A/4      2017-06-21 etxmlar     X3 UP: Downgrade to NL support for ee on secure locked hw
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
	 nodeUC640_N_011/1
	]).


-define(NC, nc1).
-define(DefaultProt, "https").
-define(DefaultPort, "8080").

-define(TCU_UP_WITH_NL_VERSION_R4AV01, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_3/R16DZ/CXP9024419_3-R16DZ.zip"). %%NL version CNX9012629-R4AV01
-define(DUS_UP_WITH_NL_VERSION_R4AV01, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_2/R16JC/CXP9024418_2-R16JC.zip").  %%NL version CNX9012629-R4AV01

-define(C608_UP_WITH_NL_VERSION_R8N04, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_6/R5A278/CXP9024419_6-R5A278.zip").  %%NL version CNX9012629-R8N04

%% -define(DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_VERSION_R8N04, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_6/R4A241/CXP9024418_6-R4A241.zip").  %%NL version CNX9012629-R8N04

%% -define(DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_DETECTING_AXM5600_BOOTSIG, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_6/R11A122/CXP9024418_6-R11A122.zip"). %% NL R9U04

%%-define(DUS53_DUS33_DUS6303_DUS6502_UP_WITH_FIRST_WORKING_NL_EE_ON_SEC_UNLOCKED_HW, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_6/R12B44/CXP9024418_6-R12B44.zip"). %% NL R9Y01

-define(DUS53_DUS33_DUS6303_DUS6502_UP_WITH_FIRST_WORKING_NL_EE_ON_SEC_LOCKED_HW, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_6/R12B73/CXP9024418_6-R12B73.zip"). %% NL R9AA01

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
			 {rct_rpc, rpc},
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
    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:log("ls : ~p ", [Ls]),
    
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

init_per_testcase(TestCase, Config) 
  when TestCase == nodeUC640_N_011 ->
    ct:pal("init TestCase: ~p",[TestCase]),

    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    TftpUP = proplists:get_value(tftp_up, Config), 

    %% cp UP to tmp dir in priv_dir
    TmpPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++TmpPath),
    os:cmd("mkdir "++TmpPath++"tmp"),
    TmpDir =TmpPath++"tmp/",
    ct:pal("TmpDir: ~p", [TmpDir]),
    os:cmd("cp "++TftpBootDir++TftpUP++" "++TmpDir),
    LS_TmpDir = string_tokens(os:cmd("ls "++TmpDir)),
    ct:log("Ls TmpDir: ~p", [LS_TmpDir]),

    SiteInstallationFile = "SiteInstallationFile.xml",

    [{tmp_dir, TmpDir}, 
     {sif, SiteInstallationFile} | Config];

init_per_testcase(_TestCase, Config) ->
    Config.

%% @hidden
end_per_testcase(TestCase, Config) 
  when TestCase == nodeUC640_N_011 ->
    ct:log("end TestCase: ~p",[TestCase]),

    %%remove SIF in priv dir
    SiteInstallationFile = proplists:get_value(sif, Config),
    remove_sif_in_tftp_dir(Config, SiteInstallationFile),

    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:pal("Testcase failed due to: ~p. Export logs. \n", [Reason]),
    	    nl_lib:export_ai_log(Config, ?DefaultProt),
    	    nl_lib:export_esi(Config, ?DefaultProt)	  		
    end,
    ok.


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
    [nodeUC640_N_011].

%%%--------------------------------------------------------------------
%%% @doc Board restore (sda2) -> Download (Upgrade NL) -> integrate.
%%% @spec nodeUC640_N_011(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nodeUC640_N_011(Config) ->
   
    ct:pal("nodeUC640_N_011:~n" 
	   "Run on both Sec/UnSec board.~n"
	   "Board restore -> Download with a self Upgrade -> Integrate.", []),

    %%%%
    %% Precondition: Board restore.
    %%%%
    ct:pal("Precondition: Board restore."),
    board_restore(Config),
    ct:pal("Board is restored."),

    %% Get NL version on node
    NodeNLVersion = get_nl_version_node(Config),
    ct:log("Network Loader Version on Node: ~p", [NodeNLVersion]),

    %% NL downgrade to make a self upgrade possible
    ct:pal("Do NL downgrade to force a self upgrade."),
    BoardType = get_board_type(),
    HW =  proplists:get_value(hw, Config), 

    case BoardType of	 
	BoardType when 	BoardType == "tcu04";				
			BoardType == "tcu0401" ->
	    download_config_in_tftpboot_dir(HW, ?TCU_UP_WITH_NL_VERSION_R4AV01);
	BoardType when 	BoardType == "dus5201";				
			BoardType == "dus3201" ->
	    download_config_in_tftpboot_dir(HW, ?DUS_UP_WITH_NL_VERSION_R4AV01);
	BoardType when 	BoardType == "dus5301";				
			BoardType == "dus3301";
			BoardType == "dus6303";
			BoardType == "dus6502" ->
	    download_config_in_tftpboot_dir(HW, ?DUS53_DUS33_DUS6303_DUS6502_UP_WITH_FIRST_WORKING_NL_EE_ON_SEC_LOCKED_HW);
	BoardType when 	BoardType == "c608"->
	    download_config_in_tftpboot_dir(HW, ?C608_UP_WITH_NL_VERSION_R8N04)
    end,
    
    aic_httpc:nl_upgrade(Config, console, "RbsSummaryFile.xml.nl"),

    TmpDir = proplists:get_value(tmp_dir, Config), 
    Org_UP =  proplists:get_value(tftp_up, Config), 
    Orignal_UP = filename:join(TmpDir, Org_UP),
    download_config_in_tftpboot_dir(HW, Orignal_UP),

    %%%%
    %% Download.
    %%%%

    %% SIF file = "SiteInstallationFile.xml",
    SiteInstallationFile =  proplists:get_value(sif, Config),
    generate_sif(Config, SiteInstallationFile),

    ct:log("# Start Download: LMT Integration on-stie configuration."),
    download(Config, SiteInstallationFile, no_local_file),
  
    %%%%
    %% Check Ailog after download
    %%%%
    ct:log("Check that Network Loader is upgraded to version: ~p ~n",[NodeNLVersion]),
    NLVersionStr =  lists:last(string:tokens(NodeNLVersion, "\- ")),
    NLStrList =
	["Upgrading Network Loader to version "++NLVersionStr,
	 "Running version: \""++NodeNLVersion++"\""],	

    check_after_download(Config, NLStrList),

    %%%%
    %% Integrate
    %%%%   
    ct:pal("### Start Integrate."),
    integrate(Config),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
board_restore(Config) ->
    aic_httpc:board_restore(Config, console),
    timer:sleep(10000),
    ok.

download(Config, SiteInstallationFile, Localfile) ->
    aic_httpc:download_files_lmt_on_site_conf_or_zt_off_site_pre_conf(Config, 
								      console, 
								      SiteInstallationFile, 
								      Localfile),

    timer:sleep(5000).

integrate(Config) ->
    aic_httpc:integrate_lmt_int_on_site_conf(Config, console, nc1),
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


get_nl_version_node(_Config) ->
    HW = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    _BOARDTYPE = ct:get_config({Hwa,board_type}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    
    start_needed_applications(?DefaultProt),

    HttpsUrl= "https://"++IP++"/help.html",
    HttpUrl= "http://"++IP++":"++?DefaultPort++"/help.html",
   
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
    
    stop_needed_applications(?DefaultProt),

    UrlListRes = string:tokens(UrlRes, " \n\t"),  

    NLNodeVerStr = 
	lists:flatten([VersionStr||VersionStr<-UrlListRes, lists:prefix("CNX", VersionStr)]),
    
    %% NLNodeVerStr = "CNX9012629-R4AC07<p>"
    NLNodeVersionStr = lists:flatten(string:tokens(NLNodeVerStr, "<p>")),
    NLNodeVersionStr.

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

check_after_download(_Config, ExpectStrList) ->

    HW = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    Hw = list_to_atom(HW),
    _BOARDTYPE = ct:get_config({Hwa,board_type}),
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
    
    start_needed_applications(?DefaultProt),

    HttpsUrl= "https://"++IP++"/ailog.txt",
    HttpUrl= "http://"++IP++":"++?DefaultPort++"/ailog.txt",
 
    UrlResStr =  
	case httpc:request(HttpsUrl) of
	    {ok,{{_,200,"OK"}, _ReplyHttpsA, ReplyHttpsB}} ->
		ct:log("ReplyHttpsB: ~p ~n",[ReplyHttpsB]),
		ReplyHttpsB;
	    {error, Reason} ->
		case httpc:request(HttpUrl) of
		    {ok,{{_,200,"OK"}, _ReplyHttpA, ReplyHttpB}} ->
			ct:log("ReplyHttpB: ~p ~n",[ReplyHttpB]),
			ReplyHttpB; 
		    {error, Reason} ->
			ct:fail("TC will fail due to unknown protocol.")
		end
	end,
    
    stop_needed_applications(?DefaultProt),
    
    Options = [global,{capture, all, binary}],

    lists:foreach(fun(String) ->    
			  case re:run(UrlResStr, String, Options) of
			      {match, [[Value]]} ->
				  ct:log("Value: ~p ~n",[Value]),
				  ok;
			      nomatch ->
				  ct:fail("TC will fail due to nomatch in ailog.txt.")	
			  end 
		  end, 
		  ExpectStrList).

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

%%--------------------------------------------------------------------
download_config_in_tftpboot_dir(HW, UP)->
    ct:pal("Insert UP in tftpboot dir: ~p ",  [UP]),

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

get_board_type()->
    BoardType = proplists:get_value(
		  board_type,ct:get_config(
			       ct:get_config({test_nodes,1}))),

    ct:log("BoardType: ~p", [BoardType]),
    BoardType.

