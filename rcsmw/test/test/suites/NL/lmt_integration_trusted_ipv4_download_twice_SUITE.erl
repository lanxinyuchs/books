%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmt_integration_trusted_ipv4_download_twice_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R7A/R8A/R9A/1
%%%
%%% @doc == AI lmt integration trusted network IPv4. ==
%%%
%%%
%%% @end

-module(lmt_integration_trusted_ipv4_download_twice_SUITE).
-author('etxmlar').
-vsn('/main/R7A/R8A/R9A/1').
-date('2017-02-07').

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
%%% R7A/1     2016-09-29  etxmlar     Created
%%% R7A/2     2016-09-29  etxmlar     Updated for TCU
%%% R8A/1     2017-01-20  etxmlar     Added support for new boardtypes
%%% R9A/1     2017-02-07  etxmlar     Updated for the new boardtypes
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
	 nodeUC640_A4_114/1
	]).


-define(NC, nc1).
-define(Protocol, "https").
-define(DefaultPort, "8080").

-define(DUS_UP_WITH_NL_VERSION_R6S04, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_5/R2DLD/CXP9024418_5-R2DLD.zip"). %%NL version CNX9012629-R6S04
-define(DUS_UP_WITH_NL_VERSION_R6Y01, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_5/R2EGN/CXP9024418_5-R2EGN.zip"). %%NL version CNX9012629-R6Y01

-define(TCU_UP_WITH_NL_VERSION_R6S04, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_5/R2GSX/CXP9024419_5-R2GSX.zip"). %%NL version CNX9012629-R6S04
-define(TCU_UP_WITH_NL_VERSION_R6Y02, "https://arm110-eiffel001.seli.gic.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_5/R2JBS/CXP9024419_5-R2JBS.zip"). %%NL version CNX9012629-R6Y02

-define(DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_VERSION_R8N04, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_6/R4A241/CXP9024418_6-R4A241.zip").  %%NL version CNX9012629-R8N04
-define(DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_VERSION_R9A03, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024418_6/R6A82/CXP9024418_6-R6A82.zip"). %%NL version CNX9012629-R9A03

-define(C608_UP_WITH_NL_VERSION_R8N04, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_6/R5A278/CXP9024419_6-R5A278.zip").  %%NL version CNX9012629-R8N04
-define(C608_UP_WITH_NL_VERSION_R9A05, "https://arm010-eiffel001.seki.rnd.ericsson.se:8443/nexus/content/repositories/public/com/ericsson/CXP9024419_6/R5A480/CXP9024419_6-R5A480.zip").  %%NL version CNX9012629-R9A05

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

    %% create tmp dir in priv_dir
    TmpPath=?config(priv_dir,Config),
    os:cmd("chmod 777 "++TmpPath),
    os:cmd("mkdir "++TmpPath++"tmp"),
    TmpDir =TmpPath++"tmp/",
    ct:pal("TmpDir: ~p", [TmpDir]),

    BoardType = get_boardtype(),

    [{tftpboot_dir, TftpBootDir},
     {tftp_up, TftpUP},
     {tmp_dir, TmpDir},
     {hw, HW},
     {board_type, BoardType}| Config].


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
  when TestCase == nodeUC640_A4_114 ->

    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    Org_UP = proplists:get_value(tftp_up, Config),
    TmpDir = proplists:get_value(tmp_dir, Config), 
    BoardType = proplists:get_value(board_type, Config),

    SiteInstallationFile = "SiteInstallationFileIpv4.xml",

    %% Check what TN_port to use depending on boardtype
    TN_port = nl_lib:get_tn_port_capitel(BoardType),
    
    %% cp original UP to priv_dir to use in testcase
    copy(Org_UP, TftpBootDir, TmpDir),

    [{sif, SiteInstallationFile},
     {tn_port, TN_port}| Config];

init_per_testcase(TestCase, Config) ->
    ct:log("TestCase: ~p ", [TestCase]),

    Config.

%% @hidden


end_per_testcase(TestCase, Config)
  when TestCase == nodeUC640_A4_114 ->

    TftpBootDir = proplists:get_value(tftpboot_dir, Config),

    %% Remove RbsSummaryFile in tftpboot dir
    RmCmd = "rm "++TftpBootDir++"RbsSummaryFile.xml",
    Rm = os:cmd(RmCmd),
    ct:log("Rm: ~p ", [Rm]),

    %% Move back orig RbsSummaryFilein tftpboot dir
    MvCmd = "mv "++TftpBootDir++"org_RbsSummaryFile.xml "++TftpBootDir++"RbsSummaryFile.xml",
    Mv = os:cmd(MvCmd),
    ct:log("Mv : ~p ", [Mv]),

    %%remove SIF in priv dir
    SiteInstallationFile = proplists:get_value(sif, Config),
    remove_sif_in_priv_dir(Config, SiteInstallationFile),

    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:log("Testcase failed due to: ~p. Export logs. \n", [Reason]),
    	    nl_lib:export_ai_log(Config, ?Protocol),
    	    nl_lib:export_esi(Config, ?Protocol)	    
    end,

    ok;

end_per_testcase(TestCase, Config) ->

    ct:log("TestCase: ~p ", [TestCase]),
    case proplists:get_value(tc_status, Config) of
    	ok ->
    	    ok;
    	{failed, Reason}  ->
    	    ct:log("Testcase failed due to: ~p. Export logs. \n", [Reason]),
    	    nl_lib:export_ai_log(Config, ?Protocol),
    	    nl_lib:export_esi(Config, ?Protocol)	    
    end,

    ok.

string_token(Str) ->
    ListOfStr = string:tokens(Str, "\n"),
    ListOfStr.

mv_RbsSummaryFile_to_org(TftpBootDir)->
   
    %% mv RbsSummaryFile to org_RbsSummaryFile.
    MvCmd = "mv "++TftpBootDir++"RbsSummaryFile.xml "++TftpBootDir++"org_RbsSummaryFile.xml",
    Mv = os:cmd(MvCmd),
    ct:log("Mv : ~p ", [Mv]),
    Ls = string_token(os:cmd("ls "++TftpBootDir)),
    ct:log("ls : ~p ", [Ls]),
    true = lists:member("org_RbsSummaryFile.xml" ,Ls),
    false = lists:member("RbsSummaryFile.xml", Ls).

get_boardtype() ->
    BoardType = proplists:get_value(board_type,
				    ct:get_config(
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
    [nodeUC640_A4_114
    ].

%%%--------------------------------------------------------------------
%%% @doc  Download and Integrate with check
%%% @spec nodeUC640_A4_114(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nodeUC640_A4_114(Config) ->
    ct:log("nodeUC640_A4_114:~n" 
	   "Run on UnSec board, IPv4, default LMT IP adress, SMRS address = IP address~n"
	   "Board restore -> Download x 2 with NL upgrade -> Integrate"),


    %%%%
    %% Precondition: Board restore and downgrade NL to version CNX9012629-R6S04
    %%%%
    ct:log("Precondition: Perform Board restore and downgrade NL."),
    board_restore(Config),

    TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    TmpDir = proplists:get_value(tmp_dir, Config), 
    Hw = proplists:get_value(hw, Config),

    %% Put correct UP + config files in tftpboot
    BoardType = get_board_type(),
   
    NL_UP1_Str = 
	case BoardType of
	    BoardType when 	
		  BoardType == "tcu04";				
		  BoardType == "tcu0401" ->

		run_rcstprep(Config, ?TCU_UP_WITH_NL_VERSION_R6S04),

		%% cp old UP to priv_dir to use in unpacking the UP to find NL version
		TCU_UP_R6S04 = filename:basename(?TCU_UP_WITH_NL_VERSION_R6S04),
		copy(TCU_UP_R6S04, TftpBootDir, TmpDir),

		%% Check NL version in UP to verify against
		TCU_NL_R6S04_Str = get_nl_version_up(Config, TCU_UP_R6S04),

		ct:log("Network Loader Version in UP: ~p",[TCU_NL_R6S04_Str]),
		TCU_NL_R6S04_Str;

	    BoardType when 	
		  BoardType == "c608"->
		run_rcstprep(Config, ?C608_UP_WITH_NL_VERSION_R8N04),

		%% cp old UP to priv_dir to use in unpacking the UP to find NL version
		TCU_UP_R8N04 = filename:basename(?C608_UP_WITH_NL_VERSION_R8N04),
		copy(TCU_UP_R8N04, TftpBootDir, TmpDir),

		%% Check NL version in UP to verify against
		TCU_NL_R8N04_Str = get_nl_version_up(Config, TCU_UP_R8N04),

		ct:log("Network Loader Version in UP: ~p",[TCU_NL_R8N04_Str]),
		TCU_NL_R8N04_Str;

	    BoardType when 
		  BoardType == "dus5201";				
		  BoardType == "dus3201" ->

		run_rcstprep(Config, ?DUS_UP_WITH_NL_VERSION_R6S04),

		%% cp old UP to priv_dir to use in unpacking the UP to find NL version
		DUS_UP_R6S04 = filename:basename(?DUS_UP_WITH_NL_VERSION_R6S04),
		copy(DUS_UP_R6S04, TftpBootDir, TmpDir),

		%% Check NL version in UP to verify against
		DUS_NL_R6S04_Str = get_nl_version_up(Config, DUS_UP_R6S04),
		ct:log("Network Loader Version in UP: ~p",[DUS_NL_R6S04_Str]),		
		DUS_NL_R6S04_Str;

	    BoardType when 	
		  BoardType == "dus5301";				
		  BoardType == "dus3301";
		  BoardType == "dus6303";
		  BoardType == "dus6502" -> 

		run_rcstprep(Config, ?DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_VERSION_R8N04),

		%% cp old UP to priv_dir to use in unpacking the UP to find NL version
		DUS_UP_R8N04 = filename:basename(?DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_VERSION_R8N04),
		copy(DUS_UP_R8N04, TftpBootDir, TmpDir),

		%% Check NL version in UP to verify against
		DUS_NL_R8N04_Str = get_nl_version_up(Config, DUS_UP_R8N04),
		ct:log("Network Loader Version in UP: ~p",[DUS_NL_R8N04_Str]),		
		DUS_NL_R8N04_Str
	end,
    
    nl_upgrade(Config, NL_UP1_Str,  "RbsSummaryFile.xml.nl"),
    ct:log("Board is restored and NL is downgraded. Now test can start."),

    %%%%
    %% First Download with upgrade NL
    %%%%
    
    ct:log("# Start: First download LMT Integration. ~n"
	   "  Check that an NL upgrade will happen."),

    NL_UP2_Str = 
	case BoardType of
	    BoardType when 	
		  BoardType == "tcu04";				
		  BoardType == "tcu0401"->

		%% Put correct UP + config files in tftpboot
		run_rcstprep(Config, ?TCU_UP_WITH_NL_VERSION_R6Y02),

		%% First move the original RbsSummaryFile
		mv_RbsSummaryFile_to_org(TftpBootDir),

		%% Generat a new RbsSummaryFile for smrs use
		nl_lib:gen_and_write_RbsSummaryFile("RbsSummaryFile.xml", Hw, TftpBootDir),

		%% cp old UP to priv_dir to use in unpacking the UP to find NL version
		TCU_UP_R6Y02 = filename:basename(?TCU_UP_WITH_NL_VERSION_R6Y02),
		copy(TCU_UP_R6Y02, TftpBootDir, TmpDir),

		%% Check NL version in UP to verify against
		TCU_NL_R6Y02_Str = get_nl_version_up(Config, TCU_UP_R6Y02),
		ct:log("First download: Network Loader Version in UP: ~p",[TCU_NL_R6Y02_Str]),
		TCU_NL_R6Y02_Str;

	    BoardType when
		  BoardType == "dus5201";				
		  BoardType == "dus3201" ->

		%% Put correct UP + config files in tftpboot
		run_rcstprep(Config, ?DUS_UP_WITH_NL_VERSION_R6Y01),

		%% First move the original RbsSummaryFile
		mv_RbsSummaryFile_to_org(TftpBootDir),

		%% Generat a new RbsSummaryFile for smrs use
		nl_lib:gen_and_write_RbsSummaryFile("RbsSummaryFile.xml", Hw, TftpBootDir),

		%% cp old UP to priv_dir to use in unpacking the UP to find NL version
		DUS_UP_R6Y01 = filename:basename(?DUS_UP_WITH_NL_VERSION_R6Y01),
		copy(DUS_UP_R6Y01, TftpBootDir, TmpDir),

		%% Check NL version in UP to verify against
		DUS_NL_R6Y01_Str = get_nl_version_up(Config, DUS_UP_R6Y01),
		ct:log("First download: Network Loader Version in UP: ~p",[DUS_NL_R6Y01_Str]),
		DUS_NL_R6Y01_Str;

	    BoardType when 	
		  BoardType == "c608"->
		run_rcstprep(Config, ?C608_UP_WITH_NL_VERSION_R9A05),

		%% cp old UP to priv_dir to use in unpacking the UP to find NL version
		TCU_UP_R9A05 =  filename:basename(?C608_UP_WITH_NL_VERSION_R9A05),
		copy(TCU_UP_R9A05, TftpBootDir, TmpDir),

		%% Check NL version in UP to verify against
		TCU_NL_R9A05_Str = get_nl_version_up(Config, TCU_UP_R9A05),

		ct:log("Network Loader Version in UP: ~p",[TCU_NL_R9A05_Str]),
		TCU_NL_R9A05_Str;

	    BoardType when 	
		  BoardType == "dus5301";				
		  BoardType == "dus3301";
		  BoardType == "dus6303";
		  BoardType == "dus6502" -> 

		run_rcstprep(Config, ?DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_VERSION_R9A03),

		%% cp old UP to priv_dir to use in unpacking the UP to find NL version
		DUS_UP_R9A03 = filename:basename(?DUS53_DUS33_DUS6303_DUS6502_UP_WITH_NL_VERSION_R9A03),
		copy(DUS_UP_R9A03, TftpBootDir, TmpDir),

		%% Check NL version in UP to verify against
		DUS_NL_R9A03_Str = get_nl_version_up(Config, DUS_UP_R9A03),
		ct:log("Network Loader Version in UP: ~p",[DUS_NL_R9A03_Str]),		
		DUS_NL_R9A03_Str

	end,
    

    %% SIF file = "SiteInstallationFileIpv4.xml",
    SiteInstallationFile =  proplists:get_value(sif, Config),
    TN_port = proplists:get_value(tn_port, Config),
    
    generate_sif_ipv4_trusted(Config, SiteInstallationFile, TN_port),
    download_with_nl_check(Config, SiteInstallationFile, NL_UP2_Str),

    ct:log("# Done: First download LMT Integration."),

    %% Remove RbsSummaryFile in tftpboot dir
    RmCmd = "rm "++TftpBootDir++"RbsSummaryFile.xml",
    Rm = os:cmd(RmCmd),
    ct:log("Rm: ~p ", [Rm]),

    %% Move back orig RbsSummaryFilein tftpboot dir
    MvCmd = "mv "++TftpBootDir++"org_RbsSummaryFile.xml "++TftpBootDir++"RbsSummaryFile.xml",
    Mv = os:cmd(MvCmd),
    ct:log("Mv : ~p ", [Mv]),

    %%%%
    %% Second Download with upgrade NL
    %%%%
    
    ct:log("# Start: Second download LMT Integration with latest NL. Check that an NL upgrade will happen."),

    %% Check NL version in UP to verify against
    Filename = proplists:get_value(tftp_up, Config),  
    Orignal_UP = filename:join(TmpDir, Filename),

    Installed_UP = filename:basename(Orignal_UP),
    NL_Str = get_nl_version_up(Config, Installed_UP),
    ct:log("Second download: Network Loader Version in UP: ~p",[NL_Str]),

    %% Put latest UP + config files in tftpboot
    run_rcstprep(Config, Orignal_UP),

    %% First move the original RbsSummaryFile
    mv_RbsSummaryFile_to_org(TftpBootDir),
    
    %% Generat a new RbsSummaryFile for smrs use
    nl_lib:gen_and_write_RbsSummaryFile("RbsSummaryFile.xml", Hw, TftpBootDir),
    
    download_with_nl_check(Config, SiteInstallationFile, NL_Str),  
    ct:log("# Done: Second download LMT Integration."),

    %%%%
    %% Integrate
    %%%%  
    integrate(Config),

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
    timer:sleep(5000).

nl_upgrade(Config, UpNLVerStr, NL_Upgrade_File)->
    ok = nl_lib:download_file_nl_upgrade(Config, console, NL_Upgrade_File, no_local_file, UpNLVerStr).


download_with_nl_check(Config, SiteInstallationFile, NL_R6Y01_Str) ->
   
  
    nl_lib:httpc_request_lmt_integration(Config, post, "Download", ?Protocol, 
					 ?DefaultPort, SiteInstallationFile),

    %%Get the boards TN port
    BoardType = proplists:get_value(board_type, Config),
    ct:log("BoardType: ~p", [BoardType]),

    TN_port =  nl_lib:get_tn_port_lower_case(BoardType),

    case  ct_telnet:expect(console,
			   "Enabling Northbound interface: "++TN_port, 
    			   [{timeout,120000},no_prompt_check]) of	    
    	{ok, _} ->
    	    {ok, _} = ct_telnet:expect(console,
				       TN_port++" enabled", 
    				       [{timeout,60000},no_prompt_check]),

	    {ok, _} = ct_telnet:expect(console,
				       "Vlan Interface Added", 
				       [{timeout,60000},no_prompt_check]),

	    {ok, _} = ct_telnet:expect(console,
				       "Default router added", 
				       [{timeout,60000},no_prompt_check]),

	    {ok, _} = ct_telnet:expect(console,
				       TN_port++" configured", 
				       [{timeout,60000},no_prompt_check]);
    	_Else ->
	    ct:fail(TN_port++" not enabled when Download files")
    end,

    nl_lib:wait_for_download_completed(Config, ?Protocol),
    timer:sleep(5000),

    UpVerStr = lists:last(string:tokens(NL_R6Y01_Str, "\- ")),
    ct:log("Check if Network Loader is upgraded to: ~p ~n",[UpVerStr]),
    check_nl_version(["Upgrading Network Loader to version "++UpVerStr,
		      "Running version: \""++NL_R6Y01_Str++"\""]),	
    
    ok.
	
check_nl_version(ExpStringList)->
    lists:foreach(fun(String) -> 
			  nl_lib:check_for_printout_in_ailog(String)
		  end, 
		  ExpStringList).	      


integrate(Config) ->

    ct:log("# Start Integrate: LMT integration."),
    aic_httpc:integrate_lmt_integration(Config, console, nc1),
    ct:log("# Integrate done: LMT integration."),
    ct:log("sleep 60sec to make sure node is up and ready for next tests!"),
    timer:sleep(60000).


generate_sif_ipv4_trusted(Config, SiteInstallationFile, TN_port)->

    ct:log("Generate SIF and write to priv dir"),  
    HwId = list_to_atom(proplists:get_value(hw, Config)),
    ct:pal("Hwid: ~p",[HwId]),

    ok = nl_lib:gen_and_write_sif_ipv4_trusted(Config,
					       SiteInstallationFile, 
					       "RbsSummaryFile.xml", 
					       HwId, TN_port),  

    %% Check SIF exist in priv dir
    Priv_dir = ?config(priv_dir, Config),
    Ls = string_token(os:cmd("ls "++Priv_dir)),

    ct:pal("ls : ~p ", [Ls]),
    true = lists:member(SiteInstallationFile, Ls).

remove_sif_in_priv_dir(Config, SiteInstallationFile)->

    Priv_dir = ?config(priv_dir, Config),
    RmCmd = "rm "++Priv_dir++SiteInstallationFile,
    Rm = os:cmd(RmCmd),
    ct:log("Rm: ~p ", [Rm]),

    Ls = string_token(os:cmd("ls "++Priv_dir)),
    ct:log("ls : ~p ", [Ls]),							
    ok.


get_node_up(TftpBootDir)->

    UpfileList = filelib:wildcard(TftpBootDir++"*.{tgz,cxs,zip}"),
    UpfileString = lists:flatten(UpfileList),
    Up = filename:basename(UpfileString),
    Up. 


run_rcstprep(Config, UP)->

    HW = proplists:get_value(hw, Config),
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

get_nl_version_up(Config, TftpUP) ->
    
    TmpDir = proplists:get_value(tmp_dir, Config), 
    Ls = string_token(os:cmd("ls "++TmpDir)),
    ct:pal("ls temp dir: ~p ", [Ls]),
    

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


copy(Filename, Fromdir, Todir) ->
    file:copy(filename:join(Fromdir, Filename), 
	      filename:join(Todir, Filename)).



get_board_type()->
    BoardType = proplists:get_value(
		  board_type,ct:get_config(
			       ct:get_config({test_nodes,1}))),

    ct:log("BoardType: ~p", [BoardType]),
    BoardType.
