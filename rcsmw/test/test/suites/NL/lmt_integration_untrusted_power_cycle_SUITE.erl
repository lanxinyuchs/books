%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmt_integration_untrusted_power_cycle_SUITE.erl %
%%% @author etxmlar
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R7A/R8A/R9A/8
%%%
%%% @doc == AI lmt integration trusted network with power cycle. ==
%%%
%%%
%%% @end

-module(lmt_integration_untrusted_power_cycle_SUITE).
-author('etxmlar').
-vsn('/main/R7A/R8A/R9A/8').
-date('2017-05-08').

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
%%% R7A/1     2016-10-03  etxmlar     Created
%%% R7A/2     2016-10-03  etxmlar     Updated and checked in
%%% R8A/1     2017-01-22  etxmlar     Added support for new boardtypes
%%% R9A/1     2017-01-23  etxmlar     Added temp. TC skip for DUS33, DUS53 and c608
%%% R9A/2     2017-01-25  etxmlar     Added temp. TC skip for DUS6303 and DUS6502
%%% R9A/3     2017-02-02  etxmlar     Updated temp. TC skip
%%% R9A/4     2017-02-20  etxmlar     Updated Temporary set ip 
%%%                                   route to get ai answere
%%% R9A/5     2017-03-15  etxmlar     Removed skip TC for dus33
%%% R9A/6     2017-03-21  etxmlar     Removed skip TC for c608
%%% R9A/7     2017-04-18  etxmlar     Removed skip TC for dus6303
%%% R9A/8     2017-05-08  etxmlar     Removed skip TC for dus5301
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
	 nodeUC640_A3_068/1,
	 nodeUC640_A3_108/1,
	 nodeUC640_A3_109/1,
	 nodeUC640_A3_110/1
	]).


-define(Protocol, "https").
-define(DefaultPort, "8080").
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
			 {rct_power,power},
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

    BoardType = get_boardtype(),

    [{tftpboot_dir, TftpBootDir},
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
  when TestCase == nodeUC640_A3_068;
       TestCase == nodeUC640_A3_109 ->

    %% First move the original RbsSummaryFile
    %% TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    %% Hw = proplists:get_value(hw, Config),
    %% mv_RbsSummaryFile_to_org(TftpBootDir),

    %% %% Generat a new RbsSummaryFile for smrs use
    %% nl_lib:gen_and_write_RbsSummaryFile("RbsSummaryFile.xml", Hw, TftpBootDir),


    %%==========================
    %% SiteInstallationFile = "SiteInstallationFileUntrustedIpv4.xml",

    BoardType = proplists:get_value(board_type, Config),
    %% %% Check what TN_port to use depending on boardtype
    %% TN_port = nl_lib:get_tn_port_capitel(BoardType),

    %% [{sif, SiteInstallationFile},
    %%  {tn_port, TN_port}| Config];

    %%Temporary skip TC for DUS53, no TN support yet
    ct:log("Check for DUS6502 board. ~n"
	   "TC temporary not valid for ~p no SECGW support yet in lab.", 
	   [BoardType]),

    case BoardType of
	BoardType when %%BoardType == "dus5301";				
		       %%BoardType == "dus3301";
		       %%BoardType == "dus6303";
		       BoardType == "dus6502" ->
		       %%BoardType == "c608" ->
	    {skip, "TC temporary skipped, because no SECGW support yet in lab for "++ BoardType};

	_BoardType ->
	    %% Check what TN_port to use depending on boardtype
	    TN_port = nl_lib:get_tn_port_capitel(BoardType),

	    SiteInstallationFile = "SiteInstallationFileUntrustedIpv4.xml",
	    [{sif, SiteInstallationFile},
	     {tn_port, TN_port}| Config]
    end;

init_per_testcase(TestCase, Config) 
  when TestCase == nodeUC640_A3_108;
       TestCase == nodeUC640_A3_110 ->

    %% First move the original RbsSummaryFile
    %% TftpBootDir = proplists:get_value(tftpboot_dir, Config),
    %% Hw = proplists:get_value(hw, Config),
    %% mv_RbsSummaryFile_to_org(TftpBootDir),

    %% %% Generat a new RbsSummaryFile for smrs use
    %% nl_lib:gen_and_write_RbsSummaryFile("RbsSummaryFile.xml", Hw, TftpBootDir),

    SiteInstallationFile = "SiteInstallationFileUntrustedIpv6.xml",

    BoardType = proplists:get_value(board_type, Config),
    %% Check what TN_port to use depending on boardtype
    TN_port = nl_lib:get_tn_port_capitel(BoardType),


    [{sif, SiteInstallationFile},
     {tn_port, TN_port}| Config];


init_per_testcase(TestCase, Config) ->
    ct:pal("TestCase: ~p",[TestCase]),
    Config.


%% @hidden


end_per_testcase(TestCase, Config)
  when TestCase == nodeUC640_A3_068;
       TestCase == nodeUC640_A3_109 ->

    %% TftpBootDir = proplists:get_value(tftpboot_dir, Config),

    %% %% Remove RbsSummaryFile in tftpboot dir
    %% RmCmd = "rm "++TftpBootDir++"RbsSummaryFile.xml",
    %% Rm = os:cmd(RmCmd),
    %% ct:log("Rm: ~p ", [Rm]),

    %% %% Move back orig RbsSummaryFilein tftpboot dir
    %% MvCmd = "mv "++TftpBootDir++"org_RbsSummaryFile.xml "++TftpBootDir++"RbsSummaryFile.xml",
    %% Mv = os:cmd(MvCmd),
    %% ct:log("Mv : ~p ", [Mv]),

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


end_per_testcase(TestCase, Config)
  when TestCase == nodeUC640_A4_108;
       TestCase == nodeUC640_A4_110 ->

    %% TftpBootDir = proplists:get_value(tftpboot_dir, Config),

    %% %% Remove RbsSummaryFile in tftpboot dir
    %% RmCmd = "rm "++TftpBootDir++"RbsSummaryFile.xml",
    %% Rm = os:cmd(RmCmd),
    %% ct:log("Rm: ~p ", [Rm]),

    %% %% Move back orig RbsSummaryFilein tftpboot dir
    %% MvCmd = "mv "++TftpBootDir++"org_RbsSummaryFile.xml "++TftpBootDir++"RbsSummaryFile.xml",
    %% Mv = os:cmd(MvCmd),
    %% ct:log("Mv : ~p ", [Mv]),

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

end_per_testcase(_TestCase, _Config) ->
    ok.


string_token(Str) ->
    ListOfStr = string:tokens(Str, "\n"),
    ListOfStr.

%% mv_RbsSummaryFile_to_org(TftpBootDir)->
   
%%     %% mv RbsSummaryFile to org_RbsSummaryFile.
%%     MvCmd = "mv "++TftpBootDir++"RbsSummaryFile.xml "++TftpBootDir++"org_RbsSummaryFile.xml",
%%     Mv = os:cmd(MvCmd),
%%     ct:log("Mv : ~p ", [Mv]),
%%     Ls = string_token(os:cmd("ls "++TftpBootDir)),
%%     ct:log("ls : ~p ", [Ls]),
%%     true = lists:member("org_RbsSummaryFile.xml" ,Ls),
%%     false = lists:member("RbsSummaryFile.xml", Ls).


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
    [nodeUC640_A3_068,
     %%nodeUC640_A3_108,
     nodeUC640_A3_109
     %%nodeUC640_A3_110
    ].

%%%--------------------------------------------------------------------
%%% @doc Download and Integrate no check
%%% @spec nodeUC640_A3_068(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nodeUC640_A3_068(Config) ->
    ct:log("nodeUC640_A3_068:~n"
	   "Run on UnSec board, IPv4, default LMT IP adress, SMRS address = IP address~n"
	   "Board restore -> Download with power cycle -> Download/Integrate"),
    
    %%%%
    %% Precondition: Board restore.
    %%%%
    ct:log("Precondition: perform board restore."),
    board_restore(Config),

    %%%%
    %% Download
    %%%%
    
    %% SIF file = "SiteInstallationFileIpv4.xml",
    SiteInstallationFile =  proplists:get_value(sif, Config),
    TN_port_capitel = proplists:get_value(tn_port, Config),
    generate_sif_ipv4_untrusted(Config, SiteInstallationFile, TN_port_capitel),
    
    %%%
    %% Power cycle 1
    %%%
    set_temporary_ip_route(),
    ct:pal("start power cycle test 1.  Power cycle random during download progress."),
    start_download(Config, SiteInstallationFile),
    random_sleep(50),
    ct:pal("## Start power cycle."),
    rct_power:cycle(power),
    ct:pal("## Done power cycle."),

    check_after_power_on(),

    %%%
    %% Power cycle 2
    %%%
    set_temporary_ip_route(),
    ct:pal("start power cycle test 2. Power cycle when start download."),
    start_download(Config, SiteInstallationFile),
    ct:pal("## Start power cycle."),
    rct_power:cycle(power),
    ct:pal("## Done power cycle."),

    check_after_power_on(),

    %%%
    %% Power cycle 3
    %%%

    BoardType = proplists:get_value(board_type, Config),
    TN_port = nl_lib:get_tn_port_lower_case(BoardType),

    set_temporary_ip_route(),
    ct:pal("start power cycle test 3. Power cycle when Enabling Northbound interface."),
    start_download(Config, SiteInstallationFile),
    {ok, _} = ct_telnet:expect(console,
    			       "Enabling Northbound interface: "++TN_port, 
    			       [{timeout,120000},no_prompt_check]),
    ct:pal("## Start power cycle."),
    rct_power:cycle(power),
    ct:pal("## Done power cycle."),

    check_after_power_on(),

    %%%%
    %% Power cycle 4
    %%%%
    set_temporary_ip_route(),
    ct:pal("startpower cycle test 4. Power cycle after Default router added"),
    start_download(Config, SiteInstallationFile),
    {ok, _} = ct_telnet:expect(console,
    			       "Default router added", 
    			       [{timeout,120000},no_prompt_check]),

    ct:pal("## Start power cycle."),
    rct_power:cycle(power),
    ct:pal("## Done power cycle."),

    check_after_power_on(),
    
    %%%%
    %% Download
    %%%%
    set_temporary_ip_route(),
    download_with_expects_ipv4(Config, SiteInstallationFile),

    %%%%
    %% Integrate
    %%%%   
    integrate(Config),
    
    ok.


%%%--------------------------------------------------------------------
%%% @doc Download and Integrate no check
%%% @spec nodeUC640_A3_108(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nodeUC640_A3_108(Config) ->
    ct:log("nodeUC640_A3_108:~n"
	   "Run on UnSec board, IPv6, default LMT IP adress, SMRS address = IP address~n"
	   "Board restore -> Download with power cycle -> Download/Integrate"),
    
    %%%%
    %% Precondition: Board restore.
    %%%%
    ct:log("Precondition: perform board restore."),
    board_restore(Config),

    %%%%
    %% Download
    %%%%
    
    %% SIF file = "SiteInstallationFileIpv6.xml",
    SiteInstallationFile =  proplists:get_value(sif, Config),
    TN_port_capitel = proplists:get_value(tn_port, Config),
    generate_sif_ipv6_untrusted(Config, SiteInstallationFile, TN_port_capitel),
    
   %%%
    %% Power cycle 1
    %%%
    set_temporary_ip_route(),
    ct:pal("start power cycle test 1.  Power cycle random during download progress."),
    start_download(Config, SiteInstallationFile),
    random_sleep(50),
    ct:pal("## Start power cycle."),
    rct_power:cycle(power),
    ct:pal("## Done power cycle."),

    check_after_power_on(),

    %%%
    %% Power cycle 2
    %%%
    set_temporary_ip_route(),
    ct:pal("start power cycle test 2. Power cycle when start download."),
    start_download(Config, SiteInstallationFile),
    ct:pal("## Start power cycle."),
    rct_power:cycle(power),
    ct:pal("## Done power cycle."),

    check_after_power_on(),

    %%%
    %% Power cycle 3
    %%%

    BoardType = proplists:get_value(board_type, Config),
    TN_port = nl_lib:get_tn_port_lower_case(BoardType),

    set_temporary_ip_route(),
    ct:pal("start power cycle test 3. Power cycle when Enabling Northbound interface."),
    start_download(Config, SiteInstallationFile),
    {ok, _} = ct_telnet:expect(console,
			       "Enabling Northbound interface: "++TN_port,
    			       [{timeout,120000},no_prompt_check]),
    ct:pal("## Start power cycle."),
    rct_power:cycle(power),
    ct:pal("## Done power cycle."),

    check_after_power_on(),

    %%%%
    %% Power cycle 4
    %%%%
    set_temporary_ip_route(),
    ct:pal("start power cycle 4. Power cycle after Default router added"),
    start_download(Config, SiteInstallationFile),
    {ok, _} = ct_telnet:expect(console,
    			       "Default router added", 
    			       [{timeout,120000},no_prompt_check]),

    ct:pal("## Start power cycle."),
    rct_power:cycle(power),
    ct:pal("## Done power cycle."),

    check_after_power_on(),

    %%%%
    %% Download
    %%%%
    set_temporary_ip_route(),
    download_with_expects_ipv6(Config, SiteInstallationFile),

    %%%%
    %% Integrate
    %%%%   
    integrate(Config),
    
    ok.

%%%--------------------------------------------------------------------
%%% @doc Download and Integrate no check
%%% @spec nodeUC640_A3_109(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nodeUC640_A3_109(Config) ->
    ct:log("nodeUC640_A3_109:~n"
	   "Run on Sec board, IPv4, default LMT IP adress, SMRS address = IP address~n"
	   "Board restore -> Download with power cycle -> Download/Integrate"),
    
    %%%%
    %% Precondition: Board restore.
    %%%%
    ct:log("Precondition: perform board restore."),
    board_restore(Config),

    %%%%
    %% Download
    %%%%
    
    %% SIF file = "SiteInstallationFileIpv4.xml",
    SiteInstallationFile =  proplists:get_value(sif, Config),
    TN_port_capitel = proplists:get_value(tn_port, Config),
    generate_sif_ipv4_untrusted(Config, SiteInstallationFile, TN_port_capitel),
    
    %%%
    %% Power cycle 1
    %%%
    set_temporary_ip_route(),
    ct:pal("start power cycle test 1.  Power cycle random during download progress."),
    start_download(Config, SiteInstallationFile),
    random_sleep(50),
    ct:pal("## Start power cycle."),
    rct_power:cycle(power),
    ct:pal("## Done power cycle."),

    check_after_power_on(),

    %%%
    %% Power cycle 2
    %%%
    set_temporary_ip_route(),
    ct:pal("start power cycle test 2. Power cycle when start download."),
    start_download(Config, SiteInstallationFile),
    ct:pal("## Start power cycle."),
    rct_power:cycle(power),
    ct:pal("## Done power cycle."),

    check_after_power_on(),
    
    %%%%
    %% Download
    %%%%
    set_temporary_ip_route(),
    download_no_expects(Config, SiteInstallationFile),

    %%%%
    %% Integrate
    %%%%   
    integrate(Config),
    
    ok.


%%%--------------------------------------------------------------------
%%% @doc Download and Integrate no check
%%% @spec nodeUC640_A3_110(Config) -> ok
%%% @end
%%%--------------------------------------------------------------------
nodeUC640_A3_110(Config) ->
    ct:log("nodeUC640_A3_110:~n"
	   "Run on Sec board, IPv6, default LMT IP adress, SMRS address = IP address~n"
	   "Board restore -> Download with power cycle -> Download/Integrate"),
    
    %%%%
    %% Precondition: Board restore.
    %%%%
    ct:log("Precondition: perform board restore."),
    board_restore(Config),

    %%%%
    %% Download
    %%%%
    
    %% SIF file = "SiteInstallationFileIpv4.xml",
    SiteInstallationFile =  proplists:get_value(sif, Config),
    TN_port_capitel = proplists:get_value(tn_port, Config),
    generate_sif_ipv6_untrusted(Config, SiteInstallationFile, TN_port_capitel),
    
    %%%
    %% Power cycle 1
    %%%
    set_temporary_ip_route(),
    ct:pal("start power cycle test 1.  Power cycle random during download progress."),
    start_download(Config, SiteInstallationFile),
    random_sleep(50),
    ct:pal("## Start power cycle."),
    rct_power:cycle(power),
    ct:pal("## Done power cycle."),

    check_after_power_on(),

    %%%
    %% Power cycle 2
    %%%
    set_temporary_ip_route(),
    ct:pal("start power cycle test 2. Power cycle when start download."),
    start_download(Config, SiteInstallationFile),
    ct:pal("## Start power cycle."),
    rct_power:cycle(power),
    ct:pal("## Done power cycle."),

    check_after_power_on(),

    %%%%
    %% Download
    %%%%
    set_temporary_ip_route(),
    download_no_expects(Config, SiteInstallationFile),

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
    ct:log("Board is restored. Now test can start."),
    timer:sleep(5000).

download_no_expects(Config, SiteInstallationFile) ->
   
    ct:log("# Start Download LMT Integration."),
    aic_httpc:download_files_lmt_integration(Config, console, SiteInstallationFile),
    ct:log("# Done download LMT Integration."),
    timer:sleep(5000).

start_download(Config, SiteInstallationFile) ->
    ct:pal("# Start Download."),
    nl_lib:httpc_request_lmt_integration(Config, post, "Download", ?Protocol, 
    					 ?DefaultPort, SiteInstallationFile),
    ct:pal("# download Started.").

download_with_expects_ipv4(Config, SiteInstallationFile) ->
   
      ct:log("# Start Download: LMT Integration."),

    nl_lib:httpc_request_lmt_integration(Config, post, "Download", ?Protocol, 
					 ?DefaultPort, SiteInstallationFile),

    %%Get the boards TN port
    BoardType = proplists:get_value(board_type, Config),
    ct:log("BoardType: ~p", [BoardType]),

    TN_port =  nl_lib:get_tn_port_lower_case(BoardType),

    %% Check Enabling Northbound interface configuration
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

    %% Check Secure Gateway configuration

    %% What to chcek regaarding the SecGateway setup
    %% - Establishing IPsec tunnel : Started
    %% - parsed IKE_SA_INIT response 0 
    %% - parsed IKE_AUTH response
    %% - authentication of 'C=SE, ST=Stockholm, L=Kista, O=Labbet, CN=JUNIPER_SRX_UABHMIK' with RSA signature successful (vendor cert)-> Check att det är successful 
    %% - connection 'vc_peer' established successfully
    %% - Secure tunnel established
    %% - Establishing IPsec tunnel : Finished
    

    ct:log("Establishing IPsec tunnel : Started"),

    case  ct_telnet:expect(console,
			   "parsed IKE_SA_INIT response 0", 
    			   [{timeout,60000},no_prompt_check]) of
	{ok,_} ->
	    case  ct_telnet:expect(console,
				   "parsed IKE_AUTH response", 
				   [{timeout,60000},no_prompt_check]) of
		{ok,_} ->
		    continue;
		_IKE_AUTH ->
		    ct:fail("No parsed IKE_AUTH response")
	    end;
	_IKE_SA_INIT ->
	    ct:fail("No parsed IKE_SA_INIT response 0")
    end,

   
    %%Example: authentication of 'C=SE, ST=Stockholm, L=Kista, O=Labbet, CN=JUNIPER_SRX_UABHMIK' with RSA signature successful
    %%{ok, Data} = ct_telnet:cmd(console,"cat /nl/log/nl_debug.1 | grep with RSA signature successful (vendor cert)"),
    %%ok  = ct_telnet:send(console,"cat /nl/log/nl_debug.1 | grep with RSA signature successful (vendor cert)"),
    %% ct_telnet:cmd(console,"cat /nl/log/nl_debug.1 | grep with RSA signature successful (vendor cert)"),
    %%{ok, _Data} = ct_telnet:get_data(console),


    %% {ok, _} = ct_telnet:expect(console, 
    %% 			       "with RSA signature successful (vendor cert)",
    %% 			       [{timeout,120000},no_prompt_check]),


    case  ct_telnet:expect(console,
			   "connection 'vc_peer' established successfully", 
			   [{timeout,60000},no_prompt_check]) of
	{ok,_} ->
	    {ok, _} = ct_telnet:expect(console,
				       "Secure tunnel established", 
				       [{timeout,60000},no_prompt_check]),

	    {ok, _} = ct_telnet:expect(console,
				       "Establishing IPsec tunnel : Finished", 
				       [{timeout,60000},no_prompt_check]);

	_VC_PEER ->
	    ct:fail("Connection 'vc_peer' established unsuccessfully")
    end,

    nl_lib:wait_for_download_completed(Config, ?Protocol),
    ct:log("# Download done: LMT Integration."),
    timer:sleep(5000),

    %%Check for InterfaceId
    InterfaceId = nl_lib:get_interface_id(BoardType, TN_port),
    check_ipv4_after_download(Config, InterfaceId),

    ok.

 
check_ipv4_after_download(Config, InterfaceId)->


    %% ok = ct_telnet:send(console, "cat /nl/log/nl_debug.1"),
    %% {ok, _} = ct_telnet:expect(console,
    %% 			       "grep with RSA signature successful (vendor cert)", 
    %% 			       [{timeout,300000},no_prompt_check]),

    HwId = list_to_atom(proplists:get_value(hw, Config)),
    ct:pal("Hwid: ~p",[HwId]),

    [{ssh,_Inner_IpAddress}, {ssh_outer, Out_IpAddress}, {netmask_outer, _Netmask_Outer}, 
     {vlan, VlanId}, {gateway_outer, _Gateway_Outer}] = ct:get_config({HwId, ssh_TN_A_ipv4_ipsec}),

    VlanId_Str = integer_to_list(VlanId),

    ct:pal("VlanId: ~p~n"
	   "Out_IpAddress: ~p~n",
	   [VlanId_Str, Out_IpAddress]),

    ok  = ct_telnet:send(console,"ifconfig"),
    {ok, _Data} = ct_telnet:get_data(console),
    timer:sleep(5000),

    ok  = ct_telnet:send(console,"ifconfig"),
    case ct_telnet:expect(console, 
			  InterfaceId,[{timeout,60000}, no_prompt_check]) of

	{ok, _} ->
    	    {ok, _} = ct_telnet:expect(console,
				       InterfaceId++"."++VlanId_Str, 
    				       [{timeout,60000},no_prompt_check]),

	    {ok, _} = ct_telnet:expect(console,
				       "inet addr:"++Out_IpAddress, 
				       [{timeout,60000},no_prompt_check]);

	_Other ->
	    ct:fail(InterfaceId++", interface not added")
    end,

    ok.

download_with_expects_ipv6(Config, SiteInstallationFile) ->
   
    ct:log("# Start Download: LMT Integration."),

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
    ct:log("# Download done: LMT Integration."),
    timer:sleep(5000),
 
    %%Check for InterfaceId
    InterfaceId = nl_lib:get_interface_id(BoardType, TN_port),
    check_ipv6_after_download(Config, InterfaceId),

    ok.

 
check_ipv6_after_download(Config, InterfaceId)->

    HwId = list_to_atom(proplists:get_value(hw, Config)),
    ct:pal("Hwid: ~p",[HwId]),

    [{ssh,_Inner_IpAddress}, {ssh_outer, Out_IpAddress}, {netmask_outer, _Netmask_Outer}, 
     {vlan, VlanId}, {gateway_outer, _Gateway_Outer}] = ct:get_config({HwId, ssh_TN_A_ipv6_ipsec}),

    VlanId_Str = integer_to_list(VlanId),

    ct:pal("VlanId: ~p~n"
	   "Out_IpAddress: ~p~n",
	   [VlanId_Str, Out_IpAddress]),

    ok  = ct_telnet:send(console,"ifconfig"),
    {ok, _Data} = ct_telnet:get_data(console),
    timer:sleep(5000),

    ok  = ct_telnet:send(console,"ifconfig"),
    case ct_telnet:expect(console, 
			  InterfaceId,[{timeout,60000}, no_prompt_check]) of

	{ok, _} ->
    	    {ok, _} = ct_telnet:expect(console,
				       InterfaceId++"."++VlanId_Str, 
    				       [{timeout,60000},no_prompt_check]),

	    {ok, _} = ct_telnet:expect(console,
				       "inet addr:"++Out_IpAddress, 
				       [{timeout,60000},no_prompt_check]);

	_Other ->
	    ct:fail(InterfaceId++", interface not added")
    end,

    ok.


integrate(Config) ->

    ct:log("# Start Integrate: LMT integration."),
    aic_httpc:integrate_lmt_integration(Config, console, nc1),
    ct:log("# Integrate done: LMT integration."),
    ct:log("sleep 60sec to make sure node is up and ready for next tests!"),
    timer:sleep(60000).

generate_sif_ipv4_untrusted(Config, SiteInstallationFile, TN_port)->

    ct:log("Generate SIF and write to priv dir"),  
    HwId = list_to_atom(proplists:get_value(hw, Config)),
    ct:pal("Hwid: ~p",[HwId]),

    ok = nl_lib:gen_and_write_sif_ipv4_untrusted(Config,
					       SiteInstallationFile, 
					       "RbsSummaryFile.xml", 
					       HwId, TN_port),  

    %% Check SIF exist in priv dir
    Priv_dir = ?config(priv_dir, Config),
    Ls = string_token(os:cmd("ls "++Priv_dir)),

    ct:pal("ls : ~p ", [Ls]),
    true = lists:member(SiteInstallationFile, Ls).


generate_sif_ipv6_untrusted(Config, SiteInstallationFile, TN_port)->

    ct:log("Generate SIF and write to priv dir"), 
    HwId = list_to_atom(proplists:get_value(hw, Config)),
    ct:pal("Hwid: ~p",[HwId]),

    ok = nl_lib:gen_and_write_sif_ipv6_untrusted(Config,
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

random_sleep(MaxNr) ->
    %%%%
    %% Get a random nr.
    %%%%
    RandomNr = rand:uniform(MaxNr),
    ct:pal("# Start Sleep for: ~p sec.", [RandomNr]),
    ct:sleep({seconds, RandomNr}),
    RandomNr.

check_after_power_on() ->
    case ct_telnet:expect(console, 
			  "Ericsson Version:",
			  [{timeout,30000}, no_prompt_check]) of
	{ok,_} ->
	    case ct_telnet:expect(console, 
				  "Network Loader Ready:",
				  [{timeout,120000}, no_prompt_check]) of
		{ok,_} -> 
		    ct_telnet:expect(console, 
				     "Build date:",
				     [{timeout,180000}, no_prompt_check]),
		    ok;
		_Other ->
		    test_server:break("Check if disc fault!!")
	    end;
	_Else -> %% Power did not work! clean for next TC.
	    ct:pal("### TC will fail ### due to power off on did not work!!!"),
	    ct:pal("### Perorm a extra power to clean up for next TC!"),
	    rct_power:cycle(power),
	    ct_telnet:expect(console, 
			     "Build date:",
			     [{timeout,120000}, no_prompt_check]),
	    ct:fail("Power did not work, Node has not restarted !!")
    end,
    timer:sleep(15000).


search_for_inet_addr([Eth0Data |RestIfConfigList])->
    case Eth0Data of
	"inet" ->
	    hd(RestIfConfigList);
	_Else ->
	    %%ct:log("Eth0Data:~p ~n",[Eth0Data]),
	    search_for_inet_addr(RestIfConfigList)
    end.

set_temporary_ip_route()->
   
    %%Temporary set ip route to get ai answere, remove when new labb (etxmlar)
    %%i,e. not possible to run on Sec board, no console contact on Sec board.

    ct:pal("Wait for networkloader prompt. To set ip route."),
    ok = nl_lib:wait_node_is_in_nl(console),  

    IfConfig = os:cmd("/sbin/ifconfig eth0"),
    IfConfigList = string:tokens(IfConfig," \n "),

    InetAddr = search_for_inet_addr(IfConfigList),
    ct:log("InetAddr:~p ~n",[InetAddr]),
    Addr = lists:flatten(string:tokens(InetAddr,"addr:")),
    ct:log("Addr:~p ~n",[Addr]),

    CmdCommand = "ip route add "++Addr++"/32 via 10.67.225.1",
    ct:log("CmdCommand:~p ~n",[CmdCommand]),
    ct_telnet:cmd(console, CmdCommand),
    ok = ct_telnet:send(console,"ip route show"), 
    {ok, _Data} = ct_telnet:get_data(console),

    %% Adding route into table 220
    CmdCommandTable220 = "ip route add "++Addr++"/32 via 10.67.225.1 table 220",
    ct:log("CmdCommandTable220:~p ~n",[CmdCommand]),
    ct_telnet:cmd(console, CmdCommandTable220),
    ok = ct_telnet:send(console,"ip route show table 220"), 
    {ok, _DataTable220} = ct_telnet:get_data(console),

    ok.
