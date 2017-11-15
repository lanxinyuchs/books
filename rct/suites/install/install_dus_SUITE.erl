%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	install_dus_SUITE.erl %
%%% @author erarube
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R6A/R7A/R8A/R9A/R10A/1
%%% @doc ==Installs du board and checks software version==
%%% @end

-module(install_dus_SUITE).
-id('Updated by CCase').
-vsn('/main/R1A/R2A/R3A/R4A/R6A/R7A/R8A/R9A/R10A/1').
-date('2017-05-24').
-author('erarube').
-include_lib("common_test/include/ct.hrl").
%%% ----------------------------------------------------------
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
%%% R1A/1      2012-03-09 etxkols     Created
%%% R1A/2      2012-06-07 etxkols     Removed ifconfig patch
%%% R1A/3      2012-06-11 etxkols     Removed logging and netconf hooks
%%% R1A/3      2012-06-20 etxkols     Changed kernel to Hw in tftp
%%% R1A/4      2012-06-27 etxkols     Added rct_htmllink hook
%%% R1A/5      2012-07-05 etxkols     New tftpboot command
%%% R1A/6      2012-07-05 etxkols     U-Boot dhcp no longer needed
%%% R1A/7      2012-08-10 etxkols     Increased waiting for # prompt to 900 sec
%%% R1A/8      2012-08-13 etxkols     Matching for ctrl chars after / # prompt
%%% R1A/9      2012-09-07 etxkols     Added testcase check_sw_version
%%% R1A/10     2012-09-07 etxkols     Added ct:log
%%% R1A/11     2012-10-03 etxkols     Added install duw2
%%% R1A/13     2012-11-27 etxkols     Changed install command
%%% R2A/3      2012-12-03 etxjovp     Changed JenkinsNode in testcase check_sw_version
%%% R2A/4      2012-12-05 etxkols     Removed unused var Cwd
%%% R2A/4      2012-12-12 etxkols     Changed riohdid to 2560
%%% R2A/5      2013-01-31 etxkols     cxs101549_1.xml changed to cxs101549_1-up.xml
%%% R2A/6      2013-03-18 etxkols     Laborating with timer because of long LTE install
%%% R2A/7      2013-03-18 etxkols     Finetuning timers even more
%%% R2A/8      2013-04-08 etxkols     Set timetrap to 60 min beause of large UPs from node CI
%%% R2A/9      2013-04-22 etxkols     Added search in consolelog.
%%% R2A/10     2013-04-22 etxkols     Unused var
%%% R2A/11     2013-05-02 etxkols     Another try to parse installlog as Node CI
%%% R2A/12     2013-05-27 etxkols     node_ci_dc_dus will be replaced by node_ci_dc_rbs
%%% R2A/13     2013-06-04 etxjovp     add node_ci_hc_rbs
%%% R2A/15     2013-08-13 etxkols     Changed raw to no_prompt_check in ct_telnet:expect
%%% R2A/16     2013-09-26 etxkols     Supporting arm
%%% R2A/17     2013-09-27 etxkols     Supporting arm
%%% R2A/18     2013-10-01 etxkols     Supporting arm
%%% R2A/19     2013-10-16 etxkols     Removed 'reboot -f' for ARM
%%% R2A/20     2013-11-13 etxkols     Temporary fix for new uboot
%%% R2A/21     2013-11-14 etxkols     Second temporary fix for new uboot
%%% R2A/23     2013-11-19 etxkols     Changed install cmd for ARM
%%% R2A/24     2013-12-11 etxkols     Suporting ARM uboot P1D
%%% R2A/25     2013-12-17 etxkols     More robust uboot break dus/w
%%% R2A/26     2014-01-15 etxkols     Suporting dus52
%%% R2A/27     2014-01-15 etxkols     Fixed dus52 check_sw_version
%%% R2A/28     2014-01-28 etxkols     Added 1 sec sleep for PPC
%%% R2A/29     2014-01-29 etxkols     Changed to 200 msec sleep for PPC
%%% R2A/30     2014-02-18 etxkols     Removed sleep when breaking uboot
%%% R2A/31     2014-02-18 etxkols     Added retries when breaking uboot
%%% R2A/32     2014-02-20 etxkols     Added support for autointegration semi
%%% R2A/33     2014-02-25 etxkols     UTC time in curl
%%% R2A/34     2014-02-25 etxkols     Fault in break uboot
%%% R2A/35     2014-03-25 etxkols     Fix for node CI tcu UP
%%% R2A/36     2014-04-03 etxkols     bootfs-arm.cpio.gz changed to bootfs.cpio.gz for ARM
%%% R2A/37     2014-04-28 etxkols     New ms_since_1970/0 algoritm
%%% R2A/38     2014-05-14 etxkols     Added dus3201
%%% R2A/39     2014-05-14 etxkols     More fixes for dus3201
%%% R2A/40     2014-06-02 etxkols     Fixes for EE TCU03
%%% R2A/41     2014-06-02 etxkols     More fixes for EE TCU03
%%% R2A/42     2014-06-04 etxkols     Using ssh instead of telnet
%%% R2A/43     2014-07-07 etxkols     wr6 restruct format on target not set, fix check_sw_version later
%%% R2A/44     2014-07-08 etxkols     Added cth_conn_log hook
%%% R2A/46     2014-08-21 etxkols     Fix for new uboot for secqure boot
%%% R2A/47     2014-09-08 etxkols     Allow uboot P revisions for tcu03
%%% R3A/1      2014-10-21 etxkols     Workaround for corrupted login: prompt on dus52
%%% R3A/2      2014-11-06 etxkols     "aimode set to: semi" Removed from NL printouts + simplifications
%%% R3A/3      2014-11-06 etxkols     Fix yellow
%%% R3A/5      2014-11-12 eransbn     Added vc card support
%%% R3A/6      2014-11-24 eransbn     Update of NL for VC card
%%% R3A/7      2014-12-16 eransbn     More Update for VC card
%%% R3A/8      2015-01-08 etxkols     Verify that pghdb is not in bootargs (wanted by EE)
%%% R3A/9      2015-01-09 etxkols     Take case of secure dusx2 boards
%%% R3A/10     2015-01-12 etxderb     Inc uboot cmd timout 90-120 s. Manual try took 99.
%%% R3A/11     2015-01-21 eransbn     Fetching fake esi on vc board
%%% R3A/12     2015-01-23 eransbn     Fetching fake esi on vc board update
%%% R3A/14     2015-02-27 etxkols     Prepare for 2 labs
%%% R3A/15     2015-03-16 eransbn     added more fault handling for vc card
%%% R3A/16     2015-03-16 eransbn     Change poll timer to 10 minutes on vc card
%%% R3A/17     2015-03-18 eransbn     Improve printing
%%% R3A/18     2015-03-19 etxkols     tcu04
%%% R3A/19     2015-03-23 etxkols     Changing search for "login:" to "CvP successful"
%%% R3A/20     2015-03-25 eransbn     Added check for extra restart for vc cards
%%% R3A/21     2015-04-20 eransbn     Change http to https for vc cards
%%% R3A/22     2015-04-21 etxkols     Change linux-arm.img to linux.img
%%% R3A/23     2015-05-04 etxkols     login: back in business
%%% R3A/24     2015-05-20 eransbn     Call lib aic_curl.erl for curl call
%%% R4A/1      2015-06-30 etxarnu     site_config_complete now handles unset rbsConfigLevel
%%% R4A/2      2015-08-25 eransbn     Remove NL upgrade for VC cards (supported from NL version R4B18)
%%% R4A/5      2015-09-21 eransbn     New tc install_generic and NL upgrade tcu03 sec
%%% R4A/6      2015-09-21 etxkols     Increased sftp timeout from 45 to 60 sec  
%%% R4A/7      2015-09-24 eransbn     Debug new reply to AutoProvisionin
%%% R4A/8      2015-09-29 etxkols     Cluster installation
%%% R4A/9      2015-09-29 eransbn     Change now() to os:timestamp() and increased timer
%%%                                   for SITE_CONFIG_COMPLETE to 15min
%%% R4A/10     2015-10-02 eransbn     Increase timer on vc cards "Reset Status = SW Ordered"
%%% R4A/11     2015-10-05 etxkols     now/0 depricated, replaced with os:timestamp/0
%%% R4A/12     2015-10-19 eransbn     Changed timer in tc install_generic 
%%% R4A/13     2015-10-20 eransbn     Fetch ESI if fail tc install_generic
%%% R4A/14     2015-11-11 eransbn     If in uboot, bring the node to NL state only for tc install_generic
%%% R4A/15     2015-11-11 etxkols     Harder uboot prompt check "=> $" and flush console before power cycle
%%% R4A/16     2016-03-23 etxkols     5g
%%% R4A/17     2016-05-03 etxkols     idu board type
%%% R4A/18     2016-05-20 etxkols     increased time to poll for SITE_CONFIG_COMPLETE to 20 min
%%% R4A/19     2016-06-08 etxkols     dus5301, dus3301 board type
%%% R6A/1      2016-09-02 etxkols     dus5301
%%% R7A/1      2016-11-03 etxkols     dus6303
%%% R7A/2      2016-11-04 etxkols     dus6303 and dus6620 fixes
%%% R7A/3      2016-11-22 etxivri     dus6502
%%% R7A/4      2016-11-24 etxkols     c608
%%% R8A/1      2016-11-28 etxkols     c608
%%% R8A/2      2016-12-07 etxkols     dus3301
%%% R10A/1     2017-05-24 erarube     Support for UBOOT DUSx3 R1A
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
         install/0,
         install/1,
	 install_generic/1,
	 check_sw_version/1]).


%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_power, rct_consserv, rct_rs232
%% @end
%%--------------------------------------------------------------------
suite() ->
    N = length(ct:get_config(test_nodes)),
    Hooks =[{rct_power,   [list_to_atom("power" ++ integer_to_list(X))||X<-lists:seq(1,N)]},
	    {rct_consserv,[list_to_atom("cs" ++ integer_to_list(X))||X<-lists:seq(1,N)]},
	    {rct_rs232,   [list_to_atom("console" ++ integer_to_list(X))||X<-lists:seq(1,N)]}],
    [{ct_hooks, [{rct_htmllink,[]},
    		 {rct_netconf, {nc1, man_auth}},
    		 {rct_ssh,{ssh,[manual_connect]}}] ++ 
	        Hooks ++
    	        [{cth_conn_log,[]}]}].

%% @hidden
init_per_suite(Config) ->
    Config.
%% @hidden
end_per_suite(_Config) ->
    ok.
%% @hidden
init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(TestCase, Config) when TestCase =:= install_generic ->
    	    case proplists:get_value(tc_status, Config) of
    		ok -> 
		    ok;
    		R ->
		    ct:log("test case ~p",[R]),
		    %%if in uboot, bring the node to NL state
		    in_uboot_state(Config),
		    poll_esi(Config)
	    end,

    Config; 
end_per_testcase(_TestCase, Config) ->
    case check_if_vc_board() of
    	"yes" ->
    	    case proplists:get_value(tc_status, Config) of
    		ok -> 
		    ok;
    		R ->
		    ct:log("test case ~p",[R]),
		    poll_esi(Config)
	    end;
	_Other -> ok
    end,
    Config. 

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [install].

install() ->
    [{timetrap,{minutes,60}}].

%%--------------------------------------------------------------------
%% @doc
%% Install du card.<br/><br/>
%%    - Break in U-boot<br/>
%%    - Load linux and start netloader<br/>
%%    - Wait for \ # prompt and reboot -f<br/>
%%    - Wait for login prompt and login
%% @end
%%--------------------------------------------------------------------
install(_Config) ->
    case os:getenv("SIM_OR_TARGET") of
	"cloudish" ->
	    ct:pal("No installation needed for Cloud");
	_ ->
	    Hw = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
	    %%Check if it is a secure card
	    case check_if_vc_board() of
		"yes" -> 	    
		    install_sec_card(Hwa);
		_ ->
		    case ct:get_config({Hwa,board_type}) of
			BOARDTYPE when BOARDTYPE == "tcu03";
				       BOARDTYPE == "tcu0401";
				       BOARDTYPE == "idu5205";
				       BOARDTYPE == "idu5209";
				       BOARDTYPE == "dus5301"; %dus53
				       BOARDTYPE == "dus3301"; %dus33 6620
				       BOARDTYPE == "dus5201";
				       BOARDTYPE == "dus3201";
				       BOARDTYPE == "c608";
				       BOARDTYPE == "dus6502"; %micro
				       BOARDTYPE == "dus6303" -> %marco
			    install_networkloader(length(ct:get_config(test_nodes))),
			    curl(list_to_atom(Hw),"RbsSummaryFile.xml"),
			    case ct_telnet:expect(console1, [{login, "login:"},{cvp,"CvP successful"}],
						  [{timeout,60000},no_prompt_check]) of
				{ok,{login,_}} ->
				    timer:sleep(15000);
				{ok,{cvp,_}} ->
				    timer:sleep(5000);
				{error,timeout} ->
				    ct:fail("Timeout")
			    end,
			    
			    ok = rct_rs232:login(console1);
			BOARDTYPE  ->
			ct:fail("Unknown boardtype ~p",[BOARDTYPE])	
		    end	
	    end
    end.

install_networkloader(0) ->
    ok;
install_networkloader(N) ->
    Console = list_to_atom("console"++integer_to_list(N)),
    Power = list_to_atom("power"++integer_to_list(N)),
    Hw = atom_to_list(Hwa = ct:get_config({test_nodes,N})),
    BOARDTYPE = ct:get_config({Hwa,board_type}),
    break_uboot_arm(Console,Power,BOARDTYPE,3,3),
    ct:log("Verify that pghdb is not set in bootargs for debugging purposes since it will fail installation (wanted by EE)"),
    ct_telnet:send(Console,"printenv bootargs"),
    case ct_telnet:expect(Console, ".*pghdb.*", [{timeout,5000},no_prompt_check]) of
	{ok,Data} ->			    
	    ct:pal(lightred,"pghdb in ~p\nPLEASE REMOVE IT MANUALLY",[Data]),
	    ct:fail("pghdb in bootargs");
	_ ->
	    ok
    end,
    Cmd = case BOARDTYPE of
	      "dus5301" -> %dus53
		  "dhcp;setenv bootargs console=ttyAMA0 earlyprintk debug initrd=0x8000000,32M rdinit=/addons/bin/labloader.sh;rcs 1;mw.b 8000000 0 2000000; tftp 4000000 " ++ Hw ++ "/linux.img;tftp 8000000 " ++ Hw ++ "/bootfs.cpio.gz;imxtract 4000000 cpm2_30_fdt 5200000;merge_dtb 5200000 5000000;bootm 4000000 - 5000000";
	      "dus3301" -> %dus33
		  "dhcp;setenv bootargs console=ttyAMA0 earlyprintk debug initrd=0x8000000,32M rdinit=/addons/bin/labloader.sh;rcs 1;mw.b 8000000 0 2000000; tftp 4000000 " ++ Hw ++ "/linux.img;tftp 8000000 " ++ Hw ++ "/bootfs.cpio.gz;imxtract 4000000 cpm2_20_fdt 5200000;merge_dtb 5200000 5000000;bootm 4000000 - 5000000";
	      BOARDTYPE when BOARDTYPE == "dus6502";
			     BOARDTYPE == "dus6303" ->
		  "dhcp;setenv bootargs console=ttyAMA0 earlyprintk debug initrd=0x8000000,32M rdinit=/addons/bin/labloader.sh;rcs 1;mw.b 8000000 0 2000000; tftp 4000000 " ++ Hw ++ "/linux.img;tftp 8000000 " ++ Hw ++ "/bootfs.cpio.gz;imxtract 4000000 cpm2_02_fdt 5200000;merge_dtb 5200000 5000000;bootm 4000000 - 5000000";
	      _ ->
		  "dhcp;setenv bootargs console=ttyAMA0,115200n8 earlyprintk debug initrd=0x8000000,30M ramdisk_size=30000 rdinit=/addons/bin/labloader.sh;rcs 1;tftp 0x4000000 " ++ Hw ++ "/linux.img;tftp 0x8000000 " ++ Hw ++ "/bootfs.cpio.gz;bootm 0x4000000 - 0x5000000"
	  end,
    ok = ct_telnet:send(Console, Cmd),
    {ok, _} = ct_telnet:expect(Console, "Welcome to Autointegration", [{timeout,120000},no_prompt_check]),
    install_networkloader(N-1).
    

install_generic(_Config)->
   Hwa = ct:get_config({test_nodes,1}),
  
    case aic_curl:board_restore() of
	ok ->    
	    check_expect_from_console("boot_count = 7",300000),
	    %%check_expect_from_console("Secure Boot Enabled",300000),
	    %% check_expect_from_console("Reset Status = SW Ordered","Bad device usb" , 20000, ""),
	    check_expect_from_console("Network Loader Ready:","Reset Status = SW Ordered",
				      40000, "Detect extra restart");
	{_, "HTTP/1.1 403 Forbidden"} -> ct:log("Probably already in NL state"), ok; 
	Response1 ->
	    ct:fail(Response1)
    end,   

    %%not really ready after printout "Network Loader Ready:"
    timer:sleep(10000),

    %%Installing up, upgrade NL for tcu03
    case ct:get_config({Hwa,board_type}) of
	BOARDTYPE when BOARDTYPE == "tcu03"->
	    case aic_curl:nl_upgrade() of
		ok ->check_expect_from_console("Reset Status = SW Ordered","Bad device usb" , 300000, ""),%%old fault check.
		     check_expect_from_console("Network Loader Ready:","Reset Status = SW Ordered",
						60000, "Detect extra restart");
		Response2 -> 	
		    ct:fail(Response2)
	    end;
	_ ->ok
    end,
    ct:log("sleep 10 s"),
    
    case aic_curl:install_sw() of
	ok -> ok;
	Response3 -> 	
	    ct:fail(Response3)
    end, 

    %%Check install
    %% check_expect_from_console("Secure Boot Enabled",300000),
    %%  check_expect_from_console("boot_count = 1",300000),

    case site_config_complete([]) of
	ok -> ok;
	{error,String}  -> 
	    ct:fail(String)

    end.

curl(Hw, RbsSummaryFile) ->
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
%    [{host, Host},{username, Username},{password, Password}] = ct:get_config({Hw,sftp_server}),
    [{host, Host},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    SftpAiInstallDir = ct:get_config({Hw,sftp_ai_install_dir}),
    SftpAiInstallDir2 = re:replace(SftpAiInstallDir,"/","%2F",[{return,list},global]),
    MyTime = integer_to_list(ms_since_1970()),
    Cmd = "curl -v -k --noproxy " ++ IP ++ " --data \"host="      ++ Host ++
	"&username=" ++ Username ++
	"&password=" ++ Password ++
	"&filename=" ++ SftpAiInstallDir2 ++ "%2F"++ RbsSummaryFile ++ "&DoIntegrate=Integrate" ++
	"&utcMs="    ++ MyTime ++ "\" "
	"https://" ++ IP ++ "/cgi-bin/nl_gui:post",
    timer:sleep(1000),
    ct:log("~s",[Cmd]),
    R = os:cmd(Cmd),
    ct:log(re:replace(R,"<","\\&lt;",[{return,list},global])).

ms_since_1970() ->
    list_to_integer(string:strip(os:cmd("date -u +%s"),right,$\n)) * 1000.
						%   Ahora = calendar:datetime_to_gregorian_seconds({date(), time()}),
						%   Jimilives = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
						%   1000*(Ahora-Jimilives).

break_uboot_arm(_Console,_Power,_,0,_) ->
    ct:log(lightred, "Could not break board in uboot"),
    ct:fail("Could not break uboot");
break_uboot_arm(Console,Power, BOARDTYPE,N,M) ->
    ct:log("board_type: ~p",[BOARDTYPE]),
    case N < M of
	true ->
	    ct:log(yellow,"Could not break board in uboot, retry ~p more times",[N]);
	_ ->
	    ok
    end,
    ct_telnet:get_data(Console), %flush console not to hit old printouts
    case rct_power:cycle(Power) of
	ok ->
	    Uboot = case BOARDTYPE of
			"tcu03" ->
			    case ct_telnet:expect(Console, ["Ericsson Version: 2/CXC1736593/03.*\\)",
							    "Ericsson Version: 4/CXC1736593/03.*\\)",
							    "Ericsson Version: 7/CXC1736593/03.*\\)"], [{timeout,30000}]) of
				{ok, Match} ->
				    case re:run(Match,"Ericsson Version: [0-9]/CXC1736593/03_(.*) \\(",[{capture,[1],list}]) of
					{match,[Rev]} ->
					    ct:pal("Uboot: ~s",[Rev]),
					    other
				    end;
				_ ->
				    error
			    end;
			"tcu0401" ->
			    case ct_telnet:expect(Console, ["Ericsson Version: 2/CXC1736593/04.*\\)",
							    "Ericsson Version: 4/CXC1736593/04.*\\)",
							    "Ericsson Version: 7/CXC1736593/04.*\\)"], [{timeout,30000}]) of
				{ok, Match} ->
				    case re:run(Match,"Ericsson Version: [0-9]/CXC1736593/04_(.*) \\(",[{capture,[1],list}]) of
					{match,[Rev]} ->
					    ct:pal("Uboot: ~s",[Rev]),
					    other
				    end;
				_ ->
				    error
			    end;
			"dus5301" ->
			    case ct_telnet:expect(Console, ["Ericsson Version: 2/CXC1739155/30.*\\)",
							    "Ericsson Version: 4/CXC1739155/30.*\\)",
							    "Ericsson Version: 7/CXC1739155/30.*\\)",
							    "Ericsson Version: 7/CXC1740623/30.*\\)"], [{timeout,30000}]) of
				{ok, Match} ->
				    case re:run(Match,"Ericsson Version: [0-9]/CXC17.*/30_(.*) \\(",[{capture,[1],list}]) of
					{match,[Rev]} ->
					    ct:pal("Uboot: ~s",[Rev]),
					    other
				    end;
				_ ->
				    error
			    end;
			"dus6303" ->
			    case ct_telnet:expect(Console, ["Ericsson Version: 2/CXC1739155/02.*\\)",
							    "Ericsson Version: 4/CXC1739155/02.*\\)",
							    "Ericsson Version: 7/CXC1739155/02.*\\)",
							    "Ericsson Version: 7/CXC1740623/02.*\\)"], [{timeout,30000}]) of
				{ok, Match} ->
				    case re:run(Match,"Ericsson Version: [0-9]/CXC17.*/02_(.*) \\(",[{capture,[1],list}]) of
					{match,[Rev]} ->
					    ct:pal("Uboot: ~s",[Rev]),
					    other
				    end;
				_ ->
				    error
			    end;
			"dus6502" ->
			    case ct_telnet:expect(Console, ["Ericsson Version: 2/CXC1739155/02.*\\)",
							    "Ericsson Version: 4/CXC1739155/02.*\\)",
							    "Ericsson Version: 7/CXC1739155/02.*\\)",
							    "Ericsson Version: 7/CXC1740623/02.*\\)"], [{timeout,30000}]) of
				{ok, Match} ->
				    case re:run(Match,"Ericsson Version: [0-9]/CXC17.*/02_(.*) \\(",[{capture,[1],list}]) of
					{match,[Rev]} ->
					    ct:pal("Uboot: ~s",[Rev]),
					    other
				    end;
				_ ->
				    error
			    end;
			"c608" ->
			    case ct_telnet:expect(Console, ["Ericsson Version: 2/CXC1736593/08.*\\)",
							    "Ericsson Version: 4/CXC1736593/08.*\\)",
							    "Ericsson Version: 7/CXC1736593/08.*\\)"], [{timeout,30000}]) of
				{ok, Match} ->
				    case re:run(Match,"Ericsson Version: [0-9]/CXC1736593/08_(.*) \\(",[{capture,[1],list}]) of
					{match,[Rev]} ->
					    ct:pal("Uboot: ~s",[Rev]),
					    other
				    end;
				_ ->
				    error
			    end;
			BOARDTYPE when BOARDTYPE == "dus5201";
			               BOARDTYPE == "idu5209" ->
			    case ct_telnet:expect(Console, ["Ericsson Version: 2/CXC1736593/52.*\\)",
							    "Ericsson Version: 4/CXC1736593/52.*\\)",
							    "Ericsson Version: 7/CXC1736593/52.*\\)"],[{timeout,30000}]) of
				{ok, Match} ->
				    case re:run(Match,"Ericsson Version: [0-9]/CXC1736593/52_(.*) \\(",[{capture,[1],list}]) of
					{match,[Rev]} ->
					    ct:pal("Uboot: ~s",[Rev]),
					    other
				    end;
				_ ->
				    error
			    end;
			BOARDTYPE when BOARDTYPE == "dus3301" ->
			    case ct_telnet:expect(Console, ["Ericsson Version: 2/CXC1739155/20.*\\)",
							    "Ericsson Version: 4/CXC1739155/20.*\\)",
							    "Ericsson Version: 7/CXC1739155/20.*\\)",
							    "Ericsson Version: 7/CXC1740623/20.*\\)"],[{timeout,30000}]) of
				{ok, Match} ->
				    case re:run(Match,"Ericsson Version: [0-9]/CXC17.*/20_(.*) \\(",[{capture,[1],list}]) of
					{match,[Rev]} ->
					    ct:pal("Uboot: ~s",[Rev]),
					    other
				    end;
				_ ->
				    error
			    end;
			BOARDTYPE when BOARDTYPE == "dus3201";
                                       BOARDTYPE == "idu5205" ->
			    case ct_telnet:expect(Console, ["Ericsson Version: 2/CXC1736593/32.*\\)", 
							    "Ericsson Version: 4/CXC1736593/32.*\\)",
							    "Ericsson Version: 7/CXC1736593/32.*\\)"],[{timeout,30000}]) of
				{ok, Match} ->
				    case re:run(Match,"Ericsson Version: [0-9]/CXC1736593/32_(.*) \\(",[{capture,[1],list}]) of
					{match,[Rev]} ->
					    ct:pal("Uboot: ~s",[Rev]),
					    other
				    end;
				_ ->
				    error
			    end
		    end,
	    case Uboot of
		error ->
		    break_uboot_arm(Console,Power,BOARDTYPE,N-1,M);
		Uboot ->
		    case ct_telnet:expect(Console, "Hit any key to stop autoboot:", [{timeout,30000},no_prompt_check]) of
			{ok, _} ->
						%		    timer:sleep(1000),
			    ct_telnet:send(Console, "\n"),
			    case ct_telnet:expect(Console, "=> $", [{timeout,10000},no_prompt_check]) of
				{ok, _} ->
				    Uboot;
				_ ->
				    break_uboot_arm(Console,Power,BOARDTYPE,N-1,M)
			    end;
			_ ->
			    break_uboot_arm(Console,Power,BOARDTYPE,N-1,M)
		    end
	    end;
	_ ->
	    break_uboot_arm(Console,Power,BOARDTYPE,N-1,M)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Check that software version installed on target is correct.<br/><br/>
%% Compares:<br/>
%%    - id and revision for "product name=RCP-T3" in /home/sirpa/software/cxs101549_1-up.xml on target<br/>
%%    - TgtLabel: label in prep4TestInfo.txt.<br/>
%%      prep4TestInfo.txt is copied in Jenkins "Execute shell" to /proj/rcs-tmp/stps/${NODE_NAME}.<br/>
%%      file:get_cwd() is used in Common test to retrieve the path to prep4TestInfo.txt
%% @end
%%--------------------------------------------------------------------
check_sw_version(_) ->
    case os:getenv("SIM_OR_TARGET") of
	"cloudish" ->
	    ct:pal("No installation needed for Cloud");
	_ ->
	    case check_if_vc_board()  of
		"yes"->ok;
		_->
		    {ok,_} = ct_ssh:connect(ssh),
		    {ok,TargSW} = ct_ssh:exec(ssh,"ls -d /software/*",5000),
		    ok = ct_ssh:disconnect(ssh),
		    ct:log("Loaded software on target:~n~s",[TargSW]),
		    JenkinsNode=lists:nth(1,string:tokens(erlang:atom_to_list(node()),"@")),
		    Prep4TestInfo = os:cmd(PrepPath = "cat /proj/rcs-tmp/stps/" ++ JenkinsNode ++ "/prep4TestInfo.txt"),
		    case find_rcs_type([{ppc,"RCS_"},{arm_wr5,"RCS-ARM_"}],TargSW) of
			unknown_rcs_version ->
			    ok;
			Prep4TestInfoLine ->
			    ct:log("Search for ~s in prep4TestInfo.txt",[Prep4TestInfoLine]),
			    ct:log("~s:~s",[PrepPath,Prep4TestInfo]),
			    case string:str(Prep4TestInfo,Prep4TestInfoLine) of
				0 -> ct:log(lightred, "Wrong software on node, ~s does not match~n~s",[Prep4TestInfoLine,Prep4TestInfo]),
				     ct:fail("Wrong software on node");
				_ -> ok
			    end
		    end
	    end
    end.

find_rcs_type([],_) ->
    ct:log(lightred, "Could not determine RCS type (RCS, RCS-ARM), can be restructured EE since format for that is not set"),
    unknown_rcs_version;
						%    ct:fail("Could not determine RCS type");
find_rcs_type([{Type,H}|T],TargSW) ->
    case re:run(TargSW,"/software/" ++ H ++ "(CXP.*_[0-9]+)_(.*)",[{capture,[1,2],list}]) of
	{match,[ProdNo,Rev]} ->
	    Label = ProdNo ++ "-" ++ Rev,
	    case Type of
		ppc ->
		    ct:pal("PPC detected on node with Label ~s",[Label]),
		    "TgtCxpLabel:" ++ Label;
		arm_wr5 ->
		    ct:pal("ARM wr5 detected on node with Label ~s",[Label]),
		    "ArmCxpLabel:" ++ Label
	    end;
	nomatch ->
	    find_rcs_type(T,TargSW)
    end.
%%##############################################
%% Functions for vc cards
%%#############################################
%%Install secure card
install_sec_card(Hwa)->



   %% ct:fail("fake fail"),
    %%Bring the node to network loader state
    case aic_curl:board_restore() of
	ok ->    
	    check_expect_from_console("boot_count = 7",300000),
	    %%check_expect_from_console("Secure Boot Enabled",300000),
	   %% check_expect_from_console("Reset Status = SW Ordered","Bad device usb" , 20000, ""),
	    check_expect_from_console("Network Loader Ready:","Reset Status = SW Ordered",
					60000, "Detect extra restart");
	{_, "HTTP/1.1 403 Forbidden"} -> ct:log("Probably already in NL state"), ok; 
	Response1 ->
	    ct:fail(Response1)
    end,   

    %%not really ready after printout "Network Loader Ready:"
    timer:sleep(10000),

    %%Installing up, upgrade NL for tcu03
    case ct:get_config({Hwa,board_type}) of
	BOARDTYPE when BOARDTYPE == "tcu03"->
	    case aic_curl:nl_upgrade() of
		ok -> check_expect_from_console("Secure Boot Enabled",300000),
		      check_expect_from_console("Reset Status = SW Ordered","Bad device usb" , 60000, ""),%%old fault check.
		      check_expect_from_console("Network Loader Ready:","Reset Status = SW Ordered",
						60000, "Detect extra restart");
		Response2 -> 	
		    ct:fail(Response2)
	    end;
	_ ->ok
    end,
    ct:log("sleep 10 s"),
    
    case aic_curl:install_sw() of
	ok -> ok;
	Response3 -> 	
	    ct:fail(Response3)
    end, 

    %%Check install
   %% check_expect_from_console("Secure Boot Enabled",300000),
  %%  check_expect_from_console("boot_count = 1",300000),

    case site_config_complete([]) of
	ok -> ok;
	{error,String}  -> 
	    ct:fail(String)

    end.



%%Check String from console log, if not try to fetch fake esi in NL state
check_expect_from_console(String, TimeOut)->
    case  ct_telnet:expect(console1, String, [{timeout,TimeOut},no_prompt_check]) of
	{ok, _} -> ok;
	_ ->  ct:fail("Didn't receive ~s",[String])
    end.
%%Check String from console log, if not try to fetch fake esi in NL state
check_expect_from_console(ExpectString, HaltString, TimeOut, ErrorReason)->
    case  ct_telnet:expect(console1, [{expectstring, ExpectString}], 
			   [sequence,{halt,[{haltstring,HaltString}]},
			    {timeout,TimeOut},no_prompt_check]) of
	{ok, _} -> ok;
	{error,timeout} -> 
	    ct:fail("Didn't receive ~s",[ExpectString]);
	{error,{haltstring,[HaltString]}} ->  ct:fail("Received ~p before ExpectString ~p: ~p  ",
						      [HaltString, ExpectString, ErrorReason]);
	Unknown ->  ct:fail("Unknown reason: ~p",[Unknown])
    end.  

%% ct:log(re:replace(R,"<","\\&lt;",[{return,list},global])).

secs_since_1970() ->
    {MSec,Sec,_} = os:timestamp(), 
    (MSec * 1000000) + Sec.

site_config_complete(_) ->
    site_config_complete(nc1, 1200).
site_config_complete(NC, Timeout) ->
    ct:log("Wait for netconf rbsConfigLevel: SITE_CONFIG_COMPLETE"),
    Time = secs_since_1970(),
    site_config_complete(NC, Time, Time + Timeout).

site_config_complete(NC, Time, Timeout) when Time < Timeout ->
    case ct_netconfc:open(NC,[{timeout, 5000},{user, "SysAdminTest"}, {password, "SysAdminTest"}]) of %%{user, "SystemAdministrator"}, {password, "SystemAdministrator"}]) of
	{ok,_} ->

	    case ct_netconfc:get(nc1,{'ManagedElement',
				      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				      [{managedElementId,[],["1"]},
				       {'NodeSupport',[],
					[{nodeSupportId,[],["1"]},
					 {'AutoProvisioning',[],
					  [{autoProvisioningId,[],["1"]}]}]}]}) of
		{error,no_such_client} ->% During startup, netconf sesssion may be closed, give it one more retry.
		    ct_netconfc:close_session(NC),
		    case get(netconf_disconnected_givit_1_more_try) of
			undefined ->
			    ct:log("netconf connection closed giving it one more retry"),
			    put(netconf_disconnected_givit_1_more_try,no),
			    timer:sleep(5000),
			    site_config_complete(NC,secs_since_1970(), Timeout);
			no ->
			    ct:log(lightred,"netconf connection closed  failed retry, giving up"),
			    {error, netconf_timeout}
		    end;
		{error,closed} ->% During startup, netconf sesssion may be closed, give it one more retry.
		    ct_netconfc:close_session(NC),
		    case get(netconf_disconnected_givit_1_more_try) of
			undefined ->
			    ct:log("netconf connection closed giving it one more retry"),
			    put(netconf_disconnected_givit_1_more_try,no),
			    timer:sleep(5000),
			    site_config_complete(NC,secs_since_1970(), Timeout);
			no ->
			    ct:log(lightred,"netconf connection closed  failed retry, giving up"),
			    {error, netconf_timeout}
		    end;
		{error,{closed}} ->% During startup, netconf sesssion may be closed, give it one more retry.
		    ct_netconfc:close_session(NC),
		    case get(netconf_disconnected_givit_1_more_try) of
			undefined ->
			    ct:log("netconf connection closed giving it one more retry"),
			    put(netconf_disconnected_givit_1_more_try,no),
			    timer:sleep(5000),
			    site_config_complete(NC,secs_since_1970(), Timeout);
			no ->
			    ct:log(lightred,"netconf connection closed, failed retry, giving up"),
			    {error, netconf_timeout}
		    end;
		{ok,[{'ManagedElement',
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],["1"]},
		       {'NodeSupport',
			[{xmlns,"urn:com:ericsson:ecim:RmeSupport"}],
			[{nodeSupportId,[],["1"]},
			 {'AutoProvisioning',
			  [{xmlns,"urn:com:ericsson:ecim:RmeAI"}],
			  [{autoProvisioningId,[],["1"]},
			   {rbsConfigLevel,[],[SITE_CONFIG_COMPLETE]}]}]}]}]} ->

	    	    case SITE_CONFIG_COMPLETE of
			"SITE_CONFIG_COMPLETE" ->
			    ct:log("rbsConfigLevel: SITE_CONFIG_COMPLETE"),
			    ok = ct_netconfc:close_session(NC);		
			_ ->
			    ct:log(yellow,"Wrong rbsConfigLevel: ~s, Retrying in 5 seconds",[SITE_CONFIG_COMPLETE]),
			    timer:sleep(5000),
			    ok = ct_netconfc:close_session(NC),
			    site_config_complete(NC,secs_since_1970(), Timeout)
		    end;

		{ok,[{'ManagedElement',
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],["1"]},
		       {'NodeSupport',
			[{xmlns,"urn:com:ericsson:ecim:RmeSupport"}],
			[{nodeSupportId,[],["1"]},
			 {'AutoProvisioning',
			  [{xmlns,"urn:com:ericsson:ecim:RmeAI"}],
			  [{autoProvisioningId,[],["1"]}]}]}]}]} 

		->
		    ct:log(yellow," rbsConfigLevel not set, Retrying in 5 seconds",[]),
		    timer:sleep(5000),
		    ok = ct_netconfc:close_session(NC),
		    site_config_complete(NC,secs_since_1970(), Timeout)
	    end;
	Other ->
	    ct:log(yellow,"Could not connect with netconf, Retrying in 5 seconds. Reason: ~p",[Other]),
	    timer:sleep(5000),
	    site_config_complete(NC,secs_since_1970(), Timeout)
    end;
site_config_complete(_NC,_Time,Timeout) ->
    ct:pal("in fail"),
    ReturnString = "Could not connect with netconf within " ++ integer_to_list(Timeout) ++ " seconds",
    {error,ReturnString} .


check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.


poll_esi(Config)->
    poll_esi(Config, 120).
poll_esi(_Config, [])->
    ok;
poll_esi(Config, TimerToPolSec) ->
    case aic_curl:fetch_esi(Config) of
	ok -> ok;
	_RespEsi  -> 
	    case aic_curl:fetch_esi_nl(Config) of
		ok -> ok;
		_RespEsiNl  ->
		    case aic_curl:fetch_fake_esi(Config) of
			ok -> ok;
			_RespEsiFake -> 
			    timer:sleep(10000),
			    poll_esi(Config, TimerToPolSec -1 )
		    end
	    end
    end.
    
in_uboot_state(_Config) -> 
    %%Check if the node are in uboot state, if bring it to NL state
    ct_telnet:send(console1, "\n"),
    case ct_telnet:expect(console1, "=> ", [{timeout,20000},no_prompt_check]) of
	{ok, _} ->
	    ct:log("Node in u-boot, reset to NL state"),
	    %%send reset to nl command;
	    ok = ct_telnet:send(console1, "mw 0x900300ec 0x10; mw 0x20000000 "
				"0xAFFE0001; mw 0x20000004 6;mw 0x20000008 0xEBBA8888"),
	    ok = ct_telnet:send(console1,"boot");

	_ ->
	    ok
    end.     
