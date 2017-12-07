%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	xl_install_SUITE.erl %
%%% @author erarube
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R2A/R3A/R4A/R6A/R7A/R11A/2
-module(xl_install_SUITE).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R6A/R7A/R11A/2').
-date('2017-11-13').
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
%%% R2A/1      2012-10-26 etxkols     Created
%%% R2A/2      2012-10-29 etxkols     Added log fetching and check
%%% R2A/4      2013-08-13 etxkols     Changed raw to no_prompt_check in ct_telnet:expect
%%% R2A/5      2013-09-02 etxkols     Added watchdog check
%%% R2A/6      2013-09-13 etxkols     Changed timeouts
%%% R2A/7      2013-10-01 etxkols     Fix for ARM
%%% R2A/8      2013-10-17 etxkols     Removed 'reboot -f' for ARM
%%% R2A/9      2013-10-21 etxkols     ctrl chars on # prompt for dus/w
%%% R2A/10     2013-11-11 etxkols     Temporary fix for new uboot
%%% R2A/11     2013-11-12 etxkols     Second temporary fix for new uboot
%%% R2A/12     2013-11-19 etxkols     Changed install cmd for ARM
%%% R2A/14     2014-01-16 etxkols     Support for dus5201. 
%%% R2A/15     2014-02-17 etxkols     Writing vc.crt and vc.key to make install time consisent. 
%%% R2A/16     2014-02-17 etxkols     Removed debugs. 
%%% R2A/17     2014-02-20 etxkols     Added support for autointegration semi
%%% R2A/18     2014-02-24 etxkols     Trouble echo string in ct_telnet
%%% R2A/19     2014-04-03 etxkols     Changed bootfs-arm.cpio.gz to bootfs.cpio.gz for ARM
%%% R2A/20     2014-04-22 etxkols     Changed reboot indication from "Restarting system" to "RBS sys: restart ordered"
%%%                                   since it does not always appear
%%% R2A/20     2014-04-23 etxkols     Trying to fix lost uboot cmd characters
%%% R2A/23     2014-05-09 etxkols     Added ms_since_1970/0
%%% R2A/24     2014-05-14 etxkols     Increased wait for ARM to 180 sec
%%% R2A/25     2014-05-28 etxkols     Check Cobra and Trinity link status 
%%% R2A/26     2014-06-03 etxkols     Fixes for EE TCU03 
%%% R2A/27     2014-06-04 etxkols     More fixes for EE TCU03 
%%% R2A/28     2014-06-04 etxkols     More fixes for EE TCU03 
%%% R2A/29     2014-07-08 etxkols     Added cth_conn_log hook
%%% R2A/30     2014-08-11 etxkols     Increase dus/w41 wait after login prompt to 180 sec
%%% R2A/32     2014-08-21 etxkols     Changed AI printout
%%% R3A/1      2014-09-02 etxkols     Added netconf check
%%% R3A/2      2014-09-10 etxkols     Support for VC and secureboot boards
%%% R3A/3      2014-11-06 etxkols     "aimode set to: semi" Removed from NL printouts 
%%% R3A/4      2014-11-24 etxkols     Redesigned
%%% R3A/5      2014-11-26 etxkols     Changed AI semi finished match to "Reboot to configured"
%%% R3A/6      2015-02-25 etxkols     Removed PPC and fixed uboot match
%%% R3A/7      2015-02-27 etxkols     Preparation for 2 labs
%%% R3A/8      2015-03-11 etxkols     AI printouts changed
%%% R3A/9      2015-03-16 etxkols     Changing linux-arm.img to linux.img
%%% R3A/10     2015-03-23 etxkols     Changing search for "login:" to "CvP successful"
%%% R3A/11     2015-03-26 eransbn     Suport for vc card
%%% R3A/12     2015-05-06 etxkols     login: back in business
%%% R4A/1      2015-07-01 etxivri     Update to use https.
%%% R4A/2      2015-07-08 etxkols     site_config_complete now handles unset rbsConfigLevel
%%% R4A/3      2015-07-16 etxkols     Removed calls to rct_check_HW.erl on EEs request
%%% R4A/6      2015-09-28 eransbn     Debug new reply to AutoProvisioning and change timer from 
%%%                                   180 to 210 s when waiting for site_config_complete
%%% R4A/7      2015-09-30 etxmlar     now() depricated in OTP 18 changed to os:timestamp() 
%%% R4A/8      2016-01-07 etxnnor     Removed check for VC in end_per_testcase when fetching ESI
%%% R6A/1      2016-08-31 etxkols     dus53
%%% R6A/2      2016-00-02 etxkols     dus53
%%% R7A/1      2016-09-19 etxivri     Remove scp of vc files to node. Not needed anymore.
%%% R7A/1      2016-11-04 etxivri     Update to handle new boards dus6303, dus6502, dus3301
%%% R7A/3      2016-11-10 etxivri     Update to use restart instead of power off/on.
%%% R7A/4      2016-11-15 etxivri     Update to use cup --reboot
%%% R7A/5      2017-05-29 erarube     Support for U-BOOT DUSx3 R1A Boards
%%% R11A/1     2017-08-29 etxivri     Update to be used in git. Some printout is needed.
%%% R11A/2     2017-11-13 erarube     Support for dus3201tk and dus5201tk.
%%% ----------------------------------------------------------
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 install/1]).

%% -define(VC_CRT,"/proj/rcs/CERT_hands_off/vc.crt").
%% -define(VC_KEY,"/proj/rcs/CERT_hands_off/vc.key").

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_consserv, rct_rs232, rct_power, rct_logging, rct_core
%% @end
%%--------------------------------------------------------------------
suite() -> 
    case check_if_vc_board() of
	"yes" -> [
		  {ct_hooks, [{rct_htmllink,[]},
			      {rct_power,power},
			      {rct_consserv,cs1},
			      {rct_netconf, {nc1, man_auth}},
			      {rct_ssh,{ssh,[manual_connect]}},
			      {rct_rs232,console},
			      {cth_conn_log,[]}]}];
	_ ->
	    [{ct_hooks, [{rct_htmllink,[]},
			 {rct_consserv,cs1},
			 {rct_rs232,console},                 
			 {rct_power,power1},
			 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}],[get_all]}},
			 {rct_scp, scp1},
			 {rct_core,[]},
			 {rct_netconf, {nc1, pretty}},
			 {cth_conn_log,[]}]}]
    end.

%% @hidden
init_per_suite(Config) -> 
    Config.
%% @hidden
end_per_suite(_Config) ->
    ok.

%% @hidden
init_per_testcase(_TestCase, Config) ->
    ct:pal("TC start"),
    Config.
%% @hidden
end_per_testcase(_TestCase, Config) ->
    ct:pal("TC end"),
    	    case proplists:get_value(tc_status, Config) of
    		ok -> 
		    ct:log("status ~p",[proplists:get_value(tc_status, Config)]);
    		R ->
		    ct:log("test case ~p",[R]),
		    poll_esi(Config)
    	    end,
   ok.	

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [install].

install(Config) ->
%    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Hw = atom_to_list(Hwa = ct:get_config({test_nodes,1})),
    BOARDTYPE = ct:get_config({Hwa,board_type}),
    %%Check if it is a secure card
    case check_if_vc_board() of
	"yes" -> install_sec_card(Hw, Config);
	_ ->
	    SecondStageUboot = ["Ericsson Version: 2/CXC1736593/",
	    			"Ericsson Version: 4/CXC1736593/",
	    			"Ericsson Version: 7/CXC1736593/",
	    			"Ericsson Version: 2/CXC1739155/",
	    			"Ericsson Version: 4/CXC1739155/",
				"Ericsson Version: 7/CXC1739155/",
	    			"Ericsson Version: 7/CXC1740623/"],
	    %% ok = rct_power:cycle(power1),
	    %% {ok,_} = ct_telnet:expect(console, SecondStageUboot, [{timeout,60000}]),
	    %% ok = ct_telnet:send(console, "/home/sirpa/bin/pgh_restart_board"),
	    %% ok = rct_rs232:login(console),
	    rct_rs232:login(console),
	    ok = ct_telnet:send(console, "cup --reboot"),
	    ct_telnet:expect(console,SecondStageUboot,[{timeout,60000},no_prompt_check]),
	    {ok,_} = expect(["Hit any key to stop autoboot:"],90000),
	    timer:sleep(1000),
	    ok = ct_telnet:send(console, "\n"),
	    {ok,_} = ct_telnet:expect(console, "=> ", [{timeout,10000},no_prompt_check]),	    
	    case BOARDTYPE of
		"dus5301" -> %dus53
		    CMD1 = "dhcp;setenv bootargs console=ttyAMA0 earlyprintk debug initrd=0x8000000,32M rdinit=/addons/bin/labloader.sh",
		    %% CMD1 = "dhcp;setenv bootargs console=ttyAMA0 earlyprintk debug initrd=0x8000000,100M rdinit=/addons/bin/labloader.sh",
		    ok = ct_telnet:send(console,CMD1),
		    {ok,_} = ct_telnet:expect(console, "=> ", [{timeout,10000},no_prompt_check]),
		    CMD2 = "rcs 1;mw.b 8000000 0 2000000; tftp 4000000 " ++ Hw ++ "/linux.img;tftp 8000000 " ++ Hw ++ "/bootfs.cpio.gz;imxtract 4000000 cpm2_30_fdt 5200000;merge_dtb 5200000 5000000;bootm 4000000 - 5000000",
		    %% CMD2 = "rcs 1;tftp 4000000 " ++ Hw ++ "/linux.img;tftp 8000000 " ++ Hw ++ "/bootfs.cpio.gz;imxtract 4000000 cpm2_30_fdt 5200000;merge_dtb 5200000 5000000;bootm 4000000 - 5000000",
		    ok = ct_telnet:send(console,CMD2);
		"dus3301" -> %dus33
		    CMD1 = "dhcp;setenv bootargs console=ttyAMA0 earlyprintk debug initrd=0x8000000,32M rdinit=/addons/bin/labloader.sh",
		    ok = ct_telnet:send(console,CMD1),
		    {ok,_} = ct_telnet:expect(console, "=> ", [{timeout,10000},no_prompt_check]),
		    CMD2 = "rcs 1;mw.b 8000000 0 2000000; tftp 4000000 " ++ Hw ++ "/linux.img;tftp 8000000 " ++ Hw ++ "/bootfs.cpio.gz;imxtract 4000000 cpm2_20_fdt 5200000;merge_dtb 5200000 5000000;bootm 4000000 - 5000000",
		    ok = ct_telnet:send(console,CMD2);
		"dus3201tk" -> %dus32 tk (dus32 from R9 and up)
		    CMD1 = "dhcp;setenv bootargs console=ttyAMA0 earlyprintk debug initrd=0x8000000,32M rdinit=/addons/bin/labloader.sh",
		    ok = ct_telnet:send(console,CMD1),
		    {ok,_} = ct_telnet:expect(console, "=> ", [{timeout,10000},no_prompt_check]),
		    CMD2 = "rcs 1;mw.b 8000000 0 2000000; tftp 4000000 " ++ Hw ++ "/linux.img;tftp 8000000 " ++ Hw ++ "/bootfs.cpio.gz;imxtract 4000000 cpm2_32_fdt 5200000;merge_dtb 5200000 5000000;bootm 4000000 - 5000000",
		    ok = ct_telnet:send(console,CMD2);
		"dus5201tk" -> %dus52 tk (dus52 from R9 and up)
		    CMD1 = "dhcp;setenv bootargs console=ttyAMA0 earlyprintk debug initrd=0x8000000,32M rdinit=/addons/bin/labloader.sh",
		    ok = ct_telnet:send(console,CMD1),
		    {ok,_} = ct_telnet:expect(console, "=> ", [{timeout,10000},no_prompt_check]),
		    CMD2 = "rcs 1;mw.b 8000000 0 2000000; tftp 4000000 " ++ Hw ++ "/linux.img;tftp 8000000 " ++ Hw ++ "/bootfs.cpio.gz;imxtract 4000000 cpm2_52_fdt 5200000;merge_dtb 5200000 5000000;bootm 4000000 - 5000000",
		    ok = ct_telnet:send(console,CMD2);
		BOARDTYPE when BOARDTYPE == "dus6502";
			       BOARDTYPE == "dus6303" ->
		    CMD1 = "dhcp;setenv bootargs console=ttyAMA0 earlyprintk debug initrd=0x8000000,32M rdinit=/addons/bin/labloader.sh",
		    ok = ct_telnet:send(console,CMD1),
		    {ok,_} = ct_telnet:expect(console, "=> ", [{timeout,10000},no_prompt_check]),
		    CMD2 = "rcs 1;mw.b 8000000 0 2000000; tftp 4000000 " ++ Hw ++ "/linux.img;tftp 8000000 " ++ Hw ++ "/bootfs.cpio.gz;imxtract 4000000 cpm2_02_fdt 5200000;merge_dtb 5200000 5000000;bootm 4000000 - 5000000",
		    ok = ct_telnet:send(console,CMD2);
		_ ->
		    CMD1 = "dhcp;setenv bootargs console=ttyAMA0,115200n8 earlyprintk debug initrd=0x8000000,30M ramdisk_size=30000 rdinit=/addons/bin/labloader.sh",
		    ok = ct_telnet:send(console,CMD1),
		    {ok,_} = ct_telnet:expect(console, "=> ", [{timeout,10000},no_prompt_check]),	    
		    CMD2 = "rcs 1;tftp 0x4000000 " ++ Hw ++ "/linux.img;tftp 0x8000000 " ++ Hw ++ "/bootfs.cpio.gz;bootm 0x4000000 - 0x5000000",
		    ok = ct_telnet:send(console,CMD2)
	    end,
	    {ok, _} = ct_telnet:expect(console, "Welcome to Autointegration", [{timeout,10000},no_prompt_check]),
	    curl(list_to_atom(Hw)),
	    {ok,_} = expect(["Integrate will continue after reboot"],120000),
	    {ok,_} = ct_telnet:expect(console, SecondStageUboot, [{timeout,60000}]),
	    case ct_telnet:expect(console, [{login, "login:"},{cvp,"CvP successful"}],
				  [{halt,[{watchdog,Uboot}||Uboot<-SecondStageUboot]},
				   {timeout,30000},no_prompt_check]) of
		{error,timeout} ->
		    ct:log(lightred,"No \"CvP successful\" after reboot, waiting 6 min to see what happens"),
		    ct:fail("No \"CvP successful\" after reboot");		
		{ok,Result} ->
		    case Result of
			{login,_} ->
			    timer:sleep(15000);
			{cvp,_} ->
			    timer:sleep(5000)
		    end,
		    ok = rct_rs232:login(console),
		    %% rct_scp:to_target(scp1, ?VC_CRT, "/home/sirpa/dev_patches/", 5),
		    %% rct_scp:to_target(scp1, ?VC_KEY, "/home/sirpa/dev_patches/", 5),
		    case wait_for_rbsConfigLevel(nc1, "anything", 210) of 
			ok ->
			    ok;
			{error, netconf_timeout} ->
			    ct:fail("Could not connect with netconf after reboot") 
		    end,
		    ct_telnet:expect(console, "Trick to collect what is happening on console during 120 seconds", [{timeout,120000},no_prompt_check]),
		    ok = check_still_logged_in();
		    %% ok = check_still_logged_in(),
		    %% case rct_check_HW:check_Taipan_Cobra_Trinity(console,BOARDTYPE) of
		    %% 	{error, Reason} ->
		    %% 	    ct:fail(Reason);
		    %% 	ok ->
		    %% 	    ok
		    %% end;
		{error,{watchdog,_}} ->
		    ct:log(lightred,"Board is rebooting one extra time, watchdog?"),
		    case ct_telnet:expect(console, "login:", [{timeout,360000},no_prompt_check]) of
			{ok, _} -> % board comes up
			    ok = rct_rs232:login(console),
						% trick to collect console log during 2 minutes
			    ct_telnet:expect(console, "root@.*:~# $", [{timeout,120000},no_prompt_check]),
						% Verify that board has not rebooted by checking that we still have userprompt
			    check_still_logged_in();
			_ -> % board does NOT come up, attempt to save next TC by powercycle
			    ok
		    end,
		    ct:fail("Watchdog reset")
	    end
    end.

expect(Match,Timeout) ->
    case ct_telnet:expect(console, Match,
			  [{halt,[{watchdog,["Ericsson Version: 2/CXC1736593/",
					     "Ericsson Version: 4/CXC1736593/",
					     "Ericsson Version: 7/CXC1736593/",
					     "Ericsson Version: 2/CXC1739155/",
					     "Ericsson Version: 4/CXC1739155/",
					     "Ericsson Version: 7/CXC1739155/",
					     "Ericsson Version: 7/CXC1740623/"]}]},
			   {timeout,Timeout},no_prompt_check]) of
	{error,{watchdog,_}} -> 
	    ct:log(lightred,"Board is rebooting one extra time, watchdog?"),
	    ct_telnet:expect(console, "Trick to collect console logs", [{timeout,30000},no_prompt_check]),
	    ct:fail("Watchdog reset while waiting for ~p",[Match]);
	{error,timeout} ->
	    ct:log(lightred,"~p NOT matched, waiting 6 min to see what happens",["Match"]),
	    ct_telnet:expect(console, "Trick to collect console logs", [{timeout,360000},no_prompt_check]),
	    ct:fail("Timeout while waiting for ~p",[Match]);
	{ok,Reply} ->
	    {ok,Reply}
    end.

curl(Hw) ->
    [{ssh, IP},_,_,_] = ct:get_config({Hw,ssh_lmt_ipv4}),
%    [{host, Host},{username, Username},{password, Password}] = ct:get_config({Hw,sftp_server}),
    [{host, Host},{username, Username},{password, Password}] = ct:get_config(sftp_server),
    SftpAiInstallDir = ct:get_config({Hw,sftp_ai_install_dir}),
    SftpAiInstallDir2 = re:replace(SftpAiInstallDir,"/","%2F",[{return,list},global]),
    MyTime = integer_to_list(ms_since_1970()),
    Cmd = "curl -v -k --noproxy " ++ IP ++ " --data \"host=" ++ Host ++ 
                                                  "&username=" ++ Username ++ 
                                                  "&password=" ++ Password ++ 
                                                  "&filename=" ++ SftpAiInstallDir2 ++ "%2FRbsSummaryFile.xml&DoIntegrate=Integrate" ++ 
                                                  "&utcMs="    ++ MyTime ++ "\" "
	                                          "https://" ++ IP ++ "/cgi-bin/nl_gui:post",
    timer:sleep(1000),
    ct:log("~s",[Cmd]),
    R = os:cmd(Cmd),
    ct:log(re:replace(R,"<","\\&lt;",[{return,list},global])).    

ms_since_1970() ->
    list_to_integer(string:strip(os:cmd("date -u +%s"),right,$\n)) * 1000.

check_still_logged_in() ->
    ct_telnet:send(console, ""),
    case ct_telnet:expect(console, "root@.*:~# $", [{timeout,5000},no_prompt_check]) of
	{ok, _} -> 
	    ok;
	_ ->
	    ct:log(lightred,"No userpromt, board has rebooted"),
	    {error, not_logged_in}
    end.

wait_for_rbsConfigLevel(NC, RbsConfigLevel, Timeout) ->
    ct:log("Wait for netconf rbsConfigLevel: " ++ RbsConfigLevel),
    Time = secs_since_1970(),
    wait_for_rbsConfigLevel(NC, RbsConfigLevel, Time, Time, Time + Timeout).

wait_for_rbsConfigLevel(NC, RbsConfigLevel, StartTime, Time, Expire) when Time < Expire ->
    case ct_netconfc:open(NC,[{timeout, 5000}]) of
	{ok,_} ->
	    case ct_netconfc:get(nc1,{'ManagedElement',
				      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				      [{managedElementId,[],["1"]},
				       {'NodeSupport',[],
					[{nodeSupportId,[],["1"]},
					 {'AutoProvisioning',[],
					  [{autoProvisioningId,[],["1"]}]}]}]}) of
		{error,no_such_client} -> % During startup, netconf sesssion may be disconnected, give it one more retry.
		    ct_netconfc:close_session(NC),
		    case get(netconf_disconnected_givit_1_more_try) of
			undefined ->
			    ct:log("netconf connection disconnected after ~p seconds, giving it one more retry",[Time - StartTime]),
			    put(netconf_disconnected_givit_1_more_try,no),
			    timer:sleep(5000),
			    wait_for_rbsConfigLevel(NC, RbsConfigLevel, StartTime, Time, Expire);
			no ->
			    ct:log(lightred,"netconf connection disconnected after ~p seconds, failed retry, giving up",[Time - StartTime]),
			    {error, netconf_timeout}
		    end;
		{error,closed} -> % During startup, netconf sesssion may be closed, give it one more retry.		    
		    ct_netconfc:close_session(NC),
		    case get(netconf_disconnected_givit_1_more_try) of
			undefined ->
			    ct:log("netconf connection closed after ~p seconds, giving it one more retry",[Time - StartTime]),
			    put(netconf_disconnected_givit_1_more_try,no),
			    timer:sleep(5000),
			    wait_for_rbsConfigLevel(NC, RbsConfigLevel, StartTime, Time, Expire);
			no ->
			    ct:log(lightred,"netconf connection closed after ~p seconds, failed retry, giving up",[Time - StartTime]),
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
			  [{autoProvisioningId,[],["1"]}]}]}]}]} ->		    
		    ct:log(yellow," rbsConfigLevel not set, Retrying in 5 seconds",[]),
		    ct_netconfc:close_session(NC),
		    timer:sleep(5000),
		    wait_for_rbsConfigLevel(NC, RbsConfigLevel, StartTime, secs_since_1970(), Expire);
		{ok,[{'ManagedElement',
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],["1"]},
		       {'NodeSupport',
			[{xmlns,"urn:com:ericsson:ecim:RmeSupport"}],
			[{nodeSupportId,[],["1"]},
			 {'AutoProvisioning',
			  [{xmlns,"urn:com:ericsson:ecim:RmeAI"}],
			  [{autoProvisioningId,[],["1"]},
			   {rbsConfigLevel,[],[RBSCONFIGLEVEL]}]}]}]}]} ->
		    case RbsConfigLevel of
			"anything" ->
			    ct:log("rbsConfigLevel: " ++ RBSCONFIGLEVEL ++ " took ~p seconds", [secs_since_1970() - StartTime]),
			    ct_netconfc:close_session(NC);
			RBSCONFIGLEVEL ->
			    ct:log("rbsConfigLevel: " ++ RbsConfigLevel ++ " took ~p seconds", [secs_since_1970() - StartTime]),
			    ct_netconfc:close_session(NC);		    
			_ ->
			    ct:log(yellow,"Wrong rbsConfigLevel: ~s, Retrying in 5 seconds",[RBSCONFIGLEVEL]),
			    ct_netconfc:close_session(NC),
			    timer:sleep(5000),
			    wait_for_rbsConfigLevel(NC, RbsConfigLevel, StartTime, secs_since_1970(), Expire)
		    end;
		NewReply ->
		    ct:log("Unexpected reply to AutoProvisioning ~p, contact etxkols",[NewReply]),
		    ct:fail("Unexpected reply to AutoProvisioning")
	    end;		   
	Other ->
	    ct:log(yellow,"Could not connect with netconf, Retrying in 5 seconds. Reason: ~p",[Other]),
	    timer:sleep(5000),
	    wait_for_rbsConfigLevel(NC, RbsConfigLevel, StartTime, secs_since_1970(), Expire)
    end;

    %% case ct_netconfc:open(NC,[{timeout, 5000}]) of
    %% 	{ok,_} ->
    %% 	    {ok,[{'ManagedElement',
    %% 		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    %% 		  [{managedElementId,[],["1"]},
    %% 		   {'NodeSupport',
    %% 		    [{xmlns,"urn:com:ericsson:ecim:RmeSupport"}],
    %% 		    [{nodeSupportId,[],["1"]},
    %% 		     {'AutoProvisioning',
    %% 		      [{xmlns,"urn:com:ericsson:ecim:RmeAI"}],
    %% 		      [{autoProvisioningId,[],["1"]},
    %% 		       {rbsConfigLevel,[],[RBSCONFIGLEVEL]}]}]}]}]} = 
    %% 		ct_netconfc:get(nc1,{'ManagedElement',
    %% 				     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
    %% 				     [{managedElementId,[],["1"]},
    %% 				      {'NodeSupport',[],
    %% 				       [{nodeSupportId,[],["1"]},
    %% 					{'AutoProvisioning',[],
    %% 					 [{autoProvisioningId,[],["1"]}]}]}]}),
    %% 	    case RbsConfigLevel of
    %% 		"anything" ->
    %% 		    ct:log("rbsConfigLevel: " ++ RBSCONFIGLEVEL ++ " took ~p seconds", [secs_since_1970() - StartTime]),
    %% 		    ct_netconfc:close_session(NC);
    %% 		RBSCONFIGLEVEL ->
    %% 		    ct:log("rbsConfigLevel: " ++ RbsConfigLevel ++ " took ~p seconds", [secs_since_1970() - StartTime]),
    %% 		    ct_netconfc:close_session(NC);		    
    %% 		_ ->
    %% 		    ct:log(yellow,"Wrong rbsConfigLevel: ~s, Retrying in 5 seconds",[RBSCONFIGLEVEL]),
    %% 		    ct_netconfc:close_session(NC),
    %% 		    timer:sleep(5000),
    %% 		    wait_for_rbsConfigLevel(NC, RbsConfigLevel, StartTime, secs_since_1970(), Expire)
    %% 	    end;
    %% 	Other ->
    %% 	    ct:log(yellow,"Could not connect with netconf, Retrying in 5 seconds. Reason: ~p",[Other]),
    %% 	    timer:sleep(5000),
    %% 	    wait_for_rbsConfigLevel(NC, RbsConfigLevel, StartTime, secs_since_1970(), Expire)
    %% end;
wait_for_rbsConfigLevel(_NC, RbsConfigLevel, StartTime, Time, _Expire) ->
    ct:log(lightred,"Could not connect with netconf or set rbsConfigLevel ~s within ~p seconds",[RbsConfigLevel, Time - StartTime]),
    {error, netconf_timeout}.

secs_since_1970() ->
    {MSec,Sec,_} = os:timestamp(),
    (MSec * 1000000) + Sec.
%%##############################################
%% Functions for vc cards
%%#############################################
%%Install secure card
install_sec_card(_Hw, _Config)->
    Hwa = ct:get_config({test_nodes,1}),
    case aic_curl:board_restore() of
	ok ->    
	    %%check_expect_from_console("boot_count = 7",300000),
	    check_expect_from_console("Secure Boot Enabled",300000),
	    check_expect_from_console("Reset Status = SW Ordered","Bad device usb" , 20000, ""),
	    check_expect_from_console("Network Loader Ready:","Reset Status = SW Ordered",
				      40000, "Detect extra restart");
	{_, "HTTP/1.1 403 Forbidden"} -> ct:log("Probably already in NL state"), ok; 
	Response1 ->
	    ct:fail(Response1)
    end,   
    case ct:get_config({Hwa,board_type}) of
	BOARDTYPE when BOARDTYPE == "tcu03"->
	    case aic_curl:nl_upgrade() of
		ok -> check_expect_from_console("Secure Boot Enabled",300000),
		      check_expect_from_console("Reset Status = SW Ordered","Bad device usb" , 20000, ""),%%old fault check.
		      check_expect_from_console("Network Loader Ready:","Reset Status = SW Ordered",
						40000, "Detect extra restart");
		Response2 -> 	
		    ct:fail(Response2)
	    end;
	_ ->ok
    end,
    %%not really ready after printout "Network Loader Ready:"
    timer:sleep(10000),

    ct:log("Installing UP"),
    case aic_curl:install_sw() of
	ok -> ok;
	Response3 -> 	
	    ct:fail(Response3)
    end, 
    %%TODO put in more check 

    %%Check mo
    case  site_config_complete([]) of
	ok -> ok;
	{error,String}  -> 
	    ct:fail(String)

    end.

%%Check String from console log, if not try to fetch fake esi in NL state
check_expect_from_console(String, TimeOut)->
    case  ct_telnet:expect(console, String, [{timeout,TimeOut},no_prompt_check]) of
	{ok, _} -> ok;
	_ ->  ct:fail("Didn't receive ~s",[String])
    end.
%%Check String from console log, if not try to fetch fake esi in NL state
check_expect_from_console(ExpectString, HaltString, TimeOut, ErrorReason)->
    case  ct_telnet:expect(console, [{expectstring, ExpectString}], 
			   [sequence,{halt,[{haltstring,HaltString}]},
			    {timeout,TimeOut},no_prompt_check]) of
	{ok, _} -> ok;
	{error,timeout} -> 
	    ct:fail("Didn't receive ~s",[ExpectString]);
	{error,{haltstring,[HaltString]}} ->  ct:fail("Received ~p before ExpectString ~p: ~p  ",
						      [HaltString, ExpectString, ErrorReason]);
	Unknown ->  ct:fail("Unknown reason: ~p",[Unknown])
    end.  


site_config_complete(_) ->
    site_config_complete(nc1, 600).
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
			  [{autoProvisioningId,[],["1"]}]}]}]}]} ->
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
