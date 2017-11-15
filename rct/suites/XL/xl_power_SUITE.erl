%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	xl_power_SUITE.erl %
%%% @author erarube
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R2A/R3A/R4A/R6A/3
-module(xl_power_SUITE).
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
%%% R2A/2      2012-10-29 etxkols     Collect console printouts while waiting for 2 min
%%% R2A/3      2013-01-23 etxkols     Wrote wrong thing here
%%% R2A/4      2013-01-23 etxkols     Added rct_core hook
%%% R2A/5      2013-08-13 etxkols     Changed raw to no_prompt_check in ct_telnet:expect
%%% R2A/6      2013-09-02 etxkols     Added watchdog check
%%% R2A/7      2013-09-25 etxkols     fix for late watchdog
%%% R2A/8      2013-10-01 etxkols     Fix for ARM
%%% R2A/9      2014-01-16 etxkols     Support for dus5201. 
%%% R2A/10     2014-03-03 etxkols     Changed sleep after login to 90 seconds
%%% R2A/11     2014-05-28 etxkols     Check Cobra and Trinity link status 
%%% R2A/12     2014-06-04 etxkols     More HW fixes and adaption to EE TCU03
%%% R2A/13     2014-06-04 etxkols     More HW fixes and adaption to EE TCU03
%%% R2A/14     2014-07-08 etxkols     Added cth_conn_log hook
%%% R3A/1      2014-09-03 etxkols     Added netconf check
%%% R3A/2      2014-09-10 etxkols     Support for VC and secureboot boards
%%% R3A/3      2014-11-24 etxkols     Redesigned
%%% R3A/4      2015-02-25 etxkols     Removed PPC and fixed uboot match
%%% R3A/5      2015-03-23 etxkols     Changing search for "login:" to "CvP successful"
%%% R3A/6      2015-05-06 etxkols     login: back in business
%%% R4A/1      2015-07-16 etxkols     Removed calls to rct_check_HW.erl on EEs request
%%% R4A/2      2015-09-21 eransbn     Add support for vc card's
%%% R4A/3      2015-09-30 etxmlar     now() depricated in OTP 18 changed to os:timestamp() 
%%% R4A/4      2015-10-05 etxkols     Added get_all to get pramfs logs
%%% R4A/5      2016-01-07 etxnnor     Removed check for VC in end_per_testcase when fetching ESI
%%% R6A/1      2016-08-31 etxkols     dus53
%%% R6A/2      2016-08-31 etxkols     more dus53
%%% R6A/3      2017-06-14 erarube     Support for DUSx3 R1A & R2A
%%% ----------------------------------------------------------
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 cycle/1]).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_consserv, rct_rs232, rct_power, rct_logging, rct_core
%% @end
%%--------------------------------------------------------------------
suite() -> 

    case check_if_vc_board() of

	"yes" -> [
		  {ct_hooks, [{rct_htmllink,[]},
			      {rct_power,node},
			      {rct_consserv,cs1},
			      {rct_netconf, {nc1, man_auth}},
			      {rct_ssh,{ssh,[manual_connect]}},
			      {rct_rs232,console},
			      {cth_conn_log,[]}]}];
	_ ->
	   [{ct_hooks, [{rct_htmllink,[]},
			{rct_consserv,cs1},
			{rct_rs232,console},                 
			{rct_power,node},
			{rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}],[get_all]}},
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
    Config.
%% @hidden
end_per_testcase(_TestCase, Config) ->
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
    [cycle].

%%--------------------------------------------------------------------
%% @doc 
%% Power cycle board, wait for login: prompt, wait 2 minutes and check still login: prompt.<br/><br/>
%% @end
%%--------------------------------------------------------------------
cycle(_) ->
    case check_if_vc_board() of
	"yes" -> power_on_sec_card();
	_ ->
	    %%    BOARDTYPE = proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))),
	    SecondStageUboot = ["Ericsson Version: 2/CXC1736593/",
				"Ericsson Version: 4/CXC1736593/",
				"Ericsson Version: 7/CXC1736593/",
				"Ericsson Version: 2/CXC1739155/",
				"Ericsson Version: 4/CXC1739155/",
				"Ericsson Version: 7/CXC1739155/",
				"Ericsson Version: 7/CXC1740623/"],
	    rct_power:cycle(node),
	    {ok, _} = ct_telnet:expect(console, SecondStageUboot, [{timeout,10000}]),
						% If "2:nd Stage Boot Loader" is received again, board has rebooted one extra time
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
		    case wait_for_rbsConfigLevel(nc1, "anything", 180) of
			ok ->
			    ok;
			{error, netconf_timeout} ->
			    ct:fail("Could not connect with netconf after reboot") 
		    end,
		    ct_telnet:expect(console, "Trick to collect what is happening on console during 50 seconds", [{timeout,50000},no_prompt_check]),
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
		    ct_telnet:expect(console, "trick to collect printouts after watchdog", [{timeout,30000},no_prompt_check]),
		    ct:fail("Watchdog reset")
	    end
    end.
power_on_sec_card()->
    rct_power:cycle(node),
    check_expect_from_console("Secure Boot Enabled",300000),
    check_expect_from_console("Reset Status = Power On","Bad device usb" , 20000, ""),
    check_expect_from_console("boot_count = 1",300000),
    
    case site_config_complete([]) of
	ok -> ok;
	{error,String}  -> 
	    ct:fail(String)

    end,
    ok.
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
	    {ok,[{'ManagedElement',
		  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		  [{managedElementId,[],["1"]},
		   {'NodeSupport',
		    [{xmlns,"urn:com:ericsson:ecim:RmeSupport"}],
		    [{nodeSupportId,[],["1"]},
		     {'AutoProvisioning',
		      [{xmlns,"urn:com:ericsson:ecim:RmeAI"}],
		      [{autoProvisioningId,[],["1"]},
		       {rbsConfigLevel,[],[RBSCONFIGLEVEL]}]}]}]}]} = 
		ct_netconfc:get(nc1,{'ManagedElement',
				     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				     [{managedElementId,[],["1"]},
				      {'NodeSupport',[],
				       [{nodeSupportId,[],["1"]},
					{'AutoProvisioning',[],
					 [{autoProvisioningId,[],["1"]}]}]}]}),
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
	Other ->
	    ct:log(yellow,"Could not connect with netconf, Retrying in 5 seconds. Reason: ~p",[Other]),
	    timer:sleep(5000),
	    wait_for_rbsConfigLevel(NC, RbsConfigLevel, StartTime, secs_since_1970(), Expire)
    end;
wait_for_rbsConfigLevel(_NC, RbsConfigLevel, StartTime, Time, _Expire) ->
    ct:log(lightred,"Could not connect with netconf or set rbsConfigLevel ~s within ~p seconds",[RbsConfigLevel, Time - StartTime]),
    {error, netconf_timeout}.

secs_since_1970() ->
    {MSec,Sec,_} = os:timestamp(),
    (MSec * 1000000) + Sec.
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
