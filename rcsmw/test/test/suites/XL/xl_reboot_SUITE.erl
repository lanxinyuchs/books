%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	xl_reboot_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/1
-module(xl_reboot_SUITE).
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
%%% R2A/1      2012-10-26 etxbolb     Created
%%% R2A/2      2013-01-23 etxkols     Added rct_core hook
%%% R2A/3      2013-08-13 etxkols     Changed raw to no_prompt_check in ct_telnet:expect
%%% R2A/4      2013-08-14 etxkols     Added check for watchdog
%%% R2A/5      2013-09-02 etxkols     Fixed compiler warning
%%% R2A/6      2013-09-25 etxkols     Fix to catch late reboots
%%% R2A/7      2013-10-01 etxkols     Fix for ARM
%%% R2A/8      2013-10-01 etxkols     Removed unused line
%%% R2A/9      2014-01-16 etxkols     Support for dus5201. 
%%% R2A/10     2014-08-28 etxkols     Check Cobra and Trinity link status 
%%% R2A/11     2014-06-04 etxkols     More HW fixes
%%% R2A/12     2014-06-04 etxkols     More HW fixes
%%% R2A/13     2014-06-11 etxkols     Fix of very rare problem
%%% R2A/14     2014-07-08 etxkols     Added cth_conn_log hook
%%% R3A/1      2014-09-03 etxkols     Added netconf check
%%% R3A/2      2014-09-10 etxkols     Support for VC and secureboot boards
%%% R3A/3      2014-11-24 etxkols     Redesigned
%%% R3A/1      2015-02-20 etxkols     Timer changes
%%% R3A/2      2015-02-25 etxkols     Removed PPC and fixed uboot match
%%% R3A/6      2015-03-23 etxkols     Changing search for "login:" to "CvP successful"
%%% R3A/7      2015-04-24 eransbn     Added support for vc card
%%% R3A/8      2015-05-06 etxkols     login: back in business
%%% R3A/7      2015-04-24 eransbn     Improved support for vc card
%%% R4A/1      2015-05-14 etxivri     Minor bug fix in end per tc.
%%% R4A/2      2015-07-16 etxkols     Removed calls to rct_check_HW.erl on EEs request
%%% R4A/4      2015-09-30 etxmlar     now() depricated in OTP 18 changed to os:timestamp() 
%%% R4A/5      2015-10-12 eransbn     Added restart_warm tc 
%%% R7A/1      2016-09-05 etxivri     Added restart_warm tc via coli.
%%% R8A/1      2017-01-09 etxivri     Update check in restart_warm.
%%% R9A/1      2017-03-10 etxivri     Update due to rolename should be unique.
%%% R9A/2      2017-03-10 etxivri     Update to printout cli results. 
%%% R9A/3      2017-03-20 etxivri     Add more printout to se why TC sometimes fails.
%%% R9A/4      2017-04-05 erarube     Fix for printouts which may come on console after
%%%                                   Warm restart, and retrigger timer. Like:
%%%                                   ncp: NCP_NCA_WAIT_FOR_RX_INTR interrupted by signal!
%%%                                   Change of timeout from 30 to 45 seconds after Warm Restart.
%%% R10A/1     2017-05-29 erarube     Update due to U-BOOT DUSx3 R1A Boards
%%% R11A/1     2017-08-29 etxivri     Update to be used in git. Some printout is needed.
%%% R12A/1     2017-12-01 etxivri     enable warm restart is noot needed.
%%% ----------------------------------------------------------
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0,
	 restart_warm/1,
	 restart_warm_coli/1,
         reboot/1]).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_consserv, rct_rs232, rct_power, rct_logging, rct_core
%% @end
%%--------------------------------------------------------------------
suite() -> 


    case check_if_vc_board() of
	"yes"-> 
	    [{ct_hooks, [{rct_htmllink,[]},
			 {rct_rs232,console},
			 {rct_power,node},                                           
			 {cth_conn_log,[]},
			 {rct_coli, {coli, [manual_connect]}},
			 {rct_consserv,cs1},
			 {rct_cli, {cli, [{user, "SysAdminTest"}, {password, "SysAdminTest"},manual_connect]}},
			 {rct_netconf, {nc1, man_auth}}
			]}];
	_->
	    [{ct_hooks, [{rct_htmllink,[]},
			 {rct_consserv,cs1},
			 {rct_rs232,console},
			 {rct_coli, {coli, [manual_connect]}},
			 {rct_power,node},                                           
		%%	 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
			 {rct_core,[]},
			 {rct_cli, {cli, [{user, "SysAdminTest"}, {password, "SysAdminTest"},manual_connect]}},
			 {rct_netconf, {nc1, pretty}},
			 {cth_conn_log,[]}]}]
    end.

%%% ----------------------------------------------------------
%%% Definitions
%%% ----------------------------------------------------------

-define(XmlDirDw2,"/proj/rbs-g2-ci/GIT_repo/" ++ get_netconf_xml_version() ++ "/ci-support/netconf_xml/").

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
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [reboot].
%%--------------------------------------------------------------------
%% @doc 
%% reboot board, wait for login: prompt, wait 2 minutes and check still login: prompt<br/><br/>
%% If login:promt not appeard after reboot the board is power cycled and the current running instance
%% of the test case is marked faulty before next reboot test continues   
%% @end
%%--------------------------------------------------------------------
reboot(Config) ->
    case check_if_vc_board() of
	"yes"-> reboot_sec_card(Config);
	_->
%	    BOARDTYPE = proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))),
	    SecondStageUboot = ["Ericsson Version: 2/CXC1736593/",
				"Ericsson Version: 4/CXC1736593/",
				"Ericsson Version: 7/CXC1736593/",
				"Ericsson Version: 2/CXC1739155/",
				"Ericsson Version: 4/CXC1739155/",
				"Ericsson Version: 7/CXC1739155/",
				"Ericsson Version: 7/CXC1740623/"],
	    ok = check_power_cycle(SecondStageUboot),
	    ok = ct_telnet:send(console, "/home/sirpa/bin/pgh_restart_board"),
	    case ct_telnet:expect(console,SecondStageUboot,[{timeout,20000},no_prompt_check]) of
		{ok,_} ->
		    ok;
		_ ->
		    ct:log(lightred,"No uboot stage 2 in 20 seconds"),
		    ct_telnet:expect(console, "Trick to collect what is happening on console during 180 seconds", [{timeout,180000},no_prompt_check]),
		    ct:fail("No uboot stage 2 in 30 seconds")
	    end,
	    case ct_telnet:expect(console, [{login, "login:"},{cvp,"CvP successful"}],
				  [{halt,[{watchdog,Uboot}||Uboot<-SecondStageUboot]},
				   {timeout,30000},no_prompt_check]) of
		{error,timeout} ->
		    ct:log(lightred,"No \"CvP successful\" or login: after reboot, waiting 6 min to see what happens"),
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
		    ct_telnet:expect(console, "Trick to collect what is happening on console during 70 seconds", [{timeout,70000},no_prompt_check]),
						% Verify that board has not rebooted by checking that we still have userprompt
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
restart_warm(_Config) ->

    clear_llog(),

    %%create mo:s if needed 
    create_mo_CustomRule(nc1),
    create_mo_CustomRole(nc1),
    create_FieldReplaceableUnit(),

    %% ok = rct_coli:connect(coli),
    %% ct:log("Enable warm restart via coli /board/restart -ew"),
    %% {ok,_} = rct_coli:send(coli,"/board/restart -ew"), 
    %% ok = rct_coli:disconnect(coli),

    %% restart fieldreplacement unit
    rct_cli:connect(cli),
    rct_cli:send(cli,"ManagedElement=1,Equipment=1,FieldReplaceableUnit=1,restartUnit RESTART_WARM PLANNED_RECONFIGURATION 0"), 
    rct_cli:disconnect(cli),
 
    %%Check that EE don't restarts
    {error,timeout} =
    ct_telnet:expect(console, "Ericsson Version:", 
		     [{total_timeout,45000}, {idle_timeout,45000}, no_prompt_check]),
    %% {error,timeout} =
    %% ct_telnet:expect(console, "Ericsson Version:", 
    %%  		[{timeout,30000}, no_prompt_check]),
    %% {error,timeout} = 
    %% 	ct_telnet:expect(console,[{abc,""}],[sequence,{halt,[{haltstring,"Restarting system"}]},
    %% 			  {timeout,20000},no_prompt_check]),

  
    %%Pool restart
    case site_config_complete([]) of
	ok -> ok;
	{error, ErrorString} -> ct:fail(ErrorString)
    end,

    Result0 =  poll_attribute
    		 (cli,
    		  "operationalState=ENABLED",
    		  "show ManagedElement=1,Equipment=1,FieldReplaceableUnit=1", 300),
    case Result0 of
    	{ok,_}-> ok;
    	_ -> ct:fail(Result0)
    end,

    check_llog("Warm", 1),
    ok.


%%--------------------------------------------------------------------
%% @doc 
%% Trig restart warm using coli.
%% @end
%%--------------------------------------------------------------------
restart_warm_coli(_Config) ->
    ct:pal("Clear llog"),
    clear_llog(),

    ok = rct_coli:connect(coli),
    ct:pal("Trig warm restart via coli /board/restart -w"),
    {ok,_} = rct_coli:send(coli,"/board/restart -w"), 
    rct_coli:disconnect(coli),

    
    %%Check that EE don't restarts
    {error,timeout} = 
	ct_telnet:expect(console,[{abc,""}],[sequence,{halt,[{haltstring,"Restarting system"}]},
			  {timeout,20000},no_prompt_check]),
  
    %%Pool restart
    ct:pal("Check for SITE_CONFIG_COMPLETE."),
    case site_config_complete([]) of
	ok -> ok;
	{error, ErrorString} -> ct:fail(ErrorString)
    end,

    ct:pal("Check llog"),
    check_llog("Warm", 1),

    ct:log("Sleep to get more logs if needed."),
    timer:sleep(30000), %% to collect more logs.

    ok.


%%========================================================================
%% Secure card test case
%%========================================================================

reboot_sec_card(Config) ->
    %%create mo:s if needed 
    create_mo_CustomRule(nc1),
    create_mo_CustomRole(nc1),
    create_FieldReplaceableUnit(),
    %% restart fieldreplacement unit
    rct_cli:connect(cli),
    rct_cli:send(cli,"ManagedElement=1,Equipment=1,FieldReplaceableUnit=1,restartUnit RESTART_COLD PLANNED_RECONFIGURATION 0"), 
    rct_cli:disconnect(cli),
 
    %%read console log
    Hw = atom_to_list(ct:get_config({test_nodes,1})),

  %%  check_expect_from_console("Restarting system", 20000, Hw, Config),
    check_expect_from_console("Secure Boot Enabled",300000 , Hw, Config),
    check_expect_from_console("Reset Status = SW Ordered",300000 , Hw, Config),
  %%  check_expect_from_console("rcs_start: OTP_ROOT",300000 , Hw, Config),

    %%Pool restart
    case site_config_complete([]) of
	ok -> ok;
	{error, ErrorString} -> ct:fail(ErrorString)
    end,

    Result0 =  poll_attribute
    		 (cli,
    		  "operationalState=ENABLED",
    		  "show ManagedElement=1,Equipment=1,FieldReplaceableUnit=1", 300),
    case Result0 of
    	{ok,_}-> ok;
    	_ -> ct:fail(Result0)
    end,
    ok.

%% =================================================================================%%%
%%                             Internal functions                                    %%
%% =================================================================================%%%
check_still_logged_in() ->
    ct_telnet:send(console, ""),
    case ct_telnet:expect(console, "root@.*:~# $", [{timeout,5000},no_prompt_check]) of
	{ok, _} -> 
	    ok;
	_ ->
	    ct:log(lightred,"No userpromt, board has rebooted"),
	    {error, not_logged_in}
    end.

% ok | {error, power_cycle}
check_power_cycle(SecondStageUboot) -> % power cycle if necessary
    rct_rs232:login(console),
    ok = ct_telnet:send(console, "\n"),
    case ct_telnet:expect(console, "root@.*:~# $", [{timeout,5000},no_prompt_check]) of
	{ok, _} -> % Board is up and ready for reboot
	    ct:log("Board has correct prompt, continue with reboot"),
	    ok;
	_ -> % Board is not up, power_cycle to attempt to correct
	    ct:log("Board has incorrect prompt, power cycle to attempt to login"),
	    rct_power:cycle(node),
	    {ok, _} = ct_telnet:expect(console, SecondStageUboot, [{timeout,60000}]),
	     case ct_telnet:expect(console, "login:", [{timeout,120000},no_prompt_check]) of
		{ok, _} ->	  
		     ok = rct_rs232:login(console),
	             % trick to collect console log during 2 minutes
		     ct_telnet:expect(console, "root@.*:~# $", [{timeout,120000},no_prompt_check]),
		     ok;
		 _Other ->
	             % trick to collect console log during 6 minutes
		     ct:log(lightred,"power cycle failed, waiting 6 minutes to see if watchdog kicks in"),
		     ct_telnet:expect(console, "root@.*:~# $", [{timeout,360000},no_prompt_check]),
		     {error, power_cycle}
	     end
    end.
    
%wait_for_rbsConfigLevel(_) ->
%    wait_for_rbsConfigLevel(nc1, "READY_FOR_SERVICE", 180).
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



%%========================================================================
%% Secure functions 
%%========================================================================
poll_attribute(Cli,ExpectedValue, Command, Timeout) ->
    ct:log("Start poll_attribute ~p ~n poll time ~ps",[Command, Timeout]),
    ok = rct_cli:connect(Cli),%%TODO: if Timeout == 0
    Return = poll_attribute(Cli, ExpectedValue, Command, Timeout, []),
    ok = rct_cli:disconnect(cli),
    ct:log("Poll result ~p",[Return]),
    Return.
poll_attribute(Cli,ExpectedValue, Command, Timeout, _Return) when Timeout > 0 ->
    Received = rct_cli:send(cli,Command , ExpectedValue, noprint),
    case Received  of
	{ok,{_,ExpectedValue}}  -> {ok, ExpectedValue};
	_->
	    timer:sleep(1000),
	    poll_attribute(Cli,ExpectedValue, Command, Timeout-1, Received)
    end;
poll_attribute(_Cli,_ExpectedValue, _Command, Timeout, Return) when Timeout =:= 0->
    Return.
%%Check String from console log, if not try to fetch fake esi in NL state
check_expect_from_console(String, TimeOut, _Hw, _Config)->
   case  ct_telnet:expect(console, String, [{timeout,TimeOut},no_prompt_check]) of
       {ok, _} -> ok;
       _ ->  ct:fail("Didn't receive ~s",[String])
end.
%% check_expect_from_console(ExpectString, HaltString, TimeOut, ErrorReason, Hw, Config)->
%%     case  ct_telnet:expect(console, [{expectstring, ExpectString}], 
%% 			   [sequence,{halt,[{haltstring,HaltString}]},
%% 			    {timeout,TimeOut},no_prompt_check]) of
%% 	{ok, _} -> ok;
%% 	{error,timeout} -> fetch_fake_esi(list_to_atom(Hw), Config),
%% 			   ct:fail("Didn't receive ~s",[ExpectString]);
%% 	{error,{haltstring,[HaltString]}} -> fetch_fake_esi(list_to_atom(Hw), Config),
%% 					     ct:fail("Received ~p before ExpectString ~p: ~p  ",
%% 						     [HaltString, ExpectString, ErrorReason]);
%% 	Unknown -> fetch_fake_esi(list_to_atom(Hw), Config),
%% 		   ct:fail("Unknown reason: ~p",[Unknown])
%%     end.  
get_netconf_xml_version()->
    %%Return latest version of xml files from dw2
    Version = os:cmd("/env/rbsg2/bin/blgen -file /env/rbsg2/app/build/0/recipe/TOOL/BL_cis-git.xml --track G2_3.0 --timeout 600"),
    Option = [global, {capture, all, list}],
    RE =  ".version=.([A-Z0-9]+).*",
    case re:run(Version, RE, Option) of
	{match,[[_ALL,Value]]} -> Value;
	{nomatch}->ct:fail("NoMatch  ~p",[Version]);
	{error, ErrType} -> ct:fail(ErrType);
	_ -> ct:fail("Unknown response from excecution from"
		    "\n/env/rbsg2/bin/blgen -file /env/rbsg2/app/build/0/recipe/TOOL/BL_cis-git.xml --track G2_2.0 --timeout 600")
    end.
    
check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.

netconf_open(Session, Param)->
    case check_if_vc_board() of
	"yes"->  ct_netconfc:open
		   (Session, [{user, "SysAdminTest"},
			      {password, "SysAdminTest"}|Param]);
	_-> ct_netconfc:open(Session,Param)

    end.
get_node_name() ->
    [{_, NodeName}] = ct:get_config(test_nodes),
    Node = atom_to_list(NodeName),
    Node.
create_FieldReplaceableUnit()->
    NodeName = get_node_name(),
    rct_cli:connect(cli),
    rct_cli:send(cli,"configure"),

    Option = [global, {capture, all, list}],
    Mo = "show ManagedElement=1,Equipment=1",
    RE =".*FieldReplaceableUnit.*",
    %%Create MO if it doesn't exist
    case rct_cli:send(cli, Mo, {RE, Option}, noprint)  of
	{ok , RecievedData} ->	ct:log("ok ~p",[RecievedData]);
	_->ct:log("#-------- Create FieldReplaceableUnit --------#"),
	   ct:log("Reading file ~p ", [?XmlDirDw2 ++ "tcu_config.xml"]),
	    os:cmd("rcs_exec -m netconf -u SysAdminTest -p SysAdminTest -f " ++
		       ?XmlDirDw2 ++ "tcu_config.xml" ++"  " ++ NodeName)
    end,
    ok = rct_cli:disconnect(cli).

create_mo_CustomRule(Nc1)->
    rct_cli:connect(cli),
    rct_cli:send(cli,"configure"),

    Mo = "show ManagedElement=1,SystemFunctions=1,SecM=1,UserManagement=1,LocalAuthorizationMethod=1",
    RE =".*CustomRule.*",
    Option = [global, {capture, all, list}],
    Answ = rct_cli:send(cli, Mo, noprint),
    ct:log("### Answer: ~p", [Answ]),
    %%Create MO if it doesn't exist
    case rct_cli:send(cli, Mo, {RE, Option}, noprint)  of
	{ok ,RecievedData} ->	ct:log("ok ~p",[RecievedData]);
	_-> ct:log("#-------- Create CustomRule --------#"),
	    %%TODO: Check op state, if unlocked lockit
	    F = fun() ->   {ok,_} = netconf_open(nc1,[]),
			   ok =
			       ct_netconfc:edit_config(
				 Nc1,running,
				 {'ManagedElement',
				  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				  [{managedElementId,[],["1"]},
				   {'SystemFunctions',[],
				    [{systemFunctionsId,[],["1"]},
				     {'SecM',
				      [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
				      [{secMId,[],["1"]},
				       {'UserManagement',[],
					[{userManagementId,[],["1"]},
					 {'LocalAuthorizationMethod',
					  [{xmlns,"urn:com:ericsson:ecim:ComLocalAuthorization"}],
					  [{localAuthorizationMethodId,[],["1"]},
					   {'CustomRule',[],
					    [{customRuleId,[],["expert"]},
					     {ruleName,[],["expert"]},
					     {permission,[],["RWX"]},
					     {ruleData,[],["ManagedElement,*"]},
					     {userLabel,[],
					      ["Temporary solution for application lab testing on secure boards"]}
					    ]}]}]}]}]}]}),
			   ok = ct_netconfc:close_session(Nc1, 10000)
		end,
	    try F()
	    catch
		_:Reason ->
		    ct_netconfc:close_session(Nc1, 10000),
		    ct:fail(Reason)
	    end
    end,
    ok = rct_cli:disconnect(cli).

create_mo_CustomRole(Nc1)->
    rct_cli:connect(cli),
    rct_cli:send(cli,"configure"),

    Mo = "show ManagedElement=1,SystemFunctions=1,SecM=1,UserManagement=1,LocalAuthorizationMethod=1",
    RE =".*CustomRole.*",
    Option = [global, {capture, all, list}],
    %%Create MO if it doesn't exist
    case rct_cli:send(cli, Mo, {RE, Option}, print)  of
	{ok ,RecievedData} ->	ct:log("ok ~p",[RecievedData]);
	ReceivedNotExpected -> ct:log("Received data when checking if CustomRole created~n~p",[ReceivedNotExpected]),
	    ct:log("#-------- Create CustomRole --------#"),

	    F = fun() ->   {ok,_} = netconf_open(nc1,[]),
			   ok =
			       ct_netconfc:edit_config(
				 Nc1,running,
				 {'ManagedElement',
				  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				  [{managedElementId,[],["1"]},
				   {'SystemFunctions',[],
				    [{systemFunctionsId,[],["1"]},
				     {'SecM',
				      [{xmlns,"urn:com:ericsson:ecim:ComSecM"}],
				      [{secMId,[],["1"]},
				       {'UserManagement',[],
					[{userManagementId,[],["1"]},
					 {'LocalAuthorizationMethod',
					  [{xmlns,"urn:com:ericsson:ecim:ComLocalAuthorization"}],
					  [{localAuthorizationMethodId,[],["1"]},
					   {'CustomRole',[],
					    [{customRoleId,[],["1"]},
					     {roleName,[],["expertUnique"]},
					     {rules,[],
					      ["ManagedElement=1,SystemFunctions=1,SecM=1,UserManagement=1,LocalAuthorizationMethod=1,CustomRule=expert"]},
					     {userLabel,[],
					      ["Temporary solution for application lab testing on secure boards"]}
					    ]}]}]}]}]}]}),
			   ok = ct_netconfc:close_session(Nc1, 10000)
		end,
	    try F()
	    catch
		_:Reason ->
		    ct_netconfc:close_session(Nc1, 10000),
		    ct:fail(Reason)
	    end
    end,
    ok = rct_cli:disconnect(cli).

site_config_complete(_) ->
    site_config_complete(nc1, 600).
site_config_complete(NC, Timeout) ->
    ct:log("Wait for netconf rbsConfigLevel: SITE_CONFIG_COMPLETE"),
    Time = secs_since_1970(),
    site_config_complete(NC, Time, Time + Timeout).

site_config_complete(NC, Time, Timeout) when Time < Timeout ->
    case ct_netconfc:open(NC,[{timeout, 5000},{user, "SysAdminTest"}, {password, "SysAdminTest"}]) of 
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
		       {rbsConfigLevel,[],[SITE_CONFIG_COMPLETE]}]}]}]}]} =
		ct_netconfc:get(nc1,{'ManagedElement',
				     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				     [{managedElementId,[],["1"]},
				      {'NodeSupport',[],
				       [{nodeSupportId,[],["1"]},
					{'AutoProvisioning',[],
					 [{autoProvisioningId,[],["1"]}]}]}]}),
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
	Other ->
	    ct:log(yellow,"Could not connect with netconf, Retrying in 5 seconds. Reason: ~p",[Other]),
	    timer:sleep(5000),
	    site_config_complete(NC,secs_since_1970(), Timeout)
    end;
site_config_complete(_NC,_Time,Timeout) ->
    ct:pal("in fail"),
    ReturnString = "Could not connect with netconf within " ++ integer_to_list(Timeout) ++ " seconds",
    {error,ReturnString} .
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
    
clear_llog() -> 
    ct:log("### clear llog.",[]),
    rct_rs232:login(console),
    ct_telnet:cmd(console, "llog -c"),
    timer:sleep(5000),
    ct_telnet:cmd(console, "llog"),
    ok.
check_llog(RestartReason, NrOfExpectedRestarts)->

    ok = rct_coli:connect(coli),
    {ok, Reply} = rct_coli:send(coli,"diagm/llog"), 
    ok = rct_coli:disconnect(coli),

    ct:log("Check that it is ~p nr of ~p restarts",[NrOfExpectedRestarts ,RestartReason]),
    {match,NumRestartList} = re:run(Reply, RestartReason, [global,{capture,all,list}]),
    NumRestart = length(NumRestartList),
    case NumRestart =:= NrOfExpectedRestarts of
	true -> ok;
	_-> ct:fail("Expected nr of ~p restart(s) ~p , done restart(s) ~p",
		    [RestartReason, NrOfExpectedRestarts , NumRestart])
    end.

