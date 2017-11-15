%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	check_after_install_SUITE.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/6

%%% @doc ==Checks after install==
%%% This Test Suite can be used on both target and simulated enviroment.
%%% @end
-module(check_after_install_SUITE).
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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R1A/1      2012-06-28 etxkols     Created
%%% R2A/1      2012-11-26 etxkols     Changed allowed memory usage to 60 Mb
%%% R2A/2      2012-12-21 etxarnu     Changed allowed memory usage to 65 Mb
%%% R2A/3      2013-01-21 etxkols     Changed allowed memory usage to 70 Mb
%%% R2A/4      2013-01-21 etxkols     Changed allowed memory usage to 75 Mb
%%% R2A/5      2013-03-15 etxkols     Changed allowed memory usage to 85 Mb
%%% R2A/6      2013-03-20 etxkols     Changed allowed cpu and core to 50 %
%%% R2A/7      2013-04-10 etxkols     Added rct_core hook
%%% R2A/8-9    2013-04-10 etxkols     Added testcases and check for node_ci
%%% R2A/10     2013-05-03 etxkols     Changed allowed memory usage to 100 Mb
%%% R2A/11     2013-05-30 etxkols     node_ci_dc_dus will be replace by node_ci_dc_rbs
%%% R2A/12     2013-06-04 etxjovp     add node_ci_hc_rbs and node_ci_hc_tcu03
%%% R2A/13     2013-06-05 etxjovp     fix bug in suite()
%%% R2A/14     2013-06-05 etxjovp     Changed allowed memory usage to 300 Mb for rbs
%%% R2A/15     2013-06-25 etxjovp     Changed allowed memory usage to 150 Mb
%%% R2A/16     2013-10-08 etxkols     Changed CPU load to 75% to handle lrat
%%% R2A/17     2013-10-16 etxkols     Accept "PCIE0: Error interrupt 0x20000"
%%% R2A/18     2013-10-30 etxkols     Increased max mem 350 and wait for lrat
%%% R2A/20     2013-12-03 etxkols     Filter "Correctable ECC Error!"
%%% R2A/21     2013-12-03 etxkols     One more try, timebomb format wrong
%%% R2A/22     2014-01-31 etxkols     Increased sleep from 210 sec to 240 for RBS,TCU
%%% R2A/23     2014-02-04 etxkols     TCU mem usage increase to 200 Mb
%%% R2A/24     2014-02-04 etxkols     Increase wait after install to 180 sec
%%% R2A/25     2014-02-21 etxkols     Increase wait after install to 360 sec for node UP
%%% R2A/14     2013-06-05 etxjovp     Changed allowed memory usage to 450 Mb for rbs
%%% R2A/27     2014-03-06 etxkols     Temporary removed CPU check because of CIM loop
%%% R2A/28     2014-03-07 etxkols     Reinserted CPU check
%%% R2A/29     2014-03-20 etxkols     Changed allowed memory usage to 500 Mb for rbs
%%% R2A/30     2014-03-25 etxkols     Changed allowed memory usage to 300 Mb for tcu
%%% R2A/31     2014-04-02 etxkols     Cleanup and filter away temporary error for dus52
%%% R2A/32     2014-04-02 etxkols     Change ct:pal to ct:log
%%% R2A/33     2014-06-03 etxkols     Fixes for EE TCU03
%%% R2A/34     2014-06-16 eransbn     cpumemory increase to 220 Mb for dus, 600 Mb for ci
%%% R2A/35     2014-06-17 etxarnu     cpumemory increase to 700 Mb for ci
%%% R2A/36     2014-06-18 eransbn     cpumemory increase to 700 Mb for ci
%%%                                   because of FAKE lsv 17318
%%% R2A/36     2014-06-25 etxivri     Increased timeout for check logs and search for ERRORs
%%% R2A/39     2014-07-04 etxkols     Added dus3201
%%% R2A/39     2014-08-05 etxkols     Increased sleep to 210 seconds for CS
%%% R2A/40     2014-08-12 etxkols     Increased sleep to 240 seconds for CS
%%% R3A/1      2014-09-03 etxkols     Added console logging
%%% R3A/2      2014-09-22 etxkols     Timebomb for dus32
%%% R3A/3      2014-10-13 etxkols     Increased memory to 300 Mb
%%% R3A/4      2014-11-10 etxkols     Increased memory to 350 Mb
%%% R3A/5      2014-11-11 etxkols     Increased memory to 400 Mb
%%% R3A/6      2014-11-11 etxkols     Aligned TCU memory usage to 350 Mb
%%% R3A/7      2014-11-13 etxkols     Increased allowed DUS memory usage to 750 Mb
%%% R3A/8      2014-11-26 eransbn     support for secure card
%%% R3A/9      2014-11-26 etxkols     Poll for board up instead of sleep
%%% R3A/10     2014-11-26 etxkols     Fix
%%% R3A/11     2014-11-26 etxkols     Compiler warning
%%% R3A/12     2014-11-26 etxkols     Increase netconf poll time to 10 minutes
%%% R3A/13     2014-12-16 eransbn     Update for VC card
%%% R3A/14     2014-12-16 etxkols     Increased allowed DUS memory usage to 850 Mb
%%%                                   Added sleep 30 sec in logs_and_cpu to allow CPU load to go down
%%% R3A/15     2014-12-18 etxkols     Temporary increase allowed CPU load to 100% to avoid rhsd failures
%%% R3A/16     2015-04-07 etxkols     Added tcu0401
%%% R3A/18     2015-04-24 etxkols     Give it one more try when netconf connection is torn down during start up
%%% R3A/19     2015-04-30 etxkols     Filter out "/dev/sda2 contains a file system with errors, check forced"
%%% R3A/20     2015-05-05 etxkols     Changed CPU load to 75%
%%%                                   Added cth_conn_log to secure boards
%%% R3A/21     2015-06-01 etxkols     Prolonged timebomb
%%% R3A/22     2015-06-17 eransbn     Increased allowed DUS memory usage to 900 Mb and tcu to 400
%%% R4A/1      2015-06-22 etxarnu     wait_for_rbsConfigLevel now handles unset rbsConfigLevel
%%% R4A/2      2015-06-22 eransbn     Increased allowed DUS memory usage to 10000
%%% R4A/3      2015-07-02 eransbn     Filter out "lsi-ncr: previous failed, error status : 0x00000000 0x00000000"
%%% R4A/4      2015-08-10 etxkols     Increased sleep after SITE_CONFIG_COMPLETE from 30 to 60 sec to let CPU load decrease
%%% R4A/5      2015-08-11 etxkols     Decreased allowed DUS memory usage to 1000
%%% R4A/6      2015-09-01 etxkols     Removed timebomb
%%% R4A/8      2015-09-10 etxkols     Debug new reply to AutoProvisioning
%%% R4A/10     2015-09-21 etxkols     New strange reply from ct_netconf
%%% R4A/11     2015-09-29 etxkols     Cluster installation
%%% R4A/12     2015-09-29 etxkols     Cluster installation
%%% R4A/13     2015-10-02 eransbn     Accept printout error: can't find an ext2 filesystem on dev pmem0
%%% R4A/14     2015-10-05 etxkols     now/0 depricated, replaced with os:timestamp/0
%%% R4A/15     2015-10-06 etxkols     Increased memory to 450 Mb
%%% R4A/16     2015-11-09 etxkols     Filter new errors
%%% R5A/1      2015-11-11 etxarnu     Changed wait_for_frum2
%%% R5A/2      2015-12-08 etxkols     Merge with R4A
%%% R5A/3      2016-01-19 etxkols     Increased allowed DUS memory usage to 1200 Mb
%%% R5A/4      2016-01-25 etxnnor     Added test in check_vendorCredential() to check if VC MO is correct (TRs HU46854, HU50353)
%%% R5A/5      2016-01-27 etxkols     Increased allowed DUS memory usage to 1300 Mb
%%% R5A/6      2016-01-27 etxkols     Forgot the actual increase
%%% R5A/7      2016-03-23 etxkols     5g
%%% R5A/8      2016-04-25 etxkols     Change ccs to rcf
%%% R5A/9      2016-05-03 etxivri     Add boardtype idu5205
%%% R5A/10     2016-05-03 etxivri     Add boardtype idu5209
%%% R5A/11     2016-05-26 etxkols     New error printout in dmesg for WR8
%%% R5A/12     2016-05-26 etxkols     More filters for WR8
%%% R6A/1      2016-05-30 eransbn     Check idl link instead of FieldReplaceableUnit for dual
%%% R6A/2      2016-07-07 etxkols     Bug when introducing WR8
%%% R6A/3      2016-06-08 etxivri     Add dus5301, dus3301
%%% R6A/4      2016-09-28 etxkols     EE wants new filter due to wr8
%%% R7A/0      2016-11-03 etxkols     dus6303
%%% R7A/2      2016-12-01 etxkols     New filter
%%% R8A/1      2017-01-11 etxkols     Using installed_type to determine max memsize
%%% R8A/2      2017-01-13 etxkols     Using installed_type to determine max memsize
%%% R9A/2      2017-05-17 erarube     Increase of CpuMemory check from 1000 to 1050 for LRAT, WRAT and GRAT due to dus53 on the limit.
%%% R9A/3      2017-06-13 etxkols     Allow "Failed to open archive: /dev/rootvg/RCS-T_CXP9031274_5" in dmesg.
%%% R9A/4      2017-06-13 etxkols     Try again
%%% R9A/5      2017-06-13 etxkols     Try again 2
%%% R9A/6      2017-06-13 etxkols     Try again 3
%%% ------------------------------------------------------
% -compile([export_all]). edoc is not generated when -compile([export_all]).
-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
         logs_and_cpu/1,
	 check_installation/1,
	 wait_for_frum2/1
	]).

%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_logging, rct_tlib
%% @end
%%--------------------------------------------------------------------
suite() ->
    CpuMemory = case ct:get_config({jenkins_config,installed_type}) of
		    no   -> 300;
		    lrat -> 1050;
		    wrat -> 1050;
		    grat -> 1050;
		    rat  -> 1000;
		    tcu  -> 1000;
		    _ ->	
			ct:pal("NO {jenkins_config,installed_type} using node_type"),
			case ct:get_config(node_type) of
			    undefined        -> 450;
			    node_ci_dc_dus   -> 1300; % Remove when node CI changes curl call
			    node_ci_dc_rbs   -> 1300;
			    node_ci_hc_rbs   -> 1300;
			    node_ci_dc_tcu03 -> 400;
			    node_ci_hc_tcu03 -> 400
		      end
	end,
    case  check_if_vc_board() of
	"yes"  ->
	    [{ct_hooks, [{rct_htmllink,[]},
			 {rct_netconf, {nc1, [man_auth, pretty]}},
			 {rct_cli, {cli1, [{user, "SysAdminTest"}, {password, "SysAdminTest"},manual_connect]}},
			 {cth_conn_log,[]}
			]}];
	_ ->
	    N = length(ct:get_config(test_nodes)),
	    Hooks =[{rct_consserv,[list_to_atom("cs" ++ integer_to_list(X))||X<-lists:seq(1,N)]},
		    {rct_rs232,   [list_to_atom("console" ++ integer_to_list(X))||X<-lists:seq(1,N)]},
		    {rct_logging, [{all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}||_X<-lists:seq(1,N)]},
		    {rct_rpc, rpc},
		    {rct_core,    [[]||_X<-lists:seq(1,N)]},
		    {rct_tlib,    [{list_to_atom("tlib" ++ integer_to_list(X)),[{cpumemory, CpuMemory},{cpuload,75}]}||X<-lists:seq(1,N)]}],
	    [{ct_hooks, [{rct_htmllink,[]}] ++
		         Hooks ++
			[{rct_netconf, {nc1, pretty}},
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
end_per_testcase(_TestCase, _Config) ->
    ok.
%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() ->
    [logs_and_cpu, check_installation].

%%--------------------------------------------------------------------
%% @doc
%% Wait 2 minutes, get all logs and search for ERROR and CRASH reports and verify CPU load and memory.<br/><br/>
%% @end
%%--------------------------------------------------------------------
logs_and_cpu(_) ->
    ct_telnet:send(console2,"reboot -f"),
    N = length(ct:get_config(test_nodes)),
    [rct_logging:get_all(list_to_atom("log"++integer_to_list(X)))||X<-lists:seq(1,N)],
    case wait_for_rbsConfigLevel(nc1,"SITE_CONFIG_COMPLETE" , 600) of
	ok ->
	    ct:log("Sleeping ~p seconds to allow processor load to go down",[Sleep = 60]),
	    timer:sleep(Sleep * 1000),
	    %%% Debug for Lava START
	    case ct_telnet:cmd(console1,"ps -eo fname,rss,vsz | grep -v \"0      0\"") of
		{ok,[_,Header|R]} ->
		    [_|Reversed] = lists:reverse(R),
		    Data = [Header] ++ lists:sort(Reversed),
		    ct:log("Debug for Lava to investigate increasing memory~n~s",[lists:flatten([X++"\n" ||X<-Data])]);
		_ ->
		    do_nothing
	    end,
	    case N of
		1 ->
		    do_nothing;
		_ -> % Cluster
		    %%ok = wait_for_frum2(nc1, 900)
		    ok = wait_for_ri_link()
	    end;
	
	    %%% Debug for Lava END
	_ ->
	    ct:fail("Netconf poll for SITE_CONFIG_COMPLETE failed")
    end.
%%--------------------------------------------------------------------
%% @doc
%% Searches /rcs/bootlogs/netloader.log and dmesg for faults.
%% @end
%%--------------------------------------------------------------------
check_installation(_Config) ->
    case os:getenv("SIM_OR_TARGET") of
	"cloudish" ->
	    ct:pal("check_installation not run for Cloud");
	_ ->
	    case check_if_vc_board()  of
		"yes" ->
		    %% site_config_complete([]),
		    check_vendorCredential();
		_ ->
		    ok = rct_rs232:login(console1),
		    ct_telnet:cmd(console1,"lsmod"),
		    IgnoreMess = case is_wr6() of
				     true ->
					 ["checksum error in super block","PCIE0: Error interrupt 0x20000",
					  "error: can't find an ext2 filesystem on dev pmem0", "ECC Error", "errors:","PCIE0: Error interrupt 0x20000","/dev/sda2 contains a file system with errors, check forced",
					  "lsi-ncr: previous failed, error status : 0x00000000 0x00000000",
					  "arm-ccn 2000000000\.ccn: Error reported in 000000000000000000000000000000000000000000000100",
					  "arm-ccn 2000000000\.ccn: Disabling interrupt generation for all errors",
					  "EXT2-fs \\(pmem0p1\\): error",
					  "/dev/sda2 contains a file system with errors, check forced"
					 ];
				     false ->
					 ["checksum error in super block","PCIE0: Error interrupt 0x20000",
					  "Failed to open archive: /dev/rootvg/",
					  "arm-ccn 2000000000\.ccn: Error reported in 000000000000000000000000000000000000000000000100",
					  "arm-ccn 2000000000\.ccn: Disabling interrupt generation for all errors",
					  "/dev/sda2 contains a file system with errors, check forced"]
				 end,
						%	    {Expire,Timebomb} = {{2015,06,01},["ncp: Config Ring Error Stat 0 =","ncp: PIO Timeout Error : Node"]},
		    %% {Expire,Timebomb} = {{2015,09,01},["arm-ccn 2000000000\.ccn: Error reported in 000000000000000000000000000000000000000000000100",
		    %% 				       "arm-ccn 2000000000\.ccn: Disabling interrupt generation for all errors"]},
		    {Expire,Timebomb} = {{2020,09,01},["Dummy string for timebomb"]},
		    ErrorsAndFatals = ['ERROR', 'Error', 'error', 'FATAL', 'Fatal', 'fatal'],
		    ct_telnet:cmd(console1,"cat /rcs/bootlogs/netloader.log"),
		    AllNetLoaderErrors = lists:foldl(fun(Key,Result) ->
							     Result ++ run_cmd("grep "++ atom_to_list(Key) ++ " /rcs/bootlogs/netloader.log")
						     end,
						     [], ErrorsAndFatals),
		    NetLoader = lists:foldl(fun(Key,Res) ->
						    [S || S <- Res, re:run(S, Key) == nomatch]
					    end,
					    AllNetLoaderErrors, IgnoreMess),
		    ct_telnet:cmd(console1,"dmesg"),
		    AllDmesgErrors = lists:foldl(fun(Key,Result) ->
							 Result ++ run_cmd("dmesg | grep "++ atom_to_list(Key))
						 end,
						 [], ErrorsAndFatals),
		    Dmesg = lists:foldl(fun(Key,Res) ->
						[S || S <- Res, re:run(S, Key) == nomatch]
					end,
					AllDmesgErrors, IgnoreMess),
		    
%%% Temporary Timebomb
		    {Days, _} = calendar:time_difference(calendar:local_time(),{Expire,{0,0,0}}),
		    Dmesg2 = case Days >= 0 of
				 true ->
				     case [Y||X<-Dmesg,Y<-Timebomb,re:run(X,Y) =/= nomatch] of
					 [] ->
					     ct:log(yellow,"Warning Timebomb on dmesg for ~p seems to be fixed, contact etxkols",[Timebomb]),
					     Dmesg;
					 _ ->
					     ct:log(yellow,"Warning Timebomb on dmesg for ~p will expire ~p. Intel to fix, contact etxkols or edavnys",[Timebomb,Expire]),
					     lists:foldl(fun(Key,Res) ->
								 [S || S <- Res, re:run(S, Key) == nomatch]
							 end,
							 Dmesg, Timebomb)
				     end;
				 
				 false ->
				     ct:log(yellow,"Warning Timebomb on dmesg for ~p expired ~p. Intel to fix, contact etxkols or edavnys",[Timebomb,Expire]),
				     Dmesg
			     end,
%%% End Temporary Timebomb
		    SumError = NetLoader ++ Dmesg2,
		    case SumError of
			[] -> ok;
			_ -> ct:fail("Errors found in installation logs: \n ~p", [SumError])
		    end
	    end
    end.


run_cmd(Cmd) ->
    {ok, [_|Data]} = ct_telnet:cmd(console1, Cmd), % strip head
    [_|Data2] = lists:reverse(Data), % strip last element
    lists:reverse(Data2).

wait_for_rbsConfigLevel(NC, RbsConfigLevel, Timeout) ->
    ct:log("Wait for netconf rbsConfigLevel " ++ RbsConfigLevel ++ " using: ~p",[ct:get_config(NC)]),
    Time = secs_since_1970(),
    wait_for_rbsConfigLevel(NC, RbsConfigLevel, Time, Time, Time + Timeout).

wait_for_rbsConfigLevel(NC, RbsConfigLevel, StartTime, Time, Expire) when Time < Expire ->
    Open = case check_if_vc_board() of
	       "yes" ->
		   ct_netconfc:open(NC,[{timeout, 5000},{user, "SysAdminTest"}, {password, "SysAdminTest"}]);
	       _ ->
		   ct_netconfc:open(NC,[{timeout, 5000}])
	   end,
    case Open of
	{ok,_} ->
	    case os:getenv("SIM_OR_TARGET") of
		"cloudish" ->
		    ct:pal("AutoProvisioning not set for Cloud~nNetconf open is good enough for considering board up"),
		    ct_netconfc:close_session(NC),
		    ok;
		_ ->
		    case ct_netconfc:get(NC,{'ManagedElement',
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
			{error,{closed,_}} -> % During startup, netconf sesssion may be closed, give it one more retry.		    
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
				   {rbsConfigLevel,[],[RBSCONFIGLEVEL]}]}]}]}]}	->
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
		    end
	    end;
	Other ->
	    ct:log(yellow,"Could not connect with netconf, Retrying in 5 seconds. Reason: ~p",[Other]),
	    timer:sleep(5000),
	    wait_for_rbsConfigLevel(NC, RbsConfigLevel, StartTime, secs_since_1970(), Expire)
    end;
wait_for_rbsConfigLevel(_NC, RbsConfigLevel, StartTime, Time, _Expire) ->
    ct:log(lightred,"Could not connect with netconf or set rbsConfigLevel ~s within ~p seconds",[RbsConfigLevel, Time - StartTime]),
    {error, netconf_timeout}.

wait_for_ri_link() ->
    wait_for_ri_link(rpc,60,[]).
wait_for_ri_link(_Rpc,0, Cause) ->
    ct:fail("idl Link not up  cause:~n~p",[Cause]);

wait_for_ri_link(Rpc, TimeOut, _Cause)  ->

    case  rct_rpc:call(Rpc,clhI,op_state,[all],10000) of
	[enabled,enabled] -> ok;

	Reply->	ct:log("Idl Link not up reason ~p",[Reply]),
		timer:sleep(10000),	
	        wait_for_ri_link(Rpc, TimeOut -1, Reply)
    end.
wait_for_frum2(_) ->
    wait_for_frum2(nc1,600).

wait_for_frum2(NC, Timeout) ->
    ct:log("Wait for netconf frum2"),
    Time = secs_since_1970(),
    wait_for_frum2(NC, Time, Time, Time + Timeout).

wait_for_frum2(NC, StartTime, Time, Expire) when Time < Expire ->
    Open = case check_if_vc_board() of
	       "yes" ->
		   ct_netconfc:open(NC,[{timeout, 15000},{user, "SysAdminTest"}, {password, "SysAdminTest"}]);
	       _ ->
		   ct_netconfc:open(NC,[{timeout, 15000}])
	   end,
    case Open of
	{ok,_} ->
	    case ct_netconfc:get(NC,{'ManagedElement',
				     [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				     [{managedElementId,[],["1"]},
				      {'Equipment',[],
				       [{equipmentId,[],["1"]},
					{'FieldReplaceableUnit',[],
					 [{fieldReplaceableUnitId,[],["2"]},
					  {operationalState,[]}]}]}]}) of
		{error,ERROR} -> % During startup, netconf sesssion may be disconnected, give it one more retry.		    
		    ct_netconfc:close_session(NC),
		    case get(netconf_disconnected_givit_1_more_try) of
			undefined ->
			    ct:log("netconf get failed after ~p seconds, Reason: ~p giving it one more retry",[Time - StartTime, ERROR]),
			    put(netconf_disconnected_givit_1_more_try,no),
			    timer:sleep(15000),
			    wait_for_frum2(NC, StartTime, Time, Expire);
			no ->
			    ct:log(lightred,"netconf get failed after ~p seconds, Reason: ~p",[Time - StartTime, ERROR]),
			    {error, netconf_timeout}
		    end;
		{ok,[{'ManagedElement',
		      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		      [{managedElementId,[],["1"]},
		       {'Equipment',
			[{xmlns,"urn:com:ericsson:ecim:ReqEquipment"}],
			[{equipmentId,[],["1"]},
			 {'FieldReplaceableUnit',
			  [{xmlns,"urn:com:ericsson:ecim:ReqFieldReplaceableUnit"}],
			  [{fieldReplaceableUnitId,[],["2"]},
			   {operationalState,[],[OperationalState]}]}]}]}]}->
		    case OperationalState of
			"ENABLED" ->
			    ct:log("FieldReplaceableUnitId=2,operationalState=ENABLED after ~p seconds", [secs_since_1970() - StartTime]),
			    ct_netconfc:close_session(NC);
			_ ->
			    ct:log(yellow,"FieldReplaceableUnitId=2,operationalState=~s, Retrying in 15 seconds",[OperationalState]),
			    ct_netconfc:close_session(NC),
			    timer:sleep(15000),
			    wait_for_frum2(NC, StartTime, secs_since_1970(), Expire)
		    end;
		NewReply ->
		    ct:log(yellow,"~p, Retrying in 15 seconds",[NewReply]),
		    ct_netconfc:close_session(NC),
		    timer:sleep(15000),
		    wait_for_frum2(NC, StartTime, secs_since_1970(), Expire)		    
		    %% ct:log("Unexpected reply to fieldReplaceableUnitId ~p, contact etxkols",[NewReply]),
		    %% ct:fail("Unexpected reply to fieldReplaceableUnitId")
	    end;
	Other ->
	    ct:log(yellow,"Wait for fieldReplaceableUnitId=2,operationalState~n Could not connect with netconf, Retrying in 15 seconds. Reason: ~p",[Other]),
	    timer:sleep(15000),
	    wait_for_frum2(NC, StartTime, secs_since_1970(), Expire)
    end;
wait_for_frum2(_NC, StartTime, Time, _Expire) ->
    ct:log(lightred,"Could not connect with netconf or wrong fieldReplaceableUnitId within ~p seconds",[Time - StartTime]),
    {error, netconf_timeout}.

secs_since_1970() ->
    {MSec,Sec,_} = os:timestamp(),
    (MSec * 1000000) + Sec.
check_vendorCredential() ->
    Mo = "show ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,VendorCredential=1",
    RE =".*VendorCredential=1.*",
    Option = [global, {capture, all, list}],
    ok = rct_cli:connect(cli1),
    rct_cli:send(cli1,"configure"),
    case  rct_cli:send(cli1, Mo, {RE, Option}, noprint) of	
	{ok ,RecievedData} ->	ct:log("ok ~p",[RecievedData]);
	{error ,RecievedData} -> ct:fail("No VendorCredential MO: ~p",[RecievedData])
    end,
    %% Additional check due to TRs HU46854, HU50353:
    {ok, Reply} = rct_cli:send(cli1, Mo),
    case re:run(Reply, "emailAddress=andreas.toyra@ericsson.com") of
	{match, _} -> ct:fail("Vendor Credential MO is wrong for a secure board!");
	nomatch -> ct:log("Vendor Credential MO verified ok")
    end,
    ok = rct_cli:disconnect(cli1).
check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.

is_wr6() -> % Check if WR6 or WR8
    case ct_telnet:cmd(console1,"uname -r") of
	{ok,[_,WrRelease,_]} ->
	    case string:str(WrRelease,"wrlp6") of
		0 ->
		    ct:pal("Windriver is NOT version 6"),
		    false;
		_ -> 
		    ct:pal("Windriver version 6"),
		    true
	    end;
	Other -> 
	    ct:pal("Cannot determine if board is running WR6 or WR8, Reason ~p~nDefaulting to WR6",[Other]),
		 true
    end.
%	{ok, [_|Data]} = ct_telnet:cmd(console1, Cmd), % strip head
