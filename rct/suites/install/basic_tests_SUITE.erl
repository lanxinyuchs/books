%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	basic_tests_SUITE.erl %
%%% @author erarube
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R7A/R8A/R9A/2
-module(basic_tests_SUITE).
-id('Updated by CCase').
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R7A/R8A/R9A/2').
-date('2017-05-17').
-author('erarube').
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
%%% R1A/1      2012-06-11 etxkols     Created
%%% R1A/2      2012-06-15 etxkols     Added load and mem check
%%%                                   Temporary filtered out Linx faults
%%% R1A/3      2012-06-19 etxkols     Added html link in end_per_suite
%%% R1A/4      2012-06-19 etxkols     Changed ct:log to ct:print
%%% R1A/5      2012-06-20 etxkols     Trying to fix html link
%%% R1A/6      2012-06-20 etxkols     Trying to fix html link
%%% R1A/9      2012-06-25 etxkols     Linx fixed and html link
%%% R1A/10     2012-06-25 etxkols     Stupid , bug
%%% R1A/11     2012-06-26 etxkols     html_link as hook
%%%                                   Allowed CPU load 10%
%%% R1A/12     2012-06-27 etxkols     Removed Linx fault filter
%%% R1A/13     2012-06-28 etxkols     Disconn/conn erlnode during power
%%%                                   and reboot
%%% R1A/14     2012-06-28 etxkols     Edited ct:fail
%%% R1A/15     2012-07-05 etxkols     Updated for new ct_netconfc
%%% R1A/16     2012-08-16 etxkols     Added tc nc_getset_config
%%% R1A/17     2012-08-16 etxkols     Changed nc tc:s so close_session is always run
%%% R1A/18     2012-09-03 etxkols     New ift_app added.
%%% R1A/18     2012-09-03 etxkols     Changed max allowed CPU load to 15%.
%%% R1A/19     2012-09-21 etxkols     Removed check_logs_after_install from all/0
%%%                                   Added linux proc testcases and edoc
%%% R1A/20     2012-10-03 etxkols     Removed duplicated testcases
%%% R1A/21     2012-10-19 etxkols     Added testcases cli and coli
%%% R2A/2      2012-10-23 etxkols     Backed out cli and coli
%%% R2A/3      2012-10-24 etxkols     Added testcases cli and coli
%%% R2A/4      2012-10-25 etxkols     Added testcases test_root_mo and test_class1_all
%%% R2A/5      2012-10-25 etxkols     Fixed xted, colid and linx-ns check for sim env
%%% R2A/6      2012-10-26 etxkols     Changed allowed memory from 50 to 60 Mb
%%% R2A/7      2012-11-13 etxkols     Added kill_com tc, removed -f at reboot
%%% R2A/9      2012-11-28 etxkols     Reverted to reboot -f because of problems
%%% R2A/10     2012-11-29 etxkols     Back to reboot after bug fix
%%% R2A/11     2012-11-30 etxkols     Added TC check_ntpd and init_restart
%%% R2A/12     2012-12-21 etxarnu     Added handling of test_oi app
%%%                                   Changed allowed memory from 60 to 65 Mb
%%% R2A/13     2013-01-08 etxkols     Fixed minor == bug
%%% R2A/14     2013-01-17 etxkols     Changed allowed memory from 65 to 70 Mb
%%% R2A/15     2013-01-21 etxkols     Changed allowed memory from 70 to 75 Mb
%%% R2A/16     2013-02-28 etxkols     Added rct_core hook
%%% R2A/17     2013-03-14 etxjovp     Changed allowed memory from 75 to 85 Mb
%%% R2A/18     2013-03-18 etxkols     Increased allowed cpu load from 15 to 20 %
%%% R2A/19     2013-03-18 etxkols     Increased allowed cpu load 20 to 50 %
%%% R2A/20     2013-03-18 etxkols     Increased allowed cpu load 50 to 100 %
%%% R2A/21     2013-04-15 etxkols     Changed allowed memory from 85 to 100 Mb
%%% R2A/22     2013-06-04 etxjovp     Add CpuMemory
%%% R2A/25     2013-06-05 etxjovp     Changed allowed memory usage to 300 Mb for rbs
%%% R2A/26     2013-06-10 etxkols     Added SNMP testcases
%%% R2A/27     2013-06-25 etxjovp     Changed allowed memory usage to 150 Mb
%%% R2A/28     2013-07-03 etxjovp     Changed linx_ns to ns in check_linx_ns  
%%% R2A/30     2013-07-18 etxjotj     Possible telnet buffer problem in xted fixed
%%% R2A/31     2013-08-13 etxkols     Changed raw to no_prompt_check in ct_telnet:expect
%%% R2A/32     2013-08-20 etxarnu     pgrep for '-ns' instead of 'ns'
%%% R2A/33     2013-08-20 etxarnu     pgrep for 'linx-ns' instead of 'ns'
%%% R2A/34     2013-09-04 etxkols     Changed coli command
%%% R2A/35     2013-09-09 etxkols     Speed up test by polling nc after reboot
%%%                                   and check EE procs in one TC
%%% R2A/36     2013-09-09 etxkols     Removed compiler warnings
%%% R2A/37     2013-09-11 etxkols     linx-ns removed
%%% R2A/38     2013-10-02 etxkols     fix for ARM
%%% R2A/39     2013-10-02 etxkols     Temp fix for memcheck ARM
%%% R2A/40     2013-10-08 etxkols     Temp fix for memcheck ARM removed
%%% R2A/41     2013-11-14 etxkols     Two pghd procs
%%% R2A/42     2013-11-25 etxkols     Removed testcases releated to FEQM
%%% R2A/43     2013-12-10 etxkols     Prolonged timer for rpc:call for snmptrap
%%% R2A/44     2013-12-16 etxkols     Changed SWM MIM name
%%% R2A/45     2014-01-10 etxkols     Increase init:restart() rpc timeout to 10 sec
%%% R2A/46     2014-01-15 etxkols     rpc:call timeout to 10 sec
%%% R2A/47     2014-01-15 etxkols     Fix for dus52 in ee_processes
%%% R2A/48     2014-01-22 etxkols     Match edit_config in nc_getset_config
%%% R2A/49     2014-01-30 etxkols     Removed bnld and rlld procs from TCU
%%% R2A/50     2014-01-31 etxkols     Increase netconf poll after restart from 2 to 3 min
%%% R2A/51     2014-02-04 etxkols     TCU mem usage increase to 200 Mb
%%% R2A/52     2014-02-04 etxarnu     Bug fix for sim in mem size check
%%% R2A/53     2014-02-04 etxkols     "sim" instead of sim
%%% R2A/54     2014-02-06 etxkols     Increase netconf poll after restart from 2 to 3 min
%%% R2A/55     2014-02-24 etxkols     LRAT mem usage increase to 400 Mb
%%% R2A/55     2014-02-24 etxkols     mem usage increase to 220 Mb
%%% R2A/59     2014-03-05 etxjovp     add new groups
%%% R2A/60     2014-03-06 etxarnu     exported groups/0
%%% R2A/61     2014-03-11 etxkols     Increased snmp informRetryCount to 6
%%% R2A/62     2014-03-17 etxkols     Wait for eriAlarmAlarmListRebuilt after restart
%%% R2A/63     2014-03-24 etxkols     Edited printout
%%% R2A/64     2014-03-25 etxkols     Changed allowed memory usage to 300 Mb for tcu
%%% R2A/65     2014-03-26 etxkols     LRAT mem usage increase to 450 Mb
%%% R2A/66     2014-03-28 etxkols     Changed fallback timer order in get_fallbackTimer
%%% R2A/67     2014-03-28 etxkols     New try to fix /66
%%% R2A/68     2014-04-24 etxkols     Allowed CPU load 90%
%%% R2A/69     2014-04-24 etxkols     Allowed CPU load 100%
%%% R2A/70     2014-05-06 etxkols     Changed trap testing trap to ResourceMonitorCoffeeBeansContainerLowLevel
%%% R2A/71     2014-05-19 etxarnu     xted removed
%%% R2A/72     2014-06-03 etxkols     Fixes for EE TCU03 
%%% R2A/73     2014-06-10 etxjovp     Add test case reinstall 
%%% R2A/74     2014-06-16 etxarnu     cpumemory increase to 220 Mb for dus, 600 Mb for ci
%%% R2A/75     2014-06-17 etxarnu     cpumemory increase to 700 Mb for ci
%%% R2A/77     2014-06-18 eransbn     cpumemory increase to 700 Mb for ci
%%%                                   because of FAKE lsv 17318
%%% R2A/78     2014-07-08 etxkols     Fixes for dus32
%%% R2A/79     2014-07-14 etxkols     Increased timeout waiting for eriAlarmAlarmListRebuilt to 240 seconds
%%% R3A/1      2014-10-09 etxkols     Increased timeout waiting for traps to 60 sec
%%% R3A/2      2014-11-11 etxkols     Changed allowed memory usage to 350 Mb for tcu
%%% R3A/3      2014-11-13 etxkols     Changed allowed memory usage to 400 Mb for tcu and 750 for DUS
%%% R3A/4      2014-11-13 etxkols     Changed allowed memory usage to 300 Mb for tcu CXS
%%% R3A/5      2014-11-14 etxarnu     Change 'pgrep com' to 'pgrep -f com.cfg' 
%%% R3A/6      2014-11-19 etxkols     Remove ee_processes test from sim tests 
%%% R3A/7      2014-12-02 etxkols     Login prompt can be corrupted for dusx2
%%% R3A/9      2014-12-16 etxkols     Corrupted login prompt for tcu
%%% R3A/10     2014-12-17 etxkols     Changed allowed memory usage to 850 for DUS
%%% R3A/13     2014-01-19 eransbn     Added tc ssh_fail_sec_card and coli_authlevel
%%% R3A/14     2015-01-29 etxkols     Temporarily commented out nc_getset_config because of write-once object
%%% R3A/16     2015-03-25 etxkols     Increase memory usage to 900 Mb for RATs
%%% R3A/17     2015-03-27 erarafo     conf_snmpmgr: logs the manager ip:port to be used
%%% R3A/18     2015-04-22 etxkols     Added tcu04
%%% R3A/20     2015-04-23 etxkols     Fixed one more tcu04 issue
%%% R3A/21     2015-04-24 etxkols     Fixed one more tcu04 issue
%%% R4A/1      2015-05-15 erarafo     Ugly workaround for SNMP problem on simulator
%%% R4A/2      2015-05-20 erarafo     Ugly workaround removed
%%% R4A/3      2015-06-29 etxmlar     When power_cycle allow eriAlarmHeartBeatNotif trap 
%%%                                   when waiting for eriAlarmAlarmListRebuilt trap
%%% R4A/4      2015-06-30 etxmlar     When power_cycle allow eriAlarmWarning trap 
%%%                                   when waiting for eriAlarmAlarmListRebuilt trap  
%%% R4A/5      2015-06-30 etxmlar     Increased timeout waiting for eriAlarmAlarmListRebuilt 
%%%                                   traps to 300 sec. To make TC more stable
%%% R4A/6      2015-07-02 etxarnu     Added {timeout,10000} to netconf_open
%%% R4A/7      2015-07-02 etxmlar     When power_cycle allow eriAlarmCritical trap 
%%%                                   when waiting for eriAlarmAlarmListRebuilt trap  
%%% R4A/8      2015-07-10 etxjovp     Add group definitions used by CS CI
%%% R4A/9      2015-07-15 etxjovp     modify group definitions used by CS CI
%%% R4A/15     2015-09-17 etxivri     Update nc_get not using ct:netconf due to
%%%                                   it takes to long time to rcv all reply.
%%% R4A/16     2015-09-22 etxkols     Fix nc_get for sim
%%% R4A/17     2015-09-22 etxkols     Increase memory usage to 1050 Mb for RATs
%%% R4A/18     2015-09-22 etxkols     Revert nc_get to use orginal test using
%%%                                   ct_netconf.
%%% R4A/19     2015-10-05 etxmlar     Changed allowed memory usage to 350 Mb for dus/tcu
%%% R4A/21     2015-10-28 etxarnu     Removed init_restart test cases
%%% R4A/22     2015-11-18 etxkols     get_all/1 in power tc
%%% R4A/23     2016-02-04 etxkols     Increase memory usage to 1150 Mb for RATs
%%% R4A/24     2016-02-04 etxkols     Added allowed traps eriAlarmMinor.
%%% R4A/25     2016-02-08 etxkols     Added allowed traps eriAlarmMajor.
%%% R5A/1      2016-02-17 etxarnu     Wait for eriAlarmHeartBeatNotif instead of
%%%                                   eriAlarmAlarmListRebuilt in wait_for_node_up
%%% R5A/2      2016-03-13 erarafo     More delay after config trap sender, needed?
%%% R5A/3      2016-03-13 erarafo     No, didn't help, restoring old delay value
%%% R5A/4      2016-04-01 etxkols     discard ee_processes for cloud
%%% R5A/5      2016-04-25 etxkols     Change ccs to rcf
%%% R5A/6      2016-05-03 etxivri     Update for idu5205 and idu5209
%%% R5A/8      2016-06-09 etxivri     Update for dus5301, dus3301
%%% R5A/9      2016-08-19 etxkols     Allowed mem usage for dus5301 & dus3301 set to 400 Mb
%%% R5A/10     2016-08-19 etxkols     Git migration requires that CC paths is not used 
%%% R7A/1      2016-11-03 etxkols     dus6303
%%% R8A/1      2016-11-36 etxkols     SWM does not exist in cloud
%%% R8A/2      2016-11-24 etxkols     c608 & dus6502
%%% R8A/3      2016-12-13 etxkols     Removing rs232 hook for cloud and added eriAlarmCleared in wait_for_node_up/1
%%% R8A/4      2017-01-13 etxkols     Using installed_type to determine max memsize
%%% R9A/1      2017-02-28 etxivri     Add allowed trap eriChangeIPAddressEvent
%%% R9A/2      2017-05-17 erarube     Increase of CpuMemory check from 1000 to 1050 for LRAT, WRAT and GRAT due to dus53 on the limit.
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 all/0,
	 groups/0,
	 power_cycle/1,
	 reboot/1,
	 reinstall/1,
	 %% init_restart/1,
	 ssh_fail_sec_card/1,
	 cli/1,
	 coli/1,
	 coli_authlevel/1,
	 nc_get_config/1,
	 nc_get/1,
	 nc_add_rbsunit/1,
	 nc_reset_rbsunit/1,
	 nc_delete_rbsunit/1,
	 nc_getset_config/1,
	 check_test_app/1,
	 kill_com/1,
	 test_root_mo/1,
	 test_class1_all/1,
	 conf_snmpmgr/1,
	 send_traps/1,
	 ee_processes/1]).


%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_consserv, rct_rs232, rct_power, rct_rpc, rct_netconf, cth_conn_log, rct_logging, rct_tlib
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
			    undefined ->
				case os:getenv("SIM_OR_TARGET") of
				    "sim" -> 300;
				    _ -> 
					400
				end;
			    node_ci_dc_dus -> 1150; % Remove when node CI changes curl call
			    node_ci_dc_rbs -> 1150;
			    node_ci_hc_rbs -> 1150;
			    node_ci_dc_tcu03 -> 400;
			    node_ci_hc_tcu03 -> 400
			end
		end,
    case check_if_vc_board()  of
	"yes" -> [{ct_hooks, [{rct_htmllink,[]},
			      {rct_consserv,cs1},
			      {cth_conn_log,[]},
			      {rct_ssh,{ssh,[manual_connect]}},
			      {rct_coli, {coli, [manual_connect]}},
			      {rct_netconf, {nc1,man_auth}},
			      {rct_cli, {cli, [{user, "SysAdminTest"}, {password, "SysAdminTest"},manual_connect]}}
			     ]}];
	_-> 
	    Serial = case os:getenv("SIM_OR_TARGET") of
			 "cloudish" -> [{cth_conn_log,[]}];
			 _ -> [{rct_rs232,console}, {cth_conn_log,[]}]
		     end,
	    [{ct_hooks, [{rct_htmllink,[]},
			 {rct_consserv,cs1},
			 {rct_power,node},
			 {rct_snmpmgr,snmp1},
			 {rct_rpc, rpc},
			 {rct_netconf, nc1},
			 {rct_cli, {cli, [manual_connect]}},
			 {rct_ssh,{ssh,[manual_connect]}},
			 {rct_coli, {coli, [manual_connect]}},
			 {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],[]}}]}},
			 {rct_tlib,{kalle,[{cpumemory, CpuMemory},{cpuload,100}]}},
			 {rct_core,[]}] ++ Serial}]
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
%% @hidden
init_per_group(_GroupName, Config) ->
    Config.
%% @hidden
end_per_group(_GroupName, _Config) ->
    ok.
%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------

all() -> 
    case check_if_vc_board() of
	"yes" -> [cli, coli, nc_get_config, nc_get, %nc_getset_config,
		  coli_authlevel, ssh_fail_sec_card];
	_->    [check_test_app, cli, coli, nc_get_config, nc_get, %nc_getset_config, 
		ee_processes, test_root_mo, test_class1_all, conf_snmpmgr, send_traps,
		power_cycle, 
		check_test_app, cli, coli, nc_get_config, nc_get, %nc_getset_config, 
		ee_processes, test_root_mo, test_class1_all, send_traps,
		reboot, 
		check_test_app, cli, coli, nc_get_config, nc_get, %nc_getset_config, 
		ee_processes, test_root_mo, test_class1_all, send_traps]
    end.

%%--------------------------------------------------------------------
%% @doc 
%% Groups.<br/><br/>
%% @end
%%--------------------------------------------------------------------
groups() ->
    AllGroup = all(),
    [{default__group, [], AllGroup},
     {group_basic_tests, [], [{group, group_basic_tests_1}]},
     {group_basic_tests_1,[],[check_test_app, cli, coli, nc_get_config, nc_get, %nc_getset_config,
			    ee_processes, test_root_mo, test_class1_all, conf_snmpmgr, send_traps ]},
     {group_basic_tests_power_cycle, [], [{group, group_basic_tests_power_cycle_1}]},
     {group_basic_tests_power_cycle_1,[],[power_cycle, check_test_app, cli, coli, nc_get_config, nc_get, %nc_getset_config, 
					ee_processes, test_root_mo, test_class1_all, conf_snmpmgr, send_traps ]},
     {group_basic_tests_reboot,[],[reboot, check_test_app, cli, coli, nc_get_config, nc_get, %nc_getset_config, 
				   ee_processes, test_root_mo, test_class1_all, conf_snmpmgr, send_traps ]},
     %% {group_basic_tests_init_restart,[],[init_restart, check_test_app, cli, coli, nc_get_config, nc_get, %nc_getset_config, 
     %% 					 ee_processes, test_root_mo, test_class1_all, conf_snmpmgr, send_traps ]},
%% SBC 
     {sbc__qual__sim__1__group, [], []},
     {sbc__qual__dus__1__group, [], []},
     {sbc__qual__tcu__1__group, [], []},
     {sbc__def__sim__1__group, [], [{group, group_basic_tests_1}]},
     {sbc__def__dus__1__group, [], [{group, default__group}]},
     {sbc__def__tcu__1__group, [], [{group, default__group}]},
%% SBC  UPGRADE
     {sbc__upgrade__dus__1__group, [], [{group, group_basic_tests_power_cycle_1}]},
     {sbc__upgrade__tcu__1__group, [], [{group, group_basic_tests_power_cycle_1}]},
     {sbc__upgrade_short__dus__1__group, [], [{group, group_basic_tests_power_cycle_1}]},
     {sbc__upgrade_short__tcu__1__group, [], [{group, group_basic_tests_power_cycle_1}]},
%% SBC PRE_DC 
     {sbc__pre_dc__sec_tcu__1__group, [], [{group, default__group}]},  
     {sbc__pre_dc__sec_tcu__1__group, [], [{group, default__group}]}, 
     {sbc__pre_dc__tcu__1__group, [], [power_cycle]},   
     {sbc__pre_dc_grat__sec_dus__1__group, [], [{group, default__group}]},
     {sbc__pre_dc_lrat__sec_dus__1__group, [], [{group, default__group}]},
     {sbc__pre_dc_wrat__sec_dus__1__group, [], [{group, default__group}]},
     {sbc__pre_dc_grat__dus__1__group, [], [power_cycle]},
     {sbc__pre_dc_lrat__dus__1__group, [], [power_cycle]},
     {sbc__pre_dc_wrat__dus__1__group, [], [power_cycle]},
%% SBC PRE_DC UPGRADE
     {sbc__pre_dc_upgrade__sec_tcu__1__group, [], [{group, default__group}]},  
     {sbc__pre_dc_upgrade__sec_tcu__1__group, [], [{group, default__group}]}, 
     {sbc__pre_dc_upgrade__tcu__1__group, [], [power_cycle]},   
     {sbc__pre_dc_grat_upgrade__sec_dus__1__group, [], [{group, default__group}]},
     {sbc__pre_dc_lrat_upgrade__sec_dus__1__group, [], [{group, default__group}]},
     {sbc__pre_dc_wrat_upgrade__sec_dus__1__group, [], [{group, default__group}]},
     {sbc__pre_dc_grat_upgrade__dus__1__group, [], [power_cycle]},
     {sbc__pre_dc_lrat_upgrade__dus__1__group, [], [power_cycle]},
     {sbc__pre_dc_wrat_upgrade__dus__1__group, [], [power_cycle]},
%% SDC
     {sdc__cover__sim__1__group, [], [{group, group_basic_tests_1}]},
     {sdc__def__dus__1__group, [], [{group, default__group}]},
     {sdc__def__tcu__1__group, [], [{group, default__group}]},
     {sdc__basic__dus__1__group, [], [{group, group_basic_tests_1}]},
     {sdc__basic__tcu__1__group, [], [{group, group_basic_tests_1}]},
     {sdc__qual__sim__1__group, [], []},
     {sdc__qual__dus__1__group, [], []},
     {sdc__qual__tcu__1__group, [], []}
    ].
%%--------------------------------------------------------------------
%% @doc 
%% Power cycle board.<br/><br/>
%% @end
%%--------------------------------------------------------------------
power_cycle(_) ->
    rct_logging:get_all(log),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),
    rct_power:cycle(node),
    case wait_for_login_prompt() of
	ok ->
	    case wait_for_node_up(300) of
		ok ->
		    net_kernel:connect(ErlNode);
		_ ->
		    net_kernel:connect(ErlNode),
		    ct:fail("Did not receive eriAlarmHeartBeatNotif within 300 sec")
	    end;
	Other ->
	    net_kernel:connect(ErlNode),
	    ct:fail("No login: prompt Reason: ~p", [Other])
    end.
   
%% wait_for_node_up_1(Timeout) ->
%%     rct_snmpmgr:wait_for_traps([[{type,eriAlarmHeartBeatNotif}]],
%% 			       [{wait, true}],
%% 			       Timeout). 
wait_for_node_up(Timeout) ->
    rct_snmpmgr:wait_for_traps([[{type,eriAlarmHeartBeatNotif}]],
			       [{wait, true},
				{allowed_traps,[[{type,eriChangeIPAddressEvent}],
						[{type,eriAlarmAlarmListRebuilt}],
						[{type,eriAlarmCleared}],
						[{type,eriAlarmCritical}],
						[{type,eriAlarmMinor}],
						[{type,eriAlarmMajor}],
						[{type,eriAlarmWarning}]]}],
			       Timeout).

wait_for_login_prompt() ->
    case ct_telnet:expect(console, "login:", [{timeout,20000},no_prompt_check]) of
	{ok, _} ->
	    ok;
	Other ->
	    case proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))) of
		BOARDTYPE when BOARDTYPE == "tcu03";
			       BOARDTYPE == "tcu0401";
			       BOARDTYPE == "idu5205";
			       BOARDTYPE == "idu5209";
			       BOARDTYPE == "dus5301"; %dus53
			       BOARDTYPE == "dus3301"; %dus33
			       BOARDTYPE == "dus5201";
			       BOARDTYPE == "dus3201";
			       BOARDTYPE == "c608";
			       BOARDTYPE == "dus6502"; %micro
			       BOARDTYPE == "dus6303" -> %marco
		    ok = ct_telnet:send(console, ""), % login: prompt can be corrupted on dus2
		    case ct_telnet:expect(console, "login:", [{timeout,5000},no_prompt_check]) of 
                        {ok, _} ->
                            ok;
                        Other2 ->
			    Other2
		    end;
		_ ->
		    Other
	    end
    end.

%%--------------------------------------------------------------------
%% @doc 
%% reboot board.<br/><br/>
%% @end
%%--------------------------------------------------------------------
reboot(_) ->
    ok = rct_rs232:login(console),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),
    ct_telnet:send(console, "reboot"),   
    case wait_for_login_prompt() of
	ok ->
	    case wait_for_node_up(180) of
		ok ->
		    net_kernel:connect(ErlNode);
		_ ->
		    net_kernel:connect(ErlNode),
		    ct:fail("Could not connect using netconf")
	    end;
	Other ->
	    net_kernel:connect(ErlNode),
	    ct:fail("No login: prompt Reason: ~p", [Other])
    end.
%%--------------------------------------------------------------------
%% @doc 
%% reinstall board.<br/><br/>
%% @end
%%--------------------------------------------------------------------
reinstall(_) ->
    ok = rct_rs232:login(console),
    ok = rct_coli:connect(coli),
    {ok,_} = rct_coli:send(coli,"/misc/authlevel disabled"), 
    timer:sleep(1000),
    {ok,_} = rct_coli:send(coli,"/sys/sw -r"), 
    ok = rct_coli:disconnect(coli),
    timer:sleep(1000),
    case wait_for_node_up(600) of
	ok ->
	    ok;
	_ ->
	    ct:fail(" No eriAlarmHeartBeatNotif received in 600 sec")
    end.
   
%%--------------------------------------------------------------------
%% @doc 
%% init:restart() beam.<br/><br/>
%% @end
%% %%--------------------------------------------------------------------
%% init_restart(_) ->
%%     case rct_rpc:call(rpc, init, restart, [], 10000) of
%% 	ok ->
%% 	    timer:sleep(20000),
%% 	    case wait_for_node_up(180) of
%% 		ok ->
%% 		    ok;
%% 		_ ->
%% 		    ct:fail("Could not connect using netconf")
%% 	    end;
%% 	Other ->
%% 	    ct:log(lightred,"init:restart() failed, Reason: ~p",[Other]),
%% 	    ct:fail("init:restart() failed, Reason: ~p", [Other])
%%     end.

%% This testcase can probably be removed when ALL boards are running restructured EE
-define(EXPECTED_PPC_EE_PROCS, ["wdd","pghd","colid","trid","ns","bnld","bvld","cim","testboxd"]).
-define(EXPECTED_ARM_EE_PROCS, ["pghd","colid","trid","ns","bvld","cim","testboxd"]).
-define(EXPECTED_SIM_EE_PROCS, ["pghd","colid","bnld","bvld"]).
ee_processes(_) ->
    {Procs,ExpProcs} = case os:getenv("SIM_OR_TARGET") of
			   SIM_OR_CLOUDISH when SIM_OR_CLOUDISH == "sim";
						SIM_OR_CLOUDISH == "cloudish" ->
%%			   "sim" ->
			       %% User = os:getenv("USER"),
			       %% ct:log(ct_internal, "Cmd: \"linxstat_usel\"~nResult: ~s",
                               %%             [os:cmd("$RDE_TOP/tools/rcssim/bin/linxstat_usel")]),
			       %% Data = string:strip(os:cmd("ps -ef | grep RCS-SIM_CXP9021221_2 | grep "++User),right,$\n),
			       %% ct:log(ct_internal, "Cmd: \"ps -ef | grep RCS-SIM_CXP9021221_2 | grep "++User++"\"~nResult: ~s",[Data]),
			       %% {string:tokens(Data,"\n"),?EXPECTED_SIM_EE_PROCS};
			       {[],[]};
			   "target" ->
			       ok = rct_rs232:login(console),
			       TargProcs = case proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))) of
					       BOARDTYPE when BOARDTYPE == "tcu03";
							      BOARDTYPE == "tcu0401";
							      BOARDTYPE == "idu5205";
							      BOARDTYPE == "idu5209";
							      BOARDTYPE == "dus5301"; %dus53
							      BOARDTYPE == "dus3301"; %dus33
							      BOARDTYPE == "dus5201";
							      BOARDTYPE == "dus3201";
							      BOARDTYPE == "c608";
							      BOARDTYPE == "dus6502"; %micro
							      BOARDTYPE == "dus6303" -> %marco
						   case is_restructured() of 
						       true ->
							   Data = [[],[]],
							   [];
						       false ->
							   {ok, [_|Data]} = ct_telnet:cmd(console,"ps -ef | grep RCS-ARM_CXP9021221_3 | grep root"),
							   ?EXPECTED_ARM_EE_PROCS
						   end;
					       BOARDTYPE when BOARDTYPE == "dus4101";
							      BOARDTYPE == "duw4101" ->
						   {ok, [_|Data]} = ct_telnet:cmd(console,"ps -ef | grep RCS_CXP9021221_1 | grep root"),
						   ?EXPECTED_PPC_EE_PROCS
					   end,
			       %%% linxstat cmd sometimes returns userprompt directly, moving it to here
			       {ok, _} = ct_telnet:cmd(console,"linxstat"),
			       [_|Data2] = lists:reverse(Data),
			       {lists:reverse(Data2),TargProcs}
		       end,
    ok = ee_processes(Procs, ExpProcs, ok).

ee_processes(_,[],R) ->
    R;
ee_processes(Procs, [Proc|Tail], R) when Proc == "pghd" ->
    case [Line||Line <- Procs, string:str(Line, Proc) /= 0] of
	[_,_] ->
	    ct:log("2 EE processes found: ~s",[Proc]),
	    ee_processes(Procs, Tail, R);
	[] ->
	    ct:log(lightred,"EE process NOT found: ~s",[Proc]),
	    ee_processes(Procs, Tail, error);
	Other ->	   
	    ct:log(lightred,"Other than 2 EE process found: ~s~n~p",[Proc,Other]),
	    ee_processes(Procs, Tail, error)
    end;
ee_processes(Procs, [Proc|Tail], R) ->
    case [Line||Line <- Procs, string:str(Line, Proc) /= 0] of
	[_] ->
	    ct:log("EE process found: ~s",[Proc]),
	    ee_processes(Procs, Tail, R);
	[] ->
	    ct:log(lightred,"EE process NOT found: ~s",[Proc]),
	    ee_processes(Procs, Tail, error);
	Other ->	   
	    ct:log(lightred,"More than 1 EE process found: ~s~n~p",[Proc,Other]),
	    ee_processes(Procs, Tail, error)
    end.
%%--------------------------------------------------------------------
%% @doc 
%% Test that ssh don't work on secure board.<br/><br/>
%% @end
%%--------------------------------------------------------------------
ssh_fail_sec_card(_)  ->
    case rct_ssh:connect(ssh) of
	{error,econnrefused} ->
	    ok;
	Error -> ct:fail("~p",[Error])
    end.




%%--------------------------------------------------------------------
%% @doc 
%% Test a cli command.<br/><br/>
%% @end
%%--------------------------------------------------------------------
cli(_) ->
    ok = rct_cli:connect(cli),
    {ok,_} = rct_cli:send(cli,"show ManagedElement=1,SystemFunctions=1", "SysM=1"),
    ok = rct_cli:disconnect(cli).

%%--------------------------------------------------------------------
%% @doc 
%% Test a coli command.<br/><br/>
%% @end
%%--------------------------------------------------------------------
coli(_) ->
    ok = rct_coli:connect(coli),
%    {ok,_} = rct_coli:send(coli,"help", "Available commands: "),    
    {ok,_} = rct_coli:send(coli,"help coli", "Coli cmd shell usage"),    
    ok = rct_coli:disconnect(coli).

%%--------------------------------------------------------------------
%% @doc 
%% Test a coli command on secure board.<br/><br/>
%%  @end
%%--------------------------------------------------------------------
coli_authlevel(_) ->
    ok = rct_coli:connect(coli),
    case rct_coli:send(coli,"/misc/authlevel disabled", "no such command") of
    	{ok,_} -> ok;
    	Error -> ct:fail("~p",[Error])
    end,
    ok = rct_coli:disconnect(coli).

%%--------------------------------------------------------------------
%% @doc 
%% netconf get all configurable data.<br/><br/>
%% @end
%%--------------------------------------------------------------------
nc_get_config(_) ->
    F = fun() -> {ok,_} = netconf_open(nc1,[]),
		 {ok,_} = ct_netconfc:get_config(nc1,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],[{managedElementId,[],["1"]}]}),
		 ok = ct_netconfc:close_session(nc1)
	end,    
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(nc1),
	    ct:fail(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc 
%% netconf get all data.<br/><br/>
%% @end
%%--------------------------------------------------------------------
nc_get(_) ->
    F = fun() -> {ok,_} = netconf_open(nc1,[]),
		 {ok,_} = ct_netconfc:get(nc1,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],[{managedElementId,[],["1"]}]}),
		 ok = ct_netconfc:close_session(nc1)
	end,    
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(nc1),
	    ct:fail(Reason)
    end.	

%%--------------------------------------------------------------------
%% @doc 
%% Test netconf create by creating a rbsUnit in FEQM.<br/><br/>
%% @end
%%--------------------------------------------------------------------
nc_add_rbsunit(_) ->
    F = fun() -> {ok,_} = netconf_open(nc1,[]),
		 ok = ct_netconfc:edit_config(nc1,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
							  [{managedElementId,[],["1"]},
							   {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
							    [{'equipmentId',[],["1"]},
							     {'RbsUnit',[{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
							      [{rbsUnitId,[],["kalle_unit"]}]}]}]}),
		 ok = ct_netconfc:close_session(nc1),
		 {ok,_} = netconf_open(nc1,[]),	
		 {ok,[{'ManagedElement',
		       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		       [{managedElementId,[],["1"]},
			{'Equipment',
			 [{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
			 [{equipmentId,[],["1"]},
			  {'RbsUnit',
			   [{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
			   [{rbsUnitId,[],["kalle_unit"]}|_]}]}]}]} =
		     ct_netconfc:get(nc1,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
					  [{managedElementId,[],["1"]},
					   {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
					    [{'equipmentId',[],["1"]}]}]}),
		 ok = ct_netconfc:close_session(nc1)
	end,    
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(nc1),
	    ct:fail(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc 
%% Test netconf action by reseting a rbsUnit in FEQM.<br/><br/>
%% @end
%%--------------------------------------------------------------------
nc_reset_rbsunit(_) ->
    F = fun() -> {ok,_} = netconf_open(nc1,[]),
		 {ok,_} = ct_netconfc:action(nc1,{'ManagedElement',
						  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
						  [{managedElementId,[],["1"]},
						   {'Equipment',
						    [{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
						    [{'equipmentId',[],["1"]},
						     {'RbsUnit',
						      [{xmlns,"urn:com:ericsson:ecim:rbsunit"}],
						      [{rbsUnitId,[],["kalle_unit"]},
						       {reset,[{'resetType',["HARD"]},{'gracefulReset',["true"]}]}]}]}]}),
		 ok = ct_netconfc:close_session(nc1)
	end,    
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(nc1),
	    ct:fail(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc 
%% Test netconf delete by deleteing a rbsUnit in FEQM.<br/><br/>
%% @end
%%--------------------------------------------------------------------
nc_delete_rbsunit(_) ->
    F = fun() -> {ok,_} = netconf_open(nc1,[]),
		 ok = ct_netconfc:edit_config(nc1,running,{'ManagedElement',
							   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
							   [{managedElementId,[],["1"]},
							    {'Equipment',
							     [{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
							     [{'equipmentId',[],["1"]},
							      {'RbsUnit',
							       [{xmlns,"urn:com:ericsson:ecim:rbsunit"},
								{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
								{'nc:operation',"delete"}],
							       [{rbsUnitId,[],["kalle_unit"]}]}]}]}),
		 ok = ct_netconfc:close_session(nc1),
		 {ok,_} = netconf_open(nc1,[]),
		 {ok,[{'ManagedElement',
		       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
		       [{managedElementId,[],["1"]},
			{'Equipment',
			 [{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
			 [{equipmentId,[],["1"]}]}]}]} =
		     ct_netconfc:get(nc1,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
					  [{managedElementId,[],["1"]},
					   {'Equipment',[{xmlns,"urn:com:ericsson:ecim:ECIM_Equipment:ECIM_Equipment:1.0"}],
					    [{'equipmentId',[],["1"]}]}]}),
		 ok = ct_netconfc:close_session(nc1)
	end,    
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(nc1),
	    ct:fail(Reason)
    end. 	    

%%--------------------------------------------------------------------
%% @doc
%% get_config on whole configuration, change SwM fallbackTimer, restore configuration and verify old value of fallbackTimer.<br/><br/>
%% @end
%%--------------------------------------------------------------------
nc_getset_config(_) ->
     F = fun() -> {ok,_} = netconf_open(nc1,[]),
		  {ok, OldFallbackTimer} = get_fallbackTimer(nc1),
		  ct:log(white,"Original fallbackTimer = ~p", [OldFallbackTimer]),
		  ct:log(white,"Get netconf configuration"),
		  {ok,[Xml]} = ct_netconfc:get_config(nc1,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],[{managedElementId,[],["1"]}]}),
		  NewFallbackTimer = OldFallbackTimer + 1,
		  ct:log(white,"Set new fallbackTimer = ~p", [NewFallbackTimer]),
		  ok = set_fallbackTimer(nc1,NewFallbackTimer),
		  ok = ct_netconfc:close_session(nc1),
		  
		  {ok,_} = netconf_open(nc1,[]),
		  ct:log(white,"Verify new fallbackTimer = ~p", [NewFallbackTimer]),
		  {ok,NewFallbackTimer} = get_fallbackTimer(nc1),
		  ok = ct_netconfc:edit_config(nc1,running,Xml),    
		  ok = ct_netconfc:close_session(nc1),

		  {ok,_} = netconf_open(nc1,[]),
		  {ok,OldFallbackTimer} = get_fallbackTimer(nc1),
		  ct:log(lightgreen,"Original fallbackTimer = ~p", [OldFallbackTimer]),    
		  ok = ct_netconfc:close_session(nc1)
	end,    
    try F()
    catch
	_:Reason ->
	    io:format("############ ~p",[ct_netconfc:close_session(nc1)]),
	    ct:fail(Reason)
    end. 	    

%%--------------------------------------------------------------------
%% @doc
%% Verify that a test_app is started.<br/><br/>
%% @end
%%--------------------------------------------------------------------
check_test_app(_) ->
    Args = [],
    ReqApps = ["test_app","ift_app","test_oi"],
    FoundApps = [A || {A,_} <- rct_rpc:call(rpc, appmServer, get_apps, Args, 10000)],
    true = all_found(ReqApps, FoundApps),
    ok.

all_found([],_) ->
    true;
all_found([H|T],Apps) ->
    case lists:member(H,Apps) of
	true -> all_found(T,Apps);
	false -> false
    end.

%%--------------------------------------------------------------------
%% @doc 
%% Kill linux com process, verify that it is restarted and verify netconf, CLI and Coli.<br/><br/>
%% @end
%%--------------------------------------------------------------------
kill_com(_) ->
    ComPid = case os:getenv("SIM_OR_TARGET") of
		 "sim" ->
		     Data = string:strip(os:cmd("ps -ef | grep com.cfg | grep local | awk -F' ' '{print $2}'"),right,$\n),
%      	             Data = string:strip(os:cmd("pgrep -f com.cfg"),right,$\n),
		     ct:log(internal, "Cmd: \"ps -ef | grep com | grep local | awk -F' ' '{print $2}'\"~nResult: ~p",[Data]),
		     Data;
		 "target" ->
		     ok = rct_rs232:login(console),
		     {ok, [_, Data, _]} = ct_telnet:cmd(console,"pgrep -f com.cfg"),
		     Data
	     end,
    {match,_} = re:run(ComPid,"^[0-9]+$",[]),
    ct:log(ct_internal, "Cmd: \"kill -9 " ++ ComPid ++ "\"~n"),
    case os:getenv("SIM_OR_TARGET") of
	"sim" ->
	    os:cmd("kill -9 " ++ ComPid);
	"target" ->
	    ct_telnet:cmd(console,"kill -9 " ++ ComPid)
    end,
    timer:sleep(15000),
    ComPid2 = case os:getenv("SIM_OR_TARGET") of
		 "sim" ->
		     Data2 = string:strip(os:cmd("ps -ef | grep com.cfg | grep local | awk -F' ' '{print $2}'"),right,$\n),
%		     Data2 = string:strip(os:cmd("pgrep -f com.cfg"),right,$\n),
		     ct:log(internal, "Cmd: \"ps -ef | grep com | grep local | awk -F' ' '{print $2}'\"~nResult: ~p",[Data2]),
		     Data2;
		 "target" ->
		     ok = rct_rs232:login(console),
		     {ok, [_, Data2, _]} = ct_telnet:cmd(console,"pgrep -f com.cfg"),
		     Data2
	     end,
    {match,_} = re:run(ComPid2,"^[0-9]+$",[]),
    case ComPid2 of
	ComPid -> ct:fail("Linux com process not restarted");
	_ -> ok
    end,
    F = fun() -> {ok,_} = netconf_open(nc1,[]),
		 {ok,_} = ct_netconfc:get_config(nc1,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],[{managedElementId,[],["1"]}]}),
		 ok = ct_netconfc:close_session(nc1)
	end,    
    try F()
    catch
	_:Reason ->
	    ct_netconfc:close_session(nc1),
	    ct:fail(Reason)
    end,
    ok = rct_cli:connect(cli),
    {ok,_} = rct_cli:send(cli,"show ManagedElement=1,SystemFunctions=1", "SwM=1"),
    ok = rct_cli:disconnect(cli),
    ok = rct_coli:connect(coli),
    {ok,_} = rct_coli:send(coli,"help", "Available commands: "),    
    ok = rct_coli:disconnect(coli).

%%--------------------------------------------------------------------
%% @doc 
%% Test root mo.<br/><br/>
%% @end
%%--------------------------------------------------------------------
test_root_mo(_Config) ->
    {ok, _} = netconf_open(nc1, []),
    {ok, [{'ManagedElement',
           [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
           [{managedElementId,[],["1"]},
            {'TestRoot',
             [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
             [{testRootId,[],["1"]}|Rest]}]}]} = ct_netconfc:get(nc1,{'ManagedElement',
                                                                      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
                                                                      [{managedElementId,[],["1"]},
                                                                       {'TestRoot',
                                                                        [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
                                                                        [{testRootId,[],["1"]}]}]}),
    
    ct_netconfc:close_session(nc1),
    case Rest of
	[] ->
	    ct:fail({error, "Missing pre-defined instances"});
	_ ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc 
%% Test class1.<br/><br/>
%% @end
%%--------------------------------------------------------------------
test_class1_all(_Config) ->
    {ok, _} = netconf_open(nc1, []),
    Res = 
        try 
            ok = ct_netconfc:edit_config(nc1,running,{'ManagedElement',
                                                      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
                                                      [{managedElementId,[],["1"]},
                                                       {'TestRoot',
                                                        [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
                                                        [{testRootId,[],["1"]},
                                                         {'TestClass1',
                                                          [],
                                                          [{'testClass1Id',[],["2"]}]}]}]}),
            {ok, [{'ManagedElement',
                   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
                   [{managedElementId,[],["1"]},
                    {'TestRoot',
                     [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
                     [{testRootId,[],["1"]},
                       {'TestClass1',
                        [],
                        [{'testClass1Id',[],["2"]}|_]}]}]}]} = ct_netconfc:get(nc1,
                                                                               {'ManagedElement',
                                                                                [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
                                                                                [{managedElementId,[],["1"]},
                                                                                 {'TestRoot',
                                                                                  [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
                                                                                  [{testRootId,[],["1"]},
                                                                                   {'TestClass1',
                                                                                    [],
                                                                                    [{'testClass1Id',[],["2"]}]}]}]}),
            
            ok = ct_netconfc:edit_config(nc1,
                                         running,
                                         {'ManagedElement',
                                          [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
                                          [{managedElementId,[],["1"]},
                                           {'TestRoot',
                                            [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
                                            [{testRootId,[],["1"]},
                                             {'TestClass1',
                                              [{'xmlns:nc',"urn:ietf:params:xml:ns:netconf:base:1.0"},
                                               {'nc:operation',"delete"}],
                                              [{'testClass1Id',[],["2"]}]}]}]}),
            {error, _} = ct_netconfc:get(nc1,
                                         {'ManagedElement',
                                          [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
                                          [{managedElementId,[],["1"]},
                                           {'TestRoot',
                                            [{xmlns,"urn:com:ericsson:ecim:TESTMOM"}],
                                            [{testRootId,[],["1"]},
                                             {'TestClass1',
                                              [],
                                              [{'testClass1Id',[],["2"]}]}]}]}),
            
            ok
            
        catch T:E ->
                {error, {T,E}}
        end,
    ct_netconfc:close_session(nc1),
    Res = ok.

%%--------------------------------------------------------------------
%% @doc 
%% Configure trapreceiver using netconf.<br/><br/>
%% @end
%%--------------------------------------------------------------------
conf_snmpmgr(_Config) ->
    {{mgr_ip,MgrIp}, 
     {mgr_port,MgrPort}, 
     {agent_ip,AgentIp}, 
     {agent_port,AgentPort}} = rct_snmpmgr:get_mgr_data(snmp1),
    Platform = os:getenv("SIM_OR_TARGET"),
    PduType = if Platform =:= "sim" -> "TRAP"; true -> "INFORM" end,
    F = fun() -> {ok,_} = netconf_open(nc1,[]),
		 ok = case Platform of
			  "sim" ->
			      ct_netconfc:edit_config(nc1,running,{'ManagedElement',
								   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
								   [{managedElementId,[],["1"]},
								    {'SystemFunctions',
								     [{systemFunctionsId,[],["1"]},
								      {'SysM',
								       [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
								       [{sysMId,[],["1"]},
									{'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
									 [{snmpId,[],["1"]},
									  {administrativeState,[],["UNLOCKED"]},
									  {'SnmpTargetV2C',
									   [{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
									   [{snmpTargetV2CId,[],["1"]},
									    {address,[],[MgrIp]},
									    {community,[],["public"]},
									    {transportMethod,[],[PduType]},
									    {port,[],[MgrPort]}]}]}]}]}]});
			  TARG_OR_CLOUD when TARG_OR_CLOUD == "target";
					     TARG_OR_CLOUD == "cloudish" ->
			      ct_netconfc:edit_config(nc1,running,{'ManagedElement',
								   [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
								   [{managedElementId,[],["1"]},
								    {'SystemFunctions',
								     [{systemFunctionsId,[],["1"]},
								      {'SysM',
								       [{xmlns,"urn:com:ericsson:ecim:ComSysM"}],
								       [{sysMId,[],["1"]},
									{'Snmp',[{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
									 [{snmpId,[],["1"]},
									  {administrativeState,[],["UNLOCKED"]},
									  {agentAddress,
									   [{struct,"HostAndPort"}],
									   [{host,[],[AgentIp]},
									    {port,[],[AgentPort]}]},
									  {'SnmpTargetV2C',
									   [{xmlns,"urn:com:ericsson:ecim:ComSnmp"}],
									   [{snmpTargetV2CId,[],["1"]},
									    {address,[],[MgrIp]},
									    {informRetryCount,[],["6"]},
									    {transportMethod,[],[PduType]},
									    {community,[],["public"]},
									    {port,[],[MgrPort]}]}]}]}]}]})
		      end,
                 ok = ct_netconfc:close_session(nc1)
        end,    
    try F()
    catch
        _:Reason ->
            ct_netconfc:close_session(nc1),
           ct:fail(Reason)
    end,
    ct:pal("SNMP ~ss will be sent to: ~s:~s", [PduType, MgrIp, MgrPort]),
    timer:sleep(1000). % Seems necessary before start sending traps


%%--------------------------------------------------------------------
%% @doc 
%% Send alarm trap and clear alarm trap.<br/><br/>
%% @end
%%--------------------------------------------------------------------
send_traps(_Config) ->
    ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmMajor},
				      {eriAlarmActiveManagedObject,"ManagedElement=1,TestRoot=1,TestClass1=1"},
				      {eriAlarmActiveSpecificProblem,"Resource Monitor Coffee Beans Container Low Level"},
				      {eriAlarmNObjAdditionalText,"Coffee beans refill required"}]],
				    [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}],
						     [{type,eriAlarmAlarmListRebuilt}]]}],
				    60),
    ok = rct_rpc:call(rpc, comsaI, send_alarm,['ResourceMonitorCoffeeBeansContainerLowLevel', major, [<<"ManagedElement=1">>,<<"TestRoot=1">>,<<"TestClass1=1">>], "Coffee beans refill required"],20000),
    ok = rct_snmpmgr:check_traps(),
    ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmCleared},
				      {eriAlarmActiveManagedObject,"ManagedElement=1,TestRoot=1,TestClass1=1"},
				      {eriAlarmActiveSpecificProblem,"Resource Monitor Coffee Beans Container Low Level"},
				      {eriAlarmNObjAdditionalText,"Coffee beans refill required"}]],
				    [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}],
						     [{type,eriAlarmAlarmListRebuilt}]]}],
				    60),
    ok = rct_rpc:call(rpc, comsaI, clear_alarm,['ResourceMonitorCoffeeBeansContainerLowLevel', [<<"ManagedElement=1">>,<<"TestRoot=1">>,<<"TestClass1=1">>]],20000),
    ok = rct_snmpmgr:check_traps().

%% send_traps(_Config) ->
%%     ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmCritical},
%% 				      {eriAlarmActiveSpecificProblem,"lihLKFFaultAlarm"},
%% 				      {eriAlarmActiveManagedObject,"ManagedElement,1,SystemFunctions,1,Licensing,1"}]],
%% 				    [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}]]}],
%% 				    20),
%%     ok = rct_rpc:call(rpc,lih_fm, request_LKF_fault_alarm,[],20000),
%%     ok = rct_snmpmgr:check_traps(),
%%     ok = rct_snmpmgr:wait_for_traps([[{type,eriAlarmCleared},
%% 				      {eriAlarmActiveSpecificProblem,"lihLKFFaultAlarm"},
%% 				      {eriAlarmActiveManagedObject,"ManagedElement,1,SystemFunctions,1,Licensing,1"}]],
%% 				    [{allowed_traps,[[{type,eriAlarmHeartBeatNotif}]]}],
%% 				    20),
%%     ok = rct_rpc:call(rpc,lih_fm, cancel_LKF_fault_alarm,[],20000),
%%     ok = rct_snmpmgr:check_traps().

%%%  INTERNAL FUNCTIONS

get_fallbackTimer(Client) ->
    {ok,[{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	  [{managedElementId,[],["1"]},
	   {'SystemFunctions',[],
	    [{systemFunctionsId,[],["1"]},
	     {'SwM',[{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
	      [{fallbackTimer,[],[FallbackTimer]},{swMId,[],["1"]}]}]}]}]} = 
	ct_netconfc:get_config(Client,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
					       [{managedElementId,[],["1"]},
						{'SystemFunctions',[],
						 [{systemFunctionsId,[],["1"]},
						  {'SwM',[{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
						   [{swMId,[],["1"]},{fallbackTimer,[],[]}]}]}]}),
    {ok, list_to_integer(FallbackTimer)}.

set_fallbackTimer(Client,FallbackTimer) ->
    ct_netconfc:edit_config(Client,running,{'ManagedElement',[{xmlns,"urn:com:ericsson:ecim:ComTop"}],
					    [{managedElementId,[],["1"]},
					     {'SystemFunctions',[],
					      [{systemFunctionsId,[],["1"]},
					       {'SwM',[{xmlns,"urn:com:ericsson:ecim:RcsSwM"}],
						[{swMId,[],["1"]},{fallbackTimer,[],[integer_to_list(FallbackTimer)]}]}]}]}).

is_restructured() ->
    ok = rct_rs232:login(console),
    {ok,Data} = ct_telnet:cmd(console,"ls -d /home/testbox"),
    case string:str(Data, "No such file or directory") of
	0 -> true;
	_ -> false
    end.
netconf_open(Session, InPar)->
    Param = [{timeout,10000}|InPar],
    case check_if_vc_board()  of
	"yes" ->  ct_netconfc:open(Session, [{user, "SysAdminTest"}, {password, "SysAdminTest"}|Param]);
	_ -> ct_netconfc:open(Session,Param)
    end.
check_if_vc_board()->
    Hw = atom_to_list(ct:get_config({test_nodes,1})),
    Secure_board =  ct:get_config({list_to_atom(Hw),secure_board}),
    Secure_board.
