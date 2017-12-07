%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	measure_restart_SUITE.erl %
%%% @author erarube
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R12A/2
%%% 
%%% @doc ==Measure times for differents restarts causes.==
%%% This Test Suite can be used on target enviroment.
%%% It will also measure differents startup times and som cases measure used memory size, 
%%% after differents types of restarts. 
%%% Used sw version and measured data is written to a file with same name as TC.
%%% Path: /proj/rcs/measurements/
%%%
%%% Note! Measure time when CS starts will sometimes fail due to sometimes the printouts on the consol
%%% will consist of of other text than expect. 
%%% So until a better way to measure when CS start can be done, then measure when login prompt comes.  
%%% <br/><br/>
%%% 
%%% @end


-module(measure_restart_SUITE).
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
%%% R2A/1      2012-10-29 etxivri     Created
%%% R2A/2      2012-10-29 etxivri     Updated date in rev log, and log result.
%%% R2A/4      2012-10-31 etxivri     Minor changes.
%%% R2A/5      2012-10-31 etxivri     Added more TC
%%% R2A/6      2012-11-01 etxivri     Fixed minor bug.
%%% R2A/7      2012-11-05 etxivri     Change ct_telenet cmd to send and expect. Etc
%%% R2A/8      2012-11-05 etxivri     Add check of memory size.
%%% R2A/10     2012-11-09 etxivri     Created a group to be runed in jenkins.
%%% R2A/12     2012-11-29 etxivri     Fix a wait_for_power_on. 
%%%                                   Created new TCs and added measure starting MW.
%%% R2A/13     2012-12-03 etxivri     Added initial_rstart TC. Some cleanup
%%%                                   Fix differents FileName depending of used HW type.
%%% R2A/14     2012-12-05 etxivri     Add measure disc usage.
%%% R2A/15     2012-01-15 etxivri     Added Node Name to logged file.
%%%                                   Change in evaluate time check.
%%% R2A/17     2013-01-18 etxivri     Increased value in evaluate time check.
%%% R2A/18     2013-01-21 etxivri     Update path for disc usage check.
%%% R2A/19     2013-02-28 etxkols     Added rct_core hook
%%% R2A/20     2013-03-06 etxivri     Updated get used mem size for beam, soo we get from correct beam.smp
%%% R2A/21     2013-03-06 etxivri     Forgot to set back the RESULTDIR that shall be used.
%%% R2A/22     2013-03-14 etxivri     Changed Timediff from 35 to 45 after Squashfs.
%%% R2A/23     2013-04-02 etxivri     Increased Timediff from 45 to 60, from MW start to netconf reply. 
%%% R2A/24     2013-04-26 etxivri     Decrease nr of times to check "starting rcs". 
%%%                                   Increased time to check that COM has restarted after initial restart. 
%%% R2A/25     2013-05-03 etxivri     Increased timeout on rct_rpc:call for "pgrep com". 
%%% R2A/26     2013-05-03 etxivri     Increased timeout on rct_rpc:call for "pgrep com". 
%%% R2A/27     2013-05-03 etxivri     Increased timeout on rct_rpc:call for appmServer get_apps 
%%%                                   in case this also will take longer time, like os cmd. 
%%% R2A/28     2013-05-03 etxivri     Increased limit times in evalute check.
%%%                                   Removed tries to check after "Starting RCS" since they are not printed out any more!
%%%                                   A new check of when start MW is TBD.
%%% R2A/29     2013-05-03 etxivri     Forgott to change back to correct RESULTDIR.
%%% R2A/30     2013-08-13 etxkols     Changed raw to no_prompt_check in ct_telnet:expect
%%% R2A/31     2013-10-02 etxkols     Increased time from in evaluate times 60 to 120.
%%% R2A/32     2013-10-16 etxivri     Updates for TCU
%%% R2A/33     2013-10-18 etxivri     Added printouts when get discusage is used.
%%% R2A/34     2013-10-18 etxivri     Upadetes in wait_for_power_on due to sometimes this could fail.
%%% R2A/35     2013-11-22 etxivri     Increase timeout when rpc is used. 
%%%                                   Removed check of unexpected HZ printouts since it is corretced. 
%%% R2A/36     2014-01-16 etxkols     Support for dus5201. 
%%% R2A/37     2014-02-04 etxkols     Prolonged rpc:call timeouts
%%% R2A/38     2014-03-07 etxivri     Update to add Branch file name when 
%%%                                   other than Branch R2A is used.
%%% R2A/39     2014-06-03 etxivri     Changed check for "du1 login" prompt to 
%%%                                   "login:" 
%%% R2A/40     2014-08-18 etxivri     Update disc usage for RCS cxp due to 
%%%                                   split of MW and EE on ARM.
%%% R2A/41     2014-08-18 etxivri     More update to handle correct discusage.
%%% R2A/42     2014-09-01 etxivri     Update to add branch on loged file.
%%% R3A/1      2014-11-12 etxivri     Update due to permission denied whe du is used 
%%%                                   on dir that contains EE, as sirpa user.
%%% R3A/2      2014-11-21 etxivri     Added tc to be runed in pre dc. 
%%%                                   Use init reboot to trig reboot. And clean up.
%%%                                   Start of new logfiles.
%%% R3A/3      2014-11-26 etxivri     Removed com and appl startup times, due to the 
%%%                                   way to meassure is obsolete. NC time is when session is open.
%%% R3A/4      2014-11-28 etxivri     Update to be more robust. Changed timeout option in ct_telnet expect.
%%% R3A/5      2014-11-30 etxarnu     Update for SWM 3.0 (read SwVersion instead of SwVersionMain)
%%% R3A/7      2014-12-1 etxivri      removed use of use of total_timeout since it not working correct.
%%% R3A/9      2014-12-3 etxivri      Add group to be runed after a upgrade.
%%% R3A/10     2014-12-03 etxivri     Minor update in group.
%%% R3A/11     2014-12-04 etxivri     Added check that it is ok to use rpc
%%% R3A/12     2014-12-05 etxivri     Try to make check for login prompt more robust.
%%% R3A/13     2014-12-06 etxivri     Minor update when disconnect to erl node.
%%% R3A/14     2014-12-16 etxivri     Check cli is up before get_sw_version.
%%% R3A/15     2015-01-19 etxivri     Correct log path.
%%% R3A/16     2014-12-16 etxivri     Update get_sw_version to be more robust
%%% R3A/17     2014-12-16 etxivri     More update in get_sw_version to be more robust,
%%% R3A/18     2015-03-05 etxivri     Bugfix and update for new boards
%%% R4A/18     2015-04-27 etxivri     Update logile name for R4 branch.
%%% R4A/2      2015-04-28 etxivri     chancged log dir and update log file name.
%%% R4A/3      2015-04-29 etxivri     Minor update.
%%% R4A/4      2015-05-04 etxivri     Some cleanup.
%%% R4A/5      2015-05-04 etxivri     Correct resultdir.
%%% R4A/5      2015-07-13 etxjovp     Add group definitions used by CS CI
%%% R4A/5      2015-07-13 etxjovp     modify group definitions used by CS CI
%%% R4A/8      2015-09-09 etxivri     Started on warm restart tc using coli.
%%% R4A/9      2015-09-10 etxivri     More update in coli warm restarts.
%%% R4A/10     2015-09-11 etxivri     Add a sleep before trig warm restart via
%%%                                   coli, to make sure write to disc is done.
%%% R4A/11     2015-09-15 etxivri     disable warm restart via coli, end of tc.
%%% R4A/12     2015-09-22 etxivri     Update to use fru warm restart on full up,
%%%                                   instead for coli. 
%%% R4A/13     2015-09-24 etxivri     Removed avli time when onlu mw is install,
%%%                                   RcsNodeDown time will not be correct.
%%% R4A/14     2015-09-30 etxivri     Changed now() to os:timestamp().
%%% R5A/1      2015-10-07 etxivri     Update resultdir for R5 and change to 
%%%                                   use build label instead for cs cxp.
%%% R5A/2      2015-10-13 etxivri     Update changes from R4 branch.
%%%                                   Disable warm restart for lte node, not
%%%                                   suported yet.
%%% R5A/3      2015-10-30 etxivri     Update changes from R4 branch.
%%%                                   - Update to get erl log after power.
%%%                                   - Removed use of init restart.
%%%                                   - Update due to warm restart is 
%%%                                     enabled on TCU
%%% R5A/4      2015-12-03 etxivri     Warm restart is now enabled on DUS default
%%% R6A/1      2016-04-28 etxivri     Update for R6
%%% R6A/2      2016-05-23 etxivri     Check if dus5201 runs as bpu.
%%% R7A/1      2016-05-27 etxivri     Update for WR8, outcome from free cmd is 
%%%                                   changed
%%% R7A/2      2016-08-31 etxivri     Update log dir for wr8_64
%%% R7A/3      2016-10-24 etxivri     Update to handle git products.
%%% R8A/1      2016-10-31 etxivri     Update for R8
%%% R8A/2      2016-11-01 etxivri     Update directory to store data for dusX3
%%% R9A/1      2016-01-23 etxivri     Update for R9
%%% R9A/1      2017-04-11 erarube     Update for result directory on R10
%%% R12A/1     2017-10-25 etxivri     Update log dir to be a generic dir R_XY there everything will be stored.
%%% R12A/2     2017-11-13 erarube     Support for dus5201tk and dus3201tk
%%% ----------------------------------------------------------
%%% 

% -compile([export_all]).
-include_lib("common_test/include/ct.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 %% wait_for_login_prompt/1,
	 wait_for_netconf_started/1,
	 get_used_memsize/0,
	 get_sw_version/0,
	 groups/0,
	 all/0,
	 meas_reboot_time_memsize/1,
	 meas_power_time_memsize/1,
	 meas_init_restart_time_memsize/1,
	 ug_meas_reboot_time_memsize/1,
	 ug_meas_power_time_memsize/1,
	 ug_meas_init_restart_time_memsize/1,
	 create_fru/1,
	 rbs_meas_fru_restart_cold/1,
	 lte_rbs_meas_fru_restart_cold/1,
	 tcu_meas_fru_restart_cold/1,
	 %% rbs_meas_power/1,
	 lte_rbs_meas_power/1,
	 tcu_meas_power/1,
	 rbs_meas_init_restart/1,
	 %% rbs_meas_reboot/1
	 meas_coli_warm_restart/1,
	 lte_meas_coli_warm_restart/1,
	 tcu_meas_coli_warm_restart/1,
	 ug_meas_coli_warm_restart/1,
	 lte_meas_fru_warm_restart/1,
	 tcu_meas_fru_warm_restart/1
	]).

-define(RESULTDIR, "/proj/rcs/measurements/measure_restart/R_XY/").
%% -define(RESULTDIR_WR8_64, "/proj/rcs/measurements/measure_restart/R8_wr8_64/").
%% -define(RESULTDIR, "/home/etxivri/tmp/").
-define(RESULTDIR_WARM, "/proj/rcs/measurements/meas_warm_restart/R_XY").
%% -define(RESULTDIR_WARM_WR8_64, "/proj/rcs/measurements/meas_warm_restart/R8_wr8_64").
%% -define(RESULTDIR_WARM, "/home/etxivri/tmp/").

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{timetrap, {minutes, 600}}, % 10 hours
     {ct_hooks, [{rct_rpc, rpc},
		 {rct_htmllink,[]},
		 {rct_power,node},
		 {rct_netconf, nc1},
		 %% {cth_conn_log, []},
		 {rct_consserv,cs1},
		 {rct_logging, {all, [{erlang,{["ERROR REPORT",
						"CRASH REPORT"],
					       [
					       ]}}]}},
                 {rct_rs232,console},
		 {rct_cli, {cli, [manual_connect]}},
		 {rct_coli, {coli, [manual_connect]}},
		 {rct_tlib,{load,[]}},
                 {rct_core,[]}
		]}].

%% @hidden
init_per_suite(Config) ->
    Config.
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
end_per_testcase(_TestCase, _Config) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
     meas_reboot_time_memsize,
     meas_power_time_memsize
     %% meas_init_restart_time_memsize
    ].

%%--------------------------------------------------------------------
%% @doc
%% TCs runed by jenkins.
%% @end
%%--------------------------------------------------------------------
groups() ->
    AllGroup=all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__dus__1__group, [], [{group, group_measure_restart_1_1}]},
     {sbc__def__tcu__1__group, [], [{group, group_measure_restart_1_1}]},
     {sbc__pre_dc__tcu__1__group, [], [{group, tcu_group_measure_restart_1_1}]},
     {sbc__pre_dc_lrat__dus__1__group, [], [{group, lte_rbs_group_measure_restart_1_1}]},
     {sbc__upgrade__dus__1__group, [], [{group, ug_group_measure_restart_1_1}]},
     {sbc__upgrade__tcu__1__group, [], [{group, ug_group_measure_restart_1_1}]},
     {sbc__upgrade_short__dus__1__group, [], [{group, ug_group_measure_restart_1_1}]},
     {sbc__upgrade_short__tcu__1__group, [], [{group, ug_group_measure_restart_1_1}]},
     {group_measure_restart_1, [], [{group, group_measure_restart_1_1}]},
     {group_measure_restart_1_1,[],[meas_reboot_time_memsize,
				    meas_power_time_memsize,
				    %% meas_init_restart_time_memsize,
				    meas_coli_warm_restart
				   ]},
     {lte_rbs_group_measure_restart_1, [], [{group, lte_rbs_group_measure_restart_1_1}]},
     {lte_rbs_group_measure_restart_1_1,[],[lte_rbs_meas_power,
					    lte_rbs_meas_fru_restart_cold,
					    %% %% lte_meas_coli_warm_restart
					    lte_meas_fru_warm_restart
					 ]},
     {tcu_group_measure_restart_1, [], [{group, tcu_group_measure_restart_1_1}]},
     {tcu_group_measure_restart_1_1,[],[tcu_meas_power,
					tcu_meas_fru_restart_cold,
					%% tcu_meas_coli_warm_restart
					tcu_meas_fru_warm_restart
				     ]},
     {ug_group_measure_restart_1, [], [{group, ug_group_measure_restart_1_1}]},
     {ug_group_measure_restart_1_1,[],[ug_meas_reboot_time_memsize,
				       ug_meas_power_time_memsize,
				       %% ug_meas_init_restart_time_memsize,
				       ug_meas_coli_warm_restart
				    ]}
     ].

%%--------------------------------------------------------------------
%% @doc
%% Measure login prompt and Netconf time and memory size, after reboot. <br/>
%% @spec meas_reboot_time_memsize(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
meas_reboot_time_memsize(_Config) ->
    ct:pal("Measure login prompt and Netconf time and memsize after reboot."),
    HW_type = get_hw_type(),
    FileName = HW_type++"_meas_reboot_time_memsize.txt",
    measure_reboot(FileName).

ug_meas_reboot_time_memsize(_Config) ->
    ct:pal("UG Measure login prompt and Netconf time and memsize after reboot."),
    HW_type = get_hw_type(),
    FileName = HW_type++"_ug_meas_reboot_time_memsize.txt",
    measure_reboot(FileName).

rbs_meas_fru_restart_cold(_Config) ->
    ct:pal("RBS Measure FRU restart cold, start MW, login prompt, Application, COM and Netconf time and memsize after reboot."),
    HW_type = get_hw_type(),
    FileName = "rbs_"++HW_type++"_meas_fru_restart_cold_time_memsize.txt",
    measure_fru_restart_cold_reboot(FileName, "rbs").

lte_rbs_meas_fru_restart_cold(_Config) ->
    ct:pal("RBS Measure FRU restart cold, start MW, login prompt, Application, COM and Netconf time and memsize after reboot."),
    HW_type = get_hw_type(),
    FileName = "lte_rbs_"++HW_type++"_meas_fru_restart_cold_time_memsize.txt",
    measure_fru_restart_cold_reboot(FileName, "rbs").

tcu_meas_fru_restart_cold(_Config) ->
    ct:pal("TCU Measure FRU restart cold, start MW, login prompt, Application, COM and Netconf time and memsize after reboot."),
    HW_type = get_hw_type(),
    FileName = "tcu_"++HW_type++"_meas_fru_restart_cold_time_memsize.txt",
    measure_fru_restart_cold_reboot(FileName, "rbs").

meas_coli_warm_restart(_Config) ->
    ct:pal("Measure Netconf time after warm restart."),
    HW_type = get_hw_type(),
    FileName = HW_type++"_meas_coli_warm_restart.txt",
    measure_coli_warm_restart(FileName, mw),
    ok.

lte_meas_coli_warm_restart(_Config) ->
    ct:pal("Measure Netconf time after warm restart."),
    HW_type = get_hw_type(),
    FileName = "lte_"++HW_type++"_meas_coli_warm_restart.txt",
    measure_coli_warm_restart(FileName, rbs),
    ok.

tcu_meas_coli_warm_restart(_Config) ->
    ct:pal("Measure Netconf time after warm restart."),
    HW_type = get_hw_type(),
    FileName = "tcu_"++HW_type++"_meas_coli_warm_restart.txt",
    measure_coli_warm_restart(FileName, rbs),
    ok.

ug_meas_coli_warm_restart(_Config) ->
    ct:pal("Measure and Netconf time after warm restart."),
    HW_type = get_hw_type(),
    FileName = "ug_"++HW_type++"_meas_coli_warm_restart.txt",
    measure_coli_warm_restart(FileName, mw),
    ok.

lte_meas_fru_warm_restart(_Config) ->
    ct:pal("Measure Netconf and avli rcs startup time after fru warm restart."),
    HW_type = get_hw_type(),
    FileName = "lte_"++HW_type++"_meas_fru_warm_restart.txt",
    measure_fru_warm_restart(FileName),
    ok.

tcu_meas_fru_warm_restart(_Config) ->
    ct:pal("Measure Netconf and avli rcs startup time after fru warm restart."),
    HW_type = get_hw_type(),
    FileName = "tcu_"++HW_type++"_meas_fru_warm_restart.txt",
    measure_fru_warm_restart(FileName),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
measure_coli_warm_restart(FileName, NodeType) ->
    %% check node is up and running.
    wait_for_netconf_started(),
    ct_netconfc:close_session(nc1),
    ct:pal("Node is up."),
    ct:pal("Start sleep 2 min to make sure everything is written to disc."),
    ct:sleep({minutes,2}),
    ct:pal("End sleep 2 min. Now test can start."),


    ct:pal("### clear avli log.",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rpc:call(rpc, alhI, reset_log, [], 10000, print),
    net_kernel:disconnect(ErlNode),

    ct:pal("### warm restart using coli!",[]),
    %% %% enable_warm_restart_via_coli(),
    %% BoardType = proplists:get_value(
    %% 		  board_type,ct:get_config(
    %% 			       ct:get_config({test_nodes,1}))),
    %% ct:pal("BoardType: ~p", [BoardType]),
    %% case BoardType of
    %% 	BoardType when BoardType == "tcu03";
    %% 		       BoardType == "tcu0401" ->
    %% 	    ok;
    %% 	_Other ->
    %% 	    ct:pal("### warm restart using coli!",[]),
    %% 	    enable_warm_restart_via_coli()
    %% end,
    ok = rct_coli:connect(coli),

    timer:sleep(1000),
    Start1 = os:timestamp(),
    {ok,_} = rct_coli:send(coli,"/board/restart -w"),
    rct_coli:disconnect(coli),
    case NodeType of
    	rbs ->
    	    timer:sleep(10000); %% take some sec before node restart warm.
    	_Otheer ->
    	    timer:sleep(3000)
    end,

    End1 = wait_for_netconf_started(),
    ct_netconfc:close_session(nc1),
    StartNetConfTime = trunc(timer:now_diff(End1, Start1) / 1000 / 1000),
    ct:pal("StartNetConfTime: ~p sec", [StartNetConfTime]),

    %% %% %% %% disable warm restart via coli.
    %% %% disable_warm_restart_via_coli(),
    %% case BoardType of
    %% 	BoardType when BoardType == "tcu03";
    %% 		       BoardType == "tcu0401" ->
    %% 	    ok;
    %% 	_Otheeer ->
    %% 	    disable_warm_restart_via_coli()
    %% end,

    case NodeType of 
	mw ->
	    ct:pal("Will not get Avli Rcs startup time.~n "
		   "only valid for full UPs.", []),
	    RcsAvliTime = no_avli_rcs;
	_Other2 ->
	    RcsAvliTime = get_times_from_avli(),
	    ct:pal("Avli Rcs tot start up time: ~p sec", [RcsAvliTime])
    end,
    
    ct:log("FileName: ~p", [FileName]),
    Branch = meas_lib:get_branch(),
    NewFileName = Branch ++ "_" ++ FileName,
    ct:log("NewFileName: ~p", [NewFileName]),

    CXS_label = get_sw_version(),
    %% CS_CXP = meas_lib:get_cs_cxp(cli),
    BuildLabel = meas_lib:get_build_label(),
    %% check_time(StartNetConfTime, NodeType),
    
    ResultDir_Warm = get_result_dir_warm_restart(),
    write_to_file(NewFileName, 
    		  "~p;~p;~w;~w;~p~n", 
    		  [httpd_util:rfc1123_date(),
    		   CXS_label,
    		   StartNetConfTime,
		   RcsAvliTime,
    		   BuildLabel],
		  ResultDir_Warm),
                  %% ?RESULTDIR_WARM),
    ok.


get_result_dir_warm_restart() ->
      ?RESULTDIR_WARM.
%%    BoardType = 
%%	proplists:get_value(board_type,
%%			    ct:get_config(ct:get_config({test_nodes,1}))),
%%    ct:log("# BoardType # : ~p", [BoardType]),
%%    ResultDir_Warm = case BoardType of
%%			 BOARDTYPE when BOARDTYPE == "tcu03";
%%					BOARDTYPE == "tcu0401";
%%					%% BOARDTYPE == "idu5205";
%%					%% BOARDTYPE == "idu5209";
%%					BOARDTYPE == "dus5201";
%%					BOARDTYPE == "dus3201" ->
%%			     ?RESULTDIR_WARM;
%%			 BOARDTYPE when BOARDTYPE == "dus5301";
%%					BOARDTYPE == "dus3301" ->
%%			     ?RESULTDIR_WARM_WR8_64;
%%			 _Other ->
%%			     ?RESULTDIR_WARM
%%		     end,
%%    ResultDir_Warm.

%% enable_warm_restart_via_coli() ->
%%     ok = rct_coli:connect(coli),
%%     timer:sleep(1000),
%%     {ok,_} = rct_coli:send(coli,"/board/restart -ew"), 
%%     timer:sleep(1000),
%%     rct_coli:disconnect(coli),
%%     ok.

%% disable_warm_restart_via_coli() ->
%%     ok = rct_coli:connect(coli),
%%     timer:sleep(1000),
%%     {ok,_} = rct_coli:send(coli,"/board/restart -dw"), 
%%     timer:sleep(1000),
%%     rct_coli:disconnect(coli),
%%     ok.

write_to_file(FileName, PrintOpts, MeasData, ResultDir) ->
    [{_, NodeName}] = ct:get_config(test_nodes),
    MeasInfo = MeasData++[NodeName],
    ct:pal("MeasInfo: ~p",[MeasInfo]),
    %% insert a ;~w before ~n due to the field NodeName.
    CompletePrintOpts = re:replace(PrintOpts, "~n", ";~w~n", [{return, list}]),
    rct_tlib:
    	writeDataToFile(ResultDir, FileName, CompletePrintOpts, MeasInfo),
    ct:pal("LogFile exist in : ~p , ~nfilename: ~p", [ResultDir, FileName]),
    ok.

get_times_from_avli() ->
    ok = rct_coli:connect(coli),
    {ok,_} = rct_coli:send(coli,"/misc/authlevel disabled"),
    {ok, A} = rct_coli:send(coli,"/log/avli -sl"),
    timer:sleep(1000),
    rct_coli:disconnect(coli),

    ct:pal("A: ~p", [A]),
    B = string:tokens(A," \r\n"),
    ct:pal("B: ~p", [B]),

    SearchList_Down = lists:dropwhile(fun(X) ->
					      X=/="RcsNodeDown" 
				      end, B),
    ct:pal("SearchList_Down: ~p", [SearchList_Down]),
    ["RcsNodeDown",_,TimeDown|_] = SearchList_Down,
    
    SearchList_Up = lists:takewhile(fun(X) ->
					    X=/="Operational" 
				    end, B),
    ct:pal("SearchList_Up: ~p", [SearchList_Up]),
    %% Check that operational is for Rcs
    N = length(SearchList_Up),
    {_List1, ["Operational", "Rcs" | _]} = lists:split(N, B),
    %% Reverse the list to get time stamp for Operational Rcs.
    ["InService", "NodeEvent", TimeUp | _] = 
	lists:reverse(SearchList_Up),
    %% TimeUp = lists:nth(3, lists:reverse(SearchList_Up)),

    ct:pal("TimeDown: ~p", [TimeDown]),
    ct:pal("TimeUp: ~p", [TimeUp]),

    RcsStartUpTime = calc_time_diff(TimeDown, TimeUp),
    ct:pal("RcsStartUpTime: ~p", [RcsStartUpTime]),

    ct:pal("check for RcsTimeChange"),
    TimeJump = 
	%% case CheckFlag of
	case lists:member("RcsTimeChange", B) of
	    %% time_jump ->
	    true ->
		ct:pal("RcsTimeChange exist. Time jump exist"),
		SearchListTimeJump = 
		    lists:dropwhile(fun(X) ->
					    X=/="RcsTimeChange" 
				    end, B),
		ct:pal("SearchListUpDownTimes: ~p", [SearchListTimeJump]),
		["RcsTimeChange" , Date_1, Time_1, Date_2, Time_2 | _] =
		    SearchListTimeJump,
		ct:log("M: ~p", [Date_1]),
		ct:log("N: ~p", [Time_2]),
		
		ct:log("F: ~p", [Date_2]),
		ct:log("G: ~p", [Time_2]),
		
		Time_Jump = calc_time_diff(Time_1, Time_2),
		ct:pal("Time jump: ~p, to be added on startup time.", 
		       [Time_Jump]),
		Time_Jump;
	    _Other ->
		ct:pal("RcsTimeChange Not exist. No time jump"),
		Time_Jump = 0,   %% No need to add or remove time.
		Time_Jump
	end,

    TotStartUpTime = round(RcsStartUpTime + TimeJump),
    ct:log("TotStartUpTime: ~p sec", [TotStartUpTime]),
    TotStartUpTime.


calc_time_diff(TimeA, TimeB) ->
    [Hrs_A, Min_A, Sec_A] = string:tokens(TimeA, ":"),
    [Hrs_B, Min_B, Sec_B] = string:tokens(TimeB, ":"),
    
    A_Millisec = timer:hms(list_to_integer(Hrs_A), 
			   list_to_integer(Min_A), 
			   list_to_integer(Sec_A)),
    ct:pal("A_Millisec: ~p", [A_Millisec]),

    B_Millisec = timer:hms(list_to_integer(Hrs_B), 
			   list_to_integer(Min_B), 
			   list_to_integer(Sec_B)),
    ct:pal("B_Millisec: ~p", [B_Millisec]),

    TimeDiff = (B_Millisec - A_Millisec)/1000, %% millisec to sec.
    ct:pal("Time diff: ~p", [TimeDiff]),
    TimeDiff.


%% check_time(StartNetConfTime, NodeType) ->
%%     case NodeType of
%% 	rbs ->
%% 	    case StartNetConfTime of
%% 		Val when Val < 60 ->
%% 		    ok;
%% 		_ToHigh ->
%% 		    ct:fail("TC will fail, "
%% 			    "StartNetConfTime time is higher than expected.")
%% 	    end;
%% 	_BB ->
%% 	    case StartNetConfTime of
%% 		Val when Val < 30 ->
%% 		    ok;
%% 		_To_High ->
%% 		ct:fail("TC will fail, "
%% 			"StartNetConfTime time is higher than expected.")
%% 	    end
%%     end.

%%--------------------------------------------------------------------
%% @doc
%% @spec create_fru(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
create_fru(_Config) ->
    ct:pal("Config:~n~p~n",[_Config]),
    NodeName = get_node_name(),
    ct:pal("Node:~n~p~n",[NodeName]),
    timer:sleep(90000), %% To ensure node is up.

    ct:pal("Conf: LRAT_basic_fru_netconf_create.xml. ~n"
	   "- FieldReplaceableUnit=1 is created in Equipment=1",[]),
    os:cmd("rcs_exec -m netconf -f /home/etxivri/Kista/rcs_ithub/egenskaper/boan/7_nov/LRAT_basic_fru_netconf_create.xml " ++ NodeName),

    timer:sleep(10000),

    ct:pal("Conf: RBSNC_2_basic_fru_netconf_create_common.xml ~n"
	   "- FieldReplaceableUnit=2 is created in Equipment=1 ~n"
	   "- RiLink=1 is created in Equipment=1 ~n"
	   "- SectorEquipmentFunction=1 is created in ManagedElement=1",[]),
    os:cmd("rcs_exec -m netconf -f /home/etxivri/Kista/rcs_ithub/egenskaper/boan/7_nov/RBSNC_2_basic_fru_netconf_create_common.xml "++ NodeName),

    timer:sleep(10000),
    ok.

%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
get_node_name() ->
    [{_, NodeName}] = ct:get_config(test_nodes),
    Node = atom_to_list(NodeName),
    Node.

%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
measure_reboot(FileName) ->
    measure_reboot(FileName, "dummy").
measure_reboot(FileName, NodeType) ->
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = ct:pal("### reboot using restart_piu_cold!",[]),

    wait_for_node_ready_for_rpc(),

    Start1 = os:timestamp(),
    ok = rct_rpc:call(rpc, appmI, restart_piu_cold, ['_'], 100000, print),
    net_kernel:disconnect(ErlNode),
    {ok,_} = ct_telnet:expect(console, "Restarting system",
    			      [{timeout,30000}, no_prompt_check]), 
    ct:pal("Matched: Restarting system , recieved.", []),
    get_times_and_du_write_to_file(FileName, NodeType, Start1, "reboot"),

    ok.

%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
measure_fru_restart_cold_reboot(FileName, NodeType) ->
    ct:pal("RBS. Measure startup times after trig restart cold using, "
	   "MO FieldReplaceableUnit::restartUnit ."),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),

    rct_cli:connect(cli),
    Start1 = os:timestamp(),
    rct_cli:send(cli,"ManagedElement=1,Equipment=1,FieldReplaceableUnit=1,restartUnit RESTART_COLD PLANNED_RECONFIGURATION 0"),
    rct_cli:disconnect(cli),
    {ok,_} = ct_telnet:expect(console, "Restarting system",
    			      [{timeout,30000}, no_prompt_check]), 
    ct:pal("Matched: Restarting system , received.", []),
    get_times_and_du_write_to_file(FileName, NodeType, Start1, "reboot"),
    ok.

%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
measure_fru_warm_restart(FileName) ->
    ct:pal("RBS. Measure startup times after trig restart war using, "
	   "MO FieldReplaceableUnit::restartUnit ."),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),
    wait_for_netconf_started(),
    ct_netconfc:close_session(nc1),

    %% %% enable_warm_restart_via_coli(),
    %% BoardType = proplists:get_value(
    %% 		  board_type,ct:get_config(
    %% 			       ct:get_config({test_nodes,1}))),
    %% ct:pal("BoardType: ~p", [BoardType]),
    %% case BoardType of
    %% 	BoardType when BoardType == "tcu03";
    %% 		       BoardType == "tcu0401" ->
    %% 	    ok;
    %% 	_Other ->
    %% 	    enable_warm_restart_via_coli()
    %% end,
    rct_rs232:login(console),
    ct:pal("Node is up."),
    ct:pal("Start sleep 2 min to make sure everything is written to disc."),
    ct:sleep({minutes,2}),
    ct:pal("End sleep 2 min. Now test can start."),
    ct:pal("### clear avli log.",[]),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rpc:call(rpc, alhI, reset_log, [], 10000, print),
    clear_llog(),
    net_kernel:disconnect(ErlNode),
    timer:sleep(5000),

    rct_cli:connect(cli),    
    Start1 = os:timestamp(),
    ct:pal("Start fru restart Warm using cli."),
    rct_cli:send(cli,"ManagedElement=1,Equipment=1,FieldReplaceableUnit=1,restartUnit RESTART_WARM PLANNED_RECONFIGURATION 0"),
    rct_cli:disconnect(cli),
    ct:pal("cli cmd sent."),
    %% timer:sleep(10000), %% take some sec before node restart warm.
    wait_for_warm_exist_in_llog(),
    End1 = wait_for_netconf_started(),
    ct:pal("Netconf up!"),
    ct_netconfc:close_session(nc1),

    StartNetConfTime = trunc(timer:now_diff(End1, Start1) / 1000 / 1000),
    ct:pal("StartNetConfTime: ~p sec", [StartNetConfTime]),

    %% %% disable_warm_restart_via_coli(),
    %% case BoardType of
    %% 	BoardType when BoardType == "tcu03";
    %% 		       BoardType == "tcu0401" ->
    %% 	    ok;
    %% 	_Otheer ->
    %% 	    disable_warm_restart_via_coli()
    %% end,

    RcsAvliTime = get_times_from_avli(),
    ct:pal("Avli Rcs tot start up time: ~p sec", [RcsAvliTime]),

    ct:log("FileName: ~p", [FileName]),
    Branch = meas_lib:get_branch(),
    NewFileName = Branch ++ "_" ++ FileName,
    ct:log("NewFileName: ~p", [NewFileName]),

    CXS_label = get_sw_version(),
    %% CS_CXP = meas_lib:get_cs_cxp(cli),
    BuildLabel = meas_lib:get_build_label(),

    ResultDir_Warm = get_result_dir_warm_restart(),
    write_to_file(NewFileName, 
    		  "~p;~p;~w;~w;~p~n", 
    		  [httpd_util:rfc1123_date(),
    		   CXS_label,
    		   StartNetConfTime,
		   RcsAvliTime,
    		   BuildLabel], 
		  ResultDir_Warm),
    		  %% ?RESULTDIR_WARM),

    ok.

clear_llog() ->
    ct_telnet:send(console, "llog -c"),
    timer:sleep(1000),
    ct_telnet:send(console, "llog -c"),

    Check_llog = rct_rpc:call(rpc, os, cmd, [llog], 10000, print),
    case re:run(Check_llog, "Warm") of
    	nomatch ->
    	    ok;
    	{match, _} ->
    	    ct:fail("llog not cleared. Better to fail TC!")
    end.

wait_for_warm_exist_in_llog() ->
    wait_for_warm_exist_in_llog(60000).
wait_for_warm_exist_in_llog(Timeout) when Timeout < 0 ->
    ct:fail("Warm not exist in llog within expected timeout");
wait_for_warm_exist_in_llog(Timeout) ->
    Check_llog = rct_rpc:call(rpc, os, cmd, ["llog"], 10000, print),
    case re:run(Check_llog, "Warm") of
	{match, _} -> 
	    ct:pal("Now Warm exist in llog.");
	_Res -> 
	    ct:log(" Warm not exist in llog, sleep and check again. ~n~p",
		   [_Res]),
	    timer:sleep(2000), 
	    wait_for_warm_exist_in_llog(Timeout-2000)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Measure Login and Netconf time and memory size after power on.<br/>
%% @spec meas_power_time_memsize(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
meas_power_time_memsize(_Config) ->
    ct:pal("Measure login prompt and Netconf time and memsize after power on."),
    HW_type = get_hw_type(),
    FileName = HW_type++"_meas_power_time_memsize.txt",
    measure_power(FileName).

ug_meas_power_time_memsize(_Config) ->
    ct:pal("UG Measure login prompt and Netconf time and memsize after power on."),
    HW_type = get_hw_type(),
    FileName = HW_type++"_ug_meas_power_time_memsize.txt",
    measure_power(FileName).

lte_rbs_meas_power(_Config) ->
    ct:pal("RBS Measure start MW, login prompt, COM, Application and Netconf time and memsize after power on."),
    HW_type = get_hw_type(),
    FileName = "lte_rbs_"++HW_type++"_meas_power_time_memsize.txt",
    measure_power(FileName, "rbs").

tcu_meas_power(_Config) ->
    ct:pal("TCU Measure start MW, login prompt, COM, Application and Netconf time and memsize after power on."),
    HW_type = get_hw_type(),
    FileName = "tcu_"++HW_type++"_meas_power_time_memsize.txt",
    measure_power(FileName, "rbs").

measure_power(FileName) ->
    measure_power(FileName, "dummy").
measure_power(FileName, NodeType) ->
    ct:pal("sleep 35 sec"),
    ct:sleep({seconds, 35}),
    rct_logging:get_all(log1), %% To get all erl log after power
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    net_kernel:disconnect(ErlNode),

    ct:pal("### power off/on!",[]),
    ok = rct_power:off(node),
    Start1 = wait_for_power_on(),
    get_times_and_du_write_to_file(FileName, NodeType, Start1, "power"),
    ct:pal("### sleep 2min to make sure is node is synched and up.",[]),
    timer:sleep(120000), %% To make sure data is synced before next tc runs.
    ok.


%% ===========================================================================
%% @doc
%% Measure Netconf time and memory size after initial restart.<br/>
%% @spec meas_init_restart_time_memsize(_Config) -> ok
%% @end
%% ===========================================================================
meas_init_restart_time_memsize(_Config) ->
    ct:pal("Measure Netconf time and memsize after initial restart."),
    %% FileName = get_filename("meas_init_restart_time_memsize.txt"),
    HW_type = get_hw_type(),
    FileName = HW_type++"_meas_init_restart_time_memsize.txt",
    measure_initial_restart(FileName).

ug_meas_init_restart_time_memsize(_Config) ->
    ct:pal("UG Measure Netconf time and memsize after initial restart."),
    HW_type = get_hw_type(),
    FileName = HW_type++"_ug_meas_init_restart_time_memsize.txt",
    measure_initial_restart(FileName).

rbs_meas_init_restart(_Config) ->
    ct:pal("RBS Measure start COM, Application and Netconf time and memsize after initial restart."),
    HW_type = get_hw_type(),
    FileName = "rbs_"++HW_type++"_meas_init_restart_time_memsize.txt",
    measure_initial_restart(FileName, "rbs").

measure_initial_restart(FileName) ->
    measure_initial_restart(FileName, "dummy").
measure_initial_restart(FileName, NodeType) ->
    %%%%
    %% Get COM pid.
    %%%%
    Com = rct_rpc:call(rpc, os, cmd, ["pgrep -f com/bin/com -u $USER"], 20000),
    ct:pal("COM: ~p", [Com]),
    ComPid = case re:run(Com, "^[0-9]+$", [{capture,[1],list}]) of
    		 {match, _Result} ->
    		     Com;
    		 _ ->
    		     ct:fail("No com pid found")
    end,

    %%%%
    %% Perform a initial restart.
    %%%%
    ct:pal("perform init restart",[]),
    Start1 = os:timestamp(),
    rct_rpc:call(rpc, init, restart, [], 1000),

    wait_for_com_restarted(ComPid),

    get_times_and_du_write_to_file(FileName, NodeType, Start1, "init_restart"),

    ok.

%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
get_times_and_du_write_to_file(FileName, NodeType, Start1, Action) ->
    ct:pal("NodeType: ~p",[NodeType]),

    case Action of
	"init_restart" ->
	    StartLogInTime = '-',
	    ok;
	_Other -> %% reboot and power.
            %%%%
	    %% Measure time for login prompt.
            %%%%
	    End2 = wait_for_login_prompt(),
	    case End2 of
		not_valid ->
		    StartLogInTime = "-";
		_ValidTime ->
		    StartLogInTime = trunc(timer:now_diff(End2, Start1) / 1000 / 1000)
	    end
    end,

    %%%%
    %% Measure startup time for Netconf
    %%%%
    End5 = wait_for_netconf_started(),
    ok = ct_netconfc:close_session(nc1),
    StartNetConfTime = trunc(timer:now_diff(End5, Start1) / 1000 / 1000),

    ct:pal("StartUp times:  LogIn: ~w,  Netconf: ~w~n", 
    	   [
    	    StartLogInTime, 
	    StartNetConfTime
    	   ]),

    %%%%
    %% Get used memory size
    %%%%
    [Tot_bc, Beam_rss, Com_rss] = get_used_memsize(),

    %%%%
    %% Get disc usage
    %%%%
    [SW_DiscUsage, RCS_DiscUsage , COM_DiscUsage] = get_disc_usage(),

    %%%%
    %% Get sw version, using cli
    %%%%
    CXS_label = get_sw_version(),

    %%%%
    %% Update log measurement file
    %%%% 
    %% CS_CXP = meas_lib:get_cs_cxp(cli),
    BuildLabel = meas_lib:get_build_label(),
    Branch = meas_lib:get_branch(),

    NewFileName = Branch ++ "_" ++ FileName,
    updateMeasResFile(NewFileName, "~p;~p;~w;~w;~s;~s;~s;~s;~s;~s;~p~n", 
		      [httpd_util:rfc1123_date(),
		       CXS_label,
		       StartLogInTime, 
		       StartNetConfTime,
		       Com_rss,
		       Beam_rss,
		       Tot_bc,
		       SW_DiscUsage, 
		       RCS_DiscUsage , 
		       COM_DiscUsage,
		       BuildLabel]),
	    
    ResultDir = get_result_dir(),
    ct:pal("results file: ~p ,\n path: ~p \n",[NewFileName, ResultDir]),
    %% ct:pal("results file: ~p ,\n path: ~p \n",[NewFileName, ?RESULTDIR]),

    ct:log("Action: ~p ", [Action]),

    ct:pal("Memsize: Com_rss: ~s,  Beam_rss: ~s,  Tot_bc: ~s ~n", 
	   [Com_rss,
	    Beam_rss,
	    Tot_bc]),

    ct:pal("Disc Usage: SW: ~s,  RCS: ~s,  Com: ~s ~n", 
    	   [SW_DiscUsage, 
	    RCS_DiscUsage , 
	    COM_DiscUsage]),

    ok.


%% ===========================================================================
%% @doc
%% Wait for power on. <br/>
%% Start timestamp when a power on. <br/>
%% Sometimes power on does not take affect due to consolserver. <br/>
%% Then a new power off/on will be done. <br/>
%% @spec wait_for_power_on() -> timestamp
%% @end
%% ===========================================================================
wait_for_power_on() ->
     ct:pal("### Start Power On.", []),
     wait_for_power_on(60000).

wait_for_power_on(Timeout) when Timeout < 0 ->
    ct:fail("Power ON failed within expected time.");
wait_for_power_on(Timeout) ->
    case wait_for_power_on_2() of
	{ok, SecondStageUboot} ->
	    Start = os:timestamp(),
	    case check_node_is_starting(SecondStageUboot) of
		ok ->
		    Start;
		{_Error, Time} ->
		    wait_for_power_on(Timeout - Time)
	    end;
	{_Error, Time} ->
	    wait_for_power_on(Timeout - Time)
    end.

wait_for_power_on_2() ->
     case  rct_power:on(node, no_retries) of
	ok ->
	    %% Check that node has restarted. If no try to restart again
	    BoardType = 
		 proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))),
	     SecondStageUboot = case BoardType of
				    BoardType when BoardType == "dus4101";
						   BoardType == "duw4101" ->
					"2:nd Stage Boot Loader";
				    _ ->
					"Ericsson Version:" % tcu03, dus5201, dus3201?
				end,
	     ct:log("SecondStageUboot: ~p", [SecondStageUboot]),
	     {ok, SecondStageUboot};
	 {error,econnrefused} = Error ->
	    timer:sleep(250), % If occupied then wait and try again.
	     {Error, 250};
	 Return ->
	     ct:pal("### Power ON failed, due to: ~w", [Return]),
	     ct:fail("Power ON failed.")
     end.

check_node_is_starting(SecondStageUboot) ->
    case ct_telnet:expect(console,
			  SecondStageUboot,
			  [{timeout, 20000}, no_prompt_check]) of
	{ok, _} ->
	    ok;
	{error, timeout} = Error ->
	    ct:pal("Power on failed within expected time, do power off/on again.",[]),
	    rct_power:cycle(node),
	    {Error, 20000};
	Res ->
	    ct:pal("### Res: ~w", [Res]),
	    ct:fail("Power ON failed, du to unexpected return value from power on.")
     end.

%% ===========================================================================
%% @doc
%% Check for login prompt. <br/>
%% Wait for login prompt arrives in the consol printouts. <br/>
%% @spec wait_for_login_prompt() -> timestamp
%% @end
%% ===========================================================================
wait_for_login_prompt() ->
    ct:pal("Wait for login prompt.",[]),
    case ct_telnet:expect(console, "login:",
			  [{total_timeout,90000}, {idle_timeout,95000} , no_prompt_check]) of
	{ok, _} ->
	    End = os:timestamp(),
	    ct:pal("Rcvd login prompt. OK.",[]),
	    End;
	_Err ->
	    ct_telnet:send(console, []),
	    case ct_telnet:expect(console, "login:", 
				  [{total_timeout,5000}, no_prompt_check]) of
		{ok, _} -> 
		    ct:pal("End Time is not valid and shall not be used.",[]),
		    not_valid;
		_Fail  ->
		    ct:pal("TC will fail due to no login prompt rcvd: ~p ", [_Fail]),
		    ct:fail("TC will fail due to no login prompt rcvd.", [])
	    end
    end.

%% ===========================================================================
%% @doc
%% Check for COM restarted. <br/>
%% Wait for COM process to be started again with a new pid. <br/>
%% @spec wait_for_com_restarted(ComPid) -> timestamp
%% @end
%% ===========================================================================
wait_for_com_restarted(ComPid) ->
    wait_for_com_restarted(ComPid, 120000).

wait_for_com_restarted(_ComPid, Timeout) when Timeout < 500 ->
    ct:fail("COM not restarted within max timeout after restart.");

wait_for_com_restarted(ComPid, Timeout) ->
    case rct_rpc:call(rpc, os, cmd, ["pgrep -f -u $USER com/bin/com"], 1000)  of
    	{badrpc, _} -> %{badrpc, timeout | nodedown}
	    timer:sleep(1000),
	    wait_for_com_restarted(ComPid, Timeout - 1000);
	[] ->	
	    timer:sleep(1000),
	    wait_for_com_restarted(ComPid, Timeout - 1000);
	Data ->
    	    case re:run(Data, "^[0-9]+$", [{capture,[1],list}]) of
    		{match, _} ->
		    case ComPid == Data of
			true ->
			    %% ct:pal("com has not restarted yet!", []),
			    timer:sleep(1000),
			    wait_for_com_restarted(ComPid, Timeout - 1000);
			false -> %% Com is restarted with new Pid.
			    %% End
			    ok
		    end;
		_Rec ->
		    timer:sleep(1000),
		    wait_for_com_restarted(ComPid, Timeout - 1000)
    	    end
    end.

%% ===========================================================================
%% @doc
%% Check for Netconf to be started. <br/>
%% Wait for ct_netconfc:get_config... returns  ok. <br/>
%% @spec wait_for_netconf_started() -> timestamp
%% @end
%% ===========================================================================
wait_for_netconf_started() ->
    wait_for_netconf_started(120000).

wait_for_netconf_started(Timeout) when Timeout < 500 ->
    ct:fail("Netconf not started within max timeout after restart.");

wait_for_netconf_started(Timeout) ->
    %% ct:pal("Poll start nc"), 
    %% test_server:break("break"),
    case ct_netconfc:open(nc1,[]) of
    %% case ct_netconfc:open(nc1,[{timeout, 500}]) of
	{ok,_} ->
	    %% {ok,_} = ct_netconfc:get_config(nc1,running,{'ManagedElement',
	    %% 						 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
	    %% 						 [{managedElementId,[],["1"]}]}),
	    End = os:timestamp(),
	    ct:pal("netconf open - ok.",[]),
	    End;
	{error,{ssh,could_not_connect_to_server,econnrefused}} ->
	    timer:sleep(250),
	    wait_for_netconf_started(Timeout - 250);
	_Other ->
	    %% ct:log("Poll start nc: ~p", [_Other]),
	    timer:sleep(250),
	    wait_for_netconf_started(Timeout - 250)
    end.

%% ===========================================================================
%% @doc
%% Check used RAM memory size. <br/>
%% Get memory size from, os:cmd "free" and "ps -eo fname,vsz,rss | egrep '(beam)|(com)'". <br/>
%% @spec get_used_memsize() -> int
%% @end
%% ===========================================================================
get_used_memsize() ->
    ct:pal("### Get Used MemSize",[]),
    Free = rct_rpc:call(rpc, os, cmd, ["free"], 30000, noprint),
    ct:log("Free: ~p. ",[Free]),

    Free1 = string:tokens(Free,"\n"),
    ct:log("Free: ~p. ",[Free1]),
    [MemUsed] = [A||A<-Free1, string:str(A,"Mem:") == 1],
    ct:log("MemUsed: ~p. ",[MemUsed]),
    %% [BuffersCache] = [S||S<-Free1, string:str(S,"-/+ buffers/cache:") == 1],
    %% ct:log("BuffersCache: ~p. ",[BuffersCache]),

    %%%%
    %% Total used RAM size
    %%%%
    %% Tot = list_to_integer(lists:nth(3,string:tokens(MemUsed," "))),
    %% ct:log("Used: ~p. ",[Tot]),
    %% Tot_bc = list_to_integer(lists:nth(3,string:tokens(BuffersCache," "))),
    Tot_bc = list_to_integer(lists:nth(6,string:tokens(MemUsed," "))),
    ct:log("Tot_bc: ~p. ",[Tot_bc]),

    PS = rct_rpc:call(rpc, os, cmd, ["ps -eo fname,vsz,rss | egrep '(beam)|(com)'"], 30000, print),
    %%%%
    %% Get used RAM size beam uses. 
    %%%%
    %% BeamPid = get_valid_beam_pid(),
    BeamPid = rct_rpc:call(rpc, os, getpid, [], 30000, noprint),
    ct:log("BeamPid: ~p. ",[BeamPid]),
    Beam_Rss_Data = rct_rpc:call(rpc, os, cmd, ["ps -o fname,rss -p "++BeamPid], 30000, print),
    ct:log("Beam_Rss_Data: ~p. ",[Beam_Rss_Data]),
    BeamRssData = string:tokens(Beam_Rss_Data," \n"),
    ct:log("BeamRssData: ~p. ",[BeamRssData]),
    Beam_rss = lists:last(BeamRssData),
    ct:log("Beam_rss: ~p. ",[Beam_rss]),

    %%%%
    %% Get used RAM size com uses. 
    %%%%
    %% {match,[_Com_vsz,Com_rss]} = re:run(PS,"com *([0-9]+) *([0-9]+).*",[{capture,[1,2],list}]),
    case re:run(PS,"com *([0-9]+) *([0-9]+).*",[{capture,[1,2],list}]) of
	{match,[_Com_vsz,Com_rss]} ->
	    ok;
	_Other ->
	    _Com_vsz = "-",
	    Com_rss = "-"
    end,
    ct:log("_Com_vsz: ~p. Com_rss: ~p",[_Com_vsz,Com_rss]),

    [integer_to_list(Tot_bc), Beam_rss, Com_rss].


%% ===========================================================================
%% @doc
%% Get disc usage of directories. <br/>
%% SW: /software/ <br/>
%% RCS: /software/RCS_CXP9021221*
%% COM: /software/RCS_CXP9021221*/COM_CXC1733991*/
%% @spec get_disc_usage() -> string
%% @end
%% ===========================================================================
get_disc_usage() ->
    %% SW_du = rct_rpc:call(rpc, os, cmd, ["du -sh /software/"], 10000, noprint), 
    %% get disc usage size.
    %% [SW_DISC_USAGE|_] = string:tokens(SW_du, "M\t"),
    SW_DISC_USAGE = "-",
    BoardType = proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))),
    ct:log("BoardType: ~p", [BoardType]),
    %% RCS_CXP = get_rcs_cxp(BoardType),
    %% ct:log("RCS_CXP: ~p", [RCS_CXP]),
    RCS_CXP = "RCSMW-ARM_CXP*",
    RCS_du = rct_rpc:call(rpc, os, cmd, ["du -sh /software/"++RCS_CXP], 10000, print), 
    [RCS_DISC_USAGE_1|_] = string:tokens(RCS_du, "M\t"),
    RCS_DISC_USAGE = "RCSMW: "++RCS_DISC_USAGE_1,

    %% case RCS_CXP of
    %% 	"RCS_CXP9021221*" ->
    %% 	    RCS_DISC_USAGE = RCS_DISC_USAGE_1;
    %% 	"RCS-ARM_CXP*" -> %% Not needed when dus52 only use wr6
    %% 	    RCS_DISC_USAGE = RCS_DISC_USAGE_1;
    %% 	_ ->
    %% 	    RCS_DISC_USAGE = "RCSMW: "++RCS_DISC_USAGE_1
    %% end,

    COM_CXC = get_com_cxc(BoardType),
    ct:log("COM_CXC: ~p", [COM_CXC]),
    COM_du = rct_rpc:call(rpc, os, cmd, ["du -sh /software/"++RCS_CXP++"/"++COM_CXC++"/"], 10000, print), 
    [COM_DISC_USAGE|_] = string:tokens(COM_du, "M\t "),
    [SW_DISC_USAGE, RCS_DISC_USAGE , COM_DISC_USAGE].

%% get_rcs_cxp(BoardType) ->
%%     ct:pal("BoardType: ~p", [BoardType]),
%%     RCS_CXP = case BoardType of
%% 		  BoardType when BoardType == "dus4101";
%% 				 BoardType == "duw4101" ->
%% 		      "RCS_CXP9021221*";
%% 		  "dus5201" -> %% Not needed when dus52 only use wr6
%% 		      case  check_if_wr5_or_wr6_is_used() of
%% 			  wr5 ->
%% 			      "RCS-ARM_CXP*";
%% 			  wr6 ->
%% 			      "RCSMW-ARM_CXP*"
%% 		      end;
%% 		  _ ->  %% tcu03
%% 		      %% "RCS-ARM_CXP9021221*" % tcu03, dus5201, dus3201?
%% 		      "RCSMW-ARM_CXP*" % RCSMW and RCSEE is separate cxps
%% 	      end,
%%     RCS_CXP.

%% check_if_wr5_or_wr6_is_used() -> %% Not needed when dus52 only use wr6
%%     A = rct_rpc:call(rpc, os, cmd, ["du -sh /software/RCSMW-*"], 10000, 
%% 			noprint), %% When wr6 is used then RCSMW is used.
%%     ct:log("A: ~p", [A]),
%%     B = string:tokens(A, "`/\n"),
%%     ct:log("B: ~p", [B]),
%%     WR = case lists:member("du: cannot access ", B) of
%% 	     true ->
%% 		 ct:log("wr5 is used.", []),
%% 		 wr5;
%% 	     false ->
%% 		 ct:log("wr6 is used.", []),
%% 		 wr6
%% 	 end,
%%     WR.

get_com_cxc(BoardType) ->
    COM_CXC = case BoardType of
		  BoardType when BoardType == "dus4101";
				 BoardType == "duw4101" ->
		      "COM_CXC1733991*";
		  _ ->
		      "COM3_CXC1733991*" % tcu03, dus5201, dus3201?
	      end,
    COM_CXC.

%% ===========================================================================
%% @doc
%% Get CXS label. <br/>
%% Get SW version using COM cli interface. <br/>
%% @spec get_sw_version() -> 'CXS_LABEL'
%% @end
%% ===========================================================================
get_sw_version() ->
    ct:pal("### Get SW version",[]),
    check_cli_is_ready_to_be_used(),
    timer:sleep(10000),
    CXS = wait_for_rcvd_data(60000),
    ct:pal("CXS: ~p", [CXS]),
    list_to_atom(CXS).


wait_for_rcvd_data(Timeout) when Timeout < 0 ->
    "undefined";
wait_for_rcvd_data(Timeout) ->
    rct_cli:connect(cli),
    case rct_cli:send(cli,"show ManagedElement=1,SystemFunctions=1,SwInventory=1") of
	{ok , ReceivedData} ->
	    ct:log("ReceivedData: ~p", [ReceivedData]),
	    Var = string:tokens(ReceivedData, "=\r\n "),
	    case lists:dropwhile(fun(X) ->
					 X =/= "SwVersion" 
				 end, Var) of
		[_, CXS | _ ] -> 
		    rct_cli:disconnect(cli),
		    CXS;
		_Else ->
		    rct_cli:disconnect(cli),
		    ct:pal("SwVersion not found:  ~p, ~n"
			   "sleep and try again.", [_Else]),
		    timer:sleep(5000),
		    wait_for_rcvd_data(Timeout-5000)
	    end;
	_Other ->
	    rct_cli:disconnect(cli),
	    ct:log("No valid data rcvd: ~p, set undefined.", [_Other]),
	    "undefined"
    end.


check_cli_is_ready_to_be_used() ->
    check_cli_is_ready_to_be_used(120000).
check_cli_is_ready_to_be_used(Timeout) when Timeout < 0 ->
    ct:fail("Cli is not ready to be used within max timeout.");
check_cli_is_ready_to_be_used(Timeout) ->
    case rct_cli:connect(cli) of
	ok -> 
	    rct_cli:disconnect(cli),
	    timer:sleep(5000);
	_Other ->
	    timer:sleep(5000),
	    check_cli_is_ready_to_be_used(Timeout-5000)
    end.

%% @hidden
%% ===========================================================================
%% @doc
%% For each testcase to use for writing testcase results and/or store measurement results <br/>
%% into a common result file. <br/>
%% @spec updateMeasResFile(FileName, PrintOpts, MeasData) -> ok
%% @end
%% ===========================================================================
updateMeasResFile(FileName, PrintOpts, MeasData) ->
    [{_, NodeName}] = ct:get_config(test_nodes),
    MeasInfo = MeasData++[NodeName],
    ct:pal("MeasInfo: ~p",[MeasInfo]),
    %% insert a ;~w before ~n due to the field NodeName.
    CompletePrintOpts = re:replace(PrintOpts, "~n", ";~w~n", [{return, list}]),
    
    ResultDir = get_result_dir(),
    rct_tlib:
    	%% writeDataToFile(?RESULTDIR, FileName, CompletePrintOpts, MeasInfo),
	writeDataToFile(ResultDir, FileName, CompletePrintOpts, MeasInfo),

    ok.

get_result_dir() ->
      ?RESULTDIR.
%%    BoardType = 
%%	proplists:get_value(board_type,
%%			    ct:get_config(ct:get_config({test_nodes,1}))),
%%    ResultDir = case BoardType  of
%%		    BOARDTYPE when BOARDTYPE == "tcu03";
%%				   BOARDTYPE == "tcu0401";
%%				   %% BOARDTYPE == "idu5205";
%%				   %% BOARDTYPE == "idu5209";
%%				   BOARDTYPE == "dus5201";
%%				   BOARDTYPE == "dus3201" ->
%%			?RESULTDIR;
%%		    BOARDTYPE when BOARDTYPE == "dus5301";
%%				   BOARDTYPE == "dus3301" ->
%%			?RESULTDIR_WR8_64;
%%		    _Other ->
%%			?RESULTDIR
%%		end,
%%    ResultDir.


%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% get_hw_type() ->
%%     BoardType = proplists:get_value(board_type,ct:get_config(ct:get_config({test_nodes,1}))),
%%     ct:log("# BoardType: ~p", [BoardType]),

%%     Board_Type = case BoardType of
%% 		     "dus5201" ->
%% 			 case meas_lib:check_if_cxs_is_bpu() of
%% 			     "bpu" ->
%% 			 	 "bpu";
%% 			     _Other ->
%% 			 	 BoardType
%% 			 end;
%% 		     _Otheeer ->
%% 			 BoardType
%% 		 end,
%%     ct:log("# Board_Type: ~p", [Board_Type]),
%%     Board_Type.

get_hw_type() ->
    BoardType = 
	proplists:get_value(board_type,
			    ct:get_config(ct:get_config({test_nodes,1}))),
    ct:log("# BoardType: ~p", [BoardType]),

    ProductType = 
	case BoardType of
	    BOARDTYPE when BOARDTYPE == "dus5201";
			   BOARDTYPE == "dus5201tk";
			   BOARDTYPE == "dus3201tk";
			   BOARDTYPE == "dus5301";
			   BOARDTYPE == "dus3301" ->
		%% "dus5201" ->
		ProductOnBoard = meas_lib:check_product_on_board(),
		case ProductOnBoard of
		    "no_match" ->  %% dus52 
			BoardType;
		    _Other -> %% could be used for differents products.
			ProductOnBoard	 
		end;
	    _Otheeer -> %% Just BoardType
		BoardType
	end,

    ct:pal("# BoardType: ~p, Product on board: ~p", [BoardType, ProductType]),
    ProductType.

%% check_if_cxs_is_bpu() ->
%%     CXS = ct:get_config({jenkins_config, cxp}),
%%     ct:log("CXS: ~p", [CXS]),
%%     Board_Type = case re:run(CXS, "CXS101665_3") of
%% 		     {match, _} ->
%% 			 "bpu";
%% 		     _Other->
%% 			 "no_bpu"
%% 		 end,
%%     Board_Type.

%% ===========================================================================
%% ===========================================================================
wait_for_node_ready_for_rpc() ->
    wait_for_node_ready_for_rpc(30000).
wait_for_node_ready_for_rpc(Timeout) when Timeout < 0 ->
    ct:fail("Node not up within max timeout after restart.");
wait_for_node_ready_for_rpc(Timeout) ->
    case rct_rpc:call(rpc, os, cmd, ["ls"], 1000, noprint) of
	%% case rct_rpc:call(rpc, os, cmd, ["ls"], 1000, print) of
	{badrpc,nodedown} ->
	    timer:sleep(5000),
	    wait_for_node_ready_for_rpc(Timeout-5000);
	_Other ->
	    ct:log("Node is up and it is ok to use rps: ~p", [_Other]),
	    ok
    end.
