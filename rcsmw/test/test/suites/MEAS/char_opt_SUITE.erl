%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	char_opt_SUITE.erl %
%%% @author etxivri
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R3A/R4A/R5A/R6A/R7A/R9A/R10A/R12A/1
%%% 
%%% @doc ==Measure times for characteristics optimization.==  
%%% <br/><br/>
%%% 
%%% @end


-module(char_opt_SUITE).
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
%%% R3A/2      2014-11-07 etxivri     Created
%%% R3A/3      2014-11-12 etxivri     Update to be more generic and added
%%%                                   Conf Cell MO.
%%% R3A/5      2014-11-13 etxivri     Change how to trig reboot.
%%% R3A/6      2014-11-13 etxivri     Update due to edoc error.
%%% R3A/7      2014-11-18 etxivri     Update so restart_cold runs in jenkins.
%%% R3A/8      2014-11-18 etxivri     Added consol time for rsc_start.
%%% R3A/8      2014-11-18 etxivri     Added otp_start time.
%%% R3A/10     2014-11-19 etxivri     Change how to trig reboot.
%%%                                   Adde trig restart FieldReplaceableUnit.
%%%                                   Changed hardcoded node to be when config
%%%                                   node is used.
%%% R3A/12     2014-11-24 etxivri     Update to check after appm_complete 
%%%                                   when get startup times. 
%%%                                   Add full restart time.
%%% R3A/14     2014-11-26 etxivri     Add application start time.
%%% R3A/16     2014-12-01 etxivri     Use SwInventory when get sw version.
%%% R3A/16     2014-12-03 etxivri     Add groups to be used in our pre dc.
%%% R3A/17     2014-12-03 etxivri     Added close nc session to cleanup.
%%% R3A/19     2014-12-04 etxivri     More update when measure time to get idle
%%% R3A/20     2014-12-08 etxivri     Added create,remove cell 1-6 and 7-12. 
%%% R3A/21     2014-12-09 etxivri     Increased threshold for idle cpu to 90%..
%%% R3A/22     2014-12-09 etxivri     changed idle cpu threshold to 85%
%%% R3A/26     2014-12-17 etxivri     Add good to have stuff. and 12cell create.
%%% R3A/27     2014-12-18 etxivri     Some cleanup, Add write CS CXP in file.
%%% R3A/28     2014-12-18 etxivri     Add a check if mw_start time is to high.
%%%                                   Then TC will fail.
%%% R3A/29     2014-12-18 etxivri     Minor correction.
%%% R3A/30     2014-12-22 etxivri     Update pre_conf.
%%% R3A/31     2015-03-09 etxivri     Temporary increased max time on mw_start.
%%% R3A/32     2015-03-13 etxivri     Add chek that node restarts.
%%% R3A/33     2015-04-10 etxivri     Update get_filename
%%% R4A/1      2015-04-27 etxivri     Changed RESULTDIR for R4.
%%% R4A/2      2015-04-28 etxivri     Update RESULTDIR for R4 and file name.
%%% R4A/3      2015-07-06 etxivri     Minor update for tcu0401.
%%% R4A/4      2015-07-13 etxjovp     Add group definitions used by CS CI
%%% R4A/5      2015-07-15 etxjovp     modify group definitions used by CS CI
%%% R4A/7      2015-09-08 etxivri     Increased time in check of mw_start. 
%%% R4A/8      2015-09-09 etxivri     Create new meas for plot from xl predc.
%%% R4A/9      2015-09-24 etxivri     Update to ensure cpu is idle.
%%% R4A/10     2015-09-30 etxivri     Changed now() to os:timestamp().
%%% R5A/1      2015-10-07 etxivri     Update for R5
%%% R5A/2      2015-10-13 etxivri     Updates with changes from R4, to make it 
%%%                                   more robust.
%%% R5A/3      2015-11-12 etxivri     Update due to changed behaviour in com.
%%% R5A/4      2015-11-13 etxivri     Change restart check to use full_restart.
%%%                                   Update how to get times. And removed some
%%%                                   measurepoints due to they are depricated
%%%                                   in OTP 18.
%%% R5A/5      2016-01-11 etxivri     bugfix when when write meas data in night.
%%% R5A/6      2016-04-28 etxivri     Update for R6
%%% R7A/1      2016-06-07 etxivri     Update for R7 WR8
%%% R9A/1      2017-04-11 erarube     Update result directory for R9
%%% R10A/1     2017-04-11 erarube     Update result directory for R10
%%% R12A/1     2017-10-25 etxivri     Update log dir to be a generic dir R_XY there everything will be stored.
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
	 wait_for_login_prompt/1,
	 wait_for_netconf_started/1,
	 get_sw_version/0,
	 groups/0,
	 all/0,
	 trig_restart_cold/1,
	 rbs_trig_restart_cold/1,
	 get_startup_times/1,
	 get_startup_times_cold/1,
	 lte_rbs_get_startup_times_cold/1,
	 tcu_get_startup_times_cold/1,
	 get_startup_times_cold_test/1,
	 create_fru/1,
	 pre_conf/1,
	 create_1_EUtranCellFDD_MO/1,
	 create_6_EUtranCellFDD_MO/1,
	 create_12_EUtranCellFDD_MO/1,
	 remove_1_EUtranCellFDD_MO/1,
	 remove_6_EUtranCellFDD_MO/1,
	 remove_12_EUtranCellFDD_MO/1,
	 get_time_rcs_start/0,
	 get_rbs_cs_cxps/0,
	 time_to_get/1,
	 get_eq_element/1,
	 get_me_element/1,
	 check_top_after_restart/1,
	 lte_night_get_meas_fru_cold_data/1,
	 lte_write_night_graph_data/1
	]).

-export([ets_new/0]).

-define(RESULTDIR, "/proj/rcs/measurements/char_opt/R_XY_char_opt/").
%% %% -define(RESULTDIR, "/proj/rcs/measurements/tory/meas_tmp_1/").
%% -define(RESULTDIR, "/home/etxivri/tmp/meas").

-define(NIGHT_RESULTDIR, "/proj/rcs/measurements/plot/night_pre_dc_r10/all/").
-define(NIGHT_GRAPH_FRU_RESTART, "/proj/rcs/measurements/plot/night_pre_dc_r10/").


-define(NC_Sess, nc1).
-define(Cli_Sess, cli).

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
						"CRASH REPORT"],[]}}]}},
                 {rct_rs232,console},
		 {rct_cli, {cli, [manual_connect]}},
		 {rct_tlib,{load,[]}},
                 {rct_core,[]}
		]}].


%% @hidden
init_per_suite(Config) ->
    register(?MODULE, spawn(?MODULE, ets_new, [])),
    ets:i(),
    Config.
%% @hidden
end_per_suite(_Config) ->
    ets:i(),
    case whereis(?MODULE) of
	Pid when is_pid(Pid) ->
	    ct:log("Pid from init per suite found: ~p", [Pid]),
	    ?MODULE ! stop;		
	_ ->
	    ct:log("No pid found from init per suite.", []),
	    ok
    end,
    ets:i(),
    BB = whereis(?MODULE),
    ct:log("BB: ~p", [BB]),
    ok.

ets_new() ->
    ets:new(?MODULE,[ordered_set,public,named_table]),
    receive
	stop ->
	    ets:delete(?MODULE)
    end.

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
     trig_restart_cold,
     get_startup_times_cold
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
     {sbc__pre_dc__tcu__1__group, [], [{group, tcu_restart_fru_cold_1}]},
     {sbc__pre_dc_lrat__dus__1__group, [], [{group, lte_rbs_restart_fru_cold_1}]},
     {startup_install_restart_cold, [],[
				  %% get_startup_times,
				  trig_restart_cold,
				  get_startup_times_cold
				 ]},
     {lte_rbs_restart_fru_cold, [], [{group, lte_rbs_restart_fru_cold_1}]},
     {lte_rbs_restart_fru_cold_1, [sequence], [rbs_trig_restart_cold,
					       lte_rbs_get_startup_times_cold
					      ]},
     {tcu_restart_fru_cold, [], [{group, tcu_restart_fru_cold_1}]},
     {tcu_restart_fru_cold_1, [sequence], [rbs_trig_restart_cold,
					   tcu_get_startup_times_cold
					  ]},
     {night_lte_restart_fru_cold, [sequence], [rbs_trig_restart_cold,
					       lte_night_get_meas_fru_cold_data
					      ]},
     {night_lte_rbs_restart_fru_cold, [], [{group, night_lte_restart_fru_cold},
					   {group, night_lte_restart_fru_cold},
					   {group, night_lte_restart_fru_cold},
					   {group, night_lte_restart_fru_cold},
					   {group, night_lte_restart_fru_cold},
					   {group, night_lte_restart_fru_cold},
					   {group, night_lte_restart_fru_cold},
					   {group, night_lte_restart_fru_cold},
					   {group, night_lte_restart_fru_cold},
					   {group, night_lte_restart_fru_cold},
					   lte_write_night_graph_data
					  ]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% @spec get_eq_element(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
get_eq_element(_Config) ->
    GetEqCmd = "/home/etxivri/Kista/rcs_ithub/egenskaper/boan/bra_o_ha_xml/get_equipment.xml ",
	get_element(GetEqCmd).

get_me_element(_Config) ->
    GetMeCmd = "/home/etxivri/Kista/rcs_ithub/egenskaper/boan/bra_o_ha_xml/get.xml ",
    get_element(GetMeCmd).

get_element(GetCmd) ->
    NodeName = get_node_name(),
    ct:pal("Node:~n~p~n",[NodeName]),

    ct:pal("Start: Get Element ",[]),
    Start1 = os:timestamp(),
    Answ = os:cmd("rcs_exec -m netconf -f "++ GetCmd ++ NodeName),
    End1 = os:timestamp(),
    ct:pal("End: Get Element ",[]),

    TimeToCreate_Cell_MO = trunc(timer:now_diff(End1, Start1) / 1000),
    ct:pal("Time to Get Equipment MO: ~p ms.",[TimeToCreate_Cell_MO]),

    simple_check_of_nc_message(Answ).

%%--------------------------------------------------------------------
%% @doc
%% @spec trig_restart_cold(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
trig_restart_cold(_Config) ->
    ct:log("Config: ~p", [_Config]),
    wait_for_netconf_started(),
    PrivDir = proplists:get_value(priv_dir, _Config),
    ct:log("PrivDir: ~p", [PrivDir]),

    ct:pal("Measure startup times after trig restart cold via appmI."),
    RcsStartTime = restart_cold(),    
    CpuIdleTime = "-",
    %% Add this log dir to get erl log for this run.
    NewConfig = [{old_priv_dir, PrivDir},
		 {rcs_start_time, RcsStartTime},
		 {cpu_idle_time, CpuIdleTime}],
    ct:pal("Add NewConfig: ~p",[NewConfig]),
    {save_config, NewConfig}.

%%--------------------------------------------------------------------
%% @doc
%% @spec rbs_trig_restart_cold(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
rbs_trig_restart_cold(_Config) ->
    ct:log("Config: ~p", [_Config]),
    wait_for_netconf_started(),
    ct:pal("Sleep 2 min to ensure node is up before restart node.", []),
    timer:sleep(120000),
    PrivDir = proplists:get_value(priv_dir, _Config),
    ct:log("PrivDir: ~p", [PrivDir]),

    ct:pal("RBS. Measure startup times after trig restart cold using, "
	   "MO FieldReplaceableUnit::restartUnit ."),
    {RcsStartTime, CpuIdleTime} = rbs_restart_cold(),

    %% Add this log dir to get erl log for this run.
    NewConfig = [{old_priv_dir, PrivDir},
		 {rcs_start_time, RcsStartTime},
		 {cpu_idle_time, CpuIdleTime}],
    ct:pal("Add NewConfig: ~p",[NewConfig]),
    {save_config, NewConfig}.

%%--------------------------------------------------------------------
%% @doc
%% @spec get_startup_times_cold(Config) -> ok
%% @end
%%--------------------------------------------------------------------
get_startup_times_cold(Config) ->
    NodeType = check_if_rbs(?NC_Sess),
    FileName = get_filename(NodeType++"_startup_cold.txt"),
    get_startup_times(Config, FileName).

lte_rbs_get_startup_times_cold(Config) ->
    NodeType = check_if_rbs(?NC_Sess),
    FileName = get_filename("lte_"++NodeType++"_startup_cold.txt"),
    get_startup_times(Config, FileName).

tcu_get_startup_times_cold(Config) ->
    NodeType = check_if_rbs(?NC_Sess),
    FileName = get_filename(NodeType++"_startup_cold.txt"),
    get_startup_times(Config, FileName).

get_startup_times_cold_test(Config) ->
    NodeType = check_if_rbs(?NC_Sess),
    FileName = get_filename(NodeType++"_startup_cold_test.txt"),
    get_startup_times(Config, FileName).

%%--------------------------------------------------------------------
%% @doc
%% @spec get_startup_times(Config) -> ok
%% @end
%%--------------------------------------------------------------------
get_startup_times(Config) ->
    NodeType = check_if_rbs(?NC_Sess),
    FileName = get_filename(NodeType++"_startup_times.txt"),
    get_startup_times(Config, FileName).


get_startup_times(Config, FileName) ->
    ct:pal("get_startup_times."),
    ct:pal("Config:~n~p~n",[Config]),
    case ?config(saved_config, Config) of
	{_Saver, ConfigList} ->
	    ct:log("Saved ConfigList: ~p",[ConfigList]),
	    OldPrivDir = proplists:get_value(old_priv_dir, ConfigList),
	    RcsStartTime = proplists:get_value(rcs_start_time, ConfigList),
	    CpuIdleTime = proplists:get_value(cpu_idle_time, ConfigList),
	    ErlLogDir = OldPrivDir;
	undefined ->
	    RcsStartTime = "-",
	    CpuIdleTime = "-",
	    ErlLogDir = "/rcs/erlang/"
    end,
    ct:log("ErlDir: ~p", [ErlLogDir]),

    {Elapsed_Ts, 
     OtpStart,
     FullRestart,
     ApplStart,
     MwStart} =
    	startup_times(ErlLogDir),

    CXS_label = get_sw_version(),
    updateMeasResFile_1(FileName, "~p;~w;~s;~s;~s;~s;~s;~s;~p~n", 
    			[httpd_util:rfc1123_date(),
    			 CXS_label,
    			 RcsStartTime,
    			 Elapsed_Ts,
    			 OtpStart,
    			 FullRestart,
			 ApplStart,
			 MwStart,
			 CpuIdleTime
    			]),

    ct:pal("results file: ~p ,\n path: ~p \n",[FileName, ?RESULTDIR]),

    %% Check that FullRestart time is not to high.
    ct:pal("## Check that full restart has not increased to much!"),
    case list_to_float(FullRestart) of
	Val when Val < 150 ->
    	    ok;
    	_ToHigh ->
    	    ct:fail("TC will fail, mw_start time is higher than expected.")
    end,

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec create_fru(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
create_fru(_Config) ->
    ct:pal("Config:~n~p~n",[_Config]),
    NodeName = get_node_name(),
    ct:pal("Node:~n~p~n",[NodeName]),

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

%%--------------------------------------------------------------------
%% @doc
%% @spec pre_conf(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
pre_conf(_Config) ->
    ct:pal("Config:~n~p~n",[_Config]),
    NodeName = get_node_name(),
    ct:pal("Node:~n~p~n",[NodeName]),

    %% test_server:break("A"),
    ct:pal("Conf: LRAT_basic_fru_netconf_create.xml. ~n"
	   "- FieldReplaceableUnit=1 is created in Equipment=1",[]),
    os:cmd("rcs_exec -m netconf -f /home/etxivri/Kista/rcs_ithub/egenskaper/boan/7_nov/LRAT_basic_fru_netconf_create.xml " ++ NodeName),

    timer:sleep(10000),

    %% test_server:break("B"),
    ct:pal("Conf: RBSNC_2_basic_fru_netconf_create_common.xml ~n"
	   "- FieldReplaceableUnit=2 is created in Equipment=1 ~n"
	   "- RiLink=1 is created in Equipment=1 ~n"
	   "- SectorEquipmentFunction=1 is created in ManagedElement=1",[]),
    os:cmd("rcs_exec -m netconf -f /home/etxivri/Kista/rcs_ithub/egenskaper/boan/7_nov/RBSNC_2_basic_fru_netconf_create_common.xml "++ NodeName),

    timer:sleep(20000),

    %% test_server:break("C"),
    ct:pal("Conf: tn_config_template.xml ",[]),
    %% os:cmd("rcs_exec -m netconf -f /home/etxivri/Kista/rcs_ithub/egenskaper/boan/dus5009/tn_config_template.xml "++ NodeName),
    %% %%from R3B1322 this new tn conf shall be used.
    A = os:cmd("rcs_exec -m netconf -f /home/etxivri/Kista/rcs_ithub/egenskaper/boan/dus5009/new_141211_tn_config_template.xml "++ NodeName),
    ct:log("tn_config_template.xml : ~s", [encodeForLog(A)]),
    timer:sleep(30000),

    %%%%
    %% Reboot needed before create TPtoMMEMO
    %%%%
    %% test_server:break("reboot node"),
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    rbs_restart(),
    net_kernel:disconnect(ErlNode),
    get_time_rcs_start(),
    ct:pal("### wait for login"),
    wait_for_login_prompt(),   
    ct:pal("### End wait for login"),

    %% wait_for_netconf_started(),
    wait_for_node_ready_for_rpc(),
    timer:sleep(10000),
    %% wait_for_exp_cpu_states(),

    ct:pal("Conf: cli_createTPtoMMEMO.txt ",[]),
    B = os:cmd("rcs_exec -m cli -f /home/etxivri/Kista/rcs_ithub/egenskaper/boan/dus5009/cli_createTPtoMMEMO.txt "++ NodeName),
    ct:log("createTPtoMMEMO: ~s", [encodeForLog(B)]),

    %% wait_for_exp_cpu_states(),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec create_1_EUtranCellFDD_MO(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
create_1_EUtranCellFDD_MO(_Config) ->
    ct:pal("Config:~n~p~n",[_Config]),
    %% CreateCmd = "/home/etxivri/Kista/rcs_ithub/egenskaper/boan/7_nov/create_sector_cell_mme.xml ",
    %% CreateCmd = "/home/etxivri/Kista/rcs_ithub/egenskaper/boan/7_nov/mod_no_etw_create_sector_cell_mme.xml ",
    CreateCmd = "/home/etxivri/Kista/rcs_ithub/egenskaper/boan/6_cell_141208/boan_create_sector_cell_mme_stub_augccb.xml ",
    ct:pal("Conf: ~p ",[CreateCmd]),
    generic_create_EUtranCellFDD_MO("1", CreateCmd).

create_6_EUtranCellFDD_MO(_Config) ->
    ct:pal("Config:~n~p~n",[_Config]),
    CreateCmd = "/home/etxivri/Kista/rcs_ithub/egenskaper/boan/6_cell_141208/new_create_sector_cell_mme_stub_augccb_6_cells.xml ",
    ct:pal("Conf: ~p ",[CreateCmd]),
    generic_create_EUtranCellFDD_MO("6", CreateCmd).

create_12_EUtranCellFDD_MO(_Config) ->
    ct:pal("Config:~n~p~n",[_Config]),
    CreateCmd = "/home/etxivri/Kista/rcs_ithub/egenskaper/boan/6_cell_141208/create_all_12_sector_cell_mme_stub_augccb.xml ",
    ct:pal("Conf: ~p ",[CreateCmd]),
    generic_create_EUtranCellFDD_MO("12", CreateCmd).


generic_create_EUtranCellFDD_MO(Nr, CreateCmd) ->
    NodeName = get_node_name(),
    ct:pal("Node:~n~p~n",[NodeName]),

    %% test_server:break("A"),
    ct:pal("Start: create ~p cell ",[Nr]),
    Start1 = os:timestamp(),
    Answ = os:cmd("rcs_exec -m netconf -f "++ CreateCmd ++ NodeName),
    End1 = os:timestamp(),
    ct:pal("End: create cell ",[]),

    TimeToCreate_Cell_MO = trunc(timer:now_diff(End1, Start1) / 1000),
    ct:pal("TimeToCreate_Cell_MO: ~p ms.",[TimeToCreate_Cell_MO]),

    CXS_label = get_sw_version(),

    simple_check_of_nc_message(Answ),
    simple_check_of_created_cell(list_to_integer(Nr)),

    FileName = "create_"++Nr++"_EUtranCellFDD_MO.txt",
    updateMeasResFile_1(FileName, "~p;~w;~p~n", 
    		      [httpd_util:rfc1123_date(),
		       CXS_label,
    		       TimeToCreate_Cell_MO
    		      ]),

    ct:pal("results file: ~p ,\n path: ~p \n",[FileName, ?RESULTDIR]),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec check_top_after_restart(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
check_top_after_restart(_Config) ->
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    rbs_restart(),
    net_kernel:disconnect(ErlNode),
    get_time_rcs_start(),
    ct:pal("### wait for login"),
    wait_for_login_prompt(),   
    ct:pal("### End wait for login"),

    wait_for_node_ready_for_rpc(),
    wait_for_ps(),

    ok.
%%--------------------------------------------------------------------
%% 
%%--------------------------------------------------------------------
simple_check_of_nc_message(Answ) ->
    ct:log("Netconf processing : ~s", [encodeForLog(Answ)]),
    case re:run(Answ, "<rpc-error>") of
    	nomatch ->
    	    ok;
    	{match, _} ->
    	    ct:fail("Test fail due to error exist in netconf message.")
    end.

%% %% From Rabbe
%% %% xml syntax need to be encoded to be printed out in our html
encodeForLog(L) ->
     lists:foldr(
       fun(C, A) ->
	      if
		  C =:= $\r ->
		      A;
		  C =:= $< ->
		      "&lt;"++A;
		  C =:= $> ->
		      "&gt;"++A;
		  true ->
		      [C]++A
	      end
       end,
       "",
       L).

%%--------------------------------------------------------------------
%% 
%%--------------------------------------------------------------------
simple_check_of_created_cell(NrOfCells) ->
    CellList = lists:seq(1, NrOfCells),
    check_of_created_cell(CellList).

check_of_created_cell(CellList) ->
    rct_cli:connect(?Cli_Sess),
    lists:foreach(fun(X) ->
			 Nr = integer_to_list(X),
			 {ok, Answ} = 
			     rct_cli:send(?Cli_Sess,
					  "show ManagedElement=1,ENodeBFunction=1,"
					  "EUtranCellFDD="++Nr++",activeServiceAreaId"),
			      ct:log("A: ~p", [Answ] ),
			  case re:run(Answ, "ERROR") of
			      nomatch ->
				  ok;
			      {match, _} ->
				  ct:fail("Test fail due to expected cell does not exist.")
			  end
		 end, CellList).

%%--------------------------------------------------------------------
%% @doc
%% @spec remove_1_EUtranCellFDD_MO(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
remove_1_EUtranCellFDD_MO(_Config) ->
    ct:pal("Config:~n~p~n",[_Config]),
    ct:pal("Conf: cli_removeTPtoMMEMO.txt ",[]),
    RemoveCmd = "/home/etxivri/Kista/rcs_ithub/egenskaper/cli_create_cell/cli_remove_cell.txt ",
    generic_remove_EUtranCellFDD_MO("1", RemoveCmd).

remove_6_EUtranCellFDD_MO(_Config) ->
    ct:pal("Config:~n~p~n",[_Config]),
    ct:pal("Conf: cli_removeTPtoMMEMO.txt ",[]),
    RemoveCmd = "/home/etxivri/Kista/rcs_ithub/egenskaper/cli_create_cell/cli_remove_6_cell.txt ",
    generic_remove_EUtranCellFDD_MO("6", RemoveCmd).

remove_12_EUtranCellFDD_MO(_Config) ->
    ct:pal("Config:~n~p~n",[_Config]),
    ct:pal("Conf: cli_removeTPtoMMEMO.txt ",[]),
    RemoveCmd = "/home/etxivri/Kista/rcs_ithub/egenskaper/cli_create_cell/cli_remove_12_cell.txt ",
    generic_remove_EUtranCellFDD_MO("12", RemoveCmd).

generic_remove_EUtranCellFDD_MO(Nr, RemoveCmd) ->
    NodeName = get_node_name(),
    ct:pal("Node:~n~p~n",[NodeName]),
    timer:sleep(10000),

    ct:pal("Remove ~p Cell",[Nr]),
    os:cmd("rcs_exec -m cli -f "++ RemoveCmd ++ NodeName),
    ct:pal("Remove done. ",[]),

    timer:sleep(10000),
    check_cell_mo_deleted(),
    ok.

check_cell_mo_deleted() ->
    ct_netconfc:open(?NC_Sess,[]),
    Answ =  ct_netconfc:get(?NC_Sess,{'ManagedElement',
				      [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
				      [{managedElementId,[],["1"]},
   					 {'ENodeBFunction',
   					  [{xmlns,"urn:com:ericsson:ecim:Lrat"}],
   					  [{'EUtranCellFDD',[],[]}]}]
				     }),
    case Answ of
	{error, _} ->
	    ct:pal("No cell exist, OK",[]);
	{ok, _} ->
	    ct:pal("Remove check answ: ~p", [Answ]),
	    ct:fail("Tc will fail due to cell could not be removed")
    end,
    ct_netconfc:close_session(?NC_Sess).

%%--------------------------------------------------------------------
%% @doc
%% @spec time_to_get(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
time_to_get(_Config) ->
    NodeName = get_node_name(),
    ct:pal("Node:~n~p~n",[NodeName]),
    GetCmd = "/home/etxivri/Kista/rcs_ithub/egenskaper/boan/bra_o_ha_xml/get.xml ",
    ct:pal("Start: get config ManagedElement ",[]),
    Start1 = os:timestamp(),
    os:cmd("rcs_exec -m netconf -f "++ GetCmd ++ NodeName),
    End1 = os:timestamp(),
    ct:pal("End: get config ManagedElement ",[]),

    TimeToGet = trunc(timer:now_diff(End1, Start1) / 1000),
    ct:pal("Time to get ManagedElement: ~p ms.",[TimeToGet]),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec lte_night_get_meas_fru_cold_data(Config) -> ok
%% @end
%%--------------------------------------------------------------------
lte_night_get_meas_fru_cold_data(Config) ->
    FileName = get_filename("lte_night_startup_cold.txt"),
    night_get_meas_fru_cold_data(Config, FileName).

night_get_meas_fru_cold_data(Config, FileName) ->
    ct:pal("get_startup_times."),
    ct:pal("Config:~n~p~n",[Config]),
    case ?config(saved_config, Config) of
	{_Saver, ConfigList} ->
	    ct:log("Saved ConfigList: ~p",[ConfigList]),
	    OldPrivDir = proplists:get_value(old_priv_dir, ConfigList),
	    RcsStartTime = proplists:get_value(rcs_start_time, ConfigList),
	    CpuIdleTime = proplists:get_value(cpu_idle_time, ConfigList),
	    ErlLogDir = OldPrivDir;
	undefined ->
	    RcsStartTime = "-",
	    CpuIdleTime = "-",
	    ErlLogDir = "/rcs/erlang/"
    end,
    ct:log("ErlDir: ~p", [ErlLogDir]),

    {Elapsed_Ts, 
     OtpStart,
     FullRestart,
     ApplStart,
     MwStart} =
	startup_times(ErlLogDir),

    CXS_label = get_sw_version(),
    updateMeasResFile_2(FileName, "~p;~w;~s;~s;~s;~s;~s;~s;~p~n",
    			[httpd_util:rfc1123_date(),
    			 CXS_label,
    			 RcsStartTime,
    			 Elapsed_Ts,
    			 OtpStart,
    			 FullRestart,
			 ApplStart,
			 MwStart,
			 CpuIdleTime
    			], ?NIGHT_RESULTDIR),

    ct:pal("results file: ~p ,\n path: ~p \n",[FileName, ?NIGHT_RESULTDIR]),


    ct:pal("FullRestart: ~p",[FullRestart]),
    ct:pal("MwStart: ~p",[MwStart]),
    ct:pal("CpuIdleTime: ~p",[CpuIdleTime]),

    case string:to_float(FullRestart) of
	{error, _} ->
	    Float_FullRestart = 0;
	{Float_FullRestart,_Rest} -> 
	    Float_FullRestart
    end,
    
    case string:to_float(MwStart) of
	{error, _} ->
	    Float_MwStart = 0;
	{Float_MwStart, _MwStart} -> 
	    Float_MwStart
    end,
	    

    insert_data_to_ets(round(Float_FullRestart), 
		       round(Float_MwStart), 
		       round(CpuIdleTime) ),

    ok.


insert_data_to_ets(FullRestart, MwStart, CpuIdleTime) ->
    ct:pal("Add data to ets table"),
    ets:i(),

    case ets:lookup(?MODULE, full_restart) of
	[] ->
	    A = [];
	[{full_restart, A}] ->
	    A
    end,
    case ets:lookup(?MODULE, mw_start) of
	[] ->
	    B = [];
	[{mw_start, B}] ->
	    B
    end,
    case ets:lookup(?MODULE, cpu_idle_time) of
	[] ->
	    C = [];
	[{cpu_idle_time, C}] ->
	    C
    end,
    case ets:lookup(?MODULE, nr_of_runs) of
	[] ->
	    Nr = 0;
	[{nr_of_runs, Nr}] ->
	    Nr
    end,

    ct:log("A: ~p", [A]),
    ct:log("B: ~p", [B]),
    ct:log("C: ~p", [C]),
    ct:log("Nr: ~p", [Nr]),

    ct:log("AA: ~p", [FullRestart]),
    ct:log("BB: ~p", [MwStart]),
    ct:log("CC: ~p", [CpuIdleTime]),

    ets:insert(?MODULE, [{nr_of_runs, Nr + 1},
			 {full_restart, A ++ [FullRestart]},
			 {mw_start, B ++ [MwStart]},
			 {cpu_idle_time, C ++ [CpuIdleTime]}
			  ]),

    D = ets:lookup(?MODULE, full_restart),
    E = ets:lookup(?MODULE, mw_start),
    F = ets:lookup(?MODULE, cpu_idle_time),
    NrOfRuns = ets:lookup(?MODULE, nr_of_runs),
    ct:log("D: ~w", [D]),
    ct:log("E: ~w", [E]),
    ct:log("F: ~w", [F]),
    ct:log("NrOfRuns: ~w", [NrOfRuns]),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec lte_write_night_graph_data(_Config) -> ok
%% @end
%%--------------------------------------------------------------------
lte_write_night_graph_data(_Config)-> 
    ct:pal("Write collected data in ets table to a file for graph plot"),

    [{full_restart, D}]= ets:lookup(?MODULE, full_restart),
    [{mw_start, E}] = ets:lookup(?MODULE, mw_start),
    [{cpu_idle_time, F}] = ets:lookup(?MODULE, cpu_idle_time),
    [{nr_of_runs, NrOfRuns}]= ets:lookup(?MODULE, nr_of_runs),

    ct:pal("End FullRestart:~n ~w", [D]),
    ct:pal("End MwStart:~n ~w", [E]),
    ct:pal("End CpuIdleTime:~n ~w", [F]),
    ct:pal("End NrOfRuns: ~w", [NrOfRuns]),

    MinFullRestart = lists:min(D),
    MaxFullRestart = lists:max(D),
    AvgFullRestart = round(lists:sum(D) / length(D)),

    MinMwStart = lists:min(E),
    MaxMwStart = lists:max(E),
    AvgMwStart = round(lists:sum(E) / length(E)),

    MinCpuIdleTime = lists:min(F),
    MaxCpuIdleTime = lists:max(F),
    AvgCpuIdleTime = round(lists:sum(F) / length(F)),

    %% FileName = "R4_lte_night_useful_log_data.txt",
    Branch = meas_lib:get_branch(),
    FileName = Branch++"_lte_night_useful_log_data.txt",

    CXS_label = get_sw_version(),
			   
    updateMeasResFile_2(FileName, "~p;~w;~w;~w;~w;~w;~w;~w;~w;~w;~w;~w~n", 
    			[httpd_util:rfc1123_date(),
    			 CXS_label,
			 MinFullRestart,
			 MaxFullRestart,
			 AvgFullRestart,
			 MinMwStart,
    			 MaxMwStart,
			 AvgMwStart,
			 MinCpuIdleTime,
			 MaxCpuIdleTime,
			 AvgCpuIdleTime,
			 NrOfRuns
    			], ?NIGHT_GRAPH_FRU_RESTART),

    ct:pal("results file: ~p ,\n path: ~p \n",[FileName, 
					       ?NIGHT_GRAPH_FRU_RESTART]),

    ok.


%% ===========================================================================
%% Internal
%% ===========================================================================
get_time_rcs_start() ->
    %% %% Get timestamp from consol log.
    case ct_telnet:expect(console, 
			  %% %% "\\[[^]]*] rcs_start: Running rcs_start",
			  "\\[[^]]*] rcs_start", 
			  [{total_timeout,60000}, 
			   {idle_timeout,10000}, 
			   no_prompt_check]) of
	{ok, [Answ]} ->
	    ct:log("Timestamp for Running rcs_start: ~p ", [Answ]),
	    MatchStr = string:tokens(Answ, "[ ]"),
	    ct:log("MatchStr: ~p ",[MatchStr]),
	    [RcsStartTimestamp | _T] = MatchStr;
	_NoMatch ->
	    ct:pal("No match on rcs_start: Running rcs_start", []),
	    RcsStartTimestamp = "-"
    end,
   
    ct:pal("Consol timestamp for Running rcs_start: ~p ", [RcsStartTimestamp]),
    RcsStartTimestamp.

%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
startup_times(ErlLogDir) ->
    %%%%
    %% Get sw version
    %%%%
    ct:log("ErlLog:~n~p~n",[ErlLogDir]),
    SearchList = get_all_startup_times(ErlLogDir),

    %% %% Elapsed_Now = lists:nth(3, SearchList),
    %% %% Elapsed_Ts = lists:nth(5, SearchList),
    %% %% ClockDiff = lists:nth(7, SearchList),
    %% %% SinceBootlogStart = lists:nth(9, SearchList),
    %% %% OtpStart = lists:nth(11, SearchList),
    %% %% FullRestart = lists:nth(13, SearchList),
    %% %% ApplStart = lists:nth(15, SearchList),
    %% %% MwStart = lists:nth(17, SearchList),

    %% Removed some measure points due to it deprecated in OTP 18.
    Elapsed_Ts = get_specific_time("elapsed_ts:", SearchList),
    OtpStart = get_specific_time("otp_start:", SearchList),
    FullRestart = get_specific_time("full_restart:", SearchList),
    ApplStart = get_specific_time("application_start:", SearchList),
    MwStart = get_specific_time("mw_start:", SearchList),


    ct:pal("Elapsed_Ts:~p~n"
    	   "OtpStart:~p~n"
    	   "FullRestart:~p~n"
	   "ApplStart:~p~n"
	   "MwStart:~p~n",
    	   [Elapsed_Ts, 
    	    OtpStart,
    	    FullRestart,
	    ApplStart,
	    MwStart]),
    
    {Elapsed_Ts, 
     OtpStart, 
     FullRestart,
     ApplStart,
     MwStart}.


%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
get_all_startup_times(ErlLogDir) ->
    case ErlLogDir of
	"/rcs/erlang/" -> % Check on node.
	    Cat = rct_rpc:call( rpc, os, cmd, 
				["cat "++ErlLogDir++"erlang.log.*"],
				30000, noprint);
	_Other -> %% file exist in logs from testruns.
	    Cat = os:cmd("cat "++ErlLogDir++"*_erlang.log*") 
    end,
    BBBB = string:tokens(Cat,"\n\r, "),
    SearchList = lists:dropwhile(fun(X) ->
    					 %% X =/= "log_startup_time"
					 X =/= "[appm_complete]}"
    				 end, BBBB),
    ct:log("SearchList:~n~p~n",[SearchList]),
    SearchList.

%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
 get_specific_time(SpecificTime, SearchList) ->
    ct:pal("Search for SpecificTime: ~p~n",[SpecificTime]),
    List = lists:dropwhile(fun(X) ->
				   X =/= SpecificTime
			   end, SearchList),
    [SpecificTime, Time | _] = List,
    ct:log("SpecificTime: ~p, Time: ~p~n",[SpecificTime, Time]),
    Time.

%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
restart_cold() ->
    ct:pal("### Order cold restart via appmI!",[]),
    
    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    ok = rct_rpc:call(rpc, appmI, restart_piu_cold, ['_'], 100000, noprint),
    %% test_server:break("A"),
    net_kernel:disconnect(ErlNode),
    %% check node restarts
    {ok,_} = ct_telnet:expect(console, 
			      "Ericsson Version:", 
			      [{idle_timeout,60000}, 
			       no_prompt_check]),

    RcsStartTime = get_time_rcs_start(),
    wait_for_login_prompt(),    
    wait_for_netconf_started(),

    %% Ensure log_startup_time exist in erl log.
    timer:sleep(30000),
    RcsStartTime.

%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
rbs_restart_cold() ->
    ct:pal("### RBS. Order fru restart cold !",[]),

    {ok, ErlNode} = rct_rpc:get_erlnode(rpc),
    StartNow = rbs_restart(),
    net_kernel:disconnect(ErlNode),

    %% check node restarts
    {ok,_} = ct_telnet:expect(console, 
			      "Ericsson Version:", 
			      [{idle_timeout,60000}, 
			       no_prompt_check]),

    RcsStartTime = get_time_rcs_start(),
    ct:pal("### wait for login"),
    wait_for_login_prompt(),   
    ct:pal("### End wait for login"),

    wait_for_node_ready_for_rpc(),
    %% CpuStopTime = wait_for_exp_cpu_states(),
    CpuStopTime = wait_and_ensure_cpu_is_idle(), %% to ensure cpu is idle
    timer:sleep(2000),
	    
    wait_for_netconf_started(),

    case CpuStopTime of
	dummy ->
	    CpuIdleTme = dum,
	    ct:pal("RestartTime1 not calulated due to dummy cpu stop time."),
	    ok;
	_Value ->
	    CpuIdleTme = 
		trunc(timer:now_diff(CpuStopTime, StartNow) / 1000 / 1000),
	    ct:pal("From restart to cpu is idle: ~p sec", [CpuIdleTme])
    end,

    %% Ensure log_startup_time exist in erl log.
    timer:sleep(30000),

    {RcsStartTime, CpuIdleTme}.


%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
updateMeasResFile_1(FileName, PrintOpts, MeasData) ->
    [{_, NodeName}] = ct:get_config(test_nodes),
    %% RBS_CS_CXP = get_rbs_cs_cxps(),
    %% MeasInfo = MeasData++[NodeName]++[RBS_CS_CXP],
    BuildLabel = meas_lib:get_build_label(),
    MeasInfo = MeasData++[NodeName]++[BuildLabel],
    ct:pal("MeasInfo: ~p",[MeasInfo]),
    %% insert a ;~w before ~n due to the field NodeName. ~p for RBS_CS_CXP.
    CompletePrintOpts = re:replace(PrintOpts, "~n", ";~w;~s~n",[{return, list}]),
    rct_tlib:
	writeDataToFile(?RESULTDIR, FileName, CompletePrintOpts, MeasInfo),
    ok.

updateMeasResFile_2(FileName, PrintOpts, MeasData, RESULT_DIR) ->
    [{_, NodeName}] = ct:get_config(test_nodes),
    %% RBS_CS_CXP = get_rbs_cs_cxps(),
    %% MeasInfo = MeasData++[NodeName]++[RBS_CS_CXP],
    BuildLabel = meas_lib:get_build_label(),
    MeasInfo = MeasData++[NodeName]++[BuildLabel],
    ct:pal("MeasInfo: ~p",[MeasInfo]),
    %% insert a ;~w before ~n due to the field NodeName. ~p for RBS_CS_CXP.
    CompletePrintOpts = re:replace(PrintOpts, "~n", ";~w;~s~n",[{return, list}]),
    rct_tlib:
	writeDataToFile(RESULT_DIR, FileName, CompletePrintOpts, MeasInfo),
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
wait_for_login_prompt() ->
    wait_for_login_prompt(60000).

wait_for_login_prompt(Timeout) when Timeout < 500 ->
    ct:fail("No login prompt within max timeout after restart.");

wait_for_login_prompt(Timeout) ->
    ct_telnet:send(console, []),
    case ct_telnet:expect(console, "login:", 
			  [{idle_timeout,5000}, no_prompt_check]) of
	{ok, _} -> 
	    ok;
	_  ->
	    timer:sleep(5000),
	    wait_for_login_prompt(Timeout - 5000)
    end.

%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
wait_for_netconf_started() ->
    wait_for_netconf_started(180000).

wait_for_netconf_started(Timeout) when Timeout < 500 ->
    ct:fail("Netconf not started within max timeout after restart.");

wait_for_netconf_started(Timeout) ->
    case ct_netconfc:open(nc1,[]) of
	{ok,_} ->
	    ct_netconfc:close_session(nc1),
	    ok;
	_Other  ->
	    ct:log("Other: ~p",[_Other]),
	    timer:sleep(5000),
	    wait_for_netconf_started(Timeout - 5000)
    end.

%% ===========================================================================
%% @doc
%% Get CXS label. <br/>
%% Get SW version using COM cli interface. <br/>
%% @end
%% ===========================================================================
get_sw_version() ->
    CXS_label = rct_tlib:get_sw_version(?Cli_Sess),
    CXS_label.

get_rbs_cs_cxps() ->
    BoardType = 
	proplists:get_value(board_type,
			    ct:get_config(
			      ct:get_config({test_nodes,1}))),
    ct:log("BoardType: ~p ",[BoardType]),
    CS_CXP = case BoardType of
		 "dus5201" ->
		     "CXP9031275";
		 BoardType when BoardType == "tcu03";
				BoardType == "tcu0401" ->
		     "CXP9031274";
		 _NoCheck ->
		     "dum"
	     end,
			 
    ct:pal("### Get SW version for CS CXP : ~p",[CS_CXP]),
    rct_cli:connect(?Cli_Sess),
    case  rct_cli:send(?Cli_Sess,"show ManagedElement=1,SystemFunctions=1,SwInventory=1") of
	{ok , RecievedData} ->
	    ct:log("RecievedData: ~s", [RecievedData]),
	    Var = string:tokens(RecievedData, "=\r\n /-"),
	    %% ct:log("SwInventory: ~p", [Var]),
	    %% test_server:break("break"),
	    case lists:dropwhile(fun(X) ->
					 X =/=  CS_CXP
				 end, Var) of
		[] ->
		    Ind = "dummy",
		    RBS_CS_Rev = "dummy";
		RBS_CS_SwItem ->
		    [CS_CXP, Ind, RBS_CS_Rev | _ ] = RBS_CS_SwItem
	    end;
	_Other ->
	    ct:pal("Unexpected data: ~p", [_Other]),
	    Ind = "dummy",
	    RBS_CS_Rev = "dummy"
    end,
    rct_cli:disconnect(?Cli_Sess),

    RBS_CS_CXP = CS_CXP++"/"++Ind++"-"++RBS_CS_Rev,
    ct:pal("RBS CS CXP: ~p", [RBS_CS_CXP]),
    RBS_CS_CXP.

%% ===========================================================================
%% ===========================================================================
get_filename(ReastartCause_info) ->
    Branch = meas_lib:get_branch(),
    BoardType = meas_lib:get_board_type(),
    FileName = Branch++"_"++BoardType ++"_"++ ReastartCause_info,
    FileName.

%% ===========================================================================
%% ===========================================================================
check_if_rbs(NC_Session) ->
    ct_netconfc:open(NC_Session, [{timeout, 30000}]),
    {ok,
     [{'ManagedElement',
       [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
       [{managedElementId,[],[MeId]} | _]}]} =
	ct_netconfc:get(NC_Session,
			{'ManagedElement',
			 [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			 [{managedElementId,[],[]}]
			}, 30000),
    
    case ct_netconfc:get(NC_Session,
			 {'ManagedElement',
			  [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
			  [{managedElementId,[],[MeId]},
			   {'TestRoot', [], []}]}, 30000) of
	{ok, _} ->
	    NodeType = "cs";
	_Other ->
	   NodeType = "rbs"
    end,
    ct_netconfc:close_session(NC_Session, 30000),
    ct:pal("NodeType: ~p", [NodeType]), 
    NodeType.

%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
rbs_restart() ->
    rct_cli:connect(?Cli_Sess),
    {ok, A} = rct_cli:send(?Cli_Sess,"ManagedElement=1,Equipment=1,FieldReplaceableUnit=1,restartUnit RESTART_COLD PLANNED_RECONFIGURATION 0"),
    StartNow = os:timestamp(),
    ct:log("RBS restart FieldReplaceableUnit start. A: ~p",[A]),
    rct_cli:disconnect(?Cli_Sess),

    StartNow.

%% %% ===========================================================================
%% %% ===========================================================================
%% wait_for_exp_cpu_states() ->
%%     timer:sleep(10000),
%%     NowTime_1 = os:timestamp(),
%%     {Idle, Wa} = vmstat(),
%%     wait_for_exp_cpu_states(NowTime_1, Idle, Wa, 240000).

%% wait_for_exp_cpu_states(_NowTime, _Idle, _Wa, Timeout) when Timeout < 500 ->
%%     CpuStopTime = dummy,
%%     CpuStopTime;

%% wait_for_exp_cpu_states(NowTime, Idle, Wa, _Timeout) when 
%%       Idle > 85 andalso Wa < 3 ->
%%     ct:pal("Cpu avg-Idle ~p and avg-Wa ~p is OK.", [Idle, Wa]),
%%     NowTime;

%% wait_for_exp_cpu_states(_NowTime, _Idle, _Wa, Timeout) ->
%%     New_NowTime = os:timestamp(),
%%     {NewCpuIdle, NewCpuWa} = vmstat(),
%%     wait_for_exp_cpu_states(New_NowTime, NewCpuIdle, NewCpuWa, Timeout - 5000).

%% ===========================================================================
%% ===========================================================================

wait_and_ensure_cpu_is_idle() ->
    NowTime_1 = os:timestamp(),
    Nr = 1,
    wait_and_ensure_cpu_is_idle(NowTime_1, Nr, 240000).

wait_and_ensure_cpu_is_idle(NowTime, 4, _Timeout) ->
    ct:pal("Cpu avg-Idle and avg-Wa has been OK 3 times in a row.", 
	   []),
    NowTime;

wait_and_ensure_cpu_is_idle(_NowTime,_Nr,Timeout) when Timeout < 500 ->
    CpuStopTime = dummy,
    CpuStopTime;

wait_and_ensure_cpu_is_idle(NowTime, Nr, Timeout) ->
    case vmstat() of
	{Idle, Wa} when Idle > 85 andalso Wa < 3 ->
	    ct:pal("Cpu avg-Idle ~p and avg-Wa ~p is OK. Nr: ~p.", 
		   [Idle, Wa, Nr]),
	    wait_and_ensure_cpu_is_idle(NowTime,Nr+1,Timeout-5000);
	_Other ->
	    ct:pal("Cpu avg-Idle and avg-Wa is NOK: ~p.Clear Nr. Check again", 
		   [_Other]),
	    ClrNr=1,
	    NewNowTime = os:timestamp(),
	    wait_and_ensure_cpu_is_idle(NewNowTime,ClrNr,Timeout-5000)
    end.

%% ===========================================================================
%% ===========================================================================
vmstat() ->
    %% The following example displays a systems virtual memory statistics 
    %% 5 times at 1 second intervals.
    NrOfIntervals = 5, %% Takes about 4-5 seconds
    CalcNr = 4, %% First sample is removed.
    VmStat = rct_rpc:call(rpc, os, cmd, 
			  ["vmstat 1 "++integer_to_list(NrOfIntervals)], 
			  10000, noprint),
    ct:pal("VmStat: ~n~s~n",[VmStat]),
    Vm_Stat = string:tokens(VmStat, " \n-"),
    %% ct:pal("Vm_Stat: ~n~p",[Vm_Stat]),

    ["st" | Search_List] = lists:dropwhile(fun(X) ->
						   X =/= "st"
						   %% X =/= "wa"
					   end, Vm_Stat),
    ct:log("Search_List:~n~p~n",[Search_List]),

    %% Searchlist 0
    {_L, SearchList} = lists:split(17, Search_List),
    ct:log("New SearchList:~n~p~n",[SearchList]),
    Id = list_to_integer(lists:nth(15, SearchList)),
    Wa = list_to_integer(lists:nth(16, SearchList)),

    %% Searchlist 1
    {_, SearchList1} = lists:split(17, SearchList),
    ct:log("New SearchList1:~n~p~n",[SearchList1]),
    Id1 = list_to_integer(lists:nth(15, SearchList1)),
    Wa1 = list_to_integer(lists:nth(16, SearchList1)),

    %% Searchlist 2
    {_, SearchList2} = lists:split(17, SearchList1),
    ct:log("New SearchList2:~n~p~n",[SearchList2]),
    Id2 = list_to_integer(lists:nth(15, SearchList2)),
    Wa2 = list_to_integer(lists:nth(16, SearchList2)),

    %% Searchlist 3
    {_, SearchList3} = lists:split(17, SearchList2),
    ct:log("New SearchList3:~n~p~n",[SearchList3]),
    Id3 = list_to_integer(lists:nth(15, SearchList3)),
    Wa3 = list_to_integer(lists:nth(16, SearchList3)),

    ct:log("All Id: ~p, ~p, ~p, ~p,",[Id, Id1, Id2, Id3]),
    ct:log("All Wa: ~p, ~p, ~p, ~p,",[Wa, Wa1, Wa2, Wa3]),
    SumId = Id + Id1 + Id2 + Id3,
    SumWa = Wa + Wa1 + Wa2 + Wa3,
    ct:pal("Sum Id: ~p, Sum Wa: ~p",[SumId, SumWa]),
    AvgId = SumId/CalcNr,
    AvgWa = SumWa/CalcNr,
    ct:pal("Avg Id: ~p, Avg Wa: ~p, on ~p intervals", [AvgId, 
    						       AvgWa, 
    						       CalcNr]),
    {AvgId, AvgWa}.

    %% ["wa" | Search_List] = lists:dropwhile(fun(X) ->
    %% 						   X =/= "wa"
    %% 					   end, Vm_Stat),
    %% ct:log("Search_List:~n~p~n",[Search_List]),
    %% %% Remove first sample. due to it could be corrupt!
    %% {_L, SearchList} = lists:split(16, Search_List),
    %% ct:log("New SearchList:~n~p~n",[SearchList]),
    %% NrList = lists:seq(1, CalcNr),
    %% MapList = lists:map(fun(N) ->
    %% 		      Id = list_to_integer(lists:nth((16*N)-1, SearchList)),
    %% 		      Wa = list_to_integer(lists:nth(16*N, SearchList)),
    %% 		      %% ct:pal("Id: ~p, Wa: ~p",[Id, Wa]),
    %% 		      {Id, Wa}
    %% 	      end, NrList),
    %% ct:pal("MapList:~n~p~n",[MapList]),

    %% {C,D} = lists:unzip(MapList),
    %% SumId = lists:sum(C),
    %% SumWa = lists:sum(D),
    %% ct:pal("Sum Id: ~p, Sum Wa: ~p",[SumId, SumWa]),
    %% AvgId = SumId/CalcNr,
    %% AvgWa = SumWa/CalcNr,
    %% ct:pal("Avg Id: ~p, Avg Wa: ~p, on ~p intervals", [AvgId, 
    %% 						       AvgWa, 
    %% 						       CalcNr]),
    %% {AvgId, AvgWa}.

%% ===========================================================================
%% ===========================================================================
wait_for_node_ready_for_rpc() ->
    wait_for_node_ready_for_rpc(60000).
wait_for_node_ready_for_rpc(Timeout) when Timeout < 0 ->
    ct:fail("Node not up within max timeout after restart.");
wait_for_node_ready_for_rpc(Timeout) ->
    case rct_rpc:call(rpc, os, cmd, ["ls"], 1000, noprint) of
	%% case rct_rpc:call(rpc, os, cmd, ["ls"], 1000, print) of
	{badrpc,nodedown} ->
	    timer:sleep(2000),
	    wait_for_node_ready_for_rpc(Timeout-2000);
	_Other ->
	    ct:log("Node is up and it is ok to use rps: ~p", [_Other]),
	    ok
    end.

%% ===========================================================================
%% @doc
%% @end
%% ===========================================================================
wait_for_ps() ->
    wait_for_ps(180000).

wait_for_ps(Timeout) when Timeout < 500 ->
    ok;

wait_for_ps(Timeout) ->
    FileName = "top_tory", 
    A = rct_rpc:call(rpc, os, cmd, ["top -b -n 1 | head -n 20"], 1000, print),
    file_write(?RESULTDIR, FileName, A),
    wait_for_ps(Timeout - 1000).

file_write(FileDir, FileName, Data) ->
    {ok, FD} = file:open(FileDir ++ "/" ++ FileName, [append]),
    file:write(FD, "\n"),
    file:write(FD, os:cmd("date")),
    file:write(FD, Data),
    file:write(FD, "\n"),
    file:close(FD),
    ok.
%% ===========================================================================
%% From Esko
%% ===========================================================================
%% get_day(Now) ->
%%     %% {_, _, MS} = os:timestamp(),
%%     {_, _, MS} = Now,
%%     get_day(calendar:now_to_universal_time(Now), MS).

%% get_day({{Y, M, D}, _}, _) ->
%%      lists:append([integer_to_list(Y), "-",
%% 		  gt_zero(M),
%% 		  integer_to_list(M), "-",
%% 		  gt_zero(D),
%% 		  integer_to_list(D), "  "]).


%% get_time({_, _, MS} = Now) ->
%%      get_time(calendar:now_to_universal_time(Now), MS).

%% get_time({_, {H, Mi, S}}, MS) ->
%%      lists:append([gt_zero(H),
%% 		  integer_to_list(H), ":",
%% 		  gt_zero(Mi),
%% 		  integer_to_list(Mi), ":",
%% 		  gt_zero(S),
%% 		  integer_to_list(S), ".",
%% 		  gt_zero_ms(MS),
%% 		  integer_to_list(MS)]).

%% gt_zero(X) when X < 10 -> "0";
%% gt_zero(_)             -> "".

%% gt_zero_ms(X) when X < 10     -> "00000";
%% gt_zero_ms(X) when X < 100    -> "0000";
%% gt_zero_ms(X) when X < 1000   -> "000";
%% gt_zero_ms(X) when X < 10000  -> "00";
%% gt_zero_ms(X) when X < 100000 -> "0";
%% gt_zero_ms(_)                 -> "".
