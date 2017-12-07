%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	appmServer.erl %
%%% Author:	etxarnu
%%% Description:
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(appmServer).
-behaviour(gen_server).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/18').
-date('2017-12-01').

%%% ----------------------------------------------------------
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% Rev        Date       Name     What
%%% < Removed older history >
%%% R3A/1     20140922   etxarnu   Added CXP_REV to env for application
%%% R3A/2     20140929   etxarnu   Added sysAdmin
%%% R3A/4     20141002   etxpeno   TR HS98250
%%% R3A/5     20141002   etxpeno   Set heartCmd before reboot to NL
%%% R3A/6     20141013   etxarnu   clear bootcounter in reset_restart_list
%%% R3A/7     20141013   etxarnu   Only clear bootcounter on arm
%%% R3A/8     20141014   etxarnu   Don't change heart command if SWM has set it
%%% R3A/9     20141016   etxarnu   Handle case when LD_LIBRARY_PATH is not set
%%% R3A/10    20141020   etxarnu   Improved restart reason in syslog
%%% R3A/11    20141022   etxarnu   Corrected make_esi
%%% R3A/12    20141105   etxjotj   Added call to log startup time
%%% R3A/13    20141107   etxarnu   Added dev_patches to LD_LIBRARY_PATH for arm
%%% R3A/14    20141117   erarafo   Typo fixed (it's -> its)
%%% R3A/15    20141119   etxarnu   Fix for multiple crashes leading to fast escalation
%%% R3A/18    20141121   etxarnu   Removed LD_LIBRARY_PATH for arm
%%% R3A/19    20141121   etxarnu   Added LD_LIBRARY_PATH for arm again (temp)
%%% R3A/20    20141121   etxjotj   Restart cycle measurements
%%% R3A/21-22 20141124   etxarnu   At manual restart, call EE directly to shorten restart time
%%% R3A/23    20141124   etxberb   Added is_avli/1.
%%% R3A/24    20141124   etxarnu   Only read pid in get_apps, not at start
%%% R3A/25    20141124   etxarnu   Read all pids into State at first get_apps call
%%% R3A/26    20141126   etxjotj   Added timestamp applications_starting
%%% R3A/27    20141127   etxarnu  For sim, don't call EE at manual restart
%%% R3A/28    20141128   etxarnu  For ppc, don't call EE at manual restart
%%% R3A/30    20141130   etxarnu  Set def_cpu_set before starting dyn program
%%% R3A/31    20141201   etxarnu  Added ns to pgm_info
%%% R3A/32    20141202   etxarnu  Now also for ppc, call EE at manual restart
%%% R3A/33    20141204   etxarnu  Added get_info_pgm/1
%%% R3A/35    20150107   etxarnu  Now also at crash escalation, call EE for
%%%                               restart
%%% R3A/36    20150108   etxarnu  Call ssh:stop() before cold restart
%%% R3A/37    20150115   etxarnu  Make restart_time_limit configurable
%%%                               Update restart_time to exclude reboot time
%%%                               Do a mnesia:sync_log at cold restart
%%% R3A/38    20150115   etxarnu  update_restart_info called after apps started
%%% R3A/39    20150120   etxarnu  Changed ssh:stop() to
%%%                               omc_api:prepare_to_shutdown()
%%% R3A/41    20150208   etxlg    Add start_internal_lm()/stop_internal_lm()
%%% R3A/42    20150209   etxlg    Allow mfa to be undefinded in
%%%                               start_internal_lm()
%%% R3A/44    20150218   etxarnu  Removed error_handler from process_info list
%%%                               to avoid false Jenkins failures
%%% R3A/45    20150223   etxberb  Removed AVLI node events from non-active DU.
%%% R3A/46    20150224   etxberb  Added restart functionality for cluster.
%%% R3A/47    20150317   etxarnu  Fixed bug that caused appmServer to crash if
%%%                               signal_to_pgm towards crashed program.
%%% R3A/48    20150319   etxarnu  Never return WARM in RESTART_TYPE
%%% R3A/49    20150401   etxlg    restart_complete towards OOT
%%% R3A/50    20150402   etxarnu  Only call restart_complete at first activate
%%% R3A/51    20150415   etxarnu  Let make_esi run in spawned process
%%% R3A/53    20150417   etxarnu  Added CXP_NO to env
%%% R3A/54    20150420   etxarnu  If program terminates and escalation=None,
%%%                               only info report
%%% R4A/1     20150424   etxpejn  Changed call to logEsi,
%%%                               to generate_esi_on_all_mps
%%% R4A/2     20150424   etxpejn  Merge from R3
%%% R4A/4     20150504   etxpeno  Update the handling of the operational LED
%%% R4A/5     20150520   etxpejn  Removed SystemLog and log_both & syslog
%%% R4A/6     20150525   etxberb  Changed clhI:role to clhI:core_state.
%%% R4A/7     20150617   etxberb  HT80765: Correction of Reason strings in llog
%%%                               (cause_avli, alh_to_appm & get_reason_arg).
%%% R4A/8     20150622   etxarnu  Added HW_PROD_NO and HW_PROD_REV env variables
%%% R4A/9     20150629   etxarnu  Added Warm restart.
%%                                Added softRt
%%% R4A/11    20150707   etxarnu  Added AVLI logging after Warm restart.
%%% R4A/15    20150819   etxarnu  Corrected warm restart for sim
%%% R4A/16    2015-08-20 etxberb  Added calls to alhI:warm_restart/1.
%%% R4A/17    2015-08-27 etxarnu  Added callback after EE warm restart
%%% R4A/18    2015-08-27 etxarnu  Handle reason_arg  ClockUpdateExceededMaxDiff
%%% R4A/22    2015-09-11 etxarnu  Cluster additions
%%% R4A/23    2015-09-11 etxberb  TR HU16863 corrected; Removed AVLI logging
%%%                               when program terminates without escalation.
%%% R4A/25    2015-09-21 etxtory  HU17506 (make_esi)
%%% R4A/26    2015-09-24 etxberb  * Added avli_get_rank/0,
%%%                                 legacy_nodeDown_ts_erase/0.
%%%                               * Moved nodeDown timestamp from APPM and
%%%                                 mnesia to ALH and file under /rcs/alh.
%%% R4A/27    2015-09-25 etxarnu  Added DUMP_DIR env variable
%%% R4A/28    2015-09-28 etxarnu  Remove dynamic programs from state when
%%%                               exiting
%%% R4A/31    2015-09-29 eolaand  Add code for tmpfs remount but don't call yet.
%%% R4A/32    2015-10-07 eolaand  Add tmpfs remount
%%% R4A/35    2015-10-08 etxarnu  Call swmI:force_auto_backup() at cold restart
%%% R4A/36    2015-10-12 etxarnu  Don't start/stop COM on regular at warm
%%%                               restart
%%% R4A/37    2015-10-13 etxarnu  Don't call SWM/COM/OMC on regular att restart
%%% R4A/38    2015-10-13 etxarnu  Bug fix in stop_com
%%% R4A/40    2015-10-23 etxarnu  Added wd_log and detection of file is removed.
%%% R4A/41    2015-10-27 etxarnu  Added start_shutdown_wd to handle hanging
%%%                               shutdown.
%%% R4A/43    2015-10-28 etxarnu  Changed init:reboot to do_cold_restart in
%%%                               revert(installation)
%%%                               Keep do_stop_lms in terminate for sim
%%% R4A/44    2015-11-10 etxarnu  Move cleaning of apptmp to before call to EE
%%%                                at warm
%%% R4A/45    2015-11-20 etxarnu  TR HU37699: don't escalate if reload backup
%%%                               started
%%% R5A/1     2015-11-18 etxarnu  Do avli logging just before call to EE at
%%%                               warm restart.
%%%                               Allow missing binaries for loadmodule but do
%%%                               error report (for sim)
%%% R5A/2     2015-11-23 etxarnu  Merged R4A/45
%%%                               Inhibit warm if warm already ongoing.
%%% R5A/4     2015-11-24 etxpeno  correct measurement of restart cycle
%%% R5A/5     2015-11-25 etxarnu  Added HW_MARKET_NAME
%%% R5A/6     2015-11-25 etxarnu  Fix HW_MARKET_NAME for sim
%%% R5A/7     2015-11-28 etxarnu  Wrong RestartReason in warm restart
%%%                               In do_cold_restart use appmPghServer call
%%%                               instead of os:cmd to pgh_restart binary
%%% R5A/8     2015-12-07 etxarnu  Set appmServer priority to high.
%%% R5A/9     2015-12-14 etxarnu  Removed obsolete code
%%% R5A/10    2016-01-20 etxarnu  Added comea_snmp/2,3,4
%%%                               Remove LD_LIBRARY_PATH from secure boards
%%%                               Added HW_SERIAL_NO
%%% R5A/12    2016-01-20 etxlg    IPv6. Extended API for start_internal_lm/1
%%%           2016-01-25 etxarnu  Replaced comea_snmp/2,3,4 with comea_snmp/1
%%% R5A/13    2016-01-26 etxtory  save /rcs/erlang/* at hw test (power)
%%% R5A/14    2016-02-01 etxarnu  Update run_comea_snmp to use sysEnv:rcs_root
%%% R5A/15    2016-02-02 etxarnu  Remove printouts from run_comea_snmp
%%% R5A/16    2016-02-03 etxlg    Option for autocleaning at internal_lm exit
%%% R5A/17    2016-02-04 etxarnu  Clear escalation states when not recently
%%%                               restarted
%%% R5A/18    2016-02-16 etxarnu  Special case for aicSnmp stop com
%%% R5A/19    2016-02-16 etxarnu  Don't use server for comea_snmp(stop) to
%%%                               avoid deadlock
%%% R5A/20    2016-02-19 etxarnu  Only stop snmpd if running when stop from AIC
%%% R5A/21    2016-02-25 etxarnu  TR HU60044 Remove obsolete applicationlog
%%%                               directories at upgrade confirm
%%% R5A/22    2016-02-25 etxarnu  Revert R5A/20
%%% R5A/23    2016-02-25 etxarnu  TR HU61382 Enable warm restart at upgrade
%%% R5A/24    2016-02-29 etxarnu  Remember that latest restart was warm
%%% R5A/25    2016-02-29 etxarnu  Added latest_restart/0
%%% R5A/26    2016-03-01 etxarnu  Consecutive warm restarts no handled correctly.
%%% R5A/27    2016-03-03 etxarnu  HU63384: only do 3 kicks of watchdog.
%%% R5A/28    2016-03-07 etxarnu  HU64325: Clear restart counters at warm restart
%%% R5A/29    2016-03-10 etxarnu  WP5132: Added get_pids/2
%%% R5A/31    2016-03-23 etxarnu  HU69659:Remove escalation in heart command after 10 min
%%% R5A/32    2016-03-30 etxarnu  Use cup --mount  for remounting tmpfs
%%%                               Added restart_hook.sh in HEART_CMD

%%% R5A/33    2016-04-01 etxarnu  Use appmWdKick to kick EE WD
%%% R5A/34-35 2016-04-14 etxarnu  HU75532: always enable warm ,
%%%                                both at restore and upgrade.
%%% R5A/36    2016-04-15 etxarnu  Added UP_NO and UP_REV env variables
%%% R5A/37    2016-04-18 etxarnu  Print UP_NO and UP_REV at startup
%%% R6A/1     2016-05-02 etxarnu  Added get_boot_count/0
%%% R6A/3     2016-06-01 etxarnu  Enable comea-snmp via appm
%%% R6A/4     2016-06-01 etxarnu  Disable comea-snmp via appm
%%% R6A/5     2016-06-09 etxarnu  If rollback_time is negative, ignore ssh_logins
%%% R6A/6     2016-06-13 erarafo  WP5493: Env variable STRUCT_IS_ENCODED_AS
%%% R6A/7     2016-06-16 etxarnu  Enable comea-snmp via appm
%%% R6A/8     2016-06-16 etxarnu  Disable comea-snmp via appm
%%% R6A/9     2016-06-20 etxarnu  Take absolute value of Time in get_revert_timeouget_rcs_ent
%%%                               In comea_snmp(stop_from_aic) do a pkill of snmpd
%%%                               Enable comea-snmp via appm
%%%                               HU91343: Added sleep 20 in HEART_CMD
%%% R6A/13    2016-07-04 etxarnu  Wait until comea-snmp  ready before return
%%% R6A/14    2016-07-08 etxarnu  Enable comea-snmp via appm
%%% R6A/15    2016-07-16 etxarnu  Use wrapper script to kill snmpd
%%% R6A/16    2016-08-18 etxarnu  HU91343: removed logging in heart command
%%% R6A/17    2016-09-05 etxarnu  handle reset_escalation in wfr state
%%% R7A/1     2016-09-16 etxarnu  Disable warm restart for vrcs
%%% R7A/2_git1 2016-10-05 egjknpa Update manual restart for vrcs
%%% R7A/2_git2 2016-10-25 etxarnu Bug in do_restart_piu(cold,.. , 
%%% R7A/2     2016-11-01 etxderb  HV37278: make_esi / clean old gpg
%%% R7A/2_git3 2016-10-31 etxaldu  Disable heart beat replies after heal request 
%%% R7A/3     2016-11-09 etxarnu  Fix start_shutdown_wd to step boot_count if needed
%%% R8A/1     2016-11-15 etxarnu  Kill possible running comea-snmp in comea_snmp(stop)
%%% R8A/2     2016-11-16 etxarnu  Added XCM environment variables
%%% R8A/3     2016-12-02 etxarnu  HV46825: do not set revert_state in get_esc_rest_type
%%% R8A/4     2016-12-19 etxarnu  XCM variables only for VRCS
%%% R8A/6     2016-12-20 etxpeno  remove support for "struct as object"
%%% R9A/1     2017-01-20 etxarnu  Added handling dynamic pgms in pgroup
%%% R9A/2     2017-02-16 etxarnu  XCM variables changed
%%% R9A/3     2017-02-24 etxarnu  XCM variables changed
%%% R9A/4     2017-02-27 etxarnu  Added pg_restart coli commands 
%%% R9A/5     2017-03-02 etxarnu  pg_restart disabled also after upgrade
%%%           2017-03-15 etxarnu  Added Reason to send_heal                    
%%%                               per default
%%% R9A/6     2017-03-23 etxarnu  Added AVLI and llog logging for pg_restart
%%%  git      2017-04-04 etxarnu  At cyclic restart, don't start applications
%%%  git      2017-04-06 etxarnu  In reset_restart_list, clear cold counter
%%% R10A/1    2017-05-18 edamkon  Fix for HV69686: prevent dynamic program start if
%%%                               node is in warm restart
%%% R10A/2    2017-05-19 etxarnu  Added pgmterm exit(0) handling
%%% R10A/3    2017-05-22 etxarnu  Bug fix in check_for_escalation
%%% R10A/4    2017-05-23 etxarnu  Bug fix in start_program for dynamic pgms
%%% R10A/5    2017-05-30 etxarnu  Added restart_cold callback handling
%%%                               Changed os:putenv to ets-table
%%%                               Added appm_ets as generic table
%%%                               Start PGH interface with new version
%%% R10A/6    2017-06-02 etxarnu  Added State after init:reboot for simulated
%%% R10A/6    2017-06-07 etxarnu  Do not destroy dynamic program if exit 0
%%% R10A/8    2017-06-08 etomist  HV93805
%%% R10A/10   2017-06-20 etxarnu  Added signal_to_internal_pgm/2
%%% R10A/11   2017-06-20 etxarnu  get_mw_cxp_info updated
%%% R10A/12   2017-06-28 etxarnu  HV9xxxx: Corrected bug in handle_ipgm_stopped
%%% R10A/13   2017-07-07 etomist  HV97965
%%% R11A/1    2017-09-04 etxjotj  Moved cup call to SWM
%%% R11A/2    2017-09-06 etxarnu  Added environment variables COMPUTE_NAME
%%%                               and SYSTEM_UUID (still only for vrcs nodes)
%%% R11A/3    2017-09-06 etxarnu  Bug fix in get_compute_name_and_uuid
%%% R11A/4    2017-09-14 ebabmat  HV99097
%%% R11A/5    2017-09-15 etxarnu  Added environment variables COMPUTE_NAME
%%%                               and SYSTEM_UUID for physical nodes
%%% R11A/6    2017-09-19 etxarnu  Added environment variables COMPUTE_NAME
%%%                               and SYSTEM_UUID for R-VNFM
%%% R11A/7    2017-09-19 etxarnu  Added environment variables COMPUTE_NAME
%%%                               and SYSTEM_UUID for rcssim
%%% R11A/8    2017-09-25 etxarnu  Added support for Lmhi_start_pgm_3, i.e.
%%%                               indication to appl at program term/crash
%%%                               Fix for HV96856 (exit0 and None at same time)
%%% R11A/9    2017-10-02 etxarnu  Added PGMTYPE (default or DP) to do_start_pgm
%%% R11A/10   2017-10-04 etxarnu  Added delayedKill for pgroups
%%% R11A/12   2017-10-09 etxarnu  Removed export_all and unused functions
%%% R11A/13   2017-10-10 estjako  Added envs in get_comea_envs fun
%%% R11A/14   2017-10-11 etxarnu  Exported RCS_MODE to applications
%%% R11A/15   2017-10-12 ekurnik  comea-snmp isDtlsSupported called through 
%%%                               os:cmd to get output
%%% R11A/16   2017-10-18 etxarnu  If start_dynamic_lm times out produce an
%%%                                  erl_crash_dump
%%% ----    ---------- -------  ------------------------------------------------
%%% R12A/1  2017-10-23 etxarnu  Added pgroup_cb callbacks
%%% R12A/2  2017-10-23 etxarnu  Bug fix.
%%% R12A/3  2017-10-23 ekurnik  Added OAM_NET_NS env var to isDtlsSupported call
%%% R12A/4  2017-10-24 etxarnu  Do not call write_llog for old rcssim
%%% R12A/6  2017-10-26 etxarnu  Add LD_LIBRARY_PATH also for vrcs again
%%% R12A/7  2017-10-26 etxlg    Do not log successful result at stop_internal_lm
%%% R12A/8  2017-10-30 etxarnu  Fix for dialyzer complaint
%%% R12A/9  2017-11-03 etxarnu  Store pg_restart_disabled in /rcs/appm/ to
%%%                             survive Data restore (HW39748)
%%% R12A/10 2017-11-06 etxarnu  Added cold_cb callbacks
%%%                             Do not crash if start_dynamic_lm times out
%%% R12A/11 2017-11-10 etxarnu  HW43399: changed GP_SEV to major
%%% R12A/12 2017-11-22 etxarnu  TK: Export BT_VARIANT if set
%%% R12A/13 2017-11-23 etxarnu  Add CpuSet to start_internal_lm
%%% R12A/14 2017-11-23 etxarnu  Log when pg_restart is enabled/disabled and
%%%                             when pg_restart maps to warm since disabled.
%%% R12A/16 2017-11-24 etxarnu  Exported disable_pg_restart/2
%%%                             Use comsaI:has_consul()
%%% R12A/17 2017-11-29 etxarnu  Exported call/1
%%% R12A/18 2017-12-01 etxarnu  HW48362:Bug in printout 
%%% --------  ---------- -------  ---END-OF-LOG---------------------------------

%-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
-export([start_link/0,
	 stop/0]).

-export([activate/0]).
-export([continue_after_upgrade/0]).
-export([confirm/0]).

% used from LMHI
-export([start_dynamic_lm/1,
	 stop_dynamic_lm/2,
	 signal_to_pgm/3,
	 get_pids/2,
	 get_itc_port/0]).

% erlang interface for starting dynamic programs
-export([start_internal_lm/1,
	 signal_to_internal_pgm/2,
	 stop_internal_lm/1]).

% used from PRI
-export([restart_piu/5]).

%% used from SWM, CLH and other RCS blocks
-export([restart_node/3]).
-export([inhibit_escalation/1]).
-export([cancel_inhibit/0]).
-export([set_revert_state/1]).

%% appmPghServer callbacks
-export([pgmcrash/2,
	 pgmterm/2,
	 pgmtermex/2,
	 warmrestart/1]).

%% used from appmHbClient
-export([pgmhanging/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2,code_change/3]).

%% Used from ALH
-export([avli_get_rank/0,
	 legacy_nodeDown_ts_erase/0]).

%% Used from comea to start comea-snmp with proper capabilities
-export([comea_snmp/1]).
-export([do_run_comea_snmp/2]).
-export([enable_comea_via_appm/0]).
-export([disable_comea_via_appm/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal exported functions

-export([call_restart_cold_cb/2]).
-export([call/1]).
-export([create_mps/1]).
-export([inhibit_timeout/0]).
-export([job/4]).
-export([make_esi/0]).
-export([remove_rollback_esi/0]).
-export([restart/5]).
-export([restart_state/4]).
-export([revert_check/0]).
-export([update_heart_cmd/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Functions for test
-export([start_lms/0,
	 stop_lms/0]).

-export([start_lm/1,
	 stop_lm/1]).

-export([start_group/1,
	 stop_group/1]).

-export([start_lm/2,
	 signal_lm/2,
	 signal_lm/3,
	 stop_lm/2]).

-export([get_state/0]).
-export([get_apps/0]).
-export([get_apps_more/0]).
-export([get_apps/1]).
-export([get_pgms/0]).
-export([get_groups/0]).
-export([get_groups/1]).
-export([get_info_pgm/1]).
-export([get_info_grp/1]).
-export([get_pgroup_lms/2]).

-export([get_info/0, get_info/1]).
-export([get_mw_cxp_info/0, get_total_env/0, get_rcs_env/1]).
-export([get_bash_envs/1]).

-export([get_boot_count/0]).
-export([set_oss_alive/1]).

-export([reset_restart_list/0]).
-export([reset_restart_list/1]). % for COLI

-export([enable_warm_restart/1]).% for COLI
-export([disable_warm_restart/1]).% for COLI
-export([is_warm_restart_enabled/0]).% for COLI

-export([enable_pg_restart/1]).% for COLI
-export([disable_pg_restart/1]).% for COLI
-export([disable_pg_restart/2]).% for internal use
-export([is_pg_restart_enabled/0]).% for COLI
-export([restart_pg/1]).% for COLI

-export([latest_restart/0]).% for COLI

-export([set_rollback_time/1]).% for COLI
-export([set_restart_time_limit/1]).% for COLI
-export([reset_restart_time_limit/0]).% for COLI

-export([recent_restarts/3]).
-export([get_revert_state/0]).% for COLI
-export([get_revert_timeout/0]).% for COLI
-export([get_esc_rest_type/1]).% for COLI


-include("appm.hrl").
-include("alhI.hrl").
-include("eqs_vii.hrl").
-include("vnfcI.hrl").

-define(RCS_ROOT,sysEnv:home_dir()).
-define(DEV_PATCHES,filename:join(?RCS_ROOT,"dev_patches")).

%%%if file exist, LMs not started
-define(DISABLE_FILE, ?DEV_PATCHES ++ "/no_lms").

-define(DEFAULT_WD_INTERVAL, 30000).
-define(DEFAULT_WD_KICKS, 3).

-define(UNEXPECTED_TIME, 60*1000*10). %10 minutes
-define(MAX_KILL_DELAY, 3000).  % Maximum time to delay program killing
-define(WFUG_TOUT,(5 * 60 * 1000) ).  % 5min timeout for upgrade
-define(ESC_RST_TOUT,(5 * 60 * 1000) ).  % 5 min timeout for reset of escalation counters

-define(DEF_RC_CB_TIMEOUT,(2 * 60 * 1000) ). %2 minutes max to wait for restart_cold callbacks


-define(RE_ALARM, 'RollbackEscalationStarted').
-define(GP_ALARM, 'SystemGeneralProblem').
-define(GP_DN, [<<"ManagedElement=1">>,
		<<"SystemFunctions=1">>,
		<<"SysM=1">>
	       ]).
-define(RE_DN, ?GP_DN).
-define(RE_SEV, major).
-define(GP_SEV, major).


-define(RESTART_TRIES, 10).

-define(ITC_PORT_NAME, <<"LMHI">>).

-define(LMHI_PGM_TERM_IND, 16#01860004).
-define(LMHI_PGM_CRASH_IND, 16#01860005).


-define(HI32BITS,   16#FFFF0000).
-define(LOW32BITS,  16#0000FFFF).
-define(PGMTERM_SIGNAL,  16#10000).
-define(PGMTERM_EXIT,    16#20000).
-define(PGMTERM_EXIT0,   ?PGMTERM_EXIT).

-define(NS_FILE, "/home/sirpa/oam_net_namespace").


-record(pgm_info, {appm_id,
		   pgh_id,
		   name_cxc,
		   dynamic = false,
		   grp,
		   ns = "",
		   mboxid = 0,
		   args,
		   cpu_set,
		   pid = 0,
		   location,
		   escalation,
		   maxRestarts,
		   maxTimeout,
		   restart_list = []
		}).

%simplified incarnation of above, used for "internal" LMs
-record(ipgm_info, {appm_id,		%as above, integer()
		    pgh_id,		%as above, ingeter()
		    name,		%name given by API user (ident/debug)
		    ns = "",		%namespace, binary() (just for debug)
		    args,		%what is started (kept for debug)
		    envs,		%what is started (kept for debug)
		    cpu_set,            %bitmap for which cpu(s) to use
		    mfa,	       	%callback to run when pgh sends exit
		    owning_pid,		%stop lm when "owning" pid exits
		    autoclean		%true|false, do pgh_destroy at exit
		   }).

-record(grp_info, {grp_name,
		   pgmList = [], %[{appmId,{name,cxc}}]
		   escalation,
		   maxRestarts,
		   maxTimeout,
		   restart_list = []}).

-record(state, {pgms = [], % [#pgm_info]
                grps = [], % [#grp_info]
                ipgms = [], % [#ipgm_info] kept aside used for "internal" LMs
                uid,
                gid,
                cpu_info,
                next_appm_id=0,
                default_cpu_set = [],
                inhibit_tref,
                esc_tref,
                wfr = false,  %wfr=Wait for revert or restore
                wfr_crash = false,  %crash during wfr
                monitored = [], %list of pids being monitored
                activated = false,
                revert_esi_generated = undefined}).

%%====================================================================
%% Startup functions
%%====================================================================
start_link() ->
    {UpNo,UpRev} = get_up_no_rev(),
    log(info,"Starting.   UP= ~s-~s~n", [UpNo,UpRev]),
    log(info,"~p~n", [get_compute_name_and_uuid()]),
    RevState = get_revert_state(),
    disk_log("Starting.   ----------------------------------~n"
	     "                           UP = ~s-~s ~n"
	     "                           Revert state = ~s~n",
	     [UpNo,UpRev,RevState]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    call(stop).



%%%====================================================================
%%% Store info restart indication for vnfc
%%%  RestartInd:
%%%   ?RESTART_USR - means restart manually ordered
%%%   ?RESTART_APP - means pgm escalation has happened
%%%
set_vnfc_info(RestartInd) ->
    set_vnfc_info(sysEnv:vrcs(), RestartInd).

set_vnfc_info(true, RestartInd) ->
    log(info, "VNFC: RestartInd: ~p",[RestartInd]),
    mnesia:dirty_write(#appmNodeTable{key = vnfc_restart_info,
                                      value = RestartInd});
set_vnfc_info(_, _) ->
    ok.

%%%====================================================================
%%% Remove restart indication for vnfc
%%%
handle_vnfc_info() ->
    handle_vnfc_info(sysEnv:vrcs()).

handle_vnfc_info(true) ->
    case mnesia:dirty_read(appmNodeTable, vnfc_restart_info) of
        [#appmNodeTable{value = RestartInd}] ->
            vnfcI:restart_ind(RestartInd),
            mnesia:dirty_delete({appmNodeTable, vnfc_restart_info});
        [] ->
            vnfcI:restart_ind(?RESTART_OSMW),
            ok
    end;
handle_vnfc_info(_) ->
    ok.


%%%====================================================================
%%% Store info about last warm/cold restart in mnesia
%%%
%%%  Escalation states:
%%%   normal
%%%   warm - previous crash was warm
%%%   cold - previous crash was cold
%%%   cold_with_test - previous crash was cold_with_test
%%%   fail, revert state will be stored a file in /rcs/revert_state
%%%  Reason:
%%%   manual - means restart manually ordered
%%%   crash  - means escalation has happened
%%%

set_restart_info(manual,_NextState) ->
    mnesia:dirty_delete({appmNodeTable, {restart_info, node()} });
set_restart_info(crash,NextState) ->
    Time = os_now(),
    mnesia:dirty_write(#appmNodeTable{key = {restart_info, node()},
				      value = {Time,  NextState}}).

%%% Update Time in restart_info to exclude time taken to reboot (incl hwtest)
update_restart_info() ->
     case mnesia:dirty_read(appmNodeTable, {restart_info, node()}) of
	[#appmNodeTable{value = {_, NextState } }] ->
	     Time = os_now(),
	     mnesia:dirty_write(#appmNodeTable{key = {restart_info, node()},
					       value = {Time, NextState }});
	[] ->
	    ok
    end.


recently_restarted() ->
    case mnesia:dirty_read(appmNodeTable, {restart_info, node()}) of
	[#appmNodeTable{value = {Time,  Next} }] ->
	    {(os_now() - Time) < restart_time_limit(),  Next};
	[] ->
	    {false,undefined}
    end.

os_now() ->
   {Msec,Sec,_} = os:timestamp(),
    1000000*Msec + Sec.

restart_time_limit() ->
    case mnesia:dirty_read(appmNodeTable, restart_time_limit) of
	[#appmNodeTable{value = Time }] ->
	    Time;
	[] ->
	    %% 5 minutes default value
	    5 * 60
    end.

set_restart_time_limit(Min) when Min >= 0 ->
    mnesia:dirty_write(#appmNodeTable{key = restart_time_limit,
				      value = Min * 60}).
reset_restart_time_limit()  ->
    mnesia:dirty_delete({appmNodeTable, restart_time_limit }).

%%====================================================================
get_esc_rest_type(RestartLevel) ->
    get_esc_rest_type(RestartLevel,readonly).


get_esc_rest_type(RestartLevel,_)
  when RestartLevel == no_restart;
       RestartLevel == wfr;
       RestartLevel == pgm;
       RestartLevel == pgm_grp->
    RestartLevel;
get_esc_rest_type(RestartLevel,Mode) ->
    case recently_restarted() of
	{false, _} ->
	    case Mode of
		update ->
		    mnesia:dirty_delete({appmNodeTable,{restart_info, node()}});
		readonly ->
		    ok
	    end,
	    RestartLevel;
	{true, warm} -> cold;
	{true, cold} -> cold_with_test;
	{true, cold_with_test} -> revert;
	{true, revert} -> fail_revert
    end.

%%====================================================================
init([]) ->
    process_flag(trap_exit, true),
    process_flag(priority, high),
    put(state, starting),
    start_watchdog_kicker(),
    timer:apply_after(?UNEXPECTED_TIME,?MODULE,update_heart_cmd,["Unexpected",false]),

    swmI:copy_upgrWindow_table(appmNodeTable),  % SWM keeps track of upgrade
						% window or not. I need to copy
						% what's in there no matter
						% which state we're in now.
    restart_revert_check(sysEnv:role()),
    remount_apptmp_size(sysEnv:target()),
    update_operational_led(),

    {ok, Options} = appmPghServer:req_init([{notify,true},{version,1}]),
    DefaultCpuSet = proplists:get_value(cpus_conf, Options, []),

    CpuInfo = lists:foldl(
		fun(Cpu, Acc) ->
			orddict:store(Cpu, [], Acc)
		end, orddict:new(), DefaultCpuSet),
    Grps = [#grp_info{grp_name = PgName,
		      pgmList = [], % filled in when LM started
		      escalation = Esc,
		      maxRestarts = MaxR,
		      maxTimeout = MaxT} ||
	       {PgName,Esc,MaxR,MaxT,_LmList} <-
		   appmAppData:get_pgroup_data()],

    State = #state{grps = Grps,
		   uid = get_uid(),
		   gid = get_gid(),
		   cpu_info = CpuInfo,
		   default_cpu_set = lists:usort(DefaultCpuSet) },

    ets:new(appm_ets, [set, public, named_table]),
    ets:new(appm_add_board, [set, public, named_table]),
    ets:new(appm_add_ext_board, [set, public, named_table]),
    ets:insert(appm_ets,{esc_cnt,"-e"}),

    {_, CxpProdNr, CxpRev} = swmI:get_cxp_source(code:lib_dir(appm)),
    ets:insert(appm_ets,{mw_cxp_info,CxpProdNr, CxpRev}),


    {ok, State}.

activate() ->
    log(info,"activate~n", []),
    cast(activate).

continue_after_upgrade()->
    log(info,"continue_after_upgrade called~n", []),
    cast(continue_after_upgrade).


confirm() ->
    proc_lib:spawn(fun() -> remove_obsolete_applogdirs() end),
    ok.
%%====================================================================
%%  Erlang API for starting programs
%%====================================================================
start_internal_lm(PropList) ->
    case proplists:get_value(args, PropList) of
	undefined ->
	    {error, "Missing property: args"};
	[] ->
	    {error, "Empty property: args"};
	Args when is_list(Args) ->
	    Name = proplists:get_value(name, PropList, "unknown"),
	    Ns = proplists:get_value(ns, PropList, <<>>),
	    Envs = proplists:get_value(envs, PropList, []),
	    CpuSet = proplists:get_value(cpu_set, PropList, 0),
	    CbMFA = proplists:get_value(mfa, PropList),
	    Owner = proplists:get_value(owner, PropList),
	    Ugid = proplists:get_value(ugid, PropList, undefined),
	    AutoClean = proplists:get_value(autoclean, PropList, false),
	    call({start_internal_lm, Name, Args, Envs, CpuSet,
		  Ns, CbMFA, Owner, Ugid, AutoClean})
    end.

stop_internal_lm(AppmId) when is_integer(AppmId) ->
    call({stop_internal_lm, AppmId}).

%%====================================================================
%% LMHI functions
%%====================================================================
start_dynamic_lm({Name, LmId, DuId, CpuSet, Ns,  Args, MboxId}) ->
    NameCxc = {b2l(Name), b2l(LmId)},
    log(info,"start_dynamic_lm(~p, ~p, ~p, ~p, ~p, ~p) called~n",
	[NameCxc, DuId, CpuSet, Ns, Args, MboxId]),
    Res =
	try 
	    call(DuId,{start_dynamic_lm,NameCxc, CpuSet, Ns, Args, MboxId}) 
	catch
	    Error:Reason ->
		log(error,"start_dynamic_lm failed, reason:~p~n",
		    [{Error,Reason}]),
		{error, Reason} %% this is almost certainly a timeout
	end,
    log(info,"start_dynamic_lm(~p, ~p, ~p, ~p, ~p, ~p) "
	"returns with result ~p~n",
	[NameCxc, DuId, CpuSet, Ns, Args, MboxId, Res]),
    Res.

stop_dynamic_lm(DuId,AppmId) ->
    log(info,"stop_dynamic_lm(~p, ~p) called~n",[DuId,AppmId]),
    Res =   try
                call(DuId,{stop_dynamic_lm, AppmId})
            catch
                Error:Reason ->
                    log(info,"stop_dynamic_lm call error: ~n~p~n", [{Error,Reason}]),
                    {error, Reason} %% this is almost certainly a timeout
            end,
    log(info,"stop_dynamic_lm(~p, ~p) returns with result ~p~n",[DuId,AppmId,Res ]),
    Res.


signal_to_pgm(DuId,AppmId, SigNo) ->
    log(info,"signal_to_pgm(~p, ~p, ~p) called~n",[DuId,AppmId,SigNo]),
    Res = call(DuId,{signal_to_pgm, AppmId, SigNo}),
    log(info,"signal_to_pgm(~p, ~p, ~p) returns with result ~p~n",[DuId,AppmId,SigNo,Res ]),
    Res.

signal_to_internal_pgm(AppmId, SigNo) ->
    log(info,"signal_to_internal_pgm(~p, ~p) called~n",[AppmId,SigNo]),
    Res = call({signal_to_internal_pgm, AppmId, SigNo}),
    log(info,"signal_to_internal_pgm(~p, ~p) returns with result ~p~n",
	[AppmId,SigNo,Res]),
    Res.

get_pids(DuId,AppmId) ->
    call(DuId,{get_pids, AppmId}).

%%====================================================================
%%
%%====================================================================
restart_piu(MpId, RestartType, RestartRank, RestartEscalation, RestartCause)
  when RestartRank == cold orelse
       RestartRank == cold_with_test orelse
       RestartRank == warm ->
    case clhI:own_mp_id() of
	MpId ->
	    restart(piu,
		    RestartType,
		    RestartRank,
		    RestartEscalation,
		    cause_avli(RestartCause));
	_ ->
	    ErlNode = clhI:erlang_node(MpId),
	    rpc:cast(ErlNode, ?MODULE, restart, [piu,
						 RestartType,
						 RestartRank,
						 RestartEscalation,
						 cause_avli(RestartCause)])
    end,
    ok.

%%====================================================================
%%
%%====================================================================
inhibit_escalation(Time) ->
    broadcast(clhI:erlang_nodes(), ?MODULE, inhibit_escalation, [Time]),
    cast({inhibit_escalation,Time}).

cancel_inhibit() ->
    broadcast(clhI:erlang_nodes(), ?MODULE, cancel_inhibit, []),
    cast(cancel_inhibit).

inhibit_timeout() ->
    cast(inhibit_timeout).


%%====================================================================
restart_node(RestartType, RestartRank, RestartCause) ->
    Scope = node,
    RestartEscalation = false,   % TODO: Check if this is correct...
    broadcast(clhI:erlang_nodes(), ?MODULE, restart, [Scope,
						      RestartType,
						      RestartRank,
						      RestartEscalation,
						      RestartCause]),
    restart(Scope, RestartType, RestartRank, RestartEscalation, RestartCause).

%%====================================================================
restart(Scope, RestartType, RestartRank, RestartEscalation, RestartCause) ->
    %% Restart cycle measurements
    sysInitI:log_restart_time(),
    case whereis(?MODULE) of
	Pid when is_pid(Pid) ->
	    cast({restart,
		  Scope,
		  RestartType,
		  RestartRank,
		  RestartEscalation,
		  RestartCause});
	_ ->
	    %% In some cases, restart is ordered during startup, before all
	    %% server processes are started.
	    add_early_MFA_state({?MODULE, restart_state, [Scope,
							  RestartRank,
							  RestartCause]})
    end,
    ok.

restart_state(Scope, RestartRank, RestartCause,State) ->
    do_restart_piu(RestartRank,
		   shutdownCommand,
		   RestartCause,
		   cause_appm(Scope, RestartCause),
		   State).

%%====================================================================
%%
%%====================================================================
broadcast([Node | Tail], M, F, A) ->
    rpc:cast(Node, M, F, A),
    broadcast(Tail, M, F, A);
broadcast([], _, _, _) ->
    ok.

%%====================================================================
cause_appm(piu, _) ->
    manual;
cause_appm(node, RestartCause) ->
    alh_to_appm(RestartCause).

%%====================================================================
cause_avli(RestartCause) ->
    case is_avli(RestartCause) of
	false ->
	    ?ALH_TAG_ExtRestartRequest;
	{true, AvliCause} ->
	    AvliCause
    end.

%%====================================================================
is_avli([$a,$v,$l,$i,$: | Tail]) ->
    {true, Tail};
is_avli(_) ->
    false.

%%====================================================================
%% appmPghServer callbacks
%%====================================================================
pgmcrash(PghId, Rank) ->
    cast({pgmcrash, PghId, Rank}),
    ok.

pgmterm(PghId, Reason) ->
    cast({pgmterm, PghId, Reason}),
    ok.

pgmtermex(PghId, Reason) ->
    cast({pgmterm, PghId, Reason}),
    ok.

warmrestart(Result) ->
    cast({warmrestart_rsp, Result}),
    ok.

%%====================================================================
%% Used by Heartbeat (appmHbClient)
%%====================================================================
pgmhanging(NameCxc) ->
    cast({pgmhanging, NameCxc}),
    ok.

%%%%%%%%%%%%%%%%%%%%%
%%% for test
%%%%%%%%%%%%%%%%%%%%%
reset_restart_list() ->
    call(reset_restart_list).
reset_restart_list(_) ->
    call(reset_restart_list).

get_boot_count() ->
    get_boot_count(sysEnv:rcs_mode()).

get_boot_count(target) ->
    list_to_integer(
      os:cmd("grep boot_count /var/log/syslog | "
	     "tail -1 | awk -F= '{ print $2 }' | "
	     "awk -F, '{ print $1 }' | tr -d ' \n'"));
get_boot_count(_) -> "Only for target environment".


%%%%%%%%%%%%%%%%%%%%%
%%%  For COLI.


%%%  Warm restart automatically enabled at upgrade and restore
%%%  since /home/sirpa is cleared then

-define(WARM_DISABLE,
	filename:join([sysEnv:home_dir(), "warm_restart_disabled"]) ).

enable_warm_restart(_) ->
    os:cmd(["rm -f ",?WARM_DISABLE]).

disable_warm_restart(_) ->
    os:cmd(["touch ",?WARM_DISABLE]).

is_warm_restart_enabled() ->
    case file:read_file_info(?WARM_DISABLE) of
	{ok, _} -> false;
	_ -> true
    end.

%%%  PG restart survivies at upgrade and restore
%%%  since /rcs/appm is not cleared then (Will change later
%%%  when PG restart is working fully)

-define(PG_DISABLE,
	filename:join([sysEnv:rcs_dir(), "appm", "pg_restart_disabled"]) ).

enable_pg_restart(X) ->
    enable_pg_restart(X,"from coli").
enable_pg_restart(_X,String) ->
    log(info, "Enabled program group restart ~s~n",[String]),
    os:cmd(["rm -f ",?PG_DISABLE]).

disable_pg_restart(X) ->
    disable_pg_restart(X,"from coli").
disable_pg_restart(_X,String) ->
    log(info, "Disabled program group restart ~s~n",[String]),
    filelib:ensure_dir(?PG_DISABLE),
    os:cmd(["touch ",?PG_DISABLE]).

is_pg_restart_enabled() ->
    case file:read_file_info(?PG_DISABLE) of
	{ok, _} -> false;
	_ -> true
    end.

restart_pg(PG) ->
    call({?FUNCTION_NAME,PG}).
    
%%%%%%%%%%%%%%%%%%%%%
latest_restart() ->
    call(?FUNCTION_NAME).


%%%%%%%%%%%%%%%%%%%%%
set_rollback_time(["help"]) ->
    io:format("\nSets the timeout before a revert is done."
	      "\nIf no parameter is given the current value is shown\n\n", []);

set_rollback_time([Val]) when is_list(Val) ->
    Timeout = list_to_integer(Val),
    set_rollback_time(Timeout);

set_rollback_time([]) ->
    T=get_stored_rt(),
    Timeout = integer_to_list(abs(T)),
    io:format("Current value (minimum 60) ~s~n", [Timeout]),
    if
	T < 0 ->
	    io:format("\nLogins does not abort rollback.\n\n", []);
	true -> ok
    end;

set_rollback_time(Timeout) when is_integer(Timeout) ->
    if
	Timeout < 0 ->
	    %% If negative value, ignore that ssh has been used, i.e. possible
	    %% to log in and use /labonly/wrat killm to force escalation
	    io:format("\nLogins does not abort rollback from now on.\n\n"),
	    set_oss_alive(ignore);
	true ->
	    case is_oss_alive() of
		ignore ->
		    io:format("\nLogins do abort rollback from now on.\n\n"),
		    set_oss_alive(false);
		_ ->
		    ok
	    end
    end,
    mnesia:dirty_write({appmNodeTable,revert_timeout, Timeout}).

get_revert_timeout() ->
    max(abs(get_stored_rt()),60).
get_stored_rt() ->
    case mnesia:dirty_read({appmNodeTable,revert_timeout}) of
	[#appmNodeTable{value=Time}] -> Time;
	%%minimum revert time 60 seconds
	[] -> 3600
    end.

start_lms() ->
    call(?FUNCTION_NAME).

stop_lms() ->
    call(?FUNCTION_NAME).

start_lm(Name) ->
    start_lm(Name, 0).
start_lm(Name, Where) ->
    call({start_lm, Name, Where}).

stop_lm(Name) ->
    stop_lm(Name, 0).
stop_lm(Name, Where) ->
    call({stop_lm, Name, Where}).

signal_lm(Name,SigNo) ->
    signal_lm(Name,SigNo, 0).
signal_lm(Name,SigNo, Where) ->
    call({signal_lm, Name,SigNo, Where}).

start_group(Name) ->
    start_group(Name, 0).
start_group(Name, Where) ->
    call({start_group, Name, Where}).

stop_group(Name) ->
    stop_group(Name, 0).
stop_group(Name, Where) ->
    call({stop_group, Name, Where}).

get_apps(pretty) ->
    log(info,"~p~n",[call(get_apps)]).

get_apps() ->
    call(?FUNCTION_NAME).

get_apps_more() ->
    call(?FUNCTION_NAME).

get_groups(pretty) ->
    log(info,"~p~n",[call(?FUNCTION_NAME)]).
get_groups() ->
    call(?FUNCTION_NAME).

get_info() ->
    call(?FUNCTION_NAME).

get_state() ->
    call(?FUNCTION_NAME).

get_pgms() ->
    S = call(get_state),
    S#state.pgms.


set_oss_alive(true) ->
    mnesia:dirty_write(#appmNodeTable{key = oss_alive,
				      value = true});

set_oss_alive(ignore) ->
    mnesia:dirty_write(#appmNodeTable{key = oss_alive,
				      value = ignore});

set_oss_alive(false) ->
    mnesia:dirty_delete({appmNodeTable, oss_alive}).

get_info_pgm(Pgm) ->
    lists:flatten(call({get_info, {pgm,Pgm}})).

get_info_grp(Grp) ->
    call({get_info, {grp,Grp}}).

get_info(pretty) ->
    log(info,"~p~n",[call(get_info)]);
get_info(Type = {grp,_Grp})  ->
    call({get_info, Type});
get_info(Type = {pgm,_Pgm})  ->
    call({get_info, Type});
get_info(Type) when Type =:= pgms;
		    Type =:= ipgms;
		    Type =:= grps;
		    Type =:= uid;
		    Type =:= gid;
		    Type =:= cpu_info;
		    Type =:= default_cpu_set ->
    call({get_info, Type});

get_info(Type) ->
    erlang:error(badarg, Type).


%%====================================================================
legacy_nodeDown_ts_erase() ->
    case whereis(?MODULE) of
	Pid when is_pid(Pid) ->
	    call(legacy_nodeDown_ts_erase);
	_ ->
	    timer:sleep(100),
	    legacy_nodeDown_ts_erase()
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
handle_call({start_internal_lm, Name, Args, Envs, Ns, CbMFA, Owner, Ugid,
	     AutoClean}, From, State) ->
    handle_call({start_internal_lm, Name, Args, Envs, 0, 
		 Ns, CbMFA, Owner, Ugid, AutoClean},
		From, State); 

handle_call({start_internal_lm, Name, Args, Envs, CpuSet,
	     Ns, CbMFA, Owner, Ugid, AutoClean},
	    _From, #state{ipgms = InIpgms} = State) ->
    {Uid, Gid} =
	case Ugid of
	    {UserId, GroupId} when is_integer(UserId), is_integer(GroupId) ->
		Ugid;
	    _ ->
		{State#state.uid, State#state.gid}
	end,
    try spawn_pgm(Args, Envs, CpuSet, Ns, [], default, unlimited, Uid, Gid) of
	PghId ->
	    NewMonitored = add_monitor(Owner, State),
	    {AppmId, NewState} = get_next_appm_id(State),
	    Ipgm = #ipgm_info{appm_id = AppmId,
			      pgh_id = PghId,
			      name = Name,
			      ns = Ns,
			      args = Args,
			      envs = Envs,
			      cpu_set = CpuSet,
			      mfa = CbMFA,
			      owning_pid = Owner,
			      autoclean = AutoClean
			     },
	    {reply, {ok, AppmId}, NewState#state{ipgms = [Ipgm | InIpgms],
						 monitored = NewMonitored}}
    catch
	throw:Reply ->
	    {reply, {error, Reply}, State}
    end;

handle_call({stop_internal_lm, AppmId}, _From,
	    #state{ipgms = InIpgms} = State) ->
    case lists:keyfind(AppmId, #ipgm_info.appm_id, InIpgms) of
	#ipgm_info{pgh_id = PghId} ->
	    case appmPghServer:req_destroy_pgm([{pgm_id, [PghId]}]) of
		{error, Reason} ->
		    log(error,
			"Killing internal program(~.16B) has failed~n"
			"Reason: ~p~n", [ PghId, Reason]),
		    {reply, {error, Reason}, State};
		{ok, _} ->
		    NewIpgms = lists:keydelete(AppmId, #ipgm_info.appm_id,
					       InIpgms),
		    {reply, ok, State#state{ipgms=NewIpgms}}
	    end;
	false ->
	    {reply, {error, no_such_object}, State}
    end;

handle_call({start_dynamic_lm, NameCxc, CpuSet, Ns, Args, MboxId}, _From, State) ->
    case get(state) of
        warm_restart -> %% HV69686
            {reply, {error, warm_restart_in_progress}, State};
        _ ->
            Tag = "dynamic",

            case appmAppData:get_lm_start_data(NameCxc, Tag) of
        	{error, NameCxc, loadmodule_not_found} ->
        	    {reply, {error, loadmodule_not_found}, State};
        	{ok, CxpPath, StartCmd, AppInfo, Pgroup} ->
        	    {AppmId,NewState} = get_next_appm_id(State),
        	    Esc = get_escalation(AppInfo),
        	    MaxRest = get_max_restarts(AppInfo),
        	    MaxTmo = get_max_time(AppInfo),

        	    set_def_cpuset(State#state.default_cpu_set,
        			   State#state.default_cpu_set --
        			       appmPgh:decode_cpuset(CpuSet)),
        	    PgmInfo = #pgm_info{appm_id = AppmId,
        				name_cxc = NameCxc,
        				dynamic = true,
        				grp = Pgroup,
        				ns = Ns,
        				args = Args,
        				mboxid = MboxId,
        				cpu_set = CpuSet,
        				escalation = Esc,
        				maxRestarts= MaxRest,
        				maxTimeout = MaxTmo,
        				restart_list = []
        			       },
        	    IsAvli = false,
        	    {Reply, NewState2} = do_start_lm(PgmInfo,
        					     CxpPath,
        					     StartCmd,
        					     AppInfo,
        					     IsAvli,
        					     NewState),
        	    {reply, Reply, NewState2}
            end
    end;

handle_call({stop_dynamic_lm, AppmId}, _From, State) ->
    case get_pgm_info({appm_id, AppmId}, State#state.pgms) of
	#pgm_info{dynamic = true} = PgmInfo ->
	    {Reply, NewState} = do_stop_lm(PgmInfo, State),
	    set_def_cpuset(State, NewState),
	    NewPgms  = remove_pgm_info(AppmId, NewState#state.pgms),
	    {reply, Reply, NewState#state{pgms=NewPgms}};
	_ ->
	    {reply, {error, no_such_object}, State}
    end;

handle_call({signal_to_pgm, AppmId, SigNo}, _From, State) ->
    case get_pgm_info({appm_id, AppmId}, State#state.pgms) of
	#pgm_info{dynamic = true} = PgmInfo ->
	    case PgmInfo#pgm_info.pgh_id of
		undefined ->
		    log(info,"Dynamic program ~p ( ~p)  is not alive.~n",
			[ PgmInfo#pgm_info.name_cxc,AppmId]),
		    {reply, {error, no_such_object}, State};
		_ ->
		    Reply  = send_signal_to_pgm(PgmInfo, SigNo),
		    {reply, Reply, State}
	    end;
	_ ->
	    {reply, {error, no_such_object}, State}
    end;

handle_call({signal_to_internal_pgm, AppmId, SigNo}, _From,
	    #state{ipgms = InIpgms} = State) ->
    case lists:keyfind(AppmId, #ipgm_info.appm_id, InIpgms) of
	#ipgm_info{pgh_id = PghId,
		   name = Name} ->
	    case PghId of
		undefined ->
		    log(info,"Internal program ~p ( ~p)  is not alive.~n",
			[ Name, AppmId]),
		    {reply, {error, no_such_object}, State};
		_ ->
		    Reply  = send_signal_to_pgm(PghId, SigNo),
		    {reply, Reply, State}
	    end;
	_ ->
	    {reply, {error, no_such_object}, State}
    end;

handle_call({get_pids, AppmId}, _From, State) ->
    case get_pgm_info({appm_id, AppmId}, State#state.pgms) of
	#pgm_info{dynamic = true} = PgmInfo ->
	    case PgmInfo#pgm_info.pgh_id of
		undefined ->
		    log(info,"Program ~p ( ~p)  is not alive.~n",
			[ PgmInfo#pgm_info.name_cxc,AppmId]),
		    {reply, {error, no_such_object}, State};
		_ ->
		    Reply  = get_pgmpid(PgmInfo),
		    {reply, Reply, State}
	    end;
	_ ->
	    {reply, {error, no_such_object}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

%% The rest of handle_call is for test only
handle_call(start_lms, _From, State) ->

    case start_all_lms(State) of
	{ok, NewState} ->
	    log(info,"LMs started in State ~p.~n",
		[State]),
	    {reply, ok, NewState}
	    %% there might come more return values from do_start in the future
	    %% so I leave the case clause as is
    end;

handle_call(stop_lms, _From, State) ->
    NewState1 = do_stop_lms(State),
    NewState = remove_dynamic_pgms(NewState1),
    log(info,"LMs stopped in State ~p.~n", [State]),
    {NewPgms,NewGrps} = do_reset_restart_lists(NewState),
    {reply, ok,  NewState#state{pgms=NewPgms,grps=NewGrps,wfr=false}};


handle_call({start_group, Name, _Where}, _From, State) ->
    {Reply, NewState} = restart_pgroup(Name, State),
    {reply, Reply, NewState};

handle_call({stop_group, Name, _Where}, _From, State) ->
    {Reply, NewState} = stop_pgroup(Name, State),
    {reply, Reply, NewState};

handle_call({start_lm, NameCxc, _Where}, _From, State) ->
    {Reply, NewState} = start_one_lm(NameCxc, State),
    {reply, Reply, NewState};

handle_call({stop_lm, NameCxc, _Where}, _From, State) ->
    {Reply, NewState} = stop_one_lm(NameCxc, State),
    {reply, Reply, NewState};

handle_call({signal_lm, NameCxc, SigNo, _Where}, _From, State) ->
    Reply = signal_lm_impl(NameCxc, SigNo, State),
    {reply, Reply, State};

handle_call(get_apps, _From, #state{pgms=[]} = State) ->
    case is_lm_start_disabled() of
	true ->
	    log(info,"No apps started since ~p file exist~n",
		[ ?DISABLE_FILE]);
	false ->
	    ok
    end,
    {reply, [], State};

handle_call(get_apps, _From, State) ->
    {NewState,Apps} = do_get_apps(State),
    {reply, Apps, NewState};

handle_call(get_apps_more, _From, State) ->
    Apps = do_get_apps_more(State),
    {reply, Apps, State};

handle_call(get_groups, _From, State) ->
    Groups = do_get_groups(State),
    {reply, Groups, State};


handle_call(get_info, _From, State) ->
    {reply, handle_get_info(State), State};

handle_call({get_info, {grp,Grp}}, _From, State) ->
    case lists:keyfind(Grp,#grp_info.grp_name, State#state.grps) of
	#grp_info{pgmList = Pgms} = GrpInfo ->
	    Pgs = [NameCxc || {NameCxc,_} <- Pgms],
	    PgInfos = [Pgminfo || Pgminfo <- State#state.pgms,
				  NameCxc <- Pgs,
				  Pgminfo#pgm_info.name_cxc == NameCxc ],
	    GI = handle_get_info(grps,[GrpInfo]),
	    PI = handle_get_info(pgms,PgInfos),
	    {reply, {GI,  PI}, State};
	_ ->
	    {reply, grp_not_found, State}
    end;
handle_call({get_info, Type}, _From, State) ->
    {reply, handle_get_info(Type, State), State};

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call(reset_restart_list, _From, State) ->
    mnesia:dirty_delete({appmNodeTable,{restart_info, node()}}),
    set_revert_state(undefined),
    case sysEnv:architecture() of
	{"arm",_} ->
	    sysRhai:setbootcounter(0);
	_ ->
	    ok
    end,
    {NewPgms,NewGrps} = do_reset_restart_lists(State),
    {reply, ok,  State#state{pgms=NewPgms,grps=NewGrps,wfr=false}};

handle_call(latest_restart, _From, State) ->
    Res = case catch ets:lookup(appm_ets,warm_time) of
	      [] ->
		  case sysEnv:target() of
		      true -> appmRhai:read("restart_type");
		      false -> "Cold in simulated environment"
		  end;
	      [{_,_TS}] ->
		  "WARM"
	  end,
    {reply, Res,  State};

handle_call({restart_pg,PG}, _From, InState) ->

    Grps = [X || X <- InState#state.grps,
		 X#grp_info.grp_name == PG],

    {Res,OutState} =
	case  Grps of
	    [] ->
		R=io_lib:format("PG  ~p unknown ",[PG]),
		{R,InState};
	    [_Grp] ->
		{ok,NewState} = restart_pgroup(PG, InState),
		R=io_lib:format("PG= ~p restarted",[PG]),
		{R,NewState}
	end,

    {reply, Res,  OutState};



handle_call(legacy_nodeDown_ts_erase, _From, State) ->
    Reply =
	case mnesia:dirty_read(appmNodeTable, {heartTimestamp, node()}) of
	    [#appmNodeTable{value = Timestamp}] ->
		mnesia:dirty_delete(appmNodeTable, {heartTimestamp, node()}),
		Timestamp;
	    [] ->
		undefined
	end,
    {reply, Reply, State};

handle_call(remove_rollback_esi, _From, State) ->
    case State#state.revert_esi_generated of
        undefined ->
            {reply, ok, State};
        false ->
            {reply, restart_timer, State};
        true ->
            cleanup_esi(),
            {reply, ok, State#state{revert_esi_generated = undefined}}
    end;

handle_call(Something,_From,State) ->
    log(info,"Received a handle_call(~p) in State= ~p~n", [Something,State]),
    {reply,ok,State}.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
handle_cast(activate, State) ->
    enable_comea_via_appm(),
    sysInitI:log_startup_time(applications_starting),
    put(early_MFAs, mnesia:dirty_read(appmNodeTable, early_MFAs)),
    mnesia:dirty_delete(appmNodeTable, early_MFAs),
    itc_open(),
    State1 = execute_early_MFAs(State),
    NewState =
	case get(state) of
	    shutting_down ->
		State1;
	    _ ->
		State2 = do_activate(State1),
		put(state, active),
		log(info,"activated~n", []),
		update_restart_info(),
                handle_vnfc_info(),
		{ok, Tref} = timer:send_after(?ESC_RST_TOUT, reset_escalation),
		auto_create_cluster(sysEnv:vrcs()),
		sysInitI:log_startup_time(appm_complete),
		State2#state{esc_tref=Tref}
	end,
    {noreply, NewState};

%%--------------------------------------------------------------------
%%
%% EE has detected a program crash
%%
handle_cast({pgmcrash, PghId, Rank}, State) ->
    handle_pgm_stopped({pgmcrash, PghId, Rank}, State);

%%--------------------------------------------------------------------
%% Heartbeat has detected a program hanging
%%
handle_cast({pgmhanging, NameCxc}, State) ->
    case get_pgm_info({name_cxc, NameCxc}, State#state.pgms) of
	#pgm_info{pgh_id = PghId} ->
	    ok;
	_ ->
	    PghId = undefined
    end,
    Rank = unspecified,
    handle_pgm_stopped({pgmhanging, PghId, Rank}, State);

%%--------------------------------------------------------------------
%% EE has detected a program termination
%%
handle_cast({pgmterm, PghId, Reason}, State) ->
    
    handle_pgm_stopped({pgmterm, PghId, {pgm,Reason}}, State);

%%--------------------------------------------------------------------
%% EE has finished warm restart
%%
handle_cast({warmrestart_rsp,Result}, State) ->
    handle_warm_cnf(Result,State);

%%--------------------------------------------------------------------
handle_cast({restart,
	     Scope,
	     RestartType,
	     RestartRank,
	     RestartEscalation,
	     RestartCause},
	    State) ->
    NewState=
	case get(state) of
	    starting ->
		add_early_MFA({?MODULE, restart, [Scope,
						  RestartType,
						  RestartRank,
						  RestartEscalation,
						  RestartCause]}),
		State;
	    _ ->
		do_restart_piu(RestartRank,
			       shutdownCommand,
			       RestartCause,
			       cause_appm(Scope, RestartCause),
			       State)
	end,
    {noreply, NewState};

%%--------------------------------------------------------------------
handle_cast(continue_after_upgrade, State) ->
    %% erarafo 2013-06-26: Do nothing here; C applications
    %% are already started and may have performed object
    %% conversions; it is up to the applications to use the
    %% waitForClasses ICTI operation if they need to hold
    %% until specific IMM classes are converted.
    %% {ok, NewState} = start_all_lms(State),
    {noreply, State};

%%--------------------------------------------------------------------
handle_cast({inhibit_escalation,Time},State) when State#state.wfr == false ->
    TimeMS=60*1000*Time,
    {ok,Tref} = timer:apply_after(TimeMS,?MODULE,inhibit_timeout,[]),
    sysInitI:info_report([{?MODULE, inhibit_escalation},
			   {timeout_minutes,Time } ]),
    NewState = State#state{wfr=true, inhibit_tref=Tref},

    {noreply,NewState};

handle_cast(inhibit_timeout,State) ->
    sysInitI:error_msg("~p: inhibit_timeout. No cancel of inhibit_escalation done.~n"
		       "A cold restart will be performed.~n",[?MODULE]),
    do_cold_restart2(false, "'inhibit timeout' "),
    {noreply,State};

handle_cast(cancel_inhibit,State) ->
    sysInitI:info_report([{?MODULE, cancel_inhibit}]),
    timer:cancel(State#state.inhibit_tref),
    case State#state.wfr_crash of
	true ->
	    sysInitI:info_msg("Crash has happened during inhibit, will restart~n",[]),
	    do_cold_restart(false, "'crash during inhibit'  " , State);
	_ ->
	    ok
    end,

    {noreply,	State#state{wfr=false, inhibit_tref=undefined}};

handle_cast(make_esi_start, State) ->
    {noreply, State#state{revert_esi_generated = false}};

handle_cast(make_esi_done, State) ->
    {noreply, State#state{revert_esi_generated = true}};

%%--------------------------------------------------------------------
handle_cast(Something,State) ->
    log(info,"Received a handle_cast(~p) in State= ~p~n",
	[ Something,State]),
    {noreply,State}.



%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%%ignore helper process normal exits
handle_info({'EXIT',_,normal}, State) ->
    {noreply, State};

%--------------------------------------------------------------------
handle_info(pgm_info, State) ->
    print_pgm_info(State#state.pgms),
    {noreply, State};

handle_info(reset_escalation,  State) when State#state.wfr == false ->
    mnesia:dirty_delete({appmNodeTable,{restart_info, node()}}),
    set_revert_state(undefined),
    {noreply,  State#state{esc_tref=undefined}};
handle_info(reset_escalation,  State) ->
    %% this can happen at restore backup
    {noreply, State};

%% this is to make the appmServer crash for debug purposes
handle_info(crash, _State) ->
    log(info,"Received crash~n",[]),
    ok; % this will cause a gen_server cash

handle_info({'DOWN', _MonitorRef, process, Pid, _Info},
	    #state{ipgms = InPgms} = State) ->
    NewMonitored = lists:delete(Pid, State#state.monitored),
    {PghIds, NewIpgms} =
	lists:foldl(
	    fun(#ipgm_info{pgh_id = PghId, owning_pid = P}, {OutPghs, OutPgms})
			when  P =:= Pid ->
		{[PghId | OutPghs], OutPgms};
	       (Pgm_started_by_other_pid, {OutPghs, OutPgms}) ->
		{OutPghs, [Pgm_started_by_other_pid | OutPgms]}
	    end, {[], []}, InPgms),
    case appmPghServer:req_destroy_pgm([{pgm_id, PghIds}]) of
	{ok, _} ->
	    {noreply, State#state{monitored = NewMonitored,
				  ipgms = NewIpgms}};
	{error, Reason} ->
	    log(error,
		"Killing program(internal) has failed~n"
		"Reason: ~p~n", [Reason]),
	    {noreply, State#state{monitored = NewMonitored}}
    end;

handle_info(Other, State) ->
    io:format("Received a handle_info(~p) in State= ~p~n",[Other,State]),
    {noreply,State}.

%%--------------------------------------------------------------------
terminate(Reason, State) ->
    put(state, shutting_down),
    mnesia:dirty_delete(appmNodeTable, early_MFAs),
    sysInitI:info_report([{?MODULE,terminate}, {reason,Reason}]),
    omc_api:prepare_to_shutdown(), %turns off incoming ssh/tls
    itc_close(),
    case sysEnv:rcs_mode() of
	target ->
	    start_shutdown_wd();
	simulated ->
	    do_stop_lms(State)
    end,
    ok.

start_shutdown_wd() ->
    [{_,EscCnt}] = ets:lookup(appm_ets,esc_cnt),
    os:cmd("echo \"sleep 60 \" > /tmp/wd.sh"),
    os:cmd("echo \"pgh_restart_board "++EscCnt++" -r 'MW shutdown WD restart'\" >> /tmp/wd.sh"),
    os:cmd("chmod +x /tmp/wd.sh"),
    os:cmd("/tmp/wd.sh&"),
    log(info,"started shutdown watchdog~n",[]).


%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================
call(MpIdIn,Msg) ->
    MpId =
	case MpIdIn of
	    0 ->
		1;
	    X ->
		X
	end,
    case lists:member(MpId, clhI:mp_id(all)) of
	true ->
	    case clhI:own_mp_id() of
		MpId ->
		    call(Msg);
		_ ->
		    ErlNode = clhI:erlang_node(MpId),
		    rpc:call(ErlNode, ?MODULE, call, [Msg])
	    end;
	false ->
	    {error,invalid_mp_id}
    end.

call(Msg) ->
    gen_server:call(?MODULE,Msg,60000).
cast(Msg) ->
    gen_server:cast(?MODULE,Msg).


%%--------------------------------------------------------------------
start_watchdog_kicker() ->
    case os:getenv("PGH_WDFN") of
	false ->
	    log(info,
		"Watchdog file not defined yet~n",
		[]),
	    undefined;
	File ->
	    case file:read_file_info(File) of
		{ok, _} ->
		    appmWdKick:start(File),
		    Interval=?DEFAULT_WD_INTERVAL,
		    log(info,
			"Watchdog file ~p will be kicked every  ~p ms.~n",
			[File,Interval]),
		    timer:apply_interval(Interval,
					 appmWdKick,
					 kick,
					 []);

		Error ->
		    log(error,
			"Failed to open Watchdog file ~p, reason~p~n",
			[File, Error]),
		    undefined
	    end
    end.



%%--------------------------------------------------------------------
add_early_MFA(MFA) ->
    Fun =
	fun() ->
		Old_MFAs =
		    case mnesia:read(appmNodeTable, early_MFAs) of
			[] ->
			    [];
			[#appmNodeTable{value = Value}] ->
			    Value
		    end,
		mnesia:write(#appmNodeTable{key = early_MFAs,
					    value = Old_MFAs ++ [MFA]})
	end,
    Result = mnesia:transaction(Fun),
    sysInitI:info_report([{?MODULE, add_early_MFA},
			      {mfa, MFA},
			      {result, Result},
			      '---process_info---:'] ++
				 lists:keydelete(error_handler,1,process_info(self()))).

%%--------------------------------------------------------------------
%%
add_early_MFA_state(MFA) ->
   add_early_MFA({mfa_includeState, MFA}).

%%--------------------------------------------------------------------
execute_early_MFAs(State) ->
    case erase(early_MFAs) of
	[] ->
	    State;
	[#appmNodeTable{value = EarlyMFAs}] ->
	    execute_early_MFAs(EarlyMFAs, State)
    end.

execute_early_MFAs([{mfa_includeState, {M, F, A}} | Tail], State) ->
    execute_early_MFAs(Tail, apply(M, F, A ++ [State]));
execute_early_MFAs([{M, F, A} | Tail], State) ->
    apply(M, F, A),
    execute_early_MFAs(Tail, State);
execute_early_MFAs([], State) ->
    State.

%%--------------------------------------------------------------------
do_activate(State) ->
    OutState =
	case swmI:is_upgrade_ongoing() of
	    true ->
		log(info,
		    "handle_cast(activate during upgrade, ~n",
		    []),
		{ok, NewState} = start_all_lms(State),
		sysInitI:restart_logger_trace(?MODULE, ?LINE,
					      "upgrade; applications started"),
		avli_node_event(operational),
		reset_operational_led(),
		case sysEnv:vrcs() of
		    true ->
			disable_warm_restart(0),
			log(info, "Disabled warm restart for VRCS~n",[]);
		    false ->
			enable_warm_restart(0),
			log(info, "Enabled warm restart for both DUS and TCU~n",[])
		end,
		disable_pg_restart(0, "at upgrade"), %Disabled per default now, will be enabled in the future

		restart_complete(State#state.activated),
		NewState#state{activated = true};
	    false ->
		{ok, NewState} = start_all_lms(State),
		sysInitI:restart_logger_trace(?MODULE,
					      ?LINE,
					      "non-upgrade; applications started"),
		avli_node_event(operational),
		reset_operational_led(),
		restart_complete(State#state.activated),
		{NewPgms,NewGrps} = do_reset_restart_lists(NewState),
		NewState#state{pgms = NewPgms, grps = NewGrps, activated = true}
	end,
    mark_activation_ready(), %Used in Jenkins for sim
    OutState.

restart_complete(false) ->
    catch omc_api:restart_complete(),
    catch ootI:restart_complete();
restart_complete(true) ->
    ok.

mark_activation_ready() ->
    case sysEnv:rcs_mode()  of
	simulated ->
	    os:cmd(["touch ",sysEnv:home_dir(), "/activation_complete"]);
	target ->
	    ok
    end.

%%--------------------------------------------------------------------
is_lm_start_disabled() ->
    case file:read_file_info(?DISABLE_FILE) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

start_all_lms(State) ->
    case is_lm_start_disabled() of
	false ->
	    case sysEnv:vrcs() andalso sysInitApp:is_cyclic_restart() of
		false -> 
		    NewState = do_start_all_lms(State),
		    Apps = do_get_apps_more(NewState),
		    log(info,
			"Started these ~p programs ~p ~n",
			[length(Apps),Apps]),
		    {ok, NewState};
		true ->
		    log(error,"LMs not started due to cyclic restart~n",[]),
		    {ok, State#state{pgms  = []}}
	    end;
	true ->
	    log(info,"LMs not started since ~p exist~n",
		[?DISABLE_FILE]),
	    {ok, State#state{pgms  = []}}
    end.

do_start_all_lms(InState) ->
    State2 = do_stop_lms(InState),

    StartParams = appmAppData:get_start_data(),
    {PgmInfos, State3} = make_pgmInfo(StartParams, State2),
    StateData = {State3#state.default_cpu_set,
		 State3#state.uid,
		 State3#state.gid},

    Resp = parallel(PgmInfos,fun lm_start/2, StateData),

    OutState=
	lists:foldl(
	  fun({_NameCxc, {ok,PgmInfo }}, State) ->
		  NewPgms =  update_pgms(PgmInfo, State#state.pgms),
		  PghId = PgmInfo#pgm_info.pgh_id,
		  CpuSet =   PgmInfo#pgm_info.cpu_set,
		  {NewCpuInfo, NewDefaultCpuSet} =
		      update_default_cpu_set(add, PghId,
					     CpuSet, State#state.cpu_info),
		  NewGrps = update_grps(add,
					PgmInfo#pgm_info.grp,
					State#state.grps,
					PgmInfo#pgm_info.appm_id,
					PgmInfo#pgm_info.name_cxc),
		  NewState = State#state{pgms = NewPgms,
					 grps = NewGrps,
					 cpu_info = NewCpuInfo,
					 default_cpu_set = NewDefaultCpuSet},
		  NewState;
	     ({NameCxc, {error,_, Reason }}, State) ->
		  log(error, "Error starting LM: ~p, Reason ~p~n",
		      [NameCxc, Reason]),
		  State
	  end,
	  State3, Resp),
    OutState.


update_pgms(PgmInfo=#pgm_info{appm_id=AppmId}, Pgms) ->
    lists:keystore(
      AppmId,
      #pgm_info.appm_id,
      Pgms,
      PgmInfo).


mark_pgms_stopped(Pgms, []) ->
    Pgms;
mark_pgms_stopped(Pgms, [PghId|PghIds]) ->
    PgmInfo = get_pgm_info({pgh_id, PghId}, Pgms),
    mark_pgms_stopped(
      lists:keystore(PgmInfo#pgm_info.appm_id,
		     #pgm_info.appm_id,
		     Pgms,
		     PgmInfo#pgm_info{pgh_id=undefined,
				      pid=undefined}),
      PghIds).


make_pgmInfo(StartParams, InState) ->
    lists:mapfoldl(
      fun({NameCxc, {ok, _CxpPath, _StartCmd, AppInfo,Grp} = StartData},
	  State) ->
	      Pgms = State#state.pgms,
	      case get_pgm_info({name_cxc, NameCxc}, Pgms) of
		  false ->
		      {AppmId,State2} = get_next_appm_id(State),
		      Esc = get_escalation(AppInfo),
		      MaxRest = get_max_restarts(AppInfo),
		      MaxTmo = get_max_time(AppInfo),

		      PgmInfo = #pgm_info{appm_id    = AppmId,
					  name_cxc   = NameCxc,
					  dynamic    = false,
					  grp        = Grp,
					  args       = [],
					  cpu_set    = 0,
					  escalation = Esc,
					  maxRestarts= MaxRest,
					  maxTimeout = MaxTmo,
					  restart_list = []},
		      {{PgmInfo,StartData},
		       State2#state{pgms = [PgmInfo|Pgms]}};
		  OldPgmInfo ->
		      {{OldPgmInfo,StartData}, State}
	      end;
	 ({NameCxc, {error,_,_}},State) ->
	      log(error, "No binary found for: ~p ~n", [NameCxc]),
	     {{no_start,NameCxc}, State}
      end,
      InState,
      StartParams).


%%--------------------------------------------------------------------
restart_pgroup(GrpName, AppmId_Failed, Reason, InState) ->
    case lists:keyfind(GrpName, #grp_info.grp_name, InState#state.grps) of
	#grp_info{pgmList = Pgms} = GrpInfo ->
	    avli_pgroup_event(Reason, GrpName),
	    write_llog(pgm_grp,GrpName,Reason,""),
	    [avli_pgm_event(true, Reason, PgmNameCxc) ||
		{AppmId,PgmNameCxc} <- Pgms, AppmId /= AppmId_Failed],
	    do_restart_pgroup(GrpInfo, InState);
	GrpInfo ->
	    do_restart_pgroup(GrpInfo, InState)
    end.

restart_pgroup(GrpName, InState) ->
    avli_pgroup_event("Manual restart", GrpName),
    write_llog(pgm_grp,GrpName,"Manual restart",""),
    do_restart_pgroup(lists:keyfind(GrpName,
				    #grp_info.grp_name,
				    InState#state.grps),
		      InState).

do_restart_pgroup(#grp_info{grp_name = GrpName,
			    restart_list = RestartList},
		  InState) ->
    call_pgroup_cbs(prep_pgroup, GrpName,
		  "informing MW applications to prepare for program group restart~n "),
    {ok,State2} = stop_pgroup(GrpName, InState),
    call_pgroup_cbs(pgroup_done, GrpName,
		  "informing MW applications of program group restart~n "),
     Pgs = [X#pgm_info.name_cxc || X <- State2#state.pgms,
				  X#pgm_info.grp == GrpName,
				  X#pgm_info.dynamic == false],
    StartParams = appmAppData:get_start_data(Pgs),
    {PgmInfos, State3} = make_pgmInfo(StartParams, State2),
    StateData = {State3#state.default_cpu_set,
		 State3#state.uid,
		 State3#state.gid},
    Resp = parallel(PgmInfos,fun lm_start/2, StateData),

    OutState=
	lists:foldl(
	  fun({NameCxc, {ok,PgmInfo }}, State) ->
		  NewPgms = update_pgms(PgmInfo, State#state.pgms),
		  PghId = PgmInfo#pgm_info.pgh_id,
		  CpuSet =   PgmInfo#pgm_info.cpu_set,
		  {NewCpuInfo, NewDefaultCpuSet} =
		      update_default_cpu_set(add,
					     PghId,
					     CpuSet,
					     State#state.cpu_info),
		  avli_pgm_event(true, pgmstart, NameCxc),
		  NewGrps = update_grps(add,
					PgmInfo#pgm_info.grp,
					State#state.grps,
					PgmInfo#pgm_info.appm_id,
					PgmInfo#pgm_info.name_cxc),
		  State#state{pgms = NewPgms,
			      grps = NewGrps,
			      cpu_info = NewCpuInfo,
			      default_cpu_set = NewDefaultCpuSet};
	     ({NameCxc, {error,Reason }}, State) ->
		  log(error, "Error starting LM: ~p, Reason ~p~n",
			   [NameCxc, Reason]),
		  State;
	     ({NameCxc, {error,_, Reason }}, State) ->
		  log(error, "Error starting LM: ~p, Reason ~p~n",
			   [NameCxc, Reason]),
		  State
	  end,
	  State3,
	  Resp),

    NewRestartList=[get_timestamp()|RestartList],
    GrpInfo = lists:keyfind(GrpName,
			    #grp_info.grp_name,
			    OutState#state.grps),
    NewGrpInfo = GrpInfo#grp_info{restart_list=NewRestartList},
    Grps  = lists:keyreplace(GrpName,
			     #grp_info.grp_name,
			     OutState#state.grps,
			     NewGrpInfo),

    {ok, OutState#state{grps = Grps}};
do_restart_pgroup(_, InState) ->
    {ok, InState}.   %no programs in group


pick_wait_pgms(Pgms) ->
    pick_wait_pgms(Pgms, {[], [], 0} ).
pick_wait_pgms([], {Norm, Wait, MaxTime} ) ->
    NewMaxTime = if
		     MaxTime < ?MAX_KILL_DELAY  ->
			 MaxTime;
		     true ->
			 log(error,
			     "Requested kill sleep  ~p~n"
			     "Maximised to ~p~n",
			     [MaxTime, ?MAX_KILL_DELAY]),
  			 ?MAX_KILL_DELAY
		 end,
    {Norm, Wait, NewMaxTime};
pick_wait_pgms([P={_,NameCxc }  | Rest ],{Norm, Wait, MaxTime}) ->
    {ok, _, _, AppInfo,_} = appmAppData:get_lm_start_data(NameCxc),
    if 
      AppInfo#appmPgmData.delayedKill =/= 0 ->
	    NewMaxTime = max(AppInfo#appmPgmData.delayedKill, MaxTime),
	    pick_wait_pgms(Rest,{Norm, Wait++[P], NewMaxTime });
	true ->
	    pick_wait_pgms(Rest,{Norm++[P], Wait, MaxTime})
    end.

stop_pgroup(PgName, InState) ->
    Pgms = get_active_pgroup_lms(PgName,InState),
    {NormPgms, WaitPgms, MaxTime} = pick_wait_pgms(Pgms),
    State2 = do_stop_lms_grp(NormPgms, InState),
    State3  =
	case WaitPgms of
	    [] ->
		State2;
	    WaitPgms ->
		log(info, "Waiting ~p milliseconds before killing ~p~n",
		    [MaxTime, WaitPgms]),
		timer:sleep(MaxTime),
		do_stop_lms_grp(WaitPgms, State2)
	end,
    {ok,State3}.

get_active_pgroup_lms(PgName,InState) ->
    Groups = InState#state.grps,
    GrpInfo = lists:keyfind(PgName, #grp_info.grp_name, Groups),
    GrpInfo#grp_info.pgmList.

get_pgroup_lms(Name, #state{pgms=Pgms}) ->
    [P#pgm_info.name_cxc || P <- Pgms,
			    P#pgm_info.grp == Name].

parallel(Progs,Fun,State) ->
    Pgs = [  catch PgmInfo#pgm_info.name_cxc  || {PgmInfo, _} <- Progs ,
						 PgmInfo /= no_start ],

    log(info, "Starting progs in parallell: ~p ~n", [Pgs]),
    process_flag(trap_exit, true),
    Pids = [ spawn_link(?MODULE, job, [self(), Fun, X, State] ) || X <- Progs,
								   {Y,_} <- [X],
								   Y =/= no_start],
    wait(Pids,[]).

wait([],Acc) ->
    lists:reverse(Acc);
wait(Pids,Acc) ->
    receive
	{ok,Ans,Pid} ->
	    unlink(Pid),  wait(Pids -- [Pid], [Ans|Acc]);
	{'EXIT',_Pid,normal} ->
	    wait(Pids,Acc);
	{'EXIT',Pid,Reason} ->
	    wait(Pids -- [Pid], [{exit,Pid,Reason}|Acc])
    after 120000 ->
	    {error,Pids,Acc}
    end.

job(Parent,Fun,X,State) ->
    Parent ! {ok, catch Fun(X,State), self()}.


% start function for parallel
lm_start({PgmInfo, {ok, CxpPath, StartCmd, AppInfo,_}},StateData) ->
    NameCxc = PgmInfo#pgm_info.name_cxc,
    {NameCxc,
     start_program(PgmInfo, CxpPath, StartCmd, AppInfo, StateData)};

lm_start({PgmInfo, {error, Reason }}, _) ->
    {PgmInfo#pgm_info.name_cxc,  {error, Reason }};
lm_start({PgmInfo, {error,_, Reason }}, _) ->
    {PgmInfo#pgm_info.name_cxc,  {error, Reason }}.


% First start of program after (i.e. not restart of program)
start_one_lm(Name, State) ->
    case appmAppData:get_lm_start_data_from_name(Name) of
	{ok, NameCxc, CxpPath, StartCmd, AppInfo, Pgroup} ->
	    start_one_lm_cont({NameCxc, CxpPath, StartCmd, AppInfo, Pgroup}, State);
	{error, _, Reason} ->
	    {{error, Reason}, State}
    end.


start_one_lm_cont({NameCxc, CxpPath, StartCmd, AppInfo, Pgroup}, State) ->
    {PgmInfo,State4}  =
	case get_pgm_info({name_cxc, NameCxc}, State#state.pgms) of
	    false ->
		{AppmId,State2} = get_next_appm_id(State),
		Esc = get_escalation(AppInfo),
		MaxRest = get_max_restarts(AppInfo),
		MaxTmo = get_max_time(AppInfo),

		PI = #pgm_info{appm_id = AppmId,
			       name_cxc = NameCxc,
			       dynamic = false,
			       args = [],
			       cpu_set = 0,
			       grp = Pgroup,
			       escalation = Esc,
			       maxRestarts= MaxRest,
			       maxTimeout = MaxTmo,
			       restart_list = []
			      },
		{PI, State2};
	    OldPgmInfo ->
		{_, State3} = do_stop_lm(OldPgmInfo, State),
		{OldPgmInfo#pgm_info{restart_list = []}, State3}
	end,

    DefCpuSet = State4#state.default_cpu_set,
    Uid = State4#state.uid,
    Gid = State4#state.gid,

    case start_program(PgmInfo, CxpPath, StartCmd, AppInfo,
		       {DefCpuSet,Uid,Gid}) of
	{ok, NewPgmInfo} ->
	    PghId = NewPgmInfo#pgm_info.pgh_id,
	    NewPgms =  lists:keystore(
			 PgmInfo#pgm_info.appm_id,
			 #pgm_info.appm_id,
			 State4#state.pgms,
			 NewPgmInfo),
	    {NewCpuInfo, NewDefaultCpuSet} =
		update_default_cpu_set(add, PghId, PgmInfo#pgm_info.cpu_set,
				       State4#state.cpu_info),
	    NewGrps = update_grps(add,
				  PgmInfo#pgm_info.grp,
				  State#state.grps,
				  PgmInfo#pgm_info.appm_id,
				  PgmInfo#pgm_info.name_cxc),
	    NewState = State4#state{pgms = NewPgms,
				    grps = NewGrps,
				    cpu_info = NewCpuInfo,
				    default_cpu_set = NewDefaultCpuSet},
	    {ok, NewState};
	{error, Reason} ->
	    log(error,
		"Failed to start ~p due to ~p~n",[NameCxc,Reason]),
	    {{error,Reason}, State4}
    end.

%%% Ordered start of program from lmhi or a restart of a program
%%%

do_start_lm(PgmInfo = #pgm_info{name_cxc = NameCxc,
				appm_id = AppmId,
				cpu_set = CpuSet,
				dynamic = Dynamic
			       },
	    CxpPath,
	    StartCmd,
	    AppInfo,
	    IsAvli,
	    State) ->
    State1 =
	case Dynamic of
	    true ->
		State; %Don't kill if dynamic (could be multiple instances)
	    false ->
		{_,St} = do_stop_lm(PgmInfo, State),
		St
	end,

    DefCpuSet = State1#state.default_cpu_set,
    Uid = State1#state.uid,
    Gid = State1#state.gid,

    case start_program(PgmInfo, CxpPath, StartCmd, AppInfo,
		       {DefCpuSet,Uid,Gid}) of
	{ok, NewPgmInfo} ->
	    PghId = NewPgmInfo#pgm_info.pgh_id,
	    NewPgms =  lists:keystore(
			 PgmInfo#pgm_info.appm_id,
			 #pgm_info.appm_id,
			 State1#state.pgms,
			 NewPgmInfo),
	    {NewCpuInfo, NewDefaultCpuSet} =
		update_default_cpu_set(add, PghId, CpuSet,
				       State1#state.cpu_info),
	    NewGrps = update_grps(add,
				  PgmInfo#pgm_info.grp,
				  State#state.grps,
				  PgmInfo#pgm_info.appm_id,
				  PgmInfo#pgm_info.name_cxc),
	    NewState = State1#state{pgms = NewPgms,
				    grps = NewGrps,
				    cpu_info = NewCpuInfo,
				    default_cpu_set = NewDefaultCpuSet},
	    avli_pgm_event(IsAvli, pgmstart, NameCxc),
	    {{ok,AppmId}, NewState};
	{error, Reason} ->
	    log(error,
		"Failed to start ~p due to ~p~n",[NameCxc,Reason]),
	    {{error,Reason}, State1}
    end.

%%% Special case "myProg -env[X=Y,A=B] arg1 arg2"  will be translated to
%%%  X=Y A=B myProg arg1 arg2 since not all programs (read OSE) can take arguments
parse_args("-env["++Rest) ->
    case  string:tokens(Rest,"]") of
	[EnvStr,Args] ->
	    {parse_envs(EnvStr),Args};
	[EnvStr] ->
	    {parse_envs(EnvStr),[]};
	_ ->
	    {[],"-env["++Rest}
    end;
parse_args(Args) ->
    {[],Args}.

parse_envs(EnvStr) ->
    string:tokens(EnvStr,",").

start_program(PgmInfo=#pgm_info{name_cxc=NameCxc,
				args=TmpArgs,
				cpu_set=CpuSet,
				ns=Ns,
				restart_list=RestartList},
	      CxpPath, StartCmd, AppInfo,
	      {DefCpuSet,Uid,Gid}) ->
    {UsrEnvs,Args} =
	case TmpArgs of
	    [] ->
		{[],[]};
	    [CmdArgs|Rest] ->
		case parse_args(binary_to_list(CmdArgs)) of
		    {UEnv = [], UArg} -> %% HV93805
			{UEnv,[list_to_binary(UArg)|Rest]};
		    {UEnv,[]} ->
			{UEnv,Rest};
		    {UEnv,UArg} ->
			{UEnv,[list_to_binary(UArg)|Rest]}
		end
	end,

    try  get_actual_prog_and_args(CxpPath, StartCmd, Args) of
	 {StartPgm,StartArgs} ->
	    AppEnv = get_app_env(AppInfo),
	    {Name,Cxc} = NameCxc,
	    {ok,Rev} = appmAppData:get_lm_rev(NameCxc),
	    {ok,CxpNo,CxpRev} = appmAppData:get_cxp_no_rev(NameCxc),
	    L = [{"CXC_NAME",Name},
		 {"CXC_NO",Cxc},
		 {"CXC_REV",Rev},
		 {"CXP_NO",CxpNo},
		 {"CXP_REV",CxpRev}],
	    Envs = UsrEnvs ++
		[A++"="++B || {A, B} <- AppEnv ++
				  get_total_env(CxpPath) ++
				  L],

	    PgmCap = get_pgm_cap(AppInfo),
	    PgmType = get_pgm_type(AppInfo),
	    MemLimit = get_mem_limit(AppInfo),

	    case do_start_pgm(StartPgm, StartArgs, CpuSet, Envs,
			      PgmCap, PgmType, MemLimit,
			      DefCpuSet,Uid,Gid,Ns) of
		{ok,  PghId} ->
		    Pid = undefined,
		    %%		    Pid = get_pid(StartPgm),
		    NewPgmInfo =
			PgmInfo#pgm_info{pgh_id     = PghId,
					 location   = StartPgm,
					 pid        = Pid,
					 restart_list = [get_timestamp()
							 |RestartList]},
		    appmHbServer:program_started(NameCxc),
		    log(info,
			"Starting program  ~p has succeeded~n"
			"UsrEnvs: ~p~n"
			"Args: ~p~n"
			"Appm ID: ~p~n"
			"Pgh ID: ~p~n",
			[StartPgm,
			 UsrEnvs,
			 StartArgs,
			 PgmInfo#pgm_info.appm_id,
			 PghId]),
		    {ok, NewPgmInfo};
		{error, Reason} ->
		    {error, Reason}
	    end
    catch
	TX:TY ->
	    {error, {TX,TY}}
    end.

get_actual_prog_and_args(CxpPath, StartCmd, Args) ->
    Location =
	case filelib:wildcard(filename:join(CxpPath, StartCmd)) of
	    [Loc] ->
		Loc;
	    _Other ->
		log(error,
		    "Can't find file ~n~p~n to start, check your CXP!~n",
		    [filename:join(CxpPath, StartCmd)]),
		throw({start_error,program_does_not_exist})
	end,

    ActualLocation = swmI:find_file(Location), %use patched version if exist

    %% If a file named <prog>.appm is found in dev_patches
    %% and it contains a prefix tuple the program pointed out
    %% will be used as a wrapper program (e.g. to start valgrind)
    Base = filename:basename(ActualLocation),
    Cfg = filename:join(sysEnv:dev_patches_dir(), Base ++ ".appm"),
    Prog =
	case file:consult(Cfg) of
	    {ok,Opts}->
		log(info,"Found ~p file for ~p with ~p~n",
		    [Cfg, Base, Opts]),
		proplists:get_value(prefix,Opts,"");
	    _ ->
		""
	end,
    case Prog of
	"" -> {ActualLocation,Args};
	[$/|_] -> {Prog, [ActualLocation] ++ Args};
	_ -> {filename:join(sysEnv:dev_patches_dir(), Prog), [ActualLocation] ++ Args}
     end.


stop_one_lm(NameCxc, State) ->
    PgmInfo = get_pgm_info({name_cxc, NameCxc}, State#state.pgms),
    do_stop_lm(PgmInfo, State).

signal_lm_impl(NameCxc, SigNo, State) ->
    case get_pgm_info({name_cxc, NameCxc}, State#state.pgms) of
	false ->
	    {error,app_not_found};
	PgmInfo ->
	    send_signal_to_pgm(PgmInfo, SigNo)
    end.

get_total_env(CxpPath) ->
    [{"PATH",os:getenv("PATH")}] ++ get_rcs_env(CxpPath).

get_total_env() ->
    [{"PATH",os:getenv("PATH")}] ++ get_rcs_env("").

get_mw_cxp_info() ->
    case catch ets:lookup(appm_ets,mw_cxp_info)  of	    
	[{_,CxpProdNr, CxpRev}] ->
	    {CxpProdNr, CxpRev} ;
	_ ->
	    {_, CxpProdNr, CxpRev} = swmI:get_cxp_source(code:lib_dir(appm)),
	    {CxpProdNr, CxpRev} 		
    end.

app_tmp_root() ->
    filename:join([sysEnv:rcs_root(),
		   "tmp",
		   "applicationtmp"]).
get_bash_envs(Envs) ->
    [A++"="++B || {A, B} <- get_rcs_env("") ++ Envs].

get_rcs_env(CxpPath) ->
    LogDir = filename:join([sysEnv:rcs_dir(),
			    "applicationlogs",
			    log_dir(CxpPath)]),
    PersDir = filename:join([sysEnv:rcs_dir(),
			     "persistent"]),
    TmpDir = filename:join([app_tmp_root(),
			    log_dir(CxpPath)]),
    {UpNo,UpRev} = get_up_no_rev(),
    {BNo,BRev,MName,SerNo} = get_prod_no_rev(),
    StructIsEncodedAs = "ATTRIBUTE",
    ld_lib_path(sysEnv:rcs_mode_3()) ++
	[{"CXP_PATH", CxpPath},
	 {"RESTART_TYPE",get_restart_type() },
	 {"LOG_DIR", LogDir},
	 {"PERSISTENT_DIR", PersDir},
	 {"RCS_PATCH_DIR", sysEnv:dev_patches_dir()},
	 {"APP_TMP", TmpDir},
	 {"RCS_ROOT", os:getenv("RCS_ROOT")},
	 {"RCS_MODE", os:getenv("RCS_MODE")},
	 {"LTTNG_HOME", os:getenv("LTTNG_HOME")},
	 {"SNAME", os:getenv("SNAME")},
	 {"BT", os:getenv("BT") },
	 {"UP_NO",UpNo},
	 {"UP_REV",UpRev},
	 {"HW_PROD_NO", BNo},
	 {"HW_PROD_REV", BRev},
	 {"HW_MARKET_NAME", MName},
	 {"HW_SERIAL_NO", SerNo},
	 {"SAFC_IMM_OI_PORT",integer_to_list(sysEnv:get_port_conf(imm_oi))},
	 {"SAFC_IMM_OM_PORT",integer_to_list(sysEnv:get_port_conf(imm_om))},
	 {"SAFC_IMM_CT_PORT",integer_to_list(sysEnv:get_port_conf(imm_ct))},
	 {"SAFC_NTF_PORT",integer_to_list(sysEnv:get_port_conf(ntf))},
	 {"SAFC_LOG_PORT",integer_to_list(sysEnv:get_port_conf(saf_log))},
	 {"LOG_PORT",integer_to_list(sysEnv:get_port_conf(log))},
	 {"CEC_PORT",integer_to_list(sysEnv:get_port_conf(cecPort))},
	 {"STRUCT_IS_ENCODED_AS", StructIsEncodedAs}
	]
	++ get_xcm_data()
	++ get_compute_name_and_uuid()
	++ add_if_set("BT_VARIANT")
	++ add_if_set("DUMP_DIR")
	++ add_if_set("MPID")
	++ add_if_set("ERL_EPMD_PORT")
	++ add_if_set("SIM_TGT")
	++ add_if_set("NVPIPARAMS")
	++ add_if_set("ITC_INSTANCE_NAME")
	++ add_if_set("ITC_RUNDIR_PATH")
	++ add_if_set("userspace_linx_portnumber").



add_if_set(Var) ->
    case os:getenv(Var) of
	false ->
	    [];
	Val ->
	    [{Var,Val}]
    end.


ld_lib_path(Type) when Type == vrcs32 orelse Type == hostsim  ->
    [{"LD_LIBRARY_PATH",
      ?DEV_PATCHES ++ ":" ++
	  case os:getenv("EXT_LD_LIBRARY_PATH") of
	      false ->
		  case os:getenv("LD_LIBRARY_PATH") of
		      false ->
			  "";
		      LLP ->
			  LLP ++ ":"
		  end;
	      LdLib ->
		  LdLib ++ ":"
	  end
      ++ sysEnv:library_dir()}];

ld_lib_path(_) -> %target or qemusim
    case sysInitServer:is_secure() of
	false ->
	    [{"LD_LIBRARY_PATH",?DEV_PATCHES}];
	true ->
	    []
    end.


get_xcm_data() ->
    case comsaI:has_consul() of
	true ->
	    [{"XCM_TLS_CERT",certI:get_cert_dist_dir()}];
	false ->
	    []
    end.

get_compute_name_and_uuid() ->
    ComputeName =
	case sysEnv:get_computeName() of
	    [] ->
		[];
	    C ->
		[{"COMPUTE_NAME",C}]
	end,
    Uuid =
	case sysEnv:get_systemUUID() of
	    [] ->
		[];
	    U ->
		[{"SYSTEM_UUID",U}]
	end,

    ComputeName ++ Uuid.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

move_core_file(simulated, Name) ->
    CoreFileName = filename:join([sysEnv:rcs_root(),"core"]),
    Prefix = filename:join([sysEnv:rcs_dir(),"dumps","core"]),
    case file:read_file_info(CoreFileName) of
	{ok,_} ->
	    NewCoreFileName =
		Prefix ++ "." ++
		Name ++ "." ++
		comsaLib:iso_time(os:timestamp(),basic),
	    log(info,"Found core file, moving it to ~p~n",[NewCoreFileName]),
	    file:rename(CoreFileName,NewCoreFileName);
	_ ->
	    ok
    end;
move_core_file(_,_) -> ok.

get_up_no_rev() ->
    UP = swmI:get_current_up_metadata(),
    UpNo = proplists:get_value(productNumber,UP,""),
    UpRev = proplists:get_value(productRevision,UP,""),
    {UpNo,UpRev}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_pgm_stopped({PgmEvent, PghId, _EriRank} = Event, InState) ->
    case get(state) of
	shutting_down ->
	    {noreply, InState};
	_ ->
	    case get_pgm_info({pgh_id, PghId}, InState#state.pgms) of
		false ->
		    handle_ipgm_stopped(PgmEvent, PghId, InState);
		PgmInfo ->
		    handle_pgm_stopped(Event, PgmInfo, InState)
	    end
    end.

handle_pgm_stopped({PgmEvent, PghId, EriRank}, PgmInfo, InState) ->
    {Name, LmId} = NameCxc = PgmInfo#pgm_info.name_cxc,
    case PgmEvent of
	pgmcrash ->
	    IsAvli = true,
	    log(error,
		"Program ID ~p has crashed ####################~n"
		"Name : ~p~n"
		"LmId : ~s~n"
		"Rank : ~s~n", [PghId, Name, LmId, EriRank]);
	pgmhanging ->
	    IsAvli = true,
	    log(error,
		"Program ID ~p is hanging ####################~n"
		"Name : ~p~n"
		"LmId : ~s~n"
		"Rank : ~s~n"
		"This has NOT generated a PMD~n"
		"Will be treated as a program crash.~n",
		[PghId, Name, LmId, EriRank]);
	pgmterm ->
	    PrettyReason = pretty_reason(EriRank),
	    case PgmInfo#pgm_info.escalation of
		"None" ->
		    IsAvli = false,
		    log(info,
			"Program ID ~p has terminated ####################~n"
			"Name : ~p~n"
			"LmId : ~s~n"
			"Reason : ~s~n"
			"Escalation=None so nothing more is done. ~n",
			[PghId, Name, LmId, PrettyReason ]);
		_ when EriRank == {pgm,?PGMTERM_EXIT0} ->
		    IsAvli = false,
		    log(info,
			"Program ID ~p has terminated ####################~n"
			"Name : ~p~n"
			"LmId : ~s~n"
			"Reason : ~s~n"
			"exit=0 so nothing more is done. ~n",
			[PghId, Name, LmId, PrettyReason]);
		_ ->
		    IsAvli = true,
		    log(error,
			"Program ID ~p has terminated ####################~n"
			"Name : ~p~n"
			"LmId : ~s~n"
			"Reason : ~s~n"
			"This has NOT generated a PMD~n"
			"Will be treated as a program crash.~n",
			[PghId, Name, LmId, PrettyReason])
	    end
    end,
    Destroy = 
	case EriRank of
	    {pgm,?PGMTERM_EXIT0} when PgmInfo#pgm_info.escalation /= "None" -> keep;
	    _  -> destroy
	end,
    {_,State1} = do_stop_lm(PgmInfo,Destroy, InState), % to also kill all spawned pgms

    %% Inform application if mbox defined
    inform_application(PgmInfo, PgmEvent,EriRank),
    
    avli_pgm_event(IsAvli, PgmEvent, NameCxc),
    %% For simulated, Move core file to rcs/dumps/<uniquename>
    move_core_file(sysEnv:rcs_mode(), Name),

    {RestartLevelT, NewState1} =
	check_for_escalation(PgmInfo, EriRank, State1),
    RestartLevel = case {RestartLevelT, is_pg_restart_enabled()} of
		       {pgm_grp, false} -> % if PG restart disabled change to warm
			   log(info, " Program group restart disabled, doing a warm ~n",[]),
			   warm;
		       _ ->
			   RestartLevelT
		   end,
    EscRestartLevel = get_esc_rest_type(RestartLevel,update),

    NewState = case swmI:is_upgrade_ongoing() of
	false -> NewState1;
	true ->
	    case RestartLevel of
		no_restart ->
		   NewState1;
		_ ->
		    %% Program crash during upgrade will
		    %%directly lead to a reboot.
		    log(info, "Upgrade ongoing, will directly "
			"escalate to cold ~n",[]),
		    Rank = cold,
		    EscCause = escalation_cause(Rank),
		    do_restart_piu(Rank,
				   PgmEvent,   % Reason
				   EscCause,
				   alh_to_appm(EscCause),
				  NewState1)
	    end
    end,

    case EscRestartLevel of
	wfr ->
	    log(
	      info,
	      "No restart of program made since in revert state ~n",
	      []),
	    set_def_cpuset(NewState, State1),
	    {noreply, NewState#state{wfr_crash=true}};
	no_restart ->
	    log(
	      info,
	      "No restart of program made  ~n",
	      []),
	    set_def_cpuset(NewState, State1),
	    AppmId = PgmInfo#pgm_info.appm_id,
	    Pgms = NewState#state.pgms,
	    NewPgms = case {PgmInfo#pgm_info.dynamic , Destroy} of
			    {true,destroy} -> remove_pgm_info(AppmId, Pgms);
			    _ -> Pgms
			end,
	    {noreply, NewState#state{pgms = NewPgms}};

	pgm ->
	    {ok, CxpPath, StartCmd, AppInfo,_} =
		appmAppData:get_lm_start_data(NameCxc),

	    {_,PgmState} = do_start_lm(PgmInfo,
				       CxpPath,
				       StartCmd,
				       AppInfo,
				       IsAvli,
				       NewState),
	    {noreply, PgmState};

	pgm_grp ->
	    log(info,
		"Program escalate to program group: ~s~n",
		[PgmInfo#pgm_info.grp]),
	    {ok, GrpState} = restart_pgroup(PgmInfo#pgm_info.grp,
					    PgmInfo#pgm_info.appm_id,
					    "Program crash",
					    NewState),
	    {noreply, GrpState};

	X when X==warm; X==cold; X==cold_with_test ->
            set_vnfc_info(?RESTART_APP),
	    NewerState = escalate_if_enabled(EscRestartLevel, PgmEvent,NewState),
	    {noreply, NewerState};

	revert ->
	    %% here we come if we get a crash after a cold with test.
	    %% Now we should check if we have lost OSS connectivity
	    %% and if so initiate a rollback chain
	    %% else and wait for the operator to
	    %% do something smart.
	    NewerState =
		case is_oss_alive() of
		true ->
		    send_alarm(?GP_ALARM),
		    log(info,
			"~n###########################################################~n"
			"Cold with test did not help and OSS is alive so~n"
			"we wait for operator intervention~n"
			"###########################################################~n",
			[]),
			NewState;
		_ ->
		    EscState = escalate_if_enabled(EscRestartLevel, PgmEvent, NewState),
		    log(info,
			"~n###########################################################~n"
			"Cold with test did not help and OSS is not up yet so~n"
			"we wait for OSS to come up~n"
			"###########################################################~n",
			[]),
			EscState
	    end,
	    {noreply, NewerState#state{wfr=true}};
	fail_revert ->
	    %% here we come if we get a crash but the reversion failed.
	    %% .
	    log(info,
		"~n###########################################################~n"
		"Revert has failed~n"
		"we stop all applications and wait for operator intervention~n"
		"###########################################################~n",
		[]),
	    {noreply, NewState#state{wfr=true}}
    end.
	    
    


handle_ipgm_stopped(PgmEvent, PghId, #state{ipgms = InPgms} = InState) ->
 %%   io:format("~n##############################~nInPgms=  ~p~n",[InPgms]),
    case lists:keyfind(PghId, #ipgm_info.pgh_id, InPgms) of
	#ipgm_info{appm_id = AppmId, mfa = MFA, autoclean = AutoClean} ->
	    case MFA of
		{?MODULE, comea_snmp_cb_keep, _} ->
		    comea_snmp_cb_keep(PghId);
		{?MODULE, comea_snmp_cb_stop, _} ->
		    comea_snmp_cb_stop(PghId);
		{M, F, A} ->
		    clean(AutoClean,PghId),
		    catch M:F([AppmId | A]);
		undefined ->
		    clean(AutoClean,PghId)
	    end,
	    NewIpgms = lists:keydelete(AppmId, #ipgm_info.appm_id, InPgms),
	    {noreply, InState#state{ipgms = NewIpgms}};
	false ->
	    log(info,
		"Got ~p for unknown PghId ~p~n",
		[PgmEvent, PghId]),
	    {noreply, InState}
    end.

clean(true,PghId) ->
    appmPghServer:req_destroy_pgm([{pgm_id, [PghId]}]);
clean(false,_) ->
    ok.

%%%
%%% Check how to escalate the program crash
check_for_escalation(_PgmInfo,
		     _EriRank,
		     State=#state{wfr=true}) ->
    {wfr, State}; % In WaitForRevert state

check_for_escalation(_PgmInfo,
		     EriRank,
		     State)
  when EriRank == {pgm,?PGMTERM_EXIT0} ->
    {no_restart, State};

check_for_escalation(PgmInfo=#pgm_info{ grp=PG},
		     EriRank,
		     State=#state{grps=Grps})
  when EriRank == unspecified;
       EriRank == pgm;
       is_tuple(EriRank) ->

    GrpInfo = lists:keyfind(PG, #grp_info.grp_name, Grps),
    case will_restart(GrpInfo) of
	false ->
	    %%escalate above Program Group
	    {get_max_rank(GrpInfo#grp_info.escalation,EriRank), State};
	true ->
	    case will_restart(PgmInfo) of
		true ->
		    {pgm, State};
		false ->
		    %%escalate
		    do_escalate(PgmInfo, EriRank, State)
	    end
    end;

check_for_escalation(#pgm_info{escalation=XmlRank}, EriRank, State) ->
    {get_max_rank(XmlRank,EriRank), State}.



do_escalate(#pgm_info{escalation="PgmGrp",
		      grp=PG },
	    EriRank,
	    State=#state{grps=Grps}) when PG =/= no_group ->
    GrpInfo = lists:keyfind(PG, #grp_info.grp_name, Grps),
    case will_restart(GrpInfo) of
	true ->
	    {pgm_grp, State};
	false ->
	    %%escalate above Program Group
	    {get_max_rank(GrpInfo#grp_info.escalation,EriRank), State}
    end;
do_escalate(#pgm_info{escalation=XmlRank},EriRank,State) ->
    {get_max_rank(XmlRank,EriRank), State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
escalate_if_enabled(Rank, PgmEvent,State) ->
    timer:cancel(State#state.esc_tref),
    S =
	case sysInitApp:is_escalation_enabled() of
	    true ->
		EscCause = escalation_cause(Rank),
		do_restart_piu(Rank,
			       PgmEvent,
			       EscCause,
			       alh_to_appm(EscCause),
			       State);

	    false ->
		log(
		  info,
		  "Program escalation turned off, WILL NOT escalate to ~p ~n",
		  [Rank]),
		State
	end,
    S#state{esc_tref=undefined}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_reset_restart_lists(#state{pgms=Pgms,grps=Grps}) ->
    sysInitApp:reset_cold_cnt(),
    TimeStamp = get_timestamp(),
    NewPgms = [P#pgm_info{restart_list=[TimeStamp]} || P <- Pgms],
    NewGrps = [G#grp_info{restart_list=[]} || G <- Grps],
    {NewPgms,NewGrps}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
escalation_cause(revert) ->
    case get_revert_state() of
	undefined ->
	    ?ALH_TAG_ProgramErrEscalatedRestore1;
	latest ->
	    ?ALH_TAG_ProgramErrEscalatedRestore2;
	after_ug ->
	    ?ALH_TAG_ProgramErrEscalatedRestore3;
	before_ug ->
	    ?ALH_TAG_ProgramErrEscalatedInstallation
    end;
escalation_cause(_) ->
    ?ALH_TAG_ProgramErrEscalated.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%            xml-reg       ERI request

get_max_rank("ColdWithTest",_              ) -> cold_with_test;
get_max_rank(_             ,cold_with_test ) -> cold_with_test;
get_max_rank("BoardCold"   ,_              ) -> cold;
get_max_rank(_             ,cold           ) -> cold;
get_max_rank("Board"       ,_              ) -> warm; %backward compatiple
get_max_rank("BoardWarm"   ,_              ) -> warm;
get_max_rank(_             ,warm           ) -> warm;
get_max_rank("PgmGrp"      ,_              ) -> pgm_grp;
get_max_rank(_             ,pgm_grp        ) -> pgm_grp;
get_max_rank("None"        ,_              ) -> no_restart;
get_max_rank(_             ,_              ) -> warm. % default escalation


%%%  Find out what restart type it is.
%%%  There is a priority order here:
%%%   UPGRADE
%%%   PGM / PGM_GRP
%%%   WARM
%%%   COLD / COLD_W_TEST
%%%
get_restart_type() ->
    case swmI:is_upgrade_ongoing() of
	true ->
	    "UPGRADE";
	false ->
	    case sysInitApp:is_cold_restart() of
	    	true -> "COLD";
	    	false -> "WARM"  %Add PGM ad PGM_GRM later
	    end
    end.


%%%  Inform application of program termination if mboxid is defined
inform_application(PgmInfo, _PgmEvent,_EriRank) when PgmInfo#pgm_info.mboxid == 0 ->
    ok;
inform_application(#pgm_info{mboxid = MboxId, appm_id = AppmId}, pgmcrash ,_EriRank) ->
				  
    Data  = << AppmId:4/native-unsigned-integer-unit:8
	    >>,
    ItcPort = get_itc_port(),
    itc:send(ItcPort, MboxId, ?LMHI_PGM_CRASH_IND, Data);

inform_application(#pgm_info{mboxid = MboxId, appm_id = AppmId}, PgmEvent,EriRank) ->
    Reason = case  {PgmEvent, EriRank} of
		 { pgmterm, {pgm,R }} -> R band ?LOW32BITS;
		 _-> 0
	     end,
				  
    Data  = << AppmId:4/native-unsigned-integer-unit:8,
	       Reason:4/native-unsigned-integer-unit:8
	    >>,
    ItcPort = get_itc_port(),
    itc:send(ItcPort, MboxId, ?LMHI_PGM_TERM_IND, Data).

log_dir("") ->
    "";
log_dir(CxpPath) ->
    A=filename:basename(CxpPath),
    %% remove _Rxxxx from end of filename
    string:join(
      lists:reverse(
	tl(
	  lists:reverse(
	    string:tokens(A,"_")
	   )
	 )
       )
      ,"_").



print_pgm_info(Pgms) ->
    lists:foreach(
      fun(Pgm) ->
	      log(info, "PGM ~p~n", [Pgm])
      end, Pgms).

log(Level, Str,Args) ->
    appmLib:log(Level, ?MODULE_STRING ++ ":" ++ Str,Args).

disk_log(Str,Args) ->
     sysInitLogDisk:write_event("~s  "++?MODULE_STRING ++ ":" ++ Str,
				[comsaI:iso_time(os:timestamp(),extended)]++Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pretty_reason({pgm,Val}) ->
    pretty_reason(Val);
pretty_reason(no_reason) ->
    "None";
pretty_reason(Val) when is_integer(Val) ->
    Type  = Val band ?HI32BITS,
    Reason = Val band ?LOW32BITS,
    case Type of
	?PGMTERM_SIGNAL->
	    "Signal ";
	?PGMTERM_EXIT ->
	    "Exit "
    end ++ io_lib:format("~p",[Reason]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_start_pgm(Location, Args, CpuSet, Envs, PgmCap, PgmType, MemLimit,
	     DefCpuSet, Uid,Gid,Ns) ->
    try

	check_cpuset(CpuSet, DefCpuSet),
	PghId = spawn_pgm( [Location|Args],Envs,CpuSet,Ns,PgmCap,PgmType,MemLimit,Uid,Gid),
	{ok, PghId}
    catch
	throw:{check_error, Reason} ->
	    {error, Reason};
	throw:{spawn_error, Reason} ->
	    {error, Reason}
    end.

spawn_pgm( Args, Envs, CpuSet, Ns, PgmCap, PgmType, MemLimit,Uid,Gid ) ->
    case appmPghServer:req_spawn_pgm([{args, Args},
				      {envs, Envs},
				      {ns, Ns},
				      {uid, Uid},
				      {gid, Gid},
				      {capabilities, PgmCap},
				      {pgmtype, PgmType},
				      {cpu_set, CpuSet},
				      {memory_limit, MemLimit}]) of
	{error, Reason} ->
	    log(error,
		"Starting program ~p has failed~n"
		%% "Args: ~p~n"
		%% "Envs: ~p~n"
		%% "CPU set: ~p~n"
		%% "Capabilities: ~p~n"
		%% "Memory limit: ~p~n"
		"Reason: ~p~n",
		[hd(Args),
		 %% Args, Envs, CpuSet, PgmCap, MemLimit,
		 Reason]),
	    throw({spawn_error, Reason});
	{ok, SpawnList} ->
	    {pgm_id, PghId} = lists:keyfind(pgm_id, 1, SpawnList),
	    PghId
    end.

%%% All LMs are stopped
%%% Used for test and e.g. cleaning up when starting all lms.
do_stop_lms(State) ->
    NewState = stop_lms_impl(State#state.pgms, State),
    remove_dynamic_pgms(NewState).


%%% Only LMs in a program group are stopped
do_stop_lms_grp(Pgms, State) ->
    Pgs = get_pgmInfos(Pgms, State#state.pgms),
    NewState = stop_lms_impl(Pgs, State),
    remove_dynamic_pgms(Pgs,NewState).


get_pgmpid(#pgm_info{pgh_id=PghId}) ->

    case appmPghServer:req_get_pids([{pgm_id, PghId}]) of
	{error, Reason} ->
	    log(error,
		"Get pgm pids has failed~n"
		"Reason: ~p~n", [ Reason]),
	    {error, no_such_object};
	{ok, Resp} ->
	    {ok, Resp}
    end.

send_signal_to_pgm(#pgm_info{pgh_id=PghId}, SigNo) ->
    send_signal_to_pgm(PghId, SigNo);
send_signal_to_pgm(PghId, SigNo) when is_integer(PghId), is_integer(SigNo) ->

    case appmPghServer:req_signal_to_pgm([{pgm_id, PghId},
					  {sig_no, SigNo}]) of
	{error, Reason} ->
	    log(error,
		"Signal to program has failed~n"
		"Reason: ~p~n", [ Reason]),
	    {error, no_such_object};
	{ok, _} ->
	    log(info,
		"Signal to program succeeded~n",
		[]),
	    ok
    end.

stop_lms_impl(Pgms,State) ->
    case [{P,
	   {P#pgm_info.pgh_id, P#pgm_info.name_cxc,P#pgm_info.pid},
	   P#pgm_info.pgh_id} ||
	     P <- Pgms,
	     P#pgm_info.pgh_id =/= undefined ] of
	[] -> State;
	PgTuples ->
	    {PgmInfos,PgmsToKill, PghIds} = lists:unzip3(PgTuples),
	    appmHbServer:stop_hb_clients(PgmsToKill),
	    log(info,
		"Requesting PGH to kill these programs ~n~p~n",
		[lists:sort(PgmsToKill)]),

	    case appmPghServer:req_destroy_pgm([{pgm_id, PghIds}]) of
		{error, Reason} ->
		    log(error,
			"Killing programs  has failed~n"
			"Reason: ~p~n", [ Reason]),
		    throw({destroy_error, Reason});
		{ok, _} ->
		    log(info,
			"Killing programs succeeded~n",
			[]),
		    ok
	    end,
	    NewPgms = mark_pgms_stopped(State#state.pgms,PghIds),
	    Fun = fun(P, G) -> 
			  update_grps(delete,
				  P#pgm_info.grp,
				  G,
				  P#pgm_info.appm_id,
				  P#pgm_info.name_cxc)
		  end,
	    NewGrps = lists:foldl(Fun, State#state.grps, PgmInfos),
	    
	    State#state{pgms = NewPgms,
			grps = NewGrps}
    end.

get_pgmInfos(Pgs, AllPgmInfo) ->
    [PgmInfo || PgmInfo <- AllPgmInfo,
		{AppmId,_} <- Pgs,

		PgmInfo#pgm_info.appm_id == AppmId].

do_stop_lm(PgmInfo,State) ->
    do_stop_lm(PgmInfo, destroy, State). %legacy behaviour

do_stop_lm(false, _,State) ->
    {{error, no_such_object}, State};
do_stop_lm(PgmInfo, _, State) when PgmInfo#pgm_info.pgh_id == undefined ->
    {ok,State};
do_stop_lm(PgmInfo, Destroy, State) ->
    PghId = PgmInfo#pgm_info.pgh_id,
    {NewCpuInfo, NewDefaultCpuSet} =
	update_default_cpu_set(remove, PghId, PgmInfo#pgm_info.cpu_set,
			       State#state.cpu_info),

    try
	NewPgmInfo = case Destroy of
			 destroy -> 
			     ok = destroy_pgm(PghId, PgmInfo#pgm_info.name_cxc),
			     PgmInfo#pgm_info{pgh_id=undefined,
					      pid=undefined};
			 keep -> 
			     PgmInfo#pgm_info{pid=undefined}
		     end,
	NewPgms = update_pgms(NewPgmInfo, State#state.pgms),
	NewGrps = update_grps(delete,
			      PgmInfo#pgm_info.grp,
			      State#state.grps,
			      PgmInfo#pgm_info.appm_id,
			      PgmInfo#pgm_info.name_cxc),
	


	{ok, State#state{pgms = NewPgms,
			 grps = NewGrps,
			 cpu_info = NewCpuInfo,
			 default_cpu_set = NewDefaultCpuSet}}
    catch
	throw:{destroy_error, Reason} ->
	    {{error, Reason}, State}
    end.

destroy_pgm(undefined, {_Name, _LmId} ) ->
    ok;
destroy_pgm(PghId, {Name, LmId} ) ->
    appmHbServer:stop_hb_client({Name, LmId}),
    case appmPghServer:req_destroy_pgm([{pgm_id, [PghId]}]) of
	{error, Reason} ->
	    log(error,
		"Killing program ID ~p has failed~n"
		"Reason: ~p~n", [PghId, Reason]),
	    throw({destroy_error, Reason});
	{ok, _} ->
	    log(info,
		"Killing program ID  ~p and all its children has succeeded~n"
		"Name : ~s~n"
		"LmId : ~s~n",
		[PghId, Name, LmId]),
	    ok
    end.

write_llog(Rank,Name,LReason,Extra) ->
    case sysEnv:rcs_mode_3() of
	hostsim ->
	    log(info,
		"Writing llog does not work in rcssim~n", [ ]);
	_ ->

	    EncodeList = [{rank,Rank},
			  {name,Name},
			  {reason,LReason},
			  {extra,Extra}],
	    case appmPghServer:req_write_llog(EncodeList) of
		{error, Reason} ->
		    log(error,
			"Writing llog has failed~n"
			"Reason: ~p~n", [ Reason]),
		    ok;
		{ok, _} ->
		    ok
	    end
    end.
    

remove_pgm_info(AppmId, Pgms) ->
    lists:keydelete(AppmId, #pgm_info.appm_id, Pgms).

get_pgm_info({pgh_id, PghId}, Pgms) ->
    lists:keyfind(PghId, #pgm_info.pgh_id, Pgms);

get_pgm_info({appm_id, AppmId}, Pgms) ->
    lists:keyfind(AppmId, #pgm_info.appm_id, Pgms);

get_pgm_info({name_cxc, Name}, []) when is_list(Name) ->
    false;
get_pgm_info({name_cxc, Name}, [H|T]) when is_list(Name) ->
    case element(#pgm_info.name_cxc, H) of
	{Name, _} -> H;
	_ -> get_pgm_info({name_cxc, Name}, T)
    end;

get_pgm_info({name_cxc, NameCxc}, Pgms) ->
    lists:keyfind(NameCxc, #pgm_info.name_cxc, Pgms).

get_app_env(undefined) ->
    [];
get_app_env(#appmPgmData{env         = AE,
			 exportedEnv = EE}) ->
    ExportedEnv =
	lists:foldl(
	  fun(Env, Acc) ->
		  case os:getenv(Env) of
		      false ->
			  Acc;
		      Value ->
			  [{Env, Value}|Acc]
		  end
	  end,
	  [], EE),
    AE ++ ExportedEnv.

get_pgm_cap(undefined) ->
    [];
get_pgm_cap(#appmPgmData{rtCap    = RtCap,
			 softRt   = SoftRt,
			 netAdmin = NetAdmin,
			 sysAdmin = SysAdmin}) ->
    RtCapDefined = is_cap_defined(RtCap),
    SoftRtDefined = is_cap_defined(SoftRt),
    NetAdminDefined = is_cap_defined(NetAdmin),
    SysAdminDefined = is_cap_defined(SysAdmin),

    if RtCapDefined -> [rt];
       true -> []
    end ++
	if SoftRtDefined -> [softrt];
	   true -> []
	end ++
	if NetAdminDefined -> [net];
	   true -> []
	end ++
	if SysAdminDefined -> [sys];
	   true -> []
	end.

get_mem_limit(undefined) ->
    unlimited;
get_mem_limit(#appmPgmData{maxMem = undefined}) ->
    unlimited;
get_mem_limit(#appmPgmData{maxMem = MaxMem}) ->
    1024*list_to_integer(MaxMem).

get_pgm_type(#appmPgmData{dataProcessing = "true"}) ->
    dp;
get_pgm_type(_) ->
    default.

-define(PGM_ESCALATION, ["PgmGrp", "BoardWarm", "BoardCold", "None"]).
-define(PGM_ESC_DEF, "Board").

get_escalation(undefined) ->
    ?PGM_ESC_DEF;
get_escalation(#appmPgmData{escalation = undefined}) ->
    ?PGM_ESC_DEF;
get_escalation(#appmPgmData{escalation = Escalation}) ->
    case lists:member(Escalation,?PGM_ESCALATION) of
	true -> Escalation;
	false ->
	    log(error,
		"Unknown escalation:  ~p~n"
		"Using default  ~p~n ",
		[Escalation,?PGM_ESC_DEF]),
	    ?PGM_ESC_DEF

    end.

get_max_restarts(undefined) ->
   ?MAX_RST_DEF_INT;
get_max_restarts(#appmPgmData{maxRestarts = MaxRes}) ->
    MaxRes.

get_max_time(undefined) ->
    ?MAX_TMO_DEF_INT;
get_max_time(#appmPgmData{maxTime = MaxTime}) ->
    MaxTime.

get_uid() ->
    list_to_integer(string:strip(os:cmd("id -u"), right, 10)).

get_gid() ->
    list_to_integer(string:strip(os:cmd("id -g"), right, 10)).


get_next_appm_id(State) ->
    NextAppmId = State#state.next_appm_id,
    {NextAppmId, State#state{next_appm_id = NextAppmId + 1}}.

%Return timestamp as {Seconds, Milliseconds} sin 1970-01-01
get_timestamp() ->
    {M,S,US} = os:timestamp(),
    MS = US div 1000,
    {M * 1000000 + S, MS}.

% Remove older timestamps than Time in list
remove_older(L,Time) ->
    remove_older(L,Time,[]).

remove_older([],_Time,Acc) ->
    lists:reverse(Acc);
remove_older([{T,_}|Rest],Time,Acc) when T < Time ->
    remove_older(Rest,Time,Acc);
remove_older([H|Rest],Time,Acc)  ->
    remove_older(Rest,Time,[H|Acc]) .



is_cap_defined(Cap) when is_list(Cap) ->
    string:to_lower(Cap) =:= "true";
is_cap_defined(_) ->
    false.

b2l(List) when is_list(List) ->
    List;
b2l(Binary) when is_binary(Binary) ->
    binary_to_list(Binary).

get_pid(Location) ->
    Cmd = "pgrep -o -fl " ++ Location,
    R = os:cmd(Cmd),
    case io_lib:fread("~u", R) of
	{ok, [Pid], _} ->
	    integer_to_list(Pid);
	_ ->
	    undefined
    end.

handle_get_info(State) ->
    Typelist = [pgms, grps, uid, gid, cpu_info, default_cpu_set],
    [{Type, handle_get_info(Type, State)} || Type <- Typelist].


handle_get_info({pgm,P}, S=#state{})  ->
    handle_get_info({pgm,P}, S#state.pgms);
handle_get_info({pgm,P}, Pgms) ->
    lists:map(
      fun(Pgm) when element(1, Pgm#pgm_info.name_cxc) == P ->
	      {Name,Cxc} = Pgm#pgm_info.name_cxc,
	      { {Name,Cxc},
		[{appmId, Pgm#pgm_info.appm_id},
		 {pghId, Pgm#pgm_info.pgh_id},
		 {grp, Pgm#pgm_info.grp},
		 {ns, Pgm#pgm_info.ns},
		 {dynamic, Pgm#pgm_info.dynamic},
		 {args, Pgm#pgm_info.args},
		 {cpuSet, Pgm#pgm_info.cpu_set},
		 {pid,Pgm#pgm_info.pid},
		 {location, Pgm#pgm_info.location},
		 {escalation, Pgm#pgm_info.escalation},
		 {maxRestarts,Pgm#pgm_info.maxRestarts},
		 {maxTimeout,Pgm#pgm_info.maxTimeout},
		 {restartList, Pgm#pgm_info.restart_list}]
	      };
	 (_)  -> []
      end,
      Pgms);

handle_get_info(pgms, S=#state{}) ->
    handle_get_info(pgms, S#state.pgms);
handle_get_info(pgms, Pgms) ->
    lists:map(
      fun(Pgm) ->
	      {Name,Cxc} = Pgm#pgm_info.name_cxc,
	      { {Name,Cxc},
		[{appmId, Pgm#pgm_info.appm_id},
		 {pghId, Pgm#pgm_info.pgh_id},
		 {grp, Pgm#pgm_info.grp},
		 {ns, Pgm#pgm_info.ns},
		 {dynamic, Pgm#pgm_info.dynamic},
		 {args, Pgm#pgm_info.args},
		 {cpuSet, Pgm#pgm_info.cpu_set},
		 {pid,Pgm#pgm_info.pid},
		 {location, Pgm#pgm_info.location},
		 {escalation, Pgm#pgm_info.escalation},
		 {maxRestarts,Pgm#pgm_info.maxRestarts},
		 {maxTimeout,Pgm#pgm_info.maxTimeout},
		 {restartList, Pgm#pgm_info.restart_list}]
	      }
      end,
      Pgms);
handle_get_info(grps, S=#state{}) ->
    handle_get_info(grps, S#state.grps);
handle_get_info(grps, Grps) ->
    lists:map(
      fun(Grp) ->
	      { Grp#grp_info.grp_name,
		[{pgmList, Grp#grp_info.pgmList},
		 {escalation, Grp#grp_info.escalation},
		 {maxRestarts,Grp#grp_info.maxRestarts},
		 {maxTimeout,Grp#grp_info.maxTimeout},
		 {restartList, Grp#grp_info.restart_list}]
	      }
      end,
      Grps);

handle_get_info(ipgms, S=#state{}) ->
    handle_get_info(ipgms, S#state.ipgms);
handle_get_info(ipgms, Ipgms) ->
    lists:map(
      fun(Ipgm) ->
	      {
		[{appmId, Ipgm#ipgm_info.appm_id},
		 {pghId, Ipgm#ipgm_info.pgh_id},
		 {ns, Ipgm#ipgm_info.ns},
		 {args, Ipgm#ipgm_info.args},
		 {envs,Ipgm#ipgm_info.envs},
		 {mfa, Ipgm#ipgm_info.mfa},
		 {owning_pid,Ipgm#ipgm_info.owning_pid},
		 {autoclean,Ipgm#ipgm_info.autoclean}]
	      }
      end,
     Ipgms );

handle_get_info(uid, State) ->
    State#state.uid;
handle_get_info(gid, State) ->
    State#state.gid;
handle_get_info(cpu_info, State) ->
    State#state.cpu_info;
handle_get_info(default_cpu_set, State) ->
    State#state.default_cpu_set.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns true if a restart of the program shall be made, i.e. no escalation
will_restart(#pgm_info{escalation="None"}  )  ->
    false;

will_restart(#pgm_info{name_cxc=Name,
		       maxTimeout=MaxTimeout,
		       maxRestarts=MaxRestarts,
		       restart_list=RestartList}  )  ->
   recent_restarts(Name, MaxTimeout, RestartList ) < MaxRestarts;
will_restart(#grp_info{grp_name=Name,
		       maxTimeout=MaxTimeout,
		       maxRestarts=MaxRestarts,
		       restart_list=RestartList}  )  ->
   recent_restarts(Name, MaxTimeout, RestartList ) =< MaxRestarts;

will_restart(false) ->
    true.

recent_restarts(Name, MaxTimeout, RestartList) ->
    {S, _} = get_timestamp(),
    OldestTime = S - MaxTimeout,
    NewRestartList = remove_older(RestartList, OldestTime),
    case Name of
	undefined ->
	    ok;
	_ ->
	    log(info,"will_restart:~p, ~nRestartlist=~p~n",
		[Name, NewRestartList])
    end,
    length(NewRestartList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_default_cpu_set(Action, PghId, CpuSet, CpuInfo) ->
    CpuList = appmPgh:decode_cpuset(CpuSet),
    NewCpuInfo =
	lists:foldl(
	  fun(Cpu, Acc) when Action =:= add ->
		  orddict:append(Cpu, PghId, Acc);
	     (Cpu, Acc) when Action =:= remove ->
		  Data = orddict:fetch(Cpu, Acc),
		  NewData = [Id || Id <- Data, Id =/= PghId],
		  orddict:store(Cpu, NewData, Acc)
	  end, CpuInfo, CpuList),
    NewDefaultCpuSet =
	lists:usort([Cpu || {Cpu, []} <- orddict:to_list(NewCpuInfo)]),
    {NewCpuInfo, NewDefaultCpuSet}.

set_def_cpuset(#state{default_cpu_set = CpuSet1},
	       #state{default_cpu_set = CpuSet2}) ->
    set_def_cpuset(CpuSet1,  CpuSet2);
set_def_cpuset(CpuSet,  CpuSet) ->
    ok;
set_def_cpuset(_,CpuSet) ->
    case sysEnv:target() of
	true ->
	    log(info, "Setting default cpuset to ~p~n", [CpuSet]),
	    {ok, []} = appmPghServer:req_set_def_cpuset([{cpus_def, CpuSet}]);
	false ->
	    log(info, "Don't set default cpuset to ~p in sim~n", [CpuSet]),
	    {ok, []}
    end.


check_cpuset(CpuSet, DefCpuSet) ->
    CpuList = appmPgh:decode_cpuset(CpuSet),
    case DefCpuSet -- CpuList of
	[] ->
	    log(error,
		"Not possible to use CpuSet ~p "
		"since it will use the last Cpu~n",
		[CpuSet]),
	    throw({check_error, faulty_default_cpu_set});
	_ ->
	    ok
    end.

update_grps(_, undefined,Groups,_AppmId,_NameCxc) ->
    Groups;
update_grps(add, Grp,Groups,AppmId,NameCxc) ->
    GrpInfo = lists:keyfind(Grp,#grp_info.grp_name,Groups),
    NewPgmList = lists:keystore(AppmId, 1, GrpInfo#grp_info.pgmList, {AppmId,NameCxc}),
    lists:keystore(Grp , #grp_info.grp_name, Groups, GrpInfo#grp_info{pgmList=NewPgmList});

update_grps(delete, Grp, Groups,AppmId,_NameCxc) ->
    GrpInfo = lists:keyfind(Grp,#grp_info.grp_name,Groups),
    NewPgmList = lists:keydelete(AppmId, 1, GrpInfo#grp_info.pgmList),
    lists:keystore(Grp, #grp_info.grp_name, Groups, GrpInfo#grp_info{pgmList=NewPgmList}).



update_pids(Pgms) ->
    lists:foldl(
      fun(PgmInfo,Acc) when PgmInfo#pgm_info.pid == undefined->
	      [PgmInfo#pgm_info{pid=get_pid(PgmInfo#pgm_info.location)}|Acc];
	 (PgmInfo,Acc) -> [PgmInfo|Acc]
      end,
      [],
      Pgms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_get_apps(State) ->
    NewState=State#state{pgms=update_pids(State#state.pgms)},
    Apps=
	lists:sort(
	  lists:flatten(
	    lists:map(
	      fun(PgmInfo) when PgmInfo#pgm_info.pgh_id =/= undefined->
		      {Name, _} = PgmInfo#pgm_info.name_cxc,
		      {Name, PgmInfo#pgm_info.pid};
		 (_) -> []
	      end,
	      NewState#state.pgms
	     )
	   )
	 ),
    {NewState,Apps}.

do_get_apps_more(State) ->
    lists:sort(
      lists:flatten(
	lists:map(
	  fun(PgmInfo) when PgmInfo#pgm_info.pgh_id =/= undefined->
		  {Name, _} = PgmInfo#pgm_info.name_cxc,
		  {Name,
		   PgmInfo#pgm_info.appm_id,
		   PgmInfo#pgm_info.pgh_id};
	     (_) -> []
	  end,
	  State#state.pgms)
       )
     ).

do_get_groups(State) ->
    State#state.grps.


handle_warm_cnf(Result,State) ->
    case {get(state),Result} of
	{warm_restart, success} ->
	    alhI:warm_restart(starting),
	    call_warm_cbs(warm_done, "informing MW applications of warm done restart~n "),
	    {ok, NewState} = start_all_lms(State),
	    on_core(comsaI,start_com,[[]]),
	    sysInitI:restart_logger_trace(?MODULE,
					  ?LINE,
					  "non-upgrade; applications started"),
	    reset_operational_led(),
	    put(state,active),
	    alhI:warm_restart(operational),
	    {NewPgms,NewGrps} = do_reset_restart_lists(NewState),
	    {noreply, NewState#state{pgms = NewPgms, grps = NewGrps}};
	{warm_restart, _} ->
	    do_cold_restart(false,
			    "'Warm restart failed, escalate to cold'" ,
			    State),
	    {noreply, State};
	{_,_} ->
	    {noreply, State}
    end.



%%%
%%%  COM is only available on core
%%%
on_core(M,F,A) ->
     case clhI:mp_role() of
	core ->
	     apply(M,F,A);
	 _ -> ok
     end.


%%%
%%%  A cold restart is not handled by heart,
%%%
%%%

do_restart_piu(warm = Rank, Reason, Cause, AppmCause,State) ->
    case get(state) of
	warm_restart ->
	    log(info, "Warm already ongoing~n",[]),
	    State;
	_ ->

	    set_restart_info(AppmCause, Rank),
	    case is_warm_restart_enabled() of
		true->

		    log(info,
			"########## Doing a warm restart #########~n",
			[]),
		    avli_node_event({outOfService, Reason, Cause, Rank}),
		    put(state, warm_restart),
		    ReasonStr = get_reason_str(Cause),
		    do_warm_restart(ReasonStr, State);
		false ->
		    log(info,
			"~n"
			"########## Doing a cold restart        #########~n"
			"########## even though warm requested  #########~n",
			[]),
		    avli_node_event({outOfService, Reason, Cause, cold}),
                    case sysEnv:rcs_mode_2() of
                        simulated ->
			    update_heart_cmd(Cause, false),
			    put(state, shutting_down),
			    init:reboot(),
			    State;
			_ -> % Skip the controlled shutdown on target
			    ReasonArg = get_reason_str(Cause),
			    do_cold_restart(false, ReasonArg, State)
		    end
	    end
    end;

do_restart_piu(cold = Rank, Reason, Cause, AppmCause,State) ->
    log(info, "########## Doing a cold restart #########~n", []),
    set_restart_info(AppmCause,Rank),
    set_vnfc_info(?RESTART_USR),
    avli_node_event({outOfService, Reason, Cause, Rank}),
    case sysEnv:rcs_mode_2() of
        simulated -> 
            update_heart_cmd(Cause, true),
	    put(state, shutting_down),
	    init:reboot(),
	    State;
        _ -> 
	    ReasonArg = get_reason_str(Cause),
	    do_cold_restart(false, ReasonArg, State )
    end;

do_restart_piu(cold_with_test = Rank, Reason, Cause, AppmCause,State) ->
    log(info, "########## Doing a cold_with_test restart #########~n", []),
    set_restart_info(AppmCause,Rank),
    avli_node_event({outOfService, Reason, Cause, Rank}),
    catch sysInitI:save_logs(),
    case sysEnv:rcs_mode_2() of
	simulated -> 
	    update_heart_cmd(Cause, true),
	    put(state, shutting_down),
	    init:reboot(),
	    State;
	_ ->
	    ReasonArg = get_reason_str(Cause),
	    do_cold_restart(true, ReasonArg ,State)
    end;

do_restart_piu(revert = Rank, Reason, Cause, AppmCause,State) ->
    set_restart_info(AppmCause, Rank),
    avli_node_event({outOfService, Reason, Cause}),
    initiate_revert(),
    State.

remove_dynamic_pgms(State) ->
    remove_dynamic_pgms(State#state.pgms, State).
remove_dynamic_pgms(Pgms, State) ->
    DynPgms =
	lists:filter(
	  fun(P) ->  P#pgm_info.dynamic  end,
	  Pgms),
     State#state{ pgms = State#state.pgms -- DynPgms}.


do_warm_restart(ReasonStr,State) ->
    put(state, warm_restart),
    call_warm_cbs(prep_warm,
		  "informing MW applications to prepare for warm restart~n "),

    log(info, "stopping applications~n ", []),
    NewState = do_stop_lms(State),
    call_warm_cbs(warm, "informing MW applications of warm restart~n "),
    log(info, "stopping COM~n ", []),
    on_core(comsaI,stop_com,[]),
    sysInitApp:inc_warm_cnt(),
    alhI:warm_restart(nodeDown),

    case sysEnv:rcs_mode() of
	target ->
	    swmI:clear_application_tmp(),
	    log(info, "Requesting EE to do a warm restart~n ", []),
	    appmPghServer:req_warm_restart([{reason,ReasonStr}]),
	    ets:insert(appm_ets, {warm_time,os:timestamp()}),
	    NewState;
	simulated ->
	    swmI:clear_application_tmp(),
	    alhI:warm_restart(starting),
	    call_warm_cbs(warm_done,
			  "informing MW applications of warm done restart~n "),
	    {ok, NewerState} = start_all_lms(NewState),
	    on_core(comsaI,start_com,[[]]),
	    sysInitI:restart_logger_trace(?MODULE,
					  ?LINE,
					  "non-upgrade; applications started"),
	    reset_operational_led(),
	    put(state,active),
	    alhI:warm_restart(operational),
	    ets:insert(appm_ets, {warm_time,os:timestamp()}),
	    {NewPgms,NewGrps} = do_reset_restart_lists(NewerState),
	    NewerState#state{pgms = NewPgms, grps = NewGrps}
    end.

do_cold_restart(TestFlag, ReasonArg ) ->
    %% only used for revert 
    put(state, shutting_down),
    log(info, "syncing mnesia_log~n ", []),
    on_core(swmI,force_auto_backup,[]),
    log(info, "stopping ssh via OMC~n ", []),
    on_core(omc_api,prepare_to_shutdown,[]),
    call_cold_cbs(prepare_cold,
		  "informing MW applications of cold restart~n "),
    do_cold_restart2(TestFlag, ReasonArg ).

do_cold_restart(TestFlag, ReasonArg, State ) ->
    put(state, shutting_down),
    log(info, "syncing mnesia_log~n ", []),
    on_core(swmI,force_auto_backup,[]),
    log(info, "stopping ssh via OMC~n ", []),
    on_core(omc_api,prepare_to_shutdown,[]),
    call_cold_cbs(prepare_cold,
		  "informing MW applications of cold restart~n "),
    case appmAppData:get_cb_apps("restart_cold") of
	[] ->
	    log(info, "No restart_cold callbacks registered.~n",[]),
	    ok;
	CbApps ->
	    log(info, "Restart_cold callbacks registered: ~p~n"
		"will be called after all applications stopped~n ", [CbApps]),
	    do_stop_lms(State),
	    call_restart_cold_cbs(CbApps)
    end,
    do_cold_restart2(TestFlag, ReasonArg ),
    State.

do_cold_restart2(TestFlag, ReasonStr ) ->
    log(info, "Calling EE to perform board restart with~n"
	"hw_test= ~p~n"
	"reason= ~p~n", [TestFlag,ReasonStr]),
    case sysEnv:vrcs() of 
        false ->
            appmPghServer:req_restart_brd([{hw_test,TestFlag},
				  {reason,ReasonStr}]);
        true ->
            case send_httpc_heal_request(vnfcI:is_standalone(), ReasonStr) of
                nok ->
                    log(info, 
                        "Set Heartbeat reply to Service Unavailable~n ", []),
                    vnfcI:stop_heartbeat(),
                    nok;
                ok -> ok
            end
    end.

send_httpc_heal_request(false, ReasonStr) ->
    case vnfcI:send_heal(?MODULE, ReasonStr) of
        ok ->
            ok;
        _ ->
            nok
    end;
send_httpc_heal_request(_Vnfm,_ReasonStr) ->
    % for vrcs test
    init:reboot().

alh_to_appm(RestartCause)
  when RestartCause == ?ALH_TAG_ExtRestartRequest orelse
       RestartCause == ?ALH_TAG_UpgradeNormal orelse
       RestartCause == ?ALH_TAG_UpgradeCancelled orelse
       RestartCause == ?ALH_TAG_DataRestore orelse
       RestartCause == ?ALH_TAG_SoftwareRestore orelse
       RestartCause == ?ALH_TAG_ManualRestart ->
    manual;
alh_to_appm("ManualCOLI") ->
    manual;
alh_to_appm(_) ->
    crash.

call_pgroup_cbs(Func,Pgroup,Str) ->
    Mods = appmAppData:get_cb_apps(pgroup_cb),
    log(info, Str ++ "Calling ~p(~p) in ~p ~n", [Func,Pgroup,Mods]),
    [catch apply(Mod,Func,[Pgroup]) || Mod <- Mods].

call_warm_cbs(Func,Str) ->
    Mods = appmAppData:get_cb_apps(warm_cb),
    log(info, Str ++ "Calling ~p in ~p ~n", [Func,Mods]),
    [catch apply(Mod,Func,[]) || Mod <- Mods].

call_cold_cbs(Func,Str) ->
    Mods = appmAppData:get_cb_apps(cold_cb),
    log(info, Str ++ "Calling ~p in ~p ~n", [Func,Mods]),
    [catch apply(Mod,Func,[]) || Mod <- Mods].

update_heart_cmd(Cause, HwTest) ->
    update_heart_cmd(Cause, HwTest, sysEnv:target()).

update_heart_cmd(_Cause, _HwTest, false) ->
    ok;

update_heart_cmd(Cause, HwTest, true) ->
    {ok,CurrHeartCmd} = heart:get_cmd(),
    case string:str(CurrHeartCmd,"pgh_restart_board") of
	0 -> % SWM has changed heart command
	    ok;
	_ ->
	    HwTestArg = get_hwtest_arg(HwTest),
	    ReasonArg = get_reason_arg(Cause),
	    ets:insert(appm_ets,{esc_cnt,""}),

	    %% wait 20 secs to give EE time to genereate PMD
	    HeartCmd = "sleep 20; pgh_restart_board" ++ HwTestArg ++ ReasonArg,
	    HScript = filename:join([code:priv_dir(sys),"bin","restart_hook.sh"]),
	    HookScript = swmI:find_file(HScript),
	    HCmd= HookScript  ++ ";" ++ HeartCmd,
	    heart:set_cmd(HCmd),
	    error_logger:info_msg("appmServer: changed HEART_CMD to ~p",[HCmd])
    end.

get_hwtest_arg(true)  -> " -t";
get_hwtest_arg(false) -> "".

get_reason_arg(Cause) ->
    " -r " ++ get_reason_str(Cause).

get_reason_str(?ALH_TAG_ExtRestartRequest) ->
    "'Manual restart'";
get_reason_str(?ALH_TAG_UpgradeNormal) ->
    "'Upgrade activate'";
get_reason_str(?ALH_TAG_UpgradeCancelled) ->
    "'Upgrade cancelled'";
get_reason_str(?ALH_TAG_DataRestore) ->
    "'Data restore'";
get_reason_str(?ALH_TAG_SoftwareRestore) ->
    "'Software restore'";
get_reason_str(?ALH_TAG_UpgradeFailure) ->
    "'Upgrade failure'";
get_reason_str(?ALH_TAG_UpgradeTimeout) ->
    "'Upgrade timeout'";
get_reason_str(?ALH_TAG_UpgradeProgramError) ->
    "'Program error during upgrade'";
get_reason_str("ClockUpdateExceededMaxDiff") ->
    "'NTP max clock diff exceeded'";
get_reason_str("ManualCOLI") ->
    "'Manual COLI restart'";
get_reason_str("Unexpected") ->
    "'Unexpected restart,cnt_clear'";
get_reason_str(_) ->
    "'Restart due to escalation'".

%%%-----------------------------------------------------------------------------
initiate_revert() ->
    case get_revert_state() of
	undefined ->
	    log(info, " spawning make_esi ~n", []),
	    spawn(?MODULE,make_esi,[]), % to store esi at start of reversion
	    set_revert_state(latest),
	    start_revert(latest);
	latest ->
	    set_revert_state(after_ug),
	    start_revert(after_ug);
	after_ug ->
	    set_revert_state(before_ug),
	    start_revert(before_ug);
	before_ug ->
	    set_revert_state(installation),
	    start_revert(installation)
    end,

    ok.

start_revert(Next) ->
    log(info, "########## Starting a revert restart timeout period to ~p ########## ~n", [Next]),
    send_alarm(?RE_ALARM),
    TimeForward = get_revert_timeout() ,
    log(info, " Revert will occur in ~p seconds  ~n", [TimeForward]),
    Timeout = os_now() + TimeForward,
    {ok,Tref} = timer:apply_interval(5000,?MODULE,revert_check,[]),
    mnesia:dirty_write({appmNodeTable,revert_waiting, {Timeout, Next, Tref}}).


revert_check() ->
    case mnesia:dirty_read({appmNodeTable,revert_waiting}) of
	[#appmNodeTable{value={Timeout, Next, Tref}}] ->
	    case is_oss_alive() of
		true ->
		    log(info,
			"~n###################################~n"
			"Someone has logged in ~n"
			"will stop reverting ~n", []),
		    timer:cancel(Tref),
		    clear_alarm(?RE_ALARM),
		    send_alarm(?GP_ALARM),
            timer:apply_after(5000, ?MODULE, remove_rollback_esi, []),
		    mnesia:dirty_delete({appmNodeTable,revert_waiting});
		_ ->
		    case os_now() > Timeout of
			true ->
			    log(info,
				"~n###################################~n"
				"Revert timeout has elapsed~n"
				"will start reverting to ~p~n", [Next]),
			    timer:cancel(Tref),
			    mnesia:dirty_delete({appmNodeTable,revert_waiting}),
			    revert(Next);
			false ->
			    %% log(info,
			    %% 	"~n###################################~n"
			    %% 	"Revert timeout will occur in ~p seconds~n"
			    %% 	"~n", [(Timeout - os_now()) ]),
			    ok
		    end
	    end;
	[] ->
	    ok
    end.

remove_rollback_esi() ->
    case call(remove_rollback_esi) of
        restart_timer ->
            timer:apply_after(5000, ?MODULE, remove_rollback_esi, []);
        _ ->
            ok
    end.

%% Do this after a restart on active
restart_revert_check(active) ->
     case mnesia:dirty_read({appmNodeTable,revert_waiting}) of
	[#appmNodeTable{value={Timeout, Next, _Tref}}] ->
	     {ok,Tref} = timer:apply_interval(5000,?MODULE,revert_check,[]),
	     mnesia:dirty_write({appmNodeTable,revert_waiting,
				 {Timeout, Next, Tref}});
	 _ ->
	     ok
     end;
restart_revert_check(_) ->
    ok.


%Here the actual revert is triggered
revert(installation) ->
    case sysEnv:architecture() of
	{"arm",_} ->
	    log(info,
		"~n#################################################~n"
		"########## Revert to networkloader ############~n", []),
	    disk_log(" Revert to networkloader ~n", []),
	    sysRhai:setbootptr(nl),
	    update_heart_cmd("Install", false),
	    put(state, shutting_down),
	    do_cold_restart(false, "'Revert to NL'" );
	{Arch,_} ->

	    log(info,
		"~n#################################################~n"
		"Here we should call EE to revert to networkloader~n"
		"but it is NOT YET IMPLEMENTED for Arch= ~p~n", [Arch])
    end;

revert(S) ->
    case swmI:revert(S) of
	ok ->
	    disk_log(" Revert to ~p ~n", [S]),
	    ok;
	nok ->
	    %% Not possible to revert to S
	    Prev = prev_revert(S),
	    log(info,
		"~n###################################~n"
		"SWM has no backup for ~p~n"
		"So we try with ~p~n", [S,Prev]),
	    disk_log("SWM has no backup for ~p~n try with ~p~n", [S,Prev]),
	    revert(Prev)
    end.

prev_revert(latest) ->
    after_ug;
prev_revert(after_ug) ->
    before_ug;
prev_revert(before_ug) ->
    installation.

%%%-----------------------------------------------------------------------------
revert_file_name() ->
    filename:join([sysEnv:rcs_dir(),"swm","revert_state"]).

set_revert_state(State) ->
    log(info,
	"~n###################################~n"
	"Setting revert state to ~p~n", [State]),
    disk_log("Setting revert state to ~p~n", [State]),
    file:write_file(revert_file_name(),atom_to_list(State)).
get_revert_state() ->
    Rstate =
	case file:read_file(revert_file_name()) of
	    {error,_} ->
		undefined;
	    {ok,Bin}->
		list_to_atom(binary_to_list(Bin))
	end,
    log(info,
	"~n###################################~n"
	"Read revert state ~p~n", [Rstate]),
    Rstate.

%%%-----------------------------------------------------------------------------
%%% check if OSS has logged in within one hour after last restart
is_oss_alive() ->
	case mnesia:dirty_read(appmNodeTable, oss_alive) of
	    [#appmNodeTable{value = IsAlive}] -> %for testing
		IsAlive;
	    [] ->
		try omc_api:is_activity_since_restart() of
		    Res -> Res
		catch
		    X:Y ->
			OmcServerStatus = (catch sys:get_status(omc_server)),
			log(info,"Call to omc_api failed with ~p~n"
			    "Status of omc_server~n~p~n",
			    [{X,Y},OmcServerStatus]),
			false
		end
	end.

%%%-----------------------------------------------------------------------------
send_alarm(Alarm) ->
    log(info,"-----Sending ~p alarm~n", [Alarm]),
    comsaI:send_alarm(Alarm, alarm_to_severity(Alarm),alarm_to_dn(Alarm), []).


clear_alarm(Alarm) ->
    log(info,"-----Clearing ~p alarm~n", [Alarm]),
    comsaI:clear_alarm(Alarm, alarm_to_dn(Alarm) ).

alarm_to_dn(?RE_ALARM) -> ?RE_DN;
alarm_to_dn(?GP_ALARM) -> ?GP_DN.

alarm_to_severity(?RE_ALARM) -> ?RE_SEV ;
alarm_to_severity(?GP_ALARM) -> ?GP_SEV.

cleanup_esi() ->
    AllNodes = clhI:erlang_nodes(all),
    NlDir = filename:join([sysEnv:rcs_dir(), "networkloader"]),
    Cmd = "rm " ++ filename:join([NlDir, "esi.*.gz*"]),
    rpc:multicall(AllNodes, os, cmd, [Cmd]).

%%%-----------------------------------------------------------------------------
%%% create an ESI at start of rollback sequence
make_esi() ->
    log(info, "Creating an ESI at start of rollback sequence~n", []),
    cleanup_esi(),
    cast(make_esi_start),
    {ResL, BadNodes} = logEsi:generate_esi_on_all_mps(),
    {OkL, NokL} = get_result(ResL),
    log(info, "Created esi OK ~p; NOK ~p; BadNodes ~p~n", [OkL, NokL, BadNodes]),
    NlDir = filename:join([sysEnv:rcs_dir(), "networkloader"]),
    [rpc:call(Node, os, cmd, ["mv "++EsiFile  ++ " " ++ NlDir]) || {Node, EsiFile} <- OkL],
    cast(make_esi_done).

get_result(ResL) ->
    get_result(ResL, _OkL = [], _NokL = []).

get_result([{Node, full, EsiFile} | T], OkL, NokL) ->
    get_result(T, OkL++[{Node, EsiFile}], NokL);
get_result([{Node, none} | T], OkL, NokL) ->
    get_result(T, OkL, NokL++[Node]);
get_result([_ | T], OkL, NokL) ->
    get_result(T, OkL, NokL);
get_result([], OkL, NokL) ->
    {OkL, NokL}.

%%%-----------------------------------------------------------------------------
avli_node_event(Event) ->
    avli_node_event(clhI:core_state(), Event).

avli_node_event(active, operational) ->
    AddInfo = case swmI:is_upgrade_ongoing() of
		  true ->
		      ?ALH_TAG_Cause(?ALH_TAG_ExtUpgradeRequest);
		  false ->
		      []
	      end,
    alhI:write_node_event(os:timestamp(),
			  ?ALH_TAG_InService,
			  ?ALH_TAG_Operational,
			  0,   % EventId
			  ?ALH_TAG_Rcs(AddInfo),
			  async);
avli_node_event(active, {outOfService, Reason, AvliCause, Rank}) ->
    AddInfo = lists:flatten([avli_get_rank(Rank),
			     ?ALH_TAG_Cause(AvliCause)]),
    alhI:write_node_event(os:timestamp(),
			  ?ALH_TAG_OutOfService,
			  avli_get_reason(Reason),
			  0,   % EventId
			  ?ALH_TAG_Rcs(AddInfo));
avli_node_event(active, {outOfService, Reason, AvliCause}) ->  %revert case
    alhI:write_node_event(os:timestamp(),
			  ?ALH_TAG_OutOfService,
			  avli_get_reason(Reason),
			  0,   % EventId
			  ?ALH_TAG_Rcs([?ALH_TAG_Cause(AvliCause)]));
avli_node_event(_, _) ->
    %% The DU is not active -> no node events shall be logged.
    ok.

%%%-----------------------------------------------------------------------------

avli_pgroup_event(PgmEvent, GrpName) ->
    Cause = avli_get_pgm_cause(PgmEvent),
    alhI:write_pgm_event(os:timestamp(),
			 ?ALH_TAG_OutOfService,
			 ?ALH_TAG_Unoperational,
			 undefined,
			 undefined,
			 {GrpName, ""},
			 ?ALH_TAG_Rcs(?ALH_TAG_Cause(Cause)),
			 async).

%%%-----------------------------------------------------------------------------
avli_pgm_event(true, pgmstart, {_, Cxc} = NameCxc) ->
    {ok, Rev} = appmAppData:get_lm_rev(NameCxc),
    alhI:write_pgm_event(os:timestamp(),
			 ?ALH_TAG_InService,
			 ?ALH_TAG_Starting,
			 undefined,
			 undefined,
			 {Cxc, Rev},
			 ?ALH_TAG_Rcs([]),
			 async);
avli_pgm_event(true, PgmEvent, {_, Cxc} = NameCxc) ->
    {ok, Rev} = appmAppData:get_lm_rev(NameCxc),
    Cause = avli_get_pgm_cause(PgmEvent),
    alhI:write_pgm_event(os:timestamp(),
			 ?ALH_TAG_OutOfService,
			 ?ALH_TAG_Unoperational,
			 undefined,
			 undefined,
			 {Cxc, Rev},
			 ?ALH_TAG_Rcs(?ALH_TAG_Cause(Cause)),
			 async);
avli_pgm_event(false, _, _) ->
    ok.

%%%-----------------------------------------------------------------------------
avli_get_pgm_cause(PgmEvent) when PgmEvent == pgmcrash orelse
				  PgmEvent == pgmhanging ->
    case swmI:is_upgrade_ongoing() of
	true ->
	    ?ALH_TAG_UpgradeProgramError;
	false ->
	    ?ALH_TAG_ProgramFailure
    end;
avli_get_pgm_cause(pgmterm) ->
    ?ALH_TAG_RestartRequest;
avli_get_pgm_cause(EscRestartLevel) ->
    escalation_cause(EscRestartLevel).

%%%-----------------------------------------------------------------------------
avli_get_rank() ->
    avli_get_rank(appmRhai:read("restart_type")).

%%%-----------------------------------------------------------------------------
avli_get_rank(warm) ->
    ?ALH_TAG_RankWarm;
avli_get_rank(cold) ->
    ?ALH_TAG_RankCold;
avli_get_rank(cold_with_test) ->
    ?ALH_TAG_RankColdWTest;
avli_get_rank("COLD") ->   % From appmRhai.
    ?ALH_TAG_RankCold;
avli_get_rank("COLDTEST") ->   % From appmRhai.
    ?ALH_TAG_RankColdWTest;
avli_get_rank(Rank) when is_list(Rank) ->   % From appmRhai.
    [];
avli_get_rank(UnrecRank) ->
    sysInitI:error_report([{?MODULE, avli_get_rank},
			       {unrecognized_rank, UnrecRank},
			       {stacktrace, erlang:get_stacktrace()}]),
    [].

avli_get_reason(shutdownCommand) ->
    ?ALH_TAG_ShutdownCommand;
avli_get_reason(_) ->
    ?ALH_TAG_Unoperational.

update_operational_led() ->
    eqs_vii:'CelloVii_visualIndRequest'(?CELLO_VII_MISSING_RESOURCE_START).

reset_operational_led() ->
    eqs_vii:'CelloVii_visualIndRequest'(?CELLO_VII_MISSING_RESOURCE_END).

add_monitor(undefined, #state{monitored = Mon}) ->
    Mon;
add_monitor(Owner, #state{monitored = Mon}) ->
    case lists:member(Owner, Mon) of
	true ->
	    Mon;
	false ->
	    monitor(process, Owner),
	    [Owner | Mon]
    end.


get_prod_no_rev() ->
    BI = eqs_pri_service:get_product_information_data(),
    No = proplists:get_value(productNumber, BI),
    Rev = proplists:get_value(productRevision, BI),
    MName = proplists:get_value(marketName, BI,""),
    SerNo = proplists:get_value(serialNumber, BI,""),
    {No,Rev,MName,SerNo}.


%%%-----------------------------------------------------------------------------
remount_apptmp_size(true) ->
    MH = #appmLmData{nameCxc = '$1',
		     cxpNo = '$2',
		     cxpRev = '$3',
		     mpInfo = #appmPgmData{tmpSize = '$4', _ = '_'},
		     _ = '_'},
    MS = [{MH, [{is_integer, '$4'}], ['$$']}],
    case mnesia:transaction(fun() -> mnesia:select(appmLmData, MS) end) of
	{atomic, AppSizes} when AppSizes =/= [] ->
	    remount_apptmp_size_sum(AppSizes);
	_ ->
	    ok
    end;

remount_apptmp_size(_False) ->
    ok.


remount_apptmp_size_sum(AppSizes) ->
    %% There is a bit of unnecessary traversing of the list here in order
    %% to get some nice log output, but the list is rather short so it
    %% should be ok.
    Str = lists:flatten([io_lib:format("~-24.. s ~-23.. s ~7.. w~n",
				       [App, CXS, Sz]) ||
			    [{App, CXS}, _CXP, _Rev, Sz] <- AppSizes]),
    sysInitI:info_msg("~p: ~p~nRequested tmpSize in MB for applicationtmp "
		      "directories~n~n~s~n",
		      [?MODULE, remount_apptmp_size_sum, Str]),
    CXPSz = lists:foldl(fun([_AppCxs, CXP, Rev, Size], Acc) ->
				Key = {CXP, Rev},
				case lists:keyfind(Key, 1, Acc) of
				    false ->
					[{Key, Size} | Acc];
				    {_Key, AccSize} ->
					lists:keyreplace(Key, 1, Acc,
							 {Key, Size + AccSize})
				end
			end, [], AppSizes),
    {DirSizes, LogStr} =
	lists:mapfoldl(fun({{CxpProdId, CxpVersion}, TmpSize}, Acc) ->
			       {ok, CxpPath} = swmI:get_cxp_path(CxpProdId,
								 CxpVersion),
			       TmpDir = filename:join([app_tmp_root(),
						       log_dir(CxpPath)]),
			       SizeStr = integer_to_list(TmpSize) ++ "M",
			       AccLog = SizeStr ++ "\t" ++ TmpDir ++ "\n",
			       {{SizeStr, TmpDir}, AccLog ++ Acc}
		       end, [], CXPSz),
    sysInitI:info_msg("~p: ~p~nRemount tmpfs with size for applicationtmp dir"
		      "~n~n" ++ LogStr ++ "~n",
		      [?MODULE, remount_apptmp_size_sum]),
    [swmI:remount_tmpfs_size_cmd(Size, Path)||{Size, Path}<-DirSizes],
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% This is called from aicSnmp to kill snmpd so that is restarted by COM
%%  and new snmp configuration is read
%%%
comea_snmp(stop_from_aic) ->
    %% Call wrapper script to avoid getting errors in syslog in case
    %% snmpd is already killed
    OrigProg = filename:join([code:priv_dir(appm),"bin", "kill_snmp.sh"]),
    Prog = swmI:find_file(OrigProg),
    Args=[Prog],
    appmPghServer:req_spawn_pgm([{args, Args}, {flag,3} ]),
    ok;

%%% This is called from comea to stop snmpd
comea_snmp(stop) ->
    comea_snmp_cb_clean(),
    ProgOrig = filename:join([sysEnv:com_top(),
			      "opt/com/comea/scripts/comea-snmp"]),
    Prog = swmI:find_file(ProgOrig),
    Args=[Prog|["snmp","stop"]],
    Envs = get_comea_envs(),
    comea_flag_set(),
    appmPghServer:req_spawn_pgm([{args, Args},  {envs, Envs} , {flag,3} ]),
    comea_flag_wait(),
    ok;

%%% This is called from the comea script in COM to run comea-snmp
%%% with correct privileges.
comea_snmp(start) ->
    run_comea_snmp(comea_snmp_cb_keep);

comea_snmp(isDtlsSupported) ->
    ProgOrig = filename:join([sysEnv:com_top(),
                  "opt/com/comea/scripts/comea-snmp"]),
    Prog = swmI:find_file(ProgOrig),
    %% Remove PATH
    ComeaEnvs = lists:filter(fun(EnvVar) ->
                                string:substr(EnvVar, 1, 5) =/= "PATH="
                        end, get_comea_envs()),
    
    ComeaEnvs2 = ComeaEnvs ++ get_oam_ns_env(),
    ComeaEnvsJoin = string:join(ComeaEnvs2, " "),
    Args = "snmp isDtlsSupported",
    ComeaSnmpCmd = string:join([ComeaEnvsJoin, Prog, Args], " "),
    %% Call as sirpa, output:"yes" or "no (...)"
    os:cmd(ComeaSnmpCmd);

comea_snmp(_) ->
    run_comea_snmp(comea_snmp_cb_stop).

run_comea_snmp(CB) ->
    case file:read_file(sysEnv:rcs_root() ++ "/tmp/comea-snmp-args") of
	{ok,Str} ->
	    file:delete("/tmp/comea-snmp-args"),
	    A=binary_to_list(Str),
	    Args = string:tokens(A," \n"),
	    do_run_comea_snmp(CB,Args);
	{error,Reason} ->
	    log(warning,"/tmp/comea-snmp-args file not found, Reason ~p~n",
		[Reason])
    end.

do_run_comea_snmp(CB,Args) ->
    ProgOrig = filename:join([sysEnv:com_top(),
			      "opt/com/comea/scripts/comea-snmp"]),
    Prog = swmI:find_file(ProgOrig),

    Envs = get_comea_envs(),
    comea_flag_set(),
    %%log(info,"Run comea-snmp ~p~n",[Args]),
    start_internal_lm([{args,[Prog|Args]},
		       {envs,Envs},
		       {name,"comea-snmp"},
		       {mfa,{?MODULE,CB,[]}},
		       {ugid,{0,0}}]),

    comea_flag_wait().


comea_flag_wait() ->
    LockFile = comea_lock_file(),
    comea_flag_wait(100,LockFile).

comea_flag_wait(0,LockFile) ->
    case file:read_file(LockFile) of
	{ok,Bin} ->
	    log(warning,"comea-snmp still locked from ~p ~n, continue anyway~n",
		[binary_to_list(Bin)]),
	    ok;
	_ ->
	    ok
    end;
comea_flag_wait(N,LockFile) ->
     case is_comea_snmp_locked(LockFile) of
	true ->
	     timer:sleep(100),
	     comea_flag_wait(N-1,LockFile);
	false ->
	     ok
     end.

is_comea_snmp_locked(LockFile) ->
    case file:read_file_info(LockFile) of
	{error, _} ->
	    false;
	{ok, _} ->
	    true
    end.

comea_flag_set() ->
    LockFile = comea_lock_file(),
    file:write_file(LockFile,
		    comsaLib:iso_time(os:timestamp(),extended)).

comea_lock_file() ->
    filename:join([comea_root_dir(),"comea-snmp.lock"]).


get_comea_envs() ->
    ComeaDir=comea_root_dir(),
    SnmpdConf=filename:join([ComeaDir,"etc","snmpd.conf"]),
	RcsMode = os:getenv("RCS_MODE"),
	RcsRoot= os:getenv("RCS_ROOT"),
    NewPath=filename:join([ComeaDir,"bin"])++";"++os:getenv("PATH"),
    ["COMEA_ROOT_DIR="++ComeaDir,
     "COMEA_CONF_DIR="++ComeaDir,
     "SNMPD_CONF="++SnmpdConf,
     "PATH="++NewPath,
	 "RCS_MODE="++RcsMode,
	 "RCS_ROOT="++RcsRoot].

comea_root_dir() ->
    filename:join([sysEnv:releases_vsn_dir(),"comte","comea"]).

get_oam_ns_env() ->
    case file:read_file(?NS_FILE) of
        {ok, NS} ->
            ["OAM_NET_NS=" ++ binary_to_list(NS)];
        _ ->
            []
    end.

enable_comea_via_appm() ->
    File = filename:join([comea_root_dir(),"comea-appm-enable"]),
    file:write_file(File,"true").

disable_comea_via_appm() ->
    File = filename:join([comea_root_dir(),"comea-appm-enable"]),
    file:delete(File).



comea_snmp_cb_clean() ->
    case ets:lookup(appm_ets,comea_pghid) of
	[] -> ok;
	[{_,OldPghId}] ->
	    appmPghServer:req_destroy_pgm([{pgm_id, [OldPghId]}]),
	    ets:delete(appm_ets,comea_pghid)
    end.
comea_snmp_cb_keep(PghId) ->
    ets:insert(appm_ets,{comea_pghid,PghId}),
    ok.
comea_snmp_cb_stop(PghId) ->
    appmPghServer:req_destroy_pgm([{pgm_id, [PghId]}]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


remove_obsolete_applogdirs() ->
    {ok,Dirs}=
	file:list_dir(filename:join([sysEnv:rcs_dir(),"applicationlogs"])),
    [rem_log_dir(D) || D <- Dirs],
    ok.

rem_log_dir(Dir) ->
    case filelib:wildcard(
	   filename:join([sysEnv:home_dir(),"software",Dir ++ "_*"])) of
	[] ->
	    LogDir = filename:join([sysEnv:rcs_dir(),"applicationlogs",Dir]),
	    Cmd=["rm","-rf",LogDir],
	    appmPghServer:req_spawn_pgm([{args, Cmd},  {flag,3} ]),
	    log(info, "Removed obsolete log dir ~p~n", [LogDir]);
	_ ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Only for VRCS where there is no NC/FRUM that can create MPs
auto_create_cluster(false) -> ok;
auto_create_cluster(true) ->
    Role = clhI:mp_role(),
    AlreadyConfigured =
	filelib:is_file(filename:join(sysEnv:home_dir(),
				      "cluster_complete")),
    MpList =
	case os:getenv("REGULAR_MP_LIST") of
	    false ->
		false;
	    [] ->
		false;
	    MpL ->
		{ok,MpL}
	end,

    case {Role, AlreadyConfigured, MpList } of
	{core, false, {ok,L}} ->
	    log(info,"creating cluster MPs with FRU prefix ~p and MPs  ~p~n",
		[os:getenv("FRU_NAME"),L]),
	    RegularMPs = [list_to_integer(X) || X <- string:tokens(L," ")],
	    spawn(?MODULE, create_mps, [RegularMPs] );
	_ ->
	    ok
    end.


create_mps(RegularMPs) ->
    FruName=os:getenv("FRU_NAME"),
    FN1=list_to_binary(FruName ++ "1"),
    clhI:associate_mp(1,FN1,primary),
    create_mps(FruName,RegularMPs).

create_mps(_FruName,[]) ->
    ok;
create_mps(FruName,[H|T]) ->
    FN=list_to_binary(FruName ++ integer_to_list(H)),
    clhI:associate_mp(H,FN,undefined),
    create_mps(FruName,T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ITC stuff used in LMHI add_board replies.
%%%
itc_open() ->
    ItcPort=itc:open(?ITC_PORT_NAME),
    ets:insert(appm_add_board, {itcPort,ItcPort }).

get_itc_port() ->
    case ets:lookup(appm_add_board, itcPort) of
	[{_,ItcPort}] ->
	    ItcPort;
	_ ->
	    false
    end.

itc_close() ->
    itc_close(get_itc_port()).


itc_close(false) ->
    ok;
itc_close(ItcPort) ->
    itc:close(ItcPort).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%


call_restart_cold_cbs(CbApps) ->
    Pids = [ spawn_link(?MODULE, call_restart_cold_cb, [self(),X] ) || X <- CbApps ],
    rc_wait(lists:zip(Pids, CbApps), []).

rc_wait([],Acc) ->
    lists:reverse(Acc);
rc_wait(Pids,Acc) ->
    Timeout = rcCb_timeout(),
    receive
	{ok,Ans,Pid} ->
	    unlink(Pid),
	    NewPids = lists:keydelete(Pid,1,Pids),
	    rc_wait(NewPids, [Ans|Acc]);
	{'EXIT',_Pid,normal} ->
	    rc_wait(Pids,Acc);
	{'EXIT',Pid,Reason} ->
	    NewPids = lists:keydelete(Pid,1,Pids),
	    rc_wait(NewPids, [{exit,Pid,Reason}|Acc])
    after Timeout ->
	    sysInitI:info_msg(
	      "~p:call_restart_cold_cbs: timeout from these : ~n ~p~n",
	      [?MODULE,Pids]),
	    {error,Pids,Acc}
    end.

rcCb_timeout() ->
    case mnesia:dirty_read({appmNodeTable, rcCbTimeout }) of
	[{_,_,Timeout}] ->
	    Timeout;
	_ ->
	    ?DEF_RC_CB_TIMEOUT
    end.
		

call_restart_cold_cb(Parent,NameCxc) ->
    {ok, CxpPath, RelPath} = appmAppData:get_lm_paths(NameCxc),
    Path = filename:join([CxpPath, RelPath]),
    Tpath = swmI:find_file(Path), %use patched version if exist

    case filelib:wildcard(Tpath) of
	[Prog] ->
	    %%could be optimized to just read once except for CXP_PATH
	    Env = get_rcs_env(CxpPath),
	    Str = ["export "++K++"='"++V++"';" ||{K,V} <- Env],
	    info_msg("Calling: ~p~n", [Prog]),
	    R = os:cmd(Str ++ Prog ),
	    info_msg("Returned from ~p with result ~p~n", [Prog,R]),
	    Parent!{ok,{NameCxc,R},self()};
	Other ->
	    sysInitI:error_msg(
	      "~p:call_restart_cold_cb() failed for ~p with reason ~p ~n",
	      [?MODULE,Tpath,Other]),
	    Parent!{ok, {NameCxc,"restart_cold_cb-file_not_found"}, self()}
    end.

info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

