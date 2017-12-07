%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmServer.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/5
%%%
%%% @doc ==The software management service==
%%% This model contains software for upgrade
%%% The actions are modelled in the ECIM SwM model, and the instrumentation
%%% is implemented in the swmModel module
%%%
%%% ==File overview==
%%% SWM manages software and backup places in a lot of places
%%% ===The software archive===
%%% The archive is located at /rcs/swm/software
%%% In there the original container files are stored unaltered. The container
%%% files are organized in directories corresponding to upgrade packages.
%%% ===The active software===
%%% The active software is located at /home/$USER/software
%%% This directory contains the *-up
%%% ==Functions overview==
%%% ===Startup functions===
%%%
%%% start/0, activate/0
%%% These are general to the framework of the system startup sequence
%%%
%%% ===SWM actions===
%%%
%%% create_package/2, remove_package/1, prepare_package/1,
%%% verify_package/1, activate_package/1, commit_package/1, cancel/1
%%%
%%% These functions correspond to the actions in the ECIM SwM model
%%% and generally creates an asynchronous job executed by the
%%% swmServer process.
%%%
%%% ===Other functions===
%%%
%%% These functions are support functions which need to be exported for a reason
%%%
%%% activation_complete/0, update_progress/1, install_on_other/1,
%%% is_upgrade_ongoing/0, open_dets/2
%%%
%%% Documentation is available on the indiviual functions
%%%
%%% ==Asynchronous actions==
%%%
%%% Most sequences here is regarded as asynchronous actions, which
%%% means that they should report the result in as progress
%%% information. Because of this the swmserver may not have
%%% uncontrolled crashes which may lead to a progress not being
%%% reported. The action_handler/1 function takes an action fun and
%%% catches all crashes so that the action result is properly
%%% reported. This function can also take care of closing of
%%% connections and other clean up activities.
%%%
%%% Sequence: action_handler/1 -> handle_cancelled/0 -> cleanup/0
%%% Sequence: action_handler/1 -> handle_fail/1 -> cleanup/0
%%%
%%% ==Progress updates==
%%%
%%% In ECIM progress reporting of asynchronous actions are standardized
%%% through the ECIM common library. A common implementation for updating
%%% the #'AsyncActionProgress' record is available in swmLib.
%%% Progress data is received as a list of key-value tuples.
%%%
%%% This is done within the scope of a transaction.
%%%
%%% Sequence: update_progress/1 -> mnesia:read/1 -> swmLib:update/2 ->
%%%           mnesia:write/1
%%%
%%% The default_progress_report/2 creates a default record if the attribute
%%% is not set.
%%%
%%% ==SwM Actions==
%%% Generally the functions enabling actions begin with "handle_", so
%%% the create_package/1 call will be implemented in the swmServer as
%%% handle_create_package and so on. That's the point of entry for all modules.
%%%
%%% ===Create upgrade package===
%%%
%%% This action should download the cxs metadata file of an upgrade
%%% package, create the UpgradePackage MO and set the state to
%%% initialized. However, since we ourselves build UP containers, it
%%% can also handle that the user points to such a file instead. In
%%% that case it should go to state prepared instead.
%%%
%%% The CXS metadata file is named *-up.xml (earlier cxs*.xml)
%%% The CXP metadata file is named cxp*.xml
%%%
%%% Sequence: handle_create_package/3 -> start_channel/4 -> download_up_info/6
%%% Alt 1: download_up_info/6
%%% Alt 2: download_up_info/6 -> download_up_file/2 -> loop_download/7
%%%
%%% The alternative depends on whether there is a file named *.cxs, or
%%% we find a file with extension .xml with recognizable xml content.
%%%
%%% The loop_download function can be stopped by a cancel message. It also
%%% accepts progress update messages, so if fed with those the download
%%% progress will be updated
%%%
%%% ===Prepare upgrade package===
%%%
%%% Based on the information in the CXS metadata file it should
%%% download the CXP files and place it in the archive
%%%
%%% Sequence: handle_prepare_package/1 -> download_packages/1 ->
%%%           extract_passwd/1 -> start_channel/4 -> loop_download/7
%%%
%%% ===Verify upgrade package===
%%%
%%% This action should make verifications on the UP. Nothing is done now
%%%
%%% Sequence: handle_verify_package/1 -> ok
%%%
%%% ===Activate upgrade package===
%%%
%%% The activation is where the new software is brought to use.
%%% In order to do that there are two home directory areas, the standard
%%% /home/$USER and /upgrade/home which is the alternate area.
%%% During the activate phase the /upgrade/home should be populated with
%%% all the necessary files for starting the new software.
%%%
%%% Registered upgrade triggers should be called for both erlang applications
%%% and APPM-controlled applications.
%%%
%%% A special install script from Linux EE shall be called to install the
%%% new version of the Linux OS when necessary.
%%%
%%% A late backup is taken which forms the basis of data conversion and
%%% initialization of the new database. (Mneisa is started from scratch.)
%%% Lastly, the current and the new areas should switch places and the system
%%% shall be restarted.
%%%
%%% The action is separated into two parts. One which is executed within the
%%% scope of a transaction, and a second, post_activate, which is done only
%%% if the transaction succeeds, because it has side effects due to activation
%%% of timers.
%%%
%%% Sequence: handle_activate_package/1 -> do_activate_package/1 ->
%%%           mount_software/1 -> install_on_other/1 -> call_mw_triggers/1 ->
%%%           appmI:call_upg_triggers/1 ->
%%% Sequence alt 1: soft simulated (
%%%           post_activate/0
%%%
%%% @end
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(swmServer).
-behaviour(gen_server).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/5').
-date('2017-12-06').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB %CCaseTemplateCopyrightYear% All rights reserved.
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
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% Early history in the bottom of the file
%%% R4A/1      2015-05-26 erarafo     Adapted to change in GMF upgrade
%%% R4A/2      2015-06-11 etxjotj     ECIM SWM 4.0 "commit" action removed
%%% R4A/3      2015-06-11 etxjotj     Compiler error fix
%%% R3A/32     2015-06-04 etxjotj     Log when CXP unmount fails
%%%                                   Set flag when auditing software
%%% R3A/33     2015-06-10 etxjotj     Mount list adoptions
%%% R3A/34     2015-06-11 etxjotj     Synchronous model updates
%%% R4A/7      2015-06-17 etxjotj     HT85211 Better sftp fault printouts
%%% R4A/8      2015-06-22 etxjotj     SFTP error handling improved
%%% R4A/9      2015-06-22 erarafo     Adjustment
%%% R4A/11     2015-07-09 etxjotj     HT91333 Remove upgrade_init_* at confirm
%%% R4A/12     2015-07-20 etxjotj     HT91333 Again + some refactoring 
%%% R4A/13     2015-07-20 etxjotj     Adding consistency check
%%% R4A/14     2015-07-21 etxjotj     Dialyzer fix
%%% R4A/15     2015-07-22 etxjotj     Added root user check in software audit
%%% R4A/16     2015-07-29 etxderb     Temporary changed error_msg=>warning_msg
%%% R4A/17     2015-08-21 etxpejn     Changed logI:write_log to swmLib:write_swm_log
%%% R4A/16     2015-07-29 etxderb     New cup in, warning_msg=>error_msg.
%%% R4A/21     2015-09-04 etxjotj     HU14586 Upgrade marker
%%% R4A/22     2015-09-04 etxjotj     HU14586 Run fsck
%%% R4A/24     2015-09-08 etxjotj     Removed unwanted error printout with info
%%% R4A/25     2015-09-09 etxjotj     HU15807 Always use link solution for fb
%%% R4A/26     2015-09-09 etxjotj     HU14513 Cancel when no op is ongoing
%%% R4A/28     2015-09-15 etxjotj     HU17751 Cancel when waiting for commit
%%% R4A/29     2015-09-15 etxjotj     Fixed revision history
%%% R4A/30     2015-09-21 etxjotj     Action id handling
%%% R4A/31     2015-09-22 etxjotj     HU19769 Use init:reboot at failed upgrade
%%% R4A/32     2015-09-28 etxjotj     HU21335 All cleanup before EE confirm
%%% R4A/33     2015-10-01 erarafo     HU22829 POST_REBOOT_ACTIVATE_TIMEOUT up by 50%
%%% R4A/34     2015-10-06 etxjotj     Improved fault handling at prepare
%%% R4A/38     2015-10-16 etxjotj     HU26187 Changed takedown procedure and
%%%                                   upgrade-init handling
%%% R4A/41     2015-10-15 etxjotj     Improved fault indications
%%% R4A/42     2015-10-16 etxjotj     Bugfix in linking
%%% R4A/44     2015-10-22 etxjotj     Make autobackup at post_activate end
%%% R4A/50     2015-11-23 etxjotj     HU37957 Autobackup at confirm
%%% R5A/1      2016-01-11 etxjotj     HU47600 Improved confirm lock mechanism
%%% R5A/2      2016-01-12 etxjotj     HU49624 Cancel flag remade
%%% R5A/3      2016-02-11 etxpejn     HU53464 Handle cancel on SwM level
%%% R5A/4      2016-02-22 etxjotj     Reduce progress reporting in prepare
%%% R5A/5      2016-02-23 etxjotj     HU59680 Stop subscribing to UP events
%%% R5A/6      2016-02-24 etxjotj     Improved printout if remote sftp close
%%% R5A/7      2016-03-01 etxberb     Going live with Hw/Sw decoupling routines,
%%%                                   using swmBoardList:products/2.
%%% R5A/8      2016-03-09 etxjotj     Remade the download procedure
%%% R5A/10     2016-03-09 etxberb     Improvements of HAL functionality.
%%% R5A/11     2016-03-11 etomist     Added ActionCapability lock/unlock
%%% R5A/12     2016-03-13 erarafo     Removed doubled LTTng trace of 'confirm'
%%% R5A/13     2016-03-14 etomist     Fixed error on upgrade caused by 
%%%                                   ActionCapability lock/unlock
%%% R5A/14     2016-03-15 etomist     Added lock_action_capable_after_upgrade
%%% R5A/15     2016-03-16 etomist     unlock_action_capable moved to before post_activate
%%% R5A/16     2016-03-18 etomist     Fixed audit_software() to work with action capability lock/unlock
%%% R5A/18     2016-03-28 etxberb     * Bug fix in download_up_info/6 at
%%%                                     xmerl_scan..
%%%                                   * Added swmBoardList:read_swp_selected in
%%%                                     dir/2.
%%% R5A/16     2016-03-29 etomist     Added ActionCapability unlock when UP activation fails
%%% R5A/20     2016-04-01 etxberb     Separated global and hal in filesizes calc
%%% R5A/21     2016-04-05 etxjotj     Make sure fallback list is committed to disk
%%% R5A/22     2016-04-05 etxjotj     Temporarily backed out fix for other things
%%% R5A/23     2016-04-05 etxjotj     Fix again
%%% R5A/24     2016-04-06 etxberb     Added call to 'swmLib:mount_sda1()'.
%%% R5A/25     2016-04-14 etxberb     Return value fr read_swp_selected changed.
%% ----    ---------- -------  ------------------------------------------------
%% R6A/1   2016-04-18 etxjotj  Local cleanup of sftp data
%% R6A/2   2016-04-19 etxpejn  See if CXP_NO is set correctly in initialize 
%% R6A/3   2016-04-20 etxjotj  Create up fix
%% R6A/4   2016-04-22 etxjotj  Progress reporting after reboot updated
%% R6A/7   2016-04-25 etxjotj  Progress now 'undefined' if not used
%% R6A/8   2016-04-28 etxberb  Changed "RADIO" to "OTHER".
%% R6A/10  2016-05-24 etxtory  More aggressive sftp (needed for longer RTD)
%% R6A/11  2016-09-14 etxberb  Added bootfallback code in audit_software/0, & in
%%                             init/1 & in handle_cast({post_confirm_package..
%%                             Added get_sw_bootfallback/0.
%%                             Added audit_software in do_activate_package/1 for
%%                             bootfallback.
%% R6A/12  2016-09-15 etxberb  Added fallback_info/0.
%% R6A/13  2016-09-26 etxberb  Added dir_mount_info/0 and excluded bootfallback
%%                             files in audit_root_users/0.
%% ----    ---------- -------  ------------------------------------------------
%% R7A/1   2016-09-26 etxberb  Merge from R6A/13.
%% R7A/2   2016-10-04 etxjotj  Added support function
%% R7A/3   2016-10-07 etxjotj  Fixed test event server
%% R7A/4   2016-10-13 etxberb  Added init_config_activate.
%% R7A/5   2016-10-14 etxberb  Added confirm_package/0 & confirm_package/2.
%% R7A/6   2016-10-19 etxarnu  WP6081: Call logEsi:generate_rollback_esi in
%%                             do_fallback
%% R7A/7   2016-11-10 etxberb  Added do_activate/1 and syncr.call to Server.
%% R7A/8   2016-11-10 etxberb  Included do_activate/1 in initialize/1.
%% R7A/9   2016-11-11 etxberb  Undo of R7A/7 & R7A/8.
%% ----    ---------- -------  ------------------------------------------------
%% R8A/1   2016-10-26 etxpejn  Added signing cert functionality
%% R8A/2   2016-11-03 erarafo  Fixed compiler warning
%% R8A/3   2016-11-10 etxberb  Merge from R7A/7.
%% R8A/4   2016-11-10 etxberb  Merge from R7A/8.
%% R8A/5   2016-11-11 etxberb  Merge from R7A/9.
%% R8A/6   2016-11-11 etxberb  Moved activate/0 functionality to initialize/1.
%% R8A/7   2016-11-18 etxberb  Added audit_archive/0.
%% R8A/8   2016-11-21 etxberb  Bundle bug fixed in audit_archive/0.
%% R8A/9   2016-11-23 etxberb  Disabled audit_archive/1 for virtual RCS.
%% R8A/11  2016-12-07 etxberb  Bug fix in log_format/1.
%% R8A/12  2016-12-16 etxberb  Added add_board/3.
%% R8A/13  2016-12-29 etxberb  MR37329 HW Sensitive Install implemented.
%% R8A/14  2017-01-04 etxberb  Added sftp_ping interval timer (no action yet).
%% R8A/15  2017-01-05 etxberb  Cleaning up action_capable_locks after restart
%%                             and upgrade.
%% R8A/16  2017-01-09 etxjotj  Log when calling internal triggers
%% R8A/18  2017-01-12 etxberb  Added audit_software_directly/0.
%% R8A/19  2017-01-12 etxpejn  Added signing_cert_timer for test
%% R8A/20  2017-01-12 etxberb  Added swmInventory:update_tables in
%%                             do_audit_software.
%% R8A/21  2017-01-15 etxpejn  Added WP5618 Signed SW Certificate Revocation & Anti-rollback
%% R8A/22  2017-01-16 emarnek  Switched ssh_sftp and sysSftp to ftpI
%% R8A/23  2017-01-19 etxberb  HV56969: Introducing mutual lock bu_restore <->
%%                                      audit_software.
%% R8A/24  2017-01-24 etxberb  Moved mounting of sda1 from initialize/1 to
%%                             swmDataInit:stPh_preInit/0.
%% ----    ---------- -------  ------------------------------------------------
%% R9A/1   2017-01-27 etxjotj  Only act on add_board if hsi_state == on
%% R9A/2   2017-02-02 etxberb  Added is_finished/2.
%% R9A/3   2017-02-06 etxberb  Re-trigger audit_software after each add_board.
%% R9A/5   2017-02-09 edamkon  Handle HTTP encoded URIs; HV58153
%% R9A/6   2017-02-10 etxberb  Added format/1 format/2 is_finished/1.
%% R9A/8   2017-02-13 etxberb  Added do_clear_fallback_alarm/1.
%% R9A/9   2017-02-16 etomist  HV61068, remove pending_ug_backup in confirm
%% R9A/10  2017-02-17 etomist  Remove HV61068 (moving it to swmFallbackList)
%% R9A/11  2017-02-24 etxberb  Added get_up_archive_dir/1 & calls to swmFtp.
%% R9A/12  2017-02-26 etxberb  Disabled {swmFtp, start_watchdog, [sftp]}.
%% R9A/14  2017-02-22 estjako  Added ftpes support
%% R9A/15  2017-03-02 etxberb  Enabled {swmFtp, start_watchdog, [sftp]}.
%% R9A/17  2017-03-13 etxberb  add_board returns error when BoardType not in xml
%% R9A/18  2017-03-13 etxberb  Added post_activate_timers/1.
%% R9A/19  2017-03-21 etxberb  Moved hsi_state check to swmI.erl.
%% R9A/20  2017-03-31 etxberb  Only current UP is checked in swmFtp:verify_URIs.
%% R9A/21  2017-04-03 etxberb  Added 'handle_call(stPh_activate'.
%% R9A/23  2017-04-12 etxberb  Added 'internal_lock' in do_activate_package.
%% R9A/24  2017-04-19 etxberb  * Added an inventory update after do_add_board.
%%                             * Added add_board_retry.
%% R9A/25  2017-04-20 etxberb  Added prepPush_appdata_single.
%% R9A/28  2017-04-27 etxberb  Added swmOs:make_links in add_board handling.
%% R9A/29  2017-05-10 etxberb  Added dets table for upgradePackage.
%% R9A/30  2017-05-15 etxberb  Added bootInstance in #mountBacklog{}.
%% R9A/32  2017-05-30 etxberb  HV90906: Updating EE's cxp_list in audit_software
%% R9A/33  2017-06-01 etxberb  HV92126: Added swmFtp:verified_URIs/1.
%% ------  ---------- -------  ------------------------------------------------
%% R10A/1  2017-06-07 etxjotj  HV92792 Soaking after confirm ug
%% R10A/2  2017-06-12 etxjotj  Fixed logging issue
%% R10A/3  2017-06-12 etxjotj  Audit sw clearing home only on 'target'
%% R10A/4  2017-06-13 etxberb  Fixed edoc problem.
%% R10A/5  2017-06-20 etxarnu  Removed os:putenv (obsolete and dangerous)
%% R10A/6  2017-06-28 etxjotj  Automatic activation complete message resend
%% R10A/7  2017-07-05 etxjotj  Removed anti rollback feature
%% R10A/8  2017-07-06 etxjotj  Handle wrong version of comsaEvent case
%% R10A/9  2017-07-06 etxjotj  HW10614 reverted store_crl
%% R10A/10 2017-07-06 etxjotj  Added printout for store crl
%% R10A/11 2017-07-06 etxjojt  Rmoved call to swmOs check crl
%% R10A/12 2017-07-10 etxjotj  Remove call to swmOs:check_crl
%% R10A/13 2017-07-13 etxjotj  Printout optimization
%% R10A/14 2017-07-13 etxjotj  Added board_added/3 for test purpose
%% R10A/15 2017-07-14 etxjotj  HW11604 reduce logging
%% R10A/16 2017-07-17 etxberb  HW12476: Fix bug introduced in R10A/6. Function
%%                             post_activate_timers/1 returns tuple with 4 elems
%% R10A/17 2017-08-23 etxberb  HW20964: Added make_cxp_list in add_board.
%% R10A/18 2017-08-25 etxjotj  Added log entries
%% R10A/19 2017-08-28 etxjotj  Handle HAL case in prepare
%% ------  ---------- -------  ------------------------------------------------
%% R11A/1  2017-08-03 etxberb  * Bootfallback enabled.
%%                             * Added current_all_sw/0. Exported
%%                               software_remove/1.
%% R11A/2  2017-08-04 ekrebak  HW17334: Added error handling and printouts for 
%%                             read_remote_up_dir/3
%% R11A/3  2017-08-15 etxberb  'swmOs:clear' only on target in audit_software.
%% R11A/4  2017-08-21 etxberb  Increased gen_srv:call-timeout for
%%                             stop_soaking_timer to 300 sec.
%% R11A/5  2017-08-23 etxberb  HW20964: Added make_cxp_list in add_board.
%% R11A/9  2017-08-30 etxberb  HW23830: Added ai_ongoing case for add_board.
%% R11A/10 2017-09-04 etxberb  HW25300: Inhibit unlock of audit_sw when reboot.
%% R11A/11 2017-09-05 etxjotj  Use rcs_mode_2
%% R11A/12 2017-09-11 etxjotj  Don't do audit_software in vrcs
%% R11A/14 2017-09-14 etxjotj  Fixed rcs_mode_2 in do_confirm_package
%% R11A/15 2017-09-18 etxarnu  Removed call to missing function 
%%%                            swmBackup:delete_all_backups
%% R11A/17 2017-09-18 etxpejn  Exported has_callback
%% R11A/18 2017-10-11 etxberb  HW32389: Added validate_current_installed_LMCs,
%%                             validate_hsi_relations/0, installed_cxps/1.
%% R11A/19 2017-10-12 etxberb  vRCS fix.
%% R11A/20 2017-10-16 etxjotj  OTP20 adaptions
%% R11A/21 2017-10-17 etxpejn  Added reason permission_denied in read_remote_up_dir/3
%% ------  ---------- -------  ------------------------------------------------
%% R12A/1  2017-10-23 etxpejn  Aded anti rollback is enabled via COLI
%% R12A/2  2017-10-27 etxberb  Additions for "SP277: backup/restore of vSD/vPP"
%% R12A/3  2017-11-17 etomist  Erlang proc info improvement - avoiding warnings in SWM
%% R12A/4  2017-11-28 ecotjos  HW45226 added handle when remote server unexpectedly 
%%                             closes the connection.
%% R12A/5  2017-12-06 etxberb  Starting to use swmvUpgrade.
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%% Applications must finalize all ICTI sessions before
%% this many seconds have passed.
-define(POST_REBOOT_ACTIVATE_TIMEOUT, 900).

%% This filename is used by some module-external entity
%% (from the initial install sequence maybe?)
-define(SQUASHFS_TMPDIR_NAME, "tmp").

%% This size is defined to be compliant to most sftp servers out there
-define(CHUNK_SIZE, 32768).

%% Maximum outstanding sftp request (used in chunk_download)
-define(MAX_NUM_REQUEST, 50).

%% Timeout for chunk request
-define(SHORT_TIMEOUT, 10).     %% 10ms
-define(LONG_TIMEOUT, 120000).  %% 2min 

%% -define(UP_METADATA_FILENAME_PATTERNS, ["*-up.xml", "cxs*.xml"]).

%% Default time for graceful period in 24 hour after a revoked certificate
-define(SIGNING_CERT_TIMER_SEC, 24*60*60).

-export([start/0]).
-export([activate/0]).

%% SWM Actions
-export([create_package/2, remove_package/1, prepare_package/1,
	 verify_package/1, activate_package/1, 
	 confirm_package/0, 
	 confirm_package/1, 
	 confirm_package/2]).
%% -export([remove_sw_version/1]).
-export([remove_software_version/1]).
-export([cancel/1]).

%% Called from swmI
-export([activation_complete/0]).
-export([is_upgrade_ongoing/0]).
-export([get_new_cxp_path/1]).
-export([get_current_archive_dir/0]).
-export([add_board/3]).

%% Called from swmModel
-export([update_progress/1]).

%% Called from swmLib
-export([ets_new/2]).

%% Called from swmBackup
-export([check_up_states_for_restore/0]).
-export([stop_soaking_timer/0]).
-export([is_soaking/0]).

%% Called from swmDataInit
-export([open_dets/2]).

%% Called from swmOs
-export([get_up_archive_dir/1,
	 get_up_archive_dir/3]).

%% Called from swmv1/swmv3
-export([has_callback/2]).

%% Called from swmFallbackList
-export([current_all_sw/0, software_remove/1]).

%% For test
-export([is_waitingForCommit/0]).
-export([current_selected_sw/0]).
-export([signing_cert_timer/1]).


%% Called from swmColi
-export([get_sign_cert_timer/0]).
-export([confirm_sign_cert/0]).
-export([change_sign_cert_timer/1]).
-export([resend_activation_complete/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-export([countdown/2]).
-export([send_fallback_alarm/2, fallback/3]).
-export([prepare_progress/2]).
-export([audit_software/0,
	 audit_software_directly/0]).
-export([validate_hsi_relations/0]).
-export([file_writer/3]).

-export([mnesia_event_server_start/0,
	 mnesia_event_server_start/1,
	 mnesia_event_server_stop/0,
	 mnesia_event_server_init/1]).

-export([start_test_event_server/0, stop_test_event_server/0]).

-export([info/0]).

-export([board_added/3]).

-include("RcsSwM.hrl").
-include("RcsSwIM.hrl").
-include("alhI.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("SwmInternal.hrl").

-define(SERVER, ?MODULE).

-define(SwmLog_CurrUP, sender(swmInventory:make_currentUP_key())).

-define(Timer_AuditSw_init, 300000).
-define(Timer_AuditSw_retry, 60000).
-define(Timer_AuditSw_short, 30000).
-define(Timer_AddBoard_Retry, 60000).
-define(Timer_AddBoard_Retry_Short, 5000).

-define(MAX_CHILD_RESTARTS, 314).

-record(state, 
	{state              :: undefined|activationInProgress|waitingForCommit,
	 fallbackTimer,     % TimerRef
	 alarmTimer,        % TimerRef
	 countdownTimer,    % TimerRef
	 signCertTimer,     % TimerRef
	 resendTimer,       % TimerRef
	 auditSwTimer,      % undefined | TimerRef
	 children = [],
	 add_board_retry = [],
	 soakingTimer       % TimerRef
        }).

-type fallbackContext() :: 
	activating_before_restart | 
	data_conversion |
	waiting_for_confirm | 
	cancel.

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Starts the swmServer server process
%%% @end
%%% ----------------------------------------------------------

start() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

%%% ----------------------------------------------------------
%%% @doc Creates an upgrade package MO
%%% Downloading of the cxsNNNN.xml metadata info file or if
%%% present, a complete upgrade package .cxs file.
%%% In the first case the UP will be initialized and a prepare
%%% bust be invoked.
%%% In the second case the UP will go directly to prepare complete
%%% @end
%%% ----------------------------------------------------------
create_package(Uri, Password) ->
    case action_handler(fun() -> create_package_fun(Uri, Password) end) of
	{ok, ActionId} when is_integer(ActionId) ->
	    ActionId;
	_ ->
	    0
    end.

create_package_fun(RawUri, Password) ->
    Uri = http_uri:decode(RawUri), %Handle HTTP encoded URIs; HV58153
    case actionId_new() of
	ActionId when ActionId /= 0 ->
	    case ftpI:parse_uri(Uri) of   % Check Uri format
		{ok, {Proto, [_ | _], [_ | _], Port, [_ | _], _}}
		  when is_integer(Port) and (Proto =:= sftp orelse Proto =:= ftpes)->
		    %% ======= Normal case here! =======
		    ok = gen_server:cast(
			   swmServer,{create_package, Uri, Password, ActionId}),
		    {ok, ActionId};
		{ok, {file, _, _, _, _, _}} ->
		    %% This is for basic testing only
		    ok = gen_server:cast(
			   swmServer,{create_package, Uri, Password, ActionId}),
		    {ok, ActionId};
		{ok, {Proto, User, Host, Port, RemoteDir, _}} when Proto =:= sftp orelse Proto =:= ftpes ->
		    PortStr =
			try integer_to_list(Port)
			catch _ : _ -> make_string(Port) ++ "<-NOT AN INTEGER"
			end,
		    ParsingProblem =
			"Missing or faulty value(s):" ++
			" User=" ++ User ++
			" Host=" ++ Host ++
			" Port=" ++ PortStr ++
			" RemoteDir=" ++ RemoteDir,
		    ResultInfo = "Uri fault: " ++ ParsingProblem,
		    create_package_logFault(ActionId, Uri),
		    throw({fail, ResultInfo});
		{ok, _} ->
		    create_package_logFault(ActionId, Uri),
		    ResultInfo = "Unknown Uri format: " ++ Uri,
		    throw({fail, ResultInfo});
		{error, ParsingProblem} ->
		    ResultInfo = "Uri fault: " ++ make_string(ParsingProblem),
		    create_package_logFault(ActionId, Uri),
		    throw({fail, ResultInfo})
	    end;
	ActionId ->   % == 0
	    ResultInfo = "Too early, start phases not completed.",
	    create_package_logFault(ActionId, Uri),
	    erlang:error([{?MODULE, create_package}, ResultInfo, {uri, Uri}])
    end.

create_package_logFault(ActionId, Uri) ->
    set_default_progress_report("createUpgradePackage", ActionId),
    swmLib:write_swm_log("SwM", info, "createUpgradePackage " ++ Uri).

%%% ----------------------------------------------------------
%%% @doc Removes the upgrade package from the SWM archive
%%% But not if it is used
%%% @end
%%% ----------------------------------------------------------

remove_package(UpDn) ->
    ok = gen_server:cast(swmServer, {remove_package, UpDn}).

%%% ----------------------------------------------------------
%%% Removes a software version 
%%% Deprected in ECIM SwM 2.1
%%% 
%%% ----------------------------------------------------------

%% remove_sw_version(VsnDn) ->
%%     ok = gen_server:cast(swmServer, {remove_sw_version, VsnDn}).

%%% ----------------------------------------------------------
%%% @doc Removes a software version 
%%% Unmounts the software of the inventory SwVersion if it is still mounted
%%% @end
%%% ----------------------------------------------------------

remove_software_version(VsnDn) ->
    ok = gen_server:cast(swmServer, {remove_software_version, VsnDn}).

%%% ----------------------------------------------------------
%%% @doc Download any *.cxp file specified at the given location
%%% @end
%%% ----------------------------------------------------------

prepare_package(UpKey) ->
    %% HS87440
    case set_up_state(UpKey, prepareInProgress) of
	{ok, _} ->
	    ok = gen_server:cast(swmServer, {prepare_package, UpKey});
	{error, {wrong_state, State}} ->
	    Msg = "Prepare can not be executed in state " ++ state(State),
	    throw({fail, Msg})
    end.

%%% ----------------------------------------------------------
%%% @doc Verify a package
%%% @end
%%% ----------------------------------------------------------
verify_package(UpKey) ->
    %% HS87440
    case get_up_state(UpKey) of
	?UpgradePackageState_PREPARE_COMPLETED -> 
	    ok = gen_server:cast(swmServer, {verify_package, UpKey});
	State ->
	    Msg = "Verify can not be executed in state " ++ state(State),
	    throw({fail, Msg})
    end.


%%% ----------------------------------------------------------
%%% @doc Unpacks a package onto a the new home partition
%%% Depending on the upgrade type (which should be determined automatically
%%% depending on the contents of the upgrade package) but is now regulated
%%% manually through mnesia.
%%% @end
%%% ----------------------------------------------------------

activate_package(UpKey) ->
    %% HS87440
    case swmFailsafe:get_usage_state() of
	idle ->
	    ok;
	_ ->
	    FsMsg = "Activate can not be executed when failsafe is active",
	    throw({fail, FsMsg})
    end,   

    case set_up_state(UpKey, activationInProgress) of
	{ok, _} ->
	    sysInitI:restart_logger_trace(?MODULE, ?LINE, "upgrade: activate"),
	    ok = gen_server:cast(swmServer, {activate_package, UpKey});
        {error, {wrong_state, State}} ->
	    SMsg = "Activate can not be executed in state " ++ state(State),
	    throw({fail, SMsg});
	{error, Reason} ->
	    throw({fail, Reason}) 
    end.


%%% ----------------------------------------------------------
%%% doc Confirm the upgrade once the activation is finished
%%% This method retains the old non ECIM-compliant asynchronous handling
%%% end
%%% ----------------------------------------------------------
%% commit_package(UpKey) ->
%%     sysInitI:restart_logger_trace(?MODULE, ?LINE, "upgrade: confirm"),
%%     ok = gen_server:cast(swmServer, {commit_package, UpKey}).

%%% ----------------------------------------------------------
%%% @doc Confirm the upgrade once the activation is finished
%%% This method implements the ECIM-compliant synchronous handling
%%% @end
%%% ----------------------------------------------------------
confirm_package() ->
    confirm_package(swmInventory:make_currentUP_key()).

%%% ----------------------------------------------------------
confirm_package(UpKey) ->
    %% HU47600 Improved locking mechanism
    case set_confirm_lock() of
	go ->
	    try do_confirm_package(UpKey)
	    catch throw:{fail, Reason} ->
		    throw({fail, Reason});
		  Type:Reason ->
		    sysInitI:error_report(
		      [{Type, Reason}, {stacktrace, erlang:get_stacktrace()}]),
		    erlang:Type(Reason)
	    after
		release_confirm_lock()
	    end;
	busy ->
	    throw({fail, "Confirm is already ongoing"})
    end.

%%% ----------------------------------------------------------
confirm_package(ProductNo, ProductRev) ->
    confirm_package(swmInventory:make_UP_key(ProductNo, ProductRev)).

%% HU47600 Improved locking mechanism
%% RAM table is chosen because we want this lock reset at reboot anyway
set_confirm_lock() ->
    Fun = fun() ->
		  case swmLib:get_ram_variable(confirm_lock) of
		      busy -> 
			  busy;
		      _ ->
			  swmLib:set_ram_variable(confirm_lock, busy),
			  go
		  end
	  end,
    case mnesia:transaction(Fun) of
	{atomic, busy} ->
	    busy;
	{atomic, go} ->
	    go;
	{aborted, Reason} ->
	    erlang:error({aborted, Reason}, [])
    end.

release_confirm_lock() ->
    swmLib:erase_ram_variable(confirm_lock).



%% This part is exceuted in the action calling process
do_confirm_package(UpKey) ->
    Sender = sender(UpKey),
    sysInitI:restart_logger_trace(?MODULE, ?LINE, "upgrade: confirm"),
    swmLib:write_swm_log(Sender, info, "confirm"),
    [Obj] = mnesia:dirty_read({upgradePackage, UpKey}),
    case Obj#upgradePackage.state of
	?UpgradePackageState_WAITING_FOR_COMMIT ->
	    %% HS87470
	    swmLib:erase_ram_variable(activationStarted),
	    %% HS67532, part 2 of 2
	    %% Handle poweroff during commit
	    HeartPath = filename:join(sysEnv:home_dir(), "heart_fallback"),
	    CFR = filename:join(sysEnv:home_dir(), "confirm_failed_rollback"),
	    case {file:copy(HeartPath, CFR), sysEnv:rcs_mode_2()} of
		{{ok, _}, target} -> 
		    cmd(["chmod a+x ", CFR, " ; sync"]),
		    ok;
		{{error, enoent}, simulated} -> ok;
		{{error, Reason}, target} ->
		    erlang:error(Reason, [UpKey]);
		{_, vrcs} ->
		    ok
	    end,
	    NewObj = Obj#upgradePackage{reportProgress = undefined},
	    mnesia:dirty_write(NewObj),
	    %% HS34720
	    send_command(UpKey, confirm, "", confirm_failed, Sender),
	    %% HS34720 end
	    ok = gen_server:call(?SERVER, {confirm_package, UpKey}, 300000),
	    swmLib:erase_variable(activating_up),
	    FinalObj = NewObj#upgradePackage{state = state(commitCompleted)},
	    mnesia:dirty_write(FinalObj),
	    ok = gen_server:cast(
		   swmServer, {post_confirm_package,element(4,UpKey)}),
	    file:delete(CFR), % HS67532
	    set_soaking(),
	    gen_server:cast(?SERVER, check_and_start_soaking_timer),
	    swmLib:write_swm_log(Sender, info, "confirm complete"),
	    ok;
	State ->
	    throw({fail, 
		   "Confirm can not be executed in state " ++ state(State)})
    end.


%%% ----------------------------------------------------------
%% @doc cancel download or activation.
%%% Although you can issue the command elsewhere it will not have any effect.
%%% @end
%%% ----------------------------------------------------------
cancel(UpKey) ->
    Fun = fun() -> reboot_if_cancel(UpKey) end,
    Sender = sender(UpKey),
    case mnesia:transaction(Fun) of
	{atomic, reboot} ->
	    info_msg("Activate cancellation ordered. Rebooting...~n"),
	    swmLib:write_swm_log(Sender, critical, 
				 "Activate cancellation ordered. Rebooting..."),
	    swmLib:sync(),
	    swmLib:restart_node(cold, ?ALH_TAG_UpgradeCancelled);
	{atomic, no_reboot} ->
	    info_msg("Activate cancellation ordered.~n"),
	    swmLib:write_swm_log(Sender, info, 
				 "Activate cancellation ordered."),
	    swmServer!{cancel, UpKey};
	{atomic, not_ongoing} -> %HU14513
	    swmLib:write_swm_log(Sender, info, 
				 "Cancel ordered. No ongoing action."),
	    ok;
	{aborted, Reason} ->
	    erlang:error({aborted, Reason}, [UpKey])
    end.

reboot_if_cancel({"1","1","1"}) ->
    %% TODO Find out if any action is ongoing on any UP
    not_ongoing;
reboot_if_cancel(UpKey) ->
    [UP] =  mnesia:read({upgradePackage, UpKey}),
    Progress = UP#upgradePackage.reportProgress,
    %% HU14513 Handle cancel when no ongoing action
    case Progress of 
	undefined -> 
	    not_ongoing;
	Progress ->
	    #'AsyncActionProgressWithSteps'{actionName = ActionName,
					    state = State}  = Progress,
	    case {ActionName, State} of
		{"activate", ?ActionStateType_RUNNING} ->
		    do_update_progress(
		      UpKey, [{state, ?ActionStateType_CANCELLING}]),
		    %% HU49624 Cancel flag fix
		    case swmLib:get_ram_variable(activate_cancel) of
			reboot -> reboot;
			_ -> no_reboot
		    end;
		%% HU17751
		{_, ?ActionStateType_FINISHED} 
		  when UP#upgradePackage.state == 
		       ?UpgradePackageState_WAITING_FOR_COMMIT ->
		    reboot;
		{_, ?ActionStateType_FINISHED} ->
		    not_ongoing;
		{_, ?ActionStateType_CANCELLED} ->
		    not_ongoing;
		_ -> 
		    no_reboot
	    end
    end.

%%% ----------------------------------------------------------
%%% @doc Automatically called after the activation
%%% At the end of the activation this function must be called to
%%% complete the upgrade
%%% This function is called by gmfImmUgMaster when the data conversion phase
%%% is complete
%%% @end
%%% ----------------------------------------------------------

activation_complete() ->
    info_msg("Got activation_complete~n"),
    sysInitI:restart_logger_trace(?MODULE, ?LINE, "upgrade: activate complete"),
    ok = gen_server:cast(swmServer, activation_complete).

%%% ----------------------------------------------------------
%%% @doc Called after complete startup
%%% This function is called by sysApp after a complete startup.
%%% @end
%%% ----------------------------------------------------------

activate() ->
    ok = gen_server:call(?SERVER, stPh_activate, 300000).

%%% ----------------------------------------------------------
%%% Description: This function is entered at system startup if it is
%%%              detected that the rollback backup has been loaded
%%% ----------------------------------------------------------

report_on_reboot(BuName) ->
    info_msg("Detecting restore of rollback backup ~p~n",[BuName]),
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    case swmLib:get_ram_variable(bu_progress_report_exists) of
	true ->
	    %% This means there was a deliberate restore ordered
	    %% No need to update UP progress, only BrM progress
	    clear_all_progress_info();
	false -> 
	    %% This means any ongoing upgrade has failed
	    %% Restore the links and set boot counter to 0
	    unlock_action_capable_after_restart(),
	    case sysEnv:board_type() of
		tcu03 ->
		    sysRhai:setbootcounter(0),
		    Other = case swmOs:get_active_instance() of
				1 -> "2";
				2 -> "1"
			    end,
		    %% swmOs:run_swm_wrapper("activate_ee_tcu03 -i "++Other);
		    swmOs:activate_os(Other);
		_ ->
		    ok
	    end,

	    [SwM] = mnesia:dirty_read({swM, {"1","1","1"}}),
	    report_failure_restart(SwM#swM.reportProgress, CompleteTime),
	    [report_failure_restart(Up#upgradePackage.upgradePackageId,
				    Up#upgradePackage.reportProgress,
				    CompleteTime)||
		Up<-ets:tab2list(upgradePackage)];
	undefined ->
	    %% Can happen if there is a power cycle immediately after 
	    %% activate, and perhaps other cases
	    unlock_action_capable_after_restart(),
	    clear_all_progress_info()
    end,
    swmLib:erase_ram_variable(bu_progress_report_exists).

clear_all_progress_info() ->
    case mnesia:is_transaction() of
	true ->
	    do_clear_all_progress_info();
	false ->
	    mnesia:transaction(fun do_clear_all_progress_info/0)
    end,
    ok.

do_clear_all_progress_info() ->
    [SwM] = mnesia:read({swM, {"1","1","1"}}),
    %% AAP = #'AsyncActionProgress'
    %% 	{actionName="",
    %% 	 additionalInfo=[""],
    %% 	 progressInfo = "Default message",
    %% 	 progressPercentage=100,
    %% 	 result=?ActionResultType_NOT_AVAILABLE,
    %% 	 resultInfo="",
    %% 	 state=?ActionStateType_FINISHED,
    %% 	 actionId=0,
    %% 	 timeActionStarted = "",
    %% 	 timeActionCompleted= "",
    %% 	 timeOfLastStatusUpdate= comsaI:iso_time(os:timestamp(), extended)},
    AAP = AAPwS = undefined, 
    mnesia:write(SwM#swM{reportProgress=AAP}),
    %% AAPwS = #'AsyncActionProgressWithSteps'
    %% 	{actionName="",
    %% 	 additionalInfo=[""],
    %% 	 progressInfo = "Default message",
    %% 	 progressPercentage=100,
    %% 	 result=?ActionResultType_NOT_AVAILABLE,
    %% 	 resultInfo="",
    %% 	 state=?ActionStateType_FINISHED,
    %% 	 actionId=0,
    %% 	 timeActionStarted = "",
    %% 	 timeActionCompleted= "",
    %% 	 timeOfLastStatusUpdate= comsaI:iso_time(os:timestamp(), extended),
    %% 	 step=-1,
    %% 	 stepProgressPercentage=100},
    [mnesia:write(UP#upgradePackage{reportProgress=AAPwS})||
	UP<-ets:tab2list(upgradePackage)].




report_failure_restart(undefined, _) ->
    ok;
report_failure_restart(Progress, CompleteTime) 
  when is_record(Progress, 'AsyncActionProgress')->
    case Progress#'AsyncActionProgress'.state of
	?ActionStateType_RUNNING ->
	    swmFallbackList:cancel_pending(), %HS51856
	    Msg = "The action was interrupted by a restart",
	    Info = "More info may be available in SwmLog",
	    update_progress([{result, ?ActionResultType_FAILURE},
			     {additionalInfo, Info},
			     {progressPercentage, 100},
			     {resultInfo, Msg},
			     {state, ?ActionStateType_FINISHED},
			     {timeActionCompleted, CompleteTime}]);
	_ ->
	    ok
    end.

report_failure_restart(_, undefined, _) ->
    ok;
report_failure_restart(Key, Progress, CompleteTime) 
  when is_record(Progress, 'AsyncActionProgressWithSteps')->
    case Progress#'AsyncActionProgressWithSteps'.state of
	?ActionStateType_RUNNING ->
	    swmFallbackList:cancel_pending(), %HS51856
	    Msg = "The action was interrupted by a restart",
	    update_progress(Key, [{result, ?ActionResultType_FAILURE},
				  {additionalInfo, Msg},
				  {progressPercentage, 100},
				  {resultInfo, Msg},
				  {state, ?ActionStateType_FINISHED},
				  {timeActionCompleted, CompleteTime}]);
	_ ->
	    ok
    end.

%%% ----------------------------------------------------------
%%% @doc Called after complete startup
%%% This function is called by sysApp after a complete startup.
%%% @end
%%% ----------------------------------------------------------

is_upgrade_ongoing() ->
    filelib:is_file(filename:join(sysEnv:home_dir(), "upgrade_init")).

%%% ===========================================================================
is_waitingForCommit() ->
    try gen_server:call(?SERVER, get_state) of
	waitingForCommit ->
	    true;
	_ ->
	    false
    catch
	_ : _ ->
	    false
    end.

%%% ----------------------------------------------------------
%%% @doc Audit software
%%% Unmounts all software that is not part of the running UP
%%% @end
%%% ----------------------------------------------------------

audit_software_directly() ->
    audit_software_directly(sysEnv:rcs_mode_2()).

audit_software_directly(vrcs) ->
    info_msg("No audit_software_directly in vrcs~n");
audit_software_directly(Env) when Env == target; Env == simulated ->
    Self = self(),
    case whereis(?SERVER) of
	Self ->
	    do_audit_software(#state{});
	_ ->
	    ok = gen_server:call(
		   ?SERVER, audit_software_directly, 300000)
    end.


audit_software() ->
    case sysEnv:rcs_mode_2() of
	vrcs -> info_msg("No audit_software in vrcs~n");
	target -> 
	    ok = gen_server:cast(?SERVER, audit_software);
	simulated -> 
	    ok = gen_server:cast(?SERVER, audit_software)
    end.

audit_software(State) ->
    audit_software(State, ?Timer_AuditSw_init).

audit_software(#state{auditSwTimer = TRef} = State, TimerValue) ->
    timer:cancel(TRef),
    State#state{auditSwTimer = audit_software_SendAfter(TimerValue)}.

audit_software_SendAfter(TiVal) ->
    sysInitI:info_report([sysUtil:term_to_string(TiVal div 1000) ++ " seconds"]),
    {ok, TRef} = timer:send_after(TiVal, audit_software_timeout),
    TRef.

do_audit_software(State) ->
    case is_soaking() of
	false ->
	    case swmLib:lock_action_capable(?AUDIT_SW_ACTION_CAPABLE_ID, 
					    ?AUDIT_SW_ACTION_CAPABLE_INFO) of
		ok ->
		    set_internal_lock_and_run_audit(State);
		_ ->
		    %% Silently fails to execute. Retry later...
		    TimerRef = audit_software_SendAfter(?Timer_AuditSw_retry),
		    State#state{auditSwTimer = TimerRef}
	    end;
	true ->
	    info_msg("Software audit deferred while soaking after upgrade~n",[])
    end.

set_internal_lock_and_run_audit(State) ->
    try
	swmLib:internal_lock([?LOCKOBJ_inst_bootfb,?LOCKOBJ_bu_restore],
			     audit_software),
	erlang:yield(),
	DoIt = {not swmLib:is_internal_lock(?LOCKOBJ_audit_sw),
		not is_upgrade_ongoing()},
	do_audit_software(DoIt, State)
    catch
	ErrClass : ErrReason ->
	    Stacktrace = erlang:get_stacktrace(),
	    sysInitI:error_report([{ErrClass, ErrReason},
		      {stacktrace, Stacktrace}
		      | ?STATE_INFO(State) ++ ?PROC_INFO(self())]),
	    State#state{auditSwTimer =
			    audit_software_SendAfter(?Timer_AuditSw_retry)}
    after
	begin
	    swmLib:internal_lock_remove([?LOCKOBJ_inst_bootfb,
					 ?LOCKOBJ_bu_restore],
					audit_software),
	    ok = swmLib:unlock_action_capable(
		   ?AUDIT_SW_ACTION_CAPABLE_ID),
	    swmInventory:update_tables(no_log) % HW11604
	end
    end.


do_audit_software({true, true}, State) ->
    swmOs:homes(mount),
    %% Clear the other home area in order to prevent the soaking algorithm
    %% to use it without a proper UP install
    %% This may be reverted when the bootfallback solution is activated, but
    %% then the bootfallback solution should handle also soaking.
    case swmFallbackList:is_bootfallback_complete() of
	true ->
	    ok;
	false ->
	    case sysEnv:rcs_mode_2() of
		target ->
		    swmOs:clear();
		_ ->
		    ok
	    end
    end,
    Installed = swmOs:get_installed_cxps(),
    Software = current_selected_sw(),
    Remove = (Installed -- Software) -- get_sw_bootfallback(),
    info_msg("Auditing software~nThe following will be removed: ~p~n",[Remove]),
    swmLib:dir_mount_info(),
    swmLib:make_cxp_list(sysEnv:home_dir(), Software),
    software_remove(Remove),
    info_msg("Software audit complete~n"),
    swmLib:write_swm_log("SwM", info, "Software audit complete"),
    audit_archive(sysEnv:rcs_mode_2()),
    swmOs:homes(umount),
    State#state{auditSwTimer = undefined};
do_audit_software(_, State) ->
    info_msg("Audit software inhibited by internal lock:~n~p~n",
	     [swmLib:internal_lock_who(?LOCKOBJ_audit_sw)]),
    State#state{auditSwTimer = audit_software_SendAfter(?Timer_AuditSw_retry)}.

%%% ###########################################################################
%%% current_all_sw
%%% 
%%% ###=====================================================================###
current_all_sw() ->
    current_all_sw(sysEnv:rcs_mode_2()).

current_all_sw(vrcs) ->
    {ok, Software} = file:list_dir(swmLib:software_dir()),
    Software;
current_all_sw(_) ->
    current_sw(all).

%%% ###########################################################################
%%% current_selected_sw
%%% 
%%% ###=====================================================================###
current_selected_sw() ->
    current_selected_sw(sysEnv:rcs_mode_2()).

current_selected_sw(vrcs) ->
    {ok, Software} = file:list_dir(swmLib:software_dir()),
    Software;
current_selected_sw(_) ->
    current_sw(swmBoardList:boardTypes_oth()).

%%% ###=====================================================================###
current_sw(BoardTypesOTH) ->
    SoftwareDir = swmLib:software_dir(),
    {ok, [{products, Selected}]} =
	swmBoardList:swp([products],
			 [{boardTypeBB, swmBoardList:boardType()},
			  {boardTypesOTH, BoardTypesOTH},
			  {options, [{global, SoftwareDir}]}]),
    Products =
	Selected ++
	swmBoardList:bundled_products(swmI:get_current_archive_dir(), Selected),
    [swmBoardList:swp_id_format(SWP) || {SWP, _} <- Products].

%%% ----------------------------------------------------------
audit_archive(Target) when Target /= vrcs ->
    ArchiveDir = get_current_archive_dir(),
    {ok, [{products, Selected}]} =
	swmBoardList:swp([products],
			 [{boardTypeBB, swmBoardList:boardType()},
			  {boardTypesOTH, swmBoardList:boardTypes_oth()},
			  {options, [{global, ArchiveDir}]}]),
    UnpackedBundled = swmBoardList:bundled_products(ArchiveDir, Selected),
    AllSelected = Selected ++ UnpackedBundled,
    SelectedFileNames = [FileName || {_, {global, FileName}} <- AllSelected],
    ArchiveFileNames = swmBoardList:archiveCXPs(ArchiveDir),
    Remove = ArchiveFileNames -- SelectedFileNames,
    Info1 = ["Auditing archive",
	     {dir, ArchiveDir},
	     "--- Obsolete files ---"
	     | files_info(Remove, ArchiveDir, [size, mtime])],
    log_SwmInternal(info, Info1),
    sysInitI:info_report(Info1),
    Result =
	[{FN, file:delete(filename:join(ArchiveDir, FN))} || FN <- Remove],
    Info2 = ["Archive audit complete"] ++
	case [FN || {FN, ok} <- Result] of
	    [] ->
		["--- No files removed ---"];
	    RemovedFiles ->
		["--- Removed files ---" | RemovedFiles]
	end,
    log_SwmInternal(info, Info2),
    sysInitI:info_report(Info2),
    audit_archive_failed(Result, ArchiveDir);
audit_archive(_) ->
    ok.

%%% ----------------------------------------------------------
files_info(FileNames, Dir, Opts) ->
    [{FN, file_info(filename:join(Dir, FN), Opts)} || FN <- FileNames].

%%% ----------------------------------------------------------
file_info(File, Opts) ->
    try
	{ok, FileInfo} = file:read_file_info(File),
	[{K,V}||{K,V}<-
		    lists:zip(record_info(fields, file_info), 
			      tl(tuple_to_list(FileInfo))),
		Opts == all orelse lists:member(K, Opts)]
			      
    catch
	ErrClass : ErrReason ->
	    [{ErrClass, ErrReason}]
    end.


%%% ----------------------------------------------------------
audit_archive_failed([{_, ok} | Tail], ArchiveDir) ->
    audit_archive_failed(Tail, ArchiveDir);
audit_archive_failed([{FileName, Reason} | Tail], ArchiveDir) ->
    Heading = "Archive audit failed for " ++ FileName,
    Info = [Heading,
	    {dir, ArchiveDir},
	    {reason, Reason},
	    "--- File info ---"
	    | file_info(filename:join(ArchiveDir, FileName), all)],
    log_SwmInternal(warning, Info),
    sysInitI:warning_report(Info),
    swmLib:write_swm_log("SwM", alert, Heading),
    audit_archive_failed(Tail, ArchiveDir);
audit_archive_failed([], _) ->
    ok.

%%% ----------------------------------------------------------
log_SwmInternal(Severity, Info) ->
    try
	logI:write_log("SwmInternal", "swmServer", Severity, log_format(Info))
    catch
	ErrClass : ErrReason ->
	    sysInitI:warning_report(["Logging failed",
				     {Severity, Info},
				     {ErrClass, ErrReason},
				     {stacktrace, erlang:get_stacktrace()}])
    end.

%%% ----------------------------------------------------------
log_format(Term) ->
    try
	io_lib:format("~s", [Term]),
	remove_last_nl(Term)
    catch
	_ : _ ->
	    remove_last_nl(lists:flatten(log_format_list(Term)))
    end.
%% log_format(Term) ->
%%     sysUtil:term_to_string(Term).

log_format_list([{T1, T2} | Tail]) ->
    ["    " ++
     sysUtil:term_to_string(T1) ++ ": " ++ sysUtil:term_to_string(T2) ++
     "\n"
     | log_format_list(Tail)];
log_format_list([Term | Tail]) ->
    ["    " ++
     sysUtil:term_to_string(Term) ++
     "\n"
     | log_format_list(Tail)];
log_format_list([]) ->
    [].

%%% ----------------------------------------------------------
remove_last_nl([]) ->
    [];
remove_last_nl(Str) ->
    case lists:last(Str) of
	$\n ->
	    remove_last_nl(lists:droplast(Str));
	_ ->
	    Str
    end.

%%% ----------------------------------------------------------
get_sw_bootfallback() ->
    swmFallbackList:bootfallback_sw().

%%% ----------------------------------------------------------
software_remove([Cxp | Tail]) ->
    case swmOs:remove_cxp(Cxp) of
	ok ->
	    Dir = filename:join(swmLib:software_dir(), Cxp),
	    case filelib:is_dir(Dir) of
		true -> 
		    cmd(["rm ", Dir]);
		false ->
		    ok
	    end;
	error ->
	    swmLib:write_swm_log("SwM", alert, 
				 "Software audit failed for " ++ Cxp)
    end,
    software_remove(Tail);
software_remove([]) ->
    audit_root_users().

%%% ----------------------------------------------------------
%% Addition to pinpoint problems reported in HT94058
audit_root_users() ->
    {ok, Files} = file:list_dir(swmLib:squash_fs_dir()),
    audit_root_users(Files -- get_sw_bootfallback()).

audit_root_users([File|Files]) ->
    SquashFsDir = swmLib:squash_fs_dir(),
    Path = filename:join(SquashFsDir, File),
    case file:read_file_info(Path) of
	{ok, #file_info{uid = 0}} ->
	    warning_msg("Owner root on at least one item in /software~n~s~n",
			[os:cmd(["ls -l ", SquashFsDir])]),
	    swmLib:write_swm_log("SwM",
				 warning,
				 "Software audit concern for " ++ File);
	_ ->
	    audit_root_users(Files)
    end;
audit_root_users([]) ->
    ok.

%%% ----------------------------------------------------------
%%% @doc File writer for upgrade
%%% All file writes are executed in own process to speed up
%%% the download time.
%%% @end
%%% ----------------------------------------------------------

file_writer(UpKey, LocalPath, Pid) ->
    case file:open(LocalPath, [write, binary, raw]) of
	{ok, Fd} ->
	    file_writer_loop(UpKey, Fd, Pid, _WriteTime = 0);
	{error, Reason} ->
	    Pid ! {file_error, {error, Reason}}
    end.

file_writer_loop(UpKey, Fd, Pid, WriteTime) ->
    receive 
	{file_data, Data} ->
	    case file_write(Fd, Data) of
		{Time, ok} ->
		    mnesia:dirty_update_counter(swmRamVariables,
						download_progress,
						size(Data)),
		    file_writer_loop(UpKey, Fd, Pid, Time + WriteTime);
		{_, {error, enospc}} ->
		    FReason = file:format_error(enospc),
		    Pid ! {file_error, {fail, FReason}};
		{_, {error, Reason}}  ->
		    FReason = file:format_error(Reason),
		    update_progress(UpKey, [{additionalInfo, FReason}]),
		    Pid ! {file_error, {error, Reason}}
	    end;
	{file_close, Pid} ->
	    file:close(Fd),
	    Pid ! {file_closed, WriteTime}
    end.

file_write(Fd, Data) ->
    timer:tc(fun() -> file:write(Fd, Data) end).

resend_activation_complete() ->
    %% Find UP in WAITING_FOR_CONFIRM
    WFC = ?UpgradePackageState_WAITING_FOR_COMMIT,
    Fun = fun() -> mnesia:match_object(#upgradePackage{state=WFC, _='_'}) end,
    case mnesia:transaction(Fun) of
	{atomic, []} ->
	    {error, "No UpgradePackage is in state WAITING_FOR_COMMIT"};
	{atomic, UPs} ->
	    {ok, 
	     [begin
		 Type = avc,
		  UpKey = UP#upgradePackage.upgradePackageId,
		  Changed = [{state, 'RcsSwM.UpgradePackageState', 
			      UP#upgradePackage.state},
			     {reportProgress, {struct,'AsyncActionProgressWithSteps'},
			      UP#upgradePackage.reportProgress}],
		  Model = "RcsSwM",
		  Class = "UpgradePackage",
		  try
		      comsaEvent:generate_event(Type, UpKey, Changed, Model, Class)
		  catch error:undef ->
			  warning_msg("Wrong version of comsaEvent~n")
		  end,
		  UpKey
	      end||UP<-UPs]};

	{aborted, Reason} ->
	    error_msg("UP match: ~p~n",[{aborted, Reason}]),
	    {error, "A software related error occured"}
    end.
			       

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
init(Args) ->
    T1 = ?MonoTime,
    swmLib:internal_lock_init(),
    ok = gen_server:cast(self(), {initialize, Args}),
    T2 = ?MonoTime,
    mnesia_event_server_start(),
    T_end = ?MonoTime,
    sysInitI:info_report([{internal_lock_init, sysUtil:time_to_string(T2 - T1)},
	       {mnesia_event_server_start, sysUtil:time_to_string(T_end - T2)},
	       {total, sysUtil:time_to_string(T_end - T1)}]),
    {ok, initializing}.

%% #############################################################################
%% initialize
%%
%% ###=======================================================================###
initialize(_Args) ->
    T1 = ?MonoTime,
    SoakingTimer = case check_and_start_soaking_timer() of
		       {ok, TRef} -> 
			   info_msg("The soaking timer has been started~n"),
			   TRef;
		       soaking_never_ends ->
			   info_msg("Open ended soaking ongoing~n"),
			   undefined;
		       no_soaking ->
			   info_msg("No soaking is ongoing~n"),
			   undefined
		   end,
    case sysEnv:rcs_mode_2() of
	vrcs ->
	    ok;
	_ ->
	    ok = swmDiskSrv:request(#{mfaFun => {?MODULE,
						 validate_hsi_relations,
						 []},
				      reply => false})
    end,
    T2 = ?MonoTime,
    case is_upgrade_ongoing() of
	true ->
	    ok;
	false ->
	    swmFallbackList:audit_bootfallback()
    end,
    T3 = ?MonoTime,
    actionId_init(),
    
    mnesia:subscribe({table, swVersion, detailed}),

    cmd(["df ",sysEnv:home_dir()]),
    info_msg("Active instance is ~w~n",[swmOs:get_active_instance()]),

    DataDir = swmLib:data_dir(),
    SwFile = filename:join(DataDir, "swdata.dets"),
    SwFile2 = filename:join(DataDir, "upgradePackage.dets"),
    filelib:ensure_dir(SwFile),

    open_dets(swVersion, [{file, SwFile}, {keypos, 2}]),
    open_dets(upgradePackage, [{file, SwFile2}, {keypos, 2}]),

    T4 = ?MonoTime,
    swmModel:update_tables(),
    swmInventory:update_tables(log), %HW11604
    T5 = ?MonoTime,

    init_config_activate(swmvBackup:init_config_activate()),
    T6 = ?MonoTime,

    %% Check if a signing cerificate is about to be revoked
    [UpVersion] = swmInventory:get_current_sw_version(),
    UpStr = 
    	UpVersion#'ProductData'.productNumber++"-"++
    	UpVersion#'ProductData'.productRevision,
    File = filename:join(sysEnv:home_dir(), "swm/SignCertTimer"),
    SignCertTimer =
    	case file:consult(File) of
    	    {ok, [{Term, UpStr}]} ->
    		%% Calculate the time left and update TimerRef
    		CurrentTime = calendar:datetime_to_gregorian_seconds(
    				calendar:universal_time()),
    		ExpirationTime = calendar:datetime_to_gregorian_seconds(Term),
    		case ExpirationTime - CurrentTime of
    		    Diff when Diff > 0 ->
    			%% The expiration time is still in the future, 
    			%% set capable to locked & start a new timer
    			swmLib:lock_action_capable(
			  ?SIGNING_CERTIFICATE_ACTION_CAPABLE_ID,
			  ?SIGNING_CERTIFICATE_ACTION_CAPABLE_INFO),
    			erlang:start_timer(Diff*1000, self(), sign_cert_timer_exipred);
    		    _Else ->
    			%% The expiration time has already happen,
    			%% store the new certificate in EE
			warning_msg("The timeout has already expired, store CRL~n",[]),
			info_msg("Updating CRL: ~n~s~n",
				 [swmOs:get_crl_ids(sysEnv:rcs_mode_2())]),
			case swmLib:get_variable(anti_rollback) of
			    on ->
				swmOs:store_crl(sysEnv:rcs_mode_2());
			    _Else ->
				%% Disabled due to HW10614
				do_nada
			end,
			file:delete(File),
    			undefined
    		end;
    	    _Error ->
    		%% No file exists or we have done a rollback to another UP,
    		%% no timer should be started
    		file:delete(File),
    		undefined
    	end,
  
    %% HS67532, part 1 of 2
    CFR = filename:join(sysEnv:home_dir(), "confirm_failed_rollback"),
    case filelib:is_file(CFR) of
	true -> 
	    error_msg("Confirm failed. Rolling back.~n"),
	    swmLib:write_swm_log("SwM",info,"Confirm failed. Rolling back"),
	    cmd(CFR);
	false -> 
	    ok
    end,
    %% HS67532 fix ends here
    T7 = ?MonoTime,
    try
	InitState = initialize_setState(swmLib:get_variable(bu_restore)),
	State = children_start(init_children(), InitState),
	initialize_ugFailed(swmLib:get_variable(ug_failed)),
	T_end = ?MonoTime,
	sysInitI:info_report([{soaking_and_validateHSI, sysUtil:time_to_string(T2 - T1)},
		   {audit_bootfallback, sysUtil:time_to_string(T3 - T2)},
		   {misc, sysUtil:time_to_string(T4 - T3)},
		   {update_tables, sysUtil:time_to_string(T5 - T4)},
		   {init_config_activate, sysUtil:time_to_string(T6 - T5)},
		   {misc, sysUtil:time_to_string(T7 - T6)},
		   {setState, sysUtil:time_to_string(T_end - T7)},
		   {total, sysUtil:time_to_string(T_end - T1)}]),
	{noreply, State#state{signCertTimer = SignCertTimer,
			      soakingTimer = SoakingTimer}}
    catch
	throw : {Reason, ErrState} ->
	    T_error = ?MonoTime,
	    sysInitI:info_report([{audit_bootfallback, sysUtil:time_to_string(T3 - T1)},
		       {misc, sysUtil:time_to_string(T4 - T3)},
		       {update_tables, sysUtil:time_to_string(T5 - T4)},
		       {init_config_activate, sysUtil:time_to_string(T6 - T5)},
		       {misc, sysUtil:time_to_string(T7 - T6)},
		       {setState, sysUtil:time_to_string(T_error - T7)},
		       {total, sysUtil:time_to_string(T_error - T1)}]),
	    {stop, Reason, ErrState}
    end.

%%% #---------------------------------------------------------
init_children() ->
    case sysEnv:rcs_mode_2() of
 	vrcs ->
 	    [];
 	_ ->
 	    StartCnt = 1,
 	    [{{swmFtp, start_watchdog, [sftp]}, StartCnt}]
    end.

%%% #---------------------------------------------------------
initialize_setState(undefined = _bu_restore) ->
    info_msg("This is not a backup fallback restart~n"),
    case is_upgrade_ongoing() of
	true ->
	    info_msg("upgrade is ongoing~n"),
	    UP = swmLib:get_variable(activating_up),
	    Key = UP#upgradePackage.upgradePackageId,
	    swmvUpgrade:activate(#{upKey => Key}),
	    update_progress(Key, [{additionalInfo, "Converting database."},
				  {progressPercentage, 76}]),
	    
	    %% This is to cause a fallback before data conversions
	    %% are ready if that task hangs
	    ok = gen_server:cast(swmServer, set_global_fallback),
	    
	    %% If there is a heart_fallback file, then this is
	    %% a reboot upgrade. 
	    Heart = filename:join(sysEnv:home_dir(), "heart_fallback"),
	    case filelib:is_file(Heart) of
		true ->
		    info_msg("Setting heart fallback cmd~n"),
		    heart:set_cmd(Heart),
		    %% HS87470
		    swmLib:set_ram_variable(activationStarted, Key);
		false ->
		    ok
	    end,
	    #state{state = activationInProgress};
	false ->
	    info_msg("upgrade is not ongoing~n"),
	    %% HU29568 upgrade marker removed
	    audit_software(),
	    audit_progress_state(),%% HS27033
	    swmLib:erase_ram_variable(activationStarted),%%HT18288
	    #state{}
    end;
initialize_setState(BuRestore) ->
    info_msg("bu_restore = ~p~n",[BuRestore]),
    audit_software(),
    swmLib:set_ram_variable(bu_progress_report_exists, 
			    swmBackup:is_progress_report()),
    swmLib:erase_ram_variable(activationStarted),
    Heart = filename:join(sysEnv:home_dir(), "heart_fallback"),
    case filelib:is_file(Heart) of
	true -> 
	    file:delete(Heart);
	false ->
	    ok
    end,
    #state{}.

%%% ----------------------------------------------------------
initialize_ugFailed(undefined) ->
    info_msg("No failed upgrade!~n"),
    case is_upgrade_ongoing() of
	true ->
	    lock_action_capable_after_upgrade(),
	    ok;
	false ->
	    %% Normal startup
	    unlock_action_capable_after_restart()
	    %% Don't clear progress info here, because OSS may not
	    %% keep up with that.
	    %% clear_all_progress_info()
    end;
initialize_ugFailed(BuName) ->
    info_msg("Upgrade rollback = ~p~n", [BuName]),
    swmLib:erase_variable(ug_failed),
    report_on_reboot(BuName).

%%% ----------------------------------------------------------
init_config_activate(false) ->
    ok;
init_config_activate(true) ->
    Key = swmInventory:make_currentUP_key(),
    NewState = state(activationInProgress),
    Transaction_SetUpState =
	fun() ->
		[Obj] = mnesia:read(upgradePackage, Key),
		NewObj = Obj#upgradePackage{state = NewState},
		ok = mnesia:write(NewObj),
		NewObj
	end,
    NewUP =
	case mnesia:transaction(Transaction_SetUpState) of
	    {atomic, SetUpState_Result} ->
		SetUpState_Result;
	    Aborted ->
		[Obj] = mnesia:dirty_read({upgradePackage, Key}),
		sysInitI:error_report([{"Failed to change state on", Key},
			  Aborted]),
		Obj#upgradePackage{state = NewState}
	end,
    %% Remember what are doing in case of restart
    swmLib:set_variable(activating_up, NewUP).

%% #############################################################################
%% validate_hsi_relations
%%
%% ###=======================================================================###
validate_hsi_relations() ->
    IsAiBuFile = filelib:is_file(swmLib:ai_bu_file()),
    try
	swmLib:set_ram_variable(add_board_STATE, blocked),
	validate_current_installed_LMCs()
    catch
	EC : ER ->
	    {EC, ER}
    after
	if
	    IsAiBuFile ->
		sysInitI:info_report(["Auto Integration with Backup detected",
			   "Keep add_board blocked!"]);
	    true ->
		swmLib:erase_ram_variable(add_board_STATE)
	end
    end.

%% ###=======================================================================###
validate_current_installed_LMCs() ->
    SwDir = swmLib:software_dir(),
    {ok, [{products, Selected}]} =
	swmBoardList:swp([products],
			 [{boardTypeBB, swmBoardList:boardType()},
			  {boardTypesOTH, all},
			  {options, [{global, SwDir}]}]),
    Products =
	Selected ++
	swmBoardList:bundled_products(swmI:get_current_archive_dir(), Selected),
    {_InstalledLMCs, PartlyInstalledLMCs} = installed_cxps(Products),
    sysInitI:info_report(["--- Installed at EE, but not at " ++ SwDir ++ " ---"
	       | [{lmc, LMC} || LMC <- PartlyInstalledLMCs]]),
%%%    [begin
%%%	 sysInitI:info_report([LMC ++ " not installed on " ++ SwDir,
%%%		    "Cleaning up is needed later (audit_software)..."]),
%%%	 swmOs:remove_cxp(LMC)   % Takes too long time. Will be taken care of
%%%						% by audit_software.
%%%						% Make sure to consider this
%%%						% case before audit_software is
%%%						% executed, use installed_cxps/1
%%%     end
%%%     || LMC <- PartlyInstalledLMCs],
    ok.

%% #############################################################################
%% installed_cxps
%%
%% ###=======================================================================###
installed_cxps(Products) ->
    CurrAllLMCs = [swmBoardList:swp_id_format(SWP) || {SWP, _} <- Products],
    SwDir = swmLib:software_dir(),
    SwDirLMCs =
	[filename:basename(LMC)
	 || LMC <- filelib:wildcard(filename:join(SwDir, "*")),
	   filelib:is_dir(LMC)],
    ObsoleteLMCs = CurrAllLMCs -- SwDirLMCs,   % If no link at home/software ->
						% 
    EeInstalledLMCs = swmOs:get_installed_cxps(),
    PartlyInstalledLMCs =
	[LMC || LMC <- ObsoleteLMCs, lists:member(LMC, EeInstalledLMCs)],
    {EeInstalledLMCs -- PartlyInstalledLMCs, PartlyInstalledLMCs}.

%%% ----------------------------------------------------------
handle_call(audit_software_directly, _, State) ->
    do_audit_software(#state{}),
    {reply, ok, State};
%%% ----------------------------------------------------------
handle_call(stPh_activate, _, State) ->
    info_msg("stPh_activate~n", []),
    {reply, ok, State};
%%% ----------------------------------------------------------
handle_call({confirm_package, UpKey}, _, State) ->
    cancel_timer(State#state.fallbackTimer),
    cancel_timer(State#state.alarmTimer),
    cancel_timer(State#state.countdownTimer),
    cancel_timer(State#state.resendTimer),
    [ets:delete(OldTab)||{_, OldTab} <- ets:tab2list(olddb)],
    ets:delete(olddb),

    {_, _, _, Up} = UpKey,
    do_clear_fallback_alarm(Up),

    %% HU21335
    %% All cleaning up must be made before confirming to EE
    %% HU29568 
    %% New upgrade_init handling
    %% Db file no longer stored in /rcs/swm but retain this cleaning
    %% for backwards compatiblity

    cmd(["rm -f ",filename:join(swmLib:swm_dir(), "upgrade_init*")]),
    cmd(["rm -f ",filename:join(sysEnv:home_dir(), "upgrade_init*")]),
    cmd(["rm -rf ",swmLib:upgrade_prep_dir()]),
    update_count(-1),
    swmDbMonitor:force_auto_backup(), % HU37957
    swmOs:commit(),

    SignCertTimerRef = check_if_crl_needs_update(Up),
   
    {reply, ok, State#state{countdownTimer = undefined,
			    fallbackTimer = undefined,
			    alarmTimer = undefined,
			    resendTimer = undefined,
			    state = undefined,
			    signCertTimer = SignCertTimerRef}};
handle_call(get_state, _From, State) ->
    {reply, State#state.state, State};
handle_call(get_keyed_state, _From, State) ->
    %% This is for testing purposes
    List = lists:zip(record_info(fields, state),
		     tl(tuple_to_list(State))),
    {reply, maps:from_list(List), State};
handle_call(get_sign_cert, _From, #state{signCertTimer = TimerRef} = State) ->
    TimeLeft =
	case TimerRef of
	    undefined ->
		%% No graceful period started
		undefined;
	    _ ->
		File = filename:join(sysEnv:home_dir(), "swm/SignCertTimer"),
		 case file:consult(File) of
		     {ok, [{Term, _Up}]} ->
			 comsaI:iso_time(Term, extended);
		     Error ->
			 %% Not able to read the file that should have be saved on  disk
			 %% Calculate the time left from the TimerRef instead.
			 error_msg("Not possible to read sign cert file: ~p~n", [Error]),
			 TimeLeftMSec = erlang:read_timer(TimerRef),
			 CurrentTime = calendar:datetime_to_gregorian_seconds(
					 calendar:universal_time()),
			 ExpirationDate = calendar:gregorian_seconds_to_datetime(
					    CurrentTime + round(TimeLeftMSec/1000)),
			 comsaI:iso_time(ExpirationDate, extended)
		 end
	end,
    {reply, TimeLeft, State};
handle_call(confirm_sign_cert, _From, #state{signCertTimer = TimerRef} = State) ->
    case TimerRef of
	undefined ->
	    %% No graceful period started
	    do_nada;
	TimerRef ->
	    erlang:cancel_timer(TimerRef),
	    confirm_certificate()
    end,
    {reply, ok, State#state{signCertTimer = undefined}};
handle_call({change_sign_cert, Timeout}, _From, 
	    #state{signCertTimer = TimerRef} = State) ->
    R = case TimerRef of
	    undefined ->
		%% No graceful period started, possible to change the timer
		swmLib:set_variable(sign_cert_timer, Timeout),
		ok;
	    TimerRef ->
		%% A graceful period is already running, not possible to change the time
		nok
	end,
    {reply, R, State};
handle_call(stop_soaking_timer, _, State) ->
    handle_stop_soaking_timer(State),
    {reply, soaking_stopped, State#state{soakingTimer = undefined}};
handle_call(_Request, _From, State) ->
    {reply, undefined, State}.

%%% ###########################################################################
%%% do_add_board
%%%
%%% ###=====================================================================###
do_add_board([],   % Not downloaded Products
	     NotInstalledProducts,
	     BoardType,
	     State) ->
    [Up] = ets:lookup(upgradePackage, swmInventory:make_currentUP_key()),
    ArchiveDir = get_up_archive_dir(Up#upgradePackage.administrativeData),
    FileSizesG = get_file_sizes_global(NotInstalledProducts, ArchiveDir),
    unpack_package(ArchiveDir, FileSizesG, swmOs:get_active_instance()),
    swmOs:make_links(ArchiveDir, swmLib:software_dir(), NotInstalledProducts),
    swmLib:make_cxp_list(sysEnv:home_dir(), current_selected_sw()),
    Heading =
	io_lib:format("Previously downloaded software for ~p is now installed",
		      [BoardType]),
    log_SwmInternal(info, Heading),
    sysInitI:info_report([{"Previously downloaded SW installed for", BoardType}]),
    swmLib:write_swm_log(?SwmLog_CurrUP, info, Heading),
    State;
do_add_board(NotDownloadedProducts,
	     NotInstalledProducts,
	     BoardType,
	     State) ->
    UpId = swmInventory:make_currentUP_key(),
    Up = get_UP_WithUri(UpId),
    case ftpI:parse_uri(Up#upgradePackage.uri) of
	{ok, Ftp} when element(1, Ftp) =:= sftp orelse
		       element(1, Ftp) =:= ftpes ->
	    ArchiveDir =
		get_up_archive_dir(Up#upgradePackage.administrativeData),
	    try
		dwnld_pckgs_partial(Up,
				    ArchiveDir,
				    NotDownloadedProducts,
				    Ftp),
		swmFtp:verified_URIs(#{up_ids => [UpId]}),
		InfoDld = io_lib:format("Software for ~p is now downloaded",
					[BoardType]),
		log_SwmInternal(info, InfoDld),
		sysInitI:info_report([{"SW downloaded for", BoardType}]),
		swmLib:write_swm_log(?SwmLog_CurrUP, info, InfoDld)
	    catch
		ErrClass : ErrReason ->
		    Stacktrace = erlang:get_stacktrace(),
		    files_delete([F || {_, {_, F}} <- NotDownloadedProducts],
				 ArchiveDir),
		    sysInitI:info_report([{notDownloadedProducts, NotDownloadedProducts},
			       {boardType, BoardType},
			       {ErrClass, ErrReason},
			       {stacktrace, Stacktrace}]),
		    throw({download_fail, ErrReason})
	    end;
	{ok, ParsedUri} when element(1, ParsedUri) == file ->
	    Reason = {error, "Unexpected SFTP server fault"},
	    sysInitI:info_report([Reason,
		       {parsedUri, ParsedUri}]),
	    erlang:error(Reason)
    end,
    case (NotInstalledProducts -- NotDownloadedProducts) of
	[] ->
	    InfoInst = io_lib:format("Software for ~p is now installed",
				     [BoardType]),
	    log_SwmInternal(info, InfoInst),
	    sysInitI:info_report([{"SW installed for", BoardType}]),
	    swmLib:write_swm_log(?SwmLog_CurrUP, info, InfoInst),
	    State;
	DownLoadedButNotInstalledProducts ->
	    do_add_board([],
			 DownLoadedButNotInstalledProducts,
			 BoardType,
			 State)
    end.

%%% ###########################################################################
%%% get_UP_isUriSet
%%%
%%% ###=====================================================================###
get_UP_isUriSet(UpId) ->
    F = fun() ->
		mnesia:read(upgradePackage, UpId)
	end,
    case transaction(F) of
	[#upgradePackage{uri = Uri} = Obj] when Uri /= undefined ->
	    Obj#upgradePackage{uri = http_uri:decode(Uri)};
						% Handle HTTP encoded URIs;
						% HV58153
	_ ->
	    false
    end.

%%% ###########################################################################
%%% get_UP_WithUri
%%%
%%% ###=====================================================================###
get_UP_WithUri(UpId) ->
    case get_UP_isUriSet(UpId) of
	false ->
	    case aicI:is_ai_ongoing() of
		false ->
		    timer:sleep(2000),   % Make sure mnesia events are done.
		    case get_UP_isUriSet(UpId) of
			false ->
			    throw({uc_error, "SFTP uri not specified"});
			UP ->
			    UP
		    end;
		true ->
		    throw(ai_ongoing)
	    end;
	UP ->
	    UP
    end.

%%% ###########################################################################
%%% swp_products_subtract
%%%
%%% ###=====================================================================###
swp_products_subtract([{SWP, _} = Product | Tail], SWPs) ->
    case lists:member(swmBoardList:swp_id_format(SWP), SWPs) of
	true ->
	    swp_products_subtract(Tail, SWPs);
	false ->
	    [Product | swp_products_subtract(Tail, SWPs)]
    end;
swp_products_subtract([], _) ->
    [].

%% #############################################################################
%% transaction
%%
%% ###=======================================================================###
transaction(F) ->
    case mnesia:is_transaction() of
	false ->
	    case mnesia:transaction(F) of
		{atomic, Result} ->
		    Result;
		Aborted ->
		    Aborted
	    end;
	true ->
	    F()
    end.

%%% ----------------------------------------------------------
%%% Description: This is to avoid blocking other applications
%%% ----------------------------------------------------------

handle_cast({initialize, Args}, initializing) ->
    initialize(Args); 


handle_cast({add_board,
	     {ProdNo, ProdRev} = BoardType,
	     NotDownloadedProducts,
	     NotInstalledProducts,
	     CbModule} = Request,
	    #state{add_board_retry = ABR} = State) ->
    NewState =
	try
	    NS = do_add_board(NotDownloadedProducts,
			      NotInstalledProducts,
			      BoardType,
			      State),
	    Sw = prepPush_appdata_single(BoardType),
	    swmBoardList:boardTypeOth_swOk(BoardType, Sw),
	    CbModule:board_added(ProdNo, ProdRev, ok),
	    swmInventory:update_table(BoardType),
	    NS#state{add_board_retry = ABR -- [BoardType]}
	catch
	    throw : {FailClass, Reason} when FailClass == fail orelse
					     FailClass == uc_error ->
		Res = {error, make_string_flat(Reason)},
		sysInitI:warning_report([{add_board, BoardType},
			   {notDownloadedproducts, NotDownloadedProducts},
			   {notInstalledproducts, NotInstalledProducts},
			   Res]),
		swmLib:write_swm_log(?SwmLog_CurrUP,
				     error,
				     make_string_flat(Reason)),
		swmBoardList:boardTypeOth_swNok(BoardType),
		swmFtp:verify_URIs(#{up_ids => [swmInventory:
						make_currentUP_key()],
				     reply => false}),
		CbModule:board_added(ProdNo, ProdRev, Res),
		State#state{add_board_retry = ABR -- [BoardType]};
	    throw : {download_fail, Reason} ->
		Res = {error, make_string_flat(Reason)},
		sysInitI:warning_report([{add_board, BoardType},
			   {notDownloadedproducts, NotDownloadedProducts},
			   {notInstalledproducts, NotInstalledProducts},
			   Res]),
		swmLib:write_swm_log(?SwmLog_CurrUP,
				     error,
				     make_string_flat(Reason)),
		swmBoardList:boardTypeOth_swNok(BoardType),
		case lists:member(BoardType, ABR) of
		    true ->
			swmFtp:verify_URIs(#{up_ids => [swmInventory:
							make_currentUP_key()],
					     reply => false}),
			CbModule:board_added(ProdNo, ProdRev, Res),
			State#state{add_board_retry = ABR -- [BoardType]};
		    false ->
			timer:apply_after(?Timer_AddBoard_Retry,
					  gen_server,
					  cast,
					  [?SERVER, Request]),
			State#state{add_board_retry = [BoardType | ABR]}
		end;
	    throw : ai_ongoing ->
		%% AI ongoing means that the procedure to set values from the
		%% siteBasic file is not ready. The defaultUri and
		%% defaultPassword - if they exist - has not been set yet.
		sysInitI:info_report([{add_board, ai_ongoing}]),
		timer:apply_after(?Timer_AddBoard_Retry_Short,
				  gen_server,
				  cast,
				  [?SERVER, Request]),
		%% No update of state.add_board_retry since we need to retry
		%% not only once, but until AI is finished.
		State;
	    ErrClass : ErrReason ->
		Stacktrace = erlang:get_stacktrace(),
		Res = {error, "Software error"},
		sysInitI:error_report([{ErrClass, ErrReason}, {stacktrace, Stacktrace}]),
		swmBoardList:boardTypeOth_swNok(BoardType),
		swmFtp:verify_URIs(#{up_ids => [swmInventory:
						make_currentUP_key()],
				     reply => false}),
		CbModule:board_added(ProdNo, ProdRev, Res),
		State#state{add_board_retry = ABR -- [BoardType]}
	end,
    {noreply, audit_software(NewState, ?Timer_AuditSw_short)};

%%% ----------------------------------------------------------
%%% Description: This is called to start a timer before it has
%%% been activation.
%%% ----------------------------------------------------------

handle_cast(set_global_fallback, State) ->
    UP = swmLib:get_variable(activating_up),
    Key = UP#upgradePackage.upgradePackageId,
    WaitTime = case swmLib:get_variable(swm_test_rollback_timer) of
		   undefined ->
		       info_msg(
			 "Enable data conversion timeout in ~w seconds~n",
			 [?POST_REBOOT_ACTIVATE_TIMEOUT]),
		       ?POST_REBOOT_ACTIVATE_TIMEOUT*1000;
		   WT ->
		       warning_msg(
			 "Data conversion timeout in ~w seconds (lab config)~n",
			 [WT/1000]),
		       WT
	       end,
    {ok, FallbackTimer} =
	timer:apply_after(WaitTime,
			  swmServer,
			  fallback,
			  [Key, 
			   data_conversion,
			   "Timeout before completion of data conversion"]),
    {noreply, State#state{fallbackTimer=FallbackTimer}};

%% handle_cast(audit_software, State) ->
%%     case is_upgrade_ongoing() of
%% 	true ->  % no audit during upgrade
%% 	    ok;
%% 	false ->
%% 	    audit_software()
%%     end,
%%     {noreply, State};

handle_cast(audit_software, State) ->
    {noreply, audit_software(State)};

handle_cast({create_package, Uri, Password, Id}, State) ->
    set_default_progress_report("createUpgradePackage", Id),
    swmDbMonitor:force_auto_backup(),
    handle_stop_soaking_timer(State),
    action_handler(fun() -> handle_create_package(Id, Uri, Password) end),
    ok = swmLib:unlock_action_capable(?CREATE_UP_ACTION_CAPABLE_ID),
    {noreply, State#state{soakingTimer=undefined}};
handle_cast({remove_package, UpDn}, State) ->
    Id = actionId_new(),
    set_default_progress_report("removeUpgradePackage", Id),
    swmDbMonitor:force_auto_backup(),
    handle_stop_soaking_timer(State),
    action_handler(fun() -> handle_remove_package(UpDn) end),
    ok = swmLib:unlock_action_capable(?REMOVE_UP_ACTION_CAPABLE_ID),
    {noreply, State#state{soakingTimer=undefined}};

%% handle_cast({remove_sw_version, VsnDn}, State) ->
%%     Id = actionId_new(),
%%     set_default_progress_report("removeSwVersion", Id),
%%     swmDbMonitor:force_auto_backup(),
%%     action_handler(fun() -> handle_remove_sw_version(VsnDn) end),
%%     {noreply, State};

handle_cast({remove_software_version, VsnDn}, State) ->
    Id = actionId_new(),
    set_default_progress_report("removeSoftwareVersion", Id),
    swmDbMonitor:force_auto_backup(),
    handle_stop_soaking_timer(State),
    action_handler(fun() -> handle_remove_software_version(VsnDn) end),
    {noreply, State#state{soakingTimer=undefined}};

handle_cast({prepare_package, UpKey}, State) ->
    Id = actionId_new(),
    set_default_up_progress_report(UpKey, "prepare", Id),
    swmDbMonitor:force_auto_backup(),
    handle_stop_soaking_timer(State),
    action_handler(fun() -> handle_prepare_package(UpKey) end, UpKey),
    ok = swmLib:unlock_action_capable(?PREPARE_UP_ACTION_CAPABLE_ID),
    {noreply, State#state{soakingTimer=undefined}};

handle_cast({verify_package, UpKey}, State) ->
    Id = actionId_new(),
    set_default_up_progress_report(UpKey, "verify", Id),
    swmDbMonitor:force_auto_backup(),
    cancel_timer(State#state.soakingTimer),
    action_handler(fun() -> handle_verify_package(UpKey) end, UpKey),
    ok = swmLib:unlock_action_capable(?VERIFY_UP_ACTION_CAPABLE_ID),
    {noreply, State#state{soakingTimer=undefined}};

handle_cast({activate_package, UpKey}, State) ->
    Id = actionId_new(),
    set_default_up_progress_report(UpKey, "activate", Id),
    swmDbMonitor:force_auto_backup(),
    cancel_timer(State#state.soakingTimer),
    Result=action_handler(fun() -> handle_activate_package(UpKey) end,UpKey),
    case Result of
	{ok, activationInProgress} ->
	    {noreply, State#state{state=activationInProgress,
				  soakingTimer=undefined}};
	_ ->
	    ok = swmLib:unlock_action_capable(?ACTIVATE_UP_ACTION_CAPABLE_ID),
	    {noreply, State#state{soakingTimer=undefined}}
    end;
handle_cast(activation_complete, State) 
  when State#state.state==activationInProgress ->
    UP = swmLib:get_variable(activating_up),
    Key = UP#upgradePackage.upgradePackageId,
    update_progress(
      Key, [{additionalInfo, "Database conversion complete"},
	    {progressPercentage, 80}]),
    %% Cancelling the post reboot activate timer
    cancel_timer(State#state.fallbackTimer),

    %% This is obviously a fake, we cannot know when the applications
    %% are started. We should get a ping from APPM or some way to monitor
    %% the startup
    timer:sleep(60000),
    update_progress(
      Key, [{additionalInfo, "System startup complete"},
	    {progressPercentage, 95}]),

    case handle_activation_complete() of
        {ok, {CountdownTimer, FallbackTimer, AlarmTimer, ResendTimer}} ->

            {noreply, State#state{state = waitingForCommit,
				  fallbackTimer = FallbackTimer,
				  alarmTimer = AlarmTimer,
				  countdownTimer = CountdownTimer,
				  resendTimer = ResendTimer}};
        Error ->
            {stop, Error, State}
    end;

handle_cast({post_confirm_package, _}, State) ->
    %% Consider moving this to the confirm_package stage
    %% All but the audit software
    swmFallbackList:add_pending(),
    [UP] = swmInventory:get_current_sw_version(),
    %% HT47248 fix
    UpStr = 
	UP#'ProductData'.productName++"_"++
	format_product_number(UP#'ProductData'.productNumber)++"_"++
	UP#'ProductData'.productRevision,
    BuName = "Final_backup_for_"++UpStr++"_"++
	comsaI:iso_time(os:timestamp(), basic),
    %% Fix ends here
    ManagerKey = {"1","1","1","1"},
    Id = swmLib:get_new_action_id(mgr),
    swmBackupModel:set_default_progress_report(mgr, "CREATE", Id),
    create_backup(list_to_binary(BuName), ManagerKey, system, mgr),
    swmFallbackList:add_backup(after_ug, BuName),
    swmFallbackList:add_backup(latest, BuName),
    swmLib:unlock_action_capable(?CONFIRM_UP_ACTION_CAPABLE_ID),
    swmDbMonitor:force_auto_backup(), % HU71270
    swmBackup:install_bootfallback(),   % Audit is done inside install_bfb
    {noreply, State};

handle_cast(soaking_expired, State) ->
    handle_soaking_expired(),
    {noreply, State#state{soakingTimer = undefined}};


handle_cast(check_and_start_soaking_timer, State) ->
    cancel_timer(State#state.soakingTimer),
    TimerRef = 
	case check_and_start_soaking_timer() of
	    {ok, TRef} ->
		info_msg("Soaking timer is started~n"),
		TRef;
	    soaking_never_ends ->
		info_msg("Soaking is open ended~n"),
		undefined;
	    no_soaking ->
		info_msg("Soaking is NOT ongoing~n"),
		undefined
	end,
    {noreply, State#state{soakingTimer = TimerRef}};

handle_cast(Request, State) ->
    error_msg("Unknown request: ~p~nState = ~p~n",[Request, State]),
    {noreply, State}.
%%% ----------------------------------------------------------
%%% ----------------------------------------------------------

handle_info({cancel, _UpKey}, State)
  when State#state.state == activationInProgress ->
    info_msg("'cancel' not supported while 'activate' in progress~n"),
    {noreply, State};

%% HS37544 solution: For the time being do not support 'cancel' at
%% all while 'activate' is in progress; neither before nor after the
%% restart. Note that COM is not enabled anyway before the state
%% changes to 'waitingForCommit'.

%% handle_info({cancel, UpKey}, State) 
%%   when State#state.state == activationInProgress ->
%% %%% FIXME! Find out if the UpKey matches the ongoing activation!!! jotj
%%     [Obj] = mnesia:dirty_read({upgradePackage, UpKey}),
%%     mnesia:dirty_write(Obj#upgradePackage{state = state(prepareCompleted)}),
%% 
%%     case swmLib:get_variable(set_unpacked) of
%%  Vsn when Vsn /= undefined->
%%      release_handler:remove_release(Vsn),
%%      swmLib:erase_variable(set_unpacked);
%%  undefined ->
%%      ok
%%     end,
%%     cmd("rm -rf "++sysEnv:home_dir_other()),
%%     swmInventory:update_tables(),
%% 
%%     CompleteTime = comsaI:iso_time(os:timestamp(), extended),
%%     update_progress([{result, ?ActionResultType_FAILURE},
%%           {state, ?ActionStateType_CANCELLED},
%%                   {progressPercentage, 100},
%%           {timeActionCompleted, CompleteTime}]),
%%     {noreply, State#state{state=undefined}};
handle_info({cancel, Key}, State) when State#state.state==waitingForCommit ->
    case global:set_lock({busy, cancel}, [node()], 0) of
	true ->
	    case swmLib:get_ram_variable(activationStarted) of
		Key ->
		    mnesia:transaction(fun() -> swmLib:erase_ram_variable(activationStarted) end);
		_Else ->
		    do_nada
	    end,
	    try handle_cancel_after_activate(Key) of
		busy ->
		    {noreply, State};
		_ -> 
		    {noreply, State#state{state=undefined}}
	    catch Type:Reason ->
		    sysInitI:error_report(
		      [{Type, Reason}, {stacktrace, erlang:get_stacktrace()}]),
		    {noreply, State}
	    after 
		global:del_lock({busy, cancel})
	    end;
	false -> 
	    {noreply, State}
    end;
handle_info({cancel, _}, State) ->
    %% HS44329 Cancel was performed out of order
    {noreply, State};
handle_info({mnesia_table_event, Event}, State) -> 
    try handle_mnesia_table_event(Event)
    catch Type:Error ->
            sysInitI:error_report(
              [{mfa, {?MODULE, handle_mnesia_table_event, [Event]}},
               {Type, Error},
	       {stacktrace, erlang:get_stacktrace()}])
    end,
    {noreply, State};
handle_info({'ETS-TRANSFER' = Msg, TabId, FromPid, HeirData}, State) ->
    sysInitI:info_report([{msg, Msg},
	       {tabId, TabId},
	       {fromPid, FromPid} | format_HeirData(HeirData)]),
    {noreply, State};
handle_info({timeout, _, sign_cert_timer_exipred}, State) ->
    confirm_certificate(),
    {noreply,  State#state{signCertTimer = undefined}};
handle_info(audit_software_timeout = Msg, State) ->
    NewState =
	try
	    do_audit_software(State)
	catch
	    ErrClass : ErrReason ->
		Stacktrace = erlang:get_stacktrace(),
		sysInitI:error_report([Msg,
			  {ErrClass, ErrReason},
			  {stacktrace, Stacktrace}]),
		State#state{auditSwTimer = undefined}
	end,
    {noreply, NewState};
handle_info({'DOWN', MonitorRef, process, _Pid, Info} = Msg, State) ->
    case Info of
	normal ->
	    ok;
	_ ->
	    sysInitI:error_report(["Monitored process terminated", {msg, Msg}])
    end,
    NewState = child_restart(MonitorRef, State),
    {noreply, NewState};
handle_info({resend_timer_expired, Time}, State) ->
    info_msg("Resend timer expired, ~p~n",[Time]),
    resend_activation_complete(),
    [SwM] = mnesia:dirty_read({swM, {"1","1","1"}}),
    FallbackTime = SwM#swM.fallbackTimer*1000,
    NewTime = case Time*2 of
		  NT when NT > FallbackTime ->
		      Time;
		  NT -> NT
	      end,
    {ok, ResendTimer} = timer:send_after(
			  NewTime, {resend_timer_expired, NewTime}),
    {noreply, State#state{resendTimer = ResendTimer}};
handle_info({children_start, Children}, State) ->
    {noreply, children_start(Children, State)};
handle_info({?MODULE, info}, State) ->
    sysInitI:info_report(?STATE_INFO(State) ++ ?PROC_INFO(self())),
    {noreply, State};
handle_info(_Request, State) ->
    {noreply, State}.


%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
terminate(_Reason, _State) ->
    dets:close(upgradePackage),
    dets:close(swVersion),
    mnesia_event_server_stop(),
    ok.

%%% ----------------------------------------------------------
format_HeirData(List) when is_list(List) ->
    List;
format_HeirData(Other) ->
    [Other].

%%% ----------------------------------------------------------
children_start([{{M, F, A} = MFA, Cnt} | Tail],
	       #state{children = Children} = State) ->
    case apply(M, F, A) of
	{ok, Pid} ->
	    MonitorRef = erlang:monitor(process, Pid),
	    children_start(Tail,
			   State#state{children = [{MonitorRef, MFA, Cnt}
						   | Children]});
	Error ->
	    sysInitI:warning_report([Error]),
	    child_restart_delay(MFA, Cnt),
	    children_start(Tail, State)
    end;
children_start([], State) ->
    State.

%%% ----------------------------------------------------------
child_restart(MonitorRef, #state{children = Children} = State) ->
    case lists:keyfind(MonitorRef, 1, Children) of
	{_, MFA, Cnt} ->
	    child_restart_delay(MFA, Cnt),
	    State#state{children = lists:keydelete(MonitorRef, 1, Children)};
	false ->
	    sysInitI:error_report([{"Unknown process monitor ref", MonitorRef}
		      | ?STATE_INFO(State)]),
	    State
    end.

%%% ----------------------------------------------------------
child_restart_delay(MFA, Cnt) when Cnt < ?MAX_CHILD_RESTARTS ->
    Delay = Cnt * 10,
    timer:send_after(Delay, {children_start, [{MFA, Cnt + 1}]}),
    sysInitI:info_report([{"Restarting child process in",
		sysUtil:time_to_string(Delay * 1000000, milli_seconds)},
	       {child_spec, MFA},
	       {startCounter, Cnt}]);
child_restart_delay(MFA, Cnt) ->
    sysInitI:warning_report([{"Restart limit reached.", Cnt},
	       {"Not restarted any more", "Functionality is disabled."},
	       {mFA, MFA}]).

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
						%internal_function1(One, Two)->
						%   nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
check_if_crl_needs_update(Up) ->
    case swmLib:get_variable(anti_rollback) of
	on ->
	    %% Check if CRL information in the flash needs to be updated
	    case swmOs:check_crl(sysEnv:rcs_mode_2()) of
		update_not_req ->
		    undefined;
		update_req ->
		    info_msg("CRL update detected: ~n~s~n",
			     [swmOs:get_crl_ids(sysEnv:rcs_mode_2())]),
		    swmLib:write_swm_log(
		      "SwM", critical, 
		      "CRL update detected. Starting anti-rollback timer"),
		    SignCertTimer =
			case swmLib:get_variable(sign_cert_timer) of
			    undefined ->
				?SIGNING_CERT_TIMER_SEC;
			    Timer ->
				Timer
			end,
		    
		    %% Write down the UP and the time in a file and store it on the disk
		    CurrentTime = calendar:datetime_to_gregorian_seconds(
				    calendar:universal_time()),
		    ExpirationDate = calendar:gregorian_seconds_to_datetime(
				       CurrentTime + SignCertTimer),
		    
		    File = filename:join(sysEnv:home_dir(), "swm/SignCertTimer"),
		    ok = filelib:ensure_dir(File),
		    case file:open(File, [write]) of
			{ok, FD} ->
			    io:format(FD, "~p.", [{ExpirationDate, Up}]),
			    file:close(FD),
			    
			    %% Make sure that no other SWM actions can be done during 
			    %% the timer.
			    %% First remove the CONFIRM_UP_ACTION and then raise 
			    %% SIGNING_CERTIFICATE
			    swmLib:unlock_action_capable(?CONFIRM_UP_ACTION_CAPABLE_ID),
			    swmLib:lock_action_capable(?SIGNING_CERTIFICATE_ACTION_CAPABLE_ID,
						       ?SIGNING_CERTIFICATE_ACTION_CAPABLE_INFO 
						       ++ comsaI:iso_time(ExpirationDate, extended));
			Error ->
			    error_msg("Not possible to open sign cert file: ~p~n", 
				      [Error])
		    end,
		    erlang:start_timer(SignCertTimer*1000, self(), 
				       sign_cert_timer_exipred)
	    end;
	_Else ->
	    %% Disabled due to HW10614
	    %% It was discovered that check_crl also writes on the disk if the flash
	    %% is empty. In order to avoid that, this will also be removed for the
	    %% time being. jotj 2017-07-10
	    ok
    end.
    
    %% Check if CRL information in the flash needs to be updated
    %% case swmOs:check_crl(sysEnv:rcs_mode_2()) of
    %% 	update_not_req ->
    %% 	    undefined;
    %% 	update_req ->
    %% 	    info_msg("CRL update detected: ~n~s~n",
    %% 		     [swmOs:get_crl_ids(sysEnv:rcs_mode_2())]),
    %% 	    swmLib:write_swm_log(
    %% 	      "SwM", critical, 
    %% 	      "CRL update detected. Starting anti-rollback timer"),
    %% 	    %% HW10614 workaround
    %% 	    error_msg("Unexpected signing result. See SwmInternal log~n"),
    %% 	    undefined
    	%% update_req -> % DISABLED DUE TO HW10614
	    %% info_msg("CRL update detected: ~n~s~n",
	    %% 	     [swmOs:get_crl_ids(sysEnv:rcs_mode_2())]),
	    %% swmLib:write_swm_log(
	    %%   "SwM", critical, 
	    %%   "CRL update detected. Starting anti-rollback timer"),
    	%%     SignCertTimer =
    	%% 	case swmLib:get_variable(sign_cert_timer) of
    	%% 	    undefined ->
    	%% 		?SIGNING_CERT_TIMER_SEC;
    	%% 	    Timer ->
    	%% 		Timer
    	%% 	end,
	 
    	%%     %% Write down the UP and the time in a file and store it on the disk
    	%%     CurrentTime = calendar:datetime_to_gregorian_seconds(
    	%% 		    calendar:universal_time()),
    	%%     ExpirationDate = calendar:gregorian_seconds_to_datetime(
    	%% 		       CurrentTime + SignCertTimer),

       	%%     File = filename:join(sysEnv:home_dir(), "swm/SignCertTimer"),
    	%%     ok = filelib:ensure_dir(File),
    	%%     case file:open(File, [write]) of
    	%% 	{ok, FD} ->
    	%% 	    io:format(FD, "~p.", [{ExpirationDate, Up}]),
    	%% 	    file:close(FD),

	%% 	    %% Make sure that no other SWM actions can be done during the timer.
	%% 	    %% First remove the CONFIRM_UP_ACTION and then raise SIGNING_CERTIFICATE
	%% 	    swmLib:unlock_action_capable(?CONFIRM_UP_ACTION_CAPABLE_ID),
	%% 	    swmLib:lock_action_capable(?SIGNING_CERTIFICATE_ACTION_CAPABLE_ID,
	%% 				       ?SIGNING_CERTIFICATE_ACTION_CAPABLE_INFO 
	%% 				       ++ comsaI:iso_time(ExpirationDate, extended));
    	%% 	Error ->
    	%% 	    error_msg("Not possible to open sign cert file: ~p~n", [Error])
    	%%     end,
    	%%     erlang:start_timer(SignCertTimer*1000, self(), sign_cert_timer_exipred)
    %% end.
 
confirm_certificate() ->
    %% Signing certificate timer has exipred or manually confirmed via COLI
    %% store the CRL in EE, change capable to unlocked,  remove file sign
    %% cert file on disk, remove old bakcups and UPs.
    warning_msg("Confirming the new certificates, old UPs and backups will be removed~n",[]),
    case swmLib:get_variable(anti_rollback) of
	on ->
	    swmOs:store_crl(sysEnv:rcs_mode_2());
	_Else ->
	    %% HW10614
	    do_nada
    end,
    File = filename:join(sysEnv:home_dir(), "swm/SignCertTimer"),
    file:delete(File),
    swmLib:unlock_action_capable(?SIGNING_CERTIFICATE_ACTION_CAPABLE_ID),
    %% Remove all backups except the last final one
    swmBackupFile:delete_all_backups(),
    swmFallbackList:audit_fallback_list(),
    swmFailsafe:add_backup_to_list("BrmFailsafeBackup", 
				   "Post_certificate_revocation_backup_"),
    AllUPs = mnesia:dirty_all_keys(upgradePackage),
    [UpVersion] = swmInventory:get_current_sw_version(),
    CurrentUp = 
	UpVersion#'ProductData'.productNumber++"-"++
	UpVersion#'ProductData'.productRevision,
    ok = remove_all_old_ups(CurrentUp, AllUPs),
    ok = swmLib:unlock_action_capable(?REMOVE_UP_ACTION_CAPABLE_ID).

remove_all_old_ups(_CurrentUp, []) ->
    ok;
remove_all_old_ups(CurrentUp, [{_,_,_,CurrentUp} | Rest]) ->
    remove_all_old_ups(CurrentUp, Rest);
remove_all_old_ups(CurrentUp, [Up | Rest]) ->
    %% Remove old UPs that is no longer supported due to updated certificates
    Id = actionId_new(),
    set_default_progress_report("removeUpgradePackage", Id),
    swmDbMonitor:force_auto_backup(),
    action_handler(fun() -> do_handle_remove_package
			      (Up, " due to signing certificate revocation") end),
    remove_all_old_ups(CurrentUp, Rest).


%% HU30730
%% Release housekeeping lock after creating a system created backup
create_backup(Name, BrmBackupManagerKey, Type=system, Progress) ->
    MoRef = swmBackup:create_backup_common(Name, BrmBackupManagerKey,
					   Type, Progress),
    Index = lists:last(string:tokens(MoRef, "=")),
    swmLib:unlock_backup(Index),
    MoRef.


%%% ----------------------------------------------------------
%%% #           audit_progress_state()
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: This function is called if swmServer is restarted for any
%%%              other than an ongoing activation, whereby the progress is
%%%              set to a final state
%%% ----------------------------------------------------------

%% HS27033
audit_progress_state() ->
    {atomic, [SwM]} =
	mnesia:transaction(fun() -> mnesia:read(swM, {"1","1","1"}) end),
    case SwM#swM.reportProgress of
	undefined -> 
	    ok;
	Progress ->
	    case Progress#'AsyncActionProgress'.state of
		?ActionStateType_RUNNING ->
		    swmFallbackList:cancel_pending(), % HS51856
		    CompleteTime =
			comsaI:iso_time(os:timestamp(), extended),
		    update_progress(
		      [{result, ?ActionResultType_FAILURE},
		       {progressPercentage, 100},
		       {state, ?ActionStateType_FINISHED},
		       {resultInfo, "The action was interrupted by a restart"},
		       {timeActionCompleted, CompleteTime}]);
		?ActionStateType_CANCELLING ->
		    swmFallbackList:cancel_pending(), %HS51856
		    %% Since the swmServer has restarted any
		    %% cancellation would have been handled by
		    %% initialization cleanup, hence sucess
		    CompleteTime =
			comsaI:iso_time(os:timestamp(), extended),
		    %% HS76784
		    update_progress([{result, ?ActionResultType_FAILURE},
				     {state, ?ActionStateType_CANCELLED},
				     {progressPercentage, 100},
				     {timeActionCompleted, CompleteTime}]),
		    ok;
		_ ->
		    %% Check reportProgress for all UPs
		    {atomic, UPKey} =
			mnesia:transaction(fun() -> mnesia:first(upgradePackage) end),
		    audit_progress_state_ups(UPKey),
		    ok
	    end
    end.

%% TODO, Johan/Rabbe, behover SWM nivan kollas eller racker det med UP?
audit_progress_state_ups('$end_of_table') ->
    %% No more Ups to search
    ok;
audit_progress_state_ups(UPKey) ->
    {atomic, [UP]} =
	mnesia:transaction(fun() -> mnesia:read(upgradePackage, UPKey) end),
    case UP#upgradePackage.reportProgress of
        undefined -> 
	    ok;
        Progress ->
	    case Progress#'AsyncActionProgressWithSteps'.state of
		?ActionStateType_RUNNING ->
		    swmFallbackList:cancel_pending(), % HS51856
		    CompleteTime =
			comsaI:iso_time(os:timestamp(), extended),
		    update_progress(UPKey,
				    [{result, ?ActionResultType_FAILURE},
				     {progressPercentage, 100},
				     {state, ?ActionStateType_FINISHED},
				     {resultInfo, "The action was interrupted by a restart"},
				     {timeActionCompleted, CompleteTime}]);
		?ActionStateType_CANCELLING ->
		    swmFallbackList:cancel_pending(), %HS51856
		    %% Since the swmServer has restarted any
		    %% cancellation would have been handled by
		    %% initialization cleanup, hence sucess
		    CompleteTime =
			comsaI:iso_time(os:timestamp(), extended),
		    %% HS76784
		    update_progress(UPKey, [{result, ?ActionResultType_FAILURE},
					    {state, ?ActionStateType_CANCELLED},
					    {progressPercentage, 100},
					    {timeActionCompleted, CompleteTime}]),
		    ok;
		_ ->
		    ok
            end
    end,
    {atomic, NextUPKey} =
	mnesia:transaction(fun() -> mnesia:next(upgradePackage, UPKey) end),
    audit_progress_state_ups(NextUPKey).

%%% ----------------------------------------------------------
%%% #           handle_create_package(ActionId, Uri, Password)
%%% Input: ActionId:integer() - Used in progress reporting
%%%        Uri:string() - Location of the UP
%%%        Password:string() - Password to the service
%%% Output:
%%% Exceptions:
%%% Description: Open sftp and download either the metadata file or entire
%%%              upgrade package container. The given URI is expected to be
%%%              of type sftp, specifying an sftp user and a server directory,
%%%              holding the UP metadata file and the CXP files.
%%% ----------------------------------------------------------

-spec handle_create_package(non_neg_integer(), string(), string()) -> ok.

handle_create_package(ActionId, RawUri, Password) ->
    Uri = http_uri:decode(RawUri), %Handle HTTP encoded URIs; HV58153
    swmLib:write_swm_log("SwM", info, "createUpgradePackage "++Uri),
    update_progress([{additionalInfoClear, "createUpgradePackage commenced"}]),

    audit_software_directly(),

    case ftpI:parse_uri(Uri) of
	{error, ParsingProblem} ->
	    throw({fail, "Uri fault: " ++ make_string(ParsingProblem)});
	{ok, {Proto, User, Host, Port, RemoteDir, _Query}} when Proto =:= sftp orelse Proto =:= ftpes ->
	    {ok, ChannelPid, CRef} = 
		start_channel(Proto, Host, Port, User, Password),
	    try 
		{SourceDir,Listing} = read_remote_up_dir(Proto, ChannelPid,RemoteDir),
		DN = download_up_info(Proto, ActionId, ChannelPid, SourceDir, 
				      Uri, Password, Listing),
		swmLib:write_swm_log("SwM", info, "Action create complete"),
		CompleteTime = comsaI:iso_time(os:timestamp(), extended),
		update_progress(
		  [{additionalInfo, "createUpgradePackage complete"},
		   {timeActionCompleted, CompleteTime},
		   {progressPercentage, 100},
		   {result, ?ActionResultType_SUCCESS},
		   {resultInfo, DN},
		   {state, ?ActionStateType_FINISHED}]),
		ok
	    after
		ftpI:stop_channel(Proto, ChannelPid),
		case Proto of
            sftp -> ssh:close(CRef);
            _-> ok
        end
	    end;
	{ok, {file, _, "localhost", _, RemoteDir, _}} ->
	    test_create_package(RemoteDir, ActionId, Uri, Password)
    end.

read_remote_up_dir(sftp, ChannelPid, RemoteDir) ->
    {ok, FIO} = 
	case ssh_sftp:read_file_info(ChannelPid, RemoteDir, 10000) of
	    {error, no_such_file} ->
		update_progress([{additionalInfo,
				  "No such file: " ++ RemoteDir}]),
		throw({fail, "No such file: "++RemoteDir});
            {error, timeout} ->
		update_progress([{additionalInfo,
				  "Timeout reading file info: " ++ RemoteDir}]),
		throw({fail, "Timeout reading file info: "++RemoteDir});
	    {error, permission_denied} ->
		update_progress([{additionalInfo,
				  "Permisson denied for remote file: "++RemoteDir}]),
		throw({fail, "Permisson denied for remote file: "++RemoteDir});
	    {error, Reason} ->
		update_progress([{additionalInfo,
				  "Reading remote file info failed with reason: " ++
				      Reason}]),
		throw({fail, "Reading remote file info failed with reason: " ++
			   Reason});
	    {ok, FIO1} ->  {ok, FIO1}
	end,
    {SourceDir, Listing} =
	case FIO#file_info.type of
	    directory ->
		%% HT85211
		{ok, BaseNames} = list_dir(sftp, ChannelPid, RemoteDir, 10000), 
		{RemoteDir, BaseNames};
	    regular ->
		{filename:dirname(RemoteDir), [filename:basename(RemoteDir)]}
	end,
    info_msg("Files found at remote dir: ~p~n",[Listing]),
    {SourceDir, Listing};

read_remote_up_dir(ftpes, ChannelPid, RemoteDir) ->
   {SourceDir, Listing} =
    case ftpesI:is_directory(ChannelPid, RemoteDir) of
        directory ->  
            {ok, BaseNames} = list_dir(ftpes, ChannelPid, RemoteDir, 10000), 
            {RemoteDir, BaseNames};
        file -> {filename:dirname(RemoteDir), [filename:basename(RemoteDir)]};
        {error, _Reason} -> 
            update_progress([{additionalInfo,
                  "No such file ftpes: " ++ RemoteDir}]),
        throw({fail, "No such file ftpes: "++RemoteDir})
    end,
    info_msg("Files found at remote dir: ~p~n",[Listing]),
    {SourceDir, Listing}.

test_create_package(RemoteDir, ActionId, Uri, Password) ->
    %% This clause is for testing purposes only, and assumes a 
    %% correctly formatted up
    File = filename:basename(RemoteDir),
    {ok, Data} = file:read_file(RemoteDir),
    {Xml, _} = xmerl_scan:string(binary_to_list(Data)),
    ProductData = extract_product_data(Xml),
    ArchivePath = get_up_archive_dir(ProductData),
    DN = 
	case filelib:is_dir(ArchivePath) of
	    true ->
		#'ProductData'{productName = Name,
			       productNumber = Id,
			       productRevision = Vsn} = ProductData,
		Msg = "UP "++Name++" ("++Id++" "++Vsn++
		    ") already exists",
		throw({fail, Msg});
	    false ->
		create_up(ActionId, File, Uri, Password, 
			  ProductData, ArchivePath, Data)
	end,

    swmLib:write_swm_log("SwM", info, "Action create complete"),
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),

    update_progress([{additionalInfo,
		      "createUpgradePackage complete"},
		     {timeActionCompleted, CompleteTime},
		     {progressPercentage, 100},
		     {result, ?ActionResultType_SUCCESS},
		     {resultInfo, DN},
		     {state, ?ActionStateType_FINISHED}]),

    ok.


%%% ----------------------------------------------------------
%%% #         download_up_info(ActionId, pid, SourceDir, Uri, Password, Files)
%%% Input: ActionId:integer() - Used in progress reporting
%%%        Pid:pid() - Ssh channel server
%%%        SourceDir:string() - Location of the up, one up only in each dir
%%%        Uri:string() - Compelte location of the up to be stored in MO
%%%        Password:string() - Passsword to be saved in MO
%%%        Files:[string()] - List of cxp files, or one up container file
%%% Output: DN:string()
%%% Exceptions:
%%% Description: Download up metadata and create UpgradePackage MO
%%%              Metadata is an xml file with recognizable format
%%%              Should there be a file named .cxs assume its a up container
%%%              and download the whole thing
%%% ----------------------------------------------------------

-spec download_up_info(atom(), non_neg_integer(), 
		       pid(), 
		       string(), 
		       string(), 
		       string(), 
		       [string()]) -> string().

download_up_info(Proto, ActionId, Pid, SourceDir, Uri, Password, [File|Files]) ->
    Path = filename:join(SourceDir, File),
    case string:str(lists:reverse(File), "lmx.pu-") of
	1 ->
	    %% Match towards the reversed file to avoid that a -up.xml~ is picked
	    %% Find out if this is a up metadata file
	    {ok, Data} = case ftpI:read_file(Proto, Pid, Path) of
			     {ok, D} -> {ok, D};
			     {error, no_such_file} ->
				 throw({fail, "No such file: "++Path});
                 {error, epath} ->
                 throw({fail, "No such file: "++Path});
			     {error, permission_denied} -> 
				 throw({fail, "Permission denied: "++Path});
			     {error, ReadFileReason} ->
				 EMsg = protocol_to_text(Proto) ++  "error: "++
				     make_string_flat(ReadFileReason)++
				     " for "++Path,
				 throw({fail, EMsg})
			 end,
	    {Xml, []} = xmerl_scan:string(binary_to_list(Data)),
	    case check_up_metadata(Xml) of
		ok ->
		    ProductData = extract_product_data(Xml),
		    ArchivePath = get_up_archive_dir(ProductData),
		    case filelib:is_dir(ArchivePath) of
			true ->
			    #'ProductData'{productName = Name,
					   productNumber = Id,
					   productRevision = Vsn} = ProductData,
			    Msg = "UP "++Name++" ("++Id++" "++Vsn++
				") already exists",
			    throw({fail, Msg});
			false ->
			    create_up(ActionId, File, Uri, Password, 
				      ProductData, ArchivePath, Data)
		    end;
		{error, Reason} ->

		    Msg = lists:flatten(io_lib:format(
					  "Ill-formed UP metadata file: ~s, ~s",
					  [File, Reason])),
		    warning_msg("~s~n",[Msg]),
		    swmLib:write_swm_log("SwM", error, Msg),
		    download_up_info(
		     Proto, ActionId, Pid, SourceDir, Uri, Password, Files)
	    end;
	_Else -> % Filename does not contain "-up.xml"
	    download_up_info(Proto, ActionId, Pid, SourceDir, Uri, Password, Files)
    end;
download_up_info(_,_, _, _, _, _, []) ->
    throw({fail, "Could not find an upgrade package"}).

%%% ----------------------------------------------------------
%%% @doc Checks that the given XML element appears to be that
%%% of a well-formed UP metadata file.
%%% ----------------------------------------------------------

-spec check_up_metadata(#xmlElement{}) -> ok | {error, string()}.

check_up_metadata(Xml) ->
    case Xml#xmlElement.name of
	configuration ->
	    try find_attribute(type, Xml) of
		"MSRBS-UP" ->
		    ok;
		Other ->
		    {error,
		     lists:flatten(
		       io_lib:format(
			 "bad value of 'type' attribute: ~s", [Other]))}
	    catch
		_:_ ->
		    %% Allowing metadata when type="MSRBS-UP" is missing;
		    %% this is not documented in the UP IWD and so can
		    %% be considered deprecated
		    try find_element(product, Xml) of
			UpProdE ->
			    try find_attribute(id, UpProdE) of
				Id ->
				    case Id of
					[$C, $X, $S|_] ->
					    ok;
					_ ->
					    {error, "product id not 'CXS*'"}
				    end
			    catch
				_:_ ->
				    {error, "missing 'id' attribute in 'product'"}
			    end
		    catch
			_:_ ->
			    {error, "missing 'product' element"}
		    end
	    end;
	_ ->
	    {error, "missing 'configuration' element"}
    end.
%%% ----------------------------------------------------------
%%% @doc Create a new UP. It is trusted that the new UP does not
%%% clash with existing ones (its name+id+version is unique).
%%% @end
%%% ----------------------------------------------------------

-spec create_up(ActionId::non_neg_integer(), 
		MetaFile::string(), 
		Uri::string(), 
		Password::string(), 
%%%               ConfigurationE::#xmlElement{},
		Productdata::#'ProductData'{},
		ArchivePath::string(), 
		Data::binary()) -> string().

create_up(ActionId, MetaFile, Uri, Password, 
%%%ConfigurationE, 
	  ProductData, 
	  ArchivePath, 
	  Data) ->
    Msg = "Reading UP data",
    update_progress([{additionalInfo, Msg }]),
    EnumState = state(initialized),
    EcimPassword = #'EcimPassword'{
		      password =
			  comsaI:encrypt_password(list_to_binary(Password))},
    Key = swmInventory:make_mom_key(ProductData#'ProductData'.productNumber,
				    ProductData#'ProductData'.productRevision),
    NewIndex = {"1","1","1", Key},
    CreateTime = comsaI:iso_time(os:timestamp(), extended),
    ActivationStep =
	#'ActivationStep'{serialNumber=1,
			  name="Activate",
			  description="Activates this upgrade package"},
    UP = #upgradePackage{upgradePackageId = NewIndex,
			 state = EnumState,
			 ignoreBreakPoints = true,
			 administrativeData = [ProductData],
			 activationStep = [ActivationStep],
			 created = CreateTime,
			 %% directoryPathExtension = undefined,
			 %% remoteFileStore = undefined,
			 %% isLocallyStored=false,
			 uri = Uri,
			 creatorActionId = ActionId,
			 password = EcimPassword},
    Fun = fun() -> mnesia:write(UP) end,
    case mnesia:transaction(Fun) of
	{atomic, ok} ->
	    cmd("mkdir -p "++ArchivePath),
	    ok = file:write_file(filename:join(ArchivePath, MetaFile), Data),
	    swmLib:sync(),
	    "UpgradePackage="++Key;
	{aborted, Reason} ->
	    throw(Reason)
    end.

%%% ----------------------------------------------------------
%%% #           extract_product_data()
%%% Input: ConfigurationE:#xmlElement{}
%%% Output: #'ProductData'{}
%%% Exceptions:
%%% Description: Get product data from a up metadata file
%%% ----------------------------------------------------------
-spec extract_product_data(#xmlElement{}) -> #'ProductData'{}.

extract_product_data(ConfigurationE) ->
    UpProdE = find_element(product, ConfigurationE),
    Name = find_attribute(name, UpProdE),
    Id = find_attribute(id, UpProdE),
    Version = find_attribute(version, UpProdE),
    DateE = find_element(date, ConfigurationE),
    Description = case find_optional_element(description, ConfigurationE) of
		      false -> "";
		      DescriptionE -> find_text(DescriptionE)
		  end,

    Type = case find_optional_element(type, ConfigurationE) of
	       false -> "";
	       TypeE -> find_text(TypeE)
	   end,
    #'ProductData'{productName = Name,
		   productNumber = swmInventory:format_product_number(Id),
		   productRevision = Version,
		   productionDate = find_text(DateE),
		   description = Description,
		   type = Type}.



%%% ----------------------------------------------------------
%%% #           handle_remove_package(UpDn)
%%% Input: UpDn:[binary()] Dn identifying an UpgradePackage MO
%%% Output: ok
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------


handle_remove_package(UpDn) ->
    Key = dn_to_key(UpDn),
    do_handle_remove_package(Key, "").

do_handle_remove_package(Key, ReasonForDelete) ->
    try element(4, Key) of
	PkgName ->
	    AddInfo = "Removing package " ++ PkgName ++ ReasonForDelete,
	    update_progress([{additionalInfoClear, AddInfo}])
    catch
	_ : _ ->
	    throw({fail, "Incomplete reference: " ++ Key})
    end,
    Up =
	case mnesia:dirty_read({upgradePackage, Key}) of
	    [Obj] ->
		Obj;
	    [] ->
		throw({fail, "Upgrade package does not exist"})
	end,
    case Up#upgradePackage.state of
	State when State =:= ?UpgradePackageState_INITIALIZED orelse 
		   State =:= ?UpgradePackageState_PREPARE_COMPLETED orelse 
		   State =:= ?UpgradePackageState_COMMIT_COMPLETED ->
	    Fun = fun() -> do_remove_package(Key) end,
	    case mnesia:transaction(Fun) of
		{atomic, ok} ->
		    Path = get_up_archive_dir(
			     Up#upgradePackage.administrativeData),
		    cmd("rm -rf "++Path),
		    audit_software_directly(),
		    swmLib:write_swm_log("SwM", info, 
					 "UpgradePackage="++element(4,Key)++" removed"
					 ++ ReasonForDelete),
		    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
		    update_progress([{additionalInfo, "Action complete"},
				     {progressPercentage, 100},
				     {timeActionCompleted, CompleteTime},
				     {result, ?ActionResultType_SUCCESS},
				     {state, ?ActionStateType_FINISHED}]);
		{atomic, stop} ->
		    swmLib:write_swm_log("SwM", info, "Action remove failed"),
		    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
		    update_progress([{additionalInfo, "Action complete"},
				     {progressPercentage, 100},
				     {timeActionCompleted, CompleteTime},
				     {result, ?ActionResultType_FAILURE},
				     {state, ?ActionStateType_FINISHED}])
	    end;
	State ->
	    ResultInfo = 
		"Upgrade package cannot be removed in state "++state(State),
	    swmLib:write_swm_log("SwM", info, ResultInfo),
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    update_progress([{resultInfo, ResultInfo},
			     {additionalInfo, "Action complete"},
			     {progressPercentage, 100},
			     {timeActionCompleted, CompleteTime},
			     {result, ?ActionResultType_FAILURE},
			     {state, ?ActionStateType_FINISHED}])
    end,
    ok.

get_up_archive_dir([ProductData]) ->
    get_up_archive_dir(ProductData);
get_up_archive_dir(#upgradePackage{administrativeData = AdmData}) ->
    get_up_archive_dir(AdmData);
get_up_archive_dir(#'ProductData'{productName = Name,
				  productNumber = Id,
				  productRevision = Version}) ->
    get_up_archive_dir(Name, Id, Version).

get_up_archive_dir(Name, Id, Version) ->
    filename:join(swmLib:archive_dir(),
		  Name++"_"++no_slash(Id)++"_"++Version).

no_slash(Str) ->
    [case X of
	 $/ -> $_;
	 _ -> X
     end || X<-Str].


%%% ###########################################################################
get_current_archive_dir() ->
    UP = swmLib:get_current_up_metadata(),
    Name = proplists:get_value(productName, UP),
    ProdId = proplists:get_value(productNumber, UP),
    Version = proplists:get_value(productRevision, UP),
    get_up_archive_dir(Name, ProdId, Version).

%%% ###########################################################################
%%% @doc Request from LMHI (CAT) to download SW for a boardType of
%%%   hwcategory="OTHER".
%%%
%%% @end
%%% ----------------------------------------------------------
add_board(AbState, BoardType, CbModule) ->
    SwpParams = [{boardTypesOTH, [BoardType]},
		 {options, [{global, swmLib:software_dir()}]}],
    try
	add_board(AbState,
		  swmBoardList:swp([products], SwpParams),
		  BoardType,
		  CbModule)
    catch
	ErrClass : ErrReason ->
	    StackTrace = erlang:get_stacktrace(),
	    Reason = "Software fault",
	    sysInitI:error_report([Reason,
		      {ErrClass, ErrReason},
		      {stackTrace, StackTrace}]),
	    add_board_reply(AbState, CbModule, BoardType, {error, Reason})
    end.

%%% ----------------------------------------------------------
add_board(AbState,
	  {ok, [{products, [_ | _] = Selected}]},
	  {ProdNo, ProdRev} = BoardType,
	  CbModule) ->
    Products =
	Selected ++
	swmBoardList:bundled_products(swmI:get_current_archive_dir(), Selected),
    Downloaded = swmBoardList:archiveSWPs(),
    {Installed, _} = installed_cxps(Products),
    case {swp_products_subtract(Products, Downloaded),
	  swp_products_subtract(Products, Installed)}
	of
	{[], []} ->
	    Format = "Software for ~s, ~s already downloaded and installed",
	    Info = lists:flatten(io_lib:format(Format, [ProdNo, ProdRev])),
	    log_SwmInternal(info, Info),
	    sysInitI:info_report([Info]),
	    swmLib:write_swm_log(?SwmLog_CurrUP, info, Info),
	    Sw = swmBoardList:swp_id_format(Products),
	    swmBoardList:boardTypeOth_swOk(BoardType, Sw),
	    add_board_reply(AbState, CbModule, BoardType, ok);
	{NotDownloadedProducts, NotInstalledProducts} ->
	    sysInitI:info_report([{notDownLoaded, NotDownloadedProducts},
		       {notInstalled, NotInstalledProducts}]),
	    ok = gen_server:cast(?SERVER, {add_board,
					   BoardType,
					   NotDownloadedProducts,
					   NotInstalledProducts,
					   CbModule}),
	    wait
    end;
add_board(AbState, Result, BoardType, CbModule) ->
    swmBoardList:boardTypeOth_swNok(BoardType),
    case Result of
	{ok, [{products, []}]} ->
	    add_board_reply(AbState,
			    CbModule,
			    BoardType,
			    {error, "BoardType not found in UP metadata file"});
	_ ->
	    add_board_reply(AbState,
			    CbModule,
			    BoardType,
			    Result)
    end.

%%% ###########################################################################
%%% add_board_reply
%%%
%%% ###=====================================================================###
add_board_reply(unblocked, CbModule, {ProdNo, ProdRev}, Res) ->
    CbModule:board_added(ProdNo, ProdRev, Res);
add_board_reply(_, _, _, Res) ->
    Res.

%%% ###########################################################################
%%% prepPush_appdata_single
%%%
%%% ###=====================================================================###
prepPush_appdata_single(BoardType) ->
    RootDir = swmLib:software_dir(),
    {ok, [{products, Products}]} =
	swmBoardList:swp([products], [{boardTypesOTH, [BoardType]},
				      {options, [{global, RootDir}]}]),
    [begin
	 ok = swmAppData:prepPush_appdata_single(SwpId),
	 SwpId
     end
     || SwpId <- swmBoardList:swp_id_format(Products)].

%%% ----------------------------------------------------------
%%% @doc Removes an UP. This functions runs within a mnesia
%%% transaction.
%%% @end
%%% ----------------------------------------------------------

-spec do_remove_package(tuple()) -> ok | stop.

do_remove_package(Key) ->
    Up =
	case mnesia:read({upgradePackage, Key}) of
	    [Obj] ->
		Obj;
	    [] ->
		throw({fail, "Upgrade package does not exist"})
	end,

    %% Check if it is the active version

    %% First, get the active version
    %% We're assuming that there is only one active sw version at a time
    [SwInventory] = mnesia:read({swInventory, {"1","1","1"}}),
    ActiveKey = dn_to_key(hd(SwInventory#swInventory.active)),
    [SwVersion] = mnesia:read({swVersion, ActiveKey}),
    ActiveProductData = SwVersion#swVersion.administrativeData,

    %% Then compare with UP product data
    [UpProductData] = Up#upgradePackage.administrativeData,

    case is_same_product(ActiveProductData, UpProductData) of
	true ->
	    Info =
		"This upgrade package contains active software. "
		"It cannot be removed at this time.",
	    do_update_progress(swm, [{resultInfo, Info}]),
	    info_msg("UP not removed; contains active software~n"),
	    info_msg("--- UP info:~n~p~n", [UpProductData]),
	    info_msg("--- Active UP info:~n~p~n", [ActiveProductData]),
	    info_msg("--- BootFallback UP info:~n~p~n",
		     [swmLib:get_bootfallback_up_metadata()]),
	    stop;
	false ->
	    case swmBackup:has_dependent_backups(UpProductData) of
		true -> 
						% HS62185, added case
		    Info =
			"This upgrade package is referred by a backup. "
			"It cannot be removed at this time.",
		    do_update_progress(swm, [{resultInfo, Info}]),
		    info_msg("UP not removed; referred by a backup~n"),
		    stop;
		false ->
		    mnesia:delete_object(Up),
		    ok
	    end
    end.


-spec is_same_product(#'ProductData'{},#'ProductData'{}) -> boolean(). 

is_same_product(P1, P2) ->
    P1#'ProductData'.productNumber == P2#'ProductData'.productNumber andalso
	P1#'ProductData'.productRevision == P2#'ProductData'.productRevision.

%%% ----------------------------------------------------------
%%% #           handle_remove_sw_version(VsnDn)
%%% Input: VsnDn: [binary()] - Dn identifying an SwVersionMain MO
%%% Output: ok
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

%% handle_remove_sw_version(_) ->

%%     %% Update tables
%%     swmModel:update_tables(),
%%     swmInventory:update_tables(),

%%     %% And report 
%%     CompleteTime = comsaI:iso_time(os:timestamp(), extended),
%%     update_progress([{additionalInfoClear, "This action is deprecated"},
%% 		     {progressPercentage, 100},
%% 		     {timeActionCompleted, CompleteTime},
%% 		     {result, ?ActionResultType_SUCCESS},
%% 		     {state, ?ActionStateType_FINISHED}]),
%%     ok.

%% HS51877 fix
handle_remove_software_version(VsnDn) ->
    Key = dn_to_key(VsnDn),
    try element(4, Key) of
	SwVsn ->
	    AddInfo = "Removing swVersion="++SwVsn,
	    update_progress([{additionalInfoClear, AddInfo}])
    catch _:_ ->
	    throw({fail, "Incomplete reference: "++VsnDn})
    end,

    %% Check that it isn't the active version
    Actives = [swmInventory:make_mom_key(PD#'ProductData'.productNumber,
					 PD#'ProductData'.productRevision)
	       ||PD<-swmInventory:get_current_sw_version()],

    case lists:member(element(4,Key), Actives) of
        true ->
	    info_msg("Currently running: ~p~n",[Actives]),
	    throw({fail, "Cannot remove active version "++element(4,Key)});
        false ->
            ok
    end,

    DnRev = lists:reverse([list_to_binary(X)||X<-string:tokens(VsnDn, ",=")]),
    {atomic, ExistMo} = mnesia:transaction(
			  fun() -> 
				  swmInventory:existsMo(DnRev, 0)
			  end),
    case ExistMo of
	true ->
	    %% This will remove all unused versions, but it will do for now
	    %% because there should only be one other swVersion so it will work 
	    %% as expected
	    audit_software_directly(),
	    %% Update tables
	    swmModel:update_tables(),
	    swmInventory:update_tables(no_log), %HW11604
	    swmLib:write_swm_log("SwM", info, "Software version removed"),
	    update_progress([{resultInfo, "Software version removed"}]);
	false ->
	    swmLib:write_swm_log("SwM", info, "No such software"),
	    update_progress([{resultInfo, "No such software"}])
    end,

    %% And report complete success
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress([{progressPercentage, 100},
		     {timeActionCompleted, CompleteTime},
		     {result, ?ActionResultType_SUCCESS},
		     {state, ?ActionStateType_FINISHED}]),
    ok.
%% HS51877 fix ends here


%%% ----------------------------------------------------------
%%% #           handle_prepare_package(Key)
%%% Input: Key:{string(), string(), string(), string()
%%%            A tuple which is a key in the upgradePackage table
%%% Output: ok
%%% Exceptions:
%%% Description: Downloads remaining parts of an upgrade package which
%%%              should leave the package in state PREPARE COMPLETED
%%% ----------------------------------------------------------

handle_prepare_package(Key) ->
    AddInfo = "Preparing package "++element(4,Key),
    Sender = sender(Key),
    swmLib:write_swm_log(Sender, info, "prepare"),
    update_progress(Key, [{additionalInfoClear, AddInfo}]),
    info_msg("~s~n",[AddInfo]),

    [Up] = mnesia:dirty_read({upgradePackage, Key}),
    case Up#upgradePackage.uri of
	undefined ->
	    throw({fail, "Missing value for uri"});
	_ ->
	    ok
    end,

    audit_software_directly(),

    try download_packages(Up) of
	ok ->
	    Key = Up#upgradePackage.upgradePackageId,
	    mnesia:transaction(
	      fun() -> 
		      [UP] = mnesia:read({upgradePackage, Key}),
		      NewUp = UP#upgradePackage{state=state(prepareCompleted)},
		      %% isLocallyStored=true},
		      mnesia:write(NewUp) 
	      end),
	    swmLib:write_swm_log(Sender, info, "Action prepare complete"),
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),    
	    update_progress(Key, [{additionalInfo, "Preparation complete"},
				  {timeActionCompleted, CompleteTime},
				  {progressPercentage, 100},
				  {result, ?ActionResultType_SUCCESS},
				  {state, ?ActionStateType_FINISHED}]),
	    ok
    catch
	Type : Reason ->

	    %% HS23697: We should remove all files that
	    %% were downloaded and caused this crash,
	    %% except for the UP metadata file.
	    ArchiveDir = get_up_archive_dir(
			   Up#upgradePackage.administrativeData),
	    {ok, Files} = file:list_dir(ArchiveDir),
	    files_delete(Files, ArchiveDir),
	    %% end of fix HS23697

	    NewUp = Up#upgradePackage{state=state(initialized)},
	    mnesia:transaction(fun() -> mnesia:write(NewUp) end),
	    case {Type, Reason} of
		{throw, cancelled} -> % HS37375
		    ok;
		_ ->
		    sysInitI:error_report(
		      [{?MODULE, handle_prepare_package, [Key]},
		       {mfa, {swmServer, download_packages, [Up]}},
		       {Type, Reason},
		       erlang:get_stacktrace()])
	    end,
	    erlang:Type(Reason)
    end.

%%% ###########################################################################
%%% files_delete
%%%
%%% ###=====================================================================###
files_delete([File | Tail], Dir) ->
    %% HS23697: We should remove all files that
    %% were downloaded and caused this crash,
    %% except for the UP metadata file.
    case swmLib:is_up_abspath(File) of
	true ->
	    ok;
	false ->
	    file:delete(filename:join(Dir, File))
    end,
    files_delete(Tail, Dir);
files_delete([], _) ->
    ok.

%%% ----------------------------------------------------------
%%% #           download_packages(Up)
%%% Input: Up:#upgradePackage{}
%%% Output: ok
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

download_packages(Up) ->
    RawUri = Up#upgradePackage.uri,
    Uri = http_uri:decode(RawUri), %Handle HTTP encoded URIs; HV58153
    case ftpI:parse_uri(Uri) of
	{ok, ParsedUri} when element(1, ParsedUri) == sftp orelse  element(1, ParsedUri) == ftpes->
	    download_packages(Up, ParsedUri);
	{ok, ParsedUri} when element(1, ParsedUri) == file ->
	    test_download_packages(Up, ParsedUri)
    end.

download_packages(Up, Ftp) when element(1, Ftp) == sftp orelse element(1, Ftp) == ftpes->
    ArchiveDir = get_up_archive_dir(Up#upgradePackage.administrativeData), 
    case
	swmBoardList:swp([products],
			 [{boardTypeBB, swmBoardList:boardType()},
			  {boardTypesOTH, swmBoardList:boardTypes_oth()},
			  {options, [{global, ArchiveDir}]}])
    of
	{ok, [{products, Products}]} ->
	    download_packages(Up, ArchiveDir, Products, Ftp);
	{error, Reason} ->
	    throw({uc_error, Reason})
    end.

download_packages(Up, ArchiveDir, Products, Ftp) ->
    info_msg("Initiating named download~n"),
    {Proto, User, Host, Port, _RemoteFile, _Query} = Ftp,

    Password = comsaI:decrypt_password(Up#upgradePackage.password),
    {ok, ChannelPid, CRef} = start_channel(Proto, Host, Port, User, Password),
    try	download_packages(Up, ArchiveDir, Products, Ftp, ChannelPid)
    after
	    ftpI:stop_channel(Proto, ChannelPid),
	    case Proto of
            sftp -> ssh:close(CRef);
            _-> ok
        end
    end.
	
download_packages(Up, ArchiveDir, Products, Ftp, ChannelPid) ->     

    {Proto, _User, _Host, _Port, RemoteFile, _Query} = Ftp,
    RemoteDir = get_remote_dir(Proto, ChannelPid, RemoteFile),

    %% Set up progress reporting by calculating the total size
    %% of all files in the directory

    %% FileSizes = [{{Name, ProdId, Version}, File, Path, Size}]

    FileSizesG = get_file_sizes_global(Proto, Products, ChannelPid, RemoteDir),
    FileSizesH = get_file_sizes_hal(Products),
    info_msg("File sizes, global: ~n~p~nFile sizes, hal: ~n~p~n",
	     [FileSizesG, FileSizesH]),

    TotalSize = lists:foldl(fun({_, _, _, S}, A) -> S+A end, 0, FileSizesG),
    info_msg("TotalSize, global: ~w bytes ~n", [TotalSize]),

    UpKey = Up#upgradePackage.upgradePackageId,
    {ok, Tref} = 
	timer:apply_interval(30000, ?MODULE,prepare_progress,[UpKey,TotalSize]),
    put(progress_timer, Tref),

    swmLib:set_ram_variable(download_progress, 0),

    info_msg("do_named_download/3, n. of CXP files: ~w~n",
	     [length(FileSizesG)]),

    download_package(Proto, Up, ArchiveDir, ChannelPid, FileSizesG),
    wait_internal_lock(),
    swmLib:internal_lock([?LOCKOBJ_audit_sw], upgrade),
    unpack_package(ArchiveDir, FileSizesG++FileSizesH, 
		   swmOs:get_inactive_instance()),
    timer:cancel(Tref),
    ok.

%%% ###########################################################################
%%% dwnld_pckgs_partial
%%%
%%% ###=====================================================================###
dwnld_pckgs_partial(Up, ArchiveDir, Products, Ftp) ->
    info_msg("Initiating partial download~n"),
    {Proto, User, Host, Port, _RemoteFile, _Query} = Ftp,
    
    Password = comsaI:decrypt_password(Up#upgradePackage.password),
    {ok, ChannelPid, CRef} = start_channel(Proto, Host, Port, User, Password),
    try
	dwnld_pckgs_partial(Up, ArchiveDir, Products, Ftp, ChannelPid)
    after
	ftpI:stop_channel(Proto, ChannelPid),
        case Proto of
            sftp -> ssh:close(CRef);
            _-> ok
        end
    end.

dwnld_pckgs_partial(Up, ArchiveDir, Products, Ftp, ChannelPid) ->     
    {Proto, _User, _Host, _Port, RemoteFile, _Query} = Ftp,
    RemoteDir = get_remote_dir(Proto, ChannelPid, RemoteFile),
    
    %% FileSizes = [{{Name, ProdId, Version}, File, Path, Size}]
    
    FileSizesG = get_file_sizes_global(Proto, Products, ChannelPid, RemoteDir),
    FileSizesH = get_file_sizes_hal(Products),
    info_msg("File sizes, global: ~n~p~nFile sizes, hal: ~n~p~n",
	     [FileSizesG, FileSizesH]),
    
    TotalSize = lists:foldl(fun({_, _, _, S}, A) -> S+A end, 0, FileSizesG),
    info_msg("TotalSize, global: ~w bytes ~n", [TotalSize]),
    
    info_msg("Partial download, no. of CXP files: ~w~n", [length(FileSizesG)]),
    
    dwnld_pckg_partial(Proto, Up, ArchiveDir, ChannelPid, FileSizesG),
    unpack_package(ArchiveDir, FileSizesG, swmOs:get_active_instance()),
    swmOs:make_links(ArchiveDir, swmLib:software_dir(), Products),
    swmLib:make_cxp_list(sysEnv:home_dir(), current_selected_sw()),
    ok.

%%% ###=====================================================================###
dwnld_pckg_partial(Proto, Up, ArchiveDir, ChannelPid, FileSizes) ->
    %% Sanity checks
    UpKey = Up#upgradePackage.upgradePackageId,
    NeededSize = 
	lists:foldl(fun({_, _, _, Size}, Accu) when Size > 0 ->
			    Size + Accu;
		       ({_, Basename, _, _}, _) ->
			    Msg = Basename ++ " is empty",
			    throw({fail, Msg})
		    end,
		    0,
		    FileSizes),
    FreeDisk = swmLib:get_free_disk(),
    info_msg("Free disk:  ~w bytes~n", [FreeDisk]),
    case FreeDisk of
	FreeDisk when FreeDisk > NeededSize -> 
	    ok;
	FreeDisk -> 
	    throw({fail, "Download failed due to disk space shortage. "++
		   integer_to_list(NeededSize div 1024)++" k needed. "++
		   integer_to_list(FreeDisk div 1024)++" k available"})
    end,
    SDT = erlang:system_time(micro_seconds),
    dwnld_pckg_partial(Proto, FileSizes, UpKey, ArchiveDir, ChannelPid, SDT, 0).

%%% ###---------------------------------------------------------------------###
dwnld_pckg_partial(Proto, [{_, Basename, Path, _} | Tail],
		   UpKey,
		   ArchiveDir,
		   ChannelPid,
		   SDT,
		   AccuWriteTime) ->
    case ftpI:open(Proto, ChannelPid, Path, [read, binary]) of
        {error, Reason} ->
            throw({connection_error, Reason});
        {ok, Handle} ->
            LocalPath = filename:join(ArchiveDir, Basename),
            WPid = spawn(?MODULE, file_writer, [UpKey, LocalPath, self()]),
            case chunk_download(Proto, ChannelPid, Handle, WPid) of
        	{ok, W} -> 
        	    dwnld_pckg_partial(Proto, Tail,
        			       UpKey,
        			       ArchiveDir,
        			       ChannelPid, 
        			       SDT,
        			       AccuWriteTime + W);
        	{error, Reason} ->
        	    sysInitI:error_report([{error, Reason},
        		      {path, Path},
        		      {upKey, UpKey}]),
        	    erlang:error(Reason)
            end
        end;
dwnld_pckg_partial(_, [], _, _, _, SDT, Write) ->
    EDT = erlang:system_time(micro_seconds),
    TotalDownloadTime = EDT - SDT,
    info_msg("Upgrade package partial download complete.~n"
	     "Total Download time (incl. Disk write time): ~p s~n"
	     "Disk write time: ~p s~n",
	     [TotalDownloadTime/1000000, Write/1000000]).

%%% ###########################################################################
wait_internal_lock() ->
    info_msg("Checking internal lock of 'upgrade'...~n", []),
    wait_internal_lock(swmLib:is_internal_lock(?LOCKOBJ_upgrade)).

wait_internal_lock(false) ->
    info_msg("No internal lock of 'upgrade'.~n", []),
    swmLib:internal_lock(?LOCKOBJ_inst_bootfb, upgrade);
wait_internal_lock(true) ->
    timer:sleep(100),
    wait_internal_lock(swmLib:is_internal_lock(?LOCKOBJ_upgrade)).

download_package(Proto, Up, ArchiveDir, ChannelPid, FileSizes) ->
    %% Sanity checks
    UpKey = Up#upgradePackage.upgradePackageId,
    NeededSize = 
	lists:foldl(
	  fun({_, _, _, Size}, Accu) when Size > 0 ->
		  Size+Accu;
	     ({_, Basename, _, _}, _) ->
		  Msg = Basename++" is empty",
		  update_progress(UpKey, [{additionalInfo, Msg}]),
		  throw({fail, Msg})
	  end, 0, FileSizes),
    FreeDisk = swmLib:get_free_disk(),
    info_msg("Free disk:  ~w bytes~n",[FreeDisk]),
    case FreeDisk of
	FreeDisk when FreeDisk > NeededSize -> 
	    ok;
	FreeDisk -> 
	    throw({fail, "Download failed due to disk space shortage. "++
		       integer_to_list(NeededSize div 1024)++" k needed. "++
		       integer_to_list(FreeDisk div 1024)++" k available"})
    end,
    SDT = erlang:system_time(micro_seconds),
    download_package(Proto, UpKey, ArchiveDir, ChannelPid, SDT, 0, FileSizes).

download_package(Proto, UpKey, ArchiveDir, ChannelPid, SDT, AccuWriteTime,
		 [{_, Basename, Path, _}|FileSizes]) ->
    update_progress(UpKey, [{additionalInfo,"Downloading "++Basename}]),
    case ftpI:open(Proto, ChannelPid, Path, [read, binary]) of
        {error, Reason} ->
            throw({connection_error, Reason});
        {ok, Handle} ->
            LocalPath = filename:join(ArchiveDir, Basename),
            WPid = spawn(?MODULE, file_writer, [UpKey, LocalPath, self()]),
            case chunk_download(Proto, ChannelPid, Handle, WPid) of
        	{ok, W} -> 
        	    download_package(Proto, UpKey, ArchiveDir, ChannelPid, 
        			     SDT, AccuWriteTime+W, FileSizes);
        	{error, Reason} ->
        	    sysInitI:error_report([{?MODULE, download_package},
        				   {path, Path},
        				   {upKey, UpKey}]),
        	    erlang:error(Reason)
            end
    end;
download_package(_, UpKey, _, _, SDT, Write, []) ->
    EDT = erlang:system_time(micro_seconds),
    TotalDownloadTime = EDT - SDT,
    Msg = "Upgrade package download complete. Unpacking...",
    update_progress(UpKey, [{additionalInfoClear, Msg}]),
    info_msg("Upgrade package download complete.~n"
	     "Total Download time (incl. Disk write time): ~p s~n"
	     "Disk write time: ~p s~n",
	     [TotalDownloadTime/1000000, Write/1000000]).

chunk_download(Proto, ChannelPid, Handle, WPid) ->
    %% Start with requesting 10 chunks (async). 
    No = 10,
    ftp_reads(Proto, ChannelPid, Handle, _CountNo = 1, No),
    case chunk_download_loop(Proto, ChannelPid, Handle, WPid, No) of
	{error, Reason} ->
	    {error, Reason};
	{eof, EofNo} ->
	    wait_eofs(EofNo),
	    ftpI:close(Proto, ChannelPid, Handle),
	    {ok, WriteTime} = file_close(WPid),
	    {ok, WriteTime}
    end.

chunk_download_loop(Proto, ChannelPid, Handle, WPid, No) ->
    Timeout = 
	if 
	    No < ?MAX_NUM_REQUEST ->
		?SHORT_TIMEOUT;
	    true ->
		?LONG_TIMEOUT
	end,
    
    receive 
	{async_reply, _, {ok, Data}} ->
	    WPid ! {file_data, Data},
	    garbage_collect(),
	    case ftpI:aread(Proto, ChannelPid, Handle, ?CHUNK_SIZE) of
            {error, Reason} ->
                throw({connection_error, Reason});
            {async, _} ->
                chunk_download_loop(Proto, ChannelPid, Handle, WPid, No)
        end;
	{async_reply, _N, eof} ->
	    {eof, No-1};
	{async_reply, _N, {error, Reason}} ->
	    ftpI:close(Proto, ChannelPid, Handle),
	    file_close(WPid),
	    {error, Reason};
	{cancel, _} ->
	    ftpI:close(Proto, ChannelPid, Handle),
	    file_close(WPid),
	    throw(cancelled);
	{file_error, FileError} ->
	    throw(FileError)
    after Timeout ->
	    case Timeout of
		?SHORT_TIMEOUT ->
        case ftpI:aread(Proto, ChannelPid, Handle, ?CHUNK_SIZE) of
            {error, Reason} ->
                throw({connection_error, Reason});
            {async, _} ->
                chunk_download_loop(Proto, ChannelPid, Handle, WPid, No+1)
        end;
		?LONG_TIMEOUT ->
		    {error, "timeout when fetching file"}
	    end
    end.

%%% ###########################################################################
ftp_reads(Proto, ChannelPid, Handle, CountNo, No) when CountNo =< No ->
    case ftpI:aread(Proto, ChannelPid, Handle, ?CHUNK_SIZE) of
        {error, Reason} ->
            throw({connection_error, Reason});
        {async, _} ->
            ftp_reads(Proto, ChannelPid, Handle, CountNo+1, No)
    end;
ftp_reads(_proto, _ChannelPid, _Handle, _CountNo, _No) ->
    ok.

%%% ###########################################################################
wait_eofs(_EofNo = 0) ->
    ok;
wait_eofs(EofNo) ->
    receive 
	{async_reply, _N, eof} ->
	    wait_eofs(EofNo-1);
	{async_reply, _N, {error, _Reason}} ->
	    ok;
	{async_reply, _N, {ok, _Data}} ->
	    %% Should never happen; ignore
	    wait_eofs(EofNo)
    after 5 * 60 * 1000 -> % 5 min
	    error_msg("eof was received as it should; continue~n", []),
	    nok
    end.

file_close(WPid) ->
    WPid ! {file_close, self()},
    receive 
	{file_closed, WriteTime} ->
	    {ok, WriteTime}
    after 5 * 60 * 1000 -> % 5 min
	    error_msg("File was not properly closed; exit file writer~n", []),
	    exit(WPid, kill),
	    nok
    end.

unpack_package(ArchiveDir, FileSizes, BootInstance) ->    
    Paths = [filename:join(ArchiveDir, Basename)||
		{_, Basename, _, _} <- FileSizes],
    lists:foldl(fun(Path, Backlog) -> 
			receive
			    {cancel, _} ->
				throw(cancelled)
			after 0 ->
				ok
			end,
			swmOs:mount_cxp_deferring(Path, Backlog)
		end,
		#mountBacklog{nofCxpsLeft = length(FileSizes),
			      bootInstance = BootInstance},
		Paths).


prepare_progress(UpKey, Size) ->
    Current = swmLib:get_ram_variable(download_progress),
    update_progress(UpKey, [{progressPercentage, Current*30 div Size}]).


get_remote_dir(sftp, Pid, RemoteFile) ->
    {ok, FIO} = ssh_sftp:read_file_info(Pid, RemoteFile),
    case FIO#file_info.type of
	directory ->
	    RemoteFile;
	_ ->
	    filename:dirname(RemoteFile)
    end;

get_remote_dir(ftpes, Pid, RemoteFile) ->
   case ftpesI:is_directory(Pid, RemoteFile) of
       directory -> 
           RemoteFile;
       _->
           filename:dirname(RemoteFile)
    end.
%%% ----------------------------------------------------------
get_file_sizes_global([{ProdId, {global, File}} | Tail], Dir) ->
    Path = filename:join(Dir, File),
    {ok, FileInfo} = file_info_local(Path),
    [{ProdId, File, Path, get_file_size_regular(FileInfo, Path)}
     | get_file_sizes_global(Tail, Dir)];
get_file_sizes_global([_ | Tail], Dir) ->
    get_file_sizes_global(Tail, Dir);
get_file_sizes_global([], _) ->
    [].

%%% ----------------------------------------------------------
get_file_sizes_global(Proto, [{ProdId, {global, File}} | Tail],
		      Pid,
		      RemoteDir) ->
    Path = filename:join(RemoteDir, File),
    {ok, FileInfo} = file_info_remote(Proto, Pid, Path),
    [{ProdId, File, Path, get_file_size_regular(FileInfo, Path)}
     | get_file_sizes_global(Proto, Tail, Pid, RemoteDir)];
get_file_sizes_global(Proto, [_ | Tail], Pid, RemoteDir) ->
    get_file_sizes_global(Proto, Tail, Pid, RemoteDir);
get_file_sizes_global(_, [], _, _) ->
    [].

%%% ----------------------------------------------------------
get_file_sizes_hal(Products) ->
    get_file_sizes_hal(Products, selected_hal_dir()).

get_file_sizes_hal([{_, {hal, File}} | Tail], HalDir) ->
    Path = filename:join(HalDir, File),
    {ok, FileInfo} = file_info_local(Path),
    [{Path, get_file_size_regular(FileInfo, Path)}
     | get_file_sizes_hal(Tail, HalDir)];
get_file_sizes_hal([_ | Tail], HalDir) ->
    get_file_sizes_hal(Tail, HalDir);
get_file_sizes_hal([], _) ->
    [].

%%% ----------------------------------------------------------
get_file_size_regular(#file_info{type = regular, size = Size}, _) ->
    Size;
get_file_size_regular(#file_info{type = OtherType}, Path) ->
    Reason =
	Path ++
	" is not a regular file. It is " ++
	atom_to_list(OtherType),
    throw({fail, Reason}).

%%% ----------------------------------------------------------
selected_hal_dir() ->
    SwpIds = swmBoardList:read_swp_selected(swp_id),
    case lists:keyfind(hal, 1, SwpIds) of
	{hal, HalSwpId} ->
	    filename:join(swmLib:software_hal_dir(), HalSwpId);
	false ->
	    swmLib:software_hal_dir()
    end.

%%% ----------------------------------------------------------
file_info_local(Path) ->
    case file:read_file_info(Path) of
	{ok, FIO} ->
	    {ok, FIO};
	{error, Reason} ->
	    throw({fail, make_string_flat(Reason) ++ " " ++ Path})
    end.

file_info_remote(sftp, Pid, Path) ->
    case ssh_sftp:read_file_info(Pid, Path, 100000) of
	{ok, FIO} ->
	    {ok, FIO};
	{error, Reason} ->
	    throw({fail, make_string_flat(Reason) ++ " " ++ Path})
    end;

file_info_remote(ftpes, Pid, Path) ->
     case ftpesI:is_directory(Pid, Path) of
        file-> 
            case ftpesI:size(Pid, Path) of
                {error, Reason} -> throw({fail, make_string_flat(Reason) ++ " " ++ Path});
                Size-> {ok, #file_info{type = regular, size = Size}}
             end;
        directory-> #file_info{type = directory};
        {error, Reason} -> throw({fail, make_string_flat(Reason) ++ " " ++ Path})
     end.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

handle_verify_package(Key) ->
    AddInfo = "Verifying package "++element(4,Key),
    Sender = sender(Key),
    swmLib:write_swm_log(Sender, info, "verify"),
    update_progress(Key, [{additionalInfoClear, AddInfo}]),

    %% The trigger action for verify is somewhat different than the others
    %% because it should not fail, just report warnings and concerns.
    update_progress(Key, [{additionalInfo, "Starting CS verification"}]),

    swmOs:clear(),
    [UP] = mnesia:dirty_read({upgradePackage, Key}),
    try swmOs:install_up(UP)
    catch error:{exit_code, 1} ->
	    throw({fail, "UP signature failure"});
	  error:{exit_code, 2} ->
	    throw({fail, "Failed to read package. It may be corrupt or a software error occured."})
    end,

    CbModules = swmLib:get_upg_callbacks(),
    [case has_callback(CbModule, verify_precondition) of
	 true ->
	     info_msg("Calling ~w:verify_precondition()~n",[CbModule]),
	     try apply(CbModule, verify_precondition, []) of
		 ok ->
		     ok;
		 {ok, Msg} ->
		     swmLib:write_swm_log(Sender, warning, Msg),
		     update_progress(Key, [{additionalInfo, Msg}])
	     catch T:E ->
		     sysInitI:error_report(
		       [{?MODULE, handle_verify_package, [Key]},
			{line, ?LINE},
			{mfa, {CbModule, verify_precondition, []}},
			{T,E}]),
		     Msg2 = "Action verifyPreconditions failed on a software "
			 "error",
		     swmLib:write_swm_log(Sender, error, Msg2),
		     update_progress(Key, [{additionalInfo, Msg2}])
	     end;
	 false ->
	     ok
     end||CbModule<-CbModules],

    update_progress(Key, [{additionalInfo, "CS verification complete"}]),

    update_progress(Key, [{additionalInfo, "Sending application triggers"}]),
    case appmI:call_upi("verifyPreconditions") of
	{ok, []} ->
	    swmLib:write_swm_log(Sender, info, "All verification complete"),
	    update_progress(Key, [{additionalInfo, "All verification complete"}]);
	{ok, Responses} ->
	    [begin
		 Msg4 = LmName++"("++ProdNr++"): "++Result--"\n",
		 update_progress(Key, [{additionalInfo, Msg4}]),
		 case Result of
		     "ERROR: "++_ ->
			 swmLib:write_swm_log(Sender, info, Msg4);
		     _  ->
			 swmLib:write_swm_log(Sender, info, Msg4)
		 end
	     end||{{LmName, ProdNr}, Result}<-Responses];
	{error, Reasons} ->
	    swmLib:write_swm_log(Sender, info, "Participants not ok"),
	    update_progress(Key, [{additionalInfo, "Participants not ok"}]),
	    throw(Reasons)

    end,

    swmLib:write_swm_log(Sender, info, "Action verify complete"),
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),   
    update_progress(Key, [{additionalInfo, "Verification complete"},
			  {progressPercentage, 100},
			  {timeActionCompleted, CompleteTime},
			  {result, ?ActionResultType_SUCCESS},
			  {state, ?ActionStateType_FINISHED}]),
    ok.

get_up_state(Key) ->
    Fun = fun() -> 
		  [Up] = mnesia:read({upgradePackage, Key}),
		  Up#upgradePackage.state
	  end,
    {atomic, State} = mnesia:transaction(Fun),
    State.


-spec handle_activate_package(tuple()) -> {ok, activationInProgress}.

handle_activate_package(Key) ->
    AddInfo = "Activating package "++element(4, Key),
    Sender = sender(Key),
    swmLib:write_swm_log(Sender, info, "activate"),
    update_progress(Key, [{additionalInfoClear, AddInfo},
			  {progressPercentage, 1}]),

    RevertStateFun = fun() -> do_state_change(Key, prepareCompleted) end,

    try pre_activate_package(Key)
    catch
	throw:Throw ->
	    mnesia:transaction(RevertStateFun),
	    throw(Throw);
	T:E ->
	    mnesia:transaction(RevertStateFun),
	    info_msg("Activate stopped at pre_activate_package ~p~n~p~n",
		     [{T,E}, erlang:get_stacktrace()]),
	    erlang:T(E, [Key])
    end,

    %% This part should NOT be executed within the scope of a mnesia
    %% transaction
    try do_activate_package(Key) of
	ok ->
	    {ok, activationInProgress}
    catch Type:Error ->
	    info_msg("Activate stopped at do_activate_package ~p~n~p~n",
		     [{Type,Error}, erlang:get_stacktrace()]),
	    %% Soft upgrade cleanup
	    case swmLib:get_variable(set_unpacked) of
		Vsn when Vsn /= undefined->
		    release_handler:remove_release(Vsn),
		    swmLib:erase_variable(set_unpacked);
		undefined ->
		    ok
	    end,
	    %% General cleanup
	    mnesia:transaction(RevertStateFun),
	    %% This may produce unwanted
	    %% case Error of
	    %%  {fail, _} -> 
	    %%      %% This indicates a fault by the operator = 
	    %%      %% no error report
	    %%      sysInitI:warning_report(
	    %%        [{Type, Error},
	    %%         {stacktrace, erlang:get_stacktrace()}]),
	    %%      ok;
	    %%  _ ->
	    %%      sysInitI:error_report(
	    %%        [{Type, Error}, 
	    %%         {stacktrace, erlang:get_stacktrace()}])
	    %% end,

	    erlang:Type(Error)
    end.
%% case mnesia:transaction(fun() -> do_activate_package(Up) end) of
%%  {atomic, ok} ->
%%      {ok, activationInProgress};
%%  {aborted, Reason} ->
%%      %% Soft upgrade cleanup
%%      case swmLib:get_variable(set_unpacked) of
%%      Vsn when Vsn /= undefined->
%%          release_handler:remove_release(Vsn),
%%          swmLib:erase_variable(set_unpacked);
%%      undefined ->
%%          ok
%%      end,
%%      %% General cleanup
%%      mnesia:dirty_write(
%%        Up#upgradePackage{state = state(prepareCompleted)}),
%%      throw(Reason)
%% end;

pre_activate_package(Key) ->
    %% Create the rollback backup
    %% HT47248 fix
    [UP] = swmInventory:get_current_sw_version(),
    UpStr = 
	UP#'ProductData'.productName++"_"++
	format_product_number(UP#'ProductData'.productNumber)++"_"++
	UP#'ProductData'.productRevision,
    BuName = "Rollback_backup_"++UpStr++"_"++
	comsaI:iso_time(os:timestamp(), basic),     
    %% Fix ends here
    %% This flag will be detected if the system restarts on rollback
    swmLib:set_variable(ug_failed, BuName),
    ManagerKey = {"1","1","1","1"},
    swmBackupModel:set_default_progress_report(mgr, "CREATE", 0),
    BuRef = create_backup(list_to_binary(BuName), ManagerKey, system, mgr),
    swmLib:erase_variable(ug_failed),
    {ok, MnesiaPath} = swmBackup:get_mnesia_file(BuRef),
    swmLib:activate_upgrWindow_tables(),
    update_progress(
      Key, [{additionalInfo, "Last chance to cancel without restart"}]),
    receive
	{cancel, Key} ->
	    throw(cancelled)
    after 10000 ->
	    %% HU49624 Cancel flag fix
	    {atomic, _} = 
		mnesia:transaction(
		  fun() -> swmLib:set_ram_variable(activate_cancel, reboot) 
		  end),
	    ok
    end,
    update_progress(
      Key, [{additionalInfo, "Cancel will now cause a restart"}]),

    swmLib:install_fallback(MnesiaPath), %HU15807
    swmFallbackList:pending_ug_backup(BuName),

    %% Activate the fallback timer before reboot
    %% This is actually not in accordance with ECIM, but we want
    %% some kind of time limited action and this is how we do it.

    [SwM] = mnesia:dirty_read({swM, {"1","1","1"}}),
    FallbackTimeout = SwM#swM.fallbackTimer,
    {ok, CountdownTimer} =
	timer:apply_interval(1000, swmServer, countdown,
			     [FallbackTimeout, os:timestamp()]),
    {ok, FallbackTimer} =
	timer:apply_after(FallbackTimeout*1000, swmServer,fallback,
			  [Key,
			   activating_before_restart,
			   "Activate timeout"]),
    put(countdown_timer, CountdownTimer),
    put(fallback_timer, FallbackTimer),
    info_msg("Fallback timer started. Fallback in ~w seconds ~n",
	     [FallbackTimeout]).

format_product_number(ProductNumber) ->
    [case X of
         $_ -> $/;
         _ -> X
     end||X<-ProductNumber].



-spec do_activate_package(tuple()) -> ok.

do_activate_package(Key) ->
    [UP] = mnesia:dirty_read({upgradePackage, Key}),
    %% Remember what are doing in case of restart
    swmLib:set_variable(activating_up, UP),
    Sender = sender(Key),

    mount_homes(),

    swmOs:clear(),

    Key = UP#upgradePackage.upgradePackageId,
    %% Progress percentage is based on measurements of time elapsed in TCU
    %% Just keep it rising in some fashion
    update_progress(Key, [{additionalInfo, "Installing upgrade package"},
			  {progressPercentage, 4}]),

    %% Prepare to generate necessary files

    swmOs:install_up(UP),

    sysInitI:restart_logger_trace(?MODULE, ?LINE, "upgrade: new UP installed"),
    update_progress(
      Key, [{additionalInfo, "Software installed. Backing up data."},
	    {progressPercentage, 42}]),

    %% HU29568
    %% New upgrade_init handling
    %% Remove the soft link solution and use a compressed backup on 
    %% OtherHomeDir instead

    OtherHomeDir = swmOs:home_dir_other(),
    UgDb = filename:join(OtherHomeDir, "upgrade_init"),
    TmpFile = filename:join(sysEnv:tmp_dir(), "upgrade_init"),
    TmpFileGz = TmpFile++".gz",

    swmLib:activate_upgrWindow_tables(),
    swmLib:set_variable(upgrade, true),
    ok = swmLib:mnesia_backup(TmpFile),
    swmLib:erase_variable(upgrade),

    {0, _} = swmOs:cmdres(["cd ",sysEnv:tmp_dir(), " ; gzip ",TmpFile]),
    {0, _} = swmOs:cmdres(["mv -f ",TmpFileGz, " ", UgDb]),

    update_progress(Key, [{additionalInfo, "Database backup complete"},
			  {progressPercentage, 43}]),

    find_new_paths(OtherHomeDir),
    %% HS23704 fix
    send_command(Key, verify_upgrade, "", verify_upgrade_failed, Sender), 
    %% Fix ends here

    send_command(Key, activate_start, "", reboot, Sender),

    make_cs_preload(OtherHomeDir),

    send_command(Key, preload, "", reboot, Sender),
    try ets:delete(swmNewPaths)
    catch _:_ ->
	    ok
    end,

    send_command(Key, activate, "", reboot, Sender),

    swmLib:internal_lock([?LOCKOBJ_inst_bootfb, ?LOCKOBJ_audit_sw], upgrade),
    case {swmLib:get_variable(upgrade_type), sysEnv:rcs_mode_2()} of
	{Type, simulated} when Type==reboot; Type==undefined ->
	    update_progress(Key,[{additionalInfo, "Reboot upgrade simulated"}]),
	    swmOs:preload(),
	    swmOs:activate(),
	    %% The "Switching over" message is used by node test cases
	    %% Don't change it
	    update_progress(Key, [{additionalInfo, "Switching over"}]),
	    unmount_homes(),
	    swmLib:order_restart_node(cold, ?ALH_TAG_UpgradeNormal);
	{Type, target} when Type==reboot; Type==undefined ->
	    update_progress(Key, [{additionalInfo, "Reboot upgrade"}]),
	    update_progress(Key, [{additionalInfo, "Loading OS"},
				  {progressPercentage, 43}]),
	    swmOs:preload(),
	    update_progress(Key, [{additionalInfo, "Activating"},
				  {progressPercentage, 50}]),
	    swmOs:activate(),

	    %% The "Switching over" message is used by node test cases
	    %% Don't change it
	    update_progress(Key, [{additionalInfo, "Switching over"},
				  {progressPercentage, 56}]),
	    unmount_homes(),
	    swmLib:sync(),
	    %% run_fsck(),
	    swmLib:order_restart_node(cold, ?ALH_TAG_UpgradeNormal)
	    %% {soft, simulated} ->
	    %%     update_progress_dirty([{additionalInfo, "Soft upgrade simulated"}]),
	    %%     %% Clear the release handler of garbage
	    %%     [begin
	    %%   release_handler:remove_release(Vsn),
	    %%   cmd("rm -rf "++sysEnv:releases_vsn_dir(Vsn))
	    %%      end||
	    %%  {_, Vsn, _, State} <-release_handler:which_releases(),
	    %%  State /= permanent],

	    %%     %% Install the backup
	    %%     mnesia:install_fallback(MnesiaPath),


	    %%     %% Create a run file for make_release.escript using the new OTP
	    %%     UgPatches = filename:join(swmLib:swm_dir(), "ug_patches"),
	    %%     MrDevPattern = filename:join(UgPatches, "make_release.escript"),
	    %%     filelib:ensure_dir(MrDevPattern),
	    %%     [MakeRelease] =
	    %%  case filelib:wildcard(MrDevPattern) of
	    %%      [] ->
	    %%      MrPattern = filename:join(
	    %%                [SwVersionDir, "*CXP*", "*CXC*",
	    %%                 "sys-*", "priv", "bin",
	    %%                 "make_release.escript"]),
	    %%      filelib:wildcard(MrPattern);
	    %%      Dev ->
	    %%      Dev
	    %%  end,

	    %%     %% TODO: The use of OtpDir is problematic here. It should be the
	    %%     %% the path to an unpacked OTP tree, but with the RBS CXP from the
	    %%     %% CI web it will be the atom 'undefined'. This is so because the
	    %%     %% code unpack_cxp_finalize/3 expects to find a tar.gz OTP package
	    %%     %% which does not happen (the package appears to be expanded
	    %%     %% already when unpack_cxp_finalize/3 executes).
	    %%     Cmd = "export OTP_ROOT="++OtpDir++"\n"++
	    %%  "export PATH=${OTP_ROOT}/bin:${PATH}\n"++
	    %%  MakeRelease++" "++SwVersionDir++" "++OtpDir++" "++OtherHomeDir++
	    %%  " upgrade\n",
	    %%     ExePath = filename:join(OtherHomeDir, "run_make_rel"),
	    %%     ok = file:write_file(ExePath, list_to_binary(Cmd)),
	    %%     cmd("chmod u+x "++ExePath++" && "++ExePath),

	    %%     %% Install other ug dev patches
	    %%     DevPatches = filename:join(OtherHomeDir, "dev_patches"),
	    %%     cmd("cp -R "++UgPatches++"/* "++DevPatches),

	    %%     %% Check if the make_release.escript program finished. There
	    %%     %% should be a data file
	    %%     UgCompletePath =
	    %%  filename:join(sysEnv:home_dir_other(), "make_release_complete"),
	    %%     case file:consult(UgCompletePath) of
	    %%  {ok, Data} ->
	    %%      [{relpath, RelPath},{appdirs, AppDirs}] = Data,
	    %%      update_progress_dirty([{additionalInfo, "New release created"}]),
	    %%      run_upgrade(RelPath, AppDirs);
	    %%  {error, Reason} ->
	    %%      error_msg("Cannot read ~p: ~p~n",[UgCompletePath, Reason]),
	    %%      throw(make_release_failed)
	    %%     end
    end,
    ok.

mount_homes() ->
    case filelib:wildcard("/software/RCSEE*")  of
	[] -> % classic
	    ok;
	_ ->
	    swmOs:homes(mount)
    end.

unmount_homes() ->
    case filelib:wildcard("/software/RCSEE*") of
	[] -> % classic
	    ok;
	_ ->
	    swmOs:homes(umount)
    end.

make_cs_preload(OtherHomeDir) ->
    swmAppData:push_appdata_upgrade(OtherHomeDir),
    %% swmUpgradeCb:make_releases(OtherHomeDir),
    ok.


%%% ----------------------------------------------------------
%%% @doc Returns a trigger string for APPM and a human-readable
%%% string for use as MO attribute values.
%%% @end
%%% ----------------------------------------------------------

-spec trigger(atom()) -> {string(), string()}.

trigger(verify_upgrade) -> {"verifyUpgrade", "verifyUpgrade"};
trigger(activate_start) -> {"activateStart", "activateStart"};
trigger(preload) ->  {"preload", "preload"};
trigger(activate) -> {"activate", "activate"};
trigger(confirm) -> {"commit", "confirm"}.


-spec send_command(
	Key::swm|{string(), string(), string(), string()},
	Action::verify_upgrade|activate_start|preload|activate|confirm,
	Args::string(),
	FailAction::reboot|confirm_failed|verify_upgrade_failed,
	Sender::string()) -> any().

send_command(Key, Action, _Args, FailAction, Sender) ->
    {AppmIAction, OperatorAction} = trigger(Action),
    CbModules = swmLib:get_upg_callbacks(),
    [case has_callback(CbModule, Action) of
	 true ->
	     info_msg("Calling ~w:~w()~n",[CbModule, Action]),
	     try apply(CbModule, Action, []) of
		 ok ->
		     ok;
		 {ok, Msg} ->
		     swmLib:write_swm_log(Sender, info, Msg),
		     case Action of
			 confirm ->  ok;
			 _ ->
			     update_progress(Key, [{additionalInfo, Msg}])
		     end;

		 {error, Msg} ->
		     swmLib:write_swm_log(Sender, error, Msg),
		     case Action of
			 confirm -> ok;
			 _ ->
			     update_progress(Key, [{additionalInfo, Msg}])
		     end,
		     throw(FailAction)
	     catch 
		 T:E ->
		     sysInitI:error_report(
		       [{?MODULE, send_command, [Action]},
			{line, ?LINE},
			{mfa, {CbModule, Action, []}},
			{T,E}]),
		     Msg3 = "Action "++OperatorAction++
			 " failed on a software error",
		     swmLib:write_swm_log(Sender, error, Msg3),
		     update_progress(Key, [{additionalInfo, Msg3}])
	     end;
	 false ->
	     ok
     end
     || CbModule <- CbModules],

    case Action of
	confirm -> ok;
	_ ->
	    update_progress(
	      Key, [{additionalInfo, "CS "++OperatorAction++" complete"}]),

	    update_progress(
	      Key, [{additionalInfo, 
		     "Sending application trigger "++OperatorAction}])
    end,
    %% Phase = case Args of
    %%      ""-> AppmIAction;
    %%      _ -> AppmIAction++" "++Args
    %%      end,
    case appmI:call_upi(AppmIAction) of
	{ok, []} ->
	    swmLib:write_swm_log(Sender, info, "All participants ok"),
	    case Action of
		confirm -> ok;
		_ ->
		    update_progress(
		      Key, [{additionalInfo, "All participants ok"}])
	    end;
	{ok, Responses} ->
	    [begin
		 Msg4 = LmName++"("++ProdNr++"): "++Result--"\n",
		 update_progress(Key, [{additionalInfo, Msg4}]),
		 case Result of
		     "ERROR: "++_ ->
			 swmLib:write_swm_log(Sender, error, Msg4),
			 throw(FailAction);
		     _  ->
			 swmLib:write_swm_log(Sender, info, Msg4),
			 ok
		 end
	     end||{{LmName, ProdNr}, Result}<-Responses];
	{error, Reasons} ->
	    swmLib:write_swm_log(Sender, error, "Participants not ok"),
	    case Action of
		confirm -> ok;
		_ -> 
		    update_progress(
		      Key, [{additionalInfo, "Participants not ok"}])
	    end,
	    throw(Reasons)

    end.

%%% ----------------------------------------------------------
%%% @doc Wraps an erlang:function_exported/3 call, providing
%%% zero for the arity argument and ensuring that the referred
%%% module is also loaded.
%%% @end
%%% ----------------------------------------------------------

-spec has_callback(module(), atom()) -> boolean().

has_callback(Module, Action) ->
    case code:ensure_loaded(Module) of
	{error, Reason} ->
	    sysInitI:error_report(
	      [{mfa, {code, ensure_loaded, [Module, Action]}}, 
	       {reason, Reason}]),
	    erlang:error(cannot_load_module, [Module, Action]);
	_ ->
	    erlang:function_exported(Module, Action, 0)
    end.

find_new_paths(HomeOther) ->
    ets:new(swmNewPaths, [set, public, named_table]),
    Pattern = filename:join([HomeOther, "software", "*", "cxp*.xml"]),
    CxpFiles = filelib:wildcard(Pattern),
    [register_paths(CxpFile)||CxpFile<-CxpFiles],
    ok.

register_paths(CxpFile) ->
    CxpPath = filename:join(swmLib:squash_fs_dir(),
			    filename:basename(filename:dirname(CxpFile))),
    {ConfigurationE, _} = xmerl_scan:file(CxpFile, []),
    ContentInfoE = find_element(contentinfo, ConfigurationE),
    [begin
	 ProdId = find_attribute(id, ProductE),
	 ets:insert_new(swmNewPaths, {ProdId, CxpPath})
     end||ProductE<-ContentInfoE#xmlElement.content,
	  ProductE#xmlElement.name == product],
    ok.

get_new_cxp_path(LmId) ->
    case ets:lookup(swmNewPaths, LmId) of
	[{_, Path}] -> Path;
	[] -> erlang:error(unknown_lm_id,[LmId])
    end.


%% run_upgrade(RelFile, AppDirs) ->

%%     NewVsn =
%%  case release_handler:set_unpacked(RelFile, AppDirs) of
%%      {ok, Vsn} ->
%%      swmLib:set_variable(set_unpacked, Vsn),
%%      Vsn;
%%      {error, Reason} ->
%%      sysInitI:error_report(
%%        [{mfa, {release_handler, set_upnacked, [RelFile, AppDirs]}},
%%         {error, Reason}]),
%%      erlang:error(Reason, [])
%%  end,
%%     case release_handler:check_install_release(NewVsn, [purge]) of
%%  {ok, _OtherVsn, _Descr} ->
%%      ok;
%%  {error, Reason2} ->
%%      sysInitI:warning_report(
%%        [{mfa, {release_handler, check_install_release, [NewVsn, purge]}},
%%         {error, Reason2}])
%%     end,
%%     Opts = [{error_action, reboot},
%%      {code_change_timeout, 10000},
%%      {suspend_timeout, 10000},
%%      {update_paths, true}],
%%     spawn(release_handler, install_release, [NewVsn, Opts]).


%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

handle_activation_complete() -> 
    UP = swmLib:get_variable(activating_up),
    Key = UP#upgradePackage.upgradePackageId,

    update_progress(Key, [{additionalInfo, "Finishing activation"},
			  {progressPercentage, 99}]),

    Fun = fun() -> do_activation_complete(Key) end,
    case mnesia:transaction(Fun) of
	{atomic, ok} ->
	    %% HT12440
	    ok = swmLib:unlock_action_capable(?ACTIVATE_UP_ACTION_CAPABLE_ID),
	    swmLib:write_swm_log(sender(Key), info, "Activation complete"),
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),       
	    update_progress(Key, [{additionalInfo, "Activation complete"},
				  {timeActionCompleted, CompleteTime},
				  {progressPercentage, 100},
				  {result, ?ActionResultType_SUCCESS},
				  {state, ?ActionStateType_FINISHED}]),
	    try post_activate() of
		{ok, Result} -> {ok, Result}
	    catch T:E ->
		    sysInitI:error_report(
		      [{T,E}, erlang:get_stacktrace()]),
		    handle_fail(Key, "Post activation procedure failed", 
				undefined),
		    swmLib:restart_node(cold, ?ALH_TAG_UpgradeFailure),
		    {T,E}
	    end;

	Reason ->
	    sysInitI:error_report([Reason, erlang:get_stacktrace()]),
	    handle_fail(Key, "Activation failed", undefined),
	    swmLib:restart_node(cold, ?ALH_TAG_UpgradeFailure),
	    Reason
    end.

do_activation_complete(Key) ->
    UP =
	case mnesia:read(upgradePackage, Key) of
	    [Obj]  ->
		Obj;
	    _ ->
		swmLib:get_variable(activating_up)
	end,
    mnesia:write(UP#upgradePackage{state = state(waitingForCommit)}).

post_activate() ->
    Result = post_activate_timers(sysEnv:rcs_mode_2()),
    erase_upgrade_marker(),
    swmDbMonitor:force_auto_backup(),
    {ok, Result}.

post_activate_timers(Target) when Target /= vrcs ->
    UP = swmLib:get_variable(activating_up),
    Key = UP#upgradePackage.upgradePackageId,
    [SwM] = mnesia:dirty_read({swM, {"1","1","1"}}),
    FallbackTimeout = SwM#swM.fallbackTimer,
    {ok, CountdownTimer} = 
	timer:apply_interval(1000, swmServer, countdown,
			     [FallbackTimeout, os:timestamp()]),
    {ok, FallbackTimer} =
	timer:apply_after(FallbackTimeout*1000, swmServer, fallback,
			  [Key,
			   waiting_for_confirm,
			   "Confirm timeout"]),
    {ok, AlarmTimer} =
	case FallbackTimeout of
	    FallbackTimeout when FallbackTimeout < 300 ->
		send_fallback_alarm(FallbackTimeout, Key),
		undefined;
	    _ ->
		timer:apply_after(1000*(FallbackTimeout-300),
				  swmServer, send_fallback_alarm, [300, Key])
	end,
    
    info_msg("resend timer"),
    {ok, ResendTimer} = 
	case swmLib:get_variable(resend_activation_complete) of
	    disabled -> 
		info_msg("resend timer disabled"),
		{ok, undefined};
	    _ -> 
		info_msg("resend timer set"),
		timer:send_after(60000, {resend_timer_expired, 60000})
	end,

    {CountdownTimer, FallbackTimer, AlarmTimer, ResendTimer};
post_activate_timers(_) ->
    {undefined, undefined, undefined, undefined}.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

handle_mnesia_table_event({write, swVersion, Obj, _, _}=E) ->
    case dets:insert(swVersion, Obj) of
	ok -> ok;
	{error, Reason} ->
	    sysInitI:error_report(
	      [{?MODULE, handle_mnesia_table_event, [E]},
	       {mfa, {dets, insert, [swVersion, Obj]}},
	       {error, Reason}])
    end;
handle_mnesia_table_event({delete, swVersion, _, _, _}) ->
    ok;

handle_mnesia_table_event(_) ->
    ok.

%%% ----------------------------------------------------------
%%% @doc Maps the given DN to a tuple containing the RDN values
%%% only. For example, ManagedElement=1,Chimney=3 would map to
%%% {"1", "3"}.
%%%
%%% TODO: No caller passes a binarized string as input? Or is it
%%% just dialyzer that does not detect it?
%%% @end
%%% ----------------------------------------------------------

-spec dn_to_key(string()|binary()) -> tuple().

dn_to_key(DnBin) when is_binary(DnBin) ->
    dn_to_key(binary_to_list(DnBin));

dn_to_key(Dn) when is_list(Dn) ->
    Parts = string:tokens(Dn, ","),
    KeyList =
	[begin
	     [_, KeyPart] = string:tokens(Part, "="),
	     KeyPart
	 end||Part<-Parts],
    list_to_tuple(KeyList).

%%% ----------------------------------------------------------
%%% #           action_handler(Fun)
%%% Input: Fun:fun() - Action to be called under supervision
%%% Output:
%%% Exceptions:
%%% Description: Performs the Fun and catches all errors with proper progress
%%%              reporting. It should also close any open sftp connections and
%%%              do whatever clean up that's necessary. In terms of error
%%%              reporting, that is a section that can be much developed.
%%% ----------------------------------------------------------

action_handler(Fun) ->
    action_handler(Fun, swm).

action_handler(Fun, Key) ->
    NoSuccess = "The action could not be completed",
    SwError = "A software related error occured",
    try Fun() of
	ok -> ok;
	{ok, Result} -> {ok, Result}
    catch
	throw : cancelled ->
	    swmLib:write_swm_log(sender(Key), info, "Operation cancelled"),
	    handle_cancelled(Key);
	throw : Reason when Reason =:= verify_upgrade_failed ->
	    %% HS62261: This is considered a non-CS fault.
	    Msg = "UP verification failed",
	    swmLib:write_swm_log(sender(Key), error, Msg),
	    warning_msg("throw ~w~n", [Reason]),
	    handle_fail(Key, NoSuccess, Reason);
	throw : Reason when Reason =:= confirm_failed ->
	    %% Unlikely to happen; enforce restart.
	    swmLib:write_swm_log(sender(Key), error, make_string_flat(Reason)),
	    error_msg("throw ~w~n", [Reason]),
	    handle_fail(Key, NoSuccess, Reason);
	throw : {fail, Reason} ->
	    %% Not a SW error. Command failed.
	    swmLib:write_swm_log(sender(Key), error, make_string_flat(Reason)),
	    warning_msg("External reason: ~s~n",[make_string_flat(Reason)]),
	    handle_fail(Key, NoSuccess, Reason);
	throw : {uc_error, Reason} ->
	    %% Not a SW error. Command failed.
	    swmLib:write_swm_log(sender(Key), error, make_string_flat(Reason)),
	    handle_fail(Key, NoSuccess, Reason);
    throw : {connection_error, Reason} ->
        swmLib:write_swm_log(sender(Key), error, "Connection error - " ++ make_string_flat(Reason)),
        warning_msg("throw ~p~n", [Reason]),
        handle_fail(Key, NoSuccess, Reason);
	throw : Reason ->
	    swmLib:write_swm_log(sender(Key), error, make_string_flat(Reason)),
	    error_msg("throw ~p~n", [Reason]),
	    handle_fail(Key, NoSuccess, Reason);
	Type : Reason ->
	    swmLib:write_swm_log(sender(Key), error, "Software error"),
	    sysInitI:error_report([{Type, Reason},
				   {stacktrace, erlang:get_stacktrace()}]),
	    handle_fail(Key, NoSuccess, SwError)
    end.

sender(swm) ->
    "SwM";
sender({"1","1","1"}) ->
    "SwM";
sender({_, _, _, UpId}) ->
    "UP="++UpId.


%%% ----------------------------------------------------------
%%% #           handle_fail(ProgressInfo)
%%% Input: ProgressInfo:string()
%%% Output:
%%% Exceptions:
%%% Description: This function reports actions that have failed
%%% ----------------------------------------------------------

-record(cleanupContext, {action}).

handle_fail(Key, ProgressInfo, Reason) ->
    Msg = make_string_flat(Reason),
    swmLib:write_swm_log(sender(Key), info, Msg),
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(Key, [{result, ?ActionResultType_FAILURE},
			  {resultInfo, Msg},
			  {progressInfo, ProgressInfo},
			  {progressPercentage, 100},
			  {state, ?ActionStateType_FINISHED},
			  {timeActionCompleted, CompleteTime}]),
    cleanup(#cleanupContext{action=Reason}, ?ALH_TAG_UpgradeFailure).

%%% ----------------------------------------------------------
%%% #           handle_cancelled()
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: This function reports actions that have been cancelled
%%% ----------------------------------------------------------

handle_cancelled(Key) ->
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(Key, [{result, ?ActionResultType_FAILURE}, % HS76784
			  {state, ?ActionStateType_CANCELLED},
			  {progressPercentage, 100},
			  {timeActionCompleted, CompleteTime}]),
    cleanup(#cleanupContext{}, ?ALH_TAG_UpgradeCancelled).


%%% ----------------------------------------------------------
actionId_init() ->
    swmLib:init_action_id(ug).

%% ets:new(?MODULE, [named_table, public, ordered_set, {keypos, 1}]),
%% %% Start at a quasi random place in the number series, but never at 0
%% {_, X, _} = os:timestamp(),
%% ets:insert(?MODULE, {actionId, (X rem 65534)+1}).

%%% ----------------------------------------------------------
actionId_new() ->
    swmLib:get_new_action_id(ug).
%% try ets:update_counter(?MODULE, actionId, 1)
%% catch
%%  _ : _ ->
%%      0
%% end.

%%% ----------------------------------------------------------
%% actionId_get() ->
%%     ets:update_counter(?MODULE, actionId, 0).

%%% ----------------------------------------------------------
%%% #           cleanup()
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: This function cleans up anything that needs cleaning up
%%% ----------------------------------------------------------

cleanup(#cleanupContext{action=Action}, Cause) ->
    case get(progress_timer) of
	undefined -> ok;
	Tref -> timer:cancel(Tref)
    end,

    case get(countdown_timer) of
	undefined -> ok;
	TimerRef ->
	    timer:cancel(TimerRef)
    end,
    try update_count(-1)
    catch _:_ -> ok
    end,
    try ets:delete(swmNewPaths) 
    catch _:_ -> ok
    end,
    %% HS51856
    try swmBackup:internal_housekeeping()
    catch T:E ->
	    sysInitI:error_report(
	      [{mfa, {swmBackup, internal_housekeeping, []}},
	       {T,E},
	       erlang:get_stacktrace()]),
	    ok
    end,
    %% HS51856 ends here
    case get(reboot_ordered) of   % HW25300
	true ->
	    ok;
	_ ->
	    %% End of HW25300
	    swmLib:internal_lock_remove([?LOCKOBJ_inst_bootfb,
					 ?LOCKOBJ_audit_sw],
					upgrade)
    end,
    case get(fallback_timer) of
	undefined ->
	    ok;
%%% HS23704 fix 
	FallbackTimer when Action == verify_upgrade_failed ->
	    timer:cancel(FallbackTimer);    
%%% HS23704 fix ends here
	_ ->
	    swmLib:restart_node(cold, Cause)
    end,

    if
	Action =:= reboot ->
	    swmLib:restart_node(cold, Cause);
	Action =:= confirm_failed ->
	    swmLib:restart_node(cold, Cause);
	true->
	    swmLib:uninstall_fallback(),
	    %% ActiveKey = 
	    swmModel:update_tables(),
	    %% swmModel:set_active_version(ActiveKey),
	    swmInventory:update_tables(no_log), %HW11604
	    %% swmInventory:set_active_version(ActiveKey),
	    %% HS87470
	    swmLib:erase_ram_variable(activationStarted),
	    swmLib:internal_lock_remove(?LOCKOBJ_inst_bootfb, upgrade),
	    swmFallbackList:audit_bootfallback()
    end.

%%% ----------------------------------------------------------
set_action_capable_info(Msg) ->
    Fun = fun() ->
		  case mnesia:wread({swM, {"1","1","1"}}) of
		      [SwM] ->
			  mnesia:write(SwM#swM{actionCapableInfo = Msg});
		      _ ->
			  ok
		  end
	  end,
    mnesia:transaction(Fun).


    


%%% ----------------------------------------------------------
%%% @doc Update progress info. The given keys are attribute
%%% names of the SwM MO. Some attributes are handled specially.
%%% The timeOfLastStatusUpdate attribute is updated automatically
%%% on each invocation.
%%% @end
%%% ----------------------------------------------------------

-spec update_progress([{atom(), string()|integer()}]) -> {atomic, ok}.

update_progress(ProgressData) ->
    %% info_msg("Update progress: ~n~p~n",[ProgressData]),
    mnesia:transaction(fun() -> do_update_progress(swm, ProgressData) end).

update_progress(Key, ProgressData) ->
    mnesia:transaction(fun() -> do_update_progress(Key, ProgressData) end).

do_update_progress(swm, ProgressData) ->
    [Obj] = mnesia:read({swM, {"1","1","1"}}),
    Old = case Obj#swM.reportProgress of
	      undefined ->
		  warning_msg("No default progress report!~n"),
		  default_progress_report("swm", 0);
	      PR -> PR
	  end,
    Progress = swmLib:update(ProgressData, Old),
    mnesia:write(Obj#swM{reportProgress=Progress});

do_update_progress(Key, ProgressData) ->
    [Obj] = mnesia:read({upgradePackage, Key}),
    Old = case Obj#upgradePackage.reportProgress of
	      undefined ->
		  default_up_progress_report("up", 1);
	      PR -> PR
	  end,
    Progress = swmLib:update_up(ProgressData, Old),
    mnesia:write(Obj#upgradePackage{reportProgress=Progress}).



set_default_progress_report(Action, Id) ->
    mnesia:transaction(fun() ->
			       [Obj] = mnesia:read({swM, {"1","1","1"}}),
			       Progress = default_progress_report(Action, Id),
			       mnesia:write(Obj#swM{reportProgress=Progress})
		       end).

set_default_up_progress_report(Key, Action, Id) ->
    mnesia:transaction(
      fun() ->
	      [Obj] = mnesia:read({upgradePackage, Key}),
	      Progress = default_up_progress_report(Action, Id),
	      mnesia:write(Obj#upgradePackage{reportProgress=Progress})
      end).


default_progress_report(Action, Id) ->
    #'AsyncActionProgress'
	{actionName=Action,
	 additionalInfo=[""],
	 progressInfo = "Action started",
	 progressPercentage=0,
	 result=?ActionResultType_NOT_AVAILABLE,
	 resultInfo="",
	 state=?ActionStateType_RUNNING,
	 actionId=Id,
	 timeActionStarted = comsaI:iso_time(os:timestamp(), extended),
	 timeActionCompleted= "",
	 timeOfLastStatusUpdate= comsaI:iso_time(os:timestamp(), extended)}.
default_up_progress_report(Action, Id) ->
    #'AsyncActionProgressWithSteps'
	{actionName=Action,
	 additionalInfo=[""],
	 progressInfo = "Action started",
	 progressPercentage=0,
	 result=?ActionResultType_NOT_AVAILABLE,
	 resultInfo="",
	 state=?ActionStateType_RUNNING,
	 actionId=Id,
	 step=0,
	 stepProgressPercentage=0,
	 timeActionStarted = comsaI:iso_time(os:timestamp(), extended),
	 timeActionCompleted= "",
	 timeOfLastStatusUpdate= comsaI:iso_time(os:timestamp(), extended)}.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

open_dets(Name, Args) ->
    case dets:open_file(Name, [{auto_save,0}|Args]) of
	{ok, Name} ->
	    {ok, Name};
	{error, Reason} ->
	    sysInitI:error_report(
	      [{?MODULE, open_dets},
	       {mfa, {dets, open_file, [Name, Args]}},
	       {error, Reason}])
    end.

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
ets_new(TableName, Opts) ->
    case whereis(?MODULE) of
	Pid when Pid == self() ->
	    catch ets:new(TableName, Opts),
	    ok;
	Pid when is_pid(Pid) ->
	    HeirData =
		[{tblName, TableName}, {tblOpts, Opts} | process_info(self())],
	    catch ets:new(TableName, Opts ++ [{heir, Pid, HeirData}]),
	    ok;
	undefined ->
	    {error, server_process_not_found}
    end.

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
get_sign_cert_timer() ->
    gen_server:call(?MODULE, get_sign_cert).

confirm_sign_cert() ->
    ok = gen_server:call(?MODULE, confirm_sign_cert).

change_sign_cert_timer(default) ->
    gen_server:call(?MODULE, {change_sign_cert, ?SIGNING_CERT_TIMER_SEC});
change_sign_cert_timer(Timeout) when Timeout>-1, 
				     Timeout<15 ->
    gen_server:call(?MODULE, {change_sign_cert, Timeout*24*60*60});
change_sign_cert_timer(_Timeout) ->
    nok_outofrange.

signing_cert_timer(TimeoutInSec) ->
    gen_server:call(?MODULE, {change_sign_cert, TimeoutInSec}).

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

-spec state(atom()|integer()) -> integer()|string().

state(initialized) -> ?UpgradePackageState_INITIALIZED;
state(prepareInProgress) -> ?UpgradePackageState_PREPARE_IN_PROGRESS;
state(prepareCompleted) -> ?UpgradePackageState_PREPARE_COMPLETED;
state(activationInProgress) -> ?UpgradePackageState_ACTIVATION_IN_PROGRESS;
state(activationStepCompleted) ->
    ?UpgradePackageState_ACTIVATION_STEP_COMPLETED;
state(waitingForCommit) -> ?UpgradePackageState_WAITING_FOR_COMMIT;
state(commitCompleted) -> ?UpgradePackageState_COMMIT_COMPLETED;
state(deactivationInProgress) -> ?UpgradePackageState_DEACTIVATION_IN_PROGRESS;

state(?UpgradePackageState_INITIALIZED) -> "INITIALIZED";
state(?UpgradePackageState_PREPARE_IN_PROGRESS) -> "PREPARE_IN_PROGRESS";
state(?UpgradePackageState_PREPARE_COMPLETED) -> "PREPARE_COMPLETED";
state(?UpgradePackageState_ACTIVATION_IN_PROGRESS) -> "ACTIVATION_IN_PROGRESS";
state(?UpgradePackageState_ACTIVATION_STEP_COMPLETED) ->
    "ACTIVATION_STEP_COMPLETED";
state(?UpgradePackageState_WAITING_FOR_COMMIT) -> "WAITING_FOR_COMMIT";
state(?UpgradePackageState_COMMIT_COMPLETED) -> "COMMIT_COMPLETED";
state(?UpgradePackageState_DEACTIVATION_IN_PROGRESS) ->
    "DEACTIVATION_IN_PROGRESS".

%%% os:cmd wrapper with printout
cmd(Cmd) ->
    info_msg("~s~n~s~n",[lists:flatten(Cmd), Res = os:cmd(Cmd)]),
    Res.

handle_cancel_after_activate(Key) ->
    Fun = fun() -> do_handle_cancel_after_activate(Key) end,
    case mnesia:transaction(Fun) of
	{atomic, goahead} ->
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    %% HS76784
	    update_progress(Key,[{result, ?ActionResultType_FAILURE},
				 {state, ?ActionStateType_CANCELLED},
				 {progressPercentage, 100},
				 {timeActionCompleted, CompleteTime}]),
	    fallback(Key, 
		     cancel, 
		     "The activation was cancelled by the user");
	{atomic, busy} ->
	    busy
    end.

do_handle_cancel_after_activate(Key) ->
    case mnesia:read({upgradePackage, Key}) of
	[] ->
	    busy;
	[UP] ->
	    case UP#upgradePackage.state of
		?UpgradePackageState_WAITING_FOR_COMMIT ->
		    mnesia:write(UP#upgradePackage{state=state(commitCompleted)}),
		    goahead;
		_ ->
		    busy
	    end
    end.

%%% ----------------------------------------------------------
%%% @doc Perform fallback. This function is called directly by the
%%% 'cancel' action, and also from timers created in 'activate',
%%% in the init phase of swmServer when an upgrade restart is
%%% taking place, and while waiting for the 'confirm' in upgrade.
%%%
%%% The given UP key refers to the To-UP of the upgrade operation
%%% in progress.
%%% @end
%%% ----------------------------------------------------------

-spec fallback(any(), fallbackContext(), string()) -> any().

fallback(UpKey, Context, Msg) ->
    case Context of 
	data_conversion ->
	    NotReceived = gmfImmUgInserter:getNotReceived(),
	    error_msg("data conversion taking too long, "
		      "classes not handled: ~p~n",
		      [ordsets:to_list(NotReceived)]);
	activating_before_restart ->
	    %% HU24092 Catch potentially hanging trigger commands
	    %% These are potentially long messages, but it is nevertheless
	    %% necessary
	    error_logger:info_report(
	      [{P,erlang:port_info(P)}||P<-erlang:ports()]),
	    error_logger:info_msg("ps -ef ~n",[]),
	    error_logger:info_msg("~s~n",[os:cmd("ps -ef ")]),
	    ok;

	_->
	    ok
    end,
    try do_fallback(UpKey, Msg)
    catch T:E ->
	    sysInitI:error_report(
	      [{?MODULE, fallback, [UpKey, Msg]},
	       {T,E},
	       erlang:get_stacktrace()])
    end.

do_fallback(Key, Msg) ->
    %% WP6081: Collect some information before fallback
    logI:generate_rollback_esi(),
    %% WP6081: End
    swmLib:write_swm_log(sender(Key), info, Msg),
    Fun = fun() ->
		  [Obj] = mnesia:read({upgradePackage, Key}),
		  mnesia:write(
		    Obj#upgradePackage{state=state(deactivationInProgress)})
	  end,
    {atomic, ok} = mnesia:transaction(Fun),
    info_msg("Fallback timer: ~s~n",[Msg]),
    %% HT47289 Make sure reason gets written to log
    swmLib:sync(),
    %% Fix ends here
    swmLib:restart_node(cold, ?ALH_TAG_UpgradeTimeout).

cancel_timer(undefined) ->
    ok;
cancel_timer(TimerRef) ->
    timer:cancel(TimerRef).

send_fallback_alarm(TimeRemaining, {ME, SF, SwM, Up}) ->
    try do_send_fallback_alarm(TimeRemaining, {ME, SF, SwM, Up})
    catch T:E ->
	    sysInitI:error_report(
	      [{?MODULE, send_fallback_alarm, [TimeRemaining, {ME,SF,SwM,Up}]},
	       {T,E},
	       erlang:get_stacktrace()])
    end.

do_clear_fallback_alarm(Up) ->
    do_clear_fallback_alarm(sysEnv:rcs_mode_2(), Up).

do_clear_fallback_alarm(vrcs, _) ->
    ok;
do_clear_fallback_alarm(_, Up) ->
    Dn = [<<"ManagedElement=1">>,
	  <<"SystemFunctions=1">>,
	  <<"SwM=1">>,
	  list_to_binary("UpgradePackage=" ++ Up)],
    comsaI:clear_alarm('FallbackOperationStartingSoon', Dn).

do_send_fallback_alarm(TimeRemaining, {ME, SF, SwM, Up}) ->
    Dn = [list_to_binary(X)||X<-["ManagedElement="++ME,
				 "SystemFunctions="++SF,
				 "SwM="++SwM,
				 "UpgradePackage="++Up]],

    Msg = lists:flatten(
	    io_lib:format(
	      "The upgrade operation has not been confirmed yet. "
	      "A fallback operation will be started in ~w seconds if the "
	      "upgrade is not confirmed.",[TimeRemaining])),
    %% HT47289 Log in SwmLog that alarm is raised
    swmLib:write_swm_log(
      "SwM", alert, 
      "Raising the 'FallbackOperationStartingSoon' alarm. Time remaining: "++
	  integer_to_list(TimeRemaining)),
    comsaI:send_alarm('FallbackOperationStartingSoon', warning, Dn, Msg).

countdown(FallbackTimeout, Now) ->
    try do_countdown(FallbackTimeout, Now)
    catch T:E ->
	    sysInitI:error_report(
	      [{?MODULE, countdown, [FallbackTimeout, Now]},
	       {T,E},
	       erlang:get_stacktrace()])
    end.

do_countdown(FallbackTimeout, Now) ->
    Elapsed = time_elapsed(os:timestamp(), Now),
    Remaining = FallbackTimeout - Elapsed,
    case Remaining of
	Remaining when Remaining > -1 ->
	    update_count(Remaining);
	_ ->
	    update_count(-1)
    end.

update_count(Count) ->
    swmLib:set_ram_variable(timeRemainingBeforeFallback, Count).

time_elapsed(Now, Start) ->
    timer:now_diff(Now, Start) div 1000000.

%% Check if the state transition is allowed, and if so update the state
set_up_state(Key, WantedState) ->
    Fun = fun() -> db_set_up_state(Key, WantedState) end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

db_set_up_state(Key, Wanted) ->
    [Obj] = case mnesia:read({upgradePackage, Key}) of
		[O] -> [O];
		[] -> mnesia:abort({no_such_up, Key})
	    end,
    case {Wanted, Obj#upgradePackage.state} of
	{prepareInProgress, ?UpgradePackageState_INITIALIZED} ->
	    do_state_change(Obj, Wanted);
	{activationInProgress, ?UpgradePackageState_PREPARE_COMPLETED} ->
	    case swmLib:get_ram_variable(activationStarted) of
		undefined ->
		    swmLib:set_ram_variable(activationStarted, Key),
		    do_state_change(Obj, Wanted);
		_UpKey ->
		    %% An activation is already ongoing, abort this one
		    {error, "An activation is already ongoing, this action will be ignored"}
	    end;
	{_, State} ->
	    {error, {wrong_state, State}}
    end.

do_state_change(Obj, Wanted) when is_record(Obj, upgradePackage) ->
    NewObj = Obj#upgradePackage{state = state(Wanted)},
    mnesia:write(NewObj),
    {ok, NewObj};
do_state_change(Key, Wanted) ->
    [UP] = mnesia:read({upgradePackage, Key}),
    do_state_change(UP, Wanted).


%%% ----------------------------------------------------------
%%% #           check_up_states_for_restore()
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Restore backup is not allowed, while an UP is in a transient
%%%              state
%%% ----------------------------------------------------------

%% HT12440 fix main part
check_up_states_for_restore() ->
    Fun = fun() -> do_check_up_states_for_restore() end,
    case mnesia:transaction(Fun) of
	{atomic, Result} ->
	    Result;
	{aborted, Reason} ->
	    erlang:error({aborted, Reason}, [])
    end.

do_check_up_states_for_restore() ->
    do_check_up_states_for_restore(mnesia:all_keys(upgradePackage)).

do_check_up_states_for_restore([Key|Keys]) ->
    [UP] = mnesia:read({upgradePackage, Key}),
    UnallowedStates = [?UpgradePackageState_ACTIVATION_IN_PROGRESS,
		       ?UpgradePackageState_ACTIVATION_STEP_COMPLETED,
		       ?UpgradePackageState_WAITING_FOR_COMMIT,
		       ?UpgradePackageState_DEACTIVATION_IN_PROGRESS],
    case lists:member(UP#upgradePackage.state, UnallowedStates) of
	true ->
	    {error, {element(4, Key), state(UP#upgradePackage.state)}};
	false ->
	    do_check_up_states_for_restore(Keys)
    end;
do_check_up_states_for_restore([]) ->
    ok.

%% Fix ends here


%%% ----------------------------------------------------------
%%% #           start_channel(Host, Port, User, Password)
%%% Input:
%%% Output: Handle- sftp file handle
%%% Exceptions: throw(Error) as returned from ftpI:start_channel/4
%%% Description: Open a sftp connection and start a channel
%%%              Store info as process variables for graceful shutdown
%%%              when there are problems
%%% ----------------------------------------------------------


start_channel(Proto, Host, Port, User, Password) ->
    case ftpI:start_channel(Proto, Host, Port, User, Password) of
	{ok, SP, C} ->
        case Proto of 
            sftp ->
	              info_msg("~p~n",[ssh:connection_info(
			       C, [client_version, server_version, peer])]);
            ftpes -> 
                info_msg("Started ftpes client : ~p~n", [SP])
        end,
        put(protocol, Proto),
	    {ok, SP, C};
	{error, E1} ->
	    Info1 =
		case E1 of
		    etimedout ->
			"Cannot establish a connection to remote server";
		    closed -> 
			"The remote server closed the connection";
		    String when is_list(String) ->
			String;
		    _ ->
			lists:flatten(io_lib:format("~p",[E1]))
		end,
	    update_progress([{resultInfo, Info1}]),
	    throw(E1)
    end.


%%% ----------------------------------------------------------
%%% #           list_dir(ChannelPid, Path, Timeout)
%%% Input: Proto::atom() 
%%%        ChannellPid::pid() - From sftp
%%%        Path::string() - The directory path
%%%        Timeout::integer() - Timeout in millisecs
%%% Output:
%%% Exceptions:
%%% Description: Wraps the ftpI:list_dir/4 function, dropping the
%%% "." and ".." directory names from a filename listing.
%%% end
%%% ----------------------------------------------------------
-spec list_dir(atom(), pid(), string(), integer()) -> {ok, [string()]}.

%% HT85211 Better error handling
list_dir(Proto, ChannelPid, Path, TimeoutMillis) ->
    case ftpI:list_dir(Proto, ChannelPid, Path, TimeoutMillis) of
	{ok, Listing} ->
	    {ok, Listing--[".", ".."]};
	{error, permission_denied} ->
	    throw({fail, "Permission denied"});
	{error, timeout} ->
	    throw({fail,"The server stopped sending data when reading "++Path});
	{error, Reason} ->
	    throw({fail, protocol_to_text(Proto) ++" error: "++make_string_flat(Reason)})
    end.


%% HU14586
%% Use upgrade marker as failsafe for detecting failed upgrades

upgrade_marker_path() ->
    filename:join(swmLib:swm_dir(), "upgrade_marker").

%% HU29568
%% Upgrade marker is no longer needed
%% Erase function retained for backwards compatibility reasons

%% set_upgrade_marker() ->
%%     ok = file:write_file(upgrade_marker_path(), <<"abcd">>).

erase_upgrade_marker() ->
    case file:delete(upgrade_marker_path()) of
	ok ->
	    ok;
	{error, enoent} ->
	    ok
    end.

%% is_upgrade_marker() ->
%%     filelib:is_file(upgrade_marker_path()).

%%% ----------------------------------------------------------
%%% #           unlock_action_capable_after_restart()
%%% Input: 
%%% Output:
%%% Exceptions:
%%% Description: Unlocks action capable after restart
%%% ----------------------------------------------------------
unlock_action_capable_after_restart() ->
    Key = {"1","1","1"},
    swmLib:erase_variable(action_capable_lock),
    swmLib:erase_variable({action_capable_locks, Key}),
    [SwM] = mnesia:dirty_read({swM, Key}),
    mnesia:dirty_write(SwM#swM{actionCapable = ?ActionCapabilityState_CAPABLE,
                               actionCapableInfo = undefined}),
    ok.


%%% ----------------------------------------------------------
%%% #           lock_action_capable_after_upgrade()
%%% Input: 
%%% Output:
%%% Exceptions:
%%% Description: Locks action capable after upgrade to enable
%%%              upgrading from older versions that do not 
%%%              support actionCapability locking
%%% ----------------------------------------------------------
lock_action_capable_after_upgrade() ->
    case swmLib:get_variable(action_capable_lock) of
        undefined ->
            swmLib:lock_action_capable(?ACTIVATE_UP_ACTION_CAPABLE_ID,
                                       ?ACTIVATE_UP_ACTION_CAPABLE_INFO);
        _ ->
	    Key = {"1","1","1"},
	    case swmLib:get_variable({action_capable_locks, Key}) of
		undefined ->
		    swmLib:lock_action_capable(?ACTIVATE_UP_ACTION_CAPABLE_ID,
					       ?ACTIVATE_UP_ACTION_CAPABLE_INFO);
		_ ->
		    ok
	    end
    end.


soaking_path() ->
    filename:join(swmLib:swm_home_dir(), "soaking_expire_time").

is_soaking() ->
    filelib:is_file(soaking_path()).

set_soaking() ->
    set_soaking(calendar:universal_time()).

set_soaking(BaseTime) ->
    case swmLib:get_variable(soaking_period) of
	infinity ->
	    filelib:ensure_dir(soaking_path()),
	    Data = io_lib:format("~w.~n", [{soakingTimer, infinity}]),
	    file:write_file(soaking_path(), Data),
	    swmLib:sync();
	undefined ->
	    file:delete(soaking_path());
	0 ->
	    file:delete(soaking_path());
	SoakingPeriod ->
	    ExpiryDate = 
		calendar:gregorian_seconds_to_datetime(
		  calendar:datetime_to_gregorian_seconds(BaseTime)+
		      SoakingPeriod),
	    filelib:ensure_dir(soaking_path()),
	    Data = io_lib:format("~w.~n",[{soakingTimer, ExpiryDate}]),
	    file:write_file(soaking_path(), Data),
	    swmLib:sync()
    end.

check_and_start_soaking_timer() ->
    case file:consult(soaking_path()) of
	{ok, Data} ->
	    case proplists:get_value(soakingTimer, Data) of
		infinity ->
		    Msg = "The system is in open ended fast restore mode",
		    set_action_capable_info(Msg),
		    swmLib:write_swm_log("SwM", alert, Msg),
		    info_msg(Msg),
		    soaking_never_ends;
		DateTime ->
		    GS = calendar:datetime_to_gregorian_seconds(DateTime),
		    UT = calendar:datetime_to_gregorian_seconds(
			   calendar:universal_time()),
		    case GS-UT of
			Diff when Diff =< 0 ->
			    %% The timer obviously has no effect, but
			    %% the return value will be of the same
			    %% type
			    timer:apply_after(0, gen_server, cast, 
					      [?SERVER, soaking_expired]);
			Diff ->
			    Msg = 
				"The system is in fast restore mode ending at "
				++comsaI:iso_time(DateTime,extended),
			    set_action_capable_info(Msg),
			    swmLib:write_swm_log("SwM", alert, Msg),
			    info_msg(Msg),
			    timer:apply_after(Diff*1000, gen_server, cast, 
					      [?SERVER, soaking_expired])
		    end
	    end;
	{error, enoent} ->
	    no_soaking;
	{error, {Line, Mod, Term}} ->
	    error_msg("Soaking error: ~s~n",
		      [file:format_error({Line, Mod, Term})]),
	    no_soaking;
	{error, Reason} ->
	    error_msg("Soaking error: ~s~n",[file:format_error(Reason)]),
	    no_soaking
    end.
	    
	    
handle_soaking_expired() ->
    case is_soaking() of
	true ->
	    Msg = "Fast restore mode expired. Starting software audit.~n",
	    info_msg(Msg),
	    swmLib:write_swm_log("SwM", info, Msg),
	    set_action_capable_info(undefined),
	    audit_software(),
	    file:delete(soaking_path());
	false ->
	    info_msg("Fast restore mode was cancelled while expire message was "
		     "in queue~n"),
	    ok
    end.

stop_soaking_timer() ->
    gen_server:call(?SERVER, stop_soaking_timer, 300000).

handle_stop_soaking_timer(State) when State#state.soakingTimer == undefined ->
    case is_soaking() of
	true ->
	    Msg = "Fast restore mode interrupted.",
	    info_msg(Msg),
	    swmLib:write_swm_log("SwM", info, Msg),
	    set_action_capable_info(undefined),
	    file:delete(soaking_path());
	false ->
	    ok
    end;
handle_stop_soaking_timer(State) ->
    Msg = "Fast restore mode interrupted.",
    info_msg(Msg),
    swmLib:write_swm_log("SwM", info, Msg),
    set_action_capable_info(undefined),
    file:delete(soaking_path()),
    cancel_timer(State#state.soakingTimer).




%%% ----------------------------------------------------------
%%% #           find_element(ElementName, Element)
%%% #           find_element(ElementName, Content)
%%% Input: ElementName:atom()
%%%        Element:#xmlElement{} or
%%%        Content.[#xmlElement{}] a list of elements
%%% Output: #xmlElement{}
%%% Exceptions: A badmatch occurs if the element does not exist
%%% Description: Finds a sub element to an xml element, or in a list
%%%              of element contents. Assumes there is only one element
%%%              with the same name
%%% ----------------------------------------------------------

find_element(ElementName, Element) when is_record(Element, xmlElement) ->
    find_element(ElementName, Element#xmlElement.content);
find_element(ElementName, ContentList) ->
    {value, Element} =
        lists:keysearch(ElementName, #xmlElement.name, ContentList),
    Element.

-spec find_optional_element(ElementName::atom(), 
			    Element::#xmlElement{}|list()) -> 
				   #xmlElement{} | false.

find_optional_element(ElementName, Element) 
  when is_record(Element, xmlElement) ->
    ContentList = Element#xmlElement.content,
    %%     find_element(ElementName, );
    %% find_optional_element(ElementName, ContentList) ->
    case lists:keysearch(ElementName, #xmlElement.name, ContentList) of
	{value, Value}  ->
	    Value;
	false ->
	    false
    end.


%%% ----------------------------------------------------------
%%% #           find_attribute(AttributeName, Element)
%%% #           find_attribute(AttributeName, AttributeList)
%%% Input: AttributeName:atom()
%%%        Element:#xmlElement{} or
%%%        AttributeList:[#xmlattribute{}] a list of xml attributes
%%% Output: Value:string()
%%% Exceptions: erlang:error invoked if attribute does not exist
%%% Description: Finds an attribute to an xml element, or in a list of
%%%              attributes and returns the value of the attribute
%%% ----------------------------------------------------------

find_attribute(AttributeName, Element) when is_record(Element, xmlElement) ->
    find_attribute(AttributeName, Element#xmlElement.attributes);
find_attribute(AttributeName, AttributeList) ->
    case lists:keysearch(AttributeName, #xmlAttribute.name, AttributeList) of
        {value, Attribute} ->
            Attribute#xmlAttribute.value;
        false ->
            erlang:error({badmatch, false}, [AttributeName, AttributeList])
    end.

find_text(Element) when is_record(Element, xmlElement) ->
    [Text] = Element#xmlElement.content,
    Text#xmlText.value.

%%% ----------------------------------------------------------
make_string(Term) ->
    make_string("~p", Term).

make_string_flat(Term) ->
    make_string("~s", Term).

make_string(Format, Term) ->
    try lists:flatten(io_lib:format(Format, [Term]))
    catch
	_ : _ ->
	    lists:flatten(io_lib:format("~p", [Term]))
    end.


%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
						%even_more_internal_function1(One, Two)->
						%   nnn.

info_msg(Format) ->
    info_msg(Format, []).
info_msg(Format, Args) ->
    Msg = lists:flatten(io_lib:format(Format, Args)),
    log_SwmInternal(info, Msg),
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

error_msg(Format) ->
    error_msg(Format, []).
error_msg(Format, Args) ->
    Msg = lists:flatten(io_lib:format(Format, Args)),
    log_SwmInternal(error, Msg),
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

warning_msg(Format) ->
    warning_msg(Format, []).
warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

mnesia_event_server_start() ->
    mnesia_event_server_start([swInventory, swVersion]).

mnesia_event_server_start(Tbls) ->
    proc_lib:spawn(?MODULE, mnesia_event_server_init, [Tbls]).

mnesia_event_server_stop() ->
    catch swmMnesiaEventServer ! stop.

mnesia_event_server_init(Tbls) ->
    try
	stop = mnesia_event_server_init_safe(Tbls),
	sysInitI:info_report(["stopped"])
    catch
	ErrClass : ErrReason ->
	    sysInitI:error_report([{ErrClass, ErrReason},
		      {stacktrace, erlang:get_stacktrace()}])
    end.

mnesia_event_server_init_safe(Tbls) ->
    register(swmMnesiaEventServer, self()),
    [mnesia:subscribe({table, Tbl, detailed}) || Tbl <- Tbls],
    mnesia_event_server_loop(Tbls).

mnesia_event_server_loop(Tbls) ->
    receive
	{mnesia_table_event, _Event} ->
	    %% sysInitI:info_report(["------- Mnesia Event -------" |
	    %% 	       mnesia_event_format(Event)] ++
	    %% 	      ["------- Table keys after operation -------" |
	    %% 	       [{Tbl, mnesia:dirty_all_keys(Tbl)} || Tbl <- Tbls]]),
	    mnesia_event_server_loop(Tbls);
	stop ->
	    stop;
    {system, _, get_state} ->
        % just so we don't get a warning when collecting Erlang process info
        mnesia_event_server_loop(Tbls);
	UnrecMsg ->
	    sysInitI:warning_report(["Unrecognized message:",
		       {msg, UnrecMsg}]),
	    mnesia_event_server_loop(Tbls)
    end.

%% mnesia_event_format({Oper, Tbl, Data, OldRecs, AId}) ->
%%     [mnesia_event_format_Operation(Oper, OldRecs),
%%      {Tbl, mnesia_event_format_Data(Data)},
%%      {activityId, AId}];
%% mnesia_event_format(Event) ->
%%     [Event].

%% mnesia_event_format_Data(Data) ->
%%     try
%% 	element(2, Data)
%%     catch
%% 	_ : _ ->
%% 	    Data
%%     end.

%% mnesia_event_format_Operation(write, []) ->
%%     {operation, write};
%% mnesia_event_format_Operation(write, [_ | _]) ->
%%     {operation, overwrite};
%% mnesia_event_format_Operation(Oper, _) ->
%%     {operation, Oper}.


%%% ----------------------------------------------------------
info() ->
    ?SERVER ! {?MODULE, info}.

%%% TEST CASE SUPPORT

%%% HU59680 Move to separate process to avoid messages getting queued up in
%%% swmServer
%%% Needed for swm_basic_test_suite
start_test_event_server() ->
    proc_lib:spawn(
      fun() ->
	      case whereis(swmTestEventServer) of
		  Pid when is_pid(Pid) -> unregister(swmTestEventServer);
		  _ -> ok
	      end,
	      register(swmTestEventServer, self()),
	      mnesia:subscribe({table, swM, detailed}),
	      mnesia:subscribe({table, upgradePackage, detailed}),
	      test_event_server_loop()
      end).

stop_test_event_server() ->
    swmTestEventServer!stop.

test_event_server_loop() ->
    receive
	{mnesia_table_event, Event} ->
	    case swmLib:get_variable(swm_basic_tests_return_pid) of
		Pid when is_pid(Pid) -> Pid!{mnesia_table_event, Event};
		_ -> ok
	    end,
	    test_event_server_loop();
	stop ->
	    ok
    end.



test_download_packages(Up, {file, _, "localhost", _, RemoteFile, _}) ->
    Uri = Up#upgradePackage.uri,
    ArchiveDir = get_up_archive_dir(Up#upgradePackage.administrativeData), 

    {ok, FIO} = file:read_file_info(RemoteFile),
    RemoteDir = case FIO#file_info.type of
		    directory ->
			RemoteFile;
		    _ ->
			filename:dirname(RemoteFile)
		end,


    %% We only handle files in a flat directory.
    {ok, Files} = file:list_dir(RemoteDir),
    info_msg("Uri: ~p~nRemoteDir: ~p~nFiles: ~p~n",[Uri , RemoteDir, Files]),
    %% Set up progress reporting by calculating the total size
    %% of all files in the directory
    FileSizes =
	[begin
	     Path = filename:join(RemoteDir, File),
	     {ok, ThisFIO} = file:read_file_info(Path),
	     case ThisFIO#file_info.type of
		 regular ->
		     case filename:extension(File) of
			 ".xml" -> {Path, 0};
			 _ ->
			     {Path, ThisFIO#file_info.size}
		     end;
		 _ ->
		     {Path, 0}
	     end
	 end||File<-Files],

    info_msg("File sizes: ~n~p~n",[FileSizes]),
    TotalSize = lists:foldl(fun({_, S}, A) -> S+A end, 0, FileSizes),
    info_msg("TotalSize: ~p~n",[TotalSize]),
    {ok, Tref} = timer:send_interval(1000, update_progress),
    put(progress_timer, Tref),

    lists:foldl(
      fun({_Path, 0}, AccuSize) ->
	      AccuSize;
	 ({Path, Size}, AccuSize) ->
	      Basename = filename:basename(Path),
	      Key = Up#upgradePackage.upgradePackageId,
	      update_progress(Key, [{additionalInfo,"Downloading "++Basename}]),
	      LocalPath = filename:join(ArchiveDir, Basename),
	      ok = file:make_link(Path, LocalPath),
	      receive
		  {cancel, _} ->
		      throw(cancelled)
	      after 0 -> ok
	      end,
						%         spawn(swmOs, mount_cxp, [LocalPath]),
	      Size + AccuSize
      end, 0, FileSizes),
    timer:cancel(Tref),
    ok.



protocol_to_text(Proto) ->
    atom_to_list(Proto).

board_added(ProdNo, ProdRev, Res) ->
    info_msg("board_added(~p, ~p, ~p)~n",[ProdNo, ProdRev, Res]).

%% erl_call(N, M, F, A) ->
%%     OtpRoot = os:getenv("OTP_ROOT"),
%%     Pattern = filename:join(
%%      [OtpRoot, "lib", "erl_interface-*", "bin","erl_call"]),
%%     [ErlCall] = filelib:wildcard(Pattern),
%%     sysOs:cmd(ErlCall++" -c bs_SIS_1 -sname "++
%%     make_string("~w -a '~w ~w ~w'", [N, M, F, A])).


