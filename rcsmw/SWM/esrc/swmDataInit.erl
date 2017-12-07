%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmDataInit.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/5
%%%
%%% @doc ==Initialization of software management==
%%% This module contains the necessary initialization of software management.

-module(swmDataInit).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/5').
-date('2017-10-11').
-author('etxberb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R1A/3      2012-01-31 etxjotj     Fixed startup
%%% R1A/24     2012-07-05 etxjotj     Added init call to swmLib
%%% R2A/9      2013-01-25 etxjotj     Rembered there was an appdatafile
%%% R2A/28     2014-02-18 etxberb     Added swmLib:delete_upgrWindow_data() in
%%%                                   activate/0.
%%% R2A/30     2014-03-24 erarafo     Upgrade callbacks encapsulated in swmLib
%%% R2A/31     2014-06-12 etxjotj     EE split, progress reporting fix
%%% R2A/34     2014-07-04 etxberb     TR HS75210: Changed swm to swM in call to
%%%                                   comsaI:register_subscriptions.
%%% R2A/34     2014-08-05 etxjotj     ESI data from SWM
%%% R3A/1      2014-11-12 etxjotj     Register comte model callbacks
%%% R3A/2      2014-11-28 etxjotj     ECIM SwM 3.0
%%% R3A/3      2014-12-01 etxjotj     ECIM SwM 3.0 upgrade
%%% R3A/6      2015-02-06 etxjotj     Register upgrade trigger module
%%% ----    ---------- -------  ------------------------------------------------
%%% R4A/1   2015-04-16 etxpejn  Moved create ESI dir to swmLib.
%%% R3A/7   2015-04-21 etxjotj  Removed activation of dbg functions
%%% R4A/4   2015-07-07 etxberb  Changed mnesia:create_table to
%%%                             clhI:mnesia_create_table.
%%% R4A/5   2015-07-21 etxjotj  Clean disk
%%% R4A/9   2015-09-17 etxjotj  Added mnesia monitor
%%% R4A/10  2015-09-24 etxjtoj  Exemption handling for mnesia monitoring
%%% R5A/2   2016-01-07 etxberb  Changed installation phases in all blocks.
%%% R5A/3   2016-01-11 etxberb  Added old installation phase functions for
%%%                             backwards compatibility reasons (explicit calls
%%%                             from $RDE_TOP/tools/mkcpi/mkcpi.escript)
%%% R5A/4   2016-01-13 etxberb  Added call to 'swmAppData:prepare_appdata()'.
%%% R5A/5   2016-01-19 etxpejn  Added upgrade for new SwM model.
%%% R5A/6   2016-01-25 etxpejn  Removed unsupported upgrade track
%%% R5A/7   2016-03-28 etxberb  Added make_dir for sda1 & hal.
%%% R6A/1   2016-04-25 etxpejn  Added CXP_* in instPhParallel_init_data()
%%% R6A/2   2016-04-25 etxjotj  Clear all progress reports
%%% R6A/3   2016-04-25 etxjotj  Default message not necessary in SwM 4.1
%%% R6A/4   2016-07-25 etxjotj  Excluded brmRollbackAtRestore timer from db
%%% R6A/5   2016-09-02 ekurnik  HV14974 set fallbackTimer to 30 min if less than that
%%% ----------------------------------------------------------
%%% R7A/1   2016-09-29 etxjotj  Fallback timer reset disabled function
%%% ----------------------------------------------------------
%%% R8A/1   2016-10-31 etxpejn  Added check if signing certifiactes needs to be
%%%                             updated
%%% R8A/2   2017-01-24 etxberb  Added stPh_preInit/0.
%%% ----------------------------------------------------------
%%% R9A/2   2017-04-19 etxberb  Added swmDiskSrv:child_specs().
%%% R9A/3   2017-04-27 etxberb  Added swmAppdataReg.
%%% ----    ---------- -------  ------------------------------------------------
%%% R10A/2  2017-06-08 etxberb  Default enabling of HSI.
%%% R10A/3  2017-06-09 etxberb  Temporarily disabled HSI.
%%% R10A/4  2017-06-20 etxarnu  Removed os:putenv (obsolete and dangerous)
%%% R10A/5  2017-06-26 etxpejn  Default enabling of HSI.
%%% R10A/6  2017-07-06 etxjotj  HW10146 Disable anti rollback feature
%%% R10A/7  2017-07-06 etxjotj  Log when new CRL is stored in flash
%%% R10A/9  2017-07-12 etxjotj  Moved mount_sda1 to swmOs
%%% R10A/10 2017-07-14 etxjotj  HW11604 Reduce logging
%%% R10A/11 2017-08-21 etxberb  Added enable_bootfallback/1.
%%% R10A/12 2017-08-22 etxberb  enable_bootfallback only on target.
%%% R11A/1  2017-09-01 etxarnu  Fixes for multi-DU
%%% R11A/2  2017-09-05 etxjotj  Use rcs_mode_2
%%% R11A/3  2017-09-05 etxjotj  Bugfix
%%% R11A/5  2017-10-11 etxberb  Placed swmDiskSrv:child_specs first in the list.
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([instPhSeqBeg_begin/0,
	 instPhParallel_init/1,
	 instPhParallel_init_data/0,
	 instPhParallel_init_board/0,
	 instPhSeqBeg_post_init/0,
	 instPhParallel_post_init/0]).
-export([stPh_preInit/0]).
-export([init/1]).
-export([children/0,
	 activate/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-export([add_clh_option/1]).

-include("RcsBrM.hrl").
-include("RcsSwM.hrl").
-include("RcsSwIM.hrl").

-import(comsaLib, [iso_time/2]).
-include("SwmInternal.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% HV14974 short term solution until MOM changes
-define(swM_fallbackTimer_long, 1800).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhSeqBeg_begin() ->
    DbNodes = mnesia:system_info(running_db_nodes),
    {atomic, ok} = 
	clhI:mnesia_create_table(swmAppdataReg,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes, record_info(fields,
							   swmAppdataReg)} | 
				  add_clh_option(swmAppdataReg)]),
    swmAppData:prepare_appdata(),
    ok.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init(DbNodes) ->

    SwM = [{swM, ?swM_types},
	   {upgradePackage, ?upgradePackage_types}],
%	   {swVersionMain, ?swVersionMain_types, ram}],

    SwIM = [{swInventory, ?swInventory_types},
    	    {swVersion, ?swVersion_types, ram},
    	    {swItem, ?swItem_types, ram}],

    BrM = [{brM, ?brM_types},
	   {brmBackupManager, ?brmBackupManager_types},
	   {brmBackup, ?brmBackup_types, ram},
	   {brmSingleEvent, ?brmSingleEvent_types},
	   {brmPeriodicEvent,?brmPeriodicEvent_types},
	   {brmBackupHousekeeping, ?brmBackupHousekeeping_types},
	   {brmBackupScheduler, ?brmBackupScheduler_types},
	   {brmCalendarBasedPeriodicEvent,?brmCalendarBasedPeriodicEvent_types},
	   {brmRollbackAtRestore, ?brmRollbackAtRestore_types},
	   {brmBackupLabelStore, ?brmBackupLabelStore_types}
	  ],
    
    BrmFailsafe = [{brmFailsafeBackup, ?brmFailsafeBackup_types}],

    [[create_table(TableDef, DbNodes)||TableDef<-Tables]||
	Tables<-[SwM, SwIM, BrM, BrmFailsafe]],

    %% SWM INTERNAL 

    {atomic, ok} =
	clhI:mnesia_create_table(swmFallbackList, 
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes, record_info(fields,
							   swmFallbackList)} |
				  add_clh_option(swmFallbackList)]),

    {atomic, ok} = 
	clhI:mnesia_create_table(
	  swmDbMonitorExemptions,
	  [{type, bag},
	   {disc_copies, DbNodes},
	   {attributes, record_info(fields, swmDbMonitorExemptions)} | 
	   add_clh_option(swmDbMonitorExemptions)]),


    [swmI:register_exemption(T, F)||
	{T,F}<-[{swM, #swM.reportProgress},
		{upgradePackage, #upgradePackage.reportProgress},
		{brmBackupManager, #brmBackupManager.progressReport},
		{brmBackup, #brmBackup.progressReport},
		{brmBackupScheduler, #brmBackupScheduler.progressReport},
		{brmFailsafeBackup, #brmFailsafeBackup.progressReport},
		%% {brmFailsafeBackup, #brmFailsafeBackup.progress},
		{brmBackupLabelStore, #brmBackupLabelStore.lastCreatedBackup},
		{brmRollbackAtRestore, 
		 #brmRollbackAtRestore.timeRemainingBeforeRollback}
	       ]],

    swmAppData:init(DbNodes),
    swmLib:init(DbNodes),

    ok.

%% Old installation phase - kept due to explicit call from
%% $RDE_TOP/tools/mkcpi/mkcpi.escript
init(DbNodes) ->
    instPhParallel_init(DbNodes).

children() ->
    Children =
	case clhI:mp_role() of
	    core ->
		swmDiskSrv:child_specs() ++
		[
		 {swmServer, {swmServer, start, []},
		  permanent, 1000, worker, [swmServer]},
		 {swmBackup, {swmBackup, start, []},
		  permanent, 1000, worker, [swmBackup]},
		 {swmBackupScheduler, 
		  {swmBackupScheduler, start, [[{"1","1","1","1","1"}]]},
		  permanent, 1000, worker, [swmBackupScheduler]},
		 {swmFailsafe,{swmFailsafe, start, []},
		  permanent, 1000, worker, [swmFailsafe]},
		 {swmDbMonitor, {swmDbMonitor, start, []},
		  permanent, 1000, worker, [swmDbMonitor]}
		];
	    _ ->
		[]
	end,
    {ok, Children}.



create_table({Name, Types}, DbNodes) ->
    Fields = [Field||{Field, _}<-Types],
    {atomic, ok} =
	clhI:mnesia_create_table(Name, [{type, set},
					{disc_copies, DbNodes},
					{attributes, Fields} |
					add_clh_option(Name)]);
create_table({Name, Types, ram}, DbNodes) ->
    Fields = [Field||{Field, _}<-Types],
    {atomic, ok} =
	clhI:mnesia_create_table(Name, [{type, set},
					{ram_copies, DbNodes},
					{attributes, Fields} |
					add_clh_option(Name)]).

%%% ###########################################################################
%%% add_clh_option
%%%
%%% Add option clh_changeCopyType_installation for large tables in order for
%%% CLH to optimize disc operations. See also edoc for clhI.
%%%
%%% ###=====================================================================###
add_clh_option(_) ->
    [].

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init_board() ->
    make_dir(filename:join(swmLib:swm_dir(), "home1")),
    make_dir(filename:join(swmLib:swm_dir(), "home2")),
    make_dir(filename:join(swmLib:swm_dir(), "boot")),

    make_dir(swmLib:sda1_dir()),
    case sysEnv:rcs_mode_2() of
	target ->
	    ok;
	simulated ->
	    make_dir(swmLib:software_hal_dir());
	_ ->
	    ok
    end,
    swmLib:make_cxp_list(sysEnv:home_dir()),
    ok.


%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init_data() ->
  
    DataDir = swmLib:data_dir(),
    SwFile = filename:join(DataDir, "swdata.dets"),
    filelib:ensure_dir(SwFile),

    %% Check if signing certifiactes needs to be updated
    case swmOs:check_crl(sysEnv:rcs_mode_2()) of
	update_req ->
	    %% The signing cerficated has been updated. No graceful period
	    %% neded if no upgrade is involved.
	    info_msg("Updating CRL: ~n~s~n",
		     [swmOs:get_crl_ids(sysEnv:rcs_mode_2())]),
	    %% Disabled due to HW10614
	    %% swmOs:store_crl(sysEnv:rcs_mode_2());
	    
	    very_much_do_nada;
	update_not_req ->
	    do_nada
    end,

    %% Create Top MOCs
    case swmI:is_upgrade_ongoing() of
	true ->
	    ok = transform_swM(),
	    ok = transform_upgradePackage(),
	    ok = swmI:copy_old_table(swInventory),
	    ok = swmI:copy_old_table(swmVariables);
	false ->
	    SwM = 
		#swM{swMId={"1","1","1"},
             %% HV14974 change initial fallback timer to 1800
		     fallbackTimer = ?swM_fallbackTimer_long,
		     timeRemainingBeforeFallback = -1,
		     timeoutFallbackCapability = 
			 ?SwMTimeoutFallbackCapability_SUPPORTED,
		     actionCapable = ?ActionCapabilityState_CAPABLE},
	    SwInventory = 
		#swInventory{swInventoryId={"1","1","1"}},
	    TopObjs = 
		fun() ->
			mnesia:write(SwM),
			mnesia:write(SwInventory)
		end,
	    mnesia:transaction(TopObjs)
    end,


    swmLib:clear_upg_callbacks(),

    swmServer:open_dets(swVersion, [{file, SwFile}, {keypos, 2}]),
    swmModel:update_tables(),
    swmInventory:update_tables(log), %HW11604

    %% Backup

    swmBackup:init_bu_info(),

    %% BrmFailsafe

    swmFailsafe:init_data(),

    %% Logs
    %% The internal log is intended as insurance for other block interface
    %% failures
    case swmI:is_upgrade_ongoing() of
	true -> 
	    ok;
	false ->
	    OptList = [{maxSize, 1}, {rotatingSegments, 3}, {public, false}],
	    logI:create_log("SwmInternal", OptList),
	    OptList2 = [{maxSize, 1}, {rotatingSegments, 3}, {public, true}],
	    logI:create_log("SwmLog", OptList2)
    end,
    logI:register_esi_dir(swmLib:esi_dir()),
    logI:register_esi_cb(swmI),

    sysServer:register_file_owner("swm", swmI),
    ok.

transform_swM() ->
    [SwM] = swmI:all_objects(swM),
    mnesia:dirty_write(do_transform_swM(SwM)).

%% %% HV14974 set fallback timer to 30 min if less than that
%% This is removed since the default value in MOM is raised to 1800 
%% do_transform_swM(
%%   {swM, SwMId, ReportProgress, FallbackTimer, TimeRemainingBeforeFallback, 
%%    LocalFileStorePath, UserLabel, TimeoutFallbackCapability}) when FallbackTimer < ?swM_fallbackTimer_long ->
%%     case swmLib:get_variable(disable_fallback_timer_reset) of
%% 	true ->
    
%% 	    do_transform_swM(
%% 	      {swM, SwMId, ReportProgress, FallbackTimer, TimeRemainingBeforeFallback, 
%% 	       LocalFileStorePath, UserLabel, TimeoutFallbackCapability});
%% 	_ ->
	
%% 	    do_transform_swM(
%% 	      {swM, SwMId, ReportProgress, ?swM_fallbackTimer_long, TimeRemainingBeforeFallback, 
%% 	       LocalFileStorePath, UserLabel, TimeoutFallbackCapability})
%%     end;

do_transform_swM(
  {swM, SwMId, ReportProgress, FallbackTimer, TimeRemainingBeforeFallback, 
   LocalFileStorePath, UserLabel, TimeoutFallbackCapability,
   ActionCapable, ActionCapableInfo}) ->
    %% upgrade from 4.1 
    #swM{swMId = SwMId,
	 reportProgress = ReportProgress,
	 fallbackTimer = FallbackTimer,
	 timeRemainingBeforeFallback = TimeRemainingBeforeFallback,
	 localFileStorePath = LocalFileStorePath,
	 userLabel = UserLabel,
	 timeoutFallbackCapability = TimeoutFallbackCapability,
	 actionCapable = ActionCapable,
	 actionCapableInfo = ActionCapableInfo};


do_transform_swM(
  {swM, SwMId, ReportProgress, FallbackTimer, TimeRemainingBeforeFallback, 
   LocalFileStorePath, UserLabel, TimeoutFallbackCapability}) ->
    %% Upgrade from 4.0 
    #swM{swMId = SwMId,
	 reportProgress = ReportProgress,
	 fallbackTimer = FallbackTimer,
	 timeRemainingBeforeFallback = TimeRemainingBeforeFallback,
	 localFileStorePath = LocalFileStorePath,
	 userLabel = UserLabel,
	 timeoutFallbackCapability = TimeoutFallbackCapability,
	 actionCapable = ?ActionCapabilityState_WAIT,
	 actionCapableInfo = ""};

%% HV14974 set fallback timer to 30 min if less than that
%% This is removed since the default value in MOM is raised to 1800 
%% do_transform_swM(#swM{fallbackTimer = FallbackTimer} = SwM) 
%%                 when FallbackTimer < ?swM_fallbackTimer_long -> 
%%     case swmLib:get_variable(disable_fallback_timer_reset) of
%% 	true ->
%% 	    SwM;
%% 	_ ->
%% 	    SwM#swM{fallbackTimer = ?swM_fallbackTimer_long}
%%     end;

do_transform_swM(X) -> X.

transform_upgradePackage() ->
    UPs = swmI:all_objects(upgradePackage),
    F = fun do_transform_upgradePackage/1,
    [mnesia:dirty_write(F(UP))||UP<-UPs],
    ok.

do_transform_upgradePackage(
  {upgradePackage, UpgradePackageId, State, ReportProgress, 
   IgnoreBreakPoints, UserLabel, AdministrativeData, ActivationStep, 
   Created, _, _, _, Uri, CreatorActionId, Password}) ->
    #upgradePackage{upgradePackageId = UpgradePackageId,
		    state = State,
		    reportProgress = ReportProgress,
		    ignoreBreakPoints = IgnoreBreakPoints,
		    userLabel = UserLabel,
		    administrativeData = AdministrativeData,
		    activationStep = ActivationStep,
		    created = Created,
		    uri = Uri,
		    creatorActionId = CreatorActionId,
		    password = Password};
do_transform_upgradePackage(X) -> X.

    

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhSeqBeg_post_init() ->
    swmAppData:push_appdata(),
    ok.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_post_init() ->
    swmI:register_upg_callback(swmUpgradeCb),
    Root = ["ManagedElement", "SystemFunctions"],
    comsaLib:register_callback(Root++["SwM"], swmModel),
    comsaLib:register_callback(Root++["SwInventory"], swmInventory),
    comsaLib:register_callback(Root++["BrM"], swmBackupModel),
    comsaLib:register_callback(
      Root++["BrM", "BrmBackupManager", "BrmFailsafeBackup"], swmFailsafe),

    comsaI:register_subscriptions(
      "RcsSwM", [{"SwM", swM}, 
		 {"UpgradePackage", upgradePackage},
		 {"SwVersionMain", swVersionMain}]),
    comsaI:register_subscriptions(
      "RcsSwIM", [{"SwInventory", swInventory},
		  {"SwVersion", swVersion},
		  {"SwItem", swItem}]),
    comsaI:register_subscriptions(
      "RcsBrM", 
      [{"BrM", brM},
       {"BrmBackupManager", brmBackupManager},
       {"BrmBackup", brmBackup},
       {"BrmSingleEvent", brmSingleEvent},
       {"BrmPeriodicEvent", brmPeriodicEvent},
       {"BrmBackupHousekeeping", brmBackupHousekeeping},
       {"BrmBackupScheduler", brmBackupScheduler},
       {"BrmCalendarBasedPeriodicEvent", brmCalendarBasedPeriodicEvent},
       {"BrmRollbackAtRestore", brmRollbackAtRestore},
       {"BrmBackupLabelStore", brmBackupLabelStore},
       {"BrmFailsafeBackup", brmFailsafeBackup}]),
    swmColi:enable_hsi(""),   % Default on!
    enable_bootfallback(swmI:node_type()),
    ok.

%%% ###########################################################################
%%  Phases for start and restart:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysinit/sysInitServer.erl
%%% ###=====================================================================###
stPh_preInit() ->
    case sysEnv:rcs_mode_2() of
	target ->
	    swmOs:mount_sda1(),
	    swmLib:dir_mount_info();
	_ ->
	    ok
    end,
    ok.

make_dir(Dir) ->
    case file:make_dir(Dir) of
	ok ->
	    ok;
	{error,eexist} ->
	    ok;
	{error, Reason} ->
	    erlang:error(Reason, [Dir])
    end.

activate()->
    case clhI:mp_role() of
	core -> 
	    swmServer:activate(),
	    swmBackup:activate(),
	    swmFailsafe:activate(),
	    swmDbMonitor:activate(),
	    swmLib:delete_upgrWindow_data() ;
	_ ->
	    ok
    end.
    
%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%% ###########################################################################
%%  enable_bootfallback
%%  
%%% ###=====================================================================###
enable_bootfallback("BPU") ->
    swmLib:set_variable(bootfallback_enabled, false);
enable_bootfallback(_) ->
    case sysEnv:rcs_mode_2() of
	target ->
	    swmLib:set_variable(bootfallback_enabled, true);
	_ ->
	    swmLib:set_variable(bootfallback_enabled, false)
    end.

%% info_msg(Format) ->
%%     info_msg(Format, []).

info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%error_msg(Format) ->
%    error_msg(Format, []).
%% error_msg(Format, Args) ->
%%     sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


