%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmDataInit.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R1A/R2A/R3A/R4A/R5A/7
%%%
%%% @doc ==Initialization of software management==
%%% This module contains the necessary initialization of software management.

-module(swmDataInit).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/7').
-date('2016-03-28').
-author('etxberb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([instPhSeqBeg_begin/0,
	 instPhParallel_init/1,
	 instPhParallel_init_data/0,
	 instPhSeqBeg_post_init/0,
	 instPhParallel_post_init/0]).
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

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhSeqBeg_begin() ->
    swmAppData:prepare_appdata(),
    ok.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init(DbNodes) ->
    file:make_dir(swmLib:sda1_dir()),
    file:make_dir(swmLib:software_hal_dir()),

    swmLib:make_cxp_list(sysEnv:home_dir()),

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

    [mnesia:dirty_write(#swmDbMonitorExemptions{table=T,field=F})||
	{T,F}<-[{swM, #swM.reportProgress},
		{upgradePackage, #upgradePackage.reportProgress},
		{brmBackupManager, #brmBackupManager.progressReport},
		{brmBackup, #brmBackup.progressReport},
		{brmBackupScheduler, #brmBackupScheduler.progressReport},
		{brmFailsafeBackup, #brmFailsafeBackup.progressReport},
		%% {brmFailsafeBackup, #brmFailsafeBackup.progress},
		{brmBackupLabelStore, #brmBackupLabelStore.lastCreatedBackup}
	       ]],

    swmAppData:init(DbNodes),
    swmLib:init(DbNodes),

    ok.

%% Old installation phase - kept due to explicit call from
%% $RDE_TOP/tools/mkcpi/mkcpi.escript
init(DbNodes) ->
    instPhParallel_init(DbNodes).

children() ->
    {ok, case clhI:mp_role() of
	     core -> 
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
	 end
    }.



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
instPhParallel_init_data() ->
    make_dir(filename:join(swmLib:swm_dir(), "home1")),
    make_dir(filename:join(swmLib:swm_dir(), "home2")),
    make_dir(filename:join(swmLib:swm_dir(), "boot")),
  
    DataDir = swmLib:data_dir(),
    SwFile = filename:join(DataDir, "swdata.dets"),
    filelib:ensure_dir(SwFile),

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
		     fallbackTimer = ?swM_fallbackTimer_default,
		     timeRemainingBeforeFallback =
			 ?swM_timeRemainingBeforeFallback_default,
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
    swmInventory:update_tables(),

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

do_transform_swM(
  {swM, SwMId, ReportProgress, FallbackTimer, TimeRemainingBeforeFallback, 
   LocalFileStorePath, UserLabel, TimeoutFallbackCapability}) ->
    %% Upgrade from 4.0 -> 4.1
    #swM{swMId = SwMId,
	 reportProgress = ReportProgress,
	 fallbackTimer = FallbackTimer,
	 timeRemainingBeforeFallback = TimeRemainingBeforeFallback,
	 localFileStorePath = LocalFileStorePath,
	 userLabel = UserLabel,
	 timeoutFallbackCapability = TimeoutFallbackCapability,
	 actionCapable = ?ActionCapabilityState_WAIT,
	 actionCapableInfo = ""};
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
    comsaLib:register_callback(Root++["BrM"], swmBackup),
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
       {"BrmFailsafeBackup", brmFailsafeBackup}]).

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
    swmServer:activate(),
    swmBackup:activate(),
    swmFailsafe:activate(),
    swmDbMonitor:activate(),
    swmLib:delete_upgrWindow_data().
    

%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%%some_method(Parameter)->
%%   nn.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%%internal_function1(One, Two)->
%%   nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%% info_msg(Format) ->
%%     info_msg(Format, []).

%% info_msg(Format, Args) ->
%%     sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%error_msg(Format) ->
%    error_msg(Format, []).
%% error_msg(Format, Args) ->
%%     sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%even_more_internal_function1(One, Two)->
%   nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


