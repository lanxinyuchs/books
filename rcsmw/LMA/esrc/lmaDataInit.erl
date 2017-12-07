%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmaDataInit.erl
%%% Author:	etxbjca
%%% Description: Data initialization functions used at system installation and
%%%              software upgrade.
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(lmaDataInit).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R9A/R11A/R12A/5').
-date('2017-11-28').
-author('etxarnu').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	lmaDataInit.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% R3A/1      2014-10-06   etxpejn     Added capacity tables and lmaLicenseUser process
%%% R3A/3      2014-10-09   etxpejn     Call lmaGlms at activate
%%% R3A/5      2015-01-29   etxpejn     Changed os:cmd to file:make_dir
%% ----    ---------- -------  ------------------------------------------------
%% R4A/2   2015-06-15 etxpejn  Prepared for WP4530
%% R4A/3   2015-06-17 etxpejn  WP4530: Check if test MOM should be loaded or not
%% R4A/7   2015-06-29 etxpejn  Corrected upgrade problem R3->R4.
%% R4A/9   2015-06-30 etxpejn  Corrected catch code
%% R4A/10  2015-07-07 etxberb  Changed mnesia:create_table to
%%                             clhI:mnesia_create_table.
%% R5A/1   2015-12-03 etxpejn  Added call to register_file_owner
%% R5A/2   2016-01-07 etxberb  Changed installation phases in all blocks.
%% R6A/1   2016-06-22 etxpejn  Convert EU from mnesia to disk under upgrade
%% R11A/1  2017-07-13 qselert  SP086: Added LicenseSupport MO changes
%% R12A/1  2017-10-19 etxpejn  SP641: Look for 5gCloudNodeTestLicense in LKF during AI
%% R12A/2  2017-10-31 echhedb  SP086: Added appData receiver registration for lmaAppData,
%%                             and function licensesupport_from_table_to_disk(..).
%% R12A/3  2017-11-02 etxpejn  Updated certI call to update_test_license_vnode
%% R12A/4  2017-11-27 etxarnu  Exported look_for_test_license/1
%% R12A/5  2017-11-28 etxarnu  Check that test license is valid
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([instPhParallel_init/1,
	 instPhParallel_init_data/0,
	 instPhParallel_post_init/0]).
-export([children/0,
	 activate/0]).

%-export([upgrade_init/0]).
-export([look_for_test_license/1]). %used from has_testlicense.escript

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("RcsLM.hrl").
-include("lma.hrl").
-include("RmeLicenseSupport.hrl").




%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


%%% ----------------------------------------------------------
%%% @doc Return the permanent process of the LMA application
%%% @end
%%% ----------------------------------------------------------
-spec children() -> {ok, list()}.

children() ->
    {ok, [
	  {lmaGlms, {lmaGlms, start_link, []},
	   permanent, 1000, worker, [lmaGlms]},
	  {lmaLicenseUser, {lmaLicenseUser, start_link, []},
	   permanent, 1000, worker, [lmaLicenseUser]}
	 ]}.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% @doc Create the Mnesia tables needed for LM ECIM model.
%%% @end
%%% ----------------------------------------------------------
-spec instPhParallel_init(list()) -> ok.

instPhParallel_init(DbNodes)->
    MO_Tables = [{lm, ?lm_types},
		 {featureKey, ?featureKey_types},
		 {capacityKey, ?capacityKey_types},
		 {emergencyUnlock, ?emergencyUnlock_types},
		 {integrationUnlock, ?integrationUnlock_types},
		 %% {maintenanceUnlock, ?maintenanceUnlock_types}, % Not used
		 %% {systemTriggeredUnlock, ?systemTriggeredUnlock_types}, % Not used
		 {autonomousMode, ?autonomousMode_types},
		 {gracePeriod, ?gracePeriod_types},
		 {keyFileManagement, ?keyFileManagement_types},
		 {keyFileInformation, ?keyFileInformation_types},
		 %% {warningThreshold, ?warningThreshold_types},
		 {featureState, ?featureState_types},
		 {capacityState, ?capacityState_types},
		 {licenseSupport, ?licenseSupport_types}],
    ok = create_mo_table(MO_Tables, DbNodes),

    %% licenseManagerSoftwareData and featureKeySoftwareData are software parameters 
    %% and these should be saved in case of rollbacks
    GlmsTables = [{licenseManagerSoftwareData, [index, info]},
		  {featureKeySoftwareData, [index, info]},
		  {licenseSupportSoftwareData, [index, info]},
		  {licenseSupportPersistentData, [index, info]},
		  {licenseManagerPersistentData, [index, info]},
		  {keyFilePersistentData, [index, info]},
		  {featureKeyPersistentData, [index, info]},
		  {eUPersistentData, [index, info]},
		  {iUPersistentData, [index, info]},
		  {pUPersistentData, [index, info]},
		  {amPersistentData, [index, info]},
		  {featureStateSoftwareData, [index, info]},
		  {featureStatePersistentData, [index, info]},
		  {capacityStateSoftwareData, [index, info]},
		  {capacityStatePersistentData, [index, info]},
		  {capacityKeySoftwareData, [index, info]},
		  {capacityKeyPersistentData, [index, info]},
		  {lma_KFLocation, [index, info, sequenceNo, testMom]},
		  {lmaModel, [index, transId]},
		  {lmaLicenseSupportModel, [index, transId]},
		  {lmaLicenseKeyData, [keyId, prodNo, prodRev]}],

    ok = create_glms_table(GlmsTables, DbNodes),
    file:make_dir(lmaLib:get_lma_dir()),
    sysServer:register_file_owner(?BLOCK, lmaI),
    ok.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% @doc Create the licensing log
%%% @end
%%% ----------------------------------------------------------
-spec instPhParallel_init_data() -> ok.

instPhParallel_init_data()->
    OptList = [{maxSize, 1}, {rotatingSegments, 3}, {public, false}],
    logI:create_log("LicensingLog", OptList),
    swmI:register_appdata_receiver("license", lmaAppData),
    
    {atomic, _} = mnesia:transaction(fun() -> do_init_data_tables() end),
    ok.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
%%% ----------------------------------------------------------
%%% @doc Run post initialization functions
%%% This involves enabling netconf AVC for RcsLM
%%% @end
%%% ----------------------------------------------------------
-spec instPhParallel_post_init() -> ok.

instPhParallel_post_init()->
    comsaI:register_subscriptions(
      "RmeLicenseSupport", [{"LicenseSupport", licenseSupport}]),
    comsaI:register_subscriptions(
      "RcsLM", [{"Lm", lm},
		{"FeatureKey", featureKey},
		{"CapacityKey", capacityKey},
		{"EmergencyUnlock", emergencyUnlock},
		{"IntegrationUnlock", integrationUnlock},
		{"MaintenanceUnlock", maintenanceUnlock},
		{"SystemTriggeredUnlock", systemTriggered},
		{"AutonomousMode", autonomousMode},
		{"GracePeriod", gracePeriod},
		{"KeyFileManagement", keyFileManagement},
		{"KeyFileInformation", keyFileInformation},
		{"WarningThreshold", warningThreshold},
		{"FeatureState", featureState},
		{"CapacityState", capacityState}]), 
    ok.


%%% ----------------------------------------------------------
%%% @doc Activate call
%%% @end
%%% ----------------------------------------------------------
-spec activate() -> ok.

activate() ->
    lmaGlms:activate(),
    ok.

%%% ----------------------------------------------------------
%%% -type upgrade_init()->                                  %#
%%%     ok                                                  %#
%%% Input: Type: atom() 
%%%        OldVersion: string()
%%% Output: 
%%% Exceptions: 
%%% Description: Application code change for XXX.
%%% ----------------------------------------------------------
%upgrade_init()->
%    ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% ----------------------------------------------------------
%%% #           create_mo_table(Tables, DbNodes)
%%% Input:      
%%% Output:     ok
%%% Exceptions: 
%%% Description: Creates RAM copie tables for each MO class
%%% ----------------------------------------------------------
create_mo_table([], _DbNodes) ->
    ok;
create_mo_table([{Table, Types}| Rest], DbNodes) ->
    Fields = [Field||{Field, _}<-Types],
    {atomic, ok} =
	clhI:mnesia_create_table(Table, [{type, set},
					 {disc_copies, DbNodes},
					 {attributes, Fields} |
					 add_clh_option(Table)]),
    create_mo_table(Rest, DbNodes).

%%% ----------------------------------------------------------
%%% #           create_glms_table(Tables, DbNodes)
%%% Input:      
%%% Output:     ok
%%% Exceptions: 
%%% Description: Creates tables for the GLMS process
%%% ----------------------------------------------------------
create_glms_table([], _DbNodes) ->
    ok;
create_glms_table([{Table, Fields}| Rest], DbNodes) ->
    {atomic, ok} =
	clhI:mnesia_create_table(Table, [{type, set},
					 {disc_copies, DbNodes},
					 {attributes, Fields} |
					 add_clh_option(Table)]),
    create_glms_table(Rest, DbNodes).


%%% ###########################################################################
%%% add_clh_option
%%%
%%% Add option clh_changeCopyType_installation for large tables in order for
%%% CLH to optimize disc operations. See also edoc for clhI.
%%%
%%% ###=====================================================================###
add_clh_option(_) ->
    [].

%%% ----------------------------------------------------------
%%% @doc Initiate the LMA tables and if upgrade is ongoing copy the old tabels.
%%% @end
%%% ----------------------------------------------------------
do_init_data_tables() ->
    logI:register_esi_dir(lmaLib:get_lma_dir()),
    case swmI:is_upgrade_ongoing() of
	true ->
	    logI:write_log("LicensingLog", "lmaDataInit", info, "Upgrade is ongoing, copy old "
			   "tables"),
	    swmI:copy_old_table(lm),
	    swmI:copy_old_table(emergencyUnlock),
	    swmI:copy_old_table(integrationUnlock),
	    swmI:copy_old_table(autonomousMode),
	    swmI:copy_old_table(licenseManagerSoftwareData),
	    swmI:copy_old_table(licenseSupportSoftwareData),
	    %% swmI:copy_old_table(licenseSupportPersistentData),
	    swmI:copy_old_table(featureKeySoftwareData),
	    swmI:copy_old_table(licenseManagerPersistentData),
	    swmI:copy_old_table(keyFilePersistentData),
	    swmI:copy_old_table(featureKeyPersistentData),
	    %% swmI:copy_old_table(eUPersistentData),
	    swmI:copy_old_table(iUPersistentData),
	    swmI:copy_old_table(pUPersistentData),
	    swmI:copy_old_table(amPersistentData),
	    swmI:copy_old_table(featureStateSoftwareData),
	    swmI:copy_old_table(featureStatePersistentData),
	    %% swmI:copy_old_table(lma_KFLocation),
	    swmI:copy_old_table(capacityStateSoftwareData),
	    swmI:copy_old_table(capacityStatePersistentData),
	    swmI:copy_old_table(capacityKeySoftwareData),
	    swmI:copy_old_table(capacityKeyPersistentData),
        swmI:copy_old_table(licenseSupport),
	    
	    [Table] = swmI:all_objects(keyFileManagement),
	    mnesia:write(Table#keyFileManagement{reportProgress = undefined}),

	    update_table(swmI:all_objects(lma_KFLocation)),

	    eu_from_table_to_disk(mnesia:dirty_all_keys(eUPersistentData)),
	    licensesupport_from_table_to_disk(mnesia:dirty_all_keys(licenseSupportPersistentData)), % SP086

	    
	    try ok = swmI:copy_old_table(lmaModel),
		 logI:write_log("LicensingLog", "lmaDataInit", info, "Upgrade ongoing, "
				++"ok to copy lmaModel table")
	    catch _ErrClass : _ErrReason ->
		    logI:write_log("LicensingLog", "lmaDataInit", info, "Upgrade ongoing, "
				   ++"failed to copy lmaModel table or the table do not exist")
	    end;
	false ->
	    logI:write_log("LicensingLog", "lmaDataInit", info, "Initiating MOM tables with default"
			   " values"),
	    LM = #lm{lmId = {"1","1","1"},
		     fingerprintUpdateable = true,
		     lmState = ?LmState_LOCKED},
	    EmergencyUnlock = #emergencyUnlock{emergencyUnlockId = {"1","1","1","1"},
					       activationState = ?LmActivationState_INACTIVE,
					       activationsLeft = 2},
	    IntegrationUnlock = #integrationUnlock{integrationUnlockId = {"1","1","1","1"},
						   activationState = ?LmActivationState_INACTIVE,
						   activationsLeft = 1},
	    AutonomousMode = #autonomousMode{autonomousModeId = {"1","1","1","1"},
					     activationState = ?LmActivationState_INACTIVE},
	    KeyFileManagement = #keyFileManagement{keyFileManagementId = {"1","1","1","1"}},


	    mnesia:write(LM),
	    mnesia:write(EmergencyUnlock),
	    mnesia:write(IntegrationUnlock),
	    mnesia:write(AutonomousMode),
	    mnesia:write(KeyFileManagement),
	    
	    LicenseSupport = #licenseSupport{licenseSupportId = {"1","1","1"}},                  
	    
	    mnesia:write(LicenseSupport),
	    %% WP4530 - Check if a LKF file should be installed on the node and search 
	    %% for hidden MOM CXC4011959
	    case aicI:get_lkf_fn() of
		{ok, LKFPath} ->
		    case lmaLkf:verify_lkf_file(LKFPath) of
			valid ->
			    case file:read_file(LKFPath) of
				{ok, FileData} ->
				    look_for_test_license(sysEnv:rcs_mode_2(), FileData),
				    CXCTag = list_to_binary(["id=\"CXC4011959"]),
				    case binary:match(FileData, CXCTag) of
					nomatch ->
					    logI:write_log("LicensingLog", "lmaDataInit", info,
							   "Ordinary MOM should be loaded"),
					    lmaLib:update_lkf_table(false);
					_Found ->
					    logI:write_log("LicensingLog", "lmaDataInit", warning,
							   "Test MOM should be loaded..."),
					    lmaLib:update_lkf_table(true)
				    end;
				_Error ->
				    do_nada
			    end;
			_Error ->
			    do_nada
		    end;
		_Error ->
		    do_nada
	    end
    end.
    
look_for_test_license(vrcs, FileData) ->
    case look_for_test_license(FileData) of
	false ->
	    logI:write_log("LicensingLog", "lmaDataInit", info,
			   "5G cloud test license NOT found - CERT will close the node"),
	    call_cert(false);
	true ->
	    logI:write_log("LicensingLog", "lmaDataInit", warning,
			   "5G cloud test license found - the node will NOT be closed"),
	    call_cert(true)
    end;
look_for_test_license(_, _FileData) ->
    do_nada.

look_for_test_license(FileData) ->
    %% Look for the 5gCloudNodeTestLicense CXC4012117
    CXCTag = list_to_binary(["id=\"CXC4012117"]),
    case binary:match(FileData, CXCTag) of
	nomatch ->
	    false;
	_Found ->
	    true
    end.    

call_cert(Value) ->
    try apply(certI, update_test_license_vnode, [Value]) of
	_ ->
	    ok
    catch _T:_E ->
	    do_nada
    end.


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
update_table([]) ->
    logI:write_log("LicensingLog", "lmaDataInit", info, "Upgrade ongoing but no #lma_KFLocation"
		   " to upgrade"),
    ok;
update_table([Tuple]) ->
    update(tuple_size(Tuple), Tuple).


update(4, {lma_KFLocation, Index, Info, No}) ->
    logI:write_log("LicensingLog", "lmaDataInit", info, "Upgrade ongoing, add testMom = false"),
    NewTab = #lma_KFLocation{index = Index,
			     info = Info,
			     sequenceNo = No,
			     testMom = false},
    mnesia:write(NewTab),
    ok;
update(5, #lma_KFLocation{index = _Index, info = _Info, sequenceNo = _No, testMom = _Mom}) ->
    %% The lma_KFLocation doesn't needs to be updated
    logI:write_log("LicensingLog", "lmaDataInit", info, "Upgrade ongoing, testMom already added"),
    swmI:copy_old_table(lma_KFLocation),
    ok.

eu_from_table_to_disk([]) ->
    logI:write_log("LicensingLog", "lmaDataInit", info, "Upgrade ongoing but no EU to convert"
		   " from mnesia to disk"),
    ok;
eu_from_table_to_disk(_Key) ->
    logI:write_log("LicensingLog", "lmaDataInit", info, "Upgrade ongoing convert EU"
		   " from mnesia to disk"),
    [{eUPersistentData, _Index, Info}] = swmI:all_objects(eUPersistentData),
    File = filename:join(lmaLib:get_lma_dir(), "eUPersistentData"),
    Bytes = erlang:term_to_binary(Info),
    case file:write_file(File, Bytes) of
	ok ->
	    ok;
	Error ->
	    sysInitI:error_report([{?MODULE, eu_from_table_to_disk},
				       {file, File},
				       {content, Bytes},
				       Error]),
	    Error
    end.



licensesupport_from_table_to_disk([]) ->
    logI:write_log("LicensingLog", "lmaDataInit", info, "Upgrade ongoing but no LicenseSupport to convert"
		   " from mnesia to disk"),
    ok;
licensesupport_from_table_to_disk(_Key) ->
    logI:write_log("LicensingLog", "lmaDataInit", info, "Upgrade ongoing convert LicenseSupport"
		   " from mnesia to disk"),
    [{licenseSupportPersistentData, _Index, Info}] = swmI:all_objects(licenseSupportPersistentData),
    File = filename:join(lmaLib:get_lma_dir(), "licenseSupportPersistentData"),
    Bytes = erlang:term_to_binary(Info),
    case file:write_file(File, Bytes) of
	ok ->
	    ok;
	Error ->
	    sysInitI:error_report([{?MODULE, licensesupport_from_table_to_disk},
				       {file, File},
				       {content, Bytes},
				       Error]),
	    Error
    end.
    
