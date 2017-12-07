%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmaLib.erl %
%%% Author:     etxpejn
%%% Description: Support module to write and read in mnesia tables.
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(lmaLib).
-vsn('/main/R2A/R3A/R4A/R5A/R9A/R12A/1').
-date('2017-11-02').
-author('echhedb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: lmaLinxHandler.erl %
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R3A/1      2014-11-26   etxpejn    Added capacity
%%% R3A/2      2014-12-05   etxpejn    Corrected create and modify GP MO.
%%% R3A/3      2015-03-26   etxpejn    Corrected HT59552, added ActiveFeatureKeyId
%%% R3A/4      2015-03-30   etxpejn    Added ActiveFeatureKeyId for capacity state
%%% R3A/5      2015-04-09   etxpejn    Changed impl. of HT59552, ref update at create/delete MO
%%% R3A/8      2015-05-25   etxpejn    Corr HT77465, do not save featureState
%%% R4A/1      2015-05-29   etxpejn    Corrected HT77481, set_fingerprint and validate
%%% R4A/2      2015-06-18   etxpejn    Added update_lkf_table
%%% R4A/6      2015-08-18   etxpejn    Removed warning printouts
%%% R4A/7      2015-09-17   etxpejn    Changed error_logger to sysInitI
%%% R5A/1      2015-11-09   etxpejn    Added featureState & serviceState to CapacityState MO
%%% R5A/2      2015-12-03   etxpejn    Take timeOfLastStatusUpdate from GLMS
%%% R9A/1      2017-01-24   etxpejn    HV58425 Changed NULL to undefined
%%% R12A/1     2017-10-31   echhedb    SP086: Added function update_licensesupport(..),
%%%                                    and added element Shared to 3 existing functions.
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([change_sec_to_ecim_date/1,
	 convert_pu_state/1,
	 create_capacity_key/11,
	 create_capacity_state/10,
	 create_feature_key/7,
	 create_feature_state/7,
	 create_grace_period/6,
	 delete_mo/2,
	 error_msg/2,
	 get_LKF_path/1,
	 get_lma_dir/0,
	 update_am/2,
	 update_eu/3,
	 update_licensesupport/1,
	 update_capacity_key/11,
	 update_capacity_state/9,
	 update_capacity_unit/3,
	 update_feature_key/7,
	 update_feature_state/7,
	 update_grace_period/6,
	 update_iu/3,
	 update_name/3,
	 update_keyfile_information/4,
	 update_lm/5,
	 update_lkf_table/1,
	 update_lkf_table/2,
	 update_lkf_table/3,
	 update_progress_struct/1]).

-include("lma.hrl").
-include("RcsLM.hrl").
-include("RmeLicenseSupport.hrl").
-include("lmaGlmsSig.hrl").

-define(LICENSE_FILE, "licensekey.xml").
-define(LICENSE_FILE_TMP, "tmp.xml").

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% @doc Create CapacityKey MO.
create_capacity_key(NoLimit, Value, ValidFrom, Expiration, LicensedCapacityLimitReached, 
		    GrantedCapacityLevel, CapacityUnit, CapacityKeyId, KeyId, Name, ProductType) ->
    {atomic, _} = 
	mnesia:transaction(
	  fun() -> do_create_capacity_key(NoLimit, Value, ValidFrom, Expiration, 
					  LicensedCapacityLimitReached, GrantedCapacityLevel, 
					  CapacityUnit, CapacityKeyId, KeyId, Name, ProductType),
		   update_ref_to_key_in_state(capacityKey, KeyId)
	  end).

do_create_capacity_key(NoLimit, Value, ValidFrom, Expiration, LicensedCapacityLimitReached, 
		       GrantedCapacityLevel, CapacityUnit, CapacityKeyId, KeyId, Name, 
		       ProductType) ->
    Id = ?ID(CapacityKeyId),
    NL = boolean_to_ecim(NoLimit),
    LimitReached = boolean_to_ecim(LicensedCapacityLimitReached),
    Dn = find_dn(mnesia:all_keys(capacityState), KeyId, [], capacityState),
    ValidFrom_ECIM = change_sec_to_ecim_date(ValidFrom),
    Expiration_ECIM = change_sec_to_ecim_date(Expiration),
        
    CapacityKeyTab = #capacityKey{capacityKeyId = Id,
				  licensedCapacityLimit = #'LmCapacityValue'{value = Value,
									     noLimit = NL},
				  capacityUnit = CapacityUnit,
				  grantedCapacityLevel = GrantedCapacityLevel,
				  licensedCapacityLimitReached = LimitReached,
				  state = Dn,
				  name = Name,
				  validFrom = ValidFrom_ECIM,
				  expiration = Expiration_ECIM,
				  keyId = KeyId,
				  productType = ProductType},
    ok = mnesia:write(CapacityKeyTab).


%%% @doc Create CapacityState MO.
create_capacity_state(NoLimit, Value, LicensedCapacityLimitReached, GrantedCapacityLevel, 
		      LicenseState, CapacityUnit, CapacityStateId, Description, KeyId, 
		      ActiveCapacityKeyId) ->
    {atomic, _} = 
	mnesia:transaction(
	  fun() -> 
		  do_create_capacity_state(NoLimit, Value, LicensedCapacityLimitReached, 
					   GrantedCapacityLevel, LicenseState, CapacityUnit, 
					   CapacityStateId, Description, KeyId, 
					   ActiveCapacityKeyId) 
		       end).

do_create_capacity_state(NoLimit, Value, LicensedCapacityLimitReached, GrantedCapacityLevel, 
			 LicenseState, CapacityUnit, CapacityStateId, Description, KeyId, 
			 _ActiveCapacityKeyId) ->
    Id = ?ID(CapacityStateId),
    Dn = find_dn(mnesia:all_keys(capacityKey), KeyId, [], capacityKey),
    LimitReached = boolean_to_ecim(LicensedCapacityLimitReached),
    NL = boolean_to_ecim(NoLimit),

    CapacityStateTab = #capacityState{capacityStateId = Id,
				      licenseState = LicenseState,
				      description = Description,
				      capacityKey = Dn,
				      keyId = KeyId,
				      currentCapacityLimit = #'LmCapacityValue'{value = Value, 
										noLimit = NL},
				      capacityUnit = CapacityUnit,
				      grantedCapacityLevel = GrantedCapacityLevel,
				      licensedCapacityLimitReached = LimitReached,
				      featureState = ?LmFeatureState_ACTIVATED,
				      serviceState = service_state(LicenseState)},
    ok = mnesia:write(CapacityStateTab).

service_state(?LmLicenseState_DISABLED) ->
    ?LmServiceState_INOPERABLE;
service_state(?LmLicenseState_ENABLED) ->
    ?LmServiceState_OPERABLE.

%%% @doc Create FeatureKey MO.
-spec create_feature_key(string(),string(),integer(),integer(),string(),string(),boolean()) -> 
				{aborted,string()} | {atomic, ok}.
create_feature_key(FeatureKeyId, Name, ValidFrom, Expiration, KeyId, ProductType, Shared) ->
    {atomic, _} = mnesia:transaction(
		    fun() -> do_create_feature_key(FeatureKeyId, Name, ValidFrom, 
						   Expiration, KeyId, ProductType, Shared),
			     update_ref_to_key_in_state(featureKey, KeyId)
		    end).

do_create_feature_key(FeatureKeyId, Name, ValidFrom, Expiration, KeyId, ProductType, Shared) ->
    FeatureId = ?ID(FeatureKeyId),
    ValidFrom_ECIM = change_sec_to_ecim_date(ValidFrom),
    Expiration_ECIM = change_sec_to_ecim_date(Expiration),
    Dn = find_dn(mnesia:all_keys(featureState), KeyId, [], featureState),

    FeatureKey = #featureKey{featureKeyId = FeatureId,
			     state = Dn,
			     name = Name,
			     validFrom = ValidFrom_ECIM,
			     expiration = Expiration_ECIM,
			     keyId = KeyId,
			     productType = ProductType,
			     shared = boolean_to_ecim(Shared)},
    ok = mnesia:write(FeatureKey).

%%% @doc Create FeatureState MO.
-spec create_feature_state(string(),integer(),integer(),integer(),string(),string(),string()) ->
				  {aborted,string()} | {atomic, ok}.
create_feature_state(Id, FeatureState, LicenseState, ServiceState, Description, KeyId, 
		     ActiveFeatureKeyId) ->
     {atomic, _} = mnesia:transaction(
		     fun() -> do_create_feature_state(Id, FeatureState, LicenseState, 
						      ServiceState, Description, KeyId, 
						      ActiveFeatureKeyId) end).

do_create_feature_state(Id, FeatureState, LicenseState, ServiceState, Description, KeyId, 
			_ActiveFeatureKeyId) ->
    FeatureStateId = ?ID(Id),
    Dn = find_dn(mnesia:all_keys(featureKey), KeyId, [], featureKey),

    FeatureStateTab = #featureState{featureStateId = FeatureStateId,
				    featureState = FeatureState,
				    licenseState = LicenseState,
				    serviceState = ServiceState,
				    description = Description,
				    featureKey = Dn,
				    keyId = KeyId},
    ok = mnesia:write(FeatureStateTab).

create_grace_period(Expiration, ConfiguredLength, ConfiguredResetThreshold, 
		    ConfiguredActivationThreshold, GracePeriodState, GracePeriodId) ->
    {atomic, _} = 
	mnesia:transaction(
	  fun() -> 
		  do_create_grace_period(Expiration, ConfiguredLength, ConfiguredResetThreshold, 
					 ConfiguredActivationThreshold, GracePeriodState, 
					 GracePeriodId) end).

do_create_grace_period(0, _ConfiguredLenght, _ConfiguredResetThreshold, 
		       _ConfiguredActivationThreshold, GracePeriodState, GracePeriodId) ->
    Id = ?GP_ID(GracePeriodId),
    GracePeriodTab = #gracePeriod{gracePeriodId = Id,
     				  gracePeriodState = GracePeriodState},
    ok = mnesia:write(GracePeriodTab);
do_create_grace_period(Expiration, _ConfiguredLenght, _ConfiguredResetThreshold, 
		       _ConfiguredActivationThreshold, GracePeriodState, GracePeriodId) ->
    Id = ?GP_ID(GracePeriodId),
    Expiration_ECIM = change_sec_to_ecim_date(Expiration),
    GracePeriodTab = #gracePeriod{gracePeriodId = Id,
     				  gracePeriodState = GracePeriodState,
    				  gracePeriodExpiration = Expiration_ECIM},
    ok = mnesia:write(GracePeriodTab).

%%% @doc Delete MO.
-spec delete_mo(string(), atom()) -> {aborted,string()} | {atomic, ok}.
delete_mo(Id, featureKey) ->
    {atomic, _} = 
	mnesia:transaction(fun() -> do_delete_mo(Id, featureKey),
				    %% Remove version from key "CXC123456_1" -> "CXC123456"
				    [BasId, _Version] = string:tokens(Id, "_"),
				    update_ref_to_key_in_state(featureKey, BasId)
			   end);
delete_mo(Id, capacityKey) ->
    {atomic, _} = 
	mnesia:transaction(fun() -> do_delete_mo(Id, capacityKey),
				    %% Remove version from key "CXC123456_1" -> "CXC123456"
				    [BasId, _Version] = string:tokens(Id, "_"),
				    update_ref_to_key_in_state(capacityKey, BasId)
			   end);			
delete_mo(Id, Mo) ->
    {atomic, _} = mnesia:transaction(fun() -> do_delete_mo(Id, Mo) end).

do_delete_mo(Id, gracePeriod) ->
    MoId = ?GP_ID(Id),
    ok = mnesia:delete({gracePeriod, MoId});
do_delete_mo(Id, Mo) ->
    MoId = ?ID(Id),
    ok = mnesia:delete({Mo, MoId}).


%%% @private
-spec get_LKF_path(atom()) -> string().
get_LKF_path(license) ->
    filename:join([get_lma_dir(), ?LICENSE_FILE]);
get_LKF_path(tmp) ->
    filename:join([get_lma_dir(), ?LICENSE_FILE_TMP]).

%%% @private
-spec get_lma_dir() -> string().
get_lma_dir() ->
    filename:join([sysEnv:rcs_dir(), ?BLOCK]).

%%% @doc Update AutonomousMode MO.
-spec update_am(integer(),integer()) -> {aborted,string()} | {atomic, ok}.
update_am(ActivationState, Expiration) ->
    {atomic, _} = mnesia:transaction(fun() -> do_update_am(ActivationState, Expiration) end).

do_update_am(ActivationState, Expiration) ->
    Expiration_ECIM = change_sec_to_ecim_date(Expiration),
    AutonomousMode = #autonomousMode{autonomousModeId = {"1","1","1","1"},
				     expiration = Expiration_ECIM,
    				     activationState = ActivationState},
    ok = mnesia:write(AutonomousMode).

%%% @doc Update EmergencyUnlock MO.
-spec update_eu(integer(),integer(),integer()) -> {aborted,string()} | {atomic, ok}.
update_eu(ActivationState, Expiration, ActivationsLeft) ->
    {atomic, _} = mnesia:transaction(fun() -> do_update_eu(ActivationState, Expiration, 
							    ActivationsLeft) end).

do_update_eu(ActivationState, Expiration, ActivationsLeft) ->
    Expiration_ECIM = change_sec_to_ecim_date(Expiration),
    EmergencyUnlock = #emergencyUnlock{emergencyUnlockId = {"1","1","1","1"},
				       activationState =  ActivationState,
				       expiration = Expiration_ECIM,
				       activationsLeft = ActivationsLeft},
    ok = mnesia:write(EmergencyUnlock).


%%% @doc Update LicenseSupport MO. SP086
-spec update_licensesupport(string()) -> {aborted,string()} | {atomic, ok}.
update_licensesupport(LicenseAreaId) ->
	    %logI:write_log("LicensingLog", "lmaLib", info, "Entered update_licensesupport. New licenseAreaId: " ++ LicenseAreaId),
    {atomic, _} = mnesia:transaction(fun() -> do_update_licensesupport(LicenseAreaId) end).

do_update_licensesupport(LicenseAreaId) ->
    LicenseSupport = #licenseSupport{licenseSupportId = {"1","1","1"},
				       licenseAreaId =  LicenseAreaId},
    ok = mnesia:write(LicenseSupport).



%%% @doc Update CapacityKey MO.
update_capacity_key(NoLimit, Value, ValidFrom, Expiration, LicensedCapacityLimitReached, 
		    GrantedCapacityLevel, CapacityUnit, CapacityKeyId, KeyId, Name, ProductType) ->
    {atomic, _} = 
	mnesia:transaction(
	  fun() -> 
		  do_create_capacity_key(NoLimit, Value, ValidFrom, Expiration, 
					 LicensedCapacityLimitReached, GrantedCapacityLevel, 
					 CapacityUnit, CapacityKeyId, KeyId, Name, ProductType) 
	  end).

%%% @doc Update name attribute in CapacityKey or FeatureKey MO.
-spec update_name(string(),string(),atom()) -> {aborted,string()} | {atomic, ok}.
update_name(Id, Name, Mo) ->
    {atomic, _} = mnesia:transaction(fun() -> do_update_name(Id, Name, Mo) end).

do_update_name(Id, Name, Mo) ->
    MoId = ?ID(Id),
    [Obj] = mnesia:read(Mo, MoId),
    NewObj = case Mo of
		 capacityKey ->
		     Obj#capacityKey{name = Name};
		 featureKey ->
		     Obj#featureKey{name = Name}
	     end,
    ok = mnesia:write(NewObj).

%%% @doc Update CapacityState MO.
update_capacity_state(NoLimit, Value, LicensedCapacityLimitReached, LicenseState, 
		      GrantedCapacityLevel, CapacityStateId, Description, KeyId, 
		      ActiveCapacityKeyId) ->
     {atomic, _} = 
	mnesia:transaction(
	  fun() -> do_update_capacity_state(NoLimit, Value, LicensedCapacityLimitReached, 
					    LicenseState, GrantedCapacityLevel, CapacityStateId, 
					    Description, KeyId, ActiveCapacityKeyId)
	  end).

do_update_capacity_state(NoLimit, Value, LicensedCapacityLimitReached, LicenseState, 
			 GrantedCapacityLevel, CapacityStateId, Description, KeyId, 
			 _ActiveCapacityKeyId) ->
    Id = ?ID(CapacityStateId),
    Dn = find_dn(mnesia:all_keys(capacityKey), KeyId, [], capacityKey),
    NL = boolean_to_ecim(NoLimit),
    LimitReached = boolean_to_ecim(LicensedCapacityLimitReached),
    NewCapacityState = 
	case mnesia:read(capacityState, Id) of
	    [CapacityState] ->
		CapacityState#capacityState{licenseState = LicenseState,
					    description = Description,
					    capacityKey = Dn,
					    keyId = KeyId,
					    currentCapacityLimit = #'LmCapacityValue'{value = Value, 
										      noLimit = NL},
					    grantedCapacityLevel = GrantedCapacityLevel,
					    licensedCapacityLimitReached= LimitReached,
					    serviceState = service_state(LicenseState)};
	    [] ->
		#capacityState{capacityStateId = Id,
			       licenseState = LicenseState,
			       description = Description,
			       capacityKey = Dn,
			       keyId = KeyId,
			       currentCapacityLimit = #'LmCapacityValue'{value = Value, 
									 noLimit = NL},
			       capacityUnit = "",
			       grantedCapacityLevel = GrantedCapacityLevel,
			       licensedCapacityLimitReached= LimitReached,
			       featureState = ?LmFeatureState_ACTIVATED,
			       serviceState = service_state(LicenseState)}
	end,
    ok = mnesia:write(NewCapacityState).

%%% @doc Update unit attribute in CapacityKey MO.
-spec update_capacity_unit(string(),string(),atom()) -> {aborted,string()} | {atomic, ok}.
update_capacity_unit(Id, Unit, Mo) ->
    {atomic, _} = mnesia:transaction(fun() -> do_update_capacity_unit(Id, Unit, Mo) end).

do_update_capacity_unit(Id, CapacityUnit, Mo) ->
    MoId = ?ID(Id),
    [Obj] = mnesia:read(Mo, MoId),
    NewObj =
	case Mo of
	    capacityKey ->
		Obj#capacityKey{capacityUnit = CapacityUnit};
	    capacityState ->
		Obj#capacityState{capacityUnit = CapacityUnit}
	end,
    ok = mnesia:write(NewObj).	

%%% @doc Update FeatureKey MO.
-spec update_feature_key(string(),string(),integer(),integer(),string(),string(),boolean()) -> 
				{aborted,string()} | {atomic, ok}.
update_feature_key(Id, Name, ValidFrom, Expiration, KeyId, ProductType, Shared) ->
    {atomic, _} = mnesia:transaction(
		    fun() -> do_create_feature_key(Id, Name, ValidFrom, Expiration, KeyId, 
						   ProductType, Shared) end).

%%% @doc Update FeatureState MO.
-spec update_feature_state(string(),integer(),integer(),integer(),string(),string(),string()) -> 
				  {aborted,string()} | {atomic, ok}.
update_feature_state(Id, FeatureState, LicenseState, ServiceState, Description, KeyId, 
		     ActiveFeatureKeyId) ->
    {atomic, _} = 
	mnesia:transaction(
	  fun() -> do_update_feature_state(Id, FeatureState, LicenseState, ServiceState, 
					   Description, KeyId, ActiveFeatureKeyId) end).

do_update_feature_state(Id, FeatureState, LicenseState, ServiceState, Description, KeyId, 
			_ActiveFeatureKeyId) ->
    FeatureStateId = ?ID(Id),
    Dn = find_dn(mnesia:all_keys(featureKey), KeyId, [], featureKey),
    NewFeatureStateTab = 
	case mnesia:read(featureState, FeatureStateId) of
	     [FeatureStateTab] ->
		FeatureStateTab#featureState{featureStateId = FeatureStateId,
					     %% featureState = FeatureState, HT77465 
					     licenseState = LicenseState,
					     serviceState = ServiceState,
					     description = Description,
					     featureKey = Dn,
					     keyId = KeyId};
	    [] ->
		#featureState{featureStateId = FeatureStateId,
			      featureState = FeatureState,
			      licenseState = LicenseState,
			      serviceState = ServiceState,
			      description = Description,
			      featureKey = Dn,
			      keyId = KeyId}
	end,
    ok = mnesia:write(NewFeatureStateTab).

%%% @doc Update Grace Period MO.
update_grace_period(Expiration, ConfiguredLength, ConfiguredResetThreshold, 
		    ConfiguredActivationThreshold, GracePeriodState, GracePeriodId) ->
      {atomic, _} = 
	mnesia:transaction(
	  fun() -> do_create_grace_period(Expiration, ConfiguredLength,
					  ConfiguredResetThreshold, ConfiguredActivationThreshold, 
					  GracePeriodState, GracePeriodId) end).

%%% @doc Update IntegrationUnlock MO.
-spec update_iu(integer(),integer(),integer()) -> {aborted,string()} | {atomic, ok}.
update_iu(ActivationState, Expiration, ActivationsLeft) ->
      {atomic, _} = mnesia:transaction(
		       fun() -> do_update_iu(ActivationState, Expiration, ActivationsLeft) end).

do_update_iu(ActivationState, Expiration, ActivationsLeft) ->
    Expiration_ECIM = change_sec_to_ecim_date(Expiration),
    IntegrationUnlock = #integrationUnlock{integrationUnlockId = {"1","1","1","1"},
					   activationState = ActivationState,
					   expiration = Expiration_ECIM,
					   activationsLeft = ActivationsLeft},
    ok = mnesia:write(IntegrationUnlock).

%%% @doc Update KeyFileInforamtion MO.
-spec update_keyfile_information(integer(),integer(),integer(),string()) -> 
					{aborted,string()} | {atomic, ok}.
update_keyfile_information(SequenceNumber, InstallationTime, Locatable, ProductType) ->
    {atomic, _} = mnesia:transaction(
		     fun() -> do_update_keyfile_information(SequenceNumber, InstallationTime, 
							    Locatable, ProductType) end).

do_update_keyfile_information(SequenceNumber, InstallationTime, Locatable, ProductType) ->
    Locatable_ECIM = boolean_to_ecim(Locatable),
    InstallationTime_ECIM = change_sec_to_ecim_date(InstallationTime),
    KeyFileInformation = #keyFileInformation{keyFileInformationId = {"1","1","1","1","1"},
					     sequenceNumber = SequenceNumber,
					     installationTime = InstallationTime_ECIM,
					     locatable = Locatable_ECIM,
					     productType = ProductType},    
    ok = mnesia:write(KeyFileInformation),

    case mnesia:read(lma_KFLocation, 1) of
	[Obj] ->
	    case SequenceNumber of
		0 ->
		    ok;
		_Else ->
		    NewObj = Obj#lma_KFLocation{sequenceNo = SequenceNumber},
		    ok = mnesia:write(NewObj)
	    end;
	[] ->
	    ok
    end.

%%% @doc Update Lm MO.
-spec update_lm(string(),integer(),integer(),integer(),integer()) -> 
		       {aborted,string()} | {atomic, ok}.
update_lm(Fingerprint, FingerprintUpdateable, LmState, LastInventoryChange, 
	  LastLicenseInventoryRefresh) ->
    {atomic, _} = mnesia:transaction(
		     fun() -> do_update_lm(Fingerprint, FingerprintUpdateable, LmState, 
					   LastInventoryChange, LastLicenseInventoryRefresh) end).

do_update_lm(_Fingerprint, FingerprintUpdateable, LmState, LastInventoryChange, 
	     LastLicenseInventoryRefresh) ->
    FingerprintUpdateable_ECIM = boolean_to_ecim(FingerprintUpdateable),
    LastInventoryChange_ECIM = change_sec_to_ecim_date(LastInventoryChange), 
    LastLicenseInventoryRefresh_ECIM = change_sec_to_ecim_date(LastLicenseInventoryRefresh),
    LmState_ECIM = convert_lm_state(LmState),

    [LmTab] = mnesia:read(lm, {"1","1","1"}),
    
    LM = LmTab#lm{lmId = {"1","1","1"},
		  %% fingerprint = Fingerprint,
		  fingerprintUpdateable = FingerprintUpdateable_ECIM,
		  lmState = LmState_ECIM,
		  lastInventoryChange = LastInventoryChange_ECIM,
		  lastLicenseInventoryRefresh = LastLicenseInventoryRefresh_ECIM},
    
    ok = mnesia:write(LM).

%%% @doc Update lma_KFLocation table
update_lkf_table(TestMom) ->
    {atomic, _} = mnesia:transaction(
		    fun() -> do_update_lkf_table(TestMom) end).

%%% @doc Update lma_KFLocation table
update_lkf_table(FileData, SequenceNo) ->
    {atomic, _} = mnesia:transaction(
		    fun() -> do_update_lkf_table(FileData, SequenceNo) end).

%%% @doc Update lma_KFLocation table
update_lkf_table(FileData, SequenceNo, TestMom) ->
    {atomic, _} = mnesia:transaction(
		    fun() -> do_update_lkf_table(FileData, SequenceNo, TestMom) end).


do_update_lkf_table(TestMom) ->
    case mnesia:read(lma_KFLocation, 1) of
	[Obj] ->
	    NewObj = Obj#lma_KFLocation{testMom = TestMom},
	    ok = mnesia:write(NewObj);
	[] ->
	    ok = mnesia:write(#lma_KFLocation{index = 1, 
					      testMom = TestMom})
    end.

do_update_lkf_table(FileData, SequenceNo) ->
    case mnesia:read(lma_KFLocation, 1) of
	[Obj] ->
	    NewObj = Obj#lma_KFLocation{info = FileData,
					sequenceNo = SequenceNo},
	    ok = mnesia:write(NewObj);
	[] ->
	    ok = mnesia:write(#lma_KFLocation{index = 1, 
					      info = FileData,
					      sequenceNo = SequenceNo})
    end.

do_update_lkf_table(FileData, SequenceNo, TestMom) ->
    case mnesia:read(lma_KFLocation, 1) of
	[Obj] ->
	    NewObj = Obj#lma_KFLocation{info = FileData,
					sequenceNo = SequenceNo,
					testMom = TestMom},
	    ok = mnesia:write(NewObj);
	[] ->
	    ok = mnesia:write(#lma_KFLocation{index = 1, 
					      info = FileData,
					      sequenceNo = SequenceNo, 
					      testMom = TestMom})
    end.


%%% @doc Update reportProgress attribute in KeyFileManagement MO.
-spec update_progress_struct(list()) -> {aborted, string()} | {atomic, ok}.
update_progress_struct(ProgressData) ->
    {atomic, _} = mnesia:transaction(fun() -> do_update_progress_struct(ProgressData) end).

do_update_progress_struct(ProgressData) ->
    [KeyFileManagement] = mnesia:read(keyFileManagement, {"1","1","1","1"}),
    OldStruct = KeyFileManagement#keyFileManagement.reportProgress,
    NewStruct = case OldStruct of
		    undefined ->
			update(ProgressData, #'AsyncActionProgress'{});
		    _Else ->
			update(ProgressData, OldStruct)
		end,
    mnesia:write(KeyFileManagement#keyFileManagement{reportProgress=NewStruct}).


update([{actionName, Name}|Data], Progress) ->
    update(Data, Progress#'AsyncActionProgress'{actionName = Name});
update([{additionalInfo, ""}|Data], Progress) ->
    update(Data, Progress);
update([{additionalInfo, Info}|Data], Progress) ->
    NewAI = case Progress#'AsyncActionProgress'.additionalInfo of
		AI when is_list(AI) -> 
		    case lists:last(AI) of
		    	Info ->
			    AI;
		    	_Else ->
			    info_msg("~s~n~n",[Info]),
			    AI++[Info]
		    end;
		_ -> 
		    info_msg("~s~n~n",[Info]),
		    [Info]
	    end,
    update(Data, Progress#'AsyncActionProgress'{additionalInfo = NewAI,
						progressInfo = Info});
update([{progressInfo, Info}|Data], P) ->
    update(Data, P#'AsyncActionProgress'{progressInfo = Info});
update([{progressPercentage, Percent}|Data], P) ->
    update(Data, P#'AsyncActionProgress'{progressPercentage = Percent});
update([{result, Result}|Data], P) ->
    update(Data, P#'AsyncActionProgress'{result = convert_progress_result(Result)});
update([{resultInfo, Info}|Data], P) ->
    update(Data, P#'AsyncActionProgress'{resultInfo = Info});
update([{state, State}|Data], P) ->
    update(Data, P#'AsyncActionProgress'{state = convert_action_state(State)});
update([{actionId, ActionId}|Data], P) ->
    update(Data, P#'AsyncActionProgress'{actionId = ActionId});
update([{timeActionStarted, Date}|Data], P) ->
    update(Data, P#'AsyncActionProgress'{timeActionStarted = change_sec_to_ecim_date(Date)});
update([{timeActionCompleted, Date}|Data], P) ->
    update(Data, P#'AsyncActionProgress'{timeActionCompleted = change_sec_to_ecim_date(Date)});
update([{timeOfLastStatusUpdate, Date}|Data], P) ->
    update(Data, P#'AsyncActionProgress'{timeOfLastStatusUpdate = change_sec_to_ecim_date(Date)});
%%% Using the additionalInfoClear "field" the additionalInfo sequence
%%% is cleared. This is not a real field, but updates the additionalInfo
update([{additionalInfoClear, Info}|Data], Progress) ->
    info_msg("~s~n~n",[Info]),
    update(Data, Progress#'AsyncActionProgress'{additionalInfo = [Info],
						progressInfo = Info});
update([X|Data], P) ->
    error_msg("Unknown progress header: ~p~n",[X]),
    update(Data, P);
update([], P) ->
    P.

update_ref_to_key_in_state(featureKey, Id) ->
    FeatureStateId = ?ID(Id),
    case mnesia:read(featureState, FeatureStateId) of
	[] ->
	    logI:write_log("LicensingLog", "lmaLib", warning, "update_ref_to_key_in_state, no "
			   "featureState found for: " ++ Id);
	[FeatureStateTab] ->
	    Dn = find_dn(mnesia:all_keys(featureKey), Id, [], featureKey),
	    NewFeatureStateTab = 
		FeatureStateTab#featureState{featureKey = Dn},
	    ok = mnesia:write(NewFeatureStateTab)
    end,
    ok;
update_ref_to_key_in_state(capacityKey, Id) ->
    CapacityStateId = ?ID(Id),
    case mnesia:read(capacityState, CapacityStateId) of
	[] ->
	    logI:write_log("LicensingLog", "lmaLib", warning, "update_ref_to_key_in_state, no "
			   "capacityState found for: " ++ Id);
	[CapacityStateTab] ->
	    Dn = find_dn(mnesia:all_keys(capacityKey), Id, [], capacityKey),
	    NewCapacityStateTab = 
		CapacityStateTab#capacityState{capacityKey = Dn},
	    ok = mnesia:write(NewCapacityStateTab)
    end,
    ok.

 
%%% @private
info_msg(_Format, [[]]) ->
    ok;
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

 	    
%%% @private
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).



boolean_to_ecim(0) ->
    false;
boolean_to_ecim(1) ->
    true.

%%% @private
change_sec_to_ecim_date(-1) ->
    %% Expiration is GLMS_NO_STOP_DATE i.e. -1
    undefined;
change_sec_to_ecim_date(0) ->
    %% No time to convert
    "";
change_sec_to_ecim_date(TimeInSec) ->
    MegaSec = TimeInSec div 1000000,
    Sec = TimeInSec rem 1000000,
    comsaI:iso_time({MegaSec, Sec, 0}, extended_zonefree).

convert_action_state(?GLMS_ACTION_STATE_CANCELLING) ->
    ?ActionStateType_CANCELLING;
convert_action_state(?GLMS_ACTION_STATE_RUNNING) ->
    ?ActionStateType_RUNNING;
convert_action_state(?GLMS_ACTION_STATE_FINISHED) ->
    ?ActionStateType_FINISHED;
convert_action_state(?GLMS_ACTION_STATE_CANCELLED) ->
    ?ActionStateType_CANCELLED.

convert_lm_state(?GLMS_LM_LOCKED) ->
    ?LmState_LOCKED;
convert_lm_state(?GLMS_LM_NORMAL) ->
    ?LmState_NORMAL;
convert_lm_state(?GLMS_LM_EMERGENCY_UNLOCK) ->
    ?LmState_EMERGENCY_UNLOCK;
convert_lm_state(?GLMS_LM_INTEGRATION_UNLOCK) ->
    ?LmState_INTEGRATION_UNLOCK;
convert_lm_state(?GLMS_LM_AUTONOMOUS_MODE) ->
    ?LmState_AUTONOMOUS_MODE.

convert_progress_result(?GLMS_SUCCESS) ->
    ?ActionResultType_SUCCESS;
convert_progress_result(?GLMS_FAILURE) ->
    ?ActionResultType_FAILURE;
convert_progress_result(?GLMS_NOT_AVAILABLE) ->
    ?ActionResultType_NOT_AVAILABLE.

%%% @private
convert_pu_state(0) ->
    "INACTIVE";
convert_pu_state(1) ->
    "ACTIVATED";
convert_pu_state(2) ->
    "ACTIVATED_EXPIRING";
convert_pu_state(3) ->
    "EXPIRED";
convert_pu_state(4) ->
    "DEACTIVATED".

find_dn([], _KeyId, AccDn, _Mo) ->
    AccDn;
find_dn([Id | RestIds], KeyId, AccDn, Mo) ->
    case mnesia:read(Mo, Id) of
    	[] ->
    	    find_dn(RestIds, KeyId, AccDn, Mo);
    	[Obj] ->
	    case Mo of
		capacityKey ->
		    case Obj#capacityKey.keyId of
			KeyId ->
			    %% Found the correcsponding capacityKey MO
			    {ME, SF, Lm, CK} = Id,
			    Dn = make_dn(["ManagedElement", ME,
					  "SystemFunctions", SF,
					  "Lm", Lm,
					  "CapacityKey", CK]),
			    find_dn(RestIds, KeyId, AccDn ++ [Dn], Mo);
			_Else ->
			    find_dn(RestIds, KeyId, AccDn, Mo)
		    end;
		featureKey ->
		    case Obj#featureKey.keyId of
			KeyId ->
			    %% Found the correcsponding featureKey MO
			    {ME, SF, Lm, FK} = Id,
			    Dn = make_dn(["ManagedElement", ME,
					  "SystemFunctions", SF,
					  "Lm", Lm,
					  "FeatureKey", FK]),
			    find_dn(RestIds, KeyId, AccDn ++ [Dn], Mo);
			_Else ->
			    find_dn(RestIds, KeyId, AccDn, Mo)
		    end;
		capacityState ->
		    case Obj#capacityState.keyId of
			KeyId ->
			    %% Found the correcsponding capacityState MO
			    {ME, SF, Lm, CS} = Id,
			    make_dn(["ManagedElement", ME,
				     "SystemFunctions", SF,
				     "Lm", Lm,
				     "CapacityState", CS]);
			_Else ->
			    find_dn(RestIds, KeyId, AccDn, Mo)
		    end;
		 featureState ->
		    case Obj#featureState.keyId of
			KeyId ->
			    %% Found the correcsponding featureState MO
			    {ME, SF, Lm, FS} = Id,
			    make_dn(["ManagedElement", ME,
				     "SystemFunctions", SF,
				     "Lm", Lm,
				     "FeatureState", FS]);
			_Else ->
			    find_dn(RestIds, KeyId, AccDn, Mo)
		    end
	    end
    end.
    
make_dn([Key, Value]) ->
    Key++"="++Value;
make_dn([Key, Value|Tail]) ->
    Key++"="++Value++","++make_dn(Tail).
