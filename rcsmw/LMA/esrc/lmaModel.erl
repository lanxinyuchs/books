%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmaModel.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/2
%%%
%%% @doc ==ECIM model callback module for LM==
%%% This module implements the callback functions for the LM model
-module(lmaModel).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/2').
-date('2017-03-06').
-author('etxjotj').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% -----      ---------  --------    ------------------------
%%% R3A/1      2014-11-28   etxberb   Added values/1.
%%% R3A/3      2015-03-17   etxpeno   Corrections in getMoAttribute/2 and
%%%                                   getMoAttributes/3
%%% R3A/6      2015-04-08   etxpejn   Removed unused code
%%% R4A/1      2015-05-29   etxpejn   Corrected HT77481, setMoAttribute(s) fingerprint
%%% R4A/2      2015-06-11   etxpejn   During AI - hold set_fingerprint rsp until install LKF is ready
%%% R4A/4      2015-06-30   etxpejn   During AI - wait a while id reportProgress is undefined. 
%%% R5A/1      2015-10-14   etxpejn   Make sure that the URI is of SFTP format. 
%%% R5A/2      2015-11-19   etxpejn   Not possible to change featureState for CapacityState MOs
%%% R6A/1      2016-08-10   etxpejn   Removed the restart of GLMS at abortTransaction
%%% R7A/1      2016-10-31   etxpejn   Added initial timer of 200 ms for AI
%%% R8A/1      2016-01-25   eivmiha   Switched ssh_sftp and sysSftp to ftpI
%%% R9A/1      2017-02-06   ekurnik   ftpI:parse_uri() added for sftp and ftpes
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([init/0]).
-export([getMoAttribute/2, nextMo/3]).
-export([setMoAttribute/4]).
-export([deleteMo/2]).
-export([prepare/3,commit/3,finish/3]).

%% These functions are needed for new COMTE API.
-export([existsMo/2]).
-export([countMoChildren/3]).
-export([getMoAttributes/3]).
-export([setMoAttributes/3]).
-export([action/4]).
-export([createMo/5]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RcsLM.hrl").
-include("lma.hrl").

-define(SET_TIMEOUT, 2000).
-define(TIMEOUT, 5000).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% @private
init() ->
    Root = ["ManagedElement", "SystemFunctions", "Lm"],
    comsaLib:register_callback(Root,lmaModel).

%%% @doc Returns true if the specified instance exists.
-spec existsMo([binary()], integer()) -> boolean().
existsMo(DnRev, _TransId) ->
    comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))).


%%% @doc Returns the number of MO instances of given class
%%% directly below the specified parent.
%%% -spec countMoChildren([binary()], binary(), integer()) -> non_neg_integer().
countMoChildren([DnRev], ClassName, _TransId) ->
    comsaGeneric:countMoChildren(DnRev, table(binary_to_list(ClassName))).


%%% @doc Gets MO attribute values.
%%% The instance is specified by ReversedDn and the attributes to be
%%% fetched are given by the AttrNames argument.
%%% -spec getMoAttributes([binary()], [binary()], integer()) -> [com_value()].
getMoAttributes(AttrNameList, DnRev, TransId) ->
    case existsMo(DnRev, TransId) of
	false ->
	    [];
	true ->
	    [getMoAttribute([AttrName|DnRev], TransId)||AttrName<-AttrNameList]
    end.

getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).


%%% @doc Gets the next MO
-spec nextMo([binary()], undefined|tuple(), integer()) -> {ok, undefined|tuple()}.
nextMo(Dn, Key, _TxHandle) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

prepare(_DN, User, _Tx) ->
    {ok,User}.
commit(_DN, User, _Tx) ->
    {ok,User}.
finish(_DN, _User, Tx) ->
    case mnesia:dirty_read(lmaModel, 1) of
	[Obj] ->
	    case Obj#lmaModel.transId of 
		Tx ->
		    %% Fingerprint is set during AI, make sure that the install LKF is
		    %% finished before returning OK to avoid race condition with 
		    %% seting featureState when MOs are not created yet.
		    %% Added initial sleep due to problems seen in HV36751
		    timer:sleep(200),
		    [KeyFileObj] = mnesia:dirty_read(keyFileManagement, {"1","1","1","1"}),
		    ReportProgress = KeyFileObj#keyFileManagement.reportProgress,
		    wait_for_install_complete(ReportProgress, 2),
		    mnesia:dirty_delete(lmaModel, 1),
		    ok;
		SavedTx ->
		    logI:write_log("LicensingLog", "lmaModel", warning, "Transaction Id in db: " ++
				       integer_to_list(SavedTx) ++ " does not match transaction id in "
				   " lmaModel:finished: " ++ integer_to_list(Tx)),
		    ok
	    end;
	[] ->
	    ok
    end.

wait_for_install_complete(undefined, 0) ->
    logI:write_log("LicensingLog", "lmaModel", warning, "AI ongoing - reportProgress undefined"),
    ok;
wait_for_install_complete(undefined, No) ->
    logI:write_log("LicensingLog", "lmaModel", warning, "AI ongoing - reportProgress undefined,"
		  " try to wait 200ms"),
    timer:sleep(200),
    [KeyFileObj] = mnesia:dirty_read(keyFileManagement, {"1","1","1","1"}),
    wait_for_install_complete(KeyFileObj#keyFileManagement.reportProgress, No-1),
    ok;
wait_for_install_complete(ReportProgress, No) ->
    case ReportProgress#'AsyncActionProgress'.state of
	?ActionStateType_FINISHED ->
	    logI:write_log("LicensingLog", "lmaModel", info, "AI ongoing - LKF installation ready!"),
	    ok;
	_Else ->
	    logI:write_log("LicensingLog", "lmaModel", info, "AI ongoing - wait for the LKF to be "
			   "installed...."),
	    timer:sleep(200),
	    [KeyFileObj] = mnesia:dirty_read(keyFileManagement, {"1","1","1","1"}),
	    NewReportProgress = KeyFileObj#keyFileManagement.reportProgress,
	    wait_for_install_complete(NewReportProgress, No)
    end.
	    
	    

  
%%% setMoAttributes(AttrNames::[com_named_attribute()], ReverseDn::[binary()],
%%%                 TransId:integer()) -> ok |{error, Reason}
%%% Set a list of attribute values (instead of only one).
setMoAttributes(NamedAttributes = [{AttrName, ComValue}], DnRev, TransId) ->
    case AttrName of
	<<"fingerprint">> ->
	    {9, FingerPrint} = ComValue,
	    case lmaGlms:set_fingerprint(FingerPrint) of
		{ok, fingerprint_updated} ->
		    logI:write_log("LicensingLog", "lmaModel", info, "setMoAttribute fingerprint "
				   "to: "++ binary_to_list(FingerPrint)),
		    ok = store_transId_if_ai(TransId),
		    Table = table(comsaGeneric:class(DnRev)),
		    comsaGeneric:set(DnRev, Table, types(Table), NamedAttributes),
		    ok;
		{error, Reason} ->
		    logI:write_log("LicensingLog", "lmaModel", warning,
				   "Set fingerprint failed, fingerprintUpdateable is false"),
		    {abort, Reason}
	    end;
	<<"featureState">> ->
	    {12, FeatureState} = ComValue,
	    case lists:nth(2, DnRev) of 
		<<"FeatureState">> ->
		    logI:write_log("LicensingLog", "lmaModel", info, "setMoAttribute featureState"
				   " called for " ++ lmaGlms:to_string(hd(DnRev))++" change "
				   "featureState to: " ++ integer_to_list(FeatureState)),
		    {ok, feature_state_change} = lmaGlms:set_featurestate(FeatureState, hd(DnRev)),
		    Table = table(comsaGeneric:class(DnRev)),
		    comsaGeneric:set(DnRev, Table, types(Table), NamedAttributes),
		    ok;
		<<"CapacityState">> ->
		    case FeatureState of
			?LmFeatureState_DEACTIVATED ->
			    logI:write_log("LicensingLog", "lmaModel", error, "FeatureState cannot"
					   " be changed to DEACTIVATED for CapacityState: " ++ 
					       lmaGlms:to_string(hd(DnRev))),
			    {abort, <<"FeatureState cannot be changed to DEACTIVATED">>};
			?LmFeatureState_ACTIVATED ->
			    ok
		    end
	    end
    end.

setMoAttribute([Attr= <<"fingerprint">>|DnRev], TypeAndValue, _, TransId) ->
    {9, FingerPrint} = TypeAndValue,
    case lmaGlms:set_fingerprint(FingerPrint) of
	{ok, fingerprint_updated} ->
	    logI:write_log("LicensingLog", "lmaModel", info, "setMoAttribute fingerprint "
			   "to: "++ binary_to_list(FingerPrint)),
	    ok = store_transId_if_ai(TransId),
	    Table = table(comsaGeneric:class(DnRev)),
	    comsaGeneric:set(DnRev, Attr, Table, types(Table), TypeAndValue);
	{error, Reason} ->
	    logI:write_log("LicensingLog", "lmaModel", warning,
			   "Set fingerprint failed, fingerprintUpdateable is false"),
	    {abort, Reason}
    end;
setMoAttribute([Attr= <<"featureState">>|DnRev], TypeAndValue, _, _) ->
    {12, FeatureState} = TypeAndValue,
    case lists:nth(2, DnRev) of 
	<<"FeatureState">> ->
	    logI:write_log("LicensingLog", "lmaModel", info, "setMoAttribute featureState called "
			   "for " ++ lmaGlms:to_string(hd(DnRev))++" change featureState to: "
			   ++ integer_to_list(FeatureState)),
	    {ok, feature_state_change} = lmaGlms:set_featurestate(FeatureState, hd(DnRev)),
	    Table = table(comsaGeneric:class(DnRev)),
	    comsaGeneric:set(DnRev, Attr, Table, types(Table), TypeAndValue);
	<<"CapacityState">> ->
	    case FeatureState of
		?LmFeatureState_DEACTIVATED ->
		    logI:write_log("LicensingLog", "lmaModel", error, "FeatureState cannot"
				   " be changed to DEACTIVATED for CapacityState: " ++ 
				       lmaGlms:to_string(hd(DnRev))),
		    {abort, <<"FeatureState cannot be changed to DEACTIVATED">>};
		?LmFeatureState_ACTIVATED ->
		    ok
	    end
    end.

store_transId_if_ai(TransId) ->
    case aicI:get_lkf_fn() of
	{ok, _Uri} ->
	    %% To avoid that set featureState failes during AI - 
	    %% Store the transaction ID and check during finished if the
	    %% installtion of the LKF is finished
	    logI:write_log("LicensingLog", "lmaModel", info, "setMoAttribute fingerprint "
			   "called during AI with LKF to install, save transaction id: "++ 
			       integer_to_list(TransId)),
	    ok = mnesia:write(#lmaModel{index = 1,
					transId = TransId});
	_Else ->
	    ok
    end.

%%% @doc Creates a new instance. The given class name is trusted to be
%%% one of the names supported by this module.
%%% -spec createMo([binary()],
%%% 	       mo_attribute_name(),
%%% 	       binary(),
%%% 	       [com_named_attribute()],
%%% 	       integer()) ->
%%% 	  {ok, tuple()}.

createMo([ClassName | ParentDnRev],
	 _KeyAttrName,
	 KeyValue,
	 InitAttrs,
	 _TransId) ->
    Table = table(binary_to_list(ClassName)),
    comsaGeneric:create(Table,
			ParentDnRev,
			KeyValue,
			values(InitAttrs),
			types(Table)).

deleteMo(DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table).


%%% action(Name::binary(), ReverseDn::[binary()], NamedParams::[com_named_parameter()], TransId:integer())
%%% Execute an action.
%%% Because the list of parameter is named, optional parameters may be excluded in the list, and the order
%%% of parameters is no longer fixed.
action(<<"activate">>, [<<"1">>,<<"EmergencyUnlock">>,<<"1">>,<<"Lm">>,<<"1">>,
			<<"SystemFunctions">>,<<"1">>,<<"ManagedElement">>],
       _NamedParams, _TransId) ->
    logI:write_log("LicensingLog", "lmaModel", info, "Request GLMS to activate Emergency Unlock"),
    try lmaGlms:activate_eu() of
 	{ok, eu_activated} ->
	    {10, true};
	{error, eu_not_activated} ->
	    {10, false}
    catch _:Reason ->
	    logI:write_log("LicensingLog", "lmaModel", warning,
			   "Activate EU failed"),
	    lmaLib:error_msg("Activate EU failed: ~p~n", [Reason]),
	    {error, <<"Timeout for emergency unlock activate">>}
    end;
action(<<"activate">>, [<<"1">>,<<"IntegrationUnlock">>,<<"1">>,<<"Lm">>,<<"1">>,
			<<"SystemFunctions">>,<<"1">>,<<"ManagedElement">>],
       _NamedParams, _TransId) ->
    logI:write_log("LicensingLog", "lmaModel", info, "Request GLMS to activate Integration Unlock"),
    try lmaGlms:activate_iu() of
 	{ok, iu_activated} ->
	    {10, true};
	{error, iu_not_activated} ->
	    {10, false}
    catch _:Reason ->
	    logI:write_log("LicensingLog", "lmaModel", warning,
			   "Activate IU failed"),
	    lmaLib:error_msg("Activate IU failed: ~p~n", [Reason]),
	    {error, <<"Timeout for integration unlock activate">>}
    end;
action(<<"installKeyFile">>, _DnRev, NamedParams, _TransId) ->
    [{<<"uri">>,{9, UriBin}}, {<<"password">>,{9,PasswdBin}}] = NamedParams,
    case ftpI:parse_uri(binary_to_list(UriBin)) of
	{ok,{Proto,_,_,_,_,_}} when Proto =:= sftp; Proto =:= ftpes->
	    try lmaGlms:install_key_file(UriBin,PasswdBin) of
		{ok, ActionId} ->
		    logI:write_log("LicensingLog", "lmaModel", info, "installKeyFile action "
				   "with ActionId: " ++ integer_to_list(ActionId)),
		    {2, ActionId};
		{error, Reason} ->
		    {error, Reason}
	    catch _:Reason ->
		    logI:write_log("LicensingLog", "lmaModel", warning,
				   "Install LKF failed"),
		    lmaLib:error_msg("Install LKF failed: ~p~n", [Reason]),
		    {error, <<"Timeout for install key file">>}
	    end;
	_Else ->
	    %% The URI is not an SFTP nor FTPES
	    {error, <<"URI protocol not supported">>} 
    end;
action(<<"refreshLicenseInventory">>, _DnRev, _NamedParams, _TransId) ->
    logI:write_log("LicensingLog", "lmaModel", info, "refreshLicenseInventory action"),
    try lmaGlms:refresh_license_inventory() of
 	{ok, refresh} ->
	    {10, true};
	{error, no_refresh} ->
	    {10, false}
    catch _:Reason ->
	    logI:write_log("LicensingLog", "lmaModel", warning,
			   "Refresh licenseInventory failed"),
	    lmaLib:error_msg("Refresh licenseInventory failed: ~p~n", [Reason]),
	    {error, <<"Timeout for refresh license inventory">>}
    end.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
table("Lm") -> lm;
table("FeatureKey") -> featureKey;
table("CapacityKey") -> capacityKey;
table("EmergencyUnlock") -> emergencyUnlock;
table("IntegrationUnlock") -> integrationUnlock;
table("MaintenanceUnlock") -> maintenanceUnlock; % Should probably be removed
table("SystemTriggeredUnlock") -> systemTriggeredUnlock; % Should probably be removed
table("AutonomousMode") -> autonomousMode;
table("GracePeriod") -> gracePeriod;
table("KeyFileManagement") -> keyFileManagement;
table("KeyFileInformation") -> keyFileInformation;
table("WarningThreshold") -> warningThreshold;
table("FeatureState") -> featureState;
table("CapacityState") -> capacityState.

types(lm) -> ?lm_types;
types(featureKey) -> ?featureKey_types;
types(capacityKey) -> ?capacityKey_types;
types(emergencyUnlock) -> ?emergencyUnlock_types;
types(integrationUnlock) -> ?integrationUnlock_types;
%% types(maintenanceUnlock) -> ?maintenanceUnlock_types; % Should probably be removed
%% types(systemTriggeredUnlock) -> ?systemTriggeredUnlock_types; % Should probably be removed
types(autonomousMode) -> ?autonomousMode_types;
types(gracePeriod) -> ?gracePeriod_types;
types(keyFileManagement) -> ?keyFileManagement_types;
types(keyFileInformation) -> ?keyFileInformation_types;
%% types(warningThreshold) -> ?warningThreshold_types;
types(featureState) -> ?featureState_types;
types(capacityState) -> ?capacityState_types.

values([{Name, {_, Value}} | Tail]) ->
    [{Name, Value} | values(Tail)];
values([Attr | Tail]) ->
    [Attr | values(Tail)];
values([]) ->
    [].

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
