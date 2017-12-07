%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	aicModel.erl %
%%% @author etxtory
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R2A/R4A/6
%%%
%%% @doc ==ECIM model callback module for AI==
%%% This module implements the callback functions for the AIC model
-module(aicModel).
-id('Updated by CCase').
-vsn('/main/R2A/R4A/6').
-date('2015-12-04').
-author('etxtory').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
%%% R2A/1      14-03-04   etxtory     Created
%%% R2A/2      14-03-05   etxtory     Implementing callbacks
%%% R2A/3      14-03-31   etxtory     Return error with <<>>
%%% R2A/4      14-05-19   etxtory     Fixed rbsConfigLevel
%%% R2A/5      14-09-26   etxtory     abort when error is setMoAttr
%%% R2A/6      14-10-21   etxtory     Remove side-effect from setMoAttr
%%% ----------------------------------------------------------
%%% R4A/1      15-08-21   etxtory     RAN integration (rbsConfigLevel updates)
%%% R4A/3      15-09-06   etxtory     Exported enum_to_int
%%% R4A/4      15-09-21   etxtory     Improved error for rbsConfigLevel
%%% R4A/5      15-09-25   etxtory     Allow CONFIG_COMPLETE to CONFIG_COMPLETE
%%% R4A/6      15-12-04   etxtory     Disallow [] rbsConfigLevel
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%% Function called by aicServer
-export([int_to_enum/1]).

%% Function called by aicServer:activate
-export([init/0]).

%% COMTE API
-export([prepare/3,
	 commit/3,
	 finish/3,
	 getMoAttributes/3,
	 getMoAttribute/2,
	 nextMo/3,
	 setMoAttribute/4,
	 setMoAttributes/3,
	 existsMo/2,
	 countMoChildren/3]).  %% Needed?

%% COMTE API Not used
%% createMo/4
%% createMo/5
%% action/4
%% deleteMo/2
%% abortTransaction/1

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RmeAI.hrl").

-define(SET_TIMEOUT, 2000).
-define(TIMEOUT, 5000).

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
init() ->
    Root = ["ManagedElement", "NodeSupport", "AutoProvisioning"],
    comsaLib:register_callback(Root, aicModel).

prepare(_DN, User, _Tx) ->
    {ok, User}.

commit(_DN, User, _Tx) ->
    {ok, User}.

finish(_DN, _User, _Tx) ->
    ok.

getMoAttributes(AttrNameList, DnRev, TransId) ->
    [getMoAttribute([AttrName | DnRev], TransId) || AttrName <- AttrNameList].

getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).

nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

setMoAttributes(NamedAttributes = [{<<"rbsConfigLevel">>, {_, Value}}], DnRev, _TransId) ->
    [Obj] = mnesia:read(autoProvisioning, {"1","1","1"}),
    ConfigLevel = Obj#autoProvisioning.rbsConfigLevel,
    case check_config_level(ConfigLevel, Value) of
	ok ->
	    Table = table(comsaGeneric:class(DnRev)),
	    comsaGeneric:set(DnRev, Table, types(Table), NamedAttributes);
	nok ->
	    Error = "Not allowed to change rbsConfigLevel to value " ++ int_to_enum(Value) ++ ".",
	    mnesia:abort(Error)
    end;

setMoAttributes([{<<"rbsConfigLevel">>, undefined}], _DnRev, _TransId) ->
    Error = "Not allowed to change rbsConfigLevel to an undefined value.",
    mnesia:abort(Error);

%% Not used (only handling rbsConfigLevel)
setMoAttributes(NamedAttributes = [{_AttrName, _ComValue}], DnRev, _TransId) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), NamedAttributes).

setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).

existsMo(DnRev, _Tx) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:existsMo(DnRev, Table).

countMoChildren(ReversedDn, ClassName, _Tx) ->
    comsaGeneric:countMoChildren(ReversedDn, ClassName).

int_to_enum(?ConfigLevel_SITE_CONFIG_COMPLETE) -> "SITE_CONFIG_COMPLETE";
int_to_enum(?ConfigLevel_OSS_CONFIGURATION_SUCCESSFUL) -> "OSS_CONFIGURATION_SUCCESSFUL";
int_to_enum(?ConfigLevel_INTEGRATION_COMPLETE) -> "INTEGRATION_COMPLETE";
int_to_enum(?ConfigLevel_READY_FOR_SERVICE)-> "READY_FOR_SERVICE";
int_to_enum(?ConfigLevel_OSS_ACTIVATING_CONFIGURATION) -> "OSS_ACTIVATING_CONFIGURATION";
int_to_enum(?ConfigLevel_OSS_CONFIGURATION_FAILED) -> "OSS_CONFIGURATION_FAILED";
int_to_enum(?ConfigLevel_ACTIVATING_FEATURES) -> "ACTIVATING_FEATURES";
int_to_enum(?ConfigLevel_ACTIVATING_FEATURES_FAILED) -> "ACTIVATING_FEATURES_FAILED";
int_to_enum(?ConfigLevel_OSS_CONTROL_NODE_CONN) -> "OSS_CONTROL_NODE_CONN";
int_to_enum(?ConfigLevel_OSS_CONTROL_NODE_CONN_FAILED) -> "OSS_CONTROL_NODE_CONN_FAILED";
int_to_enum(?ConfigLevel_UNLOCKING_CELLS) -> "UNLOCKING_CELLS";
int_to_enum(?ConfigLevel_UNLOCKING_CELLS_FAILED) -> "UNLOCKING_CELLS_FAILED";
int_to_enum(?ConfigLevel_RAN_INTEGRATION_WAS_CANCELLED) -> "RAN_INTEGRATION_WAS_CANCELLED";
int_to_enum(ConfigLevel) -> ConfigLevel.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% Conversion table & types
%%% ----------------------------------------------------------
table("AutoProvisioning") -> autoProvisioning.

types(autoProvisioning)   -> ?autoProvisioning_types.

%%% ----------------------------------------------------------
%%% check_config_level(OldConfigLevel, NewConfigLevel)
%%%
%%% OSS is setting all config-levels except SITE_CONFIG_COMPLETE.
%%% RBS/AIC is setting this when configuration files has been
%%% successfully loaded and RBS/AIC will also create a local
%%% backup.
%%% SITE_CONFIG_COMPLETE is not allowed to set by OSS.
%%%
%%% RAN_INTEGRATION_WAS_CANCELLED - the local backup created
%%% in SITE_CONFIG_COMPLETE is restored.
%%%
%%% Below is the state-machine for the config-levels.
%%% Allow all state changes but inform about incorrect behaviour.
%%% ----------------------------------------------------------
check_config_level(undefined, NewConfigLevel) ->
    %% Should not happen; OSS to not allowed to set initial config-level
    error_msg("OSS tries to set RbsConfigLevel ~p before RBS is ready~n", [int_to_enum(NewConfigLevel)]),
    nok;

check_config_level(_, ?ConfigLevel_RAN_INTEGRATION_WAS_CANCELLED) ->
    %% Allowed from any state (except undefined)
    ok;

check_config_level(?ConfigLevel_SITE_CONFIG_COMPLETE, ?ConfigLevel_SITE_CONFIG_COMPLETE) ->
    %% OSS is not allowed to set SITE_CONFIG_COMPLETE, only RBS/AIC.
    %% But allow this if SITE_CONFIG_COMPLETE is already set.
    ok;

check_config_level(_, ?ConfigLevel_SITE_CONFIG_COMPLETE) ->
    %% Not allowed to change to this value.
    nok;

check_config_level(ConfigLevel, ConfigLevel) ->
    %% OSS sets same config-level.
    warning_msg("OSS sets same config-level as current ~p~n", [int_to_enum(ConfigLevel)]),
    ok;

check_config_level(_, ?ConfigLevel_OSS_CONFIGURATION_SUCCESSFUL) ->
    %% Deprecated; should not be used.
    warning_msg("Deprecated ENUM OSS_CONFIGURATION_SUCCESSFUL used for RbsConfigLevel~n", []),
    ok;

check_config_level(?ConfigLevel_SITE_CONFIG_COMPLETE, NewConfigLevel) 
  when NewConfigLevel =:= ?ConfigLevel_OSS_ACTIVATING_CONFIGURATION ->
    ok;

check_config_level(?ConfigLevel_OSS_ACTIVATING_CONFIGURATION, NewConfigLevel) 
  when NewConfigLevel =:= ?ConfigLevel_OSS_CONFIGURATION_FAILED;
       NewConfigLevel =:= ?ConfigLevel_ACTIVATING_FEATURES ->
    ok;

check_config_level(?ConfigLevel_ACTIVATING_FEATURES, NewConfigLevel) 
  when NewConfigLevel =:= ?ConfigLevel_ACTIVATING_FEATURES_FAILED;
       NewConfigLevel =:= ?ConfigLevel_OSS_CONTROL_NODE_CONN ->
    ok;

check_config_level(?ConfigLevel_OSS_CONTROL_NODE_CONN, NewConfigLevel) 
  when NewConfigLevel =:= ?ConfigLevel_OSS_CONTROL_NODE_CONN_FAILED;
       NewConfigLevel =:= ?ConfigLevel_UNLOCKING_CELLS ->
    ok;

check_config_level(?ConfigLevel_UNLOCKING_CELLS, NewConfigLevel) 
  when NewConfigLevel =:= ?ConfigLevel_UNLOCKING_CELLS_FAILED;
       NewConfigLevel =:= ?ConfigLevel_INTEGRATION_COMPLETE ->
    ok;

check_config_level(?ConfigLevel_INTEGRATION_COMPLETE, NewConfigLevel)
  when NewConfigLevel =:= ?ConfigLevel_READY_FOR_SERVICE ->
    ok;

check_config_level(OldConfigLevel, NewConfigLevel) ->
    info_msg("OSS does not follow RbsConfigLevel state machine~n"
	     "Old RbsConfigLevel ~p; New RbsConfigLevel ~p~n",
	     [int_to_enum(OldConfigLevel), int_to_enum(NewConfigLevel)]).

%%% ----------------------------------------------------------
%%% Info-msg, error-msg and warning-msg
%%% ----------------------------------------------------------
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

