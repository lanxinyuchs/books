%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lih_cb.erl %
%%% @author Per Norberg <per.norberg@ericsson.com>
%%% @copyright Ericsson AB 2012
%%% @version /main/R1A/9
%%%
%%% @doc ==License related callback functions==

-module(lih_cb).
-vsn('/main/R1A/9').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012 All rights reserved.
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
%%% R1A/4      2012-03-05 etxpeno     Copied from git
%%% R1A/5      2012-03-06 etxpeno     Dialyzer fixes
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%-behaviour(comte).

%% API
-export([init/0]).

%% Comte callbacks
-export([getMoAttribute/2, nextMo/3]).
-export([createMo/4, setMoAttribute/4, deleteMo/2]).
-export([prepare/3, commit/3,finish/3, action/3]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("Licensing.hrl").
-include("lih_lihi.hrl").
-include("comTypes.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

-spec init() -> ok.
init() ->
    ClassPath = ["ManagedElement", "SystemFunctions", "Licensing"],
    comsaLib:register_callback(ClassPath, ?MODULE).

getMoAttribute([<<"emergencyStateInfo">>|_], _) ->
    %% This dynamic attribute (emergencyStateInfo) is not stored in mnesia
    {State, Time} = lih_lici_server:get_emergency_state_info(),
    Struct = #'EmergencyInfo'{state = State, time = Time},
    comsaGeneric:format_struct(Struct, ?'EmergencyInfo_types');
getMoAttribute([<<"lastLicensingPiChange">>|_], _) ->
    %% This dynamic attribute (lastLicensingPiChange) is not stored in mnesia
    LastChange = lih_lici_server:get_last_licensing_change(),
    ?STRING(list_to_binary(LastChange));
getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).

nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

createMo([Class|ParentDnRev], _IxAttributeB, IxValueB, _TxHandle) ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:create(Table, ParentDnRev, IxValueB).

setMoAttribute([Attribute|DnRev], TypeAndValue, _, _TxHandle) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).

deleteMo([_Key,Class|_ParentDnRev] = DnRev, _TxHandle) ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:delete(DnRev,Table).

action([<<"setEmergencyState">>|_], _Parameters, _Tx) ->
    lih_lici_server:set_emergency_state(),

    undefined;
action([<<"updateLicenseKeyFile">>|_],
       [?STRING(A), ?STRING(B), ?STRING(C), ?STRING(D)],
       _Tx) ->
    UserId = binary_to_list(A),
    Password = binary_to_list(B),
    IpAddress = binary_to_list(C),
    File = binary_to_list(D),
    lih_lici_server:update_LKF({UserId, Password, IpAddress, File}),

    undefined.

prepare(_DN, User, _Tx) ->
    {ok, User}.

commit(_DN, User, _Tx) ->
    {ok, User}.

finish([_, <<"OptionalFeature">>|_],
       #optionalFeature{'FeatureId'  = ?FEATURE_ID(Rdn),
			keyId        = KeyId,
			featureState = ?FeatureActivationState_DEACTIVATED},
        _Tx) ->
    lih_lihi_server:update_feature_state({KeyId, Rdn, deactivated});
finish([_, <<"OptionalFeature">>|_],
       #optionalFeature{'FeatureId'  = ?FEATURE_ID(Rdn),
			keyId        = KeyId,
			featureState = ?FeatureActivationState_ACTIVATED},
       _Tx) ->
    lih_lihi_server:update_feature_state({KeyId, Rdn, activated});
finish(_DN, _User, _Tx) ->
    ok.

table([H|T]) ->
    list_to_atom([(H+$a-$A)|T]). %convert first letter to lower case

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

types(licensing)       -> ?licensing_types;
types(optionalFeature) -> ?optionalFeature_types;
types(capacityFeature) -> ?capacityFeature_types.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
