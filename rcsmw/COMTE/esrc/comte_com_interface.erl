%%%-------------------------------------------------------------------
%%% @private
%%% @author Lukas Larsson <lukas@erlang-solutions.com>
%%% @end
%%% Created : 21 Mar 2011 by Lukas Larsson <lukas@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(comte_com_interface).

%% Transaction exports
-export([join/1]).
-export([prepare/1]).
-export([commit/1]).
-export([abort_transaction/1]).
-export([finish/1]).
-export([validate/1]).
%% MO exports
-export([setMoAttribute/4]).
-export([setMoAttributes/3]).
-export([getMoAttribute/3]).
-export([getMoAttributes/3]).
-export([getMoIterator/3]).
-export([createMo/6]).
-export([deleteMo/2]).
-export([existsMo/2]).
-export([countMoChildren/3]).
-export([action/4]).
%% AccessManagement export
-export([getRoles/1]).
%% Availability Controller exports
-export([acInitialize/1]).
-export([healthCheckReport/2]).
-export([haModeAssumed/1]).
-export([prepareTerminationResponse/1]).
%% Replicated List exports
-export([listCreate/2]).
-export([listDelete/1]).
-export([listClear/1]).
-export([listGetSize/1]).
-export([listIsEmpty/1]).
-export([listGetFrontRef/1]).
-export([listPushBack/2]).
-export([listPopBack/1]).
-export([listEraseItem/2]).
-export([listFindItem/2]).
-export([listReplaceItem/3]).
-export([listNumberOfListInstances/0]).
-export([listMemoryUsage/0]).
%% Crypto util exports
-export([encrypt/1]).
-export([decrypt/1]).
%% logWrite
-export([logWrite/4]).
%%Object Implementer registration
-export([registerClass/2,
         registerDn/2,
         unregisterClass/2,
         unregisterDn/2
        ]).
%% CM Event
-export([registerEventConsumer/5,
         unregisterEventConsumer/3,
         unregisterEventConsumer/2
        ]).

%% PM Measurements
-export([getPmMeasurements/1,
	 getPmMeasurements/3,
         getPmMeasurementNames/1,
	 getPmJobIds/1]).

%% PM getGp
-export([getGp/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% Transaction callbacks
join(TransId) ->
    sa_apply(
      transaction_server, join, [TransId],
      fun comte_transaction:join/1).

prepare(TransId) ->
    sa_apply(
      transaction_server, prepare, [TransId],
      fun comte_transaction:prepare/1).

commit(TransId) ->
    sa_apply(
      transaction_server, commit, [TransId],
      fun comte_transaction:commit/1).

abort_transaction(TransId) ->
    sa_apply(
      transaction_server, abort_transaction, [TransId],
      fun comte_transaction:abort_transaction/1).

finish(TransId) ->
    sa_apply(
      transaction_server, finish, [TransId],
      fun comte_transaction:finish/1).

validate(TransId) ->
    sa_apply(
      transaction_server, validate, [TransId],
      fun comte_transaction:validate/1).

%% MO Callbacks
setMoAttribute(TransId, Dn, AttrName, AttrValue) ->
    sa_apply(
      transaction_server, setMoAttribute,
      [TransId, Dn, AttrName, AttrValue],
      fun comte_transaction:setMoAttribute/1).

setMoAttributes(TransId, Dn, NamedAttributes) ->
    sa_apply(
      transaction_server, setMoAttributes,
      [TransId, Dn, NamedAttributes],
      fun comte_transaction:setMoAttributes/1).

getMoAttribute(TransId, Dn, AttrName) ->
    sa_apply(
      transaction_server, getMoAttribute, [TransId, Dn, AttrName],
      fun comte_transaction:getMoAttribute/1).

getMoAttributes(TransId, Dn, AttrNames)
  when is_list(AttrNames) ->
    sa_apply(
      transaction_server, getMoAttributes, [TransId, Dn, AttrNames],
      fun comte_transaction:getMoAttributes/1).

getMoIterator(TransId, Dn, ClassName) ->
    sa_apply(
      transaction_server, getMoIterator, [TransId, Dn, ClassName],
      fun comte_transaction:getMoIterator/1).

createMo(
  TransId, ParentDn, ClassName, KeyAttrName,
  KeyValue, InitAttrs)
  when is_list(KeyValue) ->
    createMo(
      TransId, ParentDn, ClassName, KeyAttrName,
      list_to_binary(KeyValue), InitAttrs);
createMo(
  TransId, ParentDn, ClassName, KeyAttrName,
  KeyValue, InitAttrs) ->
    sa_apply(
      transaction_server, createMo,
      [TransId, ParentDn, ClassName, KeyAttrName,
       KeyValue, InitAttrs],
      fun comte_transaction:createMo/1).

deleteMo(TransId, Dn) ->
    sa_apply(
      transaction_server, deleteMo, [TransId, Dn],
      fun comte_transaction:deleteMo/1).

existsMo(TransId, Dn) ->
    sa_apply(
      transaction_server, existsMo, [TransId, Dn],
      fun comte_transaction:existsMo/1).

countMoChildren(TransId, Dn, ClassName) ->
    sa_apply(
      transaction_server, countMoChildren, [TransId, Dn, ClassName],
      fun comte_transaction:countMoChildren/1).

action(TransId, Dn, Name, NamedParams) ->
    sa_apply(
      transaction_server, action, [TransId, Dn, Name, NamedParams],
      fun comte_transaction:action/1).



%% Replicated List
listCreate(ListInstanceName, DataBufferSize ) ->
    sa_apply(
      replicated_list, listCreate, [ListInstanceName, DataBufferSize],
      fun comte_replicated_list_api:listCreate/1).

listDelete(ListInstanceName ) ->
    sa_apply(
      replicated_list, listDelete, [ListInstanceName],
      fun comte_replicated_list_api:listDelete/1).

listClear(ListInstanceName) ->
    sa_apply(
      replicated_list, listClear, [ListInstanceName],
      fun comte_replicated_list_api:listClear/1).

listGetSize(ListInstanceName) ->
    sa_apply(
      replicated_list, listGetSize, [ListInstanceName],
      fun comte_replicated_list_api:listGetSize/1).

listIsEmpty(ListInstanceName) ->
    sa_apply(
      replicated_list, listIsEmpty, [ListInstanceName],
      fun comte_replicated_list_api:listIsEmpty/1).

listGetFrontRef(ListInstanceName) ->
    sa_apply(
      replicated_list, listGetFrontRef, [ListInstanceName],
      fun comte_replicated_list_api:listGetFrontRef/1).

listPushBack(ListInstanceName,NewItemDataBuffer) ->
    sa_apply(
      replicated_list, listPushBack, [ListInstanceName, NewItemDataBuffer],
      fun comte_replicated_list_api:listPushBack/1).

listPopBack(ListInstanceName) ->
    sa_apply(
      replicated_list, listPopBack, [ListInstanceName],
      fun comte_replicated_list_api:listPopBack/1).

listEraseItem(ListInstanceName,ListItemName ) ->
    sa_apply(
      replicated_list, listEraseItem, [ListInstanceName,ListItemName],
      fun comte_replicated_list_api:listEraseItem/1).

listFindItem(ListInstanceName,ListItemName ) ->
    sa_apply(
      replicated_list, listFindItem, [ListInstanceName,ListItemName],
      fun comte_replicated_list_api:listFindItem/1).

listReplaceItem(ListInstanceName,ListItemName, ReplaceItemData) ->
    sa_apply(
      replicated_list, listReplaceItem,
      [ListInstanceName,ListItemName, ReplaceItemData],
      fun comte_replicated_list_api:listReplaceItem/1).

listNumberOfListInstances() ->
    sa_apply(
      replicated_list, listNumberOfListInstances, [],
      fun comte_replicated_list_api:listNumberOfListInstances/1).

listMemoryUsage() ->
    sa_apply(
      replicated_list, listMemoryUsage, [],
      fun comte_replicated_list_api:listMemoryUsage/1).


%% Access Management
getRoles(User) ->
    sa_apply(
      access_mgmt, getRoles, [User],
      fun comte_access_mgmt_api:getRoles/1).


%% Crypto
encrypt(String) ->
    sa_apply(
      crypto, encrypt, [String],
      fun comte_crypto_api:encrypt/1).
decrypt(EncryptedString) ->
    sa_apply(
      crypto, decrypt, [EncryptedString],
      fun comte_crypto_api:decrypt/1).

%% Logging
logWrite(EventId, Severity, Facility, Data) ->
    %% XXX Why does this function have got type checking
    %% on the arguments - no other has???
    (comte_types:uint32(EventId) andalso
     comte_types:uint16(Severity) andalso
     comte_types:uint16(Facility) andalso
     is_binary(Data))
	orelse erlang:error(badarg, [EventId,Severity,Facility,Data]),
    sa_apply(
      log_write, logWrite, [EventId, Severity, Facility, Data],
      fun comte_log_write_api:logWrite/1).


%% Object Implementer
registerClass(OI, MocPath) ->
    sa_apply(
      oi_register, registerClass, [OI, MocPath],
      fun comte_oi_register_api:registerClass/1).
registerDn(OI, DN) ->
    sa_apply(
      oi_register, registerDn, [OI, DN],
      fun comte_oi_register_api:registerDn/1).
unregisterClass(OI, MocPath) ->
    sa_apply(
      oi_register, unregisterClass, [OI, MocPath],
      fun comte_oi_register_api:unregisterClass/1).
unregisterDn(OI, DN) ->
    sa_apply(
      oi_register, unregisterDn, [OI, DN],
      fun comte_oi_register_api:unregisterDn/1).


%% CM Consumer and Alarm consumer registrations,

registerEventConsumer(
  Producer, ConsumerId, EventType, FilterAddr, Filters)
  when Producer == cm_event_producer;
       Producer == alarm_producer; 
       Producer == pm_event_producer -> % ebabmat was here
    sa_apply(
      Producer, register_consumer,
      [ConsumerId, EventType, FilterAddr, Filters],
      fun comte_alarm_event_producer_api:register_consumer/1).

unregisterEventConsumer(Producer, ConsumerId, EventType)
  when Producer == cm_event_producer;
       Producer == alarm_producer;
       Producer == pm_event_producer ->
    sa_apply(
      Producer, unregister_consumer, [ConsumerId, EventType],
      fun comte_alarm_event_producer_api:unregister_consumer/1).

unregisterEventConsumer(Producer, all)
  when Producer == cm_event_producer;
       Producer == alarm_producer;
       Producer == pm_event_producer ->
    sa_apply(
      Producer, unregister_consumers, [],
      fun comte_alarm_event_producer_api:unregister_consumers/1).


%% PM Measurements
%% Only applicable in COM 4.0 and above

getPmMeasurements(DN) ->
    sa_apply(
      pm, get_measurements, [DN, []],
      fun comte_pm_api:get_measurements/1).
%% Only applicable in COM 5.0 and above
getPmMeasurements(DN, Names, Opts) ->
    sa_apply(
      pm, get_measurements, [DN, Names, Opts],
      fun comte_pm_api:get_measurements/1).

getPmMeasurementNames(DN) ->
    sa_apply(
      pm, get_measurement_names, [DN],
      fun comte_pm_api:get_measurement_names/1).

getPmJobIds(DN) ->
    sa_apply(
      pm, get_job_ids, [DN],
      fun comte_pm_api:get_job_ids/1).


%% Availability Controller
acInitialize(OsPid) ->
    comte_com:acInitialize(OsPid).

healthCheckReport(HealthReport, RecommendedResponse) ->
    comte_com:healthCheckReport(HealthReport, RecommendedResponse).

haModeAssumed(Error) ->
    comte_com:haModeAssumed(Error).

prepareTerminationResponse(Error) ->
    comte_com:prepareTerminationResponse(Error).


%% PM getGp
%% Only applicable in COM 10.0.1 and above
getGp(PmGpId) ->
    sa_apply(
      pm_gp, get_gp, [PmGpId],
      fun comte_gp_api:get_gp/1).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

sa_apply(Key, Function, Args, Validate) ->
    case comte_types:comte_cb_key(Key) of 
	true ->
	    Callback = comte_registrar:get_callback(Key),
	    Result = apply(Callback, Function, Args),
	    Validate(Result) orelse
		erlang:error({invalid, Result}, [Key, Function, Args, Validate]),
	    handle_apply(Result);
	false ->
	    handle_apply({error, failure})
    end.

handle_apply({error, Error}) ->
    Code = comte_lib:com_error_code(Error),
    case comte_types:error_reason(Error) of
	true ->
	    %% Handle old style reasons
	    {error, Code, Error};
	false ->
	    {error, Code}
    end;
handle_apply({error, Error, Reason}) ->
    {error, comte_lib:com_error_code(Error), Reason};
handle_apply(Result) ->
    Result.
