%%% #---------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logModel.erl %
%%% Author:	etxjotj
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(logModel).
-vsn('/main/R1A/R2A/R3A/R4A/R9A/R10A/1').
-date('2017-04-25').
-author('etxtory').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% -----      -------    --------    ------------------------
%%% R1A/1      2012-01-04 etxbjca     Created
%%% R1A/2      2012-02-06 etxjotj     First version
%%% R2A/7      2013-06-12 etxasta     Updated deleteMo
%%% R2A/10     2013-09-19 uabesvi     Updated setMoAttribute
%%% R2A/19     2013-12-12 uabesvi     changed LogM to RcsLogM
%%% R2A/22     2013-12-18 etxarnu     Fixed action for new comte
%%% R3A/3      2014-10-31 etxpeno     Check if it is possible to parse the URI
%%% R4A/1      2015-08-25 etxtory     Added granuarly parameter
%%% R4A/2      2015-09-10 etxtory     granuarly does not need to be set
%%% R4A/3      2015-10-28 etxtory     password is not mandatory (causes crash)
%%% R4A/4      2015-11-27 etxtory     Only allow one export per log at the same time
%%% R9A/1      2017-02-02 emarnek     Switched http_uri:parse to ftpI:parse_uri
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([getMoAttribute/2, nextMo/3]).
-export([createMo/4,setMoAttribute/4,deleteMo/2]).
-export([prepare/3,commit/3,finish/3]).
-export([action/3]).

-export([existsMo/2]).
-export([countMoChildren/3]).
-export([getMoAttributes/3]).
-export([setMoAttributes/3]).
-export([action/4]).
-export([createMo/5]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RcsLogM.hrl").

-define(UINT16,     6).
-define(STRING,     9).
-define(REFERENCE, 11).
-define(ENUM,      12).
-define(STRUCT,    14).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%=====================================================================
%% existsMo(DnRev::[binary()], TransId:integer()) -> true | false
%%=====================================================================
existsMo(DnRev, _Tx) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:existsMo(DnRev, Table).

%%=====================================================================
%% countMoChildren(DnRev::[binary()],
%%                 ClassName::binary(),
%%                 TransId:integer()) -> non_neg_integer().
%%=====================================================================
countMoChildren(DnRev, ClassName, _Tx) ->
    comsaGeneric:countMoChildren(DnRev, ClassName).

%%=====================================================================
%% getMoAttribute([Attribute|DnRev], TxHandle) ->
%%=====================================================================
getMoAttribute([<<"progressReport">>|DnRev]=_Dn, _Tx) ->
    Table = table(comsaGeneric:class(DnRev)),
    Key = comsaGeneric:dnrev_to_key(DnRev),

    case mnesia:read(Table, Key) of
	[] ->
	    undefined;
	[Obj] ->
	    ProgressReport =
		case Table of
		    logM ->
			Obj#logM.progressReport;
		    log ->
			Obj#log.progressReport
		end,
	    comsaGeneric:format_struct(ProgressReport,
				       ?'AsyncActionProgress_types')
    end;
getMoAttribute([<<"password">>|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    Key = comsaGeneric:dnrev_to_key(DnRev),
    case mnesia:read(Table, Key) of
	[] ->
	    undefined;
	[Obj] ->
	    Password = Obj#logPushTransfer.password,
	    comsaGeneric:format_struct(Password,
				       ?'EcimPassword_types')
    end;

getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).

%%=====================================================================
%% getMoAttributes(AttrNames::[binary()],
%%                 DnRev:[binary()], TransId:integer()) - > [com_value()]
%%=====================================================================
getMoAttributes(AttrNames, DnRev, Tx) ->
    case comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))) of
	true  -> [getMoAttribute([AN | DnRev], Tx) || AN <- AttrNames];
	false -> []
    end.

%%=====================================================================
%% nextMo(Dn, Key, TxHandle) ->
%%=====================================================================
nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

%%=====================================================================
%% setMoAttribute
%%=====================================================================
setMoAttribute([<<"severityFilter">> = Attribute | DnRev],
	       {_,_} = TypeAndValue,
	       _,
	       _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), [TypeAndValue]);
setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).

%%=====================================================================
%% setMoAttributes(AttrNames::[com_named_attribute()],
%%                 ReverseDn::[binary()],
%%                 TransId:integer()) -> ok |{error, Reason}
%%=====================================================================
setMoAttributes(AttrNames, DnRev, _Tx) ->
    sma(lists:keyfind(<<"severityFilter">>, 1, AttrNames), AttrNames, DnRev).

sma({_, Filter}, _, [<<"AlarmLog">> | _]) when Filter /= undefined ->
    {abort, <<"Not allowed to change filter settings on alarm log">>};
sma(_, AttrNames, DnRev) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), sma_sev_filter(AttrNames, [])).

sma_sev_filter([], Acc) ->
    Acc;
sma_sev_filter([{<<"severityFilter">>, {_,_} = TypeVal} | T], Acc) ->
    sma_sev_filter(T, [{<<"severityFilter">>, [TypeVal]} | Acc]);
sma_sev_filter([H | T], Acc) ->
    sma_sev_filter(T, [H | Acc]).

%%=====================================================================
%% createMo/4
%%=====================================================================
createMo([_Class, Log | _DnRev], _, _, _) 
  when Log /= <<"AuditTrailLog">> andalso
       Log /= <<"SecurityLog">> ->
    {abort, <<"LogPushTransfer is only allowed for "
	     "AuditTrailLogs and SecurityLogs">>};
createMo([Class|ParentDnRev], _, IxValueB, _) ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:create(Table, ParentDnRev, IxValueB).

%%=====================================================================
%% createMo([ClassName::binary()||ReverseDn::[binary()]],
%%          KeyAttrName::binary(),
%%          KeyValue::binary(),
%%          InitAttrs::[com_named_attribute()],
%%          TransId:integer()) -> {ok, NewObj}
%%=====================================================================
createMo([_Class, Log | _DnRev], _KeyAttrName, _KeyValue, _InitAttrs, _Tx) 
  when Log /= <<"AuditTrailLog">> andalso
       Log /= <<"SecurityLog">> ->
    {abort, <<"LogPushTransfer is only allowed for "
	     "AuditTrailLogs and SecurityLogs">>};
createMo([Class | DnRev], _KeyAttrName, KeyValue, InitAttrs, _Tx) ->
    Table = table(binary_to_list(Class)),
    Attrs = createMo_attrs(Table, InitAttrs, []),
    comsaGeneric:create(Table, DnRev, KeyValue, Attrs, types(Table)).

createMo_attrs(_, [], Attrs) ->
    lists:reverse(Attrs);
createMo_attrs(logPushTransfer = Table, [{KeyB, Val} | T], Attrs) ->
    Key = list_to_atom(binary_to_list(KeyB)),
    Type = proplists:get_value(Key, types(Table)), 
    A = trans_attr(Type, Val),
    createMo_attrs(Table, T, [{KeyB, A} | Attrs]).

trans_attr(string, {_, Val}) ->
    binary_to_list(Val);
trans_attr(string, Val) ->
    Val;
trans_attr(_, {_, Val}) ->
    Val.

%%=====================================================================
%% deleteMo
%%=====================================================================
deleteMo([_Id, Class | _] = DnRev, _) when Class == <<"LogPushTransfer">> ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:delete(DnRev, Table);
deleteMo(_,_) ->
    ok.

table("LogM") -> logM;
table("Log") ->  log;
table("LogPushTransfer") -> logPushTransfer.

types(logM)            -> ?logM_types;
types(log)             -> ?log_types;
types(logPushTransfer) -> ?logPushTransfer_types.

%%=====================================================================
%% prepare, commit, finish: currently not used
%%=====================================================================
prepare(_DN, User, _Tx) ->
    {ok,User}.
commit(_DN, User, _Tx) ->
    {ok,User}.
finish(_DN, _User, _Tx) ->
    ok.

%%=====================================================================
%% action(Name::binary(),
%%        ReverseDn::[binary()],
%%        NamedParams::[com_named_parameter()],
%%        TransId:integer())
%% Note:
%% <password> is not mandatory and CLI/Netconf can send an action without
%% password attribute. This is handled below for all actions.
%% <uri> is mandatory and COM will reject and action without uri.
%%=====================================================================
action(Action, DnRev, NamedParams, TransId)
  when Action == <<"export">>;
       Action == <<"exportAvailabilityLog">> ->
    Uri = proplists:get_value(<<"uri">>, NamedParams),
    case check_password(NamedParams) of
	{ok, Pwd} ->
	    action([Action | DnRev], [Uri, Pwd], TransId);
	nok ->
	    {error, <<"Missing password parameter">>}
    end;
action(Action, DnRev, NamedParams, TransId)
  when Action == <<"exportEsi">> ->
    Uri = proplists:get_value(<<"uri">>, NamedParams),
    case check_password(NamedParams) of
	{ok, Pwd} ->
	    Granularity = 
		case proplists:get_value(<<"granularity">>, NamedParams) of
		    undefined -> undefined;
		    {_, Gran} -> list_to_atom(binary_to_list(Gran))
		end,
	    action([Action | DnRev], [Uri, Pwd, Granularity], TransId);
	nok ->
	    {error, <<"Missing password parameter">>}
    end;
action(_Action, _DnRev, _NamedParams, _TransId) ->
    {error, <<"Unrecognized input">>}.

action([<<"export">>, LogIdBin, <<"Log">>|_], Parameters, _) ->
    [{_, UrlBin}, {_, PasswdBin}] = Parameters,
    Url = binary_to_list(UrlBin),
    LogName = binary_to_list(LogIdBin),
    Fun = fun() -> logServer:export_log(LogName,
					Url,
					binary_to_list(PasswdBin))
	  end,
    check_params_and_run([{url, Url}], LogName, Fun);
action([<<"exportEsi">>|_], Parameters, _Extra) ->
    [{_, UrlBin}, {_, PasswdBin}, Granularity] = Parameters,
    Url = binary_to_list(UrlBin),
    Fun = fun() -> logServer:transfer_esi(Url,
					  binary_to_list(PasswdBin),
					  Granularity)
	  end,
    check_params_and_run([{url, Url}, {granularity, Granularity}], exportEsi, Fun);
action([<<"exportAvailabilityLog">>|_], Parameters, _) ->
    [{_, UrlBin}, {_, PasswdBin}] = Parameters,
    Url = binary_to_list(UrlBin),
    Fun = fun() -> logServer:transfer_avli(Url,
					   binary_to_list(PasswdBin))
	  end,
    check_params_and_run([{url, Url}], exportAvailabilityLog, Fun);
action(_DnRev, _Parameters, _TransId) ->
    {error, <<"Unrecognized input">>}.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

check_params_and_run([{url, Url} | T], Action, Fun) ->
    case ftpI:parse_uri(Url) of
	{ok, _} ->
	    check_params_and_run(T, Action, Fun);
	_ ->
	    {error, invalid_argument, <<"Incorrect uri format">>}
    end;
check_params_and_run([{granularity, Granularity} | T], Action, Fun) ->
    case Granularity of
	Granularity when Granularity =:= small;
			 Granularity =:= large;
			 Granularity =:= static;
			 Granularity =:= undefined; %% CLI
			 Granularity =:='' ->       %% Netconf
	    check_params_and_run(T, Action, Fun);
	_ ->
	    {error, invalid_argument, <<"Incorrect granularity; use small or large ">>}
    end;
check_params_and_run([], Action, Fun) ->
    case logServer:is_action_ongoing(Action) of
	true ->
	    %% This Action is already ongoing.
	    %% Only allow one export per log at the same time from Mgmt interface.
	    {error, <<"Previous action is already ongoing. Wait until the previous action is ready.">>};
	false ->
	    {ok, Res} = Fun(),
	    {?UINT16, Res}
    end.

check_password(NamedParams) ->
    Pwd = proplists:get_value(<<"password">>, NamedParams),
    case Pwd of
	undefined ->
	    nok;
	{_, _PasswdBin} ->
	    {ok, Pwd};
	_ ->
	    nok
    end.
