%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	timModel.erl %
%%% @author etomist
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R4A/R10A/2
%%%
%%% @doc ==Time settings model==
%%% This module holds the agent implementation of the RmeTimeSettings model.
%%% @end
-module(timModel).
-vsn('/main/R4A/R10A/2').
-date('2017-07-19').
-author('etomist').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% R4A/1      2015-06-22 etxbjca     Created
%%% R4A/2      2015-07-07 etxjotj     First version
%%% R4A/5      2015-10-08 erarafo     Trace output (disabled)
%%% R4A/6      2015-11-03 erarafo     Line length not exceeding 80
%%% R10A/1     2017-07-19 etomist     HW12725
%%% R10A/2     2017-07-19 etomist     HW12725 - fix after tests
%%% ----------------------------------------------------------

%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([getMoAttribute/2, nextMo/3]).
-export([createMo/4,setMoAttribute/4,deleteMo/2]).
-export([prepare/3,commit/3,finish/3]).

-export([existsMo/2,
	 countMoChildren/3,
	 getMoAttributes/3,
	 setMoAttributes/3,
	 action/4,
	 createMo/5]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%-include("comte_types.hrl").
-include("tim.hrl").
%-include_lib("xmerl/include/xmerl.hrl").


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Returns true if the specified instance exists.
%%% @end
%%% ----------------------------------------------------------

existsMo(DnRev, _) ->
    comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))).


%%% ----------------------------------------------------------
%%% @doc Returns the number of MO instances of given class
%%% directly below the specified parent.
%%% @end
%%% ----------------------------------------------------------

countMoChildren(DnRev, Class, _) ->
    comsaGeneric:countMoChildren(DnRev, table(binary_to_list(Class))).


%%% ----------------------------------------------------------
%%% @doc Gets MO attribute values. 
%%% The instance is specified by ReversedDn and the attributes to be
%%% fetched are given by the AttrNames argument.
%%% ----------------------------------------------------------

getMoAttributes(AttrNames, DnRev, TxHandle) ->
    [getMoAttribute([AttrName|DnRev], TxHandle)||AttrName<-AttrNames].

getMoAttribute([Attribute|DnRev], _TxHandle) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).

nextMo(Dn, Key, _TxHandle) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

setMoAttributes([{AttrName, TypeAndValue}|Rest], DnRev, TransId) ->
    R = setMoAttribute([AttrName|DnRev], TypeAndValue, undefined, TransId),
    case Rest of
    [] -> R;
    _  -> setMoAttributes(Rest, DnRev, TransId)
    end.

setMoAttribute([<<"daylightSavingTimeStartDate">>=Attr|DnRev],
               TypeAndValue,_, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    case validateDayRule(TypeAndValue) of
        nok ->
            mnesia:abort("Invalid dayRule.");
        _ ->
            comsaGeneric:set(DnRev, Attr, Table, types(Table), TypeAndValue)
    end;
setMoAttribute([<<"daylightSavingTimeEndDate">>=Attr|DnRev],
               TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    case validateDayRule(TypeAndValue) of
        nok ->
            mnesia:abort("Invalid dayRule.");
        _ ->
            comsaGeneric:set(DnRev, Attr, Table, types(Table), TypeAndValue)
    end;
setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).


%%% ----------------------------------------------------------
%%% @doc Creates a new instance. The given class name is trusted to be
%%% one of the names supported by this module.
%%% ----------------------------------------------------------

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

createMo([Class|ParentDnRev], _, IxValueB, _) ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:create(Table, ParentDnRev, IxValueB).


deleteMo(_, _) ->
    ok.

table("TimeSettings") -> timeSettings.

types(timeSettings) -> ?timeSettings_types.

values([{Name, {_, Value}} | Tail]) ->
    [{Name, Value} | values(Tail)];
values([Attr | Tail]) ->
    [Attr | values(Tail)];
values([]) ->
    [].

prepare(_DN, User, _Tx) ->
    {ok,User}.
commit(_DN, User, _Tx) ->
    {ok,User}.
finish(_DN, _User, _Tx) ->
    ok.

action(_Name, _DnRev, _Params, _TransId) ->
    {error, <<"Not supported">>}.

validateDayRule({_DataType, Data}) ->
    case lists:keyfind(<<"dayRule">>, 1, Data) of
        false ->
            nok;
        {_Key, {_DataType2, Rule}} ->
            case re:run(Rule, ?RE_DAY_RULE_1) of
                {match, _} ->
                    ok;
                nomatch ->
                    case re:run(Rule, ?RE_DAY_RULE_2) of
                        {match, _} ->
                            ok;
                        nomatch ->
                            case re:run(Rule, ?RE_DAY_RULE_3) of
                                {match, _} ->
                                    ok;
                                nomatch ->
                                    nok
                            end
                    end
            end
    end;
validateDayRule(undefined) ->
    ok;
validateDayRule(_) ->
    nok.
