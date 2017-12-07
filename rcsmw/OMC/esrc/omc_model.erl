%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_model.erl %
%%% @author etxasta
%%% @copyright Ericsson AB 2015
%%% @version /main/R3A/R4A/1
%%%
%%% @doc ==Agent implementation for LDAP==
%%% This is the agent implementation the Ldap authentication model

-module(omc_model).
-vsn('/main/R3A/R4A/1').
-date('2015-09-11').
-author('etxasta').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015 All rights reserved.
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
%%% Rev     Date       Name        What
%%% -----   -------    --------    ------------------------
%%% R3A/1   2015-01-19 etxjotj     Created
%%% R3A/5   2015-03-17 etxtory     getMoAttributes fix
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([getMoAttribute/2, nextMo/3]).
-export([createMo/4,setMoAttribute/4,deleteMo/2]).
-export([prepare/3,commit/3,finish/3]).
-export([validate/3]).

-export([existsMo/2,
         countMoChildren/3,
         getMoAttributes/3,
         setMoAttributes/3,
         createMo/5,
	 prepareTransaction/2]).

-export([get_ldap_config/0]).
-export([get_ldap_flexible_filter/0, get_ldap_ericsson_filter/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RcsLdapAuthentication.hrl").
-include("RcsHttpM.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%% @doc Returns true if the specified instance exists.
existsMo(DnRev, _) ->
    comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))).

%%% @doc Returns the number of MO instances of given class
%%% directly below the specified parent.
countMoChildren(DnRev, Class, _) ->
    comsaGeneric:countMoChildren(DnRev, table(binary_to_list(Class))).

%%% @doc Gets MO attribute values.
%%% The instance is specified by ReversedDn and the attributes to be
%%% fetched are given by the AttrNames argument.
getMoAttributes(AttrNames, DnRev, TxHandle) ->
    case comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))) of
	true  ->
	    [getMoAttribute([AN | DnRev], TxHandle) || AN <- AttrNames];
	false ->
	    []
    end.

getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).

nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

setMoAttributes(Attrs, DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), Attrs).

setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).

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

validate(_DN, User, _Tx) ->
    {ok,User}.

prepare(_DN, User, _Tx) ->
    {ok,User}.

commit(_DN, User, _Tx) ->
    {ok,User}.

finish(_DN, _User, _Tx) ->
    ok.

prepareTransaction([], _Tx)->
    ok;
prepareTransaction([{_Dn, Obj} | T], Tx) when is_record(Obj, https) ->
    case check_mo_ref(Obj#https.nodeCredential, nodeCredential) of
	ok ->
	    case check_mo_ref(Obj#https.trustCategory, trustCategory) of
		ok ->
		    prepareTransaction(T, Tx);
		{error, Error} ->
		    {abort, Error}
	    end;
	{error, Error} ->
	    {abort, Error}
    end;
prepareTransaction([_ | T], Tx) ->
    prepareTransaction(T, Tx).

%%% Check that https NC/TC MO-ref exist in Table (nodeCredential/trustCategory)
check_mo_ref(undefined, _Table) ->
    ok;
check_mo_ref(DN, Table) ->
    case mnesia:read(Table, comsaGeneric:mo_to_key(DN)) of
	[] ->
	    {error, <<"The MO you try to set in ",
		      (list_to_binary(atom_to_list(Table)))/binary,
		      " does not exist.">>};
	_ ->
	    ok
    end.

get_ldap_config() ->
    Fun = fun() -> mnesia:read({ldap, {"1","1","1","1","1","1"}}) end,
    case mnesia:transaction(Fun) of
	{atomic, [Obj]} ->
	    Fields = record_info(fields, ldap),
	    [enum_value_to_atom({Key,Value}) ||
		{Key, Value} <- lists:zip(Fields, tl(tuple_to_list(Obj))),
		Value /= undefined];
	{atomic, []} ->
	    [];
	{aborted, Reason} ->
	    sysInitI:error_report(
	      [{mfa, {mnesia, read, {ldap, {"1","1","1","1","1","1"}}}},
	       {aborted, Reason}]),
	    []
    end.

get_ldap_flexible_filter() ->
    case mnesia:transaction(fun() ->
		mnesia:read({filter,
			{"1","1","1","1","1","1","1"}}) end) of
	{atomic, [Obj]} ->
	    Fields = record_info(fields, filter),
	    [{Key,Value}||
		{Key, Value} <- lists:zip(Fields, tl(tuple_to_list(Obj))),
		Value /= undefined];
	{atomic, []} ->
	    [];
	{aborted, Reason} ->
	    sysInitI:error_report(
	      [{?MODULE, get_ldap_flexible_filter, []},
	       {aborted, Reason}]),
	    []
    end.

get_ldap_ericsson_filter() ->
    case mnesia:transaction(fun() ->
		mnesia:read({ericssonFilter,
			{"1","1","1","1","1","1","1"}}) end) of
	{atomic, [Obj]} ->
	    Fields = record_info(fields, ericssonFilter),
	    [enum_value_to_atom({Key,Value})||
		{Key, Value} <- lists:zip(Fields, tl(tuple_to_list(Obj))),
		Value /= undefined];
	{atomic, []} ->
	    [];
	{aborted, Reason} ->
	    sysInitI:error_report(
	      [{?MODULE, get_ldap_ericsson_filter, []},
	       {aborted, Reason}]),
	    []
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% Help function
%%% ----------------------------------------------------------

table("HttpM") -> httpM;
table("Https") -> https;

table("Filter") -> filter;
table("Ldap") -> ldap;
table("LdapAuthenticationMethod") -> ldapAuthenticationMethod;
table("EricssonFilter") -> ericssonFilter.

types(httpM) -> ?httpM_types;
types(https) -> ?https_types;

types(filter) -> ?filter_types;
types(ldap) -> ?ldap_types;
types(ldapAuthenticationMethod) -> ?ldapAuthenticationMethod_types;
types(ericssonFilter) -> ?ericssonFilter_types.

values([{Name, {_, Value}} | Tail]) ->
    [{Name, Value} | values(Tail)];
values([Attr | Tail]) ->
    [Attr | values(Tail)];
values([]) ->
    [].

enum_value_to_atom({profileFilter, ?ProfileFilter_POSIX_GROUPS}) ->
    {profileFilter, posix};
enum_value_to_atom({profileFilter, ?ProfileFilter_ERICSSON_FILTER}) ->
    {profileFilter, ericsson};
enum_value_to_atom({profileFilter, ?ProfileFilter_FLEXIBLE}) ->
    {profileFilter, flexible};
enum_value_to_atom({tlsMode, ?TlsMode_STARTTLS}) ->
    {tlsMode, start_tls};
enum_value_to_atom({tlsMode, ?TlsMode_LDAPS}) ->
    {tlsMode, ldaps};
enum_value_to_atom({targetBasedAccessControl, ?BasicAdmState_LOCKED}) ->
    {targetBasedAccessControl, false};
enum_value_to_atom({targetBasedAccessControl, ?BasicAdmState_UNLOCKED}) ->
    {targetBasedAccessControl, true};
enum_value_to_atom(Any) -> Any.
