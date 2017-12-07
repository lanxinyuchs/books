%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaGeneric.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R10A/1

%%% @doc ==Library for handling COM/COMTE calls ==
%%% This is a library for handling COM/COMTE calls. The aim is to facilitate
%%% implementation of callback modules for comte by providing generic
%%% functions for the usage of mnesia as datastore for model data
%%% @end

-module(comsaGeneric).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R10A/1').
-date('2017-04-21').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% R1A/1      2012-01-10 etxbjca     Created
%%% R1A/5      2012-06-29 etxjotj     Fixed a problem with sequences of enums
%%% R1A/6      2012-06-29 etxjotj     Set of sequence fixed
%%% R1A/7      2012-07-19 qthupha     Fixing set and set_struct
%%% R1A/9      2013-08-26 uabesvi     Fixing do_fill_struct
%%% R2A/9      2013-10-16 etxjotj     Support for MafOamSpiManagedObject_3
%%% R2A/16     2013-11-04 uabesvi     do_fill_struct, added false
%%% R2A/19     2014-04-08 etxtory     Added clause in get_new_obj
%%% R2A/22     2014-07-22 etxjotj     Handle undefined case of decrypt_password
%%% R2A/23     2014-08-19 etxjotj     Return error for decrypt password
%%% R2A/24     2014-08-19 etxtjoj     Previous change was fix of HS88305
%%% R2A/25     2014-10-14 etxlg       New utility function mo_to_key/1
%%% R2A/26     2014-10-16 etxlg       fixed dialyzer-reported error
%%% R2A/27     2014-10-23 etxlg       fixed edoc build error
%%% R3A/1      2015-03-18 etxpeno     Correction of get/4
%%% R4A/1      2015-08-27 etxjotj     Undefined type
%%% R4A/2      2015-08-27 etxjotj     Undefined type again
%%% R4A/4      2015-09-08 etxjotj     HU15813 Handling of non-existent records
%%% R4A/6      2015-10-13 etxjotj     OTP18 erlang now replaced
%%% R4A/7      2015-10-14 etxpeno     change the seed to random:seed/3
%%% R5A/1      2015-10-21 etxpeno     replace random with rand
%%% R10A/1     2017-04-21 etxpeno     Improvement of nextMo/3
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([get/4, set/5, nextMo/3, create/3, create/4, create/5, delete/2]).
-export([dnrev_to_key/1]).
-export([mo_to_key/1]).
-export([class/1]).
-export([format_struct/2]).
-export([set_struct/7, make_struct/3]).

-export([existsMo/2, countMoChildren/2, set/4]).

-export([encrypt_password/1, decrypt_password/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%-export([internal_function1/2]).
%%%-export([internal_function2/3]).

-include("comTypes.hrl").
%-include("ECIM_CommonLibrary.hrl").

-record('EcimPassword', {cleartext, password}).

%-compile(export_all).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% Common type declarations

-type fieldtype()::atom()|tuple().
-type com_types()::1|2|3|4|5|6|7|8|9|10|11|12|14.
-type com_value() :: {Type::com_types(), Value::binary()|integer()}|undefined.
-type fields()::[{FieldName::atom(), FieldType::fieldtype()}].

%%% ---------------------------------------------------------
%%% @doc Converts a reversed distinguished name to a mnesia key
%%% @end

-type dn()::[binary()].
-spec dnrev_to_key(DnRev::dn()) -> tuple().

dnrev_to_key(DnRev) ->
    dnrev_to_key(DnRev, []).

dnrev_to_key([Value, _|Tail], Acc) ->
    dnrev_to_key(Tail, [Value|Acc]);
dnrev_to_key([], Acc) ->
    list_to_tuple([binary_to_list(X)||X<-Acc]).

%%% ---------------------------------------------------------
%%% @doc Converts a MO distinguished name to a mnesia key
%%% @end

%%% example:
%%% <<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=2">> ->
%%%	{"1","1","1","1","2"}

-spec mo_to_key(Mo::binary()) -> tuple().

mo_to_key(Mo) when is_binary(Mo)->
    Split = binary:split(Mo, [<<"=">>, <<",">>], [global, trim]),
    list_to_tuple(mo_t_k(Split)).

mo_t_k([]) -> [];
mo_t_k([_, V | Rest]) -> [binary_to_list(V) | mo_t_k(Rest)].
%%% ---------------------------------------------------------
%%% @doc Extract the MO class of the referred MO in a reversed distinguished name
%%% @end

-spec class(DnRev::dn()) -> string().

class([_, Class|_]) ->
    binary_to_list(Class).

%%% ---------------------------------------------------------
%%% @doc Gets the requested attribute from the mnesia table associated with that class
%%% Structs cannot be fetched this way. Get calls on structs must be captured
%%% and handled separately.
%%% @end


get(DnRev, AttributeB, Table, Fields) when is_binary(AttributeB) ->
    Key = dnrev_to_key(DnRev),
    case mnesia:read(Table, Key) of
	[Obj] ->
	    get_value(Obj, DnRev, AttributeB, Fields);
	[] ->
	    Obj = make_undefined(Fields),
	    get_value(Obj, DnRev, AttributeB, Fields)
    end;
get(DnRev, Attributes, Table, Fields) when is_list(Attributes) ->
    Key = dnrev_to_key(DnRev),
    case mnesia:read(Table, Key) of
	[Obj] ->
	    [get_value(Obj, DnRev, Attribute, Fields)||Attribute<-Attributes];
	[] ->
	    Obj = make_undefined(Fields),
	    [get_value(Obj, DnRev, Attribute, Fields)||Attribute<-Attributes]
    end.

%% If the object is not in the database, create a fake record with only
%% undefined values
make_undefined(Fields) ->
    list_to_tuple(['$head'|[undefined||_<-Fields]]).

get_value(Obj, DnRev, AttributeB, Fields) ->
    AttributeAtom = list_to_atom(binary_to_list(AttributeB)),
    InternalType = get_internal_type(AttributeAtom, Fields),
    MatchList = ['$head'|[FieldName||{FieldName,_}<-Fields]],
    AttributeList = lists:zip(MatchList, tuple_to_list(Obj)),
    KeyField = element(1, hd(Fields)),
    AttributeValue = lists:keysearch(AttributeAtom, 1, AttributeList),
    case {InternalType, AttributeValue} of
	{{struct, _}, {value, {_, undefined}}} ->
	    [?STRUCT];
	{{struct, StructName}, {value, {_, Value}}} ->
	    StructFields = comsaEcimModelAdaptor:get_struct_fields(DnRev,
								   StructName),
	    format_struct(Value, StructFields);
	{{sequence, {struct, _}},{value, {_, undefined}}} ->
	    [?STRUCT];
	{{sequence, {struct, _}},{value, {_, []}}} ->
	    [?STRUCT];
	{{sequence, {struct, StructName}},{value, {_, Values}}} ->
	    StructFields = comsaEcimModelAdaptor:get_struct_fields(DnRev,
								   StructName),
	    format_struct(Values, StructFields);
	{_, {value, {KeyField, undefined}}} ->
	    comsaEcimModelAdaptor:type(InternalType, undefined);
	{_, {value, {KeyField, TableKey}}} ->
	    Value = lists:last(tuple_to_list(TableKey)),
	    comsaEcimModelAdaptor:type(InternalType, Value);
	{_, {value, {_, Value}}} ->
	    comsaEcimModelAdaptor:type(InternalType, Value)
    end.


%%% ---------------------------------------------------------
%%% @doc Returns the index to the next MO in this class
%%% The class instance index is always the last element in the key tuple.
%%% @end

-spec nextMo(Table::atom(), Dn::dn(), Previous::undefined|tuple()) ->
		    {ok, undefined|{{9, binary()},Next::tuple()}}.

nextMo(Table, Dn, Prev) ->
    DnKeyPart = dnrev_to_key(tl(Dn)),
    nextMo1(Table, DnKeyPart, Prev).

nextMo1(Table, DnKeyPart, undefined) ->
    Result = mnesia:first(Table),
    nextMo2(Table, DnKeyPart, Result);
nextMo1(Table, DnKeyPart, Prev) ->
    Result = mnesia:next(Table, Prev),
    nextMo2(Table, DnKeyPart, Result).

nextMo2(_Table, _DnKeyPart, '$end_of_table') ->
    {ok, undefined};
nextMo2(Table, DnKeyPart, Next) ->
    case if_dn_match(DnKeyPart, Next) of
	true ->
	    NextKey = element(tuple_size(Next), Next),
	    {ok, {?STRING(list_to_binary(NextKey)), Next}};
	false ->
	    nextMo1(Table, DnKeyPart, Next)
    end.

if_dn_match(DnKeyPart, Next) ->
    NextPart = erlang:delete_element(size(Next), Next),
    DnKeyPart == NextPart.

%%% ---------------------------------------------------------
%%% @doc Set the value of a given attribute in a referred MO
%%% DnRev: the distinguished name reversed
%%% Attribute: the attribute name as a binary
%%% Table:: the mnesia table where this class is stored
%%% Fields: a list of field names and types
%%% TypeAndValue: undefined if it should be unset
%%%               A type and value if it is to be set
%%%               A list if it is a squence of objects
%%%               A 'set_struct' tuple with a record or a list of records
%%% @end
-type type_and_value() :: undefined | {com_types(), integer()|binary()} |
			  list() | {set_struct, tuple()} |
			  {set_struct, [tuple()]}.

-spec set(DnRev::dn(), Attribute::binary(), Table::atom(), Fields::fields(),
	  TypeAndValue::type_and_value()) ->
		 {ok, NewObject::tuple()}.

set(DnRev, AttributeB, Table, Fields, undefined) ->
    Key = dnrev_to_key(DnRev),
    [Obj] = mnesia:read(Table, Key),
    AttributeAtom = list_to_atom(binary_to_list(AttributeB)),
    FieldId = get_field_id(AttributeAtom, Fields),
    NewObj = setelement(FieldId, Obj, undefined),
    mnesia:write(NewObj),
    {ok, NewObj};

set(DnRev, AttributeB, Table, Fields, {Type, Value}) ->
    Key = dnrev_to_key(DnRev),
    [Obj] = mnesia:read(Table, Key),
    AttributeAtom = list_to_atom(binary_to_list(AttributeB)),
    InternalType = get_internal_type(AttributeAtom, Fields),
    {ThisType, ThisValue} =
	case InternalType of
	    _ when Type == set_struct ->
		{Type, Value};
	    {struct, StructName} ->
		StructFields =
		    comsaEcimModelAdaptor:get_struct_fields(DnRev, StructName),
		SValue = fill_struct({Type,Value}, StructName, StructFields),
		{set_struct, SValue};
	    {sequence, {struct, StructName}} ->
		StructFields =
		    comsaEcimModelAdaptor:get_struct_fields(DnRev, StructName),
		SValue = [fill_struct({Type, Value}, StructName, StructFields)],
		{set_struct, SValue};
	    {sequence, _} ->
		%% When an attribute is a sequence of strings COMTE
		%% cannot distinguish a sequence of length 1 and a
		%% scalar value so this must be translated
		{Type, [Value]};
	    _ ->
		{Type, Value}
	end,
    FieldId = get_field_id(AttributeAtom, Fields),
    NewObj = get_new_obj(InternalType, ThisType, FieldId, Obj, ThisValue),
    mnesia:write(NewObj),
    {ok, NewObj};

set(DnRev, AttributeB, Table, Fields, Sequence) ->
    Key = dnrev_to_key(DnRev),
    [Obj] = mnesia:read(Table, Key),
    AttributeAtom = list_to_atom(binary_to_list(AttributeB)),
    InternalType = get_internal_type(AttributeAtom, Fields),
    FieldId = get_field_id(AttributeAtom, Fields),
    Values =
        case InternalType of
	    {sequence, {struct, StructName}} ->
		StructFields =
		    comsaEcimModelAdaptor:get_struct_fields(DnRev, StructName),
		 [fill_struct(X, StructName, StructFields)||X<-Sequence];
            _ ->
		[case Type of
		     ?STRING -> binary_to_list(Value);
		     _ -> Value
		 end||{Type, Value}<-Sequence]
        end,
    NewObj = setelement(FieldId, Obj, Values),
    mnesia:write(NewObj),
    {ok, NewObj}.

%%% ---------------------------------------------------------
%%% @doc Set many attributes in one call
%%% @end


-type named_attributes()::[{Name::binary(), Value::com_value()}].

-spec set(DnRev::dn(), Table::atom(), Fields::fields(), NamedAttributes::named_attributes()) -> {ok, NewObject::tuple()}.

set(DnRev, Table, Fields, [{AttributeB, TypeAndValue}|Named]) ->
    {ok, NewObj} = set(DnRev, AttributeB, Table, Fields, TypeAndValue),
    case Named of
	[] ->
	    {ok, NewObj};
	_ ->
	    set(DnRev, Table, Fields, Named)
    end;
set(DnRev, Table, _, []) ->
    Key = dnrev_to_key(DnRev),
    [Obj] = mnesia:read(Table, Key),
    {ok, Obj}.

%%% ---------------------------------------------------------
%%% @doc Update an attribute which is of struct type
%%% This sort of update requires information about the struct, that is
%%% not given in the set/5 function. However, in the current release of COMSA
%%% such values are retrieved automatically by the model, but this function
%%% remains for backwards compatibility reasons
%%% @end

-spec set_struct(DnRev::dn(), AttributeB::binary(), Table::atom(),
		 Fields::fields(), TypesAndValues::[type_and_value()],
		 StructName::atom(), StructFields::fields() ) ->
		 {ok, NewObject::tuple()}.

set_struct(DnRev, AttributeB, Table, Fields, undefined, _, _) ->
    set(DnRev, AttributeB, Table, Fields, undefined);
set_struct(DnRev, AttributeB, Table, Fields, TypesAndValues, StructName,
	   StructFields) ->
    AttributeAtom = list_to_atom(binary_to_list(AttributeB)),
    InternalType = get_internal_type(AttributeAtom, Fields),
    case InternalType of
	{sequence, {struct, StructName}} ->
	    Value =
		case TypesAndValues of
		    TV when is_tuple(TV) ->
			[fill_struct(TV, StructName, StructFields)];
		    TV when is_list(TV) ->
			[fill_struct(X, StructName, StructFields)||X<-TV]
		end,
	    set(DnRev, AttributeB, Table, Fields, {set_struct, Value});
	{struct, StructName} ->
	    Value = fill_struct(TypesAndValues, StructName, StructFields),
	    set(DnRev, AttributeB, Table, Fields, {set_struct, Value});
	_ -> %% This is not a struct, pass through
	    set(DnRev, AttributeB, Table, Fields, TypesAndValues)
    end.

%%% ---------------------------------------------------------
%%% @doc Callback for the COM crypto spi
%%% @end

-spec encrypt_password(binary()) -> string().

-define(pwkey, <<243,135,249,254,22,76,28,158,2,173,30,96,2,163,208,139>>).
encrypt_password(ClearPassBin) ->
    Salt = list_to_binary([rand:uniform(256)-1||_<-lists:seq(1,16)]),
    Enc =
	crypto:block_encrypt(aes_cfb128, ?pwkey,
			     Salt,
			     ClearPassBin),

    [$1, $:|base64:encode_to_string(<<Salt/binary, Enc/binary>>)].

%%% ---------------------------------------------------------
%%% @doc Callback for the COM crypto spi
%%% @end

-spec decrypt_password(Password::tuple()|string()) -> tuple()|string().

decrypt_password(Password) when is_record(Password, 'EcimPassword') ->
    case Password#'EcimPassword'.cleartext of
	undefined ->
	    decrypt_password(Password#'EcimPassword'.password);
	_ ->
	    %% It's not encrypted
	    Password#'EcimPassword'.password
    end;

decrypt_password([$1,$:|SaltAndPassword]) ->
    Bin = base64:decode(SaltAndPassword),
    {Salt, Cipher} = erlang:split_binary(Bin, 16),
    Password = crypto:block_decrypt(aes_cfb128, ?pwkey,
				    Salt,
				    Cipher),
    binary_to_list(Password);

decrypt_password(undefined) ->
    "";

%% HS88305
decrypt_password(_) ->
    {error, "Unknown encryption format"}.



%%% ---------------------------------------------------------
%%% @doc Create an MO
%%% @end

-spec create(Table::atom(), ParentDnRev::dn(), IxValueB::binary()) ->
		    {ok, tuple()}.

create(Table, ParentDnRev, IxValueB) ->
    Attributes = mnesia:table_info(Table, attributes),
    NewObj = list_to_tuple([Table, key|[undefined||_<-tl(Attributes)]]),
    create(Table, ParentDnRev, IxValueB, NewObj).

%%% ---------------------------------------------------------
%%% @doc Create an MO using a record with default values
%%% @end

-spec create(Table::atom(), ParentDnRev::dn(), IxValueB::binary(),
	     DefaultObj::tuple()) -> {ok, tuple()}.


create(_, ParentDnRev, IxValueB, DefaultObj) ->
    Value = binary_to_list(IxValueB),
    TKey = list_to_tuple(tuple_to_list(dnrev_to_key(ParentDnRev))++[Value]),
    NewObj = setelement(2, DefaultObj, TKey),
    mnesia:write(NewObj),
    {ok, NewObj}.


-spec create(Table::atom(), ParentDnRev::dn(), IxValueB::binary(),
	     NamedAttrs::named_attributes(), Fields::fields()) -> {ok, tuple()}.

create(Table, ParentDnRev, IxValueB, NamedAttrs, Fields) ->
    DefaultObj = list_to_tuple([Table|sort_fields(NamedAttrs, Fields)]),
    create(Table, ParentDnRev, IxValueB, DefaultObj).

sort_fields(NamedAttrs, [{Field,_}|Fields]) ->
    FieldB = list_to_binary(atom_to_list(Field)),
    [proplists:get_value(FieldB, NamedAttrs, undefined)|
     sort_fields(NamedAttrs, Fields)];
sort_fields(_, []) ->
    [].

%%% ---------------------------------------------------------
%%% @doc Delete an MO
%%% @end

-spec delete(DnRev::dn(), Table::atom()) -> ok.

delete(DnRev, Table) ->
    Key = dnrev_to_key(DnRev),
    mnesia:delete({Table, Key}).

%%% ---------------------------------------------------------
%%% @doc Convert a struct in record format to a COM value list
%%% @end


format_struct(undefined, _) ->
    [?STRUCT];
format_struct(Structs, Types) when is_list(Structs) ->
    [?STRUCT|
     [lists:nth(2,format_struct(Struct, Types))||Struct<-Structs]];
format_struct(Struct, Types) ->
    [?STRUCT,
     format_struct_members(tl(tuple_to_list(Struct)), Types)].

format_struct_members([Value|Values], [{FieldA, InternalType}|Types]) ->
    FieldB = list_to_binary(atom_to_list(FieldA)),
    [?STRUCT_MEMBER(FieldB,
		    comsaEcimModelAdaptor:type(InternalType, Value))|
     format_struct_members(Values, Types)];
format_struct_members([], []) ->
    [].

%%% ---------------------------------------------------------
%%% @doc Make a struct from a list of types and values
%%% @end


make_struct(Name, TypeAndValue, Fields) ->
    MemberValues = element(2, TypeAndValue),
    list_to_tuple([Name|[get_struct_member(MemberValues, Field)||Field<-Fields]]).

%%% ---------------------------------------------------------
%%% @doc Test if an MO exists
%%% @end

-spec existsMo(DnRev::dn(), Table::atom()) -> boolean().

existsMo(DnRev, Table) ->
    Key = dnrev_to_key(DnRev),
    case mnesia:read({Table, Key}) of
	[] ->
	    false;
	[_] ->
	    true
    end.

%%% ---------------------------------------------------------
%%% @doc Count the immediate children of an MO
%%% @end

-spec countMoChildren(DnRev::dn(), Table::atom()) -> integer().

countMoChildren(DnRev, Table) ->
    ParentKey = dnrev_to_key(DnRev),
    MatchKey = list_to_tuple(tuple_to_list(ParentKey)++['_']),
    WP = mnesia:table_info(wild_pattern, Table),
    Pattern = setelement(2, WP, MatchKey),
    Objs = mnesia:match_object(Pattern),
    length(Objs).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

get_struct_member([MemberValue|MemberValues], {Name, Type}) ->
    case list_to_atom(binary_to_list(element(1, MemberValue))) of
	Name ->
	    case element(2, MemberValue) of
		{9, Bin} ->
		    binary_to_list(Bin);
		{_, X} ->
		    X
	    end;
	_ ->
	    get_struct_member(MemberValues, {Name,Type})
    end;
get_struct_member([], Field) ->
    erlang:error(no_value_provided, [[], Field]).

get_internal_type(AttributeAtom, Fields) ->
    {value, {_, InternalType}} =
        lists:keysearch(AttributeAtom, 1, Fields),
    InternalType.

get_field_id(Name, Fields) ->
    get_field_id(Name, 1, Fields).

get_field_id(Name, N, [{Name, _}|_]) ->
    N+1;
get_field_id(Name, N, [_|Fields]) ->
    get_field_id(Name, N+1, Fields);
get_field_id(Name, _, []) ->
    erlang:error({name_not_found, Name}).

%% Special handling of the ECIM password struct
get_new_obj({struct, 'EcimPassword'}, _, FieldId, Obj, Value)
  when is_list(Value) ->
    [{<<"password">>,{9,Encrypted}},{<<"cleartext">>,undefined}] = Value,
    PwStruct = #'EcimPassword'{password=binary_to_list(Encrypted)},
    setelement(FieldId, Obj, PwStruct);

%% If set_struct is Type to set, then we know that set_struct has been called
get_new_obj({sequence, {struct, StructName}}, Type, _, Obj, _) when Type /= set_struct ->
    sysInitI:error_msg("Sequence of struct ~p. "
			   "Use set_struct/7~n",[StructName]),
    Obj;
%% If set_struct is Type to set, then we know that set_struct has been called
get_new_obj({struct, StructName}, Type, _, Obj, _) when Type /= set_struct ->
    sysInitI:error_msg("Struct ~p. Use set_struct/7~n",
			   [StructName]),
    Obj;
get_new_obj({sequence, string}, ?STRING, FieldId, Obj, Values) ->
    setelement(FieldId, Obj, [binary_to_list(Value)||Value<-Values]);
get_new_obj(_, ?STRING, FieldId, Obj, Value) when is_list(Value)->
    %% Clause added: used by COM when setting SNMP views
    setelement(FieldId, Obj, Value);
get_new_obj(_, ?STRING, FieldId, Obj, Value) ->
    setelement(FieldId, Obj, binary_to_list(Value));
get_new_obj(_, _, FieldId, Obj, Value) ->
    setelement(FieldId, Obj, Value).

%% We assume here that all fields of a struct is delivered in TypeAndValue
%% We also assume that in StructFields fields appear in order

%% TypesAndValues =
%% {14,
%%  [{<<"failedTransfer">>,
%%    {9,
%%     <<"ManagedElement=1,SystemFunctions=1,FileM=1,LogicalFs=1,FileGroup=backup">
%% >}},
%%   {<<"remoteFileServerGroup">>,
%%    {9,
%%     <<"ManagedElement=1,SystemFunctions=1,FileM=1,RemoteFileServerGroup=oss">>}}
%% ,
%%   {<<"uriPathExtrension">>,{9,<<"backup">>}}]}

%% StructFields =
%%   [{uriPathExtrension, string},
%%    {remoteFileServerGroup, moRef},
%%    {failedTransfer, moRef}]


fill_struct({_, FieldsB}, StructName, StructFields) ->
    Struct = list_to_tuple([StructName|[undefined||_<-StructFields]]),
    do_fill_struct(FieldsB, Struct, 2, StructFields).


do_fill_struct(FieldsB, Struct, FieldId, [{Field, Type}|StructFields]) ->
    ErrorMsg = "Nested structs are not supported. Use "
	"comsaGeneric:make_struct/3 to convert secondary struct levels~n"
	"~p~n",
    FieldB    = list_to_binary(atom_to_list(Field)),
    FieldKS   = lists:keysearch(FieldB, 1, FieldsB),
    NewStruct = dfs(FieldKS, Type, Struct, FieldId, ErrorMsg),
    do_fill_struct(FieldsB, NewStruct, FieldId+1, StructFields);
do_fill_struct(_, Struct, _, []) ->
    Struct.

dfs({value, {_, {_, Value}}}, {sequence, {struct, StructName}}, Struct, _, ErrorMsg)
  when is_tuple(Value),
       element(1, Value) == ?STRUCT ->
    sysInitI:error_msg(ErrorMsg, [StructName]),
    Struct;
dfs({value, {_, {_, Value}}}, {sequence, {struct, StructName}}, Struct, _, ErrorMsg)
  when is_list(Value),
       is_tuple(hd(Value)),
       element(1,hd(Value)) == ?STRUCT ->
    sysInitI:error_msg(ErrorMsg,[StructName]),
    Struct;
dfs({value, {_, {_, Value}}}, {struct, StructName}, Struct, _, ErrorMsg)
  when is_tuple(Value),
       element(1, Value) == ?STRUCT ->
    sysInitI:error_msg(ErrorMsg,[StructName]),
    Struct;
dfs({value, {_FieldB, {?STRING, Value}}}, _, Struct, FieldId, _) ->
    setelement(FieldId, Struct, binary_to_list(Value));
dfs({value, {_FieldB, {_, Value}}}, _, Struct, FieldId, _) ->
    setelement(FieldId, Struct, Value);
dfs({value, {_FieldB, undefined}}, _, Struct, FieldId, _) ->
    setelement(FieldId, Struct, undefined);
dfs(false, _, Struct, _, _) ->
    Struct.

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
