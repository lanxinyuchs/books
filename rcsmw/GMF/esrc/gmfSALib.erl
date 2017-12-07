%%% #---------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfSALib.erl %
%%% Author:	qthupha
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(gmfSALib).
-vsn('/main/R1A/R2A/R3A/R4A/R6A/R7A/R8A/R10A/R11A/R12A/1').
-date('2017-11-24').
-author('qostjoa').
-shaid('b1b28fdbb2ecc670970fb0bf1c770786130b6ca0').
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
%%% -----      ---------  --------    ------------------------
%%% R1A/1      2012-04-18 qthupha     Created
%%% R1A/13     2014-01-08 uabesvi     fixes for OaM over TN
%%% R3A/1      2015-01-14 etxpeno     Correction of immdn_to_moref when a
%%%                                   class has multiple parents
%%% R4A/1      2015-06-15 erarafo     Handle IMM MOMs under SystemFunctions=1
%%% R4A/2      2015-06-18 erarafo     Removed test code
%%% R6A/3      2016-09-15 etxpeno     MIB sync improvements
%%% R7A/1      2016-11-01 etxpeno     (TR HV37485) Improvement of CM related tasks
%%% R8A/1      2016-11-09 etxpeno     Corrections in create_dn_map/3
%%% R8A/2      2016-12-13 etxpeno     Add to_coi/2
%%% R8A/4      2017-01-09 etxpeno     Correction in create_dn_map/3
%%% R10A/1     2017-05-08 etxpeno     (TR HV86130)
%%% R11A/1     2017-08-15 etxpeno     Handle external moRefs
%%% R11A/2     2017-09-07 etxpeno     TR HW26710
%%% R12A/1     2017-11-21 qostjoa     Add support for RmeSds split
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([get_com_attr_value/2]).
-export([get_imm_attr_value/2]).
-export([get_safe_type/1]).
-export([get_imm_attrs/2]).
-export([get_com_basic_type/1]).
-export([convert_ao_scope/1]).

-export([get_dn_map/1, get_imm_dn/2, get_mim_dn/2, get_parent_imm_dn/2]).
-export([mim_to_imm/1, imm_to_mim/1, get_moref/2]).
-export([to_coi/2, get_attribute_names/1]).

-export([struct_values/1, get_immCsStruct/2]).

-include("gmf.hrl").
-include("comTypes.hrl").
-include("om.hrl").


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------



%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%===========================================================================
%% @spec get_com_attr_value(Type, Value)
%%
%%       -> ok
%%
%% where
%%
%%   Type  = atom()
%%   Value = term()
%%
%% @doc
%%
%% Transform IMM values to COM values
%%
%% @end
%%===========================================================================
get_com_attr_value(_Type,   [])    -> undefined;
get_com_attr_value(boolean, Value) -> ?BOOL(Value =/= 0);
get_com_attr_value(int8,    Value) -> ?INT8(Value);
get_com_attr_value(int16,   Value) -> ?INT16(Value);
get_com_attr_value(int32,   Value) -> ?INT32(Value);
get_com_attr_value(int64,   Value) -> ?INT64(Value);
get_com_attr_value(enumRef, Value) -> ?ENUM(Value);
get_com_attr_value(uint8,   Value) -> ?UINT8(Value);
get_com_attr_value(uint16,  Value) -> ?UINT16(Value);
get_com_attr_value(uint32,  Value) -> ?UINT32(Value);
get_com_attr_value(uint64,  Value) -> ?UINT64(Value);
get_com_attr_value(string,  Value) -> ?STRING(Value).


%%===========================================================================
%% @spec get_imm_attrs(StructName, MomName)
%%
%%       -> ok
%%
%% where
%%
%%   SafeType = atom()
%%   Value    = term()
%%
%% @doc
%%
%% Get all attributes in a struct
%%
%% @end
%%===========================================================================
get_imm_attrs(StructName, MomName) ->
    StructKey = {MomName, StructName},
    [#gmfMimStruct{attributes = Attrs}] = ets:lookup(gmfMimStruct, StructKey),
    Attrs.

%%===========================================================================
%% @spec get_imm_attr_value(SafeType, Value)
%%
%%       -> ok
%%
%% where
%%
%%   SafeType = atom()
%%   Value    = term()
%%
%% @doc
%%
%% Transform COM values to IMM values
%%
%% @end
%%===========================================================================
get_imm_attr_value(sa_imm_attr_saint32t, Value) ->
    #safsImmAttrValue{saint32 = Value};
get_imm_attr_value(sa_imm_attr_sauint32t, Value) ->
    #safsImmAttrValue{sauint32 = Value};
get_imm_attr_value(sa_imm_attr_saint64t, Value) ->
    #safsImmAttrValue{saint64 = Value};
get_imm_attr_value(sa_imm_attr_sauint64t, Value) ->
    #safsImmAttrValue{sauint64 = Value};
get_imm_attr_value(sa_imm_attr_safloatt, Value) ->
    %%#safe_imm_attr_value{safloat = list_to_float(binary_to_list(Value))};
    #safsImmAttrValue{safloat = Value};
get_imm_attr_value(sa_imm_attr_sanamet, Value) when is_list(Value) ->
    #safsImmAttrValue{saname = list_to_binary(Value)};
get_imm_attr_value(sa_imm_attr_sanamet, Value) when is_binary(Value) ->
    #safsImmAttrValue{saname = Value};
get_imm_attr_value(sa_imm_attr_sastringt, Value) ->
    #safsImmAttrValue{sastring = Value};
get_imm_attr_value(sa_imm_attr_csstructt, Value) ->
    #safsImmAttrValue{csstruct = Value}.

%%===========================================================================
%% @spec get_safe_type(Type)
%%
%%       -> SafeType
%%
%% where
%%
%%   Type     = atom()
%%   SafeType = atom()
%%
%% @doc
%%
%% Transform COM types to SAFE types
%%
%% @end
%%===========================================================================
get_safe_type(boolean)   -> sa_imm_attr_saint32t;
get_safe_type(int8)      -> sa_imm_attr_saint32t;
get_safe_type(int16)     -> sa_imm_attr_saint32t;
get_safe_type(int32)     -> sa_imm_attr_saint32t;
get_safe_type(int64)     -> sa_imm_attr_saint64t;
get_safe_type(enumRef)   -> sa_imm_attr_saint32t;
get_safe_type(uint8)     -> sa_imm_attr_sauint32t;
get_safe_type(uint16)    -> sa_imm_attr_sauint32t;
get_safe_type(uint32)    -> sa_imm_attr_sauint32t;
get_safe_type(uint64)    -> sa_imm_attr_sauint64t;
get_safe_type(float)     -> sa_imm_attr_safloatt;
%% get_safe_type(double) -> sadouble;
get_safe_type(string)    -> sa_imm_attr_sastringt;
get_safe_type(moRef)     -> sa_imm_attr_sanamet;
get_safe_type(structRef) -> sa_imm_attr_csstructt.

%%===========================================================================
%% @spec get_com_basic_type(Props)
%%
%%       -> ComType
%%
%% where
%%
%%   Props   = [Prop] | tuple()
%%   Prop    = term()
%%   Comtype = term()
%%
%% @doc
%%
%% Returns com basic type from attribute properties or
%% from a complex type
%% In case of sequence and derivedDataType,
%% the simple type defining them is returned along
%% with the actual type.
%%
%% @end
%%===========================================================================
get_com_basic_type(Props) when is_list(Props) ->
    get_com_basic_type(proplists:get_value(dataType, Props));
get_com_basic_type({sequence, S} = Type) ->
    {SType, _} = get_com_basic_type(S),
    {SType, Type};
get_com_basic_type({derivedDataTypeRef, DType, MomName} = Type) ->
    Key = {MomName, DType},
    [Rec] = ets:lookup(gmfMimDerivedType, Key),
    {proplists:get_value(dataType, Rec#gmfMimDerivedType.basetype), Type};
get_com_basic_type(Type) ->
    {Type, Type}.

%%===========================================================================
%% @spec convert_ao_scope(Scope)
%%
%%       -> ImmScope
%%
%% where
%%
%%   Scope    = one | sublevel | subtree
%%   ImmScope = atom()
%%
%% @doc
%%
%% convert to IMM admin owner scope
%%
%% @end
%%===========================================================================
convert_ao_scope(one)      -> sa_imm_one;
convert_ao_scope(sublevel) -> sa_imm_sublevel;
convert_ao_scope(subtree)  -> sa_imm_subtree.

struct_values(V) when is_record(V, safsImmCsStruct) ->
    StructMembers = V#safsImmCsStruct.structMembers,
    struct_values(StructMembers);
struct_values(V) when is_record(V, safsImmAttrValues_2) ->
    AttrName = V#safsImmAttrValues_2.attrName,
    Type = V#safsImmAttrValues_2.attrValueType,
    Name = safs_imm_lib:convert_attribute_name_to_binary(AttrName),
    AttrValues = V#safsImmAttrValues_2.attrValues,
    Values = [get_attr_value(Type, AttrValue) || AttrValue <- AttrValues],
    {Name, Type, Values};
struct_values(List) when is_list(List) ->
    [struct_values(V) || V <- List];
struct_values(V) ->
    V.

get_attr_value(sa_imm_attr_saint32t,
	       AttrValue) -> AttrValue#safsImmAttrValue.saint32;
get_attr_value(sa_imm_attr_sauint32t,
	       AttrValue) -> AttrValue#safsImmAttrValue.sauint32;
get_attr_value(sa_imm_attr_saint64t,
	       AttrValue) -> AttrValue#safsImmAttrValue.saint64;
get_attr_value(sa_imm_attr_sauint64t,
	       AttrValue) -> AttrValue#safsImmAttrValue.sauint64;
get_attr_value(sa_imm_attr_satimet,
	       AttrValue) -> AttrValue#safsImmAttrValue.satime;
get_attr_value(sa_imm_attr_sanamet,
	       AttrValue) -> AttrValue#safsImmAttrValue.saname;
get_attr_value(sa_imm_attr_safloatt,
	       AttrValue) -> AttrValue#safsImmAttrValue.safloat;
get_attr_value(sa_imm_attr_sadoublet,
	       AttrValue) -> AttrValue#safsImmAttrValue.sadouble;
get_attr_value(sa_imm_attr_sastringt,
	       AttrValue) -> AttrValue#safsImmAttrValue.sastring;
get_attr_value(sa_imm_attr_saanyt,
	       AttrValue) -> AttrValue#safsImmAttrValue.saany;
get_attr_value(sa_imm_attr_csstructt,
	       AttrValue) -> AttrValue#safsImmAttrValue.csstruct.

get_immCsStruct(StructName, Structvals) ->
    [begin
	 V = com2imm(S),
	 #safsImmCsStruct{structName    = list_to_binary(StructName),
			  structMembers = V}
     end || S <- Structvals].

com2imm(AttrValuesFromCom) ->
    [com2imm(Attr, {Type, Values}) ||
        {Attr, {Type, Values}} <- AttrValuesFromCom].

com2imm(Attr, {Type, Values}) ->
    SafeType = get_safe_type(Type),
    AttrVals = [get_imm_attr_value(SafeType, V) || V <- Values],
    #safsImmAttrValues_2{attrName         = bin(Attr),
                         attrValueType    = SafeType,
                         attrValuesNumber = length(Values),
                         attrValues       = AttrVals}.

bin(Bin) when is_list(Bin) ->
    list_to_binary(Bin);
bin(Bin) ->
    Bin.

-spec get_dn_map({MimPath, Type}) ->
			{ok, DnMap, MimValues} | {error, Reason} when
      MimPath   :: [binary()],
      Type      :: mim | imm,
      DnMap     :: #gmfDnMap{},
      MimValues :: [binary()],
      Reason    :: term().
get_dn_map({MimPath, mim}) ->
    try
	{MimValues, MimNames} = split(MimPath),

	get_mim_dn_map({MimValues, MimNames}, true)
    catch
	throw:{?MODULE, error, R} -> {error, R}
    end;
get_dn_map({ImmPath, imm}) ->
    try
	{ImmNames, ImmValues} = split(ImmPath),

	case mnesia:dirty_index_read(gmfDnMap, ImmNames, #gmfDnMap.imm_names) of
	    [DnMap] ->
		{ok, DnMap, ImmValues};
	    [] ->
		DnMap = create_dn_map(ImmNames, [], imm),
		ok = mnesia:dirty_write(DnMap),
		{ok, DnMap, ImmValues}
	end
    catch
	throw:{?MODULE, error, R} -> {error, R}
    end.

get_mim_dn_map({MimValues, MimNames}, Retry) ->
    case ets:lookup(gmfDnMap, MimNames) of
	[DnMap] ->
	    {ok, DnMap, MimValues};
	[] when Retry ->
	    %% In some cases, a element in MimNames might look like
	    %% <<"E1T1Port.e1t1PortId">>
	    %% In this case the classname is "E1T1Port"
	    %% Remove the period and the Id after the period and retry
	    NewMimNames = [re:replace(C, <<"\\..*$">>, <<"">>,
				      [{return,binary}]) || C <- MimNames],

	    get_mim_dn_map({MimValues, NewMimNames}, false);
	[] ->
	    DnMap = create_dn_map(MimNames, [], mim),
	    ok = mnesia:dirty_write(DnMap),
	    {ok, DnMap, MimValues}
    end.

create_dn_map([], [],        _) ->
    throw({?MODULE, error, ""});
create_dn_map([], Acc, _) ->
    check_dn(Acc, []);
create_dn_map(DnMap, [], _) when is_record(DnMap, gmfDnMap) ->
    DnMap;
create_dn_map([Class|R], Acc, mim) ->
    Classes = [Class|Acc],
    case ets:lookup(gmfDnMap, R) of
	[] ->
	    create_dn_map(R, Classes, mim);
	[DnMap] ->
	    create_dn_map(DnMap, Classes, mim)
    end;
create_dn_map([ImmRdn|R], Acc, imm) ->
    NewAcc = [ImmRdn|Acc],
    case mnesia:dirty_index_read(gmfDnMap, R, #gmfDnMap.imm_names) of
	[] ->
	    create_dn_map(R, NewAcc, imm);
	[DnMap] ->
	    create_dn_map(DnMap, NewAcc, imm)
    end;
create_dn_map(DnMap, [Class|R], mim) when is_record(DnMap, gmfDnMap) ->
    ClassL = binary_to_list(Class),

    MimChildrenRec = ets:lookup(gmfMimChildren, DnMap#gmfDnMap.mim_class),
    MimChildrenRec /= [] orelse throw({?MODULE, error, ClassL}),
    [#gmfMimChildren{children = Children}] = MimChildrenRec,

    MimClass = lists:keyfind(ClassL, 2, Children),
    MimClass /= false orelse throw({?MODULE, error, ClassL}),

    Pattern = #gmfMimClass{key     = MimClass,
			   imm_rdn = '$1',
			   _       = '_'},
    [[ImmRdnL]] = ets:match(gmfMimClass, Pattern),

    OldMimNames = DnMap#gmfDnMap.mim_names,
    MimNames = [Class|OldMimNames],

    OldImmNames = DnMap#gmfDnMap.imm_names,
    ImmRdn = list_to_binary(ImmRdnL),
    ImmNames = [ImmRdn|OldImmNames],

    NewDnMap = DnMap#gmfDnMap{mim_names = MimNames,
			      imm_names = ImmNames,
			      mim_class = MimClass},
    create_dn_map(NewDnMap, R, mim);
create_dn_map(DnMap, [ImmRdn|R], imm) when is_record(DnMap, gmfDnMap) ->
    ImmRdnL = binary_to_list(ImmRdn),

    MimChildrenRec = ets:lookup(gmfMimChildren, DnMap#gmfDnMap.mim_class),
    MimChildrenRec /= [] orelse throw({?MODULE, error, ImmRdnL}),
    [#gmfMimChildren{children = Children}] = MimChildrenRec,

    MimClassRec =
	mnesia:dirty_index_read(gmfMimClass, ImmRdnL, #gmfMimClass.imm_rdn),
    MimClassRec /= [] orelse throw({?MODULE, error, ImmRdnL}),
    [#gmfMimClass{key = MimClass}] = MimClassRec,

    lists:member(MimClass, Children) orelse throw({?MODULE, error, ImmRdnL}),

    OldMimNames = DnMap#gmfDnMap.mim_names,
    {_, ClassL} = MimClass,
    Class = list_to_binary(ClassL),
    MimNames = [Class|OldMimNames],

    OldImmNames = DnMap#gmfDnMap.imm_names,
    ImmNames = [ImmRdn|OldImmNames],

    NewDnMap = DnMap#gmfDnMap{mim_names = MimNames,
			      imm_names = ImmNames,
			      mim_class = MimClass},
    create_dn_map(NewDnMap, R, imm).

check_dn([<<"ManagedElement">>|[Class|T]], []) ->
    MimNames = [Class, <<"ManagedElement">>],
    case ets:lookup(gmfDnMap, MimNames) of
	[] ->
	    ClassL = binary_to_list(Class),
	    throw({?MODULE, error, ClassL});
	[_] ->
	    check_dn(T, MimNames)
    end;
check_dn([Class|T], OldMimNames) ->
    MimNames = [Class | OldMimNames],
    case ets:lookup(gmfDnMap, MimNames) of
	[] ->
	    ClassL = binary_to_list(Class),
	    throw({?MODULE, error, ClassL});
	[_] ->
	    check_dn(T, MimNames)
    end.

-spec get_imm_dn(DnMap, MimValues) -> ImmDn when
      DnMap     :: #gmfDnMap{},
      MimValues :: [binary()],
      ImmDn     :: binary().
get_imm_dn(DnMap, MimValues) ->
    ImmNames = DnMap#gmfDnMap.imm_names,
    ImmValues = lists:droplast(MimValues),
    join({ImmNames, ImmValues}).

-spec get_mim_dn(DnMap, ImmValues) -> MimDn when
      DnMap     :: #gmfDnMap{},
      ImmValues :: [binary()],
      MimDn     :: binary().
get_mim_dn(DnMap, ImmValues) ->
    MimNames = lists:reverse(DnMap#gmfDnMap.mim_names),
    MimValues = ["1"|lists:reverse(ImmValues)],
    join({MimNames, MimValues}).

-spec get_parent_imm_dn(DnMap, MimValues) -> ParentImmDn when
      DnMap       :: #gmfDnMap{},
      MimValues   :: [binary()],
      ParentImmDn :: binary().
get_parent_imm_dn(DnMap, MimValues) ->
    ParentMimValues = tl(MimValues),
    ParentImmNames = tl(DnMap#gmfDnMap.imm_names),
    ParentImmValues = lists:droplast(ParentMimValues),
    join({ParentImmNames, ParentImmValues}).

-spec mim_to_imm(MimDn) -> {ok, ImmDn} | {error, Reason} when
      MimDn  :: binary(),
      ImmDn  :: binary(),
      Reason :: term().
mim_to_imm(<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1",_/binary>> = MimDn) ->
    {ok, MimDn};
mim_to_imm(<<"ManagedElement=1,NodeSupport=1,ServiceDiscovery=1">> = MimDn) ->
    {ok, MimDn};
mim_to_imm(<<"ManagedElement=1,NodeSupport=1,ServiceDiscoveryServer=1">> = MimDn) ->
    {ok, MimDn};
mim_to_imm(MimDn) ->
    MimPath = parse(MimDn, mim),
    parsed_mim_to_imm(MimPath).

parsed_mim_to_imm([_, <<"ManagedElement">>]) ->
    {ok, <<"">>};
parsed_mim_to_imm([_, <<"SystemFunctions">>,
		   _, <<"ManagedElement">>]) ->
    {ok, <<"">>};
parsed_mim_to_imm([_, <<"SysM">>,
		   _, <<"SystemFunctions">>,
		   _, <<"ManagedElement">>]) ->
    {ok, <<"">>};
parsed_mim_to_imm(MimPath) ->
    case get_dn_map({MimPath, mim}) of
	{ok, DnMap, MimValues} ->
	    ImmDn = get_imm_dn(DnMap, MimValues),
	    {ok, ImmDn};
	Error ->
	    Error
    end.

-spec imm_to_mim(ImmDn) -> {ok, MimDn} | {error, Reason} when
      ImmDn  :: binary(),
      MimDn  :: binary(),
      Reason :: term().
imm_to_mim(<<"ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1",_/binary>> = ImmDn) ->
    {ok, ImmDn};
imm_to_mim(<<"ManagedElement=1,NodeSupport=1,ServiceDiscovery=1">> = ImmDn) ->
    {ok, ImmDn};
imm_to_mim(<<"ManagedElement=1,NodeSupport=1,ServiceDiscoveryServer=1">> = ImmDn) ->
    {ok, ImmDn};
imm_to_mim(ImmDn) ->
    ImmPath = parse(ImmDn, imm),

    case get_dn_map({ImmPath, imm}) of
	{ok, DnMap, ImmValues} ->
	    MimDn = get_mim_dn(DnMap, ImmValues),
	    {ok, MimDn};
	Error ->
	    Error
    end.

-spec get_moref(Dn, Type) -> {ok, Moref} | {error, Reason} when
      Dn  :: binary(),
      Type :: internal|external,
      Moref  :: binary(),
      Reason :: term().
get_moref(<<"ManagedElement=1",_/binary>> = MimDn, internal) ->
    %% An ordinary node-internal ECIM DN. Try to map it to an IMM DN.
    mim_to_imm(MimDn);
get_moref(<<"ManagedElement=1",_/binary>> = MimDn, external) ->
    %% An node-internal ECIM DN. Probable the CertM case.
    %% Try to map it to an ECIM DN.
    imm_to_mim(MimDn);
get_moref(<<"ManagedElement=",_/binary>> = MimDn, _) ->
    %% A node-external ECIM DN. Use the DN as is.
    {ok, MimDn};
get_moref(Dn, internal) ->
    %% Some unknown type of Dn. Try to map it to an IMM DN.
    %% Will probable return an error
    mim_to_imm(Dn);
get_moref(ImmDn, external) ->
    %% An ordinary IMM DN. Try to map it to an ECIM DN.
    imm_to_mim(ImmDn).

split(Dn) -> split(Dn, [], []).

split([N, V | R], Nacc, Vacc) -> split(R, [N|Nacc], [V|Vacc]);
split([N], _, _)              -> throw({?MODULE, error, binary_to_list(N)});
split([], N, V)               -> {lists:reverse(N), lists:reverse(V)}.

join({NameList, ValueList}) -> iolist_to_binary(join(NameList, ValueList)).

join([N], [V])           -> [[N, $=, V]];
join([N | Ns], [V | Vs]) -> [[N, $=, V, $,] | join(Ns , Vs)];
join([], [])             -> [].

parse(Dn, mim) ->
    L = binary:split(Dn, [<<",">>,<<"=">>], [global]),
    lists:reverse(L);
parse(Dn, imm) ->
    binary:split(Dn, [<<",">>,<<"=">>], [global]).

to_coi(ImmDn, Values) ->
    ImmPath = parse(ImmDn, imm),
    R = get_dn_map({ImmPath, imm}),
    to_coi2(R, Values).

to_coi2({ok, DnMap, ImmValues}, Values) ->
    MimDn = get_mim_dn(DnMap, ImmValues),
    try
	ComteValues = get_coi_values(DnMap, Values),
	{ok, MimDn, ComteValues}
    catch
	throw:{?MODULE, error, R} -> {error, R}
    end;
to_coi2(Error, _Values) ->
    Error.

get_coi_values(DnMap, Values) ->
    MimClass = DnMap#gmfDnMap.mim_class,
    Pattern = #gmfMimClass{key        = MimClass,
			   attributes = '$1',
			   _          = '_'},
    [[Attrs]] = ets:match(gmfMimClass, Pattern),

    [get_coi_value(V, Attrs) || V <- Values].

get_coi_value({Attr, V}, Attrs) ->
    AttrL = binary_to_list(Attr),
    Props = proplists:get_value(AttrL, Attrs, []),
    Props =/= [] orelse throw({?MODULE, error, AttrL}),
    DataType = proplists:get_value(dataType, Props),
    NewV = gcv(DataType, V),
    {Attr, NewV}.

gcv(_DataType, []) -> [];
gcv({sequence, {derivedDataTypeRef, DType, MomName}}, VList) ->
    DataType = get_datatype(DType, MomName),
    gcv({sequence, DataType}, VList);
gcv({sequence, DataType}, VList) -> [gcv(DataType, [V]) || V <- VList];
gcv({int8, _, _}, [V]) -> {?INT8, V};
gcv({int16, _, _}, [V]) -> {?INT16, V};
gcv({int32, _, _}, [V]) -> {?INT32, V};
gcv({int64, _, _}, [V]) -> {?INT64, V};
gcv({uint8, _, _}, [V]) -> {?UINT8, V};
gcv({uint16, _, _}, [V]) -> {?UINT16, V};
gcv({uint32, _, _}, [V]) -> {?UINT32, V};
gcv({uint64, _, _}, [V]) -> {?UINT64, V};
gcv({string, _, _}, [V]) -> {?STRING, V};
gcv({enumRef, _, _}, [V]) -> {?ENUM, V};
gcv({boolean, _, _}, [V]) -> {?BOOL, get_bool_value(V)};
gcv({moRef, _, _}, [V]) -> {?REFERENCE, get_moref_value(V)};
gcv({structRef, StructName, MomName}, [V]) ->
    Attrs = get_imm_attrs(StructName, MomName),
    [StructValue] = struct_values([V]),
    {?STRUCT, structref_attr(StructValue, Attrs, [])};
gcv({derivedDataTypeRef, DType, MomName}, [V]) ->
    DataType = get_datatype(DType, MomName),
    gcv(DataType, [V]).

get_bool_value(0) -> false;
get_bool_value(_) -> true.

get_moref_value(Value) ->
    case imm_to_mim(Value) of
        {ok, MimDn} ->
            MimDn;
        {error, _} ->
            Value
    end.

get_datatype(DType, MomName) ->
    Key = {MomName, DType},
    [Rec] = ets:lookup(gmfMimDerivedType, Key),
    proplists:get_value(dataType, Rec#gmfMimDerivedType.basetype).

structref_attr([], RestAttrs, StructAttrs) ->
    C = [?STRUCT_MEMBER(list_to_binary(Attr), []) || {Attr, _} <- RestAttrs],
    lists:sort(StructAttrs ++ C);
structref_attr([{ImmAttr, _ImmType, Value}|Rest], Attrs, StructAttrs) ->
    Attr = binary_to_list(ImmAttr),
    {value, {Attr, Props}, NewAttrs} = lists:keytake(Attr, 1, Attrs),
    DataType = proplists:get_value(dataType, Props),
    CoiValue = gcv(DataType, Value),
    StructAttr = ?STRUCT_MEMBER(ImmAttr, CoiValue),
    NewStructAttrs = [StructAttr | StructAttrs],
    structref_attr(Rest, NewAttrs, NewStructAttrs).

get_attribute_names(ImmDn) ->
    ImmPath = parse(ImmDn, imm),

    case get_dn_map({ImmPath, imm}) of
	{ok, DnMap, _ImmValues} ->

	    Pattern = #gmfMimClass{key        = DnMap#gmfDnMap.mim_class,
				   attributes = '$1',
				   _          = '_'},

	    [[Attrs]] = ets:match(gmfMimClass, Pattern),

	    AttrNames = lists:filtermap(
			  fun({AttrNameL, Props}) ->
				  case proplists:is_defined(key, Props) of
				      true ->
					  false;
				      false ->
					  AttrName = list_to_binary(AttrNameL),
					  {true, AttrName}
				  end
			  end, Attrs),

	    {ok, AttrNames};
	Error ->
	    Error
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
