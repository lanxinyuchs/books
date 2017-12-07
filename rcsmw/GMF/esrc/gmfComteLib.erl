%%% #---------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfComteLib.erl %
%%% Author:	qthupha
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(gmfComteLib).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/2').
-date('2017-10-05').
-author('etxpeno').
-shaid('61e59cc718cbab48c0d829fe6d01fdbd5d1cbdbb').
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
%%% R2A/15     2013-03-19 uabesvi     dialyzer errors
%%% R2A/42     2013-12-06 etxberb     'update_moRef' takes care of error results
%%%                                   from 'gmfImmOmI:ccb_object_read'.
%%% R2A/49     2014-04-02 etxpeno     TR HS37576
%%% R3A/1      2014-10-10 etxberb     Changed integer_to_list to
%%%                                   sysUtil:term_to_string.
%%% R3A/3      2014-11-17 etxberb     createMo now includes attribute settings
%%%                                   to IMM instead of separating them into
%%%                                   an explicit modify call to IMM.
%%% R3A/4      2014-11-19 etxberb     Bug fix.
%%% R3A/6      2014-12-16 etxpeno     Bug fix in sMA_prep
%%% R3A/7      2015-01-06 etxpeno     Fixes in code and comments
%%% R5A/2      2016-02-01 etxpeno     fix of struct handling in get_comte_value/3
%%%                                   (Part of TR HU54556)
%%% R5A/3      2016-02-24 etxpeno     Support for struct as returnType in actions
%%% R5A/4      2016-02-29 etxpeno     handle all values of Name in mav2/1
%%% R6A/1      2016-06-17 etxpeno     struct as attribute
%%% R6A/2-3    2016-09-07 etxpeno     Improvement of MIB sync
%%% R6A/4-5    2016-09-15 etxpeno     Improvement of MIB sync
%%% R7A/1      2016-10-06 etxpeno     Improve error to the operator when
%%%                                   admin_owner_set fails in action/6
%%%                                   (TR HV31163)
%%% R7A/2      2016-10-13 etxpeno     Correction in structref_attr/4
%%% R7A/3      2016-11-01 etxpeno     (TR HV37485) Improvement of CM related tasks
%%% R8A/1      2016-12-13 etxpeno     Remove support for 'struct as object'
%%% R9A/2      2017-02-09 etxpeno     Improve handling of invalid moRefs
%%% R10A/1     2017-04-21 etxpeno     Improve admin owner handling
%%% R10A/2     2017-04-28 etxpeno     Correction due to the changes in R10A/1
%%% R10A/3     2017-06-20 etxpeno     Improve nextMo/6
%%% R10A/4     2017-06-30 etxpeno     Revert R10A/3 (TR HV99375)
%%% R11A/1     2017-08-15 etxpeno     Handle external moRefs
%%% R11A/2     2017-10-05 etxpeno     Improve nextMo/6
%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([getMoAttribute/5, nextMo/6]).
-export([createMo/7,setMoAttribute/7,deleteMo/5]).
-export([prepare/6,commit/6,finish/6,action/6]).

-export([commit/4]).

%% -export([prepareReplay/4]).

-export([existsMo/5, countMoChildren/6, getMoAttributes/6,
	 setMoAttributes/6, action/7, createMo/8]).

%% From IMM callback
-export([action_result/4]).

-export([map_action_values/1, action_value1/2]).


-include("gmf.hrl").
-include("comTypes.hrl").


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-define(GMFLOG, gmfLog:comte).
-define(THROW(_Err), throw({?MODULE, _Err})).

-define(SA_IMM_PARAM_ADMOP_ERROR, <<"SaImmAdminOperationError">>).
-define(ADM_OWNER_EXISTS, {error, admin_owner_already_exists}).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
getMoAttribute([Attr | ECIMPath], Tid, Ldn, {Cxp, Rev}, ImmH) ->
    case getMoAttributes([Attr], ECIMPath, Tid, Ldn, {Cxp, Rev}, ImmH) of
	[] ->
	    [];
	[Res] ->
	    Res;
	Error ->
	    Error
    end.

getMoAttributes(AttrNames, ECIMPath, Tid, Ldn, {Cxp, Rev}, ImmH) ->
    try
	?GMFLOG({?MODULE, ?FUNCTION_NAME,
		 {Cxp, Rev, ImmH},
		 [{attrnames, AttrNames},
		  {ecimpath, ECIMPath},
		  {tid, Tid},
		  {ldn, Ldn}]}),
	{ok, DnMap, MimValues} = gmfSALib:get_dn_map({ECIMPath, mim}),
	ObjDn = gmfSALib:get_imm_dn(DnMap, MimValues),

	Pattern = #gmfMimClass{key        = DnMap#gmfDnMap.mim_class,
			       imm_rdn    = '$1',
			       attributes = '$2',
			       _          = '_'},
	[[ImmRdn, Attrs]] = ets:match(gmfMimClass, Pattern),
	ImmAttrs = get_imm_attrs({ImmRdn, Attrs}, AttrNames),

	Res = get_imm_values(ImmH, ImmAttrs, ObjDn, {Cxp, Rev}, Tid),
	?GMFLOG({?MODULE, ?FUNCTION_NAME,
		 {Cxp, Rev, ImmH},
		 [{attrnames, AttrNames},
		  {ecimpath, ECIMPath},
		  {res, Res},
		  {tid, Tid}]}),
	Res
    catch
	throw:{?MODULE, ExecErr} ->
	    ExecErr
    end.

get_imm_attrs({ImmRdn, Attrs}, AttrNames) ->
    [get_imm_attr({Attrs, ImmRdn}, Attr) || Attr <- AttrNames].

get_imm_attr({Attrs, ImmRdn} = Rec, Attr) ->
    AttrL = binary_to_list(Attr),
    Props = proplists:get_value(AttrL, Attrs, []),

    check_imm_attr(Props, AttrL, Rec),

    AttrName = get_imm_attr_name(Props, AttrL, ImmRdn),

    {AttrName, Props}.

check_imm_attr([], AttrL, Rec) ->
    sysInitI:warning_msg("~p;~p~n"
			 "Attribute ~p does not exist~n"
			 "Rec: ~p~n",
			 [?MODULE, ?FUNCTION_NAME, AttrL, Rec]);
check_imm_attr(_, _, _) ->
    ok.

get_imm_attr_name(Props, AttrL, ImmRdn) ->
    IsKey = proplists:is_defined(key, Props),
    choose(IsKey, ImmRdn, AttrL).

get_imm_values(ImmH, ImmAttrs, ObjDn, {Cxp, Rev}, Tid) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev, ImmH},
	     [{immattrs, ImmAttrs},
	      {objdn, ObjDn},
	      {tid, Tid}]}),

    AttrList = [Attr || {Attr, _} <- ImmAttrs],
    Handle = {tid, Tid},
    case gmfImmOmI:accessor_get_2(Handle, ObjDn, AttrList) of
	{ok, Data} ->
	    attr_values(Data, ImmAttrs, []);
	{error, Err} ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     {Cxp, Rev, ImmH},
		     [{accessor_get_2, Err},
		      {attrlist, AttrList},
		      {objdn, ObjDn},
		      {tid, Tid}]}),
	    []
    end.

attr_values(_Data, [], Acc) ->
    lists:reverse(Acc);
attr_values(Data, [{Attr, Props}|NewImmAttrs], Acc) ->
    {value, Imm, NewData} = lists:keytake(list_to_binary(Attr), 1, Data),
    Value = attr_value(Imm, proplists:get_value(dataType, Props)),
    attr_values(NewData, NewImmAttrs, [Value|Acc]).

attr_value({_, _, Values}, {sequence, Type})  ->
    {BType, _} = gmfSALib:get_com_basic_type(Type),
    [get_comte_type(BType)|get_comte_values(BType, Values)];
attr_value({_, _, []}, Type) ->
    {BType, _} = gmfSALib:get_com_basic_type(Type),
    [get_comte_type(BType)];
attr_value({_, _, [Value]}, Type) ->
    {BType, _} = gmfSALib:get_com_basic_type(Type),
    [get_comte_type(BType)|get_comte_values(BType, [Value])].

get_comte_type({int8,      _, _}) -> ?INT8;
get_comte_type({int16,     _, _}) -> ?INT16;
get_comte_type({int32,     _, _}) -> ?INT32;
get_comte_type({int64,     _, _}) -> ?INT64;
get_comte_type({uint8,     _, _}) -> ?UINT8;
get_comte_type({uint16,    _, _}) -> ?UINT16;
get_comte_type({uint32,    _, _}) -> ?UINT32;
get_comte_type({uint64,    _, _}) -> ?UINT64;
get_comte_type({string,    _, _}) -> ?STRING;
get_comte_type({boolean,   _, _}) -> ?BOOL;
get_comte_type({moRef,     _, _}) -> ?REFERENCE;
get_comte_type({enumRef,   _, _}) -> ?ENUM;
get_comte_type({structRef, _, _}) -> ?STRUCT.

get_comte_values(_                               , [])     -> [];
get_comte_values({int8,               _,       _}, Values) -> Values;
get_comte_values({int16,              _,       _}, Values) -> Values;
get_comte_values({int32,              _,       _}, Values) -> Values;
get_comte_values({int64,              _,       _}, Values) -> Values;
get_comte_values({uint8,              _,       _}, Values) -> Values;
get_comte_values({uint16,             _,       _}, Values) -> Values;
get_comte_values({uint32,             _,       _}, Values) -> Values;
get_comte_values({uint64,             _,       _}, Values) -> Values;
get_comte_values({string,             _,       _}, Values) -> Values;
get_comte_values({boolean,            _,       _}, Values) ->
    [Value =/= 0 || Value <- Values];
get_comte_values({moRef,              _,       _}, Values) ->
    lists:filtermap(fun get_moref_value/1, Values);
get_comte_values({enumRef,            _,       _}, Values) -> Values;
get_comte_values({structRef, StructName, MomName}, Values) ->
    Attrs = gmfSALib:get_imm_attrs(StructName, MomName),
    StructValues = gmfSALib:struct_values(Values),
    [structref_attr(Data, Attrs, []) || Data <- StructValues].

get_moref_value(Dn) ->
    case gmfSALib:get_moref(Dn, external) of
	{ok, MimDn} ->
	    {true, MimDn};
	{error, _} ->
	    false
    end.

structref_attr([], RestAttrs, StructAttrs) ->
    C = lists:map(
	  fun({Attr, Props}) ->
		  DataType = proplists:get_value(dataType, Props),
		  {BType, _} = gmfSALib:get_com_basic_type(DataType),
		  ComValue = [get_comte_type(BType)],
		  ?STRUCT_MEMBER(list_to_binary(Attr), ComValue)
	  end, RestAttrs),
    lists:sort(StructAttrs ++ C);
structref_attr([{ImmAttr, ImmType, Value}|Rest], Attrs, StructAttrs) ->
    Attr = tolist(ImmAttr),
    {value, {Attr, Props}, NewAttrs} = lists:keytake(Attr, 1, Attrs),
    DataType = proplists:get_value(dataType, Props),
    ComValue = attr_value({ImmAttr, ImmType, Value}, DataType),
    StructAttr = ?STRUCT_MEMBER(list_to_binary(Attr), ComValue),
    NewStructAttrs = [StructAttr | StructAttrs],
    structref_attr(Rest, NewAttrs, NewStructAttrs).

tolist(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
tolist(Atom) when is_atom(Atom) ->
    atom_to_list(Atom).

%%% #--------------------------------------------------------------------------
%%%
%%% nextMo
%%%
%%% #--------------------------------------------------------------------------
nextMo([Class | ParentDn] = ECIMPath, undefined, Tid, _Ldn, {Cxp, Rev},
       ImmHandle) ->
    try
	?GMFLOG({?MODULE, ?FUNCTION_NAME,
		 {Cxp, Rev, ImmHandle},
		 [{class, Class},
		  {parentdn, ParentDn},
		  {tid, Tid}]}),
	{ok, DnMap, MimValues} =
	    gmfSALib:get_dn_map({[<<"1">>| ECIMPath], mim}),

	Pattern = #gmfMimClass{key        = DnMap#gmfDnMap.mim_class,
			       imm_ns     = '$1',
			       _          = '_'},
	[[ImmNs]] = ets:match(gmfMimClass, Pattern),
	ImmClassName = iolist_to_binary([ImmNs, Class]),

	ImmParentDn = gmfSALib:get_parent_imm_dn(DnMap, MimValues),

	case safs_imm_om:get_class_instances(ImmParentDn, ImmClassName) of
	    {ok, []} ->
		{ok, undefined};
	    {ok, [ObjDn|T]} ->
		EtsId = ets:new(?MODULE, []),
		Data = [{K}|| K<-T],
		ets:insert(EtsId, Data),

		ImmRdn = get_imm_rdn(DnMap),

		CurrKey = #{ets_table => EtsId,
			    imm_rdn   => ImmRdn,
			    prev_key  => undefined},
		nMo(CurrKey, ObjDn)
	end
    catch
 	throw:{?MODULE, ExecErr} ->
 	    ExecErr
    end;
nextMo(_ECIMPath, #{ets_table := EtsId,
		    prev_key  := PrevKey} = CurrKey,
       _Tid, _Ldn, {_Cxp, _Rev}, _IH) ->
    case nextMo1(EtsId, PrevKey) of
        '$end_of_table' ->
            ets:delete(EtsId),
            {ok, undefined};
        ObjDn ->
	    CurrKey1 = CurrKey#{prev_key => ObjDn},
	    nMo(CurrKey1, ObjDn)
    end.

nextMo1(EtsId, undefined) -> ets:first(EtsId);
nextMo1(EtsId, PrevKey)   -> ets:next(EtsId, PrevKey).

nMo(CurrKey, ObjDn) ->
    [ObjRdn|_] = binary:split(ObjDn, [<<",">>]),
    #{imm_rdn := ImmRdn} = CurrKey,
    [ImmRdn, Ins] = binary:split(ObjRdn, [<<"=">>]),
    {ok, {?STRING(Ins), CurrKey}}.

%%% #--------------------------------------------------------------------------
%%%
%%% createMo
%%%
%%% #--------------------------------------------------------------------------
createMo(ECIMPath, KeyName, KeyValue, TransId, _Ldn, {Cxp, Rev}, ImmH) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev, ImmH},
	     [{ecimpath, ECIMPath},
	      {keyname, KeyName},
	      {keyvalue, KeyValue},
	      {tid, TransId}]}),

    try
	RealECIMPath = [KeyValue | ECIMPath],
	{ok, DnMap, MimValues} = gmfSALib:get_dn_map({RealECIMPath, mim}),
	Pattern = #gmfMimClass{key        = DnMap#gmfDnMap.mim_class,
			       imm_rdn    = '$1',
			       attributes = '$2',
			       imm_ns     = '$3',
			       _          = '_'},
	[[ImmRdn, ClassAttrs, ImmNs]] = ets:match(gmfMimClass, Pattern),

	cMo({RealECIMPath, DnMap, MimValues, ImmRdn, ClassAttrs, ImmNs},
	    KeyName,
	    TransId,
	    {Cxp, Rev},
	    ImmH,
	    [])
    catch
	throw:{?MODULE, ?ADM_OWNER_EXISTS} ->
	    Dn = tl(ECIMPath),
	    admin_owner_exists(TransId, Dn);
	throw:{?MODULE, ExecErr} ->
	    ExecErr
    end.

createMo(ECIMPath, KeyName, KeyValue, InitAttrs, TransId, _Ldn, {Cxp, Rev},
	 ImmH) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev, ImmH},
	     [{ecimpath, ECIMPath},
	      {keyname, KeyName},
	      {keyvalue, KeyValue},
	      {initattrs, InitAttrs},
	      {tid, TransId}]}),

    try
	RealECIMPath = [KeyValue | ECIMPath],
	{ok, DnMap, MimValues} = gmfSALib:get_dn_map({RealECIMPath, mim}),
	Pattern = #gmfMimClass{key        = DnMap#gmfDnMap.mim_class,
			       imm_rdn    = '$1',
			       attributes = '$2',
			       imm_ns     = '$3',
			       _          = '_'},
	[[ImmRdn, ClassAttrs, ImmNs]] = ets:match(gmfMimClass, Pattern),

	ImmAttrValues = setMoAttr_prep(InitAttrs, ClassAttrs),

	cMo({RealECIMPath, DnMap, MimValues, ImmRdn, ClassAttrs, ImmNs},
	    KeyName,
	    TransId,
	    {Cxp, Rev},
	    ImmH,
	    ImmAttrValues)
    catch
	throw:{?MODULE, ?ADM_OWNER_EXISTS} ->
	    Dn = tl(ECIMPath),
	    admin_owner_exists(TransId, Dn);
	throw:{?MODULE, ExecErr} ->
	    ExecErr
    end.

cMo({ECIMPath, DnMap, MimValues, ImmRdn, Attrs, ImmNs},
    KeyName,
    TransId,
    {Cxp, Rev},
    ImmH,
    ImmAttrValues) ->

    [KeyValue, Class|_] = ECIMPath,

    ImmClassName = iolist_to_binary([ImmNs, Class]),

    ImmParentDn = binary_to_list(gmfSALib:get_parent_imm_dn(DnMap, MimValues)),
    ok = aos({tid, TransId}, [ImmParentDn], one),

    {_, Props} = lists:keyfind(binary_to_list(KeyName), 1, Attrs),
    {BasicType, _} = gmfSALib:get_com_basic_type(Props),
    ImmValues = [{ImmRdn, {element(1, BasicType), [KeyValue]}} | ImmAttrValues],

    EcimDn = to_ecim_dn(ECIMPath),
    ObjId = gmfTrService:get_object_id(EcimDn),
    ExtraValues = [{<<"RcsImmAttrObjId">>, {uint32, [ObjId]}},
		   {<<"RcsImmAttrEcimDn">>, {string, [EcimDn]}}],

    case gmfImmOmI:coc_s2({tid, TransId}, ImmClassName, ImmParentDn,
			  ImmValues, ExtraValues) of
	ok ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     {Cxp, Rev, ImmH},
		     [{class, Class},
		      {immclass, ImmClassName},
		      {immparentdn, ImmParentDn},
		      {immrdn, ImmRdn},
		      {keyname, KeyName},
		      {keyvalue, KeyValue},
		      {tid, TransId}]}),
	    ok;
	Err ->
	    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
		     {Cxp, Rev, ImmH},
		     [{coc_2, Err},
		      {class, Class},
		      {immclass, ImmClassName},
		      {immparentdn, ImmParentDn},
		      {immrdn, ImmRdn},
		      {keyname, KeyName},
		      {keyvalue, KeyValue},
		      {tid, TransId}]}),

	    Err
    end.

to_ecim_dn(Key) ->
    L = lists:reverse(Key),
    iolist_to_binary(ted(L)).

ted([N1, V1, N2, V2])   -> [N1, $=, V1, $,, N2, $=, V2];
ted([N, V])             -> [N, $=, V];
ted([N1, V1, N2, V2|T]) -> [[N1, $=, V1, $,, N2, $=, V2, $,]|ted(T)];
ted([N, V|T])           -> [[N, $=, V, $,]|ted(T)];
ted([])                 -> [].

%%% #--------------------------------------------------------------------------
%%%
%%% deleteMo
%%%
%%% #--------------------------------------------------------------------------
deleteMo(ECIMPath, TransId, _Ldn, {Cxp, Rev}, ImmH) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev, ImmH},
	     [{ecimpath, ECIMPath},
	      {tid, TransId}]}),

    try
	{ok, DnMap, MimValues} = gmfSALib:get_dn_map({ECIMPath, mim}),
	ImmDn = binary_to_list(gmfSALib:get_imm_dn(DnMap, MimValues)),
	ok = aos({tid, TransId}, [ImmDn], subtree),
	?GMFLOG({?MODULE, ?FUNCTION_NAME,
		 {Cxp, Rev, ImmH},
		 [{immdn, ImmDn}]}),
	Res = gmfImmOmI:cod({tid, TransId}, ImmDn),
	?GMFLOG({?MODULE, ?FUNCTION_NAME,
		 {Cxp, Rev, ImmH},
		 [{cod, Res},
		  {immdn, ImmDn},
		  {tid, TransId}]}),
	Res
    catch
	throw:{?MODULE, ?ADM_OWNER_EXISTS} ->
	    admin_owner_exists(TransId, ECIMPath);
	throw:{?MODULE, ExecErr} ->
	    ExecErr
    end.

%%% #--------------------------------------------------------------------------
%%%
%%% setMoAttribute
%%%
%%% #--------------------------------------------------------------------------
setMoAttribute(ECIMPath, Value, UserObj, TransId, Ldn, {Cxp, Rev}, ImmH) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev, ImmH},
	     [{ecimpath, ECIMPath},
	      {value, Value},
	      {userobj, UserObj},
	      {tid, TransId},
	      {ldn, Ldn}]}),

    [Attr|RealECIMPath] = ECIMPath,
    AttrNames = [{Attr, Value}],

    setMoAttributes(AttrNames, RealECIMPath, TransId, Ldn, {Cxp, Rev}, ImmH).

setMoAttributes(AttrNames, ECIMPath, TransId, Ldn, {Cxp, Rev}, ImmH) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev, ImmH},
	     [{attrnames, AttrNames},
	      {ecimpath, ECIMPath},
	      {tid, TransId},
	      {ldn, Ldn}]}),

    try
	{ok, DnMap, MimValues} = gmfSALib:get_dn_map({ECIMPath, mim}),
	Pattern = #gmfMimClass{key        = DnMap#gmfDnMap.mim_class,
			       attributes = '$1',
			       _          = '_'},
	[[ClassAttrs]] = ets:match(gmfMimClass, Pattern),

	ImmDn = binary_to_list(gmfSALib:get_imm_dn(DnMap, MimValues)),
	ok = aos({tid, TransId}, [ImmDn], one),
	ImmValues = setMoAttr_prep(AttrNames, ClassAttrs),
	setMoAttribute_rc({tid, TransId}, ImmDn, ImmValues)
    catch
	throw:{?MODULE, ?ADM_OWNER_EXISTS} ->
	    admin_owner_exists(TransId, ECIMPath);
	throw:{?MODULE, ExecErr} ->
	    ExecErr
    end.

setMoAttr_prep([], _) ->
    [];
setMoAttr_prep(AttrNames, ClassAttrs) ->
    lists:map(
      fun({Attr, ComValues}) ->
	      AttrName       = binary_to_list(Attr),
	      Props          = proplists:get_value(AttrName, ClassAttrs),
	      {BasicType, _} = gmfSALib:get_com_basic_type(Props),
	      sMA_prep(Attr, ComValues, BasicType)
      end, AttrNames).

sMA_prep(ImmAttr, undefined, BasicType) ->
    ImmType = get_imm_type(BasicType),
    {ImmAttr, {ImmType, []}};
sMA_prep(ImmAttr, ComValues, BasicType) when is_list(ComValues) ->
    ECIMValues = [ECIMValue || {_, ECIMValue} <- ComValues],
    {ImmType, ImmValues} = get_imm_type_val(ECIMValues, BasicType),
    {ImmAttr, {ImmType, ImmValues}};
sMA_prep(ImmAttr, ComValue, BasicType) ->
    sMA_prep(ImmAttr, [ComValue], BasicType).

setMoAttribute_rc(_, _, []) ->
    ok;
setMoAttribute_rc(Key, ImmDn, Values) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     Key,
	     [{immdn, ImmDn},
	      {value, Values},
	      {key, Key}]}),

    gmfImmOmI:com_2(Key, ImmDn, Values).

%% structVals towards IMM You can build this code more for nested structs
structvals(Attrs, ECIMValue) ->
    structvals(Attrs, ECIMValue, []).

structvals(_Attrs, [], StructVal) ->
    StructVal;
structvals(Attrs, [{_AttrBin, undefined}|Rest], Acc) ->
    structvals(Attrs, Rest, Acc);
structvals(Attrs, [{AttrBin, ComValues}|Rest], Acc) when is_list(ComValues) ->
    Attr  = binary_to_list(AttrBin),
    Props = proplists:get_value(Attr, Attrs),
    {BasicType, _} = gmfSALib:get_com_basic_type(Props),
    Values = [Value || {_, Value} <- ComValues],
    structvals(Attrs,
	       Rest,
	       [{AttrBin, get_imm_type_val(Values, BasicType)}|Acc]);
structvals(Attrs, [{AttrBin, ComValue}|Rest], Acc) ->
    structvals(Attrs, [{AttrBin, [ComValue]}|Rest], Acc).

%% Special handling only for structRef/boolean/moRef/sequence
get_imm_type_val(Values, {sequence, Type}) ->
    get_imm_type_val(Values, Type);
get_imm_type_val(Values, {moRef, _ClassName, _MomName}) ->
    ImmValues =
	lists:map(
	  fun(MimDn) ->
		  case gmfSALib:get_moref(MimDn, internal) of
		      {ok, Dn} ->
			  Dn;
		      {error, _} ->
			  Reason = [MimDn,
				    " does not correspond to a valid MO."],
			  throw(iolist_to_binary(Reason))
		  end
	  end, Values),
    {moRef, ImmValues};
get_imm_type_val(Values, {boolean, _, _}) ->
    ImmValues = [choose(Value, 1, 0) || Value <- Values],
    {boolean, ImmValues};
get_imm_type_val(Values, {structRef, StructName, MomName}) ->
    Key = {MomName, StructName},
    [Obj] = ets:lookup(gmfMimStruct, Key),

    Attributes = Obj#gmfMimStruct.attributes,
    ImmNs = Obj#gmfMimStruct.imm_ns,

    ImmStructName = ImmNs ++ StructName,
    StructVals = [structvals(Attributes, ECIMValue) || ECIMValue <- Values],

    ImmValues = gmfSALib:get_immCsStruct(ImmStructName, StructVals),

    {structRef, ImmValues};
get_imm_type_val(Values, {Type, _, _}) ->
    {Type, Values}.

get_imm_type({sequence, Type}) ->
    get_imm_type(Type);
get_imm_type({Type, _, _}) ->
    Type.

%%% #--------------------------------------------------------------------------
%%%
%%% prepare
%%%
%%% #--------------------------------------------------------------------------
prepare(_ECIMPath, _UserObject, TransId, _Ldn, {Cxp, Rev}, ImmH) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev, ImmH},
	     [{tid, TransId}]}),
    ok.

%%% #--------------------------------------------------------------------------
%%%
%%% commit
%%%
%%% #--------------------------------------------------------------------------
commit(_ECIMPath, _UserObject, TransId, _Ldn, {Cxp, Rev}, ImmH) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev, ImmH},
	     [{tid, TransId}]}),
    ok.

%%% #--------------------------------------------------------------------------
%%%
%%% commit
%%%
%%% #--------------------------------------------------------------------------
commit(TransId, _Ldn, {Cxp, Rev}, ImmH) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev, ImmH},
	     [{tid, TransId}]}),
    ok.

%%% #--------------------------------------------------------------------------
%%%
%%% finish
%%%
%%% #--------------------------------------------------------------------------
finish(_ECIMPath, _UserObject, TransId, _Ldn, {Cxp, Rev}, ImmH) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev, ImmH},
	     [{tid, TransId}]}),
    ok.

%%% #--------------------------------------------------------------------------
%%%
%%% action
%%%
%%% #--------------------------------------------------------------------------
action(Name, ReverseDn, NamedParams, TransId, Ldn, {Cxp, Rev}, ImmH) ->
    action([Name|ReverseDn], NamedParams, TransId, Ldn, {Cxp, Rev}, ImmH).

action([ActionBin|ECIMPath], Parameters, TransId, _Ldn, {Cxp, Rev}, ImmH) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev, ImmH},
	     [{tid, TransId},
	      {action, ActionBin, Parameters},
	      {ecimpath, ECIMPath}]}),

    try
	{ok, DnMap, MimValues} = gmfSALib:get_dn_map({ECIMPath, mim}),

	Action = binary_to_list(ActionBin),
	Pattern = #gmfMimClass{key     = DnMap#gmfDnMap.mim_class,
			       actions = '$1',
			       _       = '_'},
	[[Actions]] = ets:match(gmfMimClass, Pattern),

	ActionData = proplists:get_value(Action, Actions),

	ActParamsFun = fun(X) -> element(1, X) == parameter end,
	ActParams = lists:filter(ActParamsFun, ActionData),

	ImmParams = act_params([], Parameters, ActParams),

	AdmOpId = list_to_integer(proplists:get_value("admOpId", ActionData)),

	ImmDn = gmfSALib:get_imm_dn(DnMap, MimValues),
	ok = aos({tid, TransId}, [ImmDn], one),

	Inv = mnesia:dirty_update_counter(gmfVars, admin_invoc_id, 1),
	InvId = Inv band 16#FFFFFFFF,
	ets:insert(gmfAdminOpPid, {InvId, self()}),

	Res = gmfImmOmI:admin_operation_invoke_async_2({tid, TransId},
						       InvId,
						       ImmDn,
						       TransId, %% ContinuationID
						       AdmOpId,
						       ImmParams), %% Timeout
	ok  = gmfImmOmI:aor({tid, TransId}, [ImmDn], one),

	R = wait_for_result(Res, InvId, ActionData),
	ets:delete(gmfAdminOpPid, InvId),
	R
    catch
	throw:{?MODULE, ?ADM_OWNER_EXISTS} ->
	    admin_owner_exists(TransId, ECIMPath);
	throw:{?MODULE, ExecErr} ->
	    ExecErr
    end.

wait_for_result(ok, InvId, ActionData) ->
    receive
	{admin_operation_invoke_callback_o2, InvId, OperRetVal, Err, Params} ->
	    handle_aoic_o2({OperRetVal, Err, Params}, ActionData)
	    %%after How long should the timeout be in case if any
    end;
wait_for_result(Err, _InvId, _ActionData) ->
    Err.

handle_aoic_o2({OperRetVal, ok, Params}, ActionData) ->
    %% Handle ECIM Return o2 callbacks.
    action_value(OperRetVal, Params, ActionData);
handle_aoic_o2({_OperRetVal, Err, _Params}, _ActionData) ->
    Err.

action_result(InvId, OperRetVal, Err, Params) ->
    case gmfDb:lookup_value(gmfAdminOpPid, InvId, undefined) of
	undefined ->
	    io:format("Something is Wrong with result handling for InvId ~p~n",
		      [InvId]),
	    ok;
	Pid ->
	    Pid ! {admin_operation_invoke_callback_o2, InvId, OperRetVal, Err,
		   Params},
	    ok
    end.

act_params(Params, [], _) ->
    lists:reverse(Params);
act_params(Params, [{Name, {_, Value}}|ComParams], ActParams) ->
    %% The name of the parameter is included in the information from COM
    {value, {_, _, Props}, NewActParams} =
	lists:keytake(binary_to_list(Name), 2, ActParams),
    NewParam = actp(Name, Value, gmfSALib:get_com_basic_type(Props)),
    NewParams =	[NewParam | Params],
    act_params(NewParams, ComParams, NewActParams);
act_params(Params, [{_, Value}|ComParams], [{_, Name, Props}|NewActParams]) ->
    NewParam = actp(Name, Value, gmfSALib:get_com_basic_type(Props)),
    NewParams =	[NewParam | Params],
    act_params(NewParams, ComParams, NewActParams).

actp(Name, Value, {BasicType, _Type}) ->
    {AType, [ImmValue]} = get_imm_type_val([Value], BasicType),
    {Name, {AType, ImmValue}}.

action_value(ok, [], _) ->
    undefined;
action_value(ok, Values, ActionData) ->
    {returnType, _, ReturnType} = lists:keyfind(returnType, 1, ActionData),
    MappedValues = map_action_values(Values),
    DataType = proplists:get_value(dataType, ReturnType),
    action_value1(MappedValues, DataType);
action_value(OperRetVal, Values, _ActionData) ->
    case check_for_error_string(Values) of
	false ->
	    OperRetVal;
	{true, ErrorString} ->
	    {error, ErrorString}
    end.

map_action_values(Values) ->
    L = lists:foldl(fun mav0/2, orddict:new(), Values),
    mav1(L).

mav0({Name, _Type, Value}, Dict) ->
    {MemberName, IdxN, IdxM} = mav2(Name),
    V = mav3(MemberName, IdxN, IdxM, Value, Dict),
    orddict:store(IdxN, V, Dict).

mav1([{undefined, [{undefined, _} = V]}]) -> V;
mav1([{undefined, V}]) -> V;
mav1(L) -> lists:map(fun({_, [{undefined, _} = V]}) -> V; ({_, V}) -> V end, L).

-define(IS_DIGIT(X), X>=$0, X=<$9).
-define(IS_MEMBERNAME(X), X =/= <<>>).

mav2(<<"result">>) ->
    %% simple type with single value
    {undefined, undefined, undefined};
mav2(Name) ->
    case binary:split(Name, <<"_">>, [global]) of
	[<<"result">>, <<A,_/binary>> = IdxN] when ?IS_DIGIT(A) ->
	    %% simple type with multiple values
	    {undefined, IdxN, undefined};
	[<<"result">>, MemberName] when ?IS_MEMBERNAME(MemberName) ->
	    %% complex type with single value (member has single value)
	    {MemberName, undefined, undefined};
	[<<"result">>, <<A,_/binary>> = IdxN,
	 MemberName] when ?IS_DIGIT(A), ?IS_MEMBERNAME(MemberName) ->
	    %% complex type with multiple values (member has single value)
	    {MemberName, IdxN, undefined};
	[<<"result">>, MemberName,
	 <<A,_/binary>> = IdxM] when ?IS_DIGIT(A), ?IS_MEMBERNAME(MemberName) ->
	    %% complex type with single value (member has multiple values)
	    {MemberName, undefined, IdxM};
	[<<"result">>, <<B,_/binary>> = IdxN, MemberName,
	 <<A,_/binary>> = IdxM] when ?IS_DIGIT(A), ?IS_DIGIT(B),
				     ?IS_MEMBERNAME(MemberName) ->
	    %% complex type with multiple values (member has multiple values)
	    {MemberName, IdxN, IdxM};
	_ ->
	    %% Handle a name not matching the spec as a simple type with single
	    %% value
	    {undefined, undefined, undefined}
    end.

mav3(MemberName, IdxN, IdxM, Value, Dict) ->
    R = orddict:find(IdxN, Dict),
    mav4(R, MemberName, IdxM, Value).

mav4(error, MemberName, undefined, Value) ->
    orddict:store(MemberName, Value, orddict:new());
mav4(error, MemberName, _IdxM, Value) ->
    orddict:store(MemberName, [Value], orddict:new());
mav4({ok, Dict}, MemberName, IdxM, Value) ->
    case orddict:find(MemberName, Dict) of
	error when IdxM == undefined ->
	    orddict:store(MemberName, Value, Dict);
	error ->
	    orddict:store(MemberName, [Value], Dict);
	{ok, _} ->
	    orddict:append(MemberName, Value, Dict)
    end.

action_value1({undefined, Value}, DataType) ->
    action_value1(Value, DataType);
action_value1(Values, {sequence, DataType}) ->
    action_value1(Values, DataType);
action_value1(Values, {structRef, StructName, MomName}) ->
    Key = {MomName, StructName},
    [Obj] = ets:lookup(gmfMimStruct, Key),
    Attrs = Obj#gmfMimStruct.attributes,
    action_value_struct(Values, Attrs, []);
action_value1(Values, {derivedDataTypeRef, DType, MomName}) ->
    Key = {MomName, DType},
    [Rec] = ets:lookup(gmfMimDerivedType, Key),
    DataType = proplists:get_value(dataType, Rec#gmfMimDerivedType.basetype),
    action_value1(Values, DataType);
action_value1(Values, DataType) when is_list(Values) ->
    [action_value1(Value, DataType) || Value <- Values];
action_value1(Value, {Type, _, _}) ->
    gmfSALib:get_com_attr_value(Type, Value).

action_value_struct([], _Attrs,  StructAttrs) ->
    ?STRUCT(StructAttrs);
action_value_struct([{ImmAttr, Values}|Rest], Attrs, StructAttrs) ->
    Attr = tolist(ImmAttr),
    Props = proplists:get_value(Attr, Attrs),
    DataType = proplists:get_value(dataType, Props),
    ComValue = action_value1(Values, DataType),
    StructAttr = ?STRUCT_MEMBER(ImmAttr, ComValue),
    NewStructAttrs = [StructAttr | StructAttrs],
    action_value_struct(Rest, Attrs, NewStructAttrs);
action_value_struct(L, Attrs, StructAttrs) when is_list(L) ->
    [action_value_struct(S, Attrs, StructAttrs) || S <- L].

check_for_error_string([]) ->
    false;
check_for_error_string([{?SA_IMM_PARAM_ADMOP_ERROR,
			 sa_imm_attr_sastringt,
			 <<"@ComNbi@", ErrorString/binary>>}|_]) ->
    {true, ErrorString};
check_for_error_string([{?SA_IMM_PARAM_ADMOP_ERROR,
			 sa_imm_attr_sastringt,
			 _}|_]) ->
    false;
check_for_error_string([_|R]) ->
    check_for_error_string(R).

%% #--------------------------------------------------------------------------
%%%
%%% existsMo
%%%
%%% #--------------------------------------------------------------------------
existsMo(ECIMPath, Tid, Ldn, {Cxp, Rev}, ImmH) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev, ImmH},
	     [{ecimpath, ECIMPath},
	      {tid, Tid},
	      {ldn, Ldn}]}),

    try
	{ok, DnMap, MimValues} = gmfSALib:get_dn_map({ECIMPath, mim}),

	ImmRdn = binary_to_list(get_imm_rdn(DnMap)),
	ObjDn = gmfSALib:get_imm_dn(DnMap, MimValues),
	Res =
	    case gmfImmOmI:accessor_get_2({tid, Tid}, ObjDn, [ImmRdn]) of
		{ok, _} ->
		    true;
		_ ->
		    false
	    end,

	?GMFLOG({?MODULE, ?FUNCTION_NAME,
		 {Cxp, Rev, ImmH},
		 [{ecimpath, ECIMPath},
		  {res, Res},
		  {tid, Tid}]}),

	Res
    catch
	throw:{?MODULE, ExecErr} ->
	    ExecErr
    end.

%%% #--------------------------------------------------------------------------
%%%
%%% countMoChildren
%%%
%%% #--------------------------------------------------------------------------
countMoChildren([], Class, TransId, Ldn, {Cxp, Rev}, ImmH) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev, ImmH},
	     [{class, Class},
	      {parentdn, []},
	      {tid, TransId},
	      {ldn, Ldn}]}),

    try
	%% Fixme
	Res = 0,
	%% _ClassName = binary_to_list(Class),
	Res
    catch
	throw:{?MODULE, ExecErr} ->
	    ExecErr
    end;
countMoChildren(ParentDn, Class, TransId, Ldn, {Cxp, Rev}, ImmH) ->
    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
	     {Cxp, Rev, ImmH},
	     [{class, Class},
	      {parentdn, ParentDn},
	      {tid, TransId},
	      {ldn, Ldn}]}),

    try
	ECIMPath = [Class | ParentDn],
	{ok, DnMap, MimValues} =
	    gmfSALib:get_dn_map({[<<"1">>| ECIMPath], mim}),

	ImmRdn = binary_to_list(get_imm_rdn(DnMap)),
	ImmParentDn =
	    binary_to_list(gmfSALib:get_parent_imm_dn(DnMap, MimValues)),

	Res =
	    case gmfImmOmI:search_initialize_2(ImmH, ImmParentDn,
					       sa_imm_sublevel,
					       16#0200) of
		{error, Err} ->
		    ?GMFLOG({?MODULE, ?FUNCTION_NAME,
			     {Cxp, Rev, ImmH},
			     [{search_init, Err},
			      {tid, TransId}]}),
		    0;
		{ok, Handle} ->
		    cMC(Handle, ImmRdn)
	    end,

	?GMFLOG({?MODULE, ?FUNCTION_NAME,
		 {Cxp, Rev, ImmH},
		 [{res, Res},
		  {tid, TransId}]}),

	Res
    catch
	throw:{?MODULE, ExecErr} ->
	    ExecErr
    end.

cMC(Handle, ImmRdn) ->
    cMC(Handle, ImmRdn, gmfImmOmI:search_next_2(Handle), 0).

cMC(_Handle, _ImmRdn, {error, _Reason}, Count) ->
    Count;
cMC(Handle, ImmRdn, {ok, ObjDn, _}, Count) ->
    [ObjRdn|_] = string:tokens(ObjDn, ","),
    NewCount =
	case string:tokens(ObjRdn, "=") of
	    [ImmRdn|_] ->
		Count+1;
	    _ ->
		Count
	end,
    cMC(Handle, ImmRdn, gmfImmOmI:search_next_2(Handle), NewCount).

%%% #--------------------------------------------------------------------------
%%%
%%% admin owner handling
%%%
%%% #--------------------------------------------------------------------------
aos(_Key, [""], _Scope) ->
    ok;
aos(Key, ObjectNames, Scope) ->
    case gmfImmOmI:aos(Key, ObjectNames, Scope) of
	ok ->
	    ok;
	{error, <<"sa_ais_err_not_exist">>} ->
	    %% This case should only normally occur in setMoAttributes when
	    %% the MO instance is created in the same CCB and in that case
	    %% this is not a real error, since the admin owner is set at the
	    %% creation of the MO instance.
	    %% If this is a real error (The MO instance does not exist),
	    %% the next IMM request using that MO instance will fail.
	    ok;
	{error, <<"sa_ais_err_exist">>} ->
	    ?THROW(?ADM_OWNER_EXISTS);
	Res ->
	    ?THROW(Res)
    end.

admin_owner_exists(TransId, Dn) ->
    EcimDn = to_ecim_dn(Dn),
    ErrorString = ["Object ", EcimDn,
		   " is currently locked by another database transaction."],
    gmfImmOmI:cses({tid, TransId}, ErrorString),
    {error, <<"sa_ais_err_exist">>}.

get_imm_rdn(DnMap) ->
    %% The first element in the list of imm_names is the imm rdn
    hd(DnMap#gmfDnMap.imm_names).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

choose(true,  A, _) -> A;
choose(false, _, B) -> B.

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
