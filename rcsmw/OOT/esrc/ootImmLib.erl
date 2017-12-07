%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ootImmLib.erl %
%%% Author:	eolaand
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
%%% @private
-module(ootImmLib).
-id('Updated by CCase').
-vsn('/main/R2A/R4A/R11A/2').
-date('2017-08-15').
-author('eolaand').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	ootImm.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R2A/1      2014-01-28 eolaand     Created
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([
         initialize_om/0,
         finalize_om/1,
         initialize_om_ah/0,
         finalize_om_ah/1,
         initialize_om_ao_ccb/0,
         initialize_om_ao_ccb/1,
         finalize_om_ao_ccb/1,
         initialize_ah/1,
         finalize_ah/1,
	 get_object/1,
	 get_object/2,
	 get_attr/2,
	 get_attr/3,
	 get_attrs/2,
	 get_attrs/3,
	 get_attr_val/2,
	 get_attr_val/3,
	 set_attr/2,
	 set_attr/3,
	 set_attrs/2,
	 set_attrs/3,
         initialize_oi/2,
         finalize_oi/1,
	 obj_impl_set/2,
	 obj_impl_rel/2,
	 class_impl_set/2,
	 class_impl_rel/2,
	 is_class_defined/1,
	 admin_owner_set/2,
	 create_imm_object/3,
	 create_imm_object/4,
	 create_imm_object_extra_attrs/4,
	 create_imm_object_extra_attrs/5,
	 modify_imm_object/3,
	 modify_imm_object_extra_attrs/3,
	 delete_imm_object/2,
	 build_attr_val/3,
	 build_attr_mods/1,
	 replace_attr_val/5,
	 store_attr_val/4,
	 remove_attr_val/2
        ]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%% -export([internal_function1/2]).

%%% ----------------------------------------------------------
%%% Include files
%%% ----------------------------------------------------------
-include("om.hrl").
-include("oot.hrl").

%%% ----------------------------------------------------------
%%% Macros
%%% ----------------------------------------------------------

-define(OAP_AO_NAME, <<"OotOap">>).

%%% ----------------------------------------------------------
%%% Records
%%% ----------------------------------------------------------


%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%%     initialize_om() -> {ok, Handle} | error().
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
initialize_om() ->
    {ok, OmHandle, _} = safs_imm_om:initialize(undefined, ?SAF_VSN),
    OmHandle.


finalize_om(OmHandle) ->
    ok = safs_imm_om:finalize(OmHandle).


initialize_om_ah() ->
    OH = initialize_om(),
    AH = initialize_ah(OH),
    {OH, AH}.


finalize_om_ah({OmHandle, AH}) ->
    finalize_ah(AH),
    finalize_om(OmHandle).


initialize_om_ao_ccb() ->
    initialize_om_ao_ccb(?OAP_AO_NAME).


initialize_om_ao_ccb(AOName) ->
    OH  = initialize_om(),
    AO  = initialize_ao(OH, AOName),
    CCB = initialize_ccb(AO),
    {OH, AO, CCB}.


finalize_om_ao_ccb({OH, AO, CCB}) ->
    finalize_ccb(CCB),
    finalize_ao(AO),
    finalize_om(OH).


initialize_ah(OH) ->
    {ok, AH} = safs_imm_om:accessor_initialize(OH),
    AH.


finalize_ah({_OmHandle, AH}) ->
    ok = finalize_ah(AH);

finalize_ah(AH) ->
    ok = safs_imm_om:accessor_finalize(AH).


initialize_ccb(AH) ->
    {ok, CCB} = safs_imm_om:ccb_initialize(AH, 0),
    CCB.


finalize_ccb(CCB) ->
    ok = safs_imm_om:ccb_finalize(CCB).


%% initialize_ao(OH) ->
%%     initialize_ao(OH, ?OAP_AO_NAME).


initialize_ao(OH, AOName) ->
    {ok, AO} = safs_imm_om:admin_owner_initialize(OH, AOName, true),
    AO.


finalize_ao(AO) ->
    ok = safs_imm_om:admin_owner_finalize(AO).


admin_owner_set(AOHandle, ObjectNames) ->
    safs_imm_om:admin_owner_set(AOHandle, ObjectNames, sa_imm_one).


create_imm_object(CcbHandle, ClassName, Attrs) ->
    create_imm_object(CcbHandle, ClassName, undefined, Attrs).


create_imm_object(CcbHandle, ClassName, ParentName, Attrs) ->
    safs_imm_om:ccb_object_create_2(CcbHandle,
				    ClassName,
				    ParentName,
				    Attrs).


create_imm_object_extra_attrs(CcbHandle, ClassName, Attrs, ExtraAttrs) ->
    create_imm_object_extra_attrs(CcbHandle, ClassName, undefined, Attrs, 
				  ExtraAttrs).


create_imm_object_extra_attrs(CcbHandle, ClassName, ParentName, Attrs, 
			      ExtraAttrs) ->
    safs_imm_om:ccb_object_create_s2(CcbHandle,
				     ClassName,
				     ParentName,
				     Attrs,
				     ExtraAttrs).


delete_imm_object(CcbHandle, ObjectName) ->
    safs_imm_om:ccb_object_delete(CcbHandle, ObjectName).
    

modify_imm_object(CcbHandle, ObjectName, AttrMods) ->
    safs_imm_om:ccb_object_modify_2(CcbHandle, ObjectName, AttrMods).


modify_imm_object_extra_attrs(CcbHandle, ObjectName, ExtraAttrMods) ->
    safs_imm_om:ccb_object_modify_extra_attrs_s2(CcbHandle, ObjectName,
						 ExtraAttrMods).

%%% ----------------------------------------------------------
%%%     get_object(DN) -> {ok, Obj} | error().
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
is_class_defined(Class) ->
    OM  = initialize_om(),
    Res = safs_imm_om:class_description_get_2(OM, to_bin(Class)),
    finalize_om(OM),
    case Res of
	{ok, _, _} -> true;
	_          -> false
    end.


get_object(ObjDN) ->
    H = initialize_om_ah(),
    Res = get_object(H, ObjDN),
    finalize_om_ah(H),
    Res.


get_object({_OH, AH}, ObjDN) ->
    get_object(AH, ObjDN);

get_object(AH, ObjDN) ->
    safs_imm_om:accessor_get_2(AH, ObjDN, []).


get_attr(ObjDN, AttrName) ->
    case get_attrs(ObjDN, [AttrName]) of
	{ok, [{_Key, Val}]} ->
	    {ok, Val};
	Error ->
	    Error
    end.


get_attr(OmH, ObjDN, AttrName) ->
    case get_attrs(OmH, ObjDN, [AttrName]) of
	{ok, [{_Key, Val}]} ->
	    {ok, Val};
	Error ->
	    Error
    end.


get_attrs(ObjDN, Attrs) ->
    H = initialize_om_ah(),
    Res = get_attrs(H, ObjDN, Attrs),
    finalize_om_ah(H),
    Res.


get_attrs({_OH, AH}, ObjDN, Attrs) ->
    get_attrs(AH, ObjDN, Attrs);


get_attrs(AH, ObjDN, Attrs) ->
    case safs_imm_om:accessor_get_2(AH, ObjDN, Attrs) of
	{ok, AttrVals} ->
	    Vals = [{Name, Val} || {Name, _Type, Val} <- AttrVals],
	    {ok, Vals};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% Function: set_attrs/2
%% Description:
%%--------------------------------------------------------------------
set_attr(DN, {Attr, Type, Vals}) ->
    set_attrs(DN, [{Attr, Type, Vals}]).


set_attr(Handle, DN, {Attr, Type, Vals}) ->
    set_attrs(Handle, DN, [{Attr, Type, Vals}]).


%%--------------------------------------------------------------------
%% Function: set_attrs/2
%% Description:
%%--------------------------------------------------------------------
set_attrs(DN, AttrVals) ->
    Handle = initialize_om_ao_ccb(),
    set_attrs(Handle, DN, AttrVals),
    finalize_om_ao_ccb(Handle).
    

set_attrs({_OmHandle, AOHandle, CcbHandle}, DN, AttrVals) ->
    ok = admin_owner_set(AOHandle, [DN]),
    set_attrs(CcbHandle, DN, AttrVals);

set_attrs(CcbHandle, DN, AttrVals) ->
    SetAttrVals = [build_attr_val(Attr, Type, Val) || 
		      {Attr, Type, Val} <- AttrVals],
    OapAttrMods = build_attr_mods(SetAttrVals),
    modify_imm_object(CcbHandle, DN, OapAttrMods).


%%% ----------------------------------------------------------
%%%     initialize_oi() -> {ok, Handle} | error().
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
initialize_oi(Mod, Name) ->
    {ok, OiH, _Vsn} = safs_imm_oi:initialize(Mod, ?SAF_VSN),
    ok = safs_imm_oi:implementer_set(OiH, to_bin(Name)),
    OiH.


finalize_oi(undefined) ->
    ok;

finalize_oi(OiH) ->
    safs_imm_oi:implementer_clear(OiH),
    safs_imm_oi:finalize(OiH).


obj_impl_set(OiH, ObjDN) ->
    safs_imm_oi:object_implementer_set(OiH, to_bin(ObjDN), ?ONE).


obj_impl_rel(OiH, ObjDN) ->
    safs_imm_oi:object_implementer_release(OiH, to_bin(ObjDN), ?ONE).


class_impl_set(OiH, Class) ->
    safs_imm_oi:class_implementer_set(OiH, to_bin(Class)).


class_impl_rel(OiH, Class) ->
    safs_imm_oi:class_implementer_release(OiH, to_bin(Class)).


%%% ----------------------------------------------------------
%%%     build_attr_val() -> #safsImmAttrValues_2{}
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
build_attr_val(Attr, Type, Val) ->
    AttrVal = to_attr_val(Val, Type),
    build_attr_val(Attr, Type, length(AttrVal), AttrVal).

	    
build_attr_val(Attr, Type, N, Vals) when Type =:= string;
					 Type =:= sastring;
					 Type =:= sa_imm_attr_sastringt ->
    AttrVals  = [#safsImmAttrValue{sastring = Val} || Val <- Vals],
    #safsImmAttrValues_2{attrName         = Attr,
			 attrValueType    = sa_imm_attr_sastringt,
			 attrValuesNumber = N,
			 attrValues       = AttrVals};

build_attr_val(Attr, Type, N, Vals) when Type =:= uint32;
					 Type =:= sauint32;
					 Type =:= sa_imm_attr_sauint32t ->
    AttrVals  = [#safsImmAttrValue{sauint32 = Val} || Val <- Vals],
    #safsImmAttrValues_2{attrName         = Attr,
			 attrValueType    = sa_imm_attr_sauint32t,
			 attrValuesNumber = N,
			 attrValues       = AttrVals};

build_attr_val(Attr, Type, N, Vals) when Type =:= name;
					 Type =:= saname;
					 Type =:= sa_imm_attr_sanamet ->
    AttrVals  = [#safsImmAttrValue{saname = Val} || Val <- Vals],
    #safsImmAttrValues_2{attrName         = Attr,
			 attrValueType    = sa_imm_attr_sanamet,
			 attrValuesNumber = N,
			 attrValues       = AttrVals}.


to_attr_val(Val, _Type) 
  when Val =:= undefined; 
       Val =:= [] ->
    [];    

to_attr_val(Val, _Type) when is_integer(Val) ->
    [Val];

to_attr_val([_|_] = Val, Type) when Type =:= uint32;
				    Type =:= sauint32;
				    Type =:= sa_imm_attr_sauint32t ->
    Val;

to_attr_val([First | _] = Vals, _Type) 
  when is_list(First); is_binary(First) ->
    [to_bin(Val) || Val <- Vals];

to_attr_val(Val, _Type) ->
    [to_bin(Val)].


%%% ----------------------------------------------------------
%%%     replace_attr_val() -> #safsImmAttrValues_2{}
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Replace OldVal with NewVal if OldVal is present 
%%% ----------------------------------------------------------
replace_attr_val(Attr, Type, OldVal, NewVal, AttrVals) ->
    KeyPos = #safsImmAttrValues_2.attrName,
    OldAttrVal = to_attr_val(OldVal, Type),
    case lists:keyfind(Attr, KeyPos, AttrVals) of
	#safsImmAttrValues_2{attrValues = [#safsImmAttrValue{saname = Val}]} 
	  when Val =:= OldAttrVal andalso (Type =:= name orelse 
					   Type =:= saname orelse
					   Type =:= sa_imm_attr_sanamet) ->
	    store_attr_val(Attr, Type, NewVal, AttrVals);

	#safsImmAttrValues_2{attrValues = [#safsImmAttrValue{sauint32 = Val}]} 
	  when Val =:= OldAttrVal andalso (Type =:= uint32 orelse 
					   Type =:= sauint32 orelse
					   Type =:= sa_imm_attr_sauint32t) ->
	    store_attr_val(Attr, Type, NewVal, AttrVals);
	_ ->
	    AttrVals
    end.


remove_attr_val(Attr, AttrVals) ->
    KeyPos = #safsImmAttrValues_2.attrName,
    lists:keydelete(Attr, KeyPos, AttrVals).


store_attr_val(Attr, Type, NewVal, AttrVals) ->
    AttrVal = build_attr_val(Attr, Type, NewVal),
    lists:keystore(Attr, #safsImmAttrValues_2.attrName, AttrVals, AttrVal).


get_attr_val(Attr, #safsImmAttrValue{saname = Val}) 
  when Attr =:= ?OAP_ACC_POINT;
       Attr =:= ?OAP_IPV4_ADDR ->
    Val;

get_attr_val(Attr, #safsImmAttrValue{sauint32 = Val}) 
  when Attr =:= ?OAP_SSH_PORT;
       Attr =:= ?OAP_NETCONF_PORT;
       Attr =:= ?OAP_DSCP;
       Attr =:= ?OAP_OBJ_ID ->
    Val;

get_attr_val(Attr, #safsImmAttrValue{sastring = Val}) 
  when Attr =:= ?OAP_ECIM_DN ->
    Val;

get_attr_val(Attr, [#safsImmAttrValue{} = AttrVal]) ->
    get_attr_val(Attr, AttrVal);

get_attr_val(_Attr, []) ->
    [].


get_attr_val(Attr, Type, AttrVals) ->
    case lists:keyfind(Attr, #safsImmAttrValues_2.attrName, AttrVals) of
	#safsImmAttrValues_2{attrValues = [#safsImmAttrValue{saname = Val}]} 
	  when Type =:= name;
	       Type =:= saname;
	       Type =:= sa_imm_attr_sanamet ->
	    Val;
	#safsImmAttrValues_2{attrValues = [#safsImmAttrValue{sauint32 = Val}]} 
	  when Type =:= uint32; 
	       Type =:= sauint32;
	       Type =:= sa_imm_attr_sauint32t ->
	    Val;
	#safsImmAttrValues_2{attrValues = [#safsImmAttrValue{sastring = Val}]} 
	  when Type =:= string;
	       Type =:= sastring;
	       Type =:= sa_imm_attr_sastringt ->
	    Val;
	_ ->
	    []
    end.
    

%%% ----------------------------------------------------------
%%%     build_attr_mods() -> [#safsImmAttrModification_2{}]
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
build_attr_mods(AttrVals) ->
    [#safsImmAttrModification_2{modType = sa_imm_attr_values_replace,
				modAttr = AttrVal} || AttrVal <- AttrVals].

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%%            to_bin(Name) -> binary().
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
to_bin(L) ->
    ootLib:to_bin(L).


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
