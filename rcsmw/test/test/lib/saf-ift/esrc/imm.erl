%%% ----------------------------------------------------------
%%% %CCaseFile:	imm.erl %
%%% Author:	erarafo
%%% Description: Erlang functions that uses rct_proxy to perform
%%% selected IMM OM and IMM OI operations using the C interface.
%%%
%%% Modules used: rct_proxy
%%%
%%% Modules using this one: comsa_avc_SUITE
%%%
%%% ----------------------------------------------------------
-module(imm).
-id('Updated by CCase').
-vsn('/main/R3A/R5A/R6A/6').
-date('2016-06-19').
-author('erarafo').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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
%%% REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R3A/1      2015-03-13 erarafo     Work in progress; not ready for use
%%% R3A/2      2015-03-23 erarafo     Missing functions added
%%% R5A/1      2016-04-19 erarafo     Refined types, functions added
%%% R5A/2      2016-04-19 erarafo     Cleanup
%%% R6A/1      2016-05-25 erarafo     Added createConfigInst/6
%%% R6A/2      2016-06-09 erarafo     Adjustments
%%% R6A/3      2016-06-10 erarafo     Performance
%%% R6A/4      2016-06-10 erarafo     Better diagnostics
%%% R6A/6      2016-06-19 erarafo     Support for config object modification
%%% ----------------------------------------------------------

-export([omInitialize/1,
	 omFinalize/2,
	 omAoInitialize/4,
	 omAoFinalize/2,
	 omAoSet/4,
	 omAoRelease/4,
	 omCcbInitialize/3,
	 omCcbFinalize/2,
	 omCcbObjDelete/3,
	 omCcbObjCreate/5,
	 omCcbObjsCreate/3,
	 omCcbObjModify/4,
	 omCcbApply/2,
	 omAccessorInitialize/2,
	 omAccessorFinalize/2,
	 omAccessorGet/4,
	 oiInitialize/1,
	 oiFinalize/2,
	 oiImplementerSet/3,
	 oiImplementerClear/2,
	 oiObjCreate/5,
	 oiObjDelete/3,
	 oiRtObjectUpdate/4,
	 createConfigInst/5,
	 createConfigInst/6,
	 createConfigInsts/3,
	 deleteConfigInst/2]).

-include("imm.hrl").




%%% ----------------------------------------------------------
%%% @doc Returns a handle for IMM OM operations.
%%% @end
%%% ----------------------------------------------------------
-spec omInitialize(testContext()) -> {ok, {immHandle(), immVersion()}} | {error, any()}.

omInitialize(#testContext{node=Node, child=Child}) ->
    case
    rct_proxy:send_proxy(
	   Node,
	   Child,
	   ?UNUSED_ARG,
	   {omInitialize, {}}) of
	{error, _}=E ->
	    E;
	{ok, {ImmHandleS, {A, B, C}}} ->
	    {ok, {list_to_integer(ImmHandleS), {[A], B, C}}}
    end.

%%% ----------------------------------------------------------
%%% @doc Operations associated with the given IMM OM handle
%%% are finalized.
%%% @end
%%% ----------------------------------------------------------
-spec omFinalize(testContext(), immHandle()) -> ok | {error, any()}.

omFinalize(#testContext{node=Node, child=Child}, ImmHandle) ->
    case rct_proxy:send_proxy(Node, Child, ?UNUSED_ARG,
	   {omFinalize, {encodeUint64(ImmHandle)}}) of
	{error, _}=E ->
	    E;
	{ok} ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc
%%%
%%% If the 'Release' argument is true then the administrative
%%% ownership is cleared when the AO handle is finalized.
%%% @end
%%% ----------------------------------------------------------
-spec omAoInitialize(testContext(), immHandle(), string(), boolean()) ->
	  {ok, aoHandle()} | {error, any()}.

omAoInitialize(#testContext{node=Node, child=Child}, ImmHandle, AoName, Release) ->
    case rct_proxy:send_proxy(
	   Node,
	   Child,
	   ?UNUSED_ARG,
	   {omAoInitialize,
	    {encodeUint64(ImmHandle),
	     AoName,
	     encodeBoolean(Release)}}) of
	{error, _}=E ->
	    E;
	{ok, HandleS} ->
	    {ok, list_to_integer(HandleS)}
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------
-spec omAoFinalize(testContext(), aoHandle()) ->
	  ok | {error, any()}.

omAoFinalize(#testContext{node=Node, child=Child}, AoHandle) ->
    case rct_proxy:send_proxy(
	   Node,
	   Child,
	   ?UNUSED_ARG,
	   {omAoFinalize,
	    {encodeUint64(AoHandle)}}) of
	{error, _}=E ->
	    E;
	{ok} ->
	    ok
    end.




%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------
-spec omAoSet(testContext(), aoHandle(), [string()], integer()) ->
	  ok | {error, any()}.

omAoSet(#testContext{node=Node, child=Child}, AoHandle, Names, Scope) ->
    try 
	rct_proxy:send_proxy(
	  Node,
	  Child,
	  ?UNUSED_ARG,
	  {omAoSet,
	   {encodeUint64(AoHandle),
	    list_to_tuple([validateName(Name) || Name <- Names]),
	    Scope}}) of
	{error, _}=E ->
	    E;
	{ok} ->
	    ok
    catch
	throw:X ->
	    {error, X}
    end.


%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------
-spec omAoRelease(testContext(), aoHandle(), [string()], integer()) ->
	  ok | {error, any()}.

omAoRelease(#testContext{node=Node, child=Child}, AoHandle, Names, Scope) ->
    try rct_proxy:send_proxy(
	  Node,
	  Child,
	  ?UNUSED_ARG,
	  {omAoRelease,
	   {encodeUint64(AoHandle),
	    list_to_tuple([validateName(Name) || Name <- Names]),
	    Scope}}) of
	{error, _}=E ->
	    E;
	{ok} ->
	    ok
    catch
	throw:X ->
	    {error, X}
    end.



%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------
-spec omCcbInitialize(testContext(), aoHandle(), integer()) ->
	  {ok, ccbHandle()} | {error, any()}.

omCcbInitialize(#testContext{node=Node, child=Child}, AoHandle, CcbFlags) ->

    case rct_proxy:send_proxy(
      Node,
      Child,
      ?UNUSED_ARG,
      {omCcbInitialize,
       {encodeUint64(AoHandle),
	CcbFlags}}) of
	{error, _}=E ->
	    E;
	{ok, HandleS} ->
	    {ok, list_to_integer(HandleS)}
    end.


%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------
-spec omCcbFinalize(testContext(), ccbHandle()) ->
	  ok | {error, any()}.

omCcbFinalize(#testContext{node=Node, child=Child}, CcbHandle) ->
    case rct_proxy:send_proxy(
	   Node,
	   Child,
	   ?UNUSED_ARG,
	   {omCcbFinalize,
	    {encodeUint64(CcbHandle)}}) of
	{error, _}=E ->
	    E;
	{ok} ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------
-spec omCcbObjDelete(testContext(), ccbHandle(), string()) ->
	  ok | {error, any()}.

omCcbObjDelete(#testContext{node=Node, child=Child}, CcbHandle, Name) ->
    try rct_proxy:send_proxy(
	  Node,
	  Child,
	  ?UNUSED_ARG,
	  {omCcbObjDelete,
	   {encodeUint64(CcbHandle),
	    validateName(Name)}}) of
	{error, _}=E ->
	    E;
	{ok} ->
	    ok
    catch
	throw:X ->
	    {error, X}
    end.


%%% ----------------------------------------------------------
%%% @doc Create an instance in the given CCB.
%%% @end
%%% ----------------------------------------------------------
-spec omCcbObjCreate(testContext(), ccbHandle(), className(), dn(), [#attrValues{}]) ->
	  ok | {error, any()}.

omCcbObjCreate(#testContext{node=Node, child=Child},
	       CcbHandle,
	       ClassName,
	       ParentName,
	       AttrBindings) ->
    try
     rct_proxy:send_proxy(
	   Node,
	   Child,
	   ?UNUSED_ARG,
	   {omCcbObjCreate,
	    {encodeUint64(CcbHandle),
	     ClassName,
	     validateName(ParentName),
	     encodeAttrBindings(AttrBindings)}}) of
	{ok} ->
	    ok;
	Any ->
	    Any
    catch
	throw:X ->
	    {error, X}
    end.


%%% ----------------------------------------------------------
%%% @doc Create instances in the given CCB.
%%% @end
%%% ----------------------------------------------------------
-spec omCcbObjsCreate(testContext(), ccbHandle(), [#instDescr{}]) ->
	  ok | {error, any()}.

omCcbObjsCreate(#testContext{node=Node, child=Child},
		CcbHandle,
		InstDescrs) ->
    try
     rct_proxy:send_proxy(
	   Node,
	   Child,
	   ?UNUSED_ARG,
	   {omCcbObjsCreate,
	    {encodeUint64(CcbHandle),
	     encodeInsts(InstDescrs)}}) of
	{ok} ->
	    ok;
	Any ->
	    Any
    catch
	throw:X ->
	    {error, X}
    end.


-spec encodeInsts([#instDescr{}]) -> tuple().

encodeInsts(InstDescrs) ->
    list_to_tuple(
      lists:map(
	fun(#instDescr{className=ClassName, parentName=ParentName, attrValues=AttrValues}) ->
		{ClassName, 
		 validateName(ParentName), 
		 encodeAttrBindings(AttrValues)}
	end,
	InstDescrs)).


%%% ----------------------------------------------------------
%%% @doc Modify the given instance.
%%% @end
%%% ----------------------------------------------------------
-spec omCcbObjModify(#testContext{}, ccbHandle(), string(), [#attrMod{}]) ->
	  ok | {error, any()}.

omCcbObjModify(#testContext{node=Node, child=Child}, CcbHandle, Name, AttrMods) ->
    try
	rct_proxy:send_proxy(
	  Node,
	  Child,
	  ?UNUSED_ARG,
	  {omCcbObjModify,
	   {encodeUint64(CcbHandle),
	    validateName(Name),
	    list_to_tuple(
	      lists:map(
		fun(AttrMod) ->
			encodeAttrModification(AttrMod)
		end,
		AttrMods))}}) of
	{ok} ->
	    ok;
	Any ->
	    Any
    catch
	throw:X ->
	    {error, X}
    end.


%%% ----------------------------------------------------------
%%% @doc Operations associated with the given IMM OM handle
%%% are finalized.
%%% @end
%%% ----------------------------------------------------------
-spec omCcbApply(testContext(), ccbHandle()) ->
	  ok | {error, any()}.

omCcbApply(#testContext{node=Node, child=Child}, CcbHandle) ->
    case rct_proxy:send_proxy(
	   Node,
	   Child,
	   ?UNUSED_ARG,
	   {omCcbApply,
	    {encodeUint64(CcbHandle)}}) of
	{error, _}=E ->
	    E;
	{ok} ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc Returns an OM accessor handle.
%%% @end
%%% ----------------------------------------------------------
-spec omAccessorInitialize(#testContext{}, immHandle()) ->
	  {ok, accHandle()} | {error, any()}.

omAccessorInitialize(#testContext{node=Node, child=Child}, ImmHandle) ->
    case rct_proxy:send_proxy(
	   Node, 
	   Child, 
	   ?UNUSED_ARG, 
	   {omAccessorInitialize,
	    {encodeUint64(ImmHandle)}}) of
    	{error, _}=E ->
	    E;
	{ok, HandleS} ->
	    {ok, list_to_integer(HandleS)}
    end.


%%% ----------------------------------------------------------
%%% @doc Finalizes an OM accessor handle.
%%% @end
%%% ----------------------------------------------------------
-spec omAccessorFinalize(#testContext{}, accHandle()) ->
	  ok | {error, any()}.

omAccessorFinalize(#testContext{node=Node, child=Child}, AccHandle) ->
    case rct_proxy:send_proxy(
	   Node, 
	   Child, 
	   ?UNUSED_ARG, 
	   {omAccessorFinalize,
	    {encodeUint64(AccHandle)}}) of
    	{error, _}=E ->
	    E;
	{ok} ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc Gets attributes of the given instance.
%%% @end
%%% ----------------------------------------------------------
-spec omAccessorGet(#testContext{}, accHandle(), dn(), [string()]) ->
	  {ok, [#attrValues{}]} | {error, any()}.

omAccessorGet(#testContext{node=Node, child=Child}, AccHandle, Dn, AttrNames) ->
    case rct_proxy:send_proxy(
	   Node, 
	   Child, 
	   ?UNUSED_ARG, 
	   {omAccessorGet,
	    {encodeUint64(AccHandle),
	     Dn,
	     list_to_tuple(AttrNames)}}) of
    	{error, _}=E ->
	    E;
	{ok, Results} ->
	    DecodedResults =
		lists:foldr(
		  fun({attrValues, NameS, TypeA, ValuesT}, Acc) ->
			  ValuesL =
			      lists:map(
				fun(X) ->
					if TypeA =:= int32 ->
					       list_to_integer(binary_to_list(X));
					   true ->
					       throw({cannot_decode, TypeA, X})
					end
				end,
				tuple_to_list(ValuesT)),
			  
			  DecodedAttrValues =
			      #attrValues{name=NameS, 
					  type=TypeA, 
					  values=ValuesL},
			  [DecodedAttrValues|Acc]
		  end,
		  [],
		  tuple_to_list(Results)),
	    {ok, DecodedResults}
    end.


%%% ----------------------------------------------------------
%%% @doc Returns a handle for IMM OI operations.
%%% @end
%%% ----------------------------------------------------------
-spec oiInitialize(testContext()) -> {ok, {oiHandle(), oiVersion()}} | {error, any()}.

oiInitialize(#testContext{node=Node, child=Child}) ->
    case rct_proxy:send_proxy(
	   Node,
	   Child,
	   ?UNUSED_ARG,
	   {oiInitialize, {}}) of
	{error, _}=E ->
	    E;
	{ok, {OiHandleS, {A, B, C}}} ->
	    {ok, {list_to_integer(OiHandleS), {[A], B, C}}}
    end.

%%% ----------------------------------------------------------
%%% @doc Operations associated with the given IMM OI handle
%%% are finalized.
%%% @end
%%% ----------------------------------------------------------
-spec oiFinalize(testContext(), oiHandle()) -> ok | {error, any()}.

oiFinalize(#testContext{node=Node, child=Child}, OiHandle) ->
    case rct_proxy:send_proxy(Node, Child, ?UNUSED_ARG,
	   {oiFinalize, {encodeUint64(OiHandle)}}) of
	{error, _}=E ->
	    E;
	{ok} ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------
-spec oiImplementerSet(testContext(), oiHandle(), string()) ->
	  ok | {error, any()}.

oiImplementerSet(#testContext{node=Node, child=Child}, OiHandle, ImplementerName) ->
    case rct_proxy:send_proxy(
	   Node,
	   Child,
	   ?UNUSED_ARG,
	   {oiImplementerSet,
	    {encodeUint64(OiHandle),
	     ImplementerName}}) of
	{error, _}=E ->
	    E;
	{ok} ->
	    ok
    end.

-spec oiImplementerClear(testContext(), oiHandle()) -> ok | {error, any()}.

oiImplementerClear(#testContext{node=Node, child=Child}, OiHandle) ->
    case rct_proxy:send_proxy(
	   Node,
	   Child,
	   ?UNUSED_ARG,
	   {oiImplementerClear, {encodeUint64(OiHandle)}}) of
	{error, _}=E ->
	    E;
	{ok} ->
	    ok
    end.



%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------
-spec oiObjCreate(testContext(), oiHandle(), className(), dn(), [#attrValues{}]) ->
	  ok | {error, any()}.

oiObjCreate(#testContext{node=Node, child=Child},
	       OiHandle,
	       ClassName,
	       ParentName,
	       AttrBindings) ->
    try
     rct_proxy:send_proxy(
	   Node,
	   Child,
	   ?UNUSED_ARG,
	   {oiRtObjectCreate,
	    {encodeUint64(OiHandle),
	     ClassName,
	     validateName(ParentName),
	     encodeAttrBindings(AttrBindings)}}) of
	{ok} ->
	    ok;
	Any ->
	    Any
    catch
	throw:X ->
	    {error, X}
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------
-spec oiObjDelete(testContext(), oiHandle(), string()) ->
	  ok | {error, any()}.

oiObjDelete(#testContext{node=Node, child=Child}, OiHandle, Name) ->
    try rct_proxy:send_proxy(
	  Node,
	  Child,
	  ?UNUSED_ARG,
	  {oiRtObjectDelete,
	   {encodeUint64(OiHandle),
	    validateName(Name)}}) of
	{error, _}=E ->
	    E;
	{ok} ->
	    ok
    catch
	throw:X ->
	    {error, X}
    end.

-spec oiRtObjectUpdate(#testContext{}, oiHandle(), dn(), [#attrMod{}]) -> ok | {error, any()}.

oiRtObjectUpdate(#testContext{node=Node, child=Child}, OiHandle, Object, AttrMods) ->
    {ModTypes, AttrValuesList} =
	lists:foldr(
	  fun(#attrMod{modType=T, modAttr=A}, {MM, AA}) ->
		  {[T]++MM, [A]++AA}
	  end,
	  {[], []},
	  AttrMods),
    case rct_proxy:send_proxy(
	   Node,
	   Child,
	   ?UNUSED_ARG,
	   {oiRtObjectUpdate,
	    {encodeUint64(OiHandle),
	     validateName(Object),
	     encodeAttrBindings(AttrValuesList),
	     list_to_tuple(ModTypes)}}) of
	{error, _}=E ->
	    E;
	{ok} ->
	    ok
    end.

%%% ----------------------------------------------------------
%%% @doc Creates one config instance with no attributes.
%%% @end
%%% ----------------------------------------------------------
-spec createConfigInst(#testContext{}, string(), string(), string(), string()) ->
	  ok.

createConfigInst(TestContext, Parent, Class, RdnName, RdnValue) ->
    createConfigInst(TestContext, Parent, Class, RdnName, RdnValue, []).


%%% ----------------------------------------------------------
%%% @doc Creates one config instance with the given list of
%%% #attrValues{} items.
%%% @end
%%% ----------------------------------------------------------
-spec createConfigInst(#testContext{}, string(), string(), string(), string(), [#attrValues{}]) ->
	  ok.
	
createConfigInst(TestContext, Parent, Class, RdnName, RdnValue, AttrValues) ->
    {ok, {ImmHandle, _OmVersion}} = imm:omInitialize(TestContext),
    {ok, AoHandle} = imm:omAoInitialize(TestContext, ImmHandle, "timhv", true),
    {ok, CcbHandle} = imm:omCcbInitialize(TestContext, AoHandle, ?SA_IMM_CCB_NO_FLAG),
    ok = imm:omAoSet(TestContext, AoHandle, [Parent], ?SA_IMM_ONE),
    ok = imm:omCcbObjCreate(
	   TestContext,
	   CcbHandle,
	   Class,
	   Parent,
	   [#attrValues{name=RdnName, type=string, values=[RdnValue]}|AttrValues]),
    ok = imm:omCcbApply(TestContext, CcbHandle),
    ok = imm:omAoRelease(TestContext, AoHandle, [Parent], ?SA_IMM_ONE),
    Object = RdnName++"="++RdnValue++","++Parent,
    ok = imm:omAoRelease(TestContext, AoHandle, [Object], ?SA_IMM_ONE),
    ok = imm:omCcbFinalize(TestContext, CcbHandle),
    ok = imm:omAoFinalize(TestContext, AoHandle),
    ok = imm:omFinalize(TestContext, ImmHandle).



%%% ----------------------------------------------------------
%%% @doc Creates config instances.
%%% @end
%%% ----------------------------------------------------------
-spec createConfigInsts(#testContext{}, [string()], [#instDescr{}]) ->
	  ok.
	
createConfigInsts(TestContext, ExternalParents, InstDescrs) ->
    {ok, {ImmHandle, _OmVersion}} = imm:omInitialize(TestContext),
    {ok, AoHandle} = imm:omAoInitialize(TestContext, ImmHandle, "timhv", true),
    {ok, CcbHandle} = imm:omCcbInitialize(TestContext, AoHandle, ?SA_IMM_CCB_NO_FLAG),    
    ok = imm:omAoSet(TestContext, AoHandle, ExternalParents, ?SA_IMM_SUBTREE),  % TODO, overkill?
    R = imm:omCcbObjsCreate(
	   TestContext,
	   CcbHandle,
	   InstDescrs),
    if
	R =/= ok ->
	   ct:fail("bad result from create: ~p", [R]);
       true -> ok
    end,
    ok = imm:omCcbApply(TestContext, CcbHandle),
    %% ok = imm:omAoRelease(TestContext, AoHandle, [Object], ?SA_IMM_ONE),  %% Needed? Correct? TODO
    ok = imm:omCcbFinalize(TestContext, CcbHandle),
    ok = imm:omAoFinalize(TestContext, AoHandle),
    ok = imm:omFinalize(TestContext, ImmHandle).


%%% ----------------------------------------------------------
%%% @doc Deletes one config instance with no attributes.
%%% @end
%%% ----------------------------------------------------------

deleteConfigInst(TestContext, Object) ->
    {ok, {ImmHandle, _}} = imm:omInitialize(TestContext),
    {ok, AoHandle} = imm:omAoInitialize(TestContext, ImmHandle, "ewistad", true),
    {ok, CcbHandle} = imm:omCcbInitialize(TestContext, AoHandle, ?SA_IMM_CCB_NO_FLAG),
    ok = imm:omAoSet(TestContext, AoHandle, [Object], ?SA_IMM_ONE),
    ok = imm:omCcbObjDelete(TestContext, CcbHandle, Object),
    ok = imm:omCcbApply(TestContext, CcbHandle),
    ok = imm:omCcbFinalize(TestContext, CcbHandle),
    ok = imm:omAoFinalize(TestContext, AoHandle),
    ok = imm:omFinalize(TestContext, ImmHandle).


%%% ----------------------------------------------------------
%%% @doc Encodes a boolean to a small integer.
%%% @end
%%% ----------------------------------------------------------
-spec encodeBoolean(boolean()) -> 0|1.

encodeBoolean(false) ->
    0;
encodeBoolean(true) ->
    1.

%%% ----------------------------------------------------------
%%% @doc Encodes a positive integer in the unsigned 64-bit
%%% integer range.
%%% @end
%%% ----------------------------------------------------------
-spec encodeUint64(immHandle()) ->  binary().

encodeUint64(K) when K >= 18446744073709551616 ->
    throw({integer_too_big, K});

encodeUint64(K) when K < 0 ->
    throw({integer_less_than_zero, K});

encodeUint64(K) ->
    list_to_binary(integer_to_list(K)).


%%% ----------------------------------------------------------
%%% @doc Raises an exception if the given string is oversized
%%% for conversion to an IMM 'name' item.
%%% @end
%%% ----------------------------------------------------------
-spec validateName(string()) -> string().

validateName(Name) when length(Name) > ?SA_MAX_NAME_LENGTH ->
    throw({name_too_long, Name});

validateName(Name) ->
    Name.


%%% ----------------------------------------------------------
%%% @doc Converts a type indicator from atom to IMM numeric.
%%% @end
%%% ----------------------------------------------------------
-spec encodeType(attrType()) -> attrTypeImm().

encodeType(int32) ->
    ?SA_IMM_ATTR_SAINT32T;

encodeType(string) ->
    ?SA_IMM_ATTR_SASTRINGT;

encodeType(name) ->
    ?SA_IMM_ATTR_SANAMET;

encodeType(struct) ->
    ?SA_IMM_ATTR_CSSTRUCTT.


%%% ----------------------------------------------------------
%%% @doc Encode a list of attribute bindings. The result is a
%%% tuple reflecting the given list. Each tuple element is a
%%% 3-tuple with attribute name, type and values.
%%% @end
%%% ----------------------------------------------------------
-spec encodeAttrBindings([#attrValues{}]) -> tuple().

encodeAttrBindings(Bindings) ->
    list_to_tuple(
      lists:foldl(
	fun(AttrValues, Acc) ->
		Acc++[encodeAttrBinding(AttrValues)]
	end,
	[],
	Bindings)).


%%% ----------------------------------------------------------
%%% @doc Encode a single attribute modification as a 2-tuple
%%% of modification type and an encoded attribute binding.
%%% @end
%%% ----------------------------------------------------------
-spec encodeAttrModification(#attrMod{}) -> {modType(), {string(), attrTypeImm(), tuple()}}.
	  
encodeAttrModification(#attrMod{modType=ModType, modAttr=AttrValues}) when
  ModType =:= 1 orelse
      ModType =:= 2 orelse
      ModType =:= 3 ->
    {ModType, encodeAttrBinding(AttrValues)};

encodeAttrModification(#attrMod{modType=X}) ->
    throw({unsupported_mod_type, X}).


%%% ----------------------------------------------------------
%%% @doc Encode a single attribute binding as a 3-tuple of
%%% attribute name, type and values
%%% @end
%%% ----------------------------------------------------------
-spec encodeAttrBinding(#attrValues{}) -> {string(), attrTypeImm(), tuple()}.

encodeAttrBinding(#attrValues{name=undefined}=X) ->
    throw({bad_attr_name, X});

encodeAttrBinding(#attrValues{name=Name, type=Type, values=Values}) when
  Type =:= 'int32' orelse				     
      Type =:= 'string' orelse				     
      Type =:= 'name' orelse 
      Type =:= 'struct' ->
    ImmType = encodeType(Type),
    {Name, ImmType, encodeValues(ImmType, Values)};

encodeAttrBinding(X) ->
    throw({unsupported_type, X}).


%%% ----------------------------------------------------------
%%% @doc Encode a list of values. Integers become binarized decimal.
%%% Strings and names are prefixed to avoid empty strings. Structs
%%% become 2-tuples of structclassname and members. The result is a
%%% list converted to tuple.
%%% @end
%%% ----------------------------------------------------------
-spec encodeValues(attrTypeImm(), [integer()|string()]) ->
	  tuple().

encodeValues(Type, Values) ->
    L = lists:foldr(
	  fun(V, A) when is_integer(V) ->
		  [list_to_binary(integer_to_list(V))|A];
	     (V, A) when Type =:= ?SA_IMM_ATTR_SASTRINGT ->
		  Prefixed = "."++V,
		  [Prefixed|A];
	     (V, A) when Type =:= ?SA_IMM_ATTR_SANAMET ->
		  Prefixed = "."++V,
		  [Prefixed|A];
	     (V, A) when Type =:= ?SA_IMM_ATTR_CSSTRUCTT ->
		  [encodeStruct(V)|A];
	     (V, _A) ->
		  throw({cannot_handle_type, {Type, V}})
	  end,
	  [],
	  Values),
    list_to_tuple(L).


%%% ----------------------------------------------------------
%%% @doc Encode a struct value as a 2-tuple of struct class
%%% name and a tuplified list of member bindings.
%%% @end
%%% ----------------------------------------------------------
-spec encodeStruct(#struct{}) ->
	  {string(), tuple()}.

encodeStruct(#struct{name=Name, members=Members}) ->
    {Name,
     list_to_tuple(
       lists:foldr(
	 fun(#memberValues{name=MemberName, type=Type, values=Values}, Acc) ->
		 ImmType = encodeType(Type),
		 [{MemberName, ImmType, encodeValues(ImmType, Values)}|Acc]
	 end,
	 [],
	 Members))
    }.
