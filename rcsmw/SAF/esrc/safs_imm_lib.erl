%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016. All Rights Reserved.
%%
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%%
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_imm_lib.erl
%%
%% Description:
%%    IMM library functions
%%
%%--------------------------------------------------------------------
-module(safs_imm_lib).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("om.hrl").
-include("safs_imm_internal.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 check_if_root/1,
	 convert_attribute_name_to_binary/1,
	 mk_dnlist/1,
	 validate_version/1,
	 convert_attr_list/1,
	 convert_attrmod_list/1,
	 sort_struct_members/1,
	 set_attr_values/2,
	 set_attr_value/2,
	 get_attr_values/2,
	 get_attr_value/2,
	 get_notifiable_attrs/1,
	 ta/1,
	 tb/1
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: check_if_root/1
%% Description:
%%--------------------------------------------------------------------
check_if_root(undefined) ->
    <<>>;
check_if_root(ParentName)  when is_binary(ParentName) ->
    ParentName;
check_if_root(ParentName) when is_list(ParentName) ->
    ParentName.

%%--------------------------------------------------------------------
%% Function: convert_attribute_name_to_binary/1
%% Description:
%%--------------------------------------------------------------------
convert_attribute_name_to_binary(Name) when is_atom(Name) ->
    unicode:characters_to_binary(atom_to_list(Name), utf8);
convert_attribute_name_to_binary(Name) when is_binary(Name) ->
    Name;
convert_attribute_name_to_binary(Name) when is_list(Name) ->
    unicode:characters_to_binary(Name, utf8).

%%--------------------------------------------------------------------
%% Function: mk_dnlist/1
%% Description:
%%--------------------------------------------------------------------
mk_dnlist(Key) -> safs_imm_db:mk_key(Key).

%%--------------------------------------------------------------------
%% Function: validate_version/1
%% Description:
%%--------------------------------------------------------------------
validate_version(#safsVersion{releaseCode=?IMM_RELEASE_CODE,
			      majorVersion=?IMM_MAJOR_VERSION} = Version) ->
    {ok, Version#safsVersion{minorVersion=?IMM_MINOR_VERSION}};
validate_version(_Version) ->
    {error, sa_ais_err_version}.

%%--------------------------------------------------------------------
%% Function: convert_attr_list/1
%% Description:
%%--------------------------------------------------------------------
convert_attr_list(AttrValues) ->
    convert_attr_list(AttrValues, []).

%%--------------------------------------------------------------------
convert_attr_list([], Attrs) ->
    {ok, lists:reverse(Attrs)};
convert_attr_list([#safsImmAttrValues_2{} = Attr |Attrs], Acc) ->
    AttrName = Attr#safsImmAttrValues_2.attrName,
    {ok, AttrValues} = get_attr_values(Attr#safsImmAttrValues_2.attrValueType,
				       Attr#safsImmAttrValues_2.attrValues),
    AttrValues1 = 
	case Attr#safsImmAttrValues_2.attrValueType of %% LATH: Can perhaps be done in get_attr_values()
	    sa_imm_attr_csstructt ->
		sort_struct_members(AttrValues);
	    _ ->
		AttrValues
	end,
    convert_attr_list(Attrs, [{ta(AttrName), Attr#safsImmAttrValues_2.attrValueType, AttrValues1}| Acc]);
convert_attr_list([{AttrName, sa_imm_attr_csstructt, AttrValues} |Attrs], Acc) ->
    AttrValues1 = sort_struct_members(AttrValues),
    convert_attr_list(Attrs, [{ta(AttrName), sa_imm_attr_csstructt, AttrValues1}| Acc]);
convert_attr_list([{AttrName, Type, AttrValues} |Attrs], Acc) ->
    convert_attr_list(Attrs, [{ta(AttrName), Type, AttrValues}| Acc]).

%%--------------------------------------------------------------------
%% Function: convert_attrmod_list/1
%% Description:
%%--------------------------------------------------------------------
convert_attrmod_list(AttrMods) ->
    convert_attrmod_list(AttrMods, []).

%%--------------------------------------------------------------------
convert_attrmod_list([], Mods) ->
    {ok, lists:reverse(Mods)};
convert_attrmod_list([#safsImmAttrModification_2{} = Mod |Mods], Acc) ->
    ModType = Mod#safsImmAttrModification_2.modType,
    ModAttr = Mod#safsImmAttrModification_2.modAttr,

    AttrName = ModAttr#safsImmAttrValues_2.attrName,
    {ok, AttrValues} = get_attr_values(ModAttr#safsImmAttrValues_2.attrValueType,
				       ModAttr#safsImmAttrValues_2.attrValues),
    AttrValues1 = 
	case ModAttr#safsImmAttrValues_2.attrValueType of %% LATH: Can perhaps be done in get_attr_values()
	    sa_imm_attr_csstructt ->
		sort_struct_members(AttrValues);
	    _ ->
		AttrValues
	end,
    convert_attrmod_list(Mods, [{ModType, {ta(AttrName),
					   ModAttr#safsImmAttrValues_2.attrValueType,
					   AttrValues1}}| Acc]);
convert_attrmod_list([{ModType, {AttrName, sa_imm_attr_csstructt, AttrValues}} |Mods], Acc) ->
    AttrValues1 = sort_struct_members(AttrValues),
    convert_attrmod_list(Mods, [{ModType, {AttrName, sa_imm_attr_csstructt, AttrValues1}}| Acc]);
convert_attrmod_list([{ModType, {AttrName, Type, AttrValues}} |Mods], Acc) ->
    convert_attrmod_list(Mods, [{ModType, {AttrName, Type, AttrValues}}| Acc]).

%%--------------------------------------------------------------------
%% Function: sort_struct_members/2
%% Description:
%%--------------------------------------------------------------------
sort_struct_members(Values) ->
    sort_struct_members(Values, []).

sort_struct_members([], Acc) ->
    lists:reverse(Acc);
sort_struct_members([Value |Values], Acc) when is_record(Value, safsImmCsStruct) ->
    M = lists:keysort(2, Value#safsImmCsStruct.structMembers),
    sort_struct_members(Values, [Value#safsImmCsStruct{structMembers=M} |Acc]);
sort_struct_members([Value |Values], Acc) ->
    sort_struct_members(Values, [Value |Acc]).

%%--------------------------------------------------------------------
%% Function: set_attr_values/2
%% Description:
%%--------------------------------------------------------------------
set_attr_values(Type, AttrValues) ->
    set_attr_values(Type, AttrValues, []).

%%--------------------------------------------------------------------
set_attr_values(_Type, [], Acc) ->
    {ok, lists:reverse(Acc)};
% set_attr_values(sa_imm_attr_csstructt, AttrValues, _Acc) ->
%     {ok, AttrValues};
set_attr_values(Type, [AttrValue | AttrValues], Acc) ->
    Value = set_attr_value(Type, AttrValue),
    set_attr_values(Type, AttrValues, [Value |Acc]).

%%--------------------------------------------------------------------
set_attr_value(sa_imm_attr_saint32t, AttrValue) -> #safsImmAttrValue{saint32=AttrValue};
set_attr_value(sa_imm_attr_sauint32t, AttrValue) -> #safsImmAttrValue{sauint32=AttrValue};
set_attr_value(sa_imm_attr_saint64t, AttrValue) -> #safsImmAttrValue{saint64=AttrValue};
set_attr_value(sa_imm_attr_sauint64t, AttrValue) -> #safsImmAttrValue{sauint64=AttrValue};
set_attr_value(sa_imm_attr_satimet, AttrValue) -> #safsImmAttrValue{satime=AttrValue};
set_attr_value(sa_imm_attr_sanamet, AttrValue) -> #safsImmAttrValue{saname=AttrValue};
set_attr_value(sa_imm_attr_safloatt, AttrValue) -> #safsImmAttrValue{safloat=AttrValue};
set_attr_value(sa_imm_attr_sadoublet, AttrValue) -> #safsImmAttrValue{sadouble=AttrValue};
set_attr_value(sa_imm_attr_sastringt, AttrValue) -> #safsImmAttrValue{sastring=AttrValue};
set_attr_value(sa_imm_attr_saanyt, AttrValue) -> #safsImmAttrValue{saany=AttrValue};
set_attr_value(sa_imm_attr_csstructt, AttrValue) -> #safsImmAttrValue{csstruct=AttrValue}.

%%--------------------------------------------------------------------
%% Function: get_attr_values/2
%% Description:
%%--------------------------------------------------------------------
get_attr_values(Type, AttrValues) ->
    get_attr_values(Type, AttrValues, []).

%%--------------------------------------------------------------------
get_attr_values(_Type, [], Acc) ->
    {ok, lists:reverse(Acc)};
% get_attr_values(sa_imm_attr_csstructt, AttrValues, _Acc) ->
%     {ok, AttrValues};
get_attr_values(Type, [AttrValue | AttrValues], Acc) ->
    Value = get_attr_value(Type, AttrValue),
    get_attr_values(Type, AttrValues, [Value |Acc]).

%%--------------------------------------------------------------------
get_attr_value(sa_imm_attr_saint32t, AttrValue) -> AttrValue#safsImmAttrValue.saint32;
get_attr_value(sa_imm_attr_sauint32t, AttrValue) -> AttrValue#safsImmAttrValue.sauint32;
get_attr_value(sa_imm_attr_saint64t, AttrValue) -> AttrValue#safsImmAttrValue.saint64;
get_attr_value(sa_imm_attr_sauint64t, AttrValue) -> AttrValue#safsImmAttrValue.sauint64;
get_attr_value(sa_imm_attr_satimet, AttrValue) -> AttrValue#safsImmAttrValue.satime;
get_attr_value(sa_imm_attr_sanamet, AttrValue) -> AttrValue#safsImmAttrValue.saname;
get_attr_value(sa_imm_attr_safloatt, AttrValue) -> AttrValue#safsImmAttrValue.safloat;
get_attr_value(sa_imm_attr_sadoublet, AttrValue) -> AttrValue#safsImmAttrValue.sadouble;
get_attr_value(sa_imm_attr_sastringt, AttrValue) -> AttrValue#safsImmAttrValue.sastring;
get_attr_value(sa_imm_attr_saanyt, AttrValue) -> AttrValue#safsImmAttrValue.saany;
get_attr_value(sa_imm_attr_csstructt, AttrValue) -> AttrValue#safsImmAttrValue.csstruct.

%%--------------------------------------------------------------------
%% Function: get_notifiable_attrs/1
%% Description:
%%--------------------------------------------------------------------
get_notifiable_attrs(AttrDefs) ->
    NotifAttrs = lists:filtermap(fun({Name, _Type, Category, Flags, _Default}) ->
					 case get_notifiable(Flags) of
					     true ->
						 {true, {Name, Category, Flags}};
					     false ->
				    false
					 end
				 end, AttrDefs),
    case NotifAttrs of
	[{_Name, _Category, Flags}] ->
	    case lists:member(notify, Flags) of
		true ->
		    NotifAttrs;
		false ->
		    []
	    end;
	_ ->
	    NotifAttrs
    end.

get_notifiable([]) ->
    false;
get_notifiable([notify |_]) ->
    true;
get_notifiable([rdn |_]) ->
    true;
get_notifiable([_ |Flags]) ->
    get_notifiable(Flags).

%%--------------------------------------------------------------------
%% Function: ta/1
%% Description:
%%--------------------------------------------------------------------
ta(B) when is_binary(B) ->
    erlang:binary_to_atom(B, utf8);
ta(L) when is_list(L) ->
    list_to_atom(L);
ta(A) when is_atom(A) ->
    A.

%%--------------------------------------------------------------------
%% Function: tb/1
%% Description:
%%--------------------------------------------------------------------
tb(Bin) when is_binary(Bin) ->
    Bin;
tb(L) when is_list(L) ->
    list_to_binary(L);
tb(A) when is_atom(A) ->
    erlang:atom_to_binary(A, utf8).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
% convert_dbtype_to_satype("SA_INT32_T") ->
%     sa_imm_attr_saint32t;
% convert_dbtype_to_satype("SA_UINT32_T") ->
%     sa_imm_attr_sauint32t;
% convert_dbtype_to_satype("SA_INT64_T") ->
%     sa_imm_attr_saint64t;
% convert_dbtype_to_satype("SA_UINT64_T") ->
%     sa_imm_attr_sauint64t;
% convert_dbtype_to_satype("SA_TIME_T") ->
%     sa_imm_attr_satimet;
% convert_dbtype_to_satype("SA_NAME_T") ->
%     sa_imm_attr_sanamet;
% convert_dbtype_to_satype("SA_FLOAT_T") ->
%     sa_imm_attr_safloatt;
% convert_dbtype_to_satype("SA_DOUBLE_T") ->
%     sa_imm_attr_sadoublet;
% convert_dbtype_to_satype("SA_STRING_T") ->
%     sa_imm_attr_sastringt;
% convert_dbtype_to_satype("SA_ANY_T") ->
%     sa_imm_attr_saanyt.

%%--------------------------------------------------------------------
% convert_satype_to_dbtype(sa_imm_attr_saint32t) ->
%     "SA_INT32_T";
% convert_satype_to_dbtype(sa_imm_attr_sauint32t) ->
%     "SA_UINT32_T";
% convert_satype_to_dbtype(sa_imm_attr_saint64t) ->
%     "SA_INT64_T";
% convert_satype_to_dbtype(sa_imm_attr_sauint64t) ->
%     "SA_UINT64_T";
% convert_satype_to_dbtype(sa_imm_attr_satimet) ->
%     "SA_TIME_T";
% convert_satype_to_dbtype(sa_imm_attr_sanamet) ->
%     "SA_NAME_T";
% convert_satype_to_dbtype(sa_imm_attr_safloatt) ->
%     "SA_FLOAT_T";
% convert_satype_to_dbtype(sa_imm_attr_sadoublet) ->
%     "SA_DOUBLE_T";
% convert_satype_to_dbtype(sa_imm_attr_sastringt) ->
%     "SA_STRING_T";
% convert_satype_to_dbtype(sa_imm_attr_saanyt) ->
%     "SA_ANY_T".
