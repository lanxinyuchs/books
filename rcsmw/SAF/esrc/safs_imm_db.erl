%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2017. All Rights Reserved.
%%
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%%
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_imm_db.erl
%%
%% Description:
%%    IMM Database
%%
%%--------------------------------------------------------------------
-module(safs_imm_db).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("om.hrl").
-include("safs_imm_db.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([init/0, create_context/0, delete_context/1,
	 add_classes/1,
	 add/3, ccb_add/5, ccb_add/6, ccb_delete/2, ccb_delete/3, ccb_modify/3, ccb_modify/4,
	 ccb_read/3, search/4, search_incr/5, search_cont/1,
	 rt_add/4, rt_delete/2, rt_modify/3,
	 add_class_def/4, get_class/1, get_class/2, get_class_types/1, get_class_def/1,
	 delete_class_def/1]).
-export([oi_set/4, oi_get/1, oi_get/2, oi_release/4,
	 ci_set/3, ci_get/1, ci_release/3]).
-export([ao_set/3, ao_get/1, ao_get/2, ao_release/3, ao_clear/2, ao_involved_objs/2,
	 remove_all_object_ownership/2]).
-export([import_files/2]).
-export([get_key/1, mk_key/1, key_to_bin/1]).
-export([get_imm_objects_payload/1]).
-export([error_msg/1]).
-export([ccb_set_extra_attributes/4, ccb_read_extra_attributes/2,
	 clean_rt_data_for_oi_cold_restart/1,
	 clean_rt_data_for_oi_warm_restart/1]).
-export([get_class_index/2]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 trace_groups/0,
	 trace_points_list/0,
	 get_schema/1,
	 check_attrs_not_changed/3
        ]).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% Note: Remember to update 'get_imm_objects_payload' if ldap record is changed.
-record(ldap, {key, persistent, attrs, class, oi, ao, oi_appliers = [],
	       extra_attrs = []}).
-record(cache, {key, struct, oi, vals}).
-record(imm_reverse_refs, {key, value}).

-record(entry, {op, val}).

-record(imm_search_index, {key, value}).

%%====================================================================
%% Functions
%%====================================================================
init() ->
    internal_init().

add_classes([]) -> fun() -> mnesia:read_lock_table(imm_class), ok end;
add_classes(Cs) when is_list(Cs) ->
    fun() ->
	    mnesia:write_lock_table(imm_class),
	    %%LATH: Add validation of class def !!!!!!!!!!!!!!
	    CheckClass =
		fun(#imm_class{name=ClassName} = C) ->
			case mnesia:read(imm_class, ClassName) of
			    [] ->
				mnesia:write(imm_class, C, write);
			    _ ->
				error_logger:warning_msg("**********************************************\n"
							 "Class already exists, it will NOT be loaded!!\n"
							 "ClassName: ~p\n"
							 "**********************************************\n",
							 [ClassName])
			end
		end,
	    [First|_] = [CheckClass(C) || C <- Cs],
	    First
    end.

create_context() ->
    gb_trees:empty().

delete_context(_Ctx) ->
    ok.

get_class(DNBin) ->
    get_class(DNBin, none).
get_class(DNBin, Context) ->
    DN = mk_key(DNBin, []),
    #ldap{class=Class} = read_dn(DN, Context),
    {ok, Class}.

create_attr_records([], Acc) ->
    lists:reverse(Acc);
create_attr_records([{Name, Type, Category, Flags, Default} |AttrDefinitions], Acc) ->
    create_attr_records(AttrDefinitions,
			 [#imm_attr{name=Name,
				    type=Type,
				    category=Category,
				    flags=Flags,
				    default=Default} |Acc]).

add_class_def(ClassName, Category, {RdnName, RdnType, RdnCategory, RdnFlags}, AttrDefinitions) ->
    %%LATH: Add validation of class def everything done by OM module??
    Attrs = create_attr_records(AttrDefinitions, []),
    Rdn = #imm_rdn{name=RdnName,
		   type=RdnType,
		   category=RdnCategory,
		   flags=RdnFlags},
    ClassDef = #imm_class{name=ClassName, rdn=Rdn, category=Category, attrs=Attrs},

    [] =:= mnesia:dirty_read(imm_class, ClassName) orelse throw(already_exists),
    fun() ->
	    mnesia:write_lock_table(imm_class),
	    [] = mnesia:read(imm_class, ClassName),
	    mnesia:write(imm_class, ClassDef, write)
    end.

get_class_types(ClassName) when is_atom(ClassName) ->
    get_class_types(get_schema(ClassName));
get_class_types(#imm_class{attrs=Attrs, rdn=Rdn}) ->
    Attrs2 = [{Attr, Type} || #imm_attr{name=Attr, type=Type} <- Attrs],
    #imm_rdn{name=RName, type=RType} = Rdn,
    [{RName, RType} |Attrs2];
get_class_types(DNBin) ->
    {ok, ClassName} = get_class(DNBin),
    get_class_types(ClassName).

get_class_def(ClassName) when is_atom(ClassName) ->
    get_class_def(get_schema(ClassName));
get_class_def(#imm_class{category=Category, attrs=Attrs, rdn=Rdn}) ->
    Attrs2 = [{Name, Type, AttrCategory, Flags, Default} ||
		 #imm_attr{name=Name, type=Type, category=AttrCategory,
			   flags=Flags, default=Default} <- Attrs],
    #imm_rdn{name=RName, type=RType, category=RCategory, flags=RFlags} = Rdn,
    {Category, [{RName, RType, RCategory, [rdn |RFlags], undefined} |Attrs2]};
get_class_def(DNBin) ->
    {ok, ClassName} = get_class(DNBin),
    get_class_def(ClassName).

delete_class_def(ClassName) when is_atom(ClassName) ->
    [] =/= mnesia:dirty_read(imm_class, ClassName) orelse throw(no_such_class),

    MS = [{#ldap{class=ClassName, _='_'}, [], ['$_']}],
    [] =:= mnesia:dirty_select(imm_objects, MS) orelse throw(objects_exists),

    fun() ->
	    mnesia:write_lock_table(imm_class),
	    [_] = mnesia:read(imm_class, ClassName),
	    [] = mnesia:select(imm_objects, MS, read),
	    mnesia:delete(imm_class, ClassName, write)
    end.

get_schema(Class) when is_atom(Class) ->
    Read = case mnesia:is_transaction() of
	       true  -> mnesia:read(imm_class, Class);
	       false -> mnesia:dirty_read(imm_class, Class)
	   end,
    case Read of
	[Schema] -> Schema;
	_ -> throw(no_such_class)
    end;
get_schema(Class) ->
    get_schema(list_to_atom(binary_to_list(Class))).

add(DNBin, Class, Attrs) -> %% Used ONLY when parsing XML
    DN = mk_key(DNBin, []),
    Schema = get_schema(Class),
    [] =:= mnesia:dirty_read(imm_objects, DN) orelse throw(already_exists),
    {Persistent, Cached, _DefaultAttrs} = validate(DN, Attrs, add, config, Schema), %%LATH Persistent rt
    fun() ->
	    %% Recheck that nothing changed
	    [Schema1] = mnesia:read(imm_class, Class),
	    check_class_not_changed(Schema, Schema1),
	    [] = mnesia:read(imm_objects, DN, write),
	    %% Apply
	    write_persistent(DN, config, Persistent, Class, {undefined, undefined}, []), %%LATH Is it possible to load persistent RT objects
	    write_cached(DN, undefined, Cached),
	    write_class_index(get_parent(DN), Class, DN) %%LATH: Test of class index
    end.

get_parent([_]) ->
    <<>>;
get_parent(DN) ->
    [_ | PDN] = lists:reverse(DN),
    key_to_bin(lists:reverse(PDN)).

ccb_add(AdminOwner, ParentDN, Class, Attrs0, ExtraAttrs0) -> %% Used from OM
    ccb_add(AdminOwner, ParentDN, Class, Attrs0, ExtraAttrs0, none).
ccb_add(AdminOwner, ParentDN, Class, Attrs0, ExtraAttrs0, Context) ->
    Type = config,
    PDN = mk_key(ParentDN, []),
    case ParentDN of
	<<>> ->
	    ok;
	_ ->
	    _ = read_dn(PDN, Context)
    end,
    Schema = get_schema(Class),
    #imm_class{category=Category, rdn=#imm_rdn{name=RDName}} = Schema,
    Category =:= Type orelse throw({invalid_param, Class}),
    {value, {RDName, _RDType, [Value]}, Attrs} =   %%LATH TC FIXED
	case lists:keytake(RDName, 1, Attrs0) of
	    false -> throw({invalid_param, Class});
	    R -> R
	end,
    RDN = [<<(unicode:characters_to_binary(atom_to_list(RDName)))/binary, $=, Value/binary>>],
    DN = case ParentDN of
	     <<>> -> RDN;
	     _ -> PDN ++ RDN
	 end,
    check_no_exists(DN, Context),
    {Persistent, Cached, DefaultAttrs} = validate(DN, Attrs, add, Type, Schema),%%LATH TC FIXED
    %% Get NameT attributes that has the NO_DANGLING flag set
    NoDanglingRefs = get_nd_refs_from_attrs(Attrs, Schema),%%LATH TC FIXED
    {OI, _} = subtree_oi_ao(PDN, Context),
    AO = {AdminOwner, one},
    IsPersistentObj = is_obj_persistent(Type, Schema),

    Ldap = #ldap{key=DN, persistent=IsPersistentObj, class=Class, oi=OI, ao=AO,
		 attrs=Persistent, extra_attrs=ExtraAttrs0},
    Entry = #entry{op=write, val=Ldap},
    {fun() ->
	     %% Recheck that nothing changed
	     [Schema1] = mnesia:read(imm_class, Class),
	     check_class_not_changed(Schema, Schema1),
	     [] = mnesia:read(imm_objects, DN, write),
	     {OI, _} = subtree_oi_ao(mnesia:read(imm_objects, PDN, read)),
	     %% Apply
	     write_persistent(DN, IsPersistentObj, Persistent, Class, {OI, AO}, [], ExtraAttrs0),
	     OI2 = case get_oi_name(OI) of
		       undefined ->
			   Schema#imm_class.oi;
		       OI0 ->
			   OI0
		   end,
	     write_cached(DN, OI2, Cached),
             write_class_index(ParentDN, Class, DN)
     end,
     fun() ->
	     case mnesia:read(imm_objects, DN) of
		 [_] ->
		     ok = check_objects_exists(DN, NoDanglingRefs),
		     write_reverse_refs(DN, NoDanglingRefs);
		 [] ->
		     ok
	     end
     end,
     DN, DefaultAttrs, set_context(DN, Entry, Context)}.

rt_add(OIName, ParentDN, Class, Attrs0) ->
    Type = runtime,
    PDN = mk_key(ParentDN, []),
    fun() ->
	    case ParentDN of
		<<>> ->
		    ok;
		_ ->
		    case mnesia:read(imm_objects, PDN, read) of
			[] -> throw(no_exists);
			[_Res]  -> ok
		    end
	    end,
	    [Schema] = mnesia:read(imm_class, Class),
	    #imm_class{category=Category, rdn=#imm_rdn{name=RDName}} = Schema,
	    Category =:= Type orelse throw({invalid_param, Class}),
	    {value, {RDName, _RDType, [Value]}, Attrs} =
		case lists:keytake(RDName, 1, Attrs0) of
		    false -> throw({invalid_param, Class});
		    R -> R
		end,
	    RDN = [<<(unicode:characters_to_binary(atom_to_list(RDName)))/binary, $=, Value/binary>>],
	    DN = case ParentDN of
		     <<>> -> RDN;
		     _ -> PDN ++ RDN
		 end,
	    []  =:= mnesia:read(imm_objects, DN, write) orelse throw(already_exists),
	    {Persistent, Cached, _DefaultAttrs} = validate(DN, Attrs, add, Type, Schema),
	    %% Get NameT attributes that has the NO_DANGLING flag set
	    %% SA_IMM_ATTR_NO_DANGLING attribute is only meaningful for configuration
	    %% attributes of type SA_IMM_ATTR_SANAMET.
	    {_, AO} = subtree_oi_ao(mnesia:read(imm_objects, PDN, read)),

	    IsPersistentObj = is_obj_persistent(Type, Schema),

	    write_persistent(DN, IsPersistentObj, Persistent, Class, {{OIName, one}, AO}, []),
	    write_cached(DN, OIName, Cached),
	    write_class_index(ParentDN, Class, DN)
     end.

ccb_delete(AdminOwner, Key) ->
    ccb_delete(AdminOwner, Key, none).
ccb_delete(AdminOwner, Key0, Context) ->
    Type = config,
    [DN] = mk_search(Key0, subtree),
    MS = [{#ldap{key=DN, _='_'}, [], ['$_']}],
    Objs = read_subtree_dn(MS, DN, Context),
    Objs =:= [] andalso throw(no_exists),

    VerifyCategory = fun(#ldap{class=Class}) ->
    			     case get_schema(Class) of
    				 #imm_class{category=Type} -> true;
    				 _ -> false
    			     end
    		     end,
    VerifyCategory(lists:keyfind(mk_key(Key0), 2, Objs)) orelse throw(bad_operation),

    Check = fun(Object) ->
		    verify_privilege(AdminOwner, Object, Type) orelse throw(bad_operation)
	    end,
    [Check(Obj) || Obj <- Objs],
    Delete = fun() ->
		     Objs2 = mnesia:select(imm_objects, MS, write),

		     ok = check_objs_not_changed(Objs, Objs2),
		     DelObj = fun(#ldap{key=SubTreeDN, attrs=Attrs}) ->
				      delete_cached(SubTreeDN, Attrs),
				      mnesia:delete(imm_objects, SubTreeDN, write)
			      end,
		     [DelObj(Obj) || Obj <- Objs],
		     delete_class_index(DN),
		     ok
	     end,
    DeleteRefs = fun() ->
			 delete_reverse_refs(DN),
			 ok = check_reverse_refs_delete(DN)
		 end,

    DelContext = fun(#ldap{key=SubTreeDN}, Cnt) ->
			 set_context(SubTreeDN, #entry{op=del}, Cnt)
		 end,
    Context1 = lists:foldl(DelContext, Context, Objs),
    {Delete,
     DeleteRefs,
     lists:reverse(Objs), Context1}.

rt_delete(OIName, Key) ->
    Type = runtime,
    [DN] = mk_search(Key, subtree),
    MS = [{#ldap{key=DN, _='_'}, [], ['$_']}],
    VerifyCategory = fun(#ldap{class=Class}) ->
			     case get_schema(Class) of
				 #imm_class{category=Type} -> true;
				 _ -> false
			     end
		     end,
    fun() ->
	    Objs = mnesia:select(imm_objects, MS, write),
	    Objs =:= [] andalso throw(no_exists),
	    DelObj = fun(#ldap{key=SubTreeDN, attrs=Attrs} = Object) ->
			     verify_privilege(OIName, Object, Type) orelse throw(bad_operation),
			     VerifyCategory(Object) orelse throw(bad_operation),
			     delete_cached(SubTreeDN, Attrs),
			     mnesia:delete(imm_objects, SubTreeDN, write)
		     end,
	    [DelObj(Obj) || Obj <- Objs],
	    delete_class_index(DN),
	    delete_reverse_refs(DN),
	    ok = check_reverse_refs_delete(DN)
     end.

ccb_modify(AdminOwner, Key0, Ops) ->
    ccb_modify(AdminOwner, Key0, Ops, none).
ccb_modify(AdminOwner, Key0, Ops, Context) ->
    Type = config,
    DN = mk_key(Key0),
    Object = #ldap{attrs=Attrs, persistent=IsPersistentObj, class=Class,
		   oi=OI, ao=AO, oi_appliers=OiAppliers,
		   extra_attrs=ExtraAttrs} = read_dn(DN, Context),
    Schema = #imm_class{category=Category} = get_schema(Class),
    Type =:= config andalso Category =:= runtime andalso throw({bad_operation, not_config}),
    verify_privilege(AdminOwner, Object, Type) orelse throw({bad_operation, {not_ao, AdminOwner}}),

    {Updated, Changed} = update_attrs(Ops, Schema, DN, Type, Attrs, []), %%LATH TC
    {Persistent, Cached, _DefaultAttrs} = validate(DN, Updated, modify, Type, Schema), %%LATH TC NOT
    Ldap = #ldap{key=DN, persistent=IsPersistentObj, attrs=Persistent, class=Class,
		 oi=OI, ao=AO, oi_appliers=OiAppliers, extra_attrs=ExtraAttrs},
    Entry = #entry{op=write, val=Ldap},
    {AddNdRefs, DelNdRefs} = get_nd_refs_from_ops(Ops, Schema, Attrs),%%LATH TC

    {fun() ->
	     %% Recheck that nothing changed
	     [#ldap{attrs=Attrs1, class=Class, oi=OI2, ao=AO2,
		   extra_attrs=ExtraAttrs}]
	      	 = mnesia:read(imm_objects, DN, write),
	     check_owner_not_changed(OI, OI2) orelse throw({object_changed, oi, DN}),
	     check_owner_not_changed(AO, AO2) orelse throw({object_changed, ao, DN}),
	     {AttrsChanged, CurrentRtAttrs} =
		 check_attrs_not_changed(Schema#imm_class.attrs, Attrs, Attrs1),
	     % io:format("Persistent: ~p\n", [Persistent]),
	     % io:format("Cached: ~p\n", [Cached]),
	     % io:format("CurrentRtAttrs: ~p\n", [CurrentRtAttrs]),
	     true =:= AttrsChanged  orelse throw({object_changed, attrs, DN}),
	     [Schema1] = mnesia:read(imm_class, Class),
	     check_class_not_changed(Schema, Schema1),
	     write_persistent(DN, IsPersistentObj,
			      change_rt_values(Schema#imm_class.attrs, Persistent, CurrentRtAttrs, []),
			      Class, {OI2, AO2}, OiAppliers, ExtraAttrs),
	     OI3 = case get_oi_name(OI2) of
		       undefined ->
			   Schema#imm_class.oi;
		       OI0 ->
			   OI0
		   end,
	     Res = write_cached(DN, OI3, Cached),
	     delete_reverse_refs(DN, DelNdRefs),
	     Res
     end,
     fun() ->
	     case mnesia:read(imm_objects, DN) of
		 [_] ->
		     ok = check_objects_exists(DN, AddNdRefs),
		     write_reverse_refs(DN, AddNdRefs);
		 [] ->
		     ok
	     end
     end,
     set_context(DN, Entry, Context),
     Changed}.

change_rt_values(_AttrDefs, [], CurrentRtAttrs, Acc) ->
    lists:sort(Acc ++ CurrentRtAttrs);
change_rt_values(AttrDefs, [{N, _}=P |Persistent], CurrentRtAttrs, Acc) ->
    AttrDef = lists:keyfind(N, 2, AttrDefs),
    case AttrDef#imm_attr.category of
	config ->
	    change_rt_values(AttrDefs, Persistent, CurrentRtAttrs, [P | Acc]);
	runtime ->
	    change_rt_values(AttrDefs, Persistent, CurrentRtAttrs, Acc)
    end;
%% Structs
change_rt_values(AttrDefs, [{N, _, _}=P |Persistent], CurrentRtAttrs, Acc) ->
    AttrDef = lists:keyfind(N, 2, AttrDefs),
    case AttrDef#imm_attr.category of
	config ->
	    change_rt_values(AttrDefs, Persistent, CurrentRtAttrs, [P | Acc]);
	runtime ->
	    change_rt_values(AttrDefs, Persistent, CurrentRtAttrs, Acc)
    end.

rt_modify(OIName, Key0, Ops) ->
    Type = runtime,
    check_rt_attr_persistent(Key0, Ops) orelse throw(included_in_ccb),
    fun() ->
	    DN = mk_key(Key0),
	    [Object] = [#ldap{attrs=Attrs, persistent=IsPersistentObj, class=Class,
			      oi=OI, ao=AO, oi_appliers=OiAppliers,
			      extra_attrs=ExtraAttrs}]
		= mnesia:read(imm_objects, DN, write),
	    verify_privilege(OIName, Object, Type) orelse throw(bad_operation),
	    [Schema] = mnesia:read(imm_class, Class),
	    {Updated, Changed} = update_attrs(Ops, Schema, DN, Type, Attrs, []),
	    {Persistent, Cached, _DefaultAttrs} = validate(DN, Updated, modify, Type, Schema),
	    % io:format("Persistent: ~p\n", [Persistent]),
	    % io:format("Cached: ~p\n", [Cached]),
	    {AddNdRefs, DelNdRefs} = get_nd_refs_from_ops(Ops, Schema, Attrs),
	    ok = check_objects_exists(DN, AddNdRefs),
	    write_persistent(DN, IsPersistentObj, Persistent, Class, {OI,AO}, OiAppliers, ExtraAttrs),
	    write_cached(DN, OIName, Cached),
	    delete_reverse_refs(DN, DelNdRefs),
	    write_reverse_refs(DN, AddNdRefs),
            {ok, Changed}
    end.

check_rt_attr_persistent(DN, Ops) ->
   {_, Attrs} = get_class_def(DN),
    check_rt_attr_persistent_2(DN, Attrs, Ops).

check_rt_attr_persistent_2(_DN, _Attrs, []) ->
    true;
check_rt_attr_persistent_2(DN, Attrs, [{_, X} |Ops]) ->
    Key = element(1, X),
    case lists:keyfind(Key, 1, Attrs) of
	false ->
	    throw({invalid_param, Key});
	{Key, _, config, _, _} ->
	    throw({invalid_param, Key});
	{Key, _, runtime, Flags, _} ->
	    CachedOrPersistentRtAttr = lists:member(cached, Flags) orelse lists:member(persistent, Flags),
	    case  CachedOrPersistentRtAttr of
		true ->
		    false == safs_imm_om:affected_by_ccb(DN, sa_imm_one) orelse throw(included_in_ccb);
		false ->
		    check_rt_attr_persistent_2(DN, Attrs, Ops)
	    end
    end.

ccb_set_extra_attributes(AdminOwner, Key0, Ops, Context) ->
    Type = config,
    DN = mk_key(Key0),
    Object = #ldap{attrs=Attrs, persistent=IsPersistentObj, class=Class,
		   oi=OI, ao=AO, oi_appliers=OiAppliers,
		   extra_attrs=ExtraAttrs} = read_dn(DN, Context),
    Schema = #imm_class{category=Category} = get_schema(Class),
    Type =:= config andalso Category =:= runtime andalso throw({bad_operation, not_config}),
    verify_privilege(AdminOwner, Object, Type) orelse throw({bad_operation, {not_ao, AdminOwner}}),

    UpdatedExtraAttrs = update_extra_attrs(Ops, ExtraAttrs),
    Ldap = Object#ldap{extra_attrs=UpdatedExtraAttrs},
    Entry = #entry{op=write, val=Ldap},

    {fun() ->
	     %% Recheck that nothing changed
	     [#ldap{attrs=Attrs1, class=Class, oi=OI2, ao=AO2, extra_attrs=ExtraAttrs}]
	      	 = mnesia:read(imm_objects, DN, write),
	     check_owner_not_changed(OI, OI2) orelse throw({object_changed, oi, DN}),
	     check_owner_not_changed(AO, AO2) orelse throw({object_changed, ao, DN}),
	     {AttrsChanged, _CurrentRtAttrs} =
		 check_attrs_not_changed(Schema#imm_class.attrs, Attrs, Attrs1),
	     % io:format("CurrentRtAttrs: ~p\n", [UpdatedWithCurrentRtAttrs]),
	     true =:= AttrsChanged  orelse throw({object_changed, attrs, DN}),

	     [Schema1] = mnesia:read(imm_class, Class),
	     check_class_not_changed(Schema, Schema1),
	     write_persistent(DN, IsPersistentObj, Attrs1, Class, {OI, AO}, OiAppliers, UpdatedExtraAttrs)
	     %% Because just the extra_args are changed there is no need to update cache
	     %% and reference tables
     end,
     set_context(DN, Entry, Context)}.

ccb_read(Key0, Attr, Context) ->
    DN = mk_key(Key0),
    Object = #ldap{attrs=Attrs, class=Class, oi=OI, ao=AO} = read_dn(DN, Context),
    [{_, _, Attributes}] = return_attrs([Object], Attr, DN, true, dirty),
    {fun() ->
	     [#ldap{attrs=Attrs, class=Class, oi=OI2, ao=AO2}] = Object2
		 = mnesia:read(imm_objects, DN, read),
	     check_owner_not_changed(OI, OI2) orelse
		 throw({object_changed, oi, DN}),
	     check_owner_not_changed(AO, AO2) orelse
		 throw({object_changed, ao, DN}),
	     Object2
     end, Class, Attributes}.

ccb_read_extra_attributes(Key0, Context) ->
    DN = mk_key(Key0),
    #ldap{extra_attrs=ExtraAttributes, oi=OI, ao=AO} = read_dn(DN, Context),
    {fun() ->
	     [#ldap{extra_attrs=ExtraAttributes, oi=OI2, ao=AO2}] = Object2
		 = mnesia:read(imm_objects, DN, read),
	     check_owner_not_changed(OI, OI2) orelse
		 throw({object_changed, oi, DN}),
	     check_owner_not_changed(AO, AO2) orelse
		 throw({object_changed, ao, DN}),
	     Object2
     end, ExtraAttributes}.

%% Scope = one | sublevel | subtree
%% Attr = no | config | persistent | all | {some, [AttrName]}
search(Root, one, Attr, FailAttrNotExist) ->
    [DN] = mk_search(Root, one),
    fun() ->
	    Objs = mnesia:read(imm_objects, DN, read),
	    return_attrs(Objs, Attr, DN, FailAttrNotExist)
    end;
search(Root, Scope, Attr, FailAttrNotExist) ->
    DN = mk_search(Root, Scope),
    fun() ->
	    MS = case Attr of
		     config ->
			 [{#ldap{key=Key, persistent=config, _='_'}, [], ['$_']} || Key <- DN];
		     persistent ->
			 [{#ldap{key=Key, persistent='$1', _='_'},
			   [{'=/=', '$1', false}],
			    ['$_']} || Key <- DN];
		     _ ->
			 [{#ldap{key=Key, _='_'}, [], ['$_']} || Key <- DN]
		 end,
	    Objs = mnesia:select(imm_objects, MS, read),
	    return_attrs(Objs, Attr, DN, FailAttrNotExist)
    end.

%%%
search_incr(Root, Scope, Classes, Attr, FailAttrNotExist) ->
    DN = mk_search(Root, Scope),
    MS = mk_matchspec(DN, Classes, Attr),
    fun() ->
	    case mnesia:select(imm_objects, MS, ?SAFS_SEARCH_RESULT_LIMIT, read) of
		{Objs,Cont} ->
		    {return_attrs(Objs, Attr, DN, FailAttrNotExist),
		     {Cont, Attr, DN, FailAttrNotExist}};
		EoF -> EoF
	    end
    end.

search_cont({Cont, Attr, DN, FailAttrNotExist}) ->
    fun() ->
	    case mnesia:select(Cont) of
		{Objs,Cont1} ->
		    {return_attrs(Objs, Attr, DN, FailAttrNotExist),
		     {Cont1, Attr, DN, FailAttrNotExist}};
		EoF -> EoF
	    end
    end.


mk_matchspec(DN, undefined, Attr) ->
    case Attr of
	config ->
	    [{#ldap{key=Key, persistent=config, _='_'}, [], ['$_']} || Key <- DN];
	persistent ->
	    [{#ldap{key=Key, persistent='$1', _='_'},
	      [{'=/=', '$1', false}],
	      ['$_']} || Key <- DN];
	_ ->
	    [{#ldap{key=Key, _='_'}, [], ['$_']} || Key <- DN]
    end;
mk_matchspec(DN, Classes, Attr) ->
    case Attr of
	config ->
	    [{#ldap{key=Key, persistent=config, class='$1', _='_'},
	      [{'==', '$1', C}],
	      ['$_']} || C <- Classes, Key <- DN];
	persistent ->
	    [{#ldap{key=Key, persistent='$1', class='$2', _='_'},
	      [{'=/=', '$1', false}, {'==', '$2', C}],
	      ['$_']} || C <- Classes, Key <- DN];
	_ ->
	    [{#ldap{key=Key, class='$1', _='_'},
	      [{'==', '$1', C}],
	      ['$_']} || C <- Classes, Key <- DN]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Object | Class implementor functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Scope = one | sublevel | subtree
oi_set(Root, Name, one, Applier) ->
    [DN] = mk_search(Root, one),
    fun() ->
	    Obj = #ldap{oi=OI, class=Class, oi_appliers=OiAppliers} =
		case mnesia:read(imm_objects, DN, write) of
		    [] -> throw(no_exists);
		    [O] -> O
		end,
	    [#imm_class{category=Category}] = mnesia:dirty_read(imm_class, Class),
	    Category =:= runtime andalso throw(bad_operation),
	    case Applier of
		false ->
		    case OI of
			undefined -> ok;
			{Name, _} -> ok;
			_ -> throw(already_exists)
		    end,
		    mnesia:write(imm_objects, Obj#ldap{oi={Name, one}}, write),
		    %% Fix chached attributes (they contain the OI to improve warm startup)
		    Cached = mnesia:match_object(imm_rt_cache, #cache{key={DN,'_'}, _='_'}, write),
		    [ok = mnesia:write(imm_rt_cache, CachedAttr#cache{oi=Name}, write)
		     || CachedAttr <- Cached],
		    ok;
		true ->
		    mnesia:write(imm_objects, Obj#ldap{oi_appliers=[{Name, one} | OiAppliers]}, write),
		    ok
	    end

    end;
oi_set(Root, Name, Scope, Applier) ->
    DN = mk_search(Root, Scope),
    MS = [{#ldap{key=Key, _='_'}, [], ['$_']} || Key <- DN],

    fun() ->
	    Objs = mnesia:select(imm_objects, MS, write),
	    Objs =:= [] andalso throw(no_exists),
	    Check = fun(#ldap{oi=OI, class=Class}) ->
			    [#imm_class{category=Category}] = mnesia:dirty_read(imm_class, Class),
			    Category =:= runtime andalso throw(bad_operation),
			    case Applier of
				false ->
				    case OI of
					undefined -> ok;
					{Name, _} -> ok;
					_ -> throw(already_exists)
				    end,
				    ok;
				true ->
				    ok
			    end
		    end,
	    [Check(Obj) || Obj <- Objs],
	    Write = fun(#ldap{oi_appliers=OiAppliers} = Obj) ->
			    case Applier of
			      	false ->
				    mnesia:write(imm_objects, Obj#ldap{oi={Name,Scope}}, write),
				    %% Fix chached attributes (they contain the OI to improve warm startup)
				    Cached = mnesia:match_object(imm_rt_cache, #cache{key={Obj#ldap.key,'_'},
										      _='_'}, write),
				    [ok = mnesia:write(imm_rt_cache, CachedAttr#cache{oi=Name}, write)
				     || CachedAttr <- Cached];
				true ->
				    %%LATH: improvement, if same name do not add
				    mnesia:write(imm_objects, Obj#ldap{oi_appliers=[{Name, Scope} |OiAppliers]}, write)
			    end
		    end,
	    [Write(Obj) || Obj <- Objs],
 	    ok
    end.

oi_get(DNBin) ->
    oi_get(DNBin, none).

oi_get(DNBin, Context) ->
    DN = mk_key(DNBin, []),
    Obj = read_dn(DN, Context),

    {OI, Appliers} =
        case Obj of
	    #ldap{oi=undefined, class=Class, oi_appliers=InstAppliers} ->
		[#imm_class{oi=OI0, oi_appliers=ClassAppliers}] = mnesia:dirty_read(imm_class, Class),
		{OI0, [Name || {Name, _} <- InstAppliers] ++ ClassAppliers};
	    #ldap{oi={OI0, _Scope}, class=Class, oi_appliers=InstAppliers} ->
		[#imm_class{oi_appliers=ClassAppliers}] = mnesia:dirty_read(imm_class, Class),
		{OI0, [Name || {Name, _} <- InstAppliers] ++ ClassAppliers}
	end,
    {fun() ->
	     case mnesia:read(imm_objects, DN, read) of
		 [#ldap{oi=undefined}] ->
		     undefined = Obj#ldap.oi;
		 [#ldap{oi={OI2, _Scope2}}] ->
		     OI2 = OI;
		 [] ->
		     undefined = OI
	     end,
	     OI
     end, OI, Appliers}.

oi_release(Root, Name, one, false) ->
    [DN] = mk_search(Root, one),
    fun() ->
	    Obj = case mnesia:read(imm_objects, DN, write) of
		      [Ok=#ldap{oi={Name, _Scope}}] -> Ok;
		      _ -> throw(no_exists)
		  end,
	    mnesia:write(imm_objects, Obj#ldap{oi=undefined}, write),
	    %% Fix chached attributes (they contain the OI to improve warm startup)
	    Cached = mnesia:match_object(imm_rt_cache, #cache{key={DN,'_'}, _='_'}, write),
	    [ok = mnesia:write(imm_rt_cache, CachedAttr#cache{oi=undefined}, write)
	     || CachedAttr <- Cached],
	    ok
    end;
oi_release(Root, Name, one, true) ->
    [DN] = mk_search(Root, one),
    fun() ->
	    Obj= #ldap{oi_appliers=OiAppliers} =
		case mnesia:read(imm_objects, DN, write) of
		    [Ok] -> Ok;
		    _ -> throw(no_exists)
		end,
	    case lists:keymember(Name, 1, OiAppliers) of
		true ->
		    mnesia:write(imm_objects, Obj#ldap{oi_appliers=lists:keydelete(Name, 1, OiAppliers)}, write);
		false -> throw(no_exists)
	    end,
	    ok
    end;
oi_release(Root, Name, Scope, false) ->
    DN = mk_search(Root, Scope),
    MS = [{#ldap{key=Key, _='_'}, [], ['$_']} || Key <- DN],
    fun() ->
	    Objs = mnesia:select(imm_objects, MS, write),
	    Objs == [] andalso throw(no_exists),
	    Check = fun(#ldap{oi={OI, _Scope}}) when OI =:= Name -> ok;
		       (_) -> throw(no_exists)
		    end,
	    [Check(Obj) || Obj <- Objs],
	    Write = fun(Obj) ->
			    mnesia:write(imm_objects, Obj#ldap{oi=undefined}, write),
			    %% Fix chached attributes (they contain the OI to improve warm startup)
			    Cached = mnesia:match_object(imm_rt_cache, #cache{key={Obj#ldap.key,'_'}, _='_'}, write),
			    [ok = mnesia:write(imm_rt_cache, CachedAttr#cache{oi=undefined}, write)
			     || CachedAttr <- Cached]
		    end,
	    [Write(Obj) || Obj <- Objs],
	    ok
    end;
oi_release(Root, Name, Scope, true) ->
    DN = mk_search(Root, Scope),
    MS = [{#ldap{key=Key, _='_'}, [], ['$_']} || Key <- DN],
    fun() ->
	    Objs = mnesia:select(imm_objects, MS, write),
	    Objs == [] andalso throw(no_exists),
	    Check = fun(#ldap{oi_appliers=OiAppliers}) ->
			    case lists:keymember(Name, 1, OiAppliers) of
				true -> ok;
				false -> throw(no_exists)
			    end
		    end,
	    [Check(Obj) || Obj <- Objs],
	    Write = fun(#ldap{oi_appliers=OiAppliers} = Obj) ->
			    mnesia:write(imm_objects, Obj#ldap{oi_appliers=lists:keydelete(Name, 1, OiAppliers)}, write)
		    end,
	    [Write(Obj) || Obj <- Objs],
	    ok
    end.

ci_set(Class, Name, false) ->
    fun() ->
	    Schemas = mnesia:read(imm_class, Class, write),
	    Schemas == [] andalso throw(no_exists),
	    [Schema=#imm_class{oi=OI, category=Category}] = Schemas,
	    Category =:= runtime andalso throw(bad_operation),
	    case OI of
		undefined -> ok;
		Name -> ok;
		_ -> throw(already_exists)
	    end,
	    mnesia:write(Schema#imm_class{oi=Name}),
	    %% Fix chached attributes (they contain the OI to improve warm startup)
	    Objs = mnesia:match_object(imm_objects, #ldap{class=Class, _='_'}, read),
	    Write = fun(Obj) ->
			    Cached = mnesia:match_object(imm_rt_cache, #cache{key={Obj#ldap.key,'_'}, _='_'}, write),
			    [ok = mnesia:write(imm_rt_cache, CachedAttr#cache{oi=Name}, write)
			     || CachedAttr <- Cached]
		    end,
	    [Write(Obj) || Obj <- Objs],
	    ok
    end;
ci_set(Class, Name, true) ->
    fun() ->
	    Schemas = mnesia:read(imm_class, Class, write),
	    Schemas == [] andalso throw(no_exists),
	    [Schema=#imm_class{category=Category, oi_appliers=OiAppliers}] = Schemas,
	    Category =:= runtime andalso throw(bad_operation),
	    case lists:member(Name, OiAppliers) of
		true ->
		    ok;
		false ->
		    mnesia:write(Schema#imm_class{oi_appliers=[Name |OiAppliers]}),
		    ok
	    end
    end.

ci_get(Class) ->
    Objs = mnesia:dirty_read(imm_class, Class),
    Objs == [] andalso throw(no_exists),
    [#imm_class{oi=OI, oi_appliers=OiAppliers}] = Objs,
    {fun() ->
	     case mnesia:read(imm_class, Class, write) of
		 [#imm_class{oi=OI2}] ->
		     OI2 = OI;
		 [] ->
		     undefined = OI
	     end,
	     OI
     end, OI, OiAppliers}.

ci_release(Class, Name, false) ->
    fun() ->
	    Schemas = mnesia:read(imm_class, Class, write),
	    Schemas == [] andalso throw(no_exists),
	    [Schema=#imm_class{oi=OI}] = Schemas,
	    OI == Name orelse throw(no_exists),
	    mnesia:write(Schema#imm_class{oi=undefined}),
	    %% Fix chached attributes (they contain the OI to improve warm startup)
	    Objs = mnesia:match_object(imm_objects, #ldap{class=Class, _='_'}, read),
	    Write = fun(Obj) ->
			    Cached = mnesia:match_object(imm_rt_cache, #cache{key={Obj#ldap.key,'_'}, _='_'}, write),
			    [ok = mnesia:write(imm_rt_cache, CachedAttr#cache{oi=undefined}, write)
			     || CachedAttr <- Cached]
		    end,
	    [Write(Obj) || Obj <- Objs],
	    ok
    end;
ci_release(Class, Name, true) ->
    fun() ->
	    Schemas = mnesia:read(imm_class, Class, write),
	    Schemas == [] andalso throw(no_exists),
	    [Schema=#imm_class{oi_appliers=OiAppliers}] = Schemas,
	    mnesia:write(Schema#imm_class{oi_appliers=lists:delete(Name, OiAppliers)}),
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Admin Owner functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ao_involved_objs(Root,  one) ->
    [_DN] = mk_search(Root, one);
ao_involved_objs(Root, Scope) ->
    DN = mk_search(Root, Scope),
    MS = [{#ldap{key=Key, _='_'}, [], ['$_']} || Key <- DN],
    [ Obj || #ldap{key=Obj} <- mnesia:dirty_select(imm_objects, MS)].

%% Scope = one | sublevel | subtree
ao_set(Root, Name, one) ->
    [DN] = mk_search(Root, one),
    fun() ->
	    Obj = #ldap{ao=AO} =
		case mnesia:read(imm_objects, DN, write) of
		    [] -> throw(no_exists);
		    [O] -> O
		end,
	    case AO of
		undefined -> ok;
		{Name, _} -> ok;
		_ -> throw(already_exists)
	    end,
	    mnesia:write(imm_objects, Obj#ldap{ao={Name,one}}, write),
	    ok
    end;
ao_set(Root, Name, Scope) ->
    DN = mk_search(Root, Scope),
    MS = [{#ldap{key=Key, _='_'}, [], ['$_']} || Key <- DN],
    fun() ->
	    Objs = mnesia:select(imm_objects, MS, write),
	    Objs =:= [] andalso throw(no_exists),
	    Check = fun(#ldap{ao=AO}) ->
			    case AO of
				undefined -> ok;
				{Name, _} -> ok;
				_ -> throw(already_exists)
			    end
		    end,
	    [Check(Obj) || Obj <- Objs],
	    Write = fun(Obj) ->
			    mnesia:write(imm_objects, Obj#ldap{ao={Name,Scope}}, write)
		    end,
	    [Write(Obj) || Obj <- Objs],
	    ok
    end.

ao_get(DNBin) ->
    ao_get(DNBin, none).

ao_get(DNBin, Context) ->
    DN = mk_key(DNBin, []),
    Obj = read_dn(DN, Context),
    AO = case Obj of
	     #ldap{ao={AO0,_Scope}} ->
		 AO0;
	     _ ->
		 undefined
	 end,
    {fun() ->
	     case mnesia:read(imm_objects, DN, read) of
		 [#ldap{ao=undefined}] ->
		     undefined = AO;
		 [#ldap{ao={AO1, _Scope1}}] ->
		     AO1 = AO;
		 [] ->
		     undefined = AO
	     end,
	     AO
     end, AO}.

ao_release(Root, Name, one) ->
    [DN] = mk_search(Root, one),
    fun() ->
	    Obj = case mnesia:read(imm_objects, DN, write) of
		      [Ok=#ldap{ao={Name,_Scope}}] -> Ok;
		      _ -> throw(no_exists)
		  end,
	    mnesia:write(imm_objects, Obj#ldap{ao=undefined}, write),
	    ok
    end;
ao_release(Root, Name, Scope) ->
    DN = mk_search(Root, Scope),
    MS = [{#ldap{key=Key, _='_'}, [], ['$_']} || Key <- DN],

    fun() ->
	    Objs = mnesia:select(imm_objects, MS, write),
	    Check = fun(#ldap{ao={AO, _Scope}}) when AO =:= Name -> ok;
		       (_) -> throw(no_exists)
		    end,
	    Objs == [] andalso throw(no_exists),
	    [Check(Obj) || Obj <- Objs],
	    Write = fun(Obj) ->
			    mnesia:write(imm_objects, Obj#ldap{ao=undefined}, write)
		    end,
	    [Write(Obj) || Obj <- Objs],
	    ok
    end.

ao_clear(Root, one) ->
    [DN] = mk_search(Root, one),
    fun() ->
	    Obj = case mnesia:read(imm_objects, DN, write) of
		      [Ok = #ldap{}] -> Ok;
		      _ -> throw(no_exists)
		  end,
	    mnesia:write(imm_objects, Obj#ldap{ao=undefined}, write),
	    ok
    end;
ao_clear(Root, Scope) ->
    DN = mk_search(Root, Scope),
    MS = [{#ldap{key=Key, _='_'}, [], ['$_']} || Key <- DN],
    fun() ->
	    Objs = mnesia:select(imm_objects, MS, write),
	    Objs == [] andalso throw(no_exists),
	    Write = fun(Obj) ->
			    mnesia:write(imm_objects, Obj#ldap{ao=undefined}, write)
		    end,
	    [Write(Obj) || Obj <- Objs],
	    ok
    end.

remove_all_object_ownership(Name, DNs) ->
    DNs2 = lists:usort(DNs),
    fun() ->
	    Change = fun(DN) ->
			   Object = mnesia:read(imm_objects, DN, write),
			   case Object of
			       [#ldap{ao={AO, _Scope}} = Object2] when AO =:= Name ->
				   mnesia:write(imm_objects, Object2#ldap{ao=undefined}, write);
			       [#ldap{ao=undefined}] ->
				   ok;
			       _ ->
				   ok
			   end
		   end,
	    [Change(DN) || DN <- DNs2],
	    ok
    end.

%%====================================================================
%% Tracing
%%====================================================================
trace_groups() -> [general, object_impl, rt_objects, admin_owner, ccb, search, error].

trace_points_list() ->
    [
     {general,
      [{init, 0},
       {import_files, 2},
       {add_classes, 1},
       {get_class, 1},
       {get_class, 2},
       {get_class_types, 1}]},
     {object_impl,
      [{ci_set, 3},
       {ci_get, 1},
       {ci_release, 2},
       {oi_set, 3},
       {oi_get, 1},
       {oi_release, 3}]},
     {rt_objects,
      [{rt_add,4},
       {rt_delete,2},
       {rt_modify,3}]},
     {admin_owner,
      [{ao_set, 3},
       {ao_get, 1},
       {ao_get, 2},
       {ao_release, 3},
       {ao_clear, 2}]},
     {ccb,
      [{ccb_add, 5},
       {ccb_add, 6},
       {ccb_delete, 2},
       {ccb_delete, 3},
       {ccb_modify, 3},
       {ccb_modify, 4}]},
     {search,
      [{search, 3}]},
     {error,
      [{safs_error, 2}]}
    ].

%%--------------------------------------------------------------------
%% Function: safs_error/2
%% Description:
%%--------------------------------------------------------------------
safs_error(_F, _A) ->  ok.

%%====================================================================
%% Internal functions
%%====================================================================
update_attrs([{_, {Key, _, _}} |_], #imm_class{rdn=#imm_rdn{name=Key}}, _, _, _, _) ->
    throw({invalid_param, Key});
update_attrs([{sa_imm_attr_values_add, Attr = {Key, AType, NewValues}} |Rest],
	     Class = #imm_class{attrs=As}, DN, Type, Attrs0, ChangedAttrs0) ->
    Def = lists:keyfind(Key, #imm_attr.name, As),
    false =/= Def orelse throw({invalid_param, Key}),
    #imm_attr{category=Category} = Def,
    Type =:= Category orelse throw({invalid_param, Key}),
    case lists:keytake(Key, 1, Attrs0) of
	false ->
	    update_attrs(Rest, Class, DN, Type, [Attr |Attrs0], [Key |ChangedAttrs0]);
	{value, {_, '$imm_runtime'}, _} ->
	    %% I.e. non cached ignore
	    update_attrs(Rest, Class, DN, Type, Attrs0, ChangedAttrs0);
	{value, {_, '$imm_db_cache'}, Attrs} ->
	    %% Only OI is allowed to update so dirty should be ok
	    case mnesia:dirty_read({imm_rt_cache, {DN, Key}}) of
		[] -> update_attrs(Rest, Class, DN, Type, [Attr |Attrs0], [Key |ChangedAttrs0]);
		[#cache{vals=OldValues, struct=sa_imm_attr_csstructt}] ->
		    update_attrs(Rest, Class, DN, Type, [{Key, AType, OldValues++NewValues} |Attrs], [Key |ChangedAttrs0]);
		[#cache{vals=OldValues}] ->
		    update_attrs(Rest, Class, DN, Type, [{Key, OldValues++NewValues} |Attrs], [Key |ChangedAttrs0])
	    end;
	{value, {_, OldValues}, Attrs} when Category =:= config -> %%LATH: Persistent RT missing (RBS only using replace, so not urgent)
	    update_attrs(Rest, Class, DN, Type, [{Key, OldValues++NewValues} |Attrs], [Key |ChangedAttrs0]);
	{value, {_, _, OldValues}, Attrs} when Category =:= config -> %%LATH: Persistent RT missing (RBS only using replace, so not urgent)
	    update_attrs(Rest, Class, DN, Type, [{Key, AType, OldValues++NewValues} |Attrs], [Key |ChangedAttrs0])
    end;
update_attrs([{sa_imm_attr_values_delete, {Key, AType, Values}} |Rest],
	     Class = #imm_class{attrs=As}, DN, Type, Attrs0, ChangedAttrs0) ->
    Def = lists:keyfind(Key, #imm_attr.name, As),
    false =/= Def orelse throw({invalid_param, Key}),
    #imm_attr{category=Category} = Def,
    Type =:= Category orelse throw({invalid_param, Key}),
    case lists:keytake(Key, 1, Attrs0) of
	{value, {_, OldValues}, Attrs} when Category =:= config -> %%LATH: Persistent RT missing (RBS only using replace, so not urgent)
	    NewValues = lists:foldl(fun(Del, Vs) -> lists:delete(Del, Vs) end, OldValues, Values),
	    case {NewValues, OldValues} of
		{[], []} -> update_attrs(Rest, Class, DN, Type, Attrs, ChangedAttrs0);
		{[], _} -> update_attrs(Rest, Class, DN, Type, Attrs, [Key |ChangedAttrs0]);
		{L, L}  -> update_attrs(Rest, Class, DN, Type, [{Key, NewValues} |Attrs], ChangedAttrs0);
		_  -> update_attrs(Rest, Class, DN, Type, [{Key, NewValues} |Attrs], [Key |ChangedAttrs0])
	    end;
	{value, {_, _, OldValues}, Attrs} when Category =:= config -> %%LATH: Persistent RT missing (RBS only using replace, so not urgent)
	    NewValues = lists:foldl(fun(Del, Vs) -> lists:delete(Del, Vs) end, OldValues, Values),
	    case {NewValues, OldValues} of
		{[], []} -> update_attrs(Rest, Class, DN, Type, Attrs, ChangedAttrs0);
		{[], _} -> update_attrs(Rest, Class, DN, Type, Attrs, [Key |ChangedAttrs0]);
		{L, L}  -> update_attrs(Rest, Class, DN, Type, [{Key, AType, NewValues} |Attrs], ChangedAttrs0);
		_  -> update_attrs(Rest, Class, DN, Type, [{Key, AType, NewValues} |Attrs], [Key |ChangedAttrs0])
	    end;
	{value, {_, '$imm_runtime'}, Attrs} ->
	    update_attrs(Rest, Class, DN, Type, Attrs, ChangedAttrs0);
	{value, {_, '$imm_db_cache'}, Attrs} ->
	    %% Only OI is allowed to update so dirty should be ok
	    case mnesia:dirty_read({imm_rt_cache, {DN, Key}}) of
		[] -> throw(no_exists);
		[#cache{vals=OldValues, struct=sa_imm_attr_csstructt}] ->
		    NewValues = lists:foldl(fun(Del, Vs) -> lists:delete(Del, Vs) end, OldValues, Values),
		    case {NewValues, OldValues} of
			{[], []} -> update_attrs(Rest, Class, DN, Type, Attrs, ChangedAttrs0);
			{[], _} -> update_attrs(Rest, Class, DN, Type, Attrs, [Key |ChangedAttrs0]);
			{L, L}  -> update_attrs(Rest, Class, DN, Type, [{Key, AType, NewValues} |Attrs], ChangedAttrs0);
			_  -> update_attrs(Rest, Class, DN, Type, [{Key, AType, NewValues} |Attrs], [Key |ChangedAttrs0])
		    end;
		[#cache{vals=OldValues}] ->
		    NewValues = lists:foldl(fun(Del, Vs) -> lists:delete(Del, Vs) end, OldValues, Values),
		    case {NewValues, OldValues} of
			{[], []} -> update_attrs(Rest, Class, DN, Type, Attrs, ChangedAttrs0);
			{[], _} -> update_attrs(Rest, Class, DN, Type, Attrs, [Key |ChangedAttrs0]);
			{L, L}  -> update_attrs(Rest, Class, DN, Type, [{Key, NewValues} |Attrs], ChangedAttrs0);
			_  -> update_attrs(Rest, Class, DN, Type, [{Key, NewValues} |Attrs], [Key |ChangedAttrs0])
		    end
	    end;
	false ->
	    throw(no_exists)
    end;
update_attrs([{sa_imm_attr_values_replace, NewAttr = {Key, AType, NewValues}} |Rest],
	     Class = #imm_class{attrs=As}, DN, Type, Attrs0, ChangedAttrs0) ->
    Def = lists:keyfind(Key, #imm_attr.name, As),
    false =/= Def orelse throw({invalid_param, Key}),
    #imm_attr{category=Category, flags=Flags} = Def,
    Type =:= Category orelse throw({invalid_param, Key}),
    if
	length(NewValues) > 1 ->
	    true =:= lists:member(multi_value, Flags) orelse throw({invalid_param, Key});
	true ->
	    ok
    end,
    case lists:keytake(Key, 1, Attrs0) of
	{value, {_, '$imm_db_cache'}, Attrs} ->
	    %% Only OI is allowed to update so dirty should be ok
	    OldValues = mnesia:dirty_read({imm_rt_cache, {DN, Key}}),
	    ChangedAttrs =
		case {OldValues, NewValues} of
		    {[], []} -> ChangedAttrs0;
		    {_, []} -> [Key |ChangedAttrs0];
		    {[#cache{vals=L}], L} -> ChangedAttrs0;
		    _  -> [Key |ChangedAttrs0]
		end,
	    update_attrs(Rest, Class, DN, Type, [NewAttr|Attrs], ChangedAttrs);
	{value, _, Attrs} when NewValues == [] ->
	    update_attrs(Rest, Class, DN, Type, Attrs, [Key |ChangedAttrs0]);
	{value, {_, NewValues}, Attrs} ->
	    update_attrs(Rest, Class, DN, Type, [NewAttr|Attrs], ChangedAttrs0);
	{value, {_, _, NewValues}, Attrs} ->
	    update_attrs(Rest, Class, DN, Type, [{Key, AType, NewValues}|Attrs], ChangedAttrs0);
	{value, _, Attrs} ->
	    update_attrs(Rest, Class, DN, Type, [NewAttr|Attrs], [Key |ChangedAttrs0]);
	false when NewValues == [] ->
	    update_attrs(Rest, Class, DN, Type, Attrs0, ChangedAttrs0);
	false ->
	    update_attrs(Rest, Class, DN, Type, [NewAttr|Attrs0], [Key|ChangedAttrs0])
    end;
update_attrs([], _, _, _, Attrs, ChangedAttrs) ->
    {Attrs, ChangedAttrs}.


update_extra_attrs([{sa_imm_attr_values_add, NewAttr = {Key, Type, NewValues}}|Rest],
	     ExtraAttrs0) ->
    case lists:keytake(Key, 1, ExtraAttrs0) of
	false ->
	    update_extra_attrs(Rest, [NewAttr|ExtraAttrs0]);
	{value, {_, _, OldValues}, ExtraAttrs} ->
	    update_extra_attrs(Rest, [{Key, Type, OldValues++NewValues}|ExtraAttrs])
    end;
update_extra_attrs([{sa_imm_attr_values_delete, {Key, Type, Values}}|Rest],
		   ExtraAttrs0) ->
    case lists:keytake(Key, 1, ExtraAttrs0) of
	{value, {_, _, OldValues}, ExtraAttrs} ->
	    NewValues = lists:foldl(fun(Del, Vs) -> lists:delete(Del, Vs) end, OldValues, Values),
	    case NewValues of
		[] -> update_extra_attrs(Rest, ExtraAttrs);
		_  -> update_extra_attrs(Rest, [{Key, Type, NewValues} |ExtraAttrs])
	    end;
	false ->
	    throw(no_exists)
    end;
update_extra_attrs([{sa_imm_attr_values_replace, NewAttr = {Key, _Type, NewValues}}|Rest],
	     ExtraAttrs0) ->
    case lists:keytake(Key, 1, ExtraAttrs0) of
	{value, {_, _, _}, ExtraAttrs} when NewValues == [] ->
	    update_extra_attrs(Rest, ExtraAttrs);
	{value, {_, _, _}, ExtraAttrs} ->
	    update_extra_attrs(Rest, [NewAttr |ExtraAttrs]);
	false when NewValues == [] ->
	    update_extra_attrs(Rest, ExtraAttrs0);
	false ->
	    update_extra_attrs(Rest, [NewAttr |ExtraAttrs0])
    end;
update_extra_attrs([], ExtraAttrs) ->
    ExtraAttrs.

%%----------------------------------------------------------------------
write_persistent(DN, IsPersistent, Persistent, Class, {OI, AO}, OiAppliers) ->
    write_persistent(DN, IsPersistent, Persistent, Class, {OI, AO}, OiAppliers, []).

write_persistent(DN, IsPersistent, Persistent, Class, {OI, AO}, OiAppliers, ExtraAttrs) ->
    Rec = #ldap{key=DN, persistent=IsPersistent, attrs=Persistent, class=Class,
		oi=OI, ao=AO, oi_appliers=OiAppliers, extra_attrs=ExtraAttrs},
    ok = mnesia:write(imm_objects, Rec, write).

write_cached(_DN, _OI, []) -> ok;
write_cached(DN, OI, [{Key, Vals} |Cached]) ->
    ok = mnesia:write(imm_rt_cache, #cache{key={DN, Key}, oi=OI, vals=Vals}, write),
    write_cached(DN, OI, Cached);
write_cached(DN, OI, [{Key, sa_imm_attr_csstructt, Vals} |Cached]) ->
    ok = mnesia:write(imm_rt_cache, #cache{key={DN, Key},
					   struct=sa_imm_attr_csstructt,
					   oi=OI, vals=Vals}, write),
    write_cached(DN, OI, Cached).

delete_cached(DN, As) ->
    Delete = fun({Key, '$imm_db_cache'}) -> mnesia:delete({imm_rt_cache, {DN, Key}});
		(_) -> ok
	     end,
    lists:foreach(Delete, As).

%%----------------------------------------------------------------------
is_obj_persistent(config, _Schema) ->
    config;
is_obj_persistent(runtime, #imm_class{rdn=RDN, attrs=As}) ->
     #imm_rdn{flags=Flags} = RDN,
    case lists:member(persistent, Flags) of
	true ->
	    runtime;
	false ->
	    is_obj_persistent(As)
    end.

is_obj_persistent([]) ->
    false;
is_obj_persistent([#imm_attr{flags=Flags} |Rest]) ->
    case lists:member(persistent, Flags) of
	true ->
	    runtime;
	false ->
	    is_obj_persistent(Rest)
    end.

%%----------------------------------------------------------------------
validate(_DN, Attrs, Op, Type, #imm_class{attrs=As}) ->
    validate(lists:keysort(1, Attrs), lists:sort(As), Op, Type, [], [], []).

validate([], [], _Op, _Type, P, C, D) ->
    {P, C, D};
validate([Add={Key, '$imm_db_cache'} |Attrs],[#imm_attr{name=Key} |Rest], Op, OpType, P, C, D) ->
    validate(Attrs, Rest, Op, OpType, [Add |P], C, D);
validate([Add={Key, Value} |Attrs],  %%LATH TC Check that Types same
	 [#imm_attr{name=Key, type=Type, category=Category, flags=Flags} |Rest],
	 Op, OpType, Persistent, Cached, D) ->
    validate_type(Value, Type),
    multi_value_check(Key, Value, Flags),
    case Category of
	config  ->
	    %% op modified tested in update_attrs
	    Op =:= add andalso ((config =:= OpType) orelse throw({invalid_param, Key})),
	    validate(Attrs, Rest, Op, OpType, [Add |Persistent], Cached, D);
	runtime ->
	    IsCached = lists:member(cached, Flags),
	    IsPersistent = lists:member(persistent, Flags),
	    if
		%% op modified tested in update_attrs
		Op =:= add, OpType =:= config, not IsPersistent ->
		    throw({invalid_param, Key});
		IsPersistent ->
		    validate(Attrs, Rest, Op, OpType, [Add |Persistent], Cached, D);
		IsCached ->
		    validate(Attrs, Rest, Op, OpType, [{Key, '$imm_db_cache'} |Persistent], [Add |Cached], D);
		Op =:= add ->
		    throw({invalid_param, Key});
		Op =:= modify -> %% Ignore non-cached objects in modify
		    validate(Attrs, Rest, Op, OpType, [{Key, '$imm_runtime'} |Persistent], Cached, D)
	    end
    end;
validate([{Key, sa_imm_attr_csstructt, Value} |Attrs],
	 [#imm_attr{name=Key, type=Type, category=Category, flags=Flags} |Rest],
	 Op, OpType, Persistent, Cached, D) ->
    Type =:= sa_imm_attr_sanamet orelse throw({invalid_param, Key}),
    {ok, NewValue} = validate_struct(Op, Key, Value),
    multi_value_check(Key, NewValue, Flags),
    case Category of
	config  ->
	    %% op modified tested in update_attrs
	    Op =:= add andalso ((config =:= OpType) orelse throw({invalid_param, Key})),
	    validate(Attrs, Rest, Op, OpType, [{Key, sa_imm_attr_csstructt, NewValue} |Persistent], Cached, D);
	runtime ->
	    IsCached = lists:member(cached, Flags),
	    IsPersistent = lists:member(persistent, Flags),
	    if
		%% op modified tested in update_attrs
		Op =:= add, OpType =:= config, not IsPersistent ->
		    throw({invalid_param, Key});
		IsPersistent ->
		    validate(Attrs, Rest, Op, OpType, [{Key, sa_imm_attr_csstructt, NewValue} |Persistent], Cached, D);
		IsCached ->
		    validate(Attrs, Rest, Op, OpType, [{Key, '$imm_db_cache'} |Persistent], [{Key, sa_imm_attr_csstructt, NewValue} |Cached], D);
		Op =:= add ->
		    throw({invalid_param, Key});
		Op =:= modify -> %% Ignore non-cached objects in modify
		    validate(Attrs, Rest, Op, OpType, [{Key, '$imm_runtime'} |Persistent], Cached, D)
	    end
    end;
validate([{Key, AType, Value} |Attrs],  %%LATH TC Check that Types same
	 [#imm_attr{name=Key, type=Type, category=Category, flags=Flags} |Rest],
	 Op, OpType, Persistent, Cached, D) ->
    AType =:= Type orelse throw({invalid_param, {type_error, Key, AType, Type}}),
    ok =:= validate_type(Value, Type) orelse throw({invalid_param, {type_value_error, Key, Type, Value}}),
    multi_value_check(Key, Value, Flags),
    case Category of
	config  ->
	    %% op modified tested in update_attrs
	    Op =:= add andalso ((config =:= OpType) orelse throw({invalid_param, Key})),
	    validate(Attrs, Rest, Op, OpType, [{Key, Value} |Persistent], Cached, D);
	runtime ->
	    IsCached = lists:member(cached, Flags),
	    IsPersistent = lists:member(persistent, Flags),
	    if
		%% op modified tested in update_attrs
		Op =:= add, OpType =:= config, not IsPersistent ->
		    throw({invalid_param, Key});
		IsPersistent ->
		    validate(Attrs, Rest, Op, OpType, [{Key, Value} |Persistent], Cached, D);
		IsCached ->
		    validate(Attrs, Rest, Op, OpType, [{Key, '$imm_db_cache'} |Persistent], [{Key, Value} |Cached], D);
		Op =:= add ->
		    throw({invalid_param, Key});
		Op =:= modify -> %% Ignore non-cached objects in modify
		    validate(Attrs, Rest, Op, OpType, [{Key, '$imm_runtime'} |Persistent], Cached, D)
	    end
    end;
validate(Attrs, [#imm_attr{name=Key, default=undefined, category=config, flags=Flags} |Rest],
	 Op, Type, P, C, D) ->
    case Op =:= add andalso lists:member(initialized, Flags) of
	true  -> throw({invalid_param, Key}); %% Missing non optional attr
	false -> validate(Attrs, Rest, Op, Type, P, C, D)
    end;
validate(Attrs, [#imm_attr{name=Key, default=undefined, category=runtime, flags=Flags} |Rest],
	 Op, Type, P, C, D) ->
    IsCached = lists:member(cached, Flags),
    IsPersistent = lists:member(persistent, Flags),
    if
	IsCached ->
	    validate(Attrs, Rest, Op, Type, P, C, D);
%%	    validate(Attrs, Rest, Op, Type, [{Key, '$imm_db_cache'} |P], C);
	IsPersistent ->
	    validate(Attrs, Rest, Op, Type, P, C, D);
	true ->
	    validate(Attrs, Rest, Op, Type, [{Key, '$imm_runtime'} |P], C, D)
    end;
validate(Attrs, [#imm_attr{name=Key, default=Default, category=Category, flags=Flags} |Rest],
	 Op, Type, P, C, D) when Default =/= undefined ->
    case {Op, Category} of
	{add, config}  ->
	    validate(Attrs, Rest, Op, Type, [{Key, [Default]} |P], C, [{Key, [Default]} |D]);
	{add, runtime} ->
	    % IsCached = lists:member(cached, Flags),
	    IsPersistent = lists:member(persistent, Flags),
	    if
		IsPersistent ->
		    validate(Attrs, Rest, Op, Type, [{Key, [Default]} |P], C, [{Key, [Default]} |D]);
		true ->
		    throw({invalid_param, Key})
		end;
	_ ->
	    validate(Attrs, Rest, Op, Type, P, C, D)
    end;
%% ADD check for category , no default value on runtime (nocached)
% validate([], Attrs, Op, _Type, P, C) ->
%     HaveInit = fun(#imm_attr{category=config, flags=Flags}) ->
% 		       Op =:= add andalso lists:member(initialized, Flags);
% 		  (#imm_attr{category=runtime}) -> false
% 	       end,
%     %% Missing non optional attr
%     Failed = lists:filter(HaveInit, Attrs),
%     [] =:= Failed orelse throw({invalid_param,[Attr#imm_attr.name || Attr <- Failed]}),
%     {P, C};
validate(Attrs=[_|_], _AttrDefs, _Op, _Type, _P, _C, _D) ->
    %% Attr not in class def
    throw({invalid_param, [Key || {Key,_} <- Attrs]}).

% validate_type(Value, sa_imm_attr_saint32t) when is_integer(Value) -> ok;
% validate_type(Value, sa_imm_attr_sauint32t) when is_integer(Value) -> ok;
% validate_type(Value, sa_imm_attr_saint64t) when is_integer(Value) -> ok;
% validate_type(Value, sa_imm_attr_sauint64t) when is_integer(Value) -> ok;
% validate_type(Value, sa_imm_attr_sastringt)when  is_binary(Value) -> ok;
% validate_type(Value, sa_imm_attr_sanamet) when is_binary(Value) -> ok;
% validate_type(Value, sa_imm_attr_satimet) when is_integer(Value) -> ok;
% validate_type(Value, sa_imm_attr_safloatt) when is_float(Value) -> ok;
% validate_type(Value, sa_imm_attr_sadoublet) when is_float(Value) -> ok;
% validate_type(Value, sa_imm_attr_saanyt) when is_binary(Value) -> ok;
validate_type(_,_) -> ok.

%%----------------------------------------------------------------------
validate_struct(modify, Key, Values) ->
    validate_struct_modify(Key, Values),
    {ok, Values};
validate_struct(add, Key, Values) ->
    NewValues = validate_struct_add(Key, Values, []),
    %%io:format("NewValues:\n~p\n", [NewValues]),
    {ok, NewValues}.


validate_struct_modify(_, []) ->
    ok;
validate_struct_modify(Key, [#safsImmCsStruct{structName=Name, structMembers=Members}| Values]) ->
    #imm_class{attrs=DefinedAttrs} = get_schema(Name),
    validate_struct_members_modify(Key, Members, DefinedAttrs),
    validate_struct_modify(Key, Values);
validate_struct_modify(Key, _) ->
    throw({invalid_param, Key}).

validate_struct_members_modify(_, [] , _) ->
    ok;
validate_struct_members_modify(Key, [#safsImmAttrValues_2{attrName=MName,
							  attrValueType=MType} |
				     Members], DefinedAttrs) ->
    case lists:keyfind(erlang:binary_to_atom(MName, utf8), 2, DefinedAttrs) of
	#imm_attr{type=MType} ->
	    validate_struct_members_modify(Key, Members, DefinedAttrs);
	false ->
	    throw({invalid_param, Key})
    end.

validate_struct_add(_, [], Acc) ->
    lists:reverse(Acc);
validate_struct_add(Key, [#safsImmCsStruct{structName=Name, structMembers=Members}| Values], Acc) ->
    #imm_class{attrs=DefinedAttrs} = get_schema(Name),
    NewMembers = validate_struct_members_add(Key,
					     lists:sort(v_add_name_key(Members, [])),
					     lists:sort(DefinedAttrs), []),
    validate_struct_add(Key, Values, [#safsImmCsStruct{structName=Name, structMembers=NewMembers} |Acc]);
validate_struct_add(Key, _, _) ->
    throw({invalid_param, Key}).

v_add_name_key([], Acc) ->
    Acc;
v_add_name_key([#safsImmAttrValues_2{attrName=Name} = M |Ms], Acc) ->
    v_add_name_key(Ms, [{erlang:binary_to_atom(Name, utf8), M} |Acc]).

validate_struct_members_add(_, [] , [], Acc) ->
    lists:reverse(Acc);
validate_struct_members_add(Key,
			    [{Name , #safsImmAttrValues_2{attrValueType=MType} = Member}
			     |Members],
			    [#imm_attr{name=Name, type=Type, category=_Category, flags=_Flags}
			     |DefinedAttrs],
			    Acc) ->
    MType =:= Type orelse throw({invalid_param, Key}), %%{type_error, Key, MName, AType, Type}}),
    %%io:format("Value used: ~p\n", [Member]),
    validate_struct_members_add(Key, Members, DefinedAttrs, [Member |Acc]);
validate_struct_members_add(Key, Members,
			    [#imm_attr{name=_Name, default=undefined, type=_Type, category=_Category, flags=_Flags}
			     |DefinedAttrs],
			    Acc) ->
    %%io:format("No DefaultValue: ~p\n", [Name]),
    validate_struct_members_add(Key, Members, DefinedAttrs, Acc);
validate_struct_members_add(Key, Members,
			    [#imm_attr{name=Name, default=Default, type=Type, category=Category, flags=Flags}
			     |DefinedAttrs],
			    Acc)  when Default =/= undefined ->
    %%io:format("DefaultValue used: ~p\n", [{Name, Type, Default}]),
    case Category of
	config ->
	    validate_struct_members_add(Key, Members, DefinedAttrs,
					[#safsImmAttrValues_2{attrName=atom_to_binary(Name, utf8),
							      attrValueType=Type,
							      attrValuesNumber=1,
							      attrValues=[safs_imm_lib:set_attr_value(Type, Default)]} |Acc]);
	runtime ->
	    IsPersistent = lists:member(persistent, Flags),
	    if
		IsPersistent ->
		    validate_struct_members_add(Key, Members, DefinedAttrs,
					[#safsImmAttrValues_2{attrName=atom_to_binary(Name, utf8),
							      attrValueType=Type,
							      attrValuesNumber=1,
							      attrValues=[safs_imm_lib:set_attr_value(Type, Default)]} |Acc]);
		true ->
		    validate_struct_members_add(Key, Members, DefinedAttrs, Acc)
	    end
    end;
validate_struct_members_add(Key, _Members=[_|_], _DefinedAttrs, _Acc) ->
    throw({invalid_param, Key}). %%[Key || {Key,_} <- Attrs]}).


%%----------------------------------------------------------------------
multi_value_check(Key, Value, Flags) when length(Value) > 1 ->
    case lists:member(multi_value, Flags) of
	false ->
	    throw({invalid_param, Key});
	true ->
	    ok
    end;
multi_value_check(_Key, _Value, _Flags) ->
    ok.

%%----------------------------------------------------------------------
return_attrs(Objs, Attr, DN, FailAttrNotExist) ->
    return_attrs(Objs, Attr, DN, FailAttrNotExist, transaction).

return_attrs(Objs, no, _, _FailAttrNotExist, _) ->
    [key_to_bin(Key)|| #ldap{key=Key} <- Objs];
return_attrs(Objs, config, _DN, FailAttrNotExist, _) ->
    [{key_to_bin(Key), Class,
      select_attrs(return_attrs(Rec), config, Class, FailAttrNotExist)} ||
	Rec = #ldap{key=Key, class=Class} <- Objs];
return_attrs(Objs, persistent, _DN, FailAttrNotExist, _) ->
     [{key_to_bin(Key), Class,
      select_attrs(return_attrs(Rec), persistent, Class, FailAttrNotExist)} ||
	Rec = #ldap{key=Key, class=Class} <- Objs];
return_attrs(Objs, all, DN, FailAttrNotExist, Transaction) ->
    [{key_to_bin(Key), Class,
      fetch_cache(select_attrs(return_attrs(Rec), all, Class, FailAttrNotExist), DN, Transaction)} ||
	Rec = #ldap{key=Key, class=Class} <- Objs];
return_attrs(Objs, {some, Included}, DN, FailAttrNotExist, Transaction) ->
    [{key_to_bin(Key), Class,
      fetch_cache(select_attrs(return_attrs(Rec), Included, Class, FailAttrNotExist), DN, Transaction)} ||
	Rec = #ldap{key=Key, class=Class} <- Objs].

return_attrs(#ldap{key=Key, attrs=Attrs, class=Class, ao=undefined, oi=OI0, extra_attrs=ExtraAttrs}) ->
    NewAttrs =
	case return_attrs_1(Class, OI0) of
	    undefined ->
		[{'SaImmAttrClassName', [unicode:characters_to_binary(atom_to_list(Class))]},
		 get_rdn_attr_from_key(Key)
		 |Attrs];
	    OI ->
		[{'SaImmAttrClassName', [unicode:characters_to_binary(atom_to_list(Class))]},
		 {'SaImmAttrImplementerName', [OI]},
		 get_rdn_attr_from_key(Key)
		 |Attrs]
	end,
    {NewAttrs, ExtraAttrs};
return_attrs(#ldap{key=Key, attrs=Attrs, class=Class, ao={AO,_}, oi=OI0, extra_attrs=ExtraAttrs}) ->
    NewAttrs =
	case return_attrs_1(Class, OI0) of
	    undefined ->
		[{'SaImmAttrClassName', [unicode:characters_to_binary(atom_to_list(Class))]},
		 {'SaImmAttrAdminOwnerName', [AO]},
		 get_rdn_attr_from_key(Key)
		 |Attrs];
	    OI ->
		[{'SaImmAttrClassName', [unicode:characters_to_binary(atom_to_list(Class))]},
		 {'SaImmAttrImplementerName', [OI]},
		 {'SaImmAttrAdminOwnerName', [AO]},
		 get_rdn_attr_from_key(Key)
		 |Attrs]
	end,
    {NewAttrs, ExtraAttrs}.

return_attrs_1(Class, undefined) ->
    [#imm_class{oi=OI}] = mnesia:dirty_read(imm_class, Class),
    OI;
return_attrs_1(_Class, {OI,_}) ->
    OI.

%%----------------------------------------------------------------------
get_rdn_attr_from_key(Key) ->
    [RDN |_] = lists:reverse(Key),
    [Name, Value] = binary:split(RDN, <<"=">>),
    {erlang:binary_to_atom(Name, utf8), [Value]}.


%%----------------------------------------------------------------------
select_attrs({Attrs, ExtraAttrs}, all, ClassName, _FailAttrNotExist) ->
    {_, AttrDefs} = get_class_def(ClassName),
    DefinedAttrs0 = [AttrName || {AttrName, _, _, _, _}  <- AttrDefs],
    ReturnAttrs = ['SaImmAttrClassName', 'SaImmAttrImplementerName', 'SaImmAttrAdminOwnerName'
		   | DefinedAttrs0],
    select_attrs_1(lists:keysort(1, Attrs), lists:sort(ReturnAttrs), []) ++ ExtraAttrs;
select_attrs({Attrs, ExtraAttrs}, config, ClassName, _FailAttrNotExist) ->
    {_, AttrDefs} = get_class_def(ClassName),
    ConfigAttrs =
	lists:filtermap(fun({AttrName, _, config, _, _}) ->
				{true, AttrName};
			   (_) ->
				false
			end , AttrDefs),
    ReturnAttrs = ['SaImmAttrClassName', 'SaImmAttrImplementerName', 'SaImmAttrAdminOwnerName'
		   | ConfigAttrs],
    select_attrs_1(lists:keysort(1, Attrs), lists:sort(ReturnAttrs), []) ++ ExtraAttrs;
select_attrs({Attrs, ExtraAttrs}, persistent, ClassName, _FailAttrNotExist) ->
    {_, AttrDefs} = get_class_def(ClassName),
    PersistentAttrs =
	lists:filtermap(fun({AttrName, _, config, _, _}) ->
				{true, AttrName};
			   ({AttrName, _, runtime, Flags, _}) ->
				case lists:member(persistent, Flags) of
				    true ->
					{true, AttrName};
				    false ->
					false
				end;
			   (_) ->
				false
			end , AttrDefs),
    ReturnAttrs = ['SaImmAttrClassName', 'SaImmAttrImplementerName', 'SaImmAttrAdminOwnerName'
		   | PersistentAttrs],
    select_attrs_1(lists:keysort(1, Attrs), lists:sort(ReturnAttrs), []) ++ ExtraAttrs;
select_attrs({Attrs, ExtraAttrs}, List, ClassName, FailAttrNotExist) ->
    {_, AttrDefs} = get_class_def(ClassName),
    DefinedAttrs0 = [AttrName || {AttrName, _, _, _, _}  <- AttrDefs],
    DefinedAttrs = ['SaImmAttrClassName', 'SaImmAttrImplementerName', 'SaImmAttrAdminOwnerName'
		    | DefinedAttrs0],

    {AttrList, CheckedExtraAttrList} = lists:foldl(fun('RcsImmAttrObjId', {A, E}) -> {A,['RcsImmAttrObjId' |E]};
					       ('RcsImmAttrEcimDn', {A, E}) -> {A, ['RcsImmAttrEcimDn' |E]};
					       (X, {A, E}) -> {[X |A], E}
					    end,
					    {[],[]}, List),
    ReturnExtraAttrs = select_attrs_1(lists:keysort(1, ExtraAttrs), lists:sort(CheckedExtraAttrList), []),
    CheckedAttrList =
	lists:filtermap(fun(Elem) -> case lists:member(Elem, DefinedAttrs) of
					 true -> true;
					 false -> select_attrs_exit(FailAttrNotExist)
				     end
			end , AttrList),
    ReturnAttrs = select_attrs_1(lists:keysort(1, Attrs), lists:sort(CheckedAttrList), []),
    ReturnAttrs ++ ReturnExtraAttrs.


% select_attrs({Attrs, ExtraAttrs}, Included, ClassName, FailAttrNotExist) ->
%     {_, AttrDefs} = get_class_def(ClassName),
%     DefinedAttrs0 = [AttrName || {AttrName, _, _, _, _}  <- AttrDefs],

%     ReturnAttrs =
% 	case Included of
% 	    all ->
% 		['SaImmAttrClassName', 'SaImmAttrImplementerName', 'SaImmAttrAdminOwnerName',
% 		 'RcsImmAttrObjId', 'RcsImmAttrEcimDn' | DefinedAttrs0];
% 	    config ->
% 		ConfigAttrs =
% 		    lists:filtermap(fun({AttrName, _, config, _, _}) ->
% 					    {true, AttrName};
% 				       (_) ->
% 					    false
% 				    end , AttrDefs),
% 		['SaImmAttrClassName', 'SaImmAttrImplementerName', 'SaImmAttrAdminOwnerName',
% 		 'RcsImmAttrObjId', 'RcsImmAttrEcimDn' | ConfigAttrs];
% 	    persistent ->
% 		PersistentAttrs =
% 		    lists:filtermap(fun({AttrName, _, config, _, _}) ->
% 					    {true, AttrName};
% 				       ({AttrName, _, runtime, Flags, _}) ->
% 					    case lists:member(persistent, Flags) of
% 						true ->
% 						    {true, AttrName};
% 						false ->
% 						    false
% 					    end;
% 				       (_) ->
% 					    false
% 				    end , AttrDefs),
% 		['SaImmAttrClassName', 'SaImmAttrImplementerName', 'SaImmAttrAdminOwnerName',
% 		 'RcsImmAttrObjId', 'RcsImmAttrEcimDn' | PersistentAttrs];
% 	    List ->
% 		DefinedAttrs = ['SaImmAttrClassName', 'SaImmAttrImplementerName',
% 				'SaImmAttrAdminOwnerName', 'RcsImmAttrObjId', 'RcsImmAttrEcimDn' | DefinedAttrs0],
% 		%io:format("~p:~p\n",[DefinedAttrs, List]),
% 		lists:filtermap(fun(Elem) -> case lists:member(Elem, DefinedAttrs) of
% 						 true -> true;
% 						 false -> select_attrs_exit(FailAttrNotExist)
% 					     end
% 				end , List)
% 	end,
%     select_attrs_1(lists:keysort(1, Attrs), lists:sort(ReturnAttrs), []).

select_attrs_exit(true) ->
    throw(no_exists);
select_attrs_exit(false) ->
    false.

select_attrs_1([], [Key |Included], Acc) ->
    select_attrs_1([], Included, [{Key, []} |Acc]);
select_attrs_1(_, [], Acc) ->
    Acc;
select_attrs_1([{Key, _} = A |Attrs], [Key |Included], Acc) ->
    select_attrs_1(Attrs, Included, [A |Acc]);
select_attrs_1([{Key, _, _} = A |Attrs], [Key |Included], Acc) ->
    select_attrs_1(Attrs, Included, [A |Acc]);
select_attrs_1([{Key1, _} |Attrs], [Key2 |Included], Acc) when Key1 < Key2 ->
    select_attrs_1(Attrs, [Key2 |Included], Acc);
select_attrs_1([{Key1, _, _} |Attrs], [Key2 |Included], Acc) when Key1 < Key2 ->
    select_attrs_1(Attrs, [Key2 |Included], Acc);
select_attrs_1([{Key1, _} = A |Attrs], [Key2 |Included], Acc) when Key1 > Key2 ->
    select_attrs_1([A |Attrs], Included, [{Key2, []} |Acc]);
select_attrs_1([{Key1, _, _} = A |Attrs], [Key2 |Included], Acc) when Key1 > Key2 ->
    select_attrs_1([A |Attrs], Included, [{Key2, []} |Acc]).

%%----------------------------------------------------------------------
fetch_cache([{Key, '$imm_db_cache'}|Vs], DN, transaction) ->
    case mnesia:read({imm_rt_cache, {DN, Key}}) of
	[] ->
	    %% [{Key, '$imm_runtime'} | fetch_cache(Vs, DN, transaction)];
	    [{Key, []} | fetch_cache(Vs, DN, transaction)];
	[#cache{vals=Vals, struct=sa_imm_attr_csstructt}] ->
	    [{Key, sa_imm_attr_csstructt, Vals} | fetch_cache(Vs, DN, transaction)];
	[#cache{vals=Vals}] ->
	    [{Key, Vals} | fetch_cache(Vs, DN, transaction)]
    end;
fetch_cache([{Key, '$imm_db_cache'}|Vs], DN, dirty) ->
    case mnesia:dirty_read({imm_rt_cache, {DN, Key}}) of
	[] ->
	    %% [{Key, '$imm_runtime'} | fetch_cache(Vs, DN, dirty)];
	    [{Key, []} | fetch_cache(Vs, DN, dirty)];
	[#cache{vals=Vals, struct=sa_imm_attr_csstructt}] ->
	    [{Key, sa_imm_attr_csstructt, Vals} | fetch_cache(Vs, DN, dirty)];
	[#cache{vals=Vals}] ->
	    [{Key, Vals} | fetch_cache(Vs, DN, dirty)]
    end;
fetch_cache([RT={_Key, '$imm_runtime'}|Vs], DN, Transaction) ->
    [RT | fetch_cache(Vs, DN, Transaction)];
fetch_cache([KV = {_,_}|Vs], DN, Transaction) ->
    [KV|fetch_cache(Vs, DN, Transaction)];
fetch_cache([KV = {_,_,_}|Vs], DN, Transaction) ->
    [KV|fetch_cache(Vs, DN, Transaction)];
fetch_cache([], _, _) -> [].

%%----------------------------------------------------------------------
check_no_exists(DN, none) ->
    [] =:= mnesia:dirty_read(imm_objects, DN) orelse throw(already_exists);
check_no_exists(DN, Context) ->
    case gb_trees:lookup(DN, Context) of
	none ->
	    [] =:= mnesia:dirty_read(imm_objects, DN) orelse throw(already_exists);
	{value, #entry{op=del}} ->
	    %% Since it's only for error checking no need to store previous ops
	    true;
	{value, _} ->
	    throw(already_exists)
    end.

%%----------------------------------------------------------------------
read_dn(DN, none) ->
    case mnesia:dirty_read(imm_objects, DN) of
    	[] -> throw(no_exists);
    	[Res]  -> Res
    end;
read_dn(DN, Context) ->
    case gb_trees:lookup(DN, Context) of
	none ->
	     % case mnesia:transaction(fun() -> mnesia:read(imm_objects, DN) end) of % LATH: Temporary fix until the transaction handling corrected
	     % 	{aborted, _Reason} -> throw(no_exists);
	     % 	{atomic, []} -> throw(no_exists);
	     % 	{atomic, [Res]}  -> Res
	     % end;
	    case mnesia:dirty_read(imm_objects, DN) of
	    	[] -> throw(no_exists);
	     	[Res]  -> Res
	    end;
	{value, #entry{op=del}}  -> throw(no_exists);
	{value, #entry{val=Val}} -> Val
    end.

%%----------------------------------------------------------------------
read_subtree_dn(MS, DN, Context) ->
    % Get = fun(Stored = #ldap{key=SubTreeDN}, Acc) ->
    % 			  case gb_trees:lookup(SubTreeDN, Context) of
    % 			      none -> [Stored|Acc];
    % 			      {value, #entry{op=del}}  -> Acc;
    % 			      {value, #entry{val=Val}} -> [Val|Acc]
    % 			  end
    % 	  end,
    Stored = mnesia:dirty_select(imm_objects, MS),
    MS2 = [{{DN,'$2'},[],['$_']}],
    CMS = ets:match_spec_compile(MS2),

    case Context of
	none ->
	    Stored;
	_ ->
	    StoredCtx = ets:match_spec_run(gb_trees:to_list(Context), CMS),
	    Get2 =
		fun({DN0, #entry{op=del}}, Acc) -> lists:keydelete(DN0, #ldap.key, Acc);
		   ({DN0, #entry{op=write, val=Val}}, Acc) -> keystore(DN0, Acc, Val)
		end,
	    lists:foldr(Get2, Stored, StoredCtx)
    end.

%%----------------------------------------------------------------------
keystore(_Key, [], Elem) ->
    [Elem];
keystore(Key, [#ldap{key=HeadKey} = Head |List], Elem) ->
    case HeadKey of
	HK when HK < Key ->
	    [Head | keystore(Key, List, Elem)];
	Key ->
	    [Elem | List];
	HK when HK > Key  ->
	    [Elem, Head |List]
    end.

%%----------------------------------------------------------------------
set_context(_DN, _Entry, none) -> none;
set_context(DN, Entry, Context) ->
    gb_trees:enter(DN, Entry, Context).

%%----------------------------------------------------------------------
subtree_oi_ao(DN, none) ->
    subtree_oi_ao(mnesia:dirty_read(imm_objects, DN));
subtree_oi_ao(DN, Context) ->
    case gb_trees:lookup(DN, Context) of
	{value, #entry{op=del}} ->
	    {undefined, undefined};
	{value, #entry{val=LDap}} ->
	    subtree_oi_ao([LDap]);
	none ->
	    subtree_oi_ao(DN, none)
    end.

subtree_oi_ao([#ldap{oi=OI, ao=AO}]) -> {subtree(OI),subtree(AO)};
subtree_oi_ao([]) -> {undefined, undefined}.

subtree({_, one}) -> undefined;
subtree({Name, sublevel}) -> {Name, one};
subtree(Subtree) -> Subtree.


%%----------------------------------------------------------------------
verify_privilege(Object, #ldap{oi=OI}, runtime) -> verify_privilege_oi(Object, OI);
verify_privilege(Object, #ldap{ao=AO}, config)  -> verify_privilege_ao(Object, AO).

verify_privilege_oi(Object, {Object, _}) -> true;
verify_privilege_oi(_, undefined) -> true;
verify_privilege_oi(_, {_,_}) -> false.

verify_privilege_ao(Object, {Object, _}) -> true;
verify_privilege_ao(_, undefined) -> false;
verify_privilege_ao(_, {_,_}) -> false.

%%----------------------------------------------------------------------
check_owner_not_changed(undefined, undefined) ->
    true;
check_owner_not_changed({Owner, _}, {Owner, _}) ->
    true;
check_owner_not_changed(_, _) ->
    false.

check_objs_not_changed([], []) ->
    ok;
check_objs_not_changed([], _Objects) ->
    % case check_if_rt_objs(Objects) of
    % 	true ->
    % 	    ok;
    % 	false ->
    throw({object_changed, obj_add_or_rm,  undefined});
    % end;
check_objs_not_changed(_Objects, []) ->
    % case check_if_rt_objs(Objects) of
    % 	true ->
    % 	    ok;
    % 	false ->
    throw({object_changed, obj_add_or_rm, undefined});
    % end;
check_objs_not_changed([#ldap{key=DN, class=Class, attrs=Attrs1, oi=OI, ao=AO} |Objs],
		       [#ldap{key=DN, attrs=Attrs2, oi=OI2, ao=AO2} |Objs2]) ->
    case {check_owner_not_changed(OI, OI2),
	  check_owner_not_changed(AO, AO2)} of
	{true, true} ->
	    ObjDef = get_schema(Class),
	    case check_attrs_not_changed(ObjDef#imm_class.attrs, Attrs1, Attrs2) of
		{true, _} ->
		    check_objs_not_changed(Objs, Objs2);
		{false, _} ->
		    throw({object_changed, attrs, key_to_bin(DN)})
	    end;
	{false, true} ->
	    throw({object_changed, oi, key_to_bin(DN)});
	{true, false} ->
	    throw({object_changed, ao, key_to_bin(DN)});
	{false, false} ->
	    throw({object_changed, oi_and_ao, key_to_bin(DN)})
    end;
check_objs_not_changed([#ldap{key=DN1} = _Obj1  | _Rest1] = _Objects1,
		       [#ldap{key=DN2} = _Obj2 | _Rest2] = _Objects2) when DN1 =/= DN2 ->
    throw({object_changed, obj_add_or_rm, undefined}).

check_attrs_not_changed(Schema, OldAttrs, CurrentAttrs) ->
    check_attrs_not_changed(Schema, lists:sort(OldAttrs), lists:sort(CurrentAttrs), []).


check_attrs_not_changed(_, [], [], Acc) ->
    {true, Acc};
check_attrs_not_changed(AttrDefs, [{N, _}=A |Rest1], [A |Rest2], Acc) ->
    AttrDef = lists:keyfind(N, 2, AttrDefs),
    case AttrDef#imm_attr.category of
	runtime ->
	    check_attrs_not_changed(AttrDefs, Rest1, Rest2, [A | Acc]);
	config ->
	    check_attrs_not_changed(AttrDefs, Rest1, Rest2, Acc)
    end;
%% Structs
check_attrs_not_changed(AttrDefs, [{N, _, _}=A |Rest1], [A |Rest2], Acc) ->
    AttrDef = lists:keyfind(N, 2, AttrDefs),
    case AttrDef#imm_attr.category of
	runtime ->
	    check_attrs_not_changed(AttrDefs, Rest1, Rest2, [A | Acc]);
	config ->
	    check_attrs_not_changed(AttrDefs, Rest1, Rest2, Acc)
    end;
check_attrs_not_changed(AttrDefs, [{N, _} |Rest1], [{N, _}=A2 |Rest2], Acc) ->
    AttrDef = lists:keyfind(N, 2, AttrDefs),
    case AttrDef#imm_attr.category of
	runtime ->
	    check_attrs_not_changed(AttrDefs, Rest1, Rest2, [A2 |Acc]);
	config ->
	    {false, []}
    end;
%% Structs
check_attrs_not_changed(AttrDefs, [{N, _, _} |Rest1], [{N, _, _}=A2 |Rest2], Acc) ->
    AttrDef = lists:keyfind(N, 2, AttrDefs),
    case AttrDef#imm_attr.category of
	runtime ->
	    check_attrs_not_changed(AttrDefs, Rest1, Rest2, [A2 |Acc]);
	config ->
	    {false, []}
    end;
check_attrs_not_changed(AttrDefs, [{N1, _} |Rest1], [{N2, _} |_Rest2] = Attrs2, Acc) when N1 < N2 ->
    AttrDef1 = lists:keyfind(N1, 2, AttrDefs),
    AttrDef2 = lists:keyfind(N2, 2, AttrDefs),
    case {AttrDef1#imm_attr.category, AttrDef2#imm_attr.category} of
	{config, config} ->
	    {false, []};
	{runtime, runtime} ->
	    check_attrs_not_changed(AttrDefs, Rest1, Attrs2, Acc);
	{runtime, config} ->
	    check_attrs_not_changed(AttrDefs, Rest1, Attrs2, Acc);
	{config, runtime} ->
	    {false, []}
	    % check_attrs_not_changed(AttrDefs, Attrs1, Rest2, Acc)
    end;
%% Structs
check_attrs_not_changed(AttrDefs, [{N1, _, _} |Rest1], [{N2, _, _} |_Rest2] = Attrs2, Acc) when N1 < N2 ->
    AttrDef1 = lists:keyfind(N1, 2, AttrDefs),
    AttrDef2 = lists:keyfind(N2, 2, AttrDefs),
    case {AttrDef1#imm_attr.category, AttrDef2#imm_attr.category} of
	{config, config} ->
	    {false, []};
	{runtime, runtime} ->
	    check_attrs_not_changed(AttrDefs, Rest1, Attrs2, Acc);
	{runtime, config} ->
	    check_attrs_not_changed(AttrDefs, Rest1, Attrs2, Acc);
	{config, runtime} ->
	    {false, []}
	    % check_attrs_not_changed(AttrDefs, Attrs1, Rest2, Acc)
    end;
check_attrs_not_changed(AttrDefs, [{N1, _} |_Rest1] = Attrs1, [{N2, _}=A2 |Rest2], Acc) when N1 > N2 ->
    AttrDef1 = lists:keyfind(N1, 2, AttrDefs),
    AttrDef2 = lists:keyfind(N2, 2, AttrDefs),
    case {AttrDef1#imm_attr.category, AttrDef2#imm_attr.category} of
	{config, config} ->
	    {false, []};
	{runtime, runtime} ->
	    check_attrs_not_changed(AttrDefs, Attrs1, Rest2, [A2 |Acc]);
	{runtime, config} ->
	    {false, []};
	{config, runtime} ->
	    check_attrs_not_changed(AttrDefs, Attrs1, Rest2, [A2 |Acc])
    end;
%% Structs
check_attrs_not_changed(AttrDefs, [{N1, _, _} |_Rest1] = Attrs1, [{N2, _, _}=A2 |Rest2], Acc) when N1 > N2 ->
    AttrDef1 = lists:keyfind(N1, 2, AttrDefs),
    AttrDef2 = lists:keyfind(N2, 2, AttrDefs),
    case {AttrDef1#imm_attr.category, AttrDef2#imm_attr.category} of
	{config, config} ->
	    {false, []};
	{runtime, runtime} ->
	    check_attrs_not_changed(AttrDefs, Attrs1, Rest2, [A2 |Acc]);
	{runtime, config} ->
	    {false, []};
	{config, runtime} ->
	    check_attrs_not_changed(AttrDefs, Attrs1, Rest2, [A2 |Acc])
    end;
check_attrs_not_changed(AttrDefs, [{N, _} |Rest1], [], Acc) ->
    AttrDef = lists:keyfind(N, 2, AttrDefs),
    case AttrDef#imm_attr.category of
	runtime ->
	    check_attrs_not_changed(AttrDefs, Rest1, [], Acc);
	config ->
	    {false, []}
    end;
%% Structs
check_attrs_not_changed(AttrDefs, [{N, _, _} |Rest1], [], Acc) ->
    AttrDef = lists:keyfind(N, 2, AttrDefs),
    case AttrDef#imm_attr.category of
	runtime ->
	    check_attrs_not_changed(AttrDefs, Rest1, [], Acc);
	config ->
	    {false, []}
    end;
check_attrs_not_changed(AttrDefs, [], [{N, _}=A2 |Rest2], Acc) ->
    AttrDef = lists:keyfind(N, 2, AttrDefs),
    case AttrDef#imm_attr.category of
	runtime ->
	    check_attrs_not_changed(AttrDefs, [], Rest2, [A2 |Acc]);
	config ->
	    {false, []}
    end;
%% Structs
check_attrs_not_changed(AttrDefs, [], [{N, _, _}=A2 |Rest2], Acc) ->
    AttrDef = lists:keyfind(N, 2, AttrDefs),
    case AttrDef#imm_attr.category of
	runtime ->
	    check_attrs_not_changed(AttrDefs, [], Rest2, [A2 |Acc]);
	config ->
	    {false, []}
    end.

check_class_not_changed(#imm_class{name=N, rdn=R,  category=C, attrs=A, oi=O},
			#imm_class{name=N, rdn=R,  category=C, attrs=A, oi=O1}) ->
    O =:= O1 orelse throw({class_changed, oi, N}),
    ok;
check_class_not_changed(#imm_class{name=N}, _) ->
    throw({class_changed, definition, N}).

%%----------------------------------------------------------------------
get_key(Object) ->
    Object#ldap.key.

mk_key(Key) -> mk_key(Key, []).

mk_key(<<>>, Tail) ->
    Tail;
mk_key(Key, Tail) when is_list(Key) ->
    mk_key(unicode:characters_to_binary(Key), Tail);
mk_key(Key, Tail) when is_binary(Key) ->
    DN = binary:split(Key, <<$,>>, [global]),
    lists:reverse(DN, Tail).

key_to_bin(Path) ->
    key_to_bin(lists:reverse(Path), <<>>).
key_to_bin([Root|Rest = [_|_]], Acc) ->
    key_to_bin(Rest, <<Acc/binary, Root/binary, $,>>);
key_to_bin([Root], Acc) ->
    <<Acc/binary, Root/binary>>.

mk_search(Key, one) when is_binary(Key) ->
    [lists:reverse(binary:split(Key, <<$,>>, [global]))];
mk_search(<<>>, sublevel) ->  [['_']];
mk_search(Key, sublevel) when is_binary(Key) ->
    KeyList = binary:split(Key, <<$,>>, [global]),
    [lists:reverse(KeyList, ['_']), lists:reverse(KeyList)];
mk_search(<<>>, subtree) ->   ['_'];
mk_search(Key, subtree) when is_binary(Key) ->
    [lists:reverse(binary:split(Key, <<$,>>, [global]), '_')];
mk_search(Key, Scope) when is_list(Key) ->
    mk_search(unicode:characters_to_binary(Key), Scope).

%%----------------------------------------------------------------------
error_msg({type_error, Key, Got, ShouldBe}) ->
    lists:flatten(io_lib:format("wrong type on ~p, got ~p should be ~p", [Key, Got, ShouldBe]));
error_msg({type_value_error, Key, Type, Value}) ->
    lists:flatten(io_lib:format("the value ~p for ~p is not of type ~p", [Value, Key, Type]));
error_msg( _) ->
    ok.

%%----------------------------------------------------------------------
clean_rt_data_for_oi_cold_restart(ImplementerName) ->
    CleanFun =
	fun() ->
		Objs = mnesia:match_object(imm_objects, #ldap{oi={ImplementerName,'_'},
							      persistent=false, _='_'}, write),

		Del = fun(Obj, {DelFunList, CbFunList}) ->
			      ObjectName = safs_imm_db:key_to_bin(Obj#ldap.key),
			      {ok, ObjectClassName} = safs_imm_db:get_class(ObjectName),
			      DelFun = safs_imm_db:rt_delete(ImplementerName, ObjectName),
			      %% Callbacks to Special Appliers
			      {_, AttrDefs} = safs_imm_db:get_class_def(ObjectClassName),
			      case safs_imm_lib:get_notifiable_attrs(AttrDefs) of
				  [] ->
				      {[DelFun |DelFunList], CbFunList};
				  _ ->
				      CbFun =
					  fun(Applier) ->
						  safs_imm_oi:handle_callback(Applier, {ccb_object_delete, {0, ObjectName}})
					  end,
				      {[DelFun |DelFunList], [CbFun |CbFunList]}
			      end
		      end,
		{DelFuns, CbFuns} = lists:foldl(Del, {[],[]}, Objs),
		[DelFun() || DelFun <- DelFuns],
		CacheObjs = mnesia:match_object(imm_rt_cache, #cache{oi=ImplementerName, _='_'}, write),
		[mnesia:delete(imm_rt_cache, CacheObj#cache.key, write) || CacheObj <- CacheObjs],
	        {ok, CbFuns}
	end,
    case mnesia:transaction(CleanFun) of
	{atomic, {ok, CbFuns}} ->
	    call_special_appliers(CbFuns, safs_imm_oi:get_special_appliers_if_registered());
	{aborted, Reason} ->
	    safs_error(clean_rt_data_for_oi_cold_restart,  {" Clean Transaction failed:", {aborted, Reason}}),
	    ok
    end.

call_special_appliers([], _) ->
    ok;
call_special_appliers([CbFun |CbFuns], Appliers) ->
    [CbFun(Applier) || Applier <- Appliers],
    call_special_appliers(CbFuns, Appliers).

%%----------------------------------------------------------------------
clean_rt_data_for_oi_warm_restart(ImplementerName) ->
    CleanFun =
	fun() ->
		Objs = mnesia:match_object(imm_objects, #ldap{oi={ImplementerName,'_'},
							      persistent=false, _='_'}, write),
		F = fun(DN) ->
			    mnesia:delete(imm_objects, DN, write),
			    delete_class_index(DN),
			    delete_reverse_refs(DN)
		    end,
		[F(Obj#ldap.key) || Obj <- Objs],
		CacheObjs = mnesia:match_object(imm_rt_cache, #cache{oi=ImplementerName, _='_'}, write),
		[mnesia:delete(imm_rt_cache, CacheObj#cache.key, write) || CacheObj <- CacheObjs]
	end,
    mnesia:transaction(CleanFun).

%%====================================================================
%%  Startup and Initialization
%%====================================================================
internal_init() ->
    %% Initial prototype code
    Nodes = [node()],
    _ = mnesia:create_schema(Nodes),
    case application:start(mnesia) of
	ok -> ok;
	{error, {already_started, mnesia}} -> ok
    end,
    system_up(undefined).

system_up(XmlConfig) ->
    MeOnly = [node()],
    Tabs = mnesia:system_info(tables),
    TablesCreated = lists:member(imm_rt_cache, Tabs),
    case mnesia:system_info(running_db_nodes) of
	MeOnly ->
	    setup_mnesia(true, TablesCreated, MeOnly, XmlConfig);
	Nodes ->
	    Tabs = mnesia:system_info(tables),
	    case TablesCreated of
		true ->
		    case mnesia:wait_for_tables(tables(), 10000) of
			ok -> ok;
			_ -> system_up(XmlConfig)
		    end;
		false ->
		    setup_mnesia(false, TablesCreated, Nodes, XmlConfig)
	    end
    end.

setup_mnesia(true, true, _Nodes, Config={_Cs, Objects}) ->
    case mnesia:wait_for_tables(tables(), 10000) of
	ok ->
	    case lists:keysearch('SaImmMngt', #imm_object.class, Objects) of
		{value, #imm_object{attrs=[{saImmRepositoryInit, _, ["2"]}]}} ->
		    mnesia:clear_table(imm_objects),
		    mnesia:clear_table(imm_reverse_refs),
		    mnesia:clear_table(imm_search_index),
		    mnesia:clear_table(imm_class),
		    load_config(Config);
		_Miss ->
		    %% Remove all object ownership
		    CleanFun =
			fun() ->
				Objs = mnesia:match_object(imm_objects, #ldap{_='_'}, write),
				Select = fun(#ldap{key=Key, persistent=false}) ->
						 mnesia:delete(imm_objects, Key, write),
						 delete_class_index(Key);
					    (#ldap{oi=undefined, ao=undefined, oi_appliers=[]}) ->
						 ok;
					    (Rec) ->
						 mnesia:write(imm_objects,
							      Rec#ldap{ao=undefined, oi_appliers=[]},
							      write)
					 end,
				[Select(Obj) || Obj <- Objs],
				ok
			end,
		    mnesia:transaction(CleanFun),
		    case safs:get_env(imm_sync_cb, undefined) of
			{M, F} ->
			    M:F(),
			    ok;
			undefined ->
			    ok
		    end
	    end;
	_ ->
	    error_logger:format("~p~p Tables not created!!!!\n",
				[?MODULE, self()]),
	    setup_mnesia(true, true, _Nodes, Config)
    end;
setup_mnesia(true, false, Nodes, Config) ->
    create_tables(Nodes),
    load_config(Config);
setup_mnesia(false, false, Nodes, Config) ->
    case create_tables(Nodes) of
	created -> load_config(Config);
	wait -> system_up(Config)
    end;
setup_mnesia(MeOnly, TablesCreated, Nodes, undefined) ->
    case read_config() of
	{error, Error, Pos} ->
	    {error, Error, Pos};
	Config ->
	    setup_mnesia(MeOnly, TablesCreated, Nodes, Config)
    end.

create_tables(Nodes) ->
    case mnesia:create_table(imm_objects,
			     [{disc_copies, Nodes},
			      {type, ordered_set},
			      {attributes, record_info(fields, ldap)},
			      {record_name, ldap}
			     ]) of
	{atomic, ok} ->
	    ok(mnesia:create_table(imm_reverse_refs,
				   [{disc_copies, Nodes},
				    {attributes, record_info(fields, imm_reverse_refs)},
				    {record_name, imm_reverse_refs}
				   ])),
	    ok(mnesia:create_table(imm_class,
				   [{disc_copies, Nodes},
				    {attributes, record_info(fields, imm_class)},
				    {record_name, imm_class}
				   ])),
	    ok(mnesia:create_table(imm_search_index,
				   [{disc_copies, Nodes},
				    {type, bag},
				    {attributes, record_info(fields, imm_search_index)},
				    {record_name, imm_search_index}
				   ])),
	    ok(mnesia:create_table(imm_rt_cache,
				   [{attributes, record_info(fields, cache)},
				    {record_name, cache},
				    {ram_copies, Nodes}])),
	    created;
	{aborted, {already_exists, _}} ->
	    wait
    end.

load_config(undefined) ->
    load_config(read_config());
load_config({error, Error, Pos}) ->
    {error, Error, Pos};
load_config({Classes, Objects}) ->
    ok(mnesia:transaction(safs_imm_db:add_classes(Classes))),
    Adds = [safs_imm_db:add(Dn, Class, convert_values(Obj))
	    || Obj = #imm_object{dn=Dn, class=Class} <- Objects],
    ok(mnesia:transaction(fun() ->
				  mnesia:write_lock_table(imm_objects),
				  mnesia:write_lock_table(imm_reverse_refs),
				  mnesia:write_lock_table(imm_search_index),
				  mnesia:read_lock_table(imm_class),
				  [Add() || Add <- Adds],
				  ok
			  end)),
    case safs:get_env(imm_sync_cb, undefined) of
	{M, F} ->
	    M:F(),
	    ok;
	undefined ->
	    ok
    end.

convert_values(#imm_object{class=Class, attrs=Attrs}) ->
    #imm_class{attrs = Defs} = get_schema(Class),
    convert_attrs(Attrs, Defs).

convert_attrs([{Name, _, Vs}|Rest], Defs) ->
    #imm_attr{type=Type} = lists:keyfind(Name, 2, Defs),
    [{Name, [safs_imm_xml:to_type(Value, Type) || Value <- Vs]}
     | convert_attrs(Rest, Defs)];
convert_attrs([], _) -> [].


ok({atomic, ok}) -> ok.

tables() ->
    [imm_objects, imm_reverse_refs, imm_class, imm_rt_cache, imm_search_index].

read_config() ->
    XmlFile = case application:get_env(safs, imm_xml) of
		  undefined -> filename:join(code:lib_dir(safs),"imm.xml");
		  {ok, Path} -> Path
	      end,
    safs_imm_xml:parse_file(XmlFile, []).

import_files(Files, Options) when is_list(Files) ->
    Load = fun() ->
		   [ok = load_config(safs_imm_xml:parse_file(File, Options)) || File <- Files],
		   ok
	   end,
    case mnesia:transaction(Load) of
	{atomic, ok} ->
	    case safs:get_env(imm_sync_cb, undefined) of
		{M, F} ->
		    M:F(),
		    ok;
		undefined ->
		    ok
	    end;
	{aborted, {{badmatch, {error, Error, File}},_}} ->
	    safs_error(import_files, {error, {Error, File}}),
	    {error, {Error, File}};
	{aborted, {throw,Error}} ->
	    safs_error(import_files, {error, Error}),
	    {error, Error};
	{aborted, Error} ->
	    safs_error(import_files, {error, Error}),
	    {error, Error}
    end.

%%====================================================================
%% Reverse reference handling
%%====================================================================
%%--------------------------------------------------------------------
%%  write_reverse_refs/2
%%--------------------------------------------------------------------
write_reverse_refs(_, []) ->
    ok;
write_reverse_refs(DN, [{AttrName, _, ToDNs} |NoDanglingRefs]) ->
    F = fun(TD) ->
		Rec =
		    case mnesia:read(imm_reverse_refs, {DN, TD}, write) of
			[] ->
			    #imm_reverse_refs{key={DN, TD}, value=[AttrName]};
			[#imm_reverse_refs{key={DN, TD}, value=OldValue}] ->
			    #imm_reverse_refs{key={DN, TD}, value=[AttrName |OldValue]}
		    end,
		ok = mnesia:write(imm_reverse_refs, Rec, write)
	end,
    [F(ToDn) || ToDn <- ToDNs],
    write_reverse_refs(DN, NoDanglingRefs).

%%--------------------------------------------------------------------
%%  delete_reverse_refs/1
%%--------------------------------------------------------------------
delete_reverse_refs(DN) ->
    MS = [{#imm_reverse_refs{key={DN, '_'}, value='_'}, [], ['$_']}],
    case mnesia:select(imm_reverse_refs, MS, write) of
	[] ->
	    ok;
	Objs ->
	    DelRef = fun(#imm_reverse_refs{key=Key}) ->
			     mnesia:delete(imm_reverse_refs, Key, write)
		     end,
	    [DelRef(Obj) || Obj <- Objs]
    end.

delete_reverse_refs(_DN, []) ->
    ok;
delete_reverse_refs(DN, [{AttrName, _, ToDNs} |DelNdRefs]) ->
    F = fun(TD) ->
		case mnesia:read(imm_reverse_refs, {DN, TD}, write) of
		    [] ->
			ok;
		    [#imm_reverse_refs{key={DN, TD}, value=OldValue}] ->
			case lists:delete(AttrName, OldValue) of
			    [] ->
				mnesia:delete(imm_reverse_refs, {DN, TD}, write);
			    NewValues ->
				ok = mnesia:write(imm_reverse_refs,
						  #imm_reverse_refs{key={DN, TD}, value=NewValues},
						  write)
			end
		end
	end,
    [F(ToDn) || ToDn <- ToDNs],
    delete_reverse_refs(DN, DelNdRefs).

%%--------------------------------------------------------------------
%%  check_objects_exists/1
%%--------------------------------------------------------------------
check_objects_exists(_, []) ->
    ok;
check_objects_exists(DN, [{AttrName, _, Values} | Attrs]) ->
    F =
	fun(X) ->
		case mnesia:read(imm_objects, X, read) of
		    [#ldap{class=Class}] ->
			{Category, _} = get_class_def(Class),
 			Category =:= config orelse throw({invalid_no_dangling,
							  not_config_obj,
							  {key_to_bin(DN), AttrName, key_to_bin(X)}}),
			ok;
		    [] ->
			throw({invalid_no_dangling, obj_not_exist, {key_to_bin(DN), AttrName, key_to_bin(X)}})
		end
	end,
    [ F(V) || V <- Values],
    check_objects_exists(DN, Attrs).


%%--------------------------------------------------------------------
%%  check_object_deleted_in_transaction/1
%%--------------------------------------------------------------------
check_object_deleted_in_transaction([]) ->
    ok;
check_object_deleted_in_transaction([#imm_reverse_refs{key={FromDN, ToDN}} | Refs]) ->
    case  mnesia:read(imm_objects, FromDN) of
	[_] ->
	    throw({invalid_no_dangling, incoming_reference, {key_to_bin(FromDN), key_to_bin(ToDN)}});
	_ ->
	    check_object_deleted_in_transaction(Refs)
    end.

%%--------------------------------------------------------------------
%%  check_reverse_refs_delete/1
%%--------------------------------------------------------------------
check_reverse_refs_delete(DN) ->
    MS = [{#imm_reverse_refs{key={'_', DN}, _='_'}, [], ['$_']}],
    case mnesia:select(imm_reverse_refs, MS) of
	[] ->
	    ok;
	Refs ->
	    check_object_deleted_in_transaction(Refs)
    end.

%%--------------------------------------------------------------------
%%  check_reverse_refs/1
%%--------------------------------------------------------------------
% check_reverse_refs(DN) ->
%     MS = [{#imm_reverse_refs{key={'_', DN}, _='_'}, [], ['$_']}],
%     case mnesia:select(imm_reverse_refs, MS) of
% 	[] ->
% 	    false;
% 	_ ->
% 	    true
%     end.

%%--------------------------------------------------------------------
%%  get_nd_refs_from_attrs/2/3
%%--------------------------------------------------------------------
get_nd_refs_from_attrs(Attrs, #imm_class{attrs=As}) ->
    get_nd_refs_from_attrs(lists:sort(Attrs), lists:sort(As), []).

get_nd_refs_from_attrs([], [], Acc) ->
    Acc;
get_nd_refs_from_attrs([{AttrName, sa_imm_attr_csstructt, Values} |Attrs],
			   [#imm_attr{name=AttrName, type=sa_imm_attr_sanamet, flags=_Flags} |As],
		       Acc) ->
    Acc2 = get_nd_refs_from_struct(AttrName, Values, Acc),
    get_nd_refs_from_attrs(Attrs, As, Acc2);
get_nd_refs_from_attrs([{AttrName, sa_imm_attr_sanamet, Values} |Attrs],
			   [#imm_attr{name=AttrName, type=sa_imm_attr_sanamet, flags=Flags} |As],
			   Acc) ->
    case lists:member(no_dangling, Flags) of
	true ->
	    get_nd_refs_from_attrs(Attrs, As, [{AttrName, sa_imm_attr_sanamet, [mk_key(V) || V <- Values]} |Acc]);
	false ->
	    get_nd_refs_from_attrs(Attrs, As, Acc)
    end;
get_nd_refs_from_attrs([{AttrName, _, _Value} |Attrs], [#imm_attr{name=AttrName} |As], Acc) ->
    get_nd_refs_from_attrs(Attrs, As, Acc);
get_nd_refs_from_attrs(Attrs, [#imm_attr{name=_AttrName} |As], Acc) ->
    get_nd_refs_from_attrs(Attrs, As, Acc).


get_nd_refs_from_struct(_, [], Acc) ->
    Acc;
get_nd_refs_from_struct(AttrName,
			[#safsImmCsStruct{structName=Name, structMembers=Members} |Values],
			Acc) ->
    #imm_class{attrs=As} = get_schema(Name),
    Acc2 = get_nd_refs_from_struct_members(AttrName, Members, As, Acc),
    get_nd_refs_from_struct(AttrName, Values, Acc2).

get_nd_refs_from_struct_members(_, [], _, Acc) ->
    Acc;
get_nd_refs_from_struct_members(AttrName,
				[#safsImmAttrValues_2{attrName=MName,
						      attrValueType=sa_imm_attr_sanamet,
						      attrValues=Values}|
				 Members],
				DefinedAttrs,
				Acc) ->
    MNameAtom = erlang:binary_to_atom(MName, utf8),
    case lists:keyfind(MNameAtom, 2, DefinedAttrs) of
	#imm_attr{name=MNameAtom, type=sa_imm_attr_sanamet, flags=Flags} ->
	         case lists:member(no_dangling, Flags) of
		     true ->
			 {ok, Values1} = safs_imm_lib:get_attr_values(sa_imm_attr_sanamet,
									Values),
			 T = {{AttrName,MNameAtom}, sa_imm_attr_sanamet, [mk_key(V) || V <- Values1]},
			 get_nd_refs_from_struct_members(AttrName, Members, DefinedAttrs, [T |Acc]);
		     false ->
			 get_nd_refs_from_struct_members(AttrName, Members, DefinedAttrs, Acc)
		 end;
	 _ ->
	     throw({invalid_param, AttrName})
     end;
get_nd_refs_from_struct_members(AttrName, [_Attr |Attrs], DefinedAttrs, Acc) ->
    get_nd_refs_from_struct_members(AttrName, Attrs, DefinedAttrs, Acc).

%%--------------------------------------------------------------------
%%  get_nd_refs_from_ops/3/4
%%--------------------------------------------------------------------
get_nd_refs_from_ops(Ops, #imm_class{attrs=As}, OldAttrs) ->
    get_nd_refs_from_ops(fix_ops(Ops), lists:sort(As), OldAttrs, {[],[]}).


fix_ops(Ops) ->
    lists:sort(fix_ops(Ops, [])).

fix_ops([], Acc) ->
    Acc;
fix_ops([{sa_imm_attr_values_add, {Key, Type, Vs}} | Ops], Acc) ->
    fix_ops(Ops, [{Key, add, Type, Vs} | Acc]);
fix_ops([{sa_imm_attr_values_delete, {Key, Type, Vs}} | Ops], Acc) ->
    fix_ops(Ops, [{Key, delete, Type, Vs} | Acc]);
fix_ops([{sa_imm_attr_values_replace, {Key, Type, Vs}} | Ops], Acc) ->
    fix_ops(Ops, [{Key, replace, Type, Vs} | Acc]).

get_nd_refs_from_ops([], [], _, Acc) ->
    Acc;
get_nd_refs_from_ops([{Key, add, sa_imm_attr_csstructt, NewVs} |Ops],
 		 [#imm_attr{name=Key, type=sa_imm_attr_sanamet, category=Category} |As], OldAttrs, {Add, Delete}) ->
    case Category of
	config ->
	    Acc2 = get_nd_refs_from_struct(Key, NewVs, []),
	    get_nd_refs_from_ops(Ops, As, OldAttrs, {Acc2 ++ Add, Delete});
	_ ->
	    get_nd_refs_from_ops(Ops, As, OldAttrs, {Add, Delete})
    end;
get_nd_refs_from_ops([{Key, add, _, NewVs} |Ops],
		 [#imm_attr{name=Key, type=sa_imm_attr_sanamet, flags=Flags} |As], OldAttrs, {Add, Delete} = Acc) ->
    case lists:member(no_dangling, Flags) of
	true ->
	    get_nd_refs_from_ops(Ops, As, OldAttrs, {[{Key, sa_imm_attr_sanamet, [mk_key(V) || V <- NewVs]} |Add], Delete});
	false ->
	    get_nd_refs_from_ops(Ops, As, OldAttrs, Acc)
    end;
get_nd_refs_from_ops([{Key, delete, sa_imm_attr_csstructt, DelVs} |Ops],
	     [#imm_attr{name=Key, type=sa_imm_attr_sanamet, category=Category} |As], OldAttrs, {Add, Delete}) ->
    case Category of
	config ->
	    Acc2 = get_nd_refs_from_struct(Key, DelVs, []),
	    get_nd_refs_from_ops(Ops, As, OldAttrs, {Add, Acc2 ++ Delete});
	_ ->
	    get_nd_refs_from_ops(Ops, As, OldAttrs, {Add, Delete})
    end;
get_nd_refs_from_ops([{Key, delete, _, DelVs} |Ops],
	     [#imm_attr{name=Key, type=sa_imm_attr_sanamet, flags=Flags} |As], OldAttrs, {Add, Delete} = Acc) ->
    case lists:member(no_dangling, Flags) of
	true ->
	    get_nd_refs_from_ops(Ops, As, OldAttrs, {Add, [{Key, sa_imm_attr_sanamet, [mk_key(V) || V <- DelVs]} |Delete]});
	false ->
	    get_nd_refs_from_ops(Ops, As, OldAttrs, Acc)
    end;
get_nd_refs_from_ops([{Key, replace, sa_imm_attr_csstructt, NewVs} |Ops],
	     [#imm_attr{name=Key, type=sa_imm_attr_sanamet, category=Category} |As], OldAttrs, {Add, Delete}) ->
    case Category of
	config ->
	    case lists:keyfind(Key, 1, OldAttrs) of
		{Key, sa_imm_attr_csstructt, OldVs} ->
		    Acc2 = get_nd_refs_from_struct(Key, NewVs, []),
		    Acc3 = get_nd_refs_from_struct(Key, OldVs, []),
		    get_nd_refs_from_ops(Ops, As, OldAttrs, {Acc2 ++ Add, Acc3 ++ Delete});
		{Key, _OldVs} ->
		    Acc2 = get_nd_refs_from_struct(Key, NewVs, []),
		    get_nd_refs_from_ops(Ops, As, OldAttrs, {Acc2 ++ Add, Delete});
		false ->
		    Acc2 = get_nd_refs_from_struct(Key, NewVs, []),
		    get_nd_refs_from_ops(Ops, As, OldAttrs, {Acc2 ++ Add, Delete})
	    end;
	_ ->
	    get_nd_refs_from_ops(Ops, As, OldAttrs, {Add, Delete})
    end;
get_nd_refs_from_ops([{Key, replace, _, NewVs} |Ops],
	     [#imm_attr{name=Key, type=sa_imm_attr_sanamet, flags=Flags} |As], OldAttrs, {Add, Delete} = Acc) ->
    case lists:member(no_dangling, Flags) of
	true ->
	    case lists:keyfind(Key, 1, OldAttrs) of
		{Key, OldVs} ->
		    get_nd_refs_from_ops(Ops, As, OldAttrs, {[{Key, sa_imm_attr_sanamet, [mk_key(V) || V <- NewVs]} |Add],
							     [{Key, sa_imm_attr_sanamet, [mk_key(V) || V <- OldVs]} |Delete]});
		false ->
		    get_nd_refs_from_ops(Ops, As, OldAttrs, {[{Key, sa_imm_attr_sanamet, [mk_key(V) || V <- NewVs]} |Add], Delete})
	    end;
	false ->
	    get_nd_refs_from_ops(Ops, As, OldAttrs, Acc)
    end;
get_nd_refs_from_ops([{Key, _, _, _Value} |Ops], [#imm_attr{name=Key} |As], OldAttrs, Acc) ->
    get_nd_refs_from_ops(Ops, As, OldAttrs, Acc);
get_nd_refs_from_ops(Ops, [#imm_attr{name=_Key} |As], OldAttrs, Acc) ->
    get_nd_refs_from_ops(Ops, As, OldAttrs, Acc).


%%--------------------------------------------------------------------
%%  get_imm_objects_payload/1 - used by ICTI to parse ldap records
%%--------------------------------------------------------------------
get_imm_objects_payload({_, Key, _, Attrs, Class, OI, _, _, _}) ->
    {Class, Key, Attrs, get_oi_name(OI)};
get_imm_objects_payload({_, Key, _, Attrs, Class, OI, _, _}) ->
    {Class, Key, Attrs, get_oi_name(OI)};
get_imm_objects_payload({_, Key, _, Attrs, Class, OI, _}) ->
    {Class, Key, Attrs, get_oi_name(OI)};
get_imm_objects_payload({_, Key, Attrs, Class, OI, _}) ->
    {Class, Key, Attrs, get_oi_name(OI)}.

get_oi_name(undefined) ->
    undefined;
get_oi_name({OI,_}) ->
    OI.

%%--------------------------------------------------------------------
%% Handling of class index table
%%--------------------------------------------------------------------
delete_class_index(DN) ->
    Objs = mnesia:match_object(imm_search_index, #imm_search_index{value=DN, _='_'}, write),
    DelObj = fun(Obj) ->
		     mnesia:delete_object(Obj)
	     end,
    [DelObj(Obj) || Obj <- Objs].

write_class_index(ParentDN, Class, DN) ->
    mnesia:write(#imm_search_index{key={ParentDN, Class}, value=DN}).

get_class_index(ParentDN, Class) ->
    fun() ->
	    mnesia:read(imm_search_index, {ParentDN, Class})
    end.
%%--------------------------------------------------------------------
