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
%% File: safs_imm_om.erl
%%
%% Description:
%%    IMM OM gen_server.
%%
%%--------------------------------------------------------------------
-module(safs_imm_om).
-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("om.hrl").
-include("safs_imm_internal.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/0,
	 start/0,
	 stop/0,
	 initialize/1,
	 initialize/2,
	 initialize_o2/1,
	 initialize_o2/2,
	 finalize/1,
	 class_create_2/4,
	 class_description_get_2/2,
	 class_delete/2,
	 search_initialize_2/6,
	 search_class_initialize_s2/6,
	 search_next_2/1,
	 c_search_next_2/1,
	 search_next_n_s2/2,
	 c_search_next_n_s2/2,
	 search_finalize/1,
	 accessor_initialize/1,
	 accessor_get_2/3,
	 c_accessor_get_2/3,
	 accessor_finalize/1,
	 admin_owner_initialize/3,
	 admin_owner_finalize/1,
	 admin_owner_set/3,
	 admin_owner_release/3,
	 admin_owner_clear/3,
	 ccb_initialize/2,
	 ccb_object_create_2/4,
 	 ccb_object_delete/2,
 	 ccb_object_modify_2/3,
 	 ccb_object_read/3,
 	 c_ccb_object_read/3,
	 ccb_validate/1,
	 ccb_apply/1,
	 ccb_abort/1,
	 ccb_finalize/1,
	 admin_operation_invoke_2/6,
	 admin_operation_invoke_o2/6,
	 c_admin_operation_invoke_o2/6,
	 admin_operation_invoke_async_2/6,
	 c_admin_operation_invoke_async_2/6,
	 admin_operation_continue/3,
	 admin_operation_continue_async/4,
	 admin_operation_result/2,
	 admin_operation_result_o2/3,
 	 lookup_tbl/2,
	 get_pid_from_ccb_handle/1,
	 get_ccbid_from_ccb_handle/1,
	 get_pid_from_ccb_id/1,
	 affected_by_ccb/1,
	 affected_by_ccb/2,
	 callbacks_initialize/1,
	 get_fun_list/1,
	 send_completed/1,
	 send_apply/1,
	 send_abort/1,
	 ccb_insert_fun/2,
	 set_external_pid/2,
	 augment_ccb_initialize/3,
	 ccb_get_error_strings/1,
	 convert_search_attributes/1,
	 check_rt_attrs/3,
	 add_types_to_attributes/3,
	 create_ccb_id/0,
	 ccb_object_create_s2/5,
	 ccb_object_modify_extra_attrs_s2/3,
	 ccb_object_read_extra_attrs_s2/2,
	 get_class_instances/2
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3,
	 trace_groups/0,
	 trace_points_list/0,
	 apply_db/1,
	 admin_operation_invoke_callback/5,
	 ccb_set_error_string/2,
	 ccb_delete_error_strings/1,
	 get_ccbid_from_handle/1,
	 adm_owner/1,
	 add_ao_owned_object/2,
	 change_ccbid_in_ccb/2
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(SAFS_OM_DEBUG(F, A), safs_error(F,A)).

-define(SA_IMM_ATTR_MULTI_VALUE,   16#00000001).
-define(SA_IMM_ATTR_RDN,           16#00000002).
-define(SA_IMM_ATTR_CONFIG,        16#00000100).
-define(SA_IMM_ATTR_WRITABLE,      16#00000200).
-define(SA_IMM_ATTR_INITIALIZED,   16#00000400).
-define(SA_IMM_ATTR_RUNTIME,       16#00010000).
-define(SA_IMM_ATTR_PERSISTENT,    16#00020000).
-define(SA_IMM_ATTR_CACHED,        16#00040000).
-define(SA_IMM_ATTR_NO_DUPLICATES, 16#01000000).
-define(SA_IMM_ATTR_NOTIFY,        16#02000000).
-define(SA_IMM_ATTR_NO_DANGLING,   16#04000000).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {}).

-record(safs_imm_om_user, {
	  handle,
	  version,
	  callbacks,
	  proxy,
	  cb_proxy,
	  reply_to,
	  external_pid
	 }).

-record(safs_imm_om_accessor, {
	  accessor_handle,
	  imm_handle
	 }).

-record(safs_imm_om_admin_operation, {
	  invocation,
 	  user_invocation,
	  owner,
	  version,
	  type,
	  op,
	  continuation,
 	  from,
	  result
 	 }).

-record(safs_imm_om_ccb, {
	  handle,
	  owner_handle,
 	  id,
	  pid
	 }).

-record(safs_imm_om_ccb_error, {
 	  id,
	  string
	 }).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: start/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: stop/0
%% Description: Starts the server
%%--------------------------------------------------------------------
stop() ->
    call(stop).

%%--------------------------------------------------------------------
%% Function: initialize/1
%% Description:
%%--------------------------------------------------------------------
initialize(Callbacks) ->
    initialize(Callbacks, #safsVersion{releaseCode=?IMM_RELEASE_CODE,
				       majorVersion=?IMM_MAJOR_VERSION,
				       minorVersion=?IMM_MINOR_VERSION}).

%%--------------------------------------------------------------------
%% Function: initialize/2
%% Description:
%%--------------------------------------------------------------------
initialize(Callbacks, #safsVersion{} = Version) when is_atom(Callbacks);
						     is_function(Callbacks, 1);
						     is_record(Callbacks, safsImmCallbacks) ->
    case safs_imm_lib:validate_version(Version) of
	{ok, Version1} ->
	    call({initialize, Callbacks, Version1, self()});
	{error, sa_ais_err_version} ->
	    {error, sa_ais_err_version}
    end;
initialize(_Callbacks, _Version) ->
    {error, sa_ais_err_invalid_param}.

%%--------------------------------------------------------------------
%% Function: finalize/1
%% Description:
%%--------------------------------------------------------------------
finalize(Handle) ->
    call({finalize, Handle}).

%%--------------------------------------------------------------------
%% Function: class_create_2/4
%% Description:
%%--------------------------------------------------------------------
class_create_2(Handle, ClassName, ClassCategory, AttrDefinitions) ->
    try
	lookup_tbl(safs_imm_om_user, Handle),
	Class = list_to_atom(binary_to_list(ClassName)),
        Category = class_category_sa_to_db(ClassCategory),
        Attrs = convert_from_attr_definitions(Category, AttrDefinitions),
        {Rdn, Attrs2} = get_rdn(Attrs),
	Fun = safs_imm_db:add_class_def(Class, Category, Rdn, Attrs2),
        case apply_db([Fun]) of
	    {aborted, Reason} ->
		error_logger:info_msg("safs_imm_om:class_create_2()\n"
				      "ClassName: ~p\n"
				      "Try again due to aborted transaction with reason: ~p\n",
				      [ClassName, Reason]),
		{error, sa_ais_err_try_again};
	    {atomic, ok} ->
		case safs:get_env(imm_sync_cb, undefined) of
		    {M, F} ->
			M:F(),
			ok;
		    undefined ->
			ok
		end
	end
    catch
	throw:already_exists ->
           {error, sa_ais_err_exist};
	throw:{invalid_param, _Name} ->
           {error, sa_ais_err_invalid_param};
	throw:Error ->
	   Error
    end.

%%--------------------------------------------------------------------
%% Function: description_get_2/2
%% Description:
%%--------------------------------------------------------------------
%% class_description_get_2(Handle, <<"MpClusterHandling">> = ClassName) ->
%%     try
%%         io:format("Fetching class description for MpClusterHandling!!\n", []),
%% 	lookup_tbl(safs_imm_om_user, Handle),
%%         {Category, AttrDefs} = safs_imm_db:get_class_def(list_to_atom(binary_to_list(ClassName))),
%%         io:format("Values: ~p\n", [{Category, AttrDefs}]),
%%         {ok, class_category_db_to_sa(Category), convert_to_attr_definitions(AttrDefs)}
%%     catch
%% 	throw:no_exists ->
%% 	  {error, sa_ais_err_not_exist};
%% 	throw:no_such_class ->
%% 	  {error, sa_ais_err_not_exist};
%% 	throw:Error ->
%%           Error
%%     end;
class_description_get_2(Handle, ClassName) ->
    try
	lookup_tbl(safs_imm_om_user, Handle),
        {Category, AttrDefs} = safs_imm_db:get_class_def(list_to_atom(binary_to_list(ClassName))),
        {ok, class_category_db_to_sa(Category), convert_to_attr_definitions(AttrDefs)}
    catch
	throw:no_exists ->
	  {error, sa_ais_err_not_exist};
	throw:no_such_class ->
	  {error, sa_ais_err_not_exist};
	throw:Error ->
          Error
    end.

%%--------------------------------------------------------------------
%% Function: class_delete/2
%% Description:
%%--------------------------------------------------------------------
class_delete(Handle, ClassName) ->
     try
	lookup_tbl(safs_imm_om_user, Handle),
        Fun = safs_imm_db:delete_class_def(list_to_atom(binary_to_list(ClassName))),
        case apply_db([Fun]) of
	    {aborted, Reason} ->
		error_logger:info_msg("safs_imm_om:class_delete()\n"
				      "ClassName: ~p\n"
				      "Try again due to aborted transaction with reason: ~p\n",
				      [ClassName, Reason]),
		{error, sa_ais_err_try_again};
	    {atomic, ok} ->
		case safs:get_env(imm_sync_cb, undefined) of
		    {M, F} ->
			M:F(),
			ok;
		    undefined ->
			ok
		end
        end
    catch
      throw:objects_exists ->
	 {error, sa_ais_err_exist};
      throw:no_exists ->
	 {error, sa_ais_err_not_exist};
      throw:no_such_class ->
	 {error, sa_ais_err_not_exist};
 	throw:Error ->
          Error
   end.

%%--------------------------------------------------------------------
%% Function: search_initialize/6
%% Description:
%%--------------------------------------------------------------------
search_initialize_2(Handle, RootName, Scope, SearchOptions,
		    SearchParams, AttributeNames) ->
    {ok, SearchHandle, Pid} =
	call({search_initialize_2,
	      Handle,
	      safs_imm_lib:check_if_root(RootName),
	      convert_scope(Scope),
	      SearchOptions,
	      SearchParams,
	      AttributeNames}),
    case safs_imm_om_search:search(Pid, SearchHandle) of
	ok ->
	    {ok, SearchHandle};
	{error, Reason} ->
	    delete_search_data(SearchHandle),
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: search_class_initialize_s2/6
%% Description:
%%--------------------------------------------------------------------
search_class_initialize_s2(Handle, RootName, Scope, SearchOptions,
			   ClassNames0, AttributeNames) ->
    ClassNames1 = lists:usort(ClassNames0),
    {ok, SearchHandle, Pid} =
	call({search_class_initialize_s2,
	      Handle,
	      safs_imm_lib:check_if_root(RootName),
	      convert_scope(Scope),
	      SearchOptions,
	      ClassNames1,
	      AttributeNames}),
    case safs_imm_om_search:search(Pid, SearchHandle) of
	ok ->
	    {ok, SearchHandle};
	{error, Reason} ->
	    delete_search_data(SearchHandle),
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: search_next_2/1
%% Description:
%%--------------------------------------------------------------------
search_next_2(SearchHandle) ->
    case search_next_common(SearchHandle, 1, erlang) of
	{ok, Result} ->
	    [{ObjectName, Attributes}] =  Result,
	    {ok, ObjectName, Attributes};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% Function: c_search_next_2/1
%% Description:
%%--------------------------------------------------------------------
c_search_next_2(SearchHandle) ->
    case search_next_common(SearchHandle, 1, c) of
	{ok, Result} ->
	    [#safsImmSearchObjects_s2{objectName=ObjectName,
				      attributes=Attributes}] =  Result,
	    {ok, ObjectName, Attributes};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% Function: search_next_n_s2/2
%% Description:
%%--------------------------------------------------------------------
search_next_n_s2(SearchHandle, N) ->
    search_next_common(SearchHandle, N, erlang).

%%--------------------------------------------------------------------
%% Function: c_search_next_n_s2/2
%% Description:
%%--------------------------------------------------------------------
c_search_next_n_s2(SearchHandle, N) ->
    search_next_common(SearchHandle, N, c).

%%--------------------------------------------------------------------
%% Function: search_next_common/3
%% Description:
%%--------------------------------------------------------------------
search_next_common(SearchHandle, N, Type) ->
    try
	SD = lookup_tbl(safs_imm_om_search_data, SearchHandle),
        search_next_common_1(SD, N, N, Type, [])
    catch
	throw:ErrMsg -> ErrMsg
    end.

search_next_common_1(_SD, _N, 0, _Type, Acc) ->
    {ok, Acc};
search_next_common_1(SD, N, NRest, Type, Acc) ->
    case safs_imm_om_search:get_next_n(SD#safs_imm_om_search_data.pid, NRest,
				       SD#safs_imm_om_search_data.params) of
	{ok, SearchResult} -> %ObjectName, ClassName, Attributes} ->
	    case conv_search_attr(SearchResult, Type, []) of
		{ok, ConvertedResult} ->
		    Acc2 = Acc++ConvertedResult, %%LATH: Perhaps need som optimization
		    search_next_common_1(SD, N, N-length(Acc2), Type, Acc2);
		Error ->
		    Error
	    end;
	{error, sa_ais_err_not_exist} ->
	    case Acc of
		[] ->
		    {error, sa_ais_err_not_exist};
		_ ->
		    {ok, Acc}
	    end;
	Error2 ->
	    Error2
    end.

conv_search_attr([], _, Acc) ->
    {ok, Acc};
conv_search_attr([{ObjectName, Attributes} |Rest], erlang, Acc) ->
    conv_search_attr(Rest, erlang, [{ObjectName, Attributes} |Acc]);
conv_search_attr([{ObjectName, Attributes} |Rest], c, Acc) ->
    conv_search_attr(Rest, c, [#safsImmSearchObjects_s2{objectName=ObjectName,
							attributes=Attributes} |Acc]);
conv_search_attr([{ObjectName, ClassName, Attributes} |Rest], Type, Acc) ->
    try
      {ok, Attributes2} = check_rt_attrs(ObjectName, Attributes),
      case add_types_to_attributes(Type, ClassName, Attributes2) of
	  {ok, NewAttributes} ->
	      case Type of
		  erlang ->
		      conv_search_attr(Rest, Type, [{ObjectName, NewAttributes} |Acc]);
		  c ->
		      conv_search_attr(Rest, Type,
				       [#safsImmSearchObjects_s2{objectName=ObjectName,
								 attributes=NewAttributes} |Acc])
	      end;
	  Error ->
	      ?SAFS_OM_DEBUG(conv_search_attr, {"add_types_to_attributes", Error}),
	      Error %%%LATH Måste fixas
      end
    catch
	throw:no_exists ->
	    conv_search_attr(Rest, Type, Acc)
    end.

delete_search_data(Handle) ->
    call({delete_search_data, Handle}).

%%--------------------------------------------------------------------
%% Function: search_finalize/1
%% Description:
%%--------------------------------------------------------------------
search_finalize(Handle) ->
    call({search_finalize, Handle}).

%%--------------------------------------------------------------------
%% Function: accessor_initialize/1
%% Description:
%%--------------------------------------------------------------------
accessor_initialize(Handle) ->
    call({accessor_initialize, Handle}).

%%--------------------------------------------------------------------
%% Function: acessor_get_2/3
%% Description:
%%--------------------------------------------------------------------
accessor_get_2(AccessorHandle, ObjectName, AttributeNames) ->
    accessor_get_2_common(AccessorHandle, ObjectName, AttributeNames, erlang).

%%--------------------------------------------------------------------
%% Function: c_accessor_get_2/3
%% Description:
%%--------------------------------------------------------------------
c_accessor_get_2(AccessorHandle, ObjectName, AttributeNames) ->
    accessor_get_2_common(AccessorHandle, ObjectName, AttributeNames, c).

%%--------------------------------------------------------------------
%% Function: acessor_get_2_common/4
%% Description:
%%--------------------------------------------------------------------
accessor_get_2_common(AccessorHandle, ObjectName, AttributeNames, Type) ->
    try
        %% Check Handle
	_Accessor = lookup_tbl(safs_imm_om_accessor, AccessorHandle),
        GetOp =
	   case convert_search_attributes(AttributeNames) of
	       [] ->
		   safs_imm_db:search(ObjectName,
				      one,
				      all,
				      true);
	       ['SA_IMM_SEARCH_GET_CONFIG_ATTR'] ->
		   safs_imm_db:search(ObjectName,
				      one,
				      config,
				      true);
	       ['SA_IMM_SEARCH_GET_PERSISTENT_ATTR'] ->
		   safs_imm_db:search(ObjectName,
				      one,
				      persistent,
				      true);
	       GetAttrs ->
		   safs_imm_db:search(ObjectName,
				      one,
				      {some, GetAttrs},
				      true)
	   end,
        case mnesia:transaction(GetOp) of
	    {aborted, {throw,no_exists}} ->
		{error, sa_ais_err_not_exist};
	    {aborted, Reason} ->
		?SAFS_OM_DEBUG(accessor_get_2_common,
			       {"Transaction failed", {aborted, lists:flatten(Reason)}}),
		{error, sa_ais_err_failed_operation};
	    {atomic, []} ->
		{error, sa_ais_err_not_exist};
	    {atomic, [{_, ClassName, Attributes}]} ->
		{ok, Attributes2} = check_rt_attrs(ObjectName, Attributes),
		add_types_to_attributes(Type, ClassName, Attributes2)
	end
    catch
	throw:no_exists ->
	   {error, sa_ais_err_not_exist};
	throw:ErrMsg -> ErrMsg
    end.

check_rt_attrs(ObjectName, Attrs) ->
    check_rt_attrs(ObjectName, Attrs, none).

check_rt_attrs(ObjectName, Attrs, Context) ->
    case get_rt_attrs(Attrs) of
	{Attrs, []} ->
	    _ = safs_imm_db:oi_get(ObjectName, Context),
	    {ok, Attrs};
	{ConfigAttrs, RtAttrs} ->
	    case safs_imm_db:oi_get(ObjectName, Context) of
		{_, undefined, _} ->
		    {ok, ConfigAttrs ++ RtAttrs};
		{_, Oi, _} ->
		    case safs_imm_oi:rt_attr_update(Oi, ObjectName,
						    [safs_imm_lib:convert_attribute_name_to_binary(A) || A <- RtAttrs]) of
			{ok, RtAttrMods} ->
			    RtAttrs2 = add_empty_attrs(RtAttrMods, RtAttrs),
			    {ok, ConfigAttrs ++ RtAttrMods ++ RtAttrs2};
			{error, Reason} when Reason == sa_ais_err_failed_operation;
					     Reason == sa_ais_err_bad_operation ->
			    %%LATH: Perhaps temporary warning
			    error_logger:warning_msg("~p:check_rt_attrs/3 OI: ~p answered ~p\n"
						     "ObjectName = ~p\n"
						     "RtAttrs: ~p\n",
						     [?MODULE, Oi, Reason, ObjectName, RtAttrs]),
			    {ok, ConfigAttrs ++ RtAttrs};
			Error ->
			    error_logger:warning_msg("~p:check_rt_attrs/3 call to OI: ~p got ~p\n"
						     "ObjectName = ~p\n"
						     "RtAttrs: ~p\n",
						     [?MODULE, Oi, Error, ObjectName, RtAttrs]),
			    {ok, ConfigAttrs ++ RtAttrs}
		    end
	    end
    end.

get_rt_attrs(Attrs) ->
    get_rt_attrs(Attrs, {[],[]}).

get_rt_attrs([], {Config, Rt}) ->
    {lists:reverse(Config), lists:reverse(Rt)};
get_rt_attrs([{AttrName, '$imm_runtime'} |Attrs], {Config, Rt}) ->
    get_rt_attrs(Attrs, {Config, [AttrName |Rt]});
get_rt_attrs([Attr |Attrs], {Config, Rt}) ->
    get_rt_attrs(Attrs, {[Attr |Config], Rt}).

add_empty_attrs([], RtAttrs) ->
    RtAttrs;
add_empty_attrs([#safsImmAttrValues_2{attrName = Name}  |RtAttrMods], RtAttrs)->
    add_empty_attrs(RtAttrMods, lists:delete(safs_imm_lib:ta(Name), RtAttrs));
add_empty_attrs([{Name, _, _} |RtAttrMods], RtAttrs) ->
    add_empty_attrs(RtAttrMods, lists:delete(safs_imm_lib:ta(Name), RtAttrs)).

%%--------------------------------------------------------------------
%% Function: accessor_finalize/1
%% Description:
%%--------------------------------------------------------------------
accessor_finalize(AccessorHandle) ->
    call({accessor_finalize, AccessorHandle}).

%%--------------------------------------------------------------------
%% Function: admin_owner_initialize/3
%% Description:
%%--------------------------------------------------------------------
admin_owner_initialize(Handle, AOName, ReleaseOwnership) ->
    call({admin_owner_initialize, Handle, AOName, ReleaseOwnership}).

%%--------------------------------------------------------------------
%% Function: admin_owner_finalize/1
%% Description:
%%--------------------------------------------------------------------
admin_owner_finalize(OwnerHandle) ->
    call({admin_owner_finalize, OwnerHandle}).

%%--------------------------------------------------------------------
%% Function: admin_owner_set/3
%% Description:
%%--------------------------------------------------------------------
admin_owner_set(OwnerHandle, ObjectNames, Scope) ->
    try
	AO = lookup_tbl(safs_imm_om_adm_owner, OwnerHandle),
        Objs = [safs_imm_db:ao_involved_objs(ObjectName, convert_scope(Scope)) ||
		   ObjectName <- ObjectNames], %%LATH adm_on_finalize
        Ops = [ao_set(ObjectName, AO#safs_imm_om_adm_owner.name,
		      convert_scope(Scope)) ||
		  ObjectName <- ObjectNames],
        case apply_db(Ops) of
	    {aborted, {throw, already_exists}} ->
		{error, sa_ais_err_exist};
	    {aborted, {throw, no_exists}} ->
		    {error, sa_ais_err_not_exist};
	    {aborted, Reason} ->
		error_logger:info_msg("safs_imm_om:admin_owner_set()\n"
				      "AO: ~p\n"
				      "ObjectNames: ~p\n"
				      "Scope: ~p\n"
				      "Transaction failed: ~p\n",
				      [AO#safs_imm_om_adm_owner.name,
				       ObjectNames, Scope,
				       Reason]),
		{error, sa_ais_err_try_again};
	    {atomic, ok} ->
		case safs:get_env(imm_sync_cb, undefined) of
		    {M, F} ->
			M:F();
		    undefined ->
			ok
		end,
		[insert_tbl(safs_imm_om_administrated_objects,
			    #safs_imm_om_administrated_objects{key={AO#safs_imm_om_adm_owner.name, Obj}})
		 || Obj <- lists:append(Objs)],
		ok
	end
    catch
	throw:Term -> Term
    end.

%%--------------------------------------------------------------------
%% Function: admin_owner_release/3
%% Description:
%%--------------------------------------------------------------------
admin_owner_release(OwnerHandle, ObjectNames, Scope) ->
    try
	AO = lookup_tbl(safs_imm_om_adm_owner, OwnerHandle),
        Objs = [safs_imm_db:ao_involved_objs(ObjectName, convert_scope(Scope)) ||
		   ObjectName <- ObjectNames], %%LATH adm_on_finalize
        Ops = [safs_imm_db:ao_release(ObjectName, AO#safs_imm_om_adm_owner.name,
				      convert_scope(Scope)) ||
		  ObjectName <- ObjectNames],
        case apply_db(Ops) of
	    {aborted, {throw, no_exists}} ->
		    {error, sa_ais_err_not_exist};
	    {aborted, Reason} ->
		error_logger:info_msg("safs_imm_om:admin_owner_release()\n"
				      "AO: ~p\n"
				      "ObjectNames: ~p\n"
				      "Scope: ~p\n"
				      "Transaction failed: ~p\n",
				      [AO#safs_imm_om_adm_owner.name,
				       ObjectNames, Scope,
				       Reason]),
		{error, sa_ais_err_try_again};
	    {atomic, ok} ->
		case safs:get_env(imm_sync_cb, undefined) of
		    {M, F} ->
			M:F();
		    undefined ->
			ok
		end,
		[delete_tbl(safs_imm_om_administrated_objects,
			    {AO#safs_imm_om_adm_owner.name, Obj})
		 || Obj <- lists:append(Objs)],
		ok
	end
    catch
	throw:Term -> Term
    end.

%%--------------------------------------------------------------------
%% Function: admin_owner_clear/3
%% Description:
%%--------------------------------------------------------------------
admin_owner_clear(Handle, ObjectNames, Scope) ->
    try
	lookup_tbl(safs_imm_om_user, Handle),
	Ops = [safs_imm_db:ao_clear(ObjectName, convert_scope(Scope)) ||
		  ObjectName <- ObjectNames],
        case apply_db(Ops) of
	    {aborted, {throw, no_exists}} ->
		    {error, sa_ais_err_not_exist};
	    {aborted, Reason} ->
		error_logger:info_msg("safs_imm_om:admin_owner_clear()\n"
				      "ObjectNames: ~p\n"
				      "Scope: ~p\n"
				      "Transaction failed: ~p\n",
				      [ObjectNames, Scope,
				       Reason]),
		{error, sa_ais_err_try_again};
	    {atomic, ok} ->
		case safs:get_env(imm_sync_cb, undefined) of
		    {M, F} ->
			M:F();
		    undefined ->
			ok
		end,
		[ets:match_delete(safs_imm_om_administrated_objects,
				  {safs_imm_om_administrated_objects,
				   {'_',safs_imm_db:mk_key(ObjectName)}}) ||
		    ObjectName <- ObjectNames],
		ok
	end
    catch
	throw:Term -> Term
   end.

%%--------------------------------------------------------------------
%% Function: ccb_initialize/2
%% Description:
%%--------------------------------------------------------------------
ccb_initialize(AoHandle, CcbFlags) ->
    try
       CcbFlags2 = check_ccb_flags(CcbFlags, []),
       call({ccb_initialize, AoHandle, CcbFlags2})
    catch
       throw:{invalid_param, _Error} ->
	  {error, sa_ais_err_invalid_param}
    end.

%%--------------------------------------------------------------------
%% Function: ccb_object_create_2/4
%% Description:
%%--------------------------------------------------------------------
ccb_object_create_2(CcbHandle, ClassName, ParentName, AttrValues) ->
    Pid = get_pid_from_ccb_handle(CcbHandle),
    safs_imm_om_ccb:object_create_2(Pid, ClassName, ParentName, AttrValues).

%%--------------------------------------------------------------------
%% Function: ccb_object_create_s2/5
%% Description:
%%--------------------------------------------------------------------
ccb_object_create_s2(CcbHandle, ClassName, ParentName, AttrValues, ExtraAttrValues) ->
    Pid = get_pid_from_ccb_handle(CcbHandle),
    safs_imm_om_ccb:object_create_s2(Pid, ClassName, ParentName, AttrValues, ExtraAttrValues).

%%--------------------------------------------------------------------
%% Function: ccb_object_delete/2
%% Description:
%%--------------------------------------------------------------------
ccb_object_delete(CcbHandle, ObjectName) ->
    Pid = get_pid_from_ccb_handle(CcbHandle),
    safs_imm_om_ccb:object_delete(Pid, ObjectName).

%%--------------------------------------------------------------------
%% Function: ccb_object_modify_2/3
%% Description:
%%--------------------------------------------------------------------
ccb_object_modify_2(CcbHandle, ObjectName, AttrMods) ->
    Pid = get_pid_from_ccb_handle(CcbHandle),
    safs_imm_om_ccb:object_modify_2(Pid, ObjectName, AttrMods).

%%--------------------------------------------------------------------
%% Function: ccb_object_read/3
%% Description:
%%--------------------------------------------------------------------
ccb_object_read(CcbHandle, ObjectName, AttributeNames) ->
    Pid = get_pid_from_ccb_handle(CcbHandle),
    safs_imm_om_ccb:object_read(Pid, ObjectName, AttributeNames, erlang).

%%--------------------------------------------------------------------
%% Function: c_ccb_object_read/3
%% Description:
%%--------------------------------------------------------------------
c_ccb_object_read(CcbHandle, ObjectName, AttributeNames) ->
    Pid = get_pid_from_ccb_handle(CcbHandle),
    safs_imm_om_ccb:object_read(Pid, ObjectName, AttributeNames, c).

%%--------------------------------------------------------------------
%% Function: ccb_validate/1
%% Description:
%%--------------------------------------------------------------------
ccb_validate(CcbHandle) ->
    Pid = get_pid_from_ccb_handle(CcbHandle),
    safs_imm_om_ccb:ccb_validate(Pid).

%%--------------------------------------------------------------------
%% Function: ccb_apply/1
%% Description:
%%--------------------------------------------------------------------
ccb_apply(CcbHandle) ->
    Pid = get_pid_from_ccb_handle(CcbHandle),
    safs_imm_om_ccb:ccb_apply(Pid).

%%--------------------------------------------------------------------
%% Function: ccb_abort/1
%% Description:
%%--------------------------------------------------------------------
ccb_abort(CcbHandle) ->
    Pid = get_pid_from_ccb_handle(CcbHandle),
    safs_imm_om_ccb:ccb_abort(Pid).

%%--------------------------------------------------------------------
%% Function: send_completed/1
%% Description:
%%--------------------------------------------------------------------
get_fun_list(CcbHandle) ->
    Pid = get_pid_from_ccb_handle(CcbHandle),
    safs_imm_om_ccb:get_fun_list(Pid).

%%--------------------------------------------------------------------
%% Function: send_completed/1
%% Description:
%%--------------------------------------------------------------------
send_completed(CcbHandle) ->
    Pid = get_pid_from_ccb_handle(CcbHandle),
    safs_imm_om_ccb:send_completed(Pid).

%%--------------------------------------------------------------------
%% Function: send_apply/1
%% Description:
%%--------------------------------------------------------------------
send_apply(CcbHandle) ->
    Pid = get_pid_from_ccb_handle(CcbHandle),
    safs_imm_om_ccb:send_apply(Pid).

%%--------------------------------------------------------------------
%% Function: send_abort/1
%% Description:
%%--------------------------------------------------------------------
send_abort(CcbHandle) ->
    ccb_abort(CcbHandle).

%%--------------------------------------------------------------------
%% Function: ccb_insert_fun/2
%% Description:
%%--------------------------------------------------------------------
ccb_insert_fun(CcbHandle, Fun) ->
    Pid = get_pid_from_ccb_handle(CcbHandle),
    safs_imm_om_ccb:insert_fun(Pid, Fun).

%%--------------------------------------------------------------------
%% Function: ccb_object_modify_extra_attrs_s2/3
%% Description:
%%--------------------------------------------------------------------
ccb_object_modify_extra_attrs_s2(CcbHandle, ObjectName, ExtraAttrMods) ->
    Pid = get_pid_from_ccb_handle(CcbHandle),
    safs_imm_om_ccb:object_modify_extra_attrs_s2(Pid, ObjectName, ExtraAttrMods).

%%--------------------------------------------------------------------
%% Function: ccb_object_read_extra_attrs_s2/2
%% Description:
%%--------------------------------------------------------------------
ccb_object_read_extra_attrs_s2(CcbHandle, ObjectName) ->
    Pid = get_pid_from_ccb_handle(CcbHandle),
    safs_imm_om_ccb:object_read_extra_attrs_s2(Pid, ObjectName, erlang).

%%--------------------------------------------------------------------
%% Function: ccb_finalize/1
%% Description:
%%--------------------------------------------------------------------
ccb_finalize(CcbHandle) ->
    call({ccb_finalize, CcbHandle}).

%%--------------------------------------------------------------------
%% Function: admin_operation_invoke_2/6
%% Description:
%%--------------------------------------------------------------------
admin_operation_invoke_2(OwnerHandle, ObjectName, _ContinuationId,
		      OperationId, Params, Timeout) ->
    call({admin_operation_invoke_2, OwnerHandle, ObjectName, _ContinuationId,
	  OperationId, Params, Timeout}).

%%--------------------------------------------------------------------
%% Function: admin_operation_invoke_o2/6
%% Description:
%%--------------------------------------------------------------------
admin_operation_invoke_o2(OwnerHandle, ObjectName, _ContinuationId,
		      OperationId, Params, Timeout) ->
    call({admin_operation_invoke_o2, OwnerHandle, ObjectName, _ContinuationId,
	  OperationId, Params, Timeout, erlang}).

%%--------------------------------------------------------------------
%% Function: c_admin_operation_invoke_o2/6
%% Description:
%%--------------------------------------------------------------------
c_admin_operation_invoke_o2(OwnerHandle, ObjectName, _ContinuationId,
		      OperationId, Params, Timeout) ->
    call({admin_operation_invoke_o2, OwnerHandle, ObjectName, _ContinuationId,
	  OperationId, Params, Timeout, c}).

%%--------------------------------------------------------------------
%% Function: admin_operation_invoke_async_2/6
%% Description:
%%--------------------------------------------------------------------
admin_operation_invoke_async_2(OwnerHandle, UserInvocation, ObjectName, ContinuationId,
			     OperationId, Params) ->
    admin_operation_invoke_async_2_common(OwnerHandle, UserInvocation, ObjectName, ContinuationId,
					  OperationId, Params, erlang).

%%--------------------------------------------------------------------
%% Function: admin_operation_invoke_async_2/6
%% Description:
%%--------------------------------------------------------------------
c_admin_operation_invoke_async_2(OwnerHandle, UserInvocation, ObjectName, ContinuationId,
			     OperationId, Params) ->
    admin_operation_invoke_async_2_common(OwnerHandle, UserInvocation, ObjectName, ContinuationId,
					  OperationId, Params, c).
%%--------------------------------------------------------------------
%% Function: admin_operation_invoke_async_2/6
%% Description:
%%--------------------------------------------------------------------
admin_operation_invoke_async_2_common(OwnerHandle, UserInvocation, ObjectName, ContinuationId,
			     OperationId, Params, Type) ->
    %% LATH: Check Owner?
    %%   If ContinuationId is non zero the resut shall be saved if the admin owner
    %%   has been terminated.
    %%   Timeout handling ?
    try
      {_OiFun, Oi, _Appliers} = safs_imm_db:oi_get(ObjectName),
      %%LATH: Should appliers get an invoke ??
      case Oi of
	  undefined ->
	      {error, sa_ais_err_not_exist};
	  _ ->
	      Invocation = create_invocation_id(),
	      AdmOp = #safs_imm_om_admin_operation{invocation = Invocation,
						   user_invocation = UserInvocation,
						   owner = OwnerHandle,
						   type = Type,
						   op = {Oi, Invocation, ObjectName, OperationId, Params},
						   continuation = ContinuationId},
	      insert_tbl(safs_imm_om_admin_operation, AdmOp),

	      safs_imm_oi:admin_operation_2(Oi, Invocation, ObjectName, OperationId, Params),
	      ok
      end
    catch
	throw:no_exists ->
	   {error, sa_ais_err_not_exist};
	throw:Term -> Term
    end.

%%--------------------------------------------------------------------
%% Function: admin_operation_continue/3
%% Description:
%%--------------------------------------------------------------------
admin_operation_continue(_OwnerHandle, _ObjectName, _ContinuationId) ->
    {error, sa_ais_err_bad_operation}.

%%--------------------------------------------------------------------
%% Function: admin_continue_async/4
%% Description:
%%--------------------------------------------------------------------
admin_operation_continue_async(_OwnerHandle, _Invocation, _ObjectName, _ContinuationId) ->
    {error, sa_ais_err_bad_operation}.

%%--------------------------------------------------------------------
%% Function: callbacks_initialized/1
%% Description:
%%--------------------------------------------------------------------
callbacks_initialize(Handle) ->
    call({callbacks_initialize, Handle, self()}).

%%--------------------------------------------------------------------
%% Function: initialize/1
%% Description:
%%--------------------------------------------------------------------
initialize_o2(Callbacks) ->
    initialize_o2(Callbacks, #safsVersion{releaseCode=?IMM_RELEASE_CODE,
					  majorVersion=?IMM_MAJOR_VERSION,
					  minorVersion=?IMM_MINOR_VERSION}).

%%--------------------------------------------------------------------
%% Function: initialize_o2/2
%% Description:
%%--------------------------------------------------------------------
initialize_o2(Callbacks, #safsVersion{} = Version) when is_atom(Callbacks);
							is_function(Callbacks, 1);
							is_record(Callbacks, safsImmCallbacks_o2) ->
    case safs_imm_lib:validate_version(Version) of
	{ok, Version1} ->
	    call({initialize, Callbacks, Version1, self()});
	{error, sa_ais_err_version} ->
	    {error, sa_ais_err_version}
    end;
initialize_o2(_Callbacks, _Version) ->
    {error, sa_ais_err_invalid_param}.

%%--------------------------------------------------------------------
%% Function: affected_by_ccb/1
%% Description:
%%--------------------------------------------------------------------
affected_by_ccb(ClassName) ->
    try
	lists:any(
	  fun(#safs_imm_om_ccb{pid=Pid}) ->
		  safs_imm_om_ccb:affected_by_ccb(Pid, ClassName)
	  end,
	  ets:tab2list(safs_imm_om_ccb))
    catch
	_:_ -> false
    end.


%%--------------------------------------------------------------------
%% Function: affected_by_ccb/2
%% Description:
%%--------------------------------------------------------------------
affected_by_ccb(DN, Scope) ->
    try
	lists:any(
	  fun(#safs_imm_om_ccb{pid=Pid}) ->
		  safs_imm_om_ccb:affected_by_ccb(Pid, DN, Scope)
	  end,
	  ets:tab2list(safs_imm_om_ccb))
    catch
	_:_ -> false
    end.
%%--------------------------------------------------------------------
%% Function: admin_operation_result/2
%% Description:
%%--------------------------------------------------------------------
admin_operation_result(Invocation, OpRetVal) ->
    try
	AdmOp = lookup_tbl(safs_imm_om_admin_operation, Invocation),
        delete_tbl(safs_imm_om_admin_operation, Invocation), %% Call to server instead of public ?
         case AdmOp#safs_imm_om_admin_operation.user_invocation of
	     undefined ->
		 ?SAFS_OM_DEBUG(admin_operation_result,
				lists:flatten(io_lib:format("sync from: ~w",
							    [AdmOp#safs_imm_om_admin_operation.from]))),
		 case AdmOp#safs_imm_om_admin_operation.version of
		     2 ->
			 gen_server:reply(AdmOp#safs_imm_om_admin_operation.from, {ok, OpRetVal});
		     o2 ->
			 gen_server:reply(AdmOp#safs_imm_om_admin_operation.from, {ok, OpRetVal, []})
		 end,
		 ok;
	     UserInvocation ->
		 ?SAFS_OM_DEBUG(admin_operation_result,
				lists:flatten(io_lib:format("async from: ~w",
							    [AdmOp#safs_imm_om_admin_operation.from]))),

		 safs_imm_om:admin_operation_invoke_callback(AdmOp#safs_imm_om_admin_operation.owner,
							     UserInvocation,
							     OpRetVal,
							     ok,
							     [])
	 end
    catch
	throw:Term -> Term
    end.


%%--------------------------------------------------------------------
%% Function: admin_operation_result_o2/3
%% Description:
%%--------------------------------------------------------------------
admin_operation_result_o2(Invocation, OpRetVal, RetParams) ->
    try
	AdmOp = lookup_tbl(safs_imm_om_admin_operation, Invocation),
        delete_tbl(safs_imm_om_admin_operation, Invocation), %%LATH: Call to server instead of public ?
	RetParams2 = convert_parameter_list(RetParams, AdmOp#safs_imm_om_admin_operation.type),
        case AdmOp#safs_imm_om_admin_operation.user_invocation of
	    undefined ->
		?SAFS_OM_DEBUG(admin_operation_result_o2,
			       lists:flatten(io_lib:format("sync from: ~p",
							   [AdmOp#safs_imm_om_admin_operation.from]))),
		case AdmOp#safs_imm_om_admin_operation.version of
		    2 ->
			gen_server:reply(AdmOp#safs_imm_om_admin_operation.from, {ok, OpRetVal});
		    o2 ->
			gen_server:reply(AdmOp#safs_imm_om_admin_operation.from, {ok, OpRetVal, RetParams2})
		end,
		ok;
	    UserInvocation ->
		?SAFS_OM_DEBUG(admin_operation_result_o2,
			       lists:flatten(io_lib:format("async from: ~p",
							   [AdmOp#safs_imm_om_admin_operation.from]))),

		safs_imm_om:admin_operation_invoke_callback(AdmOp#safs_imm_om_admin_operation.owner,
							    UserInvocation,
							    OpRetVal,
							    ok,
							    RetParams2)
	end
    catch
	throw:Term -> Term
    end.

%%--------------------------------------------------------------------
%% Function: get_error_strings/1
%% Description:
%%--------------------------------------------------------------------
ccb_get_error_strings(CcbHandle) ->
    try
      %% Check that the CCB exists

      #safs_imm_om_ccb{id=CcbId} = lookup_tbl(safs_imm_om_ccb,  CcbHandle),
      ErrorStrings = lookup_bag_tbl(safs_imm_om_ccb_error, CcbId),
      {ok, [String || #safs_imm_om_ccb_error{string = String} <- ErrorStrings]}
    catch
      throw:Term -> Term
    end.

%%--------------------------------------------------------------------
%% Function: set_error_strings/2
%% Description:
%%--------------------------------------------------------------------
ccb_set_error_string(CcbId, String) ->
    try
      %% Check that the CCB exist
      case  ets:match_object(safs_imm_om_ccb, {safs_imm_om_ccb, '_', '_', CcbId, '_'}) of
	  [_Ccb] ->
	      insert_tbl(safs_imm_om_ccb_error, #safs_imm_om_ccb_error{id = CcbId, string = String});
	  [] ->
	      {error, sa_ais_err_bad_operation}
      end
    catch
      throw:Term -> Term
    end.

%%--------------------------------------------------------------------
%% Function: delete_error_strings/1
%% Description:
%%--------------------------------------------------------------------
ccb_delete_error_strings(CcbId) ->
    try
      %% Check that the CCB exists
      [_Ccb] = ets:match_object(safs_imm_om_ccb, {safs_imm_om_ccb, '_', '_', CcbId, '_'}),
      delete_tbl(safs_imm_om_ccb_error, CcbId)
    catch
      throw:Term -> Term
    end.

%%--------------------------------------------------------------------
%% Function: get_ccbid_from_handle/1
%% Description:
%%--------------------------------------------------------------------
get_ccbid_from_handle(CcbHandle) ->
    try
	#safs_imm_om_ccb{id=CcbId} = lookup_tbl(safs_imm_om_ccb,  CcbHandle),
        {ok, CcbId}
    catch
      throw:Term -> Term
    end.

%%====================================================================
%% Callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: admin_operation_invoke_callback/5
%% Description:
%%--------------------------------------------------------------------
admin_operation_invoke_callback(Handle, Invocation, OperationReturnValue, Error, Params) ->
    cast({callback, Handle,
	  {admin_operation_invoke_callback, {Invocation, OperationReturnValue, Error, Params}}}).

%%====================================================================
%% Other
%%====================================================================
%%--------------------------------------------------------------------
%% Function: set_external_pid/2
%% Description:
%%--------------------------------------------------------------------
set_external_pid(Handle, Pid) ->
     call({set_external_pid, Handle, Pid}).

%%--------------------------------------------------------------------
%% Function: augment_ccb_initialize/3
%% Description:
%%--------------------------------------------------------------------
augment_ccb_initialize(CcbId, OiName, Pid) ->
     call({augment_ccb_initialize, CcbId, OiName, Pid}).

%%--------------------------------------------------------------------
%% Function: adm_owner/1
%% Description:
%%--------------------------------------------------------------------
adm_owner(OwnerHandle) ->
    lookup_tbl(safs_imm_om_adm_owner, OwnerHandle).

%%--------------------------------------------------------------------
%% Function: add_ao_owned_object/2
%% Description:
%%--------------------------------------------------------------------
add_ao_owned_object(AdminOwner, DN) ->
    AO = adm_owner(AdminOwner),
    insert_tbl(safs_imm_om_administrated_objects,
	       #safs_imm_om_administrated_objects{key={AO#safs_imm_om_adm_owner.name, DN}}).

%%====================================================================
%% Tracing
%%====================================================================
trace_groups() -> [life_cycle, class, search, accessor, admin_owner, ccb,
		   admin_operation, error, callbacks].

trace_points_list() ->
    [
     {error,
      [{safs_error, 2}]},
     {callbacks,
      [{handle_callback, 2},
       {handle_callback, 3},
       {handle_erlang_callback, 3},
       {handle_reg_callback, 3},
       {admin_operation_invoke_callback, 4},
       {admin_operation_invoke_callback_o2, 5},
       {admin_operation_result_o2, 3},
       {admin_operation_result, 2}
      ]},
     {life_cycle,
      [{initialize, 2},
       {initialize_o2, 2},
       {callbacks_initialize, 1},
       {finalize, 1}]},
     {class,
      [{class_create_2, 4},
       {class_description_get_2, 2},
       {class_delete, 2}]},
     {search,
      [{search_initialize_2, 6},
       {search_next_2, 1},
       {c_search_next_2, 1},
       {search_next_n_s2, 2},
       {c_search_next_n_s2, 2},
       {search_finalize, 1}]},
     {accessor,
      [{accessor_initialize, 1},
       {accessor_get_2, 3},
       {c_accessor_get_2, 3},
       {accessor_finalize, 1}]},
     {admin_owner,
      [{admin_owner_initialize, 3},
       {admin_owner_finalize, 1},
       {admin_owner_set, 3},
       {admin_owner_release, 3},
       {admin_owner_clear, 3}]},
     {ccb,
      [{ccb_initialize, 2},
       {ccb_object_create_2, 4},
       {ccb_object_delete, 2},
       {ccb_object_modify_2, 3},
       {ccb_object_read, 3},
       {c_ccb_object_read, 3},
       {ccb_validate, 1},
       {ccb_apply, 1},
       {ccb_abort, 1},
       {ccb_finalize, 1},
       {send_completed, 1},
       {send_apply, 1},
       {send_abort, 1}]},
     {admin_operation,
      [{admin_operation_invoke_2, 6},
       {admin_operation_invoke_o2, 6},
       {c_admin_operation_invoke_o2, 6},
       {admin_operation_invoke_async_2, 6},
       {c_admin_operation_invoke_async_2, 6},
       {admin_operation_continue, 3},
       {admin_operation_continue_async, 4}]}
    ].

%%--------------------------------------------------------------------
%% Function: safs_error/2
%% Description:
%%--------------------------------------------------------------------
safs_error(_F, _A) ->  ok.

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    ets:new(safs_imm_om_user,
	    [named_table,
	     {keypos, #safs_imm_om_user.handle}]),
    ets:new(safs_imm_om_search_data,
	    [named_table,
	     {keypos, #safs_imm_om_search_data.search_handle}]),
    ets:new(safs_imm_om_accessor,
	    [named_table,
	     {keypos, #safs_imm_om_accessor.accessor_handle}]),
    ets:new(safs_imm_om_adm_owner,
	    [named_table, public,
	     {keypos, #safs_imm_om_adm_owner.owner_handle}]),
    ets:new(safs_imm_om_administrated_objects,
	    [named_table, public,
	     {keypos, #safs_imm_om_administrated_objects.key}]),
    ets:new(safs_imm_om_admin_operation,
	    [named_table, public,
	     {keypos, #safs_imm_om_admin_operation.invocation}]),
    ets:new(safs_imm_om_ccb,
     	    [named_table, public,
     	     {keypos, #safs_imm_om_ccb.handle}]),
     ets:new(safs_imm_om_ccb_error,
	     [named_table, public, bag,
	      {keypos, #safs_imm_om_ccb_error.id}]),
    ets:new(safs_imm_om_id, [named_table, public]),
    ets:insert(safs_imm_om_id, {handleId, 17}),
    ets:insert(safs_imm_om_id, {ccbId, 4711}),
    ets:insert(safs_imm_om_id, {invocationId, 1}),
    case safs_imm_db:init() of
	{error, Error, Pos} ->
	    {stop, {error, Error, Pos}};
	_ ->
	    process_flag(trap_exit, true),
	    {ok, #state{}}
    end.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
%% Life Cycle
handle_call({initialize, Callbacks, Version, Proxy}, _From, State) ->
    Handle = create_handle(),
    User = #safs_imm_om_user{handle = Handle,
			     version = Version,
			     callbacks = Callbacks,
			     proxy = Proxy
			    },
    insert_tbl(safs_imm_om_user, User),
    erlang:monitor(process, Proxy),
    Reply = {ok, Handle, Version},
    {reply, Reply, State};

handle_call({callbacks_initialize, Handle, CbProxy}, _From, State) ->
    try
	User = lookup_tbl(safs_imm_om_user, Handle),
        erlang:monitor(process, CbProxy),
	insert_tbl(safs_imm_om_user,
		   User#safs_imm_om_user{cb_proxy = CbProxy}),
        {reply, ok, State}
    catch
	throw:Error -> {reply, Error, State}
    end;
handle_call({finalize, Handle}, _From, State) ->
    try
	om_finalize_cleanup(Handle),
        {reply, ok, State}
    catch
	throw:Error -> {reply, Error, State}
    end;
%% Search
handle_call({search_initialize_2, Handle, RootName, Scope, SearchOptions,
	     SearchParams, AttributeNames}, _From, State) ->

    SearchHandle = create_handle(),
    {ok, Pid} = safs_imm_om_search:start_link(),
    SearchData = #safs_imm_om_search_data{search_handle = SearchHandle,
					  imm_handle = Handle,
					  rootname = RootName,
					  scope = Scope,
					  options = convert_search_options(SearchOptions),
					  params = convert_search_params(SearchParams),
					  attributes = convert_search_attributes(AttributeNames),
					  pid = Pid
					 },
    insert_tbl(safs_imm_om_search_data, SearchData),
    {reply, {ok, SearchHandle, Pid}, State};
handle_call({search_class_initialize_s2, Handle, RootName, Scope, SearchOptions,
	     ClassNames, AttributeNames}, _From, State) ->

    SearchHandle = create_handle(),
    {ok, Pid} = safs_imm_om_search:start_link(),
    SearchData = #safs_imm_om_search_data{search_handle = SearchHandle,
					  imm_handle = Handle,
					  rootname = RootName,
					  scope = Scope,
					  options = convert_search_options(SearchOptions),
					  classes = lists:map(fun(Name) -> binary_to_atom(Name, utf8) end, ClassNames),
					  %% params = {'SaImmAttrClassName', lists:map(fun(Name) -> binary_to_atom(Name, utf8) end, ClassNames)},
					  attributes = convert_search_attributes(AttributeNames),
					  pid = Pid
					 },
    insert_tbl(safs_imm_om_search_data, SearchData),
    {reply, {ok, SearchHandle, Pid}, State};
handle_call({delete_search_data, Handle}, _From, State) ->
    delete_tbl(safs_imm_om_search_data, Handle),
    {reply, ok, State};
handle_call({search_finalize, Handle}, _From, State) ->
    Reply =
	try
	    SearchData = lookup_tbl(safs_imm_om_search_data, Handle),
            sd_finalize_cleanup(SearchData)
	catch
	    throw:Term -> Term
        end,
    {reply, Reply, State};

%% Accessor
handle_call({accessor_initialize, Handle}, _From, State) ->
    AccessorHandle = create_handle(),

    Accessor = #safs_imm_om_accessor{accessor_handle = AccessorHandle,
				      imm_handle = Handle
				     },
    insert_tbl(safs_imm_om_accessor, Accessor),
    Reply = {ok, AccessorHandle},
    {reply, Reply, State};
handle_call({accessor_finalize, AccessorHandle}, _From, State) ->
     Reply =
	try
            delete_tbl(safs_imm_om_accessor, AccessorHandle),
            ok
	catch
	    throw:Term -> Term
        end,
    {reply, Reply, State};

%% Admin Owner
handle_call({admin_owner_initialize, Handle, AOName, ReleaseOwnership}, _From, State) ->

    MatchSpec = #safs_imm_om_adm_owner{owner_handle = '_',
				       imm_handle = '_',
				       name = AOName,
				       release_on_finalize = '_'},

    Reply =
	case ets:match_object(safs_imm_om_adm_owner, MatchSpec) of
	    [] ->
		AOHandle = create_handle(),
		AO = #safs_imm_om_adm_owner{owner_handle = AOHandle,
					    imm_handle = Handle,
					    name = AOName,
					    release_on_finalize = ReleaseOwnership
					   },
		insert_tbl(safs_imm_om_adm_owner, AO),
		{ok, AOHandle};
	    _ ->
		{error, sa_ais_err_exist}
	end,
    {reply, Reply, State};
handle_call({admin_owner_finalize, OwnerHandle}, _From, State) ->
    Reply =
	try
	    AO = lookup_tbl(safs_imm_om_adm_owner, OwnerHandle),
            ao_finalize_cleanup(AO)
	catch
	    throw:Term -> Term
        end,
    {reply, Reply, State};

%% CCB
handle_call({ccb_initialize, OwnerHandle, CcbFlags}, _From, State) ->
    %% Check Owner
    CcbHandle = create_handle(),
    CcbId = create_ccb_id(),

    case safs_imm_om_ccb_sup:start_ccb(CcbId, OwnerHandle, CcbFlags) of
	{ok, Pid} when is_pid(Pid) ->
	    link(Pid),
	    insert_tbl(safs_imm_om_ccb,
		       #safs_imm_om_ccb{handle = CcbHandle,
					owner_handle = OwnerHandle,
					id = CcbId,
					pid = Pid}),
	    {reply, {ok, CcbHandle}, State};
	_ ->
	    %% Couldn't start proc
	    {reply, {error, sa_ais_err_no_resources}, State}
    end;

handle_call({ccb_finalize, CcbHandle}, _From, State) ->
    %% Check Owner
    try
        Ccb = lookup_tbl(safs_imm_om_ccb,  CcbHandle),
        Reply = ccb_finalize_cleanup(Ccb),
        {reply, Reply, State}
    catch
	throw:Term ->
	    {reply, Term, State}
    end;

handle_call({admin_operation_invoke_2, OwnerHandle, ObjectName, ContinuationId,
	     OperationId, Params, _Timeout}, From, State) ->
    %% LATH: Check Owner?
    %%   If ContinuationId is non zero the resut shall be saved if the admin owner
    %%   has been terminated.
    %%   Timeout handling ?
    try
      {_OiFun, Oi, _Appliers} = safs_imm_db:oi_get(ObjectName),
      %% LATH: Should appliers get an invoke ??
      case Oi of
	  undefined ->
	      {reply, {error, sa_ais_err_not_exist}, State};
	  _ ->
	      Invocation = create_invocation_id(),
	      AdmOp = #safs_imm_om_admin_operation{invocation = Invocation,
						   owner = OwnerHandle,
						   version = 2,
						   op = {Oi, Invocation, ObjectName, OperationId, Params},
						   continuation = ContinuationId,
						   from = From},
	      insert_tbl(safs_imm_om_admin_operation, AdmOp),

	      safs_imm_oi:admin_operation_2(Oi, Invocation, ObjectName, OperationId, Params),
	      {noreply, State}
      end
    catch
	throw:no_exists ->
	   {reply, {error, sa_ais_err_not_exist}, State};
	throw:Term -> {reply, Term, State}
    end;

handle_call({admin_operation_invoke_o2, OwnerHandle, ObjectName, ContinuationId,
	     OperationId, Params, _Timeout, Type}, From, State) ->
    %% LATH: Check Owner?
    %%   If ContinuationId is non zero the resut shall be saved if the admin owner
    %%   has been terminated.
    %%   Timeout handling ?
    try
      {_OiFun, Oi, _Appliers} = safs_imm_db:oi_get(ObjectName),
      %% LATH: Should appliers get an invoke ??
      case Oi of
	  undefined ->
	      {reply, {error, sa_ais_err_not_exist}, State};
	  _ ->
	      Invocation = create_invocation_id(),
	      AdmOp = #safs_imm_om_admin_operation{invocation = Invocation,
						   owner = OwnerHandle,
						   version=o2,
						   type = Type,
						   op = {Oi, Invocation, ObjectName, OperationId, Params},
						   continuation = ContinuationId,
						   from = From},
	      insert_tbl(safs_imm_om_admin_operation, AdmOp),

	      safs_imm_oi:admin_operation_2(Oi, Invocation, ObjectName, OperationId, Params),
	      {noreply, State}
      end
    catch
	throw:no_exists ->
	   {reply, {error, sa_ais_err_not_exist}, State};
	throw:Term -> {reply, Term, State}
    end;

handle_call({callback, Handle, CBA}, From, State) ->
    case handle_callback(Handle, CBA, From) of
	{reply, Reply} ->
	    {reply, Reply, State};
	noreply ->
	    {noreply, State}
    end;

handle_call({set_external_pid, Handle, Pid}, _From, State) ->
    try
	User = lookup_tbl(safs_imm_om_user, Handle),
        insert_tbl(safs_imm_om_user, User#safs_imm_om_user{external_pid = Pid}),
        {reply, ok, State}
    catch
	throw:Term -> {reply, Term, State}
    end;

handle_call({augment_ccb_initialize, CcbId, OiName, OwnerPid}, _From, State) ->
    try
        Ccb = get_ccb_from_ccb_id(CcbId),
        ParentPid = Ccb#safs_imm_om_ccb.pid,
        AoHandle = Ccb#safs_imm_om_ccb.owner_handle,
        AugCcbHandle = create_handle(),

        case safs_imm_om_ccb_sup:start_ccb(CcbId) of
	    {ok, Pid} when is_pid(Pid) ->
		link(Pid),
		{ok, AoHandle} =
		    safs_imm_om_ccb:augment_ccb_initialize(Pid, ParentPid, OiName, OwnerPid),

		insert_tbl(safs_imm_om_ccb,
			   #safs_imm_om_ccb{handle = AugCcbHandle,
					    owner_handle = AoHandle,
					    id = augmentation, %%%LATH CCB
					    pid = Pid}),
		{reply, {ok, AugCcbHandle, AoHandle}, State};
	    Reason ->
		safs_error(handle_call, {augment_ccb_initialize,
					 "Couldn't start process for augmented ccb",
					 Reason}),
		{reply, {error, sa_ais_err_no_resources}, State}
	end
     catch
 	throw:Term -> {reply, Term, State}
     end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Msg, From, #state{} = State) ->
    error_logger:format("~p ~p got unexpected call from ~p:\n\t~p\n",
                        [?MODULE, self(), From, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({callback, Handle, CBA}, State) ->
    handle_callback(Handle, CBA),
    {noreply, State};
handle_cast(Msg, #state{} = State) ->
    error_logger:format("~p~p got unexpected cast:\n\t~p\n",
                        [?MODULE, self(), Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({callback_reply, _Cb, Handle, Reply}, State) ->
    % io:format("Got callback reply: ~p~n", [Reply]),
    case lookup_tbl(safs_imm_om_user, Handle) of
	{ok, #safs_imm_om_user{reply_to = To} = User}
	  when To =/= undefined ->
	    gen_server:reply(To, Reply),
	    insert_tbl(safs_imm_om_user,
		       User#safs_imm_om_user{reply_to = undefined});
	_Other ->
	    ok
    end,
    {noreply, State};

handle_info({'DOWN', _Ref, _Type, Pid, _Reason}, State) ->
    case get_user_from_proxy(Pid) of
	[] ->
	    ok;
	Users ->
	    [handle_user_down(User, Pid, State) || User <- Users]
    end,
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) when Reason =/= normal ->
    % error_logger:warning_msg("~p(~p) got exit from: ~p with reason: ~p\n",
    % 			     [?MODULE, self(), Pid, Reason]),
    case get_handle_from_pid(Pid) of
	{error, no_entry} ->
	    ok;
	Handle ->
    	    delete_tbl(safs_imm_om_ccb, Handle)
    end,
    {noreply, State};

handle_info(_Msg, #state{} = State) ->
    % error_logger:info_msg("~p~p got unexpected message:\n\t~p\n",
    % 			  [?MODULE, self(), Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change/3
%% Description: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
%%----------------------------------------------------------------------
%% Callbacks
%%----------------------------------------------------------------------
%% @private
handle_callback(Handle, CBA) ->
    handle_callback(Handle, CBA, undefined).

%%--------------------------------------------------------------------
%% @private
handle_callback(Handle, {admin_operation_invoke_callback, Pars}, From) ->
    try
        AO = lookup_tbl(safs_imm_om_adm_owner, Handle),
	case lookup_tbl(safs_imm_om_user, AO#safs_imm_om_adm_owner.imm_handle) of
	    User when User#safs_imm_om_user.reply_to =/= undefined,
	              From =/= undefined ->
		error_logger:info_msg("safs_imm_om: admin_operation_invoke_callback\n"
				      "Waiting for other cb to finish", []),
		{reply, {error, sa_ais_err_try_again}};
	    User when User#safs_imm_om_user.cb_proxy =/= undefined ->
		CBA = choose_adm_op_callback_version(User#safs_imm_om_user.callbacks, Pars),
		handle_reg_callback(User, CBA, From);
	    User ->
		%% Handling of erlang callbacks
		CBA = choose_adm_op_callback_version(User#safs_imm_om_user.callbacks, Pars),
		spawn_link(fun() ->
				   handle_erlang_callback(User, CBA, From),
				   exit(normal)
			   end),
		noreply
	end
    catch
	throw:Term -> {reply, Term}
    end;
handle_callback(Handle, {Cb, _} = CBA, From) ->
    try
        AO = lookup_tbl(safs_imm_om_adm_owner, Handle),
	case lookup_tbl(safs_imm_om_user, AO#safs_imm_om_adm_owner.imm_handle) of
	    User when User#safs_imm_om_user.reply_to =/= undefined,
	              From =/= undefined ->
		error_logger:info_msg("safs_imm_om: ~p\n"
				      "Waiting for other cb to finish", [Cb]),
		{reply, {error, sa_ais_err_try_again}};
	    User when User#safs_imm_om_user.cb_proxy =/= undefined ->
		handle_reg_callback(User, CBA, From);
	    User ->
		%% Handling of erlang callbacks,  only used for testing
		spawn_link(fun() ->
				   handle_erlang_callback(User, CBA, From),
				   exit(normal)
			   end),
		noreply
	end
    catch
	throw:Term -> {reply, Term}
    end.

%%--------------------------------------------------------------------
choose_adm_op_callback_version(Cb, {Invocation, OperationReturnValue, Error, _Params})
  when is_record(Cb, safsImmCallbacks) ->
    {admin_operation_invoke_callback, {Invocation, OperationReturnValue, Error}};
choose_adm_op_callback_version(Cb, Pars)
  when is_record(Cb, safsImmCallbacks_o2) ->
    {admin_operation_invoke_callback_o2, Pars};
choose_adm_op_callback_version(Cb, Pars) when is_atom(Cb); is_function(Cb, 1) -> %% Just from erlang API, chooses latest version
    {admin_operation_invoke_callback_o2, Pars}.

%%----------------------------------------------------------------------
%% Handling of erlang callbacks,  only used for testing
handle_erlang_callback(User, {Cb, Args}, From) ->
     ModOrFun = is_registered_cb(Cb, User#safs_imm_om_user.callbacks),

     Reply =
 	case catch execute_cb(ModOrFun, Cb, tuple_to_list(Args)) of
 	    ok ->
		ok;
 	    {error, ErrorVal} ->
 		{error, ErrorVal};
	    Error ->
		safs_error(handle_erlang_callback,
			   io_lib:format("~p callback failed: ~p\n",
					 [{Cb, Args}, Error])),
 		{error, sa_ais_err_failed_operation}
 	end,
    case From of
	undefined ->
	    ok;
 	_ ->
 	    gen_server:reply(From, Reply)
    end.

%%--------------------------------------------------------------------
execute_cb(undefined, _F, _Args)  ->
    ok;
execute_cb(false, _F, _Args)  ->
    ok;
execute_cb(ModOrFun, F, Args) when is_function(ModOrFun, 1) ->
    ModOrFun(list_to_tuple([F |Args]));
execute_cb(ModOrFun, F, Args) when is_atom(ModOrFun) ->
    apply(ModOrFun, F, Args).

%%----------------------------------------------------------------------
%% Handle C callbacks
%% @private
handle_reg_callback(User, {Cb, Arg}, From) ->
    case is_registered_cb(Cb, User#safs_imm_om_user.callbacks) of
 	true ->
 	    Handle = User#safs_imm_om_user.handle,
 	    Pid = User#safs_imm_om_user.cb_proxy,
 	    send_callback(From, Pid, Cb, Handle, Arg, User);
 	_False ->
 	    {reply, ok}
    end.

%%--------------------------------------------------------------------
%% @private
send_callback(undefined, Pid, Cb, Handle, Arg, _User) ->
    safs_imm_om_com:callback_message_async(Pid, Cb, Handle, Arg);
send_callback(From, Pid, Cb, Handle, Arg, User) ->
    case safs_imm_om_com:callback_message_sync(Pid, Cb, Handle, Arg) of
 	ok ->
 	    NewUser = User#safs_imm_om_user{reply_to = From},
 	    insert_tbl(safs_imm_om_user, NewUser),
 	    noreply;
 	Error ->
 	    {reply, Error}
    end.


%%----------------------------------------------------------------------
%% @private
%%----------------------------------------------------------------------
call(Request) ->
    try
	gen_server:call(?SERVER, Request, infinity)
    catch
    	exit:{noproc, _} ->
    		      {error, sa_ais_err_bad_handle};
    	exit:{normal, _} ->
    		      {error, sa_ais_err_bad_handle};
    	exit:{killed, _} ->
    		      {error, sa_ais_err_bad_handle};
    	exit:{{shutdown, _},_} ->
    		      {error, sa_ais_err_bad_handle}
    end.

cast(Request) ->
    try
	gen_server:cast(?SERVER, Request)
    catch
    	exit:{noproc, _} ->
    		      {error, sa_ais_err_bad_handle};
    	exit:{normal, _} ->
    		      {error, sa_ais_err_bad_handle};
    	exit:{killed, _} ->
    		      {error, sa_ais_err_bad_handle};
    	exit:{{shutdown, _},_} ->
    		      {error, sa_ais_err_bad_handle}
    end.

%%----------------------------------------------------------------------
%% @private
%%----------------------------------------------------------------------
create_handle() ->
    ets:update_counter(safs_imm_om_id, handleId, {2, 1, 16#7FFFFFFFFFFFFFFF, 17}).

%%--------------------------------------------------------------------
create_ccb_id() ->
    ets:update_counter(safs_imm_om_id, ccbId, {2, 1, 4200000000, 4711}).

%%--------------------------------------------------------------------
create_invocation_id() ->
    ets:update_counter(safs_imm_om_id, invocationId, {2, 1, 16#7FFFFFFFFFFFFFFF, 1}).

%%--------------------------------------------------------------------
insert_tbl(Table, Object) ->
    ets:insert(Table, Object),
    ok.

%%--------------------------------------------------------------------
lookup_tbl(Table, Handle) ->
    try
	case ets:lookup(Table, Handle) of
	    [] ->
		throw({error, sa_ais_err_bad_handle});
	    [Object] ->
		Object
	end
    catch
	'EXIT':_ ->
           throw({error, sa_ais_err_bad_handle})
    end.

lookup_bag_tbl(Table, Handle) ->
    try
	ets:lookup(Table, Handle)
    catch
	'EXIT':_ ->
           throw({error, sa_ais_err_bad_handle})
    end.

%%--------------------------------------------------------------------
delete_tbl(Table, Handle) ->
    try
	ets:delete(Table, Handle),
        ok
    catch
	'EXIT':_ ->
           throw({error, sa_ais_err_bad_handle})
    end.

%%--------------------------------------------------------------------
get_pid_from_ccb_handle(CcbHandle) ->
    Ccb = lookup_tbl(safs_imm_om_ccb,  CcbHandle),
    Ccb#safs_imm_om_ccb.pid.

get_ccbid_from_ccb_handle(CcbHandle) ->
    Ccb = lookup_tbl(safs_imm_om_ccb,  CcbHandle),
    Ccb#safs_imm_om_ccb.id.

%%--------------------------------------------------------------------
get_handle_from_pid(Pid) ->
    case ets:match_object(safs_imm_om_ccb, {safs_imm_om_ccb,'_','_','_',Pid}) of
	[Ccb] ->
	    Ccb#safs_imm_om_ccb.handle;
	[] ->
	    {error, no_entry}
    end.

%%--------------------------------------------------------------------
get_pid_from_ccb_id(CcbId) ->
    [Ccb] = ets:match_object(safs_imm_om_ccb, {safs_imm_om_ccb,'_','_',CcbId,'_'}),
    Ccb#safs_imm_om_ccb.pid.

%%--------------------------------------------------------------------
get_ccb_from_ccb_id(CcbId) ->
    case ets:match_object(safs_imm_om_ccb, {safs_imm_om_ccb,'_','_',CcbId,'_'}) of
	[Ccb] ->
	    Ccb;
	_ ->
	    error_logger:info_msg("safs_imm_om:augment_ccb_initialize()\n"
				  "Non existent CcbId: ~p used for augmenting\n",
				 [CcbId]),
	    safs_error(handle_call,
		       {augment_ccb_initialize,
			"Non existent CcbId used for augmenting"}),
	    throw({error, sa_ais_err_bad_operation})
    end.

%%--------------------------------------------------------------------
change_ccbid_in_ccb(OldCcbId, NewCcbId) ->
    [Ccb] = ets:match_object(safs_imm_om_ccb, {safs_imm_om_ccb,'_','_',OldCcbId,'_'}),
    insert_tbl(safs_imm_om_ccb, Ccb#safs_imm_om_ccb{id = NewCcbId}),
    ok.

%%--------------------------------------------------------------------
apply_db(OpList) ->
    mnesia:transaction(
      fun() ->
	      [Op() || Op <- OpList],
	      ok
      end
     ).

%%--------------------------------------------------------------------
%% Function: convert_scope/1
%% Description:
%%--------------------------------------------------------------------
convert_scope(sa_imm_one) -> one;
convert_scope(sa_imm_sublevel) -> sublevel;
convert_scope(sa_imm_subtree) -> subtree.

%%--------------------------------------------------------------------
%% Function: convert_search_options/1
%% Description:
%%--------------------------------------------------------------------
convert_search_options(SearchOption) when is_integer(SearchOption) ->
    if
	SearchOption band 16#0100 =:= 16#0100     -> all;
	SearchOption band 16#0200 =:= 16#0200     -> no;
	SearchOption band 16#0400 =:= 16#0400     -> some;
	SearchOption band 16#010000 =:= 16#010000 -> config;
	SearchOption band 16#0100000000000000  =:= 16#0100000000000000 -> persistent
    end.

%%--------------------------------------------------------------------
%% Function: ao_set/3
%% Description:
%%--------------------------------------------------------------------
ao_set(<<>>, _, _) ->
    throw({error, sa_ais_err_invalid_param});
ao_set(ObjectName, AoName, Scope) ->
   safs_imm_db:ao_set(ObjectName, AoName, Scope).

%%--------------------------------------------------------------------
%% Function: convert_search_params/1
%% Description:
%%--------------------------------------------------------------------
convert_search_params(#safsImmSearchParameters_2{searchOneAttr=undefined} = _SearchParams) ->
    undefined;
convert_search_params(#safsImmSearchParameters_2{searchOneAttr=OneAttr} = _SearchParams) ->
   AttrValue =
	case OneAttr#safsImmSearchOneAttr_2.attrValue of
	    undefined -> undefined;
	    AVal ->
		{ok, [Value]} = safs_imm_lib:get_attr_values(OneAttr#safsImmSearchOneAttr_2.attrValueType,
						[AVal]),
		Value
	end,
    {binary_to_atom(OneAttr#safsImmSearchOneAttr_2.attrName, utf8),AttrValue};
convert_search_params(undefined) ->
    undefined.

%%--------------------------------------------------------------------
%% Function: convert_search_attributes/1
%% Description:
%%--------------------------------------------------------------------
convert_search_attributes(AttributeNames) ->
    convert_search_attributes(AttributeNames, []).

%%--------------------------------------------------------------------
convert_search_attributes([], Acc) ->
    lists:reverse(Acc);
convert_search_attributes([A |As], Acc) when is_atom(A) ->
    convert_search_attributes(As, [A |Acc]);
convert_search_attributes([A |As], Acc) when is_binary(A) ->
    convert_search_attributes(As, [binary_to_atom(A, utf8) |Acc]);
convert_search_attributes([A |As], Acc) when is_list(A) ->
    convert_search_attributes(As, [list_to_atom(A) |Acc]).

%%--------------------------------------------------------------------
%% Function: add_types_to_attributes/3
%% Description:
%%--------------------------------------------------------------------
add_types_to_attributes(Lang, extra_attributes, Attributes) ->
    case Lang of
	c ->
	    add_types_to_attributes_c(Attributes, extra_attributes, []);
	erlang ->
	    add_types_to_attributes_erl(Attributes, extra_attributes, [])
    end;
add_types_to_attributes(Lang, ClassName, Attributes) ->
    AttrTypeList = safs_imm_db:get_class_types(ClassName),
    case Lang of
	c ->
	    add_types_to_attributes_c(Attributes, AttrTypeList, []);
	erlang ->
	    add_types_to_attributes_erl(Attributes, AttrTypeList, [])
    end.

%%--------------------------------------------------------------------
add_types_to_attributes_c([], _, Acc) ->
    {ok, lists:reverse(Acc)};
add_types_to_attributes_c([{Name, Values} |Attributes], AttrTypeList, Acc)
  when Name =:= 'RcsImmAttrObjId' ->
    {ok, AttrValues} = safs_imm_lib:set_attr_values(sa_imm_attr_sauint32t, Values),
    add_types_to_attributes_c(Attributes, AttrTypeList,
			      [#safsImmAttrValues_2{attrName=safs_imm_lib:convert_attribute_name_to_binary(Name),
						    attrValueType=sa_imm_attr_sauint32t,
						    attrValuesNumber=length(AttrValues),
						    attrValues=AttrValues}
			       |Acc]);
add_types_to_attributes_c([{Name, Values} |Attributes], AttrTypeList, Acc)
  when Name =:= 'RcsImmAttrEcimDn' ->
    {ok, AttrValues} = safs_imm_lib:set_attr_values(sa_imm_attr_sastringt, Values),
    add_types_to_attributes_c(Attributes, AttrTypeList,
			      [#safsImmAttrValues_2{attrName=safs_imm_lib:convert_attribute_name_to_binary(Name),
						    attrValueType=sa_imm_attr_sastringt,
						    attrValuesNumber=length(AttrValues),
						    attrValues=AttrValues}
			       |Acc]);
add_types_to_attributes_c([{Name, Values} |Attributes], AttrTypeList, Acc)
  when Name =:= 'SaImmAttrClassName';
       Name =:= 'SaImmAttrAdminOwnerName';
       Name =:= 'SaImmAttrImplementerName' ->
    {ok, AttrValues} = safs_imm_lib:set_attr_values(sa_imm_attr_sastringt, Values),
    add_types_to_attributes_c(Attributes, AttrTypeList,
			      [#safsImmAttrValues_2{attrName=safs_imm_lib:convert_attribute_name_to_binary(Name),
						    attrValueType=sa_imm_attr_sastringt,
						    attrValuesNumber=length(AttrValues),
						    attrValues=AttrValues}
			       |Acc]);
add_types_to_attributes_c([{Name, Values} |Attributes], AttrTypeList, Acc) ->
    case lists:keyfind(Name, 1, AttrTypeList) of
	{Name, Type} ->
	    %Type2 = convert_dbtype_to_satype(Type),
	    {ok, AttrValues} = safs_imm_lib:set_attr_values(Type, Values),
	    add_types_to_attributes_c(Attributes, AttrTypeList,
				      [#safsImmAttrValues_2{attrName=safs_imm_lib:convert_attribute_name_to_binary(Name),
							    attrValueType=Type,
							    attrValuesNumber=length(AttrValues),
							    attrValues=AttrValues}
				       |Acc]);
	false ->
	    error_logger:error_msg("~p:add_types_to_attributes_c/3 attribute ~p not found in class definition\n"
				   "Attribute definitions: ~p\n", [?MODULE, Name, AttrTypeList]),
	    {error, sa_ais_err_library}
    end;
add_types_to_attributes_c([{Name, Type, Values} |Attributes], AttrTypeList, Acc) ->
    {ok, AttrValues} = safs_imm_lib:set_attr_values(Type, Values),
    add_types_to_attributes_c(Attributes, AttrTypeList,
			      [#safsImmAttrValues_2{attrName=safs_imm_lib:convert_attribute_name_to_binary(Name),
						    attrValueType=Type,
						    attrValuesNumber=length(AttrValues),
						    attrValues=AttrValues}
			       |Acc]);
add_types_to_attributes_c([#safsImmAttrValues_2{} = Attr |Attributes], AttrTypeList, Acc) ->
    add_types_to_attributes_c(Attributes, AttrTypeList, [Attr |Acc]);
add_types_to_attributes_c([Name |Attributes], AttrTypeList, Acc) ->
    %% No value
    case lists:keyfind(Name, 1, AttrTypeList) of
	{Name, Type} ->
	    %Type2 = convert_dbtype_to_satype(Type),
	    add_types_to_attributes_c(Attributes, AttrTypeList,
				      [#safsImmAttrValues_2{attrName=safs_imm_lib:convert_attribute_name_to_binary(Name),
							    attrValueType=Type,
							    attrValuesNumber=0,
							    attrValues=[]}
				       |Acc]);
	false ->
	    error_logger:error_msg("~p:add_types_to_attributes_c/3 attribute ~p not found in class definition\n"
				   "Attribute definitions: ~p\n", [?MODULE, Name, AttrTypeList]),
	    {error, sa_ais_err_library}
    end.

%%--------------------------------------------------------------------
add_types_to_attributes_erl([], _, Acc) ->
    {ok, lists:reverse(Acc)};
add_types_to_attributes_erl([{Name, Values} |Attributes], AttrTypeList, Acc)
  when Name =:= 'RcsImmAttrObjId' ->
    add_types_to_attributes_erl(Attributes, AttrTypeList,
			      [{safs_imm_lib:convert_attribute_name_to_binary(Name),
				sa_imm_attr_sauint32t,
				Values}
			       |Acc]);
add_types_to_attributes_erl([{Name, Values} |Attributes], AttrTypeList, Acc)
  when Name =:= 'RcsImmAttrEcimDn' ->
    add_types_to_attributes_erl(Attributes, AttrTypeList,
			      [{safs_imm_lib:convert_attribute_name_to_binary(Name),
				sa_imm_attr_sastringt,
				Values}
			       |Acc]);
add_types_to_attributes_erl([{Name, Values} |Attributes], AttrTypeList, Acc)
  when Name =:= 'SaImmAttrClassName';
       Name =:= 'SaImmAttrAdminOwnerName';
       Name =:= 'SaImmAttrImplementerName' ->
    add_types_to_attributes_erl(Attributes, AttrTypeList,
			      [{safs_imm_lib:convert_attribute_name_to_binary(Name),
				sa_imm_attr_sastringt,
				Values}
			       |Acc]);
add_types_to_attributes_erl([{Name, Values} |Attributes], AttrTypeList, Acc) ->
    case lists:keyfind(Name, 1, AttrTypeList) of
	{Name, Type} ->
	    %Type2 = convert_dbtype_to_satype(Type),
	    add_types_to_attributes_erl(Attributes, AttrTypeList,
					[{safs_imm_lib:convert_attribute_name_to_binary(Name),
					  Type,
					  Values}
					 |Acc]);
	false ->
	    error_logger:error_msg("~p:add_types_to_attributes_erl/3 attribute ~p not found in class definition\n"
				   "Attribute definitions: ~p\n", [?MODULE, Name, AttrTypeList]),
	    {error, sa_ais_err_library}
    end;
add_types_to_attributes_erl([{Name, Type, Values} |Attributes], AttrTypeList, Acc) ->
    add_types_to_attributes_erl(Attributes, AttrTypeList,
				[{safs_imm_lib:convert_attribute_name_to_binary(Name),
				  Type,
				  Values}
				 |Acc]);
add_types_to_attributes_erl([#safsImmAttrValues_2{attrName=Name,
						  attrValueType=Type,
						  attrValues=AttrValues} |Attributes],
			    AttrTypeList, Acc) ->
    {ok, Values} = safs_imm_lib:get_attr_values(Type, AttrValues),
    add_types_to_attributes_erl(Attributes, AttrTypeList,
				[{Name, Type, Values} |Acc]);
add_types_to_attributes_erl([Name |Attributes], AttrTypeList, Acc) ->
    %% No value
     case lists:keyfind(Name, 1, AttrTypeList) of
	{Name, Type} ->
	    %Type2 = convert_dbtype_to_satype(Type),
	    add_types_to_attributes_erl(Attributes, AttrTypeList,
					[{safs_imm_lib:convert_attribute_name_to_binary(Name),
					  Type,
					  []}
					 |Acc]);
	false ->
	    error_logger:error_msg("~p:add_types_to_attributes_erl/3 attribute ~p not found in class definition\n"
				   "Attribute definitions: ~p\n", [?MODULE, Name, AttrTypeList]),
	    {error, sa_ais_err_library}
    end.

%%--------------------------------------------------------------------
%% Function: convert_parameter_list/2
%% Description:
%%--------------------------------------------------------------------
convert_parameter_list(Params, undefined) ->
    Params;
convert_parameter_list(Params, c) ->
    convert_parameter_list_c(Params, []);
convert_parameter_list(Params, erlang) ->
    convert_parameter_list_erl(Params, []).

%%--------------------------------------------------------------------
convert_parameter_list_c([], Acc) ->
    lists:reverse(Acc);
convert_parameter_list_c([P |Params], Acc) when is_record(P, safsImmAdminOperationParams_2) ->
    convert_parameter_list_c(Params, [P |Acc]);
convert_parameter_list_c([{Name, Type, Value} |Params], Acc) ->
    convert_parameter_list_c(Params, [#safsImmAdminOperationParams_2{paramName=safs_imm_lib:convert_attribute_name_to_binary(Name),
								     paramType=Type,
								     paramBuffer=safs_imm_lib:set_attr_value(Type, Value)
								    }
				      |Acc]).

%%--------------------------------------------------------------------
convert_parameter_list_erl([], Acc) ->
    lists:reverse(Acc);
convert_parameter_list_erl([#safsImmAdminOperationParams_2{paramName=Name,
							   paramType=Type,
							   paramBuffer=Value} |Params], Acc) ->
    convert_parameter_list_erl(Params, [{Name, Type, safs_imm_lib:get_attr_value(Type, Value)} |Acc]);
convert_parameter_list_erl([{_Name, _Type, _Value} = P |Params], Acc) ->
    convert_parameter_list_erl(Params, [P |Acc]).


%%--------------------------------------------------------------------
%% @private
get_user_from_proxy(Proxy) ->
    MatchSpec1 = #safs_imm_om_user{proxy = Proxy, _ = '_'},
    case ets:match_object(safs_imm_om_user, MatchSpec1) of
	[] ->
	    MatchSpec2 = #safs_imm_om_user{cb_proxy = Proxy, _ = '_'},
	    ets:match_object(safs_imm_om_user, MatchSpec2);
	User ->
	    User
    end.

%%--------------------------------------------------------------------
%% @private
handle_user_down(User, Proxy, _State) ->
    if
	User#safs_imm_om_user.cb_proxy =:= undefined ->
	    ok;
	User#safs_imm_om_user.proxy =:= Proxy ->
	    safs_imm_om_com:close(User#safs_imm_om_user.cb_proxy);
	true ->
	    safs_imm_om_com:close(User#safs_imm_om_user.proxy)
    end,
    om_finalize_cleanup(User#safs_imm_om_user.handle),
    ok.

%%--------------------------------------------------------------------
%% @private
om_finalize_cleanup(Handle) ->
    %% Clean other dependent handles
    %% AO's and  Ccb's
    AOs = ets:match_object(safs_imm_om_adm_owner, {safs_imm_om_adm_owner, '_', Handle, '_', '_'}),
    [ ao_finalize_cleanup(AO)  || AO <- AOs],
    %% Accessor Handles
    ets:match_delete(safs_imm_om_accessor, {safs_imm_om_accessor, '_', Handle}),
    %% Search Handles
    SDs = ets:match_object(safs_imm_om_search_data, #safs_imm_om_search_data{imm_handle=Handle, _='_'}),
    [ sd_finalize_cleanup(SD)  || SD <- SDs],
    %% OM User
    delete_tbl(safs_imm_om_user, Handle),
    ok.

%%--------------------------------------------------------------------
%% @private
ao_finalize_cleanup(AO) ->
    OwnerHandle = AO#safs_imm_om_adm_owner.owner_handle,
    delete_tbl(safs_imm_om_adm_owner, OwnerHandle),
    %% Clean CCB's
    Ccbs = ets:match_object(safs_imm_om_ccb, {safs_imm_om_ccb,'_', OwnerHandle, '_','_'}),
    [ ccb_finalize_cleanup(Ccb) || Ccb <- Ccbs],
    %% Remove all ownership if ReleaseOnFinalize is true
    case AO#safs_imm_om_adm_owner.release_on_finalize of
	true ->
	    %% Remove all object ownership
	    case ets:match(safs_imm_om_administrated_objects,
			   {safs_imm_om_administrated_objects,
			    {AO#safs_imm_om_adm_owner.name,'$1'}}) of
		[] ->
		    %%io:format("No object ownerships to clear\n",[]),
		    ok;
		DnList ->
		    DNs = [ D || [D] <- DnList],
		    Fun = safs_imm_db:remove_all_object_ownership(AO#safs_imm_om_adm_owner.name,
								  DNs),
		    case apply_db([Fun])  of
			{aborted, _Reason} ->
			    ok;
			{atomic, ok} ->
			    case safs:get_env(imm_sync_cb, undefined) of
				{M, F} ->
				    M:F(),
				    ok;
				undefined ->
				    ok
			    end
		    end,
		    ets:match_delete(safs_imm_om_administrated_objects,
				     {safs_imm_om_administrated_objects,
				      {AO#safs_imm_om_adm_owner.name,'_'}}),
		    ok
	    end;
	false ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @private
sd_finalize_cleanup(SearchData) ->
    safs_imm_om_search:stop(SearchData#safs_imm_om_search_data.pid),
    delete_tbl(safs_imm_om_search_data, SearchData#safs_imm_om_search_data.search_handle),
    ok.

%%--------------------------------------------------------------------
%% @private
ccb_finalize_cleanup(Ccb) ->
    case safs_imm_om_ccb:finalize(Ccb#safs_imm_om_ccb.pid) of
	ok ->
	    delete_tbl(safs_imm_om_ccb, Ccb#safs_imm_om_ccb.handle),
	    ok;
	{error, sa_ais_err_bad_handle} ->
	    delete_tbl(safs_imm_om_ccb, Ccb#safs_imm_om_ccb.handle),
	    {error, sa_ais_err_bad_handle};	   %% ????
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
is_registered_cb(admin_operation_invoke_callback, Cb) when is_record(Cb, safsImmCallbacks) ->
    Cb#safsImmCallbacks.saImmOmAdminOperationInvokeCallback;
is_registered_cb(admin_operation_invoke_callback_o2, Cb) when is_record(Cb, safsImmCallbacks_o2) ->
    Cb#safsImmCallbacks_o2.saImmOmAdminOperationInvokeCallback;
is_registered_cb(_, Cb) when is_atom(Cb); is_function(Cb, 1) ->
    Cb.

%%--------------------------------------------------------------------
% tb(B) when is_binary(B) ->
%     B;
% tb(L) when is_list(L) ->
%     list_to_binary(L);
% tb(A) when is_atom(A) ->
%     erlang:atom_to_binary(A, utf8).

%%--------------------------------------------------------------------
%% @private
check_ccb_flags(0, Acc) -> %%LATH: Still allow 0 for compatability
    Acc;
check_ccb_flags([], Acc) ->
    Acc;
check_ccb_flags([ccb_registered_oi |CcbFlags], Acc) ->
    check_ccb_flags(CcbFlags, [ccb_registered_oi |Acc]);
check_ccb_flags([ccb_allow_null_oi |CcbFlags], Acc) ->
    check_ccb_flags(CcbFlags, [ccb_allow_null_oi |Acc]);
check_ccb_flags(_, _) ->
    throw({invalid_param, ccb_flags}).


%%--------------------------------------------------------------------
%% @private
class_category_sa_to_db(sa_imm_class_runtime) -> runtime;
class_category_sa_to_db(sa_imm_class_config) -> config.

%%--------------------------------------------------------------------
%% @private
class_category_db_to_sa(runtime) -> sa_imm_class_runtime;
class_category_db_to_sa(config) -> sa_imm_class_config.

%%--------------------------------------------------------------------
convert_from_attr_definitions(ClassCategory, AttrDefinitions) ->
    convert_from_attr_definitions(ClassCategory, AttrDefinitions, []).

%%--------------------------------------------------------------------
%% @private
convert_from_attr_definitions(_ClassCategory, [], Acc) ->
    lists:reverse(Acc);
convert_from_attr_definitions(ClassCategory,
			      [#safsImmAttrDefinition_2{attrName=Name,
							attrValueType=Type,
							attrFlags=Flags,
							attrDefaultValue=Default}
			       | AttrDefinitions],
			      Acc) ->
    FlagList = flags_to_atoms(Flags),
    {Category, FlagList2} = get_category(FlagList),
    check_flag_combination(ClassCategory, Category, FlagList2),
    NewDefault =
	case Default of
	    undefined ->
		undefined;
	    D0 when is_record(D0, safsImmAttrValue) ->
		safs_imm_lib:get_attr_value(Type, D0);
	    D1 ->
		D1
	end,
    convert_from_attr_definitions(ClassCategory, AttrDefinitions,
				  [{list_to_atom(binary_to_list(Name)),
				    %convert_satype_to_dbtype(Type),
				    Type,
				    Category,
				    FlagList2,
				    NewDefault}
				   |Acc]).

%%--------------------------------------------------------------------
%% @private
flags_to_atoms(Flags) when is_integer(Flags) ->
    flags_to_atoms(Flags, []);
flags_to_atoms(Flags) ->
    true =:= check_atom_flags(Flags) orelse throw({invalid_param, flags}),
    Flags.

%%--------------------------------------------------------------------
%% @private
flags_to_atoms(Flags, Acc) when Flags == 0 ->
    Acc;
flags_to_atoms(Flags, Acc) when Flags band ?SA_IMM_ATTR_MULTI_VALUE =:= ?SA_IMM_ATTR_MULTI_VALUE ->
    flags_to_atoms(Flags bxor ?SA_IMM_ATTR_MULTI_VALUE, [multi_value |Acc]);
flags_to_atoms(Flags, Acc)  when Flags band ?SA_IMM_ATTR_RDN  =:= ?SA_IMM_ATTR_RDN ->
        flags_to_atoms(Flags bxor ?SA_IMM_ATTR_RDN, [rdn |Acc]);
flags_to_atoms(Flags, Acc)  when Flags band ?SA_IMM_ATTR_CONFIG  =:= ?SA_IMM_ATTR_CONFIG ->
        flags_to_atoms(Flags bxor ?SA_IMM_ATTR_CONFIG, [config |Acc]);
flags_to_atoms(Flags, Acc)  when Flags band ?SA_IMM_ATTR_WRITABLE  =:= ?SA_IMM_ATTR_WRITABLE ->
        flags_to_atoms(Flags bxor ?SA_IMM_ATTR_WRITABLE, [writable |Acc]);
flags_to_atoms(Flags, Acc)  when Flags band ?SA_IMM_ATTR_INITIALIZED  =:= ?SA_IMM_ATTR_INITIALIZED ->
        flags_to_atoms(Flags bxor ?SA_IMM_ATTR_INITIALIZED, [initialized |Acc]);
flags_to_atoms(Flags, Acc)  when Flags band ?SA_IMM_ATTR_RUNTIME  =:= ?SA_IMM_ATTR_RUNTIME ->
        flags_to_atoms(Flags bxor ?SA_IMM_ATTR_RUNTIME, [runtime |Acc]);
flags_to_atoms(Flags, Acc)  when Flags band ?SA_IMM_ATTR_PERSISTENT  =:= ?SA_IMM_ATTR_PERSISTENT ->
        flags_to_atoms(Flags bxor ?SA_IMM_ATTR_PERSISTENT, [persistent |Acc]);
flags_to_atoms(Flags, Acc)  when Flags band ?SA_IMM_ATTR_CACHED  =:= ?SA_IMM_ATTR_CACHED ->
        flags_to_atoms(Flags bxor ?SA_IMM_ATTR_CACHED, [cached |Acc]);
flags_to_atoms(Flags, Acc)  when Flags band ?SA_IMM_ATTR_NO_DANGLING  =:= ?SA_IMM_ATTR_NO_DANGLING ->
        flags_to_atoms(Flags bxor ?SA_IMM_ATTR_NO_DANGLING, [no_dangling |Acc]);
flags_to_atoms(_Flags, _Acc) ->
    throw({invalid_param, flags}).

%%--------------------------------------------------------------------
%% @private
atoms_to_flags(Atoms) ->
    atoms_to_flags(Atoms, 0).

%%--------------------------------------------------------------------
%% @private
atoms_to_flags([], Flags) ->
    Flags;
atoms_to_flags([multi_value |Atoms], Flags) ->
    atoms_to_flags(Atoms, Flags bxor ?SA_IMM_ATTR_MULTI_VALUE);
atoms_to_flags([rdn |Atoms], Flags) ->
    atoms_to_flags(Atoms, Flags bxor ?SA_IMM_ATTR_RDN);
atoms_to_flags([config |Atoms], Flags) ->
    atoms_to_flags(Atoms, Flags bxor ?SA_IMM_ATTR_CONFIG);
atoms_to_flags([writable |Atoms], Flags) ->
    atoms_to_flags(Atoms, Flags bxor ?SA_IMM_ATTR_WRITABLE);
atoms_to_flags([initialized |Atoms], Flags) ->
    atoms_to_flags(Atoms, Flags bxor ?SA_IMM_ATTR_INITIALIZED);
atoms_to_flags([runtime |Atoms], Flags) ->
    atoms_to_flags(Atoms, Flags bxor ?SA_IMM_ATTR_RUNTIME);
atoms_to_flags([persistent |Atoms], Flags) ->
    atoms_to_flags(Atoms, Flags bxor ?SA_IMM_ATTR_PERSISTENT);
atoms_to_flags([cached |Atoms], Flags) ->
    atoms_to_flags(Atoms, Flags bxor ?SA_IMM_ATTR_CACHED);
atoms_to_flags([no_dangling |Atoms], Flags) ->
    atoms_to_flags(Atoms, Flags bxor ?SA_IMM_ATTR_NO_DANGLING);
atoms_to_flags([_ |Atoms], Flags) ->
    atoms_to_flags(Atoms, Flags).

%%--------------------------------------------------------------------
%% @private
check_atom_flags([]) ->
    true;
check_atom_flags([multi_value |Flags]) ->
    check_atom_flags(Flags);
check_atom_flags([rdn |Flags]) ->
    check_atom_flags(Flags);
check_atom_flags([config |Flags]) ->
    check_atom_flags(Flags);
check_atom_flags([writable |Flags]) ->
    check_atom_flags(Flags);
check_atom_flags([initialized |Flags]) ->
    check_atom_flags(Flags);
check_atom_flags([runtime |Flags]) ->
    check_atom_flags(Flags);
check_atom_flags([persistent |Flags]) ->
    check_atom_flags(Flags);
check_atom_flags([cached |Flags]) ->
    check_atom_flags(Flags);
check_atom_flags([no_dangling |Flags]) ->
    check_atom_flags(Flags);
check_atom_flags([notify |Flags]) ->
    check_atom_flags(Flags);
check_atom_flags([no_duplicates |Flags]) ->
    check_atom_flags(Flags);
check_atom_flags(_) ->
    false.

%%--------------------------------------------------------------------
%% @private
check_flag_combination(config, config, FlagList2) ->
    case lists:member(rdn, FlagList2) of
	true ->
	    false =:= lists:member(multi_value, FlagList2) orelse throw({invalid_param, flags});
	false ->
	    ok
    end,
    true =/= lists:member(persistent, FlagList2) orelse throw({invalid_param, flags}),
    true =/= lists:member(cached, FlagList2) orelse throw({invalid_param, flags}),
    ok;
check_flag_combination(config, runtime, FlagList2) ->
    case lists:member(rdn, FlagList2) of
	true ->
	    throw({invalid_param, flags});
	false ->
	    ok
    end,
    true =/= lists:member(writable, FlagList2) orelse throw({invalid_param, flags}),
    true =/= lists:member(initialized, FlagList2) orelse throw({invalid_param, flags}),
    ok;
check_flag_combination(runtime, config, _FlagList2) ->
    throw({invalid_param, flags});
check_flag_combination(runtime, runtime, FlagList2) ->
    case lists:member(rdn, FlagList2) of
	true ->
	    false =:= lists:member(multi_value, FlagList2) orelse throw({invalid_param, flags});
	false ->
	    ok
    end,
    true =/= lists:member(writable, FlagList2) orelse throw({invalid_param, flags}),
    true =/= lists:member(initialized, FlagList2) orelse throw({invalid_param, flags}),
    ok.

%%--------------------------------------------------------------------
%% @private
get_category(Flags) ->
    get_category(Flags, {undefined, []}).

%%--------------------------------------------------------------------
%% @private
get_category([], {C, F}) ->
    {C, lists:reverse(F)};
get_category([runtime |Flags], {C, F}) ->
    case C of
	config ->
	    throw({invalid_param, flags});
	_ ->
	    get_category(Flags, {runtime, F})
    end;
get_category([config |Flags], {C, F}) ->
    case C of
	runtime ->
	    throw({invalid_param, flags});
	_ ->
	    get_category(Flags, {config, F})
    end;
get_category([F1|Flags], {C, F2}) ->
    get_category(Flags, {C, [F1 |F2]}).

%%--------------------------------------------------------------------
%% @private
get_rdn(Attrs) ->
    get_rdn(Attrs, {undefined, []}).

%%--------------------------------------------------------------------
%% @private
get_rdn([], {undefined, _A}) ->
    throw({invalid_param, rdn});
get_rdn([], {R, A}) ->
    {R, lists:reverse(A)};
get_rdn([{Name, Type, Category, Flags, _Default} = Attr |Attrs], {R, A}) ->
     case lists:member(rdn, Flags) of
	 true ->
	     case R of
		 undefined ->
		     get_rdn(Attrs,
			     {{Name, Type, Category, lists:delete(rdn, Flags)},
			      A});
		 _ ->
		     throw({invalid_param, Name})
	     end;
	 false ->
	     get_rdn(Attrs, {R, [Attr |A]})
     end.

%%--------------------------------------------------------------------
%% @private
convert_to_attr_definitions(AttrList) ->
    convert_to_attr_definitions(AttrList, []).

%%--------------------------------------------------------------------
%% @private
convert_to_attr_definitions([], Acc) ->
    lists:reverse(Acc);
convert_to_attr_definitions([{Name, AttrType, AttrCategory, Flags, Default} |AttrList], Acc) ->
    %AttrType = convert_dbtype_to_satype(Type),
    NewDefault =
	case Default of
	    undefined ->
		undefined;
	    D ->
		safs_imm_lib:set_attr_value(AttrType, D)
	end,
    convert_to_attr_definitions(AttrList,
				[#safsImmAttrDefinition_2{attrName = list_to_binary(atom_to_list(Name)),
							  attrValueType = AttrType,
							  attrFlags = atoms_to_flags([AttrCategory |Flags]),
							  attrDefaultValue = NewDefault}
				 |Acc]).

%%--------------------------------------------------------------------
%% Fetch all instances of a class at a specific path
%%--------------------------------------------------------------------
get_class_instances(Parent, Class) ->
    Op = safs_imm_db:get_class_index(Parent, list_to_atom(binary_to_list(Class))),
    case mnesia:transaction(Op) of
	{aborted, Reason} ->
	    error_logger:warning_msg("search_class: Transaction failed: ~p", [{aborted, Reason}]),
	    {error, sa_ais_err_try_again};
	{atomic, ObjList} ->
	    {ok, get_class_convert_result(ObjList, [])}
    end.

get_class_convert_result([], Acc) ->
    lists:reverse(Acc);
get_class_convert_result([{_,_,DN} |Objs], Acc) ->
    get_class_convert_result(Objs, [safs_imm_db:key_to_bin(DN) |Acc]).

%%--------------------------------------------------------------------
