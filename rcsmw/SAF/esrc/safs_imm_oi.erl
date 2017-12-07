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
%% File: safs_imm_oi.erl
%%
%% Description:
%%    IMM OM gen_server.
%%
%%--------------------------------------------------------------------
-module(safs_imm_oi).
-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("oi.hrl").
-include("safs_imm_internal.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
% Start and stop
-export([
	 start_link/0,
	 start/0,
	 stop/0
        ]).

% OI API
-export([
	 initialize/1,
	 initialize/2,
	 initialize_2/1,
	 initialize_2/2,
	 callbacks_initialize/1,
	 finalize/1,
	 implementer_set/2,
	 implementer_clear/1,
	 class_implementer_set/2,
	 class_implementer_release/2,
	 object_implementer_set/3,
	 object_implementer_release/3,
	 rt_object_create_2/4,
	 rt_object_delete/2,
	 rt_object_update_2/3,
	 admin_operation_result/3,
	 admin_operation_result_o2/4,
	 augment_ccb_initialize/2,
	 ccb_set_error_string/3,
	 get_cb_timeout/1,
	 rt_object_update_delayed_nc_values/3
        ]).

% Callback API
-export([ccb_object_create_2/6,
	 ccb_object_delete/4,
	 ccb_object_modify_2/5,
	 ccb_completed/3,
	 ccb_apply/3,
	 ccb_abort/3,
	 rt_attr_update/3,
	 admin_operation_2/5,
	 is_oi_attached/1
	]).

%%--------------------------------------------------------------------
%% Test exports
%%--------------------------------------------------------------------
-export([
	 dump/0,
	 get_ccb_map/0
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
	 trace_points_list/0
        ]).

-export([
	 get_special_appliers_if_registered/0,
	 handle_callback/2,
	 cb_timeout_dump/0
	]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(SAFS_OI_DEBUG(F, A), safs_error(F,A)).

-define(USRNAME(User), if is_list(User) -> User;
			  true -> User#safs_imm_oi_user.implementer_name
		       end).
-define(USRMATCH, #safs_imm_oi_user{_ = '_'}).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {
	  imm_oi_users,
	  imm_oi_ids,
	  ccbs
	 }).

-record(safs_imm_oi_user, {
	  handle,
	  version,
	  callbacks,
	  implementer_name,
	  applier = false,
	  proxy,
	  cb_proxy,
	  reply_to,
	  cb_msg_number,
	  ccb_id,
	  ccb_pid,
	  cb_timer,
	  cb_queue
	 }).

-record(safs_imm_oi_update_callback, {
	  key,
	  object_name,
	  attr_names = [],
	  attr_vals = [],
	  got_reply = false
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
%% Function: start/0
%% Description: Starts the server
%%--------------------------------------------------------------------
stop() ->
    call(stop).

%%--------------------------------------------------------------------
%% Function: initialize_2/1/2
%% Description:
%%--------------------------------------------------------------------
initialize(Callbacks) ->
    initialize_2(Callbacks).
initialize(Callbacks, Version) ->
    initialize_2(Callbacks, Version).

%%--------------------------------------------------------------------
%% Function: initialize_2/1
%% Description:
%%--------------------------------------------------------------------
initialize_2(Callbacks) ->
    initialize_2(Callbacks, #safsVersion{releaseCode=?IMM_RELEASE_CODE,
					 majorVersion=?IMM_MAJOR_VERSION,
					 minorVersion=?IMM_MINOR_VERSION}).

%%--------------------------------------------------------------------
%% Function: initialize_2/2
%% Description:
%%--------------------------------------------------------------------
initialize_2(Callbacks,  #safsVersion{} = Version) when is_atom(Callbacks);
							is_function(Callbacks, 1);
							is_record(Callbacks, safsImmOiCallbacks) ->
    case safs_imm_lib:validate_version(Version) of
	{ok, Version1} ->
	    call({initialize, Callbacks, Version1, self()});
	{error, Error} ->
	    {error, Error}
    end;
initialize_2(_Callbacks, _Version) ->
    {error, sa_ais_err_invalid_param}.

%%--------------------------------------------------------------------
%% Function: callbacks_initialize/1
%% Description:
%%--------------------------------------------------------------------
callbacks_initialize(Handle) ->
    call({callbacks_initialize, Handle, self()}).

%%--------------------------------------------------------------------
%% Function: finalize/1
%% Description:
%%--------------------------------------------------------------------
finalize(Handle) ->
    call({finalize, Handle}).

%%--------------------------------------------------------------------
%% Function: implementer_set/2
%% Description:
%%--------------------------------------------------------------------
implementer_set(Handle, ImplementerName) when is_binary(ImplementerName) ->
    call({implementer_set, Handle, ImplementerName}).

%%--------------------------------------------------------------------
%% Function: implementer_clear/2
%% Description:
%%--------------------------------------------------------------------
implementer_clear(Handle) ->
    call({implementer_clear, Handle}).

%%--------------------------------------------------------------------
%% Function: class_implementer_set/2
%% Description:
%%--------------------------------------------------------------------
class_implementer_set(Handle, ClassName) ->
    try
	User = lookup_tbl(safs_imm_oi_user, Handle),
        ImplementerName = User#safs_imm_oi_user.implementer_name,
        ImplementerName =/= undefined orelse throw({error, sa_ais_err_bad_handle}),
        Applier = User#safs_imm_oi_user.applier,
        db_call(set_class_implementer, safs_imm_db:ci_set(safs_imm_lib:ta(ClassName), ImplementerName, Applier))
    catch
	throw:already_exists  ->
		{error, sa_ais_err_exist};
	throw:no_exists ->
		{error, sa_ais_err_not_exist};
	throw:bad_operation ->
		{error, sa_ais_err_bad_operation};
	throw:Error -> Error
    end.

%%--------------------------------------------------------------------
%% Function: class_implementer_release/2
%% Description:
%%--------------------------------------------------------------------
class_implementer_release(Handle, ClassName) ->
    try
	User = lookup_tbl(safs_imm_oi_user, Handle),
        ImplementerName = User#safs_imm_oi_user.implementer_name,
        ImplementerName =/= undefined orelse throw({error, sa_ais_err_bad_handle}),
        Applier = User#safs_imm_oi_user.applier,
        case safs_imm_om:affected_by_ccb(ClassName) of
	    false ->
		db_call(rel_class_implementer, safs_imm_db:ci_release(safs_imm_lib:ta(ClassName), ImplementerName, Applier));
	    _True ->
		{error, sa_ais_err_busy}
        end
    catch
	throw:already_exists  ->
		{error, sa_ais_err_exist};
	throw:no_exists ->
		{error, sa_ais_err_not_exist};
	throw:bad_operation ->
		{error, sa_ais_err_bad_operation};
	throw:Error -> Error
    end.

%%--------------------------------------------------------------------
%% Function: object_implementer_set/3
%% Description:
%%--------------------------------------------------------------------
object_implementer_set(Handle, ObjectName, Scope) ->
    try
	User = lookup_tbl(safs_imm_oi_user, Handle),
        ImplementerName = User#safs_imm_oi_user.implementer_name,
        ImplementerName =/= undefined orelse throw({error, sa_ais_err_bad_handle}),
        Applier = User#safs_imm_oi_user.applier,
        db_call(set_object_implementer, safs_imm_db:oi_set(ObjectName, ImplementerName,
							   db_scope(Scope), Applier))
    catch
	throw:already_exists  ->
		{error, sa_ais_err_exist};
	throw:no_exists ->
		{error, sa_ais_err_not_exist};
	throw:bad_operation ->
		{error, sa_ais_err_bad_operation};
	throw:Error -> Error
    end.

%%--------------------------------------------------------------------
%% Function: object_implementer_release/3
%% Description:
%%--------------------------------------------------------------------
object_implementer_release(Handle, ObjectName, Scope) ->
    try
	User = lookup_tbl(safs_imm_oi_user, Handle),
        ImplementerName = User#safs_imm_oi_user.implementer_name,
        ImplementerName =/= undefined orelse throw({error, sa_ais_err_bad_handle}),
        Applier = User#safs_imm_oi_user.applier,
        case safs_imm_om:affected_by_ccb(ObjectName, Scope) of
	    false ->
		db_call(rel_object_implementer, safs_imm_db:oi_release(ObjectName, ImplementerName,
								       db_scope(Scope), Applier));
	    _True ->
		{error, sa_ais_err_busy}
	end
    catch
	throw:already_exists  ->
		{error, sa_ais_err_exist};
	throw:no_exists ->
		{error, sa_ais_err_not_exist};
	throw:bad_operation ->
		{error, sa_ais_err_bad_operation};
	throw:Error -> Error
    end.

%%--------------------------------------------------------------------
%% Function: rt_object_create_2/4
%% Description:
%%--------------------------------------------------------------------
rt_object_create_2(Handle, ClassName, ParentName, AttrValues) ->
    try
        {ok, Attrs} = safs_imm_lib:convert_attr_list(AttrValues),
	User = lookup_tbl(safs_imm_oi_user, Handle),

        ParentName2 = safs_imm_lib:check_if_root(ParentName),
        ClassName2 = list_to_atom(binary_to_list(ClassName)),

        Fun = safs_imm_db:rt_add(User#safs_imm_oi_user.implementer_name,
				 ParentName2,
				 ClassName2,
				 Attrs),
        db_call(rt_object_create_2, Fun),
        %% Callbacks to Special Appliers
        SpecialAppliers = get_special_appliers_if_registered(),
        {_, AttrDefs} = safs_imm_db:get_class_def(ClassName2),
        NotifiableAttrs = safs_imm_lib:get_notifiable_attrs(AttrDefs),
        send_cb_to_special_appliers(NotifiableAttrs, SpecialAppliers, User#safs_imm_oi_user.implementer_name,
				    {create, 0, ClassName, ParentName, AttrValues}, []),
        ok
    catch
	throw:no_such_class ->
		{error, sa_ais_err_not_exist};
	throw:already_exists  ->
		{error, sa_ais_err_exist};
	throw:no_exists ->
		{error, sa_ais_err_not_exist};
	throw:bad_operation ->
		{error, sa_ais_err_bad_operation};
	throw:{invalid_param, {_,_,_,_} = Error} ->
	        error_logger:error_msg("rt_object_create_2 for class ~p failed:\n  ~p\n",
				     [ClassName, safs_imm_db:error_msg(Error)]),
		{error, sa_ais_err_invalid_param};
	throw:{invalid_param, _Error} ->
		{error, sa_ais_err_invalid_param};
	throw:not_owner ->
		{error, sa_ais_err_bad_operation};
	throw:Error -> Error
    end.


%%--------------------------------------------------------------------
%% Function: rt_object_delete/2
%% Description:
%%--------------------------------------------------------------------
rt_object_delete(Handle, ObjectName) ->
    try
	User = lookup_tbl(safs_imm_oi_user, Handle),

        {ok, ObjectClassName} = safs_imm_db:get_class(ObjectName),

        Fun = safs_imm_db:rt_delete(User#safs_imm_oi_user.implementer_name,
				    ObjectName),
        db_call(rt_object_delete, Fun),
        %% Callbacks to Special Appliers
        SpecialAppliers = get_special_appliers_if_registered(),
        {_, AttrDefs} = safs_imm_db:get_class_def(ObjectClassName),
        NotifiableAttrs = safs_imm_lib:get_notifiable_attrs(AttrDefs),
        send_cb_to_special_appliers(NotifiableAttrs, SpecialAppliers, User#safs_imm_oi_user.implementer_name,
				    {delete, 0, ObjectName}, []),
        ok
    catch
	throw:no_such_class ->
		{error, sa_ais_err_not_exist};
	throw:no_exists ->
		{error, sa_ais_err_not_exist};
	throw:bad_operation ->
		{error, sa_ais_err_bad_operation};
	throw:{invalid_param, {_,_,_,_} = Error} ->
	        error_logger:error_msg("rt_object_delete for object ~p failed:\n  ~p\n",
				       [ObjectName, safs_imm_db:error_msg(Error)]),
		{error, sa_ais_err_invalid_param};
	throw:{invalid_param, _Error} ->
		{error, sa_ais_err_invalid_param};
	throw:Error -> Error
    end.

%%--------------------------------------------------------------------
%% Function: rt_object_update_2/3
%% Description:
%%--------------------------------------------------------------------
rt_object_update_2(Handle, ObjectName, AttrMods) ->
    try
        {ok, Attrs} = safs_imm_lib:convert_attrmod_list(AttrMods),
	User = lookup_tbl(safs_imm_oi_user, Handle),

        save_no_cached_attrs(Handle, ObjectName, AttrMods),
        Fun = safs_imm_db:rt_modify(User#safs_imm_oi_user.implementer_name,
				    ObjectName,
				    Attrs),
        {ok, Changed} = db_call(rt_object_update_2, Fun),

	%% Callbacks to Special Appliers
        SpecialAppliers  = safs_imm_oi:get_special_appliers_if_registered(),
	{ok, ObjectClassName} = safs_imm_db:get_class(ObjectName),
        {_Category, AttrDefs} = safs_imm_db:get_class_def(ObjectClassName),
        NotifiableAttrs = safs_imm_lib:get_notifiable_attrs(AttrDefs),
        send_cb_to_special_appliers(NotifiableAttrs, SpecialAppliers,
				    {User#safs_imm_oi_user.implementer_name, safs_imm_lib:tb(ObjectClassName)},
				    {modify, 0, ObjectName, AttrMods}, Changed),
        ok
    catch
	throw:no_such_class ->
		{error, sa_ais_err_not_exist};
	throw:not_owner ->
		{error, sa_ais_err_bad_operation};
	throw:already_exists  ->
		{error, sa_ais_err_exist};
	throw:no_exists ->
		{error, sa_ais_err_not_exist};
	throw:bad_operation ->
		{error, sa_ais_err_bad_operation};
	throw:{invalid_param, {_,_,_,_} = Error} ->
	        error_logger:error_msg("safs_imm_oi: rt_object_update_2 for object ~p failed:\n  ~p\n",
				       [ObjectName, safs_imm_db:error_msg(Error)]),
		{error, sa_ais_err_invalid_param};
	throw:{invalid_param, _Error} ->
		{error, sa_ais_err_invalid_param};
        throw:included_in_ccb ->
                error_logger:info_msg("safs_imm_oi: Try again due to ~p included in a CCB\n",
				      [ObjectName]),
		{error, sa_ais_err_try_again};
	throw:Error -> Error
    end.

save_no_cached_attrs(Handle, ObjectName, AttrMods) ->
    call({save_no_cached_attrs, Handle, ObjectName, AttrMods}).

%%--------------------------------------------------------------------
%% Function: augment_ccb_initialize/2
%% Description:
%%--------------------------------------------------------------------
augment_ccb_initialize(OiHandle, CcbId) ->
    call({augment_ccb_initialize, OiHandle, CcbId, self()}).

%%--------------------------------------------------------------------
%% Function: admin_operation_result/3
%% Description:
%%--------------------------------------------------------------------
admin_operation_result(_Handle, Invocation, OpRetVal) ->
    %% LATH:  Check Handle
    safs_imm_om:admin_operation_result(Invocation, OpRetVal).

%%--------------------------------------------------------------------
%% Function: admin_operation_result_o2/4
%% Description:
%%--------------------------------------------------------------------
admin_operation_result_o2(_Handle, Invocation, OpRetVal, RetParams) ->
    %% LATH:  Check Handle
    safs_imm_om:admin_operation_result_o2(Invocation, OpRetVal, RetParams).

%%--------------------------------------------------------------------
%% Function: set_error_string/2
%% Description:
%%--------------------------------------------------------------------
ccb_set_error_string(_Handle, CcbId, String) ->
    %% LATH: Check Handle
    safs_imm_om:ccb_set_error_string(CcbId, String).

%%--------------------------------------------------------------------
%% Function: rt_object_update_delayed_values/3
%% Description:
%%--------------------------------------------------------------------
rt_object_update_delayed_nc_values(Handle, ObjectName, Attrs) ->
    call({rt_object_update_delayed_nc_values, Handle, ObjectName, Attrs}).

%%====================================================================
%% Callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: ccb_object_create_2/6
%% Description:
%%--------------------------------------------------------------------
ccb_object_create_2(ImplementerName, CcbPid, CcbId, ClassName, ParentName, Attr) ->
    call({callback, ImplementerName, CcbPid,
	  {ccb_object_create_2, {CcbId, ClassName, ParentName, Attr}}}).

%%--------------------------------------------------------------------
%% Function: ccb_object_delete/3
%% Description:
%%--------------------------------------------------------------------
ccb_object_delete(ImplementerName, CcbPid, CcbId, ObjectName) ->
    call({callback, ImplementerName, CcbPid,
	  {ccb_object_delete, {CcbId, ObjectName}}}).

%%--------------------------------------------------------------------
%% Function: ccb_object_modify_2/4
%% Description:
%%--------------------------------------------------------------------
ccb_object_modify_2(ImplementerName, CcbPid, CcbId, ObjectName, AttrMods) ->
    call({callback, ImplementerName, CcbPid,
	  {ccb_object_modify_2, {CcbId, ObjectName, AttrMods}}}).

%%--------------------------------------------------------------------
%% Function: ccb_completed/2
%% Description:
%%--------------------------------------------------------------------
ccb_completed(ImplementerName, CcbPid, CcbId) ->
    call({callback, ImplementerName, CcbPid, {ccb_completed, CcbId}}).

%%--------------------------------------------------------------------
%% Function: ccb_apply/2
%% Description:
%%--------------------------------------------------------------------
ccb_apply(ImplementerName, CcbPid, CcbId) ->
    call({callback, ImplementerName, CcbPid, {ccb_apply, CcbId}}).

%%--------------------------------------------------------------------
%% Function: ccb_abort/2
%% Description:
%%--------------------------------------------------------------------
ccb_abort(ImplementerName, CcbPid, CcbId) ->
    call({callback, ImplementerName, CcbPid, {ccb_abort, CcbId}}).

%%--------------------------------------------------------------------
%% Function: rt_attr_update/3
%% Description:
%%--------------------------------------------------------------------
rt_attr_update(ImplementerName, ObjectName, AttributeNames) ->
    call({callback, ImplementerName, undefined,
	  {rt_attr_update, {ObjectName, AttributeNames}}}).

%%--------------------------------------------------------------------
%% Function: admin_operation_2/5
%% Description:
%%--------------------------------------------------------------------
admin_operation_2(ImplementerName, Invocation, ObjectName, OperationId, Params) ->
    cast({callback, ImplementerName,
	  {admin_operation_2, {Invocation, ObjectName, OperationId, Params}}}).

%%--------------------------------------------------------------------
%% Function: is_oi_attached/1
%% Description:
%%--------------------------------------------------------------------
is_oi_attached(ImplementerName) ->
    call({is_oi_attached, ImplementerName}).


%%====================================================================
%% Tracing
%%====================================================================
trace_groups() -> [error, callbacks, life_cycle, object_impl, rt_objects,
		   adm_ops, augmentation].

trace_points_list() ->
    [
     {error,
      [{safs_error, 2}]},
     {callbacks,
      [{handle_callback, 2},
       {handle_callback, 4},
       {handle_erlang_callback, 3},
       {handle_reg_callback, 3},
       {ccb_object_create_2, 5},
       {ccb_object_delete, 3},
       {ccb_object_modify_2, 4},
       {ccb_completed, 2},
       {ccb_apply, 2},
       {ccb_abort, 2}
      ]},
     {life_cycle,
      [{initialize, 2},
       {finalize, 1}]},
     {object_impl,
      [{implementer_set, 2},
       {implementer_clear, 1},
       {class_implementer_set, 2},
       {class_implementer_release, 2},
       {object_implementer_set, 3},
       {object_implementer_release, 3}]},
     {rt_objects,
      [{rt_object_create_2, 4},
       {rt_object_delete, 2},
       {rt_object_update_2, 3},
       {rt_attr_update, 3},
       {rt_object_update_delayed_nc_values, 3}]},
     {adm_ops,
      [{admin_operation_2, 5},
       {admin_operation_result, 3},
       {admin_operation_result_o2, 4}]},
     {augmentation,
      [{augment_ccb_initialize, 2}]}
    ].

%%--------------------------------------------------------------------
%% Function: safs_error/2
%% Description:
%%--------------------------------------------------------------------
safs_error(_F, _A) ->  ok.

%%====================================================================
%% Test functions
%%====================================================================
dump() ->
    call(dump).

get_ccb_map() ->
    call(get_ccb_map).

get_special_appliers_if_registered() ->
    List = [],
    List1 = get_special_applier_if_registered(<<"@SafsImmReplicatorA">>, List),
    get_special_applier_if_registered(<<"@SafsImmReplicatorB">>, List1).

get_special_applier_if_registered(Impl, List) ->
    case get_user_from_name(Impl) of
	[] ->
	    List;
	_OiUser ->
	    [Impl |List]
    end.

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
    Users= ets:new(safs_imm_oi_user,[named_table,
				     {keypos, #safs_imm_oi_user.handle}]),
    ets:new(safs_imm_oi_update_callback,[named_table,
				     {keypos, #safs_imm_oi_update_callback.key}]),
    Ids = ets:new(safs_imm_oi_id, [named_table]),
    ets:insert(Ids, {userId, 17}),
    ets:insert(Ids, {cb_seq_number, 1}),
    process_flag(trap_exit, true),
    {ok, #state{imm_oi_users=Users, ccbs = maps:new(), imm_oi_ids=Ids}}.

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
    Handle = create_user_id(safs_imm_oi_id),
    User = #safs_imm_oi_user{handle = Handle,
			     version = Version,
			     callbacks = Callbacks,
			     proxy = Proxy,
			     cb_queue = queue:new()
			    },
    insert_tbl(safs_imm_oi_user, User),
    erlang:monitor(process, Proxy),
    Reply = {ok, Handle, Version},
    {reply, Reply, State};

handle_call({callbacks_initialize, Handle, CbProxy}, _From, State) ->
    try
	User = lookup_tbl(safs_imm_oi_user, Handle),
        erlang:monitor(process, CbProxy),
	insert_tbl(safs_imm_oi_user,
		   User#safs_imm_oi_user{cb_proxy = CbProxy}),
        {reply, ok, State}
    catch
	throw:Error -> {reply, Error, State}
    end;

handle_call({finalize, Handle}, _From, State) ->
    try
	User = lookup_tbl(safs_imm_oi_user, Handle),
        oi_finalize(User),
        {reply, ok, State}
    catch
	throw:Error -> {reply, Error, State}
    end;

handle_call({implementer_set, Handle, IName}, _From, State) ->
    try
	User = lookup_tbl(safs_imm_oi_user, Handle),
        Reply = set_implementer_name(User, IName),
        {reply, Reply, State}
    catch
	throw:Error -> {reply, Error, State}
    end;

handle_call({implementer_clear, Handle}, _From, State) ->
    try
	User = lookup_tbl(safs_imm_oi_user, Handle),
        clear_rt_data(User#safs_imm_oi_user.implementer_name),
        Reply = clear_implementer_name(User),
        {reply, Reply, State}
    catch
	throw:Error -> {reply, Error, State}
    end;

handle_call({augment_ccb_initialize, OiHandle, CcbId, CbPid}, _From, State) ->
    %% io:format("Received Augment Ccb Initialize.),
    try
	User = lookup_tbl(safs_imm_oi_user, OiHandle),

        case safs_imm_om:augment_ccb_initialize(CcbId,
						User#safs_imm_oi_user.implementer_name,
						CbPid) of
	    {ok, CcbHandle, AoHandle} ->
		{reply, {ok, CcbHandle, AoHandle}, State};
	    {error, Error0} ->
		{reply, {error, Error0}, State}
	end
    catch
	throw:Error ->
	   {reply, Error, State}
    end;

handle_call({is_oi_attached, ImplementerName}, _From, State) ->
    case get_user_from_name(safs_imm_lib:tb(ImplementerName)) of
	[_User] ->
	    {reply, true, State};
	_ ->
	    {reply, false, State}
    end;

handle_call({save_no_cached_attrs, Handle, ObjectName, AttrMods}, _From, State) ->
    try
	case lookup_tbl(safs_imm_oi_update_callback, Handle) of
	    #safs_imm_oi_update_callback{object_name=ObjectName, attr_names=AttrNames, attr_vals=AttrVals0,
					 got_reply=GotReply} = Update ->
		AttrVals = convert_attr_mods_to_attr_values(AttrMods, []),
		TotalAttrVals = lists:append(AttrVals, AttrVals0),
		case GotReply of
		    false ->
			insert_tbl(safs_imm_oi_update_callback,
				   Update#safs_imm_oi_update_callback{attr_vals=TotalAttrVals});
		    true ->
			case check_attr_vals(AttrVals, AttrNames) of
			    got_all ->
				case lookup_tbl(safs_imm_oi_user, Handle) of
				    #safs_imm_oi_user{reply_to=To} = User  when To =/= undefined ->
					{TRef, _} = User#safs_imm_oi_user.cb_timer,
					timer:cancel(TRef),
					delete_tbl(safs_imm_oi_update_callback, User#safs_imm_oi_user.handle),
					gen_server:reply(To, {ok, TotalAttrVals}),
					next_callback_from_queue(User#safs_imm_oi_user{ccb_id=undefined, ccb_pid=undefined}, State);
				    _Other ->
					ok
				end;
			    {attrs_left, AttrNames1} ->
				insert_tbl(safs_imm_oi_update_callback,
					   Update#safs_imm_oi_update_callback{attr_names=AttrNames1,
									      attr_vals=TotalAttrVals})
			end
		end,
		{reply, ok, State};
	    _ ->
		{reply, ok, State}
	end
    catch
	throw:_Error ->
	   {reply, ok, State}
    end;
handle_call({rt_object_update_delayed_nc_values, Handle, ObjectName, Attrs}, _From, State) ->
    try
	case lookup_tbl(safs_imm_oi_update_callback, Handle) of
	    #safs_imm_oi_update_callback{} = Update ->
		ok = validate_nc_attr_names(ObjectName, Attrs),
		insert_tbl(safs_imm_oi_update_callback,
			   Update#safs_imm_oi_update_callback{attr_names=Attrs}),
		{reply, ok, State};
	    _ ->
		{reply, ok, State}
	end
    catch
	throw:no_exists ->
	    error_logger:warning_msg("~p:rt_object_update_delayed_nc_values/3\n"
				     "Object: ~p doesn't exist\n",
				    [?MODULE, ObjectName]),
	   {reply, {error, sa_ais_err_not_exist}, State};
	throw:{error, sa_ais_err_bad_parameter, Attr} ->
	   error_logger:warning_msg("~p:rt_object_update_delayed_nc_values/3\n"
				     "Attr: ~p is not a member of object: ~p class definition\n",
				    [?MODULE, Attr, ObjectName]),
	   {reply, {error, sa_ais_err_bad_parameter}, State};
	throw:Error ->
	   {reply, Error, State}
    end;

handle_call({callback, IName, CcbPid, {rt_attr_update, _} = CBA}, From, State) ->
    case handle_callback(IName, CcbPid, CBA, From) of	
	ok ->
	    {noreply, State};
	user_not_exist ->
	    {reply, {ok, []}, State};
	callback_not_registered ->
	    {reply, {ok, []}, State};
	{error, _} = Error1 ->
	    {reply, Error1, State};
	Error2 ->
	    error_logger:info_msg("Unknown error for handle_callback:\n OI: ~p\n CB: ~p\nError: ~p\n", 
				  [IName, CBA, Error2]),
	    {reply, {error, sa_ais_err_failed_operation}, State}
    end;

handle_call({callback, IName, CcbPid, CBA}, From, State) ->
    case handle_callback(IName, CcbPid, CBA, From) of
	ok ->
	    {noreply, State};
	user_not_exist ->
	    {reply, ok, State};
	callback_not_registered ->
	    {reply, ok, State};
	{error, _} = Error1 ->
	    {reply, Error1, State};
	Error2 ->
	    error_logger:info_msg("Unknown error for handle_callback:\n OI: ~p\n CB: ~p\nError: ~p\n", 
				  [IName, CBA, Error2]),
	    {reply, {error, sa_ais_err_failed_operation}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(dump, _From, State) ->
    {reply, {ok, {State, ets:tab2list(safs_imm_oi_user)}}, State};

handle_call(get_ccb_map, _From, State) ->
    {reply, {ok, State#state.ccbs}, State};

handle_call(Msg, From, #state{} = State) ->
    error_logger:format("~p~p got unexpected call from ~p:\n\t~p\n",
                        [?MODULE, self(), From, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({callback, IName, CBA}, State) ->
    _ = handle_callback(IName, CBA), 
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
handle_info({callback_reply, rt_attr_update, Handle, CbMsgNumber, Reply}, State) ->
    %%io:format("Got rt_attr_update callback reply: ~p~n", [Reply]),
    try
	case lookup_tbl(safs_imm_oi_user, Handle) of
	    #safs_imm_oi_user{reply_to=To, cb_msg_number=CbMsgNumber} = User
	    when To =/= undefined ->
		send_attr_update_reply(User, To, Reply, State),
		{noreply, State};
	    _Other ->
		{noreply, State}
	end
    catch
	throw:_Error -> {noreply, State}
    end;
handle_info({callback_reply, Cb, Handle, CbMsgNumber, Reply}, #state{ccbs=Ccbs} = State) ->
    %io:format("Got ~p callback reply: ~p~n", [Cb, Reply]),
    try
	case lookup_tbl(safs_imm_oi_user, Handle) of
	    #safs_imm_oi_user{implementer_name=ImplName, reply_to=To, cb_msg_number=CbMsgNumber,
			      ccb_id = CbcId, ccb_pid = CcbPid} = User
	    when To =/= undefined ->
		{TRef, _} = User#safs_imm_oi_user.cb_timer,
		timer:cancel(TRef),
		Ccbs1 = check_return_value_and_link(ImplName, CcbPid, CbcId, Cb, Reply, Ccbs),
		gen_server:reply(To, Reply),
		State1 = State#state{ccbs=Ccbs1},
		next_callback_from_queue(User#safs_imm_oi_user{ccb_id=undefined, ccb_pid=undefined}, State1), 
		{noreply, State1};
	    _Other ->
		%%error_logger:info_msg("Message not expected:\n ~p\n", [Other]),
		{noreply, State}
	end
    catch
	throw:_Error -> {noreply, State}
    end;

handle_info({callback_timeout, Cb, _CbMsgNumber, Handle}, State) ->
    try
	case lookup_tbl(safs_imm_oi_user, Handle) of
	    #safs_imm_oi_user{reply_to=To, cb_timer={_, Proc}} = User ->
		case Proc of
		    proxy ->
			ok;
		    {erl, Pid} ->
			exit(Pid, normal)
		end,
		case Cb of
		    rt_attr_update ->
			case lookup_tbl(safs_imm_oi_update_callback, Handle) of
			    #safs_imm_oi_update_callback{got_reply=false} ->
				Dump = cb_timeout_dump(),
				error_logger:warning_msg("Callback timed out.\nObject implementer: ~p\n"
							 "Callback: ~p\nProcs with msg queue length > 0:\n~p\n",
							 [User#safs_imm_oi_user.implementer_name, Cb, Dump]),
				delete_tbl(safs_imm_oi_update_callback, Handle),
				gen_server:reply(To, {error, sa_ais_err_timeout});
			    #safs_imm_oi_update_callback{attr_names=AttrNames, attr_vals=AttrVals,
							 got_reply=true} ->
				Dump = cb_timeout_dump(),
				error_logger:warning_msg("Callback timed out.\nObject implementer: ~p\n"
							 "Callback: ~p\nWaiting for Attrs: ~p\n"
							 "Procs with msg queue length > 0:\n~p\n",
							 [User#safs_imm_oi_user.implementer_name,
							  Cb, AttrNames, Dump]),
				delete_tbl(safs_imm_oi_update_callback, Handle),
				gen_server:reply(To, {ok, AttrVals})
			end;
		    _ ->
			Dump = cb_timeout_dump(),
			error_logger:warning_msg("Callback timed out.\nObject implementer: ~p\n"
						 "Callback: ~p\nProcs with msg queue length > 0:\n~p\n",
						 [User#safs_imm_oi_user.implementer_name, Cb, Dump]),
			gen_server:reply(To, {error, sa_ais_err_timeout})
		end,
		next_callback_from_queue(User#safs_imm_oi_user{ccb_id=undefined, ccb_pid=undefined}, State);
	    _Other ->
		ok
	end,
        {noreply, State}
    catch
	throw:_Error -> {noreply, State} %% Handle not found
    end;

handle_info({'DOWN', _Ref, _Type, Pid, _Reason}, State) ->
    case get_user_from_proxy(Pid) of
	[] ->
	    safs_error(handle_info, {"got 'DOWN' message", Pid, no_users}),
	    ok;
	Users ->
	    safs_error(handle_info, {"got 'DOWN' message", Pid, {users, Users}}),
	    [handle_user_down(User, Pid, State) || User <- Users]
    end,
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, #state{ccbs=Ccbs} = State) when Reason =/= normal ->
    % error_logger:warning_msg("~p(~p) got exit from: ~p with reason: ~p\n",
    %  			     [?MODULE, self(), Pid, Reason]),
    Ccbs1 =
	case maps:find(Pid, Ccbs) of
	    {ok, CcbId} ->
		{ok, {_, OiList}} = maps:find(CcbId, Ccbs),
		_ = [handle_callback(Oi, special_abort, {ccb_abort, CcbId}, undefined) ||
			Oi <- sets:to_list(OiList)],
		clear_ccb_entry_on_exit(Pid, Ccbs);
	    _ ->
		Ccbs
	end,
    {noreply, State#state{ccbs=Ccbs1}};

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
%% Object Implementer functions
%%----------------------------------------------------------------------
set_implementer_name(User, IName)
  when User#safs_imm_oi_user.implementer_name =:= undefined ->
    case get_user_from_name(IName) of
	[] ->
	    case IName of
		<<"@"/utf8, _/binary>> ->
		    insert_tbl(safs_imm_oi_user,
			       User#safs_imm_oi_user{implementer_name = IName,
						     applier=true});
		_ ->
		    insert_tbl(safs_imm_oi_user,
			       User#safs_imm_oi_user{implementer_name = IName})
	    end,
	    ok;
	OiUser ->
	    safs_error(set_implementer_name, ["Implementer: ", User,
					      "Other Implementer: ", OiUser,
					      "already set for: ", IName]),
	    {error, sa_ais_err_exist}
    end;
set_implementer_name(#safs_imm_oi_user{implementer_name = IName}, IName) ->
    ok;
set_implementer_name(User, IName) ->
    %% The IMM 2.01 spec doesn't mention this case but 3.01 does.
    safs_error(set_implementer_name,["Failed with User: ", User,
				     "Implementer: ", IName]),
    {error, sa_ais_err_exist}.

clear_implementer_name(User) when User#safs_imm_oi_user.implementer_name =/= undefined ->
    insert_tbl(safs_imm_oi_user,
	       User#safs_imm_oi_user{implementer_name = undefined}),
    ok;
clear_implementer_name(_User) ->
    {error, sa_ais_err_bad_handle}.

clear_rt_data(undefined) ->
    ok;
clear_rt_data(ImplementerName) ->
    case {safs:get_env(imm_clean_rt_on_oi_finalize, false),
	  safs:get_env(rcs_warm_restart, false)} of
	{true, false} ->
	    %% Remove all RT data for this OI with Notification sent
	    safs_imm_db:clean_rt_data_for_oi_cold_restart(ImplementerName),
	    ok;
	{true, true} ->
	    %% Remove all RT data for this OI without Notification sent
	    safs_imm_db:clean_rt_data_for_oi_warm_restart(ImplementerName),
	    ok;
	{false, _} ->
	    ok
    end.

%%----------------------------------------------------------------------
%% Callbacks
%%----------------------------------------------------------------------
handle_callback(IName, CBA) ->
    handle_callback(IName, undefined, CBA, undefined).

handle_callback(IName, _, {rt_attr_update, {ObjectName, _RtAttrs}} = CBA, From) ->
    case get_user_from_name(safs_imm_lib:tb(IName)) of
	[User] when User#safs_imm_oi_user.reply_to =/= undefined,
		    From =/= undefined ->
	    insert_tbl(safs_imm_oi_user,
		       User#safs_imm_oi_user{cb_queue = queue:in({IName, undefined, CBA, From},
								 User#safs_imm_oi_user.cb_queue)}),
	    safs_error(handle_callback, {"Waiting for other cb to finish",
					 {IName, CBA, From}}),
	    ok;
	[User] when User#safs_imm_oi_user.cb_proxy =/= undefined ->
	    insert_tbl(safs_imm_oi_update_callback,
		       #safs_imm_oi_update_callback{key=User#safs_imm_oi_user.handle,
						    object_name = ObjectName}),
	    handle_reg_callback(User, undefined, CBA, From);
	[User] ->
	    insert_tbl(safs_imm_oi_update_callback,
		       #safs_imm_oi_update_callback{key=User#safs_imm_oi_user.handle,
						    object_name = ObjectName}),
	    send_erlang_callback(User, undefined, CBA, From);
	_ ->
	    user_not_exist %% User doesn't exist, a queued cb can not reach this case clause
    end;
handle_callback(IName, CcbPid, {_Cb, _} = CBA, From) ->
    case get_user_from_name(safs_imm_lib:tb(IName)) of
	[User] when User#safs_imm_oi_user.reply_to =/= undefined,
		    From =/= undefined ->
	    insert_tbl(safs_imm_oi_user,
		       User#safs_imm_oi_user{cb_queue = queue:in({IName, CcbPid, CBA, From},
								 User#safs_imm_oi_user.cb_queue)}),
	    safs_error(handle_callback, {"Waiting for other cb to finish",
					 {IName, CcbPid, CBA, From}}),
	    ok;
	[User] when User#safs_imm_oi_user.cb_proxy =/= undefined ->
	    handle_reg_callback(User, CcbPid, CBA, From);
	[User] ->
	    send_erlang_callback(User, CcbPid, CBA, From);
	_ ->
	   user_not_exist %% User doesn't exist, a queued cb can not reach this case clause
    end.

%%----------------------------------------------------------------------
%% Handle Erlang Callbacks
send_erlang_callback(User, _, CBA, undefined) -> %% Cast, is never queued
    spawn_link(fun() ->
		       handle_erlang_callback(User, CBA, undefined, undefined),
		       exit(normal)
	       end),
    ok;
send_erlang_callback(User, CcbPid, {Cb, _Args} = CBA, From) -> %% Call
    Self = self(),
    Handle = User#safs_imm_oi_user.handle,
    CbMsgNumber = create_cb_seq_number(safs_imm_oi_id),
    TimeOut = safs_imm_oi:get_cb_timeout(Cb),
    {ok, TRef} =
	case TimeOut of
	    infinity ->
		{ok, undefined};
	    _ ->
		timer:send_after(TimeOut, {callback_timeout, Cb, CbMsgNumber, Handle})
	end,
    CbPid = spawn_link(fun() ->
			       handle_erlang_callback(User, CBA, CbMsgNumber, Self),
			       exit(normal)
		       end),
    NewUser = User#safs_imm_oi_user{reply_to=From, cb_msg_number=CbMsgNumber,
				    ccb_id=get_ccb_id(CBA), ccb_pid=CcbPid,
				    cb_timer={TRef, {erl, CbPid}}},
    insert_tbl(safs_imm_oi_user, NewUser),
    ok.

handle_erlang_callback(User, {Cb, Args} = CBA, undefined, undefined) ->
    ModOrFun = is_registered_cb(Cb, User#safs_imm_oi_user.callbacks),
    Handle = User#safs_imm_oi_user.handle,
    %LATH: Ta hand om att CB inte är registrerad??
    case catch execute_cb(ModOrFun, Cb, [Handle |args_to_list(Cb, Args)]) of
	ok ->
	    ok;
	{error, Error} when is_atom(Error) ->
	    ok;
	{error, Code} when is_integer(Code), Code >= 0 ->
	    ok;
	Error ->
	    safs_error(handle_erlang_callback,
		       ["Callback: ", CBA, "failed: ",Error]),
	    ok
    end;
handle_erlang_callback(User, {Cb, Args} = CBA, CbMsgNumber, From) ->
    ModOrFun = is_registered_cb(Cb, User#safs_imm_oi_user.callbacks),
    Handle = User#safs_imm_oi_user.handle,
    %LATH: Ta hand om att CB inte är registrerad??
    Reply =
	case catch execute_cb(ModOrFun, Cb, [Handle |args_to_list(Cb, Args)]) of
	    ok ->
		ok;
	    {error, Error} when is_atom(Error) ->
		{error, Error};
	    {error, Code} when is_integer(Code), Code >= 0 ->
		{error, Code};
	    Error ->
		safs_error(handle_erlang_callback,
			   ["Callback: ", CBA, "failed: ",Error]),
		{error, sa_ais_err_failed_operation}
	end,
    From ! {callback_reply, Cb, Handle, CbMsgNumber, Reply}.

args_to_list(Cb, Arg) when Cb =:= ccb_apply;
			   Cb =:= ccb_completed;
			   Cb =:= ccb_abort ->
    [Arg];
args_to_list(_, Args) ->
    tuple_to_list(Args).

execute_cb(undefined, _F, _Args)  ->
    ok;
execute_cb(false, _F, _Args)  ->
    ok;
execute_cb(ModOrFun, F, Args) when is_function(ModOrFun, 1) ->
    ModOrFun(list_to_tuple([F |Args]));
execute_cb(ModOrFun, F, Args) when is_atom(ModOrFun) ->
    apply(ModOrFun, F, Args).

get_cb_timeout(Cb) when Cb == ccb_completed ->
    safs:get_env(imm_ccb_completed_cb_timeout, ?IMM_CB_DEFAULT_TIMEOUT);
get_cb_timeout(Cb) when Cb == ccb_apply ->
    safs:get_env(imm_ccb_apply_cb_timeout, ?IMM_CB_DEFAULT_TIMEOUT);
get_cb_timeout(Cb) when Cb == ccb_abort ->
     safs:get_env(imm_ccb_abort_cb_timeout, ?IMM_CB_DEFAULT_TIMEOUT);
get_cb_timeout(_Cb) ->
    safs:get_env(imm_cb_timeout, ?IMM_CB_DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
%% Handle C callbacks
handle_reg_callback(User, CcbPid, {Cb, _} = CBA , From) ->
    case is_registered_cb(Cb, User#safs_imm_oi_user.callbacks) of
	true ->
	    Handle = User#safs_imm_oi_user.handle,
	    Pid = User#safs_imm_oi_user.cb_proxy,
	    CbMsgNumber = create_cb_seq_number(safs_imm_oi_id),
	    send_callback(From, Pid, CcbPid, CbMsgNumber, CBA, Handle, User);
	_False ->
	    callback_not_registered
    end.

send_callback(undefined, Pid, _, CbMsgNumber, {Cb, Arg}, Handle, _User) -> %% Cast, is never queued
    safs_imm_oi_com:callback_message_async(Pid, CbMsgNumber, Cb, Handle, Arg),
    ok;

send_callback(From, Pid, CcbPid, CbMsgNumber, {Cb, Arg} = CBA, Handle, User) -> %% Call
    case safs_imm_oi_com:callback_message_sync(Pid, CbMsgNumber, Cb, Handle, Arg) of
	{ok, TRef} ->
	    NewUser = User#safs_imm_oi_user{reply_to=From, cb_msg_number=CbMsgNumber,
					    ccb_id=get_ccb_id(CBA), ccb_pid=CcbPid,
					    cb_timer={TRef, proxy}},
	    insert_tbl(safs_imm_oi_user, NewUser),
	    ok;
	Error ->
	    Error
    end.

send_attr_update_reply(User, To, ok, State) ->
    try
	case lookup_tbl(safs_imm_oi_update_callback, User#safs_imm_oi_user.handle) of
	    #safs_imm_oi_update_callback{attr_names=AttrNames, attr_vals=AttrVals} = UpdateCb ->
		case check_attr_vals(AttrVals, AttrNames) of
		    got_all ->
			{TRef, _} = User#safs_imm_oi_user.cb_timer,
			timer:cancel(TRef),
			delete_tbl(safs_imm_oi_update_callback, User#safs_imm_oi_user.handle),
			gen_server:reply(To, {ok, AttrVals}),
			next_callback_from_queue(User#safs_imm_oi_user{ccb_id=undefined, ccb_pid=undefined}, 
						 State);
		    {attrs_left, AttrNames1} ->
			insert_tbl(safs_imm_oi_update_callback,
				   UpdateCb#safs_imm_oi_update_callback{attr_names=AttrNames1,
									got_reply=true}),
			ok
		end;
	    _ ->
		gen_server:reply(To, {ok, []}),
		ok
	end
    catch
	throw:_Term ->
	    gen_server:reply(To, {ok, []}),
	    ok
    end;
send_attr_update_reply(User, To, Reply, State) ->
    {TRef, _} = User#safs_imm_oi_user.cb_timer,
    timer:cancel(TRef),
    delete_tbl(safs_imm_oi_update_callback, User#safs_imm_oi_user.handle),
    gen_server:reply(To, Reply),
    next_callback_from_queue(User#safs_imm_oi_user{ccb_id=undefined, ccb_pid=undefined}, State).

check_attr_vals(_, []) ->
    got_all;
check_attr_vals([], AttrNames) ->
    {attrs_left, AttrNames};
check_attr_vals([#safsImmAttrValues_2{attrName=AttrName} | AttrVals], AttrNames) ->
    check_attr_vals(AttrVals, lists:delete(AttrName, AttrNames)).


validate_nc_attr_names(ObjectName, Attrs) ->
    {_, AttrDefs} = safs_imm_db:get_class_def(ObjectName),
    validate_nc_attr_names_1(Attrs, AttrDefs).

validate_nc_attr_names_1([], _AttrDefs) ->
    ok;
validate_nc_attr_names_1([Attr |Attrs], AttrDefs) ->
    case lists:keymember(safs_imm_lib:ta(Attr), 1, AttrDefs) of
	true ->
	    validate_nc_attr_names_1(Attrs, AttrDefs);
	false ->
	    throw({error, sa_ais_err_bad_parameter, Attr})
    end.

convert_attr_mods_to_attr_values([], Acc) ->
    lists:reverse(Acc);
convert_attr_mods_to_attr_values([#safsImmAttrModification_2{modAttr = Val} |AttrMods],
				 Acc) ->
    convert_attr_mods_to_attr_values(AttrMods, [Val| Acc]).

handle_user_down(User, Proxy, _State) ->
    if
	User#safs_imm_oi_user.cb_proxy =:= undefined ->
	    ok;
	User#safs_imm_oi_user.proxy =:= Proxy ->
	    safs_imm_oi_com:close(User#safs_imm_oi_user.cb_proxy);
	true ->
	    safs_imm_oi_com:close(User#safs_imm_oi_user.proxy)
    end,
    oi_finalize(User).


oi_finalize(User) ->
    %% Handle pending callback
    case User#safs_imm_oi_user.cb_timer of
	undefined ->
	    ok;
	{_TRef, proxy} ->
	    safs_error(oi_finalize, {"pending callback when connection lost",
				     User#safs_imm_oi_user.implementer_name}),
	    gen_server:reply(User#safs_imm_oi_user.reply_to, {error, sa_ais_err_failed_operation}),
	    send_reply_to_queued_callbacks(User#safs_imm_oi_user.cb_queue);
	{_TRef, {erl, CbPid}} ->
	    safs_error(oi_finalize, {"pending callback when connection lost",
				     User#safs_imm_oi_user.implementer_name}),
	    exit(CbPid, normal),
	    gen_server:reply(User#safs_imm_oi_user.reply_to, {error, sa_ais_err_failed_operation})
    end,
    clear_rt_data(User#safs_imm_oi_user.implementer_name),
    delete_tbl(safs_imm_oi_user, User#safs_imm_oi_user.handle),
    ok.


send_reply_to_queued_callbacks(Queue) ->
    case queue:out(Queue) of
	{empty, _Queue} ->
	    ok;
	 {{value, {_IName, _CcbPid, _CBA, From}}, Queue1} ->
	    gen_server:reply(From, {error, sa_ais_err_failed_operation}),
	    send_reply_to_queued_callbacks(Queue1)
    end.

%%----------------------------------------------------------------------
next_callback_from_queue(User, State) ->
    case queue:out(User#safs_imm_oi_user.cb_queue) of
	{empty, _CbQueue} ->
	    safs_error(next_callback_from_queue, "queue empty"),
	    insert_tbl(safs_imm_oi_user,
		       User#safs_imm_oi_user{reply_to=undefined, 
					     cb_msg_number=undefined,
					     cb_timer=undefined}),
	    ok;
	{{value, {IName, CcbPid, {Cb, _} = CBA, From}}, CbQueue1} ->
	    safs_error(next_callback_from_queue,
		       {"queue next cb", {value, {IName, CcbPid, CBA, From}}}),
	    User1 = User#safs_imm_oi_user{reply_to=undefined, cb_msg_number=undefined,
					  cb_timer=undefined, cb_queue=CbQueue1},
	    insert_tbl(safs_imm_oi_user, User1),
	    case handle_callback(IName, CcbPid, CBA, From) of	
		ok ->
		    ok;
		user_not_exist ->
		    if
			Cb =:= rt_attr_update ->
			    gen_server:reply(From, {ok, []});
			true ->
			    gen_server:reply(From, ok)
		    end,
		    next_callback_from_queue(User1#safs_imm_oi_user{ccb_id=undefined, ccb_pid=undefined}, State);
		callback_not_registered ->
		    if
			Cb =:= rt_attr_update ->
			    gen_server:reply(From, {ok, []});
			true ->
			    gen_server:reply(From, ok)
		    end,
		    next_callback_from_queue(User1#safs_imm_oi_user{ccb_id=undefined, ccb_pid=undefined}, State);		    
		{error, _} = Error1 ->
		    gen_server:reply(From, Error1),
		    next_callback_from_queue(User1#safs_imm_oi_user{ccb_id=undefined, ccb_pid=undefined}, State);
		Error2 ->
		    error_logger:info_msg("Unknown error for handle_callback:\n OI: ~p\n CB: ~p\nError: ~p\n", 
					  [IName, CBA, Error2]),
		    gen_server:reply(From, {error, sa_ais_err_failed_operation}),
		    next_callback_from_queue(User1#safs_imm_oi_user{ccb_id=undefined, ccb_pid=undefined}, State)
	    end
    end.
    
%%----------------------------------------------------------------------
is_registered_cb(_, Cb) when is_atom(Cb); is_function(Cb,1) ->
    Cb;
is_registered_cb(ccb_object_create_2, Cb) ->
    Cb#safsImmOiCallbacks.saImmOiCcbObjectCreateCallback_2;
is_registered_cb(ccb_object_delete, Cb) ->
    Cb#safsImmOiCallbacks.saImmOiCcbObjectDeleteCallback;
is_registered_cb(ccb_object_modify_2, Cb) ->
    Cb#safsImmOiCallbacks.saImmOiCcbObjectModifyCallback_2;
is_registered_cb(ccb_completed, Cb) ->
    Cb#safsImmOiCallbacks.saImmOiCcbCompletedCallback;
is_registered_cb(ccb_apply, Cb) ->
    Cb#safsImmOiCallbacks.saImmOiCcbApplyCallback;
is_registered_cb(ccb_abort, Cb) ->
    Cb#safsImmOiCallbacks.saImmOiCcbAbortCallback;
is_registered_cb(admin_operation_2, Cb) ->
    Cb#safsImmOiCallbacks.saImmOiAdminOperationCallback_2;
is_registered_cb(rt_attr_update, Cb) ->
    Cb#safsImmOiCallbacks.saImmOiRtAttrUpdateCallback.

%%----------------------------------------------------------------------
%% Commonly used functions
%%----------------------------------------------------------------------
get_user_from_name(IName) ->
    MatchSpec = ?USRMATCH#safs_imm_oi_user{implementer_name = IName},
    ets:match_object(safs_imm_oi_user, MatchSpec).

get_user_from_proxy(Proxy) ->
    MatchSpec1 = ?USRMATCH#safs_imm_oi_user{proxy = Proxy},
    case ets:match_object(safs_imm_oi_user, MatchSpec1) of
	[] ->
	    MatchSpec2 = ?USRMATCH#safs_imm_oi_user{cb_proxy = Proxy},
	    ets:match_object(safs_imm_oi_user, MatchSpec2);
	User ->
	    User
    end.

%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
create_user_id(Table) ->
    ets:update_counter(Table, userId, 1).

create_cb_seq_number(Table) ->
    ets:update_counter(Table, cb_seq_number, 1).

%%--------------------------------------------------------------------
insert_tbl(Table, Obj) ->
    ets:insert(Table, Obj).

lookup_tbl(Table, Handle) ->
    case ets:lookup(Table, Handle) of
	[] ->
	    throw({error, sa_ais_err_bad_handle});
	[Object] ->
	    Object
    end.

delete_tbl(Table, Handle) ->
    ets:delete(Table, Handle),
    ok.

%%--------------------------------------------------------------------
db_call(From, Fun) ->
    case mnesia:transaction(Fun) of
	{atomic, ok} ->
	    case safs:get_env(imm_sync_cb, undefined) of
		{M, F} ->
		    M:F(),
		    ok;
		undefined ->
		    ok
	    end;
	{atomic, {ok, Changed}} ->
	    case safs:get_env(imm_sync_cb, undefined) of
		{M, F} ->
		    M:F(),
		    {ok, Changed};
		undefined ->
		    {ok, Changed}
	    end;
	{aborted, {throw, E}} ->
	    throw(E);
	{aborted, Reason} ->
	    error_logger:info_msg("safs_imm_oi:~p: Try again due to aborted transaction with reason: ~p\n",
				  [From, Reason]),
	    throw({error, sa_ais_err_try_again})
    end.
%%--------------------------------------------------------------------

db_scope(sa_imm_one) ->
    one;
db_scope(sa_imm_sublevel) ->
    sublevel;
db_scope(sa_imm_subtree) ->
    subtree;
db_scope(one) ->
    one;
db_scope(sublevel) ->
    sublevel;
db_scope(subtree) ->
    subtree.

%%--------------------------------------------------------------------
% check_return_value(_, _, ok) ->
%     ok;
% check_return_value(_, _, sa_ais_ok) ->
%     ok;
% check_return_value(ImplName, Cb, {error, ErrNo}) ->
%     case check_if_error_code_allowed(ErrNo) of
%  	ok ->
% 	    ok;
%  	bad_error_code ->
%  	    error_logger:warning_msg("Implementer did not return an allowed error code.\n"
%  				     "Oi: ~p\nCallback: ~p\nError Code: ~p\n",
% 				     [ImplName, Cb, ErrNo]),
% 	    ok
%     end.

%%--------------------------------------------------------------------
check_return_value_and_link(ImplName, CcbPid, CcbId, Cb, Ret, Ccbs) when Ret =:= ok; Ret =:= sa_ais_ok ->
    case change_ccb_entry(Cb) of
	add ->
	    add_ccb_entry(ImplName, CcbPid, CcbId, Ccbs);
	clear ->
	    clear_ccb_entry(ImplName, CcbId, Ccbs);
	_ ->
	    Ccbs
    end;
check_return_value_and_link(ImplName, _CcbPid, CcbId, Cb, {error, ErrNo}, Ccbs) ->
    Ccbs1 =
	case change_ccb_entry(Cb) of
	    clear ->
		clear_ccb_entry(ImplName, CcbId, Ccbs);
	    _ ->
		Ccbs
	end,
    case check_if_error_code_allowed(ErrNo) of
	ok ->
	    Ccbs1;
	bad_error_code ->
	    error_logger:warning_msg("safs_imm_oi: Implementer did not return an allowed error code.\n"
				     "Oi: ~p\nCallback: ~p\nError Code: ~p\n",
				     [ImplName, Cb, ErrNo]),
	    Ccbs1
    end.

check_if_error_code_allowed(sa_ais_err_no_memory) ->
    ok;
check_if_error_code_allowed(sa_ais_err_no_resources) ->
    ok;
check_if_error_code_allowed(sa_ais_err_bad_operation) ->
    ok;
check_if_error_code_allowed(_) ->
    bad_error_code.

%%--------------------------------------------------------------------
add_ccb_entry(_ImplName, _CcbPid, 0, Ccbs) ->
    Ccbs;
add_ccb_entry(ImplName, CcbPid, CcbId, Ccbs) ->
    case maps:find(CcbId, Ccbs) of
	{ok, {CcbPid2, OIs}} ->
	    maps:update(CcbId, {CcbPid2, sets:add_element(ImplName, OIs)}, Ccbs);
	error ->
	    Ccbs2 = maps:put(CcbId, {CcbPid, sets:from_list([ImplName])}, Ccbs),
	    Ccbs3 = maps:put(CcbPid, CcbId, Ccbs2),
	    %% link CCB worker
	    link(CcbPid),
	    Ccbs3
    end.

clear_ccb_entry(_ImplName, 0, Ccbs) ->
    Ccbs;
clear_ccb_entry(ImplName, CcbId, Ccbs) ->
    case maps:find(CcbId, Ccbs) of
	 {ok, {CcbPid, OIs}} ->
		OIs2 = sets:del_element(ImplName, OIs),
		case sets:size(OIs2) of
		    0 ->
			%% unlink CCB Worker
			unlink(CcbPid),
			Ccbs2 = maps:remove(CcbPid, Ccbs),
			maps:remove(CcbId, Ccbs2);
		    _ ->
			maps:update(CcbId, {CcbPid, OIs2}, Ccbs)
		end;
	 error ->
		Ccbs
	end.

clear_ccb_entry_on_exit(CcbPid, Ccbs) ->
    case maps:find(CcbPid, Ccbs) of
	{ok, CcbId} ->
	    Ccbs2 = maps:remove(CcbPid, Ccbs),
	    maps:remove(CcbId, Ccbs2);
	error ->
	    Ccbs
    end.

change_ccb_entry(ccb_object_create_2) ->
    add;
change_ccb_entry(ccb_object_delete) ->
    add;
change_ccb_entry(ccb_object_modify_2) ->
    add;
change_ccb_entry(ccb_abort) ->
    clear;
change_ccb_entry(ccb_apply) ->
    clear;
change_ccb_entry(_) ->
    no.

get_ccb_id({ccb_object_create_2, {CcbId, _ClassName, _ParentName, _Attr}}) ->
    CcbId;
get_ccb_id({ccb_object_delete, {CcbId, _ObjectName}}) ->
    CcbId;
get_ccb_id({ccb_object_modify_2, {CcbId, _ObjectName, _AttrMods}}) ->
    CcbId;
get_ccb_id({ccb_completed, CcbId}) ->
    CcbId;
get_ccb_id({ccb_apply, CcbId}) ->
    CcbId;
get_ccb_id({ccb_abort, CcbId}) ->
    CcbId;
get_ccb_id({rt_attr_update, _}) ->
    undefined.

%%--------------------------------------------------------------------
send_cb_to_special_appliers([], _, _, _, _) ->
    ok;
send_cb_to_special_appliers(_, [], _, _, _) ->
    ok;
send_cb_to_special_appliers(NotifiableAttrs, SpecialAppliers, OI,
			      {create, CcbId, ClassName, ParentName, AttrValues}, _) ->
    % io:format("CreateAttrs: ~p\n", [AttrValues]),
    % io:format("NotifAttrs: ~p\n", [NotifiableAttrs]),
    NewAttrValues = lists:filtermap(fun(#safsImmAttrValues_2{attrName=AttrName} = Attr) ->
					    case lists:keymember(safs_imm_lib:ta(AttrName), 1, NotifiableAttrs) of
						true ->
						    {true, Attr};
						false -> false
					    end
				    end,
				    AttrValues),
    {ok, ClassNameValues} = safs_imm_lib:set_attr_values(sa_imm_attr_sastringt, [safs_imm_lib:tb(ClassName)]),
    {ok, OIValues} = safs_imm_lib:set_attr_values(sa_imm_attr_sastringt, [safs_imm_lib:tb(OI)]),
    [safs_imm_oi:ccb_object_create_2(Applier, undefined, CcbId, ClassName, ParentName,
				     [#safsImmAttrValues_2{attrName = <<"saImmAttrImplementerName">>,
						           attrValueType=sa_imm_attr_sastringt,
						           attrValuesNumber=length(OIValues),
						           attrValues=OIValues},
				      #safsImmAttrValues_2{attrName = <<"saImmAttrClassName">>,
				                           attrValueType=sa_imm_attr_sastringt,
				                           attrValuesNumber=length(ClassNameValues),
						           attrValues=ClassNameValues}
                                      | NewAttrValues])
     || Applier <- SpecialAppliers];
send_cb_to_special_appliers(_NotifiableAttrs, SpecialAppliers, _OI, {delete, CcbId, ObjectName}, _) ->
    % io:format("NotifAttrs: ~p\n", [_NotifiableAttrs]),
    F =
	fun(Applier) -> safs_imm_oi:ccb_object_delete(Applier, undefined, CcbId, ObjectName) end,
    [F(Applier) || Applier <- SpecialAppliers];
send_cb_to_special_appliers(NotifiableAttrs, SpecialAppliers, {OI, ClassName},
			    {modify, CcbId, ObjectName, AttrMods}, Changed) ->
    % io:format("NotifAttrs: ~p\n", [NotifiableAttrs]),
    NewAttrMods = lists:filtermap(fun(#safsImmAttrModification_2{modAttr=Attr} = AttrMod) ->
					  Name = safs_imm_lib:ta(Attr#safsImmAttrValues_2.attrName),
					  case {lists:keymember(Name, 1, NotifiableAttrs),
						lists:member(Name, Changed)} of
					      {true, true} ->
						  {true, AttrMod};
					      _ -> false
					  end
				    end,
				    AttrMods),
    case NewAttrMods of
	[] ->
	    ok;
	NewAttrMods1 ->
	    {ok, ClassNameValues} = safs_imm_lib:set_attr_values(sa_imm_attr_sastringt, [safs_imm_lib:tb(ClassName)]),
	    {ok, OIValues} = safs_imm_lib:set_attr_values(sa_imm_attr_sastringt, [safs_imm_lib:tb(OI)]),
	    [safs_imm_oi:ccb_object_modify_2(Applier, undefined, CcbId, ObjectName,
		[#safsImmAttrModification_2{modType=sa_imm_attr_values_replace,
					    modAttr=#safsImmAttrValues_2{attrName = <<"saImmAttrImplementerName">>,
									 attrValueType=sa_imm_attr_sastringt,
									 attrValuesNumber=length(OIValues),
									 attrValues=OIValues}},
		 #safsImmAttrModification_2{modType=sa_imm_attr_values_replace,
					    modAttr=#safsImmAttrValues_2{attrName = <<"saImmAttrClassName">>,
									 attrValueType=sa_imm_attr_sastringt,
									 attrValuesNumber=length(ClassNameValues),
									 attrValues=ClassNameValues}}
		 | NewAttrMods1])
	     || Applier <- SpecialAppliers]
    end.

%%--------------------------------------------------------------------
%% Function: cb_timeout_dump/0
%% Description: This function is used to get dump info about all
%%              processes with a message queue larger than zero
%%              when got a callback timeout
%%--------------------------------------------------------------------
cb_timeout_dump() ->
    Processes = processes(),
    lists:filtermap(
      fun(P) ->
	      case process_info(P, [message_queue_len]) of
		  [{message_queue_len,0}] -> false;
		  undefined -> false;
		  I ->
		      M = case process_info(P, [registered_name,
						current_function,
						status]) of
			      undefined -> [];
			      More -> More
			  end,
		      R = [{pid, P}] ++ I ++ M,
		      {true, R}
	      end
      end, Processes).
