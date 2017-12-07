%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
%%
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%%
%% %CopyrightEnd%
%%
%%-----------------------------------------------------------------
%% File: safs_imm_om_ccb.erl
%%
%% Description:
%%    This file contains the worker process for CCB's
%%
%%-----------------------------------------------------------------
-module(safs_imm_om_ccb).
-behaviour(gen_server).

%%-----------------------------------------------------------------
%% Suppress dialyzer warning for "no local return" in fun used
%% during CCB validation to abort the transaction.
%%-----------------------------------------------------------------
-dialyzer({[no_return], [ccb_apply_1/2, ccb_validate_1/2]}).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("om.hrl").
-include("safs_internal.hrl").
-include("safs_imm_internal.hrl").

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 initialize/1,
	 finalize/1,
	 object_create_2/4,
	 object_delete/2,
	 object_modify_2/3,
	 object_read/4,
	 ccb_validate/1,
	 ccb_apply/1,
	 ccb_abort/1,
	 get_fun_list/1,
	 send_completed/1,
	 send_apply/1,
	 insert_fun/2,
	 affected_by_ccb/2,
	 affected_by_ccb/3,
	 augment_ccb_initialize/4,
	 get_parent_state/1,
	 update_parent_state/6,
	 object_create_s2/5,
	 object_modify_extra_attrs_s2/3,
	 object_read_extra_attrs_s2/3,
	 trace_groups/0,
	 trace_points_list/0
	]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2,
	 apply_to_implementers_parallel/4
	]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {
 	  id,
 	  owner_handle,
 	  ccb_flags,
 	  context,
 	  operations = [],
	  references = [],
 	  objects = [],
 	  implementers = [],
 	  %% Additions for 2.11
 	  augments,                     % Used in Child Ccb
% 	  augmented_by = [],            % Used in Parent Ccb
 	  implementer_name,             % Used in Child Ccb
 	  marked_for_abortion = false,
 	  owner_pid,                    % Used in Child Ccb})
	  validated = no,               % no | ok | failed
	  ops_allowed = all             % all | read
 	 }).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: initialize/1
%% Description:
%%--------------------------------------------------------------------
initialize(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%%--------------------------------------------------------------------
%% Function: finalize/1
%% Description:
%%--------------------------------------------------------------------
finalize(Pid) ->
    call(Pid, finalize).

%%--------------------------------------------------------------------
%% Function: object_create_2/1
%% Description:
%%--------------------------------------------------------------------
object_create_2(Pid, ClassName, ParentName, AttrValues) ->
    call(Pid, {object_create_2, ClassName, ParentName, AttrValues}).

%%--------------------------------------------------------------------
%% Function: object_delete/2
%% Description:
%%--------------------------------------------------------------------
object_delete(Pid, ObjectName) ->
    call(Pid, {object_delete, ObjectName}).

%%--------------------------------------------------------------------
%% Function: object_modify_2/3
%% Description:
%%--------------------------------------------------------------------
object_modify_2(Pid, ObjectName, AttrMods) ->
    call(Pid, {object_modify_2, ObjectName, AttrMods}).

%%--------------------------------------------------------------------
%% Function: object_read/3
%% Description:
%%--------------------------------------------------------------------
object_read(Pid, ObjectName, AttributeNames, Lang) ->
    call(Pid, {object_read, ObjectName, AttributeNames, Lang}).

%%--------------------------------------------------------------------
%% Function: ccb_validate/1
%% Description:
%%--------------------------------------------------------------------
ccb_validate(Pid) ->
    call(Pid, ccb_validate).

%%--------------------------------------------------------------------
%% Function: ccb_apply/1
%% Description: Using ccb_apply and not apply as functiopn name
%%              so there is no confusion.
%%--------------------------------------------------------------------
ccb_apply(Pid) ->
    call(Pid, ccb_apply).

%%--------------------------------------------------------------------
%% Function: ccb_abort/1
%% Description:
%%--------------------------------------------------------------------
ccb_abort(Pid) ->
    call(Pid, ccb_abort).

%%--------------------------------------------------------------------
%% Function: get_fun_list/1
%% Description:
%%--------------------------------------------------------------------
get_fun_list(Pid) ->
    call(Pid, get_fun_list).

%%--------------------------------------------------------------------
%% Function: send_completed/1
%% Description:
%%--------------------------------------------------------------------
send_completed(Pid) ->
    call(Pid, send_completed).

%%--------------------------------------------------------------------
%% Function: send_apply/1
%% Description:
%%--------------------------------------------------------------------
send_apply(Pid) ->
    call(Pid, send_apply).

%%--------------------------------------------------------------------
%% Function: insert_fun/2
%% Description:
%%--------------------------------------------------------------------
insert_fun(Pid, Fun) ->
    call(Pid, {insert_fun, Fun}).

%%--------------------------------------------------------------------
%% Function: affected_by_ccb/2
%% Description:
%%--------------------------------------------------------------------
affected_by_ccb(Pid, ClassName) when not is_atom(ClassName) ->
    ClassName2 = list_to_atom(unicode:characters_to_list(ClassName, utf8)),
    affected_by_ccb(Pid, ClassName2);
affected_by_ccb(Pid, ClassName) ->
    try
	TimeOut = safs:get_env(imm_cb_timeout, ?IMM_CB_DEFAULT_TIMEOUT),
	call(Pid, {affected_by_ccb, ClassName}, TimeOut)
    catch
	exit:{noproc, _} ->
	  false;
	exit:{normal, _} ->
	  false;
	exit:{timeout, _} ->
	  error_logger:info_msg("safs_imm_om_ccb: affected_by_ccb timeout pid=~p class=~p\n~p\n",
				 [Pid, ClassName,
				   {process_info, erlang:process_info(Pid)}]),
	  true  %% Couldn't get an answer
    end.
%%--------------------------------------------------------------------
%% Function: affected_by_ccb/3
%% Description:
%%--------------------------------------------------------------------
affected_by_ccb(Pid, DN, Scope) when is_binary(DN) ->
    affected_by_ccb(Pid, safs_imm_lib:mk_dnlist(DN), Scope);
affected_by_ccb(Pid, DN, Scope) when is_list(DN) ->
    try
	TimeOut = safs:get_env(imm_cb_timeout, ?IMM_CB_DEFAULT_TIMEOUT),
        call(Pid, {affected_by_ccb, DN, Scope}, TimeOut)
    catch
	exit:{noproc, _} ->
	  false;
	exit:{normal, _} ->
	  false;
	exit:{timeout, _} ->
	  error_logger:info_msg("safs_imm_om_ccb: affected_by_ccb timeout pid=~p object=~p\n~p\n",
				 [Pid, {DN, Scope},
				   {process_info, erlang:process_info(Pid)}]),
	  true  %% Couldn't get an answer
    end.

%%--------------------------------------------------------------------
%% Function: augment_ccb_initialize/5
%% Description:
%%--------------------------------------------------------------------
augment_ccb_initialize(Pid, ParentPid, OiName, OwnerPid) ->
    call(Pid, {augment_ccb_initialize, ParentPid, OiName, OwnerPid}).

%%--------------------------------------------------------------------
%% Function: get_parent_state/1
%% Description:
%%--------------------------------------------------------------------
get_parent_state(Pid) ->
    call(Pid, get_parent_state).

%%--------------------------------------------------------------------
%% Function: update_parent_state/6
%% Description:
%%--------------------------------------------------------------------
update_parent_state(Pid, Ops, Refs, Objs, Impls, Context) ->
    call(Pid, {update_parent_state, Ops, Refs, Objs, Impls, Context}).

%%--------------------------------------------------------------------
%% Function: mark_parent_for_abortion/1
%% Description:
%%--------------------------------------------------------------------
mark_parent_for_abortion(Pid) ->
        call(Pid, mark_parent_for_abortion).

%%--------------------------------------------------------------------
%% Function: object_create_s2/5
%% Description:
%%--------------------------------------------------------------------
object_create_s2(Pid, ObjectName, ParentName, AttrValues, ExtraAttrValues) ->
     call(Pid, {object_create_s2, ObjectName, ParentName, AttrValues, ExtraAttrValues}).

%%--------------------------------------------------------------------
%% Function: object_modify_extra_attrs_s2/3
%% Description:
%%--------------------------------------------------------------------
object_modify_extra_attrs_s2(Pid, ObjectName, ExtraAttrMods) ->
    call(Pid, {object_modify_extra_attrs_s2, ObjectName, ExtraAttrMods}).

%%--------------------------------------------------------------------
%% Function: object_read_extra_attrs_s2/3
%% Description:
%%--------------------------------------------------------------------
object_read_extra_attrs_s2(Pid, ObjectName, Lang) ->
     call(Pid, {object_read_extra_attrs_s2, ObjectName, Lang}).

%%====================================================================
%% Tracing
%%====================================================================
trace_groups() -> [error].

trace_points_list() ->
    [
     {error,
      [{safs_error, 2}]}
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
init({initialize, CcbId, OwnerHandle, CcbFlags}) -> %% Normal CCB
    %process_flag(trap_exit, true),
    {ok, #state{id = CcbId,
		owner_handle = OwnerHandle,
		ccb_flags = CcbFlags,
		context = safs_imm_db:create_context()
		}};
init({initialize, CcbId}) -> %% Augmented CCB
    %process_flag(trap_exit, true),
    {ok, #state{id = CcbId}}.

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
handle_call(finalize, From, State) ->
    case State#state.augments of
	undefined ->
	    ccb_finalize_1(From, State);
	_ ->
	    ccb_finalize_augmented(From, State)
    end;

handle_call({object_create_2, _ClassName,  _ParentName, _AttrValues},
	    _From, #state{validated=Validated} = State) when Validated =/= no ->
    {reply, {error, sa_ais_err_failed_operation}, State};
handle_call({object_create_2, ClassName, ParentName, AttrValues}, From, State) ->
    case State#state.ops_allowed of
	all ->
	    handle_object_create_s2(ClassName, ParentName, AttrValues, [], From, State);
	read ->
	    {reply, {error, sa_ais_err_bad_operation}, State}
    end;

handle_call({object_delete, _ObjectName}, _From, #state{validated=Validated} = State) when Validated =/= no ->
    {reply, {error, sa_ais_err_failed_operation}, State};
handle_call({object_delete, ObjectName}, From, State) ->
    #state{id=CcbId, owner_handle = OwnerHandle, context = Context, ops_allowed=Allowed} = State,
     try
        Allowed =:= all orelse throw(bad_operation),
     	safs_imm_om:ccb_delete_error_strings(CcbId),
        AO = safs_imm_om:adm_owner(OwnerHandle),

        {Fun, RefFun, Objs, NewContext} = safs_imm_db:ccb_delete(AO#safs_imm_om_adm_owner.name,
								 ObjectName,
								 Context),
        delete({wait_for_callback, object_delete, From, undefined, {AO, Fun, RefFun, Objs},
		State, State#state{context=NewContext}})
     catch
     	throw:no_exists ->
     		{reply, {error, sa_ais_err_not_exist}, State};
	throw:bad_operation ->
		safs_error(handle_call, {object_delete, bad_operation, State}),
		{reply, {error, sa_ais_err_bad_operation}, State};
	throw:ErrMsg -> {reply, ErrMsg, State}
     end;

handle_call({object_modify_2, _ObjectName, _AttrMods}, _From,
	    #state{validated=Validated} = State) when Validated =/= no ->
    {reply, {error, sa_ais_err_failed_operation}, State};
handle_call({object_modify_2, ObjectName, AttrMods}, From,
	    #state{id=CcbId, owner_handle = OwnerHandle, ccb_flags = CcbFlags, operations = Ops,
		   references = Refs, objects = Objects, implementers = Impls, augments = Augments,
		   context = Context, ops_allowed=Allowed} = State) ->
      try
        Allowed =:= all orelse throw(bad_operation),
      	safs_imm_om:ccb_delete_error_strings(CcbId),
        AO =  safs_imm_om:adm_owner(OwnerHandle),

        {OiFun, Oi, Appliers} = safs_imm_db:oi_get(ObjectName, Context),
        SpecialAppliers  = safs_imm_oi:get_special_appliers_if_registered(),

        check_oi_reg_flags(ObjectName, ccb_object_modify_2, CcbFlags, Oi),

      	{ok, ConvAttrMods} = safs_imm_lib:convert_attrmod_list(AttrMods),
      	{ok, ObjectClassName} = safs_imm_db:get_class(ObjectName, Context),
      	{Fun, RefFun, NewContext, Changed} = safs_imm_db:ccb_modify(AO#safs_imm_om_adm_owner.name,
							   ObjectName,
							   ConvAttrMods,
							   Context),
        {_Category, AttrDefs} = safs_imm_db:get_class_def(ObjectClassName),
        NotifiableAttrs = safs_imm_lib:get_notifiable_attrs(AttrDefs),
        Appliers1 =
      	  case NotifiableAttrs of
      	      [] -> Appliers;
      	      _ -> SpecialAppliers ++ Appliers
      	  end,
          %% LATH: Is the context correct when read is implemented in augmented transactions
        if
      	    Oi == undefined orelse (Augments =/= undefined andalso State#state.implementer_name == Oi) ->

		%%io:format("A Obj: ~p\n", [ObjectName]),
      		%% Callbacks to Special Appliers
      		send_cb_to_special_appliers(NotifiableAttrs, SpecialAppliers,
      					    {AO#safs_imm_om_adm_owner.name, tb(ObjectClassName)},
      					    {modify, CcbId, ObjectName, AttrMods}, Changed),
      		%% Callbacks to Appliers
     		[safs_imm_oi:ccb_object_modify_2(Applier, self(), CcbId, ObjectName, AttrMods)
      		 || Applier <- Appliers],

      	        NewState = State#state{operations = [Fun, OiFun |Ops],
				       references = [RefFun |Refs],
				       objects = [{safs_imm_lib:mk_dnlist(ObjectName), ObjectClassName}
						  |Objects],
				       implementers = add_unique([{applier, Appliers1}], Impls),
				       context=NewContext} ,

      		{reply, ok, NewState};
      	    true ->
		ReplyFun =
		    fun() ->
			    %% Callbacks to Appliers
			    [safs_imm_oi:ccb_object_modify_2(Applier, self(), CcbId, ObjectName, AttrMods)
			     || Applier <- Appliers],
			    ok
		    end,
      	        NewState = State#state{operations = [Fun, OiFun |Ops],
				       references = [RefFun |Refs],
				       objects = [{safs_imm_lib:mk_dnlist(ObjectName), ObjectClassName}
						  |Objects],
				       implementers = add_unique([{applier, Appliers1}, Oi], Impls),
				       context=NewContext} ,


		%%io:format("B Obj: ~p\n", [ObjectName]),
		%% Callbacks to Special Appliers
		send_cb_to_special_appliers(NotifiableAttrs, SpecialAppliers,
					    {AO#safs_imm_om_adm_owner.name, tb(ObjectClassName)},
					    {modify, CcbId, ObjectName, AttrMods}, Changed),
		%% Callback to Implementer
		async_ccb_object_modify_2(Oi, CcbId, ObjectName, AttrMods),
		{noreply, {wait_for_callback, object_modify_2, From, ReplyFun, undefined,
			   State#state{implementers = add_unique(Oi, Impls)}, NewState}}
      	end
      catch
      	throw:no_exists ->
	      {reply, {error, sa_ais_err_not_exist}, State};
	throw:{invalid_param, {_,_,_,_} = Error} ->
	      error_logger:error_msg("object_modify_2 for object ~p failed:\n  ~p\n",
				     [ObjectName, safs_imm_db:error_msg(Error)]),
	      {reply, {error, sa_ais_err_invalid_param}, State};
	throw:{invalid_param, _Error} ->
	      {reply, {error, sa_ais_err_invalid_param}, State};
      	throw:{bad_operation, What} ->
	      case What of
		  {not_ao, AdmOwner} ->
		      error_logger:info_msg("Admin owner: ~p not owner of object: ~p\n",
					    [AdmOwner, ObjectName]);
		  not_config ->
		      error_logger:info_msg("Not a configuration object: ~p\n", [ObjectName])
	      end,
	      {reply, {error, sa_ais_err_bad_operation}, State};
      	throw:bad_operation ->
	      {reply, {error, sa_ais_err_bad_operation}, State};
      	 throw:already_exists ->
	      {reply, {error, sa_ais_err_invalid_param}, State};
      	throw:not_owner ->
	      {reply, {error, sa_ais_err_bad_operation}, State};
      	throw:Error2 -> {reply, Error2, State}
      end;

handle_call({object_read, ObjectName, AttributeNames, Lang}, _From,
	    #state{id=CcbId, context = Context} = State) ->
     try
        safs_imm_om:ccb_delete_error_strings(CcbId),

        {_Fun, ClassName, Attributes} =
     	   case safs_imm_om:convert_search_attributes(AttributeNames) of
     	       [] ->
     		   safs_imm_db:ccb_read(ObjectName,
     					all,
     					Context);
     	       GetAttrs ->
     		   safs_imm_db:ccb_read(ObjectName,
     					{some, GetAttrs},
     					Context)
     	   end,
        {ok, Attributes2} = safs_imm_om:check_rt_attrs(ObjectName, Attributes, Context),
        {reply, safs_imm_om:add_types_to_attributes(Lang, ClassName, Attributes2), State}
     catch
     	throw:no_exists ->
     		{reply, {error, sa_ais_err_not_exist}, State};
     	throw:Term -> {reply, Term, State}
     end;

handle_call(ccb_validate, _From, #state{validated=Validated} = State) when Validated =/= no ->
    {reply, {error, sa_ais_err_failed_operation}, State};
handle_call(ccb_validate, From, #state{id=CcbId, augments=Augments} = State) ->
    try
	 safs_imm_om:ccb_delete_error_strings(CcbId),
	 case Augments of
	     undefined ->
		 ccb_validate_1(From, State);
	     _ ->
		 safs_error(ccb_validate, "Not allowed to validate augmented function"),
		 {reply, {error, sa_ais_err_bad_operation}, State}
	end
    catch
	throw:Term ->
	    {reply, Term, State}
    end;

handle_call(ccb_apply, From, #state{id=CcbId, augments=Augments} = State) ->
    try
	safs_imm_om:ccb_delete_error_strings(CcbId),
        case Augments of
	    undefined ->
		ccb_apply_1(From, State);
	    _ ->
		ccb_apply_augmented(State)
	end
    catch
	throw:Term ->
	    %NewState = clear_ccb(State),	%%LATH: Clear should be made at abort/finalize
	    {reply, Term, State#state{validated=failed}}
    end;

handle_call(ccb_abort, From, #state{id=CcbId, implementers=Impls} = State) ->
    try
        safs_imm_om:ccb_delete_error_strings(CcbId),
        %% Callback to Implementers
	NewState1 = clear_ccb(State),
        async_apply_to_implementers(ccb_abort, lists:reverse(Impls), CcbId),
        {noreply, {wait_for_callback, abort, From, undefined, {noreply, NewState1}, undefined, undefined}}
     catch
     	throw:Term ->
             NewState2 = clear_ccb(State), %%LATH: This should probably clear the CCB
	     {reply, Term, NewState2}
     end;

handle_call(get_fun_list, _From, #state{operations=Ops, references=Refs} = State) ->
    Ops2 = lists:append(Refs, Ops),
    {reply, {ok, lists:reverse(Ops2)}, State};

handle_call(send_completed, _From, #state{validated=Validated} = State) when Validated =/= no ->
    {reply, {error, sa_ais_err_failed_operation}, State};
handle_call(send_completed, From,
	    #state{id=CcbId, implementers=Impls, marked_for_abortion=MarkedForAbortion} = State) ->
    try
	safs_imm_om:ccb_delete_error_strings(CcbId),
        case MarkedForAbortion of
     	    true ->
     		safs_error(send_completed,
			   {"Transaction failed",
			    {aborted, "Augmented transaction aborted"}}),
     		throw({abort, {error, sa_ais_err_failed_operation}});
     	    _ ->
     		ok
     	end,
        % ReplyFun =
	%     fun(Result) ->
	% 	    Result
	%     end,
        async_apply_to_implementers(ccb_completed, lists:reverse(Impls), CcbId),
        {noreply, {wait_for_callback, completed, From, undefined, validate, State, State}}
     catch
         throw:{abort, ErrorTuple} ->
                {reply, ErrorTuple, State#state{validated=failed}}
     end;

handle_call(send_apply, From, #state{id=CcbId, implementers=Impls} = State) ->
    try
        %% Callback to Implementer
     	NewState1 = clear_ccb(State),
        async_apply_to_implementers(ccb_apply, lists:reverse(Impls), CcbId),
        {noreply, {wait_for_callback, apply, From, undefined, {noreply, NewState1}, State, NewState1}}
    catch
        throw:Term ->
            NewState2 = clear_ccb(State), %%LATH: Clear at abort ???
	    {reply, Term, NewState2}
    end;

handle_call({insert_fun, Fun}, _From, #state{operations=Ops} = State) ->
    {reply, ok, State#state{operations=[Fun |Ops]}};

handle_call({affected_by_ccb, ClassName}, _From, #state{objects=Objs} = State) ->
    Result =
	lists:any(
	  fun({_Object, ObjectClassName}) ->
		  ClassName =:= ObjectClassName
	  end,
	  Objs),
    {reply, Result, State};

handle_call({affected_by_ccb, ClassName}, _From,
	    {wait_for_callback, _, _,  _, _, _, #state{objects=Objs}} = State) ->
    Result =
	lists:any(
	  fun({_Object, ObjectClassName}) ->
		  ClassName =:= ObjectClassName
	  end,
	  Objs),
    {reply, Result, State};

handle_call({affected_by_ccb, DN, Scope}, _From, #state{objects=Objs} = State) ->
    Result = affected_by_ccb_1(DN, Scope, Objs),
    {reply, Result, State};

handle_call({affected_by_ccb, DN, Scope}, _From,
	    {wait_for_callback, _, _,  _, _, _, #state{objects=Objs}} = State) ->
    Result = affected_by_ccb_1(DN, Scope, Objs),
    {reply, Result, State};

handle_call({augment_ccb_initialize, ParentPid, OiName, OwnerPid},
	    _From, State) ->
    {ok, OwnerHandle, CcbFlags, Context, CbType} = get_parent_state(ParentPid),
    case check_augmentation_allowed(CbType) of
	{ok, AugOpsAllowed} ->
	    {reply, {ok, OwnerHandle}, State#state{owner_handle=OwnerHandle,
						   ccb_flags = CcbFlags,
						   context=Context,
						   implementer_name=OiName,
						   owner_pid=OwnerPid,
						   augments=ParentPid,
						   ops_allowed=AugOpsAllowed}};
	{error, Reason} ->
	    safs_error(handle_call, {augment_ccb_initialize, Reason, CbType}),
	    {reply, {error, sa_ais_err_bad_operation}, State}
    end;

handle_call(get_parent_state, _From,
	    {wait_for_callback, CbType, _,  _, _, _, #state{owner_handle=OwnerHandle,
						    ccb_flags = CcbFlags,
						    context=Context}} = CbState) ->
    {reply, {ok, OwnerHandle, CcbFlags, Context, CbType}, CbState};

handle_call({update_parent_state, Ops, Refs, Objs, Impls, Context}, _From,
	    {wait_for_callback, CbType, Client, F, DeleteLoopData, OldState, NewState}) ->
    % io:format("Update parent state: \nOps: ~p\nObjs:~p\n",
    % 	      [Ops, Objs]),
    NewImpls = add_unique(Impls, NewState#state.implementers),
    {reply, ok, {wait_for_callback, CbType, Client, F, DeleteLoopData, OldState,
		 NewState#state{operations = lists:append(Ops, NewState#state.operations),
				references = lists:append(Refs, NewState#state.references),
				objects = lists:append(Objs, NewState#state.objects),
				implementers = NewImpls,
				context = Context}}};

handle_call(mark_parent_for_abortion, _From,
	    {wait_for_callback, CbType, Client, F, DeleteLoopData, OldState, NewState}) ->
    {reply, ok, {wait_for_callback, CbType, Client, F, DeleteLoopData, OldState,
		 NewState#state{marked_for_abortion = true}}};

handle_call({object_create_s2, _ClassName,  _ParentName, _AttrValues, _ExtraAttrValues},
	    _From, #state{validated=Validated} = State) when Validated =/= no ->
    {reply, {error, sa_ais_err_failed_operation}, State};
handle_call({object_create_s2, ClassName,  ParentName, AttrValues, ExtraAttrValues}, From, State) ->
    handle_object_create_s2(ClassName,  ParentName, AttrValues, ExtraAttrValues, From, State);

handle_call({object_modify_extra_attrs_s2, _ObjectName, _AttrMods}, _From,
	    #state{validated=Validated} = State) when Validated =/= no ->
    {reply, {error, sa_ais_err_failed_operation}, State};
handle_call({object_modify_extra_attrs_s2, ObjectName, AttrMods}, From,
	    #state{id=CcbId, owner_handle = OwnerHandle, ccb_flags = CcbFlags, operations = Ops,
		   references = _Refs, objects = Objects, implementers = Impls, augments = Augments,
		   context = Context} = State) ->
    try
        ok =:= check_allowed_extra_attributes(AttrMods) orelse throw({invalid_param,
								      "Extra attribute not defined"}),
	AO =  safs_imm_om:adm_owner(OwnerHandle),
        {OiFun, Oi, Appliers} = safs_imm_db:oi_get(ObjectName, Context),
        check_oi_reg_flags(ObjectName, ccb_object_modify_2, CcbFlags, Oi),

        {ok, Attrs} = safs_imm_lib:convert_attrmod_list(AttrMods),
       	{ok, ObjectClassName} = safs_imm_db:get_class(ObjectName, Context),
        {Fun, NewContext} = safs_imm_db:ccb_set_extra_attributes(AO#safs_imm_om_adm_owner.name,
								 ObjectName,
								 Attrs,
								 Context),
        if
      	    Oi == undefined orelse (Augments =/= undefined andalso State#state.implementer_name == Oi) ->
      		%% Callbacks to Appliers
     		[safs_imm_oi:ccb_object_modify_2(Applier, self(), CcbId, ObjectName, AttrMods)
      		 || Applier <- Appliers],

		NewState = State#state{operations = [Fun, OiFun |Ops],
				       objects = [{safs_imm_lib:mk_dnlist(ObjectName), ObjectClassName}
						  |Objects],
				       implementers = add_unique([{applier, Appliers}], Impls),
				       context=NewContext},
		{reply, ok, NewState};
	    true ->
		ReplyFun =
		    fun() ->
			    %% Callbacks to Appliers
			    [safs_imm_oi:ccb_object_modify_2(Applier, self(), CcbId, ObjectName, AttrMods)
			     || Applier <- Appliers],
			    %% Callbacks to Special Appliers
			    ok
		    end,
      	        NewState = State#state{operations = [Fun, OiFun |Ops],
				       objects = [{safs_imm_lib:mk_dnlist(ObjectName), ObjectClassName}
						  |Objects],
				       implementers = add_unique([{applier, Appliers}, Oi], Impls),
				       context=NewContext} ,

		%% Callback to Implementer
		async_ccb_object_modify_2(Oi, CcbId, ObjectName, AttrMods),
		{noreply, {wait_for_callback, object_modify_2, From, ReplyFun, undefined,
			   State#state{implementers = add_unique(Oi, Impls)}, NewState}}
	 end
    catch
     	throw:no_exists ->
     		{reply, {error, sa_ais_err_not_exist}, State};
	throw:{invalid_param, {_,_,_,_} = Error} ->
	      error_logger:error_msg("object_modify_2 for object ~p failed:\n  ~p\n",
				     [ObjectName, safs_imm_db:error_msg(Error)]),
	      {reply, {error, sa_ais_err_invalid_param}, State};
	throw:{invalid_param, _Error} ->
	      {reply, {error, sa_ais_err_invalid_param}, State};
      	throw:{bad_operation, What} ->
	      case What of
		  {not_ao, AdmOwner} ->
		      error_logger:info_msg("Admin owner: ~p not owner of object: ~p\n",
					    [AdmOwner, ObjectName]);
		  not_config ->
		      error_logger:info_msg("Not a configuration object: ~p\n", [ObjectName])
	      end,
	      {reply, {error, sa_ais_err_bad_operation}, State};
      	throw:bad_operation ->
	      {reply, {error, sa_ais_err_bad_operation}, State};
      	 throw:already_exists ->
	      {reply, {error, sa_ais_err_invalid_param}, State};
      	throw:not_owner ->
	      {reply, {error, sa_ais_err_bad_operation}, State};
     	throw:Error2 -> {reply, Error2, State}
    end;

handle_call({object_read_extra_attrs_s2, ObjectName, Lang}, _From,
	    #state{id=_CcbId, context = Context} = State) ->
    try
	{_Fun, Attributes} = safs_imm_db:ccb_read_extra_attributes(ObjectName, Context),
        {reply, safs_imm_om:add_types_to_attributes(Lang, extra_attributes, Attributes), State}
    catch
     	throw:no_exists ->
     		{reply, {error, sa_ais_err_not_exist}, State};
     	throw:Term -> {reply, Term, State}
    end;

handle_call(_, _From, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({create_callback_reply, Reply}, {wait_for_callback, object_create_2, From, F, undefined,
					     OldState, NewState}) ->
    case Reply of
	ok ->
	    F(),
	    gen_server:reply(From, ok),
	    {noreply, NewState};
	{error, sa_ais_err_try_again} -> %%LATH: shouldn't be valid after changes in OI cb's
	    error_logger:info_msg("safs_imm_om_ccb - create_callback_reply: ~p\n", [From]),
	    gen_server:reply(From, {error, sa_ais_err_try_again}),
	    {noreply, OldState};
	{error, Error} ->
	    gen_server:reply(From, {error, Error}),
	    {noreply, OldState#state{validated=failed}}
    end;
handle_info({modify_callback_reply, Reply}, {wait_for_callback, object_modify_2, From, F, undefined,
					     OldState, NewState}) ->
    case Reply of
	ok ->
	    F(),
	    gen_server:reply(From, ok),
	    {noreply, NewState};
	{error, sa_ais_err_try_again} -> %%LATH: shouldn't be valid after changes in OI cb's
	    error_logger:info_msg("safs_imm_om_ccb - modify_callback_reply: ~p\n", [From]),
	    gen_server:reply(From, {error, sa_ais_err_try_again}),
	    {noreply, OldState};
	{error, Error} ->
	    gen_server:reply(From, {error, Error}),
	    {noreply, OldState#state{validated=failed}}
    end;
handle_info({delete_callback_reply, Reply}, {wait_for_callback, object_delete, From, F, DeleteLoopData,
					     OldState, NewState}) ->
    #state{operations=Ops0, objects=Objects0, implementers=Impls0} = NewState,
    case Reply of
	ok ->
	    {Op, Object, Impls} = F(),
	    NewImpls = add_unique(Impls, Impls0),
	    delete({wait_for_callback, object_delete, From, undefined, DeleteLoopData, OldState,
		    NewState#state{operations=[Op|Ops0], objects=[Object|Objects0], implementers=NewImpls}});
	{error, sa_ais_err_try_again} -> %%LATH: shouldn't be valid after changes in OI cb's
	    error_logger:info_msg("safs_imm_om_ccb - delete_callback_reply: ~p\n", [From]),
	    gen_server:reply(From, {error, sa_ais_err_try_again}),
	    {noreply, OldState};
	{error, Error} ->
	    gen_server:reply(From, {error, Error}),
	    {noreply, OldState#state{validated=failed}}
    end;
handle_info({apply_to_impl_callback_reply, ccb_completed, Result},
	    {wait_for_callback, completed, From, _F, validate, _OldState, NewState}) ->
    gen_server:reply(From, Result),
    case Result of
	ok ->
	    {noreply, NewState};
	_ ->
	    {noreply, NewState#state{validated=failed}}
    end;
handle_info({apply_to_impl_callback_reply, ccb_completed, Result},
	    {wait_for_callback, completed, From, F, apply,
	     #state{id=Id, implementers=Impls} = OldState,
	     NewState}) when is_function(F, 1) ->
    case F(Result) of
	ok ->
	    %% Callback to Implementer
	    NewState1 = clear_ccb(NewState),
	    % io:format("CCB: ~p Pid: ~p sending ccb_apply cb \n", [Id, self()]),
	    async_apply_to_implementers(ccb_apply, lists:reverse(Impls), Id),
	    {noreply, {wait_for_callback, apply, From, undefined, {noreply, NewState1},
		       NewState, NewState1}};
	Error ->
	    gen_server:reply(From, Error),
	    {noreply, OldState#state{validated=failed}}
    end;

handle_info({apply_to_impl_callback_reply, ccb_apply, Result},
	    {wait_for_callback, apply, From, undefined, ReturnValue, _OldState, _NewState}) ->
    gen_server:reply(From, Result),
    ReturnValue;
handle_info({apply_to_impl_callback_reply, ccb_abort, Result},
	    {wait_for_callback, abort, From, undefined, ReturnValue, _OldState, _NewState}) ->
    gen_server:reply(From, Result),
    ReturnValue;
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info(_Msg, State) ->
    %error_logger:format("~p~p got unexpected message:\n\t~p\n\t~p\n",
    %                    [?MODULE, self(), Msg, State]),
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
handle_object_create_s2(ClassName,  ParentName, AttrValues, ExtraAttrValues, From,
			#state{id=CcbId, owner_handle=OwnerHandle, ccb_flags=CcbFlags, operations=Ops,
			       references=Refs, objects=Objects, implementers=Impls, augments=Augments,
			       context=Context} = State) ->
    try
	safs_imm_om:ccb_delete_error_strings(CcbId),
        ok =:= check_allowed_extra_attributes(ExtraAttrValues) orelse throw({invalid_param,
									     "Extra attribute not defined"}),
        {ok, Attrs} = safs_imm_lib:convert_attr_list(AttrValues),
        {ok, ExtraAttrs} = safs_imm_lib:convert_attr_list(ExtraAttrValues),
        ParentName2 = safs_imm_lib:check_if_root(ParentName),

        AO =  safs_imm_om:adm_owner(OwnerHandle),
        OwnerFun =
	  case ParentName2 of
	      <<>> ->
		  fun() -> ok end;
	      _ ->
		  check_owner(AO#safs_imm_om_adm_owner.name, ParentName2, Context)
	  end,

        ClassName2 = list_to_atom(binary_to_list(ClassName)),
        {CiFun, Ci, Appliers} = safs_imm_db:ci_get(ClassName2),
        SpecialAppliers = safs_imm_oi:get_special_appliers_if_registered(),

        check_oi_reg_flags(ClassName, ccb_object_create_2, CcbFlags, Ci),

        {Fun, RefFun, DN, DefaultAttrValues, NewContext} =
	      safs_imm_db:ccb_add(AO#safs_imm_om_adm_owner.name,
				  ParentName2,
				  ClassName2,
				  Attrs,
				  ExtraAttrs,
				  Context),
        {ok,  DefaultAttrValues2} = safs_imm_om:add_types_to_attributes(c, ClassName2, DefaultAttrValues),
        AttrValues2 = lists:append(AttrValues, DefaultAttrValues2),
        AttrValues3 = lists:append(AttrValues2, ExtraAttrValues),

        %% LATH: Is the context correct when read is implemented in augmented transactions
        {_, AttrDefs} = safs_imm_db:get_class_def(ClassName2),
        NotifiableAttrs = safs_imm_lib:get_notifiable_attrs(AttrDefs),
        Appliers1 =
	  case NotifiableAttrs of
	      [] -> Appliers;
	      _ -> SpecialAppliers ++ Appliers
	  end,
        if
	    Ci == undefined orelse (Augments =/= undefined andalso State#state.implementer_name == Ci) ->

		%%io:format("A Parent: ~p\n", [ParentName]),
		%% Callbacks to Special Appliers
		send_cb_to_special_appliers(NotifiableAttrs, SpecialAppliers, AO#safs_imm_om_adm_owner.name,
					    {create, CcbId, ClassName, ParentName, AttrValues2}, []),

		%% Callbacks to Appliers
		[safs_imm_oi:ccb_object_create_2(Applier, self(), CcbId, ClassName, ParentName, AttrValues3)
		 || Applier <- Appliers],

                NewState = State#state{operations = [Fun, CiFun, OwnerFun |Ops],
				       references = [RefFun | Refs],
				       objects = [{DN, ClassName2} |Objects],
				       implementers = add_unique([{applier, Appliers1}], Impls),
				       context=NewContext},

		safs_imm_om:add_ao_owned_object(OwnerHandle, DN),
		{reply, ok, NewState};
	    true ->
		ReplyFun =
		    fun() ->
			    %% Callbacks to Appliers
			    [safs_imm_oi:ccb_object_create_2(Applier, self(), CcbId, ClassName, ParentName, AttrValues3)
			     || Applier <- Appliers],
			    safs_imm_om:add_ao_owned_object(OwnerHandle, DN),
			    ok
		    end,
                NewState = State#state{operations = [Fun, CiFun, OwnerFun |Ops],
				       references = [RefFun | Refs],
				       objects = [{DN, ClassName2} |Objects],
				       implementers = add_unique([{applier, Appliers1}, Ci], Impls),
				       context=NewContext},


		%%io:format("B Parent: ~p\n", [ParentName]),
		%% Callbacks to Special Appliers
		send_cb_to_special_appliers(NotifiableAttrs, SpecialAppliers,
					    AO#safs_imm_om_adm_owner.name,
					    {create, CcbId, ClassName, ParentName,
					     AttrValues2}, []),

		%% Callback to Implementer
		% io:format("CCB: ~p Pid: ~p sending ccb_object_create_2 cb \n", [CcbId, self()]),
		async_ccb_object_create_2(Ci, CcbId, ClassName, ParentName, AttrValues3),
		{noreply, {wait_for_callback, object_create_2, From, ReplyFun, undefined,
			   State#state{implementers = add_unique(Ci, Impls)}, NewState}}
	end
    catch
	throw:no_such_class ->
		{reply, {error, sa_ais_err_not_exist}, State};
	throw:already_exists  ->
		{reply, {error, sa_ais_err_exist}, State};
	throw:{invalid_param, {_,_,_,_} = Error} ->
	        error_logger:error_msg("object_create_2 for class ~p failed:\n  ~p\n",
				     [ClassName, safs_imm_db:error_msg(Error)]),
		{reply, {error, sa_ais_err_invalid_param}, State};
	throw:{invalid_param, _Error} ->
		{reply, {error, sa_ais_err_invalid_param}, State};
	throw:no_exists ->
		{reply, {error, sa_ais_err_not_exist}, State};
	throw:bad_operation ->
		{reply, {error, sa_ais_err_bad_operation}, State};
	throw:not_owner ->
		{reply, {error, sa_ais_err_bad_operation}, State};
	throw:ErrMsg -> {reply, ErrMsg, State}
    end.

%%--------------------------------------------------------------------
ccb_apply_1(From, State) ->
    #state{id=Id, operations=Ops, references=Refs, implementers=Impls,
	   marked_for_abortion=MarkedForAbortion, validated=Validated} = State,
    try
	case {MarkedForAbortion, Validated} of
	    {true, _} ->
		safs_error(ccb_apply,
			       {"Transaction failed",
				{aborted, "Augmented transaction aborted"}}),
		%%LATH: Error string?
		throw({abort, {error, sa_ais_err_failed_operation}});
	    {false, failed} ->
		safs_error(ccb_apply,
			       {"Transaction failed",
				{aborted, "Earlier validation failed"}}),
		%%LATH: Error string?
		throw({abort, {error, sa_ais_err_failed_operation}});
	    {false, ok} ->
		Ops2 = lists:append(Refs, Ops),
		case apply_db(lists:reverse(Ops2)) of
		    {aborted, Reason} ->
			format_transaction_error_string(Id, Reason),
			safs_error(ccb_apply,
				       {"Transaction failed",
					{aborted, Reason}}),
			throw({abort, {error, sa_ais_err_failed_operation}});
		    {atomic, ok} ->
			case safs:get_env(imm_sync_cb, undefined) of
			    {M, F} ->
				M:F();
			    undefined ->
				ok
			end,
			%% Callback to Implementer
			NewState1 = clear_ccb(State),
			async_apply_to_implementers(ccb_apply, lists:reverse(Impls), Id),
			{noreply, {wait_for_callback, apply, From, undefined, {noreply, NewState1}, State, NewState1}}
		end;
	    {false, no}  ->
		%% Run transaction with a special abort function at the end
		Ops2 = lists:append(Refs, Ops),
		case apply_db(lists:reverse([fun() -> mnesia:abort(validation_ok) end |Ops2])) of
		    {aborted, validation_ok} ->
			ok;
		    {aborted, ValidateReason} ->
			%%LATH: Error string?
			format_transaction_error_string(Id, ValidateReason),
			safs_error(ccb_validate,
				   {"Transaction failed",
				    {aborted, ValidateReason}}),
			throw({abort, {error, sa_ais_err_failed_operation}})
		end,

		ReplyFun =
		    fun(ok) ->
			    case apply_db(lists:reverse(Ops2)) of
				{aborted, Reason} ->
				    format_transaction_error_string(Id, Reason),
				    safs_error(ccb_apply,
					       {"Transaction failed",
						{aborted, Reason}}),
				    {error, sa_ais_err_failed_operation};
				{atomic, ok} ->
				    case safs:get_env(imm_sync_cb, undefined) of
					{M, F} ->
					    M:F(),
					    ok;
					undefined ->
					    ok
				    end
			    end;
		       (Error) ->
			    Error
		    end,
		async_apply_to_implementers(ccb_completed, lists:reverse(Impls), Id),
		{noreply, {wait_for_callback, completed, From, ReplyFun, apply, State, State}}
	end
    catch
        throw:{abort, ErrorTuple} ->
                {reply, ErrorTuple, State#state{validated=failed}};
	throw:_Term ->
	        {reply, {error, sa_ais_err_failed_operation}, State#state{validated=failed}}
    end.

%%--------------------------------------------------------------------
ccb_apply_augmented(#state{operations=Ops, references=Refs, objects=Objs, implementers=Impls,
			   marked_for_abortion=MarkedForAbortion, context=Context, augments=ParentPid} = State) ->
    try
	%%ParentPid = safs_imm_om:get_pid_from_ccb_id(ParentCcbId),
        case MarkedForAbortion of
	    true ->
		safs_error(ccb_apply,
			       {"Transaction failed",
				{aborted, "Augmented transaction aborted"}}),
		mark_parent_for_abortion(ParentPid),
		throw({abort, {error, sa_ais_err_failed_operation}});
	    _ ->
		ok
	end,

        case Ops of
	    [] ->
		{reply, ok, clear_ccb(State)};
	    Ops ->
		update_parent_state(ParentPid, Ops, Refs, Objs, Impls, Context),
		{reply, ok, clear_ccb(State)}
	end
    catch
        throw:{abort, ErrorTuple} ->
		{reply, ErrorTuple, State#state{validated=failed}};
	throw:Term ->
	        {reply, Term, State#state{validated=failed}}
    end.

%%--------------------------------------------------------------------
ccb_validate_1(From, State) ->
    #state{id=CcbId, operations=Ops, references=Refs, implementers=Impls,
	   marked_for_abortion=MarkedForAbortion} = State,
    try
	case MarkedForAbortion of
	    true ->
		safs_error(ccb_validate,
			   {"Transaction failed",
			    {aborted, "Augmented transaction aborted"}}),
		%%LATH: Error string?
		throw({abort, {error, sa_ais_err_failed_operation}});
	    _ ->
		ok
	end,

        %% Run transaction with a special abort function at the end
        Ops2 = lists:append(Refs, Ops),
        case apply_db(lists:reverse([fun() -> mnesia:abort(validation_ok) end |Ops2])) of
	    {aborted, validation_ok} ->
		ok;
	    {aborted, Reason} ->
		%%LATH: Error string?
		format_transaction_error_string(CcbId, Reason),
		safs_error(ccb_validate,
			   {"Transaction failed",
			    {aborted, Reason}}),
		throw({abort, {error, sa_ais_err_failed_operation}})
	end,

        async_apply_to_implementers(ccb_completed, lists:reverse(Impls), CcbId),
        {noreply, {wait_for_callback, completed, From, undefined, validate, State, State#state{validated=ok}}}
    catch
        throw:{abort, ErrorTuple} ->
                {reply, ErrorTuple, State#state{validated=failed}};
	throw:_Term ->
	        {reply, {error, sa_ais_err_failed_operation}, State#state{validated=failed}}
    end.

%%----------------------------------------------------------------------
%% Help functions for ccb_finalize
%% @private
ccb_finalize_1(From, #state{id=CcbId, implementers=Impls} = State) ->
    async_apply_to_implementers(ccb_abort, lists:reverse(Impls), CcbId),
    {noreply, {wait_for_callback, abort, From, undefined, {stop, normal, State}, undefined, undefined}}.

%%--------------------------------------------------------------------
ccb_finalize_augmented(From, #state{id=CcbId, operations=Ops, implementers=Impls,
				    augments=ParentPid} = State) ->
    case Ops of
	[] ->
	    {stop, normal, ok, State};
	_ ->
	    mark_parent_for_abortion(ParentPid),
	    % ReplyFun =
	    % 	fun() ->
	    % 	   %%ParentPid = safs_imm_om:get_pid_from_ccb_id(ParentCcbId),
	    % 	   mark_parent_for_abortion(ParentPid)
	    % 	end,
	    async_apply_to_implementers(ccb_abort, lists:reverse(Impls), CcbId),
	    {noreply, {wait_for_callback, abort, From, undefined, {stop, normal, State}, undefined, undefined}}
    end.

%%--------------------------------------------------------------------
check_augmentation_allowed(T) when T =:= apply;
				   T =:= abort ->
    {error, augmentation_not_allowed};
check_augmentation_allowed(completed) ->
    {ok, read};
check_augmentation_allowed(_) ->
    {ok, all}.
%%--------------------------------------------------------------------
check_oi_reg_flags(_ObjectOrClass, _Operation, 0, _ImplementerName) ->
    ok;
check_oi_reg_flags(ObjectOrClass, Operation, Flags, ImplementerName) ->
    case {lists:member(ccb_registered_oi, Flags),
	  lists:member(ccb_allow_null_oi, Flags),
	  ImplementerName} of
	{true, true, undefined} ->
	    ok;
	{true, false, undefined} ->
	    error_logger:warning_msg("No Implementer registered and CCB initialized "
				     "with SA_IMM_CCB_REGISTERED_OI\n"
				     "Object/Class: ~p\n"
				     "Operation: ~p\n",
				     [ObjectOrClass, Operation]),
	    throw(no_exists);
	{true, _, Name} ->
	    case safs_imm_oi:is_oi_attached(Name) of
		true ->
		    ok;
		false ->
		    error_logger:warning_msg("No Implementer with the registered name and CCB "
					     "initialized with SA_IMM_CCB_REGISTERED_OI\n"
					     "Implementer name: ~p\n"
					     "Object/Class: ~p\n"
					     "Operation: ~p\n",
					     [Name, ObjectOrClass, Operation]),
		    throw(no_exists)
	    end;
	_ ->
	    ok
    end.

%%--------------------------------------------------------------------
%% Function: affected_by_ccb_1/3
%% Description:
%%--------------------------------------------------------------------
affected_by_ccb_1(DN, sa_imm_one, ObjectList) ->
    lists:any(fun({Object, _}) when Object =:= DN ->
		      true;
		 (_) ->
		      false
	      end, ObjectList);
affected_by_ccb_1(DN, sa_imm_sublevel, ObjectList) ->
    lists:any(fun({Object, _}) when Object =:= DN ->
		      true;
		 ({Object, _}) -> [_ | Obj1] = lists:reverse(Object),
				  lists:reverse(DN) =:= Obj1
	      end, ObjectList);
affected_by_ccb_1(DN, sa_imm_subtree, ObjectList) ->
		      lists:any(fun({Object, _}) -> lists:prefix(DN, Object) end, ObjectList).

%%--------------------------------------------------------------------

apply_to_implementers(_CcbPid, _F, [], _CcbId) ->
    ok;
apply_to_implementers(CcbPid, F, [{applier, Impl} |Impls], CcbId) ->
    safs_imm_oi:F(Impl, CcbPid, CcbId),
    apply_to_implementers(CcbPid, F, Impls, CcbId);
apply_to_implementers(CcbPid, F, [Impl |Impls], CcbId) ->
    case safs_imm_oi:F(Impl, CcbPid, CcbId) of
	sa_ais_ok ->
	    apply_to_implementers(CcbPid, F, Impls, CcbId);
	ok ->
	    apply_to_implementers(CcbPid, F, Impls, CcbId);
	Error ->
	    Error
    end.

apply_to_implementers_parallel(CcbPid, Cb, Impls, CcbId) ->
    Pid = self(),
    NoOfImpls = apply_to_implementers_parallel(Pid, CcbPid, Cb, Impls, CcbId, 0),
    apply_to_implementers_parallel_1(CcbPid, Cb, NoOfImpls, ok).

apply_to_implementers_parallel_1(CcbPid, Cb, 0, Result) ->
    CcbPid ! {apply_to_impl_callback_reply, Cb, Result},
    exit(normal);
apply_to_implementers_parallel_1(CcbPid, Cb, NoOfImpls, Result) when Result == ok ->
    receive
	{cb_reply, sa_ais_ok} ->
	    apply_to_implementers_parallel_1(CcbPid, Cb, NoOfImpls-1, Result);
	{cb_reply, ok} ->
	    apply_to_implementers_parallel_1(CcbPid, Cb, NoOfImpls-1, Result);
	{cb_reply, Error} ->
	    apply_to_implementers_parallel_1(CcbPid, Cb, NoOfImpls-1, Error)
    end;
apply_to_implementers_parallel_1(CcbPid, Cb, NoOfImpls, Result) ->
    receive
	{cb_reply, _} ->
	    apply_to_implementers_parallel_1(CcbPid, Cb, NoOfImpls-1, Result)
    end.

apply_to_implementers_parallel(_Pid, _CcbPid, _F, [], _CcbId, NoOfImpls) ->
    NoOfImpls;
apply_to_implementers_parallel(Pid, CcbPid, F, [{applier, Impl} |Impls], CcbId, NoOfImpls) ->
    spawn_link(fun() ->
		       Result = safs_imm_oi:F(Impl, CcbPid, CcbId),
		       Pid ! {cb_reply, Result},
		       exit(normal)
	       end),
    apply_to_implementers_parallel(Pid, CcbPid, F, Impls, CcbId, NoOfImpls+1);
apply_to_implementers_parallel(Pid, CcbPid, F, [Impl|Impls], CcbId, NoOfImpls) ->
    spawn_link(fun() ->
		       Result = safs_imm_oi:F(Impl, CcbPid, CcbId),
		       Pid ! {cb_reply, Result},
		       exit(normal)
	       end),
    apply_to_implementers_parallel(Pid, CcbPid, F, Impls, CcbId, NoOfImpls+1).

%%--------------------------------------------------------------------
add_applier_unique([], List) ->
    List;
add_applier_unique([E |AL], List) ->
    NewList = add_applier_unique(E, List),
    add_applier_unique(AL, NewList);
add_applier_unique(E, List) ->
    case lists:member(E, List) of
	true ->
	    List;
	false ->
	    case lists:member({applier, E}, List) of
		true ->
		    List;
		false ->
		    [{applier, E} |List]
	    end
    end.

%%--------------------------------------------------------------------
add_unique([], List) ->
    List;
add_unique([undefined |L], List) ->
    add_unique(L, List);
add_unique([{applier, AL} |L] , List) ->
    NewList = add_applier_unique(AL, List),
    add_unique(L, NewList);
add_unique([E |L] , List) ->
    NewList = add_unique(E, List),
    add_unique(L, NewList);
add_unique({applier, AL}, List) ->
    add_applier_unique(AL, List);
add_unique(E, List) ->
    case lists:member(E, List) of
	true ->
	    List;
	false ->
	    case lists:member({applier, E}, List) of
		true ->
		    lists:delete({applier, E}, List),
		    [E |List];
		false ->
		    [E |List]
	    end
    end.
%%--------------------------------------------------------------------
send_cb_to_special_appliers([], _, _, _, _) ->
    ok;
send_cb_to_special_appliers(_, [], _, _, _) ->
    ok;
send_cb_to_special_appliers(NotifiableAttrs, SpecialAppliers, AO,
			    {create, CcbId, ClassName, ParentName, AttrValues}, _) ->
    %% io:format("NotifAttrs: ~p\n", [NotifiableAttrs]),
    NewAttrValues = lists:filtermap(fun(#safsImmAttrValues_2{attrName=AttrName} = Attr) ->
					    case lists:keymember(ta(AttrName), 1, NotifiableAttrs) of
						true ->
						    {true, Attr};
						false -> false
					    end
				    end,
				    AttrValues),
    {ok, AOValues} = safs_imm_lib:set_attr_values(sa_imm_attr_sastringt, [tb(AO)]),
    {ok, ClassNameValues} = safs_imm_lib:set_attr_values(sa_imm_attr_sastringt, [tb(ClassName)]),
    [safs_imm_oi:ccb_object_create_2(Applier, self(), CcbId, ClassName, ParentName,
				     [#safsImmAttrValues_2{attrName = <<"saImmAttrAdminOwnerName">>,
							   attrValueType=sa_imm_attr_sastringt,
							   attrValuesNumber=length(AOValues),
							   attrValues=AOValues},
				      #safsImmAttrValues_2{attrName = <<"saImmAttrClassName">>,
							   attrValueType=sa_imm_attr_sastringt,
							   attrValuesNumber=length(ClassNameValues),
						           attrValues=ClassNameValues}
				      | NewAttrValues])
     || Applier <- SpecialAppliers];
send_cb_to_special_appliers(_NotifiableAttrs, SpecialAppliers, AO, {delete, CcbId, ObjectName}, _) ->
    %% io:format("NotifAttrs: ~p\n", [NotifiableAttrs]),
    {ok, AOValues} = safs_imm_lib:set_attr_values(sa_imm_attr_sastringt, [tb(AO)]),
    F =
	fun(Applier) -> safs_imm_oi:ccb_object_modify_2(Applier, self(), CcbId, ObjectName,
							[#safsImmAttrModification_2{
                                                            modType=sa_imm_attr_values_replace,
					                    modAttr=#safsImmAttrValues_2{
							      attrName = <<"saImmAttrAdminOwnerName">>,
							      attrValueType=sa_imm_attr_sastringt,
							      attrValuesNumber=length(AOValues),
							      attrValues=AOValues}}]),
			safs_imm_oi:ccb_object_delete(Applier, self(), CcbId, ObjectName) end,
    [F(Applier) || Applier <- SpecialAppliers];
send_cb_to_special_appliers(NotifiableAttrs, SpecialAppliers, {AO, ClassName},
			    {modify, CcbId, ObjectName, AttrMods}, Changed) ->
    %% io:format("NotifAttrs: ~p\n", [NotifiableAttrs]),
    NewAttrMods = lists:filtermap(fun(#safsImmAttrModification_2{modAttr=Attr} = AttrMod) ->
					  Name = ta(Attr#safsImmAttrValues_2.attrName),
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
	    {ok, ClassNameValues} = safs_imm_lib:set_attr_values(sa_imm_attr_sastringt, [tb(ClassName)]),
	    {ok, AOValues} = safs_imm_lib:set_attr_values(sa_imm_attr_sastringt, [tb(AO)]),
	    [safs_imm_oi:ccb_object_modify_2(Applier, self(), CcbId, ObjectName,
		[#safsImmAttrModification_2{modType=sa_imm_attr_values_replace,
					    modAttr=#safsImmAttrValues_2{attrName = <<"saImmAttrAdminOwnerName">>,
									 attrValueType=sa_imm_attr_sastringt,
									 attrValuesNumber=length(AOValues),
									 attrValues=AOValues}},
		 #safsImmAttrModification_2{modType=sa_imm_attr_values_replace,
					    modAttr=#safsImmAttrValues_2{attrName = <<"saImmAttrClassName">>,
									 attrValueType=sa_imm_attr_sastringt,
									 attrValuesNumber=length(ClassNameValues),
									 attrValues=ClassNameValues}}
		 | NewAttrMods1])
	     || Applier <- SpecialAppliers]
    end.




%%--------------------------------------------------------------------
dnlist_to_bin(Path) -> safs_imm_db:key_to_bin(Path).

%%--------------------------------------------------------------------
ta(B) when is_binary(B) ->
    list_to_atom(unicode:characters_to_list(B, utf8));
ta(L) when is_list(L) ->
    list_to_atom(L);
ta(A) when is_atom(A) ->
    A.

%%--------------------------------------------------------------------
tb(B) when is_binary(B) ->
    B;
tb(L) when is_list(L) ->
    list_to_binary(L);
tb(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A)).

%%--------------------------------------------------------------------
%% Function: clear_ccb/1
%% Description:
%%--------------------------------------------------------------------
clear_ccb(State) ->
    NewCcbId =
	case State#state.augments of
	    undefined ->
		OldCcbId = State#state.id,
		NewCcbId0 = safs_imm_om:create_ccb_id(),
		safs_imm_om:change_ccbid_in_ccb(OldCcbId, NewCcbId0),
		NewCcbId0;
	    _ ->
		State#state.id
	end,
    State#state{
      id = NewCcbId,
      context = safs_imm_db:create_context(),
      operations = [],
      references = [],
      objects = [],
      implementers = [],
      %augments = undefined,
      marked_for_abortion = false,
      validated = no
     }.

%%--------------------------------------------------------------------
check_owner(AoName, DN, Context) ->
    {Fun, AoName2} = safs_imm_db:ao_get(DN, Context),
    if
	AoName =:= AoName2 ->
	    Fun;
	true ->
	    throw(not_owner)
    end.

%%--------------------------------------------------------------------
apply_db(OpList) ->
    mnesia:transaction(
      fun() ->
	      [Op() || Op <- OpList],
	      ok
      end
     ).

%%-----------------------------------------------------------------
%% Func: call/2/3
%%-----------------------------------------------------------------
call(Pid, Request) ->
    try
	gen_server:call(Pid, Request, infinity)
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

call(Pid, Request, TimeOut) ->
    try
	gen_server:call(Pid, Request, TimeOut)
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

%%-----------------------------------------------------------------
async_ccb_object_create_2(Ci, CcbId, ClassName, ParentName, AttrValues) ->
    Self = self(),
    spawn_link(fun() ->
		       Reply = safs_imm_oi:ccb_object_create_2(Ci, Self, CcbId, ClassName, ParentName, AttrValues),
		       Self ! {create_callback_reply, Reply},
		       exit(normal)
	       end),
    ok.

async_ccb_object_modify_2(Oi, CcbId, ObjectName, AttrMods) ->
    Self = self(),
    spawn_link(fun() ->
		       Reply = safs_imm_oi:ccb_object_modify_2(Oi, Self, CcbId, ObjectName, AttrMods),
		       Self ! {modify_callback_reply, Reply},
		       exit(normal)
	       end),
    ok.

async_ccb_object_delete(Oi, CcbId, DN) ->
    Self = self(),
    spawn_link(fun() ->
		       Reply = safs_imm_oi:ccb_object_delete(Oi, Self, CcbId, DN),
		       Self ! {delete_callback_reply, Reply},
		       exit(normal)
	       end),
    ok.

delete({wait_for_callback, _, Client, _ReplyFun, {_AO, OpFun, RefFun, []}, _OldState,
	#state{operations=Ops, references=Refs} = NewState}) ->
         %% LATH: Possible problem with augmentation, must the CCB be updated before the callbacks??
    gen_server:reply(Client, ok),
    {noreply, NewState#state{operations = [OpFun |Ops],
		   references = [RefFun |Refs]
		  }};
delete({wait_for_callback, CbType, Client, ReplyFun0, {AO, OpFun, RefFun, [Object | Objs]},
	OldState, NewState}) ->
    #state{id=CcbId, ccb_flags=CcbFlags, %operations=Ops, references=Refs,
	   objects=Objects, implementers=Impls, augments=Augments} = NewState,
    #state{context=OldContext} = OldState,
    ObjectKey = safs_imm_db:get_key(Object),
    DN = dnlist_to_bin(ObjectKey),
    %% Using the old context so the objects from the current
    %% delete operation still exists
    {OiFun, Oi, Appliers} = safs_imm_db:oi_get(DN, OldContext),
    check_oi_reg_flags(DN, ccb_object_delete, CcbFlags, Oi),
    SpecialAppliers = safs_imm_oi:get_special_appliers_if_registered(),
    {ok, ObjectClassName} = safs_imm_db:get_class(DN, OldContext),
    {Category, AttrDefs} = safs_imm_db:get_class_def(ObjectClassName),
    NotifiableAttrs = safs_imm_lib:get_notifiable_attrs(AttrDefs),
    Appliers1 =
	case NotifiableAttrs of
	    [] -> Appliers;
	    _ -> SpecialAppliers ++ Appliers
	end,
    case Category of
	runtime ->
	    delete({wait_for_callback, CbType, Client, ReplyFun0,
		    {AO, OpFun, RefFun, Objs},
		    OldState,
		    NewState#state{objects=[{ObjectKey, ObjectClassName} |Objects]}});
	config ->

	    if
		Oi == undefined orelse (Augments =/= undefined andalso NewState#state.implementer_name == Oi)->
		    %% Callbacks to Appliers
		    [safs_imm_oi:ccb_object_delete(Applier, self(), CcbId, DN)
		     || Applier <- Appliers],

		    %%io:format("A DN: ~p\n", [DN]),
		    %% Callbacks to Special Appliers
		    send_cb_to_special_appliers(NotifiableAttrs, SpecialAppliers,
						AO#safs_imm_om_adm_owner.name,
						{delete, CcbId, DN}, []),
		    delete({wait_for_callback, CbType, Client, ReplyFun0,
			    {AO, OpFun, RefFun, Objs},
			    OldState,
			    NewState#state{objects=[{ObjectKey, ObjectClassName} |Objects],
					   implementers=add_unique({applier, Appliers1}, Impls)}});
		true ->
		    ReplyFun1 =
			fun() ->
				%% Callbacks to Appliers
				[safs_imm_oi:ccb_object_delete(Applier, self(), CcbId, DN)
				 || Applier <- Appliers],
				%%io:format("B DN: ~p\n", [DN]),
				%% Callbacks to Special Appliers
				send_cb_to_special_appliers(NotifiableAttrs, SpecialAppliers,
							    AO#safs_imm_om_adm_owner.name,
							    {delete, CcbId, DN}, []),
				{OiFun, {ObjectKey, ObjectClassName}, [{applier, Appliers1}, Oi]}
			end,
		    async_ccb_object_delete(Oi, CcbId, DN),
		    {noreply, {wait_for_callback, CbType, Client, ReplyFun1, {AO, OpFun, RefFun, Objs},
			       OldState#state{implementers = add_unique(Oi, Impls)}, NewState}}
	    end
    end.

%%---------------------------------------------------------------------------------
% async_ccb_object_read(ObjectName, Attributes, Context) ->
%     Self = self(),
%     spawn_link(fun() ->
% 		       Reply = safs_imm_om:check_rt_attrs(ObjectName, Attributes, Context),
% 		       Self ! {read_callback_reply, Reply},
% 		       exit(normal)
% 	       end),
%     ok.

async_apply_to_implementers(Cb, Impls, Id) when Cb == ccb_apply; Cb == ccb_abort ->
    Self = self(),
    spawn_link(?MODULE, apply_to_implementers_parallel, [Self, Cb, Impls, Id]),
    ok;
async_apply_to_implementers(Cb, Impls, Id) ->
    Self = self(),
    spawn_link(fun() ->
		       Result = apply_to_implementers(Self, Cb, Impls, Id),
		       Self ! {apply_to_impl_callback_reply, Cb, Result},
		       exit(normal)
	       end),
    ok.

%%---------------------------------------------------------------------------------
format_transaction_error_string(Id, {throw, {invalid_no_dangling, not_config_obj, {DN, AttrName, Value}}}) ->
    safs_imm_om:ccb_set_error_string(Id, lists:flatten(io_lib:format("'No dangling' specified attribute '~p' "
								     "in object: '~s' reference to non "
								     "configuration object: '~s'",
								     [AttrName, DN, Value])));
format_transaction_error_string(Id, {throw, {invalid_no_dangling, obj_not_exist, {DN, AttrName, Value}}}) ->
    safs_imm_om:ccb_set_error_string(Id, lists:flatten(io_lib:format("'No Dangling' specified attribute '~p' "
								     "in object: '~s' reference to non existent "
								     "object: '~s'",
								     [AttrName, DN, Value])));
format_transaction_error_string(Id, {throw, {invalid_no_dangling, incoming_reference, {FromDN, ToDn}}}) ->
    safs_imm_om:ccb_set_error_string(Id, lists:flatten(io_lib:format("Object '~s' can't be removed due to a "
								     "'no dangling' specified reference from: "
								     "'~s'",
								     [ToDn, FromDN])));
format_transaction_error_string(_Id, {throw, {object_changed, What, DN}}) ->
    String =
	case What of
	    oi -> "Object Implementer Changed";
	    ao -> "Administrative Owner Changed";
	    oi_and_ao -> "Both Object Implementer and Administrative Owner Changed";
	    attrs -> "Some Attribute changed";
	    obj_add_or_rm -> "One or more object have been added or deleted in the subtree"
	end,
    error_logger:info_msg("~s : ~p\n",[String, f_dn(DN)]);
format_transaction_error_string(_Id, {throw, {class_changed, What, Name}}) ->
    String =
	case What of
	    oi -> "Class Implementer Changed";
	    definition -> "Class Definition Changed"
	end,
    error_logger:info_msg("~s : ~p\n",[String, Name]);
format_transaction_error_string(_Id, _Reason) ->
    ok.

%%---------------------------------------------------------------------------------
f_dn(undefined) ->
    "";
f_dn(DN) ->
    DN.

%%---------------------------------------------------------------------------------
check_allowed_extra_attributes([]) ->
    ok;
check_allowed_extra_attributes([{N, _, _} |Attrs]) when N =:= <<"RcsImmAttrObjId">>;
                                                        N =:= <<"RcsImmAttrEcimDn">> ->
    check_allowed_extra_attributes(Attrs);
check_allowed_extra_attributes([#safsImmAttrValues_2{attrName=N} |Attrs])
  when N =:= <<"RcsImmAttrObjId">>;
       N =:= <<"RcsImmAttrEcimDn">> ->
    check_allowed_extra_attributes(Attrs);
check_allowed_extra_attributes([#safsImmAttrModification_2{modAttr=Attr} |Attrs])
  when Attr#safsImmAttrValues_2.attrName =:= <<"RcsImmAttrObjId">>;
         Attr#safsImmAttrValues_2.attrName =:= <<"RcsImmAttrEcimDn">> ->
    check_allowed_extra_attributes(Attrs);
check_allowed_extra_attributes(_) ->
    not_defined.

%%----------------------------------------------------------------------
%% END OF MODULE
%%----------------------------------------------------------------------
