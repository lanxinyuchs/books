%%% ----------------------------------------------------------
%%% %CCaseFile:	nc_service.erl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(nc_service).
-behaviour(gen_server).
-vsn('/main/R3A/4').
-date('2015-04-14').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
%%% R3A/1      2014-11-20 etxpeno     First version
%%% R3A/3      2014-12-18 etxpeno     Remove call to update_model()
%%% ----------------------------------------------------------

%% API
-export([start/0]).
-export([stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% safe callbacks
-export([ccb_object_create_callback_2/5, ccb_object_modify_callback_2/4,
	 ccb_object_delete_callback/3, ccb_completed_callback/2,
	 ccb_apply_callback/2, ccb_abort_callback/2]).

-include_lib("safe/include/safe.hrl").
-include_lib("safe/include/safe_imm.hrl").

-define(SERVER, ?MODULE).
-define(CLASSES, ["Equipment", "FieldReplaceableUnit", "MpClusterHandling"]).
-define(OBJECTS, ["equipmentId=1", "mpClusterHandlingId=1,nodeSupportId=1"]).

-record(state,
	{
	  oi_handle,
	  ccb_dict = orddict:new(),
	  fru_set = ordsets:new(),
	  primaryCoreRef
	}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %% update_model(),

    ok = start_safe(),

    {FruSet, PrimaryCoreRef} = read_initial_data(),

    OiHandle = initialize_oi(),

    {ok, #state{oi_handle      = OiHandle,
		fru_set        = FruSet,
		primaryCoreRef = PrimaryCoreRef}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({ccb_object_create_callback_2, OiHandle, CcbId, ClassName,
	     ParentName, Attr},
	    _From,
	    #state{oi_handle = OiHandle} = State) ->
    Req = {create, ClassName, ParentName, Attr},
    NewState = handle_change_req(CcbId, Req, State),
    Reply = ok,
    {reply, Reply, NewState};
handle_call({ccb_object_modify_callback_2, OiHandle, CcbId, ObjectName,
	     AttrMods},
	    _From,
	    #state{oi_handle = OiHandle} = State) ->
    Req = {modify, ObjectName, AttrMods},
    NewState = handle_change_req(CcbId, Req, State),
    Reply = ok,
    {reply, Reply, NewState};
handle_call({ccb_object_delete_callback, OiHandle, CcbId, ObjectName},
	    _From,
	    #state{oi_handle = OiHandle} = State) ->
    Req = {delete, ObjectName},
    NewState = handle_change_req(CcbId, Req, State),
    Reply = ok,
    {reply, Reply, NewState};
handle_call({ccb_completed_callback, OiHandle, CcbId},
	    _From,
	    #state{oi_handle = OiHandle} = State) ->
    Reply = ok,
    NewState = handle_completed(CcbId, State),
    {reply, Reply, NewState};
handle_call({ccb_apply_callback, OiHandle, CcbId},
	    _From,
	    #state{oi_handle = OiHandle} = State) ->
    Reply = ok,
    NewState = handle_apply(CcbId, State),
    {reply, Reply, NewState};
handle_call({ccb_abort_callback, OiHandle, CcbId},
	    _From,
	    #state{oi_handle = OiHandle} = State) ->
    Reply = ok,
    NewState = handle_abort(CcbId, State),
    {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ok = finalize_oi(State#state.oi_handle),
    ok = stop_safe().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% safe callbacks
%%%===================================================================
ccb_object_create_callback_2(ImmH, CcbId, ClassName, ParentName, Attr) ->
    call({ccb_object_create_callback_2, ImmH, CcbId, ClassName, ParentName,
	  Attr}).

ccb_object_modify_callback_2(ImmH, CcbId, ObjectName, AttrMods) ->
    call({ccb_object_modify_callback_2, ImmH, CcbId, ObjectName, AttrMods}).

ccb_object_delete_callback(ImmH, CcbId, ObjectName) ->
    call({ccb_object_delete_callback, ImmH, CcbId, ObjectName}).

ccb_completed_callback(ImmH, CcbId) ->
    call({ccb_completed_callback, ImmH, CcbId}).

ccb_apply_callback(ImmH, CcbId) ->
    call({ccb_apply_callback, ImmH, CcbId}).

ccb_abort_callback(ImmH, CcbId) ->
    call({ccb_abort_callback, ImmH, CcbId}).


%%%===================================================================
%%% Internal functions
%%%===================================================================
%% update_model() ->
%%     App = "ugt_nc",
%%     PrivDir = code:priv_dir(fake),
%%     BinDir = filename:join([PrivDir, target_bin_dir()]),
%%     Path =
%% 	case os:getenv("DEV_PATCHES") of
%% 	    false      -> BinDir;
%% 	    DevPatches -> DevPatches ++ ":" ++ BinDir
%% 	end,
%%     case os:find_executable(App, Path) of
%% 	false ->
%% 	    error_logger:error_msg("~p: Couldn't find ~p in ~p~n",
%% 				   [?MODULE, App, Path]),
%% 	    ok;
%% 	GlmsProg ->
%% 	    error_logger:info_msg("~p: Spawning ~p~n", [?MODULE, GlmsProg]),
%% 	    Port = open_port({spawn_executable, GlmsProg},[exit_status]),
%% 	    receive
%% 		{Port, {exit_status, Status}} ->
%% 		    error_logger:info_msg("~p: ~p has exited with status ~p~n",
%% 					  [?MODULE, App, Status])
%% 	    after 1000 ->
%% 		    error_logger:warning_msg("~p: ~p has timed out~n",
%% 					     [?MODULE, App])
%% 	    end,
%% 	    ok
%%     end.

%% target_bin_dir() ->
%%     Ark =
%%         case {architecture(),
%% 	      os:getenv("ARMDIR")} of
%%             {{Arch,_}, false} ->
%%                 Arch;
%%             {{"arm",_}, Dir} ->
%% 		Dir
%%         end,
%%     filename:join(["tgt_"++Ark, "bin"]).

%% architecture() ->
%%     case os:cmd("arch") of
%% 	"i686\n" ->
%% 	    {"i686","lib32"};
%%         "x86_64\n" ->
%%             {"i686","lib32"}; %% Assume 32 bit also on on 64 bit machine (etxarnu)
%%         "armv7l\n" ->
%%             {"arm","lib32"};
%%         Arch ->
%%             erlang:error({unknown, Arch}, [])
%%     end.

start_safe() ->
    ok = application:set_env(safe, services, [imm]),
    ok = application:ensure_started(safe).

stop_safe() ->
    ok = application:stop(safe).

read_initial_data() ->
    {ok, OmHandle, _} = safe_imm_om:initialize(?MODULE),

    FruSet = read_fru_set(OmHandle),
    PrimaryCoreRef = read_primary_core_ref(OmHandle),

    ok = safe_imm_om:finalize(OmHandle),

    {FruSet, PrimaryCoreRef}.

read_fru_set(OmHandle) ->
    {ok, Handle} = safe_imm_om:search_initialize_2(OmHandle,
						   "equipmentId=1",
						   ?SAFE_IMM_SUBLEVEL,
						   ?SAFE_IMM_SEARCH_GET_NO_ATTR,
						   undefined,
						   undefined),
    FruSet = read_fru_set(Handle, ordsets:new()),
    ok = safe_imm_om:search_finalize(Handle),
    FruSet.

read_fru_set(Handle, FruSet) ->
    case safe_imm_om:search_next_2(Handle) of
	{ok, ObjName, []} ->
	    case get_fruId(ObjName) of
		undefined ->
		    read_fru_set(Handle, FruSet);
		FruId ->
		    NewFruSet = ordsets:add_element(FruId, FruSet),
		    read_fru_set(Handle, NewFruSet)
	    end;
	{error, ?SAFE_AIS_ERR_NOT_EXIST} ->
	    FruSet
    end.

read_primary_core_ref(OmHandle) ->
    {ok, Ao} = safe_imm_om:accessor_initialize(OmHandle),
    {ok, [#safe_imm_attr_values_2{attr_name = "primaryCoreRef",
				  attr_value_type = ?SAFE_IMM_ATTR_NAME,
				  attr_values = MoRefList}]} =
	safe_imm_om:accessor_get_2(Ao,
				   "mpClusterHandlingId=1,nodeSupportId=1",
				   ["primaryCoreRef"]),
    ok = safe_imm_om:accessor_finalize(Ao),

    get_primary_core_ref(MoRefList).

get_primary_core_ref([]) -> undefined;
get_primary_core_ref([MoRef]) -> get_fruId(MoRef).

initialize_oi() ->
    {ok, Handle, _} = safe_imm_oi:initialize_2(?MODULE),
    ok = safe_imm_oi:implementer_set(Handle, ?MODULE_STRING),

    lists:foreach(
      fun(Class) ->
	      ok = safe_imm_oi:class_implementer_set(Handle, Class)
      end, ?CLASSES),

    lists:foreach(
      fun(Obj) ->
	      ok = safe_imm_oi:object_implementer_set(Handle, Obj,
						      ?SAFE_IMM_SUBTREE)
      end, ?OBJECTS),

    Handle.

finalize_oi(Handle) ->
    lists:foreach(
      fun(Obj) ->
	      ok = safe_imm_oi:object_implementer_release(Handle, Obj,
							  ?SAFE_IMM_SUBTREE)
      end, ?OBJECTS),

    lists:foreach(
      fun(Class) ->
	      ok = safe_imm_oi:class_implementer_release(Handle, Class)
      end, ?CLASSES),

    ok = safe_imm_oi:implementer_clear(Handle),
    ok = safe_imm_oi:finalize(Handle).

call(Msg) ->
    gen_server:call(?SERVER, Msg).

handle_change_req(CcbId, Req, State) ->
    CcbDict = State#state.ccb_dict,
    NewCcbDict = orddict:append(CcbId, Req, CcbDict),
    State#state{ccb_dict = NewCcbDict}.

handle_completed(_CcbId, State) ->
    State.

handle_apply(CcbId, State) ->
    CcbDict = State#state.ccb_dict,
    ReqList =
	case orddict:find(CcbId, CcbDict) of
	    {ok, Val} ->
		Val;
	    error ->
		[]
	end,
    State1 = handle_req_list(ReqList, State),
    NewCcbDict = orddict:erase(CcbId, CcbDict),
    State1#state{ccb_dict = NewCcbDict}.

handle_abort(CcbId, State) ->
    CcbDict = State#state.ccb_dict,
    NewCcbDict = orddict:erase(CcbId, CcbDict),
    State#state{ccb_dict = NewCcbDict}.

handle_req_list(ReqList, State) ->
    lists:foldl(fun handle_req/2, State, ReqList).

handle_req({modify, "fieldReplaceableUnitId=" ++ _, _}, State) ->
    %% No modification of attributes in fieldReplaceableUnitId is important
    State;
handle_req({modify, "mpClusterHandlingId=1,nodeSupportId=1",
	    [#safe_imm_attr_modification_2{mod_type = ?SAFE_IMM_ATTR_VALUES_REPLACE,
					   mod_attr = #safe_imm_attr_values_2{attr_name = "primaryCoreRef",
									      attr_value_type = ?SAFE_IMM_ATTR_NAME,

									      attr_values = MoRefList}
					  }
	    ]
	   }, State) ->

    PrimaryCoreRef = get_primary_core_ref(MoRefList),
    State#state{primaryCoreRef = PrimaryCoreRef};
handle_req({delete, ObjName}, State) ->
    case get_fruId(ObjName) of
	undefined ->
	    State;
	FruId ->
	    NewFruSet = ordsets:del_element(FruId, State#state.fru_set),
	    State#state{fru_set = NewFruSet}
    end;
handle_req({create, "FieldReplaceableUnit", "equipmentId=1",
	    [#safe_imm_attr_values_2{attr_name = "fieldReplaceableUnitId",
				     attr_value_type = ?SAFE_IMM_ATTR_STRING,
				     attr_values = [FruId]}
	    ]
	   }, State) ->
    NewFruSet = ordsets:add_element(FruId, State#state.fru_set),
    State#state{fru_set = NewFruSet};
handle_req(Req, State) ->
    error_logger:error_msg("~p: Unknown request: ~p~n", [?MODULE, Req]),
    State.

get_fruId("fieldReplaceableUnitId=" ++ Rest) ->
    lists:takewhile(fun(C) -> C /= $, end, Rest);
get_fruId(_) ->
    undefined.
