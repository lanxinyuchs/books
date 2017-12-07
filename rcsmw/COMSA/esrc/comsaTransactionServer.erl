%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaTransactionServer.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/3
%%%
%%% @doc == COMSA Transaction server ==
%%% Specialised transaction server for COMTE used for IMM transactions in
%%% combination with comsa native application

-module(comsaTransactionServer).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/3').
-date('2017-10-17').
-author('etxpeno').

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
%%% -----      -------    --------    ------------------------
%% R3A/    ---------- -------  ------------------------------------------------
%%% 1      2014-11-20 etxberb  * createMo now includes attribute settings in the
%%%                              callbacks instead of separating them into an
%%%                              explicit setMoAttributes callback.
%%%                            * Added calls to sysUtil:timediff_*.
%%% 3      2014-11-24 etxberb  Backed out previous createMo change.
%%% 4      2014-11-26 etxpeno  Corrections in getMoAttributes area
%%% R3A/5  2014-12-01 etxpeno  Backed out changes between R3A/0-R3A/3
%%% R3A/6  2014-12-09 etxberb  * createMo now includes attribute settings in the
%%%                              callbacks instead of separating them into an
%%%                              explicit setMoAttributes callback.
%%%                            * Added calls to sysUtil:timediff_*.
%%% R3A/7  2014-12-18 etxpeno  Backed out attribute settings in createMo
%%% R3A/8  2014-12-18 etxpeno  Reverted to R3A/5
%%% R3A/10 2014-12-29 etxberb  Re-Added calls to sysUtil:timediff_*.
%%% R3A/12 2015-03-10 etxpeno  Correction of handling of deleted MOs
%%% R3A/13 2015-05-07 etxpeno  Correction in count_mo_children()
%%% -----  ---------- -------  ------------------------------------------------
%%% R4A/1  2015-06-01 etxpejn  Corr HT77481, LMA will to the validation part
%%% R4A/4  2015-09-08 etxjotj  HU15813 New COMTE API for getMoAttributes
%%% R4A/5  2015-09-09 etxpeno  add logging of action request
%%% R4A/6  2015-10-16 eolaand  Add mirroring of deprecated OAP attributes
%%%                            for HU25873.
%%% R4A/7  2015-11-05 etxpeno  HU33336 Remove logging of potential information
%%%                            in actions
%%% -----  ---------- -------  ------------------------------------------------
%%% R5A/1  2015-11-10 etxpeno  replace dict() with map()
%%% R5A/3  2016-04-04 uabesvi  HU68290
%%% R5A/4  2016-04-14 eolaand  HU73664, ccb_completed moved outside of Tx
%%% -----  ---------- -------  ------------------------------------------------
%%% R7A/1  2016-10-06 etxpeno  HV31163 Improve error cause in action
%%%                            Add more logging at join() and finish()
%%% -----  ---------- -------  ------------------------------------------------
%%% R8A/1  2016-11-22 etxpeno  Translate all errors to use the correct managedElementId
%%% -----  ---------- -------  ------------------------------------------------
%%% R10A/1 2017-05-05 etxberb  Added stop_configuration/1.
%%% R10A/2 2017-05-08 etxberb  Added start_configuration/0.
%%% -----  ---------- -------  ------------------------------------------------
%%% R11A/1 2017-08-15 etxpeno  Make the finish() callback optional
%%% R11A/2 2017-09-04 etxjotj  Replaced swmDbServer with swmI
%%% R11A/3 2017-10-17 etxpeno  OTP 20 fixes and Async logging
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/1]).
%% Transaction exports
-export([join/1]).
-export([validate/1]).
-export([prepare/1]).
-export([commit/1]).
-export([abort_transaction/1]).
-export([finish/1]).
%% Specifics for COI interface
-export([reserve/1]).
-export([select_count/1, select_count/2]).
%% MO exports
-export([setMoAttribute/4]).
-export([setMoAttributes/3]).
-export([getMoAttribute/3]).
-export([getMoAttributes/3]).
-export([getMoIterator/3]).
-export([createMo/5]). % deprecated
-export([createMo/6]).
-export([deleteMo/2]).
-export([existsMo/2]).
-export([countMoChildren/3]).
-export([action/4]).
%% Upgrade by SWM
-export([start_configuration/0, stop_configuration/1]).
-export([is_config_enabled/0]).

%%% Debug functions
-export([processes/0, process_info/0, report_state/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include("comTypes.hrl").
-include("alhI.hrl").

-type reply() :: term().
-type reason() :: normal | shutdown | term().

-record(prepared, {funs = [] :: list(),
		   result = ok :: term(),
		   nonGmfFunsDnKeys = [] :: list()
		  }).

-record(state, {trans_id,
		user_objects :: map(),
		create_cache :: [{term(),{term(),term()}}],
		delete_cache :: [{term(),term()}],
		delete_history = [] :: list(),
		set_cache :: map(),
		tState :: initialized | collecting | action | commited |
			  prepared | aborting | aborted | finished,
		imm_data :: term(),
		request_q = [] :: list(),
		transaction_callbacks = [] :: list(),
		prepared = #prepared{} :: #prepared{},
		transactDisabled = false :: boolean()
	       }).

-record(comsaTransactDisabled, {key,
				reason :: binary()}).

-define(STACKTRACE_C,   % Current stacktrace
        element(2, process_info(self(), current_stacktrace))).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%% @doc
%% The callback is invoked by ComtE during startup.
%% Initializes the data callback table
%% @end
start(_Opts) ->
    ok.

%%%===================================================================
%%% Transaction callbacks and interface used by COI
%%%===================================================================
%% @doc reserve interface function used by COI.
-spec reserve(TransId :: term()) -> boolean().
reserve(TransId) ->
    mapping_reserve(TransId).

%% @doc select_count interface function used by COI.
select_count(TransIdPattern) ->
    select_count(TransIdPattern, []).

%% Example 1: select_count({coi, '_', '_'}).
%% Example 2: select_count('$1', [{is_number, '$1'}]).
select_count(TransIdPattern, Conditions) when is_list(Conditions) ->
    ets:select_count(comsaTransactionMapping,
		     [{{TransIdPattern, '_'}, Conditions, [true]}]).

%% @doc Starts the server
join(TransId) ->
    Req = "join",
    log(in, Req, [TransId]),
    R = case mapping_join(TransId) of
	    true ->
		Module = ?MODULE,
		InitArg = [TransId, self()],
		Options = [],
%%%	    Options = [{spawn_opt, [{min_heap_size, 100000}]}],
		OrigTrapExit = process_flag(trap_exit, true),
		case gen_server:start_link(Module, InitArg, Options) of
		    {ok, Pid} ->
			true = mapping_complete(TransId, Pid),
			process_flag(trap_exit, OrigTrapExit),
			ok;
		    Error ->
			Result =
			    <<"Could not create transaction, probably COM "
			      "crashed without cleaning up everything in comte.">>,
			sysInitI:error_report([Result, {gen_server, Error}] ++
						  ?STACKTRACE_C),
			mapping_delete(TransId, reserved),
			flush_exit(Error),
			process_flag(trap_exit, OrigTrapExit),
			{error, Result}
		end;
	    false ->
		Result = <<"Transaction already exist.">>,
		sysInitI:error_report([Result] ++ ?STACKTRACE_C),
		{error, Result}
	end,
    log(out, Req, [TransId, R]),
    R.

%% @doc Validate callback from COM
validate(TransId) ->
    Req = "validate",
    log(in, Req, [TransId]),
    R = call(TransId, {validate, TransId}),
    log(out, Req, [TransId, R]),
    R.

%% @doc Prepare callback from COM
prepare(TransId) ->
    Req = "prepare",
    log(in, Req, [TransId]),
    R = call(TransId, {prepare, TransId}),
    log(out, Req, [TransId, R]),
    R.

%% @doc Prepare callback from COM
commit(TransId) ->
    Req = "commit",
    log(in, Req, [TransId]),
    R = call(TransId, {commit, TransId}),
    log(out, Req, [TransId, R]),
    R.

%% @doc abort_transaction callback from COM
abort_transaction(TransId) ->
    call(TransId, {abort_transaction,TransId}).

%% @doc finish callback from COM
finish(TransId) ->
    Req = "finish",
    log(in, Req, [TransId]),
    R = call(TransId, {finish,TransId}),
    log(out, Req, [TransId, R]),
    R.

%% MO Operations

%% @doc Set MO Attribute
setMoAttribute(TransId, Dn, AttrName, AttrValue) ->
    Req = "setMoAttribute",
    log(in, Req, [TransId, Dn, AttrName, AttrValue]),
    R = call(TransId, SA = {setMoAttribute, TransId, Dn, AttrName, AttrValue}),
    log(out, Req, [TransId, R]),
    mirror_deprecated(R, SA).

setMoAttributes(TransId, Dn, NamedAttrs) ->
    Req = "setMoAttributes",
    log(in, Req, [TransId, Dn, NamedAttrs]),
    R = call(TransId, SA = {setMoAttributes, TransId, Dn, NamedAttrs}),
    log(out, Req, [TransId, R]),
    mirror_deprecated(R, SA).

%% @doc Get MO Attribute
getMoAttribute(TransId, Dn, AttrName) ->
    call(TransId, {getMoAttribute,TransId, Dn, AttrName}).

getMoAttributes(TransId, Dn, AttrNames) ->
    call(TransId, {getMoAttributes, TransId, Dn, AttrNames}).

%% @doc Next MO (MO Iterator)
getMoIterator(TransId, Dn, ClassName) ->
    call(TransId, {getMoIterator,TransId, Dn, ClassName}).

%% @doc Create MO
createMo(TransId, ParentDn, ClassName, KeyAttrName, KeyValue) ->
    Req = "createMo",
    log(in, Req, [TransId, ParentDn, ClassName, KeyAttrName, KeyValue]),
    R = call(TransId, {createMo,TransId, ParentDn, ClassName,
		   KeyAttrName, KeyValue, []}),
    log(out, Req, [TransId, R]),
    R.
createMo(TransId, ParentDn, ClassName, KeyAttrName, KeyValue, InitAttr) ->
    Req = "createMo",
    log(in, Req,
	[TransId, ParentDn, ClassName, KeyAttrName, KeyValue, InitAttr]),
    R = call(TransId, {createMo, TransId, ParentDn, ClassName,
		   KeyAttrName, KeyValue, InitAttr}),
    log(out, Req, [TransId, R]),
    R.

%% @doc Delete MO
deleteMo(TransId, Dn) ->
    Req = "deleteMo",
    log(in, Req, [TransId, Dn]),
    R = call(TransId, {deleteMo,TransId, Dn}),
    log(out, Req, [TransId, R]),
    R.

existsMo(TransId, Dn) ->
    call(TransId, {existsMo,TransId, Dn}).

countMoChildren(TransId, Dn, ClassName) ->
    call(TransId, {countMoChildren,TransId, Dn, ClassName}).

%% @doc Action
action(TransId, Dn, Name, Params) ->
    Req = "action",
    log(in, Req, [TransId, Dn, Name, filter_sensitive(Params)]),
    R = call(TransId, {action, TransId, Dn, Name, Params}),
    log(out, Req, [TransId, R]),
    R.

%% @doc Enable the operator to be able to change the configuration
-spec start_configuration() ->
    ok.
start_configuration() ->
    try
	ets:delete(comsaTransactDisabled)
    catch
	_ : _ ->
	    ok
    end,
    timer:sleep(50),   % Let any simultaneously spawned server processes become
						% registered so we can find them
						% in the next step below.
    [Pid ! start_configuration
     || {_Tid, Pid, true} <- comsaTransactionServer:processes()],
    ok.

%% @doc Disable the operator from changing the configuration
-spec stop_configuration(Reason :: string()) ->
    ok.
stop_configuration(Reason) ->
    try
	Opts = [named_table,
		public,
		{keypos, 2},
		ordered_set,
		{heir, whereis(comsaServer), undefined}],
	ets:new(comsaTransactDisabled, Opts)
    catch
	_ : _ ->
	    ok
    end,
    CompleteReason = list_to_binary("Changes are not allowed. " ++ Reason),
    ets:insert(comsaTransactDisabled,
	       #comsaTransactDisabled{key = true,
				      reason = CompleteReason}),
    [Pid ! stop_configuration
     || {_Tid, Pid, true} <- comsaTransactionServer:processes()],
    ok.

%%% #---------------------------------------------------------
is_config_enabled() ->
    undefined == ets:info(comsaTransactDisabled).


%%% #---------------------------------------------------------
filter_sensitive(L) ->
    lists:map(
      fun({Name, {T, _}}) ->
	      {Name, {T, hidden}};
	 (P) ->
	      P
      end, L).

%%% #---------------------------------------------------------
processes() ->
    lists:map(fun({TransId, {Tag, _Pid}}) ->
		      {TransId, Tag, false};
		 ({TransId, Pid}) ->
		      {TransId, Pid, is_process_alive(Pid)}
	      end,
	      ets:tab2list(comsaTransactionMapping)).

%%% #---------------------------------------------------------
process_info() ->
    lists:map(fun({TransId, {Tag, _Pid}}) ->
		      {TransId, Tag, undefined};
		 ({TransId, Pid}) ->
		      {TransId, Pid, process_info(Pid)}
	      end,
	      ets:tab2list(comsaTransactionMapping)).

report_state(Pid) when is_pid(Pid) ->
    Pid ! report_state;
report_state(TransId) ->
    case get_pid(TransId) of
	Pid when is_pid(Pid) ->
	    Pid ! report_state,
	    ok;
	Other ->
	    Other
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the server
%%--------------------------------------------------------------------
-spec init(Args :: list()) -> {ok, State :: #state{}}.
init([TransId, Pid]) ->
    erlang:monitor(process, Pid),
    process_flag(trap_exit, true),
    sysUtil:timediff_start(),
    Before = sysUtil:timediff_before(init_imm),
    ImmData = init_imm(TransId, comsaLib:get_variable(imm_global_callback)),
    sysUtil:timediff_after_and_previous(Before),
    put(transaction_callbacks, []),
    put(internal_software_error, false),
    {ok, #state{trans_id     = TransId,
		user_objects = #{},
		create_cache = [],
		delete_cache = [],
		set_cache    = #{},
		tState       = initialized,
		imm_data     = ImmData,
		transactDisabled = not is_config_enabled()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling call messages
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: term(), State :: #state{} ) ->
			 {reply, reply(), #state{}} |
			 {stop, reason(), reply(), #state{}}.

%% Make sure to update state so that an action cannot be performed
%% in the same transaction as any modification call.
handle_call(Request, From, S=#state{tState = initialized})
  when element(1, Request) == setMoAttribute;
       element(1, Request) == setMoAttributes;
       element(1, Request) == createMo;
       element(1, Request) == deleteMo ->
    handle_call(Request, From, S#state{tState = collecting});
handle_call(Request, From, S=#state{tState = prepared})
  when element(1, Request) == setMoAttribute;
       element(1, Request) == setMoAttributes;
       element(1, Request) == createMo;
       element(1, Request) == deleteMo ->
    handle_call(Request, From, reset_prepared(S));
handle_call(Request, _F, S=#state{tState=TState})
  when (element(1, Request) == setMoAttribute orelse
        element(1, Request) == setMoAttributes orelse
	element(1, Request) == createMo orelse
	element(1, Request) == deleteMo) andalso TState /= collecting ->
    Error =
	<<"Not allowed to mix set/create/delete with action in the same "
	"transaction!">>,
    {reply, {error, Error}, S};

handle_call(Request, _F, #state{transactDisabled = true} = State)
  when element(1, Request) == action orelse
       element(1, Request) == createMo orelse
       element(1, Request) == deleteMo orelse
       element(1, Request) == prepare orelse
       element(1, Request) == setMoAttribute orelse
       element(1, Request) == setMoAttributes ->
    Result = {error, transactDisabled_reason()},
    {reply, Result, State};

handle_call(Request, From, S=#state{tState=initialized})
  when element(1, Request) == action ->
    handle_call(Request, From, S#state{tState=action});
handle_call(Request, _F, S=#state{tState=TState})
  when element(1, Request) == action andalso TState /= action ->
    Error =
	<<"Not allowed to mix action with set/create/delete in the same "
	"transaction!">>,
    {reply, {error, Error}, S};

handle_call({getMoAttribute, TransId, Dn, AttrName}=Msg, _F, State) ->
    Before = sysUtil:timediff_before({getMoAttribute, AttrName}),
    Fun = fun(S) ->
		  DnKey = ecim_to_comte_key(Dn,""),
		  case get_cache_attr(DnKey, AttrName, S#state.set_cache,
				      S#state.create_cache,
				      S#state.delete_cache) of
		      undefined ->
			  {do_getMoAttribute([AttrName|DnKey], TransId),
			   S};
		      {Val, _OldValue} ->
			  {Val,S}
		  end
	  end,
    {R,S} = dirty(Fun, State, Msg),
    sysUtil:timediff_after_and_previous(Before),
    {reply, R, S};

handle_call({getMoAttributes, TransId, Dn, AttrNames} = Msg, _F, State) ->
    Before = sysUtil:timediff_before({getMoAttributes, AttrNames}),
    Fun = fun(S) ->
		  DnKey = ecim_to_comte_key(Dn,""),
		  case part_cache_get_attrs(DnKey, AttrNames,
					    State#state.set_cache) of
		      {Cache, []} ->
			  %% All in cache
			  {Cache, S};
		      {Cache,GetAttrs} ->
			  %% Some or all in storage
			  Fetched = do_getMoAttributes(GetAttrs, TransId),
			  NewAttrs = replace_attrs(Cache, Fetched),
			  {NewAttrs, S}
		  end
	  end,
    {R,S} = dirty(Fun, State, Msg),
    sysUtil:timediff_after_and_previous(Before),
    {reply, R, S};

handle_call({getMoIterator, TransId, Dn, ClassName}=Msg, _F, State) ->
    Before = sysUtil:timediff_before({getMoIterator, ClassName}),
    Fun = fun(S) ->
		  IsDnDeleted =
		      case ecim_to_comte_key(Dn,"") of
			  [] -> false;
			  [DnId|DnKey] ->
			      exists_in_delete_cache(DnKey, DnId,
						     S#state.delete_cache)
		      end,

		  case IsDnDeleted of
		      true ->
			  {[], S};
		      false ->
			  Key = ecim_to_comte_key(Dn,ClassName),
			  Keys = do_get_all_keys(Key, TransId),
			  CKeys = [?STRING(MOKey) ||
				      {_MO, {_KeyName, MOKey}} <-
					  proplists:lookup_all(
					    Key, S#state.create_cache)],
			  DKeys = [?STRING(MOKey) ||
				      {_, MOKey} <- proplists:lookup_all(
						      Key, S#state.delete_cache)],
			  {lists:sort((Keys ++ CKeys) -- DKeys),S}
		  end
	  end,
    {R,S} = dirty(Fun, State, Msg),
    sysUtil:timediff_after_and_previous(Before),
    {reply, R, S};

handle_call({setMoAttribute, TransId, Dn, AttrName, [{Tag,Value}]},
	    From, State) ->
    handle_call({setMoAttribute, TransId, Dn, AttrName, {Tag,Value}},
		From, State);
handle_call({setMoAttribute, TransId, Dn, AttrName, Value}=Msg, _F, State) ->
    Before = sysUtil:timediff_before({setMoAttribute, AttrName}),
    Fun =
	fun(S) ->
		DnKey = ecim_to_comte_key(Dn, ""),
		OrigVal =
		    case get_cache_attr(DnKey, AttrName, S#state.set_cache,
					S#state.create_cache,
					S#state.delete_cache) of
			undefined ->
			    do_getMoAttribute([AttrName|DnKey], TransId);
			{_, Old} ->
			    Old
		    end,
		AttrVals = [ {AttrName, {Value, OrigVal}} ],
		NewSetCache = set_cache_attr_values(DnKey, AttrVals,
						    S#state.set_cache),
		New_requestQ = remove_duplicates(Msg, S#state.request_q),
		{ok,S#state{set_cache = NewSetCache,
			    request_q = New_requestQ ++ [Msg]}}
	end,
    {R,S} = dirty(Fun, State, Msg),
    sysUtil:timediff_after_and_previous(Before),
    {reply, R, S};

handle_call({setMoAttributes, TransId, Dn, NamedAttrs} = Msg, _F, State) ->
    Before =
	sysUtil:timediff_before({setMoAttributes, Dn, cnt_attrs(NamedAttrs)}),
    Fun =
	fun(S) ->
		DnKey = ecim_to_comte_key(Dn, ""),
		NewNamedAttrs =
		    case part_cache_set_attrs(DnKey, NamedAttrs,
					      State#state.set_cache) of
			{Cache, []} ->
			    %% All in cache
			    Cache;
			{Cache,GetAttrs} ->
			    %% Some or all in mnesia
			    Fetched = do_getMoAttributes(GetAttrs, TransId),
			    %% Replace 'get' in Cache
			    replace_old_attrs(Cache, Fetched)
		    end,

		%% Store values
		NewSetCache = set_cache_attr_values(DnKey, NewNamedAttrs,
						    State#state.set_cache),
		New_requestQ = remove_duplicates(Msg, S#state.request_q),
		{ok, S#state{set_cache = NewSetCache,
			     request_q = New_requestQ ++ [Msg]}}

	end,
    {R,S} = dirty(Fun, State, Msg),
    sysUtil:timediff_after_and_previous(Before),
    {reply, R, S};

handle_call({createMo, TransId, ParentDn, ClassName,
	     KeyAttrName, KeyValue, InitAttrs}=Msg, _F, State) ->
    Before =
	sysUtil:timediff_before({createMo, ClassName, cnt_attrs(InitAttrs)}),
    Fun =
	fun(S) ->
		Key = ecim_to_comte_key(ParentDn,ClassName),

		NewDeleteCache = lists:delete({Key, KeyValue},
					      S#state.delete_cache),
		{NewCreateCache, NewSetCache}
		    = case do_existsMo([KeyValue|Key], TransId) of
			  %% Check if there is an element in the database and
			  %% a delete in the cache.
			  true when NewDeleteCache /= S#state.delete_cache ->
			      %% Do not modify the create cache.
			      %% But the initialAttributes should be
			      %% added to the set cache, otherwise a
			      %% get will not reply with the correct value(s)
			      %%
			      AttrSetCache = init_attr_store([KeyValue | Key],
							     InitAttrs,
							     S#state.set_cache),

			      {State#state.create_cache, AttrSetCache};
			  _Else ->
			      MergeInitAttrs =
				  [{KeyAttrName, {?STRING(KeyValue), create}}
				   | InitAttrs],
			      AttrSetCache = init_attr_store([KeyValue | Key],
							     MergeInitAttrs,
							     S#state.set_cache),
			      {[{Key, {KeyAttrName, KeyValue}} |
				S#state.create_cache],
			       AttrSetCache}
		      end,
		{ok, S#state{set_cache = NewSetCache,
			     delete_cache = NewDeleteCache,
			     create_cache = NewCreateCache,
			     request_q = S#state.request_q ++ [Msg]}}
	end,
    {R, S} = dirty(Fun, State, Msg),
    sysUtil:timediff_after_and_previous(Before),
    {reply, R, S};

handle_call({deleteMo, _TransId, Dn}=Msg, _F, State) ->
    Before = sysUtil:timediff_before({deleteMo, Dn}),
    Fun =
	fun(S) ->
		[Id|Key] = DnKey = ecim_to_comte_key(Dn,""),

		%% Delete from set cache
		NewSetCache =
		    maps:filter(fun(DNKey, _) ->
					not lists:suffix(DnKey, DNKey)
				end, S#state.set_cache),

		%% Delete from create cache
		NewCreateCache =
		    lists:filter(
		      fun({DNKey, {_KeyAttrName, KeyVal}}) ->
			      not lists:suffix(DnKey, [KeyVal|DNKey])
		      end, S#state.create_cache),

		%% Delete from create cache and add this request
		FilteredDeleteCache =
		    lists:filter(
		      fun({DNKey, KeyVal}) ->
			      not lists:suffix(DnKey, [KeyVal|DNKey])
		      end, S#state.delete_cache),

		NewDeleteCache = [{Key, Id} | FilteredDeleteCache],

		NewDeleteHistory =
		    [{DnKey, get_setCache_newValues(DnKey,
						    State#state.set_cache)} |
		     State#state.delete_history],
		New_requestQ = remove_obsolete(Msg, S#state.request_q),
		{ok, S#state{set_cache = NewSetCache,
			     create_cache = NewCreateCache,
			     delete_cache = NewDeleteCache,
			     delete_history = NewDeleteHistory,
			     request_q = New_requestQ}}
	end,
    {R,S} = dirty(Fun, State, Msg),
    sysUtil:timediff_after_and_previous(Before),
    {reply, R, S};

handle_call({existsMo, TransId, Dn} = Msg, _F, State) ->
    %% Exists in the cache or committed
    Before = sysUtil:timediff_before({existsMo, Dn}),
    Fun =
	fun(S) ->
		[Id|Key] = ecim_to_comte_key(Dn,""),
		%% Check cache
		case {exists_in_create_cache(Key, Id, S#state.create_cache),
		      exists_in_delete_cache(Key, Id, S#state.delete_cache)} of
		    {true, _} ->
			%% Exists in create cache
			{true, S};
		    {false, true} ->
			%% Exists in delete cache, this should reply
			%% with false otherwise the create operation
			%% will abort.
			{false, S};
		    {false, false} ->
			%% Check committed
			Exists = do_existsMo([Id|Key], TransId),
			{Exists, S}
		end
	end,
    {R,S} = dirty(Fun, State, Msg),
    sysUtil:timediff_after_and_previous(Before),
    {reply, R, S};


handle_call({countMoChildren, TransId, Dn, ClassName} = Msg, _F, State) ->
    Before = sysUtil:timediff_before({countMoChildren, Dn}),
    Fun =
	fun(S) ->
		Key = ecim_to_comte_key(Dn, ""),
		{_R,S} = count_mo_children(TransId, Key, ClassName, State)
	end,
    {R, S} = dirty(Fun, State, Msg),
    sysUtil:timediff_after_and_previous(Before),
    {reply, R, S};

handle_call({validate, _} = Msg, _F, #state{tState = collecting} = State) ->
    Before = sysUtil:timediff_before(validate),
    {Result, NewState} = do_prepare_and_replay(Msg, State),
    sysUtil:timediff_after_and_previous(Before),
    {reply, format_validate_result(Result), NewState};

handle_call({validate, _}, _F, #state{tState = TState,
				      prepared = #prepared{result = Result}
				     } = State)
  when TState == prepared orelse
       TState == initialized ->
    {reply, format_validate_result(Result), State};

handle_call({validate, _}, _F, #state{tState = action} = State) ->
    {reply, ok, State};

handle_call({prepare, _} = Msg, _F, State) ->
    Before = sysUtil:timediff_before(prepare),
    Before1 = sysUtil:timediff_before({prepare, replay_mnesia_transaction}),
    {Result, NewState} = do_prepare_and_replay(Msg, State),
    sysUtil:timediff_after(Before1),
    Before2 = sysUtil:timediff_before({prepare, dump_log}),
    dump_log(),
    sysUtil:timediff_after(Before2),
    Before3 =
	sysUtil:timediff_before({prepare, {apply_imm,
					   element(1,
						   NewState#state.imm_data)}}),
    internal_apply(NewState#state.trans_id, NewState#state.imm_data),
    sysUtil:timediff_after(Before3),
    sysUtil:timediff_after_and_previous(Before),
    {reply, Result, NewState};

handle_call({commit, _TransId} = _Msg, _F, State = #state{tState = prepared}) ->
    Before = sysUtil:timediff_before(commit),
    {Result, NewState} = inform_transactionCommited(State),
    sysUtil:timediff_after_and_previous(Before),
    {reply, Result, NewState#state{tState = commited}};

handle_call({abort_transaction, _TransId}, _F, State) ->
    Before = sysUtil:timediff_before(abort_transaction),
    {Result, NewState} = inform_abortTransaction(State),
    sysUtil:timediff_after_and_previous(Before),
    {reply, Result, NewState#state{tState = aborting}};

handle_call({finish, TransId} = Msg,
	    _F,
	    State = #state{tState = commited,
			   imm_data = ImmData}) ->
    Before = sysUtil:timediff_before(finish),
    try
	avli_NodeIdChanged_event(State),
	internal_finish(TransId, ImmData),
	maps:map(fun(DNKey, UserObject) ->
			 do_finish(DNKey, UserObject, TransId)
		 end, State#state.user_objects),
	swmI:quarantine_reset(),
	sysUtil:timediff_after_and_previous(Before),
	{stop, normal, ok, State#state{tState = finished}}
    catch throw:{?MODULE, Reason} ->
	    sysUtil:timediff_after_and_previous(Before),
	    report_error(Msg,State,Reason),
	    {stop, normal, {error,format_reason(Reason)},
	     State#state{tState = finished}};
	  E:Reason ->
	    sysUtil:timediff_after_and_previous(Before),
	    report_error(Msg,State,{E,Reason}),
	    {stop, normal, {error,format_reason({E,Reason})},
	     State#state{tState = finished}}
    end;
handle_call({finish, TransId}, _F, #state{imm_data=ImmData}=State) ->
    Before = sysUtil:timediff_before(finish),
    internal_finish(TransId, ImmData),
    sysUtil:timediff_after_and_previous(Before),
    {stop, normal, ok, State#state{tState = finished}};

handle_call({action, TransId, Dn, Name, NamedParams}=Msg, _F, State) ->
    try
	DNKey = ecim_to_comte_key(Dn,Name),
	{reply, do_action(DNKey, NamedParams, TransId), State}
    catch throw:{?MODULE, Reason} ->
	    report_error(Msg,State,Reason),
	    Error = {error, Reason},
	    ImmData = State#state.imm_data,
	    ConvertedError = convert_result(Error, TransId, ImmData),
	    {reply, ConvertedError, State};
	  E:Reason ->
	    report_error(Msg,State,{E,Reason}),
	    {reply, {error,format_reason({E,Reason})}, State}
    end;

handle_call(_Request, _F, State) ->
    sysInitI:warning_report([{?MODULE, handle_call},
				 unexpected_request_or_state,
				 {request, _Request}] ++ report_format(State)),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
%%--------------------------------------------------------------------
-spec handle_cast(Msg :: term(), State :: #state{}) ->
			 {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: #state{}) ->
			 {noreply, #state{}} |
			 {stop, reason(), #state{}}.
handle_info(start_configuration, State) ->
    {noreply, State#state{transactDisabled = false}};
handle_info(stop_configuration, State) ->
    {noreply, State#state{transactDisabled = true}};
handle_info({'DOWN',_, process, _, Reason}, State) ->
    {stop, Reason, State};
handle_info({'EXIT', _From, Reason}, State) ->
    {stop, Reason, State};
handle_info(report_state, State) ->
    sysInitI:info_report(report_format(State)),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate.
%%--------------------------------------------------------------------
-spec terminate(Reason :: reason(), State :: #state{}) -> ok.
terminate(_Reason, State) ->
    TransId = State#state.trans_id,
    if
	State#state.tState =:= finished ->
	    ok;
	true ->
	    internal_finish(TransId, State#state.imm_data)
    end,
    mapping_delete(TransId, self()),
    sysUtil:timediff_stop(),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Convert process state when code is changed
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: {down,term()} | term(),
		  State :: #state{},
		  Extra :: term()) ->
			 {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%--------------------------------------------------------------------
avli_NodeIdChanged_event(#state{set_cache = SetCache}) ->
    case
	is_any_attr(get_managedElement_keys(maps:keys(SetCache)),
		    [<<"networkManagedElementId">>, <<"siteLocation">>],
		    SetCache)
    of
	true ->
	    comsaServer:avli_other_event(?ALH_TAG_NodeIdChanged);
	false ->
	    ok
    end.

%%% #---------------------------------------------------------
format_validate_result(ok) ->
    %% The validation operation completed successfully with a true transaction
    %% result, i.e. the transaction result would be success if commited.
    {ok, true};
format_validate_result({error, Reason}) ->
    case get(internal_software_error) of
 	false ->
 	    %% The validation operation completed successfully with a false
 	    %% transaction result, i.e. the transaction result would be failure
 	    %% if commited.
 	    {ok, false, Reason};
 	true ->
 	    %% The validation operation failed. (Software error)
 	    {error, Reason}
    end;
format_validate_result(Result) ->
    Result.

%%% #---------------------------------------------------------
do_prepare_and_replay({Request, _TransId} = Msg, S) ->
    put(internal_software_error, false),
    case dirty(fun prepare_replay/1, S, Msg) of
	{ok, S1} ->
	    do_prep_replay(Msg, S1);
	ErrorRes ->
	    do_prepare_replay_failed(ErrorRes, Msg, Request =:= prepare)
    end.


do_prep_replay({validate, TransId} = Msg, S) ->
    case replay_trans(validate_fun(TransId), Msg, S) of
	{ok, S1} ->
	    call_completed_imm(Msg, S1);
	Error ->
	    Error
    end;

do_prep_replay({prepare, _TransId} = Msg, S)
  when S#state.tState =:= prepared ->
    replay_trans(fun prepare_fun/1, Msg, S);

do_prep_replay({prepare, TransId} = Msg, S) ->
    case replay_trans(validate_fun(TransId), Msg, S) of
	{ok, S1} ->
	    do_complete_and_prepare(Msg, S1);
	Error ->
	    Error
    end.


validate_fun(TransId) ->
    fun(S) ->
	    #prepared{nonGmfFunsDnKeys = NonGmfFunsDnKeys} = S#state.prepared,
	    Before1 = sysUtil:timediff_before({validate, get_imm_funs}),
	    case get_imm_funs(TransId, S#state.imm_data) of
		{ok, Funs} ->
		    sysUtil:timediff_after(Before1),
		    Before2 =
			sysUtil:timediff_before({validate, do_replay_mnesiaTr}),
		    {ok, #state{prepared = Prepared} = S1} =
			do_replay(Funs, NonGmfFunsDnKeys, S),
		    sysUtil:timediff_after(Before2),
		    S2 = S1#state{prepared = Prepared#prepared{funs = Funs}},
		    mnesia:abort({validate, finish_prepareTransaction(S2)});
		Error ->
		    sysUtil:timediff_after(Before1),
		    mnesia:abort({validate, {Error, S}})
	    end
    end.


prepare_fun(S) ->
    #prepared{funs = Funs,
	      nonGmfFunsDnKeys = NonGmfFunsDnKeys} = S#state.prepared,
    Before = sysUtil:timediff_before({prepare, do_replay_mnesiaTr}),
    {ok, S1} = do_replay(Funs, NonGmfFunsDnKeys, S),
    sysUtil:timediff_after(Before),
    finish_prepareTransaction(S1).


do_complete_and_prepare(Msg, S) ->
    case call_completed_imm(Msg, S) of
	{ok, S1} ->
	    replay_trans(fun prepare_fun/1, Msg, S1);
	Error ->
	    Error
    end.


call_completed_imm({Request, TransId} = Msg, State) ->
    Before = sysUtil:timediff_before({Request, completed_imm}),
    case internal_completed(TransId, State#state.imm_data) of
	ok ->
	    sysUtil:timediff_after(Before),
	    {ok, State};
	Error ->
	    sysUtil:timediff_after(Before),
	    do_prepare_replay_failed({Error, State}, Msg, false)
    end.


replay_trans(FunReplay, {Request, TransId} = Msg, PrepS) ->
    case transaction(FunReplay, PrepS, Msg) of
	{ok = Result, #state{prepared = Prepared} = S1} ->
	    NewPrepared =
		Prepared#prepared{result = Result},
	    {Result, S1#state{tState = prepared,
			      prepared = NewPrepared}};
	{validate, {Result, #state{prepared = Prepared} = S1}} ->
	    ConvertedResult =
		convert_result(Result, TransId, S1#state.imm_data),
	    NewPrepared =
		Prepared#prepared{result = ConvertedResult},
	    {ConvertedResult, S1#state{tState = prepared,
				       prepared = NewPrepared}};
	{Res1, S1} ->
	    Abort = Request =:= prepare andalso S1#state.tState =:= aborting,
	    do_prepare_replay_failed({Res1, S1}, {Request, TransId}, Abort)
    end.


do_prepare_replay_failed({Error, S}, {Request, TransId}, Abort) ->
    ImmData = S#state.imm_data,
    ConvertedError = convert_result(Error, TransId, ImmData),
    case Request of
	prepare when Abort ->
	    internal_abort(TransId, ImmData),
	    {ConvertedError, S};
	prepare ->
	    {ConvertedError, S};
	validate ->
	    S1 = S#state{prepared = #prepared{result = ConvertedError}},
	    {ConvertedError, reset_prepared(S1)}
    end.


%%% #---------------------------------------------------------
reset_prepared(#state{tState = TState,
		      prepared = #prepared{result = Result}} = State)
  when TState == prepared orelse
       TState == aborting ->
    Before = sysUtil:timediff_before({reset_prepared, abort_imm}),
    internal_abort(State#state.trans_id, State#state.imm_data),
    sysUtil:timediff_after(Before),
    State#state{user_objects = #{},
		tState = collecting,
		transaction_callbacks = [],
		prepared = #prepared{result = Result}};
reset_prepared(State) ->
    State.

%%% #---------------------------------------------------------
get_managedElement_keys([[_, <<"ManagedElement">>] = MeKey | Tail]) ->
    [MeKey | get_managedElement_keys(Tail)];
get_managedElement_keys([_ | Tail]) ->
    get_managedElement_keys(Tail);
get_managedElement_keys([]) ->
    [].

is_any_attr([Key | Tail], Attrs, Cache) ->
    case maps:find(Key, Cache) of
	{ok, CacheAttrs} ->
	    case is_attr(CacheAttrs, Attrs) of
		true ->
		    true;
		false ->
		    is_any_attr(Tail, Attrs, Cache)
	    end;
	_ ->
	    is_any_attr(Tail, Attrs, Cache)
    end;
is_any_attr([], _, _) ->
    false.

is_attr([{CacheAttr, _} | Tail], Attrs) ->
    case lists:member(CacheAttr, Attrs) of
	true ->
	    true;
	false ->
	    is_attr(Tail, Attrs)
    end;
is_attr([], _) ->
    false.

%%% #---------------------------------------------------------
transactDisabled_reason() ->
    Reason =
	try
	    [Obj] = ets:lookup(comsaTransactDisabled, true),
	    Obj#comsaTransactDisabled.reason
	catch
	    _ : _ ->
		"Unexpected error"
	end,
    format_reason(Reason).

%%% #---------------------------------------------------------
call(TransId, Msg) ->
    case (catch ets:lookup(comsaTransactionMapping, TransId)) of
	[{_, Pid}] when is_pid(Pid) ->
	    %% The timeout must be longer than COM timeout (1 hour)
	    gen_server:call(Pid, Msg, timer:hms(2, 0, 0));
	_ ->
	    {error, <<"Problems connecting to the transaction server">>}
    end.

get_pid(TransId) ->
    case
	[P || {TId, P} <- ets:tab2list(comsaTransactionMapping),
	      TId == TransId andalso is_pid(P)]
	of
	[Pid | _] ->
	    Pid;
	[] ->
	    not_found
    end.


get_all_keys(Cb, TransId, Key) ->
    get_all_keys(Cb, TransId, Key, [], undefined).
get_all_keys(Cb, TransId, Key, Acc, Prev) ->
    case tapply(Cb, nextMo, [Key, Prev, TransId]) of
	{ok,undefined} ->
	    lists:reverse(Acc);
	{ok,{ComKey,Internal}} ->
	    get_all_keys(Cb, TransId, Key, [ComKey | Acc], Internal)
    end.

%% get_user_object(Key, UserObjects) ->
%%     try
%% 	maps:fetch(Key, UserObjects)
%%     catch error:badarg ->
%% 	    undefined
%%     end.

store_user_object(Key,
		  {ok, NewUserObject},
		  #state{user_objects = UserObjects} = State) ->
    State#state{user_objects = UserObjects#{Key => NewUserObject}};
store_user_object(Key, ok, #state{user_objects = UserObjects} = State) ->
    case maps:is_key(Key, UserObjects) of
	true ->
	    State;
	false ->
	    State#state{user_objects = UserObjects#{Key => undefined}}
    end.

%%% #---------------------------------------------------------
get_old_vals([{AttrName, _Value} | Tail], DNKey, SetCache) ->
    [get_old_val(AttrName, DNKey, SetCache) |
     get_old_vals(Tail, DNKey, SetCache)];
get_old_vals([], _, _) ->
    [].

get_old_val(AttrName, DNKey, SetCache) ->
    get_old_val(maps:find(DNKey, SetCache), AttrName).

get_old_val({ok, [{AttrName, {_NewVal, OldVal}} | _]}, AttrName) ->
    OldVal;
get_old_val({ok, [_ | Tail]}, AttrName) ->
    get_old_val({ok, Tail}, AttrName);
get_old_val(error, _) ->
    throw(attr_not_found_in_cache).

%%% #---------------------------------------------------------
keysdelete_all([Key | Tail], TupleList) ->
    case lists:keymember(Key, 1, TupleList) of
	false ->
	    keysdelete_all(Tail, TupleList);
	true ->
	    keysdelete_all([Key | Tail], lists:keydelete(Key, 1, TupleList))
    end;
keysdelete_all([], TupleList) ->
    TupleList.

%%% #---------------------------------------------------------
remove_duplicates({setMoAttribute, _, Dn, AttrName, _}, Requests) ->
    remove_duplicates_set(Requests, Dn, [AttrName]);
remove_duplicates({setMoAttributes, _, Dn, Attrs}, Requests) ->
    AttrNames = [AttrName || {AttrName, _} <- Attrs],
    remove_duplicates_set(Requests, Dn, AttrNames).

remove_duplicates_set([{setMoAttribute, _, Dn, AttrName, _} = QRequest | Tail],
		      Dn,  %<<<--match-->>>/
		      AttrNames) ->
    case lists:member(AttrName, AttrNames) of
	false ->
	    [QRequest | remove_duplicates_set(Tail, Dn, AttrNames)];
	true ->
	    remove_duplicates_set(Tail, Dn, AttrNames)
    end;
remove_duplicates_set([{setMoAttributes, TransId, Dn, Attrs} | Tail],
		      Dn,  %<<<-----match----->>>/
		      AttrNames) ->
    case keysdelete_all(AttrNames, Attrs) of
	[] ->
	    remove_duplicates_set(Tail, Dn, AttrNames);
	New_Attrs ->
	    [{setMoAttributes, TransId, Dn, New_Attrs} |
	     remove_duplicates_set(Tail, Dn, AttrNames)]
    end;
remove_duplicates_set([{createMo, _, _, _, _, _, [_ | _]} = QRequest | Tail],
		      Dn,
		      AttrNames) ->
    %% Minimum matching in the function clause above for efficiency reasons.
    ParentDn = element(3, QRequest),
    Class    = element(4, QRequest),
    ClassId  = element(6, QRequest),
    Attrs    = element(7, QRequest),
    QDn =
	list_to_binary(comte_key_to_ecim([ClassId |
					  ecim_to_comte_key(ParentDn, Class)])),
    case QDn of
	Dn ->
	    [setelement(7, QRequest, keysdelete_all(AttrNames, Attrs)) |
	     remove_duplicates_set(Tail, Dn, AttrNames)];
	_ ->
	    [QRequest | remove_duplicates_set(Tail, Dn, AttrNames)]
    end;
remove_duplicates_set([QRequest | Tail], Dn, AttrNames) ->
    [QRequest | remove_duplicates_set(Tail, Dn, AttrNames)];
remove_duplicates_set([], _, _) ->
    [].

%%% #---------------------------------------------------------
remove_obsolete({deleteMo, _, Dn} = Request, Requests) ->
    NewRequests = remove_obsolete_create(Requests, Dn),
    case erase(obsolete_create_removed) of
	undefined ->
	    Requests ++ [Request];   % Same as NewRequests.
	true ->
	    remove_obsolete_set(NewRequests, Dn)
    end.

remove_obsolete_create([{createMo,_,ParentDn,Class,_,ClassId,_} = QReq | Tail],
		       Dn) ->
    QDn =
	list_to_binary(comte_key_to_ecim([ClassId |
					  ecim_to_comte_key(ParentDn, Class)])),
    QDnSize = erlang:byte_size(QDn),
    case binary:match(QDn, Dn) of
	nomatch ->
	    [QReq | remove_obsolete_create(Tail, Dn)];
	{0, QDnSize} ->
	    %% createMo of the exact same Dn as the deleteMo we're now handling.
	    put(obsolete_create_removed, true),
	    remove_obsolete_create(Tail, Dn);
	{0, _} ->
	    %% createMo of a Sub-Dn.
	    case get(obsolete_create_removed) of
		undefined ->
		    %% The "main" Dn is not created inside this transaction.
		    [QReq | remove_obsolete_create(Tail, Dn)];
		true ->
		    %% The "main" Dn exists only inside this transaction.
		    remove_obsolete_create(Tail, Dn)
	    end
    end;
remove_obsolete_create([QRequest | Tail], Dn) ->
    [QRequest | remove_obsolete_create(Tail, Dn)];
remove_obsolete_create([], _) ->
    [].

remove_obsolete_set([{setMoAttribute, _, QDn, _, _} = QRequest | Tail], Dn) ->
    case binary:match(QDn, Dn) of
	nomatch ->
	    [QRequest | remove_obsolete_set(Tail, Dn)];
	{0, _} ->
	    remove_obsolete_set(Tail, Dn)
    end;
remove_obsolete_set([{setMoAttributes, _, QDn, _} = QRequest | Tail], Dn) ->
    case binary:match(QDn, Dn) of
	nomatch ->
	    [QRequest | remove_obsolete_set(Tail, Dn)];
	{0, _} ->
	    remove_obsolete_set(Tail, Dn)
    end;
remove_obsolete_set([QRequest | Tail], Dn) ->
    [QRequest | remove_obsolete_set(Tail, Dn)];
remove_obsolete_set([], _) ->
    [].

%%% #---------------------------------------------------------
%% All callback functions
prepare_replay(#state{tState = prepared} = State) ->
    {ok, State};
prepare_replay(State) ->
    prepare_replay(State#state.request_q, State, []).

prepare_replay([{setMoAttribute, TransId, Dn, AttrName, Value} | Tail],
	       State,
	       NonGmfFunsDnKeys) ->
    [MoId | Mo] = DNKey = ecim_to_comte_key(Dn, ""),
    Callback = get_callback(DNKey),
    try
	case is_mo_created(State#state.create_cache, Mo, MoId) of
	    false ->
		OldVal = get_old_val(AttrName, DNKey, State#state.set_cache),
		check_mo_attribute([AttrName | DNKey], TransId, OldVal);
	    true ->
		%% No 'check_mo_attribute'. This is because otherwise it is not
		%% possible to give defaults to values in the createMo function.
		ok
	end
    catch
	throw : attr_not_found_in_cache ->
	    %% No 'check_mo_attribute'. The attribute has been removed from the
	    %% set-cache when a deleteMo Request was received.
	    ok
    end,
    {NewState, NewNonGmfFunsDnKeys} =
 	case
	    do_setMoAttributes(is_gmf_callback(Callback),
			       Callback,
 			       DNKey,
 			       [{AttrName, Value}],
			       TransId,
			       State)
	    of
 	    {cb_fun, CbFun} ->
		Before = sysUtil:timediff_before({prepare_imm, setMoAttribute}),
 		internal_prepare(CbFun, State#state.imm_data),
		sysUtil:timediff_after(Before),
		{State, [{CbFun, DNKey} | NonGmfFunsDnKeys]};
 	    UpdatedState ->
 		{UpdatedState, NonGmfFunsDnKeys}
 	end,
    prepare_replay(Tail, NewState, NewNonGmfFunsDnKeys);
prepare_replay([{setMoAttributes, TransId, Dn, NamedAttrs} | Tail],
	       State,
	       NonGmfFunsDnKeys) ->
    [MoId | Mo] = DNKey = ecim_to_comte_key(Dn, ""),
    Callback = get_callback(DNKey),
    try
	case is_mo_created(State#state.create_cache, Mo, MoId) of
	    false ->
		AttrKeys =
		    [[AttrName | DNKey] || {AttrName, _Value} <- NamedAttrs],
		OldVals = get_old_vals(NamedAttrs, DNKey, State#state.set_cache),
		check_mo_attributes(AttrKeys, TransId, OldVals);
	    true ->
		%% No 'check_mo_attributes'. This is because otherwise it is not
		%% possible to give defaults to values in the createMo function.
		ok
	end
    catch
	throw : attr_not_found_in_cache ->
	    %% No 'check_mo_attribute'. The attribute has been removed from the
	    %% set-cache when a deleteMo Request was received.
	    ok
    end,
    {NewState, NewNonGmfFunsDnKeys} =
 	case
	    do_setMoAttributes(is_gmf_callback(Callback),
			       Callback,
 			       DNKey,
 			       NamedAttrs,
			       TransId,
			       State)
	of
 	    {cb_fun, CbFun} ->
		Before =
		    sysUtil:timediff_before({prepare_imm, setMoAttributes}),
 		internal_prepare(CbFun, State#state.imm_data),
		sysUtil:timediff_after(Before),
		{State, [{CbFun, DNKey} | NonGmfFunsDnKeys]};
 	    UpdatedState ->
 		{UpdatedState, NonGmfFunsDnKeys}
 	end,
    prepare_replay(Tail, NewState, NewNonGmfFunsDnKeys);
prepare_replay([{createMo,
		 TransId,
		 ParentDn,
		 ClassName,
		 KeyAttrName,
		 KeyVal,
		 InitAttrs} | Tail],
	       State,
	       NonGmfFunsDnKeys) ->
    DNKey = ecim_to_comte_key(ParentDn, ClassName),
    Callback = get_callback(DNKey),
    case lists:keymember([KeyVal | DNKey], 1, State#state.delete_history) of
	false ->
	    check_mo_attribute([KeyAttrName, KeyVal | DNKey],
			       TransId,
			       []);
			       %% undefined);
	true ->
	    ok
    end,
    IsGmfCallback = is_gmf_callback(Callback),
    {CreState, CreNonGmfFunsDnKeys} =
	case
	    do_createMo(IsGmfCallback,
			Callback,
			DNKey,
			KeyAttrName,
			KeyVal,
			TransId,
			State)
	of
 	    {cb_fun, CbFun1} ->
 		Before = sysUtil:timediff_before({prepare_imm, createMo}),
		internal_prepare(CbFun1, State#state.imm_data),
		sysUtil:timediff_after(Before),
		{State, [{CbFun1, [KeyVal | DNKey]} | NonGmfFunsDnKeys]};
 	    UpdatedState1 ->
 		{UpdatedState1, NonGmfFunsDnKeys}
 	end,

    FilteredInitAttrs =
	lists:filter(fun({_,undefined}) -> false;
			(_) -> true
		     end, InitAttrs),

    {NewState, NewNonGmfFunsDnKeys} =
    	case
    	    do_setMoAttributes(IsGmfCallback,
    			       Callback,
    			       [KeyVal | DNKey],
    			       FilteredInitAttrs,
    			       TransId,
    			       CreState)
    	of
    	    {cb_fun, CbFun2} ->
    		Before2 = sysUtil:timediff_before({prepare_imm, createMo}),
    		internal_prepare(CbFun2, CreState#state.imm_data),
    		sysUtil:timediff_after(Before2),
    		{CreState, [{CbFun2, [KeyVal | DNKey]} | CreNonGmfFunsDnKeys]};
    	    UpdatedState2 ->
    		{UpdatedState2, CreNonGmfFunsDnKeys}
    	end,
    prepare_replay(Tail, NewState, NewNonGmfFunsDnKeys);
prepare_replay([{deleteMo, TransId, Dn} | Tail],
	       State,
	       NonGmfFunsDnKeys) ->
    DNKey = ecim_to_comte_key(Dn, ""),
    Callback = get_callback(DNKey),
    check_mo_exists(DNKey, TransId, State),
    {NewState, NewNonGmfFunsDnKeys} =
	case
	    do_deleteMo(is_gmf_callback(Callback),
			Callback,
			DNKey,
			TransId,
			State)
	    of
 	    {cb_fun, CbFun} ->
		Before = sysUtil:timediff_before({prepare_imm, deleteMo}),
 		internal_prepare(CbFun, State#state.imm_data),
		sysUtil:timediff_after(Before),
		{State, [{CbFun, DNKey, delete} | NonGmfFunsDnKeys]};
 	    UpdatedState ->
 		{UpdatedState, NonGmfFunsDnKeys}
 	end,
    prepare_replay(Tail, NewState, NewNonGmfFunsDnKeys);
prepare_replay([], #state{prepared = Prepared} = State, NonGmfFunsDnKeys) ->
    NewPrepared = Prepared#prepared{nonGmfFunsDnKeys = NonGmfFunsDnKeys},
    {ok, State#state{prepared = NewPrepared}}.

%%% #---------------------------------------------------------
do_replay([Fun | Tail], NonGmfFunsDnKeys, State) ->
    Before = sysUtil:timediff_before(cb_fun),
    NewState =
	case lists:keyfind(Fun, 1, NonGmfFunsDnKeys) of
	    false ->
		Fun(),
		sysUtil:timediff_after(Before),
		State;
	    {_, MO} ->
		Res = store_user_object(MO,
					Fun(),
					update_applied_cb_list(get_callback(MO),
							       State)),
		sysUtil:timediff_after(Before),
		Res;
	    {_, MO, delete} ->
		ok = Fun(),
		sysUtil:timediff_after(Before),
		{_, SetCache} =
		    lists:keyfind(MO, 1, State#state.delete_history),
		store_user_object(MO,
				  {ok, {deleted, SetCache}},
				  update_applied_cb_list(get_callback(MO),
							 State))
	end,
    do_replay(Tail, NonGmfFunsDnKeys, NewState);
do_replay([], _, State) ->
    {ok, State}.

do_get_all_keys(Key, TransId) ->
    Callback = get_callback(Key),
    get_all_keys(Callback, TransId, Key).

do_getMoAttribute(Key, TransId) ->
    Callback = get_callback(Key),
    tapply(Callback, getMoAttribute, [Key, TransId]).

do_getMoAttributes([Key|_Rest]=Keys, TransId) ->
    [_|DnRev] = Key,
    AttrNames = lists:map(fun([AttrName|DnRev1]) when DnRev==DnRev1->
				  %% Check that DnRev in Keys is the same
				  AttrName
			  end, Keys),

    Callback = get_callback(DnRev),
    tapply(Callback, getMoAttributes, [AttrNames, DnRev, TransId]).

%%% #---------------------------------------------------------
do_setMoAttributes(true, Callback, DNKey, AttrVals, TransId, State) ->
    %% gmf callback
    store_user_object(DNKey,
		      tapply(Callback,
			     setMoAttributes,
			     [AttrVals, DNKey, TransId]),
		      update_applied_cb_list(Callback, State));
do_setMoAttributes(false, Callback, DNKey, AttrVals, TransId, _) ->
    %% other callbacks
    Fun = fun() ->
		  tapply(Callback,
			 setMoAttributes,
			 [AttrVals, DNKey, TransId])
	  end,
    {cb_fun, Fun}.

%%% #---------------------------------------------------------
do_createMo(true, Callback, Key, KeyAttrName, KeyValue, TransId, State)->
    %% gmf callback
    store_user_object([KeyValue | Key],
		      tapply(Callback,
			     createMo,
			     [Key, KeyAttrName, KeyValue, TransId]),
		      update_applied_cb_list(Callback, State));
do_createMo(false, Callback, Key, KeyAttrName, KeyValue, TransId, _)->
    %% other callbacks
    Fun = fun() ->
		  tapply(Callback,
			 createMo,
			 [Key, KeyAttrName, KeyValue, TransId])
	  end,
    {cb_fun, Fun}.

%%% #---------------------------------------------------------
do_deleteMo(true, Callback, Key, TransId, State) ->
    %% gmf callback
    ok = tapply(Callback, deleteMo, [Key, TransId]),
    {_, SetCache} = lists:keyfind(Key, 1, State#state.delete_history),
    store_user_object(Key,
		      {ok, {deleted, SetCache}},
		      update_applied_cb_list(Callback, State));
do_deleteMo(false, Callback, Key, TransId, _) ->
    %% other callbacks
    Fun = fun() ->
		  tapply(Callback, deleteMo, [Key, TransId])
	  end,
    {cb_fun, Fun}.

update_applied_cb_list(Callback, #state{transaction_callbacks = CbList} = State) ->
    case lists:member(Callback, CbList) of
	false ->
	    NewCbList = [Callback | CbList],
	    put(transaction_callbacks, NewCbList),   % For abort cases.
	    State#state{transaction_callbacks = NewCbList};
	true ->
	    State
    end.

finish_prepareTransaction(State) ->
    do_applyCallback(State#state.transaction_callbacks,
		     prepareTransaction,
		     [maps:to_list(State#state.user_objects),
		      State#state.trans_id],
		     State).

inform_transactionCommited(State) ->
    {ok, State}.
%%     do_applyCallback(State#state.transaction_callbacks,
%% 		     transactionCommited,
%% 		     [maps:to_list(State#state.user_objects),
%% 		      State#state.trans_id],
%% 		     State).

inform_abortTransaction(State) ->
    do_applyCallback(get(transaction_callbacks),
		     abortTransaction,
		     [State#state.trans_id],
		     State).

do_applyCallback([Callback | Tail], CbFunc, CbArgs, State) ->
    Before = sysUtil:timediff_before({applyCB_mnesiaTr, Callback, CbFunc}),
    try xapply(Callback, CbFunc, CbArgs) of
	ok ->
	    sysUtil:timediff_after(Before),
	    do_applyCallback(Tail, CbFunc, CbArgs, State);
	{abort, Reason} ->
	    sysUtil:timediff_after(Before),
	    case mnesia:is_transaction() of
		true ->
		    mnesia:abort({abort, Reason});
		false ->
		    report_error({Callback, CbFunc, CbArgs},
				 State,
				 {no_transaction, {abort, Reason}}),
		    {{error, <<"Abort when no transaction">>}, State}
	    end;
	Unknown ->
	    sysUtil:timediff_after(Before),
	    report_error({Callback, CbFunc, CbArgs},
			 State,
			 {unknown_return, Unknown}),
	    {{error, <<"Unknown return value">>}, State}
    catch
	error : undef ->
	    %% Not all callback modules need to implement these functions.
	    %% I.e. the 'prepareTransaction' 'transactionCommited' and
	    %% 'abortTransaction' callback functions are optional.
	    sysUtil:timediff_after(Before),
	    do_applyCallback(Tail, CbFunc, CbArgs, State);
	ErrClass : ErrReason ->
	    sysUtil:timediff_after(Before),
	    report_error({Callback, CbFunc, CbArgs},
			 State,
			 {ErrClass, ErrReason}),
	    {{error, <<"Program fault">>}, State}
    end;
do_applyCallback([], _, _, State) ->
    {ok, State}.

do_existsMo(DnRev, TransId) ->
    Callback = get_callback(DnRev),
    xapply(Callback, existsMo, [DnRev, TransId]).

%% do_prepare(Key, UserObject, TransId) ->
%%     Callback = get_callback(Key),
%%     case tapply(Callback, prepare, [Key, UserObject, TransId]) of
%% 	{ok, NewUserObject} ->
%% 	    NewUserObject;
%% 	ok ->
%% 	    UserObject
%%     end.

%% do_commit(Callback, TransId) ->
%%     case safe_tapply(Callback, commit, [TransId], ok) of
%% 	ok ->
%% 	    ok
%%     end.

do_finish(Key, UserObject, TransId) ->
    Callback = get_callback(Key),
    try xapply(Callback, finish, [Key, UserObject, TransId]) of
	{error, Reason} ->
	    throw({?MODULE, Reason});
	ok ->
	    UserObject
    catch
	error:undef ->
	    UserObject
    end.

do_action(DNKey, NamedParams, TransId) ->
    Callback = get_callback(DNKey),
    [Name|Dn] = DNKey,
    case xapply(Callback, action, [Name, Dn, NamedParams, TransId]) of
	{error, Reason} ->
	    throw({?MODULE, Reason});
	Result ->
	    Result
    end.




count_mo_children(TransId, DNKey, ClassName, State) ->
    DeleteCacheChildren =
        lists:foldl(fun(_, deleted) ->
			    deleted;
		       ({[Class|Key], KeyVal}, AccDel)
			  when Class == ClassName,
			       Key == DNKey ->
                            [KeyVal|AccDel];
		       ({[Class|Key], KeyVal}, AccDel) ->
			    case lists:suffix([KeyVal,Class|Key],
					      [ClassName|DNKey]) of
				true ->
				    deleted;
				false ->
				    AccDel
			    end;
		       (_, AccDel) ->
                            AccDel
                    end, [], State#state.delete_cache),

    CreateCacheChildren =
	case DeleteCacheChildren of
	    deleted ->
		[];
	    _ ->
		lists:foldl(fun({[Class|Key],{_KeyAttr,KeyVal}}, AccCreate)
				  when Class==ClassName,
				       Key==DNKey ->
				    [KeyVal|AccCreate];
			       (_, AccCreate) ->
				    AccCreate
			    end, [], State#state.create_cache)
	end,

    StoredKeys =
	case DeleteCacheChildren of
	    deleted ->
		[];
	    _ ->
		do_get_all_keys([ClassName|DNKey], TransId)
	end,
    count_mo_children_next(StoredKeys, CreateCacheChildren,
                           DeleteCacheChildren, State).

count_mo_children_next(_, _, deleted, State) ->
    {0, State};
count_mo_children_next(StoredKeys, [], [], State) ->
    {length(StoredKeys), State};
count_mo_children_next(StoredKeys, [], DeletedKeys, State) ->
    NewList =
        lists:filter(fun({_, Val}) ->
                             not lists:member(Val, DeletedKeys)
                     end, StoredKeys),
    {length(NewList), State};
count_mo_children_next(StoredKeys, CreatedKeys, DeletedKeys, State) ->
    %% Merge stored and cached create keys
    MergedKeys =
        lists:foldl(fun(CreateVal, AccStored) ->
                            case lists:keyfind(CreateVal, 2, AccStored) of
                                {_,_} -> AccStored;
                                false -> [{cached, CreateVal} | AccStored]
                            end
                    end, StoredKeys, CreatedKeys),
    KeysCreateDelete =
        lists:foldl(fun(DelVal, AccMerged) ->
                            lists:keydelete(DelVal, 2, AccMerged)
                    end, MergedKeys, DeletedKeys),
    {length(KeysCreateDelete), State}.


check_mo_attribute(DNKey, TransId, Expected) ->
    case do_getMoAttribute(DNKey, TransId) of
	Expected ->
	    ok;
	[_|Values] when Values == Expected ->
	    ok;
	_Else ->
	    sysInitI:info_report([{?MODULE, check_mo_attribute},
				  {got, _Else},
				  {expected, Expected}]),
	    IoList = ["Data has been changed in another transaction: ",
		      comte_key_to_ecim(DNKey)],
	    Reason = translate_managed_element(IoList),
	    case mnesia:is_transaction() of
		true ->
		    mnesia:abort(Reason);
		false ->
		    throw(Reason)
	    end
    end.

check_mo_attributes(DNKeys, TransId, Expected) ->
    case do_getMoAttributes(DNKeys, TransId) of
	Expected ->
            ok;
        _Else ->
	    IoList = ["Data has been changed in another transaction: ",
		      comte_key_to_ecim(hd(DNKeys))|
		      [", "++comte_key_to_ecim(DNKey)||DNKey<-tl(DNKeys)]],
	    Reason = translate_managed_element(IoList),
	    case mnesia:is_transaction() of
		true ->
		    mnesia:abort(Reason);
		false ->
		    throw(Reason)
	    end
     end.


check_mo_exists([KeyVal, Class | ParentDn] = DnRev,
		TransId,
		#state{request_q = Req_q}) ->
    case do_existsMo(DnRev, TransId) of
	true ->
	    ok;
	false ->
	    case do_existsMo_Q(Req_q,
			       iolist_to_binary(comte_key_to_ecim(ParentDn)),
			       Class,
			       KeyVal)
		of
		true ->
		    ok;
		false ->
		    IoList = ["Data has been created in another transaction: ",
			      comte_key_to_ecim(DnRev)],
		    Reason = translate_managed_element(IoList),
		    case mnesia:is_transaction() of
			true ->
			    mnesia:abort(Reason);
			false ->
			    throw(Reason)
		    end
	    end
    end.

do_existsMo_Q([{createMo, _, ParentDn, Class, _, KeyVal, _} | _],
	      ParentDn, Class, KeyVal) ->
    true;
do_existsMo_Q([_ | Tail], ParentDn, Class, KeyVal) ->
    do_existsMo_Q(Tail, ParentDn, Class, KeyVal);
do_existsMo_Q([], _, _, _) ->
    false.

is_mo_created([{Mo, {_, MoId}} | _], Mo, MoId) ->
    true;
is_mo_created([_ | Tail], Mo, MoId) ->
    is_mo_created(Tail, Mo, MoId);
is_mo_created([], _, _) ->
    false.


tapply(M,F,A) ->
    Before = tapply_before(F, A),
    case xapply(M,F,A) of
	{abort, Reason} ->
	    sysUtil:timediff_after(Before),
	    mnesia:abort({abort, Reason});
	{error, Reason} ->
	    sysUtil:timediff_after(Before),
	    mnesia:abort({{M, F, A}, Reason});
	Res ->
	    sysUtil:timediff_after(Before),
	    Res
    end.

tapply_before(getMoAttribute = F, [[AttrName | _], _]) ->
    sysUtil:timediff_before({tapply, F, AttrName});
tapply_before(getMoAttributes = F, [AttrNames, [_, MoName | _] | _]) ->
    sysUtil:timediff_before({tapply,
			     F,
			     MoName,
			     {no_of_attrs, length(AttrNames)}});
tapply_before(setMoAttributes = F, [Attrs, [_, MoName | _] | _]) ->
    sysUtil:timediff_before({tapply, F, MoName, cnt_attrs(Attrs)});
tapply_before(createMo = F, [_, Key, _, _]) ->
    %% create is split into one createMo and one setMoAttrs. Should change...
    Attrs = [],
    sysUtil:timediff_before({tapply, F, Key, cnt_attrs(Attrs)});
tapply_before(createMo = F, [_, Key, _, Attrs | _]) ->
    sysUtil:timediff_before({tapply, F, Key, cnt_attrs(Attrs)});
tapply_before(nextMo = F, [[AttrName | _] | _]) ->
    sysUtil:timediff_before({tapply, F, AttrName});
tapply_before(F, A) ->
    sysUtil:timediff_before({tapply, F, A}).

cnt_attrs(Attrs) ->
    cnt_attrs(Attrs, []).

cnt_attrs([{_, {Type, _}} | Tail], Acc) ->
    cnt_attrs(Tail, cnt_attr(Acc, Type));
cnt_attrs([{_, [_ | _]} | Tail], Acc) ->
    cnt_attrs(Tail, cnt_attr(Acc, value_list));
cnt_attrs([_ | Tail], Acc) ->
    cnt_attrs(Tail, cnt_attr(Acc, unknown_type));
cnt_attrs([], Acc) ->
    Acc.

cnt_attr([{{_, Type}, Cnt} | Tail], Type) ->
    [{{no_of, Type}, Cnt + 1} | Tail];
cnt_attr([E | Tail], Type) ->
    [E | cnt_attr(Tail, Type)];
cnt_attr([], Type) ->
    [{{no_of, Type}, 1}].

transaction(Fun, State, Msg) ->
    case mnesia:transaction(Fun, [State], infinity) of
	{atomic, Res} ->
	    Res;
	{aborted, {validate, _} = Validate} ->
	    Validate;
	{aborted, {abort, Reason}} ->
	    {{error, format_reason(Reason)}, State#state{tState = aborting}};
	{aborted, {{?MODULE, prepare, _}, Res}} ->
	    {Res, State};
	{aborted, Reason} when is_binary(Reason) ->
	    {{error, Reason}, State#state{tState = aborting}};
	{aborted, Reason} when is_list(Reason) ->
	    {{error, list_to_binary(Reason)}, State#state{tState = aborting}};
	{aborted, {throw, Reason}} ->
	    set_error_string(Reason, State#state.imm_data),
	    {{error, <<"sa_ais_err_failed_operation">>},
	     State#state{tState = aborting}};
	{aborted, Reason} ->
	    Error = case Reason of
	    		{{_M,_F,_A},R} -> R;
	    		Reason -> Reason
	    	    end,
	    case is_safs_crash(Error) of
		true ->
		    {{error, <<"sa_ais_err_failed_operation">>},
		     State#state{tState = aborting}};
		false ->
		    report_error(Msg,State,Error),
		    %% {{error, format_reason(Error)}, State#state{tState = aborting}}
		    {{error, <<"Internal error">>},
		     State#state{tState = aborting}}
	    end
    end.

dirty(Fun, State, Msg) ->
    try
	mnesia:async_dirty(Fun, [State])
    catch
	throw:Reason when is_binary(Reason) ->
	    {{error, Reason}, State#state{tState = aborting}};
	exit : {aborted, Reason} ->
	    Error = case Reason of
			{{_M,_F,_A},R} -> R;
			Reason -> Reason
		    end,
	    report_error(Msg,State,Error),
	    {{error, format_reason(Error)}, State#state{tState = aborting}};
	T : E ->
	    Error = case E of
			{{_M,_F,_A},R} -> R;
			Reason -> Reason
		    end,
	    report_error(Msg, State, Error),
	    sysInitI:error_report([{?MODULE, dirty},
				       {T, E}]),
	    {{error, <<"Internal error">>}, State#state{tState = aborting}}
    end.


format_reason(Reason) when is_binary(Reason) ->
    Reason;
format_reason(Reason) ->
    case io_lib:printable_list(Reason) of
	true ->
	    list_to_binary(Reason);
	false ->
	    list_to_binary(lists:flatten(io_lib:format("~p",[Reason])))
    end.

%%report_warning(Msg,State,Reason) ->
%%    report(warning_report,Msg,State,Reason).
report_error(Msg,State,Reason) ->
    put(internal_software_error, true),
    report(error_report,Msg,State,Reason).

report(F, Msg, #state{tState = TState, trans_id = TransId} = S, Reason) ->
    report(F, Msg, TState, TransId, Reason, S).
report(_F, _Msg, _Error, _TransId, Reason , _) when is_binary(Reason) ->
    %% Discard all reasons that are binaries, since they are not errors
    ok;
report(F, Msg, TState, TransId, Reason, S) ->
    sysInitI:F([{application, comte},
		{pid, self()},
		{transaction_state, TState},
		{transaction_id, TransId},
		{last_message, Msg},
		{reason, Reason},
		{call_stack, erlang:get_stacktrace()}] ++ report_format(S)).

report_format(Rec) when is_tuple(Rec) ->
    try
	Len = length([RecordName | Tail] = tuple_to_list(Rec)),
	[{list_to_atom("#" ++ atom_to_list(RecordName) ++ "{}"),
	  list_to_atom("no of elements: " ++ integer_to_list(Len - 1))} |
	  report_format(get_record_info(RecordName, Len), Tail)]
    catch
	Class : Reason ->
	    [{Class, Reason}]
    end;
report_format(Other) ->
    [Other].

report_format([F | TailF], [V | TailV]) ->
    [report_field(F, V) | report_format(TailF, TailV)];
report_format([], []) ->
    [].

report_field(user_objects, V) ->
    {user_objects, maps:to_list(V)};
report_field(set_cache, V) ->
    {set_cache, maps:to_list(V)};
report_field(F, V) ->
    {F, V}.

get_record_info(state, _) ->
    record_info(fields, state);
get_record_info(_, Len) ->
    default_record_info(Len).

default_record_info(Len) ->
    default_record_info(Len - 1, 1).

default_record_info(Len, Pos) when Len > 0 ->
    [list_to_atom("e" ++ integer_to_list(Pos)) |
     default_record_info(Len - 1, Pos + 1)];
default_record_info(0, _) ->
    [].

convert_result({abort, Error}, TransId, ImmData) ->
    convert_result(Error, TransId, ImmData);
convert_result({error, SafAtom}, TransId, ImmData) when is_atom(SafAtom) ->
    Reason = list_to_binary(atom_to_list(SafAtom)),
    convert_saf_error(TransId, ImmData, Reason, SafAtom);
convert_result({error, <<"sa_ais_err_",_/binary>> = Reason}, TransId, ImmData)->
    SafAtom = list_to_atom(binary_to_list(Reason)),
    convert_saf_error(TransId, ImmData, Reason, SafAtom);
convert_result(Result, _, _) ->
    Result.

convert_saf_error(TransId, ImmData, Reason, SafAtom) ->
    SafText = [safs:format_error(SafAtom), " (", Reason, ")", $\n],
    ImmErrorStrings = [[A, $\n] || A <- get_error_strings(TransId, ImmData)],
    ErrorText = translate_managed_element([SafText, ImmErrorStrings]),
    {error, ErrorText}.

get_error_strings(_TransId, undefined) ->
    [];
get_error_strings(TransId, {ImmGlobalCbMod, ImmData}) ->
    ImmGlobalCbMod:get_error_strings_imm(TransId, ImmData).

translate_managed_element(IoList) ->
    NME = get_nme(),
    translate_managed_element(IoList, NME).

translate_managed_element(IoList, false) ->
    iolist_to_binary(IoList);
translate_managed_element(IoList, {true, Nme}) ->
    re:replace(IoList, <<"ManagedElement=1">>, [<<"ManagedElement=">>, Nme],
	       [global, {return, binary}]).

get_nme() ->
    Data = comTop:get_managed_element_data(),
    V = proplists:get_value(networkManagedElementId, Data),
    get_nme(V).

get_nme(undefined) -> false;
get_nme("1")       -> false;
get_nme(V)         -> {true, V}.

set_error_string(_Reason, undefined) ->
    ok;
set_error_string(Reason, {ImmGlobalCbMod, ImmData}) ->
    ImmGlobalCbMod:set_error_string_imm(Reason, ImmData).

get_callback(Key) ->
    comsaLib:get_callback(Key).


is_safs_crash({_, [{M,_,_,_}|_]}) ->
    string:str(atom_to_list(M), "safs") =:= 1;
is_safs_crash(_) ->
    false.


exists_in_create_cache(Key, Id, [{Key, {_KeyAttr, Id}} | _Cache]) ->
    true;
exists_in_create_cache(Key, Id, [_Mo|Cache]) ->
    exists_in_create_cache(Key, Id, Cache);
exists_in_create_cache(_Key, _Id, []) ->
    false.

exists_in_delete_cache(Key, Id, [{A, B} | Cache]) ->
    case lists:suffix([B|A], [Id|Key]) of
	true ->
	    true;
	false ->
	    exists_in_delete_cache(Key, Id, Cache)
    end;
exists_in_delete_cache(_Key, _Id, []) ->
    false.

get_cache_attr([Id|Key] = DnKey, AttrName, SetCache, CreateCache,
	       DeleteCache) ->
    case exists_in_delete_cache(Key, Id, DeleteCache) of
	true ->
	    {[], undefined};
	false ->
	    case maps:find(DnKey, SetCache) of
		{ok, AttrList} ->
		    case lists:keyfind(AttrName, 1, AttrList) of
			{AttrName, {_Val, _OldVal}=Vals} ->
			    Vals;
			false ->
			    case exists_in_create_cache(Key, Id, CreateCache) of
				true ->
				    {undefined, undefined};
				false ->
				    undefined
			    end
		    end;
		error ->
		    undefined
	    end
    end.

get_setCache_newValues(DnKey, Cache) ->
    case maps:find(DnKey, Cache) of
        {ok, Attrs} ->
	    [{AttrName, NewVal} || {AttrName, {NewVal, _OldVal}} <- Attrs];
        error ->
            []
    end.

part_cache_get_attrs(DnKey, AttrNames, Cache) ->
     case maps:find(DnKey, Cache) of
         {ok, AttrList} ->
             {Cached, Get} =
                 lists:mapfoldl(fun(AttrName, AccGet) ->
                                        case lists:keyfind(AttrName, 1, AttrList) of
                                            {AttrName, {Val, _OldVal}} ->
                                                {Val, AccGet};
                                            _Other ->
                                                {get, [[AttrName|DnKey] | AccGet]}
                                        end
                                end, [], AttrNames),
             {Cached, lists:reverse(Get)};
         error ->
             %% All to be fetched from mnesia
             {lists:duplicate(length(AttrNames), get),
              [ [AttrName|DnKey] || AttrName <- AttrNames]}
     end.

part_cache_set_attrs(DnKey, NamedAttrVals, Cache) ->
     case maps:find(DnKey, Cache) of
        {ok, AttrList} ->
             {Cached, Set} =
                 lists:mapfoldl(fun({AttrName, NewVal}, AccSet) ->
                                    case lists:keyfind(AttrName, 1, AttrList) of
                                        {AttrName, {_Val, OldVal}} ->
                                            %% Set new value
                                            { {AttrName, {NewVal, OldVal}}, AccSet };
                                        false ->
                                            { {AttrName, {NewVal, get}},
                                              [[AttrName|DnKey] | AccSet] }
                                    end
                                end, [], NamedAttrVals),
             {Cached, lists:reverse(Set)};
         error ->
             %% All to be fetched
             {C, Set} =
                 lists:mapfoldl(fun({AttrName,Val}, Acc) ->
                                        { {AttrName, {Val, get}},
                                          [ [AttrName|DnKey] | Acc ] }
                                end, [], NamedAttrVals),
             {C, lists:reverse(Set)}
     end.

set_cache_attr_values(Key, AttrVals, Cache) ->
    case maps:find(Key, Cache) of
        {ok, AttrList} ->
            NewAttrs = attr_store(AttrVals, AttrList),
	    Cache#{Key := NewAttrs};
        error ->
	    Cache#{Key => AttrVals}
    end.

attr_store([{AttrName, _AttrVal}=KV|AttrVals], AttrList) ->
    attr_store(AttrVals, lists:keystore(AttrName, 1, AttrList, KV));
attr_store([], AttrList) ->
    AttrList.

init_attr_store(DnKey, InitAttrs, Cache) ->
    NewInitAttrs =
        lists:map(fun({_AttrName, {{_T,_Val}, _}}=AttrVal) ->
                          AttrVal;
                     ({AttrName, NewVal}) ->
                          {AttrName, {NewVal, do_not_check}}
                  end, InitAttrs),
    set_cache_attr_values(DnKey, NewInitAttrs, Cache).



replace_attrs(Entries, Attrs) ->
    replace_attrs(Entries, Attrs, []).

replace_attrs([], _, Acc) ->
    lists:reverse(Acc);
replace_attrs([get | Entries], [], Acc) ->
    replace_attrs(Entries, [], [undefined | Acc]);
replace_attrs([get | Entries], [AttrVal | Attrs], Acc) ->
    replace_attrs(Entries, Attrs, [AttrVal | Acc]);
replace_attrs([Entry|Entries], Attrs, Acc) ->
    replace_attrs(Entries, Attrs, [Entry | Acc]).



replace_old_attrs(CacheAttrs, Attrs) ->
    replace_old_attrs(CacheAttrs, Attrs, []).

replace_old_attrs([], _, Acc) ->
    lists:reverse(Acc);
replace_old_attrs([{Key, {NewVal, get}} | CacheAttrs], [AttrVal | Attrs], Acc) ->
    %% Replace the placeholder
    replace_old_attrs(CacheAttrs, Attrs, [{Key, {NewVal, AttrVal}} | Acc]);
replace_old_attrs([{Key, {NewVal, get}} | CacheAttrs], undefined, Acc) ->
    %% When there is no previous value
    replace_old_attrs(CacheAttrs, undefined, [{Key, {NewVal, undefined}} | Acc]);
replace_old_attrs([Entry|CacheAttrs], Attrs, Acc) ->
    replace_old_attrs(CacheAttrs, Attrs, [Entry | Acc]).


%%%-----------------------------------------------------------------------------
%%% IMM adaptations START
%%%-----------------------------------------------------------------------------

%% IMM transaction initialisation
init_imm(_TransId, undefined) ->
    undefined;
init_imm(TransId, ImmGlobalCbMod) ->
    %% COMMENTS: init_imm
    %% IMM OM (if needed), Admin Owner, Ccb, Accessor need to be initialised
    {ImmGlobalCbMod, ImmGlobalCbMod:init_imm(TransId)}.

get_imm_funs(_TransId, undefined) ->
    ok;
get_imm_funs(TransId, {ImmGlobalCbMod, ImmData}) ->
    ImmGlobalCbMod:get_imm_funs(TransId, ImmData).

internal_finish(_TransId, undefined) ->
    ok;
internal_finish(TransId, {ImmGlobalCbMod, ImmData}) ->
    ImmGlobalCbMod:finish_imm(TransId, ImmData).

internal_completed(_TransId, undefined) ->
    ok;
internal_completed(TransId, {ImmGlobalCbMod, ImmData}) ->
    ImmGlobalCbMod:completed_imm(TransId, ImmData).

internal_prepare(_Fun, undefined) ->
    ok;
internal_prepare(Fun, {ImmGlobalCbMod, ImmData}) ->
    ImmGlobalCbMod:prepare_imm(Fun, ImmData).

internal_apply(_TransId, undefined) ->
    ok;
internal_apply(TransId, {ImmGlobalCbMod, ImmData}) ->
    ImmGlobalCbMod:apply_imm(TransId, ImmData).

internal_abort(_TransId, undefined) ->
    ok;
internal_abort(TransId, {ImmGlobalCbMod, ImmData}) ->
    ImmGlobalCbMod:abort_imm(TransId, ImmData).

%%%-----------------------------------------------------------------------------
%%% IMM adaptations STOP
%%%-----------------------------------------------------------------------------


%%%-----------------------------------------------------------------------------
%%% comte_model code START
%%%-----------------------------------------------------------------------------
ecim_to_comte_key(Dn, ClassName) when is_list(Dn) ->
    ecim_to_comte_key(list_to_binary(Dn),ClassName);
ecim_to_comte_key(Dn, ClassName) when is_list(ClassName) ->
    ecim_to_comte_key(Dn,list_to_binary(ClassName));
ecim_to_comte_key(Dn, <<"">>) ->
    split_dn(Dn, []);
ecim_to_comte_key(Dn, ClassName) ->
    [ClassName | split_dn(Dn, [])].

comte_key_to_ecim(ComteKey) ->
    format_comte_key(lists:reverse(ComteKey)).

split_dn(<<>>,Acc) ->
    Acc;
split_dn(Dn,Acc) ->
    case binary:split(Dn,<<",">>,[trim]) of
	[Elem,Rest] ->
	    NewAcc = split_elem(Elem,Acc),
	    split_dn(Rest,NewAcc);
	[Elem] ->
	    split_elem(Elem,Acc)
    end.

split_elem(Elem,Acc) ->
    case binary:split(Elem,<<"=">>,[trim]) of
	[Name,Key] ->
	    [Key, hd(binary:split(Name,<<".">>,[trim])) | Acc];
	[Name] ->
	    [Name | Acc]
    end.

format_comte_key([Class, Key | Tail]) ->
    [Class, "=", Key | format_comte_key_tail(Tail)];
format_comte_key([Member]) ->
    [".", Member];
format_comte_key([]) ->
    [].

format_comte_key_tail([Class, Key | Tail]) ->
    [",", Class, "=", Key | format_comte_key_tail(Tail)];
format_comte_key_tail([Member]) ->
    [".", Member];
format_comte_key_tail([]) ->
    [].

%%%-----------------------------------------------------------------------------
%%% comte_model code STOP
%%%-----------------------------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           xapply(M,F,A)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: This is a workaround for parameterized modules which was an
%%%              expermiental design in OTP R15 which is no longer supported
%%% ----------------------------------------------------------

xapply(Module, Function, Args) when is_atom(Module) ->
    apply(Module, Function, Args);
xapply(ParameterizedModule, Function, Args) when is_tuple(ParameterizedModule) ->
    [Module|ExtraArgs] = tuple_to_list(ParameterizedModule),
    apply(Module, Function, Args++ExtraArgs).

is_gmf_callback(Callback) when element(1, Callback) == gmfComteI ->
    true;
is_gmf_callback(_) ->
    false.

dump_log() ->
    comsaSyncServer:sync().

%%--------------------------------------------------------------------
flush_exit({error, Reason} = Error) ->
    receive
	{'EXIT', _, Reason} ->
	    flush_exit(Error)
    after
	0 ->
	    ok
    end;
flush_exit(_) ->
    ok.

%%--------------------------------------------------------------------
mapping_complete(TransId, Pid) ->
    ets:insert(comsaTransactionMapping, {TransId, Pid}).

%%--------------------------------------------------------------------
mapping_delete(TransId, MappedTo) ->
    case catch ets:lookup(comsaTransactionMapping, TransId) of
	[{_, MappedTo}] when is_pid(MappedTo) ->
	    catch ets:delete(comsaTransactionMapping, TransId),
	    true;
	[{_, {joining, _}}] ->
	    catch ets:delete(comsaTransactionMapping, TransId),
	    true;
	_ ->
	    false
    end.

%%--------------------------------------------------------------------
mapping_join(TransId) ->
    I = self(),
    MappedTo = {joining, I},
    case ets:insert_new(comsaTransactionMapping, {TransId, MappedTo}) of
	true ->
	    true;
	false ->
	    case catch ets:lookup(comsaTransactionMapping, TransId) of
		[{_, {reserved, I}}] ->
		    ets:insert(comsaTransactionMapping, {TransId, MappedTo}),
		    true;
		_ ->
		    false
	    end
    end.

%%--------------------------------------------------------------------
mapping_reserve(TransId) ->
    MappedTo = {reserved, self()},
    ets:insert_new(comsaTransactionMapping, {TransId, MappedTo}).

%%--------------------------------------------------------------------
log(Direction, Req, Data) ->
    TimeStamp = os:timestamp(),
    Format = create_format(Data),
    Msg = lists:flatten(io_lib:format(Format, Data)),
    Sender =
	case Direction of
	    in ->  Req ++ " <-";
	    out -> Req ++ " ->"
	end,
    logI:awrite_log("ComInterfaceLog", Sender, info, TimeStamp, Msg).

create_format([])    -> "";
create_format([_])   -> "~p";
create_format([_|T]) -> "~p " ++ create_format(T).

%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
%% This is to be removed when support for deprecated OamAccessPoint attributes
%% ends somewhere in 17X.
mirror_deprecated(R, SA) ->
    lists:foldl(fun(MirrorSA, ok) ->
			[Req, TransId | Pars] = tuple_to_list(MirrorSA),
			ReqStr = atom_to_list(Req),
			log(in, ReqStr, [TransId | Pars]),
			Res = call(TransId, MirrorSA),
			log(out, ReqStr, [TransId, Res]),
			Res;
		   (_, AccRes) ->
			AccRes
		end, R, get_mirror_set_attributes(R, SA)).


get_mirror_set_attributes(ok, SA) ->
    comSysM:get_mirror_set_attributes(SA);

get_mirror_set_attributes(_R, _SA) ->
    [].
