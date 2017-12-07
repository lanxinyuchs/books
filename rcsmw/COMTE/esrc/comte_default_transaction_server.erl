%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%% @author Lukas Larsson <lukas@erlang-solutions.com>
%% @author Magnus Lidén <magnus.liden@ericsson.com>
%% @author Raimo Niskanen <raimo.niskanen@ericsson.com>
%% Created : 21 Mar 2011 by Lukas Larsson

%%
%% Revision History
%%
%% Date: 20150508
%% Name: uabhten, erasipe
%% Description: OTP-11851. 
%% From Erlang 18, predefined type tuple/N, N>0,  no longer exists
%% instead spec_init
%% must be defined as normal tuple {}.

-module(comte_default_transaction_server).

-behaviour(gen_server).
-behaviour(comte_transaction).

%% API
%% Initialization
-export([start/1]).
%% Transaction exports
-export([join/1]).
-export([prepare/1]).
-export([commit/1]).
-export([abort_transaction/1]).
-export([finish/1]).
-export([validate/1]).
%% MO exports
-export([setMoAttribute/4]).
-export([setMoAttributes/3]).
-export([getMoAttribute/3]).
-export([getMoAttributes/3]).
-export([getMoIterator/3]).
-export([createMo/6]).
-export([deleteMo/2]).
-export([existsMo/2]).
-export([countMoChildren/3]).
-export([action/4]).

%% Callback registration
-export([register_data_callback/2]).
-export([get_callback/1]).

%% For test
-export([trans_tab/0]).

-include("comte_types.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Types
-type reply() :: term().
-type reason() :: normal | shutdown | term().
-type states() :: inited |
                  collecting |
                  action |
                  committed |
                  prepared |
                  aborting |
                  aborted.

-record(state, { trans_id :: transaction_id(),
		 user_objects :: Dict :: term(),
		 prepared_user_objects :: Dict :: term(),
		 create_cache :: [{term(), {term(),term()}}],
		 delete_cache :: [{term(),term()}],
		 set_cache :: Dict :: term(),
		 state :: states() }).

%% Defines
-define(SEC, 1000).
-define(DEF_TIMEOUT, 30*?SEC).

-define(def_not_exist, []).

%% Returns 'ok'
-define(
   LOGGER(Severity, Req, State, Fmt),
   begin
       comte_lib:error_logger((Severity), ?MODULE, ?LINE, (Req),
                              (State), (Fmt))
   end).
-define(
   LOGGER(Severity, Req, State, Fmt, Args),
   begin
       comte_lib:error_logger((Severity), ?MODULE, ?LINE, (Req),
                              (State), (Fmt), (Args))
   end).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc
%% Register a callback to handle operations for the MO and it's children,
%% referenced by the DN. See {@link comte_example_callback}.
%% @end
-spec register_data_callback(ECIMDn::[binary()], Cb::module()) ->
                                    ok | {error, Reason::term()} | no_return().
register_data_callback(ECIMDn, Cb)
  when is_list(ECIMDn);
       is_atom(Cb) ->
    comte_lib:tab_insert(data_tab(), lists:reverse(ECIMDn), Cb).



%% @doc
%% The callback is invoked by ComtE during startup.
%% Initializes the data callback table and registers
%% a data callback if available.
%% @end
-spec start(Opts::term()) -> ok.
start(Opts) ->
    ok = create_tables(),

    %% The callback must reply with
    %% {DN, Callback}
    [[register_data_callback(Dn, Cb)
      || {Dn,Cb} <-
             case InitDataCallback of
                 {M,F} ->
                     M:F();
                 {M,F,A} ->
        	     M:F(A)
             end]
     || {init_data_callback,InitDataCallback} <- Opts],
    ok.



%% @doc
%% As a participant/resource, this callback is a request
%% to join the transaction.
%% The default server joins the transaction by starting up
%% a transaction process.
%% @end
-spec join(TransId::transaction_id()) -> ok | com_error().
join(TransId) ->
    case gen_server:start_link(?MODULE, {TransId, self()}, []) of
	{ok, Pid} ->
            ok = comte_lib:tab_insert(trans_tab(), TransId, Pid);
	_ ->
	    {error, <<"Could not create transaction, probably COM "
		      "crashed without cleaning up everything in comte.">>}
    end.

%% @doc
%% As a participant, it's required to 'prepare' itself before it can commit.
%% If 'ok' is returned, the participant must be able to execute the commit
%% as an atomic operation.
%% @end
-spec prepare(TransId::transaction_id()) -> ok | com_error().
prepare(TransId) ->
    trans_call(tid(TransId), {prepare,TransId}).


%% @doc
%% The participant is asked to commit all changes in the transaction
%% and return 'ok' or an error.
%% @end
-spec commit(TransId::transaction_id()) -> ok | com_error().
commit(TransId) ->
    trans_call(tid(TransId), {commit,TransId}).


%% @doc
%% The participant is asked to validate all changes in the transaction.
%% If the validation failed, return for example {ok, false, [Reason]}.
%% This callback is only applicable when compiling ComtE against
%% MafOamSpiTransactionalResource_2.h and higher.
%% @end
-spec validate(TransId::transaction_id()) ->
                      ok | {ok, boolean()} | {ok, boolean(), [binary()]}
                          | com_error().
validate(TransId) ->
    trans_call(tid(TransId), {validate,TransId}).


%% @doc
%% The participant shall abort the transaction.
%% @end
-spec abort_transaction(TransId::transaction_id()) -> ok | com_error().
abort_transaction(TransId) ->
    trans_call(tid(TransId), {abort_transaction,TransId}).

%% @doc
%% This callback allows the participant to delay cleaning up until
%% the complete transaction is officially done.
%% @end
-spec finish(TransId::transaction_id()) -> ok | com_error().
finish(TransId) ->
    trans_call(tid(TransId), {finish,TransId}).


%% @doc
%% Sets an attribute value for the MO referenced in DN.
%% @end
-spec setMoAttribute(TransId::transaction_id(),
                     DN::ecim_dn(),
                     AttrName::mo_attribute_name(),
                     AttrValue::com_data()) -> ok | com_error().
setMoAttribute(TransId, Dn, AttrName, AttrValue) ->
    trans_call(tid(TransId), {setMoAttribute,TransId, Dn, AttrName, AttrValue}).


%% @doc
%% Sets attribute values for the MO(Managed Object) referenced by the
%% DN(Distinguished Name).
%% @end
-spec setMoAttributes(TransId::transaction_id(),
                     DN::ecim_dn(),
                     NamedAttrs::list(com_named_attribute())) ->
                            ok | com_error().
setMoAttributes(TransId, Dn, NamedAttrs) ->
    trans_call(tid(TransId), {setMoAttributes,TransId, Dn, NamedAttrs}).

%% @doc
%% Get a MO(Managed Object) attribute value referenced by the
%% DN(Distinguished Name).
%% @end
-spec getMoAttribute(TransId::transaction_id(),
                     DN::ecim_dn(),
                     AttrName::mo_attribute_name()) ->
                            AttrValue::com_data() | com_error().
getMoAttribute(TransId, Dn, AttrName) ->
    trans_call(tid(TransId), {getMoAttribute,TransId, Dn, AttrName}).

%% @doc
%% Note, this is not implemented!
%% Get several Managed Object attributes at once.
%% The return order of the attribute values shall be maintained.
%% @end
-spec getMoAttributes(TransId::transaction_id(),
                      DN::ecim_dn(),
                      AttrNames::list(mo_attribute_name())) ->
                            AttrValues::[com_data()] | com_error().
getMoAttributes(TransId, Dn, AttrNames) ->
    trans_call(tid(TransId), {getMoAttributes, TransId, Dn, AttrNames}).

%% @doc
%% The participant is asked to return all the direct children
%% of the MOs(Managed Object) referenced by the DN. The scope is
%% limited by the ClassName parameter.
%% @end
-spec getMoIterator(TransId::transaction_id(),
                    DN::ecim_dn(),
                    ClassName::binary()) ->
                           list(mo_instance()) | com_error().
getMoIterator(TransId, Dn, ClassName) ->
    trans_call(tid(TransId), {getMoIterator,TransId, Dn, ClassName}).

%% @doc
%% Create a MO(Managed Object) of class specified by ClassName below the
%% MO specified by ParentDN. Optional attributes to set for the MO may
%% be included.
%% @end
-spec createMo(TransId::transaction_id(),
               ParentDN::ecim_dn(),
               ClassName::binary(),
               KeyAttrName::binary(),
               KeyAttrValue::binary(),
               InitialAttributes::list(com_named_attribute())) ->
                           ok | com_error().
createMo(TransId, ParentDn, ClassName, KeyAttrName, KeyValue, InitAttr) ->
    trans_call(tid(TransId), {createMo,TransId, ParentDn, ClassName,
				      KeyAttrName, KeyValue, InitAttr}).
%% @doc
%% Delete a MO(Managed Object) referenced by the DN.
%% @end
-spec deleteMo(TransId::transaction_id(),
               DN::ecim_dn()) ->
                      ok | com_error().
deleteMo(TransId, Dn) ->
    trans_call(tid(TransId), {deleteMo,TransId, Dn}).

%% @doc
%% Check the existance of the MO(Managed Object)
%% referenced by the DN.
%% @end
-spec existsMo(TransId::transaction_id(),
               DN::ecim_dn()) ->
                      true | false | com_error().
existsMo(TransId, Dn) ->
    trans_call(tid(TransId), {existsMo,TransId, Dn}).

%% @doc
%% Count the number of Managed Object instances of a given
%% MO class (MOC) directly below the specified parent DN.
%% If no DN is specified, the top level should be
%% considered the default.
%% @end
-spec countMoChildren(TransId::transaction_id(),
                      Dn::ecim_dn(),
                      ClassName::binary()) ->
                             non_neg_integer() | com_error().
countMoChildren(TransId, Dn, ClassName) ->
    trans_call(tid(TransId), {countMoChildren,TransId, Dn, ClassName}).

%% @doc
%% The participant is asked to perform the action ActionName
%% defined in the scope of the MO referenced by the DN.
%% Use the return value 'undefined' or 'ok' for void actions.
%% @end
-spec action(TransId::transaction_id(),
             Dn::ecim_dn(),
             ActionName::binary(),
             Params::list(com_named_parameter())) ->
    ok | com_data() | com_error().
action(TransId, Dn, Name, Params) ->
    trans_call(tid(TransId), {action,TransId, Dn, Name, Params}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the server
%%--------------------------------------------------------------------
-spec init({transaction_id(), pid()}) -> {ok, State :: #state{}}.
init({TransId, Pid}) ->
    erlang:monitor(process, Pid),
    {ok, #state{
       trans_id = TransId,
       create_cache = [],
       delete_cache = [],
       set_cache = dict:new(),
       state = inited }}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling call messages
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: term(), State :: #state{} ) ->
			 {reply, reply(), #state{}} |
			 {stop, reason(), reply(), #state{}}.

%% Make sure to update state so that an action cannot be performed
%% in the same transaction as any modification call.
handle_call(Request, From, S = #state{ state = inited })
  when element(1, Request) == setMoAttribute;
       element(1, Request) == setMoAttributes;
       element(1, Request) == createMo;
       element(1, Request) == deleteMo ->
    handle_call(Request, From, S#state{ state = collecting });
handle_call(Request, _F, S = #state{ state = State })
  when (element(1, Request) == setMoAttribute orelse
        element(1, Request) == setMoAttributes orelse
	element(1, Request) == createMo orelse
	element(1, Request) == deleteMo) andalso State /= collecting ->
    {reply, {error, <<"Transaction is not in collecting state!">>}, S};

handle_call(Request, From, S = #state{ state = inited })
  when element(1, Request) == action ->
    handle_call(Request, From, S#state{ state = action });
handle_call(Request, _F, S = #state{ state = State })
  when element(1, Request) == action andalso State /= action ->
    {reply, {error, <<"Transaction is not in action state!">>}, S};

handle_call({getMoAttribute, TransId, Dn, AttrName} = Msg, _F, State) ->
    {R,S} = t(fun(S) ->
		      [Id|Key] = DnKey = comte_model:ecim_to_comte_key(Dn, <<"">>),

                      case exists_in_cache(Key, Id, S#state.delete_cache) of
                          true ->
                              %% Should reply as {error, not exist}
                              {?def_not_exist, S};
                          false ->
                              case get_cache_attr(DnKey, AttrName, S#state.set_cache) of
                                  not_found ->
                                      {do_getMoAttribute([AttrName|DnKey], TransId),
                                       S};
                                  {Val, _OldValue} ->
                                      {Val,S}
                              end
                      end
              end, State, Msg),
    {reply, R, S};

handle_call({getMoAttributes, TransId, Dn, AttrNames} = Msg, _F, State) ->
    {R,S} = t(fun(S) ->
                      DnKey = comte_model:ecim_to_comte_key(Dn, <<"">> ),

                      %% Later check for it in delete cache

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
              end, State, Msg),
    {reply, R, S};

handle_call({getMoIterator, TransId, Dn, ClassName} = Msg, _F, State) ->
    {R,S} = t(fun(S) ->
		  Key = comte_model:ecim_to_comte_key(Dn,ClassName),
		  Keys = do_get_all_keys(Key, TransId),
		  CKeys = [?STRING(MOKey) ||
			      {_MO, {_KeyName, MOKey}} <- proplists:lookup_all(
						 Key, S#state.create_cache)],
		  DKeys = [?STRING(MOKey) ||
			      {_, MOKey} <- proplists:lookup_all(
					      Key, S#state.delete_cache)],
		  {lists:sort((Keys ++ CKeys) -- DKeys),S}
	  end, State, Msg),
    {reply, R, S};

handle_call({setMoAttribute, TransId, Dn, AttrName, [{Tag,Value}]},
	    From, State) ->
    handle_call({setMoAttribute, TransId, Dn, AttrName, {Tag,Value}},
	    From, State);
handle_call({setMoAttribute, TransId, Dn, AttrName, Value} = Msg, _F, State) ->
    {R,S} = t(fun(S) ->
		      DnKey = comte_model:ecim_to_comte_key(Dn, <<"">>),
		      OrigVal =
                          case get_cache_attr(DnKey, AttrName, S#state.set_cache) of
                              not_found ->
                                  do_getMoAttribute([AttrName|DnKey], TransId);
                              {_, Old} ->
                                  Old
                          end,
                      AttrVals = [ {AttrName, {Value, OrigVal}} ],
		      NewSetCache = set_cache_attr_values(DnKey, AttrVals,
                                                          S#state.set_cache),

		      {ok,S#state{ set_cache = NewSetCache }}
	      end, State, Msg),
    {reply, R, S};

handle_call({setMoAttributes, TransId, Dn, NamedAttrs} = Msg, _F, State) ->
     {R,S} = t(fun(S) ->
                       DnKey = comte_model:ecim_to_comte_key(Dn, <<"">>),
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
                       {ok, S#state{ set_cache = NewSetCache }}
	      end, State, Msg),
    {reply, R, S};


handle_call({createMo, TransId, ParentDn, ClassName,
	     KeyAttrName, KeyValue, InitAttrs} = Msg, _F, State) ->
    {R,S} = t(fun(S) ->
		      Key = comte_model:ecim_to_comte_key(ParentDn,ClassName),
                      NewDeleteCache = lists:delete({Key, KeyValue},
						    S#state.delete_cache),

                      {NewCreateCache, NewSetCache}
			  = case lists:member(
				   ?STRING(KeyValue),
				   do_get_all_keys( Key , TransId)) of
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
                                    MergeInitAttrs = [{KeyAttrName, {?STRING(KeyValue), create}}
                                                      | InitAttrs],
                                    AttrSetCache = init_attr_store([KeyValue | Key],
                                                                   MergeInitAttrs,
                                                                   S#state.set_cache),

				    {[{Key, {KeyAttrName, KeyValue}} | S#state.create_cache ],
                                     AttrSetCache}


			    end,
                      {ok, S#state{ set_cache = NewSetCache,
				    delete_cache = NewDeleteCache,
				    create_cache = NewCreateCache } }
	      end, State, Msg),
    {reply, R, S};

handle_call({deleteMo, _TransId, Dn} = Msg, _F, State) ->
    {R,S} = t(fun(S) ->
		      [Id|Key] = DnKey = comte_model:ecim_to_comte_key(Dn, <<"">>),

		      %% Delete from set cache
		      FilteredSetCache = dict:erase(DnKey, State#state.set_cache),

		      %% Filter out the creates with the same Id
                      %% from the create cache.
                      %% Maybe change key here...
		      NewCreateCache =
			  lists:filter(fun({DNKey, {_KeyAttrName,KeyVal}}) ->
					       case {KeyVal, DNKey} of
						   {Id, Key} ->
						       false;
						   _Else ->
						       true
					       end
				       end, S#state.create_cache),

		      {NewDeleteCache, NewSetCache} =
                          case S#state.create_cache of
                              NewCreateCache ->
                                  %% MO not residing in the cache
                                  { [{Key, Id} | S#state.delete_cache],
                                    FilteredSetCache };
                              _Else ->
                                  %% MO removed from the create cache
                                  {S#state.delete_cache, FilteredSetCache}
			  end,
                      {ok, S#state{ set_cache = NewSetCache,
				    create_cache = NewCreateCache,
				    delete_cache = NewDeleteCache } }
	      end,State, Msg),
    {reply, R, S};

handle_call({existsMo, TransId, Dn} = Msg, _F, State) ->
    %% Exists in the cache or committed
    {Res, St} = t(fun(S) ->
                          [Id|Key] = comte_model:ecim_to_comte_key(Dn, <<"">>),
                          %% Check cache
                          case {exists_in_cache(Key, Id, S#state.create_cache),
                                exists_in_cache(Key, Id, S#state.delete_cache)} of
                              { true, _} ->
                                  %% Exists in create cache
                                  {true, S};
                              {_, true} ->
                                  %% Exists in delete cache, this should reply
                                  %% with false otherwise the create operation
                                  %% will abort.
                                  {false, S};
                             {_OtherCreate, _OtherDelete} ->
                                  %% Check committed
                                  Exists = do_existsMo([Id|Key], TransId),
                                  {Exists, S}

                         end
                 end, State, Msg),
    {reply, Res, St};


handle_call({countMoChildren, TransId, Dn, ClassName} = Msg, _F, State) ->
    {Res, S} = t(fun(S) ->
                         Key = comte_model:ecim_to_comte_key(Dn, <<"">>),
                         {_R,S} = count_mo_children(TransId, Key, ClassName, State)
                 end, State, Msg),
    {reply, Res, S};

handle_call({validate, TransId} = Msg, _F, State) ->
    %% Add mechanism for validate
    {Res,S} = t(fun trans_validate/2, State, [TransId], Msg),
    {reply, Res, S};
    %% case Res of
    %%     ok -> {reply, ok, S};
    %%     _ ->  {reply, Res, S}
    %% end;

handle_call({prepare, TransId} = Msg, _F, State) ->
    %% Run prepare, will abort
    {Res,S} = t(fun trans_prepare/2, State, [TransId], Msg),

    case is_list(catch dict:fetch_keys(Res)) of
	true ->
	    {reply, ok, S#state{ state = prepared,
				 prepared_user_objects = Res}};
	false ->
	    {reply, Res, S#state{ state = aborting }}
    end;

handle_call({commit, TransId} = Msg, _F, State = #state{ state = prepared }) ->
    {Res,S} = t(fun(S) ->
			{ok, ReplayS} = do_replay(TransId, S),
			CBs = lists:map(
				fun({DNKey,_}) ->
					get_callback(DNKey)
				end,dict:to_list(ReplayS#state.user_objects)),
			lists:foreach(fun(Callback) ->
					      do_commit(Callback,TransId)
				      end,lists:usort(CBs)),
			{ok, ReplayS#state{ state = committed }}
		end, State, Msg),
    {reply, Res, S};

handle_call({abort_transaction, _TransId}, _F, State) ->
    {reply, ok, State#state{ state = aborting } };

handle_call({finish, TransId} = Msg, _F, State = #state{ state = committed }) ->
    try
	dict:map(fun(DNKey, UserObject) ->
			     do_finish(DNKey, UserObject, TransId)
		     end, State#state.prepared_user_objects),
	{stop, normal, ok, State}
    catch throw:{?MODULE, Reason} ->
	    report_error(Msg,State,Reason),
	    {stop, normal, {error,format_reason(Reason)}, State};
	  E:Reason ->
	    report_error(Msg,State,{E,Reason}),
	    {stop, normal, {error,format_reason({E,Reason})}, State}
    end;
handle_call({finish, _TransId}, _F, State) ->
    {stop, normal, ok, State};

handle_call({action, TransId, Dn, Name, NamedParams} = Msg, _F, State) ->
    try
	DNKey = comte_model:ecim_to_comte_key(Dn,Name),
	{reply, do_action(DNKey, NamedParams, TransId), State}
    catch throw:{?MODULE, Reason} ->
	    report_error(Msg,State,Reason),
	    {reply, {error,format_reason(Reason)}, State};
	  E:Reason ->
	    report_error(Msg,State,{E,Reason}),
	    {reply, {error,format_reason({E,Reason})}, State}
    end;

handle_call(_Request, _F, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info({'DOWN',_, process, _, normal}, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate.
%%--------------------------------------------------------------------
-spec terminate(Reason :: reason(), State :: #state{}) -> ok.
terminate(_Reason, State) ->
    ok = comte_lib:tab_delete_entry(trans_tab(),
                                    State#state.trans_id),
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private
trans_tab() ->
    comte_transaction_procs.
%% @private
data_tab() ->
    comte_data_callbacks.

tid(TransId) ->
    case comte_lib:tab_lookup(trans_tab(), TransId) of
        {TransId, Pid} -> Pid;
        Error -> Error
    end.

trans_call(Server, Req) when is_pid(Server) ->
    gen_server:call(Server, Req, ?DEF_TIMEOUT);
trans_call(Error, Request) ->
    ?LOGGER(warning, Request, Error,
            "Unexpected, transaction server does not exist"),
    {error, <<"Transaction server not found!">>}.


create_tables() ->
    Tables = [trans_tab(),
              data_tab()],
    Opts = [set, public, named_table],
    ok = comte_lib:tab_create(Tables, Opts).


get_all_keys(Cb, TransId, Key) ->
    get_all_keys(Cb, TransId, Key, [], undefined).
get_all_keys(Cb, TransId, Key, Acc, Prev) ->
    case tapply(Cb, nextMo, [Key, Prev, TransId]) of
	{ok,undefined} ->
	    lists:reverse(Acc);
	{ok,{ComKey,Internal}} ->
	    get_all_keys(Cb, TransId, Key, [ComKey | Acc], Internal)
    end.

get_user_object(Key, UserObjects) ->
    try
	dict:fetch(Key, UserObjects)
    catch error:badarg ->
	    undefined
    end.


store_user_object(Key, UserObjects, {ok, NewUserObject}) ->
    dict:store(Key, NewUserObject, UserObjects);
store_user_object(Key, UserObjects, ok) ->
    case dict:is_key(Key, UserObjects) of
	true ->
	    UserObjects;
	false ->
	    dict:store(Key, undefined, UserObjects)
    end.

-spec trans_prepare(tuple(), integer()) -> no_return().
trans_prepare(State, TransId) ->
    {ok, NewS} = do_replay(TransId, State),
    PUOs = dict:map(
             fun(DNKey, UserObject) ->
                     do_prepare(DNKey, UserObject, TransId)
             end, NewS#state.user_objects),
    %% Return the prepared user objects which are to be used
    %% when the call to commit comes.
    mnesia:abort({{?MODULE, prepare, [TransId]}, PUOs}).


-spec trans_validate(tuple(), integer()) -> no_return().
trans_validate(State, TransId) ->
    {ok, NewS} = do_replay(TransId, State),

    %% {Valid, Messages} =
    case
	dict:fold(
	  fun (DNKey, UserObject, {Val, Msgs} = Acc) ->
		  case do_validate(DNKey, UserObject, TransId) of
		      ok ->
			  Acc;
		      {ok, V} when is_boolean(V) ->
			  {Val and V, Msgs};
		      {ok, V, M} when is_boolean(V), is_binary(M) ->
			  {Val and V, [M | Msgs]};
		      {error, Reason} = Error when is_binary(Reason) ->
			  mnesia:abort(
			    {{?MODULE, validate, [TransId]},
			     Error})
		  end
	  end,
	  {true, []},
	  NewS#state.user_objects)
    of
	{true, []} ->
	    mnesia:abort(
	      {{?MODULE, validate, [TransId]}, ok});
	{Valid, []} ->
	    mnesia:abort(
	      {{?MODULE, validate, [TransId]}, {ok, Valid}});
	{Valid, Messages} ->
	    mnesia:abort(
	      {{?MODULE, validate, [TransId]}, {ok, Valid, Messages}})
    end.

do_replay(TransId, State) ->

    DUOs = lists:foldl(fun({DNKey,Id}, UserObjects) ->
			       check_mo_exists(DNKey,Id, TransId),
			       do_deleteMo([Id|DNKey], UserObjects, TransId)
		       end, dict:new(), lists:reverse(State#state.delete_cache)),


    CUOs = lists:foldl(fun({DNKey, {Name, Key}}, UserObjects) ->
			       check_mo_attribute([Name,Key|DNKey],
                                                  TransId, ?def_not_exist),
			       do_createMo(DNKey, UserObjects,
					   Name, Key, TransId)
		       end,DUOs, lists:reverse(State#state.create_cache)),


    %% Mark those attributes which have been set after a MO has been
    %% created to do_not_check. This is done because otherwise it is
    %% not possible to give defaults to values in the createMo function.
    SetCache = mark_as_created(State#state.set_cache, State#state.create_cache),
    SetCacheList = dict:to_list(SetCache),

    NewSetCacheList =
        lists:map(fun({DnKey, AttrList}) ->
                          {DoNotCheck, Check} =
                              lists:foldl(fun({AttrName, {Val, do_not_check}}, {AccDNC, AccChk}) ->
                                                  { [{[AttrName|DnKey], Val} | AccDNC], AccChk};
                                             ({AttrName, {Val, NonCrDe}}, {AccDNC, AccChk})
                                                when NonCrDe /= create,
                                                     NonCrDe /= delete ->
                                                  {AccDNC, [{[AttrName|DnKey], {Val, NonCrDe}} | AccChk]};
                                             (_AttrVal, {AccDNC, AccChk}) ->
                                                  {AccDNC, AccChk}
                                          end, {[],[]}, AttrList),

                          {DnKey, {DoNotCheck, Check}}
                  end, SetCacheList),

    SUOs = lists:foldl(fun({_DnKey, {DoNotCheck, CheckList}}, UserObjects) ->

                               SUOs1 = do_setMoAttributes(DoNotCheck, UserObjects, TransId),

                               {Keys, ExpValues, NewCheckList} =
                                   lists:foldl(fun( {K, {New,Old}}, {AccKeys, AccExp, AccNew}) ->
                                                       {[ K | AccKeys], %% Acc the keys
                                                        [ Old | AccExp], %% Acc expected
                                                        [ {K, New} | AccNew ]} %% Acc expected old vals
                                               end,
                                               {[], [], []},
                                               CheckList),

                               check_mo_attributes(Keys, TransId, ExpValues),
                               do_setMoAttributes(NewCheckList, SUOs1, TransId)

                       end, CUOs, NewSetCacheList),

    {ok, State#state{ user_objects = SUOs } }.


do_get_all_keys(Key, TransId) ->
    Callback = get_callback(Key),
    get_all_keys(Callback, TransId, Key).

do_getMoAttribute(Key, TransId) ->
    Callback = get_callback(Key),
    tapply(Callback, getMoAttribute, [Key, TransId]).

do_getMoAttributes([], _TransId) ->
    [];
do_getMoAttributes([Key|_Rest]=Keys, TransId) ->
    Callback = get_callback(Key),
    tapply(Callback, getMoAttributes, [Keys, TransId]).


do_setMoAttributes([], UserObjects, _TransId) ->
    UserObjects;
do_setMoAttributes([{Key, _Vals} | _Rest]=NamedAttrs, UserObjects, TransId) ->
    Callback = get_callback(Key),
    UserObject = get_user_object(tl(Key), UserObjects),
    store_user_object(tl(Key), UserObjects,
                      tapply(Callback, setMoAttributes, [NamedAttrs, UserObject, TransId])).


do_createMo(Key, UserObjects, KeyAttrName, KeyValue, TransId) ->
    Callback = get_callback(Key),
    store_user_object(
      [KeyValue|Key], UserObjects,
      tapply(Callback, createMo, [Key, KeyAttrName, KeyValue, TransId])).

do_deleteMo(Key, UserObjects, TransId) ->
    Callback = get_callback(Key),
    ok = tapply(Callback, deleteMo, [Key, TransId]),
    store_user_object(
      Key, UserObjects, {ok, deleted}).

do_existsMo(Key, TransId) ->
    Callback = get_callback(Key),
    tapply(Callback, existsMo, [Key, TransId]).

do_prepare(Key, UserObject, TransId) ->
    Callback = get_callback(Key),
    case tapply(Callback, prepare, [Key, UserObject, TransId]) of
	{ok, NewUserObject} ->
	    NewUserObject;
	ok ->
	    UserObject
    end.

do_validate(Key, UserObject, TransId) ->
    Callback = get_callback(Key),
    case tapply(Callback, validate, [Key, UserObject, TransId]) of
	ok -> ok;
        Err -> Err
    end.

do_commit(Callback, TransId) ->
    case safe_tapply(Callback, commit, [TransId], ok) of
	ok ->
	    ok
    end.

do_finish(Key, UserObject, TransId) ->
    Callback = get_callback(Key),
    case apply(Callback, finish, [Key, UserObject, TransId]) of
	{error, Reason} ->
	    throw({?MODULE, Reason});
	ok ->
	    UserObject
    end.

do_action(DNKey, NamedParams, TransId) ->
    Callback = get_callback(DNKey),
    case apply(Callback, action, [DNKey, NamedParams, TransId]) of
	{error, Reason} ->
	    throw({?MODULE, Reason});
	Result ->
	    Result
    end.



count_mo_children(TransId, DNKey, ClassName, State) ->
    CreateCacheChildren =
        lists:foldl(fun({[Class|Key],{_KeyAttr,KeyVal}}, AccCreate)
                           when Class==ClassName,
                                Key==DNKey ->
                            [KeyVal|AccCreate];
                       (_, AccCreate) ->
                            AccCreate
                    end, [], State#state.create_cache),

    DeleteCacheChildren =
        lists:foldl(fun({[Class|Key], KeyVal}, AccDel)
                       when Class == ClassName,
                            Key == DNKey ->
                            [KeyVal|AccDel];
                       (_, AccDel) ->
                            AccDel
                    end, [], State#state.delete_cache),

    StoredKeys = do_get_all_keys([ClassName|DNKey], TransId),
    count_mo_children_next(StoredKeys, CreateCacheChildren,
                           DeleteCacheChildren, State).

count_mo_children_next(StoredKeys, [], [], State) ->
    {length(StoredKeys), State};
count_mo_children_next(StoredKeys, [], DeletedKeys, State) ->
    NewList =
        lists:filter(fun({_, Val}) ->
                             case lists:member(Val, DeletedKeys) of
                                 true -> false;
                                 false -> true
                             end
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
                            case lists:keyfind(DelVal, 2, AccMerged) of
                                {_,_} -> lists:keydelete(DelVal, 2, AccMerged);
                                false ->
                                    AccMerged
                            end
                    end, MergedKeys, DeletedKeys),
    {length(KeysCreateDelete), State}.


check_mo_attribute(DNKey, TransId, Expected) ->
    case do_getMoAttribute(DNKey, TransId) of
	Expected ->
	    ok;
	%% Value when Expected == not_undefined andalso Value /= undefined ->
	%%     ok;
	_Else ->
            mnesia:abort({data_has_been_changed, [DNKey]})
    end.


check_mo_attributes([], _TransId, _Expected) ->
    ok;
check_mo_attributes(DNKeys, TransId, Expected) ->
    case do_getMoAttributes(DNKeys, TransId) of
	Expected ->
	    ok;
	_Else ->
            mnesia:abort({data_has_been_changed, DNKeys})
    end.


check_mo_exists(DNKey, Id, TransId) ->
    case lists:member(?STRING(Id), do_get_all_keys(DNKey, TransId)) of
	true ->
	    ok;
	_Else ->
	    mnesia:abort({data_has_been_created, DNKey})
    end.

mark_as_created(SCache, [{DN, {_, Id}} | RestCreate]) ->
    DNMatch = [Id|DN],
    mark_as_created(
      dict:map(fun(Dn, AttrList)
                     when Dn == DNMatch ->
                       lists:map(fun need_for_attr_check/1, AttrList);
                  (_, Val) ->
                       Val
	       end, SCache), RestCreate);
mark_as_created(SCache, []) ->
    SCache.


need_for_attr_check({AttrName, {NewVal, NoNeedVal}})
  when NoNeedVal == undefined;
       NoNeedVal == ?def_not_exist ->
    {AttrName, {NewVal, do_not_check}};
need_for_attr_check(Attr) ->
    Attr.


safe_tapply(M,F,A,Default) ->
     {Mod,NewA} = fix_param_module(M,A),
     case code:is_loaded(Mod) of
	{file,_} ->
	    case [Arity || {Func,Arity} <- Mod:module_info(exports),
			   Func =:= F, Arity =:= NewA] of
		[_Arity] ->
		    tapply(M,F,A);
		_ ->
		    Default
	    end;
	_ ->
	    Default
     end.

fix_param_module(M,A) when is_atom(M) ->
     {M,length(A)};
fix_param_module(ParamMod,A) when is_tuple(ParamMod) ->
     {element(1,ParamMod),length(A)+1}.

tapply(M,F,A) ->
    case apply(M,F,A) of
	{error, Reason} ->
	    mnesia:abort({{M, F, A}, Reason});
	Res ->
	    Res
    end.

t(Fun, #state{}=State, Msg) ->
    t(Fun, State, [], Msg).
t(Fun, State, ExtraArgs, Msg) ->
    case mnesia:transaction(Fun, [State|ExtraArgs], infinity) of
	{atomic,Res} ->
	    Res;
	{aborted, {{?MODULE, prepare, _}, Res}} ->
	    {Res, State};
        {aborted, {{?MODULE, validate, _}, Res}} ->
	    {Res, State};
	{aborted, {data_has_been_changed, DNKeyList}} ->
	    {{error, iolist_to_binary(
		       ["Data has been changed in another transaction: " |
			[comte_model:comte_key_to_ecim(DNKey) || DNKey <- DNKeyList ] ])},
	     State#state{ state = aborting }};
	{aborted, {data_has_been_created, DNKeyList}} ->
	    {{error, iolist_to_binary(
		       ["Data has been created in another transaction: " |
			[comte_model:comte_key_to_ecim(DNKey) || DNKey <- DNKeyList ] ])},
	     State#state{ state = aborting }};
	{aborted, Reason} ->
	    Error = case Reason of
			{{_M,_F,_A},R} -> R;
			Reason -> Reason
		    end,
	    report_error(Msg,State,Error),
	    { {error, format_reason(Error)}, State#state{ state = aborting } }
    end.

format_reason(Reason) when is_binary(Reason) ->
    Reason;
format_reason(Reason) ->
    case io_lib:printable_list(Reason) of
	true ->
	    Reason;
	false ->
	    list_to_binary(lists:flatten(io_lib:format("~p",[Reason])))
    end.

%report_warning(Msg,State,Reason) ->
%    report(warning_report,Msg,State,Reason).
report_error(Msg,State,Reason) ->
    report(error_report,Msg,State,Reason).

report(F,Msg,#state{ state = State, trans_id = TransId },Reason) ->
    report(F,Msg,State,TransId,Reason).
report(F,Msg,Error,TransId,Reason) ->
    comte_error_logger:F([{application,comte},
		    {pid,self()},
		    {errorContext,Error},
		    {transaction_id,TransId},
		    {last_message,Msg},
		    {reason,Reason}]).

%% @private
get_callback(Key) ->
    Mo = lists:reverse(strip_indexes(lists:reverse(Key))),
    get_callback_int(Mo).

strip_indexes([Elem, _Index | Rest]) ->
    [Elem | strip_indexes(Rest)];
strip_indexes([Elem]) ->
    [Elem];
strip_indexes([]) ->
    [].

get_callback_int([_H|T] = Mo) ->
     case comte_lib:tab_lookup(data_tab(), Mo) of
	{Mo, Cb} ->
	    Cb;
	{error, _} ->
	    get_callback_int(T)
    end;
get_callback_int([]) ->
    error({no_callback_registered, comte_lib:tab_dump(data_tab()) }).


exists_in_cache(Key, Id, [{Key, {_KeyAttr, Id}} | _Cache]) ->
    true;
exists_in_cache(Key, Id, [{Key, Id} | _Cache]) ->
    true;
exists_in_cache(Key, Id, [_Mo|Cache]) ->
    exists_in_cache(Key, Id, Cache);
exists_in_cache(_Key, _Id, []) ->
    false.

%%  TODO CLEANUP
get_cache_attr(DnKey, AttrName, Cache) ->
    case dict:find(DnKey, Cache) of
        {ok, AttrList} ->
            case lists:keyfind(AttrName, 1, AttrList) of
                {AttrName, {_Val, _OldVal}=Vals} ->
                    Vals;
                false ->
                    check_cache_created(AttrList)
            end;
        error ->
            not_found
    end.

check_cache_created(AttrList) ->
    case is_created(AttrList) of
        %% If created, we can return an unset value
        true -> {undefined, undefined};
        false -> not_found
    end.

is_created([{_KeyName, {{_Type, _KeyVal}, create}} | _Rest]) ->
    true;
is_created([_Attr|Rest]) ->
    is_created(Rest);
is_created([]) ->
    false.


part_cache_get_attrs(DnKey, AttrNames, Cache) ->
     case dict:find(DnKey, Cache) of
         {ok, AttrList} ->
             {Cached, Get} =
                 lists:mapfoldl(fun(AttrName, AccGet) ->
                                        case lists:keyfind(AttrName, 1, AttrList) of
                                            {AttrName, {Val, _OldVal}} ->
						%% In cache
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
     case dict:find(DnKey, Cache) of
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
    case dict:find(Key, Cache) of
        {ok, AttrList} ->
            NewAttrs = attr_store(AttrVals, AttrList),
            dict:store(Key, NewAttrs, Cache);
        error ->
            dict:store(Key, AttrVals, Cache)
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
replace_old_attrs([{Key, {NewVal, get}} | CacheAttrs], GotVal, Acc)
  when GotVal == undefined;
       GotVal == ?def_not_exist ->
    %% When there is no previous value
    replace_old_attrs(CacheAttrs, undefined, [{Key, {NewVal, undefined}} | Acc]);
replace_old_attrs([Entry|CacheAttrs], Attrs, Acc) ->
    replace_old_attrs(CacheAttrs, Attrs, [Entry | Acc]).


