%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2013. All Rights Reserved.
%%
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%%
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_ntf_srv.erl
%%
%% Description:
%%
%%
%%--------------------------------------------------------------------
-module(safs_ntf_srv).
-behaviour(gen_server).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------

-include("ntf.hrl").
-include("safs_ais.hrl").
-include("safs_ntf.hrl").

-include("safs_ntf_db.hrl").

-include_lib("stdlib/include/ms_transform.hrl").
%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------

-export([
	 start/1,
	 lookup_tbl/2
        ]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([
         init/1,
	 handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
-define(SERVER, ?MODULE).

-define(SUPPORTED_NTF_VERSION, #safe_version{release_code = $A,
					     major_version = 1,
					     minor_version = 1}).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(safs_ntf_user,
	{handle, version, callbacks, proxy, cb_proxy, subscriptions,
	 read_handles}).
-record(state, {parent
	       }).

%%======================================================================
%% External Interface Functions
%%======================================================================
%%--------------------------------------------------------------------
%% Function: start/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start(_Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self()], []).

%%====================================================================
%% Internal interface functions
%%====================================================================

%%====================================================================
%% Server
%%====================================================================

%%----------------------------------------------------------------------
%% Function: init(Args) ->
%%     {ok, State} |
%%     {ok, State, Timeout} |
%%     ignore               |
%%     {stop, Reason}
%% Description: Initiates the server
%% @private
%%----------------------------------------------------------------------
init([Parent]) ->
    ets:new(safs_ntf_user, [named_table, {keypos, #safs_ntf_user.handle}]),
    ets:new(safs_ntf_subscriber,
	    [named_table, {keypos, #safs_ntf_subscriber.id}]),
    ets:new(safs_ntf_reader,
	    [named_table, public, {keypos, #safs_ntf_reader.read_handle}]),

    ets:new(safs_ntf_id, [named_table, public]),
    ets:insert(safs_ntf_id, {handle, 1}),
    ets:insert(safs_ntf_id, {readHandle, 1}),

    safs_ntf_db:init(),
    {ok, #state{parent = Parent}}.

%%----------------------------------------------------------------------
%% Function: handle_call(Request, From, State) ->
%%     {reply, Reply, State} |
%%     {reply, Reply, State, Timeout} |
%%     {noreply, State} |
%%     {noreply, State, Timeout} |
%%     {stop, Reason, Reply, State} |
%%     {stop, Reason, State}
%% Description: Handling call messages
%% @private
%%----------------------------------------------------------------------
handle_call({initialize, Callbacks, Version, Proxy}, _From, S) ->
    Handle = create_handle(),
    User = #safs_ntf_user{handle = Handle,
			  version = Version,
			  callbacks = Callbacks,
			  proxy = Proxy,
			  subscriptions = [],
			  read_handles = []},
    insert_tbl(safs_ntf_user, User),
    erlang:monitor(process, Proxy),
    Reply = {ok, Handle, Version},
    {reply, Reply, S};
handle_call({callbacks_initialize, Handle, CbProxy}, _From, State) ->
    try
        User = lookup_tbl(safs_ntf_user, Handle),
        insert_tbl(safs_ntf_user, User#safs_ntf_user{cb_proxy = CbProxy}),
        erlang:monitor(process, CbProxy),
        {reply, ok, State}
    catch
        throw:Term -> {reply, Term, State}
    end;
handle_call({finalize, Handle}, _From, S) ->
    Reply =
	try
	    delete_user(Handle),
	    ok
	catch
	    throw:Term -> Term
	end,

    {reply, Reply, S};
handle_call({notification_send, Handle, Notification}, _From, S) ->
    Reply =
	try
	    lookup_tbl(safs_ntf_user, Handle),
	    Id = safs_ntf_db:create_notification_id(),
	    EventTime = get_event_time(Notification),
	    SentNotification =
		#ntf_sent_notification{time_id = {EventTime, Id},
				       notification = Notification},
	    safs_ntf_db:insert_sent_notification(SentNotification),
	    SubscrList = safs_ntf_subscriber:get_subscribers(Notification),
	    exec_notification_callbacks(Notification, SubscrList),

	    {ok, Id}
	catch
	    throw:Term -> Term
	end,

    {reply, Reply, S};
handle_call({notification_subscribe, Handle, NotificationTypeFilters,
	     SubscriptionId}, _From, S) ->
    Reply =
	try
	    User = lookup_tbl(safs_ntf_user, Handle),

	    Id = {Handle, SubscriptionId},
	    case ets:lookup(safs_ntf_subscriber, Id) of
		[] -> ok;
		_ -> throw({error, sa_ais_err_exist})
	    end,

	    Subscriber = #safs_ntf_subscriber{id = Id,
					      notificationtype_filters = NotificationTypeFilters},
	    insert_tbl(safs_ntf_subscriber, Subscriber),

	    NewSubscriptions = add_subscription_id(SubscriptionId,
						   User#safs_ntf_user.subscriptions),
	    NewUser = User#safs_ntf_user{subscriptions = NewSubscriptions},
	    insert_tbl(safs_ntf_user, NewUser),

	    ok
	catch
	    throw:Term -> Term
	end,

    {reply, Reply, S};
handle_call({notification_unsubscribe, Handle, SubscriptionId}, _From, S) ->
    Reply =
	try
	    User = lookup_tbl(safs_ntf_user, Handle),

	    Id = {Handle, SubscriptionId},
	    case ets:lookup(safs_ntf_subscriber, Id) of
		[] -> throw({error, sa_ais_err_not_exist});
		_ -> ok
	    end,
	    delete_tbl(safs_ntf_subscriber, Id),

	    NewSubscriptions = delete_subscription_id(SubscriptionId,
						      User#safs_ntf_user.subscriptions),
	    NewUser = User#safs_ntf_user{subscriptions = NewSubscriptions},
	    insert_tbl(safs_ntf_user, NewUser),

	    ok
	catch
	    throw:Term -> Term
	end,
    {reply, Reply, S};
handle_call({notification_read_initialize, Handle,
	     SearchCriteria, NotificationTypeFilters}, _From,
	    S) ->
    Reply =
	try
	    User = lookup_tbl(safs_ntf_user, Handle),

	    {ok, Pid} = safs_ntf_reader:start_link(),
	    ReadHandle = create_reader_handle(),
	    Reader = #safs_ntf_reader{read_handle = ReadHandle,
				      pid = Pid,
				      handle = User#safs_ntf_user.handle,
				      search_criteria = SearchCriteria,
				      notification_type_filters = NotificationTypeFilters},
	    insert_tbl(safs_ntf_reader, Reader),

	    NewReadHandlers =
		add_read_handle(ReadHandle, User#safs_ntf_user.read_handles),
	    NewUser = User#safs_ntf_user{read_handles = NewReadHandlers},
	    insert_tbl(safs_ntf_user, NewUser),

	    ok = safs_ntf_reader:filter(Pid, ReadHandle),

	    {ok, ReadHandle}
	catch
	    throw:Term -> Term
	end,

    {reply, Reply, S};
handle_call({notification_read_next, ReadHandle, SearchDirection},
	    _From, S) ->
    Reply =
	try
	    Reader = lookup_tbl(safs_ntf_reader, ReadHandle),
	    safs_ntf_reader:get_next(Reader#safs_ntf_reader.pid,
				     SearchDirection)
	catch
	    throw:Term -> Term
	end,

    {reply, Reply, S};
handle_call({notification_read_finalize, ReadHandle}, _From, S) ->
    Reply =
	try
	    Reader = lookup_tbl(safs_ntf_reader, ReadHandle),
	    User = lookup_tbl(safs_ntf_user, Reader#safs_ntf_reader.handle),
	    safs_ntf_reader:stop(Reader#safs_ntf_reader.pid),

	    delete_tbl(safs_ntf_reader, ReadHandle),

	    NewReadHandlers =
		delete_read_handle(ReadHandle, User#safs_ntf_user.read_handles),
	    NewUser = User#safs_ntf_user{read_handles = NewReadHandlers},
	    insert_tbl(safs_ntf_user, NewUser),

	    ok
	catch
	    throw:Term -> Term
	end,

    {reply, Reply, S};
handle_call(Msg, _From, S) ->
    error_logger:format("~p ~p got unexpected call:\n\t~p\n",
                        [?MODULE, self(), Msg]),
    {noreply, S}.

%%----------------------------------------------------------------------
%% Function: handle_cast(Msg, State) ->
%%     {noreply, State} |
%%     {noreply, State, Timeout} |
%%     {stop, Reason, State}
%% Description: Handling cast messages
%% @private
%%----------------------------------------------------------------------
handle_cast(Msg, S) when is_record(S, state) ->
    error_logger:format("~p~p got unexpected cast:\n\t~p\n",
			[?MODULE, self(), Msg]),
    {noreply, S}.

%%----------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @private
%%----------------------------------------------------------------------
handle_info({'DOWN', _Ref, _Type, Pid, _Reason}, State) ->
    handle_user_down(Pid, State),
    {noreply, State};
handle_info({'EXIT', Parent, Reason}, #state{parent = Parent} = S) ->
    {stop, Reason, S};
handle_info(Msg, #state{} = S) ->
    error_logger:format("~p~p got unexpected message:\n\t~p\n",
                        [?MODULE, self(), Msg]),
    {noreply, S}.

%%----------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @private
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%% @private
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

create_handle() ->
    ets:update_counter(safs_ntf_id, handle, 1).

create_reader_handle() ->
    ets:update_counter(safs_ntf_id, readHandle, 1).

insert_tbl(Table, Obj) ->
    ets:insert(Table, Obj).

lookup_tbl(Table, Handle) ->
    case ets:lookup(Table, Handle) of
        [] ->
            throw({error, sa_ais_err_bad_handle});
        [Obj] ->
            Obj
    end.

delete_tbl(Table, Handle) ->
    ets:delete(Table, Handle).

add_subscription_id(SubscriptionId, Subscriptions) ->
    [SubscriptionId|Subscriptions].

delete_subscription_id(SubscriptionId, Subscriptions) ->
    lists:delete(SubscriptionId, Subscriptions).

add_read_handle(ReadHandle, Readers) ->
    [ReadHandle|Readers].

delete_read_handle(ReadHandle, Readers) ->
    lists:delete(ReadHandle, Readers).

get_event_time(Notification) ->
    NotificationHeader = element(2, Notification),
    NotificationHeader#safsNtfNotificationHeader.eventTime.

%%----------------------------------------------------------------------
%% Callbacks
%%----------------------------------------------------------------------
%% @private
handle_callback(User, CBA) ->
    try User of
	#safs_ntf_user{cb_proxy = CbProxy} when CbProxy =/= undefined ->
	    handle_reg_callback(User, CBA);
	_ ->
	    %% Handling of erlang callbacks
	    spawn_link(fun() ->
			       handle_erlang_callback(User, CBA),
			       exit(normal)
		       end),
	    noreply
    catch
        throw:Term -> {reply, Term}
    end.

%%----------------------------------------------------------------------
%% Handling of erlang callbacks
handle_erlang_callback(User, {Cb, Args}) ->
    ModOrFun = is_registered_cb(Cb, User#safs_ntf_user.callbacks),

    case catch execute_cb(ModOrFun, Cb, tuple_to_list(Args)) of
	ok ->
	    ok;
	{error, ErrorVal} ->
	    {error, ErrorVal};
	_Error ->
	    {error, sa_ais_err_failed_operation}
    end.

execute_cb(undefined, _F, _Args) ->
    ok;
execute_cb(false, _F, _Args)  ->
    ok;
execute_cb(ModOrFun, F, Args) when is_function(ModOrFun, 1) ->
    ModOrFun(list_to_tuple([F|Args]));
execute_cb(ModOrFun, F, Args) when is_atom(ModOrFun) ->
    apply(ModOrFun, F, Args).

%%----------------------------------------------------------------------
%% Handle C callbacks
%% @private
handle_reg_callback(User, {Cb, Arg}) ->
    case is_registered_cb(Cb, User#safs_ntf_user.callbacks) of
        true ->
            Pid = User#safs_ntf_user.cb_proxy,
            send_callback(Pid, Cb, Arg);
        _False ->
            {reply, ok}
    end.

%% @private
send_callback(Pid, Cb, Arg) ->
    safs_ntf_com:callback_message_async(Pid, Cb, Arg).

%%----------------------------------------------------------------------
%% @private
is_registered_cb(_, Cb) when is_atom(Cb);
			     is_function(Cb, 1) ->
    Cb;
is_registered_cb(notification_callback, Cb) ->
    Cb#safsNtfCallbacks.saNtfNotificationCallback.
%% is_registered_cb(notification_discarded_callback, Cb) ->
%%     Cb#safsNtfCallbacks.saNtfNotificationDiscardedCallback.

exec_notification_callbacks(Notification, SubscriberList) ->
    lists:foreach(
      fun({Handle, SubscriptionId}) ->
	      User = lookup_tbl(safs_ntf_user, Handle),
	      handle_callback(User,
			      {notification_callback,
			       {SubscriptionId, Notification}})
      end, SubscriberList).

delete_user(Handle) when is_integer(Handle) ->
    User = lookup_tbl(safs_ntf_user, Handle),
    delete_user(User);
delete_user(User) when is_record(User, safs_ntf_user) ->
    Handle = User#safs_ntf_user.handle,
    delete_tbl(safs_ntf_user, Handle),
    lists:foreach(fun(SubscriptionId) ->
			  delete_tbl(safs_ntf_subscriber,
				     {Handle, SubscriptionId})
		  end, User#safs_ntf_user.subscriptions),

    lists:foreach(fun(ReadHandle) ->
			  Reader = lookup_tbl(safs_ntf_reader,
					      ReadHandle),
			  safs_ntf_reader:stop(Reader#safs_ntf_reader.pid),
			  delete_tbl(safs_ntf_reader, ReadHandle)
		  end, User#safs_ntf_user.read_handles),
    ok.

%% @private
handle_user_down(Pid, _State) ->
    MS = ets:fun2ms(fun(#safs_ntf_user{proxy = Proxy,
				       cb_proxy = CbProxy} = User) when
			      Proxy =:= Pid;
			      CbProxy =:= Pid ->
			    User
		    end),

    lists:foreach(
      fun(User) ->
	      if
		  User#safs_ntf_user.proxy =:= Pid ->
		      safs_ntf_com:close(User#safs_ntf_user.cb_proxy);
		  User#safs_ntf_user.cb_proxy =:= Pid ->
		      safs_ntf_com:close(User#safs_ntf_user.proxy)
	      end,
	      delete_user(User)
      end, ets:select(safs_ntf_user, MS)),

    ok.
