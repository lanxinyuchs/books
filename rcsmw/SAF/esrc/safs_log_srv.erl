%%--------------------------------------------------------------------
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
%% File: safs_log_srv.erl
%%
%% Description:
%%
%%
%%--------------------------------------------------------------------
-module(safs_log_srv).
-behaviour(gen_server).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------

-include("safs_log_db.hrl").
-include("safs_log.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([start_link/0]).

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
%% Test exports
%%----------------------------------------------------------------------
-export([tables/0]).
-export([tables/1]).

-export([get_seq_no/1]).
-export([loop_data/0]).
-export([loop_data/1]).
-export([stream_pids/0]).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

-define(PREDEF_LOG, true).
-define(APP_LOG,    false).

-define(SYS_LOGS_HANDLE,  0).
-define(SYS_LOGS_TIMEOUT, 0).

%% Timeout after possible filter callback is invoked
-define(FILTER_CB_TO, 1000).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(server, {parent,
		 oi_rt_handle,
		 oi_cfg_handle,
		 stream_pids = [],    %% [{StreamName, Pid}]
		 %% currently open streams
		 active_ccb = [],     %% [{CcbId, Pid}]
		 ets_tabs}).          %% see safs_log_db for details

%%======================================================================
%% External Interface Functions
%%======================================================================

%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self()], []).

%%======================================================================
%% Test Functions
%%======================================================================
tables() ->
    tables(all).

tables(T) ->
    gen_server:cast(?MODULE, {tables, T}).

get_seq_no(Proc) ->
    gen_server:call(?MODULE, {get_seq_no, Proc}).

loop_data() ->
    gen_server:call(?MODULE, {loop_data, all}).

loop_data(Proc) ->
    gen_server:call(?MODULE, {loop_data, Proc}).

stream_pids() ->
    gen_server:call(?MODULE, get_stream_pids).


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
    process_flag(trap_exit, true), %% to receive terminate at shutdown
    safs_log_db:init_mnesia(),
    State = #server{parent   = Parent,
		    ets_tabs = safs_log_db:init_ets()},
    case init_imm_and_predef_logs(State) of
	{ok, NewState} -> {ok, NewState};
	Error          -> {stop, Error}
    end.


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

%%========================================================================
%% initialize
%% 
%% Init function for LOG service
%% 
%% Creates a handle that the users should use when opening a stream.
%%========================================================================
handle_call({initialize, {Callbacks, Version, Proxy}},
	    _From,
	    #server{ets_tabs = Tabs} = S) ->
    Handle = safs_log_db:handle_create(Tabs),
    User   = #safs_log_user{handle    = Handle,
			    version   = Version,
			    callbacks = Callbacks,
			    proxy     = Proxy},
    safs_log_db:log_user_insert(Tabs, User),
    erlang:monitor(process, Proxy),
    Reply = {ok, Handle, Version},
    {reply, Reply, S};

%%========================================================================
%% callbacks_initialize
%% 
%% Initializes the socket proxy for callback functions.
%%========================================================================
handle_call({callbacks_initialize, Handle, CbProxy}, 
	    _From, 
	    #server{ets_tabs = Tabs} = State) ->
    Res = cb_init(safs_log_db:log_user_get(Tabs, Handle), CbProxy, State),
    {reply, Res, State};

%%========================================================================
%% finalize
%% 
%% Ends a log session. The handle is not to be used any more.
%%========================================================================
handle_call({finalize, Handle}, 
	    _From, 
	    State) ->
    {Reply, NewState} = finalize(Handle, State),
    {reply, Reply, NewState};

%%========================================================================
%% stream_open
%% 
%% Opens a stream for writing.
%% Returns a StreamHandle that the user should be used when logging
%% and when closing the stream.
%% Several users can use the same stream, but they will have individual
%% StreamHandles. This, for LOG service, to know when the last user
%% closes the stream.
%%========================================================================
handle_call({stream_open_2, Params}, _From, State) ->
    case stream_open(Params, State) of
	{ok, StreamHandle, NewState} ->
	    {reply, {ok, StreamHandle}, NewState};
	Error ->
	    log(stream_open_2, Error),
	    {reply, Error, State}
    end;

%%========================================================================
%% write_log_async
%% 
%% Asynchroneaously write a log entry.
%% The user will be informed with a callback function of the result.
%%========================================================================
handle_call({write_log_async, Params}, _From, S) ->
    Res = write_log_async(Params, S),
    {reply, Res, S};

%%========================================================================
%% stream_close
%% 
%% Closes the stream for the StreamHandle.
%% Several users can use the same stream, when the last StreamHandle
%% is removed the file is closed for further writing.
%%========================================================================
handle_call({stream_close, StreamHandle},
	    _From,
	    State) ->
    NewState = stream_close(true, StreamHandle, State),
    {reply, ok, NewState};

%%========================================================================
%% delete_files
%% 
%% This function will delete all files with FileName in the
%% requested directory.
%%========================================================================
handle_call({delete_files, {FileName, FilePathName}},
	    _From,
	    State) ->
    case is_open(FileName, FilePathName, State) of
	false ->
	    safs_log_file:delete_files(FileName, FilePathName),
	    {reply, ok, State};
	true ->
	    {reply, {error, ?SA_ERR_BUSY}, State}
    end;

%%========================================================================
%% limit_get
%% 
%% This function is to find different limits in the system.
%%========================================================================
handle_call({limit_get, {_Handle, ?LIMIT_MAX_APP_STREAMS}},
	    _From,
	    State) ->
    {reply, {ok, ?MAX_NUM_APP_LOG_STREAMS}, State};
handle_call({limit_get, {_Handle, _}},
	    _From,
	    State) ->
    {reply, {error, ?SA_ERR_INVALID_PARAM}, State};


%%========================================================================
%% stream_reopen_file
%% 
%% This function is to reopen a file without changing StreamHandle
%%========================================================================
handle_call({stream_reopen_file, {StreamHandle}},
	    _From,
	    State) ->
    stream_reopen_file(StreamHandle, State),
    {reply, ok, State};

%%========================================================================
%% change_filter
%% 
%% Update the filter settings for a stream
%%========================================================================
handle_call({change_filter, StreamName, _NewFilters},
	    _From,
	    State) when StreamName == ?LOG_ALARM;
			StreamName == ?LOG_NOTIFY ->
    {reply, {error, ?SA_ERR_NOT_SUPPORTED}, State};
handle_call({change_filter, StreamName, NewFilters},
	    _From,
	    #server{stream_pids = Pids, ets_tabs = Tids} = State) ->
    StreamPid = proplists:get_value(StreamName, Pids),
    Res = change_filter(StreamPid, NewFilters),
    Res =:= ok andalso call_filter_cb(StreamName, StreamPid, Tids),
    {reply, Res, State};


handle_call({filter_modify, _CcbId, StreamName, _NewFilters},
	    _From,
	    State) when StreamName == ?LOG_ALARM;
			StreamName == ?LOG_NOTIFY ->
    {reply, {error, ?SA_ERR_NOT_SUPPORTED}, State};
handle_call({filter_modify, CcbId, StreamName, NewFilters},
	    _From,
	    #server{stream_pids = Pids, active_ccb = CcbIds} = State) ->
    StreamPid = proplists:get_value(StreamName, Pids),
    case filter_modify(StreamPid, NewFilters) of
	ok ->
	    NewCcbIds = 
		[{CcbId, StreamName} | lists:keydelete(StreamName, 2, CcbIds)],
	    {reply, ok, State#server{active_ccb = NewCcbIds}};
	Error ->
	    {reply, Error, State}
    end;

handle_call({filter_apply, CcbId},
	    _From,
	    #server{active_ccb = CcbIds, 
		    stream_pids = Pids, 
		    ets_tabs = Tids} = State) ->
    StreamName = proplists:get_value(CcbId, CcbIds),
    StreamPid = proplists:get_value(StreamName, Pids),
    filter_apply(StreamPid),
    call_filter_cb(StreamName, StreamPid, Tids),
    ActiveCcb = lists:keydelete(CcbId, 1, CcbIds),
    {reply, ok, State#server{active_ccb = ActiveCcb}};

handle_call({filter_abort, CcbId},
	    _From,
	    #server{active_ccb = CcbIds, 
		    stream_pids = Pids} = State) ->
    StreamName = proplists:get_value(CcbId, CcbIds),
    StreamPid = proplists:get_value(StreamName, Pids),
    filter_abort(StreamPid),
    ActiveCcb = lists:keydelete(CcbId, 1, CcbIds),
    {reply, ok, State#server{active_ccb = ActiveCcb}};


%%========================================================================
%% Get num of openers
%%========================================================================
handle_call({get_num_openers, StreamName}, _From, S) ->
    Handles = safs_log_db:sn2h_get(S#server.ets_tabs, StreamName),
    {reply, {ok, length(Handles)}, S};

%%========================================================================
%% For testing
%%========================================================================
handle_call(get_stream_pids, _From, #server{stream_pids = Pids} = S) ->
    {reply, Pids, S};
handle_call({loop_data, srv}, _From, S) ->
    io:format("===== SERVER loop data =====~n~p ~n", [S]),
    {reply, ok, S};
handle_call({loop_data, all}, _From, #server{stream_pids = Pids} = S) ->
    io:format("===== SERVER loop data =====~n~p ~n", [S]),
    [Pid ! loop_data || {_, Pid} <- Pids],
    {reply, ok, S};
handle_call({loop_data, Stream}, _From, #server{stream_pids = Pids} = S) ->
    io:format("===== SERVER loop data =====~n~p ~n", [S]),
    [Pid ! {loop_data, Stream} || {_, Pid} <- Pids],
    {reply, ok, S};
handle_call({get_seq_no, Stream}, _From, #server{stream_pids = Pids} = S) ->
    [Pid ! {get_seq_no, Stream, self()} || {_, Pid} <- Pids],
    Res = receive 
	      {get_seq_no_res, SeqNo} ->
		  SeqNo
	  after 2000 ->
		  timeout
	  end,
    {reply, Res, S};
handle_call(stop, _From, S) ->
    {stop, normal, ok, S};

%%========================================================================
%% Unknown message
%%========================================================================
handle_call(Msg, _From, S) ->
    error_logger:warning_msg("~p ~p got unexpected call:\n\t~p\n",
                        [?MODULE, self(), Msg]),
    {reply, {error, {unknown_msg, Msg}}, S}.

%%----------------------------------------------------------------------
%% Function: handle_cast(Msg, State) ->
%%     {noreply, State} |
%%     {noreply, State, Timeout} |
%%     {stop, Reason, State}
%% Description: Handling cast messages
%% @private
%%----------------------------------------------------------------------
%%========================================================================
%% Callbacks
%%========================================================================
handle_cast({awrite_res, CbFnc, ProxyPid, Res}, 
	    State) ->
    awrite_cb(CbFnc, ProxyPid, Res, State),
    {noreply, State};

%%========================================================================
%% For testing
%%========================================================================
handle_cast({tables, Tabs}, #server{ets_tabs = Tids} = S) ->
    print_tables(Tabs, Tids),
    {noreply, S};

%%========================================================================
%% Unknown message
%%========================================================================
handle_cast(Msg, S) when is_record(S, server) ->
    error_logger:warning_msg("~p~p got unexpected cast:\n\t~p\n",
			[?MODULE, self(), Msg]),
    {noreply, S}.

%%----------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @private
%%----------------------------------------------------------------------
%%========================================================================
%% Callbacks
%%========================================================================
handle_info({call_filter_cb, StreamPid, Handle}, 
	    #server{ets_tabs = Tabs} = State) ->
    safs_log_stream:call_filter_cb(StreamPid, 
				   safs_log_db:log_user_get(Tabs, Handle),
				   Handle),
    {noreply, State};
handle_info({'DOWN', _Ref, _Type, Pid, _Reason}, State) ->
    handle_user_down(Pid, State),
    {noreply, State};
handle_info({'EXIT', Parent, Reason}, #server{parent = Parent} = S) ->
    {stop, Reason, S};
%%========================================================================
%% Unknown message
%%========================================================================
handle_info({is_open, res, _}, #server{} = S) ->
    {noreply, S};
handle_info(Msg, #server{} = S) ->
    error_logger:warning_msg("~p~p got unexpected message:\n\t~p\n",
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
terminate(_Reason, #server{stream_pids = Pids} = State) ->
    terminate_all_streams(Pids, State),
    safs_log_oi_rt:finalize(State#server.oi_rt_handle),
    safs_log_oi_cfg:finalize(State#server.oi_cfg_handle),
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

%%========================================================================
%% stream_open({Handle, StreamName, CreateAttributes, OpenFlags, Timeout}) -> 
%%      {ok, StreamHandle, State} | {error, atom()}
%% 
%% For verification rules see the LOG Service documentation
%% chapter 3.6 Log Service Operations
%%========================================================================
stream_open({Handle, StreamName, CreateAttributes, OpenFlags, _} = Params,
	    #server{ets_tabs = Tabs} = State) ->
    so(safs_log_db:stream_config_noof(),
       so_verify(safs_log_db:log_user_get(Tabs, Handle),
		 Handle,
		 is_predef_log(StreamName),
		 stream_already_exists(safs_log_db:sn2h_get(Tabs, StreamName)),
		 safs_log_db:stream_config_get(StreamName),
		 {StreamName, CreateAttributes, OpenFlags}),
       Params,
       State).



%%===============================================================
%% so() -> so_rc()
%%===============================================================
%%---------------------------------------------------------
%% ok cases
%%---------------------------------------------------------
so(NoofOpenStreams,
   {Res, StreamConfig},
   {_Handle, StreamName, _CA, _OpenFlag, _Timeout} = Params,
   #server{ets_tabs    = Tabs,
	   oi_rt_handle = OiHandle,
	   stream_pids = StreamPids} = State)
  when (Res == new orelse Res == exists) andalso 
       NoofOpenStreams - ?NUM_PREDEF_STREAMS < ?MAX_NUM_APP_LOG_STREAMS ->
    %%--------------------------------------
    %% create stream config if new stream
    %%--------------------------------------
    case Res of
	new    -> safs_log_db:stream_config_write(StreamConfig);
	exists -> ok
    end,
    StartRes = start_stream(proplists:get_value(StreamName, StreamPids),
			    StreamPids,
			    Params,
			    OiHandle,
			    Tabs),
    so_rc(StartRes, Params, State);
%%---------------------------------------------------------
%% stream is ok but there are too many of them now
%%---------------------------------------------------------
so(NoofOpenStreams, _, Params, State)
  when NoofOpenStreams - ?NUM_PREDEF_STREAMS >= ?MAX_NUM_APP_LOG_STREAMS ->
    so_rc({error, ?SA_ERR_NO_RESOURCES}, Params, State);
%%---------------------------------------------------------
%% error
%%---------------------------------------------------------
so(_, Error, Params, State) ->
    so_rc(Error, Params, State).





%%===============================================================
%% so_rc()
%%===============================================================
%%---------------------------------------------------------
%% ok 
%%---------------------------------------------------------
so_rc({ok, StreamPids}, 
      {Handle, StreamName, _CA, _OpenFlags, _Timeout},
      #server{ets_tabs = Tabs} = State) ->
    StreamHandle = safs_log_db:stream_handle_create(Tabs),
    safs_log_db:sh2sn_insert(Tabs, StreamHandle, StreamName),
    safs_log_db:sn2h_add(Tabs, StreamName, {StreamHandle, Handle}),
    {ok, 
     StreamHandle,
     State#server{stream_pids = StreamPids}};
%%---------------------------------------------------------
%% the three well known streams shall never be deleted 
%%---------------------------------------------------------
so_rc(Error, {_, StreamName, _, _, _}, _) when StreamName == ?LOG_ALARM;
					       StreamName == ?LOG_SYSTEM;
					       StreamName == ?LOG_NOTIFY ->
    log(stream_open, Error),
    Error;
%%---------------------------------------------------------
%% delete app stream configuration if last stream
%%---------------------------------------------------------
so_rc(Error,
      {_, StreamName, _, _, _},
      #server{ets_tabs = Tabs}) ->
    log(stream_open, Error),
    so_error(safs_log_db:sn2h_get(Tabs, StreamName), StreamName),
    Error.


so_error([], StreamName) ->
    %% remove the stream configuration if no handle has it open
    safs_log_db:stream_config_delete(StreamName);
so_error(_Handles, _StreamName) ->
    ok.



%%====================================================================
%% so_verify(User, Handle, IsPredefLog, AlreadyExists,  
%%           OldStreamConfig, Params) -> 
%%     {new | exists, StreamConfig} | {error, atom()} 
%%
%% User            = [] } [#safs_log_user]
%% IsPredefLog     = boolean()
%% OldStreamConfig = [] | [#safs_log_stream_config{}]
%% Params          = {StreamName, CreateAttributes, OpenFlags}
%%
%% verify the open parameters
%% The comments are fetched from the open saf documentation for
%% LOG Service.
%% If result is 'exists' then the StreamConfig is already defined.
%%====================================================================
%%----------------------------------------------
%% Error when reading the StreamConfig record
%%----------------------------------------------
so_verify(_, _, _, _, {error, _} = Error, _) ->
    log(so_verify, {stream_config, Error}),
    {error, ?SA_ERR_LIBRARY};
%%----------------------------------------------
%% Handle
%%----------------------------------------------
%% Unknown
so_verify([], Handle, _, _, _, _)
  when Handle /= ?SYS_LOGS_HANDLE ->
    log(so_verify, {handle, user_not_found}),
    {error, ?SA_ERR_BAD_HANDLE};
%%----------------------------------------------
%% CreateAttributes
%%----------------------------------------------
%% If one of the well-known log streams is being
%% opened, this pointer must be NULL.
so_verify(_,
	  _, 
	  ?PREDEF_LOG,
	  _,
	  _, 
	  {_StreamName, CreateAttributes, _OpenFlags})
  when CreateAttributes /= undefined ->
    log(so_verify, {non_app, create_attributes}),
    {error, ?SA_ERR_INVALID_PARAM};
%% If the user intends only to open an existing application log stream by 
%% supplying the same log stream name, this value must be NULL
%%
%% If the user intends to open a (possibly) existing application log stream, 
%% but still specify creation attribute values, the provided values must be 
%% identical to those values provided by the initial logger that successfully 
%% created the application log stream.
so_verify(_,
	  _,
	  ?APP_LOG,
	  _,
	  {ok, [#safs_log_stream_config{create_attributes = OldCreateAttrs}]}, 
	  {_StreamName, NewCreateAttrs, _OpenFlags}) 
  when NewCreateAttrs /= undefined andalso
       NewCreateAttrs /= OldCreateAttrs ->
    log(so_verify, {app, create_attributes, created}),
    {error, ?SA_ERR_EXIST};
%% If the user intends to open and create an application log stream 
%% that does not yet exist, a CreateAttributes structure must be populated
so_verify(_,
	  _,
	  ?APP_LOG,
	  _,
	  {ok, []}, 
	  {_StreamName, undefined, _OpenFlags}) ->
    log(so_verify, {app, create_attributes, new}),
    {error, ?SA_ERR_INVALID_PARAM};
%%----------------------------------------------
%% OpenFlags
%%----------------------------------------------
%% The SA_LOG_STREAM_CREATE value may only be set when opening an application 
%% log stream. If one of the well-known log streams is being opened, this 
%% value must not be set.
so_verify(_,
	  _,
	  ?PREDEF_LOG,
	  _,
	  _, 
	  {_StreamName, _CreateAttributes, OpenFlags}) 
  when OpenFlags /= undefined ->
    log(so_verify, {non_app, open_flags, set}),
    {error, ?SA_ERR_INVALID_PARAM};
%% If the user intends only to open an existing application log stream by 
%% supplying the same log stream name, this value may not be set
so_verify(_,
	  _,
	  ?APP_LOG,
	  true,
	  {ok, [_]}, 
	  {_StreamName, CreateAttributes, OpenFlags}) 
  when ((OpenFlags /= undefined andalso CreateAttributes == undefined) orelse
	(OpenFlags == undefined andalso CreateAttributes /= undefined)) ->
    log(so_verify, {app, open_flags, set}),
    {error, ?SA_ERR_INVALID_PARAM};
%% If the user intends to open and create an application log stream that 
%% does not yet exist, the SA_LOG_STREAM_CREATE flag must be set.
so_verify(_,
	  _,
	  ?APP_LOG,
	  _,
	  {ok, []}, 
	  {_StreamName, _CreateAttributes, undefined}) ->
    log(so_verify, {app, open_flags, new}),
    {error, ?SA_ERR_INVALID_PARAM};
%%----------------------------------------------
%% Finally, the OK cases
%%----------------------------------------------
so_verify(_,
	  _,
	  _,
	  _,
	  {ok, []}, 
	  {StreamName, CreateAttributes, _OpenFlags}) ->
    Obj = #safs_log_stream_config{stream_name       = StreamName,
				  create_time       = os:timestamp(),
				  create_attributes = CreateAttributes},
    {new, Obj};
so_verify(_,
	  _,
	  _,
	  _,
	  {ok, [Obj]}, 
	  _) ->
    {exists, Obj};
%%----------------------------------------------
%% The ultimate error case
%%----------------------------------------------
so_verify(_, _, _, _, _, _) ->
    log(so_verify, ultimate),
    {error, ?SA_ERR_LIBRARY}.
    



%%======================================================================
%% start_stream(StreamPid, StreamPids, Params, EtsTabs) ->
%%  {ok, StreamPids} | {error, Reason}
%% 
%% Start a new stream process and create relevant ets and mnesia tables
%%======================================================================
%%------------------------------------------
%% New stream
%%------------------------------------------
start_stream(undefined, 
	     StreamPids, 
	     {Handle, StreamName, CA, _OpenFlag, _Timeout} = Params,
	     OiHandle,
	     Tabs) -> 
    sss_new(safs_log_stream:start(StreamName, 
				  CA, 
				  safs_log_db:log_user_get(Tabs, Handle),
				  OiHandle),
	    StreamPids,
	    Params,
	    Tabs);	
%%------------------------------------------
%% Already started
%%------------------------------------------
start_stream(StreamPid, 
	     StreamPids,
	     {_Handle, _StreamName, _CA, _OpenFlag, _Timeout} = Params, 
	     _OiHandle,
	     Tabs) ->
    sss_old(StreamPid,
	    StreamPids,
	    Params,
	    Tabs).


%%------------------------------------------
%% New stream
%%------------------------------------------
sss_new({ok, StreamPid}, 
	StreamPids, 
	{Handle, StreamName, _CA, _OpenFlag, _Timeout}, 
	Tabs) ->
    safs_log_db:h2sn_insert(Tabs, Handle, StreamName, StreamPid),
    timer:send_after(?FILTER_CB_TO, {call_filter_cb, StreamPid, Handle}),
    {ok, [{StreamName, StreamPid} | StreamPids]};
sss_new(Error, _, _, _) ->
    Error.

%%------------------------------------------
%% Already started
%%------------------------------------------
sss_old(StreamPid,
	StreamPids,
	{Handle, StreamName, _CA, _OpenFlag, _Timeout},
	Tabs) ->
    safs_log_db:h2sn_insert(Tabs, Handle, StreamName, StreamPid),
    timer:send_after(1000, {call_filter_cb, StreamPid, Handle}),
    {ok, StreamPids}.



%%========================================================================
%% write_log_async(Params, State) -> ok
%% 
%% write an async log message
%%========================================================================
write_log_async({StreamHandle, _, _, _} = Data, 
		#server{ets_tabs = Tabs} = State) ->
    wla(safs_log_db:sh2sn_get(Tabs, StreamHandle),
	Data,
	State).

wla({ok, StreamName},
    {StreamHandle, Invocation, AckFlags, LogRecord}, 
    #server{ets_tabs    = Tabs,
	    stream_pids = Pids}) ->
    Pid = proplists:get_value(StreamName, Pids),
    safs_log_stream:awrite(Pid,
			   StreamName,
			   LogRecord,
			   wla_get_user_data(StreamName, 
					     StreamHandle,
					     Invocation,
					     AckFlags,
					     Tabs));
wla(Error, _, _) ->
    Error.

wla_get_user_data(StreamName, StreamHandle, Invocation, AckFlags, Tabs) ->
    Handles = safs_log_db:sn2h_get(Tabs, StreamName),
    Handle  = proplists:get_value(StreamHandle, Handles),
    [#safs_log_user{cb_proxy  = ProxyPid,
		    callbacks = 
		    #safsLogCallbacks{saLogWriteLogCallback = CB}}] =
	safs_log_db:log_user_get(Tabs, Handle),
    {ProxyPid, CB, Invocation, AckFlags}.


%%========================================================================
%% stream_close(Close, StreamHandle, State) -> State
%% 
%% For verification rules see the LOG Service documentation
%% chapter 3.6 Log Service Operations
%%========================================================================
stream_close(Close, StreamHandle, #server{ets_tabs = Tabs} = State) ->
    sc_sn(safs_log_db:sh2sn_get(Tabs, StreamHandle),
	  Close,
	  StreamHandle, 
	  State).

sc_sn({ok, StreamName}, 
      Close, 
      StreamHandle, 
      #server{ets_tabs = Tabs} = State) ->
    Handles = safs_log_db:sn2h_get(Tabs, StreamName),
    Handle  = proplists:get_value(StreamHandle, Handles),
    close_stream(Handle, StreamName, StreamHandle, Close, State);
sc_sn(Error, _, _, _) ->
    Error.

%%========================================================================
%% finalize(Handle, State) -> {Reply, State}
%% 
%% close all open streams if no more handles have them open
%%========================================================================
finalize(Handle, #server{ets_tabs = Tabs} = State) ->
    NewState = close_open_streams(safs_log_db:h2sn_get(Tabs, Handle), 
				  Handle, 
				  State),
    {delete_user(safs_log_db:log_user_get(Tabs, Handle), Handle, State),
     NewState}.


%%========================================================================
%% stream_reopen_file(Params, State) -> ok
%% 
%% close and reopen a stream file without changing StreamHandle
%%========================================================================
stream_reopen_file(StreamHandle, 
		   #server{ets_tabs = Tabs} = State) ->
    srf(safs_log_db:sh2sn_get(Tabs, StreamHandle),
	State).

srf({ok, StreamName},
    #server{stream_pids = Pids}) ->
    Pid = proplists:get_value(StreamName, Pids),
    safs_log_stream:stream_reopen_file(Pid, StreamName);
srf(Error, _) ->
    Error.


%%========================================================================
%% close_open_streams([#safs_log_handle_2_stream_name], Handle, State) ->]
%%    State
%% 
%% Close all open streams if this was the last handle to have them opened.
%%========================================================================
close_open_streams([], _, State) ->
    State;
close_open_streams([{StreamName, _Pid} | T], 
		   Handle, 
		   #server{ets_tabs = Tabs} = State) ->
    Handles  = safs_log_db:sn2h_get(Tabs, StreamName), 
    NewState = cos(Handles, StreamName, Handle, State),
    close_open_streams(T, Handle, NewState).


cos([], _, _, State) ->
    State;
cos([{StreamHandle, Handle} | T], StreamName, Handle, State) ->
    NewState = close_stream(Handle, StreamName, StreamHandle, false, State),
    cos(T, StreamName, Handle, NewState);
cos([_ | T], StreamName, Handle, State) ->
    cos(T, StreamName, Handle, State).



%%========================================================================
%% close_stream(Handle, StreamName, StreamHandle, Close, State) -> State
%% 
%% Close a stream
%%========================================================================
close_stream(Handle, 
	     StreamName,
	     StreamHandle,
	     Close,
	     #server{ets_tabs = Tabs} = State) ->
    safs_log_db:h2sn_delete(Tabs, Handle, StreamName),
    safs_log_db:sh2sn_delete(Tabs, StreamHandle),
    cs_rc(safs_log_db:sn2h_delete(Tabs, StreamName, {StreamHandle, Handle}),
	  StreamName,
	  Close,
	  State).

cs_rc([], 
      StreamName,
      Close,
      #server{stream_pids = Pids} = State) ->
    Pid = proplists:get_value(StreamName, Pids),
    safs_log_stream:close(Pid, Close),
    safs_log_db:stream_config_delete(StreamName),
    State#server{stream_pids = proplists:delete(StreamName, Pids)};
cs_rc(_, _, _, State) ->
    State.



%%========================================================================
%% init_imm_and_predef_logs(State)
%% 
%% Initialize handles towards IMM and then start predef logs.
%%========================================================================
init_imm_and_predef_logs(State) ->
    case init_imm_cb(State) of
	{ok, NewState} ->    
	    init_predef_logs(NewState);
	Error ->
	    Error
    end.


init_imm_cb(State) ->
    case safs_log_oi_rt:initialize() of
	{ok, OiRtHandle} ->
	    init_imm_cb(OiRtHandle, State);
	Error ->
	    Error
    end.


init_imm_cb(OiRtHandle, State) ->
    case safs_log_oi_cfg:initialize() of
	{ok, OiCfgHandle} ->
	    {ok, State#server{oi_rt_handle = OiRtHandle,
			      oi_cfg_handle = OiCfgHandle}};
	Error ->
	    safs_log_oi_rt:finalize(OiRtHandle),
	    Error
    end.
    
%%========================================================================
%% init_predef_logs(State)
%% 
%% Check if stream is one of the three well known streams
%%========================================================================
init_predef_logs(State) ->
    case ipl([{?LOG_SYSTEM, ?LOG_SYSTEM_FORMAT}, 
	      {?LOG_NOTIFY, ?LOG_NOTIFY_FORMAT},
	      {?LOG_ALARM,  ?LOG_ALARM_FORMAT}],
	     State) of
	{ok, _S} = Res ->
	    Res;
	Error ->
	    safs_log_oi_rt:finalize(State#server.oi_rt_handle),
	    safs_log_oi_cfg:finalize(State#server.oi_cfg_handle),
	    Error
    end.


ipl([], State) ->
    {ok, State};
ipl([{StreamName, _FileFormat} | T], State) ->
    Params = {?SYS_LOGS_HANDLE, 
	      StreamName, 
	      undefined, 
	      undefined, 
	      ?SYS_LOGS_TIMEOUT},
    case stream_open(Params, State) of
	{ok, _StreamHandle, NewState} ->
	    safs_log_oi_cfg:set_creation_timestamp(State#server.oi_cfg_handle,
						   StreamName),
	    ipl(T, NewState);
	Error ->
	    log(init_system_logs, Error),
	    Error
    end.


%%========================================================================
%% is_predef_log(StreamName) -> boolean()
%% 
%% Check if stream is one of the three well known streams
%%========================================================================
is_predef_log(StreamName) ->
    StreamName == ?LOG_SYSTEM orelse
	StreamName == ?LOG_NOTIFY orelse
	StreamName == ?LOG_ALARM.


%%========================================================================
%% stream_already_exists([#safs_log_stream_name_2_handles{}]) -> boolean()
%% 
%% Check if stream already exists
%%========================================================================
stream_already_exists([]) -> false;
stream_already_exists(_)  -> true.
    


%%========================================================================
%% delete_user(Handle, State) -> ok
%% 
%% Delete user data
%%========================================================================
delete_user([User], _, State) ->
    du(User, State);
delete_user(User, Handle, _) ->
    log(delete_user, {User, Handle}),
    {error, ?SA_ERR_BAD_HANDLE}.

du(#safs_log_user{handle = Handle},
   #server{ets_tabs = Tabs}) ->
    safs_log_db:log_user_delete(Tabs, Handle),
    ok.


%%========================================================================
%% A monitored process is down.
%%========================================================================
handle_user_down(_Pid, _State) ->
    %% TODO: take care of DOWN msg
    ok.


%%========================================================================
%% cb_init(User, CbProxy, State) -> ok | {error, reason}
%% 
%% User initialtes the callback proxy process, stored in user data
%%========================================================================
cb_init([], _, _) ->
    {error, ?SA_ERR_LIBRARY};
cb_init([User], CbProxy, #server{ets_tabs = Tabs}) ->
    safs_log_db:log_user_insert(Tabs, User#safs_log_user{cb_proxy = CbProxy}),
    erlang:monitor(process, CbProxy),
    ok.


%%======================================================================
%% awrite_cb(CbFnc, ProxyPid, Res, State) -> ok | {error, Reason}
%% 
%% 
%%======================================================================
awrite_cb(CbFnc, ProxyPid, Res, _) ->
    aw_cb(ProxyPid, CbFnc, Res).

%%-------------------------------------------------------
%% C-interface call and no callback proxy process
%%-------------------------------------------------------
aw_cb(undefined, 
      {safs_log_com, callback_message_async}, 
      _Res) ->
    ok;
%%-------------------------------------------------------
%% Erlang interface and no callback proxy process
%%-------------------------------------------------------
aw_cb(undefined = Pid, {M,F}, Res) ->
    %% TODO: spawn a process
    M:F(Pid, write_log, Res);
%%-------------------------------------------------------
%% C or Erlang interface
%%-------------------------------------------------------
aw_cb(Pid, {M,F}, Res) ->
    M:F(Pid, write_log, Res);
%%-------------------------------------------------------
%% Erlang interface
%%-------------------------------------------------------
aw_cb(Pid, Fun, Res) when is_function(Fun) ->
    Fun({write_log, Res, Pid}).
    

%%======================================================================
%% change_filter(StreamName, NewFilters) -> ok | {error, atom()}
%%
%% 
%%======================================================================
change_filter(undefined, _NewFilters) ->
    {error, ?SA_ERR_NOT_EXIST};
change_filter(Pid, NewFilters) ->
    safs_log_stream:change_filter(Pid, NewFilters).

%%======================================================================
%% filter_modify(StreamPid, NewFilters) -> ok | {error, atom()}
%%
%% 
%%======================================================================
filter_modify(undefined, _NewFilters) ->
    {error, ?SA_ERR_NOT_EXIST};
filter_modify(Pid, NewFilters) ->
    safs_log_stream:filter_modify(Pid, NewFilters).

%%======================================================================
%% filter_apply(StreamPid) -> ok
%%
%% 
%%======================================================================
filter_apply(undefined) ->
    ok;
filter_apply(Pid) ->
    safs_log_stream:filter_apply(Pid).

%%======================================================================
%% filter_abort(StreamPid) -> ok
%%
%% 
%%======================================================================
filter_abort(undefined) ->
    ok;
filter_abort(Pid) ->
    safs_log_stream:filter_abort(Pid).

%%======================================================================
%% call_filter_cb(StreamName, StreamPid, Tids) -> void()
%%
%% 
%%======================================================================
call_filter_cb(StreamName, StreamPid, _Tids) 
  when StreamName =:= undefined; StreamPid =:= undefined ->
    ok;

call_filter_cb(StreamName, StreamPid, Tids) ->
    lists:foreach(
      fun({StreamHandle, Handle}) ->
	      User = safs_log_db:log_user_get(Tids, StreamHandle),
	      safs_log_stream:call_filter_cb(StreamPid, User, Handle)
      end, safs_log_db:sn2h_get(Tids, StreamName)).

%%======================================================================
%% terminate_all_streams(Pids, State) -> ok
%%
%% terminate all streams due to terminate/2 was invoked
%%======================================================================
terminate_all_streams([], #server{ets_tabs = Tabs}) ->
    safs_log_db:log_user_delete_all_obj(Tabs),
    ok;
terminate_all_streams([{StreamName, Pid} | T], 
		      #server{ets_tabs = Tabs} = State) ->

    safs_log_stream:delete_rt_obj(Pid),

    Handles = safs_log_db:sn2h_get(Tabs, StreamName),
    
    [stream_close(false, StreamHandle, State) || {StreamHandle, _} <- Handles],
    terminate_all_streams(T, State).



is_open(FileName, FilePathName, #server{stream_pids = Pids}) ->
    [safs_log_stream:is_open(Pid, FileName, FilePathName) || {_, Pid} <- Pids],
    io_wait_res(length(Pids), false).

io_wait_res(No, Res) when No =< 0 ->
    Res;
io_wait_res(No, Res) ->
    receive
	{is_open, res, false} ->
	    io_wait_res(No - 1, Res);
	{is_open, res, true} ->
	    %% at least one stream has a file open in the directory
	    io_wait_res(No - 1, true)
    after 5000 ->
	    %% not all streams have responed, assume that at least one 
	    %% stream has an open stream using FileName and FilePathName
	    true 
    end.



%%======================================================================
%% Misc Functions
%%======================================================================

log(Fnc, Error) ->
    p(user, "### ERROR. ~p:~p ~p~n", [?MODULE, Fnc, Error]). 


print_tables(all, Tids) ->
    print_tables([config, id, user, sh2sn, h2sn, sn2h], Tids);
print_tables(Tabs, Tids) ->
    safs_log_db:print_tables(Tabs, Tids).


p(_, _, _) ->
    ok.
