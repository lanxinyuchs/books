%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
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
%%

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ftpesd_listener.erl %
%%% @author enekdav
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R8A/R9A/R12A/4
%%%
%%% ----------------------------------------------------------
-module(ftpesd_listener).
-behaviour(gen_server).
-vsn('/main/R8A/R9A/R12A/4').
-date('2017-12-06').
-author('enekdav').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% R8A/1    2016-11-18   ekurnik    Created
%%% R8A/4    2016-11-23   eivmiha    Changed data port opening, config update
%%% R8A/7    2016-12-07   ekurnik    Added delay before binding to new address
%%% R8A/8    2016-12-08   ekurnik    Added additional tracing and listen error handling
%%% R8A/9    2016-12-13   ekurnik    Changed default port variable name
%%% R8A/10   2016-12-19   ekurnik    Added trust_category parameter
%%% R9A/1    2017-01-23   ekurnik    Added linger option to sockets
%%% R9A/2    2017-01-27   ekurnik    Minor change
%%% R9A/6    2017-03-02   emarnek    Improved handling of acceptors
%%% R9A/7    2017-03-08   ekurnik    Removed {active, true} from listen DSock
%%% R9A/8    2017-03-24   ekurnik    Added retry for {error, eaddrnotavail} case
%%% R12A/1   2017-11-24   eivmiha    Added idle timer
%%% R12A/3   2017-11-28   emirbos    Added control connection port
%%% R12A/4   2017-12-06   enekdav    Added min and max data port functionality
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/1,
         config_update/2,
         event/3,
         stop/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, 
         handle_call/3,
         handle_cast/2,
	     terminate/2, 
         code_change/3, 
         handle_info/2
        
        ]).

-include("ftpesd.hrl").

-define(RETRY_TIMER, 2000).

%% list of acceptors, sessions
-record(state, {port,
                args,
                acceptors = [],
                sessions = [],
                timer,
                bucket = ?BUCKET_SIZE
                }).

-record(session, {pid,
                  data_port}).


%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
start_link(Args) ->
   Name = proplists:get_value(name, Args),
   gen_server:start_link({local, Name}, ?MODULE, Args, []).

%%% ----------------------------------------------------------
%%% #   event/2
%%% ----------------------------------------------------------
%%% @doc Function which is called when we start accepting on listen socket.
%%% It takes the Pid of the session and stores it in list of all sessions
%%% and deletes the acceptor.
-spec event(Listener :: atom() | pid(), What :: atom(), Info :: any()) -> ok.
%%% ----------------------------------------------------------
event(Listener, What, Info) when is_pid(Listener)->
    gen_server:call(Listener, {event, What, Info});

event(Listener, What, Info) when is_atom(Listener)->
    ListenerPid = get_listener_pid(Listener), 
    gen_server:call(ListenerPid, {event, What, Info}).

%%% ----------------------------------------------------------
%%% #   config_update/1
%%% ----------------------------------------------------------
%%% @doc Function which is called when we need to reconfigure listener 
%%%(when node credential, trust category,oam, etc. are changed).
%%%
-spec config_update(Listener :: atom() | pid(), Args :: list()) -> ok.
%%% ----------------------------------------------------------
config_update(Listener, Args) when is_pid(Listener)->
    gen_server:cast(Listener, {config_update, Args});

config_update(Listener, Args) when is_atom(Listener)->
    ListenerPid = get_listener_pid(Listener), 
    gen_server:cast(ListenerPid, {config_update, Args}).
   
stop() ->
   gen_server:cast(?MODULE, shutdown).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

start() ->
    self() ! start.

init(Args) ->
  case initialize(Args) of
  {ok, State} ->
      start();
  {retry, State} ->
      timer:apply_after(?RETRY_TIMER, ?MODULE, config_update, [self(), [{type, ip} | Args]]);
  {error, State} ->
      ok
  end,
      
  {ok, State}.


handle_call({event, accept, {Pid}}, _From, State) ->
    ?LOG("Acceptor ~p becomes a session~n", [Pid]),
    NewSession = find_session(Pid, State#state.acceptors),
    Sessions = NewSession ++ State#state.sessions,
    Acceptors = remove_session(Pid, State#state.acceptors),
    {ok, NewState} = maybe_new_acceptor(State#state{acceptors = Acceptors, 
                                                    sessions = Sessions,
                                                    timer = update_bucket_timer(State),
                                                    bucket = drain_bucket(State)}),
    {reply, ok, NewState}.

handle_cast({config_update, Args},  #state{args = {_Args, _SockArgs, LSock}} = State) ->
    Type = proplists:get_value(type, Args),
    case Type of 
        ip -> 
          close_socket(LSock),
          exit_all_sessions(State),
          exit_all_acceptors(State),
          
          %% wait for new IP interface to become available
          timer:sleep(1000),
          
          {ok, NewState} = 
            case initialize(Args) of
                {ok, S} ->
                    maybe_new_acceptor(S);
                {retry, S} ->
                    timer:apply_after(?RETRY_TIMER, ?MODULE, config_update, [self(), Args]),
                    {ok, S};
                {error, S} ->
                    {ok, S}
            end,
          {noreply, NewState};
        _Other -> 
             {ok, NewState} = exit_all_acceptors(State),
             {NewArgs, SockArgs} = get_arguments(Args), 
             NewState2 = NewState#state{port=proplists:get_value(port, Args), args = {NewArgs, SockArgs, LSock}},
             {ok, NewState3} = maybe_new_listener(NewState2, State),
             {ok, NewState4} = maybe_new_acceptor(NewState3),
             {noreply, NewState4}
    end;


handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(_Req, State) -> {noreply, State}.

terminate(_Reason, #state{args = { _Args, _SockArgs, LSock}, timer = Timer} = State) ->
    exit_all_acceptors(State),
    exit_all_sessions(State),
    close_socket(LSock),
    case Timer of 
        undefined -> ok;
        _  -> erlang:cancel_timer(Timer)
    end,
     ?LOG("Listener terminated~n"),
    ok.

handle_info(start, State) ->
    {ok, NewState} = maybe_new_acceptor(State),
    {noreply, NewState};

%% expected behaviour, do not handle 
handle_info({'EXIT', _Pid, killed}, State) ->
    ?LOG("Process was killed: ~p~n", [_Pid]),
    {noreply, State};


handle_info({'EXIT', Pid, _Reason}, State) ->
    AcceptorPidList = extract_session_pids(State#state.acceptors),
    case lists:member(Pid, AcceptorPidList) of
        true ->
            ListAcceptors = remove_session(Pid, State#state.acceptors),
            NewState = State#state{acceptors = ListAcceptors},
            {ok, NewState2} = maybe_new_acceptor(NewState),
            {noreply, NewState2};
         _ ->
            SessionPidList = extract_session_pids(State#state.sessions),
            case lists:member(Pid, SessionPidList) of
                true ->
                     ListSessions = remove_session(Pid, State#state.sessions),
                     NewState = State#state{sessions = ListSessions},
                     %% In case max sessions are reached and bucket is empty acceptor wont start automatically
                     %% Also in case no ports are available within specified range
                     %% Check this case and start new acceptor if its true
                     {ok, NewState2} = maybe_new_acceptor(NewState),
                     {noreply, NewState2};
                _-> 
                    ?LOG("Error: Pid ~p is not a session nor an acceptor~nAcceptors: ~p~nSessions: ~p~n", 
                         [Pid, State#state.acceptors, State#state.sessions]),
                    {noreply, State}
            end
     end;

handle_info(fill_bucket, #state{bucket = 0} = State) ->
    %%%bucket is empty run maybe_new_acceptors since acceptors have been closed
    Number_of_sessions = length(State#state.sessions),
    NewState =  State#state{bucket = fill_bucket(State, Number_of_sessions),
        timer = run_bucket_timer()},
    {ok, NewState2} = maybe_new_acceptor(NewState),
    {noreply, NewState2};

handle_info(fill_bucket, #state{bucket = Bucket} = State)
            when Bucket < ?BUCKET_SIZE ->
    %%%add to bucket and run timer until full
    {noreply, State#state{bucket = fill_bucket(State),
        timer = run_bucket_timer()}};

handle_info(fill_bucket, State) ->
    %%%bucket became full now - no more timer
    {noreply, State#state{bucket = fill_bucket(State),
        timer = undefined}};

handle_info(_Info, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #   initialize/1
%%% ----------------------------------------------------------
%%% @doc Function that initializes arguments for listen socket.
%%%
%%%
-spec initialize(Args :: list()) -> {ok, #state{}} | {stop, any()}.
%%% ----------------------------------------------------------
initialize(Args)->
    process_flag(trap_exit, true),
    Port = proplists:get_value(port, Args),
    {NewArgs, SockArgs} = get_arguments(Args), 
    start_listening(NewArgs, SockArgs, Port).


get_arguments(Args) ->
    BaseArgs = [binary, {packet, 0}, {active, true}, {linger, {true, ?LINGER_TIMEOUT}}],
    TlsOptions = proplists:get_value(tls_options, Args),
    IpOptions = proplists:get_value(ip_options, Args),
    TcatDn = proplists:get_value(trusted_category, Args),
    IdleTimer = proplists:get_value(idle_timer, Args),
    MinDataPort = proplists:get_value(minDataPort, Args),
    MaxDataPort = proplists:get_value(maxDataPort, Args),
    
    Args1 = BaseArgs ++ IpOptions,

    Args2 = Args1 ++ [{reuseaddr, true}],
    SockArgs = Args2 ++
    case proplists:lookup(fd, Args) of
        none   -> [];
        FdProp -> [FdProp]
    end,
    NewArgs = SockArgs ++ [{tls_options, TlsOptions}] ++ [{ip_options, IpOptions}] ++ [{trusted_category, TcatDn}] ++ [{idle_timer, IdleTimer}] ++ [{minDataPort, MinDataPort}] ++ [{maxDataPort, MaxDataPort}],
    {NewArgs, SockArgs}.

start_listening(NewArgs, SockArgs, Port) ->
    start_listening(NewArgs, SockArgs, Port, #state{}).

start_listening(NewArgs, SockArgs, Port, State) ->
    case gen_tcp:listen(Port, SockArgs)of 
    {ok, CLSock} ->
       {ok, State#state{port = Port, args ={NewArgs, SockArgs, CLSock}}};
    %% Special case when we get the address and namespace, but socket can't be opened yet
    {error, eaddrnotavail} ->
        sysInitI:warning_msg("~p: Listener not started for ~p~nReason: eaddrnotavail~n", 
                             [?MODULE, [{port, Port}] ++ SockArgs]),
        {retry, State#state{port = Port, args ={NewArgs, SockArgs, undefined}}};
    {error, Reason} -> 
        sysInitI:warning_msg("~p: Listener not started for ~p~nReason: ~p~n", [?MODULE, [{port, Port}] ++ SockArgs, Reason]),
        {error, State#state{port = Port, args ={NewArgs, SockArgs, undefined}}}
    end.
    
maybe_new_listener(#state{port = Port}=NewState, #state{port = Port}) ->
    {ok, NewState};
maybe_new_listener(#state{port = NewPort, args = {NewArgs, SockArgs,_}} = State, #state{port = _Port, args = {_,_, LSock}}) ->
    close_socket(LSock),
%%  in case we cannot start listener we will print warning and wait for next reconfiguration 

    {_, NewState2} = start_listening(NewArgs, SockArgs, NewPort, State),
    {ok, NewState2}.


%%% ----------------------------------------------------------
%%% #   maybe_new_acceptor/1
%%% ----------------------------------------------------------
%%% @doc Function which checks if number of session is >= than maximum number 
%%% of sessions. If number of sessions is greater than max, then exit all acceptors, 
%%% else start a new acceptor and save acceptors Pid in list of acceptors.
%%%
-spec maybe_new_acceptor(State :: #state{}) -> {ok, State :: #state{}}.
%%% ----------------------------------------------------------
maybe_new_acceptor(#state{args = { _, _, undefined}} = State) ->
    ?LOG("Warning: Unable to start new acceptor - listening socket is undefined~n"),
    {ok, State};

maybe_new_acceptor(#state{acceptors = Acceptors} = State) when Acceptors =/= [] ->
    ?LOG("Already accepting~n"),
    {ok, State};

maybe_new_acceptor(#state{sessions = Sessions} = State) 
  when length(Sessions) >= ?MAX_SESSIONS ->
    sysInitI:info_msg("~p: Maximum number of sessions (" ++ 
                          integer_to_list(?MAX_SESSIONS) ++ ") reached ~n", [?MODULE]),
    ftpesd_util:sec_log("-", "FTPES: Maximum(" ++ integer_to_list(?MAX_SESSIONS) ++ 
                        ") number of simultaneous sessions reached"),
    {ok, NewState} = exit_all_acceptors(State),
    {ok, NewState};

maybe_new_acceptor(#state{bucket = Bucket} = State) when Bucket =:= 0 ->
    sysInitI:info_msg("High connection rate, limiting"),
    ftpesd_util:sec_log("-", "FTPES: High connection rate - rate limit in effect"),
    {ok, NewState} = exit_all_acceptors(State),
    {ok, NewState};
    
maybe_new_acceptor(#state{args = {Args, SockArgs, LSock}, bucket = _Bucket} = State) ->
    DSockArgs = lists:keyreplace(active, 1, SockArgs, {active, false}), %% when true, upgrade to SSL fails sometimes
    MinDataPort = proplists:get_value(minDataPort, Args),
    MaxDataPort = proplists:get_value(maxDataPort, Args),
    UsedDataPorts = extract_session_ports(State#state.acceptors) ++ extract_session_ports(State#state.sessions),
    case find_free_data_port(lists:seq(MinDataPort, MaxDataPort), UsedDataPorts, DSockArgs) of
        {error, no_port} ->
            sysInitI:info_msg("~p: Acceptor not started, reason: no available data port in range~n", [?MODULE]),
            {ok, State};
        {ok, DSock} ->
            ?LOG("Chosen data port ~p~n", [DSock]),
            ?LOG("Inet port ~p~n", [inet:port(DSock)]),
            {ok, Acceptor} = ftpesd_ctrl_conn:start_link(LSock, DSock, Args, self()),
            gen_tcp:controlling_process(DSock, Acceptor),
            ?LOG("Spawned a new acceptor: ~p~n", [Acceptor]),
            %%Storing information about new acceptor, PID and port
            %%Getting port with inet:port because if data port is random ( when port range is 0 )
            %%We don't directly know which port OS chose
            NewAcceptor = #session{pid = Acceptor, data_port = inet:port(DSock)},
            Acceptors = [NewAcceptor | State#state.acceptors],
            NewState = State#state{acceptors = Acceptors},
            {ok, NewState}
    end.

extract_session_ports(List) ->
    [Member#session.data_port || Member <- List].

extract_session_pids(List) ->
    [Member#session.pid || Member <- List].

find_session(Pid, List) ->
    [Member || Member <- List, Member#session.pid =:= Pid].

remove_session(Pid, List) ->
    [Member || Member <- List, Member#session.pid =/= Pid].

find_free_data_port([], _, _) ->
    {error, no_port};

find_free_data_port([Port|Tail], UsedDataPorts, DSockArgs) ->
    case lists:member(Port, UsedDataPorts) of
        true -> find_free_data_port(Tail, UsedDataPorts, DSockArgs);
        false ->
            case gen_tcp:listen(Port, DSockArgs) of 
                {ok, DSock} -> {ok, DSock};
                {error, _} -> find_free_data_port(Tail, UsedDataPorts, DSockArgs)
            end
    end.

%%% ----------------------------------------------------------
%%% #   exit_all_acceptors/1
%%% ----------------------------------------------------------
%%% @doc Function used for sending exit singals to acceptor process.
-spec exit_all_acceptors(S :: #state{})-> {ok, S :: #state{}}. 
%%% ----------------------------------------------------------
exit_all_acceptors(S) ->
  exit_all(extract_session_pids(S#state.acceptors)),
  {ok, S#state{acceptors = []}}.

exit_all_sessions(S) ->
  exit_all(extract_session_pids(S#state.sessions)),
  {ok, S#state{sessions = []}}.

exit_all([]) ->
    ok;
exit_all(ProcList) when is_list(ProcList) ->
    lists:foreach(fun(Process) when is_pid(Process) -> 
                    exit(Process, kill) end, ProcList).

get_listener_pid(Listener) ->
    Name = atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Listener),
    NameAtom = list_to_atom(Name),
    ListenerPid = whereis(NameAtom),
    ListenerPid.

close_socket(undefined) ->
    ok;
close_socket(Socket) when is_port(Socket) ->
    gen_tcp:close(Socket).

update_bucket_timer(#state{timer = undefined}) ->
    run_bucket_timer();
update_bucket_timer(#state{timer = Timer}) ->
    Timer.

run_bucket_timer() ->
    erlang:send_after(?BUCKET_TIMEOUT, self(), fill_bucket).

drain_bucket(#state{bucket = Bucket}) when Bucket > 0 ->
    ?LOG("Current bucket : ~p ~n",[Bucket-1]),
    Bucket - 1;
drain_bucket(_) -> 
    ?LOG("Current bucket : 0 ~n"),
    0.

fill_bucket(#state{sessions = Sessions} = State) ->
    fill_bucket(State, length(Sessions)).

fill_bucket(#state{bucket = 0}, ?MAX_SESSIONS) ->
    sysInitI:info_msg("Would unblock but maximum sessions reached"),
    ?LOG("Current bucket : 1 ~n"),
    1;
fill_bucket(#state{bucket = 0}, _) ->
    sysInitI:info_msg("Unblocking after high connectionrate"),
    ?LOG("Current bucket : 1 ~n"),
    1;
fill_bucket(#state{bucket = Bucket}, ?MAX_SESSIONS) when
                Bucket =:= (?BUCKET_SIZE - 2) ->
    sysInitI:info_msg("Connectionratelimit: normal, maximum sessions reached"),
     ?LOG("Current bucket : ~p ~n",[Bucket+1]),
    Bucket + 1;
fill_bucket(#state{bucket = Bucket}, _) when Bucket =:= (?BUCKET_SIZE - 2) ->
    sysInitI:info_msg("Connectionratelimit: normal"),
     ?LOG("Current bucket : ~p ~n",[Bucket+1]),
    Bucket + 1;
fill_bucket(#state{bucket = Bucket}, _) when Bucket < ?BUCKET_SIZE ->
     ?LOG("Current bucket : ~p ~n",[Bucket+1]),
    Bucket + 1;
fill_bucket(_, _) ->
     ?LOG("Current bucket : 15 ~n"),
    ?BUCKET_SIZE.
