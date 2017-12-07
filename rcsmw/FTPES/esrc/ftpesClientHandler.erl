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
%%% %CCaseFile:	ftpesClientHandler.erl %
%%% @author ekurnik
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/R10A/1
%%%
%%% ----------------------------------------------------------

-module(ftpesClientHandler).
-behaviour(gen_server).
-vsn('/main/R9A/R10A/1').
-date('2017-04-27').
-author('ekurnik').

%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% -----      ---------  --------    ------------------------
%%% R9A/1    2017-02-10   ekurnik    Created
%%% R9A/2    2017-02-13   ekurnik    Added aread
%%% R9A/4    2017-02-14   ekurnik    Added timeout functionality
%%% R9A/6    2017-02-10   estjako    Added ftp_command, size, nlist
%%% R9A/7    2017-02-16   ekurnik    Fixed timeout and 'EXIT' handling
%%% R9A/8    2017-02-16   emarnek    Added test_aread
%%% R9A/10   2017-02-20   ekurnik    Added binary optiond
%%% R9A/13   2017-03-10   ekurnik    Fixed timeout timing issue and 
%%%                                  sending reply when process exits
%%% R9A/14   2017-03-29   ekurnik    cert_event timing issue fix 
%%% R9A/15   2017-04-04   ekurnik    Graceful close on terminate 
%%% R10A/1   2017-04-27   ekurnik    Added OOT ready check when starting
%%%--------------------------------------------------------------------


%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/1, init/1, stop/0, start_ftpes_client_handler/0,
         stop_ftpes_client_handler/0, start_client/1, start_client/2, start_client/3,
         stop_client/1, read_start/3, write_start/2, 
         cert_event/1, close/1, aread/2,
         write_file/4, delete/3, read/2, read/3, write/3, list_dir/3, cd/2, pwd/1, put_file/3,
         read_file/3, recv_file/4, rename/3, make_dir/2, del_dir/2, type/2, ftp_command/2, size/2,
         nlist/3, change_notify/1, ftpes_cipher_notify/0,
         test_aread/5]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {client_options, %% list of client parameters, defined later in code
                clients = [], %% list of all active clients
                stopped_clients = [],  %% list of all stopped clients because of Ip interface change
                active = false %% true when activate is called from init module
                }
        ).

%% Common options for all client instances
-record (common_client_options, {node_credential,
                                 trust_category,
                                 ip_family, 
                                 tls_options}).


-record (client, {pid = undefined, %% ftpesClient pid
                  reply_queue = [], %% put pending requests here
                  aread_seq = 0} %% unique ID
        ). 

-record (pending_request, {from_pid = undefined,
                           command,
                           sync = true, %% if synchronous request is pending, don't allow new messages
                           timer = undefined}). %% timer for request timeout


-include("RcsFileTPM.hrl").
-include("ftpesd.hrl").


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

start_link(Config) ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

init([_Config]) ->
    process_flag(trap_exit, true),

    {ok, #state{}}.

stop() ->
   gen_server:cast(?MODULE, shutdown).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Starts a FTPES client handler
%%% ----------------------------------------------------------
-spec start_ftpes_client_handler() ->
    ok | {error, reason()}.
%%% ###=====================================================================###
start_ftpes_client_handler() ->
    gen_server:cast(?MODULE, start_ftpes_client_handler).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Stops a FTPES client handler
%%% ----------------------------------------------------------
-spec stop_ftpes_client_handler() ->
    ok.
%%% ###=====================================================================###
stop_ftpes_client_handler() ->
    call(?MODULE, stop_ftpes_client_handler).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Starts a FTPES client with only Host
%%% ----------------------------------------------------------
-spec start_client(Host :: inet:ip_address()) ->
          {ok, pid()} | {error, reason()}.
%%% ###=====================================================================###
start_client(Host) ->
    call(?MODULE, {start_client, [Host, undefined, undefined]}).
 
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Starts a FTPES client with Host and User
%%% ----------------------------------------------------------
-spec start_client(Host :: inet:ip_address(),
                   tuple() | integer())-> 
          {ok, pid()} |{error, reason()}.

%%% ###=====================================================================###
start_client(Host, {User, Pass})  ->
    call(?MODULE, {start_client, [Host, 21, {User, Pass}]});

start_client(Host, Port)  ->
    call(?MODULE, {start_client, [Host, Port, {undefined, undefined}]}).


 
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Starts a FTPES client with Host, User and Password
%%% ----------------------------------------------------------
-spec start_client(Host :: inet:ip_address(),
                  Port :: integer(),
                  tuple()) ->
          {ok, pid()} | {error, reason()}.
 %%% ###=====================================================================###
start_client(Host, Port, {User, Pass}) ->
    call(?MODULE, {start_client, [Host, Port, {User, Pass}]}).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Stops a FTPES client
%%% ----------------------------------------------------------
-spec stop_client(Pid :: pid()) ->
    ok.
%%% ###=====================================================================###
stop_client(Pid) ->
    call(?MODULE, {stop_client, Pid}).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Ends write function; send_chunk_end
%%% ----------------------------------------------------------
-spec close(Pid ::pid()) -> 
          ok.
%%% ###=====================================================================###
close(Pid) -> 
    call(?MODULE, {client, close, Pid, []}).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Starts transfer of the file File from the remote server.
%%% ----------------------------------------------------------
-spec read_start(Pid :: pid(),
                 File :: string(),
                 OpenArgs :: list()) ->
          ok |{error, reason()}. 
%%% ###=====================================================================###
read_start(Pid, File, OpenArgs) ->
    call(?MODULE, {client, read_start, Pid, {File, OpenArgs}}).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Starts transfer of chunks into the File at the remote server. 
%%% ----------------------------------------------------------
-spec write_start(Pid :: pid(),
                 File :: string()) ->
          ok |{error, reason()}. 
%%% ###=====================================================================###
write_start(Pid, File) ->
    call(?MODULE, {client, write_start, Pid, File}).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Receives a chunk of the remote file.
%%% ----------------------------------------------------------
-spec read(Pid :: pid(),
           Timeout :: integer() | undefined) ->
     ok | {ok, binary()} | {error, reason()}.
%%% ###=====================================================================###
read(Pid, Timeout) ->
    call(?MODULE, {client, read, Pid, [], Timeout}).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Receives a chunk of the remote file.
%%%      If chunk is larger than requested it will be stored internally
%%%      and returned next time.
%%% ----------------------------------------------------------
-spec read(Pid :: pid(),
           Chunk :: non_neg_integer(),
           Timeout :: integer() | undefined) ->
     ok | {ok, binary()} | {error, reason()}.
%%% ###=====================================================================###
read(Pid, Chunk, Timeout) ->
    call(?MODULE, {client, read, Pid, Chunk, Timeout}).

%%% ----------------------------------------------------------
-spec aread(Pid :: pid(),
            Chunk :: non_neg_integer()) ->
     {async, term()}.
%%% ###=====================================================================###
aread(Pid, Chunk) ->
    gen_server:call(?MODULE, {client, aread, Pid, {Chunk, self()}}).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Transfers the chunk Bin to the remote server.
%%% ----------------------------------------------------------
-spec write(Pid :: pid(),
            Bin :: binary(),
            Timeout :: integer() | undefined) ->
     ok | {error, reason()}.
%%% ###=====================================================================###
write(Pid, Bin, Timeout) ->
    call(?MODULE, {client, write, Pid, Bin, Timeout}).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Transfers the file LocalFile to the remote server
%%% ----------------------------------------------------------
-spec write_file(Pid :: pid(),
                 Bin :: binary(),
                 RemoteFile :: string(),
                 Timeout :: integer() | undefined) ->
    ok | {error, reason()}.
%%% ###=====================================================================###
write_file(Pid, Bin, RemoteFile, Timeout) ->
    call(?MODULE, {client, write_file, Pid, {Bin, RemoteFile}, Timeout}).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Deletes the file RemoteFile at the remote server
%%% ----------------------------------------------------------
-spec delete(Pid :: pid(),
             RemoteFile :: string(),
             Timeout :: integer() | undefined) ->
    ok | {error, reason()}.
%%% ###=====================================================================###
delete(Pid, RemoteFile, Timeout) ->
    call(?MODULE, {client, delete, Pid, RemoteFile, Timeout}).


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns a list of files in long format.
%%% Pathname can be a directory, a group of files, or a file.
%%% The Pathname string can contain wildcards.
%%% ----------------------------------------------------------
-spec list_dir(Pid :: pid(),
               Pathname:: string(),
               Timeout :: integer() | undefined) ->
       ok| {error, reason()}.
%%% ###=====================================================================###
list_dir(Pid, Pathname, Timeout) -> 
    call(?MODULE, {client, list_dir, Pid, Pathname, Timeout}).

%%% ##########################################################################
%%% ----------------------------------------------------------
%%% @doc Transfers the file Pathname from the remote server and receives it as a binary.
%%% ----------------------------------------------------------
-spec read_file(Pid :: pid(),
                Pathname :: string(),
                Timeout :: integer() | undefined) ->
       ok | {error, reason()}.
%%% ###=====================================================================###
read_file(Pid, Pathname, Timeout) ->
     call(?MODULE, {client, read_file, Pid, Pathname, Timeout}).

%%% ##########################################################################
%%% ----------------------------------------------------------
%%% @doc Transfers the file Pathname from the remote server and receives it as a binary.
%%% ----------------------------------------------------------
-spec recv_file(Pid :: pid(),
                Pathname :: string(),
                LocalFile :: string(),
                Timeout :: integer() | undefined) ->
       ok | {error, reason()}.
%%% ###=====================================================================###
recv_file(Pid, Pathname, LocalFile, Timeout) ->
     call(?MODULE, {client, recv_file, Pid, {Pathname, LocalFile}, Timeout}).

%%% ##########################################################################
%%% ----------------------------------------------------------
%%% @doc Renames Old File to New File at the remote server.
%%% ----------------------------------------------------------
-spec rename(Pid :: pid(),
             Old :: string(),
             New :: string()) ->
        ok|{error, reason()}.
%%% ###=====================================================================###           
rename(Pid, Old, New) ->
    call(?MODULE, {client, rename, Pid, {Old, New}}).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Creates the directory Dir at the remote server.
%%% ----------------------------------------------------------
-spec make_dir(Pid :: pid(),
               Dir :: string()) ->
        ok|{error, reason()}.
%%% ###=====================================================================###           
make_dir(Pid, Dir) ->
    call(?MODULE, {client, make_dir, Pid, Dir}).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Removes directory Dir at the remote server.
%%% ----------------------------------------------------------
-spec del_dir(Pid :: pid(),
              Dir :: string()) ->
        ok|{error, reason()}.
%%% ###=====================================================================###           
del_dir(Pid, Dir) ->
    call(?MODULE, {client, del_dir, Pid, Dir}).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Changes the working directory at the remote server to Dir.
%%% ----------------------------------------------------------
-spec cd(Pid :: pid(),
         Dir :: string()) ->
          ok | {error, reason()}.
%%% ###=====================================================================###  
cd(Pid, Dir) ->
     call(?MODULE, {client, cd, Pid, Dir}).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns the current working directory at the remote server.
%%% ----------------------------------------------------------
-spec pwd(Pid :: pid()) ->
          {ok, string()} | {error, reason()}.
%%% ###=====================================================================###  
pwd(Pid) ->
     call(?MODULE, {client, pwd, Pid, []}).

put_file(Pid, LocalPath, RemotePath) ->
     call(?MODULE, {client, put_file, Pid, {LocalPath, RemotePath}}).

type(Pid, Type) ->
     call(?MODULE, {client, type, Pid, Type}).

nlist(Pid, Path, Timeout) ->
    call(?MODULE, {client, nlist, Pid, Path, Timeout}).

-spec ftp_command(Pid :: pid(),
           Command :: string()) ->
          list().
ftp_command(Pid, Command) ->
    call(?MODULE, {client, ftp_command, Pid, Command}).

size(Pid, Path) ->
    call(?MODULE, {client, size, Pid, Path}).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Getting and processing Changes from client
%%% ----------------------------------------------------------
-spec change_notify(Changes :: list()) ->
    ok | {error, reason()}.
%%% ###=====================================================================###
change_notify(Changes) ->
    gen_server:cast(?MODULE, {change_notify, Changes}).

%% FIXME: Temporary solution for CERT timing issue
cert_event(MoRef) ->
    timer:apply_after(?CERT_EVENT_OFFSET, gen_server, cast, [?MODULE, {cert_event, MoRef}]).

ftpes_cipher_notify() ->
    case whereis(?MODULE) of
        undefined -> 
            ok;
        _->
            gen_server:cast(?MODULE, ftpes_cipher_notify)
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
handle_call(_Msg, _From, #state{active = false} = State)->
    sysInitI:warning_msg("~p: FTPES client handler is not active!~n", [?MODULE]),
    {reply, {error, not_active}, State};

handle_call(stop_ftpes_client_handler, _From, State)->
   sysInitI:info_msg("~p: Stopping FTPES client handler.~n", [?MODULE]),
   NewState = do_stop_ftpes_client_handler(State),
   {reply, ok, NewState#state{active = false}};

handle_call({start_client, [Host, Port, {User, Pass}]}, FromPid, State) ->
    case is_tls_client_enabled(State#state.client_options) of 
        {nok, Parameter, Value}-> 
           sysInitI:info_msg("~p: FTPES client cannot be started.~n[~p = ~p]~n", [?MODULE, Parameter, Value]),
           {reply, {error, no_tls}, State};
        ok ->
           do_start_client([Host, Port, {User, Pass}], FromPid, State)
    end;
    
handle_call({stop_client, Pid}, FromPid, State) ->
    case get_client(Pid, State#state.clients) of
        {ok, Client} ->
             do_stop_client(Client, FromPid, State);
        _-> 
            sysInitI:info_msg("~p: Client already stopped: [~p]~n", [?MODULE, Pid]),
            {reply, ok, State}
    end;
    
handle_call({client, Command, Pid, Args}, FromPid, State) ->
    handle_call({client, Command, Pid, Args, undefined}, FromPid, State);

handle_call({client, Command, Pid, Args, Timeout}, FromPid, State) ->
    case get_client(Pid, State#state.clients) of
        {ok, Client} ->
            ?LOG("Client command called [~p]:[~p(~P)]~n", [Pid, Command, Args, 40]),
            do_handle_client_command(Client, Command, Args, Timeout, FromPid, State);
        {error, _Reason}-> 
            case get_client(Pid, State#state.stopped_clients) of
                {ok, Client} ->
                    NewState = State#state{stopped_clients = remove_client(Client, State#state.stopped_clients)},
                    {reply, {error, reconf}, NewState};
                _ ->          
                    {reply, {error, no_proc}, State}
            end
    end;
        

handle_call(Msg, _From, State)->
    sysInitI:warning_msg("~p: Unexpected call: ~p~n", [?MODULE, Msg]),
    {noreply, State}.

handle_cast(start_ftpes_client_handler, #state{active = false} = State) ->
    {_, NewState} = do_start_ftpes_client_handler(State),
    {noreply, NewState};
handle_cast(start_ftpes_client_handler, #state{active = true} = State) ->
    {noreply, State};

handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(_Msg, #state{active = false} = State)->
    sysInitI:warning_msg("FTPES client handler is not active!"),
    {noreply, State};

handle_cast({change_notify, _Changes}, State) ->
    send_new_interface(State);

handle_cast({cert_event, MoRef}, State) ->
    sysInitI:info_msg("~p: cert_event received - MO changed: ~p~n", [?MODULE, MoRef]),
    NewState = stop_clients(State),
    CommonClOpts = NewState#state.client_options,
    Nc = CommonClOpts#common_client_options.node_credential,
    Tc = CommonClOpts#common_client_options.trust_category,
    NewClientOpts = CommonClOpts#common_client_options{tls_options = get_tls_options(Nc, Tc)},
    {noreply, NewState#state{client_options = NewClientOpts}};

handle_cast(ftpes_cipher_notify, State) ->
    sysInitI:info_msg("~p: cipher_notify received~n", [?MODULE]),
    NewState = stop_clients(State),
    CommonClOpts = NewState#state.client_options,
    Nc = CommonClOpts#common_client_options.node_credential,
    Tc = CommonClOpts#common_client_options.trust_category,
    NewClientOpts = CommonClOpts#common_client_options{tls_options = get_tls_options(Nc, Tc)},
    {noreply, NewState#state{client_options = NewClientOpts}};

handle_cast(Msg, State)->
    sysInitI:warning_msg("~p: Unexpected cast: ~p~n", [?MODULE, Msg]),
    {noreply, State}.

handle_info(_Msg, #state{active = false} = State)->
    sysInitI:warning_msg("FTPES client handler is not active!"),
    {noreply, State};

%% no change
handle_info({mnesia_table_event, {write, ftpTls, New, [New], _}}, State) ->
    sysInitI:info_msg("~p: ftpTls mnesia event received - no change~n", [?MODULE]),
    {noreply, State};

handle_info({mnesia_table_event, {write, ftpTls, New, _, _}}, State) ->
    %% Old params
    CommonClParams = State#state.client_options,
    ftpesLib:unsubscribe_mo(?MODULE, 
                   CommonClParams#common_client_options.node_credential, 
                   CommonClParams#common_client_options.trust_category),
    %% New params
    ftpesLib:subscribe_mo(?MODULE, 
                   New#ftpTls.nodeCredential, 
                   New#ftpTls.trustCategory),
    
    NewState = stop_clients(State),
    NewCommonClParams = CommonClParams#common_client_options{node_credential = New#ftpTls.nodeCredential,
                                       trust_category = New#ftpTls.trustCategory},

    TlsOptions = get_tls_options(New#ftpTls.nodeCredential, New#ftpTls.trustCategory),
    NewClientOpts = NewCommonClParams#common_client_options{tls_options = TlsOptions},
    
    sysInitI:info_msg("~p: ftpTls mnesia event received.~nnodeCredential: ~p~ntrustCategory: ~p~n", 
                      [?MODULE, New#ftpTls.nodeCredential, New#ftpTls.trustCategory]),
    {noreply, NewState#state{client_options = NewClientOpts}};

%% replies from ftpes clients which should be returned to the original caller
handle_info({command_reply, ClientPid, Reply}, State) ->
    ?LOG("Command reply received: [~p]: ~P~n", [ClientPid, Reply, 40]),
    case get_client(ClientPid, State#state.clients) of
        {ok, Client} ->
            handle_reply(Client, Reply, State);
        _ ->
            %% no such client, ignore the reply
            {noreply, State}
    end;

%% timeouts
handle_info({timeout, TimerRef, {command_timeout, ClientPid}}, State) ->
    ?LOG("Command timeout received: [~p]~n", [ClientPid]),
    case get_client(ClientPid, State#state.clients) of
        {ok, Client} ->
            handle_timeout(Client, TimerRef, State);
        _ ->
            %% no such client, ignore the timeout
            {noreply, State}
    end;

%% Expected when force close is called
handle_info({'EXIT', FromPid, shutdown}, State) ->
    sysInitI:info_msg("~p: Force stopped client: [~p]~n", [?MODULE, FromPid]),
    {noreply, State};

%% Expected when ftpesClient is closed
handle_info({'EXIT', FromPid, Reason}, #state{clients = Clients} = State) ->
    case {Reason, get_client(FromPid, Clients)} of
        {Reason, {ok, #client{reply_queue = ReplyQueue}}} ->
            sysInitI:info_msg("~p: Client stopped [~p] with reason [~p]~n", [?MODULE, FromPid, Reason]),
            do_reply(ReplyQueue, {error, closed}),
            {noreply, State#state{clients = remove_client(FromPid, Clients)}};
        {normal, _} ->
            sysInitI:info_msg("~p: Client stopped [~p]~n", [?MODULE, FromPid]),
            {noreply, State};
        {Reason, _} ->
            sysInitI:warning_msg("~p: Unexpected exit signal received from: ~p~nReason: ~p~n", [?MODULE, FromPid,Reason]),
            {noreply, State}
    end;
    
handle_info(Message, State) ->
    sysInitI:warning_msg("~p: Unexpected message received: ~p~n", [?MODULE, Message]),
    {noreply, State}.

terminate(_Reason, State) ->
    %% Close clients
    do_stop_clients(State),
    ok.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc This should be called only once when starting ftpesServer
%%% ----------------------------------------------------------

call(Server, Message) ->
    call(Server, Message, ?GEN_SERVER_TIMEOUT).

call(Server, Message, Timeout) ->
    gen_server:call(Server, Message, Timeout).

initialize_state(State) ->
    mnesia:subscribe({table, ftpTls, detailed}),

    {NodeCredential, TrustCategory} = ftpesLib:get_cert(),
    ftpesLib:subscribe_mo(?MODULE, NodeCredential, TrustCategory),
    
    IpFamily = ftpesLib:check_available_interface(),
    TlsOptions = get_tls_options(NodeCredential, TrustCategory),
    NewState = State#state{client_options = 
                          #common_client_options{
                               node_credential = NodeCredential,
                               trust_category = TrustCategory,
                               ip_family = IpFamily,
                               tls_options = TlsOptions
                          }
                     },
    ok = ootI:register_cfg_upd_cb(fun ?MODULE:change_notify/1),
    NewState#state{active = true}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle commands sent to ftpesClient
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_start_client([Host, Port, {User, Pass}], FromPid, State) ->
      ClOptions = State#state.client_options,
      IpFamily = ClOptions#common_client_options.ip_family,
      TlsOptions = ClOptions#common_client_options.tls_options,
    
      Args = [{port, Port}, {tls, TlsOptions}, {ipfamily, IpFamily},  {mode, passive}],
      {ok, Pid} = ftpesClient:start_link(),
      sysInitI:info_msg("~p: Starting client ~p: [~p@~p:~p]~n", [?MODULE, Pid, User, Host, Port]),
      ftpesClient:open(Pid, Host, Args, User, Pass), %% open connection

      NewClient = #client{pid = Pid, reply_queue = [#pending_request{from_pid = FromPid, command = open_conn}]},
      {noreply, State#state{clients = [NewClient | State#state.clients]}}.

%% reply queue is empty, close client gracefully
do_stop_client(#client{pid = Pid, reply_queue = []} = Client, _FromPid, State) ->
    ftpesClient:close(Pid),
    UpdatedClient = Client#client{reply_queue = [#pending_request{from_pid = undefined, command = close_conn}]},
    %% return immediately
    {reply, ok, State#state{clients = update_client(UpdatedClient, State#state.clients)}};

%% reply queue is not empty (async messages), force close a client
do_stop_client(#client{pid = Pid, reply_queue = ReplyQueue} = Client, _FromPid, State) ->
    force_close(Pid),
    do_reply(ReplyQueue, {error, closed}),
    {reply, ok, State#state{clients = remove_client(Client, State#state.clients)}}.

%% handle aread
do_handle_client_command(#client{pid = Pid, reply_queue = ReplyQueue, aread_seq = SeqID} = Client, 
                         aread, {Chunk, FromPid}, undefined, _Pid, State) ->
    ftpesClient:command(Pid, read, Chunk),
    UpdatedClient = Client#client{reply_queue = ReplyQueue ++ 
                    [#pending_request{from_pid = FromPid, command = {aread, SeqID}, sync = false}], 
                                  aread_seq = SeqID +1},

    {reply, {async, SeqID}, State#state{clients = update_client(UpdatedClient, State#state.clients)}};

%% handle sync messages (only async messages allowed in queue)
do_handle_client_command(#client{pid = Pid, reply_queue = ReplyQueue} = Client, 
                         Command, Args, Timeout, FromPid, State) ->
    %% check for sync messages in the queue
    case is_sync_request_pending(ReplyQueue) of
        false ->
            %% no sync messages in queue
            ftpesClient:command(Pid, Command, Args),
            UpdatedClient = Client#client{reply_queue = ReplyQueue ++ 
                            [#pending_request{from_pid = FromPid, command = Command, timer = do_start_timer(Pid, Timeout)}]},
            {noreply, State#state{clients = update_client(UpdatedClient, State#state.clients)}};
        true ->
            %% another sync message is already in the queue
            {reply, {error, request_pending}, State}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle ftpesClient replies
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_reply(Client, {open_conn, Reply}, State) ->
    handle_open_reply(Client, Reply, State);

handle_reply(Client, {close_conn, Reply}, State) ->
    handle_close_reply(Client, Reply, State);

handle_reply(Client, {Command, Reply}, State) ->
    handle_command_reply(Client, {Command, Reply}, State).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle reply from ftpesClient:open()
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_open_reply(#client{pid = Pid, reply_queue = [#pending_request{command = open_conn}]} = Client, Reply, State) ->
    
    case Reply of
        {ok, _} ->
            sysInitI:info_msg("~p: Opened FTPES connection on client ~p~n", [?MODULE, Pid]),
            do_reply(Client#client.reply_queue, {ok, Pid}),
            UpdatedClient = Client#client{reply_queue = []},
            {noreply, State#state{clients = update_client(UpdatedClient, State#state.clients)}};
        {error, Reason} ->
            %% no connection, remove client
            sysInitI:info_msg("~p: Cannot open FTPES connection on client ~p~nReason: ~p~n", [?MODULE, Pid, ftp:formaterror(Reason)]),
            do_reply(Client#client.reply_queue, {error, Reason}),
            {noreply, State#state{clients = remove_client(Client, State#state.clients)}};
        %% SSL verification errors don't have {error, Reason format}
        Error ->
            sysInitI:info_msg("~p: Cannot open FTPES connection on client ~p~nReason: ~p~n", [?MODULE, Pid, Error]),
            do_reply(Client#client.reply_queue, {error, Error}),
            {noreply, State#state{clients = remove_client(Client, State#state.clients)}}
    end.

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle reply from ftpesClient:close()
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% handling case when stop_client() was gracefully closed (reply is already sent)
handle_close_reply(#client{reply_queue = [#pending_request{command = close_conn}]} = Client, _Reply, State) ->
    {noreply, State#state{clients = remove_client(Client, State#state.clients)}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle reply from ftpesClient:command()
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_command_reply(#client{reply_queue = [], pid = Pid}, {Command, _Reply}, State) ->
    sysInitI:error_msg("~p: Client [~p] returned command [~p] reply, but queue is empty!~n", [?MODULE, Pid, Command]),
    {noreply, State};

%% the command which we return should always be on top of the queue (FIFO),
%% client handler must ensure this!

%% handle aread reply
handle_command_reply(#client{reply_queue = [#pending_request{command = {aread, SeqID}} 
                                              = PendingRequest | ReplyQueue]} = Client, 
                                                {read, Reply}, State) ->

    %% send reply
    NewReply = case Reply of
                   ok -> eof;
                   _ -> Reply
               end,
    do_reply(PendingRequest, {async_reply, SeqID, NewReply}),
    UpdatedClient = Client#client{reply_queue = ReplyQueue},
    {noreply, State#state{clients = update_client(UpdatedClient, State#state.clients)}};

%% handle command reply
handle_command_reply(#client{reply_queue = [#pending_request{command = Command, timer = TimerRef} 
                                              = PendingRequest | ReplyQueue]} = Client, 
                                                {Command, Reply}, State) ->

    %% send reply and stop timeout timer (if running)
    do_reply(PendingRequest, Reply),
    do_stop_timer(TimerRef),
    UpdatedClient = Client#client{reply_queue = ReplyQueue},
    {noreply, State#state{clients = update_client(UpdatedClient, State#state.clients)}};

%% Unexpected message received
handle_command_reply(#client{pid = Pid, reply_queue = ReplyQueue} = Client, {Command, _Reply}, State) ->
    
    sysInitI:error_msg("~p: Client [~p] returned command [~p] reply, but request is not on top of the queue!~nQueue: ~p~n", 
                       [?MODULE, Pid, Command, ReplyQueue]),
    print_client("Unexpected reply received: ", Client),
    {noreply, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle command timeout
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_timeout(#client{reply_queue = [#pending_request{timer = TimerRef} | _] = ReplyQueue, 
					 pid = Pid} = Client, 
                                         TimerRef, State) ->
    %% send timeout error
    do_reply(ReplyQueue, {error, timeout}),
    
    %% force close client
    force_close(Pid),
    
    %% remove client
    {noreply, State#state{clients = remove_client(Client, State#state.clients)}};

handle_timeout(#client{pid = Pid, reply_queue = ReplyQueue} = Client,TimerRef, State) ->

    sysInitI:error_msg("~p: Client [~p] got timeout [~p] reply, but request is not on top of the queue!~nQueue: ~p~n", 
                       [?MODULE, Pid, TimerRef, ReplyQueue]),
    print_client("Unexpected timeout received: ", Client),
    {noreply, State}.


get_tls_options(Nc, Tc) ->
    Result = 
    case  ftpesLib:setup_tls(Nc, Tc) of
        {ok, TlsOpts} -> TlsOpts;
        {error, _}-> undefined
    end,

    ?LOG("Fetching TLS options.~nNC: ~p~nTC: ~p~nOptions: ~P~n", [Nc, Tc, Result, 40]),
    Result.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Initial start of ftpes client handler
%%% ----------------------------------------------------------
-spec do_start_ftpes_client_handler(State :: #state{}) -> {ok | nok, #state{}}.
do_start_ftpes_client_handler(State) ->
    %% check if OOT is up
    case ootI:get_lmt_ipv4() of
        {error, oot_not_started} ->
            %% retry
            sysInitI:info_msg("~p: Initial starting failed.~nOOT is no ready - retry in 5 seconds~n", [?MODULE]),
            timer:apply_after(5000, ?MODULE, start_ftpes_client_handler, []),
            {retry, State};
        _ ->
            {ok, initialize_state(State)}
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Shutdown ftpes client handler
%%% ----------------------------------------------------------
-spec do_stop_ftpes_client_handler(State :: #state{}) ->
    #state{}.
%%% ###=====================================================================###
do_stop_ftpes_client_handler(State) ->
    certI:unsubscribe(?MODULE),
    stop_clients(State).


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc On config update check IP family, refresh State and send message
%%% ----------------------------------------------------------
-spec send_new_interface(State :: #state{}) -> 
    {noreply, #state{}}.
%%% ###=====================================================================###
send_new_interface(State) ->
    IpFamily = ftpesLib:check_available_interface(), 
    CommonClientOptions = State#state.client_options,
    NewState = State#state{client_options = 
               CommonClientOptions#common_client_options{ip_family = IpFamily}},
   {noreply, NewState}.

   
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Checking if tls parameters from TempParamList are valid
%%%      If they are valid, FTPES client can be started
%%% ----------------------------------------------------------
-spec is_tls_client_enabled(CommonOptions :: #common_client_options{}) ->
   ok | {nok, Attribute :: atom(), Value :: any()}.
%%% ###=====================================================================###
is_tls_client_enabled(#common_client_options{tls_options = TlsOpts} = _CommonOptions) ->
    ?LOG("Checking client options: ~P~n", [_CommonOptions, 40]),

    if TlsOpts == undefined -> 
           {nok, tls_options, undefined};
    true -> 
           ok
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Stops all active clients
%%% ----------------------------------------------------------
-spec stop_clients(State :: #state{}) -> #state{}.
%%% ###=====================================================================##
stop_clients(State) ->
    do_stop_clients(State).

do_stop_clients(#state{clients = []} = State) ->
    State;

%% if the reply queue is empty, wait for the next request to return closed error
do_stop_clients(#state{clients = [#client{pid = Pid, reply_queue = []} = Client | Clients], 
                       stopped_clients = StoppedClients} = State) ->
    ftpesClient:close(Pid),
    UpdatedClient = Client#client{reply_queue = [#pending_request{from_pid = undefined, command = close_conn}]},
    State#state{clients = Clients,
                stopped_clients = [UpdatedClient | StoppedClients]};

%% if the reply queue is not empty, return error close and force_close client
do_stop_clients(#state{clients = [#client{pid = Pid, reply_queue = ReplyQueue} | Clients]} = State) ->
    force_close(Pid),
    do_reply(ReplyQueue, {error, reconf}),
    %% stop timers if there are any
    lists:foreach(fun(#pending_request{timer = TimerRef}) -> do_stop_timer(TimerRef) end, ReplyQueue),
    State#state{clients = Clients}.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns client record containing given pid
%%% ----------------------------------------------------------
-spec get_client(Pid :: pid(), Clients :: [#client{}]) -> {ok, #client{}} | {error, term()}.
get_client(_, []) ->
    {error, not_found};
get_client(Pid, [#client{pid = Pid} = Client | _]) ->
    {ok, Client};
get_client(Pid, [_ | Clients]) ->
    get_client(Pid, Clients).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Removes client record containing given pid
%%% ----------------------------------------------------------
-spec remove_client(Client :: #client{} | pid(), Clients :: [#client{}]) -> [#client{}].
remove_client(Pid, Clients) when is_pid(Pid)->
    case get_client(Pid, Clients) of
        {ok, Client} ->
            remove_client(Client, Clients);
        _ ->
            Clients
    end;
remove_client(Client, Clients) when is_record(Client, client)->
    lists:delete(Client, Clients).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Updates client record containing given pid
%%% ----------------------------------------------------------
-spec update_client(UpdatedClient :: #client{}, Clients :: [#client{}]) -> [#client{}].
update_client(#client{pid = UCPid} = UpdatedClient, Clients) ->
    print_client("Updating client:", UpdatedClient),
    lists:map(fun(#client{pid = Pid}) when Pid =:= UCPid -> UpdatedClient;
                 (Client) -> Client end, Clients).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Logging command which prints out client
%%% ----------------------------------------------------------
-spec print_client(Info :: string(),Client :: #client{}) -> term().
print_client(_Info, _Client) ->
    ?LOG("~p~nPid: [~p]~nReply queue: ~P~n", 
         [_Info, _Client#client.pid, _Client#client.reply_queue, 80]).

is_sync_request_pending(ReplyQueue) ->
     case lists:filter(fun(#pending_request{sync = true}) ->
                              true;
                         (_) ->
                              false
                      end, ReplyQueue) of
         [] ->
            false;
         _ ->
            true
     end.

do_reply(#pending_request{} = PendingRequest, Reply) ->
    do_reply([PendingRequest], Reply);
do_reply([], _) ->
    [];
do_reply([#pending_request{from_pid = undefined} | ReplyQueue], Reply) ->
    do_reply(ReplyQueue, Reply);
do_reply([#pending_request{from_pid = FromPid, sync = true} | ReplyQueue], Reply) ->
    gen_server:reply(FromPid, Reply),
    do_reply(ReplyQueue, Reply);
do_reply([#pending_request{from_pid = FromPid, sync = false} | ReplyQueue], Reply) ->
    FromPid ! Reply,
    do_reply(ReplyQueue, Reply).

%% force close a client performing an operation
force_close(Pid) ->
    exit(Pid, shutdown).

do_start_timer(ClientPid, Timeout) when is_integer(Timeout) 
                        andalso Timeout > 0 ->
    erlang:start_timer(Timeout, self(), {command_timeout, ClientPid});
do_start_timer(_, _) ->
    undefined.

do_stop_timer(undefined) ->
    ok;
do_stop_timer(TimerRef) ->
    case erlang:cancel_timer(TimerRef) of
        false ->
            %% timer has already expired (msg is already in queue)
            receive
                {timeout, TimerRef, _} ->
                    ok
            after 0 ->
                    ok
            end;
        _ ->
            ok
    end.

%%% ----------------------------------------------------------
-spec test_aread(ftpes | sftp,
                 Pid :: pid(),
                 Handle :: term(),
                 Chunk :: non_neg_integer(),
                 N :: non_neg_integer()) ->
                 list().
%%% ###=====================================================================###
%% Function only for testing purposes
test_aread(Server, Pid, Handle, Chunk, N) ->
    Keys = [{_, _} = ftpI:aread(Server, Pid, Handle, Chunk) || _ <- lists:seq(1,N)],
    Replies = [receive
                   {async_reply, Key, Reply} -> ?LOG("Reply: ~p",[Reply]),
                                                Reply;
                   Other -> Other
               end || {_, Key} <- Keys],
    Replies.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
    
