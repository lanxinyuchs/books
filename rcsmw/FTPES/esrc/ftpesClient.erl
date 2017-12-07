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
%%% %CCaseFile:	ftpesClient.erl %
%%% @author ekurnik
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/5
%%%
%%% ----------------------------------------------------------

-module(ftpesClient).
-behaviour(gen_server).
-vsn('/main/R9A/5').
-date('2017-02-20').
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
%%% R9A/2    2017-02-16   estjako    handle_command for size and ftp_command
%%% R9A/4    2017-02-16   ekurnik    improved 'exit' signal handling
%%% R9A/5    2017-02-20   ekurnik    Added binary option
%%%--------------------------------------------------------------------


%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/0, init/1, open/5, close/1, command/3]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% read_write_status:
%% none - neither read nor write has been started (does not apply to read_file, write_file etc. which are done in a single command)
%% write - send_chunk_start was called, switched back to none when send_chunk_end is invoked
%% read - read_chunk_start was called, switched to read_finished when server returns ok (no more data on server)
%% read_finished - there is no more data on server, but there is still data in Acc, switched to none when close is called
-record (state, {pid = undefined, %% pid from ftp:open
                 acc = <<>>, %% accumulator, used for recv_chunk
                 read_write_status = none, %% status can be none, read, read_finished or write
                 binary = false}). %% return type of the data received from data conn

-include("ftpesd.hrl").

-type status() :: continue | stop.

-define(CLIENT_HANDLER, ftpesClientHandler).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

start_link() ->
   gen_server:start_link(?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),

    {ok, #state{}}.


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Opens an FTPES connection
%%% ----------------------------------------------------------
-spec open(Pid :: pid(), 
           Host :: inet:ip_address(), 
           Opts :: list(), 
           User :: string() | undefined, 
           Password :: string() | undefined) ->
    ok.
%%% ###=====================================================================###
open(Pid, Host, Opts, User, Password) ->
    gen_server:cast(Pid, {open, Host, Opts, User, Password}).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Closes an FTPES connection
%%% ----------------------------------------------------------
-spec close(Pid :: pid()) ->
    ok.
%%% ###=====================================================================###
close(Pid) ->
    gen_server:cast(Pid, close).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Issues a command with given arguments over FTPES
%%% ----------------------------------------------------------
-spec command(Pid ::pid(), Command :: atom(), Args :: list()) -> 
          ok.
%%% ###=====================================================================###
command(Pid, Command, Args) ->
     gen_server:cast(Pid, {command, Command, Args}).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------

handle_call(Msg, _From, State)->
    sysInitI:warning_msg("~p: Unexpected call: ~p~n", [?MODULE, Msg]),
    {noreply, State}.

handle_cast({open, Host, Opts, User, Password}, State) ->
    {Status, Reply, NewState} =
    case ftp:open(Host, Opts) of 
        {ok, Pid} -> 
             case ftp:user(Pid, User, Password) of
                 ok -> 
                   ftp:type(Pid, binary),
                   {continue, {ok, Pid},  State#state{pid = Pid}};
                {error, Reason} -> 
                   {stop, {error, Reason}, State}
             end;
        Error -> 
           {stop, Error, State}
    end,
    handle_reply(Status, {open_conn, Reply}, NewState);

handle_cast(close, #state{pid = Pid} = State) ->
    handle_reply(stop, {close_conn, ftp:close(Pid)}, State);

handle_cast({command, Command, Args}, State) ->
    {Reply, NewState} = handle_command(Command, State, Args),
    handle_reply({Command, Reply}, NewState);

handle_cast(Msg, State)->
    sysInitI:warning_msg("~p: Unexpected cast: ~p~n", [?MODULE, Msg]),
    {noreply, State}.

handle_info({'EXIT', FtpPid, Reason}, #state{pid = FtpPid} = State) ->
    ?LOG("Exit signal received from ftp client, reason: ~p~n", [Reason]),
    {stop, Reason, State};

handle_info({'EXIT', _Pid, _Reason}, #state{pid = FtpPid} = State) ->
    ?LOG("Exit signal received from [~p], reason: ~p~n", [_Pid, _Reason]),
    ftp:close(FtpPid),
    {stop, normal, State};
    
handle_info(Message, State) ->
    sysInitI:warning_msg("~p: Unexpected message received: ~p~n", [?MODULE, Message]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Handles a command received from client handler
%%% ----------------------------------------------------------
-spec handle_command(Command :: atom(), State :: #state{}, Args :: term()) ->
          {term(), #state{}}.
%% If close was called while write is started, end the session
handle_command(close, #state{pid = Pid, read_write_status = write} = State, _) ->
     {ftp:send_chunk_end(Pid), State#state{read_write_status = none}};

%% If close was called while read is not finished, end the session
handle_command(close, #state{pid = Pid, read_write_status = read} = State, _) ->
     %% force close connection
     ftp:send_chunk_end(Pid),
     {ok, State#state{read_write_status = none}};

%% If close was called while read is finished, just change status
handle_command(close, #state{read_write_status = read_finished} = State, _) ->
     {ok, State#state{read_write_status = none}};

%% Otherwise return error the command
handle_command(close, State, []) ->
     {{error, not_open}, State};

handle_command(write_file, #state{pid = Pid} = State, {Bin, RemoteFile}) ->
     {ftp:send_bin(Pid, Bin, RemoteFile), State};

handle_command(read_start, #state{pid = Pid} = State, {File, OpenArgs}) ->
     case ftp:recv_chunk_start(Pid, File) of
         ok ->
            {ok, State#state{read_write_status = read, %% track when the read operation is in progress
                             binary = lists:member(binary, OpenArgs)}};
        Error ->
            {Error, State}
     end;

handle_command(write_start, #state{pid = Pid} = State, File) ->
     case ftp:send_chunk_start(Pid, File) of
         ok ->
             {ok, State#state{read_write_status = write}}; %% track when the write operation is in progress
     Error ->
            {Error, State}
     end;

handle_command(read, #state{pid = Pid} = State, []) ->
     case ftp:recv_chunk(Pid) of
         ok ->
            {ok, State#state{read_write_status = none}};  %% track when the read operation is finished
         Result ->
             handle_output_format({Result, State})
     end;

handle_command(read, State, Chunk) ->
     handle_output_format(handle_recv_chunk(State, Chunk));

handle_command(write, #state{pid = Pid} = State, Bin) ->
     {ftp:send_chunk(Pid, Bin), State};

handle_command(delete, #state{pid = Pid} = State, RemoteFile) ->
     {ftp:delete(Pid, RemoteFile), State};

handle_command(list_dir, #state{pid = Pid} = State, RemoteFile) ->
     {ftp:ls(Pid, RemoteFile), State};

handle_command(read_file, #state{pid = Pid} = State, RemoteFile) ->
     {ftp:recv_bin(Pid, RemoteFile), State}; 

handle_command(recv_file, #state{pid = Pid} = State, {RemoteFile, LocalFile}) ->
     {ftp:recv(Pid, RemoteFile, LocalFile), State}; 

handle_command(make_dir, #state{pid = Pid} = State, Dir) ->
     {ftp:mkdir(Pid, Dir), State};

handle_command(del_dir, #state{pid = Pid} = State, Dir) ->
     {ftp:rmdir(Pid, Dir), State};

handle_command(rename, #state{pid = Pid} = State, {Old, New}) ->
     {ftp:rename(Pid, Old, New), State};

handle_command(cd, #state{pid = Pid} = State, Dir) ->
     {ftp:cd(Pid, Dir), State};

handle_command(pwd, #state{pid = Pid} = State, _) ->
    {ftp:pwd(Pid), State};

handle_command(put_file, #state{pid = Pid} = State, {LocalPath, RemotePath}) ->
    {ftp:send(Pid, LocalPath, RemotePath), State};

handle_command(type, #state{pid = Pid} = State, Type) ->
    {ftp:type(Pid, Type), State};

handle_command(nlist, #state{pid = Pid} = State, Path) ->
    {ftp:nlist(Pid, Path), State};

handle_command(size, State, Path) ->
    {Reply, NewState} = handle_command(ftp_command , State, "SIZE" ++ " " ++ Path ),
    case Reply of
        {ok, Resp} ->
            {to_integer(Resp), NewState};
        _-> 
            {Reply, NewState}
    end;

handle_command(ftp_command , #state{pid = Pid} = State, Command ) ->
    [FtpResponse | _] = ftp:quote(Pid, Command),
    [ResponseCode | ResponseMessage] = string:tokens(FtpResponse, " "),
    ResponseString= string:join(ResponseMessage, " "),
    {{handle_response_code(ResponseCode), ResponseString}, State}.



%% Status can be either read or read_finished
handle_recv_chunk(#state{read_write_status = none} = State, _Chunk) ->
    {{error, read_not_started}, State};

%% There is still data on server
handle_recv_chunk(#state{pid = Pid, read_write_status = read, acc = Acc} = State, Chunk) ->
    %% get from buffer
    if byte_size(Acc) >= Chunk ->
           {Result, NewAcc} = get_chunk(Acc, Chunk),
           {{ok, Result}, State#state{acc = NewAcc}};
       %% fetch new chunk from server
       true ->
           case ftp:recv_chunk(Pid) of
               ok ->
                   %% No data left on server
                   handle_recv_chunk(State#state{read_write_status = read_finished}, Chunk);
               {ok, Bin} ->
                   ?LOG("Fetched ~p bytes from the server~n", [byte_size(Bin)]),
                   handle_recv_chunk(State#state{acc = iolist_to_binary([Acc, Bin])}, Chunk);
               Error ->
                {Error, State}
           end
    end;

%% There is no data on server, return only from Acc
handle_recv_chunk(#state{read_write_status = read_finished, acc = <<>>} = State, _Chunk) ->
    %% No more data to be returned
    {ok, State};
handle_recv_chunk(#state{read_write_status = read_finished, acc = Acc} = State, Chunk) ->
    case max(byte_size(Acc), Chunk) of
        Chunk ->
            %% If chunk is larger than Acc size, return everything
            {{ok, Acc}, State#state{acc = <<>>}};
        _ ->
            {Result, NewAcc} = get_chunk(Acc, Chunk),
            {{ok, Result}, State#state{acc = NewAcc}}
    end.

%% Returns result chunk and the rest of the binary in tuple
get_chunk(Acc, Chunk) ->
    {binary:part(Acc, 0, Chunk), binary:part(Acc, Chunk, byte_size(Acc)-Chunk)}.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Handles a reply to client handler and return to gen_server
%%% ----------------------------------------------------------
-spec handle_reply(Reply :: term(), 
                   NewState :: #state{}) ->
    {noreply, #state{}}.
%%% ###=====================================================================###
handle_reply(Reply, NewState) ->
    handle_reply(continue, Reply, NewState).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Handles a reply to client handler and return to gen_server
%%% ----------------------------------------------------------
-spec handle_reply(Status :: status(), 
                   Reply :: term(), 
                   NewState :: #state{}) ->
    {noreply, #state{}} | {stop, normal, #state{}}.
%%% ###=====================================================================###
handle_reply(Status, Reply, NewState) ->
    ?CLIENT_HANDLER ! {command_reply, self(), Reply},
    handle_status(Status, NewState).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Handles a return to gen_server
%%% ----------------------------------------------------------
 -spec handle_status(Status :: status(), 
                     NewState :: #state{}) ->
    {noreply | stop, #state{}}.
%%% ###=====================================================================###
handle_status(continue, NewState) ->
    {noreply, NewState};
handle_status(stop, NewState) ->
    {stop, normal, NewState}.

%% currently only response code 2XX is considered ok
handle_response_code([$2 | _] = ResponseCode) when is_list(ResponseCode) ->
    ok;
handle_response_code(_) ->
    error.

to_integer(Resp) when is_list(Resp) ->
    case string:to_integer(Resp) of
        {error, _} = Error ->
            Error;
        {Number, _} ->
            Number
    end.

%% aaplies to read function - return is binary by default
handle_output_format({{ok, Data}, #state{binary = false} = State}) when is_binary(Data) ->
    {{ok, binary_to_list(Data)}, State};
handle_output_format({Result, State}) ->
    {Result, State}.

