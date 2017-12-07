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
%%% %CCaseFile:	ftpesI.erl %
%%% @author eivmiha
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R8A/R9A/12
%%%
%%% ----------------------------------------------------------

-module(ftpesI).
-vsn('/main/R8A/R9A/12').
-date('2017-03-07').
-author('eivmiha').

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
%%% R9A/4    2017-02-10   ekurnik    Switched client calls to ftpesClientHandler
%%% R9A/5    2017-02-13   eivmiha    Added aread
%%% R9A/7    2017-02-14   ekurnik    Default timeout is undefined (no timeout)
%%% R9A/8    2017-02-16   estjako    Added size, is_directory, nlist
%%% R9A/10   2017-02-20   ekurnik    Added binary option
%%% ----------------------------------------------------------

-export([
         start_client/2, start_client/3,
         stop_client/1,
         read_start/2, read_start/3,
         write_start/2,
         read/1, read/2,
         aread/2,
         read_chunk/2, read_chunk/3,
         read_file/2, read_file/3,
         recv_file/3, recv_file/4, 
         write/2, write/3,
         write_file/3, write_file/4,
         delete/2, delete/3,
         list_dir/2, list_dir/3,
         rename/3,
         make_dir/2,
         del_dir/2,
         close/1,
         cd/2,
         pwd/1,
         put_file/3,
         type/2,
         nlist/2, nlist/3, 
         size/2,
         is_directory/2
               ]).
-export([get_ftpes_session_info/1,
         get_ftpes_session_info/2,
         get_ftpes_session_info/3,
         ftpes_cipher_notify/0]).

-include("ftpesd.hrl").



%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Starts a FTPES client with Host and User
%%% ----------------------------------------------------------
-spec start_client(Host :: inet:ip_address(),
                    tuple()) ->  
          {ok, pid()} | {error, reason()}. 
%%% ###=====================================================================###
start_client(Host, {User, Pass}) ->
    ftpesClientHandler:start_client(Host, {User, Pass}).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Starts a FTPES client with Host, Port, User and Password
%%% ----------------------------------------------------------
-spec start_client(Host :: inet:ip_address(),
                   Port :: integer(),
                   UserPass :: tuple()) ->   {ok, pid()} | {error, reason()}.
%%% ###=====================================================================###
start_client(Host, Port, {User, Pass}) ->
    ftpesClientHandler:start_client(Host, Port, {User, Pass}).


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Stops a FTPES client
%%% ----------------------------------------------------------
-spec stop_client(Pid :: pid()) ->
          ok.
%%% ###=====================================================================###
stop_client(Pid) ->
    ftpesClientHandler:stop_client(Pid).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Starts transfer of the file File from the remote server.
%%% ----------------------------------------------------------
-spec read_start(Pid :: pid(),
                 File :: iodata()) ->
          ok| {error, reason()}.
%%% ###=====================================================================###
read_start(Pid, File) ->
   read_start(Pid, File, []).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Starts transfer of the file File from the remote server.
%%% OpenArgs accepts only binary option
%%% ----------------------------------------------------------
-spec read_start(Pid :: pid(),
                 File :: iodata(),
                 OpenArgs :: list()) ->
          ok| {error, reason()}.
%%% ###=====================================================================###
read_start(Pid, File, OpenArgs) ->
   ftpesClientHandler:read_start(Pid, File, OpenArgs).


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Starts transfer of chunks into the File at the remote server. 
%%% ----------------------------------------------------------
-spec write_start(Pid :: pid(),
                  File :: iodata()) ->
          ok | {error, reason()}.
%%% ###=====================================================================###
write_start(Pid, File) ->
    ftpesClientHandler:write_start(Pid, File).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Receives a chunk of undefined size from the remote file.
%%% ----------------------------------------------------------
-spec read(Pid :: pid()) ->
          {ok, binary()} | eof | {error, reason()}.
%%% ###=====================================================================###
read(Pid) ->
    read(Pid, undefined).

read(Pid, Timeout) ->
    ftpesClientHandler:read(Pid, Timeout).


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Receives a chunk of specified length from the remote file.
%%% ----------------------------------------------------------
-spec read_chunk(Pid :: pid(), Chunk :: non_neg_integer()) ->
          {ok, binary()} | ok | {error, reason()}.
%%% ###=====================================================================###
read_chunk(Pid, Chunk) ->
    read_chunk(Pid, Chunk, undefined).

read_chunk(Pid, Chunk, Timeout) ->
    ftpesClientHandler:read(Pid, Chunk, Timeout).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Receives a chunk of specified length from the remote file.
%%% ----------------------------------------------------------
-spec aread(Pid :: pid(), Chunk :: non_neg_integer()) ->
          {async, term()}.
%%% ###=====================================================================###
aread(Pid, Chunk) ->
    ftpesClientHandler:aread(Pid, Chunk).

%%% ##########################################################################
%%% ----------------------------------------------------------
%%% @doc Transfers the file RemoteFile from the remote server and receives it as a binary.
%%% ----------------------------------------------------------
-spec read_file(Pid :: pid(),
                RemoteFile :: iodata()) ->
          {ok, binary()} | {error, reason()}.
%%% ###=====================================================================###
read_file(Pid, RemoteFile) ->
    read_file(Pid, RemoteFile, undefined).

read_file(Pid, RemoteFile, Timeout) ->
     ftpesClientHandler:read_file(Pid, RemoteFile, Timeout).

%%% ##########################################################################
%%% ----------------------------------------------------------
%%% @doc Transfers the file RemoteFile from the remote server and receives it as a binary.
%%% ----------------------------------------------------------
-spec recv_file(Pid :: pid(),
                RemoteFile :: iodata(),
                LocalFile :: iodata()) ->
          ok | {error, reason()}.
%%% ###=====================================================================###
recv_file(Pid, RemoteFile, LocalFile) ->
    recv_file(Pid, RemoteFile, LocalFile, undefined).

recv_file(Pid, RemoteFile, LocalFile, Timeout) ->
     ftpesClientHandler:recv_file(Pid, RemoteFile, LocalFile, Timeout).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Transfers the chunk Bin to the remote server.
%%% ----------------------------------------------------------
-spec write(Pid :: pid(),
            Bin :: binary()) ->
          ok | {error, reason()}.
%%% ###=====================================================================###
write(Pid, Bin) ->
  write(Pid, Bin, undefined).

write(Pid, Bin, Timeout) ->
    ftpesClientHandler:write(Pid, Bin, Timeout).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Transfers the file LocalFile to the remote server
%%% ----------------------------------------------------------
-spec write_file(Pid :: pid(),
                 Bin :: binary(),
                 RemoteFile :: string()) ->
          ok | {error, reason()}.
%%% ###=====================================================================###
write_file(Pid, Bin, RemoteFile) ->
    write_file(Pid, Bin, RemoteFile, undefined).

write_file(Pid, Bin, RemoteFile, Timeout) ->
    ftpesClientHandler:write_file(Pid, Bin, RemoteFile, Timeout).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Deletes the file RemoteFile at the remote server
%%% ----------------------------------------------------------
-spec delete(Pid :: pid(),
             RemoteFile :: iodata() ) ->
          ok | {error, reason}.
%%% ###=====================================================================###
delete(Pid, RemoteFile) ->
    delete(Pid, RemoteFile, undefined).

delete(Pid, RemoteFile, Timeout) ->
    ftpesClientHandler:delete(Pid, RemoteFile, Timeout).


-spec list_dir(Pid :: pid(),
               Pathname :: string() ) ->
          {ok, string()} | {error, reason}.
%%% ###=====================================================================###
list_dir(Pid, Pathname) ->
    list_dir(Pid, Pathname, undefined).

list_dir(Pid, Pathname, Timeout) ->
     ftpesClientHandler:list_dir(Pid, Pathname, Timeout).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Renames Old File to New File at the remote server.
%%% ----------------------------------------------------------
-spec rename(Pid :: pid(),
             Old :: string(),
             New :: string()) ->
          ok | {error, reason()}.
%%% ###=====================================================================###
rename(Pid, Old, New) -> 
      ftpesClientHandler:rename(Pid, Old, New).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Creates the directory Dir at the remote server.
%%% ----------------------------------------------------------
-spec make_dir(Pid :: pid(), 
               Dir :: string()) ->
          ok | {error, reason()}.
%%% ###=====================================================================###
make_dir(Pid, Dir) ->
      ftpesClientHandler:make_dir(Pid, Dir).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Removes directory Dir at the remote server.
%%% ----------------------------------------------------------
-spec del_dir(Pid :: pid(), 
               Dir :: string()) ->
          ok | {error, reason()}.
%%% ###=====================================================================###
del_dir(Pid, Dir) ->
      ftpesClientHandler:del_dir(Pid, Dir).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Ends write function; send_chunk_end
%%% ----------------------------------------------------------
-spec close(Pid :: pid()) ->
          ok | {error, reason()}.
%%% ###=====================================================================###
close(Pid) ->
      ftpesClientHandler:close(Pid).


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Changes the working directory at the remote server to Dir.
%%% ----------------------------------------------------------
-spec cd(Pid :: pid(),
         Dir :: string()) ->
          ok | {error, reason()}.
%%% ###=====================================================================###  
cd(Pid, Dir) ->
    ftpesClientHandler:cd(Pid, Dir).
put_file(Pid, LocalPath, RemotePath) ->
    ftpesClientHandler:put_file(Pid, LocalPath, RemotePath).


%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Returns the current working directory at the remote server.
%%% ----------------------------------------------------------
-spec pwd(Pid :: pid()) ->
          {ok, string()} | {error, reason()}.
%%% ###=====================================================================###  
pwd(Pid) ->
    ftpesClientHandler:pwd(Pid).

type(Pid, Type) ->
    ftpesClientHandler:type(Pid, Type).

-spec nlist(Pid :: pid(), 
            Path :: string()) ->
          {ok, string()} | {error, reason()}.
nlist(Pid, Path) ->
     ftpesClientHandler:nlist(Pid, Path, undefined).

nlist(Pid, Path, Timeout) ->
     ftpesClientHandler:nlist(Pid, Path, Timeout).

-spec size(Pid :: pid(),
           Path :: string())->
          term() | {error, reason()}.
size(Pid, Path) ->
    ftpesClientHandler:size(Pid, Path).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc If we can change the workDir to Path than Path is a directory, else it's a file
%%% or some error. nlist checks if it's a valid file.
%%% ----------------------------------------------------------
-spec is_directory(Pid ::pid(),
                   Path :: string()) ->
          file | directory | {error, reason()}.
is_directory(Pid, Path) ->
    {ok, Pwd} = pwd(Pid),
    case cd(Pid, Path) of
      ok -> cd(Pid, Pwd), 
            directory;
      {error, epath} -> 
             case nlist(Pid, Path) of
                 {ok, []} -> {error, epath};
                 {ok, _} -> file; % Path is a file
                 {error, Reason} -> {error, Reason}
             end;
      {error, Reason} -> 
             {error, Reason}    
     end.

%%% ###########################################################################   
-spec get_ftpes_session_info(UserData :: #user_data{}) ->
                           list().
get_ftpes_session_info(UserData) ->
  ftpesd_util:get_ftpes_session_info(UserData).


-spec get_ftpes_session_info(UserData :: #user_data{},
                             KeyOrSession :: atom()) ->
                         term().
get_ftpes_session_info(UserData, KeyOrSession) ->
  ftpesd_util:get_ftpes_session_info(UserData, KeyOrSession).


-spec get_ftpes_session_info(UserData :: #user_data{},
                             KeyOrSession :: atom(), 
                             Default ::atom()) ->
                         term().
get_ftpes_session_info(UserData, Key, Default) ->
  ftpesd_util:get_ftpes_session_info(UserData, Key, Default).

%% called from COMSA
ftpes_cipher_notify() ->
    ftpesServer:ftpes_cipher_notify(),
    ftpesClientHandler:ftpes_cipher_notify().

