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
%%% %CCaseFile:	ftpI.erl %
%%% @author emarnek
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R8A/R9A/R10A/R11A/R12A/1
%%%
%%% ----------------------------------------------------------

-module(ftpI).
-vsn('/main/R8A/R9A/R10A/R11A/R12A/1').
-date('2017-11-23').
-author('emarnek').

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
%%% -----      ---------  --------    ------------------------
%%% R8A/1    2016-11-18   ekurnik    Created
%%% R8A/3    2016-12-13   estjako    Added doc and spec
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% R9A/3    2017-01-31   eivmiha    parse_uri
%%% R9A/5    2017-02-01   emarnek    format_error
%%% R9A/11   2017-02-13   eivmiha    Added aread
%%% R9A/14   2017-02-14   ekurnik    fixed format_error/2
%%% R9A/15   2017-02-20   ekurnik    Added binary mode for ftpes
%%% R9A/17   2017-03-03   ekurnik    Added new error descriptions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% R11A/1   2017-10-04   emarnek    HW33598
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% R12A/1   2017-11-23   emarnek    HW45665
%%%--------------------------------------------------------------------

-export([start_channel/3, start_channel/4,start_channel/5, start_channel/6,
         start_channel_only_alt/5, start_channel_with_alt/5,
         start_channel_only_alt/6, start_channel_with_alt/6,
         stop_channel/2, stop_channel/3,
         open/4,
         close/3,
         read/4, read/5, aread/4,
         write/4, write/5,
         delete/3, delete/4,
         list_dir/3, list_dir/4,
         read_file/3, read_file/4, 
         recv_file/3,
         write_file/4, write_file/5,
         put_file/4,
         parse_uri/1,
         format_error/2
        ]).

-type reason() :: any().


%%% ----------------------------------------------------------
%%%     # start_channel/3
%%% @doc Starts a FTPES client or an SSH channel process is started 
%%% to handle the communication with the SFTP server.
%%% The first param is an atom, which can be ftpes (used for starting FTPES client)
%%% or sftp (used for SFTP server).
%%% ----------------------------------------------------------
-spec start_channel(ftpes | sftp, 
                    Host :: string() | {string(), integer()},
                    Options :: list()) ->
          {ok, pid(), term()} | {error, reason()}.
start_channel(ftpes, Host, Options)->
    case Options of
        [] ->
            {error, euser};
        _-> 
            User = proplists:get_value(user, Options),
            Pass = proplists:get_value(password, Options),
            case User of
                undefined -> {error, euser};
                _ -> 
                    case ftpesI:start_client(Host, {User, Pass}) of
                        {ok, Pid} -> {ok, Pid, undefined};
                        {error, Reason} -> {error, Reason}
                    end
            end
    end;
start_channel(sftp, Host, Options)->
    ssh_sftp:start_channel(Host, Options).

%%% ----------------------------------------------------------
%%%     # start_channel/4
%%% @doc Starts a FTPES client or an SSH channel process is started 
%%% to handle the communication with the SFTP server.
%%% The first param is an atom, which can be ftpes (used for starting FTPES client)
%%% or sftp (used for SFTP server).
%%% ----------------------------------------------------------
-spec start_channel(ftpes | sftp, 
                    Host :: string() | {string(), integer()},
                    Port :: integer(),
                    Options :: list()) ->
          {ok, pid(), term()} | {error, reason()} | term().
start_channel(ftpes, Host, Port, Options)->
    case Options of
        [] -> 
            {error, euser};
        _->
            User = proplists:get_value(user, Options),
            Pass = proplists:get_value(password, Options),
            case User of
                undefined -> {error, euser};
                _ -> 
                    case ftpesI:start_client(Host, Port, {User, Pass}) of
                        {ok, Pid} -> {ok, Pid, undefined};
                        {error, Reason} -> {error, Reason}
                    end
            end
    end;
start_channel(sftp, Host, Port, Options)->
    ssh_sftp:start_channel(Host, Port, Options).

%%% ----------------------------------------------------------
%%%     #start_channel_only_alt/5
%%% @doc Starts a FTPES client or an SSH channel process is started 
%%% to handle the communication with the SFTP server.
%%% FTPES cannot choose IP interface, it connects to the available interface.
%%% LMT is always set.
%%% For SFTP first do the same as start_channel/5, if this fails and the
%%% connection isn't using LMT, try using the alternate
%%% OamAccessPoint.
%%% ----------------------------------------------------------
-spec start_channel_only_alt(ftpes|sftp,
                             Host :: string() | {string(), integer()},
                             Port :: integer(),
                             User :: string(),
                             Pass :: string()) ->
          {ok, pid(), term()} | {error, reason()}.

start_channel_only_alt(ftpes, Host, Port, User, Password) ->
    case ftpesI:start_client(Host, Port, {User, Password}) of
        {ok, Pid} -> {ok, Pid, undefined};
        {error, Reason} -> {error, Reason}
    end;
start_channel_only_alt(sftp, Host, Port, User, Password) ->
    sysSftp:start_channel_only_alt(Host, Port, User, Password).

%%% ----------------------------------------------------------
%%%     #start_channel_only_alt/6
%%% ----------------------------------------------------------
-spec start_channel_only_alt(ftpes|sftp,
                             Host :: string() | {string(), integer()},
                             Port :: integer(),
                             User :: string(),
                             Pass :: string(),
                             Opts :: list()) ->
          {ok, pid(), term()} | {error, reason()}.

start_channel_only_alt(ftpes, Host, Port, User, Password, _Opts) ->
    case ftpesI:start_client(Host, Port, {User, Password}) of
        {ok, Pid} -> {ok, Pid, undefined};
        {error, Reason} -> {error, Reason}
    end;
start_channel_only_alt(sftp, Host, Port, User, Password, Opts) ->
    sysSftp:start_channel_only_alt(Host, Port, User, Password, Opts).

%%% ----------------------------------------------------------
%%%     #start_channel_with_alt/5
%%%
%%% ----------------------------------------------------------
-spec start_channel_with_alt(ftpes|sftp,
                             Host :: string() | {string(), integer()},
                             Port :: integer(),
                             User :: string(),
                             Pass :: string())->
          {ok, pid(), term()} | {error, reason()}.
start_channel_with_alt(ftpes, Host, Port, User, Password) ->
    case ftpesI:start_client(Host, Port, {User, Password}) of
        {ok, Pid} -> {ok, Pid, undefined};
        {error, Reason} -> {error, Reason}
    end;
start_channel_with_alt(sftp, Host, Port, User, Password) ->   
    sysSftp:start_channel_with_alt(Host, Port, User, Password).

%%% ----------------------------------------------------------
%%%     #start_channel_with_alt/6
%%%
%%% ----------------------------------------------------------
-spec start_channel_with_alt(ftpes|sftp,
                             Host :: string() | {string(), integer()},
                             Port :: integer(),
                             User :: string(),
                             Pass :: string(),
                             Opts :: list())->
          {ok, pid(), term()} | {error, reason()}.
start_channel_with_alt(ftpes, Host, Port, User, Password, _Opts) ->
    case ftpesI:start_client(Host, Port, {User, Password}) of
        {ok, Pid} -> {ok, Pid, undefined};
        {error, Reason} -> {error, Reason}
    end;
start_channel_with_alt(sftp, Host, Port, User, Password, Opts) ->   
    sysSftp:start_channel_with_alt(Host, Port, User, Password, Opts).

%%% ----------------------------------------------------------
%%%     #start_channel/5
%%% @doc FTPES same as start_channel/4, SFTP mathod call goes through sysSftp.erl
%%% ---------------------------------------------------------- 
-spec start_channel(ftpes | sftp, 
                    Host :: string() | {string(), integer()},
                    Port :: integer(),
                    User :: string(),
                    Pass :: string()) ->
          {ok, pid(), term()} | {error, reason()} | term().
start_channel(ftpes, Host, Port, User, Password) ->
    case ftpesI:start_client(Host, Port, {User, Password}) of
        {ok, Pid} -> {ok, Pid, undefined};
        {error, Reason} -> {error, Reason}
    end;
start_channel(sftp, Host, Port, User, Password) ->
    sysSftp:start_channel(Host, Port, User, Password).


%%% ----------------------------------------------------------
%%%     #start_channel/6
%%% ---------------------------------------------------------- 
-spec start_channel(ftpes | sftp, 
                    Host :: string() | {string(), integer()},
                    Port :: integer(),
                    User :: string(),
                    Pass :: string(),
                    Opts :: list()) ->
          {ok, pid(), term()} | {error, reason()} | term().
start_channel(ftpes, Host, Port, User, Password, _Opts) ->
    case ftpesI:start_client(Host, Port, {User, Password}) of
        {ok, Pid} -> {ok, Pid, undefined};
        {error, Reason} -> {error, Reason}
    end;
start_channel(sftp, Host, Port, User, Password, Opts) ->
    sysSftp:start_channel(Host, Port, User, Password, Opts).

%%% ----------------------------------------------------------
%%%    # stop_channel/2
%%% @doc Stops an FTPES client or an SFTP channel.
%%% The first param is an atom, which can be ftpes (used for FTPES)
%%% or sftp (used for SFTP).
%%% ----------------------------------------------------------
-spec stop_channel(ftpes | sftp, 
                   Pid :: pid()) ->
          ok | {error, unsupported_protocol}.
stop_channel(ftpes, Pid) ->
    ftpesI:stop_client(Pid);
stop_channel(sftp, Pid) ->
    ssh_sftp:stop_channel(Pid);
stop_channel(_, _) ->
    {error, unsupported_protocol}.

%%% ----------------------------------------------------------
%%%    # stop_channel/3
%%% @doc Stops an FTPES client or an SFTP channel.
%%% SFTP call over sysSftp.erl
%%% ----------------------------------------------------------
-spec stop_channel(ftpes|sftp,
                   Pid :: pid(),
                   ConnectionRef :: term()) ->
          ok | {error, unsupported_protocol}.
stop_channel(ftpes, Pid, undefined) ->
    ftpesI:stop_client(Pid);
stop_channel(sftp, Pid, ConnectionRef) ->
    sysSftp:stop_channel(Pid, ConnectionRef);
stop_channel(_, _, _) ->
    {error, unsupported_protocol}.


%%% ----------------------------------------------------------
%%%    #open/4
%%% @doc For SFTP opens a file on the server and returns a handle, which can be used for 
%%% reading and writing. 
%%% For FTPES if the Mode is read starts transfer of the file File from the remote server.
%%% If Mode is write starts transfer of chunks into the File at the remote server.
%%% ----------------------------------------------------------
-spec open(ftpes | sftp,
           Pid :: pid(),
           File :: string(),
           OpenArgs ::  list() | {atom(), list()}) ->
          {ok, term()} | {error, reason()}. 
open(ftpes, Pid, File, OpenArgs) when is_list(OpenArgs)->
    open(ftpes, Pid, File, get_open_mode_args(OpenArgs));
open(ftpes, Pid, File, {read, OpenArgs}) ->
    case ftpesI:read_start(Pid, File, OpenArgs)of 
        ok -> 
            {ok, undefined};
        {error, Reason} ->
            {error, Reason}
    end;

open(ftpes, Pid, File, {write, _OpenArgs}) ->
    case ftpesI:write_start(Pid, File) of
        ok -> 
            {ok, undefined};
        {error, Reason} ->
            {error, Reason}
    end;
open(sftp, Pid, File, Mode) ->
    ssh_sftp:open(Pid, File, Mode).


%%% ----------------------------------------------------------
%%%    # close/3
%%% @doc For SFTP closes a handle to an open file or directory on the server.
%%% FTPES stops transfer of chunks to the remote server.
%%% ----------------------------------------------------------
-spec close(ftpes | sftp, 
            Pid :: pid(),
            Handle :: term() | list()) ->
          ok | {error, reason()}.
close(ftpes, Pid, _Handle) ->
    ftpesI:close(Pid);
close(sftp, _Pid, undefined) ->
    {error, undefined_handler};
close(sftp, Pid, Handle) ->
    ssh_sftp:close(Pid, Handle).


%%% ----------------------------------------------------------
%%%    #read/4
%%% @doc For SFTP reads Len bytes from the file referenced by Handle.
%%% FTPES receives a chunk of the remote file. If chunk is larger than
%%% requested, the rest is accumulated and returned on the next call
%%% ----------------------------------------------------------
-spec read(ftpes |sftp, 
           Pid :: pid(),
           Handle :: term(),
           Len :: integer()) ->
          {ok, iodata() } | eof | {error, reason()}. 
read(ftpes, Pid, _Handle, Len) ->
    case ftpesI:read_chunk(Pid, Len) of 
        ok -> eof;
        Other ->
            Other
    end;
read(sftp,Pid, Handle, Len)->
    ssh_sftp:read(Pid, Handle, Len).

%%% ----------------------------------------------------------
%%%    # read/5
%%% @doc
%%% ----------------------------------------------------------
-spec read(ftpes |sftp, 
           Pid :: pid(),
           Handle :: term(),
           Len :: integer(),
           Timeout :: integer()) ->
          {ok, iodata() } | eof | {error, reason()}.
read(ftpes, Pid, _Handle, Len, Timeout) ->
    case ftpesI:read_chunk(Pid, Len, Timeout) of 
        ok -> eof;
        Other ->
            Other
    end;
read(sftp, Pid, Handle, Len, Timeout) ->
    ssh_sftp:read(Pid, Handle, Len, Timeout).

%%% ----------------------------------------------------------
%%%    #aread/4
%%% @doc For SFTP reads Len bytes from the file referenced by Handle.
%%% FTPES receives a chunk of the remote file.
%%% ----------------------------------------------------------
-spec aread(ftpes | sftp, 
            Pid :: pid(),
            Handle :: term(),
            Len :: integer()) ->
          {async, term()} | {error, reason()}.
aread(ftpes, Pid, _Handle, Len)->
    ftpesI:aread(Pid, Len);
aread(sftp, Pid, Handle, Len)->
    ssh_sftp:aread(Pid, Handle, Len).


%%% ----------------------------------------------------------
%%%    # write/4
%%% @doc SFTP writes data to the file referenced by Handle.
%%% FTPES transfers the chunk Data to the remote server, which writes it 
%%% into the file specified in the call to send_chunk_start/2.
%%% ----------------------------------------------------------
-spec write(ftpes | sftp,
            Pid :: pid(),
            Handle :: term(),
            Data :: iodata()) ->
          ok | {error, reason()}.
write(ftpes, Pid, _Handle, Data ) ->
    ftpesI:write(Pid, Data);
write(sftp, Pid, Handle, Data) ->
    ssh_sftp:write(Pid, Handle, Data).

%%% ----------------------------------------------------------
%%%    #write/5
%%% @doc 
%%% ----------------------------------------------------------
-spec write(ftpes | sftp,
            Pid :: pid(),
            Handle :: term(),
            Data :: iodata(),
            Timeout :: integer()) ->
          ok | {error, reason()}.
write(ftpes, Pid, _Handle, Data, Timeout) ->
    ftpesI:write(Pid, Data, Timeout);
write(sftp, Pid, Handle, Data, Timeout) ->
    ssh_sftp:write(Pid, Handle, Data, Timeout).


%%% ----------------------------------------------------------
%%%    # delete/3
%%% @doc Deletes the file specified by Name. 
%%% ----------------------------------------------------------
-spec delete(ftpes |sftp, 
             Pid :: pid(),
             Name :: string()) ->
          ok | {error, reason()}.
delete(ftpes, Pid, Name) ->
    ftpesI:delete(Pid, Name);
delete(sftp, Pid, Name) ->
    ssh_sftp:delete(Pid, Name).

%%% ----------------------------------------------------------
%%%    # delete/4
%%% @doc 
%%% ----------------------------------------------------------
-spec delete(ftpes |sftp, 
             Pid :: pid(),
             Name :: string(),
             Timeout :: integer()) ->
          ok | {error, reason()}.
delete(ftpes, Pid, Name, Timeout) ->
    ftpesI:delete(Pid, Name, Timeout);
delete(sftp, Pid, Name, Timeout) ->
    ssh_sftp:delete(Pid, Name, Timeout).

%%% ----------------------------------------------------------
%%%    # list_dir/3
%%% @doc SFTP lists the given directory on the server, returning the 
%%% filenames as a list of strings.
%%% FTPES returns a list of files in long format.Pathname can be a 
%%% directory, a group of files, or a file.
%%% ----------------------------------------------------------
-spec list_dir(ftpes |sftp, 
               Pid :: pid(),
               Path :: string()) ->
          {ok, string()} | {error, reason()}.
list_dir(ftpes, Pid, Path ) ->
    case ftpesI:nlist(Pid, Path) of
        {ok, RawDirData} -> {ok, [filename:basename(string:strip(File)) || File <- string:tokens(RawDirData, "\r\n")]};
        {error, Reason} -> {error, Reason}
    end;
list_dir(sftp, Pid, Path) ->
    ssh_sftp:list_dir(Pid, Path).

%%% ----------------------------------------------------------
%%%    # list_dir/4
%%% @doc
%%% ----------------------------------------------------------
-spec list_dir(ftpes |sftp, 
               Pid :: pid(),
               Path :: string(),
               Timeout :: integer()) ->
          {ok, string()} | {error, reason()}.
list_dir(ftpes, Pid, Path, Timeout) ->
    case ftpesI:nlist(Pid, Path, Timeout) of
        {ok, RawDirData} -> {ok, [filename:basename(string:strip(File)) || File <- string:tokens(RawDirData, "\r\n")]};
        {error, Reason} -> {error, Reason}
    end;
list_dir(sftp, Pid, Path, Timeout) ->
    ssh_sftp:list_dir(Pid, Path, Timeout).

%%% ----------------------------------------------------------
%%%    # read_file/3
%%% @doc SFTP reads a file from the server, and returns the data in a binary.
%%% FTPES transfers the file RemoteFile from the remote server to the
%%% file system of the local client.
%%% ----------------------------------------------------------
-spec read_file(ftpes | sftp, 
                Pid :: pid(),
                File :: string()) ->
          {ok, binary()} | {error, reason()}.
read_file(ftpes, Pid, File) ->
    ftpesI:read_file(Pid, File);
read_file(sftp, Pid, File) ->
    ssh_sftp:read_file(Pid, File).

%%% ----------------------------------------------------------
%%%    # read_file/4
%%% @doc 
%%% ----------------------------------------------------------
-spec read_file(ftpes | sftp, 
                Pid :: pid(),
                File :: string(), 
                Timeout :: integer()) ->
          {ok, binary()} | {error, reason()}.
read_file(ftpes, Pid, File, Timeout) ->
    ftpesI:read_file(Pid, File, Timeout);
read_file(sftp, Pid, File, Timeout) ->
    ssh_sftp:read_file(Pid, File, Timeout).

%%% ----------------------------------------------------------
%%%    # recv_file/3
%%% @doc
%%% ----------------------------------------------------------
-spec recv_file(Pid::pid(),
                RemotePath :: string(),
                LocalFile :: string()) ->
          ok | {error, reason()}.
recv_file(Pid, RemotePath, LocalFile) ->
    ftpesI:recv_file(Pid, RemotePath, LocalFile).


%%% ----------------------------------------------------------
%%%    # write_file/4
%%% @doc SFTP writes a file to the server. 
%%% FTPES transfers the file to the remote server.
%%% ----------------------------------------------------------
-spec write_file(ftpes | sftp,
                 Pid :: pid(),
                 File :: string() | binary(),
                 Iolist :: iodata()) -> 
          ok | {error, reason()}.
write_file(Proto, Pid, File, Iolist) when is_binary(File)->
    write_file(Proto, Pid, binary_to_list(File), Iolist);
write_file(ftpes, Pid, File, Iolist)->
    ftpesI:write_file(Pid, Iolist, File);
write_file(sftp, Pid, File, Iolist) ->
    ssh_sftp:write_file(Pid, File, Iolist).

%%% ----------------------------------------------------------
%%%     # write_file/5
%%% @doc
%%% ----------------------------------------------------------
-spec write_file(ftpes | sftp,
                 Pid :: pid(),
                 File :: string() | binary(),
                 Iolist :: iodata(), 
                 Timeout :: integer()) -> 
          ok | {error, reason()}.
write_file(Proto, Pid, File, Iolist, Timeout) when is_binary(File)->
    write_file(Proto, Pid, binary_to_list(File), Iolist, Timeout);
write_file(ftpes, Pid, File, Iolist, Timeout)->
    ftpesI:write_file(Pid, Iolist, File, Timeout);
write_file(sftp, Pid, File, Iolist, Timeout) ->
    ssh_sftp:write_file(Pid, File, Iolist, Timeout).


%%% ----------------------------------------------------------
%%% -type put_file(Pid, LocalPath, RemotePath)              %#
%%%     ok | error().                                       %#
%%% Input: Pid - A ssh_sftp channel pid
%%%        LocalPath - Path to the local file being transferred
%%%        RemotePath - Destination path on the remote server
%%% Output: ok
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

-spec put_file(ftpes | sftp, 
               Pid::pid(), 
               LocalPath::string(), 
               RemotePath::string()) ->
          ok |
              {error, Module::any(), Reason::any()}.

put_file(ftpes, Pid, LocalPath, RemotePath) ->
    case ftpesI:put_file(Pid, LocalPath, RemotePath) of
        ok -> ok;
        {error, Reason} -> {error, ftpes, Reason}
    end;
put_file(sftp, Pid, LocalPath, RemotePath) ->
    sysSftp:put_file(Pid, LocalPath, RemotePath).

%%% ----------------------------------------------------------
%%% -type parse_uri(Uri)                                     %#
%%%     {ok, Result} | {error, Reason}.                      %#
%%% Input: URI - resource path
%%% Output: {ok, Result} | {error, Reason}
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

-spec parse_uri(Uri::string()) ->
          {ok, tuple()} |{error, tuple()}.

parse_uri(Uri) ->
    Schema = http_uri:scheme_defaults() ++ [{ftpes, 21}],
    %%HW33598 Support encoded URIs
    http_uri:parse(http_uri:decode(Uri), [{ipv6_host_with_brackets, true}, {scheme_defaults, Schema}]).

%%% ----------------------------------------------------------
%%%     # format_error/2
%%% @doc Given an error return value, this function returns a tuple
%%% {error_return_value, error_described}
%%% ----------------------------------------------------------
-spec format_error(Proto :: atom(),
                   Error :: {error, atom()} | atom() ) ->
                   string().

format_error(Proto, {error, Reason}) -> format_error(Proto, Reason);

format_error(ftpes, not_active) -> "FTPES protocol is not supported.";
format_error(ftpes, reconf_error) -> "TLS configuration change detected, retry operation.";
format_error(ftpes, no_proc) -> "FTPES client not running.";
format_error(ftpes, no_tls) -> "Invalid TLS configuration.";
format_error(ftpes, closed) -> "FTPES client connection closed.";
format_error(ftpes, request_pending) -> "FTPES client is busy performing a request.";
format_error(ftpes, timeout) -> "FTPES operation timed out.";
format_error(ftpes, not_found) -> "Unknown FTPES client.";
format_error(ftpes, Error) -> ftp:formaterror(Error);

format_error(sftp, etimedout) -> "Cannot establish a connection to remote server.";
format_error(sftp, no_such_file) -> "No such file or directory.";
format_error(sftp, nxdomain) -> "Domain name not found.";
format_error(sftp, econnrefused) -> "The remote server refused the connection.";
format_error(sftp, einval) -> "Invalid parameters.";
format_error(sftp, closed) -> "The remote server closed the connection";
format_error(sftp, Error) -> lists:flatten(io_lib:format("~p",[Error])).

%%% ----------------------------------------------------------
%%% get_open_mode_args/1
%%% @doc Parses the arguments of open command and returns mode (read or write)
%%% and parameters which are handled by ftpes (others are ignored)
%%% ----------------------------------------------------------
-spec get_open_mode_args(Args :: list()) -> {atom(), list()}.
get_open_mode_args(Args) ->
    get_open_mode_args(Args, {undefined, []}).

get_open_mode_args([], Result) ->
    Result;

get_open_mode_args([Mode | Args], {undefined, FilteredArgs}) 
                    when Mode =:= read orelse Mode =:= write ->
    get_open_mode_args(Args, {Mode, FilteredArgs});

get_open_mode_args([binary | Args], {Mode, FilteredArgs}) ->
    get_open_mode_args(Args, {Mode, [binary | FilteredArgs]});

get_open_mode_args([_Other | Args], {Mode, FilteredArgs}) ->
    get_open_mode_args(Args, {Mode, FilteredArgs}).
