%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lih_fetch.erl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(lih_fetch).
-vsn('/main/R1A/3').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012 All rights reserved.
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([ftp/2, sftp/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

-spec ftp({Host, User, Password, RemoteFile}, LocalFile) -> Result when
      Host :: string(),
      User :: string(),
      Password :: string(),
      RemoteFile :: string(),
      LocalFile :: string(),
      Result :: 'ok' | {'error', term()}.
ftp({Host, User, Password, RemoteFile}, LocalFile) ->
    case ftp:open(Host) of
	{ok, Pid} ->
	    ftp({Pid, User, Password, RemoteFile, LocalFile});
	{error, Reason} ->
	    {error, Reason}
    end.

-spec sftp({Host, User, Password, RemoteFile}, LocalFile) -> Result when
      Host :: string(),
      User :: string(),
      Password :: string(),
      RemoteFile :: string(),
      LocalFile :: string(),
      Result :: 'ok' | {'error', term()}.
sftp({Host, User, Password, RemoteFile}, LocalFile) ->
    case ssh_sftp:start_channel(Host, [{silently_accept_hosts, true},
				       {user, User}, {password, Password}]) of
	{ok, Pid, ConnectionRef} ->
	    sftp({Pid, ConnectionRef, RemoteFile, LocalFile});
	{error, Reason} ->
	    {error, Reason}
    end.

ftp({Pid, User, Password, RemoteFile, LocalFile}) ->
    try
	ok = ftp:user(Pid, User, Password),
	ok = ftp:type(Pid, binary),
	ok = filelib:ensure_dir(LocalFile),
	ok = ftp:recv(Pid, RemoteFile, LocalFile)
    of
	ok -> ok
    catch
	error:{badmatch, {error, Reason}} ->
	    {error, Reason}
    after
        ftp:close(Pid)
    end.

sftp({Pid, ConnectionRef, RemoteFile, LocalFile}) ->
    try
	{ok, Data} = ssh_sftp:read_file(Pid, RemoteFile),
	ok = filelib:ensure_dir(LocalFile),
	ok = file:write_file(LocalFile, Data)
    of
	ok -> ok
    catch
	error:{badmatch, {error, Reason}} ->
	    {error, Reason}
    after
	ok = ssh_sftp:stop_channel(Pid),
	ok = ssh:close(ConnectionRef)
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


