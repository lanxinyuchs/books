%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsSshSftpdFile.erl %
%%% @hidden
%%% Author:	eolaand, erarafo
%%%
%%% Description: This module contains functions that intercept
%%% SFTP callbacks according to the (OTP) ssh_sftpd_file_api
%%% behaviour.
%%%
%%% Only a subset of the ssh_sftpd_file_api callbacks need to be
%%% declared here. The purpose of declaring a callback is to allow
%%% some special handling of that particular callback.
%%% 
%%% A function declared here may decide to not do any special
%%% handling, based e g on an inspection of a path argument or
%%% such. In that case the function should return 'ignore', giving
%%% other interceptor modules opportunity to handle the callback.
%%% Otherwise the function must return a term of the same format as
%%% the functions in (OTP) ssh_sftpd_file return.
%%% 
%%% This module must be registered with sysFi, using the
%%% sysFi:register_sftp_handler_module/1 function.
%%%
%%% The sysFi process maintains a list of registered interceptor
%%% modules. For each callback all interceptor modules will be tried
%%% in sequence until some module defines a matching callback function
%%% and this function returns a non-'ignore' result. If the list of
%%% modules is exhausted the default handling defined in
%%% sysSshSftpdFile will take place.
%%% 
%%% Interceptor functions should be written so that module order is
%%% not significant. This may not always be possible; for that reason
%%% the modules list is built in a well-defined order where order of
%%% registration does not matter. It is also possible to prescribe
%%% modules order by associating an ordering weight to known module
%%% names in sysFi:moduleOrder/1.
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(pmsSshSftpdFile).
-vsn('/main/R2A/R3A/R4A/R8A/1').
-date('2017-01-17').
-author('estjako').
-shaid('9b8478e4c23c2c0ed73a28e8d78b956ed5e47d23').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% R1A/R2A/1  2013-01-31 eolaand     First version
%%% R3A/1      2015-01-14 erarafo     Support deletion of non-rop files
%%% R3A/2      2015-01-15 erarafo     Some tentative Edoc added
%%% R3A/3      2015-01-15 erarafo     Workaround to allow "cd"
%%% R3A/4      2015-01-20 erarafo     Interceptor pattern
%%% R3A/5      2015-01-21 erarafo     Edoc
%%% R3A/6      2015-02-12 eolaand     Change from mnesia table storage to disc
%%% R4A/3      2015-11-24 eolaand     Setting of atime no longer needed
%%% R4A/4      2015-11-24 eolaand     Revert back to setting of atime
%%% R8A/1      2017-01-17 estjako     open/3 changed to support ftpes
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% API


-export([
	 delete/2,
	 open/3
	]).

%% -compile(export_all).

-include_lib("kernel/include/file.hrl").

-define(READ, 33060).
-define(READ_WRITE, 33188).
-define(ROP_DIR, rop_dir).
-define(ROP_FILE, rop_file).

%%%===================================================================
%%% API
%%%===================================================================
%%% ----------------------------------------------------------
%%% @doc Client commands that invoke this function: rm.
%%% @end
%%% ----------------------------------------------------------
delete(Path, State) ->
    case path_type(Path) of
	?ROP_FILE ->
	    {delete_file(Path), State};
	_Other ->
	    ignore
    end.

%%% ----------------------------------------------------------
%%% @doc Client commands that invoke this function: get.
%%% The passed Path is filesystem absolute. Flags may be
%%% e g [binary,read].
%%% @end
%%% ----------------------------------------------------------
open(Path, [binary, read], State) ->
    case path_type(Path) of
    	?ROP_FILE ->
    	    BaseName = filename:basename(Path),
	    pmsDb:rop_atime_set(BaseName),
	    ignore;
    	_Other ->
    	    {{error, eacces}, State}
    end;

open(_Path, _Flags, State) ->
    User = sysFi:get_session_info(State, user),
    sysInitI:warning_msg("User ~p tried to put file:~n~p~n"
			 "Access denied!~n", [User, _Path]),
    {{error, eacces}, State}.
	    

%%%===================================================================
%%% Internal functions
%%%===================================================================
delete_file(Path) ->
    BaseName = filename:basename(Path),
    case get_file_info(BaseName) of
	{ok, #file_info{access = Access}} when Access =:= read_write ->
	    pmsDb:rop_file_delete(BaseName);
	{ok, _} ->
	    {error, eacces};
	Error ->
	    Error
    end.


path_type(Path) ->
    % If Path is a relative filename then the current directory,
    % which is typically a user home directory such as /home/sirpa
    % or /local/scratch/SIGNUM/RCS_ROOT/home/SIGNUM, is prepended
    % to the given Path. Apparently this case is not meaningful in
    % the context of the SFTP service.
    %
    % Otherwise if Path is already absolute then the given value
    % is returned. This is what occurs in test suites.
    AbsPath = filename:absname(Path),
    Dir = filename:dirname(AbsPath),
    case pmsSftpdEnv:rop_dir_path() of
	AbsPath ->
	    ?ROP_DIR;
	Dir ->
	    ?ROP_FILE;
	_RopDir ->
	    other
    end.


get_file_info(File) ->
    case pmsDb:rop_file_info(File) of
	{ok, #file_info{mtime = MTime, atime = ATime} = FileInfo} ->
	    IsFetched = MTime < ATime,
	    Access = get_access(IsFetched),
	    Mode = get_mode(IsFetched),
	    {ok, FileInfo#file_info{access = Access, mode = Mode}};
	Error ->
	    Error
    end.


get_access(false) ->
    read;

get_access(_True) ->
    read_write.


get_mode(false) ->
    ?READ;

get_mode(_True) ->
    ?READ_WRITE.


