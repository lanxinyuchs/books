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
%%% %CCaseFile:	ftpesd_dir.erl %
%%% @author ekurnik
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R8A/R9A/2
%%%
%%% ----------------------------------------------------------
-module(ftpesd_dir).
-vsn('/main/R8A/R9A/2').
-date('2017-02-10').
-author('ekurnik').
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
%%% R8A/4    2016-12-13   ekurnik    Added ftpes_root()
%%% R8A/5    2017-01-05   emarnek    Added intercept
%%% R8A/6    2017-01-10   ekurnik    Added absolute path support
%%% R8A/7    2017-01-12   ekurnik    Added ftpes tag to State in intercept
%%% R9A/1    2017-01-24   ekurnik    Fixed warnings
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([ftpes_root/0,
         short_list_dir/4,
         long_list_dir/4,
         canonicalize_path/1,
         open/3,
         close/2,
         read/3,
         write/3,
         make_dir/2,
         del_dir/2,
         delete/2,
         rename/3]).

-include("ftpesd.hrl").

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%% currently, FTPES root dir is same as sftp
ftpes_root() ->
    sysFi:sftp_root().

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc List all files in current directory in short format
%%%      used in NLST, Cwd is full path!
%%% ----------------------------------------------------------
-spec short_list_dir(RootDir :: string(),
					 Cwd :: string(),
					 Path :: string(),
					 State :: #user_data{}) -> {ok, [string()]}.
%%% ###=====================================================================###
short_list_dir(RootDir, Cwd, Path, State) ->
    case filename:pathtype(Path) of
        absolute -> short_list_absolute_dir(RootDir, Path, State);
        relative -> short_list_relative_dir(RootDir, Cwd, Path, State)
    end.

short_list_absolute_dir(RootDir, LPath, State) ->
    % Needs to canonicalize the path, otherwise .. would go out of the chroot
    LocalPath = canonicalize_path(LPath),
    case list_dir(RootDir++LocalPath, State) of
    {ok, FileNames} ->
        {ok, [ftpesd_util:concat_paths(LocalPath, FileName) || FileName <- lists:sort(FileNames)]};
    {error, enotdir} ->
        % Path is a file
        {ok, [LocalPath]};
    _Error ->
        % Intentional: both the Solaris and Linux servers seems to
        % return empty data on error instead of error codes
        {ok, []}
    end.

short_list_relative_dir(RootDir, Cwd, CPath, State) ->
    % Needs to canonicalize the path, otherwise .. would go out of the chroot
    LocalPath = canonicalize_path(ftpesd_util:concat_paths(Cwd, CPath)),
    case list_dir(RootDir++LocalPath, State) of
    {ok, FileNames} ->
        case CPath of
        "" ->
            {ok, lists:sort(FileNames)};
        _ ->
            {ok, [ftpesd_util:concat_paths(CPath, FileName) || FileName <- lists:sort(FileNames)]}
        end;
    {error, enotdir} ->
        % Path is a file
        {ok, [CPath]};
    _Error ->
        % Intentional: both the Solaris and Linux servers seems to
        % return empty data on error instead of error codes
        {ok, []}
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc List all files in current directory
%%% ----------------------------------------------------------
-spec long_list_dir(RootDir :: string(),
					Cwd :: string(),
					Path :: string(),
					State :: #user_data{}) -> {ok, [string()]}.
%%% ###=====================================================================###
long_list_dir(RootDir, Cwd, Path, State) ->
    LocalPath = RootDir++ftpesd_util:concat_paths(Cwd, Path),
    case list_dir(LocalPath, State) of
    {ok, FileNames} ->
        {ok, [ftpesd_util:get_file_info(ftpesd_util:concat_paths(LocalPath,FileName)) || FileName <- lists:sort(FileNames)]};
    {error, enotdir} ->
        % Path is a file
        {ok, [ftpesd_util:get_file_info(LocalPath)]};
    _Error ->
        % Intentional: both the Solaris and Linux servers seems to
        % return empty data on error instead of error codes
        {ok, []}
    end.

canonicalize_path(Path) ->
    PathElements = filename:split(Path),
    canonicalize_path(PathElements, [""]).

canonicalize_path([], ResultList) ->
    filename:join(lists:reverse(ResultList));
canonicalize_path(["." | Rest], ResultList) ->
    canonicalize_path(Rest, ResultList);
% don't crash when somebody wants to go upper than the root
canonicalize_path([".." | Rest], ["/" | _ ]) ->
    canonicalize_path(Rest, ["/"]);
canonicalize_path([".." | Rest], [_Parent | ResultList]) ->
    canonicalize_path(Rest, ResultList);
canonicalize_path([Dir | Rest], ResultList) ->
    canonicalize_path(Rest, [Dir | ResultList]).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Opens a file specified in Path, used in RETR
%%% ----------------------------------------------------------
-spec open(Path :: string(),
		   Flags :: [atom()],
		   State :: #user_data{}) -> {ok, pid()} | {error, reason()}.
%%% ###=====================================================================###
open(Path, Flags, State) ->
    Write = lists:member(write, Flags),
    case intercept(Path, open, [Path, Flags, State]) of
    ignore when Write ->
        {error, eacces};
    ignore ->
        file:open(Path, Flags);
    {Result, State} ->
        Result
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Closes a file referenced by IoDevice
%%% ----------------------------------------------------------
-spec close(tuple() | atom() | pid(),
		    State :: #user_data{}) -> ok | {error, reason()}.
%%% ###=====================================================================###
close({Type, Mod, IoDevice}, State) ->
    case intercept(Type, Mod, close, [IoDevice, State]) of
	ignore ->
	    file:close(IoDevice);
	{Result, State} ->
	    Result
    end;

close(IoDevice, _State) ->
    file:close(IoDevice).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Reads Number bytes/characters from the file referenced by IoDevice
%%% ----------------------------------------------------------
-spec read(tuple() | pid(),
		   Number :: integer(),
		   State :: #user_data{}) -> {ok, string() | binary()} | eof | {error, reason()}.
%%% ###=====================================================================###
read({Type, Mod, IoDevice}, Number, State) ->
    case intercept(Type, Mod, read, [IoDevice, Number, State]) of
	ignore ->
	    file:read(IoDevice, Number);
	{Result, State} ->
	    Result
    end;

read(IoDevice, Number, _State) ->
    file:read(IoDevice, Number).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Write Data to the file referenced by IoDevice
%%% ----------------------------------------------------------
-spec write(tuple() | pid(),
		   Data :: iodata(),
		   State :: #user_data{}) -> ok | {error, reason()}.
%%% ###=====================================================================###
write({Type, Mod, IoDevice}, Data, State) ->
    case intercept(Type, Mod, write, [IoDevice, Data, State]) of
	ignore ->
	    {error, eacces};
	{Result, State} ->
	    Result
    end;

write(_IoDevice, _Data, _State) ->
    {error, eacces}.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Tries to create directory Dir
%%% ----------------------------------------------------------
-spec make_dir(Dir :: string(),
			   State :: #user_data{}) -> ok | {error, reason()}.
%%% ###=====================================================================###
make_dir(Dir, State) ->
    case intercept(filename:dirname(absname(Dir)), make_dir, [Dir, State]) of
    ignore ->
        {error, eacces};
    {Result, State} ->
        Result
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Tries to delete directory Dir
%%% ----------------------------------------------------------
-spec del_dir(Path :: string(),
			  State :: #user_data{}) -> ok | {error, reason()}.
%%% ###=====================================================================###
del_dir(Path, State) ->
    case intercept(filename:dirname(absname(Path)), del_dir, [Path, State]) of
    ignore ->
        {error, eacces};
    {Result, State} ->
        Result
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Tries to delete file Path
%%% ----------------------------------------------------------
-spec delete(Path :: string(),
			 State :: #user_data{}) -> ok | {error, reason()}.
%%% ###=====================================================================###
delete(Path, State) ->
    case intercept(Path, delete, [Path, State]) of
    ignore ->
        file:delete(Path);
    {Result, State} ->
        Result
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Tries to rename the file Path to Path2
%%% ----------------------------------------------------------
-spec rename(Path :: string(),
			 Path2 :: string(),
			 State :: #user_data{}) -> ok | {error, reason()}.
%%% ###=====================================================================###
rename(Path, Path2, State) ->
    case intercept(Path, rename, [Path, Path2, State]) of
    ignore ->
        {error, eacces};
    {Result, State} ->
        Result
    end.

list_dir(Path, State) ->
    case intercept(Path, list_dir, [Path, State]) of
    ignore ->
        file:list_dir(Path);
    {Result, State} ->
        Result
    end.

intercept(Path, Op, Args) ->
    case get_sftp_dir(Path) of
    undefined ->
        ?LOG("~nModule not found.~nPath: ~p~nOperation: ~p~nArgs: ~p~n", [Path, Op, Args]),
        ignore;
    {Type, Mod} ->
        intercept(Type, Mod, Op, Args)
    end.

intercept(Type, Mod, Op, Args) ->
    case do_intercept(Mod, Op, Args) of
    {{ok, FD}, State} when Op =:= open ->
        {{ok, {Type, Mod, FD}}, State};
    Res ->
        Res
    end.

do_intercept(Mod, Op, Args) when is_atom(Mod), Mod =/= undefined ->
    ?LOG("~nModule: ~p~nOperation: ~p~nArgs: ~p~n", [Mod, Op, Args]),
    NewArgs = add_state_tag(Args, ftpes),
    try
    Arity = length(NewArgs),
    case erlang:function_exported(Mod, Op, Arity) of
        false ->
            ignore;
        true ->
            case apply(Mod, Op, NewArgs) of
            {Result, {ftpes, State}} ->
                {Result, State};
            ignore ->
                ignore
            end
    end
    catch
    ExType:ExData ->
        sysInitI:warning_msg(
          "FTPES callback, interceptor failed, mfa: ~p, ~w, ~p",
          [{Mod, Op, NewArgs}, ExType, ExData]),
        ignore
    end;
    
do_intercept(_Mod, _Op, _Args) ->
    ?LOG("~nModule not found.~nOperation: ~p~nArgs: ~p~n", [_Op, _Args]),
    ignore.

get_sftp_dir(Path) ->
    AbsName = filename:absname(Path),
    case filelib:is_dir(AbsName) of
    true ->
        sysFi:get_sftp_reg_dir(AbsName);
    _False ->
        Dir = filename:dirname(AbsName),
        sysFi:get_sftp_reg_dir(Dir)
    end.

absname("/" = Path) ->
    Path;

absname(Path) ->
    AbsName = filename:absname(Path),
    RootDir = ftpes_root(),
    case lists:prefix(RootDir, AbsName) of
    true -> 
        AbsName;
    _False ->
        sysFi:sftp_absname(AbsName)
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Function which tags the State so it is recognized to belong to FTPES session
%%% ----------------------------------------------------------
-spec add_state_tag(Args :: list(),
                    Tag :: atom()) -> list().
%%% ###=====================================================================###
add_state_tag(Args, Tag) ->
    lists:map(fun(#user_data{} = UserData) -> {Tag, UserData};
                  (Other) -> Other end, Args).

