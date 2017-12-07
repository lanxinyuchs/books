%%% ----------------------------------------------------------
%%% %CCaseFile:	sysSshSftpdFile.erl %
%%% Author:	erarafo
%%% Description: RBS CS implementation of ssh_sftpd_file_api,
%%% with an interception facility. Default behaviours are those
%%% of ssh_sftpd_file_api, except that the del_dir, make_dir,
%%% make_symlink, rename, write and write_file_info operations
%%% are treated as errors.
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(sysSshSftpdFile).
-behaviour(ssh_sftpd_file_api).
-id('Updated by CCase').
-vsn('/main/R3A/R4A/2').
-date('2015-11-02').
-author('eolaand').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015 All rights reserved.
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
%%% REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R3A/1      erarafo    2015-01-20  First version
%%% R3A/2      eolaand    2015-02-09  Changed callbacks to register for dir
%%% ----------------------------------------------------------


%% behaviour functions
-export([close/2, 
	 delete/2, 
	 del_dir/2, 
	 get_cwd/1, 
	 is_dir/2, 
	 list_dir/2, 
	 make_dir/2, 
	 make_symlink/3, 
	 open/3, 
	 position/3, 
	 read/3,
	 read_file_info/2, 
	 read_link/2, 
	 read_link_info/2, 
	 rename/3,
	 write/3, 
	 write_file_info/3
	]).


-include_lib("kernel/include/file.hrl").

-define(FD_TAG, ssh_fd).


close({?FD_TAG, {Type, Mod, IoDevice}}, State) ->
    case intercept(Type, Mod, close, [IoDevice, State]) of
	ignore ->
	    ssh_sftpd_file:close(IoDevice, State);
	Result ->
	    Result
    end;

close(IoDevice, State) ->
    ssh_sftpd_file:close(IoDevice, State).


delete(Path, State) ->
    case intercept(Path, delete, [Path, State]) of
	ignore ->
	    ssh_sftpd_file:delete(Path, State);
	Result ->
	    Result
    end.


del_dir(Path, State) ->
    case intercept(Path, del_dir, [Path, State]) of
	ignore ->
	    % ssh_sftpd_file:del_dir(Path, State);
	    {{error, eacces}, State};
	Result ->
	    Result
    end.


get_cwd(State) ->
    case ssh_sftpd_file:get_cwd(State) of
	{{ok, Path}, _State} ->
	    intercept_get_cwd(Path, State);
	Error ->
	    Error
    end.


is_dir(AbsPath, State) ->
    case intercept(AbsPath, is_dir, [AbsPath, State]) of
	ignore ->
	    ssh_sftpd_file:is_dir(AbsPath, State);
	Result ->
	    Result
    end.


list_dir(AbsPath, State) ->
    case intercept(AbsPath, list_dir, [AbsPath, State]) of
	ignore ->
	    ssh_sftpd_file:list_dir(AbsPath, State);
	Result ->
	    Result
    end.


make_dir(Dir, State) ->
    case intercept(filename:dirname(absname(Dir)), make_dir, [Dir, State]) of
	ignore ->
	    % ssh_sftpd_file:make_dir(Dir, State);
	    {{error, eacces}, State};
	Result ->
	    Result
    end.


make_symlink(Path2, Path, State) ->
    case intercept(Path, make_symlink, [Path2, Path, State]) of
	ignore ->
	    % ssh_sftpd_file:make_symlink(Path2, Path, State);
	    {{error, eacces}, State};
	Result ->
	    Result
    end.


open(Path, Flags, State) ->
    Write = lists:member(write, Flags),
    case intercept(Path, open, [Path, Flags, State]) of
	ignore when Write ->
	    {{error, eacces}, State};
	ignore ->
	    ssh_sftpd_file:open(Path, Flags, State);
	Result ->
	    Result
    end.


position({?FD_TAG, {Type, Mod, IoDevice}}, Offs, State) ->
    case intercept(Type, Mod, position, [IoDevice, Offs, State]) of
	ignore ->
	    ssh_sftpd_file:position(IoDevice, Offs, State);
	Result ->
	    Result
    end;

position(IoDevice, Offs, State) ->
    ssh_sftpd_file:position(IoDevice, Offs, State).


read({?FD_TAG, {Type, Mod, IoDevice}}, Len, State) ->
    case intercept(Type, Mod, read, [IoDevice, Len, State]) of
	ignore ->
	    ssh_sftpd_file:read(IoDevice, Len, State);
	Result ->
	    Result
    end;

read(IoDevice, Len, State) ->
    ssh_sftpd_file:read(IoDevice, Len, State).


read_link(Path, State) ->
    case intercept(Path, read_link, [Path, State]) of
	ignore ->
	    ssh_sftpd_file:read_link(Path, State);
	Result ->
	    Result
    end.


read_link_info(Path, State) ->
    case intercept(Path, read_link_info, [Path, State]) of
	ignore ->
	    ssh_sftpd_file:read_link_info(Path, State);
	Result ->
	    Result
    end.


read_file_info(Path, State) ->
    case intercept(Path, read_file_info, [Path, State]) of
	ignore ->
	    ssh_sftpd_file:read_file_info(Path, State);
	Result ->
	    Result
    end.


rename(Path, Path2, State) ->
    case intercept(Path, rename, [Path, Path2, State]) of
	ignore ->
	    % ssh_sftpd_file:rename(Path, Path2, State);
	    {{error, eacces}, State};
	Result ->
	    Result
    end.


write({?FD_TAG, {Type, Mod, IoDevice}}, Data, State) ->
    case intercept(Type, Mod, write, [IoDevice, Data, State]) of
	ignore ->
	    % ssh_sftpd_file:write(IoDevice, Data, State);
	    {{error, eacces}, State};
	Result ->
	    Result
    end;

write(_IoDevice, _Data, State) ->
    %% ssh_sftpd_file:write(IoDevice, Data, State);
    {{error, eacces}, State}.


write_file_info(Path, Info, State) ->
    case intercept(Path, write_file_info, [Path, Info, State]) of
	ignore ->
	    % ssh_sftpd_file:write_file_info(Path, Info, State);
	    {{error, eacces}, State};
	Result ->
	    Result
    end.


intercept_get_cwd(Path, State) ->
    %% This is a bit fishy but this function doesn't seem to be used anyway.
    case intercept(Path, get_cwd, [State]) of
	ignore ->
	    {{ok, Path}, State};
	Result ->
	    Result
    end.


intercept(Path, Op, Args) ->
    case get_sftp_dir(Path) of
	undefined when Op =:= read_link ->
	    read_link_ram_dir(Args);
	undefined ->
	    ignore;
	{Type, Mod} ->
	    intercept(Type, Mod, Op, Args)
    end.


intercept(Type, Mod, Op, Args) ->
    case do_intercept(Mod, Op, Args) of
	ignore when Type =:= ram,
		    Op =:= read_link ->
	    read_link_ram_dir(Args);
	ignore when Type =:= ram,
		    Op =:= read_link_info ->
	    read_link_info_ram_dir(Args);
	{{ok, FD}, State} when Op =:= open ->
	    {{ok, {?FD_TAG, {Type, Mod, FD}}}, State};
	Res ->
	    Res
    end.


do_intercept(Mod, Op, Args) when is_atom(Mod), Mod =/= undefined ->
    try
	Arity = length(Args),
	case erlang:function_exported(Mod, Op, Arity) of
	    false ->
		ignore;
	    true ->
		apply(Mod, Op, Args)
	end
    catch
	ExType:ExData ->
	    sysInitI:warning_msg(
	      "SFTP callback, interceptor failed, mfa: ~p, ~w, ~p",
	      [{Mod, Op, Args}, ExType, ExData]),
	    ignore
    end;
	
do_intercept(_Mod, _Op, _Args) ->
    ignore.


read_link_ram_dir([Path, State]) ->
    AbsName = absname(Path),
    ParentPath = filename:dirname(AbsName),
    RootDir = sysFi:sftp_root(),
    if 
	ParentPath =:= RootDir ->
	    read_link_ram_dir(Path, State);
	true ->   
	    ignore
    end.				     


%% special case: we might have a symlink from sftp root 
%% pointing into the ramdisk file tree; if that is the 
%% case pretend that the link is just a directory
read_link_ram_dir(Path, State) ->
    case ssh_sftpd_file:read_link(Path, State) of
	{{error, _Reason}, _State} = Res ->
	    %% not a symlink after all
	    Res;
	{{ok, _Target}, NewState} ->
	    %% symlink!
	    {{error, einval}, NewState}
    end.


read_link_info_ram_dir([Path, State]) ->
    case ssh_sftpd_file:read_link_info(Path, State) of
	{{ok, FileInfo}, _NewState} 
	  when FileInfo#file_info.type =:= symlink->
	    %% symlink!
	    ssh_sftpd_file:read_file_info(Path, State);
	    %% {{ok, FileInfo#file_info{type = directory}}, NewState};
	Res ->
	    %% not a symlink after all
	    Res
    end.


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
    RootDir = sysFi:sftp_root(),
    case lists:prefix(RootDir, AbsName) of
	true -> 
	    AbsName;
	_False ->
	    sysFi:sftp_absname(AbsName)
    end.
