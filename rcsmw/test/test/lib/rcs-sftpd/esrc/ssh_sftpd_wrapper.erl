%%% ----------------------------------------------------------
%%% %CCaseFile:	ssh_sftpd_wrapper.erl %
%%% Author:	erarafo
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(ssh_sftpd_wrapper).
-behaviour(ssh_sftpd_file_api).
-id('Updated by CCase').
-vsn('/main/R5A/2').
-date('2016-01-19').
-author('erarafo').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%% R5A/1      2016-01-18 erarafo     First version
%%% R5A/2      2016-01-19 erarafo     Line lengths
%%% ----------------------------------------------------------

-export(
   [close/2,
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
    write_file_info/3]).


open(Path, FlagsRaw, State) ->
    Flags = stripBadFlags(FlagsRaw),
    case ssh_sftpd_file:open(Path, Flags, State) of
	{{ok, IoDevice}=U, State} ->
	    sftpd:info_msg("opened, file: ~s, flags: ~300p", [Path, Flags]),
	    {U, State++[{IoDevice, Path}]};
	R ->
	    sftpd:warning_msg("open, unexpected: ~300p, path: ~s, flags: ~300p", [R, Path, Flags]),
	    R
    end.


close(IoDevice, State) ->
    case ssh_sftpd_file:close(IoDevice, State) of
	{ok, State} ->
	    {[[{_, Path}]], RemainingState} = proplists:split(State, [IoDevice]),
	    sftpd:info_msg("closed, file: ~s", [Path]),
	    {ok, RemainingState};
	R ->
	    sftpd:warning_msg("close, unexpected: ~p", [R]),
	    R
    end.


delete(Path, State) ->
    R = ssh_sftpd_file:delete(Path, State),
    sftpd:info_msg("deleted, file: ~s", [Path]),
    R.



del_dir(A, B) -> ssh_sftpd_file:del_dir(A, B).

get_cwd(A) -> ssh_sftpd_file:get_cwd(A).
is_dir(A, B) -> ssh_sftpd_file:is_dir(A, B).
list_dir(A, B) -> ssh_sftpd_file:list_dir(A, B).
make_dir(A, B) -> ssh_sftpd_file:make_dir(A, B).
make_symlink(A, B, C) -> ssh_sftpd_file:make_symlink(A, B, C).
position(A, B, C) -> ssh_sftpd_file:position(A, B, C).
read(A, B, C) -> ssh_sftpd_file:read(A, B, C).
read_file_info(A, B) -> ssh_sftpd_file:read_file_info(A, B).
read_link(A, B) -> ssh_sftpd_file:read_link(A, B).
read_link_info(A, B) -> ssh_sftpd_file:read_link_info(A, B).
rename(A, B, C) -> ssh_sftpd_file:rename(A, B, C).
write(A, B, C) -> ssh_sftpd_file:write(A, B, C).
write_file_info(A, B, C) -> ssh_sftpd_file:write_file_info(A, B, C).


%%% ----------------------------------------------------------
%%% @doc Remove offending flags that occur in OTP R16B03-1.
%%% @end
%%% ----------------------------------------------------------
stripBadFlags(Flags) ->
    lists:delete(creat,
	lists:delete(trunc, Flags)).
