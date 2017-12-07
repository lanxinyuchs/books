%% -*- erlang -*-
%% #0.    BASIC INFORMATION
%% ----------------------------------------------------------
%% %CCaseFile:	ecoli_api.erl %
%% %CCaseRev:	/main/R11A/1 %
%% %CCaseDate:	2017-10-05 %
%% %CCaseDocNo:	 %
%%
%% Author:	etxlg
%% Description: 
%% API module.
%% This module holds functions that may be called from places outside of ECOLI.
%% The "normal" ECOLI API (towards SSH/TLS) consists of callback modules
%% (ecoli_ssh_channel, ecoli_transport_adaptor).
%%
%% NOTE! 
%% There may be other ECOLI functions, in addition to those listed here, which
%% are also used externally. (this module was added long after ECOLI was
%% already in use).
%%
%% ----------------------------------------------------------
%% %CCaseTemplateFile:	APP.appSrc %
%% %CCaseTemplateId: CCver: /main/3 %
%%
%%
%% %CCaseCopyrightBegin%
%% Copyright (c) Ericsson AB 2017 All rights reserved.
%% 
%% The information in this document is the property of Ericsson.
%% 
%% Except as specifically authorized in writing by Ericsson, the 
%% receiver of this document shall keep the information contained 
%% herein confidential and shall protect the same in whole or in 
%% part from disclosure and dissemination to third parties.
%% 
%% Disclosure and disseminations to the receivers employees shall 
%% only be made on a strict need to know basis.
%% %CCaseCopyrightEnd%
%% 
%% ----------------------------------------------------------
%% #1.    REVISION LOG
%% ----------------------------------------------------------
%% Rev        Date         Name        What
%% --------   --------     --------    ------------------------
%% Rx         2017-10-05   etxlg       Created
%% ----------------------------------------------------------
%% 
%% 
-module(ecoli_api).
-export([external_cmd_paths/1, external_cmd_paths/2]).

-include("ecoli.hrl"). %get the coli_cmd -record

% Lookup an external/rootfs command based on its coli-cmd-path and coli-cmd-name
% as seen in the coli-shell. E.g. ("/os", "ps") -> ["/bin/ps"]
% returns either a single item list or the empty list
-spec external_cmd_paths(list(), list()) -> [list()] | [].
external_cmd_paths(Path, Name) ->
    case ecoli_datainit:lookup({Path, Name}) of
	[] -> [];
        Cmd_rec -> extract_paths([Cmd_rec])
    end.

% Lookup an external/rootfs command based on only its coli-cmd-name
% as seen in the coli-shell. E.g. ("te") -> ["/software/.../bin/tex", "/bin/te"]
% returns either a list (there may be multiple commands with the same name as
% seen in the above example) or the empty list
-spec external_cmd_paths(list()) -> [list()] | [].
external_cmd_paths(Name) ->
    MatchAll = mnesia:table_info(coli_cmd, wild_pattern),
    Match = MatchAll#coli_cmd{cli_pname     = {'_', Name},
                              type          = '$1'},
    Cond  = [{'orelse', {'==', external, '$1'}, {'==', rootfs, '$1'}}],
    Cmd_recs = mnesia:dirty_select(coli_cmd, [{Match, Cond, ['$_']}]),
    extract_paths(Cmd_recs).


%%% Internal functions %%%

extract_paths([]) -> [];
extract_paths([Cmd | Rest]) ->
    case Cmd of
	#coli_cmd{type = rootfs, filepath = Filepath} ->
	    [Filepath | extract_paths(Rest)];
	#coli_cmd{type = external, cxp_path = Cp, relpath = Rp} ->
	    case filelib:wildcard(filename:join([Cp, Rp])) of
		[One] -> [One | extract_paths(Rest)];
		_     -> extract_paths(Rest)
	    end;
	_ -> extract_paths(Rest)
    end.
