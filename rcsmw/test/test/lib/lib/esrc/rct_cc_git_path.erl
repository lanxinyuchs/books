%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_cc_git_path.erl %
%%% @author etxkols
%%% @copyright Ericsson AB 2016
%%% @version /main/R6A/1
%%% @doc
%%%
%%% Temporary lib to handle paths to both CC and git
%%%
%%% @end
-module(rct_cc_git_path).
-id('Updated by CCase').
-vsn('/main/R6A/1').
-date('2016-08-17').
-author('etxkols').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%% 
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% R6A/1      2016-08-17 etxkols     Created
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([find/1, find/2]).

%%===========================================================================
%% @spec find(Pathlist) ->
%%    string() | {error, Reason}
%% Pathlist = list()
%% @doc Find file, the first path pointing to an existing file from the list is returned.
%%
%% Error is rised when none of the paths exist.
find(Pathlist) ->
   lists:nth(1,  lists:filter(fun filelib:is_file/1, Pathlist)).

%%===========================================================================
%% @spec find(Env, Pathlist) ->
%%    string() | {error, Reason}
%% Env = string()
%% Pathlist = list()
%% @doc Find file, first path, prefixed with the value of environent variable Env,
%% pointing to an existing file  from the list is returned.
%%
%% Error is rised when none of the paths exist.
find(Env, Pathlist) ->
   find(lists:map(fun(P) -> os:getenv(Env)++"/"++P end, Pathlist)).
