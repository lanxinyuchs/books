%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	appmDebug.erl %
%%% @author etxarnu
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R11A/1
%%%
%%% @doc ==SYS Erlang trace support==
%%% This module contains helpful functions for starting traces in erlang
-module(appmDebug).
-vsn('/main/R2A/R11A/1').
-date('2017-10-10').
-author('etxarnu').

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
%%% R2A/1      2012-04-19 etxarnu     Created
%%% R11A/1     2017-10-09 etxarnu     Removed export_all
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

-export([help/0,
	 tables/1,
	 tables/0,
	 server/0,
	 libs/0]).
	 

%%% dbg shortcuts

%%% @doc Shows help text
help() ->
    io:format("~n"
	      "=============================================================~n"
	      "=== APPM debug functions                                  ===~n"
	      "=============================================================~n"
	      " help()         This help text~n"
	      "=================== appm shortcuts ==========================~n"
	      " tables()       Show all appm tables~n"
	      " server()       Show appmServer info~n"
	      " libs()         Show symlinks to application shared libraries~n"
	      "=============================================================~n~n"
	     ).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% @doc Print APPM tables in short format
tables() ->
    tables(short).

%%% @doc Print APPM tables in short or long format
tables(Type) ->
    io:format("APPM tables~n"),
    [sysDebug:tab(Type,Tab) || Tab <- appmDataInit:appm_tables()].
  

%%% @doc Print appmServer information
server() ->
    Info = appmServer:get_info(),
    io:format("~p~n", [Info]).

%%% @doc Print symlinks to application shared libraries
libs() ->    
    Res = string:tokens(
	    os:cmd("ls -l " ++
		       filename:join([sysEnv:library_root(),"*"])),"\n"),    
    [pr_link(string:tokens(X,">")) || X<- Res],
    ok.
pr_link([_]) -> ok;
pr_link([_,Str]) ->
    io:format("~p~n", [string:strip(Str)]),
    File = hd(lists:reverse(string:tokens(Str,"/"))),
    case file:read_file_info(
	   filename:join(
	     [sysEnv:dev_patches_dir(),File])) of
	{ok,_ } ->
	    io:format(" --- ~p also exist in dev_patches~n", [File]) ;
	_ ->
	    ok
    end;

pr_link(Other) ->
    io:format("Unknown ~p~n", [Other]).

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------





%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

