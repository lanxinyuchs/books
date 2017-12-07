%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfLog.erl %
%%% Author:	
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(gmfLog).
-id('').
-vsn('/main/R2A/2').
-date('2012-11-22').
-author('qthupha').
-shaid('5ed0bd10c973dcad5dfdc5c85e3b245cd83f9839').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% ----------------------------------------------------------
%%% 
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([log_start/1]).
-export([log_stop/1]).
-export([comte/1]).
-export([imm/1]).
-export([dnti/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%===========================================================================
%% @spec log_start(Type) 
%%
%%       -> ok
%%   
%%   
%% @doc
%%   Starts logging in GMF
%% @end
%%===========================================================================
log_start(Type) ->
    logs(dbg:get_tracer(), Type).
logs({error, {no_tracer_on_node, _}}, Type) ->
    io:format("~n==============~nStarting a new tracer~n==============~n"),
    dbg:tracer(),
    dbg:p(all,c),
    logs(Type);
logs(_, Type) ->
    io:format("~n==============~nRe-using a tracer~n==============~n"),
    logs(Type).

logs(all) ->
    logs(comte),
    logs(imm);
logs(comte) ->
    dbg:tp(?MODULE, comte, []);
logs(imm) ->
    dbg:tp(?MODULE, imm, []).



%%===========================================================================
%% @spec log_stop(Action) 
%%
%%       -> ok
%%   
%%   
%% @doc
%%   Stops logging in GMF
%% @end
%%===========================================================================
log_stop(stop_tracer) ->
    dbg:stop_clear();
log_stop(stop) ->
    dbg:ctp(?MODULE).

%%===========================================================================
%% @spec comte(Log) 
%%
%%       -> ok
%%
%% where
%%
%% @doc
%%   Log a message related to comte interface
%% @end
%%===========================================================================
%% In future we can add more functionality on a need basis
comte(_Log) -> ok.
    
%%===========================================================================
%% @spec imm(Log) 
%%
%%       -> ok
%%
%% where
%%
%% @doc
%%   Log a message related to imm interface
%% @end
%%===========================================================================
%% In future we can add more functionality on a need basis
imm(_Log) -> ok.
 
%%===========================================================================
%% @spec dnti(Log) 
%%
%%       -> ok
%%
%% where
%%
%% @doc
%%   Log a message related to dnti interface
%% @end
%%===========================================================================
%% In future we can add more functionality on a need basis
dnti(_Log) -> ok.
 

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

    
%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

