%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysInitEts.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2015
%%% @version /main/R4A/1

%%% @doc ==SysInitEts==
%%% This module implements the sysInitEts service, which allows applications
%%% to register an ets table that will survive a local process restart.

-module(sysInitEts).
-behaviour(gen_server).
-vsn('/main/R4A/1').
-date('2015-09-18').
-author('etxjotj').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R4A/1      2016-09-18 etxjotj     Created
%%% ----------------------------------------------------------


%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0]).

-export([ets_new/2, ets_delete/1]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).
-compile(nowarn_unused_vars).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

start() ->
    gen_server:start_link({local, sysInitEts}, ?MODULE, [], []).


ets_new(Tab, Options) ->
    gen_server:call(?MODULE, {ets_new, Tab, Options}).

ets_delete(Tab) ->
    gen_server:call(?MODULE, {ets_delete, Tab}).



%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

init(Args) ->
    {ok, state}.

handle_call({ets_new, Tab, Opts}, Reply, State) ->
    case lists:member(public, Opts) of
	true ->
	    {reply, try ets:new(Tab, Opts) of
			TabId ->
			    {ok, TabId}
		    catch T:F ->
			    {T,F}
		    end, State};
	false ->
	    {reply, try ets:new(Tab, [public|Opts]) of
			TabId ->
			    {ok, TabId}
		    catch {T,F} ->
			    {T,F}
		    end, State}
    end;
handle_call({ets_delete, Tab}, Reply, State) ->
    {reply, try ets:delete(Tab) of
		Result ->
		    Result
	    catch T:F ->
		    {T,F}
	    end, State};

handle_call(Request, Reply, State) ->
    {reply, ok, State}.

handle_cast(Request, State) ->
    {noreply, State}.

handle_info(Request, State) ->
    {noreply, State}.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    ok.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

