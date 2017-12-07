%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	oi_result_handler.erl %
%%% Author:	
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------

-module(oi_result_handler).

-id('Updated by CCase').
-vsn('/main/R2A/3').
-date('2012-12-21').
-author('uabesvi').

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
%%% R2A/2      2012-12-13 uabesvi     created
%%% ----------------------------------------------------------
%%% 
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/1]).
-export([stop/1]).
-export([init/3]).

-export([set_results/2]).
-export([get_results/3]).
-export([read_results/1]).


%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%===========================================================================
%%  start(Suite) 
%%
%%       -> ok
%%
%% where
%%
%%   Suite       = atom()
%%   
%% @doc
%%   
%%   Start a process which holds a list of expected imm oi function calls
%%   and what the function calls should return.
%%   
%%   The process is registerd with name Suite.
%%   
%%   When the test application (testapp) gets an imm oi interface call
%%   it calls the this process to get the value it should reply the
%%   oi call with. There is also a check done that the expected oi
%%   call was received.
%%   
%% @end
%%===========================================================================
start(Suite) ->
    start(whereis(Suite), Suite, 2).

start(undefined, _Suite, N) when N < 1 ->
    {error, could_not_start_process};
start(undefined, Suite, _) ->
    Ref  = make_ref(),
    Pid  = spawn(?MODULE, init, [self(), Ref, Suite]),
    MRef = erlang:monitor(process, Pid),
    Res  = receive 
	       {Pid, Ref, started} ->
		   {ok, Pid}
	   after 10000 ->
		   {error, timeout}
	   end,
    erlang:demonitor(MRef, [flush]),
    Res;
start(Pid, Suite, N) ->
    Pid ! die,
    start(whereis(Suite), Suite, N - 1).






%%===========================================================================
%%  stop(Suite) 
%%
%%       -> ok
%%
%% where
%%
%%   Suite       = atom()
%%   
%% @doc
%%   
%%   Stop the process created at start/1
%%   
%% @end
%%===========================================================================
stop(Suite) ->
    Suite ! die,
    ok.




%%===========================================================================
%%  set_results(Suite, Results)
%%
%%       -> ok
%%
%% where
%%
%%   Suite   = atom()
%%   Results = [{Result, OiFnc}] 
%%   Result  = atom()
%%   OiFnc   = atom()
%%   
%% @doc
%%   
%%   Populate the process with the Results
%%   
%% @end
%%===========================================================================
set_results(Suite, Results) ->
    send_internal(Suite, {set_results, Suite, Results}).

get_results(Suite, TestProcPid, What) ->
    send_internal(TestProcPid, {get_results, Suite, What}).

read_results(Suite) ->
    send_internal(Suite, {read_results, Suite}).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%===========================================================================
%%  init(Suite, Creator) 
%%
%%       -> ok
%%
%% where
%%
%%   Suite    = atom()
%%   Creator  = pid()
%%   
%% @doc
%%   
%%   Initialize the process holding the results
%%   
%% @end
%%===========================================================================
init(Creator, Ref, Suite) ->
    case whereis(Suite) of
	undefined ->
	    register(Suite, self());
	KillPid ->
	    KillPid ! die,
	    register(Suite, self())
    end,

    Creator ! {self(), Ref, started},
    loop([]).



%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
    
%%===========================================================================
%%  loop(Results) 
%%
%%       -> ok
%%
%% @doc
%%   
%%   Wait for oi-function calls. Check with the test suite what
%%   the function should return to IMM.
%%   
%% @end
%%===========================================================================
loop(Results) ->
    receive 
	die ->
	    ok;
	{Pid, {set_results, Suite, SetResults}} ->
	    {Res, NewResults} = update_data(Results, Suite, SetResults),
	    Pid ! {?MODULE, Res},
	    loop(NewResults);
	{Pid, {get_results, Suite, What}} ->
	    {Res, NextData, RemResults} = read_data(Results, Suite, What),
	    Pid ! {?MODULE, {get_results_rc, Res, NextData}},
	    loop(RemResults);
	{Pid, {read_results, _Suite}} ->
	    Pid ! {?MODULE, Results},
	    loop(Results)
    after 5000 ->
	    loop(Results)
    end.
  

  
send_internal(Suite, Msg) ->
    Suite ! {self(), Msg},
    receive 
	{?MODULE, Res} -> Res
    after 4000 -> {error, {?MODULE, timeout}}
    end.
    

update_data(Data, Suite, Res) ->
    {ok, [{Suite, Res} | proplists:delete(Suite, Data)]}.


read_data(Data, Suite, What) ->
    case proplists:get_value(Suite, Data) of
	Entry when Entry == undefined;
		   Entry == [] ->
	    {{error, {Suite, no_results}}, [], []}; 
	Results ->
	    rd(Results, Suite, proplists:delete(Suite, Data), What)
    end.

rd([], Suite, _, _) ->
    {{error, {Suite, no_results}}, [], []}; 
rd([H | T], Suite, Data, next) ->
    {ok, H, [{Suite, T} | Data]}.

