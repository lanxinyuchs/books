%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	oi_testapp.erl %
%%% Author:	qthupha
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------

-module(oi_testapp).
-id('Updated by CCase').
-vsn('/main/R2A/3').
-date('2014-03-26').
-author('etxivri').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2014 All rights reserved.
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
%%% R1A/1      2012-08-18 qthupha     Created
%%% R1A/2      2012-12-13 uabesvi     added calls to oi_result_handler
%%% R2A/3      2014-03-26 etxivri     Try to make stop more robust when
%%%                                   process not exist.
%%% ----------------------------------------------------------
%%% 
%%% #---------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([start/1]).
-export([stop/0]).
-export([ccb_object_create_callback_2/5]).
-export([ccb_object_modify_callback_2/4]).
-export([ccb_object_delete_callback/3]).
-export([ccb_completed_callback/2]).
-export([ccb_apply_callback/2]).
-export([ccb_abort_callback/2]).
-export([init/3]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------



-include_lib("safe/include/safe.hrl").
-include_lib("safe/include/safe_imm.hrl").

-define(SIO,   safe_imm_oi).
-define(SAEBH, {error, ?SAFE_AIS_ERR_BAD_HANDLE}).

%% Test Classes
-define(TCS, ["TESTMOMTestRoot",
	      "TESTMOMTestClass1",
	      "TESTMOMTestClass2",
	      "TESTMOMTestClass3",
	      "TESTMOMTestClass4"]).
%% Struct Classes
-define(SCS, ["TESTMOMStruct1",
	      "TESTMOMStruct2",
	      "TESTMOMStruct3"]).


%% Root Mo DN
-define(RMODN, "TESTMOMtestRootId=1").

%% oi_testapp internal process. Handles the results of the OI calls.
-define(OI_PROC, ?MODULE).  
%% Ets table for storing the implementor data.
-define(OI_ETS,  ?MODULE).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%===========================================================================
%%  start(InitData) 
%%
%%       -> ok
%%
%% where
%%
%%   InitData    = {Suite, TestProcPid}
%%   Suite       = atom()
%%   TestProcPid = pid()
%%   
%% @doc
%%   
%%   Start a process which simulates an application to test the oi-interface.
%%   
%%   The process is registered with name ?OI_PROC. When the oi-interface
%%   is invoked the application will call the test suite to get the value
%%   the function should return. It is also checked that the expected
%%   oi-function was called.
%%   
%%   The current test suite is found by means of the InitData parameter;
%%   it contains the suite name and a pid to a test process started in
%%   init_per_suite(). The test process i populated with the return values
%%   in each test case.
%%   
%%   This module creates the implementers in the init function;
%%   these are destroyed when the oi_testapp process is stopped.
%%   
%%   
%% @end
%%===========================================================================
start(InitData) ->
    Ref  = make_ref(),
    Pid  = spawn(?MODULE, init, [self(), Ref, InitData]),
    MRef = erlang:monitor(process, Pid),
    Res  = receive 
	       {Pid, Ref, started} ->
		   {ok, Pid}
	   after 10000 ->
		   {error, timeout}
	   end,
    erlang:demonitor(MRef, [flush]),
    Res.



%%===========================================================================
%%  stop() 
%%
%%       -> ok
%%
%% @doc
%%   
%%   Destroy all implementers and stop the oi_testapp process.
%%   
%% @end
%%===========================================================================
stop() ->
    [send_stop(Fun) || Fun <- [fun oir/0, fun cir/0, fun ic/0, fun final/0]],
    application:stop(safe),
    try 
	ets:delete(?OI_ETS)
    catch error:badarg ->
	    ok
    end,
    ?OI_PROC ! die.


%%===========================================================================
%%  ccb_object_create_callback_2(ImmH, CcbId, ClassName, ParentName, Attr) 
%%
%%       -> Result
%%
%% @doc
%%   
%%   
%%   
%% @end
%%===========================================================================
ccb_object_create_callback_2(ImmH, CcbId, ClassName, ParentName, Attr) ->
    io:format(user,
	      "CCB OBJECT CREATE CALLBACK 2~n"
	      "  ImmH       = ~p ~n"
	      "  CcbId      = ~p ~n"
              "  ClassName  = ~p ~n"
              "  ParentName =~p ~n"
              "  Attr       =~p ~n",
	      [ImmH, CcbId, ClassName, ParentName, Attr]),
    cb_res(ccb_object_create_callback_2).


%%===========================================================================
%%  ccb_object_modify_callback_2(ImmH, CcbId, ObjectName, AttrMods) 
%%
%%       -> Result
%%
%% @doc
%%   
%%   
%%   
%% @end
%%===========================================================================
ccb_object_modify_callback_2(ImmH, CcbId, ObjectName, AttrMods) ->
    io:format(user,
	      "CCB OBJECT MODIFY CALLBACK 2~n"
	      "  ImmH       = ~p ~n"
	      "  CcbId      = ~p ~n"
	      "  ObjectName = ~p ~n"
              "  AttrMods   = ~p ~n",
	      [ImmH, CcbId, ObjectName, AttrMods]),
    cb_res(ccb_object_modify_callback_2).


%%===========================================================================
%%  ccb_object_delete_callback(ImmH, CcbId, ObjectName) 
%%
%%       -> Result
%%
%% @doc
%%   
%%   
%%   
%% @end
%%===========================================================================
ccb_object_delete_callback(ImmH, CcbId, ObjectName) ->
    io:format(user,
	      "CCB OBJECT DELETE CALLBACK 2~n"
	      "  ImmH       = ~p ~n"
	      "  CcbId      = ~p ~n"
	      "  ObjectName = ~p ~n",
	      [ImmH, CcbId, ObjectName]),
    cb_res(ccb_object_delete_callback).    


%%===========================================================================
%%  ccb_completed_callback(ImmH, CcbId) 
%%
%%       -> Result
%%
%% @doc
%%   
%%   
%%   
%% @end
%%===========================================================================
ccb_completed_callback(ImmH, CcbId) ->
    io:format(user,
	      "CCB COMPLETED CALLBACK 2~n"
	      "  ImmH  = ~p, ~n"
	      "  CcbId = ~p ~n",
	      [ImmH, CcbId]),
    cb_res(ccb_completed_callback).


%%===========================================================================
%%  ccb_apply_callback(ImmH, CcbId) 
%%
%%       -> Result
%%
%% @doc
%%   
%%   
%%   
%% @end
%%===========================================================================
ccb_apply_callback(ImmH, CcbId) ->
    io:format(user,
	      "CCB APPLY CALLBACK 2~n"
	      "  ImmH  = ~p, ~n"
	      "  CcbId = ~p ~n",
	      [ImmH, CcbId]),
    cb_res(ccb_apply_callback).    


%%===========================================================================
%%  ccb_abort_callback(ImmH, CcbId) 
%%
%%       -> Result
%%
%% @doc
%%   
%%   
%%   
%% @end
%%===========================================================================
ccb_abort_callback(ImmH, CcbId) ->
    io:format(user,
	      "CCB ABORT CALLBACK~n"
	      "  ImmH  = ~p, ~n"
	      "  CcbId = ~p ~n",
	      [ImmH, CcbId]),
    cb_res(ccb_abort_callback).
    



%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%===========================================================================
%%  init(Creator, Ref, InitData) 
%%
%%       -> ok
%%
%%   
%% @doc
%%   Create the oi-handler and store it in an ets table.
%%   Initialize all implementers and jump to the process loop.
%%
%%   The oi_testapp process is registered as ?OI_PROC.
%%   Only one oi_testapp process is allowed at one time.
%%   
%% @end
%%===========================================================================
init(Creator, Ref, InitData) ->
    init_proc(whereis(?OI_PROC)),
    register(?OI_PROC, self()),

    init_ets(ets:info(?OI_ETS)),
    ets:new(?OI_ETS, [named_table, ordered_set, public]),

    application:set_env(safe, services, [imm]),
    ok = init_safe(application:start(safe), 2),
    {ok, ImmH, _} = ?SIO:initialize_2(?MODULE),
    init_ets_insert(?OI_ETS, {oi_handle, ImmH}),
    ok = is(),
    ok = cis(),
    ok = ois(),
    Creator ! {self(), Ref, started},
    loop(InitData).


init_proc(undefined) ->
    ok;
init_proc(Pid) ->
    unregister(?OI_PROC),
    Pid ! die.

init_ets(undefined) ->
    ok;
init_ets(_) ->
    try 
	ets:delete(?OI_ETS)
    catch error:badarg ->
	    ok
    end,
    ok.


init_ets_insert(T, KV) ->
    ets:insert(T, KV).


init_safe(ok, _) ->
    ok;
init_safe(_, N) when N > 0 ->
    application:stop(safe),
    init_safe(application:start(safe), N - 1);
init_safe(Error, _) ->
    {error, {"starting safe", Error}}.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
    
%%===========================================================================
%%  loop({Suite, TestProcPid}) 
%%
%%       -> ok
%%
%% @doc
%%   
%%   Wait for oi-function calls. Check with the test suite what
%%   the function should return to IMM.
%%   
%%   
%%   
%% @end
%%===========================================================================
loop({Suite, IoResPid} = Data) ->
    receive 
	die ->
	    ok;
	{stopping, Pid, Fun} ->
	    Res = Fun(),
	    Pid ! {?MODULE, stopping_rc, Res},
	    loop(Data);
	{get_result, Pid} ->
	    Res = case oi_result_handler:get_results(Suite, IoResPid, next) of
		      {get_results_rc, ok,   NextData} -> NextData;
		      {get_results_rc, Error, _}       -> Error
		  end,
	    Pid ! {?MODULE, get_result_rc, Res},
	    loop(Data)
    after 5000 ->
	    loop(Data)
    end.
    


%% send_stop(Fun) ->
%%     ?OI_PROC ! {stopping, self(), Fun},
%%     receive 
%% 	{?MODULE, stopping_rc, Res} ->
%% 	    Res
%%     after 1000 ->
%% 	    {error, timeout}
%%     end.

send_stop(Fun) ->
    send_stop(Fun, whereis(?OI_PROC)).

send_stop(_, undefined) ->
    ok;  %% Process already stopped.

send_stop(Fun, _) ->
    ?OI_PROC ! {stopping, self(), Fun},
    receive 
	{?MODULE, stopping_rc, Res} ->
	    Res
    after 1000 ->
	    {error, timeout}
    end.

%%===========================================================================
%% get the result from the test case
%%===========================================================================
cb_res(Fnc) ->
    ?OI_PROC ! {get_result, self()},
    receive
	{?MODULE, get_result_rc, {{finalize, Res}, Fnc}} ->
	    send_stop(fun final/0),
	    timer:sleep(1000),
	    try 
		ets:delete(?OI_ETS)
	    catch error:badarg ->
		    ok
	    end,
	    ?OI_PROC ! die,
	    Res;
	{?MODULE, get_result_rc, {Res, Fnc}} ->
	    Res;
	{?MODULE, get_result_rc, {Res, WrongFnc}} ->
	    {error, 
	     {?MODULE, "wrong function received ", {WrongFnc, Fnc}, Res}}	
    after 5000 ->
	    {error, {?MODULE, Fnc, timeout}}
    end.


%%===========================================================================
%% oi_testapp ets table handling
%%===========================================================================

ets_lookup(T, K) ->
    ets_lkp(ets:lookup(T, K)).

ets_lkp([{_, V}]) -> 
    V;
ets_lkp([]) ->
    no_value.
    



%%===========================================================================
%%  calls to safe_imm_oi 
%%===========================================================================
final() ->
    fin(ets_lookup(?OI_ETS, oi_handle)),
    ets:delete(?OI_ETS).
fin(no_value) ->
    ?SAEBH;
fin(ImmH) ->
    ?SIO:finalize(ImmH).

is() ->
    is(ets_lookup(?OI_ETS, oi_handle)).
is(no_value) ->
    ?SAEBH;
is(ImmH) ->
    ?SIO:implementer_set(ImmH, ?MODULE_STRING).


ic() ->
    ic(ets_lookup(?OI_ETS, oi_handle)).
ic(no_value) ->
    ?SAEBH;
ic(ImmH) ->
    ?SIO:implementer_clear(ImmH).

cis() ->
    cis(ets_lookup(?OI_ETS, oi_handle)).
cis(no_value) ->
    ?SAEBH;
cis(ImmH) ->
    lists:foreach(fun(C) -> ?SIO:class_implementer_set(ImmH, C) end, 
		  ?TCS ++ ?SCS).

cir() ->
    cir(ets_lookup(?OI_ETS, oi_handle)).
cir(no_value) ->
    ?SAEBH;
cir(ImmH) ->
    lists:foreach(fun(C) -> ?SIO:class_implementer_release(ImmH, C) end, 
		  ?TCS ++ ?SCS).


ois() ->
    ois(ets_lookup(?OI_ETS, oi_handle)).
ois(no_value) ->
    ?SAEBH;
ois(ImmH) ->
    ?SIO:object_implementer_set(ImmH, ?RMODN, ?SAFE_IMM_SUBTREE).


oir() ->
    oir(ets_lookup(?OI_ETS, oi_handle)).
oir(no_value) ->
    ?SAEBH;
oir(ImmH) ->
    ?SIO:object_implementer_release(ImmH, ?RMODN, ?SAFE_IMM_SUBTREE).



