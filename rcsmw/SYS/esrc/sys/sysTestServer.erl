%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	sysTestServer.erl %
%%% @author erarafo
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R3A/R4A/R7A/1

%%% @doc == Test Server Functions ==
%%% This module contains functions for execution of test cases. Typically used
%%% for testing of external Erlang Interfaces that are available on the RCS
%%% node.
%%%
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-module(sysTestServer).
-vsn('/main/R3A/R4A/R7A/1').
-date('2016-09-21').
-author(etxberb).

%%% ###=====================================================================###
%%% # 0.3   LEGAL RIGHTS
%%% ###---------------------------------------------------------------------###
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 1.    REVISION LOG
%%% ###---------------------------------------------------------------------###
%% Rev     Date       Name     What
%% ------  ---------- -------  ------------------------------------------------
%% R3A/1   2015-01-20 etxberb  Created.
%% R3A/2   2015-02-02 etxberb  Added callback functionality.
%% R7A/1   2016-09-21 erarafo  Added an interface to sysEnv:load_average/2.
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 2.    MODULE BORDER LINE AND INTERNAL DEFINITIONS
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 2.1.1 Interface functions
%%% ###---------------------------------------------------------------------###
-export([erase_callback_results/0,
	 get_callback_results/0,
	 put_callback_result/1,
	 subscr_callback_result/1,
	 run/2,
	 run/3,
	 load_average_init/2,
	 load_average/2,
	 start/0,
	 stop/0]).

%%% ###---------------------------------------------------------------------###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
-export([init/1]).

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.4   LOCAL DEFINITION OF MACROS
%%% ###---------------------------------------------------------------------###
%% General
-define(FUNCTION, 
	element(2, element(2, process_info(self(), current_function)))).

%%
-define(CallbackNotif_Tag, sysTestServer_cb).

%% Internal:
-define(MODULE_CALL, sysTestServer_call).
-define(MODULE_CAST, sysTestServer_cast).

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###
-record(state, {cb_res = [],
		subscr = [],
		load_average = orddict:new()}).

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% @doc Erase all callback function call MFA:s from the sysTestServer process.
%%%
%%% @end
%%% ###=====================================================================###
erase_callback_results() ->
    call(erase_callback_results).

%%% ###########################################################################
%%% @doc Get callback function call MFA:s from the sysTestServer process.
%%%
%%% @end
%%% ###=====================================================================###
get_callback_results() ->
    call(get_callback_results).

%%% ###########################################################################
%%% @doc Report a callback function call MFA to the sysTestServer process.
%%%
%%% @end
%%% ###=====================================================================###
put_callback_result(Cb_result) ->
    call({put_callback_result, Cb_result}).

%%% ###########################################################################
%%% @doc Subscribe for callback MFA report to the sysTestServer process.
%%%
%%% @end
%%% ###=====================================================================###
subscr_callback_result(Subscriber)
  when is_pid(Subscriber) orelse
       is_atom(Subscriber) ->
    call({subscr_callback_result, Subscriber}).

%%% ###########################################################################
%%% @doc Run a function in the sysTestServer process.
%%%
%%% @end
%%% ###=====================================================================###
run(MFA, ServerPid) ->
    run(MFA, ServerPid, 10000).

%%% ###########################################################################
%%% @doc Initializes for use of load_average/2 with the given tag.
%%% @end
%%% ###=====================================================================###
load_average_init(Tag, ServerPid) ->
    ServerPid ! {?MODULE_CAST, {load_average_init, Tag}}.

%%% ###########################################################################
%%% @doc Calls sysEnv:load_average/2 with the given tag.
%%% @end
%%% ###=====================================================================###
load_average(Tag, ServerPid) ->
    ServerPid ! {?MODULE_CALL, {load_average, Tag}, self()},
    receive
	{?MODULE_CALL, reply, Result} ->
	    {ok, Result}
    after 5000 ->
	{error, {?MODULE_CALL, {load_average, timeout}}}
    end.

%%% ###=====================================================================###
run({M, F, A} = MFA, ServerPid, Timeout)
  when is_atom(M) andalso
       is_atom(F) andalso
       is_list(A) ->
    try
	ServerPid ! {?MODULE_CALL, run, MFA, self()},
	receive
	    {?MODULE_CALL, reply, Result} ->
		{ok, Result}
	after
	    Timeout ->
		{error, {?MODULE_CALL, timeout}}
	end
    catch
	_ : _ ->
	    {error, invalid_pid}
    end;
run(MFA, _, _) ->
    sysInitI:error_report([{?MODULE, ?FUNCTION},
			       {not_an_mfa, MFA}]),
    {error, not_an_mfa}.

%%% ###########################################################################
%%% @doc Start the sysTestServer process.
%%%
%%% @end
%%% ###=====================================================================###
start() ->
    Pid = spawn(?MODULE, init, [self()]),
    receive
	{?MODULE, ok} ->
	    {ok, Pid};
	Error ->
	    Error
    after
	10000 ->
	    {error, {?MODULE, timeout}}
    end.

%%% ###########################################################################
%%% @doc Stop the sysTestServer process.
%%%
%%% @end
%%% ###=====================================================================###
stop() ->
    call(stop).

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
init(Pid) ->
    try
	register(?MODULE, self()),
	Pid ! {?MODULE, ok},
	sysInitI:info_report([{?MODULE, ?FUNCTION}, started]),
	init_go()
    catch
	_ : _ ->
	    Reason = {?MODULE, already_started},
	    sysInitI:warning_report([{?MODULE, ?FUNCTION},
					 Reason]),
	    stop(),
	    init(Pid)
    end.

init_go() ->
    try
	loop(#state{})
    catch
	ErrClass : ErrReason ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				       {ErrClass, ErrReason},
				       {stacktrace, erlang:get_stacktrace()}]),
	    exit(ErrReason)
    end.

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% call
%%%
%%% ###=====================================================================###
call(Msg) ->
    call(Msg, 10000).

call(Msg, Timeout) ->
    try
	whereis(?MODULE) ! {?MODULE_CALL, Msg, self()},
	receive
	    {?MODULE_CALL, reply, Result} ->
		{ok, Result}
	after
	    Timeout ->
		{error, {?MODULE_CALL, timeout}}
	end
    catch
	_ : _ ->
	    {error, {?MODULE, undefined}}
    end.

%%% ###########################################################################
%%% loop
%%%
%%% ###=====================================================================###
loop(State) ->
    receive
	{?MODULE_CALL, run, MFA, Pid} ->
	    Result = run_safely(MFA),
	    Pid ! {?MODULE_CALL, reply, Result},
	    loop(State);
	{?MODULE_CALL, erase_callback_results, Pid} ->
	    Pid ! {?MODULE_CALL, reply, ok},
	    loop(State#state{cb_res = []});
	{?MODULE_CALL, get_callback_results, Pid} ->
	    Pid ! {?MODULE_CALL, reply, State#state.cb_res},
	    loop(State);
	{?MODULE_CALL, {put_callback_result, Cb_result}, Pid} ->
	    Pid ! {?MODULE_CALL, reply, ok},
	    notify_subscribers(State#state.subscr, Cb_result),
	    loop(State#state{cb_res = [Cb_result | State#state.cb_res]});
	{?MODULE_CALL, {subscr_callback_result, Subscriber}, Pid} ->
	    Pid ! {?MODULE_CALL, reply, ?CallbackNotif_Tag},
	    loop(State#state{subscr = [Subscriber | State#state.subscr]});
	{?MODULE_CAST, {load_average_init, Tag}} ->
	    Dict = sysEnv:load_average_init(Tag, State#state.load_average),
	    loop(State#state{load_average = Dict});
	{?MODULE_CALL, {load_average, Tag}, Pid} ->
	    {Result, Dict} = sysEnv:load_average(Tag, State#state.load_average),
	    Pid ! {?MODULE_CALL, reply, Result},
	    loop(State#state{load_average = Dict});
	{?MODULE_CALL, stop, Pid} ->
	    unregister(?MODULE),
	    sysInitI:info_report([{?MODULE_CALL, ?FUNCTION}, stopped]),
	    Pid ! {?MODULE_CALL, reply, ok},
	    stop
    end.

%%% ###########################################################################
%%% notify_subscribers
%%%
%%% ###=====================================================================###
notify_subscribers([Subscr | Tail], Msg) when is_pid(Subscr) ->
    Subscr ! {?CallbackNotif_Tag, Msg},
    notify_subscribers(Tail, Msg);
notify_subscribers([Subscr | Tail], Msg) when is_atom(Subscr) ->
    Subscr:?CallbackNotif_Tag(Msg),
    notify_subscribers(Tail, Msg);
notify_subscribers([], _) ->
    ok.

%%% ###########################################################################
%%% run_safely
%%%
%%% ###=====================================================================###
run_safely({M, F, A}) ->
    try
	apply(M, F, A)
    catch
	ErrClass : ErrReason ->
	    Error = {ErrClass, ErrReason},
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				       Error,
				       {stacktrace, erlang:get_stacktrace()}]),
	    Error
    end.

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 4     CODE FOR TEMPORARY CORRECTIONS
%%% ###---------------------------------------------------------------------###
