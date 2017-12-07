%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysApp.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R9A/1
%%%
%%% @doc ==Basic application handling==
%%% This module provides callback functions for OTP's application handling
%%% model. 

-module(sysApp).
-behaviour(application).
-vsn('/main/R1A/R2A/R3A/R4A/R9A/1').
-date('2017-04-11').
-author('etxberb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R1A/1      2012-01-25 etxjotj     Created
%%% R2A/2      2013-05-28 etxarnu     Added calls to stop sub applications
%%% R2A/3      2013-05-31 etxarnu     Turned off DBG macro
%%% R2A/7      2014-03-19 erarafo     Corrected argument to 
%%%                                   error_logger:error_report/1
%%% R3A/1      2015-04-24 etxjotj     Added printout for hanging startup
%%% R9A/1      2017-04-11 etxberb     Added logging of run_func results
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%% Application interface
-export([start/2, start_phase/3, prep_stop/1, stop/1, config_change/3]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
-define(MonoTime, erlang:monotonic_time()).

%% Usage of error_logger:XXX_report
-define(LOG_ERR(__ReportInfo),
	try
	    sysInitI:error_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:error_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_INFO(__ReportInfo),
	try
	    sysInitI:info_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:info_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_WARN(__ReportInfo),
	try
	    sysInitI:warning_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:warning_report(?RepInfo(__ReportInfo))
	end).
-define(RepInfo(__RepInfo),
	[{?MODULE, ?FUNCTION} | __RepInfo]).
%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%-define(DBG(Args), apply(sysInitI, info_msg, Args)).
-define(DBG(_Args), ok).


%%% @doc Called when starting an application
-type start_type() :: nomal | {takeover | node()} | {failover, node()}.
-spec start(StartType::start_type(), StartArgs::any()) -> 
		   {ok, pid()} | {ok, pid(), any()} |  {error, any()}.
								 
start(StartType, StartArgs) ->
    io:format("~w: sysApp:start(~p, ~p)~n",[self(), StartType, StartArgs]),
    AppName = get_app_name(StartArgs),
    
    ExitPid = 
	case AppName of
	    sys -> 
		spawn(
		  fun() -> 
			  receive
			      ok -> ok
			  after 300000 ->
				  erlang:display({sysApp, timeout, AppName}),
				  io:format("***ERROR***~n"
					    "Timeout for startup of sys~n"
					     "Erlang node rebooting...~n",
					    []),
				  init:reboot() 
			  end
		  end);
	    _ ->
		spawn(
		  fun() -> receive
			       ok -> ok
			   after 30000 ->
				   erlang:display({sysApp, timeout, AppName}),
				   io:format("***ERROR***~n"
					     "Timeout for startup of ~w~n"
					     "Erlang node rebooting...~n",
					     [AppName]),
				   init:reboot() 
			   end
		  end)
	end,

    Module = proplists:get_value(initmodule, StartArgs),
    SupName = list_to_atom(atom_to_list(AppName)++"Super"),
    Included = 
	case application:get_env(AppName, included_applications) of
	    {ok, Inc} ->
		%% [ok = application:start(App)||App<-Inc],
		Inc;
	    undefined ->
		[]
	end,
    sysInitI:info_msg("~w: Starting ~p~n",[self(),SupName]),
    %% {ok, Pid} = case 
    %% 	supervisor:start_link({local, SupName}, sysSupervisor,
    %% 			      [Module, Included, StartType]) of
    %% 		    {ok, P} ->{ok, P};
    %% 		    Any -> 
    %% 			erlang:error(Any, [StartType, StartArgs])
    %% 		end,
    {TTime,TRes} = timer:tc(supervisor, start_link, 
			    [{local, SupName}, sysSupervisor,
			     [Module, Included, StartType]]),
    io:format("++++ Start of ~-15w: ~10w~n",[SupName, TTime]),
    {ok, Pid} = case TRes of
		    {ok, P} ->{ok, P};
		    Any -> 
			erlang:error(Any, [StartType, StartArgs])
		end,

    case AppName of
	sys -> 
	    %% FIXME! This construct allows for a local sys activate only.
	    %% Is there a need for a general activate function, or a global?
	    run_func([AppName|Included], activate),
	    %% This means sysInitServer changes the restart type
	    sysInitServer:restart_complete();
	_ ->
	    ok
    end,
    ExitPid!ok,
    {ok, Pid, {AppName,Included}}.


%%% @doc Start phases will not happen since we're not using the 
%%% application_start module

-spec start_phase(atom(), start_type(), any()) -> ok | {error, any()}.

start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.


%%% @doc Called when the application is about to be stopped
-spec prep_stop(State::any()) -> any().

prep_stop(State) ->
    do_func(State, prep_stop),
    State.

-spec stop(State::any()) -> ok.

%%% @doc Called whenever an application has stopped
stop(State = {AppName,_Included}) ->
    do_func(State, stop),
    sysInitI:info_report(progress, [{application, AppName},
					{stopped_at, node()}]).



%%% ----------------------------------------------------------
%%% @doc Call the specified 0-ary function on the given
%%% applications, in reversed order.
%%%
%%% FIXME! This construct allows for a local sys activate only.
%%% Is there a need for a general activate function, or a global?
%%% @end
%%% ----------------------------------------------------------

-spec do_func({atom(), [atom()]}, prep_stop|stop) -> ok.

do_func({AppName, Included}, Func) ->
    case AppName of
	sys -> 
	    ReversedApps = lists:reverse([AppName|Included]),
	    run_func(ReversedApps, Func);
	_ ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc Call the specified 0-ary function on each of the
%%% listed applications.
%%%
%%% TODO: If the sleep(200) is not needed, remove the
%%% commented code.
%%% @end
%%% ----------------------------------------------------------

-spec run_func([atom()], activate|prep_stop|stop) -> ok.

run_func(Apps, Func) ->
    T0 = ?MonoTime,
    {ok, Results} = run_func(Apps, Func, []),
    T1 = ?MonoTime,
    error_logger:info_report([{{?MODULE, stPh}, Func} | Results] ++
			     [{'TOTAL', sysUtil:time_to_string(T1 - T0)}]),
    ok.

run_func([App | Apps], Func, AppResults) ->
    AppResult =
	case application:get_key(App, mod) of
	    {ok, {sysApp, Data}} ->
		Module = proplists:get_value(initmodule, Data),
		case erlang:function_exported(Module, Func, 0) of
		    true ->
			?DBG(["~p: Trying to call ~p:~p([]) ~n",
			      [?MODULE, Module, Func]]),
			try
			    begin
				T0 = ?MonoTime,
				apply(Module, Func, []),
				T1 = ?MonoTime,
				%% timer:sleep(200)   % needed to take down COM
				{{App, Module}, sysUtil:time_to_string(T1 - T0)}
			    end
			catch
			    Type:Reason ->
				Stack = erlang:get_stacktrace(),
				sysInitI:error_report(
				  [{mfa, {Module, Func, []}},
				   {Type, Reason},
				   {stack, Stack},
				   {appResults, AppResults}]),
				erlang:Type(Reason, [App|Apps])
			end;
		    false ->
			?DBG(["~p:run_func  initmodule=~p  has no func= ~p ~n",
			      [?MODULE, Module, Func]]),
			{{App, Module}, undefined}
		end;
	    _ ->
		?DBG([
		      "~p:run_func  App=~p  has no mod env variable ~n",
		      [?MODULE, App]]),
		{App, undefined_mod_env}
	end,
    run_func(Apps, Func, AppResults ++ [AppResult]);
run_func([], _, AppResults) ->
    {ok, AppResults}.


-type par_val() :: {atom(), any()}.
-spec config_change([par_val()], [par_val()], [atom()]) -> ok.

%%% @doc Called by an application after code replacement

config_change(Changed, New, Removed) ->
    Module = get(module),
    catch ok = apply(Module, config_change, [Changed, New, Removed]).
	    
    


%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% some_method(Parameter)->
%%    nn.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% internal_function1(One, Two)->
%%    nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


get_app_name(StartArgs) ->
    case proplists:get_value(appname, StartArgs) of
	undefined ->
	    case application:get_application() of
		{ok, AN} ->
		    AN;
		undefined ->
		    erlang:error({no_app_name, self()}, [])
	    end;
	AN ->
	    AN
    end.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

