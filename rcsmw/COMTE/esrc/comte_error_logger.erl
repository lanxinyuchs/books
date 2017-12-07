%%%-------------------------------------------------------------------
%%% @author Chengkai YAN <ecaiyan@esekilxv7550.seki.rnd.ericsson.se>
%%% @copyright (C) 2016, Chengkai YAN
%%% @doc
%%%
%%% @end
%%% Created :  3 Mar 2016 by Chengkai YAN <ecaiyan@esekilxv7550.seki.rnd.ericsson.se>
%%%-------------------------------------------------------------------
-module(comte_error_logger).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3, 
	 warning_report/1, error_report/1, info_report/1]).

-define(SERVER, ?MODULE).
-record(?MODULE, {reportIntervalTime, reportIntervalMaxMsgs, reportExceptionMaxTermDepth, cnt }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, ReportIntervalTime} = application:get_env(comte, report_interval_time),
    {ok, ReportIntervalMaxMsgs} = application:get_env(comte, report_interval_max_msgs),
    {ok, ReportExceptionMaxTermDepth} = application:get_env(comte, report_exception_max_term_depth),
    {ok, #?MODULE{reportIntervalTime = ReportIntervalTime, 
		  reportIntervalMaxMsgs = ReportIntervalMaxMsgs, 
		  reportExceptionMaxTermDepth = ReportExceptionMaxTermDepth, 
		  cnt = 0}}.

handle_event({warning_report,[{exception,E,R}|T]}, State) ->
    case State#?MODULE.cnt of
	0 -> erlang:start_timer(State#?MODULE.reportIntervalTime, self(), time_interval);
	_ -> ok
    end,

    case State#?MODULE.cnt + 1 =< State#?MODULE.reportIntervalMaxMsgs of 
	true->
	    error_logger:info_msg("Only report first ~p invalid values",[State#?MODULE.reportExceptionMaxTermDepth]),
	    Reason = truncate(R, State#?MODULE.reportExceptionMaxTermDepth),

	    [StackTrace|Other]=T,
	    error_logger:warning_report(
		 Other++[{exception,E,Reason},StackTrace]);
	_->ok
    end,
    NewState=State#?MODULE{ cnt = State#?MODULE.cnt + 1},
    {ok, NewState};

handle_event({warning_report, L},State)->
    	error_logger:warning_report(L),
	State;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    NewState = 
	case Info of
	    {timeout,_,_}->
		error_logger:info_msg("report_interval_max_msgs_exceeded: ~p", [{report_interval_max_msgs_exceeded, State#?MODULE.cnt - State#?MODULE.reportIntervalMaxMsgs}]),
		State#?MODULE{cnt=0};
	    _ ->State
	end,
    {ok, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

warning_report(L) ->
    gen_event:notify(comte_error_logger,{warning_report, L}).

error_report(L)->
    error_logger:error_report(L).

info_report(L)->
    error_logger:info_report(L).

truncate(Term, N) when is_tuple(Term) ->
     list_to_tuple(truncate(tuple_to_list(Term), N));

truncate([H|T], N) ->
     if
       N =:= 0 ->
         ['...'];
       true ->
         [truncate(H, N-1)|truncate(T, N-1)]
     end;

truncate(Term, _) ->
     Term.
