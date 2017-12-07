%%% %CCaseFile:	sigHandler.erl %
%%% @author erarafo
%%% @copyright Ericsson AB 2015
%%% @version /main/R4A/4

%%% @doc ==Signal handler==
%%% This module implements a signal handler that are used by many
%%% TIM test cases.

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
%%% R4A/1      2015-10-30 erarafo     First version
%%% R4A/2      2015-11-02 erarafo     Switch to OTP 18 time API
%%% R4A/3      2015-11-03 erarafo     getSignals request added
%%% R4A/4      2015-11-09 erarafo     fault fixed
%%% ----------------------------------------------------------

-module(sigHandler).
-id('Updated by CCase').
-vsn('/main/R4A/4').
-date('2015-11-09').
-author('erarafo').


-export([start/2,
	 call/2,
	 call/3,
	 cast/2,
	 getSignals/3
	]).

-include("sigHandler.hrl").

-define(CALL_TIMEOUT_DEFAULT, 5000).



%%% ----------------------------------------------------------
%%% @doc Spawns a signal handler.
%%% @end
%%% ----------------------------------------------------------
-spec start(module(), {pid(), integer(), function(), [function()]}) ->
	  {ok, pid()}|{error, atom()}.

start(CbModule, Args) ->
    Pid =
	spawn(
	  fun() ->
		  case apply(CbModule, init, [Args]) of
		      {ok, State} ->
			  shLoop(CbModule, State, infinity, ?MILLIS(), orddict:new(), ordsets:new());
		      {ok, State, Timeout} ->
			  shLoop(CbModule, State, Timeout, ?MILLIS(), orddict:new(), ordsets:new())
		  end
	  end),
    case is_pid(Pid) andalso is_process_alive(Pid) of
	true ->
	    {ok, Pid};
	false ->
	    {error, process_not_alive}
    end.


call(Pid, Msg) ->
    call(Pid, Msg, ?CALL_TIMEOUT_DEFAULT).

call(Pid, Msg, Timeout) ->
    Ref = make_ref(),
    Pid ! {signal, {'$call', self(), Ref, Msg}},
    receive
	{'$response', Ref, Result} ->
	    Result
    after Timeout ->
	ct:fail("call timeout after ~w ms", [Timeout])
    end.


cast(Pid, Msg) ->
    Pid ! {signal, {'$cast', Msg}},
    ok.


-spec getSignals(pid(), 
		 {non_neg_integer(), non_neg_integer()|[non_neg_integer()]|undefined}, 
		 timeout()) ->
	  {ok, [#signal{}]}|timeout.

getSignals(Pid, Pattern, Timeout) ->
    Pid ! {signal, {'$getSignals', self(), Pattern, Timeout}},
    receive
	Result ->
	    Result
    end.


%%% ----------------------------------------------------------
%%% @doc Signal handler loop, doing callbacks in gen_server
%%% style.
%%% @end
%%% ----------------------------------------------------------
-spec shLoop(module(), 
	     any(), 
	     timeout(), 
	     timestamp(), 
	     orddict:orddict(),
	     ordsets:ordset(#signal{})) -> ok.

shLoop(CbModule, State, Timeout, Started, Requests, Signals) ->
    case rct_proxy:receive_proxy(minTimeout(Timeout, smallestTimeout(Requests))) of
	{error, timeout} ->
	    % possibly a request has timed out
	    {NewReqs, NewSigs} = scanRequests(Requests, Signals),
	    Before = orddict:size(Requests),
	    After = orddict:size(NewReqs),
	    if
		After < Before ->
		    shLoop(CbModule, State, Timeout, Started, NewReqs, NewSigs);
		true ->
		    % no; then callback instead
		    case apply(CbModule, handle_info, [timeout, State]) of
			{noreply, NewState} ->
			    shLoop(CbModule, NewState, infinity, Started, NewReqs, NewSigs);
			{noreply, NewState, NewTimeout} ->
			    shLoop(CbModule, NewState, NewTimeout, Started, NewReqs, NewSigs)
		    end
	    end;
	
	{ok, {'$call', Caller, Ref, Msg}} ->
	    case apply(CbModule, handle_call, [Msg, {Caller, Ref}, State]) of
		{reply, Result, NewState} ->
		    Caller ! {'$response', Ref, Result},
		    shLoop(CbModule, NewState, infinity, Started, Requests, Signals);
		{reply, Result, NewState, NewTimeout} ->
		    Caller ! {'$response', Ref, Result},
		    shLoop(CbModule, NewState, NewTimeout, Started, Requests, Signals)
	    end;
	
	{ok, {'$cast', Msg}} ->
	    case apply(CbModule, handle_cast, [Msg, State]) of
		{noreply, NewState} ->
		    shLoop(CbModule, NewState, infinity, Started, Requests, Signals);
		{noreply, NewState, NewTimeout} ->
		    shLoop(CbModule, NewState, NewTimeout, Started, Requests, Signals);
		{stop, Reason, NewState} ->
		    apply(CbModule, terminate, [Reason, NewState]),
		    ok
	    end;
	
	{ok, {'$getSignals', CallerPid, Pattern, ReqTimeout}} ->
	    Deadline = ?MILLIS() + ReqTimeout,
	    NewReqs1 = orddict:store(CallerPid, {Pattern, Deadline}, Requests),
	    {NewReqs2, NewSignals} = scanRequests(NewReqs1, Signals),
	    shLoop(CbModule, State, Timeout, Started, NewReqs2, NewSignals);
	
	{ok, SignalData} when
	  is_tuple(SignalData) andalso
	      tuple_size(SignalData) >= 2 ->
	    Signal =
		#signal{arrived=?MILLIS() - Started,
			source=element(1, SignalData),
			no=element(2, SignalData),
			fields=tl(tl(tuple_to_list(SignalData)))},
	    NewSignals = ordsets:add_element(Signal, Signals),
	    {NewReqs2, NewSignals2} = scanRequests(Requests, NewSignals),
	    shLoop(CbModule, State, Timeout, Started, NewReqs2, NewSignals2)
    end.


%%% ----------------------------------------------------------
%%% @doc Scan requests and send back results if available,
%%% and handle timeouts.
%%% @end
%%% ----------------------------------------------------------
-spec scanRequests(orddict:orddict(), ordsets:ordset(#signal{})) ->
	  {orddict:orddict(), ordsets:ordset(#signal{})}.

scanRequests(Requests, Signals) ->
    Now = ?MILLIS(),
    orddict:fold(
      fun(CallerPid, {Pattern, Deadline}, {Reqs, Sigs}) ->
	      if 
		  Now >= Deadline ->
		      CallerPid ! timeout,
		      {orddict:erase(CallerPid, Reqs), Sigs};
		  true ->
		      case matchSignals(Pattern, Sigs) of
			  {[], NewSigs} ->
			      {Reqs, NewSigs};
			  {Matching, NewSigs} ->
			      CallerPid ! {ok, Matching},
			      {orddict:erase(CallerPid, Reqs), NewSigs}
		      end
	      end
      end,
      {Requests, Signals},
      Requests).


%%% ----------------------------------------------------------
%%% @doc Returns matching signals and remaining signals.
%%% @end
%%% ---------------------------------------------------------- 
-spec matchSignals({non_neg_integer(), sigSpec()}, ordsets:ordset(#signal{})) ->
	  {[#signal{}], ordsets:ordset(#signal{})}.
	  
matchSignals({ChildPid, SigSpec}, Sigs) ->
    ordsets:fold(
      fun(#signal{source=S, no=SigNo}=X, {Matched, Remaining}) when S =:= ChildPid ->
	      if
		  SigSpec =:= undefined ->
		      {[X|Matched], Remaining};
		  is_integer(SigSpec) andalso SigNo =:= SigSpec ->
		      {[X|Matched], Remaining};
		  is_integer(SigSpec) ->
		      {Matched, ordsets:add_element(X, Remaining)};
		  is_list(SigSpec) ->
		      case lists:member(SigNo, SigSpec) of
			  true ->
			      {[X|Matched], Remaining};
			  false ->
			      {Matched, ordsets:add_element(X, Remaining)}
		      end
	      end;
	 (X, {Matched, Remaining}) ->
	      {Matched, ordsets:add_element(X, Remaining)}
      end,
      {[], ordsets:new()},
      Sigs).


%%% ----------------------------------------------------------
%%% @doc Returns the smallest timeout for the given
%%% collection of requests.
%%% ----------------------------------------------------------
-spec smallestTimeout(orddict:orddict()) ->
	  timeout().

smallestTimeout(Requests) ->
    Now = ?MILLIS(),
    orddict:fold(
      fun(_Pid, {_Pattern, Deadline}, Acc) ->
	      minTimeout(Deadline - Now, Acc)
      end,
      infinity,
      Requests).


%%% ----------------------------------------------------------
%%% @doc Returns the lesser of two timeouts.
%%% @end
%%% ----------------------------------------------------------
-spec minTimeout(timeout(), timeout()) ->
	  timeout().

minTimeout(infinity, infinity) -> 
    infinity;
    
minTimeout(infinity, Other) -> 
    max(0, Other);

minTimeout(Other, infinity) ->
    max(0, Other);

minTimeout(A, B) ->
    max(0, min(A, B)).
