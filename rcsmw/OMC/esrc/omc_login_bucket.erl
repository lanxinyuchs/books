%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_login_bucket.erl %
%%% Author:	etxbjca
%%% Description: Gen-server to keep track of failed logins. A delay
%%%	served based on the number of failures in the bucket.
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(omc_login_bucket).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R4A/1').
-date('2015-10-21').
-author('etxlg').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% Rx         2015-10-19   etxlg     Created
%%% ----------------------------------------------------------
%%%
%%%-include("omc.hrl").
%%%-compile([export_all]).

-define(MAX_N, 6).

%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([fail/1]).
-export([get_delay/0]).


-export([start_link/0]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%gen_server callbacks
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2]).
%format_status/2]).

-define(SERVER, ?MODULE).

-record(st, {
	bucket = 0,
        timer = undefined,
	nada
	}).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, sysEnv:role(), []).

%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
-spec fail(pid()) -> integer().
fail(Pid) ->
    gen_server:call(?SERVER, {fail, Pid}).

-spec get_delay() -> integer().
get_delay() ->
    gen_server:call(?SERVER, get_delay).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
init(active) ->
    {ok, #st{}};
init(regular) ->
    ignore.

handle_call({fail, _Pid}, _From, #st{bucket = 0} = S) ->
    {reply, 0, run_timer(S#st{bucket = 1})};
handle_call({fail, _Pid}, _From, #st{bucket = N} = S) ->
    {New_n, Backoff_time} = backoff(N),
    {reply, Backoff_time, run_timer(S#st{bucket = New_n})};

handle_call(get_delay, _From, #st{bucket = N} = S) ->
    {_, Backoff_time} = backoff(N),
    {reply, Backoff_time, S};

handle_call(_Req, _From, S) ->
    sysInitI:info_msg("~p: Unrecognized handle_call: ~p~n", [?MODULE, _Req]),
    {reply, "unrecognized call", S}.

handle_cast(_Req, S) ->
    sysInitI:info_msg("~p: Unrecognized handle_cast: ~p~n", [?MODULE, _Req]),
    {noreply, S}.

handle_info(bucket, #st{bucket = N} = S) ->
    case N of
	0 ->
	    {noreply, S#st{timer = undefined,
			   bucket= 0}};
	1 ->
	    {noreply, S#st{timer = undefined,
			   bucket= 0}};
	N ->
	    {noreply, run_timer(S#st{timer = undefined,
				     bucket= N - 1})}
	end;
handle_info(_Info, S) ->
    sysInitI:info_msg("~p: Unrecognized handle_info: ~p~n", [?MODULE, _Info]),
    {noreply, S}.

code_change(_Oldvsn, S, _Extra) ->
    {ok, S}.

terminate(_Reason, _S) ->
    ok.
%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
backoff(0) ->
    {1, my_exp(0)};
backoff(N) when N < ?MAX_N ->
    {N + 1, my_exp(N)};
backoff(?MAX_N) ->
    {?MAX_N, my_exp(?MAX_N)}.

run_timer(#st{timer = undefined, bucket = N} = S) ->
    Ref = erlang:send_after(1000 * my_mult(N), self(), bucket),
    S#st{timer = Ref};
run_timer(S) ->
    S.

%0->0, 1->1000... 6->32000
my_exp(N) when N =< ?MAX_N ->
    (1 bsl (N - 1)) * 1000.

my_mult(0) -> 1;
my_mult(N) -> N * 2.
