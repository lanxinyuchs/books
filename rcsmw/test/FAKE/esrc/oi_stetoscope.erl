%%% ----------------------------------------------------------
%%% %CCaseFile:	oi_stetoscope.erl %
%%% Author:	erarafo
%%% Description: This server expects periodic 'heartbeat' calls.
%%% In case of a too long period of silence, which is interpreted
%%% as the heartbeat source having been killed, this VM is
%%% terminated too.
%%%
%%% This module is used by the VM started by test_oi.sh.
%%%
%%% ----------------------------------------------------------
-module(oi_stetoscope).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/1').
-date('2015-09-22').
-author('etxarnu').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
%%% REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R2A/1      2014-06-12 erarafo     First version
%%% R3A/1      2015-02-19 erarafo     Comments only
%%% R3A/2      2015-02-24 etxarnu     Added timestamp to logging
%%% R4A/1      2015-09-22 etxarnu     Increased *_PERIODs to handle NTP time change
%%% ----------------------------------------------------------


-define(SERVER, ?MODULE).
-define(HEART_PERIOD, 60000).
-define(GRACE_PERIOD, 75000).

-export([start/0, heartbeat/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%% ----------------------------------------------------------
%%% @doc Starts the hearbeat listener.
%%% @end
%%% ----------------------------------------------------------

start() ->
    log("starting: ~w", [?SERVER]),
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).


%%% ----------------------------------------------------------
%%% @doc Receives one hearbeat.
%%% @end
%%% ----------------------------------------------------------

heartbeat() ->
    gen_server:cast(?SERVER, heartbeat).


-record(state, {
		birthTime=os:timestamp()  :: erlang:timestamp()
	       }).

init([]) ->
    log("init"),
    {ok, #state{}, ?HEART_PERIOD}.


handle_call(_Request, _From, State) ->
    {reply, ok, State, ?HEART_PERIOD}.


handle_cast(heartbeat, State) ->
    {noreply, State, ?HEART_PERIOD};

handle_cast(_Msg, State) ->
    {noreply, State, ?HEART_PERIOD}.


handle_info(timeout, State) ->
    Now = os:timestamp(),
    Age = timer:now_diff(Now, State#state.birthTime)/1000,
    if
	Age < ?GRACE_PERIOD ->
	    log("no heartbeat; in grace period; keep going"),
	    {noreply, State, ?HEART_PERIOD};
	true ->
	    log("no heartbeat"),
	    {stop, normal, State}
    end;

handle_info(_Info, State) ->
    {noreply, State, ?HEART_PERIOD}.


terminate(_Reason, _State) ->
    log("terminate"),
    init:stop(),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


log(Format) ->
    log(Format, []).


log(Format, Data) ->
    Now = os:timestamp(),
    T = calendar:now_to_local_time(Now),
    io:format("~w ~w: "++Format++"~n", [T|[?SERVER|Data]]).
