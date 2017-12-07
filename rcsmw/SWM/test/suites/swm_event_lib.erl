%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% @author etxjotj
%%% @copyright Ericsson AB 2017
%%% @version /main/R11A/4

%%% @doc ==Event lib==
%%% This library helps setup and use a NETCONF susbcription for events
%%% @end

-module(swm_event_lib).
-author('etxjotj').

%%% ----------------------------------------------------------
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% R11A/1     2017-09-19 etxjotj     Created
%%% R11A/3     2017-10-05 etxjotj     Added flush and drop
%%% R11A/4     2017-10-10 etxjotj     Handle noproc at stop
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([start_subscription/2, 
	 stop_subscription/0, stop_subscription/1,
	 flush_messages/0, drop_messages/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1]).
-export([event_loop/1]).
-export([system_code_change/4, system_continue/3, system_terminate/4]).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------



%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------

start_subscription(Client, Filter) ->
    Parent = self(), 
    proc_lib:start_link(?MODULE, init, [[Parent, Client, Filter]]).

 
%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------
stop_subscription() ->
    stop_subscription(flush).

stop_subscription(noflush) ->
    try proc_lib:stop(?MODULE)
    catch exit:noproc -> ok
    end;
stop_subscription(flush) ->
    stop_subscription(noflush),
    flush_messages().



flush_messages() ->    
    receive
	_ ->
	    flush_messages()
    after 0 ->
	    ok
    end.

drop_messages(MOC) ->
    receive
	{avc, _, MOC, _, _} ->
	    drop_messages(MOC);
	{objectCreated, _, MOC, _, _} ->
	    drop_messages(MOC);
	{objectDeleted, _, MOC, _} ->
	    drop_messages(MOC)
    after 0 ->
	    ok
    end.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

init([Parent, Client, Filter]) ->
    ct:print("swm_event_lib: ~w starting subscription for ~p with filter~n~p~n",
	     [Client, Parent, Filter]),
    register(?MODULE, self()),
    ct_netconfc:open(Client,[]),
    ct_netconfc:create_subscription(Client,"NETCONF"),
    proc_lib:init_ack(Parent, ok),
    event_loop(#{parent => Parent,
		 client => Client,
		 filter => Filter}).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.

system_continue(Parent, _, Misc) ->
    event_loop(Misc#{parent => Parent}).

system_terminate(Reason, _, _, State) ->
    Client = maps:get(client, State),
    ct_netconfc:close_session(Client),
    ct:print("Event subscription terminating: ~p~n",[Reason]).

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

event_loop(State) ->
    receive 
	{notification, _Attrs, Body} ->
	    %% ct:pal("Received:~n~p~n",[{notification, _Attrs, Body}]),
	    Parent = maps:get(parent, State),
	    [begin
		 %% ct:print("Sending ~p ~n",[Msg]),
		 Parent!Msg
	     end||Msg<-parse_notification(Body)],
	    ?MODULE:event_loop(State);
	%% {?MODULE, stop} ->
	%%     Client = maps:get(client, State),
	%%     ct_netconfc:close_session(Client);
	{system, From, Msg} ->
	    Parent = maps:get(parent, State),
	    sys:handle_system_msg(Msg, From, Parent, ?MODULE, [], State);
	X ->
	    ct:pal("swm_event_lib: received:~n~p~n",[X]),
	    event_loop(State)
						 
    end.

parse_notification([{eventTime, [], [EventTime]}|Body]) ->
    parse_notification(EventTime, Body).

parse_notification(EventTime, [{events, _Attrs, Events}|Body]) ->
    parse_events(EventTime, Events)++
    parse_notification(EventTime, Body);
parse_notification(_, []) -> [].


parse_events(EventTime, [{'AVC', Attrs, Body}|Events]) ->
    Dn = proplists:get_value(dn, Attrs),
    [Instance, Class|_] = lists:reverse(string:tokens(Dn, ",=")),
    Values = parse_msg_body(Body),
    [{avc, EventTime, Class, Instance, Values}|parse_events(EventTime, Events)];
parse_events(EventTime, [{objectCreated, Attrs, Body}|Events]) ->
    Dn = proplists:get_value(dn, Attrs),
    [Instance, Class|_] = lists:reverse(string:tokens(Dn, ",=")),
    Values = parse_msg_body(Body),
    [{objectCreated, EventTime, Class, Instance, Values}|
     parse_events(EventTime, Events)];
parse_events(EventTime, [{objectDeleted, [{dn, DN}], _}|Events]) ->
    [Instance, Class|_] = lists:reverse(string:tokens(DN, ",=")),
    [{objectDeleted, EventTime, Class, Instance}|
     parse_events(EventTime, Events)];
parse_events(_, []) -> [].

parse_msg_body([{attr, [{name, Name}], [{v, [], [{elem,_,_}|_] = Elements}]}|Body]) ->
    %% Structs
    Value = parse_elements(Elements),
    [{list_to_atom(Name), Value}|parse_msg_body(Body)];
parse_msg_body([{attr, [{name, Name}], Values}|Body]) ->
    Value = [V||{v, _, V}<-Values],
    %% Singelton value
    [{list_to_atom(Name), Value}|parse_msg_body(Body)];
parse_msg_body([]) -> [].

parse_elements([{elem, [{name, Name}], Values}|Elements]) ->
    Value = [V||{v, _, V}<-Values],
    [{list_to_atom(Name), Value}|parse_elements(Elements)];
parse_elements([]) ->
    [].


		      


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

