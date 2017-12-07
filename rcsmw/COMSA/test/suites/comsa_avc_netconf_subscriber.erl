%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsa_avc_netconf_subscriber.erl %
%%% Author:	erarafo
%%% Description: Implements a netconf subscriber, running as an
%%% unregistered process. Several concurrent instances can be started.
%%%
%%% Modules used: ct_netconfc
%%%
%%% ----------------------------------------------------------
-module(comsa_avc_netconf_subscriber).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R6A/1').
-date('2016-06-03').
-author('erarafo').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% R6A/1      2016-06-03 erarafo     First version, copied from com_avc_netconf_subscriber
%%% ----------------------------------------------------------

-export([start/1,
	 stop/1,
	 getNotifications/1
	]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).


-record(state, {netconfUser,
		notifications=[]
	       }).


-define(SERVER, ?MODULE).


%%% @doc Starts a netconf subscriber instance. The given userid
%%% must match the rct_netconf hook in effect.
%%% @end

-spec start(atom()) -> {ok, pid()} | {error, any()} | ignore.

start(NetconfUser) ->
    gen_server:start_link(?SERVER, [NetconfUser], []).


%%% @doc Stops the given netconf subscriber instance.
%%% @end

-spec stop(pid()) -> ok.

stop(Pid) ->
    gen_server:cast(Pid, {stop}).


%%% @doc Returns the list of collected notifications.
%%% @end

-spec getNotifications(pid()) -> [any()].

getNotifications(Pid) ->
    gen_server:call(Pid, {getNotifications}).


%%% @hidden

init([NetconfUser]) ->
    {ok,_} = ct_netconfc:open(NetconfUser, []),
    ok = ct_netconfc:create_subscription(NetconfUser),
    {ok, #state{netconfUser=NetconfUser}}.


%%% @hidden

handle_call({getNotifications}, _From, #state{notifications=M}=State) ->
    Reply = lists:reverse(M),
    {reply, Reply, State}.


%%% @hidden

handle_cast({stop}, State) ->
    {stop, normal, State}.


%%% @hidden

handle_info(Notification, #state{notifications=N}=State) ->
    {noreply, State#state{notifications=[Notification|N]}}.


%%% @hidden

terminate(_Reason, #state{netconfUser=NetconfUser}) ->
    ok = ct_netconfc:close_session(NetconfUser),
    ok.


%%% @hidden

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
