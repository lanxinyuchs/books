%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pms_mock.erl %
%%% Author:	erarafo
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(pms_mock).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R2A/5').
-date('2013-02-12').
-author('erarafo').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013 All rights reserved.
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
%%% R2A/3      2013-02-04   erarafo     Created
%%% ----------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NOTE: The current version of this module has not been thoroughly %%
%% tested with the latest versions of PMI and CEC.                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SERVER, ?MODULE).
-define(CEC_PORT, 2345).

-define(SUBSCRIBE, 1).
-define(REPORT, 2).

%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([
		 main/0,
		 cec_setup/1
		]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-export([
		 init/1,
		 handle_cast/2,
		 handle_call/3,
		 handle_info/2,
		 terminate/2,
		 code_change/3
		]).


-export([
		 scenario/1
		]).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

main() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	io:format("~s~n", ["====== RBS CS mock-up ======"]),
	cecDataInit:init([]),
	cec:register("PMI", pms_mock),
	cec_service:start([{port, ?CEC_PORT}]),
	ok.

	
cec_setup(Socket) ->
	io:format("cec_setup handling socket: ~p~n", [Socket]),
	%% NOT! io:format("setting socket option active=true (crucial)~n", []),
	%% inet:setopts(Socket, [{active, true}, {nodelay, true}]),
	
	{ok, UnnamedServerPid} = gen_server:start({local, ?SERVER}, ?MODULE, [Socket], []),
	io:format("started unregistered gen_server, pid: ~p~n", [UnnamedServerPid]),

	%% transfer control *here* so that the gen_server gets all messages
	%% from the very start

	%% NOT! gen_tcp:controlling_process(Socket, UnnamedServerPid),
	%% NOT! UnnamedServerPid ! {send_ready},
	UnnamedServerPid.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

init([Socket]) ->
	spawn(?MODULE, scenario, [self()]),
	{ok, [Socket]}.



handle_cast(stop, S) ->
	{stop, normal, S};

handle_cast(activate, S) ->
	io:format("unexpected, got 'activate'!!~n", []),
	[Socket] = S, inet:setopts(Socket, [{active, once}]),
	{noreply, S};

handle_cast(Message, S) ->	
	io:format("unexpected, got: ~p~n", [Message]),
	[Socket] = S, inet:setopts(Socket, [{active, once}]),
	{noreply, S}.



handle_call(Command, _From, S) ->
	io:format("unexpected call, got: ~p~n", [Command]),
	[Socket] = S, inet:setopts(Socket, [{active, once}]),
	{reply, Command, S}.


%% NOT! handle_info({send_ready}, S) ->
%% 	io:format("send 'ready' to C interface~n", []),
%% 	[Socket] = S,
%% 	gen_tcp:send(Socket, list_to_binary("ready")),
%% 	{noreply, S};

handle_info({subscribe, _Source, GP, GroupSpecs}, S) ->
	io:format("send 'subscribe' to C interface~n", []),
	[Socket] = S,
	gen_tcp:send(Socket, term_to_binary({?SUBSCRIBE, GP, encodeStringLengths(GroupSpecs)})),
	{noreply, S};

handle_info({report, _Source, GP, TimeSpec, Deadline}, S) ->
	io:format("send 'report' to C interface~n", []),
	[Socket] = S,
	gen_tcp:send(Socket, term_to_binary({?REPORT, GP, TimeSpec, Deadline})),
	{noreply, S};

%% handle_info({action, Action}, S) ->
%% 	io:format("send message to C interface: ~s~n", [Action]),
%% 	[Socket] = S,
%% 	gen_tcp:send(Socket, list_to_binary(Action)),
%% 	{noreply, S};

handle_info({tcp, X, Binary}, S) ->
	[Socket] = S,
	io:format("received binary: ~p, socket from message: ~p, socket we have: ~p~n", [Binary, X, Socket]),
	Bytes = binary_to_list(Binary),
	if
		length(Bytes) > 20 ->
			Term = binary_to_term(Binary),
			io:format("decoded into: ~p~n", [Term]);
		true ->
			ok
	end,
	inet:setopts(Socket, [{active, once}]),
	{noreply, S};

handle_info({tcp_closed, X}, S) ->
	io:format("connection closed by client side; socket was: ~p~n", [X]),
	{stop, normal, S};
	
handle_info(Info, S) ->
	io:format("pms:handle_info - unexpected - (~p, ~p)~n", [Info, S]),
	[Socket] = S, inet:setopts(Socket, [{active, once}]),
	{noreply, S}.


terminate(Reason, S) ->
	io:format("pms gen_server terminate, reason: ~p, state: ~p~n", [Reason, S]),
	ok.


code_change(_OldVsn, S, _Extra) ->
	{ok, S}.
	

scenario(TransientProcess) ->
	receive
	after 3000 ->
%% 			io:format("do not trigger any SUBSCRIBE yet~n", [])
			TransientProcess !
				{subscribe, self(),
				 900, 
				 [{"Lepton", ["Electron", "Muon"]}, {"Hadron", ["Neutron", "Proton"]}]
				}
	end,
		
	receive
	after 3000 ->
%% 			io:format("do not trigger any REPORT, yet~n", [])
			TransientProcess ! 
				{report, 
				 self(), 
				 900, 
				 10300, 
				 10400
				}
	end,
		
	receive
	after 3000 ->
			io:format("do not trigger any UNSUBSCRIBE~n", [])
			%TransientProcess ! {action, "UNSUBSCRIBE"}
	end.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


encodeStringLengths({Group, MTypes}) when is_list(Group), is_list(MTypes) ->
	{{length(Group), Group}, [{length(MType), MType} || MType <- MTypes]};

encodeStringLengths(Specs) when is_list(Specs) ->
	[encodeStringLengths(Spec) || Spec <- Specs].


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
