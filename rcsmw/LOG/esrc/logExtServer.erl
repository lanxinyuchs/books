%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	logExtServer.erl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(logExtServer).
-vsn('/main/R1A/R2A/R4A/1').
-date('2015-09-01').
-author('etxjotj').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2015 All rights reserved.
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
%%% R1A/1      2012-02-06 etxjotj     Created
%%% R2A/1      2014-06-10 etxarnu     TR HS61928: Bind socket to local 
%%%                                   interface only
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-export([system_code_change/4, system_continue/3, system_terminate/4]).
-export([init/0]).

-record(state, {lsock, parent, debug=[]}).
-include("LogExtServer.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

start() ->
    proc_lib:start_link(?MODULE, init, []).

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

init() ->
    register(?MODULE, self()),
    Port = sysEnv:get_port_conf(log),
    Options = [binary,
	       {ip, {127,0,0,1}},
	       {reuseaddr, true},
	       {keepalive, true},
	       {packet, tpkt},
	       {active, false}],
    case catch gen_tcp:listen(Port, Options) of
	{ok, LSock} ->
	    info_msg("Incoming on port ~w~n",[Port]),
	    %% The '$ancestors' value is set by proc_lib
	    [Parent|_] = get('$ancestors'),
	    proc_lib:init_ack({ok, self()}),
	    loop(#state{lsock = LSock, parent = Parent});
	{E, Reason} when E==error; E=='EXIT' ->
	    sysInitI:info_report(
	      [{?MODULE, init, []},
	       {mfa, {gen_tcp, listen, [Port, Options]}},
	       {E, Reason}]),
	    [Parent|_] = get('$ancestors'),
	    proc_lib:init_ack({ok, self()}),
	    socket_loop(Port, Options, calendar:universal_time(),
			#state{lsock = undefined, 
			       parent = Parent});

	{E, Reason} when E==error; E=='EXIT' ->
	    proc_lib:init_ack({error, Reason})
    end.    


system_terminate(Reason, _, _, State) when is_record(State, state)->
    gen_tcp:close(State#state.lsock),
    exit(Reason);
system_terminate(Reason, _, _, _) ->
    exit(Reason).


system_code_change(State, _, _, _) ->
    {ok, State}.

%% Debug can be updated in the sys suspend loop
system_continue(Parent, Debug, State) ->
    loop(State#state{parent = Parent, debug = Debug}).


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

socket_loop(Port, Options, Time, State) ->
    case catch gen_tcp:listen(Port, Options) of
	{ok, LSock} ->
	    %% The '$ancestors' value is set by proc_lib
	    loop(State#state{lsock = LSock});
	{E, Reason} when E==error; E=='EXIT' ->
	    sysInitI:info_report(
	      [{?MODULE, socket_loop, [Port, Options, Time, State]},
	       {mfa, {gen_tcp, listen, [Port, Options]}},
	       {E, Reason}]),
	    Now = calendar:universal_time(),
	    Diff = calendar:datetime_to_gregorian_seconds(Now)-
		calendar:datetime_to_gregorian_seconds(Time),
	    case Diff of
		Diff when Diff>60 -> 
		    erlang:error({E, Reason}, [Port, Options, Time, State]);
		_ ->
		    timer:sleep(1000),
		    socket_loop(Port, Options, Time, State)
	    end
    end.
  
loop(State) ->
    case gen_tcp:accept(State#state.lsock, 1000) of
	%% 1 second is arbitrarily chosen. Perhaps the timeout can be as much
	%% as 4 seconds. We only have to match it with the corresponding
	%% suspend/change_code/resume requests that might timeout. That
	%% is 5 seconds per default.
	{ok, Socket} ->
%	    info_msg("Socket accepted: ~p ~p~n",[Socket, inet:peername(Socket)]),
	    inet:setopts(Socket, [{reuseaddr, true},
				  {keepalive, true},
				  {active, false}]),
	    {ok, Pid} = logExtHandler:start(Socket),
	    gen_tcp:controlling_process(Socket, Pid),
	    MonitorRef = monitor(process, Pid),
	    mnesia:dirty_write(#logExtServer{monitor = MonitorRef,
					     socket = Socket,
					     pid = Pid}),
	    loop(State);
	{error, timeout} ->
	    %% I would have liked the gen_tcp:accept/2 to be asynchronous
	    %% But it turns out that it all boils down to the 
	    %% erlang:port_control/3 BIF which isn't, so we have to use
	    %% this construct. 
	    Parent = case State#state.parent of
			 Pid when is_pid(Pid) -> Pid;
			 Name when is_atom(Name) -> whereis(Name)
		     end,
	    receive
		{'EXIT', Parent, Reason} ->
		    system_terminate(Reason, Parent, State#state.debug, State);
		{'EXIT', _, _} = ExitMsg -> % Should not happen, but hey
		    system_terminate(ExitMsg, Parent, State#state.debug,State);
		{'DOWN', _, _, _, _} = Down ->
		    do_handle_down(Down);
			    
		{system, From, Msg} ->
		    ParentA = State#state.parent,
		    DebugA= State#state.debug,
		    sys:handle_system_msg(
		      Msg, From, ParentA, ?MODULE, DebugA, State)
	    after 0 ->
		    loop(State)
	    end;
	{error, Reason} -> 
	    erlang:error(Reason, [State])
    end.

%do_handle_down({'DOWN', MonitorRef, process, Pid, Info}) ->

do_handle_down({'DOWN', MonitorRef, process, _, _}) ->
    case mnesia:dirty_read({logExtServer, MonitorRef}) of
	[Obj] ->
	    %% Cleaning up the supervisor list
	    Socket = Obj#logExtServer.socket,
	    Id = {logExtHandler, Socket},
	    supervisor:terminate_child(logSuper, Id),
	    supervisor:delete_child(logSuper, Id),
	    mnesia:dirty_delete_object(Obj);
	[] -> %shouldn't really happen
	    ok
    end,
    receive
	{'DOWN', _, _, _, _} = Down -> % Flush all down messages in queue
	    do_handle_down(Down)
    after 0 ->
	    ok
    end.


%% info_msg(Format) ->
%%     info_msg(Format, []).

info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%error_msg(Format) ->
%    error_msg(Format, []).
%% error_msg(Format, Args) ->
%%     sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

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

