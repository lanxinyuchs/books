%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	cec_service.erl %
%%% @author etxarnu
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R2A/R3A/R4A/R5A/2
%%%
%%% @doc == CEC server ==
%%%
%%% ----------------------------------------------------------
-module(cec_service).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/2').
-date('2016-03-04').
-author('etxarnu').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% R1A/1      2012-10-03 etxbjca     Created
%%% R2A/5      2014-03-18 erarafo     Added an event for the restart log
%%% R2A/6      2014-05-26 etxpeno     Bind TCP socket to localhost
%%% R3A/2      2014-11-03 etxpeno     Don't crash the acceptor if the call to
%%%                                   cec_setup() crashes
%%% R3A/3      2014-12-19 etxberb     Increased timer value from 5000 to 10000.
%%% R4A/3      2015-10-06 etxberb     * Added callback function cec_takeover/1.
%%%                                   * Added error cases for gen_tcp & inet.
%%%                                   * Added warnings for unrec msgs.
%%% R4A/4      2015-10-12 etxberb     * TR HU24243: Changed from variable length
%%%                                     of signature msgs to fixed length (16).
%%%                                   * Added separate process for each socket
%%%                                     to handle the reception of signature
%%%                                     msg. The Acceptor process used to block
%%%                                     queued sockets while waiting for
%%%                                     signature messages in sequence and this
%%%                                     caused hangings in applications.
%%% R5A/1      2016-02-04 etxpeno     change an error to a warning
%%% R5A/2      2016-03-04 etxarnu     Touch /tmp/cec_ready for tmmi
%%% ----------------------------------------------------------


-export([p_al/3]).


-export([start/0,
	 start/1,
	 start_link/0,
	 start_link/1,
	 stop/0]).

-export([lookup_registration/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([recv_init/1]).

%% Test & debug
-export([get_state/0,
	 info/0,
	 info_all/0]).

%%%===================================================================
%%% Macros
%%%===================================================================
-define(SERVER, ?MODULE).

%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).
-define(PROC_INFO(Pid), sysUtil:pid_info(Pid, {all, [error_handler]})).
-define(STATE_INFO(Record),
	sysUtil:record_format(record_info(fields, state), Record)).

%%%===================================================================
%%% Records
%%%===================================================================
-record(state, {acceptor,
		port,
		registrations = []}).

%%%===================================================================
%%% Code
%%%===================================================================
start() ->
    CecPort = sysEnv:get_port_conf(cecPort),
    start([{port, CecPort}]).
start(Opts) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Opts, []).

start_link() ->
    CecPort = sysEnv:get_port_conf(cecPort),
    start_link([{port, CecPort}]).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

stop() ->
    gen_server:cast(?SERVER, stop).

lookup_registration(Packet) ->
    gen_server:call(?SERVER, {lookup_registration, Packet}).

%%% ###########################################################################
%%% @doc Get the server process state.
%%%
%%% @end
%%% ###=====================================================================###
get_state() ->
    gen_server:call(?SERVER, {?MODULE, get_state}).

%%% ###########################################################################
%%% @doc Print the server process state and information.
%%%
%%% @end
%%% ###=====================================================================###
info() ->
    Msg = {?MODULE, info},
    gen_server:cast(?SERVER, Msg).

%%% ###=====================================================================###
info_all() ->
    [rpc:cast(Node, ?MODULE, info, []) || Node <- clhI:erlang_nodes(all)].

%%% ###########################################################################
%%% @doc Start the server process.
%%%
%%% @end
%%% ###=====================================================================###
init(Opts) ->
    erlang:process_flag(trap_exit, true),
    Port = proplists:get_value(port, Opts),
    mnesia:subscribe({table, cecRegistration, detailed}),
    Registrations = get_registrations(),
    State = #state{acceptor      = start_acceptor(Port),
		   port          = Port,
		   registrations = Registrations},
    sysInitI:restart_logger_trace(?MODULE, ?LINE, "CEC service started"),
    case sysEnv:target() of
	true ->
	    file:write_file("/tmp/cec_ready","true");
	false -> ok
    end,
    {ok, State}.

handle_call({lookup_registration, Packet}, _From, S)->
    Reply =
	case gb_trees:lookup(Packet, S#state.registrations) of
	    none ->
		false;
	    {value, Module} ->
		{true, Module}
	end,

    {reply, Reply, S};
handle_call({?MODULE, get_state}, _From, State) ->
    {reply, State, State};
handle_call(Command, From, S)->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
			     "Unexpected message received",
			     {from, sysUtil:pid_info(From)},
			     {msg, Command}]),
    {reply, Command, S}.

handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast({?MODULE, info}, State) ->
    sysInitI:info_report(?PROC_INFO(self()) ++ ?STATE_INFO(State)),
    {noreply, State};
handle_cast(Msg, S) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
			     "Unexpected message received",
			     {msg, Msg}]),
    {noreply, S}.

handle_info({mnesia_table_event,
	     {write, cecRegistration, {cecRegistration, Packet, Module}, _, _}},
	    S) ->
    NewRegistrations = gb_trees:enter(Packet, Module, S#state.registrations),
    {noreply, S#state{registrations = NewRegistrations}};
handle_info({mnesia_table_event,
	     {delete, cecRegistration, {cecRegistration, Packet}, _, _}}, S) ->
    NewRegistrations = gb_trees:delete_any(Packet, S#state.registrations),
    {noreply, S#state{registrations = NewRegistrations}};
handle_info({'EXIT', Pid, What}, #state{acceptor = Pid} = S) ->
    sysInitI:error_report([{?MODULE, ?FUNCTION, ['EXIT']},
			   {pid, Pid},
			   {reason, What},
			   {state, S},
			   {acceptor, restarted}
			  ]),
    AcceptorPid = start_acceptor(S#state.port),
    {noreply, S#state{acceptor = AcceptorPid}};

handle_info({'EXIT', Pid, What}, S) ->
    sysInitI:error_report([{?MODULE, ?FUNCTION, ['EXIT']},
			   {pid, Pid},
			   {reason, What},
			   {state, S}
			  ]),
    {noreply, S};

handle_info(Info, S) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
			     "Unexpected message received",
			     {msg, Info}]),
    {noreply, S}.

terminate(Reason, #state{acceptor = Acceptor}) when is_pid(Acceptor) ->
    erlang:exit(Acceptor, Reason),
    ok;
terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

get_registrations() ->
    WildPattern = mnesia:table_info(cecRegistration, wild_pattern),
    List = mnesia:dirty_match_object(cecRegistration, WildPattern),

    lists:foldl(fun({cecRegistration, Packet, Module}, AccIn) ->
			gb_trees:enter(Packet, Module, AccIn)
		end, gb_trees:empty(), List).

start_acceptor(Port) ->
    Fun = fun() -> acceptor_init(Port) end,
    spawn_link(Fun).

acceptor_init(Port) ->
    Opts = [binary,
	    {packet, raw},
	    {backlog, 10},
	    {reuseaddr, true},
	    {active, false},
	    {ip, {127,0,0,1}}
	   ],
    {ok, Listen} = gen_tcp:listen(Port, Opts),
    acceptor_loop(Listen).

acceptor_loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),

    %% ?MODULE:p_al(time(), self(), inet:peername(Socket)),

    Pid = spawn(?MODULE, recv_init, [Socket]),
    gen_tcp:controlling_process(Socket, Pid),
    Pid ! {?MODULE, cec_takeover},

    acceptor_loop(Listen).

recv_init(Socket) ->
    try
	recv(Socket)
    catch
	ErrClass : ErrReason ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   {ErrClass, ErrReason},
				   {socket, Socket},
				   {peer, inet:peername(Socket)}])
    end,
    exit(normal).

recv(Socket) ->
    receive
	{?MODULE, cec_takeover} ->
	    handle_recv(gen_tcp:recv(Socket, 16, 30000), Socket)
    end.

handle_recv({ok, Packet},    Socket) ->
    [Tag | Tail] = binary:split(Packet, <<0>>),
    check_fill(Tail),
    handle_lookup_registration(lookup_registration(Tag),
			       Socket,
			       Packet);
handle_recv({error, Reason}, Socket) ->
    sysInitI:error_report([{?MODULE, ?FUNCTION},
			   {socket, Socket},
			   {peer, inet:peername(Socket)},
			   {error, Reason}
			  ]),
    gen_tcp:close(Socket).

check_fill([Bin | _]) ->
    check_fill((catch binary:bin_to_list(Bin)), Bin);
check_fill([]) ->
    ok.

check_fill([0 | Tail], Bin) ->
    check_fill(Tail, Bin);
check_fill([], _) ->
    ok;
check_fill(Error, Bin) ->
    sysInitI:error_report([{?MODULE, ?FUNCTION},
			   {not_fill_characters, Error},
			   {packet, Bin}]).

handle_lookup_registration(false,          Socket,  Packet) ->
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
			     {tag, Packet},
			     {error, not_registered}
			    ]),
    gen_tcp:close(Socket);
handle_lookup_registration({true, Module}, Socket, _Packet) ->
    try	Module:cec_setup(Socket) of
	Pid when is_pid(Pid) ->
	    case gen_tcp:controlling_process(Socket, Pid) of
		ok ->
		    ok;
		ErrorGenTcp ->
		    PortInfo = (catch erlang:port_info(Socket)),
		    sysInitI:error_report([{?MODULE, ?FUNCTION},
					   {gen_tcp, ErrorGenTcp},
					   {cb_module, Module},
					   {socket, Socket},
					   {packet, _Packet},
					   {port, PortInfo}]),
		    gen_tcp:close(Socket)
	    end,
	    inet:setopts(Socket, [{packet, 4}]),
	    try Module:cec_takeover(Socket)
	    catch
		error : undef ->
		    %% It is unsafe to set the socket to active directly from
		    %% the CEC process. It has been proven that an early message
		    %% can disappear when doing like this. (Warm restart case)
		    %% It is strongly recommended that the callback module
		    %% implements the cec_takeover/1 function, which should
		    %% send a message to the controlling process, and the
		    %% activation of the socket is done from there.
		    %% NOTE, the problem described here is valid when setting
		    %% Port Option '{packet, 4}' and sending signature messages
		    %% with varaible length. The problem seems to disappear when
		    %% instead using '{packet, raw}' and fixed length.
		    case inet:setopts(Socket,
				      [{active, once}, {nodelay, true}]) of
			ok ->
			    ok;
			ErrorInet ->
			    sysInitI:error_report([{?MODULE, ?FUNCTION},
						   {inet, ErrorInet}]),
			    gen_tcp:close(Socket)
		    end;
		  ErrClass : ErrReason ->
		    sysInitI:error_report([{?MODULE, ?FUNCTION},
					   {module, Module},
					   {ErrClass, ErrReason}
					  ]),
		    gen_tcp:close(Socket)
	    end;
	Not_a_Pid ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   {module, Module},
				   {pid, Not_a_Pid}
				  ]),
	    gen_tcp:close(Socket)
    catch
	EClass : EReason ->
	    sysInitI:warning_report([{?MODULE, ?FUNCTION},
				     {module, Module},
				     {EClass, EReason}
				    ]),
	    gen_tcp:close(Socket)
    end.

p_al(_T, _P, _S) ->
    ok.
%%io:format(" #### cec_service acceptor_loop ~p ~p ~p ~n", [T, P, S]).
