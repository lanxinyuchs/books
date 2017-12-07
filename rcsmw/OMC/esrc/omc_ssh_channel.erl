%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_ssh_channel.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R9A/2

%%% @doc ==CLI service==
%%% This implements the interface between the erlang ssh server and COM
%%% upon connection it runs either the COM cliss-binary or the COM netconf-
%%% binary.

-module(omc_ssh_channel).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R9A/2').
-date('2017-04-05').
-author('etxpeno').

-include("omc.hrl").
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R2A/1      2014-01-17   etxlg     Replaces sysSshCliChannel R2A/13
%%% R2A/2      2014-01-27   etxlg     Removed netconf_shell
%%%					support for ms_user_change
%%% R2A/3      2014-02-03   etxlg     New OTP -> fix dialyzer warnings
%%% R2A/4      2014-02-03   etxlg     New OTP -> fix dialyzer warnings, again
%%% R2A/5      2014-02-06   etxlg     Base reverted to OTP R16B02
%%% R2A/6      2014-04-09   etxlg     limited handling of window size
%%% R2A/7      2014-04-10   etxlg     bug fixed above
%%% R2A/8      2014-06-16   etxlg     store session information
%%% R2A/9      2014-06-17   etxlg     functions for external exit
%%% R2A/10     2014-07-03   etxberb   Added coli_logging/1.
%%% R2A/11     2014-07-07   etxlg     Peer IP included in security log
%%% R2A/12     2014-07-24   etxarnu   Moved cli/netconf logs to rcs/comte
%%% R3A/1      2015-04-15   etxlg     Print, don't crash when session goes away
%%% R3A/3      2015-05-12   etxlg     part fix TR HT74324 (data before shell)
%%% R4A/1      2015-08-31   etxlg     Alternate connection
%%% R4A/3      2015-09-24   etxlg     Handle COM going missing during warm-start
%%% R4A/4      2015-10-07   etxlg     Store ssh conn_ref, use at warm restart
%%% R5A/1      2016-03-07   etxlg     Close connection at session ending
%%% R6A/1      2016-05-19   etxpeno   Support for logging outgoing NETCONF messages
%%% R6A/2      2016-08-30   uabesvi   vRC should be treated as target and not simulated
%%% R7A/1      2016-08-08   etxpeno   dialyzer fixes
%%% R7A/2      2016-10-17   etxpeno   improve information in security log
%%% R9A/2      2017-04-05   etxpeno   support logging in vRC
%%% ---------------------------------------------------------------------- 80 >
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%-compile(export_all).
-export([init/1, terminate/2, handle_ssh_msg/2, handle_msg/2]).
-export([close_session/2]).

%% for test only
-export([log_enable/1, log_enable_outgoing/1, log_disable/1]).
-export([coli_logging/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-define(DEFAULT_COLS, 80).
-define(DEFAULT_ROWS, 24).

-record(state,
	{type :: cli|netconf,
	 interface,
	 port :: port() | 'undefined',
	 id,
	 ref,
	 user,
	 ms_user,
	 roles,
	 peer_ip = "-",
	 logFD,
	 is_change_user_cap = false,
	 ch_st, % #user_scan{}
	 rows = ?DEFAULT_ROWS,
	 cols = ?DEFAULT_COLS,
	 log_outgoing = false
        }).

-define(CLISS_BIN, "cliss").
-define(NCONF_BIN, "netconf").
-define(PIPE_PTY_BIN, "pipe_pty"). %needed for both cliss and netconf
-define(COM_USER_FILE, "/etc/rcs_cs/com_user").

-define(NETCONF_LOG_ENABLE, filename:join([sysEnv:vnf_dir(),"enable_netconf_log"]) ).
-define(NETCONF_LOG_OUTGOING_ENABLE,
	filename:join([sysEnv:vnf_dir(),"enable_outgoing_netconf_log"]) ).

-include_lib("xmerl/include/xmerl.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
close_session(Pid, Message) when is_pid(Pid) ->
    Pid ! {close_session, Message}.

coli_logging(["-d"]) ->
    log_disable([]);
coli_logging(["-e"]) ->
    log_enable([]);
coli_logging(["-a"]) ->
    log_enable_outgoing([]);
coli_logging(_) ->
    io:format("argument error~n").

log_enable(_) ->
    os:cmd("touch " ++ ?NETCONF_LOG_ENABLE),
    os:cmd("rm -f " ++ ?NETCONF_LOG_OUTGOING_ENABLE).
log_enable_outgoing(_) ->
    os:cmd("rm -f " ++ ?NETCONF_LOG_ENABLE),
    os:cmd("touch " ++ ?NETCONF_LOG_OUTGOING_ENABLE).
log_disable(_) ->
    os:cmd("rm -f " ++ ?NETCONF_LOG_ENABLE),
    os:cmd("rm -f " ++ ?NETCONF_LOG_OUTGOING_ENABLE).

log_fd(Type) ->
    case trigger_logging() of
	false ->
	    undefined;
	true ->
	    TS = {_,_,US} = os:timestamp(),
	    LogFile =
		filename:join([sysEnv:vnf_dir(),
			       "comte",
			       atom_to_list(Type)++
				   ".log." ++
				   comsaI:iso_time(TS,basic) ++
				   "." ++ integer_to_list(US)]),
	    {ok,Fd}=file:open(LogFile,[write]),
	    Fd
    end.

close_log(undefined) -> ok;
close_log(FD) -> file:close(FD).

log_data(_Data, undefined, _) -> ok;
log_data(_Data, _FD, false) -> ok;
log_data(Data, FD, true) -> io:fwrite(FD,"~s",[Data]).

log_outgoing(cli) ->
    false;
log_outgoing(netconf) ->
    trigger_logging(?NETCONF_LOG_OUTGOING_ENABLE).

trigger_logging() ->
    F = fun(Filename) -> trigger_logging(Filename) end,
    lists:any(F, [?NETCONF_LOG_ENABLE, ?NETCONF_LOG_OUTGOING_ENABLE]).

trigger_logging(Filename) ->
    case file:read_file_info(Filename) of
	{ok, _} ->
	    true;
	{error, _} ->
	    false
    end.

-spec init([cli|netconf]) -> {ok, #state{}}.
init([Type, Interface]) ->
    %% io:format("~w: init([~p,~p])~n", [?MODULE, Type, Interface]),
    {ok, #state{type = Type,
		interface = Interface,
		logFD = log_fd(Type),
		log_outgoing = log_outgoing(Type)}}.

%if CLI we defer running the portprogram until we get indication of 'shell'
%this is so that we will have received terminal geometry rows/cols which
%is set with 'pty'
handle_msg({ssh_channel_up, Id, Ref}, #state{type = Type} = S) ->
    %%erlang:display({?MODULE, "handle_msg ssh_channel_up", Id, Ref}),
    User = get_ssh_user(Ref),
    P_ip = get_ssh_peer(Ref),
    Roles = omc_server:authorize_user(User),
    InfoString = get_security_event_info_string(Type, Ref, "started"),
    C = omc_lib:add_session(ssh, Type),
    security_log_event(P_ip, S#state.interface, User, InfoString, C),
    omc_lib:add_session_info(Ref, "SSH", atom_to_list(Type), User, P_ip),
    omc_server:store_session(User, Type, S#state.interface, ssh, Roles, Ref),
    New_s = S#state{id = Id, ref = Ref, user = User, roles = Roles,
			peer_ip = P_ip},
    do_ssh_channel_up(Type, New_s);

handle_msg({flushed_data_to_user, Data}, S) ->
    case do_ssh_send(S, Data) of
	ok ->
	    {ok, S};
	{error, closed} ->
	    {stop, S#state.id, S}
    end;
handle_msg({Port, {data, Data}},
	S = #state{port = Port,
		ch_st = Ch_st,
		is_change_user_cap = true}) ->
    %%erlang:display({"message from port", Data}),
    case do_ssh_send(S, Data) of
	ok ->
	    New_ch_st = Ch_st#user_scan{last_from = Data},
	    {ok, S#state{ch_st = New_ch_st}};
	{error, closed} ->
	    {stop, S#state.id, S}
    end;
handle_msg({Port, {data, Data}}, S = #state{port = Port}) ->
    %%erlang:display({"message from port", Data}),
    case do_ssh_send(S, Data) of
	ok ->
	    {ok, S};
	{error, closed} ->
	    case process_info(self(), message_queue_len) of
		{message_queue_len, 0} ->
		    ok;
		{message_queue_len, Mlen} ->
		    sysInitI:warning_msg(
			"~p: SSH session closed during data output (client "
			"termination or network error).~n"
			"~b messages in the output buffer are discarded.~n",
			[?MODULE, Mlen])
	    end,
	    {stop, S#state.id, S}
    end;
handle_msg({Port, {exit_status, Exit}}, S = #state{port = Port}) ->
    %%erlang:display({?MODULE, "port exit", Exit}),
    if
        S#state.type =:= cli ->
            do_ssh_send(S, [<< "\r\n" >>]);
        true ->
            ok
    end,
    ssh_connection:exit_status(S#state.ref, S#state.id, Exit),
    % This is to work against "forgotten" connections clogging up the service
    % i.e. clients that doesn't close the connection at channel exit.
    % We try to enforce one channel per connection, thus this should be ok.
    % Mainly this would be useful when a timeout due to inactivity happens
    % unfortunately there is no way to differentiate between this and a normal
    % exit.
    % A potential problem is that the ssh client prints slightly differently
    % because of this...
    ssh:close(S#state.ref),
    {stop, S#state.id, S#state{port = undefined}};
handle_msg({'EXIT', Port, _What}, S = #state{port = Port}) ->
    ssh_connection:exit_status(S#state.ref, S#state.id, 0),
    {stop, S#state.id, S#state{port = undefined}};
handle_msg({'EXIT', _, normal}, #state{type = cli} = S) ->
    %this happens when we restart CLISS during ms_user_change
    {ok, S};
handle_msg({close_session, Message}, S) ->
    %can this happen _before_ the connection is "up"?
    case Message of
	[] ->
	    ok;
	Message ->
	    do_ssh_send(S, [Message, << "\r\n" >>])
    end,
    ssh_connection:exit_status(S#state.ref, S#state.id, 0),
    {stop, S#state.id, S};
handle_msg(Msg, S) ->
    erlang:display({?MODULE, "handle_msg callback", Msg}),
    {ok, S}.

%Codenomicon sends data before 'shell'
handle_ssh_msg({ssh_cm, _Ref, {data, _Id, _Type, _Data}},
		#state{port = undefined} = S) ->
    {stop, S#state.id, S};
handle_ssh_msg({ssh_cm, _Ref, {data, _Id, _Type, Data}},
		#state{type = Type, is_change_user_cap = true} = S) ->
    log_data(Data, S#state.logFD, true),
    if
	S#state.port =/= undefined ->
	    true = port_command(S#state.port, Data);
	true ->
	    true
    end,
    {User_scan, Is_user_cap, New_port, User, Roles} =
	omc_ms_user_change:ms_user_change(Type, "SSH",
					S#state.ch_st,
					Data,
					S#state.port,
					S#state.peer_ip,
					S#state.user,
					S#state.roles),
    Ms_user =
	if
   	    User =/= S#state.user ->
		S#state.user ++ ":" ++ User;
	    true ->
		undefined
	end,
    {ok, S#state{ch_st = User_scan,
		is_change_user_cap = Is_user_cap,
		port = New_port,
		ms_user = Ms_user,
		roles = Roles}};
handle_ssh_msg({ssh_cm, _Ref, {data, _Id, _Type, Data}}, S) ->
    log_data(Data, S#state.logFD, true),
    try  port_command(S#state.port, Data) of
	true ->
	    {ok, S}
    catch
	_:_ ->
	    sysInitI:warning_msg("~p: COM closed - closing SSH session.~n",
				 [?MODULE]),
	    {stop, S#state.id, S}
    end;
handle_ssh_msg({ssh_cm, _, {eof, _}}, S) ->
    %%erlang:display({?MODULE, {eof}}),
    {ok, S};
handle_ssh_msg({ssh_cm, Ref, {env, Id, WantReply, _Var, _Value}}, S) ->
    ok = ssh_connection:reply_request(Ref, WantReply, success, Id),
    {ok, S};
handle_ssh_msg({ssh_cm, Ref, {pty, Id, WantReply, Terminal_jox}}, S) ->
    %%io:format("~p pty: ~p~n", [?MODULE, Terminal_jox]),
    {Cols, Rows} = extract_winsize(Terminal_jox),
    ok = ssh_connection:reply_request(Ref, WantReply, success, Id),
    {ok, S#state{rows = Rows, cols = Cols}};
%this one is applicable only for CLI
handle_ssh_msg({ssh_cm, Ref, {shell, Id, WantReply}},
		#state{type = cli, user = User, roles = Roles} = S) ->
    %io:format("~p shell Ref: ~p Id: ~p~n", [?MODULE, Ref, Id]),
    {Change_user_cap, Change_state} =
	omc_ms_user_change:ms_user_change_capable(cli, Roles, mk_port_args(S)),
    Args = mk_port_args(S),
    Port = omc_lib:run_portprog(User, cli, Args),
    ok = ssh_connection:reply_request(Ref, WantReply, success, Id),
    {ok, S#state{port = Port, is_change_user_cap = Change_user_cap,
		 ch_st = Change_state}};
handle_ssh_msg({ssh_cm, _, {signal, _, _}}, S) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    {ok, S};
handle_ssh_msg({ssh_cm, _,
	{window_change, _Id, _Width, _Height, _Pixw, _PixH}}, S) ->
    {ok, S};
handle_ssh_msg({ssh_cm, Ref, {exec, Id, WantReply, _Cmd}}, S) ->
    ok = ssh_connection:reply_request(Ref, WantReply, success, Id),
    do_ssh_send(S, "interactive only, exec not supported\r\n"),
    {stop, S#state.id, S};
handle_ssh_msg({ssh_cm, _, {exit_signal, Id, _, _, _}},
	       S) ->
    {stop, Id,  S}.
%%handle_ssh_msg(_Msg, S) ->
%%erlang:display({?MODULE, "handle_ssh_msg callback", _Msg}),
%%{ok, S}.

terminate(_Why, #state{ms_user = undefined, user = User, type = Type,
		       ref = Ref} = S) ->
    InfoString = get_security_event_info_string(Type, Ref, "ended"),
    omc_lib:remove_session_info(Ref),
    C = omc_lib:remove_session(ssh, Type),
    security_log_event(S#state.peer_ip, S#state.interface, User, InfoString, C),
    close_log(S#state.logFD),
    %print_termination_info(),
    nop;
terminate(_Why, #state{ms_user = Ms_user, type = Type, ref = Ref} = S) ->
    InfoString = get_security_event_info_string(Type, Ref, "ended"),
    omc_lib:remove_session_info(Ref),
    C = omc_lib:remove_session(ssh, Type),
    security_log_event(S#state.peer_ip, S#state.interface, Ms_user, InfoString, C),
    close_log(S#state.logFD),
    %print_termination_info(),
    nop.



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
do_ssh_channel_up(cli, S) ->
    case sysEnv:rcs_mode_2() of
	simulated  ->
	    do_ssh_send(S,
			[<< "\r\nYou are accessing a simulated RBS\r\n\n" >>]);
	_ ->
	    ok
    end,
    {ok, S};
do_ssh_channel_up(netconf, #state{user = User, roles = Roles} = S) ->
    {Change_user_cap, Change_state} =
	omc_ms_user_change:ms_user_change_capable(netconf, Roles),
    Port =
	if
	    Change_user_cap ->
		undefined;
	    true ->
		omc_lib:run_portprog(User, netconf)
        end,

    {ok, S#state{port = Port,
		 is_change_user_cap = Change_user_cap,
		 ch_st = Change_state}}.

%return:    {Cols, Rows}
extract_winsize({_Terminal, 0, 0, _, _, _}) ->
   {?DEFAULT_COLS, ?DEFAULT_ROWS};
extract_winsize({_Terminal, 0, Rows, _, _, _}) ->
   {?DEFAULT_COLS, Rows};
extract_winsize({_Terminal, Cols, 0, _, _, _}) ->
   {Cols, ?DEFAULT_ROWS};
extract_winsize({_Terminal, Cols, Rows, _, _, _}) ->
   {Cols, Rows}.

mk_port_args(S) ->
    ["-c", integer_to_list(S#state.cols), "-r", integer_to_list(S#state.rows)].

security_log_event(IP_string, alt, User, Info_string, Count) ->
    Msg = lists:flatten(io_lib:format("SSH-alternate: User: ~s, ~s, session count ~p",
				      [User, Info_string, Count])),
    omc_lib:sec_log(IP_string, Msg);
security_log_event(IP_string, _, User, Info_string, Count) ->
    Msg = lists:flatten(io_lib:format("SSH: User: ~s, ~s, session count ~p",
				      [User, Info_string, Count])),
    omc_lib:sec_log(IP_string, Msg).

get_ssh_user(Ref) ->
    %this is the new API, which will(maybe it is now) be documented by OTP
    [{user, User}] = ssh:connection_info(Ref, [user]),
    User.

get_ssh_peer(Ref) ->
    case ssh:connection_info(Ref, [peer]) of
	[{peer,{_,{Ip_tuple,_}}}] ->
	    omc_lib:ip_to_string(Ip_tuple);
	_ -> "-"
    end.

do_ssh_send(S, Data) ->
    case ssh_connection:send(S#state.ref, S#state.id, Data) of
	ok ->
	    log_data(Data, S#state.logFD, S#state.log_outgoing),
	    ok;
	Err ->
	    Err
    end.

get_security_event_info_string(Type, Ref, End) ->
    RefS = pid_to_list(Ref),
    atom_to_list(Type) ++ " session " ++ RefS ++ " " ++ End.

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
%print_termination_info() ->
%    case process_info(self(), message_queue_len) of
%	{message_queue_len, 0} -> ok;
%	{message_queue_len, Mlen} ->
%	    sysInitI:warning_msg(
%		"~p: SSH session terminated with ~b messages pending, some "
%		"data may not have reached the client~n", [?MODULE, Mlen]),
%	    sysInitI:warning_msg("~p: Mbox content:~n~p~n",
%				     [?MODULE, process_info(self(), messages)])
%    end.


%dbg(_) -> ok.
%dbg(_, _) -> ok.
%dbg(Format) ->
%    dbg(Format, []).
%dbg(Format, Args) ->
%    io:format("dbg: ~p:" ++ Format ++ "~n", [?MODULE | Args]).
