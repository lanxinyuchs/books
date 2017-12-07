%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ecoli_ssh_channel.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R7A/R9A/R10A/1

%%% @doc ==COLI==
%%% TODO:
%%% timeouts when commands hang,
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(ecoli_ssh_channel).
-vsn('/main/R2A/R3A/R4A/R5A/R7A/R9A/R10A/1').
-date('2017-05-16').
-author('etxjotj').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% Rx         2013-07-08   etxlg     Copied and changed from orig coli
%%% R2A/2      2013-08-27   etxlg     Editing
%%% R2A/3      2013-09-01   etxlg     MOTD + minor misc.
%%% R2A/4      2013-09-04   etxlg     add init/0
%%% R2A/5      2013-09-05   etxlg     Lab usage
%%% R2A/7      2013-09-17   etxlg     Logging trail/security
%%% R2A/9      2013-10-09   etxlg     Better OTP SSH channels conformance
%%%				      added IP address of peer to log
%%% R2A/10     2013-11-06   etxlg     Anticipate change of userreg
%%% R2A/11     2014-01-22   etxlg     sys -> omc
%%% R2A/12     2014-01-29   etxlg     fixed try clause ssh_userreg
%%% R2A/13     2014-02-02   etxlg     no ~n in the security log
%%% R2A/14     2014-02-03   etxlg     Fixed dialyzer warning
%%% R2A/17     2014-04-23   etxlg     Fixed dialyzer warning
%%% R2A/18     2014-04-25   etxtory   New Welcome text introduced
%%% R2A/19     2014-08-18   etxlg     Include VC state in info -cmd
%%% R2A/20     2014-08-19   etxlg     use sec_log in ecoli_lib
%%% R2A/21     2014-09-10   etxlg     default timeout set to 15min
%%% R3A/1      2015-02-19   etxlg     Make RCS-COLI work with Maintenance User
%%% R3A/2      2015-03-19   etxlg     flowcontrol
%%% R3A/3      2015-05-12   etxlg     part of TR HT74324 (ssh-data before shell)
%%% R3A/4      2015-05-19   etxlg     ensure running coli exits at timeout/logout
%%% R3A/5      2015-05-25   etxlg     more of TR HT74324 (ssh send fails)
%%% R4A/1      2015-07-14   etxlg     roles_to_auth changed, NO_AUTH support
%%% R4A/2      2015-08-21   etxpejn   Added rpc:call for AuditTrailLog
%%% R4A/3      2015-08-31   etxlg     Alternate connection
%%% R4A/5      2015-09-15   uabesvi   error_logger -> sysInitI
%%% R4A/6      2015-09-25   etxpejn   Moved rpc:call to logI:write_log
%%% R5A/2      2016-02-25   uabesvi   call ssh:close at timeout
%%% R7A/1      2016-09-08   etxarnu   Added special case to filter out password
%%%                                   for /diagm/exportdump export
%%% R7A/2      2016-09-21   etxarnu   Filter password for /diagm/exportdump exportbb
%%% R7A/3      2016-10-17   etxpeno   Improve information in security and audit trail log
%%% R10A/1     2017-05-16   etxjotj   Don't show simulated message for cloud
%%% ----------------------------------------------------------
%%%
-behaviour(ssh_daemon_channel).
-define(WELCOME_MANDATORY,
	%1234567890123456789012345678901234567890123456789012345678901234567890 (80)
	"This is an interface for troubleshooting. Using the commands available\n"
	"through this interface can impact system performance. No actions\n"
	"should be performed by personnel without the necessary authorization\n"
	"and education.\n\n").


%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, terminate/2, handle_ssh_msg/2, handle_msg/2]).
-export([filter_pwd/1]). % used from ecoli_transport_adaptor
-include("ecoli.hrl").

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-record(state, {
        id,                     %ssh id
        ref,                    %ssh ref
	interface,		%alt | oam | lmt* | any
        user,                   %the current user
        roles,                  %list of roles for current user
        auth_level = 4,		%meaning "no valid role"
	exec_ref,
        dest = shell,
	last = undefined,
        timeout = (15 * 60 * 1000), %shell times out if inactive for 15min
        exit_reason,
        report_rc = false,
        currently_running,
        shell_state,
	src_ip,			%as string
	src_port		%as string
        }).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%this clause can be removed when OMC has been delivered
init([]) ->
    init([unknown]);
init([Interface]) ->
    ?LOG_RAM(?SEV_1, {"Started. Interface = ~p~n", [Interface]}),
    S = #state{interface = Interface},
    {ok, S, S#state.timeout}.

handle_msg({ssh_channel_up, Id, Ref}, S) ->
    User = get_ssh_user(Ref),
    Roles = omc_server:authorize_user(User),
    {Src_ip, Src_port} = extract_peer_info(Ref), %strings are returned

    {Auth_level, New_roles} = ecoli_lib:roles_to_auth(Roles),
    C = omc_lib:add_session(ssh_coli),
    omc_lib:add_session_info(Ref, "SSH", "coli", User, Src_ip),
    security_log_event(User, Ref, Src_ip, S#state.interface, "coli session started", C),
    %% store_session/4 can be removed after OMC is delivered
    try
	omc_server:store_session(User, coli, S#state.interface, ssh, Roles)
    catch
	error:undef ->
	    omc_server:store_session(User, coli, ssh, Roles)
    end,
    {ok, S#state{id = Id, ref = Ref, user = User, roles = New_roles,
		 auth_level = Auth_level, src_ip = Src_ip,
		 src_port = Src_port}, S#state.timeout};
handle_msg({sync_cmd_pipe, From, Ref, data, Data},
	   S = #state{exec_ref = Ref}) ->
    case respond(S, Data) of
	ok ->
	    From ! {sync_cmd_pipe, Ref, ack},
	    {ok, S, S#state.timeout};
	close ->
	    {stop, S#state.id, S}
    end;
handle_msg({sync_cmd_pipe, Ref, ack}, S = #state{exec_ref = Ref}) ->
    %do nothing - no flowcontrol in input direction for now
    {ok, S, S#state.timeout};
%whichever comes first message exit or real 'EXIT'
handle_msg({cmd_pipe, Ref, exit, Rc}, S = #state{exec_ref = Ref}) ->
    handle_pipe_exit(S, Rc);
%handle_msg({'EXIT', Pid, Reason}, S = #state{dest = Pid}) ->
%    handle_pipe_exit(S, Reason);
handle_msg({'EXIT', Pid, Reason}, S = #state{last = Pid}) ->
    %dbg({'EXIT', "last", Pid, Reason}),
    handle_pipe_exit(S, Reason);
handle_msg({internal_cmd, {set_report_rc, Bool}}, S) ->
    {ok, S#state{report_rc = Bool} , S#state.timeout};
handle_msg({internal_cmd, {set_timeout, infinity}}, S) ->
    {ok, S#state{timeout = infinity} , infinity};
handle_msg({internal_cmd, {set_timeout, Seconds}}, S) ->
    Timeout = 1000 * Seconds,
    {ok, S#state{timeout = Timeout} , Timeout};
handle_msg({internal_cmd, {set_auth_level, New_auth}},
	#state{shell_state = Shell_state, auth_level = Old_auth} = S) ->
    warn_msg("Authorization level changed by command, previous level: ~s, "
		"new level: ~s",
		[ecoli_lib:int_to_auth_string(Old_auth),
		ecoli_lib:int_to_auth_string(New_auth)]),
    New_shell_state = ecoli_cmd_shell:new_state(auth, Shell_state, New_auth),
    {ok, S#state{auth_level = New_auth,
		shell_state = New_shell_state}, S#state.timeout};
handle_msg({internal_cmd, {info, Ref, From}}, S) ->
    From ! {Ref, info_answer(S)},
    {ok, S, S#state.timeout};
handle_msg({internal_cmd, {set_prompt, New_prompt}},
		#state{shell_state = Shell_state} = S) ->
    New_shell_state = ecoli_cmd_shell:new_state(set_prompt, Shell_state,
						New_prompt),
    {ok, S#state{shell_state = New_shell_state}, S#state.timeout};
handle_msg({internal_cmd, {list_cmds, Ref, From, Args}}, S) ->
    From ! {Ref, command_list_answer(S, Args)},
    {ok, S, S#state.timeout};
handle_msg({internal_cmd, _What}, S) ->
    {ok, S , S#state.timeout};
handle_msg({'EXIT', _Pid, _Reason}, S) ->
    %dbg({'EXIT', _Pid, _Reason}),
    %there is a bunch of exits expected wheneve we run pipe with more than one
    %command, also if cmd_pipe,...,exit hits first we end up here
    {ok, S, S#state.timeout};
handle_msg(timeout, S)->
    ssh:close(S#state.ref),
    {stop, S#state.id, S#state{exit_reason = timeout}};
handle_msg(_Msg, S) ->
    warn_msg("Unknown message: ~p~n~p~n", [_Msg, S]),
    {ok, S, S#state.timeout}.

%during tests with Codenomicon it happens that data arrives before shell
%if so just stop
handle_ssh_msg({ssh_cm, _Ref, {data, _Id, _Type, _Data}},
	       S = #state{shell_state = undefined}) ->
    {stop, S#state.id, S};
%this function is where characters for the coli shell arrives
handle_ssh_msg({ssh_cm, _Ref, {data, _Id, _Type, Data}},
	 S = #state{dest = shell}) ->

    {Action, New_shell_state, Output_iolist, Execute_cmd} =
	ecoli_cmd_shell:parse_input(binary_to_list(Data), S#state.shell_state),

    New_state=S#state{shell_state = New_shell_state},

    case Action of
	execute ->
	    Return = execute_commands(New_state, Execute_cmd),
	    case respond(New_state, Output_iolist) of %a single newline
		ok ->
		    Return;
		close ->
		    {stop, New_state#state.id, New_state}
	    end;
        ok ->
	    case respond(New_state, Output_iolist) of
		ok ->
		    {ok, New_state, New_state#state.timeout};
		close ->
		    {stop, New_state#state.id, New_state}
	    end;
	stop ->
	    respond(New_state, Output_iolist),
            {stop, New_state#state.id, New_state}
    end;
%pipe is running divert incoming characters to it
%HERE disabled
%handle_ssh_msg({ssh_cm, _Ref, {data, _Id, _Type, <<?ETX>>}},
%    S = #state{dest = Dest, exec_ref = Exec_ref}) ->
%    Dest ! {cmd_pipe, Exec_ref, exit, 'ETX'},
%    {ok, S, S#state.timeout};
handle_ssh_msg({ssh_cm, _Ref, {data, _Id, _Type, <<?EOT>>}},
    S = #state{dest = Dest, exec_ref = Exec_ref}) -> %Fakery
    %Dest ! {cmd_pipe, Exec_ref, data, ?DEFAULT_EOF},
    Dest ! {cmd_pipe, Exec_ref, exit, "dummy"},
    {ok, S, S#state.timeout};
handle_ssh_msg({ssh_cm, _Ref, {data, _Id, _Type, Data}},
    S = #state{dest = Dest, exec_ref = Exec_ref}) ->
    Dest ! {sync_cmd_pipe, self(), Exec_ref, data, Data},
    {ok, S, S#state.timeout};
handle_ssh_msg({ssh_cm, _, {eof, _}}, S) -> %should we exit here? YES
    %this is where we end up if client does "echo command | ssh -l user -p port"
    %OTP example just returns {ok, S} here
    {stop, S#state.id, S};
handle_ssh_msg({ssh_cm, Ref, {env, Id, WantReply, _Var, _Value}}, S) ->
    ok = ssh_connection:reply_request(Ref, WantReply, success, Id),
    {ok, S, S#state.timeout};
handle_ssh_msg({ssh_cm, Ref, {pty, Id, WantReply, _Terminaljox}}, S) ->
    ok = ssh_connection:reply_request(Ref, WantReply, success, Id),
    {ok, S, S#state.timeout};
handle_ssh_msg({ssh_cm, _Ref,
		{window_change, _Id, _Width, _Height, _Pixw, _PixH}}, S) ->
    {ok, S, S#state.timeout};
handle_ssh_msg({ssh_cm, _Ref, {signal, _, _}}, S) ->
    {ok, S, S#state.timeout};
handle_ssh_msg({ssh_cm, Ref, {shell, Id, WantReply}}, S) ->
    ok = ssh_connection:reply_request(Ref, WantReply, success, Id),
    {Shell_state, Prompt}  = ecoli_cmd_shell:init(S#state.auth_level),
    Welcome = comsaI:get_cli_welcome_message(),
    case sysEnv:rcs_mode_2() of
	simulated ->
	    case respond(Ref, Id,
			 ["\nYou are accessing a simulated RBS\n\n",
			 ?WELCOME_MANDATORY, Welcome, Prompt]) of
		ok ->
		    {ok, S#state{shell_state = Shell_state}, S#state.timeout};
		close ->
		    {stop, S#state.id, S}
	    end;
	Mode when Mode==target; Mode==vrcs ->
	    case respond(Ref, Id, ["\n" ?WELCOME_MANDATORY, Welcome, Prompt]) of
		ok ->
		    {ok, S#state{shell_state = Shell_state}, S#state.timeout};
		close ->
		    {stop, S#state.id, S}
	    end
    end;
handle_ssh_msg({ssh_cm, Ref, {exec, Id, WantReply, _Cmd}}, S) ->
    ok = ssh_connection:reply_request(Ref, WantReply, success, Id),
    respond(S, "\ninteractive only, exec not supported\n\n"),
    {stop, S#state.id, S};
handle_ssh_msg({ssh_cm, _Ref, {exit_signal, Id, _, _Error, _}}, S) ->
    {stop, Id, S};
handle_ssh_msg({ssh_cm, _Ref, {exit_status, Id, _Status}}, S) ->
    {stop, Id, S}.

terminate(_Why, #state{exit_reason = auth_fail}) ->
    nop;
terminate(_Why, S = #state{exit_reason = timeout}) ->
    ensure_exit(S#state.dest),
    command_log_event(S),
    omc_lib:remove_session_info(S#state.ref),
    C = omc_lib:remove_session(ssh_coli),    
    security_log_event(S#state.user, S#state.ref, S#state.src_ip,
		       S#state.interface, "coli session timed out", C),
    nop;
terminate(Reason, S) ->
    ?LOG_RAM(?SEV_1, {"Terminate. Reason =~p~n", [Reason]}),
    ensure_exit(S#state.dest),
    command_log_event(S),
    omc_lib:remove_session_info(S#state.ref),
    C = omc_lib:remove_session(ssh_coli),    
    security_log_event(S#state.user, S#state.ref, S#state.src_ip,
		       S#state.interface, "coli session ended", C),
    nop.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
ensure_exit(shell) -> ok;
ensure_exit(Pid) ->
    exit(Pid, exit). %anything but 'normal' will do the trick

handle_pipe_exit(S, Rc) ->
    if
        S#state.report_rc ->
            respond(S, io_lib:format("RC: ~p\n", [Rc]));
        true ->
            ok
    end,
    %command_log_event(S#state.user, S#state.currently_running, Rc),
    command_log_event(S, Rc),
    Prompt = ecoli_cmd_shell:get_prompt(S#state.shell_state),
    respond(S, Prompt),
    {ok, S#state{dest = shell, exec_ref = undefined, last = undefined,
		currently_running = undefined}, S#state.timeout}.

execute_commands(S, Execute_cmd) ->
    % if there should happen to be any internal cmd in a pipe we execute it
    % and throw the rest of the cmd-pipe to the wind
    Exec_ref = make_ref(),
    {First_pid, Last_pid} =
	ecoli_exec:exec_cmd_pipe(self(), Exec_ref, S#state.auth_level,
				 Execute_cmd),
    {ok, S#state{exec_ref = Exec_ref, dest = First_pid, last = Last_pid,
		currently_running = Execute_cmd} , S#state.timeout}.

respond(#state{ref = Ref, id = Id}, IOdata) ->
    respond(Ref, Id, IOdata).
%ssh breakage if sending an empty binary it hangs
respond(_Ref, _Id, <<>>) ->
    ok;
respond(Ref, Id, IOdata) ->
    Out = re:replace(IOdata, "\\R", "\r\n", [{return, binary}, global]),
    % possibly a failure to send should result in {stop, Id, State}
    % to be fixed...
    case ssh_connection:send(Ref, Id, Out) of
	ok ->
	    ok;
	_ ->
	    warn_msg("The SSH connection closed unexpectedly.",[]),
	    close
    end.

command_log_event(#state{currently_running = undefined}) ->
    %nothing appears to be executing at time of exit - good
    ok;
command_log_event(S) ->
    %termination before an external command pipe exited - not suppose to happen
    command_log_event(S, "still running").

command_log_event(#state{user = User, currently_running = Cmd_list, ref = Ref} = S, Rc) ->
    TS  = os:timestamp(),
    Cmd_string = filter_pwd(build_cmd_string(lists:reverse(Cmd_list))),
    Msg = lists:flatten(io_lib:format("User: ~s Ref: ~p Cmd: ~s Rc: ~p",
					[User, Ref, Cmd_string, Rc])),
    logI:write_log("AuditTrailLog", S#state.src_ip, "COLI",
		   13, info, TS, Msg).

%% This is a special fix to remove password from AuditTrailLog for /diagm/exportdump
filter_pwd(String) ->
    case string:str(String,"exportdump") of
	0 ->
	    String;
	_ ->
	    case string:tokens(String," ") of
		[Cmd, SubC, Url, _Pwd |Rest] when
		      SubC == "export";
		      SubC == "exportbb"
		      ->
		    [X ++ " " || X <- [Cmd, SubC, Url, "***" |Rest] ];
		_ ->
		    String
	    end
    end.



%fairly inefficient, lets hope for short commands
build_cmd_string([#cmd{name = N, path = P, args = A}]) ->
    ecoli_lib:join(P, N) ++ build_arg_string(A);
build_cmd_string([#cmd{name = N, path = P, args = A} | Cmd_list]) ->
    ecoli_lib:join(P, N) ++ build_arg_string(A) ++
	[$| | build_cmd_string(Cmd_list)].

build_arg_string([]) ->
    [];
build_arg_string([A]) ->
    [$\s | A];
build_arg_string([A | More]) ->
    [$\s | A] ++ build_arg_string(More).

info_answer(S) ->
    lists:flatten(
	io_lib:format("User: ~s~nRoles: ~p~nAuthorization level: ~s~n"
		"Source IP address: ~p~nShell idle timeout(ms): ~p~n"
		"Report RC: ~p~nHistory depth: ~p~nVC installed: ~p~n",
		[S#state.user, S#state.roles,
		ecoli_lib:int_to_auth_string(S#state.auth_level),
		S#state.src_ip, S#state.timeout, S#state.report_rc,
		?HISTORYLENGTH, is_secure()])).

is_secure() ->
    try sysInitServer:is_secure()
    catch
	error:undef -> false %%workaround pending SYS release
    end.

command_list_answer(#state{auth_level = Auth}, _Args) ->
    Cmds = ecoli_datainit:all_cmds(Auth) ++
	ecoli_datainit:cmds_at_path(?INTERNAL_DIR, Auth),
    lists:sort([[ecoli_lib:join(D, F), "\n"] || {D, F} <- Cmds]).

warn_msg(Fmt, Data) ->
    Format = "~p: " ++  Fmt ++ "~n",
    sysInitI:warning_msg(Format, [?MODULE | Data]).

security_log_event(User, Ref, SrcIp, alt, Info_string, C) ->
    Msg = lists:flatten(io_lib:format("COLI-alternate, User: ~s, Ref: ~p, ~s, session count ~p",
            [User, Ref, Info_string, C])),
    ecoli_lib:sec_log(SrcIp, Msg);
security_log_event(User, Ref, SrcIp, _, Info_string, C) ->
    Msg = lists:flatten(io_lib:format("COLI, User: ~s, Ref: ~p, ~s, session count ~p",
            [User, Ref, Info_string, C])),
    ecoli_lib:sec_log(SrcIp, Msg).

extract_peer_info(Ref) ->
    try epi(ssh:connection_info(Ref, [peer])) of
	Values -> Values
    catch
	_A:_B ->
	    erlang:display({?MODULE, _A,_B}),
	    {"-", "-"}
    end.
epi([{peer,{undefined,{{0,0,0,0,0,65535, A, B}, Port}}}]) ->
    <<I1:8, I2:8, I3:8, I4:8>> = <<A:16, B:16>>,
    {lists:flatten(io_lib:format("~b.~b.~b.~b", [I1, I2, I3, I4])),
	integer_to_list(Port)};
epi([{peer,{_,{Ip, Port}}}]) ->
    {inet:ntoa(Ip), integer_to_list(Port)}.

get_ssh_user(Ref) ->
    [{user, User}] = ssh:connection_info(Ref, [user]),
    User.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
%dbg(Term) -> erlang:display({?MODULE, self(), Term}).
