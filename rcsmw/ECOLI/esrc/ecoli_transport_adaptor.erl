%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ecoli_transport_adaptor.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R7A/R10A/1

%%% @doc ==COLI==
%%% This is mostly a copy of ecoli_ssh_channel, when there is time
%%% the two should be joined (or something). However, this is not done
%%% now since there is little chance this works first time around and
%%% it seems like a bad idea to break the existing stuff the day before
%%% I go on vacation.
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
%-module(ecoli_ssh_channel).
-module(ecoli_transport_adaptor).
-vsn('/main/R2A/R3A/R4A/R7A/R10A/1').
-date('2017-05-16').
-author('etxjotj').
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
%%% Rx         2014-08-08   etxlg     Created
%%% R2A/2      2014-07-09 etxberb     Fixed dialyzer warning.
%%% R2A/3      2014-08-18    etxlg    Include VC state in info -cmd
%%% R2A/4      2014-08-19    etxlg    use sec_log in ecoli_lib
%%% R2A/5      2014-09-10    etxlg    default timeout set to 15min
%%% R3A/1      2015-03-27    etxlg    New sync-protocoll towards ECOLI
%%% R3A/2      2015-05-19    etxlg    Ensure running program ends
%%% R4A/1      2015-07-14    etxlg    Moved roles_to_auth
%%% R4A/2      2015-08-21    etxpejn  Added rpc:call for AuditTrailLog
%%% R4A/3      2015-09-15    uabesvi  error_logger -> sysInitI
%%% R4A/4      2015-09-25    etxpejn  Moved rpc:call to logI:write_log
%%% R7A/1      2016-09-08    etxarnu  Added special case to filter out password
%%%                                   for /diagm/exportdump
%%% R10A/1     2017-05-16    etxjotj  Don't show simulated message for cloud
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
-export([init/0, init/1, terminate/2, handle_ssh_msg/2, handle_msg/2]).
-export([init/4]).
-include("ecoli.hrl").

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-record(state, {
	pid,
        id,                     %ssh id
        ref,                    %ssh ref
        user,                   %the current user
        roles,                  %list of roles for current user
        auth_level = 4,		%meaning "no valid role"
	exec_ref,
        dest = shell,
	last = undefined,
        timeout = (15 * 60 * 1000),
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

%this is called from OMC (TLS stuff)
init(Pid, Src_ip, User, Roles) ->
    {Auth_level, New_roles} = ecoli_lib:roles_to_auth(Roles),
    {Shell_state, Prompt}  = ecoli_cmd_shell:init(Auth_level),
    Welcome = comsaI:get_cli_welcome_message(),
    case sysEnv:rcs_mode_2() of
	simulated ->
	    respond(Pid, ["\nYou are accessing a simulated RBS\n\n", 
			  ?WELCOME_MANDATORY, Welcome, Prompt]);
	Mode when Mode==target; Mode==vrcs ->
	    respond(Pid, ["\n" ?WELCOME_MANDATORY, Welcome, Prompt])
    end,
    S =  #state{pid = Pid,
		user = User,
		roles = New_roles,
		auth_level = Auth_level,
		src_ip = Src_ip,
		shell_state = Shell_state},
    {ok, S, S#state.timeout}.

init(_) ->
    init().

init() ->
    S = #state{},
    {ok, S, S#state.timeout}.

%not called when we do TLS
% handle_msg({ssh_channel_up, Id, Ref}, S) ->
%     User = get_ssh_user(Ref),
%     Roles = omc_server:authorize_user(User),
%     {Src_ip, Src_port} = extract_peer_info(Ref), %strings are returned
% 
%     case roles_to_auth(Roles) of
% 	nok->
%         security_log_event(User, Src_ip, "coli session authorization failure"),
%             {stop, Id, S#state{exit_reason = auth_fail}};
%         Auth_level ->
%             security_log_event(User, Src_ip, "coli session started"),
%             {ok, S#state{id = Id, ref = Ref, user = User, roles = Roles,
% 			auth_level = Auth_level, src_ip = Src_ip,
% 			src_port = Src_port}, S#state.timeout}
%     end;
% %handle_msg({cmd_pipe, Ref, data, Data}, S = #state{exec_ref = Ref}) ->
%    respond(S, Data),
%    {ok, S, S#state.timeout};
handle_msg({sync_cmd_pipe, From, Ref, data, Data},
	   S = #state{exec_ref = Ref}) ->
    respond(S, Data),
        From ! {sync_cmd_pipe, Ref, ack},
    {ok, S, S#state.timeout};
handle_msg({sync_cmd_pipe, Ref, ack}, S = #state{exec_ref = Ref}) ->
    %do nothing - no flowcontrol in input direction for now
    {ok, S, S#state.timeout};
%whichever comes first message exit or real 'EXIT'
handle_msg({cmd_pipe, Ref, exit, Rc}, S = #state{exec_ref = Ref}) ->
    handle_pipe_exit(S, Rc);
handle_msg({'EXIT', Pid, Reason}, S = #state{last = Pid}) ->
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
    %there is a bunch of exits expected wheneve we run pipe with more than one command
    %also if cmd_pipe,...,exit hits first we end up here
    {ok, S, S#state.timeout};
handle_msg(timeout, S)->
    {stop, S#state.id, S#state{exit_reason = timeout}};
handle_msg(_Msg, S) ->
    warn_msg("Unknown message: ~p~n", [_Msg]),
    {ok, S, S#state.timeout}.

%this function is where characters for the coli shell arrives
handle_ssh_msg({ssh_cm, _Ref, {data, _Id, _Type, Data}},
	 S = #state{dest = shell}) ->

    {Action, New_shell_state, Output_iolist, Execute_cmd} =
	ecoli_cmd_shell:parse_input(binary_to_list(Data), S#state.shell_state),

    New_state=S#state{shell_state = New_shell_state},
	
    case Action of
	execute ->
	    Return = execute_commands(New_state, Execute_cmd),
	    respond(New_state, Output_iolist), %a single newline
	    Return;
        ok ->
	    respond(New_state, Output_iolist),
            {ok, New_state, New_state#state.timeout};
	stop ->
	    respond(New_state, Output_iolist),
            {stop, New_state#state.id, New_state}
    end;
%pipe is running divert incoming characters to it
%handle_ssh_msg({ssh_cm, _Ref, {data, _Id, _Type, <<?ETX>>}},
%    S = #state{dest = Dest, exec_ref = Exec_ref}) ->
%    Dest ! {cmd_pipe, Exec_ref, exit, 'ETX'},
%    {ok, S, S#state.timeout};
handle_ssh_msg({ssh_cm, _Ref, {data, _Id, _Type, <<?EOT>>}},
    S = #state{dest = Dest, exec_ref = Exec_ref}) -> %Fakery
    Dest ! {cmd_pipe, Exec_ref, exit, "dummy"},
    {ok, S, S#state.timeout};
handle_ssh_msg({ssh_cm, _Ref, {data, _Id, _Type, Data}},
    S = #state{dest = Dest, exec_ref = Exec_ref}) ->
    %Dest ! {cmd_pipe, Exec_ref, data, Data},
    Dest !  {sync_cmd_pipe, self(), Exec_ref, data, Data},
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
	    respond(Ref, Id,
		["\nYou are accessing a simulated RBS\n\n", 
		 ?WELCOME_MANDATORY, Welcome, Prompt]);
	Mode when Mode==target; Mode==vrcs ->
	    respond(Ref, Id, ["\n" ?WELCOME_MANDATORY, Welcome, Prompt])
    end,
    {ok, S#state{shell_state = Shell_state}, S#state.timeout};
handle_ssh_msg({ssh_cm, Ref, {exec, Id, WantReply, _Cmd}}, S) ->
    ok = ssh_connection:reply_request(Ref, WantReply, success, Id),
    respond(S, "\ninteractive only, exec not supported\n\n"),
    {stop, S#state.id, S};
handle_ssh_msg({ssh_cm, _Ref, {exit_signal, Id, _, _Error, _}}, S) ->
    {stop, Id, S};
handle_ssh_msg({ssh_cm, _Ref, {exit_status, Id, _Status}}, S) ->
    {stop, Id, S}.

%terminate(_Why, #state{exit_reason = auth_fail}) ->
%    nop;
%terminate(_Why, S = #state{exit_reason = timeout}) ->
%    command_log_event(S),
%    security_log_event(S#state.user, S#state.src_ip, "coli session timed out"),
%    nop;
terminate(_Why, S) ->
    ensure_exit(S#state.dest),
    command_log_event(S),
    %security_log_event(S#state.user, S#state.src_ip, "coli session ended"),
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
	ecoli_exec:exec_cmd_pipe(self(), Exec_ref,
				 S#state.auth_level, Execute_cmd), 
    {ok, S#state{exec_ref = Exec_ref, dest = First_pid, last = Last_pid,
		currently_running = Execute_cmd} , S#state.timeout}.

%respond(#state{ref = Ref, id = Id}, IOdata) ->
%    respond(Ref, Id, IOdata);
respond(#state{pid = Pid}, Iodata) ->
    respond(Pid, Iodata);
respond(Pid, Iodata) when is_pid(Pid) ->
    Out = re:replace(Iodata, "\\R", "\r\n", [{return, binary}, global]),
    Pid ! {coli_shell, {data, Out}}.

%ssh breakage if sending an empty binary it hangs
respond(Ref, Id, IOdata) when IOdata =/= <<>> ->
    Out = re:replace(IOdata, "\\R", "\r\n", [{return, binary}, global]),
    ok = ssh_connection:send(Ref, Id, Out);
respond(_, _, _) ->
    ok.

command_log_event(#state{currently_running = undefined}) ->
    %nothing appears to be executing at time of exit - good
    ok;
command_log_event(S) ->
    %termination before an external command pipe exited - not suppose to happen
    command_log_event(S, "still running").

command_log_event(#state{user = User, currently_running = Cmd_list} = S, Rc) ->
    TS  = os:timestamp(),
    Cmd_string = ecoli_ssh_channel:filter_pwd(
		   build_cmd_string(lists:reverse(Cmd_list))),
    Msg = lists:flatten(io_lib:format("User: ~s Cmd: ~s Rc: ~p",
					[User, Cmd_string, Rc])),
    logI:write_log("AuditTrailLog", S#state.src_ip, "COLI",
		   13, info, TS, Msg).

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

%security_log_event(User, SrcIp, Info_string) ->
%    Msg = lists:flatten(io_lib:format("COLI, User: ~s, ~s",
%            [User, Info_string])),
%    ecoli_lib:sec_log(SrcIp, Msg).

%extract_peer_info(Ref) ->
%    try epi(ssh:connection_info(Ref, [peer])) of
%	Values -> Values
%    catch
%	_A:_B ->
%	    erlang:display({_A,_B}),
%	    {"-", "-"}
%    end.
%epi([{peer,{undefined,{{0,0,0,0,0,65535, A, B}, Port}}}]) ->
%    <<I1:8, I2:8, I3:8, I4:8>> = <<A:16, B:16>>,
%    {lists:flatten(io_lib:format("~b.~b.~b.~b", [I1, I2, I3, I4])),
%	integer_to_list(Port)};
%epi([{peer,{_,{Ip, Port}}}]) ->
%    {inet:ntoa(Ip), integer_to_list(Port)}.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
%get_ssh_user(Ref) ->
%    [{user, User}] = ssh:connection_info(Ref, [user]),
%    User.
%get_ssh_user(Ref) ->
%    %not documented in OTP, will be removed in later OTP release
%    %this tries to anticipate that.
%    try ssh_userreg:lookup_user(Ref) of
%        {ok, User} -> User
%    catch
%        error:undef ->
%            %this is the new API, which will be documented
%            [{user, User}] = ssh:connection_info(Ref, [user]),
%            User
%   end.

