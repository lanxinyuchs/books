%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ecoli_exec.erl %
%%% Author:     etxbjca
%%% Description:
%%  Protocol towards the ssh_channel callback,
%%  or between processes running in a pipe.
%%  {cmd_pip, Ref::ref(), data, Characters::list()}
%%  {cmd_pip, Ref::ref(), exit, Return_code::integer()}
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(ecoli_exec).
%-behaviour(behaviour).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R11A/R12A/2').
-date('2017-10-25').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% R2A/1      2013-07-12   etxlg     Created
%%% R2A/2      2013-08-27   etxlg     Support type: legacy + a lot of fixing
%%% R2A/3      2013-09-02   etxlg     Added a printout
%%% R2A/4      2013-09-04   etxlg     Locate colish on target
%%% R2A/5      2013-09-05   etxlg     Fix above, wildcarding for external cmd
%%% R2A/6      2013-10-01   etxlg     Quote arguments to colish
%%% R2A/7      2013-10-10   etxlg     Propagate exit from erts-process
%%% R2A/8      2013-10-18   etxarnu   Made it possible to patch external cmds
%%% R2A/9      2014-02-24   etxlg     New method for legacy coli evaluation
%%% R3A/1      2014-12-03   etxlg     environemnt RCS_COLI=true
%%% R3A/3      2015-03-19   etxlg     new port-proxy program, flowcontrol
%%% R3A/4      2015-03-30   etxlg     bug fix for legacy-commands
%%% R4A/10     2015-09-15   uabesvi   error_logger -> sysInitI
%%% R6A/3      2016-06-22   etxarnu   Corrected colish_cmd_dir for vrcs
%%% ----------------------------------------------------------
%%%
-include("ecoli.hrl").

%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([exec_cmd_pipe/4]).
-export([error_msg/1]).

-define(CMD_PRINT, [cli_pname,
		    cli_scope,
		    cli_type,
		    type,
		    authorization,
		    relpath,
		    filepath,
		    subcommand,
		    module,
		    function,
		    args]).

-define(TIMEOUT, 30000).

%%=====================================================================
%% ToPid:      the pid of process running ssh_channel
%% DestRef:    a reference valid within this command pipe
%% Auth:       current authorization (integer 1-3 (sometimes:  0-4))
%% Commands:   a list of #cmd{}
%%             the commands in Commands are in reverse order, the process 
%%             at the end of the pipe is started first.
%% NO Returns: pid of the first started process, the one at the "end of pipe"
%% Returns:    pid of the FIRST and LAST started process
%%=====================================================================
exec_cmd_pipe(ToPid, DestRef, Auth, Commands) ->
    exec_cmd_pipe(ToPid, undefined, ToPid, DestRef, Auth, Commands).

exec_cmd_pipe(_, Last_pid, FristPid, _,  _, []) ->
    %% dbg({"FristPid", FristPid, "Last_pid", Last_pid}),
    {FristPid, Last_pid};
exec_cmd_pipe(ShellPid,
	      FristPid,
	      ToPid, 
	      DestRef, 
	      Auth, 
	      [#cmd{name = CliName,
		    path = CliPath,
		    args = Args,
		    remote_node = Remote} | MoreCmd]) ->
    Cmd = ecoli_datainit:lookup({CliPath, CliName}, Auth),
    Pid = exec_cmd(ShellPid, ToPid, DestRef, Cmd, Args, Remote),
    %% dbg({exec, Pid}),
    case FristPid of
	undefined ->
	    exec_cmd_pipe(ShellPid, Pid, Pid, DestRef, Auth, MoreCmd);
	_ ->
	    exec_cmd_pipe(ShellPid, FristPid, Pid, DestRef, Auth, MoreCmd)
    end.


%%=====================================================================
%% ShellPid: top level pid, i.e. the one inside ssh that runs the callback
%% ToPid:    the pid running after in the pipe or the ssh_channel itself for a
%%           process running by itself or running last in a pipe
%% Ref:      reference() valid within the currently running cmd-pipe
%% Cmd:      #cmd{}
%% Args:     list() of arguments to Cmd
%% Returns:  pid()
%%=====================================================================
exec_cmd(ShellPid, ToPid, Ref, [], _, _) ->
    spawn_link(fun() -> exec_not_supported(ShellPid, 
					   Ref,
					   ToPid,
					   "internal error: no such command\n",
					   "")
	       end);
exec_cmd(ShellPid, ToPid, Ref, Cmd, Args, Remote) ->
    ?LOG_RAM(?SEV_1, {"Exec cmd.~n"
		      "  Cmd    = ~n~s~n" 
		      "  Args   = ~p~n"
		      "  Remote = ~p~n"
		      "  Ref    = ~p~n", 
		      [pp(Cmd), Args, Remote, Ref]}),
    Fun = exec_cmd_fun(Cmd#coli_cmd.type),
    Fun(ShellPid, ToPid, Ref, Cmd, Args, Remote, ?TIMEOUT). %% HERE fix timeout


exec_cmd_fun(rootfs) ->
    fun exec_cmd_rootfs/7;
exec_cmd_fun(external) ->
    fun exec_cmd_external/7;
exec_cmd_fun(legacy) ->
    fun exec_cmd_legacy/7;
exec_cmd_fun(erts) ->
    fun exec_cmd_erts/7;
exec_cmd_fun(coli_erts) ->
    fun exec_cmd_coli_erts/7;
exec_cmd_fun(fruacc) ->
    fun exec_cmd_fruacc/7.



%%=====================================================================
%% ROOT FS
%%=====================================================================
exec_cmd_rootfs(_ShellPid, ToPid, Ref, Cmd, Args, Node, Timeout) -> 
    spawn_link(get_node(Node),
	       fun() -> exec_port(Ref, 
				  ToPid,
				  Cmd#coli_cmd.filepath,
				  Cmd#coli_cmd.args ++ Args,
				  Timeout) 
	       end).

%%=====================================================================
%% EXTERNAL
%%=====================================================================
exec_cmd_external(ShellPid, ToPid, Ref, Cmd, Args, Node, Timeout) -> 
    
    Joined = filename:join([Cmd#coli_cmd.cxp_path,
			    Cmd#coli_cmd.relpath]),
    case filelib:wildcard(Joined) of 
	[ExecutableT] ->
	    Executable = swmI:find_file(ExecutableT), %check patched
	    spawn_link(get_node(Node), 
		       fun() -> exec_port(Ref,
					  ToPid, 
					  Executable,
					  Cmd#coli_cmd.args ++ Args, 
					  Timeout)
		       end);
	Matched ->
	    spawn_link(fun() -> exec_not_supported(ShellPid,
						   Ref, 
						   ToPid,
						   ece_error(Matched),
						   Joined) 
		       end)
    end.

ece_error([]) ->
    "Command binary not found\n";
ece_error(_) ->
    "Multiple command binaries matching\n".



%%=====================================================================
%% LEGACY
%%=====================================================================
exec_cmd_legacy(_ShellPid, ToPid, Ref, Cmd, Args, Node, Timeout) ->
    spawn_link(get_node(Node), 
	       fun() -> exec_port_legacy(Ref, 
					 ToPid,
					 Cmd#coli_cmd.subcommand,
					 Args, 
					 Timeout)
	       end).

%%=====================================================================
%% ERTS
%%=====================================================================
exec_cmd_erts(ShellPid, ToPid, Ref, Cmd, Args, Remote, Timeout) ->
    spawn_link(fun() -> exec_erts(ShellPid,
				  Ref, 
				  ToPid,
				  Cmd#coli_cmd.module,
				  Cmd#coli_cmd.function, 
				  Cmd#coli_cmd.args  ++ Args, 
				  false,
				  Remote,
				  Timeout) 
	       end).

%%=====================================================================
%% COLI ERTS
%%=====================================================================
exec_cmd_coli_erts(ShellPid, ToPid, Ref, Cmd, Args, Remote, Timeout) ->
    spawn_link(fun() -> exec_erts(ShellPid, 
				  Ref, 
				  ToPid,
				  Cmd#coli_cmd.module,
				  Cmd#coli_cmd.function, 
				  Cmd#coli_cmd.args  ++ Args, 
				  true, 
				  Remote,
				  Timeout) 
	       end).

%%=====================================================================
%% FRUACC
%%=====================================================================
exec_cmd_fruacc(ShellPid, ToPid, Ref, Cmd, Args, Remote, Timeout) ->
%%     io:format("#### EXEC  fruacc ~n"
%% 	      "ShellPid = ~p~n" 
%% 	      "ToPid    = ~p~n" 
%% 	      "Ref      = ~p~n" 
%% 	      "Cmd      = ~p~n" 
%% 	      "Args     = ~p~n" 
%% 	      "Remote   = ~p~n" 
%% 	      "Timeout  = ~p~n",
%% 	      [ShellPid, ToPid, Ref, Cmd, Args, Remote, Timeout]),
%%    [FruId | _] = string:tokens(atom_to_list(Remote), "@"),
    spawn_link(fun() -> exec_fruacc(Remote, 
				    Cmd, 
				    Args, 
				    ShellPid, 
				    Ref, 
				    ToPid, 
				    Timeout) 
	       end).



%%=====================================================================
%% Not supported
%%=====================================================================
exec_not_supported(ShellPid, Ref, Output, Reason, Info) ->
    ?LOG_RAM(?SEV_WARNING, {"Exec not supported. ~n"
			    "  Reason = ~p~n"
			    "  Info = ~p~n",
			    [Reason, Info]}),
    exec_erts(ShellPid, 
	      Ref, 
	      Output, 
	      ?MODULE, 
	      error_msg, 
	      [Reason],
	      false, 
	      local,
	      10000).

%%=====================================================================
%% 
%%=====================================================================
exec_port_legacy(Ref, Output, Cmd, Args, _Timeout) ->
    CM = hd(Cmd),
    AR = tl(Cmd) ++ Args,
    Proxy     = find_eof_proxy(),
    ColishDir = find_colish_cmd_dir(),
    case file:read_file_info(filename:join([ColishDir, CM])) of
	{ok, _} -> %% should we check for exec?
	    put(ecoli_process, exec_port),
	    %% Port = run_port_prog(Eof_proxy, ColishDir, CM, AR),
	    %% exec_port_loop(Ref, Output,Port);
	    {Port, Sock, ClientPort} =
		new_run_port_prog(Proxy, ColishDir, CM, AR),
		%% new_run_port_prog(Proxy, ColishDir, Cmd, Args),
	    proxy_chunk(Sock, ClientPort),
	    exec_port_loop(Ref, 
			   Output, 
			   Port, 
			   Sock, 
			   ClientPort, 
			   undefined, 
			   undefined);
	{error, What} ->
	    Output ! {cmd_pipe, Ref, exit, ["File error:"++ What]}
    end.

%%=====================================================================
%% 
%%=====================================================================
exec_port(Ref, Output, Cmd, Args, _Timeout) ->
    Proxy = find_eof_proxy(),
    case file:read_file_info(Cmd) of
	{ok, _} -> %% should we check for exec?
	    put(ecoli_process, exec_port),
	    %% Port = run_port_prog(Eof_proxy, "/tmp/", Cmd, Args),
	    %% exec_port_loop(Ref, Output,Port);
	    {Port, Sock, ClientPort} =
		new_run_port_prog(Proxy, "/tmp/", Cmd, Args),
	    proxy_chunk(Sock, ClientPort),
	    exec_port_loop(Ref, 
			   Output, 
			   Port, 
			   Sock, 
			   ClientPort, 
			   undefined,
			   undefined);
	{error, What} ->
	    Output ! {cmd_pipe, Ref, exit, ["File error:", What]}
    end.

exec_port_loop(Ref, Output, Port, Sock, ClientPort, AsyncPid, From) ->
    receive
	%%------------------------------------------
	%% send to port
	%%------------------------------------------
	{sync_cmd_pipe, New_from, Ref, data, Binary} ->
	    case AsyncPid of
		undefined ->
		    ?LOG_RAM(?SEV_1, {"Exec port. Send to port.~n"
				      "  Port = ~p~n"
				      "  Data = ~p~n", 
				      [Port, Binary]}),
		    %% HERE is this really needed?
		    {Pid, _} =
			spawn_opt(fun() -> true = port_command(Port, Binary) 
				  end,
				  [monitor, link]),
		    exec_port_loop(Ref, 
				   Output, 
				   Port, 
				   Sock, 
				   ClientPort,
				   Pid, 
				   New_from);
		_Pid ->
		    dbg("an error"),
		    exec_port_loop(Ref, 
				   Output, 
				   Port, 
				   Sock, 
				   ClientPort,
				   AsyncPid, 
				   From)
	    end;
	%%------------------------------------------
	%% source exited
	%%------------------------------------------
	{cmd_pipe, Ref, exit, _Rc} ->
	    %% dbg("got exit cmd"),
	    ?LOG_RAM(?SEV_1, {"Exec port. Source exited.~n"
			      "  Ref = ~p~n", 
			      [Ref]}),
	    proxy_close(Sock, ClientPort),
	    exec_port_loop(Ref, Output, Port, Sock, ClientPort, AsyncPid, From);
	%%------------------------------------------
	%% from port, send to Output
	%%------------------------------------------
	{Port, {data, Binary}} -> 
%% 	    ?LOG_RAM(?SEV_1, {"Exec port. Reply.~n"
%% 			      "  Ref = ~p~n", 
%% 			      [Ref]}),
	    sync_send(Output, Ref, data, Binary),
	    proxy_chunk(Sock, ClientPort),
	    exec_port_loop(Ref, Output, Port, Sock, ClientPort, AsyncPid, From);
	%%------------------------------------------
	%% port exited
	%%------------------------------------------
	{Port, {exit_status, Exit}} ->
	    ?LOG_RAM(?SEV_1, {"Exec port. Port exited.~n"
			      "  Ref = ~p~n", 
			      [Ref]}),
	    %% dbg({"port exit_status", Exit, "forwarded to", Output}),
	    Output ! {cmd_pipe, Ref, exit, Exit},
	    exit(normal);
	%%------------------------------------------
	%% unexpected message
	%%------------------------------------------
	{Port, What} ->
	    ?LOG_RAM(?SEV_WARNING, {"Exec port. Unexpected message.~n"
				    "  Ref     = ~p~n"
				    "  Message = ~p~n", 
				    [Ref, What]}),
	    sysInitI:info_msg(
	      "~p: Unexpected message from external program: ~p~n",
	      [?MODULE, What]),
	    exec_port_loop(Ref, Output, Port, Sock, ClientPort, AsyncPid, From);
	%%------------------------------------------
	%% DOWN
	%%------------------------------------------
	{'DOWN', _, process, AsyncPid, normal} ->
	    ?LOG_RAM(?SEV_WARNING, {"Exec port. DOWN.~n"
				    "  Ref = ~p~n", 
				    [Ref]}),
	    ack_data(From, Ref),
	    exec_port_loop(Ref, 
			   Output, 
			   Port, 
			   Sock, 
			   ClientPort, 
			   undefined,
			   undefined);
	%%------------------------------------------
	%% unknown message
	%%------------------------------------------
	Strange ->
	    ?LOG_RAM(?SEV_WARNING, {"Exec port. Unknown message.~n"
				    "  Message = ~p~n", 
				    [Strange]}),
	    sysInitI:info_msg("~p: Unexpected message: ~p~n",
			      [?MODULE, Strange]),
	    exec_port_loop(Ref, Output, Port, Sock, ClientPort, AsyncPid, From)
    end.

new_run_port_prog(Proxy, Cwd, Cmd, Args) ->
    {Sock, UdpPort} = open_udp_control_port(),
    %% AllArgs = ["-d", "-p", integer_to_list(UdpPort), "--", Cmd] ++ Args,
    AllArgs = ["-p", integer_to_list(UdpPort), "--", Cmd] ++ Args,
    Port = open_port({spawn_executable, Proxy},
		     [stream, 
		      {cd, Cwd},
		      {args, AllArgs}, 
		      binary,
		      exit_status, 
		      use_stdio, 
		      stderr_to_stdout]),
    receive
	{udp, Sock, _SrcIp, SrcPort, <<_P:16/big>>} ->
            %% dbg({"Port number reported", "from", SrcPort, "reported", P}),
	    {Port, Sock, SrcPort}
    after
	1000 ->
	    exit("timeout waiting for port proxy program")
    end.

open_udp_control_port() ->
    {ok, Udp}  = gen_udp:open(0, [{ip, {127,0,0,1}}, binary]),
    {ok, Port} = inet:port(Udp),
    {Udp, Port}.

proxy_chunk(Sock, ClientPort) ->
    %% dbg("proxy_chunk"),
    ok = gen_udp:send(Sock, {127,0,0,1}, ClientPort, <<1:8>>).

proxy_close(Sock, ClientPort) ->
    %% dbg("proxy_close"),
    ok = gen_udp:send(Sock, {127,0,0,1}, ClientPort, <<2:8>>).

sync_send(Output, Ref, data, Binary) ->
    Output ! {sync_cmd_pipe, self(), Ref, data, Binary},
    receive
	{sync_cmd_pipe, Ref, ack} -> ok
    after
	10000 ->
	    %% dbg({"exiting after sending", binary_to_list(Binary)}),
 	    %% dbg({"mbox", process_info(self(), messages)}),
	    %% HERE need to exit the next guy in pipe...
	    Output ! {cmd_pipe, Ref, exit, timeout},
	    exit("internal timeout sending data in pipe")
    end.

ack_data(undefined, _Ref) ->
    ok;
ack_data(From, Ref) ->
    From ! {sync_cmd_pipe, Ref, ack}.



%%=====================================================================
%% exec_erts -> 
%%
%% spawned in its ownprocess, this will be the proxy for io as it implements 
%% (parts of) an erlang io-server
%%=====================================================================
exec_erts(ShellPid, Ref, Output, M, F, A, IsColi, Remote, _Timeout) ->
    ?LOG_RAM(?SEV_5, {"Exec ERTS.~n"
		      "  MFA    = ~p~n"
		      "  Remote = ~p~n"
		      "  Ref    = ~p~n", 
		      [{M, F, A}, Remote, Ref]}),
    Self = self(),
    put(ecoli_process, exec_erts),
    Io = ecoli_io:new_io(),
    {ColiCmdPid, Monitor} = spawn_opt(fun() -> erts_proc(Self, M, F, A, Remote) 
				      end,
				      [link, monitor]),
    exec_erts_loop(ShellPid, 
		   Ref, 
		   Output, 
		   ColiCmdPid, 
		   Monitor, 
		   undefined,
		   Io, 
		   IsColi, 
		   true, 
		   0, 
		   0).

exec_erts_loop(ShellPid, 
	       Ref, 
	       Output, 
	       ColiCmdPid, 
	       Monitor, 
	       ToAck,
	       Io, 
	       IsColi, 
	       IsSrcAlive, 
	       InChars, 
	       OutChars) ->
    receive
	%%------------------------------------------
	%% IO request
	%%------------------------------------------
	Io_request when element(1, Io_request) =:= io_request ->
	    {NewToAck, NewIo, Received, Sent} =
		case ecoli_io:io_request(Io_request, IsSrcAlive, Io) of
		    {write, Reply, Chars} ->
			?LOG_RAM(?SEV_5, {"Exec ERTS. IO request. Write.~n"
					  "  Ref = ~p~n", [Ref]}),
			ecoli_io:io_reply(Reply),
			sync_send(Output, Ref, data, Chars),
			{ToAck, Io, size(Chars), 0};
		    %% not yet space in buffer, no acking
		    {read, Reply, NewIoState} ->
			?LOG_RAM(?SEV_5, {"Exec ERTS. IO request. Read.~n"
					  "  Ref = ~p~n", [Ref]}),
			ecoli_io:io_reply(Reply),
			{ToAck, NewIoState, 0, reply_to_len(Reply)};
		    %% space in buffer, do ack if ack is outstanding
		    {read_ack, Reply, NewIoState} ->
			?LOG_RAM(?SEV_5, {"Exec ERTS. IO request. Read Ack.~n"
					  "  Ref = ~p~n", [Ref]}),
			ecoli_io:io_reply(Reply),
			ack_data(ToAck, Ref),
			{undefined, NewIoState, 0, reply_to_len(Reply)};
		    {blocking, NewIoState} ->
			?LOG_RAM(?SEV_5, {"Exec ERTS. IO request. Blocking..~n"
					  "  Ref = ~p~n", [Ref]}),
			{ToAck, NewIoState, 0, 0};
		    %% these are mostly errrors
		    {reply, Reply} ->  
			?LOG_RAM(?SEV_5, {"Exec ERTS. IO request. Reply..~n"
					  "  Ref = ~p~n", [Ref]}),
			ecoli_io:io_reply(Reply),
			{ToAck, Io, 0, reply_to_len(Reply)}
		end,
	    exec_erts_loop(ShellPid, 
			   Ref, 
			   Output, 
			   ColiCmdPid, 
			   Monitor,
			   NewToAck, 
			   NewIo, 
			   IsColi, 
			   IsSrcAlive,
			   InChars + Received, 
			   OutChars + Sent);
	%%------------------------------------------
	%% source exited
	%%------------------------------------------
	{cmd_pipe, Ref, data, ?DEFAULT_EOF} -> 
	    ?LOG_RAM(?SEV_1, {"Exec ERTS. EOF.~n"
			      "  Ref = ~p~n", 
			      [Ref]}),
	    {NewToAck, NewIo} = maybe_unblock(Io, false, Output, Ref, ToAck),
	    exec_erts_loop(ShellPid, 
			   Ref, 
			   Output, 
			   ColiCmdPid, 
			   Monitor,
			   NewToAck, 
			   NewIo, 
			   IsColi, 
			   false,
			   InChars, 
			   OutChars);
	%%------------------------------------------
	%% 
	%%------------------------------------------
	{sync_cmd_pipe, From, Ref, data, Data} ->
	    ?LOG_RAM(?SEV_5, {"Exec ERTS. Sync~n"
			      "  Ref = ~p~n", 
			      [Ref]}),
	    {NewToAck, Next_io} =
		case ecoli_io:new_io(Io, Data) of
		    {more, Nxt_io} ->
			ack_data(From, Ref),
			{undefined, Nxt_io};
		    {full, Nxt_io} ->
			{From, Nxt_io}
		end,
            {Last_to_ack, NewIo} =
		maybe_unblock(Next_io, IsSrcAlive, Output, Ref, NewToAck),
	    exec_erts_loop(ShellPid, 
			   Ref, 
			   Output, 
			   ColiCmdPid, 
			   Monitor,
			   Last_to_ack, 
			   NewIo, 
			   IsColi, 
			   IsSrcAlive,
			   InChars, 
			   OutChars + size(Data));
	%%------------------------------------------
	%% source exited
	%%------------------------------------------
	{cmd_pipe, Ref, exit, _Rc} -> 
	    ?LOG_RAM(?SEV_1, {"Exec ERTS. Exited.~n"
			      "  Ref = ~p~n", 
			      [Ref]}),
	    {NewToAck, NewIo} =
		maybe_unblock(Io, false, Output, Ref, ToAck),
	    exec_erts_loop(ShellPid, 
			   Ref, 
			   Output, 
			   ColiCmdPid, 
			   Monitor,
			   NewToAck, 
			   NewIo, 
			   IsColi, 
			   false,
			   InChars, 
			   OutChars);
	%%------------------------------------------
	%% DOWN
	%%------------------------------------------
	{'DOWN', Monitor, _, _Pid, Info} ->
	    ?LOG_RAM(?SEV_1, {"Exec ERTS. DOWN.~n"
			      "  Ref = ~p~n", 
			      [Ref]}),
	    Output ! {cmd_pipe, Ref, exit, Info},
	    exit(Info);
	%%------------------------------------------
	%% 
	%%------------------------------------------
 	{internal_cmd, _} = Internal when IsColi ->
	    ?LOG_RAM(?SEV_1, {"Exec ERTS. Internal cmd.~n"
			      "  Ref = ~p~n", 
			      [Ref]}),
	    ShellPid ! Internal,
	    exec_erts_loop(ShellPid, 
			   Ref, 
			   Output, 
			   ColiCmdPid, 
			   Monitor,
			   ToAck, 
			   Io, 
			   IsColi, 
			   IsSrcAlive,
			   InChars, 
			   OutChars);
	%%------------------------------------------
	%% unknown message
	%%------------------------------------------
	Any -> %% HERE maybe this should just silently discard
	    ?LOG_RAM(?SEV_WARNING, {"ERTS. Unknown message.~n"
				    "  Message = ~p~n", 
				    [Any]}),
	    erlang:display({module, ?MODULE, "Unexpected message:", Any}),
	    exec_erts_loop(ShellPid, 
			   Ref, 
			   Output, 
			   ColiCmdPid, 
			   Monitor,
			   ToAck, 
			   Io, 
			   IsColi, 
			   IsSrcAlive,
			   InChars, 
			   OutChars)
    end.



%%=====================================================================
%% 
%%=====================================================================
exec_fruacc(FruId, Cmd, Args, ShellPid, Ref, Output, Timeout) ->
    Self = self(),
    put(ecoli_process, exec_fruacc),
    Io = ecoli_io:new_io(),
    {ColiCmdPid, Monitor} = 
	spawn_opt(fun() -> fruacc_proc(Self, FruId, Cmd, Args, Timeout) end,
		  [link, monitor]),
    %% Use erts loop, fruacc has the seame behaviour as erts
    exec_erts_loop(ShellPid, 
		   Ref, 
		   Output, 
		   ColiCmdPid, 
		   Monitor, 
		   undefined,
		   Io, 
		   true,
		   true, 
		   0, 
		   0).



fruacc_proc(Proxy,
	    FruId, 
	    #coli_cmd{cli_scope = CliScope} = Cmd, 
	    Args,
	    Timeout) ->
    put(ecoli_process, fruacc_proc),
    true = group_leader(Proxy, self()),
    ecoli_itc_server:send(CliScope, FruId, Cmd, Args),
    fruacc_proc_receive(Proxy, FruId, Cmd, Args, Timeout).

fruacc_proc_receive(Proxy,
		    FruId, 
		    #coli_cmd{cli_pname = Pname} = Cmd, 
		    Args,
		    Timeout) ->
    
    receive
	{request_reply, {?COLI_RESULT_OK, _ReqId, Reply}} ->
	    ?LOG_RAM(?SEV_5, 
		     {"FRUACC. Reply OK~n"
		      "  Reply   = ~p~n"
		      "  ColiCmd = ~p~n", 
		      [Reply, Pname]}),
	    %% print the result in the cli shell
	    io:format("~s~n", [Reply]),
	    exit(normal);
	{request_reply, {?COLI_RESULT_OK_CONTINUE, _ReqId, Reply}} ->
	    ?LOG_RAM(?SEV_5, 
		     {"FRUACC. Reply OK CONTINUE~n"
		      "  Reply   = ~p~n"
		      "  ColiCmd = ~p~n", 
		      [Reply, Pname]}),
	    %% print the result in the cli shell, restart timer
	    io:format("~s", [Reply]),
	    fruacc_proc_receive(Proxy, FruId, Cmd, Args, Timeout);
	{request_reply, {?COLI_RESULT_NO_FRU_EXISTS, _ReqId, Reply}} ->
	    ?LOG_RAM(?SEV_WARNING, 
		     {"FRUACC. ~n"
		      "  Message = ~p~n"
		      "  Reply   = ~p~n"
		      "  ColiCmd = ~p~n", 
		      ["COLI_RESULT_NO_FRU_EXISTS", Reply, Pname]}),
	    %% print the result in the cli shell
	    io:format("FRU does not exist.~n~s~n", [Reply]),
	    exit(normal);
	{request_reply, {?COLI_RESULT_OTHER_ERROR, _ReqId, Reply}} ->
	    ?LOG_RAM(?SEV_WARNING, 
		     {"FRUACC. Error message.~n"
		      "  Reply   = ~p~n"
		      "  ColiCmd = ~p~n", 
		      [Reply, Pname]}),
	    %% print the result in the cli shell
	    io:format("Other error.~n~p~n", [Reply]),
	    exit(normal);
	UnknownMsg ->
	    ?LOG_RAM(?SEV_WARNING,
		     {"FRUACC. Unexpected message.~n"
		      "  Message = ~p~n"
		      "  ColiCmd = ~p~n", 
		      [UnknownMsg, Pname]}),
	    exit("Received unknown message when waiting for fruacc coli reply")
    after Timeout ->
	    ?LOG_RAM(?SEV_WARNING, 
		     {"FRUACC. Timeout.~n"
		      "  ColiCmd = ~p~n", 
		      [Pname]}),
	    exit("Timeout when waiting for fruacc coli reply")
    end.




%%=====================================================================
%% Miscelanous functions
%%=====================================================================
reply_to_len({_, _, {read, _Io, Buf}}) when is_binary(Buf) ->
    size(Buf);
reply_to_len(_) ->
    0.

maybe_unblock(Io, IsAlive, Output, Ref, ToAck) ->
    case ecoli_io:maybe_unblock(Io, IsAlive) of
	ok ->
	    {ToAck, Io};
	{write, Reply, Chars} ->
	    ecoli_io:io_reply(Reply),
	    Output ! {cmd_pipe, Ref, data, Chars},
	    {ToAck, Io};
	{read, Reply, NewIoState} ->
	    ecoli_io:io_reply(Reply),
	    {ToAck, NewIoState};
	{read_ack, Reply, NewIoState} ->
	    %% space in buffer, do ack if ack is outstanding
	    ecoli_io:io_reply(Reply),
	    ack_data(ToAck, Ref),
	    {undefined, NewIoState};
	{blocking, NewIoState} ->
	    {ToAck, NewIoState};
	{reply, Reply} ->
	    ecoli_io:io_reply(Reply),
	    {ToAck, Io}
    end.

erts_proc(Proxy, M, F, A, local) ->
    put(ecoli_process, erts_proc),
    true = group_leader(Proxy, self()),
    apply(M, F, [A]);
erts_proc(Proxy, M, F, A, RemoteNode) ->
    put(ecoli_process, erts_proc),
    true = group_leader(Proxy, self()),
    rpc:call(get_node(RemoteNode), erlang, apply, [M, F, [A]]).


find_colish_cmd_dir() ->
    case sysEnv:rcs_mode_2() of
	simulated->
	    filename:join([os:getenv("RCS_ROOT"), "colish"]);
	_ ->  % target and vrcs
	    "/var/run/coli"
    end.

find_eof_proxy() -> %HERE
    sysEnv:find_private_binary(ecoli, ?EOF_PROXY, []).

%% Don't use any io:format in here since stuff is sometimes run with the
%% group_leader pointing to unexpected places...
dbg(Term) -> erlang:display({dbg, ?MODULE, self(), Term}).



get_node(local) -> 
    clhI:erlang_node();
get_node(Node) when is_list(Node) -> 
    gn_mp_id(clhI:mp_id(Node), Node);
get_node(Node) -> 
    Node.

gn_mp_id(undefined, Node) ->
    ?LOG_RAM(?SEV_1, {"Node not found ~p~n", [Node]}),
    undefined;
gn_mp_id(MpId, Node) ->
    gn_erlang(clhI:erlang_node(MpId), Node, MpId).

gn_erlang(undefined, Node, MpId) ->
    ?LOG_RAM(?SEV_1, {"Node not found ~n"
		      "  MO   = ~p~n" 
		      "  MpId = ~p~n", 
		      [Node, MpId]}),
    undefined;
gn_erlang(ErlangNode, _Node, _MpId) ->
    ErlangNode.


 

%%========================================================================
%% pp(Rec) -> ok.
%% 
%%========================================================================
pp(Rec) when is_record(Rec, coli_cmd) ->
    [_ | Vals] = tuple_to_list(Rec),
    F = record_info(fields, coli_cmd),
    L = pp_max_length(F, 0),
    lists:flatten([io_lib:format("    ~-*s = ~p~n", [L, K, V]) || 
	{K, V} <- lists:zip(F, Vals), lists:member(K, ?CMD_PRINT)]);
pp(Term) ->
    Term.

pp_max_length([], L) ->
    L;
pp_max_length([H|T], L) ->
    case length(atom_to_list(H)) of
	GR when GR > L -> pp_max_length(T, GR);
	_              -> pp_max_length(T, L)
    end.


error_msg(Why) ->
    io:format(Why).
