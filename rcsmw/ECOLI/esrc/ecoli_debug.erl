%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ecoli_debug.erl %
%%% Author:     etxbjca
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(ecoli_debug).
%-behaviour(behaviour).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R11A/2').
-date('2017-09-14').
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
%%% R2A/2      2013-08-27   etxlg     Editing
%%% R2A/3      2013-10-04   etxlg     function to add new cmd
%%% R3A/1      2015-03-19   etxlg     function to add new cmd, misc. fix
%%% R5A/2      2016-01-08   etxberb   Fixed dialyzer problem.
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-include("ecoli.hrl").



-export([register_appdata_receiver/0, start_coli_ssh/0, stop_coli_ssh/0]).
-export([test_init/0]).
-export([list_cmds/0, list_cmds/1]).
-export([clear_registration_table/0]).
-export([populate_registration_table/0]).

-export([testfunction/1]).
-export([load_mods/0]).

-export([add_new_cmd/1]).
-export([process_leaks/0]).
-export([add_internal_test_cmd/0, add_internal_test_cmd/1]).


-export([itc_start/1]).
-export([itc_stop/1]).
-export([itc_loop/1]).
-export([itc_hunt/2]).
-export([itc_register/2]).
-export([itc_add_fru/3]).
-export([itc_del_fru/3]).
-export([itc_reply/2]).

-export([print_coli_cmds/0]).
-export([print_coli_cmds/2]).

-export([print_coli_users/0]).
-export([print_coli_users/1]).
-export([print_coli_users/2]).

%% FAKE test coli command
-export([coli_test/1]).


coli_test(In) ->
    io:format("~nKilroy was here: ~p  ~n~p~n~n", [node(), In]),
    exit("Nu ar det slut!!!").


add_internal_test_cmd() ->
    [ecoli_register:validate_and_add_cmd(Proplist) || Proplist <- [
	[{type, rootfs}, {cli_name, "kat"}, {cli_path, "/coli_test"},
	 {args, []}, {cxp_path, "fake all the way"}, {filepath, "/bin/cat"},
	 {usage, "please"}, {description, "nothing to see here, move along"}]
	]
    ].

add_internal_test_cmd(Name) when is_list(Name) ->
    [ecoli_register:validate_and_add_cmd(Proplist) || Proplist <- [
	[{type, rootfs}, {cli_name, Name}, {cli_path, "/coli_test"},
	 {args, []}, {cxp_path, "fake all the way"},
	 {filepath, filename:join(["/tmp", Name])},
	 {usage, "please"}, {description, "nothing to see here, move along"}]
	]
    ].

add_new_cmd(Proplist) ->
    ecoli_register:validate_and_add_cmd(Proplist).

test_init() ->
    try mnesia:table_info(coli_cmd, arity) of
	_ -> mnesia:delete_table(coli_cmd)
    catch
	_:_ -> ok
    end,
    ecoli_datainit:instPhParallel_init([node()]).

process_leaks() ->
    [process_leaks(P, process_info(P, dictionary)) || P <- processes()],
    ok.
process_leaks(_, undefined) -> ok; %it exited before we did process_info()
process_leaks(_, {dictionary, []}) -> ok;
process_leaks(P, {dictionary, Dict}) ->
    [p_leak(P, D) || D <- Dict].
p_leak(P, {ecoli_process, I}) ->
    io:format("~p: ~p ~p~n", [P, I, process_info(P, memory)]);
p_leak(_, _) -> ok.

%normally done i dataInit
register_appdata_receiver() ->
    ok = swmAppData:register_appdata_receiver("coli_reg", ecoli_register),
    ok = swmAppData:register_appdata_receiver("coli_auth", ecoli_register).

clear_registration_table() ->
    {atomic, ok} =  mnesia:clear_table(coli_cmd).

populate_registration_table() ->
    swmAppData:push_appdata().

-define(TESTPORT, 12345).
-define(COLI_CHANNEL, ecoli_ssh_channel).
start_coli_ssh() ->
    Bind_ip = {127,0,0,1},
    Port = ?TESTPORT,
    Optlist =
     %disable subsystems, strangeness if client runs -s sftp
    [{ssh_cli, {?COLI_CHANNEL, [place_holder_for_arg]}},
                {pwdfun, fun authenticate_test_user/2},
             {subsystems, []},
    {system_dir,  sysEnv:releases_vsn_dir()},
    %undocumented w/a to make sure keys are NOT found, i.e. force pw-auth
    {user_dir, "/tmp"}],
    io:format("Starting ssh daemon for COLI on port: ~b~n", [Port]),
    io:format("Starting ssh daemon for COLI on Opts: ~p~n", [Optlist]),
    {ok, Ref} = ssh:daemon(Bind_ip, Port, Optlist),
    Ref.

stop_coli_ssh() ->
    ssh:stop_daemon({127,0,0,1}, ?TESTPORT).

authenticate_test_user(User, Pw) ->
    io:format("Authenticate, User: ~p, Password: ~p~n", [User, Pw]),
    true.

list_cmds() ->
    list_cmds(0).
list_cmds(Auth_level) ->
    io:format("~-15s~-30s~-10s~-10s~-6s~n",
		["CLI_name", "CLI_path", "Type", "Auth", "Args"]),
    io:format("~-15s~-30s~-10s~-10s~-6s~n",
		["========", "========", "====", "====", "===="]),
    Keys = ecoli_datainit:all_cmds(Auth_level),
    [begin
	Cmd = ecoli_datainit:lookup({Cli_path, Cli_name}, Auth_level),
	io:format("~-15s~-30s~-10s~-10s~p~n", [Cli_name, Cli_path,
		atom_to_list(Cmd#coli_cmd.type),
		ecoli_lib:int_to_auth_string(Cmd#coli_cmd.authorization),
		Cmd#coli_cmd.args])
    end || {Cli_path, Cli_name} <- Keys],
    ok.

testfunction(Arg) ->
    io:format("This is testfunction that got arg: ~p~n", [Arg]),
    exit(normal).

load_mods() ->
    %{ok,"/home/etxlg/Work/Coli/Ecoli"} = file:get_cwd(),
    Dir = "/home/sirpa/dev_patches",
    %Dir = "/view/etxlg_rcs/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/ECOLI/ECOLI_CNX9013069/ECOLI_CAX1033534/out",
    %%{ok,"/view/etxlg_rcs/vobs/rcs/dev/RCP_CSX10179/RCS_CRX901266/ECOLI/ECOLI_CNX9013069/ECOLI_CAX1033534/out"} = file:get_cwd(),
    {ok, F} = file:list_dir(Dir),
    B = lists:foldl(fun file_to_beam/2, [], F),
    [begin
	io:format("purge & load: ~p~n", [M]),
	code:purge(M), code:load_file(M)
    end || M <- B].

file_to_beam(F, Acc) ->
    case re:run(F, "(ecoli_.*)\\.beam$", [{capture, all_but_first, list}]) of
        {match, [B]} -> [list_to_atom(B) | Acc];
        _ -> Acc
    end.



print_coli_users() ->
    io:format("~nprint_coli_cmds takes one or two attributes: ~n"
	      " Key | Key and Value~n"
	      "~n"
	      "Key = [port | ldn | cli ]~n"
	      "Value = atom() | string()~n"
	      "~n"
	      " Attributes: ~n"
	      "  (port)~n"
	      "     Prints all user itc port numbers in use.~n"
	      "  (port, Port)~n"
	      "     Port = all | integer()~n"
	      "     Prints all user information ordered by port number.~n"
	      "  (ldn)~n" 
	      "     Prints all configured ldns.~n"
	      "  (ldn, Value)~n"
	      "     Value = atom() | string() : cli type~n"
	      "     Prints all configured ldns for the requested cli type.~n"
	      "  (cli)~n"
	      "     Prints ports and LDNs for all cli types.~n"
	      "  (cli, Value)~n"
	      "     Value = atom() | string() : cli type~n"
	      "     Prints ports and LDNs for the requested cli type.~n").


print_coli_users(port) ->
    ets:foldl(fun pcu_port/2, port, ecoli_fruacc),
    ok;
print_coli_users(ldn) ->
    ets:foldl(fun pcu_ldn/2, ldn, ecoli_fruacc),
    ok;
print_coli_users(cli) ->
    ets:foldl(fun pcu_cli/2, cli, ecoli_fruacc),
    ok.


print_coli_users(Key, Value) when is_atom(Value) ->
    print_coli_users(Key, atom_to_list(Value));
print_coli_users(port, Port) ->
    ets:foldl(fun pcu_port/2, Port, ecoli_fruacc),
    ok;
print_coli_users(ldn, Cli) ->
    ets:foldl(fun pcu_ldn/2, Cli, ecoli_fruacc),
    ok;
print_coli_users(cli, Cli) ->
    ets:foldl(fun pcu_cli/2, Cli, ecoli_fruacc),
    ok.



pcu_port(#fruacc{key = {Type, Port}}, port) ->
    io:format("~nPort = ~p   "
	      "Type = ~p~n~n",
	      [Port, Type]),
    port;
pcu_port(#fruacc{key  = {Type, Port},
		 ldns = [L | Ldns]},
	 Port) ->
    io:format("~nPort = ~p~n"
	      "Type = ~p~n"
	      "Ldn  = ",
	      [Port, Type]),
    io:format("~p~n", [L]),
    [io:format("       ~p~n", [Ldn]) || Ldn <- Ldns],
    io:format("~n"),
    Port;
pcu_port(_, Port) ->
    Port.


pcu_ldn(#fruacc{ldns = Ldns},
	 ldn) ->
    [io:format("  ~p~n", [Ldn]) || Ldn <- Ldns],
    ldn;
pcu_ldn(#fruacc{key  = {Cli, Port},
		ldns = [L | Ldns]},
	Cli) ->
    io:format("Port = ~p~n"
	      "Ldn  = ",
	      [Port]),
    io:format("~p~n", [L]),
    [io:format("       ~p~n", [Ldn]) || Ldn <- Ldns],
    io:format("~n"),
    Cli;
pcu_ldn(_, Cli) ->
    Cli.


pcu_cli(#fruacc{key  = {KeyCli, Port},
		 ldns = [L | Ldns]},
	 Cli) when Cli == cli orelse Cli == KeyCli ->
    io:format("~nCli  = ~p~n"
	      "Port = ~p~n"
	      "Ldn  = ",
	      [KeyCli, Port]),
    io:format("~p~n", [L]),
    [io:format("       ~p~n", [Ldn]) || Ldn <- Ldns],
    io:format("~n"),
    Cli;
pcu_cli(_, Cli) ->
    Cli.



    

print_coli_cmds() ->
    io:format("print_coli_cmds takes two attributes: Key and Value~n"
	      "~n"
	      "Key   = [cli | fru | cmd]~n"
	      "Value = atom() | string()~n"
	      "~n"
	      " - cli prints all coli commands for the requested cli type~n"
	      " - fru prints all coli commands for the requested fru type~n"
	      " - cmd prints detailed information about the coli commands where~n"
	      "   Value is part of the path or commad~n~n").


print_coli_cmds(Key, du) ->
    print_coli_cmds(Key, "undefined");
print_coli_cmds(Key, Value) when is_list(Value) ->
    print_coli_cmds(Key, list_to_atom(Value));

print_coli_cmds(fru, Value) ->
    Match_all = mnesia:table_info(coli_cmd, wild_pattern),
    Match = Match_all#coli_cmd{cli_pname = {'$1', '$2'},
			       cli_scope = '$3'},
    Cond = [{'==', Value, '$3'}],
    Clis = mnesia:dirty_select(coli_cmd, 
			       [{Match, Cond, [{{'$1', '$2'}}]}]),
    pcc("~nCliType = ~p~n~n", Value, Clis);
print_coli_cmds(cli, Value) ->
    Match_all = mnesia:table_info(coli_cmd, wild_pattern),
    Match = Match_all#coli_cmd{cli_pname = {'$1', '$2'},
			       cli_type  = '$3'},
    Cond = [{'==', Value, '$3'}],
    Frus = mnesia:dirty_select(coli_cmd, 
			       [{Match, Cond, [{{'$1', '$2'}}]}]),
    pcc("~nFruType = ~p~n~n", Value, lists:sort(Frus));
print_coli_cmds(cmd, Value) ->
    ets:foldl(fun pcc_cmd/2, atom_to_list(Value), coli_cmd);
print_coli_cmds(Key, _Value) ->
    io:format("~nDo not understand '~p'~n"
	      "Only valid values are: 'cli', 'fru' and 'cmd'~n",
	      [Key]),
    error.



pcc(Str, Key, Vals) ->
    io:format(Str, [Key]), 
    [io:format("  ~p~n", [string:join([A, B], "/")]) || {A, B} <- Vals],
    io:format("~n~n").


pcc_cmd(#coli_cmd{cli_pname = {P, C}}= ColiCmd, Value) ->
    Cmd = string:join([P, C], "/"),
    case string:str(Cmd, Value) of
	0 -> ok;
	_ -> table(coli_cmd, [ColiCmd], true)
    end,
    Value.


table(Tab, [], Print) ->
    table_name(Tab, Print),
    io:format("~n");
table(Tab, [H|T], Print) ->
    [_ | List] = tuple_to_list(H),
    table_name(Tab, Print),
    Fields = table_fields(Tab),
    L = max_length(Fields, 0),
    [io:format("  ~-*s :  ~p~n", [L, K, V]) || {K, V} <- lists:zip(Fields, List)],
    case T of
	[] -> io:format("~n");
	_  -> io:format("  ----------~n")
    end,
    table(Tab, T, false).

table_name(Tab, true) ->
    io:format("=== ~p ===~n", [Tab]);
table_name(_, _) ->   
    ok.

table_fields(coli_cmd) -> record_info(fields, coli_cmd).
%%table_fields(_)        -> unknown_record.

max_length([], L) ->
    L;
max_length([H|T], L) ->
    case length(atom_to_list(H)) of
	GR when GR > L -> max_length(T, GR);
	_              -> max_length(T, L)
    end.


-record(itc_data, {name, 
		   own_port,
		   coli_port,
		   coli_ref,
		   coli_pid,
		   coli_mboxid,
		   hunt_ref,
		   user_pid,
		   replies = []
		   }).
		   

itc_start(Name) ->
    Self = self(),
    spawn(fun() -> itc_init(Name, Self) end),
    receive
	inited ->
	    ok
    after 3000 ->
	    io:format("ITC_APP itc_start ~p~n", [timeout]),
	    timeout
    end.
    

itc_stop(Name) ->
    stop(Name),
    ok.

stop(Name) ->
    Name ! {stop, Name, self()},
    receive
	stopped ->
	    ok
    after 3000 ->
	    unregister(Name),
	    io:format("ITC_APP itc_stop ~p~n", [timeout]),
	    timeout
    end.
   
    

itc_init(Name, UserPid) ->
    io:format("ITC_APP Process started ~p~n", [Name]),
    ii(whereis(Name), Name),
    ItcPort = itc:open(atom_to_list(Name)),
    itc:listen(ItcPort),
    UserPid ! inited,
    itc_loop(#itc_data{name     = Name,
		       own_port = ItcPort}).

ii(undefined, Name) ->
    register(Name, self());
ii(_Pid, Name) ->
    stop(Name),
    register(Name, self()).
   
    

itc_register(Name, FruType) ->
    Name ! {itc_register, FruType},
    ok.

itc_add_fru(Name, FruType, Ldn) ->
    Name ! {itc_add_fru, FruType, Ldn},
    ok.

itc_del_fru(Name, FruType, Ldn) ->
    Name ! {itc_del_fru, FruType, Ldn},
    ok.

itc_hunt(Name, Hunt) ->
    Name ! {itc_hunt, self(), Hunt},
    receive
	hunted ->
	    ok;
	5000 ->
	    {error, hunt_timeout}
    end.

itc_reply(Name, Reply) ->
    Name ! {itc_reply, Reply},
    ok.



itc_loop(#itc_data{name        = Name,
		   own_port    = OwnPort,
		   coli_port   = ColiPort,
		   coli_mboxid = ColiMboxId,
		   coli_ref    = _ColiRef,
		   hunt_ref    = HuntRef,
		   user_pid    = UserPid,
		   replies     = Replies
		  } = Loop) ->
    io:format("ITC_APP Loop ~p~n", [Loop]),
    receive
	{itc_hunt, NewUserPid,Hunt} ->
	    NewHuntRef = itc:hunt(OwnPort, Hunt),
	    io:format("ITC_APP Hunt ~p ~p~n~p~n", [OwnPort, Hunt, NewHuntRef]),
	    itc_loop(Loop#itc_data{hunt_ref = NewHuntRef,
				   user_pid = NewUserPid});
	{itc_register, FruTypes} ->
	    io:format("ITC_APP Register ~p ~p ~p~n", [ColiPort, ColiMboxId, FruTypes]),
	    Fun = fun(FT) -> <<FT/binary, 0, 44>> end,
	    FTLs = [Fun(list_to_binary(FT)) || FT <- string:tokens(FruTypes, " ")],
	    Data = erlang:iolist_to_binary(FTLs),
	    io:format("ITC_APP SEND ~p ~n",
		      [{OwnPort, ColiMboxId, ?COLI_INTERFACE_REGISTRATION, Data}]),
	    io:format("ITC_APP mboxid ~p~n", [itc:get_id(OwnPort)]),
	    itc:send(OwnPort, ColiMboxId, ?COLI_INTERFACE_REGISTRATION, Data),
	    itc_loop(Loop);
	{itc_add_fru, FruType, Ldn} ->
	    io:format("ITC_APP Add fru ~p ~n", [{FruType, Ldn}]),
	    FTL = list_to_binary(FruType),
	    LDN = list_to_binary(Ldn),
	    Data = <<FTL/binary, 0, LDN/binary, 0>>,
	    itc:send(OwnPort, ColiMboxId, ?COLI_ADD_FRU, Data),
	    itc_loop(Loop);
	{itc_del_fru, FruType, Ldn} ->
	    io:format("ITC_APP Delete fru ~p ~n", [{FruType, Ldn}]),
	    FTL = list_to_binary(FruType),
	    LDN = list_to_binary(Ldn),
	    Data = <<FTL/binary, 0, LDN/binary, 0>>,
	    itc:send(OwnPort, ColiMboxId, ?COLI_DELETE_FRU, Data),
	    itc_loop(Loop);
	{itc_reply, NewReplies} ->
	    io:format("ITC_APP Update replies ~p ~n", [Replies]),
	    itc_loop(Loop#itc_data{replies = NewReplies});
	{message, _, {_FrMboxId, _ToMboxId, ?COLI_COMMAND_REQUEST, MsgData} = _M} ->
	    {Cmd, ReqId, _Ldn} = decode_cmd_req(MsgData),
%%    	    io:format("ITC_APP ~p: MESSAGE ~p~n", [Name, _M]),
%%   	    io:format("ITC_APP ~p: MESSAGE ~p~n", [Name, {Cmd, ReqId, _Ldn}]),
	    ResultC = ?COLI_RESULT_OK_CONTINUE,
	    {ReplyC, _NewRepliesC}  = get_reply(Replies, Cmd),
	    DataC = <<ResultC:1/native-unsigned-integer-unit:32, 
		    ReqId:1/native-unsigned-integer-unit:32,
		    ReplyC/binary,
		    0>>,
	    io:format("ITC_APP ~p: reply msg ~p~n", [Name, DataC]),
	    itc:send(OwnPort, ColiMboxId, ?COLI_COMMAND_REPLY, DataC),

	    loop(3, OwnPort, ColiMboxId, Cmd, ReqId),

	    timer:sleep(4000),

 	    Result = ?COLI_RESULT_OK,
 	    {Reply, NewReplies}  = get_reply2(Replies, Cmd),
	    Data = <<Result:1/native-unsigned-integer-unit:32, 
		    ReqId:1/native-unsigned-integer-unit:32,
		    Reply/binary,
		    0>>,
	    io:format("ITC_APP ~p: 2nd reply msg ~p~n", [Name, Data]),
	    itc:send(OwnPort, ColiMboxId, ?COLI_COMMAND_REPLY, Data),
	    itc_loop(Loop#itc_data{replies = NewReplies});
	{mailbox_up, OwnPort, HuntRef, Pid} = M ->
	    io:format("ITC_APP ~p: mailbox_up ~p~n", [Name, M]),
	    NewColiRef = itc:attach(OwnPort, Pid),
	    io:format("ITC_APP ~p: mailbox_up ~p ~p~n", [Name, Pid, NewColiRef]),
	    UserPid ! hunted,
	    itc_loop(Loop#itc_data{coli_mboxid = Pid,
				   hunt_ref    = undefined,
				   user_pid    = undefined});
	{mailbox_down, OwnPort, _ColiRef, ColiMboxId} ->
	    io:format("ITC_APP mailbox down ~p~n", [Name]),
	    itc_loop(Loop);
	{stop, Name, User} ->
	    itc:close(OwnPort),
	    unregister(Name),
	    User ! stopped,
	    %%io:format("ITC_APP Process stopped ~p~n", [Name]),
	    stopped;
	Unknown ->
	    io:format("ITC_APP Unknown message ~p~n", [Unknown]),
	    itc_loop(Loop)
    end.


loop(0, _, _, _, _) ->
    ok;
loop(N, OwnPort, ColiMboxId, Cmd, ReqId) ->
    ResultC = ?COLI_RESULT_OK_CONTINUE,
    {ReplyC, _NewRepliesC}  = get_reply([], Cmd),
    DataC = <<ResultC:1/native-unsigned-integer-unit:32, 
	     ReqId:1/native-unsigned-integer-unit:32,
	     ReplyC/binary,
	     0>>,
    %%io:format("ITC_APP ~p: reply msg ~p~n", [Name, DataC]),
    itc:send(OwnPort, ColiMboxId, ?COLI_COMMAND_REPLY, DataC),
    loop(N - 1, OwnPort, ColiMboxId, Cmd, ReqId).



get_reply([], Cmd) ->
    Reply = "Please, do not disturb \nI am busy \n" ++ Cmd ++ "\n",
    {list_to_binary(Reply), []};
get_reply([H | T], _) ->
    {list_to_binary(H), T}.

get_reply2([], Cmd) ->
    Reply = "Please, do not disturb \nI am a sleep \n" ++ Cmd ++ "\n",
    {list_to_binary(Reply), []};
get_reply2([H | T], _) ->
    {list_to_binary(H), T}.


decode_cmd_req(Data) ->
    {ReqId,  R1} = decode_32(Data),
    {Cmd, R2}    = decode_string(R1),
    {Ldn,  _} = decode_string(R2),
    {Cmd, ReqId, Ldn}.



decode_string(String) ->
    dec_str(String, []).

dec_str(Str, Acc) ->
    case decode_8(Str) of
	{0, Rem} -> {lists:reverse(Acc), Rem};
	{S, Rem} -> dec_str(Rem, [S | Acc])
    end.


decode_8(<<Int:1/native-unsigned-integer-unit:8, Rest/binary>>) ->
    {Int, Rest}.

decode_32(<<Int:1/native-unsigned-integer-unit:32, Rest/binary>>) ->
    {Int, Rest}.

    
