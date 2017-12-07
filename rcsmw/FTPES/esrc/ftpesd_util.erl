%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ftpesd_util.erl %
%%% @author ekurnik
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R8A/R9A/R10A/2
%%%
%%% ----------------------------------------------------------

-module(ftpesd_util).
-vsn('/main/R8A/R9A/R10A/2').
-date('2017-06-01').
-author('ekurnik').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% R8A/1    2016-11-18   ekurnik    Created
%%% R8A/5    2016-12-19   ekurnik    Added get_verify_fun/2
%%% R8A/7    2017-01-04   ekurnik    Minor fix
%%% R8A/8    2017-01-10   ekurnik    Added absolute path support
%%% R8A/9    2017-01-10   estjako    Added get_ftpes_session_info
%%% R9A/2    2017-03-29   ekurnik    Authentication sequence refactoring
%%% R10A/2   2017-06-01   ekurnik    Corrected handling of symlinks
%%% ----------------------------------------------------------


-export([format_address/2, packet_to_tokens/1, check_repr_type/1,
	 response/2, send_reply/3, send_message/2,
	 check_auth/2, implemented_commands/0, extensions/0, concat_extensions/0, 
     opts_enabled/0, supported_security_protocols/0, protection_levels/0,
	 get_file_info/1, get_full_path/1, get_full_path/2, concat_paths/2,
	 transformfrom/2, transformto/2, sec_log/2, ip_to_string/1,
	 list2portip/1, eprtlist2portip/1, get_server_ip/0, getaddr/1,
	 bin_to_upper/1, binlist_to_string/1, check_dir_in_chroot/2,     
	 get_verify_fun/2, get_current_dir/2]).

-export([get_ftpes_session_info/1,
         get_ftpes_session_info/2,
         get_ftpes_session_info/3
         ]).

-include("ftpesd.hrl").
-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/file.hrl").

%% Converts ip and port to "h1,h2,h3,h4,p1,p2" format
format_address({A1, A2, A3, A4}, Port) ->
    <<P1:8, P2:8>> = <<Port:16>>,
    lists:concat([A1,",",A2,",",A3,",",A4,",",P1,",",P2]).

bin_to_upper(T) ->
    << <<if (X=<$z)and(X>=$a) -> X-($a-$A); true -> X end>> || <<X:8>> <= T >>.

binlist_to_string(List) ->
    StrList = [ binary_to_list(E) || E <- List],
    string:join(StrList, " ").

%% Separate command from message and convert to upper case
%% eg. "user someone" -> {<<"USER">>, [<<"someone">>]}
-spec packet_to_tokens(Data :: bitstring()) ->
    {Command :: bitstring(), [Message :: bitstring()]}.
packet_to_tokens(Data) ->
    TrimmedData  = re:replace(Data, "\r\n", "",[{return,list}]),
    SplittedData = re:split(TrimmedData, " "),
    case SplittedData of
	[Command | Msg] -> {bin_to_upper(Command), Msg};
	_		-> 
               %% sysInitI:error_msg("Error: packet parse failed\n"),
               ?LOG("Error: packet parse failed\n"),
			   {<<"">>, []}
    end.

%% check for TYPE command arguments
check_repr_type([Type])     -> lists:member(Type, ["I","A"]);
check_repr_type(["L", Arg]) -> Arg == "8";
check_repr_type(_)	    -> false.

%% All implemented commands
implemented_commands() ->
    [<<"PASS">>, <<"NOOP">>, <<"QUIT">>, <<"USER">>,  <<"TYPE">>,
     <<"SIZE">>, <<"RETR">>, <<"STOR">>, <<"APPE">>, <<"CWD">>,
     <<"PWD">>,  <<"STRU">>, <<"PASV">>, <<"PORT">>, <<"EPSV">>,
     <<"EPRT">>, <<"LIST">>, <<"NLST">>, <<"REIN">>, <<"MKD">>,
     <<"RMD">>,  <<"DELE">>, <<"RNFR">>, <<"RNTO">>, <<"MODE">>,
     <<"FEAT">>, <<"OPTS">>, <<"PROT">>, <<"AUTH">>, <<"PBSZ">>, <<"ABOR">> ].
auth_command() ->
    [<<"AUTH">>].
pbsz_command() ->
    [<<"PBSZ">>].
prot_command() ->
    [<<"PROT">>].
user_command() ->
    [<<"USER">>].
%% commands not requiring authentication
non_auth_commands() ->
    [<<"QUIT">>, <<"NOOP">>, <<"FEAT">>, <<"OPTS">>].
%% commands supported after authentication
other_commands() ->
    [<<"TYPE">>, <<"QUIT">>, <<"NOOP">>,
     <<"SIZE">>, <<"RETR">>, <<"STOR">>, <<"APPE">>, <<"CWD">>,
     <<"PWD">>,  <<"STRU">>, <<"PASV">>, <<"PORT">>, <<"EPSV">>,
     <<"EPRT">>, <<"LIST">>, <<"NLST">>, <<"REIN">>, <<"MKD">>,
     <<"RMD">>,  <<"DELE">>, <<"RNFR">>, <<"RNTO">>, <<"MODE">>,
     <<"FEAT">>, <<"OPTS">>, <<"ABOR">>].

epsv_reject_commands() ->
     [<<"PASV">>, <<"PORT">>, <<"EPRT">>].


%% Extensions returned by FEAT command 
extensions() ->
    [<<"BLA x*;y*;z*;">>, <<"EPSV">>, <<"EPRT">>, <<"AUTH TLS">>, <<"PBSZ">>, <<"PROT">>].

%% Commands configurable by OPTS comand
opts_enabled() ->
    [{"BLA", ["x;", "y;", "z;"]}].


%% Supported security protocols

supported_security_protocols() ->
    ["TLS"].

protection_levels() ->
    ["P"].


concat_extensions()-> concat_extensions(extensions()).
concat_extensions(Extensions)->
    StrList = [" "++binary_to_list(E)++"\r\n" || E <- Extensions],
    string:join(StrList, "").
    
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Checks if given command meets current authentication phase.
%%% ----------------------------------------------------------
-spec check_auth(Command :: bitstring(), Args :: #ctrl_conn_data{}) -> ok | nok | bad.
%%% ###=====================================================================###
check_auth(Command, Args) ->
    ?LOG("Command is: ~p~n", [Command]),
    ImplementedCommands   = lists:member(Command, implemented_commands()),
    NonAuthCommands = lists:member(Command, non_auth_commands()),
    AuthCommand = lists:member(Command, auth_command()),
    PBSZCommand = lists:member(Command, pbsz_command()),
    PROTCommand = lists:member(Command, prot_command()),
    USERCommand = lists:member(Command, user_command()),
    OtherCommands = lists:member(Command, other_commands()),
    EPSVRejectCommands = lists:member(Command, epsv_reject_commands() ),
    AuthPhase = Args#ctrl_conn_data.auth_phase,
    case AuthPhase of
        %% Waiting for AUTH TLS, commands not requring auth also allowed
        none -> 
            case {ImplementedCommands, NonAuthCommands orelse AuthCommand} of
	            {false, _} -> ok;
	            {true, false} -> bad;
	             _		     -> ok
            end;
        %% AUTH TLS issued, next command must come after TLS negotiation
        started -> 
            nok;
        %% TLS negotiation done, waiting for [pbsz, prot] or user (both sequences supported)
        negotiation_done ->
            case PBSZCommand orelse USERCommand of
                true -> ok;
                false -> nok
            end;
        %% PBSZ done, expecting PROT command
        pbsz_done ->
            case PROTCommand of
                true -> ok;
                false -> nok
            end;
        %% PROT done, waiting for USER
        prot_done ->
            case USERCommand of
                true -> ok;
                false -> bad
            end;
        %% Sequence completed, client authenticated
        valid_seq ->
            case {ImplementedCommands, OtherCommands} of
                {false, _} -> ok;
                {true, true} -> ok;
                _ -> nok
            end;
        epsv_spec_phase ->
            case EPSVRejectCommands of
                true -> nok;
                false -> ok
            end
    end.

%% Construct tuple for response (special clause for FEAT command)
-spec response(ReplyCode :: integer(), Message :: string() | {feat, string()}) -> reply().
response(ReplyCode, Message) -> {reply, ReplyCode, Message}.

%% Convert Code and Message to packet and send
send_reply(Sock, 211, {feat, Message})  ->
    Str = integer_to_list(211) ++ "-" ++ Message ++ "\r\n",
    send_message(Sock, Str);

send_reply(Sock, Code, Message) ->
    Str = integer_to_list(Code) ++ " " ++ Message ++ "\r\n",
    send_message(Sock, Str).

%% Send raw message
send_message(Sock, Str) when is_port(Sock) ->
    %% sysInitI:info_msg("[~p-Send]: ~p\n", [Sock, Str]),
    ?LOG("[~p-Send]: ~P~n", [Sock, Str, 80]),
    gen_tcp:send(Sock, Str);

send_message(Sock, Str) when is_tuple(Sock) ->
    %% sysInitI:info_msg("[~p-Send]: ~p\n", [Sock, Str]),
    ?LOG("[~p-Send]: ~P~n", [Sock, Str, 80]),
     ssl:send(Sock, Str).

%% Get file information
%% drwxrwsr-x   3 47688    60000	4096 Dec-9-2005 empty
get_file_info(FullPath) ->
    ?LOG("FULL DIRECTORY PATH -------- ~p~n", [FullPath]),
    FileInfo =
    case file:read_file_info(FullPath) of
        {ok, Res} ->
            Res;
        {error, enoent} -> %% symlink
            get_link_info(FullPath)
    end,
    
    Size  = FileInfo#file_info.size,
    Type  = FileInfo#file_info.type,
    Mode  = FileInfo#file_info.mode,
    Links = FileInfo#file_info.links,
    UID   = FileInfo#file_info.uid,
    GID   = FileInfo#file_info.gid,
    {{MY,MM,MD}, {MH,MMin,_MS}} = FileInfo#file_info.mtime,

    Time = lists:concat(case MY < current_year() of
	true  -> [MY];
	false -> [MH,":",MMin]
    end),

    lists:concat([get_type_letter(Type),get_modes(Mode),
	" ",Links," ",UID," ",GID," ",Size," ",
	httpd_util:month(MM)," ",MD," ",Time," ",filename:basename(FullPath)]).

get_link_info(FullPath) ->
    {ok, LinkInfo} = file:read_link_info(FullPath),
    LinkInfo.

get_type_letter(device)    -> "b";
get_type_letter(directory) -> "d";
get_type_letter(regular)   -> "-";
get_type_letter(symlink)   -> "l";
get_type_letter(other)     -> "-";
get_type_letter(_)	   -> "-".

current_year() ->
    {{Year,_,_}, _} = calendar:local_time(),
    Year.

get_modes(Mode) ->
    lists:concat([get_rights(Mode, 1 bsl 6), get_rights(Mode, 1 bsl 3), get_rights(Mode, 1)]).

get_rights(Mode, User) ->
    lists:concat([check_band(Mode, User bsl 2, "r"), check_band(Mode, User bsl 1, "w"), check_band(Mode, User, "x")]).

check_band(A, B, R) ->
    case A band B of
	0 -> "-";
	_ -> R
    end.

get_full_path(Args) ->
    get_full_path(Args#ctrl_conn_data.curr_path, Args).

get_full_path(RelPath, Args) ->
    FullPath = concat_paths(Args#ctrl_conn_data.chrootdir, RelPath),
    ?LOG("Full path: ~p~n", [FullPath]),
    FullPath.

concat_paths(P1, P2) ->
    Temp = P1 ++ "/" ++ P2,
    re:replace(Temp, "\/+", "\/", [{return, list}, global]).

%% CRLF transformation from ASCII to own representation
transformfrom(Bin, _) ->
    Bin.

%% CRLF transformation from own representation to ASCII
transformto(Bin, ["A"]) ->
    Step1 = re:replace(Bin, "\r\n", "\n", [{return, binary}, global]),
    re:replace(Step1, "\n", "\r\n", [{return, binary}, global]);
transformto(Bin, _) ->
    Bin.


%% Conversion between string list and IP/Port tuple
list2portip(Lst) when length(Lst) == 6 ->
    Fun = fun(A) -> {Res, _} = string:to_integer(A), Res end,
    [A1,A2,A3,A4,P1,P2] = [ Fun(X) || X <- Lst ],
    case lists:member(error,[A1,A2,A3,A4,P1,P2]) of
	false ->
%%  	    <<Port:32>> = <<P1:16, P2:16>>,
Port = P1*256 + P2,
	    {ok, {{A1,A2,A3,A4}, Port}};
	true ->
	    {error, bad_addr}
    end;
list2portip(_) ->
    {error, bad_addr}.

eprtlist2portip(["1", SAddr, SPort]) ->
    case {inet:parse_ipv4_address(SAddr), string:to_integer(SPort)} of
	{{ok, IP}, {Port, []}} ->{ok, {IP, Port, inet}};
	_Error		       -> {error, bad_addr}
    end;
eprtlist2portip(["2", SAddr, SPort]) ->
    case {inet:parse_ipv6strict_address(SAddr), string:to_integer(SPort)} of
    {{ok, IP}, {Port, []}} -> {ok, {IP, Port, inet6}};
    _Error             -> {error, bad_addr}
    end;

eprtlist2portip(_) ->
    {error, bad_addr}.

get_server_ip() ->
    {ok, Name} = inet:gethostname(),
    case inet_res:gethostbyname(Name) of
	{ok, HostInfo} -> {ok, hd(HostInfo#hostent.h_addr_list)};
	{error, _}     -> inet:getaddr(Name, inet)
    end.

getaddr(Addr) ->
    case inet:getaddr(Addr,inet) of
	{error, _} -> inet:getaddr(Addr,inet6);
	Res	   -> Res
    end.

check_dir_in_chroot(RootDir, FullPath) ->
    check_dir_in_chroot(RootDir, FullPath, 0).

check_dir_in_chroot(_, _, 32) ->
    false; % circular symlinks
check_dir_in_chroot(RootDir, FullPath, LinkNum) ->
    case lists:prefix(RootDir, FullPath) of
        false ->
        	% link points to the outside
        	false;
        true ->
        	case file:read_link(FullPath) of
        	{error, _} ->
        	    % not a symlink, path is OK
        	    true;
        	{ok, LinkedTo} ->
        	    check_dir_in_chroot(RootDir, LinkedTo, LinkNum+1)
        	end
    end.

sec_log(SrcIp, Msg) ->
    logI:write_log("SecurityLog",  SrcIp, "1", 4, 
           info, os:timestamp(), Msg).

ip_to_string(Any)->
    case inet:ntoa(Any) of
    {error, einval} ->
    sysInitI:warning_msg(
        "~p: Failed to convert IP address: ~p to string~n",
        [?MODULE, Any]),
        "invalid address";
    Good ->
        Good
    end.
 
get_verify_fun(TcatDn, TlsOptions) ->
    case certI:mk_verify_fun(TcatDn, self()) of
    {ok, VerifyFun, PartialFun} ->
        {ok, lists:map(fun({verify_fun, _}) -> VerifyFun;
                     ({partial_chain, _}) -> PartialFun;
                     (Other) -> Other end, TlsOptions)};
    {error, Reason} ->
        {error, Reason}
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc If given path is absolute, current directory is root else fetch from state
%%% ----------------------------------------------------------
-spec get_current_dir(Path :: list() | bitstring(), Args :: #ctrl_conn_data{}) -> list().
%%% ###=====================================================================###
get_current_dir([$/ | _RelPath], _Args) ->
    %% absolute path -> root dir
    "/";
get_current_dir(Path, Args) when is_binary(Path)->
    get_current_dir(binary_to_list(Path), Args);
    
get_current_dir(_Path, Args) ->
    %% relative path -> current dir
    Args#ctrl_conn_data.curr_path.

%%% ###########################################################################
% Returns value from record UserData
%%% ###########################################################################
get_ftpes_session_info(UserData) ->
 [{address, UserData#user_data.peer_address}, {port, UserData#user_data.peer_port},
  {user, UserData#user_data.peer_user}, {username, UserData#user_data.username}].

get_ftpes_session_info(UserData, user) ->
   UserData#user_data.peer_user;

get_ftpes_session_info(_UserData, _Other) ->
    undefined.

get_ftpes_session_info(UserData, user, _Default) ->
    UserData#user_data.peer_user;
get_ftpes_session_info(_UserData, _Other, Default) ->
    Default.




