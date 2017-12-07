%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysNetloaderTls.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R4A/R5A/R11A/3
%%% @doc == TLS fileserver to serve NL on reqular nodes (cluster) ==
%%% @end
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(sysNetloaderTls).
-behaviour(gen_server).
-vsn('/main/R4A/R5A/R11A/3').
-date('2017-10-17').
-author('etxberb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% Rev     Date     Name      What
%%% -----   -------  --------  ------------------------
%%% R4A/1   150527   etxlg     Created, works, checked in for weekend
%%% ----------------------------------------------------------
%%% R5A/1   151117   etxpeno   remove dead code
%%% -----   -------    --------   ------------------------
%%% R11A/1  170908   ebabmat   Handle for fix on CERT for HW12922 and HW21881
%%% R11A/3  2017-10-17 etxberb    Adaptions to OTP20; Removed export_all.
%% ----------------------------------------------------------
%%%-compile([export_all]).

%% API
-export([start_link/0]).
-export([run_server/1, stop_server/1]).
-export([set_cxp_file/1, set_mp_contents/1, set_params/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([send/3]).

-define(SERVER, ?MODULE).

-record(sts,
	{
	  listen_port =  59, % "any private file service @IANA"
	  root = "/tmp",
	  cxp_to_file_map = [],
	  mpid_to_contents_map = [], %serve contents based on mpid
	  contents_name = "contents.txt",
	  default_contents = <<>>,
	  server_ip, %{Ip1,Ip2,Ip3,Ip4}, this is bound by the server
	  sock,
	  acceptor_pid
	}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec run_server(tuple()) -> ok | {error, any()}.
run_server(Ip) ->
    gen_server:call(?SERVER, {run_server, Ip}, 30000).

-spec stop_server(tuple()) -> ok | {error, any()}.
stop_server(Ip) -> %only one supported, Ip-arg is therefor not used
    gen_server:call(?SERVER, {stop_server, Ip}).

%-spec set_cxp_file({string(), string()}) -> ok | {error, any()};
      %set_cxp_file([{string(), string()}]) -> ok | {error, any()}.
set_cxp_file({Cxp, File}) ->
    set_cxp_file([{Cxp, File}]);
set_cxp_file(Tuple_list) when is_list(Tuple_list) ->
    gen_server:call(?SERVER, {set_cxp_file, Tuple_list}, 30000).

%-spec set_mp_contents({integer(), list()}) -> ok | {error, any()}
    %; set_mp_contents([{integer(), list()}]) -> ok | {error, any()}.
set_mp_contents({Mp_id, Mp_content}) ->
    set_mp_contents([{Mp_id, Mp_content}]);
set_mp_contents(Tuple_list) when is_list(Tuple_list) ->
    gen_server:call(?SERVER, {set_mp_contents, Tuple_list}).

% Available properties:
% {port, Listen_port_number:: integer()} The server listens here (59 is default)
% {root, Filesystem_root :: string()} Files are read here if not in cxp-map
% {default_contents, [Filename1, Filename2...FilenamN]} i.e. contents.txt
% {contents_name, string()} Match this filename to serve contents
-spec set_params([{Key :: atom(), Value :: any()}]) -> ok | {error, any()}.
set_params(Tuple_list) when is_list(Tuple_list) ->
    gen_server:call(?SERVER, {set_params, Tuple_list}, 30000).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #sts{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({run_server, Ip}, _From, S) ->
    {_ , New_s} = stop_server(S#sts.server_ip, S),
    {Reply, Last_s} = run_server(Ip, New_s),
    {reply, Reply, Last_s};
handle_call({stop_server, Ip}, _From, S) ->
    {Reply, New_s} = stop_server(Ip, S),
    {reply, Reply, New_s};
handle_call({set_cxp_file, Changes}, _From,
	    #sts{cxp_to_file_map = Keylist} = S) ->
%HERE need to restart any running acceptor -> new data
    New_keylist = update_key_list(set_cxp_file, Changes, Keylist),
    {reply, ok, S#sts{cxp_to_file_map = New_keylist}};
handle_call({set_mp_contents, Changes}, _From,
	    #sts{mpid_to_contents_map = Keylist} = S) ->
%HERE need to restart any running acceptor -> new data
    New_keylist = update_key_list(set_mp_contents, Changes, Keylist),
    {reply, ok, S#sts{mpid_to_contents_map = New_keylist}};
handle_call({set_params, Tuple_list}, _From, S) ->
%HERE need to restart any running acceptor -> new data
    Listen_port = proplists:get_value(port, Tuple_list, S#sts.listen_port),
    Root = proplists:get_value(root, Tuple_list, S#sts.root),
    Default_contents = proplists:get_value(default_contents, Tuple_list,
					   S#sts.default_contents),
    Contents_name = proplists:get_value(contents_name, Tuple_list,
					   S#sts.contents_name),
    Socket =
	if
	    Listen_port =:= S#sts.listen_port ->
		S#sts.sock;
	    true ->
		S#sts.sock %HERE FIXME close old, start new
	end,
    {reply, ok, S#sts{listen_port = Listen_port,
			  root = Root,
			  sock = Socket,
			  contents_name = Contents_name,
			  default_contents = Default_contents}};
handle_call(_, _, S) ->
    warn_msg("Unrecognized call", []),
    {reply, ok, S}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    warn_msg("Unrecognized cast", []),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', _Ref, process, Pid, normal},
	    #sts{acceptor_pid = Pid} = S) ->
%HERE MORE stuff needed to avoid patological restarting
    Apid = run_acceptor(S),
    {noreply, S#sts{acceptor_pid = Apid}};


handle_info(_Info, State) ->
    warn_msg("Unrecognized info: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
% -> {Reply, New_s}
stop_server(_Ip, S) ->
    case S#sts.sock of
	undefined ->
	    {not_running, S#sts{server_ip = undefined}};
	Socket ->
	    ssl:close(Socket),
	    catch exit(S#sts.acceptor_pid, normal),
	    {ok, S#sts{sock = undefined,
		       server_ip = undefined,
		       acceptor_pid = undefined}}
    end.

% -> {Reply, New_s}
run_server(Ip, S) ->
    case  get_all_tls_opts(Ip, S) of
	{ok, Opts} ->
	    case ssl:listen(S#sts.listen_port, Opts) of
		{ok, Sock} ->
		    New_s = S#sts{sock = Sock, server_ip = Ip},
		    Apid = run_acceptor(New_s),
		    {ok, New_s#sts{sock = Sock, server_ip = Ip,
				   acceptor_pid = Apid}};
		{error, What} ->
		    warn_msg("Listen: Failed to run TLS service: ~p", [What]),
		    {ok, S}
	    end;
	{error, What} ->
	    warn_msg("Options: failed to run TLS service: ~p", [What]),
	    {ok, S}
    end.

run_acceptor(S) ->
    Pid = proc_lib:spawn_link(fun() -> acceptor(S) end),
    monitor(process, Pid),
    Pid.

% ->  {ok, [Opts]} | {error, any()}
get_all_tls_opts(Ip, _S) ->
    case get_cert() of
	{ok, {Node_cert, Node_key, Ca_certs}} ->
	    {ok,
		case sysInitI:is_secure() of
		% case true of
		    true ->
			%Need som verify_fun stuff too?
			[
			 {mode, binary}, {packet, 4}, {reuseaddr, true},
			 {ip, Ip},
			 {cert, Node_cert}, {key, Node_key},
			 {cacerts, Ca_certs},
			 {secure_renegotiate, true},
			 {versions, [tlsv1, 'tlsv1.2']},
			 {depth, 10}, % 1 is the default
			 {verify, verify_peer}, {fail_if_no_peer_cert, true},
			 {partial_chain, get_pfun(Ca_certs)}
			];
		    false ->
			warn_msg("Insecure node, TLS service not verified", []),
			[
			 {mode, binary}, {packet, 4}, {reuseaddr, true},
			 {ip, Ip},
			 {cert, Node_cert}, {key, Node_key},
			 {secure_renegotiate, true},
			 {versions, [tlsv1, 'tlsv1.2']}
			]
		    end
	    };
	{error, What} ->
	    {error, What}
    end.

get_pfun(Ca_certs) ->

    fun(Client_presents) when is_list(Client_presents) ->
	 io:format("pfun: list of len: ~b~n", [length(Client_presents)]),
	 case lists:filtermap(
		fun(Cp) ->
		    case lists:member(Cp, Ca_certs) of
			true -> {true, {trusted_ca, Cp}};
			false -> false
		    end
		end, Client_presents) of
	    [] ->
		io:format("pfun: no match from filtermap -> unknown_ca~n"),
		unknown_ca;
	    Maybe_several ->
		io:format("pfun: filtermap  matched: ~b cert/s~n",
			  [length(Maybe_several)]),
		hd(Maybe_several)
	end;
       (_) ->
	 io:format("pfun: NOT a list -> unknown_ca~n"),
	 unknown_ca
    end.

get_cert() ->
    case catch certSecStore:get_vc() of
	{ok, VcCertPem, VcKeyPem} ->
	    case {public_key:pem_decode(VcKeyPem),
		  public_key:pem_decode(VcCertPem)} of
                {[], _} ->
                    {error, "VC found but key cannot be decoded"};
                {_, []} ->
                    {error, "VC found but cert cannot be decoded"};
                {[{KeyInfo, VcKeyData, _}],  [{'Certificate', VcCert, _} | T]} ->
                    VcKey = {KeyInfo, VcKeyData},
                    Chain = [Cert || {'Certificate', Cert, _} <- T],
                    {ok, {VcCert, VcKey, Chain}};
                _Other ->
                    {error, "VC found but can not decode"}
            end;
        not_found ->
            {error, "VC not found"};
        {error, timeout} ->
            {error, try_again}
    end.


update_key_list(Operation, Change_list, Current_list) ->
    lists:foldl(
	fun({Key, ""}, Acc) -> %empty file remove it from map
		lists:keydelete(Key, 1, Acc);
	   ({Key, L} = E, Acc) when is_list(L) ->
		case lists:keytake(Key, 1, Acc) of
		    false ->
			[E | Acc];
		    {value, {_Key, _Value}, New_acc} ->
			[E | New_acc]
		end;
	   (Err, Acc) ->
		warn_msg("Bad input data to ~p: ~p", [Operation, Err]),
		Acc
	end,
	    Current_list, Change_list).

warn_msg(Format, Args) ->
    sysInitI:warning_msg("~w: " ++ Format ++ "~n", [?MODULE | Args]).
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: " ++ Format ++ "~n", [?MODULE | Args]).


%This is spawned(-linked) in its own process
acceptor(S) ->
    case ssl:transport_accept(S#sts.sock) of
	{ok, Socket} ->
	    case ssl:ssl_accept(Socket, 5000) of
		ok ->
		    %get the IP, it could be used to look up MPID in
		    %sysDhcpcD, now we get it in the HELLO
		    {ok, {_Ip, _Port}} = ssl:peername(Socket),
		    info_msg("Enter acceptor_loop Ip: ~p", [_Ip]),
	    	    acceptor_loop(S, Socket, undefined);
		{error, What} ->
		    warn_msg("Failed ssl accept: ~p", [What]),
		    %HERE we should maybe restart the socket
		    exit(normal)
	    end;
	{error, What} ->
	    %HERE we should maybe restart the socket
	    warn_msg("Failed transport accept: ~p", [What]),
	    exit(normal)
    end.

%must be same as in the NL
-define(TLSTRANSFERVERSION, 1).
-define(HELLO, 1).
-define(ITEM, 2).
-define(BIN, 3).
-define(DONE, 128).
-define(ERROR, 250).
acceptor_loop(S, Socket, Mp_id) ->
    info_msg("Acceptor loop Mp_id: ~p", [Mp_id]),
    receive
	{ssl, Socket, Bin} ->
	    handle_tls(S, Socket, Mp_id, Bin);
	{ssl_closed, Socket} ->
	    info_msg("Open TLS socket closed unexpectedly", []),
	    exit(normal);
	{ssl_error, Socket, What} ->
	    info_msg("Open TLS socket error: ~p", [What]),
	    exit(normal);
	Any ->
	    info_msg("Got: ~p", [Any]),
	    acceptor_loop(S, Socket, Mp_id)
    end.

handle_tls(S, Socket, undefined,
	   <<(?TLSTRANSFERVERSION):8, (?HELLO):8, Mp_id:8>>) ->
    info_msg("Got HELLO from MP_ID: ~b", [Mp_id]),
    acceptor_loop(S, Socket, Mp_id);
handle_tls(S, Socket, Mp_id,
	   <<(?TLSTRANSFERVERSION):8, (?ITEM):8, Filename/binary>>) ->
    File = binary_to_list(Filename),
    info_msg("Got get ITEM from MP_ID: ~b for file ~p", [Mp_id, File]),
    serve(S, Socket, Mp_id, File);
handle_tls(S, Socket, Mp_id,
	   <<(?TLSTRANSFERVERSION):8, (?BIN):8, Filename/binary>>) ->
    File = binary_to_list(Filename),
    info_msg("Got get BIN from MP_ID: ~b for file ~p", [Mp_id, File]),
    transfer(S, Socket, Mp_id, File).

serve(#sts{contents_name = File} = S, Socket, Mp_id, File) ->
    case lists:keyfind(Mp_id, 1, S#sts.mpid_to_contents_map) of
	{Mp_id, Contents_list} ->
	    info_msg("Serving custom contents to MPid: ~b", [Mp_id]),
	    serve_content(Socket, Contents_list);
	false ->
	    case S#sts.default_contents of
		<<>> ->
		    warn_msg("No contents to serve for Mp_id: ~p - exit",
			     [Mp_id]),
%HERE send ?ERROR
		    exit(normal);
		Contents_list ->
		    info_msg("Serving default contents to MPid: ~b", [Mp_id]),
		    serve_content(Socket, Contents_list)
	    end
    end;
serve(_S, _Socket, _Mp_id, File) ->
info_msg("Duh: ~p", [File]),
exit(normal).

serve_content(Socket, List) ->
    info_msg("Serving: ~p", [List]),
    [ok = send_data(Socket, ?ITEM, Line) || Line <- List],
    ok = send_done(Socket),
    ssl:close(Socket),
    exit(normal).

%this where we open a local file and feed it to the client
transfer(S, Socket, _Mp_id, File) ->
    case lists:keyfind(File, 1, S#sts.cxp_to_file_map) of
	{File, Path} ->
	    transfer_file(S, Socket, Path);
	false ->
	    case S#sts.root of
		R when not is_list(R) ->
		    info_msg("Nothing to serve root is not set", []),
		    err_resp(Socket, "File not found - server config"),
		    exit(normal);
		R when  R =:= ""; R =:= "/" ->
		    info_msg("Nothing to serve root is incorrectly set", []),
		    err_resp(Socket, "File not found - server config"),
		    exit(normal);
		R ->
		    Path = filename:join([R, File]),
		    transfer_file(S, Socket, Path)
	    end
    end.

transfer_file(S, Socket, Path) ->
    send(S, Socket, file:open(Path, [read, raw, binary])).

-define(READBLOCKSIZE, (128 * 128)).
send(S, Sock, {ok, Io}) ->
    case file:read(Io, ?READBLOCKSIZE) of
	{ok, Bin} ->
	    ok = send_data(Sock, ?BIN, Bin),
	    send(S, Sock, {ok, Io});
	eof ->
	    ok = send_done(Sock),
	    ssl:close(Sock),
	    exit(normal);
	{error, What} ->
	    warn_msg("File read error: ~p", [What]),
	    err_resp(Sock, "File read error"),
	    exit(normal)
    end;
send(_, Sock, Err) ->
    warn_msg("Failed to open file: ~p", [Err]),
    err_resp(Sock, "File not found - open error").


send_data(Sock, Type, List) when is_list(List) ->
    send_data(Sock, Type, list_to_binary(List));
send_data(Sock, Type, Bin) ->
    ssl:send(Sock, <<(?TLSTRANSFERVERSION):8, Type:8, Bin/binary>>).

send_done(Sock) ->
    ssl:send(Sock, <<(?TLSTRANSFERVERSION):8, (?DONE):8>>).

err_resp(Sock, Message) when is_list(Message) ->
    err_resp(Sock, list_to_binary(Message));
err_resp(Sock, Message) when is_binary(Message) ->
    ssl:send(Sock, <<(?TLSTRANSFERVERSION):8, (?ERROR):8, Message/binary>>).
