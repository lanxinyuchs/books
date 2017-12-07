%% ===========================================================================
%% Copyright (c) Ericsson AB 2013 All rights reserved.
%%
%% The information in this document is the property of Ericsson.
%%
%% Except as specifically authorized in writing by Ericsson, the
%% receiver of this document shall keep the information contained
%% herein confidential and shall protect the same in whole or in
%% part from disclosure and dissemination to third parties.
%%
%% Disclosure and disseminations to the receivers employees shall
%% only be made on a strict need to know basis.
%% ===========================================================================
%% @copyright Ericsson AB 2014-2017
%% @doc
%% This module handles emergency access callbacks for type 3.
%%
%% @end
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev      Date      Name      What
%%% -----    -------   --------  ------------------------
%%% R2A/1    13-11-18  etxtory   Created
%%% R2A/3    14-08-18  etxtory   Added reset to factory via ea-Gui
%%% R2A/4    14-09-09  etxtory   DSCP added
%%% R2A/5    14-09-09  etxtory   HS92124
%%% R2A/6    14-09-23  etxpejn   Added call to sysSftp TR HS89846
%%% R2A/7    14-09-29  etxtory   HS95395
%%% R2A/7    14-09-29  etxtory   HT13787
%%% ----------------------------------------------------------
%%% R3A/1    15-01-08  etxtory   Factory reset renamed to board restore
%%% R3A/2    15-01-09  etxtory   Keep factory reset also (for curl-calls)
%%% R3A/3    15-02-16  etxtory   Directory instead of filename for ESI
%%%                              Handle redirect to https
%%% R3A/4    15-02-27  etxtory   Remove tmp esi-directory and esi-file
%%% R3A/5    15-03-31  etxlg     LMT IP check compatible with namespaced LMT
%%% R3A/6    15-05-07  etxtory   Added real factory reset needed by produktion
%%% R3A/7    15-05-08  etxtory   Changed factory reset "string"
%%% R3A/8    15-05-22  etxtory   enetunreach/nxdomain added
%%% R4A/1    15-09-09  etxpejn   export_esi/4: Fetch ESI for all MPs in a cluster
%%% R4A/3    15-09-16  etxlg     Alternate connection
%%% R4A/4    15-09-18  etxtory   logEsi interface updates
%%% R4A/5    15-09-21  etxtory   dialyze error
%%% R4A/6    15-10-03  etxtory   Add port as option
%%% ----------------------------------------------------------
%%% R5A/1    15-11-02  etxtory   Logging factory reset on core node
%%% R5A/3    15-12-14  etxtory   Merged R4A/8 (IPv6)
%%% R5A/4    16-01-18  etxtory   redirect kills process in OTP 18.2.2 (inets-6.1)
%%% R5A/5    16-02-19  etxtory   EsiFile is including all extension (.gz)
%%% R5A/8    16-04-08  etxtory   Update map_error
%%% ----------------------------------------------------------
%%% R6A/1    16-05-16  etxtory   Crash printout when factory reset
%%% ----------------------------------------------------------
%%% R10A/1   17-04-26  evanbel   Added possibility of IPv6 host when exporting ESI
%%% ----------------------------------------------------------
%%% R11A/1   17-07-20  ekastoj   Check input before exporting ESI
%%% R11A/2   17-07-21  ekastoj   Fixed unused variables
%%% ----------------------------------------------------------
-module(aicGui).
-export([post/3]).

%% Internal function (called in own process using proc_lib)
-export([export_esi/6]).
-export([factory_reset/1]).

-define(SSH_TIMEOUT, 10000).          %% connect_timeout
-define(SSH_TIMEOUT_WRITE, 60000).    %% write timeout
-define(SSH_TIMEOUT_LONG, 300000).    %% session timeout
-define(SSH_PORT, 22).                %% Standard ssh-port
-define(ESI_RESULT_FILE, "export_esi_result.txt"). %% Result file for EA
-define(LMT_IF, "eth0").

%%% ----------------------------------------------------------
%%% post(SessionId, Env, Input)
%%%       -> ok
%%% where
%%%    SessionId = Session Identity
%%%    Env = Http environment
%%%    Input = Input data from post
%%%
%%% Callback from OTP webserver. Handles the following AI-GUI events:
%%%   - Factory reset (reset the node/board to factory reset; back to NL)
%%%     Only allowed from LMT.
%%%   - Export ESI (generate and exports an ESI to sftp-server)
%%% 
%%% redirect/2 will terminate the "post" process and the SessionId process.
%%% Due to this, the code after redirect/2 needs an own process.
%%% Note: the terminate behaviour was introduced in inets-6.1 (OTP version in 16B).
%%% ----------------------------------------------------------
post(SessionId, Env, []) ->
    redirect(SessionId, Env);

post(SessionId, Env, "DoHardFactoryReset=HardFactoryReset") ->
    factory_reset(SessionId, Env, _Grade = hard);

%% Interface to be backward compatible (labs using curl)
%% Will do the same as "DoBoardRestore=BoardRestore"
post(SessionId, Env, "DoFactoryReset=FactoryReset") ->
    factory_reset(SessionId, Env, _Grade = soft);

post(SessionId, Env, "DoBoardRestore=BoardRestore") ->
    factory_reset(SessionId, Env, _Grade = soft );

post(SessionId, Env, Input) ->
    case catch parse_input(Input) of
	{ok, {esi, Host, Port, Username, Password, DirOrFile, _Time}} ->
            update_progress(start),
	    IsAltCon = is_alt_connection(Env),
	    DestDir =
		    case DirOrFile of
	  	      {filename, File} ->
			    filename:dirname(File);
	    	      {directory, Dir} ->
		            Dir
		    end,
	    case catch sysSftp:start_channel(Host, Port, Username, Password) of
		{ok, Pid, ConnRef} ->
	    		case catch ssh_sftp:opendir(Pid, DestDir) of
				{ok, _} ->
	    				ssh_sftp:stop_channel(Pid),
	    				ssh:close(ConnRef),
	   				proc_lib:spawn(?MODULE, export_esi, [IsAltCon, Host, Port, Username, Password, DirOrFile]),
	    				redirect(SessionId, Env);
				{error, Reason} ->
					Es = map_error(Reason),
					update_progress(nok, Reason, Es),
	    				redirect(SessionId, Env)
			end;
		{error, Reason} ->
	    		Es = map_error(Reason),
			update_progress(nok, Reason, Es),
		        redirect(SessionId, Env);
		{'EXIT', Reason} ->
	    	 	Es = map_error(Reason),
			update_progress(nok, Reason, Es),
	   	        redirect(SessionId, Env)
    	    end,
	    redirect(SessionId, Env);
        {ok, {export, Host, Port, Username, Password, File, _Time}} ->
            info_msg("Triggered export callback~n", []),
             case export_log(Host, Port, Username, Password, File) of
                ok ->
                    info_msg("Export autointegration log done~n", []);
                {error, Reason} ->
                    info_msg("Export autointegration log failed ~s~n", [Reason])
            end,
            redirect(SessionId, Env, "/aicomplete.html");
	{error, Reason} ->
	    update_progress(nok, Reason, Reason),
	    redirect(SessionId, Env);
	{'EXIT', Reason} ->
	    update_progress(nok, Reason, "internal error"),
	    redirect(SessionId, Env)
    end.

factory_reset(SessionId, Env, Grade) ->
    case catch is_allowed_to_factory_reset(Env) of
	true ->
	    info_msg_both("Reset to factory with Grade ~p on node ~p~n", [Grade, clhI:erlang_node()]),
	    proc_lib:spawn(?MODULE, factory_reset, [Grade]),
	    redirect(SessionId, Env);
	false ->
	    info_msg("Reset to factory not allowed~n", []),
	    redirect(SessionId, Env, "/not_allowed.html");
	Other ->
	    error_msg("Reset to factory not allowed ~n"
		      "Other ~p ~n", [Other]),
	    redirect(SessionId, Env, "/not_allowed.html")
    end.

factory_reset(Grade) ->
    catch sysNetloader:coli_factory_reset(Grade).

%%% ----------------------------------------------------------
%%% This function request the web-browser to do redirect (i.e. a refresh to
%%% the main page (ea.html) or to "not_allowed.html").
%%% ----------------------------------------------------------
redirect(SessionId, Env) ->
    redirect(SessionId, Env, "/ea.html").

redirect(SessionId, Env, Html) ->
    Redirect =
	case lists:keyfind(http_host, 1, Env) of
	    {http_host, HttpHost} ->
		PStr = get_protocol(Env),
		["<html>",
		 "<body>",
		 "<meta http-equiv=\"refresh\" content=\"0; url=" ++ PStr ++ HttpHost ++ Html ++ "\">",
		 "</body>",
		 "<html>"];
	    false ->
		["<html>",
		 "<body>",
		 "<p>Please reload!</p>",
		 "</body>",
		 "<html>"]
	end,
    mod_esi:deliver(SessionId, Redirect).

%% http:// or https://
get_protocol(Env) ->
    case lists:keyfind(server_port, 1, Env) of
	{server_port, Port} ->
	    HttpsPorts = [sysEnv:get_port_conf(https),
			  sysEnv:get_port_conf(https_login)],
	    case lists:member(Port, HttpsPorts) of
		true ->
		    "https://";
		false ->
		    "http://"
	    end;
	false ->
	    "https://"
    end.

%%% ----------------------------------------------------------
%%% Parsing the input
%%% ----------------------------------------------------------
parse_input(Input) ->
    Pq = httpd:parse_query(Input),
    Pi = parse_input(Pq,  [_Method = false,
			   _Host = false,
			   _Username = false,
			   _Passwd = false,
			   _Dir = false,
			   _UtcMs = undefined]),
    check_input(Pi).

%% ------ End clause --------------------------------------------------------
parse_input([], Acc) -> Acc;

%% ------ Methods -----------------------------------------------------------
parse_input([{"DoESI", _} | T], [_Method, Host, Username, Passwd, Dir, UtcMs]) ->
    parse_input(T, [esi, Host, Username,Passwd, Dir, UtcMs]);

parse_input([{"DoExport", _} | T], [_Method, Host, Username, Passwd, Dir, UtcMs]) ->
    parse_input(T, [export, Host, Username,Passwd, Dir, UtcMs]);

%% ------ Parameters --------------------------------------------------------
parse_input([{"filename", File} | T], [Method, Host, Username, Passwd, _Dir, UtcMs]) ->
    %% "filename" is replaced by "directory"; "filename" left to be backward compatible
    parse_input(T, [Method, Host, Username, Passwd, {filename, File}, UtcMs]);
parse_input([{"directory", Dir} | T], [Method, Host, Username, Passwd, _Dir, UtcMs]) ->
    parse_input(T, [Method, Host, Username, Passwd, {directory, Dir}, UtcMs]);
parse_input([{"host", Host} | T], [Method, _Host, Username, Passwd, Dir, UtcMs]) ->
    parse_input(T, [Method, Host, Username, Passwd, Dir, UtcMs]);
parse_input([{"password", Passwd} | T], [Method, Host, Username, _Passwd, Dir, UtcMs]) ->
    parse_input(T, [Method, Host, Username, Passwd, Dir, UtcMs]);
parse_input([{"username", Username} | T], [Method, Host, _Username, Passwd, Dir, UtcMs]) ->
    parse_input(T, [Method, Host, Username, Passwd, Dir, UtcMs]);

%% ------ Unknown -----------------------------------------------------------
parse_input([_ | T], Acc) ->
    parse_input(T, Acc).

check_input([Method, Host, Username, Passwd, Dir, UtcMs] = Pi) ->
    case {lists:member(false, Pi), lists:member([], Pi)} of
	{true, _} ->
	    {error, "Host, Username, Password or Directory missing"};
	{_, true} ->
	    {error, "Host, Username, Password or Directory missing"};
	_ ->
            case inet:parse_address(Host) of
                {ok, _} ->
                    {ok, {Method, Host, ?SSH_PORT, Username, Passwd, Dir, UtcMs}};
                {error, _} ->
                    case string:tokens(Host, ":") of
                        [H, Port] ->
                            {ok, {Method, H, list_to_integer(Port), Username, Passwd, Dir, UtcMs}};
                        [Host] ->
                            {ok, {Method, Host, ?SSH_PORT, Username, Passwd, Dir, UtcMs}};
                        _ ->
                            {error, "Incorrect format Host parameter"}
                    end
            end
    end.

%%% ----------------------------------------------------------
%%% Export AI log
%%% ----------------------------------------------------------
export_log(Host, Port, Username, Password, {filename, ToFile}) ->
    case aicLog:get_nl_logs() of
        {ok, Bin} ->
            info_msg("succeeded to fetch nl logs~n", []),
            export_log_cont(Host, Port, Username, Password, Bin, ToFile);
        {error, Reason} ->
            info_msg("export_log: failed to fetch nl log, reason: ~p~n", [Reason]),
            {error, "AI log file can not be found"}
    end.

export_log_cont(Host, Port, Username, Password, Bin, ToFile)->
    info_msg("went to cont~n", []),
    Opts = [{timeout, ?SSH_TIMEOUT_LONG},
            {connect_timeout, ?SSH_TIMEOUT}],
    case catch sysSftp:start_channel_with_alt(Host, Port, Username, Password, Opts) of
        {ok, Pid, ConnRef} ->
            info_msg("opened channel~n", []),
            case ssh_sftp:write_file(Pid, ToFile, Bin, ?SSH_TIMEOUT_WRITE) of
                ok ->
                    info_msg("Successfully wrote nl_log ~n", []),
                    IntToFile = ToFile ++ "_ericsson",
                    export_internal_logs(Pid, IntToFile),
                    ssh_sftp:stop_channel(Pid),
                    ssh:close(ConnRef),
                    ok;
                {error, Reason} ->
                    ssh_sftp:stop_channel(Pid),
                    ssh:close(ConnRef),
                    info_msg("export_log: failed to write log file~n", []),
                    {error, Reason}
            end;
        {error, Reason} ->
            info_msg("export_log: failed to open channel: ~p~n", [Reason]),
            {error, Reason};
        {'EXIT', Reason} ->
            info_msg("export_log: exited, reason: ~p~n", [Reason]),
            {error, Reason}
    end.

export_internal_logs(Pid, ToFile)->
    case aicLog:get_internal_logs() of
        {ok, Bin} ->
            case ssh_sftp:write_file(Pid, ToFile, Bin, ?SSH_TIMEOUT_WRITE) of
                ok->
                    ok;
                {error, Reason} ->
                    info_msg("Internal log writing failed, ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            info_msg("Couldn't receive binary data, reason: ~p~n", [Reason])
    end.

%%% ----------------------------------------------------------
%%% Export ESI
%%% ----------------------------------------------------------
export_esi(IsAltCon, Host, Port, Username, Password, DirOrFile) ->
    case catch do_export_esi(IsAltCon, Host, Port, Username,
			     Password, DirOrFile) of
	ok ->
	    update_progress(ready);
	{error, Reason, GuiReason} ->
	    update_progress(nok, Reason, GuiReason);
	{'EXIT', Reason} ->
	    update_progress(nok, Reason, "internal error")
    end.

do_export_esi(IsAltCon, Host, Port, Username, Password, DirOrFile) ->
    %% Only support directory from now
    DestDir =
	case DirOrFile of
	    {filename, File} ->
		filename:dirname(File);
	    {directory, Dir} ->
		Dir
	end,
    
    case logEsi:generate_esi(clhI:own_mp_id(), _Granularity = undefined, _Mode = any) of
	{_Node, Type, EsiFile} when Type =:= limited; Type =:= full->
	    %% ESI is limited or full but no chunks.
	    do_export_esi_cont(IsAltCon, Host, Port, Username, Password, DestDir,
			    {single, EsiFile});
	{_Node, chunk, EsiFile, Chunks} ->
	    %% ESI is not complete
	    EsiFileBase = filename:basename(EsiFile),
	    do_export_esi_cont(IsAltCon, Host, Port, Username, Password, DestDir,
			    {firstchunk, EsiFile, _No = 1, EsiFileBase, Chunks});
	Other ->
	    {error, {"Failed to generate ESI", Other}, "internal error"}
    end.

do_export_esi_cont(IsAltCon, Host, Port, Username, Password, DestDir, GenEsiRes) ->
    case catch start_channel_wrapper(IsAltCon, Host, Port,
				     Username, Password,
				     [{timeout, ?SSH_TIMEOUT_LONG},
				      {connect_timeout, ?SSH_TIMEOUT}]) of
	{ok, Pid, ConnRef} ->
	    Res = upload_files(Pid, DestDir, GenEsiRes, _Acc = []),
	    ssh_sftp:stop_channel(Pid),
	    ssh:close(ConnRef),
	    Res;
	{error, Reason} ->
	    Es = map_error(Reason),
	    {error, {"ssh_sftp:start_channel failed", Reason}, Es};
	{'EXIT', Reason} ->
	    Es = map_error(Reason),
	    {error, {"ssh_sftp:start_channel failed 'EXIT'", Reason}, Es}
    end.

%% Returns: ok | {error, Reason, GuiReason}
upload_files(Pid, DestDir, {single, EsiFile}, _Acc) ->
    Res = upload_file(Pid, EsiFile, DestDir),
    remove_esi(EsiFile),
    Res;
upload_files(Pid, DestDir, {firstchunk, EsiFile, No, EsiFileBase, Chunks}, Acc) ->
    NewEsiFile = EsiFile++"."++integer_to_list(No),
    file:rename(EsiFile, NewEsiFile),
    Res = upload_file(Pid, NewEsiFile, DestDir),
    remove_esi(NewEsiFile),
    upload_files(Pid, DestDir, {chunk, No+1, EsiFileBase, Chunks}, [Res | Acc]);
upload_files(Pid, DestDir, {chunk, No, EsiFileBase, [Chunk | T]}, Acc) ->
    EsiFile = logEsi:pack_esi(Chunk, No, EsiFileBase),
    Res = upload_file(Pid, EsiFile, DestDir),
    remove_esi(EsiFile),
  upload_files(Pid, DestDir, {chunk, No+1, EsiFileBase, T}, [Res | Acc]);
upload_files(_Pid, _DestDir, {chunk, _No, _EsiFileBase, []}, Acc) ->
    case lists:keyfind(error, 1, Acc) of
	false ->
	    %% All uploads is ok
	    ok;
	{error, Reason, _GuiReason} ->
	    %% At least one upload failed
	    Es = map_error(Reason),
	    {error, {"Uploading of ESI failed", Reason}, Es}
    end.

%% ok | {error, Reason, GuiReason}
upload_file(Pid, EsiFile, DestDir) ->
    TotalSize = filelib:file_size(EsiFile),
    case file:open(EsiFile, [read, raw, binary]) of
	{ok, Fd} ->
	    DestFile = filename:join([DestDir, filename:basename(EsiFile)]),
	    case ssh_sftp:open(Pid, DestFile, [write]) of
		{ok, Handle} ->
		    Res = do_upload_file(Fd, Pid, Handle, 0, TotalSize, file:read(Fd, 65536)),
		    file:close(Fd),
		    ssh_sftp:close(Pid, Handle),
		    Res;
		{error, Reason} ->
		    file:close(Fd),
		    Es = map_error(Reason),
		    {error, {"ssh_sftp:open failed", Reason}, Es}
	    end;
	{error, Reason} ->
	    FormatReason = file:format_error(Reason),
	    {error, {"file:open failed", FormatReason}, FormatReason}
    end.

do_upload_file(Fd, Pid, Handle, AccuSize, TotalSize, {ok, Data}) ->
    garbage_collect(),
    NewAccuSize = AccuSize + size(Data),
    ok = ssh_sftp:write(Pid, Handle, Data, ?SSH_TIMEOUT_WRITE),
    do_upload_file(Fd, Pid, Handle, NewAccuSize, TotalSize,
		   file:read(Fd, 65536));
do_upload_file(_, _, _, _, _, eof) ->
    ok;
do_upload_file(_, _, _, _, _, {error, Reason}) ->
    FormatReason = file:format_error(Reason),
    {error, FormatReason, FormatReason}.

%% Runs under catch, returns: true | false
is_allowed_to_factory_reset(Env) ->
    case lists:keyfind(http_host, 1, Env) of
	{http_host, HttpHost} ->
	    %% HttpHost = "169.254.2.2:8080"
	    [I1, I2, I3, I4 | _] = string:tokens(HttpHost, ":."),
	    IP = list_to_tuple([list_to_integer(No) || No <- [I1, I2, I3, I4]]),
	    do_check_for_lmt_ifs(IP);
	_ ->
	    error_msg("check_for_lmt_if failed, no http_host~n", []),
	    false
    end.

do_check_for_lmt_ifs(IP) ->
    Lmt_ips = [ootI:get_lmt_ipv4(), ootI:get_lmt_ll_ipv4()],
    lists:member(IP, Lmt_ips).

%%% ----------------------------------------------------------
%%% Update progess information for the GUI (i.e. update a file that the
%%% webserver is fetching).
%%% ----------------------------------------------------------
update_progress(start) ->
    info_msg("Export ESI started ~n", []),
    do_update("Started");
update_progress(ready) ->
    info_msg("Export ESI ready ~n", []),
    do_update("Finished").

update_progress(nok, Reason, GuiReason) ->
    error_msg("Export ESI failed: ~p~n", [Reason]),
    do_update("Failed: " ++ GuiReason).

do_update(Text) ->
    Str = get_time_stamp() ++ Text,
    DocRoot = sysEnv:www_doc_root(),
    ResultFile = filename:join(DocRoot, ?ESI_RESULT_FILE),
    file:write_file(ResultFile, list_to_binary(Str)).

%%% ----------------------------------------------------------
%%% Misc function
%%% ----------------------------------------------------------

%% TimeStamp = "2014/01/01 01:01:01 "
get_time_stamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    YearF = integer_to_list(Year),
    MonthF = double_digit(Month),
    DayF = double_digit(Day),
    HourF = double_digit(Hour),
    MinF = double_digit(Min),
    SecF = double_digit(Sec),
    YearF ++ "/" ++ MonthF ++ "/" ++ DayF ++ " " ++
	HourF ++ ":" ++ MinF ++ ":" ++ SecF ++ " ".

double_digit(Integer)
  when Integer =< 9 ->
    "0" ++ integer_to_list(Integer);
double_digit(Integer) ->
    integer_to_list(Integer).

%%% ----------------------------------------------------------
%%% Printouts
%%% ----------------------------------------------------------
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%% Log on both regular and active core MP
info_msg_both(Format, Args) ->
    case clhI:core_state() of
	active ->
	    %% This node is the active core node.
	    %% Print only on this node.
	    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]);
	_ ->
	    %% This node is not active core node.
	    %% Print on this node and the active core node.
	    [ActiveNode] = clhI:erlang_nodes(active),
	    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]),
 	    sysInitI:info_msg_node(ActiveNode, "~w: "++Format, [?MODULE|Args])
    end.

%%% ----------------------------------------------------------
%%% Mapping of error
%%% Try to keep aicGui.erl and nl_gui.erl the same.
%%% ----------------------------------------------------------

%% From ssh_xfer.erl (decode_status) - modified
%% Some of the error code are not possible to get but are kept anyway.
map_error(eof) -> "eof";
map_error(no_such_file) -> "no such file";
map_error(permission_denied) -> "permission denied";
map_error(failure) -> "failure";
map_error(bad_message) -> "bad message";
map_error(no_connection) -> "no connection";
map_error(connection_lost) -> "connection lost";
map_error(op_unsupported) -> "pp unsupported";
map_error(invalid_handle) -> "invalid handle";
map_error(no_such_path) -> "no such path";
map_error(file_already_exists) -> "file already exists";
map_error(write_protect) -> "write protect";
map_error(no_media) -> "no media";
map_error(no_space_on_filesystem) -> "no space on filesystem";
map_error(quota_exceeded) -> "quota exceeded";
map_error(unknown_principle) -> "unknown principle";
map_error(lock_conflict) -> "lock conflict";
map_error(not_a_directory) -> "not a directory";
map_error(file_is_a_directory) -> "file is a directory";
map_error(cannot_delete) -> "cannot delete";
map_error(etimedout) -> "unabled to connect to host; etimedout";
map_error(timeout) -> "unable to connect to host; timeout";
map_error(enetunreach) -> "network is unreachable";
map_error(nxdomain) -> "nxdomain";

%% From ssh_auth.erl
map_error("Unable to connect using the available authentication methods") ->
    "incorrect login (user or password)";

%% Any unknown string is handled here; better then just return "unknown error"
map_error(String) when is_list(String) ->
    warning_msg("No mapping done for ~p~n", [String]),
    String;

%% Empty; atom is not in the list above
map_error(Data) ->
    warning_msg("No mapping done for ~p~n", [Data]),
    "unknown error".

%%% ----------------------------------------------------------
%%% Remove local ESI-file without only erlang functions :)
%%% ----------------------------------------------------------
remove_esi(EsiFile) ->
    EsiDir = filename:dirname(EsiFile),
    case file:list_dir(EsiDir) of
	{ok, Fs} ->
	    [file:delete(filename:join([EsiDir, File])) || File <- Fs],
	    file:del_dir(EsiDir);
	_ ->
	    ok
    end.

%%% if the post is through the server listening on Alternate OamAP return
%%% true, if unsure return false
is_alt_connection(Env) ->
    case lists:keyfind(http_host, 1, Env) of
	false ->
	     false;
	{http_host, IpString} ->
	    case {inet:parse_address(ootI:get_oap_ip_addr_alt()),
		  inet:parse_address(IpString)} of
		{Same, Same} ->
		    true;
		_ ->
		    false
	    end
    end.

start_channel_wrapper(false, Host, Port, User, Pw, Opts) ->
    %%% even if we think this is NOT Alt let sftp try anyway...
    sysSftp:start_channel_with_alt(Host, Port, User, Pw, Opts);
start_channel_wrapper(true, Host, Port, User, Pw, Opts) ->
    info_msg("Exporting ESI using alternate OamAccessPoint~n", []),
    sysSftp:start_channel_only_alt(Host, Port, User, Pw, Opts).
