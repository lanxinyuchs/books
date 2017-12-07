%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% @author etxlg
%%% @copyright Ericsson AB 2017
%%% @version /main/R11A/2
%%%
%%% @doc ==Collection of XCM data==
%%% This module register a callback function to the LOG ESI support
%%% which dumps any XCM socket information when collecting ESI data
%%% @end
-module(sysXcm).
-vsn('/main/R11A/2').
-date('2017-10-12').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% Rx         2017-10-05 etxlg       Created
%%% R11A1      2017-10-12 etxlg       Error report -> warning
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([generate_esi/0, init_data/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%-include("template.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

init_data() ->
    ok = logI:register_esi_cb(?MODULE).

generate_esi() ->
    try
	gen_esi()
    catch
	A:B ->
	    warn_rpt([{"Class", A}, {"Exception", B}, 
		       {"Stack_trace", erlang:get_stacktrace()}]),
	    {error, "exception caught"}
    end.

%%% #---------------------------------------------------------
%%% # INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

warn_rpt(Report) ->
    sysInitI:warning_report([{"Module", ?MODULE} | Report]).

%info_msg(Format, Args) ->
%    sysInitI:info_msg("~p: " ++ Format ++ "~n", [?MODULE | Args]).

my_log_dir() ->
    % "/tmp/xcm_esi".
    filename:join([sysEnv:rcs_dir(), "sys"]).

my_log_file() ->
    %File = "ultra_dumt",
    File = "xcm_mp" ++
	   integer_to_list(clhI:own_mp_id()) ++
	   "_active_sockets.txt",
    filename:join([my_log_dir(), File]).

xcm_cmd() ->
    %["/tmp/xcmctl"].
    ecoli_api:external_cmd_paths("/diagm", "xcminfo").

gen_esi() ->
    gen_esi(xcm_cmd()).

gen_esi([Cmd]) ->
    Log_file = my_log_file(),
    gen_esi(Cmd, filelib:ensure_dir(Log_file), Log_file);
gen_esi([]) ->
    xcm_not_installed;
gen_esi(Cmd_fail) ->
    warn_rpt([{"What", "Failed to locate control binary"},
	       {"Why", Cmd_fail}]),
    {error, "xcmctl binary"}.

gen_esi(Cmd, ok, Log_file) ->
    case file:delete(Log_file) of
	Good when Good =:= ok; Good =:= {error, enoent} ->
	    do_gen_esi(Cmd, Log_file);
	Error ->
	    warn_rpt([{"What", "Failed to delete old logfile"},
		       {"File", Log_file},
		       {"Error", Error}]),
	    {error, "clean old logfile"}
    end;
gen_esi(_, Error, Log_file) ->
    warn_rpt([{"What", "Failed to ensure log directory"},
	       {"Directory", filename:dirname(Log_file)},
	       {"Error", Error}]),
    {error, "log directory"}.

do_gen_esi(Cmd, Log_file) ->
    case parse_socket_listing(xcm_list(Cmd)) of
	ok ->
	    write_log(Log_file, "No active XCM sockets found");
	{ok, Raw_socket_listing, Pid_ref_list} ->
	    do_gen_esi(Cmd, Log_file, Pid_ref_list, 
			write_log(Log_file, Raw_socket_listing))
    end.

do_gen_esi(Cmd, Log_file, Pid_ref_list, ok) ->
    do_gen_esi(Cmd, Log_file, Pid_ref_list);
do_gen_esi(_, Log_file, _, {error, Error}) ->
    warn_rpt([{"What", "Failed to write log file"},
               {"File", Log_file},
               {"Error", Error}]),
    {error, "write logfile"}.

do_gen_esi(_, _, []) ->
    ok;
do_gen_esi(Cmd, Log_file, [{Pid, Ref} | Rest]) ->
    write_log(Log_file, xcm_get(Cmd, Pid, Ref)),
    do_gen_esi(Cmd, Log_file, Rest).

% ok |	{ok, Raw_socket_listing, Pid_ref_list}
parse_socket_listing(<<>>) ->
    ok;
parse_socket_listing(Listing) ->
    Split = binary:split(Listing, <<"\n">>, [global, trim_all]),
    {Head, Socks} = psl(Split, {"not found", []}),
    Sorted_socks = lists:sort(Socks),
    {Pid_ref_list, Raw_sorted} = lists:unzip(Sorted_socks),
    {ok, [Head | Raw_sorted], Pid_ref_list}.

psl([], {Header, Socks}) -> {Header, Socks};
psl([<<"Create", _/binary>> = Header | Rest], {_, Socks}) ->
    psl(Rest, {<<Header/binary, "\n">>, Socks});
psl([Line | Rest], {Header, Socks}) ->
    psl(Rest, {Header, [psl(Line) | Socks]}).

psl(Line) ->
    [B_pid, B_id |_ ] = binary:split(Line, <<"\s">>, [global, trim_all]),
    Line_nl = <<Line/binary, "\n">>,
    {{binary_to_integer(B_pid), binary_to_integer(B_id)}, Line_nl}.

xcm_list(Cmd) ->
    case execute_return(Cmd, ["list"]) of
	{ok, Output} ->
	     Output;
	{error, Error} ->
	    warn_rpt([{"What", "Error from xcmctl command"},
		       {"Error", Error}]),
	     []
    end.

xcm_get(Cmd, Pid, Ref) ->
    Pid_s = integer_to_list(Pid),
    Ref_s = integer_to_list(Ref),
    case execute_return(Cmd, ["get", Pid_s, Ref_s]) of
	{ok, Output} ->
	     Head = << "\nPID: ",  (list_to_binary(Pid_s))/binary,
		       ", Sockref: ", (list_to_binary(Ref_s))/binary, "\n">>,
	     <<Head/binary, Output/binary>>;
	{error, Error} ->
	    warn_rpt([{"What", "Error from xcmctl command"},
		       {"Error", Error}]),
	     "get failed for pid: " ++ Pid_s ++ ", ref: " ++ Ref_s
    end.


execute_return(Cmd, Args) ->
    Port = open_port({spawn_executable, Cmd},
                     [{args, Args},
                      stream, binary, exit_status]),
    try read_from_port(Port, <<>>)
    after
        catch port_close(Port)
    end.

read_from_port(Port, Bin) ->
    receive
        {Port, {data, Data}} ->
            read_from_port(Port, <<Bin/binary, Data/binary>>);
        {Port, {exit_status, Rc}} when Rc =:= 0->
            {ok, Bin};
        {Port, {exit_status, Rc}} ->
            {error, "error return from external program, RC: " ++
		     integer_to_list(Rc)}
    after 5000 ->
        {error, "execution timeout"}
    end.

write_log(Log_file, Data) when is_list(Log_file) ->
    write_log(file:open(Log_file, [append, raw]), Data);
write_log({ok, Io}, Data) ->
    try
	file:write(Io, Data)
    after
	file:close(Io)
    end;
write_log({error, _} = Error, _) ->
    Error.

