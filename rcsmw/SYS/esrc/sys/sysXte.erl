%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% @author etomist
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R8A/R12A/2
%%%
%%% @doc ==Collection of TRI data==
%%% This module register a callback function to the LOG ESI support
%%% which dumps the contents of the TRI buffer to disk when collecting ESI
%%% data
%%% @end
-module(sysXte).
-vsn('/main/R2A/R3A/R4A/R8A/R12A/2').
-date('2017-11-20').
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
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2013-04-29 etxjotj     Created
%%% R2A/2      2013-05-07 etxarnu     Now also for sim environment
%%% R2A/3      2013-06-17 etxpeno     Added sync()
%%% R2A/4      2014-02-18 etxarnu     Changed te to tex and added LttngLog
%%% R2A/6      2014-03-31 etxarnu     spawn process to handle tex log read
%%%                                   and removed unused code.
%%% R2A/7      2014-04-04 etxarnu     Removed PreCmd from te commands
%%%                                   Added TE_LTTNG_SNAPSHOT_PATH as esi_dir
%%% R2A/8      2014-06-04 etxjotj     Removed TE_LTTNG_SNAPSHOT_PATH as esi_dir
%%%                                   Reworked timeout handling
%%% R2A/9      2014-08-19 etxjotj     Adding more paths
%%% R2A/10     2014-08-19 etxtory     Removed bootlogs
%%% R3A/1      2014-08-29 etxarnu     truncate TriLog at each new esi
%%% R3A/2-3    2015-01-16 etxarnu     Don't use disklog for TriLog
%%%                                   Removed LttngLog (obsolete)
%%% R2A/12     2015-01-20 etxarnu     Create missing TriLog directory
%%% R3A/4      2015-01-20 etxarnu     Create missing TriLog directory
%%% R4A/1      2015-05-19 etxpejn     Added init_board
%%% R4A/3      2015-09-04 etxarnu     Added a top snapshot to Top.log 
%%% R8A/1      2016-12-28 uabesvi     No Tri log in cloud
%%% R8A/2      2017-01-19 uabesvi     Added back Tri log in cloud
%%% R12A/1     2017-11-07 ecotjos     HW39130
%%% R12A/2     2017-11-17 etomist     Erlang process info added to ESI
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([generate_esi/0, init_data/0, init_board/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([generate_esi/1]).
%-include("template.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

init_board() ->
    ok = logI:create_log("TriLog", [{maxSize, 1},
				    {rotatingSegments, 3},
				    {public, false}]).


%% No tri log in cloud
init_data() ->
    ok = logI:register_esi_cb(?MODULE),
    case os:getenv("EE_ESI_OUTPUT") of
	false -> 
	    ok;
	EEDir ->
	    logI:register_esi_dir(EEDir)
    end.


generate_esi() ->
    case proc_lib:start(?MODULE, generate_esi, [self()], 70000, []) of
	ok ->
	    ok;
	{error, timeout} ->
    	    sysInitI:info_msg("sysXte:generate_esi timeout~n")
    end.


generate_esi(Parent) ->
    catch generate_erl_process_info(),

    case os:getenv("EE_ESI_COMMAND") of
    false -> ok;
    Command ->
        cmd(Command)
    end,

    TriLogDir = filename:join([sysEnv:vnf_dir(),"log","TriLog"]),
    file:make_dir(TriLogDir), %ensure it exists
    LogFile = filename:join([TriLogDir,"TriLog.1"]),
    TopFile = filename:join([TriLogDir,"Top.log"]),
    Date = comsaI:iso_time(os:timestamp(), extended),
    Msg = "Generating new te dump at "++Date,
    cmd("echo " ++ Msg ++ " > " ++ LogFile),
    cmd("te log read >> " ++ LogFile),
    cmd("top -b -n 1 > " ++ TopFile),

    proc_lib:init_ack(Parent, ok).

cmd(Cmd) ->
    sysInitI:info_msg("sysXte: ~s~n",[lists:flatten(Cmd)]),
    os:cmd(Cmd).

generate_erl_process_info() ->
    Res = file:open("/rcs/erlang_disk/erl_process_info", [write, binary]),
    case Res of 
    {ok, Out} ->
        T1 = erlang:timestamp(),
        {{Y,Mo,D},{H,Mi,S}} = calendar:local_time(),
        io:format(Out, "Current time: ~p-~p-~p ~p:~p:~p~n", 
                  [Y, Mo, D, H, Mi, S]),
        F = 
        fun(P) ->
            Info = erlang:process_info(P, 
                [registered_name, current_stacktrace, message_queue_len,
                 messages, memory, garbage_collection_info, status,
                 dictionary]),
            case Info of
                undefined -> 
                    io:format(Out, "No process data for ~p~n", [P]);
                _ ->
                    {_, N} = lists:keyfind(registered_name, 1, Info),
                    Name = 
                    case N of
                        [] -> "";
                        _ -> atom_to_list(N)
                    end,
                    {_, Stack} = lists:keyfind(current_stacktrace, 1, Info),
                    {_, QueueLen} = lists:keyfind(message_queue_len, 1, Info),
                    {_, Messages} = lists:keyfind(messages, 1, Info),
                    {_, Memory} = lists:keyfind(memory, 1, Info),
                    {_, GcInfo} = lists:keyfind(garbage_collection_info, 1, Info),
                    {_, Status} = lists:keyfind(status, 1, Info),
                    {_, Dict} = lists:keyfind(dictionary, 1, Info),
                    io:format(Out, "PID ~p, Name ~s~n", [P, Name]),
                    io:format(Out, "Status: ~p~n", [Status]),
                    io:format(Out, "Stack ~p~n", [Stack]),
                    io:format(Out, "Message queue length: ~p~n", [QueueLen]),
                    io:format(Out, "Messages: ~p~n", [Messages]),
                    io:format(Out, "Memory [bytes]: ~p~n", [Memory]),
                    io:format(Out, "Garbage collection info: ~p~n", [GcInfo]),
                    io:format(Out, "Dictionary: ~p~n", [Dict]),
                    State = 
                    %% make sure that we can actually call get_state
                    case proc_lib:translate_initial_call(P) of
                        {proc_lib, init_p, 5} ->
                            undefined;
                        {_M, _F, _A} ->
                            case catch sys:get_state(P, 5) of
                                {'EXIT', _} -> undefined;
                                X ->
                                    % remove all states that contain private keys
                                    % currently this means removing all states that
                                    % have RSAPrivateKey, ECPrivateKey and DSAPRivateKey
                                    % in them
                                    % in addition all states mentioning dh_gex_groups
                                    % will be removed since there we have e and d
                                    % for RSA
                                    % finally, remove all states with password or
                                    % otherwise, we will collect the password of the
                                    % user generating ESI
                                    SerState = io_lib:format("~p", [X]),
                                    S1 = string:find(SerState, "PrivateKey"),
                                    S2 = string:find(SerState, "dh_gex_groups"),
                                    S3 = string:find(SerState, "password"),
                                    case {S1, S2, S3} of
                                        {nomatch, nomatch, nomatch} -> X;
                                        _ -> removed_due_to_security
                                    end
                            end;
                        _ ->
                            undefined
                    end,
                    io:format(Out, "State: ~p~n", [State])
            end,
            io:format(Out, "~90.0.=s~n~n", ["="])
        end,
        lists:foreach(F, erlang:processes()),
        file:close(Out),
        T2 = erlang:timestamp(),
        sysInitI:info_msg("Erlang process info collected in ~.3f seconds~n", [timer:now_diff(T2, T1) / 1000000.0]);
    _ ->
        sysInitI:warning_msg("Cannot open /rcs/erlang_disk/erl_process_info~n", [])
    end.


