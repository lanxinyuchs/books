%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% @author etxarnu
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R7A/R8A/R9A/1
%%%
%%% @doc ==Collection of TRI data==
%%% This module register a callback function to the LOG ESI support
%%% which dumps the contents of the TRI buffer to disk when collecting ESI
%%% data
%%% @end
-module(appmEsi).
-vsn('/main/R2A/R3A/R4A/R5A/R7A/R8A/R9A/1').
-date('2017-01-26').
-author('etxarnu').
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
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2014-04-16 etxarnu     Created
%%% R4A/1      2015-04-24 etxpejn     Added esi_local
%%% R5A/1      2016-02-19 etxarnu     Fixed export string
%%% R7A/1      2016-10-03 uabesvi  HV26316 Timeout in ESI CB
%%% R7A/2-4    2016-10-05 etxarnu  HV26316 Use maxEsiCbTimeout in wait/2
%%% R7A/5      2016-10-06 etxarnu  WP6081 : Added generate_rollback_esi
%%%                                Cleaned up the code a bit
%%% R8A/1      2016-12-21 etxarnu  Changed appmAppData:get_lm_start_data
%%%                                to appmAppData:get_lm_paths
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([generate_esi/0, init_data/0, init_data/1]).
-export([generate_rollback_esi/0]).
%%%for internal use
-export([call_esi/2]).

-include("appm.hrl").

%%% ----------------------------------------------------------
init_data() ->
    mnesia:dirty_write({appmNodeTable, maxEsiCbTimeout,
			list_to_integer(?MAX_ESI_CB_TMO_DEF)*1000 }),
    ok = logI:register_esi_cb(?MODULE). 


init_data(Time) ->
    mnesia:dirty_write({appmNodeTable, maxEsiCbTimeout, Time*1000 }),
    %% Register a callback function
    ok = logI:register_esi_cb(?MODULE, Time). 

%%% @doc ESI callbacks to applications, called from LOG
%%% ===Arguments===
%%% -
generate_esi() ->
    process_flag(trap_exit, true),
    Progs = 
	case clhI:core_state() of
	    active ->
		appmAppData:get_esi_apps();
	    _ ->
		[]
	end, 
    LocalProgs = appmAppData:get_esi_local_apps(),
    AllProgs = Progs ++ LocalProgs,
    Pids = [ spawn_link(?MODULE, call_esi, [self(),X] ) || X <- AllProgs],
    wait(lists:zip(Pids, AllProgs ),[]).

generate_rollback_esi() ->
    process_flag(trap_exit, true),
    RbEsiProgs = appmAppData:get_rollback_esi_apps(),
    Pids = [ spawn_link(?MODULE, call_esi, [self(),X] ) || X <-RbEsiProgs ],
    add_log_path(wait(lists:zip(Pids, RbEsiProgs ),[]), []).

wait([],Acc) ->
    lists:reverse(Acc);
wait(Pids,Acc) ->
    [{_,_,Timeout}] = mnesia:dirty_read({appmNodeTable, maxEsiCbTimeout }),

    receive
	{ok,Ans,Pid} ->
	    unlink(Pid),
	    NewPids = lists:keydelete(Pid,1,Pids),
	    wait(NewPids, [Ans|Acc]);
	{'EXIT',_Pid,normal} ->
	    wait(Pids,Acc);
	{'EXIT',Pid,Reason} ->
	    NewPids = lists:keydelete(Pid,1,Pids),
	    wait(NewPids, [{exit,Pid,Reason}|Acc])
    after Timeout ->
	    sysInitI:info_msg(
	      "appmEsi:generate_esi: timeout from these : ~n ~p~n",
	      [Pids]),
	    {error,Pids,Acc}
    end.


call_esi(Parent,NameCxc) ->
    {ok, CxpPath, RelPath} = appmAppData:get_lm_paths(NameCxc),
    Path = filename:join([CxpPath, RelPath]),
    Tpath = swmI:find_file(Path), %use patched version if exist

    case filelib:wildcard(Tpath) of
	[Prog] ->
	    %%could be optimized to just read once except for CXP_PATH
	    Env = appmServer:get_rcs_env(CxpPath),
	    Str = ["export "++K++"='"++V++"';" ||{K,V} <- Env],
	    info_msg("Calling: ~p~n", [Prog]),
	    R = os:cmd(Str ++ Prog ),
	    info_msg("Returned from ~p with result ~p~n", [Prog,R]),
	    Parent!{ok,{NameCxc,R},self()};
	Other ->
	    sysInitI:error_msg(
	      "appmEsi:call_esi() failed for ~p with reason ~p ~n",
	      [Tpath,Other]),
	    Parent!{ok, {NameCxc,"ESI-file_not_found"}, self()}
    end.

add_log_path([],Acc) ->
    Acc;
add_log_path([{NameCxc,Res}|Rest],Acc) ->
    {ok, CxpPath,_} = appmAppData:get_lm_paths(NameCxc),
    {Name,Id,_Ver} = swmI:get_cxp_source(CxpPath),
    LogDir = filename:join([sysEnv:rcs_dir(),"applicationlogs",Name++"_"++Id]),
    add_log_path(Rest,[{NameCxc,Res,LogDir}|Acc]);
add_log_path([{exit,_Pid,_Reason}|Rest],Acc) ->
    add_log_path(Rest,Acc);
add_log_path({error,_Pids,OkEsis},[]) ->
    add_log_path(OkEsis,[]).

    

info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

