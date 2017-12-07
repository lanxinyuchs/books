%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysRhai.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R4A/R5A/R10A/R11A/1

%%% @doc ==Set boot parameters==
%%% This module implements erlang API to rhai_sys lib which
%%% is used for reboot parameter settings.
%%% @end

-module(sysRhai).
-vsn('/main/R2A/R4A/R5A/R10A/R11A/1').
-date('2017-10-03').
-author('etxpejn').

%%% ----------------------------------------------------------
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
%%% R2A/1	140407	  etxderb     Created	
%%% R2A/4	140429	  etxderb     Added setbootptr/getbootptr
%%% R5A/1	160315	  etxpejn     Added hwl_logentry
%%% R10A/1	170620	  etxarnu     added CXP_NO and CXP_REV environment
%%%                                   variables to program started in run(
%%% R11A/1	171003	  etxpejn     Added hwl_logentry/2 to add LogId
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([setbootcounter/1, 
         getbootcounter/0,
	 setbootptr/1,
	 getbootptr/0]).

-export([hwl_logentry/1]).
-export([hwl_logentry/2]).


-define(BOOTCOUNT_BIN, "bootcounter").
-define(BOOTPTR_BIN, "bootptr").
-define(HWL_BIN, "hwl").


%%% ----------------------------------------------------------
%%% @doc Set the boot counter
%%% The boot counter decides which installation that is going to be used
%%% The "configured" installation is used if bootcount is 0-2
%%% The "fallback" installation is used if bootcount is 3-5
%%% The "network loader" installation is used if bootcount is 6-
%%% The boot counter is reset by Linux EE at every restart, however there
%%% is a watchdog mechanism for Linux EE to detect middleware failure, upon
%%% which the boot counter will be restored and incremented
%%% @end
%%% ----------------------------------------------------------

-spec setbootcounter(Cnt::integer()) -> 
			    ok | {error, not_found|
				  timeout|{exit_status, ES::integer()}}.

setbootcounter(Cnt) when is_integer(Cnt) ->
    sysInitI:info_msg("~w:setbootcounter(~w)~n",[?MODULE, Cnt]),
    case run(program(?BOOTCOUNT_BIN), ["set", integer_to_list(Cnt)]) of
	{ok, _} ->
	    ok;
	Error ->
	    sysInitI:error_msg("~p: ~n",[?MODULE, Error]),
	    Error
    end.

%%% ----------------------------------------------------------
%%% @doc Get the boot counter
%%% The boot counter decides which installation that is going to be used
%%% The "configured" installation is used if bootcount is 0-2
%%% The "fallback" installation is used if bootcount is 3-5
%%% The "network loader" installation is used if bootcount is 6-
%%% The boot counter is reset by Linux EE at every restart, however there
%%% is a watchdog mechanism for Linux EE to detect middleware failure, upon
%%% which the boot counter will be restored and incremented
%%% @end
%%% ----------------------------------------------------------
-spec getbootcounter() -> {ok, integer()}| 
			  {error, not_found|
			          timeout|
			          {exit_status, ES::integer()}}.
getbootcounter() ->
    case run(program(?BOOTCOUNT_BIN), ["get"]) of
	{ok, IOs} ->
	    {ok, int_res(IOs)};
	Error -> 
	    Error
    end.

-define(BOOT_PTR_REQS, [configured, fallback, nl, upgrade, configured_once]).
-define(BOOT_PTR_STATES, [configured, fallback, nl]).
-define(is_boot_ptr_req(__Ptr), lists:member(__Ptr, ?BOOT_PTR_REQS)).
%%% ----------------------------------------------------------
%%% @doc Set the boot ptr
%%% The bootptr action sets which installation to do on next boot vi changing
%%% bootptr state and boot counter. These two values decides what boot
%%% uboot will upon next boot.
%%% Avaliable bootptr requests are: configured | fallback | nl | upgrade | configured_once.
%%% Note: there is a strict relation between the boot counter and bootptr state. 
%%%       Normally bootptr state is decied from the boot counter, 
%%%       this function overrides the boot counter setting and have side 
%%%       effects changing the counter and the boot ptr state.
%%% configured -> This is the normal case, boot on the installed application
%%%               software and configuration either installed by netloader
%%%               or same as from previous boot.
%%% fallback -> 
%%% nl -> Next boot will start the networkloader
%%% upgrade -> TODO ?? Add desciption
%%% configured_once -> Set the boot pointer to configured and boot counter to 2
%%%                    to ensure a configured boot is followed by a fallback 
%%%                    boot in case of failure.
%%% @end
%%% ----------------------------------------------------------	     
-spec setbootptr(Bootptr_req::atom()) -> 
      ok | 
      {error, not_found|
              timeout|
              {unknown, BP::atom()}|
              {exit_status, ES::integer()}}.

setbootptr(Bootptr_req) ->
    setbootptr(?is_boot_ptr_req(Bootptr_req), Bootptr_req).
setbootptr(true, Bootptr_req) ->
    sysInitI:info_msg("~w:setbootptr(~w)~n",[?MODULE, Bootptr_req]),
    case run(program(?BOOTPTR_BIN), ["set", Bootptr_req]) of
	{ok, _} ->
	    ok;
	Error ->
            sysInitI:error_msg("~p: ~n",[?MODULE, Error]),
	    Error
    end;
setbootptr(false, Bootptr_req) ->
  {error, {unknown, Bootptr_req}}.

%%% ----------------------------------------------------------
%%% @doc Get the boot ptr
%%% Reads current bootpointer
%%% Boot pointer states can be one of the following: configured | 
%%%                                                  fallback | 
%%%                                                  nl | 
%%%                                                  upgrade?? Check if this is tate or only an action 
%%% @end
%%% ----------------------------------------------------------
-spec getbootptr() ->
      {ok, configured|fallback|nl|upgrade|configured}|
      {error, not_found|
              timeout|
              {exit_status, ES::integer()}}.
	
getbootptr() ->
    case run(program(?BOOTPTR_BIN), ["get"]) of
	{ok, IOs} ->
	    {ok, atom_res(IOs)};
	Error ->
	    Error
    end.
	
%%% ----------------------------------------------------------
%%% @doc Write one entry to the HW Log area.
%%% @end
%%% ----------------------------------------------------------

-spec hwl_logentry(Entry::string()) -> 
			  ok | 
			  {error, not_found|timeout|{exit_status, ES::integer()}}.

hwl_logentry(Entry) ->
    hwl_logentry(Entry, "003").


-spec hwl_logentry(Entry::string(),
		   LogId::string()) -> 
			  ok | 
			  {error, not_found|timeout|{exit_status, ES::integer()}}.
hwl_logentry(Entry, LogId) ->
    case run_env(program(?HWL_BIN), ["log"++[0], LogId++[0], Entry++[0]]) of
	{ok, IOs} ->
	    {ok, atom_res(IOs)};
	Error ->
	    Error
    end.

atom_res(R) when is_binary(R) -> atom_res(binary_to_list(R));
atom_res(R) when is_list(R) -> list_to_atom(rem_nl(R)).

int_res(R) when is_binary(R) -> int_res(binary_to_list(R));
int_res(R) when is_list(R) -> list_to_integer(rem_nl(R)).

rem_nl(R) -> string:strip(R, right, $\n).
     

program(Prog) ->
    Progpath = filename:join([code:priv_dir(sys),
                              sysEnv:target_bin_dir(), 
                              Prog]),
    swmI:find_file(Progpath).


run_env(false, _) ->
    {error, not_found};
run_env(Program, Args) ->
    {CxpNo,CxpRev} = appmServer:get_mw_cxp_info(),
    Env = [{"CXP_NO",CxpNo},{"CXP_REV",CxpRev}],
    run(Program, Args,Env).

run(false, _) ->
    {error, not_found};
run(Program, Args) ->
    run(Program, Args,[]).

run(Program, Args,Env) ->
    Port = open_port({spawn_executable, Program},
		     [{args, Args},{env, Env}, stream, binary, exit_status]),
    try handle_port(Port, Program) of
	Res -> Res
    after
	catch port_close(Port)
    end.

handle_port(Port, Program) ->
    handle_port(Port, Program, []).
handle_port(Port, Program, Sofar) ->
    receive
	{Port, {data, Data}} ->
	    handle_port(Port, Program, Data);
	{Port, {exit_status, ES}} when ES==0->
	    {ok, Sofar};
	{Port, {exit_status, ES}} ->
	    {error, {exit_status, ES}}
    after 2000 ->
	    {error, timeout}
    end.

