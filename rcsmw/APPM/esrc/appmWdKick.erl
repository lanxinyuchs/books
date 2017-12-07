%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	appmWdKick.erl %
%%% Author:	etxbjca
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(appmWdKick).
-author(etxjotj).
-vsn('/main/R5A/1').
-date('2016-04-01').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% R5A/1      160330    etxarnu      Created


-export([start/1, stop/0, init/1]).
-export([kick/0]).

start(WdFile) ->
    ProgT=filename:join([code:priv_dir(appm),sysEnv:target_bin_dir(),"kickwd"]),
    Prog=swmI:find_file(ProgT),
    ExtPrg = Prog ++ " " ++ WdFile,
    spawn_link(?MODULE, init, [ExtPrg]).

stop() ->
    wd_kicker ! stop.

kick() ->
    call_port(kick).


call_port(Msg) ->
    wd_kicker ! {call, self(), Msg},
    receive
	{wd_kicker, Result} ->
	    Result
    end.

init(ExtPrg) ->
    register(wd_kicker, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {wd_kicker, decode(Data)}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    error_logger:error_report(
	      [{?MODULE, loop,{exit,Reason}} ,
	      erlang:get_stacktrace()]),
	    exit(port_terminated)
    end.

encode(kick) -> [1]. 

decode([Int]) -> Int.
