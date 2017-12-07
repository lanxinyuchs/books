%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	appmRhai.erl %
%%% @author etxarnu
%%% @copyright Ericsson AB 2014
%%% @version /main/R2A/3
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014 All rights reserved.
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
%%% R2A/1	140320	  etxarnu
%%% R2A/2	140322	  etxarnu    Improved read("restart_type")
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-module(appmRhai).
-vsn('/main/R2A/3').
-date('2014-04-22').
-author('etxarnu').

-export([setptr/1, read/1]).

%%% ----------------------------------------------------------

setptr(Ptr) when Ptr == "configured"; Ptr == "fallback"; Ptr == "nl" ->
  run(program("set_bootptr"), [Ptr]);
setptr(Error) ->
  {error, {unknown_boot_ptr, Error}}.

read(What) when What == "bootptr" ; What == "restart_type"; What == "bootcount" ->
   run(program("sysread"), [What]);
read(Error) ->
   {error, {unknown_readobject, Error}}.

%%% ----------------------------------------------------------
program(What) ->
    PrivDir = code:priv_dir(appm),
    BinDir = sysEnv:target_bin_dir(),
    Prog = filename:join([PrivDir, BinDir, What]),
    swmI:find_file(Prog).

%%% ----------------------------------------------------------
run(false, _) ->
  {error, not_found};
run(Program, Args) ->
    os:cmd(Program ++ " " ++ Args).
%%   Port = open_port({spawn_executable, Program},
%% 		   [{args, Args}, stream, binary, exit_status]),
%%   try handle_port(Port, Program) of
%%      ok -> ok
%%      after
%%        catch port_close(Port)
%%      end.

%% %%% ----------------------------------------------------------
%% handle_port(Port, Program) ->
%%     receive
%%        {Port, {data, Data}} ->
%%           io:format("~p~n", [binary_to_list(Data)]),
%%           handle_port(Port, Program);
%%        {Port, {exit_status, ES}} ->
%%           %% io:format("Exit from ~p with exit code: ~p~n", [Program, ES]),
%%           ok
%%     after 2000 ->
%%           io:format("Timout from ~p~n", [Program]),
%%           ok
%%     end.

