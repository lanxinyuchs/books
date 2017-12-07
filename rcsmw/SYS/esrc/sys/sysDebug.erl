%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	sysDebug.erl %
%%% @author etxarnu
%%% @author etxarnu
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R4A/R11A/1
%%%
%%% @doc ==SYS Erlang trace support==
%%% This module contains helpful functions for starting traces in erlang
-module(sysDebug).
-vsn('/main/R1A/R2A/R4A/R11A/1').
-date('2017-10-16').
-author('etxarnu').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R1A/2      2012-01-30 etxjotj     Fixed copyright info and stuff
%%% R1A/3      2012-03-02 etxpeno     Dialyzer fixes
%%% R11A/1     2017-10-16 etxarnu     Dialyzer fixes
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

-export([help/0, 
	 dt_init/0, dt_init/1,
	 dtp/1, dtp/2, dctp/1, dctp/2, dtpl/1, dtpl/2, dctpl/1, dctpl/2,
	 dctp/0, dp/1, dcp/1, dstop/0, dstop_clear/0, dstart/0,
	 tab/1, tab/2, cmd/1]).

-export([cover_compile/1, cover_analyse/2]).

-export([links_overview/0]).

%%% dbg shortcuts
%-compile(export_all).

%%% @doc Shows help text
help() ->
    io:format("~n"
	      "=========================================================~n"
	      "=== SYS debug functions                               ===~n"
	      "=========================================================~n"
	      " help()             This help text~n"
	      "=================== dbg shortcuts ===========================~n"
	      " dt_init()          Init the dbg function and start it (pretty)~n"
	      " dt_init(Type)      Init the dbg function and start it~n"
	      "                    Type ::= normal |~n"
	      "                             pretty |~n"
	      "                             extra |~n"
	      "                             timestamp~n"
	      " dtp(Mod)           Start dbg for external calls to module~n"
	      " dctp(Mod)          Stop dbg for external calls to module~n"
	      " dtpl(Mod)          Start dbg for module Mod~n"
	      " dctpl(Mod)         Stop dbg for module Mod~n"
	      " dtp(Mod,Func)      Start dbg for exported function Mod:Func~n"
	      " dctp(Mod,Func)     Stop dbg for exported function Mod:Func~n"
	      " dtpl(Mod,Func)     Start dbg for module Mod:Func~n"
	      " dctpl(Mod,Func)    Stop dbg for module Mod:Func~n"
	      " dctp()             Stop dbg for ALL functions~n"
	      " dp(Proc)           Start dbg for process Proc~n"
	      " dcp(Proc)          Stop dbg for process Proc~n"
	      " dstop()            Stop dbg~n"
	      " dstop_clear()      Stop dbg and clear all trace patterns~n"
	      " dstart()           Start dbg~n"
	      "=======================================================================~n~n"
	     ).

%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%% some_method(Parameter)->
%%    nn.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% @doc Init the dbg function and start it (pretty)

dt_init() ->
    dt_init(extra).

-type match_desc() :: {matched, node(), integer()} |
		      {matched, node(), 0, RPCError::term()}.

-spec dt_init(timestamp|pretty|normal|extra) -> {ok, MatchDesc::match_desc()}|
						{error, Reason::term()}.

%%% @doc Init the dbg function and start it with the selected format

dt_init(timestamp) ->
    dbg:tracer(process, {fun dhandler/2, {user, 1}}),
    dbg:p(all, [call, timestamp]);
dt_init(Type) ->
    dbg:tracer(process,
	       {fun(Message, Cntr)->
			{{Yr,Mo,Da},{H,M,S}} = calendar:local_time(),
			io:format(user,"Dbg:~w - ~w-~w-~w ~w:~w:~w.~w "++ str_end(Type),
				  [Cntr,Yr,Mo,Da,H,M,S,
				   element(3,os:timestamp()),
				   Message]),
			Cntr+1 end,
		1}),
    dbg:p(all, [call]).

str_end(pretty) -> "~p~n";
str_end(normal) -> "~w~n";
str_end(extra) -> "~p~n~n".

%%% @doc Start dbg
dstart() -> dbg:p(all, [call]).
%%% @doc Stop dbg
dstop() -> dbg:stop().
%%% @doc Stop dbg and clear all trace patterns
dstop_clear() -> dbg:stop_clear().

%%% @doc Start dbg for external calls to module
dtp(Mod) -> init(), dbg:tp(Mod, cx).
%%% @doc Stop dbg for external calls to module
dctp(Mod) -> dbg:ctp(Mod).

%%% @doc Start dbg for the exported function
dtp(Mod, F) -> init(), dbg:tp(Mod, F, cx).
%%% @doc Stop dbg for the exported function
dctp(Mod, F) -> dbg:ctp(Mod, F).
%%% @doc Stop dbg for all functions
dctp() -> dbg:ctp().  % turn off all traces

%%% @doc Start dbg for all functions in the module
dtpl(Mod) -> init(), dbg:tpl(Mod, cx).
%%% @doc Stop dbg for all functions in the module
dctpl(Mod) -> dbg:ctpl(Mod).

%%% @doc Start dbg for the local function
dtpl(Mod, F) -> init(), dbg:tpl(Mod, F, cx).
%%% @doc Stop dbg for the local function
dctpl(Mod, F) -> dbg:ctpl(Mod, F).

%%% @doc Start dbg for the process

-spec dp(pid() | all | new | existing | atom()) -> match_desc().
dp(Proc) -> init(), dbg:p(Proc).

%%% @doc Stop dbg for process
-spec dcp(pid() | all | new | existing |atom()) -> match_desc().
dcp(Proc) -> dbg:p(Proc, [clear]).

init() ->
    case dbg:get_tracer() of
	{error,_} ->
	    dt_init();
	{ok,_} ->
	    ok
    end.
%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%% internal_function1(One, Two)->
%%    nnn.




%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------



%%% ---------------------------------------------------------
tab(Tab) ->
    tab(full, Tab).

tab(T, Tab) ->
    print_tables(T, [Tab]).

%%% ---------------------------------------------------------
print_tables(short,[Tab|Rest]) ->
    io:format("~n"
	      "=========~p=========~n"
	      "~p~n",
	      [Tab, lists:sort(ets:tab2list(Tab))]),
    print_tables(short,Rest);

print_tables(full,[Tab|Rest]) ->
    ColNames = mnesia:table_info(Tab,attributes),
    TabRows  = lists:sort(ets:tab2list(Tab)),
    io:format("~n"
	      "=========~p=========~n", [Tab]),
    print_rows(TabRows,ColNames),
    print_tables(full,Rest);

print_tables(_,[]) ->
    ok.

print_rows([Row|Rest], ColNames) ->
    pr_row(Row,2,ColNames),
    io:format("===~n"),
    print_rows(Rest, ColNames);
print_rows([], _ColNames) ->
    io:format("~n").

pr_row(Row,Idx,[Col|T]) ->
    io:format("~p = ~p~n",[Col, element(Idx,Row)]),
    pr_row(Row,Idx+1,T);
pr_row(_Row,_Idx,[]) ->
    ok.

%%% #---------------------------------------------------------
dhandler(Trace = end_of_trace, {Out, Ctrl}) ->
    dbg:dhandler(Trace, Out),
    {Out, Ctrl+1};
dhandler(Trace, {Out, Ctrl}) ->
    case element(1, Trace) of
        trace_ts ->
            Size = tuple_size(Trace),
            Timestamp = element(Size, Trace),
            {_MegaSec, _Sec, MilliSec} = Timestamp,
            {{Yr, Mo, Da}, {H, M, S}} = calendar:now_to_local_time(Timestamp),
            io:format(Out,
                      "Dbg:~p ~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~6..0B~n",
                      [Ctrl, Yr, Mo, Da, H, M, S, MilliSec]);
        _ ->
            io:format(Out, "Dbg:~p~n", [Ctrl])
    end,

    dbg:dhandler(Trace, Out),
    {Out, Ctrl+1}.


%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

cover_compile(Apps) ->
    [case application:get_key(App, modules) of
	 {ok, Modules}  ->
	     {ok, Id} = application:get_key(App, id),
	     {ok, Vsn} = application:get_key(App, vsn),
	     Label = Id++"-"++Vsn,
	     [cover_compile_module(Module, Label)||Module<-Modules];
	 undefined ->
	     sysInitI:warning_msg(
	       "sysDebug: Cannot determine modules from app info in ~w~n",[App])
     end||App<-Apps].

cover_compile_module(Module, Label) ->
    Compile = Module:module_info(compile),
    case lists:keysearch(source, 1, Compile) of
	{value, {source, Source}} ->
	    case lists:prefix(Source, os:getenv("RCS_ROOT")) of
		true ->
		    ok;
		false ->
		    File = atom_to_list(Module)++".beam",
		    Target = code:where_is_file(File),
		    AppDir = filename:dirname(filename:dirname(Target)),
		    SrcDir = filename:join(AppDir,"src"),
		    os:cmd(["mkdir -p ", SrcDir]),
		    os:cmd(["cd ", SrcDir, " ; cp ", Source, "@@/",
			    Label, " ", filename:basename(Source)]),
		    cover:compile_beam(Module)
	    end;
	{value, _} -> wtf;
	false -> ok
    end.

cmd(Cmd) ->
    io:format("~s~n~s~n",[Cmd, Res = os:cmd(Cmd)]),
    Res.


cover_analyse(Apps, OutDir) ->
    [case application:get_key(App, modules) of
	 {ok, Modules}  ->
	     {App, [cover_analyse_module(Module, OutDir)||Module<-Modules]};
	 undefined ->
	     {App, []}
     end||App<-Apps].

cover_analyse_module(Module, OutDir) ->
    OutPath = filename:join(OutDir, atom_to_list(Module)++".COVER.html"),
    cover:analyse_to_file(Module, OutPath, [html]),
    {ok, Result} = cover:analyse(Module, module),
    Result.



links_overview() ->
    links_overview(processes()).

links_overview([Pid|Pids]) when is_pid(Pid) ->
    [{name(Pid),
      case process_info(Pid, links) of
	  [] ->
	      [];
	  {links, Linked} ->
	      links_overview(Linked)
      end}|links_overview(Pids)];
links_overview([]) -> [];
links_overview([_|Pids]) ->
    links_overview(Pids).

name(Pid) ->
    case process_info(Pid, registered_name) of
	[] ->
	    Pid;
	{registered_name, Name} ->
	    Name
    end.
