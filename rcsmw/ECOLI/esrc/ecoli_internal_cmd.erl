%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ecoli_internal_cmd.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R9A/1

%%% @doc ==COLI==
%%% This module contains the implementation for all "internal" coli commands,
%%% this includes both those commands that are truly internal, i.e. available
%%% anywhere in the command-tree, and those that are found under ?INTERNAL_DIR.
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(ecoli_internal_cmd).
-vsn('/main/R2A/R3A/R4A/R5A/R9A/1').
-date('2017-03-27').
-author('uabesvi').
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
%%% -----      -------    --------    ------------------------
%%% Rx         2013-07-08   etxlg     Copied and changed from orig coli
%%% R2A/2      2013-08-27   etxlg     Added ls + misc fix
%%% R2A/3      2013-09-02   etxlg     Improved help and cd
%%% R2A/4      2013-09-05   etxlg     Improved ls, cd, and help
%%% R2A/5      2013-09-05   etxlg     /coli -> ?INTERNAL_DIR, new cmd
%%% R2A/6      2013-09-06   etxlg     silence dialyzer, bleah!
%%% R2A/7      2013-09-11   etxlg     ensure "help help" works
%%% R2A/9      2014-01-31   etxlg     ls accept "/" at end of dir
%%% R2A/11     2014-04-23   etxlg     bug fix of "ls"
%%% R2A/12     2014-07-04 etxberb     Added coli_prompt/1.
%%% R3A/1      2015-03-29   etxlg     Updated help text
%%% R4A/8      2015-07-14   etxlg     New role names
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-include("ecoli.hrl").
-export([evaluate_cmd/2]).

% these are internally "registered" and called as if they were (almost)
% normal coli commands, used to manipulate state in ecoli_ssh_channel
-export([set_report_rc/1, set_timeout/1, set_auth_level/1, info/1]).
-export([grep/1]).
-export([tail/1]).
-export([list_cmds/1, set_prompt/1]).
-export([coli_prompt/1]).

-define(TAIL_DEF, 10).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%arg1: Shell_state::#cst{}
%arg2: Cmd_pipe, a list of commands e.g. ["ps", "ls dir"...]
%returns: {internal, #cst{}, Message} | external | exit
% where Message is printed back to the user before the prompt
%% evaluate_cmd(#cst{fruacc_names = FruAccNames} = ShellState, [Cmds | _] = CmdPipe) when is_list(Cmds) ->
%%     [Cmd | _] = Cmds,
%%     check_cmds(ShellState, CmdPipe, lists:keysearch(Cmd, 2, FruAccNames));
evaluate_cmd(#cst{fruacc_names = _FruAccNames} = ShellState, CmdPipe) ->
    check_cmds(CmdPipe, ShellState).

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

set_report_rc(["true"]) ->
    group_leader() ! {internal_cmd, {set_report_rc, true}},
    exit(normal);
set_report_rc(["false"]) ->
    group_leader() ! {internal_cmd, {set_report_rc, false}},
    exit(normal);
set_report_rc(_) ->
    io:format("Supported options: true | false~n"),
    exit("argument error").

set_timeout(["infinity"]) ->
	    group_leader() ! {internal_cmd, {set_timeout, infinity}};
set_timeout([T]) ->
    try
	list_to_integer(T)
    of
	Seconds ->
	    group_leader() ! {internal_cmd, {set_timeout, Seconds}}
    catch
	_:_ ->
	    exit("argument error")
    end;
set_timeout(_) ->
    exit("argument error").

set_auth_level([L]) when is_list(L)->
    Levels = ["disabled" | ecoli_lib:get_official_auth_strings() ++
			   ecoli_lib:get_legacy_auth_strings()],
    case lists:member(L, Levels) of
	true ->
	    New_auth_level = ecoli_lib:auth_to_integer(L),
	    group_leader() ! {internal_cmd, {set_auth_level, New_auth_level}};
	false ->
	    set_auth_error()
    end;
set_auth_level(_) ->
    set_auth_error().

set_auth_error() ->
    io:format("Supported arguments: \"~s\", or \"disabled\"~n",
	[string:join(ecoli_lib:get_official_auth_strings(), "\", \"")]),
    exit("argument error").

info(_) ->
    Ref = make_ref(),
    group_leader() ! {internal_cmd, {info, Ref, self()}},
    receive
	{Ref, Answer} ->
	    io:format("~s", [Answer]),
	    exit(normal)
    after 5000 ->
	    io:format("internal command timeout~n"),
	    exit(error)
    end.

coli_prompt(["-d"]) ->
    set_prompt([]);
coli_prompt(["-s" | New_prompt]) ->
    set_prompt(New_prompt);
coli_prompt([]) ->
    coli_prompt(["-d"]);
coli_prompt(_) ->
    io:format("argument error~n~n").

set_prompt(New_prompt) ->
    group_leader() ! {internal_cmd, {set_prompt, New_prompt}}.

list_cmds(Args) ->
    Ref = make_ref(),
    group_leader() ! {internal_cmd, {list_cmds, Ref, self(), Args}},
    receive
	{Ref, Answer} ->
	    io:format("~s", [Answer]),
	    exit(normal)
    after 5000 ->
	    io:format("internal command timeout~n"),
	    exit(error)
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

%% arg1: Shell_state::#cst{}
%% arg2: Cmd_pipe, a list of commands e.g. ["ps", "ls dir"...]
%% returns: {internal, #cst{}, Message} | external | exit
%% where Message is printed back to the user before the prompt
%%-------------------------------------
%% tail: check the syntax
%%-------------------------------------
check_cmds([#cmd{path = "/misc",
		 name = "tail",
		 args = ["-n", Int]} | Rest],
	   S) ->
    try
	_ = list_to_integer(Int),
	check_cmds(Rest, S)	
    catch error:badarg ->
	    {internal, S, "\nIllegal attributes. Only '-n N' is allowed\n"}
    end;
check_cmds([#cmd{path = "/misc",
		 name = "tail",
		 args = []} | Rest],
	   S) ->
	check_cmds(Rest, S);
check_cmds([#cmd{path = "/misc",
		 name = "tail",
		 args = _Args} | _Rest],
	   S) ->
    {internal, S, "\nIllegal attributes. Only '-n N' is allowed\n"};
%%-------------------------------------
%% other commands but tail
%%-------------------------------------
check_cmds([#cmd{path = Path,
		 name = Name} | Rest],
	   #cst{all_paths = AllPaths} = S) ->
  case lists:member(Path, AllPaths) of 
      true -> 
	  %% a "real" command skip it here
	  check_cmds(Rest, S);
      false ->
	  %% the command is not awailable on this fru
	  {internal, S, ["\nno such command: ", string:join([Path, Name], "/")]}
  end;
check_cmds([Command | _], S) ->
    %% an "internal" command execute it and skip all the reset (before and after)
    p("## Command   ~p~n", [Command]), 
    [Cmd | Args] = Command,
    Base = ecoli_lib:basename(Cmd),
    case Base of
	"exit" ->
	    exit;
	"cd" -> %% this clause needs some more love (alot!)
	    {Level, Message} = normalize_level(ecoli_lib:soft_hd(Args),
					       S#cst.level,
					       S#cst.all_paths),
	    {internal, S#cst{level = Level}, Message};
	"pwd" -> 
	    {internal, S, ["\n", S#cst.level]};
	".." -> 
	    Level = ecoli_lib:dirname(S#cst.level),
	    {internal, S#cst{level = Level}, []};
	"ls" -> 
	    {internal, S, present_dir(S, Args)};
	"help" -> 
%%	    HelpMessage = get_help(S, ecoli_lib:soft_hd(Args)),
	    HelpMessage = get_help(S, string:join(Args, " ")),
	    {internal, S, HelpMessage};
	_ ->
	    Path        = ecoli_lib:build_path(S#cst.level, Cmd),
	    MaybePath   = remove_last_slash(Path),
	    _MaybeFru    = get_maybe_fru(MaybePath),
	    MaybeDu     = string:join(Command, " "),
	    FruNames    = [Name || {_, Name} <- S#cst.fru_names],
	    FruAccNames = [Name || {_, Name} <- S#cst.fruacc_names],
	    p("## Path   ~p~n", [Path]), 
	    p("## MaybeDu   ~p~n", [MaybeDu]), 
	    p("## MaybePath ~p~n  ~p~n", [MaybePath, S#cst.all_paths]), 
	    p("## MaybeFru  ~p~n  ~p~n", [_MaybeFru, FruNames]), 
	    p("## MaybeFruacc  ~p~n  ~p~n", [MaybeDu, FruAccNames]), 
	    check_cmd(lists:member(MaybePath, S#cst.all_paths),
		      lists:member(MaybeDu,  FruNames),
		      lists:member(MaybeDu,  FruAccNames),
		      MaybePath,
		      MaybeDu,
		      Cmd,
		      S) 
    end;
check_cmds([], _) ->
    external.


check_cmd(true, false, false, NewPath, _, _, S) ->
    {internal, S#cst{level = NewPath}, []};
check_cmd(false, FruNames, FruaccNames, _, NewFru, _, S)
  when FruNames orelse FruaccNames ->
    FruData = #current_fru{role      = ecoli_lib:get_fru_type(NewFru), 
			   fru_id    = NewFru,
			   cli_types = cc_get_fru_type(NewFru),
			   paths     = []},
    
    CliType  = ecoli_lib:get_cli_type(S#cst.fru_names, S#cst.fruacc_names, NewFru),
    AllPaths = ecoli_lib:get_all_paths(S#cst.auth, NewFru, CliType),

    {internal,
     S#cst{current_fru = FruData,
	   all_paths   = AllPaths,
	   level       = "/"},
     []};
check_cmd(_, _, _, _, _, Cmd, S) ->
    {internal, S, ["\nno such command: ", Cmd]}. 


cc_get_fru_type(NewFru) ->
    case [FT || #fruacc{key  = {FT, _},
			ldns = LDNS} <- ets:tab2list(ecoli_fruacc),
		lists:member(NewFru, LDNS)] of
	[] -> ["du"];
	F  -> F
    end.
    

get_maybe_fru(MaybePath) ->
    case lists:reverse(string:tokens(MaybePath, "/")) of
	[]             -> [];
	[MaybeFru | _] -> MaybeFru
    end.



%arg1 is the argument to "cd ARG"
%arg2 is the current level in the shell
%arg2 is a list of all available paths in the cmd-tree
%returns {Possibly_new_level, Message_to_print}
normalize_level([], Old_level, _) -> %cd -> just print CWD
    {Old_level, ["\n", Old_level]};
normalize_level("..", Old_level, _) -> %cd .. -> step down in tree
    {ecoli_lib:dirname(Old_level), []};
normalize_level("/", _, _) -> %absolute / go to /
    {"/", []};
normalize_level([$/ | _] = P, Old_level, All_paths) ->
    %absolute /.... look it up in the list
    New_p = remove_last_slash(P),
    case lists:member(New_p, All_paths) of
	true ->
	    {New_p, []};
	false ->
	    {Old_level, ["\n", "no such directory: ", P]}
    end;
normalize_level(L, Old_level, All_paths) ->
    %relative keep checking
    normalize_level(ecoli_lib:join(Old_level, L), Old_level, All_paths).
%normalize_level(Bad, Old_level, _) -> %can we ever get to this clause?
%    {Old_level, ["\n", "Bad path: ", Bad]}.

%FIXME this should perhaps be wrapped into ecoli_lib:build_path(...)?
%if the path ends with "/" remove it to enable a match in the path list
%if it is already "/" pass it thru unchanged
remove_last_slash([$/]) -> [$/];
remove_last_slash(Other) -> remove_l_s(Other).
remove_l_s([$/]) -> [];
remove_l_s([H | T]) -> [H | remove_l_s(T)];
remove_l_s([]) -> [].

%present_dir(#cst{level = Level, auth = Auth}, Args) ->
present_dir(#cst{level = Level, auth = Auth, all_paths = All_paths}, Args) ->
    Paths =
	case Args of
	    [] ->
		[Level];
	    _ ->
		[case lists:reverse(Arg) of
			"/" ->
			    ecoli_lib:build_path(Level, Arg);
			"/" ++ Rest ->
			    ecoli_lib:build_path(Level, lists:reverse(Rest));
			_ ->
			    ecoli_lib:build_path(Level, Arg)
		end || Arg <- Args]
	end,
    Dirs = [present_dir(Dir, Auth, All_paths) || Dir <- Paths],
    Dirs.

present_dir(Dir, Auth, All_paths) ->
    case lists:member(Dir, All_paths) of
	true ->
	    [
		"\n", Dir, ":\n",
		%%[[D, " "] || D <- ecoli_lib:matching_paths(Dir, All_paths)],
		[[D, " "] || D <- dirs_at_path(Dir, All_paths)],
		[[C, " "] || {_, C} <- ecoli_datainit:cmds_at_path(Dir, Auth)]
	    ];
	false ->
	    ["\nno such directory: ", Dir]
    end.

get_help(_, Shell) when Shell =:= "shell"; Shell =:= "coli"  ->
    ["\n\n",
     "========================\n"
     "RCS-Coli cmd shell usage\n"
     "========================\n"
     "\n"
     "Use <TAB> to expand and see available commands and FRUs.\n"
     "Available commands are listed in one row. \n"
     "Available FRUs are listed below the commands, one FRU LDN per row.\n"
     "\n"
     "Use \"cd\", \"pwd\", \"..\", and \"ls\" to navigate in the cmd-tree.\n"
     "\n"
     "Type FRU LDN to switch FRU.\n"
     "The (default) prompt is updated with the current FRU.\n"
     "\n"
     "History is available using <CTRL>-p and <CTRL>-n.\n"
     "\"exit\" - will exit the shell\n"
     "<CTRL>-C (ETX) will cause SIGINT to be sent to an external cmd.\n"
     "<CTRL>-D (EOT) will close stdin towards an external cmd.\n"
     "All internal commands are organized under \"" ?INTERNAL_DIR "\"\n"
     "\n"
     "To get help text for help write\n"
     "\"help help\"\n"
    ];

get_help(#cst{level = Level} = S, [])  ->
    get_dir_help(S, Level);
get_help(#cst{level = Level, all_paths = All_paths} = S, Arg)  ->
    Path = remove_last_slash(ecoli_lib:build_path(Level, Arg)),
    case lists:member(Path, All_paths) of
	true ->
	    get_dir_help(S, Path);
	false ->
	    get_cmd_help(S, Arg)
    end.

get_cmd_help(#cst{level = Level, auth = Auth}, Arg) ->
    case  ecoli_lib:lookup(Level, Arg, Auth) of
	[] ->
	    get_global_cmd_help(Arg, Auth);
	#coli_cmd{cli_pname = {_, Name},
		usage = Usage,
		description = Description} ->
	    ["\n", Name, "\n", Usage, "\n", Description, "\n"]
    end.

get_global_cmd_help(Maybe_cmd, Auth) ->
    case lists:keysearch(Maybe_cmd, 2, ecoli_datainit:global_cmds()) of
	{value, Pname} ->
	    #coli_cmd{usage = U, description = D} =
		ecoli_datainit:lookup(Pname, Auth),
	    ["\n", Maybe_cmd, "\n", U, "\n", D, "\n"];
	false ->
	    ["\n", "no such command: ", Maybe_cmd]
     end.

get_dir_help(#cst{auth = Auth} = S, Path) ->
    case ecoli_datainit:cmds_at_path(Path, Auth) of
	[] -> 
	    %% nothing here print the shell help
	    get_help(S, "shell");
	Cmds ->
	    [ "\n",
	      [begin
		   #coli_cmd{cli_pname = N, 
			     usage     = U} = ecoli_datainit:lookup(C, Auth),
		   io_lib:format("~-16s - ~s~n", [element(2, N), U])
	       end || C <- Cmds]
	     ]
    end.

grep(Regexp) ->
    Rexp = ecoli_lib:soft_hd(Regexp),
    do_grep(Rexp).

do_grep(Regexp) ->
    case io:get_line('') of
	eof ->
	    t("grep line", eof),
	    exit(normal);
	{error, What} ->
	    t("grep line Error", What),
	    exit(What);
	Line ->
	    t("grep line", Line),
	    case re:run(Line, Regexp) of
		nomatch ->
		    do_grep(Regexp);
		_ ->
		    io:put_chars(Line),
		    do_grep(Regexp)
	    end
    end.

tail(Args) ->
    t("TAIL", [Args]),
    do_tail(io:get_line(''), {queue:new(), 0}, tail_get_n(Args)).


do_tail(eof, {Q, _}, _) ->
    do_tail_print(queue:out(Q)),
    exit(normal);
do_tail({error, What}, {Q, _}, _) ->
    do_tail_print(queue:out(Q)),
    exit(What);
do_tail(Line, {Q, N}, Max) when N < Max ->
    do_tail(io:get_line(''), {queue:in(Line, Q), N + 1}, Max);
do_tail(Line, {Q, N}, Max) ->
    {_, Q2} = queue:out(Q), 
    do_tail(io:get_line(''), {queue:in(Line, Q2), N}, Max).
 

do_tail_print({{value, Line}, Q}) ->  
    io:put_chars(Line),
    do_tail_print(queue:out(Q));
do_tail_print({empty, _Q}) ->   
    ok.


tail_get_n([]) ->
    ?TAIL_DEF;
tail_get_n(["-n", N| _]) ->
    Int = list_to_integer(N),
    choose(Int > 0, Int, ?TAIL_DEF);
tail_get_n([_ | T]) ->
    tail_get_n(T).






t(_,_) ->
    ok.

%% return all Dirs that are in Dir, Dir is already checked that it is actually a valid
%% dir, only the first element (with "/" appended) of a dir is returned and the result
%% is usorted since the Dirs -list contain all dirs
%example: dirs_at_path("/s", ["/","/sys","/s","/s/duh","/s/did","/s/did/more"]
%returns: ["did/","duh/"]
dirs_at_path(Dir, Dirs) ->
    Matching = dirs_at_p(Dir, Dirs),
    lists:usort(Matching).

dirs_at_p(_, []) ->
    [];
dirs_at_p(Dir, [H | T]) ->
    dirs_at_p(string:tokens(Dir, "/"), string:tokens(H, "/"), Dir, T).

dirs_at_p([], [], Dir, T) ->
    dirs_at_p(Dir, T);
dirs_at_p(_, [], Dir, T) ->
    dirs_at_p(Dir, T);
dirs_at_p([Match | Rest1], [Match | Rest2], Dir, T) ->
    dirs_at_p(Rest1, Rest2, Dir, T);
dirs_at_p([], [Next | _], Dir, T) ->
    [Next ++ "/" | dirs_at_path(Dir, T)];
dirs_at_p(_, _, Dir, T) ->
    dirs_at_p(Dir, T).
    
%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

choose(true,  T, _) -> T;
choose(false, _, F) -> F.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
%do not use io:format since group_leader is sometimes pointing
%to something other than what is needed for io:format
%dbg(_Dbg_list) -> erlang:display({?MODULE, self(), Dbg_list}).



p(_Str, _Args) ->
    ok.
