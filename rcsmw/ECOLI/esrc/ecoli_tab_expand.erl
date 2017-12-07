%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ecoli_tab_expand.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2013-2016
%%% @version /main/R2A/R4A/R5A/R7A/2

%%% @doc ==COLI==
%%% Handles "tab-expansion".
%%% Called from ecoli_cmd_shell
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(ecoli_tab_expand).
-vsn('/main/R2A/R4A/R5A/R7A/2').
-date('2016-11-03').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
%%% R2A/1      2013-10-02   etxlg     Split out out from ecoli_cmd_shell
%%% R2A/2      2013-10-14   etxlg     removed dbg function
%%% R2A/3      2014-01-31   etxlg     Improved behaviour at single match cmd|dir
%%% ----------------------------------------------------------
%%% R7A/2      2016-11-03   uabesvi   HV35899
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-include("ecoli.hrl").
-export([command_expand/2]).

-export([strip_level/2]).

%%-compile([export_all]).

-spec command_expand(S::#cst{}, R_acc::string()) -> {string(), iolist()}.
%%=============================================================================
%% command_expand(S, RAcc) -> {OutpuLine, Available}
%% 
%% S          = state
%% RAcc       = input cmd from the coli window (reversed)
%% OutputLine = output cmd to the coli window (reversed)
%% Available  = [string{}] - information about available continuations 
%%              of the command (not reversed)
%%=============================================================================
command_expand(#cst{level        = Level, 
		    all_paths    = AllPaths, 
		    auth         = Auth,
		    current_fru  = #current_fru{fru_id    = FruId,
					        cli_types = CliTypes}, 
		    fru_names    = FruNames,
		    fruacc_names = FruAccNames} = S,
	       RAcc) ->

%%     p("##### command_expand ~n~nRAcc = ~p~n ~n~s~n~n~n", [RAcc, ecoli_lib:pp(S)]),
    ecoli_lib:pp(S),

    {CmdDone, CmdNew, TabFru} = get_tab_fru(r(RAcc),
					    FruNames ++ FruAccNames, 
					    Auth),

    p("TabFru  ~p~n", [TabFru]),
    
    IsAbsolute = is_absolute(CmdNew),
    
    FullCmd = rm_tail_slash(add_level(string:strip(CmdNew), Level)),
    CmdPath = get_cmd_path(FullCmd),
    
    MatchingCmds = [get_matching_cmds(lists:member(FullCmd, AllPaths),
				      FullCmd,
				      CmdDone,
				      TabFru,
				      AllPaths, 
				      Auth, 
				      list_to_atom(CliType)) || CliType <- CliTypes],
    MatchingPaths = get_matching_paths(r(CmdNew), AllPaths, CmdPath, TabFru),
    MatchingFrus  = find_frus(FruId, 
			      FruNames ++ FruAccNames, 
			      CmdDone, 
			      CmdNew),
%%			      ecoli_lib:update_me_id(CmdNew)),
    
    p("~n ==================~n"
      " All            ~p~n"
      " FullCmd        ~p~n"
      " AllPaths       ~p~n"
      " CmdDone        ~p~n"
      " CmdNew         ~p~n"
      " IsAbsolute     ~p~n"
      " MatchingCmds   ~p~n"
      " MatchingPaths  ~p~n"
      " Frus           ~p~n"
      " Level          ~p~n"
      " ==================~n", 
      [RAcc, FullCmd, AllPaths, CmdDone, CmdNew, IsAbsolute, 
       lists:append(MatchingCmds), MatchingPaths, MatchingFrus, Level]),
    
    {CmdLine, Suggestions} = check_cmd(CmdNew, 
				       IsAbsolute, 
				       Level, 
				       lists:append(MatchingCmds), 
				       MatchingPaths, 
				       MatchingFrus),

    p("CmdLine      ~p~n", [CmdLine]),
    p("Suggestions  ~p~n", [Suggestions]),
    {r(CmdDone ++ CmdLine), Suggestions}.
    


%%=============================================================================
%% find_frus(FruId, FruNames, Before, After) -> MatchingFrus
%% 
%% get_frus that matches the charactes in After. 
%% It is not allowed change FRU after a | (pipe)
%%=============================================================================
find_frus(FruId, FruNames, [], Cmd) ->
    FN          = [Name || {_, Name} <- FruNames],
    AllFruNames = [Name || Name <- FN,  Name /= FruId],
    AllMatched  = lists:append([[AFN, "\n"] || AFN <- lists:usort(AllFruNames), 
					       lists:prefix(Cmd, AFN)]),
    LongestPrefix = get_common_prefix(AllMatched),
    
    ff_get_matched_frus(lists:prefix(LongestPrefix, Cmd),
			lists:prefix(Cmd, LongestPrefix),
			AllMatched,
			LongestPrefix);
find_frus(_, _, _, _) ->
    [].





ff_get_matched_frus(_, _, [], _LongestPrefix) ->
    [];
ff_get_matched_frus(false, false, _AllMatched, _LongestPrefix) ->
    [];
ff_get_matched_frus(_, _, AllMatched, _LongestPrefix) ->
    AllMatched.




%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%============================================================================
%% strip_level(Path1, Path2) -> Path
%%
%% Path1 is a path e.g. CWD
%% Path2 is a path that potentially has Path1 as prefix
%% 
%% Strip away Path1 from Path2 making sure that only whole path elements i.e.
%% those between "/" are removed, return the rest
%% ex ("/ada", "/ada/beda") -> "beda"
%% ex ("/ad", "/ada/beda") -> "ada/beda"
%%============================================================================
strip_level(A, B) ->
    real_strip_level(A, B).

real_strip_level("/", Rest) -> tl(Rest);
real_strip_level(Level, Path) ->
    strip_l(string:tokens(Level, "/"), string:tokens(Path, "/")).

strip_l([Match | Rest1], [Match | Rest2]) ->
    strip_l(Rest1, Rest2);
strip_l([], []) -> [];
strip_l(_, Path_elements) -> 
    string:join(Path_elements, "/").



%%============================================================================
%% get_matching_cmds(IsFullCmdInAbsolutePath, AbsolutePath, CmdDone, 
%%                   TabFru, AllPaths, Auth, CliType) -> [Cmds]
%%============================================================================
get_matching_cmds(true, AbsolutePath, CmdDone, undefined, _, Auth, CliType) ->
%%     p(" ++ get_matching_cmds  ~p~n", [{AbsolutePath, AllPaths, Auth}]),
    [ecoli_lib:join(P, C) || {P, C} <-
				 cmds_at_path(AbsolutePath, CmdDone, Auth, CliType)];
get_matching_cmds(false, AbsolutePath, CmdDone, undefined, _, Auth, CliType) ->
    Dir      = ecoli_lib:dirname(AbsolutePath), 
    Cmds     = cmds_at_path(Dir, CmdDone, Auth, CliType),
    BaseName = ecoli_lib:basename(AbsolutePath),
    [ecoli_lib:join(P, C) || {P, C} <- Cmds, 
			     lists:prefix(BaseName, C)];
get_matching_cmds(IsMember,
		  AbsolutePath, 
		  _CmdDone, 
		  #tab_fru{fru_id = _TabFruId,
			   type   = CliType,
			   paths  = AllPaths}, 
		  _AllPaths, 
		  Auth, 
		  _CliType) ->
    gmc(IsMember, AbsolutePath, "", AllPaths, Auth, CliType).



gmc(true, AbsolutePath, CmdDone, _AllPaths, Auth, CliType) ->
    [ecoli_lib:join(P, C) || {P, C} <-
				 cmds_at_path(AbsolutePath, 
					      CmdDone, 
					      Auth, 
					      CliType)];
gmc(false, AbsolutePath, CmdDone, _AllPaths, Auth, CliType) ->
    Dir      = ecoli_lib:dirname(AbsolutePath), 
    Cmds     = cmds_at_path(Dir, CmdDone, Auth, CliType),
    BaseName = ecoli_lib:basename(AbsolutePath),
    [ecoli_lib:join(P, C) || {P, C} <- Cmds, 
			     lists:prefix(BaseName, C)].

%%============================================================================
%% cmds_at_path(Path, AuthLevel, CliType) -> [{Path, Cmd}]
%% 
%% For du:s the CliType is undefined in #coli_cmd
%%============================================================================
cmds_at_path(?INTERNAL_DIR, _, _AuthLevel, _CliType) ->
    ecoli_datainit:cmds_at_path_internal();
cmds_at_path(Path, CmdDone, AuthLevel, du) ->
    cmds_at_path(Path, CmdDone, AuthLevel, undefined);
cmds_at_path(Path, CmdDone, AuthLevel, "du") ->
    cmds_at_path(Path, CmdDone, AuthLevel, undefined);
cmds_at_path(Path, "help ", AuthLevel, _CliType) ->
    Match_all = mnesia:table_info(coli_cmd, wild_pattern),
    Match = Match_all#coli_cmd{cli_pname     = {Path, '$1'},
			       type          = '$4',
			       authorization = '$3'},
    Cond  = [{'=<',  AuthLevel, '$3'},
	     {'=/=', undefined, '$4'}],
    mnesia:dirty_select(coli_cmd, [{Match, Cond, [{{Path, '$1'}}]}]);
cmds_at_path(Path, _, AuthLevel, CliType) ->
    Match_all = mnesia:table_info(coli_cmd, wild_pattern),
    Match = Match_all#coli_cmd{cli_pname     = {Path, '$1'},
			       cli_type      = '$5',
			       type          = '$4',
			       authorization = '$3'},
    Cond  = [{'=<',  AuthLevel, '$3'},
	     {'=/=', undefined, '$4'},
	     {'==',  CliType,   '$5'}],
    mnesia:dirty_select(coli_cmd, [{Match, Cond, [{{Path, '$1'}}]}]).




%%=============================================================================
%% rm_duplicates
%%=============================================================================
rm_duplicates(Level, Paths, Cmds) ->
    Sorted = lists:usort([extract_path_element(Level, P) || P <- Paths]),
    [
     [[P, "/ "] || P <- Sorted],
     lists:sort([[ecoli_lib:basename(C), "* "] || C <- Cmds])
    ].



% return the longest prefix found in Prefix_list (remove all nl)
% i.e. (["adam", "adab", "ada"]) -> "ada"
% but never return a singel "/"
get_common_prefix(List) ->
    List2 = [L || L <- List, L /= "\n"],
    case get_common_prefix2(List2) of
	"/" -> "";
	Any -> Any
    end.

get_common_prefix2([]) -> [];
get_common_prefix2([Prefix | Prefix_list]) ->
    get_common_prefix2(Prefix, Prefix_list, []).

get_common_prefix2([], _, _) ->
    [];
get_common_prefix2([P | Common], [], Done) ->
    [P | get_common_prefix2(Common, Done, [])];
get_common_prefix2(Common = [H | _], [[H | Rest] | More], Done) ->
    get_common_prefix2(Common, More, [Rest | Done]);
get_common_prefix2(_, _, _) ->
    [].

%arg1 is the current PWD
%arg2 is a path that has been found matching at current PWD OR
%     not in which case nothing is stripped
%strip away pathelements that are already in the Level and strip
%away any pathelements that are the one at Level
%ex: ("/vorpal/sword", "/vorpal/sword/x86/bin") -> "x86"
extract_path_element(Level, Path) ->
    extract_p_e(string:tokens(Level, "/"), string:tokens(Path, "/"), Path).

extract_p_e([Match | Rest], [Match | Rest2], Path) ->
    extract_p_e(Rest, Rest2, Path);
extract_p_e([], [First | _], _) ->
    First;
extract_p_e([], [], _) -> [];
extract_p_e(_, _, Path) -> Path.








add_level([$/ | _] = Cmd, _Level) ->
    Cmd;
add_level(Cmd, "/" = Level) ->
    Level ++ Cmd;
add_level(Cmd, Level) ->
    string:join([Level, Cmd], "/").



get_matching_paths([$/], AllPaths, _CmdPath, _TabFru) ->
    AllPaths -- ["/"];
get_matching_paths([$/ | _], _AllPaths, _CmdPath, _TabFru) ->
    [];
get_matching_paths(_, AllPaths, CmdPath, undefined) ->
    [MP || MP <- AllPaths, lists:prefix(CmdPath, MP)];
get_matching_paths(_, _AllPaths, CmdPath, #tab_fru{paths = AllPaths}) ->
    [MP || MP <- AllPaths, lists:prefix(CmdPath, MP)].





get_cmd_path(Cmd) ->
    Path = case string:tokens(Cmd, "/") of
	       [P | _] -> P;
	       _       -> []
    end,
    "/" ++ Path.


is_absolute([$/ | _]) -> true;
is_absolute(_)        -> false.




split_cmd(List) ->
    {Before, After} = split_at_last_pipe(List),
    sc(string:tokens(After, " "), {Before, After}).

sc([], Res) ->
    Res;
sc([FirstCmd | RemCmds], {Before, After}) ->
    sc_rc(lists:member(FirstCmd, ?INTERNAL_ALL_CMDS),
	  FirstCmd, 
	  RemCmds, 
	  Before, 
	  After).

sc_rc(true, FirstCmd, RemCmds, [], _After) ->
    {FirstCmd ++ " ", string:join(RemCmds, " ")};
sc_rc(true, FirstCmd, RemCmds, Before, _After) ->
    {Before ++ FirstCmd ++ " ", string:join(RemCmds, " ")};
sc_rc(false, _FirstCmd, _RemCmds, Before, After) ->
    {Before, After}.



split_at_last_pipe([]) ->
    {[], []};
split_at_last_pipe(List) ->
    salp(string:tokens(List, "|"), List).

salp([H], _List) ->
    {"", r(H)};
salp([H | _], List) -> 
    case r(H) of
	[32 |  R] -> 
	    {r(List -- H) ++ " ", R};
	RH ->
	    {r(List -- H), RH}
    end.


get_prefix(After, true, Cmds, _Frus) ->
    Check = [C || C <- Cmds, lists:prefix(After, C)],
    gp(Check, length(After));
get_prefix(After, false, Cmds, Frus) ->
    StrippedCmds = [Rest || [$/ | Rest] <- Cmds],
    Check = [C || C <- StrippedCmds ++ Frus, lists:prefix(After, C)],
    gp(Check, length(After)).


gp([], _) ->
    [];
gp([H | T], Min) ->
    gp3(T, H, Min).


gp3([], Prefix, _) ->
    Prefix;
gp3(_, Prefix, Min) when length(Prefix) == Min ->
    Prefix;
gp3([H | T], Prefix, Min) ->
    gp3(T, gp_check(Prefix, H, []), Min).

gp_check([], [], Acc) -> 
    lists:reverse(Acc);
gp_check([H | PreT], [H | NewT], Acc) -> 
    gp_check(PreT, NewT, [H | Acc]);
gp_check(_, _, Acc) -> 
    lists:reverse(Acc).
    




choose(true,  T, _) -> T;
choose(false, _, F) -> F.



%%============================================================================
%% check_cmd(CmdNew, IsAbsolute, Level, Cmds, Paths, Frus) -> 
%%   {CmdLine, Suggestions}
%%============================================================================

%%------------------------------------------------------------------
%% No input.
%%------------------------------------------------------------------
check_cmd([], 
	  _IsAbsolute, 
	  Level,
	  Cmds, 
	  Paths, 
	  Frus) ->
    p("### check_cmd.  No input.~n"),
    {[],
     ["\n", 
      rm_duplicates(Level, Paths, Cmds),
      ["\n"],
      Frus
     ]};

%%------------------------------------------------------------------
%% No matching cmd, path or fru.
%%------------------------------------------------------------------
check_cmd(CmdNew, 
	  _IsAbsolute, 
	  _Level, 
	  [], 
	  [], 
	  []) ->
     p("### check_cmd.  No matching cmd, path or fru~n"),
    {CmdNew, [?BEL]};

%%------------------------------------------------------------------
%% One matching Fru.
%%------------------------------------------------------------------
check_cmd(_CmdNew, 
	  _IsAbsolute, 
	  _Level, 
	  [], 
	  [], 
	  [Fru, "\n"]) ->
    p("### check_cmd.  One matching Fru. ~n"),
    {Fru ++ [$\s], []};

%%------------------------------------------------------------------
%% Several matching Frus.
%%------------------------------------------------------------------
check_cmd(_CmdNew, 
	  _IsAbsolute, 
	  _Level, 
	  [], 
	  [], 
	  Frus) ->
    p("### check_cmd.  Several matching Frus. ~n"),
    Prefix  = get_common_prefix(Frus),
    {Prefix, ["\n"] ++ Frus};
    
%%------------------------------------------------------------------
%% One matching command.
%%------------------------------------------------------------------
check_cmd(_CmdNew, 
	  IsAbsolute, 
	  Level, 
	  [Cmd], 
	  _, 
%%	  [], 
	  []) ->
    p("### check_cmd.  One matching command. ~n"),
    Leveled = choose(IsAbsolute, Cmd, strip_level(Level, Cmd)),
    {Leveled ++ [$\s], []};

%%------------------------------------------------------------------
%% Several matching commands.
%%------------------------------------------------------------------
check_cmd(_CmdNew, 
	  IsAbsolute, 
	  Level, 
	  Cmds, 
	  [], 
	  Frus) ->
    p("### check_cmd.  Several matching commands ~n"),
    Prefix  = get_common_prefix(Cmds),
    Leveled = choose(IsAbsolute, Prefix, strip_level(Level, Prefix)),
    {Leveled,
     ["\n", 
      rm_duplicates(Level, [], Cmds),
      ["\n"],
      Frus
     ]};

%%------------------------------------------------------------------
%% One matching Path.
%%------------------------------------------------------------------
check_cmd(_CmdNew, 
	  IsAbsolute, 
	  Level, 
	  [], 
	  [Path], 
	  []) ->
    p("### check_cmd.  One matching Path. ~n"),
    Leveled = choose(IsAbsolute, Path, strip_level(Level, Path)),
    {Leveled ++ [$/], []};

%%------------------------------------------------------------------
%% Several matching paths.
%%------------------------------------------------------------------
check_cmd(CmdNew, 
	  IsAbsolute, 
	  Level, 
	  [], 
	  Paths, 
	  Frus) ->
    p("### check_cmd.  Several matching paths. ~n"),
    Prefix = get_prefix(CmdNew, IsAbsolute, Paths, Frus),
    p("### check_cmd.  Several matching paths.  Prefix ~p ~n", [Prefix]),
    {Prefix, 
     ["\n", 
      rm_duplicates(Level, Paths, []),
      ["\n"],
      Frus
     ]};

%%------------------------------------------------------------------
%% All other cases.
%%------------------------------------------------------------------
check_cmd(CmdNew, 
	  IsAbsolute, 
	  Level, 
	  Cmds, 
	  [Path], 
	  Frus) ->
    p("### check_cmd.  Several commands, one path. ~n"),
    StrippedCmds = strip_cmds(Cmds, Level),
    Prefix = get_prefix(CmdNew, IsAbsolute, StrippedCmds ++ [Path], Frus),
    p("### check_cmd.             Prefix ~p ~n", [Prefix]),
    {Prefix, 
     ["\n", 
      rm_duplicates(Level, [Path], Cmds),
      ["\n"],
      Frus
     ]};

%%------------------------------------------------------------------
%% All other cases.
%%------------------------------------------------------------------
check_cmd(CmdNew, 
	  IsAbsolute, 
	  Level, 
	  _Cmds, 
	  Paths, 
	  Frus) ->
    p("### check_cmd.  Several cmds and paths. ~n"),
    Prefix = get_prefix(CmdNew, IsAbsolute, Paths, Frus),
    p("### check_cmd.   Prefix ~p ~n", [Prefix]),
    {Prefix, 
     ["\n", 
      rm_duplicates(Level, Paths, []),
      ["\n"],
      Frus
     ]}.


strip_cmds(Cmds, "/") ->
    Cmds;
strip_cmds(Cmds, Level) ->
    [strip_cmd(C, Level) || C <- Cmds].

strip_cmd(Cmd, Level) ->
    case lists:prefix(Level, Cmd) of
	true  -> Cmd -- Level;
	false -> Cmd
    end.




get_tab_fru(AccIn, FruNames, Auth) ->
    {CmdDone, CmdNew} = split_cmd(r(AccIn)),
    case AccIn of
	[$/ | _] ->
	    {CmdDone, CmdNew, undefined};
	Acc ->
	    gtf(string:tokens(Acc, " "), FruNames, Auth, {CmdDone, CmdNew})
    end.
	

gtf([], _FruNames, _Auth, {CmdDone, CmdNew}) ->
    {CmdDone, CmdNew, undefined};
gtf([PossiblyFru | Acc], FruNames, Auth, {CmdDone, CmdNew}) ->
    case lists:keysearch(PossiblyFru, 2, FruNames) of
	false ->
	    {CmdDone, CmdNew, undefined};
	{_, {Type, FruId}} -> 
	    Paths = ecoli_lib:get_all_paths(Auth, FruId, [Type]),
	    TabFru = #tab_fru{role   = fruacc,
			      fru_id = FruId,
			      mp_id  = undefined,
			      type   = Type,
			      paths  = Paths 
			     },
	    {PossiblyFru ++ " ", string:join(Acc, " "), TabFru}
    end.

    


%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
rm_tail_slash(S) ->
    case lists:reverse(S) of
	[$/ | R] when length(R) > 0 -> lists:reverse(R);
	_       -> S
    end.



r(List) ->
    lists:reverse(List).
%% r(L1, L2) ->
%%     lists:reverse(L1, L2).



%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

p(_S) ->
    ok.
p(_S,_A) ->
    ok.

%% p(S) ->
%%     io:format(S).
%% p(S,A) ->
%%     io:format(S,A).
