%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ecoli_cmd_shell.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R9A/1

%%% @doc ==COLI==
%%% Callback module for shell parse/expand/eval
%%% Called from ecoli_ssh_channel
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(ecoli_cmd_shell).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R9A/1').
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
%%% R2A/2      2013-08-27   etxlg     Editing...
%%% R2A/3      2013-09-01   etxlg     Tab expansion moved to separate module
%%% R2A/4      2013-09-05   etxlg     Dynamic change of auth_level
%%% R2A/5      2013-10-14   etxlg     removed dbg function
%%% R2A/6      2014-02-02   etxlg     limit line length
%%% R2A/7      2014-02-24   etxlg     Support for changing the prompt
%%% R3A/1      2015-03-19   etxlg     Handle lonely "|" in input string
%%% R4A/16     2015-11-25   uabesvi   HU32982   ' and "
%%% R6A/1      2016-05-27   uabesvi   LDN + command caused a crash
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-include("ecoli.hrl").
-export([init/1, get_prompt/1, new_state/3, parse_input/2]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

init(AuthLevel) ->
    Node    = clhI:erlang_node(),
    %% NodeName is used to get all paths, instead of the FruType
    %% because the FruType may not be set and the default value
    %% is something else but the NodeName
    [NodeName | _] = string:tokens(atom_to_list(Node), "@"),
    FruNames   = get_fru_names(clhI:erlang_nodes(enabled), []),
    AllPaths   = ecoli_datainit:get_all_paths(AuthLevel, NodeName),
    FruId      = clhI:fru_id(),
    CurrentFru = #current_fru{role      = central,
			      fru_id    = "", 
			      mp_id     = clhI:mp_id(FruId),
			      cli_types = ["du"],
			      paths     = []      
		     },

    S = #cst{auth         = AuthLevel, 
	     all_paths    = AllPaths,
	     fru_names    = FruNames,
	     fruacc_names = ecoli_lib:get_all_fruaccs(),
	     current_fru  = CurrentFru,
	     start_fru    = {clhI:fru_id(), ["du"], AllPaths}},
    {S, make_prompt(S)}.


new_state(level, Shell_state, Level) ->
    Shell_state#cst{level = Level};
new_state(set_prompt, Shell_state, []) ->
    #cst{prompt = Default} = #cst{}, %% back to default
    Shell_state#cst{prompt = Default};
new_state(set_prompt, Shell_state, New_prompt) ->
    White_spaced = string:join(New_prompt, " "),
    New_lined = [re:replace(White_spaced, "\\\\n", "\n", [global])],
    Fixed = [re:replace(New_lined, "\\\\s", "\s", [global])],
    Shell_state#cst{prompt = Fixed};
new_state(auth, 
	  #cst{current_fru = #current_fru{fru_id    = FruId,
					  cli_types = Type}} = S,
	  Auth) ->
    S#cst{auth      = Auth, 
	  all_paths = ecoli_lib:get_all_paths(Auth, FruId, Type)}.

%%==============================================================================
%% parse_input(Data, S) -> {Action, NewState, OutputIolist, ExecuteCmd}
%% 
%%  Action       = ok | execute | stop
%%  NewState     = #cst{} 
%%  OutputIolist = [string() | OutputIolist]
%%  ExecuteCmd   = [] | [#cmd{}]
%%==============================================================================
-spec parse_input(Data::string(), S::#cst{}) -> 
	{ok | execute | stop, #cst{}, iolist(), string()}.
	%% {atom(), #cst{}, iolist(), string()}.
parse_input(Data, 
	    #cst{auth         = _Auth,
		 all_paths    = AllPaths,
		 fru_names    = FruNames,
		 fruacc_names = FruAccNames,
		 current_fru  = #current_fru{fru_id = FruId},
		 start_fru    = {_StartFruId, _StartFruType, StartPaths}} = S) ->
    _FS = format_state(S),
    %% io:format("#### state ~n~p~n", [format_state(S)]),
    IsFruIdValid = is_fruid_valid(FruId, FruNames ++ FruAccNames),

    NS = S#cst{fru_names    = get_fru_names(clhI:erlang_nodes(enabled), []),
	       fruacc_names = ecoli_lib:get_all_fruaccs(),
	       all_paths    = choose(IsFruIdValid, AllPaths, StartPaths)
	      },

    {Action, NewState, OutputIolist, ExecuteCmd} = 
	inp(Data, pi(NS, IsFruIdValid), []),
%%	inp(ecoli_lib:update_me_id(Data, "1"), pi(NS, IsFruIdValid), []),

    #cst{auth         = NewAuth,
	 fru_names    = NewFruNames,
	 fruacc_names = NewFruAccNames,
	 current_fru  = #current_fru{fru_id = NewFruId}} = NewState,
    CliType  = ecoli_lib:get_cli_type(NewFruNames, NewFruAccNames, NewFruId),
    NewAllPaths = ecoli_lib:get_all_paths(NewAuth, NewFruId, CliType),
    {Action, NewState#cst{all_paths = NewAllPaths}, OutputIolist, ExecuteCmd}.


%%------------------------------------------------------------------------------
%% The Old FruId does not exist no more. Change to the central node. 
%%
%% In this case the prompt should be the old R15B prompt (without current FruID).
%%------------------------------------------------------------------------------
pi(#cst{current_fru = #current_fru{fru_id = [] = _FruId} = _FruData,
        start_fru   = _StartFru} = S,
   false) ->
    S;
%%------------------------------------------------------------------------------
%% fru_data FruId == []
%%
%% In this case the prompt should be the old R15B prompt (without current FruID).
%%------------------------------------------------------------------------------
pi(#cst{current_fru  = #current_fru{fru_id = [] = FruId} = FruData,
	fru_names    = FruNames,
	fruacc_names = FruAccNames,
        start_fru    = {StartFruId, StartFryType, _} = SF} = S,
   _) ->
    OwnFruId = clhI:fru_id(clhI:erlang_node()),
    StartFru = [{StartFryType, StartFruId}],
    Member   = lists:keymember(FruId, 2, (FruNames -- StartFru) ++ FruAccNames),
    NewFruId = choose(Member, OwnFruId, FruId),
    %%io:format("#### PI 1   ~p   ~p~n", [OwnFruId, NewFruId]),
    S#cst{current_fru  = FruData#current_fru{fru_id = NewFruId,
					     mp_id  = clhI:mp_id(NewFruId)},
	  fru_names    = get_fru_names(clhI:erlang_nodes(enabled), []),
	  fruacc_names = ecoli_lib:get_all_fruaccs(),
	  start_fru    = SF};
	  %%start_fru    = choose(NewFruId == [], StartFru, [])};
%%------------------------------------------------------------------------------
%% fru_data FruId =/= []
%%
%% Use the new format of the prompt, because the operator has
%% set the current FruId.
%%------------------------------------------------------------------------------
pi(#cst{current_fru  = #current_fru{fru_id = FruId} = FruData,
	fru_names    = FruNames,
	fruacc_names = FruAccNames} = S,
   _) ->
    OwnFruId = clhI:fru_id(clhI:erlang_node()),
    %%io:format("#### PI 2   ~p   ~p~n", [FruId, FruNames ++ FruAccNames]),
    Member   = lists:keymember(FruId, 2, FruNames ++ FruAccNames),
    NewFruId = choose(Member, FruId, OwnFruId),
    %%io:format("#### PI 2 OwnFruId = ~p  NewFruId = ~p~n", [OwnFruId, NewFruId]),
    S#cst{current_fru  = FruData#current_fru{fru_id = NewFruId,
					     mp_id  = clhI:mp_id(NewFruId)},
	  fru_names    = get_fru_names(clhI:erlang_nodes(enabled), []),
	  fruacc_names = ecoli_lib:get_all_fruaccs()}.



is_fruid_valid([], AllFruNames) ->
    is_fruid_valid(clhI:fru_id(), AllFruNames);
is_fruid_valid(FruId, AllFruNames) ->
    lists:keymember(FruId, 2, AllFruNames).


format_state(S) ->
    F       = record_info(fields, cst),
    [_ | L] = tuple_to_list(S), 
   lists:zip(F,L).
    
%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%% need to handle Rest at every clause! Not!
%% there are limits, some stuff work other doesn't
%% multicharacter codes are recognized only if they arrive all together
%% parsing is terminated and the Rest is discarded when encountering
%% <tab>, any history character, ETX, CR, or LF
inp(_, #cst{count = Count} = S, _) when Count >= ?MAXLINELEN ->
    {stop, S, ["Maximum(", integer_to_list(?MAXLINELEN),
		") input line length exceeded"], []};
inp([], S, Echo) ->  %% nothing more return what we have and wait for more
    {ok, S, lists:reverse(Echo), []};
%% inp([?ETX | _], S, _) ->  %% CTRL-C remove or handle when done
%%    {stop, S, ["\nbi-bi\n"], []};
inp([Crlf | _], S=#cst{acc = Acc}, Echo)
            when Crlf =:= $\n ; Crlf =:= $\r -> %% eol

    inp_nl(r(Acc), S, Echo);
inp([$\t | _], S = #cst{acc = R_acc}, Echo) ->
    {New_acc, Expansions} = ecoli_tab_expand:command_expand(S, R_acc),
    {ok,
     bump_char_count(S#cst{acc = New_acc}),
     [Expansions,
      lists:reverse(Echo),
      make_prompt(S),
      lists:reverse(New_acc)],
     []};
%% delete on empty line
inp([C | Rest], S = #cst{acc = []}, Echo) when C =:= $\d; C =:= $\b ->
    %{ok, S, [?BEL], []};
    inp(Rest, bump_char_count(S), [?BEL | Echo]);
%% delete/backspace
inp([C | Rest], S = #cst{acc = Acc}, Echo) when C =:= $\d; C =:= $\b ->
    %{ok, S#cst{acc = tl(Acc)}, rubout(), []};
    inp(Rest, bump_char_count(S#cst{acc = tl(Acc)}), [rubout() | Echo]);
inp([16 | _], S = #cst{}, _) -> %CTRL-P -> history
    {Prev, New_state} = history_get_prev(S),
        {ok, bump_char_count(New_state), Prev, []};
inp([14 | _], S = #cst{}, _) -> %CTRL-N -> history
    {Next, New_state} = history_get_next(S),
        {ok, bump_char_count(New_state), Next, []};
inp([27, $[, $A | _], S = #cst{}, _) -> %Arrow-Up -> history
    {Prev, New_state} = history_get_prev(S),
        {ok, bump_char_count(New_state), Prev, []};
inp([27, $[, $B | _], S = #cst{}, _) -> %Arrow-Down -> history
    {Next, New_state} = history_get_next(S),
        {ok, bump_char_count(New_state), Next, []};
inp([27, _, _ | Rest], S, Echo) -> %Arrow-Whatever -> discard
    inp(Rest, bump_char_count(S), Echo);
inp([C | Rest], S, Echo) when C < $\s; C > $\d -> %discard
    %% accept ascii 32-127 inclusive
    inp(Rest, bump_char_count(S), Echo);
inp([C | Rest], S = #cst{acc = Acc}, Echo) ->
    inp(Rest, bump_char_count(S#cst{acc = [C| Acc]}), [C | Echo]).





inp_nl([$/ | _], S, Echo) ->
    inp_nl_same_fru(S, Echo);
inp_nl(Acc, S, Echo) ->
    inp_nl_check(string:tokens(Acc, " "), S, Echo).


inp_nl_check(Acc, 
	     #cst{fru_names    = Frus, 
		  fruacc_names = FruAccs} = S, 
	     Echo) 
  when length(Acc) > 1 ->
    case lists:keysearch(hd(Acc), 2, Frus ++ FruAccs) of
	{value, FruData}->
	    inp_nl_other_fru(FruData, S, Echo);
	_ ->
	    inp_nl_same_fru(S, Echo)
    end;
inp_nl_check(_, S, Echo) ->
    inp_nl_same_fru(S, Echo).




%%============================================================================
%% The command is run on the current FRU
%%============================================================================
inp_nl_same_fru(#cst{acc = RAcc} = S, Echo) ->
    NewState = add_to_history(S#cst{acc = []}, r(RAcc)),
    case parse_cmd_pipe(S, RAcc, []) of  
	[] ->
	    {ok, 
	     bump_char_count(NewState),
	     [lists:reverse(Echo),
	      make_prompt(NewState)],
	     []};
	PipeList ->
	    case ecoli_internal_cmd:evaluate_cmd(NewState, PipeList) of
		{internal, LastState, Message} ->
		    {ok,
		     zero_char_count(LastState),
		     [lists:reverse(Echo),
		      Message,
		      make_prompt(LastState)],
		     []};
		external ->
		    {execute, zero_char_count(NewState), [$\n], PipeList};
		exit ->
		    {stop, NewState, ["\n"], []}
	    end
    end.


%%============================================================================
%% The command is run on another FRU
%%============================================================================
inp_nl_other_fru({Type, FruId},
	   #cst{acc  = RAcc,
		auth = Auth} = S,
	   Echo) ->
    NewState = add_to_history(S#cst{acc = []}, r(RAcc)),

    TmpFru = #current_fru{role      = fruacc,
			  fru_id    = FruId,
			  mp_id     = undefined,
			  cli_types = [Type],
			  paths     = []},
    
    %% Use a temporary state where the FRU is removed from the input line
    %% and the current_fru is temporarily updated with the values of
    %% the FRU data of the FRU where the command is to be execued.
    FruState = NewState#cst{current_fru = TmpFru,
			    all_paths   = ecoli_lib:get_all_paths(Auth, FruId, [Type])},
    case parse_cmd_pipe(FruState, r(r(RAcc) -- FruId), []) of  
	[] ->
	    {ok, 
	     bump_char_count(NewState),
	     [lists:reverse(Echo),
	      make_prompt(NewState)],
	     []};
	PipeList ->
	    case ecoli_internal_cmd:evaluate_cmd(FruState, PipeList) of
		{internal, LastState, Message} ->
		    {ok,
		     zero_char_count(LastState),
		     [lists:reverse(Echo),
		      Message,
		      make_prompt(LastState)],
		     []};
		external ->
		    {execute, zero_char_count(NewState), [$\n], PipeList};
		exit ->
		    {stop, NewState, ["\n"], []}
	    end
    end.







bump_char_count(#cst{count = Count} = S) ->
    S#cst{count = Count + 1}.
zero_char_count(S) ->
    S#cst{count = 0}.


%%===========================================================================
%% takes a reversed "cmd pipe list", e.g "ls | grep filename"
%% (it is in reverse order)
%% divides it on the "|" character and returns them in a list of:
%%  #cmd{} if found in cmd-db
%% unchanged if not, i.e. as ["cmd", "arg1", "argn"...]
%% above example would return [{cmd, "grep", Cli_path, "filename"},
%%   {cmd, "ls", Cli_path2, ""}],
%% note that the order between elements remain reversed 
%%===========================================================================
%% get rid of white space
parse_cmd_pipe(S, [ $\s | Rest], Acc) -> 
    parse_cmd_pipe(S, Rest, Acc);
%% This is a special case when the MP has not been associated with an LDN
parse_cmd_pipe(_, ?DEFAULT_MP_ID_REVERSED, _Acc) ->
    [[?DEFAULT_MP_ID]];
parse_cmd_pipe(_, [$|], Acc) ->
    lists:reverse(Acc);
parse_cmd_pipe(_, [], Acc) ->
    lists:reverse(Acc);
parse_cmd_pipe(S, Input, Acc) ->
    {Sofar, Remaining} = parse_cmd_pipe_cont(Input, []),
    case mk_cmd(S, Sofar) of
	[]  -> parse_cmd_pipe(S, Remaining, Acc);
	Cmd -> parse_cmd_pipe(S, Remaining, [Cmd | Acc])
    end.


%% command ending with |
parse_cmd_pipe_cont([$| | Remaining], []) -> 
    parse_cmd_pipe_cont(Remaining, []);
parse_cmd_pipe_cont([$| | Remaining], Sofar) ->
    {Sofar, Remaining};
parse_cmd_pipe_cont([H | T], Sofar) ->
    parse_cmd_pipe_cont(T, [H | Sofar]);
parse_cmd_pipe_cont([], Sofar) ->
    {Sofar, []}.



inp_parse([], _, Attr, Acc) ->
    [L || L <- lists:reverse([lists:reverse(Attr) | Acc]), length(L) > 0];
inp_parse([?SPACE | T], undefined, Attr, Acc) ->
    inp_parse(T, undefined, [], [lists:reverse(Attr) | Acc]);
inp_parse([?SINGLE_QUOTE = H | T], undefined, Attr, Acc) ->
    inp_parse(T, H, [], [lists:reverse(Attr) | Acc]);
inp_parse([?SINGLE_QUOTE | T], ?SINGLE_QUOTE, Attr, Acc) ->
    inp_parse(T, undefined, [], [lists:reverse(Attr) | Acc]);
inp_parse([H | T], Char, Attr, Acc) ->
    inp_parse(T, Char, [H | Attr], Acc).
    

mk_cmd(S, Command) ->
    CmdTokens = inp_parse(Command, undefined, [], []),
    Cmds = mk_cmds(CmdTokens, [], []),
    case mk_cmd_check(Cmds, S) of
	{ok, #cmd{} = ColiCmd} -> ColiCmd;
	_                      -> CmdTokens
    end.


%% build all possible combinations of command vs attributes,
%% used for checking the longest possible valid command
mk_cmds([], _Cmd, Acc) ->
    Acc;
mk_cmds([H | T], [], Acc) ->
    mk_cmds(T, H, [{H, T} | Acc]);
mk_cmds([H | T], Cmd, Acc) ->
    Cmd2 = Cmd ++ " " ++ H,
    mk_cmds(T, Cmd2, [{Cmd2, T} | Acc]).


mk_cmd_check([], _S) ->
    [];
mk_cmd_check([H], S) ->
    R = do_mk_cmd(H, S),
%%     io:format("###### mk_cmd check  ~p   ~p ~n", [H, R]),
    R;
%%    do_mk_cmd(H, S);
mk_cmd_check([H | T], S) ->

    R = do_mk_cmd(H, S),
%%     io:format("###### mk_cmd check  ~p   ~p ~n", [H, R]),

    case R of
	{ok, _} = OK -> OK;
	_            -> mk_cmd_check(T, S)
    end.


%% do_mk_cmd(Command, S) ->
%%     [Cmd | Args] = Unchanged = string:tokens(Command, " "),
do_mk_cmd({Cmd, Args}, S) ->
%%     io:format("###### do_mk_cmd   ~p ~n", [{Cmd, Args}]),
    {CliName, CliPath} =
	case Cmd of
	    [$/ | _] -> %absolute
		{ecoli_lib:basename(Cmd), ecoli_lib:dirname(Cmd)};
	    _ -> %relative
		Full = ecoli_lib:join(S#cst.level, Cmd),
		{ecoli_lib:basename(Full), ecoli_lib:dirname(Full)}
	end,
%%     io:format("###### do_mk_cmd split  ~p ~n", [{Cli_name, Cli_path}]),
    case ecoli_datainit:lookup({CliPath, CliName}, S#cst.auth) of
	#coli_cmd{type = coli_internal}  ->
	    {error, not_found};
	#coli_cmd{}  ->
	    OwnFru =  clhI:fru_id(clhI:mp_id(clhI:erlang_node())),
	    [NoSlashCmd | _] = string:tokens(Cmd, "/"),
	    Remote = get_remote_node(S, OwnFru, S#cst.level, "/" ++ NoSlashCmd),
	    {ok, #cmd{name        = CliName, 
		      path        = CliPath, 
		      args        = Args,
		      remote_node = Remote}};
	_ ->
	    {error, not_found}
    end.

%% Central DU
get_remote_node(#cst{current_fru = #current_fru{fru_id = FruId}}, FruId, _, _) ->
    local;
%% All internal cmds are run on central DU
get_remote_node(_, _, ?INTERNAL_DIR, _) ->
    local;
get_remote_node(_, _, _, ?INTERNAL_DIR) ->
    local;
get_remote_node(#cst{current_fru = #current_fru{mp_id = MpId}}, _, _, _) 
  when is_integer(MpId) ->
    clhI:erlang_node(MpId);
get_remote_node(#cst{current_fru = #current_fru{fru_id = []}}, _, _, _) ->
    clhI:erlang_node();
get_remote_node(#cst{current_fru = #current_fru{fru_id = FruId,
						mp_id  = undefined}}, _, _, _) ->
    FruId.



get_fru_names([], Acc) ->
    lists:reverse(Acc);
get_fru_names([H | T], Acc) ->
    FruName = clhI:fru_id(H),
    get_fru_names(T, [{"du", FruName} | Acc]).

    
%%======================================================================
%% get_prompt
%%======================================================================
get_prompt(ShellState) ->
    [_ | WithoutNewLine ] = make_prompt(ShellState),
    WithoutNewLine.


%%======================================================================
%% make_prompt
%%======================================================================
%% make_prompt(#cst{prompt   = Prompt, 
%% 	       level       = Level,
%% 	       current_fru = #current_fru{role   = local, 
%% 				          fru_id = "",
%% 				          type   = "",
%% 				          paths  = []
%% 				          }}) ->
%%     P1 = re:replace(Prompt, "\\\\w", Level, [global]),
%%     P2 = re:replace(P1, "\\\\h", [], [global]),
%%     ["\n", P2];
make_prompt(#cst{fru_names    = FruNames,
		 fruacc_names = FruAccNames,
		 current_fru  = #current_fru{fru_id = FruId}} = S) ->

    AllFruNames = [Name || {_, Name} <- FruNames ++ FruAccNames],
%%    io:format("### make_prompt  ~p  ~p~n", [FruId, AllFruNames]),
    make_p(lists:member(FruId, AllFruNames), S).

%%----------------------------------------------------------------------
%% Normal case:
%%  - FruId is found amongst the FruNames and FruId has been set or
%%  - FruId has never been set 
%%    (in this case it is not found among all FruNames)
%%----------------------------------------------------------------------
make_p(Member,
     #cst{prompt      = Prompt, 
	  level       = Level,
	  current_fru = #current_fru{fru_id = FruId}}) 
  when (Member andalso FruId /= []) orelse FruId == [] ->
%%     io:format("### make_prompt  1 ~n"),
    Fid = choose(FruId == [], [], FruId ++ ": "),
    P1  = re:replace(Prompt, "\\\\w", Level, [global]),
    P2  = re:replace(P1, "\\\\h", Fid, [global]),
    ["\n", P2];
%%----------------------------------------------------------------------
%% If FruId has been set but has disapeared
%%----------------------------------------------------------------------
make_p(_,
     #cst{prompt    = Prompt, 
	  start_fru = {StartFruId, _, _}}) ->
%%     io:format("### make_prompt  2 ~n"),
    Fid = choose(StartFruId == [], [], StartFruId ++ ": "),
    P1  = re:replace(Prompt, "\\\\w", "/", [global]),
    P2  = re:replace(P1, "\\\\h", Fid, [global]),
    ["\n", P2].



%%======================================================================
%% Check if the current fru id is still existing or if it has changed
%% name. In the latter case change to the central fru id because
%% we do not know to what the name was changed.
%%======================================================================
%% check_fru_id(#cst{fru_data     = #fru_data{id = FruId},
%% 		  fru_names    = FruNames,
%% 		  fruacc_names = FruAccNames} = S) ->
%%     FN  = [Name || {_, _, Name} <- FruNames],
%%     FAN = [Name || {_, _, Name} <- FruAccNames],
%%     cfi(lists:member(FruId, FN ++ FAN), S).

%% cfi(true, S) ->
%%     S;
%% cfi(false, #cst{fru_data = FruData} = S) ->
%%     FruId = clhI:fru_id(clhI:erlang_node()),
%%     S#cst{fru_data = FruData#fru_data{id = FruId}}.


%% return: {To_display, New_state}
history_get_next(S =  #cst{history = {0, _, _, _}}) ->
    {[?BEL], S};
history_get_next(S =  #cst{history = {_, [], _, _}}) ->
    {[?BEL], S};
history_get_next(S =  #cst{history = {_, [_], _, _}}) ->
    {[?BEL], S};
history_get_next(S =  #cst{history = {N, [Bh | Bt], After, All},
        acc = Acc}) ->
    N_state = S#cst{history = {N, Bt, [Bh | After], All},
        acc = lists:reverse(hd(Bt))},
    {[rubout(Acc), hd(Bt)], N_state}.

%% return: {To_display, New_state}
history_get_prev(S =  #cst{history = {0, _, _, _}}) ->
    {[?BEL], S};
history_get_prev(S =  #cst{history = {N, [], [], All}, acc = Acc}) ->
    [H | T] = lists:reverse(queue:to_list(All)),
    N_state = S#cst{history = {N, [H, lists:reverse(Acc)], T, All},
        acc = lists:reverse(H)},
    {[rubout(Acc), H], N_state};
history_get_prev(S =  #cst{history = {_, _, [], _}}) ->
    {[?BEL], S};
history_get_prev(S =  #cst{history = {N, Before, [H | T], All}, acc = Acc}) ->
    N_state = S#cst{history = {N, [H | Before], T, All},
    acc = lists:reverse(H)},
    {[rubout(Acc), H], N_state}.

%% return updated #cst{}
%% add_to_history(S, "history") -> %seems useless to add
%%    S;
add_to_history(S, []) ->
    S;
add_to_history(S = #cst{history = {N, _, _, H_queue}}, Pipe_list) ->
    Last_cmd = queue:peek_r(H_queue),
    {NN, N_queue} =
    if
        {value, Pipe_list} =:= Last_cmd ->
            {N, H_queue};
        N >= ?HISTORYLENGTH ->
            {N, queue:drop(queue:in(Pipe_list, H_queue))};
        true ->
            {N + 1, queue:in(Pipe_list, H_queue)}
    end,
    S#cst{history = {NN, [], [], N_queue}}.

rubout() -> 
    [?BS, ?SPACE, ?BS].
rubout(Number) when is_integer(Number) ->
    lists:duplicate(Number, rubout());
rubout(String) ->
    [rubout() || _ <- String].



 

choose(true,  T, _) -> T;
choose(false, _, F) -> F.

r(List) -> lists:reverse(List).

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
