%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ecoli_datainit.erl %
%%% Author:     etxbjca
%%% Description:
%%% TODO: need to add upgrade code that cleans out the database (drops it!)
%%%	  ensure that it works (or fails gracefully) if log in happens
%%	  before the database is created (is this even possible?)
%%%
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(ecoli_datainit).
%%-behaviour(behaviour).
-id('Updated by CCase').
-vsn('/main/R2A/R4A/R5A/R8A/1').
-date('2017-01-09').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% R2A/1      2013-07-12   etxlg     Created as ecoli_DataInit
%%% R2A/2      2013-08-27   etxlg     Renamed as ecoli_datainit +misc
%%% R2A/3      2013-09-01   etxlg     Edit
%%% R2A/4      2013-09-03   etxlg     Start ssh daemon - not
%%% R2A/5      2013-09-04   etxlg     Fix dialyzer warn -> %% not used func
%%% R2A/6      2013-09-04   etxlg     Added init_data/0
%%% R2A/7      2013-09-05   etxlg     All_paths always includes "/"
%%% R2A/8      2013-09-05   etxlg     /coli -> ?INTERNAL_DIR, new internal cmd
%%% R2A/9      2013-09-11   etxlg     Warning if adding already existing cmd
%%% R2A/10     2013-12-19   etxlg     Bugfix for auth before reg
%%% R2A/11     2014-02-24   etxlg     New cmds for MO-shell
%%% R2A/12     2014-04-23   etxlg     Dialyzer warning
%%% R2A/13     2014-07-04 etxberb     * Added COLI commands: cmds, authlevel,
%%%                                     prompt, reportrc, timeout.
%%%                                   * Added DEPRECATED.
%%% R2A/15     2014-09-04 etxberb     Removed deprecated COLI commands.
%%% R2A/16     2014-09-11 etxlg       Fixed to work when board is secure
%%% R4A/    ---------- -------  ------------------------------------------------
%%% R4A/1      2015-07-07 etxberb  Changed mnesia:create_table to
%%%                                clhI:mnesia_create_table.
%%% R4A/16     2015-09-15 uabesvi  error_logger -> sysInitI
%%% ----    ---------- -------  ------------------------------------------------
%%% R5A/1   2016-01-07 etxberb  Changed installation phases in all blocks.
%%% R5A/2   2016-01-11 etxberb  Added old installation phase functions for
%%%                             backwards compatibility reasons (explicit calls
%%%                             from $RDE_TOP/tools/mkcpi/mkcpi.escript)
%%% R5A/3   2016-02-17 uabesvi  Removed args
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-include("ecoli.hrl").

-export([instPhParallel_init/1,
	 instPhParallel_init_data/0]).
-export([init/1]).
-export([children/0, activate/0]).


-export([add_auth/3, add_cmd/1]).
-export([lookup/1, lookup/2, cmds_at_path/1, cmds_at_path/2, all_paths/0,
	all_paths/1, get_all_paths/2]).
-export([get_fru_type/0]).
-export([get_fru_type/1]).
-export([all_cmds/0, all_cmds/1, all_cmds/2]).
-export([global_cmds/0]).
-export([cmds_at_path_internal/0]).

-export([prep_warm/0]).
-export([warm/0]).

-export([coli_test/1]).


-define(COLI_CHANNEL, ecoli_ssh_channel).

coli_test(In) ->
    io:format("~nKilroy was here: ~p  ~n~p~n~n", [node(), In]),
    exit("Nu ar det slut!!!").



%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init(Db_nodes) ->
    {atomic, ok} =
	clhI:mnesia_create_table(coli_cmd,
				 [{attributes, record_info(fields, coli_cmd)},
				  {disc_copies, Db_nodes}]),
    ok.

%% Old installation phase - kept due to explicit call from
%% $RDE_TOP/tools/mkcpi/mkcpi.escript
init(Db_nodes) ->
    instPhParallel_init(Db_nodes).

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init_data() ->
    ok = swmI:register_appdata_receiver("coli_reg", ecoli_register),
    ok = swmI:register_appdata_receiver("coli_auth", ecoli_register).



activate() ->
    Options = [{maxNoFiles, 10}, 
	       {size,       500},
	       {header,     {pmsDebug, get_versions, [ecoli, ?ECOLI_MODS]}},
	       {zip,        true}],
    logRamI:create_log(?ECOLI_LOG, Options),
    appmI:register_warm_cb(?MODULE),  
    ecoli_itc_server:activate(),
    ok.

children() ->
    {ok, [{ecoli_itc_server, {ecoli_itc_server, start_link, []},
           permanent,
           1000,
           worker,
           [ecoli_itc_server]}]}.


%%% ----------------------------------------------------------
%%% @doc 
%%% APPM callback. 
%%% Prepare for warm restart of all applications.
%%% @end
%%% ----------------------------------------------------------
-spec prep_warm() -> ok.
prep_warm() ->
    ok.

%%% ----------------------------------------------------------
%%% @doc 
%%% APPM callback. 
%%% All applications are restarted.
%%% @end
%%% ----------------------------------------------------------
-spec warm() -> ok.
warm() ->
    ok.





warn_msg(Fmt, Data) ->
    Format = "~p: " ++  Fmt ++ "~n",
    sysInitI:warning_msg(Format, [?MODULE | Data]).

add_auth(Cli_name, Cli_path, Authorization) ->
    Tran = fun() ->
		   New_entry = 
		       %% check for existing entry,
		       %% auth may be added before the command
		       case mnesia:read(coli_cmd, {Cli_path, Cli_name}) of
			   [Cmd] ->
			       Cmd#coli_cmd{authorization = Authorization};
			   [] -> #coli_cmd{cli_pname = {Cli_path, Cli_name},
					   authorization = Authorization}
		       end,
		   mnesia:write(New_entry)
	   end,
    {atomic, ok} = mnesia:transaction(Tran).

add_cmd(CmdProplist) ->
    %%io:format("####### add_cmd  ~p~n", [CmdProplist]), 
    CliPath     = proplists:get_value(cli_path, CmdProplist),
    CliName     = proplists:get_value(cli_name, CmdProplist),
    CliPname    = {CliPath, 
		   string:join(string:tokens(CliName, " "), " ")},
    Type        = proplists:get_value(type,        CmdProplist),
    CxpPath     = proplists:get_value(cxp_path,    CmdProplist),
    Relpath     = proplists:get_value(relpath,     CmdProplist),
    Filepath    = proplists:get_value(filepath,    CmdProplist),
    Subcommand  = proplists:get_value(subcommand,  CmdProplist),
    Module      = proplists:get_value(module,      CmdProplist),
    Function    = proplists:get_value(function,    CmdProplist),
    Usage       = proplists:get_value(usage,       CmdProplist),
    Description = proplists:get_value(description, CmdProplist),
    CliType     = proplists:get_value(cli_type,    CmdProplist),
    FT          = proplists:get_value(fru_type,    CmdProplist, ?FRU_LOCAL),
    CS          = proplists:get_value(cli_scope,   CmdProplist, FT),
    CliScope    = get_coli_cmd_scope(CliType, CS),
    Tran = fun() ->
		   New_entry = 
		       case mnesia:read(coli_cmd, CliPname) of
			   %% check for existing entry to keep auth unchanged
			   [#coli_cmd{cxp_path      = OldCxpPath,
				      authorization = Auth} = Old] ->
			       New = #coli_cmd{cli_pname     = CliPname,
					       type          = Type,
					       cxp_path      = CxpPath,
					       relpath       = Relpath,
					       filepath      = Filepath,
					       subcommand    = Subcommand,
					       module        = Module,
					       function      = Function,
					       usage         = Usage,
					       cli_scope     = CliScope,
					       cli_type      = CliType,
					       description   = Description,
					       authorization = Auth},

			       check_if_same(Old, New, CliPname, {OldCxpPath, CxpPath}),
			       New;
			   [] ->
			       #coli_cmd{cli_pname   = CliPname,
					 type        = Type,
					 cxp_path    = CxpPath,
					 relpath     = Relpath,
					 filepath    = Filepath,
					 subcommand  = Subcommand,
					 module      = Module,
					 function    = Function,
					 usage       = Usage,
					 cli_scope   = CliScope,
					 cli_type    = CliType,
					 description = Description}
		       end,
		   mnesia:write(New_entry)
	   end,
    {atomic, ok} = mnesia:transaction(Tran).


check_if_same(Old, New, CliPname, CxpPaths) ->
    Fields = record_info(fields, coli_cmd),
    [_ | OldL] = tuple_to_list(Old),
    [_ | NewL] = tuple_to_list(New),
    Zip = lists:zip3(Fields, NewL, OldL),
    cis([D || {F, N, O} = D<- Zip, F /= cxp_path, N /= O], CliPname, CxpPaths).

cis([], _, _) ->
    ok;
cis(Diff, CliPname, CxpPaths) ->
    maybe_warn(CliPname, CxpPaths, Diff).


get_coli_cmd_scope(undefined, CliScope) ->
    CliScope;
get_coli_cmd_scope(_, _) ->
    fruacc.

maybe_warn(_, {undefined, _}, _) ->
    %% meaning that the command was authorized but not yet registered
    ok;
maybe_warn({CmdPath, CmdName}, {PrevCxp, ThisCxp}, Diff) ->
    warn_msg("Overwriting existing COLI command~n"
	     "command name:        ~p~n"
	     "command path:        ~p~n"
	     "Previous owning CXP: ~p~n"
	     "New owning CXP:      ~p~n"
	     "Diffing attributes:  ~p~n",
	     [CmdName, CmdPath, PrevCxp, ThisCxp, Diff]).

%returns a list of tuples {Cli_path, Cli_name}
%internal commands with Cli_path=?INTERNAL_DIR (i.e. "/misc") are excluded
all_cmds() ->
    all_cmds(0).
all_cmds(Auth_level) ->
    Match_all = mnesia:table_info(coli_cmd, wild_pattern),
    Match = Match_all#coli_cmd{cli_pname = {'$1', '$2'},
			       type = '$4',
			       authorization = '$3'},
    Cond  = [{'=<', Auth_level, '$3'}, {'=/=', undefined, '$4'}],
    mnesia:dirty_select(coli_cmd, [{Match, Cond, [{{'$1', '$2'}}]}]).

all_cmds(AuthLevel, CliScope) 
  when CliScope == ?FRU_LOCAL orelse 
       CliScope == ?FRU_CENTRAL ->
    MatchAll = mnesia:table_info(coli_cmd, wild_pattern),
    Match = MatchAll#coli_cmd{cli_pname     = {'$1', '$2'},
			      cli_type      = '$6',
			      cli_scope     = '$5',
			      type          = '$4',
			      authorization = '$3'},
    Cond  = [{'=<',  AuthLevel, '$3'}, 
	     {'=/=', undefined, '$4'},
	     {'orelse', {'==', CliScope, '$5'}, {'==', ?FRU_LOCAL, '$5'}},
	     {'==', undefined, '$6'}],
    mnesia:dirty_select(coli_cmd, [{Match, Cond, [{{'$1', '$2'}}]}]);
all_cmds(AuthLevel, CliScope) ->
    MatchAll = mnesia:table_info(coli_cmd, wild_pattern),
    Match = MatchAll#coli_cmd{cli_pname     = {'$1', '$2'},
			      cli_type      = '$6',
			      cli_scope      = '$5',
			      type          = '$4',
			      authorization = '$3'},
    
    Cond  = [{'=<',  AuthLevel, '$3'}, 
	     {'==',  CliScope,  '$4'},
	     {'==',  CliScope,  '$5'},
	     {'=/=', undefined, '$6'}],
    
    mnesia:dirty_select(coli_cmd, [{Match, Cond, [{{'$1', '$2'}}]}]).



lookup({_, _} = Cli_pname) ->
    lookup(Cli_pname, 0).
lookup({?INTERNAL_DIR, Cli_name}, _Auth_level) ->
    lookup_internal(Cli_name);
%% record_to_proplist(lookup_internal(Cli_name));
lookup(Cli_pname, Auth_level) ->
    Match_all = mnesia:table_info(coli_cmd, wild_pattern),
    Match = Match_all#coli_cmd{cli_pname = Cli_pname,
			       authorization = '$3',
			       type = '$4'},
    Cond  = [{'=<', Auth_level, '$3'}, {'=/=', undefined, '$4'}],
    %%there must be  no more than one match
    case mnesia:dirty_select(coli_cmd, [{Match, Cond, ['$_']}]) of
	[Result] ->
	    Result;
	%% record_to_proplist(Result);
	_ ->
	    []
    end.

% returns a list of tuples {Path, Cmd}
cmds_at_path(Path) ->
    cmds_at_path(Path, 0).
cmds_at_path(?INTERNAL_DIR, _Auth_level) ->
    cmds_at_path_internal();
cmds_at_path(Path, Auth_level) ->
    Match_all = mnesia:table_info(coli_cmd, wild_pattern),
    Match = Match_all#coli_cmd{cli_pname = {Path, '$1'},
				type = '$4',
				authorization = '$3'},
    Cond  = [{'=<', Auth_level, '$3'}, {'=/=', undefined, '$4'}],
    mnesia:dirty_select(coli_cmd, [{Match, Cond, [{{Path, '$1'}}]}]).

% optimize this or cache in State
% this is now read once for each session start and then cached in state
% returns a list of ALL unique cmd paths
all_paths() ->
    all_paths(0).
all_paths(Auth_level) ->
    %unfortunately this will give us only those paths that have any
    %commands in them, not those that just contain other directories
    All_paths_duplicate = [Path || {Path, _} <- all_cmds(Auth_level)],
    All_paths_with_commands = lists:usort(All_paths_duplicate),
    All_paths_expanded = path_exploder(All_paths_with_commands, [], []),
    lists:usort(["/", ?INTERNAL_DIR | All_paths_expanded]).

get_all_paths(AuthLevel, FruId) ->
    p("$$$$ DATAINIT   ~p~n", [{AuthLevel, FruId}]),
    CliScope = get_fru_type(FruId),
    p("$$$$ DATAINIT  Fru_type ~p~n", [CliScope]),
    AllPathsDuplicate = [Path || {Path, _} <- all_cmds(AuthLevel, CliScope)],
    p("$$$$ DATAINIT  All_paths_duplicate ~p~n", [AllPathsDuplicate]),
    AllPathsWithCommands = lists:usort(AllPathsDuplicate),
    p("$$$$ DATAINIT  All_paths_with_commands ~p~n", [AllPathsWithCommands]),
    AllPathsExpanded = path_exploder(AllPathsWithCommands, [], []),
    p("$$$$ DATAINIT  All_paths_expanded ~p~n", [AllPathsExpanded]),
    X = lists:usort(["/", ?INTERNAL_DIR | AllPathsExpanded]),
    p("$$$$ DATAINIT res  ~p~n", [X]),
    X.
    
p(_S,_A) ->
    ok.
%% p(S,A) ->
%%     io:format(S,A).

get_fru_id([]) ->
    undefined;
get_fru_id(FruId) ->
    [_, Node] = string:tokens(atom_to_list(clhI:erlang_node()), "@"),
    list_to_atom(FruId ++ "@" ++ Node).
    


get_fru_type() ->
    gft(clhI:mp_id(clhI:erlang_node())).
   
get_fru_type(Fru_id) ->
    gft(clhI:mp_id(get_fru_id(Fru_id))).

gft(undefined) ->
    fruacc;
gft(Mp_id) ->
    case {clhI:mp_role(Mp_id), clhI:core_state(Mp_id)} of
	{core, active} -> central;
	_              -> local
    end.   
		   
		   
    



path_exploder([], [], Many) ->
    Many;
path_exploder([P | Paths], [], Many) ->
    path_exploder(Paths, P, Many);
path_exploder(Paths, "/", Many) ->
    path_exploder(Paths, [], ["/" | Many]);
path_exploder(Paths, P, Many) ->
    path_exploder(Paths, ecoli_lib:dirname(P) , [P | Many]).

% currently not used function
%record_to_proplist(#coli_cmd{} = Cmd) ->
    %[{Name, Value} || {Name, Value} <-
	%[{cli_pname, Cmd#coli_cmd.cli_pname},
	%{type, Cmd#coli_cmd.type}, {relpath, Cmd#coli_cmd.relpath},
	%{filepath, Cmd#coli_cmd.filepath},
	%{subcommand, Cmd#coli_cmd.subcommand},
	%{module, Cmd#coli_cmd.module}, {function, Cmd#coli_cmd.function},
	%{args, Cmd#coli_cmd.args},
	%{usage, Cmd#coli_cmd.usage}, {description, Cmd#coli_cmd.description},
	%{authorization, Cmd#coli_cmd.authorization}],
	%Value =/= undefined];
%record_to_proplist([]) -> [].

% The stuff below is mainly so that internal commands can be available in
% help and for command expansion
% commands of type coli_erts are called using the (almost) normal interface
% for commands of type erts (it allows use of an internal protocoll)
% commands of type coli_internal are truly internal and also available
% in the cmd-tree, i.e. they require no path

% return a record of type coli_cmd
lookup_internal(Cli_name) ->
    lookup_internal(Cli_name, internal_cmds()).
lookup_internal(_, []) ->
    [];
lookup_internal(Cli_name,
	[Cmd_elem = #coli_cmd{cli_pname = {_, Cli_name}} | _]) ->
    Cmd_elem;
lookup_internal(Cli_name, [_ | T]) ->
    lookup_internal(Cli_name, T).

%return a list of commands in ?INTERNAL_DIR -> [{?INTERNAL_DIR, Name1},
% {?INTERNAL_DIR, Name2}...]
cmds_at_path_internal() ->
    [{?INTERNAL_DIR, Cli_name} || #coli_cmd{cli_pname = {_, Cli_name}} <-
	internal_cmds()].

%get only those commands that are "global", i.e. available without
% path
% return as a list: [{?INTERNAL_DIR, "exit"}, {?INTERNAL_DIR, "pwd"}, ...]
global_cmds() ->
    [Pname || #coli_cmd{cli_pname = Pname, type = Type} <- internal_cmds(),
	Type =:= coli_internal].

%return all the internal (i.e. at ?INTERNAL_DIR, as a list of #coli_cmd{}
%these are not put into the database table
internal_cmds() ->
    internal_cmds(ecoli_lib:is_lab()).
internal_cmds(false) ->
    int_cmds();
internal_cmds(true) ->
    int_cmds() ++
    [#coli_cmd{cli_pname = {?INTERNAL_DIR, "authlevel"},
	       type = coli_erts,
	       module = ecoli_internal_cmd,
	       function = set_auth_level,
	       usage = "arg: <new_auth>, change the authorization level",
	       description = <<
"\nChange the authorization level of the current shell.\n"
"Note that this command in itself is restricted in such a way that it is only\n"
"available when the system is running in an unsecure mode (lab or sim).\n\n"
"Argument:\n"
"AuthLevelValue = BasebandSupportBasic | BasebandSupportAdvanced | BasebandSupportExpert | disabled\n"
"      Set authorization level to a new value.\n"
"      The value \"disabled\" will enable use of all currently available\n"
"      commands including those registered without any authorization level.\n"
>>}].

int_cmds() ->
[

#coli_cmd{cli_pname = {?INTERNAL_DIR, "exit"},
			type = coli_internal,
			usage = "No options.",
			description = <<
"\nExit from the coli-shell.\n"
"This command is valid anywhere in the cmd-tree, i.e. it requires no \"path\"."
>>},

#coli_cmd{cli_pname = {?INTERNAL_DIR, "cd"},
			type = coli_internal,
			usage = "change cmd-directory "
				"within the cmd-tree",
			description = <<
"\nChange directory.\n"
"This command is valid anywhere in the cmd-tree, i.e. it requires no \"path\".\n"
"With no argument the current dir is printed."
>>},

#coli_cmd{cli_pname = {?INTERNAL_DIR, ".."},
			type = coli_internal,
			usage = "synonymous to \"cd ..\"",
			description = <<
"\nChange directory one level towards the root of the command tree."
>>},

#coli_cmd{cli_pname = {?INTERNAL_DIR, "ls"},
			type = coli_internal,
			usage = "args: [Path...], list commands and "
				"cmd-directories",
			description = <<
"\nAll commands and directories at a certain level are listed.\n\n"
"Arguments:\n"
"[Path...]\n"
"      List all commands and directories at the specified Path(s).\n"
"      Directories are expanded to show all subdirectories.\n\n"
"      If no Path specified for this command, the current level is assumed."
>>},

#coli_cmd{cli_pname = {?INTERNAL_DIR, "pwd"},
			type = coli_internal,
			usage = "print current cmd-directory level",
			description = <<
"\nPrint the current level in the command tree. I.e. the current working\n"
"directory."
>>},

#coli_cmd{cli_pname = {?INTERNAL_DIR, "help"},
			type = coli_internal,
			usage = "arg: [cmd], print help about cmd",
			description = <<
"\nHelp about the shell itself (\"help shell\") or the command given as arg.\n"
"Help without argument will print the usage string for all commands found at\n"
"the current cmd-tree level. If there are no commands it prints shell-help.\n"
"This command is valid anywhere in the cmd-tree, i.e. it requires no \"path\"."
>>},

#coli_cmd{cli_pname = {?INTERNAL_DIR, "reportrc"},
			type = coli_erts,
			module = ecoli_internal_cmd,
			function = set_report_rc,
			usage = "Option",
			description = <<
"\nSet if RC is to be printed or not.\n"
"Manipulates the internals of the coli shell itself.\n"
"Return Code (RC) is whatever is returned from a command and is not limited to"
"an integer.\n\n"
"Options:\n"
"false\n"
"      Disable printing.\n\n"
"true\n"
"      Enable printing.\n"
>>},

#coli_cmd{cli_pname = {?INTERNAL_DIR, "grep"},
			type = erts,
			module = ecoli_internal_cmd,
			function = grep,
			usage = "arg: <limited regex>, filter on pattern",
			description = <<
"\nUse in a pipe to filter out lines of interest.\n"
"The regex is VERY limited - this is not a real grep implementation.\n"
"(For now; DO NOT USE anything but a literal string)."
>>},

#coli_cmd{cli_pname = {?INTERNAL_DIR, "tail"},
			type = erts,
			module = ecoli_internal_cmd,
			function = tail,
			usage = "arg: [-n N], N number of tailing lines will be printed ",
			description = <<
"\nUse in a pipe to print only the tailing N number of lines.\n"
"If -n is not defined the 10 tailing lines will be printed\n"
"This command is similar to unix tail (but only the -n option is implemented).\n"
>>},

#coli_cmd{cli_pname = {?INTERNAL_DIR, "timeout"},
			type = coli_erts,
			module = ecoli_internal_cmd,
			function = set_timeout,
			usage = "[Arg]",
			description = <<
"\nShell inactivity timeout.\n"
"Manipulates the internals of the coli shell itself.\n"
"The shell will be terminated and the user logged out if no\n"
"activity is detected for a period exceeding the set timeout.\n\n"
"Arguments:\n"
"TimeoutValue\n"
"      Set shell inactivity timeout.\n\n"
"TimeoutValue = integer(Seconds) | \"infinity\" (to disable)\n\n"
"If no argument specified for this command, \"infinity\" is assumed.\n"
>>},

#coli_cmd{cli_pname = {?INTERNAL_DIR, "info"},
			type = coli_erts,
			module = ecoli_internal_cmd,
			function = info,
			usage = "No options.",
			description = <<
"\nReport some statistics on the current session."
>>},

#coli_cmd{cli_pname = {?INTERNAL_DIR, "cmds"},
			type = coli_erts,
			module = ecoli_internal_cmd,
			function = list_cmds,
			usage = "No options.",
			description = <<
"\nList all currently authorized commands."
>>},

#coli_cmd{cli_pname = {?INTERNAL_DIR, "prompt"},
			type = coli_erts,
			module = ecoli_internal_cmd,
			function = coli_prompt,
			usage = "Option [Arg]",
			description = <<
"\nSet prompt for the current session.\n\n"
"Options:\n"
"-d\n"
"      Set the prompt back to default.\n"
"      (Same as setting the prompt to: \"coli\\s[\\h: \\w]->\\s\")\n\n"
"-s [NewPrompt]\n"
"      Set a new prompt for the session.\n"
"      To embed a newline use the character combination \"\\n\",\n"
"      \"\\s\" will embed \" \" (space)\n"
"      \"\\w\" will insert the current working directory, and\n"
"      \"\\h\" will insert the FRU name where the command will be executed.\n\n"
"      Example:\n"
"         prompt -s \\w\\s>\\s\n"
"         creates the following prompt (if standing at root): '/ > '\n\n"
"      If no argument specified for this option, default prompt is set.\n"
>>}
].



