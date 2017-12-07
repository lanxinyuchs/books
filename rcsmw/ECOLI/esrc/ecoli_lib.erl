%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ecoli_lib.erl %
%%% Author:     etxbjca
%%% Description:
%		Auth level mapping
%
%	From xml	In database	Coli user
%	not set		0	        0: Board without Vendor Credentials
%	"expert"	1		1
%	"advanced"	2		2
%	"basic"		3		3
%	"none"				4: no valid role
% no valid role means that the user will see only /misc/*
% Command authorization algorithm is:
% Return commands where Coli_user_level =< Database_level
% For printing and external authorization (LDAP) the following "official"
% role names are used:
%	"BasebandSupportExpert"		=:=	"expert"
%	"BasebandSupportAdvanced"	=:=	"advanced"
%	"BasebandSupportBasic"		=:=	"basic"
%
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(ecoli_lib).
%-behaviour(behaviour).
-id('Updated by CCase').
-vsn('/main/R2A/R4A/R5A/R7A/R10A/1').
-date('2017-05-16').
-author('etxjotj').
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
%%% R2A/1      2013-07-12   etxlg     Created
%%% R2A/2      2013-08-27   etxlg     Some more functions + fixing
%%% R2A/3      2013-09-03   etxlg     Cleaned away unused
%%% R2A/4      2013-09-05   etxlg     added is_lab(), misc. stuff
%%% R2A/5      2014-04-15   etxlg     is_lab() -> true + info print to erl-log
%%% R2A/6      2014-08-18   etxlg     is_lab(), now checks for VC
%%% R2A/7      2014-08-19   etxlg     sec_log()
%%% R4A/6      2015-07-14   etxlg     New rolenames, login without authorization
%%% R4A/8      2015-08-20   etxpejn   Added rpc:call for SecurityLog
%%% R4A/11     2015-09-15   uabesvi   error_logger -> sysInitI
%%% R4A/12     2015-09-25   etxpejn   Moved rpc:call to logI:write_log
%%% R10A/1     2017-05-16   etxjotj   Handle lab case for cloud
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([auth_to_integer/1,
	int_to_auth_string/1]).
%-export([get_available_auth_strings/0]). %to be retired
-export([get_official_auth_strings/0, get_legacy_auth_strings/0]).
-export([roles_to_auth/1]).
-export([lookup/3]).
-export([get_all_paths/3]).
-export([build_path/2]).
-export([matching_paths/2]).
-export([basename/1, dirname/1, join/2, soft_hd/1]).

-export([is_lab/0, sec_log/2]).

-export([get_all_fruaccs/0]).
-export([get_cli_type/3]).
-export([get_fru_prefix/1]).
-export([get_fru_type/1]).

-export([update_me_id/1]).
-export([update_me_id/2]).

-export([pp/1]).


-include("ecoli.hrl").

-define(HIGHEST_AUTH, 1).
-define(NO_AUTH, 4).
-define(SUPER_ROLE, "EricssonSupport"). %from omc.hrl
% returned in order with highest authorization level first
% this reflects an intermediate situation - we support both legacy and official,
% a warning is printed when legacy is used to authorize login.
%get_compat_auth_strings() ->
%   get_official_auth_strings() ++ get_legacy_auth_strings().
%get_available_auth_strings() -> % to be removed
%    get_legacy_auth_strings().
get_official_auth_strings() ->
    ["BasebandSupportExpert", "BasebandSupportAdvanced",
     "BasebandSupportBasic"].
get_legacy_auth_strings() ->
    ["expert", "advanced", "basic"].

auth_to_integer("expert") -> 1;
auth_to_integer("advanced") -> 2;
auth_to_integer("basic") -> 3;
auth_to_integer("BasebandSupportExpert") -> 1;
auth_to_integer("BasebandSupportAdvanced") -> 2;
auth_to_integer("BasebandSupportBasic") -> 3;
auth_to_integer("disabled") -> 0;
auth_to_integer(_) -> ?NO_AUTH.

int_to_auth_string(1) -> "BasebandSupportExpert";
int_to_auth_string(2) -> "BasebandSupportAdvanced";
int_to_auth_string(3) -> "BasebandSupportBasic";
int_to_auth_string(4) -> "none";
int_to_auth_string(0) -> "disabled".

%accept a list of all roles, e.g ["flipper", "alfa", "basic", "expert"...]
%return the Highest coli authorization found
%if executing in a lab environment return a level higher than highest
roles_to_auth([?SUPER_ROLE]) ->
    % This means the user is a "Maintenance user", fudge the roles to work with
    % RCS-COLI.
    % adding the role corresponding to the highest authority level in RCS-COLI
    Authlevel = ?HIGHEST_AUTH,
    {Authlevel, [int_to_auth_string(Authlevel), ?SUPER_ROLE]};
roles_to_auth(Roles) ->
    case roles_to_auth(Roles, Roles, get_official_auth_strings()) of
	?NO_AUTH -> %temporary backwards compatibility measure
	    case roles_to_auth(Roles, Roles, get_legacy_auth_strings()) of
		?NO_AUTH ->
		    {?NO_AUTH, Roles};
		Auth_int ->
		    sysInitI:warning_msg(
			"~w: RCS-COLI: authorization succeeded using legacy "
			"roles.~nThis is a temporary measure to maintain "
			"compatibility.~n"
			"Please update all RCS-COLI users with new roles:~n"
			"BasebandSupportBasic, BasebandSupportAdvanced, "
			"BasebandSupportExpert~n", [?MODULE]),
		    {Auth_int, Roles}
	    end;
	Auth_int -> % official and good
	    {Auth_int, Roles}
    end.

roles_to_auth(_, _, []) ->
    ?NO_AUTH;
roles_to_auth([], Roles, [_ | T]) ->
    roles_to_auth(Roles, Roles, T);
roles_to_auth([H | _], _, [H | _]) ->
    auth_to_integer(H);
roles_to_auth([_ | T], Roles, Coli_roles) ->
    roles_to_auth(T, Roles, Coli_roles).


%take the current Level is the working directory in the cmd-tree
%the Cmd_arg is a command within the cmd-tree
%the Cmd_arg may be absolute or relative to Level
%return a #coli_cmd{}
%if not found (or not seen at current auth -> []
lookup(_, [$/ | _]  = Cmd_arg, Auth) ->  %absolute path in arg
    ecoli_datainit:lookup({dirname(Cmd_arg), basename(Cmd_arg)}, Auth);
lookup(Level, Cmd_arg, Auth) ->
    Pname = join(Level, Cmd_arg),
    ecoli_datainit:lookup({dirname(Pname), basename(Pname)}, Auth).





get_cli_type(_Frus, _FruAccs, []) ->
    ["du"];
get_cli_type(Frus, FruAccs, FruId) ->
    gct([Fru    || {Fru,    Id} <- Frus,    Id == FruId],
	[FruAcc || {FruAcc, Id} <- FruAccs, Id == FruId],
	FruId).

gct([], [], FruId) ->
    {error, "No CliType found for " ++ FruId};
gct(Frus, _, _) when length(Frus) > 0->
    Frus;
gct(_, FruAccs, _) ->
    FruAccs.




get_all_paths(AuthLevel, FruId, CliTypes) ->
    gap(CliTypes, AuthLevel, FruId, []).

gap([], _AuthLevel, _FruId, Acc) ->
    lists:usort(Acc);
gap([CliType| T], AuthLevel, FruId, Acc) ->
    p("~n====~n$$$$ LIB   ~p~n", [{AuthLevel, FruId, CliType}]),
    p("$$$$ LIB  COLI_CMD~n~n ~p~n~n", [ets:tab2list(coli_cmd)]),
    FruType = get_fru_type(FruId),
%%    [CliType | _] = string:tokens(FruId, "="),
    p("$$$$ LIB  FruType ~p~n", [FruType]),
    AllCmds = all_cmds(AuthLevel, FruType, list_to_atom(string:to_lower(CliType))),
    p("$$$$ LIB  AllCmds ~p~n", [AllCmds]),
    AllPathsDuplicate = [Path || {Path, _} <- AllCmds],
    p("$$$$ LIB  AllPathsDuplicate ~p~n", [AllPathsDuplicate]),
    AllPathsWithCommands = lists:usort(AllPathsDuplicate),
    p("$$$$ LIB  AllPathsWithCommands ~p~n", [AllPathsWithCommands]),
    AllPathsExpanded = path_exploder(AllPathsWithCommands, [], []),
    p("$$$$ LIB  AllPathsExpanded ~p~n", [AllPathsExpanded]),
    X = lists:usort(["/", ?INTERNAL_DIR | AllPathsExpanded]),
    p("$$$$ LIB res  ~p~n", [X]),
    p("====~p~n",[""]),
    gap(T, AuthLevel, FruId, lists:append(X, Acc)).

p(_S,_A) ->
    ok.
%% p(S,A) ->
%%     io:format(S,A).



get_fru_type([]) ->
    case clhI:fru_id() of
	""  -> central;
	Fid -> get_fru_type(Fid)
    end;
get_fru_type(FruId) ->
    gft(clhI:mp_id(FruId)).
%%    gft(clhI:mp_id(get_fru_id(FruId))).

gft(undefined) ->
    fruacc;
gft(MpId) ->
    case {clhI:mp_role(MpId), clhI:core_state(MpId)} of
	{core, active} -> central;
	_              -> local
    end.   
		   


%% get_fru_id([]) ->
%%     undefined;
%% get_fru_id(FruId) ->
%% %%     FruId = case string:tokens(Id, "=") of
%% %% 		[X]     -> X;
%% %% 		[_ | X] -> string:join(X, "=")
%% %% 	    end,
%%     [_, Node] = string:tokens(atom_to_list(clhI:erlang_node()), "@"),
%%     list_to_atom(FruId ++ "@" ++ Node).
    

%returns a list of tuples {Cli_path, Cli_name}
%internal commands with Cli_path=?INTERNAL_DIR (i.e. "/misc") are excluded

all_cmds(AuthLevel, CliScope, _) 
  when CliScope == ?FRU_LOCAL orelse 
       CliScope == ?FRU_CENTRAL ->
    p("$$$$ LIB  ALL_CMDS 1 ~p  ~p~n", [AuthLevel, CliScope]),
    MatchAll = mnesia:table_info(coli_cmd, wild_pattern),
    Match = MatchAll#coli_cmd{cli_pname     = {'$1', '$2'},
			      cli_type      = '$6',
			      cli_scope     = '$5',
			      type          = '$4',
			      authorization = '$3'},
    Cond  = [{'=<',  AuthLevel, '$3'}, 
	     {'=/=', undefined,  '$4'},
	     {'orelse', {'==', CliScope, '$5'}, {'==', ?FRU_LOCAL, '$5'}},
	     {'==', undefined,  '$6'}],
    mnesia:dirty_select(coli_cmd, [{Match, Cond, [{{'$1', '$2'}}]}]);
all_cmds(AuthLevel, CliScope, CliType) ->
    p("$$$$ LIB  ALL_CMDS 2 ~p  ~p    ~p~n", [AuthLevel, CliScope, CliType]),
    MatchAll = mnesia:table_info(coli_cmd, wild_pattern),
    Match = MatchAll#coli_cmd{cli_pname     = {'$1', '$2'},
			      cli_type      = '$6',
			      cli_scope     = '$5',
			      type          = '$4',
			      authorization = '$3'},

    Cond  = [{'=<', AuthLevel, '$3'}, 
	     {'==', CliScope,  '$4'},
	     {'==', CliScope,  '$5'},
	     {'==', CliType,   '$6'}],

    mnesia:dirty_select(coli_cmd, [{Match, Cond, [{{'$1', '$2'}}]}]).



path_exploder([], [], Many) ->
    Many;
path_exploder([P | Paths], [], Many) ->
    path_exploder(Paths, P, Many);
path_exploder(Paths, "/", Many) ->
    path_exploder(Paths, [], ["/" | Many]);
path_exploder(Paths, P, Many) ->
    path_exploder(Paths, ecoli_lib:dirname(P) , [P | Many]).




%arg1: Level, the "Working directory"
%arg2: Arg, to be combined with level to make a path
%return: a (partly) normalized path representation of the above
%i.e: begins with "/"
%if the string in Arg begins with "/" the path is "absolute"
%duplicate "/" in Arg is NOT handled - done elsewhere no need
build_path(Level, []) -> %path == Level
    Level;
build_path(_, [$/ | _] = Path) -> %absolute path given
    Path;
build_path("/", Arg) -> %relatative to Level == "/"
    [$/ | Arg];
build_path(Level, Arg) -> % relative to Level = "/path..."
    Level ++ [$/ | Arg].

%arg1: Prefix is an already "cleaned up" path
%arg2: Pathlist is a list of paths to match
%return: a list of all paths whose prefix matches Prefix, but do NOT return
%       a path  that is equal to the prefix
matching_paths(_, []) -> [];
matching_paths(Prefix, [H | T]) ->
    case lists:prefix(Prefix, H) of
        true ->
            if
                H =:= Prefix -> %discard the exact match
                    matching_paths(Prefix, T);
                true ->
                    [H | matching_paths(Prefix, T)]
            end;
        false ->
            matching_paths(Prefix, T)
   end.

%works more or less as the usual basename and basename
%but hardcoded to use "/"
%also special treatment for my particular use
basename(Cmd) ->
    basename(lists:reverse(Cmd), Cmd, []).
basename([], Cmd, _) -> Cmd;
basename([$/ | _], _, Acc) ->
    Acc;
basename([C | Rest], Cmd, Acc) ->
    basename(Rest, Cmd, [C | Acc]).

dirname([$/ | _] = Cmd) -> %allow only absolute paths
    dirn(lists:reverse(Cmd)).
dirn([_]) -> "/"; %ensure that a root is returned as a minimum
dirn([$/ | Dir]) ->
    lists:reverse(Dir);
dirn([_ | Rest]) ->
    dirn(Rest).

%mostly like normal filename:join/2
%additional "/" at the end of Dir or Name is not handled
join("/", [$/ | _] = Name) ->
    Name;
join("/", Name) ->
    [$/ | Name];
join([$/ | _] = Dir, [$/ | _] = Name) ->
    Dir ++ Name;
join([$/ | _] = Dir, Name) ->
    Dir ++ [$/ | Name].

%like hd() but returns [] if called with []
%like hd() but returns [] if called with []
soft_hd([]) -> [];
soft_hd([H | _]) -> H.

%%funny that a func named is_lab() will return true when running in simulation
%% this returns true if no VC is found on the board (or if sim)
is_lab() ->
    is_lab(sysEnv:rcs_mode_2()).
is_lab(Mode) when Mode==target; Mode==vrcs ->
    try sysInitServer:is_secure() of
	true -> false;
	false -> true
    catch
	error:undef -> true;  %this is workaround  pending the release of SYS
	_:_ -> false
    end;
is_lab(simulated) ->
    true.

sec_log(SrcIp, Msg) ->
    logI:write_log("SecurityLog", SrcIp,  get_me_id_string(), 4, 
		   info, os:timestamp(), Msg).

get_me_id_string() ->
    case proplists:get_value(networkManagedElementId,
                             comsaI:get_managed_element_data(),
                             undefined) of
        undefined ->
            "ManagedElement=1";
        Me_string when is_list(Me_string) ->
            "ManagedElement=" ++ Me_string
    end.



get_all_fruaccs() ->
    lists:sort(ets:foldl(fun gaf/2, [], ecoli_fruacc)).

gaf(#fruacc{key  = {FruType, _},
	    ldns = Ldns},
    Acc) ->
    [{FruType, update_me_id(LDN, get_me_id())} || LDN <- Ldns] ++ Acc.



get_fru_prefix(Fru) ->
    gfp(string:to_lower(Fru)).


gfp([F | Ru]) when F >= 97 andalso F =< 122 ->
    [F - 32 | Ru] ++ "=";
gfp(Fru) ->
    Fru ++ "=".


get_me_id() ->
    MEData = comsaI:get_managed_element_data(),
    case proplists:get_value(networkManagedElementId, MEData) of
	undefined -> "1";
	Id        -> Id
    end.


%%========================================================================
%% update_me_id(LDN, NewMeId) -> LDN.
%% 
%% Change the MeId to NewMeId. 
%% "1" is used internally to find the correct FRU because the current
%% networkManagedElementId can be updated at any time by the operator.
%%========================================================================
update_me_id(LDN) ->
    update_me_id(LDN, get_me_id()).
    


update_me_id(LDN, NewMeId) ->
    umi(lists:prefix("ManagedElement=", LDN), LDN, NewMeId).


umi(true, LDN, NewMeId) ->
    {ME, T} = lists:splitwith(fun(X) -> [X] /= "," end, LDN),
    [MeStr | _] = string:tokens(ME, "="),
    MeStr ++ "=" ++ NewMeId ++ T;
umi(false, LDN, _) ->
    LDN.


%%========================================================================
%% pp(Rec) -> ok.
%% 
%%========================================================================
pp(Rec) when is_record(Rec, cst) ->
    [_ | Vals] = tuple_to_list(Rec),
    F = record_info(fields, cst),
    L = max_length(F, 0),
    lists:flatten([io_lib:format("    ~-*s = ~p~n", [L, K, V]) || 
	{K, V} <- lists:zip(F, Vals)]).


max_length([], L) ->
    L;
max_length([H|T], L) ->
    case length(atom_to_list(H)) of
	GR when GR > L -> max_length(T, GR);
	_              -> max_length(T, L)
    end.

