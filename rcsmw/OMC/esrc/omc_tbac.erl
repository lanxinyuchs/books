%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_tbac.erl %
%%% Author:	etxbjca
%%% Description: eldap TBAC
%%% TBAC - Target Based Authorization
%%% TBAC - Target Based Authentication ???
%%% NOTE: authentication and authorization are not done in completely separate
%%%   steps, some things in authent are deferred to authorization
%%%   (e.g. case sensitive matching of ericssonUserAuthenticationScope).
%%% This hopefully does the following:
%%%    Unless the the profileFilter is set to 'ericsson' this makes little
%%%    sense, however it may be run even with profileFilter NOT set to 
%%%    'ericsson' since running this depends on the configuration:
%%%    EricssonFilter=1,targetBasedAccessControl=LOCKED | UNLOCKED
%%% Authentication:
%%%    If targetBasedAccessControl=UNLOCKED all the 
%%%    ericssonUserAuthenticationScope attribute-values for the <User>
%%%    is checked for a match against the list in UserManagement=1,targetType
%%%    If a match is found the user is authenticated
%%%    Additionally a search is done for the attribute-value: "*", if this
%%%    is found the user is authenticated, i.e. content of targetType does
%%%    not matter (however, it will again matter in the authorization step).
%%%    If targetBasedAccessControl=LOCKED nothing happens here (i.e. the user
%%%    is authenticated as long as the usual means succeed (bind/password)).
%%%
%%% Authorization:
%%%    in the ldap object (extended posixAccount) each user may have
%%%    multiple attributes: ericssonUserAuthorizationScope
%%%    these contain entries of the form:
%%%       "mtas:sysadmin"
%%%       "*:readalot"
%%%       "readonly"
%%%    or "mtas.ims.kista:sysadmin"
%%%    which break down into <Node Type>:<Role> i.e. separator is ":".
%%%    If targetBasedAccessControl=UNLOCKED the <Node Type> is matched
%%%    against the the list found in UserManagement=1,targetType.
%%%    NO LONGER TRUE see: artf337312 %%%Entries without <Node Type> and ...
%%%    Entries with <Node Type> = "*" will always match, example:
%%%    targetType=[alfa beta.trams]
%%%    ericssonUserAuthorizationScope "alfa:role1"	  <-MATCH
%%%    ericssonUserAuthorizationScope "alfa.vips:role2"	  <-NO MATCH
%%%    ericssonUserAuthorizationScope "beta.nopsky:role3" <-NO MATCH
%%%    ericssonUserAuthorizationScope "beta.trams:role4"  <-MATCH
%%%    ericssonUserAuthorizationScope "role5"		  <-NO MATCH
%%%    ericssonUserAuthorizationScope "zeta:role6"	  <-NO MATCH
%%%    ericssonUserAuthorizationScope "*:role7"		  <-MATCH
%%%    The user receives the roles: role1, role4, and role7
%%%    (alias resolution, if used, may change/add to  this)
%%%    ADDITION due to artf337274. ericssonUserAuthenticationScope is also 
%%%    taken into account when performing authorization.
%%%    To assign a role the <Node Type> must be present in both
%%%     ericssonUserAuthorizationScope and ericssonUserAuthenticationScope.
%%%    If targetBasedAccessControl=LOCKED the <Node Type> is NOT matched
%%%    i.e. only user roles without <Node Type>, and roles with the <Node Type>
%%%    set to an asterisk, will be assigned.
%%%    From above example the user would get the roles: role5, role7
%%%
%%% Roles grouping and aliasing
%%%    This step is performed if the configuration item
%%%    EricssonFilter=1,roleAliasesBaseDn is set.
%%%    The objectclass ericssonRoleAlias, is used for this
%%%    This objectclass also contains the attribute:
%%%    ericssonUserAuthorizationScope
%%%    This works exactly like in the case for Authorization above except
%%%    for the lookup (search). 
%%%    For authorization (above) the search is done with key = uid
%%%    (or rather memberUid = <USER> in posixGroup objects)
%%%    For grouping/aliasing the search is done with key = role..., where
%%%    role is set to the list of stuff that came out from authorization
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(omc_tbac).
-id('Updated by CCase').
-vsn('/main/R2A/R4A/1').
-date('2015-09-11').
-author('etxasta').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
%%% Rx         2014-03-05   etxlg     Broken out from omc_eldap
%%% R2A/2      2014-03-21   etxlg     artifacts and trace
%%% R2A/3      2014-03-24   etxlg     changed tracing
%%% R2A/4      2014-09-08   etxlg     make roles case sensitive
%%%				      try to conform to PB4 of
%%%				      LDAP-howto
%%%				      main fix for TR HS92410
%%% ----------------------------------------------------------
%%% 
%%%-compile([export_all]).
-include_lib("eldap/include/eldap.hrl").
-include("omc.hrl").
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%-export([authenticate/3]).
-export([authorize/5]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% ----------------------------------------------------------
%%%  authorize(P, Use_tbac, Target_list, Authent_list, Role_list)
%%% Input: #ldap_params, true|false, ["target"...], ["role"...], ["role"...]
%%% Output: ["authorized_roles"...] 
%%% Exceptions: since the search/match on ericssonUserAuthenticationScope
%%%		was done in default caseINsensitive, it is possible that
%%%		we are here even though we shouldn't be, but since authorize
%%%		and authenticate isn't really two separate steps it should
%%%		be ok that missing authentication due to case-matching are
%%%		detected here. The result will be an empty authorization list,
%%%		which in turn will cause a failure in AA.
%%% Description: see long description at the top of the file
%%% ----------------------------------------------------------
authorize(P, true, Target_list,  Authent_list, Role_list) ->
    dbg("Authorizing with TBAC"),
    ecoli_trace(P, "TBAC is enabled, filtering roles"),
    do_authorize(P, Target_list, Authent_list, Role_list);
authorize(P, false, _,  _, Role_list) ->
    %without TBAC, only roles without <Node Type> are returned
    %AND those with an explicit asterisk in the <Node Type> field
    % 6.2.1 in 2/155 01-FAE 151 04, PB4
    dbg("Authorize without TBAC available roles: ~p", [Role_list]), 
    ecoli_trace(P, "TBAC is disabled, filtering roles"),
    Roles = lists:foldl(
	fun(E, Acc) ->
		case colon_split(E) of
		    [] ->
			Acc;
		    R ->
			[R | Acc]
		end
	end, [], Role_list),
    dbg("Authorize returned roles: ~p", [Roles]), 
    Roles.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%this does auhtorization if TBAC=UNLOCKED
%Target_list is free of duplicates already
do_authorize(P, T_list, Authent_list, Role_list) ->
    %every string is made into lower-case. NO, absolutely NOT!

    %%the T_list contains items like these: "brup", "frop.vips.kola"
    %%for now strings in the Target_list containing "." receive NO special
    %%treatment (because I don't know if they should)
    ecoli_trace(P, "Target list: ~p", [T_list]),
    ecoli_trace(P, ?AUTHENTICATESCOPE " list: ~p", [Authent_list]),
    Type_roles = concat_roles(Role_list),
    ecoli_trace(P,
	"Concatenated " ?AUTHORIZESCOPE ": ~p",
	[Type_roles]),
    {I, Matching} = dumb_intersection(T_list, Authent_list, Type_roles),
    ecoli_trace(P, "Intersecting target list (from the three above): ~p", [I]),
    dbg("Matching roles after TBAC: ~p", [Matching]),
    dbg("Authorizing with TBAC returned roles: ~p", [Matching]),
    %what about filtering out duplicates (or emties)?
    Matching.

%do this in particularly inefficient way since I probably have to redo this
%later. I'm unsure about how it should work.
dumb_intersection(Type_list, Auth_list, Type_roles) ->
    lists:foldl(
	fun({"*", Roles}, {Is, Rs}) ->
		%authorization with "*" is added unconditionally
		{["*" | Is], Roles ++ Rs};
	   ({T, Roles}, {Is, Rs} = Acc) ->
		%anything but "*" requires an intersection between
		%target-type-list and authentificationscope-list 
		case lists:member(T, Type_list) andalso
			lists:member(T, Auth_list) of
		    true ->
			{[T | Is], Rs ++ Roles};
		    false ->
			Acc
		end
	end, {[], []}, Type_roles).

%input ["brap:dolk", "brap:kov", "vroom:plutt", "discard_me"...]
%output [{"brap",["dolk", "kov]}, {"vroom", ["plutt"]}..
%items without ":" discarded
concat_roles(T_r_list) ->
    lists:foldl(
	fun(Tr, Acc) ->
	    case string:tokens(Tr, ":") of
		[Type, R] ->
		   add_to_acc(Type, R, Acc);
		[_] -> %Not acceptable according to artf337312
		    Acc;
		_ -> 
		    warn_msg("Illegal role in authorization list: ~p", [Tr]),
		    Acc
	    end
	end, [], T_r_list).

add_to_acc(T, R, Acc) ->
    case lists:keytake(T, 1, Acc) of
	false ->
	    [{T, [R]} | Acc];
	{value, {T, Roles}, New_acc} ->
	    [{T, [R | Roles]} | New_acc]
    end.


% accept either a string with no colon e.g. "No_colon" or a string prepended
% with *: e.g "*:No_more_colon"
% return the string if acceptable, else return []
colon_split([]) -> [];
colon_split([$*]) -> []; %lets disallow a single asterisk too
colon_split([$*, $:]) -> [];
colon_split([$*, $: | Role]) ->
    check_for_colon(Role, Role);
colon_split(Role) ->
    check_for_colon(Role, Role).

check_for_colon([], Role) -> Role;
check_for_colon([$: | _], _) -> [];
check_for_colon([_ | T], Role) ->
    check_for_colon(T, Role).

warn_msg(Format, Params) ->
    sysInitI:warning_msg("~p: " ++ Format ++ "~n", [?MODULE | Params]).

ecoli_trace(Decide, Arg) ->
    ecoli_trace(Decide, Arg, []).

ecoli_trace(#ldap_params{trace = Io, server = Srv},
	Arg, Params) when Io =/= undefined ->
    omc_ldap_server:ecoli_trace(Io, atom_to_list(Srv) ++ ": " ++ Arg, Params);
ecoli_trace(_, _, _) ->
    ok.

dbg(_) -> ok.
dbg(_, _) -> ok.
%dbg(Format) ->
%    dbg(Format, []).
%dbg(Format, Args) ->
%    io:format("dbg: ~p:" ++ Format ++ "~n", [?MODULE | Args]).
