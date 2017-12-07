%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_ldap_instance.erl %
%%% Author:	etxbjca
%%% Description: Worker process spawn-linked from omc_ldap_server
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(omc_ldap_instance).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R11A/1').
-date('2017-10-11').
-author('etomist').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
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
%%% Rx         2014-03-05 etxlg     Created
%%% R2A/2      2014-03-10 etxlg     First working
%%% R2A/3      2014-03-11 etxlg     Handle missing bindPw, disable dbg()
%%% R2A/4      2014-03-12 etxlg     simplified
%%% R2A/5      2014-03-19 etxlg     Fix for starttls/ldaps, tracing
%%% R2A/6      2014-03-24 etxlg     Fix for aliasing, changed tracing
%%% R2A/7      2014-03-25 etxlg     Better handling of bad flexfilter
%%% R2A/8      2014-03-26 etxlg     verify_fun
%%% R2A/9      2014-07-09 etxlg     bug fix for LDAPS
%%% R2A/10     2014-07-21 etxjotj   Removed debug printout
%%% R2A/11     2014-08-18 etxlg     error -> warning print
%%% R2A/12     2014-09-08 etxlg     Improved some trace printouts
%%% R2A/13     2014-10-02 etxlg     handle options for DSCP and binding
%%% R2A/14     2014-10-23 etxtory   Remove tls vsn 3
%%% R2A/15     2014-10-28 etxlg     Role -> role, TR HT18487
%%% R2A/16     2014-12-02 etxlg     use bindDN when doing Atz TR HT29158
%%% R2A/17     2014-12-19 etxlg     workaround to fix TR HT27670
%%%                                 also fixed in OTP 17.4, maybe not merge
%%% R3A/1      2015-02-24 etxlg     verify_fun is a list [verify_fun, partial_fun]
%%% R5A/1      2016-03-08 ehsake    tls depth set to 10, TR HU64566
%%% R5A/2      2016-04-05 ehsake    rebind instead of open new connection, HU72746
%%% R6A/1      2016-08-30 emariad   CSUC feature, cipher configuration
%%% R11A/1     2017-10-11 etomist   OTP20 - disabling MITM protection
%%% ----------------------------------------------------------
%%% 
-include_lib("eldap/include/eldap.hrl").
-include("omc.hrl").

%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([authenticate/4]).
-export([authorize/3]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
authenticate(Params, Host, User, Pw) ->
    do_aa(authenticate, Params, Host, User, Pw).

authorize(Params, Host, User) ->
    do_aa(authorize, Params, Host, User, undefined).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

do_aa(What, Params, Host, User, Password) ->
    dbg("do_aa, Server: ~p, User: ~p", [Host, User]),
    ecoli_trace(Params, "Query start, type: ~p server: ~p, user: ~p",
	[What, Host, User]),
    {Handle, Dn, Authorization, Authentication} =
	bind_server_and_search(Params, Host, User),
    dbg("do_aa, Authentication: ~p, Authorization: ~p",
	[Authentication, Authorization]),
	case What of
	    authenticate ->
		ecoli_trace(Params,
		    "Preparing to authenticate, rebind to user dn"),
		bind_user(Params, Handle, Dn, Password),
		ecoli_trace(Params,
		    "Authenticate done, rebind to default bind DN"),
	        conditional_server_bind(Params, Handle);
	    authorize ->
		ecoli_trace(Params,"Proceed with authorization")
	end,
    Roles = do_authorize(Params, Handle, Authentication,
			Authorization, User),
    ecoli_trace(Params, "Roles extracted from authorization: ~p",
		[Roles]),
    dbg("authorization roles: ~p", [Roles]),
    Alias_roles =
	do_aliasing(Params, Handle, Authentication, Roles),
    ecoli_trace(Params, "Roles after completed authorization: ~p",
		[Alias_roles]),
    close_bind(Handle),
    dbg("authorization roles with aliasing: ~p", [Alias_roles]),
    exit({shutdown, {ok, Alias_roles}}). %% working case

%return {DN, Authorization_list, Authentication_list}
bind_server_and_search(Params, Host, User) ->
    case {Params#ldap_params.bind_dn, Params#ldap_params.tls_mode} of
	{"", none} ->
	    sysInitI:warning_msg(
		"~p: No bindDn and no SASL (i.e. TLS towards the LDAP-server, "
		"is configured -> cannot bind -> cannot authenticate~n",
		[?MODULE]),
	    ecoli_trace(Params,
		"Configuration: no bindDN, no TLS, unable to bind"),
	    exit({shutdown, {error,
		"erroneous configuration - cannot bind for ldap search, "
		"no bindDn, no TLS"}});
	_ ->
	    ecoli_trace(Params, "Opening connection to search and bind"),
	    Handle = open_ldap_connection(Params, Host),
	    conditional_server_bind(Params, Handle),
	    {Dn, Authorization, Authentication} =
		search_posix_account(Params, Handle, User),
	    ecoli_trace(Params, "Decoded search results DN: ~p~n"
		?AUTHENTICATESCOPE ": ~p~n"
		?AUTHORIZESCOPE    ": ~p",
		[Dn, Authentication, Authorization]),

	    dbg("Search succeded, got DN: ~s Authorization: ~p Authent: ~p",
		[Dn, Authorization, Authentication]),
	    {Handle, Dn, Authorization, Authentication}
    end.

%return the ldap-handle
open_ldap_connection(Params, Host) ->
    Options = [
	{port, Params#ldap_params.server_port},
	{timeout, ?LDAP_TIMEOUT}
	] ++ Params#ldap_params.extra_inet_opts ++ make_tls_opts(Params),
    %%an attempt to fix TR HT27670, timeout is not honored by OTP:ELDAP if
    %%{ssl, true} is set to get LDAPS (supposedly fixed in OTP 17.4)    
    {ok, Tref} = timer:exit_after(?LDAP_TIMEOUT, self(),
				  {shutdown, unavailable}),
    case eldap:open([Host], Options) of
	{ok, Handle} ->
	    timer:cancel(Tref),
	    ecoli_trace(Params, "LDAP connection opened"),
	    dbg("Opened ldap handle: ~p", [Handle]), 
	    maybe_do_starttls(Params, Handle);
	{error, "connect failed"} -> %special case, failover to secondary
	    ecoli_trace(Params, "LDAP connection FAILED: connect failed"),
	    dbg("connect failed - returning 'unavailable'" ), 
	    exit({shutdown, unavailable});
	{error, Error} ->
	    ecoli_trace(Params, "LDAP connection FAILED: ~s",
		[error_to_string(Error)]),
	    dbg("Open ldap - returning {'error', ...} error: ~p", [Error]),
	    exit({shutdown, {error, "ldap connection error: " ++
					error_to_string(Error)}})
    end.

maybe_do_starttls(#ldap_params{tls_mode = start_tls} = P, Handle) ->
    ecoli_trace(P, "Upgrading connection to TLS"),
    case eldap:start_tls(Handle, tls_opts(P), ?LDAP_TIMEOUT) of
	ok ->
	    ecoli_trace(P, "STARTTLS succeeded"),
	    dbg("STARTTLS succeeded"),
	    Handle;
	{error, {response, Server_response}} ->
	    catch eldap:close(Handle),
	    ecoli_trace(P, "STARTTLS FAILED, server error: ~s",
		[error_to_string(Server_response)]),
	    exit({shutdown, {error, "start_tls server error: " ++
					error_to_string(Server_response)}});
	{error, Error} -> 
	    catch eldap:close(Handle), %HERE Error must be sanitized
	    ecoli_trace(P, "STARTTLS FAILED"),
	    exit({shutdown, {error, "start_tls error: " ++
					error_to_string(Error)}})
    end;
maybe_do_starttls(_, Handle) ->
    Handle.

make_tls_opts(#ldap_params{tls_mode = ldaps} = Params) ->
    ecoli_trace(Params, "LDAPS: connection will be done using TLS"),
    [{ssl, true}, {sslopts, tls_opts(Params)}];
make_tls_opts(_) ->
	[{ssl, false}].

tls_opts(P) ->
    [{cert, P#ldap_params.node_cert},
     {key, P#ldap_params.node_key},
     {cacerts, P#ldap_params.ca_certs},
     {secure_renegotiate, true},  %check if this is an issue
     {depth, 10}, 
     {versions, [tlsv1, 'tlsv1.2']},
     {ciphers, comsaI:get_tls_cipher_suites()},
     %%%P#ldap_params.verify_fun,
     {log_alert, false}, %can appearantly  be very verbose...
     %%check if this makes a difference since CERT should do all the work
     %%through the verify_fun
     %% No, keep this, CERT only checks CRL stuff
     {verify, verify_peer},
     {server_name_indication, disable}] ++
     P#ldap_params.verify_fun.

conditional_server_bind(P, Handle) ->
    case {P#ldap_params.bind_dn, P#ldap_params.bind_password} of
	{"", _} -> %means SASL should do the job, skip binding
	    ecoli_trace(P,
		"Configuration: empty bindDN, bind will not be done"),
	    ok;
	{_, undefined} -> %%not a legal case, old config gets us here
			  %%or possibly this should mean pw: "" and
			  %%we attempt anonymous bind
	    ecoli_trace(P,
		"Configuration: empty bindPassword, unable to bind"),
	    exit({shutdown, {error,
		"erroneous configuration - "
		"cannot bind for ldap search, "
		"no bindPassword"}});
	_->
	    server_bind(P, Handle)
    end.

%does bind to the server using bind_dn and bind_password exit if it fails
server_bind(P, Handle) ->
    case eldap:simple_bind(Handle, P#ldap_params.bind_dn,
			comsaI:decrypt_password(P#ldap_params.bind_password)) of
	ok -> 
	    ecoli_trace(P, "bind to: ~p succeeded", [P#ldap_params.bind_dn]),
	    dbg("ldap server bind succeded to: ~p", [P#ldap_params.bind_dn]),
	    ok;
	{error, Reason} ->
	    ecoli_trace(P, "bind to: ~p FAILED", [P#ldap_params.bind_dn]),
	    exit({shutdown, {error, "bind failure: " ++
					error_to_string(Reason)}})
    end.

%returns DN of User and
search_posix_account(P, Handle, User) ->
    ecoli_trace(P, "Preparing to search for user: ~p", [User]),
    dbg("preparing to search posixAccount for User: ~p", [User]),
    %if targetBasedAccessControl=UNLOCKED this will generate a filter
    %that includes checking the ericssonUserAuthenticationScope, unfortunately
    %we need to check authent again since eldap cannot do seachmodification
    %and we should search with case here (ericssonUserAuthenticationScope is
    %defined for caseINsensitive search in the schema)
    Filter = mk_filter(P, User),
    %we search for the ericssonUserA...stuff always, it is not expected
    %unless the profileFilter is set to 'ericsson', however it is also not used
    %unless EricssonFilter=1,targetBasedAccessControl=UNLOCKED
    Atts = ["dn", ?AUTHORIZESCOPE, %does "dn" make sense here?
	    ?AUTHENTICATESCOPE], %expected by the TBAC code
    case ldap_search(Handle, P#ldap_params.base_dn, Filter, Atts) of
	{ok, #eldap_search_result{entries = [Entry]}} ->
	    ecoli_trace(P, "Entry found:~n~p", [Entry]),
	    dbg("eldap_search_result: ~p~n", [Entry]),
	    check_account(P, Entry);
	{ok, #eldap_search_result{entries = []}} ->
	    ecoli_trace(P, "Search returned empty"),
	    dbg("eldap_search_result empty~n"),
	    exit({shutdown, {error,
			"ldap search failure: search returned empty"}});
	{ok, #eldap_search_result{entries = Es}} when length(Es) =/= 1 ->
	    ecoli_trace(P, "Search returned multiple entries: ~b",
		[length(Es)]),
	    dbg("eldap_search_result multiple~n"),
	    exit({shutdown,
		{error, "ldap search failure: multiple entries returned"}});
	{error, Reason} ->
	    ecoli_trace(P, "Search failed: ~p", [error_to_string(Reason)]),
	    exit({shutdown, {error, "ldap search failure: " ++
				error_to_string(Reason)}})
    end.

ldap_search(Handle, Base, Filter, Attributes) ->
    Options = [{base, Base},
		{filter, Filter},
		{scope, eldap:wholeSubtree()}, %default and according to doc
		{deref, eldap:derefAlways()}, %not sure, but this is default
		{types_only, false}, %default
		{attributes, Attributes},
		{timeout, ?LDAP_TIMEOUT}],
    dbg("search options: ~p", [Options]),
    eldap:search(Handle, Options).

%filter taking into account that TBAC is in play
%Example of filter 
%(&((uid=User)(objectClass=posixAccount)(|
%(ericssonUserAuthenticationScope=Target1)
%(ericssonUserAuthenticationScope=Target2)
%(ericssonUserAuthenticationScope=\2a)))   <- wildcarding \2a = "*"
mk_filter(#ldap_params{use_tbac = true} = Params, User) ->
    Targets = [eldap:equalityMatch(?AUTHENTICATESCOPE, T) ||
		T <- Params#ldap_params.target_type],
    Wild = eldap:equalityMatch(?AUTHENTICATESCOPE, "*"),
    Filter = eldap:'and'([eldap:equalityMatch("uid", User),
			eldap:equalityMatch("objectClass", "posixAccount"),
			eldap:'or'([Wild | Targets])]),
    ecoli_trace(Params, "Search with TBAC filter:~n~p", [Filter]),
    Filter;
%ordinary default filter for posix
%I.e. (&(uid=User) (objectClass=posixAccount))
mk_filter(Params, User) ->
    Filter = eldap:'and'([eldap:equalityMatch("uid", User),
			eldap:equalityMatch("objectClass", "posixAccount")]),
    ecoli_trace(Params, "Search without TBAC filter:~n~p", [Filter]),
    Filter.

%only one match user not locked, account not expired,
% return {DN, AuthorizeScope, AuthenticateScope}
%according to maildiscussion with Balaz Kovacs, locked/expired really means:
%if bind succeds we are OK.
check_account(_Params, Entry)->
    {Entry#eldap_entry.object_name,
     proplists:get_value(?AUTHORIZESCOPE,
			Entry#eldap_entry.attributes, []),
     proplists:get_value(?AUTHENTICATESCOPE,
			Entry#eldap_entry.attributes, [])}.

bind_user(Params, Handle, Dn, Password) ->
    case eldap:simple_bind(Handle, Dn, Password) of
	ok -> 
	    ecoli_trace(Params, "Bind to DN: ~p succeeded", [Dn]),
	    dbg("Bind succeded to DN: ~s", [Dn]),
	    ok;
	{error, Reason} ->
	    dbg("Bind FAILED to DN: ~s", [Dn]),
	    ecoli_trace(Params, "Bind to DN: ~p FAILED: ~p", [Dn, Reason]),
	    exit({shutdown, {error, "ldap bind failure: " ++
				error_to_string(Reason)}})
    end.

%fetch roles from cn-attribute in posixGroup-object
%(& (objectClass=posixGroup)(memberUid=%u)) cn
%return: [roles]
do_authorize(#ldap_params{profile_filter = posix} = P,
		Handle,_, _, User) ->
    ecoli_trace(P, "Authorizing according to filter: posix"),
    dbg("authorize according to profilefilter: posix"),
    Filter = eldap:'and'([
		eldap:equalityMatch("objectClass", "posixGroup"),
		eldap:equalityMatch("memberUid", User)]),
    ecoli_trace(P, "Search filter: ~p", [Filter]),
    case ldap_search(Handle, P#ldap_params.base_dn, Filter, ["cn"]) of
	{ok, #eldap_search_result{entries = Entries}} ->
	    ecoli_trace(P, "Number of matching entries: ~b", [length(Entries)]),
	    ecoli_trace(P, "Entries:~n~p", [Entries]),
	    dbg("cn entries in posixGroup: ~p", [Entries]),
		extract_roles_from_type(Entries, "cn");
	{error, Reason} ->
		ecoli_trace(P, "Search FAILED: ~p", [Reason]),
		close_bind(Handle),
	    exit({shutdown, {error, "error searching posixGroups: " ++
					error_to_string(Reason)}})
    end;
%Use ericssonUserAuthorizationScope-attribute from the extend posixAccount
%optional TBAC followed by optional Role aliasing
%return: [roles]
do_authorize(#ldap_params{profile_filter = ericsson,
			  target_type = Target_type,
			  use_tbac = Use_tbac} = P,
		_Handle, Authentication, Authorization, _User) ->
    ecoli_trace(P, "Authorizing according to filter: ericsson"),
    dbg("authorize according to profilefilter: ericsson"),
    dbg("current authorization: ~p", [Authorization]),
    dbg("current authentication: ~p", [Authentication]),
    Roles_after_tbac = %HERE
	omc_tbac:authorize(P, Use_tbac, Target_type,
				Authentication, Authorization),
    dbg("authorization after TBAC: ~p", [Roles_after_tbac]),
    Roles_after_tbac;
%fetch roles using the flexible filter MO
%return: [roles]
do_authorize(#ldap_params{profile_filter = flexible,
			  flexible_filter = {Filter, Type}} = P,
			Handle, _, _, User) ->
    ecoli_trace(P, "Authorizing according to filter: flexible"),
    dbg("authorize according to profilefilter: flexible"),
    dbg("filter to use: ~p", [Filter]),
    put(flexible_filter_uid, User), %fusk
    try begin
	{ok, Lex, _} = omc_ldap_filter_lexer:string(Filter),
	omc_ldap_filter_parser:parse(Lex)
    end of
	{ok, {ok, E_filter}} ->
	    dbg("flexible filter: ~p", [E_filter]),
	    ecoli_trace(P, "Search filter:  ~p", [E_filter]),
	    case ldap_search(Handle, P#ldap_params.base_dn, E_filter, [Type]) of
		{ok, #eldap_search_result{entries = Entries}} ->
		%multiple entries->ok
		    ecoli_trace(P, "Number of matching entries: ~b",
				[length(Entries)]),
		    ecoli_trace(P, "Entries:~n~p", [Entries]),
		    ecoli_trace(P, "Extracting roles from ldap attribute:~p",
				[Type]),
		    dbg("Type: ~s entries from flexible filter search: ~p",
		    [Type, Entries]),
		    extract_roles_from_type(Entries, Type);
		{error, Reason} ->
		    ecoli_trace(P, "Search FAILED: ~p", [Reason]),
		    close_bind(Handle),
		    exit({shutdown, {error,
				"error searching with flexible filter" ++
				error_to_string(Reason)}})
	    end;
	Fail ->
	    ecoli_trace(P, "Failed to compile flexible filter expression: "
			    "~p -> ~p",
			[Filter, Fail]),
	    exit({shutdown, {error,
			"error building flexible filter expression: "
			"filter configuration error"}})
    catch
	Fail2:Fail3 ->
	    ecoli_trace(P, "Failed to compile flexible filter expression: "
			    "~p -> ~p:~p",
			[Filter, Fail2, Fail3]),
	    exit({shutdown, {error,
			     "error parsing flexible filter expression"}})
    end.

extract_roles_from_type(Entries, Type) ->
    Roles =
	lists:foldl(
	   fun(#eldap_entry{attributes = []}, Acc) -> Acc;
	      (#eldap_entry{attributes = Atts}, Acc) ->
		    case lists:keyfind(Type, 1, Atts) of
			{_, Role_list} -> Acc ++ Role_list;
			false -> Acc
		    end
	    end, [], Entries),
    %%get rid of any duplicates and emties
    lists:filter(
	fun([]) -> false;
	   (_) -> true end, lists:usort(Roles)).

	
close_bind(Handle) ->
    dbg("close bind, Handle: ~p", [Handle]),
    catch eldap:close(Handle).

do_aliasing(#ldap_params{role_alias_base_dn = []} = P, _, _, Roles) ->
    ecoli_trace(P, "Config: roleAliasesBaseDn not set, no aliasing is done"),
    dbg("No role aliasing to be done"),
    Roles;
do_aliasing(#ldap_params{role_alias_base_dn = R_base_dn} = P,
		Handle, Authent_list, Roles) ->
    ecoli_trace(P, "Performing role-aliasing by searching in DN: ~p",
	[R_base_dn]),
    ecoli_trace(P, "Searching for these roles: ~p", [Roles]),
    %%Atts = ["Role", ?AUTHORIZESCOPE], %unexplained stupidity
    Atts = ["role", ?AUTHORIZESCOPE],
    Filter = mk_alias_filter(Roles),
    ecoli_trace(P, "Search filter for role-aliasing: ~p", [Filter]),
    case ldap_search(Handle, R_base_dn, Filter, Atts) of
	{ok, #eldap_search_result{entries = []}} ->
	    ecoli_trace(P,
		"Search returned empty - returning all roles unchanged"),
	    Roles;
	{ok, #eldap_search_result{entries = Entries}} ->
	    ecoli_trace(P, "Search returned: ~p", [Entries]),
	    continue_aliasing(P, Authent_list, Roles, Entries);
	{error, Reason} ->
	    ecoli_trace(P,
		"Search returned error: ~p - returning all roles unchanged",
		[Reason]),
	    Roles
    end.

continue_aliasing(#ldap_params{use_tbac = Use_tbac,
			       target_type = Target_list} = P,
			Authent_list, Roles, Entries) ->
    Role_aliases = mk_role_aliases(Entries),
    [ecoli_trace(P, "Found alias: ~p -> ~p", [A, B]) || {A, B} <- Role_aliases],
    {Alias_list, R_list} =
	lists:foldl(
	    fun(R, {A_acc, R_acc}) ->
		case lists:keyfind(R, 1, Role_aliases) of
		    {R, []} ->
			ecoli_trace(P,
			    "Alias for role: ~p is empty - role removed", [R]), 
			{A_acc, R_acc};
		    {R, Aliases} ->
			ecoli_trace(P,
			    "role: ~p adding aliases: ~p", [R, Aliases]), 
			{A_acc ++ Aliases, R_acc};
		    false ->
			ecoli_trace(P,
			    "No alias for role: ~p - add role unchanged", [R]), 
			{A_acc, [R | R_acc]}
		end
	    end, {[], []}, Roles),
    ecoli_trace(P, "Found these aliases:~n~p", [Alias_list]),
    ecoli_trace(P, "Following roles have no alias, kept as is: ~p", [R_list]),
    After_tbac =  omc_tbac:authorize(P, Use_tbac, Target_list,
				     Authent_list, Alias_list),
    ecoli_trace(P, "Additional roles after aliasing (and possibly TBAC): ~p",
		[After_tbac]),
    R_list ++ After_tbac.

mk_alias_filter(Roles) ->
    Rf = [eldap:equalityMatch("role",R) || R <-Roles],
    eldap:'and'([eldap:equalityMatch("objectClass", "ericssonRoleAlias"),
		eldap:'or'(Rf)]).

%gets a list of #eldap_entry{} -> [{Role, [Alias, Alias]}]
mk_role_aliases(Entries) ->
    lists:foldl(
	fun(#eldap_entry{attributes = As}, Acc) ->
	    case {lists:keyfind("role", 1, As),
		  lists:keyfind(?AUTHORIZESCOPE, 1, As)} of
		{{"role", [R]}, {?AUTHORIZESCOPE, Aliases}} ->
		    [{R, Aliases} | Acc];
		_ -> Acc
	    end
	end, [], Entries).

error_to_string(Anything) when is_list(Anything) ->
    Anything;
error_to_string(Anything) ->
    lists:flatten(io_lib:format("~p", [Anything])).

ecoli_trace(Decide, Arg) ->
    ecoli_trace(Decide, Arg, []).
ecoli_trace(#ldap_params{trace = Io, server = Srv},
	Arg, Params) when Io =/= undefined ->
    omc_ldap_server:ecoli_trace(Io,
	atom_to_list(Srv) ++ ": " ++ Arg, Params);
ecoli_trace(_, _, _) ->
    ok.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

dbg(_) -> ok.
dbg(_, _) -> ok.
%dbg(Format) ->
%    dbg(Format, []).
%dbg(Format, Args) ->
%    io:format("dbg: ~p:" ++ Format ++ "~n", [?MODULE | Args]).
