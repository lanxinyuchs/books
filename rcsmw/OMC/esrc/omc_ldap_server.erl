%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_ldap_server.erl %
%%% Author:	etxbjca
%%% Description: Gen-server to keep track of ldap queries/state
%%% 	For every lookup a process is spawn-linked, this process will
%%%	in turn spawn an additional process for each server that is to be
%%%	queried.
%%%	In the normal case that will be one additional process to query
%%%	the primary ldap server, however, if the primary ldap server is
%%%	inresponsive the server state will change to 'dual' causing two
%%%	additional processes to be spawned, one to primary and one to
%%%	secondary. In state 'dual' the answer from whichever server answers
%%%	first will be used, but an answer from the primary server will be
%%%	detected causing the serverstate to return to 'primary'.
%%%
%%%	If no server responds serverstate become 'unavailable'.
%%%	It has been decided that there should be NO alarms for ldap-server
%%%     unavailbability (maybe add something to the security log)
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(omc_ldap_server).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R12A/1').
-date('2017-11-20').
-author('eivomat').
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
%%% Rx         2014-02-28   etxlg     Copied from omc_eldap
%%% R2A/2      2014-03-10   etxlg     First working
%%% R2A/3      2014-03-11   etxlg     warn print, disabled dbg()
%%% R2A/4      2014-03-19   etxlg     Fix for starttls, tracing
%%% R2A/5      2014-03-24   etxlg     Ecoli cmds
%%% R2A/6      2014-03-25   etxlg     empty Roles -> {false, "Empty roles"}
%%% R2A/7      2014-03-26   etxlg     fix print_ldap_params() dbg-function
%%% R2A/8      2014-03-26   etxlg     use verify_fun from CERT, still broken
%%% 				      handle shutdown case
%%% R2A/9      2014-03-27   etxlg     cleaned up verify_fun, it works now
%%% R2A/10     2014-03-28   etxlg     moved stuff to omc_lib
%%% R2A/11     2014-04-02   etxlg     workaround for TLM
%%% R2A/12     2014-07-04   etxlg     filter out the super-OaM-user
%%% R2A/13     2014-07-28   etxlg     secondary doesn't run if unconfigured
%%% R2A/14     2014-08-14   etxlg     removed compatibility lookup
%%% R2A/15     2014-09-08   etxlg     Targetlist made casesensitive
%%% R2A/16     2014-10-02   etxlg     DSCP and bind to OaM IP
%%% R2A/17     2014-10-09   etxlg     RCS-COLI command added(divided/changed)
%%% R2A/18     2014-10-15   etxlg     fetch params from inside gen_server
%%% R3A/1      2014-12-11   etxlg     Network namespace (OaM and LMT)
%%% R3A/2      2015-01-13   etxpeno   support for roles
%%% R3A/3      2015-02-24   etxlg     Cleaner debug printout
%%% R3A/4      2015-10-14   etxlg     \n\o\w() removed
%%% R3A/5      2015-11-09   etxlg     IPv6
%%% R5A/2      2016-01-13   etxlg     Merged from R4 (=R3)
%%% R12/1      2017-11-20   eivomat   Illegal role LDAP error to info
%%% ----------------------------------------------------------
%%%
-include_lib("eldap/include/eldap.hrl").
-include("omc.hrl").
%%no, added stuff in comSecM instead  -include("ComLdapAuthentication.hrl").

%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([ldap_lookup/2, ldap_lookup/1]).

%-export([get_user/1]).

-export([start_link/0]).

%for use when running through RCS-COLI
-export([ecoli_atrace_lookup/1]).
-export([ecoli_aatrace_lookup/1]).
-export([ecoli_mk_filter/1]).

%for printing when calling the above
-export([ecoli_trace/2, ecoli_trace/3]).

%%exported for debug
-export([print_ldap_params/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%gen_server callbacks
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2]).
%format_status/2]).

-define(SERVER, ?MODULE).

-record(st, {
	state = primary, %primary | dual | unavailable
	workers = gb_trees:empty(),
	all_params = not_used
	}).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, sysEnv:role(), []).

%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
-spec ldap_lookup(string()) -> {true, [string()]} | {false, string()}.
ldap_lookup(User) ->
    %this try-catch is mainly here to prevent badness when a call comes in
    %just as the system is shutting down
    try begin
	%Params = get_all_params(),
	gen_server:call(?SERVER, {authorize, User})
    end
    catch
	exit:{noproc, _} ->
	    {false, "no process - system shutting down"};
	_:_ ->
	    {false, "no process"}
    end.

-spec ldap_lookup(string(), string()) -> {true, [string()]} | {false, string()}.
ldap_lookup(User, Pw) ->
    case os:getenv("TARGET_TLM") of
	false ->
	    do_ldap_lookup(User, Pw);
	_ ->
	   {true, ["expert", "coli", "tlm_fake_user"]}
    end.

do_ldap_lookup(User, Pw) ->
    case get({roles, User}) of  %% For test
        undefined ->
	    try begin
		%Params = get_all_params(),
		gen_server:call(?SERVER, {authenticate, User, Pw})
	    end
	    catch
		exit:{noproc, _} ->
		    {false, "no process - system shutting down"};
		_:_ ->
		    {false, "no process"}
	    end;
        Roles ->
            {true, Roles}
    end.

%this is the version to be authorized in RCS-COLI, it takes no password, thus
%avoiding that it ends up in the audittrace-log
ecoli_atrace_lookup([User]) ->
    ecoli_aatrace_lookup([User]);
ecoli_atrace_lookup(_) ->
    exit("argument error").

ecoli_aatrace_lookup(Args) ->
    Params = (get_all_params())#ldap_params{trace = group_leader()},
    case Args of
	[User] ->
	    ecoli_trace(true,
		"Trace printing of authorization, parameter info:~n~s",
		[format_params(Params)]),
	    Result = gen_server:call(?SERVER, {authorize, Params, User}),
	    ecoli_trace(true, "Query result: ~p", [Result]);
	[User, Pw] ->
	    case get({roles, User}) of  %% For test
		undefined ->
		    ecoli_trace(true,
			"Trace printing of authentication, parameter info:~n~s",
			[format_params(Params)]),
		    Result = gen_server:call(?SERVER, {authenticate, Params, User, Pw}),
		    ecoli_trace(true, "Query result: ~p", [Result]);
		Roles ->
		    ecoli_trace(true,
			"Simulated environment, authenticated by default, "
			"roles from process dictionary"),
		    ecoli_trace(true, "Query result: ~p", [{true, Roles}])
	    end;
	_ ->
	    exit("argument error")
    end.

ecoli_mk_filter(Args) ->
    {Filter, User} =
	case Args of
	    [F] ->
		{F, "User_undefined"};
	    [F, U] ->
		{F, U};
	    _ ->
		exit("argument error")
	end,
    put(flexible_filter_uid, User),
    try begin
    {ok, Lex, _} = omc_ldap_filter_lexer:string(Filter),
    omc_ldap_filter_parser:parse(Lex)
    end of
	{ok, {ok, E_filter}} ->
	    ecoli_trace(true,  "Filter: ~p", [E_filter]);
	Error ->
	    ecoli_trace(true,  "Filter compile error: ~p", [Error])
    catch
	A:B ->
	    ecoli_trace(true,  "Filter compile error: ~p:~p", [A, B])
    end.

%If the lexer is run on a flexible filter that requires the user to be
%patched into the query, it will call here. (except if we now do /put/get)
%get_user(Pid) ->
%    gen_server:call(?SERVER, {get_filter_user, Pid}).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
init(active) ->
    process_flag(trap_exit, true),
    %%FIXME process_flag(sensitive, true), %and maybe also format_status/2
    {ok, #st{}};
init(regular) ->
    ignore.

handle_call({authenticate, User, Pw}, From, S) ->
    handle_call({authenticate, get_all_params(), User, Pw}, From, S);
handle_call({authenticate, Params, User, Pw}, From, S) ->
    New_s = do_authenticate(S, From, Params, User, Pw),
    {noreply, New_s};
handle_call({authorize, User}, From, S) ->
    handle_call({authorize, get_all_params(), User}, From, S);
handle_call({authorize, Params, User}, From, S) ->
    New_s = do_authorize(S, From, Params, User),
    {noreply, New_s};
% handle_call({get_filter_user, Pid}, _From, S) ->
%    {reply, "this is a strange user: " ++ erlang:pid_to_list(Pid), S};
handle_call(_Request, _From, S) ->
    {reply, "Request not recognized", S}.

handle_cast(_Request, S) ->
    {noreply, S}.

handle_info({'EXIT', Pid, Reason}, #st{workers = Workers} = S) ->
    case gb_trees:lookup(Pid, Workers) of
	{value, {_, _, _} = _V} ->
	    dbg("got Exit from aggregator: ~p, ~p, ~p", [Pid, _V, Reason]),
	    {noreply, S#st{workers = gb_trees:delete(Pid, Workers)}};
	none ->
	    dbg("got EXIT from unknown pid: ~p exit: ~p~n", [Pid, Reason]),
	    {noreply, S}
    end;

handle_info({Pid, Ok_error, Result},  #st{workers = Workers} = S) ->
    case gb_trees:lookup(Pid, Workers) of
        {value, {_, From, _}} ->
	    case Ok_error of
		ok ->
		    dbg("got good answer from aggregator: ~p, ~p, ~p",
			[Pid, Ok_error, Result]),
		    New_result = filter_out_super_user(Result),
		    %special case an empty role-list by transforming it into
		    %an authentication fail. I think this is what COM/ECIM
		    %expects.
		    case New_result of
			[] ->
			    gen_server:reply(From, {false, "Empty role list"});
			_ ->
			    gen_server:reply(From, {true, New_result})
		    end;
		error ->
		    dbg("got bad answer from aggregator: ~p, ~p, ~p",
			[Pid, Ok_error, Result]),
		    gen_server:reply(From, {false, Result})
	    end,
            {noreply, S};
        none ->
            dbg("got messaage from unknown pid: ~p", [Pid]),
            {noreply, S}
    end;
%add code for alarms and omc_lib:sec_log/2 HERE
%nope - there should be NO alarms for ldap
%However, we could print the state changes into the security log?
handle_info({primary, unavailable}, S) ->
    {noreply, S#st{state = dual}};
handle_info({primary, available}, S) ->
    {noreply, S#st{state = primary}};
handle_info({secondary, unavailable}, S) ->
    {noreply, S};
handle_info({secondary, available}, S) ->
    {noreply, S};
handle_info(_Info, S) ->
    dbg("Unrecognized handle_info: ~p~n", [_Info]),
    {noreply, S}.

code_change(_Oldvsn, S, _Extra) ->
    {ok, S}.

%format_status(terminate, [_, #st{all_params = P} = S]) ->
%    New_p = P#ldap_params{node_key = <<"hidden key">>},
%    [{data,  [{"State", S#st{all_params = New_p}}]}];
%format_status(normal, [_, S]) ->
%    [{data,  [{"State", S}]}].

terminate(_Reason, _S) ->
    ok.
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

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------

%return new State
do_authenticate(#st{workers = Workers, state = Qs} = S,
		From, Params, User, Pw) ->
    Parent = self(),
    ecoli_trace(Params, "Launching query aggregation process. state: ~p",
		[Qs]),
    Pid = proc_lib:spawn_link(
	fun() ->
	    query_aggregator(Parent, Qs, Params, authenticate,
				[User, Pw])
	end),
    New_workers =
	gb_trees:insert(Pid,
			{authenticate, From, os:system_time(milli_seconds)},
			Workers),
    S#st{workers = New_workers}.

%return new State
do_authorize(#st{workers = Workers, state = Qs} = S, From, Params, User) ->
    Parent = self(),
    ecoli_trace(Params, "Launching query aggregation process. state: ~p",
		[Qs]),
    Pid = proc_lib:spawn_link(
	fun() ->
	    query_aggregator(Parent, Qs, Params, authorize,
				[User])
	end),
    %New_workers = gb_trees:insert(Pid, {authorize, From, now()}, Workers),
    New_workers =
    gb_trees:insert(Pid,
        {authorize, From, os:system_time(milli_seconds)}, Workers),
    S#st{workers = New_workers}.

%%% ----------------------------------------------------------
%%% #           get_all_params()
%%% Input: -
%%% Output: #ldap_params{}
%%%
%%% Exceptions: Should be cached in the server and not run every time
%%% Description: Get LDAP attributes
%%% ----------------------------------------------------------
get_all_params() ->
    Ldap_p = omc_model:get_ldap_config(), %returns a proplist

    Tls_mode = figure_tls_mode(Ldap_p),
    Server_port = figure_server_port(Ldap_p, Tls_mode),
    {Profile_filter, Use_tbac, Role_alias_base_dn} =
	figure_filter_stuff(Ldap_p),
    Flexible_filter = figure_flexible_filter(Profile_filter),
    Dscp_opt = omc_api:get_dscp_opt_list(),
    Extra_ip_opts =
	case ootI:get_oap_ip_addr() of
	    [] ->
		%%No oap -> no bind, possibly namespace on LMT
		omc_api:get_lmt_ns_opt_list();
	    Bind_ip_string ->
		%assume this is on the form "IP1.IP2.IP3.IP4", i.e. not to be
		%run through resolver (or IPv6)
		case inet:parse_address(Bind_ip_string) of
		    {ok, Ip_tuple} ->
			[{ip, Ip_tuple}];
		    _ ->
			sysInitI:warning_msg(
			    "~p: unable to decode OaM IP address ~p~n",
				[?MODULE, Bind_ip_string]),
			[]
		end ++ omc_api:get_oam_ns_opt_list()
	end,
    {Ca_certs, Node_cert, Node_key, Vfun} =
	case Tls_mode of
	    none ->
		{[], <<>>, {undefined, <<>>},
		omc_lib:fetch_verify_fun("LDAP",
                    proplists:delete(trustCategory, Ldap_p),
                    {server, undefined})};
	    _ ->
		{N_cert, N_key, N_chain} =
                case omc_lib:fetch_node_cert(Ldap_p) of
                    {[Nc_cert|NcChain], NcKey} ->
                        {Nc_cert, NcKey, NcChain};
                    {Nc_cert, NcKey} -> % Remove when cert released
                        {Nc_cert, NcKey, []}
                end,
                TC = omc_lib:fetch_ca_certs(Ldap_p) ++ N_chain,
		{TC, N_cert, N_key, omc_lib:fetch_verify_fun("LDAP", Ldap_p,
                        {server, undefined})}
        end,

    #ldap_params{
	ldap_primary = proplists:get_value(ldapIpAddress, Ldap_p, undefined),
	ldap_secondary = proplists:get_value(fallbackLdapIpAddress, Ldap_p,
						undefined),
	server_port =  Server_port,
	bind_dn = proplists:get_value(bindDn, Ldap_p, ""),
	bind_password = proplists:get_value(bindPassword, Ldap_p, undefined),
	base_dn = proplists:get_value(baseDn, Ldap_p), %mandatory
	ca_certs = Ca_certs,
	node_cert = Node_cert,
	node_key = Node_key,
	verify_fun = Vfun, %this is now a list containing verify_fun and partial_fun
	tls_mode = Tls_mode,
	profile_filter = Profile_filter,
	flexible_filter = Flexible_filter,
	use_tbac = Use_tbac,
	%%get rid of any duplicates and emties
	target_type = normalized_target_type(),
	role_alias_base_dn = Role_alias_base_dn,
	extra_inet_opts = [{tcpopts, Dscp_opt ++ Extra_ip_opts}]
	}.

%run throught the list of target-types, get rid
%of emties and duplicates, it ends up sorted but that is incidental
normalized_target_type() ->
    Ts = lists:foldl(
		fun([], Acc) -> Acc;
		   (T, Acc) -> [T | Acc]
		 end, [], comsaI:get_target_type()),
    lists:usort(Ts).

%returns tls | start_tls | ldaps
figure_tls_mode(Ldap_p) ->
    Use_tls = proplists:get_value(useTls, Ldap_p),
    figure_tls_mode(Ldap_p, Use_tls).
figure_tls_mode(_, false) ->
    none;
figure_tls_mode(Ldap_p, true) ->
    proplists:get_value(tlsMode, Ldap_p).

figure_server_port(Ldap_p, Tls_mode) ->
    Server_port = proplists:get_value(serverPort, Ldap_p, undefined),
    case {Server_port, Tls_mode} of
	{undefined, none} -> 389;
	{undefined, start_tls} -> 389;
	{undefined, ldaps} -> 636;
	{Server_port, _} when is_integer(Server_port) ->
	    Server_port
    end.

%%%return: {Use_tbac, Role_alias_base_dn}
figure_filter_stuff(Ldap_p) ->
    case proplists:get_value(profileFilter, Ldap_p, posix) of
	posix ->
	    {posix, false, ""};
	flexible ->
	    {flexible, false, ""};
	ericsson ->
	    Efilter = omc_model:get_ldap_ericsson_filter(),
	    {ericsson,
	     proplists:get_value(targetBasedAccessControl, Efilter, false),
	     proplists:get_value(roleAliasesBaseDn, Efilter, [])}
    end.

figure_flexible_filter(flexible) ->
    Flex = omc_model:get_ldap_flexible_filter(),
    {proplists:get_value(filter, Flex, ""),
     proplists:get_value(type, Flex, "")};
figure_flexible_filter(_) ->
    {"", ""}.

-record(qa, {primary,
	     secondary,
	     sec_fun,
	     trace = undefined}).

%runs in separate spawn-linked process
query_aggregator(Parent_pid, Server_state, Params, Q_type, Q_params) ->
    process_flag(trap_exit, true),
    dbg("launching ldap query process towards primary"),
    ecoli_trace(Params, "Launching query towards primary server"),
    P_pid = proc_lib:spawn_link(omc_ldap_instance, Q_type,
                        [Params#ldap_params{server = primary},
			 Params#ldap_params.ldap_primary | Q_params]),
    Secondary_fun =
	case Params#ldap_params.ldap_secondary of
	    undefined ->
		undefined;
	    _ ->
		fun() ->
		    proc_lib:spawn_link(omc_ldap_instance, Q_type,
				[Params#ldap_params{server = secondary},
				Params#ldap_params.ldap_secondary | Q_params])
		end
	end,

    S_pid =
	case Server_state of
	    primary ->
		undefined;
	    _ -> %dual or unavailable
		case Secondary_fun of
		    undefined ->
			dbg("no secondary query - secondary unconfigured"),
			ecoli_trace(Params,
				    "No secondary query - server unconfigured"),
			undefined;
		    _ ->
			dbg("launching ldap query process towards secondary"),
			ecoli_trace(Params,
				    "Launching query towards secondary server"),
			Secondary_fun()
		end
	end,
    erlang:send_after(?LDAP_QUERY_TIMEOUT, self(), timeout),
    ecoli_trace(Params, "Query time out is: ~b ms", [?LDAP_QUERY_TIMEOUT]),
    query_loop(Parent_pid, Server_state, #qa{primary = P_pid,
					     secondary = S_pid,
					     sec_fun = Secondary_fun,
					     trace = Params#ldap_params.trace},
		[], false).

%strategy, a good answer is immediately returned
%bad (errors) are cached until time-out or all pids done, then returned
%if a good answer has not already been provided
%process exits when all pids are done, or at timeout
query_loop(Parent, S_state, #qa{primary = P_pid,
			       secondary = S_pid} = Q_state,
		Bad_resps, Answered) ->
    receive
	timeout -> %set all pids undefined to cause exit
	    ecoli_trace(Q_state, "Query TIMEOUT"),
	    check_for_done(Parent, S_state, #qa{}, Bad_resps, Answered);
	{'EXIT', P_pid, {shutdown, unavailable}} when S_state =:= primary ->
	    Secondary_pid =
		case Q_state#qa.sec_fun of
		    undefined ->
			ecoli_trace(Q_state, "Primary server unresponsive, "
					     "secondary server unconfigured"),
			undefined;
		    _ ->
			ecoli_trace(Q_state, "Primary server unresponsive, "
					     "launching query to secondary"),
			(Q_state#qa.sec_fun)()
		end,
	    Parent ! {primary, unavailable},
	    check_for_done(Parent, S_state,
				Q_state#qa{primary = undefined,
					   secondary = Secondary_pid},
				Bad_resps, Answered);
	{'EXIT', S_pid, {shutdown, unavailable}} ->
	    ecoli_trace(Q_state, "Secondary server unresponsive"),
	    Parent ! {secondary, unavailable},
	    check_for_done(Parent, S_state, Q_state#qa{secondary = undefined},
				Bad_resps, Answered);
	{'EXIT', P_pid, {shutdown, {error, Reason}}} ->
	    ecoli_trace(Q_state, "Primary server error response: ~p", [Reason]),
	    check_for_done(Parent, S_state, Q_state#qa{primary = undefined},
				[{primary, Reason} | Bad_resps], Answered);
	{'EXIT', S_pid, {shutdown, {error, Reason}}} ->
	    ecoli_trace(Q_state, "Secondary server error response: ~p", [Reason]),
	    check_for_done(Parent, S_state, Q_state#qa{secondary = undefined},
				[{secondary, Reason} | Bad_resps], Answered);
	{'EXIT', P_pid, {shutdown, {ok, Roles}}} ->
	    case S_state of
		primary ->
		    ok; %status quo
		_ ->
		     Parent ! {primary, available}
	    end,
	    ecoli_trace(Q_state, "Primary server response with roles: ~p", [Roles]),
	    Parent ! {self(), ok, Roles},
	    check_for_done(Parent, S_state, Q_state#qa{primary = undefined},
				Bad_resps, true);
	{'EXIT', S_pid, {shutdown, {ok, Roles}}} ->
	    case S_state of
		unavailable ->
		     Parent ! {secondary, available};
		_ ->
		    ok
	    end,
	    ecoli_trace(Q_state, "Secondary server response with roles: ~p", [Roles]),
	    Parent ! {self(), ok, Roles},
	    check_for_done(Parent, S_state, Q_state#qa{secondary = undefined},
				Bad_resps, true)
    end.

check_for_done(_, _, #qa{primary = undefined,
			 secondary = undefined} = Q_state, [], true) ->
    ecoli_trace(Q_state, "Query aggregation process completed successfully"),
    dbg("Query exit after good respons, no bad responses"),
    exit(normal);
check_for_done(_, _, #qa{primary = undefined,
			 secondary = undefined} = Q_state, Bad_resps, true) ->
    sysInitI:warning_msg(
	"~p: Ldap query succeeded but some server/s responded: ~s~n",
	[?MODULE, concat_errors(Bad_resps)]),
    dbg("Query exit after good respons, Bad_resps: ~p", [Bad_resps]),
    ecoli_trace(Q_state, "Query aggregation process, bad response/s:~n~p", [Bad_resps]),
    ecoli_trace(Q_state, "Query aggregation process completed successfully"),
    exit(normal);
check_for_done(P, _, #qa{primary = undefined,
			 secondary = undefined} = Q_state, [], false) ->
    dbg("Query exit after NO respons"),
    ecoli_trace(Q_state, "Query aggregation process completed - NO RESPONSE"),
    P ! {self(), error, "no response from ldap"},
    exit(normal);
check_for_done(P, _, #qa{primary = undefined,
			 secondary = undefined} = Q_state, Bad_resps, false) ->
    ecoli_trace(Q_state, "Query aggregation process, bad response/s:~n~p", [Bad_resps]),
    ecoli_trace(Q_state, "Query aggregation process completed UNSUCCESSFULLY"),
    case Bad_resps of
	[{Srv, Reason} = This] -> %just one bad response
	    dbg("Query exit with bad response: ~p", [This]),
	    P ! {self(), error, atom_to_list(Srv) ++ ": " ++ Reason};
        Multiples -> %prefer the one from primary
	    dbg("Query exit with multiple bad responses: ~p", [Multiples]),
	    case lists:keyfind(primary, 1, Multiples) of
		{primary, Reason} ->
		    P ! {self(), error, "primary: " ++ Reason};
		false -> %concatenate and return all of them
		    P ! {self(), error, concat_errors(Multiples)}
	    end
    end,
    exit(normal);
check_for_done(P_pid, S_state, Q_state, Bad_resps, Answered) ->
    query_loop(P_pid, S_state, Q_state, Bad_resps, Answered).

concat_errors([{Srv, Reason}]) ->
    [atom_to_list(Srv) ++ ": " ++ Reason];
concat_errors([{Srv, Reason} | T]) ->
    [atom_to_list(Srv) ++ ": " ++ Reason ++ ", " | concat_errors(T)].

ecoli_trace(Decider, Arg) ->
    ecoli_trace(Decider, Arg, []).
ecoli_trace(#qa{trace = Io}, Arg, Params) when Io =/= undefined ->
    do_trace(Io, Arg, Params);
ecoli_trace(#ldap_params{trace = Io}, Arg, Params) when Io =/= undefined ->
    do_trace(Io, Arg, Params);
ecoli_trace(Io, Arg, Params) when is_pid(Io) ->
    do_trace(Io, Arg, Params);
ecoli_trace(true, Arg, Params) ->
    do_trace(group_leader(), Arg, Params);
ecoli_trace(_, _, _) -> ok.

do_trace(Io, Arg, Params) ->
    io:format(Io, "AA-trace: ~s " ++ Arg ++ "~n", [mk_ds() | Params]).

mk_ds() ->
    {_,_,Micro} = Ts = os:timestamp(),
    {{Y, M, D}, {Hour,Minute,Second}} = calendar:now_to_universal_time(Ts),
    io_lib:format("~4b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b.~6..0b",
    [Y, M, D, Hour, Minute, Second, Micro]).

format_params(P) ->
    io_lib:format(
	"primary ldap server IP: ~p~n"
	"secondary ldap server IP: ~p~n"
	"server port: ~p~n"
	"connect options: ~p~n"
	"DN for binding: ~p~n"
	"bind password (encrypted): ~p~n"
	"base DN: ~p~n"
	"number of ca-certs: ~p~n"
	"node credentials available: ~p~n"
	"TLS mode: ~p~n"
	"profile filter type: ~p~n"
	"flexible {filter, type}: ~p~n"
	"use TBAC: ~p~n"
	"target types: ~p~n"
	"role alias base DN: ~p",
	[P#ldap_params.ldap_primary, P#ldap_params.ldap_secondary,
	 P#ldap_params.server_port, P#ldap_params.extra_inet_opts,
	 P#ldap_params.bind_dn, P#ldap_params.bind_password,
	 P#ldap_params.base_dn, length(P#ldap_params.ca_certs),
	 is_node_cred_ok(P#ldap_params.node_cert, P#ldap_params.node_key),
	 P#ldap_params.tls_mode, P#ldap_params.profile_filter,
	 P#ldap_params.flexible_filter, P#ldap_params.use_tbac,
	 P#ldap_params.target_type, P#ldap_params.role_alias_base_dn]).

% not sure if can assume the Keytype is always 'PrivateKeyInfo'
% {Cert, {'PrivateKeyInfo', Key}};
is_node_cred_ok(Bin_cred, {_KeyType, Bin_key}) when is_binary(Bin_cred),
						    is_binary(Bin_key),
						    size(Bin_cred) > 0,
						    size(Bin_key) > 0 ->
    true;
is_node_cred_ok(_, _) ->
    false.

filter_out_super_user(Roles) ->
    lists:filter(
	fun(?SUPER_ROLE) ->
	    sysInitI:info_msg(
		"~p: Illegal role received from LDAP query.~n"
		"Filtered out the Ericsson maintenance support role: ~p~n",
		[?MODULE, ?SUPER_ROLE]),
	    false;
	   (_)		 ->
	    true
	end, Roles).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
print_ldap_params() ->
    P = get_all_params(),
    %%io:format("duh: ~p", [P]),
    io:format("primary ldap server IP: ~p~n", [P#ldap_params.ldap_primary]),
    io:format("secondary ldap server IP: ~p~n", [P#ldap_params.ldap_secondary]),
    io:format("server port: ~p~n", [P#ldap_params.server_port]),
    io:format("DN for binding: ~p~n", [P#ldap_params.bind_dn]),
    io:format("Bind password (encrypted): ~p~n", [P#ldap_params.bind_password]),
    io:format("Base DN: ~p~n", [P#ldap_params.base_dn]),
    io:format("Number of ca-certs: ~p~n", [length(P#ldap_params.ca_certs)]),
    io:format("P#ldap_params.node_cert length: ~b~n",
	      [size(P#ldap_params.node_cert)]),
    io:format("P#ldap_params.node_key: ~p length: ~b~n",
	      [element(1, P#ldap_params.node_key),
	       size(element(2, P#ldap_params.node_key))]),
    io:format("P#ldap_params.verify_fun(with partial_fun): ~p~n",
	      [P#ldap_params.verify_fun]),
    io:format("Tls mode: ~p~n", [P#ldap_params.tls_mode]),
    io:format("Profile filter type: ~p~n", [P#ldap_params.profile_filter]),
    io:format("Flexible {filter, type}: ~p~n", [P#ldap_params.flexible_filter]),
    io:format("Use TBAC: ~p~n", [P#ldap_params.use_tbac]),
    io:format("Target types: ~p~n", [P#ldap_params.target_type]),
    io:format("Role alias base DN: ~p~n", [P#ldap_params.role_alias_base_dn]).


dbg(_)->ok.
dbg(_, _)->ok.
%dbg(Format) ->
%    dbg(Format, []).
%dbg(Format, Args) ->
%    io:format("dbg: ~p:" ++ Format ++ "~n", [?MODULE | Args]).
