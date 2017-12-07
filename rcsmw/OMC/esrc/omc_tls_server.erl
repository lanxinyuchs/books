%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_tls_server.erl %
%%% @author etomist
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R11A/1

%%% @doc ==Header==
%%% @end

-module(omc_tls_server).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R11A/1').
-date('2017-10-10').
-author('etomist').

-include("omc.hrl").

-define(SERVER_NAME, omc_tls_server).

-record(st, {admin_nconf_unlocked = false,
             admin_cli_unlocked   = false,
             tls_servers = [],	%[#serv{}]
             tls_nconf_options,
             tls_cli_options,
             trustcategory_nconf,
             trustcategory_cli,
             workers = gb_trees:empty(),
             timer,
             retry_listen_tmr,
             bucket = ?BUCKET_SIZE}).

-record(serv, { type,	%tuple() eg: {coli, oam}, {netconf, any}, {cli, lmt}
		port,
		acceptor,
		listen_sock,
		inet_opts}).

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2014-01-20   etxlg     Created
%%% R2A/2      2014-01-20   etxlg     Tried to silence dialyzer
%%% R2A/3      2014-01-20   etxlg     Tried to silence dialyzer more
%%% R2A/4      2014-01-20   etxlg     Fixed above
%%% R2A/5      2014-01-20   etxlg     Rate limitation, etc
%%% R2A/6      2014-01-29   etxlg     ms_user_change, configuration fetched/
%%%				      updated from OOT/CERTM preparations
%%% R2A/7      2014-02-19   etxlg     disable tls 1.1
%%% R2A/8      2014-03-28   etxlg     connect config to sysM and CERT
%%% R2A/9      2014-04-03   etxlg     Don't start if LOCKED
%%% R2A/10     2014-06-16   etxlg     detect change of Ecim attributes
%%% R2A/11     2014-06-24   etxlg     don't crash when loading bad statedata
%%% R2A/13     2014-06-08   etxlg     prepare for coli_tls
%%% R2A/14     2014-08-19   etxlg     fix errors found by dialyzer
%%%					depth -> 10
%%% R2A/15     2014-09-01   etxlg     ensure rcs-coli runs,
%%%				      subscribe CERTS, DSCP
%%% R2A/16     2014-09-11   etxlg     try not to crash at DSCP change during
%%%				      active session, do not print key
%%% R2A/17     2014-09-16   etxlg     bug fix of rate limitation
%%% R2A/18     2014-10-16   etxlg     add handling of cert subscription
%%% R2A/19     2014-10-16   etxlg     fixed above
%%% R2A/20     2014-10-16   etxlg     reset rate-limit at unblock
%%% R2A/21     2014-10-23   etxtory   Remove tls vsn 3
%a bit more is needed until "done":
%different/better check for configdata completeness/consistency
%add cmd for RCS-COLI debug
%%% R3A/1      2015-01-13   etxpeno   Support for roles
%%% R3A/2      2015-03-08   etxlg     Network namespacing
%%% R3A/3      2015-03-12   etxlg     fixed cyclic restart when OOT configured
%%% R3A/4      2015-03-13   erarafo   First-aid fix for dialyzer warning
%%% R3A/5      2015-03-18   etxlg     Match if LMT-ns is in use and run TLS
%%%				      on LMT always (unless impossible)
%%% R4A/1      2015-08-25   etxlg     Removed forgotten io_:_formats
%%% R4A/3      2015-09-01   etxlg     Retry if socket open fails
%%%					TR HU14006
%%% R4A/5      2015-09-28   etxlg     Avoid losing state at change_notify
%%% R4A/6      2015-11-09   etxlg     IPv6
%%% R5A/2      2016-01-12   etxlg     merged IPv6
%%% R5A/4      2016-08-19   ehsake    Worker race condition, HV18193
%%% R6A/2      2016-08-30   emariad   CSUC feature, cipher configuration
%%% R11A/1     2017-10-10   etomist   HW34290

%%-compile([export_all]).
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/0]).
-export([event/2]).
-export([change_notify/0, change_notify/1, close_all/0]).
-export([cert_event/1]).
-export([restart_complete/0]).

%mostly for debug
-export([stop/0]).
-export([info/0]).
-export([start/0]).
-export([tls_options/1]).
%test tls 
-export([ssl_client/4]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%%% gen_server callback API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3, format_status/2]).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc
%%% @end
%%% ----------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, sysEnv:role(), []).

%for debug
start() ->
    gen_server:start({local, ?SERVER_NAME}, ?MODULE, sysEnv:role(), []).

%for debug
stop() ->
    gen_server:call(?SERVER_NAME, stop).

tls_options(Type)->
    gen_server:call(?SERVER_NAME, {tls_options, Type}).

%for debug
info() ->
    gen_server:call(?SERVER_NAME, info).

%called APPM via omc_api
restart_complete() ->
    gen_server:cast(?SERVER_NAME, restart_complete).

event(What, Info) ->
    gen_server:call(?SERVER_NAME, {event, What, Info}).

%this is where CERT call back as a result of subscription
%for now, trigger a general change  that will update everything
cert_event(_What) ->
    change_notify().

%called through fun from oot
change_notify(_) ->
    change_notify().

%called from COMSA through omc_api
change_notify() ->
    gen_server:cast(?SERVER_NAME, change_notify).

%called from COMSA through omc_api
close_all()->
    gen_server:cast(?SERVER_NAME, close_all).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

init(active) ->
    process_flag(trap_exit, true),
    %%FIXME process_flag(sensitive, true),
    case sysInitI:restart_type() of
	local ->
	    %must have crashed, restart everything
	    %register the fun with module to allow module reloading
	    ok =  omc_api:subscribe(oot, fun ?MODULE:change_notify/1),
	    {ok, state_and_start()};
	vm_restart ->
	    %wait for restart_complete
	    {ok, #st{}}
    end;
init(regular) ->
    ignore.


%for debug
handle_call(stop, _From, S) ->
    {stop, {shutdown, "Stopped by command"}, ok, S};
%for debug
handle_call(info, _From, S) ->
    {reply,
	[
	 {admin_nconf_unlocked, S#st.admin_nconf_unlocked},
	 {admin_cli_unlocked, S#st.admin_cli_unlocked},
	 {number_of_sessions, gb_trees:size(S#st.workers)},
    % {workers, S#st.workers},
	 {max_number_of_sessions, ?MAX_SESSIONS},
	 {available_bucket_tokens, S#st.bucket},
	 {bucket_size, ?BUCKET_SIZE},
	 {bucket_refill_interval, ?BUCKET_TIMEOUT},
	 {refill_timer_ref, S#st.timer},
	 {retry_listen_timer_ref, S#st.retry_listen_tmr},
	 {tls_servers, S#st.tls_servers}    
	],
    S};
handle_call({event, accept, {Type, Address}}, From,
			#st{workers = Workers} = S) ->
    %%%This means that a current acceptor of type:Type just became a worker
    %%%set it to undefined so that it will be restarted by maybe_new_acceptor()
    N_state = acceptor_to_state(Type, undefined, S),
    New_worker_pid = element(1, From),

    %% MoShell connects to both CLI and COLI frequently and may cause a race condition
    %% where one acceptor gets killed while there is an outstanding event,accept call.
    %% To avoid ending up with orphan workers in the workers list, a check is added.
    New_workers = 
    case erlang:process_info(New_worker_pid) of
        undefined ->
                sysInitI:info_msg("~p: Worker has terminated. Ignore  ~p",[?MODULE,New_worker_pid]),
                Workers;
        _ ->
         gb_trees:insert(New_worker_pid, {accept, Type, Address},Workers)                    
    end,

    Num_sessions = gb_trees:size(New_workers),
    Last_state =
	maybe_new_acceptor(Num_sessions,
			   N_state#st{workers = New_workers,
					 timer = update_bucket_timer(N_state),
					 bucket = drain_bucket(N_state)}),
    {reply, ok, Last_state};
handle_call({event, login, {Type, Address, User}}, From,
    #st{workers = Workers} = S) ->
    Pid = element(1, From),
    New_workers = gb_trees:update(Pid, {login, Type, Address, User}, Workers),
    [Msg, Count] =
    case element(1, Type) of
        cli ->
            omc_lib:add_session_info(Pid, "TLS", "cli", User, omc_lib:ip_to_string(Address)),
            ["TLS: User: " ++ User ++ ", cli session started", omc_lib:add_session(tls_cli)];
        netconf ->
            omc_lib:add_session_info(Pid, "TLS", "netconf", User, omc_lib:ip_to_string(Address)),
            ["TLS: User: " ++ User ++ ", netconf session started", omc_lib:add_session(tls_netconf)];
        coli ->
            omc_lib:add_session_info(Pid, "TLS", "coli", User, omc_lib:ip_to_string(Address)),
            ["TLS: User: " ++ User ++ ", coli session started", omc_lib:add_session(tls_coli)]
    end,
    omc_lib:sec_log(omc_lib:ip_to_string(Address), Msg, Count),
    {reply, ok, S#st{workers = New_workers}};
handle_call({event, relogin, {Type, Ms_user}}, From,
	#st{workers = Workers} = S) ->
    Pid = element(1, From),
    {value, {login, Type, Address, User}} = gb_trees:lookup(Pid, Workers),
    New_workers = gb_trees:enter(Pid, {login, Type, Address, User, Ms_user},
			Workers),
    {reply, ok, S#st{workers = New_workers}};
handle_call({event, auth_error, {Type, Address, Reason}}, _From, S) ->
    sysInitI:warning_msg("~p: ~p: Source IP: ~p Cause: ~p~n",
	[?MODULE, Type, Address, Reason]),
    {reply, ok, S};
handle_call({event, What, Info}, From, S) ->
    sysInitI:warning_msg(
	"~p: Unhandled call-event, What: ~p, From: ~p, Info: ~p~n",
	[?MODULE, What, From, Info]),
    {reply, ok, S};
handle_call({tls_options, Type}, _From, S) ->
    Options =
    case Type of
        netconf->
            S#st.tls_nconf_options;
        cli->
            S#st.tls_cli_options;
        _ -> 
            []
    end,
    {reply, Options, S};
handle_call(Req, _From, S) ->
    sysInitI:warning_msg("~p: Unhandled call, request: ~p~n",
			     [?MODULE, Req]),
    {reply, ok, S}.

handle_cast(restart_complete, _S) ->
    %register the fun with module to allow module reloading
    ok =  omc_api:subscribe(oot, fun ?MODULE:change_notify/1),
    {noreply, state_and_start()};
handle_cast(change_notify, S) ->
    debug("anything_changed -> all",[]),
    close_all_services(S#st.tls_servers),
    {noreply, state_and_start(S)};
handle_cast(close_all, S)->
    close_all_services(S#st.tls_servers),
    close_all_sessions(gb_trees:keys(S#st.workers)),
    New_state = #st{workers = S#st.workers},
    {noreply, state_and_start(New_state)};
handle_cast(Req, S) ->
    sysInitI:warning_msg("~p: Unhandled cast, request: ~p~n",
			     [?MODULE, Req]),
    {noreply, S}.

handle_info({'EXIT', _Pid, max_sessions}, S) ->
    {noreply, S};
handle_info({'EXIT', _Pid, max_connection_rate}, S) ->
    {noreply, S};
handle_info({'EXIT', _Pid, config_update}, S) ->
    {noreply, S};
handle_info({'EXIT', Acc_or_pid, Reason},
		#st{tls_servers = Servers,
		    workers = Workers} = S) ->
    case lists:keytake(Acc_or_pid, #serv.acceptor, Servers) of
	{value, #serv{type = Type} = Serv, Rest_servers} -> %it's an acceptor
	   sysInitI:warning_report([
		{module, ?MODULE},
		{information, "Acceptor exited"},
		{exit_reason, Reason},
		{typ, Type}]),
	    New_serv = start_acceptor(S, Serv),
	    {noreply, S#st{tls_servers = [New_serv | Rest_servers]}};
	false -> % maybe a worker
	    case gb_trees:lookup(Acc_or_pid, Workers) of
		{value, Found} ->
		    log_logout(Found),
            omc_lib:remove_session_info(Acc_or_pid),
		    New_workers = gb_trees:delete(Acc_or_pid, Workers),
		    Num_sessions = gb_trees:size(New_workers),
		    New_s = S#st{workers = New_workers},
		    {noreply, maybe_new_acceptor(Num_sessions, New_s)};
		none ->
		    sysInitI:warning_report([
			{module, ?MODULE},
			{information, "Unexpected EXIT signal"},
			{pid, Acc_or_pid},
			{exit_reason, Reason}]),
		    {noreply, S}
	    end
    end;
handle_info(fill_bucket, #st{bucket = 0, workers = Workers} = S) ->
    %%%bucket is empty run maybe_new_acceptors since acceptors have been closed
    Num_sessions = gb_trees:size(Workers),
    New_s =  S#st{bucket = fill_bucket(S, Num_sessions),
		timer = run_bucket_timer()},
    {noreply, maybe_new_acceptor(Num_sessions, New_s)};
handle_info(fill_bucket, #st{bucket = Bucket} = S)
			when Bucket < ?BUCKET_SIZE ->
    %%%add to bucket and run timer until full
    {noreply, S#st{bucket = fill_bucket(S),
		timer = run_bucket_timer()}};
handle_info(fill_bucket, S) ->
    %%%bucket became full now - no more timer
    {noreply, S#st{bucket = fill_bucket(S),
		timer = undefined}};
handle_info(retry_listen, S) ->
    %if we are currently in "blocking" state there is a chance of overrun
    %here - no big deal (I hope...)
    Next_s = S#st{retry_listen_tmr = undefined},
    {New_servers, New_state} =
	lists:mapfoldl(
	    fun (#serv{listen_sock = undefined} = Serv, State_acc) ->
		    run_service(State_acc, Serv);
		(Serv, State_acc) ->
		    {Serv, State_acc}
	    end, Next_s, Next_s#st.tls_servers),
    {noreply, New_state#st{tls_servers = New_servers}};
handle_info(Req, S) ->
    sysInitI:warning_msg("~p: Unhandled info, request: ~p~n",
			     [?MODULE, Req]),
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_Old_version, S, _Extra) ->
    {ok, S}.

format_status(_Opt, [_Pdict, S]) ->
     [{data, [{"State", filter_out_key(S)}]}].


ssl_client(Host, Port, Cert, Key)-> 
     Opts = [{certfile, Cert}, {keyfile, Key},
         {reuse_sessions, false}, %needed as we have this in server (BUG!)
         {verify, verify_none},
         {mode, binary},
         {ciphers, comsaI:get_tls_cipher_suites()},
         {server_name_indication, disable}], %% HW34290, MITM protection disabled!

    case ssl:connect(Host, Port, Opts, 1000 * 200) of
        {ok, Sock} ->
            timer:sleep(3000), %sleep 3 seconds
            ok = ssl:send(Sock, <<"exit\n">>),
            ok = ssl:close(Sock),
            {ok, "SSL test client connected and closed successful"};
        {error, Reason} ->
            {error, Reason}
    end.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%called when config change
state_and_start(Old_state) ->
    S = state_and_start(),
    S#st{workers = Old_state#st.workers,
	 timer = Old_state#st.timer,
	 retry_listen_tmr = Old_state#st.retry_listen_tmr,
	 bucket = Old_state#st.bucket}.

%called at start, initial or after crash
state_and_start() ->
    S = load_state_data(),
    {New_servers, New_state} =
	lists:mapfoldl(
	    fun (Serv, State_acc) ->
		run_service(State_acc, Serv)
	    end, S, S#st.tls_servers),
    New_state#st{tls_servers = New_servers}.

%return a tuple list {Type, Port, Acceptor_pid, Listen_sock, Inet_opts}
%ex: {{coli, oam}, 9831, undefined, undefined, [{netns, <<NS-bin>>}, {tos, 2}]}
get_all_servers(S) ->
    get_all_servers(S, ootI:get_oap_namespace(),
		    omc_api:get_lmt_ns_opt_list()).

%no namespacing anywhere, number of servers depend on OamAP and LMT-DHCP
get_all_servers(S, {ok, <<>>}, []) ->
    Oam_dscp_opt = omc_api:get_dscp_opt_list(),
    case omc_api:get_oot_ip_opt_list() of
	[] -> %no OamAccessPoint set, just start one set of servers on 'any'
	    sysInitI:info_msg(
		"~p: no namespace and no OamAccessPoint configured, "
		"preparing ~b listeners.~n",
		[?MODULE, length(active_service(S))]),
	    [#serv{type = {Type, any}, port = omc_api:get_port(tls, Type),
		   inet_opts = Oam_dscp_opt} || Type <- active_service(S)];
	Oam_ip_opt -> %OamAccessPoint is set, run services everywhere
	    Lmt_ll_ip = ootI:get_lmt_ll_ipv4(),
	    Lmt_ll_services =
		[#serv{type = {Type, lmt_ll},
		       port = omc_api:get_port(tls, Type),
		       inet_opts =
			    [{ip, Lmt_ll_ip}]} || Type <- active_service(S)],
	    Oam_services =
		[#serv{type = {Type, oam}, port = omc_api:get_port(tls, Type),
		       inet_opts = Oam_ip_opt ++ Oam_dscp_opt} ||
		 Type <- active_service(S)],
	    Lmt_dhcp_services =
		case ootI:get_lmt_ipv4() of
		    undefined -> %no dhcp address on LMT
			[];
		    Dhcp_tuple when [{ip, Dhcp_tuple}] =/= Oam_ip_opt ->
			[#serv{type = {Type, lmt_dhcp},
			       port = omc_api:get_port(tls, Type),
			       inet_opts = [{ip, Dhcp_tuple}]} ||
			 Type <- active_service(S)];
		    Dhcp_tuple ->
			sysInitI:warning_msg(
			    "~p: DHCP address: ~p on LMT equal to "
			    "OamAccessPoint address -  service disabled~n",
			    [?MODULE, Dhcp_tuple]),
			[]
		end,
	    All_servs = Lmt_ll_services ++ Oam_services ++ Lmt_dhcp_services,
	    sysInitI:info_msg(
		"~p: no namespace configured, OamAccessPoint set, "
		"preparing ~b listeners.~n", [?MODULE, length(All_servs)]),
	    All_servs
    end;
%either or both TN and LMT are namespaced, LMT will open not bind (open any)
get_all_servers(S, {ok, Oam_ns_bin}, Lmt_ns_opt) ->
    sysInitI:info_msg(
	"~p: namespace configured, preparing ~b listeners.~n",
	[?MODULE, 2 * length(active_service(S))]),
    Oam_ns_opt = omc_api:ns_to_opt_list(Oam_ns_bin),
    Oam_ip_opt = omc_api:get_oot_ip_opt_list(),
    Oam_dscp_opt = omc_api:get_dscp_opt_list(),
    [#serv{type = {Type, oam}, port = omc_api:get_port(tls, Type),
	   inet_opts = Oam_ns_opt ++ Oam_ip_opt ++ Oam_dscp_opt} ||
		Type <- active_service(S)] ++
    [#serv{type = {Type, lmt}, port = omc_api:get_port(tls, Type),
	   inet_opts = Lmt_ns_opt} || Type <- active_service(S)];
get_all_servers(S, {error, _}, []) ->
%LMT not namespaced, there may be namespacing on Oam, we don't know yet,
%start without any OOT stuff and reconfigure later on, i.e. do NOT use the
%OamAccessPoint IP it may not be configured yet.
    sysInitI:info_msg(
	"~p: namespace may be pending, preparing ~b listeners.~n",
	[?MODULE, length(active_service(S))]),
    Oam_dscp_opt = omc_api:get_dscp_opt_list(),
    [#serv{type = {Type, any}, port = omc_api:get_port(tls, Type),
	   inet_opts = Oam_dscp_opt} || Type <- active_service(S)];
get_all_servers(S, {error, _}, Lmt_ns_opt) ->
%LMT is namespaced, there may be namespacing on Oam, we don't know yet,
%start without any OOT stuff and reconfigure later on, i.e. do NOT use the
%OamAccessPoint IP it may not be configured yet.
    sysInitI:info_msg(
	"~p: namespace may be pending, preparing ~b listeners.~n",
	[?MODULE, 2 * length(active_service(S))]),
    Oam_dscp_opt = omc_api:get_dscp_opt_list(),
    [#serv{type = {Type, oam}, port = omc_api:get_port(tls, Type),
	   inet_opts = Oam_dscp_opt} || Type <- active_service(S)] ++
    [#serv{type = {Type, lmt}, port = omc_api:get_port(tls, Type),
	   inet_opts = Lmt_ns_opt} || Type <- active_service(S)].


active_service(S) when	S#st.admin_nconf_unlocked == false,
			S#st.admin_cli_unlocked == false ->
    [];
active_service(S) when	S#st.admin_nconf_unlocked == true,
			S#st.admin_cli_unlocked == false ->
    [netconf];
active_service(S) when	S#st.admin_nconf_unlocked == false,
			S#st.admin_cli_unlocked == true ->
    [cli, coli];
active_service(S) when	S#st.admin_nconf_unlocked == true,
			S#st.admin_cli_unlocked == true ->
    [cli, netconf, coli].

filter_out_key(#st{tls_nconf_options = Tls_nconf_opts,
        tls_cli_options = Tls_cli_opts} = S) ->
    Filtered_tls_nconf_opts =
	lists:foldl(
	    fun({key, _}, Acc) ->
		    [{key, <<"filtered_out">>} | Acc];
		(Opt, Acc) ->
		    [Opt | Acc]
	    end, [], Tls_nconf_opts),
    Filtered_tls_cli_opts =
	lists:foldl(
	    fun({key, _}, Acc) ->
		    [{key, <<"filtered_out">>} | Acc];
		(Opt, Acc) ->
		    [Opt | Acc]
	    end, [], Tls_cli_opts),
    S#st{tls_nconf_options = lists:reverse(Filtered_tls_nconf_opts),
        tls_cli_options = lists:reverse(Filtered_tls_cli_opts)}.

load_state_data() ->
    Nc_props  = comsaI:get_netconf_tls_config(),
    Cli_props = comsaI:get_cli_tls_config(),

    {Node_nconf_cert, Node_nconf_key, Node_nconf_chain} =
    case omc_lib:fetch_node_cert([{cert_subscribe, ?MODULE} | Nc_props]) of
        {[NodeNconfCert|NodeNconfChain], NodeNconfKey} ->
            {NodeNconfCert, NodeNconfKey, NodeNconfChain};
        {NodeNconfCert, NodeNconfKey} -> % Remove when cert released
            {NodeNconfCert, NodeNconfKey, []}
    end,

    {Node_cli_cert, Node_cli_key, Node_cli_chain} =
    case omc_lib:fetch_node_cert([{cert_subscribe, ?MODULE} | Cli_props]) of
        {[NodeCliCert|NodeCliChain], NodeCliKey} ->
            {NodeCliCert, NodeCliKey, NodeCliChain};
        {NodeCliCert, NodeCliKey} -> % Remove when cert released
            {NodeCliCert, NodeCliKey, []}
    end,

    Ca_nconf_certs =
	omc_lib:fetch_ca_certs([{cert_subscribe, ?MODULE} | Nc_props]) ++
        Node_nconf_chain,
    Tcat_nconf     = proplists:get_value(trustCategory, Nc_props, undefined),
    Ca_cli_certs   =
	omc_lib:fetch_ca_certs([{cert_subscribe, ?MODULE} | Cli_props]) ++
        Node_cli_chain,
    Tcat_cli       = proplists:get_value(trustCategory, Cli_props, undefined),

    Admin_nconf =
	case proplists:get_value(administrativeState, Nc_props, 'LOCKED') of
	    'LOCKED' -> false;
	    'UNLOCKED' -> true
	end,
    Admin_cli =
	case proplists:get_value(administrativeState, Cli_props, 'LOCKED') of
	    'LOCKED' -> false;
	    'UNLOCKED' -> true
	end,

    Common_ssl_options = 
    [{ciphers, comsaI:get_tls_cipher_suites()},
    {secure_renegotiate, true},
    {versions, [tlsv1, 'tlsv1.2']},
    {depth, 10},
    {verify, verify_peer}, {fail_if_no_peer_cert, true}],

    Tls_nconf_options =
    [{cert, Node_nconf_cert}, {key, Node_nconf_key},
     {cacerts, Ca_nconf_certs}],

    Tls_cli_options =
    [{cert, Node_cli_cert}, {key, Node_cli_key}, 
    {cacerts, Ca_cli_certs}], 

    S = #st{admin_nconf_unlocked = Admin_nconf,
        admin_cli_unlocked   = Admin_cli,
    trustcategory_nconf = Tcat_nconf,
    trustcategory_cli   = Tcat_cli,
    tls_nconf_options   = Tls_nconf_options ++ Common_ssl_options,
        tls_cli_options     = Tls_cli_options ++ Common_ssl_options},
    S#st{tls_servers = get_all_servers(S)}.

%return new #st{}
maybe_new_acceptor(N_sessions,  S) when N_sessions >= ?MAX_SESSIONS ->
    %%% reached maximum awailable sessions - exit all accepting processes
    blocking_info("Maximum number of sessions (" ++
		  integer_to_list(?MAX_SESSIONS) ++ ") reached"),
    omc_lib:sec_log("-", "TLS: Maximum(" ++ integer_to_list(?MAX_SESSIONS) ++
		    ") number of simultaneous sessions reached"),
    exit_all_acceptors(S, max_sessions);
maybe_new_acceptor(_,  #st{bucket = 0} = S) ->
    %%% no more tokens in the bucket - exit all accepting processes
    blocking_info("High connection rate, limiting"),
    omc_lib:sec_log("-", "TLS: High connection rate - rate limit in effect"),
    exit_all_acceptors(S, max_connection_rate);
maybe_new_acceptor(_, #st{tls_servers = Servers} = S) ->
    New_servers =
	[case Serv of
	    #serv{listen_sock = undefined} ->
	    Serv;
	    #serv{acceptor = undefined} ->
		start_acceptor(S, Serv);
	    _ ->
		Serv
	end || Serv <- Servers],
    S#st{tls_servers = New_servers}.

exit_all_acceptors(#st{tls_servers = Servers} = S, Reason) ->
    Closed_servers =
	[begin
	    exit_acceptor(Pid, Reason),
	    Serv#serv{acceptor = undefined}
	end || #serv{acceptor = Pid} = Serv <- Servers],
    S#st{tls_servers = Closed_servers}.

exit_acceptor(undefined, _Reason) -> ok;
exit_acceptor(Pid, Reason)->
    exit(Pid, Reason).

%%Close all sessions
close_all_sessions([Pid | T])->
    Pid ! {close_session, ""},
    close_all_sessions(T);
close_all_sessions([])-> ok.

%config_update
close_all_services(Services) ->
    [begin
	exit_acceptor(Serv#serv.acceptor, config_update),
	close_listen_sock(Serv#serv.listen_sock)
	%%, Serv#serv{acceptor = undefined, listen_sock = undefined}
    end || Serv <- Services],
    ok.

close_listen_sock(undefined) -> ok;
close_listen_sock(Sock) ->
    gen_tcp:close(Sock).

open_listen_sock(#serv{port = Port, inet_opts = Inet_opts} = Serv) ->
    Options = [{active, false}, {mode, binary}, {reuseaddr, true},
	       {linger, {false, 0}}] ++ Inet_opts,
    case gen_tcp:listen(Port, Options) of
	{ok, Lsock} ->
	    Serv#serv{listen_sock = Lsock};
	{error, Why} ->
	    sysInitI:warning_msg(
		"~w: Failed to create listener for service: ~p, reason: ~p~n",
		[?MODULE, Serv#serv.type, Why]),
	    Serv#serv{listen_sock = undefined}
    end.

run_service(S, Serv) ->
    case open_listen_sock(Serv) of
	New_serv when New_serv#serv.listen_sock =:= undefined ->
	    {New_serv, maybe_retry_timer(S)};
	New_serv ->
	    {start_acceptor(S, New_serv), S}
    end.

maybe_retry_timer(#st{retry_listen_tmr =  undefined} = S) ->
    %no timer running start one
    sysInitI:warning_msg(
	"~w: Listener creation for TLS service will be retried in "
	"10 seconds.~n",
	[?MODULE]),
    Tmr = erlang:send_after(10 * 1000, self(), retry_listen),
    S#st{retry_listen_tmr = Tmr};
maybe_retry_timer(S) ->
    %running already - do not run more than one
    S.

%start_acceptor(#st{}, #serv{}) -> Updated#serv{}.
start_acceptor(S, #serv{type = Type} = Serv) ->
    Instance =
	case Type of
	    {coli, _}	-> omc_tls_ecoli_proxy;
	    _		-> omc_tls_instance
	end,
    case Type of
        {netconf,_} ->
            Acceptor =
            proc_lib:spawn_link(Instance, start,
                [Type, self(), Serv#serv.listen_sock,
                    S#st.trustcategory_nconf, S#st.tls_nconf_options]),
            Serv#serv{acceptor = Acceptor};
        _ ->
            Acceptor =
            proc_lib:spawn_link(Instance, start,
                [Type, self(), Serv#serv.listen_sock,
                    S#st.trustcategory_cli, S#st.tls_cli_options]),
            Serv#serv{acceptor = Acceptor}
    end.

log_logout({accept, {Type, _}, Address}) ->
    Msg = "TLS: " ++ atom_to_list(Type) ++ ": Failed to start session",
    omc_lib:sec_log(omc_lib:ip_to_string(Address), Msg);
log_logout({login, {cli, _}, Address, User}) ->
    C = omc_lib:remove_session(tls_cli),
    Msg = "TLS: User: " ++ User ++ ", cli session ended",
    omc_lib:sec_log(omc_lib:ip_to_string(Address), Msg, C);
log_logout({login, {cli, _}, Address, User, Ms_user}) ->
    C = omc_lib:remove_session(tls_cli),
    Msg = "TLS: User: " ++ User ++ ":" ++ Ms_user ++ ", cli session ended",
    omc_lib:sec_log(omc_lib:ip_to_string(Address), Msg, C);
log_logout({login, {netconf, _}, Address, User}) ->
    C = omc_lib:remove_session(tls_netconf),
    Msg = "TLS: User: " ++ User ++ ", netconf session ended",
    omc_lib:sec_log(omc_lib:ip_to_string(Address), Msg, C);
log_logout({login, {netconf, _}, Address, User, Ms_user}) ->
    C = omc_lib:remove_session(tls_netconf),
    Msg = "TLS: User: " ++ User ++ ":" ++ Ms_user ++ ", netconf session ended",
    omc_lib:sec_log(omc_lib:ip_to_string(Address), Msg, C);
log_logout({login, {coli, _}, Address, User}) ->
    C = omc_lib:remove_session(tls_coli),
    Msg = "TLS: User: " ++ User ++ ", coli session ended",
    omc_lib:sec_log(omc_lib:ip_to_string(Address), Msg, C).

drain_bucket(#st{bucket = Bucket}) when Bucket > 0 ->
    Bucket - 1;
drain_bucket(_) -> 0.

fill_bucket(S) ->
    fill_bucket(S, gb_trees:size(S#st.workers)).

fill_bucket(#st{bucket = 0}, ?MAX_SESSIONS) ->
    blocking_info("Would unblock but maximum sessions reached"),
    1;
fill_bucket(#st{bucket = 0}, _) ->
    blocking_info("Unblocking after high connectionrate"),
    1;
fill_bucket(#st{bucket = Bucket}, ?MAX_SESSIONS) when
				Bucket =:= (?BUCKET_SIZE - 2) ->
    blocking_info("Connectionratelimit: normal, maximum sessions reached"),
    Bucket + 1;
fill_bucket(#st{bucket = Bucket}, _) when Bucket =:= (?BUCKET_SIZE - 2) ->
    blocking_info("Connectionratelimit: normal"),
    Bucket + 1;
fill_bucket(#st{bucket = Bucket}, _) when Bucket < ?BUCKET_SIZE ->
    Bucket + 1;
fill_bucket(_, _) ->
    ?BUCKET_SIZE.

update_bucket_timer(#st{timer = undefined}) ->
    run_bucket_timer();
update_bucket_timer(#st{timer = Timer}) ->
    Timer.

run_bucket_timer() ->
    erlang:send_after(?BUCKET_TIMEOUT, self(), fill_bucket).

acceptor_to_state(Type, Acceptor, #st{tls_servers = Servers} = S) ->
    {value, Serv, Rest} =  lists:keytake(Type, #serv.type, Servers),
    New_servers = [Serv#serv{acceptor = Acceptor} | Rest],
    S#st{tls_servers = New_servers}.

blocking_info(Format) ->
    sysInitI:info_msg("~p:" ++ Format ++ "~n", [?MODULE]).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

debug(_, _) -> ok.
%debug(Format, Params) ->
%    io:format("dbg ~p:" ++ Format ++ "~n", [?MODULE | Params]).
