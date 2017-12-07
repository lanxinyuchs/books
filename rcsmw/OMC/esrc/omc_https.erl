%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_https.erl %
%%% @author etomist
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R5A/R6A/R9A/R10A/R11A/2

%%% @doc == https handling ==
%%% @end
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(omc_https).
-behaviour(gen_server).
-vsn('/main/R3A/R4A/R5A/R6A/R9A/R10A/R11A/2').
-date('2017-10-11').
-author('etomist').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% Rev     Date     Name      What
%%% -----   -------  --------  ------------------------
%%% R3A/1   150114   etxtory   First try
%%% R3A/2   150120   etxtory   Added "flag-file" handling for EMCLI
%%% R3A/4   150122   etxtory   Handling timeout from certSecStore
%%% R3A/5   150126   etxtory   Dialyser fix; change port-no; hide key
%%% R3A/6   150208   etxtory   NC for https-port (443)
%%% R3A/7   150208   etxtory   keep_alive_timeout in again
%%% R3A/8   150212   etxlg     When the cat is away, the mice dance
%%% R3A/9   150216   etxlg     bug fale -> false
%%% R3A/10  150216   etxlg     printout when servers are changed
%%% R3A/11  150216   etxlg     filter out duplicate servers
%%% R3A/12  150216   etxtory   Use type of key from pem_decode
%%% R3A/13  150217   etxlg     Make it work in sim
%%% R3A/16  150312   etxtory   Corrected NC handling
%%% R3A/18  150317   etxtory   versions introduced
%%% R3A/19  150319   etxtory   Added chain to https
%%% R3A/20  150331   etxlg     Fix for namespace on LMT without OamAP
%%% R3A/21  150408   etxtory   Changed sec-proto handling
%%% R3A/22  150424   etxtory   HT64288: disable webserver grabbing
%%% ----------------------------------------------------------
%%% R4A/3   150603   etxtory   Merge R3A
%%% R4A/4   150819   etxtory   HT74937 (Nessus) Increased no of clients
%%% R4A/5   150903   etxasta   Added web sec
%%% R4A/7   150903   etxlg     Alternate connection
%%% R4A/8   151021   etxlg     Alternate connection - revisited
%%%			       also, tried using profile, need OTP fix.
%%% R4A/10  151110   etxlg     IPv6
%%% R4A/11  151128   etxlg     Fixed above to work in SIM
%%% R4A/14  160401   eolaand   HU70271, retry if IPv4 address is not available.
%%%                            Same as for IPv6.
%%% ----------------------------------------------------------
%%% R5A/2    160113   etxlg     Merged IPv6 from R4
%%% R5A/3    160202   etxlg     Restart service if it fails to start
%%% R5A/4    160307   etxlg     IPv6 address not immidiately available
%%% R5A/5    160309   etxlg     Above should NOT have been error print
%%% R5A/6    160401   eolaand   Merge HU70271 from R4
%%% R5A/7    160520   etxpeno   HU84962 do not use IPv4-only data in the oot notification
%%% ----------------------------------------------------------
%%% R6A/1    160704   emariad   Changed max allowed clients from 20 -> 100
%%% R6A/2    160830   emariad   CSUC feature, cipher configuration
%%% R6A/3-4  160907   uabesvi   vRC should be treated as target and not simulated
%%% ----------------------------------------------------------
%%%          160915   etxaldu   Add vnfcs erl_script_alias for vrcs
%%%                            (git 5G)
%%% R9A/1    170126   etxberb   Backported git change to ClearCase-G2.
%%% R9A/2    170222   etxtory   Listen to any for vrcs on no namespace (tenant)
%%% R9A/3    170324   etxasta   Removed start of EM-GUI for VRCS
%%% R9A/4    170404   etxasta   EA for 5G is not using VC anymore
%%% R10A/1   170412   etxasta   EA should use port 3443 in 5G/VNF
%%% R10A/3   170502   etxasta   EA should use port 3443 only for R-VNFM
%%% R10A/4   170522   etxasta   Added get_cert_conf/0
%%% R10A/5-6 170523   etxasta   Added report of changes for R-VNFM
%%% R10A/7   170524   etxasta   Correction report of changes for R-VNFM
%%% R10A/8   170530   emariad   Added clause do_get_pre_cred for RVNFM
%%% R10A/9   170602   etxasta   Added EA fix for R-VNFM
%%% R11A/1   170908   ebabmat   Handle for fix on CERT for HW12922 and HW21881
%%% R11A/2   171011   etomist   OTP20 - disabling MITM protection
%%% ----------------------------------------------------------

%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/0]).
-export([restart_complete/0]).
-export([change_notify/0]).
-export([oot_notify/1]).
-export([cert_event/1]).
-export([tls_cipher_change_notify/0]).

%% Used by CERT
-export([get_cert_conf/0]).

%% Test function(s)
-export([update/0]).
-export([stop_services/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

%% omc_https server internal server state
-record(state,
	{
	  restart_complete = false,
	  services = []
	}).

-define(IP_ADDRESS_DEF,  "169.254.2.2").
-define(REINITTIMEOUT, 1000 * 60 * 2). %2 min, arbitrary but same as omc_server
-define(SHORT_REINITTIMEOUT, 1000 * 2). %shorter, usually fixes itself quick

%% Include(s)
-include("RcsHttpM.hrl").

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Starts the omc_https process
%%% @end
%%% ----------------------------------------------------------
start_link() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

%%% ----------------------------------------------------------
%%% @doc Called after complete startup by APPM (via omc_api)
%%% @end
%%% ----------------------------------------------------------
restart_complete() ->
    gen_server:cast(?MODULE, restart_complete).

%%% ----------------------------------------------------------
%%% @doc Called when class cli_ssh has been changed
%%% (via omc_api). If administrative state is changed the
%%% EMCLI "flag-file" is updated.
%%% @end
%%% ----------------------------------------------------------
change_notify() ->
    gen_server:cast(?MODULE, change_notify).

%%% ----------------------------------------------------------
%%% @doc Called through fun registered in the ootServer
%%% informs about dscp, net-name-space, ip-addresses (lmt and tn)
%%% @end
%%% ----------------------------------------------------------
oot_notify(Prop_list) ->
    gen_server:cast(?MODULE, {oot_notify, Prop_list}).

%%% ----------------------------------------------------------
%%% @doc CERT call back as a result of subscription.
%%% @end
%%% ----------------------------------------------------------
cert_event(_Data) ->
    gen_server:cast(?MODULE, cert_event).

%%% ----------------------------------------------------------
%%% @doc Called when tls cipher configuration has been changed
%%% (via omc_api). 
%%% @end
%%% ----------------------------------------------------------
tls_cipher_change_notify() ->
    gen_server:cast(?MODULE, tls_cipher_change_notify).

%%% ----------------------------------------------------------
%%% @doc Test function
%%% Updates all http(s) servers. Can be used to re-read
%%% the VC from /rcs/cert/vc.
%%% @end
%%% ----------------------------------------------------------
update() ->
    do_update().

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% gen_server callbacks
%%% ----------------------------------------------------------
init(_Args) ->
    mnesia:subscribe({table, https, detailed}),
    %% defining the fun like this means that it survives reload
    %% of defining module
    ootI:register_cfg_upd_cb(fun ?MODULE:oot_notify/1),
    case sysInitI:restart_type() of
	local ->
	    info_msg("Local restart of omc_https server ~n", []),
	    stop_services(),
	    restart_complete(),
	    {ok, #state{restart_complete = true}};
	Other ->
	    info_msg("~p restart of omc_https server ~n", [Other]),
	    {ok, #state{}}
    end.

%% Test function
handle_call(clear_state, _From, _State) ->
    {reply, ok, #state{}};
handle_call(_Request, _From, State) ->
    {reply, undefined, State}.

handle_cast(restart_complete, State) ->
    %% Needs to call do_change_notify to handle backup restore
    do_change_notify(),
    NewState = start_services(State#state{restart_complete = true}),
    {noreply, NewState};
handle_cast(change_notify, State) ->
    do_change_notify(),
    {noreply, State};
handle_cast({oot_notify, Prop_list}, State) ->
    %% Check if there are any props we care about
    case lists:any(
	    fun ({lmt_ipv4, _}) ->		   true;
		({oap_namespace, _}) ->		   true;
		({oap_alt_namespace, _}) ->	   true;
		({dscp, _}) ->			   true;
		({access_point_address, _}) ->	   true;
		({access_point_address_alt, _}) -> true;
		(_) ->				   false
	    end, Prop_list) of
 	true ->
	    NewState = start_services(State),
	    {noreply, NewState};
	false ->
	    {noreply, State}
    end;
handle_cast(cert_event, State) ->
    case State#state.restart_complete of
	false ->
	    %% CERT events before restart_complete.
	    %% Just ignore and let restart_complete handle this.
	    %% Should not happen.
	   {noreply, State};
	true ->
	    %% CERT events after restart_complete.
	    %% Stop all httpd services in inets (except http 8080)
	    %% and then start the services again
	    stop_services(),
	    NewState = start_services(State#state{services = []}),
	    {noreply, NewState}
    end;
handle_cast(tls_cipher_change_notify, State) ->
    case State#state.restart_complete of
          false ->
              %% Tls cipher configuration change before restart_complete.
              %% Just ignore and let restart_complete handle this.
              {noreply, State};
          true ->
              stop_services(),
              NewState = start_services(State#state{services = []}),
              {noreply, NewState}   
      end;
handle_cast(Msg, State) ->
    error_msg("Received unknown message ~p~n", [Msg]),
    {noreply, State}.

handle_info({mnesia_table_event, {write, https, New, Old,_}}, State) ->
    case swmI:node_type() of
        "R-VNFM" ->
            %% Report https changes for secServer in R-VNFM
            NcDN   = New#https.nodeCredential,
            TCatDN = New#https.trustCategory,
            Res =
            case Old of
                [] ->
                    {https, NcDN, TCatDN, undefined, undefined};
                [Obj] ->
                    OldNcDN   = Obj#https.nodeCredential,
                    OldTCatDN = Obj#https.trustCategory,
                    {https, NcDN, TCatDN, OldNcDN, OldTCatDN}
            end,
            apply(secServer, update_status, [Res]);
        _ ->
            ok
    end,
    case State#state.restart_complete of
	false ->
	    %% Mnesia updates before restart_complete.
	    %% Just ignore and let restart_complete handle this
	   {noreply, State};
	true ->
	    %% Mnesia updates after restart_complete.
	    %% Stop all httpd services in inets (except http 8080)
	    %% and then start the services again
	    stop_services(),
	    NewState = start_services(State#state{services = []}),
	    {noreply, NewState}
    end;
handle_info(reinit_timeout, State) ->
    restart_complete(),
    flush_duplicate_reinit_timeout(),
    {noreply, State};
handle_info(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% Start https services
%%% - Target port 8443 for EMGUI
%%%   Uses NC/TC. Do not start service (8443) if NC/TC
%%%   does not exist.
%%%
%%% - Target port 3443(R-VNFM) else 443 for ea.html & files (EMCLI)
%%%   Use NC/TC. Use VC if NC/TC is not configured.
%%%
%%%   Note:
%%%   VC contains Cert, CertKey and trusted chain.
%%%   NC contains Cert and CertKey; TC contains trusted chain
%%% ----------------------------------------------------------
start_services(State) ->
    {ToStop, ToStart, Unchanged} = sort_services(State),
    print_server_change(ToStop, ToStart, Unchanged),
    %% ToStart includes those that will be restarted, ensure they are stopped
    [stop_pid(P) || {P, _} <- ToStart ++ ToStop, P =/= undefined],
    {ReNewStart, AllStarted} =
	lists:foldl(
	  fun(E, {ServList, Status}) ->
		  {Serv, OkNok} = start_service(E),
		  {[Serv | ServList], Status and OkNok}
	  end, {[], true}, ToStart),
    State#state{services = ReNewStart ++ Unchanged,
		restart_complete = AllStarted}.

start_service({_, {https_login, _Name, _Ip, _Port, _Ns, _Dscp} = ServiceId}) ->
    case get_certs() of
	undefined ->
	    %% No NC or TC, do not start yet
            {{undefined, ServiceId}, true};
	{NcCert, NcCertKey, TcCerts} ->
	    %% NC/TC
	    %% https: start https_login using NC/TC
	    start_https(ServiceId, {NcCert, NcCertKey, TcCerts})
    end;
start_service({_, {https, _Name, _Ip, _Port, _Ns, _Dscp} = ServiceId}) ->
    case get_certs() of
	undefined ->
	    %% No NC or TC
	    %% https: start https using VC
	    start_https(ServiceId, undefined);
	{NcCert, NcCertKey, TcCerts} ->
	    %% NC/TC
	    %% https: start https using NC/TC
	    start_https(ServiceId, {NcCert, NcCertKey, TcCerts})
    end.

%% return: {ToStop, ToStart, Unchanged}
sort_services(State) ->
    All_servers =
    get_services(sysEnv:target(), sysEnv:vrcs(), swmI:node_type()),
    All_servers_filtered = filter_servers(All_servers),
    sort_services(State#state.services, All_servers_filtered).

sort_services([], Wanted_services) ->
    {[], Wanted_services, []};
sort_services(Current_services, []) ->
    {Current_services, [], []}; % can this happen?
sort_services(Current_services, Wanted_services) ->
    sort_services(Current_services, Wanted_services, {[], [], []}).

sort_services([], [], {_Stop, _Start, _Unchanged} = Result) ->
    Result;
sort_services([], Ws, {Stop, Start, Unchanged}) ->
    {Stop, Ws ++ Start, Unchanged};
sort_services(Cs, [], {Stop, Start, Unchanged}) ->
    {Cs ++ Stop, Start, Unchanged};
sort_services([C | Cs], Ws, {Stop, Start, Unchanged}) ->
    case check_service(C, Ws, []) of
	{stop, Serv, NWs} ->
	    sort_services(Cs, NWs, {[Serv | Stop], Start, Unchanged});
	{restart, Serv, NWs} ->
	    sort_services(Cs, NWs, {Stop, [Serv | Start], Unchanged});
	{keep, Serv, NWs} ->
	    sort_services(Cs, NWs, {Stop, Start, [Serv | Unchanged]})
    end.

%% Comparing is done based on IP and Port
check_service(Service, [], NewWsAcc) ->
    {stop, Service, NewWsAcc};
check_service({Pid, Cservice}, [{undefined, Wservice} | Ws], NewWsAcc) ->
    case {Cservice, Wservice} of
	{Match, Match} when Pid =:= undefined ->
	    {restart, {undefined, Cservice}, Ws ++ NewWsAcc};
	{Match, Match}  ->
	    {keep, {Pid, Cservice}, Ws ++ NewWsAcc};
	{{_, _, Ip, Port, _, _}, {_, _, Ip, Port, _, _}} ->
	    {restart, {Pid, Wservice}, Ws ++ NewWsAcc};
	_ ->
	    check_service({Pid, Cservice}, Ws,
			  [{undefined, Wservice} | NewWsAcc])
    end.

%% it happens in testcases that OamAP points to the DHCP-address, this causes
%% an error printout. This will ensure that all servers are unique based on
%% IP,  Port, and Namespace(not yet OTP:httpd:child_name2info/4 needs fixing
filter_servers(All_servers) ->
    {Unique, _} =
	lists:foldl(
	    fun({_, Sid} = Elem, {Out, Found}) ->
		Id = {sid_to_port(Sid),
		      sid_to_bindaddress(Sid)}, %, sid_to_profile_opt(Sid)},
		case lists:member(Id, Found) of
		    true ->
			warn_msg("Duplicate service discarded: ~p~n", [Id]),
			{Out, Found};
		    false ->
			{[Elem | Out], [Id | Found]}
		end
	    end, {[], []}, All_servers),
    Unique.

start_https(ServiceId, undefined) ->
    %% No NC/TC exist; use VC for G2 and internal cert for 5G;
    %% Only used for web, not web sec
    case get_pre_cred() of
	{ok, {VcCert, VcKey, Chain}} ->
	    info_msg("Starting https service using pre cred~n", []),
	    Pid = do_start_https(ServiceId, {VcCert, VcKey, Chain}),
	    {{Pid, ServiceId}, _RC = true};
	{error, try_again} ->
	    info_msg("Couldn't get pre cred; trying again~n", []),
	    timer:apply_after(30000, ?MODULE, restart_complete, []),
	    {{undefined, ServiceId}, _RC = false};
	{error, Reason} ->
	    info_msg("Missing pre cred ~p; https service not started~n", [Reason]),
	    {{undefined, ServiceId}, _RC = true}
    end;
start_https(ServiceId, {NcCert, NcCertKey, TcCerts}) ->
    %% NC/TC exist
    info_msg("Starting https service using NC/TC~n", []),
    Pid = do_start_https(ServiceId, {NcCert, NcCertKey, TcCerts}),
    {{Pid, ServiceId}, _RC = true}.

do_start_https({https, _Name, _Ip, _Port, _Ns, _Dscp} = ServiceId,
    {Cert, CertKey, _Chain}) ->
    Props = [{port, sid_to_port(ServiceId)},
	     {server_name, sid_to_name(ServiceId)},
	     {bind_address, sid_to_bindaddress(ServiceId)},
	     {socket_type, {essl, [{cert, Cert},
                               {key, CertKey},
                               {cacerts, []},
                               {depth, 10}, % default is 1
                               {ciphers, comsaI:get_tls_cipher_suites()},
                               {log_alert, false},
                               {versions, [tlsv1, 'tlsv1.2']},
                               {server_name_indication, disable}]
                            ++ sid_to_sock_opts(ServiceId)
                       }}], % ++ sid_to_profile_opt(ServiceId),
    CProps =
       [{server_root, sysEnv:www_server_root()},
        {document_root, sysEnv:www_doc_root()},
        {modules,[mod_alias,mod_auth,mod_esi,mod_actions,mod_cgi,
                omc_mod_dir,mod_get,mod_head,mod_log,mod_disk_log]},
        {log_format, combined},
        {error_log_format, pretty},
        {disk_log_format, external},
        {error_disk_log, "errorLog"},
        {error_disk_log_size, {10000, 3}},
        {security_disk_log, "securityLog"},
        {security_disk_log_size, {10000, 3}},
        {transfer_disk_log, "transferLog"},
        {transfer_disk_log_size, {10000, 3}},
        {directory_index,["index.html", "index.htm"]},
        {erl_script_alias, {"/cgi-bin", [aicGui]}},
        {mime_types,[{"html", "text/html"}, {"htm", "text/html"},
                {"css", "text/css"}, {"svg", "image/svg+xml"}]},
        {server_tokens, none},
        {customize, omc_httpd_custom},
        {keep_alive_timeout, 150},
        {minimum_bytes_per_second, 100},
        {max_body_size, 10000},
        {max_clients, 100},
        {max_header_size, 10240},
        {max_uri_size, 500},
        {max_keep_alive_request, 1000},
	{ipfamily, sid_to_ipfamily(ServiceId)}],
    safer_start(https, ServiceId, Props ++ CProps);
do_start_https({https_login, _Name, _Ip, _Port, _Ns, _Dscp} = ServiceId,
    {Cert, CertKey, Chain}) ->
    [Obj] = mnesia:dirty_read(https, {"1","1","1","1","1"}),
    Vfun =
    omc_lib:fetch_verify_fun("WEB SEC HTTPS",
        [{trustCategory, Obj#https.trustCategory}], {client, undefined}),
    Props = [{port, sid_to_port(ServiceId)},
	     {server_name, sid_to_name(ServiceId)},
	     {bind_address, sid_to_bindaddress(ServiceId)},
             {socket_type, {essl, [{verify, verify_peer},
                   {fail_if_no_peer_cert, true},
                   {cert, Cert},
				   {key, CertKey},
				   {cacerts, Chain},
                   {depth, 10}, % default is 1
                   {ciphers, comsaI:get_tls_cipher_suites()},
				   {log_alert, false},
				   {versions, [tlsv1, 'tlsv1.2']}] ++
                               Vfun ++ sid_to_sock_opts(ServiceId)}}],
    CProps =
       [{server_root, sysEnv:www_sec_server_root()},
        {document_root, sysEnv:www_sec_doc_root()},
        {modules,[mod_alias,mod_auth,mod_esi,mod_actions,mod_cgi,
                omc_mod_dir,mod_get,mod_head,mod_log,mod_disk_log]},
        {log_format, combined},
        {error_log_format, pretty},
        {disk_log_format, external},
        {error_disk_log, "errorLog"},
        {error_disk_log_size, {10000, 3}},
        {security_disk_log, "securityLog"},
        {security_disk_log_size, {10000, 3}},
        {transfer_disk_log, "transferLog"},
        {transfer_disk_log_size, {10000, 3}},
        {directory_index,["index.html", "index.htm"]},
        {mime_types,[{"html", "text/html"}, {"htm", "text/html"},
                {"css", "text/css"}, {"svg", "image/svg+xml"}]},
        {server_tokens, none},
        {customize, omc_httpd_custom},
        {keep_alive_timeout, 150},
        {minimum_bytes_per_second, 100},
        {max_body_size, 10000},
        {max_clients, 100},
        {max_header_size, 10240},
        {max_uri_size, 500},
        {max_keep_alive_request, 1000},
	{ipfamily, sid_to_ipfamily(ServiceId)}],
    safer_start(https_login, ServiceId, Props ++ CProps).

safer_start(Ht_type, ServiceId, Props) ->
    case workaround_listen_test(sid_to_bindaddress(ServiceId),
				sid_to_port(ServiceId),
				sid_to_sock_opts(ServiceId)) of
	{error, eaddrnotavail} ->
	    info_msg("Starting ~s service: ~p failed, "
		      "retry in ~p seconds.~n",
		      [atom_to_list(Ht_type), sid_to_name(ServiceId),
		       ?SHORT_REINITTIMEOUT div 1000]),
	    erlang:send_after(?SHORT_REINITTIMEOUT, self(), reinit_timeout),
	    undefined;
	_ ->
	    Web =
            case Ht_type of
                https ->
                    sysWeb;
                https_login ->
                    sysWebSec
            end,
	    case inets:start(httpd, Props) of
		{ok, Pid} ->
		    sysWeb:update_servers(Web, Pid),
		    Pid;
		{error, _Reason} ->
		    warn_msg("Starting ~s service: ~p failed, "
			      "retry in ~p seconds.~n",
			      [atom_to_list(Ht_type), sid_to_name(ServiceId),
			       ?REINITTIMEOUT div 1000]),
		    erlang:send_after(?REINITTIMEOUT, self(), reinit_timeout),
		    undefined
	    end
    end.

%% example of ServiceId:
%% [{undefined, {https, "1.2.3.4", {1,2,3,4}, 443, <<"fib_19">>, 3}}]
sid_to_name(ServiceId) ->
    element(2, ServiceId).

sid_to_bindaddress(ServiceId) ->
    element(3, ServiceId).

sid_to_port(ServiceId) ->
    element(4, ServiceId).

sid_to_sock_opts(ServiceId) ->
    case element(5, ServiceId) of
	<<>> -> [];
	Ns -> omc_api:ns_to_opt_list(Ns)
    end ++
    case element(6, ServiceId) of
	0 -> [];
	Dscp -> omc_api:get_dscp_opt_list(Dscp)
    end.

sid_to_ipfamily(ServiceId) ->
    case element(3, ServiceId) of
	T when size(T) =:= 4 ->
	    inet;
	T when size(T) =:= 8 ->
	    inet6;
	any -> %this is used in SIM
	    inet6
    end.

%%% ----------------------------------------------------------
%%% Certificate handling
%%% ----------------------------------------------------------
get_cert_conf() ->
    case mnesia:dirty_read(https, {"1","1","1","1","1"}) of
	[Obj] ->
            {Obj#https.nodeCredential, Obj#https.trustCategory};
        _ ->
            {undefined, undefined}
    end.

get_certs() ->
    case mnesia:dirty_read(https, {"1","1","1","1","1"}) of
	[Obj] ->
	    NcDN = Obj#https.nodeCredential,
	    TcDN = Obj#https.trustCategory,
	    case lists:member(undefined, [NcDN, TcDN]) of
		true ->
		    certI:unsubscribe(?MODULE),
		    undefined;
		false ->
		    get_cert(NcDN, TcDN)
	    end;
	Other ->
	    %% Should never happen
	    error_msg("Error when reading https certs ~p~n", [Other]),
	    undefined
    end.

get_cert(NcDN, TcDN) ->
    case catch certI:get_cert(NcDN) of
	{ok, [NcCert|NcChain], NcCertKey} ->
	    case catch certI:get_cert(TcDN) of
		{ok, TcCerts} ->
		    certI:unsubscribe(?MODULE),
		    certI:subscribe(NcDN, ?MODULE),
		    certI:subscribe(TcDN, ?MODULE),
		    {NcCert, NcCertKey, TcCerts ++ NcChain};
		Other ->
		    %% Should never happen
		    error_msg("Error when fetching TC ~p~n~p~n",
                        [TcDN, Other]),
		    undefined
	    end;
        {ok, NcCert, NcCertKey} -> % Remove when cert released
	    case catch certI:get_cert(TcDN) of
		{ok, TcCerts} ->
		    certI:unsubscribe(?MODULE),
		    certI:subscribe(NcDN, ?MODULE),
		    certI:subscribe(TcDN, ?MODULE),
		    {NcCert, NcCertKey, TcCerts};
		Other ->
		    %% Should never happen
		    error_msg("Error when fetching TC ~p~n~p~n",
                        [TcDN, Other]),
		    undefined
	    end;
	Other ->
	    %% Should never happen
	    error_msg("Error when fetching NC ~p~n~p~n", [NcDN, Other]),
	    undefined
    end.

get_pre_cred() ->
    do_get_pre_cred(sysEnv:vrcs()).

do_get_pre_cred(false) ->
    case catch certSecStore:get_vc() of
	{ok, VcCertPem, VcKeyPem} ->
	    case {public_key:pem_decode(VcKeyPem),
                    public_key:pem_decode(VcCertPem)} of
		{[], _} ->
		    {error, "VC found but key cannot be decoded"};
		{_, []} ->
		    {error, "VC found but cert cannot be decoded"};
		{[{KeyInfo, VcKeyData, _}], [{'Certificate', VcCert, _}|T]} ->
		    VcKey = {KeyInfo, VcKeyData},
		    Chain = [Cert || {'Certificate', Cert, _} <- T],
		    {ok, {VcCert, VcKey, Chain}};
		_Other ->
		    {error, "VC found but can not decode"}
	    end;
	not_found ->
	    {error, "VC not found"};
	{error, timeout} ->
	    {error, try_again}
    end;
do_get_pre_cred(true) ->
    case catch certI:get_vnf_cert() of
        {ok, [Cert, SubCa], {Format, Key}, [RootCa, _VnfmSubCa], _Type} ->
            {ok, {Cert, {Format, Key}, [SubCa, RootCa]}};
         {ok, [Cert, SubCa], {Format, Key}, [RootCa], _Type} ->
            %% For RVNFM (no VnfmSubCa since it is the same as SubCa in RVNFM)
            {ok, {Cert, {Format, Key}, [SubCa, RootCa]}};
        {error, Reason} ->
            {error, Reason};
	{'EXIT', _Reason} ->
	    %% Probably a timeout in cert. Try again.
	    {error, try_again}
    end.

%%% ----------------------------------------------------------
%%% EMCLI "flag-file" (sec_proto.txt) is updated.
%%% Prefer TLS
%%% This function is used for telling EMCLI if CLI is
%%% configured to use TLS or not.
%%% ----------------------------------------------------------
do_change_notify() ->
    TlsProps = comsaI:get_cli_tls_config(),
    SshOrTls =
	case is_tls_enabled(TlsProps) of
	    yes ->
		"TLS\n";
	    no ->
		SshProps = comsaI:get_cli_ssh_config(),
		case proplists:get_value(administrativeState, SshProps) of
		    'LOCKED' ->
			"NONE\n"; %% Should not be possible
		    'UNLOCKED' ->
			"SSH\n"
		end
	end,
    File = filename:join([sysEnv:www_doc_root(), "sec_proto.txt"]),
    case file:write_file(File, list_to_binary(SshOrTls)) of
	ok ->
	    ok;
	{error, Reason} ->
	    error_msg("Failed writing ~p ~n", [Reason])
    end.

is_tls_enabled(TlsProps) ->
    %% nodeCredential and trustcategory needs to be set and
    %% administrativeState needs to be set to UNLOCKED.
    Nc = proplists:get_value(nodeCredential, TlsProps),
    Tc = proplists:get_value(trustCategory, TlsProps),
    As = proplists:get_value(administrativeState, TlsProps),
    case {Nc, Tc, As} of
	{undefined, _TC, _As} -> no;
	{_NC, undefined, _As} -> no;
	{_Nc, _Tc, undefined} -> no;
	{_NC, _Tc, 'LOCKED'} -> no;
	{_NC, _Tc, 'UNLOCKED'} -> yes
    end.

%%% ----------------------------------------------------------
%%% stop httpd services
%%% ----------------------------------------------------------
stop_services() ->
    Services = inets:services(),
    {KeepPortWeb, KeepPortWebSec} =
    case swmI:node_type() of
        "R-VNFM" ->
            {ok, _, PortWeb, _} = apply(secI, get_if_info, ["local"]),
            {PortWeb, undefined};
        _ ->
            {sysEnv:get_port_conf(https), sysEnv:get_port_conf(https_login)}
    end,
    stop_services(Services, {KeepPortWeb, KeepPortWebSec}).

stop_services([], _) ->
    ok;
stop_services([{httpd, Pid} | T], {KeepPortWeb, KeepPortWebSec} = Ports) ->
    case catch httpd:info(Pid, [port]) of
	[{port, KeepPortWeb}] ->
            stop_pid(Pid),
	    stop_services(T, Ports);
        [{port, KeepPortWebSec}] ->
            stop_pid(Pid),
	    stop_services(T, Ports);
	_ ->
	    stop_services(T, Ports)
    end;
stop_services([_ | T], KeepPort) ->
    stop_services(T, KeepPort).

stop_pid(undefined) ->
    ok;
stop_pid(Pid) ->
    case inets:stop(httpd, Pid) of
	ok ->
	    ok;
	{error, Reason} ->
	    error_msg("Stopping https service failed ~p~n", [{Pid, Reason}])
    end.

%%% ----------------------------------------------------------
%%% return a list of tuples describing what should be started
%%% ----------------------------------------------------------
%% [{Pid_of_service, {atom(Type), string(Name), IP-tuple|any, integer(PortNo),
%%   binary(Ns), integer(Dscp)}}];
%% example: [{undefined, {https, "1.2.3.4", {1,2,3,4}, 443, <<"fib_19">>, 3}}]
%%get_services(Target, Vrcs,_) ->
get_services(false, false, _) ->
    %% Simulated - run one instance on 'any'
    PortWeb      = sysEnv:get_port_conf(https),
    PortWebSec = sysEnv:get_port_conf(https_login),
    [{undefined, {https, "Simulated RCS", any, PortWeb, <<>>, 0}},
     {undefined, {https_login, "Simulated RCS", any, PortWebSec, <<>>, 0}}];
get_services(true, _, _) ->
    %% Target environment
    PortWeb    = sysEnv:get_port_conf(https),
    PortWebSec = sysEnv:get_port_conf(https_login),

    %% Check if OOT is ready (could be waiting for IMM due to upgrade).
    case ootI:get_lmt_ipv4() of
	{error, oot_not_started} ->
	    %% Start one server for any.
	    %% http/https redirect will not work due to "Temporary webserver".
	    [{undefined, {https, "Temporary webserver", any, PortWeb, <<>>, 0}},
             {undefined, {https_login, "Temporary webserver", any, PortWebSec,
                     <<>>, 0}}];
	_ ->
	    case {sysInitI:get_lmt_ns(), ootI:get_oap_ip_addr()} of
		{<<>>, ""} ->
		    %% Lmt not namespaced; OamAccessPoint not set.
		    %% Start server for link local (eth0:1) and DHCP (eth0).
		    IpLl = ootI:get_lmt_ll_ipv4(),
		    [{undefined,
                        {https, inet:ntoa(IpLl), IpLl,
                            PortWeb, _LmtNs = <<>>, 0}},
                     {undefined,
                         {https_login, inet:ntoa(IpLl), IpLl,
                             PortWebSec, _LmtNs = <<>>, 0}}] ++
                     lmt_dhcp_service(https, _LmtNs = <<>>, PortWeb) ++
                     lmt_dhcp_service(https_login,_LmtNs = <<>>, PortWebSec);
		{LmtNs, OamIP} ->
		    %% Lmt might be namespaced; OamAccessPoint set.
		    %% Run a server for every IP. Even if namespaced,
                    %% inets doesn't know about this and will refuse multiple
                    %% servers if it seems they would clash.
		    IpLl = ootI:get_lmt_ll_ipv4(),
		    [{undefined,
		      {https, inet:ntoa(IpLl), IpLl,
                          PortWeb, LmtNs, 0}},
                     {undefined,
		      {https_login, inet:ntoa(IpLl), IpLl,
                          PortWebSec,LmtNs,0}}] ++
			lmt_dhcp_service(https, LmtNs, PortWeb) ++
			lmt_dhcp_service(https_login, LmtNs, PortWebSec) ++
			tn_oot_service(https, OamIP, PortWeb) ++
			tn_oot_service(https_login, OamIP, PortWebSec)
	    end
    end;
get_services(_, true, "R-VNFM") ->
    %% R-VNFM
    {ok, _, PortWeb, _} = apply(secI, get_if_info, ["local"]),
    case {sysInitI:get_lmt_ns(), ootI:get_oap_ip_addr()} of
	{<<>>, ""} ->
	    %% Lmt not namespaced; OamAccessPoint not set.
	    %% Listen to "any" (no LMT in cloud, eth0 is tenant)
	    [{undefined, {https, "oam_any"++integer_to_list(PortWeb),
                        any, PortWeb, _LmtNs = <<>>, 0}}];
	{LmtNs, OamIP} ->
	    %% Lmt might be namespaced; OamAccessPoint set.
	    %% Listen to "any" (no LMT in cloud, eth0 is tenant)
	    [{undefined, {https, "oam_any"++integer_to_list(PortWeb),
                        any, PortWeb, LmtNs, 0}}] ++
		tn_oot_service(https, OamIP, PortWeb)
    end;
get_services(_, true,_) ->
    %% vRCS/vPP/... 
    PortWeb    = sysEnv:get_port_conf(https),
    case {sysInitI:get_lmt_ns(), ootI:get_oap_ip_addr()} of
	{<<>>, ""} ->
	    %% Lmt not namespaced; OamAccessPoint not set.
	    %% Listen to "any" (no LMT in cloud, eth0 is tenant)
	    [{undefined, {https, "tenant_any"++integer_to_list(PortWeb),
                        any, PortWeb, _LmtNs = <<>>, 0}}];
	{LmtNs, OamIP} ->
	    %% Lmt might be namespaced; OamAccessPoint set.
	    %% Listen to "any" (no LMT in cloud, eth0 is tenant)
	    [{undefined, {https, "tenant_any"++integer_to_list(PortWeb),
                        any, PortWeb, LmtNs, 0}}] ++
		tn_oot_service(https, OamIP, PortWeb)
    end.

lmt_dhcp_service(Type, LmtNs, Port) ->
    case ootI:get_lmt_ipv4() of
	undefined ->
	    [];
	{error, oot_not_started} ->
	    %% Should not happen, but just in case to be more robust.
	    [];
	IpT ->
	    [{undefined, {Type, inet:ntoa(IpT), IpT, Port, LmtNs, 0}}]
    end.

tn_oot_service(_Type, [], _Port) ->
    %% no OamAccessPoint
    [];
tn_oot_service(Type, OamIp, Port) ->
    case ootI:get_oap_namespace() of
	{ok, Ns} ->
	    {ok, Iptuple} = inet:parse_address(OamIp),
	    Dscp = ootI:get_oap_dscp(),
	    AltIp = ootI:get_oap_ip_addr_alt(),
	    AltNs = ootI:get_oap_alt_namespace(),
	    [{undefined, {Type, OamIp, Iptuple, Port, Ns, Dscp}}] ++
		tn_oot_alt_service(Type, OamIp, AltIp, Port, Ns, AltNs, Dscp);
	{error, _} ->
	    %% resolution not done yet, return nothing and await callback
	    []
    end.

tn_oot_alt_service(_, Same, Same, _, Ns, {ok, Ns}, _) ->
    error_msg(
	"Configuration error. OaM AccessPoint and alternate OaM AccessPoint "
	"have the same address in the same namespace~n", []),
    [];
tn_oot_alt_service(_, _, "", _, _, _, _) ->
    [];
tn_oot_alt_service(Type, _, AltIp, Port, _, {ok, AltNs}, Dscp) ->
    {ok, AltTuple} = inet:parse_address(AltIp),
    [{undefined, {Type, AltIp, AltTuple, Port, AltNs, Dscp}}];
tn_oot_alt_service(_, _, _, _, _, _, _) ->
    [].

print_server_change(ToStop, ToStart, Unchanged) ->
    ToStop =:= [] orelse info_msg("HTTPS stopping service/s:~n~p~n",
				 [printify(ToStop)]),
    ToStart =:= [] orelse info_msg("HTTPS start/restart service/s:~n~p~n",
				   [printify(ToStart)]),
    Unchanged =:= [] orelse info_msg("HTTPS unchanged service/s:~n~p~n",
				     [printify(Unchanged)]).

printify([]) -> [];
printify([{Pid, Service} | T]) ->
    %% include the Pid in printout to differentiate between start/restart
    [{Pid, sid_to_bindaddress(Service), sid_to_port(Service)} |
     printify(T)].

flush_duplicate_reinit_timeout() ->
    receive
	reinit_timeout ->
	    flush_duplicate_reinit_timeout()
    after
	0 ->
	    done
    end.

%% it happens that the local IP address is still "tentative"
%% (possibly because DAD is in progress)
%% this will test using gen_tcp to avoid a crash printout from inets
workaround_listen_test(LocalIp, P, Opts)
  when is_tuple(LocalIp) andalso
       (tuple_size(LocalIp) =:= 8 orelse tuple_size(LocalIp) =:= 4) ->
    NetnsOpt = get_netns_opt(Opts),
    InetType = get_inet_type(LocalIp),
    case gen_tcp:listen(P, [InetType, {ip, LocalIp}] ++ NetnsOpt) of
        {ok, Lsock} ->
            gen_tcp:close(Lsock),
            ok;
        Err ->
            Err
    end;
workaround_listen_test(_LocalIp, _P, _Opts) ->
    ok.


get_netns_opt(Opts) ->
    case lists:keyfind(netns, 1, Opts) of
	false -> [];
	Tuple -> [Tuple]
    end.


get_inet_type(IPv6) when tuple_size(IPv6) =:= 8 ->
    inet6;
get_inet_type(_IPv4) ->
    inet.


%%% ----------------------------------------------------------
%%% Info-msg, error-msg and warning-msg
%%% ----------------------------------------------------------
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

warn_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%%% ----------------------------------------------------------
%%% Updates http(s) servers - test function
%%% ----------------------------------------------------------
do_update() ->
    inets:stop(),
    inets:start(),
    case gen_server:call(?MODULE, clear_state) of
	ok ->
	    restart_complete();
	Other ->
	    error_msg("Failed to update https (test function) ~p~n", [Other])
    end.
