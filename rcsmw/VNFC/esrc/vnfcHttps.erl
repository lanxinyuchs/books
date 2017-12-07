%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	vnfcHttps.erl %
%%% @author etxasta
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/6, checkout by etxasta in etxasta_rcs
%%% 
%%% @doc ==HTTPS server ==
%%% This module implements the HTTPS server for VNF internal
%%% communication between VNF OAM and VNFM
-module(vnfcHttps).
-behaviour(gen_server).
-vsn('').
-date('2017-02-17').
-author('etxasta').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB %CCaseTemplateCopyrightYear% All rights reserved.
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
%%% Date        Name      What
%%% ----------  --------  ------------------------------------
%%% 2017-02-17  etxasta   Created
%%% 2017-04-10  etxasta   Only start https server when vrcs
%%% 2017-05-03  etxasta   Added send_receive/1
%%% 2017-05-08  etxasta   Bug fix
%%% 2017-08-23  etxaldu   Bug fix, return {error, Reason}
%%% 2017-10-17  emariad   Added parameter server_name_indication to disabled
%%% 2017-10-20  emariad   Prepared for mutual auth, added client ssl cert
%%% 2017-10-24  emariad   Added server_name_indication in client as well
%%% 2017-11-16  etxberb   Added 'return' option in send_receive/1.
%%% 2017-11-30  emariad   Changed src ip
%%% 2017-12-01  etxberb   Added 'verbose_' options in send_receive/1.
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%% API
-export([start/0,
         start_server/0,
         restart_server/0,
         stop_server/0]).

-export([send/1]).

-export([send_receive/1]).


-export([get_certs/0,
         get_ip_type/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-record(state, {pid,             % Pid of https server process
                status = down}). % Status (up|down|wait|failed) of https server 

-include_lib("public_key.hrl").
-include_lib("vnfcs.hrl").


-define(REQUEST_TIMEOUT,  90000).
-define(REQUEST_CONNECT_TIMEOUT,  90000).
-define(VNFC_HTTPS, "vnfc_tenant").


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% -type start() ->                                        %#
%%%     ok | error().                                       %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% ----------------------------------------------------------
%%% -type start_server() ->                                 %#
%%%     ok                                                  %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
start_server() ->
    store_eth0_ip_addr(),
    gen_server:cast(?MODULE, start_server).

%%% ----------------------------------------------------------
%%% -type restart_server() ->                                 %#
%%%     ok                                                  %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
restart_server() ->
    gen_server:cast(?MODULE, restart_server).


stop_server() ->    
    gen_server:cast(?MODULE, stop_server).

%%% ----------------------------------------------------------
%%% -type send(Data) ->      %#
%%%     ok                                                  %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
send(Request) ->
    gen_server:call(?MODULE, {send, Request, post, undefined}).

%%% ----------------------------------------------------------
%%% -type send_receive(Map) ->                              %#
%%%     ok                                                  %#
%%% Input: Map - #{data    => SendData,
%%%                timeout => integer(milliseconds), -- default 30000ms
%%%                method  => atom(post|get),        -- default post 
%%%                return  => tuple|httpc|raw,       -- default tuple 
%%%                verbose_send  => boolean(),       -- default true 
%%%                verbose_receive  => boolean()}    -- default true 
%%% Output: For 'return' := tuple:
%%%             {ok, binary(Bin)} | {error, string(ReasonPhrase)}
%%%         For 'return' := httpc:   <http://erlang.org/doc/man/httpc.html>
%%%             #{http_version  => http_version(),
%%%               status_code   => status_code(),
%%%               reason_phrase => reason_phrase(),
%%%               headers       => list(header({field(), value()})),
%%%               body          => string() | binary(),
%%%               request_id    => request_id()}
%%%         For 'return' := raw:
%%%             term()   <As returned by httpc.erl>
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
send_receive(Map) ->
    Timeout = maps:get(timeout, Map, 30000),
    Request = maps:get(data, Map),
    Method  = maps:get(method, Map, post),
    case gen_server:call(?MODULE, {send, Request, Method, {self(), Map}}) of
        {ok,_} ->
            receive
		{?MODULE, Result} ->
		    Result
            after
                Timeout ->
                    {error,
                        "Timeout after "++integer_to_list(Timeout)++" millisec"}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
init(_Args) ->
    process_flag(trap_exit, true),
    case sysEnv:vrcs() of
        true ->
            case swmI:node_type() of
                "R-VNFM" -> % Don't start on R-VNFM
                    ok;
                _ ->
                    inets:start(httpc, [{profile, vnfc_send}]),
                    start_server()
            end;
        _ ->
            ok
    end,
    {ok, #state{status = init}}.

handle_call({send, Request, Method, {Pid, Map}}, _From, State) ->
    Ret = maps:get(return, Map, tuple),
    put(verbose_send, maps:get(verbose_send, Map, true)),
    put(verbose_receive, maps:get(verbose_receive, Map, true)),
    ReplyInfo = {Pid, Ret},
    {reply, handle_send(Request, Method, ReplyInfo), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(start_server, State) ->
    {noreply, handle_start_server(State)};
handle_cast(restart_server, State) ->
    {noreply, handle_restart_server(State)};
handle_cast(stop_server, State) ->
    {noreply,handle_stop_server(State)};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({http, {RequestId, {{_,200,"OK"},_, _} = Result}}, State) ->
    reply_to_pid(RequestId, Result),
    {noreply, State};
handle_info({http, {RequestId, {{_,202,"Accepted"},_, _} = Result}}, State) ->
    reply_to_pid(RequestId, Result),
    {noreply, State};
handle_info({http, {RequestId, Error}}, State) ->
    verbose_receive(RequestId, Error),
    reply_to_pid(RequestId, Error),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

reply_to_pid(RequestId, Result) ->
    case erase({send_receive, RequestId}) of
        undefined ->
            ok;
        {ReplyPid, ReturnOpt} ->
            ReplyPid ! {?MODULE, reply_format(ReturnOpt, Result)}
    end.

%%% #---------------------------------------------------------
reply_format(tuple, {{_,200,"OK"},_, Body}) ->
    {ok, Body};
reply_format(tuple, {{_,202,"Accepted"},_, Body}) ->
    {ok, Body};
reply_format(tuple, {{_, _, ReasonPhrase},_, _}) ->
    {error, ReasonPhrase};
reply_format(tuple, Error) ->
    Error;
reply_format(httpc, {{HttpVersion, StatusCode, ReasonPhrase},
		     Headers,
		     Body}) ->
    #{http_version => HttpVersion,
      status_code => StatusCode,
      reason_phrase => ReasonPhrase,
      headers => Headers,
      body => Body};
reply_format(httpc, {StatusCode, Body}) ->
    #{status_code => StatusCode,
      body => Body};
reply_format(httpc, RequestId) ->
    #{request_id => RequestId};
reply_format(raw, Result) ->
    Result;
reply_format(UnrecRetOpt, Result) ->
    sysInitI:error_report([{"Unrecognized return option", UnrecRetOpt},
			   {"...in call to", {?MODULE, send_receive}}]),
    Result.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
handle_start_server(State) ->
   case (get(inet) =:= undefined) and (get(inet6) =:= undefined) of
        true ->
            info_msg("No Ip configured, try start server again in 3 sec ", []),
            erlang:send_after(3000, self(), {start_server});
        false ->     
            handle_start_server(get_certs(), State)
    end.

handle_start_server({ok, Cert, Key, TrustList, Type}, State) ->
   Props = http_props(),
   Ssl_props = ssl_props(false, Cert, Key, TrustList, Type),
   
   %%To be added later to run pure ipv6 in dual cloud
%%    {SrcsIp, Type} =
%%        case get(inet6) of
%%            undefined ->
%%                {get(inet), inet};
%%            IP ->
%%                {IP, inet6}
%%        end,
   
   IpProps = [{ipfamily, inet6},
              {bind_address, any}],
   
   do_start_https_server(Props ++ IpProps ++ Ssl_props, State);
handle_start_server({error, Reason}, State) ->
    info_msg("Failed to start the internal vnfc(oam)-vnfm https server, ~p",
        [Reason]),
    erlang:send_after(1000, self(), start_server), 
    State#state{status = failed}. 

do_start_https_server(Props, State) ->
    https_server_res(inets:start(httpd, Props), State).

https_server_res({ok, Pid}, State) ->
    info_msg("Started ~p https server (~p)", [?MODULE, Pid]),
    put(vnfc_https_pid, Pid),
    State#state{pid = Pid, status = up};
https_server_res({error, Reason}, State) ->
    info_msg("Starting ~p https server failed, ~p", [?MODULE, Reason]),
    State#state{status = failed}.

handle_restart_server(State) ->
   State1 = handle_stop_server(State), 
   handle_start_server(State1).

handle_stop_server(State) ->
    Pid = get(vnfc_https_pid),
    info_msg("Terminating https server (~p) ", [Pid]),
    inets:stop(httpd, Pid),
    State#state{status = down, pid = undefined}.

handle_send({Uri,_,_,_} = Request, post, ReplyInfo) ->
    verbose_send(Request),
    HttpOpts = [{timeout, ?REQUEST_TIMEOUT},
                {connect_timeout, ?REQUEST_CONNECT_TIMEOUT},
                {ssl, ssl_send_opts()}],
     Opts = [{sync, false},
             {ipv6_host_with_brackets, true},
             {body_format, binary}],
    IpFamily = get_ip_type(Uri),
    
    SrcIp = get(IpFamily),
    case SrcIp =/= undefined of
        true ->
            do_handle_send(IpFamily, post, Request, HttpOpts, 
                           Opts, ReplyInfo); 
        false ->
            info_msg("Wrong Uri. No src ip configured for: ~p", [IpFamily]),
            {error, "Wrong Uri. No src ip configured"}
    end;
    %% To be added later when we sue pure ipv6
%%     %%Always use ipv6 if it is configured
%%     case get(inet6) =/= undefined of
%%         true ->
%%             case inet6 =:= IpFamily of
%%                 true ->
%%                     info_msg("IP family: ~p, Source IP: ~p",
%%                              [IpFamily, get(inet6)]),
%%                     do_handle_send(IpFamily, post, Request, HttpOpts, 
%%                                    Opts, ReplyInfo); 
%%                 false ->
%%                     info_msg("Wrong Uri. Should be ~p", [get(inet6)]),
%%                     {error, "Wrong Uri. Should be ipv6 address"}
%%             end;
%%         false ->
%%             case inet =:= IpFamily of
%%                 true ->
%%                     info_msg("IP family: ~p, Source IP: ~p", 
%%                              [IpFamily, get(inet)]),
%%                     do_handle_send(IpFamily, post, Request, HttpOpts, 
%%                                    Opts, ReplyInfo); 
%%                 false ->
%%                     info_msg("Wrong Uri. Should be ~p", [get(inet)]),
%%                     {error, "Wrong Uri. Should be ipv4 address"}
%%             end
%%     end;
handle_send({Uri,_} = Request, get, ReplyInfo) ->
    verbose_send(Request),
    HttpOpts = [{timeout, ?REQUEST_TIMEOUT},
                {connect_timeout, ?REQUEST_CONNECT_TIMEOUT},
                {ssl, ssl_send_opts()},
                {autoredirect, false}],
    Opts = [{sync, false},
             {ipv6_host_with_brackets, true},
             {body_format, binary}],
    IpFamily = get_ip_type(Uri),
    
    SrcIp = get(IpFamily),
    case SrcIp =/= undefined of
        true ->
            do_handle_send(IpFamily, get, Request, HttpOpts, 
                           Opts, ReplyInfo); 
        false ->
            info_msg("Wrong Uri. No src ip configured for: ~p", [IpFamily]),
            {error, "Wrong Uri. No src ip configured"}
    end.
     %% To be added later when we sue pure ipv6
%%      %%Always use ipv6 if it is configured
%%     case get(inet6) =/= undefined of
%%         true ->
%%             case inet6 =:= IpFamily of
%%                 true ->
%%                     info_msg("IP family: ~p, Source IP: ~p", 
%%                              [IpFamily, get(inet6)]),
%%                     do_handle_send(IpFamily, get, Request, HttpOpts, 
%%                                    Opts, ReplyInfo); 
%%                 false ->
%%                     info_msg("Wrong Uri. Should be ~p", [get(inet6)]),
%%                     {error, "Wrong Uri. Should be ipv6 address"}
%%             end;
%%         false ->
%%             case inet =:= IpFamily of
%%                 true ->
%%                     info_msg("IP family: ~p, Source IP: ~p", 
%%                              [IpFamily, get(inet)]),
%%                    do_handle_send(IpFamily, get, Request, HttpOpts, 
%%                                   Opts, ReplyInfo);
%%                 false ->
%%                     info_msg("Wrong Uri. Should be ~p", [get(inet)]),
%%                     {error, "Wrong Uri. Should be ipv4 address"}
%%             end
%%     end.
    
%%% #---------------------------------------------------------
verbose_send(Request) ->
    case erase(verbose_send) of
	VS when VS == undefined orelse VS == true ->
	    info_msg("send(~p)", [Request]);
	_ ->
	    ok
    end.

%%% #---------------------------------------------------------
verbose_receive(RequestId, Error) ->
    case erase(verbose_receive) of
	VR when VR == undefined orelse VR == true ->
	    info_msg("Send to vnfm failed, ref: ~p~nReason: ~p",
		     [RequestId, Error]);
	_ ->
	    ok
    end.

%%% #---------------------------------------------------------
do_handle_send(inet6, RequestMethod, Request, HttpOpts, Opts, ReplyInfo) ->
    httpc:set_options([{ipfamily, inet6},
                    {ip, get(inet6)}], vnfc_send),
     case catch httpc:request(RequestMethod, Request, HttpOpts,
                             Opts, vnfc_send) of
        {ok, RequestId} ->
            store_reply_pid(ReplyInfo, RequestId),
            {ok, RequestId};
        {_, Reason} ->
            info_msg("Httpc request error, reason: ~p", [Reason]),
            {error, "Failed to send message"}
    end;
do_handle_send(inet, RequestMethod, Request, HttpOpts, Opts, ReplyInfo) -> 
    httpc:set_options([{ipfamily, inet},
                    {ip, get(inet)}], vnfc_send),
    case catch httpc:request(RequestMethod, Request, HttpOpts,
                             Opts, vnfc_send) of
        {ok, RequestId} ->
            store_reply_pid(ReplyInfo, RequestId),
            {ok, RequestId};
        {_, Reason} ->
            info_msg("Httpc request error, reason: ~p", [Reason]),
            {error, "Failed to send message"}
    end.

%%% #---------------------------------------------------------
%%% #3.4   CODE FOR INTERNAL HELP FUNCTIONS
%%% #---------------------------------------------------------
http_props() ->
    [{modules, [mod_alias, mod_auth, mod_esi, mod_actions, mod_cgi, mod_get,
                mod_head, mod_log, mod_disk_log]},
     {server_name, "vnfc_tenant"},
     {server_root, server_doc_root("vnfc_tenant")},
     {document_root, server_doc_root("vnfc_tenant")},
     {port, 4443},
     {log_format, combined},
     {error_log_format, pretty},
     {disk_log_format, external},
     {error_disk_log, "errorLog"},
     {error_disk_log_size, {10000, 3}},
     {security_disk_log, "securityLog"},
     {security_disk_log_size, {10000, 3}},
     {transfer_disk_log, "transferLog"},
     {transfer_disk_log_size, {10000, 3}},
     {server_tokens, none},
     {customize, omc_httpd_custom},
     {keep_alive_timeout, 150},
     {minimum_bytes_per_second, 100},
     {max_body_size, 10000},
     {max_clients, 20},
     {max_header_size, 10240},
     {max_uri_size, 500},
     {max_keep_alive_request, 1000},
     {erl_script_alias, {"/erisup", [vnfcs]}},
     {erl_script_alias, {"/eriswm", [swmv1]}},
     {mime_types, [
                   {"json", "application/json"}]}].

ssl_props(true, Cert, Key, TrustList, real) ->  
    %% Mutual auth on
    VFun = certVerify:mk_verify_fun(TrustList, {vcpeer, undefined}, 1), 
    info_msg("SSL server options. Real network. Mutual auth on", []),
    [{socket_type,
        {essl,
            [{cert,       Cert},
             {key,        Key},
             {cacerts,    TrustList},
             {fail_if_no_peer_cert, true},
             {depth,      10},
             {ciphers, comsaI:get_tls_cipher_suites()},
             {log_alert, false},
             {server_name_indication, disable},
             {versions,   [tlsv1, 'tlsv1.2']}] ++ VFun}}];
ssl_props(false, Cert, Key, TrustList, real) -> 
    %% Mutual auth off
    info_msg("SSL server options. Real network. Mutual auth off", []),
    [{socket_type,
        {essl,
            [{cert,       Cert},
             {key,        Key},
             {cacerts,    TrustList},
             {fail_if_no_peer_cert, false},
             {verify,    verify_none},
             {depth,      10}, 
             {ciphers, comsaI:get_tls_cipher_suites()},
             {log_alert, false},
             {server_name_indication, disable},
             {versions,   [tlsv1, 'tlsv1.2']}]}}];
ssl_props(_, Cert, Key, TrustList, dummy) ->
    info_msg("SSL server options. Dummy network", []),
    [{socket_type,
        {essl,
            [{cert,       Cert},
             {key,        Key},
             {cacerts,    TrustList},
             {fail_if_no_peer_cert, false},
             {verify,    verify_none}, 
             {depth,      10},
             {ciphers, comsaI:get_tls_cipher_suites()},
             {log_alert, false},
             {server_name_indication, disable},
             {versions,   [tlsv1, 'tlsv1.2']}]}}].

get_certs() ->
    do_get_certs(certI:get_vnf_cert()).

do_get_certs({ok, [Cert, SubCa], {Format, Key}, [RootCa, VnfmSubCa], Type}) ->
    %% Root CA cert must be removed, to prevent connections to other vnfcs
    print_cert_info("VnfmSubCa", VnfmSubCa),
    print_cert_info("RootCa", RootCa),

    {ok, Cert, {Format, Key}, [SubCa, VnfmSubCa], Type};    
do_get_certs(Error) ->
    Error.

print_cert_info(Info, DerCert) ->
    OtpCert = public_key:pkix_decode_cert(DerCert, otp),
    #'OTPCertificate'{tbsCertificate = C} = OtpCert,
    #'OTPTBSCertificate'{subject = Subject} = C,
    info_msg("Certificate(~p) -> ~p", [Info, certLib:format_rdn(Subject)]).

store_reply_pid(undefined, _) ->
    ok;
store_reply_pid(ReplyInfo, RequestId) ->
    put({send_receive, RequestId}, ReplyInfo).

get_ip_type(Uri) ->
    {ok, Resolved_url} = certLib:resolve_uri(Uri),
    {ok, {_Proto, _User, Host, _Port, _Path, _Query}} = ftpI:parse_uri(Resolved_url),
    case string:str(Host, ":") > 0 of
        true -> inet6;
        false -> inet
    end.

ssl_send_opts() ->
    ssl_send_opts(get_send_certs()).

ssl_send_opts({ok, Cert, Key, TrustList, real}) ->
    %%TODO add mutual auth
    VFun = certVerify:mk_verify_fun(TrustList, {vcpeer, undefined}, 1),
    info_msg("SSL client options. Real network", []),
    [{cert,       Cert},
     {key,        Key},
     {cacerts,    TrustList},
     {verify, verify_none},
     {fail_if_no_peer_cert, false},
     {depth,      10},
     {ciphers, comsaI:get_tls_cipher_suites()},
     {server_name_indication, disable},
     {versions,   [tlsv1, 'tlsv1.2']}] ++ VFun;
ssl_send_opts({ok, Cert, Key, TrustList, dummy}) ->
    %% Mutual auth off
    info_msg("SSL client options. Dummy network", []),
    [{cert,       Cert},
     {key,        Key},
     {cacerts,    TrustList},
     {verify, verify_none}, 
     {fail_if_no_peer_cert, false},  
     {depth,      10},
     {ciphers, comsaI:get_tls_cipher_suites()},
     {server_name_indication, disable},
     {versions,   [tlsv1, 'tlsv1.2']}]. % TODO Remove tlsv1?

get_send_certs() ->
    do_get_send_certs(get(cert), get(key), get(tc), get(cert_type)).

do_get_send_certs(undefined, _Key, _Tc, _Type) ->
    %% First time, fetch the certificates
    case get_certs() of
        {ok, Cert, {Format, Key}, [SubCa, VnfmSubCa], Type} ->
            put(cert, Cert),
            put(key, {Format, Key}),
            put(tc, [SubCa, VnfmSubCa]),
            put(cert_type, Type), %% atom(dummmy or real) 
            {ok, Cert, {Format, Key}, [SubCa, VnfmSubCa], Type};
        Error ->
            Error
    end;
do_get_send_certs(Cert, Key, Tc, Type) ->
    {ok, Cert, Key, Tc, Type}.

store_eth0_ip_addr() ->
    Res = os:cmd("ip addr list eth0 | grep inet"),
    
    case Res of
        "Device \"eth0\" does not exist.\n" ->
            info_msg("no_eth0_configured ", []),
            {error, no_eth0_configured};
        [] ->
            info_msg("no_inet_configured ", []),
            {error, no_inet_configured};
        _ ->
            List = string:tokens(Res, "\n"),
            do_store_eth0_ip_addr(List)
    end.

do_store_eth0_ip_addr([H | Rest]) ->
    case string:str(H, "global") of
        0 ->
            do_store_eth0_ip_addr(Rest);
        _ ->
            do_store_eth0_ip_addr_2(string:tokens(H, " ")),
            do_store_eth0_ip_addr(Rest)
    end;
do_store_eth0_ip_addr([]) ->
    ok.

do_store_eth0_ip_addr_2(["inet"| Rest]) ->
    [Addr | _] = Rest,
    case string:str(Addr, "/") of
        0 ->
            do_store_eth0_ip_addr_3(inet:parse_address(Addr), Addr, inet);
        Int ->
        SubStringAddr = string:substr(Addr, 1, Int-1),
        do_store_eth0_ip_addr_3(inet:parse_address(SubStringAddr), 
                                SubStringAddr, inet)
    end;
do_store_eth0_ip_addr_2(["inet6"| Rest]) ->
    [Addr | _] = Rest,
    case string:str(Addr, "/") of
        0 ->
            do_store_eth0_ip_addr_3(inet:parse_address(Addr), Addr, inet6);
        Int ->
        SubStringAddr = string:substr(Addr, 1, Int-1),
        do_store_eth0_ip_addr_3(inet:parse_address(SubStringAddr), 
                                SubStringAddr, inet6)
    end;
do_store_eth0_ip_addr_2([_|_]) ->
     info_msg("inet|inet6 not found ", []).

do_store_eth0_ip_addr_3({ok, IpAddress}, _Addr, Type) ->
    info_msg("Stored ip ~p", [IpAddress]),
    put(Type, IpAddress);
do_store_eth0_ip_addr_3({error, Reason}, Addr, _Type) ->
    info_msg("Could not store ip ~p. Reason: ~p", [Addr, Reason]).

server_doc_root(Name) ->
    Path = certLib:cert_dir() ++ "/ws/" ++ Name ++"/",
    ok = filelib:ensure_dir(Path),
    Path.


info_msg(Format, Args) ->
    certLib:info_msg(?MODULE, Format, Args).


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


