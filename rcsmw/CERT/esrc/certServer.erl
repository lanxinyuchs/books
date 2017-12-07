%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certServer.erl %
%%% @author emarnek
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/2
%%% 
%%% @doc ==Main server module for CERT==
%%% This module implements the main CERT server 

-module(certServer).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/2').
-date('2017-11-20').
-author('emarnek').
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
%%% Rev      Date        Name      What
%%% -----    ---------   --------  ------------------------
%%% R2A/1    2013-08-08  etxjotj   Created
%%% R2A/9    2013-12-19  etxarnu   Corrected handle_remove_certificate
%%% R2A/46   2014-09-04  etxlg     DSCP, TR HS73150
%%% R6A/6    2016-08-18  etxasta   Clearing action at 180 timeout
%%% R6A/7    2016-08-30  emariad   CSUC feature, cipher configuration
%%% R8A/1    2017-01-16  eivmiha   switched ssh_sftp and sysSftp to ftpI
%%% R9A/1    2017-01-30  etomist   HV59927 
%%% R9A/2    2017-02-02  estjako   Added support for ftpes
%%% R9A/3    2017-02-03  etxasta   Don't check in code that can't compile
%%% R9A/4    2017-02-03  ekurnik   Code cleanup
%%% R9A/5    2017-02-15  estjako   Changes in start_channel
%%% R9A/6    2017-03-02  etomist   HV57826 (debug COLI support)
%%% R9A/7    2017-03-13  eivomat   HV70944 handle_mk_verify_fun/2 changed
%%% R10A/1   2017-05-08  etomist   HV78257
%%% R10A/2   2017-05-08  etomist   HV78257, fix 2
%%% R10A/3   2017-05-08  etomist   HV78257, fix 3, fix 2 was wrong
%%% R10A/5   2017-05-22  etomist   HV88690 - SECI bug introduced with HV78257
%%% R10A/6   2017-05-24  emajkaa   HV90577
%%% R10A/7   2017-06-14  etomist   HV93632
%%% R11A/1   2017-07-26  emajkaa   HW15696
%%% R11A/2   2017-09-04  emajkaa   HW24930
%%% R11A/3   2017-09-11  emajkaa   HW27437
%%% R11A/3   2017-09-11  emirbos   Added get_tcat_certs_and_MoRefs/1
%%% R11A/4   2017-10-03  emirbos   get_tcat_certs_and_MoRefs/1 changed
%%% R11A/5   2017-10-10  emajkaa   HW34850
%%% R12A/1   2017-10-26  etomist   Fixing dialyzer faults caused by HV78257
%%% R12A/2   2017-11-20  emarnek   HW45665
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%% Called from supervisor
-export([start/0]).

%%% Action handling
%%% Actions on CertM MOC
-export([cancel/0, download_crl/0, install_certificate/3, 
	 remove_certificate/1]).

%% certI functions
-export([get_tcat/1, get_tcat_and_crlcheck/1, get_tcat_certs_and_MoRefs/1, mk_verify_fun/2]).

%%% Support functions
-export([update_progress/1, get_tc/1]).

-export([start_channel/5]).

-compile(nowarn_unused_vars).

-export([pre_install/1, pre_install/2]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-export([ecoli_change_expiration_timer_frequency/1]).

-include("RcsCertM.hrl").
-include("cert.hrl").
-include_lib("public_key.hrl").


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Cancels an ongoing CertM operation
%%% @end
%%% ----------------------------------------------------------
cancel() ->
    gen_server:cast(certServer, cancel),
    ok.

%%% ----------------------------------------------------------
%%% @doc Downloads a Certificate revocation list based on the distribution
%%%      points in the installed certificates and acts on them
%%% @end
%%% ----------------------------------------------------------
download_crl() ->
    gen_server:cast(certServer, download_crl).

%%% ----------------------------------------------------------
%%% @doc Installs a trusted certifikate to be download at the uri location
%%% @end
%%% ----------------------------------------------------------
install_certificate("", _, Fingerprint) ->
    gen_server:call(certServer, 
        {install_certificate, Fingerprint});
install_certificate(undefined, _, Fingerprint) ->
    gen_server:call(certServer, 
        {install_certificate, Fingerprint});
install_certificate(Uri, Password, Fingerprint) ->
    gen_server:cast(certServer, 
		    {install_certificate, Uri, Password, Fingerprint}).

%%% ----------------------------------------------------------
%%% @doc Removes a trusted certificate
%%% @end
%%% ----------------------------------------------------------
-spec remove_certificate([binary()]) -> ok.

remove_certificate(MoRef) ->
    gen_server:cast(certServer, {remove_certificate, MoRef}).

%%% ----------------------------------------------------------
%%% @doc Get trust category -> list of trusted certificates
%%% @end
%%% ----------------------------------------------------------
get_tcat(Key) ->
    case catch gen_server:call(certServer, {get_tcat, Key}) of
        {'EXIT', _} ->
            {error, not_found};
        TC ->
            TC
    end.

%%% ----------------------------------------------------------
%%% @doc Get trust category -> list of trusted certificates,
%%%      and the crl check parameter
%%% @end
%%% ----------------------------------------------------------
get_tcat_and_crlcheck(Key) ->
    case catch gen_server:call(certServer, {get_tcat_and_crlcheck, Key}) of
        {'EXIT', _} ->
            {error, not_found};
        TC ->
            TC
    end.

%%% ----------------------------------------------------------
%%% @doc Get trust category -> list of trusted certificates
%%%      and MO refs
%%% @end
%%% ----------------------------------------------------------
get_tcat_certs_and_MoRefs(Key) ->
    case catch gen_server:call(certServer, {get_tcat_certs_and_MoRefs, Key}) of
        {'EXIT', _} ->
            {error, not_found};
        TC ->
            TC
    end.
%%% ----------------------------------------------------------
%%% @doc Return verify_fun for verify peer certificate
%%% @end
%%% ----------------------------------------------------------
mk_verify_fun(TrustCategoryKey, InstancePid) ->
    case catch gen_server:call(certServer,
            {mk_verify_fun, TrustCategoryKey, InstancePid}) of
        {'EXIT', _} ->
            {error, not_found};
        FunTerm ->
            FunTerm
    end.

%%% ----------------------------------------------------------
%%% @doc Starts the certServer server process
%%% @end
%%% ----------------------------------------------------------
start() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% some_method(Parameter)->
%%    nn.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

-record(state, {actionId=1}).

init(_Args) ->
    inets:start(httpc, [{profile, cert}]),
    Root =  ["ManagedElement", "SystemFunctions", "SecM", "CertM"],
    comsaLib:register_callback(Root, certModel),
    mnesia:subscribe({table, trustedCertificate, detailed}),
    mnesia:subscribe({table, nodeCredential, detailed}),
    mnesia:subscribe({table, enrollmentServerGroup, detailed}),
    mnesia:subscribe({table, enrollmentAuthority, detailed}),
    mnesia:subscribe({table, trustCategory, detailed}),
    mnesia:subscribe({table, enrollmentServer, detailed}),

    %% Check for existing trusted certifictes!!!
    %% Start expire timers!!!
    gen_server:cast(certServer, check_tc),
    {ok, #state{}}.

handle_call({get_tcat, Key}, _From, State) ->
    {ok, Result} = handle_get_tcat(Key),
    {reply, {ok, Result}, State};
handle_call({get_tcat_and_crlcheck, Key}, _From, State) ->
    {ok, TcatList, CrlCheck} = handle_get_tcat_and_crlcheck(Key),
    {reply, {ok, TcatList, CrlCheck}, State};
handle_call({get_tcat_certs_and_MoRefs, Key}, _From, State) ->
    {Result, Data} = handle_get_tcat_certs_and_MoRefs(Key),
    {reply, {Result, Data}, State};
handle_call({mk_verify_fun, TrustCategoryKey, InstancePid},
    _From, State) ->
    case handle_mk_verify_fun(TrustCategoryKey, InstancePid) of
        {error, Reason} ->
            {reply, {error, Reason}, State};
        Result ->
            {reply, Result, State}
    end;
handle_call({install_certificate, Fingerprint}, _From, State) when Fingerprint =:= "";
                                                                   Fingerprint =:= undefined ->
    Id = State#state.actionId,
    set_default_progress_report("installTrustedCertFromUri", Id),
    handle_fail({cleanup, [{resultInfo, "Fingerprint undefined"}]}),
    {reply, ok, State};
handle_call({install_certificate, Fingerprint}, _From, State) ->
    %% AI branch
    Id = State#state.actionId,
    set_default_progress_report("installTrustedCertFromUri", Id),
    action_handler(
        fun() ->
                case get(action) of
                    working ->
                        info_msg("Already working, please wait or cancel", []),
                        ok;
                    _ ->
                        handle_ai_install_certificate(Fingerprint)
                end
        end),
    {reply, ok, State#state{actionId=Id+1}};
handle_call(Request,From, State) ->
    {reply, ok, State}.

handle_cast(download_crl, State) ->
    Id = State#state.actionId,
    set_default_progress_report("downloadCrl", Id),
    action_handler(fun() -> certCrl:download_crl() end),
    {noreply, State#state{actionId=Id+1}};
handle_cast({install_certificate, Uri, Password, Fingerprint}, State) ->
    Id = State#state.actionId,
    set_default_progress_report("installTrustedCertFromUri", Id),
    action_handler(
        fun() ->
                case get(action) of
                    working ->
                        info_msg("Already working, please wait or cancel", []),
                        ok;
                    _ ->
                        put(action, working),
                        TRef =
                        erlang:send_after(180000, self(),
                            timeout_180),
                        put(timeout_180, TRef),
                        handle_install_certificate(Uri, Password, Fingerprint)
                end
        end),
    {noreply, State#state{actionId=Id+1}};
handle_cast({remove_certificate, MoRef}, State) ->
    Id = State#state.actionId,
    set_default_progress_report("removeTrustedCert", Id),
    action_handler(fun() -> 
			   handle_remove_certificate(MoRef) 
		   end),
    {noreply, State#state{actionId=Id+1}};
handle_cast(cancel, State) ->
    info_msg("Cancel~n", []),
    case get(retry_30_timer) of
        undefined ->
            ok;
        TRef1 ->
            erlang:cancel_timer(TRef1),
            put(retry_30_timer, undefined)
    end,
    case get(timeout_180) of
        undefined ->
            ok;
        TRef2 ->
            erlang:cancel_timer(TRef2),
            put(timeout_180, undefined)
    end,
    handle_cancelled(),
    {noreply, State};

handle_cast(check_tc, State) ->
    check_tc(),
    {noreply, State};
handle_cast({M,F,A}, State) ->
    apply(M,F,A),
    {noreply, State};
handle_cast(Request, State) ->
    {noreply, State}.

handle_info({mnesia_table_event, Event}, State) ->
    try handle_mnesia_table_event(Event)
    catch T:E ->
	    sysInitI:error_report(
	      [{mfa, {?MODULE, handle_mnesia_table_event, Event}},
	       {T,E},
	       erlang:get_stacktrace()])
    end,
    {noreply, State};
handle_info({expire_timeout, Level, Key, NotBefore, NotAfter}, State) ->
    handle_expire_timeout(Level, Key, NotBefore, NotAfter),
    {noreply, State};
handle_info({calc_timeout, Key, NotBefore, NotAfter}, State) ->
    handle_calc_timeout(Key, NotBefore, NotAfter),
    {noreply, State};
handle_info({handle_install_certificate, Uri, Passwd, Fingerprint}, State) ->
    handle_install_certificate(Uri, Passwd, Fingerprint),    
    {noreply, State};
handle_info(timeout_180, State) ->
    case get(retry_30_timer) of
        undefined ->
            ok;
        TRef ->
            info_msg("30 sec timer terminated by to 180 sec main timeout", []),
            erlang:cancel_timer(TRef),
            put(retry_30_timer, undefined),
            handle_fail([{additionalInfo, "Timeout, action stopped"},
                         {resultInfo,"No IP connectivity"}])
    end,
    put(timeout_180, undefined),
    put(action, undefined),
    {noreply, State};
handle_info(Request, State) ->
    {noreply, State}.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    ok.

ecoli_change_expiration_timer_frequency(Args) ->
    Res = 
    case Args of
        [] ->
            {ok, undefined};
        [Val] ->
            case catch list_to_integer(Val) of
                {'EXIT', _} ->
                    nok;
                X ->
                    {ok, X}
            end;
        _ ->
            nok
    end,
    case Res of
        {ok, T} ->
            AllKeys = mnesia:dirty_all_keys(nodeCredential),
            Fun =
                fun(K) ->
                    Name = certNcServer:make_process_name(K),
                    gen_server:cast(Name, {update_alarm_timer, K, T})
            end,
            lists:foreach(Fun, AllKeys),
            io:format("Timer changed~n");
        nok ->
            io:format("Nothing to do~n")
    end.

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% internal_function1(One, Two)->
%%    nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #--------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           check_tc()
%%% Input: -
%%% Output: -
%%% Exceptions: 
%%% Description: Check status of existing trusted certificates
%%%              at start of this procss.
%%% ----------------------------------------------------------
check_tc() ->
    check_tc(mnesia:dirty_first(trustedCertificate)),
    
    %% Clean expired CRLs
    case catch certCrl:clean_crls() of
        {'EXIT', Reason} ->
            info_msg("Clean CRLs failed, reason:~n~p", [Reason]);
        _ ->
            ok
    end,
    ok.

check_tc('$end_of_table') ->
    ok;
check_tc(Key) ->
    %% Start to expire timer
    case mnesia:dirty_read(certTC, Key) of
        [] ->
            ok;
        [TC] ->
            %% Clear expire state
            mnesia:dirty_write(TC#certTC{timeout = undefined}),
            case TC#certTC.cert of
                undefined ->
                    ok;
                Cert ->
                    start_to_expire_timer(Key, Cert)
            end
    end,
    check_tc(mnesia:dirty_next(trustedCertificate, Key)).

%%% ----------------------------------------------------------
%%% #           start_to_expire_timer(Key, Cert)
%%% Input: Key
%%%        Cert - in DER format
%%% Output: -
%%% Exceptions: 
%%% Description: Starts the to expire timer.
%%% ----------------------------------------------------------
start_to_expire_timer(Key, Cert) ->
    info_msg("Start to expire timer for tc ~p~n", [Key]),
    OTPCert = public_key:pkix_decode_cert(Cert, otp),
    TBS = OTPCert#'OTPCertificate'.tbsCertificate,
    V   = TBS#'OTPTBSCertificate'.validity,
    NotBefore = certLib:convert_cert_date(V#'Validity'.notBefore),
    NotAfter  = certLib:convert_cert_date(V#'Validity'.notAfter),
    handle_calc_timeout(Key, NotBefore, NotAfter).


%%% ----------------------------------------------------------
%%% #           action_handler(Fun, Progress)
%%% Input: Fun:fun() - Action to be called under supervision
%%% Output: 
%%% Exceptions: 
%%% Description: Performs the Fun and catches all errors with proper progress
%%%              reporting. It should also close any open sftp connections and
%%%              do whatever clean up that's necessary. In terms of error 
%%%              reporting, that is a section that can be much developed.
%%% ----------------------------------------------------------
action_handler(Fun) ->
    %% HW45665 cover faulty case to prevent crash
    try Fun() of
        ok -> ok;
        Error -> error_msg("Fun from action_handler returned ~p~n",[Error])
    catch
	throw:cancelled ->
	    handle_cancelled();
	throw:Throw ->
	    error_msg("throw ~p~n",[Throw]),
	    ProgressInfo = "The action could not be completed",
	    handle_fail({cleanup, [{progressInfo, ProgressInfo},
                     {resultInfo, ""}]});
	Type:Reason ->
	    ProgressInfo = "A software related error occured",
	    sysInitI:error_report([{Type, Reason}, 
				       erlang:get_stacktrace()]),
	    handle_fail({cleanup, [{progressInfo, ProgressInfo},
                     {resultInfo, ""}]})
    end.

%%% ----------------------------------------------------------
%%% #           handle_fail(UpdateList)
%%% Input: UpdateList:list
%%% Output: 
%%% Exceptions: 
%%% Description: This function reports actions that have failed
%%% ----------------------------------------------------------
handle_fail({cleanup, UpdateList}) ->
    handle_fail(UpdateList),
    cleanup();
handle_fail(UpdateList) when is_list(UpdateList) ->
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    CompleteFailList =  [{result, ?ActionResultType_FAILURE},
                        {state, ?ActionStateType_FINISHED},
                        {progressPercentage, 100},
                        {timeActionCompleted, CompleteTime}]
                        ++ UpdateList,
    update_progress(CompleteFailList).

%%% ----------------------------------------------------------
%%% #           handle_success(UpdateList)
%%% Input: UpdateList:list
%%% Output: 
%%% Exceptions: 
%%% Description: This function reports actions that have succeeded
%%% ----------------------------------------------------------
handle_success(UpdateList) when is_list(UpdateList) ->
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    CompleteSuccessList =  [{result, ?ActionResultType_SUCCESS},
                        {state, ?ActionStateType_FINISHED},
                        {progressPercentage, 100},
                        {timeActionCompleted, CompleteTime}]
                        ++ UpdateList,
    update_progress(CompleteSuccessList).

%%% ----------------------------------------------------------
%%% #           handle_cancelled(Target)
%%% Input: Target:certM|{nodeCredential, Key:string()}
%%% Output: 
%%% Exceptions: 
%%% Description: This function reports actions that have been cancelled
%%% ----------------------------------------------------------
handle_cancelled() ->
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress([{result, ?ActionResultType_NOT_AVAILABLE},
                     {resultInfo, ""},
		     {state, ?ActionStateType_CANCELLED},
		     {timeActionCompleted, CompleteTime}]),
    cleanup().

%%% ----------------------------------------------------------
%%% #           cleanup()
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: This function cleans up anything that needs cleaning up
%%% ----------------------------------------------------------
cleanup() ->
    put(action, undefined),
	cleanup(get(protocol)),
	
    case get(retry_30_timer) of
        undefined ->
            ok;
        TRef1 ->
            erlang:cancel_timer(TRef1),
            put(retry_30_timer, undefined)
    end,
    case get(timeout_180) of
        undefined ->
            ok;
        TRef2 ->
            erlang:cancel_timer(TRef2),
            put(timeout_180, undefined)
    end.
   

cleanup(sftp) ->
    case get(channelPid) of
        Pid when is_pid(Pid) ->
            case get(handle) of
                undefined ->
                    ok;
                _-> 
                    ftpI:close(sftp, Pid, get(handle))
            end,
            ftpI:stop_channel(sftp, Pid);
        undefined -> ok
    end,
    case get(connectionRef) of
	CRef when is_pid(CRef) -> ssh:close(CRef);
	undefined -> ok
    end;
cleanup(ftpes) ->
	case get(channelPid) of
	Pid when is_pid(Pid) ->
			%% close file if opened
			ftpI:close(ftpes, Pid, []),
			ftpI:stop_channel(ftpes, Pid);
	undefined -> ok
    end;
cleanup(_Other) ->
    ok.

update_progress(ProgressData) ->
    mnesia:transaction(fun() -> do_update_progress(ProgressData) end).

do_update_progress(ProgressData) ->
    Key = {certM, {"1","1","1","1"}},
    [Obj] = mnesia:read(Key),
    Current = Obj#certM.reportProgress,
    Old = 
	case Current of
	    undefined -> 
		warning_msg("No default progress report!~n"),
		default_progress_report("", 0);
	    Current -> Current
	end,
    Progress = comsaI:update_progress(ProgressData, Old),
    NewObj = Obj#certM{reportProgress=Progress},
    mnesia:write(NewObj).

set_default_progress_report(Action, Id) ->
    Key = {certM, {"1","1","1","1"}},
    mnesia:transaction(
      fun() ->
	      [Obj] = mnesia:read(Key),
	      Progress = default_progress_report(Action, Id),
	      NewObj = Obj#certM{reportProgress=Progress},
	      mnesia:write(NewObj)
      end).
    

default_progress_report(Action, Id) ->
    Time = comsaI:iso_time(os:timestamp(), extended),
    #'AsyncActionProgress'
	{actionName=Action,
	 additionalInfo=[""],
	 progressInfo = "Action started",
	 progressPercentage=0,
	 result=?ActionResultType_NOT_AVAILABLE,
	 resultInfo="",
	 state=?ActionStateType_RUNNING,
	 actionId=Id,
	 timeActionStarted = Time,
	 timeActionCompleted= "",
	 timeOfLastStatusUpdate= Time}.


handle_ai_install_certificate(Fingerprint) ->
    Fun = fun() ->
            Index = 
            case mnesia:all_keys(trustedCertificate) of
                [] -> "1";
                Keys ->
                    Ix = 
                    lists:max([list_to_integer(element(5, Key))||
                            Key<-Keys])+1,
                    integer_to_list(Ix)
            end,
            
            NewKey = {"1","1","1","1",Index},
            TC1 = 
            #trustedCertificate{
                trustedCertificateId=NewKey,
                certificateState = ?CertificateState_NOT_VALID_YET,
                managedState = ?ManagedCertificateState_DISABLED,
                reservedByCategory = []},
            mnesia:write(TC1),
            TC2 =
            #certTC{
                index = NewKey,
                fingerprint = certLib:decode_fingerprint(Fingerprint)},
            mnesia:write(TC2),
            NewKey
    end,
    {atomic, NewKey} = mnesia:transaction(Fun),
    handle_success([{resultInfo, mk_ext_moref(NewKey)}]),
    ok.


handle_install_certificate(Uri, Passwd, Fingerprint) ->
    info_msg("install_certificate, Uri: ~p~n", [Uri]),
    update_progress([{additionalInfoClear, "Try to download file from "++Uri},
                     {progressPercentage, 0},
                     {result, ?ActionResultType_NOT_AVAILABLE},
                     {resultInfo, ""},
                     {state, ?ActionStateType_RUNNING}]),
    case ftpI:parse_uri(Uri) of
        {ok, {Proto, User, Host, Port, Path, _Query}} when Proto =:= sftp; Proto =:= ftpes ->
            case {User, Host} of
                {[], _} -> 
                    error_msg("throw ~p~n",[faulty_uri]),
                    handle_fail({cleanup, [{resultInfo, "Missing user information in URI"}]});
                {_, []} -> 
                    error_msg("throw ~p~n",[faulty_uri]),
                    handle_fail({cleanup, [{resultInfo, "Missing host information in URI"}]});
                _ -> 
                    Result = start_channel(Proto, Host, Port, User, Passwd),
                    handle_install_ftp_certificate(Proto, Result, Path, Uri, Passwd,
                                                   Fingerprint)
            end;
        {ok, {http, _User, _Host, _Port, _Path, _Query}} ->
            handle_install_http_cert1(Uri, Fingerprint);
        {ok, {https, _User, _Host, _Port, _Path, _Query}} ->
            handle_install_http_cert1(Uri, Fingerprint);
        _ ->
            handle_fail({cleanup, [{resultInfo, "Wrong URI format"}]}),
            {error, faulty_uri}
    end.

%%% HTTP/HTTPS
handle_install_http_cert1(Url, Fingerprint) ->
    case certLib:resolve_uri(Url) of
        {ok, NewUrl} ->
            Method  = get,
            Request = {NewUrl, []},
            HTTPOptions = [
                {timeout, 30000},
                {connect_timeout, 30000},
                {ssl,[{verify,0}, {ciphers, comsaI:get_tls_cipher_suites()}]},
                {autoredirect, false}],
            Options = [{body_format, binary},{ipv6_host_with_brackets, true}],
            httpc:set_options([{ipfamily, certLib:get_inet()},
                    {socket_opts, ootI:get_all_oam_opt_list()}], cert),
            case catch httpc:request(Method, Request, HTTPOptions,
                    Options, cert) of
                {ok, {{_,200,_}, Body}} ->
                    handle_install_http_cert2({ok, Body}, Url, Fingerprint);
                {ok, {{_,200,_}, _Header, Body}} ->
                    handle_install_http_cert2({ok, Body}, Url, Fingerprint);
                {'EXIT', Reason} ->
                    info_msg("Reason: ~p~n", [Reason]),
                    handle_install_http_cert2({error, not_found},
                        Url, Fingerprint);
                A ->
                    info_msg("A: ~p~n", [A]),
                    handle_install_http_cert2({error, not_found}, Url,
                        Fingerprint)
            end;
        {error, Reason} ->
            info_msg("Resolve Uri ~p failed, reason: ~p", [Url, Reason]),
            handle_install_http_cert2({error, resolved_uri_failed}, Url,
                Fingerprint)
    end.

handle_install_http_cert2({error, Reason}, Url, Fingerprint) ->
    case get(timeout_180) of
        undefined ->
            info_msg("handle_install_http_cert2 180sec timeout, Url: ~p~n",
                [Url]),
            put(action, undefined),
            Msg =
            case Reason of
                resolved_uri_failed ->
                    "Resolve URI failed";
                _ ->
                    "No IP connectivity"
            end,
            update_progress([{resultInfo, Msg}]),
            ok;
        _ ->
            info_msg("handle_install_http_cert2 wait 30sec, Url: ~p~n", [Url]),
            update_progress([{progressInfo,
                        "Trying to connect to HTTP server..."}]),
            TRef = erlang:send_after(30000, self(),
                {handle_install_http_cert1, Url, Fingerprint}),
            put(retry_30_timer, TRef),
            ok
    end;
handle_install_http_cert2({ok, Body}, Url, Fingerprint) ->
    erlang:cancel_timer(get(timeout_180)),
    install_downloaded_cert(Body, Fingerprint).

%%% FTP
handle_install_ftp_certificate(Proto, {error, invalid_password}, Path, Uri, Passwd,
    Fingerprint) ->
    handle_fail({cleanup, [{resultInfo, "Invalid password"}]}),
    ok;
handle_install_ftp_certificate(Proto, {error, no_connectivity}, Path, Uri, Passwd,
    Fingerprint) ->
    case get(timeout_180) of
        undefined ->
            info_msg("install_certificate1 180sec timeout, Uri: ~p~n", [Uri]),
            put(action, undefined),
            update_progress([{additionalInfo,"No IP connectivity"}]),
            ok;
        _ ->
            info_msg("install_certificate1 wait 30sec, Uri: ~p~n", [Uri]),
            update_progress([{progressInfo,
                        "Trying to connect to " ++ protocol_to_text(Proto) ++" server..."}]),
            TRef = erlang:send_after(30000, self(),
                {handle_install_certificate, Uri, Passwd, Fingerprint}),
            put(retry_30_timer, TRef),
            ok
    end;
handle_install_ftp_certificate(Proto, {ok, Pid, CRef}, 
                               Path, Uri, Passwd, Fingerprint) -> 
    Result = ftpI:read_file(Proto, Pid, Path, 30000),
    ftpI:stop_channel( Proto, Pid, CRef),
    handle(Proto, Result, Fingerprint).


	
handle(Proto, Result, Fingerprint) ->
    erlang:cancel_timer(get(timeout_180)),
    case Result of
        {error,NoFileErr} when NoFileErr =:= no_such_file 
                          orelse NoFileErr =:= epath ->
            certLib:sec_log("",
                        "Download of trusted certificate via " ++ protocol_to_text(Proto) ++" failed," ++
                        "no such file"),
            error_msg("throw ~p~n",[ftp_timeout]),
            handle_fail(
                {cleanup, [{additionalInfo, "Download trusted certificate via " ++ protocol_to_text(Proto) ++
                        " failed, no such file"},
                    {resultInfo, "Download via "++ protocol_to_text(Proto) ++ " timeout"}]}),
            ok;
        {error, Reason} ->
            certLib:sec_log("",
                        "Download of trusted certificate via "++ protocol_to_text(Proto) ++" failed"),
            error_msg("throw ~p~n",[ftp_timeout]),
            handle_fail(
                {cleanup, [{additionalInfo, "Download trusted certificate via "++ protocol_to_text(Proto) ++
                        " failed"},
                    {resultInfo, "Download via "++ protocol_to_text(Proto) ++ " timeout"}]}),
            ok;
        {ok, Data} ->
            install_downloaded_cert(Data, Fingerprint)
    end.


install_downloaded_cert(Data, Fingerprint) ->
    update_progress([{additionalInfo, "File downloaded"},
            {progressPercentage, 50}]),
    Cert =
    case catch public_key:pem_decode(Data) of
        [{'Certificate',C,_}|_] ->
            C;
        [] -> % Might already be DER format
            case catch public_key:der_decode('Certificate', Data) of
                C1 when is_record(C1, 'Certificate') -> % Just for check
                    Data;
                _ ->
                    {error, faulty_certificate}
            end;
        _ ->
            {error, faulty_certificate}
    end, 
    
    case Cert of
        {error, faulty_certificate}->
            certLib:sec_log("",
                "Downloaded trusted certificate is faulty"),
            error_msg("throw ~p~n",[faulty_certificate]),
            handle_fail({cleanup, [{resultInfo, "The certificate is faulty"}]});
        _ ->
            case certVerify:verify_fingerprint(Cert, Fingerprint) of
                {no_match, ThisFingerprint} ->
                    certLib:sec_log("",
                                    "Downloaded trusted certificate fingerprint no match"),
                    error_msg("throw ~p~n",[fingerprint_mismatch]),
                    handle_fail(
                      {cleanup, [{additionalInfo, "Obtained fingerprint is: "++
                                      ThisFingerprint},
                                 {resultInfo, "The fingerprint does not match"}]});
                _ ->
                    update_progress([{additionalInfo, "Fingerprint matches"}]),
                    FP =
                    case Fingerprint of
                        undefined ->
                            Hash = certLib:get_fingerprint_support(),
                            crypto:hash(Hash, Cert);
                        _ ->
                            certLib:decode_fingerprint(Fingerprint)
                    end,
                    CC = certLib:read_cert_metadata(Cert),
                    Fun = fun() ->
                            Index = 
                            case mnesia:all_keys(trustedCertificate) of
                                [] -> "1";
                                Keys ->
                                    Ix = 
                                    lists:max([list_to_integer(element(5, Key))||
                                            Key<-Keys])+1,
                                    integer_to_list(Ix)
                            end,
                            NewKey = {"1","1","1","1",Index},
                            TC1 = 
                            #trustedCertificate{
                                trustedCertificateId=NewKey,
                                certificateContent=CC,
                                certificateState = ?CertificateState_NOT_VALID_YET,
                                managedState = ?ManagedCertificateState_DISABLED,
                                reservedByCategory = []},
                            mnesia:write(TC1),
                            TC2 =
                            #certTC{
                                index       = NewKey,
                                fingerprint = FP,
                                cert        = Cert},
                            mnesia:write(TC2),
                            NewKey
                    end,
                    {atomic, NewKey} = mnesia:transaction(Fun),
                    handle_success([{additionalInfo, "Installed Trusted Certificate"},
                                    {resultInfo, mk_ext_moref(NewKey)}]),
                    put(action, undefined),
                    %% Start expire timer
                    start_to_expire_timer(NewKey, Cert)
            end
    end,
    ok.

%%% ----------------------------------------------------------
%%% #          pre_install([DerCert|...]) 
%%% Input: -
%%% Output:  -
%%% Exceptions: 
%%% Description: Install of trusted certificate.
%%%              Used by G2 and normal VRCS CMPv2
%%% ----------------------------------------------------------
pre_install([]) ->
    ok;
pre_install(List) ->
    pre_install(undefined, List).

%%% ----------------------------------------------------------
%%% #          pre_install(TcatKey, [DerCert|...]) 
%%% Input: -
%%% Output:  -
%%% Exceptions: 
%%% Description: Install of trusted certificate.
%%%              R-VNFM uses this function, will also add
%%%              Trusted Certificates in the Trust Category
%%% ----------------------------------------------------------
pre_install(TcatKey, List) ->
    Hash = certLib:get_fingerprint_support(),
    {atomic, Keys} =
    mnesia:transaction(fun() -> mnesia:all_keys(trustedCertificate) end),
    CurrentCerts =
    lists:filtermap(
        fun(Key) ->
                case mnesia:transaction(fun() ->
                                mnesia:read(certTC, Key) end) of
                    {atomic, [Obj]} -> % Only existing if uri was empty string
                        F =
                        case Obj#certTC.cert of
                            undefined ->
                                missing;
                            Bin ->
                                crypto:hash(Hash, Bin)
                        end,
                        {true, {Key, Obj#certTC.fingerprint, F}};
                    _ ->
                        {true, {Key, undefined, missing}}
                end
        end, Keys),
    lists:foreach(
        fun(Cert) ->
                %info_msg("CurrentCerts: ~p~n", [CurrentCerts]),
                Fingerprint = crypto:hash(Hash, Cert),
                %info_msg("Cert fingerprint: ~p~n", [Fingerprint]),
                Res1 = lists:keysearch(Fingerprint, 2, CurrentCerts),
                Res2 = lists:keysearch(Fingerprint, 3, CurrentCerts),
                case {Res1, Res2} of
                    {{value,{K, Fingerprint, Fingerprint}}, _} ->
                        %% MO and binary exist, do nothing
                        info_msg("MO and binary exist, do nothing: ~p~n", [K]),
                        add_tc_to_tcat(K, TcatKey);
                    {{value,{K, Fingerprint, missing}},_} ->
                        %% MO exist, but no binary
                        info_msg("Installing TC ~p", [K]),
                        do_cmp_install(K, Cert, Fingerprint),
                        add_tc_to_tcat(K, TcatKey);
                    {{value,{K, Fingerprint,_}},_} ->
                        %% MO exist, but binary wrong, inconsistens problem
                        info_msg("Installed TC ~p not match fingerprint, " ++
                            "rewritten to fixit", [K]),
                         do_cmp_install(K, Cert, Fingerprint),
                         add_tc_to_tcat(K, TcatKey);
                     {_,{value,{K, undefined, _}}} ->
                        %% MO and binary exist, just add fingerprint to certTC
                        info_msg("MO and binary exist, just add " ++
                            "fingerprint to certTC: ~p~n", [K]),
                        AtomicFun =
                        fun() ->
                            NewTC =
                            case mnesia:read(certTC, K) of
                                [TC] ->
                                    TC#certTC{fingerprint = Fingerprint};
                                _ ->
                                    #certTC{
                                        index       = K,
                                        fingerprint = Fingerprint}
                            end,
                            mnesia:write(NewTC)
                        end,
                        mnesia:transaction(AtomicFun),
                        add_tc_to_tcat(K, TcatKey);
                    {_, {value,{K, Fingerprint,_}}} ->
                        %% MO exist, but binary wrong, inconsistens problem
                        info_msg("Installed TC ~p no match of fingerprint, " ++
                            "rewritten to fixit", [K]),
                        do_cmp_install(K, Cert, Fingerprint),
                        add_tc_to_tcat(K, TcatKey);
                    {_,_} ->
                        %% No MO and binary exist, make new install TC
                        Index = 
                        case mnesia:transaction(
                                fun() ->
                                        mnesia:all_keys(trustedCertificate)
                                end) of
                            {atomic, []} ->
                                "1";
                            {atomic, Keys1} ->
                                Ix = 
                                lists:max([list_to_integer(element(5, Key1))||
                                        Key1<-Keys1])+1,
                                integer_to_list(Ix);
                            _ ->
                                "1"
                        end,
                        NewKey = {"1","1","1","1",Index},
                        info_msg("No matching fingerprint, make new TC: ~p~n",
                            [NewKey]),
                        do_cmp_install(NewKey, Cert, Fingerprint),
                        add_tc_to_tcat(NewKey, TcatKey)
                end
        end, List).

do_cmp_install(Key, CertDer, Fingerprint) ->
    case certVerify:verify_tc(CertDer) of
        valid ->
            CC = certLib:read_cert_metadata(CertDer),
            AtomicFun =
            fun() ->
                TC1 =
                case mnesia:read(certTC, Key) of
                    [TC] ->
                        TC#certTC{fingerprint = Fingerprint,
                                  cert        = CertDer};
                    _ ->
                        #certTC{
                            index       = Key,
                            fingerprint = Fingerprint,
                            cert        = CertDer}
                end,
                mnesia:write(TC1)
            end,
            mnesia:transaction(AtomicFun),

            case mnesia:transaction(fun() -> mnesia:read(trustedCertificate, Key) end) of
                {atomic, [Obj]} -> % Already existing trusted certificate
                    TC2 =
                    Obj#trustedCertificate{
                        trustedCertificateId = Key,
                        certificateContent   = CC,
                        certificateState     = ?CertificateState_VALID,
                        managedState = ?ManagedCertificateState_ENABLED},
                    mnesia:transaction(fun() -> mnesia:write(TC2) end),
                    case Obj#trustedCertificate.managedState of
                        ?ManagedCertificateState_ENABLED ->
                            case Obj#trustedCertificate.reservedByCategory of
                                undefined -> % No configured
                                    ok;
                                ReservedByCategory ->
                                    lists:foreach(
                                        fun(MoRef) ->
                                                A =
                                                certLib:decode_moref(MoRef),
                                                case A of
                                                    error ->
                                                        ok;
                                                    {tcat, Key1} ->
                                                        certSub:trig(tcat, Key1)
                                                end
                                        end, ReservedByCategory)
                            end;
                        _ ->
                            %% Will be handled by mnesia event
                            ok
                    end;
                _ -> % New trusted cetificate
                    TC2 =
                    #trustedCertificate{
                        trustedCertificateId = Key,
                        certificateContent   = CC,
                        certificateState     = ?CertificateState_VALID,
                        managedState = ?ManagedCertificateState_ENABLED,
                        reservedByCategory   = []},
                    mnesia:transaction(fun() -> mnesia:write(TC2) end)
            end,

            %% If any old expire timer ongoing, cancel it
            case get({expire_timer_tc_ref, Key}) of
                undefined ->
                    ok;
                Ref ->            
                    erlang:cancel_timer(Ref)
            end,
            handle_success([{actionName, ""},
                            {additionalInfo, "Installed Trusted Certificate"},
                            {resultInfo, mk_ext_moref(Key)}]),
            %% Start (new) expire timer
            start_to_expire_timer(Key, CertDer);
        invalid ->
            info_msg("TC failed to be installed, key: ~p~n fingerprint: ~p~n",
                [Key, Fingerprint]),
            ok
    end.


add_tc_to_tcat(_, undefined) ->
    ok;
add_tc_to_tcat({_,_,_,_, TcIndex}, TcatKey) ->
    Fun = fun() ->
            [Obj] = mnesia:read(trustCategory, TcatKey),
            TcMoRefs = 
            case Obj#trustCategory.trustedCertificates of
                undefined -> % Might be the first time
                    [];
                List ->
                    List
            end,
            MoRef = "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1," ++
            "TrustedCertificate=" ++ TcIndex,
            case lists:member(MoRef, TcMoRefs) of
                true -> % Already exist
                    ok;
                false -> % Missing, add
                    NewObj =
                    Obj#trustCategory{trustedCertificates = TcMoRefs++[MoRef]},
                    mnesia:write(NewObj)
            end
    end,
    {atomic,_} = mnesia:transaction(Fun),
    ok.

%%% ----------------------------------------------------------
%%% #           handle_remove_certificate(MoRefBin)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Removes a trusted certificate
%%% ----------------------------------------------------------
handle_remove_certificate(MoRefBin) ->
    DnRev = lists:reverse(re:split(MoRefBin, "[,=]")),
    Key =  comsaGeneric:dnrev_to_key(DnRev),
    update_progress([{additionalInfoClear, "Removing "++binary_to_list(MoRefBin)}]),
    Fun = 
      fun() -> 
	      [TC] = mnesia:read({trustedCertificate, Key}),
	      case TC#trustedCertificate.reservedByCategory of
		  [] -> ok;
		  undefined -> ok;
		  Reservations ->
		      mnesia:abort({reservedByCategory, Reservations})
	      end,
	      mnesia:delete({trustedCertificate, Key})
      end,
    case mnesia:transaction(Fun) of
	{atomic, ok} ->
	    handle_success([]),
            case get({expire_timer_tc_ref, Key}) of
                undefined ->
                    ok;
                Ref ->
                    certAlarm:clear(tc, cert_to_expire, Key),
                    erlang:cancel_timer(Ref)
            end,
            certCrl:remove_cached_crl_by_key(Key),
            mnesia:dirty_delete(certTC, Key),
            %% Clear alarm
            certAlarm:clear(tc, cert_not_available, Key),
	    ok;
	{aborted, {reservedByCategory, Reservations}} ->
	    case length(Reservations) of
		1 ->
		    handle_fail({cleanup,
		      [{resultInfo, "This certificate is reserved by "++
			    hd(Reservations)}]}),
			error_msg("throw ~p~n",[{reservedByCategory, Reservations}]),
			ok;
		N ->
		    handle_fail({cleanup,
		      [{resultInfo, "This certificate is reserved by "++
			    integer_to_list(N)++" trust categories"}|
		       [{additionalInfo, "reservedByCategory "++MoRef}||
			   MoRef<-Reservations]]}),
		    error_msg("throw ~p~n",[{reservedByCategory, Reservations}]),
		    ok
	    end
    end.



%%% ----------------------------------------------------------
%%% #   handle_calc_timeout(Key, NotBefore, NotAfter)
%%% Input:  
%%% Output: 
%%% Exceptions: 
%%% Description: Start expire timer
%%% ----------------------------------------------------------
handle_calc_timeout(Key, NotBefore, NotAfter) ->
    %% HalfTime shall max be 3 mounth, else (NotAfter-NotBefore)/2
    {D1, T1} = calendar:time_difference(NotBefore, NotAfter),
    HalfTime =
    case round((D1*86400 + calendar:time_to_seconds(T1))/2) of
        X1 when X1 > 7884000 -> % Larger than 3 month
            7884000;
        X1 when X1 < 604800 ->
            604800;
        X1 ->
            X1
    end,

    {D2, T2} =
    calendar:time_difference(calendar:universal_time(), NotAfter),
    TimeLeft = D2*86400 + calendar:time_to_seconds(T2),


    {Level, Threshold} =
    case mnesia:dirty_read(certTC, Key) of
        [] -> % First time
            %io:format("Obj: []~n", []),
            {warning, HalfTime};
        [Obj] ->
            %io:format("Obj: ~p~n", [Obj]),
            case Obj#certTC.timeout of
                undefined -> % First time
                    {warning, HalfTime};
                warning ->
                    case round(HalfTime/3) of
                        X2 when X2 =< 604800 -> % Less than 1 week
                            {minor, 604800};
                        X2 ->
                            {minor, X2}
                    end;
                minor ->
                    case round(HalfTime/10) of
                        X2 when X2 =< 604800 -> % Less than 1 week
                            {major, 604800};
                        X2 ->
                            {major, X2}
                    end;
                major ->
                    {expired, TimeLeft};
                expired ->
                    {expired, 0}
            end
    end,

    Timeout =
    case TimeLeft - Threshold of
        X3 when X3 < 0 ->
            0;
        X3 ->
            X3
    end,

    %io:format("Timeout: ~p~n", [Timeout]),

    case Timeout of
        0 -> %% Cert is expired
            handle_expire_timeout(Level, Key, NotBefore, NotAfter);
        Sec when Sec < 86400 -> %% Will trig action at timeout
            TRef = erlang:send_after(Sec*1000, self(), {expire_timeout,
                    Level, Key, NotBefore, NotAfter}),
            put({expire_timer_tc_ref, Key}, TRef);
        _ -> %% Will make a re-calc at timeout, each 24 hour
            TRef = erlang:send_after(86400000, self(), {calc_timeout,
                    Key, NotBefore, NotAfter}),
            put({expire_timer_tc_ref, Key}, TRef)
    end.

%%% ----------------------------------------------------------
%%% #   handle_expire_timeout()   
%%% Input:  -
%%% Output: ok
%%% Exceptions: 
%%% Description: When the expire timer get hit
%%% ----------------------------------------------------------
handle_expire_timeout(warning, Key, NotBefore, NotAfter) ->
    %% Send alarm WARNING
    info_msg("ALARM WARNING: Trusted Certificate: ~p~n", [Key]),
    Msg = "Trusted Certificate need to be renewed",
    certAlarm:send(tc, cert_to_expire, Key, warning, Msg),
    case mnesia:dirty_read(certTC, Key) of
        [] ->
            mnesia:dirty_write(#certTC{index = Key, timeout = warning});
        [Obj] ->
            mnesia:dirty_write(Obj#certTC{timeout = warning})    
    end,
    %% Start 1/3 timer
    handle_calc_timeout(Key, NotBefore, NotAfter),
    ok;
handle_expire_timeout(minor, Key, NotBefore, NotAfter) ->
    %% Send alarm MINOR
    info_msg("ALARM MINOR: Trusted Certificate: ~p~n", [Key]),
    Msg = "Trusted Certificate need to be renewed",
    certAlarm:send(tc, cert_to_expire, Key, minor, Msg),
    [Obj] = mnesia:dirty_read(certTC, Key),
    mnesia:dirty_write(Obj#certTC{timeout = minor}), 
    %% 3. Start 1/10 timer
    handle_calc_timeout(Key, NotBefore, NotAfter),
    ok;
handle_expire_timeout(major, Key, NotBefore, NotAfter) ->
    %% Send alarm MAJOR
    info_msg("ALARM MAJOR: Trusted Certificate: ~p~n", [Key]),
    Msg = "Trusted Certificate need to be renewed",
    certAlarm:send(tc, cert_to_expire, Key, major, Msg),
    [Obj] = mnesia:dirty_read(certTC, Key),
    mnesia:dirty_write(Obj#certTC{timeout = major}), 
    %% 2. Start timer for system meltdown!!!!
    handle_calc_timeout(Key, NotBefore, NotAfter),
    ok;
handle_expire_timeout(expired, Key, _, _) ->
    %% System Meltdown!!!!
    info_msg("CRITICAL trusted certificate: ~p is now invalid~n", [Key]),
    certAlarm:clear(tc, cert_to_expire, Key),
    Msg = "Trusted Certificate does not exist!",
    certAlarm:send(tc, cert_not_available, Key, critical, Msg),
    [Obj] = mnesia:dirty_read(certTC, Key),
    mnesia:dirty_write(Obj#certTC{timeout = expired}),    
    %% Set trusted cert as expired
    [TC] = mnesia:dirty_read(trustedCertificate, Key),
    mnesia:dirty_write(TC#trustedCertificate{certificateState =
            ?CertificateState_EXPIRED}),
    ok.


%%% ----------------------------------------------------------
%%% #           handle_get_tcat(Key)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Get a list of all trusted certificate in a 
%%%              trust category
%%% ----------------------------------------------------------
handle_get_tcat(Key) ->
    case mnesia:dirty_read(trustCategory, Key) of
        [Obj] ->
            case Obj#trustCategory.trustedCertificates of
                [] ->
                    {ok, []};
                Certs ->
                    List =
                    lists:filtermap(
                        fun(Dn) ->
                                case get_tc(Dn) of
                                    not_found ->
                                        false;
                                    Bin ->
                                        {true, Bin}
                                end
                        end, Certs),
                    {ok, List}
            end;
        _ ->
            {ok, []}
    end.

%%% ----------------------------------------------------------
%%% #           handle_get_tcat_and_crlcheck(Key)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Get a list of all trusted certificate in a 
%%%              trust category and the crl check parameter
%%% ----------------------------------------------------------
handle_get_tcat_and_crlcheck(Key) ->
    case mnesia:dirty_read(trustCategory, Key) of
        [Obj] ->
            case Obj#trustCategory.trustedCertificates of
                [] ->
                    {ok, [], Obj#trustCategory.crlCheck};
                Certs ->
                    List =
                    lists:filtermap(
                        fun(Dn) ->
                                case get_tc(Dn) of
                                    not_found ->
                                        false;
                                    Bin ->
                                        {true, Bin}
                                end
                        end, Certs),
                    {ok, List, Obj#trustCategory.crlCheck}
            end;
        _ ->
            {ok, [], ?FeatureState_DEACTIVATED}
    end.

%%% ----------------------------------------------------------
%%% #           handle_get_tcat_certs_and_MoRefs(Key)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Get a list of all trusted certificate in a 
%%%              trust category and its Mo Refs
%%% ----------------------------------------------------------
handle_get_tcat_certs_and_MoRefs(Key) ->
    case mnesia:dirty_read(trustCategory, Key) of
        [Obj] ->
            case Obj#trustCategory.trustedCertificates of
                [] ->
                    {ok, []};
                Certs ->
                    List =
                    lists:filtermap(
                        fun(Dn) ->
                                case get_tc(Dn) of
                                    not_found ->
                                        false;
                                    Bin ->
                                        {true, {Dn, Bin}}
                                end
                        end, Certs),
                    {ok, List}
            end;
        _ ->
            {error, not_found}
    end.
%%% ----------------------------------------------------------
%%% #           get_tc(Dn)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Get a trusted certificate from DN.
%%% ----------------------------------------------------------
get_tc(Dn) ->
    case certLib:decode_moref(Dn) of
        {tc, Index} ->
            case handle_verify_peer2(Index) of
                false ->
                    not_found;
                {true, Bin} ->
                    Bin
            end;
        _ ->
            not_found
    end.  


%%% ----------------------------------------------------------
%%% #           handle_mk_verify_fun(TrustCategoryKey, InstancePid)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Make a verify_fun for OAM Security
%%% ----------------------------------------------------------
handle_mk_verify_fun(TrustCategoryKey, InstancePid) ->
    Fun = fun () -> mnesia:read(trustCategory, TrustCategoryKey) end,
    case mnesia:transaction(Fun) of
        {atomic, [Obj]} ->
            case Obj#trustCategory.trustedCertificates of
                [] ->
                    {error, not_found};
                Certs ->
                    List      = handle_verify_peer1(Certs),
                    CrlCheck  = Obj#trustCategory.crlCheck,
                    {VF, PCF} =
                    certVerify:mk_verify_fun(List, InstancePid, CrlCheck),
                    {ok, VF, PCF}
            end;
        _ ->
            {error, not_found}
    end.
 
handle_verify_peer1(TrustedCerts) ->
    lists:filtermap(
        fun(Dn) ->
                case certLib:decode_moref(Dn) of
                    {tc, Index} ->
                        handle_verify_peer2(Index);
                    _ ->
                        false
                end
        end, TrustedCerts).

handle_verify_peer2(Index) ->
    case mnesia:dirty_read(trustedCertificate, Index) of
        [Obj] ->
            CS = Obj#trustedCertificate.certificateState,
            MS = Obj#trustedCertificate.managedState,
            case {MS, CS} of
                {?ManagedCertificateState_ENABLED, ?CertificateState_VALID} ->
                    %io:format("TC VALID and ENABLED ~p~n", [Index]),
                    case mnesia:dirty_read(certTC, Index) of
                        [TC] ->
                            case TC#certTC.cert of
                                undefined ->
                                    false;
                                Bin ->
                                    %% Ska vara i DER format
                                    {true, Bin}
                            end;
                        _ ->
                            false
                    end;
                A ->
                    %io:format("TC Status: ~p~n", [A]),
                    false
            end;
        _ ->
            false
    end.


%%% ----------------------------------------------------------
%%% #           start_channel(Proto, Host, Port, User, Passwd)
%%% Input: 
%%% Output: Handle- sftp/ftpes file handle 
%%% Exceptions: throw(Error) as returned from ftpI:start_channel/6
%%% Description: Open a sftp/ftpes connection and start a channel
%%%              Store info as process variables for graceful shutdown 
%%%              when there are problems
%%% ----------------------------------------------------------
start_channel(Proto, Host, Port, User, undefined) ->
start_channel(Proto, Host, Port, User, "");
start_channel(Proto, Host, Port, User, Passwd) ->
    case ftpI:start_channel(Proto, Host, Port, User, Passwd,
            [{timeout, 30000},{connect_timeout, 30000}]) of
     {ok, SP, C} -> 
        case Proto of
             sftp ->
                   info_msg("~p~n",[ssh:connection_info(C,
                    [client_version, server_version, peer])]),
                    put(connectionRef, C);
             ftpes ->
                    info_msg("Started ftpes client : ~p~n", [SP])
         end, 
         put(protocol, Proto),
         put(channelPid, SP),
         {ok, SP, C};
     {error, E1} ->
        case E1 of
            "Unable to connect using the available authentication methods" ->
                {error, invalid_password};
            _ ->
                {error, no_connectivity}
        end
    end.

%%% ----------------------------------------------------------
%%% #           handle_mnesia_table_event
%%% Input: Event
%%% Output: 
%%% Exceptions: 
%%% Description: All subscribed mnesia table events are routed here 
%%% ----------------------------------------------------------
handle_mnesia_table_event(Event) ->			 
    case Event of
	{write, nodeCredential, NC, [], _} ->
	    Key = NC#nodeCredential.nodeCredentialId,
	    ChildSpec = certNcServer:make_child(Key),
	    case supervisor:start_child(certSuper, ChildSpec) of
		{ok, _} ->
		    ok;
		{ok, _, _} ->
		    ok;
		{error, Reason} ->
		    sysInitI:error_report(
		      [{?MODULE, handle_mnesia_table_event, [Event]},
		       {mfa, {supervisor, start_child, [certSuper, ChildSpec]}},
		       {error, Reason}])
	    end;
        {write, nodeCredential, NC, [OldNC], A} ->
            case NC#nodeCredential.renewalMode of
                ?RenewalMode_AUTOMATIC ->
                    ok;
                _ ->
	            Key = NC#nodeCredential.nodeCredentialId,
                    certAlarm:clear(nc, cert_enroll_failed, Key)
            end;
        {delete, nodeCredential, {_, Key}, _, _} ->
            %% Cancel enrollment if any
            {_,_,_,_, Index} = Key,
            certCMPv2:cancel(Index),
	    Name = certNcServer:make_process_name(Key),
            certSecStore:remove_nc_dir(Key),
            mnesia:dirty_delete(certNC, Key),
            mnesia:dirty_delete(certNcState, Key),
            %% Clear any alarms
            certAlarm:clear(nc, cert_not_available, Key),
            certAlarm:clear(nc, cert_to_expire, Key),
            certAlarm:clear(nc, cert_enroll_failed, Key),
	    ok = supervisor:terminate_child(certSuper, Name),
            ok = supervisor:delete_child(certSuper, Name);
	{write, trustedCertificate, New, [Old], _} ->
            Fun =
            fun() ->
                    case {Old#trustedCertificate.managedState,
                            New#trustedCertificate.managedState} of
                        {?ManagedCertificateState_DISABLED,
                            ?ManagedCertificateState_ENABLED} ->
                            %% disable -> enable
                            Index = New#trustedCertificate.trustedCertificateId,
                            [TC] = mnesia:read(certTC, Index),
                            case certVerify:verify_tc(TC#certTC.cert) of
                                valid ->
                                    %% Send event to subscribers of
                                    %% trusted category
                                    case New#trustedCertificate.reservedByCategory of
                                        undefined -> % No configured
                                            ok;
                                        ReservedByCategory ->
                                            lists:foreach(
                                                fun(MoRef) ->
                                                        A =
                                                        certLib:decode_moref(
                                                            MoRef),
                                                        case A of
                                                            error ->
                                                                ok;
                                                            {tcat, Key} ->
                                                                certSub:trig(
                                                                    tcat, Key)
                                                        end
                                                end, ReservedByCategory)
                                    end,
                                    mnesia:write(
                                        New#trustedCertificate{certificateState =
                                            ?CertificateState_VALID});
                                invalid ->
                                    ok
                            end;
                        {?ManagedCertificateState_ENABLED,
                            ?ManagedCertificateState_DISABLED} ->
                            %% enable -> disable
                            %% Send event to subscribers of trusted category
                            case New#trustedCertificate.reservedByCategory of
                                undefined -> % No configured
                                    ok;
                                ReservedByCategory ->
                                    lists:foreach(
                                        fun(MoRef) ->
                                                case certLib:decode_moref(
                                                        MoRef) of
                                                    error ->
                                                        ok;
                                                    {tcat, Key} ->
                                                        certSub:trig(tcat, Key)
                                                end
                                        end, ReservedByCategory),
                                    mnesia:write(
                                        New#trustedCertificate{
                                            certificateState =
                                            ?CertificateState_NOT_VALID_YET})
                            end;
                        {_,_} -> % Not changed
                            ok
                    end
            end,
            mnesia:transaction(Fun),
            ok;
        {delete, trustedCertificate, {_, Key}, [Old], _} ->
            ok;
        {write, trustCategory, New, [Old], _} ->
            case {Old#trustCategory.trustedCertificates,
                    New#trustCategory.trustedCertificates} of
                {A, A} -> % Not changed
                    ok;
                {OldMS, NewMS} ->
                    certSub:trig(tcat, New#trustCategory.trustCategoryId)
            end,
            ok;
        {delete, trustCategory, {_, Key}, [Old], _} ->
            ok;
        {write, enrollmentServerGroup, New, [Old], _} ->
            ok;
        {delete, enrollmentServerGroup, {_, Key}, [Old], _} ->
            ok;
        {write, enrollmentAuthority, New, [Old], _} ->
            ok;
        {delete, enrollmentAuthority, {_, Key}, [Old], _} ->
            ok;
        {write, enrollmentServer, New, [Old], _} ->
            ok;
        {delete, enrollmentServer, {_, Key}, [Old], _} ->
            ok;
	Event ->
	    %info_msg("Mnesia table event: ~p~n",[Event])
            ok
    end.


%%% ----------------------------------------------------------
%%% #          mk_ext_moref(Key)
%%% Input:  Key:tuple()
%%% Output: MoRef:string() - MoRef for external use
%%% Exceptions: 
%%% Description: Make a MoRef for external use by adding the
%%%              NetworkManagedElement ID
%%% ----------------------------------------------------------
mk_ext_moref({_,_,_,_,Id}) ->
    "ManagedElement=" ++ certLib:get_net_me_id() ++
    ",SystemFunctions=1,SecM=1,CertM=1,TrustedCertificate=" ++ Id.

%%%%%%%%%%%%%%%
protocol_to_text(Proto) ->
	atom_to_list(Proto).

%%% ----------------------------------------------------------
%%% INFO, WARNING and ERROR MESSAGE HANDLING
%%% ----------------------------------------------------------
info_msg(Format, Args) ->
   sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

warning_msg(Format) ->
   warning_msg(Format, []).
warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

