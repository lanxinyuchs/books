%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certNcServer.erl %
%%% @author emajkaa
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/9
%%% 
%%% @doc ==Main server module for CERT==
%%% This module implements the main CERT server 

-module(certNcServer).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/9').
-date('2017-12-06').
-author('emajkaa').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
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
%%% -----   ---------- --------  ------------------------
%%% R2A/1   2013-08-08 etxjotj   Created
%%% R5A/25  2016-04-18 uabhgma   Added clear alarm on successful get
%%% R5A/26  2016-05-11 ehsake    HU82643, redundancy not working for host URI.
%%% R6A/6   2016-08-30 emariad   CSUC feature, cipher configuration
%%% R7A/2   2016-10-13 etxasta   Fixed subjectAltName fault
%%% R7A/3   2016-10-14 etxasta   SERIALNUMBER for subjectName
%%% R7A/4   2016-10-19 ekurnik   HV33549 - OOT not avilable at startup
%%% R7A/5   2016-10-20 etxasta   Fixed bug in build_rdnSequence 
%%% R7A/8   2016-11-02 etxasta   Fixed bug in do_encode_sub_alt_name 
%%% R8A/1   2016-11-07 etomist   HV38684
%%% R8A/2   2016-11-16 etxasta   Added storage of CMPv2 shared secret
%%% R8A/4   2016-11-23 etxasta   Now using certCrypto:decode_pkcs12/2
%%% R8A/5   2017-01-16 eivmiha   switched ssh_sftp and sysSftp to ftpI
%%% R9A/2   2017-02-02 estjako   Added support for ftpes
%%% R9A/3   2017-02-03 etxasta   Don't check in code that can't compile
%%% R9A/5   2017-02-16 etomist   HV62945 (EC fix), HV64748 (IPv6 address fix)
%%% R9A/6   2017-02-22 etxasta   Moved pkcs12 to certPkcs12.erl
%%% R9A/7   2017-03-02 etomist   HV57826 (cert expiry alarm redesign)
%%% R9A/10  2017-03-20 eivomat   HV70387
%%% R9A/11  2017-03-28 ebabmat   HV75812
%%% R9A/12  2017-03-39 ebabmat   Additional fix for HV75812
%%% R9A/13  2017-04-06 ebabmat   HV78468
%%% R10A/1  2017-04-25 eivomat   HV82668
%%% R10A/2  2017-04-26 eivomat   HV80444
%%% R10A/3  2017-05-10 etxasta   Added ext_start_to_expire_timer/2
%%% R10A/4  2017-05-12 eivomat   HV81235
%%% R10A/5  2017-05-15 eivomat   HV87861
%%% R10A/6  2017-05-18 etomist   HV89802
%%% R10A/7  2017-05-24 emajkaa   HV90577
%%% R10A/8  2017-05-25 ebabmat   HV90860
%%% R10A/9  2017-06-02 ebabmat   HV92468
%%% R11A/1  2017-07-26 emajkaa   HW15696
%%% R11A/2  2017-07-28 emajkaa   HW16509
%%% R11A/3  2017-08-02 emajkaa   HW17544
%%% R11A/4  2017-08-25 emajkaa   HW22156
%%% R11A/5  2017-09-14 emajkaa   HW28496
%%% R11A/6  2017-09-14 etxasta   Changed do_renewal1
%%% R11A/7  2017-09-20 emajkaa   HW30059
%%% R11A/8  2017-09-26 emajkaa   HW31400
%%% R11A/10 2017-10-10 etxasta   SP680
%%% R11A/11 2017-10-13 emarnek   HW33598
%%% R12A/1  2017-11-04 eivirad   HW39460 and HW39468
%%% R12A/2  2017-11-04 eivirad   Revert HW39460 and HW39468 due to faulty TCs in certm suite
%%% R12A/3  2017-11-06 eivirad   HW39460 and HW39468
%%% R12A/4  2017-11-20 emajkaa   HW45658
%%% R12A/6  2017-11-22 etxasta   Updated get_src_if/1 for rvnfm
%%% R12A/7  2017-11-29 eivomat   HW48136
%%% R12A/8  2017-12-06 emarnek   HW49696
%%% R12A/9  2017-12-06 emajkaa   HW49906

%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%% Called from supervisor
-export([start/1]).

%%% Called from certDataInit

-export([children/0]).

%%% Called from certServer
-export([make_child/1, make_process_name/1]).

%%% Action handling
%%% Actions on NodeCredential MOC
-export([cancel_enrollment/1, install_credential/5, 
     start_offline_enrollment/3, start_online_enrollment/2,
     start_online_enrollment/3]).

%%% CMPv2 update status
-export([update_status/3]).

%%% certI functions
-export([get_nc_der/1]).

-export([gen_csr/3]).

-export([check_nc/2]).
-export([test/1]).
-export([test/2]).

-export([get_enrollment_servers/1]).

-export([get_next_chain_key/2]).

-export([get_chain_cc/2]).

-export([ext_start_to_expire_timer/2]).

-export([restart_expiration_timer/1]).

-export([get_src_if/1]).

-compile(nowarn_unused_vars).
%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).


-include_lib("public_key.hrl").
-include("RcsCertM.hrl").
-include("cert.hrl").

-define(MINUTE_IN_SECONDS, 60).
-define(HOUR_IN_SECONDS, 60 * 60).
-define(DAY_IN_SECONDS, 24 * 60 * 60).
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
test(Key) ->
    Name = make_process_name(Key),
    gen_server:cast(Name, {check_nc, Key, false}).

%%% ----------------------------------------------------------
%%% @doc Cancels the enrollment of a node credential
%%% @end
%%% ----------------------------------------------------------
cancel_enrollment(Key) ->
    Name = make_process_name(Key),
    gen_server:cast(Name, cancel_enrollment).

%%% ----------------------------------------------------------
%%% @doc Installs a node credential to be fetched from the uri location
%%% @end
%%% ----------------------------------------------------------
install_credential(Key, Uri, UriPwd, CredPwd, Fingerprint) ->
    Name = make_process_name(Key),
    AI   =
     case catch aicI:is_ai_ongoing() of
         {'EXIT', Reason} ->
             info_msg("AI, return true, AI crash reason: ~p", [Reason]),
             true;
         Res ->
             info_msg("AI: ~p", [Res]),
             Res
     end,
    gen_server:cast(Name,
        {install_credential, Uri, UriPwd, CredPwd, Fingerprint, AI}).


%%% ----------------------------------------------------------
%%% @doc Starts offline CSR enrollment
%%% @end
%%% ----------------------------------------------------------
start_offline_enrollment(Key, Uri, Password) ->
    Name = make_process_name(Key),
    gen_server:cast(Name, {start_offline_enrollment, Uri, Password}).

%%% ----------------------------------------------------------
%%% @doc external start of expire timer, used by R-VNFM
%%% @end
%%% ----------------------------------------------------------
ext_start_to_expire_timer(Key, Cert) ->
    Name = make_process_name(Key),
    gen_server:cast(Name, {ext_start_to_expire_timer, Key, Cert}).

%%% ----------------------------------------------------------
%%% @doc Starts online CSR enrollment
%%% @end
%%% ----------------------------------------------------------
start_online_enrollment(Key, Challenge) ->
    start_online_enrollment(Key, Challenge, oam).

start_online_enrollment(Key, Challenge, SrcIf) ->
    Name = make_process_name(Key),
    AI   =
    case catch aicI:is_ai_ongoing() of
        {'EXIT', Reason} ->
            info_msg("AI, return true, AI crash reason: ~p", [Reason]),
            true;
        Res ->
            info_msg("AI: ~p", [Res]),
            Res            
    end,
    gen_server:cast(Name, {start_online_enrollment, AI, Challenge, SrcIf}).

%%% ----------------------------------------------------------
%%% @doc Get node credential
%%% @end
%%% ----------------------------------------------------------
get_nc_der(Key) ->
    case mnesia:dirty_read(certNC, Key) of
        [] ->
            {error, not_found};
        [Obj] ->
            case Obj#certNC.cert of
                undefined ->
                    Msg = "Node Credential does not exist!",
                    certAlarm:send(nc, cert_not_available, Key, critical,Msg),
                    {error, not_found};
                Cert ->
                    case certSecStore:get_nc_key(Key, "nc.key") of
                        {ok, KeyBin} ->
                            certAlarm:clear(nc, cert_not_available, Key),
                            {ok, Cert, binary_to_term(KeyBin)};
                        _ ->
                            Msg = "Node Credential does not exist!",
                            certAlarm:send(nc, cert_not_available,
                                Key, critical, Msg),
                            {error, not_found}
                    end
            end
    end.

%%% ----------------------------------------------------------
%%% @doc Get next node credential chain key
%%% @
%%% ----------------------------------------------------------
get_next_chain_key(NcKey, undefined) ->
    do_get_next_chain_key(NcKey, 1);
get_next_chain_key(NcKey, {"1","1","1","1","1",ChainId}) ->
    do_get_next_chain_key(NcKey, list_to_integer(ChainId) + 1);
get_next_chain_key(NcKey, ChainId) ->
    do_get_next_chain_key(NcKey, ChainId + 1).


do_get_next_chain_key(NcKey, Nth) ->
    case get_nc_cert_list(NcKey) of
        List when is_list(List) ->
            case length(List) of
                Value when Value > Nth ->
                    {ok, {"1","1","1","1","1",integer_to_list(Nth)}};
                _ ->
                    {ok, undefined}
            end;
        _ ->
            {ok, undefined}
    end.

get_chain_cc(NcKey, Nth) ->
    List = get_nc_cert_list(NcKey),
    %% +1 since the first is node cert, not chain cert
    ChainCert = lists:nth(Nth+1,List),
    certLib:read_cert_metadata(ChainCert).


get_nc_cert_list(Key) ->
    case mnesia:dirty_read(certNC, Key) of
        [] ->
            [];
        [Obj] ->
            case Obj#certNC.cert of
                undefined ->
                    [];
                Cert ->
                    Cert
            end
    end.


%% Add node cert as one chain for MOM display
test(Key, ChainId) ->
    {ok, [Cert|_],_} = get_nc_der(Key),
    ChainKey = list_to_tuple(tuple_to_list(Key) ++ [ChainId]),
    mnesia:dirty_write(#chainCertificate{
            chainCertificateId = ChainKey,
            certificateContent = certLib:read_cert_metadata(Cert),
            certificateState   = ?CertificateState_VALID}).

%%% ----------------------------------------------------------
%%% @doc Update status from e.g. CMPv2
%%% @end
%%% ----------------------------------------------------------
update_status(Index, Type, Data) when is_integer(Index) ->
    Name = make_process_name({"1","1","1","1", integer_to_list(Index)}),
    gen_server:cast(Name, {update_status, Type, Data});
update_status(Index, Type, Data) ->
    Name = make_process_name({"1","1","1","1", Index}),
    gen_server:cast(Name, {update_status, Type, Data}).

restart_expiration_timer([Id, <<"NodeCredential">> | _]) ->
    Key = {"1", "1", "1", "1", binary_to_list(Id)},
    case mnesia:dirty_read(certNC, Key) of
        [] ->
            ok;
        [Cert] ->
            mnesia:dirty_write(Cert#certNC{timeout = undefined}),
            Name = make_process_name(Key),
            gen_server:cast(Name, {restart_to_expire_timer, Key})
    end.
%%% ----------------------------------------------------------
%%% @doc Starts the certNcServer server process
%%% @end
%%% ----------------------------------------------------------
start(Key) ->
    ServerName = {local, make_process_name(Key)},
    Module = ?MODULE,
    Args = [Key],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

%%% ----------------------------------------------------------
%%% @doc Makes child specs for the certNcServers
%%% @end
%%% ----------------------------------------------------------
children() ->
    info_msg("Bring the children to life again", []),
    Fun = 
    fun() ->
        Keys = mnesia:all_keys(nodeCredential),
        [make_child(Key)||Key<-Keys]
    end,
    case mnesia:transaction(Fun) of
    {atomic, Children} ->
        Children;
    {aborted, {no_exists, _}=Reason} ->
        sysInitI:warning_report([{certNcServer, children},
                     {aborted, Reason}]),
        spawn(fun() -> start_children(900) end),
        [];
    {aborted, Reason} ->
        sysInitI:error_report([{certNcServer, children},
                       {aborted, Reason}]),
        []
    end.    

start_children(0) ->
    ok;
start_children(N) ->
    Fun = 
    fun() ->
        Keys = mnesia:all_keys(nodeCredential),
        [make_child(Key)||Key<-Keys]
    end,
    case mnesia:transaction(Fun) of
    {atomic, Children} ->
        [case supervisor:start_child(certSuper, Child) of
         {ok, _} ->
             ok;
         {ok, _, _} ->
             ok;
         {error, E} ->
             sysInitI:error_report(
              [{?MODULE, start_children, [N]},
               {mfa, {supervisor, start_child, [certSuper, Child]}},
               {error, E}])
         end||Child<-Children],
        sysInitI:info_msg("start_children complete~n");
        
    {aborted, Reason} ->
        sysInitI:warning_report([{certNcServer, start_children},
                     {aborted, Reason}]),
        timer:sleep(1000),
        start_children(N-1)
    end.

%%% ----------------------------------------------------------
%%% @doc Makes child specs for a specific node certificate
%%% @end
%%% ----------------------------------------------------------
make_child(Key) ->
    Id = make_process_name(Key),
    StartFunc = {certNcServer, start, [Key]},
    {Id, StartFunc, permanent, 3000, worker, [?MODULE]}.

    
%%% ----------------------------------------------------------
%%% @doc Generates a process name from the nodeCertficateId value
%%% @end
%%% ----------------------------------------------------------
make_process_name(Key) ->
    list_to_atom("certNcServer."++element(5,Key)).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
-record(state, {actionId=1, key}).

init(Args) ->
    [Key] = Args,
    process_flag(trap_exit, true),
    put(key, Key),
    
    %% Check status of existing nc at start of this child process
    Name = make_process_name(Key),
    AI =
    case catch aicI:is_ai_ongoing() of
        {'EXIT', Reason} ->
            info_msg("AI, return true, AI crash reason: ~p", [Reason]),
            true;
        Res ->
            info_msg("AI check_nc: ~p", [Res]),
            Res
    end,
    gen_server:cast(Name, {check_nc, Key, AI}),
    {ok, #state{key=Key}}.



handle_call(Request,From, State) ->
    {reply, ok, State}.

handle_cast({update_status, Type, Data}, State) ->
    handle_update_status(Type, Data),
    {noreply, State};
handle_cast(cancel_enrollment, State) ->
    Id = State#state.actionId,
    set_default_progress_report("cancelEnrollment", Id),
    action_handler(
        fun() ->
                handle_cancel_enrollment()
        end),
    {noreply, State#state{actionId=Id+1}};
handle_cast({install_credential, Uri, UriPwd, CredPwd, Fingerprint, AI},
    State) ->
    Id = State#state.actionId,
    set_default_progress_report("installCredentialFromUri",Id),
    action_handler(
        fun() ->
            %% Number of tries, used by CMPv2
            put(number_of_tries, undefined),
            handle_install_credential(Uri, UriPwd, CredPwd, Fingerprint, AI)
        end),
    {noreply, State#state{actionId=Id+1}};
handle_cast({start_offline_enrollment, Uri, Password}, State) ->
    Id = State#state.actionId,
    set_default_progress_report("startOfflineCsrEnrollment",Id),
    action_handler(
        fun() ->
            handle_start_offline_enrollment(Uri, Password)
        end),
    {noreply, State#state{actionId=Id+1}};
handle_cast({start_online_enrollment, AI, Challenge, SrcIf}, State) ->
    Id = State#state.actionId,
    set_default_progress_report("startOnlineEnrollment", Id),
    action_handler(
        fun() ->
            %% Number of tries, used by CMPv2
            put(number_of_tries, undefined),
            put(src_if, SrcIf),
            case check_online_enrollment_mandatory_params() of
                ok ->
                    handle_start_online_enrollment(AI, Challenge);
                {fail, Msg} ->
                    enrollment_failure_update(Msg)
                end
        end),
    {noreply, State#state{actionId=Id+1}};
handle_cast({check_nc, Key, AI}, State) ->
    check_nc(Key, AI),
    {noreply, State};
handle_cast({restart_to_expire_timer, Key}, State) ->
    restart_to_expire_timer(Key),
    {noreply, State};
handle_cast({update_alarm_timer, Key, Value}, State) ->
    case mnesia:dirty_read(certNcState, Key) of
        [] ->
            mnesia:dirty_write(#certNcState{key = Key, alarm_timer_value = Value});
        [Obj] ->
            mnesia:dirty_write(Obj#certNcState{alarm_timer_value = Value})
    end,
    restart_to_expire_timer(Key),
    {noreply, State};
handle_cast({ext_start_to_expire_timer, Key, Cert}, State) ->
    start_to_expire_timer(Key, Cert),
    {noreply, State};
handle_cast(Request, State) ->
    {noreply, State}.

handle_info({expire_timeout, Level, Key, NotBefore, NotAfter}, State) ->
    handle_expire_timeout(Level, Key, NotBefore, NotAfter),
    {noreply, State};
handle_info(enrollment_timeout, State) ->
    handle_enrollment_timeout(),
    {noreply, State};
handle_info({make_enrollment, Csr, Timer, AuthorityName, SubjectName,
        Key, URI, EnrollCaCert, EnrollCaFingerpr}, State) ->
    case get(uri_resolution_timer_ref) of
        undefined -> %% Already handled or cleared
            ok;
        _ ->
            make_enrollment(Csr, Timer, AuthorityName, SubjectName,
                Key, URI, EnrollCaCert, EnrollCaFingerpr)               
    end,
    {noreply, State};
handle_info({retry_cmp, Key, Type, FailURI, NC}, State) ->
    case get(retry_cmp_timer_ref) of
        undefined ->  %% Already handled or cleared
            ok;
        _ ->
            put(retry_cmp_timer_ref, undefined),
            do_renewal2(Key, Type, FailURI, NC)
    end,
    {noreply, State};
handle_info({calc_timeout, Key, NotBefore, NotAfter}, State) ->
    handle_calc_timeout(Key, NotBefore, NotAfter),
    {noreply, State};
handle_info({retry_install, Uri, UriPwd,
        CredPwd, Fingerprint, OffCsr, OffPkcs12}, State) ->
    handle_install_cred0(Uri, UriPwd, CredPwd, Fingerprint, OffCsr, OffPkcs12),
    {noreply, State};
handle_info({handle_start_offline_enrollment3, Csr, Uri, Password}, State) ->
    handle_start_offline_enrollment3(Csr, Uri, Password),
    {noreply, State};
handle_info(timeout_180, State) ->
    case get(retry_30_timer) of
        undefined ->
            ok;
        TRef ->
            erlang:cancel_timer(TRef),
            put(retry_30_timer, undefined),
            handle_fail([{progressInfo, "Timeout, action stopped"},
                         {resultInfo, "No IP connectivity"}])
    end,
    put(timeout_180, undefined),
    {noreply, State};
handle_info(Request, State) ->
    {noreply, State}.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    info_msg("terminate(~p, ~p)~n",[Reason, State]),
    ok.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           check_nc(Key, AI)
%%% Input: -
%%% Output: -
%%% Exceptions: 
%%% Description: Check status of existing nc's at start up of
%%%              this child process.
%%% ----------------------------------------------------------
check_nc(Key, AI) ->
    %% Number of tries, used by CMPv2
    put(number_of_tries, undefined),
    case mnesia:dirty_read(certNC, Key) of
        [Obj] ->
            info_msg("Check NC, key: ~p, AI: ~p", [Key, AI]),
            %% Clear expire state
            mnesia:dirty_write(Obj#certNC{timeout = undefined,
                                          ai      = AI}),
            [NC] = read_node_credential(Key),
            mnesia:dirty_write(NC#nodeCredential{certificateState =
                    ?CertificateState_NOT_VALID_YET}),
            %% Check NC action
            check_nc(Key, Obj#certNC.action, Obj#certNC.offline_p12,
                Obj#certNC.cert);
        _ ->
            ok
    end.

%% CMPv2 was ongoing
check_nc(Key, online_enrollment, _,_) ->
    info_msg("NC online enrollment not complete, force renewal (~p)~n", [Key]),
    force_renewal(Key),
    ok;

%% CSR or install offline_enrollment was ongoing
check_nc(Key, offline_enrollment, undefined, undefined) ->
    info_msg("Check nc action offline enrollment failed (~p), cert missing~n",
        [Key]),
    handle_fail({cleanup, [{progressInfo, "Action offline enrollment failed"}]}),
    Msg = "Node Credential does not exist!",
    certAlarm:send(nc, cert_not_available, Key, critical, Msg);
check_nc(Key, offline_enrollment, undefined, Cert) ->
    info_msg("CSR enrollment failed (~p)~n", [Key]),
    handle_fail({cleanup, [{progressInfo, "Action offline enrollment failed"}]}),
    case check_cert_key(Cert, Key) of
        valid ->
            start_to_expire_timer(Key, Cert);
        failed ->
            info_msg("NC validate in check_nc failed~n", []),
            Msg = "Node Credential does not exist",
            certAlarm:send(nc, cert_not_available, Key, critical, Msg),
            [Obj] = mnesia:dirty_read(certNC, Key),
            mnesia:dirty_write(Obj#certNC{cert = undefined})
    end;

%% Pkcs#12 has been used before and might solve this
check_nc(Key, offline_enrollment, OfflineP12, undefined) ->
    info_msg("Check nc pkcs12 (~p), cert missing, try re-install~n", [Key]),
    {Uri, UriPwd, CredPwd, Fingerprint} = OfflineP12,
    install_credential(Key, Uri, UriPwd, CredPwd, Fingerprint);
check_nc(Key, offline_enrollment, OfflineP12, Cert) ->
    info_msg("Check nc pkcs12 (~p)~n", [Key]),
    case check_cert_key(Cert, Key) of
        valid ->
            start_to_expire_timer(Key, Cert);
        failed ->
            info_msg("NC validate in check_nc failed~n", []),
            Msg = "Node Credential private key does not exist!",
            certAlarm:send(nc, cert_not_available, Key, critical, Msg),
            [Obj] = mnesia:dirty_read(certNC, Key),
            mnesia:dirty_write(Obj#certNC{cert = undefined}),
            {Uri, UriPwd, CredPwd, Fingerprint} = OfflineP12,
            install_credential(Key, Uri, UriPwd, CredPwd, Fingerprint)
    end;

%% No action was ongoing
check_nc(Key, no_action, undefined, undefined) ->
    info_msg("Check nc no_action (~p), cert missing", [Key]),
    Msg = "Node Credential does not exist!",
    certAlarm:send(nc, cert_not_available, Key, critical, Msg);
check_nc(Key, no_action, undefined, Cert) ->
    info_msg("Check nc no_action (~p)", [Key]),
    case check_cert_key(Cert, Key) of
        valid ->
            start_to_expire_timer(Key, Cert);
        failed ->
            info_msg("NC validate in check_nc failed", []),
            Msg = "Node Credential private key does not exist!",
            certAlarm:send(nc, cert_not_available, Key, critical, Msg),
            [Obj] = mnesia:dirty_read(certNC, Key),
            mnesia:dirty_write(Obj#certNC{cert = undefined}),
            %% Try to use CMPv2 renewal to fix this,
            %% might be a new board
            force_renewal(Key)
    end;

%% Pkcs12 install might have been ongoing, at least it has been used before
check_nc(Key, no_action, OfflineP12, undefined) ->
    info_msg("Check nc pkcs12 (~p), cert missing~n", [Key]),
    %% Try to use install pkcs12 to fix this, might be a new board
    {Uri, UriPwd, CredPwd, Fingerprint} = OfflineP12,
    install_credential(Key, Uri, UriPwd, CredPwd, Fingerprint);
check_nc(Key, no_action, OfflineP12, Cert) ->
    info_msg("Check nc pkcs12 (~p)~n", [Key]),
    case check_cert_key(Cert, Key) of
        valid ->
            start_to_expire_timer(Key, Cert);
        failed ->
            info_msg("NC validate in check_nc failed~n", []),
            Msg = "Node Credential private key does not exist!",
            [Obj] = mnesia:dirty_read(certNC, Key),
            mnesia:dirty_write(Obj#certNC{cert = undefined}),
            certAlarm:send(nc, cert_not_available, Key, critical, Msg),
            %% Try to use install pkcs12 to fix this,
            %% might be a new board
            {Uri, UriPwd, CredPwd, Fingerprint} = OfflineP12,
            install_credential(Key, Uri, UriPwd, CredPwd, Fingerprint)
    end.

check_cert_key(Cert, Key) ->
    case do_check_cert_key(Cert, Key, "nc.key") of
        {valid, _} ->
            valid;
        {not_valid, NcKey} ->
            case do_check_cert_key(Cert, Key, "nc_old.key") of
                {valid, NcOldKey} ->
                    %% switch nc och nc_old key
                    info_msg("Switch new and old nc key ~p", [Key]),
                    certSecStore:remove_nc_key(Key, "nc.key"),
                    certSecStore:remove_nc_key(Key, "nc_old.key"),
                    certSecStore:put_nc_key(Key, NcOldKey, "nc.key"),
                    certSecStore:put_nc_key(Key, NcKey, "nc_old.key"),
                    valid;
                _ ->
                    failed
            end;
        missing ->
            case do_check_cert_key(Cert, Key, "nc_old.key") of
                {valid, NcOldKey} ->
                    %% write as nc and remove nc_old key
                    info_msg("Using old nc key ~p", [Key]),
                    certSecStore:remove_nc_key(Key, "nc_old.key"),
                    certSecStore:put_nc_key(Key, NcOldKey, "nc.key"),
                    valid;
                _ ->
                    failed
            end

    end.

do_check_cert_key([Cert|_], Key, File) ->
    case certSecStore:get_nc_key(Key, File) of
        {ok, PrivKeyBin} ->
            PrivKey = binary_to_term(PrivKeyBin),
            case certVerify:verify_nc(Cert, PrivKey) of
                valid ->
                    {valid, PrivKeyBin};
                {failed, Reason} ->
                    info_msg("Check nc key ~p failed, ~p~n", [Key, Reason]),
                    {not_valid, PrivKeyBin}
            end;
        _ ->
            missing
    end.


%%% ----------------------------------------------------------
%%% #          start_to_expire_timer(Key, Cert) 
%%% Input: Key
%%%        CertList - nc cert list in DER format
%%% Output: -
%%% Exceptions: 
%%% Description: Start to expire timer.
%%% ----------------------------------------------------------
start_to_expire_timer(Key, [Cert|_]) ->
    info_msg("NC start to expire timer, ~p~n", [Key]),
    OTPCert = public_key:pkix_decode_cert(Cert, otp),
    TBS = OTPCert#'OTPCertificate'.tbsCertificate,
    V   = TBS#'OTPTBSCertificate'.validity,
    NotBefore = certLib:convert_cert_date(V#'Validity'.notBefore),
    NotAfter  = certLib:convert_cert_date(V#'Validity'.notAfter),
    [NC] = read_node_credential(Key),
    ok = mnesia:dirty_write(NC#nodeCredential{certificateState = ?CertificateState_VALID}),
    handle_calc_timeout(Key, NotBefore, NotAfter);

% HV89082
start_to_expire_timer(_, _) ->
    info_msg("Certificate has expired, expire timer not started~n", []).

%%% ----------------------------------------------------------
%%% #          restart_to_expire_timer(Key)
%%% Input: Key
%%% Output: -
%%% Exceptions:
%%% Description: Restart to expire timer.
%%% ----------------------------------------------------------
restart_to_expire_timer(Key) ->
    case get({expire_timer_nc_ref, Key}) of
        undefined ->
            ok;
        Tref ->
            erlang:cancel_timer(Tref),
            put({expire_timer_nc_ref, Key}, undefined),
            ok
    end,

    NC = mnesia:dirty_read(certNC, Key),
    restart_to_expire_timer2(Key, NC).

restart_to_expire_timer2(Key, [NC]) ->
    Cert = NC#certNC.cert,
    restart_to_expire_timer3(Key, Cert);

restart_to_expire_timer2(_, _) ->
    info_msg("NC does not exist, restart of expire timer not done~n", []).

restart_to_expire_timer3(Key, [Cert|_]) ->
    info_msg("NC restart to expire timer, ~p~n", [Key]),
    OTPCert = public_key:pkix_decode_cert(Cert, otp),
    TBS = OTPCert#'OTPCertificate'.tbsCertificate,
    V   = TBS#'OTPTBSCertificate'.validity,
    NotBefore = certLib:convert_cert_date(V#'Validity'.notBefore),
    NotAfter  = certLib:convert_cert_date(V#'Validity'.notAfter),
    handle_calc_timeout(Key, NotBefore, NotAfter);

restart_to_expire_timer3(_, _) ->
    info_msg("Certificate has expired, restart of expire timer not done~n", []).

%%% ----------------------------------------------------------
%%% #           action_handler(Fun)
%%% Input: Fun:fun() - Action to be called under supervision
%%% Output: 
%%% Exceptions: 
%%% Description: Performs the Fun and catches all errors with proper progress
%%%              reporting. It should also close any open sftp connections and
%%%              do whatever clean up that's necessary. In terms of error 
%%%              reporting, that is a section that can be much developed.
%%% ----------------------------------------------------------
action_handler(Fun) ->
    try Fun() of
    ok -> ok
    catch
    throw:cancelled ->
        handle_cancelled();
    throw:Throw ->
        error_msg("throw ~p~n",[Throw]),
        ProgressInfo = "The action could not be completed",
        handle_fail({cleanup, [{progressInfo, ProgressInfo}]});
    Type:Reason ->
        ProgressInfo = "A software related error occured",
        sysInitI:error_report([{Type, Reason}, 
                       erlang:get_stacktrace()]),
        handle_fail({cleanup, [{progressInfo, ProgressInfo}]})
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
%%% #           handle_cancelled()
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: This function reports actions that have been cancelled
%%% ----------------------------------------------------------
handle_cancelled() ->
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress([{result, ?ActionResultType_NOT_AVAILABLE},
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
        ai ->
            ok;
        TRef2 ->
            erlang:cancel_timer(TRef2),
            put(timeout_180, undefined)
    end,

    %% Number of tries, used by CMPv2
    put(number_of_tries, undefined),

    case get(retry_cmp_timer_ref) of
        undefined ->
            ok;
        TRef3 ->
            erlang:cancel_timer(TRef3),
            put(retry_cmp_timer_ref, undefined)
    end,

    %% Clear ai flag
    Key = get(key),
    case mnesia:dirty_read(certNC, Key) of
        [] -> % nothing ongoing
            ok;
        [Obj] ->
            mnesia:dirty_write(Obj#certNC{ai = false})
    end,

    case get(uri_resolution_timer_ref) of
        undefined ->
            ok;
        TRef4 ->
            erlang:cancel_timer(TRef4),
            put(uri_resolution_timer_ref, undefined)
    end,

    case get(enrollment_timer_ref) of
        undefined ->
            ok;
        ai ->
            put(enrollment_timer_ref, undefined);
        TRef5 ->
            erlang:cancel_timer(TRef5),
            put(enrollment_timer_ref, undefined)
    end,
    update_action(no_action).

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
    Key = get(key),
    mnesia:transaction(fun() -> do_update_progress(Key, ProgressData) end).

do_update_progress(Key, ProgressData) ->
    [Obj] = read_node_credential(Key),
    Current = Obj#nodeCredential.enrollmentProgress,
    Old = 
    case Current of
        undefined -> 
        warning_msg("No default progress report!~n"),
        default_progress_report("", 0);
        Current -> Current
    end,
    Progress = comsaI:update_progress(ProgressData, Old),
    NewObj = Obj#nodeCredential{enrollmentProgress=Progress},
    mnesia:write(NewObj).

set_default_progress_report(Action, Id) ->
    Key = get(key),
    mnesia:transaction(
      fun() ->
          [Obj] = read_node_credential(Key),
          Progress = default_progress_report(Action, Id),
          NewObj = Obj#nodeCredential{enrollmentProgress=Progress},
          mnesia:write(NewObj)
      end).
    

default_progress_report(Action, Id) ->
    #'AsyncActionProgress'
    {actionName = Action,
     additionalInfo = [],
     progressInfo = "Action started",
     progressPercentage = 0,
     result = ?ActionResultType_NOT_AVAILABLE,
     resultInfo = "",
     state = ?ActionStateType_RUNNING,
     actionId = Id,
     timeActionStarted =
         comsaI:iso_time(os:timestamp(), extended),
     timeActionCompleted = "",
     timeOfLastStatusUpdate =
         comsaI:iso_time(os:timestamp(), extended)}.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
handle_cancel_enrollment() ->
    Key = get(key),
    certCMPv2:cancel(Key),

    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress([{result, ?ActionResultType_SUCCESS},
             {state, ?ActionStateType_CANCELLED},
             {resultInfo, "Enrollment is cancelled"},
             {progressPercentage, 100},
             {timeActionCompleted, CompleteTime}]),

    cleanup(),
    restart_to_expire_timer(Key),
    ok.

%%% ----------------------------------------------------------
%%% #           handle_install_credential(Uri, UriPwd, CredPwd,
%%%                                       Fingerprint, AI)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description:  Installs a node credential or certificate from
%%% the specified Uniform Resource Identifier (URI). If the
%%% container file is in PKCS#12 format, the action implicitly
%%% starts an offline enrollment. If the file is a certificate
%%% in Privacy Enhanced Mail (PEM) or Distinguished Encoding Rules
%%% (DER) format, this action closes the ongoing offline enrollment
%%% process started by the startOfflineCsrEnrollment action.
%%% The action can be invoked if the enrollmentSupport attribute of
%%% the CertMCapabilites MO indicates that offline enrollment is
%%% supported.
%%% For a container file in PKCS#12 format, the action can be
%%% invoked if there is no other action in progress on this MO.
%%% For a certificate file in PEM or DER format, the action can be
%%% invoked if there is a startOfflineCsrEnrollment action in
%%% progress on this MO.
%%% The action returns immediately after invocation. The progress
%%% of the action can be tracked via the enrollmentProgress attribute.
%%% The action returns with TRUE after successful invocation,
%%% otherwise it returns with FALSE.
%%% ----------------------------------------------------------
handle_install_credential(Uri, UriPwd, CredPwd, Fingerprint, AI) ->
    update_progress([{additionalInfoClear, "Install of Node Credentials"},
                     {progressPercentage, 0},
                     {result, ?ActionResultType_NOT_AVAILABLE},
                     {resultInfo, ""},
                     {state, ?ActionStateType_RUNNING}]),
    {OffCsr, OffPkcs12,_,_} = get_enrollment_support(),
    Key = get(key),
    case mnesia:dirty_read(certNC, Key) of
        [] ->
            case AI of
                false ->
                    TRef = erlang:send_after(180000, self(), timeout_180),
                    put(timeout_180, TRef),
                    mnesia:dirty_write(#certNC{index = Key, ai = false,
                            offline_p12 = {Uri, UriPwd, CredPwd,Fingerprint}});
                true ->
                    put(timeout_180, ai),
                    mnesia:dirty_write(#certNC{index = Key, ai = true,
                        offline_p12 = {Uri, UriPwd, CredPwd, Fingerprint}})
            end;
        [Obj] ->
            case AI of
                false ->
                    TRef = erlang:send_after(180000, self(), timeout_180),
                    put(timeout_180, TRef),
                    mnesia:dirty_write(Obj#certNC{ai = false,
                            offline_p12 = {Uri,UriPwd, CredPwd, Fingerprint}});
                true ->
                    put(timeout_180, ai),
                    mnesia:dirty_write(Obj#certNC{ai = true,
                            offline_p12 = {Uri, UriPwd, CredPwd, Fingerprint}})
            end
    end,
    update_progress([{additionalInfo, "Downloading from server, "++Uri}]),
    handle_install_cred0(Uri, UriPwd, CredPwd, Fingerprint, OffCsr, OffPkcs12).

handle_install_cred0(Uri, UriPwd, CredPwd, Fingerprint, false, false) ->
    erlang:cancel_timer(get(timeout_180)),
    put(timeout_180, undefined),
    handle_fail([{resultInfo, "No node offline enrollment support"}]),
    ok;
handle_install_cred0(Uri, UriPwd, CredPwd, Fingerprint, OffCsr, OffPkcs12) ->
    case ftpI:parse_uri(Uri) of
        {ok, {Proto, User, Host, Port, Path, _Query}} when Proto =:= sftp; Proto =:= ftpes ->
            case {User, Host} of
                {[], _} -> 
                    handle_fail({cleanup, [{resultInfo, "Missing user information in URI"}]});
                {_, []} -> 
                    handle_fail({cleanup, [{resultInfo, "Missing host information in URI"}]});
                _ -> 
                    Result = certServer:start_channel(Proto, Host, Port, User, UriPwd),
                    handle_install_ftp_cred(Proto, Result, Path, Uri, UriPwd, CredPwd,
                                            Fingerprint, OffCsr, OffPkcs12)
            end;
        {ok, {http,_,_,_,_,_}} ->
            handle_install_http_cred1(Uri, CredPwd, OffCsr, OffPkcs12,
                Fingerprint);
        {ok, {https,_,_,_,_,_}} ->
            handle_install_http_cred1(Uri, CredPwd, OffCsr, OffPkcs12,
                Fingerprint);
        _ ->
            erlang:cancel_timer(get(timeout_180)),
            put(timeout_180, undefined),
            handle_fail([{resultInfo, "Wrong URI format"}]),
            ok
    end.


%% HTTP/HTTPS
handle_install_http_cred1(Url, CredPwd, OffCsr, OffPkcs12, Fingerprint) ->
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
                    handle_install_http_cred2({ok, Body}, Url, CredPwd, OffCsr,
                        OffPkcs12, Fingerprint);
                {ok, {{_,200,_}, _Header, Body}} ->
                    handle_install_http_cred2({ok, Body}, Url, CredPwd, OffCsr,
                        OffPkcs12, Fingerprint);
                {'EXIT', Reason} ->
                    info_msg("Reason: ~p~n", [Reason]),
                    handle_install_http_cred2({error, not_found}, Url, CredPwd,
                        OffCsr, OffPkcs12, Fingerprint);
                A ->
                    info_msg("A: ~p~n", [A]),
                    handle_install_http_cred2({error, not_found}, Url, CredPwd,
                        OffCsr, OffPkcs12, Fingerprint)
            end;
        {error, Reason} ->
            info_msg("Resolve URI ~p failed, reason: ~p~n", [Url, Reason]),
            handle_install_http_cred2({error, resolved_uri_failed}, Url,
                CredPwd, OffCsr, OffPkcs12, Fingerprint)
    end.

handle_install_http_cred2({error, Reason}, Uri, CredPwd, OffCsr,
OffPkcs12, Fingerprint) ->
    %% Retry each 30 seconds, before a timer breaks the retries
    %% after 180 seconds
    case get(timeout_180) of
        undefined ->
            info_msg("handle_install_http_cred2 180sec timeout, Uri: ~p~n",
                [Uri]),
            Msg =
            case Reason of
                resolved_uri_failed ->
                    "Resolve URI failed";
                _ ->
                    "No IP connectivity"
            end,
            handle_fail([{resultInfo, Msg}]),
            ok;
        _ ->
            info_msg("handle_install_http_cred2 wait 30sec, Uri: ~p~n", [Uri]),
            update_progress([{progressInfo,
                        "Trying to connect to HTTP server..."}]),
            TRef = erlang:send_after(30000, self(),
                {retry_install, Uri, undefined, CredPwd, Fingerprint,
                    OffCsr, OffPkcs12}),
            put(retry_30_timer, TRef),
            ok
    end;
handle_install_http_cred2({ok, Body}, Uri, CredPwd, OffCsr, OffPkcs12,
    Fingerprint) ->
    case get(timeout_180) of
        ai ->
            ok;
        TRef ->
            erlang:cancel_timer(TRef),
            put(timeout_180, undefined)
    end,
    handle_install_cred1(Body, CredPwd, OffCsr, OffPkcs12, Fingerprint).

%% FTP
handle_install_ftp_cred(Proto, {error, invalid_password}, Path, Uri, UriPwd, CredPwd,
    Fingerprint, OffCsr, OffPkcs12) ->
    handle_fail([{resultInfo, "Invalid URI password"}]),
    ok;
handle_install_ftp_cred(Proto, {error, no_connectivity}, Path, Uri, UriPwd, CredPwd,
    Fingerprint, OffCsr, OffPkcs12) ->
    %% Retry each 30 seconds, before a timer breaks the retries
    %% after 180 seconds
    case get(timeout_180) of
        undefined ->
            info_msg("handle_install_ftp_cred 180sec timeout, Uri: ~p~n",
                [Uri]),
            handle_fail([{resultInfo, "No IP connectivity"}]),
            ok;
        _ ->
            info_msg("handle_install_ftp_cred wait 30sec, Uri: ~p~n", [Uri]),
            update_progress([{progressInfo,
                        "Trying to connect to " ++ protocol_to_text(Proto) ++ " server..."}]),
            TRef = erlang:send_after(30000, self(),
                {retry_install, Uri, UriPwd, CredPwd, Fingerprint,
                    OffCsr, OffPkcs12}),
            put(retry_30_timer, TRef),
            ok
    end;
handle_install_ftp_cred(Proto, {ok, Pid, CRef}, Path, Uri, UriPwd, CredPwd,
    Fingerprint, OffCsr, OffPkcs12) ->
    case get(timeout_180) of
        ai ->
            ok;
        TRef ->
            erlang:cancel_timer(TRef),
            put(timeout_180, undefined)
    end,
    Result = ftpI:read_file(Proto, Pid, Path, 30000),
    ftpI:stop_channel(Proto, Pid, CRef),
    case Result of
        {error,NoFileErr} when NoFileErr =:= no_such_file 
                          orelse NoFileErr =:= epath ->
            certLib:sec_log("", "Download of Node Credential (" ++
                element(5, get(key)) ++ ") file failed, no such file"),
            handle_fail([{resultInfo, "Download of file not working, no such file"}]),
            ok;
        {error, Reason} ->
            certLib:sec_log("", "Download of Node Credential (" ++
                element(5, get(key)) ++ ") file failed"),
            handle_fail([{resultInfo, "Download of file not working"}]),
            ok;
        {ok, Data} ->
            handle_install_cred1(Data, CredPwd, OffCsr, OffPkcs12, Fingerprint)
    end.


handle_install_cred1(Data, CredPwd, OffCsr, OffPkcs12, Fingerprint) ->
    info_msg("Downloaded credential~n", []),
    update_progress([{additionalInfo, "Downloaded credential"}]),
    %% Check fingerprint on the whole data chunck,
    case certVerify:verify_fingerprint(Data, Fingerprint) of
        {no_match, ThisFingerprint} ->
            certLib:sec_log("", "Downloaded Node credential (" ++
                element(5, get(key)) ++ ") does not match fingerprint"),
            handle_fail([{additionalInfo, "Obtained fingerprint is: "++ThisFingerprint},
                         {resultInfo, "The fingerprint does not match"}]),
            ok;
        no_fingerprint ->
            update_progress([{additionalInfo, "No fingerprint"}]),
            handle_install_cred2(Data, CredPwd, OffCsr, OffPkcs12);
        match ->
            update_progress([{additionalInfo, "Fingerprint matches"}]),
            handle_install_cred2(Data, CredPwd, OffCsr, OffPkcs12)
    end.

handle_install_cred2(Data, CredPwd, OffCsr, OffPkcs12) ->
    case certPkcs12:unpack_offline_data(Data, CredPwd) of
        {pkcs12, Cert, PrivKey, TCs} ->
            handle_install_cred3({Cert, PrivKey, TCs}, pkcs12,
                OffCsr, OffPkcs12, get_action());
        {cert, Cert, TCs} ->
            %% Get prikey from the SecEE
            case certSecStore:get_nc_key(get(key), "csr.key") of
                {ok, PrivKeyBin} ->
                    PrivKey = binary_to_term(PrivKeyBin),
                    handle_install_cred3({Cert, PrivKey, TCs}, cert,
                        OffCsr, OffPkcs12, get_action());
                {error, Reason} ->
                    certLib:sec_log("", "Downloaded Node credential (" ++
                                        element(5, get(key)) ++ ") does not match private key"),
                    handle_fail([{resultInfo, "No match between certificate and private key"}]),
                    ok
            end;
        {error,failed_decode_key_file} ->
            handle_fail([{resultInfo, "Wrong file or credential password"}]),
            ok;
        Reason -> % Not ok
            info_msg("Downloaded file not working, ~p", [Reason]),
            certLib:sec_log("", "Downloaded Node credential (" ++
                element(5, get(key)) ++ ") file is not working"),
            handle_fail([{resultInfo, "Downloaded file not working"}]),
            ok
    end.

handle_install_cred3(_, pkcs12, _, false, _) ->
    handle_fail([{resultInfo, "Offline enrollment using pkcs12, not support"}]),
    ok;
handle_install_cred3(_, pkcs12, _, true, offline_enrollment) ->
    handle_fail([{resultInfo, "Offline enrollment using CSR, already ongoing"}]),
    ok;
handle_install_cred3(Data, pkcs12, _, true, _) ->
    handle_install_cred4(Data);
handle_install_cred3(_, _, false, _, _) ->
    handle_fail([{resultInfo, "Offline enrollment using CSR, not supported"}]),
    ok;
handle_install_cred3(Data, _, true, _, offline_enrollment) ->
    handle_install_cred4(Data);
handle_install_cred3(_, _, true, _, _) ->
    handle_fail([{resultInfo, "No offline enrollment using CSR, ongoing"}]),
    ok.


handle_install_cred4({[NcCert|_] = Cert, PrivKey, TCs}) ->
    Result = certVerify:verify_nc(NcCert, PrivKey),
    handle_install_cred5(Result, Cert, PrivKey, TCs).

handle_install_cred5({failed, no_match}, _,_,_) ->
    certLib:sec_log("", "Downloaded Node credential (" ++
        element(5, get(key)) ++ ") does not match private key"),
    handle_fail([{resultInfo, "No match between certificate and private key"}]),
    ok;
handle_install_cred5({failed, invalid_date}, _,_,_) ->
    certLib:sec_log("", "Downloaded Node credential (" ++
        element(5, get(key)) ++ ") certificate date is not valid"),
    handle_fail([{resultInfo, "Certificate date is not valid"}]),
    ok;
handle_install_cred5({failed, Faulty_KeyUsage}, _,_,_) ->
    certLib:sec_log("", "Downloaded Node credential (" ++
        element(5, get(key)) ++ ") certificate has faulty keyUsage"),
    info_msg("Faulty KeyUsage: ~w~n", [Faulty_KeyUsage]),
    handle_fail([{resultInfo, "Faulty keyUsage"}]),
    ok;
handle_install_cred5(valid, [NcCert|_] = Cert, PrivKey, TCs) ->
    %% Store cert and privkey in SecEE
    %% Update nodeCredential with content and state
    Key = get(key),
    [NC] = read_node_credential(Key),
    CC = certLib:read_cert_metadata(NcCert),
    OTPCert     = public_key:pkix_decode_cert(NcCert, otp),
    TBS         = OTPCert#'OTPCertificate'.tbsCertificate,
    Subject     = TBS#'OTPTBSCertificate'.subject,
    SubjectName = certLib:format_rdn(Subject),
    NewNC = NC#nodeCredential{
        certificateContent = CC,
        certificateState   = ?CertificateState_VALID,
        subjectName = SubjectName},
    ok = mnesia:dirty_write(NewNC),
    case certSecStore:get_nc_key(Key, "nc.key") of
        {ok, PrivOldKey} ->
            info_msg("Stored old nc key ~p", [Key]),
            certSecStore:remove_nc_key(Key, "nc_old.key"),
            certSecStore:put_nc_key(Key, PrivOldKey, "nc_old.key"),
            certSecStore:remove_nc_key(Key, "nc.key");
        _ ->
            ok
    end,
    certSecStore:remove_nc_key(Key, "csr.key"),
    certSecStore:put_nc_key(Key, term_to_binary(PrivKey), "nc.key"),
    update_action_and_cert(no_action, Cert),
    handle_success([{additionalInfo, "Installation of node credential finished"},
                    {resultInfo, mk_ext_moref(Key)}]),
    %% Stop old timer in case of renewal
    case get({expire_timer_nc_ref, Key}) of
    undefined ->
        ok;
    Tref ->
        erlang:cancel_timer(Tref),
        put({expire_timer_nc_ref, Key}, undefined)
    end,
    %% Clear alarms if any
    certAlarm:clear(nc, cert_not_available, Key),
    certAlarm:clear(nc, cert_to_expire, Key),
    certAlarm:clear(nc, cert_enroll_failed, Key),
    %% Start expire timer
    start_to_expire_timer(Key, Cert),
    %% Install Trusted Certificate 
    certServer:pre_install(TCs),
    certSub:trig(nc, Key),
    ok.

%%% ----------------------------------------------------------
%%% #           handle_start_offline_enrollment(Uri)
%%% Input: Uri:string()
%%% Output: 
%%% Exceptions: 
%%% Description: Starts a manual enrollment procedure.
%%% Creates a PKCS#10 Certificate Signing Request (CSR) that is
%%% stored at the specified local Uniform Resource Identifier URI.
%%% A file transfer service can be used to fetch the CSR file from
%%% the ME. The action can be invoked if the enrollmentSupport
%%% attribute of the CertMCapabilites MO indicates that offline
%%% CSR-based enrollment is supported. The action uses attributes
%%% subjectName and keyInfo attributes as inputs. If no other action
%%% is in progress on this MO, the action can be invoked and it
%%% returns immediately after invocation. The action progress can
%%% be tracked via the enrollmentProgress attribute. The action
%%% returns with TRUE after successful invocation, otherwise it
%%% returns with FALSE.
%%% ----------------------------------------------------------
handle_start_offline_enrollment(Uri, Password) ->
    %% Intiate progress report
    Proto = get_proto_from_uri(Uri),
    {OfflineCsr, _, _, _} = get_enrollment_support(),
    Action = get_action(),
    handle_start_offline_enrollment1(Uri, Password, OfflineCsr, Action, Proto).

handle_start_offline_enrollment1(_, _, false, _, _) ->
    handle_fail([{resultInfo, "No offline csr enrollment support"}]),
    ok;
handle_start_offline_enrollment1(_, _, _, _, {error, Reason}) ->
    handle_fail([{resultInfo, Reason}]),
    ok;
handle_start_offline_enrollment1(Uri, Password, _, no_action, Proto) ->
    [NC] = read_node_credential(get(key)),
    Current = NC#nodeCredential.enrollmentProgress,
    SubjectName    = NC#nodeCredential.subjectName,
    SubjectAltName = NC#nodeCredential.subjectAltName,
    KeyInfo = NC#nodeCredential.keyInfo,
    handle_start_offline_enrollment2(Uri, Password, NC, SubjectName, SubjectAltName, Proto, KeyInfo);
handle_start_offline_enrollment1(_, _, _,_, _) ->
    handle_fail([{resultInfo, "Enrollment already ongoing"}]),
    ok.



handle_start_offline_enrollment2(_,_,_, undefined,_, _, _) ->
    handle_fail([{resultInfo, "Subject name is missing"}]),
    ok;
handle_start_offline_enrollment2(_,_,_, _,_, _, undefined) ->
    handle_fail([{resultInfo, "Key info is missing"}]),
    ok;
handle_start_offline_enrollment2(Uri, Password, NC, Subject, SubjectAltName, Proto, _) ->
    TRef = erlang:send_after(180000, self(), timeout_180),
    put(timeout_180, TRef),
    update_progress([{additionalInfoClear, "Creation of Node Credentials"},
                     {progressPercentage, 0},
                     {result, ?ActionResultType_NOT_AVAILABLE},
                     {resultInfo, ""},
                     {state, ?ActionStateType_RUNNING}]),
    KeyInfo = certKey:format_key_info(NC#nodeCredential.keyInfo),
    %% Generate CSR
    Result = gen_csr(KeyInfo, Subject, SubjectAltName),
    update_progress([{additionalInfo, "Sending CSR to " ++ protocol_to_text(Proto) ++ " server, "++Uri},
                     {progressPercentage, 25}]),
    handle_start_offline_enrollment3(Result, http_uri:decode(Uri), Password).


handle_start_offline_enrollment3({error, no_bpP521r1_support}, _, _) ->
    erlang:cancel_timer(get(timeout_180)),
    put(timeout_180, undefined),
    handle_fail([{resultInfo, "No support for brainpoolP521r1"}]),
    ok;
handle_start_offline_enrollment3({error, several_cn}, _, _) ->
    erlang:cancel_timer(get(timeout_180)),
    put(timeout_180, undefined),
    handle_fail([{resultInfo, "Several CN fields in the subjectName"}]),
    ok;
handle_start_offline_enrollment3({error, no_cn}, _, _) ->
    erlang:cancel_timer(get(timeout_180)),
    put(timeout_180, undefined),
    handle_fail([{resultInfo, "No CN field in the subjectName"}]),
    ok;
handle_start_offline_enrollment3({error, missing}, _, _) ->
    erlang:cancel_timer(get(timeout_180)),
    put(timeout_180, undefined),
    handle_fail([{resultInfo, "No subjectName"}]),
    ok;
handle_start_offline_enrollment3({error, subAltName_copy_no_support}, _, _) ->
    erlang:cancel_timer(get(timeout_180)),
    put(timeout_180, undefined),
    handle_fail([{resultInfo, "No subjectAltName support for copy"}]),
    ok;
handle_start_offline_enrollment3({error, subAltName_ip}, _, _) ->
    erlang:cancel_timer(get(timeout_180)),
    put(timeout_180, undefined),
    handle_fail([{resultInfo, "subjectAltName IP wrong format"}]),
    ok;
handle_start_offline_enrollment3({error, {subAltName, no_cn}}, _, _) ->
    erlang:cancel_timer(get(timeout_180)),
    put(timeout_180, undefined),
    handle_fail([{resultInfo, "subjectAltName, missing CN in directorName"}]),
    ok;
handle_start_offline_enrollment3({error, {subAltName, several_cn}}, _, _) ->
    erlang:cancel_timer(get(timeout_180)),
    put(timeout_180, undefined),
    handle_fail([{resultInfo, "subjectAltName, several CN in directoryName"}]),
    ok;
handle_start_offline_enrollment3({error, _}, _, _) ->
    erlang:cancel_timer(get(timeout_180)),
    put(timeout_180, undefined),
    handle_fail([{resultInfo, "Generate node credential keypair failed"}]),
    ok;
handle_start_offline_enrollment3(Csr, Uri, Password) ->
    Proto = get_proto_from_uri(Uri),
    case get(timeout_180) of 
        undefined ->
            info_msg("install_cred1 180sec timeout, Uri: ~p~n", [Uri]),
            {_,_,_,_,Index} = get(key),
            handle_fail([{resultInfo, "Failed due to no IP connectivity"}]),
            ok; 
        TRef1 ->
            Key = get(key),
            {_,_,_,_,Index} = Key,
            case write_remote(Uri, Password, Index, Csr) of
                ok ->
                    erlang:cancel_timer(TRef1),
                    put(timeout_180, undefined),
                    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
                    update_progress([{result, ?ActionResultType_SUCCESS},
                                     {state, ?ActionStateType_FINISHED},
                                     {progressPercentage, 50},
                                     {timeActionCompleted, CompleteTime},
                                     {progressInfo,
                                      "Sent CSR to " ++ protocol_to_text(Proto) ++ 
                                          " server for enrollment, waiting for credential..."},
                                     {resultInfo, mk_ext_moref(Key)}]),
                    update_action(offline_enrollment),
                    ok;
                {error, no_ip_connectivity} ->
                    info_msg("start_offline_enrollment3 wait 30sec, Uri: ~p~n",
                        [Uri]),
                    %% Retry each 30 seconds, before a timer
                    %% breaks the retries after 180 seconds
                    update_progress([{progressInfo,
                        "Trying to connect to " ++ protocol_to_text(Proto) ++ " server..."}]),
                    TRef2 =
                    erlang:send_after(30000, self(),
                        {handle_start_offline_enrollment3,
                            Csr, Uri, Password}),
                    put(retry_30_timer, TRef2),
                    ok;
                {error, invalid_password} ->
                    erlang:cancel_timer(TRef1),
                    put(timeout_180, undefined),
                    handle_fail([{resultInfo, "Invalid password"}]),
                    ok;
                {error, no_such_file} ->
                    erlang:cancel_timer(TRef1),
                    put(timeout_180, undefined),
                    handle_fail([{resultInfo, "Invalid path, no such file"}]),
                    ok;
                {error,permission_denied} ->
                    erlang:cancel_timer(TRef1),
                    put(timeout_180, undefined),
                    handle_fail([{resultInfo, "No permission to access file"}]),
                    ok;
                _ ->
                    erlang:cancel_timer(TRef1),
                    put(timeout_180, undefined),
                    handle_fail([{resultInfo, "Node credential enrollment failed"}]),
                    ok
            end
    end.

%%% ----------------------------------------------------------
%%% #           handle_start_online_enrollment(AI, Challenge)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Starts the online enrollment process.
%%% For initial enrollment when a vendor credential is not
%%% available, a challengePassword must be provided, otherwise
%%% the vendor credential or the existing valid node credential
%%% signs the PKI request. If the enrollmentSupport attribute of
%%% the CertMCapabilites MO indicates that online enrollment is
%%% supported, the action can be invoked. As inputs, the action
%%% uses subjectName, keyInfo, enrollmentServerGroup and
%%% enrollmentAuthority attributes. If no other action is in
%%% progress on this MO, the action can be invoked and it returns
%%% immediately after invocation. Action progress can be tracked
%%% via the enrollmentProgress attribute. The action returns with
%%% TRUE after successful invocation, otherwise it returns with FALSE.
%%% 
%%% NOTE: Challenge will only be used if and when Vendor
%%%       Credential is mising, which is the case in the cloud.
%%% ----------------------------------------------------------
handle_start_online_enrollment(AI, Challenge) ->
    info_msg("handle_start_online_enrollment~n", []),
    %% clear if any auto or force are ongoing
    put(number_of_tries, undefined),
    put(auto_renewal, undefined),
    case get(retry_cmp_timer_ref) of
        undefined ->
            ok;
        TRef ->
            erlang:cancel_timer(TRef),
            put(retry_cmp_timer_ref, undefined)
    end,
    %% Intiate progress report
    StartTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress([{additionalInfoClear, "Creation of Node Credentials"},
            {progressPercentage, 0},
            {result, ?ActionResultType_NOT_AVAILABLE},
            {resultInfo, ""},
            {state, ?ActionStateType_RUNNING}]),
    certAlarm:clear(nc, cert_enroll_failed, get(key)),
    Action = get_action(),       
    {_,_,_Scep, Cmp} = get_enrollment_support(), % No SCEP support, only CMPv2
    handle_start_online_enrollment1(Cmp, Action, AI, Challenge).

handle_start_online_enrollment1(false,_,_,_) ->
    handle_fail({cleanup, [{resultInfo, "No online csr enrollment support"}]}),
    ok;
handle_start_online_enrollment1(_, no_action, AI, Challenge) ->
    Key = get(key),
    [NC] = read_node_credential(Key),
    start_enrollment_timer(Key, NC),
    %% Get enrollment authority
    EAObj = mnesia:dirty_read(enrollmentAuthority,
        certLib:decode_moref(NC#nodeCredential.enrollmentAuthority)),
    %% Get enrollment server
    URIs = get_enrollment_server(NC#nodeCredential.enrollmentServerGroup),
    SubjectName    = NC#nodeCredential.subjectName,
    SubjectAltName = NC#nodeCredential.subjectAltName,
    handle_start_online_enrollment2(NC, EAObj, URIs, SubjectName,
        SubjectAltName, AI, Challenge);
handle_start_online_enrollment1(_,_,_,_) ->
    handle_fail({cleanup, [{resultInfo, "Enrollment already ongoing"}]}),
    ok.

handle_start_online_enrollment2(NC, [EA], URIs, SubjectName,
    SubjectAltName, AI, Challenge) ->
    %% Send request to enrollment server, CMPv2 or SCEP
    KeyInfo          = certKey:format_key_info(NC#nodeCredential.keyInfo),
    Result           = gen_csr(KeyInfo, SubjectName, SubjectAltName),
    handle_start_online_enrollment3(Result, NC, EA, URIs, SubjectName,
        AI, Challenge).


handle_start_online_enrollment3({error, no_bpP521r1_support}, _,_,_,_,_,_) ->
    handle_fail({cleanup, [{resultInfo, "No support for brainpoolP521r1"}]}),
    ok;
handle_start_online_enrollment3({error, several_cn}, _,_,_,_,_,_) ->
    handle_fail({cleanup, [{resultInfo, "Several CN fields in the subjectName"}]}),
    ok;
handle_start_online_enrollment3({error, no_cn},_,_,_,_,_,_) ->
    handle_fail({cleanup, [{resultInfo, "No CN field in the subjectName"}]}),
    ok;
handle_start_online_enrollment3({error, missing}, _,_,_,_,_,_) ->
    handle_fail({cleanup, [{resultInfo, "No subject name"}]}),
    ok;
handle_start_online_enrollment3({error, subAltName_copy_no_support},
    _,_,_,_,_,_) ->
    handle_fail({cleanup, [{resultInfo, "No subjectAltName support for copy"}]}),
    ok;
handle_start_online_enrollment3({error, subAltName_ip},_,_,_,_,_,_) ->
    handle_fail({cleanup, [{resultInfo, "subjectAltName IP wrong format"}]}),
    ok;
handle_start_online_enrollment3({error, {subAltName, no_cn}},_,_,_,_,_,_) ->
    handle_fail({cleanup, [{resultInfo, "subjectAltName, missing CN in directorName"}]}),
    ok;
handle_start_online_enrollment3({error, {subAltName,several_cn}},_,_,_,_,_,_) ->
    handle_fail({cleanup, [{resultInfo, "subjectAltName, several CN in directoryName"}]}),
    ok;
handle_start_online_enrollment3({error, Result}, _, _, _,_,_,_) ->
    handle_fail({cleanup, [{resultInfo, "Enrollment failed with reason: " ++ atom_to_list(Result)}]}),
    ok;
handle_start_online_enrollment3(Csr, NC, EA, URIs, SubjectName,
    AI, Challenge) ->
    Key = get(key),
    case Challenge of
        undefined ->
            ok;
        [] ->
            ok;
        _ ->
            %% Store Challenge
            certSecStore:remove_nc_key(Key, "nc.c"),
            certSecStore:put_nc_key(Key, list_to_binary(Challenge), "nc.c")
    end,
    Timer            = NC#nodeCredential.enrollmentTimer,
    AuthorityName    = EA#enrollmentAuthority.enrollmentAuthorityName,
    EnrollCaCertDn   = EA#enrollmentAuthority.enrollmentCaCertificate,
    EnrollCaCert     = certServer:get_tc(EnrollCaCertDn),
    EnrollCaFingerpr = %% Part of fix for HV70387
        case {EnrollCaCert, EnrollCaCertDn} of
            {_, undefined} ->
                % no EnrollCaCertDn, ok to take Fingerprint
                EA#enrollmentAuthority.enrollmentCaFingerprint;
            {not_found, _} ->
                % EnrollCaCert not valid (but is set), nok to take Fingerprint
                undefined;
            {_, _} ->
                % everything else ok, take Fingerprint
                EA#enrollmentAuthority.enrollmentCaFingerprint
        end,

    update_progress([{additionalInfo, "Credential created"}]),
    case mnesia:dirty_read(certNC, Key) of
        [] ->
            case AI of
                false ->
                    ok;
                true ->
                    mnesia:dirty_write(#certNC{index = Key, ai = true})
            end;
        [Obj] ->
            case AI of
                false ->
                    mnesia:dirty_write(Obj#certNC{ai = false});
                true ->
                    mnesia:dirty_write(Obj#certNC{ai = true})
            end
    end,
    put(number_of_tries, undefined),
    update_action(online_enrollment),
    Result = make_enrollment(Csr, Timer, AuthorityName, SubjectName, Key, URIs,
        EnrollCaCert, EnrollCaFingerpr),
    handle_start_online_enrollment4(Result).

handle_start_online_enrollment4(ok) ->
    update_progress([{additionalInfo, "CSR sent to enrollment server"},
            {result, ?ActionResultType_NOT_AVAILABLE},
            {progressPercentage, 50}]),
    ok;
handle_start_online_enrollment4({error, can_not_verify_ca_ra_cert}) ->
    handle_fail({cleanup, [{progressInfo,
                "Found no valid CA/RA certificate or fingerprint"},
            {resultInfo, "Online enrollment failed"}]}),
    ok;
handle_start_online_enrollment4({error, no_working_uri}) ->
    handle_fail({cleanup, [{progressInfo, "No working CMPv2 server uri"},
                    {resultInfo, "Online enrollment failed"}]}),
    ok.

check_online_enrollment_mandatory_params() ->
    [NodeCredential] = read_node_credential(get(key)), 
    KeyInfo = NodeCredential#nodeCredential.keyInfo,
    EAList = mnesia:dirty_read(enrollmentAuthority,
        certLib:decode_moref(NodeCredential#nodeCredential.enrollmentAuthority)), 
    EnrollmentServerGroup = NodeCredential#nodeCredential.enrollmentServerGroup,
    EnrollmentServers = get_enrollment_servers(EnrollmentServerGroup),
    CheckEA = 
    case EAList of
        [] -> 
            {fail, "Enrollment authority missing!"};
        [EnrollmentAuthority] ->
            AuthorityName   =
            EnrollmentAuthority#enrollmentAuthority.enrollmentAuthorityName,
            CaCertDn        =
            EnrollmentAuthority#enrollmentAuthority.enrollmentCaCertificate,
            CaCert          = certServer:get_tc(CaCertDn),
            CaFingerprint   = EnrollmentAuthority#enrollmentAuthority.enrollmentCaFingerprint,
            case {AuthorityName, CaCertDn, CaFingerprint, CaCert} of
                {undefined, _, _, _} ->            {fail, "Authority name missing!"};
                {_, undefined, undefined, _} ->    {fail, "CA certificate or fingerprint missing!"};
                {_, _, _, not_found} when CaCertDn =/= undefined -> {fail, "CA certificate disabled or invalid!"};
                {_, _, _, _} ->                    ok
            end
    end,
    case {KeyInfo, CheckEA, EnrollmentServerGroup, EnrollmentServers} of
        {undefined, _, _, _} -> {fail, "Key info missing!"};
        {_, {fail, _}, _, _} -> CheckEA;
        {_, _, undefined, _} -> {fail, "Enrollment server group missing!"};
        {_, _, _, []} ->        {fail, "Enrollment server(s) missing!"};
        {_, _, _, _} ->         ok
    end.

enrollment_failure_update(Msg) ->
    handle_fail([{resultInfo, "Online enrollment failed, " ++
                "missing mandatory fields: " ++ Msg}]),
    ok.


%%% ----------------------------------------------------------
%%% #          update_action(Action)
%%% Input:  atom(Action) | no_action
%%% Output: ok
%%% Exceptions: 
%%% Description: Used to keep track of ongoing actions for
%%%              next action.
%%% ----------------------------------------------------------
update_action(Action) ->
    Key = get(key),
    case mnesia:dirty_read(certNC, Key) of
        [] -> % nothing ongoing
            NewObj = #certNC{index = Key, action = Action},
            mnesia:dirty_write(NewObj);
        [Obj] ->
            AI =
            case Action of
                no_action ->
                    false;
                _ ->
                    Obj#certNC.ai
            end,
            NewObj =
            Obj#certNC{action = Action, ai = AI},
            mnesia:dirty_write(NewObj)
    end.  

update_action_and_cert(Action, Cert) ->
    Key = get(key),
    case mnesia:dirty_read(certNC, Key) of
        [] -> % nothing ongoing
            NewObj =
            #certNC{index = Key, action = Action,
                cert = Cert, csr = undefined},
            mnesia:dirty_write(NewObj);
        [Obj] ->
            NewObj =
            Obj#certNC{action = Action, cert = Cert,
                csr = undefined, ai = false},
            mnesia:dirty_write(NewObj)
    end.  


%%% ----------------------------------------------------------
%%% #          get_action()
%%% Input:  -
%%% Output: atom(Action)
%%% Exceptions: 
%%% Description: Used to keep track of ongoing actions for
%%%              next action.
%%% ----------------------------------------------------------
get_action() ->
    case mnesia:dirty_read(certNC, get(key)) of
        [] -> % nothing ongoing
            no_action;
        [Obj] ->
            Obj#certNC.action
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
    ",SystemFunctions=1,SecM=1,CertM=1,NodeCredential=" ++ Id.

%%% ----------------------------------------------------------
%%% #          write_remote(Uri, Password, FileName, Csr)
%%% Input:  
%%% Output:  
%%% Exceptions: 
%%% Description: Write the file on a remote sftp server or ftpes.
%%% ----------------------------------------------------------
write_remote(Uri, Password, Index, Csr) ->
    Entry  = public_key:pem_entry_encode('CertificationRequest', Csr),
    PemBin = public_key:pem_encode([Entry]),
    case ftpI:parse_uri(Uri) of
        {ok, {Proto, User, Host, Port, Path, _Query}} when Proto =:= sftp; Proto =:= ftpes ->
            case certServer:start_channel(Proto, Host, Port, User, Password) of
                {ok, Pid, CRef} ->
                    RemotePath = 
                    case string:substr(Path,length(Path),1) of
                        "/" ->
                            filename:join(Path, "csr_" ++ Index ++ ".csr");
                        _ ->
                            Path
                    end,
                    Res =
                    case ftpI:write_file(Proto, Pid, RemotePath, PemBin, 30000) of
                        ok ->
                            ok;
                        {error,failure} ->
                            fail;
                        {error, no_such_file} ->
                            {error, no_such_file};
                        {error,permission_denied} ->
                            {error,permission_denied};
                        {error, FtpReason} ->
                            sysInitI:error_report(
                                [{mfa, {ftpI, write_file,
                                            [Proto, Pid, RemotePath, PemBin, 30000]}},
                                    {error, FtpReason}]),
                            {error, FtpReason}
                    end,
                    ftpI:stop_channel(Proto, Pid, CRef),
                    Res;
                {error, invalid_password} ->
                    {error, invalid_password};
                {error, Reason} ->
                    info_msg("start_channel failed uri:~p, Reason: ~p~n",
                        [Uri, Reason]),
                    {error, no_ip_connectivity}
            end;
        {error, UriReason} ->
            info_msg("ftp_uri:parse failed uri:~p, UriReason: ~p~n",
                [Uri, UriReason]),
            {error, error}
    end.

%%% ----------------------------------------------------------
%%% #          make_enrollment(Csr, Timer, AuthorityName,
%%%                  SubjectName, Key, Uri,
%%%                  EnrollCaCert, EnrollCaFingerpr),
%%% Input:  Csr
%%%         Timer
%%% Output: 
%%% Exceptions: 
%%% Description: Make the enrollment using selected enrollment
%%%              server.
%%% ----------------------------------------------------------
make_enrollment(_, _, _, _, _, _, not_found, undefined) ->
    %% Can not verify the CA or RA certificate
    {error, can_not_verify_ca_ra_cert};
make_enrollment(_,_, _, _, _, [], _, _) ->
    %% No working URI
    {error, no_working_uri}; 
make_enrollment(Csr, Timer, AuthorityName, SubjectName,
    Key, [URI|RestURI] = URIs, EnrollCaCert, EnrollCaFingerpr) ->
    case certLib:resolve_uri(URI) of
        {ok, _ResolvedURI} ->
            do_make_enrollment(Csr, Timer, AuthorityName, SubjectName,
                Key, [URI|RestURI], EnrollCaCert, EnrollCaFingerpr);
        %% HV33549 - OOT not avilable at startup
        {error, oot_not_started} ->
            TRef =
            erlang:send_after(5000, self(),
                {make_enrollment, Csr, Timer, AuthorityName, SubjectName,
                    Key, [URI|RestURI], EnrollCaCert, EnrollCaFingerpr}),
            put(uri_resolution_timer_ref, TRef),
            ok;
        {error, Reason} ->
            info_msg("Resolve URI failed: ~p", [Reason]),
            %% move faulty URI to end of list to avoid getting stuck on first server.
            NewURIs = RestURI ++ [URI],
            TRef =
            erlang:send_after(5000, self(), % if we move on to the next URI in the list is timeout necessary? 
                                            %(if only one URI in list then this is essentialy a retry 
                                            % and timeout makes sense, otherwise it seems pointless)
                {make_enrollment, Csr, Timer, AuthorityName, SubjectName,
                    Key, NewURIs, EnrollCaCert, EnrollCaFingerpr}),
            put(uri_resolution_timer_ref, TRef),
            ok
    end.

do_make_enrollment(Csr, Timer, AuthorityName, SubjectName,
    Key, [URI|RestURI], EnrollCaCert, EnrollCaFingerpr) ->
    %% Renewal (use NC) or Initial (use VC)
    {_,_,_,_, Index} = Key,
    {ok, KeyBin} = certSecStore:get_nc_key(Key, "csr.key"),
    CsrKey = binary_to_term(KeyBin),
    SrcIf  = get_src_if(Key),
    case mnesia:dirty_read(certNC, Key) of
        [] ->
            certCMPv2:initialization_request(AuthorityName, SubjectName,
                Csr, CsrKey, Index, URI,
                EnrollCaCert, EnrollCaFingerpr, SrcIf);
        [Obj] ->
            case Obj#certNC.cert of
                undefined ->
                    certCMPv2:initialization_request(AuthorityName,
                        SubjectName, Csr, CsrKey, Index, URI,
                        EnrollCaCert, EnrollCaFingerpr, SrcIf);
                NcCert ->
                    case certSecStore:get_nc_key(Key, "nc.key") of
                        {ok, KeyBinary} ->
                            NcKey = binary_to_term(KeyBinary),
                            certCMPv2:key_update_request(AuthorityName,
                                SubjectName, Csr, CsrKey, Index, NcCert, NcKey,
                                URI, EnrollCaCert, EnrollCaFingerpr, SrcIf);
                        _ ->
                            certCMPv2:initialization_request(AuthorityName,
                                SubjectName, Csr, CsrKey, Index, URI,
                                EnrollCaCert, EnrollCaFingerpr, SrcIf)
                    end
            end
    end,
    ok.

start_enrollment_timer(Key, NC) ->
    Timer = NC#nodeCredential.enrollmentTimer,
    AI =
    case mnesia:dirty_read(certNC, Key) of
        [] ->
            false;
        [Obj] ->
            Obj#certNC.ai
    end,
    case AI of
        false ->
            %% Timeout timer for this enrollment when sending it
            %% to the enrollment server.            
            enrollment_timeout_in_minutes(Timer);
        true ->
            %% If AI ongoing no timer is started, keep on for ever
            put(enrollment_timer_ref, ai)
    end.

enrollment_timeout_in_minutes(Minutes) ->
    case get(enrollment_timer_ref) of
                undefined ->
                    TRef =
                    erlang:send_after(Minutes*60000, self(),
                        enrollment_timeout),
                    put(enrollment_timer_ref, TRef);
                _ -> % Already started
                    ok
    end.

%%% ----------------------------------------------------------
%%% #         gen_csr(KeyType, SubjectName, SubjectAltName) 
%%% Input:  
%%% Output: #'CertificationRequest'{...}
%%% Exceptions: 
%%% Description: Create node credential.
%%% ----------------------------------------------------------
gen_csr({error, Reason}, _, _) ->
    {error, Reason};
gen_csr(KeyType, SubjectName, SubjectAltName) ->
    gen_csr1(KeyType, certKey:key_gen(KeyType), SubjectName, SubjectAltName).

gen_csr1(_, {error, Reason}, _,_) ->
    {error, Reason};
%% Using RSA
gen_csr1({"genrsa",_}, {ok, PrivKey, SubjectPubkeyInfo}, SubjectName,
    SubjectAltName) ->
    #'SubjectPublicKeyInfo'{algorithm        = AlgorithmIdentifier,
                subjectPublicKey = SubPubKey} = SubjectPubkeyInfo,
   
    CriPKIAlgorithm = #'CertificationRequestInfo_subjectPKInfo_algorithm'{
        algorithm  = AlgorithmIdentifier#'AlgorithmIdentifier'.algorithm,
        parameters = asn1_NOVALUE},
    
    SubPKInfo = #'CertificationRequestInfo_subjectPKInfo'{
        algorithm        = CriPKIAlgorithm,
        subjectPublicKey = SubPubKey},
   
    case build_rdnSequence(SubjectName) of
        {error, Reason} ->
            {error, Reason};
        SubjectRdnSeq ->
            case extension(SubjectAltName) of
                {error, Reason} ->
                    {error, Reason};
                Extension ->
                    CertReqInfo = #'CertificationRequestInfo'{
                        version       = v1,
                        subject       = SubjectRdnSeq,
                        subjectPKInfo = SubPKInfo,
                        attributes    = Extension},
                    DerCri =
                    public_key:der_encode('CertificationRequestInfo',
                        CertReqInfo),
                    SignedCri = public_key:sign(DerCri, 'sha256', PrivKey),
                    
                    SignatureAlgorithm =
                    #'CertificationRequest_signatureAlgorithm'{
                        algorithm = ?'sha256WithRSAEncryption',
                        parameters = asn1_NOVALUE},
                    
                    #'CertificationRequest'{
                        certificationRequestInfo = CertReqInfo,
                        signatureAlgorithm       = SignatureAlgorithm,
                        signature                = SignedCri}
            end
    end;
%% Using EC
gen_csr1(_, {ok, ECPrivKey}, SubjectName, SubjectAltName) ->
    Parameters = ECPrivKey#'ECPrivateKey'.parameters,
    CriPKIAlgorithm = #'CertificationRequestInfo_subjectPKInfo_algorithm'{
        algorithm  = ?'id-ecPublicKey',
        parameters = {asn1_OPENTYPE, public_key:der_encode('EcpkParameters', Parameters)}},

    SubPKInfo = #'CertificationRequestInfo_subjectPKInfo'{
        algorithm        = CriPKIAlgorithm,
        subjectPublicKey = ECPrivKey#'ECPrivateKey'.publicKey},
   
    case build_rdnSequence(SubjectName) of
        {error, Reason} ->
            {error, Reason};
        SubjectRdnSeq ->
            case extension(SubjectAltName) of
                {error, Reason} ->
                    {error, Reason};
                Extension ->
                    CertReqInfo = #'CertificationRequestInfo'{
                        version       = v1,
                        subject       = SubjectRdnSeq,
                        subjectPKInfo = SubPKInfo,
                        attributes    = Extension},
                    DerCri =
                    public_key:der_encode('CertificationRequestInfo',
                        CertReqInfo),
                    SignedCri = public_key:sign(DerCri, 'sha256', ECPrivKey),
                    
                    SignatureAlgorithm =
                    #'CertificationRequest_signatureAlgorithm'{
                        algorithm = ?'ecdsa-with-SHA256',
                        parameters = asn1_NOVALUE},
                    
                    #'CertificationRequest'{
                        certificationRequestInfo = CertReqInfo,
                        signatureAlgorithm       = SignatureAlgorithm,
                        signature                = SignedCri}
            end
    end.

extension("") ->
    [];
extension(undefined) ->
    [];
extension(Val) ->
    encode_sub_alt_name(Val).


encode_sub_alt_name(Val) ->
    do_encode_sub_alt_name(string:tokens(Val, ";"), []).

do_encode_sub_alt_name([], Res) ->
    {ok, SubjectAltName} =
    'OTP-PUB-KEY':encode('SubjectAltName', Res),
    {ok, Data} =
    'OTP-PUB-KEY':encode('ExtensionRequest',
        [#'Extension'{
                extnID = ?'id-ce-subjectAltName',
                critical = false,
                extnValue = SubjectAltName}]),
    [#'AttributePKCS-10'{
            type   = ?'pkcs-9-at-extensionRequest',
            values = [{asn1_OPENTYPE, iolist_to_binary(Data)}]}];
do_encode_sub_alt_name([" "|T], Res) ->
    do_encode_sub_alt_name(T, Res);
do_encode_sub_alt_name([H|T], Res) ->
    case format_sub_alt_name_string(H) of
        [Val] -> % Treated as IP or DNS depending on value
            case inet:parse_address(Val) of
                {ok,_} -> %% IPv4, IPv6 
                    do_encode_sub_alt_name(["IP:" ++ H] ++ T, Res);
                _ -> %% Assume it is a FQDN
                    do_encode_sub_alt_name(["DNS:" ++ H] ++ T, Res)
            end;
        [Type, "copy"] ->
            info_msg("No support for SubjectAltName: ~p:~p", [Type, "copy"]),
            {error, subAltName_copy_no_support};
        ["DNS", Val] ->
            do_encode_sub_alt_name(T, lists:append(Res, [{dNSName, Val}]));
        ["Dns", Val] ->
            do_encode_sub_alt_name(T, lists:append(Res, [{dNSName, Val}]));
        ["dns", Val] ->
            do_encode_sub_alt_name(T, lists:append(Res, [{dNSName, Val}]));
        ["IP", Val] ->
            case format_ip(Val) of
                {error, Reason} ->
                    {error, Reason};
                IP ->
                    do_encode_sub_alt_name(T, lists:append(Res, IP))
            end;
        ["Ip", Val] ->
            case format_ip(Val) of
                {error, Reason} ->
                    {error, Reason};
                IP ->
                    do_encode_sub_alt_name(T, lists:append(Res, IP))
            end;
        ["ip", Val] ->
            case format_ip(Val) of
                {error, Reason} ->
                    {error, Reason};
                IP ->
                    do_encode_sub_alt_name(T, lists:append(Res, IP))
            end;
        ["URI", Val] ->
            do_encode_sub_alt_name(T,
                lists:append(Res, [{uniformResourceIdentifier, Val}]));
        ["Uri", Val] ->
            do_encode_sub_alt_name(T,
                lists:append(Res, [{uniformResourceIdentifier, Val}]));
        ["uri", Val] ->
            do_encode_sub_alt_name(T,
                lists:append(Res, [{uniformResourceIdentifier, Val}]));
        ["directoryName", Val] ->
            case build_rdnSequence(Val) of
                {error, Reason} ->
                    info_msg("directoryName encoding failed, ~p", [Reason]),
                    {error, {subAltName, Reason}};
                DN ->
                    do_encode_sub_alt_name(T,
                        lists:append(Res, [{directoryName, DN}]))
            end;
        ["DirName", Val] ->
            case build_rdnSequence(Val) of
                {error, Reason} ->
                    info_msg("directoryName encoding failed, ~p", [Reason]),
                    {error, {subAltName, Reason}};
                DN ->
                    do_encode_sub_alt_name(T,
                        lists:append(Res, [{directoryName, DN}]))
            end;
        %%% TODO email, RID, otherName, UTF8
        [Type, Val] -> % No support
            info_msg("No support for SubjectAltName: ~p:~p", [Type, Val]),
            do_encode_sub_alt_name(T, Res)
    end.


format_ip(Val) ->
    case inet:parse_address(Val) of
        {ok, {A1,A2,A3,A4}} -> %% IPv4
            [{iPAddress, list_to_binary([A1,A2,A3,A4])}];
        {ok, {A1,A2,A3,A4,A5,A6,A7,A8}} -> %%  IPv6
            Ipv6Addr = [A1,A2,A3,A4,A5,A6,A7,A8],
            IPv6Bytes = lists:flatten(lists:map(fun(X) -> [X bsr 8, X band 255] end, Ipv6Addr)),
            [{iPAddress, list_to_binary(IPv6Bytes)}];
        {error,_} -> %% Faulty
            info_msg("Faulty ip value SubjectAltName: ~p", [Val]),
            {error, subAltName_ip}
    end.


format_sub_alt_name_string(String) ->
    case string:tokens(String, ":") of
        [Val] ->
            [Val];
        [Type, Val] ->
            [Type, Val];
        [Type|_]  when Type == "IP"; Type == "Ip"; Type == "ip" ->
            ["IP", string:sub_string(String, 4)];
        _ ->
            ["IP", String]
    end.


%%% ----------------------------------------------------------
%%% #           build_rdnSequence(Subject)
%%% Input: string() - Subject name in string format
%%% Output: {directoryName, {rdnSequence, list()}}
%%% Exceptions: 
%%% Description: Build a directoryName tuple.
%%%              Ex. "C=SE,ST=Stockholm,L=Kista,O=Ericsson AB,
%%%                   OU=RO,CN=olle@ericsson.com"
%%% ----------------------------------------------------------
build_rdnSequence(undefined) ->
    {error, missing};
build_rdnSequence(Subject) ->
    build_rdnSequence(string:tokens(Subject, ","), [], undefined).

build_rdnSequence([], List, found_cn) ->
    {rdnSequence, List};
build_rdnSequence([], _, _) ->
    {error, no_cn};
build_rdnSequence([H|T], List, Before) ->
    {{Type, Value}, FoundCn} =
    case string:tokens(H, "=") of
        ["CN"|Val] ->
            {{?'id-at-commonName',
                    av(?'id-at-commonName',
                        {utf8String, lists:flatten(Val)})}, found_cn};
        [" CN"|Val] ->
            {{?'id-at-commonName',
                    av(?'id-at-commonName',
                        {utf8String, lists:flatten(Val)})}, found_cn};
        ["L"|Val] ->
            {{?'id-at-localityName',
                    av(?'id-at-localityName',
                        {printableString, lists:flatten(Val)})}, undefined};
        [" L"|Val] ->
            {{?'id-at-localityName',
                    av(?'id-at-localityName',
                        {printableString, lists:flatten(Val)})}, undefined};
        ["ST"|Val] ->
            {{?'id-at-stateOrProvinceName',
                av(?'id-at-stateOrProvinceName',
                    {printableString, lists:flatten(Val)})}, undefined};
        [" ST"|Val] ->
            {{?'id-at-stateOrProvinceName',
                av(?'id-at-stateOrProvinceName',
                    {printableString, lists:flatten(Val)})}, undefined};
        ["O"|Val] ->
            {{?'id-at-organizationName',
                av(?'id-at-organizationName',
                    {printableString, lists:flatten(Val)})}, undefined};
        [" O"|Val] ->
            {{?'id-at-organizationName',
                av(?'id-at-organizationName',
                    {printableString, lists:flatten(Val)})}, undefined};
        ["OU"|Val] ->
            {{?'id-at-organizationalUnitName',
                av(?'id-at-organizationalUnitName',
                    {printableString, lists:flatten(Val)})}, undefined};
        [" OU"|Val] ->
            {{?'id-at-organizationalUnitName',
                av(?'id-at-organizationalUnitName',
                    {printableString, lists:flatten(Val)})}, undefined};
        ["E"|Val] ->
            {{?'id-emailAddress',
                av(?'id-emailAddress', lists:flatten(Val))}, undefined};
        [" E"|Val] ->
            {{?'id-emailAddress',
                av(?'id-emailAddress', lists:flatten(Val))}, undefined};
        ["C"|Val] ->
            {{?'id-at-countryName',
                    av(?'id-at-countryName', lists:flatten(Val))}, undefined};
        [" C"|Val] ->
            {{?'id-at-countryName',
                    av(?'id-at-countryName', lists:flatten(Val))}, undefined};
        ["SERIALNUMBER"|Val] ->
            {{?'id-at-serialNumber',
                av(?'id-at-serialNumber', lists:flatten(Val))}, undefined};
        [" SERIALNUMBER"|Val] ->
            {{?'id-at-serialNumber',
                av(?'id-at-serialNumber', lists:flatten(Val))}, undefined};
        ["SN"|Val] ->
            {{?'id-at-serialNumber',
                av(?'id-at-serialNumber', lists:flatten(Val))}, undefined};
        [" SN"|Val] ->
            {{?'id-at-serialNumber',
                av(?'id-at-serialNumber', lists:flatten(Val))}, undefined};
        _ ->
            {{unsupported,undefined}, undefined}
    end,
    case Type of
        unsupported ->
            build_rdnSequence(T, List, Before);
        _ ->
            case {Before, FoundCn} of
                {found_cn, found_cn} ->
                    {error, several_cn};
                {undefined, found_cn} ->
                    NewTypeValue = 
                    [#'AttributeTypeAndValue'{type  = Type, value = Value}],
                    build_rdnSequence(T, lists:append(List,[NewTypeValue]), found_cn);
                _ ->
                    NewTypeValue = 
                    [#'AttributeTypeAndValue'{type  = Type, value = Value}],
                    build_rdnSequence(T, lists:append(List,[NewTypeValue]), Before)
            end
    end.

av(Id, Val) ->
    Fun = 'OTP-PUB-KEY':'getenc_SupportedAttributeTypeAndValues'(Id),
    {Data, _Len} = Fun('Type', Val, ignore),
    iolist_to_binary(Data).


%%% ----------------------------------------------------------
%%% #          get_enrollment_support()
%%% Input: - 
%%% Output: {OfflineCsr, OfflinePkcs12, Scep, Cmp} - true|false
%%% Exceptions: 
%%% Description: Returns if enrollment option is supported
%%%              or not. True if supported and false if not.
%%% ----------------------------------------------------------
get_enrollment_support() ->
    [CMC] = mnesia:dirty_read(certMCapabilities, {"1","1","1","1","1"}),
    ES    = CMC#certMCapabilities.enrollmentSupport,  
    {
        lists:member(?EnrollmentSupport_OFFLINE_CSR, ES),
        lists:member(?EnrollmentSupport_OFFLINE_PKCS12, ES),
        lists:member(?EnrollmentSupport_ONLINE_SCEP, ES),
        lists:member(?EnrollmentSupport_ONLINE_CMP, ES)
    }.

%%% ----------------------------------------------------------
%%% #          get_enrollment_server(EnrollmentServerGroupId)
%%% Input:  EnrollmentServerGroupId
%%% Output: no_enrollment_server | enrollment server object 
%%% Exceptions: 
%%% Description: Return the enrollment servers uri list.
%%% ----------------------------------------------------------
get_enrollment_server(EnrollmentServerGroupId) ->
    %% Get configured enrollment servers and the enrollment support
    List = get_enrollment_servers(EnrollmentServerGroupId),
    {_,_,SCEP,CMPv2} = get_enrollment_support(),
    get_enrollment_server(List, SCEP, CMPv2, []).

get_enrollment_server([],_,_,[]) ->
    no_enrollment_server;
get_enrollment_server([],_,_,List) ->
    List;
get_enrollment_server([Obj|T],SCEP,CMPv2,List) ->
    case {Obj#enrollmentServer.protocol, SCEP, CMPv2} of
        {?EnrollmentProtocol_CMP, _, true} ->
            NewList = lists:append(List, [Obj#enrollmentServer.uri]),
            get_enrollment_server(T, SCEP, CMPv2, NewList);
        _ ->
            get_enrollment_server(T, SCEP, CMPv2, List)
    end.


%%% ----------------------------------------------------------
%%% #          get_enrollment_servers(EnrollmentServerGroupId)
%%% Input:  EnrollmentServerGroupId
%%% Output: List of enrollment server objects | []
%%% Exceptions: 
%%% Description: Return a list of all enrollment servers
%%%              matching the EnrollmentServerGroupId.
%%% ----------------------------------------------------------
get_enrollment_servers(EnrollmentServerGroupId) ->
    Id = certLib:decode_moref(EnrollmentServerGroupId),
    get_enrollment_servers(mnesia:dirty_first(enrollmentServer), Id, []).

get_enrollment_servers('$end_of_table',_, List) ->
    List;
get_enrollment_servers({A1,A2,A3,A4,A5,A6}, {A1,A2,A3,A4,A5}, List) ->
    [Obj]   = mnesia:dirty_read(enrollmentServer, {A1,A2,A3,A4,A5,A6}),
    NextId  = mnesia:dirty_next(enrollmentServer, {A1,A2,A3,A4,A5,A6}),
    %% fix URI in case of cmp HV58854
    {Server, Arg1, Arg2, Arg3, URI, Arg4} = Obj,
    HttpURI = server_uri_to_http(URI),
    NewList = lists:append(List, [{Server, Arg1, Arg2, Arg3, HttpURI, Arg4}]),
    get_enrollment_servers(NextId, {A1,A2,A3,A4,A5}, NewList);
get_enrollment_servers({A1,A2,A3,A4,Another,A6}, Id, List) ->
    NextId = mnesia:dirty_next(enrollmentServer, {A1,A2,A3,A4,Another,A6}),
    get_enrollment_servers(NextId, Id, List).


server_uri_to_http(URI) ->
    %% HV58854, replaces cmp with http in URI
    %% HV64748, fix for IPv6 addresses
    case http_uri:parse(URI, [{ipv6_host_with_brackets, true}]) of
        {ok, {cmp, _UserInfo, Host, Port, Path, _Query}} ->
            string:join(["http://",
                          Host,
                          ":",
                          integer_to_list(Port),
                          Path], "");
        _ ->
            URI
    end.

%% HV81235 Refactoring
cert_expiration_alarm_state(TimeLeft, Threshold)
    when TimeLeft > Threshold, TimeLeft > 7 * ?DAY_IN_SECONDS ->
    undefined;
cert_expiration_alarm_state(TimeLeft, Threshold)
    when TimeLeft < ?MINUTE_IN_SECONDS->
    expired;
cert_expiration_alarm_state(TimeLeft, Threshold)
    when TimeLeft < 7 * ?DAY_IN_SECONDS; TimeLeft < round(Threshold / 10)->
    major;
cert_expiration_alarm_state(TimeLeft, Threshold)
    when TimeLeft < round(Threshold / 3)->
    minor;
cert_expiration_alarm_state(TimeLeft, Threshold) ->
    warning.

raise_cert_expire_alarm(Key, Level, Msg, NotBefore, NotAfter, RaiseAlarm) ->
    case {RaiseAlarm, Level} of
        {true, expired} -> 
            certAlarm:clear(nc, cert_to_expire, Key);
        {true, undefined} -> 
            certAlarm:clear(nc, cert_to_expire, Key);
        {true, _} -> 
            certAlarm:send(nc, cert_to_expire, Key, Level, Msg);
        {false, _} ->
            certAlarm:clear(nc, cert_to_expire, Key)
    end.

%%% ----------------------------------------------------------
%%% #   handle_calc_timeout(Key, NotBefore, NotAfter)
%%% Input:  
%%% Output: 
%%% Exceptions: 
%%% Description: Start expire timer
%%% ----------------------------------------------------------
handle_calc_timeout(Key, NotBefore, NotAfter) ->
    %% ExpireAlarmThreshold, when first to check certificate
    [NC] = read_node_credential(Key),
    EAT  = NC#nodeCredential.expiryAlarmThreshold * ?DAY_IN_SECONDS, % unit in sec
    RenewalMode = NC#nodeCredential.renewalMode,
    %% Time left before expire 
    {D2, T2} = calendar:time_difference(calendar:universal_time(), NotAfter),
    TimeLeft = D2 * ?DAY_IN_SECONDS + calendar:time_to_seconds(T2), % unit in sec

    Level = cert_expiration_alarm_state(TimeLeft, EAT),

    DebugTimeout =
    case mnesia:dirty_read(certNcState, Key) of
        [] -> undefined;
        [State] -> State#certNcState.alarm_timer_value
    end,

    %% HV81235
    Timeout =
        case DebugTimeout of
            undefined ->
                case TimeLeft of
                    X when X > 7 * ?DAY_IN_SECONDS -> ?DAY_IN_SECONDS;
                    X when X > 3 * ?DAY_IN_SECONDS -> 12 * ?HOUR_IN_SECONDS;
                    X when X >= ?DAY_IN_SECONDS -> 6 * ?HOUR_IN_SECONDS;
                    X when X < ?MINUTE_IN_SECONDS -> 1;
                    X when X < 10 * ?MINUTE_IN_SECONDS -> ?MINUTE_IN_SECONDS;
                    X when X < ?HOUR_IN_SECONDS -> 10 * ?MINUTE_IN_SECONDS;
                    X when X < ?DAY_IN_SECONDS -> ?HOUR_IN_SECONDS
                end;
            _ -> DebugTimeout
        end,

    {CertExpireAlarm, Args} =
    case Level of
        undefined ->
            CalcArgs = {calc_timeout, Key, NotBefore, NotAfter},
            {false, CalcArgs};
        _ ->
            %% minor, warning, major
            ExpireArgs = {expire_timeout, Level, Key, NotBefore, NotAfter},
            {true, ExpireArgs}
    end,

    Msg = "Node Credential need to be renewed",
    case {CertExpireAlarm, RenewalMode} of
        {true, ?RenewalMode_MANUAL} ->
            raise_cert_expire_alarm(Key, Level, Msg, NotBefore, NotAfter, true);
        {true, ?RenewalMode_AUTOMATIC} ->
            raise_cert_expire_alarm(Key, Level, Msg, NotBefore, NotAfter, false);
        _ -> certAlarm:clear(nc, cert_to_expire, Key)
    end,

    TRef = erlang:send_after(Timeout * 1000, self(), Args),
    case get({expire_timer_nc_ref, Key}) of
        undefined ->
            ok;
        OldTRef ->
            erlang:cancel_timer(OldTRef)
    end,
    put({expire_timer_nc_ref, Key}, TRef).

%%% ----------------------------------------------------------
%%% #   handle_expire_timeout()   
%%% Input:  -
%%% Output: ok
%%% Exceptions: 
%%% Description: When the expire timer get hit
%%% ----------------------------------------------------------
handle_expire_timeout(expired, Key, _, _) ->
    %% Remove node credential!!!!
    info_msg("CRITICAL Node Certificate: ~p is now invalid~n", [Key]),
    certAlarm:clear(nc, cert_to_expire, Key),
    Msg = "Node Credential has been removed due to expire date",
    certAlarm:send(nc, cert_not_available, Key, critical, Msg),
    certSecStore:remove_nc_dir(Key),
    [Obj] = mnesia:dirty_read(certNC, Key),
    mnesia:dirty_write(Obj#certNC{timeout = expired,
                                  cert    = undefined}),
    %% Set node credential as expired
    [NC] = read_node_credential(Key),
    mnesia:dirty_write(NC#nodeCredential{certificateState =
            ?CertificateState_EXPIRED}),
    certSub:trig(nc, Key),
    ok;
handle_expire_timeout(Level, Key, NotBefore, NotAfter) ->
    [NC] = read_node_credential(Key),
    Start =
    case NC#nodeCredential.enrollmentProgress of
        undefined ->
            ok;
        Progress ->
            case Progress#'AsyncActionProgress'.state of
                ?ActionStateType_RUNNING ->
                    info_msg("Skip renewal, enrollment ongoing: ~p~n", [Key]),
                    nok;
                ?ActionStateType_CANCELLING ->
                    info_msg("Skip renewal, enrollment cancelling ongoing: ~p~n", [Key]),
                    nok;
                _ ->
                    ok
            end
    end,

    case Start of
        ok ->
            Msg = "Node Credential need to be renewed",
            case renewal(Key, auto, "") of
                ok ->
                    ok;
                {fail, MsgFail} ->
                    enrollment_failure_update(MsgFail),
                    certAlarm:send(nc, cert_enroll_failed, Key, warning, MsgFail),
                    handle_calc_timeout(Key, NotBefore, NotAfter);
                _ ->
                    handle_calc_timeout(Key, NotBefore, NotAfter)
            end,
            [Obj] = mnesia:dirty_read(certNC, Key),
            mnesia:dirty_write(Obj#certNC{timeout = Level});
        _ ->
            handle_calc_timeout(Key, NotBefore, NotAfter)
    end,
    ok.

%% The renewal function is used for both renewal and retries
renewal(Key, Type, FailURI) when Type == auto; Type == force ->
    %% This entry is executed first time for auto and force
    put(number_of_tries, undefined),
    case get(retry_cmp_timer_ref) of
        undefined ->
            ok;
        TRef3 ->
            erlang:cancel_timer(TRef3),
            put(retry_cmp_timer_ref, undefined)
    end,
    NC = read_node_credential(Key),
    do_renewal0(Key, Type, FailURI, NC);
renewal(Key, Type, FailURI) ->
    case get(enrollment_timer_ref) of
        undefined ->
            ok;
        _ -> %% ai or timer ref
            NC = read_node_credential(Key),
            do_renewal0(Key, Type, FailURI, NC)
    end.

update_progress_renewal_started(Type) when Type == auto; Type == force ->
    Msg = case Type of 
            auto ->
            "Online enrollment started automatic due to expire date";
            force ->
            "Online enrollment started automatic due to incomplete node credential"
    end,
    put(auto_renewal, true),
    StartTime = comsaI:iso_time(os:timestamp(),
        extended),
    update_progress([
            {additionalInfoClear, Msg},
            {progressPercentage, 0},
            {result, ?ActionResultType_NOT_AVAILABLE},
            {resultInfo, ""},
            {state, ?ActionStateType_RUNNING},
            {timeActionStarted, StartTime}]);
update_progress_renewal_started(Type) ->
    ok.

do_renewal0(Key, Type, FailURI, []) ->
    {error, no_config};
do_renewal0(Key, Type, FailURI, [NC]) ->
    RenewalMod     = NC#nodeCredential.renewalMode,
    {_, _, _, Cmp} = get_enrollment_support(),
    do_renewal1(Key, Type, FailURI, NC, RenewalMod, Cmp).

do_renewal1(_Key, _Type, _FailURI, _NC, _, false) ->
    {error, no_cmpv2_support};
do_renewal1(_Key, auto, _FailURI, _NC, ?RenewalMode_MANUAL, true) ->
    %% No automatic renewal activated
    {error, manual};
do_renewal1(Key, Type, FailURI, NC, _, true) ->
    case get(number_of_tries) of
        undefined ->
            do_renewal2(Key, Type, FailURI, NC);
        NrOfTries when NrOfTries == 150 ->
            info_msg("Too many CMPv2 ~p retries, backpressure 1 min activated",
                [Key]),
            update_progress([
                    {additionalInfo, "Too many CMPv2 retries, " ++
                        "backpressure 1 min activated"},
                    {progressPercentage, 0},
                    {result, ?ActionResultType_NOT_AVAILABLE},
                    {resultInfo, ""},
                    {state, ?ActionStateType_RUNNING}]),
            TRef = erlang:send_after(60000, self(),
                {retry_cmp, Key, Type, FailURI, NC}),
            put(retry_cmp_timer_ref, TRef);
        NrOfTries when NrOfTries == 179 ->
            info_msg(
                "Too many CMPv2 ~p retries, backpressure 15 min activated",
                [Key]),
            update_progress([
                    {additionalInfo, "Too many CMPv2 retries, " ++
                        "backpressure 15 min activated"},
                    {progressPercentage, 0},
                    {result, ?ActionResultType_NOT_AVAILABLE},
                    {resultInfo, ""},
                    {state, ?ActionStateType_RUNNING}]),
            TRef = erlang:send_after(900000, self(),
                {retry_cmp, Key, Type, FailURI, NC}),
            put(retry_cmp_timer_ref, TRef);
        NrOfTries when NrOfTries > 179 ->
            TRef = erlang:send_after(900000, self(),
                {retry_cmp, Key, Type, FailURI, NC}),
            put(retry_cmp_timer_ref, TRef);
        NrOfTries when NrOfTries > 150 ->
            TRef = erlang:send_after(60000, self(),
                {retry_cmp, Key, Type, FailURI, NC}),
            put(retry_cmp_timer_ref, TRef);
        _ -> %% First 180 tries, around 5 minutes, 2 seconds per try 
            TRef = erlang:send_after(2000, self(),
                {retry_cmp, Key, Type, FailURI, NC}),
            put(retry_cmp_timer_ref, TRef)
    end.

do_renewal2(Key, Type, FailURI, NC) ->
    %% Do renewal using online enrollment
    certAlarm:clear(nc, cert_enroll_failed, Key),
    case check_online_enrollment_mandatory_params() of
        ok ->
            KeyInfo        = certKey:format_key_info(NC#nodeCredential.keyInfo),
            SubjectName    = NC#nodeCredential.subjectName,
            SubjectAltName = NC#nodeCredential.subjectAltName,
            update_progress_renewal_started(Type),
            start_enrollment_timer(Key, NC),
            Csr = gen_csr(KeyInfo, SubjectName, SubjectAltName),
            do_renewal3(Key, FailURI, NC, Csr, SubjectName);
        {fail, Msg} ->
            update_progress_renewal_started(Type),
            {fail, Msg}
    end.

do_renewal3(_Key, _FailURI, _NC, {error, Reason}, _) ->
    {error, Reason};
do_renewal3(Key, FailURI, NC, Csr, SubjectName) ->
    Timer = NC#nodeCredential.enrollmentTimer,
    EA = mnesia:dirty_read(enrollmentAuthority,
        certLib:decode_moref(NC#nodeCredential.enrollmentAuthority)),
    do_renewal4(Key, FailURI, NC, Csr, SubjectName, EA, Timer).


do_renewal4(Key, FailURI, NC, Csr, SubjectName, [EA], Timer) ->
    AuthorityName = EA#enrollmentAuthority.enrollmentAuthorityName,
    EnrollCaCertDn = EA#enrollmentAuthority.enrollmentCaCertificate,
    EnrollCaCert = certServer:get_tc(EnrollCaCertDn),
    EnrollCaFingerpr = EA#enrollmentAuthority.enrollmentCaFingerprint,
    URIs = get_enrollment_server(NC#nodeCredential.enrollmentServerGroup),
    %% Put failing uri to the end of the list
    NewURIs = move_failed_uri_to_end(FailURI, URIs, []),
    Result = make_enrollment(Csr, Timer, AuthorityName,
        SubjectName, Key, NewURIs, EnrollCaCert, EnrollCaFingerpr),
    do_renewal4(Key, Result).

do_renewal4(Key, ok) ->
    %% Step try counter
    case get(number_of_tries) of
        undefined ->
            put(number_of_tries, 1);
        NrOfTries ->
            put(number_of_tries, NrOfTries + 1)
    end,
    ok;
do_renewal4(Key, {error, can_not_verify_ca_ra_cert}) ->
    handle_fail({cleanup, [{additionalInfo, "Found no valid CA/RA certificate or fingerprint"},
                {resultInfo, "Online enrollment failed"}]}),
    {error, can_not_verify_ca_ra_cert};
do_renewal4(Key, {error, no_working_uri}) ->
    handle_fail({cleanup, [{additionalInfo, "No working CMPv2 server uri"},
                {resultInfo, "Online enrollment failed"}]}),
    {error, no_working_uri}.

force_renewal(Key) ->
    update_action(no_action),
    renewal(Key, force, "").
    

move_failed_uri_to_end(_, no_enrollment_server, _) ->
    [];
move_failed_uri_to_end(_FailURI, [], List) ->
    List;
move_failed_uri_to_end(FailURI, [FailURI|T], List) ->
    T ++ List ++ [FailURI];
move_failed_uri_to_end(FailURI, [URI|URIs], List) ->
    move_failed_uri_to_end(FailURI, URIs, List ++ [URI]).


%%% ----------------------------------------------------------
%%% #   handle_enrollment_timeout()   
%%% Input:  -
%%% Output: ok
%%% Exceptions: 
%%% Description: When the configured enrollment timer ends
%%% ----------------------------------------------------------
handle_enrollment_timeout() ->
    %% Kill ongoing CMPv2 retry if any
    case get(retry_cmp_timer_ref) of
        undefined ->
            ok;
        TRef ->
            erlang:cancel_timer(TRef),
            put(retry_cmp_timer_ref, undefined)
    end,
    % Kill uri resolution retry if any
    case get(uri_resolution_timer_ref) of
        undefined ->
            ok;
        UriResolveTRef ->
            erlang:cancel_timer(UriResolveTRef),
            put(uri_resolution_timer_ref, undefined)
    end,
    %% Check if timer already canceled, if so ignore this timeout
    Key = get(key),
    certCMPv2:cancel(Key),
    case get(enrollment_timer_ref) of
        undefined ->
            ok;
        ai ->
            ok;
        _ -> % existing timer ref
            put(enrollment_timer_ref, undefined),
            %% Update nodeCredential with content and state
            certSecStore:remove_nc_key(Key, "csr.key"),
            case mnesia:dirty_read(certNC, Key) of
                [] ->
                    ok;
                [NC] ->
                    mnesia:dirty_write(NC#certNC{csr = undefined})
            end,
            update_action(no_action),
            handle_fail([{resultInfo, "CMPv2 enrollment timed out."}]),
            Msg = "Online enrollment, enrollment timeout",
            [NodeCred] = read_node_credential(Key),
            %% HV80444
            case get(auto_renewal) of
                true ->
                    %% Send automatic enrollment failed alarm
                    certAlarm:send(nc, cert_enroll_failed, Key, warning, Msg);
                _ ->
                    ok
            end,
            restart_to_expire_timer(Key)
    end,
    ok.

%%% ----------------------------------------------------------
%%% #          handle_update_status(Type, Data)
%%% Input:  
%%% Output: 
%%% Exceptions: 
%%% Description: Handles the status received from e.g. CMPv2
%%% ----------------------------------------------------------
handle_update_status(cmpv2_approved_cert, [NcCert|_] = Cert) ->
    %% Store cert and privkey in SecEE
    %% Update nodeCredential with content and state
    Key = get(key),

    %% Stop enrollment timer
    case get(enrollment_timer_ref) of
    undefined ->
        ok;
        ai ->
            put(enrollment_timer_ref, undefined);
    Tref1 ->
        erlang:cancel_timer(Tref1),
        put(enrollment_timer_ref, undefined)
    end,

    {ok, PrivKeyBin} = certSecStore:get_nc_key(get(key), "csr.key"),

    %% Check that privkey matches
    Start =
    case certVerify:verify_nc(NcCert, binary_to_term(PrivKeyBin)) of
        valid ->
            case certSecStore:get_nc_key(Key, "nc.key") of
                {ok, PrivOldKey} ->
                    info_msg("Stored old nc key ~p", [Key]),
                    certSecStore:remove_nc_key(Key, "nc_old.key"),
                    certSecStore:put_nc_key(Key, PrivOldKey, "nc_old.key"),
                    certSecStore:remove_nc_key(Key, "nc.key");
                _ ->
                    ok
            end,
            certSecStore:remove_nc_key(Key, "csr.key"),
            certSecStore:put_nc_key(Key, PrivKeyBin, "nc.key"),
            [NC] = read_node_credential(Key),
            CC = certLib:read_cert_metadata(NcCert),
            update_action_and_cert(no_action, Cert),
            NewNC = NC#nodeCredential{
                certificateContent = CC,
                certificateState   = ?CertificateState_VALID},
            ok = mnesia:dirty_write(NewNC),
            handle_success([{resultInfo, mk_ext_moref(Key)}]),
            %% Stop old expire timer in case of renewal
            case get({expire_timer_nc_ref, Key}) of
                undefined ->
                    ok;
                Tref2 ->
                    erlang:cancel_timer(Tref2),
                    put({expire_timer_nc_ref, Key}, undefined)
            end,
            %% Stop AI mode if active
            case mnesia:dirty_read(certNC, Key) of
                [] ->
                    ok;
                [Obj] ->
                    case Obj#certNC.ai of
                        false ->
                            ok;
                        true ->
                            mnesia:dirty_write(Obj#certNC{ai = false})
                    end
            end,
            %% Clear alarms if any
            certAlarm:clear(nc, cert_not_available, Key),
            certAlarm:clear(nc, cert_to_expire, Key),
            certAlarm:clear(nc, cert_enroll_failed, Key),
            certSub:trig(nc, Key),
            ok;
        {failed, Reason} ->
            ReasonText =
            case Reason of
                no_match ->
                    "No match between certificate and private key";
                invalid_date ->
                    "Certificate date is not valid";
                Faulty_KeyUsage ->
                    info_msg("Faulty KeyUsage: ~w~n", [Faulty_KeyUsage]),
                    "Faulty keyUsage"
            end,
            certLib:sec_log("", "Certificate got via CMPv2, (" ++
                element(5, Key) ++ "), " ++ ReasonText),
            update_action(no_action),
            handle_fail([{progressInfo, "CMPv2 enrollment got canceled due to " ++
                        "error, reason: " ++ ReasonText},
                        {resultInfo, mk_ext_moref(Key)}]),
            [NodeCred] = read_node_credential(Key),
            case NodeCred#nodeCredential.renewalMode of
                ?RenewalMode_AUTOMATIC ->
                    %% Send automatic enrollment failed alarm
                    Msg = "Online enrollment, validation of the new cert failed",
                    certAlarm:send(nc, cert_enroll_failed, Key, warning, Msg);
                _ ->
                    ok
            end,
            nok
    end,
    put(number_of_tries, undefined),
    certCMPv2:cancel(Key),
    case Start of
        ok ->
            start_to_expire_timer(Key, Cert);
        _ ->
            restart_to_expire_timer(Key)
    end,
    ok;
handle_update_status(cmpv2_enrollment_failed, Reason) ->
    case print_cmp_msg() of
        print ->
            update_progress([
                    {additionalInfo, "CMPv2 enrollment failed (will retry) due to " ++
                        "error, reason: " ++ Reason}]),
            ok;
        no_print ->
            ok
    end;
handle_update_status(cmpv2_received_msg, TypeMsg) ->
    case print_cmp_msg() of
        print ->
            update_progress([
                    {additionalInfo, "Node received CMPv2 message: "++
                        atom_to_list(TypeMsg)}]),
            ok;
        no_print ->
            ok
    end;
handle_update_status(cmpv2_sent_msg, TypeMsg) ->
    case print_cmp_msg() of
        print ->
            update_progress([
                    {additionalInfo, "Node sent CMPv2 message: " ++
                        atom_to_list(TypeMsg)}]),
            ok;
        no_print ->
            ok
    end;
handle_update_status(cmpv2_msg_error,
    {MsgType, Status, StatusString, FailInfo}) ->
    FailInfoString = case FailInfo of
                         [] -> "no_fail_info_available";
                         [FailAtom|_] -> atom_to_list(FailAtom);
                         Atom when is_atom(Atom) ->
                             atom_to_list(Atom)
                     end,
    LStatus = lists:flatten(io_lib:format("~p",[Status])),
    case print_cmp_msg() of
        print ->
            update_progress([
                    {additionalInfo, "CMPv2 enrollment failed (will retry) due to " ++
                        "error message " ++ atom_to_list(MsgType) ++
                        " from CMPv2 server, status: " ++ LStatus ++
                        ", statusString: " ++ StatusString ++
                        ", failInfo: " ++ FailInfoString}]),
            ok;
        no_print ->
            ok
    end;
handle_update_status(restart, {URI, Reason}) ->
    case print_cmp_msg() of
        print ->
            info_msg("update status CMPv2, restart, ~p~n", [Reason]),
            update_progress([{additionalInfo, Reason ++ " using uri: " ++
                        URI ++ ". Will retry."}]);
        no_print ->
            ok
    end,
    certCMPv2:cancel(get(key)),
    renewal(get(key), restart, URI),
    ok;
handle_update_status(restart, Reason) ->
    case print_cmp_msg() of
        print ->
            info_msg("update status CMPv2, restart, ~p~n", [Reason]),
            update_progress([{additionalInfo, Reason}]);
        no_print ->
            ok
    end,
    certCMPv2:cancel(get(key)),
    renewal(get(key), restart, ""),   
    ok.

print_cmp_msg() ->
    case get(number_of_tries) of
        undefined ->
            print;
        NrOfTries when NrOfTries > 40 ->
            no_print;
        _ ->
            print
    end.

%%%%%%%%%%%%%%%
protocol_to_text(Proto) ->
    atom_to_list(Proto).

get_proto_from_uri(Uri) ->
     case ftpI:parse_uri(Uri) of
        {ok, {Proto, User, Host, _Port, _Path, _Query}} when Proto =:= sftp; Proto =:= ftpes ->
            case {User, Host} of
                {[], _} -> {error, "Missing user information in URI"};
                {_, []} -> {error, "Missing host information in URI"};
                _ -> Proto
            end;
         {ok, {Proto, _, _, _, _, _}} ->
             {error, "Unsupported " ++ atom_to_list(Proto) ++" protocol for this operation"};
         {error, _Reason} ->
              {error, "Wrong URI format"}
     end.

read_node_credential(Key) ->
    Fun = fun() -> mnesia:read(nodeCredential, Key) end,
    Ret = 
    case mnesia:is_transaction() of
        true -> Fun();
        false -> mnesia:transaction(Fun)
    end,
    NC =
    case Ret of
        {atomic, Cred} -> Cred;
        _ -> Ret
    end,
    NC.


%%% ----------------------------------------------------------
%%% #   get_src_if(Key)   
%%% Input:  Key - {_,_,_,_, Index}
%%% Output: SrcIf - atom(oam|infra)
%%% Exceptions: 
%%% Description: Get the source interface to used for for example
%%%              for CMPv2. It will always be the value oam,
%%%              except for R-VNFM, then it can be
%%%              the value infra aswell.
%%% ----------------------------------------------------------
get_src_if(Key) ->
    case get(src_if) of
        undefined ->
            case {sysEnv:vrcs(), swmI:node_type()} of
                {true, "R-VNFM"} ->
                    %% Check sec what the Key is used for oam or infra?
                    case apply(secI, nc_used_by_oam_or_infra, [Key]) of
                        infra ->
                            put(src_if, infra),
                            infra;
                        _ ->
                            put(src_if, oam),
                            oam
                    end;
                _ ->
                    put(src_if, oam),
                    oam
            end;
        SrcIf ->
            SrcIf
    end.


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

