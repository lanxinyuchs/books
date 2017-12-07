%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certSecCredu.erl %
%%% @author evadumb
%%% @copyright Ericsson AB 2017
%%% @version /main/R11A/23

%%% @doc ==sec_credu_api service==
%%% This module implements sec_credu_api interface service. It provides COM with
%%% access to NodeCredential certificate and key, and TrustCategory certificates.
%%% It also implements a mechanism for subscribing to certificate events.
%%% @end

-module(certSecCredu).
-vsn('/main/R11A/23').
-date('2017-10-18').
-author('evadumb').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% -------------------------------------------------------------------
%%% #1.    REVISION LOG
%%% -------------------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ---------------------------------
%%% R11A/1     2017-08-28 ekurnik     Created
%%% R11A/2     2017-08-30 estifil     gen_server behaviour added
%%% R11A/3     2017-08-31 evadumb     Added subscriber functions 
%%% R11A/4     2017-08-31 enekdav     Added directory functions
%%% R11A/5     2017-09-01 enekdav     Small change to mo_ref variables
%%% R11A/6     2017-09-04 edartop     Added Data serialization functions
%%% R11A/7     2017-09-04 ekurnik     Added DN manipulation functions
%%% R11A/8     2017-09-07 emirbos     Added handle_sec_credu functions, part I
%%% R11A/9     2017-09-07 estifil     Added cec_takeover
%%% R11A/10    2017-09-08 evadumb     Changed handle_info, added handle_sec_credu_request,
%%%                                   handle_sec_credu_response, tcat_list_to_binary,
%%%                                   and version_string_to_tuple functions
%%% R11A/11    2017-09-11 emirbos     Added handle_sec_credu functions, part II
%%% R11A/12    2017-09-12 edartop     Added additional Data serialization functions
%%% R11A/13    2017-09-15 emirbos     Added get_tcat_certs_and_MoRefs/1
%%%                                   Added handle_cert_event functions
%%% R11A/14    2017-09-18 edartop     Added activate function
%%% R11A/16    2017-09-25 evadumb     Added LOG traces
%%% R11A/17    2017-10-03 emirbos     handle_sec_credu_subscribe/4 changed
%%% R11A/18    2017-10-05 enatdok     Fixed minor issues
%%% R11A/19    2017-10-05 emirbos     store_nc_or_tcat_files/3 changed
%%% R11A/20    2017-10-10 enekdav     Added test_finalize function for testing purposes
%%% R11A/21    2017-10-17 enekdav     Separated sec_socket into sec_response and sec_event functions
%%% R11A/22    2017-10-17 ekurnik     Fixed endline encoding, removed export_all, cleaned up unused functions
%%% R11A/23    2017-10-18 evadumb     Added finalize_subscriber, changed add_subscriber/2 to add_subscriber/3
%%% -------------------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% callback from CEC
-export([cec_setup/1, cec_takeover/1]).

%% callback from CERT
-export([cert_event/1]).

-export([start_link/0, stop/0, activate/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
% gen_server callbacks
-export([init/1,
         handle_call/3, 
         handle_cast/2,
         handle_info/2, 
         terminate/2, 
         code_change/3]).

%%% ----------------------------------------------------------
%%% #2.3   EXPORTED OTHER FUNCTIONS
%%% ----------------------------------------------------------

%% export private functions for eunit
-export([mo_ref_to_id/1,
         node_credential_mo_ref/1,
         trust_category_mo_ref/1,
         
         generate_client_id/0,
         add_subscriber/3,
         remove_subscriber/2,
         finalize_subscriber/2,
         update_subscriber/2,
         get_subscriber_by_id/2,
         get_subscriber_by_comm_socket/2,
         get_subscribers_list/0,
         
         get_selection_object/1,
         set_selection_object/2,
         close_selection_object/1,
         
         generate_subscription_id/0,
         get_subscription_id/2,
         add_subscription/2,
         add_subscription/3,
         remove_subscription/2,
         unsubscribe_from_all/2,
         get_subscription_by_id/2,
         get_subscription_by_mo_ref/2,
         
         create_directory_structure/0,
         delete_directory_structure/0,
         
         get_nc_cert_filename/1,
         get_nc_key_filename/1,
         nc_cert_file_exists/1,
         nc_key_file_exists/1,
         write_nc_cert_to_dir/2,
         write_nc_key_to_dir/2,
         remove_nc_cert_from_dir/1,
         remove_nc_key_from_dir/1,
         
         get_tcat_dirname/1,
         tcat_dir_exists/1,
         create_tcat_dir/1,
         delete_tcat_dir/1,
         
         get_tc_cert_filename/1,
         write_tc_cert_to_dir/3,
         
         create_response_message/1,
         
         handle_sec_credu_initialize/2,
         handle_sec_credu_finalize/2,
         handle_sec_credu_selectionobject_get/2,
         handle_sec_credu_unsubscribe/4,
         handle_sec_credu_nodecredential_subscribe/2,
         handle_sec_credu_nodecredential_unsubscribe/2,
         handle_sec_credu_trustcategory_subscribe/2,
         handle_sec_credu_trustcategory_unsubscribe/2,
         handle_sec_credu_nodecredential_get_cert/2,
         handle_sec_credu_nodecredential_get_key/2,
         handle_sec_credu_trustcategory_get/2,
         
         send_response/2,
         send_event/2,
         serialize_integer/2,
         serialize_integer32/2,
         serialize_string_result/2,
         serialize_binary_result/2,
         
         handle_sec_credu_request/4,
         handle_sec_credu_response/2,
         
         tcat_list_to_binary/2,
         
         version_string_to_tuple/1,
         version_tuple_to_string/1
         ]).

%% function for common test
-export([test_finalize/1,
         get_cert/1,
         get_tcat_certs_and_MoRefs/1]).




%%%===================================================================
%%% Include files
%%%===================================================================

-include("certSecCredu.hrl").

-define(SERVER, ?MODULE).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

init(_Arg) ->
    create_directory_structure(),
    {ok, #state{}}.

activate() ->
    gen_server:cast(?SERVER, {cec_register}).

cert_event(MoRef) ->
    %% wait before sending to avoid race conditions
    timer:apply_after(?CERT_EVENT_OFFSET, gen_server, cast, [?MODULE, {cert_event, MoRef}]).

test_finalize(Id) ->
    gen_server:call(?MODULE, {finalize, Id}).

get_subscribers_list() ->
    ?LOG("Getting subscribers list~n"),
    gen_server:call(?MODULE, get_subscribers_list).

%%
%% @doc Check for process to give socket control to
%% @param Socket
%% @returns Pid of found process or undefined
%%

cec_setup(_Socket) ->
    case whereis(?SERVER) of
        Pid when is_pid(Pid) ->
            Pid;
        undefined ->
            throw(no_certSecCredu)
    end.

%%
%% @doc Send socket to process that will take it over
%% @param Socket
%% @returns ok
%%

cec_takeover(Socket) ->
    gen_server:cast(?SERVER, {cec_takeover, Socket}).

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({finalize, Id}, _From, #state{subscribers = Subscribers} = State) ->
    {ok, Subscriber} = get_subscriber_by_id(Id, Subscribers),
    {ok, Socket} = get_selection_object(Subscriber),
    BinaryMessage = create_event_message(finalize),
    send_event(Socket, BinaryMessage),
    UpdatedSubscriber = close_selection_object(Subscriber),
    NewSubscribers = update_subscriber(UpdatedSubscriber, Subscribers), 
    {reply, ok, State#state{subscribers = NewSubscribers}};
handle_call(get_subscribers_list, _From, #state{subscribers = Subscribers} = State) ->
    {reply, Subscribers, State};
handle_call(Msg, _From, State) ->
    sysInitI:info_msg("~p: Handle call not implemented yet, message received: ~p.~n", [?MODULE, Msg]),
    {reply, ok, State}.

handle_cast({cec_takeover, Socket}, State) ->
    sysInitI:info_msg("~p: cec_takeover received, socket: [~p]~n", [?MODULE, Socket]),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_cast({cert_event, MoRef}, #state{subscribers = Subscribers} = State) ->
    sysInitI:info_msg("~p: cert_event received - MO changed: [~p]~n", [?MODULE, MoRef]),
    NcOrTcat = get_mo_type_from_mo_ref(MoRef),
    Clients = get_subscribed_clients(Subscribers, MoRef),
    update_nc_or_tcat(MoRef, Clients, NcOrTcat),
    {noreply, State};
handle_cast({cec_register}, State) ->
    ?LOG("Handle_cast: cec_register"),
    ok = cec:register(<<"SEC_CREDU_API">>, certSecCredu),
    {noreply, State};
handle_cast(Msg, State) ->
    sysInitI:warning_msg("~p: Handle_cast: unknown message ('~p')", [Msg]),
    {noreply, State}.

handle_info({tcp, Socket,
             <<_ClientPid:4/native-unsigned-integer-unit:8,
               ?SEC_CREDU_REQ_SIG:4/native-unsigned-integer-unit:8,
               ReqId:4/native-unsigned-integer-unit:8,
               RequestData/binary>>}, State) ->
    
    ?LOG("Handle_info got message: {socket(~p), {~p,~p,~p}", [Socket, ?SEC_CREDU_REQ_SIG, ReqId, RequestData]),
    {Response, NewState} = handle_sec_credu_request(ReqId, RequestData, Socket, State),
    BinaryMessage = handle_sec_credu_response(ReqId, Response),
    send_response(Socket, BinaryMessage),

    {noreply, NewState};

handle_info({tcp_closed, Socket}, #state{subscribers = Subscribers} = State) ->
    ?LOG("TCP closed received {~p}.~n", [Socket]),
    
    Result = get_subscriber_by_comm_socket(Socket, Subscribers),
    case Result of
        {ok, Subscriber} ->     % client shutdown
            sysInitI:info_msg("~p: Client ID{~p} socket is closed, force finalization.~n", [?MODULE, Subscriber#subscriber.id]),
            {ok, NewState} = finalize_subscriber(Subscriber, State),
            {noreply, NewState};
        {error, _Error} ->               % normal finalization
            ?LOG("Finished finalization.~n"),
            {noreply, State}
    end;

handle_info({tcp_error, _Socket, _}, State) ->
    sysInitI:error_msg("TCP error received.~n"),
    {noreply, State};

handle_info(Msg, State) ->
    sysInitI:warning_msg("~p: Handle info not implemented yet, message received: ~p.~n", [?MODULE, Msg]),
    {noreply, State}.

terminate(Reason, _State) ->
    ok = cec:unregister(<<"SEC_CREDU_API">>),
    delete_directory_structure(),
    sysInitI:info_msg("Terminated with reason ~p.~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Socket handling functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% @doc Send response and prepare for new message with active once
%% @param Socket and message to send
%% @returns ok | {error, Reason}
%%

send_response(Socket, Msg) ->
    ?LOG("Sending message via socket: {socket(~p), ~p}", [Socket, Msg]),
    gen_tcp:send(Socket, Msg),
    ok = inet:setopts(Socket, [{active, once}]).

send_event(Socket, Msg) ->
    ?LOG("Sending message via socket: {socket(~p), ~p}", [Socket, Msg]),
    gen_tcp:send(Socket, Msg),
    ok.

close_socket(Socket) ->
    ?LOG("Closing socket: ~p", [Socket]),
    gen_tcp:close(Socket).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MO subscription functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Selsct sec_credu version
%% @param sec_credu version received from user
%% @returns Negotiated sec_credu_version
%%
-spec select_sec_credu_version(SecCreduVersion :: string()) -> SelectedSecCreduVersion :: string().
select_sec_credu_version(_SecCreduVersion) ->
    ?SECCREDUVERSION.
%%
%% @doc Subscribe to MO reference
%% @param MO ref
%% @returns Subscribe result
%%
-spec subscribe_mo(MoRef :: mo_ref()) -> atom().
subscribe_mo(MoRef) ->
    ?LOG("Subscribe to [~p]~n", [MoRef]),
    certI:subscribe(MoRef, ?MODULE).

%%
%% @doc Unsubscribe from MO reference
%% @param MO ref
%% @returns Unsubscribe result
%%
-spec unsubscribe_mo(MoRef :: mo_ref()) -> atom(). 
unsubscribe_mo(MoRef) ->
    ?LOG("Unsubscribe from [~p]~n", [MoRef]),
    certI:unsubscribe(MoRef, ?MODULE).

%%
%% @doc Searches MO ref among all subscribers
%% @param Subscribers List of subscribers
%% @param MO ref
%% @param List of subscribers
%% @returns List of subscribers with subscription to MO ref
%%
%%
-spec get_subscribed_clients(Subscribers :: [#subscriber{}], MoRef :: mo_ref()) ->
          SubscribersToMoRef :: [#subscriber{}].
get_subscribed_clients(Subscribers, MoRef) -> 
    lists:filter(fun(Subscriber) ->  get_subscription_id(Subscriber, MoRef) =/= {error, sub_id_not_found} end, Subscribers).

%%
%% @doc Get node certificate with NodeCredential or Trusted certificates with TrustCategory
%% @param MO ref
%% @returns Node Certificate and Private Key or List of Trusted certificates for TrustCategory
%%
-spec get_cert(MoRef :: mo_ref()) -> {ok, NodeCert :: binary(), PrivKey :: binary()} | undefined.
get_cert(MORef) ->
    case certI:get_cert(MORef, pem) of
        {ok, NodeCert, PrivKey} ->
            {ok, NodeCert, PrivKey};
        _ ->
            ?LOG("Getting node certificate resulted in undefined"),
            undefined
    end.
%%
%% @doc Get Trusted Certificates and its MO references for related TrustCategory 
%% @param TrustCategoryRef
%% @returns TrustCertandMoRefsList
%%
-spec get_tcat_certs_and_MoRefs(TrustCategoryRef :: mo_ref()) -> 
          {ok, [{TrustCertMoRef :: mo_ref(), TrustedCertificatePem :: binary()}]} | undefined.
get_tcat_certs_and_MoRefs(MORef) ->
    case certI:get_tcat_certs_and_MoRefs(MORef) of
        {ok, TrustCertandMoRefsList} ->
            {ok, TrustCertandMoRefsList};
        _ ->
            ?LOG("Getting trusted certificate resulted in undefined"),
            undefined
    end.
%%
%% @doc Stores NC files
%% @param NodeCredential Id
%% @param Node Certificate
%% @param Private Key
%% @returns ok
%%
-spec store_nc_files(NodeCredentialId :: mo_id(), NodeCert :: binary(), PrivKey :: binary()) -> ok.
store_nc_files(NodeCredentialId, NodeCert, PrivKey) ->
    NCCertFilename = get_nc_cert_filename(NodeCredentialId),
    write_nc_cert_to_dir(NCCertFilename, NodeCert),
    NCKeyFilename = get_nc_key_filename(NodeCredentialId),
    write_nc_key_to_dir(NCKeyFilename, PrivKey).

%%
%% @doc Deletes NC files if they exist
%% @param NodeCredential Id
%% @returns ok
%%
-spec delete_nc_files(NodeCredentialId :: mo_id()) -> ok.
delete_nc_files(NodeCredentialId) ->
    NCCertFilename = get_nc_cert_filename(NodeCredentialId),
    case nc_cert_file_exists(NCCertFilename) of
        true ->
            ?LOG("Deleting node credential files: ~p", [ NodeCredentialId]),
            remove_nc_cert_from_dir(NCCertFilename),
            NCKeyFilename = get_nc_key_filename(NodeCredentialId),
            remove_nc_key_from_dir(NCKeyFilename);
        false ->
            ?LOG("Trying to delete non existent node credential files: ~p", [NodeCredentialId]),
            ok
    end.   

%%
%% @doc Deletes TrustCategory files if they exist
%% @param TrustCategoryId Id
%% @returns ok
%%
-spec delete_tcat_files(TrustCategoryId :: mo_id()) -> ok.
delete_tcat_files(TrustCategoryId) ->
    TcatDirname = get_tcat_dirname(TrustCategoryId),
    case tcat_dir_exists(TcatDirname) of
        true ->
            ?LOG("Deleting trust category files: {id(~p), tcat name(~p)", [TrustCategoryId, TcatDirname]),
            delete_tcat_dir(TcatDirname);
        false ->
            ?LOG("Trying to delete non existent trust category files: ~p", [TrustCategoryId]),
            ok
    end. 


%%
%% @doc Gets list of trusted certificates, TrustedCertificate Mo refs and its filenames
%% @param List of TrustedCertificate's content and Mo refs.
%% @returns List of trusted certificates, TrustedCertificate Mo refs and its filenames
%%
-spec get_tcert_info({TrustCategoryRef :: mo_ref(),TrustedCertificatePem :: binary()} ) ->
          {TrustCertIntId :: mo_id(), TcFilename :: string(), TrustedCertificatePem :: binary()}.
get_tcert_info(TrustCertandMoRef) ->
    {TrustCertMoRef, TrustedCertificatePem} = TrustCertandMoRef,
    TrustCertIntId = mo_ref_to_id(TrustCertMoRef),
    TcFilename = get_tc_cert_filename(TrustCertIntId),
    {TrustCertIntId, TcFilename, TrustedCertificatePem}.

%%
%% @doc Write TrustedCertificates into TrustCategory directory
%% @param TrustCategory directory name
%% @param TrustedCertificate MO reference
%% @param Trusted certificate in PEM format
%% @returns ok
%%
-spec write_tc_cert_list_to_dir(TcatDirname :: string(), TrustCertMoRef:: mo_ref(), TrustedCertificatePem :: binary()) -> 
          ok.
write_tc_cert_list_to_dir(TcatDirname, TrustCertMoRef, TrustedCertificatePem) ->
    TrustedCertificateId = mo_ref_to_id(TrustCertMoRef),
    Filename = get_tc_cert_filename(TrustedCertificateId),
    write_tc_cert_to_dir(TcatDirname, Filename, TrustedCertificatePem).

%%
%% @doc Get Node certificate or Private Key
%% @param Subscriber Id
%% @param Subscribction Id
%% @param Format (pem or filepath)
%% @param function (cert or key)
%% @returns Node certificate or Private Key.
%%
-spec handle_sec_credu_nodecredential_get(Id :: id(), SubscriptionId :: id(), Subscribers :: [#subscriber{}], Format :: sec_credu_format(), CertOrKey :: atom()) ->
          {ok, Result :: binary()} | {error, error_reason()}.
handle_sec_credu_nodecredential_get(Id, SubscriptionId, Subscribers, Format, CertOrKey) ->
    case get_subscriber_by_id(Id, Subscribers) of
        {ok, Subscriber} ->
            Subs = Subscriber#subscriber.subscriptions,
            case get_subscription_by_id(Subs, SubscriptionId) of
                {ok, Subscription} ->
                    NodeCredentialRef = Subscription#subscription.mo_ref,
                    handle_sec_credu_nodecredential_get_result(NodeCredentialRef, Format, CertOrKey);
                {error, not_found} ->
                    ?LOG("Message {handle_sec_credu_nodecredential_get, subscription id not found}"),
                    {error, sub_id_not_found}
            end;
        {error, id_not_found} ->
            ?LOG("Message {handle_sec_credu_nodecredential_get, id_not_found}"),
            {error, id_not_found}
    end.
%%
%% @doc Get Node certificate or Private Key
%% @param Node Credenial MO reference
%% @param Format (pem or filepath)
%% @param function (cert or key)
%% @returns Node certificate or Private Key.
%%
-spec handle_sec_credu_nodecredential_get_result(NodeCredentialRef :: mo_ref(), Format :: sec_credu_format(), CertOrKey :: atom()) ->
          {ok, Result :: binary()} | {error, error_reason()}.
handle_sec_credu_nodecredential_get_result(NodeCredentialRef, pem, cert) ->
    case get_cert(NodeCredentialRef) of
        {ok, NodeCert, _PrivKey} ->
            {ok, NodeCert};
        _ ->
            ?LOG("Message {handle_sec_credu_nodecredential_get_result, nc_not_installed}"),
            {error, nc_not_installed}
    end;
handle_sec_credu_nodecredential_get_result(NodeCredentialRef, filepath, cert) ->
    NodeCredentialId = mo_ref_to_id(NodeCredentialRef),
    NCCertFilename = get_nc_cert_filename(NodeCredentialId),
    case nc_cert_file_exists(NCCertFilename) of
        true ->
            FilePath = ?NC_CERT_DIR ++ "/" ++ NCCertFilename,
            {ok, list_to_binary(FilePath)};
        false ->
            ?LOG("Message {handle_sec_credu_nodecredential_get_result, nc_not_installed}"),
            {error, nc_not_installed}
    end;
handle_sec_credu_nodecredential_get_result(NodeCredentialRef, pem, key) ->
    case get_cert(NodeCredentialRef) of
        {ok, _NodeCert, PrivKey} ->
            {ok, PrivKey};
        _ ->
            ?LOG("Message {handle_sec_credu_nodecredential_get_result, nc_not_installed}"),
            {error, nc_not_installed}
    end;
handle_sec_credu_nodecredential_get_result(NodeCredentialRef, filepath, key) ->
    NodeCredentialId = mo_ref_to_id(NodeCredentialRef),
    NCKeyFilename = get_nc_key_filename(NodeCredentialId),
    case nc_key_file_exists(NCKeyFilename) of
        true ->
            FilePath = ?NC_KEY_DIR ++ "/" ++ NCKeyFilename,
            {ok, list_to_binary(FilePath)};
        false ->
            ?LOG("Message {handle_sec_credu_nodecredential_get_result, nc_not_installed}"),
            {error, nc_not_installed}
    end.

%% @doc Handles sec_credu_nodecredential_subscribe and sec_credu_trustcategory_subscribe requests
%% @param Id Client id
%% @param NodeCredential or TrustCategory MO ID
%% @param State certSecCredu state
%% @param atom nodeCredential or trustCategory
%% @returns Subscription id and updated state or error
%%
-spec handle_sec_credu_subscribe(Id :: id(), MoId :: mo_id(), State :: #state{}, NcOrTcat :: atom()) ->
          {{ok, SubscriptionId :: id()} | {error, error_reason()}, NewState :: #state{}}.
handle_sec_credu_subscribe(Id, MoId, #state{subscribers = Subscribers} = State, NcOrTcat) ->
    case get_subscriber_by_id(Id, Subscribers) of
        {ok, Subscriber} ->
            MoRef = mo_id_to_mo_ref(MoId, NcOrTcat),
            case get_subscribed_clients(Subscribers, MoRef) of
                [] -> 
                    case store_nc_or_tcat_files(MoRef, MoId, NcOrTcat) of
                        ok ->
                            subscribe_mo(MoRef),
                            {ok, SubscriptionId, NewSubscribers} = handle_sec_credu_subscribe_update_subscriber(Subscriber, MoRef, Subscribers),
                            {{ok, SubscriptionId}, State#state{subscribers = NewSubscribers}};
                        undefined ->
                            {{error, mo_ref_not_found}, State}
                    end;
                _ ->
                    {ok, SubscriptionId, NewSubscribers} = handle_sec_credu_subscribe_update_subscriber(Subscriber, MoRef, Subscribers),
                    {{ok, SubscriptionId}, State#state{subscribers = NewSubscribers}}
            end;
        {error, id_not_found} ->
            ?LOG("Message {handle_sec_credu_subscribe, id_not_found}"),
            {{error, id_not_found}, State}
    end.

%% @doc Updates subscriber for sec_credu_nodecredential_subscribe and sec_credu_trustcategory_subscribe requests
%% @param Subscriber to be updated
%% @param NodeCredential or TrustCategory MO reference
%% @param List of subscribers
%% @returns Subscription id and updated list of subscribers
%%
handle_sec_credu_subscribe_update_subscriber(Subscriber, MoRef, Subscribers) ->
    UpdatedSubscriber = add_subscription(Subscriber, MoRef),
    NewSubscribers = update_subscriber(UpdatedSubscriber, Subscribers),
    {ok, SubscriptionId} = get_subscription_id(UpdatedSubscriber, MoRef),
    {ok, SubscriptionId, NewSubscribers}.

%%
%% @doc Handles sec_credu_nodecredential_unsubscribe and sec_credu_trustcategory_unsubscribe requests
%% @param Id Client id
%% @param SubscriptionId Id as returned from subscribe request
%% @param State certSecCredu state
%% @param atom nodeCredential or trustCategory
%% @returns New state with subscription id removed or error
%%
-spec handle_sec_credu_unsubscribe(Id :: id(), SubscriptionId :: id(), State :: #state{}, NcOrTcat :: atom()) ->
          {ok | {error, error_reason()}, NewState :: #state{}}.
handle_sec_credu_unsubscribe(Id, SubscriptionId, #state{subscribers = Subscribers} = State, NcOrTcat) ->
    case get_subscriber_by_id(Id, Subscribers) of
        {ok, Subscriber} ->
            Subs = Subscriber#subscriber.subscriptions,
            case get_subscription_by_id(Subs, SubscriptionId) of 
                {ok, Subscription} ->
                    MoRef = Subscription#subscription.mo_ref,
                    UpdatedSubscriber = remove_subscription(Subscriber, SubscriptionId),
                    NewSubscribers = update_subscriber(UpdatedSubscriber, Subscribers),
                    case get_subscribed_clients(NewSubscribers, MoRef) of
                        [] ->     
                            unsubscribe_mo(MoRef),
                            MoId = mo_ref_to_id(MoRef),
                            delete_nc_or_tcat_files(MoId, NcOrTcat);
                        _ ->
                            ok
                    end,
                    {ok, State#state{subscribers = NewSubscribers}};
                {error, not_found} ->
                    ?LOG("Message {handle_sec_credu_unsubscribe, sub_id_not_found}"),
                    {{error, sub_id_not_found}, State}
            end;
        {error, id_not_found} ->
            ?LOG("Message {handle_sec_credu_unsubscribe, id_not_found}"),
            {{error, id_not_found}, State}
    end.

%%
%% @doc Store NC or TCat files if they exist
%% @param MO (NC or TCat) reference
%% @param MO (NC or TCat) Id
%% @returns ok
%%
store_nc_or_tcat_files(NodeCredentialRef, NodeCredentialId, nodeCredential) ->
    {nc, Key} = certLib:decode_moref(NodeCredentialRef),
    case mnesia:dirty_read({nodeCredential, Key}) of
        [] ->
            ?LOG("NC MoRef doesn't exist"),
            undefined;
        [_] ->
            ?LOG("NC MoRef exists"),
            case get_cert(NodeCredentialRef) of
                {ok, NodeCert, PrivKey} ->
                    store_nc_files(NodeCredentialId, NodeCert, PrivKey);
                _ ->
                    ?LOG("Trying to store node credential files to non existent node credential ~p", [NodeCredentialRef]),
                    ok
            end
    end;
store_nc_or_tcat_files(TrustCategoryRef, TrustCategoryId, trustCategory) ->
    case get_tcat_certs_and_MoRefs(TrustCategoryRef) of
        {ok, []} ->
            ?LOG("Trying to store trust category files to empty tcat_ref ~p", [TrustCategoryRef]),
            ok;
        {ok, TrustCertandMoRefsList} ->
            TcatDirname = get_tcat_dirname(TrustCategoryId),
            create_tcat_dir(TcatDirname),
            lists:foreach(fun({TrustCertMoRef, TrustedCertificatePem}) ->
                                  write_tc_cert_list_to_dir(TcatDirname, TrustCertMoRef, TrustedCertificatePem) end, TrustCertandMoRefsList);
        undefined ->
            ?LOG("Trying to store trust category files to non existent tcat_ref ~p", [TrustCategoryRef]),
            undefined
    end.

%%
%% @doc Deletes NC or TCat files if they exist
%% @param MO (NC or TCat) Id
%% @param atom nodeCredential or trustCategory
%% @returns ok
%%
-spec delete_nc_or_tcat_files(MoId :: mo_id(), NcOrTcat :: atom()) -> ok.
delete_nc_or_tcat_files(NodeCredentialId, nodeCredential) ->
    delete_nc_files(NodeCredentialId);
delete_nc_or_tcat_files(TrustCategoryId, trustCategory) ->
    delete_tcat_files(TrustCategoryId).

%%
%% @doc Gets Mo type from MoReference
%% @param MO reference
%% @returns atom nodeCredential or trustCategory
%%
-spec get_mo_type_from_mo_ref(MoRef :: mo_ref()) -> NcOrTcat :: atom().
get_mo_type_from_mo_ref(MoRef) ->
    case certLib:decode_moref(MoRef) of
        {nc, _Key} ->
            ?LOG("get_cert MoRef: ~p~n", [MoRef]),
            nodeCredential;
        {tcat, _Key} ->
            ?LOG("get_cert MoRef: ~p~n", [MoRef]),
            trustCategory
    end.

%%
%% @doc Uodate nc or tcat files
%% @param MO reference
%% @param atom nodeCredential or trustCategory
%% @param Subscribers List of subscribers subscribed to this MO reference
%% @returns ok
%%
-spec update_nc_or_tcat(MoRef :: mo_ref(), Clients :: [#subscriber{}], NcOrTcat :: atom()) -> ok.
update_nc_or_tcat(_MoRef, [], _NcOrTcat) ->
    ok;
update_nc_or_tcat(MoRef, Clients, NcOrTcat) ->
    MoId = mo_ref_to_id(MoRef),
    delete_nc_or_tcat_files(MoId, NcOrTcat),
    store_nc_or_tcat_files(MoRef, MoId, NcOrTcat),
    lists:foreach(fun(Client) -> 
                          send_nc_or_tcat_event_to_client(Client, MoRef, MoId, NcOrTcat) end, Clients),
    ok.

%%
%% @doc Send NC or TCat event message to subscriber
%% @param Subscriber
%% @param MO reference
%% @param MO Id
%% @param atom nodeCredential or trustCategory
%% @returns ok
%%
-spec send_nc_or_tcat_event_to_client(Client :: #subscriber{}, MoRef :: mo_ref(), MoId :: mo_id(), NcOrTcat :: atom()) -> ok.
send_nc_or_tcat_event_to_client(Client, MoRef, MoId, NcOrTcat) ->
    {ok, SubscriptionId} = get_subscription_id(Client, MoRef),
    case get_selection_object(Client) of 
        {ok, Socket} ->
            send_sec_credu_event_msg(NcOrTcat, SubscriptionId, MoId, Socket);
        {error, sel_obj_not_found} ->
            ?LOG("Message {send_nc_or_tcat_event_to_client, sel_obj_not_found}"),
            ok
    end.

%%
%% @doc Send sec_credu event message to subscriber
%% @param atom nodeCredential or trustCategory
%% @param Subscription Id
%% @param MO reference
%% @param MO Id
%% @returns ok
%%
-spec send_sec_credu_event_msg(NcOrTcat :: atom(), SubscriptionId :: id(), MoId :: mo_id(), Socket :: socket()) -> ok.
send_sec_credu_event_msg(NcOrTcat, SubscriptionId, MoId, Socket) ->
    BasePart = create_event_message(NcOrTcat),
    BaseSubId = serialize_integer(SubscriptionId, BasePart),
    BinaryMessage = serialize_string_result(MoId, BaseSubId),
    send_event(Socket, BinaryMessage).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DN manipulation functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% @doc mapping MO DN to MoId
%% @param MoRef MO DN
%% @returns Id from MoRef
%%
-spec mo_ref_to_id(MoRef :: mo_ref()) -> mo_id().
mo_ref_to_id(MoRef) ->
    [Id | _] = lists:reverse(string:tokens(binary_to_list(MoRef), ",=")),
    Id.
%%
%% @doc mapping MoId to MO DN
%% @param MoId
%% @param atom nodeCredential or trustCategory
%% @returns MoRef from MoId
%%
-spec mo_id_to_mo_ref(MoId :: mo_id(), NcOrTcat :: atom()) -> mo_ref().
mo_id_to_mo_ref(MoId, nodeCredential) ->
    node_credential_mo_ref(MoId);
mo_id_to_mo_ref(MoId, trustCategory) ->
    trust_category_mo_ref(MoId).

%%
%% @doc Mapping nodeCredentialId to DN (mo_ref)
%% @param NodeCredentialId 
%% @returns NodeCredential DN with given Id
%%
-spec node_credential_mo_ref(NodeCredentialId :: mo_id()) -> mo_ref().
node_credential_mo_ref(NodeCredentialId)  when is_list(NodeCredentialId) ->
    list_to_binary(?NODE_CREDENTIAL_DN(NodeCredentialId)).

%%
%% @doc Mapping trustCategoryId to DN (mo_ref)
%% @param TrustCategoryId
%% @returns TrustCategory DN with given Id
%%
-spec trust_category_mo_ref(TrustCategoryId :: mo_id()) -> mo_ref().
trust_category_mo_ref(TrustCategoryId) when is_list(TrustCategoryId) ->
    list_to_binary(?TRUST_CATEGORY_DN(TrustCategoryId)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Subscriber functions 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% @doc Generates unique id for subscriber
%%
-spec generate_client_id() -> id().
generate_client_id() ->
    erlang:unique_integer([positive]).

%%
%% @doc Adds new subscriber to list of subscribers
%% @param Id Unique client id as returned by generate_client_id/0
%% @param Subscribers List of subscribers
%% @returns List of subscribers with new subscriber containing Id
%%
-spec add_subscriber(Id :: id(), Socket :: socket(), Subscribers :: [#subscriber{}]) -> 
          [#subscriber{}].
add_subscriber(Id, Socket, Subscribers) ->
    ?LOG("Adding subscriber id(~p), socket(~p)", [Id, Socket]),
    [#subscriber{id = Id, communication_socket = Socket} | Subscribers].

%%
%% @doc Removes subscriber from list of subscribers
%% @param Id Unique client id as returned by generate_client_id/0
%% @param Subscribers List of subscribers
%% @returns List of subscribers without subscriber containing Id
%%
-spec remove_subscriber(Id :: id(), Subscribers :: [#subscriber{}]) -> 
          [#subscriber{}].
remove_subscriber(Id, Subscribers) ->
    {ok, Subscriber} = get_subscriber_by_id(Id, Subscribers),
    ?LOG("Removing subscriber id(~p)", [Id]),
    lists:delete(Subscriber, Subscribers).

%%
%% @doc Updates subscriber in list of subscribers
%% @param Subscriber record
%% @param List of subscribers
%% @returns Updated list of subscribers
%%
-spec update_subscriber(Subscriber :: #subscriber{}, SubscribersList :: [#subscriber{}]) -> 
          [#subscriber{}].
update_subscriber(Subscriber, SubscribersList) ->
    ?LOG("Updating subscriber id(~p)", [Subscriber#subscriber.id]),
    ListBefore = remove_subscriber(Subscriber#subscriber.id, SubscribersList),
    [Subscriber | ListBefore].

%%
%% @doc Removes subscriber "by hand"
%% @param Subscriber
%% @param State
%% @returns ok or {error, Reason}
%%
-spec finalize_subscriber(Subscriber :: #subscriber{}, State :: #state{}) ->
          {ok, NewState :: #state{}} | {error, Reason :: error_reason()}.
finalize_subscriber(Subscriber, State) ->
    {ok, NewState} = certSecCredu:unsubscribe_from_all(Subscriber#subscriber.id, State),
    handle_sec_credu_finalize(Subscriber#subscriber.id, NewState).

%%
%% @doc Removes all subscriptions "by hand"
%% @param Subscriber ID
%% @param State
%% @returns ok or {error, Reason}
%%
-spec unsubscribe_from_all(SubscriberId :: id(), State :: #state{}) ->
          {ok, State :: #state{}} | {error, Reason :: error_reason()}.
unsubscribe_from_all(SubscriberId, #state{subscribers = Subscribers} = State) ->
    {ok, #subscriber{subscriptions = Subscriptions }} = certSecCredu:get_subscriber_by_id(SubscriberId, Subscribers),
    case Subscriptions of
        [] ->
            {ok, State};
        [Subscription | _Rest] ->
            Type = get_mo_type_from_mo_ref(Subscription#subscription.mo_ref),
            {ok, NewState} = certSecCredu:handle_sec_credu_unsubscribe(SubscriberId, Subscription#subscription.subscribe_id, State, Type),
            unsubscribe_from_all(SubscriberId, NewState)
    end.

%%
%% @doc Fetches subscriber with Id from list of subscribers
%% @param Id Unique client id as returned by generate_client_id/0
%% @param Subscribers List of subscribers
%% @returns Subscriber with Id or error if subscriber doesn't exist
%%
-spec get_subscriber_by_id(Id :: id(), Subscribers :: [#subscriber{}]) -> 
          {ok, #subscriber{}} | {error, id_not_found}.
get_subscriber_by_id(Id, Subscribers) ->
    Result = lists:filter(fun(#subscriber{id = SubId}) ->
                                  SubId =:= Id
                          end, Subscribers),
    
    case Result of 
        [Subscriber] ->
            ?LOG("Getting subscriber by id (~p): ~p", [Id, Subscriber]),
            {ok, Subscriber};
        [] ->
            ?LOG("Message {get_subscriber_by_id, id_not_found}"),
            {error, id_not_found}
    end.

%%
%% @doc Fetches subscriber by communication socket from list of subscribers
%% @param Communication socket
%% @param Subscribers List of subscribers
%% @returns Subscriber or error if subscriber doesn't exist
%%
-spec get_subscriber_by_comm_socket(Socket :: socket(), Subscribers :: [#subscriber{}]) -> 
                            {ok, #subscriber{}} | {error, sub_id_not_found}.
get_subscriber_by_comm_socket(Socket, Subscribers) ->

    Result = lists:filter(fun(#subscriber{communication_socket = Sock}) ->
                              Sock =:= Socket
                          end, Subscribers),
    
    case Result of
        [Subscriber] ->
            ?LOG("Getting subscriber by communication socket: ~p.~n", [Socket]),
            {ok, Subscriber};
        [] ->
            ?LOG("Message {get_subscriber_by_id, sub_id_not_found}"),
            {error, sub_id_not_found}
    end.

%%
%% @doc Fetches selection object from subscriber
%% @param Subscriber 
%% @returns SelObj or error if it doesn't exist
%%
-spec get_selection_object(Subscriber :: #subscriber{}) -> 
          {ok, socket()} | {error, sel_obj_not_found}.
get_selection_object(#subscriber{select_object = undefined}) ->
    ?LOG("Message {get_selection_object, sel_obj_not_found}"),
    {error, sel_obj_not_found};
get_selection_object(Subscriber) ->
    ?LOG("Getting select object: ~p", [Subscriber#subscriber.select_object]),
    {ok, Subscriber#subscriber.select_object}.

%%
%% @doc Sets selection object on subscriber
%% @param Subscriber 
%% @param SelObj Socket for async messages
%% @returns Subscriber with new select_object
%%
-spec set_selection_object(Subscriber :: #subscriber{}, 
                           SelObj :: socket()) -> #subscriber{}.
set_selection_object(Subscriber, SelObj) ->
    ?LOG("Setting selectin object to subscriber: {~p, ~p}", [Subscriber, SelObj]),
    Subscriber#subscriber{select_object = SelObj}.

%%
%% @doc Closes selection object on subscriber
%% @param Subscriber 
%% @returns Subscriber without selection object
%%
-spec close_selection_object(Subscriber :: #subscriber{}) -> 
          #subscriber{}.
close_selection_object(Subscriber) ->
    ?LOG("Closing selection object on subscriber ~p", [Subscriber]),
    ok = close_socket(Subscriber#subscriber.select_object),
    Subscriber#subscriber{select_object = undefined}.

%%
%% @doc Generates unique id for subscription
%%
-spec generate_subscription_id() -> id().
generate_subscription_id() ->
    erlang:unique_integer([positive]).

%%
%% @doc Fetches subscription based on id or MO ref from list
%% @param Subscriptions
%% @param Sucscription Id or MO ref
%% returns Subscription ID or MO ref or error if missing
%%

get_subscription_by_id(Subscriptions, SubscriptionId) ->
    Result = lists:filter(fun(#subscription{subscribe_id = SubId}) ->
                                  SubId =:= SubscriptionId
                          end, Subscriptions),
    ?LOG("Subscription (~p) by id (~p)", [Result, SubscriptionId]),
    return_subscription(Result).

get_subscription_by_mo_ref(Subscriptions, SubscriptionMoRef) ->
    Result = lists:filter(fun(#subscription{mo_ref = SubMoRef}) ->
                                  SubMoRef =:= SubscriptionMoRef
                          end, Subscriptions),
    ?LOG("Subscription (~p) by mo_ref (~p)", [Result, SubscriptionMoRef]),
    return_subscription(Result).

return_subscription([Subscription]) ->
    {ok, Subscription};
return_subscription([]) ->
    ?LOG("Message {return_subscription, not_found}"),
    {error, not_found}.


%%
%% @doc Fetches subscription id based on MO ref from subscriber
%% @param Subscriber
%% @param Dn MO ref
%% returns Subscription ID or error if missing
%%
-spec get_subscription_id(Subscriber :: #subscriber{}, Dn :: mo_ref()) -> 
          {ok, id()} | {error, sub_id_not_found}.
get_subscription_id(#subscriber{subscriptions = Subs}, Dn) ->
    Result = get_subscription_by_mo_ref(Subs, Dn),
    case Result of 
        {ok, #subscription{} = Subscription} ->
            {ok, Subscription#subscription.subscribe_id};
        {error, not_found} ->
            ?LOG("Message {get_subscription_id, sub_id_not_found}"),
            {error, sub_id_not_found}
    end.

%%
%% @doc Adds subscription to MO ref to subscriber
%% @param Subscriber
%% @param Dn MO ref
%% returns Subscriber updated with subscription
%%
-spec add_subscription(Subscriber :: #subscriber{}, 
                       Dn :: mo_ref()) -> #subscriber{}.
add_subscription(Subscriber, Dn) ->
    add_subscription(Subscriber, Dn, generate_subscription_id()).
-spec add_subscription(Subscriber :: #subscriber{}, 
                       Dn :: mo_ref(),
                       SubscriptionId :: id()) -> #subscriber{}.
add_subscription(#subscriber{subscriptions = Subs} = Subscriber, Dn, SubscriptionId) ->
    ?LOG("Adding subscription id(~p) to subscriber id(~p)", [SubscriptionId, Subscriber]),
    Subscriber#subscriber{subscriptions = [#subscription{subscribe_id = SubscriptionId, mo_ref  = Dn} | Subs]}.

%%
%% @doc Removes subscription from subscriber
%%
-spec remove_subscription(Subscriber :: #subscriber{}, 
                          SubscriptionId :: id()) -> #subscriber{}.
remove_subscription(#subscriber{subscriptions = Subs} = Subscriber, SubscriptionId) ->
    {ok, Subscription} = get_subscription_by_id(Subs, SubscriptionId),
    ?LOG("Removing subscription id(~p) from subscriber id(~p)", [SubscriptionId, Subscriber]),
    Subscriber#subscriber{subscriptions = lists:delete(Subscription, Subs)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Directory functions 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% directory structure (example):
%% /tmp
%%   /sec_credu_api
%%          /certs
%%              nc1.pem
%%              nc2.pem
%%          /cacerts
%%              /tcat1
%%                  tc1.pem
%%                  tc3.pem
%%              /tcat2
%%                  tc1.pem
%%          /private
%%              nc1.key
%%              nc2.key


%%
%% @doc Creates directory structure as explained in example
%% returns ok if successful
%%
-spec create_directory_structure() -> ok.
create_directory_structure() ->
    ?LOG("Creating directory structure"),
    create_directory_structure(?DIRS).

create_directory_structure([HDir|Restdirs]) ->
    ok = make_dir(HDir),
    create_directory_structure(Restdirs);

create_directory_structure([]) ->
    ok.

%%
%% @doc Deletes directory structure
%% returns ok if successful
%%
-spec delete_directory_structure() -> ok.
delete_directory_structure() -> 
    %%Tcat directory may have subdirectories, so we need to clear those first
    ?LOG("Deleting directory structure"),
    {ok, Dirs} = file:list_dir_all(?TCAT_ROOT_DIR),
    lists:foreach(fun(Dir) -> delete_tcat_dir(Dir) end, Dirs),
    
    delete_directory_structure(lists:reverse(?DIRS)).

delete_directory_structure(Dirs) ->
    lists:foreach(fun(Dir) -> clear_dir(Dir), ok = del_dir(Dir) end, Dirs).

%%
%% @doc Returns Node Credential certificate filename from MO ID
%% @param NC MO ref
%%
-spec get_nc_cert_filename(NodeCredentialId :: mo_id()) -> string().
get_nc_cert_filename(NodeCredentialId) ->
    ?NC_CERT_FILENAME(NodeCredentialId).

%%
%% @doc Returns Node Credential key filename from MO ID
%% @param NC MO ref
%%
-spec get_nc_key_filename(NodeCredentialId :: mo_id()) -> string().
get_nc_key_filename(NodeCredentialId) ->
    ?NC_KEY_FILENAME(NodeCredentialId).

%%
%% @doc Checks if NC CERT file exists
%% @param NC CERT filename
%% returns true if it exists, false if not
%%
-spec nc_cert_file_exists(Filename :: string()) -> true | false.
nc_cert_file_exists(Filename) ->
    file_exists(?NC_CERT_DIR ++ "/" ++ Filename).

%%
%% @doc Checks if NC key file exists
%% @param NC key filename
%% returns true if it exists, false if not
%%
-spec nc_key_file_exists(Filename :: string()) -> true | false.
nc_key_file_exists(Filename) ->
    file_exists(?NC_KEY_DIR ++ "/" ++ Filename).

%%
%% @doc Writes new NC CERT in file
%% @param NC CERT filename
%% @param data which represents certificate
%% returns ok if successful
%%
-spec write_nc_cert_to_dir(Filename :: string(), PemCert :: binary()) -> ok.
write_nc_cert_to_dir(Filename, PemCert) ->
    ok = write_file(?NC_CERT_DIR ++ "/" ++ Filename, PemCert).

%%
%% @doc Writes new NC key in file
%% @param NC key filename
%% @param data which represents key
%% returns ok if successful
%%
-spec write_nc_key_to_dir(Filename :: string(), Key :: binary()) -> ok.
write_nc_key_to_dir(Filename, Key) ->
    ok = write_file(?NC_KEY_DIR ++ "/" ++ Filename, Key).

%%
%% @doc Removes NC CERT file
%% @param NC CERT filename
%% returns ok if successful
%%
-spec remove_nc_cert_from_dir(Filename :: string()) -> ok.
remove_nc_cert_from_dir(Filename) ->
    ok = delete_file(?NC_CERT_DIR ++ "/" ++ Filename).

%%
%% @doc Removes NC key file
%% @param NC key filename
%% returns ok if successful
%%
-spec remove_nc_key_from_dir(Filename :: string()) -> ok.
remove_nc_key_from_dir(Filename) ->
    ok = delete_file(?NC_KEY_DIR ++ "/" ++ Filename).

%%
%% @doc Returns Trust Category directory from MO ID
%% @param TCAT MO ref
%%
-spec get_tcat_dirname(TrustCategoryId :: mo_id()) -> string().
get_tcat_dirname(TrustCategoryId) ->
    ?TCAT_DIRNAME(TrustCategoryId).

%%
%% @doc Checks if Trust Category directory exists
%% @param TCAT dirname
%% returns true if it exists, false if not
%%
-spec tcat_dir_exists(Dirname :: string()) -> true | false.
tcat_dir_exists(Dirname) ->
    filelib:is_dir(?TCAT_ROOT_DIR ++ Dirname).

%%
%% @doc Creates new Trust Category directory
%% @param TCAT dirname
%% returns ok if successful
%%
-spec create_tcat_dir(Dirname :: string()) -> ok.
create_tcat_dir(Dirname) ->
    ok = make_dir(?TCAT_ROOT_DIR ++ Dirname).

%%
%% @doc Deletes Trust Category directory
%% @param TCAT dirname
%% returns ok if successful
%%
-spec delete_tcat_dir(Dirname :: string()) -> ok.
delete_tcat_dir(Dirname) ->
    HDir = ?TCAT_ROOT_DIR ++ Dirname,
    clear_dir(HDir),
    %%now that dir is empty, delete it
    ok = del_dir(HDir).

%%
%% @doc Returns Trusted Certificate filename from MO ID
%% @param TC CERT MO ref
%%
-spec get_tc_cert_filename(TrustedCertificateId :: mo_id()) -> string().
get_tc_cert_filename(TrustedCertificateId) ->
    ?TC_CERT_FILENAME(TrustedCertificateId).

%%
%% @doc Writes new Trusted Certificate file in Trust Category directory
%% @param TCAT dirname
%% @param TC CERT filename
%% @param data which represents certificate
%% returns ok if successfull
%%
-spec write_tc_cert_to_dir(TcatDirname :: string(), Filename :: string(), PemCert :: binary()) -> ok.
write_tc_cert_to_dir(TcatDirname, Filename, PemCert) ->
    ok = write_file(?TCAT_ROOT_DIR ++ TcatDirname ++ "/" ++ Filename, PemCert).

clear_dir(Dir) ->
    {ok, Filenames} = file:list_dir_all(Dir),
    lists:foreach(fun(Filename) ->  ok = delete_file(Dir ++ "/" ++ Filename) end, Filenames).

write_file(Path, Content) ->
    ?LOG("Storing content: {~p} to path {~p}", [Content, Path]),
    ok = file:write_file(Path, Content).

delete_file(Path) ->
    ?LOG("Deleting file in path: ~p", [Path]),
    ok = file:delete(Path).

make_dir(Dirname) ->
    ?LOG("Creating directory: ~p", [Dirname]),
    ok = file:make_dir(Dirname).

del_dir(Dirname) ->
    ?LOG("Deleting directory: ~p", [Dirname]),
    ok = file:del_dir(Dirname).

file_exists(Filename) ->
    filelib:is_regular(Filename).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_sec_credu functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec handle_sec_credu_request(ReqId :: id(), SecCreduRequestData :: binary(), Socket :: socket(), State :: #state{}) ->
          {Response :: term(), NewState :: #state{}}.
handle_sec_credu_request(?REQ_INITIALIZE,
                         <<ReleaseCode:1/native-unsigned-integer-unit:8,
                           MajorVersion:1/native-unsigned-integer-unit:8,
                           MinorVersion:1/native-unsigned-integer-unit:8>>, Socket, State) ->
    
    ?LOG("Handle_sec_credu_request initialize with {~p, ~p, ~p}", [ReleaseCode, MajorVersion, MinorVersion]),
    {ok, SecCreduVersion} = version_tuple_to_string({ReleaseCode, MajorVersion, MinorVersion}),
    certSecCredu:handle_sec_credu_initialize({SecCreduVersion, Socket}, State);   %response = {ok, ID, SelectedVersion}

handle_sec_credu_request(?REQ_FINALIZE,
                         <<ID:4/native-unsigned-integer-unit:8>>, _Socket, State) ->
    
    ?LOG("Handle_sec_credu_request finalize with {~p}", [ID]),
    certSecCredu:handle_sec_credu_finalize(ID, State);    %response = ok or {error, reason}

handle_sec_credu_request(?REQ_SELECTION_OBJECT_GET,
                         <<ID:4/native-unsigned-integer-unit:8>>, Socket, State) ->
    
    ?LOG("Handle_sec_credu_request sel_obj_get with {~p}", [ID]),
    certSecCredu:handle_sec_credu_selectionobject_get({ID, Socket}, State);    %response = ok or {error, reason}

handle_sec_credu_request(?REQ_NC_SUBSCRIBE,
                         <<ID:4/native-unsigned-integer-unit:8,
                           _NCIDLength:4/native-unsigned-integer-unit:8,
                           NCID/binary>>, _Socket, State) ->
    
    ?LOG("Handle_sec_credu_request nc_subscribe with {~p, ~p}", [ID, NCID]),
    certSecCredu:handle_sec_credu_nodecredential_subscribe({ID, binary_to_list(NCID)}, State);    %response = {ok, SubId} or {error, reason}

handle_sec_credu_request(?REQ_NC_UNSUBSCRIBE,
                         <<ID:4/native-unsigned-integer-unit:8,
                           SubId:4/native-unsigned-integer-unit:8>>, _Socket, State) ->
    
    ?LOG("Handle_sec_credu_request nc_unsubscribe with {~p, ~p}", [ID, SubId]),
    certSecCredu:handle_sec_credu_nodecredential_unsubscribe({ID, SubId}, State);    %response = ok or {error, reason}

handle_sec_credu_request(?REQ_NC_CERT_GET,
                         <<ID:4/native-unsigned-integer-unit:8,
                           SubId:4/native-unsigned-integer-unit:8,
                           SecCreduFormat:4/native-unsigned-integer-unit:8>>, _Socket, State) ->
    
    ?LOG("Handle_sec_credu_request nc_cert_get with {~p, ~p, ~p}", [ID, SubId, SecCreduFormat]),
    certSecCredu:handle_sec_credu_nodecredential_get_cert({ID, SubId, integer_to_format(SecCreduFormat)}, State);     %response = {ok, Result/binary} or {error, reason}

handle_sec_credu_request(?REQ_NC_KEY_GET,
                         <<ID:4/native-unsigned-integer-unit:8,
                           SubId:4/native-unsigned-integer-unit:8,
                           SecCreduFormat:4/native-unsigned-integer-unit:8>>, _Socket, State) ->
    
    ?LOG("Handle_sec_credu_request nc_key_get with {~p, ~p, ~p}", [ID, SubId, SecCreduFormat]),
    certSecCredu:handle_sec_credu_nodecredential_get_key({ID, SubId, integer_to_format(SecCreduFormat)}, State);    %response = {ok, Result :: binary()} or {error, error_reason()}

handle_sec_credu_request(?REQ_TCAT_SUBSCRIBE,
                         <<ID:4/native-unsigned-integer-unit:8,
                           _TcatLength:4/native-unsigned-integer-unit:8,
                           TcatId/binary>>, _Socket, State) ->
    
    ?LOG("Handle_sec_credu_request tcat_subscribe with {~p, ~p}", [ID, TcatId]),
    certSecCredu:handle_sec_credu_trustcategory_subscribe({ID, binary_to_list(TcatId)}, State);    %response = {ok, SubscriptionId :: id()} or {error, error_reason()}

handle_sec_credu_request(?REQ_TCAT_UNSUBSCRIBE,
                         <<ID:4/native-unsigned-integer-unit:8,
                           SubId:4/native-unsigned-integer-unit:8>>, _Socket, State) ->
    
    ?LOG("Handle_sec_credu_request tcat_unsubscribe with  {~p, ~p}", [ID, SubId]),
    certSecCredu:handle_sec_credu_trustcategory_unsubscribe({ID, SubId}, State);    %response = ok or {error, error_reason()}

handle_sec_credu_request(?REQ_TCAT_GET,
                         <<ID:4/native-unsigned-integer-unit:8,
                           SubId:4/native-unsigned-integer-unit:8>>, _Socket, State) ->
    
    ?LOG("Handle_sec_credu_request tcat_get with {~p, ~p}", [ID, SubId]),
    certSecCredu:handle_sec_credu_trustcategory_get({ID, SubId}, State);    %response = {ok, Count, TcatDirName, TcatContents :: [{TcId, TcFilename, PemCert }] } or {error, error_reason()}

handle_sec_credu_request(_UnknownId, _SomeData, _Socket, State) ->
    ?LOG("Handle_sec_credu_request unknown request"),
    {{error, unknown_request}, State}.

%%
%% @doc Handles sec_credu_initialize request
%% @param SecCreduVersion Version requested by client 
%% @param State certSecCredu state
%% @returns Unique client id, selected version and state containing new subscriber with Id
%%
-spec handle_sec_credu_initialize({SecCreduVersion :: string(), Socket :: socket()}, State :: #state{}) ->
          {{ok, Id :: id(), SelectedVersion :: string()}, NewState :: #state{}}.
handle_sec_credu_initialize({SecCreduVersion, Socket}, State) ->
    Id = generate_subscription_id(),
    sysInitI:info_msg("~p: Initializing: {id(~p), version(~p), socket(~p)}", [?MODULE, Id, SecCreduVersion, Socket]),
    NewSubscribers = add_subscriber(Id, Socket, State#state.subscribers),
    SelectedVersion = select_sec_credu_version(SecCreduVersion),
    {{ok, Id, SelectedVersion}, State#state{subscribers = NewSubscribers}}.

%%
%% @doc Handles sec_credu_finalize request
%% @param Id Client id
%% @param State certSecCredu state
%% @returns State without subscriber with Id
%%
-spec handle_sec_credu_finalize(Id :: id(), State :: #state{}) ->
          {ok | {error, error_reason()}, NewState :: #state{}}.
handle_sec_credu_finalize(Id, #state{subscribers = Subscribers} = State) ->
    case get_subscriber_by_id(Id, Subscribers) of
        {ok, Subscriber} ->
            case get_selection_object(Subscriber) of 
                {ok, Socket} ->
                    BinaryMessage = create_event_message(finalize),
                    send_event(Socket, BinaryMessage),
                    close_selection_object(Subscriber);
                {error, sel_obj_not_found} ->
                    ok
            end,
            NewSubscribers = remove_subscriber(Id, Subscribers),
            {ok, State#state{subscribers = NewSubscribers}};
        {error, id_not_found} ->
            ?LOG("Message {handle_sec_credu_finalize, id_not_found}"),
            {{error, id_not_found}, State}
    end.

%%
%% @doc Handles sec_credu_selectionobject_get request
%% @param Id Client id
%% @param SelObj Socket used for async communication
%% @param State certSecCredu state
%% @returns New state with selection object stored, or error 
%%
-spec handle_sec_credu_selectionobject_get({Id :: id(), SelObj :: socket()}, State :: #state{}) ->
          {ok | {error, error_reason()}, NewState :: #state{}}.
handle_sec_credu_selectionobject_get({Id, SelObj}, #state{subscribers = Subscribers} = State) ->
    case get_subscriber_by_id(Id, Subscribers) of
        {ok, Subscriber} ->
            case  get_selection_object(Subscriber) of 
                {ok, _Socket} ->
                    close_selection_object(Subscriber);
                {error, sel_obj_not_found} ->
                    ok
            end,
            UpdatedSubscriber = set_selection_object(Subscriber, SelObj),
            NewSubscribers = update_subscriber(UpdatedSubscriber, Subscribers), 
            {ok, State#state{subscribers = NewSubscribers}};
        {error, id_not_found} ->
            ?LOG("Message {handle_sec_credu_selectionobject_get, id_not_found}"),
            {{error, id_not_found}, State}
    end.

%%
%% @doc Handles sec_credu_nodecredential_subscribe request
%% @param Id Client id
%% @param NodeCredentialId MO ID
%% @param State certSecCredu state
%% @returns Subscription id and updated state or error
%%
-spec handle_sec_credu_nodecredential_subscribe({Id :: id(), NodeCredentialId :: mo_id()}, State :: #state{}) ->
          {{ok, SubscriptionId :: id()} | {error, error_reason()}, NewState :: #state{}}.
handle_sec_credu_nodecredential_subscribe({Id, NodeCredentialId}, #state{subscribers = Subscribers} = State) ->   
    handle_sec_credu_subscribe(Id, NodeCredentialId, #state{subscribers = Subscribers} = State, nodeCredential).

%%
%% @doc Handles sec_credu_nodecredential_unsubscribe request
%% @param Id Client id
%% @param SubscriptionId Id as returned from subscribe request
%% @param State certSecCredu state
%% @returns New state with subscription id removed or error
%%
-spec handle_sec_credu_nodecredential_unsubscribe({Id :: id(), SubscriptionId :: id()}, State :: #state{}) ->
          {ok | {error, error_reason()}, NewState :: #state{}}.
handle_sec_credu_nodecredential_unsubscribe({Id, SubscriptionId}, #state{subscribers = Subscribers} = State)  ->
    handle_sec_credu_unsubscribe(Id, SubscriptionId, #state{subscribers = Subscribers} = State, nodeCredential).

%%
%% @doc Handles sec_credu_trustcategory_subscribe request
%% @param Id Client id
%% @param TrustCategoryId MO reference for subscription
%% @param State certSecCredu state
%% @returns Subscription id and updated state or error
%%
-spec handle_sec_credu_trustcategory_subscribe({Id :: id(), TrustCategoryId :: mo_id()}, State :: #state{}) ->
          {{ok, SubscriptionId :: id()} | {error, error_reason()}, NewState :: #state{}}.
handle_sec_credu_trustcategory_subscribe({Id, TrustCategoryId}, #state{subscribers = Subscribers} = State) ->
    handle_sec_credu_subscribe(Id, TrustCategoryId, #state{subscribers = Subscribers} = State, trustCategory).

%%
%% @doc Handles sec_credu_trustcategory_unsubscribe request
%% @param Id Client id
%% @param SubscriptionId Id as returned from subscribe request
%% @param State certSecCredu state
%% @returns New state with subscription id removed or error
%%
-spec handle_sec_credu_trustcategory_unsubscribe({Id :: id(), SubscriptionId :: id()}, State :: #state{}) ->
          {ok | {error, error_reason()}, NewState :: #state{}}.
handle_sec_credu_trustcategory_unsubscribe({Id, SubscriptionId}, #state{subscribers = Subscribers} = State) ->
    handle_sec_credu_unsubscribe(Id, SubscriptionId, #state{subscribers = Subscribers} = State, trustCategory).

%%
%% @doc Handles sec_credu_nodecredential_get_cert request
%% @param Id Client id
%% @param SubscriptionId Id as returned from subscribe request
%% @param Format PEM or filename
%% @param State certSecCredu state
%% @returns Result based on format or error
%%
-spec handle_sec_credu_nodecredential_get_cert({Id :: id(), SubscriptionId :: id(), Format :: sec_credu_format()}, State :: #state{}) ->
          {{ok, Result :: binary()} | {ok, Result :: string()} | {error, error_reason()}, NewState :: #state{}}.

handle_sec_credu_nodecredential_get_cert({Id, SubscriptionId, Format}, #state{subscribers = Subscribers} = State) ->
    Result = handle_sec_credu_nodecredential_get(Id, SubscriptionId, Subscribers, Format, cert),
    {Result, State}.

%%
%% @doc Handles sec_credu_nodecredential_get_key request
%% @param Id Client id
%% @param SubscriptionId Id as returned from subscribe request
%% @param Format PEM or filename
%% @param State certSecCredu state
%% @returns Result based on format or error
%%
-spec handle_sec_credu_nodecredential_get_key({Id :: id(), SubscriptionId :: id(), Format :: sec_credu_format()}, State :: #state{}) ->
          {{ok, Result :: binary()} | {ok, Result :: string()} | {error, error_reason()}, NewState :: #state{}}.
handle_sec_credu_nodecredential_get_key({Id, SubscriptionId, Format}, #state{subscribers = Subscribers} = State) ->
    Result = handle_sec_credu_nodecredential_get(Id, SubscriptionId, Subscribers, Format, key),
    {Result, State}.   

%%
%% @doc Handles sec_credu_trustcategory_get request
%% @param Id Client id
%% @param SubscriptionId Id as returned from subscribe request
%% @param State certSecCredu state
%% @returns Result containing number of TCs, dirname, and information for each TC or error
%%
-spec handle_sec_credu_trustcategory_get({Id :: id(), SubscriptionId :: id()}, State :: #state{}) ->
          {{ok, Count :: non_neg_integer(), TcatDirName :: string(), 
            TcatContents :: [{TcId :: mo_id(), TcFilename :: string(), TrustedCertificatePem :: binary()}]} | 
               {error, error_reason()}, NewState :: #state{}}.
handle_sec_credu_trustcategory_get({Id, SubscriptionId}, #state{subscribers = Subscribers} = State) ->
    case get_subscriber_by_id(Id, Subscribers) of
        {ok, Subscriber} ->
            Subs = Subscriber#subscriber.subscriptions,
            case get_subscription_by_id(Subs, SubscriptionId) of
                {ok, Subscription} ->
                    TrustCategoryRef = Subscription#subscription.mo_ref,
                    case get_tcat_certs_and_MoRefs(TrustCategoryRef) of
                        {ok, []} ->
                            ?LOG("Message {handle_sec_credu_trustcategory_get, tcat_empty}"),
                            {{error, tcat_empty}, State};
                        {ok, TrustCertandMoRefsList} ->
                            TrustCategoryId = mo_ref_to_id(TrustCategoryRef), 
                            TcatDirName = get_tcat_dirname(TrustCategoryId),
                            Count = length(TrustCertandMoRefsList),
                            TcatContents = lists:map(fun(TrustCertandMoRef) -> get_tcert_info(TrustCertandMoRef) end, TrustCertandMoRefsList),
                            {{ok, Count, ?TCAT_ROOT_DIR ++ TcatDirName, TcatContents}, State};
                        undefined ->
                            ?LOG("Message {handle_sec_credu_trustcategory_get, tcat_empty}"),
                            {{error, tcat_empty}, State}
                    end;
                {error, not_found} ->
                    ?LOG("Message {handle_sec_credu_trustcategory_get, sub_id_not_found}"),
                    {{error, sub_id_not_found}, State}
            end;
        {error, id_not_found} ->
            ?LOG("Message {handle_sec_credu_trustcategory_get, id_not_found}"),
            {{error, id_not_found}, State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data serialization functions 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Integer : 4 bytes
%% Binary/string : 4 bytes (length field) + length

%%
%% @doc Creates initial message which is then to be constructed
%% @param Status Status of response (ok or error)
%% @returns binary containing Status enumeration as only parameter
%%
-spec create_response_message(Status :: sec_credu_resp_status()) -> binary().
create_response_message(Status) ->
    Response = response_status_to_integer(Status),    
    <<?SEC_CREDU_RESP_SIG:4/native-unsigned-integer-unit:8, Response:4/native-unsigned-integer-unit:8>>.

%%
%% @doc Creates initial message which is then to be constructed
%% @param atom nodeCredential, trustCategory or finalize
%% @returns binary containing Event enumeration as only parameter
%%
-spec create_event_message(NcOrTcatOrFinal :: atom()) -> binary().
create_event_message(NcOrTcatOrFinal) ->
    Event = event_to_integer(NcOrTcatOrFinal),
    <<?SEC_CREDU_EVENT_SIG:4/native-unsigned-integer-unit:8, Event:4/native-unsigned-integer-unit:8>>.

%% @doc Adds integer parameter to binary message
%% @param Integer 
%% @param Message see create_response_message/1
%% @returns binary message with appended integer
%%
-spec serialize_integer(Integer :: integer(), Message :: binary()) -> binary().
serialize_integer(Integer, Message) ->
    serialize_integer32(Integer, Message).

%%
%% @doc Adds integer32 parameter to binary message
%% @param Integer 
%% @param Message see create_response_message/1
%% @returns binary message with appended integer
%%
-spec serialize_integer32(Integer :: integer(), Message :: binary()) -> binary().
serialize_integer32(Integer, Message) ->
    <<Message/binary, Integer:4/native-unsigned-integer-unit:8>>.

%%
%% @doc Adds string length and string itself to binary message
%% @param Result 
%% @param Message see create_response_message/1
%% @returns binary message with appended length and string
%%
-spec serialize_string_result(Result :: string(), Message :: binary()) -> binary().
serialize_string_result(Result, Message) ->
    ResultLen = string:len(Result),
    ResultBin = list_to_binary(Result),
    <<Message/binary, ResultLen:4/native-unsigned-integer-unit:8, ResultBin/binary>>.

%%
%% @doc Adds binary length and binary itself to binary message
%% @param Result 
%% @param Message see create_response_message/1
%% @returns binary message with appended length and string
%%
-spec serialize_binary_result(Result :: binary(), Message :: binary()) -> binary().
serialize_binary_result(Result, Message) ->
    Size = erlang:byte_size(Result),
    <<Message/binary, Size:4/native-unsigned-integer-unit:8, Result/binary>>.

%%
%% @doc Function for maping response status
%% @param atom
%% @returns integer
%%
response_status_to_integer(ok) ->
    ?RESP_STAT_OK;
response_status_to_integer(id_not_found) ->
    ?RESP_STAT_ERR_ID_NOT_FOUND;
response_status_to_integer(sub_id_not_found) ->
    ?RESP_STAT_ERR_SUB_ID_NOT_FOUND;
response_status_to_integer(mo_ref_not_found) ->
    ?RESP_STAT_ERR_MO_REF_NOT_FOUND;
response_status_to_integer(nc_not_installed) ->
    ?RESP_STAT_ERR_NC_NOT_INSTALLED;
response_status_to_integer(tcat_empty) ->
    ?RESP_STAT_ERR_TCAT_EMPTY;
response_status_to_integer(unknown_request) ->
    ?RESP_STAT_ERR_UNKNOWN_REQ_TYPE.

%%
%% @doc Function for mapping events
%% @param atom
%% @returns integer
%%
-spec event_to_integer(Atom :: term()) -> integer().
event_to_integer(nodeCredential) ->
    ?NC_EVENT;
event_to_integer(trustCategory) ->
    ?TCAT_EVENT;
event_to_integer(finalize) ->
    ?FINALIZE_EVENT.
%%
%% @doc Function for maping secCreduFormat from integer
%% @param integer
%% @returns atom
%%
-spec integer_to_format(Number :: integer()) -> term().
integer_to_format(1) ->
    filepath;
integer_to_format(2) ->
    pem.


%%
%% @doc Forms a tuple of elements from string
%% @param String
%% @returns {ok, tuple of numbers}
%%
-spec version_string_to_tuple(Version :: string()) -> tuple().
version_string_to_tuple(SelectedVersion) -> 
    [RC, B, C] = SelectedVersion,    % [] = "A11", RC = 65 ako je A, B = 49, [49] = "1" -> 1
    {ok, {RC, list_to_integer([B]), list_to_integer([C])}}.
%%
%% @doc Forms a string from elements in tuple
%% @param Tuple
%% @returns {ok, string}
%%
-spec version_tuple_to_string(Version :: tuple()) -> tuple().
version_tuple_to_string(SelectedVersion) ->
    {RC, B, C} = SelectedVersion,   % npr 65, 1, 1, [65] = "A"
    {ok, [RC] ++ integer_to_list(B) ++ integer_to_list(C)}.


%%
%% @doc Creates a response message in binary
%% @param RequestId
%% @param handle_sec_credu_request Data
%% @returns binary message
%%
-spec handle_sec_credu_response(ReqId :: id(), Response :: term()) -> binary().
handle_sec_credu_response(?REQ_INITIALIZE, {ok, ID, SelectedVersion}) ->
    BasePart = create_response_message(ok), %<<resp sign, ok>>
    Base = serialize_integer(ID, BasePart),  %<<resp sign, ok, id>>
    {ok, {RC, MajVer, MinVer}} = version_string_to_tuple(SelectedVersion),
    <<Base/binary,
      RC:1/native-unsigned-integer-unit:8,
      MajVer:1/native-unsigned-integer-unit:8,
      MinVer:1/native-unsigned-integer-unit:8>>;

handle_sec_credu_response(ReqId, ok) when ReqId =:= ?REQ_FINALIZE orelse
                                              ReqId =:= ?REQ_SELECTION_OBJECT_GET orelse
                                              ReqId =:= ?REQ_NC_UNSUBSCRIBE orelse
                                              ReqId =:= ?REQ_TCAT_UNSUBSCRIBE ->
    create_response_message(ok); %<<resp sign, ok>>

handle_sec_credu_response(ReqId, {error, Error}) when ReqId =:= ?REQ_FINALIZE orelse
                                                          ReqId =:= ?REQ_SELECTION_OBJECT_GET orelse
                                                          ReqId =:= ?REQ_NC_UNSUBSCRIBE orelse
                                                          ReqId =:= ?REQ_TCAT_UNSUBSCRIBE orelse
                                                          ReqId =:= ?REQ_NC_SUBSCRIBE orelse
                                                          ReqId =:= ?REQ_TCAT_SUBSCRIBE orelse 
                                                          ReqId =:= ?REQ_NC_CERT_GET orelse
                                                          ReqId =:= ?REQ_NC_KEY_GET orelse
                                                          ReqId =:= ?REQ_TCAT_GET ->
    create_response_message(Error); %<<resp sign, error>>

handle_sec_credu_response(ReqId, {ok, SubId}) when ReqId =:= ?REQ_NC_SUBSCRIBE orelse
                                                       ReqId =:= ?REQ_TCAT_SUBSCRIBE ->
    serialize_integer(SubId, create_response_message(ok));  %<<resp sign, ok, subid>>

handle_sec_credu_response(ReqId, {ok, Result}) when ReqId =:= ?REQ_NC_CERT_GET orelse
                                                        ReqId =:= ?REQ_NC_KEY_GET ->
    serialize_binary_result(Result, create_response_message(ok)); %<<resp sign, ok, size, message>>

handle_sec_credu_response(?REQ_TCAT_GET, {ok, Count, TcatDirName, TcatContents}) ->       % TcatContents :: [{TcId, TcFilename, PemCert }] list of n tuples
    BasePart = serialize_integer(Count, create_response_message(ok)), %<<resp sign, ok, count>>
    Base = serialize_string_result(TcatDirName, BasePart), %<<resp sign, ok, count, namelen, namebin>>
    TcatBinary = tcat_list_to_binary(TcatContents, <<>>),
    <<Base/binary, TcatBinary/binary>>;    % <<resp sign, ok, count, namelen, namebin, list of: datalen, tcidlen, tcid, tcfilenamelen, tcfilenamebin, pemcertlen, pemcert>>

handle_sec_credu_response(_ReqId, {error, unknown_request}) ->
    create_response_message(unknown_request).

%%
%% @doc Creates a binary 
%% @param RequestId
%% @param handle_sec_credu_request Data
%% @returns binary message
%%
-spec tcat_list_to_binary(TcatContents :: list(), Message :: binary()) -> binary().
tcat_list_to_binary([], NewBinary) ->
    NewBinary;
tcat_list_to_binary([H|Rest], NewBinary) ->     % TcatContents :: [{TcId :: mo_ref(), TcFilename :: string(), PemCert :: binary()}] list of n tuples, mo_ref() :: binary().
    {TcId, TcFileName, PemCert} = H,
    TcIdBinary = serialize_binary_result(list_to_binary(TcId), <<>>),  % <<tcidlen, tcid>>
    TcFileNameBinary = serialize_string_result(TcFileName, TcIdBinary), % <<tcidlen, tcid, tcfilenamelen, tcfilenamebin>>
    PemCertBinary = serialize_binary_result(PemCert, TcFileNameBinary), % <<tcidlen, tcid, tcfilenamelen, tcfilenamebin, pemcertlen, pemcert>>
    TcDataLen = erlang:byte_size(PemCertBinary),
    tcat_list_to_binary(Rest, <<NewBinary/binary, TcDataLen:4/native-unsigned-integer-unit:8, PemCertBinary/binary>>).



