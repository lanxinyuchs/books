%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certCMPv2.erl %
%%% @author etxasta
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/1
%%% 
%%% @doc ==Certficate revocation list handling==
%%% This module implements the certificate revocation list handling
-module(certCMPv2).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/1').
-date('2017-12-06').
-author('etxasta').

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
%%% Rev        Date        Name     What
%%% -----      ----------  -------  --------------------------
%%% R2A/1      2013-11-14  etxasta  Created
%%% R6A/7      2016-07-27  etomist  HV13246
%%% R6A/8      2016-08-29  etxasta  Changed extraCert in
%%%                                 certConf msg to empty list.
%%% R7A/3      2016-10-14  etxasta  Added SERIALNUMBER for
%%%                                 the authorityName
%%% R7A/4      2016-10-20  etxasta  Fixed old bug in build_rdnSequence
%%%                                 and get_cn
%%% R8A/1      2016-10-28  etxasta  Fixed more for OTP
%%% R8A/2      2016-11-17  etxasta  Error handling
%%% R8A/3      2016-11-18  etxasta  Fixed more for OTP
%%% R8A/4      2016-11-23  etxasta  mk_otp changes to mk_otp/3
%%% R9A/1      2017-02-09  etomist  HV61597
%%% R9A/2      2017-02-16  etomist  HV62945, EC fix
%%% R9A/3      2017-02-23  etomist  HV66400, EC fix, part 2
%%% R9A/4      2017-03-20  ebabmat  HV71517, RDN sequence decode improvement, 
%%%                                 added support for escape sequences
%%% R10A/1     2017-04-13  etomist  HV80460
%%% R10A/2     2017-04-24  emarnek  HV82599
%%% R10A/3     2017-05-11  etxasta  Added R-VNFM TCAT support
%%% R10A/4     2017-05-12  etxasta  Some more R-VNFM TCAT fix
%%% R10A/5     2017-05-12  etxasta  Changed sender for shared secret
%%% R10A/6     2017-05-16  etxasta  Little detail for R-VNFM
%%% R10A/7     2017-05-23  etxasta  Another little detail for R-VNFM
%%% R10A/8     2017-05-24  etomist  HV90254
%%% R10A/9     2017-05-25  ebabmat  HV90860
%%% R10A/10    2017-06-08  etxasta  Handling of error from get_cred
%%% R10A/11    2017-06-09  etomist  HV94563
%%% R10A/12    2017-06-29  etomist  HV98868
%%% R11A/1-2   2017-10-11  etxasta  Updates for SP680
%%% R11A/3     2017-10-16  emajkaa  HW36249
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/1, initialization_request/9,
        key_update_request/11, cancel/1]).

-export([decode_msg/1, get_cn/1]).
-export([build_rdnSequence/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     code_change/3, terminate/2]).

-include_lib("public_key.hrl").
-include("cert.hrl").
-include("PKIXCMP.hrl").
-include("PKIXCRMF-2005.hrl").
-include("RcsCertM.hrl").

-define(RETRY_TIMEOUT,   120000). % Wait 120s for CMPv2 message retry
-define(REQUEST_TIMEOUT,  60000). % Wait 60s for CMPv2 server reply
-define(REQUEST_CONNECT_TIMEOUT,  60000). % Wait 60s to connect to CMPv2 server

-define(FAIL_INFO_LIST, [{badAlg, 0},
                        {badMessageCheck, 1},
                        {badRequest, 2},
                        {badTime, 3},
                        {badCertId, 4},
                        {badDataFormat, 5},
                        {wrongAuthority, 6},
                        {incorrectData, 7},
                        {missingTimeStamp, 8},
                        {badPOP, 9},
                        {certRevoked, 10},
                        {certConfirmed, 11},
                        {wrongIntegrity, 12},
                        {badRecipientNonce, 13},
                        {timeNotAvailable, 14},
                        {unacceptedPolicy, 15},
                        {unacceptedExtension, 16},
                        {addInfoNotAvailable, 17},
                        {badSenderNonce, 18},
                        {badCertTemplate, 19},
                        {signerNotTrusted, 20},
                        {transactionIdInUse, 21},
                        {unsupportedVersion, 22},
                        {notAuthorized, 23},
                        {systemUnavail, 24},
                        {systemFailure, 25},
                        {duplicateCertReq, 26}]).

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

start(Index) ->
    Name = make_process_name(Index),
    gen_server:start_link({local, Name}, ?MODULE, Index, []).


initialization_request(AuthorityName, SubjectName, Csr, CsrKey, Index,
    URI, EnrollCaCert, EnrollCaFingerpr, If) ->
    start(Index),
    Name = make_process_name(Index),
    gen_server:cast(Name, {initialization_request,
            AuthorityName, SubjectName, Csr, CsrKey, URI, EnrollCaCert,
            EnrollCaFingerpr, If}).

key_update_request(AuthorityName, SubjectName, Csr, CsrKey,
    Index, NcCert, NcKey, URI, EnrollCaCert, EnrollCaFingerpr, If) ->
    start(Index),
    Name = make_process_name(Index),
    gen_server:cast(Name, {key_update_request,
            AuthorityName, SubjectName, Csr, CsrKey, NcCert, NcKey,
            URI, EnrollCaCert, EnrollCaFingerpr, If}).

%% Cancel an ongoing enrollment
cancel(Key) ->
    Name = make_process_name(Key),
    gen_server:cast(Name, cancel).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
init(Index) ->
    put(index, Index),
    {ok, up}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({initialization_request, AuthorityName, SubjectName, Csr, CsrKey,
        URI, EnrollCaCert, EnrollCaFingerpr, SrcIf}, State) ->
    put(enrollment_try_later_timer_ref, undefined),
    put(src_if, SrcIf),
    case encode_ir_msg(AuthorityName, SubjectName, Csr, CsrKey, URI,
            EnrollCaCert, EnrollCaFingerpr) of
        {ok, PKIMessage} ->
            send(new, PKIMessage);
        {error, Reason} ->
            certNcServer:update_status(get(index), cmpv2_enrollment_failed,
                Reason)
    end,
    {noreply, State};
handle_cast({key_update_request, AuthorityName, SubjectName, Csr, CsrKey,
        NcCert, NcKey, URI, EnrollCaCert, EnrollCaFingerpri, SrcIf}, State) ->
    put(enrollment_try_later_timer_ref, undefined),
    put(src_if, SrcIf),
    PKIMessage = encode_kur_msg(AuthorityName, SubjectName, Csr, CsrKey,
        NcCert, NcKey, URI, EnrollCaCert, EnrollCaFingerpri),
    send(new, PKIMessage),
    {noreply, State};
handle_cast(cancel, State) ->
    handle_cancel(),
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({http, {_Ref, {error, {failed_connect,_}}}}, State) ->
    case get(msg_state) of
        undefined ->
            ok;
        _ ->
            certNcServer:update_status(get(index), restart,
                {get(uri), "No response from CMPv2 server"})
    end,
    handle_cancel(),
    {noreply, State};
handle_info({http, ReplyInfo}, State) ->
    case decode_reply_info(ReplyInfo) of
        {error, Reason} ->
            info_msg("Error reply info, try again: ~p~n", [Reason]);
        Binary ->
            case catch decode_msg(Binary) of
                {_, Reason} ->
                    info_msg("CMPv2 failed to decode received msg: ~p",
                        [Reason]),
                    certLib:sec_log("", "CMPv2 failed to decode received msg");
                _ ->
                    ok
            end
    end,
    {noreply, State};
handle_info({re_send, {MsgType, {ok, DataDer}}}, State) ->
    send(re_send, {MsgType, {ok, DataDer}}),
    {noreply, State};
handle_info({new, {MsgType, {ok, DataDer}}}, State) ->
    send(new, {MsgType, {ok, DataDer}}),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
make_process_name({_,_,_,_,Index}) ->
    list_to_atom("certCMPv2." ++ Index);
make_process_name(Index) ->
    list_to_atom("certCMPv2." ++ Index).


%%% ----------------------------------------------------------
%%% #           encode_ir_msg(AuthorityName, SubjectName, Csr,
%%%                           CsrKey, URI, EnrollCaCert,
%%%                           EnrollCaFingerpri)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Encode the Initialization Request message. 
%%% ----------------------------------------------------------
encode_ir_msg(AuthorityName, SubjectName, Csr, CsrKey, URI,
    EnrollCaCert, EnrollCaFingerpri) ->
    do_encode_ir_msg_1(get_cred(), AuthorityName, SubjectName, Csr,
        CsrKey, URI, EnrollCaCert, EnrollCaFingerpri).


do_encode_ir_msg_1({cert, {VcKey, [VcCert|_] = Vc}}, AuthorityName,
    SubjectName, Csr, CsrKey, URI, EnrollCaCert, EnrollCaFingerpri) ->
    %% Using Vendor Credential
    ProtectionAlg = mk_protection_alg(cert, VcCert),
    VcList        = decode_cert(Vc),
    Sender        = build_vc_sender(hd(VcList)),
    do_encode_ir_msg_2(VcKey, ProtectionAlg, Sender, VcList, AuthorityName,
        SubjectName, Csr, CsrKey, URI, EnrollCaCert, EnrollCaFingerpri);
do_encode_ir_msg_1({shared, {Salt, IterationCount, OTP}}, AuthorityName,
    SubjectName, Csr, CsrKey, URI, EnrollCaCert, EnrollCaFingerpri) ->
    %% Using Shared Secret
    ProtectionAlg = mk_protection_alg(shared, {Salt, IterationCount}),
    Sender        = {directoryName,  build_rdnSequence(SubjectName)},
    %%Sender        = {directoryName, {rdnSequence, []}},
    put(key, OTP),
    do_encode_ir_msg_2(shared, ProtectionAlg, Sender, [], AuthorityName,
        SubjectName, Csr, CsrKey, URI, EnrollCaCert, EnrollCaFingerpri);
do_encode_ir_msg_1({error, Reason}, _AuthorityName, _SubjectName, _Csr,
    _CsrKey, _URI, _EnrollCaCert, _EnrollCaFingerpri) ->
    info_msg("CMPv2 failed getting creds, reason: ~p", [Reason]),
    {error, Reason}.


do_encode_ir_msg_2(Cred, ProtectionAlg, Sender, VcList, AuthorityName,
    SubjectName, Csr, CsrKey, URI, EnrollCaCert, EnrollCaFingerpri) ->
    List          = build_rdnSequence(AuthorityName),
    Recipient     = {directoryName, List},
    SenderKID     = get_cn(SubjectName),
    TransactionID = crypto:strong_rand_bytes(16),

    %% PKI Header
    PKIHeader =
    #'PKIHeader'{
        pvno          = cmp2000,
        sender        = Sender,
        recipient     = Recipient,
        messageTime   = utc_string(),
        protectionAlg = ProtectionAlg,
        senderKID     = SenderKID,
        transactionID = TransactionID,
        senderNonce   = crypto:strong_rand_bytes(16)},

    CertReqInfo =
    Csr#'CertificationRequest'.certificationRequestInfo,
    SubPubKey =
    CertReqInfo#'CertificationRequestInfo'.subjectPKInfo,
    CriPKIAlg =
    SubPubKey#'CertificationRequestInfo_subjectPKInfo'.algorithm,
    Alg =
    CriPKIAlg#'CertificationRequestInfo_subjectPKInfo_algorithm'.algorithm,
    Para =
    CriPKIAlg#'CertificationRequestInfo_subjectPKInfo_algorithm'.parameters,
    SubjectPublicKey =
    SubPubKey#'CertificationRequestInfo_subjectPKInfo'.subjectPublicKey,

    CertReqId = mk_cert_req_id(),

    %% HV61597 explicitly encode asn1_NOVALUE as NULL
    Para2 =
    case Para of
        asn1_NOVALUE -> <<5,0>>;
        {asn1_OPENTYPE, P} -> P
    end,

    SubjectPublicKeyInfo = 
    #'SubjectPublicKeyInfo'{
        algorithm =
        #'AlgorithmIdentifier'{
            algorithm  = Alg,
            parameters = Para2},
        subjectPublicKey = SubjectPublicKey},

    Ext =
    case CertReqInfo#'CertificationRequestInfo'.attributes of
        [{'AttributePKCS-10', ?'pkcs-9-at-extensionRequest', Values}] ->
            lists:filtermap(
                fun({asn1_OPENTYPE, Attr}) ->
                        {ok, [Data]} = 'OTP-PUB-KEY':decode('ExtensionRequest',
                            io_lib:format("~s",[Attr])),
                        Critical     = Data#'Extension'.critical,
                        Value        = Data#'Extension'.extnValue,
                        {true, #'Extension'{
                                extnID    = ?'id-ce-subjectAltName',
                                critical  = Critical,
                                extnValue = Value}};
                (_) ->
                        false
                end, Values);
        _ ->
            asn1_NOVALUE
    end,
    
    CertReq =
    #'CertRequest'{
        certReqId = CertReqId,
        certTemplate =
        #'CertTemplate'{
            subject    = CertReqInfo#'CertificationRequestInfo'.subject,
            issuer     = List,
            publicKey  = SubjectPublicKeyInfo,
            extensions = Ext}},

    SignatureAlgorithm =
    Csr#'CertificationRequest'.signatureAlgorithm,
    SignAlg =
    SignatureAlgorithm#'CertificationRequest_signatureAlgorithm'.algorithm,
    
    PopoSign = mk_popo_sign(CertReq, CsrKey), 
    
    Popo = {signature,
        #'POPOSigningKey'{
            algorithmIdentifier =
            #'AlgorithmIdentifier'{
                algorithm  = SignAlg,
                parameters = Para2},
            signature = PopoSign}},
    
    CertReqMsg =
    [#'CertReqMsg'{certReq = CertReq,
                   popo    = Popo}],

    %% Uses VC key or OTP for signing
    Protection =
    mk_protection(PKIHeader, {ir, CertReqMsg}, Cred),

    %% PKI Message
    PKIMessage =
    #'PKIMessage'{header     = PKIHeader,
                  body       = {ir, CertReqMsg},
                  protection = Protection,
                  extraCerts = mk_extra_cert(VcList)},


    put(transaction_id,  TransactionID),
    put(cert_req_id,     CertReqId),
    put(protection_alg,  ProtectionAlg),
    put(sender,          Sender),
    put(recipient,       Recipient),
    put(sender_kid,      SenderKID),
    put(uri,             URI),
    put(ca_cert,         EnrollCaCert),
    put(ca_finger_print, EnrollCaFingerpri),

    %% Set message state
    put(msg_state, ir),

    %% Encode the PKI Message
    {ok, {ir, 'PKIXCMP':encode('PKIMessage', PKIMessage)}}.

%%% ----------------------------------------------------------
%%% #           encode_kur_msg(AuthorityName, SubjectName, Csr,
%%%                            CsrKey, NcCert, NcKey, URI,
%%%                            EnrollCaCert, EnrollCaFingerpri)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Encode the Key Update Request message.
%%% ----------------------------------------------------------
encode_kur_msg(AuthorityName, SubjectName, Csr, CsrKey, [NcCert|_] = Nc,
    NcKey, URI, EnrollCaCert, EnrollCaFingerpri) ->
    %% PKI Header
    ProtectionAlg = mk_protection_alg(cert, NcCert),
    NcList        = decode_cert(Nc),
    List1         = build_rdnSequence(SubjectName),
    Sender        = {directoryName, List1},
    List2         = build_rdnSequence(AuthorityName),
    Recipient     = {directoryName, List2},
    SenderKID     = get_cn(SubjectName),
    TransactionID = crypto:strong_rand_bytes(16),

    PKIHeader =
    #'PKIHeader'{
        pvno          = cmp2000,
        sender        = Sender,
        recipient     = Recipient,
        messageTime   = utc_string(),
        protectionAlg = ProtectionAlg,
        senderKID     = SenderKID,
        transactionID = TransactionID,
        senderNonce   = crypto:strong_rand_bytes(16)},

    CertReqInfo =
    Csr#'CertificationRequest'.certificationRequestInfo,
    SubPubKey =
    CertReqInfo#'CertificationRequestInfo'.subjectPKInfo,
    CriPKIAlg =
    SubPubKey#'CertificationRequestInfo_subjectPKInfo'.algorithm,
    Alg =
    CriPKIAlg#'CertificationRequestInfo_subjectPKInfo_algorithm'.algorithm,
    Para =
    CriPKIAlg#'CertificationRequestInfo_subjectPKInfo_algorithm'.parameters,
    SubjectPublicKey =
    SubPubKey#'CertificationRequestInfo_subjectPKInfo'.subjectPublicKey,

    CertReqId = mk_cert_req_id(),

    %% HV61597 explicitly encode asn1_NOVALUE as NULL
    Para2 =
    case Para of
        asn1_NOVALUE -> <<5,0>>;
        {asn1_OPENTYPE, P} -> P %%HV66400
    end,
    
    SubjectPublicKeyInfo = 
    #'SubjectPublicKeyInfo'{
        algorithm =
        #'AlgorithmIdentifier'{
            algorithm  = Alg,
            parameters = Para2},
        subjectPublicKey = SubjectPublicKey},

    Ext =
    case CertReqInfo#'CertificationRequestInfo'.attributes of
        [{'AttributePKCS-10', ?'pkcs-9-at-extensionRequest', Values}] ->
            lists:filtermap(
                fun({asn1_OPENTYPE, Attr}) ->
                        {ok, [Data]} = 'OTP-PUB-KEY':decode('ExtensionRequest',
                            io_lib:format("~s",[Attr])),
                        Critical     = Data#'Extension'.critical,
                        Value        = Data#'Extension'.extnValue,
                        {true, #'Extension'{
                                extnID    = ?'id-ce-subjectAltName',
                                critical  = Critical,
                                extnValue = Value}};
                (_) ->
                        false
                end, Values);
        _ ->
            asn1_NOVALUE
    end,
 
    CertReq =
    #'CertRequest'{
        certReqId = CertReqId,
        certTemplate =
        #'CertTemplate'{
            subject    = CertReqInfo#'CertificationRequestInfo'.subject,
            issuer     = List2,
            publicKey  = SubjectPublicKeyInfo,
            extensions = Ext}},

    SignatureAlgorithm =
    Csr#'CertificationRequest'.signatureAlgorithm,
    SignAlg =
    SignatureAlgorithm#'CertificationRequest_signatureAlgorithm'.algorithm,

    PopoSign = mk_popo_sign(CertReq, CsrKey), 

    Popo = {signature,
        #'POPOSigningKey'{
            algorithmIdentifier =
            #'AlgorithmIdentifier'{
                algorithm  = SignAlg,
                parameters = Para2},
            signature = PopoSign}},
    
    CertReqMsg =
    [#'CertReqMsg'{certReq = CertReq,
                   popo    = Popo}],

    %{ok, Cert} ='PKIX1Explicit88':decode('Certificate', NcCert),

    %% Uses signature by still valid node credential
    Protection =
    mk_protection(PKIHeader, {kur, CertReqMsg}, NcKey),

    %% PKI Message
    PKIMessage =
    #'PKIMessage'{header     = PKIHeader,
                  body       = {kur, CertReqMsg},
                  protection = Protection,
                  %extraCerts = mk_extra_cert([Cert])},
                  extraCerts = mk_extra_cert(NcList)},

    put(transaction_id,  TransactionID),
    put(cert_req_id,     CertReqId),
    put(protection_alg,  ProtectionAlg),
    put(sender,          Sender),
    put(recipient,       Recipient),
    put(sender_kid,      SenderKID),
    put(uri,             URI),
    put(ca_cert,         EnrollCaCert),
    put(ca_finger_print, EnrollCaFingerpri),

    %% Set message state
    put(msg_state, kur),
    
    %% Encode the PKI Message
    {kur, 'PKIXCMP':encode('PKIMessage', PKIMessage)}.


%%% ----------------------------------------------------------
%%% #      encode_certconf_msg(MsgType,RecipNonce,Cert,CertReqId)
%%% Input: 
%%% Output:
%%% Exceptions: 
%%% Description: Encode the Confirmation message. 
%%% ----------------------------------------------------------
encode_certconf_msg(ip, RecipNonce, Cert, CertReqId) ->
    case get(key) of
        undefined -> %% Use Vendor Credential
            {cert, {{Format, VcKeyDer}, _VcDerList}} = get_cred(),
            do_encode_certconf_msg({Format, VcKeyDer}, RecipNonce,
                Cert, CertReqId);
        _ -> %% Use shared secret
            %% No need to call get_cred(), already make at the ir message
            do_encode_certconf_msg(shared, RecipNonce, Cert, CertReqId)
    end;
encode_certconf_msg(kup, RecipNonce, Cert, CertReqId) ->
    {ok, NcKeyBin} = certSecStore:get_nc_key(get(index), "nc.key"),
    do_encode_certconf_msg(binary_to_term(NcKeyBin), RecipNonce,
        Cert, CertReqId).

do_encode_certconf_msg(Cred, RecipNonce, Cert, CertReqId) ->
    %% PKI Header
    PKIHeader =
    #'PKIHeader'{
        pvno          = cmp2000,
        sender        = get(sender), 
        recipient     = get(recipient),
        messageTime   = utc_string(),  % ex. "20131120142016Z"
        protectionAlg = get(protection_alg),
        senderKID     = get(sender_kid),
        transactionID = get(transaction_id),
        senderNonce   = crypto:strong_rand_bytes(16),
        recipNonce    = RecipNonce}, % senderNonce from ip or kup msg

    DerPKIBody = public_key:der_encode('Certificate', Cert),
    SignAlg = Cert#'Certificate'.signatureAlgorithm#'AlgorithmIdentifier'.algorithm,
    CertHash   = crypto:hash(format_protection_alg(SignAlg), DerPKIBody),

    CertStatus =
    [#'CertStatus'{
            certHash   = CertHash,
            certReqId  = CertReqId,
            statusInfo = asn1_NOVALUE}],
    
    %% Cred can be VC, NC key or OTP
    Protection = mk_protection(PKIHeader, {certConf, CertStatus}, Cred),

    %% PKI Message
    PKIMessage =
    #'PKIMessage'{header     = PKIHeader,
                  body       = {certConf, CertStatus},
                  protection = Protection,
                  extraCerts = asn1_NOVALUE}, %% HV90254

    %% Set message state
    put(msg_state, certConf),

    %% Encode the PKI Message
    {certconf, 'PKIXCMP':encode('PKIMessage', PKIMessage)}.


%%% ----------------------------------------------------------
%%% #       encode_poll_req_msg(MsgType, RecipNonce, CertReqId)
%%% Input: 
%%% Output:
%%% Exceptions: 
%%% Description: Encode the polling request message. 
%%% ----------------------------------------------------------
encode_poll_req_msg(ir, RecipNonce, CertReqId) ->
    %% Set message state
    put(msg_state, ir),
    case get(key) of
        undefined -> %% Use Vendor Credential
            {cert, {{Format, VcKeyDer}, _VcDerList}} = get_cred(),
            do_encode_poll_req_msg({Format, VcKeyDer}, RecipNonce, CertReqId);
        _ -> %% Use shared secret
            %% No need to call get_cred(), already make at the ir message
            do_encode_poll_req_msg(shared, RecipNonce, CertReqId)
    end;
encode_poll_req_msg(kur, RecipNonce, CertReqId) ->
    %% Set message state
    put(msg_state, kur),
    {ok, NcKeyBin} = certSecStore:get_nc_key(get(index), "nc.key"),
    do_encode_poll_req_msg(binary_to_term(NcKeyBin), RecipNonce, CertReqId).


do_encode_poll_req_msg(Cred, RecipNonce, CertReqId) ->
    %% PKI Header
    PKIHeader =
    #'PKIHeader'{
        pvno          = cmp2000,
        sender        = get(sender),
        recipient     = get(recipient),
        messageTime   = utc_string(),  % ex. "20131120142016Z"
        protectionAlg = get(protection_alg),
        senderKID     = get(sender_kid),
        transactionID = get(transaction_id),
        senderNonce   = crypto:strong_rand_bytes(16),
        recipNonce    = RecipNonce}, % senderNonce from ip or kup msg

    CertStatus = [#'PollReqContent_SEQOF'{certReqId  = CertReqId}],
    
    %% Cred can be VC, NC private key or OTP
    Protection = mk_protection(PKIHeader, {pollReq, CertStatus}, Cred),

    %% PKI Message
    PKIMessage =
    #'PKIMessage'{header     = PKIHeader,
                  body       = {pollReq, CertStatus},
                  protection = Protection,
                  extraCerts = []},

       %% Encode the PKI Message
    {pollReq, 'PKIXCMP':encode('PKIMessage', PKIMessage)}.


%%% ----------------------------------------------------------
%%% #           encode_error_msg(RecipNonce, StatusString, FailInfo)
%%% Input: 
%%% Output:
%%% Exceptions: 
%%% Description: Encode the error message. 
%%%
%%% atom(PKIFailureInfo):
%%%    badAlg(0),
%%%    badMessageCheck(1),
%%%    badRequest(2),
%%%    badTime(3),
%%%    badCertId(4),
%%%    badDataFormat(5),
%%%    wrongAuthority(6),
%%%    incorrectData(7),
%%%    missingTimeStamp(8),
%%%    badPOP(9),
%%%    certRevoked(10),
%%%    certConfirmed(11),
%%%    wrongIntegrity(12),
%%%    badRecipientNonce(13),
%%%    timeNotAvailable(14),
%%%    unacceptedPolicy(15),
%%%    unacceptedExtension(16),
%%%    addInfoNotAvailable(17),
%%%    badSenderNonce(18),
%%%    badCertTemplate(19),
%%%    signerNotTrusted(20),
%%%    transactionIdInUse(21),
%%%    unsupportedVersion(22),
%%%    notAuthorized(23),
%%%    systemUnavail(24),
%%%    systemFailure(25),
%%%    duplicateCertReq(26)
%%% ----------------------------------------------------------
encode_error_msg(RecipNonce, StatusString, FailInfo) ->
    %% PKI Header
    PKIHeader =
    #'PKIHeader'{
        pvno          = cmp2000,
        sender        = get(sender), % Same as in ir/ip or kur/kup msg
        recipient     = get(recipient),
        messageTime   = utc_string(),  % ex. "20131120142016Z"
        protectionAlg = asn1_NOVALUE, 
        senderKID     = get(sender_kid),
        transactionID = get(transaction_id),
        senderNonce   = crypto:strong_rand_bytes(16),
        recipNonce    = RecipNonce}, % senderNonce from ip or kup msg

    PkiStatusInfo =
    #'PKIStatusInfo'{
        status       = rejection,
        statusString = [list_to_binary(StatusString)],
        failInfo     = create_fail_info_bitstring(FailInfo)},

    ErrorMsgContent =
    #'ErrorMsgContent'{
        pKIStatusInfo = PkiStatusInfo,
        errorCode     = asn1_NOVALUE,
        errorDetails  = asn1_NOVALUE},
    
    %% PKI Message
    PKIMessage =
    #'PKIMessage'{header     = PKIHeader,
                  body       = {error, ErrorMsgContent},
                  protection = asn1_NOVALUE,
                  extraCerts = asn1_NOVALUE},

    %% Set message state
    put(msg_state, error),

    %% Encode the PKI Message
    {error, 'PKIXCMP':encode('PKIMessage', PKIMessage)}.

create_fail_info_bitstring(FailInfo) ->
    FailInfoIndex = proplists:get_value(FailInfo, ?FAIL_INFO_LIST),
    <<1:(FailInfoIndex + 1), 0:(26 - FailInfoIndex)>>.

%%% ----------------------------------------------------------
%%% #           decode_msg(Binary)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Decode the CMPv2 message. 
%%% ----------------------------------------------------------
decode_msg(Binary) ->
    decode_msg1('PKIXCMP':decode('PKIMessage', Binary)).

decode_msg1({error, Reason}) ->
    Msg = "Failed to decode the PKI Message",
    info_msg(Msg ++ ", reason: ~p~n", [Reason]),
    certNcServer:update_status(get(index), cmpv2_enrollment_failed, Msg),
    {error, format_unknown};
decode_msg1({ok, Message}) ->
    Header  = Message#'PKIMessage'.header,
    Body    = Message#'PKIMessage'.body,
    Protection = Message#'PKIMessage'.protection,
    ExtraCerts =  Message#'PKIMessage'.extraCerts,    
    #'PKIHeader'{transactionID = TransactionID} = Header,
    case get(transaction_id) of
        TransactionID ->
            %% Cancel any earlier retries still hanging
            case get(enrollment_try_later_timer_ref) of
                canceled ->
                    ok;
                _ ->
                    decode_msg2(Header,Body,Protection,ExtraCerts,
                        get(msg_state))
            end;
        _ ->
            Msg = "No valid transactionId, ignore this CMPv2 ip msg",
            certLib:sec_log("", Msg),
            info_msg(Msg, []),
            certNcServer:update_status(get(index),
                cmpv2_enrollment_failed, Msg),
            ok
    end.


%% Decode the Initialization Response message. 
decode_msg2(Header, {ip, CertRepMessage}, Protection, ExtraCerts, ir) ->
     #'PKIHeader'{
        pvno          = Pvno,      
        protectionAlg = ProtectionAlg,
        sender        = Sender,
        senderNonce   = SenderNonce
    } = Header,
    [Response|_] = CertRepMessage#'CertRepMessage'.response,
    CaPubs = CertRepMessage#'CertRepMessage'.caPubs,
    #'CertResponse'{certReqId        = NewCertReqId,
                    status           = Status,
                    certifiedKeyPair = CertifiedKeyPair,
                    rspInfo          = RspInfo} = Response,
    Index = get(index),
    certNcServer:update_status(Index, cmpv2_received_msg, ip),
    CertList = extraCertField(ExtraCerts, CaPubs, Sender),
    case verify_pki_msg(Header, {ip, CertRepMessage}, Protection,
            CertList, Pvno, ProtectionAlg) of
        valid ->
            decode_msg3(ip, SenderNonce, get(cert_req_id), NewCertReqId,
                Status, CertifiedKeyPair, RspInfo, CertList);
        {error, Reason} ->
            certNcServer:update_status(Index, cmpv2_enrollment_failed,
                Reason),
            PKIMessage = encode_error_msg(SenderNonce, Reason, incorrectData),
            send(error, PKIMessage)
    end;
%% Decode the Key Update Response message. 
decode_msg2(Header, {kup, CertRepMessage}, Protection, ExtraCerts, kur) ->
     #'PKIHeader'{
        pvno          = Pvno,         
        protectionAlg = ProtectionAlg,
        sender        = Sender,
        senderNonce   = SenderNonce
    } = Header,
    [Response|_] = CertRepMessage#'CertRepMessage'.response,
    CaPubs = CertRepMessage#'CertRepMessage'.caPubs,
    #'CertResponse'{certReqId        = NewCertReqId,
                    status           = Status,
                    certifiedKeyPair = CertifiedKeyPair,
                    rspInfo          = RspInfo} = Response,
    Index = get(index),
    certNcServer:update_status(Index, cmpv2_received_msg, kup),
    CertList = extraCertField(ExtraCerts, CaPubs, Sender),
    case verify_pki_msg(Header, {kup, CertRepMessage}, Protection,
            CertList, Pvno, ProtectionAlg) of
        valid ->
            decode_msg3(kup, SenderNonce, get(cert_req_id), NewCertReqId,
                Status, CertifiedKeyPair, RspInfo, CertList);
        {error, Reason} ->
            certNcServer:update_status(Index, cmpv2_enrollment_failed,
                Reason),
            PKIMessage = encode_error_msg(SenderNonce, Reason, incorrectData),
            send(error, PKIMessage)
    end;
%% Received a after an error msg was sent
decode_msg2(_Header, _Response, _Protection, _ExtraCerts, error) ->
    case get(enrollment_try_later_timer_ref) of %% cancel timer
        canceled ->
            ok;
        TRef1 ->
            case TRef1 of
                undefined ->
                    ok;
                _ ->
                    erlang:cancel_timer(TRef1),
                    put(enrollment_try_later_timer_ref, undefined)
            end
    end,
    certNcServer:update_status(get(index), restart, %% retry
                                {get(uri), "ip message rejected, retrying!"});
%% Decode the Polling Response message. 
decode_msg2(Header, {pollRep, RepMsgList = [RepMessage|_]}, Protection, ExtraCerts, _) ->
     #'PKIHeader'{
        pvno          = Pvno,      
        protectionAlg = ProtectionAlg,
        sender        = Sender,
        senderNonce   = SenderNonce
    } = Header,
    #'PollRepContent_SEQOF'{certReqId  = NewCertReqId,
                            checkAfter = CheckAfter,
                            reason     = PollReason} = RepMessage,
    Index = get(index),
    certNcServer:update_status(Index, cmpv2_received_msg, pollRep),
    CertList = extraCertField(ExtraCerts, asn1_NOVALUE, Sender),
    case verify_pki_msg(Header, {pollRep, RepMsgList}, Protection,
            CertList, Pvno, ProtectionAlg) of
        valid ->
            decode_msg3(pollRep, SenderNonce, get(cert_req_id), NewCertReqId,
                PollReason, CheckAfter, undefined, CertList);
        {error, Reason} ->
            certNcServer:update_status(Index, cmpv2_enrollment_failed,
                Reason),
            PKIMessage = encode_error_msg(SenderNonce, Reason, incorrectData),
            send(error, PKIMessage)
    end;
%% Decode the Confirmation message (OPTIONAL). 
decode_msg2(_,{pkiconf,_CertReqMsg},_Protetion,_ExtraCerts,certConf) ->
    put(msg_state, pkiconf),
    handle_cancel(),
    ok;
%% Decode error message
decode_msg2(_, {error, ErrorMsg},_,_,_) ->
    #'ErrorMsgContent'{
        pKIStatusInfo = PKIStatusInfo,
        errorCode = _ErrorCode,
        errorDetails = _ErrorDetails} = ErrorMsg,

    #'PKIStatusInfo'{
        status       = Status,
        statusString = [StatusString],
        failInfo     = FailInfo} = PKIStatusInfo,
    String = binary_to_list(StatusString),
    certNcServer:update_status(get(index), cmpv2_msg_error,
        {error, Status, String, FailInfo}),
    ok;
%% Not supported messages
decode_msg2(_, {Type, _CertReqMsg}, _,_,_) ->
    Msg = "No support for CMPv2 message type: " ++ atom_to_list(Type),
    certLib:sec_log("", Msg),
    info_msg(Msg, []),
    certNcServer:update_status(get(index), cmpv2_enrollment_failed, Msg),
    ok.

decode_msg3(MsgType, SenderNonce, CertReqId, CertReqId, Status,
    CertifiedKeyPair, RspInfo, CertList)
when MsgType == ip; MsgType == kup ->
    #'PKIStatusInfo'{status       = S,
                     statusString = StatusString,
                     failInfo     = FailInfo} = Status,
    decode_msg4(MsgType, SenderNonce, CertReqId, S, StatusString,
        FailInfo, CertifiedKeyPair, RspInfo, CertList);
decode_msg3(pollRep, SenderNonce, CertReqId, CertReqId, PollReason,
    CheckAfter, _, CertList) ->
    decode_msg4(pollRep, SenderNonce, CertReqId, PollReason, undefined,
        undefined, CheckAfter, undefined, CertList);
decode_msg3(MsgType, SenderNonce, RCertReqId, PCertReqId, _Status,
    _CertifiedKeyPair, _RspInfo, _CertList) ->
    info_msg("The ~p CertReqId is not matching ~p vs ~p~n",
        [MsgType, RCertReqId, PCertReqId]),
    certNcServer:update_status(get(index), cmpv2_enrollment_failed,
        "CMPv2 " ++ atom_to_list(MsgType) ++ " message received " ++
        "certRequestId is not matching, " ++ RCertReqId  ++ " vs " ++
        PCertReqId),
    certLib:sec_log("",
        "CMPv2, certRequestId is not matching, " ++ atom_to_list(MsgType)),
    PKIMessage = encode_error_msg(SenderNonce,
        "certRequestId is not matching", badCertId),
    send(error, PKIMessage),
    ok.


%% Check status, if polling or not
%% {accepted,0}
%% {grantedWithMods,1}
%% {rejection,2}
%% {waiting,3}
%% {revocationWarning,4}
%% {revocationNotification,5}
%% {keyUpdateWarning,6}
decode_msg4(MsgType, SenderNonce, _CertReqId, accepted, _,_,
    _CertifiedKeyPair, _RspInfo, {[],_,_})
when MsgType == ip; MsgType == kup ->
    %% No valid ca certificate found
    Reason = "No valid ca certificate found",
    certNcServer:update_status(get(index), cmpv2_enrollment_failed, Reason),
    PKIMessage =
    encode_error_msg(SenderNonce, Reason, incorrectData),
    send(error, PKIMessage);
decode_msg4(MsgType, SenderNonce, CertReqId, accepted, _,_,
    CertifiedKeyPair, RspInfo, CertList)
when MsgType == ip; MsgType == kup ->
    #'CertifiedKeyPair'{certOrEncCert   = Cert,
                        privateKey      = PrivateKey,
                        publicationInfo = PublicationInfo} = CertifiedKeyPair,
    decode_msg5(MsgType, SenderNonce, CertReqId, Cert, PrivateKey,
        PublicationInfo, RspInfo, CertList);
decode_msg4(ip, SenderNonce, CertReqId, waiting, _,_,_CertifiedKeyPair,_,_) ->
    PKIMessage =
    encode_poll_req_msg(ir, SenderNonce, CertReqId),
    send(new, PKIMessage);
decode_msg4(kup, SenderNonce, CertReqId, waiting,_,_,_CertifiedKeyPair,_,_) ->
    PKIMessage =
    encode_poll_req_msg(kur, SenderNonce, CertReqId),
    send(new, PKIMessage);
decode_msg4(ip, _SenderNonce, _CertReqId, rejection, _, transactionIdInUse,
    _CertifiedKeyPair, _RspInfo, _CertList) ->
    %% TransactionId already in use.
    certNcServer:update_status(get(index), restart,
        "TransactionId already in use, restart with a new."),
    ok;
decode_msg4(kup,_SenderNonce, _CertReqId, rejection, _, transactionIdInUse,
    _CertifiedKeyPair, _RspInfo, _CertList) ->
    %% TransactionId already in use.
    certNcServer:update_status(get(index), restart,
        "TransactionId already in use, restart with a new."),
    ok;
decode_msg4(pollRep, SenderNonce, CertReqId, PollReason, _,_,
    CheckAfter, _, _) ->
    decode_msg5(pollRep, SenderNonce, CertReqId, get(msg_state), CheckAfter,
        PollReason, undefined, undefined);
decode_msg4(MsgType, _, _CertReqId, Status, StatusString, FailInfo,
    _CertifiedKeyPair, _RspInfo, _CertList) ->
    String =
    case StatusString of
        [StatusS] ->
            binary_to_list(StatusS);
        StatusS ->
            binary_to_list(StatusS)
    end,
    info_msg("MsgType: ~p, Status: ~p, StatusString: ~p, FailInfo: ~p~n",
        [MsgType, Status, String, FailInfo]),
    certNcServer:update_status(get(index), cmpv2_msg_error,
        {MsgType, Status, String, FailInfo}),
    certNcServer:update_status(get(index), restart,
        {get(uri), "CMPv2 server did not like the " ++ atom_to_list(MsgType) ++
            " message"}),
    ok.

decode_msg5(MsgType, SenderNonce, CertReqId, {certificate,
        {x509v3PKCert, Cert}}, _, _, _, {CaChain, _RaChain, TCs})
when MsgType == ip; MsgType == kup ->
    %% Verify that cert is signed by CaCertDer 
    CertDer = public_key:der_encode('Certificate', Cert),
    [CaCertDer|NcChain] = CaChain,
    Chain = lists:append(CaChain, [CertDer]),
    case public_key:pkix_path_validation(CaCertDer, Chain, []) of
        {ok,_} -> % signed by ca cert
            PKIMessage = encode_certconf_msg(MsgType, SenderNonce, Cert,
                CertReqId),
            send(new, PKIMessage),
            certNcServer:update_status(get(index), cmpv2_approved_cert,
                [CertDer] ++ NcChain),
            %% Install trusted certifcates from TCs and
            %% sub CA from CA chain list
            B =
            case lists:member(CaCertDer, TCs) of
                true ->
                    TCs;
                false ->
                    %% Use when IPsec changed
                    lists:append(TCs, [CaCertDer]) 
            end,
            %% to use new NC chain behaviour
            info_msg("Install as Trusted Certificates:~n~w~n", [B]),
            case swmI:node_type() of
                "R-VNFM" ->
                    %% R-VNFM for OAM and INFRA adds TCs to TCAT directly
                    case get(index) of
                        "oam" ->
                            case omc_https:get_cert_conf() of
                                {undefined, undefined} ->
                                    %% https MO not configured 
                                    certServer:pre_install(
                                        {"1","1","1","1","oam"}, B);
                                _ ->
                                    %% https MO configure use G2 stile
                                    certServer:pre_install(B)
                            end;
                        "infra" ->
                            certServer:pre_install({"1","1","1","1","infra"},B);
                        _ -> % As normal
                            certServer:pre_install(B)
                    end;
                _ -> % As normal
                    certServer:pre_install(B)
            end;
        Error -> % not signed or something else
            info_msg("Validation of node cert and its chain failed: ~p~n" ++
                "CaChain: ~p~nCertDer: ~p~n", [Error, CaChain, CertDer]),
            %certNcServer:update_status(get(index), cmpv2_enrollment_failed,
            %    "CMPv2 " ++ atom_to_list(MsgType) ++ " message received, " ++
            %    "certificate not signed by the CA"),
            %PKIMessage = encode_error_msg(SenderNonce,
            %    "certificate not signed by the CA", signerNotTrusted),
            %send(error, PKIMessage),
            certNcServer:update_status(get(index), restart,
                {get(uri), "CMPv2 " ++ atom_to_list(MsgType) ++
                    " message received, but the validation of the " ++
                    "certificate failed. Will retry."})
    end;
decode_msg5(pollRep, SenderNonce, CertReqId, StartMsgType, CheckAfter,
    PollReason, _, _) when StartMsgType == ir; StartMsgType == kur ->
    case get(enrollment_try_later_timer_ref) of
        canceled ->
            ok;
        TRef1 ->
            case TRef1 of
                undefined ->
                    ok;
                _ ->
                    erlang:cancel_timer(TRef1),
                    put(enrollment_try_later_timer_ref, undefined)
            end,
            info_msg("Send pollReq after ~p seconds, Index: ~p, PollReason: ~p",
                [CheckAfter, get(index), PollReason]),
            {pollReq, {ok, DataDer}} =
            encode_poll_req_msg(StartMsgType, SenderNonce, CertReqId),
            %% Delay with CheckAfter seconds
            TRef2 = erlang:send_after(CheckAfter*1000, self(),
                {new, {pollReq, {ok, DataDer}}}),
            put(enrollment_try_later_timer_ref, TRef2)
    end;
decode_msg5(MsgType, SenderNonce,_,_,_,_,_,_) when MsgType == ip; MsgType == kup ->
    info_msg("~p message, no support for encrypted cert~n", [MsgType]),
    certNcServer:update_status(get(index), cmpv2_enrollment_failed,
        "CMPv2 " ++ atom_to_list(MsgType) ++
        " message received unsupported certificate type"),
    PKIMessage = encode_error_msg(SenderNonce,
        "message received unsupported certificate type", badDataFormat),
    certLib:sec_log("", "CMPv2 " ++ atom_to_list(MsgType) ++
        " message received unsupported certificate type"),
    send(error, PKIMessage),
    ok.

%%% ----------------------------------------------------------
%%% #           handle_cancel()
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description:  Cancel an ongoing enrollment by removing the
%%%               table entry, meaning arriving CMPv2 message
%%%               will be thrown.
%%% ----------------------------------------------------------
handle_cancel() ->
    case get(http_cmpv2_ref) of
        undefined ->
            ok;
        Ref ->
            httpc:cancel_request(Ref, cert) 
    end,
    case get(enrollment_try_later_timer_ref) of
    undefined ->
        ok;
        canceled ->
            ok;
    Tref ->
        erlang:cancel_timer(Tref)
    end.

%%% ----------------------------------------------------------
%%% #           extraCertField(EnrollCaCert, EnrollCaFingerpri,
%%%                         ExtraCert)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description:  Go through the extraCert field and search for
%%%               certficates to be used.
%%% ----------------------------------------------------------
extraCertField(ExtraCerts, CaPubs, Sender) ->
    extraCertField(get(ca_cert), get(ca_finger_print), ExtraCerts, CaPubs,
        Sender, get(recipient)).

extraCertField(not_found, "", _ExtraCerts, _CaPubs,  _Sender, _Recip) ->
    info_msg("Missing both CA/RA cert and CA/RA fingerprint, one is required",
        []),
    {[], [], []};
extraCertField(_, _, asn1_NOVALUE, _CaPubs,  _Sender, _Recip) ->
    info_msg("Missing ExtraCerts, will not be able to validate CMPv2 message",
        []),
    {[], [], []};
extraCertField(not_found, EnrollCaFingerpri, ExtraCerts,
    CaPubs, Sender, Recip) ->
    search_extra_cert(not_found, EnrollCaFingerpri, ExtraCerts, CaPubs,
        Sender, Recip);
extraCertField(Cert, _, ExtraCerts, CaPubs, Sender, Recip) ->
    Hash = certLib:get_fingerprint_support(),
    Fingerprint = certLib:encode_fingerprint(crypto:hash(Hash, Cert)),
    search_extra_cert(Cert, Fingerprint, ExtraCerts, CaPubs, Sender, Recip).


search_extra_cert(Cert, Fingerprint, ExtraCerts, CaPubs, Sender, Recip) ->
    Certs =
    lists:filtermap(
        fun(X) ->
                case X of
                    {x509v3PKCert, CertEntity} ->
                        case catch public_key:der_encode('Certificate',
                                CertEntity) of
                            {'EXIT', Reason} ->
                                info_msg("~nEncode extraCert field " ++
                                    "certificate failed: ~p~n", [Reason]),
                                false;
                            CertDer ->
                                OTPCert =
                                public_key:pkix_decode_cert(CertDer, otp),
                                TBS  = OTPCert#'OTPCertificate'.tbsCertificate,
                                Issuer = format_name(
                                    TBS#'OTPTBSCertificate'.issuer),
                                Subject = format_name(
                                    TBS#'OTPTBSCertificate'.subject),
                                {true, {Issuer, Subject, CertDer}}
                        end;
                    _ ->
                        false
                end
        end, ExtraCerts),
    
    info_msg("Certs:~n~p~n", [Certs]),

    F_Recip = format_name(Recip),
    F_Sender = format_name(Sender),

    %info_msg("~nF_Recip: ~p~nF_Sender: ~p~n", [F_Recip, F_Sender]),

    CaChain = sort_out_certs(Cert, Certs, F_Recip),
    RaChain = sort_out_certs(Cert, Certs, F_Sender),
    TCs     = formatCaPubs(CaPubs),
    %info_msg("~n0 CaChain:~n~p~nRaChain:~n~p~n", [CaChain, RaChain]),

    case RaChain of
        [] ->
             %info_msg("~n1 CaChain:~n~p~nRaChain:~n~p~nTCs:~n~p~n",
             %    [CaChain, [], []]),
            {CaChain, [], TCs};
        _ ->
            [FingerCert|_] = RaChain,
            info_msg("~nCheck if fingerprint matches this cert:~n~p~n",
            [FingerCert]),
            case check_fingerprint(FingerCert, Fingerprint) of
                match ->
                    case public_key:pkix_path_validation(FingerCert, RaChain,
                            []) of
                        {ok,_} -> % siging RA cert is trusted                
                            %info_msg("~n2 CaChain:~n~p~nRaChain:~n~p~nTCs:~n~p~n",
                             %   [CaChain, RaChain, TCs]),
                            {CaChain, RaChain, TCs};
                        _ -> % Not trusted RA
                            {CaChain, [], TCs}
                    end;
                _ ->
                    %info_msg("~n3 CaChain:~n~p~nRaChain:~n~p~nTCs:~n~p~n",
                    %    [CaChain, [], []]),
                    {CaChain, [], TCs}
            end
    end.

format_name({rdnSequence, List}) ->
    certLib:format_rdn({rdnSequence, lists:sort(List)});
format_name({directoryName,{rdnSequence, List}}) ->
    certLib:format_rdn({rdnSequence, lists:sort(List)}).


check_fingerprint(DerCert, Fingerprint) ->
    case certVerify:verify_fingerprint(DerCert, Fingerprint) of
        match ->
            info_msg("~nExtra Cert fingerprint matches~n", []),
            match;
        _ ->
            no_match
    end.


sort_out_certs(Cert, Certs, Name) ->
    case lists:keyfind(Name, 2, Certs) of
        false ->
            case Cert of
                not_found ->
                    info_msg("Installed CA/RA cert not found", []),
                    [];
                _ ->
                    OTPCert = public_key:pkix_decode_cert(Cert, otp),
                    TBS  = OTPCert#'OTPCertificate'.tbsCertificate,
                    Subject = format_name(TBS#'OTPTBSCertificate'.subject),
                    case {Subject, Name} of
                        {Same, Same} -> % Match
                            [Cert];
                        _ -> %% Configured enrollment cert wrong
                            info_msg("Configured enrollment certificate " ++
                                "wrong subject", []),
                            []
                    end
            end;
        {Name, Name, CertDer} -> % Self signed, done
            [CertDer];
        {Issuer, Name, CertDer} -> % Not self signed, start build ca chain
            NewCerts = lists:keydelete(Name, 2, Certs),
            mk_chain(NewCerts, Issuer,[CertDer])
    end.


mk_chain([], _, Chain) ->
    Chain;
mk_chain(List, Issuer, Chain) ->
    case lists:keyfind(Issuer, 2, List) of
        {Issuer, Issuer, CertDer} -> % self signed
            mk_chain([], not_used, lists:append([CertDer], Chain));
        {NewIssuer, Issuer, CertDer} -> % Not self signed, keep rolling
            NewList = lists:keydelete(Issuer, 2, List),
            mk_chain(NewList, NewIssuer, lists:append([CertDer], Chain));
        false ->
            mk_chain([], not_used, Chain)
    end.

formatCaPubs(asn1_NOVALUE) ->
    [];
formatCaPubs(CaPubs) ->
    lists:filtermap(
        fun(X) ->
                case X of
                    {x509v3PKCert, CertEntity} ->
                        case catch public_key:der_encode('Certificate',
                                CertEntity) of
                            {'EXIT', Reason} ->
                                info_msg("~nEncode CaPubs field " ++
                                    "certificate failed: ~p~n", [Reason]),
                                false;
                            CertDer ->
                                {true, CertDer}
                        end;
                    _ ->
                        false
                end
        end, CaPubs).

%%% ----------------------------------------------------------
%%% #           verify_pki_msg(Header, Body, Protection, CertList,
%%%%                           Data, Pvno, Sender, ProtectionAlg)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description:  Verify the protection field and other stuff
%%%               to see that the received CMPv2 PKI message
%%%               is correct and valid.
%%% ----------------------------------------------------------
verify_pki_msg(_,_,_,_,Pvno,_)
when Pvno =/= cmp2000 ->
    {error, "CMPv2 message received rejected, Pvno == " ++ atom_to_list(Pvno)};
verify_pki_msg(_,_,_, {_,[],_},_,_)  ->
    {error, "No valid ra certificate found"};
verify_pki_msg(Header,Body,Protection,{_,RaChain,_},_,ProtectionAlg)  ->
    OTPCert = public_key:pkix_decode_cert(lists:last(RaChain), otp),
    TBS     = OTPCert#'OTPCertificate'.tbsCertificate,
    SPKI    = TBS#'OTPTBSCertificate'.subjectPublicKeyInfo,
    PubKey  = SPKI#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
    Result  = verify_protection(Header, Body, Protection, PubKey,
        ProtectionAlg),
    verify_pki_msg1(Result).

verify_pki_msg1(valid) ->
    valid;
verify_pki_msg1(not_valid) ->
    {error, "CMPv2 message received, protection field not valid"}.
    
%%% ----------------------------------------------------------
%%% #           send(Type, {MsgType, {ok, DataDer}})
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Send HTTP POST message to the CMPv2 server. 
%%% ----------------------------------------------------------
send(new, {MsgType, {ok, DataDer}}) ->
    case get(enrollment_try_later_timer_ref) of
        canceled ->
            {ok, canceled};
        TRef1 ->
            case TRef1 of
                undefined ->
                    ok;
                _ ->
                    erlang:cancel_timer(TRef1),
                    put(enrollment_try_later_timer_ref, undefined)
            end,
            do_send(certLib:resolve_uri_inet(get(uri)), MsgType, DataDer)
    end;
send(re_send, {MsgType, {ok, _DataDer}}) ->
    case get(enrollment_try_later_timer_ref) of
        canceled ->
            ok;
        _ ->
            Action =
            case {get(msg_state), MsgType} of
                {ir, ir} ->
                    restart;
                {kur, kur} ->
                    restart;
                {certConf, certConf} ->
                    restart;
                {error, _} ->
                    info_msg("Sent error msg to cmpv2 server, got no response!", []),
                    restart;
                _ ->
                    no_send
            end,
            case Action of
                restart ->
                    certNcServer:update_status(get(index), restart,
                        {get(uri), "Failed to get a response from CMPv2 server"}),
                    ok;
                _ ->
                    ok
            end
    end;
send(error, {MsgType, {ok, DataDer}}) ->
	case get(enrollment_try_later_timer_ref) of
        canceled ->
            {ok, canceled};
        TRef1 ->
            case TRef1 of
                undefined ->
                    ok;
                _ ->
                    erlang:cancel_timer(TRef1),
                    put(enrollment_try_later_timer_ref, undefined)
            end,
            do_send(certLib:resolve_uri(get(uri)), MsgType, DataDer)
    end;
send(_, {MsgType, {error, Reason}}) ->
    info_msg("Could not send due to error, reason: ~w", [Reason]),
    certNcServer:update_status(get(index), cmpv2_enrollment_failed,
        "Could not encode CMPv2 " ++ atom_to_list(MsgType) ++ " message"),
    {error, Reason}.

do_send({error, Reason}, _ , _) ->
    Uri   = get(uri),
    Index = get(index),
    info_msg("Resolve CMPv2 URI ~p failed, reason: ~p~n", [Uri, Reason]),
    certNcServer:update_status(Index, restart,
        {Uri, "Failed to resolve URI"}),
    ok;
do_send({ok, Uri, Inet}, MsgType, DataDer) ->
    SrcIf       = get(src_if),
    Header      = [],
    Type        = "application/pkixcmp",
    Body        = DataDer,
    HTTPOptions = [
        {timeout, ?REQUEST_TIMEOUT},
        {connect_timeout, ?REQUEST_CONNECT_TIMEOUT}],
    Options     = [{sync, false}, {body_format, binary},
        {ipv6_host_with_brackets, true}],
    httpc:set_options([{ipfamily, Inet},
            {socket_opts, certLib:get_socket_opts(SrcIf, Inet)}], cert),
    Index = get(index),
    info_msg("Send ~p message for ~p", [MsgType, Index]),
    case catch httpc:request(post, {Uri, Header, Type, Body},
            HTTPOptions,Options, cert) of
        {ok, RequestId} ->
            put(http_cmpv2_ref, RequestId),
            TRef2 = erlang:send_after(?RETRY_TIMEOUT, self(),
                {re_send, {MsgType, {ok, DataDer}}}),
            put(enrollment_try_later_timer_ref, TRef2),
            certNcServer:update_status(Index, cmpv2_sent_msg, MsgType);
        {_, Reason} -> % Can be 'EXIT' by catch or error by httpc
            info_msg("HTTPC REQUEST ERROR, REASON: ~p", [Reason]),
            case get(msg_state) of
                undefined ->
                    handle_cancel(),
                    ok;
                _ ->
                    certNcServer:update_status(Index, restart,
                        {Uri, "Failed to get a response from CMPv2 server"})
            end
    end.

%%% ----------------------------------------------------------
%%% #           decode_reply_info({RequestId, Result|{error, Reason}})
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Decode the reply info from the http request
%%%              post message from the CMPv2 server. 
%%% ----------------------------------------------------------
decode_reply_info({_RequestId, {error, Reason}}) ->
    {error, Reason};
decode_reply_info({_RequestId, {{"HTTP/1.1",200,_},_, Body}}) ->
    Body;
decode_reply_info({_RequestId, Result}) ->
    {error, Result}.

decode_cert(CertList) ->
    lists:filtermap(
        fun(Cert) ->
                case 'PKIX1Explicit88':decode('Certificate', Cert) of
                    {ok, C} ->
                        {true, C};
                    _ ->
                        false
                end
        end, CertList).

mk_extra_cert(CertList) ->
    lists:filtermap(
        fun(Cert) ->
                {true, {x509v3PKCert, Cert}}
        end, CertList).

build_vc_sender(Cert) when is_record(Cert, 'Certificate') ->
    TBS = Cert#'Certificate'.tbsCertificate,
    {directoryName, TBS#'TBSCertificate'.subject}.

%%% ----------------------------------------------------------
%%% #           mk_protection_alg(Type, Data) ->
%%% Input: Type: cert|shared
%%%        Data: Cert - in DER format | {Salt, IterationCount}
%%% Output: {Algorithm, Parameters}
%%% Exceptions: 
%%% Description: Make a protection alg for vc or shared secret.
%%% ----------------------------------------------------------
mk_protection_alg(cert, CertDer) ->
    OTPCert    = public_key:pkix_decode_cert(CertDer, otp),
    TBS        = OTPCert#'OTPCertificate'.tbsCertificate,
    SPKI       = TBS#'OTPTBSCertificate'.subjectPublicKeyInfo,
    DerSPK =
    case SPKI#'OTPSubjectPublicKeyInfo'.subjectPublicKey of
        {'ECPoint', D} ->
            public_key:der_encode('ECPoint', D);
        SPK ->
            public_key:der_encode(element(1, SPK), SPK)
    end,
    Algo = SPKI#'OTPSubjectPublicKeyInfo'.algorithm,
    {AlgoSign, Para} = 
    case Algo#'PublicKeyAlgorithm'.algorithm of
        ?'id-ecPublicKey' ->
            {?'ecdsa-with-SHA256', DerSPK};
        _ ->
            {?'sha256WithRSAEncryption', <<5, 0>>}  %% HV80460
    end,
    #'AlgorithmIdentifier'{
        algorithm  = AlgoSign,
        parameters = Para};
mk_protection_alg(shared, {Salt, IterationCount}) ->
    Owf =
    #'AlgorithmIdentifier'{
        algorithm  = ?'id-sha1',
        parameters = asn1_NOVALUE},
    Mac =
    #'AlgorithmIdentifier'{
        algorithm  = {1,3,6,1,5,5,8,1,2}, %% HMAC-SHA1
        parameters = asn1_NOVALUE},
    PBMP =
     #'PBMParameter'{
         salt           = Salt,
         owf            = Owf,
         iterationCount = IterationCount,
         mac            = Mac},
    {ok, PBMP_encoded} =
    'PKIXCMP':encode('PBMParameter', PBMP),
     #'AlgorithmIdentifier'{
         algorithm  = ?'id-PasswordBasedMac',
         parameters = PBMP_encoded}.
    
%%% ----------------------------------------------------------
%%% #           mk_protection(PKIHeader, PKIBody, Cred) ->
%%% Input: - 
%%% Output: binary()
%%% Exceptions: 
%%% Description: Make a protection for PKIProtection field.
%%% ----------------------------------------------------------
mk_protection(PKIHeader, PKIBody, Cred) ->
    ProtectedPart =
    #'ProtectedPart'{
        header = PKIHeader,
        body   = PKIBody},
    {ok, DerProtectedPart} =
    'PKIXCMP':encode('ProtectedPart', ProtectedPart),
    do_mk_protection(DerProtectedPart, Cred).
    

do_mk_protection(DerProtectedPart, shared) ->
    crypto:hmac('sha', get(key), DerProtectedPart);
do_mk_protection(DerProtectedPart, PrivDerKey) ->
    PrivKey = certKey:format_priv_key(PrivDerKey),
    public_key:sign(DerProtectedPart, 'sha256', PrivKey).


%%% ----------------------------------------------------------
%%% #           verify_protection(PKIHeader, PKIBody, Protection,
%%%                               DerPubKey, ProtectionAlg) ->
%%% Input: - 
%%% Output: valid | not_valid
%%% Exceptions: 
%%% Description: Verify the protection field.
%%% ----------------------------------------------------------
verify_protection(PKIHeader, PKIBody, Protection,
    PublicKey, ProtectionAlg) ->
    ProtectedPart =
    #'ProtectedPart'{
        header = PKIHeader,
        body   = PKIBody},
    {ok, DerProtectedPart} =
    'PKIXCMP':encode('ProtectedPart', ProtectedPart),
    case format_protection_alg(ProtectionAlg#'AlgorithmIdentifier'.algorithm) of
        not_supported ->
            not_valid;
        Alg ->
            case catch public_key:verify(DerProtectedPart, Alg,
                    Protection, PublicKey) of
                true ->
                    valid;
                _ ->
                    not_valid
            end
    end.

format_protection_alg(?'sha1WithRSAEncryption') ->
    'sha';
format_protection_alg(?'sha224WithRSAEncryption') ->
    'sha224';
format_protection_alg(?'sha256WithRSAEncryption') ->
    'sha256';
format_protection_alg(?'sha384WithRSAEncryption') ->
    'sha384';
format_protection_alg(?'sha512WithRSAEncryption') ->
    'sha512';
format_protection_alg(?'ecdsa-with-SHA512') ->
    'sha512';
format_protection_alg(?'ecdsa-with-SHA384') ->
    'sha384';
format_protection_alg(?'ecdsa-with-SHA256') ->
    'sha256';
format_protection_alg(?'ecdsa-with-SHA2') ->
    'sha256';
format_protection_alg(?'ecdsa-with-SHA224') ->
    'sha224';
format_protection_alg(?'ecdsa-with-SHA1') ->
    'sha';
format_protection_alg(Alg) ->
    info_msg("Protection Alg ~p is not supported", [Alg]),
    not_supported.

%%% ----------------------------------------------------------
%%% #           mk_popo_sign(POPOSigningKeyInput, DerKey) ->
%%% Input: - 
%%% Output: binary()
%%% Exceptions: 
%%% Description: Make a protection for PKIProtection field.
%%% ----------------------------------------------------------
mk_popo_sign(POPOSigningKeyInput, DerKey) ->
    {ok, Der} =
    'PKIXCRMF-2005':encode('CertRequest', POPOSigningKeyInput),
    PrivKey = certKey:format_priv_key(DerKey),
    public_key:sign(Der, 'sha256', PrivKey).


%%% ----------------------------------------------------------
%%% #           build_rdnSequence(Subject)
%%% Input: string() - Subject name in string format
%%% Output: {directoryName, {rdnSequence, list()}}
%%% Exceptions: 
%%% Description: Build a directoryName tuple.
%%%              Ex. "C=SE,ST=Stockholm,L=Kista,O=Ericsson AB,
%%%                   OU=RO,CN=Test Root,CN=olle@ericsson.com"
%%% ----------------------------------------------------------
build_rdnSequence(Subject) ->
    %% split by comma %% regex avoids spliting string comma is escaped
    CommaSplitList = [lists:concat(X) || X <- re:split(Subject, "([^\\\\]),", [{return, list}, group])],

    %% split each list element by equal sign %% regex avoids spliting string if equal sign is escaped
    EqualSignDeepList = [[lists:concat(Y) || Y <- re:split(X, "([^\\\\])=", [{return, list}, group])] || X <- CommaSplitList],

    %% creates proplist 
    RDNSequence  = lists:filtermap(
                     fun([H|T]) ->
                        Key = string:strip(H),
                        Filter = proplists:is_defined(Key, transformNameList()),
                        case Filter of
                            true ->
                                Type = proplists:get_value(Key, transformNameList()),
                                NotPrintableString = lists:member(Key, ["E", "C", "SN", "SERIALNUMBER"]),
                                Value = case NotPrintableString of
                                            true -> av(Type, decode_reserved_chars(lists:flatten(T)));
                                            false -> av(Type, {printableString, decode_reserved_chars(lists:flatten(T))})
                                        end,
                                {true, [#'AttributeTypeAndValue'{type  = Type, value = Value}]};
                            false -> false
                        end
                     end, EqualSignDeepList),
    info_msg("RDNSequence as follows: ~p", [RDNSequence]),
    {rdnSequence, RDNSequence}.

transformNameList() ->
    [{"CN", ?'id-at-commonName'},
     {"L", ?'id-at-localityName'},
     {"ST", ?'id-at-stateOrProvinceName'},
     {"O", ?'id-at-organizationName'},
     {"OU", ?'id-at-organizationalUnitName'},
     {"E", ?'id-emailAddress'},
     {"C", ?'id-at-countryName'},
     {"SERIALNUMBER", ?'id-at-serialNumber'},
     {"SN", ?'id-at-serialNumber'}].

av(Id, Val) ->
    Fun = 'OTP-PUB-KEY':'getenc_SupportedAttributeTypeAndValues'(Id),
    {Data, _Len} = Fun('Type', Val, ignore),
    iolist_to_binary(Data).

%% decodes attribute value following RFC 2253 standard
decode_reserved_chars(String) -> 
    decode_reserved_chars(String, []).
decode_reserved_chars([], ReturnString) -> 
    StripSpaces = string:strip(ReturnString), %% strip spaces
    string:strip(StripSpaces, left, $#); %% strips hashtags (#) from the beggining of attribute value
decode_reserved_chars([Char|String], ReturnString) ->
    case [Char] of
        "\\" ->
            [H|T] = String,
            MatchFirstChar = re:run([H], "[0-9|a-f|A-F]"),
            {DecodedChar, Tail} = case MatchFirstChar of
                 {match, _} -> 
                    [H2|T2] = T,
                    MatchSecondChar = re:run([H2], "[0-9|a-f|A-F]"),
                    case MatchSecondChar of
                        {match, _} ->
                            HexString = [H] ++ [H2],
                            IntASCII = list_to_integer(HexString, 16),
                            {IntASCII, T2};
                        nomatch -> bad_name_sequence % bad name, very very bad name, down name!
                    end;
                 nomatch -> {H, T}
            end,
            decode_reserved_chars(Tail, ReturnString ++ [DecodedChar]);
        _ ->
            decode_reserved_chars(String, ReturnString ++ [Char])
    end.

%%% ----------------------------------------------------------
%%% #           get_cn(SubjectName)
%%% Input: string() - Subject name in string format
%%% Output: binary() of cn
%%% Exceptions: 
%%% Description: Get the cn (common name) from the DN (subject name)
%%% ----------------------------------------------------------
get_cn(SubjectName) when is_list(SubjectName) ->
    Name =
    lists:filtermap(
        fun(X) ->
                case string:tokens(X, "=") of
                    ["CN"|Val] ->
                        {true, lists:flatten(Val)};
                    [" CN"|Val] ->
                        {true, lists:flatten(Val)};
                    _ ->
                        false
                end
        end, string:tokens(SubjectName, ",")),
    list_to_binary(Name);
get_cn(Cert) when is_record(Cert, 'Certificate') ->
    TBS = Cert#'Certificate'.tbsCertificate,
    {rdnSequence, Subject} = TBS#'TBSCertificate'.subject,
    [Name] =
    lists:filtermap(
        fun([X]) ->
                case X#'AttributeTypeAndValue'.type of
                    ?'id-at-commonName' ->
                        Binary = X#'AttributeTypeAndValue'.value,
                        %{ok, {printableString, CN}} =
                        {ok, {_, CN}} =
                        'OTP-PUB-KEY':decode('X520CommonName', Binary),
                        {true, CN};
                    _ ->
                        false
                end
        end, Subject),
    list_to_binary(Name).


%%% ----------------------------------------------------------
%%% #           mk_cert_req_id()
%%% input: 
%%% output: 
%%% exceptions: 
%%% description: create a certificate request id.
%%% ----------------------------------------------------------
mk_cert_req_id() ->
    [A1,A2,A3,A4] = binary_to_list(crypto:strong_rand_bytes(4)),
     A1*1000000 + A2*10000 + A3*100 + A4.


%%% ----------------------------------------------------------
%%% #           utc_string()
%%% input: 
%%% output: 
%%% exceptions: 
%%% description: return current utc time in cert string format
%%% ----------------------------------------------------------
utc_string() ->
    {{Y,M,D},{HH,MM,SS}} = calendar:universal_time(),
    list_to_binary(
        lists:flatten(io_lib:format("~4..0b~2..0b~2..0b~2..0b~2..0b~2..0bZ",
                [Y, M, D, HH, MM, SS]))).

%%% ----------------------------------------------------------
%%% #           get_cred()
%%% input: 
%%% output: 
%%% exceptions: 
%%% description: Fetch credential and convert the format
%%% ----------------------------------------------------------
get_cred() ->
    format_cred(certSecStore:get_cred(get(index))).

format_cred({cert, {VcCertPem, VcKeyPem}}) ->
    %% Change from PEM to DER format
    VcDerList = 
    lists:filtermap(
        fun({'Certificate', CertDer,_}) ->
                {true, CertDer};
            (_) ->
                false
        end, public_key:pem_decode(VcCertPem)),
    [{Format, VcKeyDer,_}] = public_key:pem_decode(VcKeyPem),
    {cert, {{Format, VcKeyDer}, VcDerList}};
format_cred({shared, Secret}) ->
    %% Not used yet
    Salt = crypto:strong_rand_bytes(16),
    OTP  = certKey:mk_otp(Salt, 1000, Secret),
    {shared, {Salt, 1000, OTP}};
format_cred({error, Reason}) ->
    {error, Reason}.


info_msg(Format, Args) ->
   sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

