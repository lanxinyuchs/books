%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certVerify.erl %
%%% @author ebabmat
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/3
%%% 
%%% @doc ==Verification module for certificates==
%%% This module implements the verify part in CERT 

-module(certVerify).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/3').
-date('2017-10-04').
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
%%% Rev      Date        Name     What
%%% -----    ----------  -------  -------------------------
%%% R2A/0    2014-01-15  etxasta  Created
%%% R2A/24   2014-08-28  etxlg    Disabled CRL since it doesn't work for the
%%%				  release verification and I don't know how
%%%		                  to fix it.
%%% R6A/7    2016-09-05  emariad  Random deprectated, changed to rand.
%%% R6A/8    2016-09-05  emariad  Changed back to random.
%%% R7A/1    2016-09-15  emariad  Random deprectated, changed to rand for 17B.
%%% R7A/2    2016-09-16  etomist  HV23010
%%% R7A/3    2016-09-27  etxasta  Added verify_peer_vc
%%% R7A/4    2016-09-29  ehsake   Ignore key usage for peer vc
%%% R8A/1    2016-11-17  etxasta  Added more log for validate_date
%%% R9A/1    2017-02-23  etomist  HV66400, EC fix, part 2
%%% R10A/1   2017-08-08  enenteo  Fix for IPTS fault, fix sec_verify_peer_vc
%%%                                            to behave as explained in SECI if description
%%% R11A/1,2 2017-09-27  emarnek  HW32111
%%% R11A/3   2017-10-04  ebabmat  HW15883
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([mk_verify_fun/3,
         verify_tc/1,
         verify_nc/2]).

-export([get_user/1]).

-export([verify_peer_vc/2,
         verify_peer/4]).

-record(verify_state, {
        crls     = [],
        chain    = [],
        trusted_cert,
        crl_status}).

-export([verify_fingerprint/2]).
-export([shuffle/1]).


-export([validate_date/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include_lib("public_key.hrl").
-include("RcsCertM.hrl").
-include("cert.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


%%% ----------------------------------------------------------
%%% @doc verify certificates
%%% @end
%%% ----------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #        verify_nc(Cert, PrivKey) ->
%%% Input:  
%%% Output: valid | {failed, no_match|invalid_key_usage|
%%%         ext_key_usage_notvalid|invalid_date}
%%%         
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
verify_nc(Cert, {Format, PrivKey}) ->
    info_msg("Verify NC, Key format: ~w~n", [Format]),

    case certKey:format_priv_key({Format, PrivKey}) of
        Priv when is_record(Priv, 'RSAPrivateKey') -> %% RSA
            verify_nc0(Cert, {'RSAPrivateKey', PrivKey});
        Priv when is_record(Priv, 'ECPrivateKey') -> %% EC
            verify_nc0(Cert, {'ECPrivateKey', Priv});
        _ ->
            {failed, no_match}
    end.

verify_nc0(Cert, {'RSAPrivateKey', PrivKey}) ->
    %% Check the cert and key match
    CertFile = filename:join(certLib:nc_dir(get(key)), "tmp.crt"),
    ok = filelib:ensure_dir(CertFile),
    KeyFile = filename:join(certLib:nc_dir(get(key)), "tmp.key"),
    ok = file:write_file(KeyFile, PrivKey),
    Modulus1 =
    certLib:cmd(["openssl rsa -inform DER -noout -modulus -in ",
            KeyFile, " | openssl md5"]),
    ok = file:delete(KeyFile),
    ok = file:write_file(CertFile, Cert),
    Modulus2 =
    certLib:cmd(["openssl x509 -inform DER -noout -modulus -in ",
            CertFile, " | openssl md5"]),
    ok = file:delete(CertFile),
    OTPCert = public_key:pkix_decode_cert(Cert, otp),
    TBS = OTPCert#'OTPCertificate'.tbsCertificate,
    verify_nc1(Modulus1, Modulus2, TBS);
verify_nc0(Cert, {'ECPrivateKey', Priv}) ->
    PubKey1 = Priv#'ECPrivateKey'.publicKey, %%HV66400
    OTPCert = public_key:pkix_decode_cert(Cert, otp),
    TBS     = OTPCert#'OTPCertificate'.tbsCertificate,
    SPKI    = TBS#'OTPTBSCertificate'.subjectPublicKeyInfo,
    {'ECPoint', PubKey2} = SPKI#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
    verify_nc1(PubKey1, PubKey2, TBS).


verify_nc1(Same, Same, TBS) ->
    %% Checks the valid date
    V = TBS#'OTPTBSCertificate'.validity,
    verify_nc2(validate_date(V#'Validity'.notBefore, V#'Validity'.notAfter),
    TBS);
verify_nc1(_,_,_) ->
    {failed, no_match}.


verify_nc2(invalid, _) ->
    {failed, invalid_date};
verify_nc2(valid, TBS) ->
    %% Check KeyUsage
    List =
    case TBS#'OTPTBSCertificate'.extensions of
        asn1_NOVALUE ->
            [];
        _ ->
            TBS#'OTPTBSCertificate'.extensions
    end,
    KeyUsage = 
    lists:filtermap(
        fun(X) ->
                case {X#'Extension'.extnID,
                        X#'Extension'.critical,
                        X#'Extension'.extnValue} of
                    {_, true, undefined} ->
                        {true, unknown_extension_value};
                    {?'id-ce-keyUsage', _, undefined} ->
                        {true, unknown_extension_value};
                    {?'id-ce-keyUsage', _, Value} ->
                        %% KU_DIGITAL_SIGNATURE, KU_KEY_ENCIPHERMENT and
                        %% KU_KEY_AGREEMENT must be set else reject
                        DS = lists:member(digitalSignature, Value),
                        KE = lists:member(keyEncipherment, Value),
                        KA = lists:member(keyAgreement, Value),
                        case {DS, KE, KA} of
                            {true, true, true} -> %% For server
                                false;
                            {true, false, true} -> %% For client
                                false;
                            {true, false, false} -> %% For client
                                false;
                            {true, true, false} -> %% 
                                false;
                            _ ->
                                Msg =
                                "KeyUsage failed" ++
                                " KU_DIGITAL_SIGNATURE: " ++atom_to_list(DS)++
                                ", KU_KEY_ENCIPHERMENT: " ++atom_to_list(KE) ++
                                ", KU_KEY_AGREEMENT: " ++ atom_to_list(KA),
                                info_msg(Msg, []),
                                certLib:sec_log("", Msg),
                                {true, key_usage_notvalid}
                        end;
                    {?'id-ce-extKeyUsage', _, undefined} ->
                        {true, unknown_extension_value};
                    {?'id-ce-extKeyUsage', _, Value} ->
                        %% id-kp-serverAuth set for
                        %% SSL Server cert, else reject
                        case lists:member(?'id-kp-serverAuth', Value) of
                            true ->
                                false;
                            false ->
                                %{true, ext_key_usage_notvalid}
                                %% NOTE The cert can be used as client cert also
                                false
                        end;
                    _ ->
                        false
                end
        end, List),
    case KeyUsage of
        [] ->
            valid;
        Faults ->
            info_msg("KeyUsage failed: ~p~n", [KeyUsage]),
            {failed, Faults}
    end.


validate_date({utcTime, Before}, {utcTime, After}) ->
    {{Year, Mon, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    List =
    lists:filtermap(
        fun(Integer) ->
                String = integer_to_list(Integer),
                case length(String) of
                    2 ->
                        {true, String};
                    1 ->
                        {true, "0" ++ String};
                    4 ->
                        {true, string:substr(String, 3, 4)}
                end
        end, [Year, Mon, Day, Hour, Min, Sec]),
    C = list_to_integer(lists:append(List)),
    A = list_to_integer(string:substr(After, 1, length(After)-1)),
    B = list_to_integer(string:substr(Before, 1, length(Before)-1)),

    case C of
        C when B < C, C < A ->
            valid;
        %HW32111, cover equal cases
        C when C =< B ->
            info_msg("Invalid, current time (~p) is before valid time (~p)",
                [C, B]),
            invalid;
        C when A =< C ->
            info_msg("Invalid, current time (~p) is after the valid time (~p)",
                [C, A]),
            invalid
    end;
validate_date({generalTime, Before}, {generalTime, After}) ->
    {{Year, Mon, Day}, {Hour, Min, Sec}} = calendar:universal_time(),

    List =
    lists:filtermap(
        fun(Integer) ->
                String = integer_to_list(Integer),
                case length(String) of
                    2 ->
                        {true, String};
                    1 ->
                        {true, "0" ++ String}
                end
        end, [Mon, Day, Hour, Min, Sec]),
    C = list_to_integer(integer_to_list(Year) ++ lists:flatten(List)),
    A = list_to_integer(string:substr(After, 1, length(After)-1)),
    B = list_to_integer(string:substr(Before, 1, length(Before)-1)),
    case C of
        C when B < C, C < A ->
            valid;
        %HW32111, cover equal cases
        C when C =< B ->
            info_msg("Invalid, current time (~p) is before valid time (~p)",
                [C, B]),
            invalid;
        C when A =< C ->
            info_msg("Invalid, current time (~p) is after the valid time (~p)",
                [C, A]),
            invalid
    end.
 
    

%%% ----------------------------------------------------------
%%% #        verify_tc(Cert) ->
%%% Input:  
%%% Output: valid | invalid
%%%         
%%% Exceptions: 
%%% Description: Check if the dates are valid for the trusted cert 
%%% ----------------------------------------------------------
verify_tc(undefined) ->
    valid;
verify_tc(Cert) ->
    %% Checks the valid date
    OTPCert = public_key:pkix_decode_cert(Cert, otp),
    TBS = OTPCert#'OTPCertificate'.tbsCertificate,
    V = TBS#'OTPTBSCertificate'.validity,
    validate_date(V#'Validity'.notBefore, V#'Validity'.notAfter).



%% NOTE This function should be used for VC peer certificates by DU-DU
%%% ----------------------------------------------------------
%%% #           verify_peer_vc(PeerVcChain, Socket)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Verify peer vendor cerificate towards the node vc chain.
%%% ----------------------------------------------------------
verify_peer_vc(PeerVcChain, Socket) ->
    put(verify_peer_socket, Socket),
    Chain       = certLib:pem_to_der_list(PeerVcChain),
    {ok, VC, _} = certSecStore:get_vc(),
    CaCert      = get_root_vc_cert(certLib:pem_to_der_list(VC)),
    TC          = {trusted_ca, CaCert},
    Res         = do_verify_peer(TC, Chain, vcpeer, ?FeatureState_DEACTIVATED),
    send_verify_peer_result(Res).


%%% ----------------------------------------------------------
%%% #           get_root_vc(VcChain)
%%% Input:  VC chain
%%% Output: 
%%% Exceptions: 
%%% Description: Function extract root CA cert since VC chain cannot gurantee
%%%                   the order in the chain
%%% ----------------------------------------------------------
get_root_vc_cert([]) ->
    info_msg("VC is missing root cert", []),
    [];

get_root_vc_cert([DerCert|VC_der_list]) ->
    OTPCert                = public_key:pkix_decode_cert(DerCert, otp),
    TBS                    = OTPCert#'OTPCertificate'.tbsCertificate,
    Issuer                 = TBS#'OTPTBSCertificate'.issuer,
    case TBS#'OTPTBSCertificate'.subject of
        Issuer ->
            DerCert;
         _->
        get_root_vc_cert(VC_der_list)
    end.
    

%% NOTE This function should be used for peer certificates by IPSec
%%% ----------------------------------------------------------
%%% #           verify_peer(Chain, TrustCategoryKey, Type, Socket)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Verify peer cerificate
%%% ----------------------------------------------------------
verify_peer(Chain, TrustCategoryKey, Type, Socket) ->
    put(verify_peer_socket, Socket),
    Result = 
    case mnesia:dirty_read(trustCategory, TrustCategoryKey) of
        [Obj] ->
            case Obj#trustCategory.trustedCertificates of
                [] ->
                    {error, not_found};
                TrustCertKeys ->
                    case Obj#trustCategory.crlCheck of
                        ?FeatureState_DEACTIVATED ->
                            %% Since only CRL check is required by IPSec,
                            %% if disabled nothing more is needed to be done
                            %% here. Just return valid.
                            {ok, valid};
                        CrlCheck ->
                            Certs = get_searchable_certs(Chain),
                            case get_client_cert(Certs) of
                                {ok, ClientCert} ->
                                    Sorted = sort_certs(ClientCert, Certs),
                                    TCDers = get_tc(TrustCertKeys),
                                    TrustCerts = get_searchable_certs(TCDers),
                                    {TrustAnchor, ChainTail} = 
                                    get_ta_and_ders(Sorted, TrustCerts),
                                    do_verify_peer(TrustAnchor, 
                                                   ChainTail, 
                                                   Type, 
                                                   CrlCheck);
                                Error ->
                                    info_msg("No client cert: ~p~n", [Error]),
                                    Error
                            end
                    end
            end;
        _ ->
            {error, not_found}
        end,
    send_verify_peer_result(Result).

sort_certs(Client, Certs) -> sort_certs(Client, Certs, [Client]).
sort_certs(_Cert, [], Sorted) -> Sorted;
sort_certs({Issuer, _Subject, _Der}, Certs, Sorted) ->
    case lists:keyfind(Issuer, 2, Certs) of
        false ->
            Sorted; %% reached top
        SelfSigned = {Issuer, Issuer, _CertDer} ->
            [SelfSigned|Sorted]; %% reached top
        Any -> %% Keep searching
            sort_certs(Any, lists:keydelete(Issuer, 2, Certs), [Any|Sorted])
    end.

get_ta_and_ders(ClientChain, Trusted) -> %% search for ta starts from client
    get_ta_and_ders(lists:reverse(ClientChain), Trusted, []). 
get_ta_and_ders([], _, Ders) -> 
    {unknown_ca, Ders};
get_ta_and_ders([{Issuer, _Subject, Der}|ChainTail], TrustedCerts, Ders) ->
    case lists:keyfind(Issuer, 2, TrustedCerts) of 
        false -> %% no issuer found among node trusted certs
            get_ta_and_ders(ChainTail, TrustedCerts, [Der|Ders]);
        {_I, _S, TrustAnchorDer} -> %% issuer found therefore used as anchoroooo
            {{trusted_ca, TrustAnchorDer}, [Der|Ders]}
    end.

get_searchable_certs(DerCerts) -> %% returns certs with issuer and subject
    lists:map(
        fun(Der) ->
                OTPCert = public_key:pkix_decode_cert(Der, otp),
                TBS  = OTPCert#'OTPCertificate'.tbsCertificate,
                Issuer = certLib:format_rdn(TBS#'OTPTBSCertificate'.issuer), 
                Subject = certLib:format_rdn(TBS#'OTPTBSCertificate'.subject),
                {Issuer, Subject, Der}
        end, DerCerts).

get_client_cert(Certs) -> get_client_cert(Certs, []).
get_client_cert([], _) -> 
    {error, no_client_cert}; %% no client certs provided or looped, etc...
get_client_cert([Cert], _) -> {ok, Cert}; %% client cert is the only one left.
get_client_cert([Cert = {_Issuer, Subject, _Der}|Certs], Issuers) ->
    case lists:keyfind(Subject, 1, Certs ++ Issuers) of
        false -> {ok, Cert}; %% is not signing anyone 
                             %% therefore it's a client certificate.
                             %% if two client certificates provided in the chain
                             %% this will take the first one
        _Any -> get_client_cert(Certs, [Cert|Issuers])
            %% Cert is an issuer - therefore it cannot be the client
    end.

get_tc(TrustedCerts) ->
    lists:filtermap(
        fun(Dn) ->
                case certLib:decode_moref(Dn) of
                    {tc, Index} ->
                        do_get_tc(Index);
                    _ ->
                        false
                end
        end, TrustedCerts).

do_get_tc(Index) ->
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
                _ ->
                    false
            end;
        _ ->
            false
    end.

do_verify_peer(unknown_ca, _Chain, _Type, _CrlCheck) ->
    %% No known trusted certificate, so the peer, chain and CRLs can't
    %% be trusted and therfore not verified
    info_msg("Found no matching trusted certificate", []),
    {error, {bad_cert, {revoked, unknown_ca}}};
do_verify_peer({trusted_ca, TrustedCert}, Chain, Type, CrlCheck) ->
    {VF, PF} = mk_verify_fun([TrustedCert], {Type, undefined}, CrlCheck),
    public_key:pkix_path_validation(TrustedCert, Chain, [VF, PF]).


send_verify_peer_result(Res) ->
    case get(verify_peer_socket) of
        undefined ->
            Res;
        Socket ->
            certSeci:verify_peer_result(Res, Socket),
            %% Set to undefined to prevent several sends
            %% Observe that the verify check will still be running
            put(verify_peer_socket, undefined)
    end.


%% NOTE Used by all Erlang users, e.g. TLS and HTTPS
%%% ----------------------------------------------------------
%%% #        mk_verify_fun(TrustedCert, Instance, CrlCheck) ->
%%% Input:  Instance - {server|client, Pid}
%%% Output: Fun()
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
mk_verify_fun(TrustedCerts, Instance, CrlCheck) ->
    PC_fun =
    fun(Chain) ->
            get_trusted_cert(undefined, Chain, TrustedCerts)
    end,
    V_fun =
    fun(OtpCert, Data, UserState) ->
            check_verify(OtpCert, Data, UserState, Instance,
                TrustedCerts, CrlCheck)
    end,
    {{verify_fun, {V_fun, #verify_state{}}}, {partial_chain, PC_fun}}.


check_verify(_, {bad_cert, Reason}, _,_,_,_) ->
    info_msg("bad_cert: {bad_cert, ~p}~n", [Reason]),
    {fail, {bad_cert, Reason}};
check_verify(_,
    {extension, #'Extension'{extnID = ?'id-ce-cRLDistributionPoints'}},
    UserState,_,_,_) ->
    %% DO nothing
    {valid, UserState};
check_verify(_OtpCert,
    {extension, #'Extension'{extnID = ?'id-ce-subjectKeyIdentifier'}},
    UserState,_,_,_) ->
    %% TODO something
    {unknown, UserState};
check_verify(_OtpCert,
    {extension, #'Extension'{extnID = ?'id-ce-authorityKeyIdentifier'}},
    UserState,_,_,_) ->
    %% TODO something
    {unknown, UserState};
check_verify(_OtpCert,
    {extension, #'Extension'{extnID = ?'id-ce-keyUsage'} = Extension},
    UserState, Instance,_,_) ->
    check_key_usage(Extension, UserState, Instance);
check_verify(_OtpCert,
    {extension, #'Extension'{extnID = ?'id-ce-extKeyUsage'} = Extension},
    UserState, Instance,_,_) ->
    check_key_ext_usage(Extension, UserState, Instance);
check_verify(OtpCert, Valid, UserState, Instance, TrustedCerts, CrlCheck)
when Valid == valid; Valid == valid_peer ->
    DER_cert = public_key:pkix_encode('OTPCertificate', OtpCert, otp),
    Chain = lists:append(UserState#verify_state.chain, [DER_cert]),
    TrustedCert =
    get_trusted_cert(UserState#verify_state.trusted_cert, Chain, TrustedCerts),
    peer_validate(OtpCert, TrustedCert, Chain, UserState,
        Valid, Instance, CrlCheck);
check_verify(OtpCert, {extension, Extension}, UserState,_,_,_) ->
    Sub = get_sub_string(OtpCert),
    case Extension#'Extension'.critical of
        true ->
            info_msg("VARNING, FAIL!!!  Extension(???): ~p~n" ++
                "Sub: ~p~n", [Extension, Sub]),
            certLib:sec_log("", "Critical extension in cerfircate is unknown"),
            {fail, UserState};
        false ->
            {unknown, UserState}
    end.


peer_validate(_,unknown_ca, _,UserState,_,_,_) ->
    info_msg("Found no trusted certificate", []),
    certLib:sec_log("", "Found no trusted certificate"),
    {fail, UserState};
peer_validate(OtpCert, {trusted_ca, TrustedCert}, Chain, UserState,
    Valid, Instance, ?FeatureState_DEACTIVATED) ->
    NewUserState = UserState#verify_state{
        chain        = Chain,
        trusted_cert = TrustedCert},
    extract_user(OtpCert, Valid, Instance, NewUserState, crl_ok);
peer_validate(OtpCert, {trusted_ca, TrustedCert}, Chain, UserState,
    Valid, Instance, ?FeatureState_ACTIVATED) ->
    {CrlStatus, CrlInfo, UsedUrl} =
    case UserState#verify_state.crls of
        crl_missing ->
            {crl_missing, crl_missing, undefined};
        OldCrlInfo ->
            case find_crl(OtpCert) of
                no_dp ->
                    {no_dp, OldCrlInfo, undefined};
                crl_missing ->
                    {crl_missing, crl_missing, undefined};
                {DerCrl, Dp, Url} ->
                    CrlTuple =
                    {DerCrl, public_key:der_decode('CertificateList', DerCrl)},
                    {found, lists:append(OldCrlInfo, [{Dp, CrlTuple}]), Url}
            end
    end,

    NewUserState = UserState#verify_state{
        chain        = Chain,
        trusted_cert = TrustedCert,
        crls         = CrlInfo},

    %% Update CRL fun
    Fun1 =
    fun(_,Crl) ->
            Crl % Already updated
    end,    
    
    %% Issuer fun
    Fun2 =
    fun(_,_,_,_) ->
            OTPTrusted =
            public_key:pkix_decode_cert(TrustedCert, otp),
            {ok, OTPTrusted, UserState#verify_state.chain}
    end,
    
    case CrlStatus of
        no_dp ->
            extract_user(OtpCert, Valid, Instance, NewUserState, crl_ok);
        crl_missing ->
            extract_user(OtpCert, Valid, Instance, NewUserState, crl_missing);
        found ->
            Opt = [{update_crl, Fun1}, {issuer_fun,{Fun2, []}}],
            case catch public_key:pkix_crls_validate(OtpCert, CrlInfo, Opt) of
                {'EXIT', Reason} ->
                    info_msg("pkix_crls_validate broke, reason: ~p", [Reason]),
                    certLib:sec_log("", "Failed, CRL broken: " ++ UsedUrl),
                    certCrl:remove_cached_crl(UsedUrl),
                    extract_user(OtpCert, Valid, Instance,
                        NewUserState, Reason);
                valid ->
                    info_msg("pkix_crls_validate crl_ok, ~p", [Valid]),
                    extract_user(OtpCert, Valid, Instance,
                        NewUserState, crl_ok);
                {bad_cert, revocation_status_undetermined} ->
                    info_msg("pkix_crls_validate fail, reason: ~p",
                        [revocation_status_undetermined]),
                    certLib:sec_log("", "CRL check failed: " ++
                        "revocation_status_undetermined, url: " ++ UsedUrl),
                    certCrl:remove_cached_crl(UsedUrl),
                    extract_user(OtpCert, Valid, Instance, NewUserState,
                        {bad_cert, revocation_status_undetermined});
                {bad_cert, {revoked, Reason}} ->
                    info_msg("pkix_crls_validate fail, reason: ~p", [Reason]),
                    certLib:sec_log("", "CRL check failed, " ++
                        atom_to_list(Reason)),
                    extract_user(OtpCert, Valid, Instance,
                        NewUserState, Reason)
            end
    end.



find_crl(OtpCert) ->
    case public_key:pkix_dist_points(OtpCert) of
        [] ->
            no_dp;
        DPs ->
            %% Shuffle the order of DPs
            %% to prevent getting stuck on same broken CRL
            Shuffled = shuffle(DPs),
            case do_find_crl(Shuffled, cached) of
                crl_missing ->
                    %% If Socket not undefined this is used by the SECI if.
                    %% Therefore UNKNOWN status will be returned directly,
                    %% even the check will keep on going in the background,
                    %% e.g. downloading crls.
                    send_verify_peer_result({error,
                            {bad_cert, revocation_status_undetermined}}),
                    do_find_crl(Shuffled, download);
                CrlData ->
                    CrlData
            end
    end.


shuffle([]) ->
    [];
shuffle([Elem]) ->
    [Elem];
shuffle(List) ->
    shuffle(List, length(List), []).

shuffle([], 0, Result) ->
    Result;
shuffle(List, Len, Result) ->
    {Elem, Rest} = nth_rest(rand:uniform(Len), List),
    shuffle(Rest, Len - 1, [Elem|Result]).

nth_rest(N, List) ->
    nth_rest(N, List, []).

nth_rest(1, [E|List], Prefix) ->
    {E, Prefix ++ List};
nth_rest(N, [E|List], Prefix) ->
    nth_rest(N - 1, List, [E|Prefix]).


do_find_crl([],_) ->
    crl_missing;

do_find_crl([DP|T], Mode) ->
    case DP#'DistributionPoint'.distributionPoint of
        {fullName, URLs} ->
            case get_crl(URLs, Mode) of
                crl_missing ->
                    do_find_crl(T, Mode);
                {DerCrl, Url} ->
                    {DerCrl, DP, Url}
            end;
        _ ->
            do_find_crl(T, Mode)
    end.


get_crl([],_) ->
    crl_missing;
get_crl([{uniformResourceIdentifier, Url}|T], cached) ->
    case certCrl:get_cached_crl(Url) of
        crl_missing ->
            get_crl(T, cached);
        DerCrl ->
            info_msg("Found cached CRL: ~p", [Url]),
            {DerCrl, Url}
    end;
get_crl([{uniformResourceIdentifier, Url}|T], download) ->
    case certCrl:get_crl(Url) of
        {error, _} ->
            info_msg("CRL missing, ~p", [Url]),
            certLib:sec_log("","CRL missing, " ++ Url),
            get_crl(T, download);
        DerCrl ->
            info_msg("Found CRL: ~p", [Url]),
            {DerCrl, Url}
    end;
get_crl([NoSupport|T], Mode) ->
    info_msg("No support for: ~p~n", [NoSupport]),
    get_crl(T, Mode).

validate_chain(TrustCert, [_ | PartialChain] = Chain) ->
    case public_key:pkix_path_validation(TrustCert, Chain, []) of
        {ok, _} ->
            ok;
        _ ->
            validate_chain(TrustCert, PartialChain)
    end;
validate_chain(_, []) ->
    nok.

get_trusted_cert(undefined, _, []) ->
    unknown_ca;
get_trusted_cert(undefined, Chain, [TrustCert|T]) ->
    case validate_chain(TrustCert, Chain) of
        ok ->
            {trusted_ca, TrustCert};
        nok ->
            get_trusted_cert(undefined, Chain, T)
    end;
get_trusted_cert(TrustedCert, _, _) ->
    {trusted_ca, TrustedCert}.


check_key_usage(_Extension,UserState,{vcpeer,_}) ->
    {valid,UserState};

check_key_usage(Extension, UserState, {server,_}) ->
    %% Server cert
    case {Extension#'Extension'.extnValue, Extension#'Extension'.critical} of
        {undefined, true} ->
            info_msg("keyUsage critical missing~n", []),
            {fail, UserState};
        {undefined, false} ->
            {unknown, UserState};
        {Value, _} ->
            DS = lists:member(digitalSignature, Value),
            KE = lists:member(keyEncipherment, Value),
            KA = lists:member(keyAgreement, Value),
            case {DS, KE, KA} of
                {true, true, true} ->
                    {valid, UserState};
                _ ->
                    Msg =
                    "Server certifcate KeyUsage failed" ++
                    " KU_DIGITAL_SIGNATURE: " ++ atom_to_list(DS) ++
                    ", KU_KEY_ENCIPHERMENT: " ++ atom_to_list(KE) ++
                    ", KU_KEY_AGREEMENT: " ++ atom_to_list(KA),
                    info_msg(Msg, []),
                    certLib:sec_log("", Msg),
                    {fail, UserState}
            end
    end;
check_key_usage(Extension, UserState, {client,_}) ->
    %% Client cert
    case {Extension#'Extension'.extnValue, Extension#'Extension'.critical} of
        {undefined, true} ->
            {fail, UserState};
        {undefined, false} ->
            {unknown, UserState};
        {Value, _} -> 
            DS = lists:member(digitalSignature, Value),
            KA = lists:member(keyAgreement, Value),
            case {DS, KA} of
                {true, true} ->
                    {valid, UserState};
                _ ->
                    Msg =
                    "Client certificate KeyUsage failed" ++
                    " KU_DIGITAL_SIGNATURE: " ++ atom_to_list(DS)++
                    ", KU_KEY_AGREEMENT: " ++ atom_to_list(KA),
                    info_msg(Msg, []),
                    certLib:sec_log("", Msg),
                    {fail, UserState}
            end
    end.

check_key_ext_usage(_Extension,UserState,{vcpeer,_}) ->
    {valid,UserState};

check_key_ext_usage(Extension, UserState, {server, _}) ->
    case {Extension#'Extension'.extnValue, Extension#'Extension'.critical} of
        {undefined, true} ->
            info_msg("extKeyUsage critical missing~n", []),
            {fail, UserState};
        {undefined, false} ->
            {unknown, UserState};
        {List, _} ->
            case lists:member(?'id-kp-serverAuth', List) of
                true ->
                    {valid, UserState};
                false ->
                    Msg =
                    "Server certifcate extKeyUsage failed, serverAuth missing",
                    info_msg(Msg, []),
                    certLib:sec_log("", Msg),
                    {fail, UserState}
            end
    end;
check_key_ext_usage(Extension, UserState, {client, _}) ->
    case {Extension#'Extension'.extnValue, Extension#'Extension'.critical} of
        {undefined, true} ->
            info_msg("extKeyUsage critical missing~n", []),
            {fail, UserState};
        {undefined, false} ->
            {unknown, UserState};
        {List, _} ->
            % Client cert
            %% Check id-kp-clientAuth if set for
            %% SSL Client cert, else reject
            case lists:member(?'id-kp-clientAuth', List) of
                true ->
                    {valid, UserState};
                false ->
                    Msg =
                    "Client certifcate extKeyUsage failed, clientAuth missing",
                    info_msg(Msg, []),
                    certLib:sec_log("", Msg),
                    {fail, UserState}
            end
    end.



extract_user(_,_,{_, undefined}, UserState, crl_ok) ->
    %%info_msg("extract_user 1", []),
    {valid, UserState};
extract_user(OtpCert,_,{_, undefined}, _, CrlStatus) ->
    get_cert_info(OtpCert),
    %%info_msg("extract_user 2, CrlStatus: ~p", [CrlStatus]),
    {fail, CrlStatus};
extract_user(_, valid, _, UserState, crl_ok) ->
    %%info_msg("extract_user 4", []),
    {valid, UserState};
extract_user(OtpCert, valid, _, UserState, CrlStatus) ->
    get_cert_info(OtpCert),
    info_msg("extract_user 5, CrlStatus: ~p", [CrlStatus]),
    NewUserState =
    case UserState#verify_state.crl_status of
        undefined ->
            UserState#verify_state{crl_status = CrlStatus};
        _ ->
            UserState %% Keep old crl_status
    end,
    {valid, NewUserState};
extract_user(OtpCert, valid_peer, {_, Instance_pid}, UserState, CrlStatus) ->
    %%info_msg("extract_user 6, CrlStatus: ~p", [CrlStatus]),
    {User, Subject} = decode_extension_user(OtpCert),
    SubjectNameString = certLib:format_rdn(Subject),
    info_msg("~nSubjectNameString: ~p, User:  ~p~n",
        [SubjectNameString, User]),
    StatusOfCrl =
    case UserState#verify_state.crl_status of
        undefined ->
            case CrlStatus of
                crl_ok ->
                    crl_ok;
                CrlReason ->
                    get_cert_info(OtpCert),
                    CrlReason
            end;
        crl_ok ->
            crl_ok;
        CrlReason ->
            get_cert_info(OtpCert),
            CrlReason
    end,
    Instance_pid ! {user_from_cert,
        {ok, User, SubjectNameString, StatusOfCrl}},
    {valid, UserState#verify_state{crl_status = StatusOfCrl}}.

get_user(DerCert) ->
    OtpCert = public_key:pkix_decode_cert(DerCert, otp),
    decode_extension_user(OtpCert).

decode_extension_user(OtpCert) ->
    #'OTPCertificate'{tbsCertificate = TBSC}       = OtpCert,
    #'OTPTBSCertificate'{subject     = Subject}    = TBSC,
    #'OTPTBSCertificate'{extensions  = Extensions} = TBSC,
    {rdnSequence, Atv_list} = Subject,   
    case catch find_alt_user(Extensions) of
        {'EXIT', Reason} ->
            info_msg("EXIT REASON:~n ~w~n", [Reason]),
            {"", Subject};
        undefined ->
            {find_user(Atv_list), Subject};
        AltUser ->
            {AltUser, Subject}
    end.


get_cert_info(OtpCert) ->
    #'OTPCertificate'{tbsCertificate = C} = OtpCert,
    #'OTPTBSCertificate'{subject      = Subject} = C,
    #'OTPTBSCertificate'{serialNumber = SerialNr} = C,
    SubjectName = certLib:format_rdn(Subject),
    Msg = "Certificate CRL failed -> SubjectName:" ++ SubjectName ++
    ", SerialNr:" ++ integer_to_list(SerialNr),
    info_msg(Msg, []),
    certLib:sec_log("", Msg).


find_user([]) -> [];
find_user([[#'AttributeTypeAndValue'{
                type = ?'id-at-commonName', value = V}] | _]) ->
    case V of
        {utf8String, Binary_user} ->
            binary_to_list(Binary_user);
        {printableString, User} ->
            User
    end;
find_user([_ | Rest]) ->
    find_user(Rest).

find_alt_user(asn1_NOVALUE) ->
    undefined;
find_alt_user([]) ->
    undefined;
find_alt_user([#'Extension'{extnID = ?'id-ce-subjectAltName',
            extnValue = V}|_]) ->
    search_attr(V);
find_alt_user([_|T]) ->
    find_alt_user(T).

search_attr([]) ->
    undefined;
search_attr([{directoryName, {rdnSequence, AttrList}}|T]) ->
    case search_attr_list(AttrList) of
        undefined ->
            search_attr(T);
        User ->
            User
    end;
search_attr([_|T]) ->
    search_attr(T).

search_attr_list([]) ->
    undefined;
search_attr_list([[#'AttributeTypeAndValue'{type = ?'id-at-commonName',
                value = Value}]|_]) ->
    case Value of
        {utf8String, Binary_user} ->
            binary_to_list(Binary_user);
        {printableString, User} ->
            User
    end;
search_attr_list([_|T]) ->
    search_attr_list(T).


get_sub_string(OtpCert) ->
    TBS  = OtpCert#'OTPCertificate'.tbsCertificate,
    {rdnSequence, Sub} = TBS#'OTPTBSCertificate'.subject,
    certLib:format_rdn({rdnSequence, lists:sort(Sub)}).




%%% ----------------------------------------------------------
%%% -type verify_fingerprint(Cert, Fingerprint)->        %#
%%%     match | no_match.                         %#
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
verify_fingerprint(_, undefined) ->
    no_fingerprint;
verify_fingerprint(Cert, Fingerprint) ->
    Hash = certLib:get_fingerprint_support(),
    GivenFingerprint = certLib:decode_fingerprint(Fingerprint),
    ThisFingerprint  = crypto:hash(Hash, Cert),

    case ThisFingerprint of
        GivenFingerprint ->
            match;
        _ ->
            info_msg("GivenFingerprint: ~p~nThisfingerprint: ~p~n",
                [GivenFingerprint, ThisFingerprint]),
            {no_match, certLib:encode_fingerprint(ThisFingerprint)}
    end.


info_msg(Format, Args) ->
   sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).



