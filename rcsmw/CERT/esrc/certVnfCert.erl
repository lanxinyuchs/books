%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certVnfCert.erl %
%%% @author etxasta
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/R10A/R11A/R12A/1
%%% 
%%% @doc ==Handle certificate distrubution of certificates.==
%%% Will be used for VRCS and BPU
-module(certVnfCert).
-behaviour(gen_server).
-vsn('/main/R9A/R10A/R11A/R12A/1').
-date('2017-12-04').
-author('etxasta').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev     Date       Name     What
%%% -----   ---------- -------  ------------------------------
%%% R9A/1   2017-03-28 etxasta  Created
%%% R9A/2   2017-04-06 etxasta  Converted to gen_server
%%% R10A/3  2017-05-06 etxasta  Added R-VNFM support
%%% R10A/4  2017-05-16 etxasta  Little bug
%%% R10A/5  2017-05-22 etxasta  New OAM MOM support
%%% R11A/1  2017-10-10 etxasta  SP680
%%% R11A/2  2017-10-19 etxasta  "OAM" -> "oam","INFRA" -> "infra"
%%% R12A/1  2017-11-30 etxasta  Updated get_cert with CrlCheck value
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%%% Called from supervisor
-export([start/0]).


-export([get_cert/0]).

-export([get_cert/1,
         start_cmpv2_enroll/2,
         stop_cmpv2_enroll/1,
         create_node_cred/2,
         install_p12/3,
         print_node_cred_conf/1]).

-include_lib("public_key.hrl").
-include("RcsCertM.hrl").
-include("cert.hrl").

-define(SHARED, [229,23,242,121,107,96,160,211,79,240,66,
        40,176,139,225,173,101,160,158,192]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% @doc Starts the certVnfCert server process
%%% @end
%%% ----------------------------------------------------------
start() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

%%% ----------------------------------------------------------
%%% #          get_cert()
%%% Input:  -
%%% Output: {ok, CertWithChain, Key, TrustList, Type} |
%%%         {error, string(Reason)} 
%%% Exceptions: 
%%% Description: Fetch the internal certificate use for
%%%              communication between VNFM and VNFCs.
%%%              First time it is used it is fetch from
%%%              a pkcs12 container added by VNFM.
%%%              Type is real(from vnfm) or
%%%              dummy(always the same, only for lab test)
%%% ----------------------------------------------------------
get_cert() ->
    gen_server:call(?MODULE, get_cert, 10000).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
init(_Args) ->
    {ok, up}.

handle_call(get_cert, _From, State) ->
    {reply, handle_get_cert(), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

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
%%% #          handle_get_cert()
%%% Input:  -
%%% Output: {ok, CertWithChain, Key, TrustList, Type} |
%%%         {error, string(Reason)} 
%%% Exceptions: 
%%% Description: Fetch the internal certificate use for
%%%              communication between VNFM and VNFCs.
%%%              First time it is used it is fetch from
%%%              a pkcs12 container added by VNFM.
%%%              Type is real(from vnfm) or
%%%              dummy(always the same, only for lab test)
%%% ----------------------------------------------------------
handle_get_cert() ->
    do_get_cert1(certSecStore:read("internal", "1")).


do_get_cert1({ok, Bin}) ->
    %% Found internal certificate
    {Cert, Key, TrustList, Type} = binary_to_term(Bin),
    {ok, Cert, Key, TrustList, Type};
do_get_cert1({error, not_found}) ->
    %% Unpacked internal cert Missing, check if pkcs12 file exist
    do_get_cert2(read_int_p12_files(get_vnfc_id()));
do_get_cert1({error, Reason}) -> % Some error reading
    {error, Reason}.

do_get_cert2({{ok, P12Bin}, {ok, CredBin}, Info}) ->
    %% Found pkcs12 file
    do_get_cert3(certCrypto:decrypt_shared(?SHARED, P12Bin),
        certCrypto:decrypt_shared(?SHARED, CredBin), Info);
do_get_cert2({error, Info}) ->
    info_msg("Internal credentials missing, ~p", [Info]),
    {error, "Internal credentials missing"}. 

do_get_cert3({ok, P12BinData}, {ok, CredBinData}, Info) ->
    Res = certPkcs12:unpack_offline_data(P12BinData,
        binary_to_list(CredBinData)),
    do_get_cert4(Res, Info);
do_get_cert3({error, Reason},_, {PfxFile, _SecFile, Type}) ->
    info_msg("Decode of ~p failed, ~p", [PfxFile, Type]),
    {error, Reason};
do_get_cert3(_,{error, Reason}, {_PfxFile, SecFile, Type}) ->
    info_msg("Decode of ~p failed, ~p", [SecFile, Type]),
    {error, Reason}.

do_get_cert4({pkcs12, CertWithChain, Key, TrustList},
    {PfxFile, SecFile, Type}) ->
    %% NOTE the rootCA is not in the chain, it is added to the trustlist
    DataBin = term_to_binary({CertWithChain, Key, TrustList, Type}),
    certSecStore:write("internal", "1", DataBin),
    %% Create the internal certificate files
    certDist:update_cert_dist(internal),
    file:delete(PfxFile),
    file:delete(SecFile),
    {ok, CertWithChain, Key, TrustList, Type};
do_get_cert4(Error,_) ->
    Error.


read_int_p12_files(false) ->
    info_msg("Only standalone and using dummy internal credentials", []),
    DummyPath = certLib:tmp_cert_dir(),
    DummyPfxFile  = DummyPath ++ "/vnfc.pfx",
    DummySecFile  = DummyPath ++ "/vnfc.sec",
    case {file:read_file(DummyPfxFile),file:read_file(DummySecFile)} of
        {{ok, P12Bin}, {ok, CredBin}} ->
            {{ok, P12Bin}, {ok, CredBin}, {DummyPfxFile, DummySecFile, dummy}};
        _ ->
            info_msg("VNFC didn't found dummy internal credentials", []),
            {error, not_found}
    end;
read_int_p12_files(VnfcId) ->
    RealPath = "/shared/" ++ VnfcId,
    RealPfxFile  = RealPath ++ "/vnfc.pfx",
    RealSecFile  = RealPath ++ "/vnfc.sec",
    case {file:read_file(RealPfxFile), file:read_file(RealSecFile)} of
        {{ok, P12Bin}, {ok, CredBin}} ->
            {{ok, P12Bin}, {ok, CredBin}, {RealPfxFile, RealSecFile, real}};
        _ ->
            %% Keep trying, might be delayed from vnfm.
            info_msg("No int certficates, might be delayed", []),
            %% FIXME this is due to a bug in vnfm, should not go for dummy here
            read_int_p12_files(false)
            %%{error, not_found}
    end.


get_vnfc_id() ->
    os:getenv("VNFC_ID").


%%% ----------------------------------------------------------
%%% @doc
%%% Function: get_cert(Index)
%%% Input:  Index - "oam" | "infra" | "vnf_mgmt"
%%% Output: {ok, CertDer, {KeyType, KeyDer}, TrustDerList} |
%%%         {error, string(Reason)}
%%% Exceptions: -
%%% Description: Returns node credential cert, key and trusted
%%%              certificates.
%%% @end
%%% ----------------------------------------------------------
get_cert("vnf_mgmt") ->
    case get_cert() of
        {ok, [Cert|Chain], Key, TrustList, _Type} ->
            %% Found internal certificate
            %% CrlCheck hardcoded off
            {ok, Cert, Key, TrustList ++ Chain, ?FeatureState_DEACTIVATED};
        {error, Reason} -> % Some error reading
            {error, Reason}
    end;
get_cert("infra") ->
    NcDN =
    "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=infra",
    TcatDN =
    "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,TrustCategory=infra",
    do_get_cert(NcDN, TcatDN);
get_cert("oam") ->
    OamNcDN =
    "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1,NodeCredential=oam",
    OamTcatDN =
    "Managedelement=1,SystemFunctions=1,secM=1,CertM=1,TrustCategory=oam",
    case omc_https:get_cert_conf() of
        {undefined, undefined} ->
            do_get_cert(OamNcDN, OamTcatDN);
        {undefined, _} ->
            {error, "Missing HttpM Node Credential config"};
        {_, undefined} ->
            {error, "Missing HttpM Trust Category config"};
        {NcDnBin, TcatDnBin} ->
            NcDn   = binary_to_list(NcDnBin),
            TcatDn = binary_to_list(TcatDnBin),
            certI:unsubscribe(OamNcDN, secServer),
            certI:unsubscribe(OamTcatDN, secServer),
            certI:subscribe(NcDn, secServer),
            certI:subscribe(TcatDn, secServer),
            do_get_cert(NcDn, TcatDn)
    end.

do_get_cert(NcMoRef, TcatMoRef) ->
    do_get_cert1(certI:get_cert(NcMoRef), TcatMoRef).

do_get_cert1({error, Reason}, _TcatMoRef) ->
    {error, Reason};
do_get_cert1({ok, Certs, Key}, TcatMoRef) ->
    do_get_cert2({ok, Certs, Key}, certI:get_tcat_and_crlcheck(TcatMoRef)).

do_get_cert2(_, {error, Reason}) ->
    {error, Reason};
do_get_cert2({ok, [Cert|Chain], Key}, {ok, TcList, CrlCheck}) ->
    {ok, Cert, Key, TcList ++ Chain, CrlCheck}.


%%% ----------------------------------------------------------
%%% @doc
%%% Function: start_cmpv2_enroll(Network, Challenge)
%%% Input:  Index string("oam"|"infra")
%%%         Challange - string()
%%% Output: ok
%%% Exceptions: -
%%% Description: StartReturns node credential cert, key and trusted
%%%              certificates.
%%% @end
%%% ----------------------------------------------------------
start_cmpv2_enroll("infra", Challenge) ->
    MoRef = {"1","1","1","1","infra"},
    certNcServer:start_online_enrollment(MoRef, Challenge, infra);
start_cmpv2_enroll("oam", Challenge) ->
    MoRef = {"1","1","1","1","oam"},
    certNcServer:start_online_enrollment(MoRef, Challenge, oam).


%%% ----------------------------------------------------------
%%% @doc
%%% Function: stop_cmpv2_enroll(Index)
%%% Input:  Index - string("oam"|"infra")
%%% Output: ok
%%% Exceptions: -
%%% Description: Returns node credential cert, key and trusted
%%%              certificates.
%%% @end
%%% ----------------------------------------------------------
stop_cmpv2_enroll(Index) ->
    certNcServer:cancel_enrollment({"1","1","1","1",Index}).

%%% ----------------------------------------------------------
%%% @doc Create node credential MO
%%%      Index: Name of NC, enm or infra
%%%      Opts:  maps()
%%%             user_label  => string(UserLabel)
%%%             usage       => atom(external|internal))
%%%             csr_sn      => string(SubjectName)
%%%             csr_san     => string(SubjectAltName)
%%%             csr_ki      => atom(KeyInfo)
%%%             cmp_et      => integer(Enrollment Timer)
%%%             cmp_rm      => atom(Renewal Mode)
%%%             eat         => interger(expiryAlarmThreshold)
%%%             crl_check   => atom(true|false)
%%%             cmp_servers => string(CMPv2 enrollment servers)
%%%             cmp_ca_name => string(CMPv2 signing CA DN)
%%%             cmp_ca_fp   => string(CMPv2 server message signer, CA or RA)
%%% @end
%%% ----------------------------------------------------------
create_node_cred(Index, Opts) when is_list(Index) ->
    Fun = fun() ->
            %% Enrollment Authority
            EaId = {"1", "1", "1", "1", Index},
            create_ea(EaId, Opts),
            %% Enrollment Server Group
            EsgId = {"1", "1", "1", "1", Index},
            create_esg(EsgId, EaId, Opts),
            %% Node Credential
            NcId = {"1", "1", "1", "1", Index},
            create_nc(NcId, EaId, EsgId, Opts),
            %% Trusted Category
            TcId = {"1", "1", "1", "1", Index},
            create_tcat(TcId, Opts)
    end,
    mnesia:transaction(Fun),
    ok.

create_esg(Id, EaId, Opts) ->
    do_create_esg(mnesia:read(enrollmentServerGroup, Id), Id, EaId, Opts).

do_create_esg([], {"1", "1", "1", "1", Index} = Id, EaId, Opts) ->
    case maps:get(cmp_servers, Opts, "") of
        [] -> %% Not CMPv2, skip this
            ok;
        Urls ->
            Obj =
            #enrollmentServerGroup{
                enrollmentServerGroupId = Id,
                userLabel = maps:get(user_label, Opts,
                    "Used for " ++ Index ++ "REST API")},
            mnesia:write(Obj),
            create_es(Urls, {"1", "1", "1", "1", Index, "1"}, EaId, Opts)
    end;
do_create_esg(_, Id,_,_) ->
    info_msg("Enrollment Server Group already exist, ~p", [Id]),
    ok.

create_es([], _, _, _) ->
    ok;
create_es([Url|T], {"1", "1", "1", "1", Index, EsIndex} = Id, EaId, Opts) ->
    Obj =
    #enrollmentServer{
        enrollmentServerId = Id,
        userLabel = maps:get(user_label, Opts,
            "Used for " ++ Index ++ "," ++ EsIndex ++ "REST API"),
        enrollmentAuthority = mk_moref("EnrollmentAuthority", EaId),
        uri = Url,
        protocol = ?EnrollmentProtocol_CMP},
    mnesia:write(Obj),
    NextEsIndex = integer_to_list(list_to_integer(EsIndex) + 1),
    create_es(T, {"1", "1", "1", "1", Index, NextEsIndex}, EaId, Opts).


create_ea(Id, Opts) ->
    do_create_ea(mnesia:read(enrollmentAuthority, Id), Id, Opts).

do_create_ea([], {"1", "1", "1", "1", Index} = Id, Opts) ->
    case maps:get(cmp_ca_name, Opts, undefined) of
        undefined -> %% Not CMPv2, skip this
            ok;
        EAName ->
            Obj =
            #enrollmentAuthority{
                enrollmentAuthorityId = Id,
                enrollmentCaFingerprint = maps:get(cmp_ca_fp, Opts),
                userLabel = maps:get(user_label, Opts,
                    "Used for " ++ Index ++ "REST API"),
                enrollmentAuthorityName = EAName},
            mnesia:write(Obj)
    end;
do_create_ea(_, Id, _) ->
    info_msg("Enrollment Authority already exist, ~p", [Id]),
    ok.

 
create_nc(Id, EaId, EsgId, Opts) ->
    do_create_nc(mnesia:read(nodeCredential, Id), Id, EaId, EsgId, Opts).

do_create_nc([], {"1", "1", "1", "1", Index} = Id, EaId, EsgId, Opts) ->
    EsgMoRef =
    case maps:get(cmp_servers, Opts, "") of
        [] -> %% Not CMPv2, skip this
            undefined;
        _ ->
            mk_moref("EnrollmentServerGroup", EsgId)
    end,
    EaMoRef =
    case maps:get(cmp_ca_name, Opts, undefined) of
        undefined -> %% Not CMPv2, skip this
            undefined;
        _ ->
            mk_moref("EnrollmentAuthority", EaId)
    end,            
    Obj =
    #nodeCredential{
        nodeCredentialId = Id,
        userLabel =
        maps:get(user_label, Opts, "Used for " ++ Index ++ "REST API"),
        subjectName = maps:get(csr_sn, Opts, ""),
        enrollmentTimer =
        maps:get(cmp_et, Opts, ?nodeCredential_enrollmentTimer_default),
        enrollmentServerGroup = EsgMoRef,
        keyInfo = maps:get(csr_ki, Opts, ?KeyInfo_RSA_2048),
        renewalMode =
        maps:get(cmp_rm, Opts, ?RenewalMode_MANUAL),
        enrollmentAuthority = EaMoRef,
        expiryAlarmThreshold =
        maps:get(eat, Opts, ?nodeCredential_expiryAlarmThreshold_default),
        subjectAltName = maps:get(csr_san, Opts, ""),
        certificateState = ?CertificateState_NOT_VALID_YET},
    mnesia:write(Obj);
do_create_nc(_, Id, _, _, _) ->
    info_msg("Node Credential already exist, ~p", [Id]),
    ok.

create_tcat(Id, Opts) ->
    do_create_tcat(mnesia:read(trustCategory, Id), Id, Opts).

do_create_tcat([], {"1", "1", "1", "1", Index} = Id, Opts) ->
    Obj =
    #trustCategory{
        trustCategoryId = Id,
        trustedCertificates = [],
        userLabel =
        maps:get(user_label, Opts, "Used for " ++ Index ++ "REST API"),
        crlCheck = maps:get(crl_check, Opts, ?FeatureState_DEACTIVATED)},
    mnesia:write(Obj);
do_create_tcat(_, Id, _) ->
    info_msg("Trusted Category already exist, ~p", [Id]),
    ok.


mk_moref(Mo, {"1", "1", "1", "1", Index}) ->
    "ManagedElement=1,SystemFunctions=1,SecM=1,CertM=1," ++
    Mo ++ "=" ++ Index. 


%%% ----------------------------------------------------------
%%% @doc
%%% Function: install_p12(Index, P12, Pwd)
%%% Input:  Index - string("oam"|"infra")
%%% Output: {ok, CertDer, {KeyType, KeyDer}, TrustDerList} |
%%%         {error, string(Reason)}
%%% Exceptions: -
%%% Description: 
%%% @end
%%% ----------------------------------------------------------
install_p12(Index, P12, Pwd) ->
    Key = {"1", "1", "1", "1", Index},
    info_msg("Install node credential(~p) from p12~n", [Key]),
    case certPkcs12:unpack_offline_data(P12, Pwd) of
        {pkcs12, [NcCert|_] = Cert, PrivKey, TCs} ->
            Result = certVerify:verify_nc(NcCert, PrivKey),
            do_install_p12(Result, Key, Cert, PrivKey, TCs);
        _ -> % Not ok
            info_msg("No pkcs#12 container ~w~n", [Key])
    end.

do_install_p12({failed, no_match}, Key,_,_,_) ->
    certLib:sec_log("", "Preinstalled Node credential (" ++
        element(5, Key) ++ ") certificate did not match private key"),
    info_msg("Private key and certificate did not match ~w~n", [Key]);
do_install_p12({failed, invalid_date}, Key,_,_,_) ->
    certLib:sec_log("", "Preinstalled Node credential (" ++
        element(5, Key) ++ ") certificate, invalid date"),
    info_msg("Invalid certificate date ~w~n", [Key]);
do_install_p12({failed, Faulty_KeyUsage}, Key,_,_,_) ->
    certLib:sec_log("", "Preinstalled Node credential (" ++
        element(5, Key) ++ ") certificate, faulty keyUsage"),
    info_msg("Faulty KeyUsage: ~w, ~w~n", [Faulty_KeyUsage, Key]);
do_install_p12(valid, Key, [NcCert|_] = Cert, PrivKey, TCs) ->
    %% Get subjectname from cert
    OTPCert     = public_key:pkix_decode_cert(NcCert, otp),
    TBS         = OTPCert#'OTPCertificate'.tbsCertificate,
    Subject     = TBS#'OTPTBSCertificate'.subject,
    SubjectName = certLib:format_rdn(Subject),
    KeyInfo     = guess_key_info(PrivKey),
    CC          = certLib:read_cert_metadata(NcCert),
    Fun = fun() ->
            [Obj] = mnesia:read(nodeCredential, Key),
            NewObj =
            Obj#nodeCredential{subjectName          = SubjectName,
                               keyInfo              = KeyInfo,
                               certificateContent   = CC,
                               certificateState     = ?CertificateState_VALID},
            %% Store cert and privkey in SecEE
            ok = mnesia:write(NewObj),
            ok = mnesia:write(#certNC{index = Key, cert = Cert})
    end,
    case mnesia:transaction(Fun) of
        {atomic, _} ->
            case certSecStore:get_nc_key(Key, "nc.key") of
                {ok, PrivOldKey} ->
                    info_msg("Stored old nc key ~p", [Key]),
                    certSecStore:remove_nc_key(Key, "nc_old.key"),
                    certSecStore:put_nc_key(Key, PrivOldKey, "nc_old.key"),
                    certSecStore:remove_nc_key(Key, "nc.key");
                _ ->
                    ok
            end,
            certSecStore:put_nc_key(Key, term_to_binary(PrivKey), "nc.key"),
            %% Clear alarms if any
            certAlarm:clear(nc, cert_not_available, Key),
            certAlarm:clear(nc, cert_to_expire, Key),
            %% Start expire timer
            certNcServer:ext_start_to_expire_timer(Key, Cert),
            %% Install Trusted Certificate 
            certServer:pre_install(Key, TCs),
            certSub:trig(nc, Key),
            ok;
        _ ->
            {error, "Mnesia transaction failed"}
    end.
 
guess_key_info(PrivKey) ->
    case certKey:format_priv_key(PrivKey) of
        Priv when is_record(Priv, 'RSAPrivateKey') -> %% RSA
            case length(integer_to_list(Priv#'RSAPrivateKey'.modulus)) of
                Length when Length < 450 ->
                    ?KeyInfo_RSA_1024;
                Length when Length < 750 ->
                    ?KeyInfo_RSA_2048;
                Length when Length < 1050 ->
                    ?KeyInfo_RSA_3072;
                _ ->
                    ?KeyInfo_RSA_4096
            end;
        Priv when is_record(Priv, 'ECPrivateKey') -> %% EC
            case Priv#'ECPrivateKey'.parameters of
                {namedCurve, ?'brainpoolP160r1'} ->
                    ?KeyInfo_ECDSA_160;
                {namedCurve, ?'brainpoolP224r1'} ->
                    ?KeyInfo_ECDSA_224;
                %% FIXME No OTP support for brainpoolP521r1
                %{namedCurve, ?'brainpoolP521r1'} ->
                %    ?KeyInfo_ECDSA_521;
                {namedCurve, ?'brainpoolP256r1'} ->
                    ?KeyInfo_ECDSA_BRAINPOOL_256;
                {namedCurve, ?'brainpoolP320r1'} ->
                    ?KeyInfo_ECDSA_BRAINPOOL_320;
                {namedCurve, ?'brainpoolP384r1'} ->
                    ?KeyInfo_ECDSA_BRAINPOOL_384;
                {namedCurve, ?'brainpoolP512r1'} ->
                    ?KeyInfo_ECDSA_512
            end
    end.



%%% ----------------------------------------------------------
%%% @doc print_node_cred_conf("oam"|"infra") 
%%%      Index: Name of NC, enm or infra
%%% @end
%%% ----------------------------------------------------------
print_node_cred_conf(Index) ->
    %% Print Node Credential
    print_nc({"1", "1", "1", "1", Index}),
    %% Print Enrollment Authority
    print_ea({"1", "1", "1", "1", Index}),
    %% Print Enrollment Server Group
    print_esg({"1", "1", "1", "1", Index}),
    %% Enrollment Servers
    print_es({"1", "1", "1", "1", Index}),
    %% Print Trusted Category
    print_tcat({"1", "1", "1", "1", Index}),
    ok.

print_nc({"1", "1", "1", "1", Index} = Id) ->
    case mnesia:dirty_read(nodeCredential, Id) of
        [] ->
            io:format("~n### Node Credential ~p doesn't exist!~n", [Index]);
        [Nc] ->
            io:format("~n### Node Credential(~p)~n" ++
                "nodeCredentialId: ~p~nuserLabel: ~p~nsubjectName: "++
                "~p~nreservedByUser: ~p~nenrollmentTimer: ~p~n"++
                "enrollmentServerGroup: ~p~nkeyInfo: ~p~n" ++
                "enrollmentProgress: ~p~nrenewalMode: ~p~n" ++
                "enrollmentAuthority: ~p~nexpiryAlarmThreshold: ~p~n" ++
                "subjectAltName: ~p~ncertificateContent: " ++
                "~p~ncertificateState: ~p~n",
                [Index,
                 Nc#nodeCredential.nodeCredentialId,
                 Nc#nodeCredential.userLabel,
                 Nc#nodeCredential.subjectName,
                 Nc#nodeCredential.reservedByUser,
                 Nc#nodeCredential.enrollmentTimer,
                 Nc#nodeCredential.enrollmentServerGroup,
                 Nc#nodeCredential.keyInfo,
                 Nc#nodeCredential.enrollmentProgress,
                 Nc#nodeCredential.renewalMode,
                 Nc#nodeCredential.enrollmentAuthority,
                 Nc#nodeCredential.expiryAlarmThreshold,
                 Nc#nodeCredential.subjectAltName,
                 Nc#nodeCredential.certificateContent,
                 Nc#nodeCredential.certificateState])
    end,
    ok.

print_ea({"1", "1", "1", "1", Index} = Id) ->   
    case mnesia:dirty_read(enrollmentAuthority, Id) of
        [] ->
            io:format("~n### Enrollment Authority ~p doesn't exist!~n",[Index]);
        [Ea] ->
            io:format("~n### Enrollment Authority(~p)~n" ++
                "enrollmentAuthorityId: ~p~nenrollmentCaCertificate: ~p~n" ++
                "enrollmentCaFingerprint: ~p~nauthorityType: ~p~n" ++
                "userLabel: ~p~nenrollmentAuthorityName: ~p~n",
                [Index,
                 Ea#enrollmentAuthority.enrollmentAuthorityId,
                 Ea#enrollmentAuthority.enrollmentCaCertificate,
                 Ea#enrollmentAuthority.enrollmentCaFingerprint,
                 Ea#enrollmentAuthority.authorityType,
                 Ea#enrollmentAuthority.userLabel,
                 Ea#enrollmentAuthority.enrollmentAuthorityName])
    end,
    ok.
 
print_esg({"1", "1", "1", "1", Index} = Id) ->
    case mnesia:dirty_read(enrollmentServerGroup, Id) of
        [] ->
            io:format("~n### Enrollment Server Group ~p doesn't exist!~n",
                [Index]);
        [Esg] ->
            io:format("~n### Enrollment Server Group(~p)~n" ++
                "enrollmentServerGroupId: ~p~nuserLabel: ~p~n",
                [Index,
                 Esg#enrollmentServerGroup.enrollmentServerGroupId,
                 Esg#enrollmentServerGroup.userLabel])
    end,
    ok.

print_es(Id) ->
    do_print_es(mnesia:dirty_first(enrollmentServer), Id).

do_print_es('$end_of_table',_) ->
    ok;
do_print_es({A1,A2,A3,A4,A5,A6}, {A1,A2,A3,A4,A5}) ->
    [Es]   = mnesia:dirty_read(enrollmentServer, {A1,A2,A3,A4,A5,A6}),
    NextId = mnesia:dirty_next(enrollmentServer, {A1,A2,A3,A4,A5,A6}),
    io:format("~n### Enrollment Server(~p)~n" ++
        "enrollmentServerId: ~p~nuserLabel: ~p~nenrollmentAuthority: ~p~n" ++
        "uri: ~p~nprotocol: ~p~n",
        [A5,
         Es#enrollmentServer.enrollmentServerId,
         Es#enrollmentServer.userLabel,
         Es#enrollmentServer.enrollmentAuthority,
         Es#enrollmentServer.uri,
         Es#enrollmentServer.protocol]),
    do_print_es(NextId, {A1,A2,A3,A4,A5});
do_print_es({A1,A2,A3,A4,Another,A6}, Id) ->
    NextId = mnesia:dirty_next(enrollmentServer, {A1,A2,A3,A4,Another,A6}),
    do_print_es(NextId, Id).

print_tcat({"1", "1", "1", "1", Index} = Id) ->
    case mnesia:dirty_read(trustCategory, Id) of
        [] ->
            io:format("~n### Trusted Category ~p doesn't exist!~n", [Index]);
        [Tcat] ->
            io:format("~n### Trusted Category(~p)~n" ++
                "trustCategoryId: ~p~nuserLabel: ~p~n" ++
                "trustedCertificates: ~p~nreservedByUser: ~p~n" ++
                "crlCheck: ~p~n",
                [Index,
                 Tcat#trustCategory.trustCategoryId,
                 Tcat#trustCategory.userLabel,
                 Tcat#trustCategory.trustedCertificates,
                 Tcat#trustCategory.reservedByUser,
                 Tcat#trustCategory.crlCheck])
    end,
    ok.


info_msg(Format, Args) ->
    certLib:info_msg(?MODULE, Format, Args).


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

