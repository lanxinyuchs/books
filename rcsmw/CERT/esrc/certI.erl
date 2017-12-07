%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certI.erl %
%%% @author emirbos
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/3
%%% 
%%% @doc ==Certficate revocation list handling==
%%% This module implements the certificate revocation list handling
-module(certI).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/3').
-date('2017-10-03').
-author('emirbos').
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
%%% Rev     Date       Name      What
%%% -----   ---------  --------  ------------------------
%%% R2A/1   2013-11-14 etxasta   Created
%%% R6A/2   2016-06-16 ehsake    get_cert/2 added
%%% R6A/2   2016-08-17 enenteo   Updated for SNMP over DTLS feature
%%% R7A/1   2016-09-29 emariad   encrypt_data/1, decrypt_data/1 added for vrcs
%%% R8A/1   2016-11-16 etxasta   Added get_paths/1
%%% R7A/1   2016-01-17 emariad   Added func calls to certCrypto (encryption)
%%% R8A/2   2016-11-22 emariad   Added is_encryption_enabled
%%% R8A/5   2016-11-23 etxasta   Fixed get_paths/1 for internal creds
%%% R8A/6   2016-12-05 etxasta   Updated get_paths/1
%%% R8A/8   2016-12-06 etxasta   Updated get_paths/1, check if already exist
%%% R9A/1   2017-02-03 etxasta   Updated get_paths/1
%%% R11A/1  2017-09-12 emirbos   Added get_tcat_certs_and_MoRefs/1
%%% R11A/2  2017-09-25 emariad   Added test_license/1 for virtual nodes
%%% R11A/3  2017-10-03 emirbos   get_tcat_certs_and_MoRefs/1 changed
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([generate_fingerprint/1,
	 get_cert/1,
         get_cert/2,
	 get_tcat_and_crlcheck/1,
     get_tcat_certs_and_MoRefs/1,
         get_user/1,
	 mk_verify_fun/1,
	 mk_verify_fun/2,
	 verify_peer/4,
	 subscribe/2,
	 unsubscribe/2,
	 unsubscribe/1,
         encrypt_data/1,
         decrypt_data/1,
         get_paths/1,
         is_encryption_enabled/0,
         get_cert_dist_dir/0,
         update_cert_dist/1,
         get_vnf_cert/0,
	 test_license/1]).

%% Test function
-export([cert_event/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%-export([internal_function1/2]).
%%%-export([internal_function2/3]).

%-include("RcsCertM.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc
%%% Function: get_user(PeerCert)
%%% Input: PeerCert - Peer certificate in DER format
%%% Output: string(User) 
%%% Exceptions: -
%%% Description:  Get username from peer certificate.
%%% @end
%%% ----------------------------------------------------------
get_user(PeerCert) ->
    certVerify:get_user(PeerCert).

%%% ----------------------------------------------------------
%%% @doc
%%% Function: get_cert(MoRef)
%%% Input: MoRef - string(NcDn|TrustCategoryDn)
%%% Output: {{ok, [NodeCert|Chain...], PrivKey} | {ok, [TrustCategory|...]} |
%%%         {error, Reason}}
%%% Exceptions: -
%%% Description:  Get trusted certificate by trust category or get
%%% node credential in DER format.
%%% 
%%% Ex. of MoRef "ManagedElement=1,SystemFunctions=1,
%%%               SecM=1,CertM=1,TrustCategory=certm_SUITE" 
%%% @end
%%% ----------------------------------------------------------
get_cert(MoRef) ->
    get_cert(MoRef,der).

%%% ----------------------------------------------------------
%%% @doc
%%% Function: get_cert(MoRef,Format)
%%% Input: MoRef - string(NcDn|TrustCategoryDn)
%%%      : Format :: atom() | { der, pem } - Format of returned certificates
%%% Output: {{ok, [NodeCert|Chain...], PrivKey} | {ok, [TrustCategory|...]} |
%%%         {error, Reason}}
%%% Exceptions: -
%%% Description:  Get trusted certificate by trust category or get
%%% node credential in given format.
%%% 
%%% Ex. of MoRef "ManagedElement=1,SystemFunctions=1,
%%%               SecM=1,CertM=1,TrustCategory=certm_SUITE" 
%%% @end
%%% ----------------------------------------------------------
get_cert(MoRef,der) ->
    case certLib:decode_moref(MoRef) of
        {nc, Key} ->
            info_msg("get_cert MoRef: ~p~n", [MoRef]),
            certNcServer:get_nc_der(Key);
        {tcat, Key} ->
            info_msg("get_cert MoRef: ~p~n", [MoRef]),
            certServer:get_tcat(Key);
        vc ->
            info_msg("get_cert vc", []),
            certSecStore:get_vc();
        _ ->
            info_msg("get_cert MoRef: ~p~n", [MoRef]),
            {error, not_found}
    end;
    
get_cert(MoRef,pem) ->
    Result = get_cert(MoRef,der),
    case Result of
        {ok, CertDerList, {KeyType, DerPrivKey}} -> %% NC
           NodeCert = certLib:der_to_pem(CertDerList, 'Certificate'),
           PrivKey  = certLib:der_to_pem([DerPrivKey], KeyType),
           {ok, NodeCert,PrivKey};
        {ok, CertDerList} -> %% TC or VC
            CertList = certLib:der_to_pem(CertDerList, 'Certificate'),
            {ok,CertList};
        _ ->
            Result
            
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% Function: get_tcat_and_crlcheck(MoRef)
%%% Input: MoRef - string(TrustCategoryDn)
%%% Output: {ok, [TrustCategory|...]}
%%%         {error, Reason}}
%%% Exceptions: -
%%% Description:  Get trusted certificate by trust category
%%% 
%%% Ex. of MoRef "ManagedElement=1,SystemFunctions=1,
%%%               SecM=1,CertM=1,TrustCategory=certm_SUITE" 
%%% @end
%%% ----------------------------------------------------------
get_tcat_and_crlcheck(MoRef) ->
    info_msg("get_cert_and_crlcheck MoRef: ~p~n", [MoRef]),
    case certLib:decode_moref(MoRef) of
        {tcat, Key} ->
            certServer:get_tcat_and_crlcheck(Key);
        _ ->
            {error, not_found}
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% Function: get_tcat_certs_and_MoRefs(MoRef)
%%% Input: MoRef - string(TrustCategoryDn)
%%% Output: {ok, [{TrustedCertificate_pem, TrustedCertificateDn}|...]}
%%%         {error, Reason}}
%%% Exceptions: -
%%% Description: Get trusted certificates in pem format 
%%%              per trust categoryand its Morefs
%%% 
%%% Ex. of MoRef "ManagedElement=1,SystemFunctions=1,
%%%               SecM=1,CertM=1,TrustCategory=certm_SUITE" 
%%% @end
%%% ----------------------------------------------------------
get_tcat_certs_and_MoRefs(MoRef) ->
    info_msg("get_tcat_certs_and_MoRefs, TrustCategory MoRef: ~p~n", [MoRef]),
    case certLib:decode_moref(MoRef) of
        {tcat, Key} ->
            case certServer:get_tcat_certs_and_MoRefs(Key) of
                {ok, []} ->
                    {ok, []};
                {ok, CertsAndMoRefList} ->
                    {CertMoRefs, CertDerList} = lists:unzip(CertsAndMoRefList),
                    CertPemList = lists:map(fun(CertDerEntry) -> certLib:der_to_pem([CertDerEntry], 'Certificate') end, CertDerList),
                    Result = lists:zip(CertMoRefs, CertPemList),
                    {ok, Result};
                _ ->
                    {error, not_found}
            end;
        _ ->
            {error, not_found}
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% Function: mk_verify_fun(TrustCategoryDn)
%%% Input: TrustCategoryDn - string()
%%% Output: {ok, {verify_fun, fun()}, {parial_chain, fun()}} | {error, Reason} 
%%% Exceptions: -
%%% Description: Makes and returns a verify and partial_chain fun() used to
%%% verify the peer/server cerificates.
%%% @end
%%% ----------------------------------------------------------
mk_verify_fun(TrustCategoryDn) ->
    mk_verify_fun(TrustCategoryDn, {server, undefined}).

%%% ----------------------------------------------------------
%%% @doc
%%% Function: mk_verify_fun(TrustCategoryDn, {Type, InstancePid})
%%% Input: TrustCategoryDn - string()
%%%        Type - server | client
%%%        InstancePid - return pid() to bang the user to
%%% Output: {ok, {verify_fun, fun()}, {partial_chain, fun()}} | {error, Reason} 
%%% Exceptions: -
%%% Description: Makes and returns a verify and partial_chain fun() used to
%%% verify the peer/server cerificates.
%%% Sends userName back via the InstancePid
%%% @end
%%% ----------------------------------------------------------
mk_verify_fun(TrustCategoryDn, {Type, InstancePid}) ->
    case certLib:decode_moref(TrustCategoryDn) of
        {tcat, Key} ->
            certServer:mk_verify_fun(Key, {Type, InstancePid});
        _ ->
            {error, not_found}
    end;
mk_verify_fun(TrustCategoryDn, InstancePid) ->
    mk_verify_fun(TrustCategoryDn, {client, InstancePid}).

%%% ----------------------------------------------------------
%%% @doc
%%% Function: verify_peer(Chain, TrustCategoryDn, Type, Socket)
%%% Input: Chain - list of certificates in DER format
%%%        TrustCategoryDn - string()
%%%        Type - server | client
%%%        Socket - used for SECI | undefined
%%% Output: {ok, Result} | {error, Reason}
%%% Exceptions: - 
%%% Description: Verify peer certificate
%%% @end
%%% ----------------------------------------------------------
verify_peer(Chain, TrustCategoryDn, Type, Socket) ->
    %info_msg("verify_peer, Chain: ~p, TrustCategoryDn ~p~n",
    %    [Chain, TrustCategoryDn]),
    case certLib:decode_moref(TrustCategoryDn) of
        {tcat, Key} ->
            certVerify:verify_peer(Chain, Key, Type, Socket);
        _ ->
            {error, not_found}
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% Function: subscribe(MoRef, CbModule)
%%% Input: MoRef    - binary(NcDn|TrustCategoryDn)
%%%        CbModule - module name
%%% Output: ok
%%% Exceptions: -
%%% Description: Subscription for cetificate changes.
%%% Callback function: cert_event(MoRef, Status)
%%%              MoRef:     string(NcDn|TrustCategoryDn)
%%%              Status: atom(changed)
%%% Do not block this process jump to your own process
%%% @end
%%% ----------------------------------------------------------
subscribe(MoRef, CbModule) ->
    gen_server:cast(certServer, {certSub, subscribe, [MoRef, CbModule]}),
    ok.

%% NOTE Temporary for test
cert_event(MoRef) ->
    info_msg("cert_event, MoRef: ~p~n", [MoRef]).

%%% ----------------------------------------------------------
%%% @doc
%%% Function: unsubscribe(MoRef, CbModule)
%%% Input: MoRef:   - binary(NcDn|TrustCategoryDn)
%%%        CbModule - module name
%%% Output: ok 
%%% Exceptions: -
%%% Description: Make unsubscribe for certificate changes
%%% @end
%%% ----------------------------------------------------------
unsubscribe(MoRef, CbModule) ->
    gen_server:cast(certServer, {certSub, unsubscribe, [MoRef, CbModule]}),
    ok.

%%% ----------------------------------------------------------
%%% @doc
%%% Function: unsubscribe(CbModule)
%%% Input: CbModule - module name
%%% Output: ok 
%%% Exceptions: -
%%% Description: Make unsubscribe for certificate changes
%%% @end
%%% ----------------------------------------------------------
unsubscribe(CbModule) ->
    gen_server:cast(certServer, {certSub, unsubscribe, [CbModule]}),
    ok.

%%% ----------------------------------------------------------
%%% @doc
%%% Function: generate_fingerprint()
%%% Input:  Cert - certificate
%%% Output: Fingerprint
%%% Exceptions: -
%%% Description: Generates fingerprint from certificate
%%% @end
%%% ----------------------------------------------------------
generate_fingerprint(Cert) ->
    Hash = certLib:get_fingerprint_support(),
    FPbinary=crypto:hash(Hash, Cert),
    certLib:encode_fingerprint(FPbinary).


%%% ----------------------------------------------------------
%%% @doc
%%% Function: is_decryption_enabled()
%%% Input:  
%%% Output: true - false
%%% Exceptions: -
%%% Description: Check if encryption/decryption is enabled.
%%% @end
%%% ----------------------------------------------------------
is_encryption_enabled() ->
    case sysEnv:vrcs() of
        true ->
            true;
        false ->
            false
    end.
%%% ----------------------------------------------------------
%%% @doc
%%% Function: encrypt_data()
%%% Input:  Data - binary()
%%% Output: EncryptedData - binary()
%%% Exceptions: -
%%% Description: Encrypts the data. 
%%%              In this release the returned output is the same as input.
%%% @end
%%% ----------------------------------------------------------
encrypt_data(Data) ->
    certCrypto:encrypt_data(Data).

%%% ----------------------------------------------------------
%%% @doc
%%% Function: decrypt_data()
%%% Input:  EncryptedData -binary()
%%% Output: Data - binary()
%%% Exceptions: -
%%% Description: Decrypts the encrypted data
%%%              In this release the returned output is the same as input.
%%% @end
%%% ----------------------------------------------------------
decrypt_data(EncryptedData) ->
    certCrypto:decrypt_data(EncryptedData).

%%% ----------------------------------------------------------
%%% @doc
%%% Function: get_paths(Index)
%%% Input:  Index - {NcMoRef, TcatMoRef} | int 
%%% Output: {ok, string(CertPath), string(KeyPath), string(TrustPath)}
%%%         {error, string(Reason)}
%%% Exceptions: -
%%% Description: Returns paths one for the
%%%              node/trusted/internal/ certificate and
%%%              one for the key (observe trusted, no key).
%%%
%%%              int - Usage internally a vnf,
%%%                    e.g. between vnfcs in the same vnf
%%%              Deprecated internal and external, use int and ext
%%%              internal - Usage internally a vnf,
%%%                         e.g. between vnfcs in the same vnf
%%% @end
%%% ----------------------------------------------------------
get_paths({NcMoRef, TcMoRef}) ->
    do_get_moref_paths(certLib:decode_moref(NcMoRef),
        certLib:decode_moref(TcMoRef), NcMoRef, TcMoRef);
get_paths(Index) ->
    do_get_int_paths(Index, certVnfCert:get_cert()).

do_get_moref_paths({error, Reason1}, {error, Reason2}, NcMoRef, TcMoRef) ->
    info_msg("NC(~p) failed, reason: ~p,~nTC(~p) failed, reason: ~p",
        [NcMoRef, Reason1, TcMoRef, Reason2]),
    {error, "Not found"};
do_get_moref_paths({error, Reason}, _, NcMoRef, _) ->
    info_msg("NC(~p) failed, reason: ~p", [NcMoRef, Reason]),
    {error, "Not found"};
do_get_moref_paths(_, {error, Reason}, _, TcMoRef) ->
    info_msg("TC(~p) failed, reason: ~p", [TcMoRef, Reason]),
    {error, "Not found"};
do_get_moref_paths({nc, NcKey}, {tcat, TcKey}, NcMoRef, TcMoRef) -> % Found NC
    case certNcServer:get_nc_der(NcKey) of
        {ok, CertDerList, {KeyType, DerPrivKey}} -> %% NC
            case certServer:get_tcat_and_crlcheck(TcKey) of
                {ok, TcatList, _CrlCheck} ->
                    %% Later on if crl check should pass on CrlCheck aswell
                    NodeCert = certLib:der_to_pem(CertDerList, 'Certificate'),
                    PrivKey  = certLib:der_to_pem([DerPrivKey], KeyType),
                    TcCerts  = certLib:der_to_pem(TcatList, 'Certificate'),
                    {ok, mk_tmp_file("cert_" ++ NcMoRef ++ ".pem", NodeCert),
                        mk_tmp_file("key_" ++ NcMoRef ++ ".pem", PrivKey),
                        mk_tmp_file("tc" ++ TcMoRef ++ ".pem", TcCerts)};
                Error ->
                    info_msg("TC(~p) failed, ~p", [TcMoRef, Error]),
                    {error, "Not found"}
            end;
        Error ->
            info_msg("NC(~p) failed, ~p", [NcMoRef, Error]),
            {error, "Not found"}
    end;
do_get_moref_paths(_,_, NcMoRef, TcMoRef) ->
    info_msg("NC(~p) and TC(~p) not supported", [NcMoRef, TcMoRef]),
    {error, "Not supported"}.


do_get_int_paths(internal, {ok, [Cert, SubCa], {Format, Key},
        [_RootCa, _VnfmSubCa], _Type}) ->
    %% TODO remove this later!!!
    CertPem       = certLib:der_to_pem([Cert, SubCa], 'Certificate'),
    KeyPem        = certLib:der_to_pem([Key],  Format),
    PathCert  = mk_tmp_file("int_cert.pem", CertPem),
    PathKey   = mk_tmp_file("int_key.pem", KeyPem),
    {ok, PathCert, PathKey};
do_get_int_paths(int, {ok, [Cert, SubCa], {Format, Key},
        [_RootCa, _VnfmSubCa], _Type}) ->
    %% Exclude the RootCa, to prevent connection to other VNFs
    %% Exclude the VnfmSubCa, to prevent connection to the VNFM
    CertPem       = certLib:der_to_pem([Cert], 'Certificate'),
    KeyPem        = certLib:der_to_pem([Key],  Format),
    TrustCertsPem = certLib:der_to_pem([SubCa], 'Certificate'),
    PathCert  = mk_tmp_file("int_cert.pem", CertPem),
    PathKey   = mk_tmp_file("int_key.pem", KeyPem),
    PathTrust = mk_tmp_file("int_trust.pem", TrustCertsPem),
    {ok, PathCert, PathKey, PathTrust};
do_get_int_paths(Index, Error) ->
    info_msg("~p not supported, ~p", [Index, Error]),
    Error.


mk_tmp_file(Filename, Bin) when is_binary(Bin) ->
    Dir  = certLib:tmp_cert_dir(),
    File = filename:join(Dir, Filename),
    case file:read_file(File) of
        {ok, _} -> % Already exist
            ok;
        _  -> % Make file
            ok = filelib:ensure_dir(File),
            certLib:cmd(["chmod a+rwx ", Dir]),
            ok = file:write_file(File, Bin, [sync])
    end,
    File.


%%% ----------------------------------------------------------
%%% @doc
%%% Function: update_cert_dist({NS, NcMoRef, TcMoRef}| NS) 
%%% Input: NS      - string(NameSpace)
%%%        NcMoRef - string(NcMoRef) 
%%%        TcMoRef - string(TcMoRef)
%%% Output: ok
%%% Exceptions: 
%%% Description: Update the certificate files for chosen
%%%              NameSpace. Only NS will remove certificate
%%%              files for that NS.
%%% @end
%%% ----------------------------------------------------------
update_cert_dist({NS, NcMoRef, TcMoRef}) when is_list(NS) ->
    certDist:update_cert_dist({NS, NcMoRef, TcMoRef});
update_cert_dist(NS) when is_list(NS) ->
    certDist:update_cert_dist(NS).

%%% ----------------------------------------------------------
%%% @doc
%%% Function: get_cert_dist_dir()
%%% Input:  -
%%% Output: string(DirPath)
%%% Exceptions: -
%%% Description: Returns directory path for the distributed
%%%              certificates. Used by VNFM and VNFC
%%% @end
%%% ----------------------------------------------------------
get_cert_dist_dir() ->
    certDist:get_cert_dist_dir().

%%% ----------------------------------------------------------
%%% @doc
%%% Function: get_vnf_cert()
%%% Input:  -
%%% Output: {ok, CertWithChain, Key, TrustList, Type} |
%%%         {error, string(Reason)} 
%%% Exceptions: -
%%% Description: Returns the VNFM or VNFC local internal
%%%              credential.
%%% @end
%%% ----------------------------------------------------------
get_vnf_cert() ->
    certVnfCert:get_cert().

%%% ----------------------------------------------------------
%%% @doc
%%% Function: test_license(Loaded)
%%% Input: Loaded - boolean(true|false)
%%% Output: ok
%%%         {error, string(Reason)} 
%%% Exceptions: -
%%% Description: Only used in a virtual node. Let the vnfc know
%%%		 if a test license has been loaded or removed.
%%%		 Only for R-VNFM and vRCS.
%%%              
%%% @end
%%% ----------------------------------------------------------
test_license(_Loaded) ->
    case {sysEnv:vrcs(), swmI:node_type()} of
        {true, "R-VNFM"} ->
	    ok;
        {true, "vRC"} ->
	    ok;
	_ ->
	    ok
    end.    

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

info_msg(Format, Args) ->
   sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------


