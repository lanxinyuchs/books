%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certLib.erl %
%%% @author etxasta
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/2
%%% 
%%% @doc ==Library module for CERT==
%%% This module contains library functions for CERT
%%% This is somewhat ugly, since we're reusing an UNOFFICIAL
%%% module from OTP/public-key - but because it relies on the
%%% same ASN.1 specification it's bound to have the functions
%%% specified herein. But use a test case to verify.

-module(certLib).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/2').
-date('2017-12-06').
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
%%% Rev      Date       Name     What
%%% -----    ---------- -------- ------------------------
%%% R2A/1    2013-08-08 etxjotj  Created
%%% R4A/4    2015-08-20 etxpejn  Added rpc:call for SecurityLog
%%% R4A/8    2015-09-25 etxpejn  Moved rpc:call to logI:write_log
%%% R6A/1    2016-06-16 ehsake   Added der_to_pem from certSeci
%%% R6A/3    2016-07-25 etomist  HV12899
%%% R7A/1    2016-09-27 etxasta  Added encode_w_vc & decode_w_vc
%%% R7A/4    2016-09-29 ehsake   Fixes for DU2DU authentication
%%% R7A/6    2016-10-19 ekurnik  HV33549 - OOT not avilable at startup
%%% R7A/7    2016-11-02 etxasta  Fix in format_general_name
%%% R8A/1    2016-11-16 etxasta  Updated decode_moref
%%%                              Added tmp_dir/0, tmp_cert_dir/0
%%% R8A/2    2016-12-05 etxasta  Updated decode_moref
%%% R9A/1    2017-01-30 eivmiha  http_uri:parse switched to ftpI:parse_uri
%%% R9A/2    2017-02-15 etxasta  Exported info_msg/3
%%% R10A/1   2017-05-23 ebabmat	 HV90156
%%% R11A/1   2017-10-10 etxasta  Added get_inet/1, get_socket_opts/1
%%% R12A/1   2017-11-22 etxasta  Updated get_socket_opts/1 for rvnfm
%%% R12A/2   2017-12-04 etxasta  Added resolve_uri_inet/1 for CMPv2
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([
        cert_dir/0,
        crl_dir/0,
        vc_dir/0,
        tc_dir/0,
        tc_dir/1,
        nc_dir/0,
        nc_dir/1,
        ex_dir/0,
        ex_dir/1,
        tmp_dir/0,
        tmp_cert_dir/0,
        read_cert_metadata/1,
        encode_fingerprint/1,
        decode_fingerprint/1,
        get_fingerprint_support/0,
        decode_moref/1,
        convert_cert_date/1,
        cmd/1,
        format_rdn/1,
        sec_log/2,
        resolve_uri/1,
        resolve_uri_inet/1,
        get_net_me_id/0,
        get_inet/0,
        get_inet/1,
        get_socket_opts/2,
        der_to_pem/2,
        pem_to_der_list/1,
        encode_w_vc/2,
        decode_w_vc/2,
        info_msg/3
    ]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("RcsCertM.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% @doc Returns the path to the CERT directory
%%% @end
%%% ----------------------------------------------------------

cert_dir() ->
    filename:join(sysEnv:rcs_dir(), "cert").

%%% ----------------------------------------------------------
%%% @doc Returns the path to the CERT CRL directory
%%% @end
%%% ----------------------------------------------------------

crl_dir() ->
    filename:join(cert_dir(), "crl").

%%% ----------------------------------------------------------
%%% @doc Returns the path to the Ericsson vendor credential directory
%%% @end
%%% ----------------------------------------------------------

vc_dir() ->
    filename:join(cert_dir(), "vc").

%%% ----------------------------------------------------------
%%% @doc Returns the path to the trusted certificates directory
%%% @end
%%% ----------------------------------------------------------

tc_dir() ->
    filename:join(cert_dir(), "tc").

%%% ----------------------------------------------------------
%%% @doc Returns the path to a specific trusted certificate directory
%%% @end
%%% ----------------------------------------------------------

tc_dir({_, _, _, _, Index}) ->
    filename:join(tc_dir(), Index).

%%% ----------------------------------------------------------
%%% @doc Returns the path to the node credentials directory
%%% @end
%%% ----------------------------------------------------------

nc_dir() ->
    filename:join(cert_dir(), "nc").

%%% ----------------------------------------------------------
%%% @doc Returns the path to a specific node credential directory
%%% @end
%%% ----------------------------------------------------------

nc_dir({_, _, _, _, Index}) ->
    filename:join(nc_dir(), Index);
nc_dir(Index) ->
    filename:join(nc_dir(), Index).

%%% ----------------------------------------------------------
%%% @doc Returns the path to the external usage directory
%%% @end
%%% ----------------------------------------------------------

ex_dir() ->
    filename:join(cert_dir(), "ex").

%%% ----------------------------------------------------------
%%% @doc Returns the path to a specific external usage directory
%%% @end
%%% ----------------------------------------------------------

ex_dir({_, _, _, _, Index}) ->
    filename:join(ex_dir(), Index);
ex_dir(Index) ->
    filename:join(ex_dir(), Index).

%%% ----------------------------------------------------------
%%% @doc Returns the path to the temp directory
%%% @end
%%% ----------------------------------------------------------
tmp_dir() ->
    filename:join(sysEnv:rcs_root(), "tmp").

%%% ----------------------------------------------------------
%%% @doc Returns the path to the temp cert directory
%%% @end
%%% ----------------------------------------------------------
tmp_cert_dir() ->
    filename:join(tmp_dir(), "cert").

%%% ----------------------------------------------------------
%%% @doc Read certificate metadata into a #'CertificateContent'
%%% Returns metadata according to the CertM CertificateContent struct
%%% Parameters:
%%%   Path:string() - Path to a ceritificate file
%%%   Bin:binary() - Contents of a ceritificate file
%%% @end
%%% ----------------------------------------------------------

read_cert_metadata(Path) when is_list(Path)->
    {ok, Bin} = file:read_file(Path),
    [{'Certificate',Cert,_}|_] = public_key:pem_decode(Bin),
    read_cert_metadata(Cert);
 
read_cert_metadata(CertBin) ->
    Cert = public_key:pkix_decode_cert(CertBin, otp),

    TBS = Cert#'OTPCertificate'.tbsCertificate,
    
    Version = format_version(TBS#'OTPTBSCertificate'.version),
    Signature = TBS#'OTPTBSCertificate'.signature,
    SignatureAlgorithm = 
	format_algorithm_identifier(Signature),
    
    SerialNumber = format_serial_number(TBS#'OTPTBSCertificate'.serialNumber),

    Issuer = format_rdn(TBS#'OTPTBSCertificate'.issuer),
    ValidFrom = format_date((TBS#'OTPTBSCertificate'.validity)#'Validity'.notBefore),
    ValidTo = format_date((TBS#'OTPTBSCertificate'.validity)#'Validity'.notAfter),
    Subject = format_rdn(TBS#'OTPTBSCertificate'.subject),
    SPKI = TBS#'OTPTBSCertificate'.subjectPublicKeyInfo,
    Algorithm = SPKI#'OTPSubjectPublicKeyInfo'.algorithm,
    PublicKeyAlgorithm = format_algorithm_identifier(Algorithm),
    PublicKey = format_public_key(SPKI),
    
    KeyUsage = 
	case [format_extension(Extension)||
		 Extension<-TBS#'OTPTBSCertificate'.extensions,
		 Extension#'Extension'.extnID == ?'id-ce-keyUsage'] of
	    [KU1] -> KU1;
	    [] -> undefined
	end,

    Extensions = 
	lists:foldl(
	  fun(Extension, Acc) 
		when Extension#'Extension'.extnID == ?'id-ce-keyUsage'->
		  Acc;
	     (Extension, Acc) ->
		  Acc ++format_extension(Extension)
	  end, [], TBS#'OTPTBSCertificate'.extensions),

    #'CertificateContent'
    	{version=Version,
    	 serialNumber=SerialNumber,
    	 signatureAlgorithm=SignatureAlgorithm,
    	 issuer=Issuer,
    	 validFrom=ValidFrom,
    	 validTo=ValidTo,
    	 publicKey=PublicKey,
    	 publicKeyAlgorithm=PublicKeyAlgorithm,
    	 keyUsage=KeyUsage,
    	 extensionContent=Extensions,
    	 subject = Subject}.

 
%%% ----------------------------------------------------------
%%% #           encode_finger_print()
%%% Input: Binary of octets
%%% Output: Fingerprint - a string of hexadecimal characters
%%% Exceptions: 
%%% Description: Encodes to a string of hexadecimal characters a binary
%%%              of octes
%%% ----------------------------------------------------------
encode_fingerprint(Bin) when is_binary(Bin) ->
    encode_fingerprint(binary_to_list(Bin));
encode_fingerprint(Bytes) when is_list(Bytes) ->
    Encoded = [encode_hex(Byte)||Byte<-Bytes],
    lists:foldl(fun(X,A) -> A++":"++X end, hd(Encoded), tl(Encoded)).


%%% ----------------------------------------------------------
%%% #           get_fingerprint_support()
%%% Input: -
%%% Output: atom() - supported fingerprint algorithm
%%% Exceptions: 
%%% Description: The function returns the supported
%%%              fingerprint algorithm.
%%% ----------------------------------------------------------
get_fingerprint_support() ->
    [Obj] = mnesia:dirty_read(certMCapabilities, {"1","1","1","1","1"}),
    case Obj#certMCapabilities.fingerprintSupport of
        ?FingerprintSupport_SHA_1 ->
            sha;
        ?FingerprintSupport_SHA_224 ->
            sha224;
        ?FingerprintSupport_SHA_256 ->
            sha256;
        ?FingerprintSupport_SHA_384 ->
            sha384;
        ?FingerprintSupport_SHA_512 ->
            sha512
    end.


%%% ----------------------------------------------------------
%%% #           decode_moref(MoRef - string())
%%% Input: 
%%% Output: {Type, Key}
%%% Exceptions: 
%%% Description: Decode the MoRef to a Node Credential or
%%%              Trusted Category key and type - atom(nc|tc)
%%% ----------------------------------------------------------
decode_moref("at_least_something") ->
    vc;
decode_moref(Atom) when is_atom(Atom) ->
    Atom;
decode_moref(MoRef) when is_binary(MoRef) ->
    decode_moref(binary_to_list(MoRef));
decode_moref(MoRef) when is_list(MoRef) ->
    decode_moref(string:tokens(MoRef, "=,"), []);
decode_moref(_) ->
    error.


decode_moref([], _) ->
    error;
decode_moref(["TrustCategory", Index], List) ->
    {tcat, list_to_tuple(List ++ [Index])};
decode_moref(["TrustedCertificate", Index], List) ->
    {tc, list_to_tuple(List ++ [Index])};
decode_moref(["NodeCredential", Index], List) ->
    {nc, list_to_tuple(List ++ [Index])};
decode_moref(["EnrollmentServerGroup", Index], List) ->
    list_to_tuple(List ++ [Index]);
decode_moref(["EnrollmentAuthority", Index], List) ->
    list_to_tuple(List ++ [Index]);
decode_moref([_,Index|T], List) ->
    decode_moref(T, List ++ [Index]).

%%% ----------------------------------------------------------
%%% #           cmd(Cmd)
%%% Input: A Linux command string()
%%% Output: standard out put from Linux
%%% Exceptions: 
%%% Description: Executes the command sting in a Linux prompt
%%%              and return the printout from sandard output.
%%% ----------------------------------------------------------
cmd(Cmd) ->
    Res = os:cmd(Cmd),
    Res.

%%% ----------------------------------------------------------
%%% #           encode_w_vc(PeerVcChain, Blob)
%%% Input:      PeerVcPem - Peer side Vendor Certificate incl. chain
%%%             Data - Binary data to encode
%%% Output:     Base64 Encoded blob
%%% Exceptions: 
%%% Description: 
%%%              
%%% ----------------------------------------------------------
encode_w_vc(PeerVcChain, Data) ->
    case catch do_encode_w_vc(PeerVcChain, Data) of
        {ok, Result} ->
            {ok,Result};
        {'EXIT', Error} ->
            info_msg("Encode of data failed, ~w", [Error]),
            {error, Error}
    end.

do_encode_w_vc(PeerVcChain, Data) ->
    %% Encrypt data with the peer vendor public key
    PeerCertDer            = hd(certLib:pem_to_der_list(PeerVcChain)),
    OTPCert                = public_key:pkix_decode_cert(PeerCertDer, otp),
    TBS                    = OTPCert#'OTPCertificate'.tbsCertificate,
    SPKI                   = TBS#'OTPTBSCertificate'.subjectPublicKeyInfo,
    PubKey                 = SPKI#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
    %% Note: public_key:encrypt_private do not support ec, just rsa :-(
    %% Current not problem, but might be later on
    EncryptedData = public_key:encrypt_public(Data, PubKey),
    %% Sign encrypted data with the node vendor private key
    {ok, _, VKey}          = certSecStore:get_vc(),
    [{Format, VcKeyDer,_}] = public_key:pem_decode(VKey),
    PrivKey                = certKey:format_priv_key({Format, VcKeyDer}),
    Sign                   = public_key:sign(EncryptedData, 'sha256', PrivKey),
    {ok, base64:encode(term_to_binary({EncryptedData, Sign}))}.


%%% ----------------------------------------------------------
%%% #           decode_w_vc(PeerVcPem, Blob)
%%% Input:      PeerVcPem - Peer side Vendor Certificate incl. chain
%%%             Blob - Base64 encoded blob to decode
%%% Output:     Decoded binary data
%%% Exceptions: 
%%% Description: 
%%%              
%%% ----------------------------------------------------------
decode_w_vc(PeerVcChain, Blob64) ->
    case catch do_decode_w_vc1(PeerVcChain, Blob64) of
        {ok, Result} ->
            {ok,Result};
        {_, Error} ->
            info_msg("Decode of data failed, ~w", [Error]),
            {error, Error}
    end.

do_decode_w_vc1(PeerVcChain, Blob64) ->
    %% Verify sign using peer vendor public key
    Blob              = base64:decode(Blob64),
    {EncData, Sign}   = binary_to_term(Blob),
    PeerCertDer       = hd(certLib:pem_to_der_list(PeerVcChain)),
    OTPCert           = public_key:pkix_decode_cert(PeerCertDer, otp),
    TBS               = OTPCert#'OTPCertificate'.tbsCertificate,
    SPKI              = TBS#'OTPTBSCertificate'.subjectPublicKeyInfo,
    PubKey            = SPKI#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
    Verify            = public_key:verify(EncData, 'sha256', Sign, PubKey),
    do_decode_w_vc2(Verify, EncData).

do_decode_w_vc2(true, EncData) ->
    {ok, _, VKey}          = certSecStore:get_vc(),
    [{Format, VcKeyDer,_}] = public_key:pem_decode(VKey),
    PrivKey                = certKey:format_priv_key({Format, VcKeyDer}),
    {ok, public_key:decrypt_private(EncData, PrivKey)};
do_decode_w_vc2(_, _) ->
    {error, failed}.


%%% ----------------------------------------------------------
%%% #           pem_to_der_list(PemBin)
%%% Input:      PemBin - Convert a PEM binary to a list of DER.
%%% Output:     DER list  
%%% Exceptions: 
%%% Description: 
%%%              
%%% ----------------------------------------------------------
pem_to_der_list(PemBin) ->
    lists:filtermap(
        fun({'Certificate', Der,_}) ->
                {true, Der};
            (_) ->
                false
        end, public_key:pem_decode(PemBin)).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

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
%%% #---------------------------------------------------------

%%% A general strategy for all format_X functions is that they should
%%% always return a printable string according to the CertificateContent
%%% format

%%% Input: V:atom() - 'v3' is the only supported version

format_version(V) ->
    atom_to_list(V).

%%% Input: I:integer() - We print large integers as hexadecimal octet strings
format_serial_number(I) ->
    Bin = binary:encode_unsigned(I),
    encode_hex(Bin).

%%% Input: {rdnSequence, Sequence:[#'AttributeTypeAndValue']}
%%% Output: "Field1=Value1,Field2=Value2" - Comma separated field value pairs

format_rdn({rdnSequence, Sequence}) ->
    lists:foldl(fun(S, A) ->
                A++[$,|format_rnd_sequence(S)]
        end, format_rnd_sequence(hd(Sequence)), tl(Sequence)).

format_rnd_sequence([TypeAndValue]) ->
    format_attribute_type(TypeAndValue#'AttributeTypeAndValue'.type)++
	[$=|format_attribute_value(TypeAndValue)].

%%% Description: These translations aren't exactly standardized, but the ones
%%%              are the most commonplace

format_attribute_type(Type) ->
    case Type of
	?'id-at-pseudonym' -> "psuedonym";
	?'id-at-serialNumber'-> "serialNumber";
	?'id-at-countryName'-> "C";
	?'id-at-dnQualifier'-> "dnQualifier";
	?'id-at-title'-> "title";
	?'id-at-organizationalUnitName'-> "OU";
	?'id-at-organizationName'-> "O";
	?'id-at-stateOrProvinceName'-> "ST";
	?'id-at-localityName'-> "L";
	?'id-at-commonName'-> "CN";
	?'id-at-generationQualifier'-> "generationQualifier";
	?'id-at-initials'-> "initials";
	?'id-at-givenName'-> "GN";
	?'id-at-surname'-> "SN";
	?'id-at-name'-> "name";
	?'id-domainComponent' -> "domainComponent";
	?'id-emailAddress' -> "emailAddress"
    end.

%%% Input: TypeAndValue:#'AttributeTypeAndValue'
format_attribute_value(TypeAndValue) ->
    case do_format_attribute_value(TypeAndValue#'AttributeTypeAndValue'.value) of
	{ok, Result} -> Result;
	{decode_needed, Binary} -> 
	    %% This was not pre decoded

	    Type = 
		case TypeAndValue#'AttributeTypeAndValue'.type of
		    ?'id-at-generationQualifier'-> 'X520name';
		    ?'id-at-initials'-> 'X520name';
		    ?'id-at-givenName'-> 'X520name';
		    ?'id-at-surname'-> 'X520name';
		    ?'id-at-name'-> 'X520name';
		    ?'id-at-commonName'-> 'X520CommonName';
		    ?'id-at-localityName'-> 'X520LocalityName';
		    ?'id-at-stateOrProvinceName'-> 'X520StateOrProvinceName';
		    ?'id-at-organizationName'-> 'X520OrganizationName';
		    ?'id-at-organizationalUnitName'-> 
			'X520OrganizationalUnitName';
		    ?'id-at-title'-> 'X520Title';
		    ?'id-at-dnQualifier'-> 'X520dnQualifier';
		    ?'id-at-serialNumber'-> 'X520SerialNumber';
		    ?'id-at-countryName'-> 'X520countryName';
		    ?'id-at-pseudonym' -> 'X520Pseudonym';
		    ?'id-domainComponent' -> 'DomainComponent';
		    ?'id-emailAddress' -> 'EmailAddress'
		end,
	    case do_format_attribute_value(
		   'OTP-PUB-KEY':decode(Type, Binary)) of
		{ok, R} -> R;
		{decode_needed, B} ->
		    erlang:error({decode_needed_after_decode, B}, [TypeAndValue])
	    end
    end.

%%% Input: {ok, Value} | {utf8String, binary()} | string() | binary()
%%% Description: If it's a plain binary than it's not pre decoded and it needs
%%%              decoding. The input {ok, _} is a sign it's already been decoded

do_format_attribute_value({ok, Value}) ->
    do_format_attribute_value(Value);
do_format_attribute_value(Value) ->
    case Value of
    	{utf8String, Bin} ->
	    {ok, binary_to_list(Bin)};
        {printableString, String} ->
            {ok, String};
        {teletexString, String} ->
            {ok, String};
	String when is_list(String) ->
	    {ok, String};
	Binary when is_binary(Binary) -> 
	    {decode_needed, Binary}
    end.
%%% Input: {utcTime, "YYMMDDHHMMSSZ"}
%%% Description: We assume this format is always the same

format_date({utcTime, [Y,Y2,M,M2,D,D2,H,H2,Mi,Mi2,S,S2,_Z]}) ->
    "20"++[Y,Y2,$-,M,M2,$-,D,D2,$T,H,H2,$:,Mi,Mi2,$:,S,S2]++"+00:00";
format_date({generalTime, Time}) ->
    Time.

%%% Input: AI:#'PublicKeyAlgorithm'
format_algorithm_identifier(AI) when is_record(AI, 'PublicKeyAlgorithm')->
    atom_to_list(ai(AI#'PublicKeyAlgorithm'.algorithm));
%%% Input: AI:#'SignatureAlgorithm'
format_algorithm_identifier(AI) when is_record(AI, 'SignatureAlgorithm')->
    atom_to_list(ai(AI#'SignatureAlgorithm'.algorithm)).


%%% Input: Oid:tuple()
%%% Description: Translate between the tuple encoded oids and some readable atom
ai(Oid) when is_tuple(Oid) ->
    case Oid of
	?'sha224WithRSAEncryption' -> sha224WithRSAEncryption;
	?'sha512WithRSAEncryption' -> sha512WithRSAEncryption;
	?'sha384WithRSAEncryption' -> sha384WithRSAEncryption;
	?'sha256WithRSAEncryption' -> sha256WithRSAEncryption;
	?'sha1WithRSAEncryption'   -> sha1WithRSAEncryption;
	?'md5WithRSAEncryption'    -> md5WithRSAEncryption;
	?'md2WithRSAEncryption'    -> md2WithRSAEncryption;
	?'id-RSASSA-PSS'   -> 'RSASSA-PSS';
	?'id-pSpecified'   -> pSpecified;
	?'id-RSAES-OAEP'   -> 'RSAES-OAEP';
	?'rsaEncryption'   -> rsaEncryption;
        ?'ecdsa-with-SHA1' -> ecdsaWithSHA1;
        ?'ecdsa-with-SHA224' -> ecdsaWithSHA224;
        ?'ecdsa-with-SHA256' -> ecdsaWithSHA256;
        ?'ecdsa-with-SHA384' -> ecdsaWithSHA384;
        ?'ecdsa-with-SHA512' -> ecdsaWithSHA512;
        ?'id-ecPublicKey'  -> ecPublicKey
    end.

%%% Input: KeyInfo:#'SubjectPublicKeyInfo'
%%% Description:
%%% This way of displaying the public key is not optimal when it comes
%%% machine-machine interaction, however that is what the model supports
%%% We also only support rsaEncryption

format_public_key(KeyInfo) ->
    PublicKeyAlgorithm = KeyInfo#'OTPSubjectPublicKeyInfo'.algorithm,
    case ai(PublicKeyAlgorithm#'PublicKeyAlgorithm'.algorithm) of
	rsaEncryption ->
	    RSAPublicKey = KeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
            Modulus = RSAPublicKey#'RSAPublicKey'.modulus,
	    ModulusConv =
            encode_hex(binary:encode_unsigned(Modulus)),
	    Exponent = RSAPublicKey#'RSAPublicKey'.publicExponent,
	    Fmt =
            io_lib:format("Modulus: ~s Exponent: ~w",[ModulusConv, Exponent]),
	    lists:flatten(Fmt);
        ecPublicKey ->
            %Bin = KeyInfo#'SubjectPublicKeyInfo'.subjectPublicKey,
	    %{ok, EC} = 'OTP-PUB-KEY':decode('ECPoint', Bin),
            %io:format("EC: ~w~n", [EC]),
            %case binary_to_term(Bin) of
            %    {namedCurve, Oid} ->
            %        io:format("namedCurve: ~w~n", [Oid]),
            %        ok;
            %    {ECPPoint, EcpkParameters} ->
            %        io:format("ECPPoint: ~w~n, EcpkParameters: ~w~n",
            %            [ECPPoint, EcpkParameters]),
            %        ok
            %end,
            %% FIXME
            "Eliptic Curve";
	X -> throw({unsupported_public_key_algorithm, X})
    end.


%%% Input: Extension:#'Extension'
%%% Description: 
%%% The problem with extensions is that they in themselves contain various
%%% levels of that, which is all supposed to be translated to the ECIM CertM
%%% format of Field:Value string pairs.
%%% In order to do that, while still maintaining some order, subfield names
%%% are added with a period (.) sign. For example
%%% AuthorityKeyIdentifier.keyIdenfifier
%%% AuthorityKeyIdentifier.autorityCertIssuer
%%% CRLDistributionPoints.distributionPoint.fullname
%%% CRLDistributionPoints.distributionPoint.reasons
%%% CRLDistributionPoints.distributionPoint.cRLIssuer
%%% Critical fields are marked with a (*)
%%%
%%% Each extension type will get it's own format_extension/2 function, because
%%% each extension has a different layout in the CertificateContent format.


format_extension(E) ->
    case extnID(E#'Extension'.extnID) of
        undefined ->
            [];
        %'ExtKeyUsageSyntax' ->
            %% FIXME
        %    [];
    
        ExtnId ->

            %{ok, Decoded} = 
            %'OTP-PUB-KEY':decode(ExtnId, E#'Extension'.extnValue),
            %format_extension(ExtnId, E#'Extension'{extnValue=Decoded})
            format_extension(ExtnId, E)
    end.

format_extension('KeyUsage', E) ->
    %% This is special, because keyUsage has been singled out among
    %% the extensions to fill in a dedicated struct member field. It will not
    %% be handled the same as the others.
    lists:foldl(fun(X,A) ->
			A++[$,, $\s|atom_to_list(X)]
		end,atom_to_list(hd(E#'Extension'.extnValue)),
		tl(E#'Extension'.extnValue));
format_extension('ExtKeyUsage', E) ->
    %% This is special, because extKeyUsage has been singled out among
    %% the extensions to fill in a dedicated struct member field. It will not
    %% be handled the same as the others.
    [lists:foldl(fun(X,A) ->
                A ++ ", " ++ format_ext_usage(X) 
        end, "keyExtUsage: "++format_ext_usage(hd(E#'Extension'.extnValue)),
        tl(E#'Extension'.extnValue))];
%%% All other clauses should return a list of strings
format_extension('AuthorityKeyIdentifier', E) ->
    Field = format_field(E),
    Subfields = record_info(fields, 'AuthorityKeyIdentifier'),
    Record =
    format_authority_cert_issuer(E#'Extension'.extnValue),
    format_record(Field, Subfields, Record);
format_extension('SubjectKeyIdentifier', E) ->
    Field = format_field(E),
    Value = E#'Extension'.extnValue,
    [Field++": "++format_value(keyIdentifier, Value)];
format_extension('CRLDistributionPoints', E) ->
    Field = format_field(E),
    CRLDs = E#'Extension'.extnValue, % one or more distribution points
    lists:foldl(
      fun(CRLD, Acc) ->
	      [format_crld(Field, CRLD)|Acc]
      end, [], lists:reverse(CRLDs));
format_extension('SubjectAltName', E) ->
    Field = format_field(E),
    GeneralNames = E#'Extension'.extnValue,
    [format_general_name(Field, GeneralName)||GeneralName<-GeneralNames];
format_extension('BasicConstraints', E) ->
    Field = format_field(E),
    Value = E#'Extension'.extnValue,
    #'BasicConstraints'{cA = CA,
			pathLenConstraint = PathLenConstraint} = Value,
    [Field++".CA: "++atom_to_list(CA)|
     case PathLenConstraint of
	 asn1_NOVALUE -> [];
	 _ -> 
	     [Field++".PathLenConstraint: "++integer_to_list(PathLenConstraint)]
     end];
	    
format_extension(_T, E) ->
    %% FIXME show it really by throw here????
    %throw({unknown, T,E}),
    [format_field(E)].


format_ext_usage(?'id-kp-OCSPSigning') ->
    "Signing OCSP Responses";
format_ext_usage(?'id-kp-timeStamping') ->
    "Time Stamping";
format_ext_usage(?'id-kp-emailProtection') ->
    "Email Protection";
format_ext_usage(?'id-kp-codeSigning') ->
    "Signing Of Downloadable Executable Code";
format_ext_usage(?'id-kp-clientAuth') ->
    "TLS Web Client Authentication";
format_ext_usage(?'id-kp-serverAuth') ->
    "TLS Web Server Authentication".


format_field(E) ->
    atom_to_list(extnID(E#'Extension'.extnID))++
	if E#'Extension'.critical ->
		"*";
	   true ->
		""
	end.

format_record(Field, Subfields, Record) when is_tuple(Record) ->
    Values = tl(tuple_to_list(Record)),
    Zipped = lists:zip(Subfields, Values),
    [Field++"."++atom_to_list(Subfield)++": "++format_value(Subfield, Value)||
	{Subfield,Value}<-Zipped,
	Value /= asn1_NOVALUE].
    
format_value(keyIdentifier, Value) when is_list(Value) ->
    encode_hex(list_to_binary(Value));
format_value(keyIdentifier, Value) when is_binary(Value) ->
    encode_hex(Value);
format_value(_, Value) ->
    Value.


format_crld(Field,DistributionPoint) ->
    case DistributionPoint#'DistributionPoint'.distributionPoint of
	{fullName, GeneralNames} ->
	    [format_general_name(Field++".fullname", GeneralName)||
		GeneralName<-GeneralNames];
	{nameRelativeToCRLIssuer, RDN} ->
	    [Field++".nameRelativeToCRLIssuer: "++
		 format_rdn({rdnSequence, [RDN]})];
	asn1_NOVALUE ->
	    []
    end++
	case DistributionPoint#'DistributionPoint'.reasons of
	    asn1_NOVALUE ->
		[];
	    Reasons ->
		[Field++".reasons: "++
		     lists:foldl(fun(X,A) ->
					 A++[$,, $\s|atom_to_list(X)]
				 end,atom_to_list(hd(Reasons)),
				 tl(Reasons))]
	end++
	case DistributionPoint#'DistributionPoint'.cRLIssuer of
	    asn1_NOVALUE ->
		[];
	    GeneralNames ->
		[format_general_name(Field++".cRLIssuer", GeneralName)||
		    GeneralName<-GeneralNames]
	end.

	

format_general_name(Field, GeneralName) ->
    case GeneralName of
		{otherName, Name} -> Field++".otherName: "++printable(Name);
		{rfc822Name, Name} -> Field++".rfc822Name: "++Name; %IA5String
		{dNSName, Name} -> Field++".dNSName: "++Name; % IA5String
		{x400address, Name} -> Field++".x400address: "++printable(Name); 
		{directoryName, Name} -> Field++".directoryName: "++format_rdn(Name);
		{ediPartyName, Name} -> Field++".ediPartyName: "++printable(Name);
		{uniformResourceIdentifier, Name} -> Field++".uri: "++Name; %IA5String
	    {iPAddress, Name} when is_binary(Name)->
	        format_general_name(Field, {iPAddress, binary_to_list(Name)});
	    {iPaddress, Name} ->
	        format_general_name(Field, {iPAddress, Name});
	    {iPAddress, Name} ->
		    case length(Name) of
				4 ->
		        	Field++".ip: " ++ string:join([integer_to_list(X) || X <- Name], ".");
				16 ->
		            Field++".ip: " ++ encode_ipv6_hex(Name);
		        _ ->
		            Field++".ip: " ++ Name
		    end;
		{registeredId, Name} -> 
			Field ++ ".registeredId: " ++ string:join([integer_to_list(X) || X <- Name], ".")
    end.


%% FIXME
format_authority_cert_issuer(Record) ->
    case Record#'AuthorityKeyIdentifier'.authorityCertIssuer of
        asn1_NOVALUE ->
            Record;
        _ ->
            Record#'AuthorityKeyIdentifier'{authorityCertIssuer = asn1_NOVALUE,
                authorityCertSerialNumber = asn1_NOVALUE}
    end.

%% It ain't nice, but it's printable
printable(Term) ->
    lists:flatten(io_lib:format("~p",[Term])).

extnID(?'id-ce-invalidityDate') -> 'InvalidityDate';
extnID(?'id-ce-holdInstructionCode') -> 'HoldInstructionCode';
extnID(?'id-ce-certificateIssuer') -> 'CertificateIssuer';
extnID(?'id-ce-cRLReasons') -> 'CRLReasons';
extnID(?'id-ce-deltaCRLIndicator') -> 'DeltaCRLIndicator';
extnID(?'id-ce-issuingDistributionPoint') -> 'IssuingDistributionPoint';
extnID(?'id-ce-cRLNumber') -> 'CRLNumber';
extnID(?'id-ce-freshestCRL') -> 'FreshestCRL';
extnID(?'id-ce-inhibitAnyPolicy') -> 'InhibitAnyPolicy';
extnID(?'id-ce-cRLDistributionPoints') -> 'CRLDistributionPoints';
extnID(?'id-ce-policyConstraints') -> 'PolicyConstraints';
extnID(?'id-ce-nameConstraints') -> 'NameConstraints';
extnID(?'id-ce-basicConstraints') -> 'BasicConstraints';
extnID(?'id-ce-subjectDirectoryAttributes') -> 'SubjectDirectoryAttributes';
extnID(?'id-ce-issuerAltName') -> 'IssuerAltName';
extnID(?'id-ce-subjectAltName') -> 'SubjectAltName';
extnID(?'id-ce-policyMappings') -> 'PolicyMappings';
extnID(?'id-ce-certificatePolicies') -> 'CertificatePolicies';
extnID(?'id-ce-privateKeyUsagePeriod') -> 'PrivateKeyUsagePeriod';
extnID(?'id-ce-keyUsage') -> 'KeyUsage';
extnID(?'id-ce-extKeyUsage') -> 'ExtKeyUsage';
extnID(?'id-ce-subjectKeyIdentifier') -> 'SubjectKeyIdentifier';
extnID(?'id-ce-authorityKeyIdentifier') -> 'AuthorityKeyIdentifier';
extnID(_) -> undefined.

    
%%% ----------------------------------------------------------
%%% #           decode_finger_print()
%%% Input: Fingerprint - a string of hexadecimal characters
%%% Output: Binary of octets
%%% Exceptions: 
%%% Description: Decodes a string of hexadecimal characters into a binary
%%%              of octes
%%% ----------------------------------------------------------
decode_fingerprint(Fingerprint) ->
     list_to_binary([decode_hex(H)*16+decode_hex(L)||
 		       [H,L]<-string:tokens(Fingerprint, ":")]).

decode_hex(C) when C >= $a, C =< $f ->
     C-$a+10;
decode_hex(C) when C >= $A, C =< $F ->
     C-$A+10;
decode_hex(C) when C >= $0, C =< $9 ->
     C-$0.

%%% ----------------------------------------------------------
%%% #           encode_hex(Bin)
%%% Input: Binary of octets
%%% Output: A string of comma separated double digit hexadecimal numbers
%%% Exceptions: 
%%% Description: Encodes to a string of hexadecimal characters a binary
%%%              of octes. Use letters a-f and not A-F
%%% ----------------------------------------------------------

encode_hex(Bin) when is_binary(Bin) ->
    encode_hex(binary_to_list(Bin));
encode_hex(Bytes) when is_list(Bytes) ->
    Encoded = [encode_hex(Byte)||Byte<-Bytes],
    lists:foldl(fun(X,A) -> A++":"++X end, hd(Encoded), tl(Encoded));
encode_hex(Byte) when is_integer(Byte) ->
    H = Byte div 16,
    L = Byte rem 16,
    [do_encode_hex(H),
     do_encode_hex(L)].

encode_ipv6_hex(Bytes) ->
    Encoded = [encode_hex(Byte)||Byte<-Bytes],
    concat_ipv6(Encoded, []).

concat_ipv6([X,Y|T], Acc) -> 
    concat_ipv6(T, [X ++ Y|Acc]);
concat_ipv6([], Acc) -> 
    string:join(lists:reverse(Acc), ":").

do_encode_hex(N) when N >= 10 ->
    N-10+$a;
do_encode_hex(N) ->
    N+$0.

%%% ----------------------------------------------------------
%%% #   convert_cert_date(String)   
%%% Input:  -
%%% Output: ok
%%% Exceptions: 
%%% Description: Convert the NotValidBefore or NotValidAfter
%%%              string from the cert
%%% ----------------------------------------------------------
convert_cert_date({utcTime, String}) ->
    convert_cert_date(String);
convert_cert_date({generalTime, String}) ->
    convert_cert_date(String);
convert_cert_date(String) ->
    case length(String) of
        13 ->
            YearNA  = list_to_integer("20" ++ string:substr(String, 1, 2)),
            MonNA   = list_to_integer(string:substr(String, 3, 2)),
            DayNA   = list_to_integer(string:substr(String, 5, 2)),
            HourNA  = list_to_integer(string:substr(String, 7, 2)),
            MinNA   = list_to_integer(string:substr(String, 9, 2)),
            SecNA   = list_to_integer(string:substr(String, 11, 2)),
            {{YearNA,MonNA,DayNA},{HourNA,MinNA,SecNA}};
        15 ->
            YearNA  = list_to_integer(string:substr(String, 1, 4)),
            MonNA   = list_to_integer(string:substr(String, 5, 2)),
            DayNA   = list_to_integer(string:substr(String, 7, 2)),
            HourNA  = list_to_integer(string:substr(String, 9, 2)),
            MinNA   = list_to_integer(string:substr(String, 11, 2)),
            SecNA   = list_to_integer(string:substr(String, 13, 2)),
            {{YearNA,MonNA,DayNA},{HourNA,MinNA,SecNA}}
    end.


%%% ----------------------------------------------------------
%%% #           resolve_uri(Uri)
%%% Input: Uri::string() 
%%% Output: {ok, NewUri} | {error, Reason}
%%% Exceptions:
%%% Description: Resolve uri, change domain name to ip address
%%% ----------------------------------------------------------
resolve_uri(Uri) ->
    case ftpI:parse_uri(Uri) of
        {ok, {_Proto, _User, [], _Port, _Path, _Query}} ->
            {error, empty_list};
        {ok, {Proto, User, Host, Port, Path, Query}} ->
            %% HV33549 - OOT not avilable at startup
            try ootI:getaddr(Host) of
                {ok, IpTuple} ->
                    {ok,
                        format_protocol(Proto) ++ format_user(User) ++
                        format_ip(IpTuple) ++ format_port(Port) ++
                        Path ++ Query};
                Reason ->
                    {error, Reason}
            catch
                exit:{noproc, _} ->
                    {error, oot_not_started}
            end
    end.

format_protocol(Protocol) ->
    atom_to_list(Protocol) ++ "://".

format_user([]) ->
    "";
format_user(User) ->
    User ++ "@".

format_ip(IpTuple) when tuple_size(IpTuple) == 4 -> % IPv4
    inet:ntoa(IpTuple);
format_ip(IpTuple) when tuple_size(IpTuple) == 8 -> % IPv6
    "[" ++ inet:ntoa(IpTuple) ++ "]". % Use brackets to handle port

format_port(Port) ->
    ":" ++ integer_to_list(Port).

%%% ----------------------------------------------------------
%%% #           resolve_uri_inet(Uri)
%%% Input: Uri::string() 
%%% Output: {ok, NewUri, Inet} | {error, Reason}
%%% Exceptions:
%%% Description: Resolve uri, change domain name to ip address
%%%              and get the inet.
%%% ----------------------------------------------------------
resolve_uri_inet(Uri) ->
    case ftpI:parse_uri(Uri) of
        {ok, {_Proto, _User, [], _Port, _Path, _Query}} ->
            {error, empty_list};
        {ok, {Proto, User, Host, Port, Path, Query}} ->
            %% HV33549 - OOT not avilable at startup
            try ootI:getaddr(Host) of
                {ok, IpTuple} ->
                    {Ip, Inet} = format_ip_inet(IpTuple),
                    {ok,
                        format_protocol(Proto) ++ format_user(User) ++
                        Ip ++ format_port(Port) ++
                        Path ++ Query, Inet};
                Reason ->
                    {error, Reason}
            catch
                exit:{noproc, _} ->
                    {error, oot_not_started}
            end
    end.

format_ip_inet(IpTuple) when tuple_size(IpTuple) == 4 -> % IPv4
    {inet:ntoa(IpTuple), inet};
format_ip_inet(IpTuple) when tuple_size(IpTuple) == 8 -> % IPv6
    {"[" ++ inet:ntoa(IpTuple) ++ "]", inet6}. % Use brackets to handle port


%%% ----------------------------------------------------------
%%% # der_to_pem(DerList,Type)
%%% Input: DerList List of binaries 
%%%      : Type - 'Certificate', key type
%%% Output: List of PemEntries
%%% Exceptions:
%%% Description: Converts certificate/key from DER to PEM
%%% ---------------------------------------------------------
der_to_pem(DerList, Type) ->
    der_to_pem(DerList, Type, []).

der_to_pem([], _Type, PemEntries) ->
    public_key:pem_encode(PemEntries);
der_to_pem([Der|T], Type, PemEntries) ->
    {Format, Entity} =
    case certKey:format_priv_key({Type, Der}) of
        Data when is_record(Data, 'RSAPrivateKey') -> %% RSA
            {'RSAPrivateKey', Data};
        Data when is_record(Data, 'ECPrivateKey') -> %% EC
            {'ECPrivateKey', Data};
        Data ->
            {Type, Data}
    end,
    PemEntry = public_key:pem_entry_encode(Format, Entity),
    der_to_pem(T, Type, PemEntries ++ [PemEntry]).


%%% ----------------------------------------------------------
%%% #           sec_log(SrcIp, Msg)
%%% Input: SrcIp::string() - User source IP address 
%%%      : Msg::string()
%%% Output:
%%% Exceptions:
%%% Description: Send a security event to the log block
%%% ----------------------------------------------------------
sec_log(_, Msg) ->
    List = comsaI:get_managed_element_data(),
    MeId =
    case lists:keyfind(networkManagedElementId, 1, List) of
        {networkManagedElementId, undefined} ->
            "1";
        {networkManagedElementId, ME} ->
            ME
    end,
    logI:write_log("SecurityLog", MeId, 4, info, os:timestamp(), Msg).


%%% ----------------------------------------------------------
%%% #           get_net_me_id() ->
%%% Input: -
%%% Output: MeId:string() - NetworkManagedElementId.
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
get_net_me_id() ->
    List = comsaI:get_managed_element_data(),
    case lists:keyfind(networkManagedElementId, 1, List) of
        {networkManagedElementId, undefined} ->
            "1";
        {networkManagedElementId, ME} ->
            ME
    end.



%%% ----------------------------------------------------------
%%% #           get_inet(Interface) ->
%%% input: Interface - atom(oam|infra)
%%% output: inet | inet6
%%% exceptions:
%%% description:
%%% ----------------------------------------------------------
get_inet(oam) ->
    %% Valid on all nodes for oam
    get_inet();
get_inet(infra) ->
    %% The infra interface is only existing on R-VNFM
    %% TODO
    get_inet().


%%% ----------------------------------------------------------
%%% #           get_inet() ->
%%% input: -
%%% output: inet | inet6
%%% exceptions:
%%% description: This function is only for OAM
%%% ----------------------------------------------------------
get_inet() ->
    do_get_inet(ootI:get_oap_ip_addr()).

do_get_inet("") ->
    inet;
do_get_inet(IP_string) ->
    case inet:parse_address(IP_string) of
        {ok, Tuple} when size(Tuple) =:= 8 ->
            inet6;
        {ok, Tuple} when size(Tuple) =:= 4 ->
            inet;
        _ ->
            inet
    end.


%%% ----------------------------------------------------------
%%% #           get_socket_opts(Interface, Inet) ->
%%% Input: Interface - atom(oam|infra)
%%%        Inet      - atom(inet|inet6)
%%% Output: 
%%% Exceptions:
%%% Description: Get socket option for oam or infra interface
%%% ----------------------------------------------------------
get_socket_opts(oam, _Inet) ->
    %% Valid for all nodes for oam
    ootI:get_all_oam_opt_list();
get_socket_opts(infra, Inet) ->
    %% Only valid for R-VNFM, since no one else have an infra interface
    apply(secI, get_socket_opts, [infra, Inet]).


%%% ----------------------------------------------------------
%%% INFO, WARNING and ERROR MESSAGE HANDLING
%%% ----------------------------------------------------------
info_msg(Format, Args) ->
    info_msg(?MODULE, Format, Args).

info_msg(Module, Format, Args) ->
   sysInitI:info_msg("~w: "++Format++"~n", [Module|Args]).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

