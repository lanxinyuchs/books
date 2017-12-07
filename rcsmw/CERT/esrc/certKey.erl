%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certKey.erl %
%%% @author emajkaa
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R7A/R8A/R9A/R10A/R11A/R12A/1
%%% 
%%% @doc == This module implements the key handling ==
-module(certKey).
-vsn('/main/R2A/R3A/R4A/R5A/R7A/R8A/R9A/R10A/R11A/R12A/1').
-date('2017-11-20').
-author('emajkaa').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2014-06-23 etxasta     Created
%%% R7A/1      2016-10-24 etxasta     Added salt_hmac_key/1
%%% R8A/1      2016-10-28 etxasta     Changed salt_hmac_key/1
%%%                                   to mk_otp/4
%%% R8A/4      2016-11-15 etxasta     Corrected mk_otp/4
%%% R8A/5      2016-11-23 etxasta     Changed mk_otp/4 to mk_otp/3
%%% R12A/1     2017-11-20 emajkaa     HW45658
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([
        key_gen/1,
        format_key_info/1,
        format_priv_key/1,
        mk_otp/3
    ]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS 
%%% ----------------------------------------------------------
-include_lib("public_key.hrl").
-include("RcsCertM.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #          key_gen(KeyInfo)
%%% Input:  KeyInfo - string()
%%% Output: {PrivKey, PubKey, StorageKey}
%%% Exceptions: 
%%% Description: Let Secure EE create a new key pair and return
%%%              the public key and a storage key used to fetch
%%%              the private key.
%%% ----------------------------------------------------------
key_gen({"genrsa", Length}) ->
    case catch public_key:generate_key({rsa, Length, 65537}) of
        {'EXIT', {Reason, _}} ->
            info_msg("Generation of key pair failed with reason: ~p~n", [Reason]),
            {error, gen_key_pair_failed};
        Priv_key ->
            Pub_key = pub_key(Priv_key),
            RSADer = public_key:der_encode('RSAPrivateKey', Priv_key),
            StoreKey = term_to_binary({'RSAPrivateKey',RSADer}),
            certSecStore:put_nc_key(get(key), StoreKey, "csr.key"),
            {ok, Priv_key, Pub_key}
    end;
key_gen(EC) ->
    %info_msg("KeyType: ~p~n",[EC]),
    case catch public_key:generate_key({namedCurve, EC}) of
        ECkey when is_record(ECkey, 'ECPrivateKey') ->
            ECder = public_key:der_encode('ECPrivateKey', ECkey),
            StoreKey = term_to_binary({'ECPrivateKey', ECder}),
            certSecStore:put_nc_key(get(key), StoreKey, "csr.key"),
            {ok, ECkey};
        Error ->
            info_msg("~w: failed to make eliptic curve key ~p, Error: ~p",
                [?MODULE, EC, Error]),
            {error, gen_key_pair_failed}
           end.
 
pub_key(#'RSAPrivateKey'{modulus = Mod, publicExponent = Exp}) ->
    RSAPub_key = #'RSAPublicKey'{modulus = Mod, publicExponent = Exp},
    {SubFormat, Pub_der, _} = public_key:pem_entry_encode('SubjectPublicKeyInfo', RSAPub_key),
    Pub_key = public_key:der_decode(SubFormat, Pub_der),
    Pub_key.


%%% ----------------------------------------------------------
%%% #          format_key_info(KeyInfo)
%%% Input:  KeyInfo
%%% Output: string()
%%% Exceptions: 
%%% Description: Format the CertM keyinfo attribute
%%%              to a string matching openssl
%%% ----------------------------------------------------------
format_key_info(?KeyInfo_RSA_1024) ->            {"genrsa", 1024};
format_key_info(?KeyInfo_RSA_2048) ->            {"genrsa", 2048};
format_key_info(?KeyInfo_RSA_3072) ->            {"genrsa", 3072};
format_key_info(?KeyInfo_RSA_4096) ->            {"genrsa", 4096};
format_key_info(?KeyInfo_ECDSA_160) ->           ?'brainpoolP160r1';
format_key_info(?KeyInfo_ECDSA_224) ->           ?'brainpoolP224r1';
format_key_info(?KeyInfo_ECDSA_256) ->           ?'brainpoolP256r1';
format_key_info(?KeyInfo_ECDSA_384) ->           ?'brainpoolP384r1';
format_key_info(?KeyInfo_ECDSA_512) ->           ?'brainpoolP512r1';
%% format_key_info(?KeyInfo_ECDSA_521) ->           ?'brainpoolP521r1';
format_key_info(?KeyInfo_ECDSA_521) ->           {error, no_bpP521r1_support};
format_key_info(?KeyInfo_ECDSA_BRAINPOOL_256) -> ?'brainpoolP256r1';
format_key_info(?KeyInfo_ECDSA_BRAINPOOL_320) -> ?'brainpoolP320r1';
format_key_info(?KeyInfo_ECDSA_BRAINPOOL_384) -> ?'brainpoolP384r1';
format_key_info(?KeyInfo_ECDSA_BRAINPOOL_512) -> ?'brainpoolP512r1'.


%%% ----------------------------------------------------------
%%% #      format_priv_key({Format, PrivDerKey}) ->  
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
format_priv_key({Format, PrivDerKey}) ->
    case Format of
        'PrivateKeyInfo' ->
            PKI = public_key:der_decode('PrivateKeyInfo', PrivDerKey),
            #'PrivateKeyInfo'{privateKeyAlgorithm = PKA,
                              privateKey = PK} = PKI,
            Pk_bin =
            case PK of
                PK when is_list(PK) ->
                    list_to_binary(PK);
                PK ->
                    PK
            end,
            case PKA#'PrivateKeyInfo_privateKeyAlgorithm'.algorithm of
                ?'rsaEncryption' ->
                    public_key:der_decode('RSAPrivateKey', Pk_bin);
                _ ->
                    public_key:der_decode('ECPrivateKey', Pk_bin)
            end;
        _ ->
            public_key:der_decode(Format, PrivDerKey)
    end.


%%% ----------------------------------------------------------
%%% #      mk_otp(Salt, IterationCount, Secret) ->  
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: HMAC key size:  160 bits, if Key shorter
%%%              then 20 bytes fixit, according rfc4210
%%% ----------------------------------------------------------
mk_otp(Salt, IterationCount, Secret) ->
    do_hash(Secret++binary_to_list(Salt), sha, IterationCount).

do_hash(Data, Type, IterationCount) ->
    do_hash(Data, Type, IterationCount, 0).

do_hash(Data, _, IterationCount, IterationCount) ->
    Data;
do_hash(Data, Type, IterationCount, Count) ->
    Digest = crypto:hash(Type, Data),
    do_hash(Digest, Type, IterationCount, Count+1).


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

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

info_msg(Format, Args) ->
   sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

