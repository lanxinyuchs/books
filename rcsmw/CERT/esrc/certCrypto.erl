%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	certCrypto.erl %
%%% @author enenteo
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/R7A/R8A/R9A/R10A/R11A/2
%%% 
%%% @doc ==Crypto module for encryption and decryption==
%%% 
-module(certCrypto).

%%%---------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% Rev    Date       Name    What
%%% -----  ---------- ------- --------------------------------
%%% R7A/0  2016-09-26 emariad Created
%%% R8A/1  2016-11-14 emariad Added encryption/decryption functionality
%%% R8A/3  2016-11-22 etxasta Syntax
%%% R8A/4  2016-11-22 etxasta Added decode_pkcs12/2
%%% R8A/8  2016-11-23 etxasta Added VNFM and VNFC functions 
%%% R8A/9  2016-12-05 etxasta Updated VNFM and VNFC functions 
%%% R9A/1  2017-02-03 etxasta Added get_vnfc_certs/1 
%%% R9A/7  2017-02-22 etxasta Moved decode_pkcs12 to certPkcs12.erl 
%%% R9A/8  2017-04-11 emariad Added dist_vnf_log_key for vrcs, SP531
%%% R10A/1 2017-04-19 emariad Fixed a problem with abowe release
%%% R10A/2 2017-05-30 emariad Added sync when write crypt key to file system
%%% R10A/6 2017-08-17 enenteo Updating key backup handling
%%% R11A/1 2017-08-17 enenteo Delivery to R11
%%% R11A/2 2017-08-22 enenteo Fix dialyzer warning
%%% ----------------------------------------------------------
%%%
-define(KEY_FILE, "dataEncryptionKey").

%%%ENCRYPTION/DECRYPTION HANDLING VERSIONS
-define(VERSION_1, "1").
-define(LATEST_VERSION, "1").

%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%% Used for log and database, only local
-export([encrypt_data/1,
         decrypt_data/1]).

%% Used for for sharing between VNFM and VNFCs, observe uses hardcoded secret
-export([encrypt_shared/2,
         decrypt_shared/2]).


%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #          encrypt_data(Data) ->
%%% Input: Data  - Data to encrypt
%%% Output: {ok, EncryptedData}
%%% Exceptions: 
%%% Description: Encrypts the data and return the encrypted result.
%%% ----------------------------------------------------------
encrypt_data(Data) ->
    info_msg("encrypt_data", []),
    try
        case get_key_ivec() of
            {Key, Ivec} ->
                do_encrypt(Key, Ivec, Data);
            not_found ->
                do_encrypt_key_not_found(Data)
        end    
    catch Type:Reason ->
              {Type, Reason}
    end.

%%% ----------------------------------------------------------
%%% #          decrypt_data(Data) ->
%%% Input: Data  - Data to decrypt
%%% Output: {ok, DecryptedData}
%%% Exceptions: 
%%% Description: Decrypts the encrypted data and return the data..
%%% ----------------------------------------------------------
decrypt_data(Data) -> 
    info_msg("decrypt_data", []),
    try            
        case get_key_ivec() of
            {Key, Ivec} ->
                do_decrypt(Key, Ivec, Data);
            not_found ->
                do_decrypt_no_key(Data)
        end
    catch Type:Reason ->
              {Type, Reason}
    end.

%%% ----------------------------------------------------------
%%% #          encrypt_shared(Shared, Bin)
%%% Input:  Shared - string()
%%%         Bin    - binary()
%%% Output: {ok, binary()} | {error, string(Reason)} 
%%% Exceptions: 
%%% Description: Block encrypt a binary using OTP (One Time Password)
%%%              based on the shared secret.
%%% ----------------------------------------------------------
encrypt_shared(Shared, Bin) when is_list(Shared), is_binary(Bin) ->
    Salt = crypto:strong_rand_bytes(16),
    <<OTP16:16/binary,_/binary>> = certKey:mk_otp(Salt, 1000, Shared),
    EncData = crypto:block_encrypt(aes_ecb, OTP16, pad(16, Bin)),
    {ok, term_to_binary({enc1, size(Bin), Salt, EncData})}.

%%% ----------------------------------------------------------
%%% #          decrypt_shared(Shared, Bin)
%%% Input:  Shared - string()
%%%         Bin    - binary()
%%% Output: {ok, binary()} | {error, string(Reason)} 
%%% Exceptions: 
%%% Description: Encrypt a binary using OTP (One Time Password)
%%%              based on the shared secret.
%%% ----------------------------------------------------------
decrypt_shared(Shared, Bin) ->
    do_decrypt_shared1(Shared, catch binary_to_term(Bin)).

do_decrypt_shared1(Shared, {enc1, Size, Salt, EncData}) ->
    <<OTP16:16/binary,_/binary>> = certKey:mk_otp(Salt, 1000, Shared),
    do_decrypt_shared2(Size,
        catch crypto:block_decrypt(aes_ecb, OTP16, EncData));

do_decrypt_shared1(_, {EncType,_,_,_}) ->
    {error, "Encryption not supported: " ++ atom_to_list(EncType)};

do_decrypt_shared1(_, {'EXIT', Reason}) ->
    info_msg("The binary is no term, ~p", [Reason]),
    {error, "The binary is not a term"}.

do_decrypt_shared2(_, {'EXIT', Reason}) ->
    info_msg("Unable to decrypt the binary, ~p", [Reason]),
    {error, "Unable to decrypt the binary"};

do_decrypt_shared2(Size, BinData) ->
    <<Bin:Size/binary,_/binary>> = BinData,
    {ok, Bin}.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%%  #        er
%%% Description: Get key and initialization vector from sec store
%%%              for the running instance
%%% ----------------------------------------------------------
get_key_ivec() ->
    info_msg("get_key_ivec",[]),
    case whereis(certSecStore) of
        undefined ->
             case certSecStore:read_early_phase(keyivec, ?KEY_FILE) of
                 {ok, Bin} ->
                     binary_to_term(Bin);
                 {error, _Reason} ->
                     info_msg("Key not found in read_early_phase",[]),
                     not_found
             end;
        _ ->
            case certSecStore:read(keyivec, ?KEY_FILE) of
                {ok, Bin} ->
                    binary_to_term(Bin);
                {error, _Reason} ->
                    info_msg("Key not found",[]),
                    not_found
            end
    end.


%%% ----------------------------------------------------------
%%%  #        generate_key_ivecrvnfm
%%% Description: Generate a key and initialization vector,
%%               store to sec store and return the key and ivec
%%% ----------------------------------------------------------
generate_key_ivec() ->
    
    info_msg("Generate a new key for data encryption/decryption", []),
    Key  = crypto:strong_rand_bytes(32),
    Ivec = crypto:strong_rand_bytes(16),
    certSecStore:write(keyivec, ?KEY_FILE, term_to_binary({Key, Ivec})),
    %% This is an important file. Make sure it gets to the disk
    sync(),
    {Key, Ivec}.


%%% ----------------------------------------------------------
%%%  #        do_encrypt()
%%% Description: Do the encryption and add different 'header'
%%%              depending on node type
%%%              For the VNF node append VnfcId, and for the VNFM
%%%              append VNFM
%%% ----------------------------------------------------------
%% TODO @enenteo update for different encyrption types
do_encrypt(Key, Ivec, Data) ->
    info_msg("do_encrypt",[]),
    EncryptedData = crypto:block_encrypt(aes_cfb128, Key, Ivec, Data),
    Version = get_current_version(),
    case get_vnfc_id() of        
        {ok, VnfcId} ->
            {ok, term_to_binary({VnfcId, Version, EncryptedData})};
        {not_vnfc, Info} ->
            info_msg("~p", [Info]),
            {ok, term_to_binary({not_vnfc, Version, EncryptedData})}
    end.    
    

%%% ----------------------------------------------------------
%%%  #        do_encrypt_key_not_found(Data)
%%% Description: When key is not found, generate a new key,
%%%              distribute the key and encrypt the data
%%% ----------------------------------------------------------
do_encrypt_key_not_found(Data)->
    {Key, Ivec} = generate_key_ivec(),
    certDist:dist_vnf_key(term_to_binary({Key, Ivec})),
    do_encrypt(Key, Ivec, Data).

%%% ----------------------------------------------------------
%%%  #        get_vnfc_id()
%%% Description: Get running vnfcid
%%% ----------------------------------------------------------
get_vnfc_id() ->
    case VnfcId = apply(vnfcI, vnfc_id, []) of
        false ->
            %%if VNFC_ID is not defined
            {not_vnfc, "VNFC_ID env not defined"};
        _ ->
            {ok, VnfcId}
end.
    

%%% ----------------------------------------------------------
%%%  #        get_current_version()
%%% Description: get current encryption key handling  version
%%% ----------------------------------------------------------
get_current_version() ->
    ?LATEST_VERSION.

%%% ----------------------------------------------------------
%%%  #        do_decrypt(Key, Ivec, Data)
%%% Description:  Do the decryption
%%% ----------------------------------------------------------
do_decrypt(Key, Ivec, Data) ->

    case binary_to_term(Data) of
        {not_vnfc, ?VERSION_1, EncryptedData} ->
            info_msg("Data decrypted on R-VNFM", []),
            DecryptedData = crypto:block_decrypt(aes_cfb128, Key, Ivec, EncryptedData),
            {ok, DecryptedData};
        {VnfcId, ?VERSION_1, EncryptedData} ->
            DecryptedData = crypto:block_decrypt(aes_cfb128, Key, Ivec, EncryptedData),
            info_msg("Data decrypted for VnfcId:~p", [VnfcId]),
            {ok, DecryptedData};
        _ ->
            {error, "Data corrupted"}
    end.
    
%%% ----------------------------------------------------------
%%%  #        do_decrypt_no_key(Data)
%%% Description: Handle the situation when key is not available,few options possible:
%%%              1)Database is restored from previous instance, 
%%%                try to get the key from managing instance(VNFM)
%%%              2)fault, return error
%%% ----------------------------------------------------------
do_decrypt_no_key(Data) -> 
    try
        case binary_to_term(Data) of
            {not_vnfc, ?VERSION_1,_} -> %ignore EncryptedData since it is corrupted
                {error, "Encrypted data corrupted"};
            {VnfcIdRestored, ?VERSION_1, EncryptedData} ->
                case certDist:get_vnf_log_key_from_vnfm(VnfcIdRestored) of
                    {error, Message}->
                         {error,Message};
                    {ok,Key,Ivec} ->
                        %update R-VNFM with new VnfcId
                        certDist:dist_vnf_key(term_to_binary({Key, Ivec})),
                        do_decrypt(Key, Ivec, EncryptedData)
                   
                end;
            _ ->
                {error, "Data sent to decryption is not valid"}
        end
    catch Type:Reason->
              {Type, Reason}
    end.
        
%%% ----------------------------------------------------------
%%%  #        pad
%%% Description:  
%%% ----------------------------------------------------------
pad(Width, Binary) ->
    case ((Width - size(Binary) rem Width) rem Width) of
        0 -> Binary;
        N -> <<Binary/binary, 0:(N*8)>>
    end.

%%% ----------------------------------------------------------
%%%  #        sync
%%% Description:  
%%% ----------------------------------------------------------
sync() ->   
    os:cmd("sync"),
    timer:sleep(5000).

%%% ----------------------------------------------------------
%%% INFO, WARNING and ERROR MESSAGE HANDLING
%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
%%%  #        info_msg(Format, Args)
%%% Description:  info message wrap function
%%% ----------------------------------------------------------
info_msg(Format, Args) ->
   sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

