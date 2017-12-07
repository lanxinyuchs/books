%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: omc_lib_test.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%% -----      -------    --------    ------------------------
%%% R78/1    2016-11-17   emariad      Created
%%% ----------------------------------------------------------



-module(certCrypto_test).


-include_lib("eunit/include/eunit.hrl").


setup() ->
    meck:new(certSecStore,[unstick, passthrough]),
    Config = [],
    Config.

tear_down(_Config) ->
    meck:unload(certSecStore),
    ok.


crypto_test_() ->
    { foreach,
      fun setup/0,
      fun tear_down/1,
      [{"Test case: encrypt and decrypt", fun test_encrypt_and_decrypt/0}]
     }.

%% ====================================================================
%% Test Cases
%% ====================================================================
 
test_encrypt_and_decrypt() ->

    Key = crypto:strong_rand_bytes(32),
    Ivec = crypto:strong_rand_bytes(16),
    %Bin = term_to_binary({Key, Ivec}),

    meck:expect(certSecStore,read,2, fun(_,_) -> {ok, term_to_binary({Key, Ivec})} end),
    meck:expect(certSecStore,read_early_phase,2, fun(_,_) -> {ok, term_to_binary({Key, Ivec})} end),    
    
    %Encrypt
    Data = <<"Apa">>,
    {ok, EncryptedData} = certCrypto:encrypt_data(Data),
    ?assertNotEqual(Data, EncryptedData),
    
    %Decrypt 
    {ok, DecryptedData} = certCrypto:decrypt_data(EncryptedData),
    ?assertEqual(Data, DecryptedData).



