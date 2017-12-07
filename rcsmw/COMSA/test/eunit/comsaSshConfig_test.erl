%% @author uabhgma
-module(comsaSshConfig_test).
-vsn('/main/R6A/1').
-date('2016-07-19').
-author('uabhgma').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% R6A/1      2016-07-19 uabhgma     Created

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    meck:new(mnesia,[passthrough,unstick]),
    meck:new(ssh,[passthrough,unstick]),
    Config = [],
    Config.

tear_down(_Config) ->
    meck:unload(mnesia),
    meck:unload(ssh),
    ok.


ssh_config_test_()->
    { foreach,
      fun setup/0,
      fun tear_down/1,
      [
       {"Test case 1, retrieve supported algorithms", fun test_get_supported_algorithms_by_type/0},
       {"Test case 2, extract wanted algorithms from a list of supported", fun test_extract_selected_algorithms/0},
       {"Test case 3, convert selected algos to ssh format", fun test_convert_selected_algos_to_ssh_opt_format/0},
       {"Test case 4, retrieve complete ssh preferred algos", fun test_get_ssh_preferred_algorithms/0}
      ]}.


%% ====================================================================
%% Test Cases
%% ====================================================================

%% ====================================================================
%% Test Case 1 - retrieve supported algorithms
%% ====================================================================
test_get_supported_algorithms_by_type() ->

    %% mock out the ssh default algorithms	
    DefaultAlgos = 
    [{kex,['diffie-hellman-group14-sha1',
           'diffie-hellman-group-exchange-sha256',
           'diffie-hellman-group1-sha1']},
   {public_key,['ssh-rsa','ssh-dss']},
   {cipher,[{client2server,['aes256-ctr','aes192-ctr','3des-cbc']},
            {server2client,['aes256-ctr','aes192-ctr','3des-cbc']}]},
   {mac,[{client2server,['hmac-sha2-256','hmac-sha2-512','hmac-sha1']},
         {server2client,['hmac-sha2-256','hmac-sha2-512','hmac-sha1']}]},
   {compression,[{client2server,[none,'zlib@openssh.com',zlib]},
               {server2client,[none,'zlib@openssh.com',zlib]}]}],

    ?debugFmt("The mocked out default algos ~p~n", [DefaultAlgos]),

    meck:expect(ssh,default_algorithms,fun() -> DefaultAlgos end),

    %% ------------- supported kex -------------------------------
    ExpectedKex = ["diffie-hellman-group14-sha1",
                   "diffie-hellman-group-exchange-sha256",
                   "diffie-hellman-group1-sha1"],
       
    SupportedKex = comsaSshConfig:get_supported_algorithms_by_type(kex),

    ?debugFmt("The expected kex ~p~n", [ExpectedKex]),
    ?debugFmt("Got supported kex ~p~n", [SupportedKex]),

    ?assert(ExpectedKex == SupportedKex),

    %% ------------- supported ciphers -------------------------------
    ExpectedCiphers = ["aes256-ctr","aes192-ctr","3des-cbc"],
           
    SupportedCipers = comsaSshConfig:get_supported_algorithms_by_type(cipher),

    ?debugFmt("The expected ciphers ~p~n", [ExpectedCiphers]),
    ?debugFmt("Got supported ciphers ~p~n", [SupportedCipers]),

    ?assert(ExpectedCiphers == SupportedCipers),

    %% ------------- supported macs -------------------------------
    ExpectedMacs = ["hmac-sha2-256","hmac-sha2-512","hmac-sha1"],
       
    SupportedMacs = comsaSshConfig:get_supported_algorithms_by_type(mac),

    ?debugFmt("The expected macs ~p~n", [ExpectedMacs]),
    ?debugFmt("Got supported macs ~p~n", [SupportedMacs]),

    ?assert(ExpectedMacs == SupportedMacs).

%% ====================================================================
%% Test Case 2 - extract wanted algorithms from a list of supported
%% 	It is enough to test a list of any algorithms regardless of type
%%	since type is not involved
%% ====================================================================
test_extract_selected_algorithms() ->
    
    SupportedAlgos = ["algorithm_1",
                      "algorithm_2",
 	              "algorithm_3",
                      "algorithm_4"],

    %% ------------- positive test -------------------------------
    WantedAlgos = ["algorithm_2",
                   "algorithm_4",
		   "some_other_not_supported_algo"],
    SelectedAlgos = comsaSshConfig:extract_selected_algorithms(SupportedAlgos, WantedAlgos),
    
    ExpectedAlgos = ["algorithm_2",
                     "algorithm_4"],

    ?assert(ExpectedAlgos == SelectedAlgos),

    %% ------------- negative test -------------------------------
    UnsupportedWantedAlgos = ["some_not_supported_algo",
    			      "some_other_not_supported_algo"],
    SelectedAlgos2 = comsaSshConfig:extract_selected_algorithms(SupportedAlgos, UnsupportedWantedAlgos),

    ?assert([] == SelectedAlgos2).
    
%% ====================================================================
%% Test Case 3
%% ====================================================================
test_convert_selected_algos_to_ssh_opt_format() ->

	%% ------------- positive kex algos -------------------------------
	SelectedKex = ["diffie-hellman-group14-sha1",
                       "diffie-hellman-group-exchange-sha256",
                       "diffie-hellman-group1-sha1"],

	ExpectedConvertedKex = {kex,['diffie-hellman-group14-sha1',
                       'diffie-hellman-group-exchange-sha256',
                       'diffie-hellman-group1-sha1']},

	ConvertedKex =  comsaSshConfig:convert_selected_algos_to_ssh_opt_format(SelectedKex,kex),
        ?debugFmt("The converted kex ~p~n", [ConvertedKex]),

        ?assert(ConvertedKex == ExpectedConvertedKex),

	%% ------------- positive cipher algos -------------------------------
	SelectedCiphers = ["aes256-ctr","aes192-ctr","3des-cbc"],

	ExpectedConvertedCiphers = {cipher,[{client2server,['aes256-ctr','aes192-ctr','3des-cbc']},
		                           {server2client,['aes256-ctr','aes192-ctr','3des-cbc']}]},

	ConvertedCiphers =  comsaSshConfig:convert_selected_algos_to_ssh_opt_format(SelectedCiphers,cipher),
        ?debugFmt("The converted ciphers ~p~n", [ConvertedCiphers]),

        ?assert(ConvertedCiphers == ExpectedConvertedCiphers),

	%% ------------- positive mac algos -------------------------------
	SelectedMac = ["hmac-sha2-256","hmac-sha2-512","hmac-sha1"],

	ExpectedConvertedMac = {mac,[{client2server,['hmac-sha2-256','hmac-sha2-512','hmac-sha1']},
	                               {server2client,['hmac-sha2-256','hmac-sha2-512','hmac-sha1']}]},

	ConvertedMac =  comsaSshConfig:convert_selected_algos_to_ssh_opt_format(SelectedMac,mac),
        ?debugFmt("The converted mac ~p~n", [ConvertedMac]),

        ?assert(ConvertedMac == ExpectedConvertedMac),

	      
	%% ------------- negative test (invalid type) -------------------------------
     	InvalidConverted = comsaSshConfig:convert_selected_algos_to_ssh_opt_format(SelectedKex,invalidType),
        ?assert(InvalidConverted == invalid_type).

%% ====================================================================
%% Test Case 4 - retrieve complete ssh preferred algos
%% ====================================================================
test_get_ssh_preferred_algorithms() ->

	SelectedCipher = ["cipher_algo_1","cipher_algo_2","cipher_algo_3"],
	SelectedKex = ["kex_algo_1","kex_algo_2","kex_algo_3"],
	SelectedMac = ["mac_algo_1","mac_algo_2","mac_algo_3"],

	ExpectedPreferredAlgos = {preferred_algorithms,[
		{cipher,[{client2server,['cipher_algo_1','cipher_algo_2','cipher_algo_3']},
		        {server2client,['cipher_algo_1','cipher_algo_2','cipher_algo_3']}]},
		{kex,['kex_algo_1','kex_algo_2','kex_algo_3']},
		{mac,[{client2server,['mac_algo_1','mac_algo_2','mac_algo_3']},
		     {server2client,['mac_algo_1','mac_algo_2','mac_algo_3']}]}]},
		     
	PreferredAlgos = comsaSshConfig:get_ssh_preferred_algorithms(SelectedCipher, SelectedKex, SelectedMac),
	?debugFmt("The preferred algos from comsaSshConfig: ~p~n", [PreferredAlgos]),
        ?assert(PreferredAlgos == ExpectedPreferredAlgos).

