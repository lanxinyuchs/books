%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaPass.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2013-2015
%%% @version /main/R2A/R4A/R5A/1

%%% @doc ==Callback module for the COM password encryption utility==
%%% This module implements the callback interface for encrypting and
%%% decrypting passwords
%%% @end
-module(comsaPass).
-vsn('/main/R2A/R4A/R5A/1').
-date('2015-10-21').
-author('etxpeno').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2015 All rights reserved.
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
%%% R2A/1      2013-06-10 etxjotj     Created
%%% R2A/3      2014-08-19 etxarnu     TR HS88305: fixed decrypt/1
%%% R4A/1      2015-10-14 etxpeno     change the seed to random:seed/3
%%% R5A/1      2015-10-21 etxpeno     remove seeding of random generator (not needed anymore)
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start/1]).
-export([encrypt/1]).
-export([decrypt/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%% @doc The callback is invoked by ComtE during startup.
%% No configuration performed
%% @end
%%% ----------------------------------------------------------

-spec start(Args::list()) -> ok.
start(_Args) ->
    ok.

%%% ----------------------------------------------------------
%% @doc Encrypts the password.
%% The callback is invoked by ComtE whenever an
%% attribute of type 'EcimPassword' is created/set via COM.
%%
%% Netconf edit-config message excerpt:
%% <pre>
%%      `<pwd struct="EcimPassword">'
%%          `<password>a_password</password>'
%%          `<cleartext></cleartext>'
%%      `</pwd>'
%% </pre>
%% The presence of a cleartext element indicates
%% that the value in the password element is entered in
%% plaintext format. COM requests encryption
%% before invocation of setMoAttribute.
%%
%% @end
%%% ----------------------------------------------------------
-spec encrypt(String::binary()) ->
                     EncryptedString::binary() | {error, binary()}.
encrypt(BinString) when is_binary(BinString) ->
    list_to_binary(comsaGeneric:encrypt_password(BinString));
encrypt(_String) ->
    {error, <<"not_a_binary">>}.

%%% ----------------------------------------------------------
%% @doc Returns the given parameter
%% The callback is invoked by ComtE if a
%% COM component explicitly invokes the decrypt
%% function, initially registered by ComtE.
%% @end
%%% ----------------------------------------------------------
-spec decrypt(EncryptedString::binary()) ->
                     String::binary() | {error, binary()}.
decrypt(BinString) when is_binary(BinString) ->
    case comsaGeneric:decrypt_password(binary_to_list(BinString)) of
	{error,Reason} ->
	    {error,Reason};
	Res ->
	    list_to_binary(Res)
    end;
decrypt(_String) ->
    {error, <<"not_a_binary">>}.



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

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
