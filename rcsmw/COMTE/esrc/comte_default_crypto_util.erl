%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%% @author Magnus Liden <magnus.liden@ericsson.com>
%% @doc
%%   Default callback module for the crypto utilities.
%%   It mimics COM's default behaviour, no
%%   encryption/decryption is performed.
%% @end
-module(comte_default_crypto_util).

%% API
-export([start/1]).
-export([encrypt/1]).
-export([decrypt/1]).

%% @doc
%% The callback is invoked by ComtE during startup. No configuration performed
%% @end
-spec start(Args :: list()) -> ok.
start(_Args) ->
    ok.

%% @doc
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
-spec encrypt(String :: binary()) ->
                     EncryptedString :: binary() |
					{error, comte_types:error_reason()}.
encrypt(BinString) when is_binary(BinString) ->
    BinString;
encrypt(_String) ->
    {error, <<"not_a_binary">>}.


%% @doc
%% The callback is invoked by ComtE if a
%% COM component explicitly invokes the decrypt
%% function, initially registered by ComtE.
%% @end
-spec decrypt(EncryptedString :: binary()) ->
                     String :: binary() |
			       {error, comte_types:error_reason()}.
decrypt(BinString) when is_binary(BinString) ->
    BinString;
decrypt(_String) ->
    {error, <<"not_a_binary">>}.




