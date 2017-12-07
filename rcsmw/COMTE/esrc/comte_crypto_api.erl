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
%%   Callback specification for COM SA Crypto utilities.
%%   The implementation should support encryption and decryption
%%   of ASCII strings.
%%   See default implementation: {@link comte_default_crypto_util}.
%%   <h2><a name="types">Data Types</a></h2>
%%   <h3 class="typedecl"><a name="type-error_reason">error_reason()</a></h3>
%%   <p><pre>error_reason() = binary()</pre></p>
%%   <h2>Function details</h2>
%%   <h3 class="function"><a name="decrypt-1">decrypt/1</a></h3>
%%   <div class="spec">
%%    <p><pre>decrypt(EncryptedString :: binary()) -&gt;
%%           String :: binary() | {error, <a href="#type-error_reason">error_reason()</a>}</pre></p>
%%    </div>
%%    <p>  The callback is invoked by ComtE if a
%%         COM component explicitly invokes the decrypt
%%         function, initially registered by ComtE.
%%    </p>
%%<h3 class="function"><a name="encrypt-1">encrypt/1</a></h3>
%%<div class="spec">
%%<p><pre>encrypt(String :: binary()) -&gt;
%%           EncryptedString :: binary() | {error, <a href="#type-error_reason">error_reason()</a>}</pre></p>
%%</div><p><p>The callback is invoked by ComtE whenever an
%%attribute of type 'EcimPassword' is created/set via COM.</p>
%%
%%  Netconf edit-config message excerpt:
%%  <pre>
%%       <code>&lt;pwd struct="EcimPassword"&gt;</code>
%%           <code>&lt;password&gt;a_password&lt;/password&gt;</code>
%%           <code>&lt;cleartext&gt;&lt;/cleartext&gt;</code>
%%       <code>&lt;/pwd&gt;</code>
%%  </pre>
%%  The presence of a cleartext element indicates
%%  that the value in the password element is entered in
%%  plaintext format. COM requests encryption
%%  before invocation of setMoAttribute.</p>
%%
%%<h3 class="function"><a name="start-1">start/1</a></h3>
%%<div class="spec">
%%<p><pre>start(Args :: list()) -&gt; ok</pre></p>
%%</div><p>The callback is invoked by ComtE during startup. Arguments to this functions
%% can be set together with the application environment variable 'crypto'.</p>
%% @end
-module(comte_crypto_api).

%% Return value validation exports
-export([encrypt/1, decrypt/1]).

%% API
%% Will be called if provided in app environment
-callback start(Args :: list()) ->
    ok.

%% Derived from ComMwSpiCrypto_1.h

%% Encrypt the given string
-callback encrypt(String :: binary()) ->
    EncryptedString :: binary() | comte_types:com_error().

%% Decrypt the given string
-callback decrypt(EncryptedString :: binary()) ->
    String :: binary() | comte_types:com_error().



%%% Return value validation

encrypt(X) ->
    is_binary(X) orelse comte_types:com_error(X).

decrypt(X) ->
    is_binary(X) orelse comte_types:com_error(X).
