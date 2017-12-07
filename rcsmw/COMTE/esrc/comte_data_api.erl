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
%%   Callback specification for a comte data callback module.
%%   For an implementation example, see {@link comte_example_callback}.
%% @end

%%
%% Revision History
%%
%% Date: 20150508
%% Name: uabhten, erasipe
%% Description: OTP-11851. 
%% From Erlang 18, predefined type tuple/N, N>0,  no longer exists
%% the callback setMoAttributes changed to match normal tuple {}
%% definition. 


-module(comte_data_api).

%% API
-callback prepare(
	    ECIMPath :: comte_types:ecim_path(),
	    undefined | deleted | comte_types:user_object(),
	    TransID :: comte_types:transaction_id()) ->
    ok |
    {ok, NewUserObject :: comte_types:user_object()} |
    comte_types:com_error().

-callback commit(TransId :: comte_types:transaction_id()) ->
    ok | comte_types:com_error().

-callback finish(
	    ECIMPath :: comte_types:ecim_path(),
	    UserObject :: undefined | deleted | comte_types:user_object(),
	    TransId :: comte_types:transaction_id()) ->
    ok | comte_types:com_error().

-callback action(
	    ECIMPath :: comte_types:ecim_path(),
	    Params :: [comte_types:com_named_parameter()],
	    TransId :: comte_types:transaction_id()) ->
    comte_types:com_data() | comte_types:com_error().


%% MO Callbacks
-callback getMoAttribute(
	    ECIMPath :: comte_types:ecim_path(),
	    TransId :: comte_types:transaction_id()) ->
    comte_types:com_data() | comte_types:com_error().

-callback getMoAttributes(
	    [ECIMPath :: comte_types:ecim_path()],
	    TransId :: comte_types:transaction_id()) ->
    [comte_types:com_data()] | comte_types:com_error().

-callback setMoAttribute(
	    ECIMPath :: comte_types:ecim_path(),
	    Value :: comte_types:com_data(),
	    UserObject :: undefined | comte_types:user_object(),
	    comte_types:transaction_id()) ->
    ok | {ok, NewUserObject :: comte_types:user_object()} |
    comte_types:com_error().

-callback setMoAttributes(
	    NamedAttrs :: [{ECIMPath :: comte_types:ecim_path(),
			    Value :: comte_types:com_data()}],
	    UserObject :: undefined | comte_types:user_object(),
	    comte_types:transaction_id()) ->
    ok | {ok, NewUserObject :: comte_types:user_object()} |
    comte_types:com_error().

-callback createMo(
	    ECIMPath :: comte_types:ecim_path(),
	    KeyName :: binary(),
	    KeyValue :: binary(),
	    TransId :: comte_types:transaction_id()) ->
    ok | {ok, NewUserObject :: comte_types:user_object()} |
    comte_types:com_error().

-callback deleteMo(
	    ECIMPath :: comte_types:ecim_path(),
	    TransId :: comte_types:transaction_id()) ->
    ok | comte_types:com_error().


-callback nextMo(
	    ECIMPath :: comte_types:ecim_path(),
	    CurrKey :: undefined | term(),
	    comte_types:transaction_id()) ->
    {ok,
     undefined |
     {ComKey :: comte_types:com_value(),
      NextKey :: term()}} |
    comte_types:com_error().

-callback existsMo(
	    ECIMPath :: comte_types:ecim_path(),
	    TransId :: comte_types:transaction_id()) ->
    true | false | comte_types:com_error().
