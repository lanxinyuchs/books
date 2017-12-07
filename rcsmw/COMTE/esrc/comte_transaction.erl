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
%%   Callback specification for a comte transaction server.
%%   For an implementation example, see {@link comte_default_transaction_server}.
%%   Valuable information can also be found in {@link comte_example_callback}.
%% @end

-module(comte_transaction).

%% Return value validation exports
-export([join/1, prepare/1, commit/1]).
-export([abort_transaction/1, finish/1, validate/1]).
-export([setMoAttribute/1, setMoAttributes/1, getMoIterator/1]).
-export([getMoAttribute/1, getMoAttributes/1]).
-export([createMo/1, deleteMo/1, existsMo/1, countMoChildren/1]).
-export([action/1]).

%% API
-callback start(Args :: list()) ->
    ok.

%% Derived from ComOamSpiTransactionalResource_1
-callback join(TransId :: comte_types:transaction_id()) ->
    ok | comte_types:com_error().

-callback prepare(TransId :: comte_types:transaction_id()) ->
    ok | comte_types:com_error().

-callback commit(TransId :: comte_types:transaction_id()) ->
    ok | comte_types:com_error().

-callback abort_transaction(TransId :: comte_types:transaction_id()) ->
    ok | comte_types:com_error().

-callback finish(TransId :: comte_types:transaction_id()) ->
    ok | comte_types:com_error().

%% Derived from ComOamSpiTransactionalResource_2
-callback validate(TransId :: comte_types:transaction_id()) ->
    ok |
    {ok, boolean()} |
    {ok, boolean(), comte_types:error_reason()} |
    {ok, boolean(), [comte_types:error_reason()]} |
    comte_types:com_error().


%% MO Callbacks
%% Derived from ComOamSpiManagedObject_3_1
-callback setMoAttribute(
	    TransId :: comte_types:transaction_id(),
	    Dn :: comte_types:ecim_dn(),
	    AttrName :: comte_types:mo_attribute_name(),
	    AttrValue :: comte_types:com_data()) ->
    ok | comte_types:com_error().

-callback setMoAttributes(
	    TransId :: comte_types:transaction_id(),
	    Dn :: comte_types:ecim_dn(),
	    Attributes :: [comte_types:com_named_attribute()]) ->
    ok | comte_types:com_error().

-callback getMoAttribute(
	    TransId :: comte_types:transaction_id(),
	    Dn :: comte_types:ecim_dn(),
	    AttributeName :: comte_types:mo_attribute_name()) ->
    AttrValue :: comte_types:com_data() | comte_types:com_error().

%% From COM 5.0
-callback getMoAttributes(
	    TransId :: comte_types:transaction_id(),
	    Dn :: comte_types:ecim_dn(),
	    AttributeNames :: [comte_types:mo_attribute_name()]) ->
    [AttrValue :: comte_types:com_data()] |
    comte_types:com_error().

-callback getMoIterator(
	    TransId :: comte_types:transaction_id(),
	    Dn :: comte_types:ecim_dn(),
	    ClassName :: binary()) ->
    [comte_types:mo_instance()] | comte_types:com_error().


-callback createMo(
	    TransId :: comte_types:transaction_id(),
	    Dn :: comte_types:ecim_dn(),
	    ClassName :: binary(),
	    KeyAttrName :: binary(),
	    KeyAttrValue :: binary(),
	    InitialAttributes :: [comte_types:com_named_attribute()]) ->
    ok | comte_types:com_error().

-callback deleteMo(
	    TransId :: comte_types:transaction_id(),
	    Dn :: comte_types:ecim_dn()) ->
    ok | comte_types:com_error().

-callback existsMo(
	    TransId :: comte_types:transaction_id(),
	    Dn :: comte_types:ecim_dn()) ->
    boolean() | comte_types:com_error().

-callback countMoChildren(
	    TransId :: comte_types:transaction_id(),
	    Dn :: comte_types:ecim_dn(),
	    ClassName :: binary()) ->
    non_neg_integer() | comte_types:com_error().

-callback action(
	    TransId :: comte_types:transaction_id(),
	    Dn :: comte_types:ecim_dn(),
	    ActionName :: binary(),
	    Params :: [comte_types:com_named_parameter()]) ->
    ok | comte_types:com_data() | comte_types:com_error().



%%% Callback return value validation functions

join(X) ->
    X =:= ok orelse comte_types:com_error(X).

prepare(X) ->
    X =:= ok orelse comte_types:com_error(X).

commit(X) ->
    X =:= ok orelse comte_types:com_error(X).

abort_transaction(X) ->
    X =:= ok orelse comte_types:com_error(X).

finish(X) ->
    X =:= ok orelse comte_types:com_error(X).

validate(X) ->
    case X of
	ok -> true;
	{ok, B} -> is_boolean(B);
	{ok, B, Msg} ->
	    is_boolean(B) andalso
		(comte_types:error_reason(Msg)
		 orelse comte_types:list(
			  fun comte_types:error_reason/1,
			  Msg));
	_ -> comte_types:com_error(X)
    end.



setMoAttribute(X) ->
    X =:= ok orelse comte_types:com_error(X).

setMoAttributes(X) ->
    X =:= ok orelse comte_types:com_error(X).

getMoAttribute(X) ->
    comte_types:com_data(X) orelse comte_types:com_error(X).

getMoAttributes(X) ->
    comte_types:list(fun comte_types:com_data/1, X)
	orelse comte_types:com_error(X).

getMoIterator(X) ->
    comte_types:list(fun comte_types:mo_instance/1, X)
	orelse comte_types:com_error(X).

createMo(X) ->
    X =:= ok orelse comte_types:com_error(X).

deleteMo(X) ->
    X =:= ok orelse comte_types:com_error(X).

existsMo(X) ->
    is_boolean(X) orelse comte_types:com_error(X).

countMoChildren(X) ->
    (is_integer(X) andalso X >= 0)
	orelse comte_types:com_error(X).

action(X) ->
    X =:= ok orelse comte_types:com_data(X) orelse
	comte_types:com_error(X).
