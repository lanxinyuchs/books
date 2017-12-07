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
%%   Callback specification for a COM replicated list.
%%   The provider of the replicated list shall secure that the content of all defined
%%   replicated lists survive all kinds of disturbances except a full cluster restart.
%%   This means that the replicated list does not necessarily need to be stored on
%%   persistent media.
%%
%%   For an example,
%%   see {@link comte_default_replicated_list}.
%% @end
-module(comte_replicated_list_api).

%% Return type validation exports
-export([listCreate/1, listDelete/1, listClear/1]).
-export([listGetSize/1, listIsEmpty/1]).
-export([listGetFrontRef/1, listPushBack/1, listPopBack/1]).
-export([listEraseItem/1, listFindItem/1, listReplaceItem/1]).
-export([listNumberOfListInstances/1, listMemoryUsage/1]).



%% API
-callback start(Args :: list()) ->
    ok.

%% Derived from ComMwSpiReplicatedList_1
-callback listCreate(
	    comte_types:listinstancename(), comte_types:uint32()) ->
    ok | eexist | comte_types:com_error().

-callback listDelete(comte_types:listinstancename()) ->
    ok | enoent | comte_types:com_error().

-callback listClear(comte_types:listinstancename()) ->
    ok | enoent | comte_types:com_error().

-callback listGetSize(comte_types:listinstancename()) ->
    comte_types:uint32() | enoent | comte_types:com_error().

-callback listIsEmpty(comte_types:listinstancename()) ->
    boolean() | enoent | comte_types:com_error().

-callback listGetFrontRef(comte_types:listinstancename()) ->
    [{comte_types:listitemname(), comte_types:listdata()}] |
    enoent |
    comte_types:com_error().

-callback listPushBack(
	    comte_types:listinstancename(), comte_types:listdata()) ->
    comte_types:listitemname() | enoent | comte_types:com_error().

-callback listPopBack(comte_types:listinstancename()) ->
    ok | enoent | comte_types:com_error().

-callback listEraseItem(
	    comte_types:listinstancename(), comte_types:listitemname()) ->
    ok | enoent | comte_types:com_error().

-callback listFindItem(
	    comte_types:listinstancename(), comte_types:listitemname()) ->
    comte_types:listdata() | enoent | comte_types:com_error().

-callback listReplaceItem(
	    comte_types:listinstancename(), comte_types:listitemname(),
	    comte_types:listdata()) ->
    ok | enoent | comte_types:com_error().

-callback listNumberOfListInstances() ->
    comte_types:uint32() | comte_types:com_error().

-callback listMemoryUsage() ->
    {Used :: comte_types:uint32(), Available :: comte_types:uint32()} |
    comte_types:com_error().



%%% Return type validation functions

listCreate(X) ->
    X =:= ok orelse X =:= eexist orelse comte_types:com_error(X).

listDelete(X) ->
    X =:= ok orelse X =:= enoent orelse comte_types:com_error(X).

listClear(X) ->
    X =:= ok orelse X =:= enoent orelse comte_types:com_error(X).

listGetSize(X) ->
    comte_types:uint32(X) orelse X =:= enoent orelse comte_types:com_error(X).

listIsEmpty(X) ->
    is_boolean(X) orelse X =:= enoent orelse comte_types:com_error(X).

listGetFrontRef(X) ->
    listGetFrontRef_list(X) orelse X =:= enoent	orelse
	comte_types:com_error(X).
%%
listGetFrontRef_list([]) -> true;
listGetFrontRef_list([{N, D}|Xs]) ->
    comte_types:listitemname(N) andalso comte_types:listdata(D) andalso
	listGetFrontRef_list(Xs);
listGetFrontRef_list(_) -> false.

listPushBack(X) ->
    comte_types:listitemname(X) orelse X =:= enoent orelse
	comte_types:com_error(X).

listPopBack(X) ->
    X =:= ok orelse X =:= enoent orelse	comte_types:com_error(X).

listEraseItem(X) ->
    X =:= ok orelse X =:= enoent orelse	comte_types:com_error(X).

listFindItem(X) ->
    comte_types:listdata(X) orelse X =:= enoent orelse
	comte_types:com_error(X).

listReplaceItem(X) ->
    X =:= ok orelse X =:= enoent orelse	comte_types:com_error(X).

listNumberOfListInstances(X) ->
    comte_types:uint32(X) orelse comte_types:com_error(X).

listMemoryUsage({Used, Available}) ->
    comte_types:uint32(Used) andalso comte_types:uint32(Available);
listMemoryUsage(X) -> comte_types:com_error(X).
