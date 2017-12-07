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


-module(comte_default_oi_register).

-behaviour(comte_oi_register_api).
%% Behaviour exports
-export([start/1]).
-export([registerClass/2]).
-export([registerDn/2]).
-export([unregisterClass/2]).
-export([unregisterDn/2]).


%% Key = {class, MOC} | {dn, DN}

%% key = {class, MOC} | {dn, DN}
%% -record(?MODULE, {key, object_impl_if}).

-spec start(Args :: list()) -> ok.
start(_Args) ->
    Opts = [set, public, named_table],
    comte_lib:tab_create([?MODULE], Opts).

-spec registerClass(
	OI :: tuple(),
	ClassPath :: comte_types:ecim_class_path()) ->
			   ok.
registerClass(OI, ClassPath) ->
    Key = {class, ClassPath},
    ok = comte_lib:tab_insert(?MODULE, Key, OI).

-spec registerDn(OI :: tuple(), DN :: comte_types:ecim_dn()) -> ok.
registerDn(OI, DN) ->
    ComteKey = comte_model:ecim_to_comte_key(DN, ""),
    Key = {dn, ComteKey},
    ok = comte_lib:tab_insert(?MODULE, Key, OI).

-spec unregisterClass(
	OI :: tuple(), 
	ClassPath :: comte_types:ecim_class_path()) ->
			     ok.
unregisterClass(_OI, ClassPath) ->
    Key = {class, ClassPath},
    comte_lib:tab_delete_entry(?MODULE, Key).

-spec unregisterDn(OI :: tuple(), DN :: comte_types:ecim_dn()) -> ok.
unregisterDn(_OI, DN) ->
    ComteKey = comte_model:ecim_to_comte_key(DN, ""),
    Key = {dn, ComteKey},
    comte_lib:tab_delete_entry(?MODULE, Key).



%% get_oi(DN) ->
%%     ComteKey = comte_model:ecim_to_comte_key(DN, ""),
%%     Key = {dn, ComteKey},
%%     case comte_lib:tab_lookup(?MODULE, Key) of
%% 	{Key, OI} ->
%% 	    OI;
%% 	Error ->
%% 	    Error
%%     end.
