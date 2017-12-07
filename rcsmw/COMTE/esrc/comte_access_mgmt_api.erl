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
%%   Callback specification for COM SA access management module.
%%   The module implementing this specification should handle or be
%%   the frontend of the role management. See {@link comte_default_access_mgmt}
%% @end
-module(comte_access_mgmt_api).

%% Return type validation exports
-export([getRoles/1]).

%% API
%% Derived from ComMwSpiAccessManagement_1

%% Will be called during startup of comte
-callback start(Args :: list()) ->
    ok.

%% Returns the roles for the user
-callback getRoles(User :: binary()) ->
    [Role::binary()] | comte_types:com_error().



%%% Return type validation
getRoles(X) ->
    comte_types:list(fun erlang:is_binary/1, X)
	orelse comte_types:com_error(X).
