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
%% @author Magnus Liden <magnus.liden@ericsson.com>
%% @doc
%%   Callback specification for comte object implementer register.
%%   For an implementation example, see {@link comte_default_oi_register}.
%% @end
%% %CopyrightEnd%

-module(comte_oi_register_api).

%% Return value validation exports
-export([registerClass/1, registerDn/1]).
-export([unregisterClass/1, unregisterDn/1]).


-callback start(Args :: list()) ->
    ok.

-callback registerClass(
	    OI :: tuple(), ClassPath :: comte_types:ecim_class_path()) ->
    ok | comte_types:com_error().

-callback registerDn(
	    OI :: tuple(), DN :: comte_types:ecim_dn()) ->
    ok | comte_types:com_error().

-callback unregisterClass(
	    OI :: tuple(), ClassPath :: comte_types:ecim_class_path()) ->
    ok | comte_types:com_error().

-callback unregisterDn(
	    OI :: tuple(), DN :: comte_types:ecim_dn()) ->
    ok | comte_types:com_error().



%%% Return value validation
registerClass(X) -> X =:= ok orelse comte_types:com_error(X).
registerDn(X) -> X =:= ok orelse comte_types:com_error(X).
unregisterClass(X) -> X =:= ok orelse comte_types:com_error(X).
unregisterDn(X) -> X =:= ok orelse comte_types:com_error(X).
