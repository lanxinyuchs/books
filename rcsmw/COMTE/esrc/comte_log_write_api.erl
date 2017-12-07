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
%% @author Raimo Niskanen <raimo.niskanen@ericsson.com>
%% @doc
%%   Callback specification for a comte logWrite callback module. To configure logging
%%   please refer to the overview doc of ComtE.
%%   For an implementation example, see {@link comte_default_log_write}.
%% @end
-module(comte_log_write_api).

%% Return value validation exports
-export([logWrite/1]).

%% API

-callback start(Args::list()) ->
    ok.

-callback logWrite(
	    EventId :: comte_types:uint32(),
	    Severity :: comte_types:uint16(),
	    Facility :: comte_types:uint16(),
	    Data :: binary()) ->
    ok | comte_types:com_error().



%%% Return value validation
logWrite(X) -> X =:= ok orelse comte_types:com_error(X).
