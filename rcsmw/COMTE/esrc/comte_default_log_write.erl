%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2013. All Rights Reserved.
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
%%   Default callback module for logWrite.
%%   Logs all through io:format/2.
%% @end
-module(comte_default_log_write).

%% API
-export([start/1,logWrite/4]).

%% @doc
%% The callback is invoked by ComtE during startup.
%% No configuration is performed.
%% @end
-spec start(Args :: list()) -> ok.
start(_Args) ->
    ok.

%% @doc
%%   The callback is invoked by ComtE for writing log
%%   events originating from Com.
%% @end
-spec logWrite(
	EventId :: comte_types:uint32(),
	Severity :: comte_types:uint16(),
	Facility :: comte_types:uint16(),
	Data :: binary()) ->
		      ok.
logWrite(EventId, Severity, Facility, Data) ->
    {_,_,Ms} = Timestamp = os:timestamp(),
    {{YY,MM,DD},{H,M,S}} = calendar:now_to_local_time(Timestamp),
    io:format(
      "~B: "
      "~B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~6..0B "
      "[~B] ~B ~s~n",
      [EventId,
       YY,MM,DD,H,M,S,Ms,
       Severity, Facility, Data]),
    ok.
