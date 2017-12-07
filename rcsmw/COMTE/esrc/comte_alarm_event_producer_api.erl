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
%%   Callback specification for an alarm- and configuration management event producer.
%%   For an implementation example, see {@link comte_default_alarm_event_producer}.
%% @end

%%
%% Revision History
%%
%% Date: 20150508
%% Name: uabhten, erasipe
%% Description: OTP-11851. 
%% From Erlang 18, predefined type tuple/N, N>0,  no longer exists
%% instead the userdefined types filter & consumer_data 
%% must be defined as normal tuples {}.

-module(comte_alarm_event_producer_api).

%% Includes
-include("comte_event.hrl").

%% Return value validation exports
-export([register_consumer/1]).
-export([unregister_consumer/1, unregister_consumers/1]).

%% Types
-export_type([event_cons_filter/0,
              consumer_id/0,
              consumer_data/0,
              consumer_event_type/0,
              filter/0,
              filter_addr/0
             ]).

-type event_cons_filter() :: {cm_event, {dn, comte_types:ecim_dn()}} |
                             alarm.
%% Input data types for filter_consumer.

-type consumer_id() :: non_neg_integer().
%% The id of the consumer.

-type consumer_event_type() :: binary().
%% COM event types as described in MafOamSpiCmEvent*
%% and MafOamSpiNotificationFm*.

-opaque filter_addr() :: binary().

-type filter_type() :: binary().
%% COM filter types as described in MafOamSpiCmEvent*
%% and MafOamSpiNotificationFm*.

-type filter_value() :: binary().
%% The filter value depending on type as
%% described in MafOamSpiCmEvent* and
%% MafOamSpiNotificationFm* and
%% ComOamSpiPmEvent*.

-type filter() :: {filter_type(), filter_value()}.
%% The filter combined as filter_type and filter_value.

-type consumer_data() ::
	{consumer_id(),
	 consumer_event_type(),
	 filter_addr()}.
%% Consumer data to be included for
%% alarms and cm notifications.


%% API
-callback start(Args :: list()) ->
    ok.


-callback filter_consumers(event_cons_filter()) ->
    [consumer_data()].


-callback register_consumer(consumer_id(),
                            consumer_event_type(),
                            filter_addr(),
                            [filter()] ) ->
    ok | comte_types:com_error().

-callback unregister_consumer(consumer_id(),
                              consumer_event_type()) ->
    ok | comte_types:com_error().

-callback unregister_consumers() ->
    ok | comte_types:com_error().



%%% Return value validation

register_consumer(X) ->
    X =:= ok orelse comte_types:com_error(X).

unregister_consumer(X) ->
    X =:= ok orelse comte_types:com_error(X).

unregister_consumers(X) ->
    X =:= ok orelse comte_types:com_error(X).
