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
%%   Implementation of a alarm/configuration management event producer.
%% @end
-module(comte_default_alarm_event_producer).
-behaviour(comte_alarm_event_producer_api).

-export([start/1]).
-export([register_consumer/4]).
-export([unregister_consumer/2,
         unregister_consumers/0]).
-export([filter_consumers/1]).

%%%===================================================================
%%% Macros and records
%%%===================================================================

-record(comte_consumer, {key, filter_addr, filters}).

tab_consumer() ->
    comte_consumer.


-define(
   LOGGER(Severity, Req, State, Fmt),
   begin
       comte_lib:error_logger((Severity), ?MODULE, ?LINE, (Req), (State), (Fmt))
   end).

-define(
   LOGGER(Severity, Req, State, Fmt, Args),
   begin
       comte_lib:error_logger((Severity), ?MODULE, ?LINE, (Req), (State), (Fmt), (Args))
   end).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% The callback is invoked by ComtE during startup. No configuration performed
%% @end
-spec start(list()) -> ok.
start(_Args) ->
  create_store().

%% @doc
%% Whenever an alarm or event is sent, this is called to determine affected
%% consumers.
%% @end
-spec filter_consumers(comte_alarm_event_producer_api:event_cons_filter()) ->
                              list(comte_alarm_event_producer_api:consumer_data()).
filter_consumers({cm_event, {dn, DN}}=_ConsFilter) ->
    filter_consumers_by_dn(DN);
filter_consumers(pm_gp_ready) ->
    filter_consumers_by_event(<<"ComOamSpiPmEventTypeGpReady_2">>);
filter_consumers(alarm) ->
    filter_alarm_consumers([]).


%% @doc
%% This is called during startup of COM, COM Components registers as
%% consumers of alarm and events.
%% @end
-spec register_consumer(comte_alarm_event_producer_api:consumer_id(),
                        comte_alarm_event_producer_api:consumer_event_type(),
                        comte_alarm_event_producer_api:filter_addr(),
                        [comte_alarm_event_producer_api:filter()] ) -> ok.
register_consumer(ConsumerId, EventType,
                  FilterAddr, Filters) when is_list(Filters) ->
    case compile_filters(Filters, []) of
	{error, _Error} ->
            %% Error during re compiler
            {error, could_not_compile_regexps};
        FilterList ->
            Key = {ConsumerId, EventType},
            Cons = #comte_consumer{
	      key=Key,
	      filter_addr=FilterAddr,
	      filters=FilterList},
            store_consumer(Cons)
    end.

%% @doc
%% This is called during shutdown of COM, COM Components unregisters
%% themselves as consumers.
%% @end
-spec unregister_consumer(comte_alarm_event_producer_api:consumer_id(),
                          comte_alarm_event_producer_api:consumer_event_type()) -> ok.
unregister_consumer(ConsumerId, EventType) ->
    remove_consumer({ConsumerId, EventType}).


%% @doc
%% This is called whenever all consumers should be removed.
%% @end
-spec unregister_consumers() -> ok.
unregister_consumers() ->
    clear_consumers().


%%%===================================================================
%%% Internal functions
%%%===================================================================


filter_consumers_by_dn(DN) ->
    AccIn = {fun evaluate_re_filters/2, DN, []},
    {_EF, _Args, AccOut} =
	ets:foldl(
	  fun find_consumer/2,
	  AccIn,
	  tab_consumer()),
    AccOut.

filter_alarm_consumers(Args) ->
    AccIn = {fun evaluate_alarm_filters/2, Args, []},
    {_EF, _Args, AccOut} =
	ets:foldl(
	  fun find_consumer/2,
	  AccIn,
	  tab_consumer()),
    AccOut.

filter_consumers_by_event(EventType) ->
    FilterFun = 
        fun(#comte_consumer{key={Id, ConsumerEventType},
                            filter_addr=FilterAddr},
            Acc) -> 
                case EventType =:= ConsumerEventType of
                    true -> [{Id, ConsumerEventType, FilterAddr}|Acc];
                    false -> Acc
                end
        end,
    ets:foldl(FilterFun, [], tab_consumer()).


find_consumer(#comte_consumer{filters=[]}, {_EF, _Args, Acc}) ->
    Acc;
find_consumer(#comte_consumer{key={Id, EventType},
                                 filter_addr=FilterAddr,
                                 filters=Filters},
              {EvalFun, Args, AccIn}) ->
    case EvalFun(Args, Filters) of
        ok ->
            %% Add consumer
            {EvalFun, Args, [{Id, EventType, FilterAddr} | AccIn]};
        _ ->
            {EvalFun, Args, AccIn}
    end.


evaluate_re_filters(_DN, []) ->
    {error, no_filter_match};
evaluate_re_filters(DN, [{datetime, _Filter} | FilterList]) ->
    evaluate_re_filters(DN, FilterList);
evaluate_re_filters(DN, [{regexp, Filter} | FilterList]) ->
    {_FType, _RegExp, CR} = Filter,
    case re:run(DN, CR) of
        {match, _} ->
            %% Match found, exit
            ok;
        _ ->
            evaluate_re_filters(DN, FilterList)
    end.

evaluate_alarm_filters(_UnusedArg, []) ->
    {error, no_filter_match};
evaluate_alarm_filters(UnusedArg, [{regexp, _Filter} | FilterList]) ->
    evaluate_alarm_filters(UnusedArg, FilterList);
evaluate_alarm_filters(_UnusedArg, [{datetime, _Filter} | _FilterList]) ->
    ok.


compile_filters([], Acc) ->
    lists:reverse(Acc);
%% The filter type specifies that the filter
%% is a regular expression string. The filter is applied on the MO DN
%% (dn in MafOamSpiCmEvent_Notification_1T)
compile_filters([{<<"MafOamSpiCmEvent_FilterTypeRegExp_",_Vsn/binary>> = FT,
                  RegExp} | Rest],Acc) ->
    case re:compile(RegExp) of
        {ok, CR} ->
            compile_filters(Rest,
                            [{regexp, {FT, RegExp, CR}} | Acc]);
        {error, Reason}=Error ->
            ?LOGGER(error, compile_filters, none,
                    "Could not compile regular expression ~p, reason: ~p",
                    [RegExp, Reason]),
            Error
    end;
%%
%% This filter type specifies that only notifications newer than the value
%% of this filter should be sent.
%%
%% If the value is 0 then Fault Management does not have any history of
%% any previous notifications and all notifications should be sent.
%% Otherwise compare and send only newer alarms
compile_filters([{<<"MafOamSpiNotificationFmFilterTypeDateTime_",_Vsn/binary>> = FT,
                  TimeStamp} | Rest], Acc) ->
    ComTimeNs = binary_to_integer(TimeStamp),
    %% ?LOGGER(info, compile_filters, none,
    %%         "History of previous events are not stored! T(ns) ~p",
    %%         [ComTimeNs]),
    compile_filters(Rest, [{datetime, {FT, ComTimeNs}} | Acc]).



create_store() ->
    Tab = tab_consumer(),
    Keypos = #comte_consumer.key,
    Opts = [set, public, named_table, {keypos, Keypos}],
    case ets:info(Tab) of
	undefined ->
	    Tab = ets:new(Tab, Opts);
	_ ->
	    true = ets:delete_all_objects(Tab)
    end,
    ok.

store_consumer(#comte_consumer{} = Entry) ->
    _ = ets:insert(tab_consumer(), [Entry]),
    ok.

remove_consumer(ConsumerKey) ->
    _ = ets:delete(tab_consumer(), ConsumerKey),
    ok.

clear_consumers() ->
    _ = ets:delete_all_objects(tab_consumer()),
    ok.
