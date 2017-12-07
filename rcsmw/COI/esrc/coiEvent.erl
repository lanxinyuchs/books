%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	coiEvent.erl %
%%% @author uabesvi
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R3A/R6A/3

%%% @doc == COI - Control system Oam Interface ==
%%% This module contains functions for event handling and subscription services.
%%%
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-module(coiEvent).
-vsn('/main/R3A/R6A/3').
-date('2016-09-09').
-author(etxberb).

%%% ###=====================================================================###
%%% # 0.3   LEGAL RIGHTS
%%% ###---------------------------------------------------------------------###
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
%%% 
%%% The information in this document is the property of Ericsson.
%%% 
%%% Except as specifically authorized in writing by Ericsson, the 
%%% receiver of this document shall keep the information contained 
%%% herein confidential and shall protect the same in whole or in 
%%% part from disclosure and dissemination to third parties.
%%% 
%%% Disclosure and disseminations to the receivers employees shall 
%%% only be made on a strict need to know basis.
%%% %CCaseCopyrightEnd%

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 1.    REVISION LOG
%%% ###---------------------------------------------------------------------###
%%% Rev    Date       Name     What
%% R3A/    ---------- -------  ------------------------------------------------
%% In the COMSA block:
%% ===================
%%% 1      2014-11-28 etxberb  Created.
%%% R3A/2  2014-12-12 etxberb  Added subscribe/1, unsubscribe/1,
%%%                            init_ets_tables/0 and translation to
%%%                            coi_notification_1.
%%% R3A/3  2015-01-05 etxberb  Added subscribe_background/1.
%% ===================
%% R3A/1   2015-01-13 etxberb  Moved to the COI block.
%% R3A/2   2015-02-13 etxberb  Added event sent from coiAlarm.
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 2.    MODULE BORDER LINE AND INTERNAL DEFINITIONS
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 2.1.1 Interface functions
%%% ###---------------------------------------------------------------------###
-export([notify/1,
	 subscribe/1,
	 subscribe/2,
	 unsubscribe/1,
	 unsubscribe/2,
	 get_subscriptions/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init_ets_tables/0]).
-export([subscribe_background/2]).

%%% ###---------------------------------------------------------------------###
%%% # Extended Interface
%%% ###---------------------------------------------------------------------###

%%% ###---------------------------------------------------------------------###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###
-include("comte_types.hrl").
-include("comte_event.hrl").

%%% ###=====================================================================###
%%% # 2.4   LOCAL DEFINITION OF MACROS
%%% ###---------------------------------------------------------------------###
%% General
-define(FUNCTION, 
	element(2, element(2, process_info(self(), current_function)))).

%%
-define(TblName, ?MODULE).

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###
-record(?TblName, {key,
		   value}).

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% @doc Notification to be forwarded to COI subscribers.
%%%
%%% @end
%%% ###=====================================================================###
notify(#cm_notification_1{trans_id = TransId,
			  source = Source,
			  events = Events}) ->
    CoiEvents       = coiEvents(Events),
    CoiNotification = {coi_notification_1, [{trans_id, TransId},
					    {source, Source},
					    {events, CoiEvents}]},
    notify_subscribers_cm_event(catch ets:lookup(?TblName, subscribers),
				CoiNotification);
notify({coi_notification_0, _} = CoiNotification) ->
    notify_subscribers_coi_event(catch ets:lookup(?TblName, subscribers),
				 CoiNotification).

%%% ###########################################################################
%%% @doc Subscribe for COI Notifications.
%%%
%%%   The Module subscribing for COI Notifications must export coi_notify/1.
%%%   -spec coi_notify(Notification :: coi_notification_0() |
%%%                                    coi_notification_1()) ->
%%%       ok.
%%%
%%% @end
-spec subscribe(Module :: atom()) ->
    ok.
%%% ###=====================================================================###
subscribe(Module) ->
    subscribe(Module, ["FmAlarm"]).

%%% ###########################################################################
%%% @doc Subscribe for COI Notifications.
%%%
%%%   The Module subscribing for COI Notifications must export coi_notify/1.
%%%   -spec coi_notify(Notification :: coi_notification_0() |
%%%                                    coi_notification_1()) ->
%%%       ok | {error, string()}.
%%%
%%% @end
-spec subscribe(Module :: atom(), Classes :: [string()] | all) ->
    ok | {error, string()}.
%%% ###=====================================================================###
subscribe(Module, Classes) ->
    case subscribe_insert(Module, Classes) of
	true ->
	    ok;
	table_not_exist ->
	    Pid = spawn(?MODULE, subscribe_background, [Module, Classes]),
	    error_logger:info_report([{?MODULE, subscribe},
				      {starting_background_job, Pid},
				      {module, Module},
				      {classes, Classes}]),
	    {error, "subscribe table does not exist"}
    end.

%%% ###=====================================================================###
subscribe_background(Module, Classes) ->
    timer:sleep(250),
    case subscribe_insert(Module, Classes) of
	true ->
	    error_logger:info_report([{?MODULE, subscribe},
				      {background_job_done, self()},
				      {module, Module},
				      {classes, Classes}]),
	    exit(normal);
	table_not_exist ->
	    subscribe_background(Module, Classes)
    end.

%%% ###=====================================================================###
subscribe_insert(Module, all = New) ->
    ets:match_delete(coiEvent, {coiEvent, subscribers, {Module, '_'}}),
    ets:insert(?TblName, #?TblName{key = subscribers, value = {Module, New}});
subscribe_insert(Module, Classes) ->
    try
	Old = ets:match(coiEvent, {coiEvent, subscribers, {Module, '$1'}}),
	New = lists:usort(si_subscribers(Old) ++ Classes),
	ets:match_delete(coiEvent, {coiEvent, subscribers, {Module, '_'}}),
	ets:insert(?TblName, #?TblName{key = subscribers, value = {Module, New}})
    catch
	_ : _ ->
	    %% init_ets_tables/0 has not been called yet. The coiServer process
	    %% is supposed to do that when starting!
	    table_not_exist
    end.

si_subscribers([]) ->
    [];
si_subscribers([[Old]]) ->
    Old.

%%% ###########################################################################
%%% @doc Unubscribe for COI Notifications.
%%%
%%% @end
-spec unsubscribe(Module :: atom()) ->
    ok.
%%% ###=====================================================================###
unsubscribe(Module) ->
    ets:match_delete(coiEvent, {coiEvent, subscribers, {Module, '_'}}),
    ok.

%%% ###########################################################################
%%% @doc Unubscribe for COI Notifications.
%%%
%%% @end
-spec unsubscribe(Module :: atom(), Classes :: [string()] | all) ->
    ok | {error, string()}.
%%% ###=====================================================================###
unsubscribe(Module, all) ->
    ets:match_delete(coiEvent, {coiEvent, subscribers, {Module, '_'}}),
    ok;
unsubscribe(Module, Classes) ->
    unsub_match(ets:match(coiEvent, {coiEvent, subscribers, {Module, '$1'}}),
		Module,
		Classes).

unsub_match([[all]], _Module, _Classes) ->
    {error, "subscribing to all MoClasses, cannot unsubscribe specific MoClasses"};
unsub_match([[Old]], Module, Classes) ->
    try 
	ets:match_delete(coiEvent, {coiEvent, subscribers, {Module, '_'}}),
	New = Old -- Classes,
	ets:insert(?TblName, #?TblName{key = subscribers, value = {Module, New}}),
	ok
    catch
	_ : _ ->
	    %% init_ets_tables/0 has not been called yet. The coiServer process
	    %% is supposed to do that when starting!
	    table_not_exist
    end;    
unsub_match(_, _, _) ->
    ok.

%%% ###########################################################################
%%% @doc Unubscribe for COI Notifications.
%%%
%%% @end
-spec get_subscriptions(Module :: atom()) ->
    [string()].
%%% ###=====================================================================###
get_subscriptions(Module) ->
	case ets:match(coiEvent, {coiEvent, subscribers, {Module, '$1'}}) of
	    [[Classes]] -> Classes;
	    []          -> []
	end.

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% @doc Initialize ets tables during start of the system. Called from
%%%   coiServer.erl.
%%%
%%% @end
%%% ###=====================================================================###
init_ets_tables() ->
    ets:new(?TblName, [named_table, public, bag, {keypos, 2}]),
    ok.

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% coiEvents
%%%
%%% ###=====================================================================###
coiEvents([#cm_event_1{dn = Dn,
		       event_type = EventType,
		       attributes = Attributes} | Tail]) ->
    EcimDn = [to_ecimDn(Dn)],
    [{coi_event_1, [{dn, EcimDn},
		    {event_type, EventType},
		    {attributes, Attributes}]} | coiEvents(Tail)];
coiEvents([]) ->
    [].

%%% ###########################################################################
%%% ecimPath_to_ecimDn
%%%
%%% ###=====================================================================###
ecimPath_to_ecimDn(EcimPath) ->
    list_to_binary(merge_ecimDn([sysUtil:term_to_string(E)
				 || E <- lists:reverse(EcimPath)])).

%%% ###########################################################################
%%% merge_ecimDn
%%%
%%% ###=====================================================================###
merge_ecimDn(Split_EcimDn) ->
    remove_last(lists:flatten(merge_ecimDn_loop(Split_EcimDn))).

merge_ecimDn_loop([Name, Value | Tail]) ->
    [Name, "=", Value, "," | merge_ecimDn_loop(Tail)];
merge_ecimDn_loop([]) ->
    [].

%%% ###########################################################################
%%% notify_subscribers
%%%
%%% ###=====================================================================###
notify_subscribers_coi_event([#?TblName{value = {Module, all}} | Tail], 
			     CoiNotification) ->
    nf_coi_event(true, Module, CoiNotification),
    notify_subscribers_coi_event(Tail, CoiNotification);
notify_subscribers_coi_event([#?TblName{value = {Module, Classes}} | Tail], 
			     CoiNotification) ->
    nf_coi_event(lists:member("FmAlarm", Classes), Module, CoiNotification),
    notify_subscribers_coi_event(Tail, CoiNotification);
notify_subscribers_coi_event([], _) ->
    ok;
notify_subscribers_coi_event({'EXIT', _}, _) ->
    error_logger:warning_report([{?MODULE, notify_subscribers},
				 {subscriber_table, undefined},
				 {notify, too_early_during_system_start}]).





nf_coi_event(true, Module, CoiNotification) ->
    try
	Module:coi_notify(CoiNotification)
    catch
	ErrClass : ErrReason ->
	    error_logger:error_report([{?MODULE, notify_subscribers},
				       subscriber_error,
				       {ErrClass, ErrReason},
				       {stacktrace, erlang:get_stacktrace()}])
    end;
nf_coi_event(false, _, _) ->
    ok.




notify_subscribers_cm_event([#?TblName{value = {Module, all}} | Tail], 
			    {coi_notification_1, Values} = CoiNotification) ->
    Events = proplists:get_value(events, Values, []),
    ns_cm_event(Events, Module, {coi_notification_1, Values}),
    notify_subscribers_cm_event(Tail, CoiNotification);
notify_subscribers_cm_event([#?TblName{value = {Module, Classes}} | Tail], 
			    {coi_notification_1, Values} = CoiNotification) ->
    Events         = proplists:get_value(events, Values, []),
    EventsFiltered = ns_cm_filter(Events, Classes, []),
    ValuesDeleted  = proplists:delete(events, Values),
    ns_cm_event(EventsFiltered,
		Module,
		{coi_notification_1, [{events, EventsFiltered} | ValuesDeleted]}),
    notify_subscribers_cm_event(Tail, CoiNotification);
notify_subscribers_cm_event([], _) ->
    ok;
notify_subscribers_cm_event({'EXIT', _}, _) ->
    error_logger:warning_report([{?MODULE, notify_subscribers},
				 {subscriber_table, undefined},
				 {notify, too_early_during_system_start}]).


ns_cm_event([], _Module, _) -> 
    ok;
ns_cm_event(_, Module, CoiNotification) ->
    try
	Module:coi_notify(CoiNotification)
    catch
	ErrClass : ErrReason ->
	    error_logger:error_report([{?MODULE, notify_subscribers},
				       subscriber_error,
				       {ErrClass, ErrReason},
				       {stacktrace, erlang:get_stacktrace()}])
    end.



ns_cm_filter([], _Classes, Acc) ->
    Acc;
ns_cm_filter([{coi_event_1, Values} = Event | T], Classes, Acc) ->
    [DnBin] = proplists:get_value(dn, Values, [<<"">>]),
    Dn      = binary_to_list(DnBin),
    [_, Class | _] = lists:reverse(string:tokens(Dn, ",=")),
    case lists:member(Class, Classes) of
	true  -> ns_cm_filter(T, Classes, [Event | Acc]);  
	false -> ns_cm_filter(T, Classes, Acc)
    end.





%%% ###########################################################################
%%% remove_last
%%%
%%% ###=====================================================================###
remove_last(List) ->
    {SubList, _} = lists:split(length(List) - 1, List),
    SubList.

%%% ###########################################################################
%%% ecimDn
%%%
%%% ###=====================================================================###
to_ecimDn([Dn]) when is_binary(Dn) ->
    Dn;
to_ecimDn([_ | _] = DnList) ->
    [FirstE | _] = Strings = [sysUtil:term_to_string(E) || E <- DnList],
    case string:str(FirstE, "ManagedElement") of
	0 ->
	    %% "ManagedElement" is not first in the list ->
	    %% DnList is ecim_path().
	    ecimPath_to_ecimDn(Strings);
	_ ->
	    Almost_EcimDn = lists:flatten([SubMo ++ "," || SubMo <- Strings]),
	    EcimDn = remove_last(Almost_EcimDn),   % Remove the last ",":
	    list_to_binary(EcimDn)
    end;
to_ecimDn(Other) ->
    Other.

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 4     CODE FOR TEMPORARY CORRECTIONS
%%% ###---------------------------------------------------------------------###
