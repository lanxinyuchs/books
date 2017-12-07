%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2013. All Rights Reserved.
%%
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%%
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_ntf_subscriber.erl
%%
%% Description:
%%
%%
%%--------------------------------------------------------------------
-module(safs_ntf_subscriber).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("ntf.hrl").

-include("safs_ntf_db.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------

-export([
	 get_subscribers/1
	]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------


%%======================================================================
%% External Interface Functions
%%======================================================================

get_subscribers(Notification) ->
    ets:foldl(
      fun(#safs_ntf_subscriber{id = {Handle, SubscriptionId},
			       notificationtype_filters = Filters}, Acc) ->
	      case match(Notification, Filters) of
		  true ->
		      [{Handle, SubscriptionId}|Acc];
		  false ->
		      Acc
	      end
      end, [], safs_ntf_subscriber).

%%====================================================================
%% Internal interface functions
%%====================================================================


%%====================================================================
%% Internal functions
%%====================================================================

match(Notification,
      #safsNtfNotificationTypeFilters{objectCreateDeleteNotificationFilter = Filter}) when is_record(Notification, safsNtfObjectCreateDeleteNotification) ->
    match_object_create_delete_notification(Notification, Filter);
match(Notification,
      #safsNtfNotificationTypeFilters{attributeChangeNotificationFilter = Filter}) when is_record(Notification, safsNtfAttributeChangeNotification) ->
    match_attribute_change_notification(Notification, Filter);
match(Notification,
      #safsNtfNotificationTypeFilters{stateChangeNotificationFilter = Filter}) when is_record(Notification, safsNtfStateChangeNotification) ->
    match_state_change_notification(Notification, Filter);
match(Notification,
      #safsNtfNotificationTypeFilters{alarmNotificationFilter = Filter}) when is_record(Notification, safsNtfAlarmNotification) ->
    match_alarm_notification(Notification, Filter);
match(Notification,
      #safsNtfNotificationTypeFilters{securityAlarmNotificationFilter = Filter}) when is_record(Notification, safsNtfSecurityAlarmNotification) ->
    match_security_alarm_notification(Notification, Filter).

match_object_create_delete_notification(_Notification, undefined) ->
    false;
match_object_create_delete_notification(Notification, Filter) ->
    match_header(Notification#safsNtfObjectCreateDeleteNotification.notificationHeader,
		 Filter#safsNtfObjectCreateDeleteNotificationFilter.notificationFilterHeader)
	andalso
	match_element(Notification#safsNtfObjectCreateDeleteNotification.sourceIndicator,
		      Filter#safsNtfObjectCreateDeleteNotificationFilter.sourceIndicators).

match_attribute_change_notification(_Notification, undefined) ->
    false;
match_attribute_change_notification(Notification, Filter) ->
    match_header(Notification#safsNtfAttributeChangeNotification.notificationHeader,
		 Filter#safsNtfAttributeChangeNotificationFilter.notificationFilterHeader)
	andalso
	match_element(Notification#safsNtfAttributeChangeNotification.sourceIndicator,
		      Filter#safsNtfAttributeChangeNotificationFilter.sourceIndicators).

match_state_change_notification(_Notification, undefined) ->
    false;
match_state_change_notification(Notification, Filter) ->
    match_header(Notification#safsNtfStateChangeNotification.notificationHeader,
		 Filter#safsNtfStateChangeNotificationFilter.notificationFilterHeader)
	andalso
	match_element(Notification#safsNtfStateChangeNotification.sourceIndicator,
		      Filter#safsNtfStateChangeNotificationFilter.sourceIndicators)
	andalso
	match_element(Notification#safsNtfStateChangeNotification.changedStates,
		      Filter#safsNtfStateChangeNotificationFilter.changedStates).

match_alarm_notification(_Notification, undefined) ->
    false;
match_alarm_notification(Notification, Filter) ->
    match_header(Notification#safsNtfAlarmNotification.notificationHeader,
		 Filter#safsNtfAlarmNotificationFilter.notificationFilterHeader)
	andalso
	match_element(Notification#safsNtfAlarmNotification.probableCause,
		      Filter#safsNtfAlarmNotificationFilter.probableCauses)
	andalso
	match_element(Notification#safsNtfAlarmNotification.perceivedSeverity,
		      Filter#safsNtfAlarmNotificationFilter.perceivedSeverities)
	andalso
	match_element(Notification#safsNtfAlarmNotification.trend,
		      Filter#safsNtfAlarmNotificationFilter.trends).

match_security_alarm_notification(_Notification, undefined) ->
    false;
match_security_alarm_notification(Notification, Filter) ->
    match_header(Notification#safsNtfSecurityAlarmNotification.notificationHeader,
		 Filter#safsNtfSecurityAlarmNotificationFilter.notificationFilterHeader)
	andalso
	match_element(Notification#safsNtfSecurityAlarmNotification.probableCause,
		      Filter#safsNtfSecurityAlarmNotificationFilter.probableCauses)
	andalso
	match_element(Notification#safsNtfSecurityAlarmNotification.severity,
		      Filter#safsNtfSecurityAlarmNotificationFilter.severities)
	andalso
	match_element(Notification#safsNtfSecurityAlarmNotification.securityAlarmDetector,
		      Filter#safsNtfSecurityAlarmNotificationFilter.securityAlarmDetectors)
	andalso
	match_element(Notification#safsNtfSecurityAlarmNotification.serviceUser,
		      Filter#safsNtfSecurityAlarmNotificationFilter.serviceUsers)
	andalso
	match_element(Notification#safsNtfSecurityAlarmNotification.serviceProvider,
		      Filter#safsNtfSecurityAlarmNotificationFilter.serviceProviders).

match_header(Header, FilterHeader) ->
    match_element(Header#safsNtfNotificationHeader.eventType,
		  FilterHeader#safsNtfNotificationFilterHeader.eventTypes)
	andalso
	match_element(Header#safsNtfNotificationHeader.notificationObject,
		      FilterHeader#safsNtfNotificationFilterHeader.notificationObjects)
	andalso
	match_element(Header#safsNtfNotificationHeader.notifyingObject,
		      FilterHeader#safsNtfNotificationFilterHeader.notifyingObjects)
	andalso
	match_element(Header#safsNtfNotificationHeader.notificationClassId,
		      FilterHeader#safsNtfNotificationFilterHeader.notificationClassIds).

match_element(_El, []) ->
    true;
match_element(EventType, EventTypes) ->
    lists:member(EventType, EventTypes).
