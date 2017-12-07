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
%% File: safs_ntf.erl
%%
%% Description:
%%    This file implements the erlang API for the NTF service.
%%
%%--------------------------------------------------------------------
-module(safs_ntf).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("safs_ais.hrl").
-include("safs_ntf.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Life-cycle
-export([
         initialize/1,
         initialize/2,
         finalize/1
        ]).

%%----------------------------------------------------------------------
%% Producer
-export([
         notification_send/2
        ]).

%%----------------------------------------------------------------------
%% Consumer
-export([
         notification_subscribe/3,
         notification_unsubscribe/2,
         notification_read_initialize/3,
         notification_read_next/2,
         notification_read_finalize/1
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 trace_groups/0,
	 trace_points_list/0,
	 callbacks_initialize/1
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

-define(SERVER, safs_ntf_srv).
-define(SUPPORTED_NTF_VERSION, #safsVersion{releaseCode  = $A,
					    majorVersion = 1,
					    minorVersion = 1}).

-define(NTF_RELEASE_CODE, $A).
-define(NTF_MAJOR_VERSION, 1).
-define(NTF_MINOR_VERSION, 1).
%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%======================================================================
%% Library Life Cycle
%%======================================================================

% -spec initialize(Callbacks) -> Result when
%       Callbacks :: sa_ntf_callbacks(),
%       Result :: {ok, Handle, SupportedVersion} | {error, atom()},
%       Handle :: sa_ntf_handle(),
%       SupportedVersion :: #safsVersion{}.
initialize(Callbacks) ->
    Version = #safsVersion{releaseCode = ?NTF_RELEASE_CODE,
			   majorVersion = ?NTF_MAJOR_VERSION,
			   minorVersion = ?NTF_MINOR_VERSION},
    initialize(Callbacks, Version).

% -spec initialize(Callbacks, Version) -> Result when
%       Callbacks :: sa_ntf_callbacks(),
%       Version :: #safsVersion{},
%       Result :: {ok, Handle, SupportedVersion} | {error, atom()},
%       Handle :: sa_ntf_handle(),
%       SupportedVersion :: #safsVersion{}.
initialize(Callbacks, #safsVersion{} = Version) when is_atom(Callbacks); 
						     is_function(Callbacks, 1);
						     is_record(Callbacks, safsNtfCallbacks)->
    case validate_version(Version) of
	{ok, Version1}  ->
	    call({initialize, Callbacks, Version1, self()});
	{error, Error} ->
	    safs_error(initialize, "version error"),
	    {error, Error}
    end.

% -spec finalize(Handle::sa_ntf_handle()) -> ok | {error, atom()}.
finalize(Handle) ->
    verify(Handle, handle),
    call({finalize, Handle}).

%%----------------------------------------------------------------------
%% Notification Service Producer Operations
%%----------------------------------------------------------------------

% -spec notification_send(Handle, Notification) -> Result when
%       Handle :: sa_ntf_handle(),
%       Notification :: sa_ntf_object_create_delete_notification() |
% 		      sa_ntf_attribute_change_notification()     |
% 		      sa_ntf_state_change_notification()         |
% 		      sa_ntf_alarm_notification()                |
% 		      sa_ntf_security_alarm_notification(),
%       Result :: {ok, sa_ntf_identifier()} | {error, atom()}.
notification_send(Handle,
		  Notification) ->
    verify(Handle, handle),
    verify(Notification, notification),
    UpdatedNotification = update_notification(Notification),
    call({notification_send, Handle, UpdatedNotification}).

%%====================================================================
%% Notification Service Consumer Operations
%%====================================================================

% -spec notification_subscribe(Handle,
% 			     NotificationTypeFilters,
% 			     SubscriptionId) -> Result when
%       Handle :: sa_ntf_handle(),
%       NotificationTypeFilters :: sa_ntf_notification_type_filters(),
%       SubscriptionId :: sa_ntf_subscription_id(),
%       Result :: ok | {error, atom()}.
notification_subscribe(Handle,
		       NotificationTypeFilters,
		       SubscriptionId) ->
    verify(Handle, handle),
    verify(NotificationTypeFilters, notificationtypefilters),
    verify(SubscriptionId, subscriptionid),
    call({notification_subscribe, Handle, NotificationTypeFilters,
	  SubscriptionId}).

% -spec notification_unsubscribe(Handle,
% 			       SubscriptionId) -> Result when
%       Handle :: sa_ntf_handle(),
%       SubscriptionId :: sa_ntf_subscription_id(),
%       Result :: ok | {error, atom()}.
notification_unsubscribe(Handle, SubscriptionId) ->
    verify(Handle, handle),
    verify(SubscriptionId, subscriptionid),
    call({notification_unsubscribe, Handle, SubscriptionId}).

% -spec notification_read_initialize(Handle,
% 				   SearchCriteria,
% 				   NotificationTypeFilters) -> Result when
%       Handle :: sa_ntf_handle(),
%       SearchCriteria :: sa_ntf_search_criteria(),
%       NotificationTypeFilters :: sa_ntf_notification_type_filters(),
%       Result :: {ok, ReadHandle} | {error, atom()},
%       ReadHandle :: sa_ntf_read_handle() .
notification_read_initialize(Handle,
			     SearchCriteria,
			     NotificationTypeFilters) ->
    verify(Handle, handle),
    verify(SearchCriteria, searchcriteria),
    verify(NotificationTypeFilters, notificationtypefilters),
    call({notification_read_initialize, Handle, SearchCriteria,
	  NotificationTypeFilters}).

% -spec notification_read_next(ReadHandle, SearchDirection) -> Result when
%       ReadHandle :: sa_ntf_read_handle(),
%       SearchDirection :: sa_ntf_search_direction(),
%       Result :: {ok, Notification} | {error, atom()},
%       Notification :: sa_ntf_notification().
notification_read_next(ReadHandle, SearchDirection) ->
    verify(ReadHandle, handle),
    verify(SearchDirection, searchdirection),
    call({notification_read_next, ReadHandle, SearchDirection}).

% -spec notification_read_finalize(ReadHandle) -> Result when
%       ReadHandle :: sa_ntf_read_handle(),
%       Result :: ok | {error, atom()}.
notification_read_finalize(ReadHandle) ->
    verify(ReadHandle, handle),
    call({notification_read_finalize, ReadHandle}).


%%====================================================================
%% Tracing
%%====================================================================
trace_groups() -> [life_cycle, producer, consumer, error].

trace_points_list() ->
    [
     {error,
      [{safs_error, 2}]},
     {life_cycle,
      [{initialize, 2},
       {finalize, 1}]},
     {producer,
      [{notification_send, 2}]},
     {consumer,
      [{notification_subscribe, 3},
       {notification_unsubscribe, 2},
       {notification_read_initialize, 3},
       {notification_read_next, 2},
       {notification_read_finalize, 1}]}
    ].

callbacks_initialize(Handle) ->
    call({callbacks_initialize, Handle, self()}).

%%--------------------------------------------------------------------
%% Function: safs_error/2
%% Description:
%%--------------------------------------------------------------------
safs_error(_F, _A) ->  ok.

%%====================================================================
%% Internal functions
%%====================================================================

%%----------------------------------------------------------------------
%% @private
%%----------------------------------------------------------------------
call(Request) ->
    gen_server:call(?SERVER, Request, infinity).

%%----------------------------------------------------------------------
%% @private
%%----------------------------------------------------------------------
validate_version(#safsVersion{releaseCode=?NTF_RELEASE_CODE,
			      majorVersion=?NTF_MAJOR_VERSION} = Version) ->
    {ok, Version#safsVersion{minorVersion=?NTF_MINOR_VERSION}};
validate_version(_Version) ->
    {error, sa_ais_err_version}.

verify(_,_) ->
    %% Temporary workaround
    ok.

%% verify(Callbacks, callbacks) when is_function(Callbacks, 1);
%% 				  is_atom(Callbacks) ->
%%     ok;
%% verify(#safsNtfCallbacks{saNtfNotificationCallback = CB,
%% 			 saNtfNotificationDiscardedCallback = DCB},
%%        callbacks) when (is_function(CB, 1) orelse is_atom(CB)) andalso
%% 		       (is_function(DCB, 1) orelse is_atom(DCB)) ->
%%     ok;
%% verify(Callbacks, callbacks) when not is_record(Callbacks, safsNtfCallbacks) ->
%%     Reason = {bad_callbacks_value, [{value, Callbacks}]},
%%     erlang:error({ntf_type_error, Reason});

%% verify(Version, version) when not is_record(Version, safsVersion) ->
%%     Reason = {bad_version_value, [{value, Version}]},
%%     erlang:error({ntf_type_error, Reason});

%% verify(Handle, handle) when Handle >= 0,
%% 			    Handle =< 18446744073709551615 ->
%%     ok;
%% verify(Handle, handle) ->
%%     Reason = {bad_handle_value, [{value, Handle}]},
%%     erlang:error({ntf_type_error, Reason});

%% verify(Notification, notification) when not is_record(Notification,
%% 						      safsNtfAlarmNotification),
%% 					not is_record(Notification,
%% 						      safsNtfSecurityAlarmNotification),
%% 					not is_record(Notification,
%% 						      safsNtfStateChangeNotification),
%% 					not is_record(Notification,
%% 						      safsNtfAttributeChangeNotification),
%% 					not is_record(Notification,
%% 						      safsNtfObjectCreateDeleteNotification) ->
%%     Reason = {bad_handle_value, [{value, Notification}]},
%%     erlang:error({ntf_type_error, Reason});

%% verify(NotificationTypeFilters,
%%        notificationtypefilters) when not is_record(NotificationTypeFilters,
%% 						   safsNtfNotificationTypeFilters) ->
%%     Reason = {bad_notification_type_filters_value,
%% 	      [{value, NotificationTypeFilters}]},
%%     erlang:error({ntf_type_error, Reason});

%% verify(SubscriptionId, subscriptionid) when SubscriptionId >= 0,
%% 					    SubscriptionId =< 4294967295 ->
%%     ok;
%% verify(SubscriptionId, subscriptionid) ->
%%     Reason = {bad_subscriptionid_value, [{value, SubscriptionId}]},
%%     erlang:error({ntf_type_error, Reason});

%% verify(?SA_NTF_SEARCH_OLDER,   searchdirection) -> ok;
%% verify(?SA_NTF_SEARCH_YOUNGER, searchdirection) -> ok;
%% verify(SearchDirection,        searchdirection) ->
%%     Reason = {bad_search_direction_value, [{value, SearchDirection}]},
%%     erlang:error({ntf_type_error, Reason});

%% verify(?SA_NTF_SEARCH_BEFORE_OR_AT_TIME, searchcriteria) -> ok;
%% verify(?SA_NTF_SEARCH_AT_TIME,           searchcriteria) -> ok;
%% verify(?SA_NTF_SEARCH_AT_OR_AFTER_TIME,  searchcriteria) -> ok;
%% verify(?SA_NTF_SEARCH_BEFORE_TIME,       searchcriteria) -> ok;
%% verify(?SA_NTF_SEARCH_AFTER_TIME,        searchcriteria) -> ok;
%% verify(?SA_NTF_SEARCH_NOTIFICATION_ID,   searchcriteria) -> ok;
%% verify(?SA_NTF_SEARCH_ONLY_FILTER,       searchcriteria) -> ok;
%% verify(SearchCriteria,                   searchcriteria) ->
%%     Reason = {bad_search_criteria_value, [{value, SearchCriteria}]},
%%     erlang:error({ntf_type_error, Reason});

%% verify(Arg, _Type) ->
%%     try
%% 	ntf:verify_msg(Arg)
%%     catch
%% 	error:{gpb_type_error, Reason} ->
%% 	    erlang:error({ntf_type_error, Reason})
%%     end.

update_notification(Notification) ->
    NotificationHeader =
	if
	    is_record(Notification, safsNtfObjectCreateDeleteNotification) ->
		Notification#safsNtfObjectCreateDeleteNotification.notificationHeader;
	    is_record(Notification, safsNtfAttributeChangeNotification) ->
		Notification#safsNtfAttributeChangeNotification.notificationHeader;
	    is_record(Notification, safsNtfStateChangeNotification) ->
		Notification#safsNtfStateChangeNotification.notificationHeader;
	    is_record(Notification, safsNtfAlarmNotification) ->
		Notification#safsNtfAlarmNotification.notificationHeader;
	    is_record(Notification, safsNtfSecurityAlarmNotification) ->
		Notification#safsNtfSecurityAlarmNotification.notificationHeader
	end,

    NewEventTime = update_eventtime(NotificationHeader),
    NewNotificationHeader =
	NotificationHeader#safsNtfNotificationHeader{eventTime = NewEventTime},

    if
	is_record(Notification, safsNtfObjectCreateDeleteNotification) ->
	    Notification#safsNtfObjectCreateDeleteNotification{notificationHeader = NewNotificationHeader};
	is_record(Notification, safsNtfAttributeChangeNotification) ->
	    Notification#safsNtfAttributeChangeNotification{notificationHeader = NewNotificationHeader};
	is_record(Notification, safsNtfStateChangeNotification) ->
	    Notification#safsNtfStateChangeNotification{notificationHeader = NewNotificationHeader};
	is_record(Notification, safsNtfAlarmNotification) ->
	    Notification#safsNtfAlarmNotification{notificationHeader = NewNotificationHeader};
	is_record(Notification, safsNtfSecurityAlarmNotification) ->
	    Notification#safsNtfSecurityAlarmNotification{notificationHeader = NewNotificationHeader}
    end.

update_eventtime(#safsNtfNotificationHeader{eventTime = ?SA_TIME_UNKNOWN}) ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    ?SA_TIME_ONE_SECOND*(1000000*MegaSecs+Secs) +
	?SA_TIME_ONE_MICROSECOND*MicroSecs;
update_eventtime(#safsNtfNotificationHeader{eventTime = EventTime}) ->
    EventTime.
