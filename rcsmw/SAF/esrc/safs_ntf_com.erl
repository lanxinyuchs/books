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
%% File: safs_ntf_com.erl
%%
%% Description:
%%    This file implements the interface that handles incoming
%%    NTF requests on the sockets from C.
%%
%%--------------------------------------------------------------------
-module(safs_ntf_com).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("ntf.hrl").
-include("safs_internal.hrl").
-include("safs_ais.hrl").
-include("safs_ntf.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         message/2,
	 init/2,
         callback_message_async/3,
         close/1,
	 trace_groups/0,
	 trace_points_list/0
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 safs_error/2 %% Temp fix to get dialyzer happy
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init/2
%% Description:
%%--------------------------------------------------------------------
init(_Proxy, _Cb) ->
    {ok, undefined}.

%%--------------------------------------------------------------------
%% Function: message/2
%% Description:
%%--------------------------------------------------------------------
message(Bytes, State) ->
    case catch ntf:decode_msg(Bytes, safsNtfMessage) of
	#safsNtfMessage{} = Msg ->
            {result_msg(Msg), State};
        _Error ->
            error_logger:format("~p~p couldn't decode message:\n\t~p\n",
                                [?MODULE, self(), Bytes]),
            {{error, bad_message}, State}
    end.

%%--------------------------------------------------------------------
%% Function: callback_message_async/4
%% Description:
%%--------------------------------------------------------------------
callback_message_async(Pid, Cb, Arg) ->
    case encode_callback_msg(Cb, Arg) of
        {ok, Bin} ->
            safs_com_inproxy:send(Pid, Bin);
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% Function: close/1
%% Description:
%%--------------------------------------------------------------------
close(Proxy) ->
    safs_com_inproxy:stop(Proxy).

%%====================================================================
%% Tracing
%%====================================================================
trace_groups() -> [error].

trace_points_list() ->
    [
     {error,
      [{safs_error, 2}]}
    ].

%%--------------------------------------------------------------------
%% Function: safs_error/2
%% Description:
%%--------------------------------------------------------------------
safs_error(_F, _A) ->  ok.

%%====================================================================
%% Internal functions
%%====================================================================
result_msg(#safsNtfMessage{initialize = Initialize}) when Initialize =/= undefined ->
    R = initialize_ret(Initialize),

    ntf:encode_msg(R);
result_msg(#safsNtfMessage{finalize = Finalize}) when Finalize =/= undefined ->
    R = finalize_ret(Finalize),

    ntf:encode_msg(R);
result_msg(#safsNtfMessage{notificationSend = NotSend}) when NotSend =/= undefined ->
    R = notificationSend_ret(NotSend),

    ntf:encode_msg(R);
result_msg(#safsNtfMessage{notificationSubscribe = NotS}) when NotS =/= undefined ->
    R = notificationSubscribe_ret(NotS),

    ntf:encode_msg(R);
result_msg(#safsNtfMessage{notificationUnsubscribe = NotU}) when NotU =/= undefined ->
    R = notificationUnsubscribe_ret(NotU),

    ntf:encode_msg(R);
result_msg(#safsNtfMessage{notificationReadInitialize = NotRI}) when NotRI =/= undefined ->
    R = notificationReadInitialize_ret(NotRI),

    ntf:encode_msg(R);
result_msg(#safsNtfMessage{notificationReadNext = NotRN}) when NotRN =/= undefined ->
    R = notificationReadNext_ret(NotRN),

    ntf:encode_msg(R);
result_msg(#safsNtfMessage{notificationReadFinalize = NotRF}) when NotRF =/= undefined ->
    R = notificationReadFinalize_ret(NotRF),

    ntf:encode_msg(R);
result_msg(#safsNtfMessage{callbacksInitialize = CI}) when CI =/= undefined ->
    R = callbacksInitialize_ret(CI),

    ntf:encode_msg(R).

initialize_ret(Initialize) ->
    Version = Initialize#safsNtfInitialize.version,
    case safs_ntf:initialize(Initialize#safsNtfInitialize.callbacks,
			     Version) of
	{ok, Handle, SupportedVersion} ->
	    #safsNtfInitializeRet{returnVal = sa_ais_ok,
				  handle = Handle,
				  version = SupportedVersion};
	{error, Error} ->
	    #safsNtfInitializeRet{returnVal = Error,
				  handle = 0,
				  version = Version}
    end.

finalize_ret(Finalize) ->
    case safs_ntf:finalize(Finalize#safsNtfFinalize.handle) of
	ok ->
	    #safsNtfFinalizeRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsNtfFinalizeRet{returnVal = Error}
    end.

notificationSend_ret(NotSend) ->
    Notification =
	get_notification(NotSend#safsNtfNotificationSend.notification),
    case safs_ntf:notification_send(NotSend#safsNtfNotificationSend.handle,
				    Notification) of
	{ok, Identifier} ->
	    #safsNtfNotificationSendRet{returnVal = sa_ais_ok,
					identifier = Identifier};
	{error, Error} ->
	    #safsNtfNotificationSendRet{returnVal = Error,
					identifier = 0}
    end.

notificationSubscribe_ret(NotS) ->
    case safs_ntf:notification_subscribe(NotS#safsNtfNotificationSubscribe.handle,
					 NotS#safsNtfNotificationSubscribe.notificationTypeFilters,
					 NotS#safsNtfNotificationSubscribe.subscriptionId) of
	ok ->
	    #safsNtfNotificationSubscribeRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsNtfNotificationSubscribeRet{returnVal = Error}
    end.

notificationUnsubscribe_ret(NotU) ->
    case safs_ntf:notification_unsubscribe(NotU#safsNtfNotificationUnsubscribe.handle,
					   NotU#safsNtfNotificationUnsubscribe.subscriptionId) of
	ok ->
	    #safsNtfNotificationUnsubscribeRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsNtfNotificationUnsubscribeRet{returnVal = Error}
    end.

notificationReadInitialize_ret(NotRI) ->
    case safs_ntf:notification_read_initialize(NotRI#safsNtfNotificationReadInitialize.handle,
					       NotRI#safsNtfNotificationReadInitialize.searchCriteria,
					       NotRI#safsNtfNotificationReadInitialize.notificationTypeFilters) of
	{ok, ReadHandle} ->
	    #safsNtfNotificationReadInitializeRet{returnVal  = sa_ais_ok,
						  readHandle = ReadHandle};
	{error, Error} ->
	    #safsNtfNotificationReadInitializeRet{returnVal = Error}
    end.

notificationReadNext_ret(NotRN) ->
    case safs_ntf:notification_read_next(NotRN#safsNtfNotificationReadNext.readHandle,
					 NotRN#safsNtfNotificationReadNext.searchDirection) of
	{ok, Notification} ->
	    #safsNtfNotificationReadNextRet{returnVal    = sa_ais_ok,
					    notification = Notification};
	{error, Error} ->
	    #safsNtfNotificationReadNextRet{returnVal = Error}
    end.

notificationReadFinalize_ret(NotRF) ->
    case safs_ntf:notification_read_finalize(NotRF#safsNtfNotificationReadFinalize.readHandle) of
	ok ->
	    #safsNtfNotificationReadFinalizeRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsNtfNotificationReadFinalizeRet{returnVal = Error}
    end.

callbacksInitialize_ret(CI) ->
    case safs_ntf:callbacks_initialize(CI#safsNtfCallbacksInitialize.handle) of
	ok ->
	    #safsNtfCallbacksInitializeRet{returnVal = sa_ais_ok};
	{error, Error} ->
	    #safsNtfCallbacksInitializeRet{returnVal = Error}
    end.

get_notification(#safsNtfNotification{objectCreateDeleteNotification = Not}) when Not =/= undefined ->
    Not;
get_notification(#safsNtfNotification{attributeChangeNotification = Not}) when Not =/= undefined ->
    Not;
get_notification(#safsNtfNotification{stateChangeNotification = Not}) when Not =/= undefined ->
    Not;
get_notification(#safsNtfNotification{alarmNotification = Not}) when Not =/= undefined ->
    Not;
get_notification(#safsNtfNotification{securityAlarmNotification = Not}) when Not =/= undefined ->
    Not.

%%--------------------------------------------------------------------
%% Function: encode_callback_msg/3
%% Description:
%%--------------------------------------------------------------------
%% @private
encode_callback_msg(Cb, Arg) ->
    try
        Bin = encode_callback(Cb, Arg),
        {ok, Bin}
    catch
        _: Reason ->
            ?ERROR("Failed to encode NTF ~w Callback message. Arg = ~p~n"
                   "Reason: ~p~n"
                   "Stack:~n~p~n",
                   [Cb, Arg, Reason, erlang:get_stacktrace()]),
            {error, sa_ais_err_invalid_param}
    end.

encode_callback(notification, {SubscriptionId, Notification}) ->
    Cb = #saNtfNotificationCallback{subscriptionId = SubscriptionId,
				    notification   = Notification},
    CbMsg = #'SaNtfCallbacks'{notificationCallback = Cb},
    encode_callback(CbMsg);
encode_callback(notification_discarded,
		{SubscriptionId, NotificationType, DiscardedNotificationIds}) ->
    Cb = #saNtfNotificationDiscardedCallback{subscriptionId = SubscriptionId,
					     notificationType = NotificationType,
					     discardedNotificationIdentifiers = DiscardedNotificationIds},
    CbMsg = #'SaNtfCallbacks'{notificationDiscardedCallback = Cb},
    encode_callback(CbMsg).

encode_callback(CbMsg) ->
    ntf:encode_msg(CbMsg).
