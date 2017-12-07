%%% #0.    BASIC INFORMATION
%%%-------------------------------------------------------------------
%%% %CCaseFile:	comsaNtfSubscriber.erl %
%%% @author erarafo
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R8A/2
%%%
%%% @doc == COMSA NTF subscriber ==
%%% This module acts as a subscriber to the NTF notifications.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(comsaNtfSubscriber).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/2').
-author(etxpeno).

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R2A/1      2013-04-03 etxpeno     Created
%%% R2A/2      2013-11-12 erarafo     Support for additional text in 'clear'
%%% R2A/3      2014-01-29 erarafo     Pre-release of IMM AVC
%%% R2A/6      2014-02-10 erarafo     Pre-release yet again
%%% R2A/7      2014-02-11 erarafo     Moved handling of AVC out
%%% R2A/8      2014-03-13 etxpeno     Remove call to safs_imm_special_applier:test_reg/0
%%% R2A/9      2014-05-23 uabesvi     log alarm from safs_log
%%% R2A/11     2014-07-07 etxjotj     HS75386 Split dn only on comma
%%% R2A/12     2014-08-21 erarafo     Edoc only
%%% R2A/13     2014-08-21 erarafo     Adaptation to current COMTE/COM
%%% R2A/14     2014-08-21 erarafo     Identical to R2A/11 (CNX9012610-R2A322), which is
%%%                                   safe to use for the RFA. After RFA step up to
%%%                                   R2A/13 again.
%%% R2A/15     2014-08-25 erarafo     Equal to R2A/13 with some cleanup
%%% R2A/16     2014-08-26 erarafo     Corrected typing
%%% R2A/17     2014-08-29 erarafo     Accepting 3GPP DNs in notification object
%%% R2A/18     2014-09-10 erarafo     Logging alarms; catching ill-formed data
%%% R3A/1      2014-09-30 erarafo     Support for Additional Info
%%% R3A/2      2014-09-30 erarafo     Workaround for LDAP_NAME in Additional Info
%%% R3A/3      2014-10-01 erarafo     Safe validation of LDAP_NAME
%%% R3A/5      2014-11-14 erarafo     Status LED functionality
%%% R3A/6      2014-11-14 erarafo     Minor adjustment
%%% R3A/7      2014-11-16 erarafo     Turn off polling before upgrade restart
%%% R3A/8      2014-11-18 erarafo     Using eqs_vii.hrl
%%% R3A/9      2014-11-19 erarafo     Ignore "open door" alarm for Status LED
%%% R3A/10     2014-11-21 erarafo     Minor cleanup.
%%% R3A/11     2015-01-14 etxpeno     Support for regular role
%%% R3A/12     2015-02-17 erarafo     Cleanup after Status LED
%%% R5A/2-3    2016-03-13 uabesvi     HU64737, flush the queue if too long
%%% R6A/1      2016-06-12 erarafo     Fixed typo in message
%%% R8A/1      2016-12-22 etxpeno     Using official GMF interface
%%% R8A/2      2017-01-09 erarafo     HV46949
%%% ----------------------------------------------------------

%% API

-export([start_link/0]).

%% Called by safs_ntf.
-export([notification_callback/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("safs_ntf.hrl").
-include("safs_ais.hrl").


-define(SERVER, ?MODULE).
-define(ALARM_HANDLER_MODULE, comsaAlarm).
-define(AVC_HANDLER_MODULE, comsaEvent).

-define(NtfVersion, #safsVersion{releaseCode = $A,
                                 majorVersion = 1,
                                 minorVersion = 1}).
-define(NtfLogSubscriptionId, 1).
-define(NtfAlarmSubscriptionId, 2).
-define(NtfAvcSubscriptionId, 3).

-define(MAX_MSG_QUEUE_LENGTH, 100).

-record(state,
	{log_handle,
	 alarm_handle,
	 avc_handle,
	 flush = 0     %% Number of messages to be flushed.
	               %% Used when the in-queue grows rapidly.
	}).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%% ----------------------------------------------------------
%%% @doc Callback function used by SAF NTF for log and alarm
%%% notifications.
%%%
%%% NOTE: AVC notifications are handled in the
%%% comsaEvent module and alarms are handled in
%%% the comsaAlarm module.
%%% @end
%%% ----------------------------------------------------------

-spec notification_callback(integer(), any()) -> ok.

notification_callback(SubscriptionId, Notification) ->
    gen_server:cast(?SERVER,
		    {notification_callback, SubscriptionId, Notification}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{log_handle=subscribe_log(),
		alarm_handle=subscribe_alarm(),
		avc_handle=subscribe_avc()}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% Flush log messages as long flush > 0
handle_cast({notification_callback, ?NtfLogSubscriptionId, _},
	    #state{flush = Flush} = State) when Flush > 0 ->
    {noreply, State#state{flush = Flush - 1}};
%% No flushing
handle_cast({notification_callback, ?NtfLogSubscriptionId, Notification},
            State) ->
    handle_logging(Notification),
    {noreply, State#state{flush = get_flush()}};
%% Unknown message
handle_cast(_Msg,
            #state{flush = Flush} = State) ->
    {noreply, State#state{flush = maybe_dec_flush(Flush)}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.





%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    unsubscribe(State#state.log_handle, ?NtfLogSubscriptionId),
    unsubscribe(State#state.alarm_handle, ?NtfAlarmSubscriptionId),
    unsubscribe(State#state.avc_handle, ?NtfAvcSubscriptionId),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State#state{flush = 0}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

subscribe_log() ->
    Callbacks = #safsNtfCallbacks{saNtfNotificationCallback = ?MODULE},
    {ok, Handle, _} = safs_ntf:initialize(Callbacks, ?NtfVersion),

    NotificationFilterHeader = #safsNtfNotificationFilterHeader{_ = []},

    ObjectCreateDeleteNotificationFilter =
	#safsNtfObjectCreateDeleteNotificationFilter{notificationFilterHeader = NotificationFilterHeader,
						     _ = []},
    AttributeChangeNotificationFilter =
	#safsNtfAttributeChangeNotificationFilter{notificationFilterHeader = NotificationFilterHeader,
						  _ = []},
    StateChangeNotificationFilter =
	#safsNtfStateChangeNotificationFilter{notificationFilterHeader = NotificationFilterHeader,
					      _ = []},
    AlarmNotificationFilter =
        #safsNtfAlarmNotificationFilter{notificationFilterHeader = NotificationFilterHeader,
                                        _ = []},
    SecurityAlarmNotificationFilter =
	#safsNtfSecurityAlarmNotificationFilter{notificationFilterHeader = NotificationFilterHeader,
						_ = []},
    NotificationTypeFilters =
        #safsNtfNotificationTypeFilters{objectCreateDeleteNotificationFilter = ObjectCreateDeleteNotificationFilter,
					attributeChangeNotificationFilter = AttributeChangeNotificationFilter,
					stateChangeNotificationFilter = StateChangeNotificationFilter,
					alarmNotificationFilter = AlarmNotificationFilter,
					securityAlarmNotificationFilter = SecurityAlarmNotificationFilter},

    ok = safs_ntf:notification_subscribe(Handle, NotificationTypeFilters,
                                         ?NtfLogSubscriptionId),

    Handle.

subscribe_alarm() ->
    Callbacks = #safsNtfCallbacks{saNtfNotificationCallback = ?ALARM_HANDLER_MODULE},
    {ok, Handle, _} = safs_ntf:initialize(Callbacks, ?NtfVersion),

    AlarmNotificationFilterHeader = #safsNtfNotificationFilterHeader{_ = []},
    AlarmNotificationFilter =
        #safsNtfAlarmNotificationFilter{notificationFilterHeader = AlarmNotificationFilterHeader,
                                        _ = []},
    NotificationTypeFilters =
        #safsNtfNotificationTypeFilters{alarmNotificationFilter = AlarmNotificationFilter},

    ok = safs_ntf:notification_subscribe(Handle, NotificationTypeFilters,
                                         ?NtfAlarmSubscriptionId),

    Handle.

subscribe_avc() ->
    Callbacks = #safsNtfCallbacks{saNtfNotificationCallback = ?AVC_HANDLER_MODULE},
    {ok, Handle, _} = safs_ntf:initialize(Callbacks, ?NtfVersion),
    FilterHeader = #safsNtfNotificationFilterHeader{_=[]},
    A=#safsNtfObjectCreateDeleteNotificationFilter{notificationFilterHeader=FilterHeader, _=[]},
    B=#safsNtfAttributeChangeNotificationFilter{notificationFilterHeader=FilterHeader, _=[]},
    C=#safsNtfStateChangeNotificationFilter{notificationFilterHeader=FilterHeader, _=[]},

    NotificationTypeFilters =
        #safsNtfNotificationTypeFilters{
					objectCreateDeleteNotificationFilter=A,
					attributeChangeNotificationFilter=B,
					stateChangeNotificationFilter=C
					},

    ok = safs_ntf:notification_subscribe(Handle, NotificationTypeFilters, ?NtfAvcSubscriptionId),
    Handle.


unsubscribe(undefined, _SubscriptionId) ->
    ok;

unsubscribe(Handle, SubscriptionId) ->
    ok = safs_ntf:notification_unsubscribe(Handle, SubscriptionId),
    ok = safs_ntf:finalize(Handle).


handle_logging(Notification) ->
    %% How should the notification be formatted in the log
    Msg = lists:flatten(io_lib:format("~p", [Notification])),
    logI:write_log("NotificationLog", ?MODULE_STRING, info, Msg),
    ok.


%%======================================================================
%% get_flush() -> ok
%%
%% check if message queue is very long, in that case flush
%% all messages in the queue
%%======================================================================
get_flush() ->
    {message_queue_len, MessageQueueLen} =
	erlang:process_info(self(), message_queue_len),
    gf(MessageQueueLen).

gf(MessageQueueLen) when MessageQueueLen > ?MAX_MSG_QUEUE_LENGTH ->
    error_logger:info_msg("~p: notification log overloaded.~n"
			  " Flushing ~p messages.\n",
			  [?MODULE, MessageQueueLen]),
    MessageQueueLen;
gf(_) ->
    0.


maybe_dec_flush(N) when N > 0 ->
    N - 1;
maybe_dec_flush(_) ->
    0.
