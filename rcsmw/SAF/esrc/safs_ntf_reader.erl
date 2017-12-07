%%----------------------------------------------------------------------
%%
%% %EricssonCopyright%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
%%
%% The program may be used and/or copied only with the written permission from
%% Ericsson AB, or in accordance with the terms and conditions stipulated in
%% the agreement/contract under which the program has been supplied.
%%
%% %CopyrightEnd%
%%
%%--------------------------------------------------------------------
%% File: safs_ntf_db.erl
%%
%% Description:
%%
%%--------------------------------------------------------------------
-module(safs_ntf_reader).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("safs_ais.hrl").
-include("safs_ntf.hrl").
-include("safs_ntf_db.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         start_link/0,
	 filter/2,
	 get_next/2,
         stop/1
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(NO_CACHE, undefined).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {cache, key, first}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: filter/2
%% Description:
%%--------------------------------------------------------------------
filter(Pid, ReadHandle) ->
    call(Pid, {filter, ReadHandle}).

%%--------------------------------------------------------------------
%% Function: get_next/2
%% Description:
%%--------------------------------------------------------------------
get_next(Pid, Direction) ->
    call(Pid, {get_next, Direction}).

%%--------------------------------------------------------------------
%% Function: stop/0
%% Description: Stop the server
%%--------------------------------------------------------------------
stop(Pid) ->
    call(Pid, stop).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({filter, Handle}, _From, State) ->
    Reader = safs_ntf_srv:lookup_tbl(safs_ntf_reader, Handle),

    SearchCriteria = Reader#safs_ntf_reader.search_criteria,
    MS = get_match_spec(SearchCriteria,
			Reader#safs_ntf_reader.notification_type_filters),

    CacheData = safs_ntf_db:search_sent_notification(MS),
    Cache = create_cache(CacheData),

    Key = get_key(Cache, SearchCriteria),
    {reply, ok, State#state{cache = Cache,
			    key = Key,
			    first = true}};
handle_call({get_next, _Direction}, _From,
	    #state{cache = ?NO_CACHE} = State) ->
    Reply = {error, sa_ais_err_not_exist},
    {reply, Reply, State};
handle_call({get_next, _Direction}, _From,
	    #state{key = '$end_of_table'} = State) ->
    Reply = {error, sa_ais_err_not_exist},
    {reply, Reply, State};
handle_call({get_next, ?SA_NTF_SEARCH_OLDER}, _From,
	    #state{cache = Cache,
		   key = Key,
		   first = false} = State) ->
    case ets:prev(Cache, Key) of
	'$end_of_table' ->
	    Reply = {error, sa_ais_err_not_exist},
	    {reply, Reply, State};
	NextKey ->
	    Reply = {ok, get_notification(Cache, NextKey)},
	    {reply, Reply, State#state{key = NextKey}}
    end;
handle_call({get_next, ?SA_NTF_SEARCH_YOUNGER}, _From,
	    #state{cache = Cache,
		   key = Key,
		   first = false} = State) ->
    case ets:next(Cache, Key) of
	'$end_of_table' ->
	    Reply = {error, sa_ais_err_not_exist},
	    {reply, Reply, State};
	NextKey ->
	    Reply = {ok, get_notification(Cache, NextKey)},
	    {reply, Reply, State#state{key = NextKey}}
    end;

handle_call({get_next, _Direction}, _From,
	    #state{cache = Cache,
		   key = Key,
		   first = true} = State) ->
    {reply, {ok, get_notification(Cache, Key)}, State#state{first = false}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Msg, From, #state{} = State) ->
    error_logger:format("~p~p got unexpected call from ~p:\n\t~p\n",
                        [?MODULE, self(), From, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, #state{} = State) ->
    error_logger:format("~p~p got unexpected cast:\n\t~p\n",
                        [?MODULE, self(), Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Msg, #state{} = State) ->
    error_logger:format("~p~p got unexpected message:\n\t~p\n",
                        [?MODULE, self(), Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change/3
%% Description: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%%----------------------------------------------------------------------
%% @private
%%----------------------------------------------------------------------
call(Pid, Request) ->
    gen_server:call(Pid, Request, infinity).

get_notification(Cache, Key) ->
    [Notification] = ets:lookup(Cache, Key),
    Notification#ntf_sent_notification.notification.

get_match_spec(SearchCriteria,
	       #safsNtfNotificationTypeFilters{objectCreateDeleteNotificationFilter = F1,
					       attributeChangeNotificationFilter = F2,
					       stateChangeNotificationFilter = F3,
					       alarmNotificationFilter = F4,
					       securityAlarmNotificationFilter = F5}) ->
    MS =
	lists:foldl(
	  fun(undefined, Acc) ->
		  Acc;
	     (Filter, Acc) ->
		  [get_match_spec(SearchCriteria, Filter)|Acc]
	  end, [], [F1, F2, F3, F4, F5]),

    case MS of
	[] -> error;
	_ -> MS
    end;

get_match_spec(SearchCriteria,
	       Filter) when is_record(Filter, safsNtfObjectCreateDeleteNotificationFilter) ->
    FilterHeader =
	Filter#safsNtfObjectCreateDeleteNotificationFilter.notificationFilterHeader,

    {C1, C2, C3, C4, C5} = common_conditions(SearchCriteria, FilterHeader),
    C6 = 'orelse'('$6', Filter#safsNtfObjectCreateDeleteNotificationFilter.sourceIndicators),

    MatchConditions =
	case 'andalso'([C1, C2, C3, C4, C5, C6]) of
	    {} -> [];
	    MC -> [MC]
	end,

    NotificationHeader =
	#safsNtfNotificationHeader{eventType = '$2',
				   notificationObject = '$3',
				   notifyingObject = '$4',
				   notificationClassId = '$5',
				   _ = '_'},
    ObjectCreateDeleteNotification =
	#safsNtfObjectCreateDeleteNotification{notificationHeader = NotificationHeader,
					       sourceIndicator = '$6',
					       _ = '_'},

    MatchHead = #ntf_sent_notification{time_id = {'$1', '_'},
				       notification = ObjectCreateDeleteNotification},
    MatchBody = ['$_'],

    {MatchHead, MatchConditions, MatchBody};

get_match_spec(SearchCriteria,
	       Filter) when is_record(Filter, safsNtfAttributeChangeNotificationFilter) ->
    FilterHeader =
	Filter#safsNtfAttributeChangeNotificationFilter.notificationFilterHeader,

    {C1, C2, C3, C4, C5} = common_conditions(SearchCriteria, FilterHeader),
    C6 = 'orelse'('$6', Filter#safsNtfAttributeChangeNotificationFilter.sourceIndicators),

    MatchConditions =
	case 'andalso'([C1, C2, C3, C4, C5, C6]) of
	    {} -> [];
	    MC -> [MC]
	end,

    NotificationHeader =
	#safsNtfNotificationHeader{eventType = '$2',
				   notificationObject = '$3',
				   notifyingObject = '$4',
				   notificationClassId = '$5',
				   _ = '_'},
    AttributeChangeNotification =
	#safsNtfAttributeChangeNotification{notificationHeader = NotificationHeader,
					    sourceIndicator = '$6',
					    _ = '_'},

    MatchHead = #ntf_sent_notification{time_id = {'$1', '_'},
				       notification = AttributeChangeNotification},
    MatchBody = ['$_'],

    {MatchHead, MatchConditions, MatchBody};

get_match_spec(SearchCriteria,
	       Filter) when is_record(Filter, safsNtfStateChangeNotificationFilter) ->
    FilterHeader =
	Filter#safsNtfStateChangeNotificationFilter.notificationFilterHeader,

    {C1, C2, C3, C4, C5} = common_conditions(SearchCriteria, FilterHeader),
    C6 = 'orelse'('$6', Filter#safsNtfStateChangeNotificationFilter.sourceIndicators),
    C7 = 'orelse'('$7',
		  Filter#safsNtfStateChangeNotificationFilter.changedStates),

    MatchConditions =
	case 'andalso'([C1, C2, C3, C4, C5, C6, C7]) of
	    {} -> [];
	    MC -> [MC]
	end,

    NotificationHeader =
	#safsNtfNotificationHeader{eventType = '$2',
				   notificationObject = '$3',
				   notifyingObject = '$4',
				   notificationClassId = '$5',
				   _ = '_'},
    StateChangeNotification =
	#safsNtfStateChangeNotification{notificationHeader = NotificationHeader,
					sourceIndicator = '$6',
					changedStates = '$7'},

    MatchHead = #ntf_sent_notification{time_id = {'$1', '_'},
				       notification = StateChangeNotification},
    MatchBody = ['$_'],

    {MatchHead, MatchConditions, MatchBody};

get_match_spec(SearchCriteria,
	       Filter) when is_record(Filter, safsNtfAlarmNotificationFilter) ->
    FilterHeader =
	Filter#safsNtfAlarmNotificationFilter.notificationFilterHeader,

    {C1, C2, C3, C4, C5} = common_conditions(SearchCriteria, FilterHeader),
    C6 = 'orelse'('$6', Filter#safsNtfAlarmNotificationFilter.probableCauses),
    C7 = 'orelse'('$7',
		  Filter#safsNtfAlarmNotificationFilter.perceivedSeverities),
    C8 = 'orelse'('$8', Filter#safsNtfAlarmNotificationFilter.trends),

    MatchConditions =
	case 'andalso'([C1, C2, C3, C4, C5, C6, C7, C8]) of
	    {} -> [];
	    MC -> [MC]
	end,

    NotificationHeader =
	#safsNtfNotificationHeader{eventType = '$2',
				   notificationObject = '$3',
				   notifyingObject = '$4',
				   notificationClassId = '$5',
				   _ = '_'},
    AlarmNotification =
	#safsNtfAlarmNotification{notificationHeader = NotificationHeader,
				  probableCause = '$6',
				  perceivedSeverity = '$7',
				  trend = '$8',
				  _ = '_'},

    MatchHead = #ntf_sent_notification{time_id = {'$1', '_'},
				       notification = AlarmNotification},
    MatchBody = ['$_'],

    {MatchHead, MatchConditions, MatchBody};

get_match_spec(SearchCriteria,
	       Filter) when is_record(Filter, safsNtfSecurityAlarmNotificationFilter) ->
    FilterHeader =
	Filter#safsNtfSecurityAlarmNotificationFilter.notificationFilterHeader,

    {C1, C2, C3, C4, C5} = common_conditions(SearchCriteria, FilterHeader),
    C6 = 'orelse'('$6', Filter#safsNtfSecurityAlarmNotificationFilter.probableCauses),
    C7 = 'orelse'('$7',
		  Filter#safsNtfSecurityAlarmNotificationFilter.severities),
    C8 = 'orelse'('$8', Filter#safsNtfSecurityAlarmNotificationFilter.securityAlarmDetectors),
    C9 = 'orelse'('$9', Filter#safsNtfSecurityAlarmNotificationFilter.serviceUsers),
    C10 = 'orelse'('$9', Filter#safsNtfSecurityAlarmNotificationFilter.serviceProviders),

    MatchConditions =
	case 'andalso'([C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]) of
	    {} -> [];
	    MC -> [MC]
	end,

    NotificationHeader =
	#safsNtfNotificationHeader{eventType = '$2',
				   notificationObject = '$3',
				   notifyingObject = '$4',
				   notificationClassId = '$5',
				   _ = '_'},
    SecurityAlarmNotification =
	#safsNtfSecurityAlarmNotification{notificationHeader = NotificationHeader,
					  probableCause = '$6',
					  severity = '$7',
					  securityAlarmDetector = '$8',
					  serviceUser = '$9',
					  serviceProvider = '$10'
					 },

    MatchHead = #ntf_sent_notification{time_id = {'$1', '_'},
				       notification = SecurityAlarmNotification},
    MatchBody = ['$_'],

    {MatchHead, MatchConditions, MatchBody}.

common_conditions(SearchCriteria, FilterHeader) ->
    {time_condition(SearchCriteria),
     'orelse'('$2', FilterHeader#safsNtfNotificationFilterHeader.eventTypes),
     'orelse'('$3',
	      FilterHeader#safsNtfNotificationFilterHeader.notificationObjects),
     'orelse'('$4',
	      FilterHeader#safsNtfNotificationFilterHeader.notifyingObjects),
     'orelse'('$5',
	      FilterHeader#safsNtfNotificationFilterHeader.notificationClassIds)}.

time_condition(#safsNtfSearchCriteria{searchMode = ?SA_NTF_SEARCH_BEFORE_OR_AT_TIME,
				      eventTime = EventTime}) ->
    {'=<', '$1', EventTime};
time_condition(#safsNtfSearchCriteria{searchMode = ?SA_NTF_SEARCH_AT_TIME,
				      eventTime = EventTime}) ->
    {'=:=', '$1', EventTime};
time_condition(#safsNtfSearchCriteria{searchMode = ?SA_NTF_SEARCH_AT_OR_AFTER_TIME,
				      eventTime = EventTime}) ->
    {'>=', '$1', EventTime};
time_condition(#safsNtfSearchCriteria{searchMode = ?SA_NTF_SEARCH_BEFORE_TIME,
				      eventTime = EventTime}) ->
    {'<', '$1', EventTime};
time_condition(#safsNtfSearchCriteria{searchMode = ?SA_NTF_SEARCH_AFTER_TIME,
				      eventTime = EventTime}) ->
    {'>', '$1', EventTime};
time_condition(#safsNtfSearchCriteria{searchMode = ?SA_NTF_SEARCH_NOTIFICATION_ID}) ->
    {};
time_condition(#safsNtfSearchCriteria{searchMode = ?SA_NTF_SEARCH_ONLY_FILTER}) ->
    {}.

'orelse'(_MatchVariable, []) ->
    {};
'orelse'(MatchVariable, [H|T]) ->
    O = {'=:=', MatchVariable, H},
    case 'orelse'(MatchVariable, T) of
	{} -> O;
	R -> {'orelse', O, R}
    end.

'andalso'([]) ->
    {};
'andalso'([H|T]) ->
    case 'andalso'(T) of
	{} -> H;
	R -> {'andalso', H, R}
    end.

create_cache([]) ->
    ?NO_CACHE;
create_cache(CacheData) ->
    Cache = ets:new(safs_ntf_reader_cache,
		    [ordered_set, {keypos, #ntf_sent_notification.time_id}]),
    ets:insert(Cache, CacheData),
    Cache.

get_key(?NO_CACHE, _SearchCriteria) ->
    '$end_of_table';
get_key(Cache,
	#safsNtfSearchCriteria{searchMode = ?SA_NTF_SEARCH_NOTIFICATION_ID,
			       notificationId = Id}) ->
    get_key(Cache, Id, ets:first(Cache));
get_key(Cache, _SearchCriteria) ->
    ets:first(Cache).

get_key(_Cache, _Id, '$end_of_table') ->
    '$end_of_table';
get_key(Cache, Id, Key) ->
    case ets:lookup(Cache, Key) of
	[#ntf_sent_notification{time_id = {_, Id}}] ->
	    Key;
	[#ntf_sent_notification{}] ->
	    get_key(Cache, Id, ets:next(Cache, Key))
    end.
