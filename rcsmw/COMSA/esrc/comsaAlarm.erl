%%% #0.    BASIC INFORMATION
%%%-------------------------------------------------------------------
%%% %CCaseFile:	comsaAlarm.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2017
%%% @version /main/R8A/R11A/R12A/2
%%%
%%% @doc == comsaAlarm ==
%%% This module handles NTF alarm notifications. The subscription is set up
%%% by comsaNtfSubscriber.
%%%
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(comsaAlarm).
-behaviour(gen_server).
-vsn('/main/R8A/R11A/R12A/2').
-author(erarafo).

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% Module cloned from comsaNtfSubscriber, please see history in there
%%% R8A/1      2017-01-09 erarafo     First version; HV46949 solution
%%% R8A/2      2017-01-10 erarafo     Refactoring and comments
%%% R11A/1     2017-08-28 elarrun     Alarm sent direct to comsaServer
%%%                                   instead of through
%%% R11A/2     2017-10-19 etxpeno     (MR36328) handle program group restart
%%% R12A/1     2017-10-25 etxpeno     add program group restart callbacks
%%% R12A/2     2017-11-07 etxpeno     Improve info_report in notification_callback/2
%%% ----------------------------------------------------------

%% API

-export([start_link/0]).

%% Called by safs_ntf.
-export([notification_callback/2]).

%% Called by appm.
-export([prep_pgroup/1, pgroup_done/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ComsaAlarm.hrl").
-include("safs_ntf.hrl").
-include("safs_ais.hrl").


-define(SERVER, ?MODULE).
-define(AVC_HANDLER_MODULE, comsaEvent).

-define(NtfVersion, #safsVersion{releaseCode = $A,
                                 majorVersion = 1,
                                 minorVersion = 1}).

-define(NtfAlarmSubscriptionId, 2).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%% ----------------------------------------------------------
%%% @doc Callback function used by SAF NTF for alarm notifications.
%%%
%%% Callbacks should occur at a low rate. Each call is logged.
%%%
%%% NOTE: AVC notifications are handled by comsaEvent and log notifications
%%% are handled by comsaNtfSubscriber. All subscriptions are set up by
%%% comsaNtfSubscriber.
%%% @end
%%% ----------------------------------------------------------
-spec notification_callback(integer(), #safsNtfAlarmNotification{}) -> ok.

notification_callback(
  ?NtfAlarmSubscriptionId,
  #safsNtfAlarmNotification{notificationHeader=NH,
			    probableCause=PC,
			    specificProblems=SP,
			    perceivedSeverity=PS,
			    proposedRepairActions=PRA}=Notification) ->
    sysInitI:info_report(
      [notification_callback,
       {notificationHeader, NH},
       {probableCause, PC},
       {specificProblems, SP},
       {perceivedSeverity, PS},
       {proposedRepairActions, PRA}
      ]),
    gen_server:cast(?SERVER, {notification_callback, Notification});

notification_callback(SubscriptionId, Notification) ->
    sysInitI:warning_msg("~s: ~s~n"
			 "ignoring unexpected notification: ~p~n",
			 [?MODULE, ?FUNCTION_NAME,
			  {SubscriptionId, Notification}]).

prep_pgroup(_PrgGrp) ->
    ok.

pgroup_done(PrgGrp) ->
    AlarmList = comFm:get_app_alarms(iolist_to_binary(PrgGrp)),
    sysInitI:info_msg("~s: ~s~n"
		      "clearing application alarms at restart of ~s:~n"
		      "~p~n",
		      [?MODULE, ?FUNCTION_NAME, PrgGrp, AlarmList]),

    [comFm:clear_alarm_by_instance(MajorType, MinorType, Dn) ||
	{MajorType, MinorType, Dn} <- AlarmList],

    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({notification_callback, AlarmNotification},
	    State) ->
    handle_alarm_notification(AlarmNotification),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
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
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_alarm_notification(
  #safsNtfAlarmNotification{notificationHeader=NotificationHeader,
			    perceivedSeverity=PerceivedSeverity,
			    proposedRepairActions = ProposedRepairActions}) ->
    #safsNtfNotificationHeader{notificationClassId=#safsNtfClassId{vendorId=VendorId,
								   majorId=MajorId,
								   minorId=MinorId},
			       notificationObject=NotificationObject,
			       additionalText=AdditionalText,
			       additionalInfo=AdditionalInfo}=NotificationHeader,
    AlarmName = {VendorId, MajorId, MinorId},
    PrgGrp = get_program_group(ProposedRepairActions),
    try
	send_alarm(AlarmName, PerceivedSeverity, NotificationObject,
		   AdditionalText, AdditionalInfo, PrgGrp)
    catch
	throw:{Reason, Offender} ->
	    sysInitI:error_msg("~s: ~s~n"
			       "~w: ~p~n",
			       [?MODULE, ?FUNCTION_NAME, Reason, Offender]);
	Exception:Item ->
	    sysInitI:error_msg("~s: ~s~n"
			       "failed to handle alarm: ~p~n",
			       [?MODULE, ?FUNCTION_NAME, Exception, Item])
    end.


send_alarm(AlarmName, ?SA_NTF_SEVERITY_CLEARED, Dn, AddText, AddInfo,
	   _PrgGrp) ->
    AppAlarm = true,
    comsaServer:clear_alarm(
      AlarmName,
      dnTo3gpp(AlarmName, Dn),
      toString(AddText),
      toPrintable(AddInfo),
      AppAlarm);

%% safs log alarms are cached in LogM
%% FIXME, is it correct that 'clear' alarms for safApp=safLogService
%% shall NOT be ignored (the previous clause matches)?????
send_alarm(_AlarmName,
	   _Severity,
	   <<"safApp=safLogService">>,
	   _AddText,
	   _AddInfo,
	   _PrgGrp) ->
    ok;

send_alarm(AlarmName, Severity, Dn, AddText, AddInfo, PrgGrp) ->
    AppAlarm = true,
    comsaServer:send_alarm(
      AlarmName,
      severity(Severity),
      dnTo3gpp(AlarmName, Dn),
      toString(AddText),
      toPrintable(AddInfo),
      AppAlarm,
      PrgGrp).


%%% ----------------------------------------------------------
%%% @doc Convert the given "additional info" to a printable
%%% format that suites COMTE.
%%% @end
%%% ----------------------------------------------------------

-spec toPrintable([]) -> [{binary(), binary()}].

toPrintable(AddInfo) ->
    P = [begin
	     IdStr = list_to_binary(integer_to_list(Id)),
	     Value = selectValue(ValueWrapper, 2),
	     case Type of
		 sa_ntf_value_string ->
		     splitValueString(IdStr, Value);
		 _ ->
		     {skip, Value}
	     end
	 end
	 || #safsNtfAdditionalInfo{infoId=Id,
				   infoType=Type,
				   infoValue=ValueWrapper} <- AddInfo],
    lists:filter(fun({skip, _}) -> false; (_) -> true end, P).

splitValueString(IdStr, Value) ->
    Split = binary:split(Value, [<<"=">>]),
    [SplitName | SplitValue] = Split,
    case SplitValue of
        [] ->
            {IdStr, Value};
        _Otherwise ->
            {SplitName, lists:nth(1, SplitValue)}
    end.

selectValue(ValueWrapper, K) ->
    case element(K, ValueWrapper) of
	undefined ->
	    selectValue(ValueWrapper, K+1);
	Value ->
	    Value
    end.


%%% ----------------------------------------------------------
%%% @doc If the given DN appears to be in 3GPP format then it
%%% is just wrapped in a list. Otherwise it is assumed to be an
%%% IMM DN and a conversion to 3GPP format is attempted. If
%%% conversion fails then
%%% ```
%%% [<<"ManagedElement=1">>]
%%% '''
%%% is provided as a fallback.
%%% @end
%%% ----------------------------------------------------------

-spec dnTo3gpp(tuple(), binary()) ->  [binary()].

dnTo3gpp(AlarmName, Dn) ->
    case string:to_lower(binary_to_list(Dn)) of
	[$m, $a, $n, $a, $g, $e, $d, $e, $l, $e, $m, $e, $n, $t, $=|_] ->
	    [Dn];
	_ ->
	    try gmfI:imm_to_mim(Dn) of
		{ok, MimS} ->
		    [MimS];
		Other ->
		    sysInitI:warning_report([{alarm, AlarmName},
					     {bad_dn, Dn},
					     {translation, Other}]),
		    [<<"ManagedElement=1">>]
	    catch
		Ex:What ->
		    sysInitI:warning_report([{alarm, AlarmName},
					     {bad_dn, Dn},
					     {caught, {Ex, What}}]),
		    [<<"ManagedElement=1">>]
	    end
    end.


-spec toString(undefined | binary()) ->  string().

toString(undefined) ->
    "";

toString(AddText) when is_binary(AddText) ->
    binary_to_list(AddText);

toString(Other) ->
    throw({cannot_cast_to_string, Other}).


-spec severity(atom()) -> atom().

severity(?SA_NTF_SEVERITY_INDETERMINATE) -> indeterminate;
severity(?SA_NTF_SEVERITY_WARNING)       -> warning;
severity(?SA_NTF_SEVERITY_MINOR)         -> minor;
severity(?SA_NTF_SEVERITY_MAJOR)         -> major;
severity(?SA_NTF_SEVERITY_CRITICAL)      -> critical;
severity(Other)                          -> throw({bad_severity, Other}).

get_program_group([]) ->
    undefined;
get_program_group([#safsNtfProposedRepairAction{actionId = 0,
						actionValueType = sa_ntf_value_string,
						actionValue = Value}|_]) ->
    Value#safsNtfValue.variable;
get_program_group([_|T]) ->
    get_program_group(T);
get_program_group(_ProposedRepairActions) ->
    undefined.
