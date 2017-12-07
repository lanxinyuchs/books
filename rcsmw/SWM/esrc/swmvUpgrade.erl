%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 1.    BASIC INFORMATION
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 1.1   MODULE INFORMATION
%% ###-----------------------------------------------------------------------###
%% %CCaseFile:	swmvUpgrade.erl %
%% @author etxberb
%% @copyright Ericsson AB 2017
%% @version /main/R11A/R12A/1

%% @doc == State machine for the Upgrade procedure ==
%%
%% @end

%% ###=======================================================================###
%% # 1.2   MODULE DEFINITION
%% ###-----------------------------------------------------------------------###
-module(swmvUpgrade).
-behaviour(gen_statem).
-vsn('/main/R11A/R12A/1').
-date('2017-12-06').
-author(etxberb).

%% ###=======================================================================###
%% # 1.3   LEGAL RIGHTS
%% ###-----------------------------------------------------------------------###
%% %CCaseTemplateFile:	module.erl %
%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%
%% %CCaseCopyrightBegin%
%% Copyright (c) Ericsson AB 2017 All rights reserved.
%% 
%% The information in this document is the property of Ericsson.
%% 
%% Except as specifically authorized in writing by Ericsson, the 
%% receiver of this document shall keep the information contained 
%% herein confidential and shall protect the same in whole or in 
%% part from disclosure and dissemination to third parties.
%% 
%% Disclosure and disseminations to the receivers employees shall 
%% only be made on a strict need to know basis.
%% %CCaseCopyrightEnd%

%% ###=======================================================================###
%% # 1.4   REVISION LOG
%% ###-----------------------------------------------------------------------###
%% Rev     Date       Name     What
%% ----    ---------- -------  -------------------------------------------------
%% R11A/1  2017-10-20 etxberb  Created.
%% ----    ---------- -------  -------------------------------------------------
%% R12A/1  2017-12-06 etxberb  Starting to use this module.
%% ------  ---------- -------  ---END-OF-LOG------------------------------------

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 2.    INTERNAL DEFINITIONS
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 2.1.1 Interface functions
%% ###-----------------------------------------------------------------------###
%% API
-export([activate/1,
	 cancel/1,
	 confirm/1,
	 create_backup/1,
	 create_upgrade_backup/1,
	 delete_backup/1,
	 export_backup/1,
	 is_ongoing/0,
	 is_prepared/0,
	 start/1,
	 stop/1]).

-export([stateM_evt_exc/1]).

%% ###-----------------------------------------------------------------------###
%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% Mandatory for gen_statem
-export([init/1,
	 callback_mode/0,
	 terminate/3,
	 code_change/4]).

%% StateName
-export(['ST_frNode_prepare'/3,
	 'ST_toNode_activate'/3,
	 'ST_toNode_init'/3,
	 'ST_toNode_wait4Commit'/3,
	 'ST_toNode_wait4CommitCompleted'/3]).

%% Event
-export(['EVT_activate'/2,
 	 'EVT_cancel'/2,
 	 'EVT_confirm'/2,
	 'EVT_create_backup'/2,
	 'EVT_create_upgrade_backup'/2,
	 'EVT_delete_backup'/2,
	 'EVT_export_backup'/2,
	 'EVT_mnesiaTblEvt_UP'/2]).

%% ###=======================================================================###
%% # 2.3   IMPORT OF DEFINITIONS
%% ###-----------------------------------------------------------------------###
-include("SwmREST.hrl").
-include("alhI.hrl").

%% ###=======================================================================###
%% # 2.4   LOCAL DEFINITION OF MACROS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 2.5   LOCAL DEFINITION OF RECORDS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 2.6   LOCAL DEFINITION OF TYPES
%% ###-----------------------------------------------------------------------###
-type start_data() :: #{trigger := start_trigger(),
			upKey   => tuple()}.

-type start_trigger() :: vnfm_action | node_restart | upgrade_aborted.

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 3.    CODE
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 3.1.1 API
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% @doc Activation of the Software Upgrade procedure is in progress.
%%
%% @end
%% ###=======================================================================###
-spec activate(Data :: map()) ->
    ok | undefined.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
activate(Data) ->
    cast({'EVT_activate', Data}).

%% #############################################################################
%% @doc Cancel an action.
%%
%% @end
%% ###=======================================================================###
-spec cancel(Data :: map()) ->
    ok | undefined.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cancel(Data) ->
    cast({'EVT_cancel', Data}).

%% #############################################################################
%% @doc The operator has confirmed the upgrade via R-VNFM.
%%
%% @end
%% ###=======================================================================###
-spec confirm(Data :: map()) ->
    ok | undefined.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
confirm(Data) ->
    cast({'EVT_confirm', Data}).

%% #############################################################################
%% @doc Create a backup file to be used by the 'toNode' for doing upgrade.
%%   DEPRECATED! Used by version 1.
%%
%% @end
%% ###=======================================================================###
-spec create_backup(Data :: map()) ->
    ok | undefined.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
create_backup(Data) ->
    cast({'EVT_create_backup', Data}).

%% #############################################################################
%% @doc Create a backup file to be used by the 'toNode' for doing upgrade.
%%
%% @end
%% ###=======================================================================###
-spec create_upgrade_backup(Data :: map()) ->
    ok | undefined.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
create_upgrade_backup(Data) ->
    cast({'EVT_create_upgrade_backup', Data}).

%% #############################################################################
%% @doc Delete a backup.
%%
%% @end
%% ###=======================================================================###
-spec delete_backup(Data :: map()) ->
    ok | {error, timeout} | undefined.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
delete_backup(Data) ->
    call({'EVT_delete_backup', Data}, ?TIME_DeleteBackup).

%% #############################################################################
%% @doc Export a backup file to be used by the 'toNode' for doing upgrade.
%%   DEPRECATED! Used by version 1.
%%
%% @end
%% ###=======================================================================###
-spec export_backup(Data :: map()) ->
    ok | undefined.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
export_backup(Data) ->
    cast({'EVT_export_backup', Data}).

%% #############################################################################
%% @doc Is SW Upgrade ongoing?
%%
%% @end
%% ###=======================================================================###
-spec is_ongoing() ->
    boolean().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
is_ongoing() ->
    try sys:get_state(?StateM_NAME) of
	{_CurrState, #{init_state := 'ST_toNode_init'} = _Data} ->
	    true;
	_ ->
	    false
    catch
	exit : {noproc, _} ->
	    false
    end.

%% #############################################################################
%% @doc Is SW Upgrade prepared?
%%
%% @end
%% ###=======================================================================###
-spec is_prepared() ->
    boolean().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
is_prepared() ->
    try swmLib:get_variable(?MODULE) of
	undefined ->
	    swmLib:is_init_config_upgrade();
	_ ->
	    false
    catch
	_ : _ ->
	    %% Table not created yet.
	    %% Means that this is during scratch installation.
	    swmLib:is_init_config_upgrade()
    end.

%% #############################################################################
%% @doc Start of the procedure.
%%
%% @end
%% ###=======================================================================###
-spec start(StartData :: start_data()) ->
    {ok, pid()} | ignore | {error, term()}.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start(#{trigger := node_restart} = StartData) ->
    OldData = old_data(),
    case {swmLib:is_init_config_upgrade(), OldData} of
	{_, #{}} ->
	    start(sysEnv:rcs_mode_2(), OldData#{trigger => upgrade_aborted});
	{true, undefined} ->
	    start(sysEnv:rcs_mode_2(), StartData);
	_ ->
	    ignore
    end;
start(StartData) ->
    start(sysEnv:rcs_mode_2(), StartData).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start(vrcs, #{trigger := vnfm_action} = StartData) ->
    gen_statem:start({local, ?StateM_NAME},
		     ?MODULE,
		     maps:put(init_state, 'ST_frNode_prepare', StartData),
		     []);
start(vrcs, #{trigger := node_restart} = StartData) ->
    gen_statem:start({local, ?StateM_NAME},
		     ?MODULE,
		     maps:put(init_state, 'ST_toNode_init', StartData),
		     []);
start(vrcs, #{trigger := upgrade_aborted}) ->
    swmvBackup:remove_prepared_init_config(),
    ignore;
start(_, _) ->
    ignore.

%% #############################################################################
%% @doc Stop the procedure.
%%
%% @end
%% ###=======================================================================###
-spec stop(Reason :: ({ok, OkReason :: term()} |          % No logging
		      {error, ErrReason :: term()})) ->   % Logs ERROR REPORT
    ok | noproc | timeout.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stop(Reason) ->
    StopReason =
	case Reason of
	    {ok, OkReason} ->
		{shutdown, OkReason};
	    {error, ErrReason} ->
		ErrReason
	end,
    try
	gen_statem:stop(?StateM_NAME, StopReason, timer:hms(0, 1, 0))
    catch
	exit : timeout ->
	    ?LOG_ERR([timeout | sysUtil:pid_info(whereis(?StateM_NAME))]),
	    timeout;
	exit : ER when is_atom(ER) ->
	    ER
    end.

%% ###=======================================================================###
%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 3.2.1 Mandatory for gen_statem
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% callback_mode
%%
%% ###=======================================================================###
callback_mode() ->
    state_functions.

%% #############################################################################
%% init
%%
%% ###=======================================================================###
init(Data) ->
    ?LOG_INFO([Data]),
    {ok, maps:get(init_state, Data), Data}.

%% #############################################################################
%% terminate
%%
%% ###=======================================================================###
terminate({shutdown, Reason}, State, Data) ->
    ?LOG_INFO([{reason, Reason},
	       {state, State},
	       {data, Data}]),
    swmvBackup:remove_prepared_init_config(),
    ok;
terminate(Reason, State, #{init_state := 'ST_frNode_prepare'} = Data) ->
    ?LOG_ERR([{reason, Reason},
	      {state, State},
	      {data, Data}]),
    swmvBackup:remove_prepared_init_config(),
    swmLib:order_restart_node(cold, ?ALH_TAG_UpgradeFailure),
    ok;
terminate(Reason, State, Data) ->
    ?LOG_ERR([{reason, Reason},
	      {state, State},
	      {data, Data}]),
    swmvBackup:remove_prepared_init_config(),
    swmLib:order_restart_node(cold, ?ALH_TAG_UpgradeFailure),
    UpgResult =
	#{timeActionCompleted => comsaI:iso_time(os:timestamp(), extended),
	  result => "FAILURE",
	  resultInfo => "The action was interrupted by a software error"},
    swmREST:msg2vnfm_upgrade_status(#{action_result => UpgResult}),
    ok.

%% #############################################################################
%% code_change
%%
%% ###=======================================================================###
code_change(Vsn, State, Data, Extra) ->
    ?LOG_INFO([{vsn, Vsn},
	       {state, State},
	       {data, Data},
	       {extra, Extra}]),
    {ok, State, Data}.

%% ###-----------------------------------------------------------------------###
%% # 3.2.2 State Machine
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% STATE: frNode_prepare
%%
%% ###=======================================================================###
'ST_frNode_prepare'({call, From}, {'EVT_delete_backup'=EVT, EvtData}, Data) ->
    ?EVT(EVT, EvtData#{from => From}, Data);
'ST_frNode_prepare'(cast, {'EVT_create_upgrade_backup'=EVT, EvtData}, Data) ->
    ?EVT(EVT, EvtData, Data);
'ST_frNode_prepare'(cast, {'EVT_cancel'=EVT, EvtData}, Data) ->
    ?EVT(EVT, EvtData, Data);
'ST_frNode_prepare'(cast, {'EVT_create_backup'=EVT, EvtData}, Data) ->
    ?EVT(EVT, EvtData, Data);
'ST_frNode_prepare'(cast, {'EVT_export_backup'=EVT, EvtData}, Data) ->
    ?EVT(EVT, EvtData, Data);
'ST_frNode_prepare'(EvtType, Evt, Data) ->
    ?LOG_WARN(["Unrecognized event",
	       {evtType, EvtType},
	       {evt, Evt},
	       {data, Data}]),
    keep_state_and_data.

%% #############################################################################
%% STATE: toNode_activate
%%
%% ###=======================================================================###
'ST_toNode_activate'(info,
		     {mnesia_table_event, {write, upgradePackage, Obj, _, _}},
		     Data) ->
    ?EVT('EVT_mnesiaTblEvt_UP', Obj, Data);
'ST_toNode_activate'(info, {mnesia_table_event, _}, _) ->
    keep_state_and_data;
'ST_toNode_activate'(_, _, _) ->
    {keep_state_and_data, postpone}.

%% #############################################################################
%% STATE: toNode_init
%%
%% ###=======================================================================###
'ST_toNode_init'(cast, {'EVT_activate'=EVT, EvtData}, Data) ->
    ?EVT(EVT, EvtData, Data);
'ST_toNode_init'(_, _, _) ->
    {keep_state_and_data, postpone}.

%% #############################################################################
%% STATE: toNode_wait4Commit
%%
%% ###=======================================================================###
'ST_toNode_wait4Commit'(cast, {'EVT_confirm'=EVT, EvtData}, Data) ->
    ?EVT(EVT, EvtData, Data);
'ST_toNode_wait4Commit'(_, _, _) ->
    {keep_state_and_data, postpone}.

%% #############################################################################
%% STATE: toNode_wait4CommitCompleted
%%
%% ###=======================================================================###
'ST_toNode_wait4CommitCompleted'(info,
				 {mnesia_table_event, {write,
						       upgradePackage, Obj,_,_}},
				 Data) ->
    ?EVT('EVT_mnesiaTblEvt_UP', Obj, Data);
'ST_toNode_wait4CommitCompleted'(info, {mnesia_table_event, _}, _) ->
    keep_state_and_data;
'ST_toNode_wait4CommitCompleted'(EvtType, Evt, Data) ->
    ?LOG_WARN(["Unrecognized event",
	       {evtType, EvtType},
	       {evt, Evt},
	       {data, Data}]),
    keep_state_and_data.

%% ###-----------------------------------------------------------------------###
%% # 3.2.3 Event Handling
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% EVENT: activate
%%
%% ###=======================================================================###
'EVT_activate'(EvtData, Data) ->
    mnesia:subscribe({table, upgradePackage, detailed}),
    {next_state, 'ST_toNode_activate', maps:merge(Data, EvtData)}.

%% #############################################################################
%% EVENT: cancel
%%
%% ###=======================================================================###
'EVT_cancel'(_, Data) ->
    swmvBackup:remove_prepared_init_config(),
    {keep_state, Data}.

%% #############################################################################
%% EVENT: confirm
%%
%% ###=======================================================================###
'EVT_confirm'(_EvtData, Data) ->
    swmServer:confirm_package(),
    alhI:upgrade_confirmed(),
    {next_state, 'ST_toNode_wait4CommitCompleted', Data}.

%% #############################################################################
%% EVENT: create_backup
%%
%% ###=======================================================================###
'EVT_create_backup'(#{env := Env,
		      action_id := ActionId,
		      backup_name := BackupName},
		    Data) ->
    swmLib:set_variable(?MODULE, Data),
    {ok, ActionResult} = swmvBackup:create_backup(BackupName, ActionId),
    #{status_code := 200} =
	swmREST:msg2vnfm_backup_status(#{env => Env,
					 action_id => ActionId,
					 action_result => ActionResult}),
    {keep_state, Data}.

%% #############################################################################
%% EVENT: create_upgrade_backup
%%
%% ###=======================================================================###
'EVT_create_upgrade_backup'(#{env := Env,
			      action_id := ActionId,
			      backup_name := BackupName},
			    Data) ->
    swmLib:set_variable(?MODULE, Data),
    {ok, ActionResult} = swmvBackup:create_upgrade_backup(BackupName, ActionId),
    alhI:upgrade_activated(),
    #{status_code := 200} =
	swmREST:msg2vnfm_backup_status(#{env => Env,
					 action_id => ActionId,
					 action_result => ActionResult}),
    {keep_state, Data}.

%% #############################################################################
%% EVENT: delete_backup
%%
%% ###=======================================================================###
'EVT_delete_backup'(#{from := From,
		      action_id := ActionId,
		      backup_name := BackupName},
		    Data) ->
    {ok, ActionResult} = swmvBackup:delete_backup(BackupName, ActionId),
    ?LOG_INFO([{backupName, BackupName} | ActionResult]),
    {keep_state, Data, {reply, From, ok}}.

%% #############################################################################
%% EVENT: export_backup
%%
%% ###=======================================================================###
'EVT_export_backup'(#{env := Env,
		      action_id := ActionId,
		      backup_name := BackupName,
		      uri := Uri,
		      pwd := Pwd},
		    Data) ->
    {ok, ActionResult} =
	swmvBackup:export_backup(BackupName, Uri, Pwd, ActionId),
    StatusInfo = proplists:get_value(resultInfo, ActionResult, ""),
    AR = #{action_result => ActionResult,
	   backup_file => filename:basename(StatusInfo)},
    #{status_code := 200} =
	swmREST:msg2vnfm_backup_status(#{env => Env,
					 action_id => ActionId,
					 action_result => AR}),
    {keep_state, Data}.

%% #############################################################################
%% EVENT: mnesiaTblEvt_UP
%%
%% ###=======================================================================###
'EVT_mnesiaTblEvt_UP'(Obj, #{state := 'ST_toNode_activate',
			     upKey := UpKey} = Data) ->
    UpProps = swmREST:mo_format(Obj),
    ?LOG_INFO(UpProps),
    case swmREST:mo_is_finished(UpKey, Obj) of
	false ->
	    {keep_state, Data};
	true ->
	    NewResultProp = {result, proplists:get_value(state, UpProps, "")},
	    UpgResult =
		lists:keyreplace(result,
				 1,
				 swmREST:mo_format(reportProgress, UpKey),
				 NewResultProp),
	    #{status_code := 200} =
		swmREST:msg2vnfm_upgrade_status(#{action_result => UpgResult}),
	    {next_state, 'ST_toNode_wait4Commit', Data}
    end;
'EVT_mnesiaTblEvt_UP'(Obj,
		      #{state := 'ST_toNode_wait4CommitCompleted'} = Data) ->
    UpProps = swmREST:mo_format(Obj),
    ?LOG_INFO(UpProps),
    case proplists:get_value(state, UpProps, "") of
	"COMMIT_COMPLETED" = State ->
	    TActionCompl = proplists:get_value(timeActionCompleted, UpProps, ""),
	    ResultInfo = proplists:get_value(resultInfo, UpProps, ""),
	    UpgResult = #{timeActionCompleted => TActionCompl,
			  result => State,
			  resultInfo => ResultInfo},
	    #{status_code := 200} =
		swmREST:msg2vnfm_upgrade_status(#{action_result => UpgResult}),
	    {stop, {shutdown, commit_completed}, Data};
	_ ->
	    {keep_state, Data}
    end;
'EVT_mnesiaTblEvt_UP'(_, Data) ->
    {keep_state, Data}.

%% ###-----------------------------------------------------------------------###
%% # 3.2.4 General state machine functionality
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% stateM_evt_exc
%%
%% ###=======================================================================###
stateM_evt_exc(#{excClass := throw, data := Data} = ExcData) ->
    ?LOG_ERR(["Unexpected throw", ExcData]),
    stateM_evt_exc_2vnfm(ExcData),
    {keep_state, Data};
stateM_evt_exc(#{data := Data} = ExcData) ->
    stateM_evt_exc_2vnfm(ExcData),
    {keep_state, Data}.

%% #############################################################################
%% stateM_evt_exc_2vnfm
%%
%% ###=======================================================================###
stateM_evt_exc_2vnfm(#{excReason := ExcReason,
		       data      := #{init_state := InitState} = Data}) ->
    RestApiMethod =
	case InitState of
	    'ST_frNode_prepare' ->
		"backup_status";
	    _ ->
		"upgrade_status"
	end,
    ActionIdList =
	case maps:get(action_id, Data, []) of
	    [] ->
		[];
	    ActionIdValue ->
		[{action_id, ActionIdValue}]
	end,
    FailResult =
	maps:from_list([{result, "FAILURE"},
			{resultInfo, swmREST:ensureStr(ExcReason)},
			{timeActionCompleted, comsaI:iso_time(os:timestamp(),
							      extended)}
			| ActionIdList]),
    swmREST:msg2vnfm_send(#{method => post, return => httpc},
			  ?VnfmLcmUri(swmREST:vnfm_ip(),
				      ?VnfmLcmV1,
				      RestApiMethod),
			  [{vnf_id, swmREST:vnf_id()}
			   | swmREST:actionResult_Json(FailResult)]).

%% ###=======================================================================###
%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 3.3.1 Help Functions
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% call
%%
%% ###=======================================================================###
call(Request, TimeoutValue) ->
    try
	gen_statem:call(?StateM_NAME, Request, TimeoutValue)
    catch
	exit : {noproc, _} ->
	    undefined;
	exit : {timeout, _} ->
	    ?LOG_ERR([{"Timed out, milliseconds", TimeoutValue},
		      Request]),
	    {error, timeout}
    end.

%% #############################################################################
%% cast
%%
%% ###=======================================================================###
cast(Msg) ->
    cast(whereis(?StateM_NAME), Msg).

cast(Pid, Msg) when is_pid(Pid) ->
    gen_statem:cast(?StateM_NAME, Msg);
cast(_, _) ->
    undefined.

%% #############################################################################
%% old_data
%%
%% ###=======================================================================###
old_data() ->
    try
	swmLib:erase_variable(?MODULE)
    catch
	_ : _ ->
	    %% Table not created yet.
	    %% Means that this is during scratch installation.
	    undefined
    end.

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 4     CODE FOR TEMPORARY CORRECTIONS
%% ###-----------------------------------------------------------------------###
