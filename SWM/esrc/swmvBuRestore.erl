%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 1.    BASIC INFORMATION
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 1.1   MODULE INFORMATION
%% ###-----------------------------------------------------------------------###
%% %CCaseFile:	swmvBuRestore.erl %
%% @author etxberb
%% @copyright Ericsson AB 2017
%% @version /main/R11A/R12A/11

%% @doc == State machine for the Backup Restore procedure ==
%%
%% @end

%% ###=======================================================================###
%% # 1.2   MODULE DEFINITION
%% ###-----------------------------------------------------------------------###
-module(swmvBuRestore).
-behaviour(gen_statem).
-vsn('/main/R11A/R12A/11').
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
%% R11A/1  2017-10-19 etxberb  Created.
%% ----    ---------- -------  -------------------------------------------------
%% R12A/1  2017-10-27 etxberb  Additions for "SP277: backup/restore of vSD/vPP"
%% R12A/2  2017-11-15 etxberb  Continuation of SP277.
%% R12A/3  2017-11-17 etxberb  Continuation of SP277.
%% R12A/4  2017-11-20 etxberb  Continuation of SP277.
%% R12A/6  2017-11-24 etxberb  Continuation of SP277.
%% R12A/7  2017-11-27 etxberb  SP277: Cancel action.
%% R12A/8  2017-11-28 etxberb  SP277: Cancel bug fix.
%% R12A/11 2017-12-06 etxberb  Refactoring.
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
	 confirm_restore/1,
	 is_ongoing/0,
	 is_prepared/0,
	 restore_initialized/1,
	 start/1,
	 stop/1]).

-export([appCheck_restoreStart/1]).

-export([stateM_evt_exc/1]).

%% Messages from LCM / VNFM
-export([post_restore_check/0,
	 restore_aborted/1,
	 restore_confirmed/1]).

%% ###-----------------------------------------------------------------------###
%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% Mandatory for gen_statem
-export([init/1,
	 callback_mode/0,
	 terminate/3,
	 code_change/4]).

%% StateName
-export(['ST_frNode'/3,
	 'ST_toNode_init'/3,
	 'ST_toNode_wait4Confirm'/3,
	 'ST_toNode_wait4LcmActions'/3,
	 'ST_toNode_wait4LcmCompleted'/3,
	 'ST_toNode_wait4LcmCompletedForce'/3]).

%% Event
-export(['EVT_cancel'/2,
 	 'EVT_confirm_restore'/2,
 	 'EVT_interval_IsLcmActions'/2,
 	 'EVT_interval_IsLcmStart'/2,
 	 'EVT_activate_swRestore'/2,
 	 'EVT_post_restore_check'/2,
 	 'EVT_restore_aborted'/2,
 	 'EVT_restore_confirmed'/2,
 	 'EVT_restore_initialized'/2,
 	 'EVT_timeout_LcmConfirm'/2,
 	 'EVT_timeout_LcmStart'/2]).

-import(swmBackupModel, [update_progress/2]).

%% ###=======================================================================###
%% # 2.3   IMPORT OF DEFINITIONS
%% ###-----------------------------------------------------------------------###
-include("RcsBrM.hrl").
-include("SwmREST.hrl").
-include("alhI.hrl").

%% ###=======================================================================###
%% # 2.4   LOCAL DEFINITION OF MACROS
%% ###-----------------------------------------------------------------------###
-define(interval_IsLcmActions, timer:hms(0, 0, 3)).
-define(interval_IsLcmStart, timer:hms(0, 0, 10)).
-define(timeout_LcmConfirm, timer:hms(0, 45, 0)).
-define(timeout_LcmStart_max, timer:hms(1, 0, 0)).

%% ###=======================================================================###
%% # 2.5   LOCAL DEFINITION OF RECORDS
%% ###-----------------------------------------------------------------------###

%% ###=======================================================================###
%% # 2.6   LOCAL DEFINITION OF TYPES
%% ###-----------------------------------------------------------------------###
-type start_data() :: #{trigger    := start_trigger(),
			vnfm_data  => map(),
			bu_index   => string(),
			bu_restore => string() | undefined}.

-type start_trigger() :: mo_action | node_restart | restore_aborted.

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
%% @doc Activate the Software Restore procedure.
%%
%% @end
%% ###=======================================================================###
-spec activate(Data :: map()) ->
    ok | undefined.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
activate(Data) ->
    cast({'EVT_activate_swRestore', Data}).

%% #############################################################################
%% @doc Cancel Restore is ordered.
%%
%% @end
%% ###=======================================================================###
-spec cancel(Info :: string() | map()) ->
    ok | undefined.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cancel(Info) when is_list(Info) ->
    cast({'EVT_cancel', #{info => Info}});
cancel(Data) when is_map(Data) ->
    cast({'EVT_cancel', Data}).

%% #############################################################################
%% @doc Confirm Restore is ordered.
%%
%% @end
%% ###=======================================================================###
-spec confirm_restore(CallBackFun :: function()) ->
    ok | undefined.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
confirm_restore(CallBackFun) ->
    cast({'EVT_confirm_restore', CallBackFun}).

%% #############################################################################
%% @doc Is SW Backup Restore ongoing?
%%
%% @end
%% ###=======================================================================###
-spec is_ongoing() ->
    boolean().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
is_ongoing() ->
    case whereis(?StateM_NAME) of
	undefined ->
	    false;
	Pid when is_pid(Pid) ->
	    true
    end.

%% #############################################################################
%% @doc Is SW Backup Restore prepared?
%%
%% @end
%% ###=======================================================================###
-spec is_prepared() ->
    boolean().
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
is_prepared() ->
    try swmLib:get_variable(?MODULE) of
	undefined ->
	    swmLib:is_init_config_restore();
	_ ->
	    false
    catch
	_ : _ ->
	    %% Table not created yet.
	    %% Means that this is during scratch installation.
	    swmLib:is_init_config_restore()
    end.

%% #############################################################################
%% @doc Restore actions are done.
%%
%% @end
%% ###=======================================================================###
-spec restore_initialized(Args :: map()) ->
    ok | undefined.
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
restore_initialized(Args) ->
    cast({'EVT_restore_initialized', Args}).

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
    case {swmLib:is_init_config_restore(), OldData} of
	{_, #{}} ->
	    start(sysEnv:rcs_mode_2(), OldData#{trigger => restore_aborted});
	{true, undefined} ->
	    start(sysEnv:rcs_mode_2(), StartData);
	_ ->
	    ignore
    end;
start(StartData) ->
    start(sysEnv:rcs_mode_2(), StartData).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start(vrcs, #{trigger := mo_action} = StartData) ->
    StartResult =
	gen_statem:start({local, ?StateM_NAME},
			 ?MODULE,
			 StartData#{init_state => 'ST_frNode'},
			 []),
    StartResult;
start(vrcs, #{trigger := node_restart} = StartData) ->
    swmvBackup:remove_prepared_init_config(),
    gen_statem:start({local, ?StateM_NAME},
		     ?MODULE,
		     StartData#{init_state => 'ST_toNode_init'},
		     []);
start(vrcs, #{trigger := restore_aborted} = StartData) ->
    swmvBackup:remove_prepared_init_config(),
    gen_statem:start({local, ?StateM_NAME},
		     ?MODULE,
		     StartData#{init_state => 'ST_frNode'},
		     []);
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

%% ###-----------------------------------------------------------------------###
%% # 3.1.2 Messages from LCM / VNFM
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% @doc Perform checks after the restore.
%%
%% @end
%% ###=======================================================================###
post_restore_check() ->
    cast('EVT_post_restore_check').

%% #############################################################################
%% @doc LCM has aborted the restore action.
%%
%% @end
%% ###=======================================================================###
restore_aborted(Args) ->
    swmLib:erase_variable(?MODULE),
    swmvBackup:remove_prepared_init_config(),
    ok = cast({'EVT_restore_aborted', Args}).

%% #############################################################################
%% @doc LCM has completed all restore actions after confirm.
%%
%% @end
%% ###=======================================================================###
restore_confirmed(Args) ->
    cast({'EVT_restore_confirmed', Args}).

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
    ok;
terminate(Reason, State, #{init_state := 'ST_frNode'} = Data) ->
    ?LOG_ERR([{reason, Reason},
	      {state, State},
	      {data, Data}]),
    swmLib:order_restart_node(cold, ?ALH_TAG_SoftwareRestore),
    ok;
terminate(Reason, State, Data) ->
    ?LOG_ERR([{reason, Reason},
	      {state, State},
	      {data, Data}]),
    'EVT_cancel'(#{info => "Software error"}, Data),
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
%% STATE: frNode
%%
%% ###=======================================================================###
'ST_frNode'(cast, {'EVT_activate_swRestore'=EVT, EvtData}, Data) ->
    ?EVT(EVT, EvtData, Data);
'ST_frNode'(info, 'EVT_interval_IsLcmStart'=EVT, Data) ->
    ?EVT(EVT, undefined, Data);
'ST_frNode'(cast, {'EVT_cancel' = EVT, Args}, Data) ->
    ?EVT(EVT, Args, Data);
'ST_frNode'(cast, {'EVT_restore_aborted'=EVT, Args}, Data) ->
    ?EVT(EVT, Args, Data);
'ST_frNode'(info, 'EVT_timeout_LcmStart'=EVT, Data) ->
    ?EVT(EVT, undefined, Data);
'ST_frNode'(cast,
	      {'EVT_restore_initialized', _},
	      #{trigger := restore_aborted} = Data) ->
    {keep_state, Data};
'ST_frNode'(EvtType, Evt, Data) ->
    ?LOG_WARN(["Unrecognized event",
	       {evtType, EvtType},
	       {evt, Evt},
	       {data, Data}]),
    keep_state_and_data.

%% #############################################################################
%% STATE: toNode_init
%%
%% ###=======================================================================###
'ST_toNode_init'(cast, {'EVT_restore_initialized'=EVT, Arg}, Data) ->
    MergedData = maps:merge(Arg, Data),
    ?EVT(EVT, undefined, MergedData);
'ST_toNode_init'(_, _, _) ->
    {keep_state_and_data, postpone}.

%% #############################################################################
%% STATE: toNode_wait4LcmActions
%%
%% ###=======================================================================###
'ST_toNode_wait4LcmActions'(cast, 'EVT_post_restore_check'=EVT, Data) ->
    ?EVT(EVT, undefined, Data);
'ST_toNode_wait4LcmActions'(info, 'EVT_interval_IsLcmActions'=EVT, Data) ->
    ?EVT(EVT, undefined, Data);
'ST_toNode_wait4LcmActions'(cast, {'EVT_cancel'=EVT, Args}, Data) ->
    avl(),
    ?EVT(EVT, Args, Data);
'ST_toNode_wait4LcmActions'(_, _, _) ->
    {keep_state_and_data, postpone}.

%% #############################################################################
%% STATE: toNode_wait4Confirm
%%
%% ###=======================================================================###
'ST_toNode_wait4Confirm'(cast, {'EVT_confirm_restore'=EVT, Args}, Data) ->
    ?EVT(EVT, Args, Data);
'ST_toNode_wait4Confirm'(cast, {'EVT_cancel'=EVT, Args}, Data) ->
    avl(),
    ?EVT(EVT, Args, Data);
'ST_toNode_wait4Confirm'(_, _, _) ->
    {keep_state_and_data, postpone}.

%% #############################################################################
%% STATE: toNode_wait4LcmCompleted
%%
%% ###=======================================================================###
'ST_toNode_wait4LcmCompleted'(cast, {'EVT_restore_confirmed'=EVT, Args}, Data) ->
    ?EVT(EVT, Args, Data);
'ST_toNode_wait4LcmCompleted'(info, 'EVT_timeout_LcmConfirm'=EVT, Data) ->
    ?EVT(EVT, undefined, Data);
'ST_toNode_wait4LcmCompleted'(EvtType, Evt, Data) ->
    ?LOG_WARN(["Unrecognized event",
	       {evtType, EvtType},
	       {evt, Evt},
	       {data, Data}]),
    keep_state_and_data.

%% #############################################################################
%% STATE: toNode_wait4LcmCompletedForce
%%
%% ###=======================================================================###
'ST_toNode_wait4LcmCompletedForce'(cast,
				   {'EVT_restore_confirmed'=EVT, Args},
				   Data) ->
    ?EVT(EVT, Args, Data);
'ST_toNode_wait4LcmCompletedForce'(info, 'EVT_timeout_LcmConfirm'=EVT, Data) ->
    ?EVT(EVT, undefined, Data);
'ST_toNode_wait4LcmCompletedForce'(EvtType, Evt, Data) ->
    ?LOG_WARN(["Unrecognized event",
	       {evtType, EvtType},
	       {evt, Evt},
	       {data, Data}]),
    keep_state_and_data.

%% ###-----------------------------------------------------------------------###
%% # 3.2.3 Event Handling
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% EVENT: cancel
%%
%% ###=======================================================================###
'EVT_cancel'(Args, Data) ->
    case swmREST:msg2vnfm_swRestore_cancel(Args) of
	#{status_code := 200} ->
	    ok;
	#{status_code := 404 = StatusCode} ->   % "VnfId unknown"
	    ?LOG_WARN([{"Unexpected StatusCode", StatusCode}]);
	#{status_code := StatusCode} ->
	    ?LOG_ERR([{"Unexpected StatusCode", StatusCode}])
    end,
    {stop, {shutdown, cancelled}, Data}.

%% #############################################################################
%% EVENT: confirm_restore
%%
%% ###=======================================================================###
'EVT_confirm_restore'(CallBackFun, Data) ->
    CallBackFun(),   % swmBackup:confirm_restore_cont/1
    NewData = updateProgress([], Data),   % Dump progress queue
    case swmREST:msg2vnfm_swRestore_confirm(#{}) of
	#{status_code := 200} ->
	    ok;
	#{status_code := StatusCode} ->
	    ?LOG_ERR([{"Unexpected StatusCode", StatusCode}])
    end,
    {ok, TRef} = timer:send_after(?timeout_LcmConfirm,
				  'EVT_timeout_LcmConfirm'),
    {next_state, 'ST_toNode_wait4LcmCompleted', NewData#{timeout_LcmConfirm =>
							 TRef}}.

%% #############################################################################
%% EVENT: interval_IsLcmActions
%%
%% ###=======================================================================###
'EVT_interval_IsLcmActions'(_, Data) ->
    case
	swmREST:msg2vnfm_getRestoreStatus(#{state => undefined,
					    result => undefined,
					    info => []})
	of
	#{status_code := 200,
	  state       := "WAITING_FOR_CONFIRM",
	  result      := "SUCCESS",
	  info        := Infos} ->
	    {TRef, NewData} = maps:take(interval_IsLcmActions, Data),
	    timer:cancel(TRef),
	    RetData = updateProgress(addtlInfo(Infos), NewData),
	    {next_state, 'ST_toNode_wait4Confirm', RetData};
	#{status_code := 200} ->
	    {keep_state, Data};
	#{status_code := StatusCode} ->
	    ?LOG_ERR([{"Unexpected StatusCode", StatusCode}]),
	    {next_state, 'ST_toNode_wait4Confirm', Data}
    end.

%% #############################################################################
%% EVENT: interval_IsLcmStart
%%
%% ###=======================================================================###
'EVT_interval_IsLcmStart'(_, #{bu_index := BuIndex} = Data) ->
    case swmREST:msg2vnfm_getRestoreStatus(#{state => undefined, info => []}) of
	#{status_code := 200,
	  state := State} when State == "INITIALIZED" orelse
			       State == "RESTORE_IN_PROGRESS" ->
	    {keep_state, Data};
	#{status_code := 200, state := State, info := Infos} ->
	    ?LOG_ERR([{"Unexpected R-VNFM State", State}]),
	    Key = buIndex2key(BuIndex),
	    update_progress(Key, addtlInfo(Infos)),
	    swmBackup:aborted(Key, #{resultInfo => "R-VNFM error"}),
	    {TRef, NewData} = maps:take(interval_IsLcmStart, Data),
	    timer:cancel(TRef),
	    {stop, {shutdown, 'LCM_failure'}, NewData};
	#{status_code := StatusCode} ->
	    ?LOG_WARN([{"Unexpected StatusCode", StatusCode}]),
	    {keep_state, Data}
    end.

%% #############################################################################
%% EVENT: activate_swRestore
%%
%% ###=======================================================================###
'EVT_activate_swRestore'(#{vnfm_data := VnfmData},
			 #{bu_index := BuIndex} = Data) ->
    case swmREST:msg2vnfm_swRestore_start(VnfmData) of
	#{status_code := 200} ->
	    alhI:upgrade_activated(),   % Same routine for upgrade & restore :-)
	    swmLib:set_variable(?MODULE, Data),
	    {ok, TRef1} = timer:send_interval(?interval_IsLcmStart,
					      'EVT_interval_IsLcmStart'),
	    {ok, TRef2} = timer:send_after(?timeout_LcmStart_max,
					   'EVT_timeout_LcmStart'),
	    {keep_state, Data#{interval_IsLcmStart => TRef1,
			       timeout_LcmStart_max => TRef2,
			       vnfm_data => VnfmData}};
	#{status_code := StatusCode,
	  reason_phrase := ReasonPhrase,
	  ?AttrName_UserMessage := UserMessage} ->
	    case UserMessage of
		"" ->
		    ok;
		_ ->
		    Key = buIndex2key(BuIndex),
		    update_progress(Key, addtlInfo([UserMessage]))
	    end,
	    ResultInfo =
		"R-VNFM returned status code " ++
		sysUtil:term_to_string(StatusCode) ++
		" " ++
		ReasonPhrase,
	    throw({aborted, #{resultInfo => ResultInfo}})
    end.

%% #############################################################################
%% EVENT: post_restore_check
%%
%% ###=======================================================================###
'EVT_post_restore_check'(_, Data) ->
    {Result, NewData} = appCheck_restoreStop(Data),
    case swmREST:msg2vnfm_swRestore_postCheckStatus(Result) of
	#{status_code := 200} ->
	    {ok, TRef} = timer:send_interval(?interval_IsLcmActions,
					     'EVT_interval_IsLcmActions'),
	    {keep_state, NewData#{interval_IsLcmActions => TRef}};
	#{status_code := StatusCode} ->
	    ?LOG_ERR([{"Unexpected StatusCode", StatusCode}]),
	    {keep_state, NewData}
    end.

%% #############################################################################
%% EVENT: restore_aborted
%%
%% ###=======================================================================###
'EVT_restore_aborted'(#{resultInfo := RI, info := Infos},
		      #{bu_index := BuIndex} = Data) ->
    Key = buIndex2key(BuIndex),
    update_progress(Key, addtlInfo(Infos)),
    OptionalInfo =
	case RI of
	    "" ->
		#{};
	    _ ->
		#{resultInfo => RI}
	end,
    swmBackup:aborted(Key, OptionalInfo),
    {stop, {shutdown, aborted}, Data}.

%% #############################################################################
%% EVENT: restore_confirmed
%%
%% ###=======================================================================###
'EVT_restore_confirmed'(#{info := Info}, #{bu_index := BuIndex} = Data) ->
    update_progress(buIndex2key(BuIndex), addtlInfo(Info)),
    {TRef, NewData} = maps:take(timeout_LcmConfirm, Data),
    timer:cancel(TRef),
    {stop, {shutdown, finished}, NewData}.

%% #############################################################################
%% EVENT: restore_initialized
%%
%% ###=======================================================================###
'EVT_restore_initialized'(_, #{bu_index := BuIndex} = Data) ->
    #brmBackup{} = Obj = swmBackup:backup_object(BuIndex),
    case swmREST:msg2vnfm_swRestore_status(#{brm_obj => Obj}) of
	#{status_code := 200} ->
	    ok;
	#{status_code := StatusCode} ->
	    ?LOG_ERR([{"Unexpected StatusCode", StatusCode}])
    end,
    {next_state, 'ST_toNode_wait4LcmActions', Data}.

%% #############################################################################
%% EVENT: timeout_LcmConfirm
%%
%% ###=======================================================================###
'EVT_timeout_LcmConfirm'(_, #{state := 'ST_toNode_wait4LcmCompleted'} = Data) ->
    case swmREST:msg2vnfm_swRestore_confirm_force(#{}) of
	#{status_code := 200} ->
	    {ok, TRef} = timer:send_after(?timeout_LcmConfirm,
					  'EVT_timeout_LcmConfirm'),
	    NewData = Data#{timeout_LcmConfirm => TRef};
	#{status_code := StatusCode} ->
	    ?LOG_ERR([{"Unexpected StatusCode", StatusCode}]),
	    {ok, TRef} = timer:send_after(?timeout_LcmConfirm,
					  'EVT_timeout_LcmConfirm'),
	    NewData = Data#{timeout_LcmConfirm => TRef}
    end,
    {next_state, 'ST_toNode_wait4LcmCompletedForce', NewData};
'EVT_timeout_LcmConfirm'(_, #{state := 'ST_toNode_wait4LcmCompletedForce',
			      bu_index := BuIndex} = Data) ->
    Info =
	"R-VNFM has not completed Restore Confirm Cleanup within " ++
	integer_to_list((?timeout_LcmConfirm * 2) div 1000) ++
	" seconds",
    update_progress(buIndex2key(BuIndex), addtlInfo([Info])),
    {stop, {shutdown, timeout}, maps:remove(timeout_LcmConfirm, Data)}.

%% #############################################################################
%% EVENT: timeout_LcmStart
%%
%% ###=======================================================================###
'EVT_timeout_LcmStart'(_, #{bu_index := BuIndex} = Data) ->
    Info =
	"R-VNFM has not completed Restore Confirm Cleanup within " ++
	integer_to_list(?timeout_LcmStart_max div 1000) ++
	" seconds",
    Key = buIndex2key(BuIndex),
    update_progress(Key, addtlInfo([Info])),
    swmBackup:aborted(Key, #{resultInfo => "R-VNFM timeout"}),
    {TRef, NewData} = maps:take(interval_IsLcmStart, Data),
    timer:cancel(TRef),
    {stop, {shutdown, timeout}, maps:remove(timeout_LcmStart_max, NewData)}.

%% ###-----------------------------------------------------------------------###
%% # 3.2.4 General state machine functionality
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% stateM_evt_exc
%%
%% ###=======================================================================###
stateM_evt_exc(#{excClass  := throw,
		 excReason := {aborted, OptInfo},
		 data      := #{bu_index := BuIndex} = Data}) ->
    swmBackup:aborted(buIndex2key(BuIndex), OptInfo),
    {stop, {shutdown, aborted}, Data};
stateM_evt_exc(#{excClass := throw, data := Data} = ExcData) ->
    ?LOG_ERR(["Unexpected throw", ExcData]),
    {keep_state, Data};
stateM_evt_exc(#{data := Data}) ->
    {keep_state, Data}.

%% ###=======================================================================###
%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###
%% ###-----------------------------------------------------------------------###
%% # 3.3.1 Help Functions
%% ###-----------------------------------------------------------------------###
%% #############################################################################
%% addtlInfo
%%
%% ###=======================================================================###
addtlInfo(Infos) ->
    [{additionalInfo, sysUtil:term_to_string(I)} || I <- Infos].

%% #############################################################################
%% appCheck_restoreStart
%%
%% ###=======================================================================###
appCheck_restoreStart(Key) ->
    update_progress(Key,
		    addtlInfo(["Sending application trigger restoreStart"])),
    CbModules = swmLib:get_upg_callbacks(),
    case
	{swmREST:send_cs_trigger(CbModules, start_restore, "", ok),
	 swmREST:send_appl_trigger("restoreStart")}
	of
	{{ok, I1}, {ok, I2}} ->
	    update_progress(Key, addtlInfo(I1 ++ I2)),
	    ok;
     	{{_, I1}, {_, I2}} ->
	    update_progress(Key, addtlInfo(I1 ++ I2)),
	    swmBackup:aborted(Key, #{resultInfo => "Application check error"}),
	    error
    end.

%% #############################################################################
%% appCheck_restoreStop
%%
%% ###=======================================================================###
appCheck_restoreStop(#{} = Data) ->
    NewData =
	updateProgress(addtlInfo(["Sending application trigger restoreStop"]),
		       Data),
    CbModules = swmLib:get_upg_callbacks(),
    {_, I1} = swmREST:send_cs_trigger(CbModules, stop_restore, "", ok),
    {_, I2} = swmREST:send_appl_trigger("restoreStop"),
    {#{result => "SUCCESS", info => I1 ++ I2},
     NewData}.

%% #############################################################################
%% avl
%%
%% ###=======================================================================###
avl() ->
    AddInfo = lists:flatten([?ALH_TAG_RankCold,
			     ?ALH_TAG_Cause(?ALH_TAG_SoftwareRestore)]),
    alhI:write_node_event(os:timestamp(),
			  ?ALH_TAG_OutOfService,
			  ?ALH_TAG_ShutdownCommand,
			  0,   % EventId
			  ?ALH_TAG_Rcs(AddInfo)).

%% #############################################################################
%% buIndex2key
%%
%% ###=======================================================================###
buIndex2key(Ix) ->
    {"1", "1", "1", "1", Ix}.

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
%% maps_take
%%
%% ###=======================================================================###
maps_take(Key, Data, Default) ->
    case maps:take(Key, Data) of
	error ->
	    {Default, Data};
	Taken ->
	    Taken
    end.

%% #############################################################################
%% old_data
%%
%% ###=======================================================================###
old_data() ->
    try
	swmLib:get_variable(?MODULE)
    catch
	_ : _ ->
	    %% Table not created yet.
	    %% Means that this is during scratch installation.
	    undefined
    end.

%% #############################################################################
%% updateProgress
%%
%% ###=======================================================================###
updateProgress(ProgressData, #{bu_index := BuIndex} = Data) ->
    {OldProgressData, NewData} = maps_take(progressData, Data, []),
    Key = buIndex2key(BuIndex),
    case mnesia:dirty_read({brmBackup, Key}) of
	[] ->
	    NewData#{progressData => OldProgressData ++ ProgressData};
	[_] ->
	    update_progress(Key, OldProgressData ++ ProgressData),
	    NewData
    end.

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 4     CODE FOR TEMPORARY CORRECTIONS
%% ###-----------------------------------------------------------------------###
