%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmFailsafe.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R10A/R11A/1

%%% @doc ==Failsafe configuration support==
%%% @end

-module(swmFailsafe).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R10A/R11A/1').
-date('2017-09-13').
-author('etxjotj').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% -----      ---------  --------    ------------------------
%%% R2A/1      2014-01-15 etxjotj     Created
%%% R2A/4      2014-01-29 etxjotj     Added actionid counter
%%%                                   Proper backup name
%%% R2A/5      2014-01-30 etxjotj     Exported get_usage_stage
%%% R2A/6      2014-01-31 etxjotj     Added isFailsafe marker
%%% R2A/7      2014-01-31 etxjotj     Handle possible failure of create_backup
%%% R2A/8      2014-01-31 etxjotj     Do system created backups
%%% R2A/9      2014-02-24 etxjotj     Exported get_failsafe_backup
%%% R2A/10     2014-03-25 etxjotj     Transaction context get_failsafe_backup
%%%                                   Logging improvement
%%% R2A/11     2014-04-02 etxjotj     Minor adjustments
%%% R2A/12     2014-04-14 etxjotj     HS45047 Progress at failsafe restore
%%% R2A/13     2014-04-14 etxjotj     HS48694 Countdown after failsafe restore
%%% R2A/14     2014-04-15 etxarnu     Fiexed bug in init ( false -> undefined) 
%%% R2A/16     2014-05-20 etxjotj     HS42142 Prepare for longer backup time
%%% R2A/17     2014-05-22 erarafo     Unused function arguments anonymized
%%% R3A/1      2014-11-28 etxberb     Added values/1.
%%% R3A/2      2015-01-02 etxjotj     Added is_failsafe_restart/0
%%% R3A/3      2015-05-05 etxjotj     Add progress report for deactivate
%%% R4A/1      2015-08-24 etxpejn     Changed logI:write_log to swmLib:write_swm_log
%%% R4A/3      2015-09-21 etxjotj     Action id handling
%%% R4A/4      2015-09-22 etxjotj     New action id fix
%%% R5A/1      2015-10-08 etxjotj     ECIM BrM 3.4 
%%% R5A/2      2015-11-11 etxjotj     HU30730 Avoid housekeeping of fs backup
%%% R5A/3      2015-11-13 etxjotj     Upgrade support
%%% R5A/4      2015-11-16 etxjotj     Upgrade fix
%%% R5A/5      2016-01-12 etxjotj     HU49280 Changed enumeration of UsageState
%%% R5A/6      2016-02-02 etxjotj     Failsafe locks internal housekeeping
%%% R4A/8      2015-11-23 etxjotj     HU36796 Action name changed to activate
%%% R5A/8      2016-03-17 etxjotj     HU65504 Force db save at deactivate
%%% R4A/9      2016-03-17 etxjotj     HU65504 Commit db changes at deactivate
%%% R4A/10     2016-03-18 etxjotj     Updated action id handling
%%% R4A/11     2016-03-18 etxjotj     Manual merge error fix
%%% R4A/12     2016-03-18 etxjotj     Init fix
%%% R4A/13     2016-03-18 etxjotj     Corrected action id handling of R4A/3
%%% R4A/14     2016-03-18 etxjotj     Yet another bugfix
%%% R5A/9      2016-03-18 etxjotj     Merge from R4A
%%% R5A/10     2016-03-24 etxjotj     Backed out HU65504 temporarily
%%% R5A/11     2016-03-25 ekurnik     WP5369 added actionCapable locking of
%%%                                   activate and deactivate actions
%%% R5A/12     2016-04-06 etxjotj     Try again with HU65504
%%% R5A/13     2016-04-11 etxjotj     Backed out HU65504 again
%%% R5A/14     2016-04-20 etxjotj     Enabled HU65504 again
%%% R6A/1      2016-07-08 etxjotj     Deactivate with post actions
%%% R6A/2      2016-08-24 etxpejn     Exported add_backup_to_list
%%% R6A/3      2016-08-29 etxpejn     Added NameString to add_backup_to_list
%%% R7A/1      2016-10-07 etxjotj     Don't remove all backups in clear list
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([getMoAttribute/2, nextMo/3]).
-export([createMo/4,setMoAttribute/4,deleteMo/2]).
-export([prepare/3,commit/3,finish/3]).

-export([existsMo/2,
	 countMoChildren/3,
	 getMoAttributes/3,
	 setMoAttributes/3,
	 action/4,
	 createMo/5]).

-export([init_data/0, activate/0]).
-export([start/0]).
-export([get_usage_state/0, get_failsafe_backup/0]).
-export([is_failsafe_restart/0]).
-export([add_backup_to_list/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-include("RcsBrM.hrl").
-include("comte_types.hrl").
-include("SwmInternal.hrl").

-compile([nowarn_unused_vars]).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Initialize the system created BrmFailsafeBackup MO
%%% Also internal data can be initialized here
%%% @end
%%% ----------------------------------------------------------
init_data() ->
    case swmI:is_upgrade_ongoing() of
	true ->
	    [BrmFailsafe] = swmI:all_objects(brmFailsafeBackup),
	    case BrmFailsafe of
		{brmFailsafeBackup, Id, TimeRemaining, TimeoutLength, Backup,
		 UsageState, Progress} ->
		    %% HU49280
		    %% Enumeration changed when moving to BrM 3.4
		    NewUsageState = case UsageState of
					1 -> ?UsageState_IDLE;
					2 -> ?UsageState_ACTIVE;
					3 -> ?UsageState_BUSY
				    end,
		    mnesia:dirty_write(
		      #brmFailsafeBackup{brmFailsafeBackupId = Id,
					 backupName = Backup,
					 timeRemaining = TimeRemaining,
					 usageState = NewUsageState,
					 progressReport = Progress,
					 timeoutLength = TimeoutLength,
					 backup = Backup,
					 progress = Progress});
		_ ->
		    %% Utilize standard checks in copy_old_table
		    swmI:copy_old_table(brmFailsafeBackup)
	    end;
	false ->
	    mnesia:transaction(
	      fun() ->
		      mnesia:write(#brmFailsafeBackup{
				      brmFailsafeBackupId={"1","1","1","1","1"},
				      timeoutLength = 1200,
				      usageState = ?UsageState_IDLE})
	      end)
    end.

%%% ----------------------------------------------------------
%%% @doc Start the swmFailsafe server process
%%% @end
%%% ----------------------------------------------------------
start() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

%%% ----------------------------------------------------------
%%% @doc Return the current usage state
%%% @end
%%% ----------------------------------------------------------

get_usage_state() ->
    case get_state() of
	?UsageState_IDLE -> idle;
	?UsageState_ACTIVE -> active;
	?UsageState_BUSY -> busy
    end.

get_failsafe_backup() ->
    case mnesia:is_transaction() of
	true ->
	    do_get_failsafe_backup();
	false ->
	    {atomic, Name} = mnesia:transaction(fun do_get_failsafe_backup/0),
	    Name
    end.

do_get_failsafe_backup() ->
    [Obj] = mnesia:read({brmFailsafeBackup, {"1","1","1","1","1"}}),
    Obj#brmFailsafeBackup.backupName.

%%% ----------------------------------------------------------
%%% @doc Returns true if the restart is due to a failsafe
%%% @end
%%% ----------------------------------------------------------

is_failsafe_restart() ->
    case swmLib:get_variable(isFailsafe) of
	true ->
	    true;
	_ ->
	    false
    end.

%%% ----------------------------------------------------------
%%% @doc Activate phase of the failsafe mechanism
%%% @end
%%% ----------------------------------------------------------

activate() ->
    swmLib:erase_variable(isFailsafe).

%%% ----------------------------------------------------------
%%% @doc Returns true if the specified instance exists.
%%% @end
%%% ----------------------------------------------------------

-spec existsMo([binary()], integer()) -> boolean().

existsMo(DnRev, _) ->
    comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))).


%%% ----------------------------------------------------------
%%% @doc Returns the number of MO instances of given class directly below the specified parent.
%%% ----------------------------------------------------------

-spec countMoChildren([binary()], binary(), integer()) -> non_neg_integer().

countMoChildren(DnRev, Class, _) ->
    comsaGeneric:countMoChildren(DnRev, table(binary_to_list(Class))).


%%% ----------------------------------------------------------
%%% @doc Gets MO attribute values. 
%%% The instance is specified by ReversedDn and the attributes to be
%%% fetched are given by the AttrNames argument.
%%% ----------------------------------------------------------

getMoAttributes(AttrNames, DnRev, TxHandle) ->
    [getMoAttribute([AttrName|DnRev], TxHandle)||AttrName<-AttrNames].

%%% ----------------------------------------------------------
%%% @doc Get a single attribute
%%% @end
%%% ----------------------------------------------------------

%% Deprecated attributes
getMoAttribute([<<"progress">>|DnRev], TxHandle) ->
    getMoAttribute([<<"progressReport">>|DnRev], TxHandle);
getMoAttribute([<<"backup">>|DnRev], TxHandle) ->
    getMoAttribute([<<"backupName">>|DnRev], TxHandle);

getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).


%%% ----------------------------------------------------------
%%% @doc Iterator
%%% @end
%%% ----------------------------------------------------------


						%nextMo(Dn, Key, TxHandle) ->
nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

%%% ----------------------------------------------------------
%%% @doc Set multiple attributes
%%% @end
%%% ----------------------------------------------------------

setMoAttributes(Attrs, DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), Attrs).

%%% ----------------------------------------------------------
%%% @doc Set a single attribute
%%% @end
%%% ----------------------------------------------------------


setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).


%%% ----------------------------------------------------------
%%% @doc Creates a new instance. The given class name is trusted to be
%%% one of the names supported by this module.
%%% ----------------------------------------------------------

-spec createMo([binary()], 
	       mo_attribute_name(), 
	       binary(), 
	       [com_named_attribute()], 
	       integer()) -> 
		      {ok, tuple()}.

createMo([ClassName | ParentDnRev],
	 _KeyAttrName,
	 KeyValue,
	 InitAttrs,
	 _TransId) ->
    Table = table(binary_to_list(ClassName)),
    comsaGeneric:create(Table,
			ParentDnRev,
			KeyValue,
			values(InitAttrs),
			types(Table)).

createMo([Class|ParentDnRev], _, IxValueB, _) ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:create(Table, ParentDnRev, IxValueB).

%%% ----------------------------------------------------------
%%% @doc Never used because the MOC is system created
%%% @end
%%% ----------------------------------------------------------

deleteMo(_, _) ->
    ok.

table("BrmFailsafeBackup") -> brmFailsafeBackup.

types(brmFailsafeBackup) -> ?brmFailsafeBackup_types.

values([{Name, {_, Value}} | Tail]) ->
    [{Name, Value} | values(Tail)];
values([Attr | Tail]) ->
    [Attr | values(Tail)];
values([]) ->
    [].

prepare(_DN, User, _Tx) ->
    {ok,User}.
commit(_DN, User, _Tx) ->
    {ok,User}.
finish(_DN, _User, _Tx) ->
    ok.

action(Name, DnRev, Params, TransId) ->
    try do_action(Name, DnRev, Params, TransId)
    catch Type:Error ->
	    swmLib:write_swm_log("BrmFailsafeBackup", error, 
				 "Activation failed due to a software error"),
	    error_logger:error_report(
	      [{?MODULE, do_action, [Name, DnRev, Params, TransId]},
	       {Type, Error},
	       erlang:get_stacktrace()]),
	    ?INT32(?softwareFault)
    end.


do_action(<<"activate">>, _DnRev, _, _) ->
    %% HS42142 Add a timeout on executing a backup
    case gen_server:call(swmFailsafe, activate, 600000) of
	{ok, Result} ->
	    {Severity, Msg} = decode_return_value(Result),
	    swmLib:write_swm_log("BrmFailsafeBackup", Severity, Msg),
	    ?INT32(Result);
	Error ->
	    swmLib:write_swm_log("BrmFailsafeBackup", error, 
				 "Activation failed due to a software error"),
	    error_logger:error_report
	      ([{gen_server, call, [swmFailsafe, activate]},
		Error]),
	    %% Try deactivate just to be sure
	    gen_server:cast(swmFailsafe, {deactivate, "BrmFailsafeBackup"}),
	    ?INT32(?softwareFault)
    end;   
do_action(<<"deactivate">>, _DnRev, [{<<"postAction">>, PA}], _) ->
    gen_server:cast(swmFailsafe, {deactivate, "BrmFailsafeBackup", PA}),
    ?INT32(?actionStarted).

decode_return_value(?actionStarted) ->
    {info, "The failsafe configuration function was activated"};
decode_return_value(?functionBusy) ->
    {error, "The function is busy"};
decode_return_value(?missingParameter) ->
    {error, "A parameter is missing"};
decode_return_value(?softwareFault) ->
    {error, "A software error occurred"}.

%% make_dn([Value, Key|_]) ->
%%     binary_to_list(Key)++"="++binary_to_list(Value).

%% make_dn([], Accu) -> Accu;
%% make_dn([Value, Key], Accu) ->
%%     binary_to_list(Key)++"="++binary_to_list(Value)++Accu;
%% make_dn([Value, Key|DnRev], Accu) ->
%%     ","++binary_to_list(Key)++"="++binary_to_list(Value)++Accu.



%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
-record(state, {timerRef}).
init(_) ->
    mnesia:subscribe({table, brmFailsafeBackup, detailed}),
    %% HS45047
    case is_failsafe_restart() of
	true ->
	    info_msg("The failsafe has been activated~n",[]),
	    %% The failsafe was activated, reset the progress
	    mnesia:transaction(fun reset_progress_trans/0),
	    swmLib:init_action_id(failsafe),
	    {ok, #state{}};
	false ->
	    %% HS48694
	    TimerRef = maybe_initiate_countdown(),
	    Excluded = get_excluded_id(),
	    swmLib:init_action_id(failsafe, Excluded),
	    {ok, #state{timerRef = TimerRef}}
    end.

get_excluded_id() ->
    case mnesia:transaction(fun get_excluded_id_trans/0) of
	{atomic, Excluded} -> Excluded;
	{aborted, _} -> []
    end.

get_excluded_id_trans() ->
    Key = {"1","1","1","1","1"},
    [Obj] = mnesia:wread({brmFailsafeBackup,Key}),
    case Obj#brmFailsafeBackup.progress of
	undefined -> [];
	Progress ->
	    case Progress#'AsyncActionProgress'.actionId of
		ActionId when is_integer(ActionId) ->
		    [ActionId];
		_ ->
		    []
	    end
    end.


reset_progress_trans() ->
    Key = {"1","1","1","1","1"},
    [Obj] = mnesia:wread({brmFailsafeBackup,Key}),
    NewObj = Obj#brmFailsafeBackup{progress = undefined},
    mnesia:write(NewObj).

handle_call(activate, _, State) ->
    Id = swmLib:get_new_action_id(failsafe),
    case get_state() of
	?UsageState_BUSY ->
	    {reply, {ok, ?functionBusy}, State};
	_ ->
	    case swmLib:lock_action_capable(?SWM_FAILSAFE_ACTION_CAPABLE_ID, 
					    ?ACTIVATE_FAILSAFE_ACTION_CAPABLE_INFO) of
		ok ->
		    Time = comsaI:iso_time(os:timestamp(), basic),
		    BackupName = "Failsafe_backup_"++Time,
		    case create_backup(Id, BackupName) of
			?actionStarted ->
			    {ok, TimerRef} = timer:send_interval(1000, countdown),
			    NewState = State#state{timerRef = TimerRef},               
			    {reply, {ok, ?actionStarted}, NewState};
			Res ->
			    {reply, {ok, Res}, State}
		    end;
		{nok, _} ->
		    {reply, {ok, ?functionBusy}, State}
	    end
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({deactivate, Dn, PA}, State) ->
    case get_state() of
	?UsageState_BUSY ->
	    case swmLib:lock_action_capable(
		   ?SWM_FAILSAFE_ACTION_CAPABLE_ID, 
		   ?DEACTIVATE_FAILSAFE_ACTION_CAPABLE_INFO) of
		ok ->
		    timer:cancel(State#state.timerRef),
		    Id = swmLib:get_new_action_id(failsafe),
		    swmBackupModel:set_default_progress_report(
		      failsafe, "DEACTIVATE", Id), % HU36796
		    BackupName = update_db_deactivate(),
		    %% HU65504 Commit to disk before removing the backup file
		    swmDbMonitor:force_auto_backup(),
		    delete_backup(BackupName),
		    execute_post_action(PA, Dn),
		    ok = swmLib:unlock_action_capable(
			   ?SWM_FAILSAFE_ACTION_CAPABLE_ID),
		    {noreply, State#state{timerRef=undefined}};
		{nok, _}->
		    Msg = "Failsafe configuration function failed to "
			"deactivate. " ++ swmLib:get_action_capable_info(),
		    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
		    swmBackupModel:update_progress(
		      failsafe, 
		      [{result, ?ActionResultType_FAILURE},
		       {resultInfo, Msg},
		       {progressPercentage, 100},
		       {state, ?ActionStateType_FINISHED},
		       {timeActionCompleted, CompleteTime}]),
		    {noreply, State} 
	    end;
	_ ->
	    {noreply, State}
    end;    

handle_cast(timeout, State) ->
    swmLib:write_swm_log("BrmFailsafeBackup", critical, 
			 "Failsafe configuration timeout expired. "
			 "Initiating backup restore"),
    timer:cancel(State#state.timerRef),
    spawn(fun() -> restore_backup() end),
    {noreply, State#state{timerRef=undefined}};


handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(countdown, State) ->
    handle_countdown(),
    {noreply, State};
handle_info({mnesia_table_event, {write, brmFailsafeBackup, New, [Old], _}}, State) ->
    case New#brmFailsafeBackup.timeoutLength of
	Length when Length == Old#brmFailsafeBackup.timeoutLength ->
	    ok;
	Length ->
	    case get_state() of
		?UsageState_BUSY ->
		    swmLib:write_swm_log("BrmFallbackBackup=1", info, 
					 "The timeoutLength was changed to "
					 ++integer_to_list(Length)
					 ++" while the failsafe configuration function was activated.");
		_ ->
		    ok
	    end,
	    reset_counter()
    end,
    {noreply, State};

handle_info(Request, State) ->
    info_msg("~p~n",[Request]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% Description: Check if there is an ongoing countdown. Restart if necessary

maybe_initiate_countdown() ->
    case 
	mnesia:transaction(
	  fun() ->
		  [Obj] = mnesia:read({brmFailsafeBackup, {"1","1","1","1","1"}}),
		  Obj#brmFailsafeBackup.timeRemaining
	  end) of
	{atomic, undefined} ->
	    undefined;
	{atomic, Remaining} when is_integer(Remaining) ->
	    {ok, TimerRef} = timer:send_interval(1000, countdown),
	    TimerRef
    end.

%%% Descripton: Read the usage state of the failsafe function

get_state() ->
    {atomic, State} = 
	mnesia:transaction(
	  fun() ->
		  [Obj] = 
		      mnesia:read({brmFailsafeBackup, {"1","1","1","1","1"}}),
		  Obj#brmFailsafeBackup.usageState
	  end),
    State.

%%% Description: Set the usage state of the failsafe function

%% set_state(State) ->
%%     {atomic, ok} = 
%%  mnesia:transaction(
%%    fun() ->
%%        [Obj] = 
%%            mnesia:read({brmFailsafeBackup, {"1","1","1","1","1"}}),
%%        mnesia:write(Obj#brmFailsafeBackup{usageState=State})
%%    end),
%%     ok.

update_db_activate(BackupName) ->
    {atomic, ok} = 
	mnesia:transaction(
	  fun() ->
		  [Obj]=mnesia:wread({brmFailsafeBackup,{"1","1","1","1","1"}}),
		  NewObj = 
		      Obj#brmFailsafeBackup{
			usageState = ?UsageState_BUSY,
			timeRemaining = Obj#brmFailsafeBackup.timeoutLength,
			backupName = BackupName,
			backup = BackupName},
		  mnesia:write(NewObj)
	  end),
    swmDbMonitor:force_auto_backup(),
    ok.

update_db_deactivate() ->
    {atomic, BackupName} = 
	mnesia:transaction(
	  fun() ->
		  [Obj]=mnesia:wread({brmFailsafeBackup,{"1","1","1","1","1"}}),
		  NewObj = 
		      Obj#brmFailsafeBackup{
			usageState = ?UsageState_IDLE,
			timeRemaining = undefined,
			backupName = undefined,
			backup = undefined},
		  mnesia:write(NewObj),
		  Obj#brmFailsafeBackup.backupName
	  end),
    BackupName.

%% HU30730
%% Make sure all db transactions are complete before allowing housekeeping
%% on the failsafe backup

create_backup(Id, BackupName) ->
    swmBackupModel:set_default_progress_report(
      failsafe, "ACTIVATE", Id), % HU36796
    ManagerKey = {"1","1","1","1"},
    swmLib:set_variable(isFailsafe, true),
    NameBin = list_to_binary(BackupName),
    swmBackup:failsafe_lock(),
    try swmBackup:create_backup_common(NameBin,ManagerKey,system,failsafe) of
	MoRef ->
	    try post_create_backup(MoRef, BackupName)
	    after 
		Index = lists:last(string:tokens(MoRef, "=")),
		swmLib:unlock_backup(Index),
		ok = swmLib:unlock_action_capable(?SWM_FAILSAFE_ACTION_CAPABLE_ID),
		swmBackup:failsafe_release()        
	    end
    catch T:E ->
	    swmLib:erase_variable(isFailsafe),
	    sysInitI:error_report(
	      [{mfa, {swmBackup, create_backup_common, 
		      [NameBin, ManagerKey, system, failsafe]}},
	       {T,E},
	       erlang:get_stacktrace()]),
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    swmBackupModel:update_progress(failsafe, 
				      [{result, ?ActionResultType_FAILURE},
				       {resultInfo,"A software error occurred"},
				       {progressPercentage, 100},
				       {state, ?ActionStateType_FINISHED},
				       {timeActionCompleted, CompleteTime}]),
	    ok = swmLib:unlock_action_capable(?SWM_FAILSAFE_ACTION_CAPABLE_ID),
	    ?softwareFault
    end.

post_create_backup(MoRef, BackupName) ->
    swmLib:erase_variable(isFailsafe),
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    swmBackupModel:update_progress(failsafe, 
			      [{result, ?ActionResultType_SUCCESS},
			       {resultInfo, MoRef},
			       {progressPercentage, 100},
			       {state, ?ActionStateType_FINISHED},
			       {timeActionCompleted, CompleteTime}]),
    update_db_activate(BackupName),

    ?actionStarted.



delete_backup(BackupName) ->
    swmBackup:delete_system_created_backup(BackupName, failsafe),
    ok.

restore_backup() ->
    try do_restore_backup()
    catch Type:Error ->
	    error_logger:error_report(
	      [{mfa, {?MODULE, do_restore_backup, []}},
	       {Type, Error},
	       erlang:get_stacktrace()]),
	    erlang:Type(Error)
    end.

do_restore_backup() ->
    Name = update_db_deactivate(),
    swmBackup:restore_backup(Name).


%%% Description: Reset the counter to the value of the current timeoutLength

reset_counter() ->
    {atomic, ok} = 
	mnesia:transaction(
	  fun() ->
		  [Obj]=mnesia:read({brmFailsafeBackup, {"1","1","1","1","1"}}),
		  mnesia:write(Obj#brmFailsafeBackup{timeRemaining = Obj#brmFailsafeBackup.timeoutLength})
	  end),
    ok.

handle_countdown() ->
    case mnesia:transaction(
	   fun() ->
		   [Obj]=mnesia:read({brmFailsafeBackup,{"1","1","1","1","1"}}),
		   case Obj#brmFailsafeBackup.timeRemaining of
		       undefined -> % Countdown has been cancelled
			   undefined;
		       Remaining when Remaining > 0 ->
			   mnesia:write(Obj#brmFailsafeBackup{timeRemaining=Remaining-1}),
			   Remaining - 1;
		       _ ->
			   mnesia:write(Obj#brmFailsafeBackup{timeRemaining=0}),
			   timeout
		   end
	   end) of
	{atomic, timeout} ->
	    gen_server:cast(swmFailsafe, timeout);
	{atomic, _} ->
	    %% info_msg("Current: ~p~n",[Current])
	    ok
    end.

execute_post_action(undefined, Dn) ->
    Msg = "Failsafe configuration function deactivated",
    swmLib:write_swm_log(Dn, notice, Msg),
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    swmBackupModel:update_progress(
      failsafe, 
      [{result, ?ActionResultType_SUCCESS},
       {resultInfo, Msg},
       {progressPercentage, 100},
       {state, ?ActionStateType_FINISHED},
       {timeActionCompleted, CompleteTime}]),
    ok;
execute_post_action({_, ?BrmDeactivatePostAction_NOP}, Dn) ->
    Msg = "Failsafe configuration function deactivated",
    swmLib:write_swm_log(Dn, notice, Msg),
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    swmBackupModel:update_progress(
      failsafe, 
      [{result, ?ActionResultType_SUCCESS},
       {resultInfo, Msg},
       {progressPercentage, 100},
       {state, ?ActionStateType_FINISHED},
       {timeActionCompleted, CompleteTime}]),
    ok;
execute_post_action({T, ?BrmDeactivatePostAction_CLEAR_LIST}, Dn) ->
    %% swmBackup:delete_all_backups(),
    %% swmFallbackList:audit_fallback_list(),
    swmFallbackList:clear_fallback_list(),
    execute_post_action({T, ?BrmDeactivatePostAction_ADD_BACKUP_TO_LIST}, Dn);
execute_post_action({_, ?BrmDeactivatePostAction_ADD_BACKUP_TO_LIST}, Dn) ->
    add_backup_to_list(Dn, "Post_failsafe_backup_").

add_backup_to_list(Dn, NameString) ->
    Time = comsaI:iso_time(os:timestamp(), basic),
    BackupName = NameString++Time,

    %% Create backup
    ManagerKey = {"1","1","1","1"},
    NameBin = list_to_binary(BackupName),
    swmBackup:failsafe_lock(),
    try swmBackup:create_backup_common(NameBin,ManagerKey,system,failsafe) of
	MoRef ->
	    Index = lists:last(string:tokens(MoRef, "=")),
	    swmLib:unlock_backup(Index),
	    %% Add to escalation list
	    swmFallbackList:add_backup(latest, BackupName),
	    %% Failsafe release will trigger internal houskeeping
	    %% Make sure it's done after all updates are made
	    swmBackup:failsafe_release(),
	    Msg = "Failsafe configuration function deactivated. "
		"A current backup has been added to the escalation list",
	    swmLib:write_swm_log(Dn, notice, Msg),
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    swmBackupModel:update_progress(
	      failsafe, 
	      [{result, ?ActionResultType_SUCCESS},
	       {resultInfo, Msg},
	       {progressPercentage, 100},
	       {state, ?ActionStateType_FINISHED},
	       {timeActionCompleted, CompleteTime}]),
	    ok
    catch T:E ->
	    sysInitI:error_report(
	      [{mfa, {swmBackup, create_backup_common, 
		      [NameBin, ManagerKey, system, failsafe]}},
	       {T,E},
	       erlang:get_stacktrace()]),
	    Msg = "Failsafe configuration function deactivated. "
		"The system was unable to add a current backup to the "
		"escalation list. Make a scheduled backup as soon as possible.",
	    swmLib:write_swm_log(Dn, notice, Msg),
	    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
	    swmBackupModel:update_progress(
	      failsafe, 
	      [{result, ?ActionResultType_FAILURE},
	       {resultInfo, Msg},
	       {progressPercentage, 100},
	       {state, ?ActionStateType_FINISHED},
	       {timeActionCompleted, CompleteTime}]),
	    ok
    end.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.
%% info_msg(Format) ->
%%     info_msg(Format, []).
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%%error_msg(Format) ->
%%    error_msg(Format, []).
%% error_msg(Format, Args) ->
%%     sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

