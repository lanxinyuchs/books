%49%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmBackupModel.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2017
%%% @version /main/R11A/R12A/3

%%% @doc ==Backup managmement==
%%% This module contains the agent implementation of the ECIM BrM model.
%%% It does not include the population of the BrmBackup table

-module(swmBackupModel).
-vsn('/main/R11A/R12A/3').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
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
%% Rev      Date       Name    What
%% ----    ---------- -------  -------------------------------------------------
%% R11A/1  2017-09-13 etxjotj  Created
%% R11A/2  2017-09-15 etxjotj  Redirected import, export and delete
%%                             to swmBackupFile
%% ----    ---------- -------  -------------------------------------------------
%% R12A/1  2017-11-27 etxberb  SP277: Cancel action.
%% R12A/2  2017-12-06 emarnek  HW49544
%% R12A/3  2017-12-06 etxpejn  Restore not allowed on R-VNFM
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%% ----------------------------------------------------------
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

%%% Commonly used
-export([action_handler/2,
	 set_default_progress_report/3,
	 get_backup_by_name/1,
	 default_progress_report/2,
	 default_idle_pr/0,
	 update_progress/2,
	 make_dn/1,
	 record2props/1]).

%%% Used also in create backup
-export([validate_name/1, housekeeping/0]).

%%% COMSA
-export([format_next_scheduled_time/1]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% #2.3   IMPORTED FUNCTIONS
%%% ----------------------------------------------------------

-import(swmBackup, [rollback_restore_file/1,
                    is_protected_backup/1]).
-import(swmBackupFile, [remove_backup_dir/1]).


%%% ----------------------------------------------------------
%%% Includes
-include("RcsBrM.hrl").
-include("SwmInternal.hrl").
-include("comte_types.hrl").

%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% COMTE callbacks are not further documented here.
%%% Refer to COMTE documentation for that

existsMo(DnRev, _) ->
    comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))).

countMoChildren(DnRev, Class, _) ->
    comsaGeneric:countMoChildren(DnRev, table(binary_to_list(Class))).

getMoAttributes(AttrNames, DnRev, TxHandle) ->
    [getMoAttribute([AttrName|DnRev], TxHandle)||AttrName<-AttrNames].


%%% Next scheduled time is stored as a datetime tuple in universal time
%%% so that it is possible to adapt to different time zone settings
getMoAttribute([<<"nextScheduledTime">>|DnRev], _) ->
    Key = comsaGeneric:dnrev_to_key(DnRev),
    Table = table(comsaGeneric:class(DnRev)),
    [Obj] = mnesia:read(Table, Key),
    Value = format_next_scheduled_time(
	      Obj#brmBackupScheduler.nextScheduledTime),

    InternalType = proplists:get_value(nextScheduledTime, types(Table)),
    comsaEcimModelAdaptor:type(InternalType, Value);

getMoAttribute([Attribute= <<"timeRemainingBeforeRollback">>|DnRev], _) ->
    Key = {"1","1","1","1"},
    RestoreFile = rollback_restore_file(sysEnv:home_dir()),
    NewValue = 
	case file:consult(RestoreFile) of
	    {error, enoent} -> undefined;
	    {ok, Terms} ->
		Expiry = 
		    proplists:get_value(restore_expires, Terms),
		Timeout = calendar:datetime_to_gregorian_seconds(Expiry)-
		    calendar:datetime_to_gregorian_seconds(
		      calendar:universal_time()),
		if Timeout > 0 -> Timeout;
		   true -> undefined
		end
	end,
    [Obj] = mnesia:read({brmRollbackAtRestore, Key}),
    mnesia:write(Obj#brmRollbackAtRestore{
		   timeRemainingBeforeRollback = NewValue}),
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table));
		 
getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).


%%nextMo(Dn, Key, TxHandle) ->
nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).


setMoAttributes(Attrs, DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    [check_attribute(AttrName, Value)|| {AttrName, [{_, Value}]} <- Attrs],
    [check_attribute(AttrName, Value)|| {AttrName, {_, Value}} <- Attrs],
    comsaGeneric:set(DnRev, Table, types(Table), Attrs).

setMoAttribute([Attribute|DnRev], {_Type, Value}=TV, _, _) ->
    check_attribute(Attribute, Value),
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TV).

createMo([ClassName | ParentDnRev], _KeyAttrName, KeyValue, InitAttrs, Tid) ->
    Table = table(binary_to_list(ClassName)),
    comsaGeneric:create(Table,
			ParentDnRev,
			KeyValue,
			[],  % TODO: the correct way later...
			types(Table)),
    setMoAttributes(InitAttrs, [KeyValue, ClassName | ParentDnRev], Tid).

%%% @doc Comte callback 

createMo([Class|ParentDnRev], _, IxValueB, _) ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:create(Table, ParentDnRev, IxValueB).

%%% @doc Comte callback 
deleteMo(DnRev=[_,<<"BrmSingleEvent">>|_], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table);
deleteMo(DnRev=[_,<<"BrmPeriodicEvent">>|_], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table);
deleteMo(DnRev=[_,<<"BrmCalendarBasedPeriodicEvent">>|_], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table);
deleteMo(_, _) ->
    ok.

prepare(_DN, User, _Tx) ->
    {ok,User}.

commit(_DN, User, _Tx) ->
    {ok,User}.

finish(_DN, _User, _Tx) ->
    ok.

action(Action, DnRev, Params, TransId) ->
    try do_action(Action, DnRev, Params, TransId) of
	ok -> 
	    ?INT32(?actionStarted)
    catch 
	throw:{error, Code} ->
	    %% ?INT32(Code);
	    case Code of
		?nameValidationFailure -> 
		    {error, <<"Name validation failure">>};
		?duplicateName -> 
		    {error, <<"Duplicate name">>};
		?housekeepingRequired -> 
		    {error, <<"Housekeeping required">>};
		?backupNotFound -> 
		    {error, <<"Backup not found">>};
		?functionBusy -> 
		    {error, <<"Function busy">>};
		?missingParameter -> 
		    {error, <<"Missing parameter">>};
		?softwareFault -> 
		    {error, <<"Software fault">>};
		?failsafeBusy -> %HU82011
		    Msg = "Failsafe is active. Deactivate failsafe before "
			"creating a backup",
		    {error, list_to_binary(Msg)};
		?backupNotAllowedDuringUpgrade -> %HV41457
			{error, <<"Backup creation not allowed while system is upgrading">>}
	    end;
	throw:{fail, Msg} when is_binary(Msg) -> %HT12440, part 1
	    {error, Msg};
	throw:{fail, Msg} when is_list(Msg) ->
	    {error, list_to_binary(Msg)};
	throw:{lock_fail, Msg} when is_binary(Msg) ->
	    {error, Msg};
	throw:{lock_fail, Msg} when is_list(Msg) ->
	    {error, list_to_binary(Msg)};
	T:E ->
	    sysInitI:error_report(
	      [{mfa, {?MODULE, action, [Action, DnRev, Params, TransId]}},
	       {T,E},
	       erlang:get_stacktrace()]),
	    {error, <<"Software fault">>}
    end.

%%% ----------------------------------------------------------
%%% @doc Format a date time tuple stored in mnesia to the proper string value
%%% @end
%%% ----------------------------------------------------------

format_next_scheduled_time(NextUT) when is_tuple(NextUT) ->
    NextLT = calendar:universal_time_to_local_time(NextUT),
    format_date(NextLT);
format_next_scheduled_time(undefined) ->
    undefined.

%%% ----------------------------------------------------------
set_default_progress_report(mgr, Action, Id) ->
    Fun =
	fun() ->
		[Obj] = mnesia:wread({brmBackupManager, {"1", "1","1","1"}}),
		Progress = default_progress_report(Action, Id),
		mnesia:write(Obj#brmBackupManager{progressReport=Progress}),
		BuKeys = mnesia:all_keys(brmBackup),
		IdlePr = default_idle_pr(),
		[begin
		     [BuObj] = mnesia:read({brmBackup, Key}),
		     mnesia:write(BuObj#brmBackup{progressReport=IdlePr})
		 end||Key<-BuKeys]
	end,
    transaction(Fun);
set_default_progress_report(failsafe, Action, Id) ->
    Fun =
	fun() ->
		[Obj] =
		    mnesia:wread({brmFailsafeBackup, {"1","1","1","1","1"}}),
		Progress = default_progress_report(Action, Id),
		mnesia:write(Obj#brmFailsafeBackup{progressReport=Progress})
	end,
    transaction(Fun);
set_default_progress_report(BuKey, Action, Id) ->
    Fun =
	fun() ->
		[Mgr] = mnesia:read({brmBackupManager, {"1", "1","1","1"}}),
		IdlePr = default_idle_pr(),
		mnesia:write(Mgr#brmBackupManager{progressReport=IdlePr}),
		BuKeys = mnesia:all_keys(brmBackup),
		[begin
		     [BuObj] = mnesia:read({brmBackup, Key}),
		     mnesia:write(BuObj#brmBackup{progressReport=IdlePr})
		 end||Key<-BuKeys, Key /= BuKey],
		[Obj] = mnesia:wread({brmBackup, BuKey}),
		Progress = default_progress_report(Action, Id),
		mnesia:write(Obj#brmBackup{progressReport=Progress})
	end,
    transaction(Fun).

%%% ----------------------------------------------------------
transaction(Fun) ->
    case mnesia:is_transaction() of
	true ->
	    Fun();
	false ->
	    mnesia:transaction(Fun)
    end.


%%% ----------------------------------------------------------
default_progress_report(Action, Id) ->
    #'AsyncActionProgress'
	{actionName=Action,
	 additionalInfo=["Action started"],
	 progressInfo = "Action started",
	 progressPercentage=0,
	 result=?ActionResultType_NOT_AVAILABLE,
	 resultInfo="",
	 state=?ActionStateType_RUNNING,
	 actionId=Id,
	 timeActionStarted = comsaI:iso_time(os:timestamp(), extended),
	 timeActionCompleted= undefined,
	 timeOfLastStatusUpdate= comsaI:iso_time(os:timestamp(), extended)}.

default_idle_pr() ->
    undefined.

%%% ----------------------------------------------------------
%%% @doc Find the brmBackup object for backup with name Name
%%% Works both in transactional context and outside
%%% @end
%%% ----------------------------------------------------------

get_backup_by_name(BuName) ->
    case mnesia:is_transaction() of
	true ->
	    do_get_backup_by_name(BuName);
	false ->
	    mnesia:sync_dirty(fun() -> do_get_backup_by_name(BuName) end)
    end.

do_get_backup_by_name(BuName) ->
    WP = mnesia:table_info(brmBackup, wild_pattern),
    mnesia:match_object(WP#brmBackup{backupName=BuName}).

%%% ----------------------------------------------------------
%%% @doc A generic function for updating the AsyncActionProgress struct
%%%
%%% This function updates the AsyncActionProgress struct of various classes
%%% depending on the Key attribute


update_progress(undefined, _) ->
    ok;
update_progress(mgr, ProgressData) ->
    mnesia:transaction(
      fun() ->
    	      [Obj] = mnesia:read({brmBackupManager, {"1","1","1","1"}}),
    	      Old = case Obj#brmBackupManager.progressReport of
    			undefined ->
    			    default_progress_report("brmBackupManager", 0);
    			PR -> PR
    		    end,
    	      Progress =  comsaI:update_progress(ProgressData, Old),
    	      mnesia:write(Obj#brmBackupManager{progressReport=Progress})
      end);
update_progress(schedule, ProgressData) ->
    mnesia:transaction(
      fun() ->
    	      [Obj] = mnesia:read({brmBackupScheduler, {"1","1","1","1","1"}}),
    	      Old = case Obj#brmBackupScheduler.progressReport of
    			undefined ->
    			    default_progress_report("brmBackupScheduler", 0);
    			PR -> PR
    		    end,
    	      Progress =  comsaI:update_progress(ProgressData, Old),
    	      mnesia:write(Obj#brmBackupScheduler{progressReport=Progress})
      end);
update_progress(failsafe, ProgressData) ->
    mnesia:transaction(
      fun() ->
	      [Obj] = mnesia:read({brmFailsafeBackup, {"1","1","1","1","1"}}),
	      Old = case Obj#brmFailsafeBackup.progressReport of
			undefined ->
			    default_progress_report("brmFailsafeBackup", 0);
			PR -> PR
		    end,
	      Progress = comsaI:update_progress(ProgressData, Old),
	      mnesia:write(Obj#brmFailsafeBackup{progressReport = Progress,
						 progress = Progress})
      end);
update_progress(coli, ProgressData) ->
    Format =
	"=PROGRESS REPORT====~n" ++
	lists:flatten(lists:duplicate(length(ProgressData), "~-24s ~s~n")),
    io:format(Format ++ "~n",
	      [sysUtil:term_to_string(E) || E <- tuple_flatten(ProgressData)]);
update_progress(Key, ProgressData)->
    mnesia:transaction(
      fun() ->
	      [Obj] = mnesia:read({brmBackup, Key}),
	      Old = case Obj#brmBackup.progressReport of
			undefined ->
			    default_progress_report("brmBackup", 0);
			PR -> PR
		    end,
	      Progress =  comsaI:update_progress(ProgressData, Old),
	      mnesia:write(Obj#brmBackup{progressReport=Progress})
      end).

tuple_flatten([Tuple | Tail]) when is_tuple(Tuple) ->
    tuple_flatten(tuple_to_list(Tuple) ++ Tail);
tuple_flatten([E | Tail]) ->
    [E | tuple_flatten(Tail)];
tuple_flatten([]) ->
    [].

make_dn([Key, Value]) ->
    Key++"="++Value;
make_dn([Key, Value|Tail]) ->
    Key++"="++Value++","++make_dn(Tail).


record2props(Rec) when is_record(Rec, 'AsyncActionProgress') ->
    RecInfo = record_info(fields, 'AsyncActionProgress'),
    PrettyRec = elems2string(Rec),
    [_ | PrettyRecElems] = tuple_to_list(PrettyRec),
    lists:zip(RecInfo, PrettyRecElems);
record2props(Rec) when is_tuple(Rec) ->
    warning_msg("Unrecognized record ~w", [element(1, Rec) ]),
    [].

elems2string(#'AsyncActionProgress'{result = Result, state = State} = Rec) ->
    Rec#'AsyncActionProgress'{result = actionResultType(Result),
			      state = actionStateType(State)}.

%%% ----------------------------------------------------------
actionResultType(?ActionResultType_SUCCESS) ->
    "SUCCESS";
actionResultType(?ActionResultType_FAILURE) ->
    "FAILURE";
actionResultType(?ActionResultType_NOT_AVAILABLE) ->
    "NOT_AVAILABLE";
actionResultType(Other) ->
    sysUtil:term_to_string(Other).

%%% ----------------------------------------------------------
actionStateType(?ActionStateType_CANCELLING) ->
    "CANCELLING";
actionStateType(?ActionStateType_RUNNING) ->
    "RUNNING";
actionStateType(?ActionStateType_FINISHED) ->
    "FINISHED";
actionStateType(?ActionStateType_CANCELLED) ->
    "CANCELLED";
actionStateType(Other) ->
    sysUtil:term_to_string(Other).


%%% ----------------------------------------------------------
%%% @doc Runs an asynchronous ECIM action
%%% The Fun must set a default progress report

action_handler(Fun, ReportKey) ->

    try Fun() 
    catch
	throw:cancelled ->
	    handle_cancelled(ReportKey);
	throw:{read_file_info, {no_such_file, File}} ->
	    error_msg("No such file on ~w server: ~s~n", [get(protocol), File]),
	    ProgressInfo = "The action could not be completed",
	    handle_fail(ReportKey, ProgressInfo);
	throw:{read_file_info, {Reason, File}} ->
	    error_msg("~p: ~s~n", [Reason, File]),
	    ProgressInfo = "The action could not be completed",
	    handle_fail(ReportKey, ProgressInfo);
	throw:{fail, Message} -> % Operator action needed
	    handle_fail(ReportKey, Message);
	throw:Throw ->
	    warning_msg("throw ~p~n",[Throw]),
	    ProgressInfo = "The action could not be completed",
	    handle_fail(ReportKey, ProgressInfo);
	Type:Reason ->
	    ProgressInfo = "A software related error occured",
	    sysInitI:error_report([{Type, Reason}, 
				   erlang:get_stacktrace()]),
	    handle_fail(ReportKey,ProgressInfo)
    end.

handle_cancelled(ReportKey) ->
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(ReportKey, 
		    [{result, ?ActionResultType_FAILURE}, %HS80846
		     {state, ?ActionStateType_CANCELLED},
		     {progressPercentage, 100},
		     {timeActionCompleted, CompleteTime}]),
    cleanup().


handle_fail(ReportKey, ProgressInfo) ->
    CompleteTime = comsaI:iso_time(os:timestamp(), extended),
    update_progress(ReportKey,
		    [{result, ?ActionResultType_FAILURE},
		     {additionalInfo, ProgressInfo},
		     {state, ?ActionStateType_FINISHED},
		     {timeActionCompleted, CompleteTime}]),
    cleanup(),
    appmI:cancel_inhibit(), %% HW16373
    {failed, ProgressInfo}.


cleanup() ->
    swmLib:erase_ram_variable(mgr_point_of_no_return),
    swmLib:erase_ram_variable(restore_point_of_no_return),
    swmBackupFile:cleanup(),
    case sysEnv:rcs_mode_2() of
	target -> swmBackup:cleanup();
	simulated -> swmBackup:cleanup();
	vrcs -> swmvBackup:cleanup()
    end.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

check_attribute(<<"scheduledTime">>=Attribute, Value) ->
    try swmLib:parse_date(binary_to_list(Value)) of
	{_ , DateTime} ->
	    case valid_datetime(DateTime) of
		true -> 
		    true;
		false -> 
		    Msg = "Invalid date in "++binary_to_list(Attribute),
		    mnesia:abort(list_to_binary(Msg))
	    end
    catch T:E ->
	    sysInitI:info_report(
	      [{mfa,{swmLib, parse_date, [binary_to_list(Value)]}},
	       {T,E}]),
	    Msg = "The date string for "++binary_to_list(Attribute)++
		" cannot be parsed",
	    mnesia:abort(list_to_binary(Msg))
    end;

check_attribute(<<"startTime">>=Attribute, Value) ->
    try swmLib:parse_date(binary_to_list(Value))
    catch T:E ->
	    sysInitI:error_report(
	      [{mfa,{swmLib, parse_date, [binary_to_list(Value)]}},
	       {T,E}]),
	    Msg = "The date string for "++binary_to_list(Attribute)++
		" cannot be parsed",
	    mnesia:abort(list_to_binary(Msg))
    end;

check_attribute(<<"stopTime">>=Attribute, Value) ->
    try swmLib:parse_date(binary_to_list(Value)) of
	{_, {{Year,_,_}, _}} when Year > 2037 ->
	    Msg = "No dates after 2037 are allowed.",
	    mnesia:abort(list_to_binary(Msg));
	_ ->
	    true
    catch T:E ->
	    sysInitI:error_report(
	      [{mfa,{swmLib, parse_date, [binary_to_list(Value)]}},
	       {T,E}]),
	    Msg = "The date string for "++binary_to_list(Attribute)++
		" cannot be parsed",
	    mnesia:abort(list_to_binary(Msg))
    end;

check_attribute(_Attribute, _Value) ->
    ok.

valid_datetime({Date,Time}) ->
    case calendar:valid_date(Date) of
	true ->
	    case Time of
		{H,M,S} when H<24, M < 60, S < 60 -> 
		    %% HS37092 fix
		    if element(1,Date) > 2037 -> false;
		       true -> true
		    end;
		_ -> false
	    end;
	false ->
	    false
    end.

%%% ----------------------------------------------------------
%%% @doc Helper function for comsa usage

table("BrM") -> brM;
table("BrmBackupManager") -> brmBackupManager;
table("BrmBackup") -> brmBackup;
table("BrmBackupScheduler") ->  brmBackupScheduler;
table("BrmCalendarBasedPeriodicEvent") ->  brmCalendarBasedPeriodicEvent;
table("BrmSingleEvent") -> brmSingleEvent;
table("BrmPeriodicEvent") -> brmPeriodicEvent;
table("BrmBackupHousekeeping") ->  brmBackupHousekeeping;
table("BrmRollbackAtRestore") -> brmRollbackAtRestore;
table("BrmBackupLabelStore") -> brmBackupLabelStore.

%%% @doc Helper function for comsa usage

types(brM) -> ?brM_types;
types(brmBackupManager) -> ?brmBackupManager_types;
types(brmBackup) -> ?brmBackup_types;
types(brmBackupScheduler) -> ?brmBackupScheduler_types;
types(brmCalendarBasedPeriodicEvent) -> ?brmCalendarBasedPeriodicEvent_types;
types(brmSingleEvent) -> ?brmSingleEvent_types;
types(brmPeriodicEvent) -> ?brmPeriodicEvent_types;
types(brmBackupHousekeeping) -> ?brmBackupHousekeeping_types;
types(brmRollbackAtRestore) -> ?brmRollbackAtRestore_types;
types(brmBackupLabelStore) -> ?brmBackupLabelStore_types.



%%% Actions on BrmBackupManager


do_action(<<"createBackup">>, DnRev, Params, _) -> 
    %% HT56129 fix
    Default = 
	case get_manual_name(DnRev) of
	    undefined -> "MANUAL-";
	    ManualBackupName -> ManualBackupName++"-"
	end++comsaI:iso_time(os:timestamp(), extended),
    Name = get_value("name", Params, {9, list_to_binary(Default)}),

    case validate_name(Name) of
	ok ->
	    ok;
	duplicate_name ->
	    throw({error, ?duplicateName})
    end,

    case housekeeping() of
	ok ->
	    ok;
	max_reached ->
	    throw({error, ?housekeepingRequired});
	{cleaned, _} ->
	    ok
    end,

    %% HU82011 Prevent create when in failsafe
    case swmFailsafe:get_usage_state() of
	idle -> 
	    ok;
	_ ->
	    throw({error, ?failsafeBusy})
    end,
	
    %% HV41457 Prevent manual backup while system is upgrading
	case swmI:is_upgrade_ongoing() of
		false ->
			ok;
		_ ->
			throw({error, ?backupNotAllowedDuringUpgrade})
	end,
    
    ActionId = swmLib:get_new_action_id(mgr),
    swmLib:mo_lock_action_capable(?CREATE_BACKUP_ACTION_CAPABLE_ID,
                                  ?CREATE_BACKUP_ACTION_CAPABLE_INFO),
    set_default_progress_report(mgr, "CREATE", ActionId),
    gen_server:cast(swmBackup, {createBackup, Name, DnRev, ActionId});

do_action(<<"deleteBackup">>, _, Params, _) -> 
    Name = get_value("name", Params),
    case get_backup_by_name(binary_to_list(Name)) of
	[] ->
	    throw({error, ?backupNotFound});
	_ ->
	    ActionId = swmLib:get_new_action_id(mgr),
	    swmLib:mo_lock_action_capable(?DELETE_BACKUP_ACTION_CAPABLE_ID,
					  ?DELETE_BACKUP_ACTION_CAPABLE_INFO),
	    set_default_progress_report(mgr, "DELETE", ActionId),
	    %% gen_server:cast(swmBackup,  {deleteBackup, Name, ActionId})
	    swmBackupFile:delete_backup(Name),
	    ok
    end;
do_action(<<"importBackup">>, DnRev, Params, _) -> 
    UBin = get_value("uri", Params),
    PBin = get_value("password", Params, ?STRING(<<"">>)),
    Passwd = binary_to_list(PBin),
    Uri = binary_to_list(UBin),
    ActionId = swmLib:get_new_action_id(mgr),
    swmLib:mo_lock_action_capable(?IMPORT_BACKUP_ACTION_CAPABLE_ID,
                                  ?IMPORT_BACKUP_ACTION_CAPABLE_INFO),
    set_default_progress_report(mgr, "IMPORT", ActionId),
    %% gen_server:cast(swmBackup, {importBackup, DnRev, Uri, Passwd, ActionId});
    swmBackupFile:import_backup(DnRev, Uri, Passwd),
    ok;

%%% Actions on BrmBackup

do_action(<<"export">>, DnRev, Params, _) ->
    UBin = get_value("uri", Params),
    PBin = get_value("password", Params, ?STRING(<<"">>)),
    Uri = binary_to_list(UBin),
    Passwd = binary_to_list(PBin),
    Key = comsaGeneric:dnrev_to_key(DnRev), 
    ActionId = swmLib:get_new_action_id(Key),
    swmLib:mo_lock_action_capable(?EXPORT_BACKUP_ACTION_CAPABLE_ID,
                                  ?EXPORT_BACKUP_ACTION_CAPABLE_INFO),
    set_default_progress_report(Key, "EXPORT", ActionId),
    %% gen_server:cast(swmBackup, {export, Key, Passwd, Uri, ActionId});
    swmBackupFile:export_backup(Key, Passwd, Uri),
    ok;
do_action(<<"restore">>, DnRev, _, _) ->
    Key = comsaGeneric:dnrev_to_key(DnRev), 
    case swmI:node_type() of
	"R-VNFM" ->
	    throw({fail, list_to_binary("Operation refused, restore not supported")});
	_Else ->
	    do_nada
    end,
    %% HT12440 fix, part 2
    case swmServer:check_up_states_for_restore() of
	ok ->
	    ok;
	{error, {Up, State}} ->
	    Fmt = "Operation refused because UpgradePackage=~s is in state ~s.",
	    Msg = lists:flatten(io_lib:format(Fmt,[Up, State])),
	    throw({fail, list_to_binary(Msg)})
    end,
    %% Fix ends here

    ActionId = swmLib:get_new_action_id(Key),
    swmLib:mo_lock_action_capable(?RESTORE_BACKUP_ACTION_CAPABLE_ID,
                                  ?RESTORE_BACKUP_ACTION_CAPABLE_INFO),
    case swmvBuRestore:is_ongoing() of
	false ->
	    set_default_progress_report(Key, "RESTORE", ActionId);
	true ->
	    ok
    end,
    gen_server:cast(swmBackup, {restore, Key, ActionId});
do_action(<<"cancelCurrentAction">>, [_, <<"BrmBackup">>|_] = DnRev, _, _) ->
    Key = comsaGeneric:dnrev_to_key(DnRev),
    ProgressData = [{state, ?ActionStateType_CANCELLING}],
    %% Normal update_progress will create a progress report if no job is ongoing
    %% We don't want that here
    Fun = fun() ->
		  [Obj] = mnesia:read({brmBackup, Key}),
		  case Obj#brmBackup.progressReport of
		      undefined -> undefined;
		      PR ->
			  NewPR = comsaI:update_progress(ProgressData, PR),
			  mnesia:write(Obj#brmBackup{progressReport=NewPR})
		  end
	  end,
				      
    case swmLib:get_ram_variable(restore_point_of_no_return) of
	Key -> 
	    throw({error, ?functionBusy});
	_ ->
	    info_msg("Cancel ~p acknowledged~n",[Key]),

	    mnesia:transaction(Fun),
	    swmvBuRestore:cancel(#{info => "Cancel ordered"}),
	    swmBackup ! {cancel, Key},
	    ok
    end;
do_action(<<"cancelCurrentAction">>, [_, <<"BrmBackupManager">>|_], _, _) ->
    ProgressData = [{state, ?ActionStateType_CANCELLING}],
    Fun = 
	fun() ->
		[Obj] = mnesia:read({brmBackupManager, {"1","1","1","1"}}),
		case Obj#brmBackupManager.progressReport of
		    undefined -> undefined;
		    PR -> 
			NewPR = comsaI:update_progress(ProgressData, PR),
			mnesia:write(Obj#brmBackupManager{progressReport=NewPR})
		end
	end,
    
    case swmLib:get_ram_variable(mgr_point_of_no_return) of
	true ->
	    throw({error, ?functionBusy});
	_ ->
	    info_msg("Cancel mgr acknowledged~n"),
	    mnesia:transaction(Fun),
	    swmBackup!cancel_mgr,
	    ok
    end;
do_action(<<"confirmRestore">>, _, _, _) ->
    gen_server:cast(swmBackup, confirmRestore),
    ok.





get_value(Key, Params) ->
    case proplists:get_value(list_to_binary(Key), Params) of
	{_, Value} ->
	    Value;
	_ ->
	    throw({error, ?missingParameter})
    end.
get_value(Key, Params, Default) ->
    case proplists:get_value(list_to_binary(Key), Params, Default) of
	{_, Value} ->
	    Value;
	undefined ->
	    element(2, Default);
	_ ->
	    throw({error, ?missingParameter})
    end.

validate_name(NameB) when is_binary(NameB)->
    Name = binary_to_list(NameB),
    validate_name(Name);
validate_name(Name) when is_list(Name)->
    Fun = fun() -> do_validate_name(Name) end,
    case mnesia:transaction(Fun) of
	{atomic, ok} ->
	    ok;
	{atomic, duplicate_name} ->
	    duplicate_name
    end.

do_validate_name(Name) ->
    Key = mnesia:first(brmBackup),
    do_validate_name(Name, Key).

do_validate_name(_, '$end_of_table') ->
    ok;
do_validate_name(Name, Key) ->
    [BrmBackup] = mnesia:read({brmBackup, Key}),
    case BrmBackup#brmBackup.backupName of
	Name ->
	    duplicate_name;
	_ ->
	    Next = mnesia:next(brmBackup, Key),
	    do_validate_name(Name, Next)
    end.    

housekeeping() ->
    {atomic, Result} = mnesia:transaction(fun() -> do_housekeeping() end),
    case Result of
	{cleaned, Indices} ->
	    [case remove_backup_dir(Index) of
		 ok ->
		     Msg = "Backup "++BackupName++" removed automatically "
			 "due to housekeeping.",
		     swmLib:write_swm_log("BrmBackupManager", info, Msg);
		 {error, backup_busy} ->
		     ok
	     end ||{Index, BackupName}<-Indices],
	    {cleaned, Indices};
	Result ->
	    Result
    end.

do_housekeeping() ->
    [HK] = mnesia:read({brmBackupHousekeeping,{"1","1","1","1","1"}}),
    Type = ?BrmBackupCreationType_MANUAL,
    WP = mnesia:table_info(brmBackup, wild_pattern),
    Pattern = WP#brmBackup{creationType = Type},
    Backups = mnesia:match_object(Pattern),

    case {HK#brmBackupHousekeeping.autoDelete, length(Backups)} of
	{?BrmManualBackupAutoDelete_DISABLED, N} 
	  when N>=HK#brmBackupHousekeeping.maxStoredManualBackups ->
	    max_reached;
	{?BrmManualBackupAutoDelete_ENABLED, N}
	  when N>=HK#brmBackupHousekeeping.maxStoredManualBackups ->
	    Remove = N-HK#brmBackupHousekeeping.maxStoredManualBackups+1,
	    Sorted = lists:keysort(#brmBackup.creationTime, Backups),
	    Indices = housekeeping_autodelete(Remove, Sorted),
	    {cleaned, Indices};
	_ ->
	    ok
    end.

housekeeping_autodelete(0, _) ->
    [];
housekeeping_autodelete(_, []) ->
    [];
housekeeping_autodelete(N, [Backup|Sorted]) ->
    BackupName = Backup#brmBackup.backupName,
    case is_protected_backup(BackupName) of
        true -> housekeeping_autodelete(N, Sorted);
        false -> Key = Backup#brmBackup.brmBackupId,
                 ok = mnesia:delete({brmBackup, Key}),
                 [{lists:last(tuple_to_list(Key)), BackupName}|
                 housekeeping_autodelete(N-1, Sorted)]
    end.

%%% ----------------------------------------------------------
%%% LIBRARY
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% Description: Format a datetime to a string

format_date({{Y,M,D},{H,Mi,S}}) ->
    lists:append([integer_to_list(Y), "-", padzero(M), "-", padzero(D),"T",
		  padzero(H), ":", padzero(Mi), ":", padzero(S)]).

%%% ----------------------------------------------------------
%%% Description: Add a zero to a single digit number and return 
%%%              a two digit string.

padzero(N) ->
    if N<10 -> [$0, N+$0];
       true -> integer_to_list(N)
    end.


get_manual_name(DnRev) ->
    BrmBackupManagerKey = comsaGeneric:dnrev_to_key(DnRev), 
    case mnesia:dirty_read({brmBackupManager,BrmBackupManagerKey}) of
	[Obj] ->
	    Obj#brmBackupManager.manualBackupName;
	[] ->
	    undefined
    end.



info_msg(Format) ->
    info_msg(Format, []).
info_msg(Format, Args) ->
    try
	sysInitI:info_msg("~w: "++Format, [?MODULE|Args])
    catch
	_ : _ ->
	    %% During startup, before sysInitI has started!
	    error_logger:info_msg("~w: "++Format, [?MODULE|Args])
    end.

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
warning_msg(Format, Args) ->
    try
	sysInitI:warning_msg("~w: "++Format, [?MODULE|Args])
    catch
	_ : _ ->
	    %% During startup, before sysInitI has started!
	    error_logger:warning_msg("~w: "++Format, [?MODULE|Args])
    end.

%% error_msg(Format) ->
%%     error_msg(Format, []).
error_msg(Format, Args) ->
    try
	sysInitI:error_msg("~w: "++Format, [?MODULE|Args])
    catch
	_ : _ ->
	    %% During startup, before sysInitI has started!
	    error_logger:error_msg("~w: "++Format, [?MODULE|Args])
    end.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
