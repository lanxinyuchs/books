%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfImmUgMaster.erl %
%%% Author:	erarafo
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(gmfImmUgMaster).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/2').
-date('2017-03-24').
-author('ecaiyan').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% R2A/1      2013-05-28 erarafo     Initial version.
%%% R2A/1      2013-05-28 erarafo     Created
%%% R2A/9      2013-06-04 erarafo     swmI:activation_complete() added
%%% R2A/10     2013-06-05 erarafo     Handling of fatal errors
%%% R2A/14     2013-06-13 etxjotj     Reduced logging
%%% R2A/15     2013-06-14 erarafo     Merged changes, ancestor was R2A/10
%%% R2A/16     2013-06-16 erarafo     ICTI support
%%% R2A/17     2013-06-17 erarafo     Just two levels of versioning
%%% R2A/18     2013-06-17 erarafo     Corrections to readSchemaVersion
%%% R2A/19     2013-06-19 erarafo     #oldClassInfo{} introduced
%%% R2A/20     2013-06-20 erarafo     Improved logging
%%% R2a/21     2013-06-27 erarafo     Refactoring
%%% R2A/22     2013-06-28 erarafo     Refactoring and cleanup
%%% R2A/23     2013-08-28 erarafo     Refactoring
%%% R2A/24     2013-08-28 erarafo     Version reporting
%%% R2A/25     2013-09-05 erarafo     Log message clarified
%%% R2A/26     2013-09-05 erarafo     Finalize handling improved
%%% R2A/27     2013-09-11 erarafo     Idle messages; handle unnamed S2
%%% R2A/28     2013-09-12 erarafo     S0 named, get schema version
%%% R2A/29     2013-09-16 erarafo     Support for pre-restart schema check
%%% R2A/30     2013-09-19 erarafo     eliminated #schema{}
%%% R2A/31     2013-09-24 erarafo     Support for pre-CLI-extension From-version
%%% R2A/32     2013-09-24 erarafo     Support for pre-CLI-extension From-version
%%% R2A/33     2013-09-24 erarafo     Bug in gmfCxpRev conversion fixed
%%% R2A/34     2013-09-27 erarafo     Adapted to #ldap{} change in safs_imm_db
%%% R2A/35     2013-10-03 erarafo     Edoc, -spec, logging
%%% R2A/36     2013-10-22 erarafo     Copyinstances error cases handled
%%% R2A/37     2013-10-24 erarafo     3-level schema versioning, partial support
%%% R2A/38     2013-10-25 erarafo     Restoring SAF LOG instances in upgrade
%%% R2A/39     2013-10-28 erarafo     Mark runtime classes as immediately handled
%%% R2A/40     2013-10-28 erarafo     Fixed readSchemaVersion bug
%%% R2A/41     2013-10-28 erarafo     Handling of runtime classes cleaned up
%%% R2A/42     2013-10-29 erarafo     Early suppression of runtime instances,
%%%                                     reject unknown class in waitForClasses
%%% R2A/43     2013-11-12 erarafo     Adapted to changes in SAFs
%%% R2A/44     2014-01-12 erarafo     Support failure at commit, deferred
%%%                                   reporting of versioning failures
%%% R2A/45     2014-01-13 erarafo     Use safs_imm_db:get_imm_objects_payload/1
%%% R2A/46     2014-01-15 erarafo     Restored lost code in commit
%%% R2A/47     2014-01-15 erarafo     Restored lost code in commit properly
%%% R2A/48     2014-01-17 erarafo     Improved logging of fail upgrade
%%% R2A/49     2014-01-18 erarafo     Correction for A -> B -> C upgrade
%%% R2A/50     2014-02-05 erarafo     Multiple diagnostics from failUpgrade
%%% R2A/51     2014-02-12 erarafo     Defer shutdown until no more ICTI sessions
%%% R2A/52     2014-03-06 erarafo     HS34720
%%% R2A/53     2014-03-10 erarafo     Asynchronous addInstanceGroups
%%% R2A/54     2014-03-11 erarafo     Asynchronous addInstanceGroups, fixed issue
%%% R2A/55     2014-03-12 erarafo     Extra notify for unlikely no-classes case
%%% R2A/56     2014-03-18 erarafo     Added events for restart log
%%% R2A/57     2014-03-25 erarafo     Refactoring
%%% R2A/58     2014-04-17 erarafo     Handle SA_NO_DANGLING
%%% R2A/59     2014-05-05 erarafo     Revised error handling
%%% R2A/60     2014-05-16 erarafo     HS60956: Return type of commit/0 adjusted
%%% R2A/61     2014-05-21 erarafo     HS62261: Severity WARNING when user fault
%%% R2A/62     2014-05-21 erarafo     Dead code removal, comments adjusted
%%% R2A/63     2014-05-22 erarafo     HS62272: Full support for 'confirm'
%%% R2A/64     2014-05-22 erarafo     HS62272: Cleanup
%%% R2A/65     2014-06-16 erarafo     Dump from-state data for offline analysis
%%% R2A/68     2014-07-01 erarafo     Adjustment for OTP 17
%%% R2A/69     2014-07-31 etxberb     Corrected cause value for AVLI.
%%% R2A/70     2014-08-14 erarafo     Performance lift related to HS84194
%%% R3A/1      2014-11-06 erarafo     Persistent runtime attributes
%%% R3A/2      2014-11-07 erarafo     Refactoring
%%% R3A/3      2014-11-11 erarafo     Handle writeInstances when pers runtime
%%% R3A/4      2014-11-13 erarafo     Improved logging
%%% R3A/6      2014-12-15 erarafo     Redundant code cleanup
%%% R3A/7      2014-12-16 erarafo     Improved logging
%%% R3A/8      2015-01-15 etxpeno     Support for regular role
%%% R3A/9      2015-02-12 erarafo     Use 'not handled' as ordered set
%%% R3A/10     2015-03-04 erarafo     Replay support
%%% R3A/11     2015-03-04 erarafo     Replay support, handle OamAccessPoint
%%% R3A/12     2015-03-05 erarafo     Refactoring
%%% R3A/13     2015-03-09 erarafo     Startup timing
%%% R3A/15     2015-03-17 erarafo     Stack trace if dump data fails
%%% R3A/16     2015-03-31 erarafo     Log entry format improved
%%% R3A/17     2015-04-01 erarafo     Finalize hangs until conversion complete
%%% R3A/18     2015-04-22 erarafo     Support for RT object implementer
%%% R3A/19     2015-04-23 erarafo     copyInstances supports implementer
%%% R3A/20     2015-04-23 erarafo     Edoc fixed
%%% R4A/1      2015-02-12 erarafo     No change
%%% R4A/2      2015-04-20 etxpeno     Merge (R3A/10-17)
%%% R4A/3      2015-04-23 etxpeno     Merge (R3A/18-20)
%%% R4A/4      2015-04-29 erarafo     HT68612 full solution, correction
%%% R4A/5      2015-04-30 erarafo     Fault handling, removed unnecessary notify()
%%% R4A/6      2015-04-30 erarafo     Fault handling, adjustment
%%% R4A/7      2015-05-19 erarafo     New insertion algorithm
%%% R4A/8      2015-05-22 erarafo     Back-references handling
%%% R4A/9      2015-05-25 erarafo     Initialization speedup
%%% R4A/10     2015-05-26 erarafo     Cleanup
%%% R4A/11     2015-05-26 erarafo     Moved tables to the inserter
%%% R4A/12     2015-05-26 erarafo     Restructured for fewer messages
%%% R4A/13     2015-05-28 erarafo     Scoreboard process eliminated
%%% R4A/14     2015-05-29 erarafo     Simplified use of records
%%% R4A/15     2015-06-10 erarafo     Redundant records removed
%%% R4A/16     2015-06-26 erarafo     Replay feature removed
%%% R4A/17     2015-07-04 erarafo     Minor refactoring
%%% R4A/18     2015-09-16 erarafo     "Wait for classes" cleanup
%%% R4A/19     2015-09-16 erarafo     Using sysInitI for logging
%%% R4A/20     2015-10-07 erarafo     Comments only, about tracing
%%% R4A/21     2015-10-22 erarafo     Switch off notifications during data conversion
%%% R5A/1      2015-11-17 etxpeno     remove dead code
%%% R6A/1      2016-05-13 erarafo     getOldStructClasses, tentatively added, not used yet
%%% R6A/2      2016-06-07 erarafo     WP5493, resurrection of struct instances
%%% R6A/3      2016-06-08 erarafo     WP5493, adjusted handling of 'implementer'
%%% R6A/4      2016-06-13 erarafo     LTTng trace adjusted
%%% R6A/5      2016-06-16 erarafo     WP5493, correction
%%% R6A/6      2016-08-01 uabesvi     HV12688 do not remove safApp=safLogService
%%% R6A/7      2016-08-01 eolaand     Revert to previous vsn before HV12688 fix
%%% R6A/8      2016-07-01 usbesvi     HV12688 do not remove logs when emptying IMM
%%% R6A/9      2016-08-16 erarafo     Comments only
%%% R6A/10-12  2016-09-16 etxpeno     MIB sync improvements
%%% R7A/1      2016-09-08 etxpeno     Dialyzer fixes
%%% R7A/3      2016-09-16 etxpeno     Uplift from R6A/12
%%% R8A/1      2016-11-24 erarafo     add support for safcImmCtReadInstances_2
%%% R8A/2      2016-11-28 erarafo     -spec narrowed
%%% R8A/3      2016-12-19 erarafo     Refactoring
%%% R8A/4      2017-01-03 ecaiyan     Code cleaning
%%% R8A/5      2017-01-03 ecaiyan     Remove redudant printout
%%% R9A/1      2017-02-28 ecaiyan     Remove Delay&MTC check
%%% R9A/2      2017-03-24 ecaiyan     Redirect safcImmCtInitialize_2 to safcImmCtInitialize


-define(SERVER, ?MODULE).
-define(TIMEOUT, 10000).
-define(TIMEOUT_IMMEDIATELY, 0).
-define(VERIFY_UPGRADE_MODULE, gmfImmUgVerifyUpgrade).

%% Must be less that the POST_REBOOT_ACTIVATE_TIMEOUT in swmServer
%% -define(REPORT_OFFENDERS_TIMEOUT, 100).

-include("gmfMeta.hrl").

%% OTP worker interface
-export([start/0
	]).

%% Callback from SWM
-export([confirm/0
	]).

%% ICTI interface
-export([initIctiSession/1,
	 finSync/3,
	 readSchemaVersion/2,
	 failUpgrade/3,
         readInstances/5,
	 copyInstances/4,
	 waitForClasses/4]).

%% Data conversion internals
-export([setInserterReady/0,
	 isReady/0,
	 notify/2,
	 setRestartRequested/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).


% WP5493
-export([ getOldStructClasses/0]).

-include("gmf.hrl").
-include("gmfImmUg.hrl").
-include("safs_imm_ct_p.hrl").


%% @doc This function is referred from gmfDataInit:children().
%% It will start the server if the context is upgrade.

start() ->
    ?INFO("version ~p, using gmfImmUgIcti ~p and gmfImmUgLib ~p",
	  [gmfImmUgLib:getVersion(?MODULE),
	   gmfImmUgIcti:getVersion(),
	   gmfImmUgLib:getVersion()
	  ]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%% ----------------------------------------------------------
%%% @doc Tells the master that the inserter is ready to
%%% accept instance groups.
%%% @end
%%% ----------------------------------------------------------
-spec setInserterReady() -> ok.

setInserterReady() ->
    gen_server:cast(?SERVER, {setInserterReady}).

%%% ----------------------------------------------------------
%%% @doc Returns true if the master and the inserter is ready.
%%% @end
%%% ----------------------------------------------------------
-spec isReady() -> boolean().

isReady() ->
    ?CALL(?SERVER, {isReady}).


%%% ----------------------------------------------------------
%%% @doc Returns an ICTI session handle.
%%% @end
%%% ----------------------------------------------------------
-spec initIctiSession(boolean()) -> any().

initIctiSession(_Delay) ->
    ?CALL(?SERVER, {initIctiSession, false}).


%%% ----------------------------------------------------------
%%% @doc SWM callback function. Normally 'ok' is returned.
%%% If the returned value is {error, Message} we trust that
%%% SWM enforces a restart.
%%%
%%% There are two {error, _} cases, both considered
%%% impossible.
%%% @end
%%% ----------------------------------------------------------

-spec confirm() -> ok | {error, string()}.

confirm() ->
    ?INFO("trigged: confirm", []),
    case whereis(?SERVER) of
	undefined ->
	    %% There is no reason why this should ever be the case.
	    ?FAULT([], "process not running: ~w", [?SERVER]),
	    {error, "internal error: the gmfImmUgMaster process is not running"};
	Pid ->
	    ?CALL(Pid, {confirm})
    end.





%% @doc Teardown of ICTI session.

-spec finSync(handle(), pid(), reference()) -> ok.

finSync(Handle, Pid, Ref) ->
    gen_server:cast(?SERVER, {finSync, Handle, Pid, Ref}).


%% @doc ICTI readSchemaVersion.

-spec readSchemaVersion(handle(), schemaName()) ->
	  {ok, #safsImmCtSchemaVersion{}, #safsImmCtSchemaVersion{}}
	      | {error, sa_ais_err_bad_handle}
	      | {error, sa_ais_err_not_exist}.

readSchemaVersion(Handle, SchemaName) ->
    ?CALL(?SERVER, {readSchemaVersion, Handle, SchemaName}).


%% @doc ICTI readInstances.

-spec readInstances(handle(), [className()], any(), atom(), boolean()) ->
	  {ok, [{#oldClassInfo{}, #instanceGroup{}}]} |
	      {error, sa_ais_err_bad_handle} |
	      {error, sa_ais_err_not_exist}.

readInstances(Handle, ClassNames, AppPid, Op, IncludeAll) ->
    ?CALL(?SERVER, {readInstances, Handle, ClassNames, AppPid, Op, IncludeAll}).


%% @doc ICTI failUpgrade.

-spec failUpgrade(handle(), integer(), binary()) ->
	  ok |
	  {error, sa_ais_err_bad_handle} |
	  {error, sa_ais_err_bad_operation}.

failUpgrade(Handle, AppPid, Message) ->
    ?CALL(?SERVER, {failUpgrade, Handle, AppPid, Message}).


%% @doc ICTI copyInstances

-spec copyInstances(handle(), integer(), [className()], implementer()) ->
	  ok |
	  {error, sa_ais_err_bad_handle} |
	  {error, sa_ais_err_invalid_param} |
	  {error, sa_ais_err_not_exist}.

copyInstances(Handle, AppPid, Classes, Implementer) ->
    case ?CALL(?SERVER, {copyInstances, Handle, AppPid, Classes, Implementer}) of
	{error, bad_class} ->
	    {error, sa_ais_err_invalid_param};
	{error, _Reason}=E ->
	    E;
	ok ->
	    ok
    end.


%% @doc ICTI waitForClasses: Join the given classes to the set
%% of classes already waited for by the session indicated by
%% the given handle.

-spec waitForClasses(handle(),
		     integer(),
		     write_instances|write_rt_instances|wait_for_classes,
		     [className()]) ->
	  ok |
	  {error, sa_ais_err_bad_handle} |
	  {error, sa_ais_err_invalid_param} |
	  {error, sa_ais_err_not_exist}.

waitForClasses(Handle, AppPid, Context, Classes) ->
    ?CALL(?SERVER, {waitForClasses, Handle, AppPid, Context, Classes}).


%%% ----------------------------------------------------------
%%% @doc To be called when insertions have been made. If the
%%% given argument is true then data conversion is complete.
%%% @end
%%% ----------------------------------------------------------

notify(Source, IsComplete) ->
    gen_server:cast(?SERVER, {notify, Source, IsComplete}).


%%% ----------------------------------------------------------
%%% @doc Inform the master that a restart has been requested
%%% and is in progress.
%%% @end
%%% ----------------------------------------------------------
-spec setRestartRequested() -> ok.

setRestartRequested() ->
    gen_server:cast(?SERVER, {setRestartRequested}).




%%% ----------------------------------------------------------
%%% @doc Remove IMM objects before copying starts. Consider
%%% moving this action to the appdata phase.
%%% @end
%%% ----------------------------------------------------------

deleteBeforeCopying() ->

    case ets:tab2list(imm_objects) of
	ImmObjects when length(ImmObjects) > 2 ->
	    Keys = mnesia:dirty_all_keys(imm_objects),
	    TopKey = [[<<"safApp=safImmService">>, <<"safRdn=immManagement">>]],
 	    RmKeys = lists:filter(fun([<<"safApp=safLogService">> | _]) -> false;
				     (_)                                -> true
				  end,
				  Keys -- TopKey),

	    [mnesia:dirty_delete({imm_objects, Key}) || Key <- RmKeys],
	    ok;
	ImmObjects ->
	    sysInitI:warning_msg(
	      "~w: Unexpected: Content of imm_objects before import -~n~p~n",
	      [?MODULE, ImmObjects])
    end.



-record(ictiSession, {waiting=false                    :: boolean(),
		      syncPid                          :: pid() | 'undefined',
		      ref                              :: reference() | 'undefined'
		     }).

-record(state, {
		timeoutCount=0            :: integer(),
%% 		beginIcti=os:timestamp()  :: erlang:timestamp(),
%% 		isReportedOffenders=false :: boolean(),
		phase=""                  :: string(),    %% "", "I", "II", "III"
		nextHandle=1              :: integer(),
		ictiSessions=dict:new()   :: dict:dict(), %% #ictiSession{} by handle()
		igs                       :: dict:dict() | 'undefined', %% IG by binarized classname

		oldSchemas                :: dict:dict() | 'undefined', %% all schemas by basename (string())
		newSchemasByName          :: dict:dict() | 'undefined',

		oldClassInfos             :: dict:dict() | 'undefined', %% #oldClassInfo{} by binarized classname

                success={true, ""}        :: {boolean(), string()}, %% status, reason
                stopWhenIdle=false        :: boolean(),   %% stop when all sessions terminated

		restartRequested=false    :: boolean(),   %% coordination of restarts

		classesKnown              :: sets:set() | 'undefined',

		inserterReady=false       :: boolean(),
	  
	        syncObjects=sets:new()    :: sets:set(),

	        moTreeComplete=false      :: boolean()
	       }).

init([]) ->

    case swmI:is_upgrade_ongoing() of
	false ->
	    ?INFO("context is: not upgrade", []),
	    registerForCallbacks(?VERIFY_UPGRADE_MODULE),
	    sysInitI:log_startup_time(gmfImmUgMaster_started),
	    ignore;
	true ->
	    %% Trace "everything"
	    %%     safs:trace_start(),
	    %%     safs:trace_services([{imm_om, all}, {imm_oi, all}]),
	    %%
	    %% Depp trace while chasing the "fermi" issue
	    %%     safs:trace_start(),
	    %%     safs:set_tp({safs_imm_oi, safs_error}),
	    %%     safs_trace:tp({safs_imm_oi, initialize_2, 2}),
	    %%     safs_trace:tp({safs_imm_oi, implementer_set}),
	    %%     safs_trace:tp({safs_imm_oi, implementer_clear}),
	    %%     safs_trace:tp({safs_imm_oi, finalize}),
	    %%     safs_trace:tp({safs_com_inproxy, init}),

	    case getVariable(conversionOngoing) of
		true ->
		    ?INFO("refusing to retry conversion", []),
		    sysInitI:log_startup_time(gmfImmUgMaster_started),
		    ignore;
		_ ->
		    ?INFO("context is: upgrade", []),
		    sysInitI:log_startup_time(gmfImmUgMaster_started),
		    {ok, #state{}, ?TIMEOUT_IMMEDIATELY}
	    end
    end.


%%% ----------------------------------------------------------
%%% @doc Deferred initialization, executed once.
%%% @end
%%% ----------------------------------------------------------
-spec phaseOne(#state{}) -> #state{} | abort.

phaseOne(State) ->
    gmfImmUgLib:progress(?MODULE, ?LINE, "upgrade: MO conversion phase I: analyze schemas"),
    setVariable(conversionOngoing, true),
    deleteBeforeCopying(),
    registerForCallbacks(?MODULE),

    ?INFO("get 'From' schemas", []),
    OldSchemasByBasename = getOldSchemasByBasename(),
        OldSchemasByName = gmfImmUgLib:getSchemasByName(OldSchemasByBasename),

    ?INFO("get 'To' schemas", []),
    NewSchemasByBasename = gmfImmUgLib:getCurrentSchemasByBasename("To"),
    NewSchemasByName = gmfImmUgLib:getSchemasByName(NewSchemasByBasename),

    {OldSchemasByAbspath, S0, S1V0, S1V1, S1V2, S1V3} =
	gmfImmUgLib:scanOldSchemas(
	  OldSchemasByBasename,
	  NewSchemasByBasename,
	  NewSchemasByName),

    NewSchemasMatched = S1V0++S1V1++S1V2++S1V3,
    S2 = gmfImmUgLib:scanNewSchemas(OldSchemasByBasename, NewSchemasByBasename, NewSchemasMatched),

    logSchemaClass("category S0 schemas -", S0),
    logSchemaClass("category S1V0 schemas -", S1V0),
    logSchemaClass("category S1V1 schemas -", S1V1),
    logSchemaClass("category S1V2 schemas -", S1V2),
    logSchemaClass("category S1V3 schemas -", S1V3),
    logSchemaClass("category S2 schemas -", S2),

    ?INFO("check upgrade paths of S1V1 schemas", []),
    Failures1 = gmfImmUgLib:checkUpgradePaths(S1V1),

    ?INFO("check upgrade paths of S1V2 and S1V3 schemas", []),
    Failures2 = gmfImmUgLib:checkUpgradePaths(S1V2++S1V3, OldSchemasByName),

    checkVersioningFailures(Failures1++Failures2),

    % Get the list of all {#oldClassInfo{}, #gmfImmClass{}} pairs
    OldGmfImmClassRecords = getOldImmClassRecords(),
    ?INFO("got old class definitions: ~w", [length(OldGmfImmClassRecords)]),

    OldClassInfos =
	lists:foldl(
	  fun({#oldClassInfo{className=C}=X, _}, D) ->
		  dict:store(C, X, D)
	  end,
	  dict:new(),
	  OldGmfImmClassRecords),

    gmfImmUgLib:progress(?MODULE, ?LINE, "upgrade: MO conversion phase I: restore old instances"),
    IGDict = partition(OldClassInfos),
    ?INFO("old config and persistent-runtime instance groups: ~w, instances: ~w",
	  [dict:size(IGDict),
	   dict:fold(
	     fun(_ClassnameB, #instanceGroup{instances=II}, Acc) ->
		     Acc + length(II)
	     end,
	     0,
	     IGDict)]),

    % Dump from-state data for offline analysis
    % dumpData(OldGmfImmClassRecords, OldSchemasByAbspath, IGDict),

    % Get the list of IGs that will be automatically copied
    AutoIGsFromSchemas = selectIGs([s1v0, s1v3],
				   OldGmfImmClassRecords,
				   OldSchemasByAbspath,
				   IGDict,
				   true),
    ?INFO("selected s1v0 and s1v3 IGs for copying in phase I: ~w",
	  [length(AutoIGsFromSchemas)]),

    % Get info tuples for certain special classes (runtime classes excluded)
    LogClassAndFileRecords = getClassAndFileRecords(?LOG_CLASSES),

    % Get the SAF LOG #instanceGroup{} items
    AutoIGsFromLog = getLogInstanceGroups(LogClassAndFileRecords, IGDict),
    ?INFO("selected SAF LOG instances for copying in phase I -", []),
    [?INFO("class: ~p", [ClassnameB])
     || #instanceGroup{className=ClassnameB} <- AutoIGsFromLog],

    % Trust that classnames do not clash here
    AutoIGs = AutoIGsFromSchemas++AutoIGsFromLog,
    ?INFO("S1V0, S1V3 and SAF LOG IGs: ~w, instances: ~w",
	  [length(AutoIGs),
	   lists:foldl(
	     fun(#instanceGroup{instances=II}, A) ->
		     A + length(II)
	     end,
	     0,
	     AutoIGs)]),

    % Get the total set of new config and persistent-runtime classes,
    % S2U classes excluded.
    NewClassAndFileRecords =
	getNewConfigAndPersRuntimeClasses(S2) ++
	    LogClassAndFileRecords,

    % Start tracking new classes
    % ?INFO("started tracking To-version classes: ~w", [length(NewClassAndFileRecords)]),

    ClassesKnown =
	sets:from_list(
	  [C || #classAndFile{class=C}  <- NewClassAndFileRecords]),

    % switch off notifications during data conversion

    sysInitI:info_msg("turning off IMM create/delete/AVC notifications~n", []),
    safs:set_env(imm_notification_category, none),

    gmfImmUgInserter:start(NewClassAndFileRecords),

    gmfImmUgLib:progress(?MODULE, ?LINE, "upgrade: MO conversion phase I: copy instance groups"),
    case gmfImmUgInserter:addInstGroups(auto, AutoIGs) of
	{error, _} ->
	    % restart is under way already; logging has
	    % been done already
	    abort;
	ok ->
	    gmfImmUgLib:progress(?MODULE, ?LINE, "upgrade: MO conversion phase II pass 1 starts"),
	    State#state{phase="II",
			igs=IGDict,
			oldSchemas=OldSchemasByBasename,
			newSchemasByName=NewSchemasByName,
			oldClassInfos=OldClassInfos,
			classesKnown=ClassesKnown}
    end.


handle_call({isReady}, _From, #state{inserterReady=InserterReady}=S) ->
    {reply, InserterReady, S};

handle_call({initIctiSession, Delay},
	    _From,
	    #state{ictiSessions=Sessions, nextHandle=Handle, syncObjects=SO}=S) ->
    NewSessions = dict:store(Handle, #ictiSession{}, Sessions),
    if
	Delay ->
	    NewSO=sets:add_element(Handle, SO),
	    {reply, Handle, S#state{ictiSessions=NewSessions, nextHandle=Handle+1, syncObjects=NewSO}};
	true ->
	    {reply, Handle, S#state{ictiSessions=NewSessions, nextHandle=Handle+1, syncObjects=SO}}
    end;


handle_call({failUpgrade, Handle, AppPid, Message},
	    _From,
	    #state{ictiSessions=IctiSessions}=S) ->
    case isHandleValid(Handle, IctiSessions) of
	false ->
	    {reply, {error, ?SA_ERR_BAD_HANDLE}, S, ?TIMEOUT};
	_ ->
	    if
		S#state.stopWhenIdle =:= true ->
		    {reply, {error, ?SA_ERR_BAD_OPERATION}, S, ?TIMEOUT};
		S#state.restartRequested ->
		    ?INFO("fail upgrade, requested by app: ~w, message: ~s; "
			  "restart is already in progress",
			  [AppPid, binary_to_list(Message)]),
		    {reply, ok, S#state{success={false, ""}}, ?TIMEOUT};
		true ->
		    Diagnostic =
			gmfImmUgLib:format(
			  "fail upgrade, requested by app: ~w, message: ~s",
			  [AppPid, binary_to_list(Message)]),
		    ?FAULT([restart],
			   "~s",
			   [Diagnostic]),
		    {reply, ok, S#state{success={false, Diagnostic}}, ?TIMEOUT}
	    end
    end;

handle_call({readSchemaVersion, Handle, SchemaNameB},
	    _From,
	    #state{oldSchemas = Old,
		   newSchemasByName = NBN,
		   ictiSessions = IctiSessions} = S) ->

    case isHandleValid(Handle, IctiSessions) of
	false ->
	    Reply = {error, ?SA_ERR_BAD_HANDLE},
	    {reply, Reply, S, ?TIMEOUT};
	_ ->
	    SchemaName = binary_to_list(SchemaNameB),
	    Reply =
		case dict:is_key(SchemaName, NBN) of
		    false ->
			case getSchemaByName(SchemaName, Old) of
			    none ->
				{error, ?SA_ERR_NOT_EXIST};
			    #immInfo{version=[OldMajor, OldMinor, OldMicro]} ->
				{ok,
				 #safsImmCtSchemaVersion{version=OldMajor, release=OldMinor, correction=OldMicro},
				 #safsImmCtSchemaVersion{version=0, release=0, correction=0}}
			end;
		    true ->
			#immInfo{version=[NewMajor, NewMinor, NewMicro]} = dict:fetch(SchemaName, NBN),
			case getSchemaByName(SchemaName, Old) of
			    none ->
				% matching old schema not found
				{ok,
				 #safsImmCtSchemaVersion{version=0, release=0, correction=0},
				 #safsImmCtSchemaVersion{version=NewMajor, release=NewMinor, correction=NewMicro}};
			    #immInfo{version=[OldMajor, OldMinor, OldMicro]} ->
				{ok,
				 #safsImmCtSchemaVersion{version=OldMajor, release=OldMinor, correction=OldMicro},
				 #safsImmCtSchemaVersion{version=NewMajor, release=NewMinor, correction=NewMicro}}
			end
		end,
	    {reply, Reply, S, ?TIMEOUT}
    end;


handle_call({readInstances, Handle, ClassNames, AppPid, Op, IncludeAll},
	    _From,
	    #state{ictiSessions=IctiSessions,
		   igs=IGs,
		   oldClassInfos=OldClassInfos}=S) ->
    case isHandleValid(Handle, IctiSessions) of
	false ->
	    {reply, {error, ?SA_ERR_BAD_HANDLE}, S, ?TIMEOUT};
	_ ->
	    {Pairs, Status} =
		lists:foldl(
		  fun(ClassName, {Acc, Stat}) ->
			  case dict:find(ClassName, OldClassInfos) of
			      error ->
				  ?FAULT([user],
					 "app=~w ~w, "
					 "class does not exist in old system; "
					 "handle: ~w, "
					 "class: ~p",
					 [AppPid, Op, Handle, ClassName]),
				  {Acc, error};
			      {_, OldClassInfo} ->
				  case dict:find(ClassName, IGs) of
				      error ->
					  EG = #instanceGroup{className=ClassName},
					  {[{OldClassInfo, EG}|Acc], Stat};
				      {_, IG} ->
					  if
					      IncludeAll ->
						  {[{OldClassInfo,
						     restoreAttributes(IG, OldClassInfos)}
							|Acc], Stat};
					      true ->
						  {[{OldClassInfo, IG}|Acc], Stat}
					  end
				  end
			  end
		  end,
		  {[], ok},
		  ClassNames),
	    if
		Status =/= ok ->
		    {reply, {error, ?SA_ERR_NOT_EXIST}, S, ?TIMEOUT};
		true ->
		    {reply, {ok, lists:reverse(Pairs)}, S, ?TIMEOUT}
	    end
    end;


handle_call({copyInstances, Handle, AppPid, Classes, Implementer},
	    _From,
	    #state{igs=IGDict}=S) ->
    ClassesUnique = ordsets:from_list(Classes),
    case checkOperation(Handle, AppPid, copy_instances, Classes, ClassesUnique, S) of
	{error, _Reason}=E ->
	    {reply, E, S, ?TIMEOUT};
	ok ->
	    % actually do the copying
	    try [getIG(Classname, IGDict, Implementer) || Classname <- Classes] of
		IGs ->
		    ?INFO("got IGs: ~w", [length(IGs)]),
		    %% TODO, at this point we might do an early check that
		    %% the classes that we try to copy are in fact s1v1 or s1v2. This
		    %% requires that we prepare the s1v1 and s1v2 classes list in
		    %% the init phase. We should do so for improved diagnostic and
		    %% issue a warning at least. Or be fatal if we can prove that
		    %% we will fail later anyway.
		    gmfImmUgInserter:addInstGroups(copy, IGs),
		    {reply, ok, S, ?TIMEOUT}
	    catch
		throw:{bad_class, BadClass} ->
		    % this is believed to be an application-invoked situation
		    ?FAULT([user, {source, copy}],
			   "class ~p is not 'persistent runtime', cannot set implementer: ~p",
			   [BadClass, Implementer]),
		    {reply, {error, bad_class}, S, ?TIMEOUT}
	    end
    end;


handle_call({waitForClasses, Handle, AppPid, Context, Classes}, _From, S) ->
    ClassesUnique = ordsets:from_list(Classes),
    case checkOperation(Handle, AppPid, Context, Classes, ClassesUnique, S) of
	{error, _Reason}=E ->
	    {reply, E, S, ?TIMEOUT};
	ok ->
	    {reply, ok, S, ?TIMEOUT}
    end;


handle_call({confirm}, _From, #state{success=Success}=S) ->
    case Success of
	{false, ""} ->
	    ?FAULT([user], "confirm attempted when upgrade has failed", []),
	    %% Reply 'ok' here since we trust that cold restart
	    %% is under way already.
	    {reply, ok, S, ?TIMEOUT};
	{false, Diagnostic} ->
	    ?FAULT([user],
		   "confirm attempted when upgrade has failed, reason: ~s",
		   [Diagnostic]),
	    %% Reply 'ok' here since we trust that cold restart
	    %% is under way already.
	    {reply, ok, S, ?TIMEOUT};
	{true, _} ->
	    ClassesNotReceived =
		ordsets:to_list(
		  gmfImmUgInserter:getNotReceived()),
	    if
		ClassesNotReceived =/= [] ->
		    %% Really not supposed to happen since the confirm/0
                    %% cannot be invoked by SWM until the To-UP is
                    %% in state "waiting for confirm".
		    ?FAULT([user],
			   "confirm attempted before conversion complete; "
		           "classes not handled -~n~p",
			   [ClassesNotReceived]),
		    {reply,
		     {error, "confirm before conversion complete"},
		     S,
		     ?TIMEOUT};
		true ->
		    case dict:size(S#state.ictiSessions) of
			0 ->
			    {stop, shutdown, ok, S};
			N ->
			    ?INFO("'confirm' while ~w ICTI sessions still open", [N]),
			    {reply, ok, S#state{stopWhenIdle=true}, ?TIMEOUT}
		    end
	    end
    end.






handle_cast({finSync, Handle, Pid, Ref}, #state{ictiSessions=Sessions, syncObjects=SO, moTreeComplete=MTC}=S) ->
    case dict:is_key(Handle, Sessions) of
	false ->
	    ?INFO("not a valid handle: ~p", [Handle]),
	    Pid ! {error, Ref, ?SA_ERR_BAD_HANDLE},
	    {noreply, S, ?TIMEOUT};
	true ->
	    NewSO=sets:del_element(Handle, SO),
	    if
		S#state.phase =:= "III" ->
		    ?INFO("ICTI session [~p], phase III, finalize need not hang", [Handle]),
		    NewSessions = dict:erase(Handle, Sessions),
		    Pid ! {ok, Ref},
		    case dict:size(NewSessions) of
			0 when S#state.stopWhenIdle =:= true ->
			    ?INFO("no more ICTI sessions", []),
			    {stop, shutdown, S#state{syncObjects=NewSO}};
			_ ->
			    {noreply, S#state{ictiSessions=NewSessions, syncObjects=NewSO}, ?TIMEOUT}
		    end;
		S#state.phase =:= "II" andalso MTC ->
		    case sets:size(NewSO) of
			0 ->
			    NewSessions = letFinalize(Sessions),
			    sendActivationComplete(S#state.success),
			    gmfImmUgLib:progress(?MODULE, ?LINE, "upgrade: MO conversion phase III starts after delay"),
			    {noreply, S#state{phase="III", ictiSessions=NewSessions, syncObjects=NewSO}, ?TIMEOUT};
			_ ->
			    IS = dict:fetch(Handle, Sessions),
			    NewIctiSession = IS#ictiSession{waiting=true, syncPid=Pid, ref=Ref},
			    NewSessions = dict:store(Handle, NewIctiSession, Sessions),
			    {noreply, S#state{ictiSessions=NewSessions, syncObjects=NewSO}, ?TIMEOUT}			    
		    end;
		true ->
		    % phase is II
		    ?INFO("ICTI session [~p], phase II, finalize must hang", [Handle]),
		    IS = dict:fetch(Handle, Sessions),
		    NewIctiSession = IS#ictiSession{waiting=true, syncPid=Pid, ref=Ref},
		    NewSessions = dict:store(Handle, NewIctiSession, Sessions),
		    {noreply, S#state{ictiSessions=NewSessions, syncObjects=NewSO}, ?TIMEOUT}
	    end
    end;

handle_cast({notify, _Source, false}, State) ->
    % no effect, other than to inhibit "idle" log entries
    {noreply, State};

handle_cast({notify, _Source, true}, #state{phase="II", ictiSessions=Sessions, syncObjects=SO}=State) ->
    case sets:size(SO) of
	0-> NewSessions = letFinalize(Sessions),
	    sendActivationComplete(State#state.success),
	    gmfImmUgLib:progress(?MODULE, ?LINE, "upgrade: MO conversion phase III starts"),
	    sysInitI:info_msg("turning on IMM create/delete/AVC notifications~n", []),
	    safs:set_env(imm_notification_category, all),
	    {noreply, State#state{phase="III", ictiSessions=NewSessions, moTreeComplete=true}};

	_-> gmfImmUgLib:progress(?MODULE, ?LINE, "upgrade: MO tree ready, stay in phase II"),
	    sysInitI:info_msg("turning on IMM create/delete/AVC notifications~n", []),
	    safs:set_env(imm_notification_category, all),
	    {noreply, State#state{moTreeComplete=true}}
    end;
   
handle_cast({setRestartRequested}, S) ->
    {noreply, S#state{restartRequested=true}, ?TIMEOUT};

handle_cast({setInserterReady}, S) ->
    {noreply, S#state{inserterReady=true}, ?TIMEOUT};

handle_cast(Request, S) ->
    ?WARNING("unexpected cast: ~p", [Request]),
    {noreply, S}.


handle_info(timeout, #state{phase=""}=State) ->
    case phaseOne(State) of
	abort ->
	    ?INFO("stopping: ~w", [?SERVER]),
	    {stop, shutdown, State};
	NewState ->
	    {noreply, NewState, ?TIMEOUT}
    end;

handle_info(timeout, #state{timeoutCount=K}=State) ->
    KNext = K+1,
    ?INFO("idle(~w)", [KNext]),
    {noreply, State#state{timeoutCount=KNext}, ?TIMEOUT}.


%% @doc Remove loop avoidance protection, register the 'verify
%% upgrade' callback module (since this To-role may be the From-
%% role in a subsequent upgrade), and stop the companion servers.

terminate(_Reason, #state{phase=Phase}) ->
    eraseVariable(conversionOngoing),
    registerForCallbacks(?VERIFY_UPGRADE_MODULE),
    gmfImmUgInserter:stop(),
    if
	Phase =/= "III" ->
	    % the conversion may have been aborted early on
	    ok;
	true ->
	    gmfImmUgLib:progress(?MODULE, ?LINE, "upgrade: MO conversion phase III ends")
    end,
    ?INFO("stopped: ~w", [?SERVER]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @doc Checks preconditions of copy, write and wait operations.

-spec checkOperation(
	integer(),
	handle(),
	copy_instances|write_instances|wait_for_classes,
	[className()],
	ordsets:ordset(className()),
	#state{}) ->
	  ok |
	  {error, sa_ais_err_bad_handle} |
	  {error, sa_ais_err_invalid_param} |
	  {error, sa_ais_err_not_exist}.

checkOperation(Handle,
	       AppPid,
	       Context,
	       Classes,
	       ClassesUnique,
	       #state{ictiSessions=IctiSessions,
		      classesKnown=ClassesKnown}) ->
    case isHandleValid(Handle, IctiSessions) of
	false ->
	    ?FAULT([user],
		   "~w, bad handle: ~w, classes: ~p",
		   [Context, Handle, Classes]),
	    {error, ?SA_ERR_BAD_HANDLE};
	_ ->
	    if
		length(ClassesUnique) < length(Classes) ->
		    ?FAULT([user],
			   "app=~w ~w, classes referred repeatedly: ~p",
			   [AppPid, Context, Classes]),
		    {error, ?SA_ERR_INVALID_PARAM};
		true ->
		    case getUnknown(ClassesUnique, ClassesKnown) of
			[_|_]=Unknown ->
			    %% raised severity to "ERROR" even though it is
			    %% an application error
			    ?FAULT([],
				   "app=~w ~w, operation ABORTED "
			           "because of unknown classes: ~p",
				   [AppPid, Context, Unknown]),
			    {error, ?SA_ERR_NOT_EXIST};
			[] ->
			    ok
		    end
	    end
    end.


-spec getUnknown(ordsets:ordset(className()), sets:set()) -> [className()].

getUnknown(ClassesUnique, ClassesKnown) ->
    lists:reverse(
      ordsets:fold(
	fun(C, Acc) ->
		case sets:is_element(C, ClassesKnown) of
		    true ->
			Acc;
		    _ ->
			[C|Acc]
		end
	end,
	[],
	ClassesUnique)).


%% @doc Returns #classAndFile{} records. Classes that are
%% non-persistent runtime are excluded. Classes that match
%% by IMM filename one of the given #immInfo{} records
%% and the #immInfo{} record indicates an unnamed schema
%% are excluded (this is used to exclude S2U classes).

-spec getNewConfigAndPersRuntimeClasses([#immInfo{}]) ->
	  [#classAndFile{}].

getNewConfigAndPersRuntimeClasses(S2) ->
    F2 = fun(Rec, Acc) -> [Rec|Acc] end,
    {atomic, Result} =
	mnesia:transaction(
	  fun() ->
		  mnesia:foldl(F2, [], gmfImmClass)
	  end),
    lists:append(
      [begin
	   DropS2UClass =
	       case lists:keyfind(ImmFileName, #immInfo.abspath, S2) of
		   #immInfo{type=unnamedSchema} ->
		       true;
		   _ ->
		       false
	       end,
	   if
	       DropS2UClass ->
		   ?INFO("S2U class, will not be tracked: ~p", [Name]),
		   [];
	       true ->
		   case Category of
		       "SA_CONFIG" ->
			   [#classAndFile{class=list_to_binary(Name),
					  immFile=ImmFileName}];
		       "SA_RUNTIME" ->
			   case getRuntimeVariety(AttrDescrs) of
			       persistentRuntime ->
				   [#classAndFile{class=list_to_binary(Name),
						  immFile=ImmFileName,
						  category=persistentRuntime}];
			       _ ->
				   []
			   end
		   end
	   end
       end
       || #gmfImmClass{key=Name,
		       imm_file_name=ImmFileName,
		       category={category, Category},
		       attributes=AttrDescrs} <- Result]).

%%% ----------------------------------------------------------
%%% @doc Converts the given list of binarized classnames
%%% to a list of class-and-file wrappers, referring to the
%%% To-system, with no filename specified. Classes that are
%%% 'runtime' according to the #imm_class{} record are excluded,
%%% hence all records will have category=config.
%%% @end
%%% ----------------------------------------------------------
-spec getClassAndFileRecords([className()]) -> [#classAndFile{}].

getClassAndFileRecords(ClassnamesB) ->
    lists:append(
      [begin
	   ClassnameA = list_to_atom(binary_to_list(ClassnameB)),
	   #imm_class{category=Cat} = gmfImmUgLib:getImmClass(ClassnameA),
	   case Cat of
	       runtime ->
		   ?INFO("no #classAndFile{} for: ~p", [ClassnameB]),
		   [];
	       _ ->
		   [#classAndFile{class=ClassnameB}]
	   end
       end
       || ClassnameB <- ClassnamesB]).


%% @doc Gets the schema with matching name, or 'none'. It is
%% trusted that schema names within the dict are unique.

-spec getSchemaByName(string(), dict:dict()) -> #immInfo{} | none.

getSchemaByName(Name, Schemas) ->
    M = [S || {_, #immInfo{schemaName=N}=S} <- dict:to_list(Schemas), N =:= Name],
    case M of
	[] ->
	    none;
	[S] ->
	    S
    end.


-spec logSchemaClass(string(), [#immInfo{}]) -> any().

logSchemaClass(Header, Schemas) ->
    ?INFO(Header, []),
    [begin
	 case Type of
	     unnamedSchema ->
		 ?INFO("  basename: ~s", [Basename]);
	     _ ->
		 ?INFO("  name: ~s", [Name])
	 end
     end
    || #immInfo{type=Type, schemaName=Name, file=Basename} <- Schemas].


%%% ----------------------------------------------------------
%%% @doc Let sessions finalize when possible. Returns a
%%% map of remaining active sessions.
%%%
%%% The given Sessions argument is a map from session handle
%%% to #ictiSession{} record.
%%% @end
%%% ----------------------------------------------------------
-spec letFinalize(dict:dict()) -> dict:dict().

letFinalize(Sessions) ->
    Handles = dict:fetch_keys(Sessions),
    % iterate over ICTI sessions
    lists:foldl(
      fun(Handle, SS) ->
	      case dict:fetch(Handle, SS) of
		  #ictiSession{waiting=true, syncPid=SyncPid, ref=Ref} ->
		      ?INFO("phase III, let waiting session finalize: ~p", [Handle]),
		      SyncPid ! {ok, Ref},
		      dict:erase(Handle, SS);
		  _ ->
		      SS
	      end
      end,
      Sessions,
      Handles).    


%%% ----------------------------------------------------------
%%% @doc Partitions restored instances into a dict of
%%% #instanceGroup{} by binarized class name. Runtime classes
%%% are excluded if the 'runtime' category can be looked up from
%%% the given classes info.
%%% @end
%%% ----------------------------------------------------------
-spec partition(dict:dict()) -> dict:dict().

partition(OldClassInfo) ->
    lists:foldl(
      fun({ClassnameA, Key, Attrs, Implementer}, AccDict) ->
	      ClassnameB = atom_to_binary(ClassnameA, latin1),
	      % ?INFO("pre-restart instance: ~p", [{ClassnameA, Key, Attrs}]),
	      Length = length(Key),
	      case dict:find(ClassnameB, OldClassInfo) of
		  error ->
		      % Instances of properly modelled IMM classes
		      % do not end up here.
		      ?INFO("no description for old class: ~w", [ClassnameA]),
		      StrippedAttrs =
			  [A || {_, V}=A <- Attrs, V =/= '$imm_runtime'],
		      Instance = #restoredInstance{class=ClassnameA,
						   key=Key,
						   attrs=StrippedAttrs,
						   implementer=Implementer},
		      addInstance(Instance, ClassnameB, Length, AccDict, false);
		  {_, #oldClassInfo{category=runtime}} ->
		      % there may be a great number of these!
		      % ?INFO("early dropping of runtime class instance: ~p", [Instance]),
		      AccDict;
		  {_, #oldClassInfo{category=NotPureRuntime, attributes=AttrDescrs}} ->
		      % treat 'config' and 'undefined' alike here
		      {StrippedInstance, StructInstances} =
			  createRestoredInstances(ClassnameA, Key, Attrs, AttrDescrs, Implementer, OldClassInfo),
		      ImpliedPersistent = NotPureRuntime =:= persistentRuntime,
		      NewAccDict = addInstance(StrippedInstance, ClassnameB, Length, AccDict, ImpliedPersistent),
		      lists:foldl(
			fun({SCNameB, IP, R}, A) ->
				addInstance(
				  R,
				  SCNameB,
				  Length+1,
				  A,
				  IP)
			end,
			NewAccDict,
			StructInstances)
	      end
      end,
      dict:new(),
      getOldInstances()).


%%% ----------------------------------------------------------
%%% @doc Creates restored instances. If the instance being restored
%%% has struct attributes then resurrected struct-class instances are
%%% included. Nonpersistent runtime attributes are dropped everywhere.
%%%
%%% The boolean element in the result triple is true if the instance
%%% is implied persistent.
%%% @end
%%% ----------------------------------------------------------
-spec createRestoredInstances(
	atom(),
	any(),
	[any()],
	[{string(), list()}],
	implementer(),
	dict:dict()) ->
	  {#restoredInstance{},
	   [{className(), boolean(), #restoredInstance{}}]}.

createRestoredInstances(ClassnameA, Key, Attrs, AttrDescrs, Implementer, OldClassInfo) ->
    AttrDescrsA = [{list_to_atom(AttrNameS), L} || {AttrNameS, L} <- AttrDescrs],
    {KeptAttrs, StructInstInfos} =
	lists:foldr(
	  fun({AttrA, _ValueList}=TheAttr, {AA, SS}=Acc) ->
		  Props = proplists:get_value(AttrA, AttrDescrsA),
		  case not(lists:member({category, "SA_CONFIG"}, Props)) andalso
			   not(lists:member({flag, "SA_PERSISTENT"}, Props)) of
		      true ->
			  Acc;
		      false ->
			  {[TheAttr|AA], SS}
		  end;
	     ({AttrA, ?IMM_STRUCT, ValueList}, {AA, SS}=Acc) ->
		  Props = proplists:get_value(AttrA, AttrDescrsA),
		  case not(lists:member({category, "SA_CONFIG"}, Props)) andalso
			   not(lists:member({flag, "SA_PERSISTENT"}, Props)) of
		      true ->
			  Acc;
		      false ->
			  % only expected when WP5493 has been delivered and the
			  % From-version contains MO instances with struct attributes
			  Triples = resurrect(AttrA, ValueList, Key, OldClassInfo, Implementer),
			  Dns = getStructDns(Triples),
			  StructAttr = {AttrA, Dns},
			  {[StructAttr|AA], Triples++SS}
		  end
	  end,
	  {[], []},
	  Attrs),
    R = #restoredInstance{class=ClassnameA, key=Key, attrs=KeptAttrs, implementer=Implementer},
    {R, StructInstInfos}.


%%% ----------------------------------------------------------
%%% @doc Returns a list of IMM DNs that refer to resurrected
%%% struct instances.
%%% @end
%%% ----------------------------------------------------------
-spec getStructDns([{any(), boolean(), #restoredInstance{}}]) -> [binary()].

getStructDns(Triples) ->
    lists:map(
      fun({_, _, #restoredInstance{key=Key}}) ->
	      list_to_binary(
		string:join(
		  lists:foldl(
		    fun(RdnB, Acc) ->
			    [binary_to_list(RdnB)|Acc]
		    end,
		    [],
		    Key),
		  ","))
      end,
      Triples).


%%% ----------------------------------------------------------
%%% @doc Resurrect a number of struct instances to act as the
%%% values of a MO attribute.
%%%
%%% TODO: Perhaps the implementer is not important at all
%%% when resurrecting class instances? Consider the "copy"
%%% and "read/write" cases.
%%% @end
%%% ----------------------------------------------------------
-spec resurrect(atom(), any(), any(), dict:dict(), implementer()) ->
	  [{className(), boolean(), #restoredInstance{}}].

resurrect(AttrNameA, ValueList, Key, OldClassInfo, Implementer) ->
    {Triples, _} =
	lists:foldl(
	  fun(#safsImmCsStruct{structName=StructClassNameB, structMembers=AttrValuesList}, {Acc, M}) ->
		  StructClassNameA = list_to_atom(binary_to_list(StructClassNameB)),
		  StructInstKey = Key++[list_to_binary("id="++atom_to_list(AttrNameA)++"_"++integer_to_list(M))],
		  {Attrs, ImpliedPersistent} = resurrectAttrs(AttrValuesList, StructClassNameB, OldClassInfo),
		  RestoredInstance =
		      #restoredInstance{class=StructClassNameA,
					key=StructInstKey,
					attrs=Attrs,
					implementer=Implementer},
		  {[{StructClassNameB, ImpliedPersistent, RestoredInstance}|Acc], M+1}
	  end,
	  {[], 1},
	  ValueList),
    lists:reverse(Triples).


%%% ----------------------------------------------------------
%%% @doc Convert a list of #safsImmAttrValues_2{} to
%%% attributes of a struct instance.
%%%
%%% It is trusted that values are not "struct".
%%%
%%% Attributes that are pure runtime according to the "From"
%%% class definition are dropped.
%%% @end
%%% ----------------------------------------------------------
-spec resurrectAttrs([#safsImmAttrValues_2{}], className(), dict:dict()) ->
	  {[{atom(), list()}], boolean()}.

resurrectAttrs(AttrValuesList, StructClassNameB, OldClassInfo) ->
    {AttrDescrs, ImpliedPersistent} =
	case dict:find(StructClassNameB, OldClassInfo) of
	    error ->
		?FAULT([], "really unexpected, ~p", [fixme]),
		{[], false};                                     % TODO
	    {_, #oldClassInfo{category=runtime}} ->
		% quite unexpected, mismatch between struct-owning class and this one
		?FAULT([], "rather unexpected, ~p", [fixmeToo]), % TODO
		{[], false};
	    {_, #oldClassInfo{category=config, attributes=DD}} ->
		{DD, false};
	    {_, #oldClassInfo{category=persistentRuntime, attributes=DD}} ->
		{DD, true}
	end,

    % costly and pseudo duplicated ... but maybe not so bad that it is duplicated
    AttrDescrsB = [{list_to_binary(AttrNameS), L} || {AttrNameS, L} <- AttrDescrs],

    R =
	lists:foldr(
	  fun(#safsImmAttrValues_2{attrName=AttrNameB, attrValueType=Type, attrValues=List}, Acc) ->
		  Props = proplists:get_value(AttrNameB, AttrDescrsB),
		  PureRuntime = not(lists:member({category, "SA_CONFIG"}, Props)) andalso
				    not(lists:member({flag, "SA_PERSISTENT"}, Props)),
		  if
		      PureRuntime ->
			  Acc;
		      true ->
			  AttrNameA = list_to_atom(binary_to_list(AttrNameB)),
			  [{AttrNameA, resurrectedMemberValues(Type, List)}|Acc]
		  end
	  end,
	  [],
	  AttrValuesList),
    {R, ImpliedPersistent}.


%% -define(IMM_INT32, sa_imm_attr_saint32t).
%% -define(IMM_INT64, sa_imm_attr_saint64t).
%% -define(IMM_UINT32, sa_imm_attr_sauint32t).
%% -define(IMM_UINT64, sa_imm_attr_sauint64t).
%% -define(IMM_TIME, sa_imm_attr_satimet).
%% -define(IMM_STRING, sa_imm_attr_sastringt).
%% -define(IMM_NAME, sa_imm_attr_sanamet).
%% -define(IMM_STRUCT, sa_imm_attr_csstructt).
%% -define(IMM_DOUBLE, sa_imm_attr_sadoublet).
%% -define(IMM_FLOAT, sa_imm_attr_safloatt).
%% -define(IMM_ANY, sa_imm_attr_saanyt).

%% -record(safsImmAttrValue,
%%         {saint32,                       % = 1, int32 (optional)
%%          sauint32,                      % = 2, uint32 (optional)
%%          saint64,                       % = 3, int64 (optional)
%%          sauint64,                      % = 4, uint64 (optional)
%%          satime,                        % = 5, uint64 (optional)
%%          saname,                        % = 6, string (optional)
%%          safloat,                       % = 7, float (optional)
%%          sadouble,                      % = 8, double (optional)
%%          sastring,                      % = 9, string (optional)
%%          saany                          % = 10, bytes (optional)
%%         }).

% TODO, complete enough?

%%% ----------------------------------------------------------
%%% @doc Converts a list of #safsImmAttrValue{} to naked
%%% values. TODO, is the type conversion correct for all types?
%%% TODO, are all required types handled?
%%% @end
%%% ----------------------------------------------------------
-spec resurrectedMemberValues(atom(), [#safsImmAttrValue{}]) -> [any()].

resurrectedMemberValues(?IMM_INT32, List) ->
    [X||#safsImmAttrValue{saint32=X} <- List];

resurrectedMemberValues(?IMM_STRING, List) ->
    [X||#safsImmAttrValue{sastring=X} <- List];

resurrectedMemberValues(?IMM_INT64, List) ->
    [X||#safsImmAttrValue{saint64=X} <- List];

resurrectedMemberValues(?IMM_UINT32, List) ->
    [X||#safsImmAttrValue{sauint32=X} <- List];

resurrectedMemberValues(?IMM_UINT64, List) ->
    [X||#safsImmAttrValue{sauint64=X} <- List];

resurrectedMemberValues(?IMM_TIME, List) ->
    [X||#safsImmAttrValue{satime=X} <- List];

resurrectedMemberValues(?IMM_NAME, List) ->
    [X||#safsImmAttrValue{saname=X} <- List];

resurrectedMemberValues(OtherType, _List) ->
    ?FAULT([], "cannot handle type: ~p", [OtherType]).


-spec restoreAttributes(#instanceGroup{}, dict:dict()) -> #instanceGroup{}.

restoreAttributes(#instanceGroup{className=ClassName, instances=Insts}=IG,
		  OldClassInfos) ->
    NewInsts =
	lists:reverse( % TODO, is order important?
	  lists:foldl(
	    fun(#restoredInstance{attrs=Attrs}=Inst, Acc) ->
		    case dict:find(ClassName, OldClassInfos) of
			error ->
			    % maybe impossible
			    ?WARNING("no old class info for: ~p", [ClassName]),
			    Acc;
			{_, #oldClassInfo{attributes=AttrsFromClass}} ->
			    EmptyAttrs = 
				lists:foldl(
				  fun({AttrNameS, AttrProps}, Acc2) ->
					  AttrNameA = list_to_atom(AttrNameS),
					  case lists:keyfind(AttrNameA, 1, Attrs) of
					      false ->
						  % ?INFO("!+++ ~s.~w: ~p", [ClassName, AttrNameA, AttrProps]),
						  % maybe add this attribute, although not if it is 
						  % pure runtime
						  case lists:keyfind(category, 1, AttrProps) of
						      false ->
							  % maybe impossible
							  ?WARNING("attribute has no category: ~s.~w", [ClassName, AttrNameA]),
							  Acc2;
						      {_, "SA_CONFIG"} ->
							  [{AttrNameA, []}|Acc2];
						      {_, "SA_RUNTIME"} ->
							  case lists:member({flag, "SA_PERSISTENT"}, AttrProps) of
							      true ->
								  [{AttrNameA, []}|Acc2];
							      _ ->
								  Acc2
							  end
						  end;
					      _ ->
						  Acc2
					  end
				  end,
				  [],
				  AttrsFromClass),
			    XAttrs = Attrs++EmptyAttrs,
			    [Inst#restoredInstance{attrs=XAttrs}|Acc]
		    end
	    end,
	    [],
	    Insts)),
    IG#instanceGroup{instances=NewInsts}.


%% @doc Adds an instance to the indicated instance group
%% and return the thus extended dict of instance group by
%% classname.

-spec addInstance(#restoredInstance{},
		  className(),
		  non_neg_integer(),
		  dict:dict(), boolean()) -> dict:dict().

addInstance(Instance, Classname, Length, Dict, ImpliedPersistent) ->
    case dict:find(Classname, Dict) of
	error ->
	    dict:store(
	      Classname,
	      #instanceGroup{className=Classname,
			     instances=[Instance],
			     maxLength=Length,
			     implPers=ImpliedPersistent},
	      Dict);
	{_, #instanceGroup{instances=II,
			   maxLength=MaxL}=IG} ->
	    NewIG = IG#instanceGroup{instances=[Instance|II],
				     maxLength=max(MaxL, Length)},
	    dict:store(Classname, NewIG, Dict)
    end.


-spec getRuntimeVariety([{string(), [{attrProp(), string()}]}]) ->
                           runtime|persistentRuntime.

getRuntimeVariety(AttributeDescriptors) ->
    case
	lists:any(
	  fun({_AttrName, Properties}) ->
		  lists:member({flag,"SA_PERSISTENT"}, Properties)
	  end,
	  AttributeDescriptors)
	of
	true ->
	    persistentRuntime;
	_ ->
	    runtime
    end.


%% @doc Returns the list of IGs that are of one of the given
%% schema categories. Classes that are non-persistent runtime
%% are excluded if DropRuntime is true.
%%
%% OldGmfImmClassRecords are as-dumped From-version IMM class
%% descriptions (the type is #gmfImmClass{}).
%%
%% OldByAbsPath is a dict of categorized old schemas by absolute
%% pathname (valid categories are s0, s1v0, s1v1, s1v2, s1v3).
%%
%% IGDict is a dictionary of #instanceGroup{} by binarized
%% classname.

-spec selectIGs([atom()],                             %% list of categories
                [{#oldClassInfo{}, #gmfImmClass{}}],  %% class descriptions
		dict:dict(),                          %% old schemas by absolute pathname
		dict:dict(),                          %% #instanceGroup{} by binarized classname
		boolean()) ->                         %% if true drop non-persistent runtime
	  [#instanceGroup{}].

selectIGs(Categories, OldGmfImmClassRecords, OldByAbspath, IGDict, DropRuntime) ->
    lists:append(
      [if
	   ImmCat =:= runtime andalso DropRuntime ->
	       [];
	   true ->
	       BinName = list_to_binary(Name),
	       case dict:is_key(AbsPath, OldByAbspath) of
		   false ->
		       ?FAULT([{source, auto}, restart],
			      "uncategorized class: ~s",
			      [Name]);
		   true ->
		       #immInfo{cat=Category} = dict:fetch(AbsPath, OldByAbspath),
		       case lists:member(Category, Categories) of
			   true ->
			       [getIG(BinName, IGDict, undefined)];
			   _ ->
			       []
		       end
	       end
       end
       || {#oldClassInfo{category=ImmCat}, #gmfImmClass{key=Name,
							imm_file_name=AbsPath}} <-
	      OldGmfImmClassRecords]).


%% @doc Returns a list of instance groups matching the given
%% list of infotuples. Non-existent classes will be excluded
%% from the returned list.

-spec getLogInstanceGroups([#classAndFile{}], dict:dict()) ->
	                             [#instanceGroup{}].

getLogInstanceGroups(ClassAndFileRecords, IGDict) ->
    lists:foldl(
      fun(#classAndFile{class=ClassnameB}, Acc) ->
	      case dict:find(ClassnameB, IGDict) of
		  error ->
		      ?INFO("no instance group for: ~p", [ClassnameB]),
		      Acc;
		  {_, IG} ->
		      [IG|Acc]
	      end
      end,
      [],
      ClassAndFileRecords).


%%% ----------------------------------------------------------
%%% @doc Look up the instance group implied by the given
%%% class name. Provide an empty instance group if the
%%% dictionary does not have one.
%%%
%%% Insert implementer info, unless it is undefined.
%%%
%%% It is considered an error if the implementer is defined
%%% and the class is not runtime persistent.
%%% @end
%%% ----------------------------------------------------------
-spec getIG(className(), dict:dict(), implementer()) -> #instanceGroup{}.

getIG(BinName, IGDict, Implementer) ->
    case dict:is_key(BinName, IGDict) of
	false ->
	    #instanceGroup{className=BinName};
	true ->
	    #instanceGroup{instances=Instances, implPers=ImplPers}=IG =
		dict:fetch(BinName, IGDict),
	    if
		Implementer =:= undefined ->
		    IG;
		not(ImplPers) ->
		    % trying to set the implementer on a
		    % class that is not 'persistent runtime'
		    throw({bad_class, BinName});
		true ->
		    NewInstances =
			[R#restoredInstance{implementer=Implementer} || R <- Instances],
		    IG#instanceGroup{instances=NewInstances}
	    end
    end.


getVariable(Name) ->
    QualName = qualifiedName(Name),
    Read = fun() -> mnesia:read({gmfVars, QualName}) end,
    case dbOp(Read) of
	[] ->
	    undefined;
	[Record] ->
	    Record#gmfVars.value;
	Multi ->
	    [R#gmfVars.value || R <- Multi]
    end.


setVariable(Name, Value) ->
    QualName = qualifiedName(Name),
    Write = fun() -> mnesia:write(#gmfVars{key=QualName, value=Value}) end,
    dbOp(Write),
    ok.


eraseVariable(Name) ->
    QualName = qualifiedName(Name),
    Delete = fun() -> mnesia:delete({gmfVars, QualName}) end,
    dbOp(Delete).


qualifiedName(Name) ->
    list_to_atom(atom_to_list(?MODULE)++"."++atom_to_list(Name)).


dbOp(Op) ->
    case mnesia:is_transaction() of
	true ->
	    apply(Op, []);
	false ->
	    mnesia:async_dirty(Op)
    end.


-spec isHandleValid(handle(), dict:dict()) -> boolean().

isHandleValid(Handle, IctiSessions) ->
    dict:is_key(Handle, IctiSessions).


% @doc Returns the pre-restart schemas in a dictionary, using the
%% basename string as key (it is trusted to exist always).
%% The schema names within the directory are guaranteed unique.

-spec getOldSchemasByBasename() -> dict:dict().

getOldSchemasByBasename() ->
    gmfImmUgLib:listToDict(getGmfCxpRev(), "'From'").


%% @doc Gets the From-version #gmfCxpRev{} records. If no records
%% match the #gmfCxpRev{} format then fall back to an older format.
%% This handling can be dropped when there are no pre-CXS101553-R2A2776
%% installations around.

-spec getGmfCxpRev() -> [#gmfCxpRev{}].

getGmfCxpRev() ->
    Tuples = swmI:all_objects(gmfCxpRev),
    Items = [U || #gmfCxpRev{}=U <- Tuples],
    Result =
	if
	    Items =/= [] ->
		?INFO("no conversion of gmfCxpRev records", []),
		Items;
	    true ->
		?INFO("conversion of gmfCxpRev records from an older format", []),
		[#gmfCxpRev{key=K,
			    ldn=L,
			    imm_root=IR,
			    cxp_path=C,
			    mim_info=MI,
			    imm_info=II,
			    active=A}
			   || {gmfCxpRev, K, L, IR, C, MI, II, A} <- Tuples]
	end,
%%     ?INFO("old gmfCxpRev records: ------------------", []),
%%     ?INFO("~p", [Result]),
    Result.


%%% ----------------------------------------------------------
%%% @doc Gets the From-version IMM class records and
%%% returns a list of {#oldClassInfo{}, #gmfImmClass{}} pairs.
%%% @end
%%% ----------------------------------------------------------
-spec getOldImmClassRecords() -> [{#oldClassInfo{}, #gmfImmClass{}}].

getOldImmClassRecords() ->
    AllRecords = lists:map(
		   fun(R) when is_record(R, gmfImmClass) ->
			   %% already in current format
			   R;
		      ({gmfImmClass,
			{ImmClassName, CxpId, CxpVer},
			Root,
			Category,
			Rdn,
			Attributes,
			GmfMimClass,
			ImmFileName}) ->
			   %% old format to current format
			   NewGmfMimClass = getUpdatedGmfMimClass(GmfMimClass),
			   #gmfImmClass{key           = ImmClassName,
					root          = Root,
					category      = Category,
					rdn           = Rdn,
					attributes    = Attributes,
					gmfMimClass   = NewGmfMimClass,
					imm_file_name = ImmFileName,
					cxp_info      = {CxpId, CxpVer}}
		   end, swmI:all_objects(gmfImmClass)),

    lists:map(
      fun(#gmfImmClass{key=ClassNameS,
		       rdn={RdnS, _},
		       category={category, CatS},
		       attributes=AttrDescrs}=GmfImmClass) ->
	      ClassName = list_to_binary(ClassNameS),
	      RdnName = list_to_binary(RdnS),
	      CatA = list_to_atom(string:to_lower(CatS)),
	      Category =
		  if
		      CatA =:= sa_config ->
			  config;
		      CatA =:= sa_runtime ->
			  getRuntimeVariety(AttrDescrs)
		  end,
	      {#oldClassInfo{className=ClassName,
			      rdnName=RdnName,
			      category=Category,
			      attributes=AttrDescrs},
	       GmfImmClass}
      end,
      AllRecords).

getUpdatedGmfMimClass({MomName, MimClassName, _, _}) ->
    {MomName, MimClassName};
getUpdatedGmfMimClass(GmfMimClass) ->
    GmfMimClass.

%% @doc Log versioning failures and treat as fatal.

-spec checkVersioningFailures([#versionFailure{}]) -> ok.

checkVersioningFailures(Failures) ->
    [?FAULT([{source, auto}, user],
	    "upgrade not supported for schema: ~s,"
	    " ~s from-version: ~p,"
	    " supported from-versions: ~p",
	    [N, OldM, OldV, FVV])
     ||#versionFailure{schemaName=N,
		       fromVersions=FVV,
		       oldVersion=OldV,
		       oldMode=OldM} <- Failures],
    if
	Failures =/= [] ->
	    ?FAULT([{source, auto}, restart],
		   "versioning failure for schemas: ~p",
		   [[N || #versionFailure{schemaName=N} <- Failures]]);
	true ->
	    ok
    end.


%%% @doc Reports to SWM that all classes are handled if the given
%%% argument is {true, _}. Otherwise a diagnostic is logged and
%%% nothing is sent to SWM.
%%% @end

-spec sendActivationComplete({boolean(), string()}) -> ok.

sendActivationComplete({true, _}) ->
    ?INFO("sending 'activation complete' to SWM", []),
    swmI:activation_complete();

sendActivationComplete({false, ""}) ->
    ?FAULT([user], "suppressed sending 'activation complete' to SWM", []),
    ok;

sendActivationComplete({false, Diagnostic}) ->
    ?FAULT([user],
	   "suppressed sending 'activation complete' to SWM, reason: ~s",
	   [Diagnostic]),
    ok.


-spec registerForCallbacks(atom()) -> ok.

registerForCallbacks(Module) ->
    swmI:register_upg_callback(Module),
    ?INFO("registered with SWM for callbacks to module: ~w", [Module]),
    ok.


%%% ----------------------------------------------------------
%%% @doc Get the pre-restart instances.
%%% @end
%%% ----------------------------------------------------------
-spec getOldInstances() -> [{atom(),                  % class name
			     [binary()],              % DN
			     list(),                  % attributes
			     implementer()            % OI implementer
			    }
			   ].

getOldInstances() ->
    [safs_imm_db:get_imm_objects_payload(ImmObject)
       || ImmObject <- swmI:all_objects(imm_objects)].




%%% ----------------------------------------------------------
%%% @doc WP5493: Get 'From' struct info.
%%% TODO: Is this function needed at all?
%%% The resulting dict has binarized IMM classname as key.
%%% A dict value may look like:
%%%   [{"intA",     [{dataType, {int32,undefined,undefined}}]},
%%%    {"intSeqB",  [{dataType, {sequence, {int32,undefined,undefined}}}]},
%%%    {"strSeqC",  [{dataType, {sequence, {string,undefined,undefined}}}]},
%%%    {"enumSeqL", [{dataType, {sequence, {enumRef,"TestEnumSparse", "TESTMOM"}}}]},
%%%    {"enumK",    [{dataType, {enumRef,"TestEnumSparse", "TESTMOM"}}]}]
%%% @end
%%% ----------------------------------------------------------
-spec getOldStructClasses() -> dict:dict().

getOldStructClasses() ->
    %% Convert to current format
    L = lists:map(
	  fun(#gmfMimStruct{key={_, _}} = R) ->
		  %% Already in current format. No change is needed
		  R;
	     (#gmfMimStruct{key={MimName, EcimStructClassName, _, _}} = R) ->
		  %% key has 4 members. Remove the 2 last elements
		  NewKey = {MimName, EcimStructClassName},
		  R#gmfMimStruct{key = NewKey}
	  end, swmI:all_objects(gmfMimStruct)),

    % TODO, verify that this works for moms with no prefixing too
    lists:foldl(
      fun(#gmfMimStruct{key={_MimName, EcimStructClassName},
			imm_ns=ImmNs,
			attributes=Attrs},
	  Acc) ->
	      ImmStructClassNameB = list_to_binary(ImmNs++EcimStructClassName),
	      dict:store(ImmStructClassNameB, Attrs, Acc)
      end,
      dict:new(),
      L).
