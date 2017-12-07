%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfImmUgIcti.erl %
%%% Author:	erarafo
%%% Description: ICTI callback module. Do not rename this module (the
%%% name is hardcoded in make_release.escript).
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(gmfImmUgIcti).
%%-behaviour(safs_imm_ct).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/2').
-date('2017-02-23').
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
%%% R2A/1      2013-06-14 erarafo     First version
%%% R2A/2      2013-06-17 erarafo     Fixed bug in read_instances
%%% R2A/3      2013-06-17 erarafo     read_instances handling of root instance
%%% R2A/5      2013-06-18 erarafo     Logging of ICTI session init/fin
%%% R2A/6      2013-06-19 erarafo     read_instances correction
%%% R2A/7      2013-06-25 erarafo     Improved logging
%%% R2A/8      2013-06-27 erarafo     minMaxLength/1 introduced
%%% R2A/9      2013-06-28 erarafo     refactoring and cleanup
%%% R2A/10     2013-09-04 erarafo     added version reporting
%%% R2A/11     2013-09-27 erarafo     adapted to #ldap{} change in safs_imm_db
%%% R2A/12     2013-11-12 erarafo     adapted to SAFs changes
%%% R2A/13     2014-01-12 erarafo     tweaked log messages
%%% R2A/14     2014-01-13 erarafo     Use safs_imm_db:get_imm_objects_payload/1
%%% R2A/15     2014-01-17 erarafo     Report app pid when fail upgrade
%%% R2A/17     2014-03-10 erarafo     Asynchronous addInstanceGroups
%%% R2A/18     2014-03-11 erarafo     Refactoring
%%% R2A/19     2014-03-18 erarafo     Added events for restart log
%%% R2A/20     2014-03-20 erarafo     Removed event for restart log; cleanup
%%% R2A/21     2014-05-21 erarafo     Comments cleaned up
%%% R2A/22     2014-05-22 erarafo     HS62272: Full support for 'confirm'
%%% R2A/23     2014-07-01 erarafo     Improved fault logging
%%% R2A/24     2014-07-01 erarafo     Bugfix: cached attributes dropped
%%% R2A/25     2014-08-14 erarafo     Performance lift related to HS84194
%%% R2A/26     2014-11-04 erarafo     HS89845: Need for synchronizing
%%% R3A/1      2014-11-07 erarafo     Improved logging
%%% R3A/2      2014-12-09 erarafo     Mapping Unix pid to command line
%%% R3A/3      2014-12-10 erarafo     Refactoring for speedup
%%% R3A/4      2015-01-31 erarafo     Log format improvements
%%% R3A/5      2015-04-01 erarafo     Trivial change
%%% R3A/6      2015-04-23 erarafo     Support for implementer
%%% R4A/1      2015-04-29 erarafo     Adapted to record type change
%%% R4A/2      2015-04-29 erarafo     Catching an invalid ICTI call
%%% R4A/3      2015-05-19 erarafo     New insertion algorithm
%%% R4A/4      2015-05-28 erarafo     Scoreboard eliminated
%%% R4A/5      2015-06-02 erarafo     Exception handling
%%% R4A/6      2015-06-04 erarafo     Function signature change
%%% R4A/7      2015-06-09 erarafo     Improved fault handling
%%% R4A/8      2015-06-10 erarafo     Redundant records removed
%%% R4A/9      2015-07-04 erarafo     Redundant record usage refactored
%%% R4A/10     2015-08-09 etxjotj     Added transform_schema
%%% R4A/12     2015-09-16 erarafo     Deprecated: waitForClasses/2
%%% R4A/13     2015-09-16 erarafo     Support for Erlang-internal ICTI sessions
%%% R4A/14     2015-09-17 erarafo     Refactoring
%%% R4A/15     2015-09-17 erarafo     Fixing fault that was introduced in R4A/14
%%% R4A/16     2015-10-14 etxpeno     Using the time functionality in OTP 18
%%% R6A/1      2016-08-10 erarafo     Warn if trying to write zero IGs
%%% R7A/1      2016-10-06 erarafo     EDoc additions
%%% R8A/1      2016-11-24 erarafo     Add support for safcImmCtReadInstances_2
%%% R8A/2      2017-01-03 ecaiyan     Support for initialize_2
%%% R8A/2      2017-01-03 ecaiyan     Remove redundant printout
%%% R9A/1      2017-01-31 ecaiyan     Improved logging
%%% R9A/2      2017-02-23 ecaiyan     Bug fix ,Return error when both Delay & MTC true
%%% ----------------------------------------------------------


-include("gmf.hrl").
-include("gmfImmUg.hrl").

-define(UNDEFINED, -1).
-define(MASTER_MODULE, gmfImmUgMaster).


%% Interface for ICTI sessions from within the Erlang VM.

-export([initialize/1,
	 readSchemaVersion/3,
	 readInstances/3,
	 copyInstances/3,
	 copyRtInstances/4,
	 writeInstances/3,
	 writeRtInstances/4,
	 failUpgrade/3,
	 finalize/2]).


%% Convenience functions that invoke ICTI sessions.

-export([waitForDataConversion/1,
	 waitForClasses/2,            % deprecated
	 transform_schema/4]).


%% SAF callback functions

-export([init/0,
	 handle_message/2]).


%% Block-internal functions

-export([getVersion/0]).


-record(state, {appPid=?UNDEFINED  :: integer()}).


%%% ----------------------------------------------------------
%%% @doc Initialize an ICTI session from within the Erlang VM.
%%%
%%% An Erlang ICTI user should provide an integer client id
%%% not less than 100000 (coordination between blocks is
%%% recommendable). The client id will appear in log messages.
%%%
%%% The handle returned by the initialize/1 function must be
%%% used in all subsequent function calls within the session.
%%%
%%% See also: The ICTI IWD, which describes ICTI sessions from
%%% a C application perspective.
%%% @end
%%% ----------------------------------------------------------
-spec initialize(non_neg_integer()) -> any() | {error, any()}.

initialize(ClientId) ->
    case checkClientId(ClientId) of
	{error, _}=E ->
	    E;
	ok ->
	    {{ok, Handle}, _} =
		handle_message({initialize, ClientId}, #state{}),
	    Handle
    end.


%%% ----------------------------------------------------------
%%% @doc Read the from- and to-versions of the specified
%%% schema. The format of the return value is
%%% {ok, #safsImmCtSchemaVersion{}, #safsImmCtSchemaVersion{}}
%%% where the record is defined in safs_imm_ct_p.hrl.
%%% @end
%%% ----------------------------------------------------------
-spec readSchemaVersion(non_neg_integer(), any(), binary()) ->
	  {ok, [non_neg_integer()], [non_neg_integer()]} |
	      {error, any()}.

readSchemaVersion(ClientId, Handle, SchemaName) ->
    case handle_message(
	   {read_schema_version, Handle, SchemaName},
	   #state{appPid=ClientId}) of
	{{error, _}=E, _State} ->
	    E;
	{{ok, OldVersion, NewVersion}, _State} ->
	    {ok, OldVersion, NewVersion}
    end.

%%% ----------------------------------------------------------
%%% @doc Read all from-version instances of each of the given
%%% classes. The Result is a list of #safsImmCtInstanceGroup{}
%%% as defined in safs_imm_ct_p.hrl.
%%% @end
%%% ----------------------------------------------------------
-spec readInstances(non_neg_integer(), any(), [binary()]) ->
	  {ok, any()} | {error, any()}.

readInstances(ClientId, Handle, ClassNames) ->
    case handle_message(
	   {read_instances, Handle, ClassNames},
	   #state{appPid=ClientId}) of
	{{error, _}=E, _State} ->
	    E;
	{{ok, Result}, _State} ->
	    {ok, Result}
    end.


%%% ----------------------------------------------------------
%%% @doc Copy all from-version instances of each of the given
%%% classes to the to-version IMM store.
%%% @end
%%% ----------------------------------------------------------
-spec copyInstances(non_neg_integer(), any(), [binary()]) ->
	  ok | {error, any()}.

copyInstances(ClientId, Handle, ClassNames) ->
    case handle_message(
	   {copy_instances, Handle, ClassNames},
	   #state{appPid=ClientId}) of
	{{error, _}=E, _State} ->
	    E;
	{ok, _State} ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc Copy all from-version persistent runtime instances of
%%% each of the given classes to the to-version IMM store.
%%% @end
%%% ----------------------------------------------------------
-spec copyRtInstances(non_neg_integer(), any(), binary(), [binary()]) ->
	  ok | {error, any()}.

copyRtInstances(ClientId, Handle, Implementer, ClassNames) ->
    case handle_message(
	   {copy_rt_instances, Handle, Implementer, ClassNames},
	   #state{appPid=ClientId}) of
	{{error, _}=E, _State} ->
	    E;
	{ok, _State} ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc Write the given instance groups to the to-version
%%% IMM store. The InstanceGroups argument is a list of
%%% #safsImmCtInstanceGroup{} as defined in safs_imm_ct_p.h.
%%% @end
%%% ----------------------------------------------------------
-spec writeInstances(non_neg_integer(), any(), list()) ->
	  ok | {error, any()}.

writeInstances(ClientId, Handle, InstanceGroups) ->
    case handle_message(
	   {write_instances, Handle, InstanceGroups},
	   #state{appPid=ClientId}) of
	{{error, _}=E, _} ->
	    E;
	{ok, _} ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc Write the given persistent runtime instance groups to the
%%% to-version IMM store. The InstanceGroups argument is a list of
%%% #safsImmCtInstanceGroup{} as defined in safs_imm_ct_p.h.
%%% @end
%%% ----------------------------------------------------------
-spec writeRtInstances(non_neg_integer(), any(), binary(), list()) ->
	  ok | {error, any()}.

writeRtInstances(ClientId, Handle, Implementer, InstanceGroups) ->
    case handle_message(
	   {write_rt_instances, Handle, Implementer, InstanceGroups},
	   #state{appPid=ClientId}) of
	{{error, _}=E, _} ->
	    E;
	{ok, _} ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc Logs the given message (a string is expected) and
%%% fails the upgrade.
%%% @end
%%% ----------------------------------------------------------
-spec failUpgrade(non_neg_integer(), any(), list()) -> ok | {error, any()}.

failUpgrade(ClientId, Handle, Message) ->
    case handle_message(
	   {fail_upgrade, Handle, Message},
	   #state{appPid=ClientId}) of
	{{error, _}=E, _} ->
	    E;
	{ok, _} ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc Finalize the ICTI session. The call will hang until
%%% the data conversion as a whole has terminated (this may
%%% takes several minutes).
%%% @end
%%% ----------------------------------------------------------
-spec finalize(non_neg_integer(), any()) ->
	  ok | {error, sa_ais_err_bad_handle}.

finalize(ClientId, Handle) ->
    case handle_message({finalize, Handle}, #state{appPid=ClientId}) of
	{ok, _} ->
	    ok;
	{{error, ?SA_ERR_BAD_HANDLE}, _} ->
	    {error, ?SA_ERR_BAD_HANDLE}
    end.


%%% ----------------------------------------------------------
%%% @doc The function returns when all IMM classes are
%%% converted and ready for IMM_OM and IMM_OI access.
%%% This function may only be called when the startup
%%% context is "upgrade", which must be tested with the
%%% boolean function swmI:is_upgrade_ongoing().
%%%
%%% A client id is required with a value of 100000
%%% or higher. This id will appear in log messages.
%%% The id is not checked for uniqueness; concurrent
%%% calls using the same id will not interfere except
%%% that log messages will be hard to interpret.
%%%
%%% Client id definitions may be placed in gmf.hrl.
%%%
%%% Return values are 'ok' or {error, Reason}.
%%% @end
%%% ----------------------------------------------------------

-spec waitForDataConversion(non_neg_integer()) ->
	  ok | {error, any()}.

waitForDataConversion(ClientId) ->

    case initialize(ClientId) of
	{error, _}=E ->
	    E;
	Handle ->
	    case finalize(ClientId, Handle) of
		{error, _}=E ->
		    E;
		ok ->
		    ok
	    end
    end.


%%% ----------------------------------------------------------
%%% @doc DEPRECATED:
%%%
%%% The function returns when all IMM classes are
%%% converted and ready for IMM_OM and IMM_OI access.
%%% This function may only be called when the startup
%%% context is "upgrade", which must be tested with the
%%% boolean function swmI:is_upgrade_ongoing().
%%%
%%% A client id is required with a value of 100000
%%% or higher. This id will appear in log messages.
%%% The id is not checked for uniqueness; concurrent
%%% calls using the same id will not interfere except
%%% that log messages will be hard to interpret.
%%%
%%% The "Classes" argument is ignored, ever since the
%%% "wait for classes" functionality was removed. The
%%% name of the function is misleading since there is
%%% no capability to wait for specified classes. Please
%%% use the new function waitForDataConversion instead.
%%%
%%% Return values are 'ok' or {error, Reason}.
%%% @end
%%% ----------------------------------------------------------
-spec waitForClasses(non_neg_integer(), [className()]) ->
	  ok | {error, any()}.

waitForClasses(ClientId, _Classes) ->
    waitForDataConversion(ClientId).


%%% ----------------------------------------------------------
%%% @doc Transform MO instances of classes within a schema.
%%% Transformations are performed by the given transformation
%%% function which must follow the specification
%%%
%%%    fun({OldVer, NewVer}, [#safsImmCtInstanceGroup{}]) ->
%%%            {ok, [#safsImmCtInstanceGroup{}]} |
%%%            {error, list()}
%%%
%%% where versions are represented as lists of integers of
%%% length 3.
%%%
%%% The transformation function is only invoked if the new
%%% version differs from the old version.
%%%
%%% If the transformation function returns {error, Message}
%%% then a "fail upgrade" request will be issued.
%%%
%%% In case of an 'ok' return the call will hang until the
%%% entire data conversion is completed (this may take several
%%% minutes). An {error, Reason} return on the other hand will
%%% happen promptly.
%%%
%%% Client id definitions may be placed in gmf.hrl.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec transform_schema(non_neg_integer(), binary(), [binary()], fun()) ->
	  ok | {error, any()}.

transform_schema(ClientId, SchemaName, ClassNames, TransformFun) ->
    case initialize(ClientId) of
	{error, _}=E ->
	    E;
	Handle ->
	    Finalizer = fun() -> finalize(ClientId, Handle) end,
	    case readSchemaVersion(ClientId, Handle, SchemaName) of
		{error, _}=E ->
		    spawn(Finalizer),
		    E;
		{ok, Same, Same} ->
		    %% Instances will be copied automatically,
		    %% no action allowed in this session
		    finalize(ClientId, Handle),
		    ok;
		{ok, OldVersion, NewVersion} ->
		    case readInstances(ClientId, Handle, ClassNames) of
			{error, _}=E ->
			    spawn(Finalizer),
			    E;
			{ok, InstanceGroups} ->
			    case TransformFun({OldVersion, NewVersion}, InstanceGroups) of
				{error, Reason}=E ->
				    failUpgrade(ClientId, Handle, Reason),
				    spawn(Finalizer),
				    E;
				{ok, NewInstanceGroups} ->
				    case writeInstances(ClientId, Handle, NewInstanceGroups) of
					{error, _}=E ->
					    spawn(Finalizer),
					    E;
					ok ->
					    finalize(ClientId, Handle),
					    ok
				    end
			    end
		    end
	    end
    end.


%%% ----------------------------------------------------------
%%% @doc This function may only be called from SAF.
%%% @end
%%% ----------------------------------------------------------
-spec init() -> {ok, #state{}}.

init() ->
    {ok, #state{}}.


%%% ----------------------------------------------------------
%%% @doc Handle a request by the application. All string-like
%%% data items are binarized.
%%%
%%% This function is called from SAF.
%%%
%%% Result codes "sa_ais_*" are listed in SAF_CAX*/esrc/safs_log.hrl.
%%% @end
%%% ----------------------------------------------------------
-spec handle_message(tuple(), any()) ->
	  {ok, #state{}} |
	      {{ok, any()}, #state{}} |
	      {{ok, #safsImmCtSchemaVersion{}, #safsImmCtSchemaVersion{}}, #state{}} |
	      {{error, any()}, #state{}}.


handle_message({initialize=Op, AppPid}, State) ->
    Schedule = [{1000, 10000}, {4000, 60000}, {16000, 300000}, {60000, -1}],
    waitForMaster(Schedule),
    Handle = gmfImmUgMaster:initIctiSession(false),
    ProgramName =
	case cec:get_program_name(AppPid) of
	    undefined ->
		"<not available>";
	    String ->
		String
	end,
    ?INFO("app=~w ~w, handle: ~p, program: ~s",
	  [AppPid, Op, Handle, ProgramName]),
    {{ok, Handle}, State#state{appPid=AppPid}};

handle_message({initialize_2=Op, AppPid, Delay}, State) ->
    Schedule = [{1000, 10000}, {4000, 60000}, {16000, 300000}, {60000, -1}],
    waitForMaster(Schedule),
    Handle = gmfImmUgMaster:initIctiSession(Delay),
    ProgramName =
	case cec:get_program_name(AppPid) of
	    undefined ->
		"<not available>";
	    String ->
		String
	end,
    ?INFO("app=~w ~w, handle: ~p, program: ~s",
	  [AppPid, Op, Handle, ProgramName]),
    case Handle of
	{error, Error}-> {{error, Error}, State#state{appPid=AppPid}};
	_ ->{{ok, Handle}, State#state{appPid=AppPid}}
    end;

handle_message({finalize=Op, Handle}, #state{appPid=AppPid}=State) ->
    ?INFO("app=~w ~w: requested", [AppPid, Op]),
    R = make_ref(),
    gmfImmUgMaster:finSync(Handle, self(), R),
    receive
	{error, R, ?SA_ERR_BAD_HANDLE} ->
	    {{error, ?SA_ERR_BAD_HANDLE}, State};
	{ok, R} ->
	    %?INFO("~p: finalize, ready", [From]),
	    ?INFO("app=~w ~w: accomplished", [AppPid, Op]),
	    {ok, State#state{appPid=?UNDEFINED}}
    end;

handle_message({read_schema_version=Op, Handle, SchemaName}, #state{appPid=AppPid}=State) ->
    ?INFO("app=~w ~w, schema: ~p", [AppPid, Op, SchemaName]),
    case gmfImmUgMaster:readSchemaVersion(Handle, SchemaName) of
	{error, _}=E ->
	    ?INFO("app=~w ~w, failed: ~p", [AppPid, Op, E]),
	    {E, State};
	{ok, OldVersion, NewVersion} ->
	    ?INFO("app=~w ~w, returning: old: ~120p, new: ~120p",
		  [AppPid, Op,
		   [OldVersion#safsImmCtSchemaVersion.version,
		    OldVersion#safsImmCtSchemaVersion.release,
		    OldVersion#safsImmCtSchemaVersion.correction
		   ],
		   [NewVersion#safsImmCtSchemaVersion.version,
		    NewVersion#safsImmCtSchemaVersion.release,
		    NewVersion#safsImmCtSchemaVersion.correction]
		  ]),
	    {{ok, OldVersion, NewVersion}, State}
    end;

handle_message({fail_upgrade=Op, Handle, Message}, #state{appPid=AppPid}=State) ->
    ?INFO("app=~w fail_upgrade, message: ~s", [AppPid, Message]),
    case gmfImmUgMaster:failUpgrade(Handle, AppPid, Message) of
	{error, ?SA_ERR_BAD_OPERATION}=E ->
	    ?INFO("app=~w ~w attempted after 'confirm', ignored: ~s", [AppPid, Op, E]),
	    {E, State};
	{error, _}=E ->
	    ?INFO("app=~w ~w, failed: ~s", [AppPid, Op, E]),
	    {E, State};
	_ ->
	    {ok, State}
    end;

handle_message({read_instances=Op, Handle, ClassNames}, #state{appPid=AppPid}=State) ->
    readInstancesHelper(AppPid, ClassNames, Op, State, Handle, false);
    
handle_message({read_instances_2=Op, Handle, ClassNames}, #state{appPid=AppPid}=State) ->
    readInstancesHelper(AppPid, ClassNames, Op, State, Handle, true);

handle_message({write_instances=Op, Handle, InstanceGroups}, State) ->
    writeInstances(Op, Handle, undefined, InstanceGroups, State);

handle_message({write_rt_instances=Op, Handle, Implementer, InstanceGroups}, State) ->
    writeInstances(Op, Handle, Implementer, InstanceGroups, State);

handle_message({copy_instances=Op, Handle, ClassNames}, #state{appPid=AppPid}=State) ->
    ?INFO("app=~w ~w, classes -~n~160p",
	  [AppPid, Op, gmfImmUgLib:binariesToStrings(ClassNames, ?MAX_STRLEN, [])]),
    Result = gmfImmUgMaster:copyInstances(Handle, AppPid, ClassNames, undefined),
    {Result, State};

handle_message({copy_rt_instances=Op, Handle, Implementer, ClassNames}, #state{appPid=AppPid}=State) ->
    ?INFO("app=~w ~w, implementer: ~p, classes -~n~160p",
	  [AppPid, Op, Implementer, gmfImmUgLib:binariesToStrings(ClassNames, ?MAX_STRLEN, [])]),
    Result = gmfImmUgMaster:copyInstances(Handle, AppPid, ClassNames, Implementer),
    {Result, State};

handle_message({wait_for_classes=Op, Handle, ClassNames}, #state{appPid=AppPid}=State) ->
    ?INFO("app=~w ~w -~n~160p",
	  [AppPid, Op, gmfImmUgLib:binariesToStrings(ClassNames, ?MAX_STRLEN, [])]),
    Result = gmfImmUgMaster:waitForClasses(Handle, AppPid, Op, ClassNames),
    {Result, State};

handle_message(Other, State) ->
     ?FAULT([stack, restart], "unexpected message: ~p", [{Other, State}]),
     {{error, ?SA_ERR_BAD_OPERATION}, State}.




readInstancesHelper(AppPid, ClassNames, Op, State, Handle, IncludeAll) ->
    ?INFO("app=~w Op=~w -~n~160p",
	  [AppPid, Op,
	   gmfImmUgLib:binariesToStrings(ClassNames, ?MAX_STRLEN, [])]),
    case gmfImmUgMaster:readInstances(Handle, ClassNames, AppPid, Op, IncludeAll) of
	{error, _}=E ->
	    {E, State};
	% InstanceGroupInfos :: [{#xOldClassInfo{}, #instanceGroup{}}]
	{ok, InstanceGroupInfos} ->
	    try convertInstanceGroups(InstanceGroupInfos) of
		Result ->
		    {{ok, Result}, State}
	    catch
		throw:#exception{}=Ex ->
		    #exception{text=Text, data=Data} = gmfImmUgLib:exception(Ex),
		    ?FAULT([{source, read}, stack], "app=~w ~w, ~s, ~p",
			   [AppPid, Op, Text, Data]),
		    {{error, ?SA_ERR_INVALID_PARAM}, State};
		ExType:ExData ->
		    ?FAULT([{source, read}, stack],
			   "app=~w ~w, failed: ~p",
			   [AppPid, Op, {ExType, ExData}]),
		    {{error, ?SA_ERR_INVALID_PARAM}, State}
	    end
    end.





%%% ----------------------------------------------------------
%%% @doc Returns the version of this module.
%%% @end
%%% ----------------------------------------------------------
-spec getVersion() -> atom().

getVersion() ->
    gmfImmUgLib:getVersion(?MODULE).


%%% ----------------------------------------------------------
%%% @doc Checks the given client id. An integer not less than
%%% 100000 is required.
%%% @end
%%% ----------------------------------------------------------
-spec checkClientId(non_neg_integer()) ->  ok  | {error, bad_client_id}.

checkClientId(ClientId) when not is_integer(ClientId) ->
    {error, bad_client_id};

checkClientId(ClientId) when ClientId < 100000 ->
    {error, bad_client_id};

checkClientId(_ClientId) ->
    ok.


%%% ----------------------------------------------------------
%%% @doc Waits for the master to become registered and ready.
%%% A schedule is required; the purpose is to allow frequent
%%% polls in the beginning and less frequent ones if the server
%%% does not show up. For example,
%%%
%%% waitForServer([{100, 1000}, {800, 5000}, {20000, -1}])
%%%
%%% means poll every 100 ms initially, then after 1000 ms start
%%% polling every 800 ms, then after 5000 start polling every
%%% 20000 ms forever.
%%%
%%% The time limits are relative to the point in time when this
%%% function was invoked. The schedule must contain at least
%%% one entry.
%%% @end
%%% ----------------------------------------------------------
-spec waitForMaster([{integer(), integer()}]) ->  ok.

waitForMaster(Schedule) ->
    waitForMaster(Schedule, erlang:monotonic_time()).

waitForMaster([{SleepMillis, LimitMillis}|Tail]=Schedule, Beginning) ->
    case whereis(?MASTER_MODULE) =/= undefined andalso
	     gmfImmUgMaster:isReady() of
	false ->
	    ?INFO("~p: waiting for ~p, sleep ~w ms", [self(), ?MASTER_MODULE, SleepMillis]),
	    timer:sleep(SleepMillis),
	    case Tail of
		[] ->
		    waitForMaster(Schedule, Beginning);
		_ ->
		    Now = erlang:monotonic_time(),
		    Elapsed = Now-Beginning,
		    ElapsedMillis = erlang:convert_time_unit(Elapsed, native,
							     milli_seconds),
		    if
			ElapsedMillis >= LimitMillis ->
			    waitForMaster(Tail, Beginning);
			true ->
			    waitForMaster(Schedule, Beginning)
		    end
	    end;
	_ ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc Write instance groups to the IMM store.
%%% @end
%%% ----------------------------------------------------------
-spec writeInstances (atom(), any(), any(), any(), #state{}) ->
	  {ok, #state{}} |
	      {{error, sa_ais_err_bad_handle}, #state{}} |
	      {{error, sa_ais_err_not_exist}, #state{}} |
	      {{error, sa_ais_err_invalid_param}, #state{}}.

writeInstances(Op, Handle, Implementer, InstanceGroups, #state{appPid=AppPid}=State) ->
    Classes = [C || #safsImmCtInstanceGroup{className=C} <- InstanceGroups],
    ?INFO("app=~w ~w, classes -~n~160p",
	  [AppPid, Op, gmfImmUgLib:binariesToStrings(Classes, ?MAX_STRLEN, [])]),
    case gmfImmUgMaster:waitForClasses(Handle, AppPid, Op, Classes) of
	{error, _Reason}=E ->
	    {E, State};
	ok ->
	    ImmInstanceGroups =
		[#immInstanceGroup{className=Classname,
				   immInsts=Instances,
				   maxLength=maxLength(Instances),
				   implementer=Implementer}
		 || #safsImmCtInstanceGroup{className=Classname,
					    instances=Instances} <- InstanceGroups],
	    if
		ImmInstanceGroups =:= [] ->
		    ?WARNING(
		        "attempt to insert zero IGs, source: write, app=~w",
		        [AppPid]);
		true ->
		    ok
	    end,
	    case gmfImmUgInserter:addInstGroups(write, ImmInstanceGroups) of
		{error, _}=E ->
		    {E, State};
		ok ->
		    {ok, State}
	    end
    end.












%% 	{{ok, Same, Same}, S2} ->
%% 	    %% Instances will be copied
%% 	    {ok, _} = handle_message({finalize, Handle}, S2),
%% 	    ok;
%% 	{{ok, OldVersion, NewVersion}, S2} ->
%% 	    {{ok, InstanceGroups}, S3} =
%% 		handle_message({read_instances, Handle, ClassNames}, S2),
%% 	    case TransformFun({OldVersion, NewVersion}, InstanceGroups) of
%% 		{ok, NewInstanceGroups} ->
%% 		    {ok, S4} = handle_message(
%% 				 {write_instances, Handle, NewInstanceGroups}, S3),
%% 		    {ok, _} = handle_message({finalize, Handle}, S4),
%% 		    ok;
%% 		{error, Reason} ->
%% 		    Message = list_to_binary(Reason),
%% 		    {ok, _} = handle_message({fail_upgrade, Handle, Message}, S3),
%% 		    ok
%% 	    end
%%     end.



%%% ----------------------------------------------------------
%%% @doc Convert instance groups from internal to IMM format.
%%% @end
%%% ----------------------------------------------------------
-spec convertInstanceGroups([{#oldClassInfo{}, #instanceGroup{}}]) ->
	  [#safsImmCtInstanceGroup{}].

convertInstanceGroups(InstanceGroupInfos) ->
    lists:map(
      fun({XOCI, #instanceGroup{className=ClassName, instances=Instances}}) ->
	      ConvertedInstances =
		  lists:map(
		    fun(#restoredInstance{key=Key, class=ClassA, attrs=Attrs}) ->
			    ParentName = gmfImmUgLib:mimDnToImmParentName(Key),
			    %% ?INFO("passing parent name: ~p", [ParentName]),
			    AttrValues = convertAttrValues(Attrs, Key, ClassA, XOCI),
			    %% ?INFO("passing attribute values: ~p", [AttrValues]),
			    #safsImmCtInstance{parentName=ParentName,
					       attrValues=AttrValues}
		    end,
		    Instances),
	      #safsImmCtInstanceGroup{className=ClassName,
				      instances=ConvertedInstances}
      end,
      InstanceGroupInfos).


%%% ----------------------------------------------------------
%%% @doc Create a list of #safsImmAttrValues_2{} from the given
%%% attibutes of the old-version instance.
%%% @end
%%% ----------------------------------------------------------
-spec convertAttrValues([{atom(), [term()]}], [binary()], atom(), #oldClassInfo{}) ->
                           [#safsImmAttrValues_2{}].

convertAttrValues(Attrs, Path, ClassNameA, #oldClassInfo{rdnName=RdnB}=XOCI) ->
    AttrValues =
	[gmfImmUgLib:encodeAttr(ClassNameA, AttrDescr, XOCI)
	   ||{_, Values}=AttrDescr <- Attrs,
	     Values =/= '$imm_runtime',
	     Values =/= '$imm_db_cache'],

    SelfB = lists:last(Path),
    [_, RdnValueS] = string:tokens(binary_to_list(SelfB), "="),
    Rdn = #safsImmAttrValues_2{attrName=RdnB,
			     attrValueType=sa_imm_attr_sastringt,
			     attrValuesNumber=1,
			     attrValues=
				 [#safsImmAttrValue{sastring=list_to_binary(RdnValueS)}]},
    [Rdn]++AttrValues.


%%% ----------------------------------------------------------
%%% @doc Returns the maximum DN length (in the MIM sense) of
%%% the given list of instances. If the list is empty then 0
%%% is returned.
%%% @end
%%% ----------------------------------------------------------
-spec maxLength([#safsImmCtInstance{}]) -> non_neg_integer().

maxLength(Insts) ->
    lists:foldl(
      fun(#safsImmCtInstance{parentName=PN}, Acc) ->
	      max(Acc,
		  if
		      PN =:= undefined ->
			  1;
		      true ->
			  PNS = binary_to_list(PN),
			  1 + length(string:tokens(PNS, ","))
		  end
		 )
      end,
      0,
      Insts).
