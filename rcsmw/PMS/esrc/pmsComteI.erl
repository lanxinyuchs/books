%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsComteI.erl %
%%%
%%% Description:
%%%
%%%
%%%
%%% ----------------------------------------------------------
-module(pmsComteI).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R8A/R10A/1').
-date('2017-04-21').
-author('etxpeno').
-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R1A/1      2012-01-18 etxjotj     Created
%%% R5A/1      2015-10-08 etxjotj     Preliminary OTP R18 adaption
%%% R5A/3      2015-11-19 uabesvi     Allow update of pm jobs
%%% R5A/5      2015-12-22 uabesvi     PmSupport
%%% R10A/1     2017-04-21 etxpeno     Improved nextMo/3
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([getMoAttribute/2]).
-export([nextMo/3]).
-export([createMo/4]).
-export([setMoAttribute/4]).
-export([deleteMo/2]).
-export([prepare/3]).
-export([commit/3]).
-export([finish/3]).
-export([action/3]).
-export([prepareTransaction/2]).
-export([abortTransaction/1]).

-export([existsMo/2]).
-export([countMoChildren/3]).
-export([getMoAttributes/3]).
-export([setMoAttributes/3]).
-export([action/4]).
-export([createMo/5]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("RcsPm.hrl").
-include("RmePmSupport.hrl").
-include("pms.hrl").

-define(DEFAULT_JOB, #pmJob{requestedJobState = ?JobState_ACTIVE,
			    granularityPeriod = ?TimePeriod_FIFTEEN_MIN,
 			    reportingPeriod   = ?TimePeriod_FIFTEEN_MIN,
			    jobPriority       = ?JobPriority_MEDIUM,
			    jobControl        = ?JobControl_FULL,
			    jobType           = ?JobType_MEASUREMENTJOB,
			    currentJobState   = ?JobState_STOPPED
			   }).

-define(REQ_JOB_STATE, <<"requestedJobState">>).
-define(REQ_JOB_CTRL, <<"jobControl">>).

-define(REFERENCE, 11).
-define(ENUM,      12).
-define(STRUCT,    14).

-define(STRING, 9).
-define(STRING(Str),{?STRING, Str}).

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%=====================================================================
%% existsMo(ReversedDn::[binary()], TransId:integer()) -> true | false
%%=====================================================================
existsMo(DnRev, _Tx) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:existsMo(DnRev, Table).



%%=====================================================================
%% countMoChildren(ReversedDn::[binary()],
%%                 ClassName::binary(),
%%                 TransId:integer()) -> non_neg_integer().
%%=====================================================================
countMoChildren(ReversedDn, ClassName, _Tx) ->
    comsaGeneric:countMoChildren(ReversedDn, ClassName).



getMoAttribute([<<"moClass">> | DnRev], _) ->
    Key = comsaGeneric:dnrev_to_key(DnRev),
    case pmsDb:pm_group_get(Key) of
	{ok, [Obj]} ->
	    Struct = Obj#pmGroup.moClass,
	    comsaGeneric:format_struct(Struct,
				       ?'ManagedObjectClass_types');
	{ok, []} ->
	    undefined
    end;
getMoAttribute([<<"measurementSpecification">> | DnRev], _) ->
    Key = comsaGeneric:dnrev_to_key(DnRev),
    case pmsDb:measurement_reader_get(Key) of
	{ok, [Obj]} ->
	    Struct = Obj#measurementReader.measurementSpecification,
	    comsaGeneric:format_struct(Struct,
				       ?'MeasurementSpecification_types');
	_ ->
	    undefined
    end;
getMoAttribute([<<"measurementReaderNameValue">> | DnRev], _) ->
    Key = comsaGeneric:dnrev_to_key(DnRev),
    case pmsDb:measurement_reader_get(Key) of
	{ok, [Obj]}  ->
	    case Obj#measurementReader.measurementReaderNameValue of
		undefined ->
		    undefined;
		Structs ->
		    Types = ?'MeasurementReaderNameValue_types',
		    [comsaGeneric:format_struct(Struct, Types) ||
			Struct <- Structs]
	    end;
	_ ->
	    undefined
    end;
getMoAttribute([<<"currentJobState">>, JobId | _], _) ->
    Job = pmsDb:pm_job_dirty_get({"1", "1", "1", binary_to_list(JobId)}),
    case Job of
	{ok, [#pmJob{currentJobState = CS}]} when CS == ?JobState_ACTIVE;
						  CS == ?JobState_STOPPED ->
	    {?ENUM, CS};
	_ ->
	    undefined
    end;
getMoAttribute([Attribute | DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).


%%=====================================================================
%% getMoAttributes(AttrNames::[binary()],
%%                 ReversedDn:[binary()], TransId:integer()) - > [com_value()]
%%=====================================================================
getMoAttributes(AttrNames, DnRev, Tx) ->
    case comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))) of
	true  -> getMA([getMoAttribute([AN | DnRev], Tx) || AN <- AttrNames]);
	false -> []
    end.


getMA([[]]) ->
    [];
getMA(Attrs) ->
    Attrs.

nextMo(Dn, Key, _) ->
    Table = table(binary_to_list(hd(Dn))),
    generic_nextMo(Table, Dn, Key).

generic_nextMo(Table, Dn, undefined) ->
    DnKeyPart = comsaGeneric:dnrev_to_key(tl(Dn)),
    DnKey = erlang:append_element(DnKeyPart, '$0'),
    MatchHead = matchhead(DnKey, Table),
    Result = {{'$0'}},
    MatchSpec = [{MatchHead, [], [Result]}],
    case mnesia:select(Table, MatchSpec) of
	[] ->
	    {ok, undefined};
	[{NextKey}|T] ->
	    EtsId = ets:new(?MODULE, []),
	    ets:insert(EtsId, T),
	    {ok, {?STRING(list_to_binary(NextKey)), EtsId}}
    end;
generic_nextMo(_Table, _Dn, {EtsId, PrevKey}) ->
    case ets:next(EtsId, PrevKey) of
	'$end_of_table' ->
	    ets:delete(EtsId),
	    {ok, undefined};
	NextKey ->
	    {ok, {?STRING(list_to_binary(NextKey)), {EtsId, NextKey}}}
    end;
generic_nextMo(_Table, _Dn, EtsId) ->
    case ets:first(EtsId) of
	'$end_of_table' ->
	    ets:delete(EtsId),
	    {ok, undefined};
	NextKey ->
	    {ok, {?STRING(list_to_binary(NextKey)), {EtsId, NextKey}}}
    end.

matchhead(DnKey, pm) ->
    #pm{pmId = DnKey, _ = '_'};
matchhead(DnKey, pmMeasurementCapabilities) ->
    #pmMeasurementCapabilities{pmMeasurementCapabilitiesId = DnKey, _ = '_'};
matchhead(DnKey, measurementType) ->
    #measurementType{measurementTypeId = DnKey, _ = '_'};
matchhead(DnKey, measurementReader) ->
    #measurementReader{measurementReaderId = DnKey, _ = '_'};
matchhead(DnKey, pmJob) ->
    #pmJob{pmJobId = DnKey, _ = '_'};
matchhead(DnKey, pmThresholdMonitoring) ->
    #pmThresholdMonitoring{pmThresholdMonitoringId = DnKey, _ = '_'};
matchhead(DnKey, pmGroup) ->
    #pmGroup{pmGroupId = DnKey, _ = '_'};
matchhead(DnKey, pmSupport) ->
    #pmSupport{pmSupportId = DnKey, _ = '_'}.


setMoAttribute([Attribute | DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).


%%=====================================================================
%% setMoAttributes(AttrNames::[com_named_attribute()],
%%                 ReverseDn::[binary()],
%%                 TransId:integer()) -> ok |{error, Reason}
%%=====================================================================
setMoAttributes(AttrNames, DnRev, _Tx) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), AttrNames).



createMo([<<"PmJob">> = Class | ParentDnRev], _, IxValueB, _) ->
    ?LOG_RAM(?SEV_1,
	     {"createMo ~n"
	      "  PmJob ~p~n", [binary_to_list(IxValueB)]}),
    case is_created(job, get_dn(ParentDnRev), [binary_to_list(IxValueB)]) of
	{ok, []} ->
	    Table = table(binary_to_list(Class)),
	    comsaGeneric:create(Table, ParentDnRev, IxValueB, ?DEFAULT_JOB);
	_  ->
	    {error, <<"SA_AIS_ERR_EXIST">>}
    end;
createMo([<<"MeasurementReader">> = Class | ParentDnRev], _, IxValueB, _) ->
    ?LOG_RAM(?SEV_1,
	     {"createMo ~n"
	      "  MeasurementReader ~p~n", [binary_to_list(IxValueB)]}),
    case is_created(mr, get_dn(ParentDnRev), [binary_to_list(IxValueB)]) of
	{ok, []} ->
	    Table = table(binary_to_list(Class)),
	    comsaGeneric:create(Table, ParentDnRev, IxValueB);
	_  ->
	    {error, <<"SA_AIS_ERR_EXIST">>}
    end.



is_created(job, Dn, JobId) ->
    pmsDb:pm_job_get(list_to_tuple(Dn ++ JobId));
is_created(mr, Dn, MrId) ->
    pmsDb:measurement_reader_dirty_get(list_to_tuple(Dn ++ MrId)).


get_dn(Dn) ->
    get_dn(Dn, []).

get_dn([], Acc) ->
    Acc;
get_dn([Id, _ | T], Acc) ->
    get_dn(T, [binary_to_list(Id) | Acc]).


%%=====================================================================
%% createMo([ClassName::binary()||ReverseDn::[binary()]],
%%          KeyAttrName::binary(),
%%          KeyValue::binary(),
%%          InitAttrs::[com_named_attribute()],
%%          TransId:integer()) -> {ok, NewObj}
%%=====================================================================
createMo([Class | DnRev], _KeyAttrName, KeyValue, InitAttrs, _Tx) ->
    Table = table(binary_to_list(Class)),
    Attrs = createMo_attrs(Table, InitAttrs, []),
    comsaGeneric:create(Table, DnRev, KeyValue, Attrs, types(Table)).

createMo_attrs(_, [], Attrs) ->
    lists:reverse(Attrs);
createMo_attrs(pmJob = Table, [{KeyB, Val} | T], Attrs) ->
    Key = list_to_atom(binary_to_list(KeyB)),
    Type = proplists:get_value(Key, types(Table)),
    A = trans_attr(Type, Val),
    createMo_attrs(Table, T, [{KeyB, A} | Attrs]);
createMo_attrs(measurementReader = Table,
	       [{<<"measurementSpecification">>, {?STRUCT, Val}} | T], Attrs) ->
    GrpRef = proplists:get_value(<<"groupRef">>, Val),
    MrRef  = proplists:get_value(<<"measurementTypeRef">>, Val),
    MS = #'MeasurementSpecification'{groupRef           = trans_attr(GrpRef),
				     measurementTypeRef = trans_attr(MrRef)},
    New = {<<"measurementSpecification">>, MS},
    createMo_attrs(Table, T, [New | Attrs]);
createMo_attrs(measurementReader = Table, [{KeyB, Val} | T], Attrs) ->
    Key = list_to_atom(binary_to_list(KeyB)),
    Type = proplists:get_value(Key, types(Table)),
    A = trans_attr(Type, Val),
    createMo_attrs(Table, T, [{KeyB, A} | Attrs]).



trans_attr(undefined) ->
    undefined;
trans_attr({?REFERENCE, Val}) ->
    Val.


trans_attr(string, Val) ->
    Val;
trans_attr(_, {?ENUM, Val}) ->
    Val.



deleteMo([Id, Class | _] = DnRev, _)
  when Class == <<"PmJob">>;
       Class == <<"MeasurementReader">> ->
    ?LOG_RAM(?SEV_1,
	     {"deleteMo~n"
	      "  Class = ~p~n"
	      "  Id    = ~p~n", [binary_to_list(Class), binary_to_list(Id)]}),
    Table = table(binary_to_list(Class)),
    comsaGeneric:delete(DnRev, Table).

table("Pm")                        -> pm;
table("PmMeasurementCapabilities") -> pmMeasurementCapabilities;
table("MeasurementType")           -> measurementType;
table("MeasurementReader")         -> measurementReader;
table("PmJob")                     -> pmJob;
table("PmThresholdMonitoring")     -> pmThresholdMonitoring;
table("PmGroup")                   -> pmGroup;
table("PmSupport")                 -> pmSupport.

types(pm)                        -> ?pm_types;
types(pmMeasurementCapabilities) -> ?pmMeasurementCapabilities_types;
types(measurementType)           -> ?measurementType_types;
types(measurementReader)         -> ?measurementReader_types;
types(pmJob)                     -> ?pmJob_types;
types(pmThresholdMonitoring)     -> ?pmThresholdMonitoring_types;
types(pmGroup)                   -> ?pmGroup_types;
types(pmSupport)                 -> ?pmSupport_types.


abortTransaction(Tx) ->
    gen_server:cast(pmsServer, {abort_transaction, Tx}),
    ok.




%%=====================================================================
%% prepareTransaction(Objects, Tx) -> ok | {abort, Reason}
%%
%% pmsCOmteI is subscribing to changes in
%% ME=1,SF=1,Pm=1 and ME=1,NS=1,PmSupport=1 fragments
%%
%% PM object should be sent to pmsServer,
%% PmSupport are handled locally in pmsComteI
%%=====================================================================
prepareTransaction(Objects, Tx) ->
    PmSup  = [PS || {_, #pmSupport{}} = PS <- Objects],
    ok = prepareTransaction_support(PmSup, Tx),
    prepareTransaction_pm(Objects -- PmSup, Tx).

%% The transaction contained only pmSupport objects
prepareTransaction_pm([], _Tx) ->
    ok;
prepareTransaction_pm(Objects, Tx) ->
    PmJobs  = [PmJob || {_, PmJob} <- Objects, is_record(PmJob, pmJob)],
    Deleted = [{Dn, Params} || {Dn, {deleted, Params}} <- Objects],
    try
	check_params(PmJobs),
	check_predef(Deleted),
	check_state(Deleted),
	Aid = element(2, mnesia:get_activity_id()),

	FormattedObjects = pt_format(Objects),

	FilteredObjects = [Object || {Action, Obj} = Object <- FormattedObjects,
				     Action == deleted orelse
					 is_record(Obj, measurementReader) orelse
					 is_record(Obj, pmJob)],

	case pmsMoVerifier:verify(Aid, FilteredObjects) of
	    {ok, NewNoOfJobs} ->
		PtMsg = {prepare_transaction,
			 Tx,
			 Aid,
			 FormattedObjects,
			 NewNoOfJobs},
		?LOG_RAM(?SEV_1,
			 {"verify transaction ~n"
			  "  TransactionId = ~p~n"
			  "  Result        = ~p~n", [Aid, ok]}),
		gen_server:cast(pmsServer, PtMsg);
	    Error ->
		?LOG_RAM(?SEV_ERROR,
			 {"verify transaction ~n"
			  "  TransactionId = ~p~n"
			  "  Result = ~p~n", [Aid, Error]}),
		{abort, "PMS prepareTransaction verification error."}
	end

    catch
	throw:{?MODULE, Reason} ->
	    ?LOG_RAM(?SEV_ERROR,
		     {"prepare transaction ~n"
		      "  Reason = ~p~n", [Reason]}),
	    {abort, Reason};
	  exit:Exit->
	    ProcInfo = erlang:process_info(whereis(pmsServer)),
	    ?LOG_RAM(?SEV_ERROR,
		     {"prepare transaction exit ~n"
		      "  Reason   = ~p~n"
		      "  ProcInfo = ~p~n",
		      [Exit, ProcInfo]}),
	    {abort, "PMS prepareTransaction timeout."}
    end.

prepareTransaction_support(_PmSup, _Tx) ->
    ok.











check_params([]) ->
    ok;
check_params([#pmJob{reportingPeriod   = RP,
		     granularityPeriod = GP} | _]) when RP < GP ->
    throw({?MODULE, <<"Granularity period > Reporting period">>});
check_params([_|T]) ->
    check_params(T).


%% check_mr([], []) ->
%%     ok;
%% check_mr([], _Mrs) ->
%%     throw({?MODULE,
%% 	   <<"Not allowed to add only a MeasurementReader without a PmJob">>});
%% check_mr(Jobs, Mrs) ->
%%     c_mr(Mrs, Jobs).


%% c_mr([], _Jobs) ->
%%     ok;
%% c_mr([#measurementReader{measurementReaderId = {ME, SF, Pm, Job, _}} | T],
%%      Jobs) ->
%%     case lists:keyfind({ME, SF, Pm, Job}, 2, Jobs) of
%% 	false -> {abort, <<"Not allowed to add MeasurementReader to a PmJob">>};
%% 	_     -> c_mr(T, Jobs)
%%     end.

check_predef([]) ->
    ok;
check_predef([{[JobId, <<"PmJob">> | _], _Params} | T]) ->
    Rec = pmsDb:pm_job_dirty_get({"1", "1", "1", binary_to_list(JobId)}),
    case is_predef(Rec) of
	true  ->
	    throw({?MODULE, <<"Not allowed to remove predefined jobs">>});
	false ->
	    check_predef(T)
    end;
check_predef([_ | T]) ->
    check_predef(T).


is_predef({ok, [#pmJob{jobControl = JC}]})
  when JC == ?JobControl_STARTSTOP orelse
       JC == ?JobControl_VIEWONLY ->
    true;
is_predef(_) ->
    false.



check_state([]) ->
    ok;
check_state([{[_, <<"PmJob">> | _] = Dn, Params} | T]) ->
    JobState = get_job_state(Dn, proplists:get_value(?REQ_JOB_STATE, Params)),
    case JobState of
	{_, ?JobState_ACTIVE} ->
	    throw({?MODULE, <<"Not allowed to remove PmJob in state ACTIVE">>});
	_ ->
	    check_state(T)
    end;
check_state([_ | T]) ->
    check_state(T).


get_job_state(Dn, undefined) ->
    getMoAttribute([<<"currentJobState">> | Dn], internal);
get_job_state(_, State) ->
    State.

%% check_delete(Deleted) ->
%%     Jobs = cd_job(Deleted, []),
%%     case cd_mr(Deleted, Jobs, []) of
%% 	[] -> ok;
%% 	_  -> throw({?MODULE, <<"Not allowed to remove MeasurementType">>})
%%     end.


%% cd_job([], Jobs) ->
%%     Jobs;
%% cd_job([{[_, <<"PmJob">> | _] = Job, _} | T], Jobs) ->
%%     cd_job(T, [list_to_tuple(Job) | Jobs]);
%% cd_job([_ | T], Jobs) ->
%%     cd_job(T, Jobs).


%% cd_mr([], _, MRs) ->
%%     MRs;
%% cd_mr([{[_, <<"MeasurementReader">> | Job] = MR, _} | T], Jobs, MRs) ->
%%     case lists:member(list_to_tuple(Job), Jobs) of
%% 	false -> cd_mr(T, Jobs, [list_to_tuple(MR) | MRs]);
%% 	true  -> cd_mr(T, Jobs, MRs)
%%     end;
%% cd_mr([_ | T], Jobs, MRs) ->
%%     cd_mr(T, Jobs, MRs).


pt_format(Objects) ->
    pt_format(Objects, []).

pt_format([], Objects) ->
    Objects;
pt_format([{[JobId, <<"PmJob">>,
	     PmId, <<"Pm">>,
	     SfId, <<"SystemFunctions">>,
	     MeId, <<"ManagedElement">>],
	    {deleted, _}} | T], Objects) ->
    Obj = {binary_to_list(MeId),
	   binary_to_list(SfId),
	   binary_to_list(PmId),
	   binary_to_list(JobId)},
    pt_format(T, [{deleted, Obj} | Objects]);
pt_format([{[MrId, <<"MeasurementReader">>,
	     JobId, <<"PmJob">>,
	     PmId, <<"Pm">>,
	     SfId, <<"SystemFunctions">>,
	     MeId, <<"ManagedElement">>],
	    {deleted, _}} | T], Objects) ->
    Obj = {binary_to_list(MeId),
	   binary_to_list(SfId),
	   binary_to_list(PmId),
	   binary_to_list(JobId),
	   binary_to_list(MrId)},
    pt_format(T, [{deleted, Obj} | Objects]);
pt_format([{_, Obj} | T], Objects)
  when is_record(Obj, pmJob) orelse
       is_record(Obj, measurementReader) ->
    pt_format(T, [{created, Obj} | Objects]);
%% Not PMS object
pt_format([_ | T], Objects) ->
    pt_format(T, Objects).


prepare(_DN, User, _Tx) -> {ok,User}.
commit(_DN,  User, _Tx) -> {ok,User}.
finish(_DN, _User, _Tx) -> ok.


action(DnRev, Parameters, TransId) ->
    ?LOG_RAM(?SEV_WARNING,
	     {"Not valid: action(~p, ~p, ~p)~n",
	      [DnRev, Parameters, TransId]}),
    {10, false}. %% comTypes.hrl ?BOOL

action(Action, DnRev, NamedParams, TransId) ->
    ?LOG_RAM(?SEV_WARNING,
	     {"Not valid: action(~p, ~p, ~p, ~p)~n",
	      [Action, DnRev, NamedParams, TransId]}),
    {10, false}. %% comTypes.hrl ?BOOL


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
