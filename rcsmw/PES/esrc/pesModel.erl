%%% ----------------------------------------------------------
%%% %CCaseFile:	pesModel.erl %
%%% Author:	etxbjca
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(pesModel).
-vsn('/main/R3A/R4A/R5A/1').
-date('2016-04-05').
-author('uabesvi').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R3A/2      2014-10-27 etxjotj     First real version
%%% R4A/2      2015-08-28 etxjotj     Dialyzer fix
%%% R4A/3      2015-09-08 uabesvi     error_logger -> sysInitI
%%% ----------------------------------------------------------
%%%
%%% ----------------------------------------------------------
%%% MODULE INTERFACE
%%% ----------------------------------------------------------
-export([getMoAttribute/2, nextMo/3]).
-export([createMo/4,setMoAttribute/4,deleteMo/2]).
-export([prepare/3,commit/3,finish/3]).

-export([existsMo/2,
	 countMoChildren/3,
	 getMoAttributes/3,
	 setMoAttributes/3,
	 %% action/4,
	 createMo/5]).

-export([prepareTransaction/2]).
-export([abortTransaction/1]).

-define(DEFAULT_JOB, #eventJob{requestedJobState = ?SessionState_ACTIVE,
			       currentJobState   = ?SessionState_STOPPED,
			       jobControl        = ?JobControl_FULL
			      }).

%%% ----------------------------------------------------------
%%% OTHER EXPORTED FUNCTIONS
%%% ----------------------------------------------------------


%%% ----------------------------------------------------------
%%% HEADER FILES AND MACRO DEFINITIONS
%%% ----------------------------------------------------------
-include("comte_types.hrl").
-include("pes.hrl").
-include("RcsPMEventM.hrl").

%%% ----------------------------------------------------------
%%% TYPES AND RECORDS
%%% ----------------------------------------------------------

-define(REQ_JOB_STATE, <<"requestedJobState">>).

%%% ----------------------------------------------------------
%%% FUNCTIONS
%%% ----------------------------------------------------------
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



getMoAttributes(AttrNames, DnRev, Tx) ->
    case comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))) of
	true  -> [getMoAttribute([AN | DnRev], Tx) || AN <- AttrNames];
	false -> []
    end.



%getMoAttribute([Attribute|DnRev], TxHandle) ->

getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).




%nextMo(Dn, Key, TxHandle) ->
nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).


setMoAttributes(Attrs, DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), Attrs).


setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).


%%% ----------------------------------------------------------
%%% @doc Creates a new instance. The given class name is trusted to be
%%% one of the names supported by this module.
%%% ----------------------------------------------------------
createMo([Class | ParentDnRev], _, IxValueB, _) ->
    ?LOG_RAM(?SEV_1, 
	     {"createMo ~n"
	      "  ~s ~s~n", [binary_to_list(Class), binary_to_list(IxValueB)]}),
    Table = table(binary_to_list(Class)),
    case Class of
	<<"EventJob">> ->
	    comsaGeneric:create(Table, ParentDnRev, IxValueB, ?DEFAULT_JOB);
	_ ->
	    comsaGeneric:create(Table, ParentDnRev, IxValueB)
    end.


-spec createMo([binary()], 
	       mo_attribute_name(), 
	       binary(), 
	       [com_named_attribute()], 
	       integer()) -> 
	  {ok, tuple()}.

createMo([ClassName|ParentDnRev] = Ldn, _, KeyValue, InitAttrs, Tx) ->
    Table = table(binary_to_list(ClassName)),
    comsaGeneric:create(Table, ParentDnRev, KeyValue),
    
    setMoAttributes(InitAttrs, [KeyValue | Ldn], Tx).






deleteMo(DnRev = [Id, <<"EventJob">> = Class | _], _) ->
    ?LOG_RAM(?SEV_1, 
	     {"deleteMo~n"
	      "  Class = ~s~n"
	      "  Id    = ~s~n", [binary_to_list(Class), binary_to_list(Id)]}),
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table);
deleteMo(_, _) ->
    ok.

table("PmEventM") -> pmEventM;
table("EventType") -> eventType;
table("EventJob") -> eventJob;
table("EventGroup") -> eventGroup;
table("StreamingCapabilities") -> streamingCapabilities;
table("FilePullCapabilities") -> filePullCapabilities;
table("EventCapabilities") -> eventCapabilities;
table("EventFilterType") -> eventFilterType;
table("EventProducer") -> eventProducer.			  

types(pmEventM) -> ?pmEventM_types;
types(eventType) -> ?eventType_types;
types(eventJob) -> ?eventJob_types;
types(eventGroup) -> ?eventGroup_types;
types(streamingCapabilities) -> ?streamingCapabilities_types;
types(filePullCapabilities) -> ?filePullCapabilities_types;
types(eventCapabilities) -> ?eventCapabilities_types;
types(eventFilterType) -> ?eventFilterType_types;
types(eventProducer) -> ?eventProducer_types.


prepare(_DN, User, _Tx) ->
    {ok,User}.
commit(_DN, User, _Tx) ->
    {ok,User}.
finish(_DN, _User, _Tx) ->
    ok.

%% action(Name, DnRev, Params, TransId) ->
%%     try do_action(Name, DnRev, Params, TransId)
%%     catch 
%% 	throw:{fail, Reason} ->
%% 	    %% Indicates a fault on behalf of the operator
%% 	    {error, list_to_binary(Reason)};
%% 	Type:Reason ->
%% 	    sysInitI:error_report(
%% 	      [{Type, Reason}, {stacktrace, erlang:get_stacktrace()}]),
%% 	    {error, <<"A software error occurred">>}
%%     end.

%% do_action(Name, DnRev, Params, TransId) ->
%%     {10, false}.



abortTransaction(Tx) ->
    gen_server:call(pesServer, {abort_transaction, Tx}),
    ok.

prepareTransaction(Objects, Tx) ->
    _Created = [Job || {_, Job} <- Objects, is_record(Job, eventJob)],
    Deleted = [{Dn, Params} || {Dn, {deleted, Params}} <- Objects],
    try
	%%check_params(Created),
	check_state(Deleted),
	check_predef(Deleted),
	check_delete(Deleted, []),
	Aid = element(2, mnesia:get_activity_id()),
	Formatted = pt_format(Objects),
	PtMsg = {prepare_transaction, Tx, Aid, Formatted},
	case gen_server:call(pesServer, PtMsg) of
	    ok             -> ok;
	    {error, Error} -> {abort, Error}
	end
    catch
	throw:{?MODULE, Reason} ->
	    {abort, Reason}
    end.


	
check_state([]) ->
    ok;
check_state([{[_, <<"EventJob">> | _] = Dn, Params} | T]) ->
    JobState = get_job_state(Dn, proplists:get_value(?REQ_JOB_STATE, Params)),
    case JobState of
	{_, ?JobState_ACTIVE} ->
	    throw({?MODULE, 
		   <<"Not allowed to remove EventJob in state ACTIVE">>});
	_ ->
	    check_state(T)
    end;
check_state([_ | T]) ->
    check_state(T).


get_job_state(Dn, undefined) ->
    getMoAttribute([<<"currentJobState">> | Dn], internal);
get_job_state(_, State) ->
    State.




check_predef([]) ->
    ok;
check_predef([{[JobId, <<"EventJob">> | _], _Params} | T]) ->
    Key = {"1", "1", "1", "1", binary_to_list(JobId)},
    Rec = pesDb:event_job_dirty_get(Key),
    case is_predef(Rec) of
	true  ->
	    throw({?MODULE, <<"Not allowed to remove predefined jobs">>});
	false ->
	    check_predef(T)
    end;
check_predef([_ | T]) ->
    check_predef(T).


is_predef({ok, [#eventJob{jobControl = JC}]}) 
  when JC == ?JobControl_STARTSTOP orelse
       JC == ?JobControl_VIEWONLY ->
    true;
is_predef(_) -> 
    false.



check_delete([], Jobs) ->
    Jobs;
check_delete([{[_, <<"EventJob">> | _] = Job, _} | T], Jobs) ->
    check_delete(T, [list_to_tuple(Job) | Jobs]);
check_delete([_ | T], Jobs) ->
    check_delete(T, Jobs).




pt_format(Objects) ->
    pt_format(Objects, []).

pt_format([], Objects) ->
    Objects;
pt_format([{[JobId,  <<"EventJob">>,
	     ProdId, <<"EventProducer">>,
	     EvId,   <<"PmEventM">>,
	     SfId,   <<"SystemFunctions">>,
	     MeId,   <<"ManagedElement">>],
	    {deleted, _}} | T], Objects) ->
    Obj = {binary_to_list(MeId),
	   binary_to_list(SfId),
	   binary_to_list(EvId),
	   binary_to_list(ProdId),
	   binary_to_list(JobId)},
    pt_format(T, [{deleted, Obj} | Objects]);
pt_format([{Obj, {deleted, _}} | T], Objects) ->
    pt_format(T, [{deleted, Obj} | Objects]);
pt_format([{_, Obj} | T], Objects) ->
    pt_format(T, [{created, Obj} | Objects]).
    
