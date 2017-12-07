%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfImmUgInserter.erl %
%%% Author:	erarafo
%%% Description: A process that performs insertion of IMM
%%% instances into the IMM store. Its only (synchronous)
%%% operation is insertInstanceGroups. This operation
%%% is performed by the ImmUgMaster in phase I and by the
%%% applications in phase II.
%%%
%%% Modules used: gmfImmUgScore
%%%
%%% ----------------------------------------------------------
-module(gmfImmUgInserter).
-behaviour(gen_server).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R11A/1').
-date('2017-10-17').
-author('etxpeno').
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
%%% R2A/5      2013-06-05 erarafo     Handling of fatal errors
%%% R2A/10     2013-06-11 etxjotj     Log messages, robustness
%%% R2A/11     2013-06-14 erarafo     Merged changes, ancestor was R2A/5
%%% R2A/12     2013-06-17 erarafo     Refactoring for code reuse
%%% R2A/13     2013-06-19 erarafo     Omission of '$imm_runtime' attributes
%%% R2A/14     2013-06-20 erarafo     Corrected log entry; refactoring
%%% R2A/15     2013-06-27 erarafo     WriteInstances faults fixed
%%% R2A/16     2013-06-27 erarafo     Error handling improved
%%% R2A/17     2013-06-27 erarafo     Without the dialyzer, where would we be?
%%% R2A/18     2013-06-28 erarafo     Refactoring and cleanup
%%% R2A/19     2013-09-04 erarafo     Version reporting
%%% R2A/20     2013-09-27 erarafo     Adapted to #ldap{} change in safs_imm_db
%%% R2A/21     2013-10-03 erarafo     Comments only
%%% R2A/22     2013-10-25 erarafo     Restoring SAF LOG instances,
%%%                                     dropping runtime instances,
%%%                                     support 3-level schema versioning
%%% R2A/23     2013-10-28 erarafo     getImmClass/1 moved to gmfImmUgLib
%%% R2A/24     2013-10-29 erarafo     Warn against unexpected late detection
%%%                                     of runtime instance
%%% R2A/25     2013-11-12 erarafo     Adapted to changes in SAFs
%%% R2A/26     2013-11-14 erarafo     Refactoring
%%% R2A/27     2014-01-13 erarafo     Use safs_imm_db:get_imm_objects_payload/1
%%% R2A/28     2014-03-06 erarafo     Use RESTART_COLD macro
%%% R2A/29     2014-03-10 erarafo     Refactoring: eliminated oneshotInsert
%%% R2A/30     2014-03-10 erarafo     Asynchronous addInstanceGroups
%%% R2A/31     2014-03-11 erarafo     Asynchronous addInstanceGroups, fixed issue
%%% R2A/32     2014-03-24 erarafo     Select RDN attribute by attribute name
%%% R2A/33     2014-04-17 erarafo     Handle SA_NO_DANGLING
%%% R2A/34     2014-04-19 erarafo     Fault in R2A/33 corrected
%%% R2A/35     2014-04-30 erarafo     safs_imm_om:admin_owner_release() dropped
%%% R2A/36     2014-05-05 erarafo     Revised error handling
%%% R2A/37     2014-05-15 erarafo     Handle failed_operation in ccb_apply()
%%% R2A/38     2014-05-21 erarafo     HS62261: Severity WARNING when user fault
%%% R2A/39     2014-07-31 etxberb     Corrected cause value for AVLI
%%% R2A/40     2014-08-05 erarafo     Partial fix for HS83492
%%% R2A/41     2014-08-06 erarafo     HS83492, cleanup of partial fix
%%% R2A/42     2014-08-06 erarafo     HS83492, complete solution
%%% R2A/43     2014-08-06 erarafo     HS84194, improved logging; no solution yet
%%% R2A/44     2014-08-08 erarafo     HS84194, improved logging yet again
%%% R2A/45     2014-08-14 erarafo     Performance lift related to HS84194
%%% R2A/46     2014-08-18 erarafo     HS84194, chunked insertions
%%% R2A/47     2014-08-18 erarafo     Fix for broken negative test case "a17"
%%% R3A/1      2014-11-06 erarafo     Persistent runtime attributes
%%% R3A/2      2014-11-07 erarafo     Improved logging
%%% R3A/3      2014-11-11 erarafo     Handle writeInstances when pers runtime
%%% R3A/4      2014-11-12 erarafo     Adjusted error handling
%%% R3A/5      2014-11-13 erarafo     Improved logging
%%% R3A/6      2015-03-04 erarafo     Groundwork for bidirectional support
%%% R3A/7      2015-03-05 erarafo     Bidirectional support
%%% R3A/8      2015-03-17 erarafo     Ignoring 'reservedBy' when not config attribute
%%% R3A/9      2015-03-18 erarafo     Finalizing of IMM handles revised
%%% R3A/10     2015-03-31 erarafo     Log entry format improved
%%% R3A/11     2015-03-31 erarafo     Improved logging
%%% R3A/12     2015-04-02 erarafo     Cleanup
%%% R3A/13     2015-04-21 erarafo     Refactoring, groundwork for bidir A->A
%%% R3A/14     2015-04-22 erarafo     Partial solution for HT68612
%%% R3A/15     2015-04-23 erarafo     Full solution for HT68612
%%% R3A/16     2015-04-28 erarafo     HT69896, RT instance creation
%%% R4A/1      2015-04-29 erarafo     Improved logging; adapting to record redefinition
%%% R4A/2      2015-04-30 erarafo     Fault handling adjusted
%%% R4A/3      2015-04-30 erarafo     Logging improved
%%% R4A/4      2015-05-19 erarafo     New insertion algorithm
%%% R4A/5      2015-05-21 erarafo     Eliminated #unifInstGroupMap{}
%%% R4A/6      2015-05-21 erarafo     Refactoring
%%% R4A/7      2015-05-22 erarafo     Bidir, restoring of reservedBy
%%% R4A/8      2015-05-25 erarafo     Logging adjusted
%%% R4A/9      2015-05-26 erarafo     Fixed some bad variable naming
%%% R4A/10     2015-05-26 erarafo     Moved tables into this module
%%% R4A/11     2015-05-26 erarafo     Restructured for fewer messages
%%% R4A/12     2015-05-28 erarafo     Scoreboard process eliminated
%%% R4A/13     2015-06-01 erarafo     Improved fault diagnostics
%%% R4A/14     2015-06-02 erarafo     Cleanup
%%% R4A/15     2015-06-04 erarafo     SAF IMM error string, input validation
%%% R4A/16     2015-06-09 erarafo     Trivial adjustments
%%% R4A/17     2015-06-10 erarafo     Redundant records removed
%%% R4A/18     2015-06-26 erarafo     Chunking re-introduced, meaningless code dropped
%%% R4A/19     2015-06-27 erarafo     Overload checking elaborated
%%% R4A/20     2015-06-28 erarafo     Report processing time
%%% R4A/21     2015-07-04 erarafo     Refactoring
%%% R4A/22     2015-09-16 erarafo     Trace Object Implementer Set actions, TO BE UNDONE
%%% R4A/23     2015-09-17 etxpeno     Add IMM extra attributes in insertChunk/2
%%% R4A/24     2015-10-01 erarafo     In connection with HU22829, reduced logging
%%% R4A/25     2015-12-08 eolaand     HU42699, call ootI to remove duplicate
%%%                                   reservedBy references to OamAccessPoint
%%% R5A/1      2015-10-21 etxpeno     improve time measuring
%%% R6A/1      2016-05-11 erarafo     less logging to SwmInternal
%%% R6A/2      2016-05-13 erarafo     'struct version 2' support: 'write' part
%%% R6A/3      2016-05-16 erarafo     exception handling, refactoring
%%% R6A/4      2016-05-18 erarafo     Using the IMM_STRUCT type indicator
%%% R6A/5      2016-06-13 erarafo     LTTng trace adjusted
%%% R6A/8      2016-06-14 erarafo     Adapted to change of sysEnv:struct_ver/0
%%% R6A/9      2016-06-15 erarafo     WP5493, structs as attributes, conditionally
%%% R6A/11     2016-08-01 eolaand     Upgrade problems. Handle empty InstGroups
%%% R6A/12     2016-07-01 usbesvi     HV12688 do not remove logs when emptying IMM
%%% R6A/13     2016-08-10 erarafo     Empty IGs diagnostics elaborated
%%% R6A/14     2016-08-11 erarafo     Counters for instance categories, for WP5493
%%% R6A/15     2016-09-14 erarafo     Improved logging inspired by HV25338
%%% R6A/16     2016-09-15 etxpeno     Improvement of MIB sync
%%% R6A/17     2016-09-22 etxpeno     Improvement of MIB sync
%%% R7A/1      2016-09-08 etxpeno     Dialyzer fixes
%%% R7A/2      2016-09-14 erarafo     Merge from 17A: improved logging
%%% R7A/3      2016-09-15 etxpeno     Uplift from R6A/16
%%% R7A/4      2016-09-26 etxpeno     Uplift from R6A/17
%%% R7A/5      2016-10-06 erarafo     Adjusted info wrapped with exception
%%% R7A/6      2016-10-10 erarafo     Fix SAA bug present since R6A/15
%%% R7A/7      2016-10-11 erarafo     Fix for runtime-persistent struct attrs
%%% R7A/8      2016-10-13 erarafo     Performance improvement
%%% R8A/1      2016-11-03 erarafo     Further performance improvement
%%% R8A/2      2016-11-06 erarafo     Fix dialyzer fault
%%% R7A/9      2016-11-08 erarafo     Merge from R8A/2
%%% R7A/10     2016-11-08 erarafo     Catch failure in gmfSALib
%%% R8A/3      2016-11-09 erarafo     Merge from R7A/10
%%% R8A/4      2016-11-09 erarafo     HV37260, filtering of SAF LOG instances
%%% R8A/5      2016-11-15 erarafo     Minor refactoring
%%% R8A/6      2016-11-17 erarafo     Fix fault in SAA attribute classification
%%% R8A/7      2016-12-08 erarafo     Fix unforeseen SAA termination case
%%% R8A/8      2016-12-09 erarafo     Forgiving dangling reference, HV48414 WA
%%% R8A/9      2016-12-13 etxpeno     Remove support for 'struct as object'
%%% R8A/10     2016-12-19 erarafo     Refactoring
%%% R8A/11     2017-01-19 erarafo     Remove 'struct as object' support
%%% R11A/1     2017-10-16 etxpeno     Otp 20 fixes

%% BACKLOG ------
%% TODO: Drop structClasses, -Info, -Cache, -UsersCache after Ph II pass 3
%% DONE Use SAF IMM error string feature
%% DONE 3-pass algorithm
%% DONE keep back-refs in an inserter ETS table
%% DONE eliminated the scoreboard module
%% DONE speedup of the Master initialization
%% DONE made getCategory() a local function in this module
%% DONE eliminate #unifInstGroupMap{}
%% DONE pscDepth dropped
%% DONE keep the try/catch in unifInstFromMim
%% DONE 'class is handled' message, based on an incremental analysis of xBuffer
%% DONE a type spec for config|persistentRuntime
%% DONE support source() everywhere
%% DONE scoreboard:setHandled() should not be called when EXP == 2
%% DONE infinite timeout in 'call' everywhere
%% DONE eliminate ictiSync
%% DONE try modes 0 and 2
%% DONE undone chunking; no sign of performance penalty seen for 500 instances


-include("gmf.hrl").
-include("gmfImmUg.hrl").

-define(CHUNK_SIZE, 20).

-define(SERVER, ?MODULE).

-define(MODULE_B, <<"gmfImmUgInserter">>).

-define(IMPLEMENTER_DEFAULT, <<"rbs_cs_upgrade">>).

-define(SV, structVersion).


-define(INSERTIONS, insertions).           % Counter names
-define(SIMPLE_INST, simpleInst).
-define(STRUCT_OWNER_INST, structOwnerInst).
-define(STRUCT_INST, structInst).

-define(TIMING, timing).                                       % 'ets' table name
-define(MICROS(),
	erlang:monotonic_time(microsecond)).
-define(TIMING_UPDATE(KEY, BEGIN),
	ets:update_counter(?TIMING, KEY, ?MICROS() - BEGIN)).
-define(TIMING_INCR(KEY, INCR),
	ets:update_counter(?TIMING, KEY, INCR)).

-define(TIMING_REP_INSERT, timingRepInsert).                   % ph II pass 1 work
-define(TIMING_CCB, timingCcb).                                % chunked insertions
-define(TIMING_DROP_LOG, timingDropLog).                       % rejecting special instances
-define(TIMING_XATTRS, timingXattrs).                          % extra attributes creation

-define(TIMING_IMM_OM, timingImmOm).                           % IMM OM calls
-define(TIMING_IMM_OI, timingImmOi).                           % IMM OI calls
-define(TIMING_MNESIA, timingMnesia).                          % mnesia log dumping
-define(TIMING_FORMAT, timingFormat).                          % formatting of instances


-export([start/1,
	 stop/0,
	 fault/0,
	 addInstGroups/2,
	 getNotReceived/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).


-type oiHandle()      :: any().


-record(unifInst,
	{className              :: className(),
	 parentName             :: parentName(),
	 attrs                  :: [#safsImmAttrValues_2{}],
	 implementer=undefined  :: implementer(),
	 extraAttrs             :: [#safsImmAttrValues_2{}] | 'undefined'
	}).

-record(unifInstGroup,
	{className            :: className(),
	 unifInsts            :: [#unifInst{}],
	 maxLength=0          :: integer(),
	 category=config      :: immMode()
	}).


-record(xTree,
	{dict=dict:store(undefined, none, dict:new())  :: dict:dict()   % map of DN to #xui{}
	}).

% Extended unified instance:
% isServer means that the instance is of a class that has the
% proper 'reservedBy' attribute
-record(xui,
	{ui              :: #unifInst{},
	 rdn             :: binary(),           % the RDN, in format <<"abcId=23">>

	 dn              :: binary(),           % the full IMM DN

	 bidirRefs       :: orddict:orddict(binary(), [binary()]),  % map from
                                                % binarized attribute name to
                                                % list of no-dangle DNs

	 isServer=false  :: boolean(),
	 category=config :: immMode()
	}).

% A map from RDN to XUI
-record(xSiblings,
	{dict=orddict:new()  :: orddict:orddict(binary(), #xui{})
	}).

-record(xBuffer,
	{dict=orddict:new()  :: orddict:orddict(parentName(), #xSiblings{})
	}).

% "Reapply-state", for use with the reapply higher-order function.
-record(xrs,
	{sumInserts=0    :: non_neg_integer(),
	 tree            :: #xTree{},
	 buffer          :: #xBuffer{}
	}).

-record(classInfo,
       {className= <<"">>         :: className(),
        category=config           :: immMode(),
	received=false            :: boolean(),
	reservable=undefined      :: undefined|true|false
       }).

-record(structInfo,
	{structsReferred          :: ordsets:ordset(binary()),
	 structAttributes         :: orddict:orddict(binary(), binary())  % maps
                                       % attribute name to IMM struct class name
	}).


%%% ----------------------------------------------------------
%%% @doc Starts the gmfImmUgInserter process.
%%% @end
%%% ----------------------------------------------------------
-spec start([#classAndFile{}]) -> ok.

start(NewClassAndFileRecords) ->
    ?INFO("version ~p starting", [gmfImmUgLib:getVersion(?MODULE)]),

    % create the ETS table
    ClassInfo = ets:new(classInfo, [{keypos, #classInfo.className}]),

    % populate it
    lists:foreach(
      fun(#classAndFile{class=C, category=Cat}) ->
	      Record = #classInfo{className=C, category=Cat},
	      case ets:insert_new(ClassInfo, Record) of
		  false ->
		      ?FAULT([restart], "uniqueness violated", [Record]);
		  _ ->
		      ok
	      end
      end,
      NewClassAndFileRecords),

    % and hand it over
    {ok, ServerPid} = gen_server:start({local, ?SERVER}, ?MODULE, {}, []),
    ets:give_away(ClassInfo, ServerPid, []),
    ok.


%%% ----------------------------------------------------------
%%% @doc Stops the gmfImmUgInserter process.
%%% @end
%%% ----------------------------------------------------------
-spec stop() -> ok.

stop() ->
    gen_server:cast(?SERVER, stop).


%%% ----------------------------------------------------------
%%% @doc Simulate a fault. Can be called from an Erlang shell
%%% in the To-environment to invoke an immediate fallback.
%%% @end
%%% ----------------------------------------------------------
-spec fault() -> ok.

fault() ->
    ?FAULT([restart], "~w:fault/0 called", [?MODULE]).


%%% ----------------------------------------------------------
%%% @doc Adds instance groups for insertion into the To-UP IMM store.
%%% This function returns promptly; errors are handled asynchronously
%%% and there is no easy way to match an error to a particular call.
%%% @end
%%% ----------------------------------------------------------

-spec addInstGroups(source(), [#instanceGroup{}|#immInstanceGroup{}]) ->
	  ok | {error, sa_ais_error_invalid_param}.

addInstGroups(Source, InstGroups) ->

    if
	Source =:= auto ->
	    Names =
		lists:sort(
		  lists:map(
		    fun(#instanceGroup{className=C}) ->
			    C;
		       (#immInstanceGroup{className=C}) ->
			    C
		    end,
		    InstGroups)),
	    ?INFO("auto-insert: ~p", [Names]);
	true ->
	    ok
    end,

    try
	% Put the given instance groups in an orddict with
	% DN length as key.

	AigBegin = ?MICROS(),

	{UigsMap, MaxMaxL, LookupTotal} =
	    lists:foldl(
	      fun(InstGroup, {OrdDict, AccMaxL, AccLookup}) ->
		      {#unifInstGroup{maxLength=L}=Uig, Lookup} =
			  unifInstGroup(Source, InstGroup),
		      NewDict =
			  case orddict:find(L, OrdDict) of
			      error ->
				  orddict:store(L, [Uig], OrdDict);
			      {ok, List} ->
				  orddict:store(L, [Uig|List], OrdDict)
			  end,
		      NewMaxL = max(L, AccMaxL),
		      {NewDict, NewMaxL, AccLookup+Lookup}
	      end,
	      {orddict:new(), 0, 0},
	      InstGroups),
	Ref = make_ref(),
	?INFO("(~w) ~w addInstGroups, max maxlength: ~w", [Source, Ref, MaxMaxL]),

	% Log some metrics.
	orddict:fold(
	      fun(MaxL, Uigs, _) ->
		      UigDescrs =
			  [list_to_binary(
			     lists:flatten(
			       io_lib:format(
				 "~s:~w",
				 [binary_to_list(CN), length(II)])))
			     || #unifInstGroup{className=CN, unifInsts=II} <- Uigs],
		      ?INFO("DN maxlength: ~w, classes and number of instances -~n~160p",
			    [MaxL, gmfImmUgLib:binariesToStrings(UigDescrs, ?MAX_STRLEN, [])])
	      end,
	      ignore,
	      UigsMap),

	% Submit the instance groups for asynchronous insertion.
	UigsFlat =
	    lists:append(
	      lists:reverse(
		orddict:fold(
		  fun(_L, List, Acc) ->
			  [List|Acc]
		  end,
		  [],
		  UigsMap))),

	AigEnd = ?MICROS(),
	AigFormat = AigEnd - AigBegin - LookupTotal,

	gen_server:cast(?SERVER, {addInstGroupsUnresolved, Source, Ref, UigsFlat, AigFormat})
    catch
	% Handle cases where the application (or CS itself) has
	% made a bad request and this is detected before handover
	% to the inserter.
	throw:#exception{}=Ex ->
	    #exception{text=Text,
		       data=Data,
		       errorStrings=ErrorStrings,
		       opts=Opts} = gmfImmUgLib:exception(Ex),
	    ?FAULT([{source, Source}]++Opts, "~s, ~p, ~p", [Text, Data, ErrorStrings]),
	    {error, ?SA_ERR_INVALID_PARAM};
	ExType:ExData ->
	    ?FAULT([{source, Source}, stack, restart],
		   "unspecific problem: ~p, ~p",
		   [ExType, ExData]),
	    % this return value is just a guess really
	    {error, ?SA_ERR_INVALID_PARAM}
    end.


-record(state,
	{% Ongoing conversion pass
	 pass=1                          :: 1|99,

	 % accumulated IMM classes received in 'copy' or 'write' requests
	 receivedClasses=sets:new()      :: sets:set(),

	 % MO tree instances so far
	 tree=#xTree{}                   :: #xTree{},

	 % buffered instances
	 buffer=#xBuffer{}               :: #xBuffer{},

	 % classes not yet fully inserted
	 notYetInserted=ordsets:new()    :: ordsets:ordset(className()),

	 % class info
	 classInfo=none                  :: none|ets:tid(),

	 % back-references
	 backRefs=ets:new(backRefs, [])  :: undefined|ets:tid(),

	 % forward references
	 fwdRefs=ets:new(fwdRefs, [])    :: undefined|ets:tid(),

	 % counters
	 counters=ets:new(counters, [])  :: ets:tid(),

	 % set of className(), used for IMM struct instances, readonly
	 structClasses=sets:new()        :: sets:set(),

	 % IMM classes that have struct attributes, readonly
	 structInfo=dict:new()           :: dict:dict(),

	 % maps binarized IMM classname to a map from IMM DN to #unifInst{}
	 structCache=dict:new()          :: dict:dict(),

	 %
	 structUsersCache=dict:new()     :: dict:dict()
	}).


-spec getNotReceived() ->  ordsets:ordset(className()).

getNotReceived() ->
    ?CALL(?SERVER, {getNotReceived}).


-spec getCategory(source(), className()) -> immMode().

getCategory(Source, ClassName) ->
    case
    ?CALL(?SERVER, {getCategory, ClassName}) of
	BadResult when BadResult =:= no_table orelse
			   BadResult =:= no_hit ->
	    throw(#exception{key=cat_unknown,
			     data={BadResult, Source, ClassName}});
	Result ->
	    Result
    end.


%%% ----------------------------------------------------------
%%% @doc Initializes the gmfImmUgInserter process.
%%% @end
%%% ----------------------------------------------------------
-spec init(any()) -> {ok, #state{}}.

init(_) ->

    mnesia:subscribe(system),
    State = #state{},
    Counters = State#state.counters,
    ets:insert_new(Counters, {?INSERTIONS, 0}),

    ets:insert_new(Counters, {?SIMPLE_INST, 0}),
    ets:insert_new(Counters, {?STRUCT_OWNER_INST, 0}),
    ets:insert_new(Counters, {?STRUCT_INST, 0}),

    ets:new(?TIMING, [named_table]),
    lists:foreach(
      fun(Key) ->
	      ets:insert_new(?TIMING, {Key, 0})
      end,
      [?TIMING_REP_INSERT,
       ?TIMING_CCB,
       ?TIMING_DROP_LOG,
       ?TIMING_XATTRS,
       ?TIMING_FORMAT,
       ?TIMING_IMM_OM,
       ?TIMING_IMM_OI,
       ?TIMING_MNESIA]),

    ?INFO("phase II pass 1 starts", []),

    try
	%% TODO, is this code critical for startup time?
	MimClassKeys = mnesia:dirty_all_keys(gmfMimClass),

	%% build a map {MomNameS, ClassNameS} -> atomic IMM classname
	ClassnameDict =
	    lists:foldl(
	      fun({_MomNameS, ClassNameS}=Key, Acc) ->
		      [#gmfMimClass{imm_ns=ImmNs}] =
			  mnesia:dirty_read(gmfMimClass, Key),
		      dict:store(Key, list_to_atom(ImmNs++ClassNameS), Acc)
	      end,
	      dict:new(),
	      MimClassKeys),

	%% build a set of IMM struct class names
	%% also build a map from MOM name plus ECIM struct name to IMM struct name
	{StructClasses, EcimToImm} =
	    lists:foldl(
	      fun({MimName, EcimStructClassName}=Key, {A, B}) ->
		      [#gmfMimStruct{imm_ns=ImmNs}] =
			  mnesia:dirty_read(gmfMimStruct, Key),
		      ImmStructClassNameB = list_to_binary(ImmNs++EcimStructClassName),
		      {sets:add_element(ImmStructClassNameB, A),
		       dict:store({MimName, EcimStructClassName}, ImmStructClassNameB, B)}
	      end,
	      {State#state.structClasses, dict:new()},
	      mnesia:dirty_all_keys(gmfMimStruct)),

	%% ?INFO(",,, have StructClasses: ~p", [sets:to_list(StructClasses)]),
	%% ?INFO(",,, have EcimToImm: ~p", [dict:to_list(EcimToImm)]),

	%% build the StructInfo map
	StructInfo =
	    lists:foldl(
	      fun({_MomNameS, ClassNameS}=Key, Acc1) ->
		      [#gmfMimClass{attributes = Attributes,
				    imm_ns     = ImmNs}] = mnesia:dirty_read(gmfMimClass, Key),

		      StructAttributes =
			  lists:foldl(
			    fun({AttrNameS, Props}, Acc2) ->

				    %% ?INFO(",,, model info -~n~p", [{ClassNameS, AttrNameS, Props}]),

				    %% for a root class the id attribute name is momname-prefixed
				    %% but never mind, we just recognize structref attributes
				    AttrNameB = list_to_binary(AttrNameS),

				    case trueRuntime(Props, AttrNameS, Key, ClassnameDict) of
					true ->
					    Acc2;
					false ->
					    case proplists:get_value(dataType, Props, undefined) of
						{structRef, StructClassNameS, StructClassMomNameS} ->
						    initHelper(AttrNameB,
							       StructClassNameS,
							       StructClassMomNameS,
							       EcimToImm,
							       Acc2);
						{sequence, {structRef, StructClassNameS, StructClassMomNameS}} ->
						    initHelper(AttrNameB,
							       StructClassNameS,
							       StructClassMomNameS,
							       EcimToImm,
							       Acc2);
						_Other ->
						    Acc2
					    end
				    end
			    end,
			    orddict:new(),
			    Attributes),
		      case orddict:is_empty(StructAttributes) of
			  true ->
			      Acc1;
			  false ->
			      StructsReferred =
				  orddict:fold(
				    fun(_, StructClassB, Acc2) ->
					    ordsets:add_element(StructClassB, Acc2)
				    end,
				    ordsets:new(),
				    StructAttributes),
			      ClassNameB = list_to_binary(ImmNs++ClassNameS),
			      dict:store(
				ClassNameB,
				#structInfo{structsReferred=StructsReferred,
					    structAttributes=StructAttributes},
				Acc1)
		      end
	      end,
	      State#state.structInfo,
	      MimClassKeys),

	%% This is how a gmfMimClass entry may look:
	%% The structrefs refer to ECIM struct names of course.
	%% To resolve a structref, use the gmfMimStruct table.
	%%     {gmfMimClass,
	%%               {"TESTMOM","TestClassE","CXP9021691_2","R6A46"},
	%%               false,
	%%               [{"TestRoot","TESTMOM"}],
	%%               [{"testClassEId", [{dataType,{string,undefined,undefined}}, key,restricted,noNotification,mandatory]},
	%%                {"intA",         [{dataType,{int32,undefined,undefined}}, isNillable,isNillable]},
	%%                {"intB",         [{dataType,{int32,undefined,undefined}}, isNillable,isNillable]},
	%%                {"intC",[{dataType,{int32,undefined,undefined}},mandatory]},
	%%                {"intD",[{dataType,{sequence,{int32,undefined,undefined}}}]},
	%%                {"strE", [{dataType,{string,undefined,undefined}}, isNillable,isNillable]},
	%%                {"structF", [{dataType,{structRef,"StructS","TESTMOM"}}, isNillable,isNillable]},
	%%                {"structG", [{dataType,{sequence,{structRef,"StructP","TESTMOM"}}}]},
	%%                {"enumK", [{dataType,{enumRef,"TestEnumSparse","TESTMOM"}}, isNillable,isNillable]},
	%%                {"enumSeqL", [{dataType,{sequence,{enumRef,"TestEnumSparse","TESTMOM"}}}]}
	%%               ],
	%%               [],
	%%               "TESTMOM",
	%%               "testClassEId"
	%%     }

	{ok, State#state{structInfo=StructInfo, structClasses=StructClasses}}
    catch
	ExType:ExData ->
	    ?FAULT(
	       [stack, restart],
	       "failure in gmfImmUgInserter:init/1, ~p",
	       [{ExType, ExData}])
    end.


%%% ----------------------------------------------------------
%%% @doc Returns true if the attribute being inspected is
%%% runtime and NOT persistent.
%%% @end
%%% ----------------------------------------------------------
-spec trueRuntime(list(), string(), {string(), string()}, dict:dict()) -> boolean().

trueRuntime(Props, AttrNameS, Key, ClassnameDict) ->
    case proplists:get_bool(readOnly, Props) of
	false ->
	    % ECIM modelling is not readOnly so cannot be runtime
	    false;
	true ->
	    % a check of the IMM class definition is needed here
	    ImmClassNameA = dict:fetch(Key, ClassnameDict),
	    [#imm_class{attrs=Attrs}] = mnesia:dirty_read(imm_class, ImmClassNameA),
	    AttrNameA = list_to_atom(AttrNameS),
	    #imm_attr{category=Category, flags=Flags} =
		lists:keyfind(AttrNameA, #imm_attr.name, Attrs),
	    if
		Category =:= config ->
		    % a config attribute according to IMM class definition
		    false;
		true ->
		    % finally if not flagged persistent then truly runtime
		    not(lists:member(persistent, Flags))
	    end
    end.


initHelper(AttrNameB,
	   StructClassNameS,
	   StructClassMomNameS,
	   EcimToImm,
	   Acc2) ->
    ImmStructClass = dict:fetch({StructClassMomNameS, StructClassNameS}, EcimToImm),
    orddict:store(AttrNameB, ImmStructClass, Acc2).


handle_call({getCategory, _ClassName},
	     _From,
	    #state{classInfo=none}=State) ->
     % totally unlikely
    {reply, no_table, State};

handle_call({getCategory, ClassName},
	    _From,
	    #state{classInfo=ClassInfo}=State) ->
    Result =
	case ets:lookup(ClassInfo, ClassName) of
	    [#classInfo{category=Category}] ->
		Category;
	    _ ->
		no_hit
	end,
    {reply, Result, State};


handle_call({getNotReceived}, _From, #state{classInfo=none}=State) ->
    ?WARNING("class info not available", []), % totally unlikely
    {reply, ordsets:new(), State};

handle_call({getNotReceived}, _From, #state{classInfo=ClassInfo}=State) ->
    ResultSet =
	ets:foldl(
	  fun(#classInfo{className=ClassName, received=false}, Acc) ->
		  ordsets:add_element(ClassName, Acc);
	     (_, Acc) ->
		  Acc
	  end,
	  ordsets:new(),
	  ClassInfo),
    {reply, ResultSet, State};

handle_call(Request, From, State) ->
    ?WARNING("UNEXPECTED CALL: ~p, from: ~p", [Request, From]),
    {reply, ok, State}.


handle_cast({addInstGroupsUnresolved, Source, Ref, [], _AigFormat},
	    #state{pass=99}=State) ->
    ?INFO(
        "trying to add zero IGs after ph II pass 3 end, ignored, "
        "source: ~w, ~w",
        [Source, Ref]),
    {noreply, State};

handle_cast({addInstGroupsUnresolved, Source, Ref, [], _AigFormat}, State) ->
    ?INFO("trying to add zero IGs, ignored, source: ~w, ~w", [Source, Ref]),
    {noreply, State};

handle_cast({addInstGroupsUnresolved, Source, Ref, InstGroups, AigFormat},
	    #state{receivedClasses=ReceivedClasses,
		   classInfo=ClassInfo,
		   structClasses=StructClassesByModel,
		   structInfo=StructInfo,
		   counters=Counters}=State) ->
    {UIGsNotUsingStruct, NewReceivedClasses, HandledStructClasses} =
	lists:foldl(
	  fun(#unifInstGroup{className=ClassName, unifInsts=UIs}=UIG, {A, B, S}) ->
		  case hasStruct(UIG, StructInfo) of
		      true ->
			  % instance group uses structs
			  % ?INFO(",,, structuser: ~p", [ClassName]),
			  gen_server:cast(?SERVER, {addStructUserInstGroup, UIG, Source, Ref}),
			  {A, B, S};
		      _ ->
			  case isStruct(UIG, StructClassesByModel) of
			      true ->
				  % instance group is struct instances
				  % ?INFO(",,, struct: ~p", [ClassName]),
				  gen_server:cast(?SERVER, {addStructInstGroup, UIG, Source, Ref}),
				  ets:update_element(ClassInfo, ClassName, {#classInfo.received, true}),
				  ets:update_counter(Counters, ?STRUCT_INST, length(UIs)),
				  ?INFO("(~w) ~w mark class as handled: ~p (struct)", [Source, Ref, ClassName]),
				  {A, sets:add_element(ClassName, B), [ClassName|S]};
			      _ ->
				  % instance group does not use structs
				  ets:update_counter(Counters, ?SIMPLE_INST, length(UIs)),
				  {[UIG|A], B, S}
			  end
		  end
	  end,
	  {[], ReceivedClasses, []},
	  InstGroups),
    NeedCast =
	if
	    UIGsNotUsingStruct =/= [] ->
		true;
	    HandledStructClasses =/= [] ->
		AllReceived = classesAllReceived(ClassInfo),
		if
		    AllReceived ->
			?INFO(
			"all classes received; final ones: ~p",
			[lists:reverse(HandledStructClasses)]),
			true;
		    true ->
			false
		end;
	    true ->
		false
	end,
    if
	NeedCast ->
	    gen_server:cast(
	      ?SERVER,
	      {addInstGroups,
	       Source,
	       Ref,
	       lists:reverse(UIGsNotUsingStruct),
	       AigFormat});
	true ->
	    ok
    end,
    {noreply, State#state{receivedClasses=NewReceivedClasses}};

handle_cast({addInstGroups, Source, Ref, InstGroups, _AigFormat},
	    #state{pass=99}=State) ->
    ?INFO("(~w) ~w after ph II pass 3, ignoring cast; "
          "n. of inst groups: ~w",
	  [Source, Ref,
	   length(InstGroups)]),
    {noreply, State};

handle_cast({addInstGroups, Source, Ref, InstGroups, AigFormat},
	    #state{receivedClasses=ReceivedClasses,
		   tree=Tree,
		   buffer=Buffer,
		   notYetInserted=NotYetInserted,
		   classInfo=ClassInfo,
		   backRefs=BackRefs,
		   fwdRefs=FwdRefs,
		   counters=Counters}=State) ->

    ?TIMING_INCR(?TIMING_FORMAT, AigFormat),

    % Handle another list of instance groups. The list of instance groups MAY
    % be empty, e g if the last data conversion event is the arrival of a
    % struct instance group that does not cause any instance group to be
    % resolved.
    ?INFO("(~w) ~w received more instance groups: ~w", [Source, Ref, length(InstGroups)]),
    % ?INFO("(~w) ~w classes -~n~p", [Source, Ref, InstGroups]), -- bulky logging!
    try
	% Scan the given groups, building two sets: all classes received
	% so far, and classes received in this invocation.
	{NewReceivedClasses, ReceivedNow} =
	    lists:foldl(
	      fun(#unifInstGroup{className=ClassName}=UIG, {S1, S2}) ->
		      case sets:is_element(ClassName, S1) of
			  true ->
			      throw(#exception{key=inst_grp_handled,
					       data={ClassName, UIG}});
			  false ->
			      {sets:add_element(ClassName, S1), ordsets:add_element(ClassName, S2)}
		      end
	      end,
	      {ReceivedClasses, ordsets:new()},
	      InstGroups),

	?INFO("(~w) ~w classes received by now in total: ~w", [Source, Ref, sets:size(NewReceivedClasses)]),

	% Add new instances to the buffer.
	InflatedBuffer =
	    lists:foldl(
	      fun(#unifInstGroup{unifInsts=Insts, category=Cat}, AccBuf1) ->
		      % extend the buffer with all instances in the group
		      lists:foldl(
			fun(#xui{ui=#unifInst{parentName=PN}}=XUI, AccBuf2) ->
				Siblings =
				    case bufferIsKey(PN, AccBuf2) of
					false ->
					    siblingsFromXui(XUI);
					true ->
					    addSibling(XUI, bufferGet(PN, AccBuf2), PN)
				    end,
				bufferUpdate(PN, Siblings, AccBuf2)
			end,
			AccBuf1,
			[createXui(Inst, Cat, ClassInfo) || Inst <- Insts])
	      end,
	      Buffer,
	      InstGroups),

	?INFO("(~w) ~w buffer inflated, size: ~w", [Source, Ref, bufferSize(InflatedBuffer)]),

	% repeatedly do insertions

	RepInsertBegin = ?MICROS(),
	#xrs{tree=NewTree, buffer=DeflatedBuffer, sumInserts=SumInserts} =
	    reapply(
	      fun(#xrs{tree=T1, buffer=B1, sumInserts=S1}) ->
		      Parents = dictIntersect(B1, T1),
		      % insert in tree, delete from buffer
		      {NofInserts, NewT, NewB} =
			  lists:foldl(
			    fun(PN, {N2, T2, B2}) ->
				    Siblings = bufferGet(PN, B2),
				    NewT2 = treeAddInsts(Source, Siblings, T2, BackRefs, FwdRefs, Counters),
				    NewB2 = bufferDelete(PN, B2),
				    NewInserts = N2 + nofSiblings(Siblings),
				    {NewInserts, NewT2, NewB2}
			    end,
			    {0, T1, B1},
			    Parents),
		      {NofInserts > 0, #xrs{tree=NewT, buffer=NewB, sumInserts=S1 + NofInserts}}
	      end,
	     #xrs{tree=Tree, buffer=InflatedBuffer}),
	?TIMING_UPDATE(?TIMING_REP_INSERT, RepInsertBegin),

	?INFO("(~w) ~w additional insertions made: ~w", [Source, Ref, SumInserts]),

	ReceivedNowOrNotYetInserted = ordsets:union(NotYetInserted, ReceivedNow),
	ClassesInDeflatedBuffer = classesInBuffer(DeflatedBuffer),
	Inserted = ordsets:subtract(ReceivedNowOrNotYetInserted, ClassesInDeflatedBuffer),

	ordsets:fold(
	  fun(ClassName, _) ->
		  ?INFO("(~w) ~w mark class as handled: ~p", [Source, Ref, ClassName])
	  end,
	  ok,
	  Inserted),

	NewState =
	    State#state{receivedClasses=NewReceivedClasses,
			tree=NewTree,
			buffer=DeflatedBuffer,
			notYetInserted=ClassesInDeflatedBuffer},

	NewBufferSize = bufferSize(DeflatedBuffer),
	?INFO("(~w) ~w buffer deflated, size: ~w", [Source, Ref, NewBufferSize]),

	reportClassesReceived(ReceivedNow, ClassInfo),

	ClassesNotReceived = classesNotReceived(ClassInfo),

	if
	    ClassesNotReceived =/= [] ->
		% not finished yet, just keep going
		gmfImmUgMaster:notify(Source, false),
		?INFO("(~w) ~w classes not yet received -~n~160p",
		      [Source,
		       Ref,
		       gmfImmUgLib:binariesToStrings(
			 ClassesNotReceived,
			 ?MAX_STRLEN,
			 [])]),
		{noreply, NewState};
	    NewBufferSize > 0 ->
		% no undelivered classes, yet some instances not inserted
		Stack = element(2, process_info(self(), current_stacktrace)),
		gmfImmUgLib:handleFault([user, {source, Source}],
					?MODULE,
					?LINE,
					"~w could not insert instances -~n~p",
					[Ref, DeflatedBuffer],
					Stack),
		throw(#exception{key=cannot_insert, data={Ref, NewBufferSize}});
	    true ->
		% the tree is fully populated, pass 2 of the insertion
		% algorithm follows
		gmfImmUgLib:progress(
		  ?MODULE,
		  ?LINE,
		  "upgrade: MO conversion phase II pass 2 starts"),
		{BadRefs, _} =
		    ets:foldl(
		      fun({Client, BidirRefs}, {Acc, Count}) ->
			      NewCount =
				  if
				      Count =:= ?CHUNK_SIZE ->
					  overloadCheck(Source),
					  0;
				      true ->
					  Count + 1
				  end,
			      NewFwdRefs =
				  insertFwdRefs(Client, BidirRefs, BackRefs, NewTree, Acc),
			      {NewFwdRefs, NewCount}
		      end,
		      {[], 0},
		      FwdRefs),
		ets:delete(FwdRefs),

		if
		    BadRefs =/= [] ->
			lists:foreach(
			  fun({badFwdRef, Client, AttrName, Target}) ->
				  ?WARNING("unresolved MO ref: ~p:~p -> ~p",
					   [Client, AttrName, Target])
			  end,
			  BadRefs),
			  throw(#exception{key=dangling, data={}});
		    true ->
			ok
		end,

		gmfImmUgLib:progress(
		  ?MODULE,
		  ?LINE,
		  "upgrade: MO conversion phase II pass 3 starts"),
		ets:foldl(
		  fun({Server, Clients}, Count) ->
			  NewCount =
			      if
				  Count =:= ?CHUNK_SIZE ->
				      overloadCheck(Source),
				      0;
				  true ->
				      Count + 1
			      end,
			  case dict:find(Server, NewTree#xTree.dict) of
			      error ->
				  % impossible!
				  throw(#exception{key=refs_incons,
						   data={Server, Clients}});
			      _ ->
				  addReservations(auto, Server, Clients)
			  end,
			  NewCount
		  end,
		  0,
		  BackRefs),
		ets:delete(BackRefs),
		gmfImmUgLib:progress(
		  ?MODULE,
		  ?LINE,
		  "upgrade: MO conversion phase II pass 3 ends"),
		gmfImmUgMaster:notify(Source, true),
		reportCounters(Counters),
		reportTiming(),
		{noreply, NewState#state{backRefs=undefined,
					 fwdRefs=undefined,
					 pass=99}}
	end
    catch
	throw:(#exception{}=Ex) ->
	    #exception{text=Text,
		       data=Data,
		       errorStrings=ErrorStrings,
		       opts=Opts} = gmfImmUgLib:exception(Ex),
	    ?FAULT([{source, Source}]++Opts, "~s, ~p, ~p", [Text, Data, ErrorStrings]),
	    {noreply, State};
	ExType:ExData ->
	    ?FAULT([{source, Source}, stack, restart],
		   "unspecific exception: ~p, ~p",
		   [ExType, ExData]),
	    {noreply, State}
    end;

handle_cast({addStructInstGroup, #unifInstGroup{className=ClassName}=UIG, Source, Ref},
	    #state{structCache=StructCache,
		   structUsersCache=StructUsersCache,
		   structInfo=StructInfo,
		   counters=Counters,
		   classInfo=ClassInfo}=State) ->
    % TODO, the struct cache is never decreased. Perhaps this is OK; the
    % space will be freed when data conversion completes at least. Consider
    % terminating the inserter as early as possible, OR clean up the struct
    % cache incrementally!
    try
	case dict:is_key(ClassName, StructCache) of
	    true ->
		throw(#exception{key=struct_handled, data={ClassName}});
	    _ ->
		ok
	end,

	UIDict = buildUIDict(UIG),
	% ?INFO(",,, extending struct cache with: ~p", [ClassName]),
	NewStructCache = dict:store(ClassName, UIDict, StructCache),

	% resolve all structuser IGs against extended cache
	ResolvedClasses =
	    dict:fold(
	      fun(StructUserClassName, SUUIG, Acc) ->
		      case resolveStructUsers(SUUIG, NewStructCache, StructInfo, Source, Ref, Counters, ClassInfo) of
			  false ->
			      % ?INFO(",,, not yet resolvable: ~p", [StructUserClassName]),
			      Acc;
			  true ->
			      % ?INFO(",,, resolved now: ~p, will be removed from cache", [StructUserClassName]),
			      [StructUserClassName|Acc]
		      end
	      end,
	      [],
	      StructUsersCache),

	% remove resolved classes from cache
	NewStructUsersCache =
	    lists:foldl(
	      fun(ResolvedClass, Acc) ->
		      dict:erase(ResolvedClass, Acc)
	      end,
	      StructUsersCache,
	      ResolvedClasses),

	{noreply, State#state{structCache=NewStructCache,
			      structUsersCache=NewStructUsersCache}}
    catch
	throw:(#exception{}=Ex) ->
	    #exception{text=Text,
		       data=Data,
		       errorStrings=ErrorStrings,
		       opts=Opts} = gmfImmUgLib:exception(Ex),
	    ?FAULT([{source, Source}]++Opts, "~s, ~p, ~p", [Text, Data, ErrorStrings]),
	    {noreply, State};
	ExType:ExData ->
	    ?FAULT([{source, Source}, stack, restart],
		   "unspecific exception: ~p, ~p",
		   [ExType, ExData]),
	    {noreply, State}
    end;


handle_cast({addStructUserInstGroup, #unifInstGroup{className=StructUserClassName}=UIG, Source, Ref},
	    #state{structCache=StructCache,
		   structUsersCache=StructUsersCache,
		   structInfo=StructInfo,
		   counters=Counters,
		   classInfo=ClassInfo}=State) ->
    % try to resolve immediately; if ok then don't cache
    try
	NewStructUsersCache =
	    case resolveStructUsers(UIG, StructCache, StructInfo, Source, Ref, Counters, ClassInfo) of
		false ->
		    % ?INFO(",,, could not resolve immediately: ~p", [StructUserClassName]),
		    dict:store(StructUserClassName, UIG, StructUsersCache);
		true ->
		    % ?INFO(",,, resolved immediately: ~p", [StructUserClassName]),
		    StructUsersCache
	    end,
	{noreply, State#state{structUsersCache=NewStructUsersCache}}
    catch
	throw:(#exception{}=Ex) ->
	    #exception{text=Text,
		       data=Data,
		       errorStrings=ErrorStrings,
		       opts=Opts} = gmfImmUgLib:exception(Ex),
	    ?FAULT([{source, Source}]++Opts, "~s, ~p, ~p", [Text, Data, ErrorStrings]),
	    {noreply, State};
	ExType:ExData ->
	    ?FAULT([{source, Source}, stack, restart],
		   "unspecific exception: ~p, ~p",
		   [ExType, ExData]),
	    {noreply, State}
    end;

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Request, State) ->
    ?WARNING("UNEXPECTED CAST: ~p", [Request]),
    {noreply, State}.


handle_info({'ETS-TRANSFER', ClassInfo, _FromPid, _}, State) ->
    case ets:info(ClassInfo, size) of
	0 ->
	    % Totally unlikely to happen since there are always some
	    % log-related classes that will be handled in data
	    % conversion. It is assumed that there would be no
	    % bidirectional references to handle in this extreme
	    % case.
	    gmfImmUgMaster:notify(auto, true),
	    {noreply, State#state{classInfo=ClassInfo, pass=99}};
	_ ->
	    gmfImmUgMaster:setInserterReady(),
	    {noreply, State#state{classInfo=ClassInfo}}
    end;

handle_info({mnesia_system_event,
	     {mnesia_overload, Reason}}, State) ->
    ?WARNING("mnesia overload: ~p", [Reason]),
    {noreply, State};

handle_info(Info, State) ->
    ?WARNING("UNEXPECTED INFO: ~p", [Info]),
    {noreply, State}.


terminate(Reason, _State) ->
    ?INFO("~w terminating, reason: ~p", [?SERVER, Reason]),
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


-spec hasStruct(#unifInstGroup{}, dict:dict()) ->  boolean().

hasStruct(#unifInstGroup{className=ClassName}, StructInfo) ->
    dict:is_key(ClassName, StructInfo).


-spec isStruct(#unifInstGroup{}, sets:set()) -> boolean().

isStruct(#unifInstGroup{className=ClassName}, StructClasses) ->
    sets:is_element(ClassName, StructClasses).


%%% ----------------------------------------------------------
%%% @doc Tries to resolve an entire instance group, returning
%%% 'true' if successful.
%%% @end
%%% ----------------------------------------------------------
-spec resolveStructUsers(#unifInstGroup{}, dict:dict(), dict:dict(),
			 source(), reference(), ets:tid(), ets:tid()) -> boolean().

resolveStructUsers(#unifInstGroup{className=ClassName,
				  unifInsts=UIs,
				  maxLength=MaxLength,
				  category=Category},
		   StructCache,
		   StructUserInfo,
		   Source,
		   Ref,
		   Counters,
		   ClassInfo) ->
    #structInfo{structsReferred=StructsReferred,
		structAttributes=AttributeInfo} =
	dict:fetch(ClassName, StructUserInfo),

    % check that all needed struct classes are present
    NeededStructClassesPresent =
	ordsets:fold(
	  fun(S, true) ->
		  dict:is_key(S, StructCache);
	     (_, false) ->
		  false
	  end,
	  true,
	  StructsReferred),
    if
	not (NeededStructClassesPresent) ->
	    % ?INFO(",,, not resolved UIG yet: ~p (needs: ~p)", [ClassName, StructsReferred]),
	    false;
	true ->
	    % create a modified UIG
	    % ?INFO(",,, resolve UIG now: ~p (needs: ~p)", [ClassName, StructsReferred]),
	    ResolvedInsts =
		lists:map(
		  fun(#unifInst{parentName=ParentName,
				attrs=Attrs,
				implementer=Impl,
				extraAttrs=XAttrs}=UI) ->
			  ResolvedAttrs =
			      lists:foldr(
				fun(#safsImmAttrValues_2{attrName=AttrName,
							 attrValueType= ?IMM_NAME,
							 attrValuesNumber=AttrMult,
							 attrValues=AttrValues}=AV2, Acc2) ->
					% 'name' attribute but not necessarily struct related
					case orddict:find(AttrName, AttributeInfo) of
					    error ->
						[AV2|Acc2];
					    {_, StructClass} ->
						UIDict = dict:fetch(StructClass, StructCache),
						NewAttrValues =
						    lists:foldr(
						      fun(#safsImmAttrValue{saname=Name}, Acc3) ->
							      case dict:find(Name, UIDict) of
								  error ->
								      case ets:lookup(ClassInfo, StructClass) of
									  [#classInfo{category=persistentRuntime}] ->
									      % be forgiving here; HV48414
									      ?WARNING(
									      "(~w) ~w MO instance refers to non-existent struct instance -~n"
									      "instance: ~p~n"
									      "struct: ~p -~n"
									      "no struct-value encoded for the above reference",
									      [Source, Ref,
									       UI,
									       Name]),
									      Acc3;
									  _ ->
									      % struct is not persistentRuntme
									      ?FAULT([restart],
									      "(~w) ~w MO instance refers to non-existent struct instance -~n"
									      "instance: ~p~n"
									      "struct: ~p -~n",
									      [Source, Ref,
									       UI,
									       Name]),
									      Acc3
								      end;
								  {_, #unifInst{attrs=SAttrs}} ->
								      #unifInst{attrs=SAttrs} = dict:fetch(Name, UIDict),
								      CsStruct = #safsImmCsStruct{structName=StructClass,
												  structMembers=SAttrs},
								      NewAttrValue = #safsImmAttrValue{csstruct=CsStruct},
								      [NewAttrValue|Acc3]
							      end
						      end,
						      [],
						      AttrValues),
						NewAttr =
						    #safsImmAttrValues_2{attrName=AttrName,
									 attrValueType= ?IMM_STRUCT,
									 attrValuesNumber=AttrMult,
									 attrValues=NewAttrValues},
						[NewAttr|Acc2]
					end;
				   (X, Acc2) ->
					[X|Acc2]
				end,
				[],
				Attrs),

			  #unifInst{className=ClassName,
				    parentName=ParentName,
				    attrs=ResolvedAttrs,
				    implementer=Impl,
				    extraAttrs=XAttrs}
		  end,
		  UIs),
	    ResolvedUIG =
		#unifInstGroup{className=ClassName,
			       unifInsts=ResolvedInsts,
			       maxLength=MaxLength,
			       category=Category},

	    %and cast it
	    % ?INFO(",,, resolved UIG: ~p", [ClassName]),
	    ets:update_counter(Counters, ?STRUCT_OWNER_INST, length(ResolvedInsts)),
	    gen_server:cast(?SERVER, {addInstGroups, Source, Ref, [ResolvedUIG], 0}),
	    true
    end.


%%% ----------------------------------------------------------
%%% @doc Converts a unified instance group to a dict with
%%% IMM DNs as key. The dict will contain UIs where the
%%% "id" attribute has been removed.
%%% @end
%%% ----------------------------------------------------------
-spec buildUIDict(#unifInstGroup{}) -> dict:dict().

buildUIDict(#unifInstGroup{unifInsts=UIs}) ->
    lists:foldl(
      fun(#unifInst{parentName=ParentName, attrs=Attrs}=UI, Acc1) ->
	      {ImmRdnList, NonRdnAttrs} =
		  lists:foldr(
		    fun(#safsImmAttrValues_2{attrName= <<"id">>,
					     attrValueType=?IMM_STRING,
					     attrValues=[#safsImmAttrValue{sastring=RdnValue}]},
			{Names, Acc2}) ->
			    % an rdn attribute; only one such is expected
			    Name = list_to_binary("id="++binary_to_list(RdnValue)++","++binary_to_list(ParentName)),
			    {[Name|Names], Acc2};
		       (Attr,
			{Names, Acc2}) ->
			    % a non-rdn attribute
			    {Names, [Attr|Acc2]}
		    end,
		    {[], []},
		    Attrs),
	      case ImmRdnList of
		  [ImmRdn] ->
		      dict:store(ImmRdn, UI#unifInst{attrs=NonRdnAttrs}, Acc1);
		  [] ->
		      throw(#exception{key=struct_has_no_id, data=NonRdnAttrs});
		  _ ->
		      throw(#exception{key=struct_has_multi_id, data={ImmRdnList, NonRdnAttrs}})
	      end
      end,
      dict:new(),
      UIs).


%%% ----------------------------------------------------------
%%% @doc Reports classes received in 'copy' or 'write'
%%% requests. The set of not-yet-received classes is
%%% reduced by the reported classes.
%%% @end
%%% ----------------------------------------------------------
-spec reportClassesReceived(ordsets:ordset(className), ets:tid()) -> any().

reportClassesReceived(ClassesSet, ClassInfo) ->
    ordsets:fold(
      fun(ClassName, _Acc) ->
	      ets:update_element(ClassInfo, ClassName, {#classInfo.received, true})
      end,
      ignore,
      ClassesSet).


%%% ----------------------------------------------------------
%%% @doc Returns true if the table has no received=false
%%% records.
%%% @end
%%% ----------------------------------------------------------
-spec classesAllReceived(ets:tid()) -> boolean().

classesAllReceived(ClassInfo) ->
    try
	ets:foldl(
	  fun(#classInfo{received=false}, _Acc) ->
		  throw(false);
	     (_, Acc) ->
		  Acc
	  end,
	  true,
	  ClassInfo)
    catch
	throw:false ->
	    false
    end.


%%% ----------------------------------------------------------
%%% @doc Returns classes not received as an ordered list of
%%% className().
%%% @end
%%% ----------------------------------------------------------
-spec classesNotReceived(ets:tid()) -> [className()].

classesNotReceived(ClassInfo) ->
    lists:sort(
      ets:foldl(
	fun(#classInfo{received=false, className=ClassName}, Acc) ->
		[ClassName|Acc];
	   (_, Acc) ->
		Acc
	end,
	[],
	ClassInfo)).


-spec classesInBuffer(#xBuffer{}) -> ordsets:ordset(className()).

classesInBuffer(XB) ->
    orddict:fold(
      fun(_Parent, #xSiblings{dict=SS}, Acc) ->
	      orddict:fold(
		fun(_Rdn, #xui{ui=#unifInst{className=X}}, Bcc) ->
			ordsets:add_element(X, Bcc)
		end,
		Acc,
		SS)
      end,
      ordsets:new(),
      XB#xBuffer.dict).


%%% ----------------------------------------------------------
%%% @doc Constructs a unified instance group. The instances are
%%% ordered for insertion into a CCB. The resulting list of
%%% instances may be empty.
%%%
%%% THROWS, TODO
%%% @end
%%% ----------------------------------------------------------
-spec unifInstGroup(source(), #instanceGroup{}|#immInstanceGroup{}) ->
	  {#unifInstGroup{}, non_neg_integer()}.

unifInstGroup(_Source, #instanceGroup{className=ClassName,
				     instances=[],
				     maxLength=MaxLength,
				     implPers=ImplPers}) ->
    {#unifInstGroup{className=ClassName,
		    unifInsts=[],
		    maxLength=MaxLength,
		    category= if ImplPers -> persistentRuntime; true -> config end},
     0};

unifInstGroup(Source, #instanceGroup{className=ClassName,
				     instances=Insts,
				     maxLength=MaxLength,
				     implPers=ImplPers}) ->
    % It is trusted that the ClassName and the classnames encoded in
    % the non-empty instances list agree.
    [#restoredInstance{class=ClassNameA}|_] = Insts,

    T1 = ?MICROS(),
    #imm_class{category=Cat,
	       rdn=#imm_rdn{name=RdnNameA},
	       attrs=AttrDecls}=ImmClass = gmfImmUgLib:getImmClass(ClassNameA),
    T2 = ?MICROS(),

    RdnNameB = list_to_binary(atom_to_list(RdnNameA)),

    AttrNames =
	[{ANameA, list_to_binary(atom_to_list(ANameA))}
	 ||#imm_attr{name=ANameA} <- AttrDecls],

    UnifInsts =
	lists:foldl(
	  fun(Inst, Acc) ->
		  unifInstFromMim(Source,
				  ClassName,
				  Inst,
				  ImplPers,
				  Acc,
				  Cat,
				  ImmClass,
				  RdnNameB,
				  AttrNames)
	  end,
	  [],
	  Insts),
    {#unifInstGroup{className=ClassName,
		    unifInsts=UnifInsts,
		    maxLength=MaxLength,
		    category= if ImplPers -> persistentRuntime; true -> config end},
     T2-T1};

unifInstGroup(Source, #immInstanceGroup{className=ClassName,
					 immInsts=Insts,
					 maxLength=MaxLength,
					 implementer=Implementer}) ->
    Cat = getCategory(Source, ClassName),
    if
	is_binary(Implementer) andalso Cat =/= persistentRuntime ->
	    throw(#exception{key=class_not_pers_rt,
			     data={ClassName, Implementer}});
	true ->
	    UnifInsts = [unifInstFromImm(Inst, ClassName, Implementer)||Inst <- Insts],
	    NewLength = if UnifInsts =:= [] -> 0; true -> MaxLength end,
	    {#unifInstGroup{className=ClassName,
			    unifInsts=UnifInsts,
			    maxLength=NewLength,
			    category=Cat},
	     0}
    end.


%%% ----------------------------------------------------------
%%% @doc Constructs a unified instance from restored instance
%%% data. The second argument is an accumulator.
%%% @end
%%% ----------------------------------------------------------

-spec unifInstFromMim(source(),
		      className(),
		      #restoredInstance{},
		      boolean(),
		      [#unifInst{}],
		      config|runtime,
		      #imm_class{},
		      binary(),
		      [{atom(), binary()}]) ->
	  [#unifInst{}].

unifInstFromMim(Source,
		ClassName,
		#restoredInstance{class=ClassNameA,
				  key=MimDn,
				  attrs=AttrDescrs,
				  implementer=Implementer}=I,
		ImplPers,
		Acc,
		Cat,
		#imm_class{attrs=AttrDecls}=ImmClass,
		RdnNameB,
		AttrNames) ->

    if
	Cat =:= runtime andalso ImplPers =:= false ->
	    ?INFO("late dropping runtime instance: ~p", [I]),
	    Acc;
	true ->
	    % Get the parent name, or 'undefined' if the parent is
	    % ManagedElement.
	    ParentName = gmfImmUgLib:mimDnToImmParentName(MimDn),

	    % Get the RDN value of this instance.
	    MimDnLast = lists:last(MimDn),
	    {EqualsPos, _} = binary:match(MimDnLast, <<"=">>),
	    RdnValueB = binary_part(MimDnLast, EqualsPos+1, size(MimDnLast)-EqualsPos-1),

	    % Convert attributes according to the class definition. Only
	    % config and persistent runtime attributes are included.
	    AttrValues =
		try
		    lists:append(
		      [case getImmAttrCat(AttrNameA, ClassNameA, AttrDecls) of
			   runtime ->
			       [];
			   _ ->
			       % trusted: config or persistentRuntime
			       [gmfImmUgLib:safsImmAttrValues(
				  AttrNameA,
				  Values,
				  ClassNameA,
				  AttrDecls,
				  AttrNames)]
		       end
		       || {AttrNameA, Values} <- AttrDescrs, Values =/= '$imm_runtime'])
		catch
		    throw:Info ->
			?FAULT([{source, Source}, stack],
			       "failed to prepare attribute values:~n"
			       " - class: ~w~n"
			       " - IMM class: ~p~n"
			       " - attrs: ~p~n"
			       " - reason: ~p",
			       [ClassNameA, ImmClass, AttrDescrs, Info]),
			throw(#exception{key=bad_attr_values,
					 data={ClassNameA, ImmClass, AttrDescrs, Info}});
		    Exc:What ->
			?FAULT([{source, Source}, stack],
			       "failed to prepare attribute values:~n"
			       " - class: ~w~n"
			       " - IMM class: ~p~n"
			       " - attrs: ~p~n"
			       " - exception type: ~p~n"
			       " - reason: ~p",
			       [ClassNameA, ImmClass, AttrDescrs, Exc, What]),
			throw(#exception{key=bad_attr_values,
					 data={ClassNameA, ImmClass, AttrDescrs, Exc, What}})
		end,

	    Rdn = #safsImmAttrValues_2{attrName=RdnNameB,
				       attrValueType=?IMM_STRING,
				       attrValuesNumber=1,
				       attrValues=[#safsImmAttrValue{sastring=RdnValueB}]},

	    [#unifInst{className=ClassName,
		       parentName=ParentName,
		       attrs=[Rdn|AttrValues],
		       implementer=Implementer}|Acc]
    end.


%%% ----------------------------------------------------------
%%% @doc Gets the category of an IMM attribute from the list of
%%% attribute descriptions of the class in which it belongs.
%%% @end
%%% ----------------------------------------------------------
-spec getImmAttrCat(atom(), atom(), [#imm_attr{}]) ->
	  config | runtime | persistentRuntime.

getImmAttrCat(ANameA, ClassNameA, AttrDecls) ->
    case lists:keyfind(ANameA, #imm_attr.name, AttrDecls) of
	#imm_attr{category=config} ->
	    config;
	#imm_attr{category=runtime, flags=Flags} ->
	    case lists:member(persistent, Flags) of
		true ->
		    persistentRuntime;
		_ ->
		    runtime
	    end;
	false ->
	    ?FAULT([stack, user],
		   "no description of attribute ~w in class ~w",
		   [ANameA, ClassNameA]),
	    throw(#exception{key=imm_attr_unknown, data={ANameA, ClassNameA}})
    end.


%%% ----------------------------------------------------------
%%% @doc Constructs a unified instance from IMM data.
%%% @end
%%% ----------------------------------------------------------

-spec unifInstFromImm(#safsImmCtInstance{}, className(), implementer()) ->
	  #unifInst{}.

unifInstFromImm(#safsImmCtInstance{parentName=ParentName, attrValues=AttrValues},
		ClassName,
		Implementer) ->
    case isWellFormedParentName(ParentName) of
	false ->
	    throw(#exception{key=bad_parent, data={ParentName, ClassName}});
	_ ->
	    #unifInst{className=ClassName,
		      parentName=ParentName,
		      attrs=AttrValues,
		      implementer=Implementer}
    end.


%%% ----------------------------------------------------------
%%% @doc Returns true if the parent name is well-formed,
%%% false otherwise.
%%% @end
%%% ----------------------------------------------------------
-spec isWellFormedParentName(parentName()) -> boolean().

isWellFormedParentName(undefined) ->
    true;

isWellFormedParentName(<<"">>) ->
    false;

isWellFormedParentName(NonEmptyB) ->
  S = binary_to_list(NonEmptyB),
  TT = string:tokens(S, ","),
  case string:join(TT, ",") of
      U when U =/= S ->
	  % this happens if there are adjacent commas
	  % or empty segments at the beginning or end
	  false;
      _ ->
	  lists:all(
	    fun(T) ->
		    % the checking of RDN name may be sharpened
		    case re:run(T, "[a-zA-Z0-9_]+=[^ ]+$", [anchored]) of
			nomatch ->
			    false;
			_ ->
			    true
		    end
	    end,
	    TT)
  end.


-spec insertRuntimePersistentInst(source(),
		  className(),
		  #unifInst{}) ->
	  ok.

insertRuntimePersistentInst(Source,
	   ClassName,
	   #unifInst{attrs=AttrValues, parentName=ParentName,  implementer=Impl}) ->
    Implementer =
	if
	    Impl =:= undefined ->
		?IMPLEMENTER_DEFAULT;
	    true ->
		Impl
	end,
    Handle = oiInitialize(),
    ok = oiImplementerSet(Handle, Implementer),
    ok = oiRtObjectCreate(Handle, ClassName, ParentName, AttrValues),

    ClassNameA = list_to_atom(binary_to_list(ClassName)),
    #imm_class{rdn=#imm_rdn{name=RdnNameA}} =
	gmfImmUgLib:getImmClass(ClassNameA),
    Dn = getOwnDn(Source, ParentName, RdnNameA, AttrValues),

    ok = oiImplementerClear(Handle),
    ok = oiFinalize(Handle),
    if
	Impl =:= undefined ->
	    ?FAULT([user, {source, Source}],
		   "inserted pers RT instance: ~p, DEFAULT implementer: ~p",
		   [Dn, Implementer]);
	true ->
	    ok
	    % ?INFO("(~w) inserted pers RT instance: ~p, implementer: ~p",
	    %	  [Source, Dn, Implementer])
    end,
    ok.


-spec insertChunk(source(), [#unifInst{}]) ->
	  ok.

insertChunk(Source, BagSIn) ->
    % TODO, should this filtering be done at an earlier stage?
    DropLogBegin = ?MICROS(),
    BagS =
	lists:filter(
	  fun(#unifInst{parentName= <<"safApp=safLogService">>}) ->
		  ?INFO("dropping instance under safApp=safLogService", []),
		  false;
	     (#unifInst{attrs=[Attr1|_]}) ->
		  case Attr1 of
		      #safsImmAttrValues_2{attrValues=[#safsImmAttrValue{sastring=String}|_]} when
			String =:= <<"safLogService">> orelse
			    String =:= <<"safLogServiceCfg">> ->
			  ?INFO("dropping SAF LOG instance of variety: ~p", [String]),
			  false;
		      _ ->
			  true
		  end;
	     (_) ->
		  true
	  end,
	  BagSIn),
    ?TIMING_UPDATE(?TIMING_DROP_LOG, DropLogBegin),

    case BagS of
	[] ->
	    % unlikely, but may happen if BagSIn contained only
	    % log-related instances
	    ok;
	_ ->
	    XattrsBegin = ?MICROS(),
	    NewBagS = insertExtraAttrs(BagS),
	    ?TIMING_UPDATE(?TIMING_XATTRS, XattrsBegin),

	    CcbBegin = ?MICROS(),

	    ImmHandle = getImmHandle(),
	    AoHandle = getAoHandle(undefined, ImmHandle),
	    CcbId = getCcbId(undefined, AoHandle),

	    lists:foreach(
	      fun(#unifInst{parentName=ParentName,
			    className=ClassName,
			    attrs=AttrValues,
			    extraAttrs = ExtraAttrValues}) ->
		      if
			  ParentName =:= undefined ->
			      ok;
			  true ->
			      setAdminOwner(AoHandle, [ParentName])
		      end,

		      case xcall(?TIMING_IMM_OM,
				 safs_imm_om, ccb_object_create_s2,
				 [CcbId,
				  ClassName,
				  ParentName,
				  AttrValues,
				  ExtraAttrValues]) of
			  ok ->
			      ok;
			  {error, OtherReason} ->
			      % this throw is fatal and causes rollback
			      ErrorStrings = safs_imm_om:ccb_get_error_strings(CcbId),
			      throw(#exception{key=ccb_insert,
					       data={OtherReason, Source, ClassName,
						     ParentName, AttrValues, ExtraAttrValues},
					       errorStrings=ErrorStrings})
		      end
	      end,
	      NewBagS),

	    case xcall(?TIMING_IMM_OM, safs_imm_om, ccb_apply, [CcbId]) of
		{error, Reason} ->
		    ErrorStrings = safs_imm_om:ccb_get_error_strings(CcbId),

		    finCcb(undefined, CcbId),
		    finAo(undefined, AoHandle),
		    finImm(undefined, ImmHandle),

		    throw(#exception{key=ccb_apply,
				     data={insert, Reason, Source, CcbId, NewBagS},
				     errorStrings=ErrorStrings});
		ok ->
		    finCcb(undefined, CcbId),
		    finAo(undefined, AoHandle),
		    finImm(undefined, ImmHandle),
		    ?TIMING_UPDATE(?TIMING_CCB, CcbBegin),
		    ok

	    % not a good idea if 10k instances
	    % ?INFO("(~w) inserted config instance: ~p", [Source, Dn]),
	    end
    end.


-spec insertExtraAttrs([#unifInst{}]) ->
	  [#unifInst{}].
insertExtraAttrs(L) ->
    F1 = fun(#unifInst{parentName=ParentName,
		       className=ClassName,
		       attrs=AttrValues} = Rec) ->
		 ClassNameA = list_to_atom(binary_to_list(ClassName)),
		 #imm_class{rdn=#imm_rdn{name=RdnNameA}} =
		     gmfImmUgLib:getImmClass(ClassNameA),
		 ImmRdn = list_to_binary(atom_to_list(RdnNameA)),
		 RdnAttrVal = lists:keyfind(ImmRdn, #safsImmAttrValues_2.attrName,
					    AttrValues),
		 [#safsImmAttrValue{sastring = KeyValue}] =
		     RdnAttrVal#safsImmAttrValues_2.attrValues,
		 ImmDn = getImmDn(ImmRdn, KeyValue, ParentName),
		 ExtraAttrs = getExtraAttrs(ImmDn),
		 Rec#unifInst{extraAttrs=ExtraAttrs}
	 end,
    F = fun() -> lists:map(F1, L) end,
    {atomic, R} = mnesia:transaction(F),
    R.

-spec getImmDn(binary(), binary(), binary()|undefined) ->
	  binary().
getImmDn(ImmRdn, KeyValue, undefined) ->
    iolist_to_binary([ImmRdn, "=", KeyValue]);
getImmDn(ImmRdn, KeyValue, ParentName) ->
    iolist_to_binary([ImmRdn, "=", KeyValue, ",",  ParentName]).

-spec getExtraAttrs(binary()) -> [#safsImmAttrValues_2{}].

getExtraAttrs(<<$i, $d, $=, _/bytes>>) ->
    % Only needed as long as struct-as-object is in effect
    [];
getExtraAttrs(<<"systemFunctionsId=1">>) ->
    % gmfTrService:get_ecim_dn/1 cannot handle this case,
    % stick to the same behaviour for now; note that this
    % translation may be hard for gmfSALib:imm_to_mim/1
    % as well
    [];
getExtraAttrs(<<"sysMId=1,systemFunctionsId=1">>) ->
    % gmfTrService:get_ecim_dn/1 cannot handle this case,
    % stick to the same behaviour for now; note that this
    % translation may be hard for gmfSALib:imm_to_mim/1
    % as well
    [];
getExtraAttrs(ImmDn) ->
    try gmfSALib:imm_to_mim(ImmDn) of
	{error, Reason} ->
	    ?WARNING("failed to translate: ~p, reason: ~p", [ImmDn, Reason]),
	    [];
	{ok, MimDn} ->
	    ObjId = gmfTrService:get_object_id(MimDn),
	    [getImmAttrValues(<<"RcsImmAttrObjId">>,  uint32, ObjId),
	     getImmAttrValues(<<"RcsImmAttrEcimDn">>, string, MimDn)]
    catch
	ExType:ExData ->
	    Stack = erlang:get_stacktrace(),
	    ?WARNING("failed to translate: ~p, cause: ~p, stack -~n~p",
		     [ImmDn, {ExType, ExData}, Stack]),
	    []
    end.


-spec getImmAttrValues(binary(), atom(), term()) ->
	  #safsImmAttrValues_2{}.
getImmAttrValues(AttrName, AttrType, AttrValue) ->
    AttrValues = case AttrType of
		     uint32 -> [#safsImmAttrValue{sauint32 = AttrValue}];
		     string -> [#safsImmAttrValue{sastring = AttrValue}]
		 end,
    AttrValueType = case AttrType of
			uint32 -> sa_imm_attr_sauint32t;
			string -> sa_imm_attr_sastringt
		    end,
    #safsImmAttrValues_2{attrName         = AttrName,
			 attrValueType    = AttrValueType,
			 attrValuesNumber = 1,
			 attrValues       = AttrValues}.

%%% ----------------------------------------------------------
%%% @doc Returns true if the given class is properly modelled
%%% as a server-side class for bidirectional associations. The
%%% result is based on class information only; no instance is
%%% required. The result is cached in the ClassInfo table.
%%% @end
%%% ----------------------------------------------------------
-spec isReservable(className(), ets:tid(), #imm_class{}) -> boolean().

isReservable(ClassName, ClassInfo, ImmClass) ->
    case ets:lookup(ClassInfo, ClassName) of
	[#classInfo{reservable=undefined}] ->
	    IsReservable = isReservable(ImmClass),
	    ets:update_element(ClassInfo, ClassName, {#classInfo.reservable, IsReservable}),
	    IsReservable;
	[#classInfo{reservable=Result}] when Result =:= true orelse Result =:= false ->
	    Result
    end.


-spec isReservable(#imm_class{}) -> boolean().

isReservable(#imm_class{attrs=Attrs, name=NameA, category=CatClass}) ->
    if
	CatClass =/= config ->
	    case lists:keyfind(reservedBy, #imm_attr.name, Attrs) of
		false ->
		    % ?INFO("+++ not server: not config class: ~p", [NameA]),
		    false;
		_ ->
		    ?FAULT([user], "bad model: 'reservedBy' occurs in non-config class: ~p", [NameA]),
		    false
	    end;
	true ->
	    lists:any(
	      fun(#imm_attr{name=reservedBy, flags=Flags, type=Type, category=CatAttr}) ->
		      if
			  CatAttr =/= config ->
			      ?FAULT([user], "bad model: 'reservedBy' attribute is not 'config' in class: ~p", [NameA]),
			      false;
			  Type =/= sa_imm_attr_sanamet ->
			      ?FAULT([user], "bad model: 'reservedBy' attribute is not of 'name' type in class: ~p", [NameA]),
			      false;
			  true ->
			      case lists:member(multi_value, Flags) of
				  false ->
				      ?FAULT([user], "bad model: 'reservedBy' attribute is not multivalued in class: ~p", [NameA]),
				      false;
				  true ->
				      case lists:member(writable, Flags) of
					  false ->
					      ?FAULT([user], "bad model: 'reservedBy' attribute is not writable in class: ~p", [NameA]),
					      false;
					  true ->
					      % ?INFO("+++ well-formed 'reservedBy' attribute in class: ~p", [NameA]),
					      true
				      end
			      end
		      end;
		 (_) ->
		      % ?INFO("+++ not server: no 'reservedBy' attribute in class: ~p", [NameA]),
		      false
	      end,
	      Attrs)
    end.


%%% ----------------------------------------------------------
%%% @doc Deduce own DN.
%%% @end
%%% ----------------------------------------------------------
-spec getOwnDn(source(), parentName(), atom(), [#safsImmAttrValues_2{}]) ->
	  binary().

getOwnDn(Source, ParentName, RdnNameA, AttrValues) ->
    RdnNameS = atom_to_list(RdnNameA),
    RdnNameB = list_to_binary(RdnNameS),
    RdnValue =
	lists:foldl(
	  fun(_, S) when S =/= none ->
		  S;
	     (#safsImmAttrValues_2{attrValueType=?IMM_STRING,
				   attrName=ANB,
				   attrValuesNumber=1,
				   attrValues=[#safsImmAttrValue{sastring=S}]},
	      none) when ANB =:= RdnNameB ->
		  % it is trusted that S is a binarized string
		  S;
	     (_, _) ->
		  none
	  end,
	  none,
	  AttrValues),
    if
	RdnValue =:= none ->
	    throw(#exception{key=own_dn,
			     data={Source, ParentName, RdnNameA, AttrValues}});
	ParentName =:= undefined ->
	    list_to_binary(
	      RdnNameS ++ "=" ++ binary_to_list(RdnValue));
	true ->
	    list_to_binary(
	      RdnNameS ++ "=" ++ binary_to_list(RdnValue)++ "," ++ binary_to_list(ParentName))
    end.


%%% ----------------------------------------------------------
%%% @doc Insert all forward references for the given client
%%% instance. Bad references are collected and can be reported
%%% as one grand total.
%%% @end
%%% ----------------------------------------------------------
-spec insertFwdRefs(binary(),
		    orddict:orddict(binary(), [binary()]),
		    ets:tid(),
		    #xTree{},
		    [tuple()]) ->
	  [tuple()].

insertFwdRefs(Client, BidirRefs, BackRefs, FinalTree, BadRefs) ->
    {Modifications, NewBadRefs} =
	orddict:fold(
	  fun(AttrName, Targets, {Mods, B1}) ->
		  AttrValues = [#safsImmAttrValue{saname=Target} || Target <- Targets],
		  Modification =
		      #safsImmAttrModification_2{modType=sa_imm_attr_values_add,
						 modAttr=#safsImmAttrValues_2{attrName=AttrName,
									      attrValueType=?IMM_NAME,
									      attrValuesNumber=length(Targets),
									      attrValues=AttrValues}},
		  NewB1 =
		      lists:foldl(
			fun(Target, B2) ->
				case dict:find(Target, FinalTree#xTree.dict) of
				    error ->
					% bad forward reference detected; just
					% collect the info and let the caller of
					% this function throw an exception
					B2 ++ [{badFwdRef, Client, AttrName, Target}];
				    {ok, #xui{isServer=false}} ->
					% the server instance is not suited
					% for getting a back-reference
					?WARNING("badly modeled MO ref: ~p.~p -> ~p",
						 [Client, AttrName, Target]),
					B2;
				    _ ->
					% the forward reference is good and the
					% referred instance is a well-formed server
					[{_, BRs}] = ets:lookup(BackRefs, Target),
					ets:insert(BackRefs, {Target, [Client|BRs]}),
					B2
				end
			end,
			B1,
			Targets),
		  {Mods ++ [Modification], NewB1}
	  end,
	  {[], BadRefs},
	  BidirRefs),

    if
	length(NewBadRefs) > length(BadRefs) ->
	    % do not try to insert any references for this
	    % instance since all targets are not in place.
	    % The problem is caught higher up.
	    ignore;
	true ->
	    ImmHandle = getImmHandle(),
	    AoHandle = getAoHandle(Client, ImmHandle),
	    CcbId = getCcbId(Client, AoHandle),
	    setAdminOwner(AoHandle, [Client]),

	    case xcall(?TIMING_IMM_OM,
		       safs_imm_om, ccb_object_modify_2,
		       [CcbId,
			Client,
			Modifications]) of
		{error, Reason} ->
		    ErrorStrings = safs_imm_om:ccb_get_error_strings(CcbId),
		    ?WARNING("cannot restore bidirectional attributes, "
			     "reason: ~w, "
		             "error strings: ~p, "
			     "client: ~p~n"
			     "referred servers -~n"
			     "~160p",
			     [Reason, ErrorStrings, Client,
			      gmfImmUgLib:binariesToStrings(BidirRefs, ?MAX_STRLEN, [])]),
		    finCcb(Client, CcbId),
		    finAo(Client, AoHandle),
		    finImm(Client, ImmHandle),
		    throw(#exception{key=modify_ccb,
				     data={Reason, Client, BidirRefs},
				     errorStrings=ErrorStrings});
		ok ->
		    case xcall(?TIMING_IMM_OM, safs_imm_om, ccb_apply, [CcbId]) of
			{error, Reason} ->
			    ErrorStrings = safs_imm_om:ccb_get_error_strings(CcbId),
			    finCcb(Client, CcbId),
			    finAo(Client, AoHandle),
			    finImm(Client, ImmHandle),
			    throw(#exception{key=ccb_apply,
					     data={add_forward_refs, Reason, Client, BidirRefs},
					     errorStrings=ErrorStrings});
			ok ->
			    finCcb(Client, CcbId),
			    finAo(Client, AoHandle),
			    finImm(Client, ImmHandle)
%% While fixing HU22829, dropped this logging since it becomes
%% very bulky with large configurations.
%% 		    ,
%% 			    ?INFO("bidir refs inserted: ~p ->~n~p",
%% 				  [Client, BidirRefs])
		    end
	    end
    end,
    NewBadRefs.


%%% ----------------------------------------------------------
%%% @doc Add reservations.
%%% @end
%%% ----------------------------------------------------------
-spec addReservations(source(), binary(), [binary()]) -> ok.

addReservations(_Source, _Server, []) ->
    ok;

addReservations(Source, Server, InClients) ->
    Clients = ootI:rm_duplicate_oap_ref(InClients, Server),
    ImmHandle = getImmHandle(),
    AoHandle = getAoHandle(Server, ImmHandle),
    CcbId = getCcbId(Server, AoHandle),
    setAdminOwner(AoHandle, [Server]),
    AttrValues = [#safsImmAttrValue{saname=Client} || Client <- Clients],
    case xcall(?TIMING_IMM_OM, safs_imm_om, ccb_object_modify_2, [
	   CcbId,
	   Server,
	   [#safsImmAttrModification_2{modType=sa_imm_attr_values_add,
				       modAttr=#safsImmAttrValues_2{attrName= ?RESERVED_BY,
								    attrValueType=?IMM_NAME,
								    attrValuesNumber=length(Clients),
								    attrValues=AttrValues}}]]) of
	{error, Reason} ->
	    ErrorStrings = safs_imm_om:ccb_get_error_strings(CcbId),
	    ?WARNING("(~w) cannot add 'reservedBy' entries, "
		  "reason: ~w, "
	          "error strings: ~p, "
		  "server: ~p~n"
		  "clients -~n"
		  "~160p",
		  [Source, Reason, ErrorStrings, Server,
		   gmfImmUgLib:binariesToStrings(Clients, ?MAX_STRLEN, [])]),
	    finCcb(Server, CcbId),
	    finAo(Server, AoHandle),
	    finImm(Server, ImmHandle),
	    throw(#exception{key=modify_ccb,
			     data={Reason, Server, Clients},
			     errorStrings=ErrorStrings});
	ok ->
	    case xcall(?TIMING_IMM_OM, safs_imm_om, ccb_apply, [CcbId]) of
		{error, Reason} ->
		    ErrorStrings = safs_imm_om:ccb_get_error_strings(CcbId),
		    finCcb(Server, CcbId),
		    finAo(Server, AoHandle),
		    finImm(Server, ImmHandle),
		    throw(#exception{key=ccb_apply,
				     data={add_backward_refs, Reason, Server, Clients},
				     errorStrings=ErrorStrings});
		ok ->
		    finCcb(Server, CcbId),
		    finAo(Server, AoHandle),
		    finImm(Server, ImmHandle)
%% Dropped this logging since it becomes quite bulky
%% with large configurations.
%% 	    ,
%% 		    ?INFO("(~w) ~p now reserved by -~n~p",
%% 			  [Source, Server,
%% 			   gmfImmUgLib:binariesToStrings(Clients, ?MAX_STRLEN, [])])
	    end
    end.


%%% ----------------------------------------------------------
%%% @doc Gets an IMM handle.
%%% @end
%%% ----------------------------------------------------------
getImmHandle() ->
    {ok, ImmHandle, OfferedVersion} =
	xcall(?TIMING_IMM_OM, safs_imm_om, initialize, [undefined, ?SAFS_VERSION]),
    case isGreaterVersion(?SAFS_VERSION, OfferedVersion) of
	true ->
	    throw(#exception{key=safs_version, data=OfferedVersion});
	_ ->
	    ImmHandle
    end.


isGreaterVersion(#safsVersion{releaseCode=RC1,
			      majorVersion=MA1,
			      minorVersion=MI1},
		 #safsVersion{releaseCode=RC2,
			      majorVersion=MA2,
			      minorVersion=MI2}) ->
    RC1 > RC2 orelse
    MA1 > MA2 orelse
    MI1 > MI2.


-spec oiInitialize() -> oiHandle().

oiInitialize() ->
    case
	xcall(?TIMING_IMM_OI, safs_imm_oi, initialize, [undefined])
	%safs_imm_oi:initialize(undefined)
	of
	{ok, Handle, _Version} ->
	    Handle;
	Error ->
	    throw(#exception{key=oi_init, data=Error})
    end.


-spec oiImplementerSet(oiHandle(), binary()) -> ok.

oiImplementerSet(Handle, Implementer) ->
    % ?INFO("executing: safs_imm_oi:implementer_set(~p, ~p)", [Handle, Implementer]),
    case xcall(?TIMING_IMM_OI, safs_imm_oi, implementer_set, [Handle, Implementer]) of
	ok ->
	    % ?INFO("           safs_imm_oi:implementer_set(~p, ~p) -> ok", [Handle, Implementer]),
	    ok;
	Error ->
	    throw(#exception{key=oi_impl_set,
			     data={Handle, Implementer, Error}})
    end.


-spec oiImplementerClear(oiHandle()) -> ok.

oiImplementerClear(Handle) ->
    % ?INFO("executing: safs_imm_oi:implementer_clear(~p)", [Handle]),
    case xcall(?TIMING_IMM_OI, safs_imm_oi, implementer_clear, [Handle]) of
	ok ->
	    % ?INFO("           safs_imm_oi:implementer_clear(~p) -> ok", [Handle]),
	    ok;
	Error ->
	    throw(#exception{key=oi_impl_clear, data={Handle, Error}})
    end.


-spec oiRtObjectCreate(oiHandle(), className(), parentName(), [#safsImmAttrValues_2{}]) -> ok.

oiRtObjectCreate(Handle, ClassName, ParentName, AttrValues) ->
    case xcall(
	   ?TIMING_IMM_OI,
	   safs_imm_oi,
	   rt_object_create_2,
	   [Handle, ClassName, ParentName, AttrValues]) of
	ok ->
	    ok;
	Error ->
	    throw(#exception{key=oi_create,
			     data={Handle, Error, ClassName, ParentName, AttrValues}})
    end.


-spec oiFinalize(oiHandle()) -> ok.

oiFinalize(Handle) ->
    case xcall(?TIMING_IMM_OI, safs_imm_oi, finalize, [Handle]) of
	ok ->
	    ok;
	Error ->
	    throw(#exception{key=oi_fin, data=Error})
    end.


-spec getAoHandle(className()|undefined, any()) ->
	  any().

getAoHandle(ClassName, ImmHandle) ->
    case xcall(?TIMING_IMM_OM, safs_imm_om, admin_owner_initialize, [ImmHandle, ?MODULE_B, true]) of
	{error, Reason} ->
	    throw(#exception{key=admin_owner_init, data={Reason, ClassName, ImmHandle}});
	{ok, AoHandle} ->
	    AoHandle
    end.


-spec getCcbId(className()|undefined, any()) ->
	  any().

getCcbId(ClassName, AoHandle) ->
    CcbFlags = 0,
    case xcall(?TIMING_IMM_OM, safs_imm_om, ccb_initialize, [AoHandle, CcbFlags])
%% 	safs_imm_om:ccb_initialize(AoHandle, CcbFlags)
	of
	{error, Reason} ->
	    throw(#exception{key=ccb_id, data={Reason, ClassName, AoHandle}});
	{ok, CcbId} ->
	    CcbId
    end.


setAdminOwner(AoHandle, Classes) ->
    case xcall(?TIMING_IMM_OM, safs_imm_om, admin_owner_set, [AoHandle, Classes, sa_imm_one]) of
	{error, Reason} ->
	    throw(#exception{key=admin_owner_set, data={Reason, Classes, AoHandle}});
	ok->
	    ok
    end.


-spec finCcb(className()|undefined, any()) ->
	  ok.

finCcb(ClassName, CcbId) ->
    case xcall(?TIMING_IMM_OM, safs_imm_om, ccb_finalize, [CcbId]) of
	{error, Reason} ->
	    throw(#exception{key=ccb_fin, data={Reason, ClassName, CcbId}});
	ok ->
	    ok
    end.


-spec finAo(className()|undefined, any()) ->
	  ok.

finAo(ClassName, AoHandle) ->
    case xcall(?TIMING_IMM_OM, safs_imm_om, admin_owner_finalize, [AoHandle]) of
	{error, Reason} ->
	    throw(#exception{key=admin_owner_fin, data={Reason, ClassName, AoHandle}});
	ok ->
	    ok
    end.


-spec finImm(className()|undefined, any()) ->
	  ok.

finImm(ClassName, ImmHandle) ->
    case xcall(?TIMING_IMM_OM, safs_imm_om, finalize, [ImmHandle]) of
	{error, Reason} ->
	    throw(#exception{key=imm_fin, data={Reason, ClassName, ImmHandle}});
	ok ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc Creates an extended unified instance.
%%% @end
%%% ----------------------------------------------------------
-spec createXui(#unifInst{}, immMode(), ets:tid()) ->  #xui{}.

createXui(#unifInst{className=ClassName, attrs=Attrs, parentName=PN}=UI, Cat, ClassInfo) ->
    ClassNameA = list_to_atom(binary_to_list(ClassName)),
    #imm_class{rdn=#imm_rdn{name=RdnNameA}}=ImmClass = gmfImmUgLib:getImmClass(ClassNameA),
    RdnNameB = list_to_binary(atom_to_list(RdnNameA)),

    % get the list of binarized attr names
    BidirAttrs = getBidirAttrs(ImmClass),

    IsServerClass = isReservable(ClassName, ClassInfo, ImmClass),

    AttrInfo =
	lists:foldl(
	  fun(#safsImmAttrValues_2{attrName=NameB,
				   attrValuesNumber=N,
				   attrValueType=T,
				   attrValues=[#safsImmAttrValue{sastring=ValueB}]},
	      {undefined, BidirRefs}) when
	       NameB =:= RdnNameB,
	       N =:= 1,
	       T =:= sa_imm_attr_sastringt ->
		  % We are seeing the RDN!
		  Rdn =
		      list_to_binary(binary_to_list(NameB)++"="++binary_to_list(ValueB)),
		  {Rdn, BidirRefs};

	     (#safsImmAttrValues_2{attrName=NameB,
				   attrValueType=sa_imm_attr_sanamet,
				   attrValues=AVs},
	      {Rdn, BidirRefs}) ->
		  % We are seeing a bidir attribute maybe
		  case lists:member(NameB, BidirAttrs) of
		      false ->
			  {Rdn, BidirRefs};
		      true ->
			  % We know it is no_dangling too
			  {Rdn, orddict:append_list(
			     NameB,
			     [ValueB || #safsImmAttrValue{saname=ValueB} <- AVs],
			     BidirRefs)}
		  end;
	     (_, {_, _}=Acc) ->
		  % We are seeing a non-moref attribute
		  Acc
	  end,
	  {undefined, orddict:new()},   % RDN and orddict of bidir refs
	  Attrs),

    case AttrInfo of
	{undefined, _} ->
	    throw(#exception{key=unknown_rdn, data={ClassName, RdnNameB}});
	{Rdn, BidirRefs} ->
	    Dn =
		if PN =:= undefined ->
		       Rdn;
		   true ->
		       list_to_binary(binary_to_list(Rdn)++","++binary_to_list(PN))
		end,
	    StrippedUi = stripBackReferences(IsServerClass, UI),
	    #xui{rdn=Rdn,
		 dn=Dn,
		 category=Cat,
		 bidirRefs=BidirRefs,
		 ui=StrippedUi,
		 isServer=IsServerClass}
    end.


%%% ----------------------------------------------------------
%%% @doc Gets the names of candidate bidirectional attributes
%%% for the given class.
%%% @end
%%% ----------------------------------------------------------
-spec getBidirAttrs(#imm_class{}) ->  [binary()].

getBidirAttrs(#imm_class{category=runtime}) ->
    [];

getBidirAttrs(#imm_class{attrs=Attrs}) ->
    lists:foldl(
      fun(#imm_attr{name=AttrNameA,
		    type=sa_imm_attr_sanamet,
		    category=config,
		    flags=Flags}, Acc) ->
	      case proplists:get_bool(no_dangling, Flags) of
		  false ->
		      Acc;
		  true ->
		      Acc++[list_to_binary(atom_to_list(AttrNameA))]
	      end;
	 (_, Acc) ->
	      Acc
      end,
      [],
      Attrs).


%%% ----------------------------------------------------------
%%% @doc Conditionally remove backward references from the
%%% given #unifInst{}.
%%%
%%% Also set up a scoreboard entry for collecting reservations.
%%% @end
%%% ----------------------------------------------------------
-spec stripBackReferences(boolean(), #unifInst{}) ->  #unifInst{}.

stripBackReferences(false, UI) ->
    UI;

stripBackReferences(true, #unifInst{attrs=Attrs}=UI) ->
    NewAttrs =
	lists:foldr(
	  fun(#safsImmAttrValues_2{attrName= <<"reservedBy">>}=AV2, Acc) ->
		  [AV2#safsImmAttrValues_2{attrValues=[], attrValuesNumber=0}|Acc];
	     (Other, Acc) ->
		  [Other|Acc]
	  end,
	  [],
	  Attrs),
    UI#unifInst{attrs=NewAttrs}.


-spec reapply(fun((any()) ->  {boolean(), any()}), any()) ->  any().

reapply(Fun, State) ->
    case apply(Fun, [State]) of
	{false, NewState} ->
	    NewState;
	{true, NewState} ->
	    reapply(Fun, NewState)
    end.


%%% ----------------------------------------------------------
%%% @doc Insert given instances, which are siblings under a
%%% common parent, into the given tree tracker. The updated
%%% tree tracker is returned.
%%%
%%% This function performs pass 1 of the 3-pass tree building
%%% algorithm.
%%% @end
%%% ----------------------------------------------------------
-spec treeAddInsts(source(), #xSiblings{}, #xTree{}, ets:tid(),
	                  ets:tid(), ets:tid()) ->
                      #xTree{}.

treeAddInsts(Source, #xSiblings{dict=SS}, #xTree{dict=XTreeDict}, BackRefs, FwdRefs, Counters) ->
    {NewTreeDict, BagSizeFinal, BagSFinal, BagFinal} =
	orddict:fold(
	  fun(_Rdn,
	      #xui{bidirRefs=BidirRefs,
		   category=Cat,
		   dn=InstDn,
		   isServer=IsServer,
		   ui=#unifInst{className=ClassName}}=Xui,
	      {D, BagSize, BagS, Bag}) ->
		  if
		      IsServer ->
			  case ets:insert_new(BackRefs, {InstDn, []}) of
			      false ->
				  % never supposed to happen
				  throw(#exception{key=bidir_incons,
						   data={backRefs, ClassName, InstDn}});
			      _ ->
				  ok
			  end;
		      true ->
			  ok
		  end,
		  case orddict:is_empty(BidirRefs) of
		      true ->
			  ok;
		      _ ->
			  case ets:insert_new(FwdRefs, {InstDn, BidirRefs}) of
			      false ->
				  % never supposed to happen
				  throw(#exception{key=bidir_incons,
						   data={fwdRefs, ClassName, InstDn}});
			      _ ->
				  ok
			  end
		  end,
		  StrippedUi = stripBidirRefs(Xui),
		  % the back-refs are stripped already, at xui
		  % creation time

		  if
		      Cat =:= config andalso BagSize =:= ?CHUNK_SIZE ->
			  DPlus = flushBags(Source, BagSize, BagS, Bag, Counters, D),
			  {DPlus, 1, [StrippedUi], [Xui]};
		      Cat =:= config ->
			  {D, BagSize + 1, [StrippedUi|BagS], [Xui|Bag]};
		      Cat =:= persistentRuntime ->
			  insertRuntimePersistentInst(Source, ClassName, StrippedUi),
			  ets:update_counter(Counters, ?INSERTIONS, 1),
			  {dict:store(InstDn, Xui, D), BagSize, BagS, Bag}
		  end
	  end,
	  {XTreeDict, 0, [], []},
	  SS),

    NewTreeDictFinal =
	flushBags(Source, BagSizeFinal, BagSFinal, BagFinal, Counters, NewTreeDict),
    #xTree{dict=NewTreeDictFinal}.


%%% ----------------------------------------------------------
%%% @doc Calls mnesia:dump_log/0 in case of overload. This is
%%% necessary in order to prevent the mnesia transaction log
%%% from growing in an uncontrolled manner.
%%%
%%% This function can only be used by processes that subscribe
%%% to mnesia 'system' notifications.
%%% @end
%%% ----------------------------------------------------------
-spec overloadCheck(source()) -> any().

overloadCheck(Source) ->
    receive
	{mnesia_system_event,
	 {mnesia_overload, {dump_log, write_threshold}}} ->
	    xcall(?TIMING_MNESIA, mnesia, dump_log, []);
	{mnesia_system_event, {mnesia_overload, Details}} ->
	    ?FAULT([user, {source, Source}],
		   "unexpected mnesia overload: ~p",
		   [Details])
    after 0 ->
	ok
    end.


-spec flushBags(
	source(),
	non_neg_integer(),
	[#unifInst{}],
	[#xui{}],
	ets:tid(),
	dict:dict()) -> dict:dict().

flushBags(Source, BagSize, BagS, Bag, Counters, D) ->
    overloadCheck(Source),
    if
	BagSize =:= 0 ->
	    D;
	true ->
	    insertChunk(Source, BagS),
	    ets:update_counter(Counters, ?INSERTIONS, BagSize),
	    lists:foldl(
	      fun(#xui{dn=ID}=E, A) ->
		      dict:store(ID, E, A)
	      end,
	      D,
	      Bag)
    end.


-spec stripBidirRefs(#xui{}) -> #unifInst{}.

stripBidirRefs(#xui{category=Category, ui=UnifInst}) when Category =/= config ->
    UnifInst;

stripBidirRefs(#xui{ui=UnifInst, bidirRefs=BidirRefs}) ->
    case orddict:is_empty(BidirRefs) of
	true ->
	    % checking for empty is really just an optimization
	    % that is perhaps meaningless; TODO
	    UnifInst;
	false ->
	    #unifInst{attrs=Attrs} = UnifInst,
	    NewAttrs =
		lists:filter(
		  fun(#safsImmAttrValues_2{attrName=AttrName}) ->
			  not(orddict:is_key(AttrName, BidirRefs))
		  end,
		  Attrs),
	    UnifInst#unifInst{attrs=NewAttrs}
    end.


-spec bufferUpdate(parentName(), #xSiblings{}, #xBuffer{}) ->
                      #xBuffer{}.

bufferUpdate(ParentDn, Siblings, #xBuffer{dict=Dict}) ->
    #xBuffer{dict=orddict:store(ParentDn, Siblings, Dict)}.


-spec bufferIsKey(parentName(), #xBuffer{}) ->  boolean().

bufferIsKey(ParentDn, #xBuffer{dict=Dict}) ->
    orddict:is_key(ParentDn, Dict).


%%% ----------------------------------------------------------
%%% @doc Gets the siblings for the given parent name.
%%% @end
%%% ----------------------------------------------------------
-spec bufferGet(parentName(), #xBuffer{}) ->  #xSiblings{}.

bufferGet(ParentDn, #xBuffer{dict=Dict}) ->
    orddict:fetch(ParentDn, Dict).


%%% ----------------------------------------------------------
%%% @doc Delete the given parent DN from the buffer. It is
%%% believed to exist always.
%%% @end
%%% ----------------------------------------------------------
-spec bufferDelete(parentName(), #xBuffer{}) ->  #xBuffer{}.

bufferDelete(ParentDn, #xBuffer{dict=Dict}) ->
    case orddict:is_key(ParentDn, Dict) of
	false ->
	    throw(#exception{key=buffer_incons, data=ParentDn});
	true ->
	    #xBuffer{dict=orddict:erase(ParentDn, Dict)}
    end.


%%% ----------------------------------------------------------
%%% @doc Returns the number of instances in the buffer.
%%% @end
%%% ----------------------------------------------------------
-spec bufferSize(#xBuffer{}) -> non_neg_integer().

bufferSize(#xBuffer{dict=Dict}) ->
    orddict:fold(
      fun(_ParentDn, Siblings, Acc) ->
	      Acc + nofSiblings(Siblings)
      end,
      0,
      Dict).


%%% ----------------------------------------------------------
%%% @doc Create a new siblings map and enter the given
%%% xui into it.
%%% @end
%%% ----------------------------------------------------------
-spec siblingsFromXui(#xui{}) -> #xSiblings{}.

siblingsFromXui(#xui{rdn=Rdn}=Xui) ->
    #xSiblings{dict=orddict:from_list([{Rdn, Xui}])}.


%%% ----------------------------------------------------------
%%% @doc Adds one instance to the given siblings map. The
%%% instance must not be present already.
%%% @end
%%% ----------------------------------------------------------
-spec addSibling(#xui{}, #xSiblings{}, parentName()) ->  #xSiblings{}.

addSibling(#xui{rdn=Rdn}=Xui, #xSiblings{dict=Dict}, PN) ->
    case orddict:is_key(Rdn, Dict) of
	true ->
	    % show the few first entries in the siblings dict
	    throw(#exception{key=duplicate_rdn,
			     data={PN,
				   Xui,
				   lists:sublist(orddict:to_list(Dict), 3)}});
	false ->
	    #xSiblings{dict=orddict:store(Rdn, Xui, Dict)}
    end.


%%% ----------------------------------------------------------
%%% @doc Returns the size of the siblings map.
%%% @end
%%% ----------------------------------------------------------
-spec nofSiblings(#xSiblings{}) ->  non_neg_integer().

nofSiblings(#xSiblings{dict=Dict}) ->
    orddict:size(Dict).


%%% ----------------------------------------------------------
%%% @doc Returns a list of keys that are present in both
%%% dicts.
%%% @end
%%% ----------------------------------------------------------
-spec dictIntersect(#xBuffer{}, #xTree{}) -> [binary()].

dictIntersect(XBuffer, XTree) ->
    Dict = XTree#xTree.dict,
    orddict:fold(
      fun(K, _V, Acc) ->
	      case dict:find(K, Dict) of
		  error ->
		      Acc;
		  _ ->
		      [K|Acc]
	      end
      end,
      [],
      XBuffer#xBuffer.dict).


%%% ----------------------------------------------------------
%%% @doc Accumulates time spent in function calls.
%%% @end
%%% ----------------------------------------------------------
xcall(Tag, M, F, A) ->
    T1 = ?MICROS(),
    Value = apply(M, F, A),
    T2 = ?MICROS(),
    ets:update_counter(?TIMING, Tag, T2 - T1),
    Value.


%%% ----------------------------------------------------------
%%% @doc Report counter values.
%%% @end
%%% ----------------------------------------------------------
-spec reportCounters(ets:tid()) -> any().

reportCounters(Counters) ->
    [{_, Insertions}] = ets:lookup(Counters, ?INSERTIONS),
    [{_, SimpleInst}] = ets:lookup(Counters, ?SIMPLE_INST),
    [{_, StructOwnerInst}] = ets:lookup(Counters, ?STRUCT_OWNER_INST),
    [{_, StructInst}] = ets:lookup(Counters, ?STRUCT_INST),
    ?INFO("instances inserted: ~w, "
	  "simple: ~w, "
	  "struct-owner: ~w, "
	  "struct-inst: ~w",
	  [Insertions, SimpleInst, StructOwnerInst, StructInst]).

%%% ----------------------------------------------------------
%%% @doc Report timing info.
%%% @end
%%% ----------------------------------------------------------
-spec reportTiming() -> any().

reportTiming() ->
    ?INFO("timing summary (milliseconds) -~n"
	  "repeated insert: ~w~n"
	  "  insert chunks: ~w~n"
	  "       drop log: ~w~n"
	  "    extra attrs: ~w~n"

	  "   format insts: ~w~n"
	  "         IMM OM: ~w (total time in IMM OM calls)~n"
          "         IMM OI: ~w (total time in IMM OI calls)~n"
	  "   mnesia delay: ~w~n"
	  ,
	  [reportTimingHelper(?TIMING_REP_INSERT),
	   reportTimingHelper(?TIMING_CCB),
	   reportTimingHelper(?TIMING_DROP_LOG),
	   reportTimingHelper(?TIMING_XATTRS),
	   reportTimingHelper(?TIMING_FORMAT),
	   reportTimingHelper(?TIMING_IMM_OM),
	   reportTimingHelper(?TIMING_IMM_OI),
	   reportTimingHelper(?TIMING_MNESIA)
	  ]).


%%% ----------------------------------------------------------
%%% @doc Returns milliseconds for the given key.
%%% @end
%%% ----------------------------------------------------------
-spec reportTimingHelper(atom()) -> integer().

reportTimingHelper(Key) ->
    [{_, Micros}] = ets:lookup(?TIMING, Key),
    Micros div 1000.
