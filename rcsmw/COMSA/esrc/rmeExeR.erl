%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rmeExeR.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2017
%%% @version /main/R8A/R9A/R10A/R11A/R12A/2

%%% @doc ==Agent implementation for Execution Resource==
%%% This is the agent implementation for Execution Resource, manualRestart
%%% is supported on both physical and virtual nodes.

-module(rmeExeR).
-vsn('/main/R8A/R9A/R10A/R11A/R12A/2').
-date('2017-11-30').
-author(egjknpa).
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
%% ###=======================================================================###
%% # 1   REVISION LOG
%% ###-----------------------------------------------------------------------###
%% Rev     Date       Name     What
%% ----    ---------- -------  -------------------------------------------------
%% R8A/1   2017-01-11 etxjotj  Use generated names from RmeExeR
%% R9A/1   2017-02-23 etxpejn  Added getMoAttribute for vnfIdentity
%% R9A/2   2017-03-08 etxpejn  Added getMoAttribute for upgradeStatus
%% R9A/3   2017-03-23 etxberb  Added AVLI logging for "manualRestart".
%% R10A/1  2017-05-03 etxarnu  Changed appmI:restart_node to restart_own_piu
%% R11A/1  2017-09-04 etxjotj  Replaced swmLib with swmI
%% R12A/1  2017-10-30 etxarnu  Changed swmServer:format to swmREST:mo_format
%% R12A/2  2017-11-30 etxjotj  Remove upgradeStatus for R-VNFM
%% ------  ---------- -------  ---END-OF-LOG------------------------------------

%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([getMoAttribute/2, nextMo/3]).
-export([prepare/3,commit/3,finish/3]).
-export([validate/3]).
-export([createMo/4,setMoAttribute/4,deleteMo/2]).
-export([existsMo/2,
         countMoChildren/3,
         getMoAttributes/3,
         setMoAttributes/3,
         action/4,
         createMo/5]).
-export([init_data/1]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RmeExeR.hrl").
-include("comTypes.hrl").
-include("alhI.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%% @doc Returns true if the specified instance exists.
existsMo(DnRev, _) ->
    comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))).

%%% @doc Returns the number of MO instances of given class
%%% directly below the specified parent.
countMoChildren(DnRev, Class, _) ->
    comsaGeneric:countMoChildren(DnRev, table(binary_to_list(Class))).

%%% @doc Gets MO attribute values.
%%% The instance is specified by ReversedDn and the attributes to be
%%% fetched are given by the AttrNames argument.
getMoAttributes(AttrNames, DnRev, TxHandle) ->
    case comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))) of
	true  ->
	    [getMoAttribute([AN | DnRev], TxHandle) || AN <- AttrNames];
	false ->
	    []
    end.

getMoAttribute([<<"upgradeStatus">>|_DnRev], _) ->
    case swmI:node_type() of
	"R-VNFM" ->
	    undefined;
	_ ->
	    [Obj] = mnesia:dirty_read(upgradePackage, swmInventory:make_currentUP_key()),
	    State = proplists:get_value(state, swmREST:mo_format(Obj)),
	    ?STRING(list_to_binary(State))
    end;

getMoAttribute([<<"vnfIdentity">>|_DnRev], _) ->
    case os:getenv("VNF_ID") of
	false ->
	    %% No vRCS node or a VNF standalone test node
	    undefined;
	VnfId ->
	    ?STRING(list_to_binary(VnfId))
    end;
   
getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).

%% @doc Get the next key for the MO given by the Dn
nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

setMoAttributes(Attrs, DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), Attrs).

%% @doc Set the attribute given by the Attribute to TypeAndValue.
setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).

%% @doc Create the MO given by the ClassName with the given
%%   KeyValue and InitAttrs.
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

%% @doc Delete the MO given by the Dn. 
deleteMo(_Dn, _Tx) ->
    ok.


table("ExecutionResource") -> executionResource.
types(executionResource) -> ?executionResource_types.

%% @doc Validate an ECIM MO. 
validate(_DN, User, _Tx) ->
    {ok,User}.
%% @doc Prepare an ECIM MO for being commited.
prepare(_DN, User, _Tx) ->
    {ok,User}.
%% @doc Commit an transaction.
commit(_DN, User, _Tx) ->
    {ok,User}.
%% @doc Called after the transaction is commited.
finish(_DN, _User, _Tx) ->
    ok.


values([{Name, {9, Value}} | Tail]) ->
    [{Name, binary_to_list(Value)} | values(Tail)];
values([{Name, {_, Value}} | Tail]) ->
    [{Name, Value} | values(Tail)];
values([Attr | Tail]) ->
    [Attr | values(Tail)];
values([]) ->
    [].

%% @doc Do the action given by the Name.
action(<<"manualRestart">>, _DnRev, Params, _TransId) ->
    RestartRank = get_value("restartRank", Params),
    RestartReason = get_value("restartReason", Params),
    RestartInfo = get_value("restartInfo", Params),
    RestartInfoAvli = "restartInfo: " ++ sysUtil:term_to_string(RestartInfo),
    alhI:write_node_event(os:timestamp(),
			  ?ALH_TAG_OutOfService,
			  ?ALH_TAG_ShutdownCommand,
			  0,   % EventId
			  ?ALH_TAG_Rcs([?ALH_TAG_Cause(?ALH_TAG_ManualRestart),
					?ALH_TAG_Cause(RestartInfoAvli)]),
			  async),
    info_msg("action manualRestart ~p ~p ~p~n",
	     [enum_value_to_atom({restartRank, RestartRank}),
	      enum_value_to_string({restartReason, RestartReason}),
	      RestartInfo]),
					     
    appmI:restart_own_piu(enum_value_to_atom({restartRank, RestartRank}), 
			  false,
			  enum_value_to_string({restartReason, RestartReason})),
    ok;
action(_Name, _DnRev, _Params, _TransId) ->
    {error, <<"Not implemented">>}.

get_value(Key, Params) ->
    case proplists:get_value(list_to_binary(Key), Params) of
	{_, Value} ->
	    Value;
	_ ->
	    throw(error)
    end.

init_data(upgrade) ->
    swmI:copy_old_table(executionResource);
init_data(fromScratch) ->
    mnesia:dirty_write(
      #executionResource{executionResourceId={"1","1","1"}
			}).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

enum_value_to_atom({restartRank, ?RestartRank_RESTART_WARM}) ->
    warm;
enum_value_to_atom({restartRank, ?RestartRank_RESTART_COLD}) ->
    cold;
enum_value_to_atom({restartRank, ?RestartRank_RESTART_COLDWTEST}) ->
    cold_with_test.


enum_value_to_string({restartReason, ?RestartReason_PLANNED_RECONFIGURATION}) ->
    "PLANNED_RECONFIGURATION";
enum_value_to_string({restartReason, ?RestartReason_UNPLANNED_NODE_EXTERNAL_PROBLEMS}) ->
    "UNPLANNED_NODE_EXTERNAL_PROBLEMS";
enum_value_to_string({restartReason, ?RestartReason_UNPLANNED_NODE_UPGRADE_PROBLEMS}) ->
    "UNPLANNED_NODE_UPGRADE_PROBLEMS";
enum_value_to_string({restartReason, ?RestartReason_UNPLANNED_O_AND_M_ISSUE}) ->
    "UNPLANNED_O_AND_M_ISSUE";
enum_value_to_string({restartReason, ?RestartReason_UNPLANNED_CYCLIC_RECOVERY}) ->
    "UNPLANNED_CYCLIC_RECOVERY";
enum_value_to_string({restartReason, ?RestartReason_UNPLANNED_LOCKED_RESOURCES}) ->
    "UNPLANNED_LOCKED_RESOURCES";
enum_value_to_string({restartReason, ?RestartReason_UNPLANNED_COLD_WITH_HW_TEST}) ->
    "UNPLANNED_COLD_WITH_HW_TEST";
enum_value_to_string({restartReason, ?RestartReason_UNPLANNED_CALL_PROCESSING_DEGRADATION}) ->
    "UNPLANNED_CALL_PROCESSING_DEGRADATION";
enum_value_to_string({restartReason, ?RestartReason_UNPLANNED_LOW_COVERAGE}) ->
    "UNPLANNED_LOW_COVERAGE";
enum_value_to_string({restartReason, ?RestartReason_OPERATOR_CLASSIFIED_PROBLEMS}) ->
    "OPERATOR_CLASSIFIED_PROBLEMS".


%% info_msg(Format) ->
%%     info_msg(Format, []).
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%% error_msg(Format) ->
%%     error_msg(Format, []).
%% error_msg(Format, Args) ->
%%     sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
%% warning_msg(Format, Args) ->
%%     sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
