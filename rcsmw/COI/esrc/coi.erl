%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	coi.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R5A/R6A/R8A/R9A/R11A/7

%%% @doc == COI - Control system Oam Interface ==
%%% This module contains the Erlang interface for interacting with the
%%% Operation and maintenance functionality in CS.
%%%
%%% TODO: Use of error_logger reporting functions, change to sysInitI?
%%%
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-module(coi).
-vsn('/main/R3A/R4A/R5A/R6A/R8A/R9A/R11A/7').
-date('2017-10-17').
-author(etxberb).

%%% ###=====================================================================###
%%% # 0.3   LEGAL RIGHTS
%%% ###---------------------------------------------------------------------###
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 1.    REVISION LOG
%%% ###---------------------------------------------------------------------###
%%% Rev    Date       Name     What
%% R3A/    ---------- -------  ------------------------------------------------
%% In the COMSA block:
%% ===================
%%% 1      2014-10-07 etxberb  Created.
%%% 2      2014-10-10 etxberb  Added join_new/0 and fixed new_transaction_id.
%%% 3      2014-10-20 etxberb  Added getMimFiles/0 and getMimClass/1.
%%%                            Removed setMoAttribute/4 & getMoAttribute/3
%%% 4      2014-10-28 etxberb  Added keyfind/2 & tuplepair/2.
%%% 6      2014-11-10 etxberb  Changed tuplepair/2 to pairElems/2.
%%% 7      2014-11-21 etxberb  * Removed deprecated functions.
%%%                            * Moved getMimClass execution to coiMim.erl.
%%% R3A/9  2014-12-09 etxberb  Added getMoTree/X & getMimTree/X.
%%% R3A/10 2014-12-10 etxberb  A return value (MoValue) for getMoTree changed
%%%                            from false to undefined.
%%% R3A/11 2014-12-12 etxberb  Added subscribe/1
%%% R3A/12 2014-12-30 etxberb  Added log_*/*, new_transaction_id/1,
%%%                            new_transaction_id/2, join_new/1 & join_new/2.
%%% R3A/13 2015-01-02 etxberb  Added Audit Trail logging.
%%% R3A/14 2015-01-05 etxberb  Added Audit Trail logging for Attributes in
%%%                            createMo.
%% ===================
%% R3A/1   2015-01-13 etxberb  Moved to the COI block.
%% R3A/2   2015-02-04 etxberb  Added get_transaction_xxx/1.
%% R4A/1   2015-08-20 etxpejn  Added rpc:call for SecurityLog
%% R4A/2   2015-08-21 etxpejn  Added rpc:call for AuditTrailLog
%% R4A/3   2015-08-28 etxjotj  New format for getMoAttributes from comsa
%% R4A/4   2015-08-28 etxjotj  Dialyzer fix
%% R4A/5   2015-08-28 etxberb  Correction of format conversion of
%%                             getMoAttributes from comsa.
%% R4A/6   2015-09-25 etxpejn  Moved rpc:call to logI:write_log
%% R4A/8   2015-10-13 etxberb  Added mimVal_to_mibStr/2.
%% R4A/9   2015-10-13 etxberb  Added mimVal_EventType_to_str/1.
%% R4A/10  2015-10-21 etxberb  Moved mimVal_to_mibStr functions to coiMib.erl.
%% R4A/11  2015-10-23 etxberb  Extra clauses of getMoAttributes_oldFormat added.
%% R4A/12  2015-11-11 etxberb  Added is_attribute/3.
%% R4A/14  2015-11-26 etxberb  Fixed dialyzer fault.
%% R4A/15  2015-11-29 erarafo  Result type of getMoAttributes/3 extended.
%% R5A/1   2016-01-08 etxpeno  add log_security/4 (TR HU49263)
%% R5A/2   2016-04-13 etxberb  Changed return format of is_attribute/3 for
%%                             enum & struct.
%% R6A/1   2016-04-21 erarafo  getMoAttributes/3 reverted to old return format.
%% R6A/2   2016-04-27 erarafo  Allow for is_attribute returning 'undefined'
%% ===================
%% R8A/1   2017-01-19 etxpeno  Added is_com_started/0
%% ===================
%% R9A/1-2 2017-02-16 etxpeno  Correction in to_old_format/3
%% R9A/3   2017-02-21 etxtory  Added get_app_log_dir
%% R11A/2-4 2017-10-02 eralils Added getHttpsPorts, updated lookup_user
%%                             and added createFakeUser.
%% R11A/5  2017-10-02 eralils  Added test functions: createFakeMaintenanceUser,
%%                             deleteFakeMaintenanceUser and deleteFakeUser.
%% R11A/6  2017-10-17 etxpeno  OTP 20 fixes

%% ------- ---------- -------  ---END-OF-LOG-----------------------------------

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 2.    MODULE BORDER LINE AND INTERNAL DEFINITIONS
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 2.1.1 Interface functions
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # Basic Interface (Corresponds to the COM / COMTE interface)
%%% ###---------------------------------------------------------------------###

-compile(inline_list_funcs).

%% Transaction exports
-export([new_transaction_id/0,
 	 new_transaction_id/1,
 	 new_transaction_id/2,
 	 join/1,
	 join_new/0,
	 join_new/1,
	 join_new/2,
 	 validate/1,
 	 prepare/1,
 	 commit/1,
 	 abort_transaction/1,
 	 finish/1]).

%% MO exports
-export([setMoAttributes/3,
   	 getMoAttributes/3,
   	 getMoIterator/3,
   	 createMo/6,
   	 deleteMo/2,
   	 existsMo/2,
   	 countMoChildren/3,
   	 action/4]).


%%% ###---------------------------------------------------------------------###
%%% # Extended Interface
%%% ###---------------------------------------------------------------------###
%% MIM
-export([getMimFiles/0,
	 getMimClass/1,
	 getMimClass/2,
	 getMimClassPath/1,
	 getMimTree/1,
	 getMimTree/2,
	 mimVal_to_mibStr/2]).

%% Transaction wrappers
-export([getMoTree/1,
	 getMoTree/2,
	 getMoTree/3]).

%% Logging
-export([log_availability_hw/6,
	 log_availability_hw/7,
	 log_availability_hw/8,
	 log_availability_node/3,
	 log_availability_node/4,
	 log_availability_node/5,
	 log_availability_node/6,
	 log_availability_other/3,
	 log_availability_other/4,
	 log_availability_other/5,
	 log_availability_pgm/6,
	 log_availability_pgm/7,
	 log_availability_pgm/8,
	 log_availability_piu/6,
	 log_availability_piu/7,
	 log_availability_piu/8,
	 log_availability_service/5,
	 log_availability_service/6,
	 log_availability_service/7,
	 log_security/3,
	 log_security/4]).

%% Notifications
-export([subscribe/1]).
-export([subscribe/2]).
-export([unsubscribe/1]).
-export([unsubscribe/2]).
-export([get_subscriptions/1]).

%% Security exports
-export([lookup/1,
		 lookup/2,
         getHttpsPorts/0]).

%% Miscellaneous
-export([get_app_log_dir/1,
	 is_attribute/3,
	 keyfind/2,
	 keyfind_all/2,
	 pairElems/2]).

-export([is_com_started/0]).

%%% ###---------------------------------------------------------------------###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%% For test purposes:
-export([get_transaction_application/1,
	 get_transaction_number/1,
	 get_transaction_user/1]).

%% For test purposes (for testing lookup function):
-export([createFakeUser/0]).
-export([createFakeMaintenanceUser/0]).
-export([deleteFakeMaintenanceUser/0]).
-export([deleteFakeUser/0]).

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###
-include("coi.hrl").

%%% ###=====================================================================###
%%% # 2.4   LOCAL DEFINITION OF MACROS
%%% ###---------------------------------------------------------------------###
-define(LowestTreeLevel_WithDepthLimit, 2).

%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

-define(STACKTRACE_C,   % Current stacktrace
	element(2, process_info(self(), current_stacktrace))).

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###
%% TransactionId:
-record(?MODULE, {appl,
		  user,
		  transNo}).

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 3.1.1 Basic Interface (Corresponds to the COM / COMTE interface)
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 3.1.1.1 Transaction functions
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Generate a new transaction identity.
%%%   DEPRECATED: Use new_transaction_id/1 or new_transaction_id/2 instead.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec new_transaction_id() ->
    coi_transaction_id().
%%% ###=====================================================================###
new_transaction_id() ->
    new_transaction_id(undefined).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Generate a new transaction identity.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec new_transaction_id(ApplName :: atom() | string(),
			 User     :: atom() | string()) ->
    coi_transaction_id().
%%% ###=====================================================================###
new_transaction_id(ApplName, User) ->
    new_transaction_id(ApplName, User, coiServer:new_transIdNo()).

%%% ###########################################################################
%%% @equiv new_transaction_id(MODULE, User)
-spec new_transaction_id(User :: atom() | string()) ->
    coi_transaction_id().
%%% ###=====================================================================###
new_transaction_id(User) ->
    new_transaction_id(get_previous_module(), User).

new_transaction_id(ApplName, User, TransIdNo) ->
    TransId = #?MODULE{appl = ApplName, user = User, transNo = TransIdNo},
    case comsaTransactionServer:reserve(TransId) of
	true ->
	    TransId;
	false ->
	    TransIdPattern =
		#?MODULE{appl = ApplName, user = '_', transNo = '_'},
	    case coiServer:is_max_limit_reached(TransIdPattern) of
		false ->
		    new_transaction_id(ApplName,
				       User,
				       coiServer:new_transIdNo());
		true ->
		    Result = <<"All transaction identities occupied.">>,
		    Stack = ?STACKTRACE_C,
		    ErrorInfo =
			[Result,
			 {no_more_available_ids_for, ApplName}],
		    error_logger:error_report(ErrorInfo ++ Stack),
		    throw({error, Result})
	    end
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Start a new transaction using a TransactionId generated by
%%%   the function new_transaction_id.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec join(TransId :: coi_transaction_id()) ->
    ok | {error, error_reason()}.
%%% ###=====================================================================###
join(TransId) ->
    comsaTransactionServer:join(TransId).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc DEPRECATED: Use join_new/1 or join_new/2 instead.
%%%
%%% @end
%%% ----------------------------------------------------------
%%% @equiv join_new(MODULE, undefined)
-spec join_new() ->
    {ok, TransId :: coi_transaction_id()} |
    {error, error_reason()}.
%%% ###=====================================================================###
join_new() ->
    join_new(undefined).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Select a new transaction Id and start a new transaction.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec join_new(ApplicationName :: atom() | string(),
	       User            :: atom() | string()) ->
    {ok, TransId :: coi_transaction_id()} |
    {error, error_reason()}.
%%% ###=====================================================================###
join_new(ApplicationName, User) ->
    TransId = new_transaction_id(ApplicationName, User),
    case comsaTransactionServer:join(TransId) of
	ok ->
	    {ok, TransId};
	Error ->
	    Error
    end.

%%% ###########################################################################
%%% @equiv join_new(MODULE, User)
-spec join_new(User :: atom() | string()) ->
    {ok, TransId :: coi_transaction_id()} |
    {error, error_reason()}.
%%% ###=====================================================================###
join_new(User) ->
    join_new(get_previous_module(), User).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Validate a transaction.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec validate(TransId :: coi_transaction_id()) ->
    {CommandCompletion::ok, Result::boolean()} |
    {CommandCompletion::ok, Result::boolean(), error_reason()} |
    {CommandCompletion::error, error_reason()}.
%%% ###=====================================================================###
validate(TransId) ->
    comsaTransactionServer:validate(TransId).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Prepare a transaction for commit.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec prepare(TransId :: coi_transaction_id()) ->
    ok | {error, error_reason()}.
%%% ###=====================================================================###
prepare(TransId) ->
    log_AuditTrail_commit(TransId),   % COMSA implements prepare as commit!
    comsaTransactionServer:prepare(TransId).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Commit a transaction.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec commit(TransId :: coi_transaction_id()) ->
    ok | {error, error_reason()}.
%%% ###=====================================================================###
commit(TransId) ->
    comsaTransactionServer:commit(TransId).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Abort a transaction.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec abort_transaction(TransId :: coi_transaction_id()) ->
    ok | {error, error_reason()}.
%%% ###=====================================================================###
abort_transaction(TransId) ->
    log_AuditTrail_abort(TransId),
    comsaTransactionServer:abort_transaction(TransId).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Finish a transaction.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec finish(TransId :: coi_transaction_id()) ->
    ok | {error, error_reason()}.
%%% ###=====================================================================###
finish(TransId) ->
    comsaTransactionServer:finish(TransId).

%%% ###---------------------------------------------------------------------###
%%% # 3.1.1.2 MO functions
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% ----------------------------------------------------------
%% @doc Set the attributes given by the <i>NamedAttrs</i> to their corresponding
%%   value or if it is set to undefined, then the attribute is to be
%%   unset.
%%
%% This command has to be side effect free as it will be called multiple times.
%% @end
%%% ----------------------------------------------------------
-spec setMoAttributes(TransId :: coi_transaction_id(),
		      ECIM_Dn :: ecim_dn(),
		      NamedAttrs :: list(coi_named_attribute())) ->
    ok | {error, error_reason()}.
%%% ###=====================================================================###
setMoAttributes(TransId, ECIM_Dn, NamedAttrs) ->
    log_AuditTrail_set(TransId, ECIM_Dn, NamedAttrs),
    comsaTransactionServer:setMoAttributes(TransId, ECIM_Dn, NamedAttrs).

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @doc Get the attributes given by the <i>ECIM_Dn</i> and specified attribute
%% names.
%%
%% A list of COI values is returned, representing the value of each attribute.
%% "No value" is represented as [] and may occur as attribute values and also
%% as struct member values.
%%
%% A value may be simple or structured. The most complex cases are "sequence
%% of struct" and "struct with members that are sequences". In either case the
%% bottom values are simple values.
%%
%% The special atom 'undefined' is returned for an attribute whose name is unknown,
%% or if the specified instance does not exist.
%%
%% Modules that depend on this function:
%%   comsaEvent
%%   comsaLedControl
%%   swmBoardList
%%   EM GUI
%%
%% The ootComSysM module previously used coi:getMoAttributes/3 but now calls
%% comsaTransactionServer:getMoAttributes/3 instead. Consider using
%% coi:getMoAttributes/3 again.
%% @end
%%% ----------------------------------------------------------
-spec getMoAttributes(TransId :: coi_transaction_id(),
		      ECIM_Dn :: ecim_dn(),
		      Names :: list(mo_attribute_name())) ->
	  list(coi_value_restr() | undefined) |
	  {error, error_reason()}.

%%% ###=====================================================================###
getMoAttributes(TransId, ECIM_Dn, Names) ->
    case comsaTransactionServer:getMoAttributes(TransId, ECIM_Dn, Names) of
	{error, _}=E ->
	    E;
	Result ->
	    try
	        to_old_format(Result, Names, ECIM_Dn, [])
	    catch
		throw:is_attribute_no_conclusion_1 ->
		    {error, <<"is_attribute_no_conclusion_1">>};
		throw:is_attribute_no_conclusion_2 ->
		    {error, <<"is_attribute_no_conclusion_2">>};
		throw:Reason ->
		    Stack = erlang:get_stacktrace(),
		    sysInitI:error_msg(
		      "ECIM_Dn: ~p, names: ~p, prel: ~p, reason: ~p~nstack: ~p~n",
		      [ECIM_Dn, Names, Result, Reason, Stack]),
		    {error, Reason}
	    end
    end.

%%% ###=====================================================================###
to_old_format([Attr|Attrs], [Name|Names], ECIM_Dn, Acc) ->
    R = to_old_format(Attr, ECIM_Dn, Name),
    to_old_format(Attrs, Names, ECIM_Dn, [R|Acc]);

to_old_format([], [], _ECIM_Dn, Acc) ->
    lists:reverse(Acc);

to_old_format(_Attrs, _Names, _ECIM_Dn, _Acc) ->
    throw(<<"Attribute lookup failure">>).

%%% ###=====================================================================###
to_old_format({_, _} = Attr, _, _) ->
    %% already in correct format
    Attr;

to_old_format([{_, _}|_] = Attr, _, _) ->
    %% already in correct format
    Attr;

to_old_format([Type], _, _) when is_integer(Type) ->
    % no value
    [];

to_old_format([?STRUCT, StructMembers], ECIM_Dn, Name) ->
    % one struct instance; need sequence info for attribute and struct members
    ClassPath = to_classpath(ECIM_Dn),
    AttrInfo = is_attribute(sequence, ClassPath, binary_to_list(Name)),
    case AttrInfo of
	undefined ->
	    throw(is_attribute_no_conclusion_1);
	{false, {struct, AttrInfoList}} ->
	    % attribute is simple
	    {?STRUCT,
	     lists:foldr(
	       fun({SMName, [_T]}, Acc) ->
		       % struct member has no value
		       [{SMName, []}|Acc];
		  ({SMName, [T, X]}, Acc) ->
		       % struct member has one value
		       SMNameS = binary_to_list(SMName),
		       case proplists:get_value(SMNameS, AttrInfoList, false) of
			   false ->
			       % struct member is simple
			       [{SMName, {T, X}}|Acc];
			   true ->
			       % struct member is sequence
			       [{SMName, [{T, X}]}|Acc];
			   {false, {enum, _EnumMemberTypes}} ->
			       % enum member is simple
			       [{SMName, {T, X}}|Acc];
			   {true, {enum, _EnumMemberTypes}} ->
			       % enum member is sequence
			       [{SMName, [{T, X}]}|Acc]
		       end;
		  ({SMName, [T|Values]}, Acc) ->
		       % struct member is sequence, lookup not needed
		       [{SMName, [{T, U}||U<-Values]}|Acc]
	       end,
	       [],
	       StructMembers)};
	{true, {struct, StructMemberInfo}} ->
	    % the attribute is sequence of struct with just one element
	    lists:foreach(
	      fun({_StructMemberName, false}) ->
		      ok;
		 ({_StructMemberName, true}) ->
		      % struct member must not be sequence here
		      throw(<<"Attribute too complex (1)">>);
		 ({_StructMemberEnum, {false, {enum, _MemberTypeInfo}}}) ->
		      ok;
		 ({_StructMemberEnum, {true, {enum, _MemberTypeInfo}}}) ->
		      % struct member must not be sequence here
		      throw(<<"Attribute too complex (2)">>)
	      end,
	      StructMemberInfo),
	    [{?STRUCT,
	      lists:foldr(
		fun({SMName, [_T]}, Acc) ->
			[{SMName, []}|Acc];
		   ({SMName, [T, V]}, Acc) ->
			[{SMName, {T, V}}|Acc]
		end,
		[],
		StructMembers)}]
    end;

to_old_format([?STRUCT | Structs], _ECIM_Dn, _Names) ->
    % sequence of struct, more than one element
    [{?STRUCT,
      lists:foldr(
	fun({SMName, [_T]}, Acc) ->
		[{SMName, []}|Acc];
	   ({SMName, [T, V]}, Acc) ->
		[{SMName, {T, V}}|Acc];
	   ({_SMName, [_T, _, _|_]}, _) ->
		throw(<<"Attribute too complex (3)">>)
	end,
	[],
	StructMembers)}
    || StructMembers <- Structs];

to_old_format([Type, Value], ECIM_Dn, Name) when is_integer(Type) ->
    % simple value or sequence of simple values with just one element
    ClassPath = to_classpath(ECIM_Dn),
    case is_attribute(sequence, ClassPath, binary_to_list(Name)) of
	false ->
	    {Type, Value};
	true ->
	    [{Type, Value}];
	{false, {enum, _EnumMembers}} ->
	    {Type, Value};
	{true, {enum, _EnumMembers}} ->
	    [{Type, Value}];
	undefined ->
	    throw(is_attribute_no_conclusion_2)
    end;

to_old_format([Type | Values], _, _) when is_integer(Type) ->
    % sequence of simple values, more than one element
    [{Type, Value} || Value <- Values];

to_old_format(undefined, _, _) ->
    undefined;

to_old_format(_, _, _) ->
    % cannot happen
    throw(<<"COI error">>).

%%% ###=====================================================================###
to_classpath(Dn) ->
    lists:flatten(
      ["/"++string:substr(Rdn, 1, string:str(Rdn, "=")-1)
	 ||Rdn <- string:tokens(binary_to_list(Dn), ",")]).

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @doc Get the Managed Object children (instances) given by the <i>ECIM_Dn</i>
%%   and <i>ClassName</i>.
%%
%% @end
%%% ----------------------------------------------------------
-spec getMoIterator(TransId :: coi_transaction_id(),
		    ECIM_Dn :: ecim_dn(),
		    ClassName :: mo_name()) ->
    list(mo_value()) |
	{error, error_reason()}.
%%% ###=====================================================================###
getMoIterator(TransId, ECIM_Dn, ClassName) ->
    comsaTransactionServer:getMoIterator(TransId, ECIM_Dn, ClassName).

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @doc Create a Managed Object child (instance) given by the
%%   <i>Parent_ECIM_Dn</i> and <i>ClassName</i>.
%% The <i>ClassId</i> will be set to the given <i>Value</i>.
%%
%% Attributes can be set directly in this function by the <i>ClassId</i>
%%
%% @end
%%% ----------------------------------------------------------
-spec createMo(TransId :: coi_transaction_id(),
	       Parent_ECIM_Dn :: ecim_dn(),
	       ClassName :: mo_name(),
	       ClassId :: mo_id_name(),
	       Value :: mo_child(),
	       NamedAttrs :: list(coi_named_attribute())) ->
    ok |
	{error, error_reason()}.
%%% ###=====================================================================###
createMo(TransId, Parent_ECIM_Dn, ClassName, ClassId, Value, NamedAttrs) ->
    log_AuditTrail_create(TransId, Parent_ECIM_Dn, ClassName, Value,NamedAttrs),
    comsaTransactionServer:createMo(TransId,
				    Parent_ECIM_Dn,
				    ClassName,
				    ClassId,
				    Value,
				    NamedAttrs).

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @doc Delete a Managed Object child (instance) given by the <i>ECIM_Dn</i>.
%%
%% @end
%%% ----------------------------------------------------------
-spec deleteMo(TransId :: coi_transaction_id(),
	       ECIM_Dn :: ecim_dn()) ->
    ok |
	{error, error_reason()}.
%%% ###=====================================================================###
deleteMo(TransId, ECIM_Dn) ->
    log_AuditTrail_delete(TransId, ECIM_Dn),
    comsaTransactionServer:deleteMo(TransId, ECIM_Dn).

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @doc Check if a Managed Object child (instance) given by the <i>ECIM_Dn</i>
%%   exists.
%%
%% @end
%%% ----------------------------------------------------------
-spec existsMo(TransId :: coi_transaction_id(),
	       ECIM_Dn :: ecim_dn()) ->
    boolean() |
	{error, error_reason()}.
%%% ###=====================================================================###
existsMo(TransId, ECIM_Dn) ->
    comsaTransactionServer:existsMo(TransId, ECIM_Dn).

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @doc Count number of Managed Object children (instances) given by the
%%   <i>ECIM_Dn</i> and <i>ClassName</i>.
%%
%% @end
%%% ----------------------------------------------------------
-spec countMoChildren(TransId :: coi_transaction_id(),
		      ECIM_Dn :: ecim_dn(),
		      ClassName :: mo_name()) ->
    integer() |
	{error, error_reason()}.
%%% ###=====================================================================###
countMoChildren(TransId, ECIM_Dn, ClassName) ->
    comsaTransactionServer:countMoChildren(TransId, ECIM_Dn, ClassName).

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @doc Perform a Managed Object action given by the <i>ECIM_Dn</i> and
%%   <i>Name</i>.
%%
%% The return value follows the same rules as getMoAttribute
%% except for `undefined' which means that no return should be given.
%%
%% @end
%%% ----------------------------------------------------------
-spec action(TransId :: coi_transaction_id(),
	     ECIM_Dn :: ecim_dn(),
	     Name :: mo_action_name(),
	     NamedAttrs :: list(coi_named_attribute())) ->
    coi_value() |
	[] |
	undefined |
	{error, error_reason()}.
%%% ###=====================================================================###
action(TransId, ECIM_Dn, Name, NamedAttrs) ->
    log_AuditTrail_action(TransId, ECIM_Dn, Name),
    comsaTransactionServer:action(TransId, ECIM_Dn, Name, NamedAttrs).

%%% ###---------------------------------------------------------------------###
%%% # 3.1.2 Extended Interface
%%% ###---------------------------------------------------------------------###
%%% ###---------------------------------------------------------------------###
%%% # 3.1.2.1 MIM
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Get a list of file names of all model files in the system.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec getMimFiles() ->
    list(coi_mim_file()).
%%% ###=====================================================================###
getMimFiles() ->
    {_, ProdRev} = lists:keyfind(productRevision,
				 1,
				 swmI:get_current_up_metadata()),
    ModelFileList = filename:join(filename:join(sysEnv:releases_dir(), ProdRev),
				  "comte/model_file_list.cfg"),
    case file:read_file(ModelFileList) of
	{ok, Bin} ->
	    string:tokens(binary_to_list(Bin), "\n");
	_ ->
	    []
    end.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Get model info about a MIM class.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec getMimClass(ClassName :: coi_mim_class_name()) ->
    coi_mim_info().
%%% ###=====================================================================###
getMimClass(ClassName) ->
    coiMim:getMimClass(ClassName).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Get model info about a MIM class. Return only elements matching the
%%%   <i>Key</i>.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec getMimClass(ClassName :: coi_mim_class_name(),
		  Key :: atom()) ->
    coi_mim_info().
%%% ###=====================================================================###
getMimClass(ClassName, Key) ->
    keyfind_all(getMimClass(ClassName), Key).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Get the unique class path for a MIM class name.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec getMimClassPath(ClassName :: coi_mim_class_name()) ->
    list(ecim_class_path()).
%%% ###=====================================================================###
getMimClassPath(ClassName) ->
    coiMim:getMimClassPath(ClassName).

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @doc Get the class tree given by the <i>ECIM_Dn</i>.
%%
%%   Returns a hiearchical list limited to the number of levels specified by
%%   <i>Depth</i>.
%%
%%   Default value of <i>Depth</i> is 1.
%%
%% @end
%%   Example:
%%   coi:getMimTree(<<"ManagedElement=1,TestRoot=1">>, 2) ->
%%     [{"TestRoot",
%%       [{"TestClass9",[]},
%%        {"TestClass8",[]},
%%        {"TestClass6",[]},
%%        {"TestClass5",[]},
%%        {"TestClass4",[]},
%%        {"TestClass2",[{"TestClass7",[]},{"TestClass3",[]}]},
%%        {"TestClass1",[]}]}]
%%% ----------------------------------------------------------
-spec getMimTree(ECIM_Dn :: ecim_dn(),
		 Depth :: integer() | all) ->
    list(coi_mim_tree()).
%%% ###=====================================================================###
getMimTree(ECIM_Dn, Depth) ->
    coiMim:getMimTree(ECIM_Dn, Depth).

%%% ###########################################################################
%%% @equiv getMimTree(ECIM_Dn, 1)
-spec getMimTree(ECIM_Dn :: ecim_dn()) ->
    list(coi_mim_tree()).
%%% ###=====================================================================###
getMimTree(ECIM_Dn) ->
    getMimTree(ECIM_Dn, 1).

%%% ###########################################################################
%%% ----------------------------------------------------------
%% @doc Translate from raw integer value in the MIM to the corresponding string
%%   defined in the MIB.
%%
%% NOTE 1: There is a difference between 'Attribute Name' and it's 'Data Type
%%         Name'. This function takes the 'Data Type Name' as first attribute.
%%         The two names are basically the same, but one starts with lower case
%%         and the other with upper case.
%%
%% NOTE 2: Supports only AttributeDataTypeName "ProbableCause" and "EventType".
%%
%% @end
%%% ----------------------------------------------------------
-spec mimVal_to_mibStr(AttributeDataTypeName :: string(),
		       AttributeValue :: integer()) ->
    string().
%%% ###=====================================================================###
mimVal_to_mibStr(AttributeDataTypeName, AttributeValue) ->
    coiMib:mimVal_to_mibStr(AttributeDataTypeName, AttributeValue).

%%% ###---------------------------------------------------------------------###
%%% # 3.1.2.2 Transaction wrappers
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% ----------------------------------------------------------
%% @doc Get the Managed Object children (instances) given by the <i>ECIM_Dn</i>.
%%  Returns a hiearchical list limited to the number of levels specified by
%%  <i>Depth</i>.
%%
%%   Default value of <i>Depth</i> is 1.
%%
%%   <i>Depth</i> is limited to 1 for the top ECIM_Dn level ("ManagedElement=1")
%%   and limited to 2 for the 2nd ECIM_Dn level (e.g.
%%   "ManagedElement=1,TestRoot=1").
%%
%% @end
%%   Example:
%%   coi:getMoTree(<<"ManagedElement=1,TestRoot=1">>, 2) ->
%%     [{{"TestRoot",{9,<<"1">>}},
%%       [{{"TestClass2",{9,<<"1">>}},
%%         [{{"TestClass7",{9,<<"1">>}},[]},
%%          {{"TestClass3",{9,<<"1">>}},[]}]},
%%        {{"TestClass2",{9,<<"2">>}},
%%         [{{"TestClass7",{9,<<"1">>}},[]},
%%          {{"TestClass7",{9,<<"2">>}},[]},
%%          {{"TestClass3",{9,<<"1">>}},[]}]},
%%        {{"TestClass1",{9,<<"1">>}},[]}]}]
%%% ----------------------------------------------------------
-spec getMoTree(ECIM_Dn :: ecim_dn(),
		Depth :: integer() | all,
		TransId :: coi_transaction_id()) ->
    list(coi_mo_tree()) |
	{error, error_reason()}.
%%% ###=====================================================================###
getMoTree(ECIM_Dn, Depth, TransId) ->
    try
	getMoTree_go(ECIM_Dn, Depth, TransId)
    catch
	throw : {?MODULE, Reason} ->
	    {error, Reason};
	  ErrClass : ErrReason ->
	    error_logger:error_report([{?MODULE, getMoTree},
				       {ErrClass, ErrReason},
				       {stacktrace, erlang:get_stacktrace()}]),
	    {error, <<"Unknown reason">>}
    end.

%%% ###########################################################################
%%% @equiv getMoTree(ECIM_Dn, Depth, TransId)
-spec getMoTree(ECIM_Dn :: ecim_dn(),
		Depth :: integer() | all) ->
    list(coi_mo_tree()) |
	{error, error_reason()}.
%%% ###=====================================================================###
getMoTree(ECIM_Dn, Depth) ->
    {ok, TransId} = join_new(),
    Result = getMoTree(ECIM_Dn, Depth, TransId),
    finish(TransId),
    Result.

%%% ###########################################################################
%%% @equiv getMoTree(ECIM_Dn, 1)
-spec getMoTree(ECIM_Dn :: ecim_dn()) ->
    list(coi_mo_tree()) |
	{error, error_reason()}.
%%% ###=====================================================================###
getMoTree(ECIM_Dn) ->
    getMoTree(ECIM_Dn, 1).

getMoTree_go(ECIM_Dn, Depth, TransId) ->
    Dn = sysUtil:term_to_string(ECIM_Dn),
    ClassNames = validate_ecimDn(string:tokens(Dn, ",=")),
    {Parents, [ClassName, Value]} = lists:split(length(ClassNames) - 2,
						ClassNames),
    TreeLevel = (length(ClassNames) div 2),
    if
	TreeLevel =< ?LowestTreeLevel_WithDepthLimit andalso
	Depth     > TreeLevel ->
	    Reason =
		"The tree depth is limited to " ++
		integer_to_list(TreeLevel) ++
		" for ECIM_Dn level " ++
		integer_to_list(TreeLevel) ++
		". Lowest ECIM_Dn level with limitation on Depth is " ++
		integer_to_list(?LowestTreeLevel_WithDepthLimit) ++
		".",
	    throw({?MODULE, list_to_binary(Reason)});
	?ELSE ->
	    ok
    end,
    [{ClassName, NextLevel} | _] = getMimTree(ECIM_Dn, Depth),
    case getMoIterator(TransId, to_ecimDn(Parents), list_to_binary(ClassName))
	of
	MoValues when is_list(MoValues) ->
	    case lists:keyfind(list_to_binary(Value), 2, MoValues) of
		MoValue when is_tuple(MoValue) ->
		    [{{ClassName, MoValue},
		      lists:flatten(getMoTree_loop(NextLevel, TransId, Dn))}];
		false ->
		    [{{ClassName, undefined}, []}]
	    end;
	MoValue ->
	    [{{ClassName, MoValue},
	      lists:flatten(getMoTree_loop(NextLevel, TransId, Dn))}]
    end.

getMoTree_loop([{Name, NextLevel} | Tail], TransId, Dn) ->
    [begin
	 case getMoIterator(TransId, to_ecimDn(Dn), Name) of
	     MoValues when is_list(MoValues) ->
		 MoValues;
	     _ ->
		 MoValues = []
	 end,
	 [{{Name, MoValue},
	   lists:flatten(getMoTree_loop(NextLevel,
					TransId,
					add_className(Dn, Name, Value)))}
	  || {_DataType, Value} = MoValue <- MoValues]
     end
     | getMoTree_loop(Tail, TransId, Dn)];
getMoTree_loop([], _, _) ->
    [].

%%% ###---------------------------------------------------------------------###
%%% # 3.1.2.3 Logging
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Write HW Event to the Availability Log.
%%%
%%% See also ../ALH_CXA11457/doc/15519/AvliIwd.doc for detailed description of
%%% the Availability Log Interface (AVLI).
%%%
%%% @end
%%% ----------------------------------------------------------
-spec log_availability_hw(TimeStamp     :: now_ts(),
			  ServiceStatus :: service_status(),
			  Reason        :: reason(),
			  HwType        :: hw_type(),
			  HwAddress     :: hw_address(),
			  HwPid         :: hw_pid(),
			  AddInfo       :: add_info(),
			  ExecType      :: sync | async
			 ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_hw(TimeStamp,
		    ServiceStatus,
		    Reason,
		    HwType,
		    HwAddress,
		    HwPid,
		    AddInfo,
		    ExecType) ->
    alhI:write_hw_event(TimeStamp,
			ServiceStatus,
			Reason,
			HwType,
			HwAddress,
			HwPid,
			AddInfo,
			ExecType).

%%% ###########################################################################
%%% @equiv log_availability_hw(TimeStamp,
%%%                            ServiceStatus,
%%%                            Reason,
%%%                            HwType,
%%%                            HwAddress,
%%%                            HwPid,
%%%                            AddInfo,
%%%                            sync)
-spec log_availability_hw(TimeStamp :: now_ts(),
			  ServiceStatus :: service_status(),
			  Reason :: reason(),
			  HwType :: hw_type(),
			  HwAddress :: hw_address(),
			  HwPid :: hw_pid(),
			  AddInfo :: add_info()
			 ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_hw(TimeStamp,
		    ServiceStatus,
		    Reason,
		    HwType,
		    HwAddress,
		    HwPid,
		    AddInfo) ->
    alhI:write_hw_event(TimeStamp,
			ServiceStatus,
			Reason,
			HwType,
			HwAddress,
			HwPid,
			AddInfo).

%%% ###########################################################################
%%% @equiv log_availability_hw(os:timestamp(),
%%%                            ServiceStatus,
%%%                            Reason,
%%%                            HwType,
%%%                            HwAddress,
%%%                            HwPid,
%%%                            AddInfo)
-spec log_availability_hw(ServiceStatus :: service_status(),
			  Reason :: reason(),
			  HwType :: hw_type(),
			  HwAddress :: hw_address(),
			  HwPid :: hw_pid(),
			  AddInfo :: add_info()
			 ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_hw(ServiceStatus, Reason, HwType, HwAddress, HwPid, AddInfo) ->
    alhI:write_hw_event(ServiceStatus,
			Reason,
			HwType,
			HwAddress,
			HwPid,
			AddInfo).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Write Node Event to the Availability Log.
%%%
%%% See also ../ALH_CXA11457/doc/15519/AvliIwd.doc for detailed description of
%%% the Availability Log Interface (AVLI).
%%%
%%% @end
%%% ----------------------------------------------------------
-spec log_availability_node(TimeStamp     :: now_ts(),
			    ServiceStatus :: service_status(),
			    Reason        :: reason(),
			    EventId       :: event_id(),
			    AddInfo       :: add_info(),
			    ExecType      :: sync | async
			   ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_node(TimeStamp,
		      ServiceStatus,
		      Reason,
		      EventId,
		      AddInfo,
		      ExecType) ->
    alhI:write_node_event(TimeStamp,
			  ServiceStatus,
			  Reason,
			  EventId,
			  AddInfo,
			  ExecType).

%%% ###########################################################################
%%% @equiv log_availability_node(TimeStamp,
%%%                              ServiceStatus,
%%%                              Reason,
%%%                              EventId,
%%%                              AddInfo,
%%%                              sync)
-spec log_availability_node(TimeStamp     :: now_ts(),
			    ServiceStatus :: service_status(),
			    Reason        :: reason(),
			    EventId       :: event_id(),
			    AddInfo       :: add_info()
			   ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_node(TimeStamp, ServiceStatus, Reason, EventId, AddInfo) ->
    alhI:write_node_event(TimeStamp,
			  ServiceStatus,
			  Reason,
			  EventId,
			  AddInfo).

%%% ###########################################################################
%%% @equiv log_availability_node(TimeStamp, ServiceStatus, Reason, 0, AddInfo)
-spec log_availability_node(TimeStamp     :: now_ts(),
			    ServiceStatus :: service_status(),
			    Reason        :: reason(),
			    AddInfo       :: add_info()
			   ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_node(TimeStamp, ServiceStatus, Reason, AddInfo) ->
    alhI:write_node_event(TimeStamp, ServiceStatus, Reason, AddInfo).

%%% ###########################################################################
%%% @equiv log_availability_node(os:timestamp(), ServiceStatus, Reason, AddInfo)
-spec log_availability_node(ServiceStatus :: service_status(),
			    Reason        :: reason(),
			    AddInfo       :: add_info()
			   ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_node(ServiceStatus, Reason, AddInfo) ->
    alhI:write_node_event(ServiceStatus, Reason, AddInfo).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Write Other Event to the Availability Log.
%%%
%%% See also ../ALH_CXA11457/doc/15519/AvliIwd.doc for detailed description of
%%% the Availability Log Interface (AVLI).
%%%
%%% @end
%%% ----------------------------------------------------------
-spec log_availability_other(TimeStamp        :: now_ts(),
			     ServiceStatus    :: service_status(),
			     Reason           :: reason(),
			     AvailabilityInfo :: avail_info(),
			     ExecType         :: sync | async
			    ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_other(TimeStamp,
		       ServiceStatus,
		       Reason,
		       AvailabilityInfo,
		       ExecType) ->
    alhI:write_other_event(TimeStamp,
			   ServiceStatus,
			   Reason,
			   AvailabilityInfo,
			   ExecType).

%%% ###########################################################################
%%% @equiv log_availability_other(TimeStamp,
%%%                               ServiceStatus,
%%%                               Reason,
%%%                               AvailabilityInfo,
%%%                               sync)
-spec log_availability_other(TimeStamp :: now_ts(),
			     ServiceStatus :: service_status(),
			     Reason :: reason(),
			     AvailabilityInfo :: avail_info()
			    ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_other(TimeStamp, ServiceStatus, Reason, AvailabilityInfo) ->
    alhI:write_other_event(TimeStamp, ServiceStatus, Reason, AvailabilityInfo).

%%% ###########################################################################
%%% @equiv log_availability_other(os:timestamp(),
%%%                               ServiceStatus,
%%%                               Reason,
%%%                               AvailabilityInfo)
-spec log_availability_other(ServiceStatus :: service_status(),
			     Reason :: reason(),
			     AvailabilityInfo :: avail_info()
			    ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_other(ServiceStatus, Reason, AvailabilityInfo) ->
    alhI:write_other_event(ServiceStatus, Reason, AvailabilityInfo).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Write PIU Event to the Availability Log.
%%%
%%% See also ../ALH_CXA11457/doc/15519/AvliIwd.doc for detailed description of
%%% the Availability Log Interface (AVLI).
%%%
%%% @end
%%% ----------------------------------------------------------
-spec log_availability_piu(TimeStamp     :: now_ts(),
			   ServiceStatus :: service_status(),
			   Reason        :: reason(),
			   PiuType       :: piu_type(),
			   PiuHwAddr     :: piu_hw_addr(),
			   HwPid         :: hw_pid(),
			   AddInfo       :: add_info(),
			   ExecType      :: sync | async
			  ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_piu(TimeStamp,
		     ServiceStatus,
		     Reason,
		     PiuType,
		     PiuHwAddr,
		     HwPid,
		     AddInfo,
		     ExecType) ->
    alhI:write_piu_event(TimeStamp,
			 ServiceStatus,
			 Reason,
			 PiuType,
			 PiuHwAddr,
			 HwPid,
			 AddInfo,
			 ExecType).

%%% ###########################################################################
%%% @equiv log_availability_piu(TimeStamp,
%%%                             ServiceStatus,
%%%                             Reason,
%%%                             PiuType,
%%%                             PiuHwAddr,
%%%                             HwPid,
%%%                             AddInfo,
%%%                             sync)
-spec log_availability_piu(TimeStamp :: now_ts(),
			   ServiceStatus :: service_status(),
			   Reason :: reason(),
			   PiuType :: piu_type(),
			   PiuHwAddr :: piu_hw_addr(),
			   HwPid :: hw_pid(),
			   AddInfo :: add_info()
			  ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_piu(TimeStamp,
		     ServiceStatus,
		     Reason,
		     PiuType,
		     PiuHwAddr,
		     HwPid,
		     AddInfo) ->
    alhI:write_piu_event(TimeStamp,
			 ServiceStatus,
			 Reason,
			 PiuType,
			 PiuHwAddr,
			 HwPid,
			 AddInfo).

%%% ###########################################################################
%%% @equiv log_availability_piu(os:timestamp(),
%%%                             ServiceStatus,
%%%                             Reason,
%%%                             PiuType,
%%%                             PiuHwAddr,
%%%                             HwPid,
%%%                             AddInfo)
-spec log_availability_piu(ServiceStatus :: service_status(),
			   Reason :: reason(),
			   PiuType :: piu_type(),
			   PiuHwAddr :: piu_hw_addr(),
			   HwPid :: hw_pid(),
			   AddInfo :: add_info()
			  ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_piu(ServiceStatus,
		     Reason,
		     PiuType,
		     PiuHwAddr,
		     HwPid,
		     AddInfo) ->
    alhI:write_piu_event(ServiceStatus,
			 Reason,
			 PiuType,
			 PiuHwAddr,
			 HwPid,
			 AddInfo).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Write PGM Event to the Availability Log.
%%%
%%% See also ../ALH_CXA11457/doc/15519/AvliIwd.doc for detailed description of
%%% the Availability Log Interface (AVLI).
%%%
%%% @end
%%% ----------------------------------------------------------
-spec log_availability_pgm(TimeStamp     :: now_ts(),
			   ServiceStatus :: service_status(),
			   Reason        :: reason(),
			   PiuType       :: piu_type(),
			   PiuHwAddr     :: piu_hw_addr(),
			   SwPid         :: sw_pid(),
			   AddInfo       :: add_info(),
			   ExecType      :: sync | async
			  ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_pgm(TimeStamp,
		     ServiceStatus,
		     Reason,
		     PiuType,
		     PiuHwAddr,
		     SwPid,
		     AddInfo,
		     ExecType) ->
    alhI:write_pgm_event(TimeStamp,
			 ServiceStatus,
			 Reason,
			 PiuType,
			 PiuHwAddr,
			 SwPid,
			 AddInfo,
			 ExecType).

%%% ###########################################################################
%%% @equiv log_availability_pgm(TimeStamp,
%%%                             ServiceStatus,
%%%                             Reason,
%%%                             PiuType,
%%%                             PiuHwAddr,
%%%                             SwPid,
%%%                             AddInfo,
%%%                             sync)
-spec log_availability_pgm(TimeStamp :: now_ts(),
			   ServiceStatus :: service_status(),
			   Reason :: reason(),
			   PiuType :: piu_type(),
			   PiuHwAddr :: piu_hw_addr(),
			   SwPid :: sw_pid(),
			   AddInfo :: add_info()
			  ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_pgm(TimeStamp,
		     ServiceStatus,
		     Reason,
		     PiuType,
		     PiuHwAddr,
		     SwPid,
		     AddInfo) ->
    alhI:write_pgm_event(TimeStamp,
			 ServiceStatus,
			 Reason,
			 PiuType,
			 PiuHwAddr,
			 SwPid,
			 AddInfo).

%%% ###########################################################################
%%% @equiv log_availability_pgm(os:timestamp(),
%%%                             ServiceStatus,
%%%                             Reason,
%%%                             PiuType,
%%%                             PiuHwAddr,
%%%                             SwPid,
%%%                             AddInfo)
-spec log_availability_pgm(ServiceStatus :: service_status(),
			   Reason :: reason(),
			   PiuType :: piu_type(),
			   PiuHwAddr :: piu_hw_addr(),
			   SwPid :: sw_pid(),
			   AddInfo :: add_info()
			  ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_pgm(ServiceStatus,
		     Reason,
		     PiuType,
		     PiuHwAddr,
		     SwPid,
		     AddInfo) ->
    alhI:write_pgm_event(ServiceStatus,
			 Reason,
			 PiuType,
			 PiuHwAddr,
			 SwPid,
			 AddInfo).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Write Service Event to the Availability Log.
%%%
%%% See also ../ALH_CXA11457/doc/15519/AvliIwd.doc for detailed description of
%%% the Availability Log Interface (AVLI).
%%%
%%% @end
%%% ----------------------------------------------------------
-spec log_availability_service(TimeStamp       :: now_ts(),
			       ServiceStatus   :: service_status(),
			       Reason          :: reason(),
			       ServiceType     :: service_type(),
			       ServiceInstance :: service_inst(),
			       AddInfo         :: add_info(),
			       ExecType        :: sync | async
			      ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_service(TimeStamp,
			 ServiceStatus,
			 Reason,
			 ServiceType,
			 ServiceInstance,
			 AddInfo,
			 ExecType) ->
    alhI:write_service_event(TimeStamp,
			     ServiceStatus,
			     Reason,
			     ServiceType,
			     ServiceInstance,
			     AddInfo,
			     ExecType).

%%% ###########################################################################
%%% @equiv log_availability_service(TimeStamp,
%%%                                 ServiceStatus,
%%%                                 Reason,
%%%		                    ServiceType,
%%%                                 ServiceInstance,
%%%                                 AddInfo,
%%%                                 sync)
-spec log_availability_service(TimeStamp :: now_ts(),
			       ServiceStatus :: service_status(),
			       Reason :: reason(),
			       ServiceType :: service_type(),
			       ServiceInstance :: service_inst(),
			       AddInfo :: add_info()
			      ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_service(TimeStamp,
			 ServiceStatus,
			 Reason,
			 ServiceType,
			 ServiceInstance,
			 AddInfo) ->
    alhI:write_service_event(TimeStamp,
			     ServiceStatus,
			     Reason,
			     ServiceType,
			     ServiceInstance,
			     AddInfo).

%%% ###########################################################################
%%% @equiv log_availability_service(os:timestamp(),
%%%                                 ServiceStatus,
%%%                                 Reason,
%%%		                    ServiceType,
%%%                                 ServiceInstance,
%%%                                 AddInfo)
-spec log_availability_service(ServiceStatus :: service_status(),
			       Reason :: reason(),
			       ServiceType :: service_type(),
			       ServiceInstance :: service_inst(),
			       AddInfo::add_info()
			      ) ->
    avli_return().
%%% ###=====================================================================###
log_availability_service(ServiceStatus,
			 Reason,
			 ServiceType,
			 ServiceInstance,
			 AddInfo) ->
    alhI:write_service_event(ServiceStatus,
			     Reason,
			     ServiceType,
			     ServiceInstance,
			     AddInfo).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Write Event to the Security Log.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec log_security(Facility :: sec_facility(),
		   Severity :: sec_severity(),
		   Msg      :: sec_msg()
		  ) ->
    sec_return().
%%% ###=====================================================================###
log_security(Facility, Severity, Msg) ->
     logI:write_log("SecurityLog",
		    get_networkManagedElementId(),
		    Facility,
		    Severity,
		    os:timestamp(),
		    Msg).

-spec log_security(Facility :: sec_facility(),
		   Severity :: sec_severity(),
		   Msg      :: sec_msg(),
		   SrcIp    :: sec_ip()
		  ) ->
    sec_return().
%%% ###=====================================================================###
log_security(Facility, Severity, Msg, SrcIp) ->
     logI:write_log("SecurityLog",
		    SrcIp,
		    get_networkManagedElementId(),
		    Facility,
		    Severity,
		    os:timestamp(),
		    Msg).

%%% ###---------------------------------------------------------------------###
%%% # 3.1.2.4 Notifications
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Subscribe for COI Notifications.
%%%
%%%   The Module subscribing for COI Notifications must export coi_notify/1.
%%%   -spec coi_notify(Notification :: coi_notification_1()) ->
%%%       ok.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec subscribe(Module :: atom()) ->
    ok.
%%% ###=====================================================================###
subscribe(Module) ->
    coiEvent:subscribe(Module).

%%% ----------------------------------------------------------
%%% @doc Subscribe for COI Notifications.
%%%
%%%   The Module subscribing for COI Notifications must export coi_notify/1.
%%%   -spec coi_notify(Notification :: coi_notification_1()) ->
%%%       ok.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec subscribe(Module :: atom(),
	        MoClasses :: [string()] | all) ->
    ok | {error, string()}.
%%% ###=====================================================================###
subscribe(Module, all) ->
    coiEvent:subscribe(Module, all);
subscribe(Module, MoClasses) when is_list(MoClasses) ->
    coiEvent:subscribe(Module, MoClasses);
subscribe(_, _MoClasses) ->
    {error, "illegal MoClass definition"}.

%%% ----------------------------------------------------------
%%% @doc UnSubscribe all for COI Notifications.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec unsubscribe(Module :: atom()) ->
    ok | {error, string()}.
%%% ###=====================================================================###
unsubscribe(Module) when is_atom(Module) ->
    coiEvent:unsubscribe(Module, all);
unsubscribe(_) ->
    {error, "illegal Module definition"}.


%%% ----------------------------------------------------------
%%% @doc UnSubscribe for COI Notifications.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec unsubscribe(Module :: atom(), MoClasses :: [string()] | all) ->
    ok | {error, string()}.
%%% ###=====================================================================###
unsubscribe(Module, all) ->
    coiEvent:unsubscribe(Module, all);
unsubscribe(Module, MoClasses)  when is_list(MoClasses) ->
    coiEvent:unsubscribe(Module, MoClasses);
unsubscribe(_, _MoClasses) ->
    {error, "illegal MoClass definition"}.


%%% ----------------------------------------------------------
%%% @doc Get the current subscriptions for COI Notifications.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec get_subscriptions(Module :: atom()) ->
    [MoClass :: string()] | {error, string()}.
%%% ###=====================================================================###
get_subscriptions(Module) when is_atom(Module) ->
    coiEvent:get_subscriptions(Module);
get_subscriptions(_) ->
    {error, "illegal Module definition"}.


%%% ###---------------------------------------------------------------------###
%%% # 3.1.2.5 Security
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Get the https ports from the node.
%%%
%%%   Example:
%%%   > coi:getHttpsPorts().
%%%   [{https,443},{https_login,8443}]
%%% @end
%%% ----------------------------------------------------------
-spec getHttpsPorts() -> [tuple()].
%%% ###=====================================================================###
getHttpsPorts() ->
    coiSecurity:getHttpsPorts().


%%% ----------------------------------------------------------
%%% @doc Lookup the roles for the certificate user
%%%
%%%   Example maintenace user (without ldap lookup):
%%%     coi:lookup(Certificate).
%%%     {true,["EricssonSupport"]}
%%%
%%%   Example user (with ldap lookup):
%%%     coi:lookup(Certificate).
%%%     {true,["Rcs_Application_User",
%%%       "Rcs_Application_SecurityAdministrator",
%%%       "Rcs_Application_Administrator","ENodeB_Application_User",
%%%       "ENodeB_Application_SecurityAdministrator",
%%%       "ENodeB_Application_Administrator",
%%%       "SystemSecurityAdministrator","SystemAdministrator",
%%%       "expert"]}
%%%
%%% @end
%%% ----------------------------------------------------------
-spec lookup(Certificate::term()) ->
   {true, list(string())} | {false, string()}.
%%% ###=====================================================================###
lookup(Certificate) ->
	{User, __Subject} = certI:get_user(Certificate),
	coiSecurity:lookup_user(User).

%%% ----------------------------------------------------------
%%% @doc Lookup the roles for the user with password
%%%
%%%   Example maintenace user (without ldap lookup):
%%%     coi:lookup("labuser","Letmein01").
%%%     {true,["EricssonSupport"]}
%%%
%%%   Example user (with ldap lookup):
%%%     coi:lookup("super","super01").
%%%     {true,["Rcs_Application_User",
%%%       "Rcs_Application_SecurityAdministrator",
%%%       "Rcs_Application_Administrator","ENodeB_Application_User",
%%%       "ENodeB_Application_SecurityAdministrator",
%%%       "ENodeB_Application_Administrator",
%%%       "SystemSecurityAdministrator","SystemAdministrator",
%%%       "expert"]}
%%%
%%% @end
%%% ----------------------------------------------------------
-spec lookup(Username::string(), Password::string()) ->
   {true, list(string())} | {false, string()}.
%%% ###=====================================================================###
lookup(Username, Password) ->
    coiSecurity:lookup_user(Username, Password).



%%% ###---------------------------------------------------------------------###
%%% # 3.1.2.6 Miscellaneous
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Get the application log dir used for internal logging.
%%%   The function takes the caller's erlang application as input and the
%%%   returns the path to the log-directory where the caller can stored
%%%   own logs. Each application has an own directory and is responsible for
%%%   the contect of this directory.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec get_app_log_dir(App :: atom()) ->
      {ok, string()} | {error, error_reason()}.
get_app_log_dir(App) ->
    case code:lib_dir(App) of
	{error, Reason} ->
	    {error, Reason};
	LibDir ->
	    Split = filename:split(LibDir),
	    SplitLen = erlang:length(Split),
	    CxpDirNameRev = lists:nth(SplitLen-2, Split),
	    CxpDirName = remove_revision(CxpDirNameRev),
	    AppLogDir = filename:join([sysEnv:rcs_dir(),
				       "applicationlogs",
				       CxpDirName]),
	    {ok, AppLogDir}
    end.


remove_revision(CxpDirNameRev) ->
    %% remove _Rxxxx from end of filename
    string:join(
      lists:reverse(
	tl(
	  lists:reverse(
	    string:tokens(CxpDirNameRev, "_")
	   )
	 )
       )
	       ,"_").

%%% ----------------------------------------------------------
%%% @doc Find out if attribute(s) in a class have a certain property or not.
%%%
%%% NOTE: Different format of arguments gives different format of return value.
%%%       See examples and specification below.
%%%
%%%   Examples:
%%%   > coi:is_attribute(sequence,
%%%                      "/ManagedElement/SystemFunctions/Fm/FmAlarm",
%%%                      "additionalInfo").
%%%   true
%%%
%%%   > coi:is_attribute(sequence,
%%%                      "/ManagedElement/SystemFunctions/Fm/FmAlarm",
%%%                      ["probableCause", "additionalInfo", "invalidAttr"]).
%%%   [{"probableCause",false},
%%%    {"additionalInfo",true},
%%%    {"invalidAttr",undefined}]
%%%
%%%   > coi:is_attribute(sequence,
%%%                      "/ManagedElement/SystemFunctions/Fm/FmAlarm",
%%%                      all).
%%%   [{"fmAlarmId",false},
%%%    {"source",false},
%%%    {"lastEventTime",false},
%%%    {"sequenceNumber",false},
%%%    {"activeSeverity",false},
%%%    {"additionalText",false},
%%%    {"majorType",false},
%%%    {"minorType",false},
%%%    {"specificProblem",false},
%%%    {"eventType",false},
%%%    {"probableCause",false},
%%%    {"additionalInfo",true},
%%%    {"originalEventTime",false},
%%%    {"originalSeverity",false},
%%%    {"originalAdditionalText",false}]
%%%
%%%   > coi:is_attribute(sequence, "FmAlarm", "additionalInfo").
%%%   [{"/ManagedElement/SystemFunctions/Fm/FmAlarm",true}]
%%%
%%%   > coi:is_attribute(sequence, "FmAlarm", ["additionalInfo"]).
%%%   [{"/ManagedElement/SystemFunctions/Fm/FmAlarm",[{"additionalInfo",true}]}]
%%%
%%%   > coi:is_attribute(sequence, "TestClassA", ["xyz", "enum", "menum"]).
%%%   [{"/ManagedElement/TestRoot/TestClassA",
%%%     [{"xyz",
%%%       {false,{struct,[{"x",false},{"y",false},{"z",true}]}}},
%%%      {"enum",
%%%       {false,{enum,[{"V101",false},
%%%                     {"V117",false},
%%%                     {"V118",false}]}}},
%%%      {"menum",
%%%       {true,{enum,[{"V101",false},
%%%                    {"V117",false},
%%%                    {"V118",false}]}}}]}]
%%%
%%% @end
%%% ----------------------------------------------------------
-spec is_attribute(PropertyTag :: atom(),
		   ClassNameOrPath :: coi_mim_class_name(),
		   AttributeName_s :: string() | list(string()) | all) ->
    attribute_is() | undefined |
	list({AttrName :: string(), attribute_is() | undefined}) |
	list({UniqueClassPath :: string(), (attribute_is() | undefined |
					    list({AttrName :: string(),
						  (attribute_is() |
						   undefined)}))}).
%%% ###=====================================================================###
is_attribute(Prop,
	     [$/ | _] = ClassPath,
	     [E | _] = AttrNames) when is_list(E) ->
    [{AttrName, is_attribute(Prop, ClassPath, AttrName)}
     || AttrName <- AttrNames];
is_attribute(Prop,
	     [$/ | _] = ClassPath,
	     [E | _] = AttrName) when is_integer(E) ->
    case getMimClass(ClassPath) of
	[Class] ->
	    is_attribute_individual(Prop, Class, AttrName);
	[] ->
	    undefined
    end;
is_attribute(Prop, [$/ | _] = ClassPath, all) ->
    Attrs = keyfind(keyfind(getMimClass(ClassPath), attribute), name),
    AttrNames = [Name || {_, Name} <- Attrs],
    is_attribute(Prop, ClassPath, AttrNames);
is_attribute(Prop,
	     ClassName,
	     [E | _] = AttrNames) when is_list(E) ->
    [{ClassPath, [{AttrName, is_attribute_individual(Prop, Class, AttrName)}
		  || AttrName <- AttrNames]}
     || {ClassPath, Class}
	    <- lists:zip(getMimClassPath(ClassName),
			 getMimClass(ClassName))];
is_attribute(Prop,
	     ClassName,
	     [E | _] = AttrName) when is_integer(E) ->
    [{ClassPath, is_attribute_individual(Prop, Class, AttrName)}
     || {ClassPath, Class} <- lists:zip(getMimClassPath(ClassName),
					getMimClass(ClassName))];
is_attribute(Prop, ClassName, all) ->
    [begin
	 Attrs = keyfind(keyfind(Class, attribute), name),
	 AttrNames = [Name || {_, Name} <- Attrs],
	 {ClassPath, [{AttrName, is_attribute_individual(Prop, Class, AttrName)}
		      || AttrName <- AttrNames]}
     end
     || {ClassPath, Class}
	    <- lists:zip(getMimClassPath(ClassName),
			 getMimClass(ClassName))];
is_attribute(Prop, ClassNameOrPath, AttrName) ->
    Stack = ?STACKTRACE_C,
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
			     "Unrecognized combination of attributes",
			     {property, Prop},
			     {classNameOrPath, ClassNameOrPath},
			     {attrName, AttrName},
			     {stacktrace, Stack}]),
    undefined.

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Find specified parts of model info about a MIM object that was
%%%   previously retrieved using getMimClass/1 or getMimClass/2.
%%%   This function searches only on the second level in the data structure.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec keyfind(ModelInfo :: coi_mim_info() | coi_mim_element(),
	      Key :: atom()) ->
    coi_mim_info().
%%% ###=====================================================================###
keyfind([{_, Props} | Tail], Key) ->
    keyfind_all_flat(Props, Key) ++ keyfind(Tail, Key);
keyfind([], _) ->
    [];
keyfind({class, _} = Class, Key) ->
    keyfind([Class], Key).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Find specified parts of model info about a MIM object that was
%%%   previously retrieved using getMimClass/1 or getMimClass/2.
%%%   This function searches on all levels in the data structure.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec keyfind_all(ModelInfo :: coi_mim_info(),
		  Key :: atom()) ->
    coi_mim_info().
%%% ###=====================================================================###
keyfind_all([{Key, _} = Prop | Tail], Key) ->
    [Prop | keyfind_all(Tail, Key)];
keyfind_all([{_, Value} | Tail], Key) ->
    keyfind_all(Tail, Key) ++ keyfind_all(Value, Key);
keyfind_all([_ | Tail], Key) ->
    keyfind_all(Tail, Key);
keyfind_all([], _) ->
    [].

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Pair each element in <i>List1</i> with the element from <i>List2</i>
%%%   at corresponding list position into a tuple.
%%%   If the two lists are of different size, the atom 'undefined' replaces
%%%   the missing position of the other list.
%%%
%%%   Example:
%%%   > coi:pairElems([1, 2, 3], [a, b]).
%%%   [{1,a},{2,b},{3,undefined}]
%%%
%%% @end
%%% ----------------------------------------------------------
-spec pairElems(List1 :: list(any()),
		List2 :: list(any())) ->
    list({ElementXList1::any(), ElementXList2::any()}).
%%% ###=====================================================================###
pairElems(List1, List2) ->
    sysUtil:pairElems(List1, List2).

%%% ###########################################################################
%%% ----------------------------------------------------------
%%% @doc Checks if COM is started.
%%%
%%% @end
%%% ----------------------------------------------------------

-spec is_com_started() -> boolean().
is_com_started() ->
    comsaI:is_com_started().

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% get_transaction_application
%%%
%%% ###=====================================================================###
get_transaction_application(#?MODULE{appl = Value}) ->
    Value;
get_transaction_application(_) ->
    {error, bad_format}.

%%% ###########################################################################
%%% get_transaction_number
%%%
%%% ###=====================================================================###
get_transaction_number(#?MODULE{transNo = Value}) ->
    Value;
get_transaction_number(_) ->
    {error, bad_format}.

%%% ###########################################################################
%%% get_transaction_user
%%%
%%% ###=====================================================================###
get_transaction_user(#?MODULE{user = Value}) ->
    Value;
get_transaction_user(_) ->
    {error, bad_format}.

%%% ----------------------------------------------------------
%%% @doc This function should only be used for test when
%%% rcssim or VRCS is running.
%%% For a simulated node, a LDAP server does not exist (by default) and hence
%%% no "normal" users can be defined.
%%% The function creates a fake user ("faketestuser") with some roles.
%%%
%%%   Example:
%%%   > coi:createFakeUser().
%%%   {ok,[{{roles,"faketestuser"},
%%%         ["SystemAdministrator",SystemReadOnly", expert"]}]}
%%%
%%% @end
%%%
%%% ----------------------------------------------------------
-spec createFakeUser() ->
   {ok, list()} | {false, error_reason()}.
%%% ###=====================================================================###
createFakeUser() ->
    coiSecurity:createFakeUser().

%%% ----------------------------------------------------------
%%% @doc This function should only be used for test when
%%% rcssim or VRCS is running.
%%% The function deletes the fake user and its roles
%%% created with function "createFakeUser".
%%% (user= "faketestuser").
%%%
%%% @end
%%%
%%% ----------------------------------------------------------
-spec deleteFakeUser() ->
   ok | {false, error_reason()}.
%%% ###=====================================================================###
deleteFakeUser() ->
    coiSecurity:deleteFakeUser().

%%% ----------------------------------------------------------
%%% @doc This function should only be used for test when
%%% rcssim or VRCS is running.
%%% The function creates a fake maintenance user
%%% (user= "faketestmaintuser", pwd = "Letmein01").
%%%
%%% @end
%%%
%%% ----------------------------------------------------------
-spec createFakeMaintenanceUser() ->
   ok | {false, error_reason()}.
%%% ###=====================================================================###
createFakeMaintenanceUser() ->
    coiSecurity:createFakeMaintenanceUser().

%%% ----------------------------------------------------------
%%% @doc This function should only be used for test when
%%% rcssim or VRCS is running.
%%% The function deletes the fake maintenance user
%%% created with function "createFakeMaintenanceUser".
%%% (user= "faketestmaintuser", pwd = "Letmein01").
%%%
%%% @end
%%%
%%% ----------------------------------------------------------
-spec deleteFakeMaintenanceUser() ->
   ok | {false, error_reason()}.
%%% ###=====================================================================###
deleteFakeMaintenanceUser() ->
    coiSecurity:deleteFakeMaintenanceUser().

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% add_className
%%%
%%% ###=====================================================================###
add_className(Dn, Name, Value) ->
    Dn ++
	"," ++
	sysUtil:term_to_string(Name) ++
	"=" ++
	sysUtil:term_to_string(Value).

%%% ###########################################################################
%%% get_networkManagedElementId
%%%
%%% ###=====================================================================###
get_networkManagedElementId() ->
    case lists:keyfind(networkManagedElementId,
		       1,
		       comsaI:get_managed_element_data())
	of
	{networkManagedElementId, undefined} ->
	    "1";
	{networkManagedElementId, ME} ->
	    ME
    end.

%%% ###########################################################################
%%% get_previous_module
%%%
%%% ###=====================================================================###
get_previous_module() ->
    Stack = ?STACKTRACE_C,
    get_previous_module(Stack).

%%% ###---------------------------------------------------------------------###
get_previous_module([{Module, _, _, _} | _]) when Module /= ?MODULE ->
    Module;
get_previous_module([_ | Tail]) ->
    get_previous_module(Tail);
get_previous_module([]) ->
    undefined.

%%% ###########################################################################
%%% is_attribute_find
%%%
%%% ###=====================================================================###
is_attribute_find([{attribute, Props} | Tail], AttrName) ->
    case lists:keyfind(name, 1, Props) of
	{name, AttrName} ->
	    Props;
	_ ->
	    is_attribute_find(Tail, AttrName)
    end;
is_attribute_find([_ | Tail], AttrName) ->
    is_attribute_find(Tail, AttrName);
is_attribute_find([], _) ->
    undefined.

%%% ###########################################################################
%%% is_attribute_individual
%%%
%%% ###=====================================================================###
is_attribute_individual(Prop,
			{class, _} = Class,
			[E | _] = AttrName) when is_integer(E) ->
    Attrs = keyfind(Class, attribute),
    AttrProps = is_attribute_find(Attrs, AttrName),
    is_attribute_match(AttrProps, Prop);
is_attribute_individual(Prop, ClassNameOrPath, AttrName) ->
    Stack = ?STACKTRACE_C,
    sysInitI:warning_report([{?MODULE, ?FUNCTION},
			     "Unrecognized combination of attributes",
			     {property, Prop},
			     {classNameOrPath, ClassNameOrPath},
			     {attrName, AttrName},
			     {stacktrace, Stack}]),
    undefined.

%%% ###########################################################################
%%% is_attribute_match
%%%
%%% ###=====================================================================###
is_attribute_match([{Tag, Props} | _], Prop) when Tag == struct orelse
						  Tag == enum ->
    Match =
	case Tag of
	    Prop ->
		true;
	    _ ->
		false
	end,
    {Match, {Tag, is_attribute_sublevel(Props, Prop)}};
is_attribute_match([{Prop, Props} | _], Prop) ->
    case is_sublevel(Props) of
	false ->
	    true;
	{true, {Tag, PropsSublevel}} ->
	    {true, {Tag, is_attribute_sublevel(PropsSublevel, Prop)}}
    end;
is_attribute_match([{_, [{_, _} | _] = Props} | Tail], Prop) ->
    case is_attribute_match(Props, Prop) of
	false ->
	    is_attribute_match(Tail, Prop);
	Other ->
	    Other
    end;
is_attribute_match([_ | Tail], Prop) ->
    is_attribute_match(Tail, Prop);
is_attribute_match([], _) ->
    false;
is_attribute_match(undefined, _) ->
    undefined.

%%% ###########################################################################
%%% is_sublevel
%%%
%%% ###=====================================================================###
is_sublevel([{Tag, Props} | _]) when Tag == struct orelse
				     Tag == enum ->
    {true, {Tag, Props}};
is_sublevel([_ | Tail]) ->
    is_sublevel(Tail);
is_sublevel([]) ->
    false.

%%% ###########################################################################
%%% is_attribute_sublevel
%%%
%%% ###=====================================================================###
is_attribute_sublevel([{Member, Props} | Tail], Prop)
  when Member == structMember orelse
       Member == enumMember ->
    Name =
	case lists:keyfind(name, 1, Props) of
	    {name, Val} ->
		Val;
	    _ ->
		""
	end,
    case Prop of
	Member ->
	    [{Name, true}
	     | is_attribute_sublevel(Tail, Prop)];
	_ ->
	    [{Name, is_attribute_match(Props, Prop)}
	     | is_attribute_sublevel(Tail, Prop)]
    end;
is_attribute_sublevel([{Prop, _} | _], Prop) ->
    true;
is_attribute_sublevel([_ | Tail], Prop) ->
    is_attribute_sublevel(Tail, Prop);
is_attribute_sublevel([], _) ->
    [].

%%% ###########################################################################
%%% keyfind_all_flat
%%%
%%% ###=====================================================================###
keyfind_all_flat([{Key, _} = Prop | Tail], Key) ->
    [Prop | keyfind_all_flat(Tail, Key)];
keyfind_all_flat([_ | Tail], Key) ->
    keyfind_all_flat(Tail, Key);
keyfind_all_flat([], _) ->
    [].

%%% ###########################################################################
%%% log_AuditTrail_xxx
%%%
%%% ###=====================================================================###
log_AuditTrail_abort(TransId) ->
    log_AuditTrail_write(warning, TransId, "Abort").

%%% ###=====================================================================###
log_AuditTrail_action(TransId, ECIM_Dn, Name) ->
    MsgTail =
	"Invoke action():" ++
	" DN: " ++
	sysUtil:term_to_string(ECIM_Dn) ++
	"." ++
	sysUtil:term_to_string(Name),
    log_AuditTrail_write(info, TransId, MsgTail).

%%% ###=====================================================================###
log_AuditTrail_commit(TransId) ->
    log_AuditTrail_write(notice, TransId, "Commit").

%%% ###=====================================================================###
log_AuditTrail_create(TransId, Parent_ECIM_Dn, Class, Value, NamedAttrs) ->
    ECIM_Dn_String =
	sysUtil:term_to_string(Parent_ECIM_Dn) ++
	"," ++
	sysUtil:term_to_string(Class) ++
	"=" ++
	sysUtil:term_to_string(Value),
    MsgTail =
	"Invoke createMo():" ++
	" DN: " ++
	ECIM_Dn_String,
    log_AuditTrail_write(info, TransId, MsgTail),
    log_AuditTrail_set(TransId, ECIM_Dn_String, NamedAttrs).

%%% ###=====================================================================###
log_AuditTrail_delete(TransId, ECIM_Dn) ->
    MsgTail =
	"Invoke deleteMo():" ++
	" DN: " ++
	sysUtil:term_to_string(ECIM_Dn),
    log_AuditTrail_write(info, TransId, MsgTail).

%%% ###=====================================================================###
log_AuditTrail_set(TransId, ECIM_Dn, NamedAttrs) ->
    [begin
	 MsgTail =
	     "Invoke setMo():" ++
	     " DN: " ++
	     sysUtil:term_to_string(ECIM_Dn) ++
	     " attribute: " ++
	     sysUtil:term_to_string(Attr) ++
	     " value: " ++
	     log_format_value(Value),
	 log_AuditTrail_write(info, TransId, MsgTail)
     end
     || {Attr, Value} <- NamedAttrs].

%%% ###=====================================================================###
log_AuditTrail_write(Severity,
		     #?MODULE{appl = Appl, user = User, transNo = TransNo},
		     MsgTail) ->
    Msg =
	"interface=coi" ++
	"  interface-application=" ++
	sysUtil:term_to_string(Appl) ++
	"  user-name=" ++
	sysUtil:term_to_string(User) ++
	"  Transaction " ++
	sysUtil:term_to_string(TransNo) ++
	" " ++
	MsgTail,
    logI:write_log("AuditTrailLog",
		   get_networkManagedElementId(),
		   13,   % Facility number for audit trail
		   Severity,
		   os:timestamp(),
		   Msg).

%%% ###########################################################################
%%% log_get_value
%%%
%%% ###=====================================================================###
log_format_value(Value) ->
    "'" ++ sysUtil:term_to_string(log_get_value(Value)) ++ "'".

%%% ###=====================================================================###
log_get_value({?STRUCT, StructMembers}) when is_list(StructMembers) ->
    ValuesString =
	[begin
	     sysUtil:term_to_string(Attr) ++
		 "=" ++
		 log_format_value(Value) ++
		 ","
	 end
	 || {Attr, Value} <- StructMembers],
    "[" ++ lists:droplast(lists:flatten(ValuesString)) ++ "]";
log_get_value({ValueType, Value}) when ValueType /= ?STRUCT ->
    Value;
log_get_value(Value) when is_atom(Value) ->
    Value;
log_get_value(Values) when is_list(Values) ->
    ValuesString =
	[sysUtil:term_to_string(log_get_value(Value)) ++ "," ||
	    Value <- Values],
    "[" ++ lists:droplast(lists:flatten(ValuesString)) ++ "]";
log_get_value(Value) ->
    Stack = ?STACKTRACE_C,
    error_logger:warning_report([{unrecognized_value, Value} | Stack]),
    unknown.

%%% ###########################################################################
%%% to_ecimDn
%%%
%%% ###=====================================================================###
to_ecimDn(NamesValues) ->
    list_to_binary(lists:flatten(to_ecimDn_loop(NamesValues))).

to_ecimDn_loop([ClassName, Value])
  when is_list(ClassName) andalso
       is_list(Value) ->
    [ClassName ++ "=" ++ Value];
to_ecimDn_loop([ClassName, Value | Tail])
  when is_list(ClassName) andalso
       is_list(Value) ->
    [ClassName, "=", Value, "," | to_ecimDn_loop(Tail)];
to_ecimDn_loop([_ | _] = ECIM_Dn) ->
    ECIM_Dn;
to_ecimDn_loop([]) ->
    [].

%%% ###########################################################################
%%% validate_ecimDn
%%%
%%% ###=====================================================================###
validate_ecimDn(Split_EcimDn) ->
    validate_ecimDn_loop(Split_EcimDn),
    Split_EcimDn.

validate_ecimDn_loop([ClassName, Value])
  when is_list(ClassName) andalso
       is_list(Value) ->
    ok;
validate_ecimDn_loop([ClassName, Value | Tail])
  when is_list(ClassName) andalso
       is_list(Value) ->
    validate_ecimDn_loop(Tail);
validate_ecimDn_loop([_ | _]) ->
    Reason = "Invalid format of ECIM_Dn.",
    throw({?MODULE, list_to_binary(Reason)});
validate_ecimDn_loop([]) ->
    ok.

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 4     CODE FOR TEMPORARY CORRECTIONS
%%% ###---------------------------------------------------------------------###
