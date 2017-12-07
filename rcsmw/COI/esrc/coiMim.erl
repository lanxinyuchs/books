%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	coiMim.erl %
%%% @author etxpeno
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R3A/R4A/R5A/R6A/R11A/1

%%% @doc == COI - Control system Oam Interface ==
%%% This module contains functions for reading the Management Information
%%% Model (MIM) which supports the coi.erl interface functionality.
%%%
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-module(coiMim).
-vsn('/main/R3A/R4A/R5A/R6A/R11A/1').
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
%%% 1      2014-10-24 etxberb  Created.
%%% 2      2014-11-21 etxberb  Added table in installation phase.
%%% 3      2014-12-02 etxberb  Info report additions.
%%% 4      2014-12-05 etxberb  Changed to unique class keys (ecim_class_path).
%%% R3A/6  2014-12-09 etxberb  Added getMimTree/X.
%% ===================
%% R3A/1   2015-01-13 etxberb  Moved to the COI block.
%% R4A/1   2015-05-28 etxberb  Added detection of backwards_relation.
%% R4A/2   2015-06-25 etxberb  Added init/0 & is_post_init_done/0.
%% R4A/3   2015-06-30 etxberb  Storage of 'description' as binary.
%% R4A/4   2015-07-07 etxberb  Changed mnesia:create_table to
%%                             clhI:mnesia_create_table.
%% R4A/6   2015-09-03 etxberb  Introduced parallel execution of post_init and
%%                             replaced list handling with ets tables.
%% R4A/7   2015-11-11 etxberb  Allowing Path format ("/<path>/ClassName") in
%%                             getMimClass/1 & getMimClassPath/1.
%% R5A/1   2016-01-07 etxberb  Changed installation phases in all blocks.
%%                             post_init_from_gmf/0 changed to post_init/0.
%% R6A/1   2016-05-29 erarafo  HU83277 solution
%% R11A/1  2017-10-17 etxpeno  OTP 20 fixes
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
-export([init/0]).
-export([init_tables/1]).
-export([post_init/0]).

-export([getMimClass/1,
	 getMimClassPath/1,
	 getMimTree/1,
	 getMimTree/2]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([getMimClass_parse_init/2,
	 getMimClass_write_init/4]).

%%% ###---------------------------------------------------------------------###
%%% # Extended Interface
%%% ###---------------------------------------------------------------------###

%%% ###---------------------------------------------------------------------###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###
-include("coi.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%% ###=====================================================================###
%%% # 2.4   LOCAL DEFINITION OF MACROS
%%% ###---------------------------------------------------------------------###
-define(TblName,        ?MODULE).
-define(TblName_keys,   coiMim_keys).
-define(TblName_tmp,                 coiMim_tmp).
-define(TblName_tmp_mimNames,        coiMim_tmp_mimNames).
-define(TblName_tmp_relationship,    coiMim_tmp_relationship).
-define(TblName_tmp_derivedDataType, coiMim_tmp_derivedDataType).
-define(TblName_tmp_enum,            coiMim_tmp_enum).
-define(TblName_tmp_struct,          coiMim_tmp_struct).

-define(endElement(Name),         {endElement, _, Name, _QName}).
-define(startElement(Name),       {startElement, _Uri, Name, _QName, _}).
-define(startElement_Attrs(Name), {startElement, _Uri, Name, _QName, Attrs}).

%% General
-define(ELSE, true).

-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

%% Maximum number of times that a classname can occur in a classpath in
%% the coiMim table. As of 2016-05-30 the number is set to 1, which means
%% that repetitions are not supported; this is sufficient for an immediate
%% solution of the HU83277 problem.
-define(REPEAT_COUNT_LIMIT, 1).

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###
-record(?TblName, {key,
 		   value}).

-record(?TblName_keys, {name,
			path}).

-record(state, {parsing,
		objectName,
		mimName,
		ecimMomName = [],
		class = []
	       }).

-record(parsing, {topElement,
		  data = []
		 }).

-record(parent, {key,
		 obj
		}).

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% @doc The 'init' start phase.
%%%
%%% @end
%%% ###=====================================================================###
init() ->
    ok.

%%% ###########################################################################
%%% @doc Parse MIM files and populate the MIM table.
%%%
%%% @end
%%% ###=====================================================================###
init_tables(DbNodes) ->
    {atomic, ok} =
	clhI:mnesia_create_table(?TblName_keys,
				 [{type, bag},
				  {disc_copies, DbNodes},
				  {attributes, record_info(fields,
							   ?TblName_keys)}]),
    {atomic, ok} =
	clhI:mnesia_create_table(?TblName,
				 [{type, ordered_set},
				  {disc_copies, DbNodes},
				  {attributes, record_info(fields, ?TblName)}]).

%%% ###########################################################################
%%% @doc Parse MIM files and populate the MIM table.
%%%
%%% @end
%%% ###=====================================================================###
post_init() ->
    sysInitI:info_report([{?MODULE, ?FUNCTION},
			  "Starting to parse MIM files in parallel..."]),
    ets:new(?TblName_tmp, [named_table, public, ordered_set, {keypos, 2}]),
    ets:new(?TblName_tmp_mimNames,
	    [named_table, public, ordered_set, {keypos, 1}]),
    ets:new(?TblName_tmp_relationship,
	    [named_table, public, bag, {keypos, 1}]),
    ets:new(?TblName_tmp_derivedDataType,
	    [named_table, public, ordered_set, {keypos, 1}]),
    ets:new(?TblName_tmp_enum,
	    [named_table, public, ordered_set, {keypos, 1}]),
    ets:new(?TblName_tmp_struct,
	    [named_table, public, ordered_set, {keypos, 1}]),
    MimFiles = coi:getMimFiles(),
    % Testing interactively with a single MP XML can be done; re-running
    % this function several times is not harmful.
    % MimFiles = ["/software/LRAT-ARM_CXP9025671_26_R1GT/lratMomLm/xml/Lrat_mp_test_extended.xml"],

    ParsingPids =
	[spawn_opt(?MODULE,
		   getMimClass_parse_init,
		   [MimFile, self()],
		   [{min_heap_size, 5000}]) ||
	    MimFile <- MimFiles],
    post_init_parse(ParsingPids, MimFiles, ?REPEAT_COUNT_LIMIT).

%%% ###=====================================================================###
post_init_parse([_ | _] = ParsingPids, MimFiles, Limit) ->
    receive
	{?MODULE, getMimClass_parse_done, Pid} ->
	    post_init_parse(lists:delete(Pid, ParsingPids), MimFiles, Limit)
    end;
post_init_parse([], MimFiles, Limit) ->
    NoOfMimFiles = sysUtil:term_to_string(length(MimFiles)),
    InfoParse =
	[{?MODULE, ?FUNCTION},
	 "...finished parsing",
	 "####### " ++ NoOfMimFiles ++ " MIM files parsed #######",
	 [lists:last(string:tokens(MimFile, "/")) || MimFile <- MimFiles]],
    error_logger:info_report(InfoParse),
    TargetList =
	[{enumRef, enum, ?TblName_tmp_enum},
	 {derivedDataTypeRef, derivedDataType, ?TblName_tmp_derivedDataType},
	 {structRef, struct, ?TblName_tmp_struct}],
    ClassKeys = ets_all_keys(?TblName_tmp),
    InfoPostParse =
	[{?MODULE, ?FUNCTION},
	 "Starting to replace references, sort relations & disc write..."],
    sysInitI:info_report(InfoPostParse),
    ClassPids =
	[spawn_opt(?MODULE,
		   getMimClass_write_init,
		   [ClassKey, TargetList, self(), Limit],
		   [{min_heap_size, 5000}]) ||
	    ClassKey <- ClassKeys],
    post_init_post_parse(ClassPids).

%%% ###=====================================================================###
post_init_post_parse([_ | _] = ClassPids) ->
    receive
	{?MODULE, getMimClass_write_done, Pid} ->
	    post_init_post_parse(lists:delete(Pid, ClassPids))
    end;
post_init_post_parse([]) ->
    NoOfClasses = sysUtil:term_to_string(ets:info(?TblName_tmp, size)),
    ClassesHeading =
	"####### " ++
	NoOfClasses ++
	" MIM classes complete and stored on disc; classpaths are: #######",
    ets:delete(?TblName_tmp),
    ets:delete(?TblName_tmp_mimNames),
    ets:delete(?TblName_tmp_relationship),
    ets:delete(?TblName_tmp_derivedDataType),
    ets:delete(?TblName_tmp_enum),
    ets:delete(?TblName_tmp_struct),
    Info =
	[{?MODULE, ?FUNCTION},
	 "...finished!",
	 {table_info, [{Tbl, [Item ||
			      {Tag, _} = Item
					     <- mnesia:table_info(Tbl, all),
			      Tag == size orelse Tag == memory]} ||
		       Tbl <- [?TblName, ?TblName_keys]]},
	 ClassesHeading |
	     lists:sort(mnesia:dirty_all_keys(?TblName))],
    error_logger:info_report(Info),
    ok.

%%% ###########################################################################
%%% @doc Get model info about a MIM class.
%%%
%%% @end
%%% ###=====================================================================###
getMimClass(ClassName) ->
    ClassPaths = getMimClassPath(ClassName),
    Classes =
	lists:flatten([ets:lookup(?TblName, ClassPath) || ClassPath
							      <- ClassPaths]),
    getMimClass_unpack([Class || {_, _, Class} <- Classes]).

%%% ###=====================================================================###
getMimClass_unpack([{Name, Value} | Tail]) ->
    [{Name, getMimClass_unpack(Value)} | getMimClass_unpack(Tail)];
getMimClass_unpack(Value) when is_binary(Value) ->
    binary_to_term(Value);
getMimClass_unpack(Value) ->
    Value.

%%% ###########################################################################
%%% @doc Get the ECIM class path for a MIM class.
%%%
%%% @end
%%% ###=====================================================================###
getMimClassPath([$/ | _] = ClassPath) ->
    %% First character in the string is a "/" -> the string is a path!
    case ets:lookup(?TblName, ClassPath) of
	[_] ->
	    [ClassPath];
	_ ->
	    Stack = element(2, process_info(self(), current_stacktrace)),
	    Warning = [{?MODULE, ?FUNCTION},
		       class_not_found,
		       {invalid_path, ClassPath},
		       {callstack, Stack}],
	    sysInitI:warning_report(Warning),
	    []
    end;
getMimClassPath(ClassName) ->
    EcimClassPaths = ets:lookup(?TblName_keys, ClassName),
    [EcimClassPath || #?TblName_keys{path = EcimClassPath} <- EcimClassPaths].

%%% ###########################################################################
%%% @doc Get the hierarchical tree for a MIM class.
%%%
%%% @end
%%% ###=====================================================================###
getMimTree(Dn) ->
    getMimTree(Dn, 1).

getMimTree(Dn, Depth) ->
    ClassNames =
	remove_values(string:tokens(sysUtil:term_to_string(Dn), ",=")),
    {Parents, [ClassName]} = lists:split(length(ClassNames) - 1, ClassNames),
    [{ClassName, getMimTree_depth(decr1(Depth), ClassName, Parents)}].

getMimTree_depth(Depth, ClassName, Parents) when Depth >= 0 ->
    case lists:member(ClassName, Parents) of
	true ->
	    %% My parent is also my child! Backwards (circular) relation.
	    backwards_relation;
	false ->
	    Names = getMimTree_loop(getMimClass(ClassName), Parents),
	    [{NextName, getMimTree_depth(decr1(Depth),
					 NextName,
					 Parents ++ [ClassName])}
	     || NextName <- Names]
    end;
getMimTree_depth(_, _, _) ->
    [].

getMimTree_loop([{class, Props} | Tail], Parents) ->
    case find_real_parent(Props) of
	{_, ParentProps1} ->
	    {_, ParentProps2} = lists:keyfind(hasClass, 1, ParentProps1),
	    {_, Parent} = lists:keyfind(name, 1, ParentProps2),
	    case lists:member(Parent, Parents) of
		true ->
		    getMimTree_loop_names(Props);
		false ->
		    getMimTree_loop(Tail, Parents)
	    end;
	false ->
	    %% This class is the top level - it has no parent!
	    getMimTree_loop_names(Props)
    end;
getMimTree_loop([], _) ->
    [].

getMimTree_loop_names([{child, Props} | Tail]) ->
    {_, ChildProps} = lists:keyfind(hasClass, 1, Props),
    {_, ChildName} = lists:keyfind(name, 1, ChildProps),
    [ChildName | getMimTree_loop_names(Tail)];
getMimTree_loop_names([_ | Tail]) ->
    getMimTree_loop_names(Tail);
getMimTree_loop_names([]) ->
    [].

%%% ###########################################################################
%%% decr1
%%%
%%% ###=====================================================================###
decr1(Integer) when is_integer(Integer) ->
    Integer - 1;
decr1(Atom) when is_atom(Atom) ->
    Atom.

%%% ###########################################################################
%%% remove_values
%%%
%%% Expects an even-length list, returns a list half as long
%%% by keeping elements 1, 3, .. etc.
%%%
%%% ###=====================================================================###
-spec remove_values([string()]) -> [string()].

remove_values([ClassName, _Value]) ->
    [ClassName];
remove_values([ClassName, _Value | Tail]) ->
    [ClassName | remove_values(Tail)];
remove_values([]) ->
    [].

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% getMimClass_parse_init
%%%
%%% ###=====================================================================###
getMimClass_parse_init(MimFile, PostInitMasterPid) ->
    try
	getMimClass_parse(MimFile)
    catch
	ErrClass : ErrReason ->
	    error_logger:error_report([{?MODULE, ?FUNCTION},
				       {ErrClass, ErrReason},
				       {mimFile, MimFile} |
				       erlang:get_stacktrace()])
    end,
    PostInitMasterPid ! {?MODULE, getMimClass_parse_done, self()},
    exit(normal).

%%% ###########################################################################
%%% getMimClass_write_init
%%%
%%% ###=====================================================================###
getMimClass_write_init(ClassKey, TargetList, PostInitMasterPid, Limit) ->
    try
	getMimClass_write(ClassKey, TargetList, Limit)
    catch
	ErrClass : ErrReason ->
	    error_logger:error_report([{?MODULE, ?FUNCTION},
				       {ErrClass, ErrReason},
				       {classKey, ClassKey} |
				       erlang:get_stacktrace()])
    end,
    PostInitMasterPid ! {?MODULE, getMimClass_write_done, self()},
    exit(normal).

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% elemAttrs
%%%
%%% ###=====================================================================###
elemAttrs([{_, _, Name, Value} | Tail]) ->
    [{list_to_atom(Name), Value} | elemAttrs(Tail)];
elemAttrs([_ | Tail]) ->
    elemAttrs(Tail);
elemAttrs([]) ->
    [].

%%% ###########################################################################
%%% ets_all_keys
%%%
%%% ###=====================================================================###
ets_all_keys(TblName) ->
    ets_all_keys(ets:first(TblName), TblName).

ets_all_keys(Key, TblName) when Key /= '$end_of_table' ->
    [Key | ets_all_keys(ets:next(TblName, Key), TblName)];
ets_all_keys('$end_of_table', _) ->
    [].

%%% ###########################################################################
%%% find_real_parent
%%%
%%% ###=====================================================================###
find_real_parent(Props) ->
    case lists_keyfind_all(Props, parent, 1) of
	[Parent] ->
	    Parent;
	[] ->
	    false;
	[_ | _] = Parents ->
	    Children = lists_keyfind_all(Props, child, 1),
	    ChildClasses =
		[lists:keyfind(hasClass, 1, ChildProps) ||
		    {child, ChildProps} <- Children],
	    find_real_parent(Parents, ChildClasses)
    end.

find_real_parent([{parent, ParentProps} = Parent | Tail], Children) ->
    ParentClass = lists:keyfind(hasClass, 1, ParentProps),
    case
	[backwards_relation || ChildClass <- Children,
			       ParentClass == ChildClass]
    of
	[] ->
	    Parent;
	[backwards_relation | _] ->
	    find_real_parent(Tail, Children)
    end;
find_real_parent([], _) ->
    false.

%%% ###########################################################################
%%% find_referenced_elem
%%%
%%% ###=====================================================================###
find_referenced_elem(TblName, Tag, SearchProps) ->
    case find_referenced_elem(props_keys(SearchProps), TblName) of
	{true, Props} ->
	    OrigProps =
		lists:keydelete(mimName,
				1,
				lists:keydelete(name, 1, SearchProps)),
	    {Tag, OrigProps ++ Props};
	false ->
	    Warning =
		[{?MODULE, ?FUNCTION},
		 {referenced_data_not_found, 'mim fault?'},
		 {Tag, SearchProps}],
	    error_logger:warning_report(Warning),
	    false
    end.

%%% ###=====================================================================###
find_referenced_elem([Key | Tail], TblName) ->
    case ets:lookup(TblName, Key) of
	[{_, Props}] ->
	    {true, Props};
	_ ->
	    find_referenced_elem(Tail, TblName)
    end;
find_referenced_elem([], _) ->
    false.

%%% ###########################################################################
%%% find_target
%%%
%%% ###=====================================================================###
find_target([{TagRef, TagReal, TblName} | _], {TagRef, _}) ->
    {true, TagReal, TblName};
find_target([_ | Tail], Elem) ->
    find_target(Tail, Elem);
find_target([], _) ->
    false.

%%% ###########################################################################
%%% get_ancesters
%%% Returns the most significant path up to the root class.
%%%
%%% ###=====================================================================###
-spec get_ancesters(any()) -> [string()].

get_ancesters(Relations) ->
    case get_parent(Relations) of
	{parent, ParentProps} ->
	    {_, HasClass} = lists:keyfind(hasClass, 1, ParentProps),
	    {_, ParentName} = lists:keyfind(name, 1, HasClass),
	    {_, ParentMimName} = lists:keyfind(mimName, 1, HasClass),
	    ParentRelations = get_relations(ParentName, ParentMimName),
	    [ParentName | get_ancesters(ParentRelations)];
	_ ->
	    []
    end.

%%% ###########################################################################
%%% get_ancesters_all
%%% Returns a list of all "possible" get_ancesters/1 results.
%%%
%%% ###=====================================================================###
-spec get_ancesters_all(any(), [string()], integer()) -> [[string()]].

get_ancesters_all(Relations, ClassesSeen, Limit) ->
    case get_parents(Relations) of
	[] ->
	    [[]];
	NonEmptyList ->
	    lists:foldr(
	      fun({parent, ParentProps}, Acc) ->
		      {_, HasClass} = lists:keyfind(hasClass, 1, ParentProps),
		      {_, ParentName} = lists:keyfind(name, 1, HasClass),
		      AtLeast = atLeast(Limit, ParentName, ClassesSeen),
		      if
			  AtLeast ->
			      Acc;
			  true ->
			      {_, ParentMimName} = lists:keyfind(mimName, 1, HasClass),
			      ParentRelations = get_relations(ParentName, ParentMimName),
			      Paths = [[ParentName|Path]
				       ||Path <- get_ancesters_all(ParentRelations, [ParentName|ClassesSeen], Limit)],
			      Paths++Acc
		      end
	      end,
	      [],
	      NonEmptyList)
    end.

%%% ###########################################################################
%%% atLeast
%%% Returns true if the given list holds at least N occurrences of X.
%%%
%%% ###=====================================================================###
-spec atLeast(non_neg_integer(), any(), [any()]) -> boolean().

atLeast(0, _, _) ->
    true;
atLeast(_, _, []) ->
    false;
atLeast(N, X, [X|Tail]) ->
    atLeast(N-1, X, Tail);
atLeast(N, X, [_|Tail]) ->
    atLeast(N, X, Tail).

%%% ###########################################################################
%%% get_ecim_class_path
%%%
%%% ###=====================================================================###
get_ecim_class_path(Relations, SearchProps) ->
    {_, ObjClassName} = lists:keyfind(name, 1, SearchProps),
    Path = lists:reverse([ObjClassName | get_ancesters(Relations)]),
    lists:flatten([[$/ | ClassName] || ClassName <- Path]).

%%% ###########################################################################
%%% get_ecim_class_path_all
%%% Gets a list of all possible classpaths.
%%%
%%% ###=====================================================================###
-spec get_ecim_class_path_all(any(), any(), integer()) -> [string()].

get_ecim_class_path_all(Relations, SearchProps, Limit) ->
    {_, ObjClassName} = lists:keyfind(name, 1, SearchProps),
    ManyResults = get_ancesters_all(Relations, [ObjClassName], Limit),
    [lists:flatten([[$/|U]||U <- lists:reverse([ObjClassName|OneResult])])
       ||OneResult <- ManyResults].

%%% ###########################################################################
%%% get_family
%%% Gets the parents and children of the implied class.
%%% The argument is a list of tuples describing a class:
%%%
%%% [{name, "XyzAbc"},
%%%  {mimName, "MWAH"},
%%%  {description, _},        % bulky!
%%%  {action, []},
%%%  {attribute, _}           % many!
%%% ]
%%%
%%% Keys will look like [{"UtranTDDFreqRelation", "Lrat"}], possibly longer
%%%
%%% Relation entries will be all children and all parents, may be several of each:
%%%
%%% [{{"UtranTDDFreqRelation","Lrat"},
%%%   {child,[{hasClass,[{name,"UtranCellRelation"}, {mimName,"Lrat"}]}, {cardinality,[{min,"0"}]}]}},
%%%  {{"UtranTDDFreqRelation","Lrat"},
%%%   {parent,[{hasClass,[{name,"EUtranCellFDD"}, {mimName,"Lrat"}]}]}},
%%%  {{"UtranTDDFreqRelation","Lrat"},
%%%   {parent,[{hasClass,[{name,"EUtranCellTDD"}, {mimName,"Lrat"}]}]}}]
%%%
%%% A result may look like:
%%% [{parent,[{hasClass,[{name,"EUtranCellFDD"},{mimName,"Lrat"}]}]},
%%%  {parent,[{hasClass,[{name,"EUtranCellTDD"},{mimName,"Lrat"}]}]},
%%%  {child,[{hasClass,[{name,"UtranCellRelation"},{mimName,"Lrat"}]}, {cardinality,[{min,"0"}]}]}]
%%%
%%% ###=====================================================================###
get_family(ClassProps) ->
    Keys = props_keys(ClassProps),
    TableEntries = [ets:lookup(?TblName_tmp_relationship, Key) || Key <- Keys],
    RelationEntries = lists:flatten(TableEntries),
    [Relation || {_, Relation} <- RelationEntries].

%%% ###########################################################################
%%% get_mimNames
%%%
%%% ###=====================================================================###
get_mimNames(MimName) ->
    case ets:lookup(?TblName_tmp_mimNames, MimName) of
	[] ->
	    [MimName];
	[{_, OtherMimName}] ->
	    [MimName, OtherMimName]
    end.

%%% ###########################################################################
%%% get_parent
%%%
%%% ###=====================================================================###
-spec get_parent(any()) -> {parent, list()} | false.

get_parent(Relations) ->
    case get_parents(Relations) of
	[] ->
	    false;
	[Parent] ->
	    Parent;
	Parents ->
	    most_significant_parent(Parents)
    end.

%%% ###########################################################################
%%% get_parents
%%% select {parent, _} tuples from the given list.
%%%
%%% ###=====================================================================###
-spec get_parents(list()) -> [{parent, any()}].

get_parents([{parent, _} = Parent | Tail]) ->
    [Parent | get_parents(Tail)];
get_parents([_ | Tail]) ->
    get_parents(Tail);
get_parents([]) ->
    [].

%%% ###########################################################################
%%% get_relations
%%%
%%% ###=====================================================================###
get_relations(ParentName, ParentMimName) ->
    Keys = [{ParentName, MimName} || MimName <- get_mimNames(ParentMimName)],
    TableEntries = [ets:lookup(?TblName_tmp_relationship, Key) || Key <- Keys],
    [ParentRelation || {_, ParentRelation} <- lists:flatten(TableEntries)].

%%% ###########################################################################
%%% get_unique_parents
%%%
%%% ###=====================================================================###
get_unique_parents(Keys) ->
    get_unique_parents(Keys, []).

get_unique_parents([{Name, MimName} | Tail], PrevParents) ->
    Keys = [{Name, MN} || MN <- get_mimNames(MimName)],
    case [Key || Key <- Keys, lists:member(Key, PrevParents)] of
	[] ->
	    ParentsKeys =
		[relative_key(Parent) ||
		    Parent <- get_parents(get_relations(Name, MimName))],
	    NewPrevParents =
		get_unique_parents(ParentsKeys,
				   [{Name, MimName} | PrevParents]),
	    get_unique_parents(Tail, NewPrevParents);
	_ ->
	    get_unique_parents(Tail, PrevParents)
    end;
get_unique_parents([], PrevParents) ->
    PrevParents.

%%% ###########################################################################
%%% getMimClass_parse
%%%
%%% ###=====================================================================###
getMimClass_parse(File) ->
    case xmerl_sax_parser:file(File, [{event_fun, fun parse_event/3},
				      {event_state, #state{}},
				      skip_external_dtd])
    of
	{ok,
	 #state{mimName = {_, MimName},
		ecimMomName = EcimMomNames,
		class = TmpClasses},
	 _} ->
	    [ets:insert(?TblName_tmp, TmpClass) || TmpClass <- TmpClasses],
	    getMimClass_parse_mimNames(EcimMomNames, MimName);
	Error ->
	    error_logger:warning_report([{?MODULE, ?FUNCTION},
					 {xmerl_sax_parser, file},
					 {file, File},
					 {result, Error}])
    end.

%%% ###########################################################################
%%% getMimClass_parse_mimNames
%%%
%%% ###=====================================================================###
getMimClass_parse_mimNames([{_, OtherMimName} | Tail], MimName) ->
    ets:insert(?TblName_tmp_mimNames, {MimName, OtherMimName}),
    ets:insert(?TblName_tmp_mimNames, {OtherMimName, MimName}),
    getMimClass_parse_mimNames(Tail, MimName);
getMimClass_parse_mimNames([], _) ->
    ok.

%%% ###########################################################################
%%% getMimClass_write
%%%
%%% ###=====================================================================###
getMimClass_write({ObjectName, _} = Key, TargetList, Limit)
  when ObjectName /= '$end_of_table' ->
    [{class, _, ClassProps}] = ets:lookup(?TblName_tmp, Key),
    Family = get_family(ClassProps),
    NewClassProps = ClassProps ++ Family,
    [NewClass] = replace_ref([{class, NewClassProps}], TargetList),

    % get all possible classpaths
    EcimClassPaths = get_ecim_class_path_all(Family, ClassProps, Limit),
    [mnesia:dirty_write(#?TblName{key = ECP, value = NewClass})
       ||ECP <- EcimClassPaths],

    % also get the single most significant classpath
    EcimClassPath = get_ecim_class_path(Family, ClassProps),
    mnesia:dirty_write(#?TblName_keys{name = ObjectName,
				      path = EcimClassPath}).

%%% ###########################################################################
%%% insert_relations
%%% Scans the given list for {containment, _} tuples.
%%% ###=====================================================================###
-spec insert_relations(list()) -> ok.

insert_relations([{containment, Props} | _]) ->
    insert_relations(Props, Props);
insert_relations([_ | Tail]) ->
    insert_relations(Tail);
insert_relations([]) ->
    ok.

%%% ###########################################################################
%%% insert_relations
%%% Scans the given property list for child and parent references and
%%% inserts then into the 'coiMim_tmp_relationship' table.
%%% ###=====================================================================###
-spec insert_relations(list(), list()) -> ok.

insert_relations([{child, Props} | Tail], ContainmentProps) ->
    % relation describes one parent
    {_, HasClass} = lists:keyfind(hasClass, 1, Props),
    Keys = props_keys(HasClass),
    % e g [{"EUtranFreqRelation","Lrat"}] (length-2 list possible!)
    Relative = lists:keyfind(parent, 1, ContainmentProps),
    % {parent, [{hasClass, [{name,"EUtranCellFDD"}, {mimName,"Lrat"}]}]}
    [ets:insert(?TblName_tmp_relationship, {Key, Relative}) || Key <- Keys],
    insert_relations(Tail, ContainmentProps);
insert_relations([{parent, Props} | Tail], ContainmentProps) ->
    % relation describes one contained child
    {_, HasClass} = lists:keyfind(hasClass, 1, Props),
    Keys = props_keys(HasClass),
    % e g [{"EUtranCellTDD", "Lrat"}] (length2 list also possible!)
    Relative = lists:keyfind(child, 1, ContainmentProps),
    % e g {child, [{hasClass, [{name,"EUtranFreqRelation"}, {mimName,"Lrat"}]}, {cardinality,[{min,"0"}]}]}
    [ets:insert(?TblName_tmp_relationship, {Key, Relative}) || Key <- Keys],
    insert_relations(Tail, ContainmentProps);
insert_relations([_ | Tail], ContainmentProps) ->
    insert_relations(Tail, ContainmentProps);
insert_relations([], _) ->
    ok.

%%% ###########################################################################
%%% lists_delete
%%%
%%% ###=====================================================================###
%% ####### Candidate function for the sysUtil module ? #######
%% lists_delete([Elem | Tail], List) ->
%%     lists_delete(Tail, lists:delete(Elem, List));
%% lists_delete([], List) ->
%%     List.

%%% ###########################################################################
%%% lists_keyfind_all
%%%
%%% ###=====================================================================###
lists_keyfind_all([Tuple | Tail], Key, Pos) when is_tuple(Tuple) ->
    try
	element(Pos, Tuple) of
	Key ->
	    [Tuple | lists_keyfind_all(Tail, Key, Pos)];
	_ ->
	    lists_keyfind_all(Tail, Key, Pos)
    catch
	_ : _ ->
	    lists_keyfind_all(Tail, Key, Pos)
    end;
lists_keyfind_all([_ | Tail], Key, Pos) ->
    lists_keyfind_all(Tail, Key, Pos);
lists_keyfind_all([], _, _) ->
    [].

%%% ###########################################################################
%%% most_significant_parent
%%%
%%% ###=====================================================================###
most_significant_parent([#parent{key = Key, obj = Parent} | Tail]) ->
    ParentsParents = get_unique_parents([Key]),
    case [K || K <- ParentsParents, lists:keymember(K, 2, Tail)] of
	[] ->
	    Parent;
	_ ->
	    most_significant_parent(Tail)
    end;
most_significant_parent([]) ->
    false;
most_significant_parent(Parents) ->
    most_significant_parent([#parent{key = relative_key(Parent),
				     obj = Parent} ||
				Parent <- Parents]).

%%% ###########################################################################
%%% parse_event
%%%
%%% ###=====================================================================###
parse_event({ignorableWhitespace, _}, _LineNo, State) ->
    State;
%% ------- common --------------------------------------------------------------
parse_event(?startElement_Attrs(Name),
	    _LineNo,
	    #state{parsing = #parsing{data = Data} = Pars
		  } = State) ->
    Elem = {list_to_atom(Name), elemAttrs(Attrs)},
    State#state{parsing = Pars#parsing{data = [Elem | Data]}};
parse_event({characters, Chars},
	    _LineNo,
	    #state{parsing = #parsing{data = [{Name, _} | DataTail]} = Pars
		  } = State) ->
    Value =
	case Name of
	    description ->
		term_to_binary(Chars);
	    _ ->
		Chars
	end,
    State#state{parsing = Pars#parsing{data = [{Name, Value} | DataTail]}};
%% ------- startClass ----------------------------------------------------------
parse_event(?startElement_Attrs("class"),
	    _FileName,
	    #state{parsing = undefined,
		   mimName = MimNameProp,
		   ecimMomName = EcimMomNameProps
		  } = State) ->
    {_, Name} = lists:keyfind(name, 1, elemAttrs(Attrs)),
    Props = elemAttrs(Attrs) ++ [MimNameProp | EcimMomNameProps],
    State#state{parsing = #parsing{topElement = class,
				   data = [{class, Props}]},
		objectName = Name};
parse_event(?endElement("class"),
	    _LineNo,
	    #state{parsing = #parsing{topElement = class,
				      data = [{class, Props}]},
		   objectName = ObjName,
		   class = Classes
		  } = State) ->
    NewClass = {class, {ObjName, make_ref()}, Props},
    State#state{parsing = undefined,
		class = [NewClass | Classes]};
%% ------- startRelationship ---------------------------------------------------
parse_event(?startElement_Attrs("relationship"),
	    _LineNo,
	    #state{parsing = undefined} = State) ->
    Props = elemAttrs(Attrs),
    State#state{parsing = #parsing{topElement = relationship,
				   data = [{relationship, Props}]}};
parse_event(?endElement("relationship"),
	    _LineNo,
	    #state{parsing = #parsing{topElement = relationship,
				      data = [{relationship, Props}]}
		  } = State) ->
    insert_relations(Props),
    State#state{parsing = undefined};
%% ------- startEnum ----------------------------------------------------------
parse_event(?startElement_Attrs("enum"),
	    _FileName,
	    #state{mimName = MimNameProp,
		   ecimMomName = EcimMomNameProps,
		   parsing = undefined
		  } = State) ->
    Props = elemAttrs(Attrs) ++ [MimNameProp | EcimMomNameProps],
    State#state{parsing = #parsing{topElement = enum,
				   data = [{enum, Props}]}};
parse_event(?endElement("enum"),
	    _LineNo,
	    #state{parsing = #parsing{topElement = enum,
				      data = [{enum, Props}]}
		  } = State) ->
    [ets:insert(?TblName_tmp_enum, {Key, Props}) || Key <- props_keys(Props)],
    State#state{parsing = undefined};
%% ------- startDerivedDataType ------------------------------------------------
parse_event(?startElement_Attrs("derivedDataType"),
	    _FileName,
	    #state{mimName = MimNameProp,
		   ecimMomName = EcimMomNameProps,
		   parsing = undefined
		  } = State) ->
    Props = elemAttrs(Attrs) ++ [MimNameProp | EcimMomNameProps],
    State#state{parsing = #parsing{topElement = derivedDataType,
				   data = [{derivedDataType, Props}]}};
parse_event(?endElement("derivedDataType"),
	    _LineNo,
	    #state{parsing = #parsing{topElement = derivedDataType,
				      data = [{derivedDataType, Props}]}
		  } = State) ->
    [ets:insert(?TblName_tmp_derivedDataType, {Key, Props}) ||
	Key <- props_keys(Props)],
    State#state{parsing = undefined};
%% ------- startStruct ---------------------------------------------------------
parse_event(?startElement_Attrs("struct"),
	    _FileName,
	    #state{mimName = MimNameProp,
		   ecimMomName = EcimMomNameProps,
		   parsing = undefined
		  } = State) ->
    Props = elemAttrs(Attrs) ++ [MimNameProp | EcimMomNameProps],
    State#state{parsing = #parsing{topElement = struct,
				   data = [{struct, Props}]}};
parse_event(?endElement("struct"),
	    _LineNo,
	    #state{parsing = #parsing{topElement = struct,
				      data = [{struct, Props}]}
		  } = State) ->
    [ets:insert(?TblName_tmp_struct, {Key, Props}) || Key <- props_keys(Props)],
    State#state{parsing = undefined};
%% ------- common ---------------------------------------------------------
parse_event(?endElement(_),
	    _LineNo,
	    #state{parsing = #parsing{data = Data} = Pars
		  } = State) ->
    State#state{parsing = Pars#parsing{data = parse_endElement(Data)}};
%% ------- mim -----------------------------------------------------------------
parse_event(?startElement_Attrs("extension"),
	    _LineNo,
	    #state{parsing = undefined} = State) ->
    Elems = elemAttrs(Attrs),
    case lists:keyfind(name, 1, Elems) of
	{_, "ecimMomName"} ->
	    {_, EcimMomName} = lists:keyfind(value, 1, Elems),
	    State#state{ecimMomName = [{ecimMomName, EcimMomName}]};
	_ ->
	    State
    end;
%% ------- mim -----------------------------------------------------------------
parse_event(?startElement_Attrs("mim"),
	    _LineNo,
	    #state{parsing = undefined} = State) ->
    {_, MimName} = lists:keyfind(name, 1, elemAttrs(Attrs)),
    State#state{mimName = {mimName, MimName}};
%% ------- other ---------------------------------------------------------------
parse_event(_Event, _LineNo, State) ->
    State.

%%% ###########################################################################
%%% parse_endElement
%%%
%%% ###=====================================================================###
parse_endElement([ElementData, {PrevTag, PrevProps} | Tail]) ->
    [{PrevTag, PrevProps ++ [ElementData]} | Tail].

%%% ###########################################################################
%%% props_keys
%%% Takes a list that must contain the 'name' and 'mimName' properties
%%% and optionally an 'ecimMomName' property. The returned value is
%%% [{Name, MimName}] or [{Name, MimName}, {Name, EcimMomName}].
%%%
%%% ###=====================================================================###
-spec props_keys(any()) -> [{string(), any()}].

props_keys(Props) ->
    {_, Name} = lists:keyfind(name, 1, Props),
    {_, MimName} = lists:keyfind(mimName, 1, Props),
    EcimMomNames =
	case lists:keyfind(ecimMomName, 1, Props) of
	    {_, _} = EcimMomNameProp ->
		[EcimMomNameProp];
	    _ ->
		[]
	end,
    [{Name, MimName} |
     [{Name, EcimMomName} || {_, EcimMomName} <- EcimMomNames,
			     EcimMomName /= MimName]].

%%% ###########################################################################
%%% relative_key
%%%
%%% ###=====================================================================###
relative_key({_, Props}) ->
    case lists:keyfind(name, 1, Props) of
	{_, Name} ->
	    {_, MimName} = lists:keyfind(mimName, 1, Props),
	    {Name, MimName};
	_ ->
	    relative_key(lists:keyfind(hasClass, 1, Props))
    end;
relative_key(_) ->
    false.

%%% ###########################################################################
%%% replace_ref
%%%
%%% ###=====================================================================###
replace_ref([{Tag, Props} | Tail], TargetList) ->
    case find_target(TargetList, {Tag, Props}) of
	false ->
	    [{Tag, replace_ref(Props, TargetList)} |
	     replace_ref(Tail, TargetList)];
	{true, TagReal, TblName} ->
	    case find_referenced_elem(TblName, TagReal, Props) of
		{MyTag, IncompleteMyProps} ->
		    MyProps = replace_ref(IncompleteMyProps, TargetList);
		false ->
		    {MyTag, MyProps} = {Tag, Props}
	    end,
	    [{MyTag, MyProps} |
	     replace_ref(Tail, TargetList)]
    end;
replace_ref(StringOrEmpty, _) ->
    StringOrEmpty.

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 4     CODE FOR TEMPORARY CORRECTIONS
%%% ###---------------------------------------------------------------------###
