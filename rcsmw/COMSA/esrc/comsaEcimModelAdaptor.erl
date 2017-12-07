%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaEcimModelAdaptor.erl %
%%% @author etxarnu
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R10A/4
%%%
%%% @doc == ECIM model adaptor ==
%%% This module contains various functions needed for comsaGeneric
%%% which are artefacts from the ECIM model, but which the
%%% comsaGeneric has no knowledge about.

-module(comsaEcimModelAdaptor).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R10A/4').
-date('2017-06-27').
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
%%% R1A/1      2012-01-10 etxbjca     Created
%%% R1A/7      2012-06-29 etxjotj     Enums for CertM (test)
%%% R2A/4      2013-01-31 etxjotj     Dynamic discovery of derived types
%%% R2A/18     2014-01-24 erarafo     Elaborated the Edoc
%%% R2A/19     2014-01-24 erarafo     Comments-only changes
%%% R2A/20     2014-01-24 erarafo     Edoc errors fixed hopefully
%%% R2A/21     2014-01-27 erarafo     Groundwork for AVC
%%% R2A/22     2014-01-29 erarafo     More AVC preparations
%%% R2A/23     2014-02-03 erarafo     Accept any immNamespace
%%% R2A/24     2014-02-05 erarafo     Require NONE or MOM_NAME for immNamespace
%%% R2A/25     2014-02-12 erarafo     Handle multiple mim/interMim in models
%%% R2A/26     2014-02-12 erarafo     Corrected fault in R2A/25
%%% R2A/27     2014-02-13 erarafo     Corrected fault in R2A/26
%%% R2A/28     2014-02-13 erarafo     Cleanup, functionally transparent
%%% R2A/29     2014-02-13 erarafo     Check that splitImmDn is well specified
%%% R2A/30     2014-02-21 erarafo     Check splitImmDn for IMM models only
%%% R2A/31     2014-02-21 erarafo     Comments-only change
%%% R2A/32     2014-02-21 erarafo     Bugfix
%%% R2A/33     2014-03-26 erarafo     TODO added
%%% R2A/34     2014-04-09 erarafo     splitImmDn checking relaxed
%%% R2A/36     2014-06-23 etxarnu     Use sysEnv:com_top() to find COM top
%%% R3A/1      2014-11-04 etxjotj     Fix for double parents
%%% R3A/2      2014-11-04 etxjotj     Fix for double parents, part 2
%%% R3A/4      2015-03-11 erarafo     Some type definitions moved to .hrl
%%% R4A/3      2015-08-27 etxjotj     Undefined type
%%% ----    ---------- -------  ------------------------------------------------
%%% R5A/1   2016-01-08 etxpejn  Correction for gracePeriod AVC in do_get_dn
%%% R5A/2   2016-01-13 etxberb  Parallelized parsing
%%% R6A/1   2016-05-17 erarafo  Merge from R5A: HU78363 workaround, get_dn_mim_and_class/1 correction
%%% ----    ---------- -------  ------------------------------------------------
%%% R10A/2  2017-06-22 etxpeno  Support for dev_patches when validating XML
%%% R10A/4  2017-06-27 etxarnu  Test on MKCPI instead of NODE_TYPE
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([type/2]).
-export([parse_models/2]).
-export([get_class_mim/1]).
-export([get_struct_fields/2]).
-export([get_class_types/2]).
-export([get_model_classes/1]).
-export([get_dn/3]).
-export([get_rdn_mim/1]).
-export([get_dn_mim_and_class/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([parse_model/1,
	 parse_interMim/2]).

-include("comte_types.hrl").
-include("ComsaEcimModelAdaptor.hrl").

-define(MonoTime, erlang:monotonic_time()).

-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Parse models to extract various ecim info.
%%% Data is stored in the comsaEcimTypes, comsaEcimRelations,
%%% comsaEcimClassTypes, comsaEcimImplementationModel and
%%% comsaEcimModelByRootRdn tables (they are created by
%%% comsaDataInit:init_tables()).
%%%
%%% The parse_models/1 function is called from comsaDataInit. Other
%%% blocks can then use functions in this module to fetch model data.
%%% @end

-spec parse_models([string()], mnesia|imm) -> ok.

parse_models(Models, Domain) ->

    T0 = ?MonoTime,
    ets:new(relations, [public, named_table, ordered_set, {keypos, 1}]),

    {ok, ParseModelResults} =
	sysUtil:parallel_call([{?MODULE, parse_model, [Model]}
			       || Model <- Models]),
    ParsedItems =
	lists:append([case ParseModelResult of
			  {ok, ParsedItem} ->
			      ParsedItem
		      end
		      || ParseModelResult <- ParseModelResults]),
    T1 = ?MonoTime,

    DistinctTopEs =
	sets:from_list(
	  [{TopE, Model}
	   ||#parsedItems{model=Model, topElement=TopE} <- ParsedItems]),

    T2 = ?MonoTime,
    {ok, ParseInterMimResults} =
	sysUtil:parallel_call([{?MODULE, parse_interMim, [Arguments, Domain]}
			       || Arguments <- sets:to_list(DistinctTopEs)]),
    Complaints =
	lists:append([case ParseInterMimResult of
			  {ok, Complaint} ->
			      Complaint
		      end
		      || ParseInterMimResult <- ParseInterMimResults]),
    T3 = ?MonoTime,
    if
	Complaints =:= [] ->
	    ok;
	true ->
	    sysInitI:warning_report(Complaints)
    end,

    store_relations(),
    T4 = ?MonoTime,
    ets:delete(relations),

    [parse_nameSpace(Arguments)||Arguments <- ParsedItems],

    T5 = ?MonoTime,
    %% Only print this in real environment, not when running mkcpi
    %% Use the $MKCPI as indicator that the mkcpi script is running
    case os:getenv("MKCPI") of
	false ->
	    error_logger:info_report(
	      [{?MODULE, ?FUNCTION},
	       {{'parse_model', length(ParsedItems)},
		sysUtil:time_to_string(T1 -T0)},
	       {{'DistinctTopEs', length(sets:to_list(DistinctTopEs))},
		sysUtil:time_to_string(T2 - T1)},
	       {'parse_interMim',
		sysUtil:time_to_string(T3 - T2)},
	       {'store_relations', sysUtil:time_to_string(T4 -T3)},
	       {'parse_nameSpace', sysUtil:time_to_string(T5 -T4)},
	       "-------------- Summary --------------",
	       {'TOTAL', sysUtil:time_to_string(T5 - T0)}]);
	_ ->
	    ok
    end,
    ok.


%%% ----------------------------------------------------------
%%% @doc Adds COM type value to a return value.
%%% Dynamic lookup for enums and derived datatypes.
%%% @end

type(string, undefined) -> [?STRING];
type(string, Value) when is_list(Value) ->
    [?STRING, unicode:characters_to_binary(Value)];
type(string, Value) when is_binary(Value) ->
    [?STRING, Value];

type(moRef, undefined) -> [?REFERENCE];
type(moRef, Value) when is_list(Value) ->
    [?REFERENCE,list_to_binary(Value)];
type(moRef, Value) when is_binary(Value) -> [?REFERENCE, Value];

type(uint8, undefined) -> [?UINT8];
type(uint16, undefined) -> [?UINT16];
type(uint32, undefined) -> [?UINT32];
type(uint64, undefined) -> [?UINT64];
type(int8, undefined) -> [?INT8];
type(int16, undefined) -> [?INT16];
type(int32, undefined) -> [?INT32];
type(int64, undefined) -> [?INT64];
type(boolean, undefined) -> [?BOOL];

type(enum, undefined) -> [?ENUM];

type(uint8, Value) -> [?UINT8, Value];
type(uint16, Value) -> [?UINT16, Value];
type(uint32, Value) -> [?UINT32, Value];
type(uint64, Value) -> [?UINT64, Value];
type(int8, Value) -> [?INT8, Value];
type(int16, Value) -> [?INT16, Value];
type(int32, Value) -> [?INT32, Value];
type(int64, Value) -> [?INT64, Value];
type(boolean, Value) -> [?BOOL, Value];

type(enum, Value) -> [?ENUM, Value];

%%% Types are now discovered by parsing of models.
%%% No need for hard coding

type({sequence, Type}, []) ->
    [hd(type(Type, []))];
type({sequence, Type}, undefined) ->
    [hd(type(Type, []))];
type({sequence, Type}, Values) ->
    type(Type, hd(Values))++
	[lists:nth(2, type(Type, Value))||Value<-tl(Values)];

type({struct, X}, Value) ->
    erlang:error(structs_not_supported, [{struct,X}, Value]);

type(Type, Value) when is_atom(Type) ->
    case mnesia:dirty_read({comsaEcimTypes, Type}) of
	[] ->
	    erlang:error(type_definition_unknown, [Type, Value]);
	[#comsaEcimTypes{type=Type, basicType=BasicType}] ->
	    type(BasicType, Value)
    end.

%%% ----------------------------------------------------------
%%% @doc Returns the MIM in which the leaf item of a given DN belongs.
%%% The DN may span multiple MOMs. The DN must be given in one of two
%%% formats: Either a reversed format as shown below, where RDN values
%%% are ignored,
%%% ```
%%% get_class_mim([<<"77">>, <<"TestClass1">>, <<"1">>, <<"TestRoot">>]) ->
%%%
%%% "TESTMOM"
%%% '''
%%% or a top-down format where only classnames are present:
%%% ```
%%% get_class_mim(["TestRoot", "TestClass1"]) ->
%%%
%%% "TESTMOM"
%%% '''
%%% The given DN is trusted to be non-empty. It is considered an error
%%% if the given DN is not supported by the current model.
%%% @end

-spec get_class_mim([binary()] | [string()]) -> string().

get_class_mim(DnRev) when is_binary(hd(DnRev))->
    ClassPath = classpath(DnRev),
    get_class_mim(ClassPath, root);
get_class_mim(ClassPath) ->
    get_class_mim(ClassPath, root).


get_class_mim([], {Mim, _}) ->
    Mim;
get_class_mim([Class], root) ->
    Pattern = #comsaEcimRelations{parent={'_',Class}, child='_'},
    case mnesia:dirty_match_object(Pattern) of
	[] ->
	    erlang:error(class_unknown, [[Class]]);
	[#comsaEcimRelations{parent={Mim, _}}|_] ->
	    %% Assume there is only one top root class ever
	    Mim
    end;
get_class_mim([Class|ClassPath], root) ->
    Pattern = #comsaEcimRelations{parent={'_',Class}, child='_'},
    case mnesia:dirty_match_object(Pattern) of
	[] ->
	    erlang:error(class_unknown, [[Class|ClassPath], root]);
	[#comsaEcimRelations{parent={Mim, _}}|_] ->
	    get_class_mim(ClassPath, {Mim, Class})
    end;
get_class_mim([Class|ClassPath], Parent) ->
    Pattern = #comsaEcimRelations{parent=Parent, child={'_', Class}},
    case mnesia:dirty_match_object(Pattern) of
	[] ->
	    erlang:error(class_unknown, [[Class|ClassPath], Parent]);
	[#comsaEcimRelations{child=Child}] ->
	    get_class_mim(ClassPath, Child)
    end.


%%% ----------------------------------------------------------
%%% @doc Returns the list of class types and the creation type for a given
%%% class in a given model.
%%% @end

-spec get_class_types(Model::string(), Class::string()) ->
			     {[classType()], creationType()}.

get_class_types(Model, Class) ->
    Key = list_to_atom(Model++"."++Class),
    Fun = fun() ->
		      case mnesia:read({comsaEcimClassTypes, Key}) of
			  [#comsaEcimClassTypes{attributeTypes= AT,
						creationType = CT}] ->
			    {AT, CT};
			  [] -> {[], undefined}
		      end
	  end,
    case mnesia:transaction(Fun) of
	{atomic, Result} ->
	    Result;
	{aborted, _} ->
	    {[], undefined}
    end.

%%% ----------------------------------------------------------
%%% @doc Returns the list of classes in the given model.
%%% @end

-spec get_model_classes(Model::string()) -> [string()].

get_model_classes(Model) ->
    Pattern = #comsaEcimRelations{parent='_', child={Model, '_'}},
    Fun = fun() -> mnesia:match_object(Pattern) end,
    case mnesia:transaction(Fun) of
	{atomic, Matches} ->
	    case Model of
		"ComTop" ->
		    ["ManagedElement"|
		     [element(2, element(3, Match))||Match<-Matches]];
		_ ->
		     remove_doubles(
		       [element(2, element(3, Match))||Match<-Matches])
	    end;
	{aborted, Reason} ->
	    sysInitI:error_report(
	      [{mnesia, match_object, [Pattern]},
	       {aborted, Reason}]),
	    []
    end.

remove_doubles(List) ->
    do_remove_doubles(lists:sort(List)).

do_remove_doubles([X,X|T]) ->
    do_remove_doubles([X|T]);
do_remove_doubles([X|T]) ->
    [X|do_remove_doubles(T)];
do_remove_doubles([]) ->
    [].



%%% ----------------------------------------------------------
%%% @doc The given Key should be a tuple of strings, which are
%%% taken as RDN values. A list of binaries is returned,
%%% forming a partial DN of the given class using the given RDN values.
%%% For example,
%%% ```
%%%     get_dn({"2", "17", "beta"}, "TESTMOM", "TestClass3") ->
%%%
%%%     [<<"TestRoot=2">>,<<"TestClass2=17">>,<<"TestClass3=beta">>]
%%% '''
%%% Crossing MIM boundaries works, so providing {"1", "2", "17", "beta"}
%%% as the first argument yields a DN that starts with `<<"ManagedElement=1">>'.
%%% @end

get_dn(Key, Model, Class) ->
    Fun = fun() ->
		  do_get_dn(lists:reverse(tuple_to_list(Key)), {Model, Class})
	  end,

    case mnesia:transaction(Fun) of
	{atomic, Dn} ->
	    lists:reverse(Dn);
	{aborted, Reason} ->
	    erlang:error(Reason, [Key, Model, Class])
    end.

do_get_dn([Id], Child) ->
    [list_to_binary(element(2,Child)++"="++Id)];
do_get_dn([Id|Ids], Child) ->
    Pattern = #comsaEcimRelations{parent='_', child=Child},
    {_, ChildName} = Child,
    case mnesia:match_object(Pattern) of
	[#comsaEcimRelations{parent=Parent}] ->
	    [list_to_binary(element(2,Child)++"="++Id)|do_get_dn(Ids, Parent)];
	%% Multiple parents aren't really envisioned. Until it becomes a big
	%% problem, just handle some exceptions here.
	[#comsaEcimRelations{parent=Parent}|_]
	  when {"RcsLM", "CapacityState"}==Parent ->
	    [list_to_binary(ChildName++"="++Id)|do_get_dn(Ids, Parent)];
	[_, #comsaEcimRelations{parent=Parent}]
	  when {"RcsLM", "CapacityState"}==Parent ->
	    [list_to_binary(ChildName++"="++Id)|do_get_dn(Ids, Parent)];
	[#comsaEcimRelations{parent=Parent}|_]
	  when {"RcsLM", "FeatureState"}==Parent ->
	    [list_to_binary(ChildName++"="++Id)|do_get_dn(Ids, Parent)];
	[_, #comsaEcimRelations{parent=Parent}]
	  when {"RcsLM", "FeatureState"}==Parent ->
	    [list_to_binary(ChildName++"="++Id)|do_get_dn(Ids, Parent)]

    end.


get_struct_fields(DnRev, StructName) ->
    ClassPath = classpath(DnRev),
    Mim = comsaEcimModelAdaptor:get_class_mim(ClassPath),
    StructType =
	list_to_atom(Mim++"."++atom_to_list(StructName)),
    case mnesia:dirty_read({comsaEcimTypes, StructType}) of
	[Obj]  -> Obj#comsaEcimTypes.basicType;
	[] when StructName=='AsyncActionProgress';
		StructName=='EcimPassword';
		StructName=='ProductIdentity';
		StructName=='ProductData' ->
	    StructType2 =
		list_to_atom("ECIM_CommonLibrary"++"."++
				 atom_to_list(StructName)),
	    [Obj] = mnesia:dirty_read({comsaEcimTypes, StructType2}),
	    Obj#comsaEcimTypes.basicType;
	[] ->
	    erlang:error({struct_type_not_found, StructType},
			 [DnRev, StructName])
    end.

%%% ----------------------------------------------------------
%%% @doc Returns a triple of MIM name, ECIM class name and
%%% the ECIM DN, derived from the given IMM-style DN.
%%%
%%% The MIM name is the 'name' attribute of the 'mim' element
%%% in the MP XML.
%%%
%%% TODO: This function does not handle the case where the
%%% given DN spans multiple namespace-prefixed MOMs.
%%% @end
%%% ----------------------------------------------------------
-spec get_dn_mim_and_class(binary()) -> {string(), string(), [binary()]} | none.

get_dn_mim_and_class(DnB) ->
    DnS = binary_to_list(DnB),
    RdnList = string:tokens(DnS, ","),
    RootRdn = lists:last(RdnList),
    [RootRdnName|_] = string:tokens(RootRdn, "="),
    case
	% mnesia:dirty_read({comsaEcimModelByRootRdn, RootRdnName})
	hu78363_lookup(RootRdnName)
	of
	[] ->
	    none;
	[#comsaEcimModelByRootRdn{mimInfo={MimName, Prefixing}}] ->
	    [InstanceRdn|_] = RdnList,
	    [InstanceRdnName|_] = string:tokens(InstanceRdn, "="),
	    [A|X] =
		if
		    Prefixing andalso length(RdnList) =:= 1 ->
			string:substr(InstanceRdnName,
				      length(MimName)+1,
				      length(InstanceRdnName)-length(MimName)-2);
		    true ->
			string:substr(InstanceRdnName,
				      1,
				      length(InstanceRdnName)-2)
		end,
	    Class = [string:to_upper(A)|X],
	    {MimName, Class, getDn(RdnList, Prefixing, MimName)}
    end.

%%% ----------------------------------------------------------
%%% @doc Fault-tolerant lookup; workaround for HU78363, to be removed
%%% as soon as LRAT have adjusted their IMM XML.
%%% @end
%%% ----------------------------------------------------------
hu78363_lookup(RootRdnName) ->
    case re:run(RootRdnName, <<"[Ll]rat[Ee]NodeBFunctionId">>, [anchored, {capture, none}]) of
	nomatch ->
	    mnesia:dirty_read({comsaEcimModelByRootRdn, RootRdnName});
	match ->
	    lists:foldl(
	      fun(Case, []) ->
		      case mnesia:dirty_read({comsaEcimModelByRootRdn, Case}) of
			  [] ->
			      [];
			  [_]=U ->
			      U
		      end;
		 (_Case, Acc) ->
		      Acc
	      end,
	      [],
	      ["LrateNodeBFunctionId",
	       "lratENodeBFunctionId",
	       "lrateNodeBFunctionId",
	       "LratENodeBFunctionId"])
    end.


%%% ----------------------------------------------------------
%%% @doc Converts an IMM-style DN to an ECIM DN. The given IMM
%%% DN is a list of strings, each string being an RDN. The last
%%% of the RDNs will be de-prefixed if applicable. The resulting
%%% ECIM-style DN is returned as a list of binaries.
%%% @end
%%% ----------------------------------------------------------
-spec getDn([string()], boolean(), string()) -> [binary()].

getDn(RdnList, Prefixing, MimName) ->
    [RevRdnListHead|RevRdnListTail] = lists:reverse(RdnList),
    [list_to_binary(
       getRdn(
     if
	 Prefixing ->
		 string:substr(RevRdnListHead, 1+length(MimName));
	     true ->
		 RevRdnListHead
	 end))
    |
	 [list_to_binary(getRdn(A)) || A <- RevRdnListTail]].


%%% ----------------------------------------------------------
%%% @doc Transforms a string like this: "fooBarId=56" -> "FooBar=56"
%%% @end
%%% ----------------------------------------------------------
-spec getRdn(string()) -> string().

getRdn(A) ->
    [AN, AV] = string:tokens(A, "="),
    [Q|ANSR] = string:substr(AN, 1, length(AN)-2),
    [string:to_upper(Q)|ANSR]++"="++AV.


%%% ----------------------------------------------------------
%%% @doc Returns the model name (MIM name) for the given
%%% root class RDN. The given RDN is trusted to be well-formed.
%%% The RDN value is ignored.
%%%
%%% TODO, remove this function.
%%% @end

-spec get_rdn_mim(string()) -> string().

get_rdn_mim(RdnS) ->
    [RdnName|_] = string:tokens(RdnS, "="),
    case mnesia:dirty_read({comsaEcimModelByRootRdn, RdnName}) of
	[] ->
	    undefined;
	[#comsaEcimModelByRootRdn{mimInfo={MimName, _}}] ->
	    MimName
    end.


classpath(DnRev) ->
    classpath([binary_to_list(X)||X<-DnRev], []).


classpath([_, Field|Dn],Acc) ->
    classpath(Dn, [Field|Acc]);
classpath([], Acc) ->
    Acc.



%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%some_method(Parameter)->
%   nn.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%internal_function1(One, Two)->
%   nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% ----------------------------------------------------------
%%% #           parse_model(SrcPath)
%%% Input: SrcPath:string() - Path to a model
%%% Output: ok
%%% Exceptions:
%%% Description: Parse a model for enums and derived types. Store in
%%%              comsaEcimTypes table
%%% ----------------------------------------------------------

-spec parse_model(string()) -> {ok, [#parsedItems{}]}.

parse_model(SrcPath) ->
    IncPattern = filename:join([sysEnv:com_top(), "opt", "com", "etc", "model"]),
    ModelDirs = filelib:wildcard(IncPattern),

    %% Only include dev_patches in real environment, not when running mkcpi
    %% Use the $MKCPI as indicator that the mkcpi script is running
    IncDirs =
	case os:getenv("MKCPI") of
	    false ->
		[sysEnv:dev_patches_dir()|ModelDirs];
	    _ ->
		ModelDirs
	end,
    Options = [{validation, dtd}, {fetch_path, IncDirs}],
    {TopE, _ } =
	case xmerl_scan:file(SrcPath, Options) of
	    {error, Reason} -> erlang:error(Reason, [SrcPath, IncDirs]);
	    Result -> Result
	end,
    {ok, [parse_mim(TopE, MimE, SrcPath, IncDirs)
	  ||MimE <- find_elements(mim, TopE)]}.


%%% ----------------------------------------------------------
%%% @doc Parses one mim within a given model, returning a
%%% 5-tuple of results to be used by subsequent parse tasks.
%%% @end
%%% ----------------------------------------------------------

-spec parse_mim(#xmlElement{}, #xmlElement{}, string(), [string()]) ->
	  #parsedItems{}.

parse_mim(TopE, MimE, SrcPath, IncDirs) ->
    MimName = find_attribute(name, MimE),
    EcimName = find_extension(MimE, "ecimMomName"),
    case EcimName of
	undefined ->
	    ok;
	_ ->
	    mnesia:dirty_write(
	      #comsaEcimImplementationModel{ecimName=EcimName,
					    mimName=MimName})
    end,

    EnumElements = find_enums(MimE),
    DrtElements = find_drts(MimE),
    StructElements = find_structs(MimE),
    ClassElements = find_classes(MimE),

    [register_class(MimName, ClassE) || ClassE <- ClassElements],
    [register_enum(MimName, EcimName, EnumE) || EnumE <- EnumElements],
    [register_derived_datatype(MimName, EcimName, DrtE) || DrtE <- DrtElements],
    [register_struct(MimName, StructE) || StructE <- StructElements],

    InternalRelationships = find_relationships(MimE),
    [register_relationship(Relationship)||
       Relationship<-InternalRelationships],

    #parsedItems{model=SrcPath,
		 incDirs=IncDirs,
		 topElement=TopE,
		 mimElement=MimE,
		 mimName=MimName}.


%%% ----------------------------------------------------------
%%% @doc Parses all interMim elements under the
%%% given top element, building a table of relations. A list
%%% of complaints is returned. The format of a complaint is
%%% {splitImmDn, _}.
%%% @end
%%% ----------------------------------------------------------

-spec parse_interMim({#xmlElement{}, string()}, mnesia|imm) ->
	  {ok, [{splitImmDn, any()}]}.

parse_interMim({TopE, SrcPath}, Domain) ->
    try
	InterMimEs = find_elements(interMim, TopE),
	{ok, lists:append(
	       [begin
		    ExternalRelationships = find_relationships(InterMimE),
		    [register_relationship(Relationship)
		     ||Relationship <- ExternalRelationships],
		    if
			Domain =:= imm ->
			    check_splitImmDn(ExternalRelationships, SrcPath);
			true ->
			    []
		    end
		end
		||InterMimE <- InterMimEs])}
    catch
	ExType:ExData ->
	    % TODO, consider doing erlang:error here since
	    % if we get here then some relationships may go
	    % unregistered.
	    sysInitI:warning_report(
	      [{mfa, {?MODULE, parse_interMim, [SrcPath]}},
	       {parse_interMim, {ExType, ExData}},
	       {stack, erlang:get_stacktrace()}
	      ]),
	    {ok, []}
    end.


%%% ----------------------------------------------------------
%%% @doc Checks that each of the given interMim elements
%%% specify splitImmDn=false properly. Returns a list of
%%% complaints.
%%% @end
%%% ----------------------------------------------------------

-spec check_splitImmDn([#xmlElement{}], string()) -> [{splitImmDn, any()}].

check_splitImmDn(RelationshipEs, SrcPath) ->
    lists:append(
      [case parentIsManagedElement(RelationshipE) of
	   true ->
	       [];
	   false ->
	       try find_element(domainExtension, RelationshipE) of
		   DomExtE ->
		       ExtensionEs = find_elements(extension, DomExtE),
		       ProperExtensionEs =
			   lists:append(
			     [find_named_value("splitImmDn", ExtensionE)
				||ExtensionE <- ExtensionEs]),
		       case ProperExtensionEs of
			   [missing_name] ->
			       [{splitImmDn, {missing_name, SrcPath}}];
			   [missing_value] ->
			       [{splitImmDn, {missing_value, SrcPath}}];
			   ["false"] ->
			       [];
			   [Other] ->
			       [{splitImmDn, {unsupported, Other, SrcPath}}];
			   [] ->
			       [{splitImmDn, {missing, SrcPath}}];
			   [_|_] ->
			       [{splitImmDn, {ambiguous, SrcPath}}]
		       end
	       catch
		   _:_ ->
		       [{splitImmDn, {missing, SrcPath}}]
	       end
       end
       ||#xmlElement{}=RelationshipE <- RelationshipEs]).


%%% ----------------------------------------------------------
%%% @doc Returns true if the parent end of the containment
%%% relationship is "ManagedElement".
%%% @end
%%% ----------------------------------------------------------

-spec parentIsManagedElement(#xmlElement{}) -> boolean().

parentIsManagedElement(RelationshipE) ->
    ContainmentE = find_element(containment, RelationshipE),
    ParentE = find_element(parent, ContainmentE),
    HasClassE = find_element(hasClass, ParentE),
    case find_attribute(name, HasClassE) of
	"ManagedElement" ->
	    true;
	_ ->
	    false
    end.


%%% @doc Assume the given XML element has this format,
%%% ```
%%%     <SOMETAG name="abc" value="xyz"/>
%%% '''
%%% the invocation
%%% ```
%%%     find_named_value("abc", Element)
%%% '''
%%% will then return ["xyz"]. If the 'name' attribute is missing, or
%%% if the attribute value does not match the given string then
%%% [missing_name] is returned, else if the 'value' attribute
%%% is missing then [missing_value] is returned.

-spec find_named_value(string(), #xmlElement{}) ->
	  [string() | missing_name | missing_value].

find_named_value(Name, ExtensionE) ->
    try find_attribute(name, ExtensionE) of
	Name ->
	    try find_attribute(value, ExtensionE) of
		Value ->
		    [Value]
	    catch
		_:_ ->
		    [missing_value]
	    end;
	_ ->
	    [missing_name]
    catch
	_:_ ->
	    [missing_name]
    end.


%%% ----------------------------------------------------------
%%% @doc Additional parsing; the purpose is to prepare a table
%%% that maps root class RDN names to model names (MIM names).
%%% @end

-spec parse_nameSpace(#parsedItems{}) -> any().

parse_nameSpace(#parsedItems{model=SrcPath, incDirs=IncDirs, mimElement=MimE, mimName=MimName}) ->

    ImmNamespace =
	case find_extension(MimE, "immNamespace") of
	    undefined ->
		"NONE";
	    "NONE" ->
		"NONE";
	    "MOM_NAME" ->
		"MOM_NAME";
	    Other ->
		erlang:error("unknown immNamespace value",
			     [Other, SrcPath, IncDirs])
	end,

    RootRdnNames = find_imm_root_rdn_names(MimName, ImmNamespace),

    [mnesia:dirty_write(
       #comsaEcimModelByRootRdn{rootRdnName=RootRdnName,
				mimInfo={MimName,
					 (ImmNamespace =:= "MOM_NAME")}})
    ||RootRdnName <- RootRdnNames].

%%% ----------------------------------------------------------
%%% @doc Finds the value of the given domain extension. If not
%%% found, 'undefined' is returned.
%%% @end

-spec find_extension(#xmlElement{}, string()) -> string() | undefined.

find_extension(MimE, ExtensionName) ->
    try find_element(domainExtension, MimE) of
	DomainExtensionE  ->
	    case find_attribute(domain, DomainExtensionE) of
		"ECIM" ->
		    case
			[find_attribute(value, Element)||
			   Element<-DomainExtensionE#xmlElement.content,
			   Element#xmlElement.name == extension,
			   ExtensionName == find_attribute(name, Element)] of
			[Name] ->
			    Name;
			_ ->
			    undefined
		    end;
		_ ->
		    undefined
	    end
    catch _:_ ->
	      undefined
    end.

find_classes(MimE) ->
    [Element||Element<-MimE#xmlElement.content,
	      element(#xmlElement.name, Element)==class].

register_class(Mim, ClassE) ->
    ClassName = find_attribute(name, ClassE),
    CreationType = find_creation_type(ClassE),
    Attributes = get_class_attributes(ClassE),


    mnesia:dirty_write(#comsaEcimClassTypes{
			  class = list_to_atom(Mim++"."++ClassName),
			  attributeTypes = attribute_types(Attributes),
			  creationType = CreationType}).

find_creation_type(ClassE) ->
    case [SystemCreatedE||SystemCreatedE<-ClassE#xmlElement.content,
			  SystemCreatedE#xmlElement.name == systemCreated] of
	[] ->
	    userCreated;
	_ ->
	    systemCreated
    end.


get_class_attributes(ClassE) ->
    {Keys, OtherAttributes} =
	lists:foldl(
	  fun(AttributeE, {K,A}) when AttributeE#xmlElement.name==attribute ->
		  case is_key(AttributeE) of
		      true ->
			  {K++[AttributeE],A};
		      false ->
			  {K,A++[AttributeE]}
		  end;
	     (_, {K,A}) ->
		  {K,A}
	  end, {[],[]}, ClassE#xmlElement.content),

    case length(Keys) of
	0 ->
	    OtherAttributes;
	1 ->
	    Keys++OtherAttributes;
	_  ->
	    KeyAttr = fake_key(),
	    [KeyAttr|OtherAttributes]
    end.


is_key(AttributeE) ->
    try find_element(key, AttributeE) of
	_ -> true
    catch _:_ -> false
    end.

fake_key() ->
    DataType = #xmlElement{name=dataType,
			   content=[#xmlElement{name=string}]},
    #xmlElement{name=attribute,
		attributes= [#xmlAttribute{name=name,
					   value="key"}],
		content= [DataType]}.

attribute_types(Attributes) ->
    [begin
	 Name = find_attribute(name, AttributeE),
	 DataType = find_datatype(AttributeE),
	 Access = find_access(AttributeE),
	 Notification = find_notification(AttributeE),
	 {list_to_atom(Name), format_datatype(DataType), Access, Notification}
     end||AttributeE<-Attributes].


find_access(AttributeE) ->
    case [ReadOnlyE||ReadOnlyE<-AttributeE#xmlElement.content,
		     ReadOnlyE#xmlElement.name == readOnly] of
	[] ->
	    readWrite;
	_ ->
	    readOnly
    end.

find_notification(AttributeE) ->
    case [NoNotifE||NoNotifE<-AttributeE#xmlElement.content,
		    NoNotifE#xmlElement.name == noNotification] of
	[] ->
	    notification;
	_ ->
	    noNotification
    end.

find_datatype(AttributeE) ->
    DataTypeE = find_element(dataType, AttributeE),
    try find_element(sequence, DataTypeE) of
	SequenceE ->
	    {sequence, find_subtypes(SequenceE)}
    catch _:_ ->
	    find_subtypes(DataTypeE)
    end.

find_subtypes(DataTypeE) ->
    find_subtypes(DataTypeE, [enumRef, derivedDataTypeRef, structRef]).

find_subtypes(DataTypeE, [RefType|RefTypes]) ->
   %% io:format("find_subtypes(~p, ~p)~n",[DataTypeE,  [RefType|RefTypes]]),
    try find_element(RefType, DataTypeE) of
	RefE ->
	    ReferencedType = find_attribute(name, RefE),
	    case RefType of
		structRef -> {RefType, ReferencedType};
		enumRef ->
		    try find_element(mimName, RefE) of
			MimNameE ->
			    MimName = find_text(MimNameE),
			    {enumRef, MimName++"."++ReferencedType}
		    catch _:_ ->
			    info_msg("Could not find a mimName element "
				     "for ~s. Referring to this mim.~n",
				     [ReferencedType]),
			    {enumRef, get(local_mim_name)++"."++ReferencedType}
		    end;

		_ ->
		    MimNameE = find_element(mimName, RefE),
		    MimName = find_text(MimNameE),
		    {RefType, MimName++"."++ReferencedType}
	    end
    catch _:_ ->
	    find_subtypes(DataTypeE, RefTypes)
    end;

find_subtypes(DataTypeE, []) ->
    Element = find_first_element(DataTypeE#xmlElement.content),
    {element, Element}.

format_datatype({sequence, {element, SequenceE}}) when SequenceE#xmlElement.name == sequence ->
    Element = find_first_element(SequenceE#xmlElement.content),
    {sequence, format_datatype({element, Element})};
format_datatype({sequence, DataType}) ->
    {sequence, format_datatype(DataType)};

format_datatype({element, Element}) -> Element#xmlElement.name;
format_datatype({derivedDataTypeRef, NameRef}) -> list_to_atom(NameRef);
format_datatype({enumRef, NameRef}) -> list_to_atom(NameRef);
format_datatype({struct, NameRef}) -> {struct, list_to_atom(NameRef)};
format_datatype({structRef, NameRef}) -> {struct, list_to_atom(NameRef)};
format_datatype({moRef, _}) -> moRef.


register_struct(Mim, StructE) ->
    put(local_mim_name, Mim),
    StructName = find_attribute(name, StructE),
    PrefixName = list_to_atom(Mim++"."++StructName),
    Members = [Member||Member<-StructE#xmlElement.content,
		       Member#xmlElement.name==structMember],
    Fields = [{list_to_atom(find_attribute(name, Member)),
	       format_datatype(find_struct_datatype(Member))}||
		 Member<-Members],

    mnesia:dirty_write(#comsaEcimTypes{type=PrefixName, basicType=Fields}).

find_structs(MimE) ->
    [Element||Element<-MimE#xmlElement.content,
	      element(#xmlElement.name, Element)==struct].

availableTypes() ->
    primitiveTypes() ++ [structRef, derivedDataTypeRef, sequence].

primitiveTypes() ->
    corbaTypes() ++ yangTypes() ++ [enumRef, moRef].

yangTypes() ->
    [int8, int16, int32, int64, uint8, uint16, uint32, uint64].

corbaTypes() ->
    [boolean, octet, char, double, float, long, longlong, short, string, wstring].

find_struct_datatype(StructMemberE) ->
%    io:format("find_struct_datatype(~p)~n",[StructMemberE]),
    Types = availableTypes(),
    Elements = [Element||Element<-StructMemberE#xmlElement.content,
			 is_record(Element, xmlElement) andalso
			 lists:member(Element#xmlElement.name, Types)],
    case Elements of
	[TypeE] ->
	    case TypeE#xmlElement.name of
		derivedDataTypeRef ->
		    Name = find_attribute(name, TypeE),
		    try find_element(mimName, TypeE) of
			MimNameE ->
			    MimName = find_text(MimNameE),
			    {derivedDataTypeRef, MimName++"."++Name}
		    catch _:_ ->
			    info_msg("Could not find a mimName element "
				     "for ~s. Referring to ~s.~n",
				     [Name, get(local_mim_name)]),

			    {derivedDataTypeRef, get(local_mim_name)++"."++Name}
		    end;
		sequence ->
		    SeqTypes = [structRef, derivedDataTypeRef|primitiveTypes()],
		    SeqType = find_sequence_type(TypeE, SeqTypes),
		    {sequence, SeqType};
		enumRef ->
		    Name = find_attribute(name, TypeE),
		    try find_element(mimName, TypeE) of
			MimNameE ->
			    MimName = find_text(MimNameE),
			    {enumRef, MimName++"."++Name}
		    catch _:_ ->
			    info_msg("Could not find a mimName element "
				     "for ~s. Referring to ~s.~n",
				     [Name, get(local_mim_name)]),
			    {enumRef, get(local_mim_name)++"."++Name}
		    end;
		moRef ->
		    {element, TypeE};
		_ ->
		    {element, TypeE}
	    end;
	_ ->
	    erlang:error(multiple_typedefs_in_structmember, [StructMemberE])
    end.

find_sequence_type(SequenceE, [structRef|SeqTypes]) ->
    case catch find_element(structRef, SequenceE) of
	{'EXIT', _ } ->
	    find_sequence_type(SequenceE, SeqTypes);
	StructRefE ->
	    RefName = find_attribute(name, StructRefE),
	    {struct, RefName}
    end;
find_sequence_type(SequenceE, [SeqType|SeqTypes]) ->
    case catch find_element(SeqType, SequenceE) of
	{'EXIT', _ } ->
	    find_sequence_type(SequenceE, SeqTypes);
 	TypeE->
	    {element, TypeE}
    end;
find_sequence_type(SE, []) ->
    erlang:error(could_not_find_sequence_type, [SE, []]).


%%% ----------------------------------------------------------
%%% @doc Returns all 'relationship' subelements in the given
%%% element.
%%% @end
%%% ----------------------------------------------------------

-spec find_relationships(#xmlElement{}) -> [#xmlElement{}].

find_relationships(ContainerE) ->
    [Element||Element<-ContainerE#xmlElement.content,
	      element(#xmlElement.name, Element)==relationship].

register_relationship(RelationshipE) ->
    try find_element(containment, RelationshipE) of
	ContainmentE ->
	    ParentE = find_element(parent, ContainmentE),
	    ParentClassE = find_element(hasClass, ParentE),
	    Parent = find_attribute(name, ParentClassE),
	    ParentMimE = find_element(mimName, ParentClassE),
	    ParentMim = find_text(ParentMimE),
	    ChildE = find_element(child, ContainmentE),
	    ChildClassE = find_element(hasClass, ChildE),
	    Child = find_attribute(name, ChildClassE),
	    ChildMimE = find_element(mimName, ChildClassE),
	    ChildMim = find_text(ChildMimE),
	    ets:insert(relations, {erlang:make_ref(),
				   {ParentMim, Parent},
				   {ChildMim, Child}})


    catch _:_ ->
	    ok
    end.


im(Name) ->
    case mnesia:dirty_read({comsaEcimImplementationModel, Name}) of
	[#comsaEcimImplementationModel{mimName=MimName}] ->
	    MimName;
	_ ->
	    Name
    end.


store_relations() ->
    store_relations(ets:first(relations)).

store_relations('$end_of_table') ->
    ok;
store_relations(Key) ->
    [begin
	 Parent = {im(ParentMim), ParentClass},
	 Child = {im(ChildMim), ChildClass},
	 mnesia:dirty_write(#comsaEcimRelations{parent=Parent, child=Child})
     end||{_, {ParentMim, ParentClass}, {ChildMim, ChildClass}}
	      <-ets:lookup(relations, Key)],
    store_relations(ets:next(relations, Key)).



%%% ----------------------------------------------------------
%%% #           register_enum(EnumE)
%%% Input: Mim:string()
%%%        EnumE:#xmlElement{}
%%% Output:
%%% Exceptions:
%%% Description: Extract enum name and store it in comsaEcimTypes
%%% ----------------------------------------------------------

register_enum(Mim, EcimMim, EnumE) ->
    EnumName = find_attribute(name, EnumE),
    PrefixName = list_to_atom(Mim++"."++EnumName),
    mnesia:dirty_write({comsaEcimTypes, PrefixName, enum}),
    case EcimMim of
	_ when EcimMim /= undefined ->
	    EcimPrefixName = list_to_atom(EcimMim++"."++EnumName),
	    mnesia:dirty_write({comsaEcimTypes, EcimPrefixName, enum});
	undefined ->
	    ok
    end.

%%% ----------------------------------------------------------
%%% @doc Find the RDN names of the root classes of the given
%%% model (usually just one), and represent it as a binary
%%% in the format that is used in IMM instance DNs.
%%% @end
-spec find_imm_root_rdn_names(string(), string()) -> [string()].

find_imm_root_rdn_names(MimName, ImmNamespace) ->
    [begin
	 RdnTail = lowerCamelCase(RootClass) ++ "Id",
	 case ImmNamespace of
	     "NONE" ->
		 RdnTail;
	     "MOM_NAME" ->
		 MimName ++ RdnTail
	 end
     end
     || RootClass <- find_root_classes(MimName)].

%%% ----------------------------------------------------------
%%% @doc Gets the root classes of the given model. Typically
%%% a model has just one, but multiple root classes may exist,
%%% or the model may have no classes at all.
%%% @end

-spec find_root_classes(string()) -> [string()].

find_root_classes(MimName) ->
    case get_model_classes(MimName) of
	[] ->
	    [];
	[SingleClass] ->
	    [SingleClass];
	_ ->
	    Relations =
		mnesia:dirty_match_object(
		  #comsaEcimRelations{parent={MimName, '_'},
				      child={MimName, '_'}}),
	    Parents =
		ordsets:from_list(
		  [P||#comsaEcimRelations{parent={_, P}} <- Relations]),
	    Children =
		ordsets:from_list(
		  [C||#comsaEcimRelations{child={_, C}} <- Relations]),
	    ordsets:subtract(Parents, Children)
    end.

%%% ----------------------------------------------------------
%%% @doc Enforce the leading character to be lowercase.
%%% @end

-spec lowerCamelCase(string()) ->  string().

lowerCamelCase([A|T]) ->
    [string:to_lower(A)|T];

lowerCamelCase([]) ->
    [].

%%% ----------------------------------------------------------
%%% #           register_derived_datatype(DrtE)
%%% Input: Mim:atom() - Name of the mim
%%%        DrtE:#xmlElement{}
%%% Output:
%%% Exceptions:
%%% Description: Extract derived datatype name and base type and store it
%%%              in comsaEcimTypes
%%% ----------------------------------------------------------

register_derived_datatype(Mim, EcimMim, DrtE) ->
    DrtName = find_attribute(name, DrtE),
    BaseTypeE = find_element(baseType, DrtE),
    TypeE = find_first_element(BaseTypeE),
    BaseType = TypeE#xmlElement.name,
    PrefixName = list_to_atom(Mim++"."++DrtName),
    mnesia:dirty_write({comsaEcimTypes, PrefixName, BaseType}),
    case EcimMim of
	_ when EcimMim /= undefined ->
	    EcimPrefixName = list_to_atom(EcimMim++"."++DrtName),
	    mnesia:dirty_write({comsaEcimTypes, EcimPrefixName, BaseType});
	undefined ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% #           find_enums(MimE)
%%% Input: MimE:#xmlElement{}
%%% Output:
%%% Exceptions:
%%% Description: Extract all enum type definitions from mim
%%% ----------------------------------------------------------

find_enums(MimE) ->
    [Element||Element<-MimE#xmlElement.content,
	      element(#xmlElement.name, Element)==enum].

%%% ----------------------------------------------------------
%%% #           find_drts(MimE)
%%% Input: MimE:#xmlElement{}
%%% Output:
%%% Exceptions:
%%% Description: Extract all derived data types in a mim
%%% ----------------------------------------------------------
find_drts(MimE) ->
    [Element||Element<-MimE#xmlElement.content,
	      element(#xmlElement.name, Element)==derivedDataType].

%%% ----------------------------------------------------------
%%% #           find_element(ElementName, Element)
%%% #           find_element(ElementName, Content)
%%% Input: ElementName:atom()
%%%        Element:#xmlElement{} or
%%%        Content.[#xmlElement{}] a list of elements
%%% Output: #xmlElement{}
%%% Exceptions:
%%% Description: Finds a sub element to an xml element, or in a list
%%%              of element contents. Assumes there is only one element
%%%              with the same name
%%% ----------------------------------------------------------

-spec find_element(atom(),
		   #xmlElement{} | [#xmlElement{}]) -> #xmlElement{}.

find_element(ElementName, Element) when is_record(Element, xmlElement) ->
    find_element(ElementName, Element#xmlElement.content);
find_element(ElementName, ContentList) ->
    {value, Element} =
	lists:keysearch(ElementName, #xmlElement.name, ContentList),
    Element.

%%% ----------------------------------------------------------
%%% @doc Returns all subelements of the given element that match
%%% the given element name.
%%% @end
%%% ----------------------------------------------------------

-spec find_elements(atom(), #xmlElement{}) -> [#xmlElement{}].

find_elements(ElementName, Element) ->
    [E
     ||#xmlElement{name=Name}=E <- Element#xmlElement.content,
       Name =:= ElementName].

%%% ----------------------------------------------------------
%%% #           find_attribute(AttributeName, Element)
%%% #           find_attribute(AttributeName, AttributeList)
%%% Input: AttributeName:atom()
%%%        Element:#xmlElement{} or
%%%        AttributeList:[#xmlattribute{}] a list of xml attributes
%%% Output: Value:string()
%%% Exceptions:
%%% Description: Finds an attribute to an xml element, or in a list of
%%%              attributes and returns the value of the attribute
%%% ----------------------------------------------------------

-spec find_attribute(atom(), #xmlElement{} | [#xmlAttribute{}]) ->
	  string().

find_attribute(AttributeName, Element) when is_record(Element, xmlElement) ->
    find_attribute(AttributeName, Element#xmlElement.attributes);
find_attribute(AttributeName, AttributeList) ->
    case lists:keysearch(AttributeName, #xmlAttribute.name, AttributeList) of
	{value, Attribute} ->
	    Attribute#xmlAttribute.value;
	false ->
	    erlang:error({badmatch, false}, [AttributeName, AttributeList])
    end.


%%% ----------------------------------------------------------

find_text(Element) when is_record(Element, xmlElement) ->
    [Text] = Element#xmlElement.content,
    Text#xmlText.value.

%%% ----------------------------------------------------------
%%% #           find_first_element(XmlElement)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% This function is used to find the first content that is an xml element
%%% when the name of that element is unknown
%%% ----------------------------------------------------------

find_first_element(Element) when is_record(Element, xmlElement) ->
    find_first_element(Element#xmlElement.content);

find_first_element([Element|_]) when is_record(Element, xmlElement) ->
    Element;
find_first_element([_|Elements]) ->
    find_first_element(Elements);
find_first_element([]) -> erlang:error(no_element, []).


info_msg(_,_) ->
    ok.
%% info_msg(Format, Args) ->
%%     sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
