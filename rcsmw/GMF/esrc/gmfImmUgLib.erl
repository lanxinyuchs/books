%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfImmUgLib.erl %
%%% Author:	erarafo
%%% Description: Functions used in Upgrade. These functions are used by:
%%%
%%%   gmfImmUgVerifyUpgrade
%%%   gmfImmUgMaster
%%%   gmfImmUgInserter
%%%
%%% Modules used: None
%%%
%%% ----------------------------------------------------------
-module(gmfImmUgLib).
%%-behaviour(behaviour).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/1').
-date('2016-10-13').
-author('erarafo').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2016 All rights reserved.
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
%%% R2A/1      2013-06-16 erarafo     Created
%%% R2A/2      2013-06-17 erarafo     Added mimDnToImmParentName/1
%%% R2A/4      2013-06-19 erarafo     Refactoring
%%% R2A/5      2013-06-20 erarafo     Handling "time" attribute type correctly
%%% R2A/6      2013-06-27 erarafo     #instanceGroup{} accessor, size function
%%% R2A/7      2013-06-28 erarafo     Refactoring and cleanup
%%% R2A/8      2013-09-04 erarafo     Added version reporting
%%% R2A/9      2013-09-05 erarafo     non-matching case clause corrected
%%% R2A/10     2013-09-16 erarafo     support for pre-restart schema check
%%% R2A/11     2013-09-17 erarafo     corrected derivation of abspath in schema
%%% R2A/12     2013-09-18 erarafo     refactoring
%%% R2A/14     2013-09-19 erarafo     eliminating the #schema{} record
%%% R2A/16     2013-10-21 erarafo     Logging and error invocation functions
%%% R2A/17     2013-10-22 erarafo     Fighting code duplication
%%% R2A/18     2013-10-24 erarafo     3-level schema versioning support
%%% R2A/19     2013-10-24 erarafo     3-level schema versioning support, bugfix
%%% R2A/20     2013-10-24 erarafo     Bugfix fix
%%% R2A/21     2013-10-28 erarafo     getImmClass/1 moved here
%%% R2A/22     2013-11-12 erarafo     adapted to changes in SAFs
%%% R2A/23     2013-11-14 erarafo     Refactoring
%%% R2A/24     2014-01-12 erarafo     defer failure
%%% R2A/26     2014-02-07 erarafo     downgrade stopped, diagnostic
%%% R2A/27     2014-03-06 erarafo     restartCold/4 function added
%%% R2A/28     2014-03-24 erarafo     Elaborated diagnostics
%%% R2A/29     2014-04-15 erarafo     Message cosmetics
%%% R2A/30     2014-05-05 erarafo     Revised error handling
%%% R2A/31     2014-05-21 erarafo     HS62261: Severity WARNING when user fault
%%% R2A/32     2014-06-16 erarafo     Support for dumping 'From' data
%%% R2A/36     2014-07-01 erarafo     Extended handleFault/5
%%% R2A/37     2014-07-01 erarafo     Improved fault logging
%%% R2A/38     2014-07-31 etxberb     Corrected cause value for AVLI
%%% R2A/39     2014-08-14 erarafo     Performance lift related to HS84194
%%% R3A/1      2014-10-20 erarafo     Added missing file:close() call
%%% R3A/2      2014-11-06 erarafo     Persistent runtime attributes
%%% R3A/3      2014-11-07 erarafo     Improved logging
%%% R3A/4      2014-12-09 erarafo     Minor footprint reduction
%%% R3A/5      2015-02-11 erarafo     Prepare for change of SAF IMM repr
%%% R3A/6      2015-02-12 erarafo     Minor adjustment
%%% R3A/7      2015-03-04 erarafo     Types adjusted
%%% R3A/8      2015-03-30 erarafo     Improved functions for output formatting
%%% R3A/9      2015-04-01 erarafo     Trivial change
%%% R4A/1      2015-02-12 erarafo     Comments only
%%% R4A/2      2015-04-20 etxpeno     Merge (R3A/7-9)
%%% R4A/3      2015-04-29 erarafo     EDoc update, cleanup
%%% R4A/4      2015-05-19 erarafo     New insertion algorithm
%%% R4A/5      2015-05-21 erarafo     Refactoring
%%% R4A/6      2015-05-25 erarafo     Benchmarking support
%%% R4A/7      2015-05-28 erarafo     Uniform "exception handling"
%%% R4A/8      2015-06-01 erarafo     More compact dump format
%%% R4A/9      2015-06-01 erarafo     Adjusted exception text
%%% R4A/10     2015-06-02 erarafo     Exception handling
%%% R4A/11     2015-06-04 erarafo     SAF IMM error string
%%% R4A/12     2015-06-09 erarafo     Adjusted to simplified record usage
%%% R4A/13     2015-06-10 erarafo     Redundant records removed
%%% R4A/14     2015-06-26 erarafo     Unused code removed
%%% R4A/15     2015-07-04 erarafo     Redundant record types eliminated
%%% R4A/16     2015-08-27 erarafo     More informative exception text
%%% R4A/17     2015-09-09 etxarnu     Do a 'te log read' before restart
%%% R4A/19     2015-09-09 erarafo     Using sysInitI for logging
%%% R6A/1      2016-05-11 erarafo     Unused functions removed
%%% R6A/2      2016-05-16 erarafo     Exceptions added
%%% R6A/4      2016-06-13 erarafo     Added progress/3 for LTTng trace
%%% R6A/5      2016-09-14 erarafo     Using error_logger:error_msg/2
%%% R7A/1      2016-10-13 erarafo     Added exceptions


-export([mimDnToImmParentName/1,
	 safsImmAttrValues/5,
	 encodeAttr/3,
	 getVersion/1,
	 getVersion/0,
	 getSchemasByName/1,
	 scanOldSchemas/3,
	 scanNewSchemas/3,
	 checkUpgradePaths/1,
	 checkUpgradePaths/2,
	 listToDict/2,
	 getCurrentSchemasByBasename/1,
	 getImmClass/1,
	 format/2,
	 binariesToStrings/3,
	 millisSince/1,
	 handleFault/6,
	 progress/3,
	 log/5,
	 exception/1]).
-export([make_te_log/0]). %for test

-include("gmfMeta.hrl").
-include("gmf.hrl").
-include("gmfImmUg.hrl").
-include_lib("xmerl/include/xmerl.hrl").


%%% ----------------------------------------------------------
%%% @doc Derive IMM parent name from MIM-style DN. It is trusted
%%% that the given DN is of length greater than zero. RDN names
%%% are not converted.
%%%
%%% In the special case of a root instance the parent name shall
%%% be returned as the atom 'undefined'.
%%%
%%% Example:
%%% ```
%%%     [<<a=1>>, <<b=1>>, <<c=1>>] -> <<b=1,a=1>>
%%% '''
%%% @end
%%% ----------------------------------------------------------

-spec mimDnToImmParentName([rdn()]) ->  parentName().

mimDnToImmParentName([_]) ->
    undefined;

mimDnToImmParentName(MimDn) ->
    L = length(MimDn),
    list_to_binary(
      lists:foldl(
	fun(Rdn, {K, _Acc}) when K =:= 1 ->
		{K+1, binary_to_list(Rdn)};
	   (Rdn, {K, Acc}) when K < L ->
		{K+1, binary_to_list(Rdn)++","++Acc};
	   (_, {_, Acc}) ->
		Acc
	end,
	{1, ignore},
	MimDn)).


-spec safsImmAttrValues(atom(), [any()], atom(), [#imm_attr{}], [{atom(), binary()}]) ->
	  #safsImmAttrValues_2{}.

safsImmAttrValues(AttrNameA, 
		  Values, 
		  ClassNameA, 
		  AttrDecls,
		  AttrNames) ->
    % This lookup will always succeed because of the
    % test made in gmfImmUgInserter:getImmAttrCat/3.
    {_, AttrNameB} = lists:keyfind(AttrNameA, 1, AttrNames), 
    
    TypeA = getType(AttrNameA, ClassNameA, AttrDecls),
    createImmAttrValues(TypeA, AttrNameB, Values).


%%% ----------------------------------------------------------
%%% @doc Encode one attribute. This function is invoked
%%% via safcImmCtReadInstances() and uses info about the
%%% From-system classes.
%%%
%%% Throws: imm_attr_unknown, bad_attr_type
%%% @end
%%% ----------------------------------------------------------
-spec encodeAttr(atom(), {atom(), [tuple()]}, #oldClassInfo{}) -> #safsImmAttrValues_2{}.

encodeAttr(ClassNameA, {ANameA, Values}, #oldClassInfo{attributes=AttrDescrs}) ->
    ANameS = atom_to_list(ANameA),
    TypeA =
	case lists:keyfind(ANameS, 1, AttrDescrs) of
	    false ->
		throw(#exception{key=imm_attr_unknown, 
				 data={oldClassInfo, ClassNameA, ANameA, AttrDescrs}});
	    {_, Props} ->
		case proplists:lookup('type', Props) of
		    none ->
			throw(#exception{key=bad_attr_type, 
					 data={oldClassInfo, ClassNameA, ANameA, Props}});
		    {_, TypeS} ->
			getType(TypeS)
		end
	end,
    AttrNameB = list_to_binary(atom_to_list(ANameA)),
    createImmAttrValues(TypeA, AttrNameB, Values).


-spec createImmAttrValues(immAttrType(), binary(), [any()]) ->
	  #safsImmAttrValues_2{}.

createImmAttrValues(sa_imm_attr_saint32t, AttrNameB, Values) ->
    #safsImmAttrValues_2{attrName=AttrNameB,
		   attrValueType=sa_imm_attr_saint32t,
		   attrValuesNumber=length(Values),
		   attrValues=[#safsImmAttrValue{saint32=V} || V <- Values]};

createImmAttrValues(sa_imm_attr_sauint32t, AttrNameB, Values) ->
    #safsImmAttrValues_2{attrName=AttrNameB,
		   attrValueType=sa_imm_attr_sauint32t,
		   attrValuesNumber=length(Values),
		   attrValues=[#safsImmAttrValue{sauint32=V} || V <- Values]};

createImmAttrValues(sa_imm_attr_sastringt, AttrNameB, Values) ->
    #safsImmAttrValues_2{attrName=AttrNameB,
		   attrValueType=sa_imm_attr_sastringt,
		   attrValuesNumber=length(Values),
		   attrValues=[#safsImmAttrValue{sastring=V} || V <- Values]};

createImmAttrValues(sa_imm_attr_saint64t, AttrNameB, Values) ->
    #safsImmAttrValues_2{attrName=AttrNameB,
		   attrValueType=sa_imm_attr_saint64t,
		   attrValuesNumber=length(Values),
		   attrValues=[#safsImmAttrValue{saint64=V} || V <- Values]};

createImmAttrValues(sa_imm_attr_sauint64t, AttrNameB, Values) ->
    #safsImmAttrValues_2{attrName=AttrNameB,
		   attrValueType=sa_imm_attr_sauint64t,
		   attrValuesNumber=length(Values),
		   attrValues=[#safsImmAttrValue{sauint64=V} || V <- Values]};

createImmAttrValues(sa_imm_attr_sanamet, AttrNameB, Values) ->
    #safsImmAttrValues_2{attrName=AttrNameB,
		   attrValueType=sa_imm_attr_sanamet,
		   attrValuesNumber=length(Values),
		   attrValues=[#safsImmAttrValue{saname=V} || V <- Values]};

createImmAttrValues(sa_imm_attr_satimet, AttrNameB, Values) ->
    #safsImmAttrValues_2{attrName=AttrNameB,
		   attrValueType=sa_imm_attr_satimet,
		   attrValuesNumber=length(Values),
		   attrValues=[#safsImmAttrValue{satime=V} || V <- Values]};

createImmAttrValues(sa_imm_attr_sadoublet, AttrNameB, Values) ->
    #safsImmAttrValues_2{attrName=AttrNameB,
		   attrValueType=sa_imm_attr_sadoublet,
		   attrValuesNumber=length(Values),
		   attrValues=[#safsImmAttrValue{sadouble=V} || V <- Values]};

createImmAttrValues(sa_imm_attr_safloatt, AttrNameB, Values) ->
    #safsImmAttrValues_2{attrName=AttrNameB,
		   attrValueType=sa_imm_attr_safloatt,
		   attrValuesNumber=length(Values),
		   attrValues=[#safsImmAttrValue{safloat=V} || V <- Values]};

createImmAttrValues(sa_imm_attr_saanyt, AttrNameB, Values) ->
    #safsImmAttrValues_2{attrName=AttrNameB,
		   attrValueType=sa_imm_attr_saanyt,
		   attrValuesNumber=length(Values),
		   attrValues=[#safsImmAttrValue{saany=V} || V <- Values]}.


%%% ----------------------------------------------------------
%%% @doc Gets the type of an attribute from the list of
%%% attribute descriptions of the class where it belongs.
%%%
%%% Throws: imm_attr_unknown
%%% @end
%%% ----------------------------------------------------------
-spec getType(atom(), atom(), [#imm_attr{}]) -> immAttrType().

getType(AttrNameA, ClassNameA, ImmAttrSpecs) ->
    try
	lists:foreach(
	  fun(#imm_attr{name=NameA, type=Type}) when NameA =:= AttrNameA ->
		  throw(Type);
	     (_) ->
		  ok
	  end,
	  ImmAttrSpecs)
    of
	_ ->
	    throw(#exception{key=imm_attr_unknown, 
			     data={immAttrSpecs, ClassNameA, AttrNameA, ImmAttrSpecs}})
    catch
	throw:Result -> 
	    Result
    end.


%%% ----------------------------------------------------------
%%% @doc Converts a type indicator from string to
%%% IMM representation.
%%% @end
%%% ----------------------------------------------------------
-spec getType(string()) -> immAttrType().

getType("SA_INT32_T") ->
    sa_imm_attr_saint32t;
getType("SA_INT64_T") ->
    sa_imm_attr_saint64t;
getType("SA_UINT32_T") ->
    sa_imm_attr_sauint32t;
getType("SA_UINT64_T") ->
    sa_imm_attr_sauint64t;
getType("SA_TIME_T") ->
    sa_imm_attr_satimet;
getType("SA_STRING_T") ->
    sa_imm_attr_sastringt;
getType("SA_NAME_T") ->
    sa_imm_attr_sanamet;
getType("SA_DOUBLE_T") ->
    sa_imm_attr_sadoublet;
getType("SA_FLOAT_T") ->
    sa_imm_attr_safloatt;
getType("SA_ANY_T") ->
    sa_imm_attr_saanyt;
getType(Other) ->
    throw(#exception{key=bad_attr_type, data={Other}}).


-spec getVersion(module()) -> atom() | integer().

%%% ----------------------------------------------------------
%%% @doc Returns the version of the given module. The
%%% expected result, if the -vsn() directive is present,
%%% is an atom.
%%% @end
%%% ----------------------------------------------------------

getVersion(Module) ->
    case code:which(Module) of
	Path when is_list(Path) ->
	    case beam_lib:version(Path) of
		{error, _, _} ->
		    'unknown';
		{ok, {_, [Version]}} ->
		    Version
	    end;
	_ ->
	    'unknown'
    end.


%%% ----------------------------------------------------------
%%% @doc Returns the version of this module.
%%% @end
%%% ----------------------------------------------------------
-spec getVersion() -> atom() | integer().

getVersion() ->
    getVersion(?MODULE).








%%% ----------------------------------------------------------
%%% @doc Recurse over the list of old schema basenames and
%%% return a 6-tuple of classification information.
%%%
%%% The OldByAbsPath result element is a dict of categorized
%%% old schemas by absolute pathname. The category is set to
%%% s0 | s1v0| s1v1 | s1v2 | s1v3.
%%%
%%% The S0 result element is a list of old schemas, categorized
%%% as s0.
%%%
%%% The S1V0, S1V1, S1V2 and S1V3 elements are new schemas of
%%% those categories. The 'cat' field is not set.
%%% @end
%%% ----------------------------------------------------------
-spec scanOldSchemas(dict:dict(), dict:dict(), dict:dict()) ->
	  {dict:dict(), [#immInfo{}],[#immInfo{}],[#immInfo{}],[#immInfo{}],[#immInfo{}]}.

scanOldSchemas(OldSchemasByBasename, NewSchemasByBasename, NewSchemasByName) ->
    scanOldSchemas(OldSchemasByBasename, NewSchemasByBasename, NewSchemasByName,
		   dict:fetch_keys(OldSchemasByBasename),
		   [], [], [], [], [], []).


-spec scanOldSchemas(dict:dict(), dict:dict(), dict:dict(),
		     [string()],
		     [{string(), #immInfo{}}],
		     [#immInfo{}],[#immInfo{}],[#immInfo{}],[#immInfo{}],[#immInfo{}]) ->
	  {dict:dict(), [#immInfo{}],[#immInfo{}],[#immInfo{}],[#immInfo{}],[#immInfo{}]}.

scanOldSchemas(_, _, _,
	       [],
	       ByA,
	       S0, S1V0, S1V1, S1V2, S1V3) ->
    OldByAbsPath = dict:from_list(ByA),
    DictSize = dict:size(OldByAbsPath),
    if
	DictSize < length(ByA) ->
	    %% This is really impossible since we know that basenames
	    %% are unique, then surely abspaths must be unique too?
	    AbsPaths = [AbsPath || {AbsPath, _} <- ByA],
	    ?FAULT([restart],
		   "absolute paths to IMM classes files not unique; paths are: ~p",
		   [AbsPaths]);
	true ->
	    ok
    end,
    {OldByAbsPath, S0, S1V0, S1V1, S1V2, S1V3};

scanOldSchemas(OldSchemasByBasename, NewSchemasByBasename, NewSchemasByName,
	       [B|Tail],
	       ByA,
	       S0, S1V0, S1V1, S1V2, S1V3) ->
    #immInfo{type=Type,
	     schemaName=Name,
	     version=OldVersion,
	     abspath=A} = S = dict:fetch(B, OldSchemasByBasename),
    case Type of
	unnamedSchema ->
	    %% old is unnamed
	    case dict:is_key(B, NewSchemasByBasename) of
		false ->
		    SCat = S#immInfo{cat=s0},
		    scanOldSchemas(OldSchemasByBasename,
				   NewSchemasByBasename,
				   NewSchemasByName,
				   Tail,
				   [{A, SCat}|ByA],
				   S0++[SCat],
				   S1V0,
				   S1V1,
				   S1V2,
				   S1V3);
		true ->
		    case dict:fetch(B, NewSchemasByBasename) of
			#immInfo{type=unnamedSchema}=T ->
			    %% new is unnamed too
			    SCat = S#immInfo{cat=s1v0},
			    scanOldSchemas(OldSchemasByBasename,
					   NewSchemasByBasename,
					   NewSchemasByName,
					   Tail,
					   [{A, SCat}|ByA],
					   S0,
					   S1V0++[T],
					   S1V1,
					   S1V2,
					   S1V3);
			#immInfo{schemaName=TName}=T ->
			    %% new is named
			    case
				lists:member(T, S1V2) orelse
				lists:member(T, S1V3)
				of
				true ->
				    ?FAULT([restart],
					   "new schema matched by multiple old schemas: ~s",
					   [TName]);
			    	_ ->
				    SCat = S#immInfo{cat=s1v1},
				    scanOldSchemas(OldSchemasByBasename,
						   NewSchemasByBasename,
						   NewSchemasByName,
						   Tail,
						   [{A, SCat}|ByA],
						   S0,
						   S1V0,
						   S1V1++[T],
						   S1V2,
						   S1V3)
			    end
		    end
	    end;
	_ ->
	    %% old is named
	    case dict:is_key(Name, NewSchemasByName) of
		false ->
		    %% no matching name among the new
		    SCat = S#immInfo{cat=s0},
		    scanOldSchemas(OldSchemasByBasename,
				   NewSchemasByBasename,
				   NewSchemasByName,
				   Tail,
				   [{A, SCat}|ByA],
				   S0++[S#immInfo{cat=s0}],
				   S1V0,
				   S1V1,
				   S1V2,
				   S1V3);
		true ->
		    %% new is named too
		    #immInfo{version=NewVersion,
			     schemaName=TName}=T = dict:fetch(Name,
							      NewSchemasByName),
		    case lists:member(T, S1V1) of
			true ->
			    ?FAULT([restart],
				   "new schema matched by multiple old schemas: ~s",
				   [TName]);
			_ ->
			    if
				NewVersion == OldVersion ->
				    SCat = S#immInfo{cat=s1v3},
				    scanOldSchemas(OldSchemasByBasename,
						   NewSchemasByBasename,
						   NewSchemasByName,
						   Tail,
						   [{A, SCat}|ByA],
						   S0,
						   S1V0,
						   S1V1,
						   S1V2,
						   S1V3++[T]);
				true ->
				    SCat = S#immInfo{cat=s1v2},
				    scanOldSchemas(OldSchemasByBasename,
						   NewSchemasByBasename,
						   NewSchemasByName,
						   Tail,
						   [{A, SCat}|ByA],
						   S0,
						   S1V0,
						   S1V1,
						   S1V2++[T],
						   S1V3)
			    end
		    end
	    end
    end.


%%% ----------------------------------------------------------
%%% @doc Returns a dictionary of named schemas, formed from
%%% the given dictionary of schemas.
%%% @end
%%% ----------------------------------------------------------
-spec getSchemasByName(dict:dict()) -> dict:dict().

getSchemasByName(Schemas) ->
    dict:from_list(
      [{Name, S}
      || {_, #immInfo{type=namedSchema, schemaName=Name}=S} <- dict:to_list(Schemas)]).


%%% ----------------------------------------------------------
%%% @doc Recurse over all new schemas by basename in order to find the
%%% S2 schemas.
%%%
%%% NewSchemasMatched are new schemas that were basename-matched by an unnamed
%%% old schema, or schema-name matched by an old schema.
%%% @end
%%% ----------------------------------------------------------
-spec scanNewSchemas(dict:dict(), dict:dict(), [#immInfo{}]) -> [#immInfo{}].

scanNewSchemas(OldSchemasByBasename, NewSchemasByBasename, NewSchemasMatched) ->
    scanNewSchemas(OldSchemasByBasename, NewSchemasByBasename, NewSchemasMatched,
		   dict:fetch_keys(NewSchemasByBasename),
		   []).

scanNewSchemas(_, _, _, [], S2) ->
    S2;

scanNewSchemas(OldSchemasByBasename, NewSchemasByBasename, NewSchemasMatched,
	       [B|Tail],
	       S2) ->
    #immInfo{type=Type}=T = dict:fetch(B, NewSchemasByBasename),
    case lists:member(T, NewSchemasMatched) of
	false ->
	    %% new is not any of the S1 cases
	    case Type of
		unnamedSchema ->
		    %% new is unnamed
		    case dict:is_key(B, OldSchemasByBasename) of
			true ->
			    %% there is an old with same basename; the old schema
			    %% cannot be unnamed as it would then have been an S1V0
			    %% match
			    #immInfo{schemaName=OldName} =
				dict:fetch(B, OldSchemasByBasename),
			    %% consider this an error
			    ?FAULT([restart],
				   "file: ~s, old schemaName: ~s, new schemaName missing",
				   [B, OldName]);
			false ->
			    %% no old found; this schema is truly new unnamed
			    scanNewSchemas(OldSchemasByBasename,
					   NewSchemasByBasename,
					   NewSchemasMatched,
					   Tail,
					   S2++[T])
		    end;
		_ ->
		    %% new is named and not S1; this schema must be new
		    scanNewSchemas(OldSchemasByBasename,
				   NewSchemasByBasename,
				   NewSchemasMatched,
				   Tail,
				   S2++[T])
	    end;
	true ->
	    %% new schema is S1, ignore
	    scanNewSchemas(OldSchemasByBasename,
			   NewSchemasByBasename,
			   NewSchemasMatched,
			   Tail,
			   S2)
    end.


%%% ----------------------------------------------------------
%%% @doc Check upgrade paths of schemas where only the
%%% To-version is named. A list of version failure
%%% records is returned.
%%% @end
%%% ----------------------------------------------------------
-spec checkUpgradePaths([#immInfo{}]) -> [#versionFailure{}].

checkUpgradePaths(ToSchemas) ->
    ImpliedOldVersion = [0],
    lists:append(
      [case checkUpgradePath(Name, ImpliedOldVersion, FromVersions, ToVersion)
	   of
	   true ->
	       [];
	   _ ->
	       [#versionFailure{schemaName=Name,
				fromVersions=FromVersions,
				oldVersion=ImpliedOldVersion,
				oldMode="implied",
				toVersion=ToVersion}]
       end
       || #immInfo{schemaName=Name,
		   fromVersions=FromVersions,
		   version=ToVersion} <- ToSchemas]).



%%% ----------------------------------------------------------
%%% @doc Check upgrade paths of schemas where both From-
%%% and To-versions are named. A list of version failure
%%% records is returned.
%%% @end
%%% ----------------------------------------------------------
-spec checkUpgradePaths([#immInfo{}], dict:dict()) -> [#versionFailure{}].

checkUpgradePaths(ToSchemas, OldSchemasByName) ->
    lists:append(
      [begin
	   #immInfo{version=OldVersion} = dict:fetch(Name, OldSchemasByName),
 	   ?INFO("~nchecking schema: ~s"
		 "~n  actual old version: ~p"
                 "~n  supported from-versions: ~p"
	         "~n",
		 [Name, OldVersion, FromVersions]),
	   case checkUpgradePath(Name, OldVersion, FromVersions, ToVersion) of
	       true ->
		   [];
	       _ ->
		   [#versionFailure{schemaName=Name,
				    fromVersions=FromVersions,
				    oldVersion=OldVersion,
				    toVersion=ToVersion}]
	   end
       end
       || #immInfo{schemaName=Name,
		   fromVersions=FromVersions,
		   version=ToVersion} <- ToSchemas]).


%%% ----------------------------------------------------------
%%% @doc Check that the given old-version (2nd argument) has a
%%% major number that matches a major number among the given
%%% from-versions (3rd argument), and that the old-version is
%%% greater or equal to that from-version. Recurse over the 3rd
%%% argument.
%%%
%%% Returns true if upgrade is supported, false otherwise.
%%%
%%% A check is included that the To-version is greater than or
%%% equal to the actual old version.
%%%
%%% Meaningless duplicated fromVersion elements are not detected.
%%% The search proceeds until a valid fromVersion is found, or
%%% there are no more entries to check.
%%% @end
%%% ----------------------------------------------------------
-spec checkUpgradePath(string(), [integer()], [[integer()]], [integer()]) ->
	  boolean().

checkUpgradePath(_Name, _OldVersion, [], _ToVersion) ->
    false;

checkUpgradePath(Name, [XM|_]=OldVersion, [[XM|_]=FromVersion|Tail], ToVersion) ->
    case versionGrEq(OldVersion, FromVersion) of
	true ->
	    % Probably a valid fromVersion; just check
            % that we don't have a downgrade situation.
	    case versionGrEq(ToVersion, OldVersion) of
		true ->
		    true;
		false ->
		    ?WARNING("downgrade not allowed, schema: ~s~n"
		             "from-version: ~p, to-version: ~p",
			     [Name, OldVersion, ToVersion]),
		    checkUpgradePath(Name, OldVersion, Tail, ToVersion)
	    end;
	false ->
	    checkUpgradePath(Name, OldVersion, Tail, ToVersion)
    end;

checkUpgradePath(Name, OldVersion, [_|Tail], ToVersion) ->
    checkUpgradePath(Name, OldVersion, Tail, ToVersion).


%%% ----------------------------------------------------------
%%% @doc Returns true if L >= M. L and M are lists of non-negative
%%% integers like [5, 2, 0] denoting version 5.2.0. The second
%%% list may be abbreviated; [5] implies [5, 0, 0] for 5.0.0, and
%%% [5, 2] implies [5, 2, 0] for 5.2.0. It is trusted that the first
%%% argument is of length 3. It is trusted that the second argument
%%% is of length 3, 2 or 1. It is trusted that the list elements are
%%% non-negative integers.
%%% @end
%%% ----------------------------------------------------------
-spec versionGrEq([integer()], [integer()]) -> boolean().

versionGrEq([X|_], [Y|_]) when X > Y ->
    true;

versionGrEq([X|_], [Y|_]) when X < Y ->
    false;

versionGrEq([_|XTail], [_|YTail]) ->
    versionGrEq(XTail, YTail);

versionGrEq(_, []) ->
    true.


%%% ----------------------------------------------------------
%%% @doc Returns a dictionary of schemas with basename as
%%% key. A schema is an #immInfo{type=unnamedSchema} or
%%% #immInfo{type=namedSchema}. It is guaranteed that
%%% occurring schema names are unique, and that basenames
%%% are unique.
%%% @end
%%% ----------------------------------------------------------
-spec listToDict([#gmfCxpRev{}], string()) -> dict:dict().

listToDict(GmfCxpRevRecs, Context) ->
    ImmInfosOld =
	lists:append(
	  [II || #gmfCxpRev{imm_info=II} <- GmfCxpRevRecs]),

    %% Normalize the version field to be a 3-list
    ImmInfos =
	[case V of
	     [X, Y] ->
		 S#immInfo{version=[X, Y, 0]};
	     [_, _, _] ->
		 S;
	     _ ->
		 ?FAULT([restart],
			"ill-formed version info in: ~p",
			[S])
	 end
	 || #immInfo{type=Type, version=V}=S <- ImmInfosOld,
	    Type =:= namedSchema orelse Type =:= unnamedSchema],

    KeyValuesWithSchemaInfo =
	[{Basename, S}
	|| #immInfo{type=namedSchema, file=Basename}=S <- ImmInfos],

    KeyValuesWoSchemaInfo =
	[{Basename, S}
	|| #immInfo{type=unnamedSchema, file=Basename}=S <- ImmInfos],


    case duplicateFree(KeyValuesWithSchemaInfo,
		       fun({_, #immInfo{schemaName=S}}) -> S end,
		       [])
	of
	{duplicate, S} ->
	    ?FAULT([restart],
		   "duplicate schema name among IMM classes files: ~s, context: ~s",
		   [S, Context]);
	_ ->
	    ok
    end,

    KeyValues = KeyValuesWithSchemaInfo ++ KeyValuesWoSchemaInfo,

    case duplicateFree(KeyValues, fun({Basename, _}) -> Basename end, []) of
	{duplicate, Basename} ->
	    ?FAULT([restart],
		   "duplicate filename among IMM classes files: ~s, context: ~s",
		   [Basename, Context]);
	_ ->
	    ok
    end,

    dict:from_list(KeyValues).


%%% ----------------------------------------------------------
%%% @doc Returns ok if there are no duplicates in the
%%% list obtained by mapping the given list by the given
%%% 1-ary function.
%%% @end
%%% ----------------------------------------------------------
-spec duplicateFree([term()], fun(), [term()]) -> ok | {duplicate, term()}.

duplicateFree([], _, _) ->
    ok;

duplicateFree([Object|Tail], Accessor, Seen) ->
    Component = apply(Accessor, [Object]),
    case lists:member(Component, Seen) of
	true ->
	    {duplicate, Component};
	false ->
	    duplicateFree(Tail, Accessor, [Component|Seen])
    end.


%%% ----------------------------------------------------------
%%% @doc Returns the current schemas in a dictionary, using the
%%% basename string as key (it is trusted to exist always).
%%% The schema names within the directory are guaranteed unique.
%%% @end
%%% ----------------------------------------------------------
-spec getCurrentSchemasByBasename(string()) ->  dict:dict().

getCurrentSchemasByBasename(Context) ->
    F2 = fun(Rec, Acc) -> [Rec|Acc] end,
    {atomic, Result} =
	mnesia:transaction(fun() -> mnesia:foldl(F2, [], gmfCxpRev) end),
    gmfImmUgLib:listToDict(Result, Context).


%%% ----------------------------------------------------------
%%% @doc Looks up the To-system #imm_class{} of the given class.
%%% @end
%%% ----------------------------------------------------------
-spec getImmClass(atom()) ->  #imm_class{}.

getImmClass(Class) ->
    MnesiaRecords = mnesia:dirty_read(imm_class, Class),
    case MnesiaRecords of
	[] ->
	    throw(#exception{key=imm_class_unknown, data={Class}});
	[ImmClass] ->
	    ImmClass;
	Other ->
	    throw(#exception{key=imm_class_ambiguous, data={Class, Other}})
    end.


%%% ----------------------------------------------------------
%%% @doc Produces a flattened string from the given format and
%%% data.
%%% @end
%%% ----------------------------------------------------------
-spec format(string(), [any()]) -> string().

format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).


%%% ----------------------------------------------------------
%%% @doc Handle a fault. If 'error' is among the options a simple
%%% erlang:error/1 call is made and none of the actions below
%%% apply.
%%%
%%% Otherwise a message and an optional stack trace is written to
%%% the SwmInternal log, and an ERROR REPORT is written to the
%%% Erlang log. The 'user' option lowers the severity to WARNING.
%%%
%%% The 'source' option, can be one of 'auto', 'copy', 'read' or
%%% 'write'.
%%%
%%% A restart may be requested. A cause may be specified; it
%%% defaults to ?AVLI_Cause_UpgradeFailure.
%%%
%%% Use the FAULT macro for invocations of this function.
%%% @end
%%% ----------------------------------------------------------

-type handleFaultOption() ::
	  error |
	  restart |
	  stack |
	  user |
	  {cause, string()} |
	  {source, auto} |
	  {source, copy} |
	  {source, read} |
	  {source, write}.

-spec handleFault(
	[handleFaultOption()],
	atom(),
	non_neg_integer(),
	string(),
	[any()],
	[tuple()]) ->
	  ok.

handleFault(Options, Module, Line, Format, Data, StackTrace) ->
    case proplists:get_bool(error, Options) of
	true ->
	    % the brutal 'error' option is used by gmfMetaLib
	    Message = format("~w:~w "++Format, [Module, Line|Data]),
	    erlang:error(Message);
	false ->
	    Source = proplists:get_value(source, Options),
	    Restart = proplists:get_bool(restart, Options),
	    if
		Restart ->
		    gmfImmUgMaster:setRestartRequested();
		true ->
		    ok
	    end,
	    Stack = proplists:get_bool(stack, Options),
	    User = proplists:get_bool(user, Options),
	    Ref = make_ref(),
	    Message =
		case Source of
		    undefined ->
			format("~w ~w:~w "++Format,
			       [Ref, Module, Line|Data]);
		    _ ->
			format("~w ~w:~w (~w) "++Format,
			       [Ref, Module, Line, Source|Data])
		end,
	    LogName = "SwmInternal",
	    LogUser = "UG",
	    logI:write_log(LogName,
			   LogUser,
			   if User -> warning; true -> error end,
			   Message),
	    if
		Stack ->
		    logI:write_log(LogName, LogUser, info,
				   format("~w stack:~n~p",
					  [Ref, StackTrace]));
		true ->
		    ok
	    end,
	    if
		User ->
		    sysInitI:warning_msg(Message++"~n");
		true ->
		    % not using sysInitI here since it may be really important
		    % to see the entire error message
		    error_logger:error_msg(Message++"~n")
	    end,
	    if
		Restart ->
		    AvliCause = get_cause(Options, Module, Line),
		    make_te_log(),
		    appmI:restart_node(cold, AvliCause);
		true ->
		    ok
	    end
    end.

make_te_log() ->
    TriLogDir = filename:join([sysEnv:rcs_dir(),"log","TriLog"]),
    file:make_dir(TriLogDir), %ensure it exists
    LogFile = filename:join([TriLogDir,"UgTriLog.1"]),
    Date = comsaI:iso_time(os:timestamp(), extended),
    Msg = "Generating new te dump at "++Date ++ " before rollback",
    cmd("echo " ++ Msg ++ " > " ++ LogFile),
    cmd("te log read >> " ++ LogFile).

cmd(Cmd) ->
    sysInitI:info_msg("gmfImmUgLib: ~s~n",[lists:flatten(Cmd)]),
    os:cmd(Cmd).


%%% ----------------------------------------------------------
%%% @doc Log a message to the Erlang log and the LTTng log.
%%% @end
%%% ----------------------------------------------------------
-spec progress(module(), integer(), string()) -> any().

progress(Module, Line, Message) ->
    log(Module, Line, info, "~s", [Message]),
    sysInitI:restart_logger_trace(Module, Line, Message).



%%% ----------------------------------------------------------
%%% @doc Log an error with the specified severity level in the
%%% SwmInternal log. Valid severity levels are those specified
%%% in logI.
%%% @end
%%% ----------------------------------------------------------
-spec log(module(), pos_integer(), atom(), string(), [any()]) -> any().

log(Module, Line, Severity, Format, Data) ->
    logI:write_log(
      "SwmInternal",
      "UG",
      Severity,
      format("~w:~w "++Format, [Module, Line|Data])).


get_cause(Options, Module, Line) ->
    Cause = proplists:get_value(cause, Options, ?AVLI_Cause_UpgradeFailure),
    case is_string(Cause) of
	true ->
	    Cause;
	false ->
	    format("Ill-formed cause. MODULE: ~p, LINE: ~p", [Module, Line])
    end.


is_string(Term) ->
    io_lib:char_list(Term).


%%% ----------------------------------------------------------
%%% @doc Converts a list of binaries and a maximum string length
%%% to a list of strings with comma-separated substrings. The
%%% string length is bounded, except if very long substrings are
%%% present. The third argument is an accumulator.
%%% @end
%%% ----------------------------------------------------------
-spec binariesToStrings([binary()], non_neg_integer(), [string()]) ->
	  [string()].

binariesToStrings([], _MaxLength, Acc) ->
    lists:reverse(Acc);

binariesToStrings(Binaries, MaxLength, Acc) ->
    {S, Residual} = binariesToString(Binaries, MaxLength),
    binariesToStrings(Residual, MaxLength, [S|Acc]).


%%% ----------------------------------------------------------
%%% @doc Converts a list of binaries and a maximum string length
%%% to a tuple containing a maximal string and a residual list.
%%%
%%% The string is built from the leftmost binaries with ", " as
%%% separator. At least one of the binaries (if present) is
%%% consumed, regardless of the length limit.
%%% @end
%%% ----------------------------------------------------------
-spec binariesToString([binary()], non_neg_integer()) -> {string(), [binary()]}.

binariesToString([], _) ->
    {[], ""};

binariesToString([HeadBinary|Tail], MaxLength) ->
    HeadFragment = binary_to_list(HeadBinary),
    Separator = ", ",
    try
	{Fragments, _, Residue} =
	    lists:foldl(
	      fun(B, {FF, L, R}) ->
		      S = binary_to_list(B),
		      NewLength = L + length(Separator) + length(S),
		      if
			  NewLength > MaxLength ->
			      throw({FF, R});
			  true ->
			      {FF ++ [Separator, S], NewLength, tl(R)}
		      end
	      end,
	      {[HeadFragment], length(HeadFragment), Tail},
	      Tail),
	{lists:append(Fragments), Residue}
    catch
	throw:{FF, R} ->
	    {lists:append(FF), R}
    end.


%%% ----------------------------------------------------------
%%% @doc Returns the number of milliseconds that have elapsed
%%% since the given timestamp.
%%%
%%% Currently no callers. Possibly useful for performance
%%% studies.
%%% @end
%%% ----------------------------------------------------------
-spec millisSince(erlang:timestamp()) -> integer().

millisSince(Timestamp) ->
    Micros = timer:now_diff(os:timestamp(), Timestamp),
    Micros div 1000.


%%% ----------------------------------------------------------
%%% @doc Returns an exception with information filled in.
%%% @end
%%% ----------------------------------------------------------
-spec exception(#exception{}) -> #exception{}.

exception(#exception{key=class_not_pers_rt}=Ex) ->   Ex#exception{text= <<"class is not persistent runtime">>};

exception(#exception{key=cat_unknown}=Ex) ->         Ex#exception{text= <<"class category unknown">>};

exception(#exception{key=bad_parent}=Ex) ->          Ex#exception{text= <<"ill-formed parent name">>};

exception(#exception{key=bad_attr_values}=Ex) ->     Ex#exception{text= <<"bad attribute values">>};

exception(#exception{key=bad_attr_type}=Ex) ->       Ex#exception{text= <<"attribute type unknown">>};

exception(#exception{key=imm_attr_unknown}=Ex) ->    Ex#exception{text= <<"attribute unknown">>};

exception(#exception{key=dangling}=Ex) ->            Ex#exception{text= <<"unresolved MO references, check details in SwmInternal log">>};

exception(#exception{key=refs_incons}=Ex) ->         Ex#exception{text= <<"refs inconsistency">>};

exception(#exception{key=own_dn}=Ex) ->              Ex#exception{text= <<"own DN not found">>};

exception(#exception{key=imm_class_unknown}=Ex) ->   Ex#exception{text= <<"IMM class unknown">>};

exception(#exception{key=imm_class_ambiguous}=Ex) -> Ex#exception{text= <<"IMM class ambiguous">>};

exception(#exception{key=unknown_rdn}=Ex) ->         Ex#exception{text= <<"unknown RDN encountered">>};

exception(#exception{key=duplicate_rdn}=Ex) ->       Ex#exception{text= <<"duplicate RDN encountered">>};

exception(#exception{key=cannot_insert}=Ex) ->       Ex#exception{text= <<"could not insert some instances">>};

exception(#exception{key=buffer_incons}=Ex) ->       Ex#exception{text= <<"buffer inconsistent">>};

exception(#exception{key=inst_grp_handled}=Ex) ->    Ex#exception{text= <<"instance group already handled">>};

exception(#exception{key=bidir_incons}=Ex) ->        Ex#exception{text= <<"bidir inconsistency">>};

exception(#exception{key=safs_version}=Ex) ->        Ex#exception{text= <<"offered SAFS version too old">>};

exception(#exception{key=admin_owner_init}=Ex) ->    Ex#exception{text= <<"failed to initialize admin owner">>};

exception(#exception{key=ccb_id}=Ex) ->              Ex#exception{text= <<"failed to initialize CCB">>};

exception(#exception{key=ccb_insert}=Ex) ->          Ex#exception{text= <<"failed to insert in CCB">>};

exception(#exception{key=ccb_apply}=Ex) ->           Ex#exception{text= <<"failed to apply CCB">>};

exception(#exception{key=ccb_fin}=Ex) ->             Ex#exception{text= <<"failed to finalize CCB">>};

exception(#exception{key=admin_owner_fin}=Ex) ->     Ex#exception{text= <<"failed to finalize admin owner">>};

exception(#exception{key=imm_fin}=Ex) ->             Ex#exception{text= <<"failed to finalize IMM handle">>};

exception(#exception{key=admin_owner_set}=Ex) ->     Ex#exception{text= <<"failed to set admin owner">>};

exception(#exception{key=modify_ccb}=Ex) ->          Ex#exception{text= <<"failed to update reservedBy (ccb)">>};

exception(#exception{key=oi_init}=Ex) ->             Ex#exception{text= <<"failed to initialize OI session">>};

exception(#exception{key=oi_impl_set}=Ex) ->         Ex#exception{text= <<"failed to set object implementer">>};

exception(#exception{key=oi_impl_clear}=Ex) ->       Ex#exception{text= <<"failed to clear object implementer">>};

exception(#exception{key=oi_create}=Ex) ->           Ex#exception{text= <<"failed to create RT instance">>};

exception(#exception{key=oi_fin}=Ex) ->              Ex#exception{text= <<"failed to finalize OI session">>};

exception(#exception{key=no_struct_dn}=Ex) ->        Ex#exception{text= <<"cannot determine IMM struct DN">>};

exception(#exception{key=struct_has_no_id}=Ex) ->    Ex#exception{text= <<"IMM struct instance has no id">>};

exception(#exception{key=struct_has_multi_id}=Ex) -> Ex#exception{text= <<"IMM struct instance has multiple id">>};

exception(#exception{key=struct_not_found}=Ex) ->    Ex#exception{text= <<"IMM struct not found">>};

exception(#exception{key=struct_handled}=Ex) ->      Ex#exception{text= <<"IMM struct already handled">>};

exception(Ex) ->
    Ex.

