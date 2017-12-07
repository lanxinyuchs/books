%%% ----------------------------------------------------------
%%% %CCaseFile:	upLib.erl %
%%% Author:	erarafo
%%% Description: Low-level functionality for upInspect
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(upLib).
%-behaviour(behaviour).
-id('Updated by CCase').
-vsn('/main/R5A/R6A/R8A/4').
-date('2016-12-05').
-author('erarafo').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016 All rights reserved.
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
%%% R5A/1      2016-02-11 erarafo     First version
%%% R5A/2      2016-02-15 erarafo     Minor additions
%%% R5A/3      2016-02-15 erarafo     Typespecs improved
%%% R5A/4      2016-02-18 erarafo     Added counters
%%% R5A/5      2016-02-19 erarafo     Restructuring
%%% R5A/6      2016-02-22 erarafo     Added function stringToBoolean/1
%%% R5A/7      2016-02-24 erarafo     Refactoring      
%%% R5A/8      2016-02-25 erarafo     Support for deprecation warnings
%%% R6A/2      2016-04-28 erarafo     Mim/imm cross-validation
%%% R6A/3      2016-05-22 erarafo     Changed representation of 'val'
%%% R8A/1      2016-11-16 erarafo     Graceful throw from varDictGet/2
%%% R8A/2      2016-12-01 erarafo     Support for HW/SW compatibility
%%% R8A/3      2016-12-02 erarafo     Cleanup
%%% R8A/4      2016-12-05 erarafo     Support for .xsd files in 5G repo
%%% ----------------------------------------------------------

-export([
	declare/3, declare/2, valDictBind/3, valDictFromList/2, valDictGet/3, valDictGet/2, valDictGetAll/1,
	varDictBind/3, varDictStore/3, varDictGet/3, varDictGet/2, varDictGetAll/1,
	varDictDelete/2,
	setAddElement/2,
	setMember/2,
	setGetAll/1,
	valBind/2,
	valGet/1,
	sevUpdate/2,
	sevGet/1,
	counterIncr/1,
	counterGet/1]).

-export([getTargetHandler/1,
	 schemaValidate/2,
	 schemaValidate/3,
	 findSchema/1,
	 fileIsReadable/3]).

-export([x/2]).


-export([getTopElement/1, getTopElement/2, getAttribute/2, getAttribute/3, getRequiredAttribute/2,
	 getRequiredNonNegAttribute/3, getNonNegAttribute/3,
	 getSubElements/2, getContainedText/1, getOneSubElement/2,
	 isValidNonNegNumeral/1, stringToBoolean/1,
	 strip/1]).


-export([progress/1, info/2, depr/2, warn/2, err/2]).

% -include_lib("xmerl/include/xmerl.hrl").

-include("upInspect.hrl").


%% TODO, consider macro?
-spec getTargetHandler(string()) -> #targetHandler{}.

getTargetHandler(Target) ->
    varDictGet(targetHandlers, Target).


%%% ----------------------------------------------------------
%%% @doc Validates the given XML against the given schema.
%%% Throws {schema_violated, _} or {schema_not_validated}.
%%% @end
%%% ----------------------------------------------------------

-spec schemaValidate(string(), string()) -> #xsdResult{}.

schemaValidate(XmlFile, XsdKey) ->
    schemaValidate(XmlFile, XsdKey, false).


%%% ----------------------------------------------------------
%%% @doc Validates the given XML against the given schema.
%%% Throws {schema_violated, _} or {schema_not_validated}.
%%% @end
%%% ----------------------------------------------------------

-spec schemaValidate(string(), string(), boolean()) -> #xsdResult{}.

schemaValidate(XmlFile, XsdKey, Verbose) ->
    % look up schema by key, and global_state()
    % do Erlang validation
    % do xmllib validation
    
    % info("debug: get target handler for: ~p", [XsdKey]),
    #targetHandler{schemaPath=SchemaPath, xsdState=_XsdState} = 
	getTargetHandler(XsdKey),
    if 
	SchemaPath =:= undefined ->
	    info("no validation for: ~s", [XsdKey]);
	true ->
	    RcsTop = valDictGet(args, rcsTop),
	    SchemaFile = filename:join(RcsTop, SchemaPath),
	    
	    if 
		Verbose ->
		    info("validation using: ~s", [SchemaPath]);
		true ->
		    ok
	    end,
	    
	    {LibXml2Code, Text} = 
		case x("xmllint",
		       ["--noout", "--schema", SchemaFile, XmlFile]) of
		    #osResult{code=Code, stderr=StdErr} when Code =/= 0 ->
			counterIncr(validationFailed),
			{Code, StdErr};
		    #osResult{} ->
			counterIncr(validationCount),
			{0, ""}
		end,
	    
	    #xsdResult{libXml2Code=LibXml2Code, text=Text}
    end.


	
findSchema(Schema) ->
    findSchemaHelper(Schema, {false, []}).


%%% ----------------------------------------------------------
%%% @doc Finds an .xsd file. If not in the expected place
%%% assuming a 17B view then retry with the expected place
%%% in a 5G Git clone.
%%% @end
%%% ----------------------------------------------------------
-spec findSchemaHelper(relpath(), {boolean(), [relpath()]}) -> tuple().

findSchemaHelper(Schema, {Retry, Paths}) ->
    RcsTop = valDictGet(args, rcsTop),
    SchemaInCxa = filename:join(RcsTop, Schema),
    case file:open(SchemaInCxa, [read]) of
	{ok, Stream} ->
	    file:close(Stream),
	    {cxa, RcsTop, Schema};
	{error, enoent} when Retry ->
	    throw({no_such_schemas, [Schema|Paths]});
	{error, enoent} ->
	    SchemaBasename = filename:basename(Schema),
	    Block = hd(filename:split(Schema)),
	    case
		filelib:wildcard(
		  filename:join([RcsTop, Block, "*CNX*", "*CAX*", "schema", SchemaBasename])) of
		[] ->
		    findSchemaHelper(schemaPathWithoutCxa(Schema), {true, [Schema]});
		[_, _|_]=A ->
		    throw({ambig_schema, {Schema, A}});
		[SchemaInCax] ->
		    {cax, RcsTop, dropPrefix(RcsTop, SchemaInCax)}
	    end
    end.


schemaPathWithoutCxa(Schema) ->
    case filename:split(Schema) of
	[Block, _CxaLevel|Tail] ->
	    filename:join([Block|Tail]);
	_ ->
	    throw({cannot_remove_CXA_level, Schema})
    end.


%%% ----------------------------------------------------------
%%% @doc Verify that the specified file is unique and readable.
%%% The verified pathname is returned.
%%% @end
%%% ----------------------------------------------------------

fileIsReadable(CxpDir, Path, BaseName) ->
    Components = filename:split(Path),
    DirName =
    lists:foldl(
      fun(Component, Acc) ->
	      PartialPath = filename:join(Acc, Component),
	      case filelib:wildcard(PartialPath) of
		  [] ->
		      throw({no_such_dir, PartialPath, BaseName});
		  [_, _|_]=Dirs ->
		      throw({ambig_dir, PartialPath, BaseName, Dirs});
		  [ResolvedPartialPath] ->
		      ResolvedPartialPath
	      end

      end,
      CxpDir,
      Components),
    TotalPath = filename:join(DirName, BaseName),
    case file:open(TotalPath, [read]) of
	{error, Reason} ->
	    throw({cannot_read, {TotalPath, Reason}});
	{ok, Stream} ->
	    file:close(Stream)
    end,
    TotalPath.


%%% ----------------------------------------------------------
%%% @doc Drops the given pathname prefix from the given path.
%%% A relative path is returned. The given paths may be both
%%% absolute or both relative.
%%% @end
%%% ----------------------------------------------------------
dropPrefix(Prefix, Path) ->
    PreCc = filename:split(Prefix),
    PathCc = filename:split(Path),
    ResCc = dropPrefixHelper(PreCc, PathCc),
    filename:join(ResCc).

dropPrefixHelper([], R) ->
    R;

dropPrefixHelper(_, []) ->
    throw(prefix_too_long);

dropPrefixHelper([A|R], [A|S]) ->
    dropPrefixHelper(R, S);

dropPrefixHelper([A|_R], [B|_S]) when A =/= B ->
    throw(prefix_does_not_match).




-spec x(string(), [string()]) -> #osResult{}.

x(Pgm, Args) ->
    Command = "wrapper.sh "++Pgm++" "++arglistToString(Args),
    R = string:strip(os:cmd(Command), right, $\n),
    [CodeS, StdOut, StdErr] = string:tokens(R, "|"),
    Result =
	#osResult{code=list_to_integer(CodeS),
		  stdout=fileToStringList(StdOut),
		  stderr=fileToStringList(StdErr)},
    file:delete(StdOut),
    file:delete(StdErr),
    Result.



arglistToString(Args) ->
    string:join(Args, " ").


-spec fileToStringList(string()) -> [string()].
	  
fileToStringList(File) ->
    {ok, Stream} = file:open(File, [read]),
    Content = fileToStringList(Stream, []),
    file:close(Stream),
    Content.


fileToStringList(Stream, Acc) ->
    case io:get_line(Stream, "") of
	eof ->
	    lists:reverse(Acc);
	{error, ErrorDescription} ->
	    throw({fileToStringList, ErrorDescription});
	String ->
	    fileToStringList(Stream, [string:strip(String, right, $\n)|Acc])
    end.



knownType(val) -> true;           % single assignment variable

knownType(var) -> true;           % variable

knownType(counter) -> true;       % simple counter

knownType(severity) -> true;      % enum {ok, warning, error, fatal}

knownType(valDict) -> true;       % dictionary of immutable bindings

knownType(varDict) -> true;       % dictionary

knownType(set) -> true;

knownType(_) -> false.


declare(Identifier, Type) when Type =:= valDict orelse Type =:= varDict ->
    declare(Identifier, Type, []);

declare(Identifier, set) ->
    declare(Identifier, set, []);

declare(Identifier, Type) ->
    declareHelper(Identifier, Type).


declare(Identifier, Type, InitialValue) ->
    declareHelper(Identifier, Type),
    valueCheck(InitialValue, Type),
    if
	Type =:= valDict orelse Type =:= varDict ->
	    erlang:put(Identifier, orddict:from_list(InitialValue));
	Type =:= set ->
	    erlang:put(Identifier, ordsets:from_list(InitialValue));
	Type =:= val ->
	    erlang:put(Identifier, {val, InitialValue});
	true ->
	    erlang:put(Identifier, InitialValue)
    end.


declareHelper(Identifier, Type) ->
    case knownType(Type) of
	false ->
	    throw({unknown_type, Type});
	true ->
	    Types =
		case erlang:get({types}) of
		    undefined ->
			orddict:new();
		    Dict ->
			Dict
		end,
	    case orddict:is_key(Identifier, Types) of
		true ->
		    throw({type_cannot_be_redefined, Type});
		false ->
		    erlang:put({types},
			       orddict:store(Identifier, Type, Types))
	    end
    end.

typeCheck(Identifier, Type) ->
    case isType(Identifier, Type) of
	false ->
	    throw({bad_type, {Identifier, Type}});
	true ->
	    ok
    end.

valueCheck(fatal, severity) -> true;
valueCheck(error, severity) -> true;
valueCheck(warning, severity) -> true;
valueCheck(deprec, severity) -> true;
valueCheck(ok, severity) -> true;

valueCheck(BadValue, severity) -> throw({bad_value, {BadValue, severity}});

valueCheck(PairList, Dict) when Dict =:= valDict orelse Dict =:= varDict ->
    lists:all(
      fun({_, _}) ->
	      true;
	 (_) ->
	      false
      end,
      PairList);

valueCheck(List, set) ->
    is_list(List);

valueCheck(_, _) -> true.

isType(Identifier, Type) ->
    Types = erlang:get({types}),
    case Types of
	undefined ->
	    false;
	_ ->
	    case orddict:find(Identifier, Types) of
		error ->
		    false;
		{ok, T} when T =/= Type ->
		    false;
		_ ->
		    true
	    end
    end.


-spec valDictBind(globalName(), any(), any()) -> ok.

valDictBind(DictName, Key, Value) ->
    typeCheck(DictName, valDict),
    Dict = erlang:get(DictName),
    case orddict:find(Key, Dict) of
	{ok, Old} ->
	    throw({bad_bind, {DictName, Key, Old, Value}});
	error ->
	    erlang:put(DictName, orddict:store(Key, Value, Dict)),
	    ok
    end.


valDictFromList(DictName, PairList) ->
    lists:foreach(
      fun({Key, Value}) ->
	      valDictBind(DictName, Key, Value)
      end,
      PairList).


valDictGet(DictName, Key, Default) ->
    try
	valDictGet(DictName, Key)
    catch
	throw:{unbound, _} ->
	    Default
    end.

valDictGet(DictName, Key) ->
    typeCheck(DictName, valDict),
    Dict = erlang:get(DictName),
    case orddict:find(Key, Dict) of
	error ->
	    throw({unbound, Key});
	{ok, Value} ->
	    Value
    end.


%% returns orddict

valDictGetAll(DictName) ->
    typeCheck(DictName, valDict),
    erlang:get(DictName).






-spec varDictBind(atom(), string(), any()) ->  any().

varDictBind(DictName, Key, Value) ->
    typeCheck(DictName, varDict),
    Dict = erlang:get(DictName),
    case orddict:find(Key, Dict) of
	{ok, Old} when Old =/= Value ->
	    throw({bad_bind, {DictName, Key, Old, Value}});
	{ok, _Old} ->
	    ok;
	error ->
	    erlang:put(DictName, orddict:store(Key, Value, Dict))
    end.


-spec varDictStore(atom(), any(), any()) ->  any().

varDictStore(DictName, Key, Value) ->
    typeCheck(DictName,varDict),
    Dict = erlang:get(DictName),
    erlang:put(DictName, orddict:store(Key, Value, Dict)).


varDictGet(DictName, Key, Default) ->
    try
	varDictGet(DictName, Key)
    catch
	throw:{lookup_failed, _} ->
	    Default
    end.


-spec varDictGet(atom(), any()) -> any().

varDictGet(DictName, Key) ->
    typeCheck(DictName, varDict),
    Dict = erlang:get(DictName),
    case orddict:find(Key, Dict) of
	error ->
	    throw({lookup_failed, Key});
	{_, Value} ->
	    Value
    end.


-spec varDictGetAll(atom()) -> orddict:orddict().

varDictGetAll(DictName) ->
    typeCheck(DictName, varDict),
    erlang:get(DictName).



varDictDelete(DictName, Key) ->
    typeCheck(DictName, varDict),
    Dict = erlang:get(DictName),
    erlang:put(DictName, orddict:erase(Key, Dict)).



setAddElement(Element, SetName) ->
    typeCheck(SetName, set),
    Set = erlang:get(SetName),
    NewSet = ordsets:add_element(Element, Set),
    erlang:put(SetName, NewSet).


setMember(Element, SetName) ->
    typeCheck(SetName, set),
    ordsets:is_element(Element, erlang:get(SetName)).


setGetAll(SetName) ->
    typeCheck(SetName, set),
    erlang:get(SetName).


valBind(ValName, Value) ->
    typeCheck(ValName, val),
    case erlang:get(ValName) of
	undefined ->
	    erlang:put(ValName, {val, Value});
	{val, StoredValue} when StoredValue =:= Value ->
	    ok;
	{val, StoredValue} ->
	    throw({cannot_rebind, {ValName, StoredValue, Value}})
    end.


valGet(ValName) ->
    typeCheck(ValName, val),
    case erlang:get(ValName) of
	undefined ->
	    throw({unbound, ValName});
	{val, Value} ->
	    Value
    end.

sevUpdate(Name, Value) ->
    typeCheck(Name, severity),
    valueCheck(Value, severity),
    case erlang:get(Name) of
	undefined ->
	    erlang:put(Name, Value);
	OldValue ->
	    case sevGreater(Value, OldValue) of
		false ->
		    ok;
		true ->
		    erlang:put(Name, Value)
	    end
    end.

sevGet(Name) ->
    case erlang:get(Name) of
	undefined ->
	    throw({undefined, {Name, severity}});
	Value ->
	    Value
    end.


counterIncr(Name) ->
    typeCheck(Name, counter),
    erlang:put(Name, erlang:get(Name) + 1).

counterGet(Name) ->
    typeCheck(Name, counter),
    erlang:get(Name).



-spec sevGreater(severity(), severity()) -> boolean().

sevGreater(fatal, fatal) -> false;
sevGreater(fatal, _) -> true;

sevGreater(error, ok) -> true;
sevGreater(error, deprec) -> true;
sevGreater(error, warning) -> true;
sevGreater(error, _) -> false;

sevGreater(warning, ok) -> true;
sevGreater(warning, deprec) -> true;
sevGreater(warning, _) -> false;

sevGreater(deprec, ok) -> true;
sevGreater(deprec, _) -> false;

sevGreater(ok, _) -> false.


%% @doc Gets the top element from the given XML file and
%% verifies its name.

-spec getTopElement(string()) ->  #xmlElement{}.

getTopElement(Pathname) ->
    getTopElement(Pathname, []).



%% @doc Gets the top element from the given XML file and
%% verifies its name.

-spec getTopElement(string(), [string()]) ->  #xmlElement{}.

getTopElement(Pathname, PathList) ->
    case xmerl_scan:file(Pathname, [{validation, off}, {fetch_path, PathList}]) of
	{#xmlElement{}, Junk} when Junk =/= [] ->
	    throw({trailing_data, Pathname});
	{#xmlElement{}=Result, []} ->
	    Result
    end.


%% @doc Gets the value of the named attribute from the given
%% element, or 'undefined' if the attribute is missing.

-spec getAttribute(atom(), #xmlElement{}) ->  string() | undefined.

getAttribute(Name, #xmlElement{attributes=AA}) ->
    case lists:keyfind(Name, #xmlAttribute.name, AA) of
	false ->
	    undefined;
	#xmlAttribute{value=Value} ->
	    Value
    end.


%% @doc Gets the value of the named attribute from the given
%% element, or the third argument if the attribute is missing.

-spec getAttribute(atom(), #xmlElement{}, any()) ->  string() | any().

getAttribute(Name, Element, Default) ->
    case getAttribute(Name, Element) of
	undefined ->
	    Default;
	Value ->
	    Value
    end.


%% @doc Returns the value of a required attribute. This function is
%% called from gmfAppData, gmfAppCli and gmfImmUgVerifyUpgrade.
%% @end

getRequiredAttribute(Name, #xmlElement{attributes=AA}) ->
    case lists:keyfind(Name, #xmlAttribute.name, AA) of
	false ->
	    throw({missing_attribute, Name});
	#xmlAttribute{value=Value} ->
	    Value
    end.


-spec getRequiredNonNegAttribute(atom(), #xmlElement{}, string()) -> integer().

getRequiredNonNegAttribute(Name, #xmlElement{attributes=AA}, _Context) ->
        case lists:keyfind(Name, #xmlAttribute.name, AA) of
	false ->
	    throw(fixme);
	    % ?FAULT([error], "missing attribute: ~w, context: ~s", [Name, Context]);
	#xmlAttribute{value=Value} ->
	    case isValidNonNegNumeral(Value) of
		false ->
		    throw(fixme);
		   % ?FAULT([error], "ill-formed numeric attribute: ~w, value: ~s, context: ~s", [Name, Value, Context]);
		true ->
		    list_to_integer(Value)
	    end
    end.


%% @doc Gets a non-negative attribute, or 0 if the attribute is
%% not present.

-spec getNonNegAttribute(atom(), #xmlElement{}, string()) -> integer().

getNonNegAttribute(Name, #xmlElement{attributes=AA}, _Context) ->
        case lists:keyfind(Name, #xmlAttribute.name, AA) of
	false ->
	    0;
	#xmlAttribute{value=Value} ->
	    case isValidNonNegNumeral(Value) of
		false ->
		    throw(fixme);
		   % ?FAULT([error], "ill-formed numeric attribute: ~w, value: ~s, context: ~s", [Name, Value, Context]);
		true ->
		   list_to_integer(Value)
	    end
    end.


%% @doc Returns a list of contained elements matching
%% the given element name. This function is called from
%% gmfAppCli and gmfImmUgVerifyUpgrade.
%% @end

-spec getSubElements(atom(), #xmlElement{}) -> [#xmlElement{}].

getSubElements(Name, #xmlElement{content=Content}) ->
    [S || #xmlElement{name=N}=S <- Content, N =:= Name].


%% @doc Returns the leftmost contained text from the given
%% element, or 'undefined' if none exists.

-spec getContainedText(#xmlElement{}) -> string() | undefined.

getContainedText(#xmlElement{content=Content}) ->
    lists:foldr(fun(#xmlText{value=Value}, _A) ->
			Value;
		   (_, A) ->
			A
		end,
		undefined,
		Content).


%% @doc Get exactly one subelement from the given
%% element.

-spec getOneSubElement(atom(), #xmlElement{}) -> #xmlElement{}.

getOneSubElement(Name, #xmlElement{content=Content}) ->
    case [S || #xmlElement{name=N}=S <- Content, N =:= Name] of
	[] ->
	    throw({no_element, Name});
	[Result] ->
	    Result;
	Multiple ->
	    throw({multiple_elements, Name, Multiple})
    end.

%% @doc Returns true only if the given string is "0" or
%% a non-empty list of decimal digits where the leading
%% digit is not zero.

isValidNonNegNumeral("0") ->
    true;

isValidNonNegNumeral([$0, _|_]) ->
    false;

isValidNonNegNumeral([$+|_]) ->
    false;

isValidNonNegNumeral(X) ->
    case string:to_integer(X) of
	{error, _} ->
	    false;
	{N, []} ->
	    N > 0;
	_ ->
	    false
    end.


stringToBoolean("false") ->
    false;

stringToBoolean("true") ->
    true.


%%% ----------------------------------------------------------
%%% @doc Strip leading and trailing whitespace from the
%% given string.
%%% @end
%%% ----------------------------------------------------------
-spec strip(string()) -> string().

strip(String) ->
    {_, _, Result} =
	lists:foldr(
	  fun(C, {start, _, _}=Acc) when C =:= $\s orelse
					     C =:= $\t orelse
					     C =:= $\r orelse
					     C =:= $\n orelse
					     C =:= $\f ->
		  Acc;
	     (C, {start, _, _}) ->
		  {middle, [], [C]};
	     (C, {middle, B, R}) when C =:= $\s orelse
					  C =:= $\t orelse
					  C =:= $\r orelse
					  C =:= $\n orelse
					  C =:= $\f->
		  {middle, [C|B], R};
	     (C, {middle, [_|_]=B, R}) ->
		  {middle, [], [C|B++R]};
	     (C, {middle, [], R}) ->
		  {middle, [], [C|R]}
	  end,
	  {start, [], []},
	  String),
    Result.


progress(Format) ->
    io:format("INFO: "++Format++" ...~n").


info(Format, Data) ->
    io:format("INFO: "++Format++"~n", Data).

depr(Format, Data) ->
    sevUpdate(status, deprec),
    counterIncr(deprCount),
    DeprecationWarnings = valGet(deprecationWarnings),
    if
	DeprecationWarnings ->
	    io:format("DEPR: "++Format++"~n", Data);
	true ->
	    ok
    end.

warn(Format, Data) ->
    sevUpdate(status, warning),
    counterIncr(warningCount),
    io:format("WARNING: "++Format++"~n", Data).

err(Format, Data) ->
    sevUpdate(status, error),
    counterIncr(errorCount),
    io:format("ERROR: "++Format++"~n", Data).

