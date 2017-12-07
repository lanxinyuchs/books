%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfImmUgVerifyUpgrade.erl %
%%% Author:	erarafo
%%% Description: Handles verification of schemas in the
%%% "activate" phase of upgrade (before the restart, at the
%%% verify_upgrade/0 callback).
%%%
%%% Modules used: gmfImmUgLib
%%%
%%% ----------------------------------------------------------
-module(gmfImmUgVerifyUpgrade).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/1').
-date('2016-06-13').
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
%%%%
%%% ----------------------------------------------------------
%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R2A/1      2013-09-16 erarafo     First version
%%% R2A/2      2013-09-18 erarafo     Minor adjustment
%%% R2A/4      2013-09-19 erarafo     Eliminated #schema{}
%%% R2A/5      2013-10-03 erarafo     To-version IMM classnames checked
%%% R2A/6      2013-10-21 erarafo     Long lines eliminated
%%% R2A/7      2014-01-12 erarafo     Deferred reporting of versioning errors
%%% R2A/8      2014-02-07 erarafo     Downgrade stopped, diagnostic
%%% R2A/9      2014-03-06 erarafo     Error messages cleaned up
%%% R2A/10     2014-03-18 erarafo     Events for restart log added
%%% R2A/11     2014-05-05 erarafo     Revised error handling
%%% R2A/12     2014-05-16 erarafo     HS60956: Return type adjusted
%%% R2A/13     2014-05-20 erarafo     Do not use
%%% R2A/14     2014-05-20 erarafo     Handle ill-formed XML gracefully
%%% R2A/15     2014-05-21 erarafo     HS62261: Severity WARNING when user fault
%%% R2A/18     2014-07-01 erarafo     OTP 17 adaptation
%%% R2A/19     2014-07-10 etxjotj     Added extra fault information printouts
%%% R2A/20     2014-10-14 erarafo     Listing appdata/schema/classfile relation
%%% R2A/21     2014-10-15 erarafo     Bugfix
%%% R3A/1      2014-12-05 erarafo     Extended diagnostics
%%% R3A/2      2015-03-10 erarafo     Catch reservedBy when SA_RUNTIME
%%% R3A/3      2015-03-17 erarafo     Undo the R3A/2 catch
%%% R4A/1      2015-05-19 erarafo     New insertion algorithm
%%% R4A/2      2015-09-08 etxjotj     Changed warning to errors
%%% R5A/1      2016-04-11 etxjotj     SWM update
%%% R6A/1      2016-06-13 erarafo     LTTng trace adjusted


-include("gmf.hrl").
-include("gmfMeta.hrl").
-include("gmfImmUg.hrl").

-include_lib("xmerl/include/xmerl.hrl").

-export([verify_upgrade/0]).

-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2, 
	 terminate/2, 
	 code_change/3]).


%% Fault types that the verification catches. The number indicates
%% priority; the overall verdict from the verification will be the
%% one with lowest number if several faults were reported.

-define(DUPLICATE_BASENAME, {10, "duplicate basename"}).
-define(DUPLICATE_SCHEMANAME, {20, "duplicate schema name"}).
-define(VERSIONING_FAILURE, {30, "versioning failure"}).
-define(DUPLICATE_CLASS_IN_FILE, {40, "duplicate classnames in file"}).
-define(DUPLICATE_CLASS, {50, "duplicate classnames"}).
-define(INC_RESERVED_BY, {90, "incorrect use of 'reservedBy'"}).


-record(report, {faultType      :: tuple(),
		 format=""      :: string(),
		 data=[]        :: [any()]
		}).

-record(state, {reports=ordsets:new() :: ordsets:ordset(#report{})
	       }).

-define(SERVER, ?MODULE).


%%% ----------------------------------------------------------
%%% @doc SWM callback function. If the returned value is
%%% {error, Message} we trust that SWM enforces a restart.
%%% @end
%%% ----------------------------------------------------------

-spec verify_upgrade() -> ok | {error, string()}.

verify_upgrade() ->
    ?INFO("verify_upgrade/0, version: ~p", [gmfImmUgLib:getVersion(?MODULE)]),
    gmfImmUgLib:progress(?MODULE, ?LINE, "upgrade: verify during activate starts"), 
    try 
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
	verifyUpgrade(),
	handleFaults()
    catch
	exit:({fatal, {{error, _}, _, _, _}}=Reason) ->
	    sysInitI:error_report([{mfa, {?MODULE, verify_upgrade, []}},
				   {caught, {exit, Reason}},
				   {stack, erlang:get_stacktrace()}
				  ]),
	    {error, "ill-formed XML in upgrade package"};
	Type:Reason ->
	    sysInitI:error_report([{mfa, {?MODULE, verify_upgrade, []}},
				   {caught, {Type, Reason}},
				   {stack, erlang:get_stacktrace()}
				  ]),
	    {error, gmfImmUgLib:format("~p: ~p", [Type, Reason])}
    after
	handleFaults(),
	gen_server:cast(?SERVER, {stop}),
	gmfImmUgLib:progress(?MODULE, ?LINE, "upgrade: verify during activate done")
    end.


%%% ----------------------------------------------------------
%%% @doc Verification of the To-UP, runs within a try-catch.
%%% @end
%%% ----------------------------------------------------------

-spec verifyUpgrade() -> any().

verifyUpgrade() ->
    FromSchemasByBasename = gmfImmUgLib:getCurrentSchemasByBasename("'From'"),
    FromSchemasByName = gmfImmUgLib:getSchemasByName(FromSchemasByBasename),
    
    %% Use of internal SWM functions is not allowed
    %% SwVersionDir = swmLib:software_dir_other(),
    TaintedToSchemas = gmfMetaLib:getCxsSchemas(other),
        
    ?INFO("summary of appdata in To-UP follows -", []),
    [?INFO("~s: ~s -> ~s", 
	   [AAP, 
	    if SchemaType =:= unnamedSchema -> "(unnamed)"; true -> SN end,
	    AP])
     ||#immInfo{type=SchemaType, 
		cat=AAP, 
		schemaName=SN, 
		abspath=AP} <- TaintedToSchemas],
    
    ToSchemas = gmfMetaLib:dropCat(TaintedToSchemas),

    scanClasses(ToSchemas),
    
    ToSchemasByBasename =
	schemasToDict(ToSchemas, "To-version schemas"),
    ToSchemasByName =
	gmfImmUgLib:getSchemasByName(ToSchemasByBasename),
    
    {_ToSchemasByAbspath, _S0, S1V0, S1V1, S1V2, S1V3} =
	gmfImmUgLib:scanOldSchemas(
	  FromSchemasByBasename,
	  ToSchemasByBasename,
	  ToSchemasByName),
    
    ToSchemasMatched = S1V0++S1V1++S1V2++S1V3,
    gmfImmUgLib:scanNewSchemas(
      FromSchemasByBasename,
      ToSchemasByBasename,
      ToSchemasMatched),
    
    Failures =
	gmfImmUgLib:checkUpgradePaths(S1V1)++
	    gmfImmUgLib:checkUpgradePaths(S1V2++S1V3, FromSchemasByName),
    [report(?VERSIONING_FAILURE,
	    "upgrade not supported for schema: ~s,~n"
	    "to-version: ~p,~n"
	    "~s from-version: ~p,~n"
	    "supported from-versions: ~p",
	    [N, NewV, OldM, OldV, FVV])
    ||#versionFailure{schemaName=N,
			 fromVersions=FVV,
			 oldVersion=OldV,
			 oldMode=OldM,
			 toVersion=NewV} <- Failures].


%%% ----------------------------------------------------------
%%% @doc Handles collected fault reports.
%%% @end
%%% ----------------------------------------------------------

-spec handleFaults() -> ok | {error, string()}.

handleFaults() ->
    case ?CALL(?SERVER, {getReportsAndClear}) of
	[] ->
	    ok;
	[#report{faultType={_Prio, Reason}}|_]=Reports ->
	    [?FAULT([user], Format, Data)
	     || #report{format=Format, data=Data} <- Reports],
	    {error, Reason}
    end.


%% Converts a list of #immInfo{} to a dictionary
%% with basename as key. Invoke with an empty list
%% as 2nd argument. Checks against duplicate basenames
%% and schema names are made.

%%% ----------------------------------------------------------
%%% @doc Converts a list of #immInfo{} to a dictionary
%%% with basename as key. Checks against duplicate basenames
%%% and schema names are made.
%%% @end
%%% ----------------------------------------------------------

-spec schemasToDict([#immInfo{}], string()) -> dict:dict().

schemasToDict(ImmInfos, Context) ->
    {DictResult, _} =
	lists:foldl(
	  fun(#immInfo{type=namedSchema, 
		       schemaName=SN, 
		       file=BaseName,
		       abspath=AbsPath}=I, 
	      {Dict, AbsPathBySchemaName}) ->
		  case dict:is_key(BaseName, Dict) of
		      true ->
			  reportDuplicateBasename(BaseName, AbsPath, Dict, Context),
			  {Dict, AbsPathBySchemaName};
		      _ ->
			  case orddict:find(SN, AbsPathBySchemaName) of
			      {ok, OtherAbsPath} ->
				  report(?DUPLICATE_SCHEMANAME,
					 "duplicate schema name: ~s, "
					 "context: ~s, "
					 "referred files: ~s, ~s",
					 [SN, Context, AbsPath, OtherAbsPath]),
				  {dict:store(BaseName, I, Dict), AbsPathBySchemaName};
			      _ ->
				  {dict:store(BaseName, I, Dict), 
				   orddict:store(SN, AbsPath, AbsPathBySchemaName)}
			  end
		  end;
	     (#immInfo{type=unnamedSchema, 
		       file=BaseName,
		       abspath=AbsPath}=I, 
	      {Dict, SchemaNames}) ->
		  case dict:is_key(BaseName, Dict) of
		      true ->
			  reportDuplicateBasename(BaseName, AbsPath, Dict, Context),
			  {Dict, SchemaNames};
		      _ ->
			  {dict:store(BaseName, I, Dict), SchemaNames}
		  end
	  end,
	  {dict:new(), orddict:new()},
	  ImmInfos),
    DictResult.


reportDuplicateBasename(BaseName, AbsPath, Dict, Context) ->
    #immInfo{abspath=PreviousAbsPath} = dict:fetch(BaseName, Dict),
    report(?DUPLICATE_BASENAME,
	   "duplicate basename: ~s, context: ~s, files: ~s, ~s",
	   [BaseName, Context, AbsPath, PreviousAbsPath]).


%%% ----------------------------------------------------------
%%% @doc Scans classes referenced by the given list of #immInfo{}
%%% records. Classes that occur in more than one referenced file
%%% are reported.
%%% @end
%%% ----------------------------------------------------------

-spec scanClasses([#immInfo{}]) ->  ok.

scanClasses(ImmInfos) ->
    lists:foldl(
      fun(#immInfo{type=SchemaType, abspath=AbsPath, schemaName=SchemaName}, 
	  Dict) 
	   when SchemaType =:= namedSchema orelse 
		    SchemaType =:= unnamedSchema ->
	      Name = if SchemaType =:= namedSchema -> SchemaName; true -> "<unnamed>" end, 
	      MoreClasses = scanClassesInFile(AbsPath, Name),
	      [case orddict:find(Class, Dict) of
		   error ->
		       ok;
		   {ok, OldPath} ->
		       report(?DUPLICATE_CLASS,
			      "duplicate class: ~s,"
			      " occurs in: ~s, ~s",
			      [Class, AbsPath, OldPath])
	       end
	       || Class <- MoreClasses],
	      orddict:merge(
		fun(_K, Y, _Z) -> Y end, 
		Dict, 
		orddict:from_list([{Class, AbsPath}||Class <- MoreClasses]));
	 (_, Dict) ->
	      Dict
      end,
      orddict:new(),
      ImmInfos),
    ok.


%%% ----------------------------------------------------------
%%% @doc Scans one imm_classes file and returns the set of
%%% classnames therein. Checks that classnames are not repeated,
%%% also checks that reservedBy attributes are sane everywhere.
%%% ----------------------------------------------------------

-spec scanClassesInFile(string(), string()) -> 
	  ordsets:ordset(string()).

scanClassesInFile(AbsPath, SchemaName) ->
    {TopElement, _} = xmerl_scan:file(AbsPath),
    ClassElements = gmfMetaLib:getSubElements(class, TopElement),
    ReservedByS = binary_to_list(?RESERVED_BY),
    Classes = 
	[begin 
	     ClassName = 
		 gmfMetaLib:getRequiredAttribute(name, ClassElement, ""),
	     AttrElements = gmfMetaLib:getSubElements(attr, ClassElement),
	     [begin
		  [NameElement] = gmfMetaLib:getSubElements(name, AttrElement),
		  AttrNameS = gmfMetaLib:getContainedText(NameElement),
		  case AttrNameS of
		      ReservedByS ->
			  IsNoDangling = hasFlag("SA_NO_DANGLING", AttrElement),
			  if
			      IsNoDangling ->
				  report(?INC_RESERVED_BY, 
					 "incorrect use of 'reservedBy': must not be 'SA_NO_DANGLING', "
					 "class: ~s, "
					 "schema: ~s, "
					 "file: ~s", 
					 [ClassName, SchemaName, AbsPath]);
			      true ->
%% 				  case getCategory(AttrElement) of
%% 				      "SA_RUNTIME" ->
%% 					  case hasFlag("SA_PERSISTENT", AttrElement) of
%% 					      true ->
%% 						  % allowing SA_RUNTIME if also SA_PERSISTENT
%% 						  ok;
%% 					      false ->
%% 						  report(
%% 						    ?INC_RESERVED_BY, 
%% 						    "incorrect use of 'reservedBy', "
%% 						    "must not be non-persistent 'SA_RUNTIME', "
%% 						    "class: ~s, "
%% 						    "schema: ~s, "
%% 						    "file: ~s", 
%% 						    [ClassName, SchemaName, AbsPath])
%% 					  end;
%% 				      _ ->
%% 					  % assuming "SA_CONFIG"
%% 					  ok
%% 				  end
				  ok
			  end;
		      _ ->
			  ok
		  end
	      end
	      || AttrElement <- AttrElements],
	     lists:flatten(
	       gmfMetaLib:getRequiredAttribute(
		 name,
		 ClassElement,
		 "scan for duplicate classes"))
	 end
	 || ClassElement <- ClassElements],
    ?INFO("scanned: ~s~n - classes: ~p", [AbsPath, Classes]),
    Result = ordsets:from_list(Classes), 
    Duplicates = ordsets:from_list(Classes -- ordsets:to_list(Result)),
    if
	Duplicates =/= [] ->
	    report(?DUPLICATE_CLASS_IN_FILE, 
		   "classnames repeated: ~p, in file: ~s", 
		   [Duplicates, AbsPath]);
	true ->
	    ok
    end,
    Result.


-spec report({integer(), string()}, string(), [any()]) -> any().

report(FaultType, Format, Data) ->
    gen_server:cast(?SERVER, 
		    #report{faultType=FaultType, 
			    format=Format, 
			    data=Data}).


-spec hasFlag(string(), #xmlElement{}) -> boolean().

hasFlag(FlagValue, AttrElement) ->
    lists:any(
      fun(Flag) ->
	      case gmfMetaLib:getContainedText(Flag) of
		  FlagValue ->
		      true;
		  _ ->
		      false
	      end
      end, 
      gmfMetaLib:getSubElements(flag, AttrElement)).


%% -spec getCategory(#xmlElement{}) -> string().
%% 
%% getCategory(AttrElement) ->
%%     [CategoryE] = gmfMetaLib:getSubElements(category, AttrElement),
%%     gmfMetaLib:getContainedText(CategoryE).
    
    
init([]) ->
    {ok, #state{}}.


handle_call({getReportsAndClear}, _From, #state{reports=Reports}) ->
    {reply, ordsets:to_list(Reports), #state{}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(#report{}=Report, #state{reports=Reports}=State) ->
    {noreply, State#state{reports=ordsets:add_element(Report, Reports)}};

handle_cast({clear}, _State) ->
    {noreply, #state{}};

handle_cast({stop}, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
