%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gmfMetaLib.erl %
%%% Author:	erarafo
%%% Description: Functions for metadata parsing. Ill-formed metadata
%%% causes erlang:error/1 to be invoked.
%%%
%%% This module is used by: gmfAppData, gmfAppImm, gmfAppCli,
%%% gmfImmUgVerifyUpgrade.
%%%
%%% ----------------------------------------------------------
-module(gmfMetaLib).
-vsn('/main/R2A/R3A/R4A/R5A/2').
-date('2016-04-15').
-author('etxpeno').
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
%%% R2A/1      2013-09-18 erarafo     First version
%%% R2A/2      2013-09-19 erarafo     Eliminated #schema{}
%%% R2A/3      2013-09-24 erarafo     More functions exported
%%% R2A/4      2013-10-24 erarafo     3-level schema versioning support
%%% R2A/5      2013-11-19 erarafo     Support for new metadata format
%%% R2A/6      2013-11-19 erarafo     Bugfix
%%% R2A/7      2014-05-05 erarafo     Revised error handling
%%% R2A/8      2014-07-10 etxjotj     Support for bundles
%%% R2A/10     2014-10-14 erarafo     Warn against misplaced schema element,
%%%                                   populate #immInfo.appdataAbspath
%%% R2A/11     2014-10-15 erarafo     Use #immInfo.cat instead
%%% R3A/1      2014-12-04 erarafo     Added function getContainedText/1
%%% R4A/1      2015-04-30 erarafo     Module dependencies
%%% R5A/1      2016-04-11 etxjotj     Removed getCxsSchemas and associated funcs
%%% R5A/2      2016-04-15 etxpeno     edoc fixes
%%% ----------------------------------------------------------

-export([getCxsSchemas/1,
	 getAppdataImmInfos/2,
	 getRequiredAttribute/3,
	 getSubElements/2,
	 getContainedText/1,
	 dropCat/1
	 ]).

-include("gmf.hrl").
-include("gmfMeta.hrl").
-include("gmfImmUg.hrl").
-include_lib("xmerl/include/xmerl.hrl").



%% Represents a CXP product in the CXS metadata.

%% -record(cxp, {
%% 	      name,
%% 	      id,
%% 	      ver,
%% 	      dirname
%% 	     }).


%% Represents a CXC product in the CXP metadata.

%% -record(cxc, {
%% 	      name,
%% 	      id,
%% 	      ver,
%% 	      appdatas
%% 	     }).


%% Represents an appdata reference within a CXC product
%% element. The file may be 'undefined' in which case
%% files should be wilcarded for.

%% -record(appdata, {
%% 		  relpath,
%% 		  file
%% 		 }).


%% Represents a fromVersion element in gmfImm metadata.

-record(fromVersion,{
		     version               :: non_neg_integer(),
		     release=undefined     :: non_neg_integer() | undefined,
		     correction=undefined  :: non_neg_integer() | undefined
		    }).

%%% ----------------------------------------------------------
%%% NOTE!
%%%
%%% Several functions are obsoleted because they requires knowledge
%%% about SWM file structure details that are no longer true
%%% ----------------------------------------------------------


%%% ----------------------------------------------------------
%%% @doc Gets the schemas of the CXS which is trusted
%%% to exist in the given software directory.
%%% @end
%%% ----------------------------------------------------------

%% Single caller: gmfImmUgVerifyUpgrade:verifyUpgrade/0

-spec getCxsSchemas(Version::current|other) -> [#immInfo{}].

getCxsSchemas(Version) ->
    %% CxsFile = getCxsFile(SoftwareDir),
    %% Cxps = getCxps(CxsFile),
    %% getCxpSchemas(Version).

    AppDataFiles = swmI:get_appdata_files(Version, "gmfImm"),
    lists:foldl(fun getImmInfo/2, [], AppDataFiles).

getImmInfo(Pathname, ImmData) ->
    TopElement = getTopElement(appdata, Pathname),
    %% CxpDir seems only to be used for fault identification
    %% I am therefore going to not bother about sending the correct info
    %% here
    CxpDir = case swmI:get_cxp_root(Pathname) of
		 {ok, D} -> io:format("D=~p~n",[D]),D;
		 {error, path_not_in_cxp} ->
		     erlang:error(path_not_in_cxp, [Pathname, ImmData])
	     end,
    ImmInfos = getAppdataImmInfos(TopElement, CxpDir, Pathname),
    ImmData++
	[ImmInfo || #immInfo{type=Type}=ImmInfo <- ImmInfos,
		    Type =:= namedSchema orelse Type =:= unnamedSchema].

%% -spec getCxsFile(string()) -> string().

%% getCxsFile(SoftwareDir) ->
%%     case getUniqueFile(filename:join(SoftwareDir, "*-up.xml"), optional) of
%% 	none ->
%% 	    getUniqueFile(filename:join(SoftwareDir, "cxs*.xml"), required);
%% 	Result ->
%% 	    Result
%%     end.


%% doc Gets the gmfImm schemas from the given list of
%% CXP records.


%% -spec getCxpSchemas([#cxp{}], string()) ->  [#immInfo{}].

%% getCxpSchemas(Cxps, NewSoftwareDir) ->
%%     lists:flatten(
%%       [begin
%% 	   WildName = filename:join([NewSoftwareDir, CxpDirName, "cxp*.xml"]),
%% 	   case filelib:wildcard(WildName) of
%% 	       [CxpPathname] ->
%% 		   Cxcs = getCxcs(CxpPathname),
%% 		   getCxcSchemas(Cxcs, filename:join([NewSoftwareDir, CxpDirName]));
%% 	       [] ->
%% 		   ?FAULT([error], "no file matches: ~s", [WildName]);
%% 	       Multiple ->
%% 		   ?FAULT([error], "multiple files match pattern: ~s, files: ~p", [WildName, Multiple])
%% 	   end
%%        end
%%        || #cxp{dirname=CxpDirName} <- Cxps]).



%% doc Gets gmfImm schemas from the given CXC records.

%% -spec getCxcSchemas([#cxc{}], string()) ->  [#immInfo{}].

%% getCxcSchemas(Cxcs, CxpDir) ->
%%     lists:flatten(
%%       [[case F of
%% 	    undefined ->
%% 		AppdataFiles = filelib:wildcard(filename:join([CxpDir, R, "*"])),
%% 		[getAppdataFileSchemas(AppdataFile, CxpDir)|| AppdataFile <- AppdataFiles];
%% 	    _ ->
%% 		AppdataFile = filename:join([CxpDir, R, F]),
%% 		getAppdataFileSchemas(AppdataFile, CxpDir)
%% 	end
%% 	|| #appdata{relpath=R, file=F} <- AA]
%%        || #cxc{appdatas=AA} <- Cxcs]).


%% doc Gets the gmfImm schemas from the given appdata
%% file.

%% -spec getAppdataFileSchemas(string(), string()) ->  [#immInfo{}].

%% getAppdataFileSchemas(Pathname, CxpDir) ->
%%     TopElement = getTopElement(appdata, Pathname),
%%     Target = getAttribute(target, TopElement),
%%     case Target of
%% 	"gmfImm" ->
%% 	    ImmInfos = getAppdataImmInfos(TopElement, CxpDir, Pathname),
%% 	    [ImmInfo || #immInfo{type=Type}=ImmInfo <- ImmInfos,
%% 			Type =:= namedSchema orelse Type =:= unnamedSchema];
%% 	_ ->
%% 	    []
%%     end.


%% @doc Returns a list of immInfo records found in the given appdata
%% top element.
%%
%% External caller: gmfAppImm:appdata/3

-spec getAppdataImmInfos(#xmlElement{}, string()) -> [#immInfo{}].

getAppdataImmInfos(TopElement, CxpDir) ->
    dropCat(getAppdataImmInfos(TopElement, CxpDir, "")).


%% @doc Converts the given list of #immInfo{} by
%% replacing the 'cat' field by the default value.

-spec dropCat([#immInfo{}]) -> [#immInfo{}].

dropCat(TaintedImmInfos) ->
    [#immInfo{file=ZF,
	      path=ZP,
	      abspath=ZAP,
	      type=ZT,
	      schemaName=ZSN,
	      version=ZV,
	      fromVersions=ZFV}
     ||#immInfo{file=ZF,
		path=ZP,
		abspath=ZAP,
		type=ZT,
		schemaName=ZSN,
		version=ZV,
		fromVersions=ZFV} <- TaintedImmInfos].


%% @doc Returns a list of immInfo records found in the given appdata
%% top element.
%%
%% External caller: gmfImmUgVerifyUpgrade:verifyUpgrade/0 (indirectly)

-spec getAppdataImmInfos(#xmlElement{}, string(), string()) -> [#immInfo{}].

getAppdataImmInfos(TopElement, CxpDir, AppdataAbsPath) ->
    ImmInfos = getOneSubElement(immInfos, TopElement, CxpDir),
    [getAppdataImmInfo(ImmInfo, CxpDir, AppdataAbsPath)
    || ImmInfo <- getSubElements(immInfo, ImmInfos)].


getAppdataImmInfo(ImmInfo, CxpDir, AppdataAbsPath) ->
    case getRequiredAttribute(fileType, ImmInfo, CxpDir) of
	?GMF_IMM_FT_CLASS ->
	    getAppdataImmInfoForClasses(ImmInfo, CxpDir, AppdataAbsPath);
	?GMF_IMM_FT_OBJECT ->
	    getAppdataImmInfoForObjects(ImmInfo, CxpDir, AppdataAbsPath);
	WrongType ->
	    ?FAULT([error], "ill-formed immInfo element, type: ~s, file: ~s",
		   [WrongType, CxpDir])
    end.


getAppdataImmInfoForClasses(ImmInfo, CxpDir, AppdataAbsPath) ->
    Basename = getRequiredAttribute(file, ImmInfo, CxpDir),
    WildPath = getRequiredAttribute(path, ImmInfo, CxpDir),
    AbsPath =
	case filelib:wildcard(filename:join([CxpDir, WildPath, Basename])) of
	    [OnePath] ->
		OnePath;
	    [] ->
		?FAULT([error], "missing IMM classes file, path: ~p, file: ~p",
		       [WildPath, Basename]);
	    Several ->
		?FAULT([error], "multiple files match: path: ~s, file: ~s, matching: ~p",
		       [WildPath, Basename, Several])
	end,
    case getSubElements(schema, ImmInfo) of
	[Schema] ->
	    Schemaname = getRequiredAttribute(name, Schema, AbsPath),
	    Version = getRequiredNonNegAttribute(version, Schema, AbsPath),
	    Release = getRequiredNonNegAttribute(release, Schema, AbsPath),
	    Correction = getNonNegAttribute(correction, Schema, AbsPath),
	    FromVersions = getFromVersions(Schema, AbsPath),
	    createImmInfo(Basename, WildPath, AbsPath, AppdataAbsPath,
			  Schemaname, Version, Release, Correction,
			  FromVersions);
	[_|_] ->
	    ?FAULT([error], "multiple schema elements in: ~s", AbsPath);
	[] ->
	    case getAttribute(schemaName, ImmInfo) of
		undefined ->
		    createImmInfo(unnamedSchema, Basename, WildPath, AbsPath, AppdataAbsPath);
		Schemaname ->
		    case isValidSchemaName(Schemaname) of
			false ->
			    ?FAULT([error], "empty schema name in: ~s", [AbsPath]), ok;
			true ->
			    Version = getRequiredNonNegAttribute(version, ImmInfo, AbsPath),
			    Release = getRequiredNonNegAttribute(release, ImmInfo, AbsPath),
			    Correction = getNonNegAttribute(correction, ImmInfo, AbsPath),
			    FromVersions = getFromVersions(ImmInfo, AbsPath),
			    createImmInfo(Basename, WildPath, AbsPath, AppdataAbsPath,
					  Schemaname, Version, Release, Correction,
					  FromVersions)
		    end
	    end
    end.


createImmInfo(Type, Basename, WildPath, AbsPath, AppDataAbsPath) ->
    #immInfo{type=Type,
	     file=Basename,
	     path=WildPath,
	     abspath=AbsPath,
	     cat=AppDataAbsPath}.


createImmInfo(Basename, WildPath, AbsPath, AppdataAbsPath, Schemaname, Version, Release, Correction, FromVersions) ->
    #immInfo{type=namedSchema,
	     file=Basename,
	     path=WildPath,
	     abspath=AbsPath,
	     schemaName=Schemaname,
	     version=[Version, Release, Correction],
	     fromVersions=
		 [if
		      R =:= undefined ->
			  [V];
		      true ->
			  if
			      C =:= undefined ->
				  [V, R];
			      true ->
				  [V, R, C]
			  end
		  end
		  || #fromVersion{version=V, release=R, correction=C} <- FromVersions],
	     cat=AppdataAbsPath}.


getAppdataImmInfoForObjects(ImmInfo, CxpDir, AppdataAbsPath) ->
    Basename = getRequiredAttribute(file, ImmInfo, CxpDir),
    WildPath = getRequiredAttribute(path, ImmInfo, CxpDir),
    AbsPath =
	case filelib:wildcard(filename:join([CxpDir, WildPath, Basename])) of
	    [OnePath] ->
		OnePath;
	    [] ->
		?FAULT([error], "missing IMM objects file, path: ~p, file: ~p",
		       [WildPath, Basename]);
	    Several ->
		?FAULT([error], "multiple files match: path: ~s, file: ~s, matching: ~p",
		       [WildPath, Basename, Several])
	end,
    case getSubElements(schema, ImmInfo) of
	[] ->
	    ok;
	_ ->
	    ?FAULT([user],
		   "ignoring misplaced <schema> elements in <immInfo> referring to: ~s",
		   [AbsPath])
    end,
    createImmInfo(objects, Basename, WildPath, AbsPath, AppdataAbsPath).


%% @doc Get fromversion descriptors from the given
%% ImmInfo element.

-spec getFromVersions(#xmlElement{}, string()) -> [#fromVersion{}].

getFromVersions(ContainingElement, Context) ->
    FVS = getOneSubElement(fromVersions, ContainingElement, Context),
    [begin
	 V =  getRequiredNonNegAttribute(version, FV, Context),
	 case getAttribute(release, FV) of
	     undefined ->
		 case getAttribute(correction, FV) of
		     undefined ->
			 #fromVersion{version=V};
		     _ ->
			 ?FAULT([error], "missing attribute: 'release', context: ~s", [Context])
		 end;
	     _ ->
		 case getAttribute(correction, FV) of
		     undefined ->
			 #fromVersion{version=V,
				      release=getRequiredNonNegAttribute(
						release, FV, Context)};
		     _ ->
			 #fromVersion{version=V,
				      release=getRequiredNonNegAttribute(
						release, FV, Context),
				      correction=getRequiredNonNegAttribute(
						   correction, FV, Context)}
		 end
	 end
     end
     || FV <- getSubElements(fromVersion, FVS)].


%% doc Gets the CXPs from a CXS xml.

%% -spec getCxps(string()) -> [#cxp{}].

%% getCxps(CxsPathname) ->
%%     TopElement = getTopElement(configuration, CxsPathname),
%%     ContentInfo = getOneSubElement(contentinfo, TopElement, CxsPathname),
%%     Products = getSubElements(product, ContentInfo),
%%     SourceDir = filename:dirname(CxsPathname),
%%     Bundles = getBundles(SourceDir),
%%     getCxps(CxsPathname, Bundles, Products).

%% getCxps(CxsPathname, Bundles, [P|Products]) ->
%%     CxpName = getRequiredAttribute(name, P, CxsPathname),
%%     CxpId = getRequiredAttribute(id, P, CxsPathname),
%%     CxpVer = getRequiredAttribute(version, P, CxsPathname),
%%     case lists:keyfind({CxpName, CxpId, CxpVer}, 1, Bundles) of
%% 	false -> % No bundle
%% 	    CxpDir = CxpName++"_"++CxpId++"_"++CxpVer,
%% 	    [#cxp{name=CxpName,
%% 		  id=CxpId,
%% 		  ver=CxpVer,
%% 		  dirname=CxpDir}|getCxps(CxsPathname, Bundles, Products)];
%% 	{Key, TopElement} ->
%% 	    CxpDir = CxpName++"_"++CxpId++"_"++CxpVer,
%% 	    ContentInfo = getOneSubElement(contentinfo, TopElement, CxpDir),
%% 	    BundleCxps = getSubElements(product, ContentInfo),
%% 	    NewBundles = lists:keydelete(Key, 1, Bundles),
%% 	    getCxps(CxsPathname, NewBundles, BundleCxps++Products)
%%     end;
%% getCxps(_, _, []) ->
%%     [].

%% getBundles(SourceDir) ->
%%     %% It is assumed that all cxp*.xml files found here belong to
%%     %% bundles, and that SWM will keep this correct
%%     %% Thus, no filtering of bundles is necessary
%%     Bundles = filelib:wildcard(filename:join(SourceDir, "cxp*.xml")),
%%     [begin
%% 	 TopElement = getTopElement(configuration, Bundle),
%% 	 Product = getOneSubElement(product, TopElement, Bundle),
%% 	 {{getRequiredAttribute(name, Product, Bundle),
%% 	   getRequiredAttribute(id, Product, Bundle),
%% 	   getRequiredAttribute(version, Product, Bundle)}, TopElement}
%%      end||Bundle<-Bundles].


%% doc Gets the CXCs from a CXP XML.

%% -spec getCxcs(string()) -> [#cxc{}].

%% getCxcs(CxpPathname) ->
%%     TopElement = getTopElement(configuration, CxpPathname),
%%     ContentInfo = getOneSubElement(contentinfo, TopElement, CxpPathname),
%%     Products = getSubElements(product, ContentInfo),
%%     [#cxc{name=getAttribute(name, P),
%% 	  id=getRequiredAttribute(id, P, CxpPathname),
%% 	  ver=getRequiredAttribute(version, P, CxpPathname),
%% 	  appdatas=getAppdatas(P, CxpPathname)}
%%     || P <- Products].


%% Gets the appdata descriptors from the given
%% product element.

%% -spec getAppdatas(#xmlElement{}, string()) -> [#appdata{}].

%% getAppdatas(ProductElement, CxpPathname) ->
%%     [#appdata{relpath=getRequiredAttribute(relpath, A, CxpPathname),
%% 	      file=getAttribute(file, A)}
%%     || A <- getSubElements(appdata, ProductElement)].



%% -spec getUniqueFile(string(), atom()) -> string() | none.

%% getUniqueFile(Pattern, Mode) ->
%%     case filelib:wildcard(Pattern) of
%% 	[] when Mode =:= required ->
%% 	    ?FAULT([error], "no file matches pattern: ~s", [Pattern]);
%% 	[] ->
%% 	    none;
%% 	[Result] ->
%% 	    Result;
%% 	Multiple ->
%% 	    ?FAULT([error], "multiple files match pattern: ~s, files: ~p", [Pattern, Multiple])
%%     end.



%% @doc Gets the top element from the given XML file and
%% verifies its name.

-spec getTopElement(atom(), string()) ->  #xmlElement{}.

getTopElement(Name, Pathname) ->
    case filelib:is_file(Pathname) of
	false ->
	    ?FAULT([error], "missing file: ~s", [Pathname]);
	_ ->
	    case xmerl_scan:file(Pathname) of
		{#xmlElement{name=Name}=Result, []} ->
		    Result;
		{#xmlElement{name=Other}, []} ->
		    ?FAULT([error], "file: ~s, top element is: ~s, must be: ~s", [Pathname, Other, Name]);
		{#xmlElement{}, Junk} ->
		    ?FAULT([error], "file: ~s, top element followed by garbage: ~p", [Pathname, Junk]);
		_ ->
		    ?FAULT([error], "file: ~s, could not parse", [Pathname])
	    end
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


%% @doc Returns the value of a required attribute. This function is
%% called from gmfAppData, gmfAppCli and gmfImmUgVerifyUpgrade.
%% @end

getRequiredAttribute(Name, #xmlElement{attributes=AA}, Context) ->
        case lists:keyfind(Name, #xmlAttribute.name, AA) of
	false ->
	    ?FAULT([error], "missing attribute: ~w, context: ~s", [Name, Context]);
	#xmlAttribute{value=Value} ->
	    Value
    end.


-spec getRequiredNonNegAttribute(atom(), #xmlElement{}, string()) -> integer().

getRequiredNonNegAttribute(Name, #xmlElement{attributes=AA}, Context) ->
        case lists:keyfind(Name, #xmlAttribute.name, AA) of
	false ->
	    ?FAULT([error], "missing attribute: ~w, context: ~s", [Name, Context]);
	#xmlAttribute{value=Value} ->
	    case isValidNonNegNumeral(Value) of
		false ->
		   ?FAULT([error], "ill-formed numeric attribute: ~w, value: ~s, context: ~s", [Name, Value, Context]);
		true ->
		    list_to_integer(Value)
	    end
    end.


%% @doc Gets a non-negative attribute, or 0 if the attribute is
%% not present.

-spec getNonNegAttribute(atom(), #xmlElement{}, string()) -> integer().

getNonNegAttribute(Name, #xmlElement{attributes=AA}, Context) ->
        case lists:keyfind(Name, #xmlAttribute.name, AA) of
	false ->
	    0;
	#xmlAttribute{value=Value} ->
	    case isValidNonNegNumeral(Value) of
		false ->
		   ?FAULT([error], "ill-formed numeric attribute: ~w, value: ~s, context: ~s", [Name, Value, Context]);
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

-spec getOneSubElement(atom(), #xmlElement{}, string()) -> #xmlElement{}.

getOneSubElement(Name, #xmlElement{content=Content}, Context) ->
    case [S || #xmlElement{name=N}=S <- Content, N =:= Name] of
	[Result] ->
	    Result;
	_ ->
	    ?FAULT([error], "wrong number of ~w elements, context: ~s", [Name, Context])
    end.


%% @doc Returns true if the given schema name is valid.
%% Any non-empty name is considered valid.

isValidSchemaName("") ->
    false;

isValidSchemaName(_) ->
    true.


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
