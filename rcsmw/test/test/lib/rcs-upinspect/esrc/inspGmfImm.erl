%%% ----------------------------------------------------------
%%% %CCaseFile:	inspGmfImm.erl %
%%% Author:	erarafo
%%% Description: Inspects 'gmfImm' appdata.
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(inspGmfImm).
%-behaviour(behaviour).
-id('Updated by CCase').
-vsn('/main/R5A/R6A/2').
-date('2016-04-28').
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
%%% R5A/1      2016-02-15 erarafo     First version
%%% R5A/2      2016-02-18 erarafo     Suppress zeroes in output
%%% R5A/4      2016-02-24 erarafo     Fixed fault in summary/0
%%% R6A/1      2016-04-28 erarafo     Support for cross-validation
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc ...
%%% @end
%%% ----------------------------------------------------------

-export([inspect/2, summary/0]).

-import(upLib, [err/2,
		warn/2,
		info/2,
		progress/1,
		valDictBind/3,
		valDictGetAll/1,
		getRequiredAttribute/2,
		getAttribute/2,
		getAttribute/3,
		getOneSubElement/2,
		getSubElements/2,
		getContainedText/1]).

-include("upInspect.hrl").


-record(schemaInfo,
	{basename=""       :: string(),               % basename of file
	 resolvedPath=""   :: string(),               % resolved path
	 name              :: string()|undefined,
	 version=0         :: integer(),
	 release=0         :: integer(),
	 correction=0      :: integer(),
	 fromVersions=[]   :: [{integer(), integer(), integer()}],
	 cxpDir=""         :: string()
	}).
	 

inspect(Appdata,
	#inspContext{cxpInfo=#cxpInfo{dir=CxpDir}}=Context) ->
    
    % TODO: Also check unique existence and readability for fileType =:= "objects"

    % TODO: Find out how RCS handles the case where the relpath is ambiguous
    % but the relpath+basename is unique. Accepted, or silent failure, or logged
    % failure?
    
    try
	case getSubElements(immInfos, Appdata) of
	    [] ->
		ok;
	    [ImmInfos] ->
		ImmInfoList = getSubElements(immInfo, ImmInfos),
		lists:foreach(
		  fun(ImmInfo) ->
			  FileType = getRequiredAttribute(fileType, ImmInfo),
			  BaseName = getRequiredAttribute(file, ImmInfo),
			  Path = getRequiredAttribute(path, ImmInfo),
			  % verify unique file existence
			  TotalPath = upLib:fileIsReadable(CxpDir, Path, BaseName),
			  ResolvedPath = filename:join(tl(filename:split(TotalPath))),
			  if
			      FileType =:= "objects" ->
				  ok;
			      FileType =:= "classes" ->
				  getSchemaInfo(ImmInfo, CxpDir, ResolvedPath, BaseName)
			  end,
			  inspectSchema(TotalPath, Context)
		  end,
		  ImmInfoList)
	end
    catch
	throw:{not_classes} ->
	    ok;
	throw:{no_such_dir, PartialPath, BaseName} ->
	    err("no such directory: ~s, when trying to find: ~s", 
		[PartialPath, BaseName]);
	throw:{ambig_dir, PartialPath, BaseName, Dirs} ->
	    err("ambiguous directory: ~s, when trying to find: ~s, expansions: ~p", 
		[PartialPath, BaseName, Dirs]);
	throw:{cannot_read, {TotalPath, Reason}} ->
	    err("cannot read: ~s, reason: ~p", 
		[TotalPath, Reason])
    end.


summary() ->
    progress("summary of gmfImm info"),
    orddict:fold(
      fun(_Key, #schemaInfo{basename=BaseName,
			    name=undefined,
			    resolvedPath=RP,  
			    cxpDir=CxpDir}, Acc) ->
	      info("  unnamed: ~s (~s)", [BaseName, filename:join(CxpDir, RP)]),
	      Acc;
	 (_Key, #schemaInfo{name=Schema,
			    version=Version,
			    release=Release,
			    correction=0,
			    fromVersions=FVV,
			    resolvedPath=RP,  
			    cxpDir=CxpDir}, Acc) ->
	      info("  schema: ~s ~w.~w, fvs: ~s (~s)", 
		   [Schema, Version, Release, 
		    fromVersionsToString(FVV), 
		    filename:join(CxpDir, RP)]),
	      Acc;
	 (_Key, #schemaInfo{name=Schema,
			    version=Version,
			    release=Release,
			    correction=Corr,
			    fromVersions=FVV,
			    resolvedPath=RP,  
			    cxpDir=CxpDir}, Acc) ->
	      info("  schema: ~s ~w.~w.~w, fvs: ~s (~s)", 
		   [Schema, Version, Release, Corr, 
		    fromVersionsToString(FVV), 
		    filename:join(CxpDir, RP)]),
	      Acc
      end,
      [],
      valDictGetAll(immSchemas)),
    
    orddict:fold(
      fun(_K, #immClass{name=Name, rdnName=RdnName}, _) ->
	      info("IMM class: ~s, RDN name: ~s", [Name, RdnName])
      end,
      [],
      valDictGetAll(immClasses)),
    
    ok.





%%% ----------------------------------------------------------
%%% @doc ...
%%% @end
%%% ----------------------------------------------------------
getSchemaInfo(ImmInfo, CxpDir, ResolvedPath, BaseName) ->
    case getSubElements(schema, ImmInfo) of
	[] ->
	    % info("unnamed schema: ~s", [BaseName]),
	    valDictBind(immSchemas, {CxpDir, BaseName, ResolvedPath}, 
			#schemaInfo{resolvedPath=ResolvedPath, 
				    basename=BaseName,
				    cxpDir=CxpDir});
	[Schema] ->
	    Sname = getRequiredAttribute(name, Schema),
	    Sver = getRequiredAttribute(version, Schema),
	    Srel = getRequiredAttribute(release, Schema),
	    Scorr = getAttribute(correction, Schema, "0"),
	    FromVersions = getOneSubElement(fromVersions, Schema),
	    FromVersionList = getSubElements(fromVersion, FromVersions),
	    Fvers =
		lists:foldr(
		  fun(FromVersion, A2) ->
			  Fver = list_to_integer(getRequiredAttribute(version, FromVersion)),
			  Frel = list_to_integer(getAttribute(release, FromVersion, "0")),
			  Fcorr = list_to_integer(getAttribute(correction, FromVersion, "0")),
			  
			  A2++[{Fver, Frel, Fcorr}]
		  end,
		  [],
		  FromVersionList),
	    
	    % info("named schema: ~p", [{Sname, [Sver, Srel, Scorr], Fvers}]),
	    valDictBind(immSchemas, {CxpDir, BaseName, ResolvedPath}, 
			#schemaInfo{resolvedPath=ResolvedPath, 
				    basename=BaseName,
				    name=Sname,
				    version=list_to_integer(Sver),
				    release=list_to_integer(Srel),
				    correction=list_to_integer(Scorr),
				    fromVersions=Fvers,
				    cxpDir=CxpDir})
    end.


fromVersionsToString(FVV) ->
    string:join(
      lists:map(
	fun({A, 0, 0}) ->
		lists:flatten(io_lib:format("~w", [A]));
	   ({A, B, 0}) ->
		lists:flatten(io_lib:format("~w.~w", [A, B]));
	   ({A, B, C}) ->
		lists:flatten(io_lib:format("~w.~w.~w", [A, B, C]))
	end,
	FVV),
      ", ").

inspectSchema(TotalPath, _Context) ->
    
    Top = upLib:getTopElement(TotalPath),
    % info("top: ~p", [Top]),

    Classes = getSubElements(class, Top),
    lists:foreach(
      fun(Class) ->
	      ClassName = getAttribute(name, Class),
	      
	      Rdn =
		  case getSubElements(rdn, Class) of
		      [] ->
			  throw({no_rdn, ClassName});
		      [_, _|_] ->
			  throw({multi_rdn, ClassName});
		      [A] ->
			  A
		  end,
	      [NameElement] = getSubElements(name, Rdn), 
	      
	      RdnNameS = getContainedText(NameElement),

	      
	      
	      valDictBind(immClasses, ClassName, #immClass{name=ClassName, rdnName=RdnNameS})
      end,
      Classes),
    ok.
