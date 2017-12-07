%%% ----------------------------------------------------------
%%% %CCaseFile:	upInspect.erl %
%%% Author:	erarafo
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(upInspect).
%-behaviour(behaviour).
-id('Updated by CCase').
-vsn('/main/R5A/R6A/R8A/6').
-date('2017-01-16').
-author('erarafo').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/3 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2016-2017 All rights reserved.
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
%%% R5A/1      2016-02-11 erarafo     First version.
%%% R5A/2      2016-02-12 erarafo     Added reporting of OTP version
%%% R5A/3      2016-02-15 erarafo     Steps towards an "inspector" behaviour
%%% R5A/4      2016-02-15 erarafo     Reporting schemas for which validation fails
%%% R5A/5      2016-02-18 erarafo     Refactoring of schema validation
%%% R5A/6      2016-02-19 erarafo     Restructuring
%%% R5A/7      2016-02-22 erarafo     Reporting alarm type filenames
%%% R5A/8      2016-02-24 erarafo     Refactoring
%%% R5A/9      2016-02-25 erarafo     Deprecation warnings
%%% R6A/1      2016-04-27 erarafo     gmfMim inspection
%%% R6A/2      2016-04-28 erarafo     Mim/imm cross-validation
%%% R6A/3      2016-05-22 erarafo     Report classes and structs per MOM
%%% R6A/4      2016-06-01 erarafo     Better diagnostic if metadata mismatch
%%% R8A/1      2016-11-16 erarafo     Better diagnostic of broken UP CXP9024418_6-R1B04
%%% R8A/2      2016-11-29 erarafo     Handle cases of metadata mismatch gracefully
%%% R8A/3      2016-11-29 erarafo     Eliminate false warning with SIM UP
%%% R8A/4      2016-12-01 erarafo     Support for HW/SW compatibility
%%% R8A/5      2016-12-02 erarafo     Cleanup
%%% R8A/6      2017-01-16 erarafo     VRCS support
%%% ----------------------------------------------------------

-export([start/1]).

-import(upLib, [declare/2,
		declare/3,
		valDictBind/3,
		valDictFromList/2,
		valDictGet/2,
		valDictGet/3,
		valDictGetAll/1,
		varDictBind/3,
		varDictStore/3,
		varDictDelete/2,
		varDictGet/2,
		varDictGet/3,
		varDictGetAll/1,
		setAddElement/2,
		setMember/2,
		setGetAll/1,
		valBind/2,
		valGet/1,
		sevUpdate/2,
		sevGet/1,
		counterIncr/1,
		counterGet/1,
		
		stringToBoolean/1,

		getTopElement/1,
		getOneSubElement/2,
		getRequiredAttribute/2,
		getAttribute/2,
		getAttribute/3,
		getSubElements/2,
		getContainedText/1,
		
		getTargetHandler/1,
		schemaValidate/2,
		schemaValidate/3,
		findSchema/1,
		x/2,

		strip/1,
		progress/1,
		info/2,
		warn/2,
		err/2
	       ]).

-include("upInspect.hrl").




start([Keep, UP, Wdir, DownloadedPackage|_]=Args) ->
    declare(status, severity, ok),
    declare(errorCount, counter, 0),
    declare(warningCount, counter, 0),
    declare(deprCount, counter, 0),
    try
	{Micros, _Result} = timer:tc(fun run/1, [Args]),
	info("execution took: ~w s", [Micros div 1000000])
    catch
	throw:{terminate, Reason} ->
	    info("terminating, reason: ~p", [Reason]);
	throw:hw_category_undefined ->
	    ok;
	X:Y ->
	    Stack = erlang:get_stacktrace(),
	    info("caught: ~p~nstack: ~p", [{X, Y}, Stack]),
	    sevUpdate(status, fatal)
    after
	remove(stringToBoolean(DownloadedPackage), UP),
	FinalSeverity = sevGet(status),
	info("errors: ~w, warnings: ~w, use of deprecated features: ~w", 
	     [counterGet(errorCount), counterGet(warningCount), counterGet(deprCount)]),
	info("highest severity was: ~w", [FinalSeverity]),
	ExitCode = exitCode(FinalSeverity),
	info("exiting with code: ~w", [ExitCode]),
	case stringToBoolean(Keep) of
	    true ->
		info("working directory not removed: ~s", [Wdir]),
		ok;
	    false ->
		info("removing working directory: ~s", [Wdir]),
		x("rm", ["-rf", Wdir])
	end,
	init:stop(ExitCode)
    end.


-spec exitCode(severity()) -> non_neg_integer().

exitCode(ok) -> 0;
exitCode(deprec) -> 0;
exitCode(warning) -> 0;
exitCode(error) -> 1;
exitCode(fatal) -> 2.


-spec run([string()]) -> any().

run([_Keep,
     UP,
     Wdir,
     DownloadedPackage,
     RcsTop,
     TargetsArg,
     ValidTargets,
     InspAlarmsVerbose,
     DeprecationWarnings,
     ExtendedMomAnalysis,
     HwCategory,
     HwModel,
     UpType,
     SchemaValidationStrict]) ->
    info("upgrade package: ~s", [UP]),
    info("working directory: ~s", [Wdir]),
    Targets =
	if
	    TargetsArg =:= "none" ->
		[];
	    TargetsArg =:= "all" ->
		string:tokens(ValidTargets, ":");
	    true ->
		string:tokens(TargetsArg, ",")
	end,

    declare(args, valDict, [{rcsTop, RcsTop}]),
    
    declare(inspectTargets, set, Targets),
    if 
	Targets =/= [] ->
	    info("inspect appdata for: ~p",
		 [ordsets:to_list(setGetAll(inspectTargets))]);
	true -> ok
    end,

    declare(cxps, varDict),
    declare(targets, set),
    declare(schemas, val),
    declare(alarmTypes, valDict),
    declare(alarmVariants, varDict),
    declare(appdataTypesNotValidated, set),
    declare(schemasWithFailures, set),
    declare(programs, valDict),
    declare(lmlistLmrefs, valDict),
    declare(programgroupLmrefs, valDict),

    declare(immSchemas, valDict),

    declare(validationFailed, counter, 0),
    declare(validationCount, counter, 0),
    
    declare(inspOpts, valDict),
    
    declare(mims, valDict),
    declare(immClasses, valDict),
    
    declare(deprecationWarnings, val, stringToBoolean(DeprecationWarnings)),
    valDictBind(inspOpts, inspAlarmsVerbose, stringToBoolean(InspAlarmsVerbose)),
    
    declare(extendedMomAnalysis, val, ExtendedMomAnalysis),
    
    declare(schemaStrict, val, stringToBoolean(SchemaValidationStrict)),
    
    initTargetHandlers(UpType),

    setCwd(Wdir),

    PackageFormat = packageFormat(UP),
    info("package format: ~p", [PackageFormat]),

    progress("unpack UP"),
    if
	PackageFormat =:= tgz ->
	    case x("tar", ["-xzf", UP]) of
		#osResult{code=Code}=R when Code =/= 0 ->
		    throw({untar, R});
		_ ->
		    info("UP unpacked", []),
		    ok
	    end;
	PackageFormat =:= zip ->
	    case x("unzip", ["-q", UP]) of
		#osResult{code=Code}=R when Code =/= 0 ->
		    throw({unzip, R});
		_ ->
		    info("UP unpacked", []),
		    ok
	    end;
	true ->
	    throw({package_format, PackageFormat})
    end,
    remove(DownloadedPackage, UP),

    % Expect one *-up.xml
    MetaDataTop =
	case filelib:wildcard("*-up.xml") of
	    [] ->
		throw({metadata_missing, top});
	    [_, _|_] ->
		throw({metadata_ambiguous, top});
	    [MetaDataTopFile] ->
		MetaDataTopFile
	end,
    info("found top-level metadata: ~p", [MetaDataTop]),
    progress("validating top-level metadata"),

    SchemaStrict = valGet(schemaStrict),
    case schemaValidate(MetaDataTop, "xsd:up", true) of
	#xsdResult{libXml2Code=0} ->
	    ok;
	#xsdResult{libXml2Code=MetaDataTopCode, text=MetaDataTopText} when not(SchemaStrict) ->
	    #targetHandler{schemaPath=MetaDataTopSchema} = getTargetHandler("xsd:up"),
	    setAddElement(MetaDataTopSchema, schemasWithFailures),
	    warn(
	      "top-level metadata validation failure, code: ~p, schema: ~s, text: ~s",
	      [MetaDataTopCode, MetaDataTopSchema, MetaDataTopText]);
	_ ->
	    throw({terminate, up_schema_validation_failure})
    end,

    XmlEle = getTopElement(MetaDataTop),
    Product = getOneSubElement(product, XmlEle),
    ProductId = getRequiredAttribute(id, Product),
    ProductVersion = getRequiredAttribute(version, Product),
    ProductName = getAttribute(name, Product),
    % Filename = getAttribute(filename, Product),
    info("product name: ~p, id: ~s, version: ~s", [ProductName, ProductId, ProductVersion]),

    showDescription(XmlEle),
    
    ProductType = getProductType(XmlEle),
    info("UP product type is: ~s", [ProductType]),
    
    HwSwCompat = getHwSwCompatibility(XmlEle),
    if
	HwSwCompat =:= undefined ->
	    % use productinfo in this case
	    info("HW-SW compatibility is not encoded in this UP", []),
	    #boardList{products=Products,
		       hwcategory=HwCat,
		       hwmodel=HwMod} = getBoardListFromContentInfo(XmlEle),
	    run(ProductType, HwCat, HwMod, Products);
	true ->
	    info("HW-SW compatibility, index: ~s", [HwSwCompat]),
	    BoardLists = getBoardLists(XmlEle),
	    if
		HwCategory =:= "none" ->
		    info("board lists are -", []),
		    orddict:fold(
		      fun({C, M}, _V, _) ->
			info("category: ~s, model: ~s", [C, M])
		      end,
		      none,
		      BoardLists),
		    info("consider re-run with -c option", []),
		    throw({terminate, stop});
		true ->
		    info(
		      "inspection assuming HW category: ~s, model: ~s",
		      [HwCategory, HwModel]),
		    case orddict:find({HwCategory, HwModel}, BoardLists) of
			error ->
			    throw({terminate, "unknown category or model"});
			{ok, #boardList{products=Products}} ->
			    run(ProductType, HwCategory, HwModel, Products)
		    end
			
	    end
    end.


-spec run(string(), string(), string(), [#product{}]) -> any().

run(ProductType, HwCat, HwMod, ProductsPreliminary) ->
    
    % ugly special case: no filenames in Products list, get the info from
    % inventory of .cxp files then
    FileNamesMissingAll =
	lists:all(
	  fun(#product{file=File}) -> File =:= "" end,
	  ProductsPreliminary),
    
    Products = 
	if
	    FileNamesMissingAll ->
		getProductsFromTarfiles(ProductsPreliminary);
	    true ->
		ProductsPreliminary
	end,    

    if
	HwCat =:= "N/A" ->
	    info("number of CXPs: ~w", [orddict:size(Products)]);
	true ->
	    info("~s/~s: number of CXPs: ~w", [HwCat, HwMod, orddict:size(Products)])
    end,
    
    CxpDirsAndInfo =
	lists:foldl(
	  fun(#product{name=CxpName, id=CxpId, ver=CxpVer, file=FileName}, Acc) ->
		  Dir = makeCxpDirName(CxpName, CxpId, CxpVer),
		  #cxpInfo{topElement=Top} = unpackCxpTgz(FileName),
		  CxpInfo = #cxpInfo{name=CxpName,
				  id=CxpId,
				  ver=CxpVer, 
				  filename=FileName,
				  dir=Dir,
				  topElement=Top},
		  Acc++[{Dir, CxpInfo}]
	  end,
	  [],
	  Products),

    progress("validate CXP metadata"),
    lists:foreach(
      fun({CxpDir, _CxpInfo}) ->
	      inspCxp(CxpDir)
      end,
      CxpDirsAndInfo),

    progress("dissolve bundles (if any)"),
    orddict:fold(
      fun(CxpIdVer, CxpInfo, _) ->
	      dissolveBundle(CxpIdVer, CxpInfo)
      end,
      none,
      varDictGetAll(cxps)),
    
    progress("check for 'os' CXP"),

    NumberOfOs =
	orddict:fold(
	  fun(_, #cxpInfo{topElement=Top, name=N, id=I, ver=V}, Acc) ->
		  case getSubElements(os, Top) of
		      [] ->
			  Acc;
		      [_] ->
			  info("marked as os: ~s (~s ~s)", [N, I, V]),
			  Acc + 1;
		      [_|_] ->
			  err("multiple 'os' elements: ~s (~s ~s)", [N, I, V]),
			  Acc + 1
		  end
	  end,
	  0,
	  varDictGetAll(cxps)),

    IsSimulator = ProductType =:= "RCP-SIM" orelse ProductType =:= "RCSSIM",
    if 
	IsSimulator ->
	    ok;
	NumberOfOs =:= 0 ->
	    warn("no CXP has the 'os' element; product type: ~s", [ProductType]);
	NumberOfOs > 1 ->
	    err("multiple CXPs have the 'os' element", []);
	true ->
	    ok
    end,
	    
    progress("unpack .ar files"),
    Weird =
	orddict:fold(
	  fun(IdVer, #cxpInfo{dir=Dir}, Acc) ->
		  setCwd(Dir),
		  case filelib:wildcard("*.ar") of
		      [] ->
			  setCwd(".."),
			  Acc++[{IdVer, missing_ar_file}];
		      [_, _|_]=ArFiles ->
			  setCwd(".."),
			  Acc++[{IdVer, {ambig_ar_files, ArFiles}}];
		      [ArFile] ->
			  x("ar", ["-x", ArFile]),
			  x("rm", [ArFile]),
			  setCwd(".."),
			  Acc
		  end
	  end,
	  [],
	  varDictGetAll(cxps)),

    if
	Weird =/= [] ->
	    info("expanded .ar except in: ~p", [Weird]);
	true ->
	    info("expanded .ar everywhere", [])
    end,

    progress("unsquash"),
    orddict:fold(
      fun(IdVer, #cxpInfo{dir=Dir}, Acc) ->
	      #osResult{code=Code14} = x("unsquashfs", ["-n", filename:join(Dir, "sqfs.img")]),
	      SQFR = "squashfs-root",
	      if
		  Code14 =/= 0 ->
		      info("failed to unsquash, ~p", [IdVer]),
		      Acc;
		  true ->
		      Names =
			  case file:list_dir(SQFR) of
			      {error, ListDirReason} ->
				  throw({failed_to_list_dir, {Dir, SQFR, ListDirReason}});
			      {ok, NN} ->
				  NN
			  end,

		      % move files into the CXP directory
		      lists:foreach(
			fun(Name) ->
				case file:read_file_info(filename:join(Dir, Name)) of
				    {ok, _FileInfo} ->
					% no overwrite!
					ok;
				    {error, enoent} ->
					% move!
					file:rename(filename:join(SQFR, Name),
						    filename:join(Dir, Name))
				end
			end,
			Names),
		      #osResult{code=Code141} = x("rm", ["-rf", SQFR]),
		      if
			  Code141 =/= 0 ->
			      throw({removal_failed, IdVer});
			  true ->
			      ok
		      end
	      end,
	      Acc
      end,
      [],
      varDictGetAll(cxps)),

    progress("determine OTP version"),
    determineOtpVersion(),

    progress("inspect Erlang applications"),
    inspErlangApps(),

    progress("inspect appdata"),
    _AppdataResult =
	orddict:fold(
	  fun(_IdVer, #cxpInfo{}=Cxp, Acc) ->
		  _R678 = inspAppdata(Cxp),
		  Acc
	  end,
	  [],
	  varDictGetAll(cxps)),

    info("appdata types in this UP are:~n  ~p", [ordsets:to_list(setGetAll(targets))]),

    ordsets:fold(
      fun(Target, Acc) ->
	      case getTargetHandler(Target) of
		  #targetHandler{summarizer=undefined} ->
		      ok;
		  #targetHandler{summarizer=S} ->
		      apply(S, [])
	      end,
	      Acc
      end,
      [],
      setGetAll(inspectTargets)),

%%     progress("summarize registered programs"),
%%     inspAppm:appmSummary(),
%%
%%     progress("report alarm type definition issues (if any)"),
%%     inspAlarm:alarmsSummary(),

    info("validation failures occurred for schemas: ~p",
	 [ordsets:to_list(setGetAll(schemasWithFailures))]),

    info("validations failed: ~w/~w", [counterGet(validationFailed), counterGet(validationCount)]),
    
    case ordsets:size(setGetAll(appdataTypesNotValidated)) of
	0 ->
	    info("XSD files were found for all appdata types", []);
	_ ->
	    info("appdata types not validated because no XSD available: ~199p",
		 [ordsets:to_list(setGetAll(appdataTypesNotValidated))])
    end.


-spec unpackCxpTgz(string()) -> #cxpInfo{}.

unpackCxpTgz(CxpTgz) ->
    TempDirName = "tmp",
    ok = file:make_dir(TempDirName),
    #osResult{} = x("tar", ["-xzf", CxpTgz, "-C", TempDirName]),
    info("expanded: ~p", [CxpTgz]),
    #cxpInfo{name=CxpName, id=CxpId, ver=CxpVersion}=Result =
							 getCxpDirName(TempDirName, true),
    CxpDirName = makeCxpDirName(CxpName, CxpId, CxpVersion),
    file:rename(TempDirName, CxpDirName),
    Result.


-spec getCxpDirName(string(), boolean()) -> #cxpInfo{}.

getCxpDirName(Dir, Store) ->
    MetadataFile =
	case filelib:wildcard(filename:join(Dir, "cxp*.xml")) of
	    [] ->
		throw({missing_metadata, Dir});
	    [_, _|_] ->
		throw({ambig_metadata, Dir});
	    [FileS] ->
		FileS
	end,
    Top = getTopElement(MetadataFile),
    Product = getOneSubElement(product, Top),
    CxpName = getRequiredAttribute(name, Product),
    CxpId = getRequiredAttribute(id, Product),
    CxpVersion = getRequiredAttribute(version, Product), 
    CxpInfo =
	#cxpInfo{name=CxpName,
		 id=CxpId,
		 ver=CxpVersion,
		 filename=filename:basename(MetadataFile),
		 dir=makeCxpDirName(CxpName, CxpId, CxpVersion),
		 topElement=Top},
    if
	Store ->
	    CxpIdVer = CxpId++":"++CxpVersion,
	    varDictStore(cxps, CxpIdVer, CxpInfo);
	true ->
	    ok
    end,
    CxpInfo.


-spec inspCxp(string()) -> any().

inspCxp(CxpDir) ->
    % at this point we know that there is exactly one parseable XML

    [Xml] = filelib:wildcard(filename:join(CxpDir, "cxp*.xml")),

    info("validating CXP metadata: ~s", [Xml]),
    % #targetHandler{schema=LmcSchema} = getTargetHandler("xsd:lmc"),
    
    #xsdResult{libXml2Code=0} = schemaValidate(Xml, "xsd:lmc"),
    
    SchemaStrict = valGet(schemaStrict),
    case schemaValidate(Xml, "xsd:lmc") of
	#xsdResult{libXml2Code=0} ->
	    ok;
	#xsdResult{libXml2Code=SchemaCode, text=SchemaText} when not(SchemaStrict) ->
	    #targetHandler{schemaPath=CxpSchema} = getTargetHandler("xsd:lmc"),
	    setAddElement(CxpSchema, schemasWithFailures),
	    warn(
	      "cxp metadata validation failure, code: ~p, schema: ~s, text: ~s",
	      [SchemaCode, CxpSchema, SchemaText]);
	_ ->
	    throw({schema_validation_failure, cxp})
    end.
	


%%% ----------------------------------------------------------
%%% @doc Dissolve a CXP if it turns out to be a bundle. No traces
%%% of the bundle will remain.
%%% @end
%%% ----------------------------------------------------------
-spec dissolveBundle(string(), #cxpInfo{}) -> any().

dissolveBundle(
  CxpIdVer, 
  #cxpInfo{topElement=BundleTop, dir=BundleCxpDir}=CxpInfo) ->
    case getSubElements(bundle, BundleTop) of
	[] ->
	    % info("not a bundle: ~p", [CxpDir]),
	    ok;
	Markers ->
	    if 
		length(Markers) > 1 ->
		    err("multiple bundle markers in CXP metadata", BundleCxpDir);
		true -> 
		    info("found bundle: ~s", [BundleCxpDir])
	    end,
	    info("dissolve bundle: ~s", [CxpIdVer]),
	    Cxps = filelib:wildcard(filename:join(BundleCxpDir,"*.cxp")),
	    info("bundled .cxp files are: ~p", [Cxps]),
	    ProdInfoActual =
		lists:foldl(
		  fun(CxpTgz, Acc) ->
			  file:make_dir("tmp"),
			  #osResult{} = x("tar", ["-xzf", CxpTgz, "-C", "tmp"]),
			  
			  [Xml] = filelib:wildcard("tmp/cxp*.xml"),
			  info("validating bundle CXP metadata: ~s", [Xml]),
			  % #targetHandler{schema=LmcSchema} = getTargetHandler("xsd:lmc"),
			  
			  #xsdResult{libXml2Code=0} = 
			      schemaValidate(Xml, "xsd:lmc"),
			  
			  Top = getTopElement(Xml),
			  Product = getOneSubElement(product, Top),
			  CxpName = getRequiredAttribute(name, Product),
			  CxpId = getRequiredAttribute(id, Product),
			  CxpVersion = getRequiredAttribute(version, Product),
			  
			  % info("?? CxpId: ~p, CxpVersion: ~p", [CxpId, CxpVersion]),
			  
			  CxpIdVerMember = CxpId++":"++CxpVersion,
			  
			  CxpDirMember = makeCxpDirName(CxpName, CxpId, CxpVersion),
			  
			  file:rename("tmp", CxpDirMember),
			  
			  varDictBind(
			    cxps,
			    CxpIdVerMember,
			    #cxpInfo{name=CxpName,
				     id=CxpId,
				     ver=CxpVersion,
				     dir=CxpDirMember,
				     filename=filename:basename(CxpTgz),
				     topElement=Top}),
			  
			  ordsets:add_element({CxpName, CxpId, CxpVersion}, Acc)
		  end,
		  ordsets:new(),
		  Cxps),
	    
	    % TODO check actual prod info against bundle xml
	    
	    ContentInfo = getOneSubElement(contentinfo, BundleTop),
	    
	    ProductsInBundle = getSubElements(product, ContentInfo),
	    
	    ProdInfoPromised =
		lists:foldl(
		  fun(ProductInBundle, Acc) ->
			  CxpName = getRequiredAttribute(name, ProductInBundle),
			  CxpId = getRequiredAttribute(id, ProductInBundle),
			  CxpVersion = getRequiredAttribute(version, ProductInBundle),
			  ordsets:add_element({CxpName, CxpId, CxpVersion}, Acc)
		  end,
		  ordsets:new(),
		  ProductsInBundle),
	    if
		ProdInfoActual =/= ProdInfoPromised ->
		    throw({inconsistent_bundle, {CxpInfo, ProdInfoPromised, ProdInfoActual}});
		true ->
		    ok
	    end,
	    x("rm", ["-rf", BundleCxpDir]),
	    varDictDelete(cxps, CxpIdVer)
    end.


inspAppdata(#cxpInfo{name=Name, id=Id, ver=Ver, topElement=Top, dir=CxpDir}=CxpInfo) ->
    info("cxp: ~s (~s ~s)", [Name, Id, Ver]),

    ContentInfo = getOneSubElement(contentinfo, Top),
    Products = getSubElements(product, ContentInfo),

    lists:foldl(
      fun(Product, Acc) ->
	      ProdName = getRequiredAttribute(name, Product),
	      ProdId = getRequiredAttribute(id, Product),
	      ProdVersion = getRequiredAttribute(version, Product),
	      info("  cxc: ~s (~s ~s)", [ProdName, ProdId, ProdVersion]),

	      Appdatas = getSubElements(appdata, Product),
	      lists:foreach(
		fun(Appdata) ->
			RelPath = getRequiredAttribute(relpath, Appdata),
			File = getRequiredAttribute(file, Appdata),
			if
			    File =:= undefined ->
				% TODO, is this allowed?
				info("---------- cannot handle: ~s", [RelPath]);
			    true ->
				% info("    appdata: ~s ~s", [RelPath, File]),
				AppdataDirs =
				    filelib:wildcard(
				      filename:join(CxpDir, RelPath)),
				case AppdataDirs of
				    [] ->
					warn("no dir: ~s/~s", [CxpDir, RelPath]);
				    [_, _|_] ->
					warn("ambig dir: ~s/~s", [CxpDir, RelPath]);
				    [ResolvedDir] ->
					case filelib:is_dir(ResolvedDir) of
					    false ->
						warn("not a dir: ~s", [ResolvedDir]);
					    true ->
						AppdataFile = filename:join(ResolvedDir, File),
						case filelib:is_file(AppdataFile) of
						    false ->
							warn("not file: ~s", [AppdataFile]);
						    true ->
							% info("    file exists: ~s", [AppdataFile]),

							CxcInfo = #cxcInfo{name=ProdName, id=ProdId, version=ProdVersion},
							inspAppdata(CxpInfo, CxcInfo, AppdataFile)
						end
					end
				end
			end
		end,
		Appdatas),
	      Acc
      end,
      [],
      Products),
    ok.


inspAppdata(CxpInfo, CxcInfo, AppdataFile) ->
    Top = getTopElement(AppdataFile),
    try getRequiredAttribute(target, Top) of
	Target ->
	    % info("    target: ~s", [Target]),
	    setAddElement(Target, targets),
	    
	    #targetHandler{schemaPath=SchemaPath, handler=Handler} = 
		getTargetHandler(Target),
	    if
		SchemaPath =:= undefined ->
		    setAddElement(Target, appdataTypesNotValidated),
		    info("    no validation for target: ~s", [Target]);
		true ->
		    RcsTop = valDictGet(args, rcsTop),
		    SchemaFile = filename:join(RcsTop, SchemaPath),
		    case file:open(SchemaFile, [read]) of
			{error, Reason} ->
			    % TODO, this is not possible since schemas are read at startup
			    err("failed to read schema file: ~s, for target: ~s, reason: ~p",
				[SchemaFile, Target, Reason]),
			    info("    no validation for target: ~s", [Target]);
			{ok, IoDevice} ->
			    file:close(IoDevice),
			    #xsdResult{libXml2Code=CL, xmerlXsdCode=CX} = 
				schemaValidate(AppdataFile, Target),
			    Code = max(CL, CX),
			    case Code of
				X when X =/= 0 ->
				    warn(" failed validation: ~s, target: ~s, schema: ~s",
					 [AppdataFile, Target, SchemaPath]),
				    setAddElement(SchemaPath, schemasWithFailures);
				0 ->
				    info("    validated: ~s (~s, ~s)",
					 [AppdataFile, Target, SchemaPath])
			    end
		    end
	    end,
	    
	    if
		Handler =:= undefined ->
		    ok;
		true ->
		    case setMember(Target, inspectTargets) of
			false ->
			    ok;
			true ->
			    Context = #inspContext{cxpInfo=CxpInfo, 
						   cxcInfo=CxcInfo, 
						   resolvedFile=AppdataFile,
						   options=valDictGetAll(inspOpts),
						   enabled=true},
			    apply(Handler, [Top, Context])
		    end
	    end
    catch 
	throw:{missing_attribute, _} ->
	    err("no 'target' specified in appdata file: ~s", [AppdataFile])
    end.


determineOtpVersion() ->
    try
	#osResult{stdout=StdOut} =
	    x("find", [".", "-maxdepth", "2", "-type", "d", "-name", "*CXC1733859_*"]),
	case determineOtpVersionHelper(StdOut) of
	    [] ->
		throw({no_otp_cxc});
	    [_, _|_]=Paths ->
		throw({ambiguous_otp_cxc, Paths});
	    [OtpCxcDir] ->
		#osResult{stdout=[OtpVersion]} =
		    x("find", [OtpCxcDir, "-type", "f",
			       "!", "-path", "*/tgt_i686_32/*",
			       "-name", "OTP_VERSION"]),
		{ok, Stream} = file:open(OtpVersion, [read]),
		OtpVersionString = string:strip(io:get_line(Stream, ""), right, $\n),
		file:close(Stream),
		info("OTP version: OTP ~s", [OtpVersionString])
	end
    catch
	throw:ExData ->
	    warn("failed to determine OTP version: ~p", [ExData]);
	ExType:ExData ->
	    warn("failed to determine OTP version: ~p", [{ExType, ExData}])
    end.


-spec determineOtpVersionHelper([string()]) -> [string()].

determineOtpVersionHelper(StdOut) ->
    
    lists:foldr(
      fun(S, Acc) ->
	      case re:run(S, "\\./RCSMW", [anchored, {capture, none}]) of
		  nomatch ->
		      case re:run(S, "\\./RCS-SIM", [anchored, {capture, none}]) of
			  nomatch ->
			      Acc;
			  match ->
			      [S]++Acc
		      end;
		  match ->
		      [S]++Acc
	      end
      end,
      [],
      StdOut).


inspErlangApps() ->
    orddict:fold(
      fun(_IdVer, #cxpInfo{topElement=Top, dir=Dir}, Acc) ->
	      SLCI = getOneSubElement(contentinfo, Top),
	      lists:foreach(
		fun(Product) ->
			SLCxcId = getRequiredAttribute(id, Product),
			% info("-- source labels, CXC: ~s", [SLCxcId]),
			#osResult{stdout=SLStdOut} =
			    x("find",
			      [Dir,
			       "-path", io_lib:format("*~s*", [SLCxcId]),
			       "!", "-path", "*/tgt_i686_32/*",
			       "-name", "*.app"]),
			% info("---- paths: ~p", [SLStdOut]),
			lists:foreach(
			  fun(SLPath) ->
				  try file:consult(SLPath) of
				      {ok, [{application, SLAppName, SLAppProps}]} ->
					  % info("---- app descr: ~p", [SLAppDescr]);
					  SLAppId = proplists:get_value(id, SLAppProps, "unknown"),
					  SLAppVsn = proplists:get_value(vsn, SLAppProps, "unknown"),
					  SLAppDescr = proplists:get_value(description, SLAppProps, "no description"),
					  info("    ~w, id: ~s, version: ~s, ~s",
					       [SLAppName, SLAppId, SLAppVsn, SLAppDescr]);
				      SLOther ->
					  warn(" unexpected: ~p", [SLOther])
				  catch
				      X:Y ->
					  warn("failed to parse Erlang application, ~p", [{X, {Y, Dir, SLCxcId}}])
				  end
			  end,
			  SLStdOut)
		end,
		getSubElements(product, SLCI)),
	      Acc
      end,
      [],
      varDictGetAll(cxps)).


%%% ----------------------------------------------------------
%%% @doc Determines the package format of the given file.
%%% @end
%%% ----------------------------------------------------------
-spec packageFormat(pathname()) -> tgz|zip|undefined.

packageFormat(File) ->
    case x("file", [File]) of
	#osResult{code=Code}=R when Code =/= 0 ->
	    throw({packageFormat, R});
	#osResult{stdout=StdOut} ->
	    case re:run(StdOut, "gzip compressed data", [{capture, none}]) of
		match ->
		    tgz;
		nomatch ->
		    case re:run(StdOut, "Zip archive data", [{capture, none}]) of
			match ->
			    zip;
			nomatch ->
			    undefined
		    end
	    end
    end.


%%% ----------------------------------------------------------
%%% @doc Constructs a CXP directory name.
%%% @end
%%% ----------------------------------------------------------
-spec makeCxpDirName(string(), string(), string()) -> string().

makeCxpDirName(CxpName, CxpId, CxpVersion) ->
    CxpName++"_"++CxpId++"_"++CxpVersion.


remove(true, UP) ->
    case file:delete(UP) of
	{error, _Reason} ->
	    ok;
	ok ->
	    info("removed downloaded UP: ~s", [UP])
    end;

remove(_, _) ->
    ok.


initTargetHandlers(UpType) ->
    declare(targetHandlers, varDict),
    if
	UpType =:= "vrcs" ->
	    initTargetHandler("xsd:up", "SWM/SWM_CXA11423/schema/CloudConfig.xsd"); 
	true ->
	    initTargetHandler("xsd:up", "SWM/SWM_CXA11423/schema/SwConfig.xsd")
    end,
    initTargetHandler("xsd:lmc", "SWM/SWM_CXA11423/schema/CxpInfo.xsd"),
    
    initTargetHandler("cli_extension", "GMF/GMF_CXA11422/schema/cli_extension.xsd"),
    initTargetHandler("log", "LOG/LOG_CXA11425/schema/log.xsd"),
    
    initTargetHandler("authorization", "COMSA/COMSA_CXA11418/schema/authorization.xsd"),
    
    initTargetHandler("alarm", "COMSA/COMSA_CXA11418/schema/alarm.xsd", 
		      fun inspAlarm:inspect/2, 
		      fun inspAlarm:summary/0),
    
    initTargetHandler("appm", "APPM/APPM_CXA11417/schema/appm.xsd", 
		      fun inspAppm:inspect/2, 
		      fun inspAppm:summary/0),
    
    initTargetHandler("xsd:appm_config", "APPM/APPM_CXA11417/schema/mp_config.xsd"),
    
    initTargetHandler("coli_auth", "ECOLI/ECOLI_CXA11473/schema/coli_auth.xsd"),
    initTargetHandler("coli_reg", "ECOLI/ECOLI_CXA11473/schema/coli_reg.xsd"),
    
    initTargetHandler("gmfMim", "GMF/GMF_CXA11422/schema/gmfMim.xsd",
		      fun inspGmfMim:inspect/2,
		      fun inspGmfMim:summary/0),
    
    initTargetHandler("gmfImm", "GMF/GMF_CXA11422/schema/gmfImm.xsd", 
		      fun inspGmfImm:inspect/2, 
		      fun inspGmfImm:summary/0),
    
    initTargetHandler("pmEvent", "PES/PES_CXA11494/schema/pmEvent.xsd"),
    initTargetHandler("pmEventAlias", "PES/PES_CXA11494/schema/pmEventAlias.xsd"),
    initTargetHandler("pmEventFilter", "PES/PES_CXA11494/schema/pmEventFilter.xsd"),
    initTargetHandler("pmEventJob", "PES/PES_CXA11494/schema/pmEventJob.xsd"),
    initTargetHandler("pmEventProducer", "PES/PES_CXA11494/schema/pmEventProducer.xsd"),
    
    initTargetHandler("pms", "PMS/PMS_CXA11463/schema/pms.xsd"),
    initTargetHandler("pmsCounter", "PMS/PMS_CXA11463/schema/pmsCounter.xsd"),
    initTargetHandler("pmsJob", "PMS/PMS_CXA11463/schema/pmsJob.xsd"),
    initTargetHandler("pmsGroupAlias", "PMS/PMS_CXA11463/schema/pmsAlias.xsd"),
    
    initTargetHandler("web", "SYS/SYS_CXA11464/schema/web.xsd"),
    initTargetHandler("web_sec", "SYS/SYS_CXA11464/schema/web_sec.xsd").


initTargetHandler(Target, Schema) ->
    initTargetHandler(Target, Schema, undefined, undefined).


initTargetHandler(Target, Schema, Handler, Summarizer) ->
    {_Mode, RcsTop, SchemaPath} = findSchema(Schema),
    XsdState =
	try xmerl_xsd:process_schema(filename:join(RcsTop, SchemaPath), []) of
	    {ok, XS} ->
		XS;
	    Other ->
		err("failed to parse XSD: ~p", [{Other, SchemaPath}]),
		undefined
	catch
	    ExType:ExData ->
		err("failed to parse XSD: ~p", [{ExType, {ExData, SchemaPath}}]),
		undefined
	end,
    varDictStore(
      targetHandlers, 
      Target, 
      #targetHandler{schemaPath=SchemaPath,
		     xsdState=XsdState,
		     handler=Handler,
		     summarizer=Summarizer}).


setCwd(Dir) ->
    case file:set_cwd(Dir) of
	{error, Reason} ->
	    throw({set_cwd, {Dir, Reason}});
	ok ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% @doc Display the 'description' from UP metadata.
%%% @end
%%% ----------------------------------------------------------
-spec showDescription(#xmlElement{}) -> any().

showDescription(XmlEle) ->
    case getSubElements(description, XmlEle) of
	[] ->
	    warn("missing in UP metadata: 'description'", []);
	[DescrElement] ->
	    DescrText = getContainedText(DescrElement),
	    info("description: ~s", [DescrText])
    end.


%%% ----------------------------------------------------------
%%% @doc Gets the product type string.
%%% @end
%%% ----------------------------------------------------------
-spec getProductType(#xmlElement{}) -> string().

getProductType(XmlEle) ->
    case getSubElements(type, XmlEle) of
	[] ->
	    warn("missing in UP metadata: 'type'", []),
	    "unknown";
	[TypeElement] ->
	    Raw = getContainedText(TypeElement),
	    Stripped = strip(Raw),
	    if
		Stripped =/= Raw ->
		    warn("'type' element in UP metadata is not stripped: '~s'", [Raw]);
		true ->
		    ok
	    end,
	    Stripped;
	[_|_] ->
	    warn("multiple 'type' elements in UP metadata", []),
	    "unknown"
    end.


%%% ----------------------------------------------------------
%%% @doc Gets the HW/SW compatibility index.
%%% @end
%%% ----------------------------------------------------------
-spec getHwSwCompatibility(#xmlElement{}) -> string()|undefined.

getHwSwCompatibility(XmlEle) ->
    case getSubElements(hwSwCompatibility, XmlEle) of
	[] ->
	    undefined;
	[CompatElement] ->
	    getRequiredAttribute(index, CompatElement);
	[_|_] ->
	    err("multiple 'hwSwCompatibility' elements in UP metadata", []),
	    undefined
    end.


%%% ----------------------------------------------------------
%%% @doc Constructs a #boardList{} from UP metadata.
%%% @end
%%% ----------------------------------------------------------
-spec getBoardListFromContentInfo(#xmlElement{}) -> #boardList{}.
	  
getBoardListFromContentInfo(XmlEle) ->
    ContentInfo = getOneSubElement(contentinfo, XmlEle),
    ProductEles = getSubElements(product, ContentInfo),
    Products =
	lists:map(
	  fun(ProductEle) ->
		  getProduct(ProductEle)
	  end,
	  ProductEles),
    #boardList{products=Products, hwcategory="N/A", hwmodel="N/A"}.


%%% ----------------------------------------------------------
%%% @doc Gets a map of #boardList{} indexed by {HwCat, HwMod}.
%%% @end
%%% ----------------------------------------------------------
-spec getBoardLists(#xmlElement{}) ->
	  orddict:orddict({string(), string()}, #boardList{}).

getBoardLists(XmlEle) ->
    % exactly one <boardLists> element
    BoardListsEle = getOneSubElement(boardLists, XmlEle),
    BoardListEles = getSubElements(boardList, BoardListsEle),
    lists:foldl(
      fun(BoardListEle, Acc) ->
	      HwCat = getRequiredAttribute(hwcategory, BoardListEle),
	      HwMod = getRequiredAttribute(hwmodel, BoardListEle),
	      orddict:store({HwCat, HwMod}, getBoardList(BoardListEle), Acc)
      end,
      orddict:new(),
      BoardListEles).

			  
%%% ----------------------------------------------------------
%%% @doc Makes a #boardList{} record from the given 'boardList' element.
%%% @end
%%% ----------------------------------------------------------
-spec getBoardList(#xmlElement{}) -> #boardList{}.
	  
getBoardList(BoardListEle) ->
    BoardTypes =
	lists:map(
	  fun(BoardTypeEle) ->
		  Pno=getRequiredAttribute(productNumber, BoardTypeEle),
		  Rev=getRequiredAttribute(revision, BoardTypeEle),
		  #boardType{productNumber=Pno, revision=Rev}
	  end,
	  getSubElements(boardType, BoardListEle)),
    Products =
	lists:map(
	  fun getProduct/1,
	  getSubElements(product, BoardListEle)),
    HwCategory=getRequiredAttribute(hwcategory, BoardListEle),
    HwModel=getRequiredAttribute(hwmodel, BoardListEle),
    #boardList{hwmodel=HwModel,
	       hwcategory=HwCategory,
	       boardTypes=BoardTypes,
	       products=Products}.
    

%%% ----------------------------------------------------------
%%% @doc Makes a #product{} record from the given 'product' element.
%%% @end
%%% ----------------------------------------------------------
-spec getProduct(#xmlElement{}) -> #product{}.

getProduct(ProductEle) ->
    #product{name=getRequiredAttribute(name, ProductEle),
	     id=getRequiredAttribute(id, ProductEle),
	     ver=getRequiredAttribute(version, ProductEle),
	     file=getAttribute(filename, ProductEle, "")}.


%%% ----------------------------------------------------------
%%% @doc Get a list of #product{} entries by inspecting
%%% all .cxp files. The obtained list is checked against
%%% the given list which is from UP metadata.
%%% @end
%%% ----------------------------------------------------------
-spec getProductsFromTarfiles([#product{}]) -> [#product{}].
  
getProductsFromTarfiles(ProductsPreliminary) ->
    TempDirName = "tmp",
    CxpFiles = filelib:wildcard("*.cxp"),
    Products =
	lists:map(
	  fun(CxpTgz) ->
		  ok = file:make_dir(TempDirName),
		  case x("tar",
			 ["--wildcards", "-xzf", CxpTgz,
			  "-C", TempDirName,
			  "cxp*.xml"]) of
		      #osResult{code=Code, stderr=StdErr} when Code =/= 0 ->
			  throw({failed_to_find_cxp_metadata, {CxpTgz, StdErr}});
		      _ ->
			  info("extracted cxp*.xml from: ~p", [CxpTgz])
		  end,
		  #cxpInfo{name=CxpName,
			   id=CxpId,
			   ver=CxpVersion} = getCxpDirName(TempDirName, false),
		  Files = filelib:wildcard(filename:join(TempDirName, "*")),
		  lists:foreach(fun(File) -> ok = file:delete(File) end, Files),
		  ok = file:del_dir(TempDirName),
		  #product{name=CxpName, id=CxpId, ver=CxpVersion, file=CxpTgz}
	  end,
	  CxpFiles),

    ProdPrelD = makeDict(ProductsPreliminary, metadata_from_up),
    ProdD = makeDict(Products, metadata_from_cxp),
    
    % each product entry from UP metadata should map to a file
    orddict:fold(
      fun(K, #product{name=NameFromUp}, _) ->
	      case orddict:find(K, ProdD) of
		  error ->
		      err("no CXP file has id:ver matching ~s", [K]);
		  {ok, #product{name=NameFromCxp}} when NameFromCxp =/= NameFromUp ->
		      
		      err("mismatch of 'name' attribute, "
			  "id:ver is: ~s, "
			  "name from UP metadata: ~s, "
			  "name from CXP metadata: ~s",
			  [K, NameFromUp, NameFromCxp]);
		  _ ->
		      ok
	      
	      end
      end,
      none,
      ProdPrelD),
    
    % each product entry from actual CXP files should map to a UP entry
    orddict:fold(
      fun(K, _, _) ->
	      case orddict:find(K, ProdPrelD) of
		  error ->
		      err("CXP file with id:ver = ~s is not listed in UP metadata", [K]);
		  _ ->
		      ok
	      end
      end,
      none,
      ProdD),
    
    Products.


%%% ----------------------------------------------------------
%%% @doc Strip leading and trailing whitespace from the
%% given string.
%%% @end
%%% ----------------------------------------------------------
-spec makeDict([#product{}], atom()) -> orddict:orddict(string(), #product{}).

makeDict(Products, Context) ->
    lists:foldl(
      fun(#product{id=Id, ver=Ver}=Q, Acc) ->
	      Key = Id++":"++Ver,
	      case orddict:find(Key, Acc) of
		  {_, P} ->
		      err("duplicate key: ~s, ~p, ~p, context: ~w",
			  [Key, P, Q, Context]);
		  error ->
		      ok
	      end,
	      orddict:store(Key, Q, Acc)
      end,
      orddict:new(),
      Products).
