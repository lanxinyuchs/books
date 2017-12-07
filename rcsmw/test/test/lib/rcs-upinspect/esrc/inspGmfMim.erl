%%% ----------------------------------------------------------
%%% %CCaseFile:	inspGmfMim.erl %
%%% Author:	erarafo
%%% Description: Inspects 'gmfImm' appdata.
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(inspGmfMim).
%-behaviour(behaviour).
-id('Updated by CCase').
-vsn('/main/R6A/4').
-date('2016-05-22').
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
%%% R6A/1      2016-04-27 erarafo     First version
%%% R6A/2      2016-04-28 erarafo     Second version
%%% R6A/3      2016-05-03 erarafo     Report hidden IMM classes
%%% R6A/4      2016-05-22 erarafo     Report classes and structs per MOM
%%% ----------------------------------------------------------

-export([inspect/2, summary/0]).

-import(upLib, [err/2,
		warn/2,
		info/2,
		progress/1,
		valGet/1,
		valDictBind/3,
		valDictGetAll/1,
		getRequiredAttribute/2,
		getAttribute/2,
		getAttribute/3,
		getOneSubElement/2,
		getSubElements/2,
		setMember/2]).

-include("upInspect.hrl").

-record(mimClass,
	{
	 name   :: string(),
	 isRoot  :: boolean(),
	 rdnName :: string()
	}).

-record(mimStruct,
	{
	 name   :: string()
	}).


-record(mim, 
	{mimName     :: string(), 
	 prefixing   :: boolean(),
	 mimClasses  :: ordsets:ordset(#mimClass{}),
	 mimStructs  :: ordsets:ordset(#mimStruct{})
	}).


inspect(Appdata, #inspContext{cxpInfo=#cxpInfo{dir=CxpDir}}=Context) ->
    PathPattern = "*RCS*/COM*_CXC*/com-*/priv/tgt_*/opt/com/etc/model",
    PathList = 
	case
	    filelib:wildcard(PathPattern) of
	    [] ->
		throw({no_dtd_directory, PathPattern});
	    Other ->
		Other
	end,
    info("using model paths: ~p", [PathList]),
    try
	case getSubElements(mimInfos, Appdata) of
	    [] ->
		% a bit strange .. error? TODO
		ok;
	    [MimInfos] ->
		MimInfoList = getSubElements(mimInfo, MimInfos),
		lists:foreach(
		  fun(MimInfo) -> 
			  MpXmlPath = getAttribute(path, MimInfo),
			  MpXmlFile = getAttribute(file, MimInfo),
			  MpXmlIfType = getAttribute(ifType, MimInfo),
			  if
			      MpXmlIfType =:= "erl" ->
				  ok;
			      MpXmlIfType =:= "imm" ->
				  TotalPath = upLib:fileIsReadable(CxpDir, MpXmlPath, MpXmlFile),
				  inspectSchema(TotalPath, Context, PathList),
				  ok;
			      true ->
				  % TODO, warning?
				  ok
			  end
		  end,
		  MimInfoList);
	    [_, _|_] ->
		% error maybe? TODO
		ok
	end
    catch
	X:Y ->
	    Stack = erlang:get_stacktrace(),
	    err("unspecific CATCH: ~p, ~p~nstack: ~p", [X, Y, Stack])
    end.


summary() ->
    
    Mims = valDictGetAll(mims),
    
    progress("summary of gmfMim info"),
    % collected MIM-related info

    orddict:fold(
      fun(MimName, #mim{prefixing=Prefixing, mimClasses=MimClasses}, _) ->
	      info("MIM: ~s, prefixing: ~s",
		   [MimName, if Prefixing -> "yes"; true -> "no" end]),
	      ordsets:fold(
		fun(#mimClass{name=ClassName, isRoot=true}, _) ->
			info("  root ~s", [ClassName]);
		   (#mimClass{name=ClassName}, _) ->
			info("       ~s", [ClassName])
		end,
		[],
		MimClasses)
      end,
      none,
      Mims),
    
    % verification of IMM data against MIM data
    case setMember("gmfImm", inspectTargets) of
	false ->
	    info("no cross-validation against gmfImm metadata (add -a gmfImm to enable)", []),
	    info("no reporting of hidden IMM classes (add -a gmfImm to enable)", []),
	    ok;
	true ->
	    ImmClasses = valDictGetAll(immClasses),
	    orddict:fold(
	      fun(MimName, #mim{prefixing=Prefixing, mimClasses=MimClasses}, Acc) ->
		      ordsets:fold(
			fun(#mimClass{name=MimClassName, isRoot=IsRoot, rdnName=MimRdnName}, Bcc) ->
				% info("about to cross-check: ~p", 
				%      [{MimClassName, MimRdnName, {Prefixing, MimName}, IsRoot}]),
				ImmClassName = 
				    if
					Prefixing ->
					    MimName++MimClassName;
					true ->
					    MimClassName
				    end,
				case orddict:find(ImmClassName, ImmClasses) of
				    error ->
					err("no IMM class found: ~s", [ImmClassName]);
				    {_, #immClass{name=_N, rdnName=ImmRdnName}} ->
					% info("ok, found IMM class: ~s", [N]),
					ExpectedImmRdnName =
					    if
						Prefixing andalso IsRoot ->
						    MimName++MimRdnName;
						true ->
						    MimRdnName
					    end,
					if
					    ImmRdnName =:= ExpectedImmRdnName ->
						% info("rdn name validated: ~s", [ImmRdnName]),
						ok;
					    true ->
						err("bad IMM rdn name: expected: ~s, actual: ~s", 
						    [ExpectedImmRdnName, ImmRdnName])
					end
				end,
				Bcc
			end,
			[],
			MimClasses),
		      Acc
	      end,
	      [],
	      Mims),
	    
	    % report hidden IMM classes also
	    ImmClassesSet = 
		orddict:fold(
		  fun(_Key, #immClass{name=ImmClassName}, Acc) ->
			  ordsets:add_element(ImmClassName, Acc)
		  end,
		  ordsets:new(),
		  ImmClasses),
	    
	    HiddenClassesSet =
		orddict:fold(
		  fun(MimName, #mim{mimClasses=CC, mimStructs=SS, prefixing=Pfx}, Acc1) ->
			  % info("MIM: ~p, prefixing: ~p, structs: ~p", [MimName, Pfx, SS]),
			  Acc2 =
			      ordsets:fold(
				fun(#mimClass{name=ClassName}, Acc) ->
					ImmClassName = if Pfx -> MimName++ClassName; true -> ClassName end,
					ordsets:del_element(ImmClassName, Acc)
				end,
				Acc1,
				CC),
			  
			  ordsets:fold(
			    fun(#mimStruct{name=StructName}, Acc) ->
				    ImmClassName = if Pfx -> MimName++StructName; true -> StructName end,
				    ordsets:del_element(ImmClassName, Acc)
			    end,
			    Acc2,
			    SS)
		  end,
		  ImmClassesSet,
		  Mims),
	    
	    info("hidden IMM classes: ~p", [ordsets:to_list(HiddenClassesSet)])
    end,
    ok.


%%% ----------------------------------------------------------
%%% @doc Inspects the given schema.
%%% @end
%%% ----------------------------------------------------------
inspectSchema(TotalPath, Context, PathList) ->    
    Models = upLib:getTopElement(TotalPath, PathList),
    case getSubElements(mim, Models) of
	[] ->
	    err("no <mim> element: ~s", [TotalPath]),
	    ok;
	Mims ->
	    try
	    lists:foreach(
	      fun(Mim) ->
		      MimName = getAttribute(name, Mim),
		      if
			  MimName =:= undefined ->
			      throw({no_mim_name, TotalPath});
			  true ->
			      ok
		      end,
		      ExtendedAnalysis = needsExtendedAnalysis(MimName),
		      IsPrefixing = getPrefixing(MimName, Mim),
		      MimClasses = getSubElements(class, Mim),
		      MimClassSet =
			  lists:foldl(
			    fun(MimClass, Acc) ->
				    try
					MimClassName = getAttribute(name, MimClass),
					IsRoot = isRoot(MimClassName, MimClass),
					
					RdnName = getNamingAttribute(MimClassName, MimClass),
					ordsets:add_element(
					  #mimClass{name=MimClassName, isRoot=IsRoot, rdnName=RdnName}, 
					  Acc)
				    catch
					throw:Exceptiondata ->
					    #inspContext{resolvedFile=File}=Context,
					    err("bad MIM class: ~p, appdata file: ~s, mp.xml file: ~s",
						[Exceptiondata, File, TotalPath]),
					    Acc
				    end
			    end,
			    ordsets:new(),
			    MimClasses),
		      validateOneRootClass(MimName, MimClassSet),
		      MimStructs = getSubElements(struct, Mim),
		      MimStructSet = 
			  lists:foldl(
			    fun(MimStruct, Acc) ->
				    MimStructName = getAttribute(name, MimStruct),
				    ordsets:add_element(#mimStruct{name=MimStructName}, Acc)
			    end,
			    ordsets:new(),
			    MimStructs),
		      valDictBind(mims, 
				  MimName, 
				  #mim{mimName=MimName, 
				       prefixing=IsPrefixing, 
				       mimClasses=MimClassSet,
				       mimStructs=MimStructSet}),
		      if
			  ExtendedAnalysis ->
			      lists:foreach(
				fun(MimClass) ->
					MimClassName = getAttribute(name, MimClass),
					info("MOM=~s, EcimClass=~s, ImmClass=~s",
					     [MimName,
					      MimClassName,
					      if IsPrefixing -> MimName++MimClassName;
						 true -> MimClassName
					      end])
				end,
				MimClasses),
			      lists:foreach(
				fun(MimStruct) ->
					MimStructName = getAttribute(name, MimStruct),
					info("MOM=~s, EcimStruct=~s, ImmClass=~s",
					     [MimName,
					      MimStructName,
					      if IsPrefixing -> MimName++MimStructName;
						 true -> MimStructName
					      end])
				end,
				MimStructs);
			  true ->
			      ok
		      end
	      end,
	      Mims)
	    catch
		throw:ExceptionData ->
		    err("bad MIM, ~p", [ExceptionData])
	    end
    end,
    
    ok.


%%% ----------------------------------------------------------
%%% @doc Returns prefixing info for the given MIM.
%%% @end
%%% ----------------------------------------------------------
-spec getPrefixing(string(), #xmlElement{}) -> boolean().

getPrefixing(MimName, Mim) ->
    case getSubElements(domainExtension, Mim) of
	[DX] ->
	    "ECIM" = getAttribute(domain, DX),
	    case getSubElements(extension, DX) of
		Extensions ->
			lists:foldl(
			  fun(Ext, Acc) ->
				  ExtName = getAttribute(name, Ext),
				  ExtValue = getAttribute(value, Ext),
				  if
				      ExtName =:= "immNamespace" andalso Acc =:= undefined ->
					  if
					      ExtValue =:= "NONE" ->
						  false;
					      ExtValue =:= "MOM_NAME" ->
						  true;
					      true ->
						  throw({bad_extension_value, {MimName, ExtValue}})
					  end;
				      ExtName =:= "immNamespace" ->
					  throw({multiple_immNamespace_definition, MimName});
				      true ->
					  Acc
				  end
			  end,
			  undefined,
			  Extensions)
	    end;
	[] ->
	    throw({no_domain_extension, MimName});
	_Multi ->
	    throw({multiple_domain_extension, MimName})
    end.


%%% ----------------------------------------------------------
%%% @doc Returns true if the given class is a root class.
%%% @end
%%% ----------------------------------------------------------
isRoot(MimClassName, MimClass) ->
    case getSubElements(root, MimClass) of
	[] ->
	    false;
	[_] ->
	    true;
	[_, _|_] ->
	    throw({multiple_root_in_class, MimClassName})
    end.


%%% ----------------------------------------------------------
%%% @doc Validates that the number of root classes in the
%%% given set of classes is exactly 1.
%%% @end
%%% ----------------------------------------------------------
validateOneRootClass(MimName, Set) ->
    ordsets:fold(
      fun(#mimClass{isRoot=true}, 0) ->
	      1;
	 (#mimClass{isRoot=true}, 1) ->
	      throw({multi_root_class_in_mom, MimName});
	 (_, Acc) ->
	      Acc
      end,
      0,
      Set).


%%% ----------------------------------------------------------
%%% @doc Returns the naming attribute of the given class.
%%% @end
%%% ----------------------------------------------------------
getNamingAttribute(MimClassName, MimClass) ->
    Candidates =
	lists:foldl(
	  fun(Attr, Acc) ->
		  case getSubElements(key, Attr) of
		      [] ->
			  Acc;
		      [_] ->
			  [Attr|Acc];
		      _ManyKeyElements ->
			  throw({multiple_key_marks, MimClassName})
		  end
	  end,
	  [],
	  getSubElements(attribute, MimClass)),
    case Candidates of
	[] ->
	    throw({no_naming_attribute, MimClassName});
	[NamingAttr] ->
	    RdnName = string:to_lower([hd(MimClassName)])++tl(MimClassName)++"Id",
	    AttrName = getAttribute(name, NamingAttr),
	    if
		RdnName =:= AttrName ->
		    RdnName;
		true ->
		    throw({bad_naming_attribute_name, {MimClassName, AttrName}})
	    end;
	Many ->
	    throw({multiple_naming_attributes, {MimClassName, [getAttribute(name, One)||One <- Many]}})
    end.


needsExtendedAnalysis(MimName) ->
    case valGet(extendedMomAnalysis) of
	"none" ->
	    false;
	"all" ->
	    true;
	CSS ->
	    lists:member(MimName, string:tokens(CSS, ","))
    end.
