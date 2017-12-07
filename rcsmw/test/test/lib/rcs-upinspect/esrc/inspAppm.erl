%%% ----------------------------------------------------------
%%% %CCaseFile:	inspAppm.erl %
%%% Author:	erarafo
%%% Description: Inspects 'appm' appdata.
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(inspAppm).
%-behaviour(behaviour).
-id('Updated by CCase').
-vsn('/main/R5A/R6A/R8A/1').
-date('2016-11-29').
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
%%% R5A/2      2016-02-15 erarafo     "inspector" behaviour
%%% R5A/3      2016-02-19 erarafo     Validate APPM config files
%%% R5A/4      2016-02-25 erarafo     Inspection elaborated
%%% R5A/6      2016-03-18 erarafo     False positive fixed
%%% R6A/1      2016-09-13 erarafo     Faulty presentation fixed
%%% R8A/1      2016-11-29 erarafo     Refactoring
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc ...
%%% @end
%%% ----------------------------------------------------------

-export([inspect/2, summary/0]).

-import(upLib, [err/2,
		warn/2,
		depr/2,
		info/2,


		progress/1,

		getTargetHandler/1,
		schemaValidate/2,

		valDictBind/3,
		valDictGet/2,
		valDictGetAll/1,
		getRequiredAttribute/2,
		getAttribute/2,
		getSubElements/2]).

-include("upInspect.hrl").

-include_lib("kernel/include/file.hrl").

inspect(Appdata,
	#inspContext{cxpInfo=#cxpInfo{dir=CxpDir, name=CxpName, id=CxpId, ver=CxpVer},
		     cxcInfo=#cxcInfo{name=CxcName, id=CxcId},
		     resolvedFile=ResFile}) ->
    NameIdVer={CxpName, CxpId, CxpVer},

    Lms = getSubElements(loadmodule, Appdata),

    lists:foreach    % for each <loadmodule> in this <appdata>
                 (
      fun(Lm) ->
	      Tag = getRequiredAttribute(tag, Lm),
	      Name = getRequiredAttribute(name, Lm),
	      Id = getRequiredAttribute(id, Lm),

	      %% 	      CxcIdMangled = [if C =:= $/ -> $_; true -> C end || C <- CxcId],
	      %% 	      IdMangled = [if C =:= $/ -> $_; true -> C end || C <- Id],
	      if
		  Id =/= CxcId ->
		      warn("<loadmodule id=\"~s\"> unexpected, should be: id=\"~s\", ~s",
			   [Id, CxcId, ResFile]);
		  true ->
		      ok
	      end,

	      Files = getSubElements(file, Lm),
	      {ExecutablesPerType, FileInfos} =
		  lists:foldl(
		    fun(File, {EPT, FileInfosSoFar}=Acc) ->
			    Type = getRequiredAttribute(type, File),
			    RelPath = getRequiredAttribute(relpath, File),
			    UpRelPath = filename:join(CxpDir, RelPath),
			    case filelib:wildcard(UpRelPath) of
				[] ->
				    err("no such file: ~s", [UpRelPath]),
				    Acc;
				[_, _|_]=Other ->
				    err("ambiguous path: ~s -> ~p", [UpRelPath, Other]),
				    Acc;
				[OneFile] ->
				    %% 				    {ok, #file_info{type=OsType, mode=OsMode}} =
				    %% 					file:read_file_info(OneFile),
				    %% 				    info("type: ~p, mode: ~p, file: ~s", [OsType, OsMode, OneFile]),
				    if
					Type =:= "i386" orelse
					    Type =:= "arm-linux-gnueabi" orelse
					    Type =:= "i686" orelse
					    Type =:= "armhf" ->
					    % executable
					    info("      type: ~s, file: ~s", [Type, OneFile]),
					    if
						Type =:= "i686" orelse Type =:= "armhf" ->
						    depr("deprecated: type=\"~s\" in ~s",
							 [Type, ResFile]);
						true ->
						    ok
					    end,
					    {orddict:append(typeResolved(Type), true, EPT),
					     FileInfosSoFar++[#programFileInfo{type=Type, relPath=RelPath}]};
					Type =:= "config" ->
					    #xsdResult{libXml2Code=Code} =
						schemaValidate(OneFile, "xsd:appm_config"),
					    if
						Code =/= 0 ->
						    warn("   type: ~s, failed validation, file: ~s", [Type, OneFile]);
						true ->
						    info("      type: ~s, validated, file: ~s", [Type, OneFile])
					    end,
					    {EPT, FileInfosSoFar++[#programFileInfo{type=Type, relPath=RelPath}]};
					true ->
					    {EPT, FileInfosSoFar++[#programFileInfo{type=Type, relPath=RelPath}]}
				    end
			    end
		    end,
		    {orddict:new(), []},
		    Files),

	      ExecutablesCount =
		  lists:foldl(
		    fun({_Type, Occs}, Acc) ->
			    max(Acc, length(Occs))
		    end,
		    0,
		    orddict:to_list(ExecutablesPerType)),

	      if
		  ExecutablesCount > 1 ->
		      err("multiple executables in loadmodule element: ~s", [ResFile]);
		  true ->
		      ok
	      end,

	      NameId = {Name, Id},

	      % this ensures that {name, Id} is globally unique (not just unique within CXP)
	      valDictBind(programs,
			  NameId,
			  #programInfo{tag=Tag,
				       name=Name,
				       cxcName=CxcName,
				       cxpNameIdVer=NameIdVer,
				       cxpDir=CxpDir,
				       fileInfos=FileInfos})
      end,
      Lms),  % processed each <loadmodule> in this <appdata>


    LmLists = getSubElements(lmlist, Appdata),
    lists:foreach(
      fun(LmList) ->
	      BoardTypes = getBoardTypes(LmList, ResFile),

	      LmListLms = getSubElements(lm, LmList),
	      lists:foreach(
		fun(LmListLm) ->
			LmListLmName = getRequiredAttribute(name, LmListLm),
			LmListLmId = getRequiredAttribute(id, LmListLm),
			lists:foreach(
			  fun(BT) ->
				  valDictBind(lmlistLmrefs,
					      {NameIdVer, BT, LmListLmName, LmListLmId},
					      #lmlistLmRef{cxpNameIdVer=NameIdVer,
							   cxcId=CxcId,
							   boardType=BT,
							   resFile=ResFile})
			  end,
			  BoardTypes)
		end,
		LmListLms),
	      ok
      end,
      LmLists),

    ProgramGroups = getSubElements(programgroup, Appdata),
    lists:foreach(
      fun(ProgramGroup) ->
	      ProgramGroupName = getRequiredAttribute(name, ProgramGroup),
	      ProgramGroupLms = getSubElements(lm, ProgramGroup),
	      lists:foreach(
		fun(ProgramGroupLm) ->
			ProgramGroupLmName = getRequiredAttribute(name, ProgramGroupLm),
			ProgramGroupLmId = getRequiredAttribute(id, ProgramGroupLm),
			% TODO, for now assume that programgroup names are globally unique?
			valDictBind(programgroupLmrefs, {ProgramGroupName, ProgramGroupLmName, ProgramGroupLmId},
				    #programgroupLmRef{resFile=ResFile})
		end,
		ProgramGroupLms),
	      ok
      end,
      ProgramGroups),

    ok.


typeResolved("armhf") ->
    "arm-linux-gnueabi";

typeResolved("i686") ->
    "i386";

typeResolved(Any) ->
    Any.






summary() ->

    progress("summarize registered programs"),

    orddict:fold(
      fun(_Key, #programInfo{tag=Tag,
			     name=Name,
			     fileInfos=FileInfos,
			     cxpNameIdVer={CxpName, _, _},
			     cxpDir=CxpDir,
			     cxcName=CxcName}, Acc) ->
	      info("  ~s ~s ~s ~s", [Tag, Name, CxpName, CxcName]),
	      lists:foreach(
		fun(#programFileInfo{type=Type, relPath=RelPath}) ->
			info("    ~s ~s", [Type, filename:join(CxpDir, RelPath)])
		end,
		FileInfos),
	      Acc
      end,
      [],
      valDictGetAll(programs)),

    % ensure that all LM refs in lmlists are correct
    LmlistErrorCount =
	orddict:fold(
	  fun({_, _, Name, Id},
	      #lmlistLmRef{boardType=BoardType,
			   resFile=ResFile},
	      Acc) ->
		  ProgramsKey = {Name, Id},
		  try valDictGet(programs, ProgramsKey) of
		      _ ->
			  Acc
		  catch
		      throw:{unbound, _} ->
			  err("lmlist for board type ~s refers to unknown load module: ~s ~s, ~s",
			      [BoardType, Name, Id, ResFile]),
			  Acc + 1
		  end
	  end,
	  0,
	  valDictGetAll(lmlistLmrefs)),
    if
	LmlistErrorCount > 0 ->
	    info("number of lmlist reference errors: ~w", [LmlistErrorCount]);
	true ->
	    info("lmlist references are valid", [])
    end,

    % ensure that all LM refs in programgroups are correct
    ProgramgroupErrorCount =
	orddict:fold(
	  fun({ProgramGroupName, Name, Id}, #programgroupLmRef{resFile=ResFile}, Acc) ->
		  try valDictGet(programs, {Name, Id}) of
		      _ ->
			  Acc
		  catch
		      throw:{unbound, _} ->
			  err("programgroup ~s refers to nonexistent loadmodule ~s ~s, ~s",
			      [ProgramGroupName, Name, Id, ResFile]),
			  Acc+1
		  end
	  end,
	  0,
	  valDictGetAll(programgroupLmrefs)),
    if
	ProgramgroupErrorCount > 0 ->
	    info("number of programgroup reference errors: ~w", [ProgramgroupErrorCount]);
	true ->
	    info("programgroup references are valid", [])
    end,
    ok.


getBoardTypes(LmList, ResFile) ->
    BoardTypeDeprec = getAttribute(boardType, LmList),
    if
	BoardTypeDeprec =/= undefined ->
	    depr("boardType=\"~s\" in lmlist, ~s", [BoardTypeDeprec, ResFile]),
	    BoardTypes = getSubElements(boardType, LmList),
	    if
		BoardTypes =/= [] ->
		    warn("ignoring boardType elements: ~p, in ~199s",
			 [[getRequiredAttribute(id, T)||T <- BoardTypes],
			  ResFile]);
		true ->
		    ok
	    end,
	    [BoardTypeDeprec];
	BoardTypeDeprec =:= undefined ->
	    Result =
		lists:foldl(
		  fun(BoardType, Acc) ->
			  Acc++[getRequiredAttribute(id, BoardType)]
		  end,
		  [],
		  getSubElements(boardType, LmList)),
	    if
		Result =:= [] ->
		    err("lmlist specifies no board type, ~s", [ResFile]),
		    [];
		true ->
		    Result
	    end
    end.

