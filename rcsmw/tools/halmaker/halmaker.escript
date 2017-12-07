#!/usr/bin/env escript
%% -*- erlang -*-

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	halmaker.escript %
%%% @author etxjotj
%%% @copyright Ericsson AB 2016-2017
%%% @version /main/11

%%% @doc ==HAL-SWP and HYBRID-UP generator==
%%% This script generates a HAL-SWP based on a baseline UP, a content file
%%% towards a legacy UP
%%% This script generats a HYBRID UP with the content resulting from a
%%% merger of a HAL-SWP for a board, and a legacy UP
%%%
%%% To create both artefacts, run this
%%% halmaker.escript
%%%   --legacy-up legacy_up.zip
%%%   --baseline-up baseline_up.zip
%%%   --out out 
%%%   --swp-pnr CXP123456 
%%%   --swp-vsn P1A 
%%%   --swp-name HAL-SWP 
%%%   --swp-content content.xml 
%%%   --hybrid-name HYBRID 
%%%   --hybrid-pnr CXP987654 
%%%   --hybrid-vsn P1A
%%%
%%% To create only HAL-SWP, run this
%%% halmaker.escript
%%%   --legacy-up legacy_up.zip
%%%   --baseline-up baseline_up.zip
%%%   --out out 
%%%   --swp-pnr CXP123456 
%%%   --swp-vsn P1A 
%%%   --swp-name HAL-SWP 
%%%   --swp-content content.xml 
%%%
%%% To create only the HybridUP, run this
%%%   --legacy-up legacy_up.zip
%%%   --out out 
%%%   --hybrid-name HYBRID 
%%%   --hybrid-pnr CXP987654 
%%%   --hybrid-vsn P1A
%%%   --hal-swp hal_swp.zip
%%%
%%% @end

-module(halmaker).
-vsn('/main/11').
-date('2017-01-20').
-author('etxjotj').
-mode(compile).
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% /main/1    2016-02-02 etxjotj     Created
%%% /main/4    2016-12-01 etxjotj     Remade for halmaker
%%% /main/7    2016-12-08 etxjotj     First version of halmaker
%%% /main/8    2016-12-09 etxjotj     Make hybrid up
%%% /main/9    2016-12-28 etxberb     Strict match on [] from xmerl_scan:file
%%% /main/10   2016-12-28 etxberb     
%%% /main/11   2017-01-20 etxjotj     Allow for more than one board in HAL-SWP
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-include_lib("xmerl/include/xmerl.hrl").
-export([main/1]).

-record(basic, {typeA, product, date, description, type, release, 
		framework, contentinfo}).
%-compile(nowarn_unused_funcs).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Main function
%%% Baseline-UP and Legacy-UP data is used with a content definition file
%%% in order to create a HAL-SWP
%%% A HAL-SWP and a Legacy-UP is used to create a hybrid UP with merged
%%% contents from these two sources
%%% @end
%%% ----------------------------------------------------------

main(Args) ->
    main(Args, #{}).

main(["-help"|_],_) ->
    main(x,x);

main(["--legacy-up", Path|Args], Opts) ->
    main(Args, Opts#{legacyUP => Path});
main(["--baseline-up", Path|Args], Opts) ->
    main(Args, Opts#{baselineUP => Path});
main(["--description", Description|Args], Opts) ->
    main(Args, Opts#{description => Description});
main(["--swp-content", SwpContent|Args], Opts) ->
    main(Args, Opts#{swpContent => SwpContent});
main(["--swp-pnr", ProdNr|Args], Opts) ->
    main(Args, Opts#{swpnr => ProdNr});
main(["--swp-vsn", Vsn|Args], Opts) ->
    main(Args, Opts#{swpvsn => Vsn});
main(["--swp-name", Name|Args], Opts) ->
    main(Args, Opts#{swpname => Name});
main(["--hal-swp", Path|Args], Opts) ->
    main(Args, Opts#{halpath => Path});
main(["--hybrid-pnr", ProdNr|Args], Opts) ->
    main(Args, Opts#{hybpnr => ProdNr});
main(["--hybrid-vsn", Vsn|Args], Opts) ->
    main(Args, Opts#{hybvsn => Vsn});
main(["--hybrid-name", Name|Args], Opts) ->
    main(Args, Opts#{hybname => Name});
main(["--workspace", Workspace|Args], Opts) ->
    main(Args, Opts#{workspace => Workspace});
main(["--override-index", LegacyIndex|Args], Opts) ->
    main(Args, Opts#{override_index => LegacyIndex});
main(["--out", Out|Args], Opts) ->
    main(Args, Opts#{out => Out});
main(["--debug"|Args], Opts) ->
    put(debug, true),
    main(Args, Opts);
main(["--help"|_], _) ->
    main(x,x);

main([], Opts) ->
    start(Opts);

main(_, _) ->
    io:format(
      "==============================~n"
      "Generate HAL-SWP and Hybrid UP~n"
      "==============================~n"
      "~n"
      "This tool generates HAL-SWP on a baseline UP~n"
      "It also produces a hyrbid UP if such metadata is given~n"
      "Usage: halmaker.escript [Flags]~n"
      "~n"
      "General flags:~n"
      "--------------~n"
      "  --workspace Path   : Where this script should store temporary files~n"
      "                       This can be controlled by the $WORKSPACE ~n"
      "                       variable or is default /tmp~n"
      "  --out Path         : A directory where the out result is stored~n"
      "  --debug            : [optional] Adds debug printouts~n"
      "~n"
      "Flags for building a HAL-SWP~n"
      "----------------------------~n"
      "  --legacy-up Path   : Path to a legacy UP which the HAL-SWP is built ~n"
      "                       towards~n"
      "  --baseline-up Path : Path to a baseline UP from which the HAL-SWP ~n"
      "                       contents is fetched.~n"
      "  --swp-content Path : Path to a file specifying the content of the ~n"
      "                       HAL-SWP~n"
      "  --swp-pnr ProdNr   : Product number of the HAL-SWP~n"
      "  --swp-vsn Version  : R-state of the HAL-SWP~n"
      "  --swp-name Name    : Name of the HAL-SWP~n"
      "~n"
      "Flags for building a Hybrid UP~n"
      "------------------------------~n"
      "  --hal-path Path    : Path to a pre-existing HAL-SWP if not build~n"
      "                       at the same time.~n"
      "  --hybrid-pnr ProdNr: Product number of the Hybrid-UP~n"
      "  --hybrid-vsn Version: R-state of the Hybrid-UP~n"
      "  --hybrid-name Name : Name of the Hybrid-UP~n"
      "  --legacy-up Path   : Same as above. Only include once!~n"
	     ,[]).


%%% ----------------------------------------------------------
%%% adoc
%%% aend
%%% ----------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           start(Opts)
%%% Input: Opts:[{atom(),term()} - a key-value tuple lists with options
%%% Output: 
%%% Exceptions: 
%%% Description: The main sequence of events
%%% ----------------------------------------------------------


start(Opts) ->
    debug("Options parsed~n~p~n",[Opts]),
    
    NewOpts = normalize_paths(Opts),
    debug("Normalized paths~n~p~n",[NewOpts]),

    RootDir = make_tmp_dir(NewOpts, "halmaker"),
    try 
	%% Find MW CXP and unpack it
	enable_mw_cxp(NewOpts, RootDir),
	case make_hal_swp(NewOpts#{rootDir => RootDir}) of
	    {ok, HalPath} ->
		make_hybrid_up(NewOpts#{rootDir => RootDir,
					halpath => HalPath});
	    no_hal_swp ->
		make_hybrid_up(NewOpts#{rootDir => RootDir})
	end
    after
	case get(debug) of 
	    true -> ok ;
	    _ -> cmd(["rm -rf ", RootDir])
	end
    end.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

normalize_paths(Opts) ->
    normalize_paths(Opts, [legacyUP, baselineUP, workspace, swpContent, out,
			   halpath]).

normalize_paths(Opts, [Key|Keys]) ->
    try #{Key := Value} = Opts,
	 normalize_paths(Opts#{Key := normalize_path(Value)}, Keys)
    catch error:{badmatch,_} -> 
	    normalize_paths(Opts, Keys)
    end;
normalize_paths(Opts, []) ->
    Opts.
	  
%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

%% Unpack the RCSMW CXP and unpack it, then add path to the beam files
%% It is assumed that RCSMW is a global CXP and there is only one in the UP

enable_mw_cxp(Opts, RootDir) ->
    [LegacyUP] = get_parameters(Opts, [legacyUP]),
    LegacyDir = filename:join(RootDir, "legacy"),
    unpack_up(LegacyUP, LegacyDir),
    SquashFsDir = filename:join(RootDir, "software"),
    [CxpFile] = filelib:wildcard(filename:join(LegacyDir, "RCSMW*.cxp")),
    debug("Found RCSMW at: ~p~n",[CxpFile]),
    CxpDir = unpack_cxp(CxpFile, SquashFsDir),
    Dirs = filelib:wildcard(filename:join([CxpDir, "*", "*", "ebin"])),
    [code:add_path(Dir)||Dir<-Dirs],
    ets:new(sysErrorLoggerStats, [set, public, named_table]),
    ets:new(sysErrorLoggerThresholds, [set, public, named_table]),
    os:putenv("DEV_PATCHES", SquashFsDir),
    os:putenv("RCS_ROOT", RootDir),
    cmd(["mkdir -p ", RootDir, "/rcs/swm"]).
    
%%% ----------------------------------------------------------
%%% MAKE HAL-SWP    
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

make_hal_swp(Opts) ->
    try #{swpContent := _ } = Opts,
	 do_make_hal_swp(Opts)
    catch error:{badmatch, X} when is_map(X) ->
	    debug("No swp content~n",[]),
	    no_hal_swp
    end.

do_make_hal_swp(Opts) ->
    [LegacyUP, BaselineUP, SwpContent, 
     ProdNr, ProdVsn, ProdName,
     RootDir, OutDir] = 
	get_parameters(Opts, [legacyUP, baselineUP, swpContent,
			      swpnr, swpvsn, swpname,
			      rootDir, out]),
    BaselineDir = filename:join(RootDir, "baseline"),
    LegacyDir = filename:join(RootDir, "legacy"),
    SwpDir = filename:join(RootDir, "swp"),
    unpack_up(BaselineUP, BaselineDir),
    case filelib:is_dir(LegacyDir) of
	true -> ok;
	false -> unpack_up(LegacyUP, LegacyDir)
    end,
    BaselineData = get_up_data(BaselineDir),
    LegacyData = get_up_data(LegacyDir),
    LegacyIndex = 
	try #{override_index := LI} = Opts,
	     put(override_index, true),
	     LI
	catch error:{badmatch, _} ->
		HwSwCE = find_element(hwSwCompatibility, LegacyData),
		find_attribute(index, HwSwCE)
	end,
    {BoardTypes, CxpList, ExcludeList} = 
	get_swp_content(SwpContent, LegacyIndex),
    debug("SwpContent:~n~p~n",[{BoardTypes, CxpList, ExcludeList}]),

    %% When moving cxps we assume that all boardtypes uses the same list
    %% Because they are the same category
    {BoardPnr, BoardVsn} = hd(BoardTypes),

    {HwCategory, HwModel, MovedCxps} = 
	move_cxps(BaselineDir, BaselineData, SwpDir, 
		  BoardPnr, BoardVsn, CxpList),

    generate_hal_metadata(MovedCxps, ExcludeList, LegacyData, LegacyIndex,
			  HwCategory, HwModel,
			  ProdName, ProdNr, ProdVsn, 
			  BoardTypes,
			  SwpDir),
    debug("Creating HAL-SWP container~n",[]),
    ZipFile = filename:join(OutDir, ProdName++"_"++ProdNr++"_"++ProdVsn++".zip"),
    cmd(["cd ",SwpDir," ; zip ",ZipFile, " *"]),
    {ok, ZipFile}.


%%% ----------------------------------------------------------
%%% MAKE HYBRID UP	     
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

make_hybrid_up(Opts) ->
    try #{hybpnr := _} = Opts,
	 do_make_hybrid_up(Opts)
    catch error:{badmatch, X} when is_map(X) ->
	    debug("No hybrid product number. No hybrid UP will be produced~n",
		  [])
    end.

do_make_hybrid_up(Opts) ->
    [LegacyUP, HalPath, 
     ProdNr, ProdVsn, ProdName,
     RootDir, OutDir] = 
	get_parameters(Opts, [legacyUP, halpath,
			      hybpnr, hybvsn, hybname,
			      rootDir, out]),
    %% If not unpacked, unpack these packages
    LegacyDir = filename:join(RootDir, "legacy"),
    case filelib:is_dir(LegacyDir) of
	true -> ok;
	false -> unpack_up(LegacyUP, LegacyDir)
    end,

    SwpDir = filename:join(RootDir, "swp"),
    case filelib:is_dir(SwpDir) of
	true -> ok;
	false -> unpack_up(HalPath, SwpDir)
    end,

    debug("Reading HAL-SWP metadata~n",[]),
    HalData = get_up_data(SwpDir),
    HalBasic = extract_basic_metadata(HalData),
    HalProd = HalBasic#basic.product,

    %% We assume only one board list, and at at least (and usually only)
    %% one board type
    BoardListsE = find_element(boardLists, HalData),
    BoardListE = find_element(boardList, BoardListsE),
    BoardTypes = [{find_attribute(productNumber, BoardTypeE), 
		   find_attribute(revision, BoardTypeE)}||
		     BoardTypeE<-BoardListE#xmlElement.content,
		     BoardTypeE#xmlElement.name == boardType],

    HwCategory = find_attribute(hwcategory,BoardListE),
    HwModel = find_attribute(hwmodel, BoardListE),
    
    BoardType = {nospc(element(1, hd(BoardTypes))),
		 element(2, hd(BoardTypes))},

    {ok, Products} = swmBoardList:products(BoardType, [{global, LegacyDir},
						       {hal, SwpDir}]),
    debug("Selected products: ~n~p~n",[Products]),
    HybridDir = filename:join(RootDir, "hybrid"),
    %% Link CXP files to hybrid dir
    cmd(["mkdir -p ", HybridDir]),
    [begin
	 SourceDir = case Source of
			 global -> 
			     LegacyDir;
			 hal -> 
			     SwpDir
		     end,
	 cmd(["ln ", filename:join(SourceDir, File), " ", HybridDir])
     end||{_, {Source, File}}<-Products],
    HybridProd = {ProdName, ProdNr, ProdVsn, undefined},
    
    generate_hybrid_metadata(Products, LegacyDir, BoardTypes, 
			     HwCategory, HwModel,
			     HybridProd, HalProd,
			     HybridDir),
    debug("Creating hybrid container~n",[]),
    ZipFile=filename:join(OutDir,ProdName++"_"++ProdNr++"_"++ProdVsn++".zip"),
    cmd(["cd ",HybridDir," ; zip ",ZipFile, " *"]),
    {ok, ZipFile}.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

generate_hybrid_metadata(Products, LegacyDir, BoardTypes, 
			 HwCategory, HwModel,
			 HybridProd, HalProd, HybridDir) ->

    {HalName, HalNr, HalVsn, _} = HalProd,

    debug("Reading legacy metadata~n",[]),
    LegacyData = get_up_data(LegacyDir),
    LegacyBasic = extract_basic_metadata(LegacyData),

    

    debug("~p~n",[LegacyBasic]),

    {ProdName, ProdNr, ProdVsn, _} = HybridProd,
    Fmt = "Hybrid UP for ~s ~s based on ~s ~s ~s and ~s ~s ~s",
    Args = [HwCategory, HwModel, ProdName, ProdNr, ProdVsn,
	    HalName, HalNr, HalVsn],
    Description = lists:flatten(io_lib:format(Fmt, Args)),
    BasicData = LegacyBasic#basic{typeA="MSRBS-UP",
				  product = HybridProd,
				  date = calendar:universal_time(),
				  description = Description
				 },
    ProdNr = element(2, HybridProd),

    HwSwCE = find_element(hwSwCompatibility, LegacyData),
    Index = find_attribute(index, HwSwCE),


    ProductList = [{Name, Nr, Vsn, File}||
		      {{Name, Nr, Vsn}, {_, File}}<-Products],

    Path = filename:join(HybridDir, ProdNr++"-up.xml"),
    debug("Writing ~p~n",[Path]),
    {ok, Fd} = file:open(Path, [write]),
    write_header(Fd),
    write_basic(Fd, BasicData),
    write(Fd, 1, "<hwSwCompatibility index=~p />~n",[Index]),
    write_board_lists(Fd, 1, HwCategory, HwModel, BoardTypes, ProductList),
    write(Fd, 0, "</configuration>~n"),
    file:close(Fd),
    ok.

%%% ----------------------------------------------------------
%%% LIBRARY FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

unpack_cxp(CxpFile, SquashFsDir)->
    UnSquashFsCmd = "/app/rbs/wrtools/tools-sdk-20130220/usr/sbin/unsquashfs",
    cmd(["mkdir -p ",SquashFsDir, "/tmp"]),
    cmd("cd "++SquashFsDir++"/tmp; tar xf "++CxpFile),
    cmd("cd "++SquashFsDir++"/tmp; ar x sw.ar sqfs.img"),
    R = cmd("cd "++SquashFsDir++"/tmp; " ++
		UnSquashFsCmd ++ " -f -d . -n sqfs.img"),
    debug("~s~n~s~n",["cd "++SquashFsDir++"/tmp; " ++ UnSquashFsCmd ++ 
			  " -f -d . -n sqfs.img", R]),
    cmd("cd "++SquashFsDir++"/tmp; rm sw.ar sqfs.img"),
    XmlPattern = filename:join([SquashFsDir, "tmp", "cxp*.xml"]),
    [CxpMeta] = 
	case filelib:wildcard(XmlPattern) of
	    [] -> 
		error_logger:error_msg(
		  "No metadata found in ~s/tmp~n~s~n",
		  [SquashFsDir, 
		   os:cmd(["ls -la ",SquashFsDir,"/tmp"])]),
		erlang:error(no_metadata_found,[CxpFile, SquashFsDir]);
	    [CM] -> [CM];
	    MultipleFiles ->
		error_logger:error_msg(
		  "Multiple metadata found in ~s/tmp~n~s~n",
		  [SquashFsDir, 
		   os:cmd(["ls -la ",SquashFsDir,"/tmp"])]),
		erlang:error({multiple_metadata_found, MultipleFiles},
			     [CxpFile, SquashFsDir])
	end,
    
    {{ConfigurationE, []}, CxpMeta} = {xmerl_scan:file(CxpMeta), CxpMeta},
    ProductE = find_element(product, ConfigurationE),
    Name = find_attribute(name, ProductE),
    ProdId = find_attribute(id, ProductE),
    Version = find_attribute(version, ProductE),
    
    CxpName = Name++"_"++ProdId++"_"++Version,
    CxpDir = filename:join(SquashFsDir, CxpName),
    cmd("mv "++SquashFsDir++"/tmp "++CxpDir),
    CxpDir.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

make_tmp_dir(Opts, DirName) ->
    WrkSpc = work_dir(Opts),
    cmd(["mktemp -d ",WrkSpc,"/",DirName,".XXXXX"])--"\n".
    
%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

work_dir(Opts) ->
    case os:getenv("WORKSSPACE") of
	false ->
	    try #{workspace := Value} = Opts,
		 Value
	    catch error:{badmatch, _} ->
		    "/tmp"
	    end;
	EnvDir -> 
	    EnvDir
    end.

%%% ----------------------------------------------------------
%%% #           unpack_up(ZipFile, TargetDir)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Unpack a zip file into the specified dir
%%% ----------------------------------------------------------

unpack_up(ZipFile, TargetDir) ->
    debug("Unpacking: ~p~n",[ZipFile]),
    cmd(["mkdir -p ", TargetDir, " ; cd ", TargetDir, " ; unzip ",ZipFile]).

%%% ----------------------------------------------------------
%%% #           get_up_data(UpDir)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Read the metadata of a UP or HAL-SWP
%%% ----------------------------------------------------------

get_up_data(UpDir) ->
    case filelib:wildcard(filename:join(UpDir, "{*-up.xml,*-hal.xml}")) of
	[] ->
	    erlang:error(missing_up_metadata, [UpDir]);
	[Path] ->
	    {{ConfigurationE, []}, Path} = {xmerl_scan:file(Path), Path},
	    ConfigurationE
    end.

%%% ----------------------------------------------------------
%%% #           get_swp_content(SwpContent, LegacyIndex)
%%% Input: 
%%% Output: {BoardPnr, BoardVsn, IncludedLMCs, ExcludedLMCs}
%%% Exceptions: 
%%% Description: Read the contents of the content definition file
%%% ----------------------------------------------------------

get_swp_content(SwpContent, LegacyIndex) ->
    debug("Legacy index is ~p~n",[LegacyIndex]),
    {{SwpContentE, []}, SwpContent} = {xmerl_scan:file(SwpContent), SwpContent},
    BoardTypesE = find_element(boardTypes, SwpContentE),
    IncludedE = find_element(included, SwpContentE),
    HwSwCE = find_exclude_list(LegacyIndex, 
			       [E||E<-SwpContentE#xmlElement.content,
				   E#xmlElement.name == hwSwCompatibility]),


    BoardTypes = [board_type_id(BoardTypeE)
		   ||BoardTypeE<-BoardTypesE#xmlElement.content,
		     BoardTypeE#xmlElement.name == boardType],
    LMCs = [{find_attribute(name, LmcE),
	     find_attribute(id, LmcE)}||
	       LmcE<-IncludedE#xmlElement.content,
	       LmcE#xmlElement.name == lmc],
    
    ExcludedLMCs = [find_attribute(name, Element)||
		       Element<-HwSwCE#xmlElement.content,
		       Element#xmlElement.name == exclude_lmc],
    
    {BoardTypes, LMCs, ExcludedLMCs}.

%%% ----------------------------------------------------------
%%% #           find_exclude_list(Index, Content)
%%% Input: Index:string()
%%%        Content:#xmlElement.content
%%% Output: 
%%% Exceptions: 
%%% Description: Find the excluded list matching the given index
%%% ----------------------------------------------------------

find_exclude_list(LegacyIndex, [HwSwCE|Content])  ->
  %% when HwSwCE#xmlElement.name == hwSwCompatibility ->
    case find_attribute(index, HwSwCE) of
	LegacyIndex ->
	    HwSwCE;
	_ ->
	    find_exclude_list(LegacyIndex, Content)
    end;
find_exclude_list(Index, []) ->
    case get(override_index) of
	true ->
	    error_logger:warning_msg(
	      "The override index (~p) is not defined in "
	      "the SwpContent~n", [Index]),
	    erlang:error(no_matching_index, [Index, []]);
	_ -> 
	    error_logger:error_msg(
	      "The index in the legacy up (~p) is not defined in "
	      "the SwpContent~n",
	      [Index]),
	    erlang:error(no_matching_index, [Index, []])
    end.


%%% ----------------------------------------------------------
%%% #           move_cxps
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%%   Move the CXP from the baseline UP that we want in HAL
%%%   Return the CXPs that were moved
%%% ----------------------------------------------------------

move_cxps(BaselineDir, BaselineData, SwpDir, BoardPnr, BoardVsn, CxpList) ->
    cmd(["mkdir -p ", SwpDir]),
    BoardListE = 
	case find_board_list(BaselineData, BoardPnr, BoardVsn) of
	    {ok, BL} -> 
		BL;
	    {error, board_not_found} ->
		error_logger:error_msg(
		  "The baseline UP does not support board ~p ~p~n",
		  [BoardPnr, BoardVsn]),
		erlang:error(board_not_found)
	end,
			    
    CxpContent = extract_products(BoardListE),
    debug("Content in baseline UP:~n~p~n",[CxpContent]),

    MovedCxps = 
	[begin 
	     cmd(["cd ",BaselineDir, " ; ln ", File, " ", SwpDir]),
	     {Name, Id, Vsn, File}
	 end||{Name, Id, Vsn, File} <- CxpContent,
	      is_in_cxp_list(Id, CxpList)],
    {find_attribute(hwcategory, BoardListE),
     find_attribute(hwmodel, BoardListE),
     MovedCxps}.

%%% ----------------------------------------------------------
%%% #           is_in_cxp_list(ID, CxpList)
%%% Input: 
%%% Output: true|false
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

is_in_cxp_list(Id, CxpList) ->
    case lists:keysearch(Id, 2, CxpList) of
	{value, _} ->
	    true;
	false ->
	    false
    end.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

%% Find and return the board list given by BoardPnr and BoardVsn
find_board_list(BaselineData, BoardPnr, BoardVsn) ->
    BoardListsE = find_element(boardLists, BaselineData),
    do_find_board_list(BoardPnr, BoardVsn, BoardListsE#xmlElement.content).

do_find_board_list(BoardPnr, BoardVsn, [BoardListE|Content]) 
  when BoardListE#xmlElement.name == boardList ->
    ListContent = BoardListE#xmlElement.content,
    case is_board_type_in_list(BoardPnr, BoardVsn, ListContent) of
	true ->
	    {ok, BoardListE};
	false ->
	    do_find_board_list(BoardPnr, BoardVsn, Content)
    end;
do_find_board_list(BoardPnr, BoardVsn, [_|Content]) -> 
    do_find_board_list(BoardPnr, BoardVsn, Content);
do_find_board_list(_, _, []) -> 
    {error, board_not_found}.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

is_board_type_in_list(BoardPnr, BoardVsn, [BoardTypeE|Content]) 
  when BoardTypeE#xmlElement.name == boardType ->
    BoardPnrS = nospc(BoardPnr),
    case find_attribute(productNumber, BoardTypeE) of
	BoardPnrS ->
	    Range = find_attribute(revision, BoardTypeE),
	    case sysUtil:is_rev_inRange(BoardVsn, Range) of
		true ->
		    true;
		false ->
		    is_board_type_in_list(BoardPnr, BoardVsn, Content)
	    end;
	_ ->
	    is_board_type_in_list(BoardPnr, BoardVsn, Content)
    end;
is_board_type_in_list(BoardPnr, BoardVsn, [_|Content]) ->
    is_board_type_in_list(BoardPnr, BoardVsn, Content);
is_board_type_in_list(_, _, []) ->
    false.

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------

%%% In a board list, extract all product elements
extract_products(BoardListE) ->
    [format_product(ProductE)||
	ProductE <- BoardListE#xmlElement.content,
	ProductE#xmlElement.name == product].
    
%%% ----------------------------------------------------------
%%% #           format_product
%%% Input: ProductE: #xmlElement{} - A product element
%%% Output: 
%%% Exceptions: 
%%% Description: Collapses a XML element into a 4-size tuple
%%% {Name, Id, Version, Filename}. The first three are mandatory. Filename is
%%% optional and can be 'undefined'
%%% ----------------------------------------------------------

format_product(ProductE) ->
    {find_attribute(name, ProductE),
     find_attribute(id, ProductE),
     find_attribute(version, ProductE),
     try find_attribute(filename, ProductE) of
	 F -> F
     catch _:_ -> undefined
     end}.


cmd(Cmd) ->
    Res = os:cmd(Cmd),
    debug("~s~n~s~n",[lists:flatten(Cmd), Res]),
    Res.



%%% ----------------------------------------------------------
%%% #           board_type_id(BoardTypeE)
%%% Input: BoardTypeE:#xmlElement{}
%%% Output: 
%%% Exceptions: 
%%% Description: Extracts the identifying attributes for a board type
%%% ----------------------------------------------------------

board_type_id(BoardTypeE) ->
    try {find_attribute(productNumber, BoardTypeE),
	 find_attribute(revision, BoardTypeE)} of
	{PN, R} -> {PN, R}
    catch _:_ ->
	    error_logger:error_report(
	      [{?MODULE, board_type_id},
	       {error, incorrect_boardtype_identifier},
	       {correct_keys, [productNumber, revision]}|
	       [{A#xmlAttribute.name, A#xmlAttribute.value}||
		   A<-BoardTypeE#xmlElement.attributes]]),
		throw(incorrect_boardtyp_identifier)
    end.

nospc([$ |T]) ->
    nospc(T);
nospc([H|T]) ->
    [H|nospc(T)];
nospc([]) -> [].

%%% ----------------------------------------------------------
%%% #           
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------


generate_hal_metadata(MovedCxps, ExcludeList, LegacyData, LegacyIndex,
		      HwCategory, HwModel, 
		      ProdName, ProdNr, ProdVsn, 
		      BoardTypes,
		      SwpDir) ->
    
    ContentinfoE = find_element(contentinfo, LegacyData),
    LegacyContent = extract_products(ContentinfoE),
    %% BoardListE = find_board_list(LegacyData, BoardPnr, BoardVsn),
    %% LegacyContent = extract_products(BoardListE),
    debug("Legacy content:~n~p~n",[LegacyContent]),
    
    BasicData = #basic{typeA = "MSRBS-UP",
		       product = {ProdName, ProdNr, ProdVsn, undefined},
		       date = calendar:universal_time(),
		       contentinfo = MovedCxps},

    ProductList = compile_product_list(LegacyContent, MovedCxps, ExcludeList),
	   
    %% HwSwCE = find_element(hwSwCompatibility, LegacyData),
    %% Index = find_attribute(index, HwSwCE),
	
    Path = filename:join(SwpDir, ProdNr++"-hal.xml"),

    debug("Writing ~p~n",[Path]),
    {ok, Fd} = file:open(Path, [write]),
    write_header(Fd), 
    write_basic(Fd, BasicData),
    write(Fd, 1, "<hwSwCompatibility index=~p />~n",[LegacyIndex]),
    write_board_lists(Fd, 1, HwCategory, HwModel, BoardTypes,
		      ProductList),
    write(Fd, 0, "</configuration>~n"),
    file:close(Fd),
    ok.

compile_product_list([{LName, LId, _, _}|LegacyContent],[],ExcludeList) ->
    case lists:member(LName, ExcludeList) of
	true ->
	    compile_product_list(LegacyContent, [], ExcludeList);
	false ->
	    [{LName, LId, "*", undefined}|
	     compile_product_list(LegacyContent, [], ExcludeList)]
    end;
compile_product_list([{LName, LId, _, _}|LegacyContent], MovedCxps,
		     ExcludeList) ->
    case lists:keysearch(LId, 2, MovedCxps) of
	{value, {MName, MId, MVsn, MFile}} ->
	    [{MName, MId, MVsn, MFile}|
	     compile_product_list(LegacyContent, 
				  MovedCxps--[{MName, MId, MVsn, MFile}],
				  ExcludeList)];

	false ->
	    case lists:member(LName, ExcludeList) of
		true ->
		    compile_product_list(LegacyContent, MovedCxps, ExcludeList);
		false ->
		    [{LName, LId, "*", undefined}|
		     compile_product_list(LegacyContent,MovedCxps,ExcludeList)]
	    end
    end;
compile_product_list([], MovedCxps, _) ->
    MovedCxps;
compile_product_list(_, _, _) ->
    [].

%%% ----------------------------------------------------------
%%% #           extract_basic_metadata(ConfigurationE)
%%% Input: ConfigurationE: #xmlElement{} of a UP metadata
%%% Output: 
%%% Exceptions: 
%%% Description: Takes a UP metafile and adds (overwrites) hw 
%%%              compatibility info
%%% ----------------------------------------------------------

extract_basic_metadata(ConfigurationE) ->
    TypeA = try find_attribute(type, ConfigurationE) of
		A -> A
	    catch _:_ -> undefined
	    end,
    ProductE = find_element(product, ConfigurationE),
    Product = format_product(ProductE),
    Date = find_text(find_element(date, ConfigurationE)),
    Description = try find_element(description, ConfigurationE) of
		      D -> find_text(D)
		  catch _:_ -> undefined
		  end,
    Type = try find_element(type, ConfigurationE) of
	       T -> find_text(T)
	   catch _:_ -> undefined
	   end,
    Release = try find_element(release, ConfigurationE) of
		  R -> find_text(R)
	      catch _:_ -> undefined
	      end,
    Framework = try find_element(framework, ConfigurationE) of
		    FrameworkE -> 
			[format_product(Element)||
			    Element<-FrameworkE#xmlElement.content,
			    Element#xmlElement.name == product]
		catch _:_ -> undefined
		end,
    ContentInfoE = find_element(contentinfo, ConfigurationE),
    ContentInfo = [format_product(Element)||
		      Element<-ContentInfoE#xmlElement.content,
		      Element#xmlElement.name == product],
    #basic{typeA = TypeA,
	   product = Product,
	   date = Date,
	   description = Description,
	   type = Type,
	   release = Release,
	   framework = Framework,
	   contentinfo = ContentInfo}.

write_header(Fd) ->
    write(Fd, 0, "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>~n"),
    write(Fd, 0, "<!--~n"),
    write(Fd, 0, "# =======================================================~n"),
    write(Fd, 0, "#~n"),
    {{Year,_,_},_} = calendar:local_time(),
    write(Fd, 0, "# Copyright (c) Ericsson AB ~w All rights reserved~n",[Year]),
    write(Fd, 0, "# ~n"),
    Vsn= hd(proplists:get_value(vsn, ?MODULE:module_info(attributes), unknown)),
    Version = atom_to_list(Vsn),
    write(Fd, 0, "# Generated by halmaker.escript@@~s~n",[Version]),
    write(Fd, 0, "#~n"),
    write(Fd, 0, "# =======================================================~n"),
    write(Fd, 0, "-->~n"),
    write(Fd, 0, "~n").

write_basic(Fd, BasicData) ->
    write(Fd, 0, "<configuration type=~p>~n",[BasicData#basic.typeA]),
    write_product(Fd, 1, BasicData#basic.product),
    write(Fd, 1, "<date>~s</date>~n",[format_date(calendar:local_time())]),
    write(Fd, 1, "<description>~s</description>~n",
	  [case BasicData#basic.description of
	       undefined -> "";
	       Description -> Description
	   end]),
    case BasicData#basic.type of
	undefined -> ok;
	Type -> 
	    write(Fd, 1, "<type>~s</type>~n",[Type])
    end,
    case BasicData#basic.release of
	undefined -> ok;
	Release ->
	    write(Fd, 1, "<release>~s</release>~n",[Release])
    end,
    case BasicData#basic.framework of
	undefined -> ok;
	Frameworks ->
	    write(Fd, 1, "<framework>~n"),
	    [write_product(Fd, 2, FwP)||FwP<-Frameworks],
	    write(Fd, 1, "</framework>~n")
    end,
    write(Fd, 1, "<contentinfo>~n"),
    [write_product(Fd, 2, CIP)||CIP<-BasicData#basic.contentinfo],
    write(Fd, 1, "</contentinfo>~n"),
    ok.

write_board_lists(Fd, Indent, HwCategory, HwModel, BoardTypes, ProductList) ->
    Indent1 = Indent+1,
    Indent2 = Indent+2,
    write(Fd, Indent, "<boardLists>~n"),
    write(Fd, Indent1, "<boardList hwcategory=~p hwmodel=~p>~n",
	  [HwCategory, HwModel]),
    [write(Fd, Indent2, "<boardType productNumber=~p revision=~p />~n",
	   [BoardPnr, BoardVsn])||{BoardPnr, BoardVsn}<-BoardTypes],
    [write_product(Fd, Indent2, Cxp)|| Cxp<-ProductList],

    write(Fd, Indent1, "</boardList>~n"),
    write(Fd, Indent, "</boardLists>~n").


write_product(Fd, Indent, {N, P, V, undefined}) ->
    validate_stringNotEmpty([{"product name", N},
			     {"product id", P},
			     {"product version", V}]),
    write(Fd, Indent, "<product name=~p id=~p version=~p />~n", [N,P,V]);
write_product(Fd, Indent, {N, P, V, F}) ->
    validate_stringNotEmpty([{"product name", N},
			     {"product id", P},
			     {"product version", V}]),
    write(Fd,
	  Indent,
	  "<product name=~p id=~p version=~p filename=~p />~n",
	  [N, P, V, F]).


write(Fd, Indent, Fmt) ->
    write(Fd, Indent, Fmt, []).
   
write(Fd, Indent, Fmt, Args) ->
    IndentStr = lists:append(tl(["  "||_<-lists:seq(0,Indent)])),
    io:format(Fd, IndentStr++Fmt, Args).

format_date({{Year,Month,Day},{Hour, Minute, Second}}) ->
    lists:append([io_lib:format("~w-",[Year]),
		  io_lib:format("~2..0w-",[Month]),
		  io_lib:format("~2..0wT",[Day]),
		  io_lib:format("~2..0w:",[Hour]),
		  io_lib:format("~2..0w:",[Minute]),
		  io_lib:format("~2..0w",[Second])]).

validate_stringNotEmpty(Params) ->
    validate_stringNotEmpty(Params, Params).

validate_stringNotEmpty([{_, [_ | _]} | Tail], Params) ->
    validate_stringNotEmpty(Tail, Params);
validate_stringNotEmpty([{Key, Value}  | _]=X, Params) ->
    case Value of
	"" -> 
	    error_logger:error_msg("Mandatory value is missing: ~p~n",[Key]),
	    erlang:error(missing_mandatory, [X, Params]);
	_ ->
	    error_logger:error_msg("Value is not a string: ~p~n",[Key]),
	    erlang:error(not_a_string, [X, Params])
    end;
validate_stringNotEmpty([], _) ->
    ok.

%% verify(Path, Schema) ->
%%     Cmd = ["xmllint --noout --schema ",Schema," ", Path],
%%     case cmdres(Cmd) of
%% 	{0, _} ->
%% 	    ok;
%% 	{1, Res} ->
%% 	    error_logger:error_msg("Unclassified error:~n~s~n",[Res]),
%% 	    throw({xmllint, unclassified});
%% 	{2, Res} ->
%% 	    error_logger:error_msg("Error in DTD:~n~s~n",[Res]),
%% 	    throw({xmllint, dtdError});
%% 	{3, Res} ->
%% 	    error_logger:error_msg("Validation error:~n~s~n",[Res]),
%% 	    throw({xmllint, validationError3});
%% 	{4, Res} ->
%% 	    error_logger:error_msg("Validation error:~n~s~n",[Res]),
%% 	    throw({xmllint, validationError4});
%% 	{5, Res} ->
%% 	    error_logger:error_msg("Error in schema compilation:~n~s~n",[Res]),
%% 	    throw({xmllint, schemaCompilationError});
%% 	{6, Res} ->
%% 	    error_logger:error_msg("Error writing output:~n~s~n",[Res]),
%% 	    throw({xmllint, outputWriteError});
%% 	{7, Res} ->
%% 	    error_logger:error_msg("Error in pattern:~n~s~n",[Res]),
%% 	    throw({xmllint, patternError});
%% 	{8, Res} ->
%% 	    error_logger:error_msg("Error in reader registration:~n~s~n",[Res]),
%% 	    throw({xmllint, readerRegistrationError});
%% 	{9, Res} ->
%% 	    error_logger:error_msg("Out of memory error:~n~s~n",[Res]),
%% 	    throw({xmllint, outOfMemoryError})
%%     end.
    
%% cmdres(CmdList) ->
%%     Cmd = lists:flatten(CmdList),
%%     CmdR = Cmd++" ; echo -n \"Res=$?\"",
%%     debug("~s~n~s~n",[Cmd, Res = os:cmd(CmdR)]),
%%     Code = lists:last(string:tokens(Res, "\n=")),
%%     Rev = lists:reverse(Res),
%%     Result =
%% 	case string:str(Rev, "\n") of
%% 	    0 -> "";
%% 	    Pos -> lists:reverse(string:sub_string(Rev, Pos))
%% 	end,
%%     {list_to_integer(Code), Result}.


%% %%% ----------------------------------------------------------
%% %%% #           
%% %%% Input: 
%% %%% Output: 
%% %%% Exceptions: 
%% %%% Description: 
%% %%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           normalize_path(Path)
%%% Input: Path:string()
%%% Output: string()
%%% Exceptions:
%%% Description: Expand a short or relative path to an absolute path
%%% ----------------------------------------------------------

normalize_path([$/|_]=Path) -> 
    Path;
normalize_path([$~,$/|Path]) ->
    Home = os:getenv("HOME"),
    filename:join(Home, Path);
normalize_path([$~|Path]) ->
    Home = os:getenv("HOME"),
    filename:join([filename:dirname(Home)|filename:split(Path)]);
normalize_path(Path) ->
    {ok, Cwd} = file:get_cwd(),
    filename:join(Cwd, Path).

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

find_element(ElementName, Element) when is_record(Element, xmlElement) ->
    find_element(ElementName, Element#xmlElement.content);
find_element(ElementName, ContentList) ->
    case lists:keysearch(ElementName, #xmlElement.name, ContentList) of
	{value, Element} ->
	    Element;
	false ->
	    erlang:error(no_such_element, [ElementName, ContentList])
    end.

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
%%% #           find_text(Element)
%%% Input: Element:#xmlElement{} or
%%% Output: Value:string()
%%% Exceptions:
%%% Description: Extract the text content from an xmlText element
%%% ----------------------------------------------------------

find_text(Element) when is_record(Element, xmlElement) ->
    case Element#xmlElement.content of
	[Text] ->
	    Text#xmlText.value;
	[] ->
	    "";
	Texts -> lists:append(Texts)
    end.

%%% ----------------------------------------------------------
%%% #           debug(Format, Args)
%%% Input: 
%%% Output: 
%%% Exceptions:
%%% Description: Print in the shell if debug flag is used
%%% ----------------------------------------------------------

%% debug(Format) ->
%%     debug(Format, []).
    
debug(Format, Args) ->
    case get(debug) of
	true ->
	    io:format(Format, Args);
	_ ->
	    ok
    end.


get_parameters(Opts, [Key|Keys]) ->
    try #{Key := Value} = Opts,
	 [Value|get_parameters(Opts, Keys)]
    catch error:{badmatch, _} ->
	    error_logger:error_msg("Parameter ~p is missing~n",[Key]),
	    erlang:error({missing_mandatory_parameter, Key},
			 [Opts, [Key|Keys]])
    end;    
get_parameters(_, []) -> [].









%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: 
%%% ----------------------------------------------------------
%% even_more_internal_function1(One, Two)->
%%    nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

