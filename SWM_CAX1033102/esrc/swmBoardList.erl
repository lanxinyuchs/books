%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	swmBoardList.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2015-2016
%%% @version /main/R5A/20

%%% @doc ==Header==
%%%   Handling of HAL (Hardware Abstraction Layer) product lists.
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-module(swmBoardList).
-vsn('/main/R5A/20').
-date('2016-03-28').
-author('etxberb').

%%% ###=====================================================================###
%%% # 0.3   LEGAL RIGHTS
%%% ###---------------------------------------------------------------------###
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2016 All rights reserved.
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

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 1.    REVISION LOG
%%% ###---------------------------------------------------------------------###
%% Rev     Date       Name     What
%% ----    ---------- -------  ------------------------------------------------
%% R5A/1   2015-11-24 etxpejn  Created
%% R5A/6   2015-11-30 etxarnu  Updated is_compatible_rev/2
%% R5A/13  2016-01-21 etxberb  Made all_hw/0 more robust and removed spaces in
%%                             productNumber strings.
%% R5A/14  2016-01-22 etxberb  Enhanced get_cxp_list/2 to take a list of KduId.
%% R5A/15  2016-02-26 etxberb  Total refactor - new functions products/1 & /2.
%% R5A/17  2016-03-09 etxberb  Added boardType/0 & boardTypes/0.
%% R5A/18  2016-03-14 etxberb  Exported hwSwCompatibilityIndex/1.
%% R5A/20  2016-03-28 etxberb  Added read_swp_selected/0 & read_swp_selected/1.
%% ------- ---------- -------  ---END-OF-LOG-----------------------------------
%%% ###########################################################################
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% This module is copied in these places:
%%% - $RCS_TOP block SWM
%%% - $OS_TOP block NL
%%% When updating this module, please consider also to update the other copies
%%% !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!! NOTE !!! IMPORTANT !!!
%%% ###########################################################################

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 2.    MODULE BORDER LINE AND INTERNAL DEFINITIONS
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
-export([boardType/0,
	 boardTypes/0,
	 hwSwCompatibilityIndex/1,
	 products/1,
	 products/2,
	 read_swp_selected/0,
	 read_swp_selected/1]).

%%% ###=====================================================================###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
-export([all_hw/0]).
-export([values/2,
	 values/3]).

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###
-include_lib("xmerl/include/xmerl.hrl").

%%% ###=====================================================================###
%%% # 2.4   LOCAL DEFINITION OF MACROS
%%% ###---------------------------------------------------------------------###
-define(HwInventory, <<"ManagedElement=1,SystemFunctions=1,HwInventory=1">>).
-define(SWP_METADATA_filenames_global, "*-up.xml").
-define(SWP_METADATA_filenames_hal,    "*-hal.xml").
-define(SWP_METADATA_path_global, swmLib:software_dir()).
-define(SWP_METADATA_paths_hal, filename:join(swmLib:software_hal_dir(), "*")).
-define(UP_FILES, get(globalUpFiles) ++ get(halUpFiles)).

-define(BoardType_default, {"undefined", "R0A"}).

%% Usage of error_logger:XXX_report
-define(LOG_ERR(ReportInfo),
	sysInitI:error_report(?RepInfo(ReportInfo))).
-define(LOG_ERR_ALL(ReportInfo),
	error_logger:error_report(?RepInfo(ReportInfo))).
-define(LOG_INFO(ReportInfo),
	sysInitI:info_report(?RepInfo(ReportInfo))).
-define(LOG_INFO_ALL(ReportInfo),
	error_logger:info_report(?RepInfo(ReportInfo))).
-define(LOG_WARN(ReportInfo),
	sysInitI:warning_report(?RepInfo(ReportInfo))).
-define(RepInfo(ReportInfo),
	[{?MODULE, ?FUNCTION} | ReportInfo]).

%% Usecase Error
-define(UC_ERR_2(Reason),             {uc_error, Reason}).
-define(UC_ERR_3(Reason, ReportInfo), {uc_error, Reason, ReportInfo}).

%% Reason for Usecase Error
-define(UC_ERR_ParsingFault,
	"Parsing fault").
-define(UC_ERR_MoreThanOneBoardTypeSpecForBB,
	"More than one BoardType specified for Baseband").
-define(UC_ERR_UnrecognizedArg,
	"Unrecognized argument").
-define(UC_ERR_NoGlobalUP,
	"No GlobalUP found").
-define(UC_ERR_MoreThanOneGlobalUP,
	"More than one GlobalUP found").
-define(UC_ERR_MultipleHwSwCompIndexInGlobalUP,
	"Multiple hwSwCompatibilityIndex found in Global UP file").
-define(UC_ERR_MissingHwSwCompIndexInGlobalUP,
	"Missing hwSwCompatibilityIndex in Global UP").
-define(UC_ERR_BoardTypeNotSupportedByNeitherHalGlobalUP,
	"BoardType not supported by neither HalUP nor GlobalUP").
-define(UC_ERR_NoProductListInGlobalUP,
	"No product list found in Global UP").
-define(UC_ERR_MultipleHwSwCompIndexInHalUP,
	"Multiple hwSwCompatibilityIndex found in Hal UP file").
-define(UC_ERR_MissingHwSwCompIndexInHalUP,
	"Missing hwSwCompatibilityIndex in Hal UP file").
-define(UC_ERR_EmptyProductListIn_Source_UP(Source),
	"Empty product list in " ++ atom_to_list(Source) ++ " UP").
-define(UC_ERR_DuplicateIndexInHalUP,
	"Duplicate index in Hal UP").
-define(UC_ERR_ProductNotFoundInSelBoardListInGlobalUP,
	"Product not found in selected boardLists in Global UP").
-define(UC_ERR_BoardTypeDefinedMultipleTimes,
	"BoardType defined multiple times").
-define(UC_ERR_MandatoryInfoMissing,
	"Mandatory information missing").

%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###
-type products() :: [{{Name :: string(), Id :: string(), Version :: string()},
		      {Source :: global | hal, Filename :: string()}}].

-type boardType()    :: {ProductNumber :: string(), Revision :: string()}.
-type boardTypes()   :: boardType_bb() | {boardType_bb(), boardType_ra()}.
-type boardType_bb() :: boardType() | [boardType()]. % hwcategory="BASEBAND" |
						% hwcategory="BASEBAND-T"
-type boardType_ra() :: [boardType()] | all.    % hwcategory="RADIO"

-type metadataPaths() :: (metadataPath_global() |
			  metadataPath_hal() | % global from swmLib:software_dir
			  metadataPath_all() |
			  undefined).          % global from swmLib:software_dir
-type metadataPath_all()    :: {all, Path :: string()}.
-type metadataPath_global() :: Path :: string() | {global, Path :: string()}.
-type metadataPath_hal()    :: {hal, Path :: string()}.

-type uc_err_reason() :: string().
-type uc_err_2()      :: {uc_error,
			  Reason :: uc_err_reason()}.

-type exception_throw(_) :: none().   % erlang:throw(uc_err_2())

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 3.    CODE
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% @doc DEPRECATED. Use boardTypes/0 instead.
%%%
%%% @end
%%% ----------------------------------------------------------
all_hw() ->
    boardTypes().

%%% ###########################################################################
%%% @doc Returns the BoardType for the board I'm executing on.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec boardType() ->
    boardType().
%%% ###=====================================================================###
boardType() ->
    boardType(eqs_pri_service:get_product_information_data()).

%%% ###########################################################################
%%% @doc Returns a list of BoardType for all hardware defined in HwInventory.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec boardTypes() ->
    list(boardType()).
%%% ###=====================================================================###
boardTypes() ->
    [{{"HwInventory", _}, HwItems}] = coi:getMoTree(?HwInventory, 2),
    {ok, TransId} = coi:join_new(),
    Result = boardTypes(HwItems, TransId),
    coi:finish(TransId),
    Result.

boardTypes([{{"HwItem", {_, Ix}}, _} | Tail], TransId) ->
    Ecim_Dn = <<?HwInventory/binary, <<",HwItem=">>/binary, Ix/binary>>,
    case coi:getMoAttributes(TransId, Ecim_Dn, [<<"productData">>]) of
	[{_, StructData}] ->
	    case boardType(StructData) of
		BoardType when is_tuple(BoardType) ->
		    [BoardType | boardTypes(Tail, TransId)];
		undefined ->
		    boardTypes(Tail, TransId)
	    end;
	_ ->
	    boardTypes(Tail, TransId)
    end;
boardTypes([_ | Tail], TransId) ->
    boardTypes(Tail, TransId);
boardTypes([], _) ->
    [].

%%% ###########################################################################
%%% @doc Returns the hwSwCompatibilityIndex for a specified metadata config.
%%%
%%% @end
%%% ###=====================================================================###
hwSwCompatibilityIndex(ConfigE) ->
    case xmlElemRec_all(hwSwCompatibilityIndex, ConfigE) of
	[HwSwCompatibilityIndexE] ->
	    xmlAttrVal(index, HwSwCompatibilityIndexE);
	[_ | _] = HwSwCompatibilityIndexEs ->
	    [xmlAttrVal(index, HwSwCompatibilityIndexE)
	     || HwSwCompatibilityIndexE <- HwSwCompatibilityIndexEs];
	[] ->
	    undefined
    end.

%%% ###########################################################################
%%% @doc Returns the product list for a specified board.
%%%   The product list is composed by matching the productNumber and revision
%%%   against the boardLists in the metadata files for the Global UP and the
%%%   Hal.
%%%
%%%   Metadata files with boardLists have file names ending with:
%%%   - "-up.xml" for Global UP, path found by 'swmLib:software_dir/0'
%%%   - "-hal.xml" for Hal, path found by 'swmLib:software_hal_dir/0'
%%%
%%%   If a metadata file for the Global UP is found in the 'dev_patches'
%%%   directory with the same file name as the original file, it replaces the
%%%   original file.
%%%   If a "*-hal.xml" file is found in the 'dev_patches' directory it replaces
%%%   any original Hal file with the same name or is added to the list of
%%%   valid Hal files if the file name does not match any original Hal file.
%%%
%%%   For test purposes, the function 'products/3' takes a 'Paths' argument
%%%   with possibility to disable not only original Global (for normal upgrade
%%%   and installation) but also Hal UP files.
%%%   All metadata files will be taken from the specified 'Paths' directory.
%%%   The 'dev_patches' directory still has the same role in replacing
%%%   original files as stated above.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec products(BoardTypes :: boardTypes()) ->
    {ok, products()} |
	uc_err_2() |
	exception_throw(uc_err_2()).
%%% ###=====================================================================###
products(BoardTypes) ->
    products(BoardTypes, undefined).

%%% ----------------------------------------------------------
-spec products(BoardTypes :: boardTypes(),
	       Paths      :: metadataPaths()) ->
    {ok, products()} |
	uc_err_2() |
	exception_throw(uc_err_2()).
%%% ###=====================================================================###
products({[Char | _], _} = BoardTypeBB, Paths) when is_integer(Char) ->
    products(BoardTypeBB, all, Paths);
products([BoardTypeBB], Paths) ->
    products(BoardTypeBB, all, Paths);
products({{[Char | _], _} = BoardTypeBB, BoardTypesRA}, Paths)
  when is_integer(Char) ->
    products(BoardTypeBB, BoardTypesRA, Paths);
products({[BoardTypeBB], BoardTypesRA}, Paths) ->
    products({BoardTypeBB, BoardTypesRA}, Paths);
products([], Paths) ->
    products(?BoardType_default, Paths);
products({[], BoardTypesRA}, Paths) ->
    products({?BoardType_default, BoardTypesRA}, Paths);
products(UnrecognizedArg, _) ->
    ?LOG_ERR([?UC_ERR_UnrecognizedArg, {argument, UnrecognizedArg}]),
    throw(?UC_ERR_2(?UC_ERR_UnrecognizedArg)).

%%% ###########################################################################
%%% products
%%%
%%% ###=====================================================================###
products(BoardType, BoardTypesRA, Paths) ->
    GlobalUpFiles = swp_metadata_files(global, Paths),
    HalUpFiles = swp_metadata_files(hal, Paths),
    put(globalUpFiles, GlobalUpFiles),   % Used for error logging.
    put(halUpFiles, HalUpFiles),         % Used for error logging.
    erase(swp_selected),                 % Used for info and error logging.
    try
	{ok, products_main(BoardType, GlobalUpFiles, HalUpFiles, BoardTypesRA)}
    catch
	throw : ?UC_ERR_2(Reason) ->
	    ?UC_ERR_2(Reason);
        throw : ?UC_ERR_3(Reason, ReportInfo) ->
	    ?LOG_ERR_ALL([Reason | ReportInfo ++ [{files, ?UP_FILES}]]),
	    ?UC_ERR_2(Reason);
        exit : {fatal, Details} ->   % Exception from xmerl_scan.
	    ?LOG_ERR_ALL([?UC_ERR_ParsingFault,
			  {fatal, Details},
			  {files, ?UP_FILES}]),
	    ?UC_ERR_2(?UC_ERR_ParsingFault)
    end.

%%% ###########################################################################
%%% read_swp_selected
%%%
%%% ###=====================================================================###
read_swp_selected() ->
    read_swp_selected(all).

read_swp_selected(AttrName) when is_atom(AttrName) ->
    case file:read_file(filename:join(swmLib:swm_dir(), swp_selected)) of
	{ok, Bin} ->
	    read_swp_selected(AttrName, erlang:binary_to_term(Bin));
	Error ->
	    ?LOG_WARN([Error]),
	    []
    end.

read_swp_selected(all, Selected) ->
    Selected;
read_swp_selected(AttrName, Selected) when is_atom(AttrName) ->
    read_swp_selected([AttrName], Selected);
read_swp_selected([AttrName | Tail], Selected) ->
    read_swp_selected_attr(Selected, AttrName) ++
	read_swp_selected(Tail, Selected);
read_swp_selected([], _) ->
    [].

read_swp_selected_attr([{AttrName, AttrValue} | Tail], AttrName) ->
    [AttrValue | read_swp_selected_attr(Tail, AttrName)];
read_swp_selected_attr([_ | Tail], AttrName) ->
    read_swp_selected_attr(Tail, AttrName);
read_swp_selected_attr([], _) ->
    [].

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% values
%%%
%%% ###=====================================================================###
values(E, AttrName) ->
    values(E, AttrName, all).

values([#xmlElement{name = AttrName} = E | Tail],
       AttrName,
       AttrValNames = all) when is_atom(AttrName) ->
    [list2tuple(xmlAttrVals(E)) | values(Tail, AttrName, AttrValNames)];
values([#xmlElement{name = AttrName} = E | Tail], AttrName, AttrValNames)
  when is_atom(AttrName) ->
    [list2tuple([xmlAttrVal_optional(AttrValName, E)
		 || AttrValName <- AttrValNames])
     | values(Tail, AttrName, AttrValNames)];
values(#xmlElement{name = Scope} = E, {Scope, AttrName}, AttrValNames) ->
    values(E#xmlElement.content, AttrName, AttrValNames);
values([#xmlElement{name = Scope} = E | Tail],
       {Scope, AttrName},
       AttrValNames) ->
    values(E#xmlElement.content, AttrName, AttrValNames) ++
	values(Tail, {Scope, AttrName}, AttrValNames);
values([#xmlElement{} = E | Tail], {Scope, AttrName}, AttrValNames) ->
    values(E#xmlElement.content, {Scope, AttrName}, AttrValNames) ++
	values(Tail, {Scope, AttrName}, AttrValNames);
values([#xmlElement{} = E | Tail], AttrName, AttrValNames) ->
    values(E#xmlElement.content, AttrName, AttrValNames) ++
	values(Tail, AttrName, AttrValNames);
values(#xmlElement{} = E, AttrName, AttrValNames) ->
    values(E#xmlElement.content, AttrName, AttrValNames);
values([Char | _] = FileName, AttrName, AttrValNames)
  when is_integer(Char) ->
    values(parse_configE(FileName), AttrName, AttrValNames);
values([[Char | _] | _] = FileNames, AttrName, AttrValNames)
  when is_integer(Char) ->
    values(parse_configE(FileNames), AttrName, AttrValNames);
values([_ | Tail], AttrName, AttrValNames) ->
    values(Tail, AttrName, AttrValNames);
values([], _, _) ->
    [].

%%% ###=====================================================================###
%%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% boardList2products
%%%
%%% ###=====================================================================###
boardList2products(BoardListE, Source) ->
    case
	[{{xmlAttrVal(name, ProductE),
	   xmlAttrVal(id, ProductE),
	   xmlAttrVal(version, ProductE)},
	  {Source,
	   xmlAttrVal_optional(filename, ProductE)}} % Change to mandatory later
	 ||  ProductE <- BoardListE#xmlElement.content,
	     ProductE#xmlElement.name == product]
	of
	[_ | _] = Products ->
	    Products;
	[] ->
	    Reason = ?UC_ERR_EmptyProductListIn_Source_UP(Source),
	    [BL_Attrs] = values([BoardListE], boardList, all),
	    ?LOG_ERR([Reason,
		      {boardList, BL_Attrs},
		      {files, ?UP_FILES}]),
	    throw(?UC_ERR_2(Reason))
    end.

%%% ###########################################################################
%%% boardListE
%%%
%%% ###=====================================================================###
boardListE([#xmlElement{name = boardList} = E | Tail],
	   boardList = ElementName,
	   BoardType) ->
    case is_valid_board_type(BoardType, boardType_all(E)) of
	true ->
	    [BL_Attrs] = values([E], boardList, [hwcategory, hwmodel]),
	    case {BoardType, BL_Attrs} of
		{{all, HwCat}, {HwCat, _}} ->
		    [{{boardList, BL_Attrs}, E} | boardListE(Tail,
							     ElementName,
							     BoardType)];
		{{all, _}, _} ->
		    boardListE(Tail, ElementName, BoardType);
		_ ->
		    {{boardList, BL_Attrs}, E}
	    end;
	false ->
	    boardListE(Tail, ElementName, BoardType)
    end;
boardListE([#xmlElement{name = contentinfo} = E | _],
	   contentinfo,
	   _) ->
    {contentinfo, E};
boardListE([#xmlElement{name = boardLists} = E | _],
	   boardList = ElementName,
	   BoardType) ->
    boardListE(E#xmlElement.content, ElementName, BoardType);
boardListE(#xmlElement{name = configuration} = E,
	   ElementName,
	   BoardType) ->
    boardListE(E#xmlElement.content, ElementName, BoardType);
boardListE([_ | Tail], ElementName, BoardType) ->
    boardListE(Tail, ElementName, BoardType);
boardListE([], _, {all, _}) ->
    [];
boardListE([], _, _) ->
    undefined.

%%% ###########################################################################
%%% boardType_all
%%%
%%% ###=====================================================================###
boardType_all(E) ->
    values(E, boardType, [productNumber, revision]).

%%% ###########################################################################
%%% is_valid_board_type
%%%
%%% ###=====================================================================###
is_valid_board_type({all, _}, _) ->
    true;
is_valid_board_type({_, _} = BoardType, BoardTypes) ->
    is_valid_board_type(BoardTypes, BoardType, false).

is_valid_board_type([{No, RevRange} | Tail], {No, Rev} = BoardType, false) ->
    put(checked_boardType_range, {No, RevRange}),
    is_valid_board_type(Tail,
			BoardType,
			sysUtil:is_rev_inRange(Rev, RevRange));
is_valid_board_type([{No, RevRange} | Tail],
		    {No, Rev} = BoardType,
		    true = Bool) ->
    case sysUtil:is_rev_inRange(Rev, RevRange) of
	true ->
	    Multiple = [erase(checked_boardType_range), {No, RevRange}],
	    ?LOG_ERR([?UC_ERR_BoardTypeDefinedMultipleTimes,
		      {boardType, BoardType},
		      {multiple_defs, Multiple},
		      {files, ?UP_FILES}]),
	    throw(?UC_ERR_2(?UC_ERR_BoardTypeDefinedMultipleTimes));
	false ->
	    is_valid_board_type(Tail, BoardType, Bool)
    end;
is_valid_board_type([_ | Tail], BoardType, Bool) ->
    is_valid_board_type(Tail, BoardType, Bool);
is_valid_board_type([], _, Bool) ->
    erase(checked_boardType_range),
    Bool.

%%% ###########################################################################
%%% list2tuple
%%%
%%% ###=====================================================================###
list2tuple([E]) ->
    E;
list2tuple(List) ->
    list_to_tuple(List).

%%% ###########################################################################
%%% lists_merge
%%%
%%% ###=====================================================================###
lists_merge([Elem1 | Tail1], List2) ->
    lists_merge(Tail1, [Elem1 | lists:delete(Elem1, List2)]);
lists_merge([], MergedList) ->
    MergedList.

%%% ###########################################################################
%%% boardType
%%%
%%% ###=====================================================================###
boardType(Attrs) ->
    try
	boardType(proplists:get_value(<<"productNumber">>, Attrs),
		  proplists:get_value(<<"productRevision">>, Attrs))
    catch
	_ : _ ->
	    try
		boardType(proplists:get_value(productNumber, Attrs),
			  proplists:get_value(productRevision, Attrs))
	    catch
		_ : _ ->
		    undefined
	    end
    end.

boardType({_, ProdNo}, {_, ProdRev}) ->
    boardType(binary_to_list(ProdNo), binary_to_list(ProdRev));
boardType(ProdNo, ProdRev) when is_list(ProdNo) andalso is_list(ProdRev) ->
    {sysUtil:remove_chars($ , ProdNo), ProdRev}.

%%% ###########################################################################
%%% products_clean
%%%
%%% ###=====================================================================###
products_clean([{{Name, Id, Rev}, _} = Product | Tail]) ->
    case products_keyfind(Tail, Name, Id) of
	false ->
	    [Product | products_clean(Tail)];
	{{_, _, Rev}, _} ->   % Same product, same revision
	    products_clean(Tail);
	{{_, _, RevTail}, _} ->   % Same product, another revision
	    case sysUtil:is_rev_higher(Rev, RevTail) of
		true ->
		    products_clean(Tail ++ [Product]);
		false ->
		    products_clean(Tail)
	    end
    end;
products_clean([]) ->
    [].

%%% ###########################################################################
%%% products_global
%%%
%%% ###=====================================================================###
products_global(ConfigE, BoardType) ->
    case boardListE(ConfigE, boardList, BoardType) of
	undefined ->
	    case boardListE(ConfigE, contentinfo, BoardType) of
		undefined ->
		    ?LOG_ERR([?UC_ERR_NoProductListInGlobalUP,
			      {files, ?UP_FILES}]),
		    throw(?UC_ERR_2(?UC_ERR_NoProductListInGlobalUP));
		{BoardList, ContentinfoE} ->
		    trace_add(swp_selected, {global, BoardList}),
		    {BoardList, boardList2products(ContentinfoE, global)}
	    end;
	{BoardList, BoardListE} ->
	    trace_add(swp_selected, {global, BoardList}),
	    {BoardList, boardList2products(BoardListE, global)}
    end.

%%% ###########################################################################
%%% products_hal
%%%
%%% ###=====================================================================###
products_hal([ConfigE | Tail], GlobalSwpId, HwSwCompIx, BoardType) ->
    case hwSwCompatibilityIndex(ConfigE) of
	HwSwCompIx ->
	    case boardListE(ConfigE, boardList, BoardType) of
		undefined ->
		    products_hal(Tail, GlobalSwpId, HwSwCompIx, BoardType);
		{BoardList, BoardListE} ->
		    trace_add(swp_selected, {hal, BoardList}),
		    trace_add(swp_selected, {hal_swp_id, swp_id(ConfigE)}),
		    {BoardList, boardList2products(BoardListE, hal), ConfigE}
	    end;
	[Ix | _] = HwSwCompIxs when is_list(Ix) ->
	    case lists:member(HwSwCompIx, HwSwCompIxs) of
		true ->
		    ?LOG_ERR([?UC_ERR_MultipleHwSwCompIndexInHalUP,
			      {indexList, HwSwCompIxs},
			      {files, ?UP_FILES}]),
		    throw(?UC_ERR_2(?UC_ERR_MultipleHwSwCompIndexInHalUP));
		false ->
		    products_hal(Tail, GlobalSwpId, HwSwCompIx, BoardType)
	    end;
	undefined ->
	    ?LOG_ERR([?UC_ERR_MissingHwSwCompIndexInHalUP,
		      {files, ?UP_FILES}]),
	    throw(?UC_ERR_2(?UC_ERR_MissingHwSwCompIndexInHalUP));
	_ ->
	    products_hal(Tail, GlobalSwpId, HwSwCompIx, BoardType)
    end;
products_hal([], _, _, _) ->
    {undefined, [], undefined}.

%%% ###########################################################################
%%% products_keyfind
%%%
%%% ###=====================================================================###
products_keyfind([{{Name, Id, _}, _} = Product | _], Name, Id) ->
    Product;
products_keyfind([_ | Tail], Name, Id) ->
    products_keyfind(Tail, Name, Id);
products_keyfind([], _, _) ->
    false.

%%% ###########################################################################
%%% products_main
%%%
%%% ###=====================================================================###
products_main(BoardType, GlobalUpFiles, HalUpFiles, BoardTypesRA) ->
    HalConfigEs = parse_configE(HalUpFiles),
    case parse_configE(GlobalUpFiles) of
	[GlobalConfigE] ->
	    Products =
		products_parsedUPs(BoardType,
				   GlobalConfigE,
				   swp_id(GlobalConfigE),
				   HalConfigEs,
				   BoardTypesRA),
	    write_swp_selected(),
	    ?LOG_INFO_ALL([{selected, erase(swp_selected)},
			   {files, ?UP_FILES} | Products]),
	    Products;
	[] ->
	    ?LOG_ERR([?UC_ERR_NoGlobalUP]),
	    throw(?UC_ERR_2(?UC_ERR_NoGlobalUP));
	[_ | _] ->
	    ?LOG_ERR([?UC_ERR_MoreThanOneGlobalUP,
		      {"GlobalUPs", GlobalUpFiles}]),
	    throw(?UC_ERR_2(?UC_ERR_MoreThanOneGlobalUP))
    end.

%%% ###########################################################################
%%% products_parsedUPs
%%%
%%% ###=====================================================================###
products_parsedUPs(BoardType, GlobalUP, GlobalSwpId, HalUPs, BoardTypesRA) ->
    GlobalIndex =
	case hwSwCompatibilityIndex(GlobalUP) of
	    [Char | _] = Ix when is_integer(Char) ->
		Ix;
	    undefined ->
		undefined;
	    [Ix | _] = IndexList when is_list(Ix) ->
		?LOG_ERR([?UC_ERR_MultipleHwSwCompIndexInGlobalUP,
			  {indexList, IndexList},
			  {files, ?UP_FILES}]),
		throw(?UC_ERR_2(?UC_ERR_MultipleHwSwCompIndexInGlobalUP))
	end,
    trace_add(swp_selected, {hwSwCompatibilityIndex, GlobalIndex}),
    trace_add(swp_selected, {global_swp_id, GlobalSwpId}),
    {GlProdsFrom, GlobalProducts} = products_global(GlobalUP, BoardType),
    case products_hal(HalUPs, GlobalSwpId, GlobalIndex, BoardType) of
	{HalProdsFrom, [_ | _] = HalProducts, HalUP} ->
	    validate_HwSwCompIndex(HalUPs, GlobalIndex),
  	    is_valid_board_type(BoardType, boardType_all(HalUP)), % Duplicates
  	    is_valid_board_type(BoardType, boardType_all(GlobalUP)),% Duplicates
	    RadioProducts =
		products_radio(HalProdsFrom, BoardTypesRA, GlobalUP),
	    Products =
		lists_merge(products_select(HalProducts,
					    GlobalProducts ++ RadioProducts),
			    RadioProducts),
	    products_clean(Products);
	{_, [], _} ->
	    BTsGlobal = boardType_all(GlobalUP),
	    BTsHal = lists:flatten([boardType_all(HalUP) || HalUP <- HalUPs]),
	    case {GlProdsFrom, GlobalIndex, BTsGlobal} of
		{{boardList, _}, [_ | _], _} ->
		    is_valid_board_type(BoardType, BTsGlobal),   % Duplicates
		    Products =
			lists_merge(GlobalProducts,
				    products_radio(GlProdsFrom,
						   BoardTypesRA,
						   GlobalUP)),
		    products_clean(Products);
		{contentinfo, undefined, []} ->
		    %% Legacy Global UP file.
		    GlobalProducts;
		{_, undefined, _} ->
		    ?LOG_ERR([?UC_ERR_MissingHwSwCompIndexInGlobalUP,
			      {boardType, BoardType},
			      {supported_global, BTsGlobal},
			      {supported_hal, BTsHal},
			      {global_products_from, GlProdsFrom},
			      {files, ?UP_FILES}]),
		    throw(?UC_ERR_2(?UC_ERR_MissingHwSwCompIndexInGlobalUP));
		{contentinfo, _, _} ->
		    Reason = ?UC_ERR_BoardTypeNotSupportedByNeitherHalGlobalUP,
		    ?LOG_ERR([Reason,
			      {boardType, BoardType},
			      {supported_global, BTsGlobal},
			      {supported_hal, BTsHal},
			      {global_products_from, GlProdsFrom},
			      {files, ?UP_FILES}]),
		    throw(?UC_ERR_2(Reason))
	    end
    end.

%%% ###########################################################################
%%% products_radio
%%%
%%% ###=====================================================================###
products_radio({boardList, {"BASEBAND", _}}, all, GlobalUP) ->
    BoardLists = boardListE(GlobalUP, boardList, {all, "RADIO"}),
    lists:flatten([begin
		       trace_add(swp_selected, {global, BoardList}),
		       boardList2products(BoardListE, global)
		   end
		   || {BoardList, BoardListE} <- BoardLists]);
products_radio({boardList, {"BASEBAND", _}}, BoardTypesRA, GlobalUP) ->
    BoardLists =
	lists:flatten([boardListE(GlobalUP, boardList, BoardType)
		       || BoardType <- BoardTypesRA]),
    lists:flatten([begin
		       trace_add(swp_selected, {global, BoardList}),
		       boardList2products(BoardListE, global)
		   end
		   || {BoardList, BoardListE} <- BoardLists]);
products_radio(_, _, _) ->
    [].

%%% ###########################################################################
%%% products_select
%%%
%%% ###=====================================================================###
products_select([{{Name, Id, "*"}, _} = P | Tail], GlobalProducts) ->
    case products_keyfind(GlobalProducts, Name, Id) of
	false ->
	    ?LOG_ERR([?UC_ERR_ProductNotFoundInSelBoardListInGlobalUP,
		      {product, P},
		      {selected, erase(swp_selected)},
		      {files, ?UP_FILES}]),
	    throw(?UC_ERR_2(?UC_ERR_ProductNotFoundInSelBoardListInGlobalUP));
	Product ->
	    [Product | products_select(Tail, GlobalProducts)]
    end;
products_select([Product | Tail], GlobalProducts) ->
    [Product | products_select(Tail, GlobalProducts)];
products_select([], _) ->
    [].

%%% ###########################################################################
%%% parse_configE
%%%
%%% ###=====================================================================###
parse_configE([File | Tail]) when is_list(File) ->
    [parse_configE(File) | parse_configE(Tail)];
parse_configE([]) ->
    [];
parse_configE(File) ->
    {ConfigE, []} = xmerl_scan:file(File),
    ConfigE.

%%% ###########################################################################
%%% swp_id
%%%
%%% ###=====================================================================###
swp_id(#xmlElement{content = ContentE}) ->
    [ProductE] = [E || E <- ContentE, E#xmlElement.name == product],
    xmlAttrVal(name, ProductE) ++
	"_" ++
	xmlAttrVal(id, ProductE) ++
	"_" ++
	xmlAttrVal(version, ProductE);
swp_id([Char | _] = MetadataPath) when is_integer(Char) ->
    lists:last(filename:split(filename:dirname(MetadataPath))).

%%% ###########################################################################
%%% swp_metadata_files
%%%
%%% ###=====================================================================###
swp_metadata_files(global, Paths) ->
    Files = swp_metadata_wildcard(swp_metadata_path(global, Paths),
				  ?SWP_METADATA_filenames_global),
    [swmI:find_file(File) || File <- Files];
swp_metadata_files(hal, Paths) ->
    Files =
	[swmI:find_file(File)
	 || File <- swp_metadata_wildcard(swp_metadata_path(hal, Paths),
					  ?SWP_METADATA_filenames_hal)],
    PatchedHalFiles =
	filelib:wildcard(filename:join(sysEnv:dev_patches_dir(),
				       ?SWP_METADATA_filenames_hal)),
    lists_merge(PatchedHalFiles, Files).

%%% ###########################################################################
%%% swp_metadata_path
%%%
%%% ###=====================================================================###
swp_metadata_path(global, [Char | _] = Path) when is_integer(Char) ->
    Path;
swp_metadata_path(global, {MetadataType, Path})
  when MetadataType == global orelse MetadataType == all ->
    Path;
swp_metadata_path(global, _) ->
    ?SWP_METADATA_path_global;
swp_metadata_path(hal, {MetadataType, Path})
  when MetadataType == hal orelse MetadataType == all ->
    Path;
swp_metadata_path(hal, _) ->
    ?SWP_METADATA_paths_hal.

%%% ###########################################################################
%%% swp_metadata_wildcard
%%%
%%% ###=====================================================================###
swp_metadata_wildcard([Char | _] = Path, WildcardedFileName)
  when is_integer(Char) ->
    filelib:wildcard(filename:join(Path, WildcardedFileName));
swp_metadata_wildcard([Path | Tail], WildcardedFileName) when is_list(Path) ->
    swp_metadata_wildcard(Path, WildcardedFileName) ++
	swp_metadata_wildcard(Tail, WildcardedFileName);
swp_metadata_wildcard([], _) ->
    [].

%%% ###########################################################################
%%% trace_add
%%%
%%% ###=====================================================================###
trace_add(Tag, Data) ->
    case get(Tag) of
	undefined ->
	    put(Tag, [Data]);
	OldData ->
	    put(Tag, OldData ++ [Data])
    end.

%%% ###########################################################################
%%% validate_HwSwCompIndex
%%%
%%% ###=====================================================================###
validate_HwSwCompIndex(UPs, Index) ->
    case
	%% Index must not be duplicated:
	lists:member(Index, lists:delete(Index,
					 values(UPs,
						hwSwCompatibilityIndex)))
	of
	false ->
	    ok;
	true ->
	    ?LOG_ERR([{?UC_ERR_DuplicateIndexInHalUP, Index},
		      {files, ?UP_FILES}]),
	    throw(?UC_ERR_2(?UC_ERR_DuplicateIndexInHalUP))
    end.

%%% ###########################################################################
%%% write_swp_selected
%%%
%%% ###=====================================================================###
write_swp_selected() ->
    ok = file:write_file(filename:join(swmLib:swm_dir(), swp_selected),
			 erlang:term_to_binary(get(swp_selected))).

%%% ###########################################################################
%%% xmlAttrVal
%%%
%%% #           xmlAttrVal(AttributeName, Element)
%%% #           xmlAttrVal(AttributeName, AttributeList)
%%% Input: AttributeName:atom()
%%%        Element:#xmlElement{} or
%%%        AttributeList:[#xmlattribute{}] a list of xml attributes
%%% Output: Value:string()
%%% Exceptions: erlang:error invoked if attribute does not exist
%%% Description: Finds an attribute to an xml element, or in a list of
%%%              attributes and returns the value of the attribute
%%% ###=====================================================================###
xmlAttrVal(AttrName, Element) when is_record(Element, xmlElement) ->
    xmlAttrVal(AttrName, Element#xmlElement.attributes);
xmlAttrVal(AttrName, AttrList) ->
    case lists:keysearch(AttrName, #xmlAttribute.name, AttrList) of
        {value, Attribute} ->
            Attribute#xmlAttribute.value;
        false ->
	    throw(?UC_ERR_3(?UC_ERR_MandatoryInfoMissing,
			    ?RepInfo([{not_found, {xmlAttribute, AttrName}},
				      {attributes, AttrList}])))
    end.

%%% ###########################################################################
%%% xmlAttrVals
%%%
%%% ###=====================================================================###
xmlAttrVals([#xmlAttribute{value = Value} | Tail]) ->
    [Value | xmlAttrVals(Tail)];
xmlAttrVals(#xmlElement{} = E) ->
    xmlAttrVals(E#xmlElement.attributes);
xmlAttrVals([_ | Tail]) ->
    xmlAttrVals(Tail);
xmlAttrVals([]) ->
    [].

%%% ###########################################################################
%%% xmlAttrVal_optional
%%%
%%% ###=====================================================================###
xmlAttrVal_optional(AttrName, Element) ->
    try
	xmlAttrVal(AttrName, Element)
    catch
	throw : ?UC_ERR_3(?UC_ERR_MandatoryInfoMissing, _) ->
	    undefined
    end.

%%% ###########################################################################
%%% xmlElemRec_all
%%%
%%% ###=====================================================================###
xmlElemRec_all(ElemName, Element) when is_record(Element, xmlElement) ->
    xmlElemRec_all(ElemName, Element#xmlElement.content);
xmlElemRec_all(ElemName, ContentList) ->
    case sysUtil:keyfind_all(ElemName, #xmlElement.name, ContentList) of
	Elements when is_list(Elements) ->
	    Elements;
	false ->
	    []
    end.

%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 4     CODE FOR TEMPORARY CORRECTIONS
%%% ###---------------------------------------------------------------------###
