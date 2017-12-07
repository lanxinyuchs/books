%%% ###===###===###===###===###===###===###===###===###===###===###===###===###
%%% # 0.    BASIC INFORMATION
%%% ###---------------------------------------------------------------------###
%%% ###=====================================================================###
%%% # 0.1   MODULE INFORMATION
%%% ###---------------------------------------------------------------------###
%%% %CCaseFile:	swmBoardList.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2015-2017
%%% @version /main/R5A/R6A/R8A/R9A/R10A/R11A/3

%%% @doc ==Header==
%%%   Handling of HAL (Hardware Abstraction Layer) product lists.
%%% @end

%%% ###=====================================================================###
%%% # 0.2   MODULE DEFINITION
%%% ###---------------------------------------------------------------------###
-module(swmBoardList).
-vsn('/main/R5A/R6A/R8A/R9A/R10A/R11A/3').
-date('2017-10-18').
-author('etxberb').

%%% ###=====================================================================###
%%% # 0.3   LEGAL RIGHTS
%%% ###---------------------------------------------------------------------###
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%% R5A/21  2016-03-29 etxberb  Changed metadataPaths() to metaOpts() and added
%%                             possibility to give a list of options.
%% R5A/23  2016-04-14 etxberb  Changed xml tag name hwSwCompatibilityIndex to
%%                             hwSwCompatibility.
%% R5A/24  2016-04-15 etxberb  Bug fix in read_swp_selected.
%% ----    ---------- -------  ------------------------------------------------
%% R6A/1   2016-05-03 etxberb  Changed "RADIO" to "OTHER".
%% R6A/2   2016-05-04 etxberb  Added back "RADIO" for backwards compatibility.
%% R6A/3   2016-09-01 etxberb  Allowing productNumbers to contain spaces in
%%                             *.xml.
%% R8A/1   2016-11-21 etxberb  Added bundled_products/2.
%% R8A/2   2016-12-12 etxberb  Added boardtype_format/1.
%% R8A/3   2016-12-16 etxberb  Added swp/2, swp_id_current/0, swp_id_format/1.
%% R8A/4   2016-12-29 etxberb  Added boardTypeOth_xxx/1.
%%                             MR37329 HW Sensitive Install implemented.
%% ----    ---------- -------  ------------------------------------------------
%% R9A/1   2017-03-07 etxberb  Added validate_node_type/1.
%% R9A/2   2017-03-13 etxberb  Changed from error to warning for
%%                             BoardTypeNotSupportedByNeitherHalGlobalUP.
%% R9A/3   2017-03-21 etxberb  Changed validate_board_types/3 to
%%                             validate_board_type_duplicate/2.
%% R9A/7   2017-05-30 etxberb  Added archiveSWPs/0 & /1, archiveCXPs/0 & /1.
%% R9A/8   2017-05-31 etxberb  Fixed dialyzer problem.
%% R9A/9   2017-06-17 etxberb  Added swp_id_global/1.
%% ----    ---------- -------  ------------------------------------------------
%% R10A/1  2017-07-03 etxjotj  Removed large printout
%% R10A/2  2017-07-04 etxjotj  Fixed dialyzer warnings
%% R10A/3  2017-07-25 etxberb  Added back printouts with reduced size.
%% R10A/4  2017-07-26 etxberb  Fixed dialyzer problem.
%% R10A/5  2017-08-03 etxberb  boardTypes_oth/0 returns [] instead of 'all' when
%%                             no CAT  registrated boards.
%% R10A/6  2017-08-31 etxberb  Bug fix in read_swp_selected.
%% R10A/7  2017-09-06 etxberb  Logging enhancement.
%% ----    ---------- -------  ------------------------------------------------
%% R11A/1  2017-10-05 etxberb  Added check of <bundle/> in bundled_products_parse/1.
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
-export([archiveCXPs/0,
	 archiveCXPs/1,
	 archiveSWPs/0,
	 archiveSWPs/1,
	 boardType/0,
	 boardTypes/0,
	 boardTypes_oth/0,
	 boardTypes_oth_ok/0,
	 boardTypeOth_add/1,
	 boardTypeOth_st/1,
	 boardTypeOth_sw/1,
	 boardTypeOth_swNok/1,
	 boardTypeOth_swOk/1,
	 boardTypeOth_swOk/2,
	 bundled_products/2,
	 hwSwCompatibilityIndex/1,
	 products/1,
	 products/2,
	 read_swp_selected/0,
	 read_swp_selected/1,
	 swp/2,
	 swp_id_current/0,
	 swp_id_format/1,
	 swp_id_global/1]).

%%% ###=====================================================================###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
-export([all_hw/0]).
-export([values/2,
	 values/3]).

%%% For test purposes
-export([board_added/3]).

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###
-include("SwmInternal.hrl").
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

-define(default_BoardType, {"undefined", "R0A"}).

%% Usecase Error
-define(UC_ERR_2(Reason),             {uc_error, Reason}).
-define(UC_ERR_3(Reason, ReportInfo), {uc_error, Reason, ReportInfo}).

-define(ERR_UnrecognizedArg,
	"Unrecognized argument").
-define(ERR_SoftwareFault,
	"Software fault").
%% Reason for Usecase Error
-define(UC_ERR_ParsingFault,
	"Parsing fault").
-define(UC_ERR_MoreThanOneBoardTypeSpecForBB,
	"More than one BoardType specified for Baseband").
-define(UC_ERR_NoGlobalUP,
	"No GlobalUP found").
-define(UC_ERR_MoreThanOneGlobalUP,
	"More than one GlobalUP found").
-define(UC_ERR_MultipleHwSwCompIndexInGlobalUP,
	"Multiple hwSwCompatibility Index found in Global UP file").
-define(UC_ERR_MissingHwSwCompIndexInGlobalUP,
	"Missing hwSwCompatibility Index in Global UP").
-define(UC_ERR_BoardTypeNotSupportedByNeitherHalGlobalUP,
	"BoardType not supported by neither HalUP nor GlobalUP").
-define(UC_ERR_NoProductListInGlobalUP,
	"No product list found in Global UP").
-define(UC_ERR_MultipleHwSwCompIndexInHalUP,
	"Multiple hwSwCompatibility Index found in Hal UP file").
-define(UC_ERR_MissingHwSwCompIndexInHalUP,
	"Missing hwSwCompatibility Index in Hal UP file").
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
-define(UC_ERR_NodeTypeNotValid,
	"Node Type in -up.xml is not valid").
-define(UC_ERR_NodeTypeMissing,
	"Node Type in -up.xml is missing").

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###

%%% ###=====================================================================###
%%% # 2.6   LOCAL DEFINITION OF TYPES
%%% ###---------------------------------------------------------------------###
-type products() :: [{{Name :: string(), Id :: string(), Version :: string()},
		      {Source :: global | hal, Filename :: string()}}].

-type boardType()     :: {ProductNumber :: string(), Revision :: string()}.
-type boardTypes()    :: boardType_bb() | {boardType_bb(), boardType_oth()}.
-type boardType_bb()  :: (boardType() |     % hwcategory="BASEBAND" |
			  [boardType()] |   % hwcategory="BASEBAND-T"
			  undefined).
-type boardType_oth() :: [boardType()] | all.   % hwcategory="OTHER"
-type boardType_swState() :: ok | nok | pending | undefined.

-type metaOpt() :: (metadataPath_global() |
		    metadataPath_hal() | % global from swmLib:software_dir
		    metadataPath_all() |
		    undefined).          % global from swmLib:software_dir
-type metaOpts() :: list(metaOpt() |
			 {swp_selected, keep}).
-type metadataPath_all()    :: {all, Path :: string()}.
-type metadataPath_global() :: Path :: string() | {global, Path :: string()}.
-type metadataPath_hal()    :: {hal, Path :: string()}.

-type swpItem()      :: {swpItemKey(), swpItemValue()}.
-type swpItemKey()   :: (products |
			 hwSwCompatibility |
			 swp_id |
			 boardList |
			 contentinfo |
			 files |
			 boardTypeBB).
-type swpItemValue() :: list().
-type swpParam()     :: ({boardTypeBB, boardType()} |
			 {boardTypesOTH, boardType_oth()} |
			 {options, metaOpts()}).

-type uc_err_reason() :: string().
-type uc_err_2()      :: {uc_error,
			  Reason :: uc_err_reason()}.

%-type exception_throw(_) :: none().   % erlang:throw(uc_err_2())

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
%%% @doc Returns the CXP file names of current UP:s archive.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec archiveCXPs() ->
    list().
%%% ###=====================================================================###
archiveCXPs() ->
    archiveCXPs(swmI:get_current_archive_dir()).

%%% ###########################################################################
%%% @doc Returns the CXP file names of a specific archive.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec archiveCXPs(ArchiveDir :: string()) ->
    list().
%%% ###=====================================================================###
archiveCXPs(ArchiveDir) ->
    [filename:basename(AFile)
     || AFile <- filelib:wildcard(filename:join(ArchiveDir, "*.cxp"))].

%%% ###########################################################################
%%% @doc Returns the SWP Ids of current UP:s archive.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec archiveSWPs() ->
    list().
%%% ###=====================================================================###
archiveSWPs() ->
    archiveSWPs(swmI:get_current_archive_dir()).

%%% ###########################################################################
%%% @doc Returns the SWP Ids of a specific archive.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec archiveSWPs(ArchiveDir :: string()) ->
    list().
%%% ###=====================================================================###
archiveSWPs(ArchiveDir) ->
    ACxpFiles = archiveCXPs(ArchiveDir),
    try
	{ok, [{products, Products}]} = swp([products],
					   [{boardTypeBB, boardType()},
					    {boardTypesOTH, all},
					    {options, [{global, ArchiveDir}]}]),
	[swp_id_format(NameIdRev) || {NameIdRev, {_Source, File}}
					 <- Products,
				     lists:member(File, ACxpFiles)]
    catch
	ExcClass : ExcReason ->
	    ?LOG_WARN([{ExcClass, ExcReason}]),
	    []
    end.

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
%%% @doc Get all registered boardTypes of hwcategory="OTHER" regardless of
%%%   state.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec boardTypes_oth() ->
    list(boardType()) |
	all.   % all :: We assume legacy CAT and thus the need for having
						% all "OTHER" SW installed.
						% I.e. HW sensitive install is
						% disabled.
boardTypes_oth() ->
    case swmLib:get_variable(hsi_state) of
	on ->
	    try
		boardTypes_oth(ets:first(swmBoardOth))
	    catch
		_ : _ ->
		    []
	    end;
	_ ->
	    all
    end.

boardTypes_oth('$end_of_table') ->
    [];
boardTypes_oth(Key) ->
    [Key | boardTypes_oth(ets:next(swmBoardOth, Key))].

%%% ###########################################################################
%%% @doc Get all registered boardTypes of hwcategory="OTHER" with state = ok.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec boardTypes_oth_ok() ->
    list(boardType()) |
	all.   % all :: CAT has not registered any boards. We assume legacy CAT
						% and thus the need for having
						% all "OTHER" SW installed.
						% I.e. HW sensitive install is
						% disabled.
boardTypes_oth_ok() ->
    case boardTypes_oth() of
	BTs when is_list(BTs) ->
	    case [BT || BT <- BTs, is_BT_OTH_ok(BT)] of
		[] ->
		    all;
		BTsOK ->
		    BTsOK
	    end;
	BTs ->
	    BTs
    end.

%%% ###########################################################################
%%% @doc Add a BoardType if not already added.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec boardTypeOth_add(BoardType :: boardType()) ->
    boardType_swState().
boardTypeOth_add(BT) ->
    BoardType = boardtype_format(BT),
    try
	[#swmBoardOth{sw_state = SwState}] = ets:lookup(swmBoardOth, BoardType),
	SwState
    catch
	_ : _ ->
	    Sw_State = pending,
	    boardTypeOth_insert(#swmBoardOth{boardType = BoardType,
					     sw_state = Sw_State}),
	    Sw_State
    end.

%%% ###########################################################################
%%% @doc Get the software state for a BoardType.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec boardTypeOth_st(BoardType :: boardType()) ->
    boardType_swState().
boardTypeOth_st(BT) ->
    BoardType = boardtype_format(BT),
    try
	[#swmBoardOth{sw_state = SwState}] = ets:lookup(swmBoardOth, BoardType),
	SwState
    catch
	_ : _ ->
	    undefined
    end.

%%% ###########################################################################
%%% @doc Get the software list for a BoardType.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec boardTypeOth_sw(BoardType :: boardType()) ->
    list(string()).
boardTypeOth_sw(BT) ->
    BoardType = boardtype_format(BT),
    try
	[#swmBoardOth{sw = Sw}] = ets:lookup(swmBoardOth, BoardType),
	Sw
    catch
	_ : _ ->
	    []
    end.

%%% ###########################################################################
%%% @doc Set the software state = nok for a BoardType.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec boardTypeOth_swNok(BoardType :: boardType()) ->
    nok.
boardTypeOth_swNok(BT) ->
    BoardType = boardtype_format(BT),
    SwState = nok,
    boardTypeOth_insert(#swmBoardOth{boardType = BoardType,
				     sw_state = SwState}),
    SwState.

%%% ###########################################################################
%%% @doc Set the software state = ok for a BoardType.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec boardTypeOth_swOk(BoardType :: boardType()) ->
    ok.
boardTypeOth_swOk(BT) ->
    boardTypeOth_swOk(BT, []).

-spec boardTypeOth_swOk(BoardType :: boardType(),
			Software  :: list(string())) ->
    ok.
boardTypeOth_swOk(BT, SW) ->
    BoardType = boardtype_format(BT),
    SwState = ok,
    boardTypeOth_insert(#swmBoardOth{boardType = BoardType,
				     sw = SW,
				     sw_state = SwState}),
    SwState.

%%% ###########################################################################
boardTypeOth_insert(SwmBoardOth) ->
    try
	ets:insert(swmBoardOth, SwmBoardOth)
    catch
	_ : _ ->
	    ets:new(swmBoardOth, [public,
				  named_table,
				  ordered_set,
				  {keypos, 2},
				  {heir, whereis(swmServer), []}]),
	    boardTypeOth_insert(SwmBoardOth)
    end.

%%% ###########################################################################
%%% @doc Returns the product list for unpacked bundled products.
%%%
%%% @end
%%% ###=====================================================================###
bundled_products(Dir, Products) ->
    Bundles = filelib:wildcard(filename:join(Dir, "cxp*.xml")),
    BundledProducts = bundled_products_parse(Bundles),
    bundled_products_match(BundledProducts, Products).

%%% ###########################################################################
bundled_products_parse([Bundle | Tail]) ->
    {ConfigE, []} = xmerl_scan:file(Bundle),
    case xmlElemRec_all(bundle, ConfigE) of
	[_] ->
	    [ContentInfoE] = xmlElemRec_all(contentinfo, ConfigE),
	    Source = undefined,
	    Products =
		[{{xmlAttrVal(name, ProdE),
		   xmlAttrVal(id, ProdE),
		   xmlAttrVal(version, ProdE)},
		  {Source, xmlAttrVal(filename, ProdE)}}
		 || ProdE <- ContentInfoE#xmlElement.content,
		    ProdE#xmlElement.name == product],
	    [{product_attrs(ConfigE), Products} | bundled_products_parse(Tail)];
	[] ->
	    bundled_products_parse(Tail)
    end;
bundled_products_parse([]) ->
    [].

%%% ###########################################################################
bundled_products_match([{Name_Id_Version, B_Products} | Tail], Products) ->
    case lists:keyfind(Name_Id_Version, 1, Products) of
	{Name_Id_Version, {Source, _}} ->
	    [{NIV, {Source, File}} || {NIV, {_, File}} <- B_Products] ++
		bundled_products_match(Tail, Products);
	false ->
	    bundled_products_match(Tail, Products)
    end;
bundled_products_match([], _) ->
    [].

%%% ###########################################################################
%%% @doc Returns the hwSwCompatibilityIndex for a specified metadata config.
%%%
%%% @end
%%% ###=====================================================================###
hwSwCompatibilityIndex(ConfigE) ->
    case xmlElemRec_all(hwSwCompatibility, ConfigE) of
	[HwSwCompatibilityE] ->
	    xmlAttrVal(index, HwSwCompatibilityE);
	[_ | _] = HwSwCompatibilityEs ->
	    [xmlAttrVal(index, HwSwCompatibilityE)
	     || HwSwCompatibilityE <- HwSwCompatibilityEs];
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
	none().	      
	%% exception_throw(uc_err_2()).
%%% ###=====================================================================###
products(BoardTypes) ->
    products(BoardTypes, undefined).

%%% ----------------------------------------------------------
-spec products(BoardTypes      :: boardTypes(),
	       MetadataOptions :: metaOpt() | metaOpts()) ->
    {ok, products()} |
	uc_err_2() |
	none().
	%% exception_throw(uc_err_2()).
%%% ###=====================================================================###
products({[Char | _], _} = BoardTypeBB, MetaOpts) when is_integer(Char) ->
    products(BoardTypeBB, all, MetaOpts);
products([BoardTypeBB], MetaOpts) ->
    products(BoardTypeBB, all, MetaOpts);
products({{[Char | _], _} = BoardTypeBB, BoardTypesOTH}, MetaOpts)
  when is_integer(Char) ->
    products(BoardTypeBB, BoardTypesOTH, MetaOpts);
products({undefined = BoardTypeBB, BoardTypesOTH}, MetaOpts) ->
    products(BoardTypeBB, BoardTypesOTH, MetaOpts);
products(undefined = BoardTypeBB, MetaOpts) ->
    products(BoardTypeBB, all, MetaOpts);
products({[BoardTypeBB], BoardTypesOTH}, MetaOpts) ->
    products({BoardTypeBB, BoardTypesOTH}, MetaOpts);
products([], MetaOpts) ->
    products(?default_BoardType, MetaOpts);
products({[], BoardTypesOTH}, MetaOpts) ->
    products({?default_BoardType, BoardTypesOTH}, MetaOpts);
products(UnrecognizedArg, _) ->
    ?LOG_ERR([?ERR_UnrecognizedArg, {argument, UnrecognizedArg}]),
    throw(?UC_ERR_2(?ERR_UnrecognizedArg)).

%%% ###########################################################################
%%% products
%%%
%%% ###=====================================================================###
products(BT, BTsOTH, [E | _] = MetaOpts) when not is_integer(E) ->
    BoardType = boardtype_format(BT),
    BoardTypesOTH = boardtype_format(BTsOTH),
    GlobalUpFiles = swp_metadata_files(global, MetaOpts),
    HalUpFiles = swp_metadata_files(hal, MetaOpts),
    put(globalUpFiles, GlobalUpFiles),   % Used for error logging.
    put(halUpFiles, HalUpFiles),         % Used for error logging.
    try
	{ok, products_main(BoardType,
			   GlobalUpFiles,
			   HalUpFiles,
			   BoardTypesOTH,
			   MetaOpts)}
    catch
	throw : ?UC_ERR_2(Reason) ->
	    ?UC_ERR_2(Reason);
        throw : ?UC_ERR_3(Reason, ReportInfo) ->
	    ?LOG_ERR_ALL([Reason | ReportInfo ++
			  [{selected, get(swp_selected)}]]),
	    ?UC_ERR_2(Reason);
        exit : {fatal, Details} ->   % Exception from xmerl_scan.
	    ?LOG_ERR_ALL([?UC_ERR_ParsingFault,
			  {fatal, Details},
			  {selected, get(swp_selected)}]),
	    ?UC_ERR_2(?UC_ERR_ParsingFault)
    after
	swp_selected_erase(MetaOpts)
    end;
products(BoardType, BoardTypesOTH, MetaOpt) ->
    products(BoardType, BoardTypesOTH, [MetaOpt]).

%%% ###########################################################################
%%% read_swp_selected
%%%
%%% ###=====================================================================###
read_swp_selected() ->
    read_swp_selected(all).

read_swp_selected(all) ->
    {ok, []} = swp([], [{boardTypeBB, swmBoardList:boardType()}]),   % Generate!
    case file:read_file(filename:join(swmLib:swm_dir(), swp_selected)) of
	{ok, Bin} ->
	    erlang:binary_to_term(Bin);
	Error ->
	    ?LOG_WARN([Error]),
	    []
    end;
read_swp_selected(AttrName) when is_atom(AttrName) ->
    {ok, [{AttrName, Result}]} =
	swp([AttrName], [{boardTypeBB, swmBoardList:boardType()}]),
    Result;
read_swp_selected(AttrNames) when is_list(AttrNames) ->
    {ok, Result} = swp(AttrNames, [{boardTypeBB, swmBoardList:boardType()}]),
    Result.

%%% ###########################################################################
%%% swp
%%%
%%% ###=====================================================================###
-spec swp(SwpItemKeys :: list(swpItemKey()),
	  SwpParams   :: list(swpParam())) ->
    {ok, list(swpItem())} |
	{error, Reason :: string()}.
swp(SwpItems, SwpParams) ->
    try products(swp_boardTypes(SwpParams),
		 [{swp_selected, keep} | swp_metaOpts(SwpParams)])
	of
	{ok, Products} ->
	    {ok, swp_items(SwpItems, Products)};
	?UC_ERR_2(Reason) ->
	    {error, Reason}
    catch
	throw : ?UC_ERR_2(Reason) ->
	    ?LOG_INFO([{swpItems, SwpItems},
		       {swpParams, SwpParams}]),
	    {error, Reason};
	  throw : ?UC_ERR_3(Reason, ReportInfo) ->
	    ?LOG_ERR_ALL([Reason
			  | ReportInfo ++ [{swpItems, SwpItems},
					   {swpParams, SwpParams}]]),
	    {error, Reason};
	  ErrClass : ErrReason ->
	    StackTrace = erlang:get_stacktrace(),
	    Reason = ?ERR_SoftwareFault,
	    ?LOG_ERR_ALL([Reason,
			  {ErrClass, ErrReason},
			  {stackTrace, StackTrace}]),
	    {error, Reason}
    after
	erase(swp_selected)
    end.

%%% ###=====================================================================###
swp_items([products | Tail], Products) ->
    [{products, Products} | swp_items(Tail, Products)];
swp_items([SwpItem | Tail], Products) ->
    SwpItemValue = proplists:get_value(SwpItem, get(swp_selected), []),
    [{SwpItem, SwpItemValue} | swp_items(Tail, Products)];
swp_items([], _) ->
    [].

%%% ###########################################################################
%%% swp_id_format
%%%
%%% ###=====================================================================###
swp_id_format({Name, Id, Ver}) when is_list(Name) ->
    Name ++ "_" ++ Id ++ "_" ++ Ver;
swp_id_format({{Name, _, _} = SwpId, {_, _}}) when is_list(Name) ->
    swp_id_format(SwpId);
swp_id_format([{SwpId, _} | Tail]) ->
    [swp_id_format(SwpId) | swp_id_format(Tail)];
swp_id_format([]) ->
    [].

%%% ###########################################################################
%%% swp_id_global
%%%
%%% ###=====================================================================###
swp_id_global([{global, SwpId} | _]) ->
    SwpId;
swp_id_global([_ | Tail]) ->
    swp_id_global(Tail);
swp_id_global([]) ->
    "".

%%% ###########################################################################
%%% swp_boardTypes
%%%
%%% ###=====================================================================###
swp_boardTypes(SwpParams) ->
    swpParams_validate(SwpParams),
    BoardTypeBB = proplists:get_value(boardTypeBB, SwpParams, undefined),
    BoardTypesOTH = proplists:get_value(boardTypesOTH, SwpParams, []),
    {BoardTypeBB, BoardTypesOTH}.

%%% ###########################################################################
%%% swp_metaOpts
%%%
%%% ###=====================================================================###
swp_metaOpts(SwpParams) ->
    proplists:get_value(options, SwpParams, []).

%%% ###########################################################################
%%% swp_id_current
%%%
%%% ###=====================================================================###
swp_id_current() ->
    UPmeta = swmLib:get_current_up_metadata(),
    Name = proplists:get_value(productName, UPmeta),
    Id = proplists:get_value(productNumber, UPmeta),
    Ver = proplists:get_value(productRevision, UPmeta),
    Name ++ "_" ++ Id ++ "_" ++ Ver.

%%% ###########################################################################
%%% swp_selected_erase
%%%
%%% ###=====================================================================###
swp_selected_erase(MetaOpts) ->
    case lists:keyfind(swp_selected, 1, MetaOpts) of
	{swp_selected, keep} ->
	    get(swp_selected);
	_ ->
	    erase(swp_selected)
    end.

%%% ###########################################################################
%%% swpParams_validate
%%%
%%% ###=====================================================================###
swpParams_validate([{Tag, _} | Tail]) when Tag == boardTypeBB orelse
					   Tag == boardTypesOTH orelse
					   Tag == options ->
    swpParams_validate(Tail);
swpParams_validate([{Tag, _} | _]) ->
    throw(?UC_ERR_3(?ERR_UnrecognizedArg, [{invalid_param, Tag}]));
swpParams_validate([]) ->
    ok.

%%% ###=====================================================================###
%%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% ###########################################################################
%%% value
%%%
%%% ###=====================================================================###
value(Element) when is_record(Element, xmlElement) ->
    [Text] = Element#xmlElement.content,
    Text#xmlText.value.

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

%%% ###########################################################################
%%% board_added
%%%
%%% ###=====================================================================###
board_added(ProdNo, ProdRev, Res) ->
    ?LOG_INFO([{prodNo, ProdNo}, {prodRev, ProdRev}, {res, Res}]),
    Res.

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
		{{all, "OTHER"}, {"RADIO", _}} ->
		    %% For backwards compatibility. May be removed later...
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
	   {_, _}) ->
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
    [boardtype_format(BT)
     || {_PN, _Rev} = BT <- values(E, boardType, [productNumber, revision])].

%%% ###########################################################################
%%% is_BT_OTH_ok
%%%
%%% ###=====================================================================###
is_BT_OTH_ok(BT) ->
    case boardTypeOth_st(BT) of
	ok ->
	    true;
	_ ->
	    false
    end.

%%% ###########################################################################
%%% is_valid_board_type
%%%
%%% ###=====================================================================###
is_valid_board_type({all, _}, _) ->
    true;
is_valid_board_type({_, _} = BoardType, BoardTypes) ->
    is_validBoardType(BoardTypes, BoardType, false);
is_valid_board_type(undefined, _) ->
    false.

%%% ###=====================================================================###
validate_board_type_duplicate(BoardTypeBB, BoardTypes) ->
    is_valid_board_type(BoardTypeBB, BoardTypes).

%%% ###=====================================================================###
is_validBoardType([{No, RevRange} | Tail], {No, Rev} = BoardType, false) ->
    put(checked_boardType_range, {No, RevRange}),
    is_validBoardType(Tail,
		      BoardType,
		      sysUtil:is_rev_inRange(Rev, RevRange));
is_validBoardType([{No, RevRange} | Tail],
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
	    is_validBoardType(Tail, BoardType, Bool)
    end;
is_validBoardType([_ | Tail], BoardType, Bool) ->
    is_validBoardType(Tail, BoardType, Bool);
is_validBoardType([], _, Bool) ->
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
%%% boardList_isDupl
%%%
%%% ###=====================================================================###
boardList_isDupl([{Source, {_, HwMod}} | _], Source, HwMod) ->
    true;
boardList_isDupl([_ | Tail], Source, HwMod) ->
    boardList_isDupl(Tail, Source, HwMod);
boardList_isDupl([], _, _) ->
    false.

%%% ###########################################################################
%%% boardList_remDupl
%%%
%%% ###=====================================================================###
boardList_remDupl([{Source, {_, HwMod}} | Tail], Source, HwMod) ->
    boardList_remDupl(Tail, Source, HwMod);
boardList_remDupl([ListInfo | Tail], Source, HwMod) ->
    [ListInfo | boardList_remDupl(Tail, Source, HwMod)];
boardList_remDupl([], _, _) ->
    [].

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
    boardtype_format({ProdNo, ProdRev}).

%%% ###########################################################################
%%% boardtype_format
%%%
%%% ###=====================================================================###
boardtype_format({ProdNo, ProdRev}) ->
    {sysUtil:remove_chars($ , ProdNo), ProdRev};
boardtype_format([BT | Tail]) ->
    [boardtype_format(BT) | boardtype_format(Tail)];
boardtype_format(Other) ->
    Other.

%%% ###########################################################################
%%% boardOth
%%%
%%% ###=====================================================================###
boardOth(BTs) when is_list(BTs) ->
    [{BT, {boardTypeOth_st(BT), boardTypeOth_sw(BT)}} || BT <- BTs];
boardOth(BTs) ->
    [BTs | ["--- Registered boardTypeOTH ---" | boardTypes_oth_info()]].

%%% ###########################################################################
%%% boardTypes_oth_info
%%%
%%% ###=====================================================================###
boardTypes_oth_info() ->
    try
	[{BoardType, {Sw_State, Sw}}
	 || #swmBoardOth{boardType = BoardType,
			 sw = Sw,
			 sw_state = Sw_State} <- ets:tab2list(swmBoardOth)]
    catch
	_ : _ ->
	    []
    end.

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
products_global(ConfigE, {_, _} = BoardType) ->
    case boardListE(ConfigE, boardList, BoardType) of
	undefined ->
	    case boardListE(ConfigE, contentinfo, BoardType) of
		undefined ->
		    ?LOG_ERR([?UC_ERR_NoProductListInGlobalUP,
			      {files, ?UP_FILES}]),
		    throw(?UC_ERR_2(?UC_ERR_NoProductListInGlobalUP));
		{BoardList, ContentinfoE} ->
		    trace_add(swp_selected, BoardList, global),
		    {BoardList, boardList2products(ContentinfoE, global)}
	    end;
	{BoardList, BoardListE} ->
	    trace_add(swp_selected, BoardList, global),
	    {BoardList, boardList2products(BoardListE, global)}
    end;
products_global(_, undefined) ->
    {undefined, []}.

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
		    trace_add(swp_selected, BoardList, hal),
		    trace_add(swp_selected, {swp_id, swp_id(ConfigE)}, hal),
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
products_main(BoardType, GlobalUpFiles, HalUpFiles, BoardTypesOTH, MetaOpts) ->
    trace_add(swp_selected, {boardTypeBB, BoardType}),
    trace_add(swp_selected, {files, ?UP_FILES}),
    HalConfigEs = parse_configE(HalUpFiles),
    case parse_configE(GlobalUpFiles) of
	[GlobalConfigE] ->
	    Products =
		products_parsedUPs(BoardType,
				   GlobalConfigE,
				   swp_id(GlobalConfigE),
				   HalConfigEs,
				   BoardTypesOTH),
	    write_swp_selected(),
	    %% Warning side-effects in this function
	    Selected = swp_selected_erase(MetaOpts),
	    ?LOG_INFO_ALL([sysUtil:get_previous_modFun(),
	     		   "--- Selected ---" | selected_format(Selected)] ++
	     		  ["--- Requested boardTypeOTH ---"
	     		   | boardOth(BoardTypesOTH)]),
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
products_parsedUPs(BoardType, GlobalUP, GlobalSwpId, HalUPs, BoardTypesOTH) ->
    GlobalIndex =
	case hwSwCompatibilityIndex(GlobalUP) of
	    [Char | _] = Ix when is_integer(Char) ->
		trace_add(swp_selected, {hwSwCompatibility, [{index, Ix}]}),
		Ix;
	    undefined ->
		trace_add(swp_selected, {hwSwCompatibility, undefined}),
		undefined;
	    [Ix | _] = IndexList when is_list(Ix) ->
		?LOG_ERR([?UC_ERR_MultipleHwSwCompIndexInGlobalUP,
			  {indexList, IndexList},
			  {files, ?UP_FILES}]),
		throw(?UC_ERR_2(?UC_ERR_MultipleHwSwCompIndexInGlobalUP))
	end,
    trace_add(swp_selected, {swp_id, GlobalSwpId}, global),
    ok = validate_node_type(GlobalUP),
    products_lists(products_hal(HalUPs, GlobalSwpId, GlobalIndex, BoardType),
		   products_global(GlobalUP, BoardType),
		   boardType_all(GlobalUP),
		   GlobalIndex,
		   BoardType,
		   GlobalUP,
		   HalUPs,
		   BoardTypesOTH).

%%% ###########################################################################
%%% products_lists
%%%
%%% ###=====================================================================###
products_lists({HalProdsFrom, [_ | _] = HalProducts, HalUP},
	       {_GlProdsFrom, GlobalProducts},
	       BTsGlobal,
	       GlobalIndex,
	       BoardType,
	       GlobalUP,
	       HalUPs,
	       BoardTypesOTH) ->
    validate_HwSwCompIndex(HalUPs, GlobalIndex),
    validate_board_type_duplicate(BoardType, boardType_all(HalUP)),
    validate_board_type_duplicate(BoardType, boardType_all(GlobalUP)),
    OtherProducts = products_other(HalProdsFrom,
				   BoardTypesOTH,
				   GlobalUP,
				   BTsGlobal),
    Products = lists_merge(products_select(HalProducts,
					   GlobalProducts ++ OtherProducts),
			   OtherProducts),
    products_clean(Products);
products_lists({_, [], _},   % Nothing from HAL.
	       {{boardList, _} = GlProdsFrom, GlobalProducts},
	       BTsGlobal,
	       [_ | _],   % GlobalIndex
	       {_, _} = BoardType,
	       GlobalUP,
	       _,
	       BoardTypesOTH) ->
    validate_board_type_duplicate(BoardType, BTsGlobal),
    Products = lists_merge(GlobalProducts, products_other(GlProdsFrom,
							  BoardTypesOTH,
							  GlobalUP,
							  BTsGlobal)),
    products_clean(Products);
products_lists({_, [], _},   % Nothing from HAL.
	       {contentinfo = _GlProdsFrom, GlobalProducts},
	       [],   % BTsGlobal
	       undefined,   % GlobalIndex
	       {_, _},   % BoardType
	       _,
	       _,
	       _) ->
    %% Legacy Global UP file.
    GlobalProducts;
products_lists({_, [], _},   % Nothing from HAL.
	       {GlProdsFrom, _},
	       BTsGlobal,
	       undefined,   % GlobalIndex
	       {_, _} = BoardType,
	       _,
	       HalUPs,
	       _) ->
    BTsHal = lists:flatten([boardType_all(HalUP) || HalUP <- HalUPs]),
    ?LOG_ERR([?UC_ERR_MissingHwSwCompIndexInGlobalUP,
	      {boardType, BoardType},
	      {supported_global, BTsGlobal},
	      {supported_hal, BTsHal},
	      {global_products_from, GlProdsFrom},
	      {files, ?UP_FILES},
	      {selected, get(swp_selected)}]),
    throw(?UC_ERR_2(?UC_ERR_MissingHwSwCompIndexInGlobalUP));
products_lists({_, [], _},   % Nothing from HAL.
	       {contentinfo = GlProdsFrom, _},
	       BTsGlobal,
	       _GlobalIndex,
	       {_, _} = BoardType,
	       _,
	       HalUPs,
	       _) ->
    Reason = ?UC_ERR_BoardTypeNotSupportedByNeitherHalGlobalUP,
    BTsHal = lists:flatten([boardType_all(HalUP) || HalUP <- HalUPs]),
    ?LOG_ERR([Reason,
	      {boardType, BoardType},
	      {supported_global, BTsGlobal},
	      {supported_hal, BTsHal},
	      {global_products_from, GlProdsFrom},
	      {files, ?UP_FILES},
	      {selected, get(swp_selected)}]),
    throw(?UC_ERR_2(Reason));
products_lists({_, [], _},   % Nothing from HAL.
	       {undefined = GlProdsFrom, []},   % Nothing from Global
	       BTsGlobal,
	       _,   % GlobalIndex
	       undefined = BoardType,
	       GlobalUP,
	       _,
	       BoardTypesOTH) ->
    validate_board_type_duplicate(BoardType, BTsGlobal),
    products_clean(products_other(GlProdsFrom,
				  BoardTypesOTH,
				  GlobalUP,
				  BTsGlobal)).

%%% ###########################################################################
%%% products_other
%%%
%%% ###=====================================================================###
products_other({boardList, {"BASEBAND", _}},
	       BoardTypesOTH,
	       GlobalUP,
	       BTsGlobal) ->
    lists:flatten(products_other(BoardTypesOTH, GlobalUP, BTsGlobal));
products_other(undefined, BoardTypesOTH, GlobalUP, BTsGlobal) ->
    lists:flatten(products_other(BoardTypesOTH, GlobalUP, BTsGlobal));
products_other(_, _, _, _) ->   % "BASEBAND-T"
    [].

%%% ###=====================================================================###
products_other([BoardType | Tail], GlobalUP, BTsGlobal) ->
    case boardListE(GlobalUP, boardList, BoardType) of
	{BoardList, BoardListE} ->
	    trace_add(swp_selected, BoardList, global),
	    [boardList2products(BoardListE, global)
	     | products_other(Tail, GlobalUP, BTsGlobal)];
	undefined ->
	    ?LOG_WARN([?UC_ERR_BoardTypeNotSupportedByNeitherHalGlobalUP,
		       {boardTypeOTH, BoardType},
		       "--- Supported global boardTypes ---"
		       | BTsGlobal] ++
		      ["--- Registered boardTypeOTH ---"
		       | boardTypes_oth_info()]),
	    products_other(Tail, GlobalUP, BTsGlobal)
    end;
products_other([], _, _) ->
    [];
products_other(all, GlobalUP, _) ->
    BoardLists = boardListE(GlobalUP, boardList, {all, "OTHER"}),
    [begin
	 trace_add(swp_selected, BoardList, global),
	 boardList2products(BoardListE, global)
     end
     || {BoardList, BoardListE} <- BoardLists].

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
%%% product_attrs
%%%
%%% ###=====================================================================###
product_attrs(#xmlElement{content = ContentE}) ->
    [ProductE] = [E || E <- ContentE, E#xmlElement.name == product],
    {xmlAttrVal(name, ProductE),
     xmlAttrVal(id, ProductE),
     xmlAttrVal(version, ProductE)}.

%%% ###########################################################################
%%% selected_bl_clean
%%%
%%% ###=====================================================================###
selected_bl_clean([{Source, {"OTHER", HwMod}} = ListInfo | Tail]) ->
    [ListInfo | selected_bl_clean(boardList_remDupl(Tail, Source, HwMod))];
selected_bl_clean([{Source, {"RADIO", HwMod}} = ListInfo | Tail]) ->
    case boardList_isDupl(Tail, Source, HwMod) of
 	true ->
 	    selected_bl_clean(Tail);
 	false ->
 	    [ListInfo | selected_bl_clean(Tail)]
    end;
selected_bl_clean([ListInfo | Tail]) ->
    [ListInfo | selected_bl_clean(Tail)];
selected_bl_clean([]) ->
    [].

%%% ###########################################################################
%%% selected_format
%%%
%%% ###=====================================================================###
selected_format([{boardList, ListInfos} | Tail]) ->
    [{boardList, ListInfo} || ListInfo <- selected_bl_clean(ListInfos)] ++
 	selected_format(Tail);
selected_format([Sel | Tail]) ->
    [Sel | selected_format(Tail)];
selected_format([]) ->
    [].

%%% ###########################################################################
%%% swp_id
%%%
%%% ###=====================================================================###
swp_id(#xmlElement{} = XmlElem) ->
    {Name, Id, Ver} = product_attrs(XmlElem),
    Name ++ "_" ++ Id ++ "_" ++ Ver;
swp_id([Char | _] = MetadataPath) when is_integer(Char) ->
    lists:last(filename:split(filename:dirname(MetadataPath))).

%%% ###########################################################################
%%% swp_metadata_files
%%%
%%% ###=====================================================================###
swp_metadata_files(global, MetaOpts) ->
    Files = swp_metadata_wildcard(swp_metadata_path(global, MetaOpts),
				  ?SWP_METADATA_filenames_global),
    [swmI:find_file(File) || File <- Files];
swp_metadata_files(hal, MetaOpts) ->
    Files =
	[swmI:find_file(File)
	 || File <- swp_metadata_wildcard(swp_metadata_path(hal, MetaOpts),
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
swp_metadata_path(Source, {Source, Path}) ->
    Path;
swp_metadata_path(_, {all, Path}) ->
    Path;
swp_metadata_path(Source, [MetaOpt | Tail]) ->
    try
	swp_metadata_path(Source, MetaOpt)
    catch
	_ : _ ->
	    swp_metadata_path(Source, Tail)
    end;
swp_metadata_path(global, MetaOpts) when MetaOpts == undefined orelse
					 MetaOpts == [] ->
    ?SWP_METADATA_path_global;
swp_metadata_path(hal, MetaOpts) when MetaOpts == undefined orelse
				      MetaOpts == [] ->
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

trace_add(Tag, SubTag1, SubTag2) ->
    Data = trace_add_data(SubTag1, SubTag2),
    case get(Tag) of
	undefined ->
	    put(Tag, [Data]);
	OldData ->
	    NewData = trace_add_sublevel(OldData, Data),
	    put(Tag, NewData)
    end.

trace_add_data({SubTag1, Value}, SubTag2) ->
    {SubTag1, [{SubTag2, Value}]};
trace_add_data(SubTag1, SubTag2) ->
    {SubTag1, [SubTag2]}.

trace_add_sublevel([{Tag, OldData} | Tail], {Tag, Data}) ->
    [{Tag, OldData ++ Data} | Tail];
trace_add_sublevel([E | Tail], {Tag, Data}) ->
    [E | trace_add_sublevel(Tail, {Tag, Data})];
trace_add_sublevel([], {Tag, Data}) ->
    [{Tag, Data}].

%%% ###########################################################################
%%% validate_node_type
%%%
%%% ###=====================================================================###
validate_node_type([UpConfigE | Tail]) ->
    case xmlElemRec_all(type, UpConfigE) of
	[NodeTypeE] ->
	    NodeType = value(NodeTypeE),
	    case swmI:is_node_type_valid(NodeType) of
		true ->
		    validate_node_type(Tail);
		deprecated ->
		    ?LOG_INFO([{"Deprecated Node Type", NodeType},
			       {selected, get(swp_selected)},
			       {files, ?UP_FILES}]),
		    validate_node_type(Tail);
		false ->
		    %% TODO: Change to:
		    %% ?LOG_ERR([?UC_ERR_NodeTypeNotValid,
		    %% 	      {"Node Type", NodeType},
		    %%	      {selected, erase(swp_selected)},
		    %% 	      {files, ?UP_FILES}]),
		    %% throw(?UC_ERR_2(?UC_ERR_NodeTypeNotValid))
		    ?LOG_WARN([{?UC_ERR_NodeTypeNotValid, NodeType},
			       {selected, get(swp_selected)},
			       {files, ?UP_FILES}]),
		    validate_node_type(Tail)
	    end;
	Unexpected ->
	    ?LOG_ERR([{"Unexpected", Unexpected},
		      {selected, erase(swp_selected)},
		      {files, ?UP_FILES}]),
	    throw(?UC_ERR_2(?UC_ERR_NodeTypeMissing))
    end;
validate_node_type([]) ->
    ok;
validate_node_type(UpConfigE) when is_tuple(UpConfigE) ->
    validate_node_type([UpConfigE]).

%%% ###########################################################################
%%% validate_HwSwCompIndex
%%%
%%% ###=====================================================================###
validate_HwSwCompIndex(UPs, Index) ->
    case
	%% Index must not be duplicated:
	lists:member(Index, lists:delete(Index, values(UPs, hwSwCompatibility)))
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
