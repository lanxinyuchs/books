%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmInventory.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R7A/R8A/R9A/R10A/R11A/6
%%%
%%% @doc ==Software inventory==
%%% This module contains the agent implementation of the ECIM SwIM model.

-module(swmInventory).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R7A/R8A/R9A/R10A/R11A/6').
-date('2017-10-11').
-author('etxpejn').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R1A/1      2012-01-31 etxjotj     Created
%%% R1A/12     2012-08-17 etxjotj     Fixed problem with no product info
%%% R2A/20     2013-10-17 erarafo     Support for new Oam Mgmt SPI in COM
%%% R2A/30     2014-04-07 etxjotj     Changed log entry
%%% R2A/33     2014-06-12 etxjotj     Adaptions for EE split
%%% R3A/1      2014-11-20 etxjotj     Error report when inventory fails
%%% R3A/2      2014-11-28 etxberb     Added values/1.
%%% R3A/3      2014-11-28 etxjotj     ECIM SwM 3.0
%%% R3A/4      2015-01-20 etxjotj     Extended esi info
%%% R4A/1      2015-08-17 etxjotj     Handle product numbers without variants
%%% R4A/2      2015-08-24 etxpejn     Changed logI:write_log to swmLib:write_swm_log
%%% R5A/1      2016-03-21 etxjotj     New inventory mgmt for HALI
%%% R5A/2      2016-04-01 etxpejn     Added UP version in the HW log
%%% R5A/3      2016-04-01 etxpejn     Added hw_log/3
%%% R5A/4      2016-04-01 etxjotj     Indentation in SwmLog for UP content
%%% R5A/5      2016-04-04 etxpejn     Updated the msg in HW log
%%% R5A/6      2016-04-06 etxpejn     Added CXP NO in hw_log/4
%%% R5A/7      2016-04-15 etxberb     Removed 'io:format' in make_bundle
%%% R7A/1      2016-10-03 etxberb     Added make_currentUP_key/0, make_UP_key/1,
%%%                                   make_UP_key/2.
%%% R8A/1      2016-12-19 etxjotj     SwInventory fixed for decoupling
%%% R8A/2      2016-12-21 etxberb     Reverted R8A/1 change for vrcs.
%%% R8A/3      2017-01-11 etxjotj     Inventory for VRCS
%%% R8A/4      2017-01-12 etxjotj     Bugfix
%%% R8A/5      2017-01-13 etxjotj     Workaround for old metadata
%%% R9A/1      2017-03-06 etxberb     Added make_sw_item_NotDL/3. Mark swItems
%%%                                   with "Not downloaded" in additionalInfo.
%%% R9A/2      2017-04-06 etxberb     Bug correction in make_sw_item/4.
%%% R9A/3      2017-04-19 etxberb     Added update_table/1.
%%% R9A/4      2017-06-17 etxberb     HV96262: Checking against archive to
%%%                                   determine DownloadedProducts.
%%% R9A/5      2017-06-20 etxberb     HV96980: Checking only ProductsOTH against
%%%                                   archive to determine DownloadedProducts.
%%% R10A/1     2017-07-14 etxjotj     HW11604 Reduce logging
%%% R10A/2     2017-07-17 etxberb     Bug fix for 5G in hw_log/4.
%%% R11A/1     2017-08-18 etxjotj     Improved logging
%%% R11A/2     2017-09-04 etxjotj     Handle 'not_supported' active instance
%%% R11A/3     2017-09-08 etxjotj     Better not_supported handling for vrcs
%%% R11A/4     2017-09-29 etxpejn     Added hw_log/1
%%% R11A/5     2017-10-03 etxpejn     Added do_write_hw_log
%%% R11A/6     2017-10-11 etxpejn     Handle when hw_log includes atom
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([getMoAttribute/2, nextMo/3]).
-export([createMo/4,setMoAttribute/4,deleteMo/2]).
-export([prepare/3,commit/3,finish/3]).

%% -export([set_active_version/1]).
-export([update_table/1,
	 update_tables/0, update_tables/1,
	 update_sw_version/1]).
-export([backup/0]).
-export([format_product_number/1]).
-export([make_mom_key/2]).

-export([existsMo/2,
	 countMoChildren/3,
	 getMoAttributes/3,
	 setMoAttributes/3,
	 createMo/5]).

-export([get_current_sw_version/0]).
-export([make_currentUP_key/0,
	 make_UP_key/1,
	 make_UP_key/2]).

-export([print_inventory/0, print_inventory/1]).

-export([hw_log/1]).
 
%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-export([get_sw_item_pd/1, get_sw_version_pd/1]).

-include("comte_types.hrl").
-include("RcsSwIM.hrl").
-include("SwmInternal.hrl").
-include_lib("xmerl/include/xmerl.hrl").
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%% @doc Returns true if the specified instance exists.

-spec existsMo([binary()], integer()) -> boolean().

existsMo(DnRev, _) ->
    comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))).


%%% @doc Returns the number of MO instances of given class
%%% directly below the specified parent.

-spec countMoChildren([binary()], binary(), integer()) -> non_neg_integer().

countMoChildren(DnRev, Class, _) ->
    comsaGeneric:countMoChildren(DnRev, table(binary_to_list(Class))).

getMoAttributes(AttrNames, DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, AttrNames, Table, types(Table)).

getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).



%nextMo(Dn, Key, TxHandle) ->
nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).



setMoAttributes(Attrs, DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), Attrs).

setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).


%% @doc Creates a new instance. The given class name is trusted to be
%% one of the names supported by this module.
%%
%% TODO: The KeyAttrName argument is not used, should it be?

-spec createMo([binary()], 
	       mo_attribute_name(), 
	       binary(), 
	       [com_named_attribute()], 
	       integer()) -> 
	  {ok, tuple()}.

createMo([ClassName | ParentDnRev],
	 _KeyAttrName,
	 KeyValue,
	 InitAttrs,
	 _TransId) ->
    Table = table(binary_to_list(ClassName)),
    comsaGeneric:create(Table,
			ParentDnRev,
			KeyValue,
			values(InitAttrs),
			types(Table)).

createMo([Class|ParentDnRev], _, IxValueB, _) ->
    Table = table(binary_to_list(Class)),
    comsaGeneric:create(Table, ParentDnRev, IxValueB).

deleteMo(_, _) ->
    ok.

table("SwInventory") -> swInventory;
table("SwVersion") -> swVersion;
table("SwItem") -> swItem.

types(swInventory) -> ?swInventory_types;
types(swVersion) -> ?swVersion_types;
types(swItem) -> ?swItem_types.

values([{Name, {_, Value}} | Tail]) ->
    [{Name, Value} | values(Tail)];
values([Attr | Tail]) ->
    [Attr | values(Tail)];
values([]) ->
    [].

prepare(_DN, User, _Tx) ->
    {ok,User}.
commit(_DN, User, _Tx) ->
    {ok,User}.
finish(_DN, _User, _Tx) ->
    ok.

%% set_active_version(Key) ->
%%     swmLib:db_op(fun() -> do_set_active_version(Key) end).

set_active_version({_, _, _, Key} = SwVersionId) ->

    Active = 
	["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwVersion="++Key],

    [SwInventory] = mnesia:read({swInventory, {"1","1","1"}}),
    mnesia:write(SwInventory#swInventory{active = Active}),

    [SwVersion] = mnesia:read({swVersion, SwVersionId}),
    Time = comsaI:iso_time(os:timestamp(), extended),
    NewSwVersion = SwVersion#swVersion{timeOfActivation = Time},
    mnesia:write(NewSwVersion),
    NewSwVersion.


write_log("SwmLog", Sender, Severity, Msg) ->
    case swmLib:write_swm_log(Sender, Severity, Msg) of
	ok -> ok;
	AnyError ->
	    warning_msg("SwmLog appears to be disabled: ~p~n",[AnyError])
    end;
write_log(Log, Sender, Severity, Msg) ->
    case logI:write_log(Log, Sender, Severity, Msg) of
	ok -> ok;
	AnyError ->
	    warning_msg("SwmLog appears to be disabled: ~p~n",[AnyError])
    end.


log_sw_item(SwItemMoRef) ->
    DnChunks = string:tokens(SwItemMoRef, ","),
    KeyList = [lists:last(string:tokens(Chunk,"="))||Chunk<-DnChunks],
    Key = list_to_tuple(KeyList),
    [SwItem] = mnesia:dirty_read({swItem, Key}),
    #'ProductData'{productName = Name,
		   productNumber = Id,
		   productRevision = Rev} = 
	SwItem#swItem.administrativeData,

    Msg = "    Consists of "++Name++" "++Id++" "++Rev,
    write_log("SwmLog", "SwInventory", info, Msg).

hw_log(AttributeString) when is_list(AttributeString) ->
    MsgString = "Site : " ++AttributeString,
    hw_log(hw, MsgString, "NetworkName", AttributeString, sysEnv:rcs_mode_2());
hw_log(Attribute) ->
    hw_log(atom_to_list(Attribute)).
    
hw_log(Type, MsgString, FileName, Info, target) ->
    %% Write the UP in i the HW log:
    File = filename:join([sysEnv:rcs_dir(), "persistent/", FileName]),
    ok = filelib:ensure_dir(File),
    BinInfo = list_to_binary(Info),
    case file:read_file(File) of
	{ok, BinInfo} ->
	    %% The info is already written in the HW log
	    do_nada;
	_Else ->
	    %% Other info is written in the file or the 
	    %% file does not exist. Insert the info in the HW log
	    %% and in the file
	    do_write_hw_log(Type, MsgString,  FileName, Info, File)
    end;
hw_log(_Type, _MsgString, _FileName, _Info, _) ->
    %% No need to write in the log
    do_nada.

do_write_hw_log(hw, MsgString,  FileName, Info, File) ->
    case sysRhai:hwl_logentry(MsgString, "000") of
	{error, Reason} ->
	    %% faild to write in the hw log 
	    sysInitI:warning_report([{swmInventory, do_write_hw_log, 
				      [hw, MsgString, FileName, Info]},
				     [{"Failed to write in the HW log", 
				       {error, Reason}}]]);
	{ok, _} ->
	    %% HW log entry succeful, update the file on disk
	    ok = file:write_file(File, Info)
    end;
do_write_hw_log(sw, MsgString,  FileName, Info, File) ->
    case sysRhai:hwl_logentry(MsgString) of
	{error, Reason} ->
	    %% faild to write in the hw log 
	    sysInitI:warning_report([{swmInventory, do_write_hw_log, 
				      [sw, MsgString, FileName, Info]},
				     [{"Failed to write in the HW log", 
				       {error, Reason}}]]);
	{ok, _} ->
	    %% HW log entry succeful, update the file on disk
	    ok = file:write_file(File, Info)
    end.

	
%%% Description:
%%%  Update the swVersion and swItem tables based on the content of the
%%%  $RCS_ROOT/home/$USER/swm and
%%%  $RCS_ROOT/home/$USER/software

update_table(BoardType) ->
    %% Virtual nodes (vrcs) should never call this function.
    case swmLib:db_op(fun() -> do_update_table(BoardType) end) of 
	{atomic, _} ->
	    {atomic, ok};
	{aborted, Reason} ->
	    sysInitI:error_report([{swmInventory, do_update_table, [BoardType]},
				   {aborted, Reason}])
    end.

update_tables() ->
    update_tables(no_log).

update_tables(Log) ->
    case swmLib:db_op(fun() -> do_update_tables() end) of 
	{atomic, SwVersion} ->
	    backup(),
	    %% HW11604 move logging of inventory to this function instead
	    %% of set_activate_instance, because that func is running within
	    %% a transaction
	    #'ProductData'{productName = Name,
			   productNumber = Id,
			   productRevision = Rev} = 
		SwVersion#swVersion.administrativeData,
	    case Log of
		no_log -> 
		    ok;
		log ->
		    Current = case swmOs:get_active_instance() of
				  X when is_integer(X) -> 
				      " as instance "++integer_to_list(X);
				  not_supported -> ""
			      end,
		    Msg = "Running upgrade package "++
			Name++" "++Id++" "++Rev++Current,
		    write_log("SwmLog", "SwInventory", info, Msg),
		    [log_sw_item(SwItemMoRef)||
			SwItemMoRef<-
			    lists:sort(SwVersion#swVersion.consistsOf)],
		    info_msg("~s~n",[Msg])
	    end,
	    
	    MsgString = "SW version : "++Name++" "++Id++" "++Rev++" started",
	    hw_log(sw, MsgString, "UP", Rev, sysEnv:rcs_mode_2()),
	    
	    %%HW11604 ends here

	    {atomic, ok};
	{aborted, Reason} ->
	    sysInitI:error_report([{swmInventory, do_update_tables, []},
				       {aborted, Reason}])
    end.
			     
do_update_table(BoardType) ->
    RootDir = swmLib:software_dir(),
    {ok, [{products, Products}]} =
	swmBoardList:swp([products], [{boardTypesOTH, [BoardType]},
				      {options, [{global, RootDir}]}]),
    make_consists_of_2(RootDir, Products).

do_update_tables() ->
    scratch_table(swVersion),
    scratch_table(swItem),
    UpPattern = filename:join(swmLib:software_dir(), "*-up.xml"),
    case filelib:wildcard(UpPattern) of
	[] ->
	    erlang:error({missing_up_metadata, UpPattern});
	[UpFile] ->
	    SwVersion = update_sw_version(UpFile),
	    set_active_version(SwVersion#swVersion.swVersionId)
    end.


update_sw_version(UpFile) ->
    RootDir = filename:dirname(UpFile),

    {ConfigurationE, []} = xmerl_scan:file(UpFile),

    try find_attribute(type, ConfigurationE) of
	"MSRBS-UP" ->
	    update_msrbs(RootDir, ConfigurationE);
	"IMAGE" ->
	    update_image(RootDir, ConfigurationE)
    catch error:{badmatch,false} ->
	    error_msg("In ~p~nthe <configuration> element does not have the type attribute~n",[UpFile]),
	    update_msrbs(RootDir, ConfigurationE)
    end.

update_msrbs(RootDir, ConfigurationE) ->
    ConsistsOf =
	case sysEnv:rcs_mode_2() of
	    vrcs ->
		ContentInfoE = find_element(contentinfo, ConfigurationE),
		make_consists_of(RootDir, ContentInfoE);
	    _ ->
		BoardTypeBB = swmBoardList:boardType(),
		BoardTypesOTH = all,
		{ok, Products} =
		    swmBoardList:products({BoardTypeBB, BoardTypesOTH}),
		make_consists_of_2(RootDir, Products)
	end,
    
    SwVersion = read_sw_dates(get_sw_version_pd(ConfigurationE)),
    FinalSwVersion = SwVersion#swVersion{consistsOf=ConsistsOf},
    swmLib:db_op(fun() -> mnesia:write(FinalSwVersion) end),
    FinalSwVersion.

update_image(RootDir, ConfigurationE) ->
    ConsistsOf = try make_image_consists_of(RootDir, ConfigurationE) 
		 catch T:E ->
			 io:format("~p~n~p~n",[{T,E},erlang:get_stacktrace()]),
			 []
		 end,
    SwVersion = read_sw_dates(get_sw_version_pd(ConfigurationE)),
    FinalSwVersion = SwVersion#swVersion{consistsOf=ConsistsOf},
    swmLib:db_op(fun() -> mnesia:write(FinalSwVersion) end),
    FinalSwVersion.

make_image_consists_of(RootDir, ConfigurationE) ->
    ItemId = make_image_sw_item(RootDir, ConfigurationE),
    ["ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem="++ItemId].

make_image_sw_item(RootDir, ConfigurationE) ->
    VnfProductE = find_element(product, ConfigurationE),

    ImageProductE = find_element(product, VnfProductE),
    ImageName = find_attribute(name, ImageProductE),
    ImageId = find_attribute(id, ImageProductE),
    ImageVsn = find_attribute(version, ImageProductE),
    SwItemKey= make_mom_key(ImageId, ImageVsn),

    PD = #'ProductData'{productName = ImageName,
			productNumber = format_product_number(ImageId),
			productRevision = ImageVsn,
			productionDate = "",
			description = "",
			type = "Image"},
	
    ContentInfoE = find_element(contentinfo, ConfigurationE),
    ConsistsOf = make_consists_of(RootDir, ContentInfoE),

    SwItem = #swItem{swItemId = {"1","1","1", SwItemKey},
		     additionalInfo = "Image file",
		     administrativeData = PD,
		     consistsOf = ConsistsOf},
    swmLib:db_op(fun() -> mnesia:write(SwItem) end),
    SwItemKey.

make_consists_of(RootDir, ContentInfoE) ->
    [begin
	 PName = find_attribute(name, ProdE),
	 PId = find_attribute(id, ProdE),
	 PVsn = find_attribute(version, ProdE),
	 ItemId = make_mom_key(PId, PVsn),
	 make_sw_item(RootDir, PName, PId, PVsn),
	 "ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem="++ItemId
     end||ProdE<-ContentInfoE#xmlElement.content,
	  ProdE#xmlElement.name == product].

make_consists_of_2(RootDir, Products) ->
    {ok, [{products, Selected_ProductsOTH}]} =
	swmBoardList:swp([products],
			 [{boardTypesOTH, swmBoardList:boardTypes_oth()},
			  {options, [{global, RootDir}]}]),
    {ok, [{products, Selected_ProductsBB}, {swp_id, SIs}]} =
	swmBoardList:swp([products, swp_id],
			 [{boardTypeBB, swmBoardList:boardType()},
			  {options, [{global, RootDir}]}]),
    SwpId = swmBoardList:swp_id_global(SIs),
    ArchiveDir = filename:join([swmLib:archive_dir(), SwpId]),
    ArchiveFiles = swmBoardList:archiveCXPs(ArchiveDir),
    %% Downloaded products: Need to check against archive in case of backup
    %% restore, and doing this at startup - before CAT has registered any boards
    DL_Products =
	Selected_ProductsBB ++
	dl_products(Selected_ProductsOTH, ArchiveFiles),
    [begin
	 %% PName = find_attribute(name, ProdE),
	 %% PId = find_attribute(id, ProdE),
	 %% PVsn = find_attribute(version, ProdE),
	 ItemId = make_mom_key(PId, PVsn),
	 case lists:keyfind({PName, PId, PVsn}, 1, DL_Products) of
	     {_, _} ->
		 make_sw_item(RootDir, PName, PId, PVsn);
	     false ->
		 make_sw_item_NotDL(PName, PId, PVsn)
	 end,
	 "ManagedElement=1,SystemFunctions=1,SwInventory=1,SwItem=" ++ ItemId
     end
     || {{PName, PId, PVsn}, _} <- Products].

%%% ----------------------------------------------------------
dl_products([{_, {global, FileName}} = Product | Tail], ArchiveFiles) ->
    case lists:member(FileName, ArchiveFiles) of
	true ->
	    [Product | dl_products(Tail, ArchiveFiles)];
	false ->
	    dl_products(Tail, ArchiveFiles)
    end;
dl_products([{_, {hal, _}} = Product | Tail], ArchiveFiles) ->
    [Product | dl_products(Tail, ArchiveFiles)];
dl_products([], _) ->
    [].

%%% ----------------------------------------------------------
get_sw_version_pd(ConfigurationE) ->
    ProductE = find_element(product, ConfigurationE),
    Name = find_attribute(name, ProductE),
    ProdId = find_attribute(id, ProductE),
    Version = find_attribute(version, ProductE),
    DateE = find_element(date, ConfigurationE),
    DescriptionE = try find_element(description, ConfigurationE) 
		   catch _:_ -> ""
		   end,
    TypeE = try find_element(type, ConfigurationE) 
	    catch _:_ -> ""
	    end,
    
    ProductData = 
	#'ProductData'{productName = Name,
		       productNumber = format_product_number(ProdId),
		       productRevision = Version,
		       productionDate = find_text(DateE),
		       description = find_text(DescriptionE),
		       type = find_text(TypeE)},

    SwVersionId = {"1","1","1",make_mom_key(ProdId,Version)},
    #swVersion{swVersionId=SwVersionId,
	       administrativeData = ProductData}.

read_sw_dates(SwVersion) ->
    {Install, Activate, Deactivate} = 
	try dets:lookup(swVersion, SwVersion#swVersion.swVersionId) of
	    [] -> 
		{comsaI:iso_time(os:timestamp(), extended),
		 undefined, 
		 undefined};
	    [OldObj] ->
		try begin
			{OldObj#swVersion.timeOfInstallation,
			 OldObj#swVersion.timeOfActivation,
			 OldObj#swVersion.timeOfDeactivation} end of
		    {I,A,D} -> {I,A,D}
		catch _:_ ->
			{comsaI:iso_time(os:timestamp(), extended),
			 undefined, 
			 undefined}
		end
	catch _:_ ->
		{comsaI:iso_time(os:timestamp(), extended),
		 undefined, 
		 undefined}
	end,
    SwVersion#swVersion{timeOfInstallation = Install,
			timeOfActivation = Activate,
			timeOfDeactivation = Deactivate}.

make_sw_item(RootDir, ProdName, ProdId, ProdVsn) ->
    CxpDir = ProdName++"_"++ProdId++"_"++ProdVsn,
    MetaPattern = filename:join([RootDir, CxpDir, "cxp*.xml"]),
    case filelib:wildcard(MetaPattern) of
	[MetaPath] ->
	    {ConfigurationE, []} = xmerl_scan:file(MetaPath),
	    %% If the os element is present it is empty, so its
	    %% existence is all we care about
	    PD = get_sw_item_pd(ConfigurationE),
	    ItemId = make_mom_key(ProdId, ProdVsn),
	    SwItem = #swItem{swItemId = {"1","1","1", ItemId},
			     administrativeData = PD,
			     consistsOf = []},
	    swmLib:db_op(fun() -> mnesia:write(SwItem) end);

	[] ->
	    try
		make_bundle(RootDir, ProdName, ProdId, ProdVsn)
	    catch
		error : cxp_metadata_not_found ->
		    %% This CXP may not present due to decoupling
		    %% Therefore only limited data is available (for now)
		    %% Potentially the archive could be picked for metadata
		    %% files, but that's a later improvement
		    %% This also means, if it's a bundle it won't get detected
		    %% jotj 2016-03-21
		    make_sw_item_NotDL(ProdName, ProdId, ProdVsn)
	    end

    end.

make_sw_item_NotDL(ProdName, ProdId, ProdVsn) ->
    NA = "Not available",
    PD = #'ProductData'{productName = ProdName,
			productNumber = format_product_number(ProdId),
			productRevision = ProdVsn,
			productionDate = NA,
			description = NA,
			type = NA},
    ItemId = make_mom_key(ProdId, ProdVsn),
    SwItem = #swItem{swItemId = {"1","1","1", ItemId},
		     additionalInfo = "Item is not downloaded",
		     administrativeData = PD,
		     consistsOf = []},
    swmLib:db_op(fun() -> mnesia:write(SwItem) end).

get_sw_item_pd(ConfigurationE) ->
    ProductE = find_element(product, ConfigurationE),
    Name = find_attribute(name, ProductE),
    ProdId = find_attribute(id, ProductE),
    Version = find_attribute(version, ProductE),
    DateE = find_element(date, ConfigurationE),
    DescriptionE = try find_element(description, ConfigurationE)
		   catch
		       _:_ -> ""
		   end,
    
    TypeE = try find_element(type, ConfigurationE) 
	    catch
		_:_ -> ""
	    end,

    #'ProductData'{productName = Name,
		   productNumber = format_product_number(ProdId),
		   productRevision = Version,
		   productionDate = find_text(DateE),
		   description = find_text(DescriptionE),
		   type = find_text(TypeE)
		  }.

make_bundle(RootDir, ProdName, ProdId, ProdVsn) ->
    %% This may be a bundle, check for available bundles
    BundleP = filename:join(RootDir, "cxp*.xml"),
    Bundles =  filelib:wildcard(BundleP),
    make_bundle(RootDir, ProdName, ProdId, ProdVsn, Bundles).

make_bundle(RootDir, ProdName, ProdId, ProdVsn, []) ->
    erlang:error(cxp_metadata_not_found, 
		 [RootDir, ProdName, ProdId, ProdVsn, []]);
make_bundle(RootDir, ProdName, ProdId, ProdVsn, [Bundle|Bundles]) ->
    try
	do_make_bundle(RootDir, ProdName, ProdId, ProdVsn, Bundle)
    catch
	_ : _ ->
	    make_bundle(RootDir, ProdName, ProdId, ProdVsn, Bundles)
    end.

do_make_bundle(RootDir, ProdName, ProdId, ProdVsn, Bundle) ->
    {ConfigurationE, []} = xmerl_scan:file(Bundle),
    
    %% These are for assertions only
    %% A crash or badmatch will cause this file to be skipped
    find_element(bundle, ConfigurationE),
    ProductE = find_element(product, ConfigurationE),
    ProdName = find_attribute(name, ProductE),
    ProdId = find_attribute(id, ProductE),
    ProdVsn = find_attribute(version, ProductE),

    DateE = find_element(date, ConfigurationE),
    DescriptionE = try find_element(description, ConfigurationE)
		   catch
		       _:_ -> ""
		   end,
    
    TypeE = try find_element(type, ConfigurationE) 
	    catch
		_:_ -> ""
	    end,

    PD = #'ProductData'{productName = ProdName,
			productNumber = format_product_number(ProdId),
			productRevision = ProdVsn,
			productionDate = find_text(DateE),
			description = find_text(DescriptionE),
			type = find_text(TypeE)
		       },
    ContentInfoE = find_element(contentinfo, ConfigurationE),
    ConsistsOf = make_consists_of(RootDir, ContentInfoE),
    ItemId = make_mom_key(ProdId, ProdVsn),
    SwItem = #swItem{swItemId = {"1","1","1", ItemId},
		     additionalInfo = "bundle",
		     administrativeData = PD,
		     consistsOf = ConsistsOf},
    swmLib:db_op(fun() -> mnesia:write(SwItem) end).



    




backup() ->
    %% It is assumed that the table has been opened by swmServer
    Objects = ets:tab2list(swVersion),
    try dets:insert(swVersion, Objects) of
	ok ->
	    ok;
	{error, Reason} ->
	    sysInitI:error_report(
	      [{?MODULE, backup},
	       {mfa, {dets, insert, [swVersion, Objects]}},
	       {error, Reason}])
    catch _:_ ->
	    ok
    end.

get_current_sw_version() ->
    Fun = fun() -> do_get_current_sw_version() end,
    swmLib:db_op_dirty(Fun).    

do_get_current_sw_version() ->
    [SwIM] = mnesia:read({swInventory, {"1","1","1"}}),
    ActiveDNs = SwIM#swInventory.active,
    [begin
	 Vsn = lists:last(string:tokens(ActiveDN, "=,")),
	 SwVersionKey = {"1","1","1",Vsn},
	 [SwVersion]= mnesia:read({swVersion, SwVersionKey}),
	 SwVersion#swVersion.administrativeData
     end||ActiveDN<-ActiveDNs].


print_inventory() ->
    print_inventory(standard_io).

print_inventory(Fd) ->
    [print_sw_version(Fd, SwVersion)||SwVersion<-ets:tab2list(swVersion)],
    ok.



%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%some_method(Parameter)->
%   nn.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%internal_function1(One, Two)->
%   nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

print_sw_version(Fd, SwVersion) ->
    io:format(Fd, "~76c~n",[$=]),
    io:format(Fd, "Content for "++
		  format_product_data(SwVersion#swVersion.administrativeData,head)++
		  "~n",[]),
    io:format(Fd, "~76c~n",[$-]),
    print_sw_items(Fd, "", SwVersion#swVersion.consistsOf),
    io:format(Fd, "~76c~n~n",[$=]).
    

print_sw_items(Fd, Indent, MoRefs) ->
    [print_sw_item(Fd, Indent, MoRef)||MoRef<-lists:sort(MoRefs)].

print_sw_item(Fd, Indent, MoRef) ->
    Key = make_key(MoRef),
    [SwItem] = mnesia:dirty_read({swItem, Key}),
    Info = case SwItem#swItem.additionalInfo of
	       undefined -> "";
	       I -> I
	   end,
    io:format(Fd, format_product_data(SwItem#swItem.administrativeData,item,
				      Indent)++" ~s~n",[Info]),
    print_sw_items(Fd, "  "++Indent, SwItem#swItem.consistsOf).    

make_key(MoRef) ->
    list_to_tuple([lists:nth(2, string:tokens(X,"="))||
		      X<-string:tokens(MoRef,",")]
		 ).

format_product_data(ProductData, Type) ->
    format_product_data(ProductData, Type, "").

format_product_data(ProductData, Type, Indent) ->
    #'ProductData'{productName = Name,
		   productNumber = Prod,
		   productRevision = Rev,
		   productionDate = Date} = ProductData,
    case Type of 
	head -> 
	    io_lib:format("~s ~s ~s ~s",[Name, format_prod(Prod), Rev, Date]);
	item -> 
	    io_lib:format("~-20s ~-18s ~-7s ~19s",
			  [Indent++Name, format_prod(Prod), Rev, Date])
    end.

format_prod([A,B,C|ProductNumber]) ->
    case string:tokens(ProductNumber, "/") of
	[Main, Variant] ->
	    [A,B,C,$  ]++format_main(Main)++"/"++Variant;
	[Main] ->
	    [A,B,C,$ |format_main(Main)]
    end.    

format_main([C1,C2,C3|Sequence]) ->    
    [C1,C2,C3,$  | Sequence].

make_currentUP_key() ->
    [UP_metadata] = get_current_sw_version(),
    make_UP_key(UP_metadata).

make_UP_key(#'ProductData'{productNumber = Product,
			   productRevision = Version}) ->
    make_UP_key(Product, Version);
make_UP_key([#'ProductData'{} = ProductData | Tail]) ->
    lists:flatten([make_UP_key(ProductData) | make_UP_key(Tail)]);
make_UP_key([Prop | _] = ProductData) when is_tuple(Prop) ->
    make_UP_key(proplists:get_value(productNumber, ProductData),
		proplists:get_value(productRevision, ProductData));
make_UP_key([PropsList | Tail]) when is_list(PropsList) ->
    lists:flatten([make_UP_key(PropsList) | make_UP_key(Tail)]);
make_UP_key([]) ->
    [].

make_UP_key(Product, Version) ->
    {"1", "1", "1", make_mom_key(Product, Version)}.

make_mom_key(Product, Version) ->
    format_product_number(Product)++"-"++Version.

format_product_number(ProductNumber) ->
    [case X of
	 $_ -> $/;
	 _ -> X
     end||X<-ProductNumber].

    
    
scratch_table(Table) ->
    swmLib:db_op(
      fun() -> 
	      [mnesia:delete({Table, Key})||Key<-mnesia:all_keys(Table)]
      end),
    ok.


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
    {value, Element} =
        lists:keysearch(ElementName, #xmlElement.name, ContentList),
    Element.

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

find_text(Element) when is_record(Element, xmlElement) ->
    [Text] = Element#xmlElement.content,
    Text#xmlText.value;
find_text([]) -> "".


%% info_msg(Format) ->
%%     info_msg(Format, []).
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%error_msg(Format) ->
%    error_msg(Format, []).
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%even_more_internal_function1(One, Two)->
%   nnn.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

