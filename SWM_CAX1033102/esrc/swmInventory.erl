%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	swmInventory.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2012-2016
%%% @version /main/R1A/R2A/R3A/R4A/R5A/1
%%%
%%% @doc ==Software inventory==
%%% This module contains the agent implementation of the ECIM SwIM model.

-module(swmInventory).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/1').
-date('2016-03-21').
-author('etxjotj').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
-export([update_tables/0, update_sw_version/1]).
-export([backup/0]).
-export([format_product_number/1]).
-export([make_mom_key/2]).

-export([existsMo/2,
	 countMoChildren/3,
	 getMoAttributes/3,
	 setMoAttributes/3,
	 createMo/5]).

-export([get_current_sw_version/0]).

-export([print_inventory/0, print_inventory/1]).
 
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
    mnesia:write(SwVersion#swVersion{timeOfActivation = Time}),

    #'ProductData'{productName = Name,
		   productNumber = Id,
		   productRevision = Rev} = 
	SwVersion#swVersion.administrativeData,

    Msg = "Running upgrade package "++Name++" "++Id++" "++Rev,
    write_log("SwmLog", "SwInventory", info, Msg),
    
    [log_sw_item(SwItemMoRef)||SwItemMoRef<-
				   lists:sort(SwVersion#swVersion.consistsOf)],

    info_msg("~s~n",[Msg]).  

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
    [SwItem] = mnesia:read({swItem, Key}),
    #'ProductData'{productName = Name,
		   productNumber = Id,
		   productRevision = Rev} = 
	SwItem#swItem.administrativeData,

    Msg = "Consists of "++Name++" "++Id++" "++Rev,
    write_log("SwmLog", "SwInventory", info, Msg).


%%% Description:
%%%  Update the swVersion and swItem tables based on the content of the
%%%  $RCS_ROOT/home/$USER/swm and
%%%  $RCS_ROOT/home/$USER/software

update_tables() ->
    case swmLib:db_op(fun() -> do_update_tables() end) of 
	{atomic, _} ->
	    backup(),
	    {atomic, ok};
	{aborted, Reason} ->
	    sysInitI:error_report([{swmInventory, do_update_tables, []},
				       {aborted, Reason}])
    end.
			     

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

%% update_sw_versions(UpFiles) ->
%%     [try update_sw_version(UpFile)
%%      catch T:E ->
%% 	     erlang:error({{T,E}, erlang:get_stacktrace()}, [UpFile])
%%      end||UpFile<-UpFiles].

update_sw_version(UpFile) ->
    RootDir = filename:dirname(UpFile),

    {ConfigurationE, []} = xmerl_scan:file(UpFile),
    ContentInfoE = find_element(contentinfo, ConfigurationE),
    ConsistsOf = make_consists_of(RootDir, ContentInfoE),
    
    SwVersion = read_sw_dates(get_sw_version_pd(ConfigurationE)),
    FinalSwVersion = SwVersion#swVersion{consistsOf=ConsistsOf},
    swmLib:db_op(fun() -> mnesia:write(FinalSwVersion) end),
    FinalSwVersion.

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
	    try make_bundle(RootDir, ProdName, ProdId, ProdVsn)
	    catch error:cxp_metadata_not_found ->
		    %% This CXP may not present due to decoupling
		    %% Therefore only limited data is available (for now)
		    %% Potentially the archive could be picked for metadata
		    %% files, but that's a later improvement
		    %% This also means, if it's a bundle it won't get detected
		    %% jotj 2016-03-21
		    PD = #'ProductData'{
			    productName = ProdName,
			    productNumber = format_product_number(ProdId),
			    productRevision = ProdVsn},
		    ItemId = make_mom_key(ProdId, ProdVsn),
		    SwItem = #swItem{swItemId = {"1","1","1", ItemId},
				     administrativeData = PD,
				     consistsOf = []},
		    swmLib:db_op(fun() -> mnesia:write(SwItem) end)
	    end

    end.

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
    try do_make_bundle(RootDir, ProdName, ProdId, ProdVsn, Bundle)
	    
    catch T:E ->
	    io:format("~p~n",[{T,E}]),
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
    [print_sw_item(Fd, Indent, MoRef)||MoRef<-MoRefs].

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
%% error_msg(Format, Args) ->
%%     sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

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

