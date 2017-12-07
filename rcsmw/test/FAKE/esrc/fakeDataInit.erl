%%% ----------------------------------------------------------
%%% %CCaseFile:	fakeDataInit.erl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(fakeDataInit).
-vsn('/main/R5A/R9A/1').
-date('2017-04-05').
-author('etxlg').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
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
%%% R5A/1      2016-01-22 etxpeno     First version
%%% R5A/2      2016-01-26 etxpeno     correction of delete_hwItem/1
%%% R9A/1      2017-04-05 etxlg	      Add NR into this application
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([instPhParallel_init/1,
         instPhParallel_init_data/0,
         instPhParallel_post_init/0]).

-export([children/0]).

-export([create_hwItem/2, delete_hwItem/1]).

-export([nextMo/3, existsMo/2, getMoAttributes/3]).
-export([setMoAttributes/3]).
-export([finish/3]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RcsHwIM_mp.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

instPhParallel_init(DbNodes) ->
    Tables = [{hwInventory, ?hwInventory_types},
	      {hwItem, ?hwItem_types}],
    ok = create_tables(Tables, DbNodes),
    catch nr:instPhParallel_init(DbNodes),
    ok.

instPhParallel_init_data() ->
    {atomic, ok} = mnesia:transaction(fun do_init_data_tables/0),
    catch nr:instPhParallel_init_data(),
    ok.

instPhParallel_post_init() ->
    ObjectPath = ["ManagedElement", "SystemFunctions", "HwInventory"],
    ok = comsaI:register_callback(ObjectPath, ?MODULE),
    catch nr:instPhSeqBeg_post_init(),
    ok.

children() ->
    try nr:children()
    catch
	_:_ -> {ok, []}
    end.

create_hwItem(I, Props) ->
    HwItemId = {"1", "1", "1", I},
    ProductName     = get_property(productName,     Props),
    ProductNumber   = get_property(productNumber,   Props),
    ProductRevision = get_property(productRevision, Props),
    ProductionDate  = get_property(productionDate,  Props),
    Description     = get_property(description,     Props),
    Type            = get_property(type,            Props),
    ProductData = #'ProductData'{productName     = ProductName,
				 productNumber   = ProductNumber,
				 productRevision = ProductRevision,
				 productionDate  = ProductionDate,
				 description     = Description,
				 type            = Type},
    Rec = #hwItem{hwItemId    = HwItemId,
		  productData = ProductData},
    ok = mnesia:write(Rec).

delete_hwItem(I) ->
    HwItemId = {"1", "1", "1", I},
    ok = mnesia:delete({hwItem, HwItemId}).

%%% @doc Gets MO attribute values.
%%% The instance is specified by ReversedDn and the attributes to be
%%% fetched are given by the AttrNames argument.
%%% -spec getMoAttributes([binary()], [binary()], integer()) -> [com_value()].
getMoAttributes(AttrNameList, DnRev, _TransId) ->
    Table = table(comsaGeneric:class(DnRev)),
    case comsaGeneric:existsMo(DnRev, Table) of
        false ->
            [];
        true ->
	    Types = types(Table),
	    comsaGeneric:get(DnRev, AttrNameList, Table, Types)
    end.

%%% @doc Gets the next MO
-spec nextMo([binary()], undefined|tuple(), integer()) -> {ok, undefined|tuple()
							  }.
nextMo(Dn, Key, _TransId) ->
    Table = table(binary_to_list(hd(Dn))),
    comsaGeneric:nextMo(Table, Dn, Key).

%%% @doc Returns true if the specified instance exists.
-spec existsMo([binary()], integer()) -> boolean().
existsMo(DnRev, _TransId) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:existsMo(DnRev, Table).

setMoAttributes(Attrs, DnRev, _TransId) ->
    Table = table(comsaGeneric:class(DnRev)),
    Types = types(Table),
    comsaGeneric:set(DnRev, Table, Types, Attrs).

finish(_DN, _User, _TransId) ->
    ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

create_tables(Tables, DbNodes) ->
    lists:foreach(
      fun({Table, Types}) ->
	      Fields = [Field || {Field, _} <- Types],
	      Options = [{type, set},
			 {disc_copies, DbNodes},
			 {attributes, Fields} | add_clh_option(Table)],
	      {atomic, ok} = clhI:mnesia_create_table(Table, Options)
      end, Tables),
    ok.

%%% ###########################################################################
%%% add_clh_option
%%%
%%% Add option clh_changeCopyType_installation for large tables in order for
%%% CLH to optimize disc operations. See also edoc for clhI.
%%%
%%% ###=====================================================================###
add_clh_option(_) ->
    [].

table("HwInventory") -> hwInventory;
table("HwItem")      -> hwItem.

types(hwInventory) -> ?hwInventory_types;
types(hwItem)      -> ?hwItem_types.

do_init_data_tables() ->
    IsUpgradeOngoing = swmI:is_upgrade_ongoing(),
    do_init_data_tables(IsUpgradeOngoing).

do_init_data_tables(true) ->
    %% upgrade is ongoing

    swmI:copy_old_table(hwInventory),
    swmI:copy_old_table(hwItem),

    ok;
do_init_data_tables(false) ->
    %% no upgrade is ongoing

    %% create MO "ManagedElement=1,SystemFunctions=1,HwInventory=1"
    ok = create_hwInventory(),

    %% create MO "ManagedElement=1,SystemFunctions=1,HwInventory=1,HwItem=1"
    %% This is normally done by a non RCS application, but is needed when that
    %% application is not included in the UP
    ok = create_hwItem().

create_hwInventory() ->
    HwInventoryId = {"1", "1", "1"},
    Rec = #hwInventory{hwInventoryId = HwInventoryId},
    ok = mnesia:write(Rec).

create_hwItem() ->
    PID = eqs_pri_service:get_product_information_data(),
    Idx = "1",
    F = fun({productDate, Value}, Map) -> Map#{productionDate => Value};
	   ({Key,         Value}, Map) -> Map#{Key            => Value}
	end,
    Acc = #{type=>"FieldReplaceableUnit"},
    Props = lists:foldl(F, Acc, PID),
    ok = create_hwItem(Idx, Props).

get_property(Key, Map) ->
    case maps:find(Key, Map) of
	{ok, Value} when is_list(Value) ->
	    Value;
	{ok, Value} when is_binary(Value) ->
	    binary_to_list(Value);
	_ ->
	    undefined
    end.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
