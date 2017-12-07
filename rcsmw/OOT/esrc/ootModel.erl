%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 1.    BASIC INFORMATION
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 1.1   MODULE INFORMATION
%% ###-----------------------------------------------------------------------###
%% %CCaseFile:	ootModel.erl %
%% @author etxpeno
%% @copyright Ericsson AB 2017
%% @version /main/R9A/3

%% @doc
%% Interface module for the class OamIpSupport.
%%
%% @end
%% ###=======================================================================###
%% # 1.2   MODULE DEFINITION
%% ###-----------------------------------------------------------------------###
-module(ootModel).
-vsn('/main/R9A/3').
-date('2017-02-21').
-author(etxpeno).

%% ###=======================================================================###
%% # 1.3   LEGAL RIGHTS
%% ###-----------------------------------------------------------------------###
%% %CCaseTemplateFile:  module.erl %
%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%
%% %CCaseCopyrightBegin%
%% Copyright (c) Ericsson AB 2017 All rights reserved.
%%
%% The information in this document is the property of Ericsson.
%%
%% Except as specifically authorized in writing by Ericsson, the
%% receiver of this document shall keep the information contained
%% herein confidential and shall protect the same in whole or in
%% part from disclosure and dissemination to third parties.
%%
%% Disclosure and disseminations to the receivers employees shall
%% only be made on a strict need to know basis.
%% %CCaseCopyrightEnd%

%% ###=======================================================================###
%% # 1.4   REVISION LOG
%% ###-----------------------------------------------------------------------###
%% Rev     Date       Name     What
%% ----    ---------- -------  -------------------------------------------------
%% R9A/1   2017-02-20 etxpeno  Created.
%% ------  ---------- -------  ---END-OF-LOG------------------------------------

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 2.    INTERNAL DEFINITIONS
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%% ###-----------------------------------------------------------------------###

%% ###-----------------------------------------------------------------------###
%% # 2.1.1 Functions used by ootDataInit
%% ###-----------------------------------------------------------------------###

-export([init/1, init_data/0, post_init/0]).

%% ###-----------------------------------------------------------------------###
%% # 2.1.2 Functions used by the COMTE API
%% ###-----------------------------------------------------------------------###

-export([prepare/3,
         commit/3,
         finish/3,
         getMoAttributes/3,
         getMoAttribute/2,
         nextMo/3,
         setMoAttribute/4,
         setMoAttributes/3,
         existsMo/2,
         countMoChildren/3]).  %% Needed?

%% ###-----------------------------------------------------------------------###
%% # 2.1.3 Functions used by ootI
%% ###-----------------------------------------------------------------------###

-export([is_change_status_changed/1]).

%% ###-----------------------------------------------------------------------###
%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###


%% ###=======================================================================###
%% # 2.3   IMPORT OF DEFINITIONS
%% ###-----------------------------------------------------------------------###

-include("RmeOamIpSupport_mp.hrl").

%% ###=======================================================================###
%% # 2.4   LOCAL DEFINITION OF MACROS
%% ###-----------------------------------------------------------------------###


%% ###=======================================================================###
%% # 2.5   LOCAL DEFINITION OF RECORDS
%% ###-----------------------------------------------------------------------###


%% ###=======================================================================###
%% # 2.6   LOCAL DEFINITION OF TYPES
%% ###-----------------------------------------------------------------------###


%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 3.    CODE
%% ###-----------------------------------------------------------------------###
%% ###=======================================================================###
%% # 3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%% ###-----------------------------------------------------------------------###

init(DbNodes) ->
    MoTables = [{oamIpSupport, ?oamIpSupport_types}],
    ok = create_mo_table(MoTables, DbNodes),
    ok.

init_data() ->
    case swmI:is_upgrade_ongoing() of
        false -> %% Initial installation
	    ok = populate_oamIpSupport();
        true -> %% Upgrade
	    ok = upgrade_oamIpSupport()
    end.

post_init() ->
    Root = ["ManagedElement", "NodeSupport", "OamIpSupport"],
    comsaLib:register_callback(Root, ootModel),
    ok.

prepare(_DN, User, _Tx) ->
    {ok, User}.

commit(_DN, User, _Tx) ->
    {ok, User}.

finish(_DN, _User, _Tx) ->
    ok.

getMoAttributes(AttrNameList, DnRev, TransId) ->
    [getMoAttribute([AttrName | DnRev], TransId) || AttrName <- AttrNameList].

getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).

nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

setMoAttributes(Attrs, DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), Attrs).

setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).

existsMo(DnRev, _Tx) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:existsMo(DnRev, Table).

countMoChildren(ReversedDn, ClassName, _Tx) ->
    comsaGeneric:countMoChildren(ReversedDn, ClassName).

is_change_status_changed(IpAddressChangeStatus) ->
    OamIpSupportId = {"1", "1", "1"},
    case mnesia:dirty_read(oamIpSupport, OamIpSupportId) of
	[#oamIpSupport{ipAddressChangeStatus = IpAddressChangeStatus}] ->
	    true;
	_ ->
	    false
    end.

%% ###=======================================================================###
%% # 3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###


%% ###=======================================================================###
%% # 3.3   CODE FOR INTERNAL FUNCTIONS
%% ###-----------------------------------------------------------------------###

%% ###-----------------------------------------------------------------------###
%% # 3.3.1 Help Functions
%% ###-----------------------------------------------------------------------###

create_mo_table([], _DbNodes) ->
    ok;
create_mo_table([{Table, Types}| Rest], DbNodes) ->
    Fields = [Field || {Field, _} <- Types],
    {atomic, ok} =
        clhI:mnesia_create_table(Table, [{type, set},
                                         {disc_copies, DbNodes},
                                         {attributes, Fields} |
                                         add_clh_option(Table)]),
    create_mo_table(Rest, DbNodes).

add_clh_option(_) ->
    [].

populate_oamIpSupport() ->
    %% ManagedElement=1, NodeSupport=1, OamIpSupport=1,
    OamIpSupportId = {"1", "1", "1"},
    Rec = #oamIpSupport{oamIpSupportId = OamIpSupportId},
    ok = mnesia:dirty_write(Rec).

upgrade_oamIpSupport() ->
    [swmI:copy_old_table(Tab)||Tab<-[oamIpSupport]],

    case mnesia:dirty_first(oamIpSupport) of
	'$end_of_table' ->
	    %% Upgrading from a version where this MO is not present
	    populate_oamIpSupport();
	_ ->
	    %% Object already exists
	    ok
    end.

%%% ----------------------------------------------------------
%%% Conversion table & types
%%% ----------------------------------------------------------
table("OamIpSupport") -> oamIpSupport.

types(oamIpSupport)   -> ?oamIpSupport_types.

%% ###=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=###
%% # 4     CODE FOR TEMPORARY CORRECTIONS
%% ###-----------------------------------------------------------------------###
