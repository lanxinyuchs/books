%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comTop.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R10A/R11A/1
%%%
%%% @doc ==Agent implementation for ComTop==
%%% This is the agent implementation for ComTop.

-module(comTop).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R10A/R11A/1').
-date('2017-09-04').
-author('etxjotj').
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
%%% R1A/1      2012-01-10 etxbjca     Created3
%%% R1A/7      2012-09-25 eblakes     Fixed productIdentity
%%% R1A/8      2012-10-01 etxjotj     Bugfix for Transport
%%% R2A/4      2013-04-04 erarafo     Support for upgrade
%%% R2A/6      2013-05-02 etxjotj     Check for supported timezones
%%% R2A/18     2014-06-05 etxpeno     Remove check for supported timezones
%%% R2A/19     2014-06-10 etxjotj     Dialyzer fix
%%% R2A/20     2014-09-18 etxberb     Added validate/3.
%%% R3A/1      2014-10-31 etxjotj     Better error cause for missing callback
%%% R3A/2      2015-03-17 etxtory     getMoAttributes fix
%%% R11A/1     2017-09-04 etxjotj     Replaced swmLib with swmI
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([getMoAttribute/2, nextMo/3, setMoAttribute/4]).
-export([prepare/3,commit/3,finish/3]).
-export([validate/3]).
-export([existsMo/2,
         countMoChildren/3,
         getMoAttributes/3,
         setMoAttributes/3]).
-export([init_data/1]).
-export([set_node_type/2]).
-export([get_managed_element_data/0]).
%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("ComTop.hrl").
-include("comTypes.hrl").

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%% @doc Returns true if the specified instance exists.
existsMo(DnRev, _) ->
    comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))).

%%% @doc Returns the number of MO instances of given class
%%% directly below the specified parent.

countMoChildren(DnRev, Class, _) ->
    comsaGeneric:countMoChildren(DnRev, table(binary_to_list(Class))).

%%% @doc Gets MO attribute values.
%%% The instance is specified by ReversedDn and the attributes to be
%%% fetched are given by the AttrNames argument.
getMoAttributes(AttrNames, DnRev, TxHandle) ->
    case comsaGeneric:existsMo(DnRev, table(comsaGeneric:class(DnRev))) of
	true  ->
	    [getMoAttribute([AN | DnRev], TxHandle) || AN <- AttrNames];
	false ->
	    []
    end.

%% Real time values are handle specifically
getMoAttribute([<<"localDateTime">>, _, <<"ManagedElement">>], _) ->
    TimeStr = comsaLib:iso_time(os:timestamp(), extended_zonefree),
    ?STRING(list_to_binary(TimeStr));
getMoAttribute([<<"dateTimeOffset">>, _, <<"ManagedElement">>], _) ->
    DiffStr = comsaLib:time_offset(os:timestamp()),
    ?STRING(list_to_binary(DiffStr));

%% getMoAttribute([<<"productIdentity">>|DnRev], _) ->
%%     Key = comsaGeneric:dnrev_to_key(DnRev),
%%     case mnesia:read(managedElement, Key) of
%% 	[] ->
%% 	    undefined;
%% 	[Obj] ->
%% 	    Struct = Obj#managedElement.productIdentity,
%% 	    comsaGeneric:format_struct(Struct, ?'ProductIdentity_types')
%%     end;

getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).

nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

setMoAttributes(Attrs, DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), Attrs).    
%% setMoAttributes(Attrs, DnRev, _) ->
%%     case validate(Attrs) of
%% 	ok ->
%% 	    Table = table(comsaGeneric:class(DnRev)),
%% 	    comsaGeneric:set(DnRev, Table, types(Table), Attrs);
%% 	{error, Reason} ->
%% 	    {error, Reason}
%%     end.

%% setMoAttribute([AttributeB= <<"productIdentity">>|DnRev], TypeAndValue, _, _) ->
%%     Table = table(comsaGeneric:class(DnRev)),
%%     comsaGeneric:set_struct(DnRev, AttributeB, Table, types(Table), TypeAndValue, 'ProductIdentity',
%% 	   ?'ProductIdentity_types');

setMoAttribute([AttributeB= <<"timeZone">>|DnRev], TypeAndValue, _, _) ->
    %% {_, TimeZoneB} = TypeAndValue,
    %% TimeZone = binary_to_list(TimeZoneB),
    %% Path = filename:join(["/usr", "share", "zoneinfo", TimeZone]),
    %% case filelib:is_file(Path) of
    %% 	true ->
    %% 	    Table = table(comsaGeneric:class(DnRev)),
    %% 	    comsaGeneric:set(DnRev, AttributeB, Table, types(Table),
    %% 			     TypeAndValue);
    %% 	false ->
    %% 	    {error, "Not supported timezone: "++TimeZone}
    %% end;
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, AttributeB, Table, types(Table), TypeAndValue);

%setMoAttribute([Attribute|DnRev], TypeAndValue, Internal, TxHandle) ->
setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).

validate(_DN, User, _Tx) ->
    {ok,User}.
prepare(_DN, User, _Tx) ->
    {ok,User}.
commit(_DN, User, _Tx) ->
    {ok,User}.
finish(_DN, _User, _Tx) ->
    ok.

table("ManagedElement") -> managedElement;
table("SystemFunctions") -> systemFunctions;
table("Transport") -> transport;
table("Legacy") -> legacy;
table(Class) -> 
    %% This faults make comTop crash even though code is missing in
    %% other blocks, so make a proper printout.
    sysInitI:error_msg(
      "~w: A comte callback must be registered for ~p~n",[?MODULE, Class]),
    throw({error, {no_comte_callback_registered, Class}}).

types(managedElement) -> ?managedElement_types;
types(systemFunctions) -> ?systemFunctions_types;
types(transport) -> ?transport_types;
types(legacy) -> ?legacy_types.

init_data(upgrade) ->
    swmI:copy_old_table(managedElement),
    swmI:copy_old_table(systemFunctions),
    swmI:copy_old_table(transport);

init_data(fromScratch) ->

    TimeZone = case sysEnv:rcs_mode_2() of
		   simulated -> "Europe/Stockholm";
		   _ -> "UTC"
	       end,

    mnesia:dirty_write(#managedElement{managedElementId = {"1"},
				       timeZone = TimeZone,
				       networkManagedElementId = undefined,
				       managedElementType="",
				       release=""
				      }
		       ),
    mnesia:dirty_write(#systemFunctions{systemFunctionsId = {"1", "1"}}),
    mnesia:dirty_write(#transport{transportId = {"1","1"}}).

%%% ----------------------------------------------------------
%%% @doc Set the ManagedElement node type and release attributes from SWM
%%% @end
%%% ----------------------------------------------------------

set_node_type(NodeType, Release) ->
    Fun = fun() ->
		  [ME] = mnesia:wread({managedElement, {"1"}}),
		  mnesia:write(ME#managedElement{managedElementType=NodeType,
						 release=Release})
	  end,
    case mnesia:is_transaction() of
	true -> Fun();
	false ->
	    {atomic, ok} = mnesia:transaction(Fun),
	    ok
    end.

%%% ----------------------------------------------------------
%%% @doc Get managed element data for PM ROP files and Backup metadata
%%% @end
%%% ----------------------------------------------------------
get_managed_element_data() ->
    {atomic, [ME]} =
	mnesia:transaction(
	  fun() ->
		  mnesia:read({managedElement, {"1"}})
	  end),
    [{dnPrefix, ME#managedElement.dnPrefix},
     {managedElementType, ME#managedElement.managedElementType},
     {networkManagedElementId, ME#managedElement.networkManagedElementId},
     {siteLocation, ME#managedElement.siteLocation},
     {userLabel, ME#managedElement.userLabel}].

%% validate([{<<"timeZone">>, {_, TimeZone}}|Rest]) ->
%%     Path =
%% 	filename:join(["/usr", "share", "zoneinfo", binary_to_list(TimeZone)]),
%%     case filelib:is_file(Path) of
%% 	true ->
%% 	    validate(Rest);
%% 	false ->
%% 	    {error, "Not supported timezone: "++binary_to_list(TimeZone)}
%%     end;
%% validate([{_, _}|Rest]) ->
%%     validate(Rest);
%% validate([]) ->
%%     ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
