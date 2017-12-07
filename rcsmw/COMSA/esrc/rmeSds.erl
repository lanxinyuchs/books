%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rmeSds.erl %
%%% @author qostjoa
%%% @copyright Ericsson AB 2017
%%% @version /main/R8A/R9A/R11A/R12A/3

%%% @doc ==Agent implementation for Service Discovery==
%%% This is the agent implementation for Service Discovery MO
%%% 

-module(rmeSds).
-vsn('/main/R8A/R9A/R11A/R12A/3').
-date('2017-11-28').
-author(etxarnu).
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% Rev     Date        Name      What
%%% -----   -------     --------  ------------------------
%%% R8A/1   2017-01-16  etxarnu   Created
%%% R9A/1   2017-01-20  etxarnu   Handle deleteMo
%%%                               Do nothing at init_data(fromScratch)
%%% R9A/2   2017-02-27  etxarnu   Handle upgrade
%%% R9A/3   2017-03-06  etxarnu   HV69056: Only run init_data(upgrade) on 5G nodes
%%% R9A/4   2017-04-07  etxarnu   RmeSds version 2.0 updates
%%% R11A/1  2017-09-04  etxarnu   Handle upgrade from R8A 
%%% R11A/2  2017-09-07  etxarnu   Fix for Handle upgrade from R8A 
%%% R11A/3  2017-11-15  etxjotj   HW44638 fix for incomplete MO entry
%%% R11A/4  2017-11-16  etxarnu   Bug fix.
%%% R12A/1  2017-11-16  qostjoa   RmeSds version 3.0 updates, split MOM in two MOs, server and client
%%% R12A/2  2017-11-27  qostjoa   Revert to RmeSds 2.0
%%% R12A/3  2017-11-28  qostjoa   HW47044, add a missing case-clause in the fix for HW44638
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([getMoAttribute/2, nextMo/3]).
-export([prepare/3,commit/3,finish/3]).
-export([validate/3]).
-export([createMo/4,setMoAttribute/4,deleteMo/2]).
-export([existsMo/2,
         countMoChildren/3,
         getMoAttributes/3,
         setMoAttributes/3,
         createMo/5]).
-export([init_data/1]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-include("RmeSds.hrl").

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


getMoAttribute([Attribute|DnRev], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:get(DnRev, Attribute, Table, types(Table)).

%% @doc Get the next key for the MO given by the Dn
nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

setMoAttributes(Attrs, DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), Attrs).

%% @doc Set the attribute given by the Attribute to TypeAndValue.
setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).

%% @doc Create the MO given by the ClassName with the given
%%   KeyValue and InitAttrs.
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

%% @doc Delete the MO given by the Dn. 
deleteMo(DnRev=[_,<<"ServiceDiscovery">>|_], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table);
deleteMo(_Dn, _Tx) ->
    ok.


table("ServiceDiscovery") -> serviceDiscovery.
types(serviceDiscovery) -> ?serviceDiscovery_types.

%% @doc Validate an ECIM MO. 
validate(_DN, User, _Tx) ->
    {ok,User}.
%% @doc Prepare an ECIM MO for being commited.
prepare(_DN, User, _Tx) ->
    {ok,User}.
%% @doc Commit an transaction.
commit(_DN, User, _Tx) ->
    {ok,User}.
%% @doc Called after the transaction is commited.
finish(_DN, _User, _Tx) ->
    ok.


values([{Name, {_, Value}} | Tail]) ->
    [{Name, Value} | values(Tail)];
values([Attr | Tail]) ->
    [Attr | values(Tail)];
values([]) ->
    [].

%% HW44638 make sure no 'undefined' values in GsdsAddress and LocalAddress
init_data(upgrade) ->
    case comsaServDiscServer:has_consul() of
	true -> %Only run this on 5G nodes
	    try case swmI:is_old_table(serviceDiscovery) of
		    false -> % The serviceDiscovery didn't exist before
			ok;
		    true -> 
			convert_table()
		end
	    catch
		throw : ok -> ok;
		ErrClass : ErrReason ->
		    Err = {ErrClass, [ErrReason | erlang:get_stacktrace()]},
		    sysInitI:error_report([{?MODULE,init_data , [upgrade]},
					   Err]),
		    exit(ErrReason)
	    end;
	false ->
	    ok
    end;
init_data(fromScratch) ->
    ok.

convert_table() ->
    case swmI:all_objects(serviceDiscovery) of
	[] ->
	    ok;
	[Old] ->
	    case tuple_to_list(Old) of 
		[_, _, GsdsAddress, LocalAddress|_] when GsdsAddress == undefined;     
							 LocalAddress == undefined ->
		    %% Incorrectly formatted record -> remove
		    throw(ok);
		[_|_] ->
		    ok
	    end,

	    case swmI:is_attributes_in_old_record(serviceDiscovery, [datacenter]) of
		true ->  %latest version: RmeSds 2.0
		    swmI:copy_old_table(serviceDiscovery),
		    throw(ok);
		false ->
		    ok
	    end,

	    case swmI:is_attributes_in_old_record(serviceDiscovery, [trustCategory]) of
		true -> %latest addition: RmeSds 1.1 -> 2.0
		    Added = [{datacenter,
			      ?serviceDiscovery_datacenter_default}],
		    [begin
			 Record = swmI:transform_obj(Obj,
						     Added),
			 mnesia:dirty_write(Record)
		     end || Obj <- swmI:all_objects(
				     serviceDiscovery)];

		false -> %previous addition: RmeSds 1.0 -> 2.0
		    Added = [{trustCategory, undefined},
			     {nodeCredential, undefined},
			     {datacenter,?serviceDiscovery_datacenter_default}],
		    [begin
			 Record = swmI:transform_obj(Obj, Added),
			 mnesia:dirty_write(Record)
		     end || Obj <- swmI:all_objects(serviceDiscovery)]
	    end
    end.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------




%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
