%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rmeSdsServer.erl %
%%% @author qostjoa
%%% @copyright Ericsson AB 2017
%%% @version /main/R12A/2

%%% @doc ==Agent implementation for Service Discovery==
%%% This is the agent implementation for Service Discovery MO
%%% 

-module(rmeSdsServer).
-vsn('/main/R12A/2').
-date('2017-11-27').
-author(qostjoa).
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
%%% R12A/1  2017-10-20  qostjoa   Created
%%% R12A/2  2017-10-27  qostjoa   Corrected upgrade logic

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
-include("RmeSdsServer.hrl").
-include("RmeSds.hrl"). % For conversion from old MO

%%% ----------------------------------------------------------
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
deleteMo(DnRev=[_,<<"ServiceDiscoveryServer">>|_], _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table);
deleteMo(_Dn, _Tx) ->
    ok.


table("ServiceDiscoveryServer") -> serviceDiscoveryServer.
types(serviceDiscoveryServer) -> ?serviceDiscoveryServer_types.

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

init_data(upgrade) ->
    case comsaI:has_consul() of
	true -> %Only run this on vRAN nodes
	    try case swmI:is_old_table(serviceDiscoveryServer) of
		    false -> % The serviceDiscoveryServer MO didn't exist before
			ok;
		    true -> 
			convert_table()
		end,
		case swmI:is_old_table(serviceDiscovery) of
		    false ->
			ok;
		    true ->
			NodeType = swmI:node_type(),
			case NodeType of
			    "vSD" ->
				convert_client_table();
			    _ ->
				ok
			end
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

% Remove this when upgrade from RmeSds 2.0 is no longer supported!
convert_client_table() ->
    case swmI:all_objects(serviceDiscovery) of
	[] ->
	    ok;
	[_] ->
	    % Upgrade from RmeSds 2.0, convert to new MO
	    % Old MO will be automatically removed since we do not write it to the new DB
	    [begin
		 [_, _, {'HostAndPort',SdHost,SdPort},
		  LocalAddress, TcMo, NcMo, DC] =
		     tuple_to_list(Obj),
		 % Perform a raw table write to Mnesia
		 Record = {serviceDiscoveryServer,  % Table
			   {"1","1","1"},           % serviceDiscoveryServerId
			   LocalAddress,
			   {'SdCluster', SdHost, SdPort, DC},
			   undefined,               % gsdsStatus
			   undefined,               % members
			   TcMo,                    % trustCategory
			   NcMo},                   % nodeCredential
		 mnesia:dirty_write(Record)
	     end || Obj <- swmI:all_objects(
			     serviceDiscovery)],
	    ok
    end.

convert_table() ->
    case swmI:all_objects(serviceDiscoveryServer) of
	[] ->
	    ok;
	[_Old] ->
	    swmI:copy_old_table(serviceDiscoveryServer),
	    ok
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------




%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
