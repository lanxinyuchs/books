%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comSnmp.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R5A/R6A/R10A/R11A/2

%%% @doc ==Agent implementation for SNMP==
%%% This is the agent implementation the SNMP model.
%%% The data is then forward to ComEA for configuration the net-snmp library

-module(comSnmp).
-vsn('/main/R1A/R2A/R3A/R5A/R6A/R10A/R11A/2').
-date('2017-09-04').
-author(etxjotj).

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
%%% R1A/1      2012-01-10 etxbjca     Created
%%% R2A/5      2013-01-28 etxarnu     Used dynamic port for snmpAgent in sim
%%% R2A/6      2013-03-01 etxarnu     Read operationalState from DB
%%% R2A/7      2013-04-04 erarafo     Support for upgrade
%%% R2A/13     2014-01-16 etxarnu     types(snmpTargetV3Dtls)  corrected
%%% R2A/14     2014-05-20 etxjotj     SNMP adm state unlocked default HS61624
%%% R2A/15     2014-09-11 etxlg       kick comSnmpFirewall at end of trans
%%% R2A/16     2014-09-18 etxberb     Added validate/3.
%%% R2A/17     2014-09-19 etxlg       don't kick comSnmpFirewall
%%% R3A/1      2014-11-28 etxberb     Added values/1.
%%% R3A/2      2015-02-02 etxlg       Added get_agent_params/0
%%% R3A/2      2015-02-03 etxlg       exported snmp_agent_to_prop_list/1
%%% R3A/4      2015-03-17 etxtory     getMoAttributes fix
%%% R5A/1      2016-01-27 etxjotj     Model name switch
%%% R10A/1     2017-05-16 etxjotj
%%% R10A/2     2017-06-20 etxarnu Removed call to comSnmpDtlsConfig:update_env
%%% R10A/3     2017-07-14 estjako     Removed Snmpv3 over Dtls functions
%%% R11A/2     2017-08-04 etxjotj Replaced swmLib with swmI
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([getMoAttribute/2, nextMo/3]).
-export([createMo/4,setMoAttribute/4,deleteMo/2]).
-export([prepare/3,commit/3,finish/3]).
-export([validate/3]).

-export([existsMo/2,
         countMoChildren/3,
         getMoAttributes/3,
         setMoAttributes/3,
         createMo/5]).

-export([init_data/1]).

-export([get_agent_params/0]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%comsaServer uses this to tranlate from record(s) to -> list
-export([snmp_agent_to_prop_list/1]).
-include("RcsSnmp.hrl").
-include("ComTop.hrl").

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

nextMo(Dn, Key, _) ->
    comsaGeneric:nextMo(table(binary_to_list(hd(Dn))), Dn, Key).

setMoAttributes(Attrs, DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Table, types(Table), Attrs).

%% un-comment later
%% when agentAddressDtls is set, configure DTLS
%%TODO what if attribute is set 2 times???
%%setMoAttribute([<<"agentAddressDtls">>|DnRev], TypeAndValue, _, _)->
%%        %%TODO perform check if attributes are set
%%         init_dtls(),
%%        Attribute = [<<"agentAddressDtls">>],
%%        Table = table(comsaGeneric:class(DnRev)),
%%        comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue);

setMoAttribute([Attribute|DnRev], TypeAndValue, _, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:set(DnRev, Attribute, Table, types(Table), TypeAndValue).


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

deleteMo(DnRev, _) ->
    Table = table(comsaGeneric:class(DnRev)),
    comsaGeneric:delete(DnRev, Table).

get_agent_params() ->
    %it is possible to have multiples of agentAddress, but then the
    %netnamespace solution doesn't work. It only works if there is one
    %agent and that one is listening on 'any', i.e. 0.0.0.0
    {atomic, [Snmp]} =
	mnesia:transaction(
		fun() -> mnesia:read(snmp, {"1","1","1","1"}) end),
    snmp_agent_to_prop_list(Snmp).


snmp_agent_to_prop_list(#snmp{agentAddress = [Agent | _]}) ->
    [{host, Agent#'HostAndPort'.host},
     {port, Agent#'HostAndPort'.port}];
snmp_agent_to_prop_list(#snmp{agentAddress = undefined}) ->
    [].

table("Snmp") -> snmp;
table("SnmpTargetV1") -> snmpTargetV1;
table("SnmpTargetV2C") -> snmpTargetV2C;
table("SnmpTargetV3") -> snmpTargetV3;
table("SnmpViewV1") ->  snmpViewV1;
table("SnmpViewV2C") -> snmpViewV2C;
table("SnmpViewV3") -> snmpViewV3;
table("SnmpTargetV3Dtls") -> snmpTargetV3Dtls.

types(snmp) -> ?snmp_types;
types(snmpTargetV1) -> ?snmpTargetV1_types;
types(snmpTargetV2C) -> ?snmpTargetV2C_types;
types(snmpTargetV3) -> ?snmpTargetV3_types;
types(snmpTargetV3Dtls) -> ?snmpTargetV3Dtls_types;
types(snmpViewV1) -> ?snmpViewV1_types;
types(snmpViewV2C) -> ?snmpViewV2C_types;
types(snmpViewV3) -> ?snmpViewV3_types.

values([{Name, {_, Value}} | Tail]) ->
    [{Name, Value} | values(Tail)];
values([Attr | Tail]) ->
    [Attr | values(Tail)];
values([]) ->
    [].

validate(_DN, User, _Tx) ->
    {ok,User}.
prepare(_DN, User, _Tx) ->
    {ok,User}.
commit(_DN, User, _Tx) ->
    {ok,User}.
finish(_DN, _User, _Tx) ->
    ok.

init_data(upgrade) ->
    upgrade_table(snmp),
    upgrade_table(snmpTargetV1),
    upgrade_table(snmpTargetV2C),
    swmI:copy_old_table(snmpTargetV3),
    swmI:copy_old_table(snmpViewV1),
    swmI:copy_old_table(snmpViewV2C),
    swmI:copy_old_table(snmpViewV3);

init_data(fromScratch) ->
    mnesia:dirty_write(
      #snmp{snmpId={"1","1","1","1"},
	    agentAddress=
		case sysEnv:rcs_mode_2() of
		    simulated ->
			[#'HostAndPort'{host=find_local_ip_address(),
					port=sysEnv:get_port_conf(snmpAgent)}];
		    target ->
			undefined;
		    vrcs ->
			undefined
		end,
	    operationalState = ?OperState_DISABLED,
	    administrativeState = ?BasicAdmState_UNLOCKED, %HS61624
	    enableSourceCheckForV1V2C = ?snmp_enableSourceCheckForV1V2C_default,
	    port = ?snmp_port_default,
	    portDtls = ?snmp_portDtls_default}).

upgrade_table(snmp) ->
    Objs = swmI:all_objects(snmp),
    lists:foreach(
      fun({snmp, SnmpId, OperationalState, AdministrativeState, AgentAddress,
	   NodeCredential, AgentAddressDtls, TrustCategory}) ->
	      %% From version 1.2
	      Rec = #snmp{snmpId = SnmpId,
			  operationalState    = OperationalState,
			  administrativeState = AdministrativeState,
			  agentAddress        = AgentAddress,
			  nodeCredential      = NodeCredential,
			  agentAddressDtls    = AgentAddressDtls,
			  trustCategory       = TrustCategory,
			  enableSourceCheckForV1V2C = ?snmp_enableSourceCheckForV1V2C_default,
			  port = ?snmp_port_default,
			  portDtls = ?snmp_portDtls_default},
	      ok = mnesia:dirty_write(Rec);
	 ({snmp, SnmpId, OperationalState, AdministrativeState, AgentAddress,
	   NodeCredential, AgentAddressDtls, TrustCategory,
	   EnableSourceCheckForV1V2C, Port, PortDtls}) ->
	      %% From version 1.4
	      Rec = #snmp{snmpId = SnmpId,
			  operationalState    = OperationalState,
			  administrativeState = AdministrativeState,
			  agentAddress        = AgentAddress,
			  nodeCredential      = NodeCredential,
			  agentAddressDtls    = AgentAddressDtls,
			  trustCategory       = TrustCategory,
			  enableSourceCheckForV1V2C = EnableSourceCheckForV1V2C,
			  port = Port,
			  portDtls = PortDtls},
	      ok = mnesia:dirty_write(Rec)
      end, Objs);
upgrade_table(snmpTargetV1) ->
    Objs = swmI:all_objects(snmpTargetV1),
    lists:foreach(
      fun({snmpTargetV1, SnmpTargetV1Id, Community, AdministrativeState,
	   Address, Port, IsMibWritable, OperationalState}) ->
	      %% From version 1.2
	      Rec = #snmpTargetV1{snmpTargetV1Id = SnmpTargetV1Id,
				  community = Community,
				  administrativeState = AdministrativeState,
				  address = Address,
				  port = Port,
				  isMibWritable = IsMibWritable,
				  operationalState = OperationalState},
	      ok = mnesia:dirty_write(Rec);
	 ({snmpTargetV1, SnmpTargetV1Id, Community, AdministrativeState,
	   Address, Port, IsMibWritable, OperationalState,
	   NetworkPrefixLength}) ->
	      %% From version 1.4
	      Rec = #snmpTargetV1{snmpTargetV1Id = SnmpTargetV1Id,
				  community = Community,
				  administrativeState = AdministrativeState,
				  address = Address,
				  port = Port,
				  isMibWritable = IsMibWritable,
				  operationalState = OperationalState,
				  networkPrefixLength = NetworkPrefixLength},
	      ok = mnesia:dirty_write(Rec)
      end, Objs);
upgrade_table(snmpTargetV2C) ->
    Objs = swmI:all_objects(snmpTargetV2C),
    lists:foreach(
      fun({snmpTargetV2C, SnmpTargetV2CId, Community, InformRetryCount,
	   Address, Port, InformTimeout, TransportMethod,
	   IsMibWritable, OperationalState, AdministrativeState}) ->
	      %% From version 1.2
	      Rec = #snmpTargetV2C{snmpTargetV2CId = SnmpTargetV2CId,
				   community = Community,
				   informRetryCount = InformRetryCount,
				   address = Address,
				   port = Port,
				   informTimeout = InformTimeout,
				   transportMethod = TransportMethod,
				   isMibWritable = IsMibWritable,
				   operationalState = OperationalState,
				   administrativeState = AdministrativeState},
	      ok = mnesia:dirty_write(Rec);
	 ({snmpTargetV2C, SnmpTargetV2CId, Community, InformRetryCount,
	   Address, Port, InformTimeout, TransportMethod,
	   IsMibWritable, OperationalState, AdministrativeState,
	   NetworkPrefixLength}) ->
	      %% From version 1.4
	      Rec = #snmpTargetV2C{snmpTargetV2CId = SnmpTargetV2CId,
				   community = Community,
				   informRetryCount = InformRetryCount,
				   address = Address,
				   port = Port,
				   informTimeout = InformTimeout,
				   transportMethod = TransportMethod,
				   isMibWritable = IsMibWritable,
				   operationalState = OperationalState,
				   administrativeState = AdministrativeState,
				   networkPrefixLength = NetworkPrefixLength},
	      ok = mnesia:dirty_write(Rec)
      end, Objs).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
find_local_ip_address() ->
    {ok,Addrs}=inet:getifaddrs(),
    find_addr(Addrs).

find_addr([]) ->
    "";
find_addr([{N,Opt}|T]) ->
    Addrs = [{N,Ip} || {addr,Ip} <- Opt,
		       tuple_size(Ip) == 4,
		       Ip /= {127,0,0,1},
		       Ip /= {127,0,0,2}],
    case Addrs of
	[] ->
	  find_addr(T)  ;
	[{_,A}|_] ->
	    format_ip(A)
    end.

format_ip({A,B,C,D}) ->
    lists:flatten(io_lib:format("~p.~p.~p.~p",[A,B,C,D])).
