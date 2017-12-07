%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ootImm.erl %
%%% Author:	eolaand
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
%%% @private
-module(ootImm).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/3').
-date('2017-11-01').
-author('eolaand').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	ootImm.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2017 All rights reserved.
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
%%% R2A/1      2014-01-23 eolaand     Created
%%% R3A/1      2014-12-10 etxlg       Print return from  ootImmLib
%%% R3A/2      2014-12-17 etxjotj     Degraded printout from error to info
%%% R4A/1      2015-08-26 eolaand     Handle new attribute accessPoint
%%% R4A/2      2015-09-02 eolaand     Handle new instance of OamAccessPoint
%%% R4A/3      2015-09-07 eolaand     Validate attrs for alt OamAccessPoint
%%% R4A/4      2015-09-07 eolaand     Use accessPoint as primary value for
%%%                                   IP address DN. Use ipv4address only if
%%%                                   accessPoint is undefined
%%% R4A/5      2015-09-10 eolaand     Move some fcns to ootLib
%%% R4A/10     2015-09-15 eolaand     Add fcn create_oap_alt
%%% R4A/11     2015-09-16 eolaand     Add fcn set_oap_ecim_attrs
%%% R4A/12     2015-09-18 eolaand     Add mirroring of attributes to new ECIM
%%%                                   model and new attribute accessPoint
%%% R4A/13     2015-09-22 eolaand     Add create and delete fcns for OAP
%%% R4A/14     2015-09-25 eolaand     Do not allow empty value for dscp.
%%%                                   Allow only value of 0 for alt OAP dscp
%%% R4A/15     2015-10-01 eolaand     Add create fcns with extra attributes
%%% R4A/16     2015-10-09 eolaand     Check consistency between OAP=1 and
%%%                                   OAP=Alternative at modify.
%%% R4A/17     2015-10-14 eolaand     Add get functions for ip address DN
%%%                                   Add more checks in preparation for IPv6.
%%% R4A/18     2015-10-22 eolaand     Remove mirroring of deprecated attributes
%%% R4A/20     2015-11-20 eolaand     Move upgrade functions from ootServer and
%%%                                   add functions to remove duplicated
%%%                                   reservedBy references.
%%% R5A/1      2015-12-04 eolaand     IPv6 updates
%%% R5A/2      2016-03-01 eolaand     HU62357, use both OiHandle and CcbId as
%%%                                   key when storing ccb data.
%%% R5A/3      2016-04-14 eolaand     Use cached value when checking existing
%%%                                   accessPoint DN in validation.
%%% R6A/1      2016-08-11 eolaand     Check that reservedBy actually contains
%%%                                   duplicated references before removing
%%% R7A/1      2016-10-06 erarafo     ICTI request ids gathered in gmf.hrl
%%% R8A/2      2016-12-16 etxpeno     Using GMF official interface
%%% R9A/2      2017-01-30 eolaand     Add functions for IP addresses
%%% R9A/4      2017-02-06 eolaand     Only check and get address attribute from
%%%                                   AddressIPv4/IPv6, not usedAddress.
%%% R9A/5      2017-04-07 eolaand     HV75411 vPP node not accessible after  
%%%                       uabhten     configuring OamAccessPoint=Alternative
%%% R9A/6      2017-04-07 uabhten     Removed error string.               
%%% R10A/2     2017-04-25 eolaand     Remove OAP=Alternative in vrcs env.
%%% R10A/3     2017-04-27 eolaand     Remove OAP=Alternative during upgrade in 
%%%                                   vrcs env.
%%% R10A/5     2017-07-11 egabing     Removed dependency towards TN IMM objects
%%% R11A/1     2017-08-07 eolaand     Remove support for deprecated attributes
%%% R11A/2     2017-08-15 eolaand     Revert removal of support for deprecated 
%%%                                   attributes and add error log instead.
%%% R12A/1     2017-10-30 eolaand     Allow netconf port 830 for all env except
%%%                                   old rcssim on host.
%%% R12A/2     2017-10-30 eolaand     Remove support for deprecated attributes
%%% R12A/3     2017-11-01 eolaand     Revert removal of support for deprecated 
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%%----------------------------------------------------------------------
%% API functions
%%----------------------------------------------------------------------
-export([
	 get_config/0,
	 get_config/1,
	 get_oap_config/0,
	 get_oap_config/1,
	 get_ip_addr_config/0,
	 get_ip_addr_config/1,
	 get_deprecated_ipv4_addr/2,
	 get_port_conf/2,
	 get_acc_point_dn/1,
	 get_acc_point_alt_dn/1,
	 get_ipv4_dn/0,
	 get_ipv4_dn/1,
	 get_ipv4_alt_dn/0,
	 get_ipv4_alt_dn/1,
	 get_dscp/1,
	 create_oap/0,
	 create_oap/1,
	 create_oap/2,
	 create_oap/3,
	 create_oap/4,
	 create_oap_alt/0,
	 create_oap_alt/1,
	 get_oap/0,
	 get_oap_alt/0,
	 delete_oap/0,
	 delete_oap/1,
	 delete_oap_alt/0,
	 delete_oap_alt/1,
	 set_oap_attrs/2,
	 set_oap_attrs/3,
	 set_oap_ao_and_attrs/3,
	 set_oap_ecim_attrs/0,
	 set_oap_ecim_attrs/1,
	 set_oap_ao_and_ecim_attrs/1
	]).

%%----------------------------------------------------------------------
%% Upgrade functions
%%----------------------------------------------------------------------
-export([wait_for_upgrade_transform/0,
	 wait_for_objects/0,
	 check_duplicated_reserved_by/3, % To be removed
	 remove_duplicated_reserved_by/2,
	 rm_obsolete_attr_vals/1]). % To be removed

%%----------------------------------------------------------------------
%% Life-cycle
%%----------------------------------------------------------------------
-export([
         initialize_om/0,
         finalize_om/1,
         initialize_oap_oi/0,
         finalize_oap_oi/1
        ]).

%%----------------------------------------------------------------------
%% OI callback functions
%%----------------------------------------------------------------------
-export([
	 ccb_object_create_2/5,
	 ccb_object_delete/3,
	 ccb_object_modify_2/4,
	 ccb_completed/2,
	 ccb_apply/2,
	 ccb_abort/2,
	 rt_attr_update/3
	]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%% -compile(export_all).

%%% ----------------------------------------------------------
%%% Include files
%%% ----------------------------------------------------------
%% -include("om.hrl").
-include("oot.hrl").
-include("safs_imm_ct_p.hrl").
-include("gmf.hrl").

%%% ----------------------------------------------------------
%%% Macros
%%% ----------------------------------------------------------
-define(OOT_OI_NAME, <<"OotOI">>).
-define(OOT_OI_LISTENER_NAME, <<"@OotOI">>).
-define(TN_AO_NAME, <<"OotTNAO">>).
-define(OAP_ID_BIN_VAL, <<"1">>).
-define(ALT_OAP_ID_BIN_VAL, <<"Alternative">>).
-define(OAP_PORTS, [?OAP_NETCONF_PORT, ?OAP_SSH_PORT]).
-define(OAP_IP_ADDR_DN_ATTRS, [?OAP_ACC_POINT, ?OAP_IPV4_ADDR]).
-define(OAP_OBSOLETE_ATTRS, [?OAP_IPV4_ADDR, 
			     ?OAP_SSH_PORT, 
			     ?OAP_NETCONF_PORT, 
			     ?OAP_DSCP]).

-define(OAP_ECIM_DN(Id),
	case Id of
	    ?OAP_ID_VAL ->
		?ECIM_DN_OAP;
	    ?ALT_OAP_ID_VAL ->
		?ECIM_DN_ALT_OAP
	end).

-define(ERR_STR_ALT_OAP_VRCS, 
	"Not allowed to set attribute accessPoint for "
	"OamAccessPoint=Alternative.").


%%% ----------------------------------------------------------
%%% Records
%%% ----------------------------------------------------------

%% ccb_object_modify_2(_, CcbId, ObjectName,
%% 		    [#safsImmAttrModification_2{modType = Type,
%% 						modAttr = ModAttr}]) ->
%% ObjectName = <<"oamAccessPointId=1,sysMId=1,systemFunctionsId=1">>
%% AttrMods = [{safsImmAttrModification_2,sa_imm_attr_values_replace,
%%                 {safsImmAttrValues_2,<<"sshPort">>,sa_imm_attr_sauint32t,1,
%%                     [{safsImmAttrValue,undefined,37654,undefined,undefined,
%%                          undefined,undefined,undefined,undefined,undefined,
%%                          undefined}]}}]
%%         {attrName,                     % = 1, string
%%          attrValueType,                % = 2, {enum,safsImmValueType}
%%          attrValuesNumber,             % = 3, uint32
%%          attrValues                    % = 4, [{msg,safsImmAttrValue}]

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
-spec get_config() -> Config::list().
get_config() ->
    exec_with_handle(fun get_config/1).


get_config(Handle) ->
    OapConfig = get_config(Handle, ?OAP_DN, ?OAP_ATTRS),
    OapAltConfig = get_alt_oap_config(Handle),
    lists:keysort(1, OapConfig ++ OapAltConfig).


get_alt_oap_config(Handle) ->
    Config = get_config(Handle, ?ALT_OAP_DN, ?OAP_IP_ADDR_DN_ATTRS),
    [{oap_to_alt_key(Key), Val} || {Key, Val} <- Config].


get_oap_config() ->
    exec_with_handle(fun get_oap_config/1).


get_oap_config(Handle) ->
    get_oap_config(Handle, ?OAP_DN, ?OAP_ATTRS).


get_ip_addr_config() ->
    exec_with_handle(fun get_ip_addr_config/1).


get_ip_addr_config(OmHandle) ->
    AccPointDN = get_acc_point_dn(OmHandle),
    OAPIpAddrCfg = get_ip_addr_config(OmHandle, AccPointDN),
    AltAccPointDN = get_acc_point_alt_dn(OmHandle),
    AltOAPIpAddrCfg = 
	[{oap_to_alt_key(Key), Val} || 
	    {Key, Val} <- get_ip_addr_config(OmHandle, AltAccPointDN)],
    lists:keysort(1, OAPIpAddrCfg ++ AltOAPIpAddrCfg).
    
    
get_ip_addr_config(OmHandle, AccPointDN) ->
    AccPointAddr = get_oap_ip_addr(OmHandle, AccPointDN),
    IPV4Addr = get_deprecated_ipv4_addr(AccPointDN, AccPointAddr),
    [{?CNF_ACC_POINT_ADDR, AccPointAddr}, {?CNF_IPV4_ADDR, IPV4Addr}].
    

oap_to_alt_key(?CNF_ACC_POINT_ADDR) ->
    ?CNF_ACC_POINT_ADDR_ALT;

oap_to_alt_key(?CNF_IPV4_ADDR) ->
    ?CNF_IPV4_ADDR_ALT.


get_port_conf(OmHandle, Type) ->
    Attr = ootLib:port_type_to_attr(Type),
    case ootImmLib:get_attr(OmHandle, ?OAP_DN, Attr) of
	{ok, [Port]} ->
	    Port;
	_ ->
	    undefined
    end.


get_acc_point_dn(OmHandle) when is_tuple(OmHandle) ->
    get_acc_point_dn(OmHandle, ?OAP_DN);

get_acc_point_dn(OapDN) when is_binary(OapDN) ->
    F = fun(OmHandle) -> 
		get_acc_point_dn(OmHandle, OapDN)
	end,
    exec_with_handle(F).


get_acc_point_dn(OmHandle, OapDN) ->
    case ootImmLib:get_attr(OmHandle, OapDN, ?OAP_ACC_POINT) of
	{ok, [DN]} ->
	    DN;
	_ ->
	    undefined
    end.


get_acc_point_alt_dn(OmHandle) when is_tuple(OmHandle) ->
    get_acc_point_dn(OmHandle, ?ALT_OAP_DN).


get_ipv4_dn() ->
    OmHandle = ootImmLib:initialize_om_ah(),
    Res = get_ipv4_dn(OmHandle),
    finalize_om(OmHandle),
    Res.


get_ipv4_dn(OmHandle) ->
    get_ipv4_dn(?OAP_IP_ADDR_DN_ATTRS, ?OAP_DN, OmHandle).


get_ipv4_dn([Attr | Attrs], OAPDN, OmHandle) ->
    case ootImmLib:get_attr(OmHandle, OAPDN, Attr) of
	{ok, [DN]} ->
	    DN;
	_ ->
	    get_ipv4_dn(Attrs, OAPDN, OmHandle)
    end;

get_ipv4_dn([], _OAPDN, _OmHandle) ->
    undefined.


get_ipv4_alt_dn() ->
    OmHandle = ootImmLib:initialize_om_ah(),
    Res = get_ipv4_alt_dn(OmHandle),
    finalize_om(OmHandle),
    Res.


get_ipv4_alt_dn(OmHandle) ->
    get_ipv4_dn(?OAP_IP_ADDR_DN_ATTRS, ?ALT_OAP_DN, OmHandle).


get_dscp(OmHandle) ->
    case ootImmLib:get_attr(OmHandle, ?OAP_DN, ?OAP_DSCP) of
	{ok, [DSCP]} ->
	    DSCP;
	_ ->
	    undefined
    end.


create_oap() ->
    exec_as_admin_owner(fun create_oap/1).


create_oap({_OmHandle, _AOHandle, CcbHandle}) ->
    create_oap(CcbHandle);

create_oap(CcbHandle) ->
    create_oap(CcbHandle, ?OAP_ID_VAL).


create_oap(CcbHandle, Id) ->
    create_oap(CcbHandle, Id, undefined).


create_oap(CcbHandle, Id, SshPort) ->
    create_oap(CcbHandle, Id, SshPort, undefined).


create_oap(CcbHandle, Id, SshPort, NetconfPort) ->
    Vals = [{?OAP_ID, string, Id},
	    {?OAP_SSH_PORT, uint32, SshPort},
	    {?OAP_NETCONF_PORT, uint32, NetconfPort}],
    AttrVals = [ootImmLib:build_attr_val(Attr, Type, Val) ||
		{Attr, Type, Val} <- Vals, Val =/= undefined],
    OapDN = ?OAP_ECIM_DN(Id),
    [OapObjId] = ootLib:get_oap_ecim_obj_ids([OapDN]),
    EcimAttrVals = build_oap_ecim_attr_vals(OapObjId, OapDN),
    %% OapExtraAttrs = ootImmLib:build_attr_mods(OapAtvList),
    ootImmLib:create_imm_object_extra_attrs(CcbHandle, ?OAP, ?OAP_PARENT_DN,
					    AttrVals, EcimAttrVals).


delete_oap() ->
    exec_as_admin_owner(fun delete_oap/1).


delete_oap({_OmHandle, _AOHandle, CcbHandle}) ->
    delete_oap(CcbHandle);

delete_oap(CcbHandle) ->
    delete_oap(CcbHandle, ?OAP_DN).


delete_oap(CcbHandle, DN) ->
    ootImmLib:delete_imm_object(CcbHandle, DN).


create_oap_alt() ->
    exec_as_admin_owner(fun create_oap_alt/1).


create_oap_alt({_OmHandle, _AOHandle, CcbHandle}) ->
    create_oap_alt(CcbHandle);

create_oap_alt(CcbHandle) ->
    create_oap(CcbHandle, ?ALT_OAP_ID_VAL).


delete_oap_alt() ->
    exec_as_admin_owner(fun delete_oap_alt/1).


delete_oap_alt({_OmHandle, _AOHandle, CcbHandle}) ->
    delete_oap_alt(CcbHandle);

delete_oap_alt(CcbHandle) ->
    delete_oap(CcbHandle, ?ALT_OAP_DN).


get_oap() ->
    ootImmLib:get_object(?OAP_DN).


get_oap_alt() ->
    ootImmLib:get_object(?ALT_OAP_DN).

%%--------------------------------------------------------------------
%% Function: set_oap_attrs/1
%% Description:
%%--------------------------------------------------------------------
set_oap_attrs(DN, AttrVals) ->
    exec_as_admin_owner(fun(Handle) ->
				set_oap_ao_and_attrs(Handle, DN, AttrVals)
			end).


set_oap_ao_and_attrs({_OmHandle, AOHandle, CcbHandle}, DN, AttrVals) ->
    ok = ootImmLib:admin_owner_set(AOHandle, [DN]),
    set_oap_attrs(CcbHandle, DN, AttrVals).


set_oap_attrs(CcbHandle, DN, AttrVals) ->
    SetAttrVals = [ootImmLib:build_attr_val(Attr, Type, Val) ||
		      {Attr, Type, Val} <- AttrVals],
    OapAttrMods = ootImmLib:build_attr_mods(SetAttrVals),
    ootImmLib:modify_imm_object(CcbHandle, DN, OapAttrMods).

%%--------------------------------------------------------------------
%% Function: set_oap_ecim_attrs/1
%% Description:
%%--------------------------------------------------------------------
set_oap_ecim_attrs() ->
    exec_as_admin_owner(fun set_oap_ao_and_ecim_attrs/1).


set_oap_ao_and_ecim_attrs({_OmHandle, AOHandle, CcbHandle}) ->
    ok = ootImmLib:admin_owner_set(AOHandle, [?OAP_DN, ?ALT_OAP_DN]),
    set_oap_ecim_attrs(CcbHandle).


set_oap_ecim_attrs(CcbHandle) ->
    OAPDNs = [?ECIM_DN_OAP, ?ECIM_DN_ALT_OAP],
    [OapObjId, AltOapObjId] = ootLib:get_oap_ecim_obj_ids(OAPDNs),
    OapAtvList = build_oap_ecim_attr_vals(OapObjId, ?ECIM_DN_OAP),
    OapExtraAttrs = ootImmLib:build_attr_mods(OapAtvList),
    ok = ootImmLib:modify_imm_object_extra_attrs(CcbHandle, ?OAP_DN,
						 OapExtraAttrs),
    AltOapAtvList = build_oap_ecim_attr_vals(AltOapObjId, ?ECIM_DN_ALT_OAP),
    AltOapExtraAttrs = ootImmLib:build_attr_mods(AltOapAtvList),
    ok = ootImmLib:modify_imm_object_extra_attrs(CcbHandle, ?ALT_OAP_DN,
     						 AltOapExtraAttrs).

build_oap_ecim_attr_vals(ObjId, ECIMDN) ->
    [ootImmLib:build_attr_val(?OAP_OBJ_ID, sauint32, ObjId),
     ootImmLib:build_attr_val(?OAP_ECIM_DN, sastring, ECIMDN)].


%%--------------------------------------------------------------------
%% Function: wait_for_upgrade_transform/0
%% Description:
%%--------------------------------------------------------------------
wait_for_upgrade_transform() ->
    RequestId = ?REQUEST_ID_OOT_1,
    SchemaName = ?OAP_SCHEMA,
    TransformFun = fun transform_oap/2,
    ClassNames = [?OAP],
    ok = gmfImmUgIcti:transform_schema(RequestId, SchemaName, ClassNames,
				       TransformFun),

    RequestId2 = ?REQUEST_ID_OOT_2,
    ok = gmfImmUgIcti:waitForDataConversion(RequestId2).

%%--------------------------------------------------------------------
%% Function: wait_for_objects/0
%% Description:
%%--------------------------------------------------------------------
wait_for_objects() ->
    case sysEnv:vrcs() of
	true ->
	    wait_for_objects([?OAP_DN]);
	_False ->
	    wait_for_objects([?OAP_DN, ?ALT_OAP_DN])
    end.


wait_for_objects([DN | NextDNs] = DNs) ->
    timer:sleep(1000),
    case ootImmLib:get_object(DN) of
	{ok, AttrVals} ->
	    sysInitI:info_msg("~p: IMM object ~s available.~n",
			      [?MODULE, ootLib:to_list(DN)]),
	    wait_for_ip_addrs(DN, AttrVals),
	    wait_for_objects(NextDNs);
	_ ->
	    wait_for_objects(DNs)
    end;

wait_for_objects([]) ->
    ok.

wait_for_ip_addrs(OapDN, AttrVals) ->
    case [Val || {Attr, _Type, [Val]} <- AttrVals,
		 Attr =:= ?OAP_IPV4_ADDR orelse Attr =:= ?OAP_ACC_POINT] of
	[IpAddrDN] ->
	    wait_for_ip_addr(OapDN, {?OAP_ACC_POINT, IpAddrDN});
	[IpAddrDN, IpAddrDN] ->
	    wait_for_ip_addr(OapDN, {?OAP_IPV4_ADDR, IpAddrDN});
	_Val ->
	    ok
    end.


wait_for_ip_addr(OapDN, {Attr, IpAddrDN} = AttrVal) ->
    timer:sleep(1000),
    case ootImmLib:get_object(IpAddrDN) of
	{ok, AttrVals} ->
	    sysInitI:info_msg( "~p: IMM object ~s available.~n",
			       [?MODULE, ootLib:to_list(IpAddrDN)]),
	    check_duplicated_reserved_by(Attr, OapDN, IpAddrDN, AttrVals);
	_ ->
	    wait_for_ip_addr(OapDN, AttrVal)
    end.


check_duplicated_reserved_by(?OAP_IPV4_ADDR, _OapDN, IpAddrDN, AttrVals) ->
    case lists:keyfind(?ADDR_IPV4_RESERVED_BY, 1, AttrVals) of
	{_Attr, _Type, Vals} when length(Vals) > 1 ->
	    sysInitI:info_msg( "~p: ~s~nreservedBy~n~p~n",
			       [?MODULE, ootLib:to_list(IpAddrDN), Vals]),
	    %% check_duplicated_reserved_by(OapDN, IpAddrDN, Vals);
	    ok;
	{_Attr, _Type, [Val]} ->
	    sysInitI:info_msg( "~p: ~s~nreservedBy~n~s~n",
			       [?MODULE, ootLib:to_list(IpAddrDN),
				ootLib:to_list(Val)]),
	    ok;
	_False ->
	    sysInitI:warning_msg( "~p: reservedBy unexpectedly empty for~n~s~n",
				  [?MODULE, ootLib:to_list(IpAddrDN)]),
	    ok
    end;

check_duplicated_reserved_by(_Attr, _OapDN, _IpAddrDN, _AttrVals) ->
    ok.


check_duplicated_reserved_by(OapDN, IpAddrDN, Vals) ->
    case lists:delete(OapDN, Vals) of
	Vals ->
	    ok;
	_StrippedVals ->
	    remove_duplicated_reserved_by(OapDN, IpAddrDN)
    end.


%%--------------------------------------------------------------------
%% Function: transform_oap/2
%% Description:
%%--------------------------------------------------------------------
transform_oap({OldSchema, NewSchema}, InstanceGroups) ->
    OldVsn = schema_to_string(OldSchema),
    NewVsn = schema_to_string(NewSchema),
    ?LOG_INFO("Upgrade path ~s -> ~s~n", [OldVsn, NewVsn]),
    sysInitI:info_msg("~p: Upgrade path ~s -> ~s~n", [?MODULE, OldVsn, NewVsn]),
    IsVRCS = sysEnv:vrcs(),
    case OldSchema of
	NewSchema ->
	    %% We should not get here. Instances should be copied by IMM
	    {ok, InstanceGroups};

	_ when IsVRCS, NewSchema == #safsImmCtSchemaVersion{version = 2,
							    %% release = 4,
							    release = 3,
							    correction = 0} ->
	    IGNoAlt = remove_alt_oap(InstanceGroups),
	    %% IGNew = rm_obsolete_attr_vals(IGNoAlt),
	    IGNew = IGNoAlt,
	    log_oap_transform(IGNew, InstanceGroups),
	    {ok, IGNew};
	_ when NewSchema == #safsImmCtSchemaVersion{version = 2,
						    %% release = 4,
						    release = 3,
						    correction = 0} ->
	    IGOapAlt = transform_alt_oap(InstanceGroups),
	    %% IGNew = rm_obsolete_attr_vals(IGOapAlt),
	    %% log_oap_transform(IGNew, InstanceGroups),
	    IGNew = IGOapAlt,
	    log_oap_transform(IGNew, InstanceGroups),
	    {ok, IGNew};
	_ ->
	    {error, ootLib:to_bin("Upgrade path " ++ OldVsn ++ " -> " 
				  ++ NewVsn ++ " not supported")}
    end.


remove_alt_oap([#safsImmCtInstanceGroup{className = ?OAP,
					instances = [_OAP1]}] = IG) ->
    IG;

remove_alt_oap([#safsImmCtInstanceGroup{className = ?OAP,
					instances = Instances} = IG]) ->
    F = fun(#safsImmCtInstance{attrValues = AttrVals}) ->
		case lists:keyfind(?OAP_ID, #safsImmAttrValues_2.attrName, 
				   AttrVals) of
		    #safsImmAttrValues_2{attrValues = [AttrVal]} ->
			?ALT_OAP_ID_BIN_VAL =/= 
			    AttrVal#safsImmAttrValue.sastring;
		    _False ->
			false
		end
	end,
    NewInstances = lists:filter(F, Instances),
    [IG#safsImmCtInstanceGroup{instances = NewInstances}].

    
transform_alt_oap([#safsImmCtInstanceGroup{className = ?OAP,
					   instances = [OAP1]} = IG]) ->
    AttrValues = [ootImmLib:build_attr_val(?OAP_ID, string, ?ALT_OAP_ID_VAL)],
    OAP2 = #safsImmCtInstance{parentName = ?OAP_PARENT_DN,
			      attrValues = AttrValues},
    [IG#safsImmCtInstanceGroup{instances = [OAP2, OAP1]}];

transform_alt_oap([#safsImmCtInstanceGroup{className = ?OAP,
					   instances = Instances} = IG]) ->

    F = fun(#safsImmCtInstance{parentName = ?OAP_PARENT_DN,
			       attrValues = AttrValues} = Inst) ->
		NewAttrVals = update_alt_oap_id(AttrValues),
		Inst#safsImmCtInstance{attrValues = NewAttrVals}
	end,
    case lists:map(F, Instances) of
	Instances ->
	    [IG];
	NewInstances ->
	    [IG#safsImmCtInstanceGroup{instances = NewInstances}]
    end.


update_alt_oap_id(AttrValues) ->
    OldId = ootLib:to_bin(?ALT_OAP_ID_OLD_VAL),
    case ootImmLib:get_attr_val(?OAP_ID, string, AttrValues) of
	OldId ->
	    ootImmLib:store_attr_val(?OAP_ID, string,
				     ?ALT_OAP_ID_VAL, AttrValues);
	_ ->
	    AttrValues
    end.


rm_obsolete_attr_vals([IG])
  when IG#safsImmCtInstanceGroup.className =:= ?OAP ->
    Instances = IG#safsImmCtInstanceGroup.instances,
    F = fun(#safsImmCtInstance{parentName = ?OAP_PARENT_DN,
			       attrValues = AttrValues} = Inst) ->
		NewAttrVals = rm_obsolete_oap_attr_vals(AttrValues),
		Inst#safsImmCtInstance{attrValues = NewAttrVals}
	end,
    case lists:map(F, Instances) of
	Instances ->
	    [IG];
	NewInstances ->
	    [IG#safsImmCtInstanceGroup{instances = NewInstances}]
    end.


rm_obsolete_oap_attr_vals(AttrValues) ->
    case ootImmLib:get_attr_val(?OAP_ID, string, AttrValues) of
    	?OAP_ID_BIN_VAL ->
    	    lists:foldl(fun rm_obsolete_attr_val/2, AttrValues, 
    			?OAP_OBSOLETE_ATTRS);
    	_ ->
    	    AttrValues
    end.


rm_obsolete_attr_val(?OAP_DSCP = Attr, AttrValues) ->
    ootImmLib:store_attr_val(Attr, uint32, 0, AttrValues);

rm_obsolete_attr_val(Attr, AttrValues) ->
    ootImmLib:remove_attr_val(Attr, AttrValues).


log_oap_transform(IGOld, IGOld) ->
    ?LOG_INFO("No need to transform OamAccessPoint~n"
	      "Old InstanceGroups = ~p~n",
	      [IGOld]),
    sysInitI:info_msg("~p: No need to transform OamAccessPoint~n"
		      "Old InstanceGroups = ~p~n",
		      [?MODULE, IGOld]);

log_oap_transform(IGNew, IGOld) ->
    ?LOG_INFO("Transform OamAccessPoint~n"
	      "Old InstanceGroups = ~p~n"
	      "New InstanceGroups = ~p~n",
	      [IGOld, IGNew]),
    sysInitI:info_msg("~p: Transform OamAccessPoint~n"
		      "Old InstanceGroups = ~p~n"
		      "New InstanceGroups = ~p~n",
		      [?MODULE, IGOld, IGNew]).


schema_to_string(#safsImmCtSchemaVersion{version = Version,
					 release = Release,
					 correction = Correction}) ->
    lists:flatten(io_lib:format("~w.~w.~w",[Version, Release, Correction])).

%%--------------------------------------------------------------------
%% Function: remove_duplicated_reserved_by/2
%% Description:
%%--------------------------------------------------------------------
remove_duplicated_reserved_by(OapDN, IpAddrDN) ->
    Handle = ootImmLib:initialize_om_ao_ccb(?TN_AO_NAME),
    try
	remove_duplicated_reserved_by(Handle, OapDN, IpAddrDN)
    catch _:Error ->
	    ST = erlang:get_stacktrace(),
	    sysInitI:error_msg("~p: Failed to remove duplicated reservedBy "
			       "for ~s~n~p~n~p~n",
			       [?MODULE, ootLib:to_list(IpAddrDN), Error, ST])
    end,
    ootImmLib:finalize_om_ao_ccb(Handle).


%%% ----------------------------------------------------------
%%% -type some_method(Parameter : parameterType())->        %#
%%%     ok | error().                                       %#
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
initialize_om() ->
    Handle = ootImmLib:initialize_om_ah(),
    case {ootImmLib:get_object(Handle, ?OAP_DN),
	  ootImmLib:get_object(Handle, ?ALT_OAP_DN)} of
	{{ok, _OAP}, {ok, _AltOAP}} ->
	    {ok, Handle};
	{{ok, _OAP}, _Error} ->
	    sysInitI:warning_msg(
	      "~p: Result of ootImmLib:get_object(~s): ~p~n",
	      [?MODULE, ootLib:to_list(?ALT_OAP_DN), _Error]),
	    {ok, Handle};
	_Error ->
	    sysInitI:info_msg(
	      "~p: Error from ootImmLib:get_object(): ~p~n",
	      [?MODULE, _Error]),
	    ootImmLib:finalize_om_ah(Handle),
	    {error, oap_not_defined}
    end.


finalize_om(undefined) ->
    ok;

finalize_om(Handle) ->
    ootImmLib:finalize_om_ah(Handle).


initialize_oap_oi() ->
    OiHandle = ootImmLib:initialize_oi(?MODULE, ?OOT_OI_NAME),
    case ootImmLib:class_impl_set(OiHandle, ?OAP) of
	Res when Res =:= ok; Res =:= {error, ?ERR_EXIST} ->
	    ok;
	Error ->
	    ok = ootImmLib:finalize_oi(OiHandle),
	    throw({"Failed to set class implementer", Error})
    end,
    OiHandle.



%% ip_addr_attr(?OAP_DN) ->
%%     ?OAP_IPV4_ADDR;

%% ip_addr_attr(?ALT_OAP_DN) ->
%%     ?OAP_ACC_POINT.


finalize_oap_oi(undefined) ->
    ok;

finalize_oap_oi(OiH) ->
    %% ok = ootImmLib:obj_impl_rel(OiH, ?OAP_DN),
    ok = ootImmLib:class_impl_rel(OiH, ?OAP),
    ootImmLib:finalize_oi(OiH).


%%====================================================================
%% Callback functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: ccb_object_create_2/5
%% Description:
%%--------------------------------------------------------------------
ccb_object_create_2(_OiHandle, _CcbId, _ClassName, _ParentName, _Attr) ->
    ok.

%%--------------------------------------------------------------------
%% Function: ccb_object_delete/3
%% Description:
%%--------------------------------------------------------------------
ccb_object_delete(_OiHandle, _CcbId, DN)
  when DN =:= ?OAP_DN; DN =:= ?ALT_OAP_DN->
    {error, ?ERR_BAD_OP};

ccb_object_delete(_OiHandle, _CcbId, _ObjectName) ->
    ok.

%%--------------------------------------------------------------------
%% Function: ccb_object_modify_2/4
%% Description:
%%--------------------------------------------------------------------
ccb_object_modify_2(OiHandle, CcbId, DN, AttrMods)
  when DN =:= ?OAP_DN; DN =:= ?ALT_OAP_DN ->
    CcbKey = {OiHandle, CcbId},
    ?LOG_INFO("CCB Object Modify ~p: ~p", [CcbKey, DN]),
    case check_oap_attr_mods(DN, AttrMods, CcbId) of
	ok ->
	    ootServer:store_ccb_attrs(CcbKey, [{DN, AttrMods}]);
	Error ->
	    ?LOG_ERROR("CCB Object Modify failed ~p", [Error]),
	    Error
    end;

ccb_object_modify_2(OiHandle, CcbId, DN, AttrMods) ->
    CcbKey = {OiHandle, CcbId},
    case is_address_present(AttrMods) of
	true ->
	    ?LOG_INFO("~p: ~p address updated", [CcbKey, DN]),
	    %% ootServer:store_ccb_attrs({OiHandle, CcbId}, [{DN, AttrMods}]);
	    ootServer:store_ccb_attrs(CcbKey, []);
	_False ->
	    ?LOG_INFO("Ignore CCB Object Modify ~p: ~p", 
		      [CcbKey, DN]),
	    ok
    end.


%%--------------------------------------------------------------------
%% Function: ccb_completed/2
%% Description:
%%--------------------------------------------------------------------
ccb_completed(OiHandle, CcbId) ->
    CcbKey = {OiHandle, CcbId},
    ?LOG_INFO("CCB Completed: ~p", [CcbKey]),
    DNAttrMods = ootServer:get_ccb_attrs(CcbKey),
    case verify_config_change(DNAttrMods) of
	ok ->
	    ?LOG_INFO("CCB ~p Completed verified ok", [CcbKey]),
	    %% ootServer:store_ccb_attrs(CcbKey, DNAttrMods),
	    ok;
	Error ->
	    ?LOG_INFO("CCB ~p Completed verification failed: ~p", 
		      [CcbKey, Error]),
	    ootServer:del_ccb_attrs(CcbKey),
	    Error
    end.

%%--------------------------------------------------------------------
%% Function: ccb_apply/2
%% Description:
%%--------------------------------------------------------------------
ccb_apply(OiHandle, CcbId) ->
    CcbKey = {OiHandle, CcbId},
    ?LOG_INFO("CCB Apply: ~p", [CcbKey]),
    %% ootServer:del_ccb_attrs(CcbKey),
    %% _DNAttrMods = ootServer:get_ccb_attrs({OiHandle, CcbId}),
    %% DNAttrVals = [{DN, attr_mods_to_attr_vals(AttrMods)} ||
    %% 		     {DN, AttrMods} <- DNAttrMods],
    %% mirror_updated_oap_attrs(DNAttrVals),
    ootServer:config_update(CcbKey),
    %% mirror_config_to_ecim(),
    ok.

%%--------------------------------------------------------------------
%% Function: ccb_abort/2
%% Description:
%%--------------------------------------------------------------------
ccb_abort(OiHandle, CcbId) ->
    CcbKey = {OiHandle, CcbId},
    ?LOG_INFO("CCB Abort: ~p", [CcbKey]),
    ootServer:del_ccb_attrs(CcbKey),
    ok.

%%--------------------------------------------------------------------
%% Function: rt_attr_update/3
%% Description:
%%--------------------------------------------------------------------
rt_attr_update(_OiHandle, _ObjectName, _AttributeNames) ->
    ok.


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
%% internal_function1(One, Two)->
%%    nnn.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% #           even_more_internal_function1(One, Two)
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
%%% Check attribute modification
%%% ----------------------------------------------------------
%% This may be overkill but it can't hurt to reject invalid changes.
%% (Most of it is checked by COM but not by IMM).
check_oap_attr_mods(DN, AttrMods, CcbId) ->
    AM = attr_mods_to_modtype_attr_vals(AttrMods),
    case {lists:keyfind(?OAP_IPV4_ADDR, 2, AM),
    	  lists:keyfind(?OAP_ACC_POINT, 2, AM)} of
    	{{MType, _IPv4, Val}, {MType, _AccPoint, Val}} ->
    	    check_oap_attr_mod(DN, AM, CcbId);
    	{AV1, AV2} when AV1 =:= false; AV2 =:= false ->
    	    check_oap_attr_mod(DN, AM, CcbId);
    	_ ->
    	    {error, ?ERR_BAD_OP}
    end.
    %% case check_vrcs_attr_mod(sysEnv:vrcs(), DN) of
    %% 	ok ->
    %% 	    AM = attr_mods_to_modtype_attr_vals(AttrMods),
    %% 	    check_oap_attr_mod(DN, AM, CcbId);
    %% 	Error ->
    %% 	    Error
    %% end.
    

%% %% This is a bit overkill now since Alt OAP is removed in VRCS
%% check_vrcs_attr_mod(true, ?ALT_OAP_DN = DN) ->
%%     %% safs_imm_om:ccb_set_error_string(CcbId, ?ERR_STR_ALT_OAP_VRCS),
%%     ?LOG_WARNING("~p~nFailed to modify attributes, bad operation", [DN]),
%%     {error, ?ERR_BAD_OP};
    
%% check_vrcs_attr_mod(_, _DN) ->
%%     ok.


check_oap_attr_mod(?OAP_DN = DN, [{Type, Attr, Val} | TAVs], CcbId) ->
    log_use_of_obsolete_attr(Attr, Val),
    case check_oap_attr_vals(Attr, Type, Val) of
	ok ->
	    check_oap_attr_mod(DN, TAVs, CcbId);
	Error ->
	    Error
    end;

check_oap_attr_mod(_DN, [], _CcbId) ->
    ok;

check_oap_attr_mod(?ALT_OAP_DN = DN, [{Type, Attr, Val} | TAVs], CcbId)
%% check_oap_attr_mod(DN, [{Type, Attr, Val} | TAVs], CcbId)
  when Attr =:= ?OAP_ACC_POINT;
       Attr =:= ?OAP_IPV4_ADDR;
       Attr =:= ?OAP_ECIM_DN;
       Attr =:= ?OAP_OBJ_ID ->
    ?LOG_INFO("~p~nModify attribute ~p, Val = ~p",
	      [DN, Attr, ootImmLib:get_attr_val(Attr, Val)]),
   case check_vrcs_attr_mod(sysEnv:vrcs(), Attr, Type, Val, CcbId) of
	ok ->
	    check_oap_attr_mod(DN, TAVs, CcbId);
	Error ->
	    Error
    end;
    %% case check_oap_attr_vals(Attr, Type, Val) of
    %% 	ok ->
    %% 	    check_oap_attr_mod(DN, TAVs, CcbId);
    %% 	Error ->
    %% 	    Error
    %% end;

check_oap_attr_mod(DN, 
		   [{Type, Attr, [#safsImmAttrValue{sauint32 = 0}]} | TAVs], 
		   CcbId)
  when DN =:= ?ALT_OAP_DN, Type =:= ?REPLACE, Attr =:= ?OAP_DSCP ->
  %% when Type =:= ?REPLACE, Attr =:= ?OAP_DSCP ->
    ?LOG_INFO("~p~nModify attribute ~p, Val = ~p", [DN, Attr, 0]),
    check_oap_attr_mod(DN, TAVs, CcbId);

%% check_oap_attr_mod(?ALT_OAP_DN, _, _CcbId) ->
check_oap_attr_mod(_DN, _TAVs, _CcbId) ->
    ?LOG_WARNING("~p~nFailed to modify attributes, bad operation", [_DN]),
    {error, ?ERR_BAD_OP}.


check_vrcs_attr_mod(true, Attr, _Type, _Val, _CcbId) 
  when Attr =:= ?OAP_ACC_POINT;
       Attr =:= ?OAP_IPV4_ADDR ->
    %% safs_imm_om:ccb_set_error_string(CcbId, ?ERR_STR_ALT_OAP_VRCS),
    {error, ?ERR_BAD_OP};
    
check_vrcs_attr_mod(_, Attr, Type, Val, _CcbId) ->
    log_use_of_obsolete_attr(Attr, Val),
     check_oap_attr_vals(Attr, Type, Val).


log_use_of_obsolete_attr(Attr, Val) ->
    case lists:member(Attr, ?OAP_OBSOLETE_ATTRS) of
	false ->
	    ok;
	true ->
	    AttrVal = ootImmLib:get_attr_val(Attr, Val),
	    ?LOG_WARNING("Executing ccb including obsolete OamAccessPoint "
			 "attribute ~p~nVal = ~p", [Attr, AttrVal])
	    %% sysInitI:error_msg("~p: Executing ccb including obsolete "
	    %% 		       "OamAccessPoint attribute ~p~nVal = ~p~n",
	    %% 		       [?MODULE, Attr, AttrVal])
    end.


check_oap_attr_vals(Attr, _Type, _AttrVals)
  when Attr =/= ?OAP_NETCONF_PORT,
       Attr =/= ?OAP_SSH_PORT,
       Attr =/= ?OAP_IPV4_ADDR,
       Attr =/= ?OAP_ACC_POINT,
       Attr =/= ?OAP_DSCP ->
    ok;

check_oap_attr_vals(?OAP_NETCONF_PORT, Type,
		    [#safsImmAttrValue{sauint32 = Val}] = AttrVal) ->
    case ootLib:is_host_sim() of
	false when Val =:= ?DEFAULT_NETCONF_PORT_VAL ->
	    ok;
	_ ->
	    check_oap_port(Type, AttrVal)
    end;

check_oap_attr_vals(?OAP_SSH_PORT, Type, AttrVal) ->
    check_oap_port(Type, AttrVal);

check_oap_attr_vals(Attr, _Type, [])
  when Attr =:= ?OAP_IPV4_ADDR; Attr =:= ?OAP_ACC_POINT ->
    ok;

check_oap_attr_vals(Attr, _Type, [#safsImmAttrValue{saname = Val}])
  when (Attr =:= ?OAP_IPV4_ADDR orelse Attr =:= ?OAP_ACC_POINT)
       andalso is_binary(Val) ->
    case {is_ipv4_dn(Val), is_ipv6_dn(Val)} of
	{true, false} ->
	    ok;
	{false, true} when Attr =:= ?OAP_ACC_POINT ->
	    ok;
	_ ->
	    ?LOG_WARNING("Failed to modify attribute ~p, Val = ~p~n"
			 "bad operation", [Attr, Val]),
	    {error, ?ERR_BAD_OP}
    end;

check_oap_attr_vals(?OAP_DSCP, _Type, [#safsImmAttrValue{sauint32 = Val}])
  when is_integer(Val) ->
    ok;

check_oap_attr_vals(_Attr, _Type, _AttrVals) ->
    ?LOG_WARNING("Failed to modify attribute ~p, bad operation", [_Attr]),
    {error, ?ERR_BAD_OP}.


check_oap_port(Type, [#safsImmAttrValue{sauint32 = Val}])
  when is_integer(Val),
       ?MIN_PORT_NO =< Val,
       Val =< ?MAX_PORT_NO,
       Type =:= ?REPLACE ->
    ok;

check_oap_port(_Type, _Val) ->
    ?LOG_WARNING("Failed to modify port attribute, Val = ~p~n"
		 "bad operation", [_Val]),
    {error, ?ERR_BAD_OP}.

%%% ----------------------------------------------------------
%%% Check if address attribute is present in modification
%%% ----------------------------------------------------------
is_address_present(AttrMods) ->
    Attrs = [ModAttr#safsImmAttrValues_2.attrName ||
		#safsImmAttrModification_2{modAttr = ModAttr} <- AttrMods],
    lists:member(?TN_IP_ADDRESS, Attrs).
    

%%% ----------------------------------------------------------
%%% Check consistency between modified and existing attributes
%%% ----------------------------------------------------------
verify_config_change(DNAttrMods) when DNAttrMods =/= [] ->
    OapAttrMods = dn_attr_mods_to_attr_vals(?OAP_DN, DNAttrMods),
    AltOapAttrMods = dn_attr_mods_to_attr_vals(?ALT_OAP_DN, DNAttrMods),
    OapAccPoint = proplists:get_value(?OAP_ACC_POINT, OapAttrMods),
    OapIPv4Addr = proplists:get_value(?OAP_IPV4_ADDR, OapAttrMods),
    AltAccPoint = proplists:get_value(?OAP_ACC_POINT, AltOapAttrMods),
    AltIPv4Addr = proplists:get_value(?OAP_IPV4_ADDR, AltOapAttrMods),
    IsOapV4 = is_ipv4_dn(OapAccPoint),
    IsAltOapV4 = is_ipv4_dn(AltAccPoint),
    if
	OapAccPoint =:= undefined, OapIPv4Addr =:= undefined,
	AltAccPoint =:= undefined, AltIPv4Addr =:= undefined ->
	    verify_port_config_change(OapAttrMods);
 	IsOapV4, OapAccPoint =/= undefined, OapIPv4Addr =/= undefined,
	OapAccPoint =/= OapIPv4Addr ->
	    {error, ?ERR_BAD_OP};
 	IsAltOapV4, AltAccPoint =/= undefined, AltIPv4Addr =/= undefined,
	AltAccPoint =/= AltIPv4Addr ->
	    {error, ?ERR_BAD_OP};
 	OapAccPoint =/= undefined, OapIPv4Addr =/= undefined,
	OapIPv4Addr =/= [], OapAccPoint =/= OapIPv4Addr  ->
	    {error, ?ERR_BAD_OP};
 	AltAccPoint =/= undefined, AltIPv4Addr =/= undefined,
	AltIPv4Addr =/= [], AltAccPoint =/= AltIPv4Addr ->
	    {error, ?ERR_BAD_OP};
 	true ->
	    Oap = get_oap_accpoint(OapAccPoint, OapIPv4Addr, ?OAP_DN),
	    AltOap = get_oap_accpoint(AltAccPoint, AltIPv4Addr, ?ALT_OAP_DN),
	    verify_oap_config_change(Oap, AltOap, OapAttrMods)
    end;

verify_config_change(_DNAttrMods) ->
    ok.


get_oap_accpoint(undefined, undefined, DN) ->
    %% get_oap_attr_val(DN, ?OAP_ACC_POINT);
    case get_oap_accpoint_dn(DN) of
	undefined ->
	    [];
	Val ->
	    {ok, Val2} = gmfI:mim_to_imm(Val),
	    Val2
    end;

get_oap_accpoint(OapAccPoint, _OapIPv4Addr, _DN)
  when OapAccPoint =/= undefined ->
    OapAccPoint;

get_oap_accpoint(_OapAccPoint, OapIPv4Addr, _DN) ->
    OapIPv4Addr.


get_oap_accpoint_dn(?OAP_DN) ->
    ootServer:get_access_point_dn();

get_oap_accpoint_dn(?ALT_OAP_DN) ->
    ootServer:get_access_point_alt_dn().


verify_oap_config_change([], [], OapAttrMods) ->
    verify_port_config_change(OapAttrMods);

verify_oap_config_change(_OapAccPoint, [], OapAttrMods) ->
    verify_port_config_change(OapAttrMods);

verify_oap_config_change([], _AltOapAccPoint, _OapAttrMods) ->
    {error, ?ERR_BAD_OP};

verify_oap_config_change(Same, Same, _OapAttrMods) ->
    {error, ?ERR_BAD_OP};

verify_oap_config_change(_OapAccPoint, _AltOapAccPoint, OapAttrMods) ->
    verify_port_config_change(OapAttrMods).


verify_port_config_change(OapAttrMods) ->
    Config = attr_vals_to_config([{Name, Val} ||
				     {_Type, Name, Val} <- OapAttrMods]),
    case ootLib:verify_port_config_change(Config) of
	ok ->
	    ok;
	{error, _Reason} ->
	    {error, ?ERR_BAD_OP}
    end.


%% get_oap_attr_val(DN, Attr) ->
%%     {ok, AttrVals} = ootImmLib:get_object(DN),
%%     case lists:keyfind(Attr, 1, AttrVals) of
%% 	{_Attr, _Type, [Val]} ->
%% 	    Val;
%% 	_ ->
%% 	    []
%%     end.


%%% ----------------------------------------------------------
%%% Mirror between deprecated and new attributes
%%% ----------------------------------------------------------
%% mirror_updated_oap_attrs(DNAttrVals) ->
%%     lists:foreach(fun({DN, AttrVals}) ->
%% 			  IPv4 = proplists:get_value(?OAP_IPV4_ADDR, AttrVals),
%% 			  AccP = proplists:get_value(?OAP_ACC_POINT, AttrVals),
%% 			  IsIPv6 = is_ipv6_dn(AccP),
%% 			  if
%% 			      IPv4 =:= AccP ->
%% 				  ok;
%% 			      IsIPv6, IPv4 =:= [] ->
%% 				  ok;
%% 			      true ->
%% 				  mirror_ipv4_addr(IPv4, AccP, DN)
%% 			  end
%% 		  end, DNAttrVals).


%% mirror_ipv4_addr(Val, _Undef, DN) when Val =:= [] ->
%%     AccPoint = get_oap_attr_val(DN, ?OAP_ACC_POINT),
%%     case is_ipv6_dn(AccPoint) of
%% 	true ->
%% 	    ok;
%% 	_False ->
%% 	    set_name_attr_if_updated(DN, ?OAP_ACC_POINT, Val)
%%     end;

%% mirror_ipv4_addr(Val, _Undef, DN) when Val =/= undefined ->
%%     set_name_attr_if_updated(DN, ?OAP_ACC_POINT, Val);

%% mirror_ipv4_addr(_Undef, Val, DN) when Val =:= [] ->
%%     set_name_attr_if_updated(DN, ?OAP_IPV4_ADDR, Val);

%% mirror_ipv4_addr(_Undef, Val, DN) when Val =/= undefined ->
%%     case is_ipv6_dn(Val) of
%% 	true ->
%% 	    set_name_attr_if_updated(DN, ?OAP_IPV4_ADDR, []);
%% 	_False ->
%% 	    set_name_attr_if_updated(DN, ?OAP_IPV4_ADDR, Val)
%%     end.


%% set_name_attr_if_updated(DN, Attr, Val) ->
%%     set_attr_if_updated(DN, {Attr, sa_imm_attr_sanamet, Val}).


%% set_attr_if_updated(DN, {Attr, _Tp, Val} = AttrVal) ->
%%     {ok, AttrVals} = ootImmLib:get_object(DN),
%%     case lists:keyfind(Attr, 1, AttrVals) of
%% 	{_Attr, _Type, [Val]} ->
%% 	    ok;
%% 	{_Attr, _Type, Val} ->
%% 	    ok;
%% 	_Other ->
%% 	    ootServer:set_oap_attrs(DN, [AttrVal])
%%     end.


%% mirror_config_to_ecim() ->
%%     Config = get_config(),
%%     ootComSysM:set_updated_config(Config).


%%% ----------------------------------------------------------
%%% Execute CCB as admin owner
%%% ----------------------------------------------------------
exec_as_admin_owner(Fun) ->
    exec_as_admin_owner(Fun, [?SYS_M_PARENT_DN, ?OAP_PARENT_DN]).


exec_as_admin_owner(Fun, Objects) ->
    {_OmHandle, AOHandle, CcbHandle} = H = ootImmLib:initialize_om_ao_ccb(),
    try
	ok = ootImmLib:admin_owner_set(AOHandle, Objects),
	Res = Fun(H),
	ok = safs_imm_om:ccb_apply(CcbHandle),
	ootImmLib:finalize_om_ao_ccb(H),
	Res
    catch _:Error ->
	    ST = erlang:get_stacktrace(),
	    sysInitI:warning_msg("~p: Failed to execute ccb functions on "
				 "OamAccessPoint, ~p~n",
				 [?MODULE, ST]),
	    catch ootImmLib:finalize_om_ao_ccb(H),
	    {error, Error}
    end.


%%% ----------------------------------------------------------
%%% Get handle and execute IMM function
%%% ----------------------------------------------------------
exec_with_handle(F) ->
    Handle = ootImmLib:initialize_om_ah(),
    Res = F(Handle),
    finalize_om(Handle),
    Res.

%%% ----------------------------------------------------------
%%% Check reservedBy and remove duplicates if any
%%% ----------------------------------------------------------
remove_duplicated_reserved_by(Handle, OapDN, IpAddrDN) ->
    {OmH, AOH, CcbH} = Handle,
    case ootImmLib:admin_owner_set(AOH, [IpAddrDN]) of
	ok ->
	    remove_duplicated_reserved_by(OmH, CcbH, OapDN, IpAddrDN);
	_Error ->
	    timer:sleep(100),
	    remove_duplicated_reserved_by(Handle, OapDN, IpAddrDN)
    end.


remove_duplicated_reserved_by(OmH, CcbH, OapDN, IpAddrDN) ->
    Attrs = [?ADDR_IPV4_RESERVED_BY],
    AH = ootImmLib:initialize_ah(OmH),
    {ok, [{Attr, Type, Vals}]} =
	safs_imm_om:accessor_get_2(AH, IpAddrDN, Attrs),
    ok = ootImmLib:finalize_ah(AH),
    NewVals = lists:delete(OapDN, Vals),
    case lists:member(OapDN, NewVals) of
	true ->
	    %% This is already handled by gmfImmUgInserter so this piece
	    %% of code could probably be removed.
	    sysInitI:info_msg("~p: Found duplicated reservedBy for ~s~n"
			      "~p~nRemove duplicates~n",
			      [?MODULE, ootLib:to_list(IpAddrDN), Vals]),
	    ok = ootImmLib:set_attr(CcbH, IpAddrDN, {Attr, Type, NewVals}),
	    ok = safs_imm_om:ccb_apply(CcbH);
	_False ->
	    ok
    end.


%%% ----------------------------------------------------------
%%% Commonly used functions
%%% ----------------------------------------------------------
get_oap_config(Handle, OapDN, Attrs) ->
    case ootImmLib:get_attrs(Handle, OapDN, Attrs) of
	{ok, AttrVals} ->
	    attr_vals_to_config(AttrVals);
	_Error ->
	    []
    end.


get_config(Handle, OapDN, Attrs) ->
    case ootImmLib:get_attrs(Handle, OapDN, Attrs) of
	{ok, AttrVals} ->
	    AccPointDN = get_acc_point_dn_val(AttrVals),
	    AccPointAddr = get_oap_ip_addr(Handle, AccPointDN),
	    IPV4Addr = get_deprecated_ipv4_addr(AccPointDN, AccPointAddr),
	    OapConfig = attr_vals_to_config(AttrVals),
	    AccPointCnf = {?CNF_ACC_POINT_ADDR, AccPointAddr},
	    IPv4AddrCnf = {?CNF_IPV4_ADDR, IPV4Addr},
	    [AccPointCnf, IPv4AddrCnf | OapConfig];
	_Error ->
	    []
    end.


get_acc_point_dn_val(AttrVals) ->
    case proplists:get_value(?OAP_ACC_POINT, AttrVals) of
	[DN] when is_binary(DN), DN =/= <<>> ->
	    DN;
	_Undef ->
	    []
    end.


get_deprecated_ipv4_addr(AccPointDN, AccPointAddr) ->
    case is_ipv4_dn(AccPointDN) of
	true ->
	    AccPointAddr;
	_False ->
	    []
    end.


get_oap_ip_addr(Handle, AccPointDN)
  when AccPointDN =/= [], AccPointDN =/= <<>>,  AccPointDN =/= undefined ->
    %% Attrs = [?TN_IP_ADDRESS, ?TN_5G_USED_IP_ADDRESS],
    Attrs = [?TN_IP_ADDRESS],
    get_oap_ip_addr(Attrs, Handle, AccPointDN);

get_oap_ip_addr(_Handle, _AccPointDN) ->
    [].


get_oap_ip_addr([Attr | Attrs], Handle, AccPointDN) ->
    case ootImmLib:get_attrs(Handle, AccPointDN, [Attr]) of
	{ok, [{_Key, [Val]}]}
	  when Val =/= undefined, Val =/= <<>>, Val =/= [] ->
	    ootLib:to_list(Val);
	_Error ->
	    get_oap_ip_addr(Attrs, Handle, AccPointDN)
    end;

get_oap_ip_addr([], _Handle, _AccPointDN) ->
    [].


attr_vals_to_config(AttrVals) ->
    ootLib:imm_attr_vals_to_config(AttrVals).


dn_attr_mods_to_attr_vals(DN, DNAttrMods) ->
    OapAttrMods = lists:flatten([attr_mods_to_attr_vals(AttrMod) ||
				    {OAPDN, AttrMod} <- DNAttrMods,
				    OAPDN =:= DN]),
    lists:ukeysort(1, OapAttrMods).


attr_mods_to_attr_vals(AttrMods) ->
    [{Attr, attr_val_rec_to_val(Attr, ValRec)} ||
	{_Type, Attr, ValRec} <- attr_mods_to_modtype_attr_vals(AttrMods)].


attr_mods_to_modtype_attr_vals(AttrMods) ->
    [{ModType,
      ModAttr#safsImmAttrValues_2.attrName,
      ModAttr#safsImmAttrValues_2.attrValues} ||
	#safsImmAttrModification_2{modType = ModType,
				   modAttr = ModAttr} <- AttrMods].


attr_val_rec_to_val(Attr, [Rec]) when Attr =:= ?OAP_NETCONF_PORT;
				    Attr =:= ?OAP_SSH_PORT;
				    Attr =:= ?OAP_DSCP;
				    Attr =:= ?OAP_OBJ_ID ->
    Rec#safsImmAttrValue.sauint32;
attr_val_rec_to_val(Attr, [Rec]) when Attr =:= ?OAP_IPV4_ADDR;
				    Attr =:= ?OAP_ACC_POINT ->
    Rec#safsImmAttrValue.saname;
attr_val_rec_to_val(Attr, [Rec]) when Attr =:= ?OAP_ECIM_DN;
				    Attr =:= ?OAP_OBJ_ID ->
    Rec#safsImmAttrValue.sastring;
attr_val_rec_to_val(_Attr, []) ->
    [].


is_ipv4_dn(DN) when is_binary(DN) ->
    case re:run(DN, ?ADDR_IPV4_ID) of
	{match, _} ->
	    true;
	_NoMatch ->
	    false
    end;

is_ipv4_dn(_DN) ->
    false.


is_ipv6_dn(DN) when is_binary(DN) ->
    case re:run(DN, ?ADDR_IPV6_ID) of
	{match, _} ->
	    true;
	_NoMatch ->
	    false
    end.


%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

%% from_to_vsn() ->
%%     {{safsImmCtSchemaVersion,2,2,0},{safsImmCtSchemaVersion,2,3,0}}.


%% inst_groups() ->
%%     [{safsImmCtInstanceGroup,<<"OamAccessPoint">>,
%%       [{safsImmCtInstance,<<"sysMId=1,systemFunctionsId=1">>,
%% 	[{safsImmAttrValues_2,<<"oamAccessPointId">>,sa_imm_attr_sastringt,
%% 	  1,
%% 	  [{safsImmAttrValue,undefined,undefined,undefined,undefined,
%% 	    undefined,undefined,undefined,undefined,<<"1">>,undefined,
%% 	    undefined}]},
%% 	 {safsImmAttrValues_2,<<"sshPort">>,sa_imm_attr_sauint32t,1,
%% 	  [{safsImmAttrValue,undefined,30404,undefined,undefined,
%% 	    undefined,undefined,undefined,undefined,undefined,
%% 	    undefined,undefined}]},
%% 	 {safsImmAttrValues_2,<<"netconfPort">>,sa_imm_attr_sauint32t,1,
%% 	  [{safsImmAttrValue,undefined,30403,undefined,undefined,
%% 	    undefined,undefined,undefined,undefined,undefined,
%% 	    undefined,undefined}]},
%% 	 {safsImmAttrValues_2,<<"dscp">>,sa_imm_attr_sauint32t,1,
%% 	  [{safsImmAttrValue,undefined,0,undefined,undefined,undefined,
%% 	    undefined,undefined,undefined,undefined,undefined,
%% 	    undefined}]}]},
%%        {safsImmCtInstance,<<"sysMId=1,systemFunctionsId=1">>,
%% 	[{safsImmAttrValues_2,<<"oamAccessPointId">>,sa_imm_attr_sastringt,
%% 	  1,
%% 	  [{safsImmAttrValue,undefined,undefined,undefined,undefined,
%% 	    undefined,undefined,undefined,undefined,<<"Alternative">>,
%% 	    undefined,undefined}]},
%% 	 {safsImmAttrValues_2,<<"dscp">>,sa_imm_attr_sauint32t,1,
%% 	  [{safsImmAttrValue,undefined,0,undefined,undefined,undefined,
%% 	    undefined,undefined,undefined,undefined,undefined,
%% 	    undefined}]}]}]}].
