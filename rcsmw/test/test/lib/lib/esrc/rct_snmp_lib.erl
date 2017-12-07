%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	rct_snmp_lib.erl %
%%% Author: 
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------

-module(rct_snmp_lib).
-vsn('/main/R10A/3').
-author('ekurnik').

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
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R10A/1   2017-06-13   ekurnik      Created
%%% R10A/2   2017-06-14   ekurnik      Library design done, Snmp part tested
%%% R10A/3   2017-06-17   ekurnik      SnmpTarget tested
%%%--------------------------------------------------------------------


%% main interfaces functions
-export([get_snmp_dn/0,
         set_snmp_administrative_state/2,
         get_snmp_administrative_state/1,
         get_snmp_operational_state/1,
         get_snmp_agent_address/2,
         set_snmp_agent_address/3,
         get_snmp_node_credential/1,
         get_snmp_trust_category/1,
         set_snmp_node_credential/2,
         set_snmp_trust_category/2,
         get_snmp_targets/1,
         get_snmp_targets/2,
         create_snmp_target/4,
         delete_snmp_target/3,
         create_snmp_target_configuration/5,
         get_snmp_target_dn/2,
         set_snmp_target_administrative_state/4,
         get_snmp_target_administrative_state/3,
         get_snmp_target_operational_state/3
         ]).

%% for test:
-export([suite/0, all/0, test/1, test_target/1]).

-define(SNMP_DN, "ManagedElement=1,SystemFunctions=1,SysM=1,Snmp=1").
-define(SNMP_TARGET_DN(TargetType, Id), ?SNMP_DN ++ "," ++ type_to_mo(TargetType) ++ "=" ++ Id).

suite() ->
    [{ct_hooks, [{rct_htmllink,[]},
     {rct_netconf,{nc1, html}},
     {cth_conn_log,[]},
     {rct_core,[]},
     {rct_logging,
         {all,
             [{erlang,
                     {["ERROR REPORT", "CRASH REPORT"], 
                      []}}]}}
    ]}].

all() ->
    [test, test_target].

%% Used for testing the library
test(_Config) ->
    Handler = {netconf, nc1},
    
    %% Snmp MO
    ct:pal("Snmp DN: ~p~n", [get_snmp_dn()]),
    
    ok = set_snmp_administrative_state(Handler, locked),
    timer:sleep(5000),
    {ok, locked} = get_snmp_administrative_state(Handler),
    {ok, disabled} = get_snmp_operational_state(Handler),
    
    ok = set_snmp_administrative_state(Handler, unlocked),
    timer:sleep(5000),
    {ok, unlocked} = get_snmp_administrative_state(Handler),
    {ok, enabled} = get_snmp_operational_state(Handler),
    
    {ok, [{agentAddress, [{Host, Port}]}]} = get_snmp_agent_address(Handler, udp),
    ct:pal("agentAddress: ~p:~p~n", [Host, Port]),
    
    ok = set_snmp_agent_address(Handler, dtls, [{Host, 10161}]),

    ct:pal("agentAddress all: ~p~n", [get_snmp_agent_address(Handler, all)]),
    
    ok = set_snmp_agent_address(Handler, dtls, []),
    
    {ok, [{agentAddressDtls, []}]} = get_snmp_agent_address(Handler, dtls),
    
    %% Setup certs for NC/Tcat test
    ok = rct_certm_lib:create_node_credential(Handler, "snmp_cert"),
    {ok, TcId} = rct_certm_lib:install_trusted_certificate_from_uri(Handler, 
                                         "sftp://labuser@10.68.101.131/home/labuser/FTP_TLS_SETUP/user-ca.crt", 
                                         "labuser", "DB:59:94:FB:BE:E6:1B:83:D4:77:88:BF:F8:27:9B:B9:BC:A0:5D:23"),
    ok = rct_certm_lib:create_trust_category(Handler, "tcat_1", [TcId]),
    
    NCDn = rct_certm_lib:get_node_credential_dn("snmp_cert"),
    TcatDn =  rct_certm_lib:get_trust_category_dn("tcat_1"),
    
    ok = set_snmp_node_credential(Handler, NCDn),
    ok = set_snmp_trust_category(Handler, TcatDn),
    
    {ok, NCDn} = get_snmp_node_credential(Handler),
    {ok, TcatDn} = get_snmp_trust_category(Handler),
    
    ok = set_snmp_node_credential(Handler, undefined),
    ok = set_snmp_trust_category(Handler, undefined),
    
    {ok, undefined} = get_snmp_node_credential(Handler),
    {ok, undefined} = get_snmp_trust_category(Handler),
    
    ok = rct_certm_lib:delete_trust_category(Handler, "tcat_1"),
    ok = rct_certm_lib:delete_trusted_certificate(Handler, TcId),
    ok = rct_certm_lib:delete_node_credential(Handler, "snmp_cert"),

    ok.

test_target(_) ->
    Handler = {netconf, nc1},
    
    %% SnmpTarget MO
    ct:pal("V1 target DN: ~p~n", [get_snmp_target_dn(v1, "V1")]),
    ct:pal("V2C target DN: ~p~n", [get_snmp_target_dn(v2c, "V2C")]),
    ct:pal("V3 target DN: ~p~n", [get_snmp_target_dn(v3, "V3")]),
    ct:pal("V3Dtls target DN: ~p~n", [get_snmp_target_dn(v3dtls, "V3Dtls")]),
    
    ct:pal("All targets:~p~n", [get_snmp_targets(Handler)]),
    ct:pal("V1 targets:~p~n", [get_snmp_targets(Handler, v1)]),
    ct:pal("V2C targets:~p~n", [get_snmp_targets(Handler, v2c)]),
    ct:pal("V3 targets:~p~n", [get_snmp_targets(Handler, v3)]),
    ct:pal("V3Dtls targets:~p~n", [get_snmp_targets(Handler, v3dtls)]),
    
    V1Config = create_snmp_target_configuration(v1, "147.214.13.193", 162, "public", undefined),
    V2CConfig = create_snmp_target_configuration(v2c, "147.214.13.193", 162, "public", undefined),
    V3Config = create_snmp_target_configuration(v3, "147.214.13.193", 162, "public", {"publicpublic", "publicpublic"}),
    V3DtlsConfig = create_snmp_target_configuration(v3dtls, "147.214.13.193", 10162, "public", undefined),
    
    ct:pal("V1Config: ~p~n", [V1Config]),
    ct:pal("V2CConfig: ~p~n", [V2CConfig]),
    ct:pal("V3Config: ~p~n", [V3Config]),
    ct:pal("V3DtlsConfig: ~p~n", [V3DtlsConfig]),
    
    ok = create_snmp_target(Handler, v1, "V1", V1Config),
    ok = create_snmp_target(Handler, v2c, "V2C", V2CConfig),
    ok = create_snmp_target(Handler, v3, "V3", V3Config),
    ok = create_snmp_target(Handler, v3dtls, "V3Dtls", V3DtlsConfig),
    
    ct:pal("All targets:~p~n", [get_snmp_targets(Handler)]),
    ct:pal("V1 targets:~p~n", [get_snmp_targets(Handler, v1)]),
    ct:pal("V2C targets:~p~n", [get_snmp_targets(Handler, v2c)]),
    ct:pal("V3 targets:~p~n", [get_snmp_targets(Handler, v3)]),
    ct:pal("V3Dtls targets:~p~n", [get_snmp_targets(Handler, v3dtls)]),
    
    ok = delete_snmp_target(Handler, v1, "V1"),
    ok = delete_snmp_target(Handler, v2c, "V2C"),
    ok = delete_snmp_target(Handler, v3, "V3"),
    ok = delete_snmp_target(Handler, v3dtls, "V3Dtls"),
    
    set_snmp_target_administrative_state(Handler, v2c, "1", locked),
    timer:sleep(5000),
    {ok, locked} = get_snmp_target_administrative_state(Handler, v2c, "1"),
    {ok, disabled} = get_snmp_target_operational_state(Handler, v2c, "1"),
    
    set_snmp_target_administrative_state(Handler, v2c, "1", unlocked),
    timer:sleep(5000),
    {ok, unlocked} = get_snmp_target_administrative_state(Handler, v2c, "1"),
    {ok, enabled} = get_snmp_target_operational_state(Handler, v2c, "1"),
    
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Snmp
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Gets Snmp MO DN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_snmp_dn() -> rct_mo_handler_lib:dn().
get_snmp_dn() ->
    ?SNMP_DN.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Sets adminState on Snmp MO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_snmp_administrative_state(Handler :: rct_mo_handler_lib:handler(), 
                                    AdminState :: locked | unlocked) -> ok | {error, term()}.
set_snmp_administrative_state(Handler, AdminState) ->
    ct:log("Setting adminState [~p] on Snmp MO~n", [AdminState]),
    rct_mo_handler_lib:set_mo(Handler, ?SNMP_DN, [{administrativeState, encode_admin_state(AdminState)}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Gets adminState on Snmp MO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_snmp_administrative_state(Handler :: rct_mo_handler_lib:handler()) -> 
                                        {ok, locked | unlocked} | {error, term()}.
get_snmp_administrative_state(Handler) ->
    case rct_mo_handler_lib:get_mo(Handler, ?SNMP_DN, [administrativeState]) of
        {ok, [{administrativeState, AdminState}]} ->
            ct:log("Snmp MO administrativeState: [~p]", [decode_admin_state(AdminState)]),
            {ok, decode_admin_state(AdminState)};
        Other ->
            Other
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Gets operationalState on Snmp MO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_snmp_operational_state(Handler :: rct_mo_handler_lib:handler()) ->
                                    {ok, enabled | disabled} | {error, term()}.
get_snmp_operational_state(Handler) ->
    case rct_mo_handler_lib:get_mo(Handler, ?SNMP_DN, [operationalState]) of
        {ok, [{operationalState, OperState}]} ->
            ct:log("Snmp MO operationalState: [~p]", [decode_operational_state(OperState)]),
            {ok, decode_operational_state(OperState)};
        Other ->
            Other
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Gets list of agentAddresses (udp, dtls or both)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_snmp_agent_address(Handler :: rct_mo_handler_lib:handler(),
                             Protocol :: udp | dtls | all) -> 
                             {ok, [{atom(), [{string(), integer()}]}]} | {error, term()}.
get_snmp_agent_address(Handler, Protocol) ->
    case rct_mo_handler_lib:get_mo(Handler, ?SNMP_DN, proto_to_agent_address(Protocol)) of
        {ok,Result} ->
            AgentAddress = decode_agent_address(Result),
            ct:log("Snmp MO agentAddress for [~p]: [~p]", [Protocol, AgentAddress]),
            {ok, AgentAddress};
        Other ->
            Other
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Sets agent address(es) (udp or dtls)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_snmp_agent_address(Handler :: rct_mo_handler_lib:handler(),
                             Protocol :: udp | dtls,
                             AddressList :: [{string(), integer() | string()}]) ->
                             ok | {error, term()}.
set_snmp_agent_address(Handler, Protocol, AddressList) ->
    [AgentAddressAttr] = proto_to_agent_address(Protocol),
    Attrs = [{AgentAddressAttr, encode_agent_address(AddressList)}],
    case rct_mo_handler_lib:set_mo(Handler, ?SNMP_DN, Attrs) of
        ok ->
            ct:log("Successfully set [~p: ~p] on Snmp MO", [AgentAddressAttr, AddressList]),
            ok;
        Other ->
            ct:log("Failed to set [~p: ~p] on Snmp MO~nReason: ~p~n", 
                    [AgentAddressAttr, AddressList, Other]),
            Other
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Gets nodeCredential from Snmp MO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_snmp_node_credential(Handler :: rct_mo_handler_lib:handler()) ->
                                {ok, rct_mo_handler_lib:dn() | undefined} | {error, term()}.
get_snmp_node_credential(Handler) ->
    case rct_mo_handler_lib:get_mo(Handler, ?SNMP_DN, [nodeCredential]) of
        {ok, [{nodeCredential, NC}]} ->
            ct:log("Snmp MO nodeCredential: ~p~n", [NC]),
            {ok, NC};
        Other ->
            Other
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Gets trustCategory from Snmp MO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_snmp_trust_category(Handler :: rct_mo_handler_lib:handler()) ->
                                {ok, rct_mo_handler_lib:dn() | undefined} | {error, term()}.
get_snmp_trust_category(Handler) ->
    case rct_mo_handler_lib:get_mo(Handler, ?SNMP_DN, [trustCategory]) of
        {ok, [{trustCategory, Tcat}]} ->
            ct:log("Snmp MO trustCategory: ~p~n", [Tcat]),
            {ok, Tcat};
        Other ->
            Other
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Sets nodeCredential on Snmp MO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_snmp_node_credential(Handler :: rct_mo_handler_lib:handler(),
                               NodeCredential :: rct_mo_handler_lib:dn() | undefined) -> 
                                ok | {error, term()}.
set_snmp_node_credential(Handler, NodeCredential) ->
    ct:log("Setting nodeCredential [~p] on Snmp MO~n", [NodeCredential]),
    rct_mo_handler_lib:set_mo(Handler, ?SNMP_DN, [{nodeCredential, NodeCredential}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Sets trustCategory on Snmp MO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_snmp_trust_category(Handler :: rct_mo_handler_lib:handler(),
                               TrustCategory :: rct_mo_handler_lib:dn() | undefined) -> 
                                ok | {error, term()}.
set_snmp_trust_category(Handler, TrustCategory) ->
    ct:log("Setting trustCategory [~p] on Snmp MO~n", [TrustCategory]),
    rct_mo_handler_lib:set_mo(Handler, ?SNMP_DN, [{trustCategory, TrustCategory}]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SnmpTarget
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Gets all SnmpTarget MO DNs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_snmp_targets(Handler :: rct_mo_handler_lib:handler()) -> 
                        {ok, [rct_mo_handler_lib:dn()]} | {error, term()}.
get_snmp_targets(Handler) ->
    get_snmp_targets(Handler, all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Gets all SnmpTarget MO DNs of certain protocol type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_snmp_targets(Handler :: rct_mo_handler_lib:handler(), SnmpVer :: v1 | v2c | v3 | v3dtls) -> 
                        {ok, [rct_mo_handler_lib:dn()]} | {error, term()}.
get_snmp_targets(Handler, v3) ->
    rct_mo_handler_lib:get_children_mo(Handler, ?SNMP_DN, type_to_mo(v3) ++ "="); %% special case, exclude V3Dtls
get_snmp_targets(Handler, SnmpVer) ->
    rct_mo_handler_lib:get_children_mo(Handler, ?SNMP_DN, type_to_mo(SnmpVer)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates SnmpTarget of protocol type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_snmp_target(Handler :: rct_mo_handler_lib:handler(), SnmpVer :: v1 | v2c | v3 | v3dtls,
                          SnmpTargetId :: string(), Attrs :: rct_mo_handler_lib:attrs()) ->
                            ok | {error, term()}.
create_snmp_target(Handler, SnmpVer, SnmpTargetId, Attrs) ->
     case rct_mo_handler_lib:create_mo(Handler, ?SNMP_TARGET_DN(SnmpVer, SnmpTargetId), Attrs) of
         ok ->
             ct:log("Successfully created ~p~n", [?SNMP_TARGET_DN(SnmpVer, SnmpTargetId)]),
             ok;
         Other ->
             Other
     end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Delete SnmpTarget MO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec delete_snmp_target(Handler :: rct_mo_handler_lib:handler(), SnmpVer :: v1 | v2c | v3 | v3dtls,
                          SnmpTargetId :: string()) -> ok | {error, term()}.
delete_snmp_target(Handler, SnmpVer, SnmpTargetId) ->
    case rct_mo_handler_lib:delete_mo(Handler, ?SNMP_TARGET_DN(SnmpVer, SnmpTargetId)) of
         ok ->
             ct:log("Successfully deleted ~p~n", [?SNMP_TARGET_DN(SnmpVer, SnmpTargetId)]),
             ok;
         Other ->
             Other
     end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Create configuration used for creating/setting SnmpTarget MO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_snmp_target_configuration(SnmpVer :: v1 | v2c | v3 | v3dtls,
                                       Address :: string(),
                                       Port :: string() | integer(),
                                       Username :: string(),
                                       Password :: {string(), string()}) ->
                                         rct_mo_handler_lib:attrs().
create_snmp_target_configuration(SnmpVer, Address, Port, Username, Password) when is_integer(Port) ->
    create_snmp_target_configuration(SnmpVer, Address, integer_to_list(Port), Username, Password);
%% v1 or v2c
create_snmp_target_configuration(SnmpVer, Address, Port, Username, _Password) 
                                    when SnmpVer =:= v1 orelse SnmpVer =:= v2c ->
    [{address, Address}, {port, Port}, {community, Username}];
create_snmp_target_configuration(v3, Address, Port, Username, {AuthKey, PrivKey}) ->
    [{address, Address}, {port, Port}, {user, Username},
     {authKey, [{structName, 'EcimPassword'}, {cleartext, ""}, {password, AuthKey}]},
     {privKey, [{structName, 'EcimPassword'}, {cleartext, ""}, {password, PrivKey}]}];
create_snmp_target_configuration(v3dtls, Address, Port, Username, _Password) ->
    [{address, Address}, {port, Port}, {user, Username}].
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Get DN of SnmpTarget MO based on protocol and id
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_snmp_target_dn(SnmpVer :: v1 | v2c | v3 | v3dtls,
                         SnmpTargetId :: string()) -> rct_mo_handler_lib:dn().
get_snmp_target_dn(SnmpVer, SnmpTargetId) ->
    ?SNMP_TARGET_DN(SnmpVer, SnmpTargetId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Set adminState for SnmpTarget
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_snmp_target_administrative_state(Handler :: rct_mo_handler_lib:handler(),
                                    SnmpVer :: v1 | v2c | v3 | v3dtls,
                                    SnmpTargetId :: string(),
                                    AdminState :: locked | unlocked) -> ok | {error, term()}.
set_snmp_target_administrative_state(Handler, SnmpVer, SnmpTargetId, AdminState) ->
    ct:log("Setting adminState [~p] on SnmpTarget MO: [~p]~n", 
                                    [AdminState, ?SNMP_TARGET_DN(SnmpVer, SnmpTargetId)]),
    rct_mo_handler_lib:set_mo(Handler, ?SNMP_TARGET_DN(SnmpVer, SnmpTargetId), 
                                [{administrativeState, encode_admin_state(AdminState)}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Get adminState of SnmpTarget
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_snmp_target_administrative_state(Handler :: rct_mo_handler_lib:handler(),
                                    SnmpVer :: v1 | v2c | v3 | v3dtls,
                                    SnmpTargetId :: string()) -> 
                                        {ok, locked | unlocked} | {error, term()}.
get_snmp_target_administrative_state(Handler, SnmpVer, SnmpTargetId) ->
    case rct_mo_handler_lib:get_mo(Handler, ?SNMP_TARGET_DN(SnmpVer, SnmpTargetId), [administrativeState]) of
        {ok, [{administrativeState, AdminState}]} ->
            ct:log("SnmpTarget MO [~p] administrativeState: [~p]", 
                   [?SNMP_TARGET_DN(SnmpVer, SnmpTargetId), decode_admin_state(AdminState)]),
            {ok, decode_admin_state(AdminState)};
        Other ->
            Other
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Get operationalState of SnmpTarget
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_snmp_target_operational_state(Handler :: rct_mo_handler_lib:handler(),
                                 SnmpVer :: v1 | v2c | v3 | v3dtls,
                                 SnmpTargetId :: string()) ->
                                    {ok, enabled | disabled} | {error, term()}.
get_snmp_target_operational_state(Handler, SnmpVer, SnmpTargetId) ->
    case rct_mo_handler_lib:get_mo(Handler, ?SNMP_TARGET_DN(SnmpVer, SnmpTargetId), [operationalState]) of
        {ok, [{operationalState, OperState}]} ->
            ct:log("SnmpTarget MO [~p] operationalState: [~p]", 
                   [?SNMP_TARGET_DN(SnmpVer, SnmpTargetId), decode_operational_state(OperState)]),
            {ok, decode_operational_state(OperState)};
        Other ->
            Other
    end.

-spec type_to_mo(TargetType :: all | v1 | v2c | v3 | v3dtls) ->
          string().
type_to_mo(all) ->
    "SnmpTarget";
type_to_mo(v1) ->
    "SnmpTargetV1";
type_to_mo(v2c) ->
    "SnmpTargetV2C";
type_to_mo(v3) ->
    "SnmpTargetV3";
type_to_mo(v3dtls) ->
    "SnmpTargetV3Dtls".

encode_admin_state(locked) ->
    "LOCKED";
encode_admin_state(unlocked) ->
    "UNLOCKED".

decode_admin_state("LOCKED") ->
    locked;
decode_admin_state("UNLOCKED") ->
    unlocked.

decode_operational_state("ENABLED") ->
    enabled;
decode_operational_state("DISABLED") ->
    disabled.

proto_to_agent_address(udp) ->
    [agentAddress];
proto_to_agent_address(dtls) ->
    [agentAddressDtls];
proto_to_agent_address(all) ->
    [agentAddress, agentAddressDtls].

decode_agent_address(AgentAddress) ->
    decode_agent_address(AgentAddress, []).

decode_agent_address([], Result) ->
    Result;
decode_agent_address([{AddressAttr, undefined} | Rest], Result) ->
    decode_agent_address(Rest, Result ++ [{AddressAttr, []}]);
decode_agent_address([{AddressAttr, [Param | _] = AddressStruct} | Rest], Result) when is_tuple(Param) ->
    decode_agent_address(Rest, Result ++ [{AddressAttr, [get_host_port_from_struct(AddressStruct)]}]);
decode_agent_address([{AddressAttr, AddressList} | Rest], Result) ->
    ParsedAddrList = lists:map(fun(AddressStruct) -> 
                                    get_host_port_from_struct(AddressStruct)
                               end, AddressList),
    decode_agent_address(Rest, Result ++ [{AddressAttr, ParsedAddrList}]).

get_host_port_from_struct(AddressStruct) ->
    Host = proplists:get_value(host, AddressStruct, undefined),
    Port = proplists:get_value(port, AddressStruct, undefined),
    {Host, Port}.

encode_agent_address(AgentAddress) ->
    encode_agent_address(AgentAddress, []).

encode_agent_address([], []) ->
    undefined;
encode_agent_address([], Result) ->
    Result;
encode_agent_address([{Address, Port} | AgentAddress], Result) when is_integer(Port) ->
    encode_agent_address([{Address, integer_to_list(Port)} | AgentAddress], Result);
encode_agent_address([{Address, Port} | AgentAddress], Result) ->
    encode_agent_address(AgentAddress, Result ++ [[{structName, 'HostAndPort'}, 
                                                   {host, Address}, {port, Port}]]).
    
