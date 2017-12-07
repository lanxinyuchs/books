%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	oot_SUITE.erl %
%%% Author:	eolaand
%%% @version /main/R2A/R3A/R4A/R6A/R8A/R10A/R11A/R12A/3

%%% @doc 
%%% == Test suite for one sim or target env ==
%%% Test set and get of OamAccessPoint attributes
%%% @end

-module(oot_SUITE).
-include_lib("common_test/include/ct.hrl").

%%% ----------------------------------------------------------
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
%%% -----      ---------  --------    ------------------------
%%% R1A/1      2014-03-07 uabesvi     Created
%%% R3A/5      2015-02-28 etxkols     Preparation for cluster
%%% R3A/6      2015-03-24 eolaand     Get ports from hooks instead of rpc
%%% R3A/7      2015-03-25 eolaand     Check that new ports are free
%%% R3A/9      2015-05-04 etxivri     Update to make it more robust  
%%% R3A/10     2015-07-10 etxjovp     Add group definitions used by CS CI   
%%% R4A/2      2015-10-22 eolaand     Add test cases for new ECIM model
%%% R4A/3      2015-10-22 eolaand     Remove alt OAP test cases for now
%%% R4A/4      2015-10-23 eolaand     Wait in end_per_suite until netconf is up
%%% R6A/1      2016-09-16 eolaand     Increase Wait time in end_per_suite
%%% R12A/2     2017-10-31 eolaand     Deprecated attributes not supported 
%%% R12A/3     2017-11-01 eolaand     Revert to support deprecated attributes
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 init_per_group/2,
	 end_per_group/2,
	 all/0,
	 groups/0]).


-export([set_oap_access_point_ipv4/1,
	 set_oap_ipv4address/1,
	 set_alt_oap_access_point_ipv4/1,
	 set_alt_oap_ipv4address/1,
	 check_oap_address_unique/1,
	 set_oap_ssh_port/1,
	 set_oap_netconf_port/1,
	 set_oap_dscp/1,
	 set_cli_ssh_port/1,
	 set_netconf_ssh_port/1,
	 set_otc_dscp/1]).

%% -compile(export_all).


-define(TNODE, n1).
-define(NETCNF, nc1).
-define(CLI, c1).

-define(COMTOP,  {xmlns, "urn:com:ericsson:ecim:ComTop"}).


-define(L2B(__L), list_to_binary(__L)).

-define(NC_NAMESPACE, "urn:ietf:params:xml:ns:netconf:base:1.0").

-define(DELETE,  [{'xmlns:nc',     ?NC_NAMESPACE},
		  {'nc:operation', "delete"}]).


-define(DEFAULT_USER, "expert").
-define(DEFAULT_PASSWORD, "expert").
-define(TARGET_NC_PORT_830, 830).
-define(TARGET_NC_PORT, 2022).
-define(TARGET_CLI_PORT, 2023).
-define(PORT_ADD, 35).
-define(DEFAULT_PROMPT, ">$").
-define(CLI_OR_COLI, cli).
-define(DEFAULT_OPTS, [{user, ?DEFAULT_USER}, {password, ?DEFAULT_PASSWORD}]).
-define(OAP_ID, "1").
-define(ALT_OAP_ID, "Alternative").
-define(IPV4_IF_ID1, "1").
-define(IPV4_IF_ID2, "2").
-define(IPV4_IF_ID3, "3").
-define(IPV4_IF_LDN1, 
	"ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=1,AddressIPv4=1").
-define(IPV4_IF_LDN2, 
	"ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=2,AddressIPv4=1").
-define(IPV4_IF_LDN3, 
	"ManagedElement=1,Transport=1,Router=1,InterfaceIPv4=3,AddressIPv4=1").
-define(ERLANG_EXCLUDE, 
	"Executing Tx including obsolete OamAccessPoint attribute").
%%--------------------------------------------------------------------
%% @doc
%% Used hooks: rct_htmllink, rct_netconf, cth_conn_log
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks,
      [
       {rct_logging, {all, [{erlang,{["ERROR REPORT","CRASH REPORT"],
				     [
				      %% ?ERLANG_EXCLUDE
				     ]}}]}},
       {rct_netconf, ?NETCNF},
       {rct_rpc, ?TNODE},
       {rct_cli, {?CLI,[manual_connect]}},
       {cth_conn_log,[]},
       {oot_log_hook,[{nodeId, ?TNODE}]}
      ]}].



%% @hidden
init_per_suite(Config) ->
    %%================================
    %% Create addressIPv4 instance
    %%================================
    %% AddrFmt = "100.11.22.33/32",
    IP = get_new_ip_addr(),
    Addr = IP ++ "/32",
    open_trans(),
    create_tn_ipv4_address(?IPV4_IF_ID1, Addr),
    %% create_tn_ipv4_address(?IPV4_IF_ID2, Addr),
    %% create_tn_ipv4_address(?IPV4_IF_ID3, Addr),
    Ldn = ?IPV4_IF_LDN1,
    OapId = ?OAP_ID,
    ct:pal("Get old OamAccessPoint=~p", [OapId]),
    OldOap = get_oap_attr_vals(OapId),
    RestoreValsOap = get_oap_attr_restore_val(accessPoint, OldOap),
    ct:pal("Set accessPoint ~s for OamAccessPoint=~p", [Ldn, OapId]),
    set_oap_attr_val(accessPoint, Ldn, OapId),
    %% AltOapId = ?ALT_OAP_ID,
    %% ct:pal("Get old OamAccessPoint=~p", [AltOapId]),
    %% OldAltOap = get_oap_attr_vals(AltOapId),
    %% RestoreValsAltOap = get_oap_attr_restore_val(accessPoint, OldAltOap),
    %% ct:pal("Delete existing accessPoint for OamAccessPoint=~p", [AltOapId]),
    %% delete_oap_attr_val(accessPoint, AltOapId),
    ok = close_trans(),
    [{oap_restore_vals, RestoreValsOap}
     %% {alt_oap_restore_vals, RestoreValsAltOap} 
     | Config].

%% @hidden
end_per_suite(Config) ->
    RestoreValsOap = ?config(oap_restore_vals, Config),
    %% RestoreValsAltOap = ?config(alt_oap_restore_vals, Config),
    open_trans(),
    %% ct:pal("Restore OamAccessPoint=~p to original value: ~p", 
    %% 	   [?ALT_OAP_ID, RestoreValsAltOap]),
    %% restore_oap(RestoreValsAltOap),
    ct:pal("Restore OamAccessPoint=~p to original value: ~p", 
	   [?OAP_ID, RestoreValsOap]),
    restore_oap(RestoreValsOap),
    ok = close_trans(),
    timer:sleep(5000),
    ok = wait_netconf_up().

%% @hidden
init_per_group(_Group, Config) ->
    Config.

%% @hidden
end_per_group(_Group, _Config) ->
    ok.

%% @hidden
init_per_testcase(TestCase, Config) 
  when TestCase =:= set_oap_access_point_ipv4;
       TestCase =:= set_oap_ipv4address;
       TestCase =:= check_oap_address_unique ->
    log_tc_start(TestCase),
    OapId = ?OAP_ID,
    open_trans(),
    ct:pal("Delete existing accessPoint for OamAccessPoint=~p", [OapId]),
    delete_oap_attr_val(accessPoint, OapId),
    ok = close_trans(),
    Config;

init_per_testcase(TestCase, Config) ->
    log_tc_start(TestCase),
    Config.
    

%% @hidden
end_per_testcase(TestCase, _Config)
  when TestCase =:= set_oap_access_point_ipv4;
       TestCase =:= set_oap_ipv4address;
       TestCase =:= check_oap_address_unique ->
    Ldn = ?IPV4_IF_LDN1,
    OapId = ?OAP_ID,
    open_trans(),
    ct:pal("Set accessPoint ~s for OamAccessPoint=~p", [Ldn, OapId]),
    set_oap_attr_val(accessPoint, Ldn, OapId),
    ok = close_trans(),
    ok;

end_per_testcase(_TestCase, _Config) ->
    ok.



%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
     set_oap_access_point_ipv4,
     set_oap_ipv4address,
     %% set_alt_oap_access_point_ipv4,
     %% set_alt_oap_ipv4address,
     check_oap_address_unique,
     set_oap_ssh_port,
     set_oap_netconf_port,
     set_oap_dscp,
     set_cli_ssh_port,
     set_netconf_ssh_port,
     set_otc_dscp
    ].


groups() ->
    AllGroup = all(),
    [
     {default__group, [], AllGroup},
     {sbc__qual__all__1__group, [], []},
     {sbc__def__all__1__group, [], [{group, default__group}]},  
     {sbc__upgrade__all__1__group, [], [{group, default__group}]},  
     {sdc__cover__sim__1__group, [], [{group, default__group}]},
     {sdc__def__all__1__group, [], [{group, default__group}]},  
     {sdc__qual__all__1__group, [], []}
    ].


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR TESTCASES
%%% #---------------------------------------------------------


%%========================================================================
%% set_oap_address(Config) -> ok.
%% 
%% @doc 
%% Set OAM Access Point accessPoint.
%% @end
%%========================================================================
set_oap_access_point_ipv4(_Config) ->
    Ldn = ?IPV4_IF_LDN1,
    OapId = ?OAP_ID,

    %%================================
    %% Set the new value
    %%================================
    open_trans(),
    ct:pal("Set new accessPoint ~s for OamAccessPoint=~p", [Ldn, OapId]),
    set_oap_attr_val(accessPoint, Ldn, OapId),
    ok = close_trans(),

    %%================================
    %% Get the new value
    %%================================
    open_trans(),
    ct:pal("Get the new accessPoint", []),
    NewAddr = get_oap_attr_val(accessPoint, OapId),
    ct:pal("New accessPoint: ~s", [NewAddr]),
    NewAddr = Ldn,
    ok = close_trans().



%%========================================================================
%% set_oap_ipv4address(Config) -> ok.
%% 
%% @doc 
%% Set OAM Access Point IPV4 address.
%% @end
%%========================================================================
set_oap_ipv4address(_Config) ->
    Ldn = ?IPV4_IF_LDN1,
    OapId = ?OAP_ID,

    %%================================
    %% Set the new value
    %%================================
    open_trans(),
    ct:pal("Set new ipv4address ~s for OamAccessPoint=~p", [Ldn, OapId]),
    set_oap_attr_val(ipv4address, Ldn, OapId),
    ok = close_trans(),

    %%================================
    %% Get the new value
    %%================================
    open_trans(),
    ct:pal("Get the new ipv4address", []),
    NewAddr = get_oap_attr_val(ipv4address, OapId),
    ct:pal("New ipv4address: ~s", [NewAddr]),
    NewAddr = Ldn,
    ok = close_trans().


%%========================================================================
%% set_alt_oap_access_point_ipv4(Config) -> ok.
%% 
%% @doc 
%% Set OAM Access Point accessPoint.
%% @end
%%========================================================================
set_alt_oap_access_point_ipv4(_Config) ->
    Ldn = ?IPV4_IF_LDN2,
    OapId = ?ALT_OAP_ID,

    %%================================
    %% Set the new value
    %%================================
    open_trans(),
    ct:pal("Set new accessPoint ~s for OamAccessPoint=~p", [Ldn, OapId]),
    set_oap_attr_val(accessPoint, Ldn, OapId),
    ok = close_trans(),

    %%================================
    %% Get the new value
    %%================================
    open_trans(),
    ct:pal("Get the new accessPoint", []),
    NewAddr = get_oap_attr_val(accessPoint, OapId),
    ct:pal("New accessPoint: ~s", [NewAddr]),
    NewAddr = Ldn,

    ok = close_trans().



%%========================================================================
%% set_alt_oap_ipv4address(Config) -> ok.
%% 
%% @doc 
%% Set OAM Access Point IPV4 address.
%% @end
%%========================================================================
set_alt_oap_ipv4address(_Config) ->
    Ldn = ?IPV4_IF_LDN2,
    OapId = ?ALT_OAP_ID,

    %%================================
    %% Set the new value
    %%================================
    open_trans(),
    ct:pal("Set new ipv4address ~s for OamAccessPoint=~p", [Ldn, OapId]),
    set_oap_attr_val(ipv4address, Ldn, OapId),

    ok = close_trans(),

    open_trans(),
    ct:pal("Get the new ipv4address", []),
    NewAddr = get_oap_attr_val(ipv4address, OapId),
    ct:pal("New ipv4address: ~s", [NewAddr]),
    NewAddr = Ldn,

    ok = close_trans().

%%========================================================================
%% check_oap_address_unique(Config) -> ok.
%% 
%% @doc 
%% Verify that the same address cannot be set on both
%% OamAccessPoint=1 and OamAccessPoint=Alternative
%% @end
%%========================================================================
check_oap_address_unique(_Config) ->
    Ldn = ?IPV4_IF_LDN1,
    OapId = ?OAP_ID,
    AltOapId = ?ALT_OAP_ID,

    %%================================
    %% Set the new value
    %%================================
    open_trans(),
    ct:pal("Set the default accessPoint ~s for OamAccessPoint=~p", [Ldn, OapId]),
    set_oap_attr_val(accessPoint, Ldn, OapId),
    ok = close_trans(),


    %%================================
    %% Try setting the same value
    %%================================
    open_trans(),
    ct:pal("Set the alternative accessPoint ~s for OamAccessPoint=~p", [Ldn, AltOapId]),
    set_oap_attr_val(accessPoint, Ldn, AltOapId),
    {error, Reason} = close_trans(),
    ct:log("Transaction successfully failed with reason ~p", [Reason]).    

%%========================================================================
%% set_oap_ssh_port(Config) -> ok.
%% 
%% @doc 
%% Set OAM Access Point ssh port.
%% @end
%%========================================================================
set_oap_ssh_port(_Config) ->
    {ok, SshPort} = rct_cli:get_port(?CLI),
    {ok, NewSshPort} = get_new_cli_port(SshPort),
    ct:pal("Set new CLI port ~p", [NewSshPort]),
    open_trans(),
    ok = ct_netconfc:edit_config(?NETCNF,
				 running,
				 {'ManagedElement',
				  [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'SystemFunctions',
				    [], 
				    [{systemFunctionsId, [], ["1"]},
				     {'SysM',
				      [],
				      [{sysMId, [], ["1"]},
				       {'OamAccessPoint', 
					[],
					[{oamAccessPointId, [], ["1"]},
					 {sshPort, [], [itl(NewSshPort)]}]
				       }]
				     }]
				   }]
				 }),
    
    ok = close_trans(),
    timer:sleep(5000),
    {ok, IP} = rct_cli_coli_common:get_ip(?CLI),
    timer:sleep(3000),
    ok = rct_cli_coli_common:connect(?CLI, IP, NewSshPort, ?DEFAULT_USER, 
				     ?DEFAULT_PASSWORD, ?DEFAULT_PROMPT, 
				     print),
    ok = rct_cli:disconnect(?CLI),
    ct:pal("Set old CLI port ~p", [SshPort]),
    open_trans(),

    ok = ct_netconfc:edit_config(?NETCNF,
				 running,
				 {'ManagedElement',
				  [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'SystemFunctions',
				    [], 
				    [{systemFunctionsId, [], ["1"]},
				     {'SysM',
				      [],
				      [{sysMId, [], ["1"]},
				       {'OamAccessPoint', 
					[],
					[{oamAccessPointId, [], ["1"]},
					 {sshPort, [], [itl(SshPort)]}]
				       }]
				     }]
				   }]
 				 }),
    
    ok = close_trans(),
    timer:sleep(5000),
    ok = rct_cli_coli_common:connect(?CLI, IP, SshPort, ?DEFAULT_USER, 
				     ?DEFAULT_PASSWORD, ?DEFAULT_PROMPT, 
				     print),
    ok = rct_cli:disconnect(?CLI).


%%========================================================================
%% set_oap_netconf_port(Config) -> ok.
%% 
%% @doc 
%% Set OAM Access Point netconf port.
%% @end
%%========================================================================
set_oap_netconf_port(_Config) ->
    {ok, NCPort} = rct_netconf:get_port(?NETCNF),
    {ok, NewNCPort} = get_new_netconf_port(NCPort),
    ct:pal("Set new NetConf port ~p", [NewNCPort]),
    {ok, _} = open_trans(),
    ok = ct_netconfc:edit_config(?NETCNF,
				 running,
				 {'ManagedElement',
				  [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'SystemFunctions',
				    [], 
				    [{systemFunctionsId, [], ["1"]},
				     {'SysM',
				      [],
				      [{sysMId, [], ["1"]},
				       {'OamAccessPoint', 
					[],
					[{oamAccessPointId, [], ["1"]},
					 {netconfPort, [], [itl(NewNCPort)]}]
				       }]
				     }]
				   }]
				 }),
    ok = close_trans(),
    timer:sleep(5000),
    {error, _} = E = open_trans(), 
    ct:log("Expected failure to connect on old port: ~p", [E]),
    {ok, IP} = rct_cli_coli_common:get_ip(?CLI),
    ct:pal("Set old NetConf port ~p", [NCPort]),
    {ok, Handle} = open_trans(IP, NewNCPort),
    ok = ct_netconfc:edit_config(Handle,
				 running,
				 {'ManagedElement',
				  [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'SystemFunctions',
				    [], 
				    [{systemFunctionsId, [], ["1"]},
				     {'SysM',
				      [],
				      [{sysMId, [], ["1"]},
				       {'OamAccessPoint', 
					[],
					[{oamAccessPointId, [], ["1"]},
					 {netconfPort, [], [itl(NCPort)]}]
				       }]
				     }]
				   }]
				 }),
    ok = close_trans(Handle),
    timer:sleep(5000),
    {ok, _} = open_trans(),
    ok = close_trans().


%%========================================================================
%% set_oap_dscp(Config) -> ok.
%% 
%% @doc 
%% Set OAM Access Point dscp.
%% @end
%%========================================================================
set_oap_dscp(_Config) ->
    open_trans(),
    OapId = ?OAP_ID,
    OldDSCP = get_oap_attr_val(dscp, OapId),
    NewDSCP = get_new_dscp(OldDSCP),
    ct:pal("Set new dscp value ~p", [NewDSCP]),
    set_oap_attr_val(dscp, NewDSCP, OapId),
    ok = close_trans(),
    timer:sleep(3000),
    open_trans(),
    ct:pal("Get the new dscp value", []),
    DSCP = get_oap_attr_val(dscp, OapId),
    ct:pal("New dscp value: ~p", [DSCP]),
    DSCP = NewDSCP,
    ct:pal("Restore old dscp value ~p", [OldDSCP]),
    set_oap_attr_val(dscp, OldDSCP, OapId),
    ok = close_trans().


%%========================================================================
%% set_cli_ssh_port(Config) -> ok.
%% 
%% @doc 
%% Set CliSsh port.
%% @end
%%========================================================================
set_cli_ssh_port(_Config) ->
    {ok, SshPort} = rct_cli:get_port(?CLI),
    {ok, NewSshPort} = get_new_cli_port(SshPort),
    ct:pal("Set new CliSsh port ~p", [NewSshPort]),
    open_trans(),
    ok = ct_netconfc:edit_config(?NETCNF,
				 running,
				 {'ManagedElement',
				  [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'SystemFunctions',
				    [], 
				    [{systemFunctionsId, [], ["1"]},
				     {'SysM',
				      [],
				      [{sysMId, [], ["1"]},
				       {'CliSsh', 
					[],
					[{cliSshId, [], ["1"]},
					 {port, [], [itl(NewSshPort)]}]
				       }]
				     }]
				   }]
				 }),
    
    ok = close_trans(),
    timer:sleep(5000),
    {ok, IP} = rct_cli_coli_common:get_ip(?CLI),
    timer:sleep(3000),
    ok = rct_cli_coli_common:connect(?CLI, IP, NewSshPort, ?DEFAULT_USER, 
				     ?DEFAULT_PASSWORD, ?DEFAULT_PROMPT, 
				     print),
    ok = rct_cli:disconnect(?CLI),
    ct:pal("Set old CliSsh port ~p", [SshPort]),
    open_trans(),

    ok = ct_netconfc:edit_config(?NETCNF,
				 running,
				 {'ManagedElement',
				  [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'SystemFunctions',
				    [], 
				    [{systemFunctionsId, [], ["1"]},
				     {'SysM',
				      [],
				      [{sysMId, [], ["1"]},
				       {'CliSsh', 
					[],
					[{cliSshId, [], ["1"]},
					 {port, [], [itl(SshPort)]}]
				       }]
				     }]
				   }]
 				 }),
    
    ok = close_trans(),
    timer:sleep(5000),
    ok = rct_cli_coli_common:connect(?CLI, IP, SshPort, ?DEFAULT_USER, 
				     ?DEFAULT_PASSWORD, ?DEFAULT_PROMPT, 
				     print),
    ok = rct_cli:disconnect(?CLI).


%%========================================================================
%% set_netconf_ssh_port(Config) -> ok.
%% 
%% @doc 
%% Set NetconfSsh port.
%% @end
%%========================================================================
set_netconf_ssh_port(_Config) ->
    {ok, NCPort} = rct_netconf:get_port(?NETCNF),
    {ok, NewNCPort} = get_new_netconf_port(NCPort),
    ct:pal("Set new NetConfSsh port ~p", [NewNCPort]),
    {ok, _} = open_trans(),
    ok = ct_netconfc:edit_config(?NETCNF,
				 running,
				 {'ManagedElement',
				  [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'SystemFunctions',
				    [], 
				    [{systemFunctionsId, [], ["1"]},
				     {'SysM',
				      [],
				      [{sysMId, [], ["1"]},
				       {'NetconfSsh', 
					[],
					[{netconfSshId, [], ["1"]},
					 {port, [], [itl(NewNCPort)]}]
				       }]
				     }]
				   }]
				 }),
    ok = close_trans(),
    timer:sleep(5000),
    {error, _} = E = open_trans(), 
    ct:log("Expected failure to connect on old port: ~p", [E]),
    {ok, IP} = rct_cli_coli_common:get_ip(?CLI),
    ct:pal("Set old NetConfSsh port ~p", [NCPort]),
    {ok, Handle} = open_trans(IP, NewNCPort),
    ok = ct_netconfc:edit_config(Handle,
				 running,
				 {'ManagedElement',
				  [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'SystemFunctions',
				    [], 
				    [{systemFunctionsId, [], ["1"]},
				     {'SysM',
				      [],
				      [{sysMId, [], ["1"]},
				       {'NetconfSsh', 
					[],
					[{netconfSshId, [], ["1"]},
					 {port, [], [itl(NCPort)]}]
				       }]
				     }]
				   }]
				 }),
    ok = close_trans(Handle),
    timer:sleep(5000),
    {ok, _} = open_trans(),
    ok = close_trans().


%%========================================================================
%% set_otc_dscp(Config) -> ok.
%% 
%% @doc 
%% Set OamTrafficClass DSCP.
%% @end
%%========================================================================
set_otc_dscp(_Config) ->
    open_trans(),
    OldDSCP = get_otc_attr_val(dscp),
    NewDSCP = get_new_dscp(OldDSCP),
    ct:pal("Set new dscp value ~p", [NewDSCP]),
    set_otc_attr_val(dscp, NewDSCP),
    ok = close_trans(),
    timer:sleep(3000),
    open_trans(),
    ct:pal("Get the new dscp value", []),
    DSCP = get_otc_attr_val(dscp),
    ct:pal("New dscp value: ~p", [DSCP]),
    DSCP = NewDSCP,
    ct:pal("Restore old dscp value ~p", [OldDSCP]),
    set_otc_attr_val(dscp, OldDSCP),
    ok = close_trans().


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
create_tn_ipv4_address(InterfaceId, IpAddr) ->

    %%================================
    %% Create addressIPv4 instance
    %%================================

    ct:pal("Set IP addr ~s for ~s", [IpAddr, ipv4_ldn(InterfaceId)]),
    %% open_trans(),
    ok = ct_netconfc:edit_config(?NETCNF,
				 running,
				 {'ManagedElement',
				  [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'Transport',
				    [], 
				    [{transportId, [], ["1"]},
				     {'Router',
				      [],
				      [{routerId, [], ["1"]},
				       {'InterfaceIPv4', 
					[],
					[{interfaceIPv4Id, [], [InterfaceId]},
					 {'AddressIPv4', 
					  [],
					  [{addressIPv4Id, [], ["1"]}, 
					   {address, [], [IpAddr]}]}]
				       }]
				     }]}]
				 }).


get_oap_attr_val(Attr, OapId) ->
    {'OamAccessPoint', _, AttrVals} = get_oap_attr_vals(OapId),
    case lists:keyfind(Attr, 1, AttrVals) of
	{_Attr, _, [Val]} ->    
	    Val;
	_False ->
	    undefined
    end.


get_oap_attr_vals(OapId) ->
    {ok,[{'ManagedElement', _, [{managedElementId,[],["1"]},
				{'SystemFunctions',[],
				 [{systemFunctionsId,[],["1"]},
				  {'SysM', _, [{sysMId,[],["1"]},
					       {'OamAccessPoint',
						_,
						_OamAccessPointAttr} 
					       = OldOam]}]}]}]} =
        ct_netconfc:get_config(?NETCNF, 
			       running,
			       {'ManagedElement',
				[?COMTOP],
				[{managedElementId, [], ["1"]},
				 {'SystemFunctions',
				  [], 
				  [{systemFunctionsId, [], ["1"]},
				   {'SysM',
				    [],
				    [{sysMId, [], ["1"]},
				     {'OamAccessPoint', [], 
				      [{oamAccessPointId, [],[OapId]}]}]
				   }]
				 }]
			       }),
    OldOam.


set_oap_attr_val(Attr, Val, OapId) ->

    %%================================
    %% Set the new value
    %%================================
     ok = ct_netconfc:edit_config(?NETCNF,
				 running,
				 {'ManagedElement',
				  [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'SystemFunctions',
				    [], 
			       [{systemFunctionsId, [], ["1"]},
				{'SysM',
				 [],
				 [{sysMId, [], ["1"]},
				  {'OamAccessPoint', 
				   [],
				   [{oamAccessPointId, [], [OapId]},
				    {Attr, [], [Val]}]
				  }]
				}]
				   }]
				 }).
    

delete_oap_attr_val(Attr, OapId) ->

    %%================================
    %% Delete the value
    %%================================
    ok = ct_netconfc:edit_config(?NETCNF,
				 running,
				 {'ManagedElement',
				  [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'SystemFunctions',
				    [], 
				    [{systemFunctionsId, [], ["1"]},
				     {'SysM',
				      [],
				      [{sysMId, [], ["1"]},
				       {'OamAccessPoint', 
					[],
					[{oamAccessPointId, [], [OapId]},
					 {Attr, ?DELETE, []}]
				       }]
				     }]
				   }]
				 }).


restore_oap(OldAttrVals) ->
    ok = ct_netconfc:edit_config(?NETCNF,
				 running,
				 {'ManagedElement',
				  [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'SystemFunctions',
				    [], 
				    [{systemFunctionsId, [], ["1"]},
				     {'SysM',
				      [],
				      [{sysMId, [], ["1"]},
				       OldAttrVals]
				     }]
				   }]
				 }).


get_oap_attr_restore_val(Attr, OldOap) ->
    {'OamAccessPoint', [], OamAccessPointAttr} = OldOap, 
    case lists:keysearch(Attr, 1, OamAccessPointAttr) of
	false -> 
	    Delete = {Attr, ?DELETE, []},
	    OapId = lists:keyfind(oamAccessPointId, 1, OamAccessPointAttr),
	    {'OamAccessPoint', [], [OapId, Delete]};
	{value, _} -> 
	    OldOap
    end.
    

get_otc_attr_val(Attr) ->
    {ok,[{'ManagedElement', _, [{managedElementId,[],["1"]},
				{'SystemFunctions',[],
				 [{systemFunctionsId,[],["1"]},
				  {'SysM', _, [{sysMId,[],["1"]},
					       {'OamTrafficClass',
						_,
						OamTrafficClassAttr}]}]}]}]} =
        ct_netconfc:get_config(?NETCNF, 
			       running,
			       {'ManagedElement',
				[?COMTOP],
				[{managedElementId, [], ["1"]},
				 {'SystemFunctions',
				  [], 
				  [{systemFunctionsId, [], ["1"]},
				   {'SysM',
				    [],
				    [{sysMId, [], ["1"]},
				     {'OamTrafficClass', [], 
				      [{oamTrafficClassId, [],["1"]}]}]
				   }]
				 }]
			       }),
    {_Attr, _, [Val]} = lists:keyfind(Attr, 1, OamTrafficClassAttr),
    Val.


set_otc_attr_val(Attr, Val) ->

    %%================================
    %% Set the new value
    %%================================
    ok = ct_netconfc:edit_config(?NETCNF,
				 running,
				 {'ManagedElement',
				  [?COMTOP],
				  [{managedElementId, [], ["1"]},
				   {'SystemFunctions',
				    [], 
				    [{systemFunctionsId, [], ["1"]},
				     {'SysM',
				      [],
				      [{sysMId, [], ["1"]},
				       {'OamTrafficClass', 
					[],
					[{oamTrafficClassId, [], ["1"]},
					 {Attr, [], [itl(Val)]}]
				       }]
				     }]
				   }]
				 }).


%% exec_trans(Fun) ->
%%     open_trans(),
%%     Res = Fun(),
%%     case close_trans() of
%% 	ok ->
%% 	    Res;
%% 	Error ->
%% 	    Error
%%     end.


open_trans(IP, Port)  -> 
    timer:sleep(3000),
    ct_netconfc:open([{ssh, IP}, {port, Port}] ++ ?DEFAULT_OPTS).

open_trans()  -> 
    timer:sleep(3000),
    ct_netconfc:open(?NETCNF, []).

close_trans() -> ct_netconfc:close_session(?NETCNF).

close_trans(Handle) -> ct_netconfc:close_session(Handle).


wait_netconf_up() ->
    wait_netconf_up(60).


wait_netconf_up(N) when N > 0 ->
    case open_trans() of
	{ok, _} ->
	    ct:pal("Verified that netconf is up again after the tests", []), 
	    timer:sleep(3000),
	    close_trans();    
	_Error ->
	    ct:log("Failed to open netconf session: ~p", [_Error]),
	    wait_netconf_up(N - 1)
    end;

wait_netconf_up(_N) ->
    {error, netconf_not_up}.


get_new_ip_addr() ->
    case is_target() of
	true ->
	    {ok, IP} = rct_cli_coli_common:get_ip(?CLI),
	    IP;
	_False ->
	    get_sim_ip_addr()
    end.

get_sim_ip_addr() ->
    {ok, Host} = inet:gethostname(),
    {ok, IP} = inet:getaddr(Host, inet),
    inet:ntoa(IP).

get_new_netconf_port(?TARGET_NC_PORT_830) ->
    get_next_free_port(?TARGET_NC_PORT);
get_new_netconf_port(Port) ->
    get_next_free_port(Port).

get_new_cli_port(Port) ->
    get_next_free_port(Port).

get_next_free_port(OldPort) ->
    NewPort = OldPort + ?PORT_ADD,
    case is_target() of
	true ->
	    {ok, NewPort};
	_False ->
	    ct:log("Get busy ports!", []),
	    BusyPorts = get_busy_ports(),
	    ct:log("Busy ports = ~p", [BusyPorts]),
	    get_next_free_port(NewPort, BusyPorts)
    end.

get_next_free_port(16#10000, _BusyPorts) ->
    {error, failed_to_find_free_port};
get_next_free_port(NewPort, BusyPorts) ->
    case lists:member(NewPort, BusyPorts) of
	true ->
	    get_next_free_port(NewPort + 1, BusyPorts);
	_False ->
	    {ok, NewPort}
    end.	    

get_busy_ports() ->
    {ok, CSPorts} = rct_rpc:call(?TNODE, sysEnv, get_port_conf, [], 20000),
    [Port || {_, Port} <- CSPorts] ++ tcp_ports("tcp") ++ tcp_ports("tcp6").

get_new_dscp("1") ->
    "2";
get_new_dscp(_) ->
    "1".

is_target() ->
    case os:getenv("SIM_OR_TARGET") of
	"target" ->
	    true;
	_Sim ->
	    false 
    end.

tcp_ports(Tcp) ->
    L = os:cmd("cat /proc/net/" ++ Tcp ++" | cut -f3 -d: | awk '{print $1}'"),
    [_ | Ports] = string:tokens(L, "\n"),
    [list_to_integer(N, 16) || N <- Ports].


ipv4_ldn(?IPV4_IF_ID1) ->
    ?IPV4_IF_LDN1;
ipv4_ldn(?IPV4_IF_ID2) ->
    ?IPV4_IF_LDN2;
ipv4_ldn(?IPV4_IF_ID3) ->
    ?IPV4_IF_LDN3.


itl(I) when is_integer(I) ->
    integer_to_list(I);
itl(L) when is_list(L) ->
    L.


log_tc_start(TC) ->
    ct:print("~n=================== TestCase =======================~n~p~n"
	     "====================================================", [TC]).
    
