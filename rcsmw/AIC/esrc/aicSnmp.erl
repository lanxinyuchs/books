%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	aicSnmp.erl %
%%% @author estjako
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R10A/R11A/R12A/3
%%% @doc ==Handles discovery MIB==
%%% @end
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(aicSnmp).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R10A/R11A/R12A/3').
-date('2017-11-08').
-author('estjako').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% ----------------------------------------------------------
%%% R3A/1      2015-01-09 etxtory     Tie port 199 to 127.0.0.1
%%% R3A/2      2015-02-02 etxlg       Work better with empty OAM IP
%%% R3A/3      2015-02-02 etxtory     Work even better with empty OAM IP
%%% R3A/4      2015-04-13 etxtory     Removed "add_view"; not working and problem
%%%                                   with installation (comea-snmp kills add-view sed)
%%%                                   set_discovery_status for eriDiscoveryStatus
%%% R3A/5      2015-06-15 etxtory     update_snmp makes sure to update once
%%% ----------------------------------------------------------
%%% R4A/1      2015-08-28 etxtory     HU12176
%%% R4A/2      2015-10-13 etxpejn     update_snmp should only be run on core DU
%%% ----------------------------------------------------------
%%% R5A/1      2015-11-16 etxpeno     remove call to deprecated function
%%% R5A/2      2015-12-22 etxtory     Merge R4A/4
%%% R5A/3      2016-01-28 etxtory     eriDiscoverySecCS
%%% R5A/4      2016-02-02 etxarnu     Changed kill_snmp to call appmServer
%%%                                   Corrected get_so_file for sim
%%% R5A/5      2016-02-16 etxarnu     Fixed kill_snmp with unique file name
%%% R5A/6      2016-02-16 etxarnu     Fixed kill_snmp with no file usage
%%% R5A/7      2016-02-23 etxtory     INFORM trap was not sent via (to LMT)
%%% R5A/8      2016-03-04 etxarnu     Reverted to not call appmServer
%%% R5A/9      2016-03-08 etxarnu     New try with appmServer but now checking
%%%                                   lock file
%%% R5A/10     2016-03-14 etxtory     Stop net-snmpd using APPM
%%% R5A/11     2016-03-31 etxarnu     is_comea_via_appm_enabled added and used
%%% R5A/12     2016-04-12 etxarnu     Bug in kill_snmpd
%%% R6A/3      2016-08-17 enenteo     Update for SNMPv3 over DTLS feature
%%% ----------------------------------------------------------
%%% R9A/1      2017-02-14 etxpeno     support for changing attribute ipVersion
%%%                                   in libcom_fm_snmp.cfg
%%% R9A/2      2017-02-23 etxpeno     Add send_eriChangeIPAddressEvent_trap/1
%%% R9A/3      2017-03-06 etxpeno     remove printout in send_eriChangeIPAddressEvent_trap/1
%%% R9A/5      2017-03-09 etxpeno     In test purpose: remove R9A/1-3
%%% R9A/6      2017-03-09 etxpeno     Add send_eriChangeIPAddressEvent_trap/1 again
%%% R9A/8      2017-03-28 etxpeno     Correct the value eriChangeIPAddressAckFdn in the
%%%				      eriChangeIPAddressEvent SNMP trap
%%% R9A/9      2017-04-05 etxpeno     use 10 seconds as timeout value in stop_com()
%%% R9A/10     2017-04-07 etxtory     eriDiscoveryNodeIpAddrOam1Type for ipv6
%%% R9A/11     2017-04-20 etxpeno     use 20 seconds as timeout value in stop_com()
%%% R9A/12     2017-04-20 etxpeno     (TR HV81969) Don't restart COM when OamIpAddr is not
%%%                                   defined
%%% ----------------------------------------------------------
%%% R10A/1     2017-05-19 etxpeno     Using new functions in comsaI
%%% R10A/2     2017-06-02 etxpeno     Remove usage of os:putenv/2
%%% ----------------------------------------------------------
%%% R11A/1     2017-08-24 etxpeno     always print when sending an
%%%                                   eriChangeIPAddressEvent SNMP trap
%%% ----------------------------------------------------------
%%% R12A/1     2017-11-06 eivmiha     Exported do_send_trap/0 
%%% R12A/2     2017-11-06 estjako     HW37384 
%%% R12A/3     2017-11-08 estjako     Changed do_send_trap for snmpv3 over dtls
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([send_trap/1]).
-export([set_discovery_status/1]).
-export([remove_discovery_status/0]).
-export([update_snmp/0]).
-export([update_oam_ip_data/0]).
-export([is_comea_snmp_locked/0]).
-export([update_snmp_fingerprint/1]).
-export([send_eriChangeIPAddressEvent_trap/1]).

%% Debug function(s)
-export([get_so_file/0]).
-export([kill_snmpd/1]).
-export([get_dn/0]).
-export([do_send_trap/0]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("RcsSnmp.hrl").

-define(SNMP_TARGET_V3, snmpTargetV3).
-define(SNMP_TARGET_V3_DTLS, snmpTargetV3Dtls).

-define(TIMER_INCREASE, 10000).  %% Increase each time with 10secs
-define(MAX_TIMER, 300000).      %% 300sec (5min)

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% Sends the discovery trap again if eriDiscoveryStatus
%%% is set to enabled (1). eriDiscoveryStatus is transferred
%%% from netsnmp to erlang via file (discoveryStatus).
%%% ----------------------------------------------------------
send_trap(OldTimerVal) ->
    DS = filename:join([sysEnv:rcs_root(), "rcs/networkloader/discoveryStatus"]),
    case file:read_file(DS) of
	{ok, <<"1", _T/binary>>} ->
	    %% eriDiscoveryStatus is enabled (1)
	    %% send eriDiscoveryEvent trap
	    case do_send_trap() of
		done ->
		    done;
		continue ->
		    NewTimerVal = get_new_timer_val(OldTimerVal),
		    {ok, NewTimerVal}
	    end;

	{ok, <<"0", _T/binary>>} ->
	    %% eriDiscoveryStatus is disabled (0)
	    %% No more trap sending.
	    %% eriDiscoveryStatus can not change from
	    %% disabled (0) to enabled (1); stopped
	    %% in C-implementation.
	    done;

	{error, _} ->
	    %% Treat this as eriDiscoveryStatus is enabled (1)
	    case do_send_trap() of
		done ->
		    done;
		continue ->
		    NewTimerVal = get_new_timer_val(OldTimerVal),
		    {ok, NewTimerVal}
	    end
    end.

%%% ----------------------------------------------------------
%%% Set SNMP discovery status
%%% ERICSSON-DISCOVERY-MIB:eriDiscoveryStatus
%%% ----------------------------------------------------------
set_discovery_status(disabled) ->
    DS = filename:join([sysEnv:rcs_root(), "rcs/networkloader/discoveryStatus"]),
    file:write_file(DS, integer_to_binary(0)).

%%% ----------------------------------------------------------
%%% Remove SNMP discovery status file
%%% ----------------------------------------------------------
remove_discovery_status() ->
    DS = filename:join([sysEnv:rcs_root(), "rcs/networkloader/discoveryStatus"]),
    file:delete(DS).

%%% ----------------------------------------------------------
%%% Updates net-snmp's snmpd.conf
%%% - Add entry in snmpd.conf to be able to set/get in ERICSSON-DISCOVERY-MIB
%%% - Add entries in snmpd.conf to load shared library in netsnmp
%%% Must only be once per installation/upgrade.
%%% ----------------------------------------------------------
update_snmp() ->
    case clhI:core_state() of
	active ->
	    case application:get_env(comte, com_conf_dir) of
		{ok, ConfDir} ->
		    FlagFile = filename:join([ConfDir, "comea/etc/snmpd.updated"]),
		    case filelib:is_file(FlagFile) of
			true ->
			    ok;
			false ->
			    SnmpdConf = filename:join([ConfDir, "comea/etc/snmpd.conf"]),
			    case update_snmp(SnmpdConf) of
				ok ->
				    %% Create flag-file to ensure not run again
				    file:write_file(FlagFile, <<>>);
				{error, Reason} ->
				    error_msg("~p: update_snmp failed~n~p~n",
					      [?MODULE, Reason])
			    end
		    end;
		Other ->
		    error_msg("~p: Missing erlang application variable ~p~n",
			      [?MODULE, Other])
	    end;
	_ ->
	    %% The node is a regular or stand-by node
	    ok
    end.

update_snmp(SnmpdConf) ->
    %% Change permissions on snmpd.conf
    case file:change_mode(SnmpdConf, 8#00755) of
	ok ->
	    add_data(SnmpdConf); %% ok | {error, Reason}
	{error, Reason} ->
	    {error, {"Changing permissons failed ", Reason}}
    end.

%%% ----------------------------------------------------------
%%% Updates net-snmp's snmpd.conf
%%% - clientaddr OamIpAddr
%%%   This is needed to avoid linux to stamp the source IP addr
%%%   with the physical address instead of a required other
%%%   IP addr (example: loopback IP address as OAM IP address)
%%% ----------------------------------------------------------
update_oam_ip_data() ->
    OamIpAddr =  ootI:get_oap_ip_addr(),
    %% OamIpAddr = [] | IpAddr
    do_update_oam_ip(OamIpAddr).

%%% ----------------------------------------------------------
%%% Updates net-snmp's snmpd.conf
%%% - fingerPrint
%%%   Update netsnmp.conf file with fingerprint needed
%%%   to use DTLS in SNMP
%%% ----------------------------------------------------------
update_snmp_fingerprint(Fingerprint) ->
    %%<comea_cmd> snmp configure dtlsNodeCredential <fingerprint>
    Args = "snmp configure dtlsNodeCredential  "  ++  Fingerprint,
    run_comea_script(Args).

%%% ----------------------------------------------------------
%%% Sends the eriChangeIPAddressEvent trap
%%% ----------------------------------------------------------
send_eriChangeIPAddressEvent_trap(Data) ->
    AgentX = filename:join(sysEnv:releases_vsn_dir(),
    			   "comte/comea/run/agentx-socket"),

    VarBinds = get_var_binds_dynamicIpAddress(Data),

    %% env MIBDIRS and SNMP_PERSISTENT_FILE needs to be set
    %% for net-snmp (snmpd).
    Cmd = string:join([get_agentx_env(),
		       "agentxtrap -x",
		       AgentX,
		       VarBinds], " "),

    Result = os:cmd(Cmd),
    info_msg("Sending eriChangeIPAddressEvent trap~n"
	     "~s~n"
	     "Result ~p~n",
	     [Cmd, Result]),
    ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% Sends the actual trap (via netsnmp)
%%% The trap will only be to all SNMP v3 targets.
%%% ----------------------------------------------------------
do_send_trap() ->
	case {ets:first(?SNMP_TARGET_V3),ets:first(?SNMP_TARGET_V3_DTLS)} of
	{'$end_of_table','$end_of_table'}  ->
		info_msg("No autointegration discovery trap is sent. "
		 "No SNMP V3 targets and SNMP V3 DTLS targets defined ~n", []),
		done;
	{V3Key, V3DtlsKey} ->
		V3 = get_snmp_targets(V3Key, [], ?SNMP_TARGET_V3),
		V3Dtls = get_snmp_targets(V3DtlsKey, V3, ?SNMP_TARGET_V3_DTLS),
		VarBinds = get_var_binds(),
		do_send_trap(VarBinds, V3Dtls),
		continue
	end.
% Check if there is some unlocked target. It's ok to have just one target to send trap
target_unlocked(V3Targets) ->
	case lists:filter(fun(#snmpTargetV3{administrativeState = ?BasicAdmState_UNLOCKED}) -> true;
				     (#snmpTargetV3Dtls{administrativeState = ?BasicAdmState_UNLOCKED}) -> true;
				     (_) -> false
				end, V3Targets) of
		[] -> no;
		_-> yes
	end.

do_send_trap(_VarBinds, []) -> ok;
do_send_trap(VarBinds, V3Targets) ->
    case target_unlocked(V3Targets) of
        yes ->
        %% UNLOCKED (but catch all just in case)
        AgentX = filename:join(sysEnv:releases_vsn_dir(),
                 "comte/comea/run/agentx-socket"),
        %% env MIBDIRS and SNMP_PERSISTENT_FILE needs to be set
        %% for net-snmp (snmpd).
        Cmd = string:join([get_agentx_env(),
              "agentxtrap -x",
              AgentX,
              VarBinds], " "),

        Result = os:cmd(Cmd),
        info_msg("Autointegration trap~n~s~n"
                 "Result ~p~n",
                  [Cmd, Result]);
        no -> 
        info_msg("~p:no unlocked snmpTargetV3s or snmpTargetV3Dtls targets~n", [?MODULE])
    end.

get_snmp_targets('$end_of_table', Acc, _) -> Acc;
get_snmp_targets(Key, Acc, TargetType) ->
	NewTarget =
	case ets:lookup(TargetType, Key) of
		[] ->
		%% Should really not happen.
		[];
		[V3Target] when is_record(V3Target, snmpTargetV3) ->
		[V3Target];
		[V3TargetDtls] when is_record(V3TargetDtls, snmpTargetV3Dtls) ->
		[V3TargetDtls];
		Other ->
		error_msg("~p:get_snmpTargetV3s. get_snmpTagetsV3Dtls failed~n~p~n", [?MODULE, Other]),
		[]
	end,
	NextKey = ets:next(TargetType, Key),
	get_snmp_targets(NextKey, lists:append(Acc, NewTarget), TargetType).

get_var_binds() ->
    NodeType = "2",  %% serial (1) | fdn (2)
    DN = get_dn(),
    SecCS = "2147483647",
    SecCSVer = "\"1.0\"",
    {NodeIpType, NodeIpAddr} = get_node_oam_ip_and_type(),
    "ERICSSON-DISCOVERY-MIB::eriDiscoveryEvent "
	"ERICSSON-DISCOVERY-MIB::eriDiscoveryNodeIdType.0 i " ++ NodeType ++ " " ++
	"ERICSSON-DISCOVERY-MIB::eriDiscoveryNodeIdValue s " ++ DN ++ " " ++
	"ERICSSON-DISCOVERY-MIB::eriDiscoverySecCS i " ++ SecCS ++ " " ++
	"ERICSSON-DISCOVERY-MIB::eriDiscoverySecCSVer s " ++ SecCSVer ++ " " ++
	"ERICSSON-DISCOVERY-MIB::eriDiscoveryNodeIpAddrOam1Type i " ++ NodeIpType ++ " " ++
	"ERICSSON-DISCOVERY-MIB::eriDiscoveryNodeIpAddrOam1 s " ++ NodeIpAddr ++ " ".

get_dn() ->
    Med = comsaI:get_managed_element_data(),
    case lists:keyfind(networkManagedElementId, 1, Med) of
	{networkManagedElementId, Str} when is_list(Str) ->
	    "\"" ++ Str ++ "\" ";
	_Other ->
	    info_msg("networkManagedElementId not set. RAN integration won't work!! ~n",
		     []),
	    FakeDn = "networkManagedElementId not set in siteBasic file",
	    "\"" ++ FakeDn ++ "\" "
    end.

get_node_oam_ip_and_type() ->
    case ootI:get_oap_ip_addr() of
	[] ->
	    %% Must fill in something
	    info_msg("No OAP IP address set~n", []),
	    NodeIpType = "1", %% InetAddressType: ipv4 (1)
	    NodeIpAddr = "127.0.0.1" ++ " ",
	    {NodeIpType, NodeIpAddr};
	IpAddr ->
	    NodeIpType = get_node_type(IpAddr),
	    NodeIpAddr = IpAddr ++ " ",
	    {NodeIpType, NodeIpAddr}
    end.

get_node_type(IpAddr) ->
    case inet:parse_address(IpAddr) of
	{ok, {_I1, _I2, _I3, _I4}} ->
	    %% InetAddressType: ipv4 (1)
	    "1";
	{ok, {_I1, _I2, _I3, _I4, _I5, _I6, _I7, _I8}} ->
	    %% InetAddressType: ipv6(2)
	    "2";
	Other ->
	    warning_msg("Failed to get type of IP address: ~p~n", [Other]),
	    "2"
	end.

get_var_binds_dynamicIpAddress(EriChangeIPAddressData) ->
    Nme = get_nme(),
    NodeIdValue = get_node_rdn(Nme),
    NodeIpType = maps:get(eriChangeIPAddressNewNodeOamIpAddressType,
			  EriChangeIPAddressData),
    NodeIpAddr = maps:get(eriChangeIPAddressNewNodeOamIpAddress,
			  EriChangeIPAddressData),
    AckFdn = "ManagedElement="++Nme++",NodeSupport=1,OamIpSupport=1",
    AckAttrName = "ipAddressChangeStatus",
    AckAttrValue = maps:get(eriChangeIPAddressAckAttributeValue,
			    EriChangeIPAddressData),
    Retries = maps:get(eriChangeIPAddressRemainingRetries,
		       EriChangeIPAddressData, "0"),

    "ERICSSON-CHANGEIPADDRESS-MIB::eriChangeIPAddressEvent"
	" ERICSSON-CHANGEIPADDRESS-MIB::eriChangeIPAddressNodeIdValue s " ++ NodeIdValue ++
	" ERICSSON-CHANGEIPADDRESS-MIB::eriChangeIPAddressNewNodeOamIpAddressType i " ++ NodeIpType ++
	" ERICSSON-CHANGEIPADDRESS-MIB::eriChangeIPAddressNewNodeOamIpAddress s " ++ NodeIpAddr ++
	" ERICSSON-CHANGEIPADDRESS-MIB::eriChangeIPAddressAckFdn s " ++ AckFdn ++
	" ERICSSON-CHANGEIPADDRESS-MIB::eriChangeIPAddressAckAttributeName s " ++ AckAttrName ++
	" ERICSSON-CHANGEIPADDRESS-MIB::eriChangeIPAddressAckAttributeValue i " ++ AckAttrValue ++
	" ERICSSON-CHANGEIPADDRESS-MIB::eriChangeIPAddressRemainingRetries i " ++ Retries.

get_nme() ->
    Med = comsaI:get_managed_element_data(),
    Nme = proplists:get_value(networkManagedElementId, Med),
    get_nme(Nme).

get_nme(undefined) -> "1";
get_nme(Nme) when is_list(Nme) -> Nme.

get_node_rdn(Nme) ->
    "\"" ++ Nme ++ "\" ".

%%% ----------------------------------------------------------
%%% Get new timer value
%%% ---------------------------------------------------------
get_new_timer_val(OldTimerVal) when OldTimerVal >= ?MAX_TIMER ->
    ?MAX_TIMER;
get_new_timer_val(OldTimerVal) ->
    OldTimerVal + ?TIMER_INCREASE.

get_agentx_env() ->
    string:join([get_agentx_env("MIBDIRS"),
		 get_agentx_env("SNMP_PERSISTENT_FILE")], " ").

get_agentx_env("MIBDIRS") ->
    StandardPath = sysEnv:dev_patches_dir() ++ ":/usr/share/snmp/mibs/",
    Dirs =
	case application:get_env(comte, com_top) of
	    {ok, CtPath} ->
		ComPath = filename:join([CtPath, "opt/com/etc/mibs"]),
		StandardPath ++ ":" ++ ComPath;
	    _ ->
		error_msg("~p:~p failure~n", [?MODULE, ?FUNCTION_NAME]),
		StandardPath
	end,

    "MIBDIRS=" ++ Dirs;
get_agentx_env("SNMP_PERSISTENT_FILE") ->
    %% Only used by AI, placed in /tmp, does not need to survive restart
    "SNMP_PERSISTENT_FILE=/tmp/snmp_persistent_file".

%%% ---------------------------------------------------------
%%% Functions needed for updating snmpd.conf
%%% ---------------------------------------------------------
add_data(SnmpdConf) ->
    %% Add entry in snmpd.conf to load shared library in netsnmp
    %% Add entry in snmpd.conf to tie port 199 to localhost (127.0.0.1)
    SoFile = get_so_file(),
    case file:open(SnmpdConf, [append, raw]) of
	{ok, Fd} ->
	    Str = "dlmod ericssonDiscoveryMIB " ++ SoFile ++ "\n" ++
		"smuxsocket 127.0.0.1\n",
	    file:write(Fd, list_to_binary(Str)),
	    file:close(Fd),
	    ok;
	{error, Reason} ->
	    {error, {"Open snmpd.conf failed", Reason}}
    end.

get_so_file() ->
    AicPD = code:priv_dir(aic),
    {PreLib,Lib} =
	case sysEnv:architecture() of
	    {"arm", L} ->
		{"tgt_arm-wr6", L};
	    {A, L} ->
	    	{"tgt_"++A, L}
	end,

    OrigFile = filename:join([AicPD, PreLib, Lib, "ericssonDiscoveryMIB.so"]),
    swmI:find_file(OrigFile).

%%% ----------------------------------------------------------
%%% Updates the OAM IP address in snmpd.conf
%%% Change the permission; needed for upgrade but do it always
%%% ----------------------------------------------------------
do_update_oam_ip(OamIpAddr) ->
    case application:get_env(comte, com_conf_dir) of
	{ok, ConfDir} ->
	    SnmpdConf = filename:join([ConfDir, "comea/etc/snmpd.conf"]),
	    do_update_oam_ip(OamIpAddr, SnmpdConf),
	    update_oap_ipv6_file(OamIpAddr),
	    OapIpVersionUpdated = update_oap_ip_version(OamIpAddr),
	    restart_com(OapIpVersionUpdated),
	    ok;
	_ ->
	    ok
    end.

do_update_oam_ip(OamIpAddr = [], SnmpdConf) ->
    %% Corresponds to unsetting the OAP IP address.
    remove_oam_ip_addr(SnmpdConf),
    kill_snmpd(OamIpAddr);
do_update_oam_ip(OamIpAddr, SnmpdConf) ->
    %% OamIpAddr and/or Name space is updated
    remove_oam_ip_addr(SnmpdConf),
    updated_oam_ip_addr(OamIpAddr, SnmpdConf),
    kill_snmpd(OamIpAddr).

remove_oam_ip_addr(SnmpdConf) ->
    Cmd = ["sed '/^\\[snmp]$/,/^\\[snmpd]$/d' ",
	   SnmpdConf, " >/tmp/snmpd.conf",
	   " && ", "mv -f /tmp/snmpd.conf ",
	   SnmpdConf],
    os:cmd(Cmd),
    file:change_mode(SnmpdConf, 8#00755).

updated_oam_ip_addr(OamIpAddr, SnmpdConf) ->
    case file:open(SnmpdConf, [append, raw]) of
	{ok, Fd} ->
	    Str =
		"[snmp]\n" ++
		"clientaddr " ++ OamIpAddr ++ ":11111\n" ++
		"[snmpd]\n",
	    file:write(Fd, list_to_binary(Str)),
	    file:close(Fd);
	{error, Reason} ->
	    error_msg("Updating snmpd.conf with OAM IP addr failed ~p~n",
		      [{Reason, OamIpAddr}])
    end.

update_oap_ipv6_file(OamIpAddr) ->
    Filename = filename:join([sysEnv:releases_vsn_dir(),
			      "comte","comea","oap_ipv6_addr.conf"]),
    case inet:parse_ipv6strict_address(OamIpAddr) of
	{ok, _IPv6Address} ->
	    Bytes = [",udp6:[", OamIpAddr, "]"],
	    file:write_file(Filename, Bytes),
	    ok;
	_ ->
	    file:delete(Filename),
	    ok
    end.

update_oap_ip_version(OamIpAddr) ->
    IpVersion = ip_version(OamIpAddr),
    CurrentIpVersion = get_current_ip_version(),
    case CurrentIpVersion of
	undefined ->
	    %% Cannot find any ipVersion in the configuration file
	    false;
	IpVersion ->
	    %% No changes
	    false;
	_ when IpVersion == undefined ->
	    %% No IP version defined in OamIpAddr
	    false;
	_ ->
	    info_msg("Update ipVersion in the libcom_fm_snmp.cfg from ~p to ~p~n",
		     [CurrentIpVersion, IpVersion]),
	    comte:updateIpVersion(IpVersion),
	    true
    end.

ip_version(OamIpAddr) ->
    ip_version2(inet:parse_address(OamIpAddr)).

ip_version2({ok, IPv4Address}) when tuple_size(IPv4Address) == 4 -> "IPV4";
ip_version2({ok, IPv6Address}) when tuple_size(IPv6Address) == 8 -> "IPV6";
ip_version2({error, einval})                                     -> undefined.

get_current_ip_version() ->
    case application:get_env(comte, com_conf_dir) of
	{ok, ConfDir} ->
	    LibcomFmSnmpConf = filename:join([ConfDir, "libcom_fm_snmp.cfg"]),
	    case file:read_file(LibcomFmSnmpConf) of
		{error, _} ->
		    undefined;
		{ok, Binary} ->
		    case re:run(Binary,
				"<ipVersion>(.*)</ipVersion>",
				[{capture, all_but_first, list}]) of
			{match,[IpVersion]} ->
			    string:to_upper(IpVersion);
			_ ->
			    undefined
		    end
	    end;
	_ ->
	    undefined
    end.

restart_com(true) ->
    info_msg("Trying to restart COM~n", []),
    case stop_com() of
    	ok ->
	    info_msg("Restart COM~n", []),
    	    start_com();
    	{error, already_stopped} ->
	    info_msg("COM already stopped~n", []),
    	    ok
    end;
restart_com(false) ->
    ok.

stop_com() ->
    comsaI:com_stopped(),
    %% 20 seconds timeout
    comte:stop_com(20000).

start_com() ->
    comte:start_com(),
    comsaI:com_started().

%%% ----------------------------------------------------------
%% Calls appm to terminate snmpd.
%% COM detects this, restarts the snmpd process and snmpd will reload the
%% new configurarion.
%% kill -HUP <snmpd> or UCD-SNMP-MIB::versionUpdateConfig (1) should reload the
%% configuration but when doing any of these two, the alarm sending from snmpd
%% stops to work.
%%% ----------------------------------------------------------
kill_snmpd(OamIpAddr) ->
    info_msg("Changing OAM IP address or Name Space ~p~n"
	     "Need to restart SNMP to update configuration ~n",
	     [OamIpAddr]),
    appmServer:comea_snmp(stop_from_aic).

%%% ----------------------------------------------------------
%%%  The comea-snmp.lock file is set in appmServer when comea-snmp is called
%%%  and removed when comea-snmp is ready
%%% ----------------------------------------------------------
is_comea_snmp_locked() ->
    LockFile=filename:join([sysEnv:releases_vsn_dir(),
			    "comte","comea","comea-snmp.lock"]),
    case file:read_file_info(LockFile) of
	{error, _} ->
	    false;
	{ok, _} ->
	    true
    end.

%%% ----------------------------------------------------------
%%% error-msg
%%% ----------------------------------------------------------
error_msg(Format, Args) ->
    sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%%% ----------------------------------------------------------
%%%  Wrapper function for fetch and execution of comea
%%%  script
%%% ----------------------------------------------------------
run_comea_script(Args) ->

    ComeaDir = filename:join([sysEnv:releases_vsn_dir(), "comte", "comea"]),
    SnmpdCF = filename:join([ComeaDir, "etc/snmpd.conf"]),

    Env = string:join(["COMEA_ROOT_DIR=" ++ ComeaDir,
		       "COMEA_CONF_DIR=" ++ ComeaDir,
		       "SNMPD_CONF=" ++ SnmpdCF], " "),
    ComeaScript = filename:join([ComeaDir, "bin/comea"]),
    ComeaCmd = string:join([Env, ComeaScript, Args], " "),

    %%TODO update to check the output of os:cmd
    os:cmd(ComeaCmd).

%%TODO check if the method should come here???
%%run_snmpset_cmd(Args)->
%%    SnmpsetCmd = "snmpset " ++ Args,
%%    os:cmd(SnmpsetCmd).
