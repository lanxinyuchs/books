%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	omc_alt_oam_SUITE.erl %
%%% @author etxlg
%%% @copyright Ericsson AB 2015
%%% @version /main/R4A/1
%%% 
%%% @doc == Test Suite 
%%% This Test Suite needs an UP with TN
%%% <br/><br/>
%%% This attempts to check functionality tied to Alternative Oam Accesss.
%%% Contents is a combination (i.e. copy/paste) of omc_oap_SUITE,
%%% and additional stuff; SSH using rct-hooks, alarms,
%%% sftp, and http (maybe if there was time to add it).
%%% need to ensure all TC aren't run in sequence since the functionality for
%%% handling toggling alarms othervise come into play, causing an alarm to be
%%% permanently on (until some timeout that I don't know what it is).
%%% rct_netconf is used in ct_hooks:
%%% see <a href="file:///vobs/rcs/test/RCT_CRX901275/test/lib/supp/esrc/rct_netconf.erl">rct_netconf.erl</a><br/>
%%% @end


-module(omc_alt_oam_SUITE).
%%% Except as specifically authorized in writing by Ericsson, the 
%%% receiver of this document shall keep the information contained 
%%% herein confidential and shall protect the same in whole or in 
%%% part from disclosure and dissemination to third parties.
%%% 
%%% Disclosure and disseminations to the receivers employees shall 
%%% only be made on a strict need to know basis.
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date        Name        What
%%% -----      ----------  --------    -----------------------
%%% Rx         2015-11-11  etxlg       Created
%%% Rx         2015-11-13  etxlg       First, need remove (some) alarm checks
%%% ----------------------------------------------------------
%%% 

%%-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([suite/0,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 all/0
	 %groups/0
	]).

%%what to test?
%%ok, configure, happens in init_per_suite
%%ok, check that Alternative Oam works at all (netconf)
%%ok, check ssh login netconf (also alarm activating and ceasing)
%%ok, check ssh login CLI (also alarm activating and ceasing)
%%ok, check ssh login RCS-COLI (also alarm activating and ceasing)
%%ok, check ssh login SFTP (also alarm activating and ceasing)
%%ok, check http access
%%ok, check http sftp fileoutput (ESI)
%%HERE FIXME need lab support to disable primary OamAP to test SFTP output,
%%also need to run the test as maintenance user since there won't be any LDAP
%%check ssh fileoutput SFTP (Avli log?)
%%ok remove Alt oam -> NOT possible, add Alt oam, -> possible (netconf)

-export([check_netconf/1]).		%%% ensure connectivity on alt_Oap
-export([check_netconf_and_alarm/1]).	%%% NETCONF access gives alarm
-export([check_cli_and_alarm/1]).	%%% CLI access gives alarm
-export([check_coli_and_alarm/1]).	%%% RCS-COLI access gives alarm
-export([check_sftp_and_alarm/1]).	%%% SFTP (PM-server) access gives alarm
-export([check_https/1]).		%%% Connect https, no actions
-export([https_esi_export/1]).		%%% Connect https, post to export ESI
-export([delete_check_add_check/1]).	%%% remove altOap check no access

%%--------------------------------------------------------------------
%% @doc
%% runs the ct_hooks that shall be used.<br/>
%% rct_netconf. 
%% @end
%%--------------------------------------------------------------------
suite() -> 
    [{ct_hooks, [{rct_rpc, rpc1},
		 {rct_netconf,{nc1, expert_auth}},
 		 {rct_netconf,  [{1, nc_alt, ssh_TN_A_ipv4_alt}]},
 		 %{rct_netconf,  [{1, nc_alt2, ssh_TN_A_ipv4_alt}]},
                 {rct_cli, [{1, cli_alt, ssh_TN_A_ipv4_alt,[manual_connect]}]},
                 {rct_coli,
		      [{1, coli_alt, ssh_TN_A_ipv4_alt,[manual_connect]}]},
		 {rct_http, [{1, http_alt, ssh_TN_A_ipv4_alt}]},
                 {cth_conn_log,[]},
                 {rct_logging, {all,
			[{erlang,{["ERROR REPORT","CRASH REPORT"],
				  ["SSL: certify: ssl_handshake.erl:"]}}]}}
    		]
     }].


%% @hidden
init_per_suite(In_config) ->
    %%There should also be a check for "Complete UP"
    os:getenv("SIM_OR_TARGET") =:= "sim" andalso 
	    ct:fail("This suite does NOT work in simulated environment"),
    crypto:start(),
    ssh:start(),
    ssl:start(),
    inets:start(),
    ct:pal("Preparing ct-loopdata (Config)"),
    Config = prepare_config(In_config), %add; things from stp.cfg
    ct:pal("In_config: ~p~nPrepared_config:~p~n", [In_config, Config]),
    ct:pal("Checking/Configuring the site for OamAccessPoint"),
    add_oap_to_node(Config),
    ct:pal("OamAccessPoint configured, configuring Alternative OamAP"),
    ok = add_alt_oap_to_node(Config),
    ct:pal("Alternative OamAccessPoint configured - ready to run testcases"),
    Config.

%% @hidden
end_per_suite(_Config) ->
	%duh
    ok.

%% @hidden
init_per_testcase(_Tc, Config) ->
    %Config is already prepared in init_per_suite/1
    Config.

end_per_testcase(_Tc, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Runs all testcases in SUITE.
%% @end
%%--------------------------------------------------------------------
all() -> 
    [check_netconf, check_netconf_and_alarm, check_cli_and_alarm,
     check_coli_and_alarm, check_sftp_and_alarm, check_https,
     https_esi_export, delete_check_add_check].

%groups() ->
%%%HERE FIXME
%    AllGroup = all(),
%    [
%     {default__group, [], AllGroup},
%     {sbc__qual__all__1__group, [], []},
%     {sbc__def__all__1__group, [], []},  
%     {sbc__upgrade__all__1__group, [], []},  
%     {sdc__cover__sim__1__group, [], []},
%     {sdc__def__all__1__group, [], []},  
%     {sdc__qual__all__1__group, [], [{group, all_working_1}]},
%     {all_working, [], [{group, all_working_1}]},  
%     {all_working_1, [sequence], [duh, dah]}].

check_netconf(Config) ->
    Me_id = ?config(me_id, Config),
% make a harmless change to confirm we have access
    Change = {'ManagedElement',
		[{'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
		[{managedElementId,[],[Me_id]},
		 {userLabel, [], [?MODULE_STRING]}]},
    ok = alt_netconf(edit_config, [nc_alt, running, Change]),
    ct:pal("Changed ManagedElement:userLabel with netconf - ok").
    
check_netconf_and_alarm(Config) ->
    {ok, _} = ct_netconfc:open(nc_alt, [{user, "muexpert"},
					{password, "muexpert"}]),

    ok = check_wait_for_alarm(Config),
    ok = ct_netconfc:close_session(nc_alt),
    ok = check_wait_for_cease(Config),
    ct:pal("Alternative NETCONF accesss cause/cease alarm - ok").

check_cli_and_alarm(Config) ->
    ok = rct_cli:connect(cli_alt),
    ok = check_wait_for_alarm(Config),
    ok = rct_cli:disconnect(cli_alt),
    ok = check_wait_for_cease(Config),
    ct:pal("Alternative CLI accesss cause/cease alarm - ok").

check_coli_and_alarm(Config) ->
    ok = rct_cli:connect(coli_alt),
    ok = check_wait_for_alarm(Config),
    ok = rct_cli:disconnect(coli_alt),
    ok = check_wait_for_cease(Config),
    ct:pal("Alternative RCS-CLI accesss cause/cease alarm - ok").

check_sftp_and_alarm(Config) ->
    %%this is done manually, don't know if there is a ct_hook
    Tn_ip_alt = ?config(tn_ip_alt, Config),
    Sftp_port = ?config(sftp_port, Config),
    User = ?config(sftp_user, Config),
    Pw = ?config(sftp_pw, Config),
    Opts =
	[{silently_accept_hosts, true}, {user_interaction, false},
	 {connect_timeout, 5000}, {user, User}, {password, Pw}],
    {ok, Pid, Ref} = ssh_sftp:start_channel(Tn_ip_alt, Sftp_port, Opts),
    {ok, Files} = ssh_sftp:list_dir(Pid, "/", 5000),
    ct:pal("Sftp: list dir: ~p~n", [Files]),
    true = lists:member("rop", Files),
    true = lists:member("pm_data", Files),
    ok = check_wait_for_alarm(Config),
    ok = ssh_sftp:stop_channel(Pid),
    ok = ssh:close(Ref),
    ok = check_wait_for_cease(Config),
    ct:pal("Alternative SFTP accesss cause/cease alarm - ok").

check_https(_Config) ->
    {ok, Url} = rct_http:https_url(http_alt, "/ea.html"),
    ct:pal("HTTP for Emergency Access, URL: ~p~n", [Url]),
    case httpc:request(Url) of
	{ok, {{"HTTP/1.1", 200, "OK"}, _Headers, _Doc}} ->
	    ct:pal("HTTPS - ok");
	Unexpected ->
	    ct:fail("HTTPS request failure: ~p", [Unexpected])
    end.

https_esi_export(Config) ->
%%% much stuff copied from aic_httpc.erl
%%% a WARNING report from http/ssl will come, don't know wby
    Tn_ip_alt = ?config(tn_ip_alt, Config),
    EsiLogPath = ?config(priv_dir, Config),
    os:cmd("chmod 777 " ++ EsiLogPath),
    Sftp_params = ct:get_config(sftp_server),
    ct:pal("Obtaining initial (old, \"pre export\") result", []),
    First_export_result = get_first_export_result(Config, 10),

    ct:pal("Posting these sftp params: ~p", [Sftp_params]),
    [{host, SftpHost},{username, Username},{password, Password}] = Sftp_params,
    Body = "host="++SftpHost++"&"++
        "username="++Username++"&"++
        "password="++Password++"&"++
        "directory="++EsiLogPath++"&"++
        "DoESI=ESI",
    Url_https = "https://"++Tn_ip_alt++"/cgi-bin/aicGui:post",
    ct:pal("Exporting ESI to: ~p", [EsiLogPath]),
    case httpc:request(post,
		       {Url_https, [], [], Body},
		       [{timeout, 10 * 60 * 1000}],
		       [{sync, true}]) of
	{ok, {{"HTTP/1.1", 200, "OK"}, _Headers, _Doc}} ->
	    ct:pal("HTTPS POST to export ESI - ok"),
	    wait_check_esi_export(Config, First_export_result);
	Unexpected ->
	    ct:fail("HTTPS POST to expoert ESI failure: ~p", [Unexpected])
    end.

delete_check_add_check(Config) ->
    delete_alt_oap(Config),
    wait_for_not_access(Config),
    create_alt_oap(Config),
    wait_for_access(Config).

wait_for_not_access(Config) ->
    check_access(Config, not_access).
wait_for_access(Config) ->
    check_access(Config, access).

check_access(Config, Wanted) ->
    check_access(Config, Wanted, 10).
check_access(_Config, Wanted, 0) ->
    ct:fail("Error checking netconf access, wanted access is: ~p", [Wanted]);
check_access(Config, Wanted, N) ->
    case {alt_connect_disconnect(), Wanted} of
	{ok, access} ->
	    ct:pal("Netconf access to alt is available, good"),
	    ok;
	{ok, not_access} -> 
	    ct:pal("Netconf access to alt is available, try again"),
	    timer:sleep(1000),
	    check_access(Config, Wanted, N - 1);
	{{error, _}, access} ->
	    ct:pal("Netconf access to alt is unavailable, try again"),
	    timer:sleep(1000),
	    check_access(Config, Wanted, N - 1);
	{{error, _}, not_access} ->
	    ct:pal("Netconf access to alt is unavailable, good"),
	    ok
    end.

get_first_export_result(_Config, 0) ->
    ct:fail("Failed to read first export_esi_result.txt");
get_first_export_result(Config, N) ->
    case get_export_result() of
	"crash" ->
	    timer:sleep(1000),
	    get_first_export_result(Config, N - 1);
	Resp when is_list(Resp) ->
	    Resp
    end.

wait_check_esi_export(Config, First_export_result) ->
    Started = wait_for_begin(Config, First_export_result, 60), %60 times 1sec
    ct:pal("Esi export started: ~p", [Started]),
    wait_for_finish(Config, 6 * 10). % 60 times 10sec

wait_for_begin(_, First_export, 0) ->
    ct:fail("Export never started, export_esi_result.txt: ~p", [First_export]);
wait_for_begin(Config, First_export, N) ->
    case {First_export, get_export_result()} of
	{Same, Same} ->
	    timer:sleep(1000),
	    wait_for_begin(Config, First_export, N - 1);
	{_, Started} ->
	    Started
    end.

wait_for_finish(_Config, 0) ->
    ct:fail("Export never finished, giving up after 10minutes.", []);
wait_for_finish(Config, N) ->
    case is_finished(get_export_result()) of
	true  -> ok;
	false ->
	    timer:sleep(10000),
	    wait_for_finish(Config, N - 1)
    end.

is_finished(Text) ->
%%% example: "2015/11/12 08:55:16 Finished"
    case string:tokens(Text, "\s") of
	[_, _, "Finished"] -> true;
	_ -> false
    end.

get_export_result() ->
    {ok, Url} = rct_http:https_url(http_alt, "/export_esi_result.txt"),
    case httpc:request(Url) of
	{ok, {{"HTTP/1.1", 404, "Object Not Found"}, _Headers, _Doc}} ->
	    %allow this since maybe the file doesn't exist first time around
	    ct:pal("Export result: "),
	    [];
	{ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Doc}} ->
	    ct:pal("Export result: ~p", [Doc]),
	    Doc;
	Unexpected->
	    ct:pal("HTTPS request failure at get_export_result(): ~p",
		   [Unexpected]),
	    ct:pal("Export result: crash"),
	    "crash"
    end.


%%--------------------------------------------------------------------
%% @doc
%% Add needed parameters to Config -proplist,
%% the majority of environment hardcoding is here.
%% @spec prepare_config(Config) -> New_config
%% @end
%%--------------------------------------------------------------------
%% we need
%% ME_ID: assume "1"
%% FieldReplaceableUnit: -> 1
%% TnPort: -> TN_A
%% IPaddress: from stp.cfg via ssh_TN_A_ipv4-tuple

-define(ME_ID, "1").
-define(FRU, "1").
-define(TN_PORT, "TN_A").
-define(SFTPPORT, 2024).
-define(SFTPPW, "expert").
-define(SFTPUSER, "expert").
prepare_config(Config) ->
    Dut = ?config(1, ct:get_config(test_nodes)),
    Dut_props =  ct:get_config(Dut),
    Config_attribute = list_to_atom("ssh_" ++ ?TN_PORT ++ "_ipv4"),
    Config_attribute_alt = list_to_atom("ssh_" ++ ?TN_PORT ++ "_ipv4_alt"),
    Tn_ip = ?config(ssh, ?config(Config_attribute, Dut_props)),
    Tn_mask =
	to_mask(?config(netmask, ?config(Config_attribute, Dut_props))),
    Tn_gw = ?config(gateway, ?config(Config_attribute, Dut_props)),
    Tn_vlan = ?config(vlan, ?config(Config_attribute, Dut_props)),
    Tn_vlan_port = ?TN_PORT ++"_OAM_" ++ integer_to_list(Tn_vlan),
    Tn_ip_alt = ?config(ssh, ?config(Config_attribute_alt, Dut_props)),
    Tn_mask_alt =
	to_mask(?config(netmask, ?config(Config_attribute_alt, Dut_props))),
    Tn_gw_alt = ?config(gateway, ?config(Config_attribute_alt, Dut_props)),
    Tn_vlan_alt = ?config(vlan, ?config(Config_attribute_alt, Dut_props)),
    Tn_vlan_port_alt = ?TN_PORT ++"_OAM_" ++ integer_to_list(Tn_vlan_alt),
    [
	{me_id, ?ME_ID},
	{fru, ?FRU},
	{sftp_port, ?SFTPPORT},
	{sftp_user, ?SFTPUSER},
	{sftp_pw, ?SFTPPW},
	{tn_port, ?TN_PORT},
	{tn_ip, Tn_ip},
	{tn_vlan, Tn_vlan},
	{tn_vlan_port, Tn_vlan_port},
	{tn_gw, Tn_gw},
	{tn_mask, Tn_mask},
	{tn_ip_alt, Tn_ip_alt},
	{tn_vlan_alt, Tn_vlan_alt},
	{tn_vlan_port_alt, Tn_vlan_port_alt},
	{tn_gw_alt, Tn_gw_alt},
	{tn_mask_alt, Tn_mask_alt}
    ] ++ Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_oap_to_node(Config) ->
    add_oap_to_node(is_oap_configured(Config), Config).
add_oap_to_node(true, _) ->
    ct:pal("ME is already configured OamAccessPoint=1");
add_oap_to_node(false, Config) ->
    create_fru(is_fru_created(Config), Config),
    create_transport(is_transport_created(Config), Config),
    create_oap(Config).

add_alt_oap_to_node(Config) ->
    add_alt_oap_to_node(is_alt_oap_configured(Config), Config).
add_alt_oap_to_node(true, _) ->
    ct:pal("ME is already configured with OamAccessPoint=Alternative");
add_alt_oap_to_node(false, Config) ->
    activate_license(is_license_activated(Config), Config),
    create_fru(is_fru_created(Config), Config),
    create_alt_transport(is_alt_transport_created(Config), Config),
    create_alt_oap(Config).

-define(FEATURE, "CXC4011823").
activate_license(true, _) ->
    ct:pal("ME already have multiple-router-license activated"),
    ok;
activate_license(false, Config) ->
    ct:pal("Activating multiple-router-license"),
    Me_id = ?config(me_id, Config),
    Edit =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
	     {'Lm',
	      [{lmId, [], ["1"]},
		{'FeatureState',
		 [{featureStateId, [], [?FEATURE]},
		  {featureState, [], ["ACTIVATED"]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Edit]),
    ct:pal("Multiple-router-license ACTIVATED"),
    ok.

is_license_activated(Config) ->
    Me_id = ?config(me_id, Config),
    Get =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
	     {'Lm',
	      [{lmId, [], ["1"]},
		{'FeatureState',
		 [{featureStateId, [], [?FEATURE]}]}]}]}]},
    {ok, Conf} = netconf(get, [nc1, Get]),
    Names = ['ManagedElement','SystemFunctions', 'Lm', 'FeatureState',
	     featureState],
    case struct(Conf, Names) of
	[] ->
	    ct:fail("Featurestate not found: ~p", [Conf]);
	[["ACTIVATED"]] ->
	    true;
	[["DEACTIVATED"]] ->
	    false;
	Other ->
	    ct:fail("Unexpected, Conf: ~p~nStruct: ~p", [Conf, Other])
    end.

create_oap(Config) ->
    Tn_vlan_port = ?config(tn_vlan_port, Config),
    Router= ?MODULE_STRING,
    create_oap(Config, Router, Tn_vlan_port, "1", "").

create_alt_oap(Config) ->
    Tn_vlan_port_alt = ?config(tn_vlan_port_alt, Config),
    Router= ?MODULE_STRING ++ "_alt",
    create_oap(Config, Router, Tn_vlan_port_alt, "Alternative", "Alternative").

create_oap(Config, Router, Tn_vlan_port, Oam_AP_id, Oap_type) ->
    Me_id = ?config(me_id, Config),
    Point = "ManagedElement=" ++ Me_id ++ ",Transport=1,Router=" ++
	    Router ++ ",InterfaceIPv4=" ++ Tn_vlan_port ++ 
	    ",AddressIPv4=" ++ Tn_vlan_port,
    Edit = 
	{'ManagedElement',
                [{managedElementId,[],[Me_id]},
                 {'SystemFunctions',[],
                     [{systemFunctionsId,[],["1"]},
                      {'SysM',
                          [{sysMId,[],["1"]},
                           {'OamAccessPoint',[],
                               [{oamAccessPointId,[],[Oam_AP_id]},
                                {accessPoint,[],
                                    [Point]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Edit]),
    ct:pal("~s OamAccessPoint created, pointing to: ~p", [Oap_type, Point]).

delete_alt_oap(Config) ->
    Me_id = ?config(me_id, Config),
    Oam_AP_id = "Alternative",
    Oap_type =  "Alternative",
    Edit =
        {'ManagedElement',
		[{'xmlns:xc', "urn:ietf:params:xml:ns:netconf:base:1.0"}],
                [{managedElementId,[],[Me_id]},
                 {'SystemFunctions',[],
                     [{systemFunctionsId,[],["1"]},
                      {'SysM',
                          [{sysMId,[],["1"]},
                           {'OamAccessPoint',[],
                               [{oamAccessPointId,[],[Oam_AP_id]},
                                %%{accessPoint,[], []}
                                {accessPoint,[{'xc:operation', "delete"}], []}
	 ]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Edit]),
    ct:pal("~s OamAccessPoint deleted", [Oap_type]).



%%%FRU stuff is the same for both OamAP=1 and OamAP=Alternative
create_fru(true, _) ->
    ct:pal("FieldReplaceableUnit is already created");
create_fru(false, Config) ->
    Me_id = ?config(me_id, Config),
    Tn_port = ?config(tn_port, Config),
    Fru_id = ?config(fru, Config),
    Edit =
        {'ManagedElement',
         [{managedElementId,[],[Me_id]},
          {'Equipment',
           [{equipmentId,[],["1"]},
	    {'FieldReplaceableUnit',
	     [{fieldReplaceableUnitId,[],[Fru_id]},
	      {administrativeState,[],["UNLOCKED"]},
	      {'TnPort',
	       [{tnPortId,[],[Tn_port]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Edit]),
    ct:pal("FieldReplaceableUnit=1 with TnPort=~p - created", [Tn_port]).

is_fru_created(Config) ->
true andalso
begin
    Me_id = ?config(me_id, Config),
    Tn_port = ?config(tn_port, Config),
    Get =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'Equipment',
           [{equipmentId,[],["1"]}]}]},
    {ok, Conf} = netconf(get, [nc1, Get]),
    Names = ['ManagedElement','Equipment', 'FieldReplaceableUnit', 'TnPort'],
    case struct(Conf, Names) of
	[] -> false;
	[Equipment] ->
	    case lists:member({tnPortId,[],[Tn_port]}, Equipment) of
		true ->
		    true;
		_ ->
		    ct:pal("Equipment conf: ~p", Conf),
		    ct:pal("Equipment attrs: ~p", [Equipment]),
		    false
	    end
    end
end.

create_transport(Done, Config) ->
    Ip = ?config(tn_ip, Config),
    Vlan = ?config(tn_vlan, Config),
    Tn_vlan_port = ?config(tn_vlan_port, Config),
    Mask = ?config(tn_mask, Config),
    Gw = ?config(tn_gw, Config),
    Router= ?MODULE_STRING,
    create_transport(Done, Config, Ip, Vlan, Tn_vlan_port, Mask, Gw,
		     Router, "").
create_alt_transport(Done, Config) ->
    Ip = ?config(tn_ip_alt, Config),
    Vlan = ?config(tn_vlan_alt, Config),
    Tn_vlan_port = ?config(tn_vlan_port_alt, Config),
    Mask = ?config(tn_mask_alt, Config),
    Gw = ?config(tn_gw_alt, Config),
    Router= ?MODULE_STRING ++ "_alt",
    Fudged_done = case Done of between -> false; _ -> Done end,
    create_transport(Fudged_done, Config, Ip, Vlan, Tn_vlan_port, Mask, Gw,
		     Router, "Alternative").

create_transport(true, _, _, _, _, _, _, _, Oap_type) ->
    ct:pal("~sTransport is already created", [Oap_type]);
create_transport(between, _,_,  _, _, _, _, _, Oap_type) ->
    ct:pal("~sTransport is partly configured - manual intervention required",
	   [Oap_type]),
    exit(duh);
create_transport(false, Config, Ip, Vlan, Tn_vlan_port, Mask, Gw, Router,
		 Oap_type) ->
    ct:pal("Configuring Transport"),
    Me_id = ?config(me_id, Config),
    Tn_port = ?config(tn_port, Config),
    Fru_id = ?config(fru, Config),
    Encap_eth = "ManagedElement=" ++ Me_id ++
		    ",Equipment=1,FieldReplaceableUnit=" ++
		    Fru_id ++ ",TnPort=" ++ Tn_port,
    Encap_vlan = "ManagedElement=" ++ Me_id ++ ",Transport=1,EthernetPort=" ++
		 Tn_port,
    Encap_int = "ManagedElement=" ++ Me_id ++ ",Transport=1,VlanPort=" ++
		    Tn_vlan_port,
    Address = Ip ++ "/" ++ Mask,
    Edit =
        {'ManagedElement',
         [{managedElementId,[],[Me_id]},
	  {'Transport',[],
	   [{transportId,[],["1"]},
	    {'EthernetPort',
	     [{ethernetPortId,[],[Tn_port]},
	      {encapsulation,[], [Encap_eth]},
	      {administrativeState,[],["UNLOCKED"]}]},
	    {'VlanPort',
	     [{vlanPortId,[],[Tn_vlan_port]},
	     {vlanId,[],[integer_to_list(Vlan)]},
	     {encapsulation,[],
		[Encap_vlan]},
	     {isTagged,[],["true"]}]},
	    {'Router',
	     [{routerId,[],[Router]},
	      {'InterfaceIPv4',
	       [{interfaceIPv4Id,[],[Tn_vlan_port]},
		{encapsulation,[], [Encap_int]},
		{mtu,[],["9000"]},
		{'AddressIPv4',[],
		 [{addressIPv4Id,[],[Tn_vlan_port]},
		  {address,[],[Address]}]}]},
	      {'RouteTableIPv4Static',
	       [{routeTableIPv4StaticId,[],["1"]},
		{'Dst',[],
		 [{dst,[],["0.0.0.0/0"]},
		  {dstId,[],["default"]},
		  {'NextHop',[],
		   [{address,[],[Gw]},
		    {nextHopId,[], [Gw]}]}]}]}]}]}]},
    ok = netconf(edit_config, [nc1, running, Edit]),
    ct:pal("~sTransport=1 with TnPort=~p Vlan=~p - created",
	   [Oap_type, Tn_port, Vlan]),
    ct:pal("~sIP/Mask: ~p/~p, Gateway: ~p", [Oap_type, Ip, Mask, Gw]).

is_transport_created(Config) ->
    Router= ?MODULE_STRING,
    is_transport_created(Config, Router, ?config(tn_vlan_port, Config), "").
is_alt_transport_created(Config) ->
    Router= ?MODULE_STRING ++ "_alt",
    is_transport_created(Config, Router, ?config(tn_vlan_port_alt, Config),
			 "Alternative").
is_transport_created(Config, Router, Tn_vlan_port, Oap_type) ->
    Me_id = ?config(me_id, Config),
    Tn_port = ?config(tn_port, Config),
    Get =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'Transport',
           [{transportId,[],["1"]}]}]},
    {ok, Conf} = netconf(get, [nc1, Get]),
    %ct:pal("Transport conf: ~p", Conf),
    Names1 = ['ManagedElement','Transport', 'Router'],
    Names2 = ['ManagedElement','Transport', 'EthernetPort'],
    Names3 = ['ManagedElement','Transport', 'VlanPort'],
    Routers = struct(Conf, Names1),
    Ethers = struct(Conf, Names2),
    Vlans = struct(Conf, Names3),
    Router_ids = [lists:keyfind(routerId, 1, R) || R <- Routers],
    Ether_ids = [lists:keyfind(ethernetPortId, 1, E) || E <- Ethers],
    Vlan_ids = [lists:keyfind(vlanPortId, 1, V) || V <- Vlans],
    REV = {lists:keyfind([Router], 3, Router_ids),
	  lists:keyfind([Tn_port], 3, Ether_ids),
	  lists:keyfind([Tn_vlan_port], 3, Vlan_ids)},
    ct:pal("debug REV: ~p ~n", [REV]),
    case REV of
	{false, false, false} -> %nothing configured go on
	    false;
	REV ->
	    case lists:member(false, tuple_to_list(REV)) of
		true ->
		    {R, E, V} = REV,
		    ct:pal("~s Transport conf: ~p~n"
			   "~s Transport Router: ~p~n"
			   "~s Transport Ether: ~p~n"
			   "~s Transport Vlan: ~p~n",
			   [Oap_type, Conf, Oap_type, Routers,
			    Oap_type, Ethers, Oap_type, Vlans]),
		    R =:= false andalso
			ct:pal("~s Transport has no Router - "
				"erase/fix this manually.~n", [Oap_type]),
		    E =:= false andalso
			ct:pal("~s Transport has no Etherport - "
				"erase/fix this manually.~n", [Oap_type]),
		    V =:= false andalso
			ct:pal("~s Transport has no Vlan - "
				"erase/fix this manually.~n", [Oap_type]),
		    between;
		false ->
		    %configured ok-ish
		    %we skip checking the address and such
		    true
	    end
    end.

is_oap_configured(Config) ->
    is_oap_configured(Config, "1", "").
is_alt_oap_configured(Config) ->
    is_oap_configured(Config, "Alternative", "Alternative").
is_oap_configured(Config, Oam_AP_id, Oap_type) ->
true andalso
begin
    Me_id = ?config(me_id, Config),
    Get =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
            {'SysM', [{xmlns,"urn:com:ericsson:ecim:ComSySM"}],
             [{sysMId,[],["1"]},
              {'OamAccessPoint', [],
               [{oamAccessPointId,[],[Oam_AP_id]}]}]}]}]},
    {ok, Conf} = netconf(get, [nc1, Get]),
    %ct:pal("OamAP conf: ~p", Conf),
    Names = ['ManagedElement','SystemFunctions','SysM','OamAccessPoint'],
    [Oap_attributes] = struct(Conf, Names),
    %ct:pal("Oap_attributes: ~p", [Oap_attributes]),
    case lists:keyfind(accessPoint, 1, Oap_attributes) of
	{accessPoint, [], [Oap_is_here]} ->
	    ct:pal("The ~s OamAccessPoint points to here: ~p",
		   [Oap_type, Oap_is_here]),
	    true;
	false ->
	    false
    end
end.

alt_connect_disconnect() ->
     case ct_netconfc:open(nc_alt, [{user, "muexpert"},
				    {password, "muexpert"}]) of
	{ok, _} ->
            ok = ct_netconfc:close_session(nc_alt),
	    ok;
	Err ->
	    {error, Err}
    end.

alt_netconf(F, A) ->
    {ok, _} = ct_netconfc:open(nc_alt, [{user, "muexpert"},
					{password, "muexpert"}]),
    Res = apply(ct_netconfc, F, A),
    case Res of
        {error, _} ->
            catch ct_netconfc:close_session(nc_alt),
            Res;
        _ ->
            ok = ct_netconfc:close_session(nc_alt)
    end,
    Res.

netconf(F, A) ->
    %%{ok, _} = ct_netconfc:open(nc1, [{user, "expert"}, {password, "expert"}]),
    {ok, _} = ct_netconfc:open(nc1, []),
    Res = apply(ct_netconfc, F, A),
    case Res of
        {error, _} ->
            catch ct_netconfc:close_session(nc1),
            Res;
        _ ->
            ok = ct_netconfc:close_session(nc1)
    end,
    Res.

%descend into the structure following list(Names), when only one Name
%remains collect all matching and return them
struct([{Name, _, Next} | T], [Name]) ->
    [Next | struct(T, [Name])];
struct([{_, _, _} | T], [Name]) ->
    struct(T, [Name]);
struct([], _) -> [];
struct([{Name, _, Next} | _], [Name | Names]) ->
    struct(Next, Names);
struct([{_, _, _} | T], Names) ->
    struct(T, Names).

to_mask(IP) when is_list(IP) ->
    to_mask(inet:parse_address(IP));
to_mask({ok, {A, B, C, D}}) ->
    to_mask(<<A:8, B:8, C:8, D:8>>);
to_mask(Bin) when is_binary(Bin) ->
    integer_to_list(bit_size(<< << F:1>> || <<F:1>> <= Bin, F=:=1 >>)).

check_wait_for_alarm(Config) ->
    Me_id = ?config(me_id, Config),
    check_wait_for_alarm(Me_id, 0).
check_wait_for_alarm(_, 10) ->
    ct:pal("No alarm found after checking/waiting 10 seconds"),
    not_found;
check_wait_for_alarm(Me_id, N) ->
    Get =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
	     {'Fm',
	      [{fmId, [], ["1"]} %,
		%%{'FmAlarm', [],[]} %if no instance at all this fails
		]}]}]},
    {ok, Conf} = netconf(get, [nc1, Get]),
    Names = ['ManagedElement','SystemFunctions', 'Fm', 'FmAlarm'],
    Fm = struct(Conf, Names),
    try have_alarm(Fm) of
	true ->
    	    ct:pal("Alarm found after waiting ~p sec.", [N]),
	    ok;
	false ->
	    timer:sleep(1000),
	    check_wait_for_alarm(Me_id, N + 1)
    catch
	A:B ->
    	    ct:pal("have_alarm crash: ~p:~p~n", [A, B]),
	    timer:sleep(1000),
	    check_wait_for_alarm(Me_id, N + 1)
    end.

have_alarm([H | T]) ->
    case {lists:keyfind(majorType, 1, H),
	  lists:keyfind(minorType, 1, H)} of
	{{_, _, ["193"]}, {_, _, ["9175162"]}} ->
	    true;
	_ ->
	    have_alarm(T)
    end;
have_alarm([]) ->
    false.

check_wait_for_cease(Config) ->
    Me_id = ?config(me_id, Config),
    check_wait_for_cease(Me_id, 0).
check_wait_for_cease(_, 20) ->
    ct:pal("Still alarm after checking/waiting 20 seconds"),
    nok;
check_wait_for_cease(Me_id, N) ->
    Get =
        {'ManagedElement',
         [{xmlns,"urn:com:ericsson:ecim:ComTop"}],
         [{managedElementId,[],[Me_id]},
          {'SystemFunctions',
           [{systemFunctionsId,[],["1"]},
	     {'Fm',
	      [{fmId, [], ["1"]}% ,
		%%%{'FmAlarm', [],[]} %fails if no instance at all
		]}]}]},
    {ok, Conf} = netconf(get, [nc1, Get]),
    Names = ['ManagedElement','SystemFunctions', 'Fm', 'FmAlarm'],
    Fm = struct(Conf, Names),
    try have_alarm(Fm) of
	true ->
	    timer:sleep(1000),
	    check_wait_for_cease(Me_id, N + 1);
	false ->
    	    ct:pal("Alarm ceased after waiting ~p sec.", [N]),
	    ok
    catch
	A:B ->
    	    ct:pal("have_alarm crash: ~p:~p~n", [A, B]),
	    timer:sleep(1000),
	    check_wait_for_cease(Me_id, N + 1)
    end.

%%%make_safe({A, B}) ->
%%%    "{" ++ m_safe(A) ++ "," ++ m_safe(B) ++ "}".

%%%m_safe(C) ->
%%%    re:replace(
%%%	io_lib:format("~p", [C]),
%%%	"error", "boogaloo",
%%%	[global, {return, list}]).

