%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaFirewall.erl %
%%% @author etomist
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R10A/1

%%% @doc ==Firewall configuration for DSCP settings on SNMP PDUs==
%%%
%%% Where is the information we want:

%% table("Snmp") -> snmp;			%agentAddress
%% table("SnmpTargetV1") -> snmpTargetV1; 	%trapreceiver address, port
%% table("SnmpTargetV2C") -> snmpTargetV2C;	%trapreceiver address, port
%% table("SnmpTargetV3") -> snmpTargetV3;	%trapreceiver address, port
%% ntpServer <- back per TR HT43928
%% ldap <- removed, DSCP is set directly on socket by client
%% dns <- removed, TN does this now

%% the config to enable tracestreaming through the OaM interface
%% (and its network-namespace) is also added here
%% this means:
%%		a bridge between namespaces
%%		network on the bridge (IPv4 and IPv6)
%%		a default route (IPv4 and IPv6)
%%		a rule in the NAT table (IPv4 and IPv6)

-module(comsaFirewall).
-behaviour(gen_server).

-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R10A/1').
-date('2017-07-12').
-author(etxlg).

-include("RcsSnmp.hrl").
-include("RcsSysM.hrl").   %get #ntpServer{}
-define(SUDO, "sudo").
-define(IP, "ip").
-define(IPTABLES, "iptables").
-define(IP6TABLES, "ip6tables").
-define(RETRY_INT, 5000).
-define(NTP_PORT, 123).
-define(DNS_PORT, 53).

%%%-compile([export_all]).

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
%%% Rx	       2014-09-11   etxlg     Created
%%% R2A/2      2014-09-11   etxlg     OOT not ready try looping
%%% R2A/3      2014-09-11   etxlg     Do LDAP also
%%% R2A/4      2014-09-18   etxlg     block printing of ERROR REPORT
%%% R2A/5      2014-09-19   etxlg     checkin to enable release
%%% R2A/6      2014-09-19   etxlg     rewrite, new name (was comSnmpFirewall)
%%%					redo as a gen_server
%%% R2A/7      2014-09-22   etxlg     ci by _mistake_, still rewriting
%%% R2A/8      2014-09-23   etxlg     fixed yesterdays ci mistake
%%% R2A/9      2014-09-23   etxlg     filter out early gen_cast
%%% R2A/10     2014-09-26   etxlg     handle all fun-callbacks from oot
%%% R2A/11     2014-10-03   etxlg     ldap removed, done by the client
%%% R2A/12     2014-10-17   etxlg     try to ensure only one reg to OOT and ';'
%%% R2A/13     2014-11-21   etxlg     default rule for DNS
%%% R3A/1      2015-02-10   etxlg     Namespacing
%%% R3A/2      2015-02-11   etxlg     Restored ignore for SIM case
%%% R3A/3      2015-02-13   etxlg     Bug fix for config_update
%%% R3A/4      2015-03-02   etxlg     Disable dbg-printouts
%%% R3A/5      2015-03-10   etxlg     DCSP for NTP back in play, TR HT43928
%%% R3A/6      2015-04-10   etxlg     badarg if no NS, part fix TR HT47659
%%% R4A/2      2015-10-05   etxlg     TR HU20774, trace-streaming
%%% R4A/3      2015-10-15   etxtory   Revert back to R4A/1
%%% R4A/4      2015-10-19   etxlg     TR HU20774, trace-streaming, try again,
%%%				      now with agread TN workaround
%%% R4A/5      2015-11-17   etxlg     TR HU36655, ensure ICMP for PMTU allowed
%%% R4A/6      2016-01-14   etxlg     TR HU50146, use set DSCP not current
%%% R5A/2      2016-02-02   etxlg     IPv6
%%% R5A/3      2016-02-03   etxlg     Clean up pgh when using start_interal_lm
%%% R6A/1      2016-09-06   ekurnik   HU89514, handling NS change when init is not finished
%%% R7A/1      2016-10-08   etxpeno   Do not use deprecated OTP function crypto:rand_bytes/1
%%% R10A/1     2017-07-12   etomist   HV97442
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/0]).
-export([config_update/1]).

%%for debug
-export([stop/0]).
-export([start/0]).
-export([iptable_test/4]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%callback from APPM
-export([iptable_exit/1]).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

start_link() ->
    case sysEnv:target() of
	true ->
	    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []);
	false ->
	    ignore
    end.

%%for testing
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%exported for test and debug
stop() ->
    gen_server:call(?MODULE, stop).
%%exported for test and debug
iptable_test(Family, Table, Args, Ns) ->
    gen_server:call(?MODULE, {iptable_test, Family, Table, Args, Ns}).

%%% EXPORTED INTERFACE FUNCTION
config_update(Prop_list) ->
    dbg("config_update(~p)", [Prop_list]),
    Dscp = proplists:get_value(dscp, Prop_list),
    Ns = proplists:get_value(oap_namespace, Prop_list),
    Ip = proplists:get_value(access_point_address, Prop_list),
    gen_server:cast(?MODULE, {config_update, Dscp, Ns, Ip}).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%oldold init progresses: false, %false|subscribe|tables|output_rules|ready
%%old init progresses: false, %false|subscribe|tables|output_rules|bridge|ready
%%init progresses: start|subscribe|bridge|tables|output_rules|lttng|ready
-record(st,
    {init = start,  %states during init/new config, see above
     dscp,          %current DSCP value
     nns,           %current OAM-namespace (-binary())
     random,        %used to create IPv6 network on bridge
     is_ipv6 = false,
     ip6_veth0,     %constructed from bridge MAC and random
     ip6_veth1,     % --"--
     extra_tables = [],
     output_rules = [],
     ntp_rules = [],
     snmp_rules = [],
     te_rules = []}).

-record(rule, {
        family,
        table,
        chain,
        proto = undefined,
        src = undefined,
        dst = undefined,
        sport = undefined,
        dport = undefined,
        sinterface = undefined,
        dinterface = undefined,
        target = undefined,
        dscp = undefined,
        icmp_type = undefined}).

-define(SNMP_CHAIN, "snmp_dscp").
-define(NTP_CHAIN, "ntp_dscp").
-define(TRACE_CHAIN, "te_stream").
%%curstom target-chains created at start-up
-define(ALL_TABLES,
	[{inet, "mangle", ?SNMP_CHAIN},
	 {inet, "mangle", ?NTP_CHAIN},
	 {inet, "nat", ?TRACE_CHAIN},
	 {inet, "filter", ?TRACE_CHAIN},
	 {inet6, "mangle", ?SNMP_CHAIN},
	 {inet6, "mangle", ?NTP_CHAIN},
	 {inet6, "nat", ?TRACE_CHAIN},
	 {inet6, "filter", ?TRACE_CHAIN}]).
%%these chains are flushed and recreated when config changes
-define(DSCP_DYN_TABLES,
	[{"mangle", ?SNMP_CHAIN},
	 {"mangle", ?NTP_CHAIN},
	 {"filter", ?TRACE_CHAIN},
	 {"nat", ?TRACE_CHAIN}]).
%%fixed rules added to built-in chains diverting packets to custom-chains
%%created at start-up
-define(FIXED_RULES,
	[{inet, "mangle", "OUTPUT", "udp", ?SNMP_CHAIN},
	 {inet, "mangle", "OUTPUT", "udp", ?NTP_CHAIN},
	 {inet, "nat", "POSTROUTING", "tcp", ?TRACE_CHAIN},
	 {inet, "filter", "FORWARD", "tcp", ?TRACE_CHAIN},
	 {inet, "filter", "FORWARD", "icmp", ?TRACE_CHAIN}, %for PMTU
	 {inet6, "mangle", "OUTPUT", "udp", ?SNMP_CHAIN},
	 {inet6, "mangle", "OUTPUT", "udp", ?NTP_CHAIN},
	 {inet6, "nat", "POSTROUTING", "tcp", ?TRACE_CHAIN},
	 {inet6, "filter", "FORWARD", "tcp", ?TRACE_CHAIN},
	 {inet6, "filter", "FORWARD", "icmp", ?TRACE_CHAIN}]). %for PMTU
-define(IP0, "169.254.201.1").
-define(IP1, "169.254.201.2").

-define(NO_DROP_RULES,
	[#rule{family = inet,
	       table = "filter",
	       chain = ?TRACE_CHAIN,
	       proto = undefined,
	       sinterface = "veth1",
	       target = "ACCEPT"},
	 #rule{family = inet,
	       table = "filter",
	       chain = ?TRACE_CHAIN,
	       proto = undefined,
	       dinterface = "veth1",
	       target = "ACCEPT"},
	 #rule{family = inet6,
	       table = "filter",
	       chain = ?TRACE_CHAIN,
	       proto = undefined,
	       sinterface = "veth1",
	       target = "ACCEPT"},
	 #rule{family = inet6,
	       table = "filter",
	       chain = ?TRACE_CHAIN,
	       proto = undefined,
	       dinterface = "veth1",
	       target = "ACCEPT"}]).

%% HV97442
-define(DROP_RULES,
    [#rule{family = inet,
           table = "filter",
           chain = "INPUT",
           proto = "icmp",
           icmp_type = "timestamp-request",
           target = "DROP"},
     #rule{family = inet,
           table = "filter",
           chain = "INPUT",
           proto = "icmp",
           icmp_type = "timestamp-reply",
           target = "DROP"}]).

init([]) ->
    self() ! async_init,
    {ok, #st{}}.

handle_call(stop, _From, S) ->
    {stop, {shutdown, "stopped by command"}, S};
handle_call({iptable_test, Family, Table, Args, Ns}, _From, S) ->
    {reply, iptable(Family, Table, Args, Ns), S};
handle_call(Request, _From, S) ->
    error_msg("Unexpected call: ~p", [Request]),
    {reply, error, S}.

handle_cast({config_update, Dscp, Ns, Ip}, S) ->
    handle_config_update(S, Dscp, Ns, Ip);
handle_cast(Request, S) ->
    error_msg("Unexpected cast: ~p", [Request]),
    {noreply, S}.

%%first time or repeat until OOT is up
handle_info(async_init, #st{init = start} = S) ->
    case register_in_oot() of
	{ok, Dscp, Ns, Is_ipv6} ->
        dbg("Init state: start, NS: ~p", [Ns]),
	    start_subscriptions(),
	    continue_init(S#st{init = subscribe, dscp = Dscp, nns = Ns,
			       is_ipv6 = Is_ipv6});
	nok ->
	    info_msg("OOT not ready - retry in ~p ms", [?RETRY_INT]),
	    erlang:send_after(?RETRY_INT, self(), async_init),
	    {noreply, S}
    end;
%%something went wrong during iptables, keep trying
handle_info(async_init, S) ->
    continue_init(S);
handle_info(Any, #st{init = Init} = S) when Init =/= ready ->
    dbg("Event before ready (~p): ~p", [Init, Any]),
    {noreply, S};
handle_info({mnesia_table_event, {write, Record, _}}, S) ->
    handle_table_event(element(1, Record), S);
handle_info({mnesia_table_event, {delete, Record, _}}, S) ->
    handle_table_event(element(1, Record), S);
handle_info({mnesia_table_event,  Table_event}, S) ->
    dbg("Got unhandled table_event: ~p", [Table_event]),
    {noreply, S};
handle_info(Message, S) ->
    error_msg("Unexpected info: ~p", [Message]),
    {noreply, S}.

code_change(_Old, S, _Extra) ->
    {ok, S}.

terminate(_Reason, _S) ->
    dbg("Terminating with reason: ~p", [_Reason]),
    ok.


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
get_te_rules(S) ->
    [#rule{	family = inet,
		table = "nat",
		chain = ?TRACE_CHAIN,
		proto = "tcp",
		src = ?IP0 ++ "/32",
		dst = "0.0.0.0/0",
		sport = undefined,
		dport = undefined,
		target = "MASQUERADE",
		dscp = undefined},
     #rule{family = inet6,
		table = "nat",
		chain = ?TRACE_CHAIN,
		proto = "tcp",
		src = S#st.ip6_veth0 ++ "/128",
		dst = "::/0",
		sport = undefined,
		dport = undefined,
		target = "MASQUERADE",
		dscp = undefined}].

%%we got config update while still trying to create iptables template
%%just update the data, iptables will be created later
handle_config_update(#st{init = Init, nns = Old_ns} = S, Dscp, Ns, Ip)
	when Init =/= ready ->
    {_Is_changed, New_s} = update_state_variable(S, Dscp, Ns, Ip),
    %% HU89514, handling NS change, restart init procedure
    if Old_ns =/= Ns ->
        info_msg(
        "config_update, before ready, NS change, reconfig, Ns: ~p, Dscp: ~p, Ip: ~p",
        [Ns, Dscp, Ip]),
        {noreply, New_s#st{init = start}};
    true ->
        info_msg("config_update, before ready, no change, Ns: ~p, Dscp: ~p, Ip: ~p",
             [Ns, Dscp, Ip]),
        {noreply, New_s}
    end;

%%"normal" config_update, to simplify, if anything changed
%%(Namespace, Dscp, or IPv4/Ipv6) just clean up all firewall rules
%%and start over from almost scratch.
%%If namespace changes, start over even closer to scratch
handle_config_update(#st{nns = Old_ns} = Current_s,
		     Dscp, Ns, Ip) ->
    {Is_changed, New_s} = update_state_variable(Current_s, Dscp, Ns, Ip),
    if
	not Is_changed ->
	    info_msg("config_update, no change, Ns: ~p, Dscp: ~p, Ip: ~p",
		     [Ns, Dscp, Ip]),
	    {noreply, Current_s};
	Ns =/= Old_ns ->
	    info_msg(
		"config_update, NS change, reconfig, Ns: ~p, Dscp: ~p, Ip: ~p",
		[Ns, Dscp, Ip]),
	    Next_s = get_input_config(New_s),
	    del_bridge(),
	    [silent_flush_table(Tab, Chain, Old_ns) ||
		{Tab, Chain} <- ?DSCP_DYN_TABLES],
	    continue_init(Next_s#st{init = subscribe});
	true ->
	    info_msg(
		"config_update, erase and recreate, Ns: ~p, Dscp: ~p, Ip: ~p",
		[Ns, Dscp, Ip]),
	    Next_s = get_input_config(New_s),
	    del_bridge(),
	    [silent_flush_table(Tab, Chain, Old_ns) ||
		{Tab, Chain} <- ?DSCP_DYN_TABLES],
	    continue_init(Next_s#st{init = bridge})
    end.

del_bridge() ->
    %this will cause the route to go as well
    dbg("Deleting bridge", []),
    Delete = ["ip", "link", "del", "veth0"],
    cmd("ip link del", Delete).

add_bridge() ->
    dbg("Creating bridge", []),
    Cmd = ["ip", "link", "add", "veth0", "type", "veth",
	   "peer", "name", "veth1"],
    ok = cmd("ip link add veth0 <-> veth1", Cmd).

activate_lttng_workaround(_, <<>>) ->
    dbg("No NS, the bridge will not be activated", []),
    ok;
activate_lttng_workaround(S, Ns) ->
    dbg("Activating bridge to NS: ~p", [Ns]),
    Ip60 = S#st.ip6_veth0,
    Ip61 = S#st.ip6_veth1,
    try begin
	Cmd2 = ["ip", "link", "set", "veth1", "netns", Ns],
	ok = cmd("ip netns link set veth1", Cmd2),
	%for some reason this is needed _after_ moving veth1 to ns
	enable_ipv6([{<<>>, "veth0"}, {Ns, "veth1"}]),
	Cmd3 = ["ip", "netns", "exec", Ns,
		"ip", "address", "add", ?IP1 ++ "/24", "dev", "veth1"],
	ok = cmd("ip netns address veth1", Cmd3),
	Cmd36 = ["ip", "netns", "exec", Ns,
		"ip", "-6", "address", "add", Ip61 ++ "/64", "dev", "veth1"],
	ok = cmd("ip6 netns address ip6 veth1", Cmd36),
	Cmd4 = ["ip", "address", "add", ?IP0 ++ "/24", "dev", "veth0"],
	ok = cmd("ip address add veth0", Cmd4),
	Cmd46 = ["ip", "-6", "address", "add", Ip60 ++ "/64", "dev", "veth0"],
	ok = cmd("ip6 address add ip6 veth0", Cmd46),
	Cmd5 = ["ip", "link", "set", "veth0", "up"],
	ok = cmd("ip link set veth0 up", Cmd5),
	Cmd5b = ["ip", "netns", "exec", Ns, "ip", "link", "set", "veth1", "up"],
	ok = cmd("ip netns link set veth1 up", Cmd5b),
	Cmd6 = ["ip", "route", "add", "to", "default", "via", ?IP1],
	ok = cmd("ip route add default", Cmd6),
	Cmd66 = ["ip", "-6", "route", "add", "to", "default", "via", Ip61],
        ok = cmd("ip6 route add default", Cmd66),
        ok
	end
     of
	ok ->
	    ok
    catch
	A:B ->
	    %HERE need cleaning for IPv6?
	    dbg("Failure activating the bridge: ~p:~p~n~p",
		[A, B, erlang:get_stacktrace()]),
	    dbg("Deleting bridge to NS: ~p", [Ns]),
	    %this is workaround to remove any default put there by lab-adaption
	    No_default = ["ip", "route", "delete", "default"],
	    cmd("ip route del default", No_default),
	    del_bridge(),
	    add_bridge(),
	    nok
    end.

enable_ipv6(Devices) ->
    %find_private_binary should be typed to accept any() in 3:rd argument
    %and thus also return any()
    Program = sysEnv:find_private_binary(sys, "sys_sysctl", "#undef"),
    enable_ipv6(Devices, Program).

enable_ipv6(_ , "#undef") ->
    error_msg("comsa_sysctl not found, bridge will not be IPv6 enabled");
enable_ipv6(Devices, Program) ->
    [begin
	Fpath = "net/ipv6/conf/" ++ Vdev ++ "/disable_ipv6",
	case appmServer:start_internal_lm([{name, "comsa_sysctl"},
					   {args, [Program, Fpath, "0"]},
					   {ns, Ns},
					   {ugid, {0, 0}},
					   {owner, self()},
					   {autoclean, true}]) of
	    {ok, _} ->
		ok;
	    Err ->
		error_msg("Appm failed to execute comsa_sysctl: ~p", [Err])
	end
     end || {Ns, Vdev} <- Devices].

update_state_variable(#st{dscp = Old_dscp,
			   nns = Old_ns,
			   is_ipv6 = Old_is_ipv6} = S,
		      New_dscp, New_ns, Ip) ->
    Dscp =
	case New_dscp of
	    undefined -> Old_dscp;
	    New_dscp -> New_dscp
	end,
    Ns =
	case New_ns of
	    undefined -> Old_ns;
	    New_ns -> New_ns
	end,
    Is_ipv6 =
	case Ip of
	    undefined -> Old_is_ipv6;
	    Ip -> get_ip_version(Ip)
	end,
    Is_changed = Dscp =/= Old_dscp orelse
		 Ns =/= Old_ns orelse
		 Is_ipv6 =/= Old_is_ipv6 ,
    {Is_changed, S#st{dscp = Dscp, nns = Ns, is_ipv6 = Is_ipv6}}.

%% new strategy: ensure that it works to register, get Dscp, and Namespace
%% before returning ok. Hopefully this avoids to much reconfiguration during
%% start up
%% nok | {ok, Dscp, Namespace}
register_in_oot() ->
    register_in_oot(ootI:get_oap_namespace()).

register_in_oot({error, _}) ->
    nok;
register_in_oot({ok, _}) ->
    %we got the namespace - normally this means that all the other stuff in
    %OOT is available, read the namespace again after registration to avoid
    %race
    try
	begin
	    ok =  ootI:register_cfg_upd_cb(fun ?MODULE:config_update/1),
	    {ok, Ns} = ootI:get_oap_namespace(),
	    Dscp = ootI:get_oap_dscp(),
	    true = is_integer(Dscp),
	    Is_ipv6 = get_ip_version(ootI:get_oap_ip_addr()),
	    {ok, Dscp, Ns, Is_ipv6}
	end
    of
	Tuple -> Tuple
    catch
	_:_ -> nok
    end.

get_ip_version([]) ->
    false; %corresponding to OamAP not set
get_ip_version(Ip_string) ->
    case inet:parse_address(Ip_string) of
	{ok, Tuple} when size(Tuple) =:= 4 ->
	    false;
	{ok, Tuple} when size(Tuple) =:= 8 ->
	    true;
	_ ->
	    error_msg("Cannot make sense of OamIP: ~p - defaulting to IPv4",
		      [Ip_string]),
	    false
    end.

start_subscriptions() ->
    mnesia:subscribe({table, ntpServer, simple}),
    mnesia:subscribe({table, snmp, simple}), %ensure DSCP for agent is set
    % and DSCP for all possible trap receivers (not Dtls - yet)
    mnesia:subscribe({table, snmpTargetV1, simple}),
    mnesia:subscribe({table, snmpTargetV2C, simple}),
    mnesia:subscribe({table, snmpTargetV3, simple}).

%%subsription in place (tables and dscp/ns from OOT), create the tables we use
continue_init(#st{init = subscribe, nns = Ns} = S) ->
    dbg("Init state: subscribe, NS: ~p", [Ns]),
    Extra_tables =
	[create_extra_tables(Family, Tab, Chain, Ns) ||
	 {Family, Tab, Chain} <- ?ALL_TABLES],
    if
	Extra_tables =:= ?ALL_TABLES ->
	    continue_init(S#st{init = bridge, extra_tables = Extra_tables});
	true ->
	    info_msg("Failed to create extra tables - retry in ~b ms",
			[?RETRY_INT]),
	    erlang:send_after(?RETRY_INT, self(), async_init),
	    {noreply, S#st{extra_tables = Extra_tables}}
    end;
%%intermediate, create the bridge here so that we can extract MAC and use
%%in IPv6 address generation, keep the addresses and reuse "forever"
continue_init(#st{init = bridge, random = Random, nns = Ns} = S) ->
    dbg("Init state: bridge, NS: ~p", [Ns]),
    try add_bridge() of
	ok ->
	    case Random of
		undefined ->
		    New_rand = crypto:strong_rand_bytes(5),
		    continue_init(
			S#st{init = tables,
			     random = New_rand,
			     ip6_veth0 = get_eui_address(New_rand, "veth0"),
			     ip6_veth1 = get_eui_address(New_rand, "veth1")});
		Random ->
		    continue_init(
			S#st{init = tables})
	    end
    catch
	A:B ->
	    dbg("Failure adding bridge: ~p:~p~n~p",
		[A, B, erlang:get_stacktrace()]),
	    dbg("Deleting bridge to try again", []),
	    Delete = ["ip", "link", "del", "veth0"],
	    cmd("ip link del", Delete),
	    erlang:send_after(?RETRY_INT, self(), async_init),
	    {noreply, S}
    end;
%%tables in place, create the permanent rules to each table, and read
%%config from all the places that are to have rules
continue_init(#st{init = tables, nns = Ns} = S) ->
    dbg("Init state: tables, NS: ~p", [Ns]),
    Output_rules = [create_fixed_rules(R, Ns) || R <- ?FIXED_RULES],
    if
	Output_rules =:= ?FIXED_RULES ->
	    New_s = get_input_config(S),
	    continue_init(New_s#st{output_rules = Output_rules,
				    init = output_rules});
	true ->
	    info_msg("Failed to create output rules - retry in ~p ms",
			[?RETRY_INT]),
	    erlang:send_after(?RETRY_INT, self(), async_init),
	    {noreply, S#st{output_rules = Output_rules}}
    end;
%%all the "create once" stuff is in place set up firewall rules...
continue_init(#st{init = output_rules, nns = Ns} = S) ->
    dbg("Init state: output_rules, NS: ~p", [Ns]),
    add_rules(all_rules(S), Ns),
    continue_init(S#st{init = lttng});
continue_init(#st{init = lttng, nns = Ns} = S) ->
    dbg("Init state: lttng, NS: ~p", [Ns]),
    case activate_lttng_workaround(S, Ns) of
	ok ->
	    dbg("continue_init/1 - reached state: ready!", []),
	    {noreply, S#st{init = ready}};
	nok ->
	    erlang:send_after(?RETRY_INT, self(), async_init),
	    {noreply, S}
    end.

create_extra_tables(Family, Table, Chain, Ns) ->
    case iptable(Family, Table, ["-L", Chain], Ns) of
	ok ->
	    flush_table(Family, Table, Chain, Ns),
	    {Family, Table, Chain};
	_ ->
	    iptable(Family, Table, ["-N", Chain], Ns),
	    case iptable(Family, Table, ["-L", Chain], Ns) of
		ok ->
		    {Family,Table, Chain};
		_ ->
		    nok
	    end
    end.

create_fixed_rules({Family, T, C, P, J}, Ns) ->
    case iptable(Family, T, ["-C", C, "-p", P, "-j", J], Ns) of
	ok ->
	    {Family, T, C, P, J};
	_ ->
	    iptable(Family, T, ["-I", C, "-p", P, "-j", J], Ns),
    	    case iptable(Family, T, ["-C", C, "-p", P, "-j", J], Ns) of
	    ok ->
		{Family, T, C, P, J};
	    _ ->
		nok
	end
    end.

handle_table_event(ntpServer, #st{dscp = Dscp,
				  nns = Ns,
				  ntp_rules = Ntp_rules} = S) ->
    New_ntp_rules =  get_ntp_rules(Dscp),
    if
	New_ntp_rules =:= Ntp_rules ->
	    dbg("Ntp table event - no change in rules"),
	    {noreply, S};
	true ->
	    silent_flush_table("mangle", ?NTP_CHAIN, Ns),
	    add_rules(New_ntp_rules, Ns),
	    {noreply, S#st{ntp_rules = New_ntp_rules}}
    end;
handle_table_event(snmp, S) ->
    handle_snmp_table_event(S);
handle_table_event(snmpTargetV1, S) ->
    handle_snmp_table_event(S);
handle_table_event(snmpTargetV2C, S) ->
    handle_snmp_table_event(S);
handle_table_event(snmpTargetV3, S) ->
    handle_snmp_table_event(S);
handle_table_event(Event, S) ->
    error_msg("Unexpected table event: ~p", [Event]),
    {noreply, S}.

handle_snmp_table_event(#st{dscp = Dscp,
			    nns = Ns,
			    snmp_rules = Snmp_rules,
			    is_ipv6 = Is_ipv6} = S) ->
    New_snmp_rules =  get_snmp_rules(Is_ipv6, Dscp),
    if
	New_snmp_rules =:= Snmp_rules ->
	    dbg("Snmp table event - no change in rules"),
	    {noreply, S};
	true ->
	    silent_flush_table("mangle", ?SNMP_CHAIN, Ns),
	    add_rules(New_snmp_rules, Ns),
	    {noreply, S#st{snmp_rules = New_snmp_rules}}
    end.

all_rules(S) ->
    S#st.ntp_rules ++
    S#st.snmp_rules ++
    S#st.te_rules ++
    ?DROP_RULES.

add_rules([], _) -> ok;
add_rules([R | Rs], Ns) ->
    case rule_to_list(R) of
	Rls when is_list(Rls) ->
	    dbg("New rule [family: ~p, ns: ~p]: ~p",
		[R#rule.family, binary_to_list(Ns), Rls]),
	    case iptable(R#rule.family, Rls, Ns) of
		ok ->
		    ok;
		Badness ->
		    error_msg("Failure adding: ~p got: ~p", [Rls, Badness])
	    end;
	_ ->
	    error_msg("Failure creating rule from: ~p", [R])
    end,
    add_rules(Rs, Ns).

rule_to_list(#rule{} = R) ->
    case R#rule.table of
        undefined -> [];
        Tab -> [ "-t",  Tab]
    end ++
    ["-A", R#rule.chain] ++
    case R#rule.proto of
        undefined -> [];
        P -> [ "-p",  P]
    end ++
    case R#rule.src of
        undefined -> [];
        S -> [ "-s",  S]
    end ++
    case R#rule.dst of
        undefined -> [];
        D -> [ "-d",  D]
    end ++
    case R#rule.sport of
        undefined -> [];
        Sp -> [ "--sport",  integer_to_list(Sp)]
    end ++
    case R#rule.dport of
        undefined -> [];
        Dp -> [ "--dport",  integer_to_list(Dp)]
    end ++
    case R#rule.sinterface of
        undefined -> [];
        Si -> [ "-i",  Si]
    end ++
    case R#rule.dinterface of
        undefined -> [];
        Di -> [ "-o",  Di]
    end ++
    case R#rule.target of
        undefined -> [];
        T -> [ "-j",  T]
    end ++
    case R#rule.dscp of
        undefined -> [];
        Dscp -> [ "--set-dscp",  integer_to_list(Dscp)]
    end ++
    case R#rule.icmp_type of
        undefined -> [];
        Type -> [ "--icmp-type",  Type]
    end;
rule_to_list(Not_rule) ->
    {error, Not_rule}.

silent_flush_table(Table, Chain, Ns) ->
    %flush both v6 and v4 - simplifies the code
    dbg("silent_flush_table inet/inet6, ns: ~p, table: ~p, Chain: ~p",
	[Ns, Table, Chain]),
    iptable(inet, Table, ["-F", Chain], Ns),
    iptable(inet6, Table, ["-F", Chain], Ns).

flush_table(Family, Table, Chain, Ns) ->
    case iptable(Family, Table, ["-F", Chain], Ns) of
	ok ->
	    dbg("Flushed table/chain: ~p/~p/~p", [Family, Table, Chain]),
	    ok;
	Badness ->
	    error_msg("Failure flushing chain: ~p/~p/~p got: ~p",
		      [Family, Table, Chain, Badness])
    end.

cmd(Name, Args) ->
    cmd(Name, os:find_executable(?SUDO),  Args).

cmd(_, false, _) ->
    nok;
cmd(Name, Cmd, Args) ->
    dbg("Cmd: ~p, Args: ~p", [Cmd, Args]),
    Port = open_port({spawn_executable, Cmd},
			[{args, Args},
			 stream, binary, exit_status]),
    case read_from_port(Name, Port, <<>>) of
        {ok, _} ->
            ok;
        {error, Duh} ->
	    info_msg("Cmd failure[~p]: ~p ~p -> ~p", [Name, Cmd, Args, Duh]),
            nok
    end.

%%iptable(Family, Args, Ns) ->
%%    iptable(Family, os:find_executable(?SUDO),  Args, Ns, silent).
%%iptable(Family, Table, Args, Ns) ->
%%    Complete_args = ["-t", Table | Args],
%%    iptable(Family, os:find_executable(?SUDO), Complete_args, Ns, silent).

%% returns ok|nok
%%iptable(_, false, _, _, _) ->
%%    nok;
%%iptable(inet6, _Cmd, Args, <<>>, _Verbosity) ->
%%this is temporary workaround for ip6tables NOT yet in sudoers
%%    info_msg("ip6tables not available fake return ok for cmd: ~p",
%%	[Args]),
%%    ok;
%%iptable(Family, Cmd, Args, Ns, _Verbosity) ->
%%    Port = open_iptable_port(Family, Cmd, Args, Ns),
%%    case read_from_port(family_to_iptable_cmd(Family), Port, <<>>) of
%%        {ok, _} ->
%%            ok;
%%        {error, Error} ->
%%	    dbg("iptable, ns: ~p, cmd: ~p, returned: ~p", [Ns, Args, Error]),
%%            nok
%%    end.

%%open_iptable_port(Family, Cmd, Args, Ns) ->
%%    Iptables = family_to_iptable_cmd(Family),
%%    Full_args =
%%	case Ns of
%%	    <<>> -> %no ns
%%		[Iptables | Args];
%%	    Ns ->
%%		[?IP, "netns", "exec", binary_to_list(Ns),
%%		 Iptables | Args]
%%	end,
 %%   open_port({spawn_executable, Cmd},
%%	      [{args, Full_args},
%%	       stream, binary, exit_status]).

%% return {ok, binary()} | {error, string() | timeout}
read_from_port(Name, Port, Sofar) when is_port(Port)->
    receive
        {Port,{data, Data}} ->
            read_from_port(Name, Port, <<Sofar/binary, Data/binary>>);
        {Port,{exit_status, Status}} when Status =:= 0 ->
            {ok, Sofar};
        {Port,{exit_status, Status}}  ->
            {error, Name ++ ": " ++ binary_to_list(Sofar) ++ " RC: " ++
		    integer_to_list(Status)}
    after 2000 ->
        catch port_close(Port),
        {error, timeout}
    end.

get_input_config(#st{dscp = Dscp, nns = Ns, is_ipv6 = Is_ipv6} = S) ->
    S#st{ntp_rules = try get_ntp_rules(Dscp) catch _:_ -> [] end,
	 snmp_rules = try get_agent_rules(Is_ipv6, Dscp) catch _:_ -> [] end ++
			try get_target_rules(Dscp) catch _:_ -> [] end,
	 te_rules = case Ns of
			<<>> -> [];
			_ -> get_te_rules(S) ++ ?NO_DROP_RULES
		    end}.

family_to_iptable_cmd(inet) -> "sys_iptables";
family_to_iptable_cmd(inet6) -> "sys_ip6tables".
%%family_to_iptable_cmd(inet) -> ?IPTABLES;
%%family_to_iptable_cmd(inet6) -> ?IP6TABLES.

ip_to_family(Ip_string) ->
    case inet:parse_address(Ip_string) of
	{ok, Tuple} when size(Tuple) =:= 4 -> inet;
	{ok, Tuple} when size(Tuple) =:= 8 -> inet6;
	Err ->
	    error_msg(
		"Failed to convert IP adddress to family. In: ~p, Out: ~p",
		[Ip_string, Err]),
	    inet
    end.

get_ntp_rules(Dscp) ->
    Wild = mnesia:table_info(ntpServer, wild_pattern),
    Trans = fun () -> mnesia:match_object(Wild) end,
    case mnesia:transaction(Trans) of
	{atomic, Obj} ->
	    [#rule{family = ip_to_family(A),
		   table = "mangle",
		   chain = ?NTP_CHAIN,
		   proto = "udp",
		   dst = A,
		   sport = ?NTP_PORT,
		   dport = ?NTP_PORT,
		   target = "DSCP",
		   dscp = Dscp} || #ntpServer{serverAddress = A} <- Obj];
	_ -> []
    end.

get_snmp_rules(Is_ipv6, Dscp) ->
    get_agent_rules(Is_ipv6, Dscp) ++ get_target_rules(Dscp).

get_agent_rules(Is_ipv6, Dscp) ->
    Family = if Is_ipv6 -> inet6; true -> inet end,
    Wild = mnesia:table_info(snmp, wild_pattern),
    Trans = fun () -> mnesia:match_object(Wild) end,
    case mnesia:transaction(Trans) of
	{atomic, Obj} ->
	    Agent_adds = [A || #snmp{agentAddress = A} <- Obj],
	    [#rule{family = Family,
		   table = "mangle",
		   chain = ?SNMP_CHAIN,
		   proto = "udp",
		   sport = P,
		   target = "DSCP",
		   dscp = Dscp} || {'HostAndPort', _I, P} <-
					lists:flatten(Agent_adds)];
	_ -> []
    end.

get_target_rules(Dscp) ->
    All_targets = get_targets_v1() ++ get_targets_v2c() ++ get_targets_v3(),
    [#rule{family = ip_to_family(I),
	   table = "mangle",
	   chain = ?SNMP_CHAIN,
	   proto = "udp",
	   dst = I,
	   dport = P,
	   target = "DSCP",
	   dscp = Dscp} || {I, P} <- All_targets].

get_targets_v1() ->
    Wild = mnesia:table_info(snmpTargetV1, wild_pattern),
    Trans = fun () -> mnesia:match_object(Wild) end,
    case mnesia:transaction(Trans) of
        {atomic, Obj} ->
            [{I, P} || #snmpTargetV1{address = I, port = P} <- Obj];
        _ -> []
    end.

get_targets_v2c() ->
    Wild = mnesia:table_info(snmpTargetV2C, wild_pattern),
    Trans = fun () -> mnesia:match_object(Wild) end,
    case mnesia:transaction(Trans) of
	{atomic, Obj} ->
	    [{I, P} || #snmpTargetV2C{address = I, port = P} <- Obj];
	_ -> []
    end.

get_targets_v3() ->
    Wild = mnesia:table_info(snmpTargetV3, wild_pattern),
    Trans = fun () -> mnesia:match_object(Wild) end,
    case mnesia:transaction(Trans) of
        {atomic, Obj} ->
            [{I, P} || #snmpTargetV3{address = I, port = P} <- Obj];
        _ -> []
    end.

get_eui_address(Random, Interface) ->
    <<R1:8, R2:8, R3:8, R4:8, R5:8>> = Random,
    {ok, Is} = inet:getifaddrs(),
    {Interface, Addresses} = lists:keyfind(Interface, 1, Is),
    {hwaddr, [A,B,C,D,E,F]} = lists:keyfind(hwaddr, 1, Addresses),
    %veth hw addresses are (it seems) already created as "locally administered",
    %i.e. U/L-bit is set, thus it should NOT be inverted when used in EUI-64.
    %I think?/lg
    %<<A1:6, A2:1, A3:1>> = <<A:8>>,
    %<<AA:8>> = <<A1:6, (bnot A2):1, A3:1>>,
    AA = A,
    lists:flatten(
	io_lib:format("~.16b~2.16.0b:~.16b~2.16.0b:~.16b~2.16.0b::"
		      "~.16b~2.16.0b:~.16b~2.16.0b:~.16b~2.16.0b:~.16b~2.16.0b",
		      [16#fd, R1, R2, R3, R4, R5, AA, B, C, 16#ff, 16#fe,
		       D, E, F])).

error_msg(Fmt) ->
    error_msg(Fmt, []).

error_msg(Fmt, Args) ->
    sysInitI:warning_msg("~p: " ++ Fmt ++ "~n", [?MODULE | Args]).

info_msg(Fmt, Args) ->
    sysInitI:info_msg("~p: " ++ Fmt ++ "~n", [?MODULE | Args]).

dbg(_) -> ok.
dbg(_, _) -> ok.
%dbg(Fmt) ->
%    dbg(Fmt, []).

%dbg(Fmt, Args) ->
%    io:format("~p: dbg: " ++ Fmt ++ "~n", [?MODULE | Args]).

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
%%following is new implementation of iptables/ip6tables that avoids "sudo"
iptable(Family, Table, Args, Ns) ->
    Complete_args = ["-t", Table | Args],
    iptable(Family, Complete_args, Ns).
iptable(Family, Args, Ns) ->
    Iptables = family_to_iptable_cmd(Family),
    Program = sysEnv:find_private_executable(sys, Iptables, undefined),
    iptab(Program, Args, Ns).

iptab(undefined, _, _) ->
   nok;
iptab(Program, Args, Ns) ->
    Resultfile = "/tmp/comsa_iptables_result." ++ os:getpid(),
    Ref = make_ref(),
    try appmServer:start_internal_lm([{name, "comsa running iptables"},
                                      {mfa, {?MODULE, iptable_exit,[Ref]}},
                                      {args, [Program, Resultfile | Args]},
                                      {ns, Ns},
                                      {ugid, {0, 0}},
                                      {owner, self()},
                                      {autoclean, true}]) of
        {ok, Appid} ->
            dbg("appm_spawn: ~p", [Appid]),
            wait_for_iptables_result(Appid, Ref, Resultfile)
    catch
    A:B ->
        dbg("appm_spawn: ~p:~p", [A, B]),
        nok
    end.

iptable_exit([Appid, Ref|_]) ->
    dbg("iptable_exit, Id: ~p, Ref: ~p", [Appid, Ref]),
    ?MODULE ! {iptable_exit, {Appid, Ref}}.

wait_for_iptables_result(Appid, Ref, Resultfile) ->
    receive
	{iptable_exit, {Appid, Ref}} ->
	    return_result(Resultfile);
	{iptable_exit, Wrong} ->
	    error_msg("Unexpected return from APPM. Expected: ~p, Got: ~p",
		      [{iptable_exit, {Appid, Ref}}, {iptable_exit, Wrong}]),
	    nok
    after
	3000 ->
	    error_msg("Timeout waiting for APPM/iptables."),
	    catch appmServer:stop_internal_lm(Appid),
	    nok
    end.

return_result(Resultfile) ->
    case file:read_file(Resultfile) of
	{ok, Binary} ->
	    case string:to_integer(binary_to_list(Binary)) of
		{0, _} ->
		    ok;
		{Ret, _} when is_integer(Ret)->
		    nok;
		{error, _Err} ->
		    error_msg("Invalid data in return file from iptables: ~p",
			      [Binary]),
		    nok
	    end;
	Err ->
	    error_msg("No result file from iptables: ~p", [Err]),
	    nok
    end.
