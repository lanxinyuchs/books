%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ootServer.erl %
%%% @author eolaand
%%%
%%% @doc ==Main server for OamAccessPoint==
%%% Manages OamAccessPoint MO

-module(ootServer).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R10A/R11A/9').
-date('2017-10-06').
-author('eolaand').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
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
%%% R3A/1      2014-11-10 etxlg       New interface for OaM NET_NS
%%% R3A/3      2014-11-12 etxlg       Slight change to CTN
%%% R3A/4      2014-12-08 etxlg       Wait for IMM after upgrade
%%% R3A/5      2014-12-10 etxlg       Keep track of LMT DHCP IP-address
%%% R3A/6      2014-12-11 etxlg       Convert IMM-DN to MIM-DN
%%% R3A/7      2014-12-17 etxlg       Null terminate the MIM-DN
%%% R3A/8      2014-12-19 etxlg       Workaround, poll IMM after upgrade
%%% R3A/9      2015-01-27 etxlg       Filter out DHCP when no change
%%%				      notice that OapDN changed (TR HT42199)
%%% R3A/10     2015-02-03 etxlg       Better error on fun-crash
%%% R3A/11     2015-02-03 etxlg       Write Oam-amespace to file
%%% R3A/12     2015-02-11 etxlg       Get and store the mask from dhcp on LMT
%%% R3A/13     2015-02-11 etxlg       Give LMT namespace to SNMP if no OaMAP
%%% R3A/14     2015-05-28 etxtory     Create Link Local IP in init/1
%%% R4A/1      2015-07-10 etxjotj     Calling ICTI for upgrade
%%% R4A/3      2015-08-13 etxtory     Netmask for ll 24 -> 16
%%% R4A/4      2015-08-24 eolaand     Added stub for COMSA notifications
%%% R4A/5      2015-08-26 eolaand     Added fcns for alt IP address
%%% R4A/6      2015-08-27 etxlg       Warm restart stuff
%%% R4A/8      2015-09-02 eolaand     Add fcns to store and get COMSA tx data
%%% R4A/9      2015-09-07 eolaand     Handle update of alt OamAccessPoint
%%% R4A/12     2015-09-08 etxlg       Warm restart revisited
%%% R4A/13     2015-09-08 etxlg       Make it work also without OamAccessPoint
%%% R4A/17     2015-09-15 eolaand     Wait for objects at upgrade
%%% R4A/18     2015-09-18 eolaand     Add slave process to set_oap_attrs
%%% R4A/19     2015-09-23 eolaand     Add transform clause for upgrade to 2.1.0
%%% R4A/20     2015-10-01 eolaand     Add transform clause for upgrade to 2.2.0
%%%                                   Rename OamAccessPoint=2 to Alternative
%%% R4A/21     2015-10-12 etxlg       namespace/cstn moved out
%%% R4A/22     2015-10-14 etxlg       Botched the notification
%%% R4A/23     2015-10-20 etxlg       Removed deadlock (with ootCstns-erver)
%%% R4A/24     2015-11-20 eolaand     Move upgrade functions to ootImm.
%%%                                   Fix bug with release of ao for IP addr.
%%% R4A/25     2015-12-01 etxlg       Allow unset of IPv4LMT. Needs testing!
%%% R5A/1      2015-12-04 eolaand     IPv6 updates
%%% R6A/1      2016-05-12 etxarnu     Removed RCSEE/WR6 check
%%% R6A/2      2016-08-30 uabesvi     vRC does not have lmt dhcp, read the IP
%%%                                   adress from eth0
%%% R8A/1      2016-12-16 etxpeno     Using official GMF interface
%%% R9A/2      2017-01-31 eolaand     Support CSTN vsn 2
%%% R9A/3      2017-02-01 uabesvi     Added logging
%%% R9A/7      2017-02-06 eolaand     Do not remove ccb attrs in get
%%% R9A/8      2017-02-07 eolaand     Handle removal of accessPoint also for
%%%                                   CSTN vsn 2
%%% R10A/2     2017-04-25 eolaand     Remove OAP=Alternative in vrcs env
%%% R10A/3     2017-05-23 eolaand     Remove OAP=1 in R-VNFM env, read from
%%%                                   net_mapping.json file.
%%% R10/4-5    2017-06-01 etxpeno     Add retries in read_net_mapping_file/1
%%%                                   until a usable IP address is found
%%% R10A/6     2017-06-14 ecaiyan     Added support for CSTN vsn 3
%%% R10A/7     2017-06-16 eolaand     Get ports and dscp from COMSA, not IMM
%%% R10A/8     2017-06-19 eolaand     Add call to fetch IPv4 address
%%% R10A/9     2017-06-22 ecaiyan     Adapt code to test initialize3
%%% R10A/9     2017-07-11 egabing     Removed dependency towards TN IMM objects
%%% R11A/2     2017-08-02 eolaand     Create log also at upgrade
%%% R11A/4     2017-09-12 eolaand     Notify NS update only when changed
%%% R11A/5     2017-09-14 eolaand     Return empty namespace in test mode
%%% R11A/6     2017-09-15 eolaand     Return empty OAP IP address in test mode
%%% R11A/9     2017-10-06 eolaand     Set default NS for R-VNFM also when eth0
%%%                                   is used for oam.
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([start_link/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-export([get_cached_config/0,
	 store_ccb_attrs/2,
	 get_ccb_attrs/1,
	 del_ccb_attrs/1]).

%% -export([store_tx_objects/2,
%% 	 get_tx_objects/1]).

-export([config_update/0,
	 config_update/1]).

-export([set_oap_attrs/2]).

%% exported for use from ootCstn
-export([get_access_point_dn/0,
	 get_access_point_alt_dn/0,
	 cstn_complete/1,
	 cstn_update/1]).

%% exported for use from net_mapping slave
-export([net_mapping/1]).

%% called by ootDhcp
-export([set_lmt_ip/1]).
-export([set_lmt_mask/1]).

%% callback from APPM
-export([warm/0, warm_done/0]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% Test functions
-export([test_register_cfg_upd_cb/1,
	 test_register_cfg_upd_cb/2]).


%% -compile([export_all, nowarn_unused_vars]).
-compile([nowarn_unused_vars]).

%%% ----------------------------------------------------------
%%% Include files
%%% ----------------------------------------------------------
-include("oot.hrl").

%%% ----------------------------------------------------------
%%% Macros
%%% ----------------------------------------------------------
-define(SERVER, ?MODULE).
-define(INIT_IMM_TIMEOUT, 1000).
-define(CEC_CALL_TIMEOUT, 60000).
-define(GET_CALL_TIMEOUT, 30000).
-define(NS_FILE, "/home/sirpa/oam_net_namespace").

-define(INITIAL_IP_CNF, [{?CNF_IPV4_ADDR, []},
			 {?CNF_IPV4_ADDR_ALT, []},
			 {?CNF_ACC_POINT_ADDR, []},
			 {?CNF_ACC_POINT_ADDR_ALT, []},
			 {?CNF_OAP_NAMESPACE, undefined},
			 {?CNF_OAP_ALT_NAMESPACE, undefined}]).

-define(INITIAL_RVNFM_IP_CNF, [{?CNF_IPV4_ADDR, []},
			       {?CNF_ACC_POINT_ADDR, []},
			       {?CNF_OAP_NAMESPACE, undefined}]).

-define(INITIAL_VRCS_IP_CNF, [{?CNF_IPV4_ADDR, []},
			      {?CNF_ACC_POINT_ADDR, []},
			      {?CNF_OAP_NAMESPACE, undefined}]).

-define(EMPTY_IP_CNF, [{?CNF_IPV4_ADDR, []},
		       {?CNF_IPV4_ADDR_ALT, []},
		       {?CNF_ACC_POINT_ADDR, []},
		       {?CNF_ACC_POINT_ADDR_ALT, []},
		       {?CNF_OAP_NAMESPACE, <<>>},
		       {?CNF_OAP_ALT_NAMESPACE, <<>>}]).

-define(EMPTY_RVNFM_IP_CNF, [{?CNF_IPV4_ADDR, []},
			     {?CNF_ACC_POINT_ADDR, []},
			     {?CNF_OAP_NAMESPACE, <<>>}]).

-define(EMPTY_VRCS_IP_CNF, [{?CNF_IPV4_ADDR, []},
			    {?CNF_ACC_POINT_ADDR, []},
			    {?CNF_OAP_NAMESPACE, <<>>}]).

-define(NET_MAPPING_FILE, "/var/run/net_mapping.json").
%% -define(NET_MAPPING_FILE, "/home/eolaand/tmp/net_mapping.json").
-define(OAM_KEY, <<"oam">>).
-define(DEVICE_KEY, <<"device">>).
-define(RVNFM_CNF_KEYS, [?CNF_OAP_NAMESPACE,
			 ?CNF_IPV4_ADDR,
			 ?CNF_ACC_POINT_ADDR]).

-define(VRCS_CNF_KEYS, [?CNF_CLI_PORT,
			?CNF_NETCONF_PORT,
			?CNF_DSCP,
			?CNF_OAP_NAMESPACE,
			?CNF_IPV4_ADDR,
			?CNF_ACC_POINT_ADDR]).

%%% ----------------------------------------------------------
%%% Records
%%% ----------------------------------------------------------
-record(state, {imm_oi_handle_oap,
		imm_om_handle,
		config = [],
		test_cfg_upd_cb = [],
		cfg_upd_cb = [],
		pending_ccb = [],
		pending_tx = [],
		dhcp_ref,
		access_point_dn,
		access_point_alt_dn,
		lmt_ipv4,
		lmt_ipv4_mask = 24, % default incase the dhcp-client misses
		wait_for_imm = false,
		warm_restart = off,	% off | release | on
		ao_slave,
		nm_slave,
		vrcs = false,
		rvnfm = false}).

%%% ----------------------------------------------------------
%%% Type specs
%%% ----------------------------------------------------------
-type error()::{error, Reason::string()}.

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
%%% @doc
%%% Starts the OOT server.
%%% @end
-spec start_link() -> {ok, pid()} | error().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%% Called from ootImm
%%% @private
store_ccb_attrs(CcbId, Attrs) ->
    gen_server:cast(?SERVER, {store_ccb_attrs, {CcbId, Attrs}}).

%%% @private
get_ccb_attrs(CcbId) ->
    gen_server:call(?SERVER, {get_ccb_attrs, CcbId}, ?GET_CALL_TIMEOUT).

%%% @private
del_ccb_attrs(CcbId) ->
    gen_server:cast(?SERVER, {del_ccb_attrs, CcbId}).

%%% @private
config_update() ->
    gen_server:cast(?SERVER, config_update).

%%% @private
config_update(CcbId) ->
    gen_server:cast(?SERVER, {config_update, CcbId}).

%%% @private
get_access_point_dn() ->
    gen_server:call(?SERVER, get_access_point_dn, ?GET_CALL_TIMEOUT).

%%% @private
get_access_point_alt_dn() ->
    gen_server:call(?SERVER, get_access_point_alt_dn, ?GET_CALL_TIMEOUT).

%%% private
%% store_tx_objects(Tx, Objs) ->
%%     gen_server:cast(?SERVER, {store_tx_objects, {Tx, Objs}}).

%%% private
%% get_tx_objects(Tx) ->
%%     gen_server:call(?SERVER, {get_tx_objects, Tx}, ?GET_CALL_TIMEOUT).

%%% @private
get_cached_config() ->
    gen_server:call(?SERVER, get_cached_config, ?GET_CALL_TIMEOUT).

%% Called from ootCstn

%%% @private
cstn_complete(TNData) ->
    gen_server:cast(?SERVER, {cstn_complete, TNData}).

%%% @private
cstn_update(TNData) ->
    gen_server:cast(?SERVER, {cstn_update, TNData}).

%%% @private
net_mapping(NMData) ->
    gen_server:cast(?SERVER, {net_mapping, NMData}).

%%% @private
set_lmt_ip({_, _, _, _} = IP) ->
    gen_server:cast(?SERVER, {set_lmt_ip, IP});
set_lmt_ip(undefined) ->
    gen_server:cast(?SERVER, {set_lmt_ip, undefined}).

%%% @private
set_lmt_mask(Mask) when is_integer(Mask) ->
    gen_server:cast(?SERVER, {set_lmt_mask, Mask}).

%%% @private
warm() ->
    ?LOG_INFO("warm request"),
    ootCstn:warm(),
    gen_server:cast(?SERVER, warm).

%%% @private
warm_done() ->
    gen_server:cast(?SERVER, warm_done).

%%% @private
set_oap_attrs(DN, AttrVals) ->
    gen_server:cast(?SERVER, {set_oap_attrs, DN, AttrVals}).

%%% Functions used for test

%%% ----------------------------------------------------------
%%% Register callback for notifications about port config updates
%%% of test subscribers. Note that the calling pid is monitored and 
%%% the registration is only valid as long as the process is alive. 
%%% Also, when test mode is activated, subscribers using the standard
%%% function are ignored.
%%%
%%% ----------------------------------------------------------
%%% @private
test_register_cfg_upd_cb(Fun) when is_function(Fun) ->
    test_register_cfg_upd_cb(Fun, self()).


test_register_cfg_upd_cb(Fun, Pid) when is_function(Fun), is_pid(Pid) ->
    gen_server:call(?SERVER, {test_register_cfg_upd_cb, Fun, Pid}, 
		    ?GET_CALL_TIMEOUT).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    OptList2 = [{maxSize,          1},
		{maxSizeKb,        1024},
		{rotatingSegments, 3},
		{public,           false},
		{local,            false},
		{encrypted,        false},
		{compressed,       false}],
    logI:create_log("OotLog", OptList2),
    ootLib:log_versions(), %Print module versions in top of log
    create_link_local(),
    appmI:register_warm_cb(?MODULE), %mnesia write, appm need not be running
    IsVRCS = ootLib:is_vrcs(),
    IsRVNFM = ootLib:is_rvnfm(),
    NMPid = start_net_mapping_slave(IsRVNFM),
    AOPid = start_admin_owner(IsRVNFM),
    State = #state{ao_slave = AOPid,
		   nm_slave = NMPid,
		   vrcs = IsVRCS,
		   rvnfm = IsRVNFM},

    case swmI:is_upgrade_ongoing() of
        true when not State#state.rvnfm ->
	    ?LOG_INFO("Started."),
            Self = self(),
            proc_lib:spawn_link(fun() -> wait_for_imm(Self) end),
            {ok, State#state{wait_for_imm = true}};
	_False ->
            continue_init(State)
    end.


continue_init(State) ->
    %% split these two so that they can be called separately after upgrade
    {ok, NextState} = maybe_run_dhcp(State),
    ?LOG_INFO("Started."),
    init_state(NextState).


start_net_mapping_slave(false) ->
    undefined;

start_net_mapping_slave(_True) ->
    spawn_link(fun net_mapping_reader/0).


start_admin_owner(false) ->
    spawn_link(fun imm_admin_owner/0);

start_admin_owner(_True) ->
    undefined.


maybe_run_dhcp(State) ->
    run_dhcp(ootLib:is_target(), State).

%%----------------------
%% Target node
%%----------------------
run_dhcp(true, State) ->
    case ootDhcp:start_client() of
	{ok, Pid} ->
	    Ref = erlang:monitor(process, Pid),
	    {ok, State#state{dhcp_ref = Ref}};
	Error ->
	    {stop, Error}
    end;

%%----------------------
%% Simulated node
%%----------------------
run_dhcp(_,  State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_ccb_attrs, CcbId}, _From, State) ->
    Attrs = proplists:get_value(CcbId, State#state.pending_ccb, []),
    {reply, Attrs, State};

%% handle_call({get_tx_objects, Tx}, _From, State) ->
%%     Objs = proplists:get_value(Tx, State#state.pending_tx, []),
%%     NewPending = lists:keydelete(Tx, 1, State#state.pending_tx),
%%     {reply, Objs, State#state{pending_tx = NewPending}};

handle_call(get_cached_config, _From, State) ->
    {reply, State#state.config, State};


handle_call({test_register_cfg_upd_cb, Fun, Pid}, _From, State) ->
    CBFuns = State#state.test_cfg_upd_cb,
    lists:keymember(Pid, 1, CBFuns) orelse erlang:monitor(process, Pid),
    NewCBFuns = lists:keystore(Pid, 1, CBFuns, {Pid, Fun}),
    {reply, ok, State#state{test_cfg_upd_cb = NewCBFuns}};

handle_call({register_cfg_upd_cb, Fun, Pid}, _From, State) ->
    CBFuns = State#state.cfg_upd_cb,
    lists:keymember(Pid, 1, CBFuns) orelse erlang:monitor(process, Pid),
    NewCBFuns = lists:keystore(Pid, 1, CBFuns, {Pid, Fun}),
    {reply, ok, State#state{cfg_upd_cb = NewCBFuns}};


handle_call(dump, _From, State) ->
    {reply, {ok, State}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({stop, Reason}, _From, State) ->
    {stop, Reason, ok, State};

%%% Above here handle_call() works even while upgrade of IMM is pending
handle_call(_Any, _From, #state{wait_for_imm = true} = State) ->
    {reply, {error, oot_not_started}, State};

handle_call(get_oap_port_conf, _From, State) ->
    Conf = [{ootLib:conf_type_to_port_type(Type), Port} ||
	       {Type, Port} <- State#state.config,
	       lists:member(Type, ?CNF_PORTS)],
    {reply, Conf, State};

handle_call({get_oap_port_conf, Type}, _From, State) ->
    ConfType = ootLib:port_type_to_conf_type(Type),
    case proplists:get_value(ConfType, State#state.config) of
	Port when is_integer(Port) ->
	    {reply, Port, State};
	_ ->
	    {reply, sysEnv:get_initial_port_conf(Type), State}
    end;

handle_call({get_port_conf, Type}, _From, State)
  when State#state.imm_om_handle =/= undefined ->
    case ootImm:get_port_conf(State#state.imm_om_handle, Type) of
	undefined ->
	    {reply, sysEnv:get_initial_port_conf(Type), State};
	Port ->
	    {reply, Port, State}
    end;

handle_call({get_port_conf, Type}, _From, State) ->
    Port = sysEnv:get_initial_port_conf(Type),
    {reply, Port, State};

handle_call(get_oap_ip_addr, _From, State)
  when State#state.warm_restart =/= off; 
       State#state.test_cfg_upd_cb =/= [] ->
    {reply, [], State};

handle_call(get_oap_ip_addr, _From, State) ->
    Reply = get_ip_addr_from_conf(State#state.config),
    {reply, Reply, State};

handle_call(get_ip_addr, _From, State)
  when State#state.imm_om_handle =/= undefined ->
    Config = ootImm:get_config(State#state.imm_om_handle),
    Reply = get_ip_addr_from_conf(Config),
    {reply, Reply, State};

handle_call(get_ip_addr, _From, State) ->
    {reply, "", State};

handle_call(get_oap_ip_addr_alt, _From, State)
  when State#state.warm_restart =/= off; 
       State#state.test_cfg_upd_cb =/= [] ->
    {reply, [], State};

handle_call(get_oap_ip_addr_alt, _From, State) ->
    Reply = ootLib:get_ip_addr_from_conf(?CNF_ACC_POINT_ADDR_ALT, 
					 State#state.config),
    {reply, Reply, State};

handle_call(get_ip_addr_alt, _From, State)
  when State#state.imm_om_handle =/= undefined ->
    Config = ootImm:get_config(State#state.imm_om_handle),
    Reply = ootLib:get_ip_addr_from_conf(?CNF_ACC_POINT_ADDR_ALT, Config),
    {reply, Reply, State};

handle_call(get_ip_addr_alt, _From, State) ->
    {reply, "", State};

handle_call(get_oap_ipv4_addr, _From, State) ->
    Reply = ootLib:get_ip_addr_from_conf(?CNF_IPV4_ADDR, State#state.config),
    {reply, Reply, State};

handle_call(get_config_from_imm, _From, State)
  when State#state.imm_om_handle =/= undefined ->
    Config = ootImm:get_config(State#state.imm_om_handle),
    {reply, {ok, Config}, State};

handle_call(get_config_from_imm, _From, State) ->
    {reply, {error, imm_not_available}, State};

handle_call(get_oap_dscp, _From, State) ->
    Reply = proplists:get_value(?CNF_DSCP, State#state.config),
    {reply, Reply, State};

handle_call(get_dscp, _From, State)
  when State#state.imm_om_handle =/= undefined ->
    Reply = ootImm:get_dscp(State#state.imm_om_handle),
    {reply, Reply, State};

handle_call(get_dscp, _From, State) ->
    {reply, undefined, State};

handle_call(get_lmt_ipv4, _From, State) ->
    Addr = ootLib:get_lmt_ipv4(State#state.vrcs, State#state.lmt_ipv4),
    {reply, Addr, State};

handle_call(get_lmt_ipv4_with_mask, _From, State) ->
    Addr     = ootLib:get_lmt_ipv4(State#state.vrcs, State#state.lmt_ipv4),
    AddrMask = ootLib:get_lmt_ipv4_mask(State#state.vrcs, 
					State#state.lmt_ipv4_mask),
    {reply, {Addr, AddrMask}, State};

handle_call(get_access_point_dn, _From, State) ->
    Res = ootLib:imm_to_dn(State#state.access_point_dn),
    ?LOG_INFO("get_access_point_dn: ~p", [Res]),
    {reply, Res, State};

handle_call(get_access_point_alt_dn, _From, State) ->
    Res = ootLib:imm_to_dn(State#state.access_point_alt_dn),
    ?LOG_INFO("get_access_point_alt_dn: ~p", [Res]),
    {reply, Res, State};

handle_call(Cmd, _From, State)
  when Cmd =:= get_oap_namespace; Cmd =:= get_oap_alt_namespace ->
    Key = get_ns_cfg_key(Cmd),
    case proplists:get_value(Key, State#state.config) of
	Val when Val =/= undefined, State#state.test_cfg_upd_cb =:= [] ->
	    ?LOG_INFO("~p: ~p", [Cmd, Val]),
	    {reply, {ok, Val}, State};
	Val when Val =/= undefined ->
	    ?LOG_INFO("~p, Namespace = ~p, Return <<>> in test mode", 
		      [Cmd, Val]),
	    {reply, {ok, <<>>}, State};
	_Undef ->
	    {reply, {error, resolution_pending}, State}
    end;

handle_call(Call, _From, State) ->
    Reply = {error, {unknown_call, Call}},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(config_update, State) ->
    ?LOG_INFO("COM transaction finished, get updated config", []),
    NewState = handle_config_update(State),
    {noreply, NewState};

handle_cast({config_update, CcbId}, State) ->
    Pending = State#state.pending_ccb,
    case proplists:get_value(CcbId, Pending) of
	undefined ->
	    {noreply, State};
	_ ->
	    ?LOG_INFO("CCB ~p ready, get updated config", [CcbId]),
	    NewState = handle_config_update(State),
	    NewPending = lists:keydelete(CcbId, 1, Pending),
	    {noreply, NewState#state{pending_ccb = NewPending}}
    end;

handle_cast({store_ccb_attrs, {CcbId, Attrs}}, State) ->
    Pending = State#state.pending_ccb,
    PendAttrs = proplists:get_value(CcbId, Pending, []),
    NewPending =
	lists:keystore(CcbId, 1, Pending, {CcbId, Attrs ++ PendAttrs}),
    {noreply, State#state{pending_ccb = NewPending}};

handle_cast({del_ccb_attrs, CcbId}, State) ->
    NewPending = lists:keydelete(CcbId, 1, State#state.pending_ccb),
    {noreply, State#state{pending_ccb = NewPending}};

%% handle_cast({store_tx_objects, {Tx, Objs}}, State) ->
%%     Pending = State#state.pending_tx,
%%     PendObjs = proplists:get_value(Tx, Pending, []),
%%     NewObjs =
%% 	lists:foldl(fun(Obj, AccObjs) ->
%% 			    lists:keystore(element(1, Obj), 1, AccObjs, Obj)
%% 		    end, PendObjs, Objs),
%%     NewPending = lists:keystore(Tx, 1, Pending, {Tx, NewObjs}),
%%     {noreply, State#state{pending_tx = NewPending}};

handle_cast({set_lmt_ip, IP}, #state{lmt_ipv4 = IP} = State) ->
    {noreply, State};

handle_cast({set_lmt_ip, IP}, State) ->
    notify_subscribers(State, [{lmt_ipv4, IP}]),
    {noreply, State#state{lmt_ipv4 = IP}};

handle_cast({set_lmt_mask, Mask}, State) ->
    {noreply, State#state{lmt_ipv4_mask = Mask}};


handle_cast({object_update_notify, _Obj}, State) ->
    {noreply, State};

handle_cast({set_oap_attrs, DN, AttrVals} = Item, State) ->
    State#state.ao_slave ! Item,
    {noreply, State};

handle_cast({Tag, Data} = Cast, #state{rvnfm = true} = State)
  when Tag =:= cstn_version; Tag =:= cstn_complete; Tag =:= cstn_update ->
    ?LOG_INFO("R-VNFM: ignore ~p", [Cast]),
    {noreply, State};

handle_cast({cstn_complete, TNData}, State) ->
    ?LOG_INFO("Got cstn_complete: ~p", [TNData]),
    NewState = handle_cstn_complete(TNData, State),
    {noreply, NewState};

handle_cast({cstn_update, TNData}, State) ->
    ?LOG_INFO("Got cstn_update: ~p", [TNData]),
    IpAddrCnf = tn_data_to_ip_addr_cnf(TNData, State),
    UpdConf = [Par || Par <- IpAddrCnf,
		      not lists:member(Par, State#state.config)],
    APDN = State#state.access_point_dn,
    APAltDN = State#state.access_point_alt_dn,
    NewState = handle_updated_config(UpdConf, APDN, APAltDN, State),
    {noreply, NewState};

handle_cast({net_mapping, NMData}, State) ->
    ?LOG_INFO("Got net_mapping: ~p", [NMData]),
    NewState = handle_net_mapping(NMData, State),
    {noreply, NewState};

%% fourth attempt, notify after warm happens when cstn runs (i.e. TN drives 
%% this) when warm restart begins, notify everyone that OaM IP (and alt) has 
%% been unset and namespace removed, during warmrestart queries of OaM IP, alt
%% IP, and namespace will be answered as if they were not configured.
%% At warm_done, answer queries after OaM IP, alt IP truthfully.
%% If OamAccessPoint is unset, set warm_restart=off at cstn_complete().
handle_cast(warm, State) when State#state.vrcs ->
    %% EmptyCnf = lists:map(fun({Key, undefined}) ->
    %% 				 {Key, <<>>};
    %% 			    (KeyVal) ->
    %% 				 KeyVal
    %% 			 end, ?EMPTY_VRCS_CNF),
    %% notify_subscribers(State, EmptyCnf),
    notify_subscribers(State, ?EMPTY_VRCS_IP_CNF),
    {noreply, State#state{warm_restart = on}};

handle_cast(warm, State) ->
    %% EmptyConf = [{?CNF_OAP_NAMESPACE, <<>>},
    %% 		 {?CNF_OAP_ALT_NAMESPACE, <<>>} | ?EMPTY_IP_CNF],
    %% notify_subscribers(State, EmptyConf),
    notify_subscribers(State, ?EMPTY_IP_CNF),
    {noreply, State#state{warm_restart = on}};
handle_cast(warm_done, State) ->
    {noreply, State#state{warm_restart = release}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(imm_ready, State) ->
    case init_state(State) of
	{ok, NextState} -> %imm init worked
	    {ok, NewState} = maybe_run_dhcp(NextState),
	    ootCstn:new_dn(), %not sure if needed
	    %% self() ! connect_cec,
	    notify_subscribers(NewState, NewState#state.config),
	    {noreply, NewState};
	{error, NextState} -> %imm not ready, retry
	    sysInitI:warning_msg("~p: IMM not ready, retry in 500ms~n",
				 [?MODULE]),
	    erlang:send_after(500, self(), imm_ready),
	    {noreply, NextState}
    end;

handle_info({'DOWN', OldRef, process, _, Reason},
	    #state{dhcp_ref = OldRef} = State) ->
    %% DHCP process down
    case ootDhcp:start_client() of
	{ok, Pid} ->
	    NewRef = erlang:monitor(process, Pid),
	    {noreply, State#state{dhcp_ref = NewRef}};
	Error ->
	    {stop, {Reason, Error}, State}
    end;

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    case lists:keymember(Pid, 1, State#state.test_cfg_upd_cb) of
        true -> %% Test mode
            ?LOG_INFO("DOWN in test mode, ~p", [State#state.test_cfg_upd_cb]),
            %% Test config update subscriber is down. Remove subscription.
            NewCBFuns = lists:keydelete(Pid, 1, State#state.test_cfg_upd_cb),
            {noreply, State#state{test_cfg_upd_cb = NewCBFuns}};
        false ->
            ?LOG_INFO("Config update subscriber ~p is DOWN: ~p", 
		      [Pid, State#state.cfg_upd_cb]),
            %% Config update subscriber is down. Remove subscription.
            NewCBFuns = lists:keydelete(Pid, 1, State#state.cfg_upd_cb),
            {noreply, State#state{cfg_upd_cb = NewCBFuns}}
    end;

handle_info(_Info, State) ->
    ?LOG_INFO("handle_info: ~p", [_Info]),
    sysInitI:info_msg("~p: handle_info: ~p~n", [?MODULE, _Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ootImm:finalize_oap_oi(State#state.imm_oi_handle_oap),
    ootImm:finalize_om(State#state.imm_om_handle),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%% run by fun() in separately spawned process
wait_for_imm(Parent) ->
    ?LOG_INFO("Upgrade ongoing, begin waiting for IMM objects."),
    sysInitI:info_msg("~p: Upgrade ongoing, begin waiting for IMM objects.~n", 
		      [?MODULE]),
    ootImm:wait_for_upgrade_transform(),
    ?LOG_INFO("IMM class OamAccessPoint available"),
    sysInitI:info_msg( "~p: IMM class OamAccessPoint available.~n",
		       [?MODULE]),
    ootImm:wait_for_objects(),
    Parent ! imm_ready,
    normal.

%%% ----------------------------------------------------------
%%%            net_mapping_reader() -> ok
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Run in a separate process that polls the file system
%%%
%%% ----------------------------------------------------------
net_mapping_reader() ->
    ?LOG_INFO("Read net_mapping file", []),
    %% erlang:monitor(process, whereis(?SERVER)),
    read_net_mapping_loop().


read_net_mapping_loop() ->
    case read_net_mapping() of
	{ok, {IPv6Addr, IPv4Addr}} ->
	    DefaultNS = <<>>,
	    net_mapping({IPv6Addr, IPv4Addr, DefaultNS});
	stop_reading ->
	    DefaultNS = <<>>,
	    DefaultIPv6Addr = [],
	    DefaultIPv4Addr = [],
	    net_mapping({DefaultIPv6Addr, DefaultIPv4Addr, DefaultNS});
	_Error ->
	    erlang:start_timer(3000, self(), read_net_mapping)
    end,
    receive
	{timeout, _TRef, read_net_mapping} ->
	    read_net_mapping_loop();
	%% {'DOWN', _OldRef, process, _Pid, _Reason} ->
	%%     ok;
	stop ->
	    ok
    end.


read_net_mapping() ->
    case file:read_file(?NET_MAPPING_FILE) of
	{ok, Bin} ->
	    read_net_mapping_file(Bin);
	{error, Reason} ->
	    {error, "net_mapping.json file is missing"}
    end.


read_net_mapping_file(Bin) ->
    try
	NameMap = maps:get(?OAM_KEY, jsone:decode(Bin)),
	case ootLib:to_list(maps:get(?DEVICE_KEY, NameMap)) of
	    "eth0" ->
		%% Temp fix to avoid collision with LMT port
		?LOG_INFO("~s: Device eth0, ignore to avoid conflict with LMT",
			  [?NET_MAPPING_FILE]),
		stop_reading;
	    Device ->
		?LOG_INFO("~s read successfully!~nDevice = ~s",
			  [?NET_MAPPING_FILE, Device]),
		get_ip_addresses(Device)
	end
    catch _:_ ->
	    {error, "net_mapping.json file is corrupt"}
    end.


get_ip_addresses(Device) ->
    ootLib:get_ip_addresses(Device).


%%% ----------------------------------------------------------
%%%            imm_admin_owner() -> ok
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description: Run in a separate process to avoid deadlocks and conflicts
%%%              with GMF as admin owner of OamAccessPoint
%%% ----------------------------------------------------------
imm_admin_owner() ->
    erlang:monitor(process, whereis(?SERVER)),
    erlang:register(ootAO, self()),
    imm_admin_owner_loop([]).


imm_admin_owner_loop(Pending) ->
    receive
	{timeout, _TRef, set_next_oap_attrs} ->
	    case set_oap_attrs_in_imm(Pending) of
		[] ->
		    imm_admin_owner_loop([]);
		NewPending ->
		    erlang:start_timer(100, self(), set_next_oap_attrs),
		    imm_admin_owner_loop(NewPending)
	    end;
	{set_oap_attrs, DN, AttrVals} = Next ->
	    Pending =/= [] orelse
		erlang:start_timer(500, self(), set_next_oap_attrs),
	    imm_admin_owner_loop(Pending ++ [Next]);
	{'DOWN', _OldRef, process, _Pid, _Reason} ->
	    ok;
	stop ->
	    ok
    end.


set_oap_attrs_in_imm([{_Tag, DN, AttrVals} | T] = Pending) ->
    try
	ok = ootImm:set_oap_attrs(DN, AttrVals),
	set_oap_attrs_in_imm(T)
    catch _:_ ->
	    Pending
    end;

set_oap_attrs_in_imm([]) ->
    [].


%%% ----------------------------------------------------------
%%%            init_state(Old_or_empty_State) ->
%%%			{ok, State::state{}} | {error, State::state{}}.
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
init_state(State) ->
    InitialConf = get_initial_config(State),
    NewState = State#state{config = InitialConf},
    case NewState#state.rvnfm of
	true ->
	    {ok, NewState#state{wait_for_imm = false}};
	_False ->
	    init_state_imm(NewState)
    end.


init_state_imm(State) ->
    case init_imm(State) of
    	{ok, NewState} ->
	    OmHandle = NewState#state.imm_om_handle,
	    Config = get_config(OmHandle, NewState),
	    NewConfig = merge_config(Config, NewState#state.config),
	    APDN = ootImm:get_acc_point_dn(OmHandle),
	    APAltDN = ootImm:get_acc_point_alt_dn(OmHandle),
	    {ok, NewState#state{config = NewConfig,
				access_point_dn = APDN,
				access_point_alt_dn = APAltDN,
				wait_for_imm = false}};
    	_Error ->
	    %% HERE this is a real error, happens after upgrade, currently
	    %% worked around with retrying/lg
	    %% sysInitI:warning_msg("~p: Failed to initiate IMM: ~p~n",
	    %% 			     [?MODULE, _Error]),
    	    {error, State}
    end.


get_config(_OmHandle, _State) ->
    %% ootImm:get_oap_config(OmHandle).
    ootComSysM:get_ecim_config().

%%% ----------------------------------------------------------
%%%            init_imm() -> {ok, State::state()} | error().
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
init_imm(State) ->
    try
	case ootImm:initialize_om() of
	    {ok, OmH} ->
		OiHOAP = ootImm:initialize_oap_oi(),
		NewState = State#state{imm_oi_handle_oap = OiHOAP,
				       imm_om_handle = OmH},
		?LOG_INFO("Successfully initialized IMM."),
		sysInitI:info_msg("~p: Successfully initialized IMM.~n",
				  [?MODULE]),
		{ok, NewState};
	    Error ->
		Error
	end
    catch _:_E ->
	    ?LOG_WARNING("Failed to initiate IMM: ~p~n~p",
			 [_E, erlang:get_stacktrace()]),
	    sysInitI:warning_msg("~p: Failed to initiate IMM: ~p~n~p~n",
				 [?MODULE, _E, erlang:get_stacktrace()]),
	    {error, failed_to_init_imm}
    end.


%%% ----------------------------------------------------------
%%%            handle_config_update(State::state()) -> NewState::state().
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
handle_config_update(State) ->
    APDN = ootImm:get_acc_point_dn(State#state.imm_om_handle),
    APAltDN = ootImm:get_acc_point_alt_dn(State#state.imm_om_handle),
    case get_config_updates(APDN, APAltDN, State) of
	[] when APDN =:= State#state.access_point_dn,
		APAltDN =:= State#state.access_point_alt_dn ->
	    ?LOG_INFO("No config updates", []),
	    State;
	UpdatedConfig ->
	    ?LOG_INFO("Updated Config: ~p", [UpdatedConfig]),
	    handle_updated_config(UpdatedConfig, APDN, APAltDN, State)
    end.


%%% ----------------------------------------------------------
%%%            handle_updated_config() -> State::state().
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
handle_updated_config(UpdatedConfig, APDN, APAltDN, State) ->
    notify_subscribers(State, UpdatedConfig),
    NewConfig = merge_config(UpdatedConfig, State#state.config),
    NewState = State#state{config = NewConfig,
			   access_point_dn = APDN,
			   access_point_alt_dn = APAltDN},
    %% HERE remove initiate_namespace_read(State, NewState).
    new_dn(State, NewState),
    NewState.


%%% ----------------------------------------------------------
%%%            handle_cstn_complete() -> State::state().
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
handle_cstn_complete(TNData, State) ->
    IpAddrConf = tn_data_to_ip_addr_cnf(TNData, State),
    NSConf = [KV || {K, V} = KV <- TNData, K =:= ?CNF_OAP_NAMESPACE
			orelse K =:= ?CNF_OAP_ALT_NAMESPACE],
    handle_config_complete(IpAddrConf, NSConf, State).

%%% ----------------------------------------------------------
%%%            handle_net_mapping() -> State::state().
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
handle_net_mapping({IPv6Addr, IPv4Addr, NS}, State) ->
    NSConf = [{?CNF_OAP_NAMESPACE, NS}],
    IpAddrConf = net_mapping_ip_addr_conf(IPv6Addr, IPv4Addr),
    handle_config_complete(IpAddrConf, NSConf, State).


net_mapping_ip_addr_conf([], IPv4Addr) ->
    %% net_mapping_ip_addr_conf(_, IPv4Addr) ->
    [{?CNF_IPV4_ADDR, IPv4Addr}, {?CNF_ACC_POINT_ADDR, IPv4Addr}];

net_mapping_ip_addr_conf(IPv6Addr, IPv4Addr) ->
    [{?CNF_IPV4_ADDR, IPv4Addr}, {?CNF_ACC_POINT_ADDR, IPv6Addr}].


%%% ----------------------------------------------------------
%%%           handle_config_complete() -> State::state().
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
handle_config_complete(IpAddrConf, NSConf, State) ->
    UpdConf = [Par || Par <- IpAddrConf ++ NSConf,
		      not lists:member(Par, State#state.config)],
    NewConf = merge_config(UpdConf, State#state.config),
    NewState = State#state{config = NewConf},
    case NewState#state.warm_restart of
	release ->
	    notify_config_complete(NSConf, NewState),
	    NewState#state{warm_restart = off};
	off ->
	    notify_subscribers(State, UpdConf),
	    NewState;
	_ ->
	    NewState
    end.


notify_config_complete(NSConf, State) ->
    IpAddrs = [KV || {Key, Val} = KV <- State#state.config,
		     Key =:= ?CNF_IPV4_ADDR orelse
			 Key =:= ?CNF_IPV4_ADDR_ALT orelse
			 Key =:= ?CNF_ACC_POINT_ADDR orelse
			 Key =:= ?CNF_ACC_POINT_ADDR_ALT],
    notify_subscribers(State, NSConf ++ IpAddrs).


%%% ----------------------------------------------------------
%%%            notify_subscribers() -> ok.
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
notify_subscribers(#state{}, UpdConf = []) ->
    %% if only accessPoint DN changed
    ok; 

%%% There are contents in test_cfg_up_cb which means that we should run in 
%%% testmode, thereby only notifying test subscribers and ignoring normal 
%%% subscribers in the cfg_upd_cb list 
notify_subscribers(#state{test_cfg_upd_cb = CBFuns} = State, UpdConf)
  when CBFuns =/= [] ->
    FilteredConf = filter_conf_values(UpdConf, State),
    ?LOG_INFO("Do not notify_subscribers, test mode ~p", [FilteredConf]),
    lists:foreach(fun({_Pid, CB}) ->
			  spawn(fun() ->
					notify_subscriber(CB, FilteredConf)
				end)
		  end, CBFuns);

notify_subscribers(#state{cfg_upd_cb = CBFuns} = State, UpdConf) ->
    FilteredConf = filter_conf_values(UpdConf, State),
    ?LOG_INFO("notify_subscribers: ~p", [FilteredConf]),
    lists:foreach(fun({_Pid, CB}) ->
			  spawn(fun() ->
					notify_subscriber(CB, FilteredConf)
				end)
		  end, CBFuns).


filter_conf_values(Config, State) ->
    case State#state.rvnfm of
	true ->
	    [{Key, Val} || {Key, Val} <- Config,
			   lists:member(Key, ?RVNFM_CNF_KEYS)];
	_ when State#state.vrcs ->
	    [{Key, Val} || {Key, Val} <- Config,
			   lists:member(Key, ?VRCS_CNF_KEYS)];
	_ ->
	    Config
    end.


notify_subscriber(CB, UpdConf) when is_function(CB) ->
    try
	CleanConf = clean_conf_values(UpdConf),
	CB(CleanConf)
    catch
	E:R ->
	    ?LOG_WARNING(
	       "OOT failed to execute callback: {~p,~p}~n~p",
	       [E, R, erlang:get_stacktrace()]),
	    sysInitI:warning_msg(
	      "OOT failed to execute callback: {~p,~p}~n~p~n",
	      [E, R, erlang:get_stacktrace()])
    end.


clean_conf_values(Conf) ->
    [clean_conf_value(KV) || KV <- Conf].


clean_conf_value({Key, IPStr})
  when (Key =:= ?CNF_IPV4_ADDR orelse
	Key =:= ?CNF_IPV4_ADDR_ALT orelse
	Key =:= ?CNF_ACC_POINT_ADDR orelse
	Key =:= ?CNF_ACC_POINT_ADDR_ALT),
       is_list(IPStr), IPStr =/= [] ->
    [IPAddr | _] = string:tokens(IPStr, "/"),
    {Key, IPAddr};

clean_conf_value({Key, _IPStr})
  when Key =:= ?CNF_IPV4_ADDR orelse
       Key =:= ?CNF_IPV4_ADDR_ALT orelse
       Key =:= ?CNF_ACC_POINT_ADDR orelse
       Key =:= ?CNF_ACC_POINT_ADDR_ALT ->
    {Key, ""};

clean_conf_value({Key, undefined})
  when Key =:= ?CNF_OAP_NAMESPACE orelse
       Key =:= ?CNF_OAP_ALT_NAMESPACE ->
    {Key, <<>>};

clean_conf_value(KV) ->
    KV.


get_config_updates(APDN, APAltDN, State) ->
    IpCnfEmpty = get_empty_ip_conf(APDN, APAltDN, State),
    Conf = ootComSysM:get_ecim_config() ++
	[{Key, Val} || {undefined, {Key, Val}} <- IpCnfEmpty],
    [Par || Par <- Conf, not lists:member(Par, State#state.config)].


get_empty_ip_conf(_APDN, _APAltDN, State)
  when State#state.rvnfm  ->
    [];

get_empty_ip_conf(APDN, _APAltDN, State)
  when State#state.vrcs ->
    lists:zip([APDN, APDN, APDN], ?EMPTY_VRCS_IP_CNF);

get_empty_ip_conf(APDN, APAltDN, State) ->
    lists:zip([APDN, APAltDN, APDN, APAltDN, APDN, APAltDN], ?EMPTY_IP_CNF).
    

merge_config(UpdConf, OldConf) ->
    lists:foldl(fun({Key, _Val} = KV, Conf) ->
			lists:keystore(Key, 1, Conf, KV)
		end, OldConf, UpdConf).


tn_data_to_ip_addr_cnf(TNData, State) ->
    IpAddrs = [add_deprecated_ipv4_addr({Key, ootLib:to_list(Val)}, State) ||
		  {Key, Val} <- TNData, Key =:= ?CNF_ACC_POINT_ADDR
		      orelse Key =:= ?CNF_ACC_POINT_ADDR_ALT],
    lists:keysort(1, lists:append(IpAddrs)).


add_deprecated_ipv4_addr({_Key, []} = KV, State) ->
    [KV];

add_deprecated_ipv4_addr({?CNF_ACC_POINT_ADDR, Val} = KV, State) ->
    [IPAddr | _] = string:tokens(Val, "/"),
    case ootLib:is_ipv4_addr(IPAddr) of
	true ->
	    [KV, {?CNF_IPV4_ADDR, Val}];
	_False ->
	    [KV, {?CNF_IPV4_ADDR, []}]
    end;

add_deprecated_ipv4_addr({?CNF_ACC_POINT_ADDR_ALT, Val} = KV, State) ->
    [IPAddr | _] = string:tokens(Val, "/"),
    case ootLib:is_ipv4_addr(IPAddr) of
	true ->
	    [KV, {?CNF_IPV4_ADDR_ALT, Val}];
	_False ->
	    [KV, {?CNF_IPV4_ADDR_ALT, []}]
    end.


get_initial_config(State) when State#state.rvnfm ->
    ?INITIAL_RVNFM_IP_CNF;

get_initial_config(State) when State#state.vrcs ->
    CLIPort = sysEnv:get_initial_port_conf(?SYS_CLI_PORT),
    NetConfPort = sysEnv:get_initial_port_conf(?SYS_NETCONF_PORT),
    DSCP = 0,
    [{?CNF_CLI_PORT, CLIPort},
     {?CNF_NETCONF_PORT, NetConfPort},
     {?CNF_DSCP, DSCP} | ?INITIAL_VRCS_IP_CNF];

get_initial_config(_) ->
    CLIPort = sysEnv:get_initial_port_conf(?SYS_CLI_PORT),
    NetConfPort = sysEnv:get_initial_port_conf(?SYS_NETCONF_PORT),
    DSCP = 0,
    [{?CNF_CLI_PORT, CLIPort},
     {?CNF_NETCONF_PORT, NetConfPort},
     {?CNF_DSCP, DSCP} | ?INITIAL_IP_CNF].


get_ip_addr_from_conf(Config) ->
    ootLib:get_ip_addr_from_conf(?CNF_ACC_POINT_ADDR, Config).


create_link_local() ->
    os:cmd("sudo ip link set dev eth0 up"),
    os:cmd("sudo ip addr add dev eth0 label eth0:1 scope link local "
	   "169.254.2.2/16 broadcast +").


new_dn(#state{access_point_dn = DN1, access_point_alt_dn = DN2},
       #state{access_point_dn = DN1, access_point_alt_dn = DN2}) ->
    %% DNs didn't change
    ok;

new_dn(_, _) ->
    ootCstn:new_dn().


get_ns_cfg_key(get_oap_namespace) ->
    ?CNF_OAP_NAMESPACE;
get_ns_cfg_key(get_oap_alt_namespace) ->
    ?CNF_OAP_ALT_NAMESPACE.

%%% #---------------------------------------------------------
%%% #5     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
