%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ootI.erl %
%%% @author etxlg
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R8A/R9A/R10A/R11A/R12A/1
%%%
%%% @doc == OOTI - OAM over TN Interface ==
%%% This module implements the API functions of OOT.
%%% @end
%%% ----------------------------------------------------------
-module(ootI).
-vsn('/main/R2A/R3A/R4A/R5A/R8A/R9A/R10A/R11A/R12A/1').
-date('2017-10-26').
-author('etxlg').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	ootI.erl %
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
%%% ----------------------------------------------------------
%%% R4A/1      2015-08-12 etxtory     Netmask changed acc to standard
%%% R4A/2      2015-08-24 eolaand     Add fcn for COMSA notifications
%%% R4A/3      2015-08-26 eolaand     Add stub fcns for alt IP address
%%% R4A/6      2015-10-12 etxlg       get_oap_alt_namespace()
%%% R4A/7      2015-11-10 etxlg       IPv6
%%% R4A/8      2015-11-10 etxlg       IPv6 getaddr/1
%%% R4A/9      2015-12-08 eolaand     Add fcn rm_duplicate_oap_ref/2,
%%% R9A/2      2017-01-24 uabesvi     new functions
%%%                                   get_ns_and_ip register_ip_change_cb
%%% ----------------------------------------------------------
%%% R5A/1      2016-04-26 etxpeno     (TR HU78388)
%%% ----------------------------------------------------------
%%% R9A/1      2017-02-21 etxpeno     Add is_change_status_changed/1
%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
%%% R10A/2     2017-05-23 eolaand     Remove OAP in R-VNFM environment
%%% R10A/4     2017-06-14 eolaand     Always call COMSA to fetch ports
%%% R10A/5     2017-06-19 eolaand     Add fcn get_oap_ipv4_addr
%%% R10A/7     2017-06-26 ecaiyan     Add unregister_ip_change_cb
%%% R10A/9     2017-07-11 egabing     Removed dependency towards TN IMM objects
%%% ----------------------------------------------------------
%%% R11A/1     2017-08-16 eolaand     Add prototypes for subscribe functions
%%% R11A/3     2017-09-14 eolaand     Always call ootServer to get namespace 
%%% ----------------------------------------------------------
%%% R12A/1     2017-10-25 etxlg       ootResolver:get_addresses/3
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% API
-export([register_cfg_upd_cb/1,
	 register_cfg_upd_cb/2,
	 get_oap_port_conf/0,
	 get_oap_port_conf/1,
	 get_oap_ipv4_addr/0,
	 get_lmt_ipv4/0,
	 get_lmt_ipv4_with_mask/0,
	 get_lmt_ll_ipv4/0,
	 get_lmt_ll_ipv4_with_mask/0]).

-export([snmp_agent_update/1]).

%% Inet options stuff (net_ns, dscp, oam-ip)
-export([get_oap_namespace/0, get_oap_alt_namespace/0,
	 get_ns_opt_list/0,
	 get_oam_ns_opt_list/0, get_lmt_ns_opt_list/0]).
-export([get_oap_dscp/0, get_dscp_opt_list/0, get_dscp_opt_list/1]).
-export([get_oap_ip_addr/0, get_oap_ip_opt_list/0, get_oap_ip_opt_list/1]).
-export([get_oap_ip_addr_alt/0]).
-export([get_all_oam_opt_list/0]).
%% Net NS aware resolver functions
-export([getaddr/1, getaddr/2, getaddr_lmt/2, getaddr_oam/2]).
-export([get_addresses/3]).	%SP338? resolve in arbitrary NS
%% called from appmServer
-export([restart_complete/0]).
%% called from comSysM
-export([object_update_notify/1]).
%% called from gmfImmUgInserter
-export([rm_duplicate_oap_ref/2]).

%% called from AIC
-export([is_change_status_changed/1]).

%% get name space and ip addres for an MO ref
-export([get_ns_and_ip/1]).
-export([register_ip_change_cb/2,
	 register_ip_change_cb/3,
	 unregister_ip_change_cb/2,
	 unregister_ip_change_cb/3]).

%% Prototypes for notification callbacks 
-export([ip_changed/4]).
-export([get_cfg_upd_cb_fun/0,
	 get_cfg_upd_cb_fun/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% Include files
%%% ----------------------------------------------------------
-include("oot.hrl").

%%% ----------------------------------------------------------
%%% Macros
%%% ----------------------------------------------------------
-define(SERVER, ootServer).
-define(CSTN_SERVER, ootCstn).
-define(CALL_TIMEOUT, 30000).

-define(IPPROTO_IPV6, 41).
-define(IPV6_TCLASS, 67).

%%% ----------------------------------------------------------
%%% Type specs
%%% ----------------------------------------------------------
-type port_type()::netconf | cli.

%%%
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% @doc
%%% Get port number for ssh services.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec get_oap_port_conf() -> [{Type::port_type(), Port::integer()}].
get_oap_port_conf() ->
    ootComSysM:get_ecim_port_conf().

%%% ----------------------------------------------------------
%%% @doc
%%% Get port number for ssh service.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec get_oap_port_conf(Type::port_type()) -> Port::integer().
get_oap_port_conf(Type) ->
    ootComSysM:get_ecim_port_conf(ootLib:conf_type_to_port_type(Type)).

%%% ----------------------------------------------------------
%%% @doc
%%% Get DSCP value
%%%
%%% @end
%%% ----------------------------------------------------------
-spec get_oap_dscp() -> DSCP::integer() | undefined.
get_oap_dscp() ->
    ootComSysM:get_ecim_dscp().

%%% ----------------------------------------------------------
%%% @doc
%%% Convert a DSCP value into an inet compatible optionlist to set DSCP.
%%% @end
%%% ----------------------------------------------------------
-spec get_dscp_opt_list(integer()) -> [] | [{tos, integer()}] |
				      [{raw, integer(), integer(), integer()}].
get_dscp_opt_list(0) ->
    [];
get_dscp_opt_list(Dscp) ->
    InetType = get_inet_type(),
    get_dscp_opt_list(Dscp, InetType).

%%% ----------------------------------------------------------
%%% @doc
%%% Get an inet compatible optionlist to set DSCP.
%%% @end
%%% ----------------------------------------------------------
-spec get_dscp_opt_list() -> [] | [{tos, integer()}] |
			     [{raw, integer(), integer(), integer()}].
get_dscp_opt_list() ->
  case get_oap_dscp() of
      0 ->
	  [];
      undefined ->
	  [];
      Num when is_integer(Num) ->
	  InetType = get_inet_type(),
	  get_dscp_opt_list(Num, InetType)
  end.

get_dscp_opt_list(Num, inet) ->
    [{tos, Num bsl 2}];
get_dscp_opt_list(Num, inet6) ->
    [{raw, ?IPPROTO_IPV6, ?IPV6_TCLASS, <<(Num bsl 2):32/native>>}].

get_inet_type() ->
    OapIpAddr = get_oap_ip_addr(),
    case inet:parse_ipv6strict_address(OapIpAddr) of
	{ok, _} ->
	    inet6;
	_ ->
	    inet
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% Register callback for notifications about port config updates.
%%% Note that the calling pid is monitored and the registration is
%%% only valid as long as the process is alive.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec register_cfg_upd_cb(Fun::fun()) -> ok | {error, oot_not_started}.
register_cfg_upd_cb(Fun) when is_function(Fun) ->
    register_cfg_upd_cb(Fun, self()).


-spec register_cfg_upd_cb(Fun::fun(), Pid::pid()) -> 
				 ok | {error, oot_not_started}.
register_cfg_upd_cb(Fun, Pid) when is_function(Fun), is_pid(Pid) ->
    call({register_cfg_upd_cb, Fun, Pid}).

%%% ----------------------------------------------------------
%%% @doc
%%% Get TN IP address
%%% @end
%%% ----------------------------------------------------------
-spec get_oap_ip_addr() -> IpAddress::string().
get_oap_ip_addr() ->
    case call(get_oap_ip_addr) of
	IP when is_list(IP) ->
	    IP;
	_Error ->
	    ""
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% Get IPv4 address if present
%%% @end
%%% ----------------------------------------------------------
-spec get_oap_ipv4_addr() -> IpAddress::string().
get_oap_ipv4_addr() ->
    case call(get_oap_ipv4_addr) of
	IP when is_list(IP) ->
	    IP;
	_Error ->
	    ""
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% Get an inet compatible optionlist with primary oam ip-address to
%%% bind to the OamAccessPoint.
%%% @end
%%% ----------------------------------------------------------
-spec get_oap_ip_opt_list() -> list().
get_oap_ip_opt_list() ->
    case get_oap_ip_addr() of
        [] ->
	    [];
        IPString ->
	    get_oap_ip_opt_list(IPString)
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% Get an inet compatible optionlist to bind to the OamAccessPoint.
%%% @end
%%% ----------------------------------------------------------
-spec get_oap_ip_opt_list(IPString::string()) -> list().
get_oap_ip_opt_list(IPString) when is_list(IPString), IPString =/= "" ->
    case inet:parse_address(IPString) of
	{ok, IPTuple} ->
	    [{ip, IPTuple}];
	_ ->
	    sysInitI:warning_msg(
	      "~p: unable to decode OaM IP address ~p~n",
	      [?MODULE, IPString]),
	    []
    end;

get_oap_ip_opt_list(_IPString) ->
    [].

%%% ----------------------------------------------------------
%%% @doc
%%% Get TN IP address
%%% @end
%%% ----------------------------------------------------------
-spec get_oap_ip_addr_alt() -> IpAddress::string().
get_oap_ip_addr_alt() ->
    case call(get_oap_ip_addr_alt) of
	IP when is_list(IP) ->
	    IP;
	_Error ->
	    ""
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% Get the name of the Network Namespace  used for OaM
%%% @end
%%% ----------------------------------------------------------
-spec get_oap_namespace() -> {ok, binary()} | {error, atom()}.
get_oap_namespace() ->
    call(?SERVER, get_oap_namespace).
    %% %% Temp fix to handle legacy
    %% case ootLib:is_rvnfm() of
    %% 	false ->
    %% 	    call(?CSTN_SERVER, get_oap_namespace);
    %% 	_True ->
    %% 	    call(?SERVER, get_oap_namespace)
    %% end.

%%% ----------------------------------------------------------
%%% @doc
%%% Get the name of the Alternative Network Namespace  used for OaM
%%% @end
%%% ----------------------------------------------------------
-spec get_oap_alt_namespace() -> {ok, binary()} | {error, atom()}.
get_oap_alt_namespace() ->
    call(?SERVER, get_oap_alt_namespace).
    %% %% Temp fix to handle legacy
    %% case ootLib:is_rvnfm() of
    %% 	false ->
    %% 	    call(?CSTN_SERVER, get_oap_alt_namespace);
    %% 	_True ->
    %% 	    call(?SERVER, get_oap_alt_namespace)
    %% end.

%%% ----------------------------------------------------------
%%% @doc
%%% Get an inet compatible optionlist to access the OaM network namespace.
%%% @end
%%% ----------------------------------------------------------
-spec get_oam_ns_opt_list() -> [] | [{netns, binary()}].
get_oam_ns_opt_list() ->
    case get_oap_namespace() of
        {ok, <<>>} -> [];
        {ok, Ns} -> [{netns, <<"/var/run/netns/", Ns/binary>>}];
        _Err -> []
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% Get an inet compatible optionlist to access the LMT network namespace.
%%% @end
%%% ----------------------------------------------------------
-spec get_lmt_ns_opt_list() -> [] | [{netns, binary()}].
get_lmt_ns_opt_list() ->
    case sysInitI:get_lmt_ns() of
	<<>> -> [];
	Ns -> [{netns, <<"/var/run/netns/", Ns/binary>>}]
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% Get an inet compatible optionlist to access network namespace for external
%%% communication. If OamAccessPoint is configured this returns the namespace
%%% for communicating using the OamAccessPoint, if the OamAccessPoint is unset,
%%% the namespace in use on LMT will be returned.
%%% @end
%%% ----------------------------------------------------------
-spec get_ns_opt_list() -> [] | [{netns, binary()}].
get_ns_opt_list() ->
    case get_oap_ip_addr() of
	[] ->
	    get_lmt_ns_opt_list();
	_ ->
	    get_oam_ns_opt_list()
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% Get a complete inet compatible optionlist for Oam access.
%%% Returns the concatenation of: get_ns_opt_list/0, get_oap_ip_opt_list/0,
%%% and get_dscp_opt_list/0.
%%% @end
%%% ----------------------------------------------------------
-spec get_all_oam_opt_list() -> list().
get_all_oam_opt_list() ->
    get_ns_opt_list() ++ get_oap_ip_opt_list() ++ get_dscp_opt_list().

%%% ----------------------------------------------------------
%%% @doc
%%% Perform host to ipv4-tuple conversion in relevant network namespace
%%% Resolution is always done in the Oam namespace, unless OamAccessPoint
%%% isn't set, in which case it resolves to IPv4 in default namespace.
%%% If the OamAccessPoint points to IPv6 a resolution to IPv6 will be
%%% returned, if OamAccessPoints points to IPv4  a IPv4 address will
%%% be returned.
%%% @end
%%% ----------------------------------------------------------
-spec getaddr(list()) -> {ok, tuple()} | {error, any()}.
getaddr(Host) ->
    ootResolver:getaddr(Host).


%%% ----------------------------------------------------------
%%% @doc
%%% Perform host to ipv4/6-tuple conversion in arbitrary network namespace.
%%% When successful a list with one or more IP-address tuples will be returned.
%%% @end
%%% ----------------------------------------------------------
-spec get_addresses(Host::list(), Inet::atom(), Namespace::list()) ->
	{ok, [tuple()]} | {error, any()}.
get_addresses(Host, Inet, Ns) ->
    ootResolver:get_addresses(Host, Inet, Ns).
%%% ----------------------------------------------------------
%%% @doc
%%% Perform host to ipv4-tuple conversion in relevant network namespace
%%% If Oam namespace is set use it, else use Lmt namespace,
%%% if Lmt namespace isn't in use resolve in defaultnamespace.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec getaddr(list(), inet) -> {ok, tuple()} | {error, any()}.
getaddr(Host, inet) ->
    %case {get_oap_ip_addr(), sysInitI:get_lmt_ns()} of
    case {get_oap_namespace(), sysInitI:get_lmt_ns()} of
	{{ok, <<_:8, _/binary>>}, _} ->
	    ootResolver:getaddr(Host, inet, oam);
	{_, <<_:8, _/binary>>} ->
	    ootResolver:getaddr(Host, inet, lmt);
	_ ->
	    inet:getaddr(Host, inet)
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% Perform host to ipv4-tuple conversion in Lmt network namespace
%%% If Lmt network namespace isn't used, resolve in default ns.
%%%
%%%
%%% @end
%%% ----------------------------------------------------------
-spec getaddr_lmt(list(), inet) -> {ok, tuple()} | {error, any()}.
getaddr_lmt(Host, inet) ->
    case sysInitI:get_lmt_ns() of
	<<>> ->
	    inet:getaddr(Host, inet);
	_Binary_ns ->
	    ootResolver:getaddr(Host, inet, lmt)
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% Perform host to ipv4-tuple conversion in OaM network namespace
%%% If Oam network namespace isn't used, resolve in default ns.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec getaddr_oam(list(), inet) -> {ok, tuple()} | {error, any()}.
getaddr_oam(Host, inet) ->
    case get_oap_namespace() of
	{ok, <<>>} ->
	    inet:getaddr(Host, inet);
	{ok, <<_>>} ->
	    ootResolver:getaddr(Host, inet, oam);
	_->
	    inet:getaddr(Host, inet)
    end.

%%% ----------------------------------------------------------
%%% @doc
%%% Get the DHCP IP address set on the LMT port
%%% @end
%%% ----------------------------------------------------------
-spec get_lmt_ipv4() -> tuple() | undefined.
get_lmt_ipv4() ->
    call(get_lmt_ipv4).

%%% ----------------------------------------------------------
%%% @doc
%%% Get the DHCP IP address and the mask-length set on the LMT port
%%% @end
%%% ----------------------------------------------------------
-spec get_lmt_ipv4_with_mask() -> {tuple(), integer()} |
				  {undefined, integer()} | {error, any()}.
get_lmt_ipv4_with_mask() ->
    call(get_lmt_ipv4_with_mask).

%%% ----------------------------------------------------------
%%% @doc
%%% Get fixed link-local IP address set on the LMT port
%%% @end
%%% ----------------------------------------------------------
%set in in OOT:etc/dhcpc_conf.sh
-spec get_lmt_ll_ipv4() -> tuple().
get_lmt_ll_ipv4() ->
    {169, 254, 2, 2}. %also in etc/dhcpc_conf.sh

%%% ----------------------------------------------------------
%%% @doc
%%% Get fixed link-local IP address and netmask set on the LMT port
%%% @end
%%% ----------------------------------------------------------
%set in OOT:etc/dhcpc_conf.sh
-spec get_lmt_ll_ipv4_with_mask() -> {{169, 254, 2, 2}, 16}.
get_lmt_ll_ipv4_with_mask() ->
    {{169, 254, 2, 2}, 16}.

%%% ----------------------------------------------------------
%%% @doc
%%% Inform OOT (ootSnmpProxy) about (new) agent adress
%%% @end
%%% ----------------------------------------------------------
%called from comsaServer
-spec snmp_agent_update(PL::list()) -> ok.
snmp_agent_update(PL) ->
    ootSnmpProxy:agent_update(PL).

%%% ----------------------------------------------------------
%%% @doc
%%% Inform OOT that APPM finished starting
%%% @end
%%% ----------------------------------------------------------
%called from appmServer
-spec restart_complete() -> ok.
restart_complete() ->
    ootResolver:restart_complete(),
    ok.

%%% ----------------------------------------------------------
%%% @doc
%%% Notify OOT about changed configuration
%%% @end
%%% ----------------------------------------------------------
-spec object_update_notify(Obj::string()) -> ok.
object_update_notify(Obj) ->
    gen_server:cast(?SERVER, {object_update_notify, Obj}).

%%% ----------------------------------------------------------
%%% @doc
%%% Remove duplicate references to OamAccessPoint
%%% @end
%%% ----------------------------------------------------------
-spec rm_duplicate_oap_ref(DNs::list(), Reserved::binary()) -> NewDns::list().
rm_duplicate_oap_ref(DNs, Reserved) ->
    lists:foldl(fun(DN, AccDNs) ->
			case lists:delete(DN, AccDNs) of
			    AccDNs ->
				AccDNs;
			    NewDNs ->
				RestDNs = lists:delete(DN, NewDNs),
				log_removed(Reserved, AccDNs, RestDNs),
				[DN | RestDNs]
			end
		end, DNs, [?OAP_DN, ?ALT_OAP_DN]).

%%% ----------------------------------------------------------
%%% @doc
%%% Get Name space and IP address for an MO ref
%%% @end
%%% ----------------------------------------------------------
-spec get_ns_and_ip(Obj::string()) ->
    {ok, NameSpace::list(), IpAddress::list()} |
	{error, Error::term()}.
get_ns_and_ip(Obj) ->
    ootCstn:get_ns_and_ip(Obj).

%%% ----------------------------------------------------------
%%% @doc
%%% Register a callback for name space and IP address updates
%%% for  an MO ref.
%%%
%%% If an MO ref is updated the following callback will be invoked
%%% CbModule:ip_changed(MoRef, NameSpace, IpAddress)
%%%
%%% Optionally it is also possible to include a list of extra arguments.
%%% Example:
%%%   register_ip_change_cb(MoRef, CbModule, ExtraArgs = [Arg1, Arg2])
%%%
%%% The callback implementation should then look like this:
%%% CbModule:ip_changed(MoRef, NameSpace, IpAddress, Arg1, Arg2)
%%%
%%% @end
%%% ----------------------------------------------------------
-spec register_ip_change_cb(MoRef::string(), CbModule::atom()) ->
    ok | {error, Error::term()}.
register_ip_change_cb(MoRef, CbModule) ->
    register_ip_change_cb(MoRef, CbModule, []).

-spec register_ip_change_cb(MoRef::string(), CbModule::atom(), 
			    ExtraArgs::list()) ->
				   ok | {error, Error::term()}.
register_ip_change_cb(MoRef, CbModule, ExtraArgs) ->
    ootCstn:register_ip_change_cb(MoRef, CbModule, ExtraArgs).

%%% ----------------------------------------------------------
%%% @doc
%%% Unregister a callback for name space and IP address updates
%%% for  an MO ref.
%%%
%%% @end
%%% ----------------------------------------------------------
-spec unregister_ip_change_cb(MoRef::string(), CbModule::atom()) ->
    ok | {error, Error::term()}.
unregister_ip_change_cb(MoRef, CbModule) ->
    unregister_ip_change_cb(MoRef, CbModule, []).


-spec unregister_ip_change_cb(MoRef::string(), CbModule::atom(), 
			      ExtraAttrs::list()) ->
				     ok | {error, Error::term()}.
unregister_ip_change_cb(MoRef, CbModule, ExtraAttrs) ->
    ootCstn:unregister_ip_change_cb(MoRef,  CbModule, ExtraAttrs).

%%% ----------------------------------------------------------
%%% @doc
%%% Checks if the attribute ipAddressChangeStatus in
%%% "ManagedElement=1,NodeSupport=1,OamIpSupport=1" is set to the value in
%%% parameter IpAddressChangeStatus
%%%
%%% @end
%%% ----------------------------------------------------------
-spec is_change_status_changed(IpAddressChangeStatus::integer()) -> boolean().
is_change_status_changed(IpAddressChangeStatus) ->
    ootModel:is_change_status_changed(IpAddressChangeStatus).

%%% ----------------------------------------------------------
%%% @doc
%%% Prototype function for registered ip change callback when ExtraArgs is used
%%%
%%% @end
%%% ----------------------------------------------------------
-spec ip_changed(Ldn::list(), NS::list(), IpAddr::list(), Pid::pid()) -> ok.
ip_changed(Ldn, NS, IpAddr, Pid) ->
    Pid ! {ip_changed, {Ldn, NS, IpAddr}},
    ok.

%%% ----------------------------------------------------------
%%% @doc
%%% Returns a fun/1 that can be used to register for notifications about config
%%% updates
%%% 
%%% @end
%%% ----------------------------------------------------------
-spec get_cfg_upd_cb_fun() -> Fun::fun((Config::list()) -> ok).
get_cfg_upd_cb_fun() ->
    get_cfg_upd_cb_fun(self()).

-spec get_cfg_upd_cb_fun(Pid::pid()) -> Fun::fun((Config::list()) -> ok).
get_cfg_upd_cb_fun(Pid) ->
    fun(Config) ->    
	    Pid ! {oot_config_update, Config},
	    ok
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
call(Call) ->
    call(?SERVER, Call).
call(Server, Call) ->
    try
	gen_server:call(Server, Call, ?CALL_TIMEOUT)
    catch
	_: {timeout, _} ->
	    {error, timeout};
	_:_ ->
	    {error, oot_not_started}
    end.


log_removed(Reserved, AccDNs, RestDNs) ->
    case length(AccDNs) - length(RestDNs) of
	L when L > 1 ->
	    sysInitI:info_msg("~p: Found duplicate reservedBy~n~s~n"
			      "~p~nRemove duplicate~n",
			      [?MODULE, ootLib:to_list(Reserved), AccDNs]);
	_ ->
	    ok
    end.


%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
