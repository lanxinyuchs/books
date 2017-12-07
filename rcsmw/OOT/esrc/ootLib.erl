%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ootLib.erl %
%%% Author:	eolaand
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
%%% @private
-module(ootLib).
-id('Updated by CCase').
-vsn('/main/R4A/R5A/R6A/R7A/R9A/R10A/R11A/R12A/1').
-date('2017-10-30').
-author('eolaand').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	ootLib.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2015-2017 All rights reserved.
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
%%% R4A/1      2015-09-01 eolaand     Created
%%% R7A/1      2016-11-01 etxpeno     remove to_ipv4_ecim_dn/1
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([get_oap_ecim_obj_ids/1]).
-export([verify_port_config_change/1]).
-export([imm_attr_vals_to_config/1,
	 config_to_imm_attr_vals/1]).
-export([config_to_ecim_oap_attr_vals/1]).
-export([config_to_ecim_set_mo_attrs/1]).
-export([imm_to_ecim_oap_attr_vals/1]).
-export([imm_attr_val_to_conf_val/1]).
%% -export([ecim_attr_val_to_conf_val/1]).
-export([port_type_to_attr/1]).
-export([imm_dn_to_ecim_dn_oap/1,
	 ecim_dn_to_imm_dn_oap/1]).


-export([get_ip_addresses/0,
	 get_ip_addresses/1]).


-export([is_target/0,
	 is_host_sim/0,
	 is_arm_wr6/0,
	 is_arm/0,
	 is_wr6/0,
	 is_vrcs/0,
	 is_rvnfm/0,
	 is_ipv4_addr/1,
	 is_ipv6_addr/1]).

-export([port_type_to_conf_type/1,
	 conf_type_to_port_type/1,
	 get_ip_addr_from_conf/2,
	 imm_to_dn/1,
	 get_lmt_ipv4/2,
	 get_lmt_ipv4_mask/2,
	 get_first_ipv4_addr/1,
	 get_first_ipv6_global_addr/1]).

-export([to_bin/1,
	 to_list/1]).
	 
-export([
        log_versions/0,
        get_versions/1        
        ]).

%% -compile(export_all).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%% -export([internal_function1/2]).

%%% ----------------------------------------------------------
%%% Include files
%%% ----------------------------------------------------------
%% -include("om.hrl").
-include("oot.hrl").

%%% ----------------------------------------------------------
%%% Macros
%%% ----------------------------------------------------------
-define(OAP_TYPE_TO_STRING(OapType),
	case OapType of
	    ?CNF_CLI_PORT ->
		"CLI";
	    ?CNF_NETCONF_PORT ->
		"Netconf"
	end).

-define(BUSY_PORT_WARNING(Port, OapType),
	to_list(?MODULE) ++ ": Rejected attempt to set busy port "
	++ to_list(Port) ++ " as " ++ ?OAP_TYPE_TO_STRING(OapType)
	++ " Port~n").

%%% ----------------------------------------------------------
%%% Records
%%% ----------------------------------------------------------


%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%%     get_oap_ecim_obj_ids(DNs) -> {ok, Ids::list()}  |
%%%                                  {error, Reason::atom()}.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
get_oap_ecim_obj_ids(DNs) ->
    {atomic, Ids} =
	mnesia:transaction(fun() ->
				   [gmfTrService:get_object_id(DN) ||
				       DN <- DNs]
			   end),
    Ids.

%%% ----------------------------------------------------------
%%%     verify_config_change() -> ok | {error, Reason::atom()}.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
verify_port_config_change(Config) ->
    PortConfig = get_port_config(Config),
    case check_duplicate_val(PortConfig) of
	ok ->
	    verify_port_free(PortConfig);
	Error ->
	    Error
    end.


verify_port_free(Config) ->
    CachedConf = ootServer:get_cached_config(),
    OldPortConf = get_port_config(CachedConf),
    ChangedPortConf = [Par || {Type, Val} = Par <- Config,
			      proplists:get_value(Type, OldPortConf) =/= Val],
    {ok, PortConf} = sysEnv:get_initial_port_conf(),
    Ports = [Port || {Type, Port} <- OldPortConf ++ PortConf,
		     Type =/= netconf, Type =/= cli],
    %% This check will fail if netconf and cli ports changes place in one
    %% transaction. It could be allowed but will probably just cause problems.
    F = fun({OapType, Val}) ->
		case lists:member(Val, Ports) of
		    true ->
			sysInitI:warning_msg(?BUSY_PORT_WARNING(Val, OapType)),
			false;
		    _False ->
			true
		end
	end,
    case lists:dropwhile(F, ChangedPortConf) of
	[] ->
	    ok;
	[{_Type, Port} | _] ->
	    {error, {port_busy, Port}}
    end.


check_duplicate_val([{_Type, Val} | KeyVals]) ->
    case lists:keymember(Val, 2, KeyVals) of
	true ->
	    sysInitI:warning_msg("~w: Rejected attempt to set port ~w as both"
				 " CLI and Netconf port~n", [?MODULE, Val]),
	    {error, {duplicate_val, Val}};
	_False ->
	    check_duplicate_val(KeyVals)
    end;

check_duplicate_val([]) ->
    ok.


get_port_config(Config) ->
    [Par || {Type, _Val} = Par <- Config, lists:member(Type, ?CNF_PORTS)].

%%% ----------------------------------------------------------
%%%     attr_vals_to_config() -> list()
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
imm_attr_vals_to_config(AttrVals) ->
    [{attr_to_conf_key(Attr), imm_attr_val_to_conf_val(Val)} ||
	{Attr, Val} <- lists:ukeysort(1, AttrVals),
	Attr =/= ?OAP_IPV4_ADDR, Attr =/= ?OAP_ACC_POINT].


attr_to_conf_key(?OAP_SSH_PORT) ->
    ?CNF_CLI_PORT;
attr_to_conf_key(?OAP_NETCONF_PORT) ->
    ?CNF_NETCONF_PORT;
attr_to_conf_key(?OAP_DSCP) ->
    ?CNF_DSCP;
attr_to_conf_key(?TN_IP_ADDRESS) ->
    ?CNF_ACC_POINT_ADDR;
attr_to_conf_key(?TN_5G_USED_IP_ADDRESS) ->
    ?CNF_ACC_POINT_ADDR.


imm_attr_val_to_conf_val([Val]) when is_binary(Val) ->
    binary_to_list(Val);
imm_attr_val_to_conf_val([Val]) ->
    Val;
imm_attr_val_to_conf_val([]) ->
    undefined.


%%% ----------------------------------------------------------
%%%     config_to_imm_attr_vals() -> list()
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
config_to_imm_attr_vals(Config) ->
    [ootImmLib:build_attr_val(conf_key_to_attr(Key), uint32, Val) ||
	{Key, Val} <- Config,
	lists:member(Key, [?CNF_NETCONF_PORT, ?CNF_CLI_PORT, ?CNF_DSCP])].


conf_key_to_attr(?CNF_CLI_PORT) ->
    ?OAP_SSH_PORT;
conf_key_to_attr(?CNF_NETCONF_PORT) ->
    ?OAP_NETCONF_PORT;
conf_key_to_attr(?CNF_DSCP) ->
    ?OAP_DSCP;
conf_key_to_attr(?CNF_IPV4_ADDR) ->
    ?TN_IPV4_ADDR;
conf_key_to_attr(?CNF_ACC_POINT_ADDR) ->
    ?TN_IP_ADDRESS.


port_type_to_attr(netconf) ->
    ?OAP_NETCONF_PORT;
port_type_to_attr(cli) ->
    ?OAP_SSH_PORT.


%%% ----------------------------------------------------------
%%%     config_to_ecim_oap_attr_vals() -> list()
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
config_to_ecim_oap_attr_vals(Config) ->
    [config_to_ecim_oap_attr_val(Item) || Item <- Config].


config_to_ecim_oap_attr_val({?CNF_CLI_PORT, Val}) ->
    {?OAP_SSH_PORT, ?ECIM_UINT16_VAL(Val)};

config_to_ecim_oap_attr_val({?CNF_NETCONF_PORT, Val}) ->
    {?OAP_NETCONF_PORT, ?ECIM_UINT16_VAL(Val)};

config_to_ecim_oap_attr_val({?CNF_DSCP, Val}) ->
    {?OAP_DSCP, ?ECIM_UINT8_VAL(Val)}.


%%% ----------------------------------------------------------
%%%     imm_to_ecim_oap_attr_vals() -> list()
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
imm_to_ecim_oap_attr_vals(IMMAttrVals) ->
    [imm_to_ecim_oap_attr_val({Attr, imm_to_ecim_val(Val)}) ||
	{Attr, Val} <- remove_type_from_attr_vals(IMMAttrVals)].


imm_to_ecim_oap_attr_val({Attr, Val})
  when Val =:= undefined; Val =:= [] ->
    {Attr, Val};

imm_to_ecim_oap_attr_val({?OAP_IPV4_ADDR, Val}) ->
    {?OAP_IPV4_ADDR, ?ECIM_REF_VAL(Val)};

imm_to_ecim_oap_attr_val({?OAP_ACC_POINT, Val}) ->
    {?OAP_ACC_POINT, ?ECIM_REF_VAL(Val)};

imm_to_ecim_oap_attr_val({?OAP_SSH_PORT, Val}) ->
    {?OAP_SSH_PORT, ?ECIM_UINT16_VAL(Val)};

imm_to_ecim_oap_attr_val({?OAP_NETCONF_PORT, Val}) ->
    {?OAP_NETCONF_PORT, ?ECIM_UINT16_VAL(Val)};

imm_to_ecim_oap_attr_val({?OAP_DSCP, Val}) ->
    {?OAP_DSCP, ?ECIM_UINT8_VAL(Val)}.


imm_to_ecim_val([Val]) ->
    Val;

imm_to_ecim_val([]) ->
    undefined;

imm_to_ecim_val(undefined) ->
    undefined;

imm_to_ecim_val(Val) ->
    Val.


remove_type_from_attr_vals(AttrVals) ->
    lists:map(fun({Attr, _Type, Val}) ->
		      {Attr, Val};
		 ({Attr, Val}) ->
		      {Attr, Val}
	      end, AttrVals).


%%% ----------------------------------------------------------
%%%     config_to_ecim_set_mo_attrs() -> list()
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
config_to_ecim_set_mo_attrs(Config) ->
    lists:flatmap(fun(Item) ->
			  config_to_ecim_set_mo_attr(Item)
		  end, Config).


config_to_ecim_set_mo_attr({?CNF_NETCONF_PORT, Val})
  when is_integer(Val) ->
    [{?ECIM_NETCONF_SSH_DN, [{?ECIM_PORT, ?ECIM_UINT16_VAL(Val)}]}];

config_to_ecim_set_mo_attr({?CNF_CLI_PORT, Val})
  when is_integer(Val) ->
    [{?ECIM_CLI_SSH_DN, [{?ECIM_PORT, ?ECIM_UINT16_VAL(Val)}]}];

config_to_ecim_set_mo_attr({?CNF_DSCP, Val})
  when is_integer(Val) ->
    [{?ECIM_OAM_TR_CLASS_DN, [{?ECIM_DSCP, ?ECIM_UINT8_VAL(Val)}]}];

config_to_ecim_set_mo_attr(_) ->
    [].


imm_dn_to_ecim_dn_oap(?OAP_DN) ->
    ?ECIM_DN_OAP;
imm_dn_to_ecim_dn_oap(?ALT_OAP_DN) ->
    ?ECIM_DN_ALT_OAP.


ecim_dn_to_imm_dn_oap(?ECIM_DN_OAP) ->
    ?OAP_DN;
ecim_dn_to_imm_dn_oap(?ECIM_DN_ALT_OAP) ->
    ?ALT_OAP_DN.

%%% ----------------------------------------------------------
%%%     get_ip_addresses(Dev) -> {ok, {IPv6Addr, IPv4Addr}}.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
get_ip_addresses(Device) ->
    {ok, IfList} = inet:getifaddrs(),
    DevAddrs = proplists:get_value(Device, IfList, []),
    case proplists:get_all_values(addr, DevAddrs) of
	[] ->
	    ?LOG_INFO("Device ~p: No IP addresses found ~n", [Device]),
	    timer:sleep(1000),
	    get_ip_addresses(Device);
	IpAddresses ->
	    ?LOG_INFO("Device ~p: IP addresses: ~p~n", [Device, IpAddresses]),
	    IPv6Addr = get_first_ipv6_global_addr(IpAddresses),
	    IPv4Addr = get_first_ipv4_addr(IpAddresses),
	    case {IPv6Addr, IPv4Addr} of
		{[], []} ->
		    ?LOG_INFO("No usable IP addresses found~n"),
		    timer:sleep(1000),
		    get_ip_addresses(Device);
		_ ->
		    ?LOG_INFO("Device ~p: IPv6Addr:~p IPv4Addr:~p~n",
			      [Device, IPv6Addr, IPv4Addr]),
		    {ok, {IPv6Addr, IPv4Addr}}
	    end
    end.


get_ip_addresses() ->
    get_ip_addresses("eth0").


%%% ----------------------------------------------------------
%%%     is_target() -> true | false.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
is_target() ->
    sysEnv:target().


%%% ----------------------------------------------------------
%%%     is_target() -> true | false.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
is_host_sim() ->
    sysEnv:rcs_mode_3() =:= hostsim.


%%% ----------------------------------------------------------
%%%     is_arm_wr6() -> true | false.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
is_arm_wr6() ->
    is_arm() andalso is_wr6().


%%% ----------------------------------------------------------
%%%     is_arm() -> true | false.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
is_arm() ->
    case sysEnv:architecture() of
	{"arm", _} ->
	    true;
	_ ->
	    false
    end.


%%% ----------------------------------------------------------
%%%     is_wr6() -> true | false.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
is_wr6() ->
    case sysEnv:target_bin_dir() of
	"tgt_arm-wr6/bin" ->
	    true;
	_ ->
	    false
    end.


%%% ----------------------------------------------------------
%%%     is_vrcs() -> true | false.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
is_vrcs() ->
    %% not sysEnv:vrcs().
    sysEnv:vrcs().

%%% ----------------------------------------------------------
%%%     is_rvnfm() -> true | false.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
is_rvnfm() ->
    %% not case swmI:node_type() of
    case swmI:node_type() of
	"R-VNFM" ->
	    true;
	_ ->
	    false
    end.


%%% ----------------------------------------------------------
%%%     is_ipv4_addr(IpAddr) -> true | false.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
is_ipv4_addr(IpAddr) when is_list(IpAddr) ->
    case inet:parse_ipv4strict_address(IpAddr) of
	{ok, _Addr} ->
	    true;
	_Error ->
	    false
    end.

%%% ----------------------------------------------------------
%%%     is_ipv6_addr(IpAddr) -> true | false.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
is_ipv6_addr(IpAddr) when is_list(IpAddr) ->
    case inet:parse_ipv6strict_address(IpAddr) of
	{ok, _Addr} ->
	    true;
	_Error ->
	    false
    end.


%%% ----------------------------------------------------------
%%%     port_type_to_conf_type(SysType) -> CnfType
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
port_type_to_conf_type(?SYS_CLI_PORT) ->
    ?CNF_CLI_PORT;

port_type_to_conf_type(?SYS_NETCONF_PORT) ->
    ?CNF_NETCONF_PORT;

port_type_to_conf_type(UnknownType) ->
    UnknownType.


%%% ----------------------------------------------------------
%%%     conf_type_to_port_type(CnfType) -> SysType
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
conf_type_to_port_type(?CNF_CLI_PORT) ->
    ?SYS_CLI_PORT;

conf_type_to_port_type(?CNF_NETCONF_PORT) ->
    ?SYS_NETCONF_PORT;

conf_type_to_port_type(UnknownType) ->
    UnknownType.

%%% ----------------------------------------------------------
%%%     get_ip_addr_from_conf(Type, Config) -> IpAddr::string()
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
get_ip_addr_from_conf(Type, Config) ->
    case proplists:get_value(Type, Config) of
	IPStr when is_list(IPStr), IPStr =/= [] ->
	    [IPAddr | _] = string:tokens(IPStr, "/"),
	    IPAddr;
	_ ->
	    ""
    end.

%%% ----------------------------------------------------------
%%%     imm_to_dn(ImmDN) -> MimDN.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
imm_to_dn(<<>>) ->
    undefined;

imm_to_dn(undefined) ->
    undefined;

imm_to_dn(Imm_dn) ->
    case gmfI:imm_to_mim(Imm_dn) of
	{ok, Mim_dn} ->
	    Mim_dn;
	_Err ->
	    ?LOG_ERROR("Failed to convert IMM-DN: ~p -> ~p",
		       [Imm_dn, _Err]),
	    sysInitI:error_msg("~p: Failed to convert IMM-DN: ~p -> ~p~n",
				   [?MODULE, Imm_dn, _Err]),
	    undefined
    end.

%%% ----------------------------------------------------------
%%%     get_lmt_ipv4(IsVrcs, IpAddr) -> IpAddr.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
get_lmt_ipv4(true, undefined) ->
    Eth0 = get_eth0(inet:getifaddrs()),
    get_addr(Eth0);

get_lmt_ipv4(_, Addr) ->
    Addr.

%%% ----------------------------------------------------------
%%%     get_lmt_ipv4_mask(IsVrcs, IpAddrMask) -> IpAddrMask.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
get_lmt_ipv4_mask(true, undefined) ->
    Eth0 = get_eth0(inet:getifaddrs()),
    get_mask(Eth0);

get_lmt_ipv4_mask(_, Addr) ->
    Addr.


%%% ----------------------------------------------------------
%%%     get_first_ipv4_addr(IfConfig) -> IpAddr.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
get_first_ipv4_addr([]) ->
    [];

get_first_ipv4_addr([Tuple | _]) when size(Tuple) == 4 ->
    inet:ntoa(Tuple);

get_first_ipv4_addr([_ | T]) ->
    get_first_ipv4_addr(T).


%%% ----------------------------------------------------------
%%%     get_first_ipv6_global_addr(IfConfig) -> IpAddr.
%%%
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
get_first_ipv6_global_addr([]) ->
    [];

get_first_ipv6_global_addr([Tuple | T]) when size(Tuple) > 4 ->
    case element(1, Tuple) of
	16#fe80 ->
	    get_first_ipv6_global_addr(T);
	_ ->
	    inet:ntoa(Tuple)
    end;

get_first_ipv6_global_addr([_ | T]) ->
    get_first_ipv6_global_addr(T).


%%% ----------------------------------------------------------
%%%            to_bin(Name) -> binary().
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
to_bin(Bin) when is_binary(Bin) ->
    Bin;

to_bin(List) when is_list(List) ->
    iolist_to_binary([List]);

to_bin(A) when is_atom(A) ->
    to_bin(to_list(A)).

%%% ----------------------------------------------------------
%%%            to_list(Name) -> list().
%%% Input:
%%% Output:
%%% Exceptions:
%%% Description:
%%% ----------------------------------------------------------
to_list(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);

to_list(List) when is_list(List) ->
    List;

to_list(undefined) ->
    [];

to_list(A) when is_atom(A) ->
    atom_to_list(A);

to_list(I) when is_integer(I) ->
    integer_to_list(I).

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%Print module versions    
log_versions() ->
    VersionList = get_versions(oot),
    FilteredVersionList = lists:foldl(fun(X, Result) -> case X of "\n" -> 
        Result ++ [X,"\t"]; _ -> Result ++ [X] end end, [], VersionList),
	?LOG_INFO("~n==========================~n~s~n==========================", 
	    io_lib:format("~s", [FilteredVersionList])).
	    
%%========================================================================
%% get_versions(Prefix, Modules) -> ok.
%% 
%%========================================================================
get_versions(Prefix) ->
    Versions = get_version_attributes(Prefix),
    Text = "Versions:~n" ++ string:join(lists:reverse(Versions), "~n") ++ "~n",
    io_lib:format(Text, []).
    
%%===========================================================================
%% get_version_attributes(Prefix) -> string()
%%===========================================================================
get_version_attributes(Prefix) ->
    vsn_attrs(modules(Prefix),
	      fun(W, M) -> ltf_vsn(W, M, vsn, fun vsn/1) end).

vsn_attrs(Modules, Fun)
  when is_list(Modules) ->
    W = 2 + widest(Modules),
    lists:foldl(fun(M, Acc) -> S = Fun(W, M), [S | Acc]  end,
		[], 
		Modules).		
		
ltf_vsn(Width, Mod, Attr, VFun) ->
    Time = attr(Mod, date),
    Str  = io_lib:format(": ~*s ~s | ~s", 
			 [-Width, Mod, Time, attr(Mod, Attr, VFun)]),
    lists:flatten(Str).    

attr(Mod, Attr, VFun) ->
    try
	VFun(val(Attr, keyfetch(Attr, Mod:module_info(attributes))))
    catch
	_:_ ->
	    "-"
    end.    

attr(Mod, Attr) ->
    attr(Mod, Attr, fun attr/1).

attr(T) when is_atom(T) ->
    atom_to_list(T);
attr(N) when is_integer(N) ->
    integer_to_list(N);
attr(V) ->
    case is_list(V) andalso lists:all(fun is_char/1, V) of
        true ->  %% string
            V;
        false ->
            io_lib:format("~p", [V])
    end.    
    
modules(Prefix)
  when is_atom(Prefix) ->
    lists:sort(mods(Prefix));
modules(Prefixes)
  when is_list(Prefixes) ->
    lists:sort(lists:flatmap(fun modules/1, Prefixes)).    

mods(Prefix) ->
    P = atom_to_list(Prefix),
    lists:filter(fun(M) ->
                         lists:prefix(P, atom_to_list(M))
                 end,
                 erlang:loaded()).
                 		
%% widest/1

widest(List) ->
    lists:foldl(fun widest/2, 0, List).

widest(T, Max)
  when is_atom(T) ->
    widest(atom_to_list(T), Max);

widest(T, Max)
  when is_integer(T) ->
    widest(integer_to_list(T), Max);

widest(T, Max)
  when is_list(T) ->  %% string
    max(length(T), Max).   

keyfetch(Key, List) ->
    {Key, V} = lists:keyfind(Key, 1, List),
    V.

val(_, [V]) ->
    V.    

is_char(C) ->
    0 =< C andalso C < 256.  

vsn("/main/" ++ V) ->
    V;

vsn(T) when is_atom(T) ->
    vsn(atom_to_list(T)).

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
get_mask(undefined) ->
    undefined;

get_mask([]) ->
    undefined;

get_mask([{addr, {A, B, C, D}} | _]) ->
    Mask = lists:flatten([hd(io_lib:format("~.2B", [X])) || X <- [A, B, C, D]]),
    case string:str(Mask, "0") of
	0 -> 32;
	N -> N - 1
    end;

get_mask([_ | T]) ->
    get_mask(T).


get_eth0({ok, List}) ->
    proplists:get_value("eth0", List);

get_eth0(_) ->
    undefined.

get_addr(undefined) ->
    undefined;

get_addr([]) ->
    undefined;

get_addr([{addr, Tuple} | _]) when size(Tuple) == 4 ->
    Tuple;

get_addr([_ | T]) ->
    get_addr(T).

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
