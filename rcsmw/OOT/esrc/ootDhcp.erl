%% ===========================================================================
%% Copyright (c) Ericsson AB 2013-2015 All rights reserved.
%%
%% The information in this document is the property of Ericsson.
%%
%% Except as specifically authorized in writing by Ericsson, the
%% receiver of this document shall keep the information contained
%% herein confidential and shall protect the same in whole or in
%% part from disclosure and dissemination to third parties.
%%
%% Disclosure and disseminations to the receivers employees shall
%% only be made on a strict need to know basis.
%% ===========================================================================
%% @copyright Ericsson AB 2014-2017
%% @doc
%% This module implements a very simple dhcp client.
%% @end
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R2A/1      2014-02-04 uabesvi     Stolen from NL
%%% R3A/3      2014-12-10 etxlg       Cleaned away crud and other more of same
%%% R3A/4      2015-02-10 etxlg       dhcp-client optionally runs in net_ns
%%% R3A/6      2015-03-31 etxlg       no patchdir if secure
%%% R4A/1      2015-09-24 eolaand     Change error_logger to sysInitI 
%%% R4A/2      2015-12-09 etxlg       Support new DHCP silliness (lab w/a)
%%% R5A/1      2015-12-15 etxlg       Branched, R4 will now be reverted
%%% ----------------------------------------------------------

-module(ootDhcp).
-vsn('/main/R2A/R3A/R4A/R5A/R9A/1').
-id('').
-date('2017-02-01').

-export([start_client/0]).
-export([init/2]).

%% for debug
-export([stop/0]).
-export([dhcp_wait/2]).  %to do code change during testing

-define(START_TO, 2000).
-define(SUDO, "/usr/bin/sudo").
-define(CB_SCRIPT, "dhcpc_conf.sh").
-define(DHCP_LG,   filename:join([code:priv_dir(oot), "etc", ?CB_SCRIPT])).
-define(UDHCPC_LG, ["udhcpc", "-f", "-i", "eth0", "-s"]).
-define(LEASED, "Lease of").

-record(binding, {ip,
		  mask,
		  subnet,
		  router,
		  dns,
		  nextserver,
		  lab_workaround = false
		 }).

-include("oot.hrl").

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%===========================================================================
%% @spec start_client()
%%
%%       -> {ok, Pid} | {error, Reason}
%%
%% where
%%
%%   Pid    = pid() 
%%   Reason = term()
%%
%% @doc
%% DHCP client controller.
%%
%% This is called from ootServer
%%
%% At init start a DHCP client and wait for bound.
%% A callback script, dhcp_script, is used to detect bound.
%% When bound the received IP address is written to ipconfic
%% (done in the script).
%% 
%% After that the DHCP client is restarted and is again waiting for bound.
%% @end
%%===========================================================================
start_client() -> 
    sc(sysEnv:target()).

sc(false) ->
    ok;
sc(true) ->
    Self = self(),
%%spawn_link so we don't lose it
    proc_lib:spawn_link(fun() -> init(Self, start) end),
    receive
	{start_client_res, Res} ->
	    Res
    after ?START_TO ->
	    {error, {timeout, {?MODULE, start_client}}}
    end.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%===========================================================================
%% init(Pid, Reason) -> ok
%% 
%%   Pid    = pid()             Pid to user process
%%   Reason = start | restart   start   = start timer for default address
%%                              restart = default address just written
%% 
%% Process controlling DHCP.
%% 
%%===========================================================================
%% @private
init(Pid, Reason) ->
    true = register(?MODULE, self()), %make is possible to find during debug
    Pid ! {start_client_res, {ok, self()}},
    Port = dhcp_open(Reason),
    ?LOG_INFO("Started."),
    dhcp_wait(Port, #binding{}).

stop() ->
    ?MODULE ! stop.
    
%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%===========================================================================
%% open an Erlang port and start the Busybox DHCP client.
%%===========================================================================
dhcp_open(_Reason) ->
    Cmd    = os:find_executable("sudo"),
    Callback = callback_filepath(sysInitI:is_secure()),
    Args =
	case sysInitI:get_lmt_ns() of
	    <<>> -> %not namespaced
                 ?UDHCPC_LG ++ [Callback];
	    Ns_bin -> %may need some path to these cmds...
		["ip", "netns", "exec", binary_to_list(Ns_bin)] ++
		 ?UDHCPC_LG ++ [Callback]
	end,
    open_port({spawn_executable, Cmd},
                 [stream, exit_status, binary, stderr_to_stdout,
                 {cd, "/"},
                 {args, Args}]).

%%===========================================================================
%% Wait for messages from the client, 
%% or the timer to set default address to expire.
%%===========================================================================
dhcp_wait(Port, Binding) ->
    receive
	{Port, {data, Data}} ->
 	    Lines    = binary:split(Data, <<"\n">>, [trim, global]),
	    New_binding = dhcp_msg(Lines, Binding),
	    dhcp_wait(Port, New_binding);
	{Port, {exit_status, Rc}} ->
	    ?LOG_WARNING("dhcp-client exit_status ~p, Binding: ~p", [Rc, Binding]),
	    sysInitI:warning_msg(
		"~p: dhcp-client exit_status: ~p, Binding: ~p~n",
		[?MODULE, Rc, Binding]),
	    ootServer:set_lmt_ip(undefined),
	    safe_port_close(Port),
	    exit_status;
	dump ->
	    ?LOG_INFO("Port: ~p, Binding: ~p", [Port, Binding]),
	    sysInitI:info_msg("~p: Port: ~p, Binding: ~p~n", [?MODULE, Port, Binding]),
	    ?MODULE:dhcp_wait(Port, Binding);
	stop ->
	    ?LOG_INFO("exit by command"),
	    sysInitI:info_msg("~p: exit by command~n", [?MODULE]),
	    exit(normal);
	_ ->
	    dhcp_wait(Port, Binding)
    end.

%%===========================================================================
%% Check if bound
%% This works because there are printouts (echo) added to the dhcp-config-file
%% extract the IP  10.86.148.49 convert to tuple and send to the ootServer
%% extract the mask (integer()) send to ootServer (ultimately used by sysSftp)
%%===========================================================================
dhcp_msg([], Binding) ->
    Binding;
dhcp_msg([Msg | T], Binding) ->
    New_binding =
	try parse_dhcp_line(binary:split(Msg, <<"<space>">>, []), Binding) of
	    Nb -> Nb
	catch
	    _:_ -> Binding
	end,
    ?LOG_INFO("NewBinding: ~p", [New_binding]),
    dhcp_msg(T, New_binding).

parse_dhcp_line([<<"#BOUND_IP#">>, Ip], Binding) ->
    case inet:parse_address(binary_to_list(Ip)) of
	{ok, IPtuple} ->
	    ootServer:set_lmt_ip(IPtuple),
	    Binding#binding{ip = IPtuple};
	_Err ->
	    ?LOG_WARNING("failed parsing IP from DHCP: ~p: ~p", [Ip, _Err]),
	    sysInitI:warning_msg(
	      "~p: failed parsing IP from DHCP: ~p: ~p~n",
	      [?MODULE, Ip, _Err]),
	    Binding
    end;
parse_dhcp_line([<<"#MASK#">>, Mask], Binding) ->
    M = list_to_integer(binary_to_list(Mask)),
    ootServer:set_lmt_mask(M),
    Binding#binding{mask = M};
parse_dhcp_line([<<"#SUBNET#">>, Subnet], Binding) ->
    case inet:parse_address(binary_to_list(Subnet)) of
	{ok, IPtuple} ->
	    Binding#binding{subnet = IPtuple};
	_Err ->
	    Binding
    end;
parse_dhcp_line([<<"#ROUTER#">>, Router], Binding) ->
    case inet:parse_address(binary_to_list(Router)) of
	{ok, IPtuple} ->
	    Binding#binding{router = IPtuple};
	_Err ->
	    Binding
    end;
parse_dhcp_line([<<"#DNS#">>, DNS_list], Binding) ->
	    Binding#binding{dns = DNS_list};
parse_dhcp_line([<<"#NEXTSERVER#">>, Next_server], Binding) ->
    case inet:parse_address(binary_to_list(Next_server)) of
	{ok, IPtuple} ->
	    Binding#binding{nextserver = IPtuple};
	_Err ->
	    Binding
    end;
parse_dhcp_line([<<"#LAB_WORKAROUND#">>], Binding) ->
    Router = safe_ip_to_string(Binding#binding.router),
    ?LOG_INFO("Adding routing for lab workaround on LMT:~n"
	      "route: 128.0.0.0/1 -> ~s~n"
	      "route: 0.0.0.0/1 -> ~s~n",
	      [Router, Router]),
    sysInitI:info_msg("~p: Adding routing for lab workaround on LMT:~n"
		      "route: 128.0.0.0/1 -> ~s~n"
		      "route: 0.0.0.0/1 -> ~s~n",
		      [?MODULE, Router, Router]),
    Binding#binding{lab_workaround = true};
parse_dhcp_line([<<"#DECONFIG#">>], Binding) ->
    case Binding#binding.ip of
	undefined -> %%no ip this is probably first time startup
	    ok;
	_ ->
	    ?LOG_INFO("DHCP address removed(deconfig) from LMT"),
   	    sysInitI:info_msg("~p: DHCP address removed(deconfig) from LMT~n", [?MODULE]),
	    ootServer:set_lmt_ip(undefined)
    end,
    #binding{};
parse_dhcp_line([<<"#LEASEFAIL#">>], _Binding) ->
    ?LOG_INFO("DHCP address removed(leasefail) from LMT"),
    sysInitI:info_msg("~p: DHCP address removed(leasefail) from LMT~n", [?MODULE]),
    ootServer:set_lmt_ip(undefined),
    #binding{};
parse_dhcp_line([<<"#END#">>], Binding) ->
    ?LOG_INFO("Obtained DHCP lease on LMT~n"
	      "IP         = ~p~n"
	      "Mask       = ~p~n"
	      "Subnet     = ~p~n"
	      "Router     = ~p~n"
	      "DNS        = ~p~n"
	      "Nextserver = ~p",
	      [Binding#binding.ip,
	       Binding#binding.mask,
	       Binding#binding.subnet,
	       Binding#binding.router,
	       Binding#binding.dns,
	       Binding#binding.nextserver]),
    sysInitI:info_msg("~p: Obtained DHCP lease on LMT:~n"
		      "IP: ~p~n"
		      "Mask: ~p~n"
		      "Subnet: ~p~n"
		      "Router: ~p~n"
		      "DNS: ~p~n"
		      "Nextserver: ~p~n",
		      [?MODULE,
		       Binding#binding.ip,
		       Binding#binding.mask,
		       Binding#binding.subnet,
		       Binding#binding.router,
		       Binding#binding.dns,
		       Binding#binding.nextserver]),
    Binding;
parse_dhcp_line(_What, Binding) ->
    %%%io:format("~p: Got: ~p~n", [?MODULE, _What]),
    %%%io:format("~p: Binding: ~p~n", [?MODULE, Binding]),
    Binding.

safe_ip_to_string(Ip_tuple) when is_tuple(Ip_tuple) ->
    case inet:ntoa(Ip_tuple) of
	{error, _} -> "error converting IP";
	String -> String
    end;
safe_ip_to_string(undefined) ->
    "no gateway";
safe_ip_to_string(_) ->
    "unknown".


%%% #---------------------------------------------------------
%%% #3.4   MISC FUNCTIONS
%%% #---------------------------------------------------------

%%===========================================================================
%% close the port
%%===========================================================================
safe_port_close(Port) -> 
    try port_close(Port) 
    catch _:_ -> ok 
    end.

callback_filepath(true) ->
    ?DHCP_LG;
callback_filepath(false) ->
    Patched = filename:join([sysEnv:dev_patches_dir(), ?CB_SCRIPT]),
    case  file:read_file_info(Patched) of
	{ok, _} -> Patched;
	_ -> ?DHCP_LG
    end.
