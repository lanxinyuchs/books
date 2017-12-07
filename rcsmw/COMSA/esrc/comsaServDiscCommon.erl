%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaServDiscCommon.erl %
%%% @author qostjoa
%%% @copyright Ericsson AB 2017
%%% @version /main/R12A/2

%%% @doc ==Service Discovery client starter== 
%%% This module implements the startup of the Consul agents

-module(comsaServDiscCommon).
-vsn('/main/R12A/2').
-date('2017-11-27').
%%% ----------------------------------------------------------
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
%%% 
%%% The information in this document is the property of Ericsson.
%%% 
%%% Except as specifically authorized in writing by Ericsson, the 
%%% receiver of this document shall keep the information contained 
%%% herein confidential and shall protect the same in who or in 
%%% part from disclosure and dissemination to third parties.
%%% 
%%% Disclosure and disseminations to the receivers employees shall 
%%% only be made on a strict need to know basis.
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name     What
%%% -----      ---------  ------   ------------------------
%%% R12A/1     2017-11-20 qostjoa  Created

%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% -include("comsaServDiscCommon.hrl").
%% -include("RmeSds.hrl").

%% -export([get_consul_binary/0, get_vsdupgrade_binary/0,
%% 	has_consul/0, has_vsdupgrade/0, do_vsd_command/1]).
%% -export([dns_check/2, dns_poll/3]).
%% -export([get_oam_ip/0, appm_start/4]).
%% -export([cluster_retry_join/2, cluster_retry_join/3,
%% 	client_cmd/5, client_cmd/6, server_cmd/3]).
%% -export([write_mom_data_file/2, create_config_file/7, create_domain_file/3]).
%% -export([get_dc_host_and_port/1, map_localhost/1, log_wrapper/0, get_ns_and_ip/1]).
%% %%% ----------------------------------------------------------

%% get_consul_binary() ->
%%     case appmAppData:get_lm_start_data_from_name("consul") of
%% 	{ok, _NameCxc, CxpPath, BinFile, _Info, _Pgroup} ->
%% 	    filename:join([CxpPath,BinFile]);
%% 	_ ->
%% 	    case file:read_file_info("/usr/bin/consul") of
%% 		{ok,_} ->
%% 		    "/usr/bin/consul";
%% 		_ ->
%% 		    {error,file_not_found}
%% 	    end
%%     end.

%% %%% ----------------------------------------------------------

%% get_vsdupgrade_binary() ->
%%     case appmAppData:get_lm_start_data_from_name("vsdupgrade") of
%% 	{ok, _NameCxc, CxpPath, BinFile, _Info, _Pgroup} ->
%% 	    filename:join([CxpPath,BinFile]);
%% 	_ ->
%% 	    case file:read_file_info("/usr/bin/vsdupgrade") of
%% 		{ok,_} ->
%% 		    swmI:find_file("/usr/bin/vsdupgrade" );
%% 		_ ->
%% 		    false
%% 	    end
%%     end.
   
%% %%% ----------------------------------------------------------

%% has_consul() ->
%%     case file:read_file_info("/usr/bin/consul") of
%% 	{ok,_} ->
%% 	    true;
%% 	_ ->
%% 	    case filelib:wildcard(filename:join(
%% 				    [sysEnv:home_dir(),"software/SDEE-*"])) of
%% 		[] ->
%% 		    false;
%% 		_ ->
%% 		    true
%% 	    end
	    
%%     end.

%% %%% ----------------------------------------------------------

%% has_vsdupgrade() ->
%%     case file:read_file_info("/usr/bin/vsdupgrade") of
%% 	{ok,_} ->
%% 	    true;
%% 	_ ->
%% 	    case filelib:wildcard(filename:join(
%% 				    [sysEnv:home_dir(),"software/SDEE-*"])) of
%% 		[] ->
%% 		    false;
%% 		_ ->
%% 		    true
%% 	    end
%%     end.

%% %%% ----------------------------------------------------------

%% do_vsd_command(Command) ->
%%    case has_vsdupgrade() of
%%      false -> ok;
%%      true ->
%%           RES = get_vsdupgrade_binary(),
%%           case  RES of
%%           false ->
%%                 {error, "ERROR: Upgrade Binary Check Failed"};
%%           _ ->
%%                 case os:cmd(RES ++ " " ++ Command) of
%%                 "OK" -> 
%%                         ok;
%%                 Reason ->
%%                         {error, Reason}
%%                 end
%%           end

%%    end.          
%% %%% #---------------------------------------------------------

%% dns_check(IPAddrDns, Ns) -> 
%%             dns_poll(?MAX_DNS_RETRY,IPAddrDns, Ns).

%% %%% ----------------------------------------------------------

%% dns_poll(Retries,IPAddrDns, Ns) when Retries > 0 ->
%%              ClusterIpT = ootI:get_addresses(IPAddrDns,inet,Ns),
%%              ClusterIpList = case ClusterIpT of
%%                              {ok, ClusterIps} -> 
%% 		                                ClusterIps;
%%                              {error,nxdomain} ->
%%                                                 sysInitI:error_msg("~p: DNS Resolve failed~n"
%% 								   "DNS host name : ~p~n"
%%                                                                    "Error: nxdomain~n"
%%                                                                    "Continue Polling..... ~n",
%%                                                                    [?MODULE,IPAddrDns]),
%%                                                 timer:sleep(?POLL_TIME),
%%                                                 dns_poll(Retries-1,IPAddrDns,Ns);
%%                              {error,eagain} ->
%%                                                 sysInitI:error_msg("~p: DNS Resolve failed~n"
%% 								   "DNS host name : ~p~n"
%%                                                                    "Error: eagain~n"
%%                                                                    "Continue Polling..... ~n",
%%                                                                    [?MODULE,IPAddrDns]),
%%                                                 timer:sleep(?POLL_TIME),
%%                                                 dns_poll(Retries-1,IPAddrDns,Ns);
%%                              {error,einval} ->
%%                                                 case ootI:get_addresses(IPAddrDns,inet6,Ns) of
%%                                                 {ok, ClusterIpsV6} -> 
%% 		                                                    ClusterIpsV6;
%%                                                 {error, ERRORNAME} -> 
%%                                                                     sysInitI:error_msg("~p: DNS Resolve failed~n"
%% 								    "DNS host name : ~p~n"
%%                                                                     "Error: ~p~n"
%%                                                                     "Continue Polling...... ~n",
%%                                                                     [?MODULE,IPAddrDns,ERRORNAME]),
%%                                                                     timer:sleep(?POLL_TIME),
%%                                                                     dns_poll(Retries-1,IPAddrDns,Ns)
%%                                                  end;
%%                              {_,Reason} ->  
%%                                   sysInitI:error_msg("~p: Fetching the IP Address from DNS server failed~n"
%% 				                     "Reason = ~p~n",
%%                             		             [?MODULE,Reason]),
%%                                   throw ({error,Reason})
%%                              end, 

%%              case ClusterIpList
%%              of Addrs when length(Addrs) /= ?NUM_CLUSTER_MEMBERS ->
%%                       timer:sleep(?POLL_TIME),
%%                       dns_poll(Retries-1,IPAddrDns,Ns);
%%              _ -> 
%%                       ClusterIpList
%%              end;

%% dns_poll(0,_,_) ->
%%            Reason = "Maximum number of trials to fetch cluster members from DNS is reached",   
%%            sysInitI:error_msg("~p: DNS Failed to provide 5 cluster mebers required~n"
%% 			       "Reason = ~p~n",
%% 			       [?MODULE,Reason]),
%%            throw ({error,Reason}).

%% %%% ----------------------------------------------------------

%% get_oam_ip() ->
%%     %% here we get the IP address of the Oam node in the VNF
%%     CoreMpId = clhI:mp_id({mpRole, core}),
%%     inet:ntoa(clhI:ip_address(CoreMpId)).

%% %%% ----------------------------------------------------------

%% appm_start(Name,Cb, Cmd, NsIn) ->
%%     Ns = case NsIn of
%% 	     no_ns -> [];
%% 	     NsIn -> [{ns, NsIn}]
%% 	 end,
%%     appmServer:start_internal_lm([{name, Name},
%% 				  {mfa, {?MODULE, Cb,[]}},
%% 				  {args, Cmd},
%% 				  {owner, self()},
%% 				  {autoclean, true}] ++ Ns).
    
%% %%% #---------------------------------------------------------

%% cluster_retry_join(IpList,"") ->    
%%     cluster_retry_join(IpList, "", "");

%% cluster_retry_join(IpList,Port) ->
%%     cluster_retry_join(IpList,":"++integer_to_list(Port),"").

%% cluster_retry_join([],_SrvPort,Acc) ->
%%     Acc;

%% cluster_retry_join([IP|T],SrvPort,"") ->
%%     IPNew = inet:ntoa(IP),
%%     case inet:parse_ipv4_address(IPNew) of
%% 	{ok, _} ->
%% 	    NewAcc = " -retry-join="++IPNew++SrvPort;
%% 	{error, _} ->
%% 	    NewAcc = " -retry-join="++"["++IPNew++"]"++SrvPort
%%     end,
%%     cluster_retry_join(T,SrvPort,NewAcc);


%% cluster_retry_join([IP|T],SrvPort,Acc) ->
%%     IPNew = inet:ntoa(IP),
%%     case inet:parse_ipv4_address(IPNew) of
%% 	{ok, _} ->
%% 	    NewAcc = Acc ++ " -retry-join="++IPNew++SrvPort;
%% 	{error, _} ->
%% 	    NewAcc = Acc ++ " -retry-join="++"["++IPNew++"]"++SrvPort
%%     end,
%%     cluster_retry_join(T,SrvPort,NewAcc).

%% %%% ----------------------------------------------------------

%% client_cmd(LogFile,IP, SrvIPIn, SrvPort, CfgFile) ->
%%     SrvIP = case inet:parse_address(SrvIPIn) of
%% 		{ok, X} when size(X) == 4 ->
%% 		    SrvIPIn;
%% 		{ok, X} when size(X) == 8 ->
%% 		    "["++SrvIPIn++"]";
%% 		_ -> 
%% 		    SrvIPIn
%% 	    end,
		
%%     [?LOG_WRAPPER,
%%      LogFile,
%%      ?CONSUL_BIN,
%%      "agent",
%%      "-config-file="++CfgFile,
%%      "-bind="++IP,
%%      "-retry-join="++SrvIP++case SrvPort of
%% 				"" -> "";
%% 				Port -> ":"++integer_to_list(Port)
%% 			    end
%%     ].

%% client_cmd(LogFile,IP, _SrvIPIn, SrvPort, ClusterIps, CfgFile) ->
%%     [?LOG_WRAPPER,
%%      LogFile,
%%      ?CONSUL_BIN,
%%      "agent",
%%      "-config-file="++CfgFile,
%%      "-bind="++IP,
%%      cluster_retry_join(ClusterIps, SrvPort)
%%     ].

%% %%% ----------------------------------------------------------

%% server_cmd(LogFile,IP,CfgFile) ->
%%     [?LOG_WRAPPER,
%%      LogFile,
%%      ?CONSUL_BIN,
%%      "agent",
%%      "-server",
%%      "-config-file="++CfgFile,
%%      "-bind="++IP
%%     ].

%% %%% ----------------------------------------------------------

%% get_ns_and_ip(undefined) ->
%%     {error,no_mo_ref};

%% get_ns_and_ip(MoRef) ->
%%     case file:consult("/home/sirpa/ns_ip.txt") of
%% 	{ok,Terms} ->
%% 	    %% for testing
%% 	    Ns = proplists:get_value(ns,Terms),
%% 	    Ip = proplists:get_value(ip,Terms),
%% 	    {ok, Ns, Ip};
%% 	_ ->
%% 	    case ootI:get_ns_and_ip(MoRef) of
%% 		{ok, _Ns, Ip } when Ip == <<>>; Ip == "" ->
%% 		    {error, ip_not_defined };
%% 		{ok, NsBin, IpBin} ->
%% 		    IpStr = binary_to_list(IpBin),
%% 		    Ip = hd(string:tokens(IpStr,"/")), % remove stuff after possible /
%% 		    {ok, binary_to_list(NsBin), Ip};
%% 		Other ->
%% 		    Other
%% 	    end
%%     end.

%% %%% ----------------------------------------------------------

%% %%% This file is used by SD clients to find the IP address and namespace
%% write_mom_data_file(Ip, Ns) ->
%%     File=?MOM_DATA_FILE,
%%     Str=Ns ++ ";" ++ Ip,
%%     sysInitI:info_msg("~p: Writing mom_data file: ~p ~n",[?MODULE,File]),
%%     ok=file:write_file(File,Str).

%% %%% ----------------------------------------------------------

%% %%% Build a unique node name based on domain and IP address
%% get_node_name(Domain, Ip) ->
%%   case Domain of
%%     local  -> "sds-local";
%%     global -> "sdc-global-"++Ip;
%%     gsds   -> "sds-global-"++Ip;
%%     fa     ->
%%       case clhI:core_state() of
%%         active -> "sds-fa-";
%%         _      -> "sdc-fa-"
%%       end ++Ip
%%   end.

%% %%% ----------------------------------------------------------

%% %%% Add configuration options needed to encrypt all Consul interfaces
%% add_crypto_config(CMap, NcMo, TcMo, Ns)
%%   when NcMo /= undefined, TcMo /= undefined ->
%%     CertPath     = certI:get_cert_dist_dir(),
%%     CertFile     = CertPath ++ "/cert_" ++ Ns ++ ".pem",
%%     KeyFile      = CertPath ++ "/key_"  ++ Ns ++ ".pem",
%%     TrustFile    = CertPath ++ "/tc_"   ++ Ns ++ ".pem",
%%     CMap#{
%%       encrypt         => list_to_binary(?SERF_ENC_KEY),
%%       verify_incoming => true,
%%       verify_outgoing => true,
%%       ca_file         => list_to_binary(TrustFile),
%%       key_file        => list_to_binary(KeyFile),
%%       cert_file       => list_to_binary(CertFile)
%%      };

%% add_crypto_config(CMap, _NcMo, _TcMo, _Ns) ->
%%     CMap.

%% %%% ----------------------------------------------------------

%% add_port_config(CMap, Domain) ->
%%     PMap =
%% 	case Domain of
%% 	    local ->
%% 		#{
%% 	      server => ?LOCAL_SERVER_PORT,
%% 	      serf_lan => ?LOCAL_SERF_LAN_PORT,
%% 	      serf_wan => ?LOCAL_SERF_WAN_PORT
%% 	     };
%% 	    fa ->
%% 		#{
%% 	      server => ?FA_SERVER_PORT,
%% 	      serf_lan => ?FA_SERF_LAN_PORT,
%% 	      serf_wan => ?FA_SERF_WAN_PORT
%% 	     };
%% 	    global ->
%% 		#{
%% 	      serf_lan => ?GLOBAL_SERF_LAN_PORT,
%% 	      serf_wan => ?GLOBAL_SERF_WAN_PORT
%% 	     };
%% 	    gsds ->
%% 		#{
%% 	      server => ?GSDS_SERVER_PORT
%% 	     }
%% 	end,
%%     DnsMap = PMap#{
%% 	       dns => ?CONSUL_DNS_PORT
%% 	      },
%%     CMap#{
%%       ports => DnsMap
%%      }.

%% %%% ----------------------------------------------------------

%% add_local_socket(CMap, Domain) ->
%%     CMap#{
%%       addresses => #{
%% 	http => case Domain of
%% 		    local  -> list_to_binary(?LOCAL_UNIX_SOCKET);
%% 		    fa     -> list_to_binary(?FA_UNIX_SOCKET);
%% 		    global -> list_to_binary(?GLOBAL_UNIX_SOCKET);
%% 		    gsds   -> list_to_binary(?GSDS_UNIX_SOCKET)
%% 		end
%%        }
%%      }.

%% %%% ----------------------------------------------------------

%% set_bootstrap_mode_vsd(CMap, SrvIp)
%%   when SrvIp == "localhost"; SrvIp == "127.0.0.1" ; SrvIp == "::1" ->
%%        CMap#{
%% 	 bootstrap_expect => 1
%%   };

%% set_bootstrap_mode_vsd(CMap, _SrvIp) ->
%%   CMap#{
%%     bootstrap_expect => ?NUM_CLUSTER_MEMBERS
%%   }.

%% %%% ----------------------------------------------------------

%% add_server_settings(CMap, Domain, SrvIp)
%%   when Domain /= global ->
%%   NodeType = swmI:node_type(),
%%   case NodeType of
%%        "vSD" ->
%% 	      set_bootstrap_mode_vsd(CMap, SrvIp);
%%         _ ->
%%               CMap#{
%%                  bootstrap_expect => 1
%%               }
%%   end;

%% add_server_settings(CMap, _Domain, _SrvIp) ->
%%     CMap.

%% %%% ----------------------------------------------------------

%% add_base_config(Domain, SrvIp) ->
%%     BaseMap = #{
%%       disable_update_check => true,
%%       disable_host_node_id => true,
%%       disable_remote_exec  => false,
%%       enable_syslog        => true,
%%       syslog_facility      => list_to_binary("LOCAL1")
%%      },
%%     SocketMap = add_local_socket(BaseMap, Domain),
%%     add_server_settings(SocketMap, Domain, SrvIp).

%% %%% ----------------------------------------------------------

%% add_data_dir(CMap, Domain) ->
%%     CMap#{
%%       data_dir => case Domain of
%% 		      local  -> list_to_binary(?CONSUL_TMP ++ "local");
%% 		      fa     -> list_to_binary(?CONSUL_TMP ++ "fa");
%% 		      global -> list_to_binary(?CONSUL_TMP ++ "global");
%% 		      gsds   -> list_to_binary(?CONSUL_TMP ++ "gsds")
%% 		  end
%%      }.

%% %%% ----------------------------------------------------------

%% %%% Create a Consul configuration for a given domain.
%% create_config_file(Domain, IP, NS, DC, SrvIp, TcMo, NcMo) ->
%%     CMap = add_base_config(Domain,SrvIp),
%%     DataMap = add_data_dir(CMap, Domain),
%% %%% Update global config file with user supplied data center name
%%     DcMap =
%% 	case Domain of
%% 	    gsds   -> maps:put(datacenter, list_to_binary(DC), DataMap);
%% 	    global -> maps:put(datacenter, list_to_binary(DC), DataMap);
%% 	    fa     -> maps:put(datacenter, list_to_binary(?DEFAULT_DC_NAME), DataMap);
%% 	    local  -> maps:put(datacenter, list_to_binary(?DEFAULT_DC_NAME), DataMap)
%%         end,
%% %%% The consul config files are updated with a unique node_name
%%     NodeMap = maps:put(node_name, list_to_binary(get_node_name(Domain, IP)), DcMap),
%%     {FRes, FHandle} = file:open(?NO_TLS_FLAG, [read]),
%%     CryptMap = case FRes of
%% 		   ok ->
%% 		       file:close(FHandle),
%% 		       NodeMap;
%% 		   error ->
%% 		       add_crypto_config(NodeMap, NcMo, TcMo, NS)
%% 	       end,
%%     PortMap = add_port_config(CryptMap, Domain),
%% %%% Save JSON with nice formatting to make it easier to read
%%     COut = jsone:encode(PortMap, [{space, 1}, {indent, 4}]),
%%     file:write_file(
%%       case Domain of
%% 	  local  -> ?LOCAL_CFG;
%% 	  fa     -> ?FA_CFG;
%% 	  global -> ?GLOBAL_CFG;
%% 	  gsds   -> ?GLOBSRV_CFG
%%       end,
%%       COut
%%      ).

%% %%% ----------------------------------------------------------

%% create_domain_file(Ip, SrvIp, SrvPort) ->
%%     sysInitI:info_msg("~p:create_domain_file ~n", [?MODULE]),
%%     PortsKey=jsone:encode({{json,"ports"}}),
%%     HttpKey=jsone:encode({{json,"http"}}),
%%     ServerKey=jsone:encode({{json,"serf_lan"}}),

%%     {ok,LBin} = file:read_file(?LOCAL_CFG),
%%     LMap = jsone:decode(LBin),
%%     LPmap=maps:get(PortsKey, LMap),
%%     LHttpPort=maps:get(HttpKey,LPmap,40500),
%%     LServPort=maps:get(ServerKey,LPmap),

%%     {ok,FBin} = file:read_file(?FA_CFG),
%%     FMap = jsone:decode(FBin),
%%     FPmap=maps:get(PortsKey, FMap),
%%     FHttpPort=maps:get(HttpKey,FPmap,40501),
%%     FServPort=maps:get(ServerKey,FPmap),

%%     {ok,GBin} = file:read_file(?GLOBAL_CFG),
%%     GMap = jsone:decode(GBin),
%%     GPmap=maps:get(PortsKey, GMap),
%%     GHttpPort=maps:get(HttpKey,GPmap,40502),

%%     Str = io_lib:format(
%% 	    "local;~w;127.0.0.1;~w;/tmp/consul/consul_local_server_socket;0;1~n"
%% 	    "fa;~w;~s;~w;/tmp/consul/consul_fa_server_socket;0;2~n"
%% 	    "global;~w;~s;~w;/tmp/consul/consul_global_client_socket;0;3~n",
%% 	    [LHttpPort,LServPort,
%% 	     FHttpPort,Ip,FServPort,
%% 	     GHttpPort,SrvIp,SrvPort ]),

%%     case filelib:ensure_dir(?DOMAIN_FILE) of
%% 	ok ->
%% 	    ok;
%% 	ErrEnsure ->
%% 	    sysInitI:error_msg(
%% 	      "~p:create_domain_file, cannot ensure Domain file directory~n"
%% 	      " Reason ~p,",  [?MODULE,ErrEnsure])
%%     end,

%%     case file:write_file(?DOMAIN_FILE, Str) of
%% 	ok ->
%% 	    sysInitI:info_msg(
%% 	      "~p:create_domain_file,  Domain file ~p written successfully ~n",
%% 	      [?MODULE,?DOMAIN_FILE]),
%% 	    ok;
%% 	ErrWrite ->
%% 	    sysInitI:error_msg(
%% 	      "~p:create_domain_file, cannot write Domain file ~n"
%% 	      " Reason ~p,",  [?MODULE,ErrWrite])
%%     end,		    
%%     ok.

%% %%% ----------------------------------------------------------

%% %% Consul does not like localhost so map it to "::1"
%% get_dc_host_and_port(#'SdCluster'{host=GipSrv,port=GipPort,serviceArea=DC}) ->
%%     NewGipSrv = map_localhost(GipSrv),
%%     {DC,NewGipSrv,GipPort}.

%% %%% ----------------------------------------------------------

%% map_localhost("localhost") -> "::1";
%% map_localhost(GipSrv)      -> GipSrv.

%% %%% ----------------------------------------------------------

%% log_wrapper() ->
%%     F = filename:join([code:priv_dir(comsa),"bin","consul.sh"]),
%%     swmI:find_file(F).

%% %%% ----------------------------------------------------------
