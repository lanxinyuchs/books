%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaServDiscServer.erl %
%%% @author etxarnu
%%% @copyright Ericsson AB 2017
%%% @version /main/R9A/R10A/R11A/R12A/14

%%% @doc ==Service Discovery server starter== 
%%% This module implements the startup of the Consul agents

-module(comsaServDiscServer).
-vsn('/main/R9A/R10A/R11A/R12A/14').
-date('2017-12-01').
%%% ----------------------------------------------------------
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%%            2016-11-11 etxarnu  Created
%%%            2016-12-19 etxarnu  Fix get_ip_address
%%% R9A/1      2017-01-23 etxarnu  Moved this module here from SYS
%%%                                Added MO support
%%% R9A/2      2017-01-29 etxarnu  added post_init
%%% R9A/6      2017-01-30 etxarnu  More features and fixes
%%% R9A/7      2017-02-01 etxarnu  Handle case when ootI:get_ns_and_ip
%%%                                returns a undefined IP (in DUMMY UP)
%%% R9A/8      2017-02-02 etxarnu  Made node_name in global/fa.json unique
%%% R9A/9      2017-02-16 etxarnu  Fixed timoeut case when MO deleted
%%% R9A/10     2017-02-19 etxarnu  Updates and fixes
%%% R9A/13     2017-02-20 etxarnu  Copy global.json from /home/sirpa/consul 
%%%                                to /etc/consul if not there
%%% R9A/16     2017-02-20 etxarnu  Added /tmp/consul/domains file
%%% R9A/17     2017-02-21 etxarnu  Added [] around IPv6 address in join option
%%% R9A/19     2017-02-22 etxarnu  Added more info to coli sd state command
%%%                                Moved create_domain_file to after start of 
%%%                                consul servers
%%% R9A/21     2017-02-24 etxarnu  Added trustedCategory and nodeCredential
%%% R9A/22     2017-02-27 etxarnu  Handle upgrade case
%%% R9A/23     2017-02-28 etxarnu  Start FA server also on BPU
%%% R9A/24     2017-02-28 etxarnu  Updated creation of domains file
%%% R9A/25     2017-03-01 etxarnu  More fixes to domains file
%%% R9A/26     2017-03-01 etxarnu  Added dummy id to each domain line
%%% R9A/27     2017-03-03 etxarnu  coli_members corrected to handle namespace
%%% R9A/28     2017-03-16 etxarnu  Bug fix in get_ns_and_ip
%%%                                Use serf_lan instead of server in domain 
%%% R9A/29     2017-03-29 etxarnu  Added wrapper for consul so stdout can
%%%                                be redirected to log files.
%%%                                Removed ee_consul stuff
%%% R9A/30     2017-04-03 etxarnu  Call certI:update_cert_dist at activate
%%% R9A/32     2017-04-07 etxarnu  RmeSds version 2.0 updates 
%%% R9A/33     2017-04-07 etxarnu  Ensure that /tmp/consul exist in init()
%%% R9A/34     2017-04-11 etxarnu  HV79419: restart consul at warm restart
%%% R9A/35     2017-04-12 etxarnu  Call certI:update_cert_dist at timeout,retryGlip
%%% R9A/36     2017-04-13 etxarnu  Set default value of node in 
%%%                                comsaServiceDiscovery table 
%%%                                Added stop_gRpc/0.
%%% R9A/37     2017-04-13 etxberb  Added flag 'stopped' for stop_gRpc which
%%%                                prevents it from being restarted.
%%% R10A/1     2017-04-20 etxarnu  Added start of SD server if IP=localhost/::1
%%% R10A/2     2017-05-08 etxarnu  Fixed bind address for gsds server
%%% R10A/3     2017-05-10 etxarnu  Fixed join address for gsds client when gsds server started.
%%%                                Added gsds server to coli members
%%% R10A/4     2017-05-10 etxarnu  Corrected case where stop_gRpc comes
%%%                                before SD MO is created 
%%% R10A/5     2017-05-23 etxarnu  write_mom_data_file added for vSD
%%% R10A/8     2017-06-14 qostjoa  Configuration files are generated internally, without relying on EE templates.
%%%                                Added support for TLS encryption for Consul interfaces
%%%                                If certificates are not set in the MO, unencrypted connection
%%%                                will be used.
%%% R10A/10    2017-06-20 qostjoa  Send SIGUP to the wrapper script before asking PGH to terminate.
%%% R10A/11    2017-06-26 etxarnu  Create /var/log/consul if it does not exist.
%%% R11A/1     2017-09-12 etxarnu  Bug in printout in handle_ip_changed
%%% R11A/2     2017-09-13 etxarnu  Convert GlipMo to string before storing in mnesia
%%% R11A/5     2017-10-12 erohjun  Added upgrade callbacks
%%% R11A/7     2017-10-12 erohjun  Replaced upgrade event commit to confirm.
%%% R12A/1     2017-11-10 erohjun  Updated with cluster functionality.
%%% R12A/2     2017-11-10 erohjun  Minor BUG fix in cluster functionality.
%%% R12A/3     2017-11-14 qostjoa  Added brackets around IPv6 addresses in 
%%%                                retry-join.
%%% R12A/4     2017-11-15 erohjun  Netmask handling was removed accidently 
%%%                                during merge, fixed again.
%%% R12A/5     2017-11-20 erohjun  Minor bug fixes in upgrade call backs.
%%% R12A/6     2017-11-24 erohjun  Consul domains not restarted on receving 
%%%                                same IP or NS.
%%% R12A/7     2017-11-24 qostjoa  RmeSds version 3.0 updates, split MOM in two
%%%                                MOs, server and client
%%%                                Client related functions movesd to 
%%%                                comsaServDiscClient
%%% R12A/8     2017-11-25 etxarnu  Added has_consul/0 back temporarily
%%% R12A/9     2017-11-27 qostjoa  Fixed an error when attempting
%%%                                to create the log dir twice.
%%% R12A/10    2017-11-27 qostjoa  Revert to RmeSds 2.0
%%% R12A/11    2017-11-28 erohjun  Binding consul to core 2.
%%% R12A/12    2017-11-30 erohjun  Binding consul to core 2 only for target.
%%% R12A/13    2017-12-01 erohjun  To check vSD IP part of DNS cluster members.
%%% R12A/14    2017-12-01 etxarnu  Minor editorial changes

%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-compile(export_all). %%%TO BE REMOVED

%% API
-export([start_link/0]).
-export([activate/0]).
-export([init_tables/1,post_init/0]).
-export([init_data/0]).

-export([has_consul/0]).

-export([stop_gRpc/0]).

%% COLI 
-export([sys_sd/1]).

%% UPGRADE
-export([verify_precondition/0]).
-export([activate_start/0]).
-export([start_configuration/0]).
-export([stop_configuration/0]).
-export([confirm/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


%% OOT callback
-export([ip_changed/3]).

%% APPM callbacks
-export([ warm_done/0]).

-export([local_exit/1]).     %callback from APPM if local exits
-export([fa_client_exit/1]). %callback from APPM if fa client exits
-export([fa_server_exit/1]). %callback from APPM if fa server exits
-export([gsds_client_exit/1]).%callback from APPM if gsds_client exits
-export([gsds_server_exit/1]).%callback from APPM if gsds_server exits

%%% ----------------------------------------------------------

-define(CONSUL_BIN, get_consul_binary()).
-define(DEFAULT_DC_NAME, "vrcs").
-define(SERVER_NAME, ?MODULE).
-define(CONSUL_DIR,"/home/sirpa/consul/").
-define(LOCAL_CFG,    ?CONSUL_DIR ++ "local.json").
-define(FA_CFG,       ?CONSUL_DIR ++ "fa.json").
-define(GLOBAL_CFG,   ?CONSUL_DIR ++ "global.json").
-define(GLOBSRV_CFG,  ?CONSUL_DIR ++ "gsds.json").
-define(CONSUL_TMP, "/tmp/consul/").
-define(MOM_DATA_FILE,?CONSUL_TMP ++ "mom_data").
-define(LOCAL_TMP,    ?CONSUL_TMP ++ "local").
-define(FA_TMP,       ?CONSUL_TMP ++ "fa").
-define(GLOBAL_TMP,   ?CONSUL_TMP ++ "global").
-define(GLOBSRV_TMP,  ?CONSUL_TMP ++ "gsds").
-define(DOMAIN_FILE,  ?CONSUL_TMP ++ "domains").
-define(LOG_WRAPPER, log_wrapper() ).
-define(LOG_DIR,"/var/log/consul/").
-define(LOCAL_LOG,   ?LOG_DIR ++ "local.log").
-define(FA_LOG,      ?LOG_DIR ++ "fa.log").
-define(GLOBAL_LOG,  ?LOG_DIR ++ "global.log").
-define(GLOBSRV_LOG, ?LOG_DIR ++ "gsds.log").
-define(SERF_ENC_KEY, "x6R5vIx0rJJNuRR5KK3Gug=="). %Key for UDP encryption (does not use TSL)
-define(NO_TLS_FLAG, "/home/sirpa/NO_TLS"). %Existence of this file indicates that TLS should be disabled.
%%% Ports used by the various Consul domains
-define(CONSUL_DNS_PORT, -1).
%%% Local domain
-define(LOCAL_SERVER_PORT,   43400).
-define(LOCAL_SERF_LAN_PORT, 23501).
-define(LOCAL_SERF_WAN_PORT, 23502).
%%% FA domain
-define(FA_SERVER_PORT,   41400).
-define(FA_SERF_LAN_PORT, 21501).
-define(FA_SERF_WAN_PORT, 21502).
%%% Global domain
-define(GLOBAL_SERF_LAN_PORT, 22501).
-define(GLOBAL_SERF_WAN_PORT, 22502).
%%% GSDS domain
-define(GSDS_SERVER_PORT, 44400).
%%% Unix sockets used by the domains
-define(LOCAL_UNIX_SOCKET,  "unix://" ++ ?CONSUL_TMP ++ "consul_local_server_socket").
-define(FA_UNIX_SOCKET,     "unix://" ++ ?CONSUL_TMP ++ "consul_fa_server_socket").
-define(GLOBAL_UNIX_SOCKET, "unix://" ++ ?CONSUL_TMP ++ "consul_global_client_socket").
-define(GSDS_UNIX_SOCKET,   "unix://" ++ ?CONSUL_TMP ++ "consul_global_server_socket").
%%% Signal for terminating the wrapper script
-define(SIGHUP, 1).
%%% poll time to have vSD cluster ready.
-define(POLL_TIME, 1000).
-define(NUM_CLUSTER_MEMBERS,5).
-define(MAX_DNS_RETRY,10000).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Usage of error_logger:XXX_report
-define(LOG_ERR(__ReportInfo),
	try
	    sysInitI:error_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:error_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_INFO(__ReportInfo),
	try
	    sysInitI:info_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:info_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_WARN(__ReportInfo),
	try
	    sysInitI:warning_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:warning_report(?RepInfo(__ReportInfo))
	end).
-define(RepInfo(__RepInfo),
	[{?MODULE, ?FUNCTION} | __RepInfo]).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).
-define(MODULE_STR, atom_to_list(?MODULE)).
-define(MonoTime, erlang:monotonic_time()).
-define(PROC_INFO(__Pid), sysUtil:pid_info(__Pid, {all, [error_handler]})).
-define(STATE_INFO(__Record),
	sysUtil:record_format(record_info(fields, state), __Record)).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-include("RmeSds.hrl").

-record(st,
	{dc, %datacenter
	 ip, %fronthaul IP
	 ns, %fronthaul namespace
	 local_appm_id,
	 local_rst_cnt=0,
	 fa_appm_id,
	 fa_rst_cnt=0,
	 glob_appm_id,	  
	 glob_rst_cnt=0,
	 glob_status,
	 globsrv_appm_id,	  
	 gsds_ip,
	 gsds_port,
	 tcMo,
	 ncMo
	}).


-record(comsaServiceDiscovery,
	{node = node(), % key to table
	 glipMo, % GSDS local IP MOref
	 tcMo, % TC MO ref for front haul nw
	 ncMo, % NC MO ref for front haul nw
	 dc, % datacenter name
	 glip,   %  local IP address
	 glns,   %  local name space
	 gIpSrv, % GSDS Server IP address
	 gPortSrv % GSDS Server port number
	}).

%%===========================================================================
%% Init functions
%%===========================================================================
init_tables(DbNodes) ->
    case has_consul() of
	true ->
	    {atomic, ok} =
		clhI:mnesia_create_table(
		  comsaServiceDiscovery,
		  [{type, set},
		   {disc_copies, DbNodes},
		   {attributes, record_info(fields,
					    comsaServiceDiscovery)}]);
	false ->
	    ok
    end,
    ok.


init_data() ->
    case has_consul() of
	true ->
	    
	    mnesia:dirty_write(
	      #comsaServiceDiscovery{ });
	false ->
	    ok
    end,
    ok.



post_init() ->
    case has_consul() of
	true ->
	    comsaI:register_subscriptions(
	      "RmeSds", [{"ServiceDiscovery", serviceDiscovery}]);
	false ->
	    ok
    end,
    ok.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [], []).

activate() ->
    cast(?FUNCTION_NAME),
    ok.


%% Restart consul programs after warm restart.
warm_done() ->
    case has_consul() of
	true ->
	    activate(); 
	false ->
	    ok
    end.


get_consul_binary() ->
    case appmAppData:get_lm_start_data_from_name("consul") of
	{ok, _NameCxc, CxpPath, BinFile, _Info, _Pgroup} ->
	    filename:join([CxpPath,BinFile]);
	_ ->
	    case file:read_file_info("/usr/bin/consul") of
		{ok,_} ->
		    "/usr/bin/consul";
		_ ->
		    {error,file_not_found}
	    end
    end.

%%% ----------------------------------------------------------

get_vsdupgrade_binary() ->
    case appmAppData:get_lm_start_data_from_name("vsdupgrade") of
	{ok, _NameCxc, CxpPath, BinFile, _Info, _Pgroup} ->
	    filename:join([CxpPath,BinFile]);
	_ ->
	    case file:read_file_info("/usr/bin/vsdupgrade") of
		{ok,_} ->
		    swmI:find_file("/usr/bin/vsdupgrade" );
		_ ->
		    false
	    end
    end.



   
%%% ----------------------------------------------------------
has_consul() ->
    case file:read_file_info("/usr/bin/consul") of
	{ok,_} ->
	    true;
	_ ->
	    case filelib:wildcard(filename:join(
				    [sysEnv:home_dir(),"software/SDEE-*"])) of
		[] ->
		    false;
		_ ->
		    true
	    end
	    
    end.

%%% ----------------------------------------------------------
has_vsdupgrade() ->
    case file:read_file_info("/usr/bin/vsdupgrade") of
	{ok,_} ->
	    true;
	_ ->
	    case filelib:wildcard(filename:join(
				    [sysEnv:home_dir(),"software/SDEE-*"])) of
		[] ->
		    false;
		_ ->
		    true
	    end
    end.

%%% ----------------------------------------------------------
%%% OOT callback
ip_changed(MoRef, NS, IpAddr) ->
    cast({?FUNCTION_NAME,MoRef, NS, IpAddr}),
    ok.

%%% ----------------------------------------------------------
%%% APPM callbacks
-define(RESTART_DELAY,5000).
local_exit(List) ->
    sysInitI:error_msg("~p:~p, restarting it~n",
		       [?MODULE, ?FUNCTION_NAME]),
    erlang:send_after(?RESTART_DELAY,?MODULE,{?FUNCTION_NAME, hd(List)}).

fa_client_exit(List) ->
    sysInitI:error_msg("~p:~p, restarting it~n",
		       [?MODULE, ?FUNCTION_NAME]),
    erlang:send_after(?RESTART_DELAY,?MODULE,{?FUNCTION_NAME, hd(List)}).

fa_server_exit(List) ->
    sysInitI:error_msg("~p:~p, restarting it~n",
		       [?MODULE, ?FUNCTION_NAME]),
    erlang:send_after(?RESTART_DELAY,?MODULE,{?FUNCTION_NAME, hd(List)}).

gsds_client_exit(List) ->
    sysInitI:error_msg("~p:~p, restarting it~n",
		       [?MODULE, ?FUNCTION_NAME]),
    erlang:send_after(?RESTART_DELAY,?MODULE,{?FUNCTION_NAME, hd(List)}).

gsds_server_exit(List) ->
    sysInitI:error_msg("~p:~p, restarting it~n",
		       [?MODULE, ?FUNCTION_NAME]),
    erlang:send_after(?RESTART_DELAY,?MODULE,{?FUNCTION_NAME, hd(List)}).

%% Used by SWM during upgrade. The FROM node is about to go down and this
%% SD channel is not supposed to be restarted.
stop_gRpc() ->
    call(stop_gRpc).

%%%===================================================================
%%% Coli calls
%%%===================================================================
%%% #---------------------------------------------------------
sys_sd(["stop"]) ->
    call(coli_stop);
sys_sd(["start"]) ->
    call(coli_start);
sys_sd(["state"]) ->
    {ok,Res}=call(coli_state),
    io:format("~s~n",[Res]);
sys_sd(["members"]) ->
    {ok,Res}=call(coli_members),
    io:format("~s~n",[Res]);
sys_sd(["disable_ee"]) ->
    {ok,Res}=call(coli_disable_ee),
    io:format("~s~n",[Res]);
sys_sd(["enable_ee"]) ->
    {ok,Res}=call(coli_enable_ee),
    io:format("~s~n",[Res]);

sys_sd(_) ->
    io:format("argument error~n~n").

	    
%%%===================================================================
%%% Upgrade calls
%%%===================================================================
%%% #---------------------------------------------------------

do_vsd_command(Command) ->
   case has_vsdupgrade() of
     false -> ok;
     true ->
          RES = get_vsdupgrade_binary(),
          case  RES of
          false ->
                {error, "ERROR: Upgrade Binary Check Failed"};
          _ ->
                case os:cmd(RES ++ " " ++ Command) of
                "OK" -> 
                        ok;
                Reason ->
                        {error, Reason}
                end
          end
   end.
          
%%% #---------------------------------------------------------

verify_precondition() ->
    do_vsd_command("verifyPreconditions").

activate_start() ->
    do_vsd_command("activateStart").

stop_configuration() ->
    do_vsd_command("configurationStop").

start_configuration() ->
    do_vsd_command("configurationStart").

confirm() ->
    do_vsd_command("commit").


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([]) ->
    %% subscribe to MO changes
    mnesia:subscribe({table, serviceDiscovery, simple}),
    filelib:ensure_dir(?GLOBAL_CFG), %% for BPU
    filelib:ensure_dir(?MOM_DATA_FILE),

    {ok, #st{}}.

    

 
%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

handle_call(coli_stop, _From, State) ->
    sysInitI:info_msg("~p:Stopping consul from coli~n",
		      [?MODULE]),
    NewState=stop_all(State),
    os:cmd("pkill -f consul"), % EE started consul processes
    {reply, ok, NewState};

handle_call(coli_start, _From, State) ->
    sysInitI:info_msg("~p:Starting consul from coli~n",
		      [?MODULE]),
    NewState = do_activate(State),
    {reply, ok, NewState};

handle_call(coli_state, _From,
	    State=#st{ip=Ip,ns=Ns,gsds_ip=SrvIp,gsds_port=SrvPort}) ->
    ResIP = io_lib:format(
	      "~nSD configuration~n"
	      "Fronthaul IP:  ~p~n"
	      "Fronthaul namespace:  ~p~n"
	      "GSDS server IP: ~p~n"
	      "GSDS server port: ~p~n~n"
	      "Consul processes~n",
	    [Ip,Ns,SrvIp,SrvPort ]),
    ResProc = case os:cmd("pgrep -V | grep procps-ng") of
		  [] -> os:cmd("pgrep -fl consul"); %WR6
		  _ -> os:cmd("pgrep -fla consul") %WR8
	      end,
    {reply, {ok,ResIP++ResProc}, State};

handle_call(coli_members, _From, State) ->
    
    NodeType = swmI:node_type(),

    Result =
        case NodeType of
            "vSD" ->
                os:cmd(?CONSUL_BIN ++ " members -http-addr=" ++ ?GSDS_UNIX_SOCKET );
             _ ->
                ResLoc  = os:cmd(?CONSUL_BIN ++ " members -http-addr=" ++ ?LOCAL_UNIX_SOCKET),
	        ResFa   = os:cmd(?CONSUL_BIN ++ " members -http-addr=" ++ ?FA_UNIX_SOCKET),
                ResGlob = os:cmd(?CONSUL_BIN ++ " members -http-addr=" ++ ?GLOBAL_UNIX_SOCKET),
                ResLoc ++ "\n" ++ ResFa  ++ "\n" ++ ResGlob++ "\n"
        end, 

    {reply,
     {ok, Result},
     State};

handle_call(stop_gRpc, _From, State = #st{ glob_status = undefined}) ->
    {reply, {ok, ok}, State};

handle_call(stop_gRpc = Req, _From, State) ->
    OsCmd = ?CONSUL_BIN ++ " leave -http-addr=" ++ ?GSDS_UNIX_SOCKET,
    ResGlob = os:cmd(OsCmd),
    ?LOG_INFO([{req, Req},
	       {osCmd, OsCmd},
	       {result, ResGlob}]),
    {reply, {ok, ResGlob}, State#st{glob_status = stopped}};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%%% ----------------------------------------------------------
handle_cast(activate, State) ->
    NewState = do_activate(State),
    {noreply, NewState};
handle_cast({ip_changed,MoRef, NS, IpAddr}, S) ->
    {noreply, handle_ip_changed(S, b2l(MoRef), b2l(NS), b2l(IpAddr))};

handle_cast(_Msg, State) ->
    {noreply, State}.


%%% ----------------------------------------------------------
handle_info({mnesia_table_event, Event}, S) ->
    New_s = handle_table_event(Event, S),
    {noreply, New_s};
handle_info({timeout, Type}, S) ->
    {noreply, handle_timeout(Type, S)};

handle_info({local_exit, _Appm_id}, S) ->
    {noreply, start_local_server(S)};
handle_info({fa_client_exit, _Appm_id}, S) ->
    {noreply, start_fa_client(S)};
handle_info({fa_server_exit, _Appm_id}, S) ->
    {noreply, start_fa_server(S)};
handle_info({gsds_client_exit, _Appm_id}, #st{glob_status = GStatus} = S) ->
    case GStatus of
	stopped ->
	    %% It has intentionally been stopped and should not be restarted.
	    {noreply, S};
	_ ->
	    {noreply, start_gsds_client(S)}
    end;
handle_info({gsds_server_exit, _Appm_id}, S) ->
    {noreply, start_gsds_server(S)};

handle_info(_Info, State) ->
    {noreply, State}.

%%% ----------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%% ----------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

cast(Msg) ->
    gen_server:cast(?SERVER_NAME, Msg).

call(Msg) ->
    gen_server:call(?SERVER_NAME, Msg).

%%% ----------------------------------------------------------
handle_table_event({delete, {serviceDiscovery, _Id}, _}, S) ->
    sysInitI:info_msg("~p:handle_table_event(delete)~n", [?MODULE]),   
    mnesia:dirty_write(
      #comsaServiceDiscovery{}),
    stop_all(S);

handle_table_event({write,
		    SD=#serviceDiscovery{localAddress = GlipMoBin,
					 trustCategory = TcMo,
					 nodeCredential = NcMo,
					 datacenter = DC,
					 gsdsAddress = GipServer
					} ,
		    _}, S) ->
    GlipMo=b2l(GlipMoBin),
    sysInitI:info_msg("~p:handle_table_event(write,~p)~n", [?MODULE,SD]),

    {GipSrv,GipPort}=get_ip_and_port(GipServer),
    [R=#comsaServiceDiscovery{glipMo=RlipMo,
			      tcMo=RTcMo,
			      ncMo=RNcMo,
			      dc=RDC,
			      gIpSrv=RipSrv,
			      gPortSrv=RipPort}] =
	mnesia:dirty_read(comsaServiceDiscovery,node()),

    case {GlipMo, TcMo, NcMo, DC, GipSrv, GipPort} of
	{RlipMo, RTcMo, RNcMo, RDC, RipSrv, RipPort} ->
	    %% no change, do nothing
	    S;
	_ ->
	    case get_ns_and_ip(GlipMo) of
		{ok, Ns, Ip} ->

		    mnesia:dirty_write(
		      R#comsaServiceDiscovery{
			glipMo=GlipMo,
			tcMo=TcMo,
			ncMo=NcMo,
			dc=DC,
			glip=Ip,
			glns=Ns,
			gIpSrv=GipSrv,
			gPortSrv=GipPort}),
		    ok=register_ip_change_cb(GlipMo, ?MODULE),
		    update_cert(Ns, NcMo, TcMo),
		    start_fa_and_global(
		      S#st{ip=Ip,
			   ns=Ns,
			   dc=DC,
			   gsds_ip=GipSrv,
			   gsds_port=GipPort,
			   tcMo=TcMo,
			   ncMo=NcMo});
		{error,Reason} ->
		    sysInitI:error_msg(
		      "~p:ootI:get_ns_and_ip failed with Reason ~p~n"
		      "retrying ~n",
		      [?MODULE,Reason]),
		    mnesia:dirty_write(
		      R#comsaServiceDiscovery{
			glipMo=GlipMo,
			tcMo=TcMo,
			ncMo=NcMo,
			dc=DC,
			glip=undefined,
			glns=undefined,
			gIpSrv=GipSrv,
			gPortSrv=GipPort}),
		    start_timer(),
		    S#st{dc=DC,
			 gsds_ip=GipSrv,
			 gsds_port=GipPort}
	    end

    end.

update_cert(Ns, NcMo, TcMo)
  when NcMo /= undefined, TcMo /= undefined ->
    ok=certI:update_cert_dist({Ns,b2l(NcMo),b2l(TcMo)});
update_cert(Ns, _NcMo, _TcMo) ->
    ok=certI:update_cert_dist(Ns).


%%% ----------------------------------------------------------
start_timer() ->
    timer:send_after(30000, {timeout,retryGlip}).

handle_timeout(retryGlip, S) ->
    [R=#comsaServiceDiscovery{glipMo=GlipMo,
                              dc =DC,
                              gIpSrv=GipSrv,
                              gPortSrv=GipPort}] =
	mnesia:dirty_read(comsaServiceDiscovery,node()),
    case get_ns_and_ip(GlipMo) of
	{ok, Ns, Ip} ->	    
	    mnesia:dirty_write(
	      R#comsaServiceDiscovery{
		glip=Ip,
		glns=Ns}),
	    NcMo = R#comsaServiceDiscovery.ncMo,
	    TcMo = R#comsaServiceDiscovery.tcMo,
	    ok=register_ip_change_cb(GlipMo, ?MODULE),
	    update_cert(Ns, NcMo, TcMo),	    
	    start_fa_and_global(S#st{ip=Ip,
			             ns=Ns,
				     dc=DC,
				     gsds_ip=GipSrv,
				     gsds_port=GipPort,
				     tcMo=TcMo,
				     ncMo=NcMo}); 
	{error,no_mo_ref} ->
	    %% MO has been deleted during timeout
	    S;
	{error,Reason} ->
	    sysInitI:error_msg("~p:ootI:get_ns_and_ip failed with Reason ~p~n"
			       "retrying ~n",
			       [?MODULE,Reason]),
	    start_timer(),
	    S
    end.

is_ip_or_ns_changed(GLip,GLns,NewNs,IP)
when GLip /= IP; GLns /= NewNs->
              true;

is_ip_or_ns_changed(_GLip,_GLns,_NewNs,_IP)->
              false.

handle_ip_changed(S, MoRef, NewNs, [])  ->
    sysInitI:warning_msg("~p:~p(~p,~p)Empty IP changed notification received   ~n"
			 "ignoring ~n",
			 [?MODULE,?FUNCTION_NAME,
			  MoRef, NewNs]),
    S;

%%% ----------------------------------------------------------
handle_ip_changed(S, MoRef, NewNs, NewIp)  ->
    [R=#comsaServiceDiscovery{glipMo=GlipMo,
			      tcMo=TcMo,
			      ncMo=NcMo,
                              dc=DC,
                              glip=GLip,
                              glns=GLns, 
                              gIpSrv=GipSrv,
                              gPortSrv=GipPort}] =
	mnesia:dirty_read(comsaServiceDiscovery,node()),

    Ip = hd(string:tokens(NewIp,"/")),     
    Result = is_ip_or_ns_changed(GLip,GLns,NewNs,Ip),

    case Result of
	true ->
	    sysInitI:info_msg("~p: GSDS local IP has changed to NS  =~p IP=~p ~n",
			      [?MODULE, NewNs, NewIp]),
	    case MoRef of
		GlipMo ->
		    mnesia:dirty_write(
		      R#comsaServiceDiscovery{
			glip=Ip,
			glns=NewNs}),
		    update_cert(NewNs, NcMo, TcMo),
		    start_fa_and_global(S#st{ip=Ip,
					     ns=NewNs,
					     dc=DC,
					     gsds_ip=GipSrv,
					     gsds_port=GipPort,
					     tcMo=TcMo,
					     ncMo=NcMo});
		_ ->
		    sysInitI:error_msg("~p:~p(~p,~p,~p)  Unknown MoRef ~p~n"
				       "ignoring ~n",
				       [?MODULE,?FUNCTION_NAME,
					MoRef, NewNs, NewIp,GlipMo]),
		    S
	    end;
	false ->
	    sysInitI:warning_msg("~p:~p(~p,~p)  No Change in IP Or NS ~n"
                                 "ignoring ~n",
                   	         [?MODULE,?FUNCTION_NAME,
				  NewNs, NewIp]),
	    S
    end.
	   
%%% --------------------------------------------------------
do_activate(S) ->
    sysInitI:info_msg("~p:activate ~n", [?MODULE]),
    case filelib:ensure_dir(?LOCAL_LOG) of
	ok ->
	    ok;
	_ ->
	    Cmd = ["mkdir","-p","-m","777",?LOG_DIR],
	    appmPghServer:req_spawn_pgm([{args, Cmd},  {flag,1} ]),
	    sysInitI:info_msg("~p:created logging directory ~p ~n", [?MODULE,?LOG_DIR])
    end,

    case mnesia:dirty_read(serviceDiscovery,{"1","1","1"}) of
	[#serviceDiscovery{localAddress = GlipMoBin,
			   trustCategory = TcMo,
			   nodeCredential = NcMo,
			   datacenter = DC,
			   gsdsAddress = GipServer}] ->
	    GlipMo=b2l(GlipMoBin),
	    {GipSrv,GipPort}=get_ip_and_port(GipServer),
	    case get_ns_and_ip(GlipMo) of
		{ok, Ns, Ip} ->
		    mnesia:dirty_write(
		      #comsaServiceDiscovery{
			 glipMo=GlipMo,
			 tcMo=TcMo,
			 ncMo=NcMo,
			 dc=DC,
			 glip=Ip,
			 glns=Ns,
			 gIpSrv=GipSrv,
			 gPortSrv=GipPort}),	       
		    ok=register_ip_change_cb(GlipMo, ?MODULE),
		    update_cert(Ns, NcMo, TcMo),

		    start_fa_and_global(S#st{ip=Ip,
					     ns=Ns,
					     dc=DC,
					     gsds_ip=GipSrv,
					     gsds_port=GipPort,
					     tcMo=TcMo,
					     ncMo=NcMo});
		{error,Reason} ->
		    sysInitI:error_msg(
		      "~p:ootI:get_ns_and_ip failed with Reason ~p,"
		      "retrying ~n",
		      [?MODULE,Reason]),
		    mnesia:dirty_write(
		      #comsaServiceDiscovery{
			 glipMo=GlipMo,
			 tcMo=TcMo,
			 ncMo=NcMo,
			 dc=DC,
			 glip=undefined,
			 glns=undefined,
			 gIpSrv=GipSrv,
			 gPortSrv=GipPort}),
		    start_timer(),
		    S#st{dc=DC,
			 gsds_ip=GipSrv,
			 gsds_port=GipPort}
	    end;
	[] ->
	    sysInitI:info_msg(
	      "~p: SDS MO not created so wait until done~n",
	      [?MODULE]),
	    S
    end.

%%% ----------------------------------------------------------
start_local_server(Sin) -> 
    S = stop_local(Sin),
    Cmd = server_cmd(?LOCAL_LOG,"127.0.0.1",?LOCAL_CFG),
    case appm_start("SD local server", local_exit, Cmd, no_ns) of
	{ok, AppmId} ->
	    sysInitI:info_msg("~p: Started local consul server~n",
			       [?MODULE]),
	    NewCnt=S#st.local_rst_cnt + 1,
	    S#st{local_appm_id=AppmId, local_rst_cnt=NewCnt };
	{error,Reason} ->
	    sysInitI:error_msg("~p: APPM failed to start local consul~n"
			       "Reason = ~p~n",
			       [?MODULE,Reason]),
	    S
    end.
	    
%%% ----------------------------------------------------------
start_fa_and_global(Sin) ->
    NodeType = swmI:node_type(),
    start_fa_and_global(NodeType, Sin).

start_fa_and_global("vSD", Sin) ->
    write_mom_data_file(Sin),
    start_gsds_server(Sin);

start_fa_and_global(_NodeType, Sin) ->
    create_config_file(local, Sin),
    create_config_file(fa, Sin),
    create_config_file(global, Sin),
    S = start_local_server(Sin),
    write_mom_data_file(S),
    CoreState = clhI:core_state(),
    IsVrcs = sysEnv:vrcs(),
    NewState =
	case {IsVrcs,CoreState}  of
	    {true, active} ->
		%% The Oam node in a VNF starts fa server and gsds client
		Sfa = start_fa_server(S),
		Ssrv = start_gsds_server(Sfa), %possibly start gsds server
		start_gsds_client(Ssrv);
	    {true, _} ->
		%% The regular node in a VNF starts 
		%% fa client towards OaM node
		start_fa_client(S);
	    {false,_ } ->
		%% The BPU starts a fa server and a gsds client
		Sfa = start_fa_server(S),
		Ssrv = start_gsds_server(Sfa), %possibly start gsds server
		start_gsds_client(Ssrv)
	end,
    create_domain_file(NewState).
   

%%% ----------------------------------------------------------
start_fa_server(Sin=#st{ip=Ip,ns=Ns}) ->
    S = stop_fa(Sin),
    Cmd = server_cmd(?FA_LOG,Ip,?FA_CFG),
    case appm_start("SD FA server", fa_server_exit, Cmd, l2b(Ns)) of
	{ok, AppmId} ->
	    sysInitI:info_msg("~p: Started FA consul server~n"
			      "In NS= ~p and IP= ~p~n",
			      [?MODULE,Ns,Ip]),
	    NewCnt=S#st.fa_rst_cnt + 1,
	    S#st{fa_appm_id=AppmId, fa_rst_cnt=NewCnt };
	{error,Reason} ->
	    sysInitI:error_msg("~p: APPM failed to start fa server consul~n"
			       "Reason = ~p~n",
			       [?MODULE,Reason]),
	    S
    end.

%%% ----------------------------------------------------------
start_fa_client(Sin=#st{ip=Ip,ns=Ns}) ->
    S = stop_fa(Sin),
    SrvIP = get_oam_ip(), 
    Cmd = client_cmd(?FA_LOG,Ip, SrvIP,"",?FA_CFG),
    case appm_start("SD FA client", fa_client_exit, Cmd, l2b(Ns)) of
	{ok, AppmId} ->
	    sysInitI:info_msg("~p: Started FA consul client~n"
			       "In NS= ~p and IP= ~p~n",
			       [?MODULE,Ns,Ip]),
	    NewCnt=S#st.fa_rst_cnt + 1,
	    S#st{fa_appm_id=AppmId, fa_rst_cnt=NewCnt };
	{error,Reason} ->
	    sysInitI:error_msg("~p: APPM failed to start FA client consul~n"
			       "Reason = ~p~n",
			       [?MODULE,Reason]),
	    S
    end.

%%% ----------------------------------------------------------
start_gsds_client(Sin=#st{gsds_ip=""}) ->
     stop_gsds_client(Sin);

%%% ----------------------------------------------------------
start_gsds_client(Sin=#st{ip=Ip,ns=Ns,gsds_ip=SrvIp,gsds_port=SrvPort})
when SrvIp == "localhost"; SrvIp == "127.0.0.1" ; SrvIp == "::1" ->
    S = stop_gsds_client(Sin),
    SIP = case SrvIp of
	      "::1" -> Ip;
	      _     -> SrvIp
	  end,

    Cmd = client_cmd(?GLOBAL_LOG, Ip, SIP, SrvPort, ?GLOBAL_CFG),

    case appm_start("GSDS client", gsds_client_exit, Cmd, l2b(Ns)) of
	{ok, AppmId} ->
	    sysInitI:info_msg("~p: Started Global consul client~n"
			       "In NS= ~p and IP= ~p~n"
			       "Server:IP ~p, port=  ~p~n",
			       [?MODULE, Ns, Ip, SrvIp, SrvPort]),
	    NewCnt=S#st.glob_rst_cnt + 1,
	    S#st{glob_appm_id=AppmId, glob_rst_cnt=NewCnt };
	{error,Reason} ->
	    sysInitI:error_msg("~p: APPM failed to start GSDS client consul~n"
			       "Reason = ~p~n",
			       [?MODULE,Reason]),
	    S
    end;


start_gsds_client(Sin=#st{ip=Ip,ns=Ns,gsds_ip=SrvIp,gsds_port=SrvPort}) ->
    S = stop_gsds_client(Sin),

    SIP = case inet:parse_ipv4_address(SrvIp) of
	      {ok, _} ->
		  SrvIp;
	      {error, _} ->
		  case inet:parse_ipv6_address(SrvIp) of
		      {ok, _} ->
			  SrvIp;
		      {error, _} ->
			  ""
		  end
          end,

    Cmd = case SIP of
	      "" ->  
		  ClusterIps = dns_check(SrvIp, Ip, Ns),
		  client_cmd(?GLOBAL_LOG, Ip, SrvIp, SrvPort, ClusterIps, ?GLOBAL_CFG);
	      _ ->   
		  client_cmd(?GLOBAL_LOG, Ip, SrvIp, SrvPort, ?GLOBAL_CFG)
          end,

    case appm_start("GSDS client", gsds_client_exit, Cmd, l2b(Ns)) of
	{ok, AppmId} ->
	    sysInitI:info_msg("~p: Started Global consul client~n"
			      "In NS= ~p and IP= ~p~n"
			      "Server:IP ~p, port=  ~p~n",
			      [?MODULE, Ns, Ip, SrvIp, SrvPort]),
	    NewCnt=S#st.glob_rst_cnt + 1,
	    S#st{glob_appm_id=AppmId, glob_rst_cnt=NewCnt };
	{error,Reason} ->
	    sysInitI:error_msg("~p: APPM failed to start GSDS client consul~n"
			       "Reason = ~p~n",
			       [?MODULE,Reason]),
	    S
    end.


check_cluster_member(Ip, ClusterIpList) ->
    case inet:parse_address(Ip) of
	{ok, Address} ->
	    lists:member(Address, ClusterIpList);
	{error,ERROR} ->
	    sysInitI:error_msg("~p: Parsing of address failed~n"
			       "Error = ~p~n",
			       [?MODULE,ERROR]),
	    throw ({error,ERROR}) 
    end.


%%%===================================================================
%%% DNS Poll
%%%===================================================================
%%% #---------------------------------------------------------


dns_check(IPAddrDns,Ip,Ns) -> 
    dns_poll(?MAX_DNS_RETRY,IPAddrDns,Ip,Ns).

dns_poll(Retries,IPAddrDns,Ip,Ns) when Retries > 0 ->
    ClusterIpT = ootI:get_addresses(IPAddrDns,inet,Ns),
    ClusterIpList =
	case ClusterIpT of
	    {ok, ClusterIps} -> 
		ClusterIps;
	    {error,nxdomain} ->
		print_seldom(error, Retries,
			     "~p: DNS Resolve failed~n"
			     "DNS host name : ~p~n"
			     "Error: nxdomain~n"
			     "Continue Polling..... ~n",
			     [?MODULE,IPAddrDns]),
		timer:sleep(?POLL_TIME),
		dns_poll(Retries-1,IPAddrDns,Ip,Ns);
	    {error,eagain} ->
		print_seldom(error, Retries,
			     "~p: DNS Resolve failed~n"
			     "DNS host name : ~p~n"
			     "Error: eagain~n"
			     "Continue Polling..... ~n",
			     [?MODULE,IPAddrDns]),
		timer:sleep(?POLL_TIME),
		dns_poll(Retries-1,IPAddrDns,Ip,Ns);
	    {error,einval} ->
		case ootI:get_addresses(IPAddrDns,inet6,Ns) of
		    {ok, ClusterIpsV6} -> 
			ClusterIpsV6;
		    {error, ERRORNAME} ->
			print_seldom(error, Retries,
				     "~p: DNS Resolve failed~n"
				     "DNS host name : ~p~n"
				     "Error: ~p~n"
				     "Continue Polling...... ~n",
				     [?MODULE,IPAddrDns,ERRORNAME]),
			timer:sleep(?POLL_TIME),
			dns_poll(Retries-1,IPAddrDns,Ip,Ns)
		end;
	    {_,Reason} ->  
		sysInitI:error_msg("~p: Fetching the IP Address from DNS server failed~n"
				   "Reason = ~p~n",
				   [?MODULE,Reason]),
		throw ({error,Reason})
	end, 

    case ClusterIpList of
	Addrs when length(Addrs) /= ?NUM_CLUSTER_MEMBERS  ->
	    print_seldom(warning, Retries,
			 "~p: Cluster Members Must be 5~n"
			 "DNS host name : ~p~n"
			 "Warning: eagain~n"
			 "Continue Polling..... ~n",
			 [?MODULE,IPAddrDns]),
	    timer:sleep(?POLL_TIME),
	    dns_poll(Retries-1,IPAddrDns,Ip,Ns);
	_ ->
	    NodeType = swmI:node_type(),
	    case NodeType of
		"vSD" ->
		    case check_cluster_member(Ip,ClusterIpList) of
			true ->
			    ClusterIpList;
			false ->
			    print_seldom(warning, Retries,
					 "~p:Ip Mismatch from DNS Resolve~n"
					 "DNS host name : ~p~n"
					 "Warning: eagain~n"
					 "Continue Polling..... ~n",
					 [?MODULE,IPAddrDns]),
			    timer:sleep(?POLL_TIME),
			    dns_poll(Retries-1,IPAddrDns,Ip,Ns)
		    end;
		_ ->
		    ClusterIpList
	    end
    end;

dns_poll(0,_,_,_) ->
    Reason = "Maximum number of trials to fetch cluster members from DNS is reached",   
    sysInitI:error_msg("~p: DNS Failed to provide 5 cluster mebers required~n"
		       "Reason = ~p~n",
		       [?MODULE,Reason]),
    throw ({error,Reason}).


%%% ----------------------------------------------------------
print_seldom(error, Retries, Str,Args ) when (Retries rem 100) == 0 ->
    sysInitI:error_msg(Str,Args);
print_seldom(warning, Retries,Str,Args ) when (Retries rem 100) == 0 ->
    sysInitI:warning_msg(Str,Args);
print_seldom(_,_,_,_) -> ok.


 
%%% ----------------------------------------------------------
start_gsds_server(Sin=#st{gsds_ip=""}) ->
     stop_gsds_server(Sin);


%%% ----------------------------------------------------------
start_gsds_server(Sin=#st{ip=Ip,ns=Ns,gsds_ip=SrvIp,gsds_port=SrvPort})
when SrvIp == "localhost"; SrvIp == "127.0.0.1" ; SrvIp == "::1" ->
    create_config_file(gsds, Sin),
    case file:read_file_info(?GLOBSRV_CFG) of
	{ok,_} ->
	    S = stop_gsds_server(Sin),
	    Cmd = server_cmd(?GLOBSRV_LOG,Ip,?GLOBSRV_CFG),
	    case appm_start("GSDS server", gsds_server_exit, Cmd, l2b(Ns)) of
		{ok, AppmId} ->
		    sysInitI:info_msg("~p: Started Global consul server~n"
				      "In NS= ~p and IP= ~p~n"
			       "Server:IP ~p, port=  ~p~n",
				      [?MODULE, Ns, Ip, SrvIp, SrvPort]),
		    NewCnt=S#st.glob_rst_cnt + 1,
		    S#st{globsrv_appm_id=AppmId, glob_rst_cnt=NewCnt };
		{error,Reason} ->
		    sysInitI:error_msg("~p: APPM failed to start GSDS server consul~n"
				       "Reason = ~p~n",
				       [?MODULE,Reason]),
		    S
	    end;
	_  ->
	    Sin
    end;


%%% ----------------------------------------------------------
start_gsds_server(Sin=#st{ip=Ip,ns=Ns,gsds_ip=SrvIp,gsds_port=SrvPort}) ->

    SIP = case inet:parse_ipv4_address(SrvIp) of
	      {ok, _} ->
		  "";
	      {error, _} ->
		  case inet:parse_ipv6_address(SrvIp) of
		      {ok, _} ->
			  "";
		      {error, _} ->
			  NodeType = swmI:node_type(),
			  case NodeType of
			      "vSD" ->
				  SrvIp;
			      _ ->
				  ""       
			  end
		  end
          end,

    case SIP of
	"" -> 
	    stop_gsds_server(Sin); 
	_  ->
	    create_config_file(gsds, Sin),
	    case file:read_file_info(?GLOBSRV_CFG) of
		{ok,_} ->
		    S = stop_gsds_server(Sin),

		    ClusterIps = dns_check(SrvIp,Ip,Ns),
		    Cmd = server_cmd(?GLOBSRV_LOG,Ip, ClusterIps,SrvPort,?GLOBSRV_CFG), 

		    case appm_start("GSDS server", gsds_server_exit, Cmd, l2b(Ns)) of
			{ok, AppmId} ->
			    sysInitI:info_msg("~p: Started Global consul server~n"
					      "In NS= ~p and IP= ~p~n"
					      "Server:IP ~p, port=  ~p~n",
					      [?MODULE, Ns, Ip, SrvIp, SrvPort]),
			    NewCnt=S#st.glob_rst_cnt + 1,
			    S#st{globsrv_appm_id=AppmId, glob_rst_cnt=NewCnt };
			{error,Reason} ->
			    sysInitI:error_msg("~p: APPM failed to start GSDS server consul~n"
					       "Reason = ~p~n",
					       [?MODULE,Reason]),
			    S
		    end;
         	_  ->
		    Sin
	    end
    end.

%%% ----------------------------------------------------------
get_oam_ip() ->
    %% here we get the IP address of the Oam node in the VNF
    CoreMpId = clhI:mp_id({mpRole, core}),
    inet:ntoa(clhI:ip_address(CoreMpId)).

%%% ----------------------------------------------------------
appm_start(Name,Cb, Cmd, NsIn) ->
    Ns = case NsIn of
	     no_ns -> [];
	     NsIn -> [{ns, NsIn}]
	 end,
    CpuSet = 
	case sysEnv:target() of
	    true -> [{cpu_set, 2}];
	    false -> []
	end,
    appmServer:start_internal_lm([{name, Name},
				  {mfa, {?MODULE, Cb,[]}},
				  {args, Cmd},
				  {owner, self()},
				  {autoclean, true}]
				 ++ CpuSet
				 ++ Ns).  

%%% #---------------------------------------------------------



client_cmd(LogFile,IP, SrvIPIn, SrvPort, CfgFile) ->
    SrvIP = case inet:parse_address(SrvIPIn) of
		{ok, X} when size(X) == 4 ->
		    SrvIPIn;
		{ok, X} when size(X) == 8 ->
		    "["++SrvIPIn++"]";
		_ -> 
		    SrvIPIn
	    end,

    [?LOG_WRAPPER,
     LogFile,
     ?CONSUL_BIN,
     "agent",
     "-config-file="++CfgFile,
     "-bind="++IP,
     "-retry-join="++SrvIP++case SrvPort of
				"" -> "";
				Port -> ":"++integer_to_list(Port)
			    end
    ].

client_cmd(LogFile,IP, _SrvIPIn, SrvPort, ClusterIps, CfgFile) ->
    [?LOG_WRAPPER,
     LogFile,
     ?CONSUL_BIN,
     "agent",
     "-config-file="++CfgFile,
     "-bind="++IP,
     cluster_retry_join(ClusterIps, SrvPort)
    ].


cluster_retry_join(IpList,"") ->    
    cluster_retry_join(IpList, "", "");

cluster_retry_join(IpList,Port) ->
    cluster_retry_join(IpList,":"++integer_to_list(Port),"").

cluster_retry_join([],_SrvPort,Acc) ->
    Acc;

cluster_retry_join([IP|T],SrvPort,"") ->
    IPNew = inet:ntoa(IP),
    case inet:parse_ipv4_address(IPNew) of
	{ok, _} ->
	    NewAcc = " -retry-join="++IPNew++SrvPort;
	{error, _} ->
	    NewAcc = " -retry-join="++"["++IPNew++"]"++SrvPort
    end,
    cluster_retry_join(T,SrvPort,NewAcc);


cluster_retry_join([IP|T],SrvPort,Acc) ->
    IPNew = inet:ntoa(IP),
    case inet:parse_ipv4_address(IPNew) of
	{ok, _} ->
	    NewAcc = Acc ++ " -retry-join="++IPNew++SrvPort;
	{error, _} ->
	    NewAcc = Acc ++ " -retry-join="++"["++IPNew++"]"++SrvPort
    end,
    cluster_retry_join(T,SrvPort,NewAcc).

server_cmd(LogFile,IP,CfgFile) ->
    [?LOG_WRAPPER,
     LogFile,
     ?CONSUL_BIN,
     "agent",
     "-server",
     "-config-file="++CfgFile,
     "-bind="++IP
    ].

server_cmd(LogFile,IP, ClusterIps, SrvPort, CfgFile) ->
    [?LOG_WRAPPER,
     LogFile,
     ?CONSUL_BIN,
     "agent",
     "-server",
     "-config-file="++CfgFile,
     "-bind="++IP,
     cluster_retry_join(ClusterIps, SrvPort)
    ].

stop_all(S) ->
    stop_local(
	stop_fa(
	    stop_gsds_server(
		stop_gsds_client(S)))).
    
stop_local(S) ->
    NewState =
	case S#st.local_appm_id of
	    undefined ->
		S;
	    AppmId ->
		appmServer:signal_to_internal_pgm(AppmId, ?SIGHUP),
		timer:sleep(100),
		appmServer:stop_internal_lm(AppmId),
		S#st{local_appm_id=undefined}
	end,
    os:cmd("rm -rf " ++ ?LOCAL_TMP),
    NewState.
    
stop_fa(S) ->
    NewState =
	case S#st.fa_appm_id of
	    undefined ->
		S;
	    AppmId ->
		appmServer:signal_to_internal_pgm(AppmId, ?SIGHUP),
		timer:sleep(100),
		appmServer:stop_internal_lm(AppmId),
		S#st{fa_appm_id=undefined}
	end,
    os:cmd("rm -rf " ++ ?FA_TMP),
    NewState.
    
stop_gsds_client(S) ->
    NewState =
	case S#st.glob_appm_id of
	    undefined ->
		S;
	    AppmId ->
		appmServer:signal_to_internal_pgm(AppmId, ?SIGHUP),
		timer:sleep(100),
		appmServer:stop_internal_lm(AppmId),
		S#st{glob_appm_id=undefined}
	end,
    os:cmd("rm -rf " ++ ?GLOBAL_TMP),
    NewState.

stop_gsds_server(S) ->
    NewState =
	case S#st.globsrv_appm_id of
	    undefined ->
		S;
	    AppmId ->
		appmServer:signal_to_internal_pgm(AppmId, ?SIGHUP),
		timer:sleep(100),
		appmServer:stop_internal_lm(AppmId),
		S#st{globsrv_appm_id=undefined}
	end,
    os:cmd("rm -rf " ++ ?GLOBSRV_TMP),
    NewState.


get_ns_and_ip(undefined) ->
    {error,no_mo_ref};
get_ns_and_ip(MoRef) ->
    case file:consult("/home/sirpa/ns_ip.txt") of
	{ok,Terms} ->
	    %% for testing
	    Ns = proplists:get_value(ns,Terms),
	    Ip = proplists:get_value(ip,Terms),
	    {ok, Ns, Ip};
	_ ->
	    case ootI:get_ns_and_ip(MoRef) of
		{ok, _Ns, Ip } when Ip == <<>>; Ip == "" ->
		    {error, ip_not_defined };
		{ok, NsBin, IpBin} ->
		    IpStr = b2l(IpBin),
		    Ip = hd(string:tokens(IpStr,"/")), % remove stuff after possible /
		    {ok, b2l(NsBin), Ip};
		Other ->
		    Other
	    end
    end.

register_ip_change_cb(GlipMo, Module) ->
    ootI:register_ip_change_cb(GlipMo, Module).


%%% This file is used by SD clients to find the IP address and namespace
write_mom_data_file(#st{ip=Ip,ns=Ns}) ->
    File=?MOM_DATA_FILE,
    Str=Ns ++ ";" ++ Ip,
    sysInitI:info_msg("~p: Writing mom_data file: ~p ~n",[?MODULE,File]),
    ok=file:write_file(File,Str).

%%% Build a unique node name based on domain and IP address
get_node_name(Domain, Ip) ->
  case Domain of
    local  -> "sds-local";
    global -> "sdc-global-"++Ip;
    gsds   -> "sds-global-"++Ip;
    fa     ->
      case clhI:core_state() of
        active -> "sds-fa-";
        _      -> "sdc-fa-"
      end ++Ip
  end.

%%% Add configuration options needed to encrypt all Consul interfaces
add_crypto_config(CMap, #st{ncMo=NcMo, tcMo=TcMo, ns=Ns})
  when NcMo /= undefined, TcMo /= undefined ->
    CertPath     = certI:get_cert_dist_dir(),
    CertFile     = CertPath ++ "/cert_" ++ Ns ++ ".pem",
    KeyFile      = CertPath ++ "/key_"  ++ Ns ++ ".pem",
    TrustFile    = CertPath ++ "/tc_"   ++ Ns ++ ".pem",
    CMap#{
      encrypt         => list_to_binary(?SERF_ENC_KEY),
      verify_incoming => true,
      verify_outgoing => true,
      ca_file         => list_to_binary(TrustFile),
      key_file        => list_to_binary(KeyFile),
      cert_file       => list_to_binary(CertFile)
     };
add_crypto_config(CMap, _S) ->
    CMap.

add_port_config(CMap, Domain) ->
    PMap =
	case Domain of
	    local ->
		#{
	      server => ?LOCAL_SERVER_PORT,
	      serf_lan => ?LOCAL_SERF_LAN_PORT,
	      serf_wan => ?LOCAL_SERF_WAN_PORT
	     };
	    fa ->
		#{
	      server => ?FA_SERVER_PORT,
	      serf_lan => ?FA_SERF_LAN_PORT,
	      serf_wan => ?FA_SERF_WAN_PORT
	     };
	    global ->
		#{
	      serf_lan => ?GLOBAL_SERF_LAN_PORT,
	      serf_wan => ?GLOBAL_SERF_WAN_PORT
	     };
	    gsds ->
		#{
	      server => ?GSDS_SERVER_PORT
	     }
	end,
    DnsMap = PMap#{
	       dns => ?CONSUL_DNS_PORT
	      },
    CMap#{
      ports => DnsMap
     }.

add_local_socket(CMap, Domain) ->
    CMap#{
      addresses => #{
	http => case Domain of
		    local  -> list_to_binary(?LOCAL_UNIX_SOCKET);
		    fa     -> list_to_binary(?FA_UNIX_SOCKET);
		    global -> list_to_binary(?GLOBAL_UNIX_SOCKET);
		    gsds   -> list_to_binary(?GSDS_UNIX_SOCKET)
		end
       }
     }.

set_bootstrap_mode_vsd(CMap,_Sin=#st{gsds_ip=SrvIp})
  when SrvIp == "localhost"; SrvIp == "127.0.0.1" ; SrvIp == "::1" ->
       CMap#{
       bootstrap_expect => 1
  };
set_bootstrap_mode_vsd(CMap,_Sin) ->
  CMap#{
  bootstrap_expect => ?NUM_CLUSTER_MEMBERS
  }.

add_server_settings(CMap, Domain,State)
  when Domain /= global ->
  NodeType = swmI:node_type(),
  case NodeType of
       "vSD" ->
	      set_bootstrap_mode_vsd(CMap,State);
        _ ->
              CMap#{
                 bootstrap_expect => 1
              }
  end;

add_server_settings(CMap, _Domain, _State) ->
    CMap.

add_base_config(Domain,State) ->
    BaseMap = #{
      disable_update_check => true,
      disable_host_node_id => true,
      disable_remote_exec  => false,
      enable_syslog        => true,
      syslog_facility      => list_to_binary("LOCAL1")
     },
    SocketMap = add_local_socket(BaseMap, Domain),
    add_server_settings(SocketMap, Domain,State).

add_data_dir(CMap, Domain) ->
    CMap#{
      data_dir => case Domain of
		      local  -> list_to_binary(?CONSUL_TMP ++ "local");
		      fa     -> list_to_binary(?CONSUL_TMP ++ "fa");
		      global -> list_to_binary(?CONSUL_TMP ++ "global");
		      gsds   -> list_to_binary(?CONSUL_TMP ++ "gsds")
		  end
     }.

%%% Create a Consul configuration for a given domain.
create_config_file(Domain, State) ->
    CMap = add_base_config(Domain,State),
    DataMap = add_data_dir(CMap, Domain),
%%% Update global config file with user supplied data center name
    DcMap =
	case Domain of
	    gsds   -> maps:put(datacenter, list_to_binary(State#st.dc), DataMap);
	    global -> maps:put(datacenter, list_to_binary(State#st.dc), DataMap);
	    fa     -> maps:put(datacenter, list_to_binary(?DEFAULT_DC_NAME), DataMap);
	    local  -> maps:put(datacenter, list_to_binary(?DEFAULT_DC_NAME), DataMap)
        end,
%%% The consul config files are updated with a unique node_name
    NodeMap = maps:put(node_name, list_to_binary(get_node_name(Domain, State#st.ip)), DcMap),
    {FRes, FHandle} = file:open(?NO_TLS_FLAG, [read]),
    CryptMap = case FRes of
		   ok ->
		       file:close(FHandle),
		       NodeMap;
		   error ->
		       add_crypto_config(NodeMap, State)
	       end,
    PortMap = add_port_config(CryptMap, Domain),
%%% Save JSON with nice formatting to make it easier to read
    COut = jsone:encode(PortMap, [{space, 1}, {indent, 4}]),
    file:write_file(
      case Domain of
	  local  -> ?LOCAL_CFG;
	  fa     -> ?FA_CFG;
	  global -> ?GLOBAL_CFG;
	  gsds   -> ?GLOBSRV_CFG
      end,
      COut
     ).


create_domain_file(State=#st{ip=Ip,
			     gsds_ip=SrvIp,
			     gsds_port=SrvPort}) ->
    sysInitI:info_msg("~p:create_domain_file ~n", [?MODULE]),
    PortsKey=jsone:encode({{json,"ports"}}),
    HttpKey=jsone:encode({{json,"http"}}),
    ServerKey=jsone:encode({{json,"serf_lan"}}),

    {ok,LBin} = file:read_file(?LOCAL_CFG),
    LMap = jsone:decode(LBin),
    LPmap=maps:get(PortsKey, LMap),
    LHttpPort=maps:get(HttpKey,LPmap,40500),
    LServPort=maps:get(ServerKey,LPmap),

    {ok,FBin} = file:read_file(?FA_CFG),
    FMap = jsone:decode(FBin),
    FPmap=maps:get(PortsKey, FMap),
    FHttpPort=maps:get(HttpKey,FPmap,40501),
    FServPort=maps:get(ServerKey,FPmap),

    {ok,GBin} = file:read_file(?GLOBAL_CFG),
    GMap = jsone:decode(GBin),
    GPmap=maps:get(PortsKey, GMap),
    GHttpPort=maps:get(HttpKey,GPmap,40502),

    Str = io_lib:format(
	    "local;~w;127.0.0.1;~w;/tmp/consul/consul_local_server_socket;0;1~n"
	    "fa;~w;~s;~w;/tmp/consul/consul_fa_server_socket;0;2~n"
	    "global;~w;~s;~w;/tmp/consul/consul_global_client_socket;0;3~n",
	    [LHttpPort,LServPort,
	     FHttpPort,Ip,FServPort,
	     GHttpPort,SrvIp,SrvPort ]),

    case filelib:ensure_dir(?DOMAIN_FILE) of
	ok ->
	    ok;
	ErrEnsure ->
	    sysInitI:error_msg(
	      "~p:create_domain_file, cannot ensure Domain file directory~n"
	      " Reason ~p,",  [?MODULE,ErrEnsure])
    end,

    case file:write_file(?DOMAIN_FILE, Str) of
	ok ->
	    sysInitI:info_msg(
	      "~p:create_domain_file,  Domain file ~p written successfully ~n",
	      [?MODULE,?DOMAIN_FILE]),
	    ok;
	ErrWrite ->
	    sysInitI:error_msg(
	      "~p:create_domain_file, cannot write Domain file ~n"
	      " Reason ~p,",  [?MODULE,ErrWrite])
    end,
		    
    State.
    
    
%%%%
%% consul does not like localhost so map it to "::1"
get_ip_and_port(#'HostAndPort'{host=GipSrv,port=GipPort}) ->
    NewGipSrv = map_localhost(GipSrv),
    {NewGipSrv,GipPort}.

map_localhost("localhost") -> "::1";
map_localhost(GipSrv)      -> GipSrv.

    

b2l(B) when is_binary(B) ->
    binary_to_list(B);
b2l(L) when is_list(L) ->
    L.

l2b(L) when is_list(L) ->
    list_to_binary(L);
l2b(B) when is_binary(B) ->
    B.

i2l(I) when is_integer(I) ->
    integer_to_list(I);
i2l(L) ->
    L.



log_wrapper() ->
    F = filename:join([code:priv_dir(comsa),"bin","consul.sh"]),
    swmI:find_file(F).
