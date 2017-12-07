%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaServDiscClient.erl %
%%% @author qostjoa
%%% @copyright Ericsson AB 2017
%%% @version /main/R12A/3

%%% @doc ==Service Discovery client starter== 
%%% This module implements the startup of the Consul agents

-module(comsaServDiscClient).
-vsn('/main/R12A/3').
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
%%% R12A/2     2017-11-27 qostjoa  Fixed an error when attempting
%%%                                to create the log dir twice.

%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
%% -compile(export_all). %%%TO BE REMOVED

%% %% API
%% -export([start_link/0]).
%% -export([activate/0]).
%% -export([init_tables/1,post_init/0]).
%% -export([init_data/0]).

%% -export([stop_gRpc/0]).

%% %% UPGRADE
%% -export([verify_precondition/0]).
%% -export([activate_start/0]).
%% -export([start_configuration/0]).
%% -export([stop_configuration/0]).
%% -export([confirm/0]).

%% %% gen_server callbacks
%% -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%% 	 terminate/2, code_change/3]).


%% %% OOT callback
%% -export([ip_changed/3]).

%% %% APPM callbacks
%% -export([warm_done/0]).

%% -export([local_exit/1]).     %callback from APPM if local exits
%% -export([fa_client_exit/1]). %callback from APPM if fa client exits
%% -export([fa_server_exit/1]). %callback from APPM if fa server exits
%% -export([gsds_client_exit/1]).%callback from APPM if gsds_client exits

%% %%% ----------------------------------------------------------

%% -include("comsaServDiscCommon.hrl").
%% -include("RmeSds.hrl").

%% -record(st,
%% 	{dc_pri, % Primary datacenter
%% 	 dc_sec, % Secondary datacenter
%% 	 ip,     % Fronthaul IP
%% 	 ns,     % Fronthaul namespace
%% 	 local_appm_id,
%% 	 local_rst_cnt=0,
%% 	 fa_appm_id,
%% 	 fa_rst_cnt=0,
%% 	 glob_appm_id_pri,
%% 	 glob_appm_id_sec,
%% 	 glob_rst_cnt_pri=0,
%% 	 glob_status_pri,
%% 	 glob_rst_cnt_sec=0,
%% 	 glob_status_sec,
%% 	 gsds_ip_pri,
%% 	 gsds_port_pri,
%% 	 gsds_ip_sec,
%% 	 gsds_port_sec,
%% 	 tcMo,
%% 	 ncMo
%% 	}).


%% -record(comsaServiceDiscovery,
%% 	{node = node(), % Key to table
%% 	 glipMo,     % GSDS local IP MOref
%% 	 tcMo,       % TC MO ref for front haul nw
%% 	 ncMo,       % NC MO ref for front haul nw
%% 	 dcPri,      % Primary datacenter name
%% 	 dcSec,      % Secondary datacenter name
%% 	 glip,       % Local IP address
%% 	 glns,       % Local name space
%% 	 gIpSrvPri,  % Primary GSDS Server IP address
%% 	 gPortSrvPri,% Primary GSDS Server port number
%% 	 gIpSrvSec,  % Secondary GSDS Server IP address
%% 	 gPortSrvSec % Secondary GSDS Server port number
%% 	}).


%% %%===========================================================================
%% %% Init functions
%% %%===========================================================================

%% init_tables(DbNodes) ->
%%     case comsaServDiscCommon:has_consul() of
%% 	true ->
%% 	    {atomic, ok} =
%% 		clhI:mnesia_create_table(
%% 		  comsaServiceDiscovery,
%% 		  [{type, set},
%% 		   {disc_copies, DbNodes},
%% 		   {attributes, record_info(fields,
%% 					    comsaServiceDiscovery)}]);
%% 	false ->
%% 	    ok
%%     end,
%%     ok.

%% %%% ----------------------------------------------------------

%% init_data() ->
%%     case comsaServDiscCommon:has_consul() of
%% 	true ->
	    
%% 	    mnesia:dirty_write(
%% 	      #comsaServiceDiscovery{ });
%% 	false ->
%% 	    ok
%%     end,
%%     ok.

%% %%% ----------------------------------------------------------

%% post_init() ->
%%     case comsaServDiscCommon:has_consul() of
%% 	true ->
%% 	    comsaI:register_subscriptions(
%% 	      "RmeSds", [{"ServiceDiscovery", serviceDiscovery}]);
%% 	false ->
%% 	    ok
%%     end,
%%     ok.

%% %%%===================================================================
%% %%% API
%% %%%===================================================================

%% %%--------------------------------------------------------------------
%% %% @doc
%% %% Starts the server
%% %%
%% %% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% %% @end
%% %%--------------------------------------------------------------------
%% start_link() ->
%%     gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [], []).

%% %%% ----------------------------------------------------------

%% activate() ->
%%     cast(?FUNCTION_NAME),
%%     ok.

%% %%% ----------------------------------------------------------

%% %% Restart consul programs after warm restart.
%% warm_done() ->
%%     case comsaServDiscCommon:has_consul() of
%% 	true ->
%% 	    activate(); 
%% 	false ->
%% 	    ok
%%     end.

%% %%% ----------------------------------------------------------

%% %%% OOT callback
%% ip_changed(MoRef, NS, IpAddr) ->
%%     cast({?FUNCTION_NAME,MoRef, NS, IpAddr}),
%%     ok.

%% %%% ----------------------------------------------------------

%% %%% APPM callbacks
%% -define(RESTART_DELAY,5000).
%% local_exit(List) ->
%%     sysInitI:error_msg("~p:~p, restarting it~n",
%% 		       [?MODULE, ?FUNCTION_NAME]),
%%     erlang:send_after(?RESTART_DELAY,?MODULE,{?FUNCTION_NAME, hd(List)}).

%% fa_client_exit(List) ->
%%     sysInitI:error_msg("~p:~p, restarting it~n",
%% 		       [?MODULE, ?FUNCTION_NAME]),
%%     erlang:send_after(?RESTART_DELAY,?MODULE,{?FUNCTION_NAME, hd(List)}).

%% fa_server_exit(List) ->
%%     sysInitI:error_msg("~p:~p, restarting it~n",
%% 		       [?MODULE, ?FUNCTION_NAME]),
%%     erlang:send_after(?RESTART_DELAY,?MODULE,{?FUNCTION_NAME, hd(List)}).

%% gsds_client_exit(List) ->
%%     sysInitI:error_msg("~p:~p, restarting it~n",
%% 		       [?MODULE, ?FUNCTION_NAME]),
%%     erlang:send_after(?RESTART_DELAY,?MODULE,{?FUNCTION_NAME, hd(List)}).

%% %% Used by SWM during upgrade. The FROM node is about to go down and this
%% %% SD channel is not supposed to be restarted.
%% stop_gRpc() ->
%%     call(stop_gRpc).

	    
%% %%%===================================================================
%% %%% Upgrade calls
%% %%%===================================================================

%% verify_precondition() ->
%%     comsaServDiscCommon:do_vsd_command("verifyPreconditions").

%% activate_start() ->
%%     comsaServDiscCommon:do_vsd_command("activateStart").

%% stop_configuration() ->
%%     comsaServDiscCommon:do_vsd_command("configurationStop").

%% start_configuration() ->
%%     comsaServDiscCommon:do_vsd_command("configurationStart").

%% confirm() ->
%%     comsaServDiscCommon:do_vsd_command("commit").


%% %%%===================================================================
%% %%% gen_server callbacks
%% %%%===================================================================


%% init([]) ->
%%     %% subscribe to MO changes
%%     mnesia:subscribe({table, serviceDiscovery, simple}),
%%     filelib:ensure_dir(?GLOBAL_CFG), %% for BPU
%%     filelib:ensure_dir(?MOM_DATA_FILE),

%%     {ok, #st{}}.
 
%% %%% ----------------------------------------------------------
%% %%% #2.2   EXPORTED INTERNAL FUNCTIONS
%% %%% ----------------------------------------------------------

%% handle_call(stop_gRpc, _From, State = #st{ glob_status_pri = undefined}) ->
%%     {reply, {ok, ok}, State};

%% handle_call(stop_gRpc = Req, _From, State) ->
%%     OsCmd = ?CONSUL_BIN ++ " leave -http-addr=" ++ ?GSDS_UNIX_SOCKET,
%%     ResGlob = os:cmd(OsCmd),
%%     ?LOG_INFO([{req, Req},
%% 	       {osCmd, OsCmd},
%% 	       {result, ResGlob}]),
%%     {reply, {ok, ResGlob}, State#st{glob_status_pri = stopped}};

%% handle_call(_Msg, _From, State) ->
%%     {reply, ok, State}.

%% %%% ----------------------------------------------------------

%% handle_cast(activate, State) ->
%%     NewState = do_activate(State),
%%     {noreply, NewState};
%% handle_cast({ip_changed,MoRef, NS, IpAddr}, S) ->
%%     {noreply, handle_ip_changed(S, binary_to_list(MoRef), binary_to_list(NS), binary_to_list(IpAddr))};

%% handle_cast(_Msg, State) ->
%%     {noreply, State}.

%% %%% ----------------------------------------------------------

%% handle_info({mnesia_table_event, Event}, S) ->
%%     New_s = handle_table_event(Event, S),
%%     {noreply, New_s};
%% handle_info({timeout, Type}, S) ->
%%     {noreply, handle_timeout(Type, S)};

%% handle_info({local_exit, _Appm_id}, S) ->
%%     {noreply, start_local_server(S)};
%% handle_info({fa_client_exit, _Appm_id}, S) ->
%%     {noreply, start_fa_client(S)};
%% handle_info({fa_server_exit, _Appm_id}, S) ->
%%     {noreply, start_fa_server(S)};
%% handle_info({gsds_client_exit, _Appm_id}, #st{glob_status_pri = GStatus} = S) ->
%%     case GStatus of
%% 	stopped ->
%% 	    %% It has intentionally been stopped and should not be restarted.
%% 	    {noreply, S};
%% 	_ ->
%% 	    {noreply, start_gsds_client(S)}
%%     end;

%% handle_info(_Info, State) ->
%%     {noreply, State}.

%% %%% ----------------------------------------------------------

%% terminate(_Reason, _State) ->
%%     ok.


%% %%% ----------------------------------------------------------

%% code_change(_OldVsn, State, _Extra) ->
%%     {ok, State}.

%% %%% ----------------------------------------------------------
%% %%% #3.    CODE
%% %%% #---------------------------------------------------------
%% %%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%% %%% #---------------------------------------------------------

%% cast(Msg) ->
%%     gen_server:cast(?SERVER_NAME, Msg).

%% call(Msg) ->
%%     gen_server:call(?SERVER_NAME, Msg).

%% %%% ----------------------------------------------------------

%% handle_table_event({delete, {serviceDiscovery, _Id}, _}, S) ->
%%     sysInitI:info_msg("~p:handle_table_event(delete)~n", [?MODULE]),   
%%     mnesia:dirty_write(
%%       #comsaServiceDiscovery{}),
%%     stop_all(S);

%% handle_table_event({write,
%% 		    SD=#serviceDiscovery{localAddress = GlipMoBin,
%% 					 trustCategory = TcMo,
%% 					 nodeCredential = NcMo,
%% 					 primaryGsds = Primary
%% 					} ,
%% 		    _}, S) ->
%%     GlipMo=binary_to_list(GlipMoBin),
%%     sysInitI:info_msg("~p:handle_table_event(write,~p)~n", [?MODULE,SD]),
    
%%     {DC,GipSrv,GipPort}=comsaServDiscCommon:get_dc_host_and_port(Primary),
%%     [R=#comsaServiceDiscovery{glipMo=RlipMo,
%% 			      tcMo=RTcMo,
%% 			      ncMo=RNcMo,
%% 			      dcPri=RDC,
%% 			      gIpSrvPri=RipSrv,
%% 			      gPortSrvPri=RipPort}] =
%% 	mnesia:dirty_read(comsaServiceDiscovery,node()),
    
%%     case {GlipMo, TcMo, NcMo, DC, GipSrv, GipPort} of
%% 	{RlipMo, RTcMo, RNcMo, RDC, RipSrv, RipPort} ->
%% 	    %% no change, do nothing
%% 	    S;
%% 	_ ->
%% 	    case comsaServDiscCommon:get_ns_and_ip(GlipMo) of
%% 		{ok, Ns, Ip} ->

%% 		    mnesia:dirty_write(
%% 		      R#comsaServiceDiscovery{
%% 			glipMo=GlipMo,
%% 			tcMo=TcMo,
%% 			ncMo=NcMo,
%% 			dcPri=DC,
%% 			glip=Ip,
%% 			glns=Ns,
%% 			gIpSrvPri=GipSrv,
%% 			gPortSrvPri=GipPort}),
%% 		    ok=register_ip_change_cb(GlipMo, ?MODULE),
%% 		    update_cert(Ns, NcMo, TcMo),
%% 		    start_fa_and_global(
%% 		      S#st{ip=Ip,
%% 			   ns=Ns,
%% 			   dc_pri=DC,
%% 			   gsds_ip_pri=GipSrv,
%% 			   gsds_port_pri=GipPort,
%% 			   tcMo=TcMo,
%% 			   ncMo=NcMo});
%% 		{error,Reason} ->
%% 		    sysInitI:error_msg(
%% 		      "~p:ootI:get_ns_and_ip failed with Reason ~p~n"
%% 		      "retrying ~n",
%% 		      [?MODULE,Reason]),
%% 		    mnesia:dirty_write(
%% 		      R#comsaServiceDiscovery{
%% 			glipMo=GlipMo,
%% 			tcMo=TcMo,
%% 			ncMo=NcMo,
%% 			dcPri=DC,
%% 			glip=undefined,
%% 			glns=undefined,
%% 			gIpSrvPri=GipSrv,
%% 			gPortSrvPri=GipPort}),
%% 		    start_timer(),
%% 		    S#st{dc_pri=DC,
%% 			 gsds_ip_pri=GipSrv,
%% 			 gsds_port_pri=GipPort}
%% 	    end
	    
%%     end.

%% %%% ----------------------------------------------------------

%% update_cert(Ns, NcMo, TcMo)
%%   when NcMo /= undefined, TcMo /= undefined ->
%%     ok=certI:update_cert_dist({Ns,binary_to_list(NcMo),binary_to_list(TcMo)});
%% update_cert(Ns, _NcMo, _TcMo) ->
%%     ok=certI:update_cert_dist(Ns).

%% %%% ----------------------------------------------------------

%% start_timer() ->
%%     timer:send_after(30000, {timeout,retryGlip}).

%% handle_timeout(retryGlip, S) ->
%%     [R=#comsaServiceDiscovery{glipMo=GlipMo,
%%                               dcPri=DC,
%%                               gIpSrvPri=GipSrv,
%%                               gPortSrvPri=GipPort}] =
%% 	mnesia:dirty_read(comsaServiceDiscovery,node()),
%%     case comsaServDiscCommon:get_ns_and_ip(GlipMo) of
%% 	{ok, Ns, Ip} ->	    
%% 	    mnesia:dirty_write(
%% 	      R#comsaServiceDiscovery{
%% 		glip=Ip,
%% 		glns=Ns}),
%% 	    NcMo = R#comsaServiceDiscovery.ncMo,
%% 	    TcMo = R#comsaServiceDiscovery.tcMo,
%% 	    ok=register_ip_change_cb(GlipMo, ?MODULE),
%% 	    update_cert(Ns, NcMo, TcMo),	    
%% 	    start_fa_and_global(S#st{ip=Ip,
%% 			             ns=Ns,
%% 				     dc_pri=DC,
%% 				     gsds_ip_pri=GipSrv,
%% 				     gsds_port_pri=GipPort,
%% 				     tcMo=TcMo,
%% 				     ncMo=NcMo}); 
%% 	{error,no_mo_ref} ->
%% 	    %% MO has been deleted during timeout
%% 	    S;
%% 	{error,Reason} ->
%% 	    sysInitI:error_msg("~p:ootI:get_ns_and_ip failed with Reason ~p~n"
%% 			       "retrying ~n",
%% 			       [?MODULE,Reason]),
%% 	    start_timer(),
%% 	    S
%%     end.

%% %%% ----------------------------------------------------------

%% is_ip_or_ns_changed(GLip,GLns,NewNs,IP)
%% when GLip /= IP; GLns /= NewNs->
%%               true;

%% is_ip_or_ns_changed(_GLip,_GLns,_NewNs,_IP)->
%%               false.

%% %%% ----------------------------------------------------------

%% handle_ip_changed(S, MoRef, NewNs, [])  ->
%%       	      sysInitI:warning_msg("~p:~p(~p,~p)Empty IP changed notification received   ~n"
%% 			       "ignoring ~n",
%% 			       [?MODULE,?FUNCTION_NAME,
%% 				MoRef, NewNs]),
%%               S;

%% handle_ip_changed(S, MoRef, NewNs, NewIp)  ->
%%     [R=#comsaServiceDiscovery{glipMo=GlipMo,
%% 			      tcMo=TcMo,
%% 			      ncMo=NcMo,
%%                               dcPri=DC,
%% 			      glip=GLip,
%% 			      glns=GLns,
%%                               gIpSrvPri=GipSrv,
%%                               gPortSrvPri=GipPort}] =
%% 	mnesia:dirty_read(comsaServiceDiscovery,node()),
    
%%     Ip = hd(string:tokens(NewIp,"/")),     
%%     Result = is_ip_or_ns_changed(GLip,GLns,NewNs,Ip),
    
%%     case Result of
%% 	true ->
	    
%% 	    sysInitI:info_msg("~p: GSDS local IP has changed to NS=~p IP=~p ~n",
%% 		      [?MODULE, NewNs, NewIp]),
%% 	    case MoRef of
%% 		GlipMo ->
		    
%% 		    mnesia:dirty_write(
%% 		      R#comsaServiceDiscovery{
%% 			glip=Ip,
%% 			glns=NewNs}),
%% 		    update_cert(NewNs, NcMo, TcMo),
%% 		    start_fa_and_global(S#st{ip=Ip,
%% 					     ns=NewNs,
%% 					     dc_pri=DC,
%% 					     gsds_ip_pri=GipSrv,
%% 					     gsds_port_pri=GipPort,
%% 					     tcMo=TcMo,
%% 					     ncMo=NcMo});
%% 		_ ->
%% 		    sysInitI:error_msg("~p:~p(~p,~p,~p)  Unknown MoRef ~p~n"
%% 				       "ignoring ~n",
%% 				       [?MODULE,?FUNCTION_NAME,
%% 					MoRef, NewNs, NewIp,GlipMo]),
%% 		    S
%% 	    end;
%% 	false ->
%% 	    sysInitI:warning_msg("~p:~p(~p,~p)  No Change in IP Or NS ~n"
%%                                  "ignoring ~n",
%%                    	         [?MODULE,?FUNCTION_NAME,
%% 				  NewNs, NewIp]),
%% 	    S
%%     end.

%% %%% --------------------------------------------------------

%% do_activate(S) ->
%%     sysInitI:info_msg("~p:activate ~n", [?MODULE]),
%%     case filelib:ensure_dir(?LOCAL_LOG) of
%% 	ok ->
%% 	    ok;
%% 	_ ->
%% 	    Cmd = ["mkdir","-p","-m","777",?LOG_DIR],
%% 	    appmPghServer:req_spawn_pgm([{args, Cmd},  {flag,1} ]),
%% 	    sysInitI:info_msg("~p:created logging directory ~p ~n", [?MODULE,?LOG_DIR])
%%     end,

%%     case mnesia:dirty_read(serviceDiscovery,{"1","1","1"}) of
%% 	[#serviceDiscovery{localAddress = GlipMoBin,
%% 			   trustCategory = TcMo,
%% 			   nodeCredential = NcMo,
%% 			   primaryGsds = Primary}] ->
%% 	    GlipMo=binary_to_list(GlipMoBin),
%% 	    {DC,GipSrv,GipPort}=comsaServDiscCommon:get_dc_host_and_port(Primary),
%% 	    case comsaServDiscCommon:get_ns_and_ip(GlipMo) of
%% 		{ok, Ns, Ip} ->
%% 		    mnesia:dirty_write(
%% 		      #comsaServiceDiscovery{
%% 			 glipMo=GlipMo,
%% 			 tcMo=TcMo,
%% 			 ncMo=NcMo,
%% 			 dcPri=DC,
%% 			 glip=Ip,
%% 			 glns=Ns,
%% 			 gIpSrvPri=GipSrv,
%% 			 gPortSrvPri=GipPort}),	       
%% 		    ok=register_ip_change_cb(GlipMo, ?MODULE),
%% 		    update_cert(Ns, NcMo, TcMo),

%% 		    start_fa_and_global(S#st{ip=Ip,
%% 					     ns=Ns,
%% 					     dc_pri=DC,
%% 					     gsds_ip_pri=GipSrv,
%% 					     gsds_port_pri=GipPort,
%% 					     tcMo=TcMo,
%% 					     ncMo=NcMo});
%% 		{error,Reason} ->
%% 		    sysInitI:error_msg(
%% 		      "~p:ootI:get_ns_and_ip failed with Reason ~p,"
%% 		      "retrying ~n",
%% 		      [?MODULE,Reason]),
%% 		    mnesia:dirty_write(
%% 		      #comsaServiceDiscovery{
%% 			 glipMo=GlipMo,
%% 			 tcMo=TcMo,
%% 			 ncMo=NcMo,
%% 			 dcPri=DC,
%% 			 glip=undefined,
%% 			 glns=undefined,
%% 			 gIpSrvPri=GipSrv,
%% 			 gPortSrvPri=GipPort}),
%% 		    start_timer(),
%% 		    S#st{dc_pri=DC,
%% 			 gsds_ip_pri=GipSrv,
%% 			 gsds_port_pri=GipPort}
%% 	    end;
%% 	[] ->
%% 	    sysInitI:info_msg(
%% 	      "~p: SDS MO not created so wait until done~n",
%% 	      [?MODULE]),
%% 	    S
%%     end.

%% %%% ----------------------------------------------------------

%% start_local_server(Sin) -> 
%%     S = stop_local(Sin),
%%     Cmd = comsaServDiscCommon:server_cmd(?LOCAL_LOG,"127.0.0.1",?LOCAL_CFG),
%%     case comsaServDiscCommon:appm_start("SD local server", local_exit, Cmd, no_ns) of
%% 	{ok, AppmId} ->
%% 	    sysInitI:info_msg("~p: Started local consul server~n",
%% 			       [?MODULE]),
%% 	    NewCnt=S#st.local_rst_cnt + 1,
%% 	    S#st{local_appm_id=AppmId, local_rst_cnt=NewCnt };
%% 	{error,Reason} ->
%% 	    sysInitI:error_msg("~p: APPM failed to start local consul~n"
%% 			       "Reason = ~p~n",
%% 			       [?MODULE,Reason]),
%% 	    S
%%     end.
	    
%% %%% ----------------------------------------------------------

%% start_fa_and_global(Sin) ->
%%     comsaServDiscCommon:create_config_file(local, Sin#st.ip, Sin#st.ns, Sin#st.dc_pri,
%% 					   Sin#st.gsds_ip_pri,  Sin#st.tcMo,  Sin#st.ncMo),
%%     comsaServDiscCommon:create_config_file(fa, Sin#st.ip, Sin#st.ns, Sin#st.dc_pri,
%% 					   Sin#st.gsds_ip_pri,  Sin#st.tcMo,  Sin#st.ncMo),
%%     comsaServDiscCommon:create_config_file(global, Sin#st.ip, Sin#st.ns, Sin#st.dc_pri,
%% 					   Sin#st.gsds_ip_pri,  Sin#st.tcMo,  Sin#st.ncMo),
%%     S = start_local_server(Sin),
%%     comsaServDiscCommon:write_mom_data_file(S#st.ip, S#st.ns),
%%     CoreState = clhI:core_state(),
%%     IsVrcs = sysEnv:vrcs(),
%%     NewState =
%% 	case {IsVrcs,CoreState}  of
%% 	    {true, active} ->
%% 		%% The Oam node in a VNF starts fa server and gsds client
%% 		Sfa = start_fa_server(S),
%% 		start_gsds_client(Sfa);
%% 	    {true, _} ->
%% 		%% The regular node in a VNF starts 
%% 		%% fa client towards OaM node
%% 		start_fa_client(S);
%% 	    {false,_ } ->
%% 		%% The BPU starts a fa server and a gsds client
%% 		Sfa = start_fa_server(S),
%% 		start_gsds_client(Sfa)
%% 	end,
%%     comsaServDiscCommon:create_domain_file(NewState#st.ip,
%% 					   NewState#st.gsds_ip_pri,
%% 					   NewState#st.gsds_port_pri).
   

%% %%% ----------------------------------------------------------

%% start_fa_server(Sin=#st{ip=Ip,ns=Ns}) ->
%%     S = stop_fa(Sin),
%%     Cmd = comsaServDiscCommon:server_cmd(?FA_LOG,Ip,?FA_CFG),
%%     case comsaServDiscCommon:appm_start("SD FA server", fa_server_exit, Cmd, list_to_binary(Ns)) of
%% 	{ok, AppmId} ->
%% 	    sysInitI:info_msg("~p: Started FA consul server~n"
%% 			       "In NS= ~p and IP= ~p~n",
%% 			       [?MODULE,Ns,Ip]),
%% 	    NewCnt=S#st.fa_rst_cnt + 1,
%% 	    S#st{fa_appm_id=AppmId, fa_rst_cnt=NewCnt };
%% 	{error,Reason} ->
%% 	    sysInitI:error_msg("~p: APPM failed to start fa server consul~n"
%% 			       "Reason = ~p~n",
%% 			       [?MODULE,Reason]),
%% 	    S
%%     end.

%% %%% ----------------------------------------------------------

%% start_fa_client(Sin=#st{ip=Ip,ns=Ns}) ->
%%     S = stop_fa(Sin),
%%     SrvIP = comsaServDiscCommon:get_oam_ip(), 
%%     Cmd = comsaServDiscCommon:client_cmd(?FA_LOG,Ip, SrvIP,"",?FA_CFG),
%%     case comsaServDiscCommon:appm_start("SD FA client", fa_client_exit, Cmd, list_to_binary(Ns)) of
%% 	{ok, AppmId} ->
%% 	    sysInitI:info_msg("~p: Started FA consul client~n"
%% 			       "In NS= ~p and IP= ~p~n",
%% 			       [?MODULE,Ns,Ip]),
%% 	    NewCnt=S#st.fa_rst_cnt + 1,
%% 	    S#st{fa_appm_id=AppmId, fa_rst_cnt=NewCnt };
%% 	{error,Reason} ->
%% 	    sysInitI:error_msg("~p: APPM failed to start FA client consul~n"
%% 			       "Reason = ~p~n",
%% 			       [?MODULE,Reason]),
%% 	    S
%%     end.

%% %%% ----------------------------------------------------------

%% start_gsds_client(Sin=#st{gsds_ip_pri=""}) ->
%%      stop_gsds_client(Sin);

%% %%% ----------------------------------------------------------

%% start_gsds_client(Sin=#st{ip=Ip,ns=Ns,gsds_ip_pri=SrvIp,gsds_port_pri=SrvPort})
%% when SrvIp == "localhost"; SrvIp == "127.0.0.1" ; SrvIp == "::1" ->
%%     S = stop_gsds_client(Sin),
%%     SIP = case SrvIp of
%% 	      "::1" -> Ip;
%% 	      _     -> SrvIp
%% 	  end,

%%     Cmd = comsaServDiscCommon:client_cmd(?GLOBAL_LOG, Ip, SIP, SrvPort, ?GLOBAL_CFG),

%%     case comsaServDiscCommon:appm_start("GSDS client", gsds_client_exit, Cmd, list_to_binary(Ns)) of
%% 	{ok, AppmId} ->
%% 	    sysInitI:info_msg("~p: Started Global consul client~n"
%% 			       "In NS= ~p and IP= ~p~n"
%% 			       "Server:IP ~p, port=  ~p~n",
%% 			       [?MODULE, Ns, Ip, SrvIp, SrvPort]),
%% 	    NewCnt=S#st.glob_rst_cnt_pri + 1,
%% 	    S#st{glob_appm_id_pri=AppmId, glob_rst_cnt_pri=NewCnt };
%% 	{error,Reason} ->
%% 	    sysInitI:error_msg("~p: APPM failed to start GSDS client consul~n"
%% 			       "Reason = ~p~n",
%% 			       [?MODULE,Reason]),
%% 	    S
%%     end;


%% start_gsds_client(Sin=#st{ip=Ip,ns=Ns,gsds_ip_pri=SrvIp,gsds_port_pri=SrvPort}) ->
%%     S = stop_gsds_client(Sin),

%%     SIP = case inet:parse_ipv4_address(SrvIp) of
%%           {ok, _} ->
%%                     SrvIp;
%%           {error, _} ->
%%                     case inet:parse_ipv6_address(SrvIp) of
%%                     {ok, _} ->
%%                               SrvIp;
%%                     {error, _} ->
%%                               ""
%%                     end
%%           end,

%%     Cmd = case SIP of
%%                "" ->  
%%                     ClusterIps = comsaServDiscCommon:dns_check(SrvIp,Ns),
%%                     comsaServDiscCommon:client_cmd(?GLOBAL_LOG, Ip, SrvIp, SrvPort, ClusterIps, ?GLOBAL_CFG);
%%                _ 
%%                   ->   
%%                     comsaServDiscCommon:client_cmd(?GLOBAL_LOG, Ip, SrvIp, SrvPort, ?GLOBAL_CFG)
%%           end,

%%     case comsaServDiscCommon:appm_start("GSDS client", gsds_client_exit, Cmd, list_to_binary(Ns)) of
%% 	{ok, AppmId} ->
%% 	    sysInitI:info_msg("~p: Started Global consul client~n"
%% 			       "In NS= ~p and IP= ~p~n"
%% 			       "Server:IP ~p, port=  ~p~n",
%% 			       [?MODULE, Ns, Ip, SrvIp, SrvPort]),
%% 	    NewCnt=S#st.glob_rst_cnt_pri + 1,
%% 	    S#st{glob_appm_id_pri=AppmId, glob_rst_cnt_pri=NewCnt };
%% 	{error,Reason} ->
%% 	    sysInitI:error_msg("~p: APPM failed to start GSDS client consul~n"
%% 			       "Reason = ~p~n",
%% 			       [?MODULE,Reason]),
%% 	    S
%%     end.



%% %%%===================================================================
%% %%% DNS Poll
%% %%%===================================================================

%% stop_all(S) ->
%%     stop_local(
%% 	stop_fa(
%% 	  stop_gsds_client(S))).
    
%% stop_local(S) ->
%%     NewState =
%% 	case S#st.local_appm_id of
%% 	    undefined ->
%% 		S;
%% 	    AppmId ->
%% 		appmServer:signal_to_internal_pgm(AppmId, ?SIGHUP),
%% 		timer:sleep(100),
%% 		appmServer:stop_internal_lm(AppmId),
%% 		S#st{local_appm_id=undefined}
%% 	end,
%%     os:cmd("rm -rf " ++ ?LOCAL_TMP),
%%     NewState.
    
%% stop_fa(S) ->
%%     NewState =
%% 	case S#st.fa_appm_id of
%% 	    undefined ->
%% 		S;
%% 	    AppmId ->
%% 		appmServer:signal_to_internal_pgm(AppmId, ?SIGHUP),
%% 		timer:sleep(100),
%% 		appmServer:stop_internal_lm(AppmId),
%% 		S#st{fa_appm_id=undefined}
%% 	end,
%%     os:cmd("rm -rf " ++ ?FA_TMP),
%%     NewState.
    
%% stop_gsds_client(S) ->
%%     NewState =
%% 	case S#st.glob_appm_id_pri of
%% 	    undefined ->
%% 		S;
%% 	    AppmId ->
%% 		appmServer:signal_to_internal_pgm(AppmId, ?SIGHUP),
%% 		timer:sleep(100),
%% 		appmServer:stop_internal_lm(AppmId),
%% 		S#st{glob_appm_id_pri=undefined}
%% 	end,
%%     os:cmd("rm -rf " ++ ?GLOBAL_TMP),
%%     NewState.

%% %%% --------------------------------------------------------

%% register_ip_change_cb(GlipMo, Module) ->
%%     ootI:register_ip_change_cb(GlipMo, Module).

%% %%% --------------------------------------------------------

