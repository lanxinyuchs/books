%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmaLicenseUser.erl %
%%% @author etxpejn
%%% @copyright Ericsson AB 2014-2015
%%% @version /main/R3A/12
%%% @doc ==LMA license user module==
%%% This module is a server that handles the communication over LIHI for license users within CS.
%%% @end
%%%
%%% ----------------------------------------------------------

-module(lmaLicenseUser).
-behaviour(gen_server).
-vsn('/main/R3A/12').
-date('2015-04-07').
-author('etxpejn').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2015 All rights reserved.
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
%%% R3A/1     2014-10-02   etxpejn    Created
%%% R3A/5     2014-10-09   etxpejn    Added subscribe_over_lihi/0
%%% R3A/6     2014-10-10   etxpejn    Added call to logStreamServer:license_state
%%% R3A/7     2014-11-26   etxpejn    Added lfci_feature_license_disconnect_ind
%%% R3A/9     2015-03-24   etxpejn    WP4505 - Changes CXC no for RTSEL
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

%%% Called from supervisor
-export([start_link/0,
	 stop/0]).
-export([activate/0]).
-export([get_state/0]).
-export([subscribe_over_lihi/0]).


%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ---------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(RTSEL_CXC, "CXC4040010").
-define(CMPV2_CXC, "CXC4011817").


-record(state, {ns_spid = undefined,
		itc_port = undefined,
		license_spid = undefined,
		rtsel_license = undefined,
		cmpv2_license = undefined,
		server_ref = undefined}).

%%% ----------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

    
%%% ----------------------------------------------------------
%%% @doc Starts the lmaLicenseUser server process
%%% @private
%%% @end
%%% ----------------------------------------------------------
start_link() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

%%% ----------------------------------------------------------
%%% @doc Stops the lmaLicenseUser server process
%%% @private
%%% @end
%%% ----------------------------------------------------------
stop() ->
    gen_server:cast(?MODULE, stop).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% @doc Fetches the server process information
%%% @private
%%% @end
%%% ----------------------------------------------------------
get_state() ->
     gen_server:call(?MODULE, get_state).

activate() ->
    gen_server:cast(?MODULE, activate).

subscribe_over_lihi() ->
    gen_server:call(?MODULE, subscribe_over_lihi).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%%--------------------------------------------------------------------
%%% #           init(Args)
%%% @private
%%% @doc
%%% Initializes the server
%%%
%%% @spec init(Args) -> {ok, State} |
%%%                     {ok, State, Timeout} |
%%%                     ignore |
%%%                     {stop, Reason}
%%% @end
%%%--------------------------------------------------------------------
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.   	  
    
%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

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
handle_call(get_state, _From, State) ->
    log("Got get_state request"),
    {reply, State, State};
handle_call(subscribe_over_lihi, _From, #state{ns_spid = Spid,
					       itc_port = ItcPort} = State) ->
    log("Setup subscription over LIHI"),
    lmaLinxHandler:send_ns_hunt_req(ItcPort, Spid),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    log("Got Request in handle_call"),
    Reply = ok,
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
handle_cast(activate, State) ->
    log("Time to start hunt for LIHI"),
    %% Start up ITC
    ItcPort = itc:open(atom_to_list(?MODULE)),
    HuntRef = itc:hunt(ItcPort, "ose_ns"),
    Spid = receive 
	       {mailbox_up, ItcPort, HuntRef, S} -> 
		   S
	   after 20000 ->
		   undefined
	   end,
    ok = itc:listen(ItcPort),
    lmaLinxHandler:send_ns_hunt_req(ItcPort, Spid),
    {noreply, State#state{ns_spid = Spid,
			  itc_port = ItcPort}};
handle_cast(stop, State) ->
    {stop, normal, State};
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
handle_info({message, ItcPort, {LicenseSpid, _ToMboxId, SigNo, Data}},
	    #state{ns_spid = Spid, itc_port = ItcPort, server_ref = _ServerRef} = State) ->
    case lmaLinxHandler:parse_itc({SigNo, Data}) of
	
	{lfci_hunt_ok} ->
	    log("lfci_hunt_ok, send_conn_to_server"),
	    ok = lmaLinxHandler:send_conn_to_server(ItcPort, LicenseSpid),
	    {noreply, State#state{license_spid = LicenseSpid}};
    
	{lfci_conn_to_server_cfm, NewServerRef} ->
	    log("lfci_conn_to_server_cfm"),

	    %% Subscribe for RTSEL license state
	    log("send_feature_subscribe_req for RTSEL, CXC4040010"),
	    RTSEL_LicenseKeyId = <<?RTSEL_CXC>>, 
	    RTSEL_LicenseMoId = <<"RTSEL">>, 
	    RTSEL_LicenseName = <<"Real Time Security Event Logging">>,
	    ok = lmaLinxHandler:send_feature_subscribe_req(ItcPort, LicenseSpid, NewServerRef, 
	    						   RTSEL_LicenseKeyId, RTSEL_LicenseMoId, 
	    						   RTSEL_LicenseName),
	    %% Subscribe for CMPv2 license state
	    %% log("send_feature_subscribe_req for CMPv2, CXC4011817"),
	    %% CMPV2_LicenseKeyId = <<?CMPV2_CXC>>, 
	    %% CMPV2_LicenseMoId = <<"CMPV2">>, 
	    %% CMPV2_LicenseName = <<"Certificate Management Protocol version 2">>,
	    %% ok = lmaLinxHandler:send_feature_subscribe_req(ItcPort, LicenseSpid, NewServerRef, 
	    %% 						   CMPV2_LicenseKeyId, CMPV2_LicenseMoId, 
	    %% 						   CMPV2_LicenseName),
	    {noreply, State#state{server_ref = NewServerRef}};
	
	{lfci_feature_license_sub_cfm, ?RTSEL_CXC, LicenseState} ->
	    log("lfci_feature_license_sub_cfm for RTSEL, CXC4040010 with LicenseState: " 
		++ atom_to_list(LicenseState)),
	    logStreamServer:license_state(LicenseState),
	    {noreply, State#state{rtsel_license = LicenseState}};
	
	{lfci_feature_license_sub_cfm, ?CMPV2_CXC, LicenseState} ->
	    log("lfci_feature_license_sub_cfm for CVPM2, CXC4011817 with LicenseState: " 
		++ atom_to_list(LicenseState)),
	    {noreply, State#state{cmpv2_license = LicenseState}};
	
	{lfci_feature_license_change_ind, ?RTSEL_CXC, LicenseState} ->
	    log("lfci_feature_license_change_ind for RTSEL, CXC4040010 with LicenseState: " 
		++ atom_to_list(LicenseState)),
	    logStreamServer:license_state(LicenseState),
	    {noreply, State#state{rtsel_license = LicenseState}};
	
	{lfci_feature_license_change_ind, ?CMPV2_CXC, LicenseState} ->
	    log("lfci_feature_license_change_ind for CMPV2, CXC4011817 with LicenseState: " 
		++ atom_to_list(LicenseState)),
	    {noreply, State#state{cmpv2_license = LicenseState}};
	{lfci_feature_license_disconnect_ind, ErrorInfo} ->
	    log("lfci_feature_license_disconnect_ind due to: "++ErrorInfo),
	    %% Disable the license
	    logStreamServer:license_state(inoperable),
	    %% At disconnect a new subscribe is needed.
	    lmaLinxHandler:send_ns_hunt_req(ItcPort, Spid),
	    {noreply, State#state{rtsel_license = inoperable}};
	{unknown_signal, SigNo} ->
	    log(warning, "Unknown signal with No:" ++ integer_to_list(SigNo)),
	    {noreply, State}
    end;
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
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



log(String) ->
    log("lmaLicenseUser", info, String).

log(Severity, String) ->
    log("lmaLicenseUser", Severity, String).

log(M, Severity, String) ->
    logI:write_log("LicensingLog", M, Severity, String).



%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

