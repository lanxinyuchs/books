%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lmaGlms.erl %
%%% @author echhedb
%%% @copyright Ericsson AB 2013-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R11A/R12A/1
%%% @doc ==LMA server module==
%%% This module is the server within LMA and handels call to and from GLMS
%%% @end
%%%
%%% ----------------------------------------------------------

-module(lmaGlms).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R8A/R9A/R11A/R12A/1').
-date('2017-11-02').
-author('echhedb').

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2013-2017 All rights reserved.
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
%%% R4A/1      2015-05-29   etxpejn     Corrected HT77481, set_fingerprint and validate
%%% R4A/7      2015-06-09   etxpejn     No call to do_init_data_tables in initialize
%%% R4A/8      2015-06-15   etxpejn     Do not delete LKF in /rcs/networkloader/
%%% R4A/9      2015-06-17   etxpejn     WP4530: Check if test MOM should be loaded or not
%%% R4A/13     2015-06-24   etxpejn     Added functionality for IP sec - embargo
%%% R4A/14     2015-06-29   etxpejn     Test MOM can only be added in AI
%%% R4A/15     2015-07-26   etxpejn     HT92440: Added read_mo_grace_period
%%% R4A/16     2015-08-17   etxpejn     Added sync as option to file:write_file
%%% R4A/17     2015-08-18   etxpejn     Removed logging at dump rsp OK 
%%% R4A/18     2015-08-25   etxpejn     Removed logging during upgrade, added log2/1 
%%% R4A/19     2015-08-31   etxpejn     Prolonged heartbeat timer towards GLMS.
%%% R4A/20     2015-09-03   etxpejn     Handle error that LKF on node has no contain.
%%% R4A/21     2015-09-10   etxpejn     Removed logging from log2/1.
%%% R4A/21     2015-09-10   etxpejn     Removed logging from log2/1.
%%% R4A/22     2015-10-06   etxpejn     Removed more logs, prolonged heartbeat timer to 60s
%%%                                     and improved fault code if local LKF is used.
%%% R5A/1      2015-11-09   etxpejn     Fixed report_progress after restart
%%% R5A/3      2015-12-03   etxpejn     Take timeOfLastStatusUpdate from GLMS
%%% R5A/4      2016-01-25   etxpejn     Added coorelation with LMA alarms and lic appl.
%%% R5A/5      2016-01-27   etxpejn     Corr when backup and 2 different LKFs has the same seqno
%%% R5A/6      2016-02-01   etxpejn     Backup restore with empty mnesia, clarify printouts
%%% R5A/7      2016-02-05   etxpejn     Added encrypt_esi_log for WP4716, looking for CXC4012032
%%%                                     Corr in failsafe and 2 different LKFs has the same seqno
%%% R5A/8      2016-02-10   etxpejn     Added Alarm Correlation Event Id in alarm
%%% R5A/9      2016-02-12   etxpejn     Changed Alarm Correlation Event Id to eventId
%%% R5A/10     2016-04-05   etxpejn     Correction in dump_*_rsp 
%%% R6A/1      2016-06-15   ekurnik     HU92208 Update of additional text for 
%%%                                     License Key Not Available alarm.
%%% R6A/2      2016-06-20   etxpejn     Save EU on disk and not in mnesia so that it can't be 
%%%                                     reused by restoring an old backup 
%%% R6A/3      2016-06-22   etxpejn     Only add description to license key N/A alarm if it exists 
%%% R6A/5      2016-07-08   ekurnik     Fixed Licence key N/A alarm text
%%% R6A/6      2016-08-09   etxpejn     HU98808 fixed in encrypt_esi_log/0
%%% R6A/7      2016-08-23   etxberb     HU98808 additional fix.
%%% R8A/1      2016-12-22   etxpejn     Added info in log when PKI fails due to exired cert
%%% R8A/3      2017-01-25   eivmiha     Switched ssh_sftp and sysSftp to ftpI
%%% R9A/1      2017-02-06   ekurnik     ftpI:parse_uri() added for sftp and ftpes
%%% R9A/2      2017-04-11   etxpejn     HV32829, added license_key_exiration_alarm
%%% R10A/1     2017-04-20   etxpejn     Changed -15 to -9 for killing GLMS
%%% R11A/1     2017-07-13   qselert     SP086 changes
%%% R11A/2     2017-07-14   etxarnu     Fix wrong include file name (_mp remvd) 
%%%                                     Included R10A/1 changes
%%% R11A/4     2017-09-04   etxjotj     Replaced swmFailsafe with swmI
%%% R11A/5     2017-09-08   etxpejn     Prolong timeout from 10 to 20 sec - prob seen in PU
%%% R11A/6     2017-10-12   etxpejn     Rermoved call to undefined lmaLinxHandler:send_install_area_keyfile_req
%%% R12A/1     2017-10-31   echhedb     SP086: Added more LicenseSupport functionality.
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
-export([activate_eu/0]).
-export([activate_iu/0]).
-export([initialize_license_connection/0]).
-export([install_area_license_keys/1]).
-export([dump_capacity_key_data_req/0]).
-export([dump_feature_key_data_req/0]).
-export([dump_glms_state_data_req/0]).
-export([dump_lcci_client_data_req/0]).
-export([dump_lfci_client_data_req/0]).
-export([encrypt_esi_log/0]).
-export([get_state/0]).
-export([install_key_file/2]).
-export([refresh_license_inventory/0]).
-export([kill_glms/0]).
-export([set_fingerprint/1]).
-export([set_featurestate/2]).
-export([start_glms/0]).
-export([to_string/1]).
-export([update_gp_attributes/4]).
-export([set_licenseAreaId/1]).
-export([notify_ready_for_service/0]).

%% COLI
-export([pu/1]).
%% COLI For test only
-export([adjust_glms_timer/1]).
-export([adjust_glms_timer_test/1]).

%% For test only
-export([change_test_mode/1]).
-export([test_function_for_alarm/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ---------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("lmaGlmsSig.hrl").
-include("RcsLM.hrl").
-include("lma.hrl").

-define(HEARTBEAT_INTERVAL, 60000).
-define(RESET_RESTART_TIMER, 180000).
-define(TIMEOUT, 20000).
-define(LONG_TIMEOUT, 900000). % 15 min

%% General
-define(ELSE, true).
-define(FUNCTION, 
	element(2, element(2, process_info(self(), current_function)))).

-record(state, {port = undefined,
		pid = undefined,
		spid = undefined,
		itc_port = undefined,
		glms_alive = not_started,
		glms_restarted = {false, default},
		ecim_pid = undefined,
		action_id = 0,
		timer_ref = undefined,
		test_mode = off,
		queue = queue:new(),
		encryption = true}).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
activate() ->
    gen_server:cast(?MODULE, activate).

%%% ----------------------------------------------------------
%%% @doc Emergency Unlock activation
%%% @private
%%% @end
%%% ----------------------------------------------------------
activate_eu() ->
    gen_server:call(?MODULE, activate_eu, ?TIMEOUT).

%%% ----------------------------------------------------------
%%% @doc Integration Unlock activation
%%% @private
%%% @end
%%% ----------------------------------------------------------
activate_iu() ->
    gen_server:call(?MODULE, activate_iu, ?TIMEOUT).

%%% ----------------------------------------------------------
%%% @doc Initialize License Connection
%%% @private
%%% @end
%%% ----------------------------------------------------------
initialize_license_connection() ->
    gen_server:call(?MODULE, initialize_license_connection, ?TIMEOUT).

%%% ----------------------------------------------------------
%%% @doc Initialize License Connection
%%% @private
%%% @end
%%% ----------------------------------------------------------
install_area_license_keys(Keys) ->
    gen_server:call(?MODULE, {install_area_license_keys, Keys}, ?TIMEOUT).

%%% ----------------------------------------------------------
%%% @doc Adjust the GLMS time, this should only be used for test purpose
%%% @end
%%% ----------------------------------------------------------
adjust_glms_timer(["-d"]) ->
    log(warning, "Adjusting GLMS timer back to default"),
    gen_server:cast(?MODULE, {adjust_glms_timer, default});
adjust_glms_timer(["-a"]) ->
    Timer = 86400, % 1 day turns into 1 sec
    log(warning, "Adjusting GLMS timer to: " ++ integer_to_list(Timer)),
    gen_server:cast(?MODULE, {adjust_glms_timer, Timer});
adjust_glms_timer(Args) ->
    log(warning, "Adjust timer called with faulty argument: " ++ Args).

adjust_glms_timer_test(Timer) when is_integer(Timer) ->
    log(warning, "Adjusting GLMS timer to: " ++ integer_to_list(Timer)),
    gen_server:cast(?MODULE, {adjust_glms_timer, Timer}).

%%% ----------------------------------------------------------
%%% @doc Change test mode on GLMS server in order to avoid PKI verifications
%%%      of LKF files. This should only be used for test purpose
%%% @end
%%% ----------------------------------------------------------
-spec change_test_mode(atom()) -> ok.

change_test_mode(Mode) ->
    log(warning, "Changing test mode on GLMS server to: "++atom_to_list(Mode)),
    gen_server:cast(?MODULE, {change_test_mode, Mode}).


info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%%% ----------------------------------------------------------
%%% @doc Request a dump of current capacity key data.
%%% @private
%%% @end
%%% ----------------------------------------------------------
dump_capacity_key_data_req() ->
    gen_server:cast(?MODULE, dump_capacity_key_data_req).

%%% ----------------------------------------------------------
%%% @doc Request a dump of current feature key data.
%%% @private
%%% @end
%%% ----------------------------------------------------------
dump_feature_key_data_req() ->
    gen_server:cast(?MODULE, dump_feature_key_data_req).

%%% ----------------------------------------------------------
%%% @doc Request a dump of current GLMS status data.
%%% @private
%%% @end
%%% ----------------------------------------------------------
dump_glms_state_data_req() ->
    gen_server:cast(?MODULE, dump_glms_state_data_req).

%%% ----------------------------------------------------------
%%% @doc Request a dump of current LCCI client statuses from GLMS
%%% @private
%%% @end
%%% ----------------------------------------------------------
dump_lcci_client_data_req() ->
    gen_server:cast(?MODULE, dump_lcci_client_data_req).

%%% ----------------------------------------------------------
%%% @doc Request a dump of current LFCI client statuses from GLMS
%%% @private
%%% @end
%%% ----------------------------------------------------------
dump_lfci_client_data_req() ->
    gen_server:cast(?MODULE, dump_lfci_client_data_req).

%%% ----------------------------------------------------------
%%% @doc Find out if the license CXC4012032 is installed
%%%      i.e. if the ESI should be encrypt or not.
%%% @private
%%% @end
%%% ----------------------------------------------------------
encrypt_esi_log() ->
    %% HU98808 Added case for gen_server:call
    try
	{ok, Result} = gen_server:call(?MODULE, encrypt_esi_log, 10000),
	Result
    catch
	ErrClass : ErrReason ->
	    log(error, "Failed to get encrypted esi state, use encrypted"),
	    error_logger:error_report
	      ([{gen_server, call, [?MODULE, ?FUNCTION]},
		{ErrClass, ErrReason}
		| sysUtil:pid_info(whereis(?MODULE), {all, []})]),
	    true
    end.

%%% ----------------------------------------------------------
%%% @doc Installing a new key file
%%% @private
%%% @end
%%% ----------------------------------------------------------
install_key_file(Uri, Password) ->
    gen_server:call(?MODULE, {install_key_file, Uri, Password}, ?TIMEOUT).

%%% ----------------------------------------------------------
%%% @doc Inventory refresh will re-read the Key File and perform a 
%%%      license validation
%%% @private
%%% @end
%%% ----------------------------------------------------------
refresh_license_inventory() ->
    gen_server:call(?MODULE, refresh_license_inventory, ?TIMEOUT).

%%% ----------------------------------------------------------
%%% @doc The mnesia transaction has been aborted, glms needs to be
%%%      restarted in order to have the same view as the adaptor.
%%% @private
%%% @end
%%% ----------------------------------------------------------
kill_glms() ->
    gen_server:cast(?MODULE, kill_glms).

%%% ----------------------------------------------------------
%%% @doc Production Unlock call via COLI
%%% @private
%%% @end
%%% ----------------------------------------------------------
pu(["-d"]) ->
    log("Deactivate PU called"),
    Result = 
	try gen_server:call(?MODULE, deactivate_pu, ?TIMEOUT) of
	    {ok, pu_deactivated, {_Expiration,_ActivationsLeft,ActivationState,_PuDeactivated}} ->
		lmaLib:convert_pu_state(ActivationState);
	    {error,pu_not_deactivated, {_Expiration,_ActivationsLeft,ActivationState,
					_PuDeactivated}} ->
		lmaLib:convert_pu_state(ActivationState)
	catch _:Reason ->
		log(warning, "Deactivate PU failed"),
		lmaLib:error_msg("Deactivate PU failed: ~p~n", [Reason]),
		nok	    
	end,  
    io:format("PU result: ~p~n", [Result]);
pu(["-a"]) ->
    log("Activate PU called"),
    Result = 
	try gen_server:call(?MODULE, activate_pu, ?TIMEOUT) of
 	    {ok, pu_activated, {Expiration,_ActivationsLeft,ActivationState,_PuDeactivated}} ->
		State = lmaLib:convert_pu_state(ActivationState),
		State ++ " until " ++ Expiration;
	    {error, pu_not_activated, {_Expiration,_ActivationsLeft,ActivationState,
				       _PuDeactivated}} ->
		lmaLib:convert_pu_state(ActivationState)
	catch _:Reason ->
		log(warning, "Activate PU failed"),
		lmaLib:error_msg("Activate PU failed: ~p~n", [Reason]),
		nok
	end,
    io:format("PU result: ~p~n", [Result]);
pu([]) ->
    pu(["-a"]) ; 
pu([Args]) ->
    log(warning, "PU called with faulty argument: " ++ Args).

%%% ----------------------------------------------------------
%%% @doc A request to change the administratively state of a feature.
%%% @private
%%% @end
%%% ----------------------------------------------------------
set_featurestate(_FeatureState, _FeatureStateId) ->
    %% In order to avoid mensia deadlock the change feature_state call to GLMS was moved to 
    %% handle_info({mnesia_table_event, {write, featureState. 
    %% Problem seen during AI when featureState was changed in siteBasicFilePath. Both 
    %% lmaLib and setMoAttributes try to do mnesia:transaction.
    {ok, feature_state_change}.

set_licenseAreaId(LicenseAreaId) ->
    gen_server:call(?MODULE, {send_update_area_id_req, LicenseAreaId}, ?TIMEOUT).

%%% ----------------------------------------------------------
%%% @doc A request to update the fingerprint attribute.
%%% @private
%%% @end
%%% ----------------------------------------------------------
set_fingerprint(BinFingerprint) ->
    %% HT77481
    %% To be able to use validate the checks for changing fingerprint needs to be done
    %% in the erlang part of LMA and not i GLMS.
    [Lm] = mnesia:dirty_read(lm, {"1","1","1"}),
    Fingerprint = binary_to_list(BinFingerprint),
    case Lm#lm.fingerprint of
	Fingerprint ->
	    %% The fingerprint has not changed
	    {ok, fingerprint_updated};
	_ ->
	    case Lm#lm.fingerprintUpdateable of
		true ->
		    {ok, fingerprint_updated};
		false ->
		    {error, <<"Fingerprint not possible to update">>}
	    end
    end.

%%% ----------------------------------------------------------
%%% @doc Starts the lmaGlms server process
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
%%% @doc Stop the lmaGlms server process
%%% @private
%%% @end
%%% ----------------------------------------------------------
stop() ->
    gen_server:cast(?MODULE, stop).

%%% ----------------------------------------------------------
%%% @doc A request to GLMS to update the GP attributes
%%% @private
%%% @end
%%% ----------------------------------------------------------
update_gp_attributes(GracePeriodId, GpActivationThreshold, GpResetThreshold, GpLength) when 
      is_list(GracePeriodId) ->
    gen_server:cast(?MODULE, {update_gp_attributes, GracePeriodId, GpActivationThreshold, 
			      GpResetThreshold, GpLength}).


%%% ----------------------------------------------------------
%%% @doc AIC callback
%%% @private
%%% @end
%%% ----------------------------------------------------------
notify_ready_for_service() ->
    gen_server:cast(?MODULE, notify_ready_for_service),
    ok.


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% @doc Fetches the lmaGlms server process information
%%% @private
%%% @end
%%% ----------------------------------------------------------
get_state() ->
     gen_server:call(?MODULE, get_state).

test_function_for_alarm(Action) ->
    gen_server:cast(?MODULE, {test_function_for_alarm, Action}).

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
    case sysEnv:role() of
	active ->
	    log("Init lmaGlms on active MP"),
	    gen_server:cast(self(), {initialize, _Args}),
	    {ok, #state{}};
	regular ->
	    log("Init lmaGlms on regular MP, dont start the lmaGlms server"),
	    ignore
    end.

initialize() ->
    log("Initialize lmaGlms"),
    process_flag(trap_exit, true),
    lmaModel:init(),
    lmaLicenseSupportModel:init(),
    mnesia:subscribe({table, featureState, detailed}),
    mnesia:subscribe({table, lm, detailed}),
    case sysInitI:restart_type() of
	local ->
	    log("Init, local restart, activate lmaLicenseUser"), 
	    activate();
	_ ->
	    do_nada
    end,
    case catch swmI:is_failsafe_restart() of
	false ->
	    do_nada;
	true ->
	    %% Check if the LKF in this backup has a lower sequense no 
	    %% then the one, if any, stored at the node. It that case remove
	    %% the LKF on node.
	    log(warning, "Restart is due to failsafe"),
	    failsafe_find_lkf();
	_ ->
	    log(error, "swmI:is_failsafe_ongoing does not exist")
    end,
    case start_glms() of
	{error,Reason} ->
	    erlang:error(Reason),
	    {stop, Reason};
	{Port, GlmsPid} ->
	    %% Start up ITC
	    ItcPort = itc:open("lma"),
	    HuntRef = itc:hunt(ItcPort, "glms"),
	    Spid = receive 
		       {mailbox_up, _ItcPort, HuntRef, S} -> 
			   log("Receive mailbox up"),
			   S
		   after ?TIMEOUT ->
			   log(error, "Hunt for glms failed"),
			   undefined
		   end,
	    ok = itc:listen(ItcPort),
	    send_activate_to_glms(ItcPort, Spid),
	    TimerRef = start_heartbeat(),
	    
	    {noreply, #state{port = Port,
			     pid = GlmsPid,
			     spid = Spid,
			     itc_port = ItcPort,
			     timer_ref = TimerRef
			    }}
    end.
    
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
handle_call(activate_eu, From, 
	    #state{itc_port = ItcPort, spid = Spid, queue = Queue} = State) ->
    lmaLinxHandler:send_activate_eu(ItcPort, Spid),
    {noreply, State#state{queue = queue:in(From, Queue)}};
handle_call(activate_iu, From,
	    #state{itc_port = ItcPort, spid = Spid, queue = Queue} = State) ->
    lmaLinxHandler:send_activate_iu(ItcPort, Spid),
    {noreply, State#state{queue = queue:in(From, Queue)}};
handle_call(activate_pu, From,
	    #state{itc_port = ItcPort, spid = Spid, queue = Queue} = State) ->
    lmaLinxHandler:send_activate_pu(ItcPort, Spid),
    {noreply, State#state{queue = queue:in(From, Queue)}};
handle_call(deactivate_pu, From, 
	    #state{itc_port = ItcPort, spid = Spid, queue = Queue} = State) ->
    lmaLinxHandler:send_deactivate_pu(ItcPort, Spid),
    {noreply, State#state{queue = queue:in(From, Queue)}};
handle_call(encrypt_esi_log, _From, #state{encryption = Encryption} = State) ->
    log("Got encrypt_esi_log request, return: "++ atom_to_list(Encryption)),
    {reply, {ok, Encryption}, State};
handle_call(get_state, _From, State) ->
    log("lmaGlms, got get_state request"),
    {reply, State, State};
handle_call({install_key_file, Uri, Password}, From,
	    #state{itc_port = ItcPort, spid = Spid, queue = Queue} = State) ->
    log("Installing key file with URI: "++ to_string(Uri)),
    lmaLinxHandler:send_install_keyfile_req(ItcPort, Spid, Uri, Password),
    {noreply, State#state{queue = queue:in(From, Queue)}};
handle_call(refresh_license_inventory, From,
	    #state{itc_port = ItcPort, spid = Spid, queue = Queue} = State) ->
    lmaLinxHandler:send_refresh_license_inventory(ItcPort, Spid),
    {noreply, State#state{queue = queue:in(From, Queue)}};
%SP086
% handles action LicenseSupport.installAreaLicenseKeys
% Example Keys string: 
% v1.0@asdf@AREA12345Kista@1@f:CXC4040999:1000:2000@f:cxc002:1000:0@emergencyReset:0:-1@f:cxc003:1000:2000:100
handle_call({install_area_license_keys, Keys}, From,
	    #state{itc_port = ItcPort, spid = Spid, queue = Queue} = State) ->
    log("Installing key file with Keys " ++ Keys),
    CreateStateMos = case aicI:is_ready_for_service() of
	    false ->
			   log("AI is ongoing"),
			   info_msg("---AI is ongoing ~n", []),
			   1;
	    _true ->
			   info_msg("---AI is NOT ongoing ~n", []),
			   0
	end,
	info_msg("---CreateStateMos ~p~n", [CreateStateMos]),
    lmaLinxHandler:send_install_area_license_keys_req(ItcPort, Spid, CreateStateMos, Keys),
    {noreply, State#state{queue = queue:in(From, Queue)}};
handle_call(initialize_license_connection, From,
	    #state{itc_port = _ItcPort, spid = _Spid, queue = Queue} = State) ->
    log("Initialize "),
    %TODO: 
    {noreply, State#state{queue = queue:in(From, Queue)}};
%SP086
handle_call({send_update_area_id_req, LicenseAreaId}, From,
	    #state{itc_port = ItcPort, spid = Spid, queue = Queue} = State) ->
    lmaLinxHandler:send_update_area_id_req(ItcPort, Spid, LicenseAreaId),
    {noreply, State#state{queue = queue:in(From, Queue)}};
handle_call(_Request, _From, State) ->
    log("lmaGlms, got Request in handle_call"),
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
    aicI:register_ready_for_service_cb(?MODULE),  %
    lmaLicenseUser:activate(),
    {noreply, State};
handle_cast({adjust_glms_timer, Timer}, State) ->
    kill_glms(),
    {noreply, State#state{glms_restarted = {false, Timer}}};
handle_cast({change_test_mode, Mode}, State) ->
    {noreply, State#state{test_mode = Mode}};
handle_cast(dump_capacity_key_data_req, #state{itc_port = ItcPort, spid = Spid} = State) ->
    log("Send dump_capacity_key_data_req"),
    lmaLinxHandler:send_dump_capacity_key_data_req(ItcPort, Spid),
    {noreply, State};
handle_cast(dump_feature_key_data_req, #state{itc_port = ItcPort, spid = Spid} = State) ->
    log("Send dump_feature_key_data_req"),
    lmaLinxHandler:send_dump_feature_key_data_req(ItcPort, Spid),
    {noreply, State};
handle_cast(dump_glms_state_data_req, #state{itc_port = ItcPort, spid = Spid} = State) ->
    log("Send dump_glms_state_data_req"),
    lmaLinxHandler:send_dump_glms_state_data_req(ItcPort, Spid),
    {noreply, State};
handle_cast(dump_lcci_client_data_req, #state{itc_port = ItcPort, spid = Spid} = State) ->
    log("Send dump_lcci_client_data_req"),
    lmaLinxHandler:send_dump_lcci_client_data_req(ItcPort, Spid),
    {noreply, State};
handle_cast(dump_lfci_client_data_req, #state{itc_port = ItcPort, spid = Spid} = State) ->
    log("Send dump_lfci_client_data_req"),
    lmaLinxHandler:send_dump_lfci_client_data_req(ItcPort, Spid),
    {noreply, State};
handle_cast({initialize, _Args}, _State) ->
    %% This is to avoid blocking other applications
    initialize();
handle_cast(stop, #state{pid = Pid, timer_ref = TimerRef} = S) ->
    erlang:cancel_timer(TimerRef),
    PidStr = integer_to_list(Pid),
    SigStr = integer_to_list(9), % Kill
    log("Send kill to GLMS pid"),
    os:cmd("kill -"++SigStr++" "++PidStr),
    {stop, normal, S};
handle_cast(kill_glms, #state{pid = Pid, timer_ref = TimerRef} = State) ->
    erlang:cancel_timer(TimerRef),
    PidStr = integer_to_list(Pid),
    SigStr = integer_to_list(9), % Kill
    log("Send kill -9 to GLMS pid"),
    os:cmd("kill -"++SigStr++" "++PidStr),
    {noreply, State};
handle_cast({test_function_for_alarm, Action}, State) ->
    log(warning, "test_function_for_alarm called! Changing Keyfile Fault alarm with action:" 
	++ atom_to_list(Action)),
    Dn = [list_to_binary("ManagedElement=1"),
	  list_to_binary("SystemFunctions=1"),
	  list_to_binary("Lm=1")],
    case Action of
	raise ->
	    comsaI:send_alarm('KeyFileFault', critical, Dn, "");
	cease ->
	    comsaI:clear_alarm('KeyFileFault', Dn)
    end,
    {noreply, State};
handle_cast({update_gp_attributes, GracePeriodId, GpActivationThreshold, GpResetThreshold, 
	     GpLength}, #state{itc_port = ItcPort, spid = Spid} = State) ->
    log("Update GP attribute called for "++GracePeriodId++" with GpActivationThreshold: "
	++integer_to_list(GpActivationThreshold)++", GpResetThreshold: "
	++integer_to_list(GpResetThreshold)++", GpLength: "++integer_to_list(GpLength)),
    lmaLinxHandler:send_update_grace_period_attributes_req(ItcPort, Spid, 
							   list_to_binary(GracePeriodId), 
							   GpActivationThreshold, 
							   GpResetThreshold, GpLength),
    {noreply, State};
handle_cast(notify_ready_for_service, #state{itc_port = ItcPort, spid = Spid} = State) ->
    handle_notify_ready_for_service(ItcPort, Spid),
    {noreply, State};
handle_cast(_Msg, State) ->
    log("lmaGlms, got Msg in handle_cast"),
    %% io:format("lmaGlms, got Msg: ~p in handle_cast", [Msg]),
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
handle_info({message, ItcPort, {Spid, _ToMboxId, SigNo, Data}},
	    #state{action_id = Id, itc_port = ItcPort, glms_restarted = {_, PortTimer}, 
		   pid = Pid, spid = Spid, test_mode = TestMode, queue = Queue} = State) ->

    case lmaLinxHandler:parse_itc({SigNo, Data}) of
	
	%% SOFTWARE PARAMETER
	{software_parameter_index_list_req, RequestId, Table} ->
	    IndexList = mnesia:dirty_all_keys(table(Table)),
	    log("Got software_parameter_index_list_req for table: "++Table++" answer OK with no "
		"of indexes: "++integer_to_list(length(IndexList))),
	    lmaLinxHandler:send_soft_param_index_list_rsp(ItcPort, Spid, RequestId, IndexList),
	    {noreply, State};
	{software_parameter_set_req, RequestId, Table, Index, Value} ->
	    log2("Got software_parameter_set_req for table: "++Table++" answer OK"),
	    ok = update_table(Table, Index, Value),
	    lmaLinxHandler:send_soft_param_set_rsp(ItcPort, Spid, RequestId, ?GLMS_OK),
	    {noreply, State};
	{software_parameter_get_req, RequestId, Table, BinTable} ->
	    log2("Got software_parameter_get_req for table: " ++ Table),
	    IndexList = mnesia:dirty_all_keys(table(Table)),
	    read_data_and_send_rsp(IndexList, table(Table), BinTable, ItcPort, Spid, RequestId, 
				   software),
	    {noreply, State};
	{software_parameter_delete_index_req, RequestId, Table, Index} ->
	    log("Got software_parameter_delete_index_req for table: " ++ Table ++ " and Index: " ++ 
		    integer_to_list(Index)),
	    ok = update_table(Table, Index),
	    lmaLinxHandler:send_software_parameter_delete_index_rsp(ItcPort, Spid, RequestId, 
								    ?GLMS_OK),
	    {noreply, State};
	    	 
	%% PERSISTENT PARAMETERS

	% SP086 LicenseSupportPersistentData storage
	{persistent_parameter_index_list_req, RequestId, "LicenseSupportPersistentData" = Table} ->
	    %% Save LicenseSupportPersistentData on disk and not in mnesia in order not to reuse with an backup
	    File = filename:join(lmaLib:get_lma_dir(), "licenseSupportPersistentData"),
	    IndexList = 
		case file:read_file(File) of
		    {ok, _Bin} ->
			[1];
		    _ ->
			[]
		end,
	    log("Got persistent_parameter_index_list_req for table: "++Table++ " answer OK with "
		"no of indexes: "++integer_to_list(length(IndexList))),	
	    lmaLinxHandler:send_persistant_param_index_list_rsp(ItcPort, Spid, RequestId, IndexList),
	    {noreply, State};

	{persistent_parameter_index_list_req, RequestId, "EUPersistentData" = Table} ->
	    %% Save EU on disk and not i mnesia in order not to reuse with an backup
	    File = filename:join(lmaLib:get_lma_dir(), "eUPersistentData"),
	    IndexList = 
		case file:read_file(File) of
		    {ok, _Bin} ->
			[1];
		    _ ->
			[]
		end,
	    log("Got persistent_parameter_index_list_req for table: "++Table++ " answer OK with "
		"no of indexes: "++integer_to_list(length(IndexList))),
	    lmaLinxHandler:send_persistant_param_index_list_rsp(ItcPort, Spid, RequestId, IndexList),
	    {noreply, State};
	{persistent_parameter_index_list_req, RequestId, Table} ->
	    IndexList = mnesia:dirty_all_keys(table(Table)),
	    log("Got persistent_parameter_index_list_req for table: "++Table++ " answer OK with "
		"no of indexes: "++integer_to_list(length(IndexList))),
	    lmaLinxHandler:send_persistant_param_index_list_rsp(ItcPort, Spid, RequestId, IndexList),
	    {noreply, State};
	{persistent_parameter_set_req, RequestId, Table, Index, Value} ->
	    log2("Got persistent_parameter_set_req for table: "++Table++" answer OK"),
	    ok = update_table(Table, Index, Value),	    
	    lmaLinxHandler:send_persistant_param_set_rsp(ItcPort, Spid, RequestId, ?GLMS_OK),
	    {noreply, State};

	 % SP086 LicenseSupportPersistentData storage
	{persistent_parameter_get_req, RequestId, "LicenseSupportPersistentData" = Table, BinTable} ->
	    %% Save LicenseSupportPersistentData on disk and not in mnesia in order not to reuse with an backup
	    log2("Got persistent_parameter_get_req for table: " ++ Table),
	    IndexList = [1],
	    read_data_and_send_rsp(IndexList, table(Table), BinTable, ItcPort, Spid, RequestId, 
				   persistent),
	    {noreply, State};

	{persistent_parameter_get_req, RequestId, "EUPersistentData" = Table, BinTable} ->
	    %% Save EU on disk and not i mnesia in order not to reuse with an backup
	    log2("Got persistent_parameter_get_req for table: " ++ Table),
	    IndexList = [1],
	    read_data_and_send_rsp(IndexList, table(Table), BinTable, ItcPort, Spid, RequestId, 
				   persistent),
	    {noreply, State};
	{persistent_parameter_get_req, RequestId, Table, BinTable} ->
	    log2("Got persistent_parameter_get_req for table: " ++ Table),
	    IndexList = mnesia:dirty_all_keys(table(Table)),
	    read_data_and_send_rsp(IndexList, table(Table), BinTable, ItcPort, Spid, RequestId, 
				   persistent),
	    {noreply, State};
	{persistent_parameter_delete_index_req, RequestId, Table, Index} ->
	    log("Got persistent_parameter_delete_index_req for table: " ++ Table ++ " and Index: " 
		++integer_to_list(Index)),
	    ok = update_table(Table, Index),
	    lmaLinxHandler:send_persistant_param_delete_index_rsp(ItcPort, Spid, RequestId, ?GLMS_OK),
	    {noreply, State};

	%SP086 GLH -> Adapter
   	{feature_configuration_list_req} ->
        	    log("Got feature_configuration_list_req"),
	    %READ APPDATA
	    KeyIdsList = lmaAppData:get_keyids(),
    	    KeyIdsListLen = length(KeyIdsList),
	    %AppData = <<"CXC4011018", "CXC4011929", "CXC4040099">>,
	    lmaLinxHandler:send_feature_configuration_list_rsp(ItcPort, Spid, ?GLMS_OK, KeyIdsListLen, KeyIdsList), 
	    %log("sent feature_configuration_list_rsp ~p~n",[KeyIdsList]),
	    {noreply, State};

    %SP086 GLH -> Adapter
	{install_area_license_keys_rsp, ResultCode} ->
    	    {{value, From}, NewQueue} = queue:out(Queue),
    	    %log("Got area_license_keys_rsp with ResultCode ~p~n",[ResultCode]),
	    gen_server:reply(From, {ok, ResultCode}),
	    {noreply, State#state{queue = NewQueue}};


  %SP086 GLH -> Adapter
	{update_area_id_rsp, GlmsResult, ResultInfo} ->
	    info_msg("---UPDATE_AREA_ID_RSP received from GLMS ~p~n", [ResultInfo]),
    	    {{value, From}, NewQueue} = queue:out(Queue),
	    case GlmsResult of
		?GLMS_NOK ->
		    log(warning, "Got area_license_keys_rsp with NOK"),
		    log("GLMS", warning, ResultInfo);
		?GLMS_OK ->
		    log("Got area_license_keys_rsp with OK")	    
	    end,
	    gen_server:reply(From, {ok, licenseareaid_updated}), 
	    {noreply, State#state{queue = NewQueue}};

	%SP086 GLH -> Adapter
	{state_mo_audit_rsp, GlmsResult, ResultInfo} ->
    	%%{{value, From}, NewQueue} = queue:out(Queue),
    	    info_msg("---AUDIT_RSP received from GLMS~n", []),
	    case GlmsResult of
		?GLMS_NOK ->
		    log(warning, "Got state_mo_audit_rsp with NOK"),
		    log("GLMS", warning, ResultInfo);
		?GLMS_OK ->
		    log("Got state_mo_audit_rsp with OK")	    
	    end,
	    {noreply, State};
	
	%% Activate RSP
	{activate_rsp, GlmsResult, _HighestSupportedPv, ResultInfo} ->
	    case GlmsResult of
		?GLMS_NOK ->
		    log(warning, "Got activate_rsp with NOK"),
		    log("GLMS", warning, ResultInfo);
		?GLMS_OK ->
		    log("Got activate_rsp with OK"),
		    log("Send subscribe_mo_updates_req"),
		    lmaLinxHandler:send_subscribe_mo_updates_req(ItcPort, Spid)
	    end,
	    {noreply, State};

	{activate_eu_rsp, GlmsResult, ResultInfo} ->
	    {{value, From}, NewQueue} = queue:out(Queue),
	    case GlmsResult of
		?GLMS_NOK ->
		    gen_server:reply(From, {error, eu_not_activated}),
		    log(warning, "Got activate_eu_rsp with NOK"),
		    log("GLMS", warning, ResultInfo);
		?GLMS_OK ->
		    gen_server:reply(From, {ok, eu_activated}),
		    log("Got activate_eu_rsp with OK")
	    end,
	    {noreply, State#state{queue = NewQueue}};
	{activate_iu_rsp, GlmsResult, ResultInfo} ->
	    {{value, From}, NewQueue} = queue:out(Queue),
	    case GlmsResult of
		?GLMS_NOK ->
		    gen_server:reply(From, {error, iu_not_activated}),
		    log(warning, "Got activate_iu_rsp with NOK"),
		    log("GLMS", warning, ResultInfo);
		?GLMS_OK ->
		    gen_server:reply(From, {ok, iu_activated}),
		    log("Got activate_iu_rsp with OK")
	    end,
	    {noreply, State#state{queue = NewQueue}};
	{activate_pu_rsp, GlmsResult, Expiration, ActivationsLeft, ActivationState, PuDeactivated, 
	 ResultInfo} ->
	    Expiration_ECIM = lmaLib:change_sec_to_ecim_date(Expiration),
	    {{value, From}, NewQueue} = queue:out(Queue),
	    case GlmsResult of
		?GLMS_NOK ->
		    log(warning, "Got activate_pu_rsp with NOK"),
		    log("GLMS", warning, ResultInfo),
		    gen_server:reply(From, {error, pu_not_activated, 
					    {Expiration_ECIM, ActivationsLeft,
					     ActivationState, PuDeactivated}});
		?GLMS_OK ->
		    log("Got activate_pu_rsp with OK"),
		    gen_server:reply(From, {ok, pu_activated, 
					    {Expiration_ECIM, ActivationsLeft, 
					     ActivationState, PuDeactivated}})
	    end,
	    {noreply, State#state{queue = NewQueue}};

	%% Deactivate RSP
	{deactivate_rsp, GlmsResult, ResultInfo} ->
	    case GlmsResult of
		?GLMS_NOK ->
		    log(warning, "Got deactivate_rsp with NOK"),
		    log("GLMS", warning, ResultInfo);
		?GLMS_OK ->
		    log("Got deactivate_rsp with OK")
	    end,
	    itc_close(ItcPort),
	    kill_glms(Pid),
	    NewState = restart_glms(State, PortTimer),
	    {noreply, NewState};
	{deactivate_pu_rsp, GlmsResult, Expiration, ActivationsLeft, ActivationState, PuDeactivated,
	 ResultInfo} ->
	    Expiration_ECIM = lmaLib:change_sec_to_ecim_date(Expiration),
	    {{value, From}, NewQueue} = queue:out(Queue),
	    case GlmsResult of
		?GLMS_NOK ->
		    log(warning, "Got deactivate_pu_rsp with NOK"),
		    log("GLMS", warning, ResultInfo),
		    gen_server:reply(From, {error, pu_not_deactivated, 
					    {Expiration_ECIM, ActivationsLeft,
					     ActivationState, PuDeactivated}});
		?GLMS_OK ->
		    log("Got deactivate_pu_rsp with OK"),
		    gen_server:reply(From, {ok, pu_deactivated, 
					    {Expiration_ECIM, ActivationsLeft,
					     ActivationState, PuDeactivated}})
	    end,
	    {noreply, State#state{queue = NewQueue}};
	
	%% MO Signals
	{subscribe_mo_updates_rsp, GlmsResult, ResultInfo} ->
	    case GlmsResult of
		?GLMS_NOK ->
		    log(warning, "Got subscribe_mo_updates_rsp with NOK"),
		    log("GLMS", warning, ResultInfo);
		?GLMS_OK ->
		    log("Got subscribe_mo_updates_rsp with OK"),
		    log("Sending read_mo_*_req"), 
		    lmaLinxHandler:send_read_mo_lm_req(ItcPort, Spid),
		    lmaLinxHandler:send_read_mo_install_keyfile_report_progress_req(ItcPort, Spid),
		    lmaLinxHandler:send_read_mo_key_file_information_req(ItcPort, Spid),
		    lmaLinxHandler:send_read_licensesupport_mo_req(ItcPort, Spid),
		    lmaLinxHandler:send_read_eu_mo_req(ItcPort, Spid),
		    lmaLinxHandler:send_read_iu_mo_req(ItcPort, Spid),
		    lmaLinxHandler:send_read_am_mo_req(ItcPort, Spid),
		    lmaLinxHandler:send_get_feature_key_mo_list_req(ItcPort, Spid),
		    lmaLinxHandler:send_get_feature_state_mo_list_req(ItcPort, Spid),
		    lmaLinxHandler:send_get_capacity_key_mo_list_req(ItcPort, Spid),
		    lmaLinxHandler:send_get_capacity_state_mo_list_req(ItcPort, Spid)
	    end,
	    {noreply, State};

	%% FeatureKey MO
	{create_feature_key_mo_req
	, ValidFrom, Expiration, Shared, FeatureKeyId, KeyId, Name, 
	 ProductType} ->
	    log2("Got create_feature_key_mo_req for "++KeyId++" with id: "++FeatureKeyId
		 ++ " answer OK"),
	    lmaLib:create_feature_key(FeatureKeyId, Name, ValidFrom, Expiration, KeyId, 
				      ProductType, Shared),
	    lmaLinxHandler:send_create_mo_rsp(ItcPort, Spid, ?GLMS_OK, 
					      list_to_binary(FeatureKeyId), featureKey),
	    {noreply, State};
	{delete_feature_key_mo_req, FeatureKeyId} ->
	    log("Got delete_feature_key_mo_req for "++ FeatureKeyId ++ " answer OK"),
	    lmaLib:delete_mo(FeatureKeyId, featureKey),
	    lmaLinxHandler:send_delete_mo_rsp(ItcPort, Spid, ?GLMS_OK, 
					      list_to_binary(FeatureKeyId), featureKey),
	    {noreply, State};
	{mo_update_feature_key_name_ind, FeatureKeyId, FeatureName} ->
	    log("Got mo_update_featurekey_name_ind for "++FeatureKeyId++" with name: "
		++FeatureName),
	    lmaLib:update_name(FeatureKeyId, FeatureName, featureKey),
	    {noreply, State};
	{read_mo_feature_key_rsp, GlmsResult, ValidFrom, Expiration, FeatureKeyId, KeyId, 
	 Name, ProductType, Shared, ResultInfo} ->
	    case GlmsResult of
		?GLMS_OK ->
		    log2("Got read_mo_feature_key_rsp with OK"),
		    lmaLib:update_feature_key(FeatureKeyId, Name, ValidFrom, Expiration, KeyId,
					      ProductType, Shared);
		?GLMS_NOK ->
		    log(warning, "Got read_mo_feature_key_rsp for "++FeatureKeyId++" with NOK"),
		    log("GLMS", warning, ResultInfo)
	    end,
	    {noreply, State};
	{get_feature_key_mo_list_rsp, GlmsResult, NrOfFeatureKeyIds, FeatureKeyId} ->
	    case GlmsResult of
		?GLMS_OK ->
		    log("Got get_feature_key_mo_list_rsp with OK noOfFeatureKeys: " 
			++ integer_to_list(NrOfFeatureKeyIds)),
		    split_list(NrOfFeatureKeyIds, binary_to_list(FeatureKeyId), ItcPort, Spid,
			       featureKey);
		?GLMS_NOK ->
		    log(warning, "Got get_feature_key_mo_list_rsp with NOK")
	    end,
	    {noreply, State};

	%% FeatureState MO
	{create_feature_state_mo_req, FeatureState, LicenseState, ServiceState, FeatureStateId, 
	 Description, KeyId, ActiveFeatureKeyId} ->
	    log2("Got create_feature_state_mo_req for "++KeyId++" with id: "++FeatureStateId
		 ++ " answer OK"),
	    lmaLib:create_feature_state(FeatureStateId, FeatureState, LicenseState, ServiceState,
					Description, KeyId, ActiveFeatureKeyId),
	    lmaLinxHandler:send_create_mo_rsp(ItcPort, Spid, ?GLMS_OK, 
					      list_to_binary(FeatureStateId), featureState),
	    {noreply, State};
	{delete_feature_state_mo_req, FeatureStateId} ->
	    log("Got delete_feature_state_mo_req for "++ FeatureStateId ++ " answer OK"),
	    lmaLib:delete_mo(FeatureStateId, featureState),
	    lmaLinxHandler:send_delete_mo_rsp(ItcPort, Spid, ?GLMS_OK, 
					      list_to_binary(FeatureStateId), featureState),
	    {noreply, State};
	{mo_update_feature_state_ind, FeatureState, LicenseState, ServiceState, FeatureStateId, 
	 Description, KeyId, ActiveFeatureKeyId} ->
	    log2("Got mo_update_feature_state_ind for "++FeatureStateId),
	    lmaLib:update_feature_state(FeatureStateId, FeatureState, LicenseState, ServiceState, 
					Description, KeyId, ActiveFeatureKeyId),
	    {noreply, State};
	{get_feature_state_mo_list_rsp, GlmsResult, NrOfFeatureStateIds, FeatureStateId} ->
	    case GlmsResult of
		?GLMS_OK ->
		    log("Got get_feature_state_mo_list_rsp with OK noOfFeatureStates: "
			++ integer_to_list(NrOfFeatureStateIds)),
		    split_list(NrOfFeatureStateIds, binary_to_list(FeatureStateId), ItcPort, 
			       Spid, featureState);
		?GLMS_NOK ->
		    log(warning, "Got get_feature_state_mo_list_rsp with NOK")
	    end,
	    {noreply, State};
	{read_mo_feature_state_rsp, GlmsResult, FeatureState, LicenseState, ServiceState, 
	 FeatureStateId, Description, KeyId, ResultInfo, ActiveFeatureKeyId} ->
	    case GlmsResult of
		?GLMS_OK ->
		    log2("Got read_mo_feature_state_rsp with OK"),
		    lmaLib:update_feature_state(FeatureStateId, FeatureState, LicenseState, 
						ServiceState, Description, KeyId, ActiveFeatureKeyId);
		?GLMS_NOK ->
		    log(warning, "Got read_mo_feature_state_rsp for "++FeatureStateId++" with NOK"),
		    log("GLMS", warning, ResultInfo)
	    end,
	    {noreply, State};

	%% CapacityState MO
	{create_capacity_state_mo_req, NoLimit, Value, LicensedCapacityLimitReached, 
	 GrantedCapacityLevel, LicenseState, CapacityUnit, CapacityStateId,
	 Description, KeyId, ActiveCapacityKeyId} ->
	    log2("Got create_capacity_state_mo_req for "++KeyId++" with id: "++CapacityStateId 
		++ " answer OK"),
	    lmaLib:create_capacity_state(NoLimit, Value, LicensedCapacityLimitReached, 
					 GrantedCapacityLevel, LicenseState, CapacityUnit, 
					 CapacityStateId, Description, KeyId, ActiveCapacityKeyId),
	    lmaLinxHandler:send_create_mo_rsp(ItcPort, Spid, ?GLMS_OK, 
					      list_to_binary(CapacityStateId), capacityState),
	    {noreply, State};
	{mo_update_capacity_state_ind, NoLimit, Value, LicensedCapacityLimitReached, LicenseState, 
	 GrantedCapacityLevel, CapacityStateId, Description, KeyId, ActiveCapacityKeyId} ->
	    log2("Got mo_update_capacity_state_ind for "++CapacityStateId),
	    lmaLib:update_capacity_state(NoLimit, Value, LicensedCapacityLimitReached, 
					 LicenseState, GrantedCapacityLevel, CapacityStateId, 
					 Description, KeyId, ActiveCapacityKeyId),
	    {noreply, State};
	{mo_update_capacity_state_capacity_unit_ind, CapacityUnit, CapacityStateId} ->
	    log("Got mo_update_capacity_state_capacity_unit_ind for "++CapacityStateId),
	    lmaLib:update_capacity_unit(CapacityStateId, CapacityUnit, capacityState),
	    {noreply, State};
	{get_capacity_state_mo_list_rsp, GlmsResult, NrOfCapacityStateIds, CapacityStateId} ->
	    case GlmsResult of
		?GLMS_OK ->
		    log("Got get_capacity_state_mo_list_rsp with OK noOfCapacityStates: "
			++ integer_to_list(NrOfCapacityStateIds)),
		    split_list(NrOfCapacityStateIds, binary_to_list(CapacityStateId), 
			       ItcPort, Spid, capacityState);
		?GLMS_NOK ->
		    log(warning, "Got get_capacity_state_mo_list_rsp with NOK")
	    end,
	    {noreply, State};
	{read_mo_capacity_state_rsp, NoLimit, Value, LicensedCapacityLimitReached, Result, 
	 LicenseState, GrantedCapacityLevel, CapacityUnit, CapacityStateId, 
	 Description, KeyId, ResultInfo, ActiveCapacityKeyId} ->
	    case Result of
		?GLMS_OK ->
		    log2("Got read_mo_capacity_state_rsp with OK"),
		    lmaLib:update_capacity_state(NoLimit, Value, LicensedCapacityLimitReached, 
						 LicenseState, GrantedCapacityLevel, CapacityStateId, 
						 Description, KeyId, ActiveCapacityKeyId),
		    lmaLib:update_capacity_unit(CapacityStateId, CapacityUnit, capacityState);
		?GLMS_NOK ->
		    log(warning, "Got read_mo_capacity_state_rsp for "++CapacityStateId++
			    " with NOK"),
		    log("GLMS", warning, ResultInfo)
	    end,
	    {noreply, State};
	{delete_capacity_state_mo_req, CapacityStateId} ->
	    log("Got delete_capacity_state_mo_req for "++ CapacityStateId ++ " answer OK"),
	    lmaLib:delete_mo(CapacityStateId, capacityState),
	    lmaLinxHandler:send_delete_mo_rsp(ItcPort, Spid, ?GLMS_OK, 
					      list_to_binary(CapacityStateId), capacityState),
	    {noreply, State};

	%% CapacityKey MO
	{create_capacity_key_mo_req, NoLimit, Value, ValidFrom, Expiration, 
	 LicensedCapacityLimitReached, GrantedCapacityLevel, CapacityUnit, CapacityKeyId, KeyId, 
	 Name, ProductType} ->
	    log2("Got create_capacity_key_mo_req for "++KeyId++" with id: "++CapacityKeyId 
		 ++ " answer OK"),
	    lmaLib:create_capacity_key(NoLimit, Value, ValidFrom, Expiration, 
				       LicensedCapacityLimitReached, GrantedCapacityLevel, 
				       CapacityUnit, CapacityKeyId, KeyId, Name, ProductType),
	    lmaLinxHandler:send_create_mo_rsp(ItcPort, Spid, ?GLMS_OK,
					      list_to_binary(CapacityKeyId), capacityKey),
	    {noreply, State};
	{get_capacity_key_mo_list_rsp, GlmsResult, NrOfCapacityKeyIds, CapacityKeyId} ->
	    case GlmsResult of
		?GLMS_OK ->
		    log("Got get_capacity_key_mo_list_rsp with OK noOfCapacityKeys: " 
			++ integer_to_list(NrOfCapacityKeyIds)),
		    split_list(NrOfCapacityKeyIds, binary_to_list(CapacityKeyId), ItcPort, 
			       Spid, capacityKey);
		_ ->
		    log(warning, "Got get_capacity_key_mo_list_rsp with NOK")
	    end,
	    {noreply, State};
	{read_mo_capacity_key_rsp, NoLimit, Value, ValidFrom, Expiration, 
	 LicensedCapacityLimitReached, Result, GrantedCapacityLevel, CapacityUnit, ResultInfo, 
	 CapacityKeyId, KeyId, Name, ProductType} ->
	    case Result of
		?GLMS_OK ->
		    log2("Got read_mo_capacity_key_rsp with OK"),
		    lmaLib:update_capacity_key(NoLimit, Value, ValidFrom, Expiration,
					       LicensedCapacityLimitReached, GrantedCapacityLevel, 
					       CapacityUnit, CapacityKeyId, KeyId, Name, 
					       ProductType);
		_ ->
		    log(warning, "Got read_mo_capacity_key_rsp for "++CapacityKeyId++" with NOK"),
		    log("GLMS", warning, ResultInfo)
	    end,
	    {noreply, State};
	{mo_update_capacity_key_name_ind, CapacityKeyId, Name} ->
	    log("Got mo_update_capacity_key_name_ind for "++CapacityKeyId++" with name: "++Name),
	    lmaLib:update_name(CapacityKeyId, Name, capacityKey),
	    {noreply, State};
	{mo_update_capacity_key_capacity_unit_ind, CapacityUnit, CapacityKeyId} ->
	    log("Got mo_update_capacity_key_capacity_unit_ind for "++CapacityKeyId++" with unit: "
		++CapacityUnit),
	    lmaLib:update_capacity_unit(CapacityKeyId, CapacityUnit, capacityKey),
	    {noreply, State};
	{delete_capacity_key_mo_req, CapacityKeyId} ->
	    log("Got delete_capacity_key_mo_req for "++ CapacityKeyId ++ " answer OK"),
	    lmaLib:delete_mo(CapacityKeyId, capacityKey),
	    lmaLinxHandler:send_delete_mo_rsp(ItcPort, Spid, ?GLMS_OK, 
					      list_to_binary(CapacityKeyId), capacityKey),
	    {noreply, State};
	
	%% GracePeriod MO
	{create_grace_period_mo_req, Expiration, ConfiguredLength, ConfiguredResetThreshold, 
	 ConfiguredActivationThreshold, GracePeriodState, GracePeriodId} ->
	    log2("Got create_grace_period_mo_req for "++GracePeriodId ++ " answer OK"),
	    lmaLib:create_grace_period(Expiration, ConfiguredLength, ConfiguredResetThreshold, 
				       ConfiguredActivationThreshold, GracePeriodState, 
				       GracePeriodId),
	    lmaLinxHandler:send_create_mo_rsp(ItcPort, Spid, ?GLMS_OK,
					      list_to_binary(GracePeriodId), gracePeriod),
	    {noreply, State};
	{mo_update_grace_period_ind, Expiration, ConfiguredLength, ConfiguredResetThreshold, 
	 ConfiguredActivationThreshold, GracePeriodState, GracePeriodId} ->
	    log2("Got mo_update_grace_period_ind for "++GracePeriodId),
	    lmaLib:update_grace_period(Expiration, ConfiguredLength, ConfiguredResetThreshold, 
				       ConfiguredActivationThreshold, GracePeriodState, 
				       GracePeriodId),
	    {noreply, State};
	{read_mo_grace_period_rsp, Expiration, ConfiguredLength, ConfiguredResetThreshold, 
	 ConfiguredActivationThreshold, GracePeriodState, GracePeriodId, _Result, 
	 _ResultInfo} ->
	    case GracePeriodId of
		[] ->
		    do_nada;
		_Else ->
		    %% case Result of
		    %% 	?GLMS_OK ->
		    log("Got read_mo_grace_period_rsp for "++GracePeriodId),
		    lmaLib:update_grace_period(Expiration, ConfiguredLength, ConfiguredResetThreshold, 
					       ConfiguredActivationThreshold, GracePeriodState, 
					       GracePeriodId)
		    %% _ ->
		    %% log(warning, "Got read_mo_grace_period_rsp with NOK for "++GracePeriodId),
		    %% log("GLMS", warning, ResultInfo)
		    %% end
	    end,
	    {noreply, State};
	
	{delete_grace_period_mo_req, GracePeriodId} ->
	    log("Got delete_grace_period_mo_req for "++ GracePeriodId ++ " answer OK"),
	    lmaLib:delete_mo(GracePeriodId, gracePeriod),
	    lmaLinxHandler:send_delete_mo_rsp(ItcPort, Spid, ?GLMS_OK, 
					      list_to_binary(GracePeriodId), gracePeriod),
	    {noreply, State};
	{update_grace_period_attributes_rsp, Result, CapacityStateId, ResultInfo} ->
	    case Result of
		?GLMS_OK ->
		    log2("Got update_grace_period_attributes_rsp for "++CapacityStateId
			 ++" with OK");
		?GLMS_NOK ->
		    log("Got update_grace_period_attributes_rsp for "++CapacityStateId
			++" with NOK"),
		    log("GLMS", error, ResultInfo)
	    end,
	    {noreply, State};
			    
	%% LM MO
	{mo_update_lm_ind, Fingerprint, FingerprintUpdateable, LmState, LastInventoryChange, 
	 LastLicenseInventoryRefresh} ->
	    log("Got mo_update_lm_ind"),
	    lmaLib:update_lm(Fingerprint, FingerprintUpdateable, LmState, LastInventoryChange, 
			     LastLicenseInventoryRefresh),
	    {noreply, State};
	{read_mo_lm_rsp, GlmsResult, Fingerprint, FingerprintUpdateable, LmState, 
	 LastInventoryChange, LastLicenseInventoryRefresh, ResultInfo} ->
	    case GlmsResult of
		?GLMS_OK ->
		    log2("Got read_mo_lm_rsp with OK"),
		    lmaLib:update_lm(Fingerprint, FingerprintUpdateable, LmState, 
				     LastInventoryChange, LastLicenseInventoryRefresh);
		?GLMS_NOK ->
		    log(warning, "Got read_mo_lm_rsp with NOK"),
		    log("GLMS", warning, ResultInfo)
	    end,
	    {noreply, State};
	 
	%% EU MOs
	{mo_update_eu_ind, ActivationState, Expiration, ActivationsLeft} ->
	    log("Got mo_update_eu_ind, ActivationState: "++integer_to_list(ActivationState)
		++", Expiration: "++integer_to_list(Expiration)++", ActivationsLeft: " 
		++integer_to_list(ActivationsLeft)), 
	    lmaLib:update_eu(ActivationState, Expiration, ActivationsLeft),
	    {noreply, State};
	{read_eu_mo_rsp, GlmsResult, Expiration, ActivationsLeft, ActivationState, ResultInfo} ->
	    case GlmsResult of
		?GLMS_OK ->
		    log("Got read_eu_mo_rsp with OK"),
		    lmaLib:update_eu(ActivationState, Expiration, ActivationsLeft);
		?GLMS_NOK ->
		    log(warning, "Got read_eu_mo_rsp with NOK"),
		    log("GLMS", warning, ResultInfo)
	    end,
	    {noreply, State};


	%% LicenseSupport MOs. SP086
	{mo_update_licensesupport_ind, AreaId} ->
	    log("Got mo_update_licensesupport_ind, AreaId: "++AreaId),
	    lmaLib:update_licensesupport(AreaId),
	    {noreply, State};

	{read_licensesupport_mo_rsp, GlmsResult, ResultInfo, AreaId} ->
	    case GlmsResult of
		?GLMS_OK ->
		    log("Got read_licensesupport_mo_rsp with OK"),
		    lmaLib:update_licensesupport(AreaId);
		?GLMS_NOK ->
		    log(warning, "Got read_licensesupport_mo_rsp with NOK"),
		    log("GLMS", warning, ResultInfo)
	    end,
	    {noreply, State};

	
	%% IU MOs
	{mo_update_iu_ind, ActivationState, Expiration, ActivationsLeft} ->
	    log("Got mo_update_iu_ind"),
	    lmaLib:update_iu(ActivationState, Expiration, ActivationsLeft),
	    {noreply, State};
	{read_iu_mo_rsp, GlmsResult, Expiration, ActivationsLeft, ActivationState, ResultInfo} ->
	    case GlmsResult of
		?GLMS_OK ->
		    log2("Got read_iu_mo_rsp with OK"),
		    lmaLib:update_iu(ActivationState, Expiration, ActivationsLeft);
		?GLMS_NOK ->
		    log(warning, "Got read_iu_mo_rsp with NOK"),
		    log("GLMS", warning, ResultInfo)
	    end,
	    {noreply, State};

	%% AM
	{mo_update_am_ind, ActivationState, Expiration} ->
	    log("Got mo_update_am_ind"),
	    lmaLib:update_am(ActivationState, Expiration),
	    {noreply, State};
	 {read_am_mo_rsp, GlmsResult, Expiration, ActivationState, ResultInfo} ->
	    case GlmsResult of
		?GLMS_OK ->
		    log2("Got read_am_mo_rsp with OK"),
		    lmaLib:update_am(ActivationState, Expiration);
		?GLMS_NOK ->
		    log(warning, "Got read_am_mo_rsp with NOK"),
		    log("GLMS", warning, ResultInfo)
	    end,
	    {noreply, State};
	

	%% KEY FILE MO
	{mo_update_keyfile_ind, SequenceNumber, InstallationTime, Locatable, ActionId, ActionName,
	 AdditionalInfo, ProgressInfo, ProgressPercentage, Result, ResultInfo, Glms_State, 
	 TimeActionCompleted, TimeActionStarted, TimeOfLastStatusUpdate, ProductType} ->

	    log2("Got mo_update_keyfile_ind"),
	    lmaLib:update_keyfile_information(SequenceNumber, InstallationTime, Locatable, 
					      ProductType),
	    case Id of
		ActionId ->
	    	    lmaLib:update_progress_struct([{actionName, ActionName},
						   {additionalInfo, AdditionalInfo},
						   {progressInfo, ProgressInfo},
						   {progressPercentage, ProgressPercentage},
						   {result, Result},
						   {resultInfo, ResultInfo},
						   {state, Glms_State},
						   {actionId, ActionId},
						   {timeActionStarted, TimeActionStarted},
						   {timeActionCompleted, TimeActionCompleted},
						   {timeOfLastStatusUpdate, 
						    TimeOfLastStatusUpdate}]);
		_NewActionId ->
		    lmaLib:update_progress_struct([{actionName, ActionName},
						   {additionalInfoClear, AdditionalInfo}, 
						   {progressInfo, ProgressInfo},
						   {progressPercentage, ProgressPercentage},
						   {result, Result},
						   {resultInfo, ResultInfo},
						   {state, Glms_State},
						   {actionId, ActionId},
						   {timeActionStarted, TimeActionStarted},
						   {timeActionCompleted, TimeActionCompleted},
						   {timeOfLastStatusUpdate, 
						    TimeOfLastStatusUpdate}])
	    end,
	    {noreply, State#state{action_id = ActionId}};
	{read_mo_install_keyfile_report_progress_rsp, GlmsResult, ActionId, ActionName, 
	 AdditionalInfo, ProgressInfo, ProgressPercentage, Result, ResultInfo, Glms_State, 
	 TimeActionCompleted, TimeActionStarted, _TimeOfLastStatusUpdate, ResultInfo2} ->

	    case GlmsResult of
		?GLMS_OK ->
		    log("Got read_mo_install_keyfile_report_progress_rsp with OK"),
		    lmaLib:update_progress_struct([{actionName, ActionName},
						   {additionalInfoClear, AdditionalInfo},
						   {progressInfo, ProgressInfo},
						   {progressPercentage, ProgressPercentage},
						   {result, Result},
						   {resultInfo, ResultInfo},
						   {state, Glms_State},
						   {actionId, ActionId},
						   {timeActionStarted, TimeActionStarted},
						   {timeActionCompleted, TimeActionCompleted}]);
		?GLMS_NOK ->
		    log(warning, "Got read_mo_install_keyfile_report_progress_rsp with NOK"),
		    log("GLMS", warning, ResultInfo2)
	    end,
	    {noreply, State};
	{read_mo_keyfile_information_rsp, GlmsResult, SequenceNumber, InstallationTime, Locatable,
	 ProductType, ResultInfo} ->
	    case GlmsResult of
		?GLMS_OK ->
		    log("Got read_mo_keyfile_information_rsp with OK"),
		    lmaLib:update_keyfile_information(SequenceNumber, InstallationTime, Locatable,
						      ProductType);
		?GLMS_NOK ->
		    log(warning, "Got read_mo_keyfile_information_rsp with NOK"),
		    log("GLMS", warning, ResultInfo)
	    end,
	    {noreply, State};
	{get_keyfile_location_req} ->
	    {KFLocation, GlmsResult, Encryption} = 
		case mnesia:dirty_read(lma_KFLocation, 1) of
		    [Obj] ->
			case Obj#lma_KFLocation.sequenceNo of
			    undefined ->
				log(warning, "Got get_keyfile_location_req answer will be empty "
				    "since no file is stored"),
				{<<"">>, ?GLMS_OK, true};
			    _No ->
				log("Got get_keyfile_location_req and a LKF is stored in mnesia"),
				find_latest_installed_lkf(Obj)
			end;
		    _Else ->
			%% No LKF stored in mnesia, check the disk also
			case find_sequence_no_on_node() of
			    {none, 0} ->
				log(warning, "Got get_keyfile_location_req answer will be empty "
				    "since no file is stored"),
				{<<"">>, ?GLMS_OK, true};
			    {_FileDataFromFile, SequenceNoFile} ->
				%% A LKF is found on the node but not stored in mnesia i.e.
				%% a backup is restored where there was no LKF installed.
				%% The LKF on the node can not be used since no fingerprint is 
				%% stored in mnesia, the installation will be rejected.
				log(warning, "Got get_keyfile_location_req and a LKF with sequence"
				    " no: "++ integer_to_list(SequenceNoFile) ++" is stored on disk"
				    " but non in mnesia. The LKF can not be used, answer will be "
				    "empty"),
				LKFPath = lmaLib:get_LKF_path(license),
				log("No LKF stored in mensia, a LKF stored at the node, remove "
				    "this one"),
				file:delete(LKFPath),
				{<<"">>, ?GLMS_OK, true}
			end
		end,
	    lmaLinxHandler:send_get_key_file_loc_rsp(ItcPort, Spid, GlmsResult, KFLocation),
	    {noreply, State#state{encryption = Encryption}};
	{install_keyfile_rsp, GlmsResult, GlmsActionId} ->
	    NQueue = 
		case queue:out(Queue) of
		    {{value, From}, NewQueue} ->
			case  GlmsActionId of 
			    0 ->
				gen_server:reply(From, {ok, 0}),
				%% Another installation is already ongoing, this one will be aborted
				log(warning, "Got install_keyfile_rsp with ActionId=0. This i"
				    "nstallation will be aborted due to already ongoing action");
			    _Else ->
				case GlmsResult of
				    ?GLMS_NOK ->
					log(warning, "Got install_keyfile_rsp with NOK");
				    ?GLMS_OK ->
					log("Got install_keyfile_rsp with OK")
				end,
				gen_server:reply(From, {ok, GlmsActionId})
			end,
			NewQueue;
		    _NoValueInQue ->
			%% AI in ongoing, no one is waiting for the reply
			case GlmsResult of
			    ?GLMS_NOK ->

				log(warning, "Got install_keyfile_rsp with NOK during AI, no one "
				    ++ "is waiting for a result");
			    ?GLMS_OK ->
				log(warning, "Got install_keyfile_rsp with OK during AI, no one "
				    ++ "is waiting for a result")
			end,
			Queue
		end,
	    {noreply, State#state{queue = NQueue}};		    
	{install_keyfile_ind, GlmsResult, AdditionalInfo} ->
	    case GlmsResult of
		?GLMS_NOK ->
		    log(warning, "Got install_keyfile_ind with NOK"),
		    log("GLMS", warning, AdditionalInfo);
		?GLMS_OK ->
		    log2("Got install_keyfile_ind with OK"),
		    log("GLMS", info, AdditionalInfo)
	    end,
	    {noreply, State};
	{store_keyfile_req, KFLocation} ->
	    case file:read_file(KFLocation) of
		{ok, FileData} ->
		    log("Got store_keyfile_req for "++KFLocation ++ " answer OK"),
		    Encryption = search_for_CXC_in_LKF("CXC4012032", FileData),
		    search_for_CXC_in_LKF("CXC4011959", FileData),
		    Path = lmaLib:get_LKF_path(license),
		    {ok, _Bytes} = file:copy(KFLocation, Path),
		    lmaLinxHandler:send_store_key_file_rsp(ItcPort, Spid, ?GLMS_OK, 
							   list_to_binary(Path)),
		    {noreply, State#state{encryption = Encryption}};
		_Else ->
		    log("Got store_keyfile_req for "++KFLocation ++ 
			    " answer NOK, not able to read file"),
		    lmaLinxHandler:send_store_key_file_rsp(ItcPort, Spid, ?GLMS_NOK, <<>>),
		    {noreply, State}
	    end;
	    {download_keyfile_req, KfUri, Password} ->
	    log("Got download_keyfile_req"),
	    {Result, TmpLKFPath} =
		case ftpI:parse_uri(to_string(KfUri)) of
		    {ok, {Proto, User, Host, Port, File, _}} ->
			start_channel(Proto, Host, Port, User, Password, File);
		    {error, UriReason} ->
			lmaLib:error_msg("lmaGlms failed to parse the URI, reason: ~p~n",
					 [UriReason]),
			log(error,"Sending download_key_file_rsp with NOK, failed to parse the URI"),
			{?GLMS_NOK,lmaLib:get_LKF_path(tmp)}
		end,
	    lmaLinxHandler:send_download_key_file_rsp(ItcPort, Spid, Result, list_to_binary(TmpLKFPath)),
	    {noreply, State};

	{refresh_license_inventory_rsp, GlmsResult, ResultInfo} ->
	    {{value, From}, NewQueue} = queue:out(Queue),
	    case GlmsResult of
		?GLMS_NOK ->
		    log(warning, "Got refresh_license_inventory_rsp with NOK"),
		    log("GLMS", warning, ResultInfo),
		    gen_server:reply(From, {error, no_refresh});
		?GLMS_OK ->
		    log("Got refresh_license_inventory_rsp with OK"),
		    gen_server:reply(From, {ok, refresh})
	    end,
	    {noreply, State#state{queue = NewQueue}};
	
	{pki_verification_req, KfLocation} ->
	    case TestMode of
		off->
		    GlmsResult =
			case lmaLkf:verify_lkf_file(KfLocation) of
			    valid  ->
				log("PKI verification done, send OK to GLMS"),
				?GLMS_OK;
			    {failed, Reason} ->
				log(error, "PKI verification failed with reason: "++ 
					atom_to_list(Reason) ++ " send NOK to GLMS"),
				?GLMS_NOK;
			    {error, file_not_found} ->
				log(error, "PKI verification failed, not possible to read the file: " 
				    ++ KfLocation),
				%% Temporary fix until GLMS will reject a install of a local LKF
				%% if the file does not exist.
				?GLMS_OK;
			    {error, ReasonAtom} when is_atom(ReasonAtom) ->
				log(error, "PKI verification failed with reason: "
				    ++atom_to_list(ReasonAtom) ++ " for file: " ++ KfLocation),
				?GLMS_NOK;
			    {error, Reason} when is_list(Reason) ->
				log(error, "PKI verification failed with reason: "++Reason ++ 
					" for file: " ++ KfLocation ++ " send NOK to GLMS"),
				?GLMS_NOK;
			    {error, {bad_cert,cert_expired}} ->
				log(error, "PKI verification failed since the certificate "
				    "has expired, send NOK to GLMS"),
				?GLMS_NOK;
			    {error, _Reason} ->
				log(error, "PKI verification failed send NOK to GLMS"),
				?GLMS_NOK
			end;
		on ->
		    log(warning, "Test mode is turned on, no PKI verification is done"),
		    GlmsResult = ?GLMS_OK
	    end,
	    lmaLinxHandler:send_pki_verification_rsp(ItcPort, Spid, GlmsResult, KfLocation),
	    {noreply, State};

	{set_fingerprint_rsp, GlmsResult, ResultInfo} ->
	    case GlmsResult of
		?GLMS_NOK ->
		    log(warning, "Got set_fingerprint_rsp with NOK"),
		    log("GLMS", warning, ResultInfo);
		?GLMS_OK ->
		    log2("Got set_fingerprint_rsp with OK"),
		    LKFPath = lmaLib:get_LKF_path(license),
		    case file:read_file(LKFPath) of
			{ok, _FileData} ->
			    %% A LKF is stored at the node, no AI is ongoing
			    do_nada;
			_Else ->
			    %% Check if a LKF is included in AI, if yes install that one
			    case aicI:get_lkf_fn() of
				{ok, Uri} ->
				    log("AI is ongoing, installing file: "++Uri),
				    lmaLinxHandler:send_install_keyfile_req(ItcPort, Spid, 
									    list_to_binary(Uri), 
									    list_to_binary(""));
				_Error ->
				    do_nada
			    end
		    end
	    end,
	    {noreply, State};

	{set_feature_state_rsp, GlmsResult, ResultInfo, FeatureStateId} ->
	    case GlmsResult of
		?GLMS_NOK ->
		    log(warning, "Got set_feature_state_rsp with NOK for "++ FeatureStateId),
		    log("GLMS", warning, ResultInfo);
		?GLMS_OK ->
		    log("Got set_feature_state_rsp with OK for " ++ FeatureStateId)
	    end,
	    {noreply, State};

	%% Alarm
	{keyfile_fault_alarm_req, GlmsAlarmState} ->
	    Dn = [list_to_binary("ManagedElement=1"),
		  list_to_binary("SystemFunctions=1"),
		  list_to_binary("Lm=1")],
	    AlarmEventId =
		case GlmsAlarmState of
		    ?GLMS_ALARM_ACTIVATED ->
			Oei = sysOei:get_id(),
		log(alert, "Got keyfile_fault_alarm_req Activated with Oei: " ++ 
				integer_to_list(Oei)),
			OeiBin = integer_to_binary(Oei),
			comsaI:send_alarm('KeyFileFault', critical, Dn, "", 
					  [{<<"eventId">>, OeiBin}]),
			Oei;
		    ?GLMS_ALARM_CEASED ->
			log(alert, "Got keyfile_fault_alarm_req Ceased"),
			comsaI:clear_alarm('KeyFileFault', Dn),
			0
		end,
	    lmaLinxHandler:send_keyfile_fault_alarm_rsp(ItcPort, Spid, AlarmEventId),
	    {noreply, State};
	{eu_alarm_ind, GlmsAlarmState, GlmsAlarmSeverity} ->
	    Dn = [list_to_binary("ManagedElement=1"),
		  list_to_binary("SystemFunctions=1"),
		  list_to_binary("Lm=1"),
		  list_to_binary("EmergencyUnlock=1")],
	    case GlmsAlarmState of
		?GLMS_ALARM_ACTIVATED ->
		    log(alert, "Got eu_alarm_ind Activated with Severity: "
			++integer_to_list(GlmsAlarmSeverity)),
		    case GlmsAlarmSeverity of
			?GLMS_ALARM_WARNING ->
			    comsaI:send_alarm('EmergencyUnlockResetKeyRequired', warning, Dn, "");
			?GLMS_ALARM_MAJOR ->
			    comsaI:send_alarm('EmergencyUnlockResetKeyRequired', major, Dn, "")
		    end;
		?GLMS_ALARM_CEASED ->
		    log(alert, "Got eu_alarm_ind Ceased"),
		    comsaI:clear_alarm('EmergencyUnlockResetKeyRequired', Dn)
	    end,
	    {noreply, State};
	{license_key_not_available_alarm_req, AlarmKeyId, GlmsLicenseType, GlmsAlarmState,
	 GlmsAlarmReason} ->
	    case GlmsLicenseType of
		?GLMS_FEATURE_KEY ->
		    Dn = [list_to_binary("ManagedElement=1"),
			  list_to_binary("SystemFunctions=1"),
			  list_to_binary("Lm=1"),
			  list_to_binary("FeatureState="++AlarmKeyId)],
		    AlarmEventId =
			case GlmsAlarmState of
			    ?GLMS_ALARM_ACTIVATED ->
				%% HU92208
				FeatureName = get_description(AlarmKeyId),
				Msg =
				    case GlmsAlarmReason of
					?GLMS_ALARM_REASON_KEY_NOT_AVAILABLE ->
					    lists:flatten(
					      io_lib:format("License key for ~s (~s) required for the "
							    "configured functioning of the Managed "
							    "Element is not available in currently "
							    "installed key file.",
							    [FeatureName, AlarmKeyId]));
					?GLMS_ALARM_REASON_KEY_EXPIRED ->
					    lists:flatten(
					      io_lib:format("License key for ~s (~s) expired and must "
							    "be renewed for the configured "
							    "functioning of the Managed Element.",
							    [FeatureName, AlarmKeyId]))
				    end,
				Oei = sysOei:get_id(),		
				OeiBin = integer_to_binary(Oei),
				log(alert,"Got license_key_not_available_alarm_req Activated for: "
				    ++AlarmKeyId ++ " with Oei: " ++ integer_to_list(Oei)),
				comsaI:send_alarm('LicenseKeyNotAvailable', major, Dn, Msg,
						  [{<<"eventId">>, OeiBin}]),
				Oei;
			    ?GLMS_ALARM_CEASED ->
				log(alert, "Got license_key_not_available_alarm_req Ceased for: "
				    ++AlarmKeyId),
				comsaI:clear_alarm('LicenseKeyNotAvailable', Dn),
				0
			end,
		    lmaLinxHandler:send_license_key_not_available_alarm_rsp(
		      ItcPort, Spid, list_to_binary(AlarmKeyId), GlmsLicenseType, AlarmEventId);
		?GLMS_CAPACITY_KEY ->
		    %% license_key_not_available_alarm_req not supported for capacity keys
		    %% Oei = sysOei:get_id(),
		    lmaLinxHandler:send_license_key_not_available_alarm_rsp(
		      ItcPort, Spid, list_to_binary(AlarmKeyId), GlmsLicenseType, 0)
	    end,
	    {noreply, State};
	{am_alarm_ind, GlmsAlarmState} ->
	    Dn = [list_to_binary("ManagedElement=1"),
		  list_to_binary("SystemFunctions=1"),
		  list_to_binary("Lm=1"),
		  list_to_binary("AutonomousMode=1")],
	    case GlmsAlarmState of
		?GLMS_ALARM_ACTIVATED ->
		    log(alert, "Got am_alarm_ind Activated"),
		    comsaI:send_alarm('AutonomousModeActivated', major, Dn, "");
		?GLMS_ALARM_CEASED ->
		    log(alert, "Got am_alarm_ind Ceased"),
		    comsaI:clear_alarm('AutonomousModeActivated', Dn)
	    end,
	    {noreply, State};
	{gp_alarm_ind, GracePeriodId, GlmsAlarmState, GlmsAlarmSeverity} ->
	    Dn = [list_to_binary("ManagedElement=1"),
		  list_to_binary("SystemFunctions=1"),
		  list_to_binary("Lm=1"),
		  list_to_binary("CapacityState="++GracePeriodId),
		  list_to_binary("GracePeriod="++GracePeriodId)],
	    case GlmsAlarmState of
		?GLMS_ALARM_ACTIVATED ->
		    log(alert, "Got gp_alarm_ind Activated for "++GracePeriodId++" with Severity: "
			++integer_to_list(GlmsAlarmSeverity)),
		    case GlmsAlarmSeverity of
			?GLMS_ALARM_MINOR ->
			    Msg = 
				lists:flatten(io_lib:format("Grace period has been activated "
							    "for CapacityKey=~p.",[GracePeriodId])),
			    comsaI:send_alarm('GracePeriodActivated', minor, Dn, Msg);
			?GLMS_ALARM_MAJOR ->
			    Msg = 
				lists:flatten(io_lib:format("Grace period is about to expire for "
							    "CapacityKey=~p.",[GracePeriodId])),
			    comsaI:send_alarm('GracePeriodActivated', major, Dn, Msg)
		    end;
		?GLMS_ALARM_CEASED ->
		    log(alert, "Got gp_alarm_ind Ceased for "++GracePeriodId),
		    comsaI:clear_alarm('GracePeriodActivated', Dn)
	    end,
	    {noreply, State};
	{license_key_exiration_alarm_ind, GlmsAlarmState, AdditionalInfo} ->
	    Dn = [list_to_binary("ManagedElement=1"),
		  list_to_binary("SystemFunctions=1"),
		  list_to_binary("Lm=1")],
	    case GlmsAlarmState of
		?GLMS_ALARM_ACTIVATED ->
		    log(alert, "Got license_key_exiration_alarm_ind Activated for " 
			++ AdditionalInfo),
		    comsaI:send_alarm('LicenseKeyCloseToExpiration', warning, Dn, AdditionalInfo);
		?GLMS_ALARM_CEASED ->
		    log(alert, "Got license_key_exiration_alarm_ind Ceased"),
		    comsaI:clear_alarm('LicenseKeyCloseToExpiration', Dn)
	    end,
	    {noreply, State};

	%% Dump response
	{dump_capacity_key_data_rsp, GlmsResult, _LastResponse, ResultInfo, Dump} ->
	    case GlmsResult of
		?GLMS_NOK ->
		    log(warning, "Got dump_capacity_key_data_rsp with NOK"),
		    log("GLMS", warning, ResultInfo),
		    lmaDebug:print("No info from glms", capacity_key_data);
		?GLMS_OK ->
		    lmaDebug:print(to_string(Dump), capacity_key_data)
	    end,
	    {noreply, State};
	{dump_feature_key_data_rsp, GlmsResult, _LastResponse, ResultInfo, Dump} ->
	    case GlmsResult of
		?GLMS_NOK ->
		    log("Got dump_feature_key_data_rsp with NOK"),
		    log("GLMS", warning, ResultInfo),
		    lmaDebug:print("No info from glms", feature_key_data);
		?GLMS_OK ->
		    lmaDebug:print(to_string(Dump), feature_key_data)
	    end,
	    {noreply, State};
	{dump_glms_state_data_rsp, GlmsResult, _LastResponse, ResultInfo, Dump} ->
	    case GlmsResult of
		?GLMS_NOK ->
		    log("Got dump_glms_state_data_rsp with NOK"),
		    log("GLMS", warning, ResultInfo),
		    lmaDebug:print("No info from glms", glms_state_data);
		?GLMS_OK ->
		    lmaDebug:print(to_string(Dump), glms_state_data)
	    end,
	    {noreply, State};
	{dump_lcci_client_data_rsp, GlmsResult, _LastResponse, ResultInfo, Dump} ->
	    case GlmsResult of
		?GLMS_NOK ->
		    log("Got dump_lcci_client_data_rsp with NOK"),
		    log("GLMS", warning, ResultInfo),
		    lmaDebug:print("No info from glms", lcci_client_data);
		?GLMS_OK ->
		    lmaDebug:print(to_string(Dump), lcci_client_data)
	    end,
	    {noreply, State};
	{dump_lfci_client_data_rsp, GlmsResult, _LastResponse, ResultInfo, Dump} ->
	    case GlmsResult of
		?GLMS_NOK ->
		    log("Got dump_lfci_client_data_rsp with NOK"),
		    log("GLMS", warning, ResultInfo),
		    lmaDebug:print("No info from glms", lfci_client_data);
		?GLMS_OK ->
		    lmaDebug:print(to_string(Dump), lfci_client_data)
	    end,
	    {noreply, State};

	{heartbeat_rsp} ->
	    {noreply, State#state{glms_alive = alive}};

	{log_ind, LogData} ->
	    log("GLMS", info, LogData),
	    {noreply, State};
	{unknown_signal, SigNo} ->
	    log(warning, "Unknown signal with No:" ++ integer_to_list(SigNo)),
	    {noreply, State}
    end;

handle_info({message, _UnknownItcPort, {_Spid, _ToMboxId, SigNo, Data}},
	    #state{itc_port = _ItcPort} = State) ->
    %% Message recieved from unknown Itc port, print possible log_ind
    case lmaLinxHandler:parse_itc({SigNo, Data}) of
	{log_ind, LogData} ->
	    log(error, "Log message to unknown spid"),
	    log("GLMS", error, LogData),
	    {noreply, State};
	{unknown_signal, SigNo} ->
	    log(error, "Unknown signal with No:" ++ integer_to_list(SigNo) ++" to unknown spid"),
	    {noreply, State}
    end;
 
handle_info({timeout, _, check_heartbeat}, 
	    #state{pid = Pid, spid = Spid, itc_port = ItcPort, timer_ref = TimerRef} = State) ->
    case State#state.glms_alive of
	waiting_for_rsp ->
	    log("Got check_heartbeat but still no answer from the latest call, restart GLMS"),
	    erlang:cancel_timer(TimerRef),
	    itc_close(ItcPort),
	    kill_glms(Pid),
	    %% Don't send restart_glms here. A EXIT will follow the kill_glms call and the restart
	    %% will be done then.
	    %% NewState = restart_glms(State, PortTimer),
	    {noreply, State#state{glms_alive = timed_out}};
	_AliveNotStartedOrTimedOut ->
	    lmaLinxHandler:send_heartbeat_req(ItcPort, Spid),
	    Timerref = start_heartbeat(),
	    {noreply, State#state{glms_alive = waiting_for_rsp, timer_ref = Timerref}}
    end;
handle_info({timeout, _, reset_restart_timer}, #state{glms_restarted = GlmsRestared} = State) ->
    case GlmsRestared of
	{true, PortTime} ->
	    log("Resetting restrart timer since no more restart of GLMS has happen in 3 minutes"),
	    {noreply, State#state{glms_restarted = {false, PortTime}}};
	{false, _PortTime} ->
	    log(warning, "Got reset_restart_timer but state says is not restarted"),
	    {noreply, State}
    end;
handle_info({'EXIT',Port,normal}, 
	    #state{pid = Pid, itc_port = ItcPort, port = Port, 
		   glms_restarted = GlmsRestared, timer_ref = TimerRef} = State) ->
    log(error, "Got Exit from GLMS port, restart GLMS"),
    erlang:cancel_timer(TimerRef),
    case GlmsRestared of
	{true, _PortTime} ->
	    %% GLMS has already been restarted, let LMA restart as well
	    {stop, "lmaGlms has got EXIT from GLMA port twice in 3 minutes, restart LMA",State};
	{false, PortTime} ->
	    itc_close(ItcPort),
	    kill_glms(Pid),
	    NewState = restart_glms(State, PortTime),
	    {noreply, NewState}
    end;
handle_info({Port,{exit_status,_Status}}, #state{port = Port} = State) ->
    log("Got {exit_status,Status} from GLMS port"),
    {noreply, State};

handle_info({mnesia_table_event, {write, featureState, New, [Old], _ActivityId}}, 
	    #state{itc_port = ItcPort, spid = Spid} = State) ->
    OldFeatureState = Old#featureState.featureState,
    NewFeatureState = New#featureState.featureState,
    case OldFeatureState ==  NewFeatureState of
	false ->
	    %% The feaureState has changed, send the new featureState to GLMS
	    FeatureStateId = New#featureState.keyId,
	    log("FeatureState changed changed for "++FeatureStateId++" to: "
		++ integer_to_list(NewFeatureState)),
	    FeatureStateId = New#featureState.keyId,
	    lmaLinxHandler:send_set_feature_state_req(ItcPort, Spid, NewFeatureState, 
						      list_to_binary(FeatureStateId));
	true ->
	    do_nada
    end,
    {noreply, State};
handle_info({mnesia_table_event, {write, lm, New, [Old], _ActivityId}}, 
	    #state{itc_port = ItcPort, spid = Spid} = State) ->
    OldFingerprint = Old#lm.fingerprint,
    NewFingerprint = New#lm.fingerprint,

    case NewFingerprint of
	undefined ->
	    do_nada;
	OldFingerprint ->
	    do_nada;
	_Else ->
	    log("The fingerprint has changed to " ++ NewFingerprint ++", send it to GLMS"),
	    lmaLinxHandler:send_set_fingerprint_req(ItcPort, Spid, list_to_binary(NewFingerprint))
    end,
    {noreply, State};

handle_info({mnesia_table_event, _Event}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    lmaLib:error_msg("lmaGlms, got unknown info: ~p at state: ~p~n",[Info, State]),
    log(error, "Got unknown info"),
    %% io:format("lmaGlms Got Info: ~p~n", [Info]),
    {noreply, State}.


get_description(AlarmKeyId) ->
    %% TR ??
    case mnesia:dirty_read(featureState, {"1", "1", "1", AlarmKeyId}) of
	[] ->
	    %% No description to add to the alarm
	    "";
	[#featureState{description = FeatureName}] ->
	    FeatureName
    end.

to_string(Text) when is_binary(Text) ->
    to_string(binary_to_list(Text));

to_string(Text) when is_list(Text) ->
    lists:takewhile(fun(B) -> B =/= 0 end, Text).

search_for_CXC_in_LKF("CXC4012032", FileData) ->
    %% WP4716  - Search the LKF fileData for the EsiEncryptionTestLicense CXC 
    CXCTag = list_to_binary(["id=\"CXC4012032"]),
    case binary:match(FileData, CXCTag) of
	nomatch ->
	    log("ESI will be encrypted"),
	    true;
	_Found ->
	    log(warning, "The ESI will not be encrypted"),
	    false
    end;
search_for_CXC_in_LKF("CXC4011959", FileData) ->
    %% WP4530 - Search the LKF fileData for the hidden MOM CXC 
    CXCTag = list_to_binary(["id=\"CXC4011959"]),
    SeqNo = find_sequence_no(FileData),
    case binary:match(FileData, CXCTag) of
	nomatch ->
	    %% log("Ordinary MOM should be loaded at next COM restart"),
	    %% lmaLib:update_lkf_table(FileData, SeqNo, false);
	    lmaLib:update_lkf_table(FileData, SeqNo);
	_Found ->
	    %% log(warning, "Test MOM should be loaded at next COM restart"),
	    %% lmaLib:update_lkf_table(FileData, SeqNo, true)
	    lmaLib:update_lkf_table(FileData, SeqNo)
    end.

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
terminate(_Reason, #state{pid = Pid, itc_port = ItcPort} = _State) ->
    log(warning, "lmaGlms will terminate"),
    case Pid of
	undefined ->
	    do_nada;
	_Else ->
	    itc_close(ItcPort),
	    kill_glms(Pid)
    end,
    ok.

itc_close(undefined) ->
    ok;
itc_close(ItcPort) ->
    itc:close(ItcPort).

kill_glms(Pid) ->
    log(warning, "Kill GLMS pid: " ++integer_to_list(Pid)),
    PidStr = integer_to_list(Pid),
    SigStr = integer_to_list(15), % Terminate
    os:cmd("kill -"++SigStr++" "++PidStr).

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

%%% ----------------------------------------------------------
%%% #           find_latest_installed_lkf(lma_KFLocationList)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Checks if there is an LKF stored at the node, and if - give GLMS the 
%%% one with the highest sequence number i.e. the latest one installed on the node
%%% During backup & restore the lma_KFLocation in mnesia could contain an old LKF.
%%% ----------------------------------------------------------
find_latest_installed_lkf(Obj) ->
    LKFPath = lmaLib:get_LKF_path(license),
    FileDataFromMnesia = Obj#lma_KFLocation.info,
    SequenceNoMnesia = Obj#lma_KFLocation.sequenceNo,
    {FileDataFromFile, SequenceNoFile} = find_sequence_no_on_node(),
    
    Encryption = 
	case SequenceNoFile >= SequenceNoMnesia of
	    true ->
		%% Backup-restore is ongoing if the SequenceNo on disk has a higher
		%% value then the SequenceNo in mnesia.
		log(warning, "Backup-restore might be ongoing. The LKF will be restored"
		    " from the disk instead from mnesia"),
		log("Sequence no on disk: " ++ integer_to_list(SequenceNoFile) 
		    ++ " Sequence no on mnesia: "++ integer_to_list(SequenceNoMnesia)),
		search_for_CXC_in_LKF("CXC4011959", FileDataFromFile),
		search_for_CXC_in_LKF("CXC4012032", FileDataFromFile);
	    false ->
		ok = filelib:ensure_dir(LKFPath),
		ok = file:write_file(LKFPath, FileDataFromMnesia, [sync]),
		search_for_CXC_in_LKF("CXC4012032", FileDataFromMnesia)
	end,
    {list_to_binary(LKFPath), ?GLMS_OK, Encryption}.

%%% ----------------------------------------------------------
%%% #           failsafe_find_lkf()
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Check if the LKF in this backup has a lower sequense no 
%%%              then the one, if any, stored at the node. It that case remove
%%%              the LKF on node.
%%% ----------------------------------------------------------
failsafe_find_lkf() ->
    {FileDataNode, SequenceNoNode} = find_sequence_no_on_node(),
    LKFPath = lmaLib:get_LKF_path(license),
    case mnesia:dirty_read(lma_KFLocation, 1) of
	[Obj] ->
	    SequenceNoMnesia = Obj#lma_KFLocation.sequenceNo,
	    FileDataMnesia = Obj#lma_KFLocation.info,
	    
	    case SequenceNoMnesia of
		undefined ->
		    check_file_data(FileDataNode);
		SequenceNoMnesia ->
		    case SequenceNoNode >= SequenceNoMnesia of
			true ->
			    %% Sequence no in backup is lower or the same as the sequence no on
			    %% node i.e. the version in the backup should be keept and the
			    %% node version thrown.
			    log(warning, "Failsafe is ongoing. The LKF will be restored"
				" from the backup instead from the node"),
			    ok = filelib:ensure_dir(LKFPath),
			    ok = file:write_file(LKFPath, FileDataMnesia, [sync]);
			false ->
			    %% Sequence no on backup is higher then the one stored on node
			    log(error, "Failsafe is ongoing but the LKF in the backup has "
				"higher sequence no as on the node") 
		    end,
		    log("Sequence no on disk: " ++ integer_to_list(SequenceNoNode) 
			++ " Sequence no in backup: "++ integer_to_list(SequenceNoMnesia))
	    end;
	_Else ->
	    check_file_data(FileDataNode)
    end.

find_sequence_no_on_node() ->
    LKFPath = lmaLib:get_LKF_path(license),
    case file:read_file(LKFPath) of
	{ok, FileData} ->
	    %% A LKF is stored at the node, check the sequence number
	    SequenceNo = find_sequence_no(FileData),
	    {FileData, SequenceNo};
	_Else ->
	    %% No LKF stored at the node
	    {none, 0}
    end.

find_sequence_no(FileData) ->
    Re = ".*sequenceNumber>+(\\d+).*",
    Options = [global, {capture, all, binary}],
    case re:run(FileData, Re, Options) of
	{match, [[_, Value]]} ->
	    list_to_integer(binary_to_list(Value));
	nomatch ->
	    log(error, "Faild to read the contain of the LKF stored at the node"),
	    lmaLib:error_msg("lmaGlms failed to read sequenceNumber of the file stored at "
			     "the node~n", []),
	    0
    end.
	    
check_file_data(none) ->
    log("No LKF stored in mensia, no LKF installed at the node");
check_file_data(_Else) ->
    LKFPath = lmaLib:get_LKF_path(license),
    log("No LKF stored in mensia, a LKF stored at the node, remove this one"),
    file:delete(LKFPath).
    
    
%%% ----------------------------------------------------------
%%% @doc Send activate to the GLMS process
%%% @private
%%% @end
%%% ----------------------------------------------------------
send_activate_to_glms(ItcPort, Spid) ->
    log("Send activate to GLMS with ProtocolVersion = 1 and LogLevel = GLMS_LOG_HIGH"),
    ProtocolVersion = 1,
    LogLevel = ?GLMS_LOG_HIGH,
    RestrictedLicenses = ["CXC4040004"],
    lmaLinxHandler:send_activate(ItcPort, Spid, ProtocolVersion, LogLevel, RestrictedLicenses).

%%% ----------------------------------------------------------
%%% @doc Starting the GLMS process
%%% @private
%%% @end
%%% ----------------------------------------------------------
start_glms() ->
    start_glms(default).

start_glms(Timer) ->
    App = "lma_app",
    LmaPrivDir = code:priv_dir(lma),
    BinDir = sysEnv:target_bin_dir(),
    GlmsProg = filename:join([LmaPrivDir, BinDir, App]),

    %% Look in dev_patches and replace file if needed.
    RealGlmsProg = swmI:find_file(GlmsProg),
    log("Starting GLMS by open port"), 
    try
	case Timer of
	    default ->
		open_port(
		  {spawn_executable, RealGlmsProg},[exit_status]);
	    Time when is_integer(Time) ->
		open_port(
		  {spawn_executable, RealGlmsProg},[exit_status,
						    {env,[{"GLMS_TIME_SCALE", 
							   integer_to_list(Timer)}]}])
	end
    of
	Port ->
	    Pid = find_os_pid(erlang:port_info(Port)),
	    log("GLMS started with port: " ++ erlang:port_to_list(Port) ++ " and pid: " ++ 
		    integer_to_list(Pid)),
	    {Port,Pid}
    catch error:Reason ->
	    lmaLib:error_msg("lmaGlms failed to open port with GLMS reason:  ~p~n", 
			     [Reason]),
	    {error, Reason}
    end.

find_os_pid([]) ->
    io:format("lmaGlms; No PID found, glms could not have been started correct");
find_os_pid([{os_pid,Pid}| _Rest]) ->
    Pid;
find_os_pid([{_Else, _Value}| Rest]) ->
    find_os_pid(Rest).

%%% ----------------------------------------------------------
%%% #           start_heartbeat()
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Starting the health check of the GLMS process
%%% ----------------------------------------------------------
start_heartbeat() ->
    erlang:start_timer(?HEARTBEAT_INTERVAL, self(), check_heartbeat).

%%% ----------------------------------------------------------
%%% #           start_channel(Proto, Host, Port, User, Password, File)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Starting the channel towards  server
%%% ----------------------------------------------------------
start_channel(Proto, Host, Port, User, Password, File) ->
    TmpLKFPath = lmaLib:get_LKF_path(tmp),
    case ftpI:start_channel(Proto, Host, Port, User, to_string(Password)) of
    	{ok, Pid, ConnectionRef} ->
	    try
		{ok, FileData} = ftpI:read_file(Proto, Pid, File),
		lmaLib:update_lkf_table(FileData, undefined),
		TmpLKFPath = lmaLib:get_LKF_path(tmp),
		ok = filelib:ensure_dir(TmpLKFPath),
		ok = file:write_file(TmpLKFPath, FileData, [sync])
	    of
		ok ->
		    log("Succefully fetched the LKF from the server"),
		    {?GLMS_OK, TmpLKFPath}
	    catch
		error:{badmatch, {error, Reason}} ->
		    lmaLib:error_msg("lmaGlms failed to save LKF file ~p, reason:  ~p~n", 
				     [File, Reason]),
		    log(error, "Failed to save LKF file, reason: " ++ atom_to_list(Reason)),
		    {?GLMS_NOK, TmpLKFPath}
	    after
		ftpI:stop_channel(Proto, Pid, ConnectionRef)
	    end;
	{error, EHost} when EHost =:= etimedout 
                   orelse EHost =:= ehost ->
	    lmaLib:error_msg("lmaGlms failed to establish a connection to remote server ~p~n",
			     [Host]),
	    log(error, "Failed to establish a connection to remote server:" ++ Host),
	    {?GLMS_NOK, TmpLKFPath};
	{error, Reason} ->
	    lmaLib:error_msg("lmaGlms failed to start channel towards ~p, reason: ~p~n",
			     [Proto, Reason]),
	    log(error, "Failed to start channel towards " ++ atom_to_list(Proto)),
	    {?GLMS_NOK, TmpLKFPath}
    end.

%%% ----------------------------------------------------------
%%% #           restart_glms(State, Timer)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Restarting the GLMS process
%%% ----------------------------------------------------------
restart_glms(State, Timer) ->
    case start_glms(Timer) of
	{error,Reason} ->
	    erlang:error(Reason),
	    {stop, Reason};
	{NewPort, NewGlmsPid} ->
	    %% Start up ITC
	    NewItcPort = itc:open("lma"),
	    HuntRef = itc:hunt(NewItcPort, "glms"),
	    NewSpid = receive 
			  {mailbox_up, _ItcPort, HuntRef, S} ->
			      log("Receive mailbox up"),
			      S
		      after ?TIMEOUT ->
			      log(error, "Hunt for glms failed") ,   
			      undefined
		      end,
	    ok = itc:listen(NewItcPort),
	    
	    send_activate_to_glms(NewItcPort, NewSpid),
	    lmaLinxHandler:send_heartbeat_req(NewItcPort, NewSpid),
	    NewTimerRef = start_heartbeat(),
	    erlang:start_timer(?RESET_RESTART_TIMER, self(), reset_restart_timer),
	    GlmsRestarted = case Timer of 
				default ->
				    {true, Timer};
				_Else ->
				    {false, Timer}
			    end,
	    log("Subscribe over LIHI again"), 
	    lmaLicenseUser:subscribe_over_lihi(),
	    State#state{port = NewPort, pid = NewGlmsPid, itc_port = NewItcPort,
			spid = NewSpid, glms_restarted = GlmsRestarted, 
			timer_ref = NewTimerRef}
    end.

split_list(0, _FeatureStateIds, _Port, _Spid, _Request) ->
    done;
split_list(NrOfIds, IdList, ItcPort, Spid, Request) ->
    %% Find the first Id in the list, 0 will end the Key
    {Id, RestIds} = lists:splitwith(fun(B) -> B =/= 0 end, IdList),
    %% Remove all the zeros untill next id starts.
    StrippedRestIds = lists:subtract(RestIds, lists:takewhile(fun(B) -> B == 0 end, RestIds)),
    lmaLinxHandler:send_read_mo_req(ItcPort, Spid, list_to_binary(Id), Request),
    split_list(NrOfIds -1, StrippedRestIds, ItcPort, Spid, Request).

log(String) ->
    log("lmaGlms", info, String).

log2(_String) -> 
    %% removed logging to speed up startup and upgrade. 
    %% case swmI:is_upgrade_ongoing() of
    %% 	true ->
    %% 	    %% Do not log since upgrade is on
    %% 	    do_nada;
    %% 	false ->
    %% 	    logI:write_log("LicensingLog", "lmaGlms", info, String)
    %% end,
    ok.

log(Severity, String) ->
    log("lmaGlms", Severity, String).

log(M, Severity, String) ->
    logI:write_log("LicensingLog", M, Severity, String).

update_table("LicenseManagerSoftwareData", Index, Value) -> 
    mnesia:dirty_write(#licenseManagerSoftwareData{index = Index, info = Value});
update_table("FeatureKeySoftwareData", Index, Value) -> 
    mnesia:dirty_write(#featureKeySoftwareData{index = Index, info = Value});
update_table("LicenseManagerPersistentData", Index, Value) -> 
    mnesia:dirty_write(#licenseManagerPersistentData{index = Index, info = Value});
update_table("KeyFilePersistentData", Index, Value) -> 
    mnesia:dirty_write(#keyFilePersistentData{index = Index, info = Value});
update_table("FeatureKeyPersistentData", Index, Value) -> 
    mnesia:dirty_write(#featureKeyPersistentData{index = Index, info = Value});
update_table("EUPersistentData", _Index, Value) -> 
    %% Save EU on disk and not i mnesia in order not to reuse with an backup
    file_write("eUPersistentData", Value);
update_table("IUPersistentData", Index, Value) -> 
    mnesia:dirty_write(#iUPersistentData{index = Index, info = Value});
update_table("PUPersistentData", Index, Value) -> 
    mnesia:dirty_write(#pUPersistentData{index = Index, info = Value});
update_table("AmPersistentData", Index, Value) -> 
    mnesia:dirty_write(#amPersistentData{index = Index, info = Value});
update_table("FeatureStateSoftwareData", Index, Value) -> 
    mnesia:dirty_write(#featureStateSoftwareData{index = Index, info = Value});
update_table("FeatureStatePersistentData", Index, Value) -> 
    mnesia:dirty_write(#featureStatePersistentData{index = Index, info = Value});
update_table("CapacityStateSoftwareData", Index, Value) -> 
    mnesia:dirty_write(#capacityStateSoftwareData{index = Index, info = Value});
update_table("CapacityStatePersistentData", Index, Value) -> 
    mnesia:dirty_write(#capacityStatePersistentData{index = Index, info = Value});
update_table("CapacityKeySoftwareData", Index, Value) -> 
    mnesia:dirty_write(#capacityKeySoftwareData{index = Index, info = Value});
update_table("CapacityKeyPersistentData", Index, Value) -> 
    mnesia:dirty_write(#capacityKeyPersistentData{index = Index, info = Value});
update_table("LicenseSupportSoftwareData", Index, Value) -> 
    mnesia:dirty_write(#licenseSupportSoftwareData{index = Index, info = Value});
update_table("LicenseSupportPersistentData", _Index, Value) ->
    %% Save LicenseSupportPersistentData on disk and not i mnesia in order not to reuse with an backup
    file_write("licenseSupportPersistentData", Value). % SP086 LicenseSupportPersistentData storage

update_table("LicenseManagerSoftwareData", Index) -> 
    mnesia:dirty_delete(licenseManagerSoftwareData, Index);
update_table("FeatureKeySoftwareData", Index) -> 
    mnesia:dirty_delete(featureKeySoftwareData, Index);
update_table("LicenseManagerPersistentData", Index) -> 
    mnesia:dirty_delete(licenseManagerPersistentData, Index);
update_table("KeyFilePersistentData", Index) -> 
    mnesia:dirty_delete(keyFilePersistentData, Index);
update_table("FeatureKeyPersistentData", Index) -> 
    mnesia:dirty_delete(featureKeyPersistentData, Index);
update_table("EUPersistentData", _Index) ->
    %% Save EU on disk and not i mnesia in order not to reuse with an backup
    File = filename:join(lmaLib:get_lma_dir(), "eUPersistentData"),
    file:delete(File);
update_table("IUPersistentData", Index) -> 
    mnesia:dirty_delete(iUPersistentData, Index);
update_table("PUPersistentData", Index) -> 
    mnesia:dirty_delete(pUPersistentData, Index);
update_table("AmPersistentData", Index) -> 
    mnesia:dirty_delete(amPersistentData, Index);
update_table("FeatureStateSoftwareData", Index) ->
    mnesia:dirty_delete(featureStateSoftwareData, Index);
update_table("FeatureStatePersistentData",Index) ->
    mnesia:dirty_delete(featureStatePersistentData, Index);
update_table("LicenseSupportSoftwareData", Index) ->
    mnesia:dirty_delete(licenseSupportSoftwareData, Index);
update_table("LicenseSupportPersistentData", _Index) ->
    % SP086 LicenseSupportPersistentData storage
    %% Save LicenseSupportPersistentData on disk and not in mnesia in order not to reuse with an backup
    File = filename:join(lmaLib:get_lma_dir(), "licenseSupportPersistentData"),
    file:delete(File);
update_table("CapacityStateSoftwareData",Index) ->
    mnesia:dirty_delete(capacityStateSoftwareData, Index);
update_table("CapacityStatePersistentData",Index) ->
    mnesia:dirty_delete(capacityStatePersistentData, Index);
update_table("CapacityKeySoftwareData",Index) ->
    mnesia:dirty_delete(capacityKeySoftwareData, Index);
update_table("CapacityKeyPersistentData",Index) ->
    mnesia:dirty_delete(capacityKeyPersistentData, Index).


table("LicenseManagerSoftwareData") -> licenseManagerSoftwareData;
table("FeatureKeySoftwareData") -> featureKeySoftwareData;
table("LicenseSupportSoftwareData") -> licenseSupportSoftwareData;
table("LicenseSupportPersistentData") -> licenseSupportPersistentData;
table("LicenseManagerPersistentData") -> licenseManagerPersistentData;
table("KeyFilePersistentData") -> keyFilePersistentData;
table("FeatureKeyPersistentData") -> featureKeyPersistentData;
table("EUPersistentData") -> eUPersistentData;
table("IUPersistentData") -> iUPersistentData;
table("PUPersistentData") -> pUPersistentData;
table("AmPersistentData") ->  amPersistentData;
table("FeatureStateSoftwareData") -> featureStateSoftwareData;
table("FeatureStatePersistentData") -> featureStatePersistentData;
table("CapacityStateSoftwareData") -> capacityStateSoftwareData;
table("CapacityStatePersistentData") -> capacityStatePersistentData;
table("CapacityKeySoftwareData") -> capacityKeySoftwareData;
table("CapacityKeyPersistentData") -> capacityKeyPersistentData.

file_write(FileName, Content) ->
    %% Save on disk and not i mnesia in order not to reuse with an backup

    File = filename:join(lmaLib:get_lma_dir(), FileName),
    Bytes = erlang:term_to_binary(Content),
    case file:write_file(File, Bytes) of
	ok ->
	    ok;
	Error ->
	    sysInitI:error_report([{?MODULE, file_write},
				       {file, File},
				       {content, Bytes},
				       Error]),
	    Error
    end.

read_data_and_send_rsp([], _Table, _BinTable, _Port, _Spid, _RequestId, _) ->
    ok;
read_data_and_send_rsp([Index | RestList], Table, BinTable, Port, Spid, RequestId, software) ->
    case mnesia:dirty_read(Table, Index) of
	[{Table, Index, Value}] ->
	    Result = ?GLMS_OK,
	    log2("Sending SoftwareParameterGetRsp for index: " ++ integer_to_list(Index) 
		 ++ " with OK"),
	    lmaLinxHandler:send_software_parameter_get_rsp(Port, Spid, RequestId, Result, BinTable,
							   Index, Value);
	_Else ->
	    Result = ?GLMS_NOK,
	    log("Sending SoftwareParameterGetRsp for index: " ++ integer_to_list(Index) 
		++ " with NOK"),
	    lmaLinxHandler:send_software_parameter_get_rsp(Port, Spid, RequestId, Result, BinTable,
							   Index, <<>>)
    end,
    read_data_and_send_rsp(RestList, Table, BinTable, Port, Spid, RequestId, software);
read_data_and_send_rsp([Index | RestList], eUPersistentData = Table, BinTable, Port, Spid, 
		       RequestId, persistent) ->
    %% Save EU on disk and not i mnesia in order not to reuse with an backup
    File = filename:join(lmaLib:get_lma_dir(), "eUPersistentData"),
    case file:read_file(File) of
	{ok, Bin} ->
	    Value = binary_to_term(Bin),
	    Result = ?GLMS_OK,
	    Index = 1, 
	    
	    lmaLinxHandler:send_persistent_parameter_get_rsp(Port, Spid, RequestId, Result, 
							     BinTable, Index, Value);
	_ ->
	    Result = ?GLMS_NOK,
	    log("Sending PersistentParameterGetRsp for index: " ++ integer_to_list(Index) 
		++ " with NOK"),
	    lmaLinxHandler:send_persistent_parameter_get_rsp(Port, Spid, RequestId, Result, 
							     BinTable, Index, <<>>)
    end,
    read_data_and_send_rsp(RestList, Table, BinTable, Port, Spid, RequestId, persistent);

% SP086 LicenseSupportPersistentData storage
read_data_and_send_rsp([Index | RestList], licenseSupportPersistentData = Table, BinTable, Port, Spid, 
		       RequestId, persistent) ->
    %% Save LicenseSupportPersistentData on disk and not i mnesia in order not to reuse with an backup
    File = filename:join(lmaLib:get_lma_dir(), "licenseSupportPersistentData"),
    case file:read_file(File) of
	{ok, Bin} ->
	    Value = binary_to_term(Bin),
	    Result = ?GLMS_OK,
	    Index = 1, 

	    lmaLinxHandler:send_persistent_parameter_get_rsp(Port, Spid, RequestId, Result, 
							     BinTable, Index, Value);
	_ ->
	    Result = ?GLMS_NOK,
	    log("Sending PersistentParameterGetRsp (licenseSupportPersistentData) for index: " ++ integer_to_list(Index) 
		++ " with NOK"),
	    lmaLinxHandler:send_persistent_parameter_get_rsp(Port, Spid, RequestId, Result, 
							     BinTable, Index, <<>>)
    end,
    read_data_and_send_rsp(RestList, Table, BinTable, Port, Spid, RequestId, persistent);



read_data_and_send_rsp([Index | RestList], Table, BinTable, Port, Spid, RequestId, persistent) ->
    case mnesia:dirty_read(Table, Index) of
	[{Table, Index, Value}] ->
	    Result = ?GLMS_OK,
	    log2("Sending PersistentParameterGetRsp for index: " ++ integer_to_list(Index) 
		++ " with OK"),
	    lmaLinxHandler:send_persistent_parameter_get_rsp(Port, Spid, RequestId, Result, 
							     BinTable, Index, Value);
	_Else ->
	    Result = ?GLMS_NOK,
	    log("Sending PersistentParameterGetRsp for index: " ++ integer_to_list(Index) 
		++ " with NOK"),
	    lmaLinxHandler:send_persistent_parameter_get_rsp(Port, Spid, RequestId, Result, 
							     BinTable, Index, <<>>)
    end,
    read_data_and_send_rsp(RestList, Table, BinTable, Port, Spid, RequestId, persistent).


%%% ----------------------------------------------------------
%SP086
%%% ----------------------------------------------------------
%%% #           handle_notify_ready_for_service(ItcPort, Spid)
%%% Input: 
%%% Output: 
%%% Exceptions: 
%%% Description: Triggered when ready for service is set. 
%%% ----------------------------------------------------------
handle_notify_ready_for_service(ItcPort, Spid)  ->
    info_msg("---READY_FOR_SERVICE notification to LMA~n", []),
    io:format("process: ~p~n", [self()]),
    lmaLinxHandler:send_audit_req(ItcPort, Spid).
    
 

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

