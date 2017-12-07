%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	lih_lici_server.erl %
%%% Author:     etxpeno
%%% Description:
%%%
%%% Modules used:
%%%
%%% ----------------------------------------------------------
-module(lih_lici_server).
-vsn('/main/R1A/R2A/R5A/1').
-author('etxtory').
-behaviour(gen_server).
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2015 All rights reserved.
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
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      ---------  --------    ------------------------
%%% R1A/5      2012-03-07 etxpeno     Copied from git
%%% R1A/7      2012-04-11 etxpeno     Support for lihLKFFaultAlarm and
%%%                                   lihLKFMissingAlarm
%%% R1A/8      2012-09-10 etxpeno     Correct return value from add_status_subscription/2
%%% R2A/5      2013-06-12 etxpeno     Removed calls to lih_fm module
%%% R2A/8      2014-08-19 etxpeno     Set timeout in gen_server:call to 40 seconds
%%% ----------------------------------------------------------
%%% R5A/1      2015-12-07 etxtory     Remove update of parameters file
%%% ----------------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([
	 start/0,
	 start_link/0,
	 stop/0
	]).

-export([
	 activate/0,
	 get_info/0,
	 get_lkf_info/0
	]).

%%% API used by lih_lici_worker.erl
-export([
	 add_subscription/1,
	 remove_subscription/1,
	 get_LKF_installed_rsp/0
	]).

%%% API used by lici.erl
-export([
	 initiate_service/1,
	 terminate_service/1
	]).

%%% API used by lih_db.erl
-export([
	 is_LKF_installed/0
	]).

%%% OAM actions
-export([
	 update_LKF/1,
	 set_emergency_state/0,
	 production_unlock/1,
	 integration_unlock/1
	]).

%%% OAM attributes
-export([
	 get_emergency_state_info/0,
	 get_last_licensing_change/0,
	 get_license_file_url/0
	]).

%%% Internal API used to trigger internal actions
-export([
	 read_LKF/0,
	 update_parameter_file/0
	]).

%% Test purpose
-export([
	 emergency_state_timeout/0
	]).

%% Internal gen_server API
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

-include("lih_lici_sig.hrl").
-include("lih_lici.hrl").
-include("Licensing.hrl").

-record(state,
	{itc_port,
	 lkf_installed = false :: 'false' | {'true', erlang:timestamp()},
	 seq_no = 0 :: non_neg_integer(),
	 emergency = never_used :: 'never_used' |
				   {'active', erlang:timestamp()} |
				   {'degraded', erlang:timestamp()} |
				   {'active_again', erlang:timestamp()} |
				   {'disabled', erlang:timestamp()},
	 finger_print = "" :: string(),
	 prod_unlocked = false :: 'false' |
				  {'false', erlang:timestamp()} |
				  {'true', erlang:timestamp()},
	 integr_unlocked = false :: 'false' |
				    {'false', erlang:timestamp()} |
				    {'true', erlang:timestamp()},
	 parameter_file_md5 :: 'undefined' | binary(),
	 parameter_file_timer :: 'undefined' | timer:tref(),
	 sub_counter = 0 :: non_neg_integer(),
	 lkf_file_timer :: 'undefined' | timer:tref(),
	 lkf = [] :: [{atom(), term()}],
	 unlock_timer :: 'undefined' | timer:tref()}
       ).

-define(SERVER, ?MODULE).
-define(SECONDS_IN_7DAYS, 604800).
-define(SECONDS_IN_21DAYS, 1814400).

-define(SECONDS_IN_50YEARS, 1576800000).

-define(CALL_TIMEOUT, 40000).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

start() ->
    Args = [],
    gen_server:start({local, ?SERVER}, ?MODULE, Args, []).

start_link() ->
    Args = [],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

stop() ->
    gen_server:cast(?SERVER, stop).

-spec get_info() -> [{atom(), term()}].
get_info() ->
    gen_server:call(?SERVER, getInfo, ?CALL_TIMEOUT).

-spec get_lkf_info() -> [{atom(), term()}].
get_lkf_info() ->
    gen_server:call(?SERVER, getLkfInfo, ?CALL_TIMEOUT).

-spec add_subscription(SubscriptionId) -> Result when
      SubscriptionId :: {'feature', FeatureId} |
			{'capacity', CapacityId} |
			'status',
      Result :: {'true', FeatureId, FeatureStatus, ChangeReason} |
		{'false', FeatureId, RejectReason} |
		{'true', CapacityId, LicensedLevel, HardLimit, ChangeReason} |
		{'false', CapacityId, RejectReason} |
		{'true', LicMgrStatus, EmergencyCounter} |
		{'false', RejectReason},
      FeatureId :: string(),
      FeatureStatus :: feature_status(),
      CapacityId :: string(),
      LicensedLevel :: capacity_limit(),
      HardLimit :: capacity_limit(),
      LicMgrStatus :: lic_mgr_status(),
      EmergencyCounter :: emergency_counter(),
      ChangeReason :: change_reason(),
      RejectReason :: reject_reason().
add_subscription(SubscriptionId) ->
    gen_server:call(?SERVER, {addSubscription, SubscriptionId, self()},
		    ?CALL_TIMEOUT).

remove_subscription(SubscriptionId) ->
    gen_server:call(?SERVER, {removeSubscription, SubscriptionId, self()},
		    ?CALL_TIMEOUT).

-spec get_LKF_installed_rsp() -> Result when
      Result :: ?CELLO_LICI_LKF_INSTALLED | ?CELLO_LICI_LKF_NOT_INSTALLED.
get_LKF_installed_rsp() ->
    gen_server:call(?SERVER, getLKFInstalledRsp, ?CALL_TIMEOUT).

initiate_service({SignalRevision, WantedPvList}) ->
    try gen_server:call(?SERVER,
			{initiateService, SignalRevision, WantedPvList},
			timer:minutes(1)) of
	{lici_init_cfm, Pid, _SignalRevision, _SelectedPV} = Result ->
	    put({lici_ref, Pid}, monitor(process, Pid)),
	    Result;
	Result ->
	    Result
    catch
	exit:{noproc, _Reason} ->
	    timer:sleep(100),
	    initiate_service({SignalRevision, WantedPvList})
    end.

-spec terminate_service(Pid) -> Result when
      Pid :: pid(),
      Result :: atom().
terminate_service(Pid) ->
    case erase({lici_ref, Pid}) of
	undefined                  -> ok;
	Ref when is_reference(Ref) -> demonitor(Ref, [flush])
    end,

    gen_server:call(?SERVER, {terminateService, Pid}, ?CALL_TIMEOUT).

-spec is_LKF_installed() -> Result when
      Result :: boolean().
is_LKF_installed() ->
    gen_server:call(?SERVER, isLKFInstalled, ?CALL_TIMEOUT).

-spec update_LKF({UserId, Password, IpAddress, File}) -> Result when
      UserId :: string(),
      Password :: string(),
      IpAddress :: string(),
      File :: string(),
      Result :: 'ok' | 'nok'.
update_LKF({UserId, Password, IpAddress, File}) ->
    gen_server:call(?SERVER, {updateLKF, UserId, Password, IpAddress, File},
		    ?CALL_TIMEOUT).

-spec set_emergency_state() -> Result when
      Result :: 'ok' | 'nok'.
set_emergency_state() ->
    gen_server:call(?SERVER, setEmergencyState, ?CALL_TIMEOUT).

-spec production_unlock(Action) -> Result when
      Action :: 'activate' | 'deactivate' | 'status',
      Result ::  'ok' | {'error', string()} |
		 {status, Active, Used, TimeLeft},
      Active :: boolean(),
      Used :: boolean(),
      TimeLeft :: non_neg_integer().
production_unlock(Action) ->
    gen_server:call(?SERVER, {productionUnlock, Action}, ?CALL_TIMEOUT).

-spec integration_unlock(Action) ->  Result when
      Action :: 'activate' | 'deactivate' | 'status',
      Result ::  'ok' | {'error', string()} |
		 {status, Active, Used, TimeLeft},
      Active :: boolean(),
      Used :: boolean(),
      TimeLeft :: non_neg_integer().
integration_unlock(Action) ->
    gen_server:call(?SERVER, {integrationUnlock, Action}, ?CALL_TIMEOUT).

emergency_state_timeout() ->
    gen_server:call(?SERVER, emergencyStateTimeout, ?CALL_TIMEOUT).

read_LKF() ->
    gen_server:cast(?SERVER, readLKF).

update_parameter_file() ->
    gen_server:cast(?SERVER, updateParameterFile).

-spec get_emergency_state_info() -> Result when
      Result :: {integer(), integer()}.
get_emergency_state_info() ->
    gen_server:call(?SERVER, getEmergencyStateInfo, ?CALL_TIMEOUT).

-spec get_last_licensing_change() -> Result when
      Result :: string().
get_last_licensing_change() ->
    gen_server:call(?SERVER, getLastLicensingChange, ?CALL_TIMEOUT).

-spec get_license_file_url() -> Result when
      Result :: string().
get_license_file_url() ->
    gen_server:call(?SERVER, getLicenseFileUrl, ?CALL_TIMEOUT).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init(_) -> {'ok',#state{}} | 'ignore'.
init(_Args) ->
    try
	erlang:process_flag(trap_exit, true),

	lici_subscription = ets:new(lici_subscription,
				    [named_table,
				     {keypos, #lici_subscription.id}]),
	lici_provider = ets:new(lici_provider,
				[named_table, bag,
				 {keypos, #lici_provider.spid}]),
	lici_licensekey = ets:new(lici_licensekey,
				  [named_table, bag,
				   {keypos, #lici_license_key.id}]),

	mnesia:subscribe({table, licensing, detailed}),

	State0 = #state{finger_print         = lih_db:get_fingerprint(),
			parameter_file_timer = trigger_parameter_file_check()},
	State1 = read_stored_values(State0),

	case is_local_restart() of
	    false ->
		{ok, State1};
	    true ->
		{ok, handle_activate(State1)}
	end
    catch
	error:_Reason ->
	    %% error_logger:error_msg("~p:init() failed~n"
	    %% 			   "Reason: ~p~n"
	    %% 			   "Server ~p is not started~n",
	    %% 			   [?MODULE, Reason, ?SERVER]),
	    ignore
    end.

activate() ->
    gen_server:cast(?MODULE, activate).

handle_call(getInfo, _From, S) ->
    handle_get_info(S);
handle_call(getLkfInfo, _From, S) ->
    handle_get_lkf_info(S);

handle_call({addSubscription, {feature, RawFeatureId}, Pid}, _From, S) ->
    FeatureId = lih_lib:convert_id(RawFeatureId),
    Reply = add_feature_subscription({FeatureId, Pid}, S),
    NewState =
	case Reply of
	    {true, _, _, _} ->
		Cnt = S#state.sub_counter,
		S#state{sub_counter = Cnt+1};
	    {false, _, _} ->
		S
	end,
    {reply, Reply, NewState};
handle_call({addSubscription, {capacity, RawCapacityId}, Pid}, _From, S) ->
    CapacityId = lih_lib:convert_id(RawCapacityId),
    Reply = add_capacity_subscription({CapacityId, Pid}, S),
    NewState =
	case Reply of
	    {true, _, _, _, _} ->
		Cnt = S#state.sub_counter,
		S#state{sub_counter = Cnt+1};
	    {false, _, _} ->
		S
	end,
    {reply, Reply, NewState};
handle_call({addSubscription, status, Pid}, _From, S) ->
    Reply = add_status_subscription(Pid, S),
    NewState =
	case Reply of
	    {true, _, _} ->
		Cnt = S#state.sub_counter,
		S#state{sub_counter = Cnt+1};
	    {false, _} ->
		S
	end,
    {reply, Reply, NewState};
handle_call({removeSubscription, {feature, RawFeatureId}, Pid}, _From, S) ->
    FeatureId = lih_lib:convert_id(RawFeatureId),
    Reply = {true, FeatureId},
    case remove_feature_subscription({FeatureId, Pid}) of
	true ->
	    Cnt = S#state.sub_counter,
	    {reply, Reply, S#state{sub_counter = Cnt-1}};
	false ->
	    {reply, Reply, S}
    end;

handle_call(isLKFInstalled, _From, S) ->
    handle_is_LKF_installed(S);
handle_call(getLKFInstalledRsp, _From, S) ->
    handle_get_LKF_installed_rsp(S);
handle_call({updateLKF, UserId, Password, IpAddress, RemoteFile},
	    _From, S) ->
    handle_update_LKF({UserId, Password, IpAddress, RemoteFile}, S);
handle_call(setEmergencyState, _From, S) ->
    handle_set_emergency_state(S);
handle_call({productionUnlock, Action}, _From, S) ->
    handle_production_unlock(Action, S);
handle_call({integrationUnlock, Action}, _From, S) ->
    handle_integration_unlock(Action, S);
handle_call(emergencyStateTimeout, _From, S) ->
    handle_emergency_state_timeout(S);
handle_call(getEmergencyStateInfo, _From, S) ->
    handle_get_emergency_state_info(S);
handle_call(getLastLicensingChange, _From, S) ->
    handle_get_last_licensing_change(S);
handle_call(getLicenseFileUrl, _From, S) ->
    handle_get_license_file_url(S);
handle_call({initiateService, SignalRevision, WantedPvList}, From, S) ->
    {noreply, handle_init_req({From, SignalRevision, WantedPvList}, S)};

handle_call({terminateService, Pid}, From, S) ->
    {noreply, handle_term_req({From, Pid}, S)};

handle_call(_Msg, _From, S) ->
    {reply, _Msg, S}.

handle_cast(readLKF, S) ->
    LKFPath = lih_lib:get_LKF_path(license),
    case check_license_file(LKFPath, S) of
	{false, _} ->
	    BackupLKFPath = lih_lib:get_LKF_path(backup),
	    case check_license_file(BackupLKFPath, S) of
		{false, LKF} ->
		    {noreply, update_after_new_LKF(LKF, S, false)};
		{true, LKF} ->
		    file:copy(BackupLKFPath, LKFPath),
		    {noreply, update_after_new_LKF(LKF, S, false)}
	    end;
	{true, LKF} ->
	    {noreply, update_after_new_LKF(LKF, S, false)}
    end;

handle_cast(activate, S) ->
    {noreply, handle_activate(S)};

handle_cast(updateParameterFile, S) ->
    State0 = handle_check_unlock(read_stored_values(S)),
    {noreply, write_stored_values(State0)};

handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({message, Port, {Spid, _ToMboxId, SigNo, Data}},
	    #state{itc_port = Port} = S) ->
    case lih_linx_handler:parse_linx({SigNo, Data}) of
	{lici_initiate_service_req, SignalRevision,
	 PvFirstWanted, PvSecondWanted, PvThirdWanted} ->
	    WantedPvList = [PvFirstWanted, PvSecondWanted, PvThirdWanted],
	    {noreply, handle_init_req({Spid, SignalRevision, WantedPvList}, S)};
	lici_terminate_service_req ->
	    NewState = handle_term_req({Spid, all}, S),
	    {noreply, NewState};
	_ ->
	    {noreply, S}
    end;
handle_info(check_parameter_file,
	    #state{parameter_file_md5 = MD5} = S) ->
    ParameterFile = lih_lib:get_parameter_file_path(),
    NewState =
	case get_md5(ParameterFile) of
	    MD5 when MD5 =/= undefined ->
		S#state{parameter_file_timer = trigger_parameter_file_check()};
	    _ ->
		%% error_logger:info_msg("Someone has tampered with the license parameter file~n"
		%% 		      "Creating a new license parameter file~n"),
		write_stored_values(S#state{parameter_file_timer = trigger_parameter_file_check()})
	end,
    {noreply, NewState};
handle_info(check_lkf_file, S) ->
    handle_cast(readLKF, S);
handle_info(check_unlock, S) ->
    State0 = handle_check_unlock(S),
    {noreply, write_stored_values(State0)};
handle_info({mnesia_table_event,
	     {write, licensing, #licensing{fingerprint = FingerPrint}, _, _}},
	    S) ->
    {noreply, S#state{finger_print = FingerPrint}};
handle_info({'EXIT', Pid, _What}, S) ->
    Cnt = remove_subscriptions(Pid, S#state.sub_counter),
    delete_providers(Pid),
    NewState = S#state{sub_counter = Cnt},
    {noreply, NewState};
handle_info({'DOWN', _Ref, process, _Pid, _What}, S) ->
    check_lici_checker(),
    {noreply, S};

handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

-spec code_change(OldVsn :: {down,term()} | term(),
                  State :: #state{},
                  Extra :: term()) -> {ok, NewState :: #state{}}.
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

handle_init_req({From, SignalRevision, WantedPvList}, S) ->
    CheckPv = check_pv(WantedPvList),
    CheckClients = check_clients(),

    case From of
	Spid when is_integer(Spid) ->
	    Fun =
		fun(Arg1) ->
			Data =
			    <<SignalRevision:4/native-unsigned-integer-unit:8,
			      ?CELLO_LICI_PV3:4/native-unsigned-integer-unit:8,
			      Arg1:2/native-unsigned-integer-unit:8>>,
			itc:send(S#state.itc_port, Spid,
				 ?CELLO_LICI_INITIATE_SERVICE_REJ, Data)
		end;
	{Spid, _Tag} ->
	    Fun =
		fun(Arg1) ->
			lih_erlang_handler:send_lici_init_rej(From,
							      {SignalRevision,
							       Arg1})
		end
    end,

    if CheckPv =:= false ->
	    Fun(?CELLO_LICI_INVALID_PROTOCOL_VERSION);
       not CheckClients ->
	    Fun(?CELLO_LICI_UNEXPECTED_ERROR);
       true ->
	    {true, SelectedPV} = CheckPv,
	    case lih_lici_worker:start_link({From, SignalRevision,
					     SelectedPV}) of
		{ok, Pid} ->
		    Obj = #lici_provider{spid = Spid, provider_pid = Pid},
		    true = ets:insert(lici_provider, Obj);
		{error, _Error} ->
		    Fun(?CELLO_LICI_UNEXPECTED_ERROR)
	    end
    end,
    S.

handle_term_req({From, Pid}, S) ->
    case From of
	Spid when is_integer(Spid) ->
	    ok;
	{Spid, _Tag} ->
	    ok
    end,

    Providers = ets:lookup(lici_provider, Spid),

    Cnt =
	lists:foldl(
	  fun(#lici_provider{provider_pid = Pid0} = Obj,
	      OldCnt) when Pid =:= Pid0;
			   Pid =:= all ->
		  unlink(Pid0),
		  NewCnt = remove_subscriptions(Pid0, OldCnt),
		  lih_lici_worker:stop(Pid0),
		  ets:delete_object(lici_provider, Obj),
		  NewCnt;
	     (_, OldCnt) ->
		  OldCnt
	  end, S#state.sub_counter, Providers),

    if
	is_integer(Spid) ->
	    itc:send(S#state.itc_port, Spid,
		     ?CELLO_LICI_TERMINATE_SERVICE_CFM, <<>>);
	is_pid(Spid) ->
	    lih_erlang_handler:send_lici_term_cfm(From)
    end,

    S#state{sub_counter = Cnt}.

handle_activate(S) ->
    Port = itc:open("LICI"),
    itc:listen(Port),

    %% Triggers an LKF check
    read_LKF(),

    ok = lih_cb:init(),
    ok = lih_cli:register(),

    State0 = handle_check_unlock(S#state{itc_port = Port}),
    write_stored_values(State0).

check_pv([])    -> false;
check_pv([Pv|_]) when Pv =:= ?CELLO_LICI_PV1;
		      Pv =:= ?CELLO_LICI_PV2;
		      Pv =:= ?CELLO_LICI_PV3 -> {true, Pv};
check_pv([_|T]) -> check_pv(T).

check_clients() ->
    ets:info(lici_provider, size) =< ?MAX_NR_OF_CLIENTS.

read_stored_values(S) ->
    case read_parameter_file() of
	{Line1, Line2, Line3, Line4, Line5, Line6, Line7, Line8, Line9, MD5} ->
	    S#state{lkf_installed   = convert_to(lkf_installed, {Line1, Line2}),
		    seq_no          = convert_to(seq_no,    Line3),
		    emergency       = convert_to(emergency, {Line4, Line5}),
		    prod_unlocked   = convert_to(prod_unlocked, {Line6, Line7}),
		    integr_unlocked = convert_to(integr_unlocked,
						 {Line8, Line9}),
		    parameter_file_md5 = MD5
		   };
	false ->
	    %% error_logger:info_msg("Couldn't find the LICI parameter file~n"
	    %% 			  "Creating the file using default values~n"),

%%% Start code for increment 2 (etxpeno 20121026)
	    write_stored_values(S#state{integr_unlocked = {true, get_timestamp()}})
%%% End code for increment 2
	    %% write_stored_values(S)
    end.

read_parameter_file() ->
    ParameterFile = lih_lib:get_parameter_file_path(),
    read_parameter_files([ParameterFile]).

read_parameter_files([]) ->
    false;
read_parameter_files([File|T]) ->
    case read_parameter_file(File) of
	false ->
	    read_parameter_files(T);
	Result ->
	    Result
    end.

read_parameter_file(File) ->
    case file:read_file(File) of
	{ok, Binary} ->
	    MD5 = erlang:md5(Binary),
	    case binary:split(Binary, <<"\n">>, [global,trim]) of
		[Line1, Line2, Line3, Line4, Line5, Line6, Line7, Line8,
		 Line9] ->
		    {binary_to_list(Line1),
		     binary_to_list(Line2),
		     binary_to_list(Line3),
		     binary_to_list(Line4),
		     binary_to_list(Line5),
		     binary_to_list(Line6),
		     binary_to_list(Line7),
		     binary_to_list(Line8),
		     binary_to_list(Line9),
		     MD5};
		_ ->
		    false
	    end;
	{error, _Reason} ->
	    false
    end.

convert_to(seq_no, SeqNo) -> list_to_integer(SeqNo);

convert_to(emergency, {"1", _TimeT}) -> never_used;
convert_to(emergency, {"2",  TimeT}) -> {active,       time_t_to_now(TimeT)};
convert_to(emergency, {"3",  TimeT}) -> {degraded,     time_t_to_now(TimeT)};
convert_to(emergency, {"4",  TimeT}) -> {active_again, time_t_to_now(TimeT)};
convert_to(emergency, {"5",  TimeT}) -> {disabled,     time_t_to_now(TimeT)};

convert_to(lkf_installed, {"0", TimeT})  -> {true, time_t_to_now(TimeT)};
convert_to(lkf_installed, {"1", _TimeT}) -> false;

convert_to(Type, {"0", "1"}) when Type =:= prod_unlocked;
				  Type =:= integr_unlocked ->
    false;
convert_to(Type, {TimeT, "1"}) when Type =:= prod_unlocked;
				    Type =:= integr_unlocked ->
    {false, time_t_to_now(TimeT)};
convert_to(Type, {TimeT, "0"}) when Type =:= prod_unlocked;
				    Type =:= integr_unlocked ->
    {true, time_t_to_now(TimeT)}.

time_t_to_now(Time) when is_integer(Time) ->
    {Time div 1000000, Time rem 1000000, 0};
time_t_to_now(Time) when is_list(Time) ->
    time_t_to_now(list_to_integer(Time)).

write_stored_values(#state{lkf_installed   = LkfInstalled,
			   seq_no          = SeqNo,
			   emergency       = Emergency,
			   prod_unlocked   = ProdUnlocked,
			   integr_unlocked = IntegrUnlocked} = S) ->
    {Line1, Line2} = convert_from(lkf_installed,   LkfInstalled),
    Line3          = convert_from(seq_no,          SeqNo),
    {Line4, Line5} = convert_from(emergency,       Emergency),
    {Line6, Line7} = convert_from(prod_unlocked,   ProdUnlocked),
    {Line8, Line9} = convert_from(integr_unlocked, IntegrUnlocked),
    Data = [Line1, Line2, Line3, Line4, Line5, Line6, Line7, Line8, Line9],

    ParameterFile = lih_lib:get_parameter_file_path(),
    ok = filelib:ensure_dir(ParameterFile),
    ok = file:write_file(ParameterFile, Data),

    S#state{parameter_file_md5 = get_md5(ParameterFile)}.

convert_from(lkf_installed, false) ->
    {"1\n", "0\n"};
convert_from(lkf_installed, {true, Installed}) ->
    {"0\n", now_to_time_t(Installed, string)};

convert_from(seq_no, SeqNo) -> integer_to_list(SeqNo) ++ [$\n];

convert_from(emergency, never_used) ->
    {"1\n", "0\n"};
convert_from(emergency, {active, Activation})->
    {"2\n", now_to_time_t(Activation, string)};
convert_from(emergency, {degraded, Activation})->
    {"3\n", now_to_time_t(Activation, string)};
convert_from(emergency, {active_again, Activation}) ->
    {"4\n", now_to_time_t(Activation, string)};
convert_from(emergency, {disabled, Activation}) ->
    {"5\n", now_to_time_t(Activation, string)};

convert_from(Type, false) when Type =:= prod_unlocked;
			       Type =:= integr_unlocked ->
    {"0\n", "1\n"};
convert_from(Type, {false, Unlocked}) when Type =:= prod_unlocked;
					   Type =:= integr_unlocked ->
    {now_to_time_t(Unlocked, string), "1\n"};
convert_from(Type, {true, Unlocked}) when Type =:= prod_unlocked;
					  Type =:= integr_unlocked ->
    {now_to_time_t(Unlocked, string), "0\n"}.

now_to_time_t({MegaSec, Sec, _}, integer) ->
    1000000*MegaSec+Sec;
now_to_time_t(Now, string) ->
    integer_to_list(now_to_time_t(Now, integer)) ++ [$\n].

trigger_parameter_file_check() ->
    undefined.
    %% R5A/1 - Remove update of parameters file.
    %% LIH interface (LICI) is still used but no parameter file.
    %% Parameter update is therefor removed to save CPU cycles.
    %% send_after(timer:seconds(1), check_parameter_file).

trigger_lkf_file_check(#state{lkf_file_timer = OldTimerRef}) ->
    cancel_timer(OldTimerRef),

    Seconds = lih_lib:seconds_to_midnight(erlang:universaltime()),
    send_after(timer:seconds(Seconds), check_lkf_file).

add_feature_subscription({FeatureId, _Pid},
			 #state{sub_counter = Cnt}) when Cnt >= ?MAX_NR_OF_SUBSCRIPTIONS ->
    %% Too many subscriptions. Reject..
    {false, FeatureId, ?CELLO_LICI_UNEXPECTED_ERROR};
add_feature_subscription({FeatureId, Pid}, S) ->
    Id = {featureKey, FeatureId},
    case ets:lookup(lici_subscription, Id) of
	[] ->
	    {FeatureStatus, ChangeReason} = get_feature_data(FeatureId, S),
	    Obj = #lici_subscription{id             = Id,
				     pid_list       = [Pid],
				     feature_status = FeatureStatus,
				     change_reason  = ChangeReason},
	    true = ets:insert(lici_subscription, Obj),
	    {true, FeatureId, FeatureStatus, ChangeReason};
	[#lici_subscription{pid_list = PidList} = Obj] ->
	    %% A subscription already exist
	    IsAlreadySubscribed = lists:member(Pid, PidList),
	    TooManySubscriptions = (length(PidList) >= ?MAX_NR_OF_CLIENTS_PER_KEY),

	    if
		IsAlreadySubscribed ->
		    {false, FeatureId, ?CELLO_LICI_ALREADY_SUBSCRIBED};
		TooManySubscriptions ->
		    {false, FeatureId, ?CELLO_LICI_UNEXPECTED_ERROR};
		true ->
		    NewObj = Obj#lici_subscription{pid_list = [Pid|PidList]},
		    true = ets:insert(lici_subscription, NewObj),

		    {true, FeatureId,
		     Obj#lici_subscription.feature_status,
		     Obj#lici_subscription.change_reason}
	    end
    end.

remove_feature_subscription({FeatureId, Pid}) ->
    Id = {featureKey, FeatureId},
    case ets:lookup(lici_subscription, Id) of
	[] ->
	    false;
	[#lici_subscription{pid_list = [Pid]}] ->
	    ets:delete(lici_subscription, Id),
	    true;
	[#lici_subscription{pid_list = PidList} = Obj] ->
	    case lists:member(Pid, PidList) of
		false ->
		    false;
		true ->
		    NewPid = PidList -- [Pid],
		    NewObj = Obj#lici_subscription{pid_list = NewPid},
		    true = ets:insert(lici_subscription, NewObj)
	    end
    end.

add_capacity_subscription({CapacityId, _Pid},
			  #state{sub_counter = Cnt}) when Cnt >= ?MAX_NR_OF_SUBSCRIPTIONS ->
    %% Too many subscriptions. Reject..
    {false, CapacityId, ?CELLO_LICI_UNEXPECTED_ERROR};
add_capacity_subscription({CapacityId, Pid}, S) ->
    Id = {capacityKey, CapacityId},
    case ets:lookup(lici_subscription, Id) of
	[] ->
	    %% First subscription
	    {LicensedLevel, HardLimit, ChangeReason} =
		get_capacity_data(CapacityId, S),
	    Obj = #lici_subscription{id             = Id,
				     pid_list       = [Pid],
				     licensed_level = LicensedLevel,
				     hard_limit     = HardLimit,
				     change_reason  = ChangeReason},
	    true = ets:insert(lici_subscription, Obj),
	    {true, CapacityId, LicensedLevel, HardLimit, ChangeReason};
	[#lici_subscription{pid_list = PidList} = Obj] ->
	    %% A subscription already exist
	    IsAlreadySubscribed = lists:member(Pid, PidList),
	    TooManySubscriptions = (length(PidList) >= ?MAX_NR_OF_CLIENTS_PER_KEY),

	    if
		IsAlreadySubscribed ->
		    {false, CapacityId, ?CELLO_LICI_ALREADY_SUBSCRIBED};
		TooManySubscriptions ->
		    {false, CapacityId, ?CELLO_LICI_UNEXPECTED_ERROR};
		true ->
		    NewObj = Obj#lici_subscription{pid_list = [Pid|PidList]},
		    true = ets:insert(lici_subscription, NewObj),

		    {true, CapacityId,
		     Obj#lici_subscription.licensed_level,
		     Obj#lici_subscription.hard_limit,
		     Obj#lici_subscription.change_reason}
	    end
    end.

%% Check if this clause should exist. Not included in 4/1553-CXA 110 1361 Uen
%% add_status_subscription(_Pid,
%% 			#state{sub_counter = Cnt}) when Cnt >= ?MAX_NR_OF_SUBSCRIPTIONS ->
%%     %% Too many subscriptions. Reject..
%%     {false, ?CELLO_LICI_UNEXPECTED_ERROR};
add_status_subscription(Pid, S) ->
    case ets:lookup(lici_subscription, status) of
	[] ->
	    %% First subscription
	    {LicenseMgrStatus, EmergencyCounter} = get_status_data(S),
	    Obj = #lici_subscription{id            = status,
				     pid_list      = [Pid],
				     mgr_status    = LicenseMgrStatus,
				     emergency_cnt = EmergencyCounter},
	    true = ets:insert(lici_subscription, Obj),
	    {true, LicenseMgrStatus, EmergencyCounter};
	[#lici_subscription{pid_list = PidList} = Obj] ->
	    %% A subscription already exist
	    IsAlreadySubscribed = lists:member(Pid, PidList),
	    if
		IsAlreadySubscribed ->
		    {false, ?CELLO_LICI_ALREADY_SUBSCRIBED};
		true ->
		    NewObj = Obj#lici_subscription{pid_list = [Pid|PidList]},
		    true = ets:insert(lici_subscription, NewObj),

		    {true,
		     Obj#lici_subscription.mgr_status,
		     Obj#lici_subscription.emergency_cnt}
	    end
    end.

get_feature_data(_FeatureId, #state{prod_unlocked = {true, _}} = S) ->
    {?CELLO_LICI_FEATURE_ENABLED,
     get_change_reason(?CELLO_LICI_LICENSED_VALUE, S)};
get_feature_data(_FeatureId, #state{integr_unlocked = {true, _}} = S) ->
    {?CELLO_LICI_FEATURE_ENABLED,
     get_change_reason(?CELLO_LICI_LICENSED_VALUE, S)};
get_feature_data(FeatureId, S) ->
    LicenseList = ets:lookup(lici_licensekey, FeatureId),
    Date = date(),
    Acc = {?CELLO_LICI_FEATURE_DISABLED, ?CELLO_LICI_NOT_ACTIVATED},
    %% Check for the most generous license
    get_feature_data(LicenseList, Date, S, Acc).

get_feature_data([], _Date, S, {FeatureStatus, ChangeReason}) ->
    {FeatureStatus, get_change_reason(ChangeReason, S)};
get_feature_data([License|R], Date, S, _Acc) ->
    case is_license_valid(Date, License) of
	true ->
	    %% Found a valid license. No more checks are needed.
	    {?CELLO_LICI_FEATURE_ENABLED,
	     get_change_reason(?CELLO_LICI_LICENSED_VALUE, S)};
	false ->
	    get_feature_data(R, Date, S, {?CELLO_LICI_FEATURE_DISABLED,
					  ?CELLO_LICI_LICENSED_VALUE})
    end.

get_capacity_data(_CapacityId,  #state{prod_unlocked = {true, _}} = S) ->
    {?LICI_NO_LIMIT, ?LICI_NO_LIMIT,
     get_change_reason(?CELLO_LICI_LICENSED_VALUE, S)};
get_capacity_data(_CapacityId,  #state{integr_unlocked = {true, _}} = S) ->
    {?LICI_NO_LIMIT, ?LICI_NO_LIMIT,
     get_change_reason(?CELLO_LICI_LICENSED_VALUE, S)};
get_capacity_data(CapacityId, S) ->
    LicenseList = ets:lookup(lici_licensekey, CapacityId),
    Date = date(),
    Acc = {?LICI_LIMIT(0), ?LICI_LIMIT(0), ?CELLO_LICI_NOT_ACTIVATED},

    get_capacity_data(LicenseList, Date, S, Acc).

get_capacity_data([], _Date, S,
		  {LicensedLevel, HardLimit, ChangeReason}) ->
    {LicensedLevel, HardLimit, get_change_reason(ChangeReason, S)};
get_capacity_data(_LicenseList, _Date, S,
		  {?LICI_NO_LIMIT, ?LICI_NO_LIMIT, ChangeReason}) ->
    {?LICI_NO_LIMIT, ?LICI_NO_LIMIT, get_change_reason(ChangeReason, S)};
get_capacity_data([License|R], Date, S,
		  {LicensedLevel, HardLimit, _ChangeReason}) ->

    NewAcc =
	case is_license_valid(Date, License) of
	    true ->
		{max(License#lici_license_key.capacity, LicensedLevel),
		 max(License#lici_license_key.hard_limit, HardLimit),
		 ?CELLO_LICI_LICENSED_VALUE};
	    false ->
		{LicensedLevel, HardLimit, ?CELLO_LICI_LICENSED_VALUE}
	end,
    get_capacity_data(R, Date, S, NewAcc).

get_status_data(#state{emergency = never_used}) ->
    {?CELLO_LICI_EMERGENCY_DEACTIVATED, ?CELLO_LICI_NO_EMERGENCY};
get_status_data(#state{emergency = {active, _}}) ->
    {?CELLO_LICI_EMERGENCY_ACTIVATED,   ?CELLO_LICI_EMERGENCY_ONCE};
get_status_data(#state{emergency = {degraded, _}}) ->
    {?CELLO_LICI_EMERGENCY_DEACTIVATED, ?CELLO_LICI_EMERGENCY_ONCE};
get_status_data(#state{emergency = {active_again, _}}) ->
    {?CELLO_LICI_EMERGENCY_ACTIVATED,   ?CELLO_LICI_EMERGENCY_TWICE};
get_status_data(#state{emergency = {disabled, _}}) ->
    {?CELLO_LICI_EMERGENCY_DEACTIVATED, ?CELLO_LICI_EMERGENCY_TWICE}.

get_subscriptions() ->
    ets:tab2list(lici_subscription).

remove_subscriptions(Pid, SubCounter) ->
    Fun =
	fun(#lici_subscription{pid_list = []}, Subs) ->
		Subs;
	   (#lici_subscription{id       = Id,
			       pid_list = PidList} = Obj, Subs) ->
		case PidList -- [Pid] of
		    [] ->
			%% Last subscriber has been deleted
			delete_unused_subscription(Id),
			Subs-1;
		    PidList ->
			%% No changes. Do nothing
			Subs;
		    NewPidList ->
			%% Update
			NewObj = Obj#lici_subscription{pid_list = NewPidList},
			true = ets:insert(lici_subscription, NewObj),
			Subs-1
		end
	end,
    lists:foldl(Fun, SubCounter, get_subscriptions()).

check_subscriptions(S) ->
    lists:foreach(
      fun(Subscription) ->
	      check_subscription(Subscription, S)
      end, get_subscriptions()).

check_subscription(#lici_subscription{id = status} = Subscription, S) ->
    {LicMgrStatus, EmergencyCounter} = get_status_data(S),
    case Subscription of
	#lici_subscription{mgr_status    = LicMgrStatus,
			   emergency_cnt = EmergencyCounter} ->
	    %% No changes
	    true;
	#lici_subscription{pid_list = PidList} = Obj ->
	    %% Changes found
	    %% Call the providers and update the ETS table
	    %% error_logger:info_report(["License Manager Status changed",
	    %% 			      {licenseManagerStatus, LicMgrStatus},
	    %% 			      {emergencyCounter, EmergencyCounter}]),
	    lih_lici_worker:status_changed(PidList,
					   {LicMgrStatus, EmergencyCounter}),
	    NewObj = Obj#lici_subscription{mgr_status    = LicMgrStatus,
					   emergency_cnt = EmergencyCounter},
	    true = ets:insert(lici_subscription, NewObj)
    end;
check_subscription(#lici_subscription{id = {featureKey, FeatureId}} = Subscription,
		   S) ->
    {FeatureStatus, ChangeReason} = get_feature_data(FeatureId, S),
    case Subscription of
	#lici_subscription{feature_status = FeatureStatus,
			   change_reason  = ChangeReason} ->
	    %% No changes
	    true;
	#lici_subscription{pid_list = PidList} = Obj ->
	    %% Changes found
	    %% Call the providers and update the ETS table
	    %% error_logger:info_report(["Feature changed",
	    %% 			      {featureId, FeatureId},
	    %% 			      {featureStatus, FeatureStatus},
	    %% 			      {changeReason, ChangeReason}]),
	    lih_lici_worker:feature_changed(PidList,
					    {FeatureId, FeatureStatus,
					     ChangeReason}),
	    NewObj = Obj#lici_subscription{feature_status = FeatureStatus,
					   change_reason  = ChangeReason},
	    true = ets:insert(lici_subscription, NewObj)
    end;
check_subscription(#lici_subscription{id = {capacityKey, CapacityId}} = Subscription,
		   S) ->
    {LicensedLevel, HardLimit, ChangeReason} = get_capacity_data(CapacityId, S),
    case Subscription of
	#lici_subscription{licensed_level = LicensedLevel,
			   hard_limit     = HardLimit,
			   change_reason  = ChangeReason} ->
	    %% No changes
	    true;
	#lici_subscription{pid_list = PidList} = Obj ->
	    %% Changes found
	    %% Call the providers and update the ETS table
	    %% error_logger:info_report(["Capacity changed",
	    %% 			      {capacityId, CapacityId},
	    %% 			      {licensedLevel, LicensedLevel},
	    %% 			      {hardLimit, HardLimit},
	    %% 			      {changeReason, ChangeReason}]),
	    lih_lici_worker:capacity_changed(PidList,
					     {CapacityId, LicensedLevel,
					      HardLimit, ChangeReason}),
	    NewObj = Obj#lici_subscription{licensed_level = LicensedLevel,
					   hard_limit     = HardLimit,
					   change_reason  = ChangeReason},
	    true = ets:insert(lici_subscription, NewObj)
    end.

get_providers() ->
    ets:tab2list(lici_provider).

delete_providers(Pid) ->
    true = ets:match_delete(lici_provider,
			    #lici_provider{provider_pid = Pid, _ = '_'}).

get_license_keys() ->
    ets:tab2list(lici_licensekey).

is_license_valid(Date, #lici_license_key{start = StartDate,
					 stop  = StopDate}) ->
    (Date >= StartDate) andalso (StopDate =:= infinity orelse Date =< StopDate).

get_unlocked_status(Type, ActivationTS) ->
    {get_timeleft({Type, ActivationTS})>0, ActivationTS}.

get_emergency_status(never_used) -> never_used;
get_emergency_status({degraded, Ts}) -> {degraded, Ts};
get_emergency_status({disabled, Ts}) -> {disabled, Ts};
get_emergency_status({active, Ts}) ->
    case get_timeleft({emergency, Ts}) of
	0 -> {degraded, get_timestamp()};
	_ -> {active, Ts}
    end;
get_emergency_status({active_again, Ts}) ->
    case get_timeleft({emergency, Ts}) of
	0 -> {disabled,  get_timestamp()};
	_ -> {active_again, Ts}
    end.


get_change_reason(_ChangeReason, #state{emergency = {active, _}}) ->
    ?CELLO_LICI_EMERGENCY_REASON;
get_change_reason(_ChangeReason, #state{emergency = {active_again, _}}) ->
    ?CELLO_LICI_EMERGENCY_REASON;
get_change_reason(ChangeReason, _S) ->
    ChangeReason.

store_license_keys(LicenseKeys) ->
    ets:delete_all_objects(lici_licensekey),
    ets:insert(lici_licensekey, LicenseKeys),
    create_subscriptions().

fetch_LKF({User, Password, IpAddress, RemoteFile}, LocalFile) ->
    case lih_fetch:ftp({IpAddress, User, Password, RemoteFile}, LocalFile) of
	ok ->
	    ok;
	{error, epath} ->
	    error;
	{error, _Reason} ->
	    case lih_fetch:sftp({IpAddress, User, Password, RemoteFile},
				LocalFile) of
		ok ->
		    ok;
		{error, _} ->
		    error
	    end
    end.

verify_LKF(Filename) ->
    case lih_verify:verifyLicenseFile(Filename) of
	ok ->
	    ok;
	{error, Reason} ->
	    throw({error, verifyLKF, Reason})
    end.

-spec check_reset_emergency_counter() -> boolean().

check_reset_emergency_counter() ->
    LicenseKeys = get_license_keys(),
    Date = date(),
    lists:any(
      fun(#lici_license_key{emergency_reset = true} = License) ->
	      true = ets:delete(lici_licensekey, emergencyReset),
	      is_license_valid(Date, License);
	 (#lici_license_key{emergency_reset = false}) ->
	      false
      end, LicenseKeys).

-spec check_license_file(Filename :: string(),
			 State :: #state{}) -> {boolean(), #lici_lkf{}}.

check_license_file(Filename, #state{seq_no       = SeqNo,
				    finger_print = FingerPrint}) ->
    try
	verify_LKF(Filename),
	lih_xml:convert(Filename)
    of
	{ok, #lici_lkf{format_version = FormatVersion}} when FormatVersion =/= "2.0" ->
	    %% error_logger:info_msg("Checking LKF ~p~n"
	    %% 			  "Unsupported format version.~n", [Filename]),
	    {false, #lici_lkf{status = faulty}};
	{ok, #lici_lkf{signature_type = SignatureType}} when SignatureType =/= 5 ->
	    %% error_logger:info_msg("Checking LKF ~p~n"
	    %% 			  "Faulty signature type (~p) in the license key file.~n",
	    %% 			  [Filename, SignatureType]),
	    {false, #lici_lkf{status = faulty}};
	{ok, LKF} when SeqNo > LKF#lici_lkf.seq_no ->
	    %% error_logger:info_msg("Checking LKF ~p~n"
	    %% 			  "Faulty sequence number in the license key file.~n",
	    %% 			  [Filename]),
	    {false, #lici_lkf{status = faulty}};
	{ok, #lici_lkf{method = Method}} when Method =/= 1,
					      Method =/= 11 ->
	    %% error_logger:info_msg("Checking LKF ~p~n"
	    %% 			  "Fingerprint method ~p is not supported.~n",
	    %% 			  [Filename, Method]),
	    {false, #lici_lkf{status = faulty}};
	{ok, #lici_lkf{print = Print}} when FingerPrint =/= Print ->
	    %% error_logger:info_msg("Checking LKF ~p~n"
	    %% 			  "Faulty fingerprint in the license key file.~n",
	    %% 			  [Filename]),
	    {false, #lici_lkf{status = faulty}};
	{ok, #lici_lkf{license_keys = LicenseKeys}} when length(LicenseKeys) > ?MAX_NR_OF_KEYS ->
	    %% error_logger:info_msg("Checking LKF ~p~n"
	    %% 			  "Max number of keys (~p) exceeded.~n",
	    %% 			  [Filename, ?MAX_NR_OF_KEYS]),
	    {false, #lici_lkf{status = faulty}};
	{ok, #lici_lkf{license_keys = LicenseKeys} = LKF} ->
	    check_duplicate_license_keys(LicenseKeys),
	    NewLicenseKeys =
		case LKF#lici_lkf.seq_no of
		    SeqNo ->
			%% This LKF has the same sequence number as the last used LKF
			%% Remove all emergencyReset
			lists:filter(
			  fun(#lici_license_key{emergency_reset = EmergencyReset}) ->
				  not EmergencyReset
			  end, LicenseKeys);
		    _ ->
			LicenseKeys
		end,
	    %% error_logger:info_msg("Checking LKF ~p~n", [Filename]),
	    {true, LKF#lici_lkf{license_keys = NewLicenseKeys}}
    catch
	throw:{error, verifyLKF, liciMissingFile} ->
	    %% error_logger:info_msg("Missing LKF ~p~n", [Filename]),
	    {false, #lici_lkf{status = missing}};
	throw:{error, verifyLKF, liciBadFile} ->
	    %% error_logger:info_msg("Checking LKF ~p~n"
	    %% 			  "Incorrect format~n", [Filename]),
	    {false, #lici_lkf{status = faulty}};
	throw:{error, verifyLKF, liciVfyFailed} ->
	    %% error_logger:info_msg("Checking LKF ~p~n"
	    %% 			  "Verifying LKF failed~n", [Filename]),
	    {false, #lici_lkf{status = faulty}};
	_A:_B ->
	    %% error_logger:info_msg("Checking LKF ~p~n"
	    %% 			  "Unknown error~n"
	    %% 			  "~p:~p~n", [Filename, A, B]),
	    {false, #lici_lkf{status = faulty}}
    end.

update_after_new_LKF(LKF, S, Updating) ->
    Status = LKF#lici_lkf.status,
    %% error_logger:info_msg("The LKF has been read.~n"
    %% 			  "Status: ~p~n", [Status]),
    store_license_keys(LKF#lici_lkf.license_keys),

    NewEmergency =
	case check_reset_emergency_counter() of
	    true  ->
		%% A valid emergencyResetKey has been found
		%% Reset the emergency counter
		%% Cancel emergency unlock related alarms
		%% lih_fm:cancel_emergency_unlock_alarm(),
		%% lih_fm:cancel_emergency_unlock_expired_alarm(),

		never_used;
	    false ->
		%% No change
		S#state.emergency
	end,

    LKFInstalled =
	if
	    Status =/= ok ->
		false;
	    Updating;
	    S#state.lkf_installed =:= false ->
		{true, get_timestamp()};
	    true ->
		S#state.lkf_installed
	end,

    ProdUnlocked = update_unlocked_status(S#state.prod_unlocked, LKFInstalled),
    IntUnlocked = update_unlocked_status(S#state.integr_unlocked, LKFInstalled),

    LkfData =
	if
	    Status =/= ok ->
		[{status, Status}];
	    true ->
		[{status, Status},
		 {format_version, LKF#lici_lkf.format_version},
		 {seq_no, LKF#lici_lkf.seq_no},
		 {finger_print, LKF#lici_lkf.print},
		 {signature_type, LKF#lici_lkf.signature_type},
		 {swlt_id, LKF#lici_lkf.swlt_id},
		 {product_type, LKF#lici_lkf.product_type},
		 {customer_id, LKF#lici_lkf.customer_id}]
	end,

    State0 = S#state{lkf_installed   = LKFInstalled,
		     seq_no          = LKF#lici_lkf.seq_no,
		     emergency       = NewEmergency,
		     prod_unlocked   = ProdUnlocked,
		     integr_unlocked = IntUnlocked,
		     lkf_file_timer  = trigger_lkf_file_check(S),
		     lkf             = LkfData},

    check_subscriptions(State0),
    delete_unused_subscriptions(),

    case Status of
	ok ->
	    %% lih_fm:cancel_LKF_fault_alarm(),
	    %% lih_fm:cancel_LKF_missing_alarm(),
	    ok;
	missing ->
	    %% lih_fm:request_LKF_missing_alarm(),
	    %% lih_fm:cancel_LKF_fault_alarm(),
	    ok;
	faulty ->
	    %% lih_fm:request_LKF_fault_alarm(),
	    %% lih_fm:cancel_LKF_missing_alarm(),
	    ok
    end,

    write_stored_values(State0).

create_subscriptions() ->
    lists:foreach(
      fun(#lici_license_key{id = emergencyReset}) ->
	      true;
	 (#lici_license_key{id = Id}) ->
	      case ets:lookup(lici_subscription, Id) of
		  [] ->
		      Obj = #lici_subscription{id = Id},
		      true = ets:insert(lici_subscription, Obj);
		  _ ->
		      true
	      end
      end, get_license_keys()).

delete_unused_subscriptions() ->
    lists:foreach(
      fun(#lici_subscription{id = Id, pid_list = []}) ->
	      %% There is no subscriptions
	      %% Check if there are any references in the LKF
	      delete_unused_subscription(Id);
	 (_) ->
	      ok
      end, get_subscriptions()).

delete_unused_subscription(Id) ->
    case ets:lookup(lici_licensekey, Id) of
	[] ->
	    %% There is no reference in the LKF
	    %% Delete it.
	    ets:delete(lici_subscription, Id);
	_ ->
	    %% There are at least one reference in the LKF
	    %% Keep it
	    ok
    end.

check_duplicate_license_keys([]) ->
    ok;
check_duplicate_license_keys([LicenseKey|T]) ->
    NewT = check_duplicate_license_keys(LicenseKey, T, [], 0),
    check_duplicate_license_keys(NewT).

check_duplicate_license_keys(_LicenseKey, [], Acc, 0) ->
    Acc;
check_duplicate_license_keys(#lici_license_key{id = {_Type, _Key}}, [], Acc,
			     _Cnt) ->
    %% error_logger:info_msg("~p ~s has multiple (~p) keys.~n",
    %% 			  [Type, Key, Cnt+1]),
    Acc;
check_duplicate_license_keys(#lici_license_key{id = Id} = LicenseKey,
			     [#lici_license_key{id = Id}|T], Acc, Cnt) ->
    %% A duplicate is found. Increase counter and remove the duplicate
    check_duplicate_license_keys(LicenseKey, T, Acc, Cnt+1);
check_duplicate_license_keys(LicenseKey, [H|T], Acc, Cnt) ->
    check_duplicate_license_keys(LicenseKey, T, [H|Acc], Cnt).

get_md5(File) ->
    case file:read_file(File) of
	{ok, Binary} ->
	    erlang:md5(Binary);
	{error, _Reason} ->
	    undefined
    end.

cancel_timer(undefined) -> ok;
cancel_timer(TRef)      -> timer:cancel(TRef), ok.

send_after(undefined, _Msg) -> undefined;
send_after(Time,       Msg) -> {ok, TRef} = timer:send_after(Time, Msg), TRef.

trigger_unlock_timer(S) ->
    cancel_timer(S#state.unlock_timer),
    Timeout = get_unlock_timeout(S),
    send_after(Timeout, check_unlock).

handle_get_info(S) ->
    Reply = [
	     {itc_port,             S#state.itc_port},
	     {providers,            get_providers()},
	     {subscriptions,        get_subscriptions()},
	     {license_keys,         get_license_keys()},
	     {lkf_installed,        S#state.lkf_installed},
	     {seq_no,               S#state.seq_no},
	     {finger_print,         S#state.finger_print},
	     {emergency,            S#state.emergency},
	     {prod_unlocked,        S#state.prod_unlocked},
	     {integr_unlocked,      S#state.integr_unlocked},
	     {parameter_file_md5,   S#state.parameter_file_md5},
	     {parameter_file_timer, S#state.parameter_file_timer},
	     {sub_counter,          S#state.sub_counter},
	     {lkf_file_timer,       S#state.lkf_file_timer},
	     {unlock_timer,         S#state.unlock_timer}
	    ],
    {reply, Reply, S}.

handle_get_lkf_info(S) ->
    {reply, S#state.lkf, S}.

handle_get_emergency_state_info(#state{emergency = never_used} = S) ->
    {reply, {?EmergencyStatus_NEVER_USED, 0}, S};
handle_get_emergency_state_info(#state{emergency = {active, TS}} = S) ->
    RemainingTime = get_timeleft({emergency, TS}),
    {reply, {?EmergencyStatus_ACTIVE, RemainingTime}, S};
handle_get_emergency_state_info(#state{emergency = {degraded, _}} = S) ->
    {reply, {?EmergencyStatus_USE_DEGRADED, 0}, S};
handle_get_emergency_state_info(#state{emergency = {active_again, TS}} = S) ->
    RemainingTime = get_timeleft({emergency, TS}),
    {reply, {?EmergencyStatus_ACTIVE_AGAIN, RemainingTime}, S};
handle_get_emergency_state_info(#state{emergency = {disabled, _}} = S) ->
    {reply, {?EmergencyStatus_USE_DISABLED, 0}, S}.

handle_is_LKF_installed(#state{lkf_installed = {true, _}} = S) ->
    {reply, true, S};
handle_is_LKF_installed(#state{lkf_installed = false} = S)     ->
    {reply, false, S}.

handle_get_LKF_installed_rsp(#state{lkf_installed = {true, _}} = S) ->
    {reply, ?CELLO_LICI_LKF_INSTALLED, S};
handle_get_LKF_installed_rsp(#state{lkf_installed = false} = S)     ->
    {reply, ?CELLO_LICI_LKF_NOT_INSTALLED, S}.

handle_update_LKF({UserId, Password, IpAddress, RemoteFile}, S) ->
    TmpLKFPath = lih_lib:get_LKF_path(tmp),
    case fetch_LKF({UserId, Password, IpAddress, RemoteFile}, TmpLKFPath) of
	ok ->
	    case check_license_file(TmpLKFPath, S) of
		{true, LKF} ->
		    LKFPath = lih_lib:get_LKF_path(license),
		    BackupLKFPath = lih_lib:get_LKF_path(backup),
		    file:copy(LKFPath, BackupLKFPath),
		    file:copy(TmpLKFPath, LKFPath),
		    file:copy(LKFPath, BackupLKFPath),
		    ok = file:delete(TmpLKFPath),

		    {reply, ok, update_after_new_LKF(LKF, S, true)};
		{false, _LKF} ->
		    ok = file:delete(TmpLKFPath),

		    {reply, nok, S}
	    end;
	error ->
	    {reply, nok, S}
    end.

handle_set_emergency_state(#state{emergency = never_used} = S) ->
    Reply = ok,
    State0 = S#state{emergency = {active, get_timestamp()}},
    check_subscriptions(State0),
    %% lih_fm:request_emergency_unlock_alarm(),
    TimerRef = trigger_unlock_timer(State0),
    {reply, Reply, write_stored_values(State0#state{unlock_timer = TimerRef})};
handle_set_emergency_state(#state{emergency = {degraded, _}} = S) ->
    Reply = ok,
    State0 = S#state{emergency = {active_again, get_timestamp()}},
    check_subscriptions(State0),
    %% lih_fm:request_emergency_unlock_alarm(),
    %% lih_fm:cancel_emergency_unlock_expired_alarm(),
    TimerRef = trigger_unlock_timer(State0),
    {reply, Reply, write_stored_values(State0#state{unlock_timer = TimerRef})};
handle_set_emergency_state(S) ->
    Reply = nok,
    {reply, Reply, S}.

handle_production_unlock(activate, #state{lkf_installed = false,
					  prod_unlocked = false} = S) ->
    Reply = ok,
    State0 = S#state{prod_unlocked = {true, get_timestamp()}},
    check_subscriptions(State0),
    %% lih_fm:request_production_unlock_alarm(),
    TimerRef = trigger_unlock_timer(State0),
    {reply, Reply, write_stored_values(State0#state{unlock_timer = TimerRef})};
handle_production_unlock(activate, #state{prod_unlocked = {true, _}} = S) ->
    Reply = ok,
    {reply, Reply, S};
handle_production_unlock(activate, #state{lkf_installed = {true, _}} = S) ->
    Reply = {error, "LKF is installed"},
    {reply, Reply, S};
handle_production_unlock(activate, #state{prod_unlocked = {false, TS}} = S) ->
    case get_unlocked_status(prod_unlocked, TS) of
	{false, TS} ->
	    Reply = {error, "Production unlock is not valid anymore"},
	    {reply, Reply, S};
	{true, TS} ->
	    Reply = ok,
	    State0 = S#state{prod_unlocked = {true, TS}},
	    check_subscriptions(State0),
	    %% lih_fm:request_production_unlock_alarm(),
	    TimerRef = trigger_unlock_timer(State0),
	    {reply, Reply,
	     write_stored_values(State0#state{unlock_timer = TimerRef})}
    end;
handle_production_unlock(deactivate, #state{prod_unlocked = {true, TS}} = S) ->
    Reply = ok,
    State0 = S#state{prod_unlocked = {false, TS}},
    check_subscriptions(State0),
    %% lih_fm:cancel_production_unlock_alarm(),
    TimerRef = trigger_unlock_timer(State0),
    {reply, Reply, write_stored_values(State0#state{unlock_timer = TimerRef})};
handle_production_unlock(deactivate, S) ->
    Reply = ok,
    {reply, Reply, S};
handle_production_unlock(status, #state{prod_unlocked = false} = S) ->
    Active = false,
    Used = false,
    TimeLeft = 0,
    Reply = {status, Active, Used, TimeLeft},
    {reply, Reply, S};
handle_production_unlock(status, #state{prod_unlocked = {false, _}} = S) ->
    Active = false,
    Used = true,
    TimeLeft = 0,
    Reply = {status, Active, Used, TimeLeft},
    {reply, Reply, S};
handle_production_unlock(status, #state{prod_unlocked = {true, TS}} = S) ->
    Active = true,
    Used = true,
    TimeLeft = get_timeleft({prod_unlocked, TS}),
    Reply = {status, Active, Used, TimeLeft},
    {reply, Reply, S}.

handle_integration_unlock(activate, #state{lkf_installed   = false,
					   integr_unlocked = false} = S) ->
    Reply = ok,
    State0 = S#state{integr_unlocked = {true, get_timestamp()}},
    check_subscriptions(State0),
    %% lih_fm:request_integration_unlock_alarm(),
    TimerRef = trigger_unlock_timer(State0),
    {reply, Reply, write_stored_values(State0#state{unlock_timer = TimerRef})};
handle_integration_unlock(activate, #state{integr_unlocked = {true, _}} = S) ->
    Reply = ok,
    {reply, Reply, S};
handle_integration_unlock(activate, #state{lkf_installed = {true, _}} = S) ->
    Reply = {error, "LKF is installed"},
    {reply, Reply, S};
handle_integration_unlock(activate,
			  #state{integr_unlocked = {false, TS}} = S) ->
    case get_unlocked_status(integr_unlocked, TS) of
	{false, TS} ->
	    Reply = {error, "Integration unlock is not valid anymore"},
	    {reply, Reply, S};
	{true, TS} ->
	    Reply = ok,
	    State0 = S#state{integr_unlocked = {true, TS}},
	    check_subscriptions(State0),
	    %% lih_fm:request_integration_unlock_alarm(),
	    TimerRef = trigger_unlock_timer(State0),
	    {reply, Reply,
	     write_stored_values(State0#state{unlock_timer = TimerRef})}
    end;
handle_integration_unlock(deactivate,
			  #state{integr_unlocked = {true, TS}} = S) ->
    Reply = ok,
    State0 = S#state{integr_unlocked = {false, TS}},
    check_subscriptions(State0),
    %% lih_fm:cancel_integration_unlock_alarm(),
    TimerRef = trigger_unlock_timer(State0),
    {reply, Reply, write_stored_values(State0#state{unlock_timer = TimerRef})};
handle_integration_unlock(deactivate, S) ->
    Reply = ok,
    {reply, Reply, S};
handle_integration_unlock(status, #state{integr_unlocked = false} = S) ->
    Active = false,
    Used = false,
    TimeLeft = 0,
    Reply = {status, Active, Used, TimeLeft},
    {reply, Reply, S};
handle_integration_unlock(status, #state{integr_unlocked = {false, _}} = S) ->
    Active = false,
    Used = true,
    TimeLeft = 0,
    Reply = {status, Active, Used, TimeLeft},
    {reply, Reply, S};
handle_integration_unlock(status, #state{integr_unlocked = {true, TS}} = S) ->
    Active = true,
    Used = true,
    TimeLeft = get_timeleft({integr_unlocked, TS}),
    Reply = {status, Active, Used, TimeLeft},
    {reply, Reply, S}.

handle_emergency_state_timeout(S) ->
    Reply = ok,

    State0 =
	case S#state.emergency of
	    {active, _} ->
		handle_check_unlock(S#state{emergency = {active, {0,0,0}}});
	    {active_again, _} ->
		handle_check_unlock(S#state{emergency = {active_again, {0,0,0}}});
	    _ ->
		handle_check_unlock(S)
	end,

    {reply, Reply, write_stored_values(State0)}.

handle_get_last_licensing_change(S) ->
    TS0 =
	case S#state.lkf_installed of
	    false      -> {0, 0, 0};
	    {true, TS} -> TS
	end,

    {{Year, Month, Day}, {Hour, Min, Sec}} =calendar:now_to_local_time(TS0),
    Reply = lists:flatten(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w",
					[Year, Month, Day, Hour, Min, Sec])),
    {reply, Reply, S}.

handle_get_license_file_url(#state{lkf_installed = false} = S)       ->
    {reply, "", S};
handle_get_license_file_url(#state{lkf_installed = {'true', _}} = S) ->
    %% FIXME peno
    {reply, "http://localhost/tmp/q.xml", S}.

get_timeleft({prod_unlocked, TS})   ->
    get_remaining_time(?SECONDS_IN_7DAYS, TS);
get_timeleft({integr_unlocked, TS}) ->
    get_remaining_time(?SECONDS_IN_50YEARS, TS);
%% get_timeleft({integr_unlocked, TS}) ->
%%     get_remaining_time(?SECONDS_IN_21DAYS, TS);
get_timeleft({emergency, TS})       ->
    get_remaining_time(?SECONDS_IN_7DAYS, TS).

get_remaining_time(Offset, TS) ->
    SinceTS = timer:now_diff(get_timestamp(), TS) div 1000000,

    if
	Offset > SinceTS -> Offset - SinceTS;
	true             -> 0
    end.

get_timestamp() ->
    %% Don't use microseconds in os:timestamp()
    setelement(3, os:timestamp(), 0).

update_unlocked_status({true, TS}, {true, _}) -> {false, TS};
update_unlocked_status(OldUnlocked, _)        -> OldUnlocked.

get_unlock_timeout(S) ->
    ProdUnlockTimeout =
	case S#state.prod_unlocked of
	    {true, ProdTS} -> get_timeleft({prod_unlocked, ProdTS});
	    _              -> undefined
	end,

    IntUnlockTimeout =
	case S#state.integr_unlocked of
	    {true, IntTS} -> get_timeleft({integr_unlocked, IntTS});
	    _             -> undefined
	end,

    EmergUnlockTimeout =
	case S#state.emergency of
	    {active, EmergTS}       -> get_timeleft({emergency, EmergTS});
	    {active_again, EmergTS} -> get_timeleft({emergency, EmergTS});
	    _                       -> undefined
	end,

    %% Using the fact that the atom 'undefined' is greater than any integer
    %% in erlang
    case lists:min([ProdUnlockTimeout, IntUnlockTimeout, EmergUnlockTimeout]) of
	undefined ->
	    undefined;
	Timeout ->
	    timer:seconds(Timeout)
    end.

handle_check_unlock(#state{emergency       = OldEmergency,
			   prod_unlocked   = OldProdUnlocked,
			   integr_unlocked = OldIntUnlocked} = S) ->

    Emergency = get_emergency_status(OldEmergency),
    case Emergency of
	OldEmergency -> ok;
	_ ->
	    %% lih_fm:request_emergency_unlock_expired_alarm(),
	    %% lih_fm:cancel_emergency_unlock_alarm(),
	    ok
    end,

    ProdUnlocked =
	case OldProdUnlocked of
	    {true, ProdTs}  -> get_unlocked_status(prod_unlocked, ProdTs);
	    OldProdUnlocked -> OldProdUnlocked
	end,

    case ProdUnlocked of
	OldProdUnlocked -> ok;
	_ ->
	    %% lih_fm:request_production_unlock_expired_alarm(),
	    %% lih_fm:cancel_production_unlock_alarm(),
	    ok
    end,

    IntUnlocked =
	case OldIntUnlocked of
	    {true, IntTs} -> get_unlocked_status(integr_unlocked, IntTs);
	    OldIntUnlocked -> OldIntUnlocked
	end,

    case IntUnlocked of
	OldIntUnlocked -> ok;
	_ ->
	    %% lih_fm:request_integration_unlock_expired_alarm(),
	    %% lih_fm:cancel_integration_unlock_alarm(),
	    ok
    end,

    State0 = S#state{emergency       = Emergency,
		     prod_unlocked   = ProdUnlocked,
		     integr_unlocked = IntUnlocked},
    check_subscriptions(State0),
    TimerRef = trigger_unlock_timer(State0),

    State0#state{unlock_timer = TimerRef}.

is_local_restart() ->
    {Started, _Pid} = start_lici_checker(true),
    Started.

check_lici_checker() ->
    start_lici_checker(false),
    ok.

start_lici_checker(Monitor) ->
    case whereis(lici_checker) of
	undefined ->
	    {Pid, _Ref} =
		spawn_monitor(
		  fun() -> register(lici_checker, self()), lici_checker() end),
	    {false, Pid};
	Pid when Monitor ->
	    monitor(process, Pid),
	    {true, Pid};
	Pid ->
	    {true, Pid}
    end.

lici_checker() ->
    receive _ -> lici_checker() end.

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
