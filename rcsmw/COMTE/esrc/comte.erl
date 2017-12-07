%%%-------------------------------------------------------------------
%%% @author Lukas Larsson <lukas@erlang-solutions.com>
%%% @doc Interface module for ComtE (COM to Erlang).
%%%
%%% <p>Regarding Application Environment Variables:
%%% In the text below `{Key}' refers to an application environment variable
%%% of the name `Key'.</p>
%%%
%%% @end
%%% Created : 18 Mar 2011 by Lukas Larsson <lukas@erlang-solutions.com>
%%%-------------------------------------------------------------------

%%
%% Revision History
%%
%% Date: 20150508
%% Name: uabhten, erasipe
%% Description: OTP-11851. 
%% From Erlang 18, predefined type tuple/N, N>0,  no longer exists
%% instead the userdefined types alarm_add_info 
%% must be defined as normal tuple {}.

-module(comte).

%% API
-export([
         register_callback/2,
         register_role_callback/1,
	 register_replicated_list_callback/1,
         register_transaction_server_callback/1,
         register_crypto_callback/1,
	 register_log_write_callback/1,
         register_object_implementer_reg_callback/1,
         register_cm_event_producer_callback/1
        ]).

-export([start_link/1, stop/0,
	 start_com/0, start_com/1,
         stop_com/0, stop_com/1,
	 kill_com/0, kill_com/1]).

-export([send_alarm/5]).
-export([notify/1]).
-export([pm_gp_ready/1]).
-export([log/6]).
-export([setHaMode/3, healthCheck/0, healthCheck/1]).
-export([info/0]).
-export([updateIpVersion/1]).


-include("comte_fm.hrl").
-include("comte_ac.hrl").
-include("comte_event.hrl").
-include("comte_log.hrl").

%% Application internal
-export([app/0,
         get_env/1, get_env/2, get_all_env/0,
         set_env/2, template_dir/0]).

%% @private
app() ->
    comte.

-type file_path() :: string().
%% A file path.  May be relative.

-type abs_file_path() :: string().
%% An absolute file path.

-type app_env() ::
	{ start_com, boolean() } |
	{ start_com_prog, file_path() } |
	{ start_com_timeout, non_neg_integer() } |
        { comte_com_timeout, non_neg_integer() } |
	{ com_initial_ha_mode, 'ComMwSpiHaMode'() | mfa() } |
	{ com_initial_ha_mode_timeout, non_neg_integer() } |
	{ comte_port, non_neg_integer() } |
	{ com_port, non_neg_integer() } |
	{ comte_ip, string() | inet:ip_address() } |
	{ com_ip, string() | inet:ip_address() } |
	{ com_start_mw, boolean() } |
	{ callback_registrar, abs_file_path() } |
        { comte_cb_key, module() | {module(),list()} } |
        { healthcheck_interval, non_neg_integer() } |
	{ healthcheck_timeout, non_neg_integer() }.
%% Application environment variables which can be set.
%%
%% <dl>
%%    <dt><b>`start_com'</b></dt>
%%    <dd>Automatically start COM when ComtE starts.
%%       Default: `false'.
%%    </dd>
%%    <dt><b>`start_com_prog'</b></dt>
%%    <dd>The program (executable (script)) to be used to start COM.
%%       If not an absolute path it is regarded as relative to
%%       `{template_dir}'.
%%       The config function {@link comte_com_config:install/1}
%%       generates a suitable script and returns a list of environment
%%       variables to set, in particular this one.
%%       Default: `"start_com.example"' which is used as
%%       a template for {@link comte_com_config:install/1}
%%       to tweak.
%%       The program is supposed to start COM in the same
%%       process i.e finish with an 'exec' of COM. All printout
%%       from the program is ignored by ComtE so any logging of
%%       COM printout has to be set up by the script. The start
%%       program is also supposed to kill off old identical COM
%%       instances through any suitable OS features such as 'pgrep'
%%       before starting COM.
%%    </dd>
%%    <dt><b>`start_com_timeout'</b></dt>
%%    <dd>The default timeout for startup of COM
%%       Default: `10000'.
%%    </dd>
%%    <dt><b>`comte_com_timeout'</b></dt>
%%    <dd>The default timeout for the communication with COM over the socket.
%%        This applies for calls like sending an alarm or a notification.
%%       Default: `5000'.
%%    </dd>
%%    <dt><b>`template_dir'</b></dt>
%%    <dd>Directory where to find e.g "start_com.example" and other
%%      template files used by {@link comte_com_config:install/1}.
%%      If this is not an absolute path it is regareded as
%%      relative to `code:priv_dir' for the ComtE application
%%      and this is also the default.
%%    </dd>
%%    <dt><b>`com_initial_ha_mode'</b></dt>
%%    <dd>The initial HA mode for COM to be set to when started.
%%        Default: `?ComMwSpiHaModeUnassigned'.
%%    </dd>
%%    <dt><b>`comte_port'</b></dt>
%%    <dd>The TCP port used form COM-&gt;Erlang. ComtE communication.
%%       Default: `2002'.
%%    </dd>
%%    <dt><b>`com_port'</b></dt>
%%    <dd>The TCP port used form Erlang. ComtE-&gt;Com communication.
%%       Default: `2003'.
%%    </dd>
%%    <dt><b>`comte_ip'</b></dt>
%%    <dd>The IP address where Erlang. ComtE is running.
%%       Default: `{127,0,0,1}'.
%%    </dd>
%%    <dt><b>`com_ip'</b></dt>
%%    <dd>The IP address where COM is running.
%%       Default: `{127,0,0,1}'.
%%    </dd>
%%    <dt><b>`com_start_mw'</b></dt>
%%    <dd>If the ComtE MW should be started.
%%       Default: `true'.
%%    </dd>
%%    <dt><b>`callback_registrar'</b></dt>
%%    <dd>The path to where the callback register file should be stored.
%%       This variable is normally set by `comte:install_config/5',
%%       however if `install_config/5' is not called then this has to be
%%       set manually.
%%    </dd>
%%    <dt><b>`access_mgmt'</b></dt>
%%    <dd>The access management callback to register upon startup.
%%       Also, the `Module:start/1' function will be called upon startup
%%       of ComtE.  Unless specified, the empty list will be given
%%       as argument.
%%       Default: {@link comte_default_access_mgmt}
%%    </dd>
%%    <dt><b>`replicated_list'</b></dt>
%%    <dd>The replicated list callback to register upon startup.
%%       Also, the `Module:start/1' function will be called upon startup
%%       of ComtE.  Unless specified, the empty list will be given
%%       as argument.
%%       Default: {@link comte_default_replicated_list}
%%    </dd>
%%    <dt><b>`transaction_server'</b></dt>
%%    <dd>The transaction server callback module to register upon startup.
%%       Also, the `Module:start/1' function will be called upon startup
%%       of ComtE.  Unless specified, the empty list will be given
%%       as argument.
%%       Default: {@link comte_default_transaction_server}
%%    </dd>
%%    <dt><b>`crypto'</b></dt>
%%    <dd>The crypto utility callback module to register upon startup.
%%       Also, the `Module:start/1' function will be called upon startup
%%       of ComtE.  Unless specified, the empty list will be given
%%       as argument.
%%       Default: {@link comte_default_crypto_util}
%%    </dd>
%%    <dt><b>`log_write'</b></dt>
%%    <dd>The logging callback module to register upon startup.
%%       Also, the `Module:start/1' function will be called upon startup
%%       of ComtE.  Unless specified, the empty list will be given
%%       as argument.
%%    </dd>
%%    <dt><b>`cm_event_producer'</b></dt>
%%    <dd>The configuration mgmt event producer callback module to register
%%       upon startup. Also, the `Module:start/1' function will be called
%%       upon startup of ComtE.  Unless specified, the empty list will be given
%%       as argument.
%%       Default: {@link comte_default_alarm_event_producer}
%%    </dd>
%%    <dt><b>`alarm_producer'</b></dt>
%%    <dd>The alarm producer callback module to register upon startup.
%%       Also, the `Module:start/1' function will be called upon startup
%%       of ComtE.  Unless specified, the empty list will be given
%%       as argument.
%%       Default: {@link comte_default_alarm_event_producer}
%%    </dd>
%%    <dt><b>`oi_register'</b></dt>
%%    <dd>The object implementer register callback module to register upon startup.
%%       Also, the `Module:start/1' function will be called upon startup
%%       of ComtE.  Unless specified, the empty list will be given
%%       as argument.
%%       Default: {@link comte_default_oi_register}
%%    </dd>
%%    <dt><b>`pm'</b></dt>
%%    <dd>The pm callback module to register upon startup.
%%       Also, the `Module:start/1' function will be called upon startup
%%       of ComtE.  Unless specified, the empty list will be given
%%       as argument.
%%       Default: {@link comte_default_pm_handler}
%%    </dd>
%%    <dt><b>`healthcheck_interval'</b></dt>
%%    <dd>ComtE will call `comte:healthCheck()' with this interval
%%       if there are no other requests, to verify
%%       that COM is running fine.  If a healthCheck returns a bad
%%       result or times out COM is killed and the application
%%       ComtE is stopped.
%%       Default: `3000'.
%%    </dd>
%%    <dt><b>`healthcheck_timeout'</b></dt>
%%    <dd>The timeout to use for `comte:healthCheck()'.
%%       Default: `2000'.
%%    </dd>
%% </dl>

-type 'ComOamSpiNotificationFmSeverity'() ::
	?FmSeverityCleared | ?FmSeverityIndeterminate |
	?FmSeverityWarning | ?FmSeverityMinor |
	?FmSeverityMajor | ?FmSeverityCritical.
%% All possible severity levels on an alarm. The macros are defined in
%% comte/include/comte_fm.hrl.

-type 'ComOamSpiNotificationFmMajorId'() :: 0..4294967295.
%% Major type is a vendor specific integer defined by
%% IANA as SMI Network Management Private Enterprise Codes.
%% Ex: 193 is for Ericsson.

-type 'ComOamSpiNotificationFmMinorId'() :: 0..4294967295.
%% Minor type consists of two 16-bit positive integer.
%% The most significant 16-bit part identifies the
%% subsystem or area and must be registered with COM.
%% The less significant 16-bit part is defined inside
%% the subsystem or area.

-type alarm_add_text() :: binary().
%% Additional information about
%% the alarm. It's optional.

-type alarm_add_info() :: {Name::binary(), Value::binary()}.
%% Further info about the alarm. It's optional.

-type alarm_opt() :: {add_text, alarm_add_text()} |
                     {add_info, list(alarm_add_info())}.
%% The valid alarm optionals.

-type 'ComMwSpiHaMode'() :: ?ComMwSpiHaModeUnassigned |
			    ?ComMwSpiHaModeActive |
			    ?ComMwSpiHaModeStandby.
%% All possible High Availability modes.
%% The macros with descriptions are defined in comte/include/comte_ac.hrl.

-type 'ComMwSpiHaReason'() :: ?ComMwSpiHaReasonInit |
			      ?ComMwSpiHaReasonFail |
			      ?ComMwSpiHaReasonSwitchover.
%% The possible reasons for changing the High Availability mode.
%% The macros with descriptions are defined in comte/include/comte_ac.hrl.

-type 'ComMwSpiRecommendedRecovery'() :: ?ComMwSpiRecommendedRecoveryRestart |
					 ?ComMwSpiRecommendedRecoveryFailover.
%% The suggested actions to be done to recover from an bad health status report.
%% The macros with descriptions are defined in comte/include/comte_ac.hrl.

-type 'ComMwSpiReturn'() :: ?ComOk | ?ComTryAgain | ?ComNotActive |
			     ?ComFailure | ?ComAlreadyExist | ?ComAborted |
			     ?ComObjectLocked | ?ComPrepareFailed |
			     ?ComCommitFailed | ?ComInvalidArgument |
			     ?ComValidationFailed | ?ComNoResources |
			     ?ComTimeOut.
%% The possible response types for Availability Controller checks.
%% These values are normally used in fasion so that if:
%% Return value = `?ComOk', everything is ok
%% Return value &lt; `?ComOk', an error has occured.
%% Return value &gt; `?ComOk', the service is not available right now.
%%
%% The macros with descriptions are defined in comte/include/comte_ac.hrl.

-define(DEFAULT_AC_TIMEOUT, 10000).


%%%===================================================================
%%% API
%%%===================================================================
%% @doc Common register callback API.
%%      It can be used to register callbacks in ComtE
%%      in the same manner as 'register_comte_service_callback' APIs.
%% @end
-spec register_callback(comte_types:comte_cb_key(), module()) -> ok.
register_callback(Key, Cb) ->
    case comte_types:comte_cb_key(Key) of
	true ->
	    comte_registrar:register(Key, Cb);
	false ->
	    error({invalid_key, Key})
    end.

%% @doc Register a callback to handle any role MW SPI requests.
%% The callback module should implement a single function called `getRoles'
%% defined in {@link comte_access_mgmt_api}.
%% which takes one binary string argument and returns a list of binary strings.
%%
%% E.g: `getRoles(<<"testuser">>) -> [<<"admin">>,<<"wheel">>].'
%%
%% The roles returned then have to be retrievable under
%% `ManagedElement=1,SystemFunctions=1,SecM=1,
%% Authorization=1,Local=1,Role={RoleName}'.
%% See ComSecAuthorization.xml for details.
%%
%% @end
-spec register_role_callback(atom()) -> ok.
register_role_callback(Cb) ->
    comte_registrar:register(access_mgmt, Cb).


%% @doc Register a new callback for replicated linked lists.
%% It should follow the {@link comte_replicated_list_api} specification.
%% At startup of ComtE a replicated list specified by `{replicated_list}'
%% will be registered.
%% @end.
%% @see comte_default_replicated_list
-spec register_replicated_list_callback(atom()) -> ok.
register_replicated_list_callback(Cb) ->
    comte_registrar:register(replicated_list, Cb).

%% @doc Register a new callback for a transaction server.
%% It should follow the {@link comte_transaction} specification.
%% At startup of comte a default transaction server is registered.
%% @end.
%% @see comte_default_transaction_server

-spec register_transaction_server_callback(atom()) -> ok.
register_transaction_server_callback(Cb) ->
    comte_registrar:register(transaction_server, Cb).


%% @doc Register a new callback for crypto utility functions.
%% It should follow the {@link comte_crypto_api} specification.
%% At startup of comte a crypto handler specified by '{crypto}'
%% will be registered.
%% @end.

-spec register_crypto_callback(atom()) -> ok.
register_crypto_callback(Cb) ->
    comte_registrar:register(crypto, Cb).


%% @doc Register a new callback for log write.
%% It should follow the {@link comte_log_write_api} specification.
%%
%% This callback will be called whenever COM generates a log event,
%% if the ComtE shared library configuration variable
%% `comte_logging' is `true'.
%% Log events with lesser severity (higher severity value) than
%% set in the variable `comte_logging_level' will be discarded
%% without reaching the callback.
%
%% Note that facilities 100 and 101 (Fault Management alarm records)
%% always goes to a dedicated log file regardless of severity and
%% will never reach the callback.
%% @end.

-spec register_log_write_callback(atom()) -> ok.
register_log_write_callback(Cb) ->
    comte_registrar:register(log_write, Cb).




%% @doc Register a new callback for an object implementer register.
%% It should follow the {@link comte_oi_register_api}
%% specification.
%%
%% This callback will be called during COM startup and
%% shutdown, specifically when a COM component registers
%% itself as an object implementer.
%%
%% Use the 'oi_register' environment variable to
%% change the default object implementer register implementation.
%% @end.
%% @see comte_default_oi_register

-spec register_object_implementer_reg_callback(atom()) -> ok.
register_object_implementer_reg_callback(Cb) ->
    comte_registrar:register(oi_register, Cb).


%% @doc Register a new callback for a configuration
%% management event producer.
%% It should follow the {@link comte_alarm_event_producer_api}
%% specification.
%%
%% This callback will be called during COM startup and
%% shutdown, specifically when a COM component registers
%% itself as a cm event subscriber.
%%
%% Use the 'cm_event_producer' environment variable to
%% change the default cm producer implementation.
%% @end.
%% @see comte_default_alarm_event_producer
-spec register_cm_event_producer_callback(atom()) -> ok.
register_cm_event_producer_callback(Cb) ->
    comte_registrar:register(cm_event_producer, Cb).


%% @doc Start the erlang side of ComtE.
%% @end
-spec start_link(list(app_env())) -> {ok,pid()}.
start_link(Opts) ->
    set_envs(Opts),
    comte_app:start(normal, []).

%% @doc Stop ComtE and COM if COM was started using this module.
%% @end
-spec stop() -> ok | {error,_}.
stop() ->
    application:stop(comte).


%% @doc
%% Start COM without additional app environment
%% @end
-spec start_com() ->
		       {ok, LinuxPid :: string()} |
		       {error, already_started} |
		       {error, Reason :: term()}.
start_com() ->
    start_com([]).

%% @doc Start the COM component using the `{start_com_prog}'
%% starter program containing whatever needed for COM to start and run.
%% When this function returns COM is started and ready for other operations.
%%
%% If COM is already started `{error, already_started}' is returned.
%%
%% If the operation times out COM is killed and the ComtE application
%% is stopped. Timeout time is `{start_com_timeout}'.
%% @end
-spec start_com(list(app_env())) ->
		       {ok, LinuxPid :: string()} |
		       {error, already_started} |
		       {error, Reason :: term()}.
start_com(Opts) ->
    set_envs(Opts),
    comte_com:start().


%% @doc stop_com(5000).
%% @end
-spec stop_com() ->
		      ok |
		      {error, already_stopped} |
		      {error, Reason :: term()}.
stop_com() ->
    comte_com:stop().

%% @doc Stop the currently running COM instance.
%%
%% If COM is not started `{error, already_stopped}' is returned.
%%
%% If the operation times out COM is killed and the ComtE application
%% is stopped.
%% @end
-spec stop_com(non_neg_integer()) ->
		      ok |
		      {error, already_stopped} |
		      {error, Reason :: term()}.
stop_com(Timeout) ->
    comte_com:stop(Timeout).


%% @doc kill_com(3000)
%% @end
-spec kill_com() ->
		      ok |
		      {error, already_stopped} |
		      {error, Reason :: term()}.
kill_com() ->
    comte_com:kill().

%% @doc Kill any currently running COM instance.
%%
%% If COM is not started `{error, already_stopped}' is returned.
%%
%% If the operation times out COM is killed and the ComtE application
%% is stopped.
%% @end
-spec kill_com(non_neg_integer()) ->
		      ok |
		      {error, already_stopped} |
		      {error, Reason :: term()}.
kill_com(Timeout) ->
    comte_com:kill(Timeout).


%% @doc Send a notification using COM FM.
%% The alarm/alert has to be be present in the
%% `ManagedElement=1,SystemFunctions=1,Fm=1,FmAlarmModel' DN.
%% See ComFm.xml for details.
%%
%% In order for a SNMP trap to be triggered when sending an alarm you have to enable
%% the libcom_fm_snmp SA (see install/5 for details).
%%
%% In order for stateful alarms to be shown when browsing the configuration tree
%% you have to register a replicated list callback and install the ComFmMoImplComponent
%% to take care of the FmAlarm root.
%% @end

-spec send_alarm(comte_types:ecim_dn(),
                 'ComOamSpiNotificationFmMajorId'(),
		 'ComOamSpiNotificationFmMinorId'(),
                 'ComOamSpiNotificationFmSeverity'(), 
		 [alarm_opt()] | alarm_add_text()) ->
			ok | {error, Reason :: term()}.
send_alarm(DN, MajorId, MinorId, Severity, AlarmOpts)
  when is_integer(MajorId),
       is_integer(MinorId),
       is_integer(Severity),
       is_list(AlarmOpts) ->
    UpdAlarmOpts = [{severity, Severity} | AlarmOpts],
    case comte_lib:validate_alarm_opts(UpdAlarmOpts) of
        {error, Reason} ->
            {error, Reason};
        FlatAlarmOpts ->
            case comte_registrar:get_callback(alarm_producer) of
                {error, Reason} -> {error, Reason};
                Callback ->
                    Consumers = apply(Callback, filter_consumers, [alarm]),
                    comte_lib:send_alarm(Consumers, DN,
                                         MajorId, MinorId,
                                         FlatAlarmOpts)
            end
    end;

send_alarm(DN, MajorId, MinorId, Severity, AdditionalText)
  when is_binary(AdditionalText) ->
    AlarmOpts = [{add_text, AdditionalText}],
    send_alarm(DN, MajorId, MinorId, Severity, AlarmOpts).


%% @doc Send an event using COM.
%%      For an SA that reports both CM notifications
%%      generated from the NBI and from the MW,
%%      the function for reported NBI updates by the
%%      CM Router must be turned off for that CM Participant.
%%      Please consult the COM documentation for configuration options.
%%
%%      Supported event:
%% <dl>
%%     <dt><b>{@type com_notification()}</b></dt>
%%     <dd>Event to send when a configuration change
%%         has been made by the system.
%%         Foreach event in the notification event list,
%%         a netconf event will be generated by COM.
%%
%%         See details in comte_event.hrl
%%     </dd>
%% </dl>
%% @end
-spec notify(com_notification()) -> ok | {error, Reason :: term()}.
notify(#cm_notification_1{} = Notification) ->
    case comte_lib:validate_notification(Notification) of
        {error, Reason} ->
            {error, Reason};
        UpdNtf ->
            [#cm_event_1{dn=DN} | _Events] = UpdNtf#cm_notification_1.events,
            case comte_registrar:get_callback(cm_event_producer) of
                {error, Reason} -> {error,Reason};
                Callback ->
                    Consumers = apply(Callback, filter_consumers,
                                      [{cm_event, {dn, DN}}]),
                    comte_lib:notify_consumers(Consumers, UpdNtf)
            end
    end.

-spec pm_gp_ready(comte_pm_gp:pm_gp_ready_1()) -> ok | {error, Reason :: term()}.
pm_gp_ready(PmGp) ->
    case comte_lib:validate_pm_gp(PmGp) of
        {error, Reason} ->
            {error, Reason};
        ok -> 
            case comte_registrar:get_callback(pm_event_producer) of
                {error, Reason} -> {error, Reason};
                Callback ->
                    Consumers = apply(Callback, filter_consumers, [pm_gp_ready]),
                    comte_lib:notify_consumers(Consumers, PmGp)
            end
    end.

%% @doc Log to COM logs.
%% Write log entry to a COM log, for instance
%% the default com.log or com_alarm.log. Note! If <code>comte_logging</code>
%% is enabled in <code>libComtE.cfg,</code> the log entry might be returned
%% to the registered logWrite callback module.
%% See details in <code>comte_log.hrl</code>
%% @end
-spec log(File::module() | binary(), Func::atom() | binary(),
          Line::0..65536,
          Severity::comte_log_severity() | com_log_severity(),
          Facility::comte_log_facility(),
          Msg::binary()) ->
                 ok | {error, Reason :: term()}.
log(File, Func, Line, Severity, Facility, Msg) ->
    comte_lib:log_to_com(File, Func, Line, Severity, Facility, Msg).


%% @doc Set COM's HA mode.
%%
%% HaMode is the HA mode that COM shall operate in.
%%
%% HaReason is the reason for the change of the HA mode.
%%
%% If the operation times out COM is killed and the ComtE application
%% is stopped.
%% @end
-spec setHaMode(
	'ComMwSpiHaMode'(), 'ComMwSpiHaReason'(), non_neg_integer()) ->
		       'ComMwSpiReturn'() |
		       {error,com_not_started}.
setHaMode(HaMode, HaReason, Timeout) ->
    comte_com:setHaMode(HaMode, HaReason, Timeout).


%% @doc healthCheck(T) where T is `{healthcheck_timeout}'
%% @end
-spec healthCheck() ->
			 ?ComOk |
			 { 'ComMwSpiReturn'(),
			   'ComMwSpiRecommendedRecovery'()} |
			 {error,com_not_started}.
healthCheck() ->
    comte_com:healthCheck().

%% @doc Issue a health check to COM.
%%
%% The function returns `?ComOk' if the check went ok or a tuple containing
%% the response and a recommended response if an error was detected.
%%
%% If the operation times out COM is killed and the ComtE application
%% is stopped.
%% @end
-spec healthCheck(non_neg_integer()) ->
			 ?ComOk |
			 { 'ComMwSpiReturn'(),
			   'ComMwSpiRecommendedRecovery'()} |
			 {error,com_not_started}.
healthCheck(Timeout) ->
    comte_com:healthCheck(Timeout).


%% @doc Returns various information about COM and ComtE.
%%
%% @end
-spec info() ->
		  [{com_health,
		    ?ComOk |
		    {'ComMwSpiReturn'(),
		     'ComMwSpiRecommendedRecovery'()} |
		    {error, com_not_started} } |
		   {com_pid, integer() | undefined} |
                   {access_mgmt, Module :: atom()} |
                   {replicated_list_callback, Module :: atom()} |
                   {transaction_server, Module :: atom()} |
                   {crypto, Module :: atom()} |
                   {log_write, Module :: atom()} |
                   {cm_event_producer, Module :: atom()} |
                   {pm_event_producer, Module :: atom()} |
                   {alarm_producer, Module :: atom()} |
                   {oi_register, Module :: atom()} |
                   {pm, Module :: atom()} |
                   {pm_gp, Module :: atom()} |
                   {environment, list(app_env())}].
info() ->
    [{com_health, catch healthCheck()},
     {com_pid, catch comte_com:get_pid()},
     {access_mgmt, catch comte_registrar:get_callback(access_mgmt)},
     {replicated_list_callback, catch comte_registrar:get_callback(replicated_list)},
     {transaction_server, catch comte_registrar:get_callback(transaction_server)},
     {crypto, catch comte_registrar:get_callback(crypto)},
     {log_write, catch comte_registrar:get_callback(log_write)},
     {cm_event_producer, catch comte_registrar:get_callback(cm_event_producer)},
     {pm_event_producer, catch comte_registrar:get_callback(pm_event_producer)},
     {alarm_producer, catch comte_registrar:get_callback(alarm_producer)},
     {oi_register, catch comte_registrar:get_callback(oi_register)},
     {pm, catch comte_registrar:get_callback(pm)},
     {pm_gp, catch comte_registrar:get_callback(pm_gp)},
     {environment, catch get_all_env()}].


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
get_env(Par) ->
    application:get_env(app(), Par).

%% @private
get_env(Par, Default) ->
    application:get_env(app(), Par, Default).

%% @private
get_all_env() ->
    application:get_all_env(app()).

%% @private
set_env(Par, Val) ->
    application:set_env(app(), Par, Val).

%% @private
template_dir() ->
    PrivDir = code:priv_dir(app()),
    {ok,TemplateDir} = get_env(template_dir),
    filename:join(PrivDir, TemplateDir).


set_envs(Opts) ->
    App = app(),
    [application:set_env(App, Par, Val)  || {Par,Val} <- Opts].


updateIpVersion(IPversion)->
    comte_com_config:updateIpVersion(IPversion).
