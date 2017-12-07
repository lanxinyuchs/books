%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaNtpServer.erl %
%%% @author ekurnik
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R9A/R10A/4

%%% @doc ==Module title==
%%% Gen_server that looks after the NTP daemon.
%%%
%%% The AllNtpServersUnreachableAlarm is raised when the NtpServerUnreachableAlarm
%%% is raised on all NtpServers or there is no NtpServer configured
%%%
%%% The NtpServerUnreachableAlarm is raised when the configured NtpServer
%%% is unreachable for 4 hours. The alarm is ceased if the NtpServer is removed or
%%% becomes reachable
%%%
%%% Most status changes are event driven (traps from ntpd), but while there
%%% is anything NOT ok, the status is polled every 5 minutes, this is because
%%% there is no (reliable) event reporting when an individual server becomes ok.
%%% @end

-module(comsaNtpServer).
-behaviour(gen_server).
-author('ekurnik').
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R9A/R10A/4').


%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
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
%%% Rev      Date        Name        What
%%% -----    ---------   --------    ------------------------
%%% Rx       2014-02-05  etxlg       Created
%%% R2A/2    2014-02-12  etxlg       Continued
%%% R2A/3    2014-02-17  etxlg       Alarm sending
%%% R2A/4    2014-02-18  etxlg       Jipii, first bugfix (not really)
%%% R2A/5    2014-02-19  etxlg       second bugfix (eternal loop)
%%% R2A/6    2014-02-19  etxlg       disabled reboot and removed ERROR REPORT
%%% R2A/7    2014-02-19  etxlg       third bugfix (cease alarms)
%%% R2A/8    2014-02-24  etxlg       New restart detection
%%% R2A/9    2014-02-27  etxlg       Print more when state: initial->running
%%% R2A/10   2014-03-19  etxlg       notification at timestep, reboot enabled
%%% R2A/11   2014-03-19  etxlg       handle sim and print when stepping
%%% R2A/12   2014-03-26  etxlg       Adaption for ecoli commands
%%% R2A/13   2014-03-28  etxlg       Disable RCS-COLI commands in simulator
%%% R2A/14   2014-04-14  etxlg       Retry restart until...
%%% R2A/15   2014-04-22  etxlg       DN for alarms
%%% R2A/16   2014-04-22  etxlg       find ntp_control.sh using $PATH
%%% R2A/17   2014-06-25  etxlg       is_synced() used in testcases, sec-log
%%% R2A/18   2014-07-08  etxberb     Changed restart_piu to restart_node.
%%% R2A/19   2014-08-19  etxlg       Correct MeId in the securitylog
%%% R2A/20   2014-09-19  etxlg       New definitions used to raise/clear alarms
%%%				     still need to add proper restrict and
%%%				     test trick to get OaM address in packet
%%% R2A/21   2014-09-30  etxlg       name-changed alarms
%%% R2A/22   2014-10-03  etxlg       restrict and bind to OaM TR HS89845
%%% R2A/23   2014-10-10  etxlg       name changed alarms again
%%% R3A/1    2014-09-30  erarafo     Support for Additional Info
%%% R3A/4    2015-01-26  etxlg       Name-spaceing use APPM to start
%%%				     NEED APPM callback to detect ntpd exit
%%%				     NOT prepared for namespacing on LMT
%%%				     partial merge from R2 (all that should be)
%%% R3A/5    2015-01-30  etxlg       Startup ordering changed due to long wait
%%%				     for OOT/net_ns
%%% R3A/6    2015-02-02  etxlg       Kick the trace subsystem at time change
%%%				     fix error print in handle_table_event
%%% R3A/7    2015-02-04  etxlg       Bug fix, doesn't run at all if no net_ns
%%% R3A/8    2015-02-09  etxlg       Prepare to use new API towards APPM
%%% R3A/9    2015-02-11  etxlg       Work with namespaced LMT (lab use)
%%% R3A/10   2015-04-01  etxlg       Name resolution namespace aware
%%% R3A/11   2015-04-15  etxlg       TR HT65237 workaround
%%% R3A/12   2015-04-28  etxlg       One more place nns could be 'undefined'
%%% R4A/1    2015-08-20  etxpejn     Added rpc:call for SecurityLog
%%% R4A/2    2015-09-04  uabesvi     HT83145
%%% R4A/8    2015-09-10  etxpejn     Regular MP should fetch and compair time with core MP
%%% R4A/10   2015-09-25  etxpejn     Moved rpc:call to logI:write_log
%%% R4A/13   2015-10-15  etxberb     OTP-18: * Replaced erlang:now/0.
%%%                                  * Added timestep/1 & timestep/2.
%%% R4A/14   2015-10-16  uabesvi     appm_up -> true at local restart
%%% R4A/15   2015-11-11  etxberb     Added sleep in check_for_clock_step to
%%%                                  allow for the OS clock to be updated before
%%%                                  to start reading the new time.
%%% ----    ---------- -------  ------------------------------------------------
%%% R5A/1   2015-11-09   uabesvi     added UPDATE FOR NTP CLUSTER (as comments)
%%%                                  Not ready, not fully tested and 
%%%                                  not activated.
%%% R5A/2   2015-11-09   uabesvi     Reverted back to R4A/14
%%% R5A/4   2015-12-18   etxlg       IPv6
%%% R5A/5   2015-12-22   etomist     Removed NTP server restart if time jump > 5 min
%%% R5A/7   2016-02-03   etxlg       Clean up pgh when using start_internal_lm
%%% R5A/8   2016-02-18	 ekurnik     NTP Alarm changes according to HU28114.
%%%				     NTPServerUnavailable alarm raise time set to 4 hours.
%%%				     The alarm raise condition is reachability.
%%%				     CalendarClockMisalignment alarm cease time set to 1 hour.	
%%% R6A/1   2016-08-30	 etxpejn     Fixed indentation
%%% R6A/2   2016-08-30	 etxpejn     Fixed HV20790, restart ntpd in do_polling_if_any_nok even
%%%                                  if no peer exists but a ntp server is configured
%%% R7A/1   2016-09-23	 etxpejn     Added fucntions to be used when collecting EIS
%%% R7A/2   2016-09-23	 etxpejn     Only run get_server_state/1 on target
%%% R7A/4   2016-09-28	 etxarnu     Enabled NTP for VRCS
%%% R7A/5   2016-09-28	 etxpejn     Changed the POLL_TIMEOUT to 1,5 minutes to avoid that 
%%%                                  alarm is raised at the same time as the ntpd is restarted
%%% R7A/6   2016-10-11	 etxpejn     Made call to do_print_server_state/2 more robust 
%%% R9A/1   2017-01-31	 etomist     HV53411
%%% R10A/1  2017-06-09   ekurnik     COMSA part of HV94590 solution
%%% R10A/2  2017-07-03   enekdav     COMSA trap notifications design
%%% R10A/3  2017-07-03   enekdav     Added poll ntpd message delay
%%% R10A/4  2017-07-05   ekurnik     Solution for HV97497
%%% ----------------------------------------------------------

%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% ###=====================================================================###
%%% # 2.1   EXPORTED INTERFACE FUNCTIONS
%%% ###---------------------------------------------------------------------###
-export([start_link/0]).
-export([subscribe_timestep/1,
	 timestep/1,
	 timestep/2]).
-export([ntp_exit/1]). %callback from APPM if ntpd exits

%%exported for RCS-COLI
-export([ecoli_ntpd_state/1, ecoli_print_server_state/1, ecoli_print_traps/1]).
%% exported for CCI use
-export([cci_get_ntpd_sync_source_state/0]).
%% exported mostly for lab use
-export([is_synced/0, is_synced_dirty/0]).
%% exported for debug
-export([start/1]).
-export([stop/0]).
-export([restart_ntpd/0]).
-export([print_ntp_state/1]).
-export([print_traps/0, print_traps/1, print_traps/2]).
-export([set_verbosity_level/1]).
-export([ts_now_diff/0]).
-export([print_server_state/0, print_server_state/1]).
-export([print_ntpd_state/1]).
-export([reread_ntpd_status/0]).
-export([test_write_config/0]).

-export([check_if_time_on_reg_needs_to_be_updated/0]).

-export([start_ntp/0]).

%%% ###---------------------------------------------------------------------###
%%% # 2.2   EXPORTED INTERNAL FUNCTIONS
%%% ###---------------------------------------------------------------------###
%%% gen_server callback API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3, format_status/2]).

%called in apply_after
-export([restart_cold/1]).

%%% ###=====================================================================###
%%% # 2.3   IMPORT OF DEFINITIONS
%%% ###---------------------------------------------------------------------###
-include("RcsSysM.hrl").
-include_lib("kernel/include/inet.hrl"). %for #hostent{}

%-compile([export_all]).

%%% ###=====================================================================###
%%% # 2.4   LOCAL DEFINITION OF MACROS
%%% ###---------------------------------------------------------------------###
-define(NTP_PORT, 123).
-define(LBI, {127, 0, 0, 1}).
-define(LBIv6, {0, 0, 0, 0, 0, 0, 0, 1}).
-define(OP_CODE_TRAP, 7).
-define(OP_CODE_RSTATUS, 1).
-define(SERVER_NAME, ?MODULE).
-define(SUDO, "/usr/bin/sudo").
-define(CONTROL_SCRIPT, "ntp_control.sh").
-define(MILLION, 1000000).
-define(TRAP_HOLDOFF_TIME, 2000). % ms to wait before checking ntp after trap
-define(MAX_TRAP_PACKETS, 50). % this many are kept in the state variable
-define(DEFAULT_DRIFT, 0.0). % ppm
%% 5 minutes of time offset -> 8.3 hours to fix itself (adjusts at 1%)
-define(INITIAL_MAX_NOW_DIFF, 5 * 60 * ?MILLION). % 5minutes in microseconds
-define(MAX_INITIAL_STATE_TIME, 1000 * 3600). % one hour
-define(ALARM_HOLDOFF_TIME, 1000 * 60 * 2). % 2 minutes
-define(INITIAL_RETRY_TIME, 1000). % 1 second
-define(POLL_TIMEOUT, 1000 * 90). % 1,5 minute
-define(POLL_NTPD_DELAY, 1000 * 3). % 3 seconds
-define(MNESIA_RAM_TABLE, comsa_ntp_alarms).
-define(SYSM_DN, [<<"ManagedElement=1">>,
		<<"SystemFunctions=1">>,
		<<"SysM=1">>]).
%alarms, corresponding to what is in app-registration-xml
-define(ALL_NTP_ALARM, 'CalendarClockAllNTPServersUnavailable').
-define(NTP_ALARM, 'CalendarClockNTPServerUnavailable').
-define(CALENDAR_CLOCK_ALARM, 'CalendarClockMisaligned').
%% For Availability Logging:
%% Restart case not affecting ISP, so no need to publish this case in ALH IWD.
-define(AVLI_CAUSE_ClockUpdateExceededMaxDiff, "ClockUpdateExceededMaxDiff").

-define(NTP_SERVER_UNAVAILABLE_ALARM_RAISE_TIME, 1000 * 60 * 60 * 4). % 4 hours
-define(ALL_NTP_SERVERS_UNAVAILABLE_ALARM_RAISE_TIME, 0). % instant
-define(CALENDAR_CLOCK_MISALIGNED_ALARM_RAISE_TIME, 0). %instant

-define(NTP_SERVER_UNAVAILABLE_ALARM_CEASE_TIME, 0). % instant
-define(ALL_NTP_SERVERS_UNAVAILABLE_ALARM_CEASE_TIME, 0). % instant
-define(CALENDAR_CLOCK_MISALIGNED_ALARM_CEASE_TIME, 1000 * 60 * 60). % 1 hour

-define(NTP_SERVER_UNAVAILABLE_ALARM_TIMES, 
	{?NTP_SERVER_UNAVAILABLE_ALARM_RAISE_TIME, ?NTP_SERVER_UNAVAILABLE_ALARM_CEASE_TIME}).
-define(ALL_NTP_SERVERS_UNAVAILABLE_ALARM_TIMES, 
	{?ALL_NTP_SERVERS_UNAVAILABLE_ALARM_RAISE_TIME, ?ALL_NTP_SERVERS_UNAVAILABLE_ALARM_CEASE_TIME}).
-define(CALENDAR_CLOCK_MISALIGNED_ALARM_TIMES, 
	{?CALENDAR_CLOCK_MISALIGNED_ALARM_RAISE_TIME, ?CALENDAR_CLOCK_MISALIGNED_ALARM_CEASE_TIME}).

-define(NTP_SELECT_SYNC_SOURCE, 6). %% codeword from peerstatus

%% General
-define(FUNCTION, 
	element(2, element(2, process_info(self(), current_function)))).

%%% ###=====================================================================###
%%% # 2.5   LOCAL DEFINITION OF RECORDS
%%% ###---------------------------------------------------------------------###
-record(st, {
	  server_state = initial, %% initial -> (restarting) ->  running
	  ntp_state,   %% system-state and list of server-state
	               %% now_diff = 0,
	  ntp_servers = [], %% to translate between IP and object reference
	  ntp_unresolved_servers = [],
	  oam_ip = [], %% fetched from OOT
	  oam_ip_family,
	  loopback,
	  sock,
	  udp_portno,
	  ntp_pgm_id,	%% this IS used
	  ntpd_started_at,
	  config_file_name,
	  pid_file_name,
	  ctrl_file_name,
	  seq_no = 1,
	  reassembly,
	  trap_ho_tref,
	  alarm_ho_tref,
	  initial_tref,
	  update_monitor,
	  trap_packets  = {0, queue:new()},
	  freq_offset   = ?DEFAULT_DRIFT,
	  verbosity     = 0,
	  timestep_funs = [],
	  reply_here    = undefined,
	  clock_stepped_since_start,
	  nns,
	  appm_up = false,
	  ntp_alarm_timers = []
	 }).

-record(comsaNtpTime, {
	  offset_start,
	  offset_latestStep,
	  step_start = 0,
	  step_latest = 0
	 }).

-type alarm() :: {atom(), list(), warning | minor | major}.
-type st() :: #st{}.
-type proplist() :: [{term(), term()}].
%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------


%%% @doc Starts the server.
%%%
%%%
%%% ===Arguments===
%%% no arguments
start_link() ->
    case target_or_vrcs() of
	true ->
    	    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, 0, []);
	_ ->
	    ignore
    end.

subscribe_timestep(Fun) ->
    case target_or_vrcs() of
	true ->
	    gen_server:call(?SERVER_NAME, {subscribe_timestep, Fun});
	_ ->
	    ok
    end.

timestep(Opt) ->
    timestep(Opt, native).

timestep(Opt, TimeUnit) ->
    [#comsaNtpTime{step_start = StepS, step_latest = StepL}] =
	ets:lookup(comsaNtpTime, comsaNtpTime),
    case Opt of
	latest ->
	    erlang:convert_time_unit(StepL, native, TimeUnit);
	from_start ->
	    erlang:convert_time_unit(StepS, native, TimeUnit);
	_ ->
	    sysInitI:error_report([{?MODULE, ?FUNCTION},
				   {unrecognized_option, Opt},
				   {module, sysUtil:get_previous_module()}]),
	    0
    end.

ntp_exit(List) ->
    gen_server:cast(?SERVER_NAME, {ntp_exit, hd(List)}).


print_ntpd_state(Fd) ->
    exit_if_simulated(),
    Read_status = {_, _, _, _, Rest} =
	gen_server:call(?SERVER_NAME, {get_ntp_state, 0}),
    Peer_assocs = extract_peer_assocs(Rest),
    comsaNtpUtil:decode_packet(3, os:timestamp(), Read_status, Fd),
    comsaNtpUtil:decode_and_print_variables(3, Read_status, Fd),
    [begin
	Rs = gen_server:call(?SERVER_NAME, {get_ntp_state, Assoc}),
    comsaNtpUtil:decode_packet(3, os:timestamp(), Rs, Fd),
    comsaNtpUtil:decode_and_print_variables(3, Rs, Fd)
     end || Assoc <- Peer_assocs].

%these are for rcs-coli, test and debug
ecoli_ntpd_state(_Arg) ->
    exit_if_simulated(),
    Read_status = {_, _, _, _, Rest} =
	gen_server:call(?SERVER_NAME, {get_ntp_state, 0}),
    Peer_assocs = extract_peer_assocs(Rest),
    comsaNtpUtil:decode_packet(3, os:timestamp(), Read_status),
    comsaNtpUtil:decode_and_print_variables(3, Read_status),
    [begin
	Rs = gen_server:call(?SERVER_NAME, {get_ntp_state, Assoc}),
    comsaNtpUtil:decode_packet(3, os:timestamp(), Rs),
    comsaNtpUtil:decode_and_print_variables(3, Rs)
     end || Assoc <- Peer_assocs].

ecoli_print_server_state(_Arg) ->
    exit_if_simulated(),
    print_server_state().

ecoli_print_traps([]) ->
    exit_if_simulated(),
    print_traps(2);
ecoli_print_traps(Arg) ->
    exit_if_simulated(),
    try list_to_integer(hd(Arg)) of
	Int when Int=<3, Int>=1 ->
	    print_traps(Int);
	_ ->
	    io:format("Argument: Verbosity can be 1, 2, or 3~n"),
	    exit("argument error")
    catch
	_:_ ->
	    io:format("Argument: Verbosity can be 1, 2, or 3~n"),
	    exit("argument error")
    end.

%% @doc Function which returns state of ntpd sync source, called from CCH
-spec cci_get_ntpd_sync_source_state() -> 
          no_sync_source | {sync_source, proplist()}.
cci_get_ntpd_sync_source_state() ->
    exit_if_simulated(),
    case get_current_ntpd_sync_source_state() of
        [] ->
            %% no current sync source
            no_sync_source;
        NtpdVars ->
            {sync_source, NtpdVars}
    end.

get_current_ntpd_sync_source_state() ->
    {_, _, _, _, Bin} =
    gen_server:call(?SERVER_NAME, {get_ntp_state, 0}),
    Peer_assocs = extract_peer_assocs(Bin),
    
    poll_ntpd_sync_source(Peer_assocs).

%% @doc Function which polls one assoc at time and searches for sync source
poll_ntpd_sync_source([]) ->
    %% no sync source in any assoc
    [];
poll_ntpd_sync_source([Assoc | Rest]) ->
    {_, _, Assoc, Peerstatus, Vars} = 
    gen_server:call(?SERVER_NAME, {get_ntp_state, Assoc}),
    
    case peerstatus_to_select(<<Peerstatus:16>>) of
        ?NTP_SELECT_SYNC_SOURCE -> %% sync source
            comsaNtpUtil:decode_variables(Vars);
        _ -> %% not a sync source, proceed
            poll_ntpd_sync_source(Rest)
    end.

% if really synced -> true othervise false
is_synced() ->
    S = gen_server:call(?SERVER_NAME, get_server_state),
      is_synced(S).

is_synced(S) ->
    S#st.server_state =:= running andalso		%out of initial state
    S#st.ntp_servers =/= [] andalso			%servers configured
    element(1, S#st.ntp_state) =:= ok. %% andalso	%inverse 'never synced'
    %%mnesia:table_info(?MNESIA_RAM_TABLE, size) =:= 0.	% no alarms

%% this replies true as soon as there has been a clock-step
%% also if there is no step (time is already perfect, or MAX_INITIAL_STATE_TIME
%% expired) it will fall back on the more correct (but usually takes 15min
%% before it can report 'true')
is_synced_dirty() ->
    S = gen_server:call(?SERVER_NAME, get_server_state),
    is_step_10_seconds_ago(S#st.clock_stepped_since_start) orelse is_synced(S).

start(Verbosity) when is_integer(Verbosity) ->
    gen_server:start({local, ?SERVER_NAME}, ?MODULE, Verbosity, []).
stop() ->
    gen_server:call(?SERVER_NAME, stop).
restart_ntpd() ->
    gen_server:call(?SERVER_NAME, restart_ntpd).
print_ntp_state(Assoc) ->
    gen_server:call(?SERVER_NAME, {print_ntp_state, Assoc}).
print_traps() ->
    print_traps(2).
print_traps(Lvl) ->
    Packets = gen_server:call(?SERVER_NAME, get_trap_packets),
    comsaNtpUtil:decode_and_print_trap_packets(Lvl, Packets).
print_traps(Lvl, Fd) ->
    exit_if_simulated(),
    Packets = gen_server:call(?SERVER_NAME, get_trap_packets),
    comsaNtpUtil:decode_and_print_trap_packets(Lvl, Packets, Fd).

% 0 normal mostly no printing, 1 print errors and some stuff, 2 print useful
% >3 print mostly everything + decode traps
set_verbosity_level(Level) when is_integer(Level) ->
    gen_server:call(?SERVER_NAME, {set_verbosity_level, Level}).
print_server_state() ->
    S = gen_server:call(?SERVER_NAME, get_server_state),
    do_print_server_state(S).
print_server_state(Fd) ->
    exit_if_simulated(),
    S = gen_server:call(?SERVER_NAME, get_server_state),
    try do_print_server_state(S, Fd)
    catch _ErrClass : _ErrReason ->
	    io:format(Fd, "Server state: ~p~n", [S])
    end.

reread_ntpd_status() ->
    ?MODULE ! {timeout, refresh_after_trap}.

test_write_config() ->
    gen_server:call(?SERVER_NAME, test_write_config).

oot_new_data(Prop_list) ->
    %% There is already a printout from inside the gen_server.
    %%sysInitI:info_msg(
    %%  "~p: OOT - got new parameters  ~p~n",
    %%  [?MODULE, Prop_list]),
    %check that this is relevant for us to avoid restarting if not needed
    case lists:filter(
	    fun ({access_point_address, _}) ->  true;
		({oap_namespace, _}) -> true;
		(_) -> false
	    end, Prop_list) of
	[] -> ok;
	Relevant_props ->
	    gen_server:cast(?SERVER_NAME, {oot_new_data, Relevant_props})
    end.

start_ntp() ->
    gen_server:cast(?SERVER_NAME, start_ntp).


%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
init(Verbosity) ->
    timestep_init(),
    case clhI:mp_role() of
	core ->
	    case sysInitI:restart_type() of
		local ->
		    %%start after process crash - skip any chance of autonomous restart
		    %%also - this should give clock alarm more or less immidiately
		    S = #st{server_state = running,
			    verbosity    = Verbosity,
			    appm_up      = true},
		    conditional_report(S, 0,
				       "Going straight into running-state - no restart attempted."),
		    try_init(S);
		_ ->
		    I_timer = erlang:send_after(?MAX_INITIAL_STATE_TIME, self(),
						{timeout, leave_initial_state}),
		    S = #st{server_state = initial,
			    initial_tref = I_timer},
		    try_init(S)
	    end;
	regular ->
	    check_if_time_on_reg_needs_to_be_updated(),
	    {ok, #st{}}
    end.


%% if we end up doing retry_start (most likely) this is were we retry from
try_init(#st{appm_up = false} = S) ->
    sysInitI:info_msg(
      "~p: APPM not yet ready - retry in ~p ms~n",
      [?MODULE, ?INITIAL_RETRY_TIME]),
    erlang:send_after(?INITIAL_RETRY_TIME, self(), retry_start),
    {ok, S};
try_init(#st{} = S) ->
    case try_register_in_oot() of
	{ok, Ns} ->
	    sysInitI:info_msg(
	      "~p:  we have everything needed - get running ~n",
	      [?MODULE]),
	    %% we have everything needed - get running
	    Aht = refresh_timer(refresh_alarms,
			  	?ALARM_HOLDOFF_TIME,
				undefined),
	    New_s = load_state_data(S#st{nns = Ns,
					 alarm_ho_tref = Aht}),
	    Last_s = open_trap_port(New_s),
	    create_config_file(Last_s),
	    {ok, run_ntp_daemon(Last_s)};
	registered ->
	    %we are registered i OOT, just wait for NS to be set
	    sysInitI:info_msg(
		"~p: registered in OOT - waiting for Net namespace~n",
		[?MODULE]),
	    New_s = load_state_data(S),
	    {ok, New_s};
	{error, Result} ->
	    %registration failed, start timer that will get us back to here
	    sysInitI:info_msg(
		"~p: OOT not yet ready[~p] - retry in ~p ms~n",
		[?MODULE, Result, ?INITIAL_RETRY_TIME]),
	    erlang:send_after(?INITIAL_RETRY_TIME, self(), retry_start),
	    {ok, S}
    end.

load_state_data(#st{} = S) ->
    mnesia:subscribe({table, ntpServer, simple}),
    {Ntp_servers, Unresolved} = get_ntp_servers(),
    {ok, Control_script} = find_control_script(?CONTROL_SCRIPT),
    Volatile_dir = get_volatile_dir_HERE(),
    Oam_ip = ootI:get_oap_ip_addr(),
    {Oam_ip_family, Loopback} = ip_family(Oam_ip),
    S#st{ntp_servers = Ntp_servers,
	 ntp_unresolved_servers = Unresolved,
	 oam_ip = Oam_ip,
	 oam_ip_family = Oam_ip_family,
	 loopback = Loopback,
         freq_offset = ?DEFAULT_DRIFT,
	 config_file_name = filename:join([Volatile_dir, "ntp.conf"]),
	 ctrl_file_name = Control_script,
	 pid_file_name = filename:join([Volatile_dir, "ntp.pid"])}.

open_trap_port(#st{sock = Old_sock,
		   oam_ip_family = Family,
		   loopback = Lb} = S) ->
    case Old_sock of
	undefined -> ok;
	Old_sock ->
	    catch gen_udp:close(Old_sock)
    end,
    {ok, Sock} =
	gen_udp:open(0, [{ip, Lb}, binary, Family] ++ ns_to_opt(S)),
    {ok, Udp_port} = inet:port(Sock),
    S#st{sock = Sock, udp_portno = Udp_port}.

check_if_time_on_reg_needs_to_be_updated() ->
    %%see if core MP is synced
    %%if yes, get the time from core
    [ErlangNode] = clhI:erlang_nodes(active),
    case rpc:call(ErlangNode, ?MODULE, is_synced, []) of
	true ->
	    CoreTime = rpc:call(ErlangNode, os, timestamp, []),
	    Diff = us_diff(CoreTime, os:timestamp()),
	    case abs(Diff) of
		A_diff when A_diff > 1000 -> % 1 sec
		    error_logger:info_msg(
		      "~p: NTP: clock diff, core and regular MP system time differ.~n"
		      "\t\tcore time is ~s of regular time.~n",
		      [?MODULE, comsaNtpUtil:us_to_print_string(Diff)]),
		    {{Y, M, D}, {Hour,Minute,Second}} = calendar:now_to_universal_time(CoreTime),
		    Date = lists:flatten(io_lib:format("~w~2.2.0w~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
						       [Y, M, D, Hour, Minute, Second])),
		    os:cmd("sudo date --set="++"\""++Date++"\"");
		_ ->
		    ok
	    end,
	    erlang:send_after(24*60*60*1000, self(), {timeout, check_core_sync});
	_Else ->
	    %%the core is not in sync or the rpc has failed, check again in 5 (?) min.
	    erlang:send_after(5*60*1000, self(), {timeout, check_core_sync})
    end.

handle_call(stop, _From, S) ->
    {stop, {shutdown, "Stopped by command"}, ok, S};
handle_call(restart_ntpd, _From, #st{} = S) ->
    create_config_file(S),
    New_s = restart_ntp_daemon(S),
    {reply, ok, New_s#st{update_monitor = undefined}};
handle_call({get_ntp_state, Assoc}, From, S) ->
    do_print_ntp_state(S, Assoc),
    {noreply, S#st{reply_here = From}};
handle_call({print_ntp_state, Assoc}, _From, S) ->
    do_print_ntp_state(S, Assoc),
    {reply, ok, S};
handle_call(get_trap_packets, _From,
		#st{trap_packets = Packs} = S) ->
    {reply, Packs, S};
handle_call({set_verbosity_level, Level}, _From,
		#st{verbosity = Old_verb} = S) ->
    {reply, {"previous verbosity level", Old_verb}, S#st{verbosity = Level}};
handle_call(get_server_state, _From, S) ->
    {reply, S, S};
handle_call({subscribe_timestep, Fun}, _From, #st{timestep_funs = Funs} = S) ->
    {reply, ok, S#st{timestep_funs = [Fun | Funs]}};
handle_call(test_write_config,  _From, #st{config_file_name = Cf} = S) ->
    create_config_file(S#st{config_file_name = Cf ++ ".test_write_config"}),
    {reply, ok, S};
handle_call(Req, _From, S) ->
    conditional_report(S, 1, "Unhandled call, request: ~p", [Req]),
    {reply, ok, S}.

handle_cast({oot_new_data, Prop_list},
	    #st{oam_ip = Old_ip, nns = Old_ns} = S) ->
    sysInitI:info_msg("~p: OOT - oot_new_data: ~p~n", [?MODULE, Prop_list]),
    New_ip = proplists:get_value(access_point_address, Prop_list, Old_ip),
    New_ns = proplists:get_value(oap_namespace, Prop_list, Old_ns),
    case {New_ip, New_ns} of
	{Old_ip, Old_ns} -> %no change
	    {noreply, S};
	{[], _} -> %oamAccessPoint not set
	    Aht = refresh_timer(refresh_alarms,
				?ALARM_HOLDOFF_TIME,
				S#st.alarm_ho_tref),
	    New_s = S#st{oam_ip = New_ip,
			 oam_ip_family = inet,
			 loopback = ?LBI,
			 nns = sysInitI:get_lmt_ns(),
			 alarm_ho_tref = Aht},
	    Last_s = open_trap_port(New_s),
	    create_config_file(Last_s),
	    {noreply, restart_ntp_daemon(Last_s)};
	_ ->
	    Aht = refresh_timer(refresh_alarms,
				?ALARM_HOLDOFF_TIME,
				S#st.alarm_ho_tref),
	    %% it happens we get a new OamIP but the NS is reported separately,
	    %% to ensure it is not 'undefined' from here on, read it explicitly
	    Safe_ns =
		case ootI:get_oap_namespace() of
		    {ok, Actual_nss} -> Actual_nss;
		    _ -> <<>>
		end,
	    {Oam_ip_family, Loopback} = ip_family(New_ip),
	    New_s = S#st{oam_ip = New_ip, oam_ip_family = Oam_ip_family,
			 loopback = Loopback, nns = Safe_ns,
			 alarm_ho_tref = Aht},
	    Last_s = open_trap_port(New_s),
	    create_config_file(Last_s),
	    {noreply, restart_ntp_daemon(Last_s)}
    end;
%% Invoked from comsaServer's activate start phase, to be sure that appm is started
handle_cast(start_ntp, S) ->
    {noreply, S#st{appm_up = true}};
handle_cast({ntp_exit, Appm_id}, S) ->
    conditional_report(S, 1,
	"Port program (NTP) exit [old:~p, exiting:~p] - restarting",
	[S#st.ntp_pgm_id, Appm_id]),
    Aht = refresh_timer(refresh_alarms,
			?ALARM_HOLDOFF_TIME,
			S#st.alarm_ho_tref),
    {noreply, restart_ntp_daemon(S#st{alarm_ho_tref = Aht})};
handle_cast(Req, S) ->
    conditional_report(S, 1, "Unhandled cast, request: ~p", [Req]),
    {noreply, S}.

handle_info(retry_start, S) ->
    {ok, New_s} = try_init(S),
    {noreply, New_s};
handle_info({udp, Sock, Lb, ?NTP_PORT, Data},
	    #st{sock = Sock, loopback = Lb} = S) ->
    % whenever there is a ANY UDP (most likely a TRAP from NTP), check for
    % ts-now-diff, and raise/cease -alarms accordingly
    New_state = process_calendar_clock_misaligned_alarm(S),
    {noreply, handle_udp(Data, New_state)};
handle_info({timeout, Type}, S) ->
    {noreply, handle_timeout(Type, S)};
%spawned update process returns current ntp-state
handle_info({'DOWN', Mref, process, Pid, {ok, _, Is_sync, Peers} = _Result},
		#st{update_monitor = {Pid, Mref}} = S) ->
    New_ntp_state = {Is_sync, Peers},
    Next_s = process_ntp_state_change(S, S#st.ntp_state, New_ntp_state),
    New_s = do_polling_if_any_nok(Next_s#st{ntp_state = New_ntp_state}),
    {noreply, New_s#st{update_monitor = undefined}};
%spawned update process failed
handle_info({'DOWN', Mref, process, Pid, Failure},
		#st{update_monitor = {Pid, Mref}} = S) ->
    conditional_report(S, 1, "Failure in update process - WHAT TO DO: ~p",
	[Failure]),
    {noreply, S#st{update_monitor = undefined}};
handle_info({mnesia_table_event, _}, #st{ntp_pgm_id = undefined} = S) ->
    %daemon is not yet running, probably waiting for OOT
    {noreply, S};
handle_info({mnesia_table_event, Event}, S) ->
    New_s = handle_table_event(Event, S),
    Alarm_holdoff_timer = refresh_timer(refresh_alarms,
					?ALARM_HOLDOFF_TIME,
					New_s#st.alarm_ho_tref),
    {noreply, New_s#st{alarm_ho_tref = Alarm_holdoff_timer}};
handle_info(Req, S) ->
    conditional_report(S, 1, "Unhandled info, request: ~p", [Req]),
    {noreply, S}.

terminate(_Reason, S) ->
    stop_ntp_daemon(S),
    ok.

code_change(_Old_version, S, _Extra) ->
    {ok, S}.

format_status(_Opt, [_Pdict, #st{trap_packets = {N, _}} = S]) ->
     [{data, [{"State", S#st{trap_packets = "Packets in queue: " ++
					    integer_to_list(N)}}]}].

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

target_or_vrcs() ->
    case sysEnv:rcs_mode_2() of
	simulated -> false;
	_ -> true
    end.


spawn_update_process(S) ->
    spawn_monitor(fun() -> return_ntp_state(S) end).

create_config_file(#st{config_file_name = Ntpconf,
			ntp_servers =  Ntp_servers,
			udp_portno =  Udp_port,
			oam_ip = Ip_string,
			oam_ip_family = Family,
			loopback = Lb,
			freq_offset = Drift,
			nns = Ns}) ->
    %% Ns really shouldn't be 'undefined' but it slipped through before, this
    %% at least, will prevent a crash.
    Safe_ns =
	case Ns of
	    undefined -> <<>>;
	    Ns -> Ns
	end,
    Family_arg = case  Family of inet -> "-4"; inet6 -> "-6" end,
    Address_len = case Family of inet -> "32"; inet6 -> "128" end,
    Port = integer_to_list(Udp_port),
    Conf =
	["# ntp.conf generated by ", atom_to_list(?MODULE), "\n",
	%%"interface listen ", ip_to_string(?LBI), "/32\n",
	case Ip_string of
	    [] ->
		[];
	    Ip_string ->
		["interface listen ", Ip_string, "/", Address_len, "\n",
		 "# Network Namespace: <<", binary_to_list(Safe_ns), ">>\n"]
	end,
	[["server ", Family_arg, " ", ip_to_string(Ip), " iburst version 4\n"] ||
		{_, Ip} <- Ntp_servers],
	"# disable kernel\n"
	"# disable ntp\n"
	"restrict ", Family_arg, " default nomodify nopeer notrap noquery version\n"
	"restrict ", Family_arg, " ", ip_to_string(Lb), "\n"
	"# hardcode initial drift to 0.0ppm because starting with an initial\n"
	"# frequency is faster and our x-tal is really, really  good\n"
	"tinker freq ", float_to_list(Drift, [{decimals, 2}]), "\n"
	"logconfig=all\n",   %HERE
	"trap ", ip_to_string(Lb)," port ", Port, " interface ",
		ip_to_string(Lb), "\n",
    "# adjust slew to avoid leap second problems (HV53411)\n",
    "# only slew to avoid clock stepping back (HV97497)\n",
    "tinker stepback 3\n"
	],
    ok = file:write_file(Ntpconf, Conf).

restart_ntp_daemon(S) ->
    run_ntp_daemon(stop_ntp_daemon(S)).

run_ntp_daemon(#st{config_file_name = Ntp_conf,
		oam_ip_family = Family,
		ctrl_file_name = Prog,
		pid_file_name = Pid_file,
		nns = Ns} = S) ->
    Family_arg = case  Family of inet -> "-4"; inet6 -> "-6" end,
    Ntp_start_strings =
	[?SUDO, Prog,
	 "restart", Family_arg, "-u", "ntp:ntp", "-g", "-n",
	 "-c", Ntp_conf,
	 "-p", Pid_file],
    case appm_start_ntp(S, Ntp_start_strings, Ns) of
	{ok, Appm_id} ->
	    S#st{ntp_pgm_id = Appm_id,
		 ntpd_started_at = erlang:timestamp()};
	{error, Reason} -> %FIXME HERE what to do now
	    sysInitI:error_msg("Failed to run ntpd: ~p~n", [Reason]),
	    S#st{ntp_pgm_id = undefined}
    end.

appm_start_ntp(S, Ntp_start_strings, Ns) ->
    try appmServer:start_internal_lm([{name, "comsa running ntpd"},
				      {mfa, {?MODULE, ntp_exit,[]}},
				      {args, Ntp_start_strings},
				      {ns, Ns},
				      {owner, self()},
				      {autoclean, true}]) of
	Whatever ->
	    conditional_report(S, 2, "Result of appm::start_internal_lm: ~p",
			       [Whatever]),
	    Whatever
    catch
	error:undef ->
	    case appmPghServer:req_spawn_pgm([{args, Ntp_start_strings},
					      {ns, Ns}]) of
		{ok, Pgh_res} ->
		    Pgh_id = proplists:get_value(pgm_id, Pgh_res),
		    {ok, Pgh_id};
	  	Badness ->
		    Badness
	    end
    end.
stop_ntp_daemon(#st{ntp_pgm_id = undefined} = S) ->
    S;
stop_ntp_daemon(#st{ntp_pgm_id = Pgm_id} = S) ->
    Res =
	try appmServer:stop_internal_lm(Pgm_id) of
	    _ ->
		ok
	catch
	    error:undef ->
		appmPghServer:req_destroy_pgm([{pgm_id, [Pgm_id]}])
	end,
    conditional_report(S, 2, "Result of destroy_pgm: ~p", [Res]),
    S#st{ntp_pgm_id = undefined}.

refresh_timer(Name, Time, undefined) ->
    erlang:send_after(Time, self(), {timeout, Name});
refresh_timer(Name, Time, Ref) ->
    case erlang:cancel_timer(Ref) of
	false ->
	    receive
		{timeout, Name} -> ok
	    after
		0 -> ok
	    end;
	_ -> ok
    end,
    erlang:send_after(Time, self(), {timeout, Name}).

add_trap_packet({Size, Q}, Tp) when Size >= ?MAX_TRAP_PACKETS ->
    New_q = queue:drop(Q),
    {Size, queue:in({os:timestamp(), Tp}, New_q)};
add_trap_packet({Size, Q}, Tp) ->
    {Size + 1, queue:in({os:timestamp(), Tp}, Q)}.

check_for_clock_step(S, Trap) ->
    case comsaNtpUtil:is_clock_step(Trap)  of
	true ->
	    %% Have to give clock update some more time; The OS clock has not
	    %% always been updated when we receive this trap. (One out of
	    %% twenty times has shown old time from os:system_time/0 here.)
	    timer:sleep(667),
	    timestep_update(),
	    call_timestep_funs(S),
	    security_log_timestep(ts_now_diff()),
	    Diff = ts_now_diff(),
	    sysInitI:info_msg(
			"~p: NTP: clock stepped, real time is ~s "
			"of system time~n",
			[?MODULE, comsaNtpUtil:us_to_print_string(Diff)]),
	    S#st{clock_stepped_since_start = erlang:timestamp()};
	false ->
	    S
    end.

timestep_init() ->
    ets:new(comsaNtpTime, [named_table, public, ordered_set, {keypos, 1}]),
    OffsetStart = os:system_time() - erlang:monotonic_time(),
    ets:insert(comsaNtpTime, #comsaNtpTime{offset_start = OffsetStart,
					   offset_latestStep = OffsetStart}).
timestep_update() ->
    [#comsaNtpTime{offset_start = OffsetS,
		   offset_latestStep = OffsetL} = CNT] =
	ets:lookup(comsaNtpTime, comsaNtpTime),
    NewOffsetL = os:system_time() - erlang:monotonic_time(),
    StepStart = NewOffsetL - OffsetS,
    StepLatest = NewOffsetL - OffsetL,
    ets:insert(comsaNtpTime, CNT#comsaNtpTime{offset_latestStep = NewOffsetL,
					      step_start = StepStart,
					      step_latest = StepLatest}).

call_timestep_funs(#st{timestep_funs = Funs}) ->
    sysInitI:info_msg(
	"~p: Effectuating timestep subscriptions and restart tracing: ~b~n",
	[?MODULE, length(Funs)]),
    os:cmd("tta"), %restart trace subsystem in EE
    [try F() catch _:_ -> ok end || F <- Funs].

process_ntp_state_change(S, Old_ntp_state, {System, _} = New_ntp_state) ->
    report_ntp_state_change(S#st.verbosity, Old_ntp_state, New_ntp_state),
    process_ntp_alarms(S, New_ntp_state),
    case {S#st.server_state, System} of
	{initial, ok} ->
	    conditional_report(S, 1,
		"System synchronized, leaving initial state"),
		Diff = ts_now_diff(),
	    sysInitI:info_msg(
			"~p: NTP: clock synchronized, real time is ~s "
			"of system time~n",
			[?MODULE, comsaNtpUtil:us_to_print_string(Diff)]),
	    catch erlang:cancel_timer(S#st.initial_tref),
	    S#st{server_state = running, initial_tref = undefined};
	_ ->
	    S
    end.

restart_cold(Diff) ->
    sysInitI:info_msg(
	"~p: NTP: clock set, system time and real time differ.~n"
	"\t\treal time is ~s of system time.~n"
	"Initiating cold restart~n",
	[?MODULE, comsaNtpUtil:us_to_print_string(Diff)]),
    appmI:restart_node(cold, ?AVLI_CAUSE_ClockUpdateExceededMaxDiff).

-spec process_ntp_alarms(st(), {ok | nok, list()}) -> st().
process_ntp_alarms(#st{alarm_ho_tref = undefined} = S, {_System, ServerList}) ->
	NewState = process_ntp_server_unavailable_alarms(S, ServerList),
	process_ntp_alarm(NewState, {?ALL_NTP_ALARM, ?SYSM_DN, minor}, 
			  all_ntp_servers_unavailable(NewState, ServerList), 
			  ?ALL_NTP_SERVERS_UNAVAILABLE_ALARM_TIMES);

process_ntp_alarms(S, _) ->
    S.

-spec process_ntp_server_unavailable_alarms(st(), list()) -> st().
process_ntp_server_unavailable_alarms(#st{alarm_ho_tref = undefined} = S, []) ->
    S;
process_ntp_server_unavailable_alarms(#st{ntp_servers = Ntp_servers, alarm_ho_tref = undefined} = S, 
				      [{_Assoc_id, Ip_tuple, _Ntp_state, Reach} | ServerList]) ->
    NewState =
	case find_server(Ip_tuple, Ntp_servers) of
	    {ok, Object} ->
		process_ntp_alarm(S, {?NTP_ALARM, object_to_dn(Object), warning}, reach_to_state(Reach), 
				  ?NTP_SERVER_UNAVAILABLE_ALARM_TIMES);
	    _ ->
		sysInitI:error_msg(
		  "Failed to map ntp server IP to MomId~n"
		  "DNS name resolution may have changed~n"
		  "Ip address configured in NTP: ~p~n", [Ip_tuple]),
		S
	end,
    process_ntp_server_unavailable_alarms(NewState, ServerList);

process_ntp_server_unavailable_alarms(S, _) ->
    S.

-spec process_calendar_clock_misaligned_alarm(st()) -> st().
process_calendar_clock_misaligned_alarm(S) ->
    New_s = process_ntp_alarm(S, {?CALENDAR_CLOCK_ALARM, ?SYSM_DN, warning}, 
			      check_for_time_offset(), ?CALENDAR_CLOCK_MISALIGNED_ALARM_TIMES),
    %% if the alarm was raised by previous command, start cease timer, else do nothing
    process_ntp_alarm(New_s, {?CALENDAR_CLOCK_ALARM, ?SYSM_DN, warning}, ok, 
		      ?CALENDAR_CLOCK_MISALIGNED_ALARM_TIMES).

-spec process_ntp_alarm(st(), alarm(), ok | nok, {non_neg_integer(), non_neg_integer()}) -> st().
process_ntp_alarm(S, {Type, Object, _Severity} = Alarm, Ok_Nok, {Raise_time, Cease_time}) ->
    case {is_alarm_raised(Type, Object), is_alarm_timer_started(S, Alarm), Ok_Nok} of
	%% if alarm is not raised nor timer started and status is nok -> start timer for raise
	{false, false, nok} ->
	    start_alarm_timer(S, {raise_alarm, Alarm}, Raise_time);
	%% if alarm is not raised and timer is started (for raise), but status is ok -> cancel timer
	{false, true, ok} ->
	    cancel_alarm_timer(S, Alarm);
	%% if alarm is raised and timer is not started but status is ok -> start timer for cease
	{true, false, ok} ->
	    start_alarm_timer(S, {cease_alarm, Alarm}, Cease_time);
	%% if alarm is raised and timer is started (for cease) but status is nok -> cancel timer
	{true, true, nok} ->
	    cancel_alarm_timer(S, Alarm);
	_ ->
	    S
    end.

%% if alarm is raised for all NTP servers, Calendar Clock All NTP Servers alarm should be raised
-spec all_ntp_servers_unavailable(st(), list()) -> ok | nok.
all_ntp_servers_unavailable(#st{alarm_ho_tref = undefined} = S, ServerList) ->
    case lists:member(false, [is_alarm_raised(S, Server) || Server <- ServerList]) of
	true -> ok;
	false -> nok
    end;
all_ntp_servers_unavailable(_, _) ->
    ok.

-spec is_alarm_raised (st(), tuple()) -> false | true;
		      (atom(), list()) -> false | true.
is_alarm_raised(#st{ntp_servers = Ntp_servers}, {_Assoc_id, Ip_tuple, _Ntp_state, _Reach} = 
		    _Server) ->
    case find_server(Ip_tuple, Ntp_servers) of
	{ok, Object} ->
	    is_alarm_raised(?NTP_ALARM, object_to_dn(Object));
	_ ->
	    sysInitI:error_msg(
	      "Failed to map ntp server IP to MomId~n"
	      "DNS name resolution may have changed~n"
	      "Ip address configured in NTP: ~p~n", [Ip_tuple]),
	    undefined
    end;

is_alarm_raised(Type, Object) ->
    case mnesia:dirty_read(?MNESIA_RAM_TABLE, {Type, Object}) of 
	[] -> false;
	[_Obj] -> true
    end.

-spec is_alarm_timer_started (st(), alarm()) -> false | true.
is_alarm_timer_started(#st{ntp_alarm_timers = Alarm_timers}, Alarm) ->
    case lists:keysearch(Alarm, 2, Alarm_timers) of
	{value, _} -> true;
	false -> false
    end.

%% if raise or cease time is 0, the alarm is raised/ceased instantly (no timer)
-spec start_alarm_timer(st(), {raise_alarm | cease_alarm, alarm()}, non_neg_integer()) -> st().
start_alarm_timer(S, {RaiseOrCease, {Type, Object, Severity}}, 0) ->
    case RaiseOrCease of
	raise_alarm ->
	    alarm(S, nok, Type, Object, Severity);
	cease_alarm ->
	    alarm(S, ok, Type, Object, Severity)
    end,
    S;

start_alarm_timer(#st{ntp_alarm_timers = Alarm_timers} = S, 
		  {RaiseOrCease, {Type, Object, _Severity} = Alarm} = TimerMsg, Time) ->
    conditional_report(S, 1, "Starting timer for alarm for: ~p on object: ~p with ~p time: ~p~n",
		       [Type, Object, RaiseOrCease, Time]),
    TimerRef = refresh_timer(TimerMsg, Time, undefined),
    S#st{ntp_alarm_timers = [{TimerRef, Alarm} | Alarm_timers]}.

-spec cancel_alarm_timer(st(), alarm()) -> st().
cancel_alarm_timer(#st{ntp_alarm_timers = Alarm_timers} = S, {Type, Object, _Severity} = Alarm) ->
    case lists:keysearch(Alarm, 2, Alarm_timers) of
	{value, {TimerRef, Alarm}} -> 
	    conditional_report(S, 1, "Canceling timer for alarm for: ~p on object: ~p~n",
			       [Type, Object]),
	    erlang:cancel_timer(TimerRef),
	    S#st{ntp_alarm_timers = lists:keydelete(Alarm, 2, Alarm_timers)};
	false -> S
    end.

-spec reach_to_state(reach_ok | reach_nok) -> ok | nok.
reach_to_state(Reach) ->
    case Reach of
	reach_ok -> ok;
	reach_nok -> nok
    end.

%this is tricky (and of cause not a great idea). The ntp-daemon  does its own
%name resolution, not only at start up but presumably until it gets a
%resolution. Also what comes in from COM is a string, it may be either a name
%or an IP (indeed, the IP can also be something like "01.03.99.3")
find_server(_, []) -> nok;
find_server(Ip_tuple, [{Object, Ip_string} | T]) ->
    case inet:parse_address(Ip_string) of
	{ok, Ip_tuple} -> {ok, Object};
	{ok, _} ->
	    find_server(Ip_tuple, T);
	{error, einval} -> %try to resolve it
	    %this is now namespaced, however, it doesn't work if the host
	    %should happen to be multihomed and the IP selected by ntpd isn't
	    %the first one (getaddr will simply return the first address only)
	    case ootI:getaddr(Ip_string) of
		{ok, Ip_tuple} -> {ok, Object};
		{ok, _}		-> find_server(Ip_tuple, T);
		_		-> find_server(Ip_tuple, T)
	    end
    end.

report_ntp_state_change(Verbosity, _, _) when Verbosity < 1 -> ok;
report_ntp_state_change(Verbosity, Old, New) ->
    io:format("~n*** ~s ***~n", [comsaNtpUtil:mk_ds()]),
    report_ntp_sc(Verbosity, Old, New).

report_ntp_sc(_, undefined, {System, Peers}) ->
    report("initial state, system sync is: ~p", [System]),
    [report("\tpeer: ~p state: ~p reach: ~p", [Pi, Ps, Pr]) ||
	{_, Pi, Ps, Pr} <- Peers];
report_ntp_sc(_, Same, Same) ->
    report("no overall state change");
report_ntp_sc(_, _Old, {System, Peers}) ->
    report("state change, system sync is: ~p", [System]),
    [report("\tpeer: ~p state: ~p reach: ~p", [Pi, Ps, Pr]) ||
	{_, Pi, Ps, Pr} <- Peers].

report(Info) ->
    report(Info, []).
report(Format, Params) ->
    io:format("~s" ++ Format ++ "~n",
	["NTP event: " | Params]).

conditional_report(S, Lvl, Format) ->
    conditional_report(S, Lvl, Format, []).
conditional_report(#st{verbosity = Vlvl}, Lvl, _, _) when Lvl > Vlvl ->
    ok;
conditional_report(_, _, Format, Args) ->
    io:format("~n*** ~s ***~n", [comsaNtpUtil:mk_ds()]),
    report(Format, Args).

find_control_script(File) ->
    %this now relies on the command being in $PATH, sudoers must have been
    %set up correctly
    case os:find_executable(File) of
	File_path when is_list(File_path) ->
	    {ok, File_path};
	false ->
	    nok
    end.

get_ntp_servers() ->
    try ets:tab2list(ntpServer) of
	Ntp_servers ->
	    get_ntp_servers(Ntp_servers)
    catch
	_:_ ->
	    {[],[]}
    end.
get_ntp_servers(Ntp_servers) ->
    lists:foldl(
	fun(#ntpServer{administrativeState = ?BasicAdmState_UNLOCKED,
			ntpServerId = Id,
			serverAddress = Ip_or_host},
		{All, Unresolved}) ->
		    Item = {Id, Ip_or_host},
		    case do_resolve(Ip_or_host) of
			{ok, _Ip_string} ->
			    {[Item | All], Unresolved};
			error ->
			    {[Item | All], [Item | Unresolved]}
		    end;
	    ({Id, Ip_or_host}, {All, Unresolved}) ->
		    Item = {Id, Ip_or_host},
		    case do_resolve(Ip_or_host) of
			{ok, _Ip_string} ->
			    {[Item | All], Unresolved};
			error ->
			    {[Item | All], [Item | Unresolved]}
		    end;
	    (_, Acc) ->
		    Acc end , {[],[]}, Ntp_servers).

do_resolve(Ip_or_host) ->
    case ootI:getaddr(Ip_or_host) of
	{ok, Ip} ->
	    {ok, ip_to_string(Ip)};
	{error, _} ->
	    error
    end.

%Opcode: 1, matching Sequence -> response to query
handle_udp(<<_:2, 4:3, 6:3, 1:1, 0:1, _M:1, ?OP_CODE_RSTATUS:5, _/binary>> = U,
	#st{reassembly = Reass, reply_here = Reply} = S) ->
    conditional_report(S, 1, "Got a response packet(Op: ~p)",
	[?OP_CODE_RSTATUS]),
    try
	case comsaNtpUtil:reassemble(Reass, U) of
	    {more, New_reass} ->
		S#st{reassembly = New_reass};
	    {done, Read_status} ->
		case Reply of
		    undefined ->
			comsaNtpUtil:decode_packet(3, os:timestamp(), Read_status),
			comsaNtpUtil:decode_and_print_variables(3, Read_status);
		    Reply ->
			gen_server:reply(Reply, Read_status)
		end,
		bump_seq(S#st{reassembly = undefined, reply_here = undefined})
	end
    catch
	A:B ->
	    conditional_report(S, 1, "Crash in reassemble/decode/print: ~p:~p",
		[A, B]),
	    bump_seq(S#st{reassembly = undefined, reply_here = undefined})
    end;
%Opcode: 7, trap message while server_state:ANY (possibly printing)
handle_udp(<<_:2, 4:3, 6:3, 1:1, 0:1, _M:1, ?OP_CODE_TRAP:5, _/binary>> = U,
 	#st{server_state = Server_state, verbosity = Verb,
 	reassembly = Reass, trap_packets = Tps} = S) ->
    case comsaNtpUtil:reassemble(Reass, U) of
 	{more, New_reass} ->
 	    S#st{reassembly = New_reass,
 			trap_packets = add_trap_packet(Tps, U)};
 	{done, Trap} ->
 	    comsaNtpUtil:decode_packet(Verb, os:timestamp(), Trap),
 	    comsaNtpUtil:decode_and_print_variables(Verb, Trap),
 	    New_s =
 		case Server_state of
 		    initial ->
 		        check_for_clock_step(S, Trap);
 		    _ ->
 		        case send_poll_ntpd(Trap) of
 		            yes -> erlang:send_after(?POLL_NTPD_DELAY, cch_service, poll_ntpd);
 		            _ -> ok
 		        end,
 		        S
 		end,
 	    New_s#st{reassembly = undefined,
 			trap_ho_tref = refresh_timer(refresh_after_trap,
 						?TRAP_HOLDOFF_TIME,
 						New_s#st.trap_ho_tref),
 			trap_packets = add_trap_packet(Tps, U)};
 	error ->
 	    S#st{reassembly = undefined,
 			trap_ho_tref = refresh_timer(refresh_after_trap,
 						?TRAP_HOLDOFF_TIME,
 						S#st.trap_ho_tref),
 			trap_packets = add_trap_packet(Tps, U)}
     end;
%Opcode: 1, matching Sequence, error -> response to bad query
handle_udp(<<_:2, 4:3, 6:3, 1:1, 1:1, _:1, _:5, Seq:16, _Status:16, _Ass_id:16,
	_Offset:16, _Len:16, Rest/binary>>,
	#st{seq_no = Seq} = S) ->
    conditional_report(S, 1,
	"Error response returned from ntpd (bad query?), payload: ~p",
	[Rest]),
    bump_seq(S);
%unmatched UDP
handle_udp(<<_:2, V:3, 6:3, R:1, E:1, M:1, Op:5, Seq:16, Status:16, Ass_id:16,
	Offset:16, Length:16, Rest/binary>>,
	S) ->
    conditional_report(S, 1,
	"{udp WHAT, V: ~b, R: ~b, E: ~b, M: ~b, Opcode: ~b, "
	"Seq: ~b, Status: ~b, Ass_id: ~b, Offset: ~b, Length: ~b, "
	"size_rest: ~b, Rest:  ~p}",
	[V, R, E, M, Op, Seq, Status, Ass_id, Offset, Length,
	size(Rest), Rest]),
    conditional_report(S, 1, "Message: ~p", [binary:part(Rest, {0, Length})]),
    S;
handle_udp(_Udp, S) ->
    conditional_report(S, 1, "UNMATCHED UDP: ~p", [_Udp]),
    S.

handle_timeout(refresh_after_trap, #st{update_monitor = undefined} = S) ->
    conditional_report(S, 2, "Spawning process to check ntpd status~n"),
    S#st{update_monitor = spawn_update_process(S),
	   trap_ho_tref = undefined};
handle_timeout(refresh_after_trap, #st{update_monitor = _Running} = S) ->
    S#st{trap_ho_tref = undefined};
handle_timeout(refresh_alarms, S) ->
    conditional_report(S, 1, "alarm holdoff timer expired: ~s~n",
	[comsaNtpUtil:mk_ds()]),
    process_ntp_alarms(S#st{alarm_ho_tref = undefined}, S#st.ntp_state);

handle_timeout(leave_initial_state, #st{server_state = initial,
					trap_packets = Packets} = S) ->
    sysInitI:error_msg(
	"~p: NTP: initial supervision timer expired, time of day clock has "
	"not been set.~n Real time is ~s of system time.~n",
	[?MODULE, comsaNtpUtil:us_to_print_string(ts_now_diff())]),
    catch do_print_server_state(S),
    catch do_print_ntp_state(S, 0),
    catch comsaNtpUtil:decode_and_print_trap_packets(2, Packets),
    S#st{server_state = running,
	 initial_tref = undefined};
handle_timeout(leave_initial_state, S) ->
    conditional_report(S, 1, "Initial state timer expired"),
    S#st{initial_tref = undefined};
handle_timeout(check_core_sync, S) ->
    check_if_time_on_reg_needs_to_be_updated(),
    S;
handle_timeout({raise_alarm, Alarm}, S) ->
	handle_alarm_timeout(nok, Alarm, S);
handle_timeout({cease_alarm, Alarm}, S) ->
	handle_alarm_timeout(ok, Alarm, S).

handle_alarm_timeout(Ok_Nok, {Type, Object, Severity} = Alarm, 
		     #st{ntp_alarm_timers = Alarm_timers} = S) ->
    alarm(S, Ok_Nok, Type, Object, Severity),
    Alarm_holdoff_timer = refresh_timer(refresh_alarms,
					?ALARM_HOLDOFF_TIME,
					S#st.alarm_ho_tref),
    S#st{ntp_alarm_timers = lists:keydelete(Alarm, 2, Alarm_timers), 
	 alarm_ho_tref = Alarm_holdoff_timer}.

%server deleted from config
handle_table_event({delete, {ntpServer, Id}, _}, S) ->
    case lists:keytake(Id, 1, S#st.ntp_servers) of
	{value, Deleted, New_servers} ->
	    conditional_report(S, 2, "Deleted server: ~p", [Deleted]),
	    New_s = process_ntp_alarm(S#st{ntp_servers = New_servers}, 
				      {?NTP_ALARM, object_to_dn(Id), warning}, ok, 
				      ?NTP_SERVER_UNAVAILABLE_ALARM_TIMES),
	    create_config_file(New_s),
	    restart_ntp_daemon(New_s);
	false -> %not known here probably locked before
	    conditional_report(S, 2, "Deleted already locked server: ~p",
		[Id]),
	    S
    end;
%server added with unlocked state
handle_table_event({write,
		    #ntpServer{
		      administrativeState = ?BasicAdmState_UNLOCKED,
		      ntpServerId = Id,
		      serverAddress = Ip_or_host}, _},
		   #st{ntp_servers = Current_servers} = S) ->
    New_server = {Id, Ip_or_host},
    Maybe_existing = lists:keyfind(Id, 1, Current_servers),
    if
	Maybe_existing =:= New_server ->
	    conditional_report(S, 2, "User label changed?: ~p",
		[{Id, Ip_or_host}]),
	    S;
	true ->
	    conditional_report(S, 2, "Add server: ~p", [New_server]),
	    New_servers = lists:keystore(Id, 1, Current_servers, New_server),
	    {All, Unresolved} = get_ntp_servers(New_servers),
	    New_s = S#st{ntp_servers = All,
			ntp_unresolved_servers = Unresolved},
	    create_config_file(New_s),
	    restart_ntp_daemon(New_s)
    end;
%server added to config but not yet unlocked
handle_table_event({write,
			#ntpServer{
				ntpServerId = Id,
				serverAddress = _Ip_or_host}, _}, S) ->
    %not unlocked, treat it as a delete, but don't restart unless needed
    case lists:keytake(Id, 1, S#st.ntp_servers) of
	{value, Locked, New_servers} ->
	    conditional_report(S, 2, "Existing server locked: ~p",
			       [Locked]),
	    New_s = process_ntp_alarm(S#st{ntp_servers = New_servers}, 
				      {?NTP_ALARM, object_to_dn(Id), warning}, ok, 
				      ?NTP_SERVER_UNAVAILABLE_ALARM_TIMES),
	    create_config_file(New_s),
	    restart_ntp_daemon(New_s);
	false -> %not known here probably created but not unlocked (yet)
	    conditional_report(S, 2, "Added locked server: ~p", [Id]),
	    S
    end.

check_for_time_offset() ->
    Diff = ts_now_diff(),
    case abs(Diff) of
	A_diff when A_diff > ?INITIAL_MAX_NOW_DIFF ->
	    nok;
	_ ->
	    ok
    end.

alarm(S, On_off, Type, Object, Severity) ->
    case {On_off, mnesia:dirty_read(?MNESIA_RAM_TABLE, {Type, Object})} of
	{nok, []} ->
	    conditional_report(S, 1, "Raising alarm for: ~p on object: ~p~n",
				[Type, Object]),
	    ok = mnesia:dirty_write({?MNESIA_RAM_TABLE, {Type, Object},
			calendar:universal_time()}),
	    comsaI:send_alarm(Type, Severity, Object, "", []);
	{ok, []} ->
	    ok;
	{nok, [_Ob]} ->
	    ok;
	{ok, [_Ob]} ->
	    conditional_report(S, 1, "Ceasing alarm for: ~p on object: ~p~n",
				[Type, Object]),
	    ok = mnesia:dirty_delete(?MNESIA_RAM_TABLE, {Type, Object}),
	    comsaI:clear_alarm(Type, Object)
    end.

do_print_ntp_state(#st{sock = undefined}, _Assoc) ->
    sysInitI:info_msg("~p: Printing ntp state but socket is undefined~n",
		      [?MODULE]);
do_print_ntp_state(#st{sock = Sock, seq_no = Seq, loopback = Lb}, Assoc) ->
    Qp = comsaNtpUtil:mk_query_packet(Seq, Assoc),
    ok = gen_udp:send(Sock, Lb, ?NTP_PORT, Qp).

do_print_server_state(S) ->
    io:format("Servers current state (server_state): ~p~n",
	[S#st.server_state]),
    io:format("State interpreted from ntpd (ntp_state): ~p~n",
	[S#st.ntp_state]),
    io:format("IP Mom translation (ntp_servers): ~p~n", [S#st.ntp_servers]),
    io:format("(ntp_unresolved_servers): ~p~n", [S#st.ntp_unresolved_servers]),
    io:format("Contents of alarmtable: ~p~n",
	[catch ets:tab2list(?MNESIA_RAM_TABLE)]),
    io:format("UDP socket (sock): ~p~n", [S#st.sock]),
    io:format("UDP local port number (udp_portno): ~p~n", [S#st.udp_portno]),
    io:format("Pgm id of ntp-process (ntp_pgm_id): ~p~n", [S#st.ntp_pgm_id]),
    io:format("NTP daemon started at (ntpd_started_at): ~p~n",
	      [S#st.ntpd_started_at]),
    io:format("OaM IP address: ~p~n", [S#st.oam_ip]),
    io:format("OaM network namespace: ~p~n", [S#st.nns]),
    io:format("(config_file_name): ~p~n", [S#st.config_file_name]),
    io:format("(pid_file_name): ~p~n", [S#st.pid_file_name]),
    io:format("(ctrl_file_name): ~p~n", [S#st.ctrl_file_name]),
    io:format("Sequence number for mode 6 (seq_no): ~p~n", [S#st.seq_no]),
    Reass =
	case S#st.reassembly of
	    undefined -> undefined;
	    {Seq, Offset, Binary} ->
		{"Sequence: " ++ integer_to_list(Seq),
		"Current offset: " ++ integer_to_list(Offset),
		"Size of reassembly buffer: " ++ integer_to_list((Binary))}
	end,
    io:format("(reassembly): ~p~n", [Reass]),
    io:format("(trap_ho_tref): ~p [~s]~n", [S#st.trap_ho_tref,
	timer_info_to_string(S#st.trap_ho_tref)]),
    io:format("(alarm_ho_tref): ~p [~s]~n", [S#st.alarm_ho_tref,
	timer_info_to_string(S#st.alarm_ho_tref)]),
    io:format("(initial_tref): ~p [~s]~n", [S#st.initial_tref,
	timer_info_to_string(S#st.initial_tref)]),
    io:format("(update_monitor): ~p~n", [S#st.update_monitor]),
    io:format("Default frequency offset: ~p~n", [S#st.freq_offset]),
    {Trap_buf_len, _} = S#st.trap_packets,
    io:format("Number of trap packets in log-buffer(len(trap_packets)): ~p~n",
	      [Trap_buf_len]),
    io:format("Printing level (verbosity): ~p~n", [S#st.verbosity]),
    Now_diff = ts_now_diff(),
    io:format("Funs to call at timestep: ~p~n", [S#st.timestep_funs]),
    io:format("Clockstep (erlang:timestamp()|undefined) : ~p~n", [S#st.clock_stepped_since_start]),
    io:format("Time/now diff usec: ~p ~s~n",
	      [Now_diff, comsaNtpUtil:us_to_print_string(Now_diff)]),
    [io:format("Alarm timer: ~p, [~s]~n", [Alarm, timer_info_to_string(AlarmRef)]) || 
	    {AlarmRef, Alarm} <- S#st.ntp_alarm_timers],
    ok.

do_print_server_state(S, Fd) ->
    io:format(Fd, "Servers current state (server_state): ~p~n",
	      [S#st.server_state]),
    io:format(Fd, "State interpreted from ntpd (ntp_state): ~p~n",
	      [S#st.ntp_state]),
    io:format(Fd, "IP Mom translation (ntp_servers): ~p~n", [S#st.ntp_servers]),
    io:format(Fd, "(ntp_unresolved_servers): ~p~n", [S#st.ntp_unresolved_servers]),
    io:format(Fd, "Contents of alarmtable: ~p~n",
	      [catch ets:tab2list(?MNESIA_RAM_TABLE)]),
    io:format(Fd, "UDP socket (sock): ~p~n", [S#st.sock]),
    io:format(Fd, "UDP local port number (udp_portno): ~p~n", [S#st.udp_portno]),
    io:format(Fd, "Pgm id of ntp-process (ntp_pgm_id): ~p~n", [S#st.ntp_pgm_id]),
    io:format(Fd, "NTP daemon started at (ntpd_started_at): ~p~n",
	      [S#st.ntpd_started_at]),
    io:format(Fd, "OaM IP address: ~p~n", [S#st.oam_ip]),
    io:format(Fd, "OaM network namespace: ~p~n", [S#st.nns]),
    io:format(Fd, "(config_file_name): ~p~n", [S#st.config_file_name]),
    io:format(Fd, "(pid_file_name): ~p~n", [S#st.pid_file_name]),
    io:format(Fd, "(ctrl_file_name): ~p~n", [S#st.ctrl_file_name]),
    io:format(Fd, "Sequence number for mode 6 (seq_no): ~p~n", [S#st.seq_no]),
    Reass =
	case S#st.reassembly of
	    undefined -> undefined;
	    {Seq, Offset, Binary} ->
		{"Sequence: " ++ integer_to_list(Seq),
		"Current offset: " ++ integer_to_list(Offset),
		"Size of reassembly buffer: " ++ integer_to_list((Binary))}
	end,
    io:format(Fd, "(reassembly): ~p~n", [Reass]),
    io:format(Fd, "(trap_ho_tref): ~p [~s]~n", [S#st.trap_ho_tref,
						timer_info_to_string(S#st.trap_ho_tref)]),
    io:format(Fd, "(alarm_ho_tref): ~p [~s]~n", [S#st.alarm_ho_tref,
						 timer_info_to_string(S#st.alarm_ho_tref)]),
    io:format(Fd, "(initial_tref): ~p [~s]~n", [S#st.initial_tref,
						timer_info_to_string(S#st.initial_tref)]),
    io:format(Fd, "(update_monitor): ~p~n", [S#st.update_monitor]),
    io:format(Fd, "Default frequency offset: ~p~n", [S#st.freq_offset]),
    {Trap_buf_len, _} = S#st.trap_packets,
    io:format(Fd, "Number of trap packets in log-buffer(len(trap_packets)): ~p~n",
	      [Trap_buf_len]),
    io:format(Fd, "Printing level (verbosity): ~p~n", [S#st.verbosity]),
    Now_diff = ts_now_diff(),
    io:format(Fd, "Funs to call at timestep: ~p~n", [S#st.timestep_funs]),
    io:format(Fd, "Clockstep (erlang:timestamp()|undefined) : ~p~n", [S#st.clock_stepped_since_start]),
    io:format(Fd, "Time/now diff usec: ~p ~s~n",
	      [Now_diff, comsaNtpUtil:us_to_print_string(Now_diff)]),
    [io:format(Fd, "Alarm timer: ~p, [~s]~n", [Alarm, timer_info_to_string(AlarmRef)]) || 
	{AlarmRef, Alarm} <- S#st.ntp_alarm_timers],
    ok.

timer_info_to_string(undefined) -> "not running";
timer_info_to_string(Tref) ->
    try erlang:read_timer(Tref) of
	Ms when is_integer(Ms) ->
	    integer_to_list(Ms) ++ " ms remaining";
	_ -> "no info"
    catch
	_:_ -> "not running"
    end.

%%% assume this is either an ip-tuple or ip-string (not host/domain-name)
%%% special case "" which means the OamAccessPoint isn't set -> LMT always IPv4
ip_family([]) ->
    {inet, ?LBI};
ip_family(IP_tuple) when tuple_size(IP_tuple) =:= 4 ->
    {inet, ?LBI};
ip_family(IP_tuple) when tuple_size(IP_tuple) =:= 8 ->
    {inet6, ?LBIv6};
ip_family(IP_list) when is_list(IP_list) ->
    {ok, IP_tuple} = inet:parse_address(IP_list),
    ip_family(IP_tuple).

ip_to_string(Ip_string) when is_list(Ip_string) ->
    Ip_string;
ip_to_string(Ip_tuple) when is_tuple(Ip_tuple) ->
    inet:ntoa(Ip_tuple).

bump_seq(#st{seq_no = 16#ffff} = S) ->
    S#st{seq_no = 1};
bump_seq(#st{seq_no = Seq} = S) ->
    S#st{seq_no = Seq + 1}.

ts_now_diff() ->
    us_diff(os:timestamp(), erlang:timestamp()).

us_diff({Ms1, S1, Us1}, {Ms2, S2, Us2}) ->
    (Ms1 - Ms2) * ?MILLION * ?MILLION + (S1 - S2) * ?MILLION + (Us1 - Us2).


%these are spawned in separate process to return system and server states
return_ntp_state(#st{loopback = Lb, oam_ip_family = Family} = S) ->
    {ok, Sock} = gen_udp:open(0, [{ip, Lb}, binary, Family] ++ ns_to_opt(S)),
    Qp = comsaNtpUtil:mk_query_packet(1, 0),
    ok = gen_udp:send(Sock, Lb, ?NTP_PORT, Qp),
    receive_system_info(S, Sock, undefined).

receive_system_info(#st{loopback = Lb} = S, Sock, Reass) ->
    receive
	{udp, Sock, Lb, ?NTP_PORT,
        <<_:2, 4:3, 6:3, 1:1, 0:1, _M:1, 1:5, 1:16, _/binary>> = Data} ->
	    conditional_report(S, 2, "Receive system info"),
	    case comsaNtpUtil:reassemble(Reass, Data) of
		{done, {1, 1, 0, Status, Rest}} ->
		    System_ok = status_to_ok_state(<<Status:16>>),
		    Peer_assocs = extract_peer_assocs(Rest),
		    Peers = return_ntp_peers(S, Sock, 2, Peer_assocs),
		    exit({ok, Status, System_ok, Peers});
		{more, New_reass} ->
		    receive_system_info(S, Sock, New_reass)
	    end;
	Any ->
	    conditional_report(S, 1, "Error in receive_system_info: ~p", [Any]),
	    exit(error)
    after
	1000 ->
	    exit(timeout_system)
    end.

return_ntp_peers(_, _, _, []) -> [];
return_ntp_peers(#st{loopback = Lb} = S, Sock, Seq, [A | Assocs]) ->
    Qp = comsaNtpUtil:mk_query_packet(Seq, A),
    ok = gen_udp:send(Sock, Lb, ?NTP_PORT, Qp),
    [receive_peer_info(S, Sock, Seq, A, undefined) |
	return_ntp_peers(S, Sock, Seq + 1, Assocs)].

receive_peer_info(#st{loopback = Lb} = S, Sock, Seq, Assoc, Reass) ->
    receive
	{udp, Sock, Lb, ?NTP_PORT,
        <<_:2, 4:3, 6:3, 1:1, 0:1, _M:1, 1:5, Seq:16, _/binary>> = Data} ->
	    conditional_report(S, 2, "Receive peer info"),
	    case comsaNtpUtil:reassemble(Reass, Data) of
		{done, {Seq, 1, Assoc, Status, Rest}} ->
		    conditional_report(S, 2, "Assoc: ~p, Status: ~p",
					[Assoc, Status]),
		    Vars = comsaNtpUtil:decode_variables(Rest),
		    {_, Srcadr} = lists:keyfind(<<"srcadr">>, 1, Vars),
		    %%{_, Flash} = lists:keyfind(<<"flash">>, 1, Vars),
		    {Assoc,
			comsaNtpUtil:string_to_ip(Srcadr),
			peerstatus_to_ok_state(S, <<Status:16>>),
			peerstatus_to_reach(<<Status:16>>)};
		{more, New_reass} ->
		    receive_peer_info(S, Sock, Seq, Assoc, New_reass)
	    end;
	Any ->
	    conditional_report(S, 1, "Error in receive_peerinfo: ~p", [Any]),
	    exit(error)
    after
	1000 ->
	    exit(timeout_peer)
    end.

% if source field is 6(NTP) we are OK, if 0(not synced) we are NOK,
% any other value will cause warn-print and NOK
status_to_ok_state(<<_:2, 6:6, _/bits>>) -> ok;
status_to_ok_state(<<_:2, 0:6, _/bits>>) -> nok;
status_to_ok_state(<<_:2, What:6, _/bits>>) ->
    sysInitI:warning_msg(
	"~p: Unrecognized value in System Status Word: Source Field: ~b, "
	"assuming NTP not synchronized~n", [?MODULE, What]),
    nok.

peerstatus_to_ok_state(S, <<_:3, 1:1, _:1, Sel:3, _:8>>)  when Sel >= 4 ->
    conditional_report(S, 2, "Select: ~b", [Sel]),
    ok;
peerstatus_to_ok_state(S, <<_:3, Reach:1, _:1, Sel:3, _:8>>) ->
    conditional_report(S, 2, "Select: ~b Reach: ~b", [Sel, Reach]),
    nok.

peerstatus_to_reach(<<_:3, 1:1, _:12>>) ->
    reach_ok;
peerstatus_to_reach(<<_:3, 0:1, _:12>>) ->
    reach_nok.

-spec peerstatus_to_select(binary()) -> non_neg_integer().
peerstatus_to_select(<<_:3, _:1, _:1, Sel:3, _:8>>) ->
    Sel.

%if Leapindicator is 3(never synced) we are nok, else ok
%status_to_ok_state(<<3:2, _/bits>>) -> nok;
%status_to_ok_state(_) -> ok.

%if the flashword contains 0 (i.e. <<"0x0">>) the peer is ok, else nok
%may have to change this to use other criteria than flash...
% this IS changed now, see the top of file where it is explained
%since it maybe that flash is "flashing" to much
%flash_to_ok_state(<<"0x0">>) -> ok;
%flash_to_ok_state(_) -> nok.

%%extract_peer_assocs_stat(<<>>) -> [],

extract_peer_assocs(<<>>) -> []; %client running without any servers configured
extract_peer_assocs(<<A:16, _:16>>) -> [A];
extract_peer_assocs(<<A:16, _:16, Rest/bits>>) ->
    [A | extract_peer_assocs(Rest)].

object_to_dn({A, B, C, D}) ->
    [<<"ManagedElement=", (list_to_binary(A))/binary>>,
	<<"SystemFunctions=", (list_to_binary(B))/binary>>,
	<<"SysM=", (list_to_binary(C))/binary>>,
	<<"NtpServer=", (list_to_binary(D))/binary>>].

exit_if_simulated() ->
    case target_or_vrcs() of
	true -> ok;
	_ ->
	    io:format("This command is not available in the simulated "
		      "environment~n"),
	    exit("simulator")
    end.

%% appearantly there is a requirement for logging timesteps to the security log
%% Diff is the estimated step size in microsecs (+ or -)
security_log_timestep(Diff) ->
    Msg = "Time of day clock stepped by NTP (" ++ integer_to_list(Diff) ++
	" microseconds)",
    logI:write_log("SecurityLog", "-", get_me_id_string(), 4,
		   info, os:timestamp(), Msg).

%if the erlang:timestamp() tuple is more than 10 sec old -> true
is_step_10_seconds_ago(Before) when is_tuple(Before) ->
    Before_secs = calendar:datetime_to_gregorian_seconds(
			calendar:now_to_universal_time(Before)),
    Now_secs = calendar:datetime_to_gregorian_seconds(
			calendar:now_to_universal_time(erlang:timestamp())),
    if
	Now_secs > (Before_secs + 10) -> true;
	true -> false
    end;
is_step_10_seconds_ago(_) -> false.

get_me_id_string() ->
    case proplists:get_value(networkManagedElementId,
                             comsaI:get_managed_element_data(),
                             undefined) of
        undefined ->
            "ManagedElement=1";
        Me_string when is_list(Me_string) ->
            "ManagedElement=" ++ Me_string
    end.

do_polling_if_any_nok(#st{ntp_state = {_, []}, ntp_servers = []} =  S) ->
    S; %no servers - no use doing any polling
do_polling_if_any_nok(#st{ntp_state = {Synced, Peers}} =  S) ->
    Any_peer_nok =
	lists:any(
	    fun({_, _, nok, _}) -> true;
	       (_) -> false end, Peers),
    Any_peer_reachable =
	lists:any(
	    fun({_, _, _, reach_ok}) -> true;
	       (_) -> false end, Peers),
    % sligthtly less than 1,5 minute (since we disregard the microsecs)
    More_than_one_minutes_gone = now_diff(S#st.ntpd_started_at, erlang:timestamp()) > 88,
    if
	More_than_one_minutes_gone andalso not Any_peer_reachable ->
	    sysInitI:warning_msg(
		"~p: More than 1 minute since starting and still no server "
		"reachable - restarting NTP daemon~n", [?MODULE]),
	    restart_ntp_daemon(S);
	(Synced == nok) orelse Any_peer_nok ->
	    conditional_report(S, 2, "Ntp not ok, new poll in: ~b ms",
				[?POLL_TIMEOUT]),
	    Again_ref = refresh_timer(refresh_after_trap,
				      ?POLL_TIMEOUT,
				      S#st.trap_ho_tref),
	    S#st{trap_ho_tref = Again_ref};
	true ->
	    S
    end.

%return difference in seconds, micro part ignored
now_diff({Meg, SecA, _}, {Meg, SecB, _}) ->
    SecB - SecA;
now_diff({MegA, SecA, _}, {MegB, SecB, _}) ->
    (MegB - MegA) * ?MILLION + SecB - SecA.

ns_to_opt(#st{nns = undefined}) -> [];
ns_to_opt(#st{nns = <<>>})      -> [];
ns_to_opt(#st{nns = Ns})        -> [{netns, <<"/var/run/netns/", Ns/binary>>}].

% return {ok, NS} | {error, Err} | registered
try_register_in_oot() ->
    case ootI:register_cfg_upd_cb(fun oot_new_data/1) of
	ok ->
    	    case {ootI:get_oap_namespace(), ootI:get_oap_ip_addr()} of
		{{ok, Ns}, [_|_]} ->
		    {ok, Ns};
		{{ok, _}, []} -> %oamAccessPoint not set
		    {ok, sysInitI:get_lmt_ns()};
		{{error, _}, _} ->
		    %%io:format("~w : get_oap_namespace: ~p~n", [?MODULE, _W]),
		    registered
	    end;
	{error, Err} ->
	    {error, Err}
    end.

send_poll_ntpd(Trap) ->
    case comsaNtpUtil:get_code(Trap) of
        "program restart" ->  yes;
        "become system peer" -> yes;
        _ -> no
    end.

%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
get_volatile_dir_HERE() ->
    "/tmp".

%debug(Format) ->
%    debug(Format, []).

%debug(Format, Params) ->
%    io:format("dbg ~p:" ++ Format ++ "~n", [?MODULE | Params]).
