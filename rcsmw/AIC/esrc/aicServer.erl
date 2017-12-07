%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	aicServer.erl %
%%% @author etxjotj
%%% @copyright Ericsson AB 2014-2017
%%% @version /main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/15

%%% @doc
%%%  == The auto integration control ==
%%%
%%% Auto integration is run in four phases.
%%%
%%%  1 - Read configuration files<br/>
%%%  2 - Parse configuration files<br/>
%%%  3 - Configure node<br/>
%%%  4 - Configuration complete<br/>
%%%
%%% AIC starts two gen_server processes, <i>aicServer</i> and <i>aicHbServer</i>.
%%% However, the phases 1 and 2 are run in the system start phase <i>activate</i>.
%%% If 1 and 2 are successful then <i>aicServer</i> is invoked to do the actual
%%% configuration.
%%%
%%%  === AIC status ===
%%%
%%% <i>aicHbServer</i> process is only used as a placeholder for the AIC status.
%%% <i>aicServer</i> will contineously update <i>aicHbServer</i>
%%% with the current status.
%%% The AIC status is only used in cloud environment when VNFM sends
%%% heart beat message to VNF.
%%% The heart beat message is handled in VNFC in <i>vnfcs</i> module,
%%% <i>vnfcs</i> reads the AIC status from <i>aicHbServer</i> and replies
%%% the status to VNFM.
%%%
%%% AIC status consists of the following attributes:
%%%
%%% <code>
%%% <b>status</b>: [STARTING | OPERATIONAL | FAILED]<br/>
%%% <b>detail</b>: string(). Not used<br/>
%%% <b>error</b>:  {errorStatus, errorDetail, errorAdditionalDetail}<br/>
%%% <b>errorStatus</b>:           integer(). HTTP error code<br/>
%%% <b>errorDetail</b>:           string().  Error description<br/>
%%% <b>errorAdditionalDetail</b>: string().  Additional error description<br/>
%%% </code>
%%%
%%% <b>error</b> is only valid if status is FAILED.
%%%
%%%  === Read configuration files ===
%%%
%%% AIC tries to read the original configuration files.
%%% If no original configuration files are found
%%% then it is assumed that the auto integration
%%% has already been executed (it should only be done once),
%%% or no auto integration is to be done.
%%% After a succesful auto integration the original files will be renamed,
%%% see below.
%%%
%%% The result of this phase is a list of [{IgnoreError, FilePath/FileName}].
%%% IgnoreError indicates if the file is necessary for the auto integration
%%% to be successfull.
%%%
%%%  === Parse configuration files ===
%%%
%%% First the files are parsed to remove all comments.
%%% The results are stored in new files with the same name as the original files
%%% but extended with postfix <b>_nc</b>.
%%%
%%% Then the no comment files are split into several files;
%%% one file per hello/close-session pair found in the file.
%%% The split files will get a postfix <b>.X</b> where X is an integer [1..N]
%%%
%%% The result of this phase is [{IgnoreError, FilePath/FileName_nc.X}]
%%%
%%%  === Configure node ===
%%%
%%% In this phase AIC will first register an authentication fun in OMC
%%% to let AIC handle the authentication during the configuration.
%%%
%%% Thereafter, the following is done for each of the configuration files
%%%
%%%  - a port to netconf is opened<br/>
%%%  - a hello message is sent to check the connection<br/>
%%%  - the configuration command is sent<br/>
%%%  - waiting for a reply<br/>
%%%
%%% If everything goes well an internal message <i>loading_complete</i>
%%% is sent to <i>aicServer</i> process. See Configuration complete below.
%%%
%%% In case of an error an internal message <i>loading_failed</i> is sent to
%%% <i>aicServer</i> process. See Error handling below.
%%%
%%%  === Configuration complete  ===
%%%
%%% In this phase the following is done:
%%%
%%%  - all original configuration file names will be appended with <b>_loaded_ok</b><br/>
%%%  - the authentication fun in OMC is deleted<br/>
%%%  - configuration level is set to completed<br/>
%%%  - <i>node up</i> trap is sent to OSS<br/>
%%%  - status complete is written to file <i>../networkloader/node_status</i><br/>
%%%
%%%  === Error handling ===
%%%
%%%  ==== Phases 1 and 2 ====
%%%
%%% If anything during these phases fails then the auto integration
%%% is considered to have failed.
%%%
%%% In cloud the node is left as it is and VNFM/ENM has to take actions
%%% to possibly take an ESI and then delete the node.
%%%
%%% On real HW the node is restarted, but before that it is marked
%%% that it should be started in network loader mode.
%%%
%%%  ==== Phase 3 ====
%%%
%%% A number of retries is done for each failing configuration file.
%%% If a mandatory file (IgnoreError = false) fails then the auto integration
%%% is considered to have failed, see above.
%%% Otherwise the file is ignored and configuration continues with
%%% the next file.
%%%
%%% @end
%%% ----------------------------------------------------------
-module(aicServer).
-behaviour(gen_server).
-vsn('/main/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/15').
-date('2017-12-06').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile: module.erl %
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
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R3A/1      15-01-14   etxtory     Added handle_cast default clause
%%% R3A/2      15-02-02   etxtory     updated_oam_ip_data via aicServer
%%%                                   to avoid interference with AI (update_snmp)
%%% R3A/3      15-03-02   etxtory     Added log-entry when ready
%%% R3A/4      15-03-26   etxtory     Added AVC handling when AIC sets MO-attribute
%%% R3A/6      15-04-13   etxtory     Update discovery status when ready
%%% R3A/7      15-05-19   etxtory     ERROR to WARNING for loading failed
%%% ----------------------------------------------------------
%%% R4A/1      15-06-03   etxtory     Ignore siteEqm fault
%%% R4A/2      15-06-15   etxpejn     Delete LKF in coli_reinstall
%%% R4A/3      15-06-17   etxtory     Merge R3A/8
%%% R4A/4      15-06-25   etxtory     HT85636 - added is_ai_ongoing
%%% R4A/6      15-08-12   etxberb     Removing cluster_complete & cluster_config
%%%                                   in coli_reinstall/1.
%%% R4A/7      15-08-21   etxtory     RAN integration
%%% R4A/8      15-08-28   etxjotj     Fix for changed typing in comsa
%%% R4A/9      15-08-28   etxtory     HU12176
%%% R4A/11     15-09-06   etxtory     convert_nl_log moved
%%% R4A/12     15-09-08   etxtory     Handling partial response from COM
%%% R4A/13     15-09-17   etxtory     logEsi changes
%%% R4A/14     15-09-21   etxtory     HU16575 (try again)
%%% R4A/15     15-10-02   etxtory     HU22520
%%% R4A/17     15-11-10   etxtory     get_backup_name
%%% R4A/18     15-11-30   etxtory     OSS_CONFIGURATION_FAILED -> load backup
%%% ----------------------------------------------------------
%%% R5A/1      15-12-08   etxtory     Remove comments before split_files
%%% R5A/2      16-01-14   etxderb     HU50271 siteBasic run before siteEqm
%%% R5A/3      16-02-01   etxtory     Own netconf file for security
%%% R5A/4      16-03-14   etxtory     Update SNMP when not locked
%%% R5A/5      16-04-06   evanbel     Added support for AI GUI progress bar
%%% R5A/7      16-05-20   etxpeno     HU84962 use data from access_point_address
%%%                                   instead of ipv4_address in the oot notification
%%% ----------------------------------------------------------
%%% R6A/1      16-05-16	  evanbel     Separated siteconfig counters
%%% R6A/4      16-05-20   etxpeno     HU84962 use data from access_point_address
%%%                                   instead of ipv4_address in the oot notification
%%% R6A/5      16-06-15   etxtory     HU82571; cannot handle multireply at the same time
%%% R6A/6      16-08-17   etxarnu     HV17552: Reduce wait time from 20 sec to 1 sec in
%%%                                   update_oam_ip_data
%%% ----------------------------------------------------------
%%% R7A/1      16-10-31   etxderb     HV37278; make_esi, rm old esi*tar.gz.gpg
%%% ----------------------------------------------------------
%%% R8A/1      2017-01-16 etxjotj     HV55838 Use improved AI backup functions
%%% ----------------------------------------------------------
%%% R9A/1      2017-01-30 etxderb     Not revert log at load backup + aicLog
%%% R9A/2      2017-02-23 etxpeno     Support for eriChangeIPAddressEvent trap
%%% R9A/3      2017-03-01 etxpeno     Fixes for eriChangeIPAddressEvent trap
%%% ----------------------------------------------------------
%%% R10A/1     2017-05-19 etxjotj     Moved aicModel init to post init
%%% R10A/2     2017-05-22 emariad     Added site configuration file paths for
%%%                                   RVNFM. Moved changes for vrcs in GIT to CC
%%% R10A/3     2017-05-24 emariad     Moved aicModel init back from post init
%%% R10A/4     2017-05-30 etxjotj     EDIT WAR! :-) Move aicModel:init again
%%% ----------------------------------------------------------
%%% R11A/1     2017-07-09 uabesvi     Heartbeat status update
%%% R11A/2     2017-08-15 emarkbu     Change config files path for VNFs
%%% R11A/3     2017-08-24 etxpeno     Add info report
%%% R11A/4     2017-09-04 etxjotj     Replaced interal swm calls with swmI
%%% R11A/5     2017-10-04 uabesvi     Added Detail and AdditionalDetail
%%% R11A/6     2017-10-10 uabesvi     Do not send node up until heartbeat is received
%%%                                   Refactoring of many functions
%%% R11A/6     2017-10-11 uabesvi     Send node up trap in cloud only if heartbeated.
%%% R11A/7     2017-10-17 uabesvi     added logs
%%% R11A/8     2017-10-24 etxderb     Removed retries for virtual nodes
%%% ----------------------------------------------------------
%%% R12A/2     17-10-26   qselert     Added ready_for_service callback functionality
%%%                                   for SP086
%%% R12A/5     17-11-07   etxpeno     Add send_change_ip_address_trap/0
%%% R12A/6     2017-11-07 uabesvi     added addInfo to missing xml tag
%%% R12A/9     2017-11-20 uabesvi     Do not send node up trap for VNFM
%%% R12A/11-12 2017-11-30 uabesvi     Added back retries for virtual nodes
%%%                                   until the missing xml tag is solved
%%% R12A/15    2017-12-06 etxjotj     Don't do AI backup for R-VNFM
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export([start/0]).
-export([activate/0]).
-export([coli_reinstall/1]).
-export([is_ai_ongoing/0]).
-export([is_ready_for_service/0]).
-export([update_oam_ip_data/1]).
-export([get_backup_name/0]).
-export([register_ready_for_service_cb/1]).
-export([unregister_ready_for_service_cb/1]).
-export([send_change_ip_address_trap/0]).


%% Test interface
-export([generate_avc/1]).
-export([make_esi/0]).
-export([create_backup/0]).
-export([split_files/1]).
-export([get_conf_files/0]).
-export([get_cfgfiles_path/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
-export([load_config/2]).
-export([reset_to_factory/0]).
-export([send_snmp_trap/1]).
-export([update_oam_ip_snmp/0]).
-export([check_short/1, check_long/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%% Configuration files produced by NL (LKF-FILE is handled in aicI.erl)
-define(SITE_EQM_FILE,   "siteEquipmentFile.netconf").
-define(SITE_SEC_FILE,   "siteSecurityFile.netconf").
-define(SITE_BASIC_FILE, "siteBasicFile.netconf").

%% Old name of configuration file.
%% If this file exist; then do nothing since configuration is handled
%% in sysNetloader.
-define(INITIAL_CONFIG, "config_initial.netconf").

-define(INIT_TIME, 30000).    %% Try to load Netconf-files after 30 seconds
-define(RETRY_PERIOD, 30000). %% retry every 30 seconds
-define(RETRIES, 3).          %% retry file loading this many times
-define(NCONF_BIN, "netconf").
-define(COM_USER_FILE, "/etc/rcs_cs/com_user").

-define(BU_NAME, "Auto integration backup - SITE_CONFIG_COMPLETE").

-define(HHTP_OK,                    200).
-define(HHTP_INTERNAL_SERVER_ERROR, 500).

%% Include(s)
-include_lib("xmerl/include/xmerl.hrl").
-include("RmeAI.hrl").
-include("comte_types.hrl").
-include("comte_event.hrl").
-include("aic.hrl").

%% aicServer internal server state
-record(state,
        {
          plainfiles        = undefined,
          splitfiles        = undefined,
          user              = undefined,
          loader_pid        = undefined,
          eq_retries        = 0,
	  basic_sec_retries = 0,
          monitor           = undefined,
          snmp_timer_ref    = undefined,
          oam_ip_data       = false,
          oam_ip_data_timer_ref         = undefined,
	  oam_ip_change_short_timer_ref = undefined,
	  oam_ip_change_long_timer_ref  = undefined,
	  node_oam_ip_address,
	  ack_attribute_value,
      update_cb = []
        }).

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Starts the aicServer process
%%% @end
%%% ----------------------------------------------------------
start() ->
    ServerName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

%%% ----------------------------------------------------------
%%% @doc Called after complete startup
%%%  This is called from sysApp via aicDataInit after a complete startup.
%%% @end
%%% ----------------------------------------------------------
activate() ->
    aicHbServer:status_update(?HB_STARTING, "activating", ?HB_NOT_APPLICAPLE),
    %aicModel:init(),
    aicLog:open_log(),
    update_node_info(),

    %% Update SNMP configuration for the part that may only be made
    %% once and must be made before sending the EriDiscovery trap
    %% (done in send_snmp_trap).
    %% aicSnmp:update_snmp() makes sure this will only be made once.
    %% Once at installation and for each upgrade (which is an installation).
    aicSnmp:update_snmp(),
    update_oam_ip_snmp(),

    FunOam = fun(OamIpData) -> aicI:updated_oam_ip_data(OamIpData) end,
    ootI:register_cfg_upd_cb(FunOam),

    aicHbServer:status_update(?HB_STARTING,
			      "SNMP updated",
			      ?HB_NOT_APPLICAPLE),

    case get_conf_files() of
        {ok, Files} ->
            %% Autointegration; load configuration files
            %% Timer to make sure that all application is
	    %% ready for Netconf commands.
            %% Convert NL log only at configuration
            info_msg("Found initial configuration files ~p~n", [Files]),
            case catch split_files(Files) of
                {ok, SplitFiles} ->
                    PlainFiles = [PFile || {_IgnoreError, PFile} <- Files],
                    timer:apply_after(?INIT_TIME,
				      ?MODULE,
				      load_config,
				      [PlainFiles, SplitFiles]);
                {error, {Detail, AddDetail}} = Other ->
                    error_msg("Failed - splitting ~p~n", [Other]),
                    Msg = "Failed when checking configuration files",
                    aicLog:write_error(error, Msg),
                    %% "AutoIntegration fails"
                    %% GREEN LED:  Fast Blink
                    %% YELLOW LED: ON
                    %% 2 minutes timer before going back to NL.
                    set_led(fast, on),
		    HttpCode = ?HHTP_INTERNAL_SERVER_ERROR,
		    aicHbServer:status_update(?HB_FAILED,
					      ?HB_NOT_APPLICAPLE,
					      {HttpCode, Detail, AddDetail}),
                    timer:apply_after(1000*2*60, ?MODULE, reset_to_factory, [])
            end;

        _ ->
            %% Configuration has already been loaded.
            %% Check if OSS has acknowledged the auto-integration
            %% (OSS needs to set rbsConfigLevel).
	    aicHbServer:status_update(?HB_OPERATIONAL,
				      ?HB_NOT_APPLICAPLE,
				      ?HB_NOT_APPLICAPLE),
            case get_config_level() of
                ?ConfigLevel_SITE_CONFIG_COMPLETE ->
                    Msg = "No auto integration configuration files.~n"
			"RAN integration not ready, restarting SNMP ~n",
                    info_msg(Msg, []),
                    gen_server:cast(?MODULE, restart_send_trap);
                _ ->
                    %% Auto-integration is ready or RAN integration ongoing
                    ok
            end
    end,
    ok.

%%% ----------------------------------------------------------
%%% @doc COLI-cmd interface: reinstall the configuration.
%%  That is, revert it back to as after install
%%% @end
%%% ----------------------------------------------------------
coli_reinstall(_Arg) ->
    %% Delete Backup created by AI (if still left)
    %% Keep other backup, don't know how BU is used in testing
    swmI:clear_ai_backup(), % HV55838

    %% Handle configuration files
    InstCompl     = filename:join([sysEnv:home_dir(), "install_complete"]),
    ClusterCompl  = filename:join([sysEnv:home_dir(), "cluster_complete"]),
    ClusterConfig = filename:join([sysEnv:home_dir(), "cluster_config"]),

    DstB = get_cfgfiles_path(?SITE_BASIC_FILE),
    SrcB = DstB ++ "_loaded_ok",

    DstS = get_cfgfiles_path(?SITE_SEC_FILE),
    SrcS = DstS ++ "_loaded_ok",

    DstE = get_cfgfiles_path(?SITE_EQM_FILE),
    SrcE = DstE ++ "_loaded_ok",

    info_msg("coli_reinstall: Deleting~n"
             "~p~n"
             "~p~n"
             "~p~n"
             "Renaming~n"
             "~p to ~p~n",
             [InstCompl, ClusterCompl, ClusterConfig, SrcB, DstB]),
    file:delete(InstCompl),
    file:delete(ClusterCompl),
    file:delete(ClusterConfig),
    file:rename(SrcB, DstB),

    case filelib:is_file(SrcE) of
        true ->
            info_msg("coli_reinstall: Renaming ~n~p to ~p~n", [SrcE, DstE]),
            file:rename(SrcE, DstE);
        false ->
            ok
    end,

    case filelib:is_file(SrcS) of
        true ->
            info_msg("coli_reinstall: Renaming ~n~p to ~p~n", [SrcS, DstS]),
            file:rename(SrcS, DstS);
        false ->
            ok
    end,

    %% Delete any installed LKF
    file:delete(lmaLib:get_LKF_path(license)),

    %% Handle discovery-status
    aicSnmp:remove_discovery_status(),

    info_msg("coli_reinstall: Now rebooting ~n", []),
    appmI:restart_piu_cold(0).

%%% ----------------------------------------------------------
%%% @doc Check if AI is ongoing (called from aicI)
%%% true - Autointegration ongoing (configuration files loaded)
%%% false - No autointegration ongoing (configuration files not yet loaded)
%%% @end
%%% ----------------------------------------------------------
is_ai_ongoing() ->
    case get_conf_files() of
        {ok, _Files} ->
            true;
        _ ->
            false
    end.

%%% ----------------------------------------------------------
%%% @doc Check if "ready for service" is true (called from aicI)
%%% true - READY FOR SERVICE is true
%%% false - READY FOR SERVICE is not reached yet
%%% @end
%%% ----------------------------------------------------------
is_ready_for_service() ->
    case get_config_level() of
        ?ConfigLevel_READY_FOR_SERVICE ->
            true;
        _ ->
            false
    end.

%%% ----------------------------------------------------------
%%% @doc Register RFS callback function. To be called from subscriber during init.
%%% @end
%%% ----------------------------------------------------------
register_ready_for_service_cb(CbModule) ->
    gen_server:cast(?MODULE,
		    {register_ready_for_service_cb, {CbModule}}).

%%% ----------------------------------------------------------
%%% @doc Unregister RFS callback function.
%%% @end
%%% ----------------------------------------------------------
unregister_ready_for_service_cb(CbModule) ->
    gen_server:cast(?MODULE,
		    {unregister_ready_for_service_cb, CbModule}).

%%% ----------------------------------------------------------
%%% @doc Request sending of the eriChangeIPAddressEvent trap
%%% @end
%%% ----------------------------------------------------------
send_change_ip_address_trap() ->
    gen_server:cast(?MODULE, send_change_ip_address_trap).

%%% ----------------------------------------------------------
%%% @doc Called from OOT (via aicI) when any data is updated.
%%% AIC only cares about OaM IP addr and name space.
%%% @end
%%% ----------------------------------------------------------
update_oam_ip_data(OamIpData) ->
    gen_server:cast(?MODULE, {update_oam_ip_data, OamIpData}).

%%% ----------------------------------------------------------
%%% @doc Called by aicI (see aicI:get_backup_name).
%%% @end
%%% ----------------------------------------------------------
get_backup_name() ->
    ?BU_NAME.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% Module internal exported function
%%% ----------------------------------------------------------
auth_mod(Props) ->
    User = proplists:get_value(user, Props),
    Roles = proplists:get_value(roles, Props),
    gen_server:call(?MODULE, {auth_mod, {User, Roles}}).

load_config(PlainFiles, SplitFiles) ->
    gen_server:cast(?MODULE, {load_config, PlainFiles, SplitFiles}).



reset_to_factory() ->
    update_node_status("Node has been up but failed AI configuration"),
    reset_to_factory(sysEnv:rcs_mode_2()).

reset_to_factory(vrcs) ->
    %% Wait for someone (ENM/VNFM) to take actions
    %% what to do with this VNF
    ok;
reset_to_factory(_) ->
    %% Updates node_status; used in emergency access
    make_esi(),
    sysNetloader:revert_to_netloader(),
    appmI:restart_node(cold, "AIC").


%% Send node up trap only if vRcs and not VNFM
send_snmp_trap(OldTimerVal) ->
    gen_server:cast(?MODULE,
		    {send_snmp_trap, sysEnv:rcs_mode_2(), OldTimerVal}).

update_oam_ip_snmp() ->
    gen_server:cast(?MODULE, update_oam_ip_snmp).

check_short({Timeout, AckAttributeValue}) ->
    gen_server:cast(?MODULE, {check_short, Timeout, AckAttributeValue}).

check_long() ->
    gen_server:cast(?MODULE, check_long).

%%% ----------------------------------------------------------
%%% gen_server callbacks
%%% ----------------------------------------------------------
init(_Args) ->
    mnesia:subscribe({table, autoProvisioning, simple}),

    %% Only send the trap once
    %% NodeOamIpAddress = aicDb:read(eriChangeIPAddressNewNodeOamIpAddress, ""),

    %% Send the trap at every restart
    %% NodeOamIpAddress = "",

    NodeOamIpAddress = "",
    AckAttributeValue = aicDb:read(eriChangeIPAddressAckAttributeValue, 0),
    {ok, #state{node_oam_ip_address = NodeOamIpAddress,
		ack_attribute_value = AckAttributeValue}}.


%%=============================================================================
%% call
%%=============================================================================
%%----------------------------------------------------------
%% auth_mod
%%----------------------------------------------------------
handle_call({auth_mod, {User, []}}, _From, #state{user = User} = State) ->
    info_msg("got auth_info: ~p~n", [{User, []}]),
    Roles = ["EricssonSupport"],
    info_msg("current autointegration-user matched, returning: ~p~n", [Roles]),
    {reply, Roles, State};

handle_call({auth_mod, {User, []}}, _From, State) ->
    info_msg("Got auth_info: ~p~n", [{User, []}]),
    info_msg("Not enough power - continuing~n", []),
    {reply, [], State};

handle_call(_Request, _From, State) ->
    {reply, undefined, State}.

%%=============================================================================
%% cast
%%=============================================================================
%%----------------------------------------------------------
%% load_config
%%----------------------------------------------------------
handle_cast({load_config, _PlainFiles,_SplitFiles}, State)
  when State#state.loader_pid /= undefined ->
    info_msg("Loading of config already started, ignoring~n", []),
    {noreply, State};

handle_cast({load_config, PlainFiles, SplitFiles}, State) ->
    {noreply, do_load_config(PlainFiles, SplitFiles, State)};

%%----------------------------------------------------------
%% loading_complete
%%----------------------------------------------------------
handle_cast(loading_complete, State) ->
    Msg = "Configuration files loaded",
    aicLog:write_log(info, Msg),
    aicHbServer:status_update(?HB_OPERATIONAL, Msg, ?HB_NOT_APPLICAPLE),
    case State#state.plainfiles of
        undefined ->
            error_msg("Missing configuration files ~n", []);
        PlainFiles ->
            info_msg("Renaming files. Appending _loaded_ok ~p~n", [PlainFiles]),
            [file:rename(File, File ++ "_loaded_ok") || File <- PlainFiles]
    end,
    clear_auto_fun(),
    set_config_level(?ConfigLevel_SITE_CONFIG_COMPLETE),

    %% Create AI backup - used in RAN integration
    create_backup(),

    %% Update SNMP configuration for the part that may only be made once and must
    %% be made before sending the EriDiscovery trap (done in send_snmp_trap).
    %% This is done in aicServer:activate().
    {ok, TRef} = timer_send_snmp_trap(10000),

    %% Some OOT updates might be sent to AIC before the loading is complete.
    %% These updates are saved and executed here to avoid interference with
    %% AI-self configuration (MOM SNMP configuration) and the SNMP configuration above.
    %% Updates SNMP configuration for OAM IP address.
    case State#state.oam_ip_data of
        false ->
            ok;
        _ ->
            %% OamIpAddr and/or namespace updated
            update_oam_ip_snmp()
    end,

    %% Update node_status; used in emergency access
    update_node_status("Node has been up after AI configuration"),

    info_msg("Loading complete~n", []),
    {noreply, State#state{plainfiles     = undefined,
                          splitfiles     = undefined,
                          loader_pid     = undefined,
                          user           = undefined,
                          monitor        = undefined,
                          snmp_timer_ref = TRef,
                          oam_ip_data    = false}};

%%----------------------------------------------------------
%% loading_failed
%%----------------------------------------------------------
handle_cast({loading_failed, [{IgnoreError, File} | T] = LeftFiles}, State) ->
    clear_auto_fun(),
    NumRetries = get_retry_counter(IgnoreError, State),
    MaxRetries = max_retries(),
    if
        NumRetries < MaxRetries ->
	    After = (NumRetries * ?RETRY_PERIOD) div 1000,
            info_msg("Loading failed after ~p seconds and ~p retries, "
                     "will retry ~p times more.~n",
                     [After, NumRetries, MaxRetries - NumRetries]),

	    HbMsg = "Loading failed after " ++ integer_to_list(After) ++
		" seconds and " ++ integer_to_list(NumRetries) ++
		"retries, will retry " ++ integer_to_list(MaxRetries - NumRetries) ++
		"times more",
	    aicHbServer:status_update(?HB_STARTING, HbMsg, ?HB_NOT_APPLICAPLE),

            timer:apply_after(?RETRY_PERIOD, ?MODULE, load_config,
                              [State#state.plainfiles, LeftFiles]);

        true ->
            case IgnoreError of
                true ->
                    %% MaxRetries consumed; continue with next file
                    warning_msg("Ignoring ~p~n", [File]),
                    load_config(State#state.plainfiles, T);

                false ->
                    %% MaxRetries consumed; treat this as an error
                    %% Loading Netconf files failed.
                    error_msg("Loading failed after ~p seconds and ~p retries - "
                              "giving up.~n",
                              [(NumRetries * ?RETRY_PERIOD) div 1000,
                               NumRetries]),
                    Msg = "Failed when loading configuration files",
                    aicLog:write_log(error, Msg),
		    %% All tries done. Set status to failed but do not
		    %% change the error attribute. It should contain
		    %% the last error reason.
		    aicHbServer:status_update(?HB_FAILED,
					      Msg ++ ". Resetting to factory.",
					      ?HB_NO_CHANGE),
                    %% "AutoIntegration fails"
                    %% GREEN LED:  Fast Blink
                    %% YELLOW LED: ON
                    %% 2 minutes timer before going back to NL.
                    set_led(fast, on),
                    timer:apply_after(1000*2*60, ?MODULE, reset_to_factory, [])
            end
    end,
    State2 = increment_retry_counter(IgnoreError, State),
    {noreply, State2#state{splitfiles = LeftFiles,
                          loader_pid = undefined,
                          user = undefined}};

%%----------------------------------------------------------
%% update_oam_ip_data
%%----------------------------------------------------------
handle_cast({update_oam_ip_data, OamIpData}, State)
  when State#state.loader_pid =:= undefined ->
    %% AI is ready with loading configuration files
    case {get_data(access_point_address, OamIpData),
	  get_data(oap_namespace, OamIpData)} of
        {undefined, undefined} ->
            %% No change
            {noreply, State};
        _ ->
            %% Oam IP addr and/or Name space is updated
            NewState = State#state{oam_ip_data = false},
            handle_cast(update_oam_ip_snmp, NewState)
    end;

handle_cast({update_oam_ip_data, OamIpData}, State) ->
    %% AI is loading configuration files
    case {get_data(access_point_address, OamIpData),
	  get_data(oap_namespace, OamIpData)} of
        {undefined, undefined} ->
            %% No change
            {noreply, State};
        _ ->
            %% Oam IP addr and/or Name space is updated
            %% Perform the change when loading is ready
            {noreply, State#state{oam_ip_data = true}}
    end;

%%----------------------------------------------------------
%% update_oam_ip_snmp
%%----------------------------------------------------------
handle_cast(update_oam_ip_snmp, State) ->
    cancel_timer(State#state.oam_ip_data_timer_ref),
    case aicSnmp:is_comea_snmp_locked() of
        false ->
            aicSnmp:update_oam_ip_data(),
	    TrapSendingForced = false,
	    case maybe_send_change_ip_addr_trap(State, TrapSendingForced) of
		false ->
		    NewState = State#state{oam_ip_data_timer_ref = undefined},
		    {noreply, NewState};
		{false, NewNodeOamIpAddr, NewAckAttributeValue} ->
		    cancel_timer(State#state.oam_ip_change_short_timer_ref),
		    cancel_timer(State#state.oam_ip_change_long_timer_ref),
		    NewState =
			State#state{oam_ip_data_timer_ref = undefined,
				    oam_ip_change_short_timer_ref = undefined,
				    oam_ip_change_long_timer_ref = undefined,
				    node_oam_ip_address = NewNodeOamIpAddr,
				    ack_attribute_value = NewAckAttributeValue},
		    {noreply, NewState};
		{true, NewNodeOamIpAddr, NewAckAttributeValue} ->
		    cancel_timer(State#state.oam_ip_change_short_timer_ref),
		    cancel_timer(State#state.oam_ip_change_long_timer_ref),
		    ShortTimeout = 60*1000,       %% 1 minute
		    LongTimeout  = 24*60*60*1000, %% 24 hours
		    {ok, ShortRef} = timer:apply_after(ShortTimeout, ?MODULE,
						       check_short,
						       [{ShortTimeout,
							 NewAckAttributeValue}]),
		    {ok, LongRef} = timer:apply_after(LongTimeout, ?MODULE,
						      check_long, []),
		    NewState =
			State#state{oam_ip_data_timer_ref = undefined,
				    oam_ip_change_short_timer_ref = ShortRef,
				    oam_ip_change_long_timer_ref = LongRef,
				    node_oam_ip_address = NewNodeOamIpAddr,
				    ack_attribute_value = NewAckAttributeValue},
		    {noreply, NewState}
	    end;
        true ->
            {ok, NewTRef} = timer:apply_after(1000, ?MODULE, update_oam_ip_snmp, []),
            {noreply, State#state{oam_ip_data_timer_ref = {true, NewTRef}}}
    end;

%%----------------------------------------------------------
%% restart_send_trap
%%----------------------------------------------------------
handle_cast(restart_send_trap, State) ->
    case State#state.snmp_timer_ref of
        undefined ->
            case aicSnmp:send_trap(10000) of
                done ->
                    {noreply, State#state{snmp_timer_ref = undefined}};
                {ok, NewTimerVal} ->
                    {ok, TRef} = timer_send_snmp_trap(NewTimerVal),
                    {noreply, State#state{snmp_timer_ref = TRef}}
            end;
        _ ->
            %% Should not happen, error
            error_msg("restart_send_trap received in wrong state - ignoring~n"
                      "State ~p~n", [State]),
            {noreply, State}
    end;

%%----------------------------------------------------------
%% send_snmp_trap
%%----------------------------------------------------------
handle_cast({send_snmp_trap, vrcs, OldTimerVal}, State) ->
    {noreply, do_send_snmp_trap(State#state.snmp_timer_ref,
				aicHbServer:get_heartbeat_status(),
				OldTimerVal,
				State)};

handle_cast({send_snmp_trap, _, OldTimerVal}, State) ->
    {noreply, do_send_snmp_trap(State#state.snmp_timer_ref,
				true, %% no heartbeat on HW
				OldTimerVal,
				State)};

%%----------------------------------------------------------
%% check_short
%%----------------------------------------------------------
handle_cast({check_short, Timeout, AckAttributeValue},
	    #state{ack_attribute_value = AckAttributeValue,
		   oam_ip_change_long_timer_ref = LongTimerRef} = State) ->
    NodeOamIpAddress = State#state.node_oam_ip_address,
    case ootI:is_change_status_changed(AckAttributeValue) of
	true ->
	    cancel_timer(State#state.oam_ip_change_short_timer_ref),
	    cancel_timer(State#state.oam_ip_change_long_timer_ref),

	    ok = aicDb:write(eriChangeIPAddressAckAttributeValue,
			     AckAttributeValue),

	    ok = aicDb:write(eriChangeIPAddressNewNodeOamIpAddress,
			     NodeOamIpAddress),

	    {noreply, State#state{oam_ip_change_short_timer_ref = undefined,
				  oam_ip_change_long_timer_ref = undefined}};
	false when LongTimerRef == undefined ->
	    %% Last try to send the trap
	    Retries = 0,
	    send_change_ip_addr_trap(NodeOamIpAddress,
				     AckAttributeValue,
				     Retries),
            {noreply, State#state{oam_ip_change_short_timer_ref = undefined}};
	false ->
	    Retries = 100,
	    send_change_ip_addr_trap(NodeOamIpAddress,
				     AckAttributeValue,
				     Retries),
	    ShortTimeout = new_timeout(Timeout),
	    {ok, ShortRef} = timer:apply_after(ShortTimeout, ?MODULE,
					       check_short,
					       [{ShortTimeout,
						 AckAttributeValue}]),
            {noreply, State#state{oam_ip_change_short_timer_ref = ShortRef}}
    end;
handle_cast({check_short, _Timeout, _AckAttributeValue}, State) ->
    {noreply, State};


%%----------------------------------------------------------
%% check_long
%%----------------------------------------------------------
handle_cast(check_long, State) ->
    {noreply, State#state{oam_ip_change_long_timer_ref = undefined}};
%SP086 register_ready_for_service_cb
handle_cast({register_ready_for_service_cb, CbModule}, State) ->
    {noreply, do_register_ready_for_service_cb(CbModule, State)};

%SP086 unregister_ready_for_service_cb
handle_cast({unregister_ready_for_service_cb, CbModule}, State) ->
    NewState = do_unregister_ready_for_service_cb(CbModule, State),
    {noreply, NewState};

handle_cast(send_change_ip_address_trap, State) ->
    case aicSnmp:is_comea_snmp_locked() of
        false ->
	    cancel_timer(State#state.oam_ip_change_short_timer_ref),
	    cancel_timer(State#state.oam_ip_change_long_timer_ref),
	    TrapSendingForced = true,
	    case maybe_send_change_ip_addr_trap(State, TrapSendingForced) of
		{false, NewNodeOamIpAddr, NewAckAttributeValue} ->
		    NewState =
			State#state{oam_ip_change_short_timer_ref = undefined,
				    oam_ip_change_long_timer_ref = undefined,
				    node_oam_ip_address = NewNodeOamIpAddr,
				    ack_attribute_value = NewAckAttributeValue},
		    {noreply, NewState};
		{true, NewNodeOamIpAddr, NewAckAttributeValue} ->
		    ShortTimeout = 60*1000,       %% 1 minute
		    LongTimeout  = 24*60*60*1000, %% 24 hours
		    {ok, ShortRef} = timer:apply_after(ShortTimeout, ?MODULE,
						       check_short,
						       [{ShortTimeout,
							 NewAckAttributeValue}]),
		    {ok, LongRef} = timer:apply_after(LongTimeout, ?MODULE,
						      check_long, []),
		    NewState =
			State#state{oam_ip_change_short_timer_ref = ShortRef,
				    oam_ip_change_long_timer_ref = LongRef,
				    node_oam_ip_address = NewNodeOamIpAddr,
				    ack_attribute_value = NewAckAttributeValue},
		    {noreply, NewState}
	    end;
        true ->
            {ok, NewTRef} = timer:apply_after(1000, ?MODULE,
					      send_change_ip_address_trap, []),
            {noreply, State#state{oam_ip_data_timer_ref = {true, NewTRef}}}
    end;

handle_cast(Msg, State) ->
    error_msg("Received unknown message ~p~n", [Msg]),
    {noreply, State}.


%%=============================================================================
%% info
%%=============================================================================
%%----------------------------------------------------------
%% mnesia_table_event
%%----------------------------------------------------------
handle_info({mnesia_table_event, {write, Auto, _}}, State)
  when is_record(Auto, autoProvisioning) ->
    Msg = "Mgmt set rbsConfigLevel to " ++
        aicModel:int_to_enum(Auto#autoProvisioning.rbsConfigLevel),
    aicLog:write_log(info, Msg),
    aicLog:write_progress_bar(Auto#autoProvisioning.rbsConfigLevel),

    case Auto#autoProvisioning.rbsConfigLevel of
        ?ConfigLevel_OSS_ACTIVATING_CONFIGURATION ->
            %% "The node is able to to connect towards OSS"
            %% GREEN LED:  Fast Blink
            %% YELLOW LED: OFF
            %% SNMP: eriDiscoveryStatus=DISABLED(0)
            info_msg("Mgmt set rbsConfigLevel to OSS_ACTIVATING_CONFIGURATION~n", []),
            set_led(fast, off),

            case State#state.snmp_timer_ref of
                undefined ->
                    ok;
                TRef ->
                    timer:cancel(TRef)
            end,
            aicSnmp:set_discovery_status(disabled),
            {noreply, State#state{snmp_timer_ref = undefined}};

        ?ConfigLevel_READY_FOR_SERVICE ->
            %% "The node is ready to carry traffic"
            %% GREEN LED:  Steady ON
            %% YELLOW LED: OFF
            %% SNMP: eriDiscoveryStatus=DISABLED(0)
            %% Remove AI local backup.
            info_msg("Mgmt sets rbsConfigLevel to READY_FOR_SERVICE~n", []),
            %notify ready for service callback.
            notify_callback(State),
            set_led(on, off),
            case State#state.snmp_timer_ref of
                undefined ->
                    ok;
                TRef ->
                    timer:cancel(TRef)
            end,
            aicSnmp:set_discovery_status(disabled),
	    swmI:clear_ai_backup(), %HV55838
            {noreply, State#state{snmp_timer_ref = undefined}};

        ConfigLevel when
              ConfigLevel =:= ?ConfigLevel_OSS_CONFIGURATION_FAILED;
              ConfigLevel =:= ?ConfigLevel_RAN_INTEGRATION_WAS_CANCELLED ->

            %% Restore to the AI-backup
            info_msg("Mgmt set rbsConfigLevel to RAN_INTEGRATION_WAS_CANCELLED~n"
                     "Restoring backup to state after SITE_CONFIG_COMPLETE~n", []),

            try swmI:restore_backup(?BU_NAME) of
                ok ->
                    ok;
                {error, Reason} ->
                    error_msg("Restoring AI backup failed ~p~n", [Reason])
            catch Type:Error ->
                    error_msg("Restoring AI backup failed ~p~n", [{Type, Error}])
            end,
            {noreply, State};

        _ ->
            {noreply, State}
    end;

%%----------------------------------------------------------
%% DOWN
%%----------------------------------------------------------
handle_info({'DOWN', MonitorRef, process, _, Info}, State)
  when State#state.monitor == MonitorRef ->
    case Info of
        normal ->
            {noreply, State#state{monitor=undefined}};
        _ ->
            error_msg("Failed - loader process crashed~n~w~n", [Info]),
            handle_cast({loading_failed, State#state.splitfiles}, State)
    end;

handle_info(_Request, State) ->
    {noreply, State}.

%%=============================================================================
%% code change
%%=============================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%=============================================================================
%% terminate
%%=============================================================================
terminate(_Reason, _State) ->
    ok.



%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------



%%=============================================================================
%%=============================================================================
%%=============================================================================
%% Phase 1:  Read configuration files
%%=============================================================================
%%=============================================================================
%%=============================================================================

%%% ----------------------------------------------------------
%%% get_conf_files() -> {ok, [{IgnoreError, File}]} | none
%%%
%%% IgnoreError : boolean()
%%%
%%% Fetches the configuration if they have not been run.
%%% Accepts three cases:
%%% - Real SB and SE; with or without Sec
%%% - Real SB and faked SE; with or without Sec
%%% - Lab IC
%%% ----------------------------------------------------------
get_conf_files() ->
    SiteEqm   = get_cfgfiles_path(?SITE_EQM_FILE),
    SiteSec   = get_cfgfiles_path(?SITE_SEC_FILE),
    SiteBasic = get_cfgfiles_path(?SITE_BASIC_FILE),

    case file:read_file_info(SiteBasic) of
        {ok, _} ->
            SiteEqmTuple   = get_conf_file(SiteEqm, _IgnoreError1 = true),
            SiteSecTuple   = get_conf_file(SiteSec, _IgnoreError2 = false),
            SiteBasicTuple = [{_IgnoreError = false, SiteBasic}],
            {ok, SiteEqmTuple ++ SiteSecTuple ++ SiteBasicTuple};
        _ ->
            case lab_file() of
                {ok, Labfile} ->
                    %% Make it to same senario as RCS with eqm dummy.
                    file:rename(Labfile, SiteBasic),
                    file:write_file(SiteEqm, "Dummy generated by aicServer\n"),
                    {ok, [{_IgnoreError = false, SiteBasic}]};
                _ ->
                    none
            end
    end.

get_conf_file(File, IgnoreError) ->
    info_msg("Reading configuration file ~p. IgnoreError = ~p~n",
	     [File, IgnoreError]),
    case file:read_file(File) of
        {ok, B} ->
            case is_dummy(binary_to_list(B)) of
                false ->
                    [{IgnoreError, File}];
                true ->
                    []
            end;
        _ ->
            []
    end.

%%% ----------------------------------------------------------
%%%  get_cfgfiles_path() -> string()
%%%
%%% ----------------------------------------------------------
get_cfgfiles_path() ->
    filename:join([get_base_path(),
                   get_cfg_subdir()]).



get_cfgfiles_path(CfgFileName) ->
    filename:join([get_cfgfiles_path(), CfgFileName]).

%%% ----------------------------------------------------------
%%%  get_base_path() -> string()
%%%
%%% ----------------------------------------------------------
get_base_path() ->
    get_base_path(sysEnv:rcs_mode_2()).


get_base_path(vrcs) ->
    case swmI:node_type() of
        "R-VNFM" ->
            get_base_path(rvnfm);
        _ ->
            "/rcs"
    end;
%%RVNFM or G2
get_base_path(_Other) ->
    sysEnv:rcs_dir().


%%% ----------------------------------------------------------
%%%  get_cfg_subdir() -> string()
%%%
%%% ----------------------------------------------------------
get_cfg_subdir() ->
    get_cfg_subdir(sysEnv:rcs_mode_2()).


get_cfg_subdir(vrcs) ->
    case swmI:node_type() of
        "R-VNFM" ->
            get_cfg_subdir(rvnfm);
        _ ->
            "aiconf"
    end;
%%RVNFM or G2
get_cfg_subdir(_Other) ->
    "networkloader".

%%% ----------------------------------------------------------
%%%  lab_file() -> {ok, LabFilePath} | {error, Reason}
%%%
%%% ----------------------------------------------------------
lab_file() ->
    Ic = get_cfgfiles_path(?INITIAL_CONFIG),
    case file:read_file_info(Ic) of
        {ok, _} ->
            {ok, Ic};
        Error ->
            Error
    end.


is_dummy("Dummy"++_) -> true;
is_dummy(_) -> false.



%%=============================================================================
%%=============================================================================
%%=============================================================================
%% Phase 2:  Parse configuration files
%%=============================================================================
%%=============================================================================
%%=============================================================================

%%=============================================================================
%% split_files(Files) -> {ok, SplitFiles} | {error, {Detail, AddDetail}}
%%
%% split files into one file per hello/close-session pair
%%=============================================================================
split_files(Files) ->
    %% Remove all comments. This is to ensure that the split
    %% files does not start with a comment line.
    %% This causes Netconf loading to fail.
    case remove_comments(Files, []) of
        {ok, NewFiles} ->
            split_files(NewFiles, _Acc = []);
        {error, {_, _} = Reason} ->
            {error, Reason}
    end.

split_files([], Acc) ->
    {ok, Acc};
split_files([File | T], Acc) ->
    SplitFiles = split(File),
    split_files(T, Acc ++ SplitFiles).

split({IgnoreError, FileIn}) ->
    {ok, FdIn} = file:open(FileIn, [read, raw]),
    split(IgnoreError, FdIn, FileIn, _No = 1, _Acc = []).

split(IgnoreError, FdIn, FileIn, No, Acc) ->
    FileOut = string:concat(FileIn, "." ++ integer_to_list(No)),
    case find_close_session(FdIn) of
        {more, Str} ->
            file:write_file(FileOut, list_to_binary(Str)),
            NewAcc = Acc ++ [{IgnoreError, FileOut}],
            split(IgnoreError, FdIn, FileIn, No+1, NewAcc);
        eof ->
            Acc;
        {error, Error} ->
	    Msg = "File " ++ FileIn ++ ": " ++ Error,
	    aicLog:write_log(error, Msg),
            Acc
    end.

find_close_session(Fd) ->
    case file:read_line(Fd) of
        {ok, "\n"} ->
            find_close_session(Fd);
        {ok, Read} ->
            find_close_session(Fd, Read, _Acc = Read);
        eof ->
            eof;
        {error, Reason} ->
            {error, Reason}
    end.

find_close_session(Fd, Read, Acc) ->
    case re:run(Read, "close-session") of
        {match, _} ->
            case ensure_close_session(Read) of
                yes ->
		    case find_end(Fd) of
			{error, _} = Error ->
			    Error;
			Str ->               
			    {more, string:concat(Acc, Str)}
		    end;
                no ->
                    {ok, NewRead} = file:read_line(Fd),
                    NewAcc = string:concat(Acc, NewRead),
                    find_close_session(Fd, NewRead, NewAcc)
            end;
        nomatch ->
            {ok, NewRead} = file:read_line(Fd),
            NewAcc = string:concat(Acc, NewRead),
            find_close_session(Fd, NewRead, NewAcc)
    end.

find_end(Fd) ->
    find_end(file:read_line(Fd), Fd).


find_end({ok, Read}, Fd) ->
    find_end(Fd, Read, _Acc = Read);
find_end(eof, _Fd) ->
    {error, "end tag not found"}.


find_end(Fd, Read, Acc) ->
    case re:run(Read, "]]>]]>") of
        {match, _} ->
            Acc;
        nomatch ->
	    case file:read_line(Fd) of
		eof -> 
		    {error, "end tag not found"};
		{ok, NewRead} ->
		    NewAcc = string:concat(Acc, NewRead),
		    find_end(Fd, NewRead, NewAcc)
	    end
    end.

%% This string contains; make sure that this is a real close-session.
%% Handles the following cases:
%% <close-session/> : including whitespace(s) within <>
%% <close-session></close-session> : including whitespace(s) within any/both <>
ensure_close_session(Read) ->
    %% Trim all white-spaces
    Trim = re:replace(Read, "\\s+", "", [global, {return, list}]),
    case re:run(Trim, "<close-session") of
        {match, _} -> yes;
        nomatch    -> no
    end.

%%% ----------------------------------------------------------
%%% Extract data
%%% ----------------------------------------------------------
get_data(Type, TupleList) ->
    case lists:keyfind(Type, 1, TupleList) of
        {Type, Data} -> Data;
        _ -> undefined
    end.

%%=============================================================================
%% remove_comments([{IgnoreError, File}], Acc) ->
%%   {ok, [{IgnoreError, File_nc}]} | {error, {Detail, AddDetail}}
%%
%% Remove all comments and others before start of xml
%% document (<?xml).
%%
%% Postfix '_nc' is added to the file names.
%%=============================================================================
remove_comments([{IgnoreError, File} | T], Acc) ->
    {ok, Fd} = file:open(File, [read, raw]),
    case do_remove_comments(Fd, File, _Acc = []) of
        {ok, Str} ->
            NewFile = File ++ "_nc",
	    info_msg("Removed comments. File ~p~n", [NewFile]),
            file:write_file(NewFile, list_to_binary(Str)),
            file:close(Fd),
            remove_comments(T, [{IgnoreError, NewFile} | Acc]);
        {error, {_, _} = Reason} ->
            {error, Reason}
    end;
remove_comments([], Acc) ->
    {ok, lists:reverse(Acc)}.


do_remove_comments(Fd, File, Acc) ->
    case find_start(Fd, File, Acc) of
        {more, NewAcc} ->
            %% Check for more comments
            do_remove_comments(Fd, File, NewAcc);
        {ok, NewAcc} ->
            {ok, NewAcc};
        {error, {_, _} = Reason} ->
            {error, Reason}
    end.

find_start(Fd, File, Acc) ->
    case file:read_line(Fd) of
        {ok, "\n"} ->
            %% Empty line; remove
            find_start(Fd, File, Acc);
        {ok, Read} ->
            case re:run(Read, "<!--") of
                {match, [{Start, 4}]} ->
                    %% Start comment found;
                    %% Include chars before
                    %% Check for stop including this line
                    case Start of
                        0 ->
                            find_stop(Fd, _FRL = {ok, Read}, File, Acc);
                        _ ->
                            Str = string:sub_string(Read, 1, Start),
                            NewAcc = string:concat(Acc, Str),
                            find_stop(Fd, _FRL = {ok, Read}, File, NewAcc)
                    end;
                nomatch ->
                    NewAcc = string:concat(Acc, Read),
                    find_start(Fd, File, NewAcc)
            end;
        eof ->
            {ok, Acc};
        {error, Reason} ->
	    Detail = "VFN File (" ++ File ++
		") read error when searching for start tag",
	    AddDetail = lists:flatten(io_lib:format("~p", [{Reason}])),
            {error, {Detail, AddDetail}}
    end.

find_stop(Fd, FRL, File, Acc) ->
    case FRL of
        {ok, "\n"} ->
            %% Empty line; remove
            NRead = file:read_line(Fd),
            find_stop(Fd, NRead, File, Acc);
        {ok, Read} ->
            case re:run(Read, "-->") of
                {match, [{Start, 3}]} ->
                    %% Stop comment found
                    Len = string:len(Read),
                    case string:sub_string(Read, Start+1+3, Len) of
                        "\n" ->
                            {more, Acc};
                        Str ->
                            NewAcc = string:concat(Acc, Str),
                            {more, NewAcc}
                    end;
                nomatch ->
                    %% Line within comment; remove
                    NRead = file:read_line(Fd),
                    find_stop(Fd, NRead, File, Acc)
            end;
        eof ->
	    warning_msg("Incorrect xml-file; missing end comment. ~p~n", [File]),
            {error, {"Incorrect xml-file; missing end comment", File}};
        {error, Reason} ->
	    warning_msg("Read error when finding stop. ~p~n", [File]),
	    Detail = "VFN File (" ++ File ++
		") read error when searching for stop tag",
	    AddDetail = lists:flatten(io_lib:format("~p", [{Reason}])),
            {error, {Detail, AddDetail}}
    end.





%%=============================================================================
%%=============================================================================
%%=============================================================================
%% Phase 3:  Configure
%%=============================================================================
%%=============================================================================
%%=============================================================================

%%=============================================================================
%% Number of retries after failure to load config file
%%=============================================================================

%% Added back the retries for cloud. This is temporarily until 
%% the missing xml tag problem is solved.

max_retries() ->
    ?RETRIES.


%% max_retries() ->
%%    max_retries(sysEnv:rcs_mode_2()).

%% max_retries(vrcs) -> 0;
%% max_retries(_) -> ?RETRIES.


%%=============================================================================
%% load_netconf_files(Files, User) 
%% 
%% Use netconf to configure the node.
%% Loop over the split files.
%% 
%% The result is that a internal message is sent to aicServer process, either
%% loading_complete or
%% {loading_failed, RemainingFiles}
%% 
%%=============================================================================
load_netconf_files(Files, User) ->
    PortBin = filename:join([sysEnv:com_top(), "opt", "com", "bin", ?NCONF_BIN]),
    {ok, Port} = application:get_env(comte, netconf_port),
    load_netconf_files(Files, User, PortBin, Port).


load_netconf_files([], _User, _PortBin, _Port) ->
    Msg = "Configuration files loaded",
    info_msg(Msg ++ "~n", []),
    aicHbServer:status_update(?HB_OPERATIONAL, Msg, ?HB_NOT_APPLICAPLE),
    gen_server:cast(?MODULE, loading_complete);
load_netconf_files(Left = [{_IgnoreError, File} | T], User, PortBin, Port) ->
    NcPort = open_port({spawn_executable, PortBin},
		       [stream,
			{cd, "/tmp"}, 
			{env, [{"USER", User}]},
			{args, ["-c", ?COM_USER_FILE , integer_to_list(Port)]},
			binary, 
			exit_status, 
			use_stdio, 
			stderr_to_stdout]),
    case check_hello_response(NcPort, <<>>) of
        ok ->
            info_msg("Good initial response from netconf - "
                     "proceeding to load~n~p~n", [File]),

	    aicHbServer:status_update(?HB_STARTING,
				      "Loading file: " ++ File,
				      ?HB_NOT_APPLICAPLE),

            {ok, Data} = file:read_file(File), %% Data = binary()
            RpcCount   = string:len(binary:matches(Data, <<"<rpc">>)),
            true       = port_command(NcPort, Data),
            case decode_netconf_response(NcPort, RpcCount, <<>>, File) of
                ok ->
                    info_msg("Loaded ok ~p~n", [File]),
		    aicHbServer:status_update(?HB_STARTING,
					      "File loaded: " ++ File,
					      ?HB_NOT_APPLICAPLE),
                    load_netconf_files(T, User, PortBin, Port);
                {error, {Detail, AddDetail}} ->
		    AicError = "Loading failed. File: " ++ File ++ ". " ++
			Detail ++ " " ++ AddDetail,
		    ResCode = ?HHTP_INTERNAL_SERVER_ERROR,
		    aicHbServer:status_update(?HB_STARTING,
					      ?HB_NOT_APPLICAPLE,
					      {ResCode, Detail, AddDetail}),
                    warning_msg("~p~n", [AicError]),
                    aicLog:write_error(error, AicError),
                    gen_server:cast(?MODULE, {loading_failed, Left})
            end;
        {error, {Detail, AddDetail}} ->
	    ResCode = ?HHTP_INTERNAL_SERVER_ERROR,
	    aicHbServer:status_update(?HB_STARTING,
				      ?HB_NOT_APPLICAPLE,
				      {ResCode, Detail, AddDetail}),
            warning_msg("Failed - initial response~n~s~n", [Detail]),
            gen_server:cast(?MODULE, {loading_failed, Left})
    end.


%%% ----------------------------------------------------------
%%% hello response
%%% ----------------------------------------------------------
check_hello_response(NcPort, Resp) ->
    receive
        {NcPort, {data, Data}} ->
            case binary:match(Data, <<"Failed">>) of
                {0, 6} ->
                    {error, {"VNF Failed to connect to netconf", ""}};
                _ ->
                    case binary:match(Data, <<"]]>]]>">>) of
                        nomatch ->
                            check_hello_response(NcPort,
						 <<Resp/binary, Data/binary>>);
                        _ -> %% Match
                            do_check_hello_response(<<Resp/binary, Data/binary>>)
                    end
            end
    after
        9000 ->
            {error, {"VNF Timeout when checking netconf connection.", ""}}
    end.

do_check_hello_response(Resp) ->
    Xml = element(1, xmerl_scan:string(binary_to_list(Resp))),
    if
        element(1, Xml) =:= xmlElement,
        element(2, Xml) =:= hello -> ok;
        true -> {error, {"VNF No response when checking netconf connection.", ""}}
    end.

%%=============================================================================
%% decode_netconf_response(NcPort, RpcCount, Resp, File) -> ok | {error, Reason}
%% 
%% NcPort:   Netconf port used
%% RpcCount: Number of <rpc> tags found in the file
%% Resp:     Currently received bytes (used if reply is divided into several msgs) 
%% File:     Current file
%% 
%%=============================================================================
decode_netconf_response(NcPort, RpcCount, Resp, File) ->
    receive
        {NcPort, {exit_status, 0}} ->
            case RpcCount of
                0 ->
                    info_msg("Got netconf exit_status(0): ok~n", []),
                    ok;
                _ ->
                    warning_msg("Exit from netconf received before reply~n", []),
                    {error,
		     {"VNF Received exit from netconf. File = " ++ File,
		      ""}}
            end;
        {NcPort, {data, Data}} ->
	    NewData = <<Resp/binary, Data/binary>>,
            case binary:matches(NewData, <<"]]>]]>">>) of
                [] -> %% No match
                    decode_netconf_response(NcPort, RpcCount, NewData, File);
                Matches ->
                    case do_decode_netconf_response(NewData,
						    Matches,
						    _CurrentPos = 0,
						    _No         = 0,
						    File) of
                        {ok, NoOfReplies, Rest} ->
                            info_msg("Got netconf ~p response(s): ok (~p)~n",
				     [NoOfReplies, RpcCount - NoOfReplies]),
                            decode_netconf_response(NcPort,
						    RpcCount - NoOfReplies,
						    Rest,
						    File);
                        {error, {_, _} = Reason} ->
                            {error, Reason}
                    end
            end
    after
        300000 -> %% 5min (5min * 60sek * 1000ms)
            {error,
	     {"VNF Netconf response timeout. File = " ++ File, ""}}
    end.

%%=============================================================================
%% do_decode_netconf_response() ->
%%    {ok, NoOfReplies, RestBin} | {error, {Detail, AdditionalDetail}}
%%
%% There can one or several (two) replies.
%% Need also to handle any rest after the last reply (i.e. after last <<"]]>]]>").
%%=============================================================================
do_decode_netconf_response(Resp,
			   [{Pos, Len}| T],
			   CurrentPos,
			   NoOfReplies,
			   File) ->
    BinLen = Pos + Len - CurrentPos,
    Bin    = binary:part(Resp, CurrentPos, BinLen),
    case do_decode_netconf_response_bin(Bin, File) of
	ok ->
	    do_decode_netconf_response(Resp, T, BinLen, NoOfReplies + 1, File);
	{error, {_, _} = Error} ->
	    {error, Error}
    end;
do_decode_netconf_response(Resp, [], CurrentPos, NoOfReplies, _) ->
    RespLen = erlang:byte_size(Resp),
    RestBin = binary:part(Resp, CurrentPos, RespLen - CurrentPos),
    case RestBin of
	<<>> ->
	    {ok, NoOfReplies, <<>>};
	<<"\n">> ->
	    {ok, NoOfReplies, <<>>};
	_ ->
	    warning_msg("Reply message contained data after stop tag: ~p~n", 
			[RestBin]),
	    {ok, NoOfReplies, RestBin}
    end.

do_decode_netconf_response_bin(Resp, File) ->
    case filter_response(Resp, File) of
	{ok, FResp} ->
	    Xml       = binary_to_list(FResp),
	    {MsgE, _} = xmerl_scan:string(Xml),
	    MsgId     = get_message_id(MsgE),
	    case catch check_for_error([MsgE]) of
		ok ->
		    ok;
		ErrorStr ->
		    Detail = "VNF failed to parse " ++ File ++
			" in transaction " ++ MsgId,
		    {error, {Detail, ErrorStr}}
	    end;
	{error, {_, _} = Error} ->
	    {error, Error}
    end.

%% Remove any chars before "<?xml version="; else the xmerl-call crashes
filter_response(Resp, File) ->
    case binary:match(Resp, <<"<?xml">>) of
	nomatch ->
	    %% Should not happen
	    NcError = "Error in netconf response for file: " ++ File ++ ". " ++
		binary_to_list(Resp),
	    sysInitI:error_msg(NcError),
	    {error,
	     {"VNF RPC-response is incorrect, missing <?xml tag. File = " ++ File,
	      fr_add_info(size(Resp), Resp)}};
	{0, _} ->
	    NcOk = "OK in netconf response for file: " ++ File ++ ". " ++
		binary_to_list(Resp),
	    sysInitI:info_msg(NcOk),
	    {ok, Resp};
	{Start, _} when is_integer(Start) ->
	    NcStart = "OK (" ++ integer_to_list(Start) ++
		") in netconf response for file: " ++ File ++ ". " ++
		binary_to_list(Resp),
	    sysInitI:info_msg(NcStart),
	    RespLen = erlang:byte_size(Resp),
	    {ok, binary:part(Resp, Start, RespLen - Start)};
	{Start, _} ->
	    NcStart = "OK in netconf response for file: " ++ File ++ ". " ++
		binary_to_list(Resp),
	    sysInitI:info_msg(NcStart),
	    RespLen = erlang:byte_size(Resp),
	    {ok, binary:part(Resp, Start, RespLen - Start)}
    end.

fr_add_info(Size, Resp) when Size < 200 ->
    <<TwoH:Size/binary, _/binary>> = Resp,
    binary_to_list(TwoH);
fr_add_info(_, Resp) ->
    <<TwoH:200/binary, _/binary>> = Resp,
    binary_to_list(TwoH).

%%=============================================================================
%% get_message_id()
%%=============================================================================
get_message_id(#xmlElement{attributes = Attrs}) ->
    get_message_id_attr(Attrs);
get_message_id(_) ->
    "".

get_message_id_attr([]) ->
    "";
get_message_id_attr([#xmlAttribute{name  = 'message-id',
				   value = Msg} | _]) ->
    Msg;
get_message_id_attr([_ | T]) ->
    get_message_id_attr(T).


%%=============================================================================
%% check_for_error()
%%
%% If error found, find the error message hidden in the xml
%%=============================================================================
check_for_error([]) ->
    ok;
check_for_error([#xmlElement{name = ok} | _]) ->
    ok;
check_for_error([#xmlElement{name    = 'rpc-error',
			     content = Content} | _]) ->
    get_error_info(Content);
check_for_error([#xmlElement{content = Content} | T]) ->
    check_for_error(Content ++ T);
check_for_error([_ | T]) ->
    check_for_error(T).


get_error_info([]) ->
    "";
get_error_info([#xmlElement{name    = 'error-message',
			    content = Content} | _]) ->
    get_error_message(Content);
get_error_info([_ | T]) ->
    get_error_info(T).


get_error_message([]) ->
    "";
get_error_message([#xmlText{value = Msg} | _]) ->
    Msg;
get_error_message([_ | T]) ->
    get_error_message(T).





%%=============================================================================
%% Start loading configuration.
%%=============================================================================
do_load_config(PlainFiles, SplitFiles, State) ->
    Msg = "Start loading configuration files",
    info_msg(Msg ++ "~n", []),
    aicLog:write_log(info, Msg),
    aicHbServer:status_update(?HB_STARTING, Msg, ?HB_NOT_APPLICAPLE),
    set_auto_fun(),
    User = mk_user(),
    Fun  = fun()-> load_netconf_files(SplitFiles, User) end,
    {Pid, MonitorRef} = start_process(Fun),
    State#state{plainfiles = PlainFiles,
		splitfiles = SplitFiles,
		user       = User,
		loader_pid = Pid,
		monitor    = MonitorRef}.


%%=============================================================================
%% Send node up trap in cloud if heartbeat is received from VNFM.
%% On real HW the there is no VNFM and the trap can always be sent.
%%=============================================================================
%% Timer fired in the msg queue when cancelling the timer; ignore.
do_send_snmp_trap(undefined, _RcsMode, _OldTimerVal, State) ->
    State;
%% Cloud and no heartbeat has yet been received
do_send_snmp_trap(_, false, OldTimerVal, State) ->
    Msg = "No heartbeat received yet",
    info_msg(Msg ++ ". Checking again after ~p ms~n", [OldTimerVal]),
    aicLog:write_log(info, Msg),
    {ok, TRef} = timer_send_snmp_trap(OldTimerVal),
    State#state{snmp_timer_ref = TRef};
%% Real HW or cloud and heartbeat has been received
do_send_snmp_trap(_, true, OldTimerVal, State) ->
    case aicSnmp:send_trap(OldTimerVal) of
	done ->
	    Msg = "Node up trap sent",
	    info_msg(Msg ++ "~n", []),
	    aicLog:write_log(info, Msg),
	    State#state{snmp_timer_ref = undefined};
	{ok, NewTimerVal} ->
	    Msg = "Sending node up trap",
	    info_msg(Msg ++ ". Checking after ~p ms if ready.~n", [NewTimerVal]),
	    aicLog:write_log(info, Msg),
	    {ok, TRef} = timer_send_snmp_trap(NewTimerVal),
	    State#state{snmp_timer_ref = TRef}
    end.



%%=============================================================================
%%=============================================================================
%%=============================================================================
%% Misc help functions
%%=============================================================================
%%=============================================================================
%%=============================================================================


%%% ----------------------------------------------------------
%%% Creating backup and also handle retries
%%% ----------------------------------------------------------
create_backup() ->
    create_backup(1).

create_backup(N) when N>3 ->
    error_msg("Failed to create AI backup. Giving up... ~n", []);
create_backup(No) ->
    case swmI:node_type() of
	"R-VNFM" -> %% Don't make any AI backup on R-VNFM
	    ok;
	_ ->
	    %% HV55838
	    case swmI:make_ai_backup() of
		ok ->
		    ok;
		{error, {Type, Error}} ->
		    warning_msg("Failure when creating AI backup ~p~n",
				[{Type,Error}]),
		    create_backup(No+1)
	    end
    end.


%%% ----------------------------------------------------------
%%% Netconf loader process handling
%%% ----------------------------------------------------------
start_process(Fun) when is_function(Fun) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    spawn_monitor(proc_lib, init_p, [Parent, Ancestors, Fun]).

get_my_name() ->
    case process_info(self(), registered_name) of
        {registered_name,Name} -> Name;
        _                      -> self()
    end.

get_ancestors() ->
    case get('$ancestors') of
        A when is_list(A) -> A;
        _                 -> []
    end.


%%% ----------------------------------------------------------
%%% Get or set retry counter depending on the ignorability of
%%% the current file
%%% ----------------------------------------------------------
get_retry_counter(IgnoreError, State) ->
    case IgnoreError of
	true ->
	    State#state.eq_retries;
	_ ->
	    State#state.basic_sec_retries
    end.

increment_retry_counter(IgnoreError, State) ->
    case IgnoreError of
	true ->
	    State#state{eq_retries = State#state.eq_retries + 1};
	false ->
	    State#state{basic_sec_retries = State#state.basic_sec_retries + 1}
    end.

%%% ----------------------------------------------------------
%%% ssh for temporary user
%%% ----------------------------------------------------------
set_auto_fun()->
    omc_server:autointegration_fun(fun auth_mod/1).
clear_auto_fun()->
    omc_server:autointegration_fun(undefined).

%%% ----------------------------------------------------------
%%% Create random user
%%% ----------------------------------------------------------
mk_user() ->
    "autoint-" ++ [get_rand_alphanum() || _<- lists:seq(1,10)].

get_rand_alphanum() ->
    C = rand:uniform($z),
    if
        C >= $0, C =< $9 -> C;
        C >= $A, C =< $Z -> C;
        C >= $a, C =< $z -> C;
        true -> get_rand_alphanum()
    end.

%%% ----------------------------------------------------------
%%% Set rbsConfigLevel MIM attribute
%%% Generates AVC towards COMTE
%%% ----------------------------------------------------------
set_config_level(ConfigLevel) ->
    Fun =
        fun() ->
                case mnesia:read({autoProvisioning, {"1","1","1"}}) of
                    [Obj] ->
                        NewObj = Obj#autoProvisioning
                            {rbsConfigLevel = ConfigLevel},
                        mnesia:write(NewObj);
                    _ ->
                        ok
                end
        end,
    {atomic, ok} = mnesia:transaction(Fun),
    generate_avc(ConfigLevel).

generate_avc(ConfigLevel) ->
    Dn = comsaEcimModelAdaptor:get_dn({"1","1","1"},"RmeAI","AutoProvisioning"),
    [T,V] = comsaEcimModelAdaptor:type(enum, ConfigLevel),
    Attribute = {<<"rbsConfigLevel">>, {T,V}},
    CmEvent = #cm_event_1{dn = Dn,
                          event_type = ?AttributeValueChange,
                          attributes = [Attribute]},
    Notification =
        #cm_notification_1{
           trans_id = 1,
           source = ?ResourceOperation,
           events = [CmEvent]},
    notify(Notification).

notify(Notification) ->
    case comte:notify(Notification) of
        ok ->
            ok;
        {error, no_registered_consumers} ->
            warning_msg("AVC failed for rbsConfigLevel ~p~n", [no_registered_consumers]);
        {error, Reason} ->
            error_msg("AVC failed for rbsConfigLevel ~p~n", [Reason])
    end.

%%% ----------------------------------------------------------
%%% get_config_level()
%%% ----------------------------------------------------------
get_config_level() ->
    case mnesia:dirty_read({autoProvisioning, {"1","1","1"}}) of
        [Obj] ->
            Obj#autoProvisioning.rbsConfigLevel;
        _ ->
            %% Should not happen
            ?ConfigLevel_READY_FOR_SERVICE
    end.

%%% ----------------------------------------------------------
%%% Set LED
%%% fast = fast blick
%%% on   = steady on
%%% off  = off
%%% ----------------------------------------------------------
set_led(Green, Yellow) ->
    info_msg("Setting Leds - Green ~p; Yellow ~p~n", [Green, Yellow]),
    eqs_mmi:set_led_behavior(led_indicator(green), led_behaviour(Green)),
    eqs_mmi:set_led_behavior(led_indicator(yellow), led_behaviour(Yellow)).

led_indicator(green) -> operational;
led_indicator(yellow) -> status.
%%led_indicator(red) -> fault;
%%led_indicator(blue) -> maintenance.

led_behaviour(on) -> steady_on;
led_behaviour(off) -> off;
led_behaviour(fast) -> fast_blink.
%%led_behaviour(slow) -> slow_blink;
%%led_behaviour(flash_off) -> double_flash_off;
%%led_behaviour(flash_on) -> double_flash_on.

%%% ----------------------------------------------------------
%%% Update node status; used in emergency access in network loader
%%% ----------------------------------------------------------
update_node_status(Str) ->
    NsFile = filename:join([sysEnv:rcs_dir(), "networkloader", "node_status"]),
    file:write_file(NsFile, list_to_binary(Str)).

%%% ----------------------------------------------------------
%%% Update node info; used in emergency access in type 3
%%% TBD for multi-DU: AIC should subscribe for changes in CLH
%%% and update node info (info.txt).
%%% ----------------------------------------------------------
update_node_info() ->
    Str =  get_node_op_state() ++ get_product(),
    case erlang:function_exported(sysEnv, www_doc_root, 0) of
        true ->
            InfoFile = filename:join(sysEnv:www_doc_root(), "info.txt"),
            file:write_file(InfoFile, list_to_binary(Str));
        false ->
            warning_msg("sysEnv:www_doc_root() not yet exported ~n", [])
    end.

get_node_op_state() ->
    case catch clhI:get_node_op_state() of
        {ok, NodeStatusStr} ->
            NodeStatusStr ++ "\n";
        Error ->
            warning_msg("clhI:get_node_op_state failed: ~p~n", [Error]),
            ""
    end.

get_product() ->
    case catch eqs_pri_service:get_product() of
        {ok, ProductStr} ->
            ProductStr ++ "\n";
        Error ->
            warning_msg("eqs_pri_service:get_product() failed: ~p~n", [Error]),
            ""
    end.

%%% ----------------------------------------------------------
%%% Make an ESI
%%% ----------------------------------------------------------
make_esi() ->
    %% Remove any old ESI file; this new ESI needs to be
    %% the one and only since EA in NL only handles one ESI.
    OldEsiFile = filename:join([sysEnv:rcs_dir(), "networkloader", "esi.*.tar.gz"]),
    OldEsiFileCrypt = filename:join([sysEnv:rcs_dir(), "networkloader", "esi.*.tar.gz.gpg"]),
    os:cmd("rm " ++ OldEsiFile),
    os:cmd("rm " ++ OldEsiFileCrypt),

    case logEsi:generate_esi() of
        {_Node, full, EsiFile} ->
            EsiFileName = filename:basename(EsiFile),
            NlEsiFile = filename:join([sysEnv:rcs_dir(),
                                       "networkloader",
                                       EsiFileName]),
            file:copy(EsiFile, NlEsiFile);
        _ ->
            ok
    end.


cancel_timer(undefined)    -> ok;
cancel_timer({true, TRef}) -> timer:cancel(TRef);
cancel_timer(TRef)         -> timer:cancel(TRef).

new_timeout(60*1000) -> 2*60*1000;
new_timeout(2*60*1000) -> 4*60*1000;
new_timeout(4*60*1000) -> 6*60*1000;
new_timeout(6*60*1000) -> 8*60*1000;
new_timeout(8*60*1000) -> 10*60*1000;
new_timeout(10*60*1000) -> 10*60*1000.

maybe_send_change_ip_addr_trap(State, TrapSendingForced) ->
    NewNodeOamIpAddr = ootI:get_oap_ip_addr(),
    info_msg("~s~n"
	     "OldNodeOamIpAddr:     ~s~n"
	     "OldAckAttributeValue: ~p~n"
	     "NewNodeOamIpAddr:     ~s~n"
	     "Send trap forced:     ~p~n",
	     [?FUNCTION_NAME,
	      State#state.node_oam_ip_address,
	      State#state.ack_attribute_value,
	      NewNodeOamIpAddr,
	      TrapSendingForced]),
    maybe_send_change_ip_addr_trap(State, NewNodeOamIpAddr, TrapSendingForced).

maybe_send_change_ip_addr_trap(#state{node_oam_ip_address = OldNodeOamIpAddr},
			       OldNodeOamIpAddr,
			       false) ->
    %% No change of OaM IP address and no forced trap sending
    false;
maybe_send_change_ip_addr_trap(#state{node_oam_ip_address = "",
				      ack_attribute_value = OldAckAttributeValue},
			       NewNodeOamIpAddr = "",
			       true) ->
    %% No OaM IP address and no change of OaM IP address
    {false, NewNodeOamIpAddr, OldAckAttributeValue};
maybe_send_change_ip_addr_trap(#state{ack_attribute_value = OldAckAttributeValue},
			       NewNodeOamIpAddr = "",
			       _) ->
    %% No OaM IP address and change of OaM IP address
    NewAckAttributeValue = OldAckAttributeValue+1,
    {false, NewNodeOamIpAddr, NewAckAttributeValue};
maybe_send_change_ip_addr_trap(#state{ack_attribute_value = OldAckAttributeValue},
			       NewNodeOamIpAddr,
			       _) ->
    %% Change of OaM IP address
    NewAckAttributeValue = OldAckAttributeValue+1,
    Retries = 100,

    send_change_ip_addr_trap(NewNodeOamIpAddr, NewAckAttributeValue, Retries),

    {true, NewNodeOamIpAddr, NewAckAttributeValue}.

send_change_ip_addr_trap(NodeOamIpAddr, AckAttributeValue, Retries) ->
    AckAttributeValueL = integer_to_list(AckAttributeValue),
    NodeOamIpAddressType = ip_address_type(NodeOamIpAddr),
    RetriesL = integer_to_list(Retries),
    EriChangeIPAddressData =
	#{eriChangeIPAddressNewNodeOamIpAddressType => NodeOamIpAddressType,
	  eriChangeIPAddressNewNodeOamIpAddress => NodeOamIpAddr,
	  eriChangeIPAddressAckAttributeValue => AckAttributeValueL,
	  eriChangeIPAddressRemainingRetries => RetriesL},
    aicSnmp:send_eriChangeIPAddressEvent_trap(EriChangeIPAddressData),
    ok.

ip_address_type(IpAddr) when is_list(IpAddr)->
    ip_address_type(inet:parse_address(IpAddr));
ip_address_type({ok, IpAddr}) when tuple_size(IpAddr) == 4 ->
    "1";
ip_address_type({ok, IpAddr}) when tuple_size(IpAddr) == 8 ->
    "2".


%%===================================================================
%% do_register_ready_for_service_cb
%%===================================================================
do_register_ready_for_service_cb(CbMod, #state{update_cb = Ucb} = State) ->
    info_msg("---register READY_FOR_SERVICE adding ~p~n to this ~p~n", [CbMod, Ucb]),
    State#state{update_cb = [CbMod | Ucb]}.

%%===================================================================
%% do_unregister_ready_for_service_cb(MoRef, CbMod, State) -> State
%%===================================================================
do_unregister_ready_for_service_cb(CbMod, #state{update_cb = Ucb} = State) ->
    %Val   = proplists:get_value(MoRef, Ucb, []),
    %NewVal = lists:delete(CbMod, Val),
    %UcbRm = proplists:delete(MoRef, Ucb),
    Rest = lists:delete(CbMod, Ucb),
    State#state{update_cb = Rest}.



%%%===================================================================
%%% send message to ourselves if node up trap should be sent,
%%% i.e in all case but when the node is VNFM
%%%===================================================================
timer_send_snmp_trap(TimerVal) ->
    tsst(swmI:node_type(), TimerVal).

tsst({type,"R-VNFM"}, _TimerVal) ->
    Msg = "NodeUp trap not sent because the node is of type R-VNFM",
    aicLog:write_log(info, Msg),
    {ok, undefined};
tsst(_NodeType, TimerVal) ->
    timer:apply_after(TimerVal, ?MODULE, send_snmp_trap, [TimerVal]).


%%%===================================================================
%%% notify aic server
%%%===================================================================
notify_callback(#state{update_cb = Ucb} = State) ->
    info_msg("---notify_callback for READY_FOR_SERVICE~n~p~n", [Ucb]),
    call_cbs(Ucb),
    State.

call_cbs([])-> [];
call_cbs([{H}|T]) ->
    io:format("calling: ~p~n", [H]),
    H:notify_ready_for_service(),
    call_cbs(T).
%%% ----------------------------------------------------------
%%% Info-msg, error-msg and warning-msg
%%% ----------------------------------------------------------
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: " ++ Format, [?MODULE|Args]).

error_msg(Format, Args) ->
    sysInitI:error_msg("~w: " ++ Format, [?MODULE|Args]).

warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: " ++ Format, [?MODULE|Args]).
