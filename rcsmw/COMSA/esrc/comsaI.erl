%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaI.erl %
%%% @author etxtory
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R8A/R10A/R11A/R12A/4

%%% @doc ==Interface for COMSA - COM Support agent ==
%%% This module contains the interface for the COM support agent COMSA,
%%% and associated libraries.
%%%
%%% Applications can use this interface for interacting with the COM
%%% component and the function provided.
%%%
%%% @end

-module(comsaI).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R8A/R10A/R11A/R12A/4').
-date('2017-11-28').
-author(etxjotj).

%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2017 All rights reserved.
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
%%% R1A/4      120620     tory        register_mims/1
%%% R1A/6      20120714   etxjotj     edoc-ified this module
%%%                                   Added update_progress/2
%%% -----      -------    --------    ------------------------
%%% R2A/6      2013-01-24 etxjotj     Specific problem is not always the name
%%% R2A/10     2013-09-20 erarafo     Support for CLI Extensions
%%% R2A/12     2013-11-12 erarafo     Support for additional text in 'clear'
%%% R2A/13     2013-11-21 etxpejn     Added support for alarm buffering
%%% R2A/17     2014-02-28 etxlg       Two new secM get_filter
%%% R2A/18     2014-03-19 etxlg       Register a fun to run at timestep
%%% R2A/19     2014-03-28 etxlg       Api for netconf-{tls,ssh}
%%% R2A/20     2014-04-25 etxjotj     Removed reference to ECIM_CommonLibrary
%%% R2A/21     2014-04-29 etxtory     Api for adding new cli welcome text
%%% R2A/22     2014-04-30 etxpejn     Send clear_alarm to comsaServer
%%% R2A/23     2014-06-12 etxlg       is_super_oam_user/1
%%% -----      -------    --------    ------------------------
%%% R3A/1      2014-09-30 erarafo     Support for Additional Info
%%% R3A/3      2015-02-02 etxlg       Add get_snmp_agent_params()
%%% R3B/1      2015-02-12 etxlg       -spec update
%%% R3A/5      2015-03-25 erarafo     Narrowed and exported severity_type()
%%% R3A/6      2015-04-16 etxjotj     Get active alarms of a certain type
%%% R3A/7      2015-04-21 etxjotj     Spec fix
%%% R3A/8      2015-05-12 etxjotj     ESI info
%%% -----      -------    --------    ------------------------
%%% R4A/2      2015-07-22 etxjotj     Narrower typing for update_progress
%%% R4A/3      2015-07-22 etxjotj     Disk clean support
%%% R4A/4      2015-08-12 etxtory     Fixed above
%%% R4A/6      2015-09-01 etxlg       Fix dialyzer pettiness
%%% R4A/7      2015-09-10 erarafo     Support for warm restart
%%% R4A/8      2015-10-13 eolaand     Fix R18 compiler whining
%%% -----      -------    --------    ------------------------
%%% R6A/1      2016-05-04 uabhgma     ComSecM -> RcsSecM
%%% R6A/2      2016-08-17 etxpejn     Updated iso_time spec to handle datetime
%%% R6A/3      2016-08-30 emariad     TLS and SSH MO impl, cipher configuration
%%% -----      -------    --------    ------------------------
%%% R8A/1      2017-01-19 etxpeno     add is_com_started/0
%%% -----  ---------- -------  ------------------------------------------------
%%% R10A/1 2017-05-05 etxberb  Added stop_configuration/1.
%%% R10A/2 2017-05-08 etxberb  Added start_configuration/0.
%%% R10A/3 2017-05-17 etxpeno  Add com_started/0 and com_stopped/0
%%% -----  ---------- -------  ------------------------------------------------
%%% R11A/1 2017-10-19 etxpeno  (MR36328) handle program groups
%%% ----------------------------------------------------------
%%% R12A/1 2017-11-25 etxarnu  Added has_consul/0
%%% R12A/4 2017-11-27 etxtory  Added get_ssh_modify_algorithms()
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------

-export_type([severity_type/0]).

-export([send_alarm/3, send_alarm/4, send_alarm/5, send_alarm/6]).
-export([clear_alarm/2, clear_alarm/3, clear_alarm/4]).
-export([clear_alarm_by_instance/3]).
-export([get_alarms/1]).
-export([has_consul/0]).
-export([register_alarm/9]).
-export([register_alarm/8]). % deprecated
-export([register_callback/2]).
-export([register_imm_global_callback/1]).
-export([iso_time/2]).
-export([get_target_type/0]).
-export([register_mims_and_cli_exts/2]).
-export([update_progress/2]).
-export([encrypt_password/1, decrypt_password/1]).
-export([start_com/1, stop_com/0]).
-export([set_node_type/2]).
-export([get_all_security_roles/0]).
-export([get_managed_element_data/0]).
-export([register_subscriptions/1, register_subscriptions/2]).
-export([subscribe_timestep/1]).
-export([get_netconf_tls_config/0, get_netconf_ssh_config/0]).
-export([get_cli_tls_config/0, get_cli_ssh_config/0]).
-export([get_oam_traffic_class_config/0]).
-export([get_cli_welcome_message/0]).
-export([is_super_oam_user/1]).
-export([get_snmp_agent_params/0]).
-export([generate_esi/0]).
-export([clean_disk/1]).
-export([get_tls_cipher_suites/0]).
-export([get_ssh_preferred_algorithms/0]).
-export([get_ssh_modify_algorithms/0]).
-export([is_com_started/0]).
-export([start_configuration/0, stop_configuration/1]).
-export([com_started/0, com_stopped/0]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ----------------------------------------------------------
%%% @doc Send an alarm or alert via COM
%%%
%%% This function relays an alarm or an alert to COM.
%%% Input: AlarmName :: atom() | {VendorId, MajorId, MinorId} -
%%%          The name of the alarm or the NTF notification class identifier
%%%        Severity :: atom() - The severity of the alarm. Default is indeterminate
%%%        Dn :: [binary()] - Distinguished name of the issuing object
%%%        AdditionalText :: string() - A text message to the operator
%%%        AdditionalInfo :: list() - Additional info to the operator
%%% @end
%%% ----------------------------------------------------------

-type dn_type() :: [binary()].
-type alarm_id_type() :: {VendorId::integer(), MajorId::integer(), MinorId::integer()}|atom().

-spec send_alarm(AlarmName::alarm_id_type(),
		 Dn::dn_type(),
		 AdditionalText::string()) -> ok.

send_alarm(AlarmName, DN, AdditionalText) ->
    send_alarm(AlarmName, indeterminate, DN, AdditionalText, []).

-type severity_type() :: indeterminate | warning | minor | major | critical.
-spec send_alarm(AlarmName::alarm_id_type(),
		 Severity::severity_type(),
		 Dn::dn_type(),
		 AdditionalText::string()) -> ok.

send_alarm(AlarmName, Severity, Dn, AdditionalText) ->
    send_alarm(AlarmName, Severity, Dn, AdditionalText, []).

-spec send_alarm(AlarmName::alarm_id_type(),
		 Severity::severity_type(),
		 Dn::dn_type(),
		 AdditionalText::string(),
		 AdditionalInfo::list()) -> ok.

send_alarm(AlarmName, Severity, Dn, AdditionalText, AdditionalInfo) ->
    AppAlarm = false,
    PrgGrp = undefined,
    comsaServer:send_alarm(AlarmName, Severity, Dn, AdditionalText,
			   AdditionalInfo, AppAlarm, PrgGrp, {}).

-type uuid_type() :: binary().
-type correlation_info_type() :: {} | {primary, CorrelationUUID::uuid_type()}.
-spec send_alarm(AlarmName::alarm_id_type(),
		 Severity::severity_type(),
		 Dn::dn_type(),
		 AdditionalText::string(),
		 AdditionalInfo::list(),
                 CorrelationInfo::correlation_info_type()) -> ok.

send_alarm(AlarmName, Severity, Dn, AdditionalText, AdditionalInfo,
	   CorrelationInfo) ->
    AppAlarm = false,
    PrgGrp = undefined,
    comsaServer:send_alarm(AlarmName, Severity, Dn, AdditionalText,
			   AdditionalInfo, AppAlarm, PrgGrp, CorrelationInfo).

%%% ----------------------------------------------------------
%%% @doc Clear an active alarm in COM
%%%
%%% Same as clear_alarm/3 with no additional text.
%%%
%%% TODO ...... fix this typing issue: The first argument is an atom
%%% or a 3-tuple of integers according to alarm_id_type(). However,
%%% a 3-tuple of integers will cause a badarg in comFm:get_alarm_type/1.
%%% @end
%%% ----------------------------------------------------------

-spec clear_alarm(AlarmName::alarm_id_type(), Dn::dn_type()) -> ok.

clear_alarm(AlarmName, Dn) ->
    AppAlarm = false,
    comsaServer:clear_alarm(AlarmName, Dn, "", [], AppAlarm).

%%% ----------------------------------------------------------
%%% @doc Clear an active alarm in COM
%%%
%%% Same as clear_alarm/4 with no additional info.
%%%
%%% @end
%%% ----------------------------------------------------------

-spec clear_alarm(AlarmName::alarm_id_type(),
		  Dn::dn_type(),
		  AdditionalText::string()) -> ok.

clear_alarm(AlarmName, Dn, AdditionalText) ->
    AppAlarm = false,
    comsaServer:clear_alarm(AlarmName, Dn, AdditionalText, [], AppAlarm).

%%% ----------------------------------------------------------
%%% @doc Clear an active alarm in COM
%%%
%%% This function clears an alarm in COM for the specific MO Class. The
%%% given additional text overrides the text in the FmAlarmType instance.
%%% Input:  AlarmName :: atom() | {VendorId, MajorId, MinorId} -
%%%           The name of the alarm or the NTF notification class identifier
%%%         Dn :: [binary()] - Distinguished name of the issuing object
%%%         AdditionalText :: string() - A text message to the operator
%%%         AdditionalInfo :: list() - Additional info to the operator
%%% @end
%%% ----------------------------------------------------------

-spec clear_alarm(AlarmName::alarm_id_type(),
		  Dn::dn_type(),
		  AdditionalText::string(),
		  AdditionalInfo::list()) -> ok.

clear_alarm(AlarmName, Dn, AdditionalText, AdditionalInfo) ->
    AppAlarm = false,
    comsaServer:clear_alarm(AlarmName, Dn, AdditionalText, AdditionalInfo,
			    AppAlarm).


%%% ----------------------------------------------------------
%%% @doc Clears an alarm instance specified by MOM major/minor
%%% type and the MO instance. The MO instance is a single
%%% binarized string in 3GPP style.
%%% @end
%%% ----------------------------------------------------------
-spec clear_alarm_by_instance(non_neg_integer(), non_neg_integer(), binary()) -> ok.

clear_alarm_by_instance(MajorType, MinorType, Dn) ->
    comsaServer:clear_alarm_by_instance(MajorType, MinorType, Dn).


%%% ----------------------------------------------------------
%%% @doc Get an active alarm from the alarm list
%%%
%%% This function can be used to check if there is an active alarm of
%%% a certain type in the active alarm list, and also to get the alarm
%%% data
%%% @end
%%% ----------------------------------------------------------

-type alarm_info()::[{Key::atom(), Value::integer()|boolean()|string()}].
-spec get_alarms(AlarmName::alarm_id_type()) -> [alarm_info()].

get_alarms(AlarmName) ->
    comFm:get_alarms(AlarmName).

%%% ----------------------------------------------------------
%%% @doc Register an alarm
%%%
%%% This function registers an alarm type, which means it will be known to COM
%%% as that an alarm of this type may be sent. No alarm can be sent without
%%% it being first registered with this function.
%%% The alarm types are displayed in the FM branch as AlarmType objects
%%%
%%% Compile /opt/com/etc/mibs/ERICSSON-ALARM-PC-MIB.mib to get correct
%%% enums for the ProbableCause parameter
%%%
%%% Input: Name          - Id to uniquely identify this alarm
%%%                        It will be indexed with this name, and the
%%%                        name will show in the specific problem
%%%                        field.
%%%        MajorType     - Alarm keys are to be obtained centrally
%%%        MinorType     - Alarm keys are to be obtained centrally
%%%        MoClasses     - A comma separated list of classes that can issue
%%%                        this alarm. ECIM Tool-Chain User Guide
%%%                        2/1553-LXA 119 613/3
%%%        EventType     - Allowed values are defined by ITU-T X.733 and X.736
%%%                        Some names are shortened in the api for
%%%                        practical reasons
%%%        ProbableCause - As specified in ITU recommendations M.3100, X.733
%%%                        and X.736 and ETSI recommendation
%%%                        GSM12.11. Refer to the Ericsson Alarm
%%%                        Probable Cause MIB 4/196 03-CXC 172 7549 Rev A
%%%        Type          - Alarm or alert. Alarms must be cleared.
%%%        AdditionalText- This is used by the designer to specify extra
%%%                        information that would not be contained in
%%%                        other attributes
%%% @end
%%% ----------------------------------------------------------

-type event_type():: other|communications|qos|processing|equipment|
		     environmental|integrityViolation|operationalViolation|
		     physicalViolation|securityViolation|timeDomainViolation.

-type alarm_type() :: alarm|alert.

-spec register_alarm(Name::atom(), MajorType::integer(), MinorType::integer(),
		     MoClasses::string(), SpecificProblem::string(),
		     EventType::event_type(),
		     ProbableCause::integer(), Type::alarm_type(),
		     AdditionalText::string()) -> ok.


register_alarm(Name, MajorType, MinorType, MoClasses, SpecificProblem,
	       EventType, ProbableCause, Type, AdditionalText) ->
    comFm:register_alarm(Name, MajorType, MinorType, MoClasses, SpecificProblem,
			 EventType, ProbableCause, Type, AdditionalText).

%%% @doc DEPRECATED. This register function uses the alarm name as specific problem.
%%% @end

register_alarm(Name, MajorType, MinorType, MoClasses, EventType, ProbableCause,
	       Type, AdditionalText) ->
    sysInitI:warning_msg("comsaI:register_alarm/8 is deprecated. Please use register_alarm/9"),
    register_alarm(Name, MajorType, MinorType, MoClasses, atom_to_list(Name),
		   EventType, ProbableCause, Type, AdditionalText).

%%% ----------------------------------------------------------
%%% @doc Register a configuration data callback module
%%%
%%% The system reliease on callback modules to handle the specific
%%% calls through the COM OAM SPI. This function wraps the COMTE
%%% registration function to catch exceptions and forward those as
%%% error results.
%%%
%%% Input: ObjectPath - The path to the top object of the branch to be handled
%%%                     by this callback module
%%%        CbModule   - The callback module that's going to respond to the
%%%                     calls for this branch
%%% @end
%%% ----------------------------------------------------------

-spec register_callback(ObjectPath::[string()]|[binary()], CbModule::term()) -> ok.

register_callback(ObjectPath, CbModule) ->
    comsaLib:register_callback(ObjectPath, CbModule).

%%% ----------------------------------------------------------
%%% @doc Register a global IMM callback module
%%%
%%% This allows an IMM based service to add a specific handle during
%%% the COM transaction for calls to IMM.
%%%
%%% Input: CbModule - The callback module that's going to respond to the
%%%                   global IMM handling
%%%
%%% @end
%%% ----------------------------------------------------------

-spec register_imm_global_callback(CbModule::atom()) -> ok.

register_imm_global_callback(CbModule) ->
    comsaLib:register_imm_global_callback(CbModule).

%%% ----------------------------------------------------------
%%% @doc Get the a time string in ISO 8601 format
%%%
%%% Input: Now - a now() time format tuple
%%%        Format - 'basic'             -> "20110704T073946+0200"
%%%                 'extended'          -> "2011-07-04T07:40:11+02:00"
%%%                 'extended_zonefree' -> "2011-07-04T07:40:42"
%%%
%%% Using extended_zonefree gives correct format for ECIM MIM
%%% DateTimeWithoutOffset data type.
%%% @end
%%% ----------------------------------------------------------

-type now_time() :: {integer(), integer(), integer()}.
-type time_format() :: basic|extended|extended_zonefree.

-spec iso_time(now_time() | calendar:datetime(), time_format()) -> string().

iso_time(Now, Format) ->
    comsaLib:iso_time(Now, Format).


%%% ----------------------------------------------------------
%%% @doc Provide OMC with netconf TLS configuration
%%% This function returns the current netconf TLS configuration in a keyed list
%%% format for use in the OMC implementation of netconf on TLS
%%% @end
%%% ----------------------------------------------------------

-type nc_netconf_tls_option() :: {netconfTlsId, tuple()} |
		       {administrativeState, 'LOCKED' | 'UNLOCKED' } |
		       {nodeCredential, binary() } |
		       {trustCategory, binary()} |
		       {port, integer()}.
-type nc_netconf_tls_option_list() :: [nc_netconf_tls_option()].
-spec get_netconf_tls_config() -> nc_netconf_tls_option_list().

get_netconf_tls_config() ->
    comSysM:get_netconf_tls_config().

%%% ----------------------------------------------------------
%%% @doc Provide OMC with netconf SSH configuration
%%% This function returns the current netconf SSH configuration in a keyed list
%%% format for use in the OMC implementation of netconf on SSH
%%% @end
%%% ----------------------------------------------------------
-type nc_netconf_ssh_option() :: {netconfSshId, tuple()} |
		       {administrativeState, 'LOCKED' | 'UNLOCKED' }.
-type nc_netconf_ssh_option_list() :: [nc_netconf_ssh_option()].
-spec get_netconf_ssh_config() -> nc_netconf_ssh_option_list().

get_netconf_ssh_config() ->
    comSysM:get_netconf_ssh_config().


%%% ----------------------------------------------------------
%%% @doc Provide OMC with cli TLS configuration
%%% This function returns the current cli TLS configuration in a keyed list
%%% format for use in the OMC implementation of cli on TLS
%%% @end
%%% ----------------------------------------------------------

-type nc_cli_tls_option() :: {cliTlsId, tuple()} |
		       {administrativeState, 'LOCKED' | 'UNLOCKED' } |
		       {nodeCredential, binary() } |
		       {trustCategory, binary()} |
		       {port, integer()}.
-type nc_cli_tls_option_list() :: [nc_cli_tls_option()].
-spec get_cli_tls_config() -> nc_cli_tls_option_list().

get_cli_tls_config() ->
    comSysM:get_cli_tls_config().

%%% ----------------------------------------------------------
%%% @doc Provide OMC with cli SSH configuration
%%% This function returns the current cli SSH configuration in a keyed list
%%% format for use in the OMC implementation of cli on SSH
%%% @end
%%% ----------------------------------------------------------
-type nc_cli_ssh_option() :: {cliSshId, tuple()} |
		       {administrativeState, 'LOCKED' | 'UNLOCKED' }.
-type nc_cli_ssh_option_list() :: [nc_cli_ssh_option()].
-spec get_cli_ssh_config() -> nc_cli_ssh_option_list().

get_cli_ssh_config() ->
    comSysM:get_cli_ssh_config().

%%% ----------------------------------------------------------
%%% @doc Provide OMC with Oam Traffic Class configuration
%%% This function returns the current OTC configuration in a keyed list
%%% format for use in the OMC implementation
%%% @end
%%% ----------------------------------------------------------
-type nc_oam_traffic_class_option() :: {oamTrafficClassId, tuple()} |
				       {name, string() } |
				       {dscp, integer()}.
-type nc_oam_traffic_class_option_list() :: [nc_oam_traffic_class_option()].
-spec get_oam_traffic_class_config() -> nc_oam_traffic_class_option_list().

get_oam_traffic_class_config() ->
    comSysM:get_oam_traffic_class_config().

%%% ----------------------------------------------------------
%%% @doc Returns the selected preferred crypto algorithms as
%%% configured in the Ssh MO. The algorithms are returned within a
%%% tuple which is compliant to the ssh standard for this option:
%%% {preferred_algorithms,{cipher,{client2server,[Selected_ciphers]},{server2client[Selected_ciphers]}},
%%%                       {kex,[Selected_kex]},
%%%                       {mac,{client2server,[Selected_macs]},{server2client[Selected_macs]}}.
%%% @end
%%% ----------------------------------------------------------
-spec get_ssh_preferred_algorithms() -> {'preferred_algorithms',['invalid_type' |
                                  {'cipher',[any(),...]} |
                                  {'kex',[any()]} |
                                  {'mac',[any(),...]},...]}.

get_ssh_preferred_algorithms() ->
    rcsSecM:get_ssh_preferred_algorithms().

%%% ----------------------------------------------------------
%%% @doc Returns the modified algorothms (if any).
%%% {modify_algorithms, modify_algs_list()}.
%%% See OTP ssh documentation (OTP 20.1 and forward) for information.
%%% @end
%%% ----------------------------------------------------------
get_ssh_modify_algorithms() ->
    rcsSecM:get_ssh_modify_algorithms().

%%% ----------------------------------------------------------
%%% @doc Provide OMC with the correct TBAC type information
%%% This function returns the current type information for target
%%% based access control.
%%% @end
%%% ----------------------------------------------------------
-spec get_target_type() -> [string()].

get_target_type() ->
    rcsSecM:get_target_type().

-spec get_tls_cipher_suites() -> list().
%%% ----------------------------------------------------------
%%% @doc Returns the TLS/SSL enabled cipher suites as
%%% configured in the Tls MO. The cipher suites of tuples are returned within a list
%%% which is compliant to the tls/ssl standard for this option:
%%% @end
%%% ----------------------------------------------------------
get_tls_cipher_suites() ->
    rcsSecM:get_tls_enabled_ciphers().

%%% ----------------------------------------------------------
%%% @doc Register a MIM with COM
%%%
%%% This function is called by GMF during initial installation and
%%% during upgrade when there are new or updated application
%%% MIM-files. Note: this function does not start nor restart COM.
%%%
%%% The MIMs argument is a list of MIM-file specifications.
%%% A specification is an absolute XML file path, or a 2-tuple
%%% of absolute CXP directory and a list of XML file paths relative
%%% to that directory.
%%%
%%% The CliSharedLibs is a list of absolute paths to .so files.
%%%
%%% @end
%%% ----------------------------------------------------------

-type cxp_name() :: string().
-type mim_path() :: string().
-type cli_shared_lib() :: string().
-type mim_file_spec() :: {cxp_name(), [mim_path()]} | mim_path().

-spec register_mims_and_cli_exts([mim_file_spec()], [cli_shared_lib()]) ->  ok.

register_mims_and_cli_exts(MIMs, CliSharedlibs) ->
    comsaLib:register_mims_and_cli_exts(MIMs, CliSharedlibs).

%%% ----------------------------------------------------------
%%% @doc Update a 'AsyncActionProgress' record DEPRECATED
%%%
%%% WARNING
%%%
%%% This function supports ECIM_CommonLibrary 1.5. Newer versions of CL may
%%% included updates to this struct which makes it necessary to make a COPY
%%% of this implementation rather than to use a common function.
%%%
%%% The 'AsyncActionProgress' is a struct defined in the common library and
%%% is reused whenever an action results in a long lasting action.
%%% This function provides a uniform way of updating fieds.
%%%
%%% Initially the additionalInfoClear key can be used to clear the
%%% additional info struct member, as a normal update would simply add
%%% to the existing data. The latest additionalInfo is also copied to
%%% progressInfo.
%%%
%%% Updating the additionalInfo field will result in printout in the
%%% erlang shell. The latest additionalINfo will be stored as progressInfo
%%% as well.
%%%
%%% Database handling is not included. The user must retrieve the appropriate
%%% record, and store the returned record.
%%% Input: Data:[{FieldType:atom(), Value:term()}]
%%%        Progress:#'AsyncActionProgress'
%%% @end
%%% ----------------------------------------------------------

-record('AsyncActionProgress', {actionName,
                                additionalInfo,
                                progressInfo,
                                progressPercentage,
                                result,
                                resultInfo,
                                state,
                                actionId,
                                timeActionStarted,
                                timeActionCompleted,
                                timeOfLastStatusUpdate}).

-record('EcimPassword', {cleartext,
                         password}).

-type data_type() :: actionName|additionalInfo|additionalInfoClear|progressInfo |progressPercentage|result|resultInfo|state|actionId|timeActionStarted|timeActionCompleted.
-type progress_type():: #'AsyncActionProgress'{}.
-type progress_data():: undefined|integer()|string()|binary().
-spec update_progress(Data::[{data_type(), progress_data()}], Progress::undefined|progress_type()) -> Progress::progress_type().

update_progress(Data, Progress) ->
    comsaLib:update_progress(Data, Progress).

-spec encrypt_password(PasswordBin::binary()) -> string().
encrypt_password(PasswordBin) ->
    comsaGeneric:encrypt_password(PasswordBin).

-type password_type() :: string() | #'EcimPassword'{}.
-spec decrypt_password(Password::password_type()) -> string() | {error, string()}.
decrypt_password(Password) ->
    comsaGeneric:decrypt_password(Password).

%%% ----------------------------------------------------------
%%% @doc List all security roles
%%% An expedient way to get all system created security roles
%%% @end
%%% ----------------------------------------------------------

-spec get_all_security_roles() -> [string()].

get_all_security_roles() ->
    rcsSecM:get_all_security_roles().

%%% ----------------------------------------------------------
%%% @doc Starts a running COM instance
%%% @end
%%% ----------------------------------------------------------

-spec start_com(Opts::list()) -> ok.

start_com(Opts) ->
    comsaLib:start_com(Opts),
    ok.

%%% ----------------------------------------------------------
%%% @doc Stops the present running COM instance
%%% @end
%%% ----------------------------------------------------------

-spec stop_com() -> ok.

stop_com() ->
    comsaLib:stop_com(),
    ok.

%%% ----------------------------------------------------------
%%% @doc Set the ManagedElement attributes node type and release
%%% This function is used by SWM in order to set the ManagedElement
%%% node type and release attributes which is information which in the
%%% upgrade package metadata
%%% @end
%%% ----------------------------------------------------------

-spec set_node_type(NodeType::string(), Release::string()) -> ok.

set_node_type(NodeType, Release) ->
    comTop:set_node_type(NodeType, Release).

%%% ----------------------------------------------------------
%%% @doc Get managed element data
%%% The managed element data is used in PM ROP files and Backup metadata.
%%% A property list is returned. The order of properties are not guaranteed.
%%% @end
%%% ----------------------------------------------------------

-type me_key() :: dnPrefix|managedElementType|networkManagedElementId|
		  siteLocation|userLabel.
-type me_data() :: {me_key(), string()|undefined}.
-spec get_managed_element_data() -> [me_data()].

get_managed_element_data() ->
    comTop:get_managed_element_data().

%%% ----------------------------------------------------------
%%% @doc Register event subscribers for selected classes in a model
%%% This function is used to start a generic event subscriber that generate
%%% AVC notifications over netconf for read only attributes.
%%%
%%% The service presumes that the MO instances are stored as mnesia records,
%%% in the format used by comsaGeneric
%%% @end
%%% ----------------------------------------------------------

-type class_and_table()::{ClassName::string(), TableName::atom()}.
-spec register_subscriptions(Model::string(),
			     ClassesAndTables::[class_and_table()]) ->
				    ok | {aborted, Reason::any()}.

register_subscriptions(Model, ClassesAndTables) ->
    comsaEvent:register_subscriptions(Model, ClassesAndTables).

%%% ----------------------------------------------------------
%%% @doc Register event subscribers for all classes in a model WARNING READ BELOW
%%% This function is used to start a generic event subscriber that generate
%%% AVC notifications over netconf for read only attributes.
%%%
%%% WARNING! This requires that the comsaEcimModelAdaptor has had time to read
%%% all model information, or the lookup for classes will fail. It cannot be
%%% used to early in the initial install
%%%
%%% The service presumes that the MO instances are stored as mnesia records,
%%% in the format used by comsaGeneric
%%%
%%% This function also presumes mnesia table names coincide with the class
%%% name in the same way as mpDtdParser sets up
%%% @end
%%% ----------------------------------------------------------

-spec register_subscriptions(Model::string()) -> ok | {aborted, Reason::any()}.

register_subscriptions(Model) ->
    comsaEvent:register_subscriptions(Model).

%%% ----------------------------------------------------------
%%% @doc Register to receive a call when NTP steps system time
%%%
%%% The supplied fun will be called whenever time is stepped by ntpd.
%%% Normally this is expected to happen a few seconds after system start,
%%% however, it may also happen at a later time if the time provided by
%%% the external NTP server(s) change.
%%% The fun() should be of arity 0, it will be called in a catch, its return
%%% is not checked, and it is not persistent through restarts.
%%% @end
%%% ----------------------------------------------------------

-spec subscribe_timestep(fun(() -> ok)) -> ok.

subscribe_timestep(Fun) when is_function(Fun, 0) ->
    comsaNtpServer:subscribe_timestep(Fun).

%%% ----------------------------------------------------------
%%% @doc Gets the CLI/COLI welcome text.
%%% This function is called by ECOLI to get the operator
%%% settable part.
%%% @end
%%% ----------------------------------------------------------

-spec get_cli_welcome_message() -> string().

get_cli_welcome_message() ->
    case comsaLib:get_variable(welcome_message) of
	undefined ->
	    comsaDataInit:get_default_cli_welcome_message();
	Message ->
	    Message
    end.

%%% ----------------------------------------------------------
%%% @doc Check if a user is configured to authorize as EricssonSupport
%%% This is called from OMC (Operation and Maintenance Connections) when
%%% a user authenticated by certificate fails to receive any authorization.
%%% If the user has been added as a Maintenance user authorization role
%%% "EricssonSupport" is granted.
%%% @end
%%% ----------------------------------------------------------

-spec is_super_oam_user(string() | tuple()) -> boolean().

is_super_oam_user(Subject_name) ->
    comsaUser:is_super_oam_user(Subject_name).

%%% ----------------------------------------------------------
%%% @doc Read the SNMP agent configuration data
%%%      Returns a proplist, e.g. [{host,"0.0.0.0"},{port,6161}]
%%% @end
%%% ----------------------------------------------------------

-spec get_snmp_agent_params() -> [tuple()].
get_snmp_agent_params() ->
    comSnmp:get_agent_params().

%%% ----------------------------------------------------------
%%% @doc Generate esi information
%%% @end
%%% ----------------------------------------------------------

-spec generate_esi() -> ok.

generate_esi() ->
    comsaLib:generate_esi().

%%% ----------------------------------------------------------
%%% @doc Clean disk on request by SYS
%%% @end
%%% ----------------------------------------------------------

-spec clean_disk(minor|major) -> ok.

clean_disk(Severity) ->
    comsaLib:clean_disk(Severity).

%%% ----------------------------------------------------------
%%% @doc Checks if COM is started.
%%% @end
%%% ----------------------------------------------------------

-spec is_com_started() -> boolean().
is_com_started() ->
    comsaServer:is_com_started().

%%% ----------------------------------------------------------
%%% @doc Enable the operator to be able to change the configuration.
%%% @end
%%% ----------------------------------------------------------

-spec start_configuration() ->
    ok.
start_configuration() ->
    comsaTransactionServer:start_configuration().

%%% ----------------------------------------------------------
%%% @doc Disable the operator from changing the configuration.
%%% @end
%%% ----------------------------------------------------------

-spec stop_configuration(Reason :: string()) ->
    ok.
stop_configuration(Reason) ->
    comsaTransactionServer:stop_configuration(Reason).

%%% ----------------------------------------------------------
%%% @doc Tell comsa that COM has been started.
%%%      Used by aicSnmp, since it is calling comte:start_com() directly
%%% @end
%%% ----------------------------------------------------------
-spec com_started() -> ok.
com_started() ->
    comsaServer:com_started().

%%% ----------------------------------------------------------
%%% @doc Tell comsa that COM has been stopped.
%%%      Used by aicSnmp, since it is calling comte:stop_com() directly
%%% @end
%%% ----------------------------------------------------------
-spec com_stopped() -> ok.
com_stopped() ->
    comsaServer:com_stopped().

%%% ----------------------------------------------------------
%%% @doc Check if node has consul
%%%      
%%% @end
%%% ----------------------------------------------------------
-spec has_consul() -> true | false.
has_consul() ->
    comsaServDiscServer:has_consul().

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
