%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	comsaDataInit.erl %
%%% @author ekurnik
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/6
%%%
%%% @doc == Initialization of COMSA ==
%%% This module initializes COMSA through the various callbacks to SYS etc

-module(comsaDataInit).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/R7A/R8A/R9A/R10A/R11A/R12A/6').
-date('2017-12-06').
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
%%% R1A/4      2012-01-26 etxjotj     Added correct copyright info
%%% R1A/15     2012-04-13 etxpeno     Dialyzer fix
%%% R1A/18     2012-06-20 etxtory     Adding application MIMs
%%% R2A/2      2012-12-17 etxarnu     Changed to 64bit libComte in sim
%%% R2A/9      2013-04-04 erarafo     Support for upgrade
%%% R2A/11     2013-05-08 etxarnu     Changes due to new comte
%%% R2A/12-13  2013-05-15 etxarnu     Get comte_lib from comte application
%%% R2A/14     2013-05-15 etxarnu     Added com_run_dir and only run
%%%                                   install_comte once
%%% R2A/15     2013-05-17 etxarnu     Added comea_dir and comea_conf_dir
%%% R2A/16     2013-05-17 etxarnu     Removed libcom_authorization_agent
%%% R2A/17     2013-05-22 etxarnu     Adapted to COM 3.3
%%% R2A/19     2013-06-13 etxarnu     Use sysEnv to read port number
%%% R2A/21     2013-08-29 etxarnu     Adapted to COM 3.3 BD7 and added
%%%                                   libmaf_svs for schematron support
%%% R2A/22     2013-09-02 etxarnu     Reverted to COM 3.3 BD6
%%% R2A/23     2013-09-02 etxarnu     Adapted to COM 3.3 BD7 but removed
%%%                                   libmaf_svs for schematron support
%%% R2A/25     2013-09-09 etxarnu     Turn on comte_logging
%%% R2A/26     2013-09-24 erarafo     Support for CLI extensions
%%% R2A/33     2013-10-31 etxarnu     Set com_logging_level to 6
%%% R2A/35     2013-12-18 etxarnu     Updated Comps for new comte
%%% R2A/42     2014-01-27 erarafo     Added table comsaEcimModelByRootRdn
%%% R2A/42     2014-01-27 erarafo     Added table comsaEcimModelByRootRdn
%%% R2A/44     2014-02-03 etxberb     Added activate/0.
%%% R2A/48     2014-02-12 etxlg       Added comsaNtpServer as child
%%% R2A/49     2014-02-18 erarafo     Alarm Filtering config parameters
%%% R2A/50     2014-02-18 etxarnu     SnmpLibMajorVersion config
%%% R2A/52     2014-02-21 erarafo     Passing "domain" to parse_models
%%% R2A/53     2014-02-21 etxarnu     ComCLiAgent,connectionTimeOut,3600
%%% R2A/54     2014-02-24 etxlg       Ram table for ntp
%%% R2A/55     2014-02-21 etxarnu     ComCLiAgent,connectionTimeOut,900
%%% R2A/56     2014-02-27 etxlg       ericssonFilter moved
%%% R2A/57     2014-03-07 etxarnu     Added libcom_visibility_controller
%%% R2A/58     2014-03-24 etxarnu     Added libcli_pipe to enable COM filter cmd
%%% R2A/59     2014-03-24 etxarnu     In sim, select the correct libnetsnmp version
%%% R2A/60     2014-03-27 etxarnu     COM3.4 sh10: SysM 3.1 : netconfSsh,Tls
%%% R2A/61     2014-04-11 etxtory     Added child sync-server
%%% R2A/62     2014-04-23 etxtory     New CLI login-text; from legal
%%% R2A/63     2014-04-30 etxtory     Added table maintenanceUser
%%% R2A/64     2014-05-07 etxarnu     Prepare for proot
%%% R2A/62     2014-05-20 etxjotj     Support for ECIM TimeM
%%% R2A/67     2014-06-03 etxarnu     Changed wildcard to RCS* in install_comte_int
%%%                                   to handle split RCS
%%% R2A/68     2014-06-17 etxlg       init_data:comsaUser()
%%% R3A/1      2014-09-12 etxarnu     Added comte_log_force_facilities
%%% R3A/2      2014-10-07 etxberb     Added coiServer.
%%% R3A/3      2014-11-27 etxarnu     Added libcli_basic_component and libcli_legacy
%%%                                   Set com_logging_level to 3
%%% R3A/6      2014-12-02 etxberb     Added calls to coiMim.
%%% R3A/7      2014-12-02 etxberb     reverted to libcli_basic.
%%% R3A/8      2014-12-09 etxlg       Put back comsaFirewall, missed in merge
%%% R3A/9      2014-12-12 etxarnu     New try with COM5.0 libs
%%% R3A/10     2014-12-15 etxarnu     Revert COM 5.0 libs
%%% R3A/11     2014-12-15 etxarnu     New try with COM5.0 libs
%%% R3A/12     2014-12-19 etxarnu     Added dev_patches to LibDirs
%%% R3A/13     2015-01-13 etxberb     Removed all coi related code. (Moved to
%%%                                   the new COI block)
%%% R3A/14     2015-01-27 etxjotj     Discontinued ComLdapAuthentication
%%% R3A/15     2015-01-30 erarafo     Provisional solution for LED status
%%% R3A/16     2015-02-06 etxpeno     Add log ComInterfaceLog
%%% R3A/17     2015-02-13 erarafo     Status LED, not yet enabled
%%% R3A/18     2015-02-16 erarafo     WP3722: Status LED
%%% R3A/19     2015-02-24 etxjotj     Authorization appdata
%%% ----    ---------- -------  ------------------------------------------------
%%% R4A/1   2015-04-27 etxarnu  COM 5.1: removed libcli_legacy
%%%                             and added cliSsh/cliTls_types
%%% R4A/2   2015-05-12 erarafo  Turn off notifications generated by COM
%%% R4A/3   2015-05-19 etxpejn  Removed AvcLog
%%% R4A/4   2015-05-23 etxlg    Merge: No internal trap receiver,
%%%                             TR HT83216, (HT50573?)
%%% R4A/5   2015-07-07 etxjotj  SysM 3.3
%%% R4A/6   2015-07-08 etxberb  Changed mnesia:create_table to
%%%                             clhI:mnesia_create_table.
%%% R4A/7   2015-07-22 etxjotj  Register file owner
%%% R4A/8   2015-08-28 erarafo  Warm restart, preparations
%%% R4A/11  2015-11-25 etxberb  Changed from 'set' to 'ordered_set' on fmAlarm.
%%% ----    ---------- -------  ------------------------------------------------
%%% R5A/1   2015-11-09 uabesvi  added UPDATE FOR NTP CLUSTER (as comments)
%%% R4A/12  2015-12-03 etxjotj  HU42180: Updated schema locations
%%% ----    ---------- -------  ------------------------------------------------
%%% R5A/3   2015-12-21 etxtory  HU35038: Changed transientFilter 2->4
%%% R5A/5   2016-01-07 etxberb  Changed installation phases in all blocks.
%%% R5A/6   2016-01-11 etxberb  Added old installation phase functions for
%%%                             backwards compatibility reasons (explicit calls
%%%                             from $RDE_TOP/tools/mkcpi/mkcpi.escript)
%%% R5A/8   2016-01-27 etxberb  Added instPhSeqBeg_init_data/0.
%%% R5A/9   2016-01-28 etxjotj  Switch model names
%%% R5A/10  2016-01-28 etxjotj  Switch names names
%%% R5A/11  2016-02-01 etxjotj  Switch model names again
%%% R5A/12  2016-02-19 etxarnu  Fix comlibs for i686
%%% R5A/14  2016-02-22 etxarnu  Fix ModelP for VRCS
%%% R5A/15  2016-02-23 etxarnu  Fix ModelP for VRCS again
%%% ----    ---------- -------  ------------------------------------------------
%%% R6A/1   2016-04-25 etxarnu  Fix spelling error
%%%                               get_default_cli_welcome_message
%%% R6A/2   2016-05-04 uabhgma  ComSecM -> RcsSecM
%%% ----    ---------- -------  ------------------------------------------------
%%% R7A/1   2016-07-12 etxpeno  support for COM 7.0 CP1
%%% R7A/2   2016-08-18 etxarnu  Support for aarch64
%%% R7A/3   2016-09-23 etxpejn  Added dir and callback at ESI collection
%%% R8A/1   2016-10-25 etxberb  Added mnesia table fmAlarmType_notReg.
%%% R8A/2   2016-11-07 egjknpa  Init mnesia table for rcsExeR.
%%% R8A/3   2016-11-15 etxberb  Added exclude_models_from_vrcs/1.
%%% R8A/3   2016-12-13 uabesvi  Moved register_esi_dir for comte here from sysDataInit
%%% R9A/1   2016-11-25 egjknpa  rename rcsExeR to rmeExeR.
%%% R8A/4   2017-01-09 erarafo  HV46949, add gen_server comsaAlarm
%%% R8A/5   2017-01-11 etxjotj  Added model filtering for RmeExeR
%%%                             Generalized model filtering
%%%                             Fixes for ExecutionResource
%%% R8A/6   2017-01-11 etxjotj
%%% R8A/7   2017-01-16 etxarnu  Added RmeSds handling
%%% R8A/8   2017-01-17 etxpeno  Comte-1.10 changes
%%% ----    ---------- -------  ------------------------------------------------
%%% R9A/1   2017-01-22 etxarnu  RmeSds only if consul avail
%%% R9A/2   2017-01-29 etxarnu  RmeSds only if consul avail more..
%%% R9A/3   2017-04-03 etxjotj  New way of definining excluded models
%%% R9A/4   2017-04-03 etxjotj  Now included SDS in excluded models
%%%                             Exported exclude function for mkcpi.escript
%%% R9A/5   2017-04-03 etxjotj  Added RadioTNode to exclude definitions
%%% R9A/6   2017-04-05 etxjotj  Backed out new exclusion logic temporarily
%%% R9A/7   2017-04-11 etxarnu  HV79419:Register comsaServDiscServer for warm_cb
%%% ----    ---------- -------  ------------------------------------------------
%%% R10A/1  2017-05-08 etxjotj  Only load relevant models node type
%%% R10A/2-3 2017-05-09 etxarnu Do not exclude RmeSds for vSD
%%% R10A/4  201705-17  etxjotj  Add PM MOM to vSD
%%% R10A/5  2017-05-18 etxarnu  Exclude RmeSds for VtfRadioNode
%%% R10A/6  2017-07-13 eolaand  Exclude RcsOamAccessPoint for all NodeTypes
%%% ----    ---------- -------  ------------------------------------------------
%%% R11A/1  2017-07-27 estifil  comSnmpDtlsConfig added
%%% R11A/2  2017-08-02 ekurnik  Added activate for comSnmpDtlsConfig process
%%% R11A/3  2017-08-08 etxjotj  Add RmeSds to all RadioNode UP types
%%% R11A/4  2017-08-09 etxjotj  Fix additions of MO tables
%%% R11A/5  2017-08-10 etxjotj  Removed unncessare calls
%%% R11A/6  2017-09-10 etxarnu  Fix for new rcs-sim
%%% R11A/8  2017-10-04 etxpeno  create mnesia table comsaAppAlarm
%%% R11A/9  2017-10-09 eivmiha  Removed comSnmpDtlsConfig start_link and activate
%%% R11A/10 2017-10-12 erohjun  Upgrade Call back interface registered for module comsaServDisvServer
%%% R11A/11 2017-10-17 estjako  Added library path for lib_sec_credu_api
%%% ----    ---------- -------  ------------------------------------------------
%%% R12A/1  2017-10-25 etxpeno  support restart of program groups
%%% R12A/2  2017-11-06 ebabmat  HW39540 prevent ComFileTPM.xml to be added to the build
%%% R12A/3  2017-11-22 eivmiha  added FTpServer
%%% R12A/6  2017-12-06 ekurnik  Added SnmpTargetV3Dtls to registered subscriptions
%%% ----    ---------- -------  ------------------------------------------------
%%%
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([children/0,
	 instPhParallel_init/1,
	 instPhSeqBeg_init_data/0,
	 instPhParallel_init_data/0,
	 instPhParallel_post_init/0]).
-export([init/1]).
-export([install_comte/2]).
-export([activate/0]).
-export([get_default_cli_welcome_message/0]).
-export([get_excluded_models/1]). % Exported for mkcpi
%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


-define(MonoTime, erlang:monotonic_time()).

%-define(ModelsToExcludeFromVrcs, ["RcsSwM.xml"]).

%% Usage of error_logger:XXX_report
-define(LOG_ERR(__ReportInfo),
	sysInitI:error_report(?RepInfo(__ReportInfo))).
-define(LOG_INFO(__ReportInfo),
	sysInitI:info_report(?RepInfo(__ReportInfo))).
-define(LOG_WARN(__ReportInfo),
	sysInitI:warning_report(?RepInfo(__ReportInfo))).
-define(RepInfo(__RepInfo),
	[{?MODULE, ?FUNCTION} | __RepInfo]).
%% General
-define(ELSE, true).
-define(FUNCTION,
	element(2, element(2, process_info(self(), current_function)))).

-include("ComFm.hrl").
-include("RcsLocalAuthorization.hrl").
-include("RcsSecM.hrl").
-include("ComTop.hrl").
-include("RcsSnmp.hrl").
-include("RcsSysM.hrl").
-include("RmeExeR.hrl").
-include("RmeSds.hrl").
-include("RcsFileTPM.hrl").
-include("ComsaEcimModelAdaptor.hrl").
-include("ComsaEvent.hrl").
-include("RcsUser.hrl").
-include("RcsTimeM.hrl").
-include("ComsaTypes.hrl").
-include("ComsaAlarm.hrl").

%%% #---------------------------------------------------------
%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init(DbNodes) ->
    init_tables(DbNodes),
    ok.

%% Old installation phase - kept due to explicit call from
%% $RDE_TOP/tools/mkcpi/mkcpi.escript
init(DbNodes) ->
    instPhParallel_init(DbNodes).

children() ->
    {ok, case clhI:mp_role() of
	     core ->
		 [{comsaServer, {comsaServer, start, []},
		   permanent, 1000, worker, [comsaServer]},
		  {comsaAlarm, {comsaAlarm, start_link, []},
		   permanent, 1000, worker, [comsaAlarm]},
		  {comsaNtfSubscriber, {comsaNtfSubscriber, start_link, []},
		   permanent, 1000, worker, [comsaNtfSubscriber]},
		  %% --- UPDATE FOR NTP CLUSTER BEGIN ---
		  %% 		  {comsaNtpProxy, {comsaNtpProxy, start_link, []},
		  %% 		   permanent, 1000, worker, [comsaNtpProxy]},
		  %% --- UPDATE FOR NTP CLUSTER END ---
		  {comsaEvent, {comsaEvent, start, []},
		   permanent, 1000, worker, [comsaEvent]},
		  {comsaNtpServer, {comsaNtpServer, start_link, []},
		   permanent, 1000, worker, [comsaNtpServer]},
		  {comsaSyncServer, {comsaSyncServer, start_link, []},
		   permanent, 1000, worker, [comsaSyncServer]},
		  {comsaFirewall, {comsaFirewall, start_link, []},
		   permanent, 1000, worker, [comsaFirewall]},
		  {comsaLedControl, {comsaLedControl, start_link, []},
		   permanent, 1000, worker, [comsaLedControl]}
		 ] ;
	     _ ->
		 [{comsaNtpServer, {comsaNtpServer, start_link, []},
		   permanent, 1000, worker, [comsaNtpServer]},
		  {comsaLedControl, {comsaLedControl, start_link, []},
		   permanent, 1000, worker, [comsaLedControl]}
		 ]
	 end ++
	 case comsaI:has_consul() of
	     true ->
		 [{comsaServDiscServer,
		   {comsaServDiscServer, start_link, []},
		   permanent, 1000, worker, [comsaServDiscServer]}];
	     false ->
		 []
	 end
    }.

init_tables(DbNodes) ->
    ComMaintenanceUserSec = [{maintenanceUserSecurity, ?maintenanceUserSecurity_types}],
    ComMaintenanceUser = [{maintenanceUser, ?maintenanceUser_types}],
    ComFm = [{fm, ?fm_types},
	     {fmAlarmModel, ?fmAlarmModel_types},
	     {fmAlarmType, ?fmAlarmType_types}],
    ComFmAlarm = [{fmAlarm, ?fmAlarm_types}],
    RcsLocalAuthorization = [{role, ?role_types},
		  {rule, ?rule_types},
		  {customRole, ?customRole_types},
		  {customRule, ?customRule_types},
		  {localAuthorizationMethod, ?localAuthorizationMethod_types}],
    RcsSecM = [{secM, ?secM_types},
	       {tls, ?tls_types},
               {ssh, ?ssh_types},
	       {authenticationOrder, ?authenticationOrder_types},
	       {authorizationOrder, ?authorizationOrder_types},
	       {userManagement, ?userManagement_types}],
    ComSnmp = [{snmp, ?snmp_types},
	       {snmpTargetV1, ?snmpTargetV1_types},
	       {snmpTargetV2C, ?snmpTargetV2C_types},
	       {snmpTargetV3, ?snmpTargetV3_types},
	       {snmpViewV1, ?snmpViewV1_types},
	       {snmpViewV2C, ?snmpViewV2C_types},
	       {snmpViewV3, ?snmpViewV3_types},
	       {snmpTargetV3Dtls, ?snmpTargetV3Dtls_types}
	      ],
    RmeExeR      = [{executionResource, ?executionResource_types}],
    RmeSds       = [{serviceDiscovery, ?serviceDiscovery_types}],
    ComSysM = [{sysM, ?sysM_types},
	       {ntpServer, ?ntpServer_types},
	       {sysMSchema, ?sysMSchema_types},
	       {netconfTls,?netconfTls_types},
	       {netconfSsh,?netconfSsh_types},
	       {cliSsh,?cliSsh_types},
	       {cliTls,?cliTls_types},
	       {oamTrafficClass, ?oamTrafficClass_types}],
    RcsFileTPM = [{fileTPM, ?fileTPM_types},
		  {ftpTls, ?ftpTls_types},
          {ftpServer, ?ftpServer_types},
		  {ftpTlsServer, ?ftpTlsServer_types},
		  {sftp, ?sftp_types},
		  {sftpServer, ?sftpServer_types}],
    ComTop = [{managedElement, ?managedElement_types},
	      {systemFunctions, ?systemFunctions_types},
	      {transport, ?transport_types},
	      {legacy, ?legacy_types}],

    RcsTimeM = [{timeM, ?timeM_types},
		{dateAndTime, ?dateAndTime_types}],

    %% Tables are created for all known MOMs regardless if they are visible
    %% or not.

    AllTables = [ComMaintenanceUserSec, ComMaintenanceUser, ComFm,
		 RcsLocalAuthorization, RcsSecM, ComSnmp, ComSysM,
		 ComTop, RcsFileTPM, RcsTimeM, RmeExeR, RmeSds],

    [[create_table(Name, DbNodes, Types)||{Name, Types}<-Tables]||
	Tables<-AllTables],

    [[create_ordered_table(Name, DbNodes, Types)||{Name, Types}<-Tables]||
	Tables<-[ComFmAlarm]],

    %% Internal

    %% This table can be used anywhere
    {atomic, ok} =
	clhI:mnesia_create_table(comsaVariables,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes, [key, value]} |
				  add_clh_option(comsaVariables)]),

    %% This table is for comsaTransactionServer
    {atomic, ok} =
	clhI:mnesia_create_table(comsaCallbacks,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes, [key, value]} |
				  add_clh_option(comsaCallbacks)]),

    %% This table is for comsaServer
    {atomic, ok} =
	clhI:mnesia_create_table(comsaSchemaLocations,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes, record_info(
						 fields,
						 comsaSchemaLocations)} |
				  add_clh_option(comsaSchemaLocations)]),

    %% These tables are for the use of comsaEcimModelAdaptor
    {atomic, ok} =
	clhI:mnesia_create_table(comsaEcimTypes,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes, record_info(fields,
							   comsaEcimTypes)} |
				  add_clh_option(comsaEcimTypes)]),

    {atomic, ok} =
	clhI:mnesia_create_table(comsaEcimRelations,
				 [{type, bag},
				  {disc_copies, DbNodes},
				  {attributes, record_info(fields,
							   comsaEcimRelations)}|
				  add_clh_option(comsaEcimRelations)]),

    {atomic, ok} =
	clhI:mnesia_create_table(comsaEcimImplementationModel,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes,
				   record_info(fields,
					       comsaEcimImplementationModel)} |
				  add_clh_option(comsaEcimImplementationModel)]),

    {atomic, ok} =
	clhI:mnesia_create_table(comsaEcimClassTypes,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes,
				   record_info(fields, comsaEcimClassTypes)} |
				  add_clh_option(comsaEcimClassTypes)]),

    {atomic, ok} =
	clhI:mnesia_create_table(comsaEcimModelByRootRdn,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes,
				   record_info(fields,
					       comsaEcimModelByRootRdn)} |
				  add_clh_option(comsaEcimModelByRootRdn)]),

    %% End of comsaEcimModelAdaptor tables

    {atomic, ok} =
	clhI:mnesia_create_table(comsaEvent,
				 [{type, set},
				  {disc_copies, DbNodes},
				  {attributes, record_info(fields,
							   comsaEvent)} |
				  add_clh_option(comsaEvent)]),

    %% RAM table to track the alarms in comsaNtpServer
    {atomic, ok} =
	clhI:mnesia_create_table(comsa_ntp_alarms,
				 [{type, set},
				  {attributes, [key, value]},
				  {ram_copies, DbNodes} |
				  add_clh_option(comsa_ntp_alarms)]),

    {atomic, ok} =
	clhI:mnesia_create_table(fmAlarmType_notReg,
				 [{type, set},
				  {attributes, record_info(fields,
							   fmAlarmType_notReg)},
				  {ram_copies, DbNodes} |
				  add_clh_option(fmAlarmType_notReg)]),
    comsaServDiscServer:init_tables(DbNodes),

    {atomic, ok} =
	clhI:mnesia_create_table(comsaAppAlarm,
				 [{type, set},
				  {attributes, record_info(fields,
							   comsaAppAlarm)},
				  {ram_copies, DbNodes} |
				  add_clh_option(comsaAppAlarm)]),

    ok.

create_table(Name, DbNodes, Types) ->
    Fields = [Field||{Field, _}<-Types],
    {atomic, ok} =
	clhI:mnesia_create_table(Name, [{type, set},
					{disc_copies, DbNodes},
					{attributes, Fields} |
					add_clh_option(Name)]).

create_ordered_table(Name, DbNodes, Types) ->
    Fields = [Field||{Field, _}<-Types],
    {atomic, ok} =
	clhI:mnesia_create_table(Name, [{type, ordered_set},
					{disc_copies, DbNodes},
					{attributes, Fields} |
					add_clh_option(Name)]).

%%% ###########################################################################
%%% add_clh_option
%%%
%%% Add option clh_changeCopyType_installation for large tables in order for
%%% CLH to optimize disc operations. See also edoc for clhI.
%%%
%%% ###=====================================================================###
add_clh_option(_) ->
    [].

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhSeqBeg_init_data() ->
%%% CREATE SYSTEM CREATED OBJECTS IN COM MIM FRAGMENTS
    comTop:init_data(start_mode()),
    ok.

%%% ###########################################################################
%%% start_mode
%%%
%%% ###=====================================================================###
start_mode() ->
    start_mode(swmI:is_upgrade_ongoing()).

start_mode(true) ->
    upgrade;
start_mode(_) ->
    fromScratch.

%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init_data() ->
%%% CREATE SYSTEM CREATED OBJECTS IN COM MIM FRAGMENTS

    StartMode = start_mode(),
    rcsSecM:init_data(StartMode),
    comSysM:init_data(StartMode),

    case sysEnv:rcs_mode_2() of
	vrcs -> rmeExeR:init_data(StartMode);
	_ -> ok
    end,
    rmeSds:init_data(StartMode),
    comsaServDiscServer:init_data(),
    comSnmp:init_data(StartMode),
    comFm:init_data(StartMode),
    comsaUser:init_data(StartMode),
    swmI:register_appdata_receiver("alarm", comFm),
    swmI:register_appdata_receiver("authorization", comsaAuth),

    sysServer:register_file_owner("comsa", comsaI),
    sysServer:register_file_owner("comte", comsaI),

    ComModelP = filename:join(
		  [sysEnv:com_top(), "opt", "com", "etc", "model", "*.xml"]),
    ComModels = filelib:wildcard(ComModelP),
    Models = [swmI:find_file(Path) || Path <- ComModels,
				      is_path_ok(Path)],
    comsaEcimModelAdaptor:parse_models(Models, mnesia),


    OptList2 = [{maxSize, 1}, {rotatingSegments, 3}, {public, false}],
    logI:create_log("ComInterfaceLog", OptList2),
    logI:register_esi_dir(comsaLib:esi_dir()),
    logI:register_esi_cb(comsaLib),
    logI:register_esi_dir(filename:join([sysEnv:vnf_dir(), "comte"])).


is_path_ok(Path) ->
    case filename:basename(Path) of
	"ComSysM.xml" ->
	    false;
	"ComSnmp.xml" ->
	    false;
	"ComSecM.xml" ->
	    false;
	"ComLocalAuthorization.xml" ->
	    false;
        "ComFileTPM.xml" ->
            false;
	_ ->
	    true
    end.


%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_post_init() ->
    comsaI:register_subscriptions(
      "ComFm", [{"Fm", fm},
		{"FmAlarmModel", fmAlarmModel},
		{"FmAlarmType", fmAlarmType},
		{"FmAlarm", fmAlarm}]),
    comsaI:register_subscriptions(
      "RcsLocalAuthorization", [{"Role", role},
				{"Rule", rule},
				{"CustomRole", customRole},
				{"CustomRule", customRule},
				{"LocalAuthorizationMethod",
				 localAuthorizationMethod}]),
    comsaI:register_subscriptions(
      "RcsSnmp", [{"Snmp", snmp},
		  {"SnmpTargetV1", snmpTargetV1},
		  {"SnmpTargetV2C", snmpTargetV2C},
		  {"SnmpTargetV3", snmpTargetV3},
          {"SnmpTargetV3Dtls", snmpTargetV3Dtls},
		  {"SnmpViewV1", snmpViewV1},
		  {"SnmpViewV2C", snmpViewV2C},
		  {"SnmpViewV3", snmpViewV3}]),
    comsaI:register_subscriptions(
      "RcsSysM", [{"SysM", sysM},
		  {"NtpServer", ntpServer},
		  {"Schema", sysMSchema},
		  {"NetconfSsh", netconfSsh},
		  {"NetconfTls", netconfTls},
		  {"CliSsh", cliSsh},
		  {"CliTls", cliTls},
		  {"OamTrafficClass", oamTrafficClass}]),
    case sysEnv:rcs_mode_2() of
	vrcs ->
	    comsaI:register_subscriptions(
	      "RmeExeR", [{"ExecutionResource", executionResource}]);
	_ -> % target| simulated
	    ok
    end,
    comsaServDiscServer:post_init(),
    comsaI:register_subscriptions(
      "RcsFileTPM", [{"FileTPM", fileTPM},
		     {"FtpTls", ftpTls},
             {"FtpServer", ftpServer},
		     {"FtpTlsServer", ftpTlsServer},
		     {"Sftp", sftp},
		     {"SftpServer", sftpServer}]),
    %% Transport is handled through IMM
    %% Legacy is not part of the "real "model
    comsaI:register_subscriptions(
      "ComTop", [{"ManagedElement", managedElement},
		 {"SystemFunctions", systemFunctions}]),

    appmI:register_warm_cb(comsaLedControl),
    appmI:register_warm_cb(comsaServDiscServer),

    appmI:register_pgroup_cb(comsaAlarm),

    NodeType = swmI:node_type(),

    _Result =
        case NodeType of
            "vSD" ->
                swmLib:register_upg_callback(comsaServDiscServer);
        _ -> ok
        end.


%% @doc COMTE install, TODO, -spec
install_comte(ApplMims, CliSharedLibs) ->
    case comsaLib:get_variable(comte_installed) of
	true ->
	    ok;
	undefined ->
	    install_comte_int(ApplMims, CliSharedLibs),
	    comsaLib:set_variable(comte_installed, true)
    end.

%% @doc TODO, -spec
install_comte_int(ApplMims, CliSharedLibs) ->

    T0 = ?MonoTime,
    ReleasesVsnDir = sysEnv:releases_vsn_dir(),
    ComteDir = filename:join(ReleasesVsnDir, "comte"),
    filelib:ensure_dir(filename:join(ComteDir, "x")),
    ComteLogDir = comsaLib:comte_log_dir(),


    ComTop = sysEnv:com_top(),
    ComeaDir = filename:join([ComTop, "opt","com","comea"]),
    ComeaConfDir = filename:join([ComteDir,"comea"]),
    ComeaRunDir = filename:join([ComteDir,"comea","run"]),
    ComeaSnmpAgentxSocket = filename:join([ComeaRunDir, "agentx-socket"]),
    ComeaSnmpPwdFile = filename:join([ComeaRunDir, "snmpPwdFile"]),

    MwDir =
	filename:dirname(filename:dirname(filename:dirname(code:priv_dir(comsa)))),
    ModelP = filename:join([MwDir, "*", "*", "priv", "model", "*"]),
    NodeType = proplists:get_value(type, swmI:get_current_up_metadata()),
    Excluded = get_excluded_models(NodeType),

    info_msg("NodeType = ~p~nExcluded models = ~p~n",[NodeType, Excluded]),

    RcsModels = find_models(ModelP, Excluded),

    T1 = ?MonoTime,
    RcsModelsResolved = [swmI:find_file(Path) || Path <- RcsModels],
    T2 = ?MonoTime,
    comsaEcimModelAdaptor:parse_models(RcsModelsResolved, mnesia),
    T3 = ?MonoTime,
    ApplModelsResolved = [swmI:find_file(Path) || Path <- ApplMims],
    T4 = ?MonoTime,
    comsaEcimModelAdaptor:parse_models(ApplModelsResolved, imm),
    T5 = ?MonoTime,

    {ok, ComteLib} = application:get_env(comte,comte_lib),
    {ok, ComRunDir} = application:get_env(comte,com_run_dir),

    Comps =
	[{[<<"ManagedElement">>], {"OamComtEComponent",
				   [{"MafOamSpiManagedObject", 3},
				    {"MafOamSpiTransactionalResource", 1}]},
	  [{"disableNotifications", true}]},
	 {[<<"FmAlarm">>], {"ComFmMoImplComponent",
			    [{"MafOamSpiManagedObject", 1},
			     {"MafOamSpiTransactionalResource", 1}]}}],

    OamSas =
	%% COM5.0
	%%	["libcli_basic",  %pre COM 5.0
	["libcli_basic_component",
	 "libcom_cli_agent",
	 "libcli_pipe",
	 "libcom_common_component",
	 "libcom_ext_cm_router_service",
	 "libcom_fm",
	 "libcom_fm_snmp",
	 "libcom_netconf_agent",
	 "libcom_notification_service",
	 "libcom_security_mgmt_service",
	 "libcom_visibility_controller",
	 "libmaf_session_management_component",
 	 "libmaf_svs"
	]
        ++ [{CliSharedLib, ""} || CliSharedLib <- CliSharedLibs],
    {Arch,_} = sysEnv:architecture(),
    UsrLib =
	case {Arch, sysEnv:vrcs()} of
	    {"x86_64",true} -> filename:join([ComTop, "usr", "lib64"]); %vrcs
	    {"i686",false} ->
		case sysEnv:sim32() of
		    true ->
			filename:join([ComTop, "usr", "lib"]); %sim32
		    false ->
			filename:join([ComTop, "usr", "lib64"]) %rcssim
		end;
	    _ -> filename:join([ComTop, "usr", "lib"])
	end,

	CertPrivDir = code:priv_dir(cert),
	{Architecture, Lib} =
	case {sysEnv:architecture(),os:getenv("ARMDIR")} of
	    {{Archi, LibTag}, false} ->
		 {Archi, LibTag};
	    {{"arm", LibTag}, Dir} ->
	     {Dir, LibTag}
	end,
    Tgt = "tgt_"++ Architecture,
    TgtLibDir = filename:join([CertPrivDir, Tgt]),
    CertLibDir = filename:join([TgtLibDir, Lib]),

    LibDirs =
	sysEnv:dev_patches_dir() ++ ":"
	++ UsrLib ++  ":" ++ CertLibDir
	++ case {sysEnv:target(),Arch}  of
	       {true,_} -> "";
	       {_,"i686"} -> "";
	       {_,"x86_64"} -> "";
	       {false, _} ->
		   ":/usr/lib/perl5/5.10.0/x86_64-linux-thread-multi/CORE/"
	   end,
    SnmpLibVersion = case (sysEnv:target() or sysEnv:proot() or sysEnv:vrcs()) of
    			 true -> 30;
    			 _    ->
    			     case filelib:wildcard("/usr/lib64/libnetsnmp.so.*") of
    				 [H|_] ->
    				     "/usr/lib64/libnetsnmp.so."++Rest = H,
    				     sysInitI:info_msg(
    				       "comsaDataInit:Found snmpLib version ~p~n",[Rest]),
    				     list_to_integer(Rest);
    				 _ ->
    				     15
    			     end
    		     end,

    ComteConfItems =
	[{cli_welcome_message, comsaI:get_cli_welcome_message()},
	 {com_top, ComTop},
	 {comte_lib, ComteLib },
	 {com_conf_dir, ComteDir},
	 {com_run_dir, ComRunDir},
	 {comea_dir, ComeaDir},
	 {comea_conf_dir, ComeaConfDir},
	 {comea_snmp_agentx_socket, ComeaSnmpAgentxSocket},
	 {comea_snmp_pwd_file, ComeaSnmpPwdFile},
	 {com_default_models, ["ComTop","ComFm"]},
	 {com_oam_components, Comps},
	 {com_models, RcsModelsResolved++ApplModelsResolved},
	 {com_default_oam_sas, OamSas},
	 {com_ip, "localhost"},
	 {comte_ip, "localhost"},
	 {ld_library_path, LibDirs},
	 {log_dir, ComteLogDir},
	 {comte_logging, true},
	 {comte_logging_level, 3},
	 {com_logging_level, 3},
	 {comte_log_force_facilities,[100,101,13]}, %alarm, alert, audit-trail
	 {com_oam_sas_config,
	  [{{"ComFmComponent", "transientRaiseAlarmFilterTime"}, 4},
	   {{"ComFmComponent", "transientCeaseAlarmFilterTime"}, 4},
	   {{"ComFmComponent", "alarmToggleTriggerCount"}, 3},
	   {{"ComFmComponent", "alarmToggleWindowSize"}, 60},
	   {{"ComFmComponent", "alarmToggleSilentTimeout"}, 180},
	   {{"ComFmSnmp", "netSnmpLibMajorVersion"}, SnmpLibVersion},
	   {{"ComFmSnmp", "useInternalTrapListener"}, false},
	   {{"ComFmSnmp", "ipv6PortWorkaroudEnabled"}, false},
	   {{"ComFmSnmp", "ipVersion"}, "IPv4"},
	   {{"ComCliAgent", "connectionTimeOut"}, 900}
	  ]}

	] ++
	begin
	    {ok, Data} = sysEnv:get_port_conf(),
	    ComPort = sysEnv:get_port_conf(comPort, Data),
	    ComtePort = sysEnv:get_port_conf(comtePort, Data),
	    ComNetconfPort = sysEnv:get_port_conf(comNetconfPort, Data),
	    ComCliPort = sysEnv:get_port_conf(comCliPort, Data),
	    [{com_port, ComPort},
	     {comte_port, ComtePort},
	     {netconf_port, ComNetconfPort},
	     {cli_port, ComCliPort}]
	end,

    T6 = ?MonoTime,

    EnvItems = comte_com_config:install(ComteConfItems),
    T7 = ?MonoTime,
    ?LOG_INFO([{{'RcsModelsResolved', length(RcsModelsResolved)},
		sysUtil:time_to_string(T2 -T1)},
	       {'parse_models mnesia', sysUtil:time_to_string(T3 - T2)},
	       {{'ApplModelsResolved', length(ApplModelsResolved)},
		sysUtil:time_to_string(T4 - T3)},
	       {'parse_models imm', sysUtil:time_to_string(T5 -T4)},
	       {'comte:install', sysUtil:time_to_string(T7 -T6)},
	       "-------------- Summary --------------",
	       {'TOTAL', sysUtil:time_to_string(T7 - T0)}]),
    EnvItems.

%%% ----------------------------------------------------------

find_models(ModelP, Excluded) ->
    Models = filelib:wildcard(ModelP),
    [Model||Model<-Models, not is_excluded(Model, Excluded)].

is_excluded(Model, Excluded) ->
    case lists:member(filename:basename(Model), Excluded) of
	false -> false;
	true ->
	    ?LOG_INFO([{"MimInfo not registered",
			"Model not applicable on this Node Type."},
		       {model, Model},
		       {node_type, swmI:node_type()}]),
	    true
    end.


get_excluded_models(NodeType) when NodeType=="vRC";
				   NodeType=="vPP" ->
    ["RcsSwM.xml","RcsOamAccessPoint.xml","RmeSdsServer.xml"];
get_excluded_models(NodeType) when NodeType=="R-VNFM"->
    ["RcsSwM.xml",
     "RcsPm.xml", "RmePmSupport.xml",
     "RcsPMEventM.xml",
     "RmeSds.xml",
     "RmeSdsServer.xml",
     "RcsOamAccessPoint.xml"];
get_excluded_models(NodeType) when NodeType=="vSD" ->
    ["RcsSwM.xml",
     "RcsPMEventM.xml",
     "RcsOamAccessPoint.xml",
     "RmeSdsServer.xml"];
%% get_excluded_models(NodeType) when NodeType=="RadioNode";
%% 				   NodeType=="VtfRadioNode";
%% 				   NodeType=="RadioTNode" ->
%%     ["RmeExeR.xml", "RmeSds.xml","RcsOamAccessPoint.xml"];
get_excluded_models(NodeType) when NodeType=="5GRadioNode";
				   NodeType=="RnNode" ;
				   NodeType=="RadioNode";
				   NodeType=="VtfRadioNode";
				   NodeType=="RadioTNode"->
    ["RmeExeR.xml","RcsOamAccessPoint.xml","RmeSdsServer.xml"];
get_excluded_models(NodeType) ->
    warning_msg("Unknown node type: ~p No excluded models ~n",[NodeType]),
    legacy_get_excluded_models().


%% Legacy variant temporarily kept until Node-CI has fixed all issues

legacy_get_excluded_models() ->
    ["RmeSdsServer.xml", "RcsOamAccessPoint.xml" |
     case sysEnv:rcs_mode_2() of
	 vrcs ->
	     ["RcsSwM.xml"];
	 _ -> % target, simulated
	     ["RmeExeR.xml"]
     end ++
	 case comsaI:has_consul() of
	     true ->
		 [];
	     false ->
		 ["RmeSds.xml", "RmeSdsServer.xml"]
	end].


%%% #------------------------------------------------------------------------------
%%% This is a poor man's version of a StartPhase, originating from sysApp.
activate() ->
    try
	comsaServer:activate()
    catch
	ErrClass : ErrReason ->
	    ?LOG_ERR([{ErrClass, ErrReason},
		      {stacktrace, erlang:get_stacktrace()}])
    end,
    ok.

get_default_cli_welcome_message() ->
    %% Default text
%%%  1234567890123456789012345678901234567890123456789012345678901234567890 (80)
    "This system is restricted solely to authorized users for legitimate\n"
    "business purposes only. The actual or attempted unauthorized access,\n"
    "use, or modification of this system is strictly prohibited.\n"
    "\n"
    "Unauthorized users are subject to appropriate disciplinary proceedings\n"
    "and/or criminal and civil penalties under state, federal, or other\n"
    "applicable domestic and foreign laws.\n"
    "\n"
    "The use of this system may be monitored and recorded for\n"
    "administrative and security reasons. Anyone accessing this system\n"
    "expressly consents to such monitoring and is advised that if\n"
    "monitoring reveals possible evidence of criminal activity, the owner\n"
    "of this equipment may provide the evidence of such activity to law\n"
    "enforcement officials.\n"
    "\n"
    "All authorized users shall comply with the security policies,\n"
    "instructions and requirements related to the business purpose and in\n"
    "case of doubt shall seek advice from his/her manager.\n".

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
%% info_msg(Format) ->
%%     info_msg(Format, []).
info_msg(Format, Args) ->
    sysInitI:info_msg("~w: "++Format, [?MODULE|Args]).

%% error_msg(Format) ->
%%     error_msg(Format, []).
%% error_msg(Format, Args) ->
%%     sysInitI:error_msg("~w: "++Format, [?MODULE|Args]).

%% warning_msg(Format) ->
%%     warning_msg(Format, []).
warning_msg(Format, Args) ->
    sysInitI:warning_msg("~w: "++Format, [?MODULE|Args]).

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
