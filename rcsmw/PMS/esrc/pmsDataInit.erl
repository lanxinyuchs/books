%%% ----------------------------------------------------------
%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	pmsDataInit.erl %
%%% Author:	etxjotj
%%% Description:     
%%%
%%% Modules used:    
%%%
%%% ----------------------------------------------------------
-module(pmsDataInit).
-vsn('/main/R1A/R2A/R3A/R4A/R5A/R6A/4').
-date('2016-08-31').
-author('uabesvi').
-shaid('31926b2842acde35ca6ced9bb555a0a34bc2eeb4').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: 53/002 01-LXA 119 334 Ux, Rev: /main/4 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2012-2016 All rights reserved.
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
%%% Rev     Date       Name     What
%%% -----   -------    -------  ------------------------
%%% R1A/1   2012-01-18 etxjotj  Imported from git
%%% R5A/1   2015-10-08 etxjotj  ECIM PM 2.3
%%% R5A/5   2015-12-22 uabesvi  PmSupport
%%% R5A/6   2016-01-07 etxberb  Changed installation phases in all blocks.
%%% R5A/7   2016-01-11 etxberb  Added old installation phase functions for
%%%                             backwards compatibility reasons (explicit calls
%%%                             from $RDE_TOP/tools/mkcpi/mkcpi.escript)
%%% R5A/10  2016-02-02 uabesvi  max no of ROP files = 400
%%% R5A/11  2016-02-11 eolaand  Move registration of ROP-file sftp dir
%%%                             from pmsServer to init_data.
%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([instPhParallel_init/1,
	 instPhParallel_init_data/0,
	 instPhParallel_post_init/0]).
-export([init/1,
	 init_data/0]).
-export([activate/0]).
-export([children/0]).

%%-compile(export_all).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------


-include("pms.hrl").
-include("RcsPm.hrl").
-include("RmePmSupport.hrl").


-define(PM, #pm{pmId = {"1","1","1"}}).


%% NOTE: if these values are changed do not forget to
%%       update pms2_rop_uc_SUITE:tc_pm_capabilities
-define(MEASUREMENT_CAPABILITIES,
	#pmMeasurementCapabilities
	{pmMeasurementCapabilitiesId = {"1","1","1","1"},
	 maxNoOfJobs                 = 50,
	 jobStartStopSupport         = ?JobStartStopSupport_BASIC,
	 finalROP                    = false,
	 jobPrioritizationSupport    = false,
	 maxNoOfMeasurements         = undefined,
	 maxNoOfPmFiles              = 400,
	 alignedReportingPeriod      = true,
	 measurementJobSupport       = true,
	 realTimeJobSupport          = false,
	 thresholdJobSupport         = false,
	 fileLocation                = "/rop",
	 fileRPSupported             = false,
	 supportedRopPeriods         = [?TimePeriod_FIFTEEN_MIN],
	 supportedMeasJobGps         = [?TimePeriod_FIFTEEN_MIN],
	 supportedRtJobGps           = undefined,
	 supportedThreshJobGps       = undefined,
	 supportedCompressionTypes   = [?CompressionTypes_GZIP],
	 jobGroupingSupport          = true,
	 producesUtcRopFiles         = true,
	 ropFilenameTimestamp        = ?RopFilenameTimestamp_UTC_NO_OFFSET
	}).

-define(MNESIA_SET(__Attrs), [{type,        set},
			      {attributes,  __Attrs}]).
-define(MNESIA_SET_DISC(__Attrs, __Nodes), [{disc_copies, __Nodes},
					    {type,        set},
					    {attributes,  __Attrs}]).
-define(MNESIA_BAG(__Attrs), [{type,        bag},
			      {attributes,  __Attrs}]).
-define(MNESIA_BAG_DISC(__Attrs, __Nodes), [{disc_copies, __Nodes},
					    {type,        bag},
					    {attributes,  __Attrs}]).
-define(ROP_SIZE, 30000). % ROP-file dir size in KB

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
    Tabs = [{pmsAppsInfo, record_info(fields, pmsAppsInfo)}],

    [pmsDb:create_table(Name, ?MNESIA_SET(Fields)) || {Name, Fields} <- Tabs],
    
    Bags = [{pmsAppRegistry, record_info(fields, pmsAppRegistry)},
	    {pmsScAppMoLdns, record_info(fields, pmsScAppMoLdns)}
	   ],
    [pmsDb:create_table(Name, ?MNESIA_BAG(Fields)) || {Name, Fields} <- Bags],
    
    DiscBags = [{pmsScMoClasses, record_info(fields, pmsScMoClasses)}
	       ],
    [pmsDb:create_table(Name, ?MNESIA_BAG_DISC(Fields, DbNodes)) || 
	{Name, Fields} <- DiscBags],
    
    DiscTabs = 
	[{pm,                        i_f(?pm_types)},
	 {pmSupport,                 record_info(fields, pmSupport)},
	 {pmMeasurementCapabilities, i_f(?pmMeasurementCapabilities_types)},
	 {pmGroup,                   i_f(?pmGroup_types)},
	 {measurementType,           i_f(?measurementType_types)},
	 {pmThresholdMonitoring,     i_f(?pmThresholdMonitoring_types)},
	 {pmJob,                     i_f(?pmJob_types)},
	 {measurementReader,         i_f(?measurementReader_types)},
	 {pmsCounterAliases,         record_info(fields, pmsCounterAliases)},
	 {pmsEnv,                    record_info(fields, pmsEnv)}
	],
    [pmsDb:create_table(Name, ?MNESIA_SET_DISC(Fields, DbNodes)) || 
	{Name, Fields} <- DiscTabs],

    %% init the max length used in pmslog
    %% this must be done because LRAT has many many counters
    %% and that would fill the log in no time
    pmsDb:pms_env_set(log_max, ?LOG_MAX),
    ok.

%% Old installation phase - kept due to explicit call from
%% $RDE_TOP/tools/mkcpi/mkcpi.escript
init(DbNodes) ->
    instPhParallel_init(DbNodes).


i_f(Types) ->
    [Field || {Field, _} <- Types].



children() ->
    {ok, [{pmsServer, {pmsServer, start, []},
	   permanent, 1000, worker, [pmsServer]},
	  {pmsAppRegistry, {pmsAppRegistry, start, []},
	   permanent, 1000, worker, [pmsAppRegistry]}]}.

activate() ->
    %%================================================================
    %% register callback module for show counters
    %%================================================================
    %%comte:register_callback(pm, pmsShowCountersI),

    appmI:register_warm_cb(pmsI),  

    cec:register("PMI",  pmsSession),
    cec:register("PMI2", pmsSession2),
    PM = [<<"ManagedElement">>, <<"SystemFunctions">>, <<"Pm">>],
    ok = comsaI:register_callback(PM, pmsComteI),
    PS = [<<"ManagedElement">>, <<"NodeSupport">>, <<"PmSupport">>],
    ok = comsaI:register_callback(PS, pmsComteI).
    



%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_post_init() ->
    %%================================================================
    %% Reformat records, if needed.
    %%================================================================
    pmsDataUpgrade:upgrade(swmI:is_upgrade_ongoing()),

    %%================================================================
    %% Register ROP-file dir as ESI dir.
    %%================================================================
    ok = logI:register_esi_dir(pmsSftpdEnv:rop_dir_path()),

    comsaI:register_subscriptions(
      "RcsPm", [{"PmJob",                     pmJob}, 
		{"MeasurementReader",         measurementReader},
		{"MeasurementType",           measurementType},
		{"PmGroup",                   pmGroup},
		{"PmMeasurementCapabilities", pmMeasurementCapabilities},
		{"PmThresholdMonitoring",     pmThresholdMonitoring}
	       ]),

    comsaI:register_subscriptions(
      "RmePmSupport", [{"PmSupport", pmSupport}]).


%%% ###########################################################################
%%  Phases for installation and upgrade:
%%  See ../SYS/SYS_CNX9012620/SYS_CAX1033072/esrc/sysdb/sysDbServer.erl
%%% ###=====================================================================###
instPhParallel_init_data() ->
    %%================================================================
    %% init things
    %%================================================================
    %% create_rop_dir(),
    swmI:register_appdata_receiver("pms",           pmsAppData),
    swmI:register_appdata_receiver("pmsCounter",    pmsAppData),
    swmI:register_appdata_receiver("pmsJob",        pmsAppData),
    swmI:register_appdata_receiver("pmsGroupAlias", pmsAppData),

    %%================================================================
    %% create Pm
    %%================================================================
    pmsDb:pm_set(?PM),
    pmsDb:pm_support_set(?FileHandlingMethod_SINGLE_ROP_FILE),

    %%================================================================
    %% create PmMeasurementCapabilities
    %%================================================================
    pmsDb:meas_capabilities_set(?MEASUREMENT_CAPABILITIES),

    %%================================================================
    %% inhibit other than 15 min RP/GP
    %%================================================================
    pmsDb:pms_env_set(reporting_period, legacy),

    %%================================================================
    %% Register sftp dir for ROP-files
    %%================================================================
    RopDir = pmsSftpdEnv:rop_dir(),
    FileHandler = pmsSftpdEnv:file_handler(),
    sysFi:register_sftp_dir(RopDir, ?ROP_SIZE, FileHandler),

    %%================================================================
    %% no filtering on RcsPmCounter log
    %%================================================================
%%     Filters = 
%% 	[warning, misc, db, process, interface, rop, loop, sc, coli],
    Filters = os:getenv("rcs_pms_counter_filters", "off"),
    pmsDb:pms_env_set(counter_filters, list_to_atom(Filters)),

    ok.

%% Old installation phase - kept due to explicit call from
%% $RDE_TOP/tools/mkcpi/mkcpi.escript
init_data() ->
    instPhParallel_init_data().

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------


%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------

%% create_rop_dir() ->
%%     Dirs = [pmsSftpdEnv:root_dir(), 
%% 	    pmsSftpdEnv:rop_dir()],
%%     lists:foreach(fun file:make_dir/1, Dirs).



%%% ----------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------

