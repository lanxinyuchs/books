%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	alhI.erl %
%%% @author etxberb
%%% @copyright Ericsson AB 2012-2017
%%% @version /main/R2A/R3A/R4A/R5A/R9A/R10A/R11A/2
%%%
%%% @doc == ALH Service ==
%%% This module implements the user API of the RBS CS Availability Log Service.
%%% @end
%%% ----------------------------------------------------------
-module(alhI).
-id('Updated by CCase').
-vsn('/main/R2A/R3A/R4A/R5A/R9A/R10A/R11A/2').
-date('2017-10-16').
-author('etxberb').
%%% ----------------------------------------------------------
%%% %CCaseTemplateFile:	module.erl %
%%% %CCaseTemplateId: CCver: /main/2 %
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
%%% R2A/1      2012-12-10 eolaand     Created
%%% R2A/9      2014-01-24 etxberb     Introduced AVLI version 4.
%%% R2A/10     2014-01-28 etxberb     Added AdditionalInfo & AvailabilityInfo in
%%%                                   the Erlang (alhI) Interface.
%%% R2A/21     2014-04-09 etxberb     Added WRITE_ASYNC.
%%% R3A/1      2015-03-27 etxberb     Added swm_upgrWindow_active/0.
%%% R4A/1      2015-08-20 etxberb     Added warm_restart/1.
%%% R9A/1      2017-04-04 uabesvi     Added dump_to_file/1.
%%% R11A/1     2017-10-01 etxberb     SP531: Add start_traffic/1,stop_traffic/3,
%%%                                   upgrade_activated/0, upgrade_confirmed/0.
%%% R11A/2     2017-10-16 etxberb     Adaptions to OTP20.
%%% ----------------------------------------------------------
%%% 
%%% #2.    EXPORT LISTS
%%% ----------------------------------------------------------
%%% #2.1   EXPORTED INTERFACE FUNCTIONS
%%% ----------------------------------------------------------
-export([write_node_event/3,
	 write_node_event/4,
	 write_node_event/5,
	 write_node_event/6,
	 write_piu_event/6,
	 write_piu_event/7,
	 write_piu_event/8,
	 write_hw_event/6,
	 write_hw_event/7,
	 write_hw_event/8,
	 write_service_event/5,
	 write_service_event/6,
	 write_service_event/7,
	 write_other_event/3,
	 write_other_event/4,
	 write_other_event/5,
	 write_pgm_event/6,
	 write_pgm_event/7,
	 write_pgm_event/8
	]).

%% From SWM REST interface
-export([start_traffic/1,
	 stop_traffic/3,
	 upgrade_activated/0,
	 upgrade_confirmed/0]).

-export([default_log_file/0,
	 default_log_dir/0,
	 generate_esi/0,
	 generate_log/0,
	 export_log/0, 
	 export_log/1, 
	 export_log/2,
	 export_log/3,
	 get_log/0,
	 get_log/1,
	 reset_log/0]).

-export([print_from/1,
	 print_from/2]).
-export([print_clientInfo_from/1,
	 print_clientInfo_from/2]).
-export([print_latest/0,
	 print_latest/1]).
-export([print_oldest/0,
	 print_oldest/1]).

-export([swm_upgrWindow_active/0]).

-export([warm_restart/1]).

-export([dump_to_file/1]).
-export([push_from_file/1]).

%%% ----------------------------------------------------------
%%% #2.2   EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------
%%% For debug and test
-export([examples/0]).

%%% #3.    CODE
%%% #---------------------------------------------------------
%%% #3.1   CODE FOR EXPORTED INTERFACE FUNCTIONS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% Include files
%%% ----------------------------------------------------------
-include("alh_service.hrl").
-include("alhI.hrl").

%%% ----------------------------------------------------------
%%% Macros
%%% ----------------------------------------------------------
-define(LOG_DIR, alh_service:default_log_dir()).
-define(LOG_FILE, alh_service:default_log_file()).
-define(TIME(TS), alh_service:to_datetime(TS)).
-define(WRITE(E, P), alh_service:write_event(E, P)).
-define(WRITE_ASYNC(E, P), alh_service:write_event_async(E, P)).

-define(CHECK_FAIL(Reason), {check_fail, Reason}).
-define(THROW_CHECK_FAIL(Reason), throw(?CHECK_FAIL(Reason))).

%%% ----------------------------------------------------------
%%% Type specs
%%% ----------------------------------------------------------
-type w_ret_type() :: ok | {error, illegal_param}.
-type now_ts() :: {MegSec::integer(), Sec::integer(), MicSec::integer()} | 
		  integer() .
%%% Result of os:timestamp() or seconds since Jan 1 1970 00:00:00.
-type string_60() :: string().
-type string_100() :: string().
%%% The maximum allowed length is 60 characters.
-type service_status() :: 'InService' | 'OutOfService' | 
			  'PartiallyOutOfService' | undefined.
-type reason() :: 'ShutdownCommand' | 'UnOperational' | 'Starting' | 
		  'Operational' | undefined.
-type event_id() :: integer().
-type cause() :: string().
-type appLog() :: {'Cause', cause()} |
                  {'InfoText', string()} |
                  'RankCold' |
                  'RankWarm' |
                  'RankColdWTest' |
                  'RestartCompleted' |
                  {'PiuInfo', string()} |
                  {'NodeInfo', string()}.
-type rcsNodeDown() :: {timestamp_DownTime, now_ts()} |
                       {'DownTime', 'Unknown'}.
-type rcs() :: {'Cause', cause()} |
               'RankCold' |
               'RankWarm' |
               'RankColdWTest' |
               {'RcsNodeDown', rcsNodeDown()}.
-type add_info() :: string() |
                    {'AppLog', appLog() | list(appLog())} |
                    {'Rcs', rcs() | list(rcs())}.
%%% The maximum allowed length is 1500 characters.
-type piu_type() :: 'Bp' | 'Mp' | undefined.
-type piu_hw_addr() :: {SwitchModNo::integer(), SwitchPortNo::integer()} |
                       undefined.
-type prod_no() :: string().
%%% The maximum allowed length is 25 characters.
-type prod_rev() :: string().
%%% The maximum allowed length is 8 characters.
-type prod_name() :: string().
%%% The maximum allowed length is 13 characters.
-type hw_pid() :: {ProdNo::prod_no(), ProdRev::prod_rev(), 
		   ProdName::prod_name()}.
-type sw_pid() :: {ProdNo::prod_no(), ProdRev::prod_rev()}.
-type hw_type() :: string_60().
-type hw_address() :: string_60().
-type service_type() :: string_60().
-type service_inst() :: string_100().
-type nodeIdentity() :: {'SiteLocation', string()} |
			{'NetworkManagedElementId', string()} |
			{'ProdNo', string()} |
			{'ProdRev', string()} |
			{'ProdName', string()}.
-type rcsNodeIdentityInfo() :: {'NodeIdReason', string()} |
			       {'NodeIdentity', list(nodeIdentity())} |
			       {'UpgradePackage', string()}.
-type avail_info() :: string() |
                      {'RcsNodeIdentityInfo', list(rcsNodeIdentityInfo())} |
                      {'AppLog', appLog() | list(appLog())}.
%%% The maximum allowed length is 15000 characters.
-type get_log_opt() :: {reset_log, boolean()}.
%%% The option reset_log is used to indicate if the log should be reset after
%%% reading. Default value is false.
-type get_log_opts() :: [Opt::get_log_opt()].
-type export_log_opts() :: [Opt::get_log_opt() | {zip, boolean()} | 
			   {zipfile_ext, string()}].
%%% The option zip is used to indicate if the log should be compressed.
%%% The option zipfile_ext is used to append a file extension to the log file
%%% when compression is used.
%%% Default values are: {zip,false}, {zipfile_ext,".gz"}.
%%% @end
%%%
%%% ----------------------------------------------------------
%%% @doc Write Node Event to the Availability Log. 
%%% @end
%%% ----------------------------------------------------------
-spec write_node_event(TimeStamp     :: now_ts(),
		       ServiceStatus :: service_status(), 
		       Reason        :: reason(),
		       EventId       :: event_id(),
		       AddInfo       :: add_info(),
		       ExecType      :: sync | async
		      ) ->
    w_ret_type().
write_node_event(TimeStamp, ServiceStatus, Reason, EventId, AddInfo, ExecType)->
    try
	check_timestamp(TimeStamp),
	check_service_status(ServiceStatus),
	check_reason(Reason),
	check_event_id(EventId),
	check_add_info(AddInfo),
	case ExecType of
	    sync ->
		?WRITE(?NODE_EVENT, {?TIME(TimeStamp),
				     ServiceStatus,
				     Reason,
				     EventId,
				     AddInfo});
	    async ->
		?WRITE_ASYNC(?NODE_EVENT, {?TIME(TimeStamp),
					   ServiceStatus,
					   Reason,
					   EventId,
					   AddInfo})
	end
    catch
	throw : ?CHECK_FAIL(FailReason) ->
	    ?LOG_ERR([?AVLI_HEADING,
		      {'Verification of indata failed',
		       'No log entry written.'}] ++
		     ['-- Error --' | FailReason] ++
		     ['-- Indata --',
		      {timeStamp, TimeStamp},
		      {serviceStatus, ServiceStatus},
		      {reason, Reason},
		      {eventId, EventId},
		      {addInfo, AddInfo}]),
	    {error, illegal_param};
	ErrClass : ErrReason ->
	    Stacktrace = ?STACKTRACE_E,
	    ?LOG_ERR([?AVLI_HEADING,
		      {ErrClass, ErrReason},
		      {stacktrace, Stacktrace},
		      '-- Indata --',
		      {timeStamp, TimeStamp},
		      {serviceStatus, ServiceStatus},
		      {reason, Reason},
		      {eventId, EventId},
		      {addInfo, AddInfo}]),
	    {error, illegal_param}
    end.

%%% @equiv write_node_event(TimeStamp, ServiceStatus, Reason, EventId, AddInfo, sync)
-spec write_node_event(TimeStamp     :: now_ts(),
		       ServiceStatus :: service_status(), 
		       Reason        :: reason(),
		       EventId       :: event_id(),
		       AddInfo       :: add_info()
		      ) ->
    w_ret_type().
write_node_event(TimeStamp, ServiceStatus, Reason, EventId, AddInfo) ->
    write_node_event(TimeStamp, ServiceStatus, Reason, EventId, AddInfo, sync).

%%% @equiv write_node_event(TimeStamp, ServiceStatus, Reason, 0, AddInfo)
-spec write_node_event(TimeStamp     :: now_ts(),
		       ServiceStatus :: service_status(), 
		       Reason        :: reason(),
		       AddInfo       :: add_info()
		      ) ->
    w_ret_type().
write_node_event(TimeStamp, ServiceStatus, Reason, AddInfo) ->
    write_node_event(TimeStamp, ServiceStatus, Reason, 0, AddInfo).

%%% @equiv write_node_event(os:timestamp(), ServiceStatus, Reason, AddInfo)
-spec write_node_event(ServiceStatus :: service_status(), 
		       Reason        :: reason(),
		       AddInfo       :: add_info()
		      ) ->
    w_ret_type().
write_node_event(ServiceStatus, Reason, AddInfo) ->
    write_node_event(os:timestamp(), ServiceStatus, Reason, AddInfo).

%%% ----------------------------------------------------------
%%% @doc Write PIU Event to the Availability Log. 
%%% @end
%%% ----------------------------------------------------------
-spec write_piu_event(TimeStamp::now_ts(),
		      ServiceStatus::service_status(), 
		      Reason::reason(),
		      PiuType::piu_type(), 
		      PiuHwAddr::piu_hw_addr(),
		      HwPid::hw_pid(), 
		      AddInfo::add_info(),
		      ExecType::sync | async) -> w_ret_type().

write_piu_event(TimeStamp, ServiceStatus, Reason, PiuType, PiuHwAddr, HwPid, 
		AddInfo, ExecType) ->
    try
	check_timestamp(TimeStamp),
	check_service_status(ServiceStatus),
	check_reason(Reason),
	check_piu_type(PiuType),
	check_piu_hw_addr(PiuHwAddr),
	check_hw_pid(HwPid),
	check_add_info(AddInfo),
	case ExecType of
	    sync ->
		?WRITE(?PIU_EVENT, {?TIME(TimeStamp),
				    ServiceStatus,
				    Reason,
				    PiuType, 
				    PiuHwAddr,
				    HwPid,
				    AddInfo});
	    async ->
		?WRITE_ASYNC(?PIU_EVENT, {?TIME(TimeStamp),
					  ServiceStatus,
					  Reason,
					  PiuType, 
					  PiuHwAddr,
					  HwPid,
					  AddInfo})
	end
    catch
	throw : ?CHECK_FAIL(FailReason) ->
	    ?LOG_ERR([?AVLI_HEADING,
		      {'Verification of indata failed',
		       'No log entry written.'}] ++
		     ['-- Error --' | FailReason] ++
		     ['-- Indata --',
		      {timeStamp, TimeStamp},
		      {serviceStatus, ServiceStatus},
		      {reason, Reason},
		      {piuType, PiuType},
		      {piuHwAddr, PiuHwAddr},
		      {hwPid, HwPid},
		      {addInfo, AddInfo}]),
	    {error, illegal_param};
	ErrClass : ErrReason ->
	    Stacktrace = ?STACKTRACE_E,
	    ?LOG_ERR([?AVLI_HEADING,
		      {ErrClass, ErrReason},
		      {stacktrace, Stacktrace},
		      '-- Indata --',
		      {timeStamp, TimeStamp},
		      {serviceStatus, ServiceStatus},
		      {reason, Reason},
		      {piuType, PiuType},
		      {piuHwAddr, PiuHwAddr},
		      {hwPid, HwPid},
		      {addInfo, AddInfo}]),
	    {error, illegal_param}
    end.

%%% @equiv write_piu_event(TimeStamp, ServiceStatus, Reason, PiuType, 
%%%                        PiuHwAddr, HwPid, AddInfo, sync)
-spec write_piu_event(TimeStamp::now_ts(), ServiceStatus::service_status(), 
		      Reason::reason(), PiuType::piu_type(), 
		      PiuHwAddr::piu_hw_addr(), HwPid::hw_pid(), 
		      AddInfo::add_info()) -> w_ret_type().

write_piu_event(TimeStamp, ServiceStatus, Reason, PiuType, PiuHwAddr, HwPid, 
		AddInfo) ->
    write_piu_event(TimeStamp, ServiceStatus, Reason, PiuType, PiuHwAddr, 
		    HwPid, AddInfo, sync).

%%% @equiv write_piu_event(os:timestamp(), ServiceStatus, Reason, PiuType, 
%%%                        PiuHwAddr, HwPid, AddInfo)
-spec write_piu_event(ServiceStatus::service_status(), 
		      Reason::reason(), PiuType::piu_type(), 
		      PiuHwAddr::piu_hw_addr(), HwPid::hw_pid(), 
		      AddInfo::add_info()) -> w_ret_type().

write_piu_event(ServiceStatus, Reason, PiuType, PiuHwAddr, HwPid, 
		AddInfo) ->
    write_piu_event(os:timestamp(), ServiceStatus, Reason, PiuType, PiuHwAddr, 
		    HwPid, AddInfo).

%%% ----------------------------------------------------------
%%% @doc Write HW Event to the Availability Log. 
%%% @end
%%% ----------------------------------------------------------
-spec write_hw_event(TimeStamp::now_ts(),
		     ServiceStatus::service_status(), 
		     Reason::reason(),
		     HwType::hw_type(), 
		     HwAddress::hw_address(),
		     HwPid::hw_pid(), 
		     AddInfo::add_info(),
		     ExecType::sync | async) -> w_ret_type().

write_hw_event(TimeStamp, ServiceStatus, Reason, HwType, HwAddress, HwPid, 
	       AddInfo, ExecType) ->
    try
	check_timestamp(TimeStamp),
	check_service_status(ServiceStatus),
	check_reason(Reason),
	check_hw_type(HwType),
	check_hw_address(HwAddress),
	check_hw_pid(HwPid),
	check_add_info(AddInfo),
	case ExecType of
	    sync ->
		?WRITE(?HW_EVENT, {?TIME(TimeStamp),
				   ServiceStatus,
				   Reason,
				   HwType, 
				   HwAddress,
				   HwPid,
				   AddInfo});
	    async ->
		?WRITE_ASYNC(?HW_EVENT, {?TIME(TimeStamp),
					 ServiceStatus,
					 Reason,
					 HwType, 
					 HwAddress,
					 HwPid,
					 AddInfo})
	end
    catch
	throw : ?CHECK_FAIL(FailReason) ->
	    ?LOG_ERR([?AVLI_HEADING,
		      {'Verification of indata failed',
		       'No log entry written.'}] ++
		     ['-- Error --' | FailReason] ++
		     ['-- Indata --',
		      {timeStamp, TimeStamp},
		      {serviceStatus, ServiceStatus},
		      {reason, Reason},
		      {hwType, HwType},
		      {hwAddress, HwAddress},
		      {hwPid, HwPid},
		      {addInfo, AddInfo}]),
	    {error, illegal_param};
	ErrClass : ErrReason ->
	    Stacktrace = ?STACKTRACE_E,
	    ?LOG_ERR([?AVLI_HEADING,
		      {ErrClass, ErrReason},
		      {stacktrace, Stacktrace},
		      '-- Indata --',
		      {timeStamp, TimeStamp},
		      {serviceStatus, ServiceStatus},
		      {reason, Reason},
		      {hwType, HwType},
		      {hwAddress, HwAddress},
		      {hwPid, HwPid},
		      {addInfo, AddInfo}]),
	    {error, illegal_param}
    end.

%%% @equiv write_hw_event(TimeStamp, ServiceStatus, Reason, HwType, 
%%%                       HwAddress, HwPid, AddInfo, sync)
-spec write_hw_event(TimeStamp::now_ts(), ServiceStatus::service_status(), 
		     Reason::reason(), HwType::hw_type(), 
		     HwAddress::hw_address(), HwPid::hw_pid(), 
		     AddInfo::add_info()) -> w_ret_type().

write_hw_event(TimeStamp, ServiceStatus, Reason, HwType, HwAddress, HwPid, 
	       AddInfo) ->
    write_hw_event(TimeStamp, ServiceStatus, Reason, HwType, HwAddress, 
		   HwPid, AddInfo, sync).

%%% @equiv write_hw_event(os:timestamp(), ServiceStatus, Reason, HwType, 
%%%                       HwAddress, HwPid, AddInfo)
-spec write_hw_event(ServiceStatus::service_status(), 
		     Reason::reason(), HwType::hw_type(), 
		     HwAddress::hw_address(), HwPid::hw_pid(), 
		     AddInfo::add_info()) -> w_ret_type().

write_hw_event(ServiceStatus, Reason, HwType, HwAddress, HwPid, AddInfo) ->
    write_hw_event(os:timestamp(), ServiceStatus, Reason, HwType, HwAddress, 
		   HwPid, AddInfo).


%%% ----------------------------------------------------------
%%% @doc Write Service Event to the Availability Log. 
%%% @end
%%% ----------------------------------------------------------
-spec write_service_event(TimeStamp::now_ts(),
			  ServiceStatus::service_status(), 
			  Reason::reason(),
			  ServiceType::service_type(), 
			  ServiceInstance::service_inst(), 
			  AddInfo::add_info(),
			  ExecType::sync | async) -> w_ret_type().

write_service_event(TimeStamp, ServiceStatus, Reason, ServiceType, 
		    ServiceInstance, AddInfo, ExecType) ->
    try
	check_timestamp(TimeStamp),
	check_service_status(ServiceStatus),
	check_reason(Reason),
	check_service_type(ServiceType),
	check_service_inst(ServiceInstance),
	check_add_info(AddInfo),
	case ExecType of
	    sync ->
		?WRITE(?SERVICE_EVENT, {?TIME(TimeStamp),
					ServiceStatus,
					Reason, 
					ServiceType,
					ServiceInstance,
					AddInfo});
	    async ->
		?WRITE_ASYNC(?SERVICE_EVENT, {?TIME(TimeStamp),
					      ServiceStatus,
					      Reason, 
					      ServiceType,
					      ServiceInstance,
					      AddInfo})
	end
    catch
	throw : ?CHECK_FAIL(FailReason) ->
	    ?LOG_ERR([?AVLI_HEADING,
		      {'Verification of indata failed',
		       'No log entry written.'}] ++
		     ['-- Error --' | FailReason] ++
		     ['-- Indata --',
		      {timeStamp, TimeStamp},
		      {serviceStatus, ServiceStatus},
		      {reason, Reason},
		      {serviceType, ServiceType},
		      {serviceInstance, ServiceInstance},
		      {addInfo, AddInfo}]),
	    {error, illegal_param};
	ErrClass : ErrReason ->
	    Stacktrace = ?STACKTRACE_E,
	    ?LOG_ERR([?AVLI_HEADING,
		      {ErrClass, ErrReason},
		      {stacktrace, Stacktrace},
		      '-- Indata --',
		      {timeStamp, TimeStamp},
		      {serviceStatus, ServiceStatus},
		      {reason, Reason},
		      {serviceType, ServiceType},
		      {serviceInstance, ServiceInstance},
		      {addInfo, AddInfo}]),
	    {error, illegal_param}
    end.

%%% @equiv write_service_event(TimeStamp, ServiceStatus, Reason, 
%%%		               ServiceType, ServiceInstance, AddInfo, sync)
-spec write_service_event(TimeStamp::now_ts(), ServiceStatus::service_status(), 
			  Reason::reason(), ServiceType::service_type(), 
			  ServiceInstance::service_inst(), 
			  AddInfo::add_info()) -> w_ret_type().

write_service_event(TimeStamp, ServiceStatus, Reason, ServiceType, 
		    ServiceInstance, AddInfo) ->
    write_service_event(TimeStamp, ServiceStatus, Reason, ServiceType, 
			ServiceInstance, AddInfo, sync).

%%% @equiv write_service_event(os:timestamp(), ServiceStatus, Reason, 
%%%		               ServiceType, ServiceInstance, AddInfo)
-spec write_service_event(ServiceStatus::service_status(), 
			  Reason::reason(), ServiceType::service_type(), 
			  ServiceInstance::service_inst(), 
			  AddInfo::add_info()) -> w_ret_type().

write_service_event(ServiceStatus, Reason, ServiceType, 
		    ServiceInstance, AddInfo) ->
    write_service_event(os:timestamp(), ServiceStatus, Reason, ServiceType, 
			ServiceInstance, AddInfo).


%%% ----------------------------------------------------------
%%% @doc Write Other Event to the Availability Log. 
%%% @end
%%% ----------------------------------------------------------
-spec write_other_event(TimeStamp::now_ts(),
			ServiceStatus::service_status(), 
			Reason::reason(),
			AvailabilityInfo::avail_info(),
			ExecType::sync | async) -> 
    w_ret_type().

write_other_event(TimeStamp, ServiceStatus, Reason,
		  AvailabilityInfo, ExecType) ->
    try
	check_timestamp(TimeStamp),
	check_service_status(ServiceStatus),
	check_reason(Reason),
	check_avail_info(AvailabilityInfo),
	case ExecType of
	    sync ->
		?WRITE(?OTHER_EVENT, {?TIME(TimeStamp),
				      ServiceStatus,
				      Reason, 
				      AvailabilityInfo});
	    async ->
		?WRITE_ASYNC(?OTHER_EVENT, {?TIME(TimeStamp),
					    ServiceStatus,
					    Reason, 
					    AvailabilityInfo})
	end
    catch
	throw : ?CHECK_FAIL(FailReason) ->
	    ?LOG_ERR([?AVLI_HEADING,
		      {'Verification of indata failed',
		       'No log entry written.'}] ++
		     ['-- Error --' | FailReason] ++
		     ['-- Indata --',
		      {timeStamp, TimeStamp},
		      {serviceStatus, ServiceStatus},
		      {reason, Reason},
		      {availabilityInfo, AvailabilityInfo}]),
	    {error, illegal_param};
	ErrClass : ErrReason ->
	    Stacktrace = ?STACKTRACE_E,
	    ?LOG_ERR([?AVLI_HEADING,
		      {ErrClass, ErrReason},
		      {stacktrace, Stacktrace},
		      '-- Indata --',
		      {timeStamp, TimeStamp},
		      {serviceStatus, ServiceStatus},
		      {reason, Reason},
		      {availabilityInfo, AvailabilityInfo}]),
	    {error, illegal_param}
    end.

%%% @equiv write_other_event(TimeStamp, ServiceStatus, Reason, 
%%%                          AvailabilityInfo, sync)
-spec write_other_event(TimeStamp::now_ts(), ServiceStatus::service_status(), 
			Reason::reason(), AvailabilityInfo::avail_info()) -> 
			       w_ret_type().

write_other_event(TimeStamp, ServiceStatus, Reason, AvailabilityInfo) ->
   write_other_event(TimeStamp, ServiceStatus, Reason, AvailabilityInfo, sync).

%%% @equiv write_other_event(os:timestamp(), ServiceStatus, Reason, 
%%%                          AvailabilityInfo)
-spec write_other_event(ServiceStatus::service_status(), Reason::reason(), 
			AvailabilityInfo::avail_info()) -> w_ret_type().

write_other_event(ServiceStatus, Reason, AvailabilityInfo) ->
   write_other_event(os:timestamp(), ServiceStatus, Reason, AvailabilityInfo).


%%% ----------------------------------------------------------
%%% @doc Write PGM Event to the Availability Log. 
%%% @end
%%% ----------------------------------------------------------
-spec write_pgm_event(TimeStamp::now_ts(),
		      ServiceStatus::service_status(), 
		      Reason::reason(),
		      PiuType::piu_type(), 
		      PiuHwAddr::piu_hw_addr(),
		      SwPid::sw_pid(), 
		      AddInfo::add_info(),
		      ExecType::sync | async) -> w_ret_type().

write_pgm_event(TimeStamp, ServiceStatus, Reason, PiuType, PiuHwAddr, SwPid, 
		AddInfo, ExecType) ->
    try
	check_timestamp(TimeStamp),
	check_service_status(ServiceStatus),
	check_reason(Reason),
	check_piu_type(PiuType),
	check_piu_hw_addr(PiuHwAddr),
	check_sw_pid(SwPid),
	check_add_info(AddInfo),
	case ExecType of
	    sync ->
		?WRITE(?PGM_EVENT, {?TIME(TimeStamp),
				    ServiceStatus,
				    Reason,
				    PiuType, 
				    PiuHwAddr,
				    SwPid,
				    AddInfo});
	    async ->
		?WRITE_ASYNC(?PGM_EVENT, {?TIME(TimeStamp),
					  ServiceStatus,
					  Reason,
					  PiuType, 
					  PiuHwAddr,
					  SwPid,
					  AddInfo})
	end
    catch
	throw : ?CHECK_FAIL(FailReason) ->
	    ?LOG_ERR([?AVLI_HEADING,
		      {'Verification of indata failed',
		       'No log entry written.'}] ++
		     ['-- Error --' | FailReason] ++
		     ['-- Indata --',
		      {timeStamp, TimeStamp},
		      {serviceStatus, ServiceStatus},
		      {reason, Reason},
		      {piuType, PiuType},
		      {piuHwAddr, PiuHwAddr},
		      {swPid, SwPid},
		      {addInfo, AddInfo}]),
	    {error, illegal_param};
	ErrClass : ErrReason ->
	    Stacktrace = ?STACKTRACE_E,
	    ?LOG_ERR([?AVLI_HEADING,
		      {ErrClass, ErrReason},
		      {stacktrace, Stacktrace},
		      '-- Indata --',
		      {timeStamp, TimeStamp},
		      {serviceStatus, ServiceStatus},
		      {reason, Reason},
		      {piuType, PiuType},
		      {piuHwAddr, PiuHwAddr},
		      {swPid, SwPid},
		      {addInfo, AddInfo}]),
	    {error, illegal_param}
    end.

%%% @equiv write_pgm_event(TimeStamp, ServiceStatus, Reason, PiuType, 
%%%                        PiuHwAddr, SwPid, AddInfo, sync)
-spec write_pgm_event(TimeStamp::now_ts(), ServiceStatus::service_status(), 
		      Reason::reason(), PiuType::piu_type(), 
		      PiuHwAddr::piu_hw_addr(), SwPid::sw_pid(), 
		      AddInfo::add_info()) -> w_ret_type().

write_pgm_event(TimeStamp, ServiceStatus, Reason, PiuType, PiuHwAddr, SwPid, 
		AddInfo) ->
    write_pgm_event(TimeStamp, ServiceStatus, Reason, PiuType, PiuHwAddr, 
		    SwPid, AddInfo, sync).

%%% @equiv write_pgm_event(os:timestamp(), ServiceStatus, Reason, PiuType, 
%%%                        PiuHwAddr, SwPid, AddInfo)
-spec write_pgm_event(ServiceStatus::service_status(), 
		      Reason::reason(), PiuType::piu_type(), 
		      PiuHwAddr::piu_hw_addr(), SwPid::sw_pid(), 
		      AddInfo::add_info()) -> w_ret_type().

write_pgm_event(ServiceStatus, Reason, PiuType, PiuHwAddr, SwPid, 
		AddInfo) ->
    write_pgm_event(os:timestamp(), ServiceStatus, Reason, PiuType, PiuHwAddr, 
		    SwPid, AddInfo).

%%% ----------------------------------------------------------
%%% @doc Message from VNFM (LCM). Attributes from stop_traffic via LCM/VNFM.
%%% @end
%%% ----------------------------------------------------------
-spec start_traffic(StopTime :: string()) ->
    ok.

start_traffic(StopTime) ->
    alh_service:start_traffic(StopTime).

%%% ----------------------------------------------------------
%%% @doc Message from VNFM (LCM).
%%% @end
%%% ----------------------------------------------------------
-spec stop_traffic(TimeOfAction :: string(),   % Gregorian seconds
		   Cause        :: string(),
		   StopTime     :: term()) ->
    list(ReturnValue :: {Tag :: term(), Value :: term()}).

stop_traffic(TimeOfAction, Cause, StopTime) ->
    alh_service:stop_traffic(TimeOfAction, Cause, StopTime).

%%% ----------------------------------------------------------
%%% @doc Message from VNFM (LCM).
%%% @end
%%% ----------------------------------------------------------
-spec upgrade_activated() ->
    ok.

upgrade_activated() ->
    alh_service:upgrade_activated().

%%% ----------------------------------------------------------
%%% @doc Message from VNFM (LCM).
%%% @end
%%% ----------------------------------------------------------
-spec upgrade_confirmed() ->
    ok.

upgrade_confirmed() ->
    alh_service:upgrade_confirmed().

%%% ----------------------------------------------------------
%%% @doc Returns the default file name used for AVLI log file.
%%% @end
%%% ----------------------------------------------------------
-spec default_log_file() -> string().

default_log_file() ->
    ?LOG_FILE.


%%% ----------------------------------------------------------
%%% @doc Dump the mnesia table to a file
%%% 
%%% This function is requested from LOG when upgrading vRCS.
%%% The AVLI log from the old VM is stored in a file to later 
%%% be sent to the new VM where the entries are written to
%%% the AVLI LOG, refer to push_from_file/1
%%% 
%%% @end
%%% ----------------------------------------------------------
-spec dump_to_file(File::string()) -> ok | {error, term()}.

dump_to_file(File) ->
    try
	alh_service:dump_to_file(File)
    catch
	EC : ER ->
	    Stacktrace = ?STACKTRACE_E,
	    ?LOG_ERR([{EC, ER}, "--- Stacktrace ---" | Stacktrace])
    end.

%%% ----------------------------------------------------------
%%% @doc Push AVLI LOG from file to the mnesia table
%%% 
%%% This function is requested from LOG when upgrading vRCS.
%%% File contains the AVLI LOG entries from the OLD VM.
%%% The last entries from the old VM are stored in the new VM's
%%% AVLI LOG.
%%% It is assumed that there are no entries written to the AVLI
%%% LOG in the NEW VM before this function is executed.
%%% 
%%% @end
%%% ----------------------------------------------------------
-spec push_from_file(File::string()) -> ok | {error, term()}.

push_from_file(File) ->
    try
	alh_service:push_from_file(File)
    catch
	EC : ER ->
	    Stacktrace = ?STACKTRACE_E,
	    ?LOG_ERR([{EC, ER}, "--- Stacktrace ---" | Stacktrace])
    end.

    
%%% ----------------------------------------------------------
%%% @doc Returns the default directory used for AVLI log file.
%%% @end
%%% ----------------------------------------------------------
-spec default_log_dir() -> string().

default_log_dir() ->
    ?LOG_DIR.

%%% @hidden
%%% ----------------------------------------------------------
%%% @doc ESI callback function for exporting log to file.
%%% @end
%%% ----------------------------------------------------------
-spec generate_esi() -> ok.

generate_esi() ->
    export_log().

%%% @hidden
%%% ----------------------------------------------------------
%%% @doc Callback function for exporting log to file and return 
%%%      the file name.
%%% @end
%%% ----------------------------------------------------------
-spec generate_log() -> {ok, {FileDir::string(), FileName::string()}}.

generate_log() ->
    LogFile = default_log_file(),
    LogDir = default_log_dir(),
    export_log(LogDir, LogFile, [{zip, true}]),
    {ok, {LogDir, LogFile ++ ".gz"}}.

%%% ----------------------------------------------------------
%%% @doc Print Availability Log Events to the Erlang Log in short form. 
%%% @end
%%% ----------------------------------------------------------
-spec print_from(LogNo::integer(), Max::integer()) -> ok.
print_from(LogNo, Max) ->
    alh_service:print_from(LogNo, Max),
    ok.

%%% @equiv print_from(LogNo, default_max_printouts())
-spec print_from(LogNo::integer()) -> ok.
print_from(LogNo) ->
    print_from(LogNo, ?DEFAULT_MAX_PRINTOUTS).

%%% ----------------------------------------------------------
%%% @doc Print client information for Availability Log Events to the Erlang Log in short form. 
%%% @end
%%% ----------------------------------------------------------
-spec print_clientInfo_from(LogNo::integer(), Max::integer()) -> ok.
print_clientInfo_from(LogNo, Max) ->
    alh_service:print_clientInfo_from(LogNo, Max),
    ok.

%%% @equiv print_clientInfo_from(LogNo, default_max_printouts())
-spec print_clientInfo_from(LogNo::integer()) -> ok.
print_clientInfo_from(LogNo) ->
    print_clientInfo_from(LogNo, ?DEFAULT_MAX_PRINTOUTS).

%%% ----------------------------------------------------------
%%% @doc Print latest Availability Log Events to the Erlang Log in short form. 
%%% @end
%%% ----------------------------------------------------------
-spec print_latest(Max::integer()) -> ok.
print_latest(Max) ->
    alh_service:print_latest(Max),
    ok.

%%% @equiv print_latest(default_max_printouts())
-spec print_latest() -> ok.
print_latest() ->
    print_latest(?DEFAULT_MAX_PRINTOUTS).

%%% ----------------------------------------------------------
%%% @doc Print oldest Availability Log Events to the Erlang Log in short form. 
%%% @end
%%% ----------------------------------------------------------
-spec print_oldest(Max::integer()) -> ok.
print_oldest(Max) ->
    alh_service:print_oldest(Max),
    ok.

%%% @equiv print_oldest(default_max_printouts())
-spec print_oldest() -> ok.
print_oldest() ->
    print_oldest(?DEFAULT_MAX_PRINTOUTS).

%%% ----------------------------------------------------------
%%% @doc Export AVLI log to file.
%%% @end
%%% ----------------------------------------------------------
-spec export_log(LogDir::string(), FileName::string(), 
		 Opts::export_log_opts()) -> ok.

export_log(LogDir, FileName, Opts) ->
    alh_service:export_log(LogDir, FileName, Opts).


%%% @equiv export_log(LogDir, default_log_file(), Opts)
-spec export_log(LogDir::string(), Opts::export_log_opts()) -> ok.

export_log(LogDir, Opts) ->
    export_log(LogDir, ?LOG_FILE, Opts).


%%% @equiv export_log(LogDir, [])
-spec export_log(LogDir::string()) -> ok.

export_log(LogDir) ->
    export_log(LogDir, []).


%%% @equiv export_log(default_log_dir())
-spec export_log() -> ok.

export_log() ->
    export_log(?LOG_DIR).


%%% ----------------------------------------------------------
%%% @doc Get AVLI log as an XML string.
%%% @end
%%% ----------------------------------------------------------
-spec get_log(Opts::get_log_opts()) -> {ok, string()}.

get_log(Opts) ->
    alh_service:get_log(Opts).


%%% @equiv get_log([])
-spec get_log() -> {ok, string()}.

get_log() ->
    get_log([]).


%%% ----------------------------------------------------------
%%% @doc Delete existing AVLI log in DB and create a new log.
%%% @end
%%% ----------------------------------------------------------
-spec reset_log() -> ok.

reset_log() ->
    alh_service:reset_log().

%%% ----------------------------------------------------------
%%% @doc Information from SWM that the upgrade procedure is activated.
%%% @end
%%% ----------------------------------------------------------
-spec swm_upgrWindow_active() -> ok.

swm_upgrWindow_active() ->
    swmI:write_upgrWindow_table(?MNESIA_TAB),
    ok.

%%% ----------------------------------------------------------
%%% @doc Information from APPM about the WarmRestart procedure.
%%% @end
%%% ----------------------------------------------------------
-spec warm_restart(RestartPhase :: nodeDown | starting | operational) ->
    ok.

warm_restart(RestartPhase) ->
    alh_service:warm_restart({RestartPhase, os:timestamp()}),
    ok.

%%% #---------------------------------------------------------
%%% #3.2   CODE FOR EXPORTED INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
examples() ->
    write_node_event(os:timestamp(),
		     ?ALH_TAG_InService,
		     ?ALH_TAG_Starting,
		     88,   % EventId
		     ?ALH_TAG_Rcs(?ALH_TAG_Cause("Cause example"))),
    write_node_event(os:timestamp(),
		     ?ALH_TAG_InService,
		     ?ALH_TAG_Operational,
		     88,   % EventId
		     ?ALH_TAG_Rcs([?ALH_TAG_RankColdWTest,
				   ?ALH_TAG_Cause("Winter")])),
    write_node_event(os:timestamp(),
		     ?ALH_TAG_OutOfService,
		     ?ALH_TAG_ShutdownCommand,
		     88,   % EventId
		     ?ALH_TAG_Rcs(?ALH_TAG_RankCold)),
    write_node_event(os:timestamp(),
		     ?ALH_TAG_PartiallyOutOfService,
		     ?ALH_TAG_Unoperational,
		     88,   % EventId
		     ?ALH_TAG_Rcs(?ALH_TAG_RcsNodeDown(?ALH_TAG_DownTime(os:timestamp())))),
    write_node_event(os:timestamp(),
		     ?ALH_TAG_OutOfService,
		     ?ALH_TAG_ShutdownCommand,
		     88,   % EventId
		     ?ALH_TAG_Rcs(?ALH_TAG_RcsNodeDown(?ALH_TAG_DownTime_Unknown))),
    write_other_event(os:timestamp(),
		      ?ALH_TAG_PartiallyOutOfService,
		      ?ALH_TAG_Unoperational,
		      ?ALH_TAG_RcsNodeIdentityInfo("NodeIdReason example",
						   "Ericsson",
						   "Kista",
						   "CXP 99",
						   "R1A01",
						   "RBS",
						   "UpgradePackage example")),
    generate_log().

%%% #---------------------------------------------------------
%%% #3.3   CODE FOR INTERNAL FUNCTIONS
%%% #---------------------------------------------------------
check_timestamp({MegSec, Sec, MicSec}) 
  when is_integer(MegSec),
       is_integer(Sec),
       is_integer(MicSec) ->
    ok;
check_timestamp(Sec) when is_integer(Sec) ->
    ok;
check_timestamp(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {timestamp, Val},
		       wrong_format]).


check_service_status(Status) 
  when Status =:= ?ALH_TAG_InService;
       Status =:= ?ALH_TAG_OutOfService;
       Status =:= ?ALH_TAG_PartiallyOutOfService;
       Status =:= undefined ->
    ok;
check_service_status(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {service_status, Val},
		       unrecognized_tag]).


check_reason(Reason) 
  when Reason =:= ?ALH_TAG_ShutdownCommand;
       Reason =:= ?ALH_TAG_Unoperational;
       Reason =:= ?ALH_TAG_Starting;
       Reason =:= ?ALH_TAG_Operational;
       Reason =:= undefined ->
    ok;
check_reason(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {reason, Val},
		       unrecognized_tag]).


check_event_id(EvtId) 
  when is_integer(EvtId), EvtId >= 0 ->
    ok;
check_event_id(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {event_id, Val},
		       not_a_positive_integer_nor_zero]).


check_add_info(Info)
  when is_tuple(Info) ->
    ok;
check_add_info(Info) 
  when length(Info) =< ?MAX_ADD_INFO_LEN ->
    ok;
check_add_info(Val) when is_list(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {add_info, Val},
		       {string_too_long, {max, ?MAX_ADD_INFO_LEN}}]);
check_add_info(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {add_info, Val},
		       wrong_format]).


check_piu_type(Type) when Type =:= 'Bp';
			  Type =:= 'Mp';
			  Type =:= undefined ->
    ok;
check_piu_type(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {piu_type, Val},
		       unrecognized_tag]).


check_piu_hw_addr({Smn, Spn}) 
  when is_integer(Smn), 
       is_integer(Spn) ->
    ok;
check_piu_hw_addr(undefined) ->
    ok;
check_piu_hw_addr(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {piu_hw_addr, Val},
		       wrong_format]).


check_hw_pid({ProdNo, ProdRev, ProdName}) ->
    check_prod_no(ProdNo),
    check_prod_rev(ProdRev),
    check_prod_name(ProdName);
check_hw_pid(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {hw_pid, Val},
		       wrong_format]).


check_prod_no(ProdNo) 
  when length(ProdNo) =< ?MAX_PROD_NUM_LEN ->
    ok;
check_prod_no(Val) when is_list(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {prod_no, Val},
		       {string_too_long, {max, ?MAX_PROD_NUM_LEN}}]);
check_prod_no(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {prod_no, Val},
		       wrong_format]).


check_prod_rev(ProdRev) 
  when length(ProdRev) =< ?MAX_PROD_REV_LEN ->
    ok;
check_prod_rev(Val) when is_list(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {prod_rev, Val},
		       {string_too_long, {max, ?MAX_PROD_REV_LEN}}]);
check_prod_rev(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {prod_rev, Val},
		       wrong_format]).


check_prod_name(ProdName)
  when length(ProdName) =< ?MAX_PROD_NAME_LEN ->
    ok;
check_prod_name(Val) when is_list(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {prod_name, Val},
		       {string_too_long, {max, ?MAX_PROD_NAME_LEN}}]);
check_prod_name(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {prod_name, Val},
		       wrong_format]).


check_sw_pid({ProdNo, ProdRev}) ->
    check_prod_no(ProdNo),
    check_prod_rev(ProdRev);
check_sw_pid(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {sw_pid, Val},
		       wrong_format]).


check_hw_type(HwType) 
  when length(HwType) =< ?MAX_HW_TYPE_LEN ->
    ok;
check_hw_type(Val) when is_list(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {hw_type, Val},
		       {string_too_long, {max, ?MAX_HW_TYPE_LEN}}]);
check_hw_type(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {hw_type, Val},
		       wrong_format]).

    
check_hw_address(HwAddress) 
  when length(HwAddress) =< ?MAX_HW_ADDR_LEN ->
    ok;
check_hw_address(Val) when is_list(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {hw_address, Val},
		       {string_too_long, {max, ?MAX_HW_ADDR_LEN}}]);
check_hw_address(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {hw_address, Val},
		       wrong_format]).

    
check_service_type(ServiceType) 
  when length(ServiceType) =< ?MAX_SERV_TYPE_LEN ->
    ok;
check_service_type(Val) when is_list(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {service_type, Val},
		       {string_too_long, {max, ?MAX_SERV_TYPE_LEN}}]);
check_service_type(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {service_type, Val},
		       wrong_format]).

    
check_service_inst(ServiceInst) 
  when length(ServiceInst) =< ?MAX_SERV_INST_LEN ->
    ok;
check_service_inst(Val) when is_list(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {service_inst, Val},
		       {string_too_long, {max, ?MAX_SERV_INST_LEN}}]);
check_service_inst(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {service_inst, Val},
		       wrong_format]).


check_avail_info(Info) 
  when is_tuple(Info) ->
    ok;
check_avail_info(Info) 
  when length(Info) =< ?MAX_AVAIL_INFO_LEN ->
    ok;
check_avail_info(Val) when is_list(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {avail_info, Val},
		       {string_too_long, {max, ?MAX_AVAIL_INFO_LEN}}]);
check_avail_info(Val) ->
    ?THROW_CHECK_FAIL([{error, illegal_param},
		       {avail_info, Val},
		       wrong_format]).

%%% #---------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%% #---------------------------------------------------------
