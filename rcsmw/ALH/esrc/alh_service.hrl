%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	alh_service.hrl %
%%% Author:	etxbjca
%%% Description:     
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R2A/R4A/R5A/R6A/R11A/3').
-hrl_date('2017-10-18').
-hrl_author('etxberb').
%%% %CCaseTemplateFile:	module.hrl %
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
%%% R2A/1      2012-12-17 eolaand     Created
%%% R2A/2      2014-01-24 etxberb     Introduced AVLI version 4.
%%% R4A/1      2015-11-24 etxberb     Increased from 60 to 100:
%%%                                   CELLO_AVLI_MAX_SERVICE_INSTANCE_LEN
%%% R5A/1      2015-12-11 ekurnik     Moved event defines from alh_service.erl
%%%                                   Added port names and trace definitions
%%% R5A/2      2015-12-29 etomist     Removed event defines from alh_service.erl
%%% R6A/1      2016-07-11 etxarnu     MR2230:Increased 
%%%                                     CELLO_MAX_PRODUCT_NAME_LEN to 33
%%% R6A/2      2016-08-23 etxarnu     MR2230: Updated to use Cello_Avli5_writeHwEvent
%%% R11A/1     2017-10-01 etxberb     SP531: Upgrade handling for 5G.

%%% ----------------------------------------------------------
%%% 
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           CONSTANT_NAME
%%% Description: 
%%% ----------------------------------------------------------
%% Mnesia table
-define(MNESIA_TAB, alhLog).

%% Events
-define(NODE_EVENT, 'CelloAvli4_writeNodeEvent').
-define(PIU_EVENT, 'CelloAvli2_writePiuEvent').
-define(HW_EVENT, 'Cello_Avli2_writeHwEvent').
-define(HW5_EVENT, 'Cello_Avli5_writeHwEvent').
-define(SERVICE_EVENT, 'Cello_Avli2_writeServiceEvent').
-define(OTHER_EVENT, 'Cello_Avli2_writeOtherEvent').
-define(PGM_EVENT, 'CelloAvli3_writePgmEvent').

%% Cello AVLI max lengths
-define(CELLO_AVLI_MAX_ADD_INFO_LEN, 1500).
-define(CELLO_AVLI_MAX_HW_TYPE_LEN, 60).
-define(CELLO_AVLI_MAX_HW_ADDRESS_LEN, 60).
-define(CELLO_AVLI_MAX_SERVICE_TYPE_LEN, 60).
-define(CELLO_AVLI_MAX_SERVICE_INSTANCE_LEN, 100).
-define(CELLO_AVLI_MAX_AVAILABILITY_INFO_LEN, 15000).
-define(CELLO_MAX_PRODUCT_NUMBER_LEN, 25).
-define(CELLO_MAX_PRODUCT_REVISION_LEN, 8).
-define(CELLO_MAX_PRODUCT_NAME_LEN, 13).
-define(CELLO_AVLI_MAX_PRODUCT_NAME_LEN, 33).
-define(CELLO_MAX_PRODUCT_DATE_LEN, 9).
-define(CELLO_MAX_SERIAL_NUMBER_LEN, 14).

%% Short hand max lengths
-define(MAX_ADD_INFO_LEN, ?CELLO_AVLI_MAX_ADD_INFO_LEN).
-define(MAX_HW_TYPE_LEN, ?CELLO_AVLI_MAX_HW_TYPE_LEN).
-define(MAX_HW_ADDR_LEN, ?CELLO_AVLI_MAX_HW_ADDRESS_LEN).
-define(MAX_SERV_TYPE_LEN, ?CELLO_AVLI_MAX_SERVICE_TYPE_LEN).
-define(MAX_SERV_INST_LEN, ?CELLO_AVLI_MAX_SERVICE_INSTANCE_LEN).
-define(MAX_AVAIL_INFO_LEN, ?CELLO_AVLI_MAX_AVAILABILITY_INFO_LEN).
-define(MAX_PROD_NUM_LEN, ?CELLO_MAX_PRODUCT_NUMBER_LEN).
-define(MAX_PROD_REV_LEN, ?CELLO_MAX_PRODUCT_REVISION_LEN).
-define(MAX_PROD_NAME_LEN, ?CELLO_MAX_PRODUCT_NAME_LEN).
-define(MAX_AVLI_PROD_NAME_LEN, ?CELLO_AVLI_MAX_PRODUCT_NAME_LEN).
-define(MAX_PROD_DATE_LEN, ?CELLO_MAX_PRODUCT_DATE_LEN).
-define(MAX_SERIAL_NUM_LEN, ?CELLO_MAX_SERIAL_NUMBER_LEN).

%% COLI and print functions
-define(DEFAULT_MAX_PRINTOUTS, 30).

%% Upgrade
-define(FILENAME_UpgradeInfo, alh_upgrade_info).
-define(FILENAME_EventQRollback, alh_eventQ_rollback).
-define(FILE_EventQRollback,
	filename:join(alh_service:vLog_dir(), ?FILENAME_EventQRollback)).
-define(FILE_EventQRollback_backup,
	filename:join([alh_service:vLog_dir(),
		       "backup",
		       ?FILENAME_EventQRollback])).
-define(FILE_UpgradeInfo,
	filename:join(alh_service:alh_home_dir(), ?FILENAME_UpgradeInfo)).
-define(UgState_activated,       upgrade_activated).
-define(UgState_rollbackStb_started, upgrade_rollbackStandby_started).
-define(UgState_rollbackStb_traffic, upgrade_rollbackStandby_traffic).

-define(FilePushedFromOldNode,
	filename:join(alh_service:alh_dir(), "FilePushedFromOldNode")).

%% error_logger
-define(AVLI_HEADING, {'Reporter', 'AVailability Log Interface (AVLI)'}).

%% General
-define(ELSE, true).
-define(FUNCTION, 
	element(2, element(2, process_info(self(), current_function)))).
-define(time2string(NativeTime),
	sysUtil:time_to_string(NativeTime, nano_seconds)).

%% debug
-define(ALH_TRACE(Format, Arg),
	io:format("[~p]: " ++ Format, [?MODULE | Arg])).

-define(MonoTime, erlang:monotonic_time()).
-define(PROC_INFO(Pid), sysUtil:pid_info(Pid, {all, [error_handler]})).
-define(STATE_INFO(Record),
	sysUtil:record_format(record_info(fields, state), Record)).
-define(STACKTRACE_C,   % Current stacktrace
	element(2, process_info(self(), current_stacktrace))).
-define(STACKTRACE_E,   % Stacktrace at Exception
	erlang:get_stacktrace()).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Usage of error_logger:XXX_report
-define(LOG_ERR(__ReportInfo),
	try
	    sysInitI:error_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:error_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_INFO(__ReportInfo),
	try
	    sysInitI:info_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:info_report(?RepInfo(__ReportInfo))
	end).
-define(LOG_WARN(__ReportInfo),
	try
	    sysInitI:warning_report(?RepInfo(__ReportInfo))
	catch
	    _ : _ ->
		%% During startup, before sysInitI has started!
		error_logger:warning_report(?RepInfo(__ReportInfo))
	end).
-define(RepInfo(__RepInfo),
	[{?MODULE, ?FUNCTION} | __RepInfo]).

%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------
%%% ----------------------------------------------------------
%%% #           ?MNESIA_TAB
%%% Description: 
%%% ----------------------------------------------------------
-record(?MNESIA_TAB, {logNo,
		      clientInfo = [],
		      logRec
		     }
       ).

-record('LogRecord', {attr = [],
		      val = []}
       ).
