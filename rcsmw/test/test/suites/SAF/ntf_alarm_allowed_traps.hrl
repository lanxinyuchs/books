%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	ntf_alarm_allowed_traps.hrl %
%%% Author:	erarafo
%%% Description: Traps that are generated by CS at unpredictable
%%% points in time. Suites that tests alarms may use the traps
%%% defined here.
%%%
%%% ----------------------------------------------------------
-hrl_id('Updated by CCase').
-hrl_vsn('/main/R2A/R3A/R4A/R5A/1').
-hrl_date('2016-01-26').
-hrl_author('erarafo').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: CCver: /main/2 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014-2016 All rights reserved.
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
%%% R2A/1      2014-08-25 erarafo     First version
%%% R2A/2      2014-08-29 erarafo     Allowing all SysM=1 traps
%%% R3A/1      2014-10-01 erarafo     Extended list of allowed traps
%%% R4A/1      2015-10-30 erarafo     Extended list of allowed traps again
%%% R5A/1      2016-01-26 erarafo     Added allowed trap: "Grace Period Activated"


-define(ADD_TEXT_ANYBODY_THERE, "/NOT sent by COM/ Anybody there?").

-define(ALLOWED_TRAPS,
	[[{type, eriAlarmHeartBeatNotif}],
	 
	 [{type, eriAlarmAlarmListRebuilt}],
	 
	 [{eriAlarmActiveManagedObject, "ManagedElement=1,SystemFunctions=1,SysM=1"}],
	 
	 [{type, eriAlarmCritical},
	  {eriAlarmActiveManagedObject, "ManagedElement=1,SystemFunctions=1,Lm=1"},
	  {eriAlarmActiveSpecificProblem, "License Key File Fault"}],
	 
	 [{eriAlarmActiveManagedObject, "ManagedElement=1,SystemFunctions=1,Lm=1,EmergencyUnlock=1"},
	  {eriAlarmActiveSpecificProblem, "Emergency Unlock Reset Key Required"}],
	 
	 [{eriAlarmActiveSpecificProblem, "Grace Period Activated"}]
	
	%% allow this just in case a very late trap from the
	%% do_verify_snmp_mgr testcase would unexpectedly be
	%% received.
	%% 	 [{type, eriAlarmIndAlert}, 
	%% 	  {eriAlarmNObjAdditionalText, ?ADD_TEXT_ANYBODY_THERE}]
	]
       ).