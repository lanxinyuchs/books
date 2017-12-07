%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	alhI.hrl %
%%% @author etxberb
%%% @copyright Ericsson AB 2014
%%% @version /main/R2A/15
%%%
%%% @doc == ALH Service ==
%%% This file publishes the user API of the RBS CS Availability Log Service.
%%% @end
%%% ----------------------------------------------------------

-hrl_vsn('/main/R2A/15').
-hrl_date('2014-05-05').
-hrl_author('etxberb').

%%% %CCaseTemplateFile: module.hrl %
%%% %CCaseTemplateId: 54/002 01-LXA 119 334 Ux, Rev: /main/6 %
%%%
%%% %CCaseCopyrightBegin%
%%% Copyright (c) Ericsson AB 2014 All rights reserved.
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
%%% -----      ---------- --------    ------------------------
%%% R2A/1      2014-01-24 etxberb     Created
%%% ----------------------------------------------------------
%%%
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------

%%% Content of 'AvailabilityInfo' & 'AdditionalInfo':
-define(ALH_TAG_AppLog(Value), {'AppLog', Value}).
-define(ALH_TAG_Rcs(Value),    {'Rcs', Value}).
-define(ALH_TAG_RcsNodeIdentityInfo(NodeIdReason,
				    SiteLocation,
				    NetworkManagedElementId,
				    ProdNo,
				    ProdRev,
				    ProdName,
				    UpgradePackage),
	{'RcsNodeIdentityInfo', [{'NodeIdReason', NodeIdReason},
				 ?ALH_TAG_NodeIdentity(SiteLocation,
						       NetworkManagedElementId,
						       ProdNo,
						       ProdRev,
						       ProdName),
				 {'UpgradePackage', UpgradePackage}]}).

-define(ALH_TAG_Cause(Value),        {'Cause', Value}).
-define(ALH_TAG_InfoText(Value),     {'InfoText', Value}).
-define(ALH_TAG_RankCold,            'RankCold').
-define(ALH_TAG_RankWarm,            'RankWarm').
-define(ALH_TAG_RankColdWTest,       'RankColdWTest').
-define(ALH_TAG_RestartCompleted,    'RestartCompleted').
-define(ALH_TAG_PiuInfo(Value),      {'PiuInfo', Value}).
-define(ALH_TAG_NodeInfo(Value),     {'NodeInfo', Value}).
-define(ALH_TAG_RcsNodeDown(Value),  {'RcsNodeDown', Value}).
-define(ALH_TAG_DownTime(Timestamp), {timestamp_DownTime, Timestamp}).
-define(ALH_TAG_DownTime_Unknown,    {'DownTime', 'Unknown'}).
-define(ALH_TAG_NodeIdentity(SiteLocation,
			     NetworkManagedElementId,
			     ProdNo,
			     ProdRev,
			     ProdName),
	{'NodeIdentity', [{'SiteLocation', SiteLocation},
			  {'NetworkManagedElementId', NetworkManagedElementId},
			  {'ProdNo', ProdNo},
			  {'ProdRev', ProdRev},
			  {'ProdName', ProdName}]}).

%%% Cause:
-define(ALH_TAG_DataRestore,                 "DataRestore").
-define(ALH_TAG_ExtRestartRequest,           "ExtRestartRequest").
-define(ALH_TAG_ExtUpgradeRequest,           "ExtUpgradeRequest").
-define(ALH_TAG_ManualRestart,               "ManualRestart").
-define(ALH_TAG_ProgramErrEscalated,         "ProgramErrEscalated").
-define(ALH_TAG_ProgramErrEscalatedInstallation,
	"ProgramErrEscalatedInstallation").
-define(ALH_TAG_ProgramErrEscalatedRestore1, "ProgramErrEscalatedRestore1").
-define(ALH_TAG_ProgramErrEscalatedRestore2, "ProgramErrEscalatedRestore2").
-define(ALH_TAG_ProgramErrEscalatedRestore3, "ProgramErrEscalatedRestore3").
-define(ALH_TAG_ProgramFailure,              "ProgramFailure").
-define(ALH_TAG_RestartRequest,              "RestartRequest").
-define(ALH_TAG_SoftwareRestore,             "SoftwareRestore").
-define(ALH_TAG_UpgradeCancelled,            "UpgradeCancelled").
-define(ALH_TAG_UpgradeFailure,              "UpgradeFailure").
-define(ALH_TAG_UpgradeNormal,               "UpgradeNormal").
-define(ALH_TAG_UpgradePiuError,             "UpgradePiuError").
-define(ALH_TAG_UpgradeProgramError,         "UpgradeProgramError").
-define(ALH_TAG_UpgradeTimeout,              "UpgradeTimeout").

%%% service_status:
-define(ALH_TAG_InService,             'InService').
-define(ALH_TAG_OutOfService,          'OutOfService').
-define(ALH_TAG_PartiallyOutOfService, 'PartiallyOutOfService').

%%% reason:
-define(ALH_TAG_ShutdownCommand, 'ShutdownCommand').
-define(ALH_TAG_Unoperational,   'UnOperational').
-define(ALH_TAG_Starting,        'Starting').
-define(ALH_TAG_Operational,     'Operational').

%%% nodeIdReason:
-define(ALH_TAG_NodeIdChanged,   "NodeIdChanged").
-define(ALH_TAG_NodeRestarted,   "NodeRestarted").
-define(ALH_TAG_PeriodicLogging, "PeriodicLogging").

%%% LogInfoRecord:
-define(ALH_TAG_RcsNodeRestart, 'RcsNodeRestart').
-define(ALH_TAG_RcsTimeChange(OldTime, NewTime),
	{'RcsTimeChange', [{'OldTime', OldTime},
			   {'NewTime', NewTime}]}).
