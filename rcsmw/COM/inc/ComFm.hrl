%%% --------------------------------------------------------
%%% Copyright (c) Ericsson AB 2017 All rights reserved.
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
%%% --------------------------------------------------------

-hrl_id({"ComFm","12.1.0","/main/R2A/R3A/R5A/R6A/R9A/R11A/1"}).


%% -------------- CLASS Fm -------------------------

%% Description:
%% The root MOC of the Fault Management branch.

-record(fm, {fmId,
             lastSequenceNo,
             sumCritical,
             sumMajor,
             sumMinor,
             sumWarning,
             totalActive,
             lastChanged,
             heartbeatInterval}).

-define(fm_types,
        [{fmId, string},
         {lastSequenceNo, uint64},
         {sumCritical, uint32},
         {sumMajor, uint32},
         {sumMinor, uint32},
         {sumWarning, uint32},
         {totalActive, uint32},
         {lastChanged, 'ComFm.DateTime'},
         {heartbeatInterval, uint32}]).

-define(fm_heartbeatInterval_default, 60).
-define(Fm_restricted, [fmId]).


%% -------------- CLASS FmAlarmModel -------------------------

%% Description:
%% This is a container for grouping FM alarm types.

-record(fmAlarmModel, {fmAlarmModelId,
                       dummy}).

-define(fmAlarmModel_types,
        [{fmAlarmModelId, string},
         {dummy, atom}]).

-define(FmAlarmModel_restricted, [fmAlarmModelId]).


%% -------------- CLASS FmAlarmType -------------------------

%% Description:
%% A specific kind of alarm that can be reported, for example "power failure".
%% In an X.733 context it maps to event type, probable cause, and specific problem. The same principle is used for alert types as for alarm types.

-record(fmAlarmType, {fmAlarmTypeId,
                      majorType,
                      minorType,
                      moClasses,
                      specificProblem,
                      eventType,
                      probableCause,
                      isStateful,
                      additionalText,
                      configuredSeverity,
                      defaultSeverity}).

-define(fmAlarmType_types,
        [{fmAlarmTypeId, string},
         {majorType, uint32},
         {minorType, uint32},
         {moClasses, string},
         {specificProblem, string},
         {eventType, 'ComFm.EventType'},
         {probableCause, 'ComFm.ProbableCause'},
         {isStateful, boolean},
         {additionalText, string},
         {configuredSeverity, 'ComFm.SeverityLevel'},
         {defaultSeverity, 'ComFm.SeverityLevel'}]).

-define(FmAlarmType_restricted, [fmAlarmTypeId]).


%% -------------- CLASS FmAlarm -------------------------

%% Description:
%% An FmAlarm instance represents an active alarm.
%% An alarm is a persistent indication of a fault that clears only when the triggering condition has been resolved.

-record(fmAlarm, {fmAlarmId,
                  source,
                  lastEventTime,
                  sequenceNumber,
                  activeSeverity,
                  additionalText,
                  majorType,
                  minorType,
                  specificProblem,
                  eventType,
                  probableCause,
                  additionalInfo,
                  originalEventTime,
                  originalSeverity,
                  originalAdditionalText}).

-define(fmAlarm_types,
        [{fmAlarmId, string},
         {source, string},
         {lastEventTime, 'ComFm.DateTime'},
         {sequenceNumber, int64},
         {activeSeverity, 'ComFm.SeverityLevel'},
         {additionalText, string},
         {majorType, uint32},
         {minorType, uint32},
         {specificProblem, string},
         {eventType, 'ComFm.EventType'},
         {probableCause, 'ComFm.ProbableCause'},
         {additionalInfo, {sequence,{struct,'AdditionalInformation'}}},
         {originalEventTime, 'ComFm.DateTime'},
         {originalSeverity, 'ComFm.SeverityLevel'},
         {originalAdditionalText, string}]).

-define(FmAlarm_restricted, [fmAlarmId]).


%% ------------------ ENUM EventType ----------------------
-ifndef('EventType').
-define('EventType', 1).

-define(EventType_OTHER, 1).
-define(EventType_COMMUNICATIONSALARM, 2).
-define(EventType_QUALITYOFSERVICEALARM, 3).
-define(EventType_PROCESSINGERRORALARM, 4).
-define(EventType_EQUIPMENTALARM, 5).
-define(EventType_ENVIRONMENTALALARM, 6).
-define(EventType_INTEGRITYVIOLATION, 7).
-define(EventType_OPERATIONALVIOLATION, 8).
-define(EventType_PHYSICALVIOLATION, 9).
-define(EventType_SECURITYSERVICEORMECHANISMVIOLATION, 10).
-define(EventType_TIMEDOMAINVIOLATION, 11).

-endif. % EventType

%% ------------------ ENUM SeverityLevel ----------------------
-ifndef('SeverityLevel').
-define('SeverityLevel', 1).

-define(SeverityLevel_CRITICAL, 3).
-define(SeverityLevel_MAJOR, 4).
-define(SeverityLevel_MINOR, 5).
-define(SeverityLevel_WARNING, 6).

-endif. % SeverityLevel

%% ------------------ STRUCT AdditionalInformation ----------------------
-ifndef(_ADDITIONAL_INFORMATION).
-define(_ADDITIONAL_INFORMATION, 1).

-record('AdditionalInformation', {name,
                                  value}).

-define('AdditionalInformation_types',
        [{name, string},
         {value, string}]).


-endif. % _ADDITIONAL_INFORMATION

