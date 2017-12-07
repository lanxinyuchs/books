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

-hrl_id({"RcsPm","1.5.0","/main/R2A/R3A/R5A/R6A/R8A/1"}).


%% -------------- CLASS Pm -------------------------

%% Description:
%% The top class of the Performance Management model.

-record(pm, {pmId,
             dummy}).

-define(pm_types,
        [{pmId, string},
         {dummy, atom}]).

-define(Pm_restricted, [pmId]).


%% -------------- CLASS PmMeasurementCapabilities -------------------------

%% Description:
%% Contains the measurement capabilities of the ME.

-record(pmMeasurementCapabilities, {pmMeasurementCapabilitiesId,
                                    maxNoOfJobs,
                                    jobStartStopSupport,
                                    finalROP,
                                    jobPrioritizationSupport,
                                    maxNoOfMeasurements,
                                    maxNoOfPmFiles,
                                    alignedReportingPeriod,
                                    measurementJobSupport,
                                    realTimeJobSupport,
                                    thresholdJobSupport,
                                    fileLocation,
                                    fileGroup,
                                    fileRPSupported,
                                    supportedRopPeriods,
                                    supportedMeasJobGps,
                                    supportedRtJobGps,
                                    supportedThreshJobGps,
                                    supportedCompressionTypes,
                                    jobGroupingSupport,
                                    producesUtcRopFiles,
                                    ropFilenameTimestamp}).

-define(pmMeasurementCapabilities_types,
        [{pmMeasurementCapabilitiesId, string},
         {maxNoOfJobs, uint16},
         {jobStartStopSupport, 'RcsPm.JobStartStopSupport'},
         {finalROP, boolean},
         {jobPrioritizationSupport, boolean},
         {maxNoOfMeasurements, uint32},
         {maxNoOfPmFiles, uint16},
         {alignedReportingPeriod, boolean},
         {measurementJobSupport, boolean},
         {realTimeJobSupport, boolean},
         {thresholdJobSupport, boolean},
         {fileLocation, string},
         {fileGroup, moRef},
         {fileRPSupported, boolean},
         {supportedRopPeriods, {sequence,'RcsPm.TimePeriod'}},
         {supportedMeasJobGps, {sequence,'RcsPm.TimePeriod'}},
         {supportedRtJobGps, {sequence,'RcsPm.TimePeriod'}},
         {supportedThreshJobGps, {sequence,'RcsPm.TimePeriod'}},
         {supportedCompressionTypes, {sequence,'RcsPm.CompressionTypes'}},
         {jobGroupingSupport, boolean},
         {producesUtcRopFiles, boolean},
         {ropFilenameTimestamp, 'RcsPm.RopFilenameTimestamp'}]).

-define(PmMeasurementCapabilities_restricted, [pmMeasurementCapabilitiesId]).


%% -------------- CLASS PmGroup -------------------------

%% Description:
%% A grouping of the measurements into logical grouping. 

-record(pmGroup, {pmGroupId,
                  category,
                  consistentData,
                  generation,
                  switchingTechnology,
                  validity,
                  moClass,
                  description,
                  pmGroupVersion}).

-define(pmGroup_types,
        [{pmGroupId, string},
         {category, string},
         {consistentData, boolean},
         {generation, string},
         {switchingTechnology, string},
         {validity, boolean},
         {moClass, {struct,'ManagedObjectClass'}},
         {description, string},
         {pmGroupVersion, string}]).

-define(PmGroup_restricted, [pmGroupId]).


%% -------------- CLASS MeasurementType -------------------------

%% Description:
%% Defines a Measurement Type on the Managed Element to be monitored and collected.

-record(measurementType, {measurementTypeId,
                          measurementName,
                          size,
                          collectionMethod,
                          description,
                          condition,
                          aggregation,
                          measurementStatus,
                          measurementResult,
                          multiplicity,
                          initialValue,
                          resetAtGranPeriod,
                          derSampleRate,
                          fmAlarmType,
                          thresholdDirection,
                          isCompressed}).

-define(measurementType_types,
        [{measurementTypeId, string},
         {measurementName, string},
         {size, uint16},
         {collectionMethod, 'RcsPm.CollectionMethod'},
         {description, string},
         {condition, string},
         {aggregation, 'RcsPm.Aggregation'},
         {measurementStatus, 'RcsPm.MeasurementStatus'},
         {measurementResult, string},
         {multiplicity, int16},
         {initialValue, int32},
         {resetAtGranPeriod, boolean},
         {derSampleRate, int16},
         {fmAlarmType, moRef},
         {thresholdDirection, 'RcsPm.ThresholdDirection'},
         {isCompressed, boolean}]).

-define(MeasurementType_restricted, [measurementTypeId]).


%% -------------- CLASS PmThresholdMonitoring -------------------------

%% Description:
%% Contains configuration of each threshold for Threshold jobs.

-record(pmThresholdMonitoring, {pmThresholdMonitoringId,
                                thresholdHigh,
                                thresholdLow,
                                thresholdSeverity}).

-define(pmThresholdMonitoring_types,
        [{pmThresholdMonitoringId, string},
         {thresholdHigh, int64},
         {thresholdLow, int64},
         {thresholdSeverity, 'RcsPm.SeverityLevel'}]).

-define(pmThresholdMonitoring_thresholdSeverity_default, 'MINOR').
-define(PmThresholdMonitoring_restricted, [pmThresholdMonitoringId]).


%% -------------- CLASS PmJob -------------------------

%% Description:
%% Describes a user-defined PM job on the Managed Element.

-record(pmJob, {pmJobId,
                requestedJobState,
                reportingPeriod,
                jobType,
                jobPriority,
                granularityPeriod,
                currentJobState,
                jobControl,
                compressionType,
                jobGroup}).

-define(pmJob_types,
        [{pmJobId, string},
         {requestedJobState, 'RcsPm.JobState'},
         {reportingPeriod, 'RcsPm.TimePeriod'},
         {jobType, 'RcsPm.JobType'},
         {jobPriority, 'RcsPm.JobPriority'},
         {granularityPeriod, 'RcsPm.TimePeriod'},
         {currentJobState, 'RcsPm.JobState'},
         {jobControl, 'RcsPm.JobControl'},
         {compressionType, 'RcsPm.CompressionTypes'},
         {jobGroup, string}]).

-define(pmJob_requestedJobState_default, 'ACTIVE').
-define(pmJob_reportingPeriod_default, 'FIFTEEN_MIN').
-define(pmJob_jobType_default, 'MEASUREMENTJOB').
-define(pmJob_jobPriority_default, 'MEDIUM').
-define(pmJob_granularityPeriod_default, 'FIFTEEN_MIN').
-define(PmJob_restricted, [pmJobId]).


%% -------------- CLASS MeasurementReader -------------------------

%% Description:
%% Represents the capability to read the value of the MeasurementType or group of MeasurementTypes.

-record(measurementReader, {measurementReaderId,
                            measurementReaderNameValue,
                            measurementSpecification,
                            moInstances,
                            thresholdRateOfVariation,
                            thresholdDirection}).

-define(measurementReader_types,
        [{measurementReaderId, string},
         {measurementReaderNameValue, {sequence,{struct,'MeasurementReaderNameValue'}}},
         {measurementSpecification, {struct,'MeasurementSpecification'}},
         {moInstances, {sequence,'RcsPm.MoFilter'}},
         {thresholdRateOfVariation, 'RcsPm.PerTimeInterval'},
         {thresholdDirection, 'RcsPm.ThresholdDirection'}]).

-define(MeasurementReader_restricted, [measurementReaderId]).


%% ------------------ ENUM TimePeriod ----------------------
-ifndef('TimePeriod').
-define('TimePeriod', 1).

-define(TimePeriod_ONE_MIN, 3).
-define(TimePeriod_FIVE_MIN, 4).
-define(TimePeriod_FIFTEEN_MIN, 5).
-define(TimePeriod_THIRTY_MIN, 6).
-define(TimePeriod_ONE_HOUR, 7).
-define(TimePeriod_TWELVE_HOUR, 8).
-define(TimePeriod_ONE_DAY, 9).
-define(TimePeriod_TEN_SECONDS, 1).
-define(TimePeriod_THIRTY_SECONDS, 2).

-endif. % TimePeriod

%% ------------------ ENUM CompressionTypes ----------------------
-ifndef('CompressionTypes').
-define('CompressionTypes', 1).

-define(CompressionTypes_GZIP, 0).

-endif. % CompressionTypes

%% ------------------ ENUM RopFilenameTimestamp ----------------------
-ifndef('RopFilenameTimestamp').
-define('RopFilenameTimestamp', 1).

-define(RopFilenameTimestamp_LOCAL_WITH_UTC_OFFSET, 0).
-define(RopFilenameTimestamp_UTC_NO_OFFSET, 1).

-endif. % RopFilenameTimestamp

%% ------------------ ENUM JobPriority ----------------------
-ifndef('JobPriority').
-define('JobPriority', 1).

-define(JobPriority_LOW, 1).
-define(JobPriority_MEDIUM, 2).
-define(JobPriority_HIGH, 3).

-endif. % JobPriority

%% ------------------ ENUM JobType ----------------------
-ifndef('JobType').
-define('JobType', 1).

-define(JobType_MEASUREMENTJOB, 1).
-define(JobType_THRESHOLDJOB, 2).
-define(JobType_REALTIMEJOB, 3).

-endif. % JobType

%% ------------------ ENUM JobState ----------------------
-ifndef('JobState').
-define('JobState', 1).

-define(JobState_ACTIVE, 1).
-define(JobState_STOPPED, 2).

-endif. % JobState

%% ------------------ ENUM PerTimeInterval ----------------------
-ifndef('PerTimeInterval').
-define('PerTimeInterval', 1).

-define(PerTimeInterval_PER_SECOND, 0).
-define(PerTimeInterval_PER_GP, 1).

-endif. % PerTimeInterval

%% ------------------ ENUM CollectionMethod ----------------------
-ifndef('CollectionMethod').
-define('CollectionMethod', 1).

-define(CollectionMethod_CC, 1).
-define(CollectionMethod_GAUGE, 2).
-define(CollectionMethod_DER, 3).
-define(CollectionMethod_SI, 4).

-endif. % CollectionMethod

%% ------------------ ENUM ThresholdDirection ----------------------
-ifndef('ThresholdDirection').
-define('ThresholdDirection', 1).

-define(ThresholdDirection_INCREASING, 1).
-define(ThresholdDirection_DECREASING, 2).

-endif. % ThresholdDirection

%% ------------------ ENUM JobControl ----------------------
-ifndef('JobControl').
-define('JobControl', 1).

-define(JobControl_FULL, 0).
-define(JobControl_STARTSTOP, 1).
-define(JobControl_VIEWONLY, 2).

-endif. % JobControl

%% ------------------ ENUM SeverityLevel ----------------------
-ifndef('SeverityLevel').
-define('SeverityLevel', 1).

-define(SeverityLevel_CRITICAL, 3).
-define(SeverityLevel_MAJOR, 4).
-define(SeverityLevel_MINOR, 5).
-define(SeverityLevel_WARNING, 6).

-endif. % SeverityLevel

%% ------------------ ENUM JobStartStopSupport ----------------------
-ifndef('JobStartStopSupport').
-define('JobStartStopSupport', 1).

-define(JobStartStopSupport_NONE, 0).
-define(JobStartStopSupport_BASIC, 1).

-endif. % JobStartStopSupport

%% ------------------ ENUM MeasurementStatus ----------------------
-ifndef('MeasurementStatus').
-define('MeasurementStatus', 1).

-define(MeasurementStatus_USED, 1).
-define(MeasurementStatus_DEPRECATED, 2).
-define(MeasurementStatus_OBSOLETE, 3).
-define(MeasurementStatus_PRELIMINARY, 4).

-endif. % MeasurementStatus

%% ------------------ ENUM Aggregation ----------------------
-ifndef('Aggregation').
-define('Aggregation', 1).

-define(Aggregation_SUM, 2).
-define(Aggregation_AVG, 3).
-define(Aggregation_MIN, 4).
-define(Aggregation_MAX, 5).
-define(Aggregation_LAST_UPDATE, 6).

-endif. % Aggregation

%% ------------------ STRUCT MeasurementReaderNameValue ----------------------
-ifndef(_MEASUREMENT_READER_NAME_VALUE).
-define(_MEASUREMENT_READER_NAME_VALUE, 1).

-record('MeasurementReaderNameValue', {currentValue,
                                       lastUpdated,
                                       moClassInstance,
                                       suspectFlag}).

-define('MeasurementReaderNameValue_types',
        [{currentValue, string},
         {lastUpdated, 'RcsPm.DateTime'},
         {moClassInstance, moRef},
         {suspectFlag, boolean}]).


-endif. % _MEASUREMENT_READER_NAME_VALUE


%% ------------------ STRUCT ManagedObjectClass ----------------------
-ifndef(_MANAGED_OBJECT_CLASS).
-define(_MANAGED_OBJECT_CLASS, 1).

-record('ManagedObjectClass', {moClassName,
                               mimName,
                               mimVersion,
                               mimRelease}).

-define('ManagedObjectClass_types',
        [{moClassName, string},
         {mimName, string},
         {mimVersion, string},
         {mimRelease, string}]).


-endif. % _MANAGED_OBJECT_CLASS


%% ------------------ STRUCT MeasurementSpecification ----------------------
-ifndef(_MEASUREMENT_SPECIFICATION).
-define(_MEASUREMENT_SPECIFICATION, 1).

-record('MeasurementSpecification', {groupRef,
                                     measurementTypeRef}).

-define('MeasurementSpecification_types',
        [{groupRef, moRef},
         {measurementTypeRef, moRef}]).


-endif. % _MEASUREMENT_SPECIFICATION

