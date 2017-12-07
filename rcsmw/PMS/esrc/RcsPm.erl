-module('RcsPm').


-export([string/1]).
-export([int16/1]).
-export([int32/1]).
-export([uint16/1]).
-export([uint32/1]).
-export([boolean/1]).
-export([moRef/1]).
-export(['RcsPm.JobStartStopSupport'/1]).
-export(['RcsPm.JobType'/1]).
-export(['RcsPm.JobPriority'/1]).
-export(['RcsPm.Aggregation'/1]).
-export(['RcsPm.JobState'/1]).
-export(['RcsPm.SeverityLevel'/1]).
-export(['RcsPm.TimePeriod'/1]).
-export(['RcsPm.MeasurementStatus'/1]).
-export(['RcsPm.ThresholdDirection'/1]).
-export(['RcsPm.CollectionMethod'/1]).
-export(['RcsPm.JobControl'/1]).
-export(['RcsPm.CompressionTypes'/1]).

 

%% default functions START
string(Val)  -> Val.
int16(Val)   -> list_to_integer(Val).
int32(Val)   -> list_to_integer(Val).
uint16(Val)  -> list_to_integer(Val).
uint32(Val)  -> list_to_integer(Val).
boolean(Val) -> list_to_atom(Val).
moRef(Val)   -> list_to_atom(Val).
%% default functions END




%% JobStartStopSupportSTART
%% NONE<====>0
'RcsPm.JobStartStopSupport'(0)->"NONE";
'RcsPm.JobStartStopSupport'("NONE")->0;
%% BASIC<====>1
'RcsPm.JobStartStopSupport'(1)->"BASIC";
'RcsPm.JobStartStopSupport'("BASIC")->1.

%% JobStartStopSupportEND


%% JobTypeSTART
%% MEASUREMENTJOB<====>1
'RcsPm.JobType'(1)->"MEASUREMENTJOB";
'RcsPm.JobType'("MEASUREMENTJOB")->1;
%% THRESHOLDJOB<====>2
'RcsPm.JobType'(2)->"THRESHOLDJOB";
'RcsPm.JobType'("THRESHOLDJOB")->2;
%% REALTIMEJOB<====>3
'RcsPm.JobType'(3)->"REALTIMEJOB";
'RcsPm.JobType'("REALTIMEJOB")->3.

%% JobTypeEND


%% JobPrioritySTART
%% LOW<====>1
'RcsPm.JobPriority'(1)->"LOW";
'RcsPm.JobPriority'("LOW")->1;
%% MEDIUM<====>2
'RcsPm.JobPriority'(2)->"MEDIUM";
'RcsPm.JobPriority'("MEDIUM")->2;
%% HIGH<====>3
'RcsPm.JobPriority'(3)->"HIGH";
'RcsPm.JobPriority'("HIGH")->3.

%% JobPriorityEND


%% AggregationSTART
%% SUM<====>2
'RcsPm.Aggregation'(2)->"SUM";
'RcsPm.Aggregation'("SUM")->2;
%% AVG<====>3
'RcsPm.Aggregation'(3)->"AVG";
'RcsPm.Aggregation'("AVG")->3;
%% MIN<====>4
'RcsPm.Aggregation'(4)->"MIN";
'RcsPm.Aggregation'("MIN")->4;
%% MAX<====>5
'RcsPm.Aggregation'(5)->"MAX";
'RcsPm.Aggregation'("MAX")->5;
%% LAST_UPDATE<====>6
'RcsPm.Aggregation'(6)->"LAST_UPDATE";
'RcsPm.Aggregation'("LAST_UPDATE")->6.

%% AggregationEND


%% JobStateSTART
%% ACTIVE<====>1
'RcsPm.JobState'(1)->"ACTIVE";
'RcsPm.JobState'("ACTIVE")->1;
%% STOPPED<====>2
'RcsPm.JobState'(2)->"STOPPED";
'RcsPm.JobState'("STOPPED")->2.

%% JobStateEND


%% SeverityLevelSTART
%% CRITICAL<====>3
'RcsPm.SeverityLevel'(3)->"CRITICAL";
'RcsPm.SeverityLevel'("CRITICAL")->3;
%% MAJOR<====>4
'RcsPm.SeverityLevel'(4)->"MAJOR";
'RcsPm.SeverityLevel'("MAJOR")->4;
%% MINOR<====>5
'RcsPm.SeverityLevel'(5)->"MINOR";
'RcsPm.SeverityLevel'("MINOR")->5;
%% WARNING<====>6
'RcsPm.SeverityLevel'(6)->"WARNING";
'RcsPm.SeverityLevel'("WARNING")->6.

%% SeverityLevelEND


%% TimePeriodSTART
%% ONE_MIN<====>3
'RcsPm.TimePeriod'(3)->"ONE_MIN";
'RcsPm.TimePeriod'("ONE_MIN")->3;
%% FIVE_MIN<====>4
'RcsPm.TimePeriod'(4)->"FIVE_MIN";
'RcsPm.TimePeriod'("FIVE_MIN")->4;
%% FIFTEEN_MIN<====>5
'RcsPm.TimePeriod'(5)->"FIFTEEN_MIN";
'RcsPm.TimePeriod'("FIFTEEN_MIN")->5;
%% THIRTY_MIN<====>6
'RcsPm.TimePeriod'(6)->"THIRTY_MIN";
'RcsPm.TimePeriod'("THIRTY_MIN")->6;
%% ONE_HOUR<====>7
'RcsPm.TimePeriod'(7)->"ONE_HOUR";
'RcsPm.TimePeriod'("ONE_HOUR")->7;
%% TWELVE_HOUR<====>8
'RcsPm.TimePeriod'(8)->"TWELVE_HOUR";
'RcsPm.TimePeriod'("TWELVE_HOUR")->8;
%% ONE_DAY<====>9
'RcsPm.TimePeriod'(9)->"ONE_DAY";
'RcsPm.TimePeriod'("ONE_DAY")->9;
%% TEN_SECONDS<====>1
'RcsPm.TimePeriod'(1)->"TEN_SECONDS";
'RcsPm.TimePeriod'("TEN_SECONDS")->1;
%% THIRTY_SECONDS<====>2
'RcsPm.TimePeriod'(2)->"THIRTY_SECONDS";
'RcsPm.TimePeriod'("THIRTY_SECONDS")->2.

%% TimePeriodEND


%% MeasurementStatusSTART
%% USED<====>1
'RcsPm.MeasurementStatus'(1)->"USED";
'RcsPm.MeasurementStatus'("USED")->1;
%% DEPRECATED<====>2
'RcsPm.MeasurementStatus'(2)->"DEPRECATED";
'RcsPm.MeasurementStatus'("DEPRECATED")->2;
%% OBSOLETE<====>3
'RcsPm.MeasurementStatus'(3)->"OBSOLETE";
'RcsPm.MeasurementStatus'("OBSOLETE")->3;
%% PRELIMINARY<====>4
'RcsPm.MeasurementStatus'(4)->"PRELIMINARY";
'RcsPm.MeasurementStatus'("PRELIMINARY")->4.

%% MeasurementStatusEND


%% ThresholdDirectionSTART
%% INCREASING<====>1
'RcsPm.ThresholdDirection'(1)->"INCREASING";
'RcsPm.ThresholdDirection'("INCREASING")->1;
%% DECREASING<====>2
'RcsPm.ThresholdDirection'(2)->"DECREASING";
'RcsPm.ThresholdDirection'("DECREASING")->2.

%% ThresholdDirectionEND


%% CollectionMethodSTART
%% CC<====>1
'RcsPm.CollectionMethod'(1)->"CC";
'RcsPm.CollectionMethod'("CC")->1;
%% GAUGE<====>2
'RcsPm.CollectionMethod'(2)->"GAUGE";
'RcsPm.CollectionMethod'("GAUGE")->2;
%% DER<====>3
'RcsPm.CollectionMethod'(3)->"DER";
'RcsPm.CollectionMethod'("DER")->3;
%% SI<====>4
'RcsPm.CollectionMethod'(4)->"SI";
'RcsPm.CollectionMethod'("SI")->4.

%% CollectionMethodEND


%% JobControlSTART
%% FULL<====>0
'RcsPm.JobControl'(0)->"FULL";
'RcsPm.JobControl'("FULL")->0;
%% STARTSTOP<====>1
'RcsPm.JobControl'(1)->"STARTSTOP";
'RcsPm.JobControl'("STARTSTOP")->1;
%% VIEWONLY<====>2
'RcsPm.JobControl'(2)->"VIEWONLY";
'RcsPm.JobControl'("VIEWONLY")->2.

%% JobControlEND


%% CompressionTypesSTART
%% GZIP<====>0
'RcsPm.CompressionTypes'(0)->"GZIP";
'RcsPm.CompressionTypes'("GZIP")->0.

%% CompressionTypesEND


