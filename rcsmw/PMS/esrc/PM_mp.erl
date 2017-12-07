-module('PM_mp').


-export([string/1]).
-export([int16/1]).
-export([int32/1]).
-export([uint16/1]).
-export([uint32/1]).
-export([boolean/1]).
-export([moRef/1]).
-export(['ECIM_PM.JobStartStopSupport'/1]).
-export(['ECIM_PM.JobType'/1]).
-export(['ECIM_PM.JobPriority'/1]).
-export(['ECIM_PM.Aggregation'/1]).
-export(['ECIM_PM.JobState'/1]).
-export(['ECIM_PM.SeverityLevel'/1]).
-export(['ECIM_PM.TimePeriod'/1]).
-export(['ECIM_PM.MeasurementStatus'/1]).
-export(['ECIM_PM.ThresholdDirection'/1]).
-export(['ECIM_PM.CollectionMethod'/1]).



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
'ECIM_PM.JobStartStopSupport'(0)->"NONE";
'ECIM_PM.JobStartStopSupport'("NONE")->0;
%% BASIC<====>1
'ECIM_PM.JobStartStopSupport'(1)->"BASIC";
'ECIM_PM.JobStartStopSupport'("BASIC")->1.

%% JobStartStopSupportEND


%% JobTypeSTART
%% MEASUREMENTJOB<====>1
'ECIM_PM.JobType'(1)->"MEASUREMENTJOB";
'ECIM_PM.JobType'("MEASUREMENTJOB")->1;
%% THRESHOLDJOB<====>2
'ECIM_PM.JobType'(2)->"THRESHOLDJOB";
'ECIM_PM.JobType'("THRESHOLDJOB")->2;
%% REALTIMEJOB<====>3
'ECIM_PM.JobType'(3)->"REALTIMEJOB";
'ECIM_PM.JobType'("REALTIMEJOB")->3.

%% JobTypeEND


%% JobPrioritySTART
%% LOW<====>1
'ECIM_PM.JobPriority'(1)->"LOW";
'ECIM_PM.JobPriority'("LOW")->1;
%% MEDIUM<====>2
'ECIM_PM.JobPriority'(2)->"MEDIUM";
'ECIM_PM.JobPriority'("MEDIUM")->2;
%% HIGH<====>3
'ECIM_PM.JobPriority'(3)->"HIGH";
'ECIM_PM.JobPriority'("HIGH")->3.

%% JobPriorityEND


%% AggregationSTART
%% SUM<====>2
'ECIM_PM.Aggregation'(2)->"SUM";
'ECIM_PM.Aggregation'("SUM")->2;
%% AVG<====>3
'ECIM_PM.Aggregation'(3)->"AVG";
'ECIM_PM.Aggregation'("AVG")->3;
%% MIN<====>4
'ECIM_PM.Aggregation'(4)->"MIN";
'ECIM_PM.Aggregation'("MIN")->4;
%% MAX<====>5
'ECIM_PM.Aggregation'(5)->"MAX";
'ECIM_PM.Aggregation'("MAX")->5;
%% LAST_UPDATE<====>6
'ECIM_PM.Aggregation'(6)->"LAST_UPDATE";
'ECIM_PM.Aggregation'("LAST_UPDATE")->6.

%% AggregationEND


%% JobStateSTART
%% ACTIVE<====>1
'ECIM_PM.JobState'(1)->"ACTIVE";
'ECIM_PM.JobState'("ACTIVE")->1;
%% STOPPED<====>2
'ECIM_PM.JobState'(2)->"STOPPED";
'ECIM_PM.JobState'("STOPPED")->2.

%% JobStateEND


%% SeverityLevelSTART
%% CRITICAL<====>3
'ECIM_PM.SeverityLevel'(3)->"CRITICAL";
'ECIM_PM.SeverityLevel'("CRITICAL")->3;
%% MAJOR<====>4
'ECIM_PM.SeverityLevel'(4)->"MAJOR";
'ECIM_PM.SeverityLevel'("MAJOR")->4;
%% MINOR<====>5
'ECIM_PM.SeverityLevel'(5)->"MINOR";
'ECIM_PM.SeverityLevel'("MINOR")->5;
%% WARNING<====>6
'ECIM_PM.SeverityLevel'(6)->"WARNING";
'ECIM_PM.SeverityLevel'("WARNING")->6.

%% SeverityLevelEND


%% TimePeriodSTART
%% ONE_MIN<====>3
'ECIM_PM.TimePeriod'(3)->"ONE_MIN";
'ECIM_PM.TimePeriod'("ONE_MIN")->3;
%% FIVE_MIN<====>4
'ECIM_PM.TimePeriod'(4)->"FIVE_MIN";
'ECIM_PM.TimePeriod'("FIVE_MIN")->4;
%% FIFTEEN_MIN<====>5
'ECIM_PM.TimePeriod'(5)->"FIFTEEN_MIN";
'ECIM_PM.TimePeriod'("FIFTEEN_MIN")->5;
%% THIRTY_MIN<====>6
'ECIM_PM.TimePeriod'(6)->"THIRTY_MIN";
'ECIM_PM.TimePeriod'("THIRTY_MIN")->6;
%% ONE_HOUR<====>7
'ECIM_PM.TimePeriod'(7)->"ONE_HOUR";
'ECIM_PM.TimePeriod'("ONE_HOUR")->7;
%% TWELVE_HOUR<====>8
'ECIM_PM.TimePeriod'(8)->"TWELVE_HOUR";
'ECIM_PM.TimePeriod'("TWELVE_HOUR")->8;
%% ONE_DAY<====>9
'ECIM_PM.TimePeriod'(9)->"ONE_DAY";
'ECIM_PM.TimePeriod'("ONE_DAY")->9;
%% TEN_SECONDS<====>1
'ECIM_PM.TimePeriod'(1)->"TEN_SECONDS";
'ECIM_PM.TimePeriod'("TEN_SECONDS")->1;
%% THIRTY_SECONDS<====>2
'ECIM_PM.TimePeriod'(2)->"THIRTY_SECONDS";
'ECIM_PM.TimePeriod'("THIRTY_SECONDS")->2.

%% TimePeriodEND


%% MeasurementStatusSTART
%% USED<====>1
'ECIM_PM.MeasurementStatus'(1)->"USED";
'ECIM_PM.MeasurementStatus'("USED")->1;
%% DEPRECATED<====>2
'ECIM_PM.MeasurementStatus'(2)->"DEPRECATED";
'ECIM_PM.MeasurementStatus'("DEPRECATED")->2;
%% OBSOLETE<====>3
'ECIM_PM.MeasurementStatus'(3)->"OBSOLETE";
'ECIM_PM.MeasurementStatus'("OBSOLETE")->3;
%% PRELIMINARY<====>4
'ECIM_PM.MeasurementStatus'(4)->"PRELIMINARY";
'ECIM_PM.MeasurementStatus'("PRELIMINARY")->4.

%% MeasurementStatusEND


%% ThresholdDirectionSTART
%% INCREASING<====>1
'ECIM_PM.ThresholdDirection'(1)->"INCREASING";
'ECIM_PM.ThresholdDirection'("INCREASING")->1;
%% DECREASING<====>2
'ECIM_PM.ThresholdDirection'(2)->"DECREASING";
'ECIM_PM.ThresholdDirection'("DECREASING")->2.

%% ThresholdDirectionEND


%% CollectionMethodSTART
%% CC<====>1
'ECIM_PM.CollectionMethod'(1)->"CC";
'ECIM_PM.CollectionMethod'("CC")->1;
%% GAUGE<====>2
'ECIM_PM.CollectionMethod'(2)->"GAUGE";
'ECIM_PM.CollectionMethod'("GAUGE")->2;
%% DER<====>3
'ECIM_PM.CollectionMethod'(3)->"DER";
'ECIM_PM.CollectionMethod'("DER")->3;
%% SI<====>4
'ECIM_PM.CollectionMethod'(4)->"SI";
'ECIM_PM.CollectionMethod'("SI")->4.

%% CollectionMethodEND
