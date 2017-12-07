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

-hrl_id({"RmeTimeSettings","1.0.0","/main/R4A/3"}).


%% -------------- CLASS TimeSettings -------------------------

%% Description:
%% This MO class contain configuration for time zone information broadcast. 
%% The settings in this MO do not affect the system local time zone.

-record(timeSettings, {timeSettingsId,
                       daylightSavingTimeEndDate,
                       daylightSavingTimeOffset,
                       daylightSavingTimeStartDate,
                       gpsToUtcLeapSeconds,
                       gpsToUtcLeapSecondsChangeDate,
                       timeOffset}).

-define(timeSettings_types,
        [{timeSettingsId, string},
         {daylightSavingTimeEndDate, {struct,'DstRule'}},
         {daylightSavingTimeOffset, 'RmeTimeSettings.RmeTimeSettings_TimeSettings_daylightSavingTimeOffset'},
         {daylightSavingTimeStartDate, {struct,'DstRule'}},
         {gpsToUtcLeapSeconds, 'RmeTimeSettings.RmeTimeSettings_TimeSettings_gpsToUtcLeapSeconds'},
         {gpsToUtcLeapSecondsChangeDate, 'RmeTimeSettings.RmeTimeSettings_TimeSettings_gpsToUtcLeapSecondsChangeDate'},
         {timeOffset, 'RmeTimeSettings.RmeTimeSettings_TimeSettings_timeOffset'}]).

-define(timeSettings_daylightSavingTimeOffset_default, 1:00).
-define(timeSettings_timeOffset_default, +00:00).
-define(TimeSettings_restricted, [timeSettingsId]).


%% ------------------ ENUM DstMonth ----------------------
-ifndef('DstMonth').
-define('DstMonth', 1).

-define(DstMonth_JANUARY, 1).
-define(DstMonth_FEBRUARY, 2).
-define(DstMonth_MARCH, 3).
-define(DstMonth_APRIL, 4).
-define(DstMonth_MAY, 5).
-define(DstMonth_JUNE, 6).
-define(DstMonth_JULY, 7).
-define(DstMonth_AUGUST, 8).
-define(DstMonth_SEPTEMBER, 9).
-define(DstMonth_OCTOBER, 10).
-define(DstMonth_NOVEMBER, 11).
-define(DstMonth_DECEMBER, 12).

-endif. % DstMonth

%% ------------------ STRUCT DstRule ----------------------
-ifndef(_DST_RULE).
-define(_DST_RULE, 1).

-record('DstRule', {month,
                    dayRule,
                    time}).

-define('DstRule_types',
        [{month, 'RmeTimeSettings.DstMonth'},
         {dayRule, string},
         {time, 'RmeTimeSettings.RmeTimeSettings_DstRule_time'}]).


-endif. % _DST_RULE

