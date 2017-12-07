%% COM LOG Severity levels taken from MafMwSpiLog_1.h

%% The system is unusable.
-define(LogSeverityEmergency, 0).

%% Action must be taken immediately.
-define(LogSeverityAlert, 1).

%% Critical condition.
-define(LogSeverityCritical, 2).

%% Error condition.
-define(LogSeverityError, 3).

%% Warning condition.
-define(LogSeverityWarning, 4).

%% Normal but significant condition.
-define(LogSeverityNotice, 5).

%% Informative message.
-define(LogSeverityInfo, 6).


-type com_log_severity() :: ?LogSeverityEmergency |
                            ?LogSeverityAlert |
                            ?LogSeverityCritical |
                            ?LogSeverityError |
                            ?LogSeverityWarning |
                            ?LogSeverityNotice |
                            ?LogSeverityInfo.

-type comte_log_severity() :: emergency |
                              alert |
                              critical |
                              error |
                              warning |
                              notice |
                              info.


%% Ericsson defined facilities
-define(LogFacilityAlarm, 100).
-define(LogFacilityAlert, 101).

%% See syslog rfc5424
-type comte_log_facility() :: 0..23 |
                              ?LogFacilityAlarm |
                              ?LogFacilityAlert.
