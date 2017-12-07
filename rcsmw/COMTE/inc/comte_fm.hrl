%% COM FM Severity levels taken from ComOamSpiNotificationFm_1.h

%%
%% Indicates that the problem has disappeared.
%% Only the stateful alarms can be cleared.
%% The stateless alarms (alerts) can not be cleared.
%%
-define(FmSeverityCleared,0).
%%
%% If the severity level is not specified, this level
%% will be used by default.
%%
-define(FmSeverityIndeterminate,1).
%%
%% Indicates that there is a problem.
%% The system may work without serious problems,
%% but the problem must be solved.
%%
-define(FmSeverityWarning,2).
%%
%% Indicates that there is a problem.
%% It must be solved.
%%
-define(FmSeverityMinor,3).
%%
%% Indicates that there is a problem.
%% It can lead to a partial or a complete
%% unfunctional system.
%%
-define(FmSeverityMajor,4).
%%
%% Indicates that there is a serious problem.
%% Immediate actions must be taken by the operator,
%% otherwise the system will be completely unfunctional.
%%
-define(FmSeverityCritical,5).
