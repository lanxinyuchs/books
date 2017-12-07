%% COM AC enums taken from ComMwSpiAvailabilityController_1.h

%%%
%%% The High Availability modes that COM can operate in.
%%%

%% <p>COM has started its processes and made other initializations
%% but none of its services is offered.
%% COM ends up in this mode after being started, but the
%% Middleware (MW) Support Agent (SA) can also order COM
%% to return to this mode after being in Active or Standby mode.</p>
%%
-define(ComMwSpiHaModeUnassigned,1).
%%
%% <p>COM provides all the services.</p>
%%
-define(ComMwSpiHaModeActive,2).
%%
%% <p>COM acts as standby and keeps the same state while it is
%% operating in Active mode.</p>
%%
-define(ComMwSpiHaModeStandby,3).

%%%
%%% The reasons for changes of the High Availability operational mode.
%%% 

%%
%% <p>This reason is given when MW SA sets the HA mode the first
%% time to a COM component.</p>
%%
-define(ComMwSpiHaReasonInit,1).
%%
%% <p>This reason is given when MW SA sets the HA mode Active to
%% a COM component that before had the HA mode Standby.
%% The HA mode change was triggered by a fault.</p>
%%
-define(ComMwSpiHaReasonFail,2).
%%
%% <p>This reason is given when MW SA sets a new HA mode due to
%% some other reason, for example a management operation.</p>
%%
-define(ComMwSpiHaReasonSwitchover,3).


%%%
%%% The recovery options that COM can recommend.
%%%

%%
%% <p>Restart the current COM component without changing the HA mode.</p>
%%
-define(ComMwSpiRecommendedRecoveryRestart,1).
%%
%% <p>Restart the current COM component and activate the standby COM.</p>
%%
-define(ComMwSpiRecommendedRecoveryFailover,2).


%%%
%%% The function return codes must be used by all the interfaces.
%%% The purpose of these codes is to allow the caller of a function to
%%% come to a programmatic decision based on the return code.
%%% It is recommended to return human-readable error information using the
%%% ComMgmtSpiThreadContext if a return code is to be interpreted as an error.
%%%
%%% Return value = 0 is always OK. @n
%%% Return value < 0 is normally an error. @n
%%% Return value > 0 is normally not an error but
%%% informs the caller that the service could not be provided.
%%%

%%
%% The function call executed successfully.
%%
-define(ComOk,0).
%%
%% <p>The function could not provide any service at this point in time.
%% The problem that occured is temporary, and the caller may retry later.
%%
%% <p>Note that this error code is only to be returned from interfaces
%% that explicitely declares that this error code can be returned
%% and have this meaning. Currently all use cases of COM SPIs interprets
%% anything other than ComOk
%% a failure and will abort the operation.</p>
%%
-define(ComTryAgain,1).
%%
%% <p>The function could not provide any service since the service is not started.</p>
%%
-define(ComNotActive,2).
%%
%% <p>The function call failed, an error has occured which is specific
%% for the function implementation.</p>
%%
-define(ComFailure,-1).
%%
%% <p>The function call failed since something sought after did not exist.
%% Detailed information can be obtained from ComMgmtSpiThreadContext.</p>
%%
-define(ComNotExist,-2).
%%
%% <p>The function call failed since something that was to be created already exists.
%% Detailed information can be obtained from ComMgmtSpiThreadContext.</p>
%%
-define(ComAlreadyExist,-3).
%%
%% <p>The function call failed and was aborted. The function did not change
%% any persistent data.
%% Detailed information can be obtained from ComMgmtSpiThreadContext.</p>
%%
-define(ComAborted,-4).
%%
%% <p>The function call failed since an object is locked.</p>
%%
-define(ComObjectLocked,-5).
%%
%% <p>The function call failed in the prepare phase of the transaction.
%% The transaction has been aborted.</p>
%%
-define(ComPrepareFailed,-6).
%%
%% <p>The function call failed in the commit phase.
%% Some participants may have failed to commit and the
%% total transactional result may be inconsistent. A human
%% may be needed to resolve the situation.</p>
%%
-define(ComCommitFailed,-7).
%%
%% <p>The function call failed since an argument is invalid.</p>
%%
-define(ComInvalidArgument,-8).
%%
%% <p>The function call failed since the data did not validate.</p>
%%
-define(ComValidationFailed,-9).
%%
%% <p>The function call failed since there was no available resource, such as memory.</p>
%%
-define(ComNoResources,-10).
%%
%% <p>Some vital resource needed in the function call has timed out.
%% It is unspecified whether the call succeeded or whether it did not,
%% the user of interface must find it out.</p>
%%
-define(ComTimeOut,-11).
