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

-hrl_id({"RmeExeR","1.2.0","/main/R8A/1"}).


%% -------------- CLASS ExecutionResource -------------------------

%% Description:
%% This MO class contains actions on the node.

-record(executionResource, {executionResourceId,
                            upgradeStatus,
                            vnfIdentity}).

-define(executionResource_types,
        [{executionResourceId, string},
         {upgradeStatus, string},
         {vnfIdentity, 'RmeExeR.Uuid'}]).

-define(ExecutionResource_restricted, [executionResourceId]).


%% ------------------ ENUM RestartReason ----------------------
-ifndef('RestartReason').
-define('RestartReason', 1).

-define(RestartReason_PLANNED_RECONFIGURATION, 0).
-define(RestartReason_UNPLANNED_NODE_EXTERNAL_PROBLEMS, 1).
-define(RestartReason_UNPLANNED_NODE_UPGRADE_PROBLEMS, 2).
-define(RestartReason_UNPLANNED_O_AND_M_ISSUE, 3).
-define(RestartReason_UNPLANNED_CYCLIC_RECOVERY, 4).
-define(RestartReason_UNPLANNED_LOCKED_RESOURCES, 5).
-define(RestartReason_UNPLANNED_COLD_WITH_HW_TEST, 6).
-define(RestartReason_UNPLANNED_CALL_PROCESSING_DEGRADATION, 7).
-define(RestartReason_UNPLANNED_LOW_COVERAGE, 8).
-define(RestartReason_OPERATOR_CLASSIFIED_PROBLEMS, 31).

-endif. % RestartReason

%% ------------------ ENUM RestartRank ----------------------
-ifndef('RestartRank').
-define('RestartRank', 1).

-define(RestartRank_RESTART_WARM, 0).
-define(RestartRank_RESTART_COLD, 1).
-define(RestartRank_RESTART_COLDWTEST, 2).

-endif. % RestartRank
