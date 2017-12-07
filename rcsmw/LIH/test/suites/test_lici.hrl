-define(LICI,1).

-define(CelloLici_initiateMemory, 1).
-define(CelloLici_initiateService, 2).
-define(CelloLici_terminateService, 3).
-define(CelloLici_internal, 4).
-define(CelloLici_featureSubscription, 5).
-define(CelloLici_capacitySubscription, 6).
-define(CelloLici_statusSubscription, 7).
-define(CelloLici_isLKFInstalled, 8).

-define(CELLO_LICI_FEATURE_SUBSCRIBE_CFM,  (16#1066E)). % 67182
-define(CELLO_LICI_FEATURE_SUBSCRIBE_REJ,  (16#1066F)). % 67183
-define(CELLO_LICI_CAPACITY_SUBSCRIBE_CFM, (16#10670)). % 67184
-define(CELLO_LICI_CAPACITY_SUBSCRIBE_REJ, (16#10671)). % 67185
-define(CELLO_LICI_FEATURE_CHANGE_IND,     (16#10672)). % 67186
-define(CELLO_LICI_CAPACITY_CHANGE_IND,    (16#10673)). % 67187
-define(CELLO_LICI_STATUS_SUBSCRIBE_CFM,   (16#10674)). % 67188
-define(CELLO_LICI_STATUS_CHANGE_IND,      (16#10675)). % 67189
-define(CELLO_LICI_SERVER_UP_IND,          (16#10676)). % 67190
-define(CELLO_LICI_SERVER_DOWN_IND,        (16#10677)). % 67191
-define(CELLO_LICI_SERVER_UNPUBLISH_IND,   (16#10678)). % 67192
-define(CELLO_LICI_INITIATE_SERVICE_CFM,   (16#10679)). % 67193
-define(CELLO_LICI_INITIATE_SERVICE_SUS,   (16#1067A)). % 67194
-define(CELLO_LICI_INITIATE_SERVICE_REJ,   (16#1067B)). % 67195
-define(CELLO_LICI_TERMINATE_SERVICE_CFM,  (16#1067C)). % 67196
-define(CELLO_LICI_STATUS_SUBSCRIBE_REJ,   (16#106FB)). % 67197
-define(CELLO_LICI_IS_LKF_INSTALLED_RSP,   (16#10AC6)). % 67198

%% CelloLiciRejectReason
-define(CELLO_LICI_SUCCESS,                      0).
-define(CELLO_LICI_SERVICE_UNAVAIL,              1).
-define(CELLO_LICI_ILLEGAL_PARAM,                2).
-define(CELLO_LICI_UNEXPECTED_ERROR,             3).
-define(CELLO_LICI_ILLEGAL_SIGNAL,               4).
-define(CELLO_LICI_ALREADY_SUBSCRIBED,           5).
-define(CELLO_LICI_INVALID_PROTOCOL_VERSION,     6).
-define(CELLO_LICI_SERVER_NOT_UNAVAIL,           7).
-define(CELLO_LICI_NOT_SUPPORTED_BY_SELECTED_PV, 8).

%% CelloLiciProtocolVersion
-define(CELLO_LICI_NO_PV, 0).
-define(CELLO_LICI_PV1,   1).
-define(CELLO_LICI_PV2,   2).
-define(CELLO_LICI_PV3,   3).

%% CelloLiciChangeReason
-define(CELLO_LICI_LICENSED_VALUE,   0).
-define(CELLO_LICI_EMERGENCY_REASON, 1).
-define(CELLO_LICI_NOT_ACTIVATED,    2).

%% CelloLiciFeatureStatus
-define(CELLO_LICI_FEATURE_ENABLED,  0).
-define(CELLO_LICI_FEATURE_DISABLED, 1).

%% CelloLiciLicMgrStatus
-define(CELLO_LICI_EMERGENCY_ACTIVATED,   0).
-define(CELLO_LICI_EMERGENCY_DEACTIVATED, 1).

%% CelloLiciEmergencyCounter
-define(CELLO_LICI_NO_EMERGENCY,    0).
-define(CELLO_LICI_EMERGENCY_ONCE,  1).
-define(CELLO_LICI_EMERGENCY_TWICE, 2).

-define(CELLO_LICI_LICENSED_LEVEL_VALUE_VALID,     0).
-define(CELLO_LICI_LICENSED_LEVEL_VALUE_NOT_VALID, 1).
-define(CELLO_LICI_HARD_LIMIT_VALUE_VALID,         0).
-define(CELLO_LICI_HARD_LIMIT_NOT_VALID,           1).

%%
-define(CELLO_LICI_LKF_NOT_INSTALLED, 1).
-define(CELLO_LICI_LKF_INSTALLED,     0).


-define(CelloLiciSignalRevision, []).

-define(CelloLiciFeatureId, []).

-define(CelloLiciCapacityId, []).

-define(CelloLiciResult, [{0,"CELLO_LICI_SUCCESS"},
			  {1,"CELLO_LICI_SERVICE_UNAVAIL"},
			  {4,"CELLO_LICI_ILLEGAL_SIGNAL"},
			  {7,"CELLO_LICI_SERVER_NOT_UNAVAIL"},
			  {8,"CELLO_LICI_NOT_SUPPORTED_BY_SELECTED_PV"}]).

-define(CelloLiciRejectReason, [{3,"CELLO_LICI_UNEXPECTED_ERROR"},
				{5,"CELLO_LICI_ALREADY_SUBSCRIBED"},
				{6,"CELLO_LICI_INVALID_PROTOCOL_VERSION"}]).

-define(CelloLiciProtocolVersion, [{1,"CELLO_LICI_PV1"},
				   {2,"CELLO_LICI_PV2"},
				   {3,"CELLO_LICI_PV3"}]).

-define(CelloLiciFeatureStatus, [{0,"CELLO_LICI_FEATURE_ENABLED"},
				 {1,"CELLO_LICI_FEATURE_DISABLED"}]).

-define(CelloLiciChangeReason, [{0,"CELLO_LICI_LICENSED_VALUE"},
				{1,"CELLO_LICI_EMERGENCY_REASON"},
				{2,"CELLO_LICI_NOT_ACTIVATED"}]).

-define(CelloLiciLicMgrStatus, [{0,"CELLO_LICI_EMERGENCY_ACTIVATED"},
				{1,"CELLO_LICI_EMERGENCY_DEACTIVATED"}]).

-define(CelloLiciEmergencyCounter, [{,"CELLO_LICI_NO_EMERGENCY"},
				    {,"CELLO_LICI_EMERGENCY_ONCE"},
				    {,"CELLO_LICI_EMERGENCY_TWICE"}]).

-define(CelloLiciSignals, [{?CELLO_LICI_FEATURE_SUBSCRIBE_CFM,
			    {"CELLO_LICI_FEATURE_SUBSCRIBE_CFM",
			     [{"featureId",?CelloLiciFeatureId},
			      {"featureStatus",?CelloLiciFeatureStatus},
			      {"changeReson",?CelloLiciChangeReason}]}},
			   {?CELLO_LICI_FEATURE_SUBSCRIBE_REJ,
			    {"CELLO_LICI_FEATURE_SUBSCRIBE_REJ",
			     [{"featureId",?CelloLiciFeatureId},
			      {"rejectReason",?CelloLiciRejectReason}]}},
			   {?CELLO_LICI_CAPACITY_SUBSCRIBE_CFM,
			    {"CELLO_LICI_CAPACITY_SUBSCRIBE_CFM",
			     [{"capacityId",?CelloLiciCapacityId},
			      {"licensedLevel",?CelloLiciLicensedLevel}]}},
			   {?CELLO_LICI_CAPACITY_SUBSCRIBE_REJ,
			    "CELLO_LICI_CAPACITY_SUBSCRIBE_REJ"},
			   {?CELLO_LICI_FEATURE_CHANGE_IND,
			    {"CELLO_LICI_FEATURE_CHANGE_IND",
			     [{"featureId",?CelloLiciFeatureId},
			      {"featureStatus",?CelloLiciFeatureStatus},
			      {"changeReason",?CelloLiciChangeReason}]}},
			   {?CELLO_LICI_CAPACITY_CHANGE_IND,
			    "CELLO_LICI_CAPACITY_CHANGE_IND"},
			   {?CELLO_LICI_STATUS_SUBSCRIBE_CFM,
			    "CELLO_LICI_STATUS_SUBSCRIBE_CFM"},
			   {?CELLO_LICI_STATUS_CHANGE_IND,
			    "CELLO_LICI_STATUS_CHANGE_IND"},
			   {?CELLO_LICI_SERVER_UP_IND,
			    {"CELLO_LICI_SERVER_UP_IND",[]}},
			   {?CELLO_LICI_SERVER_DOWN_IND,
			    {"CELLO_LICI_SERVER_DOWN_IND",[]}},
			   {?CELLO_LICI_SERVER_UNPUBLISH_IND,
			    {"CELLO_LICI_SERVER_UNPUBLISH_IND",[]}},
			   {?CELLO_LICI_INITIATE_SERVICE_CFM,
			    {"CELLO_LICI_INITIATE_SERVICE_CFM",
			     [{"signalRevision",?CelloLiciSignalRevision},
			      {"selectedPV",?CelloLiciProtocolVersion}]}},
			   {?CELLO_LICI_INITIATE_SERVICE_SUS,
			    {"CELLO_LICI_INITIATE_SERVICE_SUS",
			     [{"signalRevision",?CelloLiciSignalRevision},
			      {"protocolVersion",?CelloLiciProtocolVersion}]}},
			   {?CELLO_LICI_INITIATE_SERVICE_REJ,
			    {"CELLO_LICI_INITIATE_SERVICE_REJ",
			     [{"signalRevision",?CelloLiciSignalRevision},
			      {"protocolVersion",?CelloLiciProtocolVersion},
			      {"rejectReason",?CelloLiciRejectReason}]}}},
	{?CELLO_LICI_TERMINATE_SERVICE_CFM,
	 {"CELLO_LICI_TERMINATE_SERVICE_CFM",[]}},
	{?CELLO_LICI_STATUS_SUBSCRIBE_REJ,
	 "CELLO_LICI_STATUS_SUBSCRIBE_REJ"},
	{?CELLO_LICI_IS_LKF_INSTALLED_RSP,
	 "CELLO_LICI_IS_LKF_INSTALLED_RSP"}]).
