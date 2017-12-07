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

%%
-define(CELLO_LICI_LKF_NOT_INSTALLED, 1).
-define(CELLO_LICI_LKF_INSTALLED,     0).

-define(CELLO_LICI_MAX_ID_LENGTH, 24).

-define(LICI_NO_LIMIT,     {1, 0}).
-define(LICI_LIMIT(Value), {0, (Value)}).

-define(MAX_NR_OF_CLIENTS, 100).
-define(MAX_NR_OF_KEYS, 400).
-define(MAX_NR_OF_SUBSCRIPTIONS, 800).
-define(MAX_NR_OF_CLIENTS_PER_KEY, 2).

-record(lici_license_key,
	{
	  id :: 'emergencyReset' | {'featureKey' | 'capacityKey', string()},
	  start :: calendar:date() | 'undefined',
	  stop :: calendar:date() | 'infinity' | 'undefined',
	  capacity :: {0..1, integer()} | 'undefined',
	  hard_limit :: {0..1, integer()} | 'undefined',
	  emergency_reset = false :: boolean()
	}
       ).

-record(lici_lkf,
	{
	  status :: 'ok' | 'missing' | 'faulty',
	  format_version :: string() | 'undefined',
	  signature_type :: non_neg_integer() | 'undefined',
	  seq_no = 0 :: non_neg_integer(),
	  customer_id :: string() | 'undefined',
	  product_type :: string() | 'undefined',
	  swlt_id :: string() | 'undefined',
	  method :: non_neg_integer() | 'undefined',
	  print :: string() | 'undefined',
	  license_keys = [] :: [#lici_license_key{}]
	}
       ).

-record(lici_subscription,
	{
	  id :: {'featureKey', string()} |
		{'capacityKey', string()} |
		'status',
	  pid_list = [] :: [pid()],
	  feature_status = ?CELLO_LICI_FEATURE_DISABLED :: non_neg_integer(),
	  licensed_level = ?LICI_LIMIT(0),
	  hard_limit = ?LICI_LIMIT(0),
	  change_reason = ?CELLO_LICI_NOT_ACTIVATED :: non_neg_integer(),
	  mgr_status :: non_neg_integer() | 'undefined',
	  emergency_cnt :: non_neg_integer() | 'undefined'
	}
       ).

-record(lici_provider,
	{
	  spid,
	  provider_pid :: pid()
	}
       ).

-record(lih_worker,
	{
	  spid,
	  worker_pid :: pid()
	}
       ).

-type lici_id() :: pid().
-type signal_revision() :: non_neg_integer().
-type protocol_version() :: non_neg_integer().
-type capacity_limit() :: 'nolimit' | non_neg_integer().
-type change_reason() :: 'licensed_value' | 'emergency_reason' |
			 'not_activated'.
-type reject_reason() :: 'unexpected_error' | 'already_subscribed' |
			 'invalid_protocol_version'.
-type feature_status() :: 'enabled' | 'disabled'.
-type lic_mgr_status() :: 'activated' | 'deactivated'.
-type emergency_counter() :: 'no_emergency' | 'emergency_once' |
			     'emergency_twice'.
