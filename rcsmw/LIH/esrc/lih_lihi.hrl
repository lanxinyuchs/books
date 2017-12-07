-define(CELLO_LIHI_NO_ERROR, 0).
-define(CELLO_LIHI_INVALID_PROTOCOL_VERSION, 1).
-define(CELLO_LIHI_UNEXPECTED_ERROR, 2).
-define(CELLO_LIHI_ALREADY_SUBSCRIBED,           5).

-define(CELLO_LIHI_PV1, 1).

-define(CELLO_LIHI_SERVICE_OPERABLE, 0).
-define(CELLO_LIHI_SERVICE_INOPERABLE, 1).

-define(FEATURE_ID(Rdn), {"1", "1", "1", Rdn}).

-record(lihi_feature_subscription,
	{id            :: {feature_key_id(), feature_rdn()},
	 pid_list = [] :: [pid()],
	 feature_state :: feature_state(),
	 license_state :: license_state(),
	 service_state :: non_neg_integer()
	}).

-record(lihi_feature_license,
	{id    :: feature_key_id(),
	 state :: license_state(),
	 rdn_list = [] :: [feature_rdn()]
	}).

-type lihi_id() :: pid().
-type key_id() :: string().
-type rdn() :: string().
-type feature_key_id() :: key_id().
-type feature_rdn() :: rdn().
-type capacity_key_id() :: {sw_license(), hwac()}.
-type capacity_rdn() :: {sw_rdn(), hwac_rdn()}.
-type capacity_unit() :: string().
-type sw_rdn() :: rdn().
-type hwac_rdn() :: rdn().
-type hwac() :: {key_id(), boolean()}.
-type sw_license() :: {key_id(), boolean()}.
-type feature_state() :: 'activated' | 'deactivated'.
-type license_state() :: 'enabled' | 'disabled'.
-type service_state() :: 'operable' | 'inoperable'.
