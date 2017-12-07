%% must match definition in master.h of ift_app
-define(OEI, 8).

%% must match definitions in test_oei.c
-define(RcsOei_getEventIdentity, 3).
-define(RcsOei_getDerivedCorrelationUUID, 4).
-define(RcsOei_getDerivedCorrelationUUID2, 5).
-define(RcsOei_getRcsOeiUUID, 6).

%% must match values in cello_oei.h
-define(RCS_OEI_OK,                  0).
-define(RCS_OEI_TIMEOUT,             1).
-define(RCS_OEI_NO_SERVER,           2).
-define(RCS_OEI_SERVER_ERROR,        3).
-define(RCS_OEI_INVALID_UUID_FORMAT, 4).


-define(RCS_OEI_UNSPECIFIED_EVENT_ID, 0).
-define(RCS_OEI_UNSPECIFIED_CORR_ID, "00000000-0000-0000-0000-000000000000").

-define(RCS_OEI_NO_TIMEOUT_VALUE, 0).

-define(RCS_OEI_UUID_LEN, 36).

