%% must match definition in master.h of ift_app
-define(OEI, 8).

%% must match definitions in test_oei.c
-define(Oei_initiateMemory, 1).
-define(Oei_getEventIdentity, 2).

%% must match values in cello_oei.h
-define(CELLO_OEI_OK,           0).
-define(CELLO_OEI_TIMEOUT,      1).
-define(CELLO_OEI_NO_SERVER,    2).
-define(CELLO_OEI_SERVER_ERROR, 3).

-define(CELLO_OEI_UNSPECIFIED_EVENT_ID, 0).

-define(CELLO_OEI_NO_TIMEOUT_VALUE, 0).
